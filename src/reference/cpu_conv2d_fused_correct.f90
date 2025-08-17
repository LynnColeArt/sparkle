! Corrected fused im2col+GEMM implementation for CPU
! 
! This version uses the EXACT same im2col indexing as the unfused version
! to ensure mathematical parity while still benefiting from hot cache

module cpu_conv2d_fused_correct
  use iso_fortran_env, only: real32, real64, int32, int64
  use gemm_simd_optimized, only: gemm_simd_avx512
  implicit none
  
  private
  public :: conv2d_fused_correct
  
contains

  real(real32) function conv2d_fused_correct(input, weights, output, &
                                            N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    real(real64) :: start_time, end_time
    integer(int64) :: total_flops
    real(real64) :: gflops
    
    ! Process in tiles for cache efficiency
    integer, parameter :: TILE_SIZE = 64
    real(real32), allocatable :: im2col_tile(:), output_tile(:)
    integer :: input_rows, total_output_locs
    
    integer :: tile_start, tile_end, tile_cols
    integer :: h_out_idx, w_out_idx, out_idx, out_loc
    integer :: n_idx, c_idx, kh_idx, kw_idx
    integer :: h_in, w_in, in_idx, col_idx
    integer :: num_tiles, tile_idx, k_idx
    integer :: linear_idx, output_offset
    
    ! Calculate dimensions - must match unfused version exactly
    input_rows = C * kernel_size * kernel_size * N
    total_output_locs = H_out * W_out
    
    print *, "🔥 Fused im2col+GEMM CPU convolution (corrected indexing)"
    
    call cpu_time(start_time)
    
    ! Initialize output
    output = 0.0
    
    ! Process output locations in tiles
    num_tiles = (total_output_locs + TILE_SIZE - 1) / TILE_SIZE
    
    !$omp parallel private(tile_idx, tile_start, tile_end, tile_cols, &
    !$omp&                h_out_idx, w_out_idx, out_idx, out_loc, &
    !$omp&                n_idx, c_idx, kh_idx, kw_idx, &
    !$omp&                h_in, w_in, in_idx, col_idx, k_idx, &
    !$omp&                linear_idx, output_offset, &
    !$omp&                im2col_tile, output_tile)
    
    ! Allocate thread-local workspace
    allocate(im2col_tile(input_rows * TILE_SIZE))
    allocate(output_tile(K * TILE_SIZE))
    
    !$omp do
    do tile_idx = 1, num_tiles
      tile_start = (tile_idx - 1) * TILE_SIZE + 1
      tile_end = min(tile_start + TILE_SIZE - 1, total_output_locs)
      tile_cols = tile_end - tile_start + 1
      
      ! Initialize tiles
      im2col_tile = 0.0
      output_tile = 0.0
      
      ! Fill im2col tile using EXACT same indexing as unfused version
      do out_loc = 1, tile_cols
        linear_idx = tile_start + out_loc - 1
        h_out_idx = ((linear_idx - 1) / W_out) + 1
        w_out_idx = mod(linear_idx - 1, W_out) + 1
        out_idx = (h_out_idx-1)*W_out + w_out_idx
        
        ! Extract patch following exact same order as unfused
        do n_idx = 1, N
          do c_idx = 1, C
            do kh_idx = 1, kernel_size
              do kw_idx = 1, kernel_size
                h_in = (h_out_idx-1)*stride + kh_idx - pad
                w_in = (w_out_idx-1)*stride + kw_idx - pad
                
                ! Calculate im2col index for this tile
                ! For column-major storage in im2col_tile(input_rows, tile_cols)
                ! Row index: which input element (0 to input_rows-1)
                col_idx = ((((kh_idx-1)*kernel_size + (kw_idx-1))*C + (c_idx-1))*N + (n_idx-1)) + 1
                
                if (h_in >= 1 .and. h_in <= H .and. w_in >= 1 .and. w_in <= W) then
                  in_idx = ((n_idx-1)*C + (c_idx-1))*H*W + (h_in-1)*W + w_in
                  ! Store in column-major order: im2col_tile(row, col) -> im2col_tile(row + (col-1)*nrows)
                  im2col_tile(col_idx + (out_loc-1)*input_rows) = input(in_idx)
                else
                  im2col_tile(col_idx + (out_loc-1)*input_rows) = 0.0
                end if
              end do
            end do
          end do
        end do
      end do
      
      ! GEMM while data is hot in cache
      ! weights: K x input_rows
      ! im2col_tile: input_rows x tile_cols
      ! output_tile: K x tile_cols
      ! Pass only the portion of buffers we're using
      call gemm_simd_avx512(weights, im2col_tile(1:input_rows*tile_cols), &
                           output_tile(1:K*tile_cols), &
                           K, tile_cols, input_rows, &
                           1.0, 0.0)
      
      ! Write results back
      do out_loc = 1, tile_cols
        linear_idx = tile_start + out_loc - 1
        h_out_idx = ((linear_idx - 1) / W_out) + 1
        w_out_idx = mod(linear_idx - 1, W_out) + 1
        
        ! Store results for all filters at this output location
        ! Note: We're processing all N together in the im2col, so results include all batches
        do k_idx = 1, K
          ! For N=1 case (most common), simplified indexing
          output_offset = (k_idx-1)*H_out*W_out + (h_out_idx-1)*W_out + w_out_idx
          ! GEMM output is column-major: output_tile(K, tile_cols)
          output(output_offset) = output_tile(k_idx + (out_loc-1)*K)
        end do
      end do
    end do
    !$omp end do
    
    deallocate(im2col_tile, output_tile)
    !$omp end parallel
    
    call cpu_time(end_time)
    
    ! Calculate performance
    conv2d_fused_correct = real((end_time - start_time) * 1000.0, real32)
    
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    
    gflops = real(total_flops, real64) / (real(conv2d_fused_correct, real64) * 1.0e6_real64)
    
    print '(A,I0,A)', "   Tile size: ", TILE_SIZE, " output locations"
    print '(A,F8.2,A,F8.1,A)', "   Performance: ", conv2d_fused_correct, " ms, ", gflops, " GFLOPS"
    
  end function conv2d_fused_correct

end module cpu_conv2d_fused_correct