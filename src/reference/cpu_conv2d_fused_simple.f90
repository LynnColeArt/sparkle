! Simplified fused im2col+GEMM implementation for CPU
! 
! This version uses 1D arrays throughout for simplicity

module cpu_conv2d_fused_simple
  use iso_fortran_env, only: real32, real64, int32, int64
  use gemm_simd_optimized, only: gemm_simd_avx512
  implicit none
  
  private
  public :: conv2d_fused_simple
  
contains

  real(real32) function conv2d_fused_simple(input, weights, output, &
                                           N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    real(real64) :: start_time, end_time
    integer(int64) :: total_flops
    real(real64) :: gflops
    
    ! Process in smaller tiles for better cache usage
    integer, parameter :: TILE_SIZE = 64
    real(real32), allocatable :: im2col_tile(:), output_tile(:)
    integer :: input_rows
    
    integer :: n_idx, tile_start, tile_end, tile_cols
    integer :: h_out_idx, w_out_idx, c_idx, kh_idx, kw_idx
    integer :: h_in, w_in, out_loc, im2col_idx
    integer :: input_offset, output_offset, linear_out_idx
    integer :: num_tiles, tile_idx, k_idx
    
    ! Calculate dimensions
    input_rows = C * kernel_size * kernel_size
    
    print *, "🔥 Fused im2col+GEMM CPU convolution (simplified)"
    
    call cpu_time(start_time)
    
    ! Initialize output to zero
    output = 0.0
    
    ! Process each sample
    do n_idx = 1, N
      ! Process output locations in tiles
      num_tiles = (H_out * W_out + TILE_SIZE - 1) / TILE_SIZE
      
      !$omp parallel private(tile_idx, tile_start, tile_end, tile_cols, &
      !$omp&                h_out_idx, w_out_idx, out_loc, c_idx, kh_idx, kw_idx, &
      !$omp&                h_in, w_in, input_offset, im2col_idx, k_idx, &
      !$omp&                linear_out_idx, output_offset, &
      !$omp&                im2col_tile, output_tile)
      
      ! Allocate thread-local workspace
      allocate(im2col_tile(input_rows * TILE_SIZE))
      allocate(output_tile(K * TILE_SIZE))
      
      !$omp do
      do tile_idx = 1, num_tiles
        tile_start = (tile_idx - 1) * TILE_SIZE + 1
        tile_end = min(tile_start + TILE_SIZE - 1, H_out * W_out)
        tile_cols = tile_end - tile_start + 1
        
        ! Initialize tiles
        im2col_tile = 0.0
        output_tile = 0.0
        
        ! Build im2col matrix for this tile
        do out_loc = 1, tile_cols
          linear_out_idx = tile_start + out_loc - 1
          h_out_idx = ((linear_out_idx - 1) / W_out) + 1
          w_out_idx = mod(linear_out_idx - 1, W_out) + 1
          
          ! Extract patch - row-major order in im2col
          im2col_idx = 0
          do c_idx = 1, C
            do kh_idx = 1, kernel_size
              do kw_idx = 1, kernel_size
                im2col_idx = im2col_idx + 1
                
                h_in = (h_out_idx - 1) * stride + kh_idx - pad
                w_in = (w_out_idx - 1) * stride + kw_idx - pad
                
                if (h_in >= 1 .and. h_in <= H .and. w_in >= 1 .and. w_in <= W) then
                  input_offset = ((n_idx-1)*C + (c_idx-1))*H*W + (h_in-1)*W + w_in
                  ! Store in column-major format for GEMM
                  im2col_tile((out_loc-1)*input_rows + im2col_idx) = input(input_offset)
                end if
              end do
            end do
          end do
        end do
        
        ! GEMM: output_tile = weights * im2col_tile
        call gemm_simd_avx512(weights, im2col_tile, output_tile, &
                             K, tile_cols, input_rows, &
                             1.0, 0.0)
        
        ! Write back results
        do out_loc = 1, tile_cols
          linear_out_idx = tile_start + out_loc - 1
          h_out_idx = ((linear_out_idx - 1) / W_out) + 1
          w_out_idx = mod(linear_out_idx - 1, W_out) + 1
          
          do k_idx = 1, K
            output_offset = ((n_idx-1)*K + (k_idx-1))*H_out*W_out + (h_out_idx-1)*W_out + w_out_idx
            ! GEMM output is column-major: result at (k_idx, out_loc)
            output(output_offset) = output_tile(k_idx + (out_loc-1)*K)
          end do
        end do
      end do
      !$omp end do
      
      deallocate(im2col_tile, output_tile)
      !$omp end parallel
    end do
    
    call cpu_time(end_time)
    
    ! Calculate performance
    conv2d_fused_simple = real((end_time - start_time) * 1000.0, real32)
    
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    
    gflops = real(total_flops, real64) / (real(conv2d_fused_simple, real64) * 1.0e6_real64)
    
    print '(A,I0,A)', "   Tile size: ", TILE_SIZE, " output locations"
    print '(A,F8.2,A,F8.1,A)', "   Performance: ", conv2d_fused_simple, " ms, ", gflops, " GFLOPS"
    
  end function conv2d_fused_simple

end module cpu_conv2d_fused_simple