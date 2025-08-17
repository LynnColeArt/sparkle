! Fused im2col+GEMM implementation for CPU - Version 2
! 
! This version uses a simpler approach that directly matches the GEMM interface
! without complex im2col indexing. We process tiles of output locations and
! build the im2col matrix on-the-fly in the correct format for GEMM.

module cpu_conv2d_fused_v2
  use iso_fortran_env, only: real32, real64, int32, int64
  use gemm_simd_optimized, only: gemm_simd_avx512
  implicit none
  
  private
  public :: conv2d_fused_hot_cache_v2
  
contains

  ! Fused convolution that keeps data hot in cache
  real(real32) function conv2d_fused_hot_cache_v2(input, weights, output, &
                                                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    real(real64) :: start_time, end_time
    integer(int64) :: total_flops
    real(real64) :: gflops
    
    ! Workspace for a tile of im2col data
    integer, parameter :: TILE_SIZE = 128  ! Process 128 output locations at a time
    real(real32), allocatable :: im2col_tile(:,:), output_tile(:,:)
    integer :: input_rows, num_output_locs
    
    integer :: n_idx, tile_start, tile_end, tile_size_actual
    integer :: h_out_idx, w_out_idx, c_idx, kh_idx, kw_idx
    integer :: h_in, w_in, out_loc, im2col_row
    integer :: input_offset, output_offset
    integer :: num_tiles, tile_idx, k_idx
    
    ! Calculate dimensions
    input_rows = C * kernel_size * kernel_size
    num_output_locs = H_out * W_out
    
    ! Allocate workspace for one tile
    allocate(im2col_tile(input_rows, TILE_SIZE))
    allocate(output_tile(K, TILE_SIZE))
    
    print *, "🔥 Fused im2col+GEMM CPU convolution V2 (hot cache)"
    
    call cpu_time(start_time)
    
    ! Initialize output to zero
    output = 0.0
    
    ! Process each sample
    do n_idx = 1, N
      ! Process output locations in tiles to maximize cache reuse
      num_tiles = (num_output_locs + TILE_SIZE - 1) / TILE_SIZE
      
      !$omp parallel do private(tile_idx, tile_start, tile_end, tile_size_actual, &
      !$omp&                   h_out_idx, w_out_idx, out_loc, c_idx, kh_idx, kw_idx, &
      !$omp&                   h_in, w_in, input_offset, im2col_row, k_idx, &
      !$omp&                   im2col_tile, output_tile)
      do tile_idx = 1, num_tiles
        ! Allocate thread-local workspace
        if (.not. allocated(im2col_tile)) then
          allocate(im2col_tile(input_rows, TILE_SIZE))
          allocate(output_tile(K, TILE_SIZE))
        end if
        
        tile_start = (tile_idx - 1) * TILE_SIZE + 1
        tile_end = min(tile_start + TILE_SIZE - 1, num_output_locs)
        tile_size_actual = tile_end - tile_start + 1
        
        ! Initialize tiles to zero
        im2col_tile = 0.0
        output_tile = 0.0
        
        ! Fill im2col_tile with data for this tile
        ! im2col_tile has shape [C*kernel_size*kernel_size, tile_size_actual]
        do out_loc = 1, tile_size_actual
          ! Convert linear output index to 2D position
          h_out_idx = ((tile_start + out_loc - 2) / W_out) + 1
          w_out_idx = mod(tile_start + out_loc - 2, W_out) + 1
          
          ! Extract the patch for this output location
          im2col_row = 0
          do c_idx = 1, C
            do kh_idx = 1, kernel_size
              do kw_idx = 1, kernel_size
                im2col_row = im2col_row + 1
                
                h_in = (h_out_idx - 1) * stride + kh_idx - pad
                w_in = (w_out_idx - 1) * stride + kw_idx - pad
                
                if (h_in >= 1 .and. h_in <= H .and. w_in >= 1 .and. w_in <= W) then
                  input_offset = ((n_idx-1)*C + (c_idx-1))*H*W + (h_in-1)*W + w_in
                  im2col_tile(im2col_row, out_loc) = input(input_offset)
                else
                  im2col_tile(im2col_row, out_loc) = 0.0
                end if
              end do
            end do
          end do
        end do
        
        ! HOT GEMM - data is still in cache!
        ! weights: K x (C*kernel_size*kernel_size)
        ! im2col_tile: (C*kernel_size*kernel_size) x tile_size_actual  
        ! output_tile: K x tile_size_actual
        !
        ! We need to compute: output_tile = weights * im2col_tile
        ! But gemm_simd_avx512 computes: C = A * B
        ! So we pass weights as A and im2col_tile as B
        ! Flatten the 2D arrays to 1D for GEMM
        call gemm_simd_avx512(weights, &
                             reshape(im2col_tile(:, 1:tile_size_actual), [input_rows * tile_size_actual]), &
                             reshape(output_tile(:, 1:tile_size_actual), [K * tile_size_actual]), &
                             K, tile_size_actual, input_rows, &
                             1.0, 0.0)
        
        ! Reshape output back
        output_tile(:, 1:tile_size_actual) = reshape(output_tile(:, 1:tile_size_actual), [K, tile_size_actual])
        
        ! Write results back to output
        do out_loc = 1, tile_size_actual
          h_out_idx = ((tile_start + out_loc - 2) / W_out) + 1
          w_out_idx = mod(tile_start + out_loc - 2, W_out) + 1
          
          ! Output is in NCHW format
          do k_idx = 1, K
            output_offset = ((n_idx-1)*K + (k_idx-1))*H_out*W_out + (h_out_idx-1)*W_out + w_out_idx
            output(output_offset) = output_tile(k_idx, out_loc)
          end do
        end do
      end do
      !$omp end parallel do
    end do
    
    call cpu_time(end_time)
    
    ! Calculate performance
    conv2d_fused_hot_cache_v2 = real((end_time - start_time) * 1000.0, real32)
    
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    
    gflops = real(total_flops, real64) / (real(conv2d_fused_hot_cache_v2, real64) * 1.0e6_real64)
    
    print '(A,I0,A,I0)', "   Tile size: ", TILE_SIZE, " output locations"
    print '(A,F8.2,A,F8.1,A)', "   Performance: ", conv2d_fused_hot_cache_v2, " ms, ", gflops, " GFLOPS"
    
    ! Cleanup thread-local allocations will happen automatically
    deallocate(im2col_tile, output_tile)
    
  end function conv2d_fused_hot_cache_v2

end module cpu_conv2d_fused_v2