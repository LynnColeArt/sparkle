! Fused im2col+GEMM implementation for CPU
! 
! Key insight: Don't create a cold intermediate buffer!
! Process convolution patches while they're hot in cache.
!
! Target: Match GPU approach of direct convolution

module cpu_conv2d_fused
  use iso_fortran_env, only: real32, real64, int32, int64
  use gemm_simd_optimized, only: gemm_simd_avx512
  implicit none
  
  private
  public :: conv2d_fused_hot_cache
  
contains

  ! Fused convolution that keeps data hot in cache
  real(real32) function conv2d_fused_hot_cache(input, weights, output, &
                                               N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    real(real64) :: start_time, end_time
    integer(int64) :: total_flops
    real(real64) :: gflops
    
    ! Workspace for a tile of im2col data
    integer, parameter :: TILE_SIZE = 64  ! Process 64 output locations at a time
    real(real32), allocatable :: input_tile(:), output_tile(:)
    integer :: input_rows
    
    integer :: n_idx, out_y, out_x, out_idx
    integer :: tile_start, tile_end, tile_cols
    integer :: k_idx, c_idx, ky, kx
    integer :: in_y, in_x, weight_offset, input_offset
    integer :: num_tiles, tile_idx
    
    ! Calculate dimensions
    input_rows = C * kernel_size * kernel_size
    
    ! Allocate workspace for one tile
    allocate(input_tile(input_rows * TILE_SIZE))
    allocate(output_tile(K * TILE_SIZE))
    
    print *, "🔥 Fused im2col+GEMM CPU convolution (hot cache)"
    
    call cpu_time(start_time)
    
    ! Initialize output to zero
    output = 0.0
    
    ! Process each sample
    do n_idx = 1, N
      ! Process output locations in tiles to maximize cache reuse
      num_tiles = (H_out * W_out + TILE_SIZE - 1) / TILE_SIZE
      
      !$omp parallel do private(tile_idx, tile_start, tile_end, tile_cols, &
      !$omp&                   out_y, out_x, out_idx, c_idx, ky, kx, &
      !$omp&                   in_y, in_x, input_offset, weight_offset, &
      !$omp&                   input_tile, output_tile) firstprivate(input_rows)
      do tile_idx = 1, num_tiles
        ! Allocate thread-local workspace
        if (.not. allocated(input_tile)) then
          allocate(input_tile(input_rows * TILE_SIZE))
          allocate(output_tile(K * TILE_SIZE))
        end if
        
        tile_start = (tile_idx - 1) * TILE_SIZE + 1
        tile_end = min(tile_start + TILE_SIZE - 1, H_out * W_out)
        tile_cols = tile_end - tile_start + 1
        
        ! Initialize tiles to zero
        input_tile = 0.0
        output_tile = 0.0
        
        ! Fill input_tile with im2col data for this tile
        ! This is the HOT path - data goes directly from input to GEMM
        do out_idx = 1, tile_cols
          ! Convert linear index to 2D position
          out_y = ((tile_start + out_idx - 2) / W_out) + 1
          out_x = mod(tile_start + out_idx - 2, W_out) + 1
          
          ! Extract the patch for this output location
          do c_idx = 1, C
            do ky = 1, kernel_size
              do kx = 1, kernel_size
                in_y = (out_y - 1) * stride + ky - pad
                in_x = (out_x - 1) * stride + kx - pad
                
                if (in_y >= 1 .and. in_y <= H .and. in_x >= 1 .and. in_x <= W) then
                  input_offset = ((n_idx-1)*C + (c_idx-1))*H*W + (in_y-1)*W + in_x
                  ! Calculate position in flattened kernel space
                  weight_offset = ((c_idx-1)*kernel_size + (ky-1))*kernel_size + kx
                  ! Store in column-major format for GEMM
                  input_tile((out_idx-1)*input_rows + weight_offset) = input(input_offset)
                else
                  ! Calculate position in flattened kernel space (same as non-padding case)
                  weight_offset = ((c_idx-1)*kernel_size + (ky-1))*kernel_size + kx
                  ! Store 0 for padded regions
                  input_tile((out_idx-1)*input_rows + weight_offset) = 0.0
                end if
              end do
            end do
          end do
        end do
        
        ! HOT GEMM - data is still in cache!
        ! weights: K x (C*kernel_size*kernel_size)
        ! input_tile: (C*kernel_size*kernel_size) x tile_cols
        ! output_tile: K x tile_cols
        call gemm_simd_avx512(weights, input_tile, output_tile, &
                             K, tile_cols, input_rows, &
                             1.0, 0.0)
        
        ! Write results back to output
        do out_idx = 1, tile_cols
          out_y = ((tile_start + out_idx - 2) / W_out) + 1
          out_x = mod(tile_start + out_idx - 2, W_out) + 1
          
          do k_idx = 1, K
            ! GEMM output is column-major: C(i,j) = C(i + (j-1)*m)
            output(((n_idx-1)*K + (k_idx-1))*H_out*W_out + (out_y-1)*W_out + out_x) = &
              output_tile(k_idx + (out_idx-1)*K)
          end do
        end do
      end do
      !$omp end parallel do
    end do
    
    call cpu_time(end_time)
    
    ! Calculate performance
    conv2d_fused_hot_cache = real((end_time - start_time) * 1000.0, real32)
    
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    
    gflops = real(total_flops, real64) / (real(conv2d_fused_hot_cache, real64) * 1.0e6_real64)
    
    print '(A,I0,A,I0)', "   Tile size: ", TILE_SIZE, " output locations"
    print '(A,F8.2,A,F8.1,A)', "   Performance: ", conv2d_fused_hot_cache, " ms, ", gflops, " GFLOPS"
    
    ! Cleanup thread-local allocations will happen automatically
    deallocate(input_tile, output_tile)
    
  end function conv2d_fused_hot_cache

end module cpu_conv2d_fused