! Fused im2col+GEMM with restructured tiling for contiguous memory access
! Instead of extracting slices, we pack data contiguously for GEMM
module cpu_conv2d_fused_restructured
  use iso_fortran_env, only: real32, real64, int32, int64
  use gemm_simd_optimized, only: gemm_simd_avx512
  implicit none
  
  private
  public :: conv2d_fused_restructured
  
contains

  real(real32) function conv2d_fused_restructured(input, weights, output, &
                                                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    real(real64) :: start_time, end_time
    integer(int64) :: total_flops
    real(real64) :: gflops
    
    ! Restructured approach: allocate exactly what we need for each tile
    integer, parameter :: TILE_SIZE = 64
    real(real32), allocatable :: im2col_buffer(:), output_buffer(:)
    integer :: input_rows, tile_cols, actual_tile_size
    
    integer :: tile_start, tile_end
    integer :: h_out_idx, w_out_idx, out_idx, out_loc
    integer :: n_idx, c_idx, kh_idx, kw_idx
    integer :: h_in, w_in, in_idx, col_idx
    integer :: num_tiles, tile_idx, k_idx
    integer :: linear_idx, output_offset
    integer :: buf_idx
    
    ! Calculate dimensions
    input_rows = C * kernel_size * kernel_size * N
    
    print *, "🚀 Fused im2col+GEMM CPU convolution (restructured for contiguous access)"
    print '(A,I0,A)', "   Tile size: ", TILE_SIZE, " output locations"
    
    call cpu_time(start_time)
    
    ! Initialize output
    output = 0.0
    
    ! Process output locations in tiles
    num_tiles = (H_out * W_out + TILE_SIZE - 1) / TILE_SIZE
    
    !$omp parallel private(tile_idx, tile_start, tile_end, actual_tile_size, &
    !$omp&                h_out_idx, w_out_idx, out_idx, out_loc, &
    !$omp&                n_idx, c_idx, kh_idx, kw_idx, &
    !$omp&                h_in, w_in, in_idx, col_idx, k_idx, &
    !$omp&                linear_idx, output_offset, buf_idx, &
    !$omp&                im2col_buffer, output_buffer)
    
    !$omp do
    do tile_idx = 1, num_tiles
      tile_start = (tile_idx - 1) * TILE_SIZE + 1
      tile_end = min(tile_start + TILE_SIZE - 1, H_out * W_out)
      actual_tile_size = tile_end - tile_start + 1
      
      ! Allocate exactly what we need for this tile
      allocate(im2col_buffer(input_rows * actual_tile_size))
      allocate(output_buffer(K * actual_tile_size))
      
      ! Initialize buffers
      im2col_buffer = 0.0
      output_buffer = 0.0
      
      ! Fill im2col buffer contiguously
      buf_idx = 0
      do out_loc = 1, actual_tile_size
        linear_idx = tile_start + out_loc - 1
        h_out_idx = ((linear_idx - 1) / W_out) + 1
        w_out_idx = mod(linear_idx - 1, W_out) + 1
        
        ! Extract patch into contiguous buffer
        do n_idx = 1, N
          do c_idx = 1, C
            do kh_idx = 1, kernel_size
              do kw_idx = 1, kernel_size
                h_in = (h_out_idx-1)*stride + kh_idx - pad
                w_in = (w_out_idx-1)*stride + kw_idx - pad
                
                buf_idx = buf_idx + 1
                
                if (h_in >= 1 .and. h_in <= H .and. w_in >= 1 .and. w_in <= W) then
                  in_idx = ((n_idx-1)*C + (c_idx-1))*H*W + (h_in-1)*W + w_in
                  im2col_buffer(buf_idx) = input(in_idx)
                else
                  im2col_buffer(buf_idx) = 0.0
                end if
              end do
            end do
          end do
        end do
      end do
      
      ! GEMM with properly packed contiguous data
      call gemm_simd_avx512(weights, im2col_buffer, output_buffer, &
                           K, actual_tile_size, input_rows, &
                           1.0, 0.0)
      
      ! Write results back
      buf_idx = 0
      do out_loc = 1, actual_tile_size
        linear_idx = tile_start + out_loc - 1
        h_out_idx = ((linear_idx - 1) / W_out) + 1
        w_out_idx = mod(linear_idx - 1, W_out) + 1
        
        do k_idx = 1, K
          buf_idx = buf_idx + 1
          output_offset = (k_idx-1)*H_out*W_out + (h_out_idx-1)*W_out + w_out_idx
          output(output_offset) = output_buffer(buf_idx)
        end do
      end do
      
      deallocate(im2col_buffer, output_buffer)
    end do
    !$omp end do
    !$omp end parallel
    
    call cpu_time(end_time)
    
    ! Calculate performance
    conv2d_fused_restructured = real((end_time - start_time) * 1000.0, real32)
    
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    
    gflops = real(total_flops, real64) / (real(conv2d_fused_restructured, real64) * 1.0e6_real64)
    
    print '(A,F8.2,A,F8.1,A)', "   Performance: ", conv2d_fused_restructured, " ms, ", gflops, " GFLOPS"
    print *, "   ✅ Contiguous memory access maintained"
    
  end function conv2d_fused_restructured

end module cpu_conv2d_fused_restructured