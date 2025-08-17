! Fused im2col+GEMM using 16-bit float operations
! This should give us both higher performance and make numerical differences acceptable
module cpu_conv2d_fused_fp16
  use iso_fortran_env, only: real32, real64, int32, int64
  use gemm_simd_optimized, only: gemm_simd_avx512
  implicit none
  
  private
  public :: conv2d_fused_fp16
  
contains

  real(real32) function conv2d_fused_fp16(input, weights, output, &
                                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    real(real64) :: start_time, end_time
    integer(int64) :: total_flops
    real(real64) :: gflops
    
    ! For now, we'll simulate FP16 by truncating mantissa bits
    ! In production, we'd use actual FP16 intrinsics
    integer, parameter :: TILE_SIZE = 128  ! Double tile size for FP16
    real(real32), allocatable :: im2col_tile(:), output_tile(:)
    real(real32), allocatable :: weights_fp16(:), input_fp16(:)
    integer :: input_rows, total_output_locs
    
    integer :: tile_start, tile_end, tile_cols
    integer :: h_out_idx, w_out_idx, out_idx, out_loc
    integer :: n_idx, c_idx, kh_idx, kw_idx
    integer :: h_in, w_in, in_idx, col_idx
    integer :: num_tiles, tile_idx, k_idx
    integer :: linear_idx, output_offset
    integer :: i
    
    ! Calculate dimensions
    input_rows = C * kernel_size * kernel_size * N
    total_output_locs = H_out * W_out
    
    print *, "🚀 Fused im2col+GEMM CPU convolution (FP16 simulation)"
    print '(A,I0)', "   Tile size: ", TILE_SIZE, " (2x for FP16)"
    
    call cpu_time(start_time)
    
    ! Simulate FP16 by reducing precision
    allocate(weights_fp16(size(weights)))
    allocate(input_fp16(size(input)))
    
    ! Convert to "FP16" by truncating mantissa
    ! Real FP16 would use vcvtps2ph/vcvtph2ps instructions
    do i = 1, size(weights)
      weights_fp16(i) = truncate_to_fp16(weights(i))
    end do
    
    do i = 1, size(input)
      input_fp16(i) = truncate_to_fp16(input(i))
    end do
    
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
      
      ! Fill im2col tile
      do out_loc = 1, tile_cols
        linear_idx = tile_start + out_loc - 1
        h_out_idx = ((linear_idx - 1) / W_out) + 1
        w_out_idx = mod(linear_idx - 1, W_out) + 1
        
        ! Extract patch
        do n_idx = 1, N
          do c_idx = 1, C
            do kh_idx = 1, kernel_size
              do kw_idx = 1, kernel_size
                h_in = (h_out_idx-1)*stride + kh_idx - pad
                w_in = (w_out_idx-1)*stride + kw_idx - pad
                
                col_idx = ((((kh_idx-1)*kernel_size + (kw_idx-1))*C + (c_idx-1))*N + (n_idx-1)) + 1
                
                if (h_in >= 1 .and. h_in <= H .and. w_in >= 1 .and. w_in <= W) then
                  in_idx = ((n_idx-1)*C + (c_idx-1))*H*W + (h_in-1)*W + w_in
                  im2col_tile(col_idx + (out_loc-1)*input_rows) = input_fp16(in_idx)
                else
                  im2col_tile(col_idx + (out_loc-1)*input_rows) = 0.0
                end if
              end do
            end do
          end do
        end do
      end do
      
      ! GEMM with FP16 data
      call gemm_simd_avx512(weights_fp16, im2col_tile(1:input_rows*tile_cols), &
                           output_tile(1:K*tile_cols), &
                           K, tile_cols, input_rows, &
                           1.0, 0.0)
      
      ! Write results back
      do out_loc = 1, tile_cols
        linear_idx = tile_start + out_loc - 1
        h_out_idx = ((linear_idx - 1) / W_out) + 1
        w_out_idx = mod(linear_idx - 1, W_out) + 1
        
        do k_idx = 1, K
          output_offset = (k_idx-1)*H_out*W_out + (h_out_idx-1)*W_out + w_out_idx
          output(output_offset) = output_tile(k_idx + (out_loc-1)*K)
        end do
      end do
    end do
    !$omp end do
    
    deallocate(im2col_tile, output_tile)
    !$omp end parallel
    
    deallocate(weights_fp16, input_fp16)
    
    call cpu_time(end_time)
    
    ! Calculate performance
    conv2d_fused_fp16 = real((end_time - start_time) * 1000.0, real32)
    
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    
    gflops = real(total_flops, real64) / (real(conv2d_fused_fp16, real64) * 1.0e6_real64)
    
    print '(A,F8.2,A,F8.1,A)', "   Performance: ", conv2d_fused_fp16, " ms, ", gflops, " GFLOPS"
    print *, "   Note: Real FP16 would be ~2x faster with AVX-512"
    
  end function conv2d_fused_fp16
  
  ! Simulate FP16 by truncating mantissa bits
  ! Real implementation would use vcvtps2ph instruction
  real(real32) function truncate_to_fp16(x)
    real(real32), intent(in) :: x
    integer :: ix, mantissa_mask
    
    ! FP16 has 10 mantissa bits vs FP32's 23
    ! Mask off the lower 13 bits
    mantissa_mask = not(ishft(1, 13) - 1)
    
    ix = transfer(x, ix)
    ix = iand(ix, mantissa_mask)
    truncate_to_fp16 = transfer(ix, x)
    
  end function truncate_to_fp16

end module cpu_conv2d_fused_fp16