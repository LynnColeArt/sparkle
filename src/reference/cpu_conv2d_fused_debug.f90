! Debug version of fused im2col+GEMM to trace the issue
module cpu_conv2d_fused_debug
  use iso_fortran_env, only: real32, real64, int32, int64
  use gemm_simd_optimized, only: gemm_simd_avx512
  implicit none
  
  private
  public :: conv2d_fused_debug
  
contains

  real(real32) function conv2d_fused_debug(input, weights, output, &
                                           N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    real(real64) :: start_time, end_time
    integer(int64) :: total_flops
    
    ! Process just first tile for debugging
    integer, parameter :: TILE_SIZE = 64
    real(real32) :: im2col_tile(C * kernel_size * kernel_size * N, TILE_SIZE)
    real(real32) :: output_tile(K, TILE_SIZE)
    integer :: input_rows, total_output_locs
    
    integer :: tile_cols
    integer :: h_out_idx, w_out_idx, out_idx, out_loc
    integer :: n_idx, c_idx, kh_idx, kw_idx
    integer :: h_in, w_in, in_idx, col_idx
    integer :: k_idx, linear_idx, output_offset
    
    ! Calculate dimensions
    input_rows = C * kernel_size * kernel_size * N
    total_output_locs = H_out * W_out
    tile_cols = min(TILE_SIZE, total_output_locs)
    
    print *, "🔍 Debug fused convolution"
    print '(A,I0,A,I0)', "   Input rows (C*k*k*N): ", input_rows, ", tile cols: ", tile_cols
    
    call cpu_time(start_time)
    
    ! Initialize
    output = 0.0
    im2col_tile = 0.0
    output_tile = 0.0
    
    ! Fill im2col for first tile only
    do out_loc = 1, tile_cols
      h_out_idx = ((out_loc - 1) / W_out) + 1
      w_out_idx = mod(out_loc - 1, W_out) + 1
      
      if (out_loc <= 3) then
        print '(A,I0,A,I0,A,I0)', "   Out loc ", out_loc, " -> h_out=", h_out_idx, ", w_out=", w_out_idx
      end if
      
      ! Extract patch
      do n_idx = 1, N
        do c_idx = 1, C
          do kh_idx = 1, kernel_size
            do kw_idx = 1, kernel_size
              h_in = (h_out_idx-1)*stride + kh_idx - pad
              w_in = (w_out_idx-1)*stride + kw_idx - pad
              
              ! Calculate im2col index
              col_idx = ((((kh_idx-1)*kernel_size + (kw_idx-1))*C + (c_idx-1))*N + (n_idx-1))*tile_cols + out_loc
              
              if (h_in >= 1 .and. h_in <= H .and. w_in >= 1 .and. w_in <= W) then
                in_idx = ((n_idx-1)*C + (c_idx-1))*H*W + (h_in-1)*W + w_in
                ! Actually, col_idx calculation is wrong - let me fix it
                ! For column-major: row + (col-1)*nrows
                col_idx = ((((kh_idx-1)*kernel_size + (kw_idx-1))*C + (c_idx-1))*N + (n_idx-1)) + 1
                im2col_tile(col_idx, out_loc) = input(in_idx)
              else
                col_idx = ((((kh_idx-1)*kernel_size + (kw_idx-1))*C + (c_idx-1))*N + (n_idx-1)) + 1
                im2col_tile(col_idx, out_loc) = 0.0
              end if
            end do
          end do
        end do
      end do
    end do
    
    ! Show first column of im2col
    print *, "   First column of im2col (should have", input_rows, "elements):"
    print '(A,10F8.3)', "   ", im2col_tile(1:min(10, input_rows), 1)
    
    ! GEMM
    call gemm_simd_avx512(weights, im2col_tile(:, 1:tile_cols), output_tile(:, 1:tile_cols), &
                         K, tile_cols, input_rows, 1.0, 0.0)
    
    ! Show GEMM output
    print *, "   GEMM output (K x tile_cols):"
    do k_idx = 1, K
      print '(A,I0,A,10F8.3)', "   Filter ", k_idx, ": ", output_tile(k_idx, 1:min(10, tile_cols))
    end do
    
    ! Write results back
    do out_loc = 1, tile_cols
      h_out_idx = ((out_loc - 1) / W_out) + 1
      w_out_idx = mod(out_loc - 1, W_out) + 1
      
      do k_idx = 1, K
        output_offset = (k_idx-1)*H_out*W_out + (h_out_idx-1)*W_out + w_out_idx
        output(output_offset) = output_tile(k_idx, out_loc)
        
        if (out_loc <= 3) then
          print '(A,I0,A,I0,A,I0,A,F8.3)', "   Storing filter ", k_idx, " loc ", out_loc, &
            " to output[", output_offset, "] = ", output_tile(k_idx, out_loc)
        end if
      end do
    end do
    
    call cpu_time(end_time)
    conv2d_fused_debug = real((end_time - start_time) * 1000.0, real32)
    
  end function conv2d_fused_debug

end module cpu_conv2d_fused_debug