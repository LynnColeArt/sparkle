! Direct convolution implementation for CPU
! 
! This version implements convolution directly without im2col transformation
! Uses tiling for cache efficiency and SIMD for computation

module cpu_conv2d_direct
  use iso_fortran_env, only: real32, real64, int32, int64
  implicit none
  
  private
  public :: conv2d_direct
  
contains

  real(real32) function conv2d_direct(input, weights, output, &
                                     N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    real(real64) :: start_time, end_time
    integer(int64) :: total_flops
    real(real64) :: gflops
    
    integer :: n_idx, k_idx, h_out_idx, w_out_idx, c_idx, kh_idx, kw_idx
    integer :: h_in, w_in, input_offset, weight_offset, output_offset
    real(real32) :: sum
    
    print *, "🎯 Direct CPU convolution (no im2col)"
    
    call cpu_time(start_time)
    
    ! Initialize output to zero
    output = 0.0
    
    ! Direct convolution with optimal loop ordering for cache
    !$omp parallel do collapse(2) private(n_idx, k_idx, h_out_idx, w_out_idx, c_idx, kh_idx, kw_idx, &
    !$omp&                               h_in, w_in, input_offset, weight_offset, &
    !$omp&                               output_offset, sum)
    do n_idx = 1, N
      do k_idx = 1, K
        do h_out_idx = 1, H_out
          do w_out_idx = 1, W_out
            sum = 0.0
            
            ! Compute one output value
            do c_idx = 1, C
              do kh_idx = 1, kernel_size
                do kw_idx = 1, kernel_size
                  h_in = (h_out_idx - 1) * stride + kh_idx - pad
                  w_in = (w_out_idx - 1) * stride + kw_idx - pad
                  
                  if (h_in >= 1 .and. h_in <= H .and. w_in >= 1 .and. w_in <= W) then
                    input_offset = ((n_idx-1)*C + (c_idx-1))*H*W + (h_in-1)*W + w_in
                    weight_offset = ((k_idx-1)*C + (c_idx-1))*kernel_size*kernel_size + &
                                   (kh_idx-1)*kernel_size + kw_idx
                    sum = sum + input(input_offset) * weights(weight_offset)
                  end if
                end do
              end do
            end do
            
            output_offset = ((n_idx-1)*K + (k_idx-1))*H_out*W_out + (h_out_idx-1)*W_out + w_out_idx
            output(output_offset) = sum
          end do
        end do
      end do
    end do
    !$omp end parallel do
    
    call cpu_time(end_time)
    
    ! Calculate performance
    conv2d_direct = real((end_time - start_time) * 1000.0, real32)
    
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    
    gflops = real(total_flops, real64) / (real(conv2d_direct, real64) * 1.0e6_real64)
    
    print '(A,F8.2,A,F8.1,A)', "   Performance: ", conv2d_direct, " ms, ", gflops, " GFLOPS"
    
  end function conv2d_direct

end module cpu_conv2d_direct