! Fixed production interface with optimized CPU implementation
module sparkle_conv2d_fixed
  use iso_fortran_env
  implicit none
  
  private
  public :: conv2d_cpu
  
  interface
    real(real32) function conv2d_fused_final(input, weights, output, &
                                            N, C, H, W, K, kernel_size, stride, pad, H_out, W_out) &
                                            bind(C, name="conv2d_fused_final_wrapper")
      import :: real32
      real(real32), intent(in) :: input(*), weights(*)
      real(real32), intent(out) :: output(*)
      integer, value :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    end function
  end interface
  
contains
  
  subroutine conv2d_cpu(input, weights, output, &
                       N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    
    real(real32) :: time_ms, gflops
    integer(int64) :: total_flops
    
    ! Use optimized fused implementation
    time_ms = conv2d_fused_final(input, weights, output, &
                                N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    
    ! Calculate and report performance
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    gflops = real(total_flops, real32) / (time_ms * 1.0e6)
    
    print '(A,F6.2,A,F6.1,A)', "CPU conv2d: ", time_ms, " ms, ", gflops, " GFLOPS"
  end subroutine conv2d_cpu
  
end module sparkle_conv2d_fixed