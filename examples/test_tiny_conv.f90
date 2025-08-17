program test_tiny_conv
  use iso_fortran_env
  use cpu_conv2d_fused_simple
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  integer :: i
  real(real32) :: time1, time2
  
  ! Tiny test case for debugging
  N = 1
  C = 1
  H = 3
  W = 3
  K = 1
  kernel_size = 3
  stride = 1
  pad = 1
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print *, "=== TINY CONVOLUTION TEST ==="
  print '(A,I0,A,I0,A,I0,A,I0)', "Input: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0,A,I0,A,I0)', "Output: ", N, "x", K, "x", H_out, "x", W_out
  print *, ""
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  ! Initialize with simple pattern
  input = [1.0, 2.0, 3.0, &
           4.0, 5.0, 6.0, &
           7.0, 8.0, 9.0]
  
  ! Simple averaging kernel
  weights = 1.0 / 9.0
  
  print *, "Input (3x3):"
  print '(3F6.1)', input(1:3)
  print '(3F6.1)', input(4:6)
  print '(3F6.1)', input(7:9)
  print *, ""
  
  print *, "Weights (3x3 averaging kernel):"
  print '(9F6.3)', weights
  print *, ""
  
  ! Test unfused
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  print *, "Unfused output (3x3):"
  print '(3F8.3)', output1(1:3)
  print '(3F8.3)', output1(4:6)
  print '(3F8.3)', output1(7:9)
  
  ! Test fused
  output2 = 0.0
  time2 = conv2d_fused_simple(input, weights, output2, &
                             N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  print *, "Fused output (3x3):"
  print '(3F8.3)', output2(1:3)
  print '(3F8.3)', output2(4:6)
  print '(3F8.3)', output2(7:9)
  
  print *, ""
  print *, "Differences:"
  do i = 1, 9
    if (abs(output1(i) - output2(i)) > 1e-6) then
      print '(A,I0,A,F8.3,A,F8.3,A,F8.3)', &
        "Position ", i, ": unfused=", output1(i), " fused=", output2(i), " diff=", abs(output1(i) - output2(i))
    end if
  end do
  
  ! Expected results for averaging kernel with padding:
  ! Top-left: (0+0+0+0+1+2+0+4+5)/9 = 12/9 = 1.333
  ! Top-center: (0+0+0+1+2+3+4+5+6)/9 = 21/9 = 2.333
  ! etc.
  
  deallocate(input, weights, output1, output2)
  
end program test_tiny_conv