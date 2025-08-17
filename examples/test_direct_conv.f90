program test_direct_conv
  use iso_fortran_env
  use cpu_conv2d_direct
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time1, time2, max_diff
  integer :: i
  
  print *, "=== TESTING DIRECT CONVOLUTION ==="
  print *, ""
  
  ! Test parameters
  N = 1
  C = 64
  H = 56
  W = 56
  K = 64
  kernel_size = 3
  stride = 1
  pad = 1
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Problem size: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0,A,I0,A,I0)', "Output size: ", N, "x", K, "x", H_out, "x", W_out
  print *, ""
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  ! Initialize with test data
  call random_number(input)
  call random_number(weights)
  
  ! Test 1: Original approach with im2col
  print *, "Test 1: UNFUSED approach (im2col + GEMM)"
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  
  ! Test 2: Direct convolution
  print *, "Test 2: DIRECT convolution (no im2col)"
  output2 = 0.0
  time2 = conv2d_direct(input, weights, output2, &
                       N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Verify results match
  max_diff = 0.0
  do i = 1, size(output1)
    max_diff = max(max_diff, abs(output1(i) - output2(i)))
  end do
  
  print *, ""
  print *, "=== RESULTS ==="
  print '(A,F10.2,A)', "Im2col+GEMM time: ", time1, " ms"
  print '(A,F10.2,A)', "Direct time: ", time2, " ms"
  print '(A,F10.2,A)', "Speedup: ", time2 / time1, "x (negative means direct is slower)"
  print '(A,E12.5)', "Max difference: ", max_diff
  
  if (max_diff < 1e-4) then
    print *, "✅ Results match!"
  else
    print *, "❌ Results differ!"
  end if
  
  ! Test on a tiny example to verify correctness
  print *, ""
  print *, "=== TINY TEST FOR CORRECTNESS ==="
  
  deallocate(input, weights, output1, output2)
  
  N = 1; C = 1; H = 3; W = 3; K = 1
  allocate(input(9), weights(9), output1(9), output2(9))
  
  input = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]
  weights = 1.0 / 9.0  ! averaging filter
  
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  output2 = 0.0
  time2 = conv2d_direct(input, weights, output2, &
                       N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  print *, "Output from im2col+GEMM:"
  print '(3F8.3)', output1(1:3)
  print '(3F8.3)', output1(4:6)
  print '(3F8.3)', output1(7:9)
  
  print *, ""
  print *, "Output from direct:"
  print '(3F8.3)', output2(1:3)
  print '(3F8.3)', output2(4:6)
  print '(3F8.3)', output2(7:9)
  
  deallocate(input, weights, output1, output2)
  
end program test_direct_conv