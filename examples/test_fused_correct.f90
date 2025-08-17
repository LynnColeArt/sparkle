program test_fused_correct
  use iso_fortran_env
  use cpu_conv2d_fused_correct
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time1, time2, max_diff
  integer :: i
  
  print *, "=== TESTING CORRECTED FUSED CONVOLUTION ==="
  print *, ""
  
  ! Start with tiny test for correctness
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
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Tiny test: ", N, "x", C, "x", H, "x", W
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  ! Simple test pattern
  input = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]
  weights = 1.0 / 9.0  ! averaging filter
  
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  output2 = 0.0
  time2 = conv2d_fused_correct(input, weights, output2, &
                              N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  print *, "Unfused output:"
  print '(3F8.3)', output1(1:3)
  print '(3F8.3)', output1(4:6)
  print '(3F8.3)', output1(7:9)
  
  print *, ""
  print *, "Fused output:"
  print '(3F8.3)', output2(1:3)
  print '(3F8.3)', output2(4:6)
  print '(3F8.3)', output2(7:9)
  
  max_diff = maxval(abs(output1 - output2))
  print *, ""
  print '(A,E12.5)', "Max difference: ", max_diff
  
  if (max_diff < 1e-5) then
    print *, "✅ Tiny test PASSED!"
  else
    print *, "❌ Tiny test FAILED!"
  end if
  
  ! Now test larger size
  deallocate(input, weights, output1, output2)
  
  print *, ""
  print *, "=== LARGER TEST ==="
  
  N = 1
  C = 64
  H = 56
  W = 56
  K = 64
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  call random_number(input)
  call random_number(weights)
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Problem size: ", N, "x", C, "x", H, "x", W
  print *, ""
  
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  output2 = 0.0
  time2 = conv2d_fused_correct(input, weights, output2, &
                              N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  max_diff = maxval(abs(output1 - output2))
  
  print *, ""
  print *, "=== RESULTS ==="
  print '(A,F10.2,A)', "Unfused time: ", time1, " ms"
  print '(A,F10.2,A)', "Fused time: ", time2, " ms"
  print '(A,F10.2,A)', "Speedup: ", time1 / time2, "x"
  print '(A,E12.5)', "Max difference: ", max_diff
  
  if (max_diff < 1e-4) then
    print *, "✅ Results match!"
  else
    print *, "❌ Results differ!"
  end if
  
  deallocate(input, weights, output1, output2)
  
end program test_fused_correct