program test_debug_fused
  use iso_fortran_env
  use cpu_conv2d_fused_debug
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time1, time2
  integer :: i
  
  \! Very small test
  N = 1
  C = 1
  H = 3
  W = 3
  K = 2
  kernel_size = 3
  stride = 1
  pad = 1
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Test: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0)', "Output: ", H_out, "x", W_out
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  \! Simple input pattern
  input = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]
  
  \! Different weights for each filter
  weights(1:9) = 0.1   \! Filter 1
  weights(10:18) = 0.2 \! Filter 2
  
  print *, ""
  print *, "=== UNFUSED ==="
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  print *, "=== FUSED DEBUG ==="
  output2 = 0.0
  time2 = conv2d_fused_debug(input, weights, output2, &
                            N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  print *, "=== RESULTS ==="
  print *, "Unfused output:"
  do i = 1, K
    print '(A,I0,A,9F8.3)', "Filter ", i, ": ", output1((i-1)*9+1:i*9)
  end do
  
  print *, ""
  print *, "Fused output:"
  do i = 1, K
    print '(A,I0,A,9F8.3)', "Filter ", i, ": ", output2((i-1)*9+1:i*9)
  end do
  
  deallocate(input, weights, output1, output2)
  
end program test_debug_fused
EOF < /dev/null
