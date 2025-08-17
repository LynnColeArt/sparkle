program test_fused_tiny
  use iso_fortran_env
  use cpu_conv2d_fused_simple
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time1, time2, max_diff
  integer :: i
  
  print *, "=== TESTING SIMPLE FUSED (that worked before) ==="
  print *, ""
  
  ! Test with moderate size
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
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Test size: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0)', "Output size: ", H_out, "x", W_out
  print '(A,I0)', "Filters: ", K
  print *, ""
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  call random_number(input)
  call random_number(weights)
  
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  output2 = 0.0
  time2 = conv2d_fused_simple(input, weights, output2, &
                             N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  max_diff = maxval(abs(output1 - output2))
  
  print *, ""
  print *, "=== RESULTS ==="
  print '(A,F10.2,A)', "Unfused time: ", time1, " ms"
  print '(A,F10.2,A)', "Fused time: ", time2, " ms"
  print '(A,F10.2,A)', "Speedup: ", time1 / time2, "x"
  print '(A,E12.5)', "Max difference: ", max_diff
  print '(A,F8.1,A)', "Performance: ", real(N) * real(K) * real(H_out) * real(W_out) * &
                      real(C) * real(kernel_size) * real(kernel_size) * 2.0 / (time2 * 1.0e6), " GFLOPS"
  
  if (max_diff < 1e-4) then
    print *, "✅ Simple fused still works!"
  else
    print *, "❌ Simple fused broken too!"
  end if
  
  deallocate(input, weights, output1, output2)
  
end program test_fused_tiny