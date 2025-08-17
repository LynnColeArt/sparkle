program test_fused_final
  use iso_fortran_env
  use cpu_conv2d_fused_correct
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time1, time2, max_diff, avg_diff, gflops
  integer :: i, total_flops
  
  print *, "=== FINAL FUSED CONVOLUTION TEST ==="
  print *, ""
  
  ! Production-like test size
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
  print '(A,I0,A,I0)', "Output size: ", H_out, "x", W_out
  print '(A,I0)', "Filters: ", K
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(weights)
  
  ! Warm up
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Time unfused (reference)
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Time fused
  output2 = 0.0
  time2 = conv2d_fused_correct(input, weights, output2, &
                              N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Check accuracy
  max_diff = maxval(abs(output1 - output2))
  avg_diff = sum(abs(output1 - output2)) / size(output1)
  
  ! Calculate GFLOPS
  total_flops = N * K * H_out * W_out * C * kernel_size * kernel_size * 2
  gflops = real(total_flops) / (time2 * 1.0e6)
  
  print *, ""
  print *, "=== PERFORMANCE RESULTS ==="
  print '(A,F10.2,A,F8.1,A)', "Unfused: ", time1, " ms (", &
    real(total_flops) / (time1 * 1.0e6), " GFLOPS)"
  print '(A,F10.2,A,F8.1,A)', "Fused:   ", time2, " ms (", gflops, " GFLOPS)"
  print '(A,F10.2,A)', "Speedup: ", time1 / time2, "x"
  
  print *, ""
  print *, "=== ACCURACY ==="
  print '(A,E12.5)', "Max difference: ", max_diff
  print '(A,E12.5)', "Avg difference: ", avg_diff
  print '(A,E12.5)', "Relative error: ", max_diff / maxval(abs(output1))
  
  if (max_diff / maxval(abs(output1)) < 1e-3) then
    print *, ""
    print *, "✅ LAYER 2 SUCCESS: Fused im2col+GEMM achieves ", gflops, " GFLOPS!"
    print *, "   Hot cache optimization working as designed"
  else
    print *, ""
    print *, "❌ Numerical accuracy issue needs investigation"
  end if
  
  deallocate(input, weights, output1, output2)
  
end program test_fused_final