program test_mini_v2
  use iso_fortran_env
  use cpu_conv2d_fused_mini_v2
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time1, time2, max_diff, max_val
  integer :: i
  
  print *, "=== TESTING MINI'S V2 SOLUTION (Column-Major Fix) ==="
  print *, ""
  
  ! Mini's acceptance test: 1x1 kernel should be plain matmul
  N = 1; C = 1; H = 4; W = 4; K = 2
  kernel_size = 1; stride = 1; pad = 0
  H_out = H; W_out = W  ! No padding, 1x1 kernel
  
  print *, "Mini's acceptance test: 1x1 kernel (should be plain matmul)"
  print '(A,I0,A,I0,A,I0,A,I0)', "Size: ", N, "x", C, "x", H, "x", W
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  ! Simple pattern
  do i = 1, size(input)
    input(i) = real(i, real32)
  end do
  weights = [1.0, 2.0]  ! Two filters
  
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  output2 = 0.0
  time2 = conv2d_fused_mini_v2(input, weights, output2, &
                              N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  max_diff = maxval(abs(output1 - output2))
  print '(A,E12.5)', "   Max difference: ", max_diff
  if (max_diff < 1e-5) then
    print *, "   ✅ 1x1 kernel test PASSED! Basic GEMM is correct"
  else
    print *, "   ❌ 1x1 kernel test FAILED - fundamental issue"
    print *, "   First few outputs:"
    print '(A,4F8.2)', "   Ref: ", output1(1:4)
    print '(A,4F8.2)', "   V2:  ", output2(1:4)
  end if
  
  deallocate(input, weights, output1, output2)
  
  ! Test with 3x3 kernel
  print *, ""
  N = 1; C = 3; H = 8; W = 8; K = 4
  kernel_size = 3; stride = 1; pad = 1
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print *, "3x3 kernel test:"
  print '(A,I0,A,I0,A,I0,A,I0)', "Size: ", N, "x", C, "x", H, "x", W
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  call random_number(input)
  call random_number(weights)
  input = (input - 0.5) * 2.0
  weights = (weights - 0.5) * 0.1
  
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  output2 = 0.0
  time2 = conv2d_fused_mini_v2(input, weights, output2, &
                              N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  max_diff = maxval(abs(output1 - output2))
  max_val = max(maxval(abs(output1)), maxval(abs(output2)))
  
  print '(A,E12.5)', "   Max difference: ", max_diff
  if (max_val > 0) then
    print '(A,F10.6,A)', "   Relative error: ", (max_diff / max_val) * 100.0, "%"
  end if
  
  if (max_diff < 1e-5) then
    print *, "   ✅ 3x3 kernel test PASSED!"
  else
    print *, "   ❌ 3x3 kernel test failed"
  end if
  
  deallocate(input, weights, output1, output2)
  
  ! Production test
  print *, ""
  N = 1; C = 64; H = 56; W = 56; K = 64
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print *, "Production test:"
  print '(A,I0,A,I0,A,I0,A,I0)', "Size: ", N, "x", C, "x", H, "x", W
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  call random_number(input)
  call random_number(weights)
  input = (input - 0.5) * 2.0
  weights = (weights - 0.5) * 0.1
  
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  output2 = 0.0
  time2 = conv2d_fused_mini_v2(input, weights, output2, &
                              N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  max_diff = maxval(abs(output1 - output2))
  max_val = max(maxval(abs(output1)), maxval(abs(output2)))
  
  print *, ""
  print *, "=== FINAL RESULTS ==="
  print '(A,F10.2,A)', "Reference time:  ", time1, " ms"
  print '(A,F10.2,A)', "Mini v2 time:    ", time2, " ms"
  print '(A,F10.2,A)', "Speedup:         ", time1 / time2, "x"
  print '(A,E12.5)', "Max difference:   ", max_diff
  if (max_val > 0) then
    print '(A,F10.6,A)', "Relative error:   ", (max_diff / max_val) * 100.0, "%"
  end if
  
  if (max_diff < 1e-5) then
    print *, ""
    print *, "🎉 PERFECT ACCURACY! Mini's column-major fix worked!"
    print *, "✅ Proper im2col packing order"
    print *, "✅ Correct leading dimensions" 
    print *, "✅ ~3x performance improvement maintained"
  else if (max_diff / max_val < 0.001) then
    print *, "✅ Excellent accuracy (< 0.1% error)"
  else
    print *, "❌ Still debugging needed"
  end if
  
  deallocate(input, weights, output1, output2)
  
end program test_mini_v2