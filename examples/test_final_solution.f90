program test_final_solution
  use iso_fortran_env
  use cpu_conv2d_fused_final
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time1, time2, max_diff, max_val
  integer :: i
  
  print *, "=== TESTING FINAL SOLUTION WITH MINI'S CONTRACT ==="
  print *, "🍩 Badge of Valor awarded to Mini!"
  print *, ""
  
  ! Mini's 1x1 kernel sanity trap
  N = 1; C = 1; H = 4; W = 4; K = 2
  kernel_size = 1; stride = 1; pad = 0
  H_out = H; W_out = W
  
  print *, "1×1 kernel fast-path test (should be reshape+GEMM):"
  print '(A,I0,A,I0,A,I0,A,I0)', "Size: ", N, "x", C, "x", H, "x", W
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  ! Simple sequential pattern
  do i = 1, size(input)
    input(i) = real(i, real32)
  end do
  weights = [1.0, 2.0]
  
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  output2 = 0.0
  time2 = conv2d_fused_final(input, weights, output2, &
                            N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, "Expected output (filter 1): 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16"
  print *, "Expected output (filter 2): 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32"
  print '(A,16F5.1)', "Reference: ", output1
  print '(A,16F5.1)', "Final:     ", output2
  
  max_diff = maxval(abs(output1 - output2))
  print '(A,E12.5)', "Max difference: ", max_diff
  
  if (max_diff < 1e-5) then
    print *, "✅ 1×1 kernel test PASSED! Contract is correct"
  else
    print *, "❌ 1×1 kernel test FAILED"
  end if
  
  deallocate(input, weights, output1, output2)
  
  ! 3x3 kernel test
  print *, ""
  N = 1; C = 3; H = 8; W = 8; K = 4
  kernel_size = 3; stride = 1; pad = 1
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print *, "3×3 kernel test:"
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
  time2 = conv2d_fused_final(input, weights, output2, &
                            N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  max_diff = maxval(abs(output1 - output2))
  max_val = max(maxval(abs(output1)), maxval(abs(output2)))
  
  print '(A,E12.5)', "Max difference: ", max_diff
  if (max_val > 0) then
    print '(A,F10.6,A)', "Relative error: ", (max_diff / max_val) * 100.0, "%"
  end if
  
  if (max_diff < 1e-5) then
    print *, "✅ 3×3 kernel test PASSED!"
  else
    print *, "❌ Still have accuracy issues"
  end if
  
  deallocate(input, weights, output1, output2)
  
  ! Production test
  print *, ""
  N = 1; C = 64; H = 56; W = 56; K = 64
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print *, "Production test (64×64×56×56):"
  
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
  time2 = conv2d_fused_final(input, weights, output2, &
                           N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Results
  max_diff = maxval(abs(output1 - output2))
  max_val = max(maxval(abs(output1)), maxval(abs(output2)))
  
  print *, ""
  print *, "=== FINAL RESULTS ==="
  print '(A,F10.2,A)', "Reference time:  ", time1, " ms"
  print '(A,F10.2,A)', "Final time:      ", time2, " ms"
  print '(A,F10.2,A)', "Speedup:         ", time1 / time2, "x"
  print '(A,E12.5)', "Max difference:   ", max_diff
  if (max_val > 0) then
    print '(A,F10.6,A)', "Relative error:   ", (max_diff / max_val) * 100.0, "%"
  end if
  
  print *, ""
  if (max_diff < 1e-5) then
    print *, "🎉 PERFECT ACCURACY ACHIEVED!"
    print *, "✅ Mini's im2col contract works perfectly"
    print *, "✅ Leading dimensions properly handled"
    print *, "✅ Column-major packing correct"
    print *, "✅ Layer 2 complete with full accuracy and performance!"
    print *, ""
    print *, "🍬 Time for candy! We did it!"
  else if (max_diff / max_val < 0.001) then
    print *, "✅ Excellent accuracy (< 0.1% error)"
  else
    print *, "❌ Still debugging needed"
  end if
  
  deallocate(input, weights, output1, output2)
  
end program test_final_solution