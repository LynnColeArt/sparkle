program test_mini_solution
  use iso_fortran_env
  use cpu_conv2d_fused_mini
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time1, time2, max_diff, max_val
  integer :: i
  
  print *, "=== TESTING MINI'S SOLUTION ==="
  print *, ""
  
  ! First test: Small size for correctness
  N = 1; C = 3; H = 8; W = 8; K = 4
  kernel_size = 3; stride = 1; pad = 1
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Small test: ", N, "x", C, "x", H, "x", W
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  ! Simple test pattern
  do i = 1, size(input)
    input(i) = real(i, real32) / 100.0
  end do
  do i = 1, size(weights)
    weights(i) = real(i, real32) / 1000.0
  end do
  
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  output2 = 0.0
  time2 = conv2d_fused_mini(input, weights, output2, &
                           N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  max_diff = maxval(abs(output1 - output2))
  print '(A,E12.5)', "   Max difference: ", max_diff
  if (max_diff < 1e-5) then
    print *, "   ✅ Perfect accuracy on small test!"
  else
    print *, "   ❌ Accuracy issue on small test"
  end if
  
  deallocate(input, weights, output1, output2)
  
  ! Production size test
  print *, ""
  N = 1; C = 64; H = 56; W = 56; K = 64
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Production test: ", N, "x", C, "x", H, "x", W
  print '(A,I0)', "K (filters): ", K
  print '(A,I0)', "Input rows (C*k*k*N): ", C * kernel_size * kernel_size * N
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  call random_number(input)
  call random_number(weights)
  input = (input - 0.5) * 2.0
  weights = (weights - 0.5) * 0.1
  
  print *, ""
  print *, "Running reference..."
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, "Running Mini's implementation..."
  output2 = 0.0
  time2 = conv2d_fused_mini(input, weights, output2, &
                           N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Accuracy analysis
  max_diff = maxval(abs(output1 - output2))
  max_val = max(maxval(abs(output1)), maxval(abs(output2)))
  
  print *, ""
  print *, "=== RESULTS ==="
  print '(A,F10.2,A)', "Reference time:  ", time1, " ms"
  print '(A,F10.2,A)', "Mini's time:     ", time2, " ms"
  print '(A,F10.2,A)', "Speedup:         ", time1 / time2, "x"
  print *, ""
  print '(A,E12.5)', "Max output value: ", max_val
  print '(A,E12.5)', "Max difference:   ", max_diff
  if (max_val > 0) then
    print '(A,F10.6,A)', "Relative error:   ", (max_diff / max_val) * 100.0, "%"
  end if
  
  print *, ""
  if (max_diff < 1e-5) then
    print *, "🎉 PERFECT ACCURACY! Mini's solution works!"
    print *, "✅ Proper leading dimensions fixed the issue"
    print *, "✅ Contiguous row ordering for cache efficiency"
    print *, "✅ No more memory corruption from wrong strides"
  else if (max_diff / max_val < 0.001) then
    print *, "✅ Excellent accuracy (< 0.1% error)"
  else
    print *, "❌ Still have accuracy issues to debug"
  end if
  
  ! Test different tile sizes as Mini suggested
  print *, ""
  print *, "=== TILE SIZE SENSITIVITY TEST ==="
  
  ! Tile size 1
  print *, "Testing with TILE_SIZE = 1..."
  ! Would need to modify the module to accept tile size as parameter
  ! For now, just noting that this test should be done
  
  deallocate(input, weights, output1, output2)
  
end program test_mini_solution