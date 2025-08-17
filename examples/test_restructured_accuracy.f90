program test_restructured_accuracy
  use iso_fortran_env
  use cpu_conv2d_fused_restructured
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time1, time2, max_diff, max_val
  integer :: i
  
  print *, "=== RESTRUCTURED FUSED ACCURACY TEST ==="
  print *, ""
  
  ! Test with the problematic size
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
  print '(A,I0)', "K (filters): ", K
  print '(A,I0)', "Input rows (C*k*k*N): ", C * kernel_size * kernel_size * N
  print *, ""
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(weights)
  
  ! Scale to reasonable range
  input = (input - 0.5) * 2.0
  weights = (weights - 0.5) * 0.1
  
  print *, "Running reference implementation..."
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, "Running restructured fused implementation..."
  output2 = 0.0
  time2 = conv2d_fused_restructured(input, weights, output2, &
                                   N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Compute accuracy metrics
  max_diff = maxval(abs(output1 - output2))
  max_val = max(maxval(abs(output1)), maxval(abs(output2)))
  
  print *, ""
  print *, "=== RESULTS ==="
  print '(A,F10.2,A)', "Reference time:    ", time1, " ms"
  print '(A,F10.2,A)', "Restructured time: ", time2, " ms"
  print '(A,F10.2,A)', "Speedup:           ", time1 / time2, "x"
  print *, ""
  print '(A,E12.5)', "Max output value:  ", max_val
  print '(A,E12.5)', "Max difference:    ", max_diff
  if (max_val > 0) then
    print '(A,F10.6,A)', "Relative error:    ", (max_diff / max_val) * 100.0, "%"
  end if
  
  print *, ""
  if (max_diff < 1e-5) then
    print *, "✅ PERFECT ACCURACY! Restructuring solved the problem!"
  else if (max_diff < 1e-3) then
    print *, "✅ Excellent accuracy (< 0.001 absolute error)"
  else if (max_diff / max_val < 0.01) then
    print *, "⚠️  Good accuracy (< 1% relative error)"
  else
    print *, "❌ Poor accuracy - still debugging needed"
    
    ! Show some specific differences
    print *, ""
    print *, "Sample differences (first 10 that exceed 1e-5):"
    do i = 1, min(10, size(output1))
      if (abs(output1(i) - output2(i)) > 1e-5) then
        print '(A,I6,A,F12.6,A,F12.6,A,F12.6)', &
          "  [", i, "] ref=", output1(i), ", restructured=", output2(i), &
          ", diff=", abs(output1(i) - output2(i))
      end if
    end do
  end if
  
  deallocate(input, weights, output1, output2)
  
end program test_restructured_accuracy