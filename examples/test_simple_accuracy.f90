program test_simple_accuracy
  use iso_fortran_env
  use cpu_conv2d_fused_correct
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time1, time2, max_diff, max_val
  integer :: i
  
  print *, "=== SIMPLE ACCURACY TEST ==="
  print *, ""
  
  ! Very simple test case
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
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  ! Simple test pattern
  input = [1.0, 2.0, 3.0, &
           4.0, 5.0, 6.0, &
           7.0, 8.0, 9.0]
  
  weights = 1.0 / 9.0  ! Average filter
  
  print *, "Input (3x3):"
  print '(3F8.3)', input(1:3)
  print '(3F8.3)', input(4:6)
  print '(3F8.3)', input(7:9)
  
  print *, ""
  print *, "Weights (all", weights(1), "):"
  
  ! Run unfused
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Run fused
  output2 = 0.0
  time2 = conv2d_fused_correct(input, weights, output2, &
                              N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  print *, "Unfused output (3x3):"
  print '(3F8.3)', output1(1:3)
  print '(3F8.3)', output1(4:6)
  print '(3F8.3)', output1(7:9)
  
  print *, ""
  print *, "Fused output (3x3):"
  print '(3F8.3)', output2(1:3)
  print '(3F8.3)', output2(4:6)
  print '(3F8.3)', output2(7:9)
  
  max_diff = maxval(abs(output1 - output2))
  print *, ""
  print '(A,E12.5)', "Max difference: ", max_diff
  
  if (max_diff < 1e-6) then
    print *, "✅ PERFECT MATCH!"
  else
    print *, "❌ Results differ"
    
    ! Show element-wise differences
    print *, ""
    print *, "Differences:"
    do i = 1, size(output1)
      if (abs(output1(i) - output2(i)) > 1e-6) then
        print '(A,I2,A,F8.3,A,F8.3,A,F8.3)', &
          "  [", i, "] unfused=", output1(i), ", fused=", output2(i), &
          ", diff=", abs(output1(i) - output2(i))
      end if
    end do
  end if
  
  ! Now test with random data but controlled range
  print *, ""
  print *, "=== RANDOM DATA TEST (controlled range) ==="
  
  call random_number(input)
  call random_number(weights)
  
  ! Keep values small to avoid overflow
  input = input * 2.0 - 1.0      ! Range [-1, 1]
  weights = weights * 0.2 - 0.1  ! Range [-0.1, 0.1]
  
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  output2 = 0.0
  time2 = conv2d_fused_correct(input, weights, output2, &
                              N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  max_diff = maxval(abs(output1 - output2))
  max_val = max(maxval(abs(output1)), maxval(abs(output2)))
  
  print '(A,E12.5)', "Max output value: ", max_val
  print '(A,E12.5)', "Max difference: ", max_diff
  print '(A,F8.5,A)', "Relative error: ", (max_diff / max_val) * 100.0, "%"
  
  deallocate(input, weights, output1, output2)
  
end program test_simple_accuracy