program test_fused_debug
  use iso_fortran_env
  use cpu_conv2d_fused_correct
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time1, time2, max_diff
  integer :: i
  
  print *, "=== DEBUGGING FUSED CONVOLUTION ==="
  print *, ""
  
  ! Test with N=1, small C to debug
  N = 1
  C = 3
  H = 8
  W = 8
  K = 4
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
  
  ! Initialize with specific pattern
  do i = 1, size(input)
    input(i) = real(i, real32) / 100.0
  end do
  
  ! Simple weights
  weights = 0.1
  
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  output2 = 0.0
  time2 = conv2d_fused_correct(input, weights, output2, &
                              N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, "First few output values:"
  print *, "Unfused:", output1(1:min(10, size(output1)))
  print *, "Fused:  ", output2(1:min(10, size(output2)))
  
  max_diff = maxval(abs(output1 - output2))
  print *, ""
  print '(A,E12.5)', "Max difference: ", max_diff
  
  if (max_diff < 1e-5) then
    print *, "✅ Test PASSED!"
  else
    print *, "❌ Test FAILED!"
    
    ! Find where they differ
    do i = 1, size(output1)
      if (abs(output1(i) - output2(i)) > 1e-5) then
        print '(A,I0,A,F10.6,A,F10.6,A,F10.6)', &
          "Differ at index ", i, ": unfused=", output1(i), ", fused=", output2(i), &
          ", diff=", abs(output1(i) - output2(i))
        if (i > 10) exit
      end if
    end do
  end if
  
  deallocate(input, weights, output1, output2)
  
end program test_fused_debug