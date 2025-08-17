program test_fused_simple
  use iso_fortran_env
  use cpu_conv2d_fused_simple
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time_unfused, time_fused, max_diff
  integer :: i
  
  print *, "=== TESTING SIMPLIFIED FUSED CONVOLUTION ==="
  print *, ""
  
  ! Test parameters
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
  print '(A,I0,A,I0,A,I0,A,I0)', "Output size: ", N, "x", K, "x", H_out, "x", W_out
  print *, ""
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  ! Initialize with test data
  call random_number(input)
  call random_number(weights)
  
  ! Test 1: Original unfused approach
  print *, "Test 1: UNFUSED approach"
  output1 = 0.0
  time_unfused = fused_conv2d_cpu(input, weights, output1, &
                                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  
  ! Test 2: Simplified fused approach
  print *, "Test 2: SIMPLIFIED FUSED approach"
  output2 = 0.0
  time_fused = conv2d_fused_simple(input, weights, output2, &
                                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Verify results match
  max_diff = 0.0
  do i = 1, size(output1)
    max_diff = max(max_diff, abs(output1(i) - output2(i)))
  end do
  
  print *, ""
  print *, "=== RESULTS ==="
  print '(A,F10.2,A)', "Unfused time: ", time_unfused, " ms"
  print '(A,F10.2,A)', "Fused time: ", time_fused, " ms"
  print '(A,F10.2,A)', "Speedup: ", time_unfused / time_fused, "x"
  print '(A,E12.5)', "Max difference: ", max_diff
  
  if (max_diff < 1e-4) then
    print *, "✅ Results match!"
  else
    print *, "❌ Results differ!"
    ! Show first few differences
    print *, "First 10 values:"
    do i = 1, min(10, size(output1))
      if (abs(output1(i) - output2(i)) > 1e-5) then
        print '(A,I5,A,F10.5,A,F10.5,A,F10.5)', &
          "  [", i, "] unfused=", output1(i), " fused=", output2(i), " diff=", abs(output1(i) - output2(i))
      end if
    end do
  end if
  
  deallocate(input, weights, output1, output2)
  
end program test_fused_simple