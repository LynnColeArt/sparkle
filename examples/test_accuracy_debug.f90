program test_accuracy_debug
  use iso_fortran_env
  use cpu_conv2d_fused_correct
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  real(real32) :: time1, time2
  integer :: i, j, num_diffs, filter_idx, spatial_idx
  real(real32) :: diff
  
  print *, "=== DEBUGGING ACCURACY ISSUES ==="
  print *, ""
  
  ! Start with small size to debug
  N = 1
  C = 3
  H = 8  
  W = 8
  K = 2
  kernel_size = 3
  stride = 1
  pad = 1
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Test size: ", N, "x", C, "x", H, "x", W
  print '(A,I0)', "Filters: ", K
  print '(A,I0,A,I0)', "Output: ", H_out, "x", W_out
  print '(A,I0)', "Total output elements: ", N * K * H_out * W_out
  
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output1(N * K * H_out * W_out))
  allocate(output2(N * K * H_out * W_out))
  
  ! Use simple pattern for debugging
  do i = 1, size(input)
    input(i) = real(i, real32) / 100.0
  end do
  
  ! Simple weights - different for each filter
  do i = 1, K
    weights((i-1)*C*kernel_size*kernel_size + 1 : i*C*kernel_size*kernel_size) = real(i, real32) * 0.1
  end do
  
  ! Run unfused
  output1 = 0.0
  time1 = fused_conv2d_cpu(input, weights, output1, &
                          N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Run fused
  output2 = 0.0
  time2 = conv2d_fused_correct(input, weights, output2, &
                              N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  print *, ""
  print *, "=== OUTPUT COMPARISON ==="
  
  ! Print first few values from each filter
  do filter_idx = 1, K
    print *, ""
    print '(A,I0,A)', "Filter ", filter_idx, " output (first 10 values):"
    print *, "Unfused:"
    print '(10F8.3)', output1((filter_idx-1)*H_out*W_out + 1 : &
                              min((filter_idx-1)*H_out*W_out + 10, filter_idx*H_out*W_out))
    print *, "Fused:"
    print '(10F8.3)', output2((filter_idx-1)*H_out*W_out + 1 : &
                              min((filter_idx-1)*H_out*W_out + 10, filter_idx*H_out*W_out))
  end do
  
  ! Find where they differ
  print *, ""
  print *, "=== DIFFERENCES ==="
  num_diffs = 0
  do i = 1, size(output1)
    diff = abs(output1(i) - output2(i))
    if (diff > 1e-5) then
      num_diffs = num_diffs + 1
      if (num_diffs <= 10) then
        filter_idx = (i-1) / (H_out * W_out) + 1
        spatial_idx = mod(i-1, H_out * W_out) + 1
        print '(A,I4,A,I1,A,I3,A,F8.3,A,F8.3,A,F8.3)', &
          "Index ", i, " (filter=", filter_idx, ", spatial=", spatial_idx, &
          "): unfused=", output1(i), ", fused=", output2(i), ", diff=", diff
      end if
    end if
  end do
  
  print *, ""
  print '(A,I0,A,I0)', "Total differences: ", num_diffs, " out of ", size(output1)
  
  deallocate(input, weights, output1, output2)
  
end program test_accuracy_debug