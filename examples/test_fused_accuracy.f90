program test_fused_accuracy
  use iso_fortran_env
  use cpu_conv2d_fused_simple
  use universal_memory_optimization, only: fused_conv2d_cpu
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output1(:), output2(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  integer :: i, num_diffs, first_diff_idx
  real(real32) :: time1, time2, max_diff, avg_diff, sum_diff
  
  print *, "=== DEBUGGING FUSED CONVOLUTION ACCURACY ==="
  print *, ""
  
  ! Test with progressively larger sizes
  call test_size(1, 1, 4, 4, 1)    ! Tiny
  call test_size(1, 2, 8, 8, 2)    ! Small 
  call test_size(1, 4, 16, 16, 4)  ! Medium
  call test_size(1, 8, 32, 32, 8)  ! Large
  
contains

  subroutine test_size(N_test, C_test, H_test, W_test, K_test)
    integer, intent(in) :: N_test, C_test, H_test, W_test, K_test
    
    N = N_test
    C = C_test
    H = H_test
    W = W_test
    K = K_test
    kernel_size = 3
    stride = 1
    pad = 1
    H_out = (H + 2*pad - kernel_size) / stride + 1
    W_out = (W + 2*pad - kernel_size) / stride + 1
    
    print '(A,I0,A,I0,A,I0,A,I0,A,I0)', &
      "Testing size: ", N, "x", C, "x", H, "x", W, " -> ", K, " filters"
    
    ! Allocate arrays
    if (allocated(input)) deallocate(input)
    if (allocated(weights)) deallocate(weights)
    if (allocated(output1)) deallocate(output1)
    if (allocated(output2)) deallocate(output2)
    
    allocate(input(N * C * H * W))
    allocate(weights(K * C * kernel_size * kernel_size))
    allocate(output1(N * K * H_out * W_out))
    allocate(output2(N * K * H_out * W_out))
    
    ! Initialize with deterministic pattern for debugging
    do i = 1, size(input)
      input(i) = sin(real(i, real32) * 0.1)
    end do
    
    do i = 1, size(weights)
      weights(i) = cos(real(i, real32) * 0.05)
    end do
    
    ! Run unfused
    output1 = 0.0
    time1 = fused_conv2d_cpu(input, weights, output1, &
                            N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    
    ! Run fused
    output2 = 0.0
    time2 = conv2d_fused_simple(input, weights, output2, &
                               N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    
    ! Analyze differences
    max_diff = 0.0
    sum_diff = 0.0
    num_diffs = 0
    first_diff_idx = 0
    
    do i = 1, size(output1)
      if (abs(output1(i) - output2(i)) > 1e-5) then
        num_diffs = num_diffs + 1
        if (first_diff_idx == 0) first_diff_idx = i
      end if
      max_diff = max(max_diff, abs(output1(i) - output2(i)))
      sum_diff = sum_diff + abs(output1(i) - output2(i))
    end do
    
    avg_diff = sum_diff / size(output1)
    
    print '(A,F8.3,A,F8.3,A,F6.2,A)', &
      "  Times: unfused=", time1, "ms, fused=", time2, "ms (", time1/time2, "x speedup)"
    print '(A,E12.5,A,E12.5)', &
      "  Max diff: ", max_diff, ", Avg diff: ", avg_diff
    print '(A,I0,A,I0,A,F6.2,A)', &
      "  Differences: ", num_diffs, " out of ", size(output1), " (", &
      real(num_diffs)/real(size(output1))*100.0, "%)"
    
    if (num_diffs > 0 .and. size(output1) <= 100) then
      print *, "  First difference at index ", first_diff_idx
      ! Convert to output coordinates
      i = first_diff_idx
      print '(A,F10.5,A,F10.5)', &
        "    Unfused: ", output1(i), ", Fused: ", output2(i)
    end if
    
    if (max_diff < 1e-4) then
      print *, "  ✅ PASS"
    else
      print *, "  ❌ FAIL"
    end if
    print *, ""
    
  end subroutine test_size

end program test_fused_accuracy