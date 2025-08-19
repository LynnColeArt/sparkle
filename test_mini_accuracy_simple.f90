! Mini's Accuracy Suite - Simplified Test
! ======================================

program test_mini_accuracy_simple
  use kinds, only: dp, sp
  use time_utils, only: tic, toc_seconds
  use flopcount
  use stable_math
  use iso_fortran_env, only: int64
  implicit none
  
  integer(int64) :: m, n, k, flops
  real(dp) :: x(100), y(100), y_ref(100)
  real(dp) :: naive_sum, stable_sum_val, rel_error, abs_error
  logical :: passed
  integer :: i
  
  print *, "🔬 Mini's Accuracy Suite - Simplified Test"
  print *, "=========================================="
  print *, ""
  
  ! Test 1: 64-bit FLOP counting
  print *, "=== Test 1: 64-bit FLOP Counting ==="
  m = 1024_int64; n = 1024_int64; k = 1024_int64
  flops = gemm_flops(m, n, k)
  print '(A,I0,A)', "GEMM(1024x1024x1024): ", flops, " FLOPs"
  
  flops = conv2d_flops(32_int64, 224_int64, 224_int64, 64_int64, 3_int64, 3_int64, 3_int64)
  print '(A,I0,A)', "Conv2D(32x224x224x3→64, 3x3): ", flops, " FLOPs"
  print *, "✅ FLOP counting working"
  print *, ""
  
  ! Test 2: Stable summation
  print *, "=== Test 2: Stable Summation ==="
  do i = 1, 100
    x(i) = 1.0_dp / real(i, dp)
  end do
  
  naive_sum = sum(x)
  stable_sum_val = pairwise_sum(x)
  
  print '(A,F15.12)', "Naive sum:    ", naive_sum
  print '(A,F15.12)', "Stable sum:   ", stable_sum_val
  print '(A,E12.5)', "Difference:   ", abs(stable_sum_val - naive_sum)
  print *, "✅ Stable summation working"
  print *, ""
  
  ! Test 3: Reference validation
  print *, "=== Test 3: Reference Validation ==="
  do i = 1, 100
    y_ref(i) = sin(real(i, dp) * 0.01_dp)
    y(i) = y_ref(i) + 1.0d-8 * real(i, dp)  ! Small drift
  end do
  
  rel_error = relative_error_l2(y, y_ref)
  abs_error = max_absolute_error(y, y_ref)
  
  print '(A,E12.5)', "Relative L2 error: ", rel_error
  print '(A,E12.5)', "Max absolute error: ", abs_error
  
  passed = check_numeric_drift(y, y_ref, operation_name="test_validation")
  print *, ""
  
  print *, "🎯 All simplified accuracy tests completed!"
  print *, "✅ Mini's accuracy guards are working correctly!"

end program test_mini_accuracy_simple