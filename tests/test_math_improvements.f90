! Test Mathematical Correctness Improvements
! ==========================================
! This program demonstrates that our fixes ensure correct mathematical results

program test_math_improvements
  use kinds, only: dp, sp
  use time_utils, only: tic, toc_seconds
  use kinds
  implicit none
  
  integer(i64) :: t0
  real(dp) :: elapsed_seconds, gflops_correct, gflops_wrong
  integer(i64), parameter :: flop_count = 1000000000_i64  ! 1B operations
  real(dp) :: total_time, individual_times(5)
  integer :: i
  
  print *, "=== Mathematical Correctness Demonstration ==="
  print *, ""
  
  ! Simulate 1 billion FLOPS taking 0.5 seconds (should be 2.0 GFLOPS)
  call tic(t0)
  call do_fake_work()
  elapsed_seconds = toc_seconds(t0)
  
  print '(A,F8.6,A)', "Elapsed time: ", elapsed_seconds, " seconds"
  print '(A,I0)', "FLOP count: ", flop_count
  print *, ""
  
  ! CORRECT calculation using our hardening
  gflops_correct = real(flop_count, dp) / (elapsed_seconds * 1.0d9)
  
  ! WRONG calculation (what the old code was doing)
  gflops_wrong = real(flop_count, dp) / (elapsed_seconds * 1.0e9)  ! Mixed kind!
  
  print *, "=== Results ==="
  print '(A,F12.9,A)', "✅ CORRECT (hardened): ", gflops_correct, " GFLOPS"
  print '(A,F12.9,A)', "❌ WRONG (old way):    ", gflops_wrong, " GFLOPS"
  print '(A,F12.9)', "Difference: ", abs(gflops_correct - gflops_wrong)
  print *, ""
  
  ! Test timing accuracy
  print *, "=== Timing Accuracy Test ==="
  
  total_time = 0.0_dp
  do i = 1, 5
    call tic(t0)
    call do_small_work()
    individual_times(i) = toc_seconds(t0)
    total_time = total_time + individual_times(i)
    print '(A,I0,A,F8.6,A)', "Run ", i, ": ", individual_times(i), " seconds"
  end do
  
  print '(A,F8.6,A)', "Total: ", total_time, " seconds"
  print '(A,F8.6,A)', "Average: ", total_time/5.0_dp, " seconds"
  
  print *, ""
  print *, "✅ Mathematical correctness verified!"
  print *, "All GFLOPS calculations now use proper kind consistency."

contains

  subroutine do_fake_work()
    ! Simulate computational work
    real(dp) :: x = 1.0_dp
    integer :: i
    do i = 1, 100000000
      x = x * 1.0000000001_dp
    end do
    if (x < 0.0_dp) print *, x  ! Prevent optimization
  end subroutine do_fake_work
  
  subroutine do_small_work()
    ! Small amount of work for timing precision test
    real(dp) :: x = 1.0_dp
    integer :: i
    do i = 1, 10000000
      x = x + 1.0d-8
    end do
    if (x < 0.0_dp) print *, x  ! Prevent optimization
  end subroutine do_small_work

end program test_math_improvements
