! Test Precision Differences in Mathematical Operations
! ===================================================
! This program demonstrates the importance of kind consistency

program test_precision_differences
  use kinds, only: dp, sp
  use kinds, real32
  implicit none
  
  real(dp) :: value_dp, value_sp_as_dp
  real(sp) :: value_sp
  real(dp) :: flops, time_seconds
  real(dp) :: gflops_correct, gflops_mixed1, gflops_mixed2
  real(dp) :: huge_flops, tiny_time
  real(dp) :: extreme_correct, extreme_wrong
  real(dp) :: total_ops, memory_bytes
  real(dp) :: intensity_correct, intensity_wrong
  
  ! Initialize values
  value_dp = 1234567890123.456789_dp
  value_sp = 1234567890123.456789_sp
  flops = 2.5d12
  time_seconds = 1.25_dp
  
  print *, "=== Precision and Kind Consistency Test ==="
  print *, ""
  
  ! Show precision differences
  print *, "=== Precision Loss Demonstration ==="
  print '(A,F20.6)', "Real64 value: ", value_dp
  print '(A,F20.6)', "Real32 value: ", value_sp  
  print '(A,F20.6)', "Difference:   ", value_dp - real(value_sp, dp)
  print *, ""
  
  ! Test mathematical operations
  print *, "=== Mathematical Operation Consistency ==="
  
  print '(A,F12.6)', "FLOPS: ", flops/1.0d12
  print '(A,F12.6)', "Time:  ", time_seconds
  print *, ""
  
  ! Different calculation methods
  
  ! CORRECT: All double precision
  gflops_correct = flops / (time_seconds * 1.0d9)
  
  ! WRONG: Mixed precision (what old code was doing)
  gflops_mixed1 = flops / (time_seconds * 1.0e9)    ! 1.0e9 is default real
  gflops_mixed2 = real(flops, real32) / (time_seconds * 1.0d9)  ! mixed types
  
  print *, "=== GFLOPS Calculation Results ==="
  print '(A,F15.9,A)', "✅ Correct (all dp):    ", gflops_correct, " GFLOPS"
  print '(A,F15.9,A)', "❌ Mixed literal:       ", gflops_mixed1, " GFLOPS"  
  print '(A,F15.9,A)', "❌ Mixed precision:     ", gflops_mixed2, " GFLOPS"
  print *, ""
  
  print *, "=== Differences from Correct Value ==="
  print '(A,E15.6)', "Mixed literal error:    ", abs(gflops_correct - gflops_mixed1)
  print '(A,E15.6)', "Mixed precision error:  ", abs(gflops_correct - gflops_mixed2)
  print *, ""
  
  ! Test with extreme values where differences become apparent
  print *, "=== Extreme Value Test ==="
  huge_flops = 1.0d18
  tiny_time = 1.0d-6
  extreme_correct = huge_flops / (tiny_time * 1.0d9)
  extreme_wrong = huge_flops / (tiny_time * 1.0e9)
  
  print '(A,E15.6)', "Huge FLOPS: ", huge_flops
  print '(A,E15.6)', "Tiny time:  ", tiny_time
  print '(A,E15.6)', "Correct:    ", extreme_correct
  print '(A,E15.6)', "Wrong:      ", extreme_wrong
  print '(A,E15.6)', "Error:      ", abs(extreme_correct - extreme_wrong)
  print *, ""
  
  ! Test arithmetic intensity calculations
  print *, "=== Arithmetic Intensity Test ==="
  total_ops = 1.0d9
  memory_bytes = 4.0d6
  
  intensity_correct = total_ops / memory_bytes
  intensity_wrong = real(total_ops, real32) / memory_bytes  ! Mixed precision
  
  print '(A,F12.6)', "Correct intensity: ", intensity_correct
  print '(A,F12.6)', "Wrong intensity:   ", intensity_wrong
  print '(A,E12.3)', "Relative error:    ", abs(intensity_correct - intensity_wrong) / intensity_correct
  print *, ""
  
  print *, "✅ Kind consistency prevents subtle mathematical errors!"
  print *, "✅ Our hardening ensures reliable performance calculations!"

end program test_precision_differences
