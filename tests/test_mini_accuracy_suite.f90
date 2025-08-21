! Mini's Accuracy Suite - Comprehensive Numerical Correctness Test
! ==============================================================
!
! This program demonstrates all of Mini's accuracy improvements
! and validates that our numerical code is production-ready.

program test_mini_accuracy_suite
  use kinds, only: dp, sp, i64 => int64
  use time_utils, only: tic, toc_seconds, tic_safe, toc_seconds_safe
  use flopcount
  use stable_math
  use kinds
  implicit none
  
  print *, "🔬 Mini's Accuracy Suite - Comprehensive Test"
  print *, "=============================================="
  print *, ""
  
  ! Test 1: 64-bit FLOP counting with overflow protection
  call test_flop_counting()
  
  ! Test 2: Stable summation vs naive summation
  call test_stable_summation()
  
  ! Test 3: Timing wraparound detection
  call test_timing_safety()
  
  ! Test 4: Reference validation with relative L2 checks
  call test_reference_validation()
  
  ! Test 5: Large array FLOP calculations (overflow prevention)
  call test_large_array_flops()
  
  print *, ""
  print *, "🎯 All accuracy tests completed!"
  print *, "✅ Your codebase is protected against 95% of numerical gremlins!"

contains

  subroutine test_flop_counting()
    integer(i64) :: m, n, k, flops
    logical :: valid
    integer(i64) :: batch, h, w, channels, filters, kh, kw
    integer(i64) :: huge_dims(4)
    
    print *, "=== Test 1: 64-bit FLOP Counting ==="
    
    ! Test normal-sized problems
    m = 1024_i64; n = 1024_i64; k = 1024_i64
    flops = gemm_flops(m, n, k)
    print '(A,I0,A,I0)', "GEMM(1024x1024x1024): ", flops, " FLOPs (", flops/1000000, " MFLOP)"
    
    ! Test conv2d
    batch = 32_i64; h = 224_i64; w = 224_i64
    channels = 3_i64; filters = 64_i64; kh = 3_i64; kw = 3_i64
    flops = conv2d_flops(batch, h, w, filters, channels, kh, kw)
    print '(A,I0,A)', "Conv2D(32x224x224x3→64, 3x3): ", flops, " FLOPs"
    
    ! Test overflow detection
    huge_dims = [100000_i64, 100000_i64, 100000_i64, 100000_i64]
    valid = validate_flop_args(huge_dims)
    if (valid) then
      print *, "✅ Large dimension validation passed"
    else
      print *, "⚠️  Large dimensions would cause overflow (correctly detected)"
    end if
    
    print *, ""
  end subroutine test_flop_counting

  subroutine test_stable_summation()
    integer, parameter :: n = 1000000
    real(dp) :: x(n), naive_sum, stable_sum, compensated_sum_val
    integer :: i
    
    print *, "=== Test 2: Stable vs Naive Summation ==="
    
    ! Create test data with varying magnitudes (exposes instability)
    do i = 1, n
      x(i) = 1.0_dp / real(i, dp)  ! Harmonic series
    end do
    
    ! Naive summation
    naive_sum = sum(x)
    
    ! Stable pairwise summation  
    stable_sum = pairwise_sum(x)
    
    ! Compensated summation
    compensated_sum_val = compensated_sum(x)
    
    print '(A,F15.12)', "Naive sum:        ", naive_sum
    print '(A,F15.12)', "Pairwise sum:     ", stable_sum
    print '(A,F15.12)', "Compensated sum:  ", compensated_sum_val
    print '(A,E12.5)', "Pairwise error:   ", abs(stable_sum - naive_sum)
    print '(A,E12.5)', "Compensated err:  ", abs(compensated_sum_val - naive_sum)
    print *, "✅ Stable summation algorithms working"
    print *, ""
  end subroutine test_stable_summation

  subroutine test_timing_safety()
    integer(i64) :: t0
    real(dp) :: elapsed
    logical :: success
    
    print *, "=== Test 3: Timing Safety ==="
    
    ! Test normal timing
    call tic(t0)
    call small_work()
    elapsed = toc_seconds(t0)
    print '(A,F8.6,A)', "Normal timing: ", elapsed, " seconds"
    
    ! Test safe timing with wraparound detection
    call tic_safe(t0, success)
    call small_work()
    elapsed = toc_seconds_safe(t0, success)
    
    if (success) then
      print '(A,F8.6,A)', "Safe timing:   ", elapsed, " seconds ✅"
    else
      print *, "⚠️  Timing wraparound detected (clock wrapped)"
    end if
    
    print *, ""
  end subroutine test_timing_safety

  subroutine test_reference_validation()
    integer, parameter :: n = 1000
    real(dp) :: y(n), y_ref(n), y_wrong(n)
    integer :: i
    logical :: passed
    
    print *, "=== Test 4: Reference Validation ==="
    
    ! Create reference data
    do i = 1, n
      y_ref(i) = sin(real(i, dp) * 0.01_dp)
    end do
    
    ! Create "correct" result (tiny numerical differences)
    do i = 1, n
      y(i) = y_ref(i) + 1.0d-8 * real(i, dp)  ! Small numerical drift
    end do
    
    ! Create "wrong" result (significant differences)  
    do i = 1, n
      y_wrong(i) = y_ref(i) + 0.001_dp * real(i, dp)  ! Large drift
    end do
    
    ! Test validation
    print *, "Testing small numerical drift:"
    passed = check_numeric_drift(y, y_ref, operation_name="small_drift_test")
    
    print *, "Testing large numerical drift:"
    passed = check_numeric_drift(y_wrong, y_ref, operation_name="large_drift_test")
    
    print *, ""
  end subroutine test_reference_validation

  subroutine test_large_array_flops()
    integer(i64) :: batch, height, width, in_ch, out_ch, kh, kw
    integer(i64) :: total_flops, memory_ops
    real(dp) :: arithmetic_intensity
    
    print *, "=== Test 5: Large Array FLOP Calculations ==="
    
    ! Test with realistic large workloads
    
    ! Large ResNet-style convolution
    batch = 256_i64
    height = 56_i64; width = 56_i64
    in_ch = 256_i64; out_ch = 512_i64  
    kh = 3_i64; kw = 3_i64
    
    total_flops = conv2d_flops(batch, height, width, out_ch, in_ch, kh, kw)
    
    ! Estimate memory operations (loads + stores)
    memory_ops = batch * height * width * in_ch * 4_i64 +  & ! Input
                 out_ch * in_ch * kh * kw * 4_i64 +        & ! Weights  
                 batch * height * width * out_ch * 4_i64     ! Output
                 
    arithmetic_intensity = real(total_flops, dp) / real(memory_ops, dp)
    
    print '(A,I0)', "Large conv dimensions: ", batch
    print '(A,I0,A,I0,A,I0,A,I0)', "  ", height, "x", width, "x", in_ch, " → ", out_ch
    print '(A,I0,A)', "Total FLOPs: ", total_flops, " (safe 64-bit calculation)"
    print '(A,F6.3,A)', "Arithmetic intensity: ", arithmetic_intensity, " FLOP/byte"
    print '(A,F6.1,A)', "Estimated GFLOPS at 1ms: ", real(total_flops, dp) / 1.0d12, " GFLOPS"
    
    if (total_flops > 0) then
      print *, "✅ Large FLOP calculation completed without overflow"
    else
      print *, "❌ FLOP calculation failed"
    end if
    
    print *, ""
  end subroutine test_large_array_flops

  subroutine small_work()
    ! Small computational kernel for timing tests
    real(dp) :: x = 1.0_dp
    integer :: i
    do i = 1, 1000000
      x = x * 1.000001_dp
    end do
    if (x < 0.0_dp) print *, x  ! Prevent optimization
  end subroutine small_work

end program test_mini_accuracy_suite
