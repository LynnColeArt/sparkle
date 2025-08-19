program performance_regression_test
  ! Performance Regression Test Suite
  ! =================================
  ! 
  ! Ensures our hard-won optimizations don't regress
  ! Fails loudly if performance drops below thresholds
  
  use kinds
  use time_utils, only: tic, toc_seconds
  use flopcount, only: conv2d_flops
  implicit none
  
  ! Performance thresholds (80% of our achieved performance)
  real(dp), parameter :: CPU_SIMD_THRESHOLD = 157.0_dp      ! 80% of 196.7 GFLOPS
  real(dp), parameter :: GPU_SINGLE_THRESHOLD = 356.0_dp    ! 80% of 445.7 GFLOPS  
  real(dp), parameter :: GPU_ASYNC_THRESHOLD = 2900.0_dp    ! 80% of 3630 GFLOPS
  real(dp), parameter :: ASYNC_SPEEDUP_THRESHOLD = 5.0_dp   ! Minimum 5x speedup
  
  ! Test parameters
  integer, parameter :: WARMUP_RUNS = 5
  integer, parameter :: TEST_RUNS = 10
  
  logical :: all_tests_passed = .true.
  integer :: failed_tests = 0
  
  print *, "🔍 PERFORMANCE REGRESSION TEST SUITE"
  print *, "==================================="
  print *, ""
  print *, "Checking that our optimizations haven't regressed..."
  print *, ""
  
  ! Run all tests
  call test_cpu_simd_performance()
  call test_gpu_single_performance()
  call test_gpu_async_performance()
  call test_timing_accuracy()
  call test_flop_counting_safety()
  
  ! Summary
  print *, ""
  print *, "==================================="
  if (all_tests_passed) then
    print *, "✅ ALL PERFORMANCE TESTS PASSED!"
    print *, "Our optimizations are intact! 🎉"
    stop 0
  else
    print '(A,I0,A)', "❌ FAILED ", failed_tests, " TESTS!"
    print *, "Performance has regressed - investigate immediately!"
    stop 1
  end if
  
contains

  subroutine test_cpu_simd_performance()
    real(dp) :: measured_gflops
    logical :: passed
    
    print *, "1. Testing CPU SIMD Performance..."
    
    ! Run the CPU SIMD benchmark
    measured_gflops = run_cpu_simd_benchmark()
    
    passed = measured_gflops >= CPU_SIMD_THRESHOLD
    
    if (passed) then
      print '(A,F6.1,A,F6.1,A)', "   ✅ CPU SIMD: ", measured_gflops, &
            " GFLOPS (threshold: ", CPU_SIMD_THRESHOLD, ")"
    else
      print '(A,F6.1,A,F6.1,A)', "   ❌ CPU SIMD: ", measured_gflops, &
            " GFLOPS (BELOW threshold: ", CPU_SIMD_THRESHOLD, ")"
      all_tests_passed = .false.
      failed_tests = failed_tests + 1
    end if
    
  end subroutine test_cpu_simd_performance
  
  subroutine test_gpu_single_performance()
    real(dp) :: measured_gflops
    logical :: passed
    
    print *, "2. Testing GPU Single Kernel Performance..."
    
    ! Run the GPU single kernel benchmark
    measured_gflops = run_gpu_single_benchmark()
    
    passed = measured_gflops >= GPU_SINGLE_THRESHOLD
    
    if (passed) then
      print '(A,F6.1,A,F6.1,A)', "   ✅ GPU Single: ", measured_gflops, &
            " GFLOPS (threshold: ", GPU_SINGLE_THRESHOLD, ")"
    else
      print '(A,F6.1,A,F6.1,A)', "   ❌ GPU Single: ", measured_gflops, &
            " GFLOPS (BELOW threshold: ", GPU_SINGLE_THRESHOLD, ")"
      all_tests_passed = .false.
      failed_tests = failed_tests + 1
    end if
    
  end subroutine test_gpu_single_performance
  
  subroutine test_gpu_async_performance()
    real(dp) :: measured_gflops, speedup
    logical :: passed
    
    print *, "3. Testing GPU Async Pipeline Performance..."
    
    ! Run the GPU async benchmark
    call run_gpu_async_benchmark(measured_gflops, speedup)
    
    passed = (measured_gflops >= GPU_ASYNC_THRESHOLD) .and. &
             (speedup >= ASYNC_SPEEDUP_THRESHOLD)
    
    if (passed) then
      print '(A,F7.1,A,F7.1,A)', "   ✅ GPU Async: ", measured_gflops, &
            " GFLOPS (threshold: ", GPU_ASYNC_THRESHOLD, ")"
      print '(A,F5.2,A,F5.2,A)', "   ✅ Speedup: ", speedup, &
            "x (threshold: ", ASYNC_SPEEDUP_THRESHOLD, "x)"
    else
      if (measured_gflops < GPU_ASYNC_THRESHOLD) then
        print '(A,F7.1,A,F7.1,A)', "   ❌ GPU Async: ", measured_gflops, &
              " GFLOPS (BELOW threshold: ", GPU_ASYNC_THRESHOLD, ")"
      end if
      if (speedup < ASYNC_SPEEDUP_THRESHOLD) then
        print '(A,F5.2,A,F5.2,A)', "   ❌ Speedup: ", speedup, &
              "x (BELOW threshold: ", ASYNC_SPEEDUP_THRESHOLD, "x)"
      end if
      all_tests_passed = .false.
      failed_tests = failed_tests + 1
    end if
    
  end subroutine test_gpu_async_performance
  
  subroutine test_timing_accuracy()
    integer(i64) :: t0, t1
    real(dp) :: elapsed
    logical :: passed
    
    print *, "4. Testing Timing Accuracy (Mini's hardening)..."
    
    ! Test that timing is working correctly
    call tic(t0)
    call sleep_ms(100)  ! Sleep for 100ms
    elapsed = toc_seconds(t0) * 1000.0_dp
    
    ! Should be between 95ms and 105ms
    passed = (elapsed > 95.0_dp) .and. (elapsed < 105.0_dp)
    
    if (passed) then
      print '(A,F6.1,A)', "   ✅ Timing accuracy: ", elapsed, " ms for 100ms sleep"
    else
      print '(A,F6.1,A)', "   ❌ Timing accuracy FAILED: ", elapsed, " ms for 100ms sleep"
      all_tests_passed = .false.
      failed_tests = failed_tests + 1
    end if
    
  end subroutine test_timing_accuracy
  
  subroutine test_flop_counting_safety()
    integer(i64) :: flops, expected
    logical :: passed
    
    print *, "5. Testing FLOP Counting Safety (Mini's hardening)..."
    
    ! Test large convolution that would overflow int32
    flops = conv2d_flops(256_i64, 224_i64, 224_i64, &
                        512_i64, 256_i64, 3_i64, 3_i64)
    
    expected = 2_i64 * 256_i64 * 224_i64 * 224_i64 * &
               512_i64 * 256_i64 * 3_i64 * 3_i64
    
    passed = (flops == expected) .and. (flops > 0)
    
    if (passed) then
      print '(A,I0,A)', "   ✅ 64-bit FLOP counting: ", flops/1000000000_i64, " billion FLOPs"
    else
      print '(A,I0)', "   ❌ FLOP counting FAILED! Got: ", flops
      print '(A,I0)', "      Expected: ", expected
      all_tests_passed = .false.
      failed_tests = failed_tests + 1
    end if
    
  end subroutine test_flop_counting_safety
  
  ! Wrappers that call the real implementations
  function run_cpu_simd_benchmark() result(gflops)
    use performance_regression_impl, only: benchmark_cpu_simd
    real(dp) :: gflops
    gflops = benchmark_cpu_simd()
  end function run_cpu_simd_benchmark
  
  function run_gpu_single_benchmark() result(gflops)
    use performance_regression_impl, only: benchmark_gpu_single
    real(dp) :: gflops
    gflops = benchmark_gpu_single()
  end function run_gpu_single_benchmark
  
  subroutine run_gpu_async_benchmark(gflops, speedup)
    use performance_regression_impl, only: benchmark_gpu_async
    real(dp), intent(out) :: gflops, speedup
    call benchmark_gpu_async(gflops, speedup)
  end subroutine run_gpu_async_benchmark
  
  subroutine sleep_ms(milliseconds)
    integer, intent(in) :: milliseconds
    ! Simple sleep implementation
    call execute_command_line("sleep " // trim(adjustl(str(milliseconds/1000.0))))
  end subroutine sleep_ms
  
  function str(x) result(s)
    real, intent(in) :: x
    character(len=20) :: s
    write(s, '(F10.3)') x
  end function str
  
end program performance_regression_test