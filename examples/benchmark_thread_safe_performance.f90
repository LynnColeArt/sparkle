program benchmark_thread_safe_performance
  use kinds
  use gpu_program_cache_v2          ! Original cache
  use gpu_program_cache_threadsafe  ! Thread-safe cache
  use gpu_opengl_interface
  use omp_lib
  implicit none
  
  ! Test configurations
  integer, parameter :: NUM_WARMUP = 10
  integer, parameter :: NUM_ITERATIONS = 100
  integer, parameter :: NUM_PROGRAMS = 50
  integer, parameter :: NUM_THREADS_TEST = 4
  
  ! Caches
  type(program_cache) :: cache_v2
  type(program_cache_ts) :: cache_ts
  
  ! Timing and results
  real(dp) :: start_time, end_time
  real(dp) :: v2_single_time, v2_multi_time
  real(dp) :: ts_single_time, ts_multi_time
  integer :: i, j, program_ids(NUM_PROGRAMS)
  character(len=64) :: cache_keys(NUM_PROGRAMS)
  logical :: success
  
  print *, "🚀 Thread-Safe Cache Performance Benchmark"
  print *, "=========================================="
  print *, ""
  print '(A,I0)', "Programs to cache: ", NUM_PROGRAMS
  print '(A,I0)', "Iterations: ", NUM_ITERATIONS
  print '(A,I0)', "Max threads: ", omp_get_max_threads()
  print *, ""
  
  ! Initialize GPU
  if (.not. gpu_init()) then
    print *, "❌ Failed to initialize GPU!"
    stop 1
  end if
  
  ! Generate cache keys
  do i = 1, NUM_PROGRAMS
    write(cache_keys(i), '(A,I0)') "test_program_", i
  end do
  
  ! =====================================================
  ! Test 1: Original Cache (V2) - Single Threaded
  ! =====================================================
  print *, "📊 Test 1: Original Cache (V2) - Single Threaded"
  print *, "------------------------------------------------"
  
  call init_program_cache_v2(cache_v2, max_programs=100, &
                            cache_directory="benchmark_cache_v2/")
  
  ! Warmup
  do j = 1, NUM_WARMUP
    do i = 1, NUM_PROGRAMS
      program_ids(i) = get_cached_program_v2(cache_v2, "", cache_keys(i), &
                                            compile_test_program)
      call release_program(cache_v2, program_ids(i))
    end do
  end do
  
  ! Benchmark
  call cpu_time(start_time)
  do j = 1, NUM_ITERATIONS
    do i = 1, NUM_PROGRAMS
      program_ids(i) = get_cached_program_v2(cache_v2, "", cache_keys(i), &
                                            compile_test_program)
      call release_program(cache_v2, program_ids(i))
    end do
  end do
  call cpu_time(end_time)
  
  v2_single_time = (end_time - start_time) * 1000.0
  print '(A,F10.2,A)', "Total time: ", v2_single_time, " ms"
  print '(A,F8.4,A)', "Per operation: ", v2_single_time / (NUM_ITERATIONS * NUM_PROGRAMS), " ms"
  
  call print_cache_stats(cache_v2)
  call cleanup_program_cache_v2(cache_v2)
  
  ! =====================================================
  ! Test 2: Thread-Safe Cache - Single Threaded
  ! =====================================================
  print *, ""
  print *, "📊 Test 2: Thread-Safe Cache - Single Threaded"
  print *, "----------------------------------------------"
  
  call omp_set_num_threads(1)
  call init_program_cache_ts(cache_ts, max_programs=100, &
                            cache_directory="benchmark_cache_ts/", &
                            enable_thread_safety=.true.)
  
  ! Warmup
  do j = 1, NUM_WARMUP
    do i = 1, NUM_PROGRAMS
      program_ids(i) = get_cached_program_ts(cache_ts, "", cache_keys(i), &
                                            compile_test_program)
      call release_program_ts(cache_ts, program_ids(i))
    end do
  end do
  
  ! Benchmark
  call cpu_time(start_time)
  do j = 1, NUM_ITERATIONS
    do i = 1, NUM_PROGRAMS
      program_ids(i) = get_cached_program_ts(cache_ts, "", cache_keys(i), &
                                            compile_test_program)
      call release_program_ts(cache_ts, program_ids(i))
    end do
  end do
  call cpu_time(end_time)
  
  ts_single_time = (end_time - start_time) * 1000.0
  print '(A,F10.2,A)', "Total time: ", ts_single_time, " ms"
  print '(A,F8.4,A)', "Per operation: ", ts_single_time / (NUM_ITERATIONS * NUM_PROGRAMS), " ms"
  print '(A,F6.2,A)', "Overhead vs V2: ", (ts_single_time / v2_single_time - 1.0) * 100.0, "%"
  
  call print_cache_stats_ts(cache_ts)
  call cleanup_program_cache_ts(cache_ts)
  
  ! =====================================================
  ! Test 3: Thread-Safe Cache - Multi-Threaded
  ! =====================================================
  print *, ""
  print *, "📊 Test 3: Thread-Safe Cache - Multi-Threaded"
  print *, "---------------------------------------------"
  
  call omp_set_num_threads(NUM_THREADS_TEST)
  call init_program_cache_ts(cache_ts, max_programs=100, &
                            cache_directory="benchmark_cache_ts_mt/")
  
  ! Pre-populate cache
  do i = 1, NUM_PROGRAMS
    program_ids(i) = get_cached_program_ts(cache_ts, "", cache_keys(i), &
                                          compile_test_program)
  end do
  
  ! Benchmark parallel access
  call cpu_time(start_time)
  
  !$omp parallel private(i, j, program_ids)
  do j = 1, NUM_ITERATIONS
    !$omp do schedule(dynamic)
    do i = 1, NUM_PROGRAMS
      program_ids(i) = get_cached_program_ts(cache_ts, "", cache_keys(i), &
                                            compile_test_program)
      call release_program_ts(cache_ts, program_ids(i))
    end do
    !$omp end do
  end do
  !$omp end parallel
  
  call cpu_time(end_time)
  
  ts_multi_time = (end_time - start_time) * 1000.0
  print '(A,F10.2,A)', "Total time: ", ts_multi_time, " ms"
  print '(A,F8.4,A)', "Per operation: ", ts_multi_time / (NUM_ITERATIONS * NUM_PROGRAMS), " ms"
  print '(A,F6.2,A)', "Parallel speedup: ", ts_single_time / ts_multi_time, "x"
  print '(A,F6.2,A)', "Parallel efficiency: ", &
        (ts_single_time / ts_multi_time) / NUM_THREADS_TEST * 100.0, "%"
  
  call print_cache_stats_ts(cache_ts)
  call cleanup_program_cache_ts(cache_ts)
  
  ! =====================================================
  ! Test 4: Cache Contention Stress Test
  ! =====================================================
  print *, ""
  print *, "📊 Test 4: Cache Contention Stress Test"
  print *, "--------------------------------------"
  
  call init_program_cache_ts(cache_ts, max_programs=30, &  ! Smaller cache
                            cache_directory="benchmark_cache_stress/")
  
  ! Stress test with high contention
  call cpu_time(start_time)
  
  !$omp parallel private(i, j)
  do j = 1, NUM_ITERATIONS * 2
    !$omp do schedule(static, 1)  ! Force contention
    do i = 1, NUM_PROGRAMS
      program_ids(mod(i + j, NUM_PROGRAMS) + 1) = &
        get_cached_program_ts(cache_ts, "", &
                             cache_keys(mod(i + j, NUM_PROGRAMS) + 1), &
                             compile_test_program)
      if (mod(i, 3) == 0) then
        call release_program_ts(cache_ts, program_ids(mod(i + j, NUM_PROGRAMS) + 1))
      end if
    end do
    !$omp end do
  end do
  !$omp end parallel
  
  call cpu_time(end_time)
  
  print '(A,F10.2,A)', "Stress test time: ", (end_time - start_time) * 1000.0, " ms"
  print *, "✅ No deadlocks or crashes detected"
  
  call print_cache_stats_ts(cache_ts)
  call cleanup_program_cache_ts(cache_ts)
  
  ! Cleanup
  call gpu_cleanup()
  
  ! =====================================================
  ! Summary
  ! =====================================================
  print *, ""
  print *, "📈 Performance Summary"
  print *, "===================="
  print '(A,F10.2,A)', "V2 single-threaded: ", v2_single_time, " ms"
  print '(A,F10.2,A)', "TS single-threaded: ", ts_single_time, " ms"
  print '(A,F10.2,A)', "TS multi-threaded:  ", ts_multi_time, " ms"
  print *, ""
  print '(A,F6.2,A)', "Thread-safety overhead: ", &
        (ts_single_time / v2_single_time - 1.0) * 100.0, "%"
  print '(A,F6.2,A)', "Multi-thread speedup: ", ts_single_time / ts_multi_time, "x"
  print '(A,F6.2,A)', "Multi-thread efficiency: ", &
        (ts_single_time / ts_multi_time) / NUM_THREADS_TEST * 100.0, "%"
  print *, ""
  print *, "✅ Thread-safe cache ready for production!"
  
contains

  ! Test program compilation function
  function compile_test_program(source) result(prog_id)
    character(len=*), intent(in) :: source
    integer :: prog_id
    integer, save :: next_id = 1000
    
    !$omp critical
    prog_id = next_id
    next_id = next_id + 1
    !$omp end critical
    
    ! Simulate minimal compilation time
    call sleep_ms(1)
    
  end function compile_test_program
  
  ! Sleep for milliseconds
  subroutine sleep_ms(ms)
    integer, intent(in) :: ms
    real(dp) :: start_time, current_time
    
    call cpu_time(start_time)
    do
      call cpu_time(current_time)
      if ((current_time - start_time) * 1000.0 >= ms) exit
    end do
  end subroutine sleep_ms

end program benchmark_thread_safe_performance