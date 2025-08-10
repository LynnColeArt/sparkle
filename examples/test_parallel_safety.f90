program test_parallel_safety
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_int
  use sparkle_config
  use omp_lib
  implicit none
  
  type(sparkle_config_type) :: config
  integer :: total_threads, safe_threads
  integer :: i, n
  real(real64) :: start_time, end_time
  real(real32), allocatable :: data(:)
  real(real32) :: sum_result
  
  print *, "🛡️  Sparkle Thread Safety Configuration"
  print *, "======================================"
  print *, ""
  
  ! Show current system info
  total_threads = omp_get_max_threads()
  print '(A,I0)', "Total CPU threads available: ", total_threads
  
  ! Test 1: Default configuration
  print *, ""
  print *, "Test 1: Default Configuration"
  print *, "-----------------------------"
  config = sparkle_get_config()
  print '(A,I0)', "Thread reserve: ", config%thread_reserve
  print '(A,I0)', "Max CPU threads: ", config%max_cpu_threads
  
  ! Calculate safe thread count
  if (config%max_cpu_threads > 0) then
    safe_threads = min(config%max_cpu_threads, total_threads)
  else
    safe_threads = max(1, total_threads - config%thread_reserve)
  end if
  print '(A,I0)', "Safe thread count: ", safe_threads
  
  ! Test 2: User configuration (14 threads max)
  print *, ""
  print *, "Test 2: User Configuration (14 threads max)"
  print *, "------------------------------------------"
  config%max_cpu_threads = 14
  config%thread_reserve = 0  ! Already handled by max_cpu_threads
  call sparkle_set_config(config)
  
  config = sparkle_get_config()
  print '(A,I0)', "Max CPU threads: ", config%max_cpu_threads
  print '(A,I0)', "Safe thread count: ", min(14, total_threads)
  
  ! Test 3: Environment variable
  print *, ""
  print *, "Test 3: Environment Variable Override"
  print *, "------------------------------------"
  print *, "Set SPARKLE_MAX_CPU_THREADS=12 to test"
  print *, "Set SPARKLE_THREAD_RESERVE=4 to test"
  
  ! Test 4: Parallel performance test
  print *, ""
  print *, "Test 4: Parallel Performance Comparison"
  print *, "--------------------------------------"
  
  n = 100000000  ! 100M elements
  allocate(data(n))
  call random_number(data)
  
  ! Single-threaded
  call omp_set_num_threads(1)
  start_time = omp_get_wtime()
  sum_result = 0.0
  !$OMP PARALLEL DO REDUCTION(+:sum_result)
  do i = 1, n
    sum_result = sum_result + data(i)
  end do
  !$OMP END PARALLEL DO
  end_time = omp_get_wtime()
  print '(A,F8.3,A)', "Single-threaded time: ", (end_time - start_time) * 1000.0, " ms"
  
  ! Safe multi-threaded (14 threads)
  call omp_set_num_threads(14)
  start_time = omp_get_wtime()
  sum_result = 0.0
  !$OMP PARALLEL DO REDUCTION(+:sum_result)
  do i = 1, n
    sum_result = sum_result + data(i)
  end do
  !$OMP END PARALLEL DO
  end_time = omp_get_wtime()
  print '(A,F8.3,A,I0,A)', "Safe parallel time: ", (end_time - start_time) * 1000.0, &
        " ms (", 14, " threads)"
  
  ! Show actual threads used
  !$OMP PARALLEL
  !$OMP MASTER
  print '(A,I0)', "Actual threads in use: ", omp_get_num_threads()
  !$OMP END MASTER
  !$OMP END PARALLEL
  
  deallocate(data)
  
  print *, ""
  print *, "Configuration Guide:"
  print *, "==================="
  print *, "1. In code: config%max_cpu_threads = 14"
  print *, "2. Environment: export SPARKLE_MAX_CPU_THREADS=14"
  print *, "3. Reserve threads: export SPARKLE_THREAD_RESERVE=2"
  print *, ""
  print *, "For your system with 16 threads:"
  print *, "- Use max_cpu_threads=14 to leave 2 for system"
  print *, "- Or use thread_reserve=2 (auto calculates to 14)"
  print *, ""
  print *, "✨ The Sparkle Way: Be a good neighbor to your OS!"
  
end program test_parallel_safety