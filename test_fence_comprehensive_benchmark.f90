program test_fence_comprehensive_benchmark
  ! Comprehensive Fence Performance Benchmark
  ! ========================================
  !
  ! Tests fence performance across various scenarios
  
  use kinds
  use iso_c_binding
  use gpu_fence_primitives
  use gpu_opengl_interface, only: gpu_init, gpu_cleanup
  implicit none
  
  interface
    subroutine glFinish() bind(C, name="glFinish")
    end subroutine glFinish
    
    subroutine glFlush() bind(C, name="glFlush")
    end subroutine glFlush
  end interface
  
  ! Test parameters
  integer, parameter :: WARMUP_ITERATIONS = 100
  integer, parameter :: TEST_ITERATIONS = 1000
  
  print *, "📊 Comprehensive Fence Benchmark"
  print *, "================================"
  print *, ""
  
  ! Initialize GPU
  if (.not. gpu_init()) then
    print *, "❌ Failed to initialize GPU"
    stop 1
  end if
  
  ! Run all benchmarks
  call benchmark_basic_operations()
  call benchmark_concurrent_fences()
  call benchmark_timeout_scenarios()
  call benchmark_pool_performance()
  call benchmark_real_workload()
  
  ! Cleanup
  call gpu_fence_pool_cleanup()
  call gpu_cleanup()
  
  print *, ""
  print *, "✅ All benchmarks completed!"
  
contains

  ! Benchmark 1: Basic fence operations vs glFinish
  subroutine benchmark_basic_operations()
    type(gpu_fence) :: fence
    integer(i64) :: start_time, end_time
    real(dp) :: fence_create_time, fence_wait_time, fence_destroy_time
    real(dp) :: finish_time, clock_rate
    integer :: i, status
    
    print *, "=== Benchmark 1: Basic Operations ==="
    
    ! Warm up
    do i = 1, WARMUP_ITERATIONS
      call glFlush()
      call glFinish()
      fence = gpu_fence_create()
      call gpu_fence_destroy(fence)
    end do
    
    ! Test fence creation
    call system_clock(start_time, count_rate=clock_rate)
    do i = 1, TEST_ITERATIONS
      fence = gpu_fence_create()
      call gpu_fence_destroy(fence)
    end do
    call system_clock(end_time)
    fence_create_time = real(end_time - start_time) / clock_rate / TEST_ITERATIONS * 1e6
    
    ! Test fence wait
    call system_clock(start_time)
    do i = 1, TEST_ITERATIONS
      call glFlush()
      fence = gpu_fence_create()
      status = gpu_fence_wait(fence, FENCE_INFINITE_TIMEOUT)
      call gpu_fence_destroy(fence)
    end do
    call system_clock(end_time)
    fence_wait_time = real(end_time - start_time) / clock_rate / TEST_ITERATIONS * 1e6
    
    ! Test glFinish
    call system_clock(start_time)
    do i = 1, TEST_ITERATIONS
      call glFlush()
      call glFinish()
    end do
    call system_clock(end_time)
    finish_time = real(end_time - start_time) / clock_rate / TEST_ITERATIONS * 1e6
    
    ! Results
    print '(A,F8.3,A)', "Fence create/destroy: ", fence_create_time, " µs"
    print '(A,F8.3,A)', "Fence wait:           ", fence_wait_time, " µs"
    print '(A,F8.3,A)', "glFinish:             ", finish_time, " µs"
    print '(A,F8.2,A)', "Speedup:              ", finish_time / fence_wait_time, "x"
    print *, ""
    
  end subroutine benchmark_basic_operations
  
  ! Benchmark 2: Multiple concurrent fences
  subroutine benchmark_concurrent_fences()
    type(gpu_fence) :: fences(10)
    integer(i64) :: start_time, end_time
    real(dp) :: concurrent_time, sequential_time, clock_rate
    integer :: i, j, status
    
    print *, "=== Benchmark 2: Concurrent Fences ==="
    
    ! Test creating multiple fences
    call system_clock(start_time, count_rate=clock_rate)
    do i = 1, TEST_ITERATIONS/10
      ! Create 10 fences
      do j = 1, 10
        call glFlush()
        fences(j) = gpu_fence_create()
      end do
      
      ! Wait for all
      do j = 1, 10
        status = gpu_fence_wait(fences(j), FENCE_INFINITE_TIMEOUT)
      end do
      
      ! Cleanup
      do j = 1, 10
        call gpu_fence_destroy(fences(j))
      end do
    end do
    call system_clock(end_time)
    concurrent_time = real(end_time - start_time) / clock_rate / (TEST_ITERATIONS/10) * 1e3
    
    ! Compare with sequential glFinish
    call system_clock(start_time)
    do i = 1, TEST_ITERATIONS/10
      do j = 1, 10
        call glFlush()
        call glFinish()
      end do
    end do
    call system_clock(end_time)
    sequential_time = real(end_time - start_time) / clock_rate / (TEST_ITERATIONS/10) * 1e3
    
    print '(A,F8.3,A)', "10 concurrent fences: ", concurrent_time, " ms"
    print '(A,F8.3,A)', "10 sequential glFinish: ", sequential_time, " ms"
    print '(A,F8.2,A)', "Speedup: ", sequential_time / concurrent_time, "x"
    print *, ""
    
  end subroutine benchmark_concurrent_fences
  
  ! Benchmark 3: Timeout scenarios
  subroutine benchmark_timeout_scenarios()
    type(gpu_fence) :: fence
    integer(i64) :: start_time, end_time
    real(dp) :: immediate_time, short_time, clock_rate
    integer :: i, status
    integer :: timeout_count, ready_count
    
    print *, "=== Benchmark 3: Timeout Scenarios ==="
    
    ! Test immediate check (0 timeout)
    timeout_count = 0
    ready_count = 0
    
    call system_clock(start_time, count_rate=clock_rate)
    do i = 1, TEST_ITERATIONS
      fence = gpu_fence_create()
      status = gpu_fence_wait(fence, 0_i64)  ! No wait
      if (status == FENCE_TIMEOUT) timeout_count = timeout_count + 1
      if (status == FENCE_READY) ready_count = ready_count + 1
      call gpu_fence_destroy(fence)
    end do
    call system_clock(end_time)
    immediate_time = real(end_time - start_time) / clock_rate / TEST_ITERATIONS * 1e6
    
    ! Test short timeout (1µs)
    call system_clock(start_time)
    do i = 1, TEST_ITERATIONS
      fence = gpu_fence_create()
      status = gpu_fence_wait(fence, 1000_i64)  ! 1µs
      call gpu_fence_destroy(fence)
    end do
    call system_clock(end_time)
    short_time = real(end_time - start_time) / clock_rate / TEST_ITERATIONS * 1e6
    
    print '(A,F8.3,A)', "Immediate check (0ns): ", immediate_time, " µs"
    print '(A,I0,A,I0)', "  Ready: ", ready_count, ", Timeout: ", timeout_count
    print '(A,F8.3,A)', "Short timeout (1µs): ", short_time, " µs"
    print *, ""
    
  end subroutine benchmark_timeout_scenarios
  
  ! Benchmark 4: Fence pool performance
  subroutine benchmark_pool_performance()
    type(gpu_fence) :: fence
    integer(i64) :: start_time, end_time
    real(dp) :: pool_time, clock_rate
    integer :: i, created, max_concurrent
    type(gpu_fence) :: active_fences(100)
    
    print *, "=== Benchmark 4: Fence Pool Performance ==="
    
    ! Test pool allocation speed
    call system_clock(start_time, count_rate=clock_rate)
    do i = 1, TEST_ITERATIONS * 10
      fence = gpu_fence_create()
      call gpu_fence_destroy(fence)
    end do
    call system_clock(end_time)
    pool_time = real(end_time - start_time) / clock_rate / (TEST_ITERATIONS * 10) * 1e6
    
    ! Test pool limits
    created = 0
    do i = 1, 100
      active_fences(i) = gpu_fence_create()
      if (gpu_fence_is_valid(active_fences(i))) created = created + 1
    end do
    max_concurrent = created
    
    ! Cleanup
    do i = 1, 100
      if (gpu_fence_is_valid(active_fences(i))) then
        call gpu_fence_destroy(active_fences(i))
      end if
    end do
    
    print '(A,F8.3,A)', "Pool alloc/free: ", pool_time, " µs"
    print '(A,I0)', "Max concurrent fences: ", max_concurrent
    print '(A,I0)', "Pool size: ", 64  ! From gpu_fence_primitives
    print *, ""
    
  end subroutine benchmark_pool_performance
  
  ! Benchmark 5: Real workload simulation
  subroutine benchmark_real_workload()
    type(gpu_fence) :: fence
    integer(i64) :: start_time, end_time
    real(dp) :: fence_workload_time, finish_workload_time, clock_rate
    integer :: i, status
    
    print *, "=== Benchmark 5: Real Workload Simulation ==="
    
    ! Simulate workload with fences
    call system_clock(start_time, count_rate=clock_rate)
    do i = 1, 100
      ! Submit work
      call glFlush()
      
      ! Create fence
      fence = gpu_fence_create()
      
      ! Do CPU work while GPU runs
      call simulate_cpu_work()
      
      ! Wait for GPU
      status = gpu_fence_wait(fence, 100000000_i64)  ! 100ms timeout
      
      ! Cleanup
      call gpu_fence_destroy(fence)
    end do
    call system_clock(end_time)
    fence_workload_time = real(end_time - start_time) / clock_rate
    
    ! Simulate workload with glFinish
    call system_clock(start_time)
    do i = 1, 100
      ! Submit work
      call glFlush()
      
      ! Wait for GPU (blocks CPU)
      call glFinish()
      
      ! Do CPU work after GPU completes
      call simulate_cpu_work()
    end do
    call system_clock(end_time)
    finish_workload_time = real(end_time - start_time) / clock_rate
    
    print '(A,F8.3,A)', "Fence-based workload: ", fence_workload_time * 1000, " ms"
    print '(A,F8.3,A)', "glFinish workload: ", finish_workload_time * 1000, " ms"
    print '(A,F8.2,A)', "Speedup: ", finish_workload_time / fence_workload_time, "x"
    print '(A,F8.3,A)', "Time saved: ", (finish_workload_time - fence_workload_time) * 1000, " ms"
    print *, ""
    
  end subroutine benchmark_real_workload
  
  ! Simulate CPU work
  subroutine simulate_cpu_work()
    real(dp) :: x
    integer :: i
    x = 1.0_dp
    do i = 1, 10000
      x = x * 1.000001_dp
    end do
    if (x < 0) print *, x  ! Prevent optimization
  end subroutine simulate_cpu_work
  
end program test_fence_comprehensive_benchmark