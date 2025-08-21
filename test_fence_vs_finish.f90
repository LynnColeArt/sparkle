program test_fence_vs_finish
  ! Fence vs glFinish Performance Comparison
  ! ========================================
  !
  ! Empirically measures the performance difference
  
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
  
  integer, parameter :: NUM_ITERATIONS = 1000
  integer(i64) :: start_time, end_time
  real(rk64) :: finish_time_us, fence_time_us, speedup
  type(gpu_fence) :: fence
  integer :: i, status
  
  print *, "⚡ Fence vs glFinish Performance Test"
  print *, "===================================="
  print *, ""
  
  ! Initialize GPU
  if (.not. gpu_init()) then
    print *, "❌ Failed to initialize GPU"
    stop 1
  end if
  
  ! Warm up
  do i = 1, 100
    call glFlush()
    fence = gpu_fence_create()
    call gpu_fence_destroy(fence)
  end do
  
  ! Test 1: glFinish performance
  print *, "Testing glFinish performance..."
  call system_clock(start_time)
  
  do i = 1, NUM_ITERATIONS
    call glFlush()  ! Ensure some work
    call glFinish() ! Full sync
  end do
  
  call system_clock(end_time)
  finish_time_us = real(end_time - start_time, rk64) / real(NUM_ITERATIONS, rk64) / 1000.0_rk64
  
  ! Test 2: Fence performance
  print *, "Testing fence performance..."
  call system_clock(start_time)
  
  do i = 1, NUM_ITERATIONS
    call glFlush()  ! Ensure some work
    fence = gpu_fence_create()
    status = gpu_fence_wait(fence, FENCE_INFINITE_TIMEOUT)
    call gpu_fence_destroy(fence)
  end do
  
  call system_clock(end_time)
  fence_time_us = real(end_time - start_time, rk64) / real(NUM_ITERATIONS, rk64) / 1000.0_rk64
  
  ! Calculate speedup
  speedup = finish_time_us / fence_time_us
  
  ! Results
  print *, ""
  print *, "📊 Performance Results"
  print *, "--------------------"
  print '(A,F8.2,A)', " glFinish time:    ", finish_time_us, " µs/call"
  print '(A,F8.2,A)', " Fence time:       ", fence_time_us, " µs/call"
  print '(A,F8.2,A)', " Speedup:          ", speedup, "x"
  print *, ""
  
  ! Analysis
  if (speedup > 1.5) then
    print *, "✅ Significant speedup achieved!"
    print '(A,F6.1,A)', "   You save ", finish_time_us - fence_time_us, " µs per sync"
    print '(A,F6.1,A)', "   For 1000 syncs/sec, that's ", &
      (finish_time_us - fence_time_us) * 1000.0 / 1000.0, " ms saved"
  else
    print *, "⚠️  Speedup less than expected"
    print *, "   This might be due to:"
    print *, "   - No actual GPU work between syncs"
    print *, "   - Driver optimizations for glFinish"
    print *, "   - Platform-specific behavior"
  end if
  
  ! Test 3: Fence creation/destruction overhead
  print *, ""
  print *, "Testing fence overhead..."
  call system_clock(start_time)
  
  do i = 1, NUM_ITERATIONS * 10
    fence = gpu_fence_create()
    call gpu_fence_destroy(fence)
  end do
  
  call system_clock(end_time)
  print '(A,F6.2,A)', " Fence create/destroy: ", &
    real(end_time - start_time, rk64) / real(NUM_ITERATIONS * 10, rk64) / 1000.0_rk64, &
    " µs/pair"
  
  ! Cleanup
  call gpu_fence_pool_cleanup()
  call gpu_cleanup()
  
  ! Summary
  print *, ""
  print *, "💡 Key Insights:"
  print *, "   - Fences are lightweight sync objects"
  print *, "   - Best speedup with actual GPU work"
  print *, "   - Pool prevents allocation overhead"
  print *, ""
  print *, "🎯 Recommended usage:"
  print *, "   - Use fences for fine-grained sync"
  print *, "   - Keep fences alive across frames"
  print *, "   - Use timeout for async operations"

end program test_fence_vs_finish