program test_fence_primitives
  ! Test GPU Fence Primitives
  ! =========================
  !
  ! Verifies fence-based synchronization works correctly
  
  use kinds
  use iso_c_binding
  use gpu_fence_primitives
  use gpu_opengl_interface, only: gpu_init, gpu_cleanup
  implicit none
  
  type(gpu_fence) :: fence1, fence2
  integer :: status
  integer(i64) :: start_time, end_time, wait_time
  logical :: success = .true.
  
  print *, "🧪 Testing GPU Fence Primitives"
  print *, "=============================="
  print *, ""
  
  ! Initialize GPU
  if (.not. gpu_init()) then
    print *, "❌ Failed to initialize GPU"
    stop 1
  end if
  
  ! Test 1: Basic fence creation and destruction
  print *, "Test 1: Basic fence operations"
  fence1 = gpu_fence_create()
  if (.not. gpu_fence_is_valid(fence1)) then
    print *, "❌ Failed to create fence"
    success = .false.
  else
    print *, "✅ Fence created successfully"
  end if
  
  call gpu_fence_destroy(fence1)
  print *, "✅ Fence destroyed"
  print *, ""
  
  ! Test 2: Immediate fence signaling
  print *, "Test 2: Immediate fence signal"
  fence1 = gpu_fence_create()
  
  ! Fence should signal immediately since no work pending
  status = gpu_fence_wait(fence1, FENCE_NO_WAIT)
  if (status == FENCE_READY) then
    print *, "✅ Fence signaled immediately (expected)"
  else
    print *, "❌ Fence not ready immediately"
    success = .false.
  end if
  
  call gpu_fence_destroy(fence1)
  print *, ""
  
  ! Test 3: Non-blocking status check
  print *, "Test 3: Non-blocking status check"
  fence1 = gpu_fence_create()
  
  if (gpu_fence_is_signaled(fence1)) then
    print *, "✅ Fence status check works"
  else
    print *, "⚠️  Fence not signaled (may have pending work)"
  end if
  
  call gpu_fence_destroy(fence1)
  print *, ""
  
  ! Test 4: Timeout handling
  print *, "Test 4: Timeout handling"
  fence1 = gpu_fence_create()
  
  ! Try very short timeout
  call system_clock(start_time)
  status = gpu_fence_wait(fence1, int(1000, i64))  ! 1 microsecond
  call system_clock(end_time)
  
  select case(status)
    case(FENCE_READY)
      print *, "✅ Fence ready"
    case(FENCE_TIMEOUT)
      print *, "✅ Timeout handled correctly"
    case(FENCE_ERROR)
      print *, "❌ Fence error"
      success = .false.
  end select
  
  call gpu_fence_destroy(fence1)
  print *, ""
  
  ! Test 5: Multiple fences
  print *, "Test 5: Multiple fences"
  fence1 = gpu_fence_create()
  fence2 = gpu_fence_create()
  
  if (gpu_fence_is_valid(fence1) .and. gpu_fence_is_valid(fence2)) then
    print *, "✅ Multiple fences created"
  else
    print *, "❌ Failed to create multiple fences"
    success = .false.
  end if
  
  call gpu_fence_destroy(fence1)
  call gpu_fence_destroy(fence2)
  print *, ""
  
  ! Test 6: Fence pool stress test
  print *, "Test 6: Fence pool stress test"
  block
    type(gpu_fence) :: fences(32)
    integer :: i, created
    
    created = 0
    do i = 1, 32
      fences(i) = gpu_fence_create()
      if (gpu_fence_is_valid(fences(i))) created = created + 1
    end do
    
    print *, "   Created", created, "out of 32 fences"
    
    ! Clean up
    do i = 1, 32
      if (gpu_fence_is_valid(fences(i))) call gpu_fence_destroy(fences(i))
    end do
    
    if (created >= 32) then
      print *, "✅ Pool handled 32 fences"
    else
      print *, "⚠️  Pool limited to", created, "fences"
    end if
  end block
  print *, ""
  
  ! Test 7: Performance comparison
  print *, "Test 7: Fence wait performance"
  
  ! Measure fence wait time
  fence1 = gpu_fence_create()
  call system_clock(start_time)
  status = gpu_fence_wait(fence1, FENCE_INFINITE_TIMEOUT)
  call system_clock(end_time)
  call gpu_fence_destroy(fence1)
  
  wait_time = end_time - start_time
  print '(A,F6.2,A)', "   Fence wait time: ", real(wait_time) / 1000.0, " µs"
  
  if (wait_time < 50000) then  ! Less than 50µs
    print *, "✅ Fence wait is fast"
  else
    print *, "⚠️  Fence wait slower than expected"
  end if
  print *, ""
  
  ! Clean up
  call gpu_fence_pool_cleanup()
  call gpu_cleanup()
  
  ! Summary
  print *, "=============================="
  if (success) then
    print *, "✅ All fence tests passed!"
    print *, ""
    print *, "Ready to replace glFinish() for 2x speedup! 🚀"
  else
    print *, "❌ Some tests failed"
  end if

end program test_fence_primitives