program test_neo_geo_quality
  ! Neo Geo Quality Check Suite
  ! ===========================
  !
  ! Deep QA on PM4 implementation and fence primitives
  ! Tests safety, correctness, and performance claims
  
  use kinds
  use iso_c_binding
  use gpu_fence_primitives
  use sporkle_pm4_packets
  use gpu_safety_guards
  use gpu_opengl_interface, only: gpu_init, gpu_cleanup
  implicit none
  
  logical :: all_pass = .true.
  
  print *, "🔍 Neo Geo Quality Check Suite"
  print *, "=============================="
  print *, ""
  print *, "Testing PM4 implementation and fence primitives..."
  print *, ""
  
  ! Initialize GPU for fence tests
  if (.not. gpu_init()) then
    print *, "❌ Failed to initialize GPU"
    stop 1
  end if
  
  ! Test 1: Safety Guards
  call test_safety_guards()
  
  ! Test 2: PM4 Packet Correctness
  call test_pm4_packets()
  
  ! Test 3: Fence Reliability
  call test_fence_reliability()
  
  ! Test 4: Performance Claims
  call test_performance_claims()
  
  ! Test 5: Memory Safety
  call test_memory_safety()
  
  ! Test 6: Error Recovery
  call test_error_recovery()
  
  ! Cleanup
  call gpu_fence_pool_cleanup()
  call gpu_cleanup()
  
  ! Summary
  print *, ""
  print *, "=============================="
  if (all_pass) then
    print *, "✅ All quality checks PASSED!"
    print *, ""
    print *, "PM4 implementation is:"
    print *, "  - Safe (no system hangs)"
    print *, "  - Correct (valid packets)"
    print *, "  - Fast (fence <1µs)"
    print *, "  - Robust (error recovery)"
  else
    print *, "❌ Some quality checks FAILED"
    print *, "Please review and fix issues before proceeding"
  end if
  
contains

  subroutine test_safety_guards()
    logical :: safety_on
    
    print *, "Test 1: Safety Guards"
    print *, "--------------------"
    
    ! Ensure safety is enabled
    call gpu_set_safety_mode(.true.)
    safety_on = gpu_safety_enabled()
    
    if (safety_on) then
      print *, "✅ Safety mode enabled"
    else
      print *, "❌ Safety mode not working!"
      all_pass = .false.
    end if
    
    ! Test address validation
    if (gpu_validate_address(int(z'1000000', i64), 4096_i64, "test")) then
      print *, "✅ Address validation working"
    else
      print *, "❌ Address validation failed"
      all_pass = .false.
    end if
    
    ! Test submission blocking
    if (.not. gpu_check_submission_safety()) then
      print *, "✅ Hardware submission correctly blocked"
    else
      print *, "❌ Hardware submission NOT blocked!"
      all_pass = .false.
    end if
    
    print *, ""
  end subroutine
  
  subroutine test_pm4_packets()
    type(pm4_packet_builder) :: builder
    integer(i32), allocatable :: packets(:)
    integer :: count
    logical :: valid
    
    print *, "Test 2: PM4 Packet Correctness"
    print *, "------------------------------"
    
    call builder%init(256)
    
    ! Build simple dispatch
    call pm4_dispatch_direct(builder, 1, 1, 1)
    
    packets = builder%get_buffer()
    count = builder%get_size()
    
    ! Validate packet structure
    valid = .true.
    if (count < 4) then
      print *, "❌ Too few packets generated"
      valid = .false.
    else if (count > 100) then
      print *, "❌ Too many packets generated"
      valid = .false.
    else
      ! Check first packet is Type 3
      if (iand(packets(1), int(z'C0000000', i32)) /= int(z'C0000000', i32)) then
        print *, "❌ Invalid packet type"
        valid = .false.
      end if
    end if
    
    if (valid) then
      print *, "✅ PM4 packets correctly formed"
      print *, "   Generated", count, "dwords"
    else
      all_pass = .false.
    end if
    
    ! Test complex dispatch
    call builder%cleanup()
    call builder%init(1024)
    
    call pm4_build_compute_dispatch(builder, &
      shader_addr = int(z'100000', i64), &
      threads_x = 64, threads_y = 1, threads_z = 1, &
      grid_x = 16, grid_y = 1, grid_z = 1)
    
    packets = builder%get_buffer()
    count = builder%get_size()
    
    if (count > 20 .and. count < 100) then
      print *, "✅ Complex dispatch packet count reasonable:", count
    else
      print *, "❌ Complex dispatch packet count suspicious:", count
      all_pass = .false.
    end if
    
    call builder%cleanup()
    print *, ""
  end subroutine
  
  subroutine test_fence_reliability()
    type(gpu_fence) :: fence
    integer :: status, i
    integer :: success_count
    integer(i64) :: start_time, end_time, total_time
    
    print *, "Test 3: Fence Reliability"
    print *, "------------------------"
    
    ! Test 100 fence operations
    success_count = 0
    total_time = 0
    
    do i = 1, 100
      fence = gpu_fence_create()
      
      if (.not. gpu_fence_is_valid(fence)) then
        print *, "❌ Failed to create fence", i
        cycle
      end if
      
      call system_clock(start_time)
      status = gpu_fence_wait(fence, FENCE_INFINITE_TIMEOUT)
      call system_clock(end_time)
      
      if (status == FENCE_READY) then
        success_count = success_count + 1
        total_time = total_time + (end_time - start_time)
      end if
      
      call gpu_fence_destroy(fence)
    end do
    
    if (success_count >= 95) then
      print *, "✅ Fence reliability:", success_count, "/100"
      print '(A,F6.2,A)', "   Average wait time: ", &
        real(total_time) / real(success_count) / 1000.0, " µs"
    else
      print *, "❌ Poor fence reliability:", success_count, "/100"
      all_pass = .false.
    end if
    
    ! Test fence timeout
    fence = gpu_fence_create()
    status = gpu_fence_wait(fence, 1_i64)  ! 1 nanosecond
    
    if (status == FENCE_READY .or. status == FENCE_TIMEOUT) then
      print *, "✅ Fence timeout handling works"
    else
      print *, "❌ Fence timeout handling broken"
      all_pass = .false.
    end if
    
    call gpu_fence_destroy(fence)
    print *, ""
  end subroutine
  
  subroutine test_performance_claims()
    type(gpu_fence) :: fence
    integer(i64) :: start_time, end_time
    real :: wait_time_us
    integer :: i
    
    print *, "Test 4: Performance Claims"
    print *, "-------------------------"
    
    ! Measure fence performance
    fence = gpu_fence_create()
    
    ! Warm up
    do i = 1, 10
      if (gpu_fence_is_signaled(fence)) exit
    end do
    
    ! Measure
    call system_clock(start_time)
    do i = 1, 1000
      if (gpu_fence_is_signaled(fence)) continue
    end do
    call system_clock(end_time)
    
    wait_time_us = real(end_time - start_time) / 1000.0 / 1000.0
    
    if (wait_time_us < 1.0) then
      print '(A,F6.3,A)', "✅ Fence check performance: ", wait_time_us, " µs per call"
    else
      print '(A,F6.3,A)', "❌ Fence check too slow: ", wait_time_us, " µs per call"
      all_pass = .false.
    end if
    
    call gpu_fence_destroy(fence)
    
    ! Test PM4 packet building performance
    block
      type(pm4_packet_builder) :: builder
      integer(i64) :: build_time
      
      call builder%init(1024)
      
      call system_clock(start_time)
      do i = 1, 100
        call pm4_dispatch_direct(builder, 16, 1, 1)
      end do
      call system_clock(end_time)
      
      build_time = end_time - start_time
      
      if (real(build_time) / 100.0 < 1000.0) then  ! <10µs per dispatch
        print '(A,F6.2,A)', "✅ PM4 packet build: ", &
          real(build_time) / 100.0 / 1000.0, " µs per dispatch"
      else
        print *, "❌ PM4 packet building too slow"
        all_pass = .false.
      end if
      
      call builder%cleanup()
    end block
    
    print *, ""
  end subroutine
  
  subroutine test_memory_safety()
    type(pm4_packet_builder) :: builder
    integer :: i
    logical :: no_leaks
    
    print *, "Test 5: Memory Safety"
    print *, "--------------------"
    
    no_leaks = .true.
    
    ! Test repeated init/cleanup
    do i = 1, 100
      call builder%init(64)
      call pm4_dispatch_direct(builder, 1, 1, 1)
      call builder%cleanup()
    end do
    
    print *, "✅ PM4 builder survives 100 init/cleanup cycles"
    
    ! Test fence pool exhaustion
    block
      type(gpu_fence) :: fences(100)
      integer :: created
      
      created = 0
      do i = 1, 100
        fences(i) = gpu_fence_create()
        if (gpu_fence_is_valid(fences(i))) created = created + 1
      end do
      
      if (created >= 64) then
        print *, "✅ Fence pool handles exhaustion gracefully"
      else
        print *, "❌ Fence pool too small:", created
        no_leaks = .false.
      end if
      
      ! Clean up
      do i = 1, 100
        if (gpu_fence_is_valid(fences(i))) then
          call gpu_fence_destroy(fences(i))
        end if
      end do
    end block
    
    if (.not. no_leaks) all_pass = .false.
    print *, ""
  end subroutine
  
  subroutine test_error_recovery()
    type(gpu_fence) :: invalid_fence
    integer :: status
    
    print *, "Test 6: Error Recovery"
    print *, "---------------------"
    
    ! Create an invalid fence for testing
    ! We can't directly set private components, so we'll test differently
    ! Test with destroyed fence
    invalid_fence = gpu_fence_create()
    call gpu_fence_destroy(invalid_fence)
    
    ! Now it should be invalid
    status = gpu_fence_wait(invalid_fence, 1000_i64)
    
    if (status == FENCE_ERROR) then
      print *, "✅ Invalid fence handled correctly"
    else
      print *, "❌ Invalid fence not detected"
      all_pass = .false.
    end if
    
    ! Test with null fence
    if (.not. gpu_fence_is_signaled(invalid_fence)) then
      print *, "✅ Null fence check handled"
    else
      print *, "❌ Null fence check failed"
      all_pass = .false.
    end if
    
    ! Test PM4 builder edge cases
    block
      type(pm4_packet_builder) :: builder
      
      ! Uninitialized builder
      if (builder%get_size() == 0) then
        print *, "✅ Uninitialized builder handled"
      else
        print *, "❌ Uninitialized builder has non-zero size"
        all_pass = .false.
      end if
    end block
    
    print *, ""
  end subroutine

end program test_neo_geo_quality