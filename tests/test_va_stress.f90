program test_va_stress
  use iso_c_binding
  use sporkle_amdgpu_direct
  use sporkle_gpu_va_allocator
  implicit none
  
  type(amdgpu_device) :: device
  type(amdgpu_buffer) :: buffers(10)
  integer :: status, i
  integer(c_int64_t) :: va_addrs(10)
  integer(c_int32_t) :: context_id, bo_list
  integer(c_int32_t) :: handles(10)
  
  print *, "=== VA Mapping Stress Test ==="
  
  ! Open device
  device = amdgpu_open_device("/dev/dri/renderD128")  ! dGPU
  if (device%fd < 0) then
    print *, "Failed to open device"
    stop 1
  end if
  
  ! Initialize VA allocator  
  call gpu_va_init()
  
  ! Create context for command submission
  context_id = amdgpu_create_context(device)
  if (context_id < 0) then
    print *, "Failed to create context"
    stop 1
  end if
  
  print *, "=== Test 1: Multiple buffer allocation and VA mapping ==="
  do i = 1, 10
    ! Allocate buffer
    buffers(i) = amdgpu_allocate_buffer(device, 4096_c_int64_t)
    if (buffers(i)%handle == 0) then
      print *, "Failed to allocate buffer", i
      stop 1
    end if
    
    ! Map buffer for CPU access
    status = amdgpu_map_buffer(device, buffers(i))
    if (status /= 0) then
      print *, "Failed to map buffer", i, "for CPU"
      stop 1
    end if
    
    ! Allocate VA address
    va_addrs(i) = gpu_va_allocate(buffers(i)%size)
    if (va_addrs(i) == 0) then
      print *, "Failed to allocate VA for buffer", i
      stop 1
    end if
    
    ! Map to GPU VA
    status = amdgpu_map_va(device, buffers(i), va_addrs(i))
    if (status /= 0) then
      print *, "VA mapping failed for buffer", i, "status:", status
      stop 1
    end if
    
    handles(i) = buffers(i)%handle
    print '(A,I0,A,Z16)', "Buffer ", i, " mapped to VA: 0x", va_addrs(i)
  end do
  
  print *, "=== Test 2: Creating BO list with mapped buffers ==="
  bo_list = amdgpu_create_bo_list(device, handles, 10)
  if (bo_list == 0) then
    print *, "Failed to create BO list"
    stop 1
  end if
  
  print *, "=== Test 3: Invalid VA mapping attempts ==="
  ! Try to map same buffer to overlapping address
  status = amdgpu_map_va(device, buffers(1), va_addrs(2))
  if (status == 0) then
    print *, "WARNING: Overlapping VA mapping succeeded (unexpected)"
  else
    print *, "Expected: Overlapping VA mapping failed (errno should be EBUSY)"
  end if
  
  ! Try to map to invalid address (0x0)
  status = amdgpu_map_va(device, buffers(1), 0_c_int64_t)
  if (status == 0) then
    print *, "WARNING: NULL VA mapping succeeded (unexpected)"
  else
    print *, "Expected: NULL VA mapping failed"
  end if
  
  ! Try to map to address outside our VA space
  status = amdgpu_map_va(device, buffers(1), int(z'80000000000', c_int64_t))
  if (status == 0) then
    print *, "WARNING: Out-of-range VA mapping succeeded (might be OK)"
  else
    print *, "Expected: Out-of-range VA mapping failed"
  end if
  
  print *, "=== Test 4: Command submission with VA-mapped buffers ==="
  block
    type(amdgpu_command_buffer) :: cmd_buf
    type(amdgpu_buffer) :: ib_buffer
    integer(c_int32_t), pointer :: ib(:)
    
    ! Allocate command buffer
    ib_buffer = amdgpu_allocate_buffer(device, 1024_c_int64_t)
    status = amdgpu_map_buffer(device, ib_buffer)
    
    ! Simple command that does nothing harmful
    call c_f_pointer(ib_buffer%cpu_ptr, ib, [256])
    ib(1) = int(z'FFFF1000', c_int32_t)  ! NOP packet
    ib(2) = int(z'FFFF1000', c_int32_t)  ! NOP packet
    
    ! Setup command buffer
    cmd_buf%ctx_id = context_id
    cmd_buf%bo_list_handle = bo_list
    cmd_buf%ib_buffer = ib_buffer
    cmd_buf%ib_size = 8  ! 2 dwords
    
    ! Submit (this might reveal VA mapping issues)
    status = amdgpu_submit_command_buffer(device, cmd_buf)
    if (status /= 0) then
      print *, "Command submission failed with status:", status
    else
      print *, "Command submission with VA-mapped buffers succeeded"
      ! Wait for completion
      status = amdgpu_wait_buffer_idle(device, ib_buffer, 1000000000_c_int64_t)
      if (status /= 0) then
        print *, "Wait failed with status:", status
      end if
    end if
  end block
  
  print *, "=== Cleanup ==="
  ! Unmap all VAs
  do i = 1, 10
    call amdgpu_unmap_va(device, buffers(i))
  end do
  
  ! Destroy BO list
  call amdgpu_destroy_bo_list(device, bo_list)
  
  ! Destroy context
  call amdgpu_destroy_context(device, context_id)
  
  call amdgpu_close_device(device)
  print *, "✅ VA stress test complete!"
  
end program