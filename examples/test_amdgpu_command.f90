program test_amdgpu_command
  ! Test program for AMDGPU command submission
  
  use sporkle_amdgpu_direct
  use iso_c_binding
  implicit none
  
  type(amdgpu_device) :: device
  type(amdgpu_buffer) :: data_buffer
  type(amdgpu_command_buffer) :: cmd_buf
  integer :: status
  real, pointer :: buffer_data(:)
  integer :: i
  integer(c_int32_t) :: ctx_id, bo_list_handle
  integer(c_int32_t) :: handles(2)
  
  ! PM4 packet data for testing
  ! Using simple NOP packets with proper PM4 format
  integer(c_int32_t) :: shader_data(4) = [ &
    int(z'C0001000', c_int32_t), & ! PM4 NOP packet (type 3, NOP opcode, count=1)
    int(z'00000000', c_int32_t), & ! NOP data
    int(z'C0001000', c_int32_t), & ! PM4 NOP packet
    int(z'00000000', c_int32_t)  & ! NOP data
  ]
  
  print *, "=== AMDGPU Command Submission Test ==="
  
  ! Open device
  device = amdgpu_open_device()
  if (.not. device%is_open) then
    print *, "Failed to open device"
    stop 1
  end if
  
  ! Create context
  ctx_id = amdgpu_create_context(device)
  if (ctx_id < 0) then
    print *, "Failed to create context"
    call amdgpu_close_device(device)
    stop 1
  end if
  
  ! Allocate data buffer (1MB)
  data_buffer = amdgpu_allocate_buffer(device, int(1024*1024, c_int64_t))
  if (data_buffer%handle == 0) then
    print *, "Failed to allocate data buffer"
    call amdgpu_close_device(device)
    stop 1
  end if
  
  ! Map data buffer
  status = amdgpu_map_buffer(device, data_buffer)
  if (status /= 0) then
    print *, "Failed to map data buffer"
    call amdgpu_close_device(device)
    stop 1
  end if
  
  ! Initialize data buffer
  call c_f_pointer(data_buffer%cpu_ptr, buffer_data, [1024*1024/4])
  do i = 1, size(buffer_data)
    buffer_data(i) = real(i)
  end do
  print *, "✅ Initialized data buffer"
  
  ! Allocate command buffer (4KB)
  cmd_buf%ib_buffer = amdgpu_allocate_buffer(device, int(4096, c_int64_t))
  if (cmd_buf%ib_buffer%handle == 0) then
    print *, "Failed to allocate command buffer"
    call amdgpu_close_device(device)
    stop 1
  end if
  
  ! Map command buffer
  status = amdgpu_map_buffer(device, cmd_buf%ib_buffer)
  if (status /= 0) then
    print *, "Failed to map command buffer"
    call amdgpu_close_device(device)
    stop 1
  end if
  
  ! Map buffers to GPU VA space
  status = amdgpu_map_va(device, data_buffer, int(z'400000', c_int64_t))  ! 4MB
  if (status /= 0) then
    print *, "Failed to map data buffer to GPU VA"
    call amdgpu_close_device(device)
    stop 1
  end if
  
  status = amdgpu_map_va(device, cmd_buf%ib_buffer, int(z'500000', c_int64_t))  ! 5MB
  if (status /= 0) then
    print *, "Failed to map command buffer to GPU VA"
    call amdgpu_close_device(device)
    stop 1
  end if
  
  ! Create BO list with both buffers
  handles(1) = data_buffer%handle
  handles(2) = cmd_buf%ib_buffer%handle
  bo_list_handle = amdgpu_create_bo_list(device, handles, 2)
  if (bo_list_handle == 0) then
    print *, "Failed to create BO list"
    call amdgpu_close_device(device)
    stop 1
  end if
  
  ! Setup command buffer
  cmd_buf%ib_cpu_ptr = cmd_buf%ib_buffer%cpu_ptr
  cmd_buf%ctx_id = ctx_id  ! Use our created context
  cmd_buf%bo_list_handle = bo_list_handle  ! Use our BO list
  
  ! Write shader to command buffer
  call amdgpu_write_compute_shader(cmd_buf, shader_data, int(size(shader_data) * 4, c_int64_t))
  
  ! Debug: print first few dwords of command buffer
  block
    integer(c_int32_t), pointer :: ib_data(:)
    integer :: i
    call c_f_pointer(cmd_buf%ib_cpu_ptr, ib_data, [size(shader_data)])
    print *, "Command buffer contents:"
    do i = 1, size(shader_data)
      print '(A,I0,A,Z8)', "  IB[", i-1, "] = 0x", ib_data(i)
    end do
  end block
  
  ! Submit command buffer
  print *, "Submitting command buffer..."
  status = amdgpu_submit_command_buffer(device, cmd_buf)
  if (status /= 0) then
    print *, "Failed to submit command buffer"
  else
    print *, "Command submitted successfully!"
    
    ! Wait for completion
    print *, "Waiting for completion..."
    status = amdgpu_wait_buffer_idle(device, cmd_buf%ib_buffer)
    
    ! Check first few values (they should be unchanged for NOP shader)
    print *, "First 5 buffer values after execution:"
    do i = 1, 5
      print '(A,I0,A,F10.2)', "  buffer[", i, "] = ", buffer_data(i)
    end do
  end if
  
  ! Cleanup
  if (bo_list_handle > 0) then
    call amdgpu_destroy_bo_list(device, bo_list_handle)
  end if
  if (ctx_id >= 0) then
    call amdgpu_destroy_context(device, ctx_id)
  end if
  call amdgpu_close_device(device)
  
  print *, "=== Test complete ==="
  
end program test_amdgpu_command