program test_amdgpu_direct_integration
  use iso_fortran_env, only: real32, int64
  use iso_c_binding
  use amdgpu_device_mod
  use sparkle_types
  implicit none
  
  type(amdgpu_compute_device), allocatable :: device
  type(sparkle_buffer) :: input_buffer, output_buffer
  integer(int64) :: buffer_size
  integer :: i
  real(real32), pointer :: input_data(:), output_data(:)
  
  print *, "=== Testing AMDGPU Direct Integration ==="
  print *, ""
  
  ! Create device for discrete GPU (card1)
  print *, "Creating AMDGPU device for discrete GPU..."
  device = create_amdgpu_device(1)
  
  if (.not. device%is_available) then
    print *, "❌ Failed to initialize AMDGPU device"
    stop 1
  end if
  
  print *, ""
  print *, "Device info:"
  print *, "  Name: ", trim(device%name)
  print *, "  ID: ", device%device_id
  print *, "  Memory BW: ", device%capabilities%memory_bandwidth_gbps, "GB/s"
  print *, ""
  
  ! Test buffer allocation
  buffer_size = 1024 * 1024 * 4  ! 4MB
  print *, "Allocating buffers..."
  
  input_buffer = device%allocate(buffer_size)
  output_buffer = device%allocate(buffer_size)
  
  if (.not. c_associated(input_buffer%data)) then
    print *, "❌ Failed to allocate input buffer"
    call device%cleanup()
    stop 1
  end if
  
  if (.not. c_associated(output_buffer%data)) then
    print *, "❌ Failed to allocate output buffer"
    call device%cleanup()
    stop 1
  end if
  
  print *, "✅ Allocated buffers:", buffer_size, "bytes each"
  
  ! Initialize input data
  call c_f_pointer(input_buffer%data, input_data, [buffer_size/4])
  do i = 1, size(input_data)
    input_data(i) = real(i, real32)
  end do
  
  ! Test memcpy
  print *, ""
  print *, "Testing memory copy..."
  if (device%memcpy(output_buffer, input_buffer, buffer_size) == 0) then
    print *, "✅ Memory copy successful"
    
    ! Verify
    call c_f_pointer(output_buffer%data, output_data, [buffer_size/4])
    if (abs(output_data(1) - 1.0) < 1e-6 .and. &
        abs(output_data(100) - 100.0) < 1e-6) then
      print *, "✅ Data verification passed"
    else
      print *, "❌ Data verification failed"
    end if
  else
    print *, "❌ Memory copy failed"
  end if
  
  ! Test kernel execution (placeholder)
  print *, ""
  print *, "Testing kernel execution..."
  block
    type(c_ptr) :: args(3)
    integer :: grid(3) = [256, 1, 1]
    integer :: block(3) = [64, 1, 1]
    integer(int64), target :: buffer_size_arg
    
    buffer_size_arg = buffer_size
    args(1) = input_buffer%data
    args(2) = output_buffer%data
    args(3) = c_loc(buffer_size_arg)
    
    if (device%execute("test_kernel", args, grid, block) == 0) then
      print *, "✅ Kernel execution command accepted"
    else
      print *, "❌ Kernel execution failed"
    end if
  end block
  
  ! Cleanup
  print *, ""
  print *, "Cleaning up..."
  call device%deallocate(input_buffer)
  call device%deallocate(output_buffer)
  call device%cleanup()
  
  print *, "✅ Test complete!"
  
end program test_amdgpu_direct_integration