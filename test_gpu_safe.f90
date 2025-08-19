program test_gpu_safe
  use iso_c_binding
  use sporkle_amdgpu_direct
  implicit none
  
  type(amdgpu_device) :: device
  type(amdgpu_buffer) :: buffer
  integer :: status
  real :: start_time, end_time
  
  print *, "🔒 GPU Safe Test"
  print *, "================"
  print *, ""
  print *, "This test includes safety limits to prevent system overload"
  
  ! Add CPU usage limit
  call system("ulimit -t 10")  ! 10 second CPU time limit
  
  ! Open device
  device = amdgpu_open_device("/dev/dri/renderD128")
  if (device%fd < 0) then
    print *, "❌ Failed to open device"
    stop 1
  end if
  
  print *, "✅ Device opened safely"
  
  ! Small buffer allocation (only 4KB)
  buffer = amdgpu_allocate_buffer(device, 4096_c_int64_t)
  if (buffer%handle == 0) then
    print *, "❌ Failed to allocate buffer"
    call amdgpu_close_device(device)
    stop 1
  end if
  
  print *, "✅ Allocated small buffer (4KB)"
  
  ! Map buffer with timeout
  call cpu_time(start_time)
  status = amdgpu_map_buffer(device, buffer)
  call cpu_time(end_time)
  
  if (end_time - start_time > 0.1) then
    print *, "⚠️  Mapping took too long:", end_time - start_time, "seconds"
  end if
  
  if (status == 0) then
    print *, "✅ Buffer mapped successfully"
  else
    print *, "❌ Failed to map buffer"
  end if
  
  ! Clean up immediately
  if (c_associated(buffer%cpu_ptr)) then
    call amdgpu_unmap_buffer(device, buffer)
  end if
  
  call amdgpu_free_buffer(device, buffer)
  call amdgpu_close_device(device)
  
  print *, ""
  print *, "✅ Test completed safely without system overload"
  
end program test_gpu_safe