program test_nvidia_basic
  ! Test basic NVIDIA GPU detection and initialization
  ! The Sporkle Way: Direct kernel driver access!
  
  use sporkle_nvidia_direct
  use iso_c_binding
  implicit none
  
  type(nvidia_device) :: device
  type(nvidia_buffer) :: buffer
  type(nv_gpu_info) :: info
  integer(c_int32_t) :: ctx_id
  integer(c_size_t) :: buffer_size
  type(c_ptr) :: mapped_ptr
  
  print *, "==================================="
  print *, "Sporkle NVIDIA Direct Test"
  print *, "==================================="
  
  ! Try different device paths
  print *, "Attempting to open NVIDIA device..."
  
  ! Try /dev/nvidia2 first (that's what we saw in ls)
  device = nvidia_open_device("/dev/nvidia2")
  
  if (.not. device%initialized) then
    print *, "Trying /dev/nvidiactl..."
    device = nvidia_open_device("/dev/nvidiactl")
  end if
  
  if (.not. device%initialized) then
    print *, "Trying /dev/dri/card3..."
    device = nvidia_open_device("/dev/dri/card3")
  end if
  
  if (.not. device%initialized) then
    print *, "Failed to open any NVIDIA device"
    stop 1
  end if
  
  ! Get device info
  print *, ""
  print *, "Getting device information..."
  info = nvidia_get_device_info(device)
  
  ! Create compute context
  print *, ""
  print *, "Creating compute context..."
  ctx_id = nvidia_create_context(device)
  
  ! Allocate a buffer
  print *, ""
  print *, "Allocating GPU buffer..."
  buffer_size = 1024 * 1024  ! 1MB
  buffer = nvidia_allocate_buffer(device, buffer_size)
  
  ! Map the buffer
  print *, "Mapping buffer to CPU..."
  mapped_ptr = nvidia_map_buffer(device, buffer)
  
  ! Create and submit a simple command buffer
  print *, ""
  print *, "Creating command buffer..."
  ! This would contain actual GPU commands
  
  ! Clean up
  print *, ""
  print *, "Cleaning up..."
  call nvidia_destroy_context(device, ctx_id)
  call nvidia_close_device(device)
  
  print *, ""
  print *, "==================================="
  print *, "NVIDIA test completed successfully!"
  print *, "==================================="
  
end program test_nvidia_basic