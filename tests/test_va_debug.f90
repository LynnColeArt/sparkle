program test_va_debug
  use iso_c_binding
  use sporkle_amdgpu_direct
  use sporkle_gpu_va_allocator
  implicit none
  
  type(amdgpu_device) :: device
  type(amdgpu_buffer) :: buffer
  integer :: status
  integer(c_int64_t) :: va_addr
  
  print *, "=== Testing VA Mapping Errors ==="
  
  ! Open device
  device = amdgpu_open_device("/dev/dri/renderD128")  ! dGPU
  if (device%fd < 0) then
    print *, "Failed to open device"
    stop 1
  end if
  
  ! Initialize VA allocator  
  call gpu_va_init()
  
  ! Allocate buffer
  buffer = amdgpu_allocate_buffer(device, 4096_c_int64_t)
  if (buffer%handle == 0) then
    print *, "Failed to allocate buffer"
    stop 1
  end if
  
  ! Map buffer for CPU access
  status = amdgpu_map_buffer(device, buffer)
  if (status /= 0) then
    print *, "Failed to map buffer for CPU"
    stop 1
  end if
  
  ! Allocate VA address
  va_addr = gpu_va_allocate(buffer%size)
  if (va_addr == 0) then
    print *, "Failed to allocate VA"
    stop 1
  end if
  
  ! Map to GPU VA - this is where errors likely occur
  print *, "Attempting VA mapping..."
  print '(A,Z16)', "Buffer handle: 0x", buffer%handle
  print '(A,Z16)', "VA address: 0x", va_addr
  print '(A,I0)', "Buffer size: ", buffer%size
  
  status = amdgpu_map_va(device, buffer, va_addr)
  if (status /= 0) then
    print *, "VA mapping failed with status:", status
  else
    print *, "VA mapping succeeded!"
    call amdgpu_unmap_va(device, buffer)
  end if
  
  call amdgpu_close_device(device)
end program