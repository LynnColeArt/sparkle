program test_simple_write
  use iso_c_binding
  use sporkle_types
  use sporkle_amdgpu_direct
  use sporkle_amdgpu_memory
  implicit none
  
  type(amdgpu_device) :: device
  type(amdgpu_buffer) :: test_buffer
  integer :: status, i
  integer(c_size_t) :: buffer_size
  real(c_float), allocatable, target :: host_data(:)
  integer :: num_elements
  
  print *, "=== Testing Simple GPU Write/Read ==="
  print *, ""
  
  ! Open device
  device = amdgpu_open_device("/dev/dri/card0")
  if (device%fd < 0) then
    print *, "Failed to open device"
    stop 1
  end if
  
  print *, "Device opened successfully"
  
  ! Set up test data
  num_elements = 16
  buffer_size = int(num_elements * 4, c_size_t)  ! 4 bytes per float
  
  allocate(host_data(num_elements))
  
  ! Initialize with test pattern
  do i = 1, num_elements
    host_data(i) = real(i) * 10.0
  end do
  
  print *, "Test data:"
  print '(8F8.1)', host_data
  
  ! Allocate GPU buffer
  test_buffer = amdgpu_allocate_buffer(device, buffer_size)
  if (test_buffer%handle == 0) then
    print *, "Failed to allocate buffer"
    stop 1
  end if
  
  print *, ""
  print *, "Allocated buffer:"
  print *, "  Handle:", test_buffer%handle
  print *, "  Size:", test_buffer%size
  
  ! Map buffer for CPU access
  status = amdgpu_map_buffer(device, test_buffer)
  if (status /= 0) then
    print *, "Failed to map buffer"
    stop 1
  end if
  
  print *, "  Mapped to CPU"
  
  ! Write data to GPU
  print *, ""
  print *, "Writing data to GPU..."
  status = amdgpu_write_buffer(device, test_buffer, c_loc(host_data), buffer_size)
  if (status /= 0) then
    print *, "Failed to write buffer"
    stop 1
  end if
  
  ! Clear host data to verify read
  host_data = 0.0
  print *, "Cleared host data:", host_data(1:4), "..."
  
  ! Read data back from GPU
  print *, ""
  print *, "Reading data from GPU..."
  status = amdgpu_read_buffer(device, test_buffer, c_loc(host_data), buffer_size)
  if (status /= 0) then
    print *, "Failed to read buffer"
    stop 1
  end if
  
  print *, "Read back:"
  print '(8F8.1)', host_data
  
  ! Verify
  print *, ""
  do i = 1, num_elements
    if (abs(host_data(i) - real(i) * 10.0) > 0.001) then
      print *, "ERROR: Mismatch at element", i
      stop 1
    end if
  end do
  
  print *, "✅ All values correct! GPU memory operations working!"
  
  deallocate(host_data)
  
end program test_simple_write