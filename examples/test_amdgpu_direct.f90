program test_amdgpu_direct
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding
  use sparkle_amdgpu_direct
  implicit none
  
  type(amdgpu_device) :: device
  type(amdgpu_buffer) :: buffer
  integer :: status
  integer(int64) :: buffer_size
  real(real32), pointer :: data(:)
  integer :: i
  
  print *, "🚀 Sparkle Direct AMDGPU Test"
  print *, "============================="
  print *, ""
  print *, "Going straight to the metal!"
  print *, ""
  
  ! Open the GPU device
  device = amdgpu_open_device()
  
  if (.not. device%is_open) then
    print *, "❌ Failed to open GPU device"
    print *, "   Make sure you have an AMD GPU and permissions for /dev/dri/card0"
    stop
  end if
  
  ! Allocate a buffer on the GPU
  buffer_size = 1024 * 1024 * 4  ! 4MB
  print *, ""
  print *, "Allocating GPU buffer..."
  buffer = amdgpu_allocate_buffer(device, int(buffer_size, c_int64_t))
  
  if (buffer%handle == 0) then
    print *, "❌ Failed to allocate GPU buffer"
    call amdgpu_close_device(device)
    stop
  end if
  
  ! Map the buffer for CPU access
  print *, ""
  print *, "Mapping buffer for CPU access..."
  status = amdgpu_map_buffer(device, buffer)
  
  if (status /= 0) then
    print *, "❌ Failed to map buffer"
    call amdgpu_close_device(device)
    stop
  end if
  
  ! Access the buffer from CPU
  print *, ""
  print *, "Writing test pattern to GPU memory..."
  call c_f_pointer(buffer%cpu_ptr, data, [buffer_size/4])
  
  ! Write a test pattern
  do i = 1, size(data)
    data(i) = real(i, real32) * 3.14159
  end do
  
  ! Verify we can read it back
  print *, ""
  print *, "Verifying GPU memory access..."
  print '(A,F10.6)', "  First value: ", data(1)
  print '(A,F10.6)', "  Last value: ", data(size(data))
  print '(A,F10.6)', "  Middle value: ", data(size(data)/2)
  
  ! Check pattern
  if (abs(data(1) - 3.14159) < 0.0001 .and. &
      abs(data(size(data)) - real(size(data), real32) * 3.14159) < 0.1) then
    print *, ""
    print *, "✅ GPU memory access verified!"
    print *, "   We are talking directly to the AMD GPU!"
  else
    print *, "❌ Memory pattern verification failed"
  end if
  
  ! TODO: Next steps
  print *, ""
  print *, "Next steps:"
  print *, "  1. Command buffer submission"
  print *, "  2. Shader upload and execution"
  print *, "  3. DMA transfers"
  print *, "  4. Compute dispatch"
  
  ! Cleanup
  print *, ""
  print *, "Cleaning up..."
  call amdgpu_close_device(device)
  
  print *, ""
  print *, "The Sparkle Way: No ROCm needed! 🎉"
  
end program test_amdgpu_direct