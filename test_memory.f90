program test_memory
  use kinds, int64, real32, int8
  use iso_c_binding, only: c_ptr
  use sparkle_memory
  use sparkle_types
  use cpu_device_module
  use kinds
  implicit none
  
  type(memory_handle) :: h1, h2, h3
  type(memory_pool) :: pool
  logical :: success
  integer :: i
  real(sp), pointer :: data(:)
  type(c_ptr) :: temp_ptr
  
  print *, "=== Sparkle Memory Management Test ==="
  print *, ""
  
  ! Test 1: Basic host memory allocation
  print *, "Test 1: Basic host memory allocation"
  h1 = create_memory(1024_i64, tag="test_buffer")
  call memory_info(h1)
  
  if (h1%is_allocated) then
    print *, "✓ Successfully allocated 1KB of host memory"
  else
    print *, "✗ Failed to allocate host memory"
  end if
  print *, ""
  
  ! Test 2: Memory set and copy
  print *, "Test 2: Memory operations"
  h2 = create_memory(1024_i64, tag="copy_buffer")
  
  call memory_set(h1, int(42, int8))
  success = memory_copy(h2, h1, 1024_i64, MEM_HOST_TO_HOST)
  
  if (success) then
    print *, "✓ Memory copy succeeded"
  else
    print *, "✗ Memory copy failed"
  end if
  print *, ""
  
  ! Test 3: Memory pool
  print *, "Test 3: Memory pool management"
  call pool%init(10)
  
  ! Allocate several buffers
  do i = 1, 5
    h3 = pool%allocate(int(i * 256, int64), tag="pool_buffer_" // char(48 + i))
  end do
  
  call pool%report()
  print *, ""
  
  ! Test 4: Cleanup
  print *, "Test 4: Memory cleanup"
  call destroy_memory(h1)
  call destroy_memory(h2)
  call pool%cleanup()
  
  print *, "✓ All memory cleaned up"
  print *, ""
  print *, "=== Memory management tests complete ==="
  
contains

  ! Helper to convert int to string (simple version)
  function char(n) result(c)
    integer, intent(in) :: n
    character(1) :: c
    c = achar(n)
  end function char

end program test_memory
