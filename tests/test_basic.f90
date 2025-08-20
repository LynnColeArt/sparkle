program test_basic
  use sparkle_types
  use cpu_device_module
  use iso_c_binding, only: c_associated
  use kinds
  implicit none
  
  type(cpu_device) :: cpu
  type(sparkle_buffer) :: buffer1, buffer2
  integer :: status
  
  print *, "=== Sparkle Basic Test ==="
  print *, "Testing the foundation for the people's AI infrastructure"
  print *, ""
  
  ! Create CPU device
  cpu = cpu_device()
  
  print *, "Device Info:"
  print *, "  Name: ", cpu%name
  print *, "  Type: CPU"
  print *, "  Compute Units: ", cpu%capabilities%compute_units
  print *, "  Instruction Set: ", cpu%capabilities%instruction_set
  print *, "  Supports Float64: ", cpu%capabilities%supports_float64
  print *, ""
  
  ! Test memory allocation
  print *, "Testing memory allocation..."
  buffer1 = cpu%allocate(1024_i64)
  
  if (c_associated(buffer1%data)) then
    print *, "  ✓ Allocated 1KB buffer"
    print *, "  Size: ", buffer1%size_bytes, " bytes"
    print *, "  Owner: Device ", buffer1%owning_device
  else
    print *, "  ✗ Allocation failed!"
    stop 1
  end if
  
  ! Test memory copy
  print *, ""
  print *, "Testing memory operations..."
  buffer2 = cpu%allocate(1024_i64)
  status = cpu%memcpy(buffer2, buffer1, 512_i64)
  
  if (status == SPARKLE_SUCCESS) then
    print *, "  ✓ Memory copy successful"
  else
    print *, "  ✗ Memory copy failed!"
  end if
  
  ! Cleanup
  call cpu%deallocate(buffer1)
  call cpu%deallocate(buffer2)
  print *, "  ✓ Memory freed"
  
  print *, ""
  print *, "=== Basic test complete ==="
  print *, "Ready to democratize AI compute!"
  
end program test_basic
