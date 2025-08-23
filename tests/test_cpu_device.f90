! Test CPU device functionality
program test_cpu_device
  use iso_c_binding
  use kinds
  use sporkle_types
  use cpu_device_module
  implicit none
  
  type(cpu_device) :: cpu_dev
  type(sporkle_buffer) :: buffer_a, buffer_b, buffer_c
  type(c_ptr) :: args(8)
  integer(i32) :: grid(3), block(3)
  integer :: status
  logical :: test_passed
  
  ! Test data
  real(sp), pointer :: A(:), B(:), C(:), result(:)
  integer, target :: m, n, k
  real(sp), target :: alpha, beta
  integer :: i
  
  print *, "=== CPU Device Test Suite ==="
  test_passed = .true.
  
  ! Test 1: Device creation
  print *, "Test 1: Creating CPU device..."
  cpu_dev = cpu_device(0)
  
  if (cpu_dev%device_type%value == 1) then  ! DEVICE_CPU
    print *, "✅ Device type correct"
  else
    print *, "❌ Wrong device type"
    test_passed = .false.
  end if
  
  if (allocated(cpu_dev%name)) then
    print *, "✅ Device name:", trim(cpu_dev%name)
  else
    print *, "❌ Device name not allocated"
    test_passed = .false.
  end if
  
  ! Test 2: Memory allocation
  print *, ""
  print *, "Test 2: Memory allocation..."
  buffer_a = cpu_dev%allocate(16_int64 * 4_int64)  ! 16 floats
  
  if (c_associated(buffer_a%data)) then
    print *, "✅ Buffer allocated"
    call c_f_pointer(buffer_a%data, A, [16])
    ! Initialize data
    do i = 1, 16
      A(i) = real(i, sp)
    end do
  else
    print *, "❌ Buffer allocation failed"
    test_passed = .false.
  end if
  
  ! Test 3: Memory copy
  print *, ""
  print *, "Test 3: Memory copy..."
  buffer_b = cpu_dev%allocate(16_int64 * 4_int64)
  
  status = cpu_dev%memcpy(buffer_b, buffer_a, 16_int64 * 4_int64)
  
  if (status == SPORKLE_SUCCESS) then
    call c_f_pointer(buffer_b%data, B, [16])
    if (all(B == A)) then
      print *, "✅ Memory copy successful"
    else
      print *, "❌ Memory copy data mismatch"
      test_passed = .false.
    end if
  else
    print *, "❌ Memory copy failed"
    test_passed = .false.
  end if
  
  ! Test 4: GEMM execution
  print *, ""
  print *, "Test 4: GEMM execution..."
  
  ! Setup 4x4 matrices
  m = 4
  n = 4
  k = 4
  alpha = 1.0
  beta = 0.0
  
  buffer_c = cpu_dev%allocate(16_int64 * 4_int64)
  call c_f_pointer(buffer_c%data, C, [16])
  C = 0.0
  
  ! Setup args for GEMM
  args(1) = buffer_a%data
  args(2) = buffer_b%data
  args(3) = buffer_c%data
  args(4) = c_loc(m)
  args(5) = c_loc(n)
  args(6) = c_loc(k)
  args(7) = c_loc(alpha)
  args(8) = c_loc(beta)
  
  grid = [1, 1, 1]
  block = [1, 1, 1]
  
  status = cpu_dev%execute("gemm", args, grid, block)
  
  if (status == SPORKLE_SUCCESS) then
    print *, "✅ GEMM executed successfully"
    print *, "   Result C(1:4) =", C(1:4)
  else
    print *, "❌ GEMM execution failed"
    test_passed = .false.
  end if
  
  ! Test 5: Unknown kernel
  print *, ""
  print *, "Test 5: Unknown kernel handling..."
  
  status = cpu_dev%execute("unknown_kernel", args, grid, block)
  
  if (status == SPORKLE_ERROR) then
    print *, "✅ Unknown kernel properly rejected"
  else
    print *, "❌ Unknown kernel not handled"
    test_passed = .false.
  end if
  
  ! Test 6: Synchronization
  print *, ""
  print *, "Test 6: Synchronization..."
  
  status = cpu_dev%synchronize()
  
  if (status == SPORKLE_SUCCESS) then
    print *, "✅ Synchronization successful"
  else
    print *, "❌ Synchronization failed"
    test_passed = .false.
  end if
  
  ! Test 7: Memory deallocation
  print *, ""
  print *, "Test 7: Memory deallocation..."
  
  call cpu_dev%deallocate(buffer_a)
  call cpu_dev%deallocate(buffer_b)
  call cpu_dev%deallocate(buffer_c)
  
  if (.not. c_associated(buffer_a%data)) then
    print *, "✅ Memory deallocated"
  else
    print *, "❌ Memory still allocated"
    test_passed = .false.
  end if
  
  ! Summary
  print *, ""
  print *, "=== Test Summary ==="
  if (test_passed) then
    print *, "✅ All tests passed!"
  else
    print *, "❌ Some tests failed"
    stop 1
  end if
  
end program test_cpu_device