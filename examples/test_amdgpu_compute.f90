program test_amdgpu_compute
  use iso_c_binding
  use sporkle_types
  use sporkle_amdgpu_direct
  use sporkle_amdgpu_shaders
  use sporkle_amdgpu_memory
  implicit none
  
  type(amdgpu_device) :: device
  type(amdgpu_buffer) :: a_buffer, b_buffer, c_buffer, args_buffer
  type(amdgpu_shader) :: vector_add_shader
  integer :: status, i, ctx_id, errors
  integer(c_size_t) :: buffer_size, args_size
  integer :: num_elements
  real(c_float), allocatable, target :: host_a(:), host_b(:), host_c(:)
  real :: expected
  
  ! Kernel arguments structure
  type, bind(C) :: kernel_args_t
    integer(c_int64_t) :: a_addr
    integer(c_int64_t) :: b_addr
    integer(c_int64_t) :: c_addr
    integer(c_int32_t) :: n
  end type kernel_args_t
  type(kernel_args_t), target :: args
  
  print *, "=== Testing AMDGPU Compute Shader Execution ==="
  print *, ""
  
  ! Open device
  device = amdgpu_open_device("/dev/dri/card0")
  if (device%fd < 0) then
    print *, "Failed to open device"
    stop 1
  end if
  
  ! Create context
  ctx_id = amdgpu_create_context(device)
  if (ctx_id == 0) then
    print *, "Failed to create context"
    stop 1
  end if
  
  print *, "Device initialized successfully"
  print *, ""
  
  ! Create shader
  vector_add_shader = create_vector_add_shader()
  print *, "Shader created:"
  print *, "  Code size:", vector_add_shader%code_size, "bytes"
  print *, "  SGPRs:", vector_add_shader%num_sgprs
  print *, "  VGPRs:", vector_add_shader%num_vgprs
  print *, ""
  
  ! Set up test data
  num_elements = 1024
  buffer_size = int(num_elements * 4, c_size_t)  ! 4 bytes per float
  
  allocate(host_a(num_elements))
  allocate(host_b(num_elements))
  allocate(host_c(num_elements))
  
  ! Initialize test data
  do i = 1, num_elements
    host_a(i) = real(i)
    host_b(i) = real(i * 2)
    host_c(i) = 0.0
  end do
  
  print *, "Allocating GPU buffers..."
  
  ! Allocate GPU buffers
  a_buffer = amdgpu_allocate_buffer(device, buffer_size)
  if (a_buffer%handle == 0) then
    print *, "Failed to allocate buffer A"
    goto 999
  end if
  
  b_buffer = amdgpu_allocate_buffer(device, buffer_size)
  if (b_buffer%handle == 0) then
    print *, "Failed to allocate buffer B"
    goto 998
  end if
  
  c_buffer = amdgpu_allocate_buffer(device, buffer_size)
  if (c_buffer%handle == 0) then
    print *, "Failed to allocate buffer C"
    goto 997
  end if
  
  ! Allocate kernel arguments buffer - use at least 4KB to avoid issues
  args_size = int(sizeof(args), c_size_t)
  args_buffer = amdgpu_allocate_buffer(device, max(args_size, 4096_c_size_t))
  if (args_buffer%handle == 0) then
    print *, "Failed to allocate args buffer"
    goto 996
  end if
  
  print *, "Buffers allocated (handles):"
  print *, "  A: handle =", a_buffer%handle
  print *, "  B: handle =", b_buffer%handle  
  print *, "  C: handle =", c_buffer%handle
  print *, "  Args: handle =", args_buffer%handle
  print *, ""
  
  ! Map buffers to GPU virtual address space
  status = amdgpu_map_va(device, a_buffer, int(z'1000000', c_int64_t))
  if (status /= 0) print *, "Warning: VA mapping for A failed"
  
  status = amdgpu_map_va(device, b_buffer, int(z'2000000', c_int64_t))
  if (status /= 0) print *, "Warning: VA mapping for B failed"
  
  status = amdgpu_map_va(device, c_buffer, int(z'3000000', c_int64_t))
  if (status /= 0) print *, "Warning: VA mapping for C failed"
  
  ! Try a higher address for args buffer to avoid conflicts
  status = amdgpu_map_va(device, args_buffer, int(z'10000000', c_int64_t))
  if (status /= 0) print *, "Warning: VA mapping for args failed"
  
  print *, "VA mapped addresses:"
  print *, "  A:", a_buffer%va_addr
  print *, "  B:", b_buffer%va_addr
  print *, "  C:", c_buffer%va_addr
  print *, "  Args:", args_buffer%va_addr
  print *, ""
  
  ! Set up kernel arguments - use VA addresses if mapped
  if (a_buffer%is_va_mapped) then
    args%a_addr = a_buffer%va_addr
  else
    args%a_addr = a_buffer%gpu_addr
  end if
  
  if (b_buffer%is_va_mapped) then
    args%b_addr = b_buffer%va_addr
  else
    args%b_addr = b_buffer%gpu_addr
  end if
  
  if (c_buffer%is_va_mapped) then
    args%c_addr = c_buffer%va_addr
  else
    args%c_addr = c_buffer%gpu_addr
  end if
  
  args%n = num_elements
  
  print *, "Kernel arguments structure:"
  print '(A,Z16)', "  a_addr: 0x", args%a_addr
  print '(A,Z16)', "  b_addr: 0x", args%b_addr
  print '(A,Z16)', "  c_addr: 0x", args%c_addr
  print *, "  n:", args%n
  print *, ""
  
  ! Copy data to GPU
  print *, "Copying data to GPU..."
  
  ! Map buffers if needed
  if (.not. c_associated(a_buffer%cpu_ptr)) then
    status = amdgpu_map_buffer(device, a_buffer)
  end if
  if (.not. c_associated(b_buffer%cpu_ptr)) then
    status = amdgpu_map_buffer(device, b_buffer)
  end if
  if (.not. c_associated(c_buffer%cpu_ptr)) then
    status = amdgpu_map_buffer(device, c_buffer)
  end if
  if (.not. c_associated(args_buffer%cpu_ptr)) then
    status = amdgpu_map_buffer(device, args_buffer)
  end if
  
  ! Write test data
  status = amdgpu_write_buffer(device, a_buffer, c_loc(host_a), buffer_size)
  if (status /= 0) print *, "Failed to write buffer A"
  
  status = amdgpu_write_buffer(device, b_buffer, c_loc(host_b), buffer_size)
  if (status /= 0) print *, "Failed to write buffer B"
  
  ! Write kernel arguments
  status = amdgpu_write_buffer(device, args_buffer, c_loc(args), args_size)
  if (status /= 0) print *, "Failed to write args"
  
  ! Launch compute shader
  print *, ""
  print *, "Launching compute shader..."
  print *, "  Workgroups: 16 x 1 x 1"
  print *, "  Threads per group: 64 x 1 x 1"
  print *, "  Total threads:", 16 * 64, "(covers", num_elements, "elements)"
  
  status = dispatch_compute_shader(device, vector_add_shader, args_buffer, &
                                  16, 1, 1,    & ! workgroups
                                  64, 1, 1)      ! threads per group
  
  if (status /= 0) then
    print *, "ERROR: Compute shader dispatch failed with status:", status
  else
    print *, "Compute shader completed successfully!"
  end if
  
  ! Copy results back
  print *, ""
  print *, "Copying results from GPU..."
  
  status = amdgpu_read_buffer(device, c_buffer, c_loc(host_c), buffer_size)
  if (status /= 0) then
    print *, "Failed to read results"
  end if
  
  ! Verify results
  print *, ""
  print *, "Verification:"
  print *, "First 10 results:"
  do i = 1, min(10, num_elements)
    expected = host_a(i) + host_b(i)
    print '(A,I4,A,F8.2,A,F8.2,A,F8.2,A,F8.2,A)', &
      "  [", i, "] ", host_a(i), " + ", host_b(i), " = ", host_c(i), &
      " (expected: ", expected, ")"
    
    if (abs(host_c(i) - expected) > 0.001) then
      print *, "    ⚠️  MISMATCH!"
    end if
  end do
  
  ! Check all results
  errors = 0
  do i = 1, num_elements
    if (abs(host_c(i) - (host_a(i) + host_b(i))) > 0.001) then
      errors = errors + 1
    end if
  end do
  
  if (errors == 0) then
    print *, ""
    print *, "✅ ALL RESULTS CORRECT! GPU computation verified!"
  else
    print *, ""
    print *, "❌ Found", errors, "errors out of", num_elements, "elements"
  end if
  
  ! Cleanup
  ! In real implementation, would free buffers here
996 continue
997 continue  
998 continue
999 continue
  
  deallocate(host_a, host_b, host_c)
  
  ! Device cleanup handled by kernel when process exits
  ! In production, would properly clean up context and buffers
  
  print *, ""
  print *, "Test completed!"
  
end program test_amdgpu_compute