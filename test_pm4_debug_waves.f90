program test_pm4_debug_waves
  ! Debug why waves aren't launching
  use iso_c_binding
  use kinds
  use pm4_device_discovery
  use sporkle_pm4_compute
  use sporkle_amdgpu_direct
  use sporkle_rdna3_shaders
  implicit none
  
  type(pm4_context) :: ctx
  type(amdgpu_buffer) :: test_buffer
  integer :: status, i
  integer(i64) :: shader_addr
  real(sp), pointer :: data(:)
  
  print *, "=== PM4 Wave Debug Test ==="
  print *, ""
  
  ! Initialize with debug output
  print *, "1. Initializing PM4 context..."
  status = pm4_init_context(ctx)
  if (status /= 0) then
    print *, "❌ Failed to initialize PM4 context"
    stop 1
  end if
  
  print *, "✅ PM4 context initialized"
  print '(A,Z8)', "   Device ID: 0x", ctx%device_info%device_id
  print '(A,A)', "   Device name: ", trim(ctx%device_info%name)
  print *, ""
  
  ! Allocate test buffer
  print *, "2. Allocating test buffer..."
  test_buffer = amdgpu_allocate_buffer(ctx%device, 4096_i64, AMDGPU_GEM_DOMAIN_GTT)
  if (test_buffer%handle == 0) then
    print *, "❌ Failed to allocate buffer"
    call pm4_cleanup_context(ctx)
    stop 1
  end if
  
  print *, "✅ Buffer allocated"
  print '(A,Z16)', "   Handle: 0x", test_buffer%handle
  print '(A,I0,A)', "   Size: ", test_buffer%size, " bytes"
  
  ! Map buffer
  print *, ""
  print *, "3. Mapping buffer..."
  status = amdgpu_map_buffer(ctx%device, test_buffer)
  if (status /= 0) then
    print *, "❌ Failed to map buffer"
    call pm4_cleanup_context(ctx)
    stop 1
  end if
  
  print *, "✅ Buffer mapped"
  print '(A,Z16)', "   CPU ptr: 0x", transfer(test_buffer%cpu_ptr, 0_i64)
  
  ! Map to GPU VA
  print *, ""
  print *, "4. Mapping to GPU VA..."
  test_buffer%va_addr = int(z'100000', i64)  ! 1MB
  status = amdgpu_map_va(ctx%device, test_buffer, test_buffer%va_addr)
  if (status /= 0) then
    print *, "❌ Failed to map VA"
    call pm4_cleanup_context(ctx)
    stop 1
  end if
  
  print *, "✅ VA mapped"
  print '(A,Z16)', "   GPU VA: 0x", test_buffer%va_addr
  
  ! Initialize buffer
  call c_f_pointer(test_buffer%cpu_ptr, data, [1024])
  data = 0.0_sp
  data(1) = 123.456_sp  ! Marker value
  
  ! Compile shader
  print *, ""
  print *, "5. Compiling shader..."
  shader_addr = pm4_compile_shader("copy_shader", "")
  if (shader_addr == 0) then
    print *, "❌ Failed to compile shader"
    call pm4_cleanup_context(ctx)
    stop 1
  end if
  
  print *, "✅ Shader compiled"
  print '(A,Z16)', "   Shader VA: 0x", shader_addr
  
  ! Execute minimal compute
  print *, ""
  print *, "6. Executing compute..."
  block
    type(amdgpu_buffer) :: buffers(1)
    buffers(1) = test_buffer
    
    ! Just 1 workgroup, 1 thread
    status = pm4_execute_compute(ctx, shader_addr, buffers, 1, 1, 1)
  end block
  
  if (status /= 0) then
    print *, "❌ Compute execution failed"
  else
    print *, "✅ Compute submitted"
    
    ! Check if data changed
    print *, ""
    print *, "7. Checking results..."
    print '(A,F10.3)', "   data[1] = ", data(1)
    print '(A,F10.3)', "   data[2] = ", data(2)
    
    if (data(2) /= 0.0_sp) then
      print *, "✅ Shader wrote data!"
    else
      print *, "❌ No data written - waves didn't execute"
    end if
  end if
  
  ! Let's also check what packets were generated
  print *, ""
  print *, "8. PM4 packet analysis:"
  print *, "   Check with: sudo umr -r *.*.umr_pm4_stream_decode"
  print *, "   Or: sudo umr -r gfx1036.*.mmCP_HQD_ACTIVE"
  
  ! Cleanup
  print *, ""
  print *, "Cleaning up..."
  call pm4_cleanup_context(ctx)
  
  print *, ""
  print *, "Debug hints:"
  print *, "- If waves aren't launching, check compute mode initialization"
  print *, "- May need MEC (Micro Engine Compute) setup"
  print *, "- Check if shader ISA matches GPU architecture"
  print *, "- Verify ring buffer is actually being consumed"
  
end program test_pm4_debug_waves