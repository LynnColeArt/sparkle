program test_fortran_shader
  use iso_c_binding
  use sparkle_types
  use sparkle_fortran_shaders
  use gl_constants
  implicit none
  
  integer, parameter :: N = 64
  type(glsl_context) :: ctx
  integer :: output_buffer
  integer(c_int32_t), allocatable, target :: output_data(:)
  type(c_ptr) :: buffers(1)
  integer :: status, i
  logical :: all_correct
  
  print *, "=== Testing Fortran Shader System ==="
  
  ! Set environment for hardware acceleration
  ! call setenv("LIBGL_ALWAYS_SOFTWARE", "0", 1)
  
  ! Initialize GLSL context for buffer creation
  status = glsl_init(ctx)
  if (status /= 0) then
    print *, "Failed to initialize GLSL context"
    stop 1
  end if
  
  ! Create output buffer
  allocate(output_data(N))
  output_data = int(z'BAD0BAD0', c_int32_t)
  
  call glGenBuffers(1, output_buffer)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, output_buffer)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, int(N * 4, c_size_t), &
                    c_loc(output_data), GL_DYNAMIC_COPY)
  
  ! Setup buffer array - pass the GL buffer ID directly
  buffers(1) = transfer(output_buffer, c_null_ptr)
  
  print *, "Output buffer GL ID:", output_buffer
  
  ! Check GL renderer info
  block
    interface
      function glGetString(name) bind(C, name="glGetString")
        import :: c_int, c_ptr
        integer(c_int), value :: name
        type(c_ptr) :: glGetString
      end function
    end interface
    integer, parameter :: GL_RENDERER = int(z'1F01', c_int)
    integer, parameter :: GL_VERSION = int(z'1F02', c_int)
    character(len=256) :: renderer_str
    type(c_ptr) :: renderer_ptr
    integer :: i
    character(c_char), pointer :: char_array(:)
    
    renderer_ptr = glGetString(GL_RENDERER)
    if (c_associated(renderer_ptr)) then
      call c_f_pointer(renderer_ptr, char_array, [256])
      renderer_str = ""
      do i = 1, 256
        if (char_array(i) == c_null_char) exit
        renderer_str(i:i) = char_array(i)
      end do
      print *, "GL Renderer:", trim(renderer_str)
    end if
  end block
  
  ! Compile and dispatch our Fortran kernel
  call sporkle_compile_and_dispatch( &
    kernel_file = "examples/kernels.f90", &
    kernel_name = "store_deadbeef", &
    global_size = N, &
    buffers = buffers, &
    status = status &
  )
  
  if (status /= 0) then
    print *, "Failed to compile and dispatch kernel"
    call glsl_cleanup(ctx)
    stop 1
  end if
  
  ! Read back results - try mapping first
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, output_buffer)
  
  block
    interface
      function glMapBufferRange(target, offset, length, access) bind(C, name="glMapBufferRange")
        import :: c_int, c_intptr_t, c_size_t, c_ptr
        integer(c_int), value :: target
        integer(c_intptr_t), value :: offset
        integer(c_size_t), value :: length
        integer(c_int), value :: access
        type(c_ptr) :: glMapBufferRange
      end function
      
      function glUnmapBuffer(target) bind(C, name="glUnmapBuffer")
        import :: c_int
        integer(c_int), value :: target
        integer(c_int) :: glUnmapBuffer
      end function
    end interface
    
    integer, parameter :: GL_MAP_READ_BIT = int(z'0001', c_int)
    type(c_ptr) :: ptr
    integer(c_int32_t), pointer :: mapped(:)
    
    ptr = glMapBufferRange(GL_SHADER_STORAGE_BUFFER, 0_c_intptr_t, &
                           int(N * 4, c_size_t), GL_MAP_READ_BIT)
    if (c_associated(ptr)) then
      call c_f_pointer(ptr, mapped, [N])
      output_data = mapped
      if (glUnmapBuffer(GL_SHADER_STORAGE_BUFFER) == 0) then
        print *, "Warning: unmap failed"
      end if
    else
      ! Fall back to glGetBufferSubData
      call glGetBufferSubData(GL_SHADER_STORAGE_BUFFER, 0_c_intptr_t, &
                              int(N * 4, c_size_t), c_loc(output_data))
    end if
  end block
  
  ! Verify results
  print *, "First 8 values:"
  do i = 1, min(8, N)
    write(*, '(A,I0,A,Z8.8)') "  output[", i-1, "] = 0x", output_data(i)
  end do
  
  ! Check all values
  all_correct = .true.
  do i = 1, N
    if (output_data(i) /= int(z'DEADBEEF', c_int32_t)) then
      all_correct = .false.
      exit
    end if
  end do
  
  if (all_correct) then
    print *, "SUCCESS: All values are 0xDEADBEEF!"
    print *, ""
    print *, "🎉 Fortran shaders are working! 🎉"
  else
    print *, "FAILURE: Some values are incorrect"
  end if
  
  ! Cleanup
  call glDeleteBuffers(1, output_buffer)
  call glsl_cleanup(ctx)
  deallocate(output_data)
  
end program test_fortran_shader