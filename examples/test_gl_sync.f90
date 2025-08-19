program test_gl_sync
  use iso_c_binding
  use sporkle_fortran_shaders
  use gl_constants
  implicit none
  
  type(glsl_context) :: ctx
  integer :: status
  integer :: shader, program, buffer
  integer(c_int32_t), target :: data(1)
  character(len=512) :: shader_source
  type(c_ptr) :: source_ptr
  character(len=:,kind=c_char), allocatable, target :: c_source
  integer :: compile_status
  
  print *, "=== GL Sync Test ==="
  
  ! Initialize
  status = glsl_init(ctx)
  if (status /= 0) stop "Failed to init GL"
  
  ! Simple shader that writes a constant
  shader_source = &
    "#version 310 es" // NEW_LINE('A') // &
    "layout(local_size_x = 1) in;" // NEW_LINE('A') // &
    "layout(std430, binding = 0) buffer Data {" // NEW_LINE('A') // &
    "  uint value;" // NEW_LINE('A') // &
    "};" // NEW_LINE('A') // &
    "void main() {" // NEW_LINE('A') // &
    "  value = 0x12345678u;" // NEW_LINE('A') // &
    "}"
  
  ! Convert to C string
  c_source = trim(shader_source) // c_null_char
  source_ptr = c_loc(c_source)
  
  ! Create shader
  shader = glCreateShader(GL_COMPUTE_SHADER)
  if (shader == 0) stop "Failed to create shader"
  
  ! Compile
  call glShaderSource(shader, 1, source_ptr, c_null_ptr)
  call glCompileShader(shader)
  call glGetShaderiv(shader, GL_COMPILE_STATUS, compile_status)
  if (compile_status /= GL_TRUE) stop "Shader compilation failed"
  
  ! Create program
  program = glCreateProgram()
  call glAttachShader(program, shader)
  call glLinkProgram(program)
  
  ! Create buffer with initial value
  data(1) = int(z'BAD0BAD0')
  call glGenBuffers(1, buffer)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffer)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, int(4, c_size_t), &
                    c_loc(data), GL_DYNAMIC_COPY)
  
  print *, "Initial value:", data(1)
  
  ! Bind and dispatch
  call glUseProgram(program)
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, buffer)
  
  ! Add multiple sync points
  call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
  call glDispatchCompute(1, 1, 1)
  call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
  call glFinish()
  
  ! Try mapping the buffer instead of glGetBufferSubData
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
    integer(c_int32_t), pointer :: mapped_data
    
    ptr = glMapBufferRange(GL_SHADER_STORAGE_BUFFER, 0_c_intptr_t, &
                           int(4, c_size_t), GL_MAP_READ_BIT)
    if (c_associated(ptr)) then
      call c_f_pointer(ptr, mapped_data)
      write(*, '(A,Z8.8)') "Mapped value: 0x", mapped_data
      data(1) = mapped_data
      if (glUnmapBuffer(GL_SHADER_STORAGE_BUFFER) == 0) then
        print *, "Warning: glUnmapBuffer failed"
      end if
    else
      print *, "Failed to map buffer"
    end if
  end block
  
  ! Also try regular read
  call glGetBufferSubData(GL_SHADER_STORAGE_BUFFER, 0_c_intptr_t, &
                          int(4, c_size_t), c_loc(data))
  write(*, '(A,Z8.8)') "glGetBufferSubData value: 0x", data(1)
  
  if (data(1) == int(z'12345678')) then
    print *, "✓ SUCCESS!"
  else
    print *, "✗ FAILED"
  end if
  
  ! Cleanup
  call glDeleteBuffers(1, buffer)
  call glDeleteProgram(program)
  call glDeleteShader(shader)
  call glsl_cleanup(ctx)
  
end program test_gl_sync