program test_fortran_direct
  use iso_c_binding
  use sporkle_fortran_shaders
  use gl_constants
  implicit none
  
  type(glsl_context) :: ctx
  integer :: status
  integer :: buffer
  integer(c_int32_t), target :: data(4)
  character(len=512) :: shader_source
  integer :: program
  type(c_ptr) :: source_ptr
  character(len=:,kind=c_char), allocatable, target :: c_source
  
  print *, "=== Testing Direct GLSL in Fortran Shader Module ==="
  
  ! Initialize
  status = glsl_init(ctx)
  if (status /= 0) stop "Failed to init"
  
  ! Create test buffer
  data = int(z'BAD0BAD0')
  call glGenBuffers(1, buffer)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffer)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, int(16, c_size_t), &
                    c_loc(data), GL_DYNAMIC_COPY)
  
  ! Simple shader
  shader_source = &
    "#version 310 es" // NEW_LINE('A') // &
    "layout(local_size_x = 4) in;" // NEW_LINE('A') // &
    "layout(std430, binding = 0) buffer Out {" // NEW_LINE('A') // &
    "  uint data[];" // NEW_LINE('A') // &
    "};" // NEW_LINE('A') // &
    "void main() {" // NEW_LINE('A') // &
    "  data[gl_GlobalInvocationID.x] = 0xDEADBEEFu;" // NEW_LINE('A') // &
    "}"
  
  ! Use the internal function
  block
    interface
      function glsl_compile_compute_shader(source) result(program)
        character(len=*), intent(in) :: source
        integer :: program
      end function
    end interface
  end block
  
  ! Can't call internal function, so inline the compilation
  c_source = trim(shader_source) // c_null_char
  source_ptr = c_loc(c_source)
  
  ! Create shader
  block
    integer :: shader, compile_status, link_status
    character(len=1024), target :: info_log
    
    shader = glCreateShader(GL_COMPUTE_SHADER)
    if (shader == 0) stop "Failed to create shader"
    
    call glShaderSource(shader, 1, source_ptr, c_null_ptr)
    call glCompileShader(shader)
    call glGetShaderiv(shader, GL_COMPILE_STATUS, compile_status)
    
    if (compile_status /= GL_TRUE) then
      call glGetShaderInfoLog(shader, 1024, c_null_ptr, c_loc(info_log))
      print *, "Compilation failed:", trim(info_log)
      stop
    end if
    
    program = glCreateProgram()
    call glAttachShader(program, shader)
    call glLinkProgram(program)
    call glGetProgramiv(program, GL_LINK_STATUS, link_status)
    
    if (link_status /= GL_TRUE) then
      print *, "Link failed"
      stop
    end if
    
    print *, "✓ Shader compiled"
    
    ! Use it
    call glUseProgram(program)
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, buffer)
    
    ! Dispatch
    call glDispatchCompute(1, 1, 1)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    call glFinish()
    
    ! Check GL error
    block
      interface
        function glGetError() bind(C, name="glGetError")
          import :: c_int
          integer(c_int) :: glGetError
        end function
      end interface
      integer(c_int) :: error
      error = glGetError()
      if (error /= 0) then
        write(*, '(A,Z8.8)') "GL Error: 0x", error
      end if
    end block
    
    ! Read back with mapping
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
                             int(16, c_size_t), GL_MAP_READ_BIT)
      if (c_associated(ptr)) then
        call c_f_pointer(ptr, mapped, [4])
        print *, "Mapped values:"
        write(*, '(4(Z8.8,1X))') mapped
        data = mapped
        if (glUnmapBuffer(GL_SHADER_STORAGE_BUFFER) == 0) then
          print *, "Warning: unmap failed"
        end if
      else
        print *, "Failed to map"
      end if
    end block
    
    ! Also try regular read
    call glGetBufferSubData(GL_SHADER_STORAGE_BUFFER, 0_c_intptr_t, &
                            int(16, c_size_t), c_loc(data))
    print *, "glGetBufferSubData values:"
    write(*, '(4(Z8.8,1X))') data
    
    if (all(data == int(z'DEADBEEF'))) then
      print *, "✓ SUCCESS!"
    else
      print *, "✗ FAILED"
    end if
    
    call glDeleteProgram(program)
    call glDeleteShader(shader)
  end block
  
  call glDeleteBuffers(1, buffer)
  call glsl_cleanup(ctx)
  
end program test_fortran_direct