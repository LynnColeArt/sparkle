program test_gl_basic
  use iso_c_binding
  use gl_constants
  use sporkle_fortran_shaders
  implicit none
  
  type(glsl_context) :: ctx
  integer :: status
  integer :: buffer
  integer(c_int32_t), target :: test_data(4)
  integer(c_int32_t), target :: read_data(4)
  
  print *, "=== Basic GL Test ==="
  
  ! Initialize context
  status = glsl_init(ctx)
  if (status /= 0) then
    print *, "Failed to initialize GL context"
    stop 1
  end if
  print *, "✓ GL context created"
  
  ! Create and test buffer
  test_data = [1, 2, 3, 4]
  
  call glGenBuffers(1, buffer)
  print *, "Buffer ID:", buffer
  
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffer)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, int(16, c_size_t), &
                    c_loc(test_data), GL_DYNAMIC_COPY)
  
  ! Read back
  call glGetBufferSubData(GL_SHADER_STORAGE_BUFFER, 0_c_intptr_t, &
                          int(16, c_size_t), c_loc(read_data))
  
  print *, "Written:", test_data
  print *, "Read back:", read_data
  
  if (all(read_data == test_data)) then
    print *, "✓ Buffer read/write works!"
  else
    print *, "✗ Buffer read/write failed"
  end if
  
  ! Test simple shader
  block
    character(len=*), parameter :: simple_shader = &
      "#version 310 es" // NEW_LINE('A') // &
      "layout(local_size_x = 1) in;" // NEW_LINE('A') // &
      "layout(std430, binding = 0) buffer TestBuffer {" // NEW_LINE('A') // &
      "  uint data[];" // NEW_LINE('A') // &
      "};" // NEW_LINE('A') // &
      "void main() {" // NEW_LINE('A') // &
      "  data[gl_GlobalInvocationID.x] = 0xDEADBEEFu;" // NEW_LINE('A') // &
      "}"
    
    integer :: program
    
    ! Need to declare the function interface
    interface
      function glsl_compile_compute_shader(source) result(program)
        character(len=*), intent(in) :: source
        integer :: program
      end function
    end interface
    
    program = glsl_compile_compute_shader(simple_shader)
    if (program /= 0) then
      print *, "✓ Shader compiled successfully"
      
      ! Use it
      call glUseProgram(program)
      call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, buffer)
      call glDispatchCompute(4, 1, 1)
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
      call glFinish()
      
      ! Read back
      call glGetBufferSubData(GL_SHADER_STORAGE_BUFFER, 0_c_intptr_t, &
                              int(16, c_size_t), c_loc(read_data))
      
      print *, "After shader (hex):"
      print '(4(Z8.8,1X))', read_data
      
      if (all(read_data == int(z'DEADBEEF', c_int32_t))) then
        print *, "✓ Shader execution works!"
      else
        print *, "✗ Shader didn't execute"
      end if
      
      call glDeleteProgram(program)
    else
      print *, "✗ Shader compilation failed"
    end if
  end block
  
  ! Cleanup
  call glDeleteBuffers(1, buffer)
  call glsl_cleanup(ctx)
  
end program test_gl_basic