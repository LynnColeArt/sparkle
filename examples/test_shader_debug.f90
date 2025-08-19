program test_shader_debug
  use iso_c_binding
  use sporkle_fortran_shaders
  use gl_constants
  implicit none
  
  type(glsl_context) :: ctx
  integer :: status
  integer :: program
  character(len=512) :: shader_source
  character(len=1024), target :: info_log
  integer :: compile_status, link_status
  integer :: buffer
  integer, target :: test_data(1)
  
  print *, "=== Debug Shader Compilation ==="
  
  ! Initialize context
  status = glsl_init(ctx)
  if (status /= 0) then
    print *, "Failed to init GL"
    stop 1
  end if
  
  ! Simple test shader
  shader_source = &
    "#version 310 es" // NEW_LINE('A') // &
    "layout(local_size_x = 1) in;" // NEW_LINE('A') // &
    "layout(std430, binding = 0) buffer Out {" // NEW_LINE('A') // &
    "  uint data[];" // NEW_LINE('A') // &  
    "};" // NEW_LINE('A') // &
    "void main() {" // NEW_LINE('A') // &
    "  data[0] = 0x12345678u;" // NEW_LINE('A') // &
    "}"
    
  print *, "Shader source:"
  print *, trim(shader_source)
  
  ! Compile directly
  block
    interface
      function glsl_compile_compute_shader(source) result(program)
        character(len=*), intent(in) :: source
        integer :: program
      end function
    end interface
    
    program = glsl_compile_compute_shader(shader_source)
    print *, "Program ID:", program
    
    if (program /= 0) then
      print *, "✓ Compilation successful"
      
      ! Create a test buffer
      test_data(1) = int(z'BAD0BAD0')
      
      call glGenBuffers(1, buffer)
      call glBindBuffer(GL_SHADER_STORAGE_BUFFER, buffer)
      call glBufferData(GL_SHADER_STORAGE_BUFFER, int(4, c_size_t), &
                        c_loc(test_data), GL_DYNAMIC_COPY)
      
      print *, "Buffer ID:", buffer
      print *, "Initial value:", test_data(1)
      
      ! Dispatch
      call glUseProgram(program)
      call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, buffer)
      call glDispatchCompute(1, 1, 1)
      call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
      call glFinish()
      
      ! Read back
      call glGetBufferSubData(GL_SHADER_STORAGE_BUFFER, 0_c_intptr_t, &
                              int(4, c_size_t), c_loc(test_data))
      
      write(*, '(A,Z8.8)') "Result: 0x", test_data(1)
      
      if (test_data(1) == int(z'12345678')) then
        print *, "✓ SUCCESS - Shader executed!"
      else
        print *, "✗ FAIL - Shader didn't execute"
      end if
      
      call glDeleteBuffers(1, buffer)
      call glDeleteProgram(program)
    else
      print *, "✗ Compilation failed"
    end if
  end block
  
  call glsl_cleanup(ctx)
  
end program test_shader_debug