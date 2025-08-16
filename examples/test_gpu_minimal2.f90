subroutine test_gpu_minimal2() bind(C, name="test_gpu_minimal2")
  use iso_c_binding
  use gl_constants
  implicit none
  
  interface
    subroutine set_shader_source_from_c(shader) bind(C, name="set_shader_source_from_c")
      import :: c_int
      integer(c_int), value :: shader
    end subroutine set_shader_source_from_c
  end interface
  
  integer(c_int) :: shader, compile_status
  character(len=512, kind=c_char), target :: info_log
  integer(c_int) :: info_len
  
  print *, "=== Minimal GPU Test 2 ==="
  
  ! Create shader
  shader = glCreateShader(GL_COMPUTE_SHADER)
  print *, "Created shader:", shader
  
  ! Set source from C
  call set_shader_source_from_c(shader)
  
  ! Compile
  call glCompileShader(shader)
  
  ! Check status
  call glGetShaderiv(shader, GL_COMPILE_STATUS, compile_status)
  print *, "Compile status:", compile_status
  
  if (compile_status == 0) then
    call glGetShaderiv(shader, GL_INFO_LOG_LENGTH, info_len)
    call glGetShaderInfoLog(shader, min(info_len, 512), C_NULL_PTR, c_loc(info_log))
    print *, "Error:", trim(info_log)
  else
    print *, "Success!"
  end if
  
  call glDeleteShader(shader)
  
end subroutine test_gpu_minimal2