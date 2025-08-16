subroutine test_gpu_minimal() bind(C, name="test_gpu_minimal")
  use iso_c_binding
  use gl_constants
  implicit none
  
  integer(c_int) :: shader, compile_status
  character(len=256, kind=c_char), target :: shader_src
  character(len=512, kind=c_char), target :: info_log
  integer(c_int) :: info_len
  type(c_ptr), target :: src_ptr_array(1)
  
  print *, "=== Minimal GPU Test ==="
  
  ! Create minimal shader - use simple newline
  shader_src = "#version 430 core" // NEW_LINE('A') // &
               "layout(local_size_x = 64) in;" // NEW_LINE('A') // &
               "void main() {}" // NEW_LINE('A') // C_NULL_CHAR
  
  shader = glCreateShader(GL_COMPUTE_SHADER)
  print *, "Created shader:", shader
  
  ! Set source
  src_ptr_array(1) = c_loc(shader_src)
  call glShaderSource(shader, 1, c_loc(src_ptr_array), C_NULL_PTR)
  
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
  
end subroutine test_gpu_minimal