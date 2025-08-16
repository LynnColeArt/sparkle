program test_simple_glsl
  use iso_c_binding
  use gl_constants
  implicit none
  
  integer(c_int) :: shader, compile_status
  character(len=1024), target :: info_log
  integer(c_int), target :: info_len, actual_len
  
  ! Minimal GLSL code
  character(len=*), parameter :: glsl_code = &
    '#version 430 core'//NEW_LINE('A')// &
    'layout(local_size_x = 64) in;'//NEW_LINE('A')// &
    'void main() {}'
  
  ! C string with null terminator
  character(len=:,kind=c_char), allocatable, target :: c_source
  type(c_ptr) :: source_ptr
  type(c_ptr), target :: source_ptr_array(1)
  integer(c_int), target :: source_len_array(1)
  
  print *, "Creating compute shader..."
  shader = glCreateShader(GL_COMPUTE_SHADER)
  print *, "Shader ID:", shader
  
  ! Prepare source
  c_source = glsl_code // c_null_char
  source_ptr = c_loc(c_source)
  source_ptr_array(1) = source_ptr
  source_len_array(1) = len(glsl_code)
  
  print *, "Setting shader source..."
  print *, "Source length:", len(glsl_code)
  print *, "First 50 chars: '", glsl_code(1:min(50,len(glsl_code))), "'"
  
  call glShaderSource(shader, 1, c_loc(source_ptr_array), c_loc(source_len_array))
  
  print *, "Compiling shader..."
  call glCompileShader(shader)
  
  call glGetShaderiv(shader, GL_COMPILE_STATUS, compile_status)
  print *, "Compile status:", compile_status
  
  if (compile_status == 0) then
    call glGetShaderiv(shader, GL_INFO_LOG_LENGTH, info_len)
    print *, "Info log length:", info_len
    call glGetShaderInfoLog(shader, min(info_len, 1024), c_loc(actual_len), c_loc(info_log))
    print *, "ERROR:", info_log(1:actual_len)
  else
    print *, "SUCCESS!"
  end if
  
  call glDeleteShader(shader)
  
end program test_simple_glsl