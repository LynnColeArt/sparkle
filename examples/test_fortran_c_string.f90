program test_fortran_c_string
  use iso_c_binding
  implicit none
  
  interface
    subroutine test_c_string(str) bind(C, name="test_c_string")
      import :: c_ptr
      type(c_ptr), value :: str
    end subroutine test_c_string
  end interface
  
  character(len=100, kind=c_char), target :: shader_str
  character(len=100), parameter :: test_str = "#version 430 core" // C_NULL_CHAR
  
  ! Method 1: Direct string literal with C_NULL_CHAR
  shader_str = "#version 430 core" // C_NULL_CHAR
  call test_c_string(c_loc(shader_str))
  
  ! Method 2: Using parameter
  shader_str = test_str
  call test_c_string(c_loc(shader_str))
  
end program test_fortran_c_string