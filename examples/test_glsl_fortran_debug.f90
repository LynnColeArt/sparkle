subroutine test_glsl_fortran_debug() bind(C, name="test_glsl_fortran_debug")
  use iso_c_binding
  use gl_constants
  implicit none
  
  interface
    subroutine test_shader_from_fortran() bind(C, name="test_shader_from_fortran")
    end subroutine test_shader_from_fortran
  end interface
  
  ! Call C function to test
  call test_shader_from_fortran()
  
end subroutine test_glsl_fortran_debug