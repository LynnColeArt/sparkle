program test_parser_v2_full
  use kinds
  use sporkle_shader_parser_v2
  use sporkle_fortran_params
  implicit none
  
  type(shader_kernel_v2) :: kernel
  character(len=:), allocatable :: glsl_source
  integer :: i
  
  print *, "=== Testing Full GLSL Generation ==="
  
  ! Test parsing with UNIFORM method
  print *, "Parsing with UNIFORM method..."
  kernel = parse_fortran_kernel_v2("examples/kernels_adaptive.f90", "scaled_add", PARAMS_UNIFORM)
  
  glsl_source = generate_glsl_v2(kernel)
  print *, "UNIFORM GLSL:"
  print *, trim(glsl_source)
  print *, ""
  
  ! Test with BUFFER method
  print *, "Parsing with BUFFER method..."
  kernel = parse_fortran_kernel_v2("examples/kernels_adaptive.f90", "scaled_add", PARAMS_BUFFER)
  
  glsl_source = generate_glsl_v2(kernel)
  print *, "BUFFER GLSL:"
  print *, trim(glsl_source)
  print *, ""
  
  ! Test with INLINE method
  print *, "Parsing with INLINE method..."
  kernel = parse_fortran_kernel_v2("examples/kernels_adaptive.f90", "scaled_add", PARAMS_INLINE)
  
  glsl_source = generate_glsl_v2(kernel)
  print *, "INLINE GLSL:"
  print *, trim(glsl_source)
  
end program test_parser_v2_full