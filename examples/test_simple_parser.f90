program test_simple_parser
  use kinds
  use sporkle_shader_parser_v2
  use sporkle_fortran_params
  implicit none
  
  type(shader_kernel_v2) :: kernel
  character(len=:), allocatable :: glsl_source
  
  print *, "=== Testing Simple Kernel ==="
  kernel = parse_fortran_kernel_v2("examples/test_simple_kernel.f90", "simple_add", PARAMS_BUFFER)
  print *, "Kernel body: '", trim(kernel%body), "'"
  glsl_source = generate_glsl_v2(kernel)
  print *, "Generated GLSL:"
  print *, trim(glsl_source)
  
  print *, ""
  print *, "=== Testing Scaled Add Kernel - BUFFER Method ==="
  kernel = parse_fortran_kernel_v2("examples/kernels_adaptive.f90", "scaled_add", PARAMS_BUFFER)
  print *, "Kernel body: '", trim(kernel%body), "'"
  glsl_source = generate_glsl_v2(kernel)
  print *, "Generated GLSL:"
  print *, trim(glsl_source)
  
  print *, ""
  print *, "=== Testing Scaled Add Kernel - UNIFORM Method ==="
  kernel = parse_fortran_kernel_v2("examples/kernels_adaptive.f90", "scaled_add", PARAMS_UNIFORM)
  glsl_source = generate_glsl_v2(kernel)
  print *, "Generated GLSL:"
  print *, trim(glsl_source)
  
  print *, ""
  print *, "=== Testing Scaled Add Kernel - INLINE Method ==="
  kernel = parse_fortran_kernel_v2("examples/kernels_adaptive.f90", "scaled_add", PARAMS_INLINE)
  glsl_source = generate_glsl_v2(kernel)
  print *, "Generated GLSL:"
  print *, trim(glsl_source)
  
end program test_simple_parser