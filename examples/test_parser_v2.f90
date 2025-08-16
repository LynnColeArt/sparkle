program test_parser_v2
  use iso_fortran_env
  use sparkle_shader_parser_v2
  implicit none
  
  type(shader_kernel_v2) :: kernel
  character(len=:), allocatable :: glsl_source
  
  print *, "=== Testing Fortran Shader Parser V2 ==="
  
  ! Test parsing a simple kernel
  print *, "Parsing scaled_add kernel..."
  kernel = parse_fortran_kernel_v2("examples/kernels_adaptive.f90", "scaled_add")
  
  print *, "Kernel name:", trim(kernel%name)
  print *, "Local size X:", kernel%local_size_x
  
  if (allocated(kernel%args)) then
    print *, "Number of args:", size(kernel%args)
  else
    print *, "No args allocated"
  end if
  
  if (allocated(kernel%params)) then
    print *, "Number of params:", size(kernel%params)
  else
    print *, "No params allocated"
  end if
  
  ! Generate GLSL
  print *, ""
  print *, "Generating GLSL..."
  glsl_source = generate_glsl_v2(kernel)
  
  if (allocated(glsl_source)) then
    print *, "GLSL source generated, length:", len(glsl_source)
    print *, "First 100 chars:"
    print *, glsl_source(1:min(100, len(glsl_source)))
  else
    print *, "Failed to generate GLSL"
  end if
  
  print *, ""
  print *, "Test complete."
  
end program test_parser_v2