program test_parser_body
  use kinds
  use sporkle_shader_parser_v2
  implicit none
  
  type(shader_kernel_v2) :: kernel
  
  print *, "=== Testing Kernel Body Parsing ==="
  
  kernel = parse_fortran_kernel_v2("examples/kernels_adaptive.f90", "scaled_add")
  
  print *, "Kernel body:"
  print *, "------------"
  print *, trim(kernel%body)
  print *, "------------"
  print *, "Body length:", len_trim(kernel%body)
  
end program test_parser_body