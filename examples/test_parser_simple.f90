program test_parser_simple
  use sporkle_shader_parser_simple
  implicit none
  
  type(shader_kernel_simple) :: kernel
  
  print *, "=== Testing Simple Parser ==="
  print *, ""
  
  ! Test with scaled_add kernel
  kernel = parse_kernel_simple("examples/kernels_adaptive.f90", "scaled_add")
  call print_kernel_info(kernel)
  
  print *, ""
  print *, "=== Testing with simple_add ==="
  kernel = parse_kernel_simple("examples/test_simple_kernel.f90", "simple_add") 
  call print_kernel_info(kernel)
  
end program test_parser_simple