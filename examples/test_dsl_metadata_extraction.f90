program test_dsl_metadata_extraction
  use kinds
  use sporkle_shader_parser_v2
  use sporkle_fortran_params
  implicit none
  
  type(shader_kernel_v2) :: kernel
  integer :: i
  
  print *, "=== Testing DSL Metadata Extraction ==="
  print *, ""
  
  ! Parse kernel to extract metadata
  kernel = parse_fortran_kernel_v2("examples/convolution_kernels.f90", "conv2d_direct", PARAMS_BUFFER)
  
  print *, "Kernel Metadata:"
  print *, "  Name: ", trim(kernel%name)
  print *, "  Local size: [", kernel%local_size_x, ",", kernel%local_size_y, ",", kernel%local_size_z, "]"
  print *, ""
  
  print *, "Arguments:"
  if (allocated(kernel%args)) then
    do i = 1, size(kernel%args)
      print '(A,I2,A,A,A,A,A,A,A,I2)', &
        "  Arg ", i, ": ", trim(kernel%args(i)%name), &
        " (", trim(kernel%args(i)%type_str), ", ", trim(kernel%args(i)%intent), &
        ") binding=", kernel%args(i)%binding_slot
    end do
  end if
  print *, ""
  
  print *, "Scalar Parameters (for GPU parameter buffer):"
  if (allocated(kernel%params)) then
    do i = 1, size(kernel%params)
      print '(A,I2,A,A,A,A,A,I2)', &
        "  Param ", i, ": ", trim(kernel%params(i)%name), &
        " (", trim(kernel%params(i)%type_str), &
        ") index=", kernel%params(i)%param_index
    end do
  end if
  print *, ""
  
  print *, "This metadata can be used to:"
  print *, "  1. Configure buffer bindings for a pre-written GLSL shader"
  print *, "  2. Set up the parameter buffer with correct types and order"
  print *, "  3. Determine the dispatch size based on kernel requirements"
  print *, "  4. Validate that inputs match the kernel's expected signature"
  print *, ""
  
  print *, "Example parameter buffer layout for GPU:"
  print *, "  struct ParamBuffer {"
  if (allocated(kernel%params)) then
    do i = 1, size(kernel%params)
      select case(trim(kernel%params(i)%type_str))
      case("integer(i32)")
        print *, "    uint ", trim(kernel%params(i)%name), ";"
      case("real(sp)")
        print *, "    float ", trim(kernel%params(i)%name), ";"
      case("real(dp)")
        print *, "    double ", trim(kernel%params(i)%name), ";"
      end select
    end do
  end if
  print *, "  };"
  
end program test_dsl_metadata_extraction