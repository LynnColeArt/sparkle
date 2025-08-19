program test_multiline_parser
  ! Test the parser with multi-line signatures
  use sporkle_shader_parser_v2
  use sporkle_fortran_params
  implicit none
  
  type(shader_kernel_v2) :: kernel
  character(len=:), allocatable :: glsl_source
  integer :: i
  
  print *, "Testing Multi-line Signature Parser"
  print *, "==================================="
  print *, ""
  
  ! Parse the multiline kernel
  kernel = parse_fortran_kernel_v2("examples/test_multiline_kernel.f90", "multiline_test", PARAMS_BUFFER)
  
  print *, "Kernel name: ", trim(kernel%name)
  print *, "Total arguments: ", size(kernel%args)
  print *, "Scalar parameters: ", size(kernel%params)
  print *, ""
  
  ! Show all arguments
  print *, "All arguments:"
  if (allocated(kernel%args)) then
    do i = 1, size(kernel%args)
      print '(A,I2,A,A,A,L1)', &
        "  ", i, ": ", trim(kernel%args(i)%name), &
        " (scalar=", kernel%args(i)%is_scalar
    end do
  end if
  print *, ""
  
  ! Show scalar parameters  
  print *, "Scalar parameters for adaptive passing:"
  if (allocated(kernel%params)) then
    do i = 1, size(kernel%params)
      print '(A,I2,A,A)', &
        "  ", i, ": ", trim(kernel%params(i)%name)
    end do
  end if
  print *, ""
  
  ! Generate GLSL to verify it works
  print *, "Generated GLSL (first 20 lines):"
  glsl_source = generate_glsl_v2(kernel)
  call print_first_lines(glsl_source, 20)
  
contains

  subroutine print_first_lines(text, n_lines)
    character(len=*), intent(in) :: text
    integer, intent(in) :: n_lines
    integer :: i, line_count, newline_pos
    
    i = 1
    line_count = 0
    
    do while (i <= len_trim(text) .and. line_count < n_lines)
      newline_pos = index(text(i:), NEW_LINE('A'))
      if (newline_pos > 0) then
        print *, text(i:i+newline_pos-2)
        i = i + newline_pos
      else
        print *, text(i:)
        exit
      end if
      line_count = line_count + 1
    end do
  end subroutine

end program test_multiline_parser