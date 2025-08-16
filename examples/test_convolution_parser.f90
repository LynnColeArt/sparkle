program test_convolution_parser
  ! Test parsing the actual convolution kernels
  use sparkle_shader_parser_v2
  use sparkle_fortran_params
  implicit none
  
  type(shader_kernel_v2) :: kernel
  character(len=:), allocatable :: glsl_source
  integer :: i
  
  print *, "Testing Convolution Kernel Parsing"
  print *, "=================================="
  print *, ""
  
  ! Test im2col kernel
  print *, "=== im2col_nhwc Kernel ==="
  kernel = parse_fortran_kernel_v2("examples/kernels_convolution.f90", "im2col_nhwc", PARAMS_BUFFER)
  
  print *, "Kernel name: ", trim(kernel%name)
  print *, "Total arguments: ", size(kernel%args)
  if (allocated(kernel%params)) then
    print *, "Scalar parameters: ", size(kernel%params)
    ! Show first few scalar parameters
    do i = 1, min(8, size(kernel%params))
      print '(A,A)', "  ", trim(kernel%params(i)%name)
    end do
    if (size(kernel%params) > 8) print *, "  ... and ", size(kernel%params) - 8, " more"
  else
    print *, "No scalar parameters found!"
  end if
  print *, ""
  
  ! Test gemm_tiled kernel 
  print *, "=== gemm_tiled Kernel ==="
  kernel = parse_fortran_kernel_v2("examples/kernels_convolution_annotated.f90", "gemm_tiled", PARAMS_BUFFER)
  
  print *, "Kernel name: ", trim(kernel%name)  
  print *, "Total arguments: ", size(kernel%args)
  if (allocated(kernel%params)) then
    print *, "Scalar parameters: ", size(kernel%params)
    ! Show all scalar parameters for this smaller kernel
    do i = 1, size(kernel%params)
      print '(A,A)', "  ", trim(kernel%params(i)%name)
    end do
  else
    print *, "No scalar parameters found!"
  end if
  print *, ""
  
  ! Generate GLSL for gemm_tiled to verify
  print *, "Generated GLSL for gemm_tiled (first 25 lines):"
  glsl_source = generate_glsl_v2(kernel)
  call print_first_lines(glsl_source, 25)
  
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

end program test_convolution_parser