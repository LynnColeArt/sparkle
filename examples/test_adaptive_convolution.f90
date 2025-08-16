program test_adaptive_convolution
  ! Test the adaptive convolution orchestrator
  use iso_fortran_env
  use sparkle_shader_parser_v2
  use sparkle_fortran_params
  implicit none
  
  type(shader_kernel_v2) :: kernel
  character(len=:), allocatable :: glsl_source
  integer :: i
  
  print *, "========================================================"
  print *, "Testing Adaptive Convolution Kernel Parsing"
  print *, "========================================================"
  print *, ""
  
  ! Test 1: Parse simple conv kernel first
  print *, "=== Simple Conv Kernel ==="
  kernel = parse_fortran_kernel_v2("examples/test_conv_simple.f90", "conv_simple", PARAMS_BUFFER)
  
  print *, "Kernel: ", trim(kernel%name)
  print *, "Total arguments: ", size(kernel%args)
  print *, "Scalar parameters: ", size(kernel%params)
  print *, ""
  
  print *, "Arguments:"
  if (allocated(kernel%args)) then
    do i = 1, size(kernel%args)
      print '(A,I2,A,A,A,A,A,A,L1)', &
        "  ", i, ": ", trim(kernel%args(i)%name), &
        " (", trim(kernel%args(i)%type_str), &
        ", ", trim(kernel%args(i)%intent), &
        ", scalar=", kernel%args(i)%is_scalar
    end do
  end if
  print *, ""
  
  print *, "Scalar parameters for adaptive passing:"
  if (allocated(kernel%params)) then
    do i = 1, size(kernel%params)
      print '(A,I2,A,A,A,A)', &
        "  ", i, ": ", trim(kernel%params(i)%name), &
        " (", trim(kernel%params(i)%type_str), ")"
    end do
  end if
  print *, ""
  
  ! Generate GLSL for all three methods
  print *, "=== BUFFER Method GLSL ==="
  kernel%param_method = PARAMS_BUFFER
  glsl_source = generate_glsl_v2(kernel)
  print *, trim(glsl_source)
  print *, ""
  
  print *, "=== UNIFORM Method GLSL (first few lines) ==="
  kernel%param_method = PARAMS_UNIFORM
  glsl_source = generate_glsl_v2(kernel)
  call print_first_lines(glsl_source, 15)
  print *, ""
  
  ! Test 2: Direct convolution kernel
  print *, "========================================================"
  print *, "=== Direct Convolution Kernel ==="
  kernel = parse_fortran_kernel_v2("examples/kernels_convolution.f90", "conv2d_direct", PARAMS_BUFFER)
  
  print *, "Scalar parameters: ", size(kernel%params)
  if (allocated(kernel%params)) then
    do i = 1, min(5, size(kernel%params))
      print '(A,A)', "  ", trim(kernel%params(i)%name)
    end do
    if (size(kernel%params) > 5) print *, "  ... and ", size(kernel%params) - 5, " more"
  end if
  
contains

  subroutine print_first_lines(text, n_lines)
    character(len=*), intent(in) :: text
    integer, intent(in) :: n_lines
    integer :: i, line_count, pos, newline_pos
    
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

end program test_adaptive_convolution