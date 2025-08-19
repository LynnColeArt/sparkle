program test_gpu_conv_simple
  ! Simplest possible test - just parse and generate GLSL
  use kinds
  use sporkle_shader_parser_v2
  use sporkle_fortran_params
  implicit none
  
  type(shader_kernel_v2) :: kernel
  character(len=:), allocatable :: glsl_code
  integer :: i
  
  print *, "=== GPU Convolution GLSL Generation Test ==="
  print *, ""
  
  ! Parse conv2d_direct kernel
  print *, "Parsing conv2d_direct kernel..."
  kernel = parse_fortran_kernel_v2("examples/kernels_convolution.f90", &
                                   "conv2d_direct", PARAMS_BUFFER)
  
  print *, "Kernel name: ", trim(kernel%name)
  print *, "Local size: ", kernel%local_size_x
  print *, "Param method: ", kernel%param_method
  
  if (allocated(kernel%args)) then
    print *, "Arguments:"
    do i = 1, size(kernel%args)
      print *, "  ", i, ": ", trim(kernel%args(i)%name), &
               " (", merge("scalar", "array ", kernel%args(i)%is_scalar), ")"
    end do
  end if
  
  if (allocated(kernel%params)) then
    print *, "Parameters:"
    do i = 1, size(kernel%params)
      print *, "  ", i, ": ", trim(kernel%params(i)%name)
    end do
  end if
  
  ! Generate GLSL
  print *, ""
  print *, "Generating GLSL..."
  glsl_code = generate_glsl_v2(kernel)
  
  print *, "Generated ", len(glsl_code), " characters of GLSL"
  print *, ""
  print *, "==== GLSL CODE ===="
  print *, glsl_code
  print *, "==================="
  
  ! Show what would happen with real execution
  print *, ""
  print *, "To execute on GPU:"
  print *, "1. Compile this shader with glCompileShader"
  print *, "2. Create buffers for input (32x32x3), weights (64x3x3x3), output (32x32x64)"
  print *, "3. Set parameters via SSBO"
  print *, "4. Dispatch with glDispatchCompute(32*32, 1, 1)"
  print *, "5. Read back results"
  
end program test_gpu_conv_simple