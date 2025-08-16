program test_convolution_profiles
  ! Profile convolution kernels arithmetic intensity and parameter methods
  use iso_fortran_env
  use sparkle_shader_parser_v2
  use sparkle_fortran_params
  implicit none
  
  type(shader_kernel_v2) :: kernel_im2col, kernel_gemm, kernel_direct
  
  ! Test parameters
  integer, parameter :: batch_size = 1
  integer, parameter :: height = 224
  integer, parameter :: width = 224
  integer, parameter :: channels = 3
  integer, parameter :: kernel_h = 3
  integer, parameter :: kernel_w = 3
  integer, parameter :: stride = 1
  integer, parameter :: pad = 1
  integer, parameter :: output_h = 224
  integer, parameter :: output_w = 224
  
  integer :: work_size_im2col, work_size_gemm, work_size_direct
  real :: intensity_im2col, intensity_gemm, intensity_direct
  
  print *, "========================================"
  print *, "Convolution Kernel Profiling"
  print *, "========================================"
  print *, ""
  
  ! Parse kernels
  print *, "Parsing convolution kernels..."
  kernel_im2col = parse_fortran_kernel_v2("examples/kernels_convolution.f90", &
                                          "im2col_nhwc", PARAMS_BUFFER)
  kernel_gemm = parse_fortran_kernel_v2("examples/kernels_convolution_annotated.f90", &
                                        "gemm_tiled", PARAMS_BUFFER)
  kernel_direct = parse_fortran_kernel_v2("examples/kernels_convolution.f90", &
                                          "conv2d_direct", PARAMS_BUFFER)
  
  ! Calculate work sizes
  work_size_im2col = batch_size * output_h * output_w * channels * kernel_h * kernel_w
  work_size_gemm = (channels * kernel_h * kernel_w) * (output_h * output_w)
  work_size_direct = batch_size * output_h * output_w
  
  print *, ""
  print *, "Configuration:"
  print '(A,I4,A,I4,A,I4,A,I4)', "  Input: ", batch_size, "x", height, "x", width, "x", channels
  print '(A,I4,A,I4)', "  Kernel: ", kernel_h, "x", kernel_w
  print '(A,I4,A,I4)', "  Output: ", output_h, "x", output_w
  print *, ""
  
  ! Profile im2col kernel
  print *, "========== im2col_nhwc Kernel =========="
  print *, "Kernel: ", trim(kernel_im2col%name)
  print *, "Work size: ", work_size_im2col
  if (allocated(kernel_im2col%params)) then
    print *, "Scalar parameters: ", size(kernel_im2col%params)
  end if
  intensity_im2col = estimate_arithmetic_intensity("im2col", work_size_im2col)
  print '(A,F8.2)', "  Arithmetic intensity: ", intensity_im2col
  print *, "  Recommended method: ", recommended_param_method(size(kernel_im2col%params))
  print *, "  Device routing: ", recommended_device(intensity_im2col)
  print *, ""
  
  ! Profile GEMM kernel
  print *, "========== gemm_tiled Kernel =========="
  print *, "Kernel: ", trim(kernel_gemm%name)
  print *, "Work size: ", work_size_gemm
  if (allocated(kernel_gemm%params)) then
    print *, "Scalar parameters: ", size(kernel_gemm%params)
  end if
  intensity_gemm = estimate_arithmetic_intensity("gemm", work_size_gemm)
  print '(A,F8.2)', "  Arithmetic intensity: ", intensity_gemm
  print *, "  Recommended method: ", recommended_param_method(size(kernel_gemm%params))
  print *, "  Device routing: ", recommended_device(intensity_gemm)
  print *, ""
  
  ! Profile direct convolution
  print *, "========== conv2d_direct Kernel =========="
  print *, "Kernel: ", trim(kernel_direct%name)
  print *, "Work size: ", work_size_direct
  if (allocated(kernel_direct%params)) then
    print *, "Scalar parameters: ", size(kernel_direct%params)
  end if
  intensity_direct = estimate_arithmetic_intensity("conv", work_size_direct)
  print '(A,F8.2)', "  Arithmetic intensity: ", intensity_direct
  print *, "  Recommended method: ", recommended_param_method(size(kernel_direct%params))
  print *, "  Device routing: ", recommended_device(intensity_direct)
  print *, ""
  
  ! Summary recommendations
  print *, "========================================"
  print *, "Optimization Recommendations"
  print *, "========================================"
  print *, ""
  print *, "1. Parameter Passing:"
  print *, "   - im2col: ", recommended_param_method(size(kernel_im2col%params)), &
           " (", size(kernel_im2col%params), " scalars)"
  print *, "   - GEMM: ", recommended_param_method(size(kernel_gemm%params)), &
           " (", size(kernel_gemm%params), " scalars)"
  print *, "   - Direct: ", recommended_param_method(size(kernel_direct%params)), &
           " (", size(kernel_direct%params), " scalars)"
  print *, ""
  print *, "2. Device Routing:"
  print *, "   - im2col: ", recommended_device(intensity_im2col)
  print *, "   - GEMM: ", recommended_device(intensity_gemm)
  print *, "   - Direct: ", recommended_device(intensity_direct)
  print *, ""
  print *, "3. Local Size Recommendations:"
  print *, "   - Memory-bound kernels: 64-128 threads"
  print *, "   - Compute-bound kernels: 256-512 threads"
  print *, ""
  
contains

  function estimate_arithmetic_intensity(kernel_type, work_size) result(intensity)
    character(len=*), intent(in) :: kernel_type
    integer, intent(in) :: work_size
    real :: intensity
    
    integer :: flops, bytes_accessed
    
    select case (kernel_type)
    case ("gemm")
      ! GEMM: 2*M*N*K FLOPs, roughly 2*work_size for simplified estimate
      flops = 2 * work_size
      bytes_accessed = work_size * 4 * 3  ! A, B, C matrices
      
    case ("im2col")
      ! im2col: Mostly memory movement
      flops = work_size  ! One op per output element
      bytes_accessed = work_size * 4 * 2  ! Read input, write output
      
    case ("conv")
      ! Direct convolution: K*K multiplies per output
      flops = work_size * kernel_h * kernel_w
      bytes_accessed = work_size * 4 * 3  ! Input, kernel, output
      
    case default
      flops = work_size * 10
      bytes_accessed = work_size * 4 * 3
    end select
    
    intensity = real(flops) / real(bytes_accessed)
  end function estimate_arithmetic_intensity
  
  function recommended_param_method(num_params) result(method)
    integer, intent(in) :: num_params
    character(len=20) :: method
    
    if (num_params <= 4) then
      method = "UNIFORM"
    else if (num_params <= 16) then
      method = "BUFFER"
    else
      method = "BUFFER (consider INLINE)"
    end if
  end function recommended_param_method
  
  function recommended_device(intensity) result(device)
    real, intent(in) :: intensity
    character(len=20) :: device
    
    if (intensity < 10.0) then
      device = "iGPU (memory-bound)"
    else if (intensity < 50.0) then
      device = "iGPU or dGPU"
    else
      device = "dGPU (compute-bound)"
    end if
  end function recommended_device

end program test_convolution_profiles