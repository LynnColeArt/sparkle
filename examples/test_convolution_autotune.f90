program test_convolution_autotune
  ! Autotune convolution kernels with real GPU execution
  use kinds
  use iso_c_binding  
  use gl_constants
  use sporkle_shader_parser_v2
  use sporkle_gpu_executor
  use sporkle_gpu_benchmark, only: benchmark_kernel, gpu_benchmark_param_methods, &
                                  measure_arithmetic_intensity, profile_kernel, kernel_profile
  use sporkle_fortran_params
  implicit none
  
  type(shader_kernel_v2) :: kernel_im2col, kernel_gemm, kernel_direct
  type(kernel_profile) :: profile_im2col, profile_gemm, profile_direct
  type(gpu_executor) :: exec
  
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
  
  real(sp), allocatable :: params_im2col(:), params_gemm(:), params_direct(:)
  integer :: work_size_im2col, work_size_gemm, work_size_direct
  type(c_ptr) :: window
  integer :: ctx
  
  print *, "========================================"
  print *, "Convolution Kernel Autotuning"
  print *, "========================================"
  print *, ""
  
  ! Initialize OpenGL context
  call initialize_gl_context(window, ctx)
  if (ctx == 0) then
    print *, "Failed to create OpenGL context"
    stop
  end if
  
  ! Parse kernels
  print *, "Parsing convolution kernels..."
  kernel_im2col = parse_fortran_kernel_v2("examples/kernels_convolution.f90", &
                                          "im2col_nhwc", PARAMS_BUFFER)
  kernel_gemm = parse_fortran_kernel_v2("examples/kernels_convolution_annotated.f90", &
                                        "gemm_tiled", PARAMS_BUFFER)
  kernel_direct = parse_fortran_kernel_v2("examples/kernels_convolution.f90", &
                                          "conv2d_direct", PARAMS_BUFFER)
  
  ! Prepare parameters for im2col
  allocate(params_im2col(13))
  params_im2col = [real(batch_size, real32), real(height, real32), real(width, real32), real(channels, real32), &
                   real(kernel_h, real32), real(kernel_w, real32), real(stride, real32), real(stride, real32), &
                   real(pad, real32), real(pad, real32), real(output_h, real32), real(output_w, real32), 0.0_real32]
  work_size_im2col = batch_size * output_h * output_w * channels * kernel_h * kernel_w
  
  ! Prepare parameters for GEMM
  allocate(params_gemm(10))
  params_gemm = [0.0, 0.0, &  ! idx, idy set by GPU
                 real(channels * kernel_h * kernel_w), &  ! M
                 real(output_h * output_w), &              ! N  
                 real(height * width), &                   ! K
                 1.0, 0.0, &                              ! alpha, beta
                 16.0, 16.0, 8.0]                         ! tile sizes
  work_size_gemm = (channels * kernel_h * kernel_w) * (output_h * output_w)
  
  ! Prepare parameters for direct convolution
  allocate(params_direct(12))
  params_direct = [real(batch_size, real32), real(height, real32), real(width, real32), real(channels, real32), &
                   real(kernel_h, real32), real(kernel_w, real32), real(stride, real32), real(stride, real32), &
                   real(pad, real32), real(pad, real32), real(output_h, real32), real(output_w, real32)]
  work_size_direct = batch_size * output_h * output_w
  
  print *, ""
  print *, "Configuration:"
  print '(A,I4,A,I4,A,I4,A,I4)', "  Input: ", batch_size, "x", height, "x", width, "x", channels
  print '(A,I4,A,I4)', "  Kernel: ", kernel_h, "x", kernel_w
  print '(A,I4,A,I4)', "  Output: ", output_h, "x", output_w
  print *, ""
  
  ! Profile im2col kernel
  print *, "========== im2col_nhwc Kernel =========="
  profile_im2col = profile_kernel(kernel_im2col, work_size_im2col, params_im2col(1:13))
  print *, ""
  
  ! Profile GEMM kernel
  print *, "========== gemm_tiled Kernel =========="
  profile_gemm = profile_kernel(kernel_gemm, work_size_gemm, params_gemm)
  print *, ""
  
  ! Profile direct convolution
  print *, "========== conv2d_direct Kernel =========="
  profile_direct = profile_kernel(kernel_direct, work_size_direct, params_direct)
  print *, ""
  
  ! Summary
  print *, "========================================"
  print *, "Autotuning Summary"
  print *, "========================================"
  print *, ""
  print *, "im2col_nhwc:"
  print '(A,I4,A,I2,A,F8.2,A)', "  Best config: local_size=", profile_im2col%optimal_local_size, &
         ", method=", profile_im2col%optimal_param_method, ", time=", profile_im2col%time_ms, " ms"
  print *, ""
  print *, "gemm_tiled:"
  print '(A,I4,A,I2,A,F8.2,A)', "  Best config: local_size=", profile_gemm%optimal_local_size, &
         ", method=", profile_gemm%optimal_param_method, ", time=", profile_gemm%time_ms, " ms"
  print *, ""
  print *, "conv2d_direct:"
  print '(A,I4,A,I2,A,F8.2,A)', "  Best config: local_size=", profile_direct%optimal_local_size, &
         ", method=", profile_direct%optimal_param_method, ", time=", profile_direct%time_ms, " ms"
  print *, ""
  
  ! Recommendation based on arithmetic intensity
  print *, "Device Routing Recommendations:"
  if (profile_im2col%arithmetic_intensity < 10.0) then
    print *, "  im2col: Route to iGPU (memory-bound)"
  else
    print *, "  im2col: Route to dGPU (compute-bound)"
  end if
  
  if (profile_gemm%arithmetic_intensity > 50.0) then
    print *, "  GEMM: Route to dGPU (highly compute-bound)"
  else
    print *, "  GEMM: Route to iGPU (balanced)"
  end if
  
  ! Clean up
  call cleanup_gl_context(window, ctx)
  
contains

  subroutine initialize_gl_context(window, context)
    type(c_ptr), intent(out) :: window
    integer, intent(out) :: context
    integer(c_int) :: major, minor
    
    ! Initialize GLFW
    if (glfwInit() == 0) then
      print *, "Failed to initialize GLFW"
      context = 0
      return
    end if
    
    ! Request OpenGL 4.3 (for compute shaders)
    call glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4)
    call glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3)
    call glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)
    call glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)  ! Hidden window
    
    ! Create window
    window = glfwCreateWindow(1, 1, "Sporkle GPU Context" // c_null_char, c_null_ptr, c_null_ptr)
    if (.not. c_associated(window)) then
      print *, "Failed to create GLFW window"
      call glfwTerminate()
      context = 0
      return
    end if
    
    ! Make context current
    call glfwMakeContextCurrent(window)
    
    ! Check OpenGL version
    call glGetIntegerv(GL_MAJOR_VERSION, major)
    call glGetIntegerv(GL_MINOR_VERSION, minor)
    print '(A,I1,A,I1)', "OpenGL version: ", major, ".", minor
    
    context = 1
  end subroutine initialize_gl_context
  
  subroutine cleanup_gl_context(window, context)
    type(c_ptr), intent(in) :: window
    integer, intent(in) :: context
    
    if (c_associated(window)) then
      call glfwDestroyWindow(window)
    end if
    call glfwTerminate()
  end subroutine cleanup_gl_context

end program test_convolution_autotune