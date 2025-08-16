program test_single_gpu_conv
  ! Minimal test: Parse -> Generate -> Run -> Measure
  use iso_fortran_env
  use iso_c_binding
  use gl_constants
  use sparkle_shader_parser_v2
  use sparkle_gpu_executor
  use sparkle_fortran_params
  implicit none
  
  type(shader_kernel_v2) :: kernel
  type(gpu_executor) :: exec
  character(len=:), allocatable :: glsl_code
  
  ! Test parameters - small fixed size
  integer, parameter :: N = 1, H = 32, W = 32, C = 3, K = 64
  integer, parameter :: kernel_h = 3, kernel_w = 3
  integer, parameter :: stride = 1, pad = 1
  integer, parameter :: H_out = (H + 2*pad - kernel_h)/stride + 1
  integer, parameter :: W_out = (W + 2*pad - kernel_w)/stride + 1
  
  ! Buffers
  real(real32), allocatable, target :: input(:), weights(:), output(:)
  integer(c_int) :: in_buf, weight_buf, out_buf
  integer :: input_size, weight_size, output_size
  
  ! Timing
  real(real64) :: start_time, end_time, gpu_time_ms
  
  ! GL context
  type(c_ptr) :: window
  integer :: ctx
  
  print *, "=== Sporkle GPU Convolution Test ==="
  print *, "Shape: N=", N, " H=", H, " W=", W, " C=", C, " K=", K
  print *, "Kernel: ", kernel_h, "x", kernel_w
  print *, "Output: ", H_out, "x", W_out
  print *, ""
  
  ! Initialize OpenGL context
  call init_gl_context()
  
  ! 1. PARSE KERNEL
  print *, "1. Parsing kernel..."
  kernel = parse_fortran_kernel_v2("examples/kernels_convolution.f90", &
                                   "conv2d_direct", PARAMS_BUFFER)
  print *, "   Parsed: ", trim(kernel%name)
  print *, "   Args: ", size(kernel%args)
  print *, "   Params: ", size(kernel%params)
  
  ! 2. GENERATE GLSL
  print *, ""
  print *, "2. Generating GLSL..."
  glsl_code = generate_glsl_v2(kernel)
  print *, "   Generated ", len(glsl_code), " chars of GLSL"
  ! Uncomment to see GLSL:
  ! print *, "---- GLSL CODE ----"
  ! print *, glsl_code
  ! print *, "-------------------"
  
  ! 3. INITIALIZE GPU EXECUTOR
  print *, ""
  print *, "3. Initializing GPU executor..."
  call initialize_executor(exec, kernel)
  if (.not. exec%initialized) then
    print *, "ERROR: Failed to initialize executor"
    stop
  end if
  print *, "   Executor initialized"
  
  ! 4. CREATE BUFFERS
  print *, ""
  print *, "4. Creating GPU buffers..."
  
  ! Allocate CPU memory
  input_size = N * C * H * W
  weight_size = K * C * kernel_h * kernel_w
  output_size = N * K * H_out * W_out
  
  allocate(input(input_size))
  allocate(weights(weight_size))
  allocate(output(output_size))
  
  ! Initialize with test data
  input = 1.0
  weights = 0.1
  output = 0.0
  
  ! Create GPU buffers
  in_buf = create_buffer(exec, int(input_size * 4, c_size_t), 0)
  weight_buf = create_buffer(exec, int(weight_size * 4, c_size_t), 1)
  out_buf = create_buffer(exec, int(output_size * 4, c_size_t), 2)
  
  ! Upload data
  call update_buffer(in_buf, c_loc(input), int(input_size * 4, c_size_t))
  call update_buffer(weight_buf, c_loc(weights), int(weight_size * 4, c_size_t))
  call update_buffer(out_buf, c_loc(output), int(output_size * 4, c_size_t))
  
  print *, "   Input buffer: ", input_size, " floats"
  print *, "   Weight buffer: ", weight_size, " floats"
  print *, "   Output buffer: ", output_size, " floats"
  
  ! 5. PREPARE PARAMETERS
  print *, ""
  print *, "5. Setting kernel parameters..."
  block
    real(real32), allocatable :: params(:)
    allocate(params(12))  ! conv2d_direct has 12 scalar params
    params = [real(N,real32), real(H,real32), real(W,real32), real(C,real32), &
              real(K,real32), real(kernel_h,real32), real(kernel_w,real32), &
              real(stride,real32), real(stride,real32), &
              real(pad,real32), real(pad,real32), &
              real(H_out*W_out,real32)]
    
    ! 6. RUN ON GPU
    print *, ""
    print *, "6. Running on GPU..."
    
    ! Start timing
    call cpu_time(start_time)
    
    ! Execute kernel
    call execute_kernel(exec, kernel, N * H_out * W_out, params)
    
    ! End timing
    call cpu_time(end_time)
    gpu_time_ms = (end_time - start_time) * 1000.0
    
    print *, "   Execution complete"
  end block
  
  ! 7. READ BACK RESULTS
  print *, ""
  print *, "7. Reading results..."
  call read_buffer(out_buf, c_loc(output), int(output_size * 4, c_size_t))
  
  ! Check if output is non-zero
  print *, "   Output[0] = ", output(1)
  print *, "   Output sum = ", sum(output)
  print *, "   Output mean = ", sum(output) / size(output)
  
  ! 8. REPORT TIMING
  print *, ""
  print *, "=== RESULTS ==="
  print *, "GPU time (ms): ", gpu_time_ms
  print *, ""
  
  ! Cleanup
  call delete_buffer(in_buf)
  call delete_buffer(weight_buf)
  call delete_buffer(out_buf)
  call cleanup_executor(exec)
  
contains

  subroutine init_gl_context()
    ! Minimal GL context setup without GLFW
    ! For now, assume GL context exists or use EGL
    print *, "Note: Using existing GL context"
    ! In production, would create context with EGL or similar
  end subroutine init_gl_context

end program test_single_gpu_conv