subroutine test_conv_dsl_to_gpu() bind(C, name="test_conv_dsl_to_gpu")
  use iso_fortran_env
  use iso_c_binding
  use gl_constants
  use sparkle_shader_parser_v2
  use sparkle_fortran_params
  implicit none
  
  interface
    function create_egl_context() bind(C, name="create_egl_context")
      import :: c_int
      integer(c_int) :: create_egl_context
    end function create_egl_context
  end interface
  
  type(shader_kernel_v2) :: kernel
  character(len=:), allocatable :: glsl_source
  
  ! Test parameters
  integer, parameter :: N = 1, H = 224, W = 224, C = 3, K = 64
  integer, parameter :: kernel_size = 7, stride = 2, pad = 3
  integer, parameter :: H_out = (H + 2*pad - kernel_size) / stride + 1
  integer, parameter :: W_out = (W + 2*pad - kernel_size) / stride + 1
  
  ! Data
  real(c_float), allocatable, target :: input(:), weights(:), output(:)
  integer, target :: params(10)
  
  ! OpenGL objects
  integer(c_int) :: shader, program
  integer(c_int) :: in_ssbo, weight_ssbo, out_ssbo, param_ssbo
  integer(c_int) :: compile_status, link_status
  integer(c_int), target :: query_ids(2)
  integer(c_int64_t) :: time_start, time_end
  character(len=512, kind=c_char), target :: info_log
  integer(c_int) :: info_len
  integer :: i
  real(real64) :: gpu_time_ms
  
  print *, "=== Testing DSL to GPU Shader Generation ==="
  print *, ""
  
  ! Parse the convolution kernel from DSL
  print *, "1. Parsing convolution kernel from DSL..."
  kernel = parse_fortran_kernel_v2("examples/convolution_kernels.f90", "conv2d_direct", PARAMS_BUFFER)
  
  print *, "   Kernel name: ", trim(kernel%name)
  print *, "   Number of args: ", size(kernel%args)
  print *, "   Number of params: ", size(kernel%params)
  if (allocated(kernel%params)) then
    do i = 1, size(kernel%params)
      print *, "   Param ", i, ": ", trim(kernel%params(i)%name), " type: ", trim(kernel%params(i)%type_str)
    end do
  end if
  
  ! Generate GLSL source
  print *, ""
  print *, "2. Generating GLSL shader..."
  glsl_source = generate_glsl_v2(kernel)
  
  ! Write shader to file for inspection
  block
    integer :: unit
    open(newunit=unit, file="generated_conv2d.glsl", status="replace")
    write(unit, '(A)') glsl_source
    close(unit)
    print *, "   Shader written to: generated_conv2d.glsl"
  end block
  
  ! Initialize OpenGL context
  if (create_egl_context() == 0) then
    print *, "ERROR: Failed to create EGL context"
    stop 1
  end if
  
  ! Create and compile shader
  print *, ""
  print *, "3. Compiling GLSL shader..."
  shader = glCreateShader(GL_COMPUTE_SHADER)
  
  ! Set shader source
  block
    character(len=len(glsl_source)+1, kind=c_char), target :: c_source
    integer(c_int), target :: source_len
    type(c_ptr), target :: source_ptr
    
    c_source = glsl_source // C_NULL_CHAR
    source_len = len(glsl_source)
    source_ptr = c_loc(c_source)
    
    call glShaderSource(shader, 1, c_loc(source_ptr), c_loc(source_len))
  end block
  
  call glCompileShader(shader)
  
  call glGetShaderiv(shader, GL_COMPILE_STATUS, compile_status)
  if (compile_status == 0) then
    call glGetShaderiv(shader, GL_INFO_LOG_LENGTH, info_len)
    call glGetShaderInfoLog(shader, min(info_len, 512), C_NULL_PTR, c_loc(info_log))
    print *, "ERROR: Shader compilation failed:"
    print *, trim(info_log)
    stop 1
  end if
  print *, "   Shader compiled successfully!"
  
  ! Create and link program
  program = glCreateProgram()
  call glAttachShader(program, shader)
  call glLinkProgram(program)
  
  call glGetProgramiv(program, GL_LINK_STATUS, link_status)
  if (link_status == 0) then
    call glGetProgramiv(program, GL_INFO_LOG_LENGTH, info_len)
    call glGetProgramInfoLog(program, min(info_len, 512), C_NULL_PTR, c_loc(info_log))
    print *, "ERROR: Program linking failed:"
    print *, trim(info_log)
    stop 1
  end if
  
  ! Initialize data
  print *, ""
  print *, "4. Running GPU convolution..."
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  
  call random_number(input)
  call random_number(weights)
  input = input * 2.0 - 1.0
  weights = weights * 0.1
  
  ! Set up parameters based on the parsed kernel params
  params = [N, H, W, C, K, kernel_size, stride, pad, H_out, W_out]
  
  ! Create buffers
  call glGenBuffers(1, in_ssbo)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, in_ssbo)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                    int(size(input) * 4, c_size_t), &
                    c_loc(input), GL_DYNAMIC_DRAW)
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, in_ssbo)
  
  call glGenBuffers(1, weight_ssbo)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, weight_ssbo)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                    int(size(weights) * 4, c_size_t), &
                    c_loc(weights), GL_DYNAMIC_DRAW)
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, weight_ssbo)
  
  call glGenBuffers(1, out_ssbo)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, out_ssbo)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                    int(size(output) * 4, c_size_t), &
                    C_NULL_PTR, GL_DYNAMIC_DRAW)
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, out_ssbo)
  
  ! Parameter buffer at binding 15 (PARAM_BUFFER_BINDING)
  call glGenBuffers(1, param_ssbo)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, param_ssbo)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                    int(size(params) * 4, c_size_t), &
                    c_loc(params), GL_DYNAMIC_DRAW)
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, PARAM_BUFFER_BINDING, param_ssbo)
  
  call glUseProgram(program)
  
  ! Warmup
  do i = 1, 10
    call glDispatchCompute((N * K * H_out * W_out + 63) / 64, 1, 1)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    call glFinish()
  end do
  
  ! Benchmark
  call glGenQueries(2, query_ids)
  call glQueryCounter(query_ids(1), GL_TIMESTAMP)
  
  do i = 1, 20
    call glDispatchCompute((N * K * H_out * W_out + 63) / 64, 1, 1)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
  end do
  call glFinish()
  
  call glQueryCounter(query_ids(2), GL_TIMESTAMP)
  
  ! Get timestamps
  call glGetQueryObjectui64v(query_ids(1), GL_QUERY_RESULT, time_start)
  call glGetQueryObjectui64v(query_ids(2), GL_QUERY_RESULT, time_end)
  
  gpu_time_ms = real(time_end - time_start, real64) / 1.0e6_real64 / 20.0_real64
  
  ! Read back results
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, out_ssbo)
  call glGetBufferSubData(GL_SHADER_STORAGE_BUFFER, &
                          0_c_size_t, &
                          int(size(output) * 4, c_size_t), &
                          c_loc(output))
  
  ! Performance metrics
  block
    integer(int64) :: total_flops
    real(real64) :: gflops
    
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    
    gflops = real(total_flops, real64) / (gpu_time_ms * 1.0e6_real64)
    
    print *, "   GPU time: ", gpu_time_ms, " ms"
    print *, "   Performance: ", gflops, " GFLOPS"
    print *, "   Output sample: ", output(1:5)
  end block
  
  ! Cleanup
  call glDeleteBuffers(1, in_ssbo)
  call glDeleteBuffers(1, weight_ssbo)
  call glDeleteBuffers(1, out_ssbo)
  call glDeleteBuffers(1, param_ssbo)
  call glDeleteProgram(program)
  call glDeleteShader(shader)
  call glDeleteQueries(2, query_ids)
  
  deallocate(input, weights, output)
  
  print *, ""
  print *, "=== Success! DSL kernel compiled and executed on GPU ==="
  
end subroutine test_conv_dsl_to_gpu