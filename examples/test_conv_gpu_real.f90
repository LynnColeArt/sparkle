subroutine test_conv_gpu_real() bind(C, name="test_conv_gpu_real")
  use iso_fortran_env
  use iso_c_binding
  use gl_constants
  implicit none
  
  interface
    subroutine set_conv2d_shader_source(shader) bind(C, name="set_conv2d_shader_source")
      import :: c_int
      integer(c_int), value :: shader
    end subroutine set_conv2d_shader_source
  end interface
  
  ! OpenGL objects
  integer(c_int) :: shader, program
  integer(c_int) :: in_ssbo, weight_ssbo, out_ssbo, param_ssbo
  integer(c_int) :: compile_status, link_status
  integer(c_int), target :: query_ids(2)
  integer(c_int64_t) :: time_start, time_end
  
  ! Test parameters
  integer, parameter :: N = 1, H = 224, W = 224, C = 3, K = 64
  integer, parameter :: kernel_size = 7, stride = 2, pad = 3
  integer, parameter :: H_out = (H + 2*pad - kernel_size) / stride + 1
  integer, parameter :: W_out = (W + 2*pad - kernel_size) / stride + 1
  
  ! Data
  real(c_float), allocatable, target :: input(:), weights(:), output(:)
  integer, target :: params(10)
  
  ! Timing
  real(real64) :: gpu_time_ms
  integer :: i
  
  character(len=512, kind=c_char), target :: info_log
  integer(c_int) :: info_len
  
  ! Performance metrics
  integer(int64) :: total_flops
  real(real64) :: gflops
  integer(int64) :: bytes_read, bytes_written
  real(real64) :: bandwidth_gb
  
  print *, "=== Real GPU Convolution Test ==="
  print *, "Input shape: ", N, "x", C, "x", H, "x", W
  print *, "Output shape: ", N, "x", K, "x", H_out, "x", W_out
  print *, "Kernel: ", kernel_size, "x", kernel_size
  print *, "Stride: ", stride, ", Pad: ", pad
  
  ! Initialize data
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  
  ! Initialize with random values
  call random_number(input)
  call random_number(weights)
  output = 0.0
  
  params = [N, H, W, C, K, kernel_size, stride, pad, H_out, W_out]
  
  ! Create and compile shader
  print *, ""
  print *, "1. Compiling shader..."
  shader = glCreateShader(GL_COMPUTE_SHADER)
  
  ! Set source from C
  call set_conv2d_shader_source(shader)
  
  call glCompileShader(shader)
  
  call glGetShaderiv(shader, GL_COMPILE_STATUS, compile_status)
  if (compile_status == 0) then
    call glGetShaderiv(shader, GL_INFO_LOG_LENGTH, info_len)
    call glGetShaderInfoLog(shader, min(info_len, 512), C_NULL_PTR, c_loc(info_log))
    print *, "ERROR: Shader compilation failed:"
    print *, trim(info_log)
    return
  end if
  print *, "   Shader compiled successfully"
  
  ! Create and link program
  print *, "2. Linking program..."
  program = glCreateProgram()
  call glAttachShader(program, shader)
  call glLinkProgram(program)
  
  call glGetProgramiv(program, GL_LINK_STATUS, link_status)
  if (link_status == 0) then
    call glGetProgramiv(program, GL_INFO_LOG_LENGTH, info_len)
    call glGetProgramInfoLog(program, min(info_len, 512), C_NULL_PTR, c_loc(info_log))
    print *, "ERROR: Program linking failed:"
    print *, trim(info_log)
    return
  end if
  print *, "   Program linked successfully"
  
  ! Create buffers
  print *, "3. Creating buffers..."
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
                    c_loc(output), GL_DYNAMIC_DRAW)
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, out_ssbo)
  
  call glGenBuffers(1, param_ssbo)
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, param_ssbo)
  call glBufferData(GL_SHADER_STORAGE_BUFFER, &
                    int(size(params) * 4, c_size_t), &
                    c_loc(params), GL_DYNAMIC_DRAW)
  call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 3, param_ssbo)
  
  print *, "   Buffers created"
  
  ! Use program
  call glUseProgram(program)
  
  ! Warm up
  print *, "4. Warming up GPU..."
  do i = 1, 5
    call glDispatchCompute((N * K * H_out * W_out + 63) / 64, 1, 1)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    call glFinish()
  end do
  
  ! Time with GPU queries
  print *, "5. Benchmarking..."
  call glGenQueries(2, query_ids)
  
  call glQueryCounter(query_ids(1), GL_TIMESTAMP)
  
  ! Execute 10 times
  do i = 1, 10
    call glDispatchCompute((N * K * H_out * W_out + 63) / 64, 1, 1)
    call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
  end do
  call glFinish()
  
  call glQueryCounter(query_ids(2), GL_TIMESTAMP)
  
  ! Get timestamps
  call glGetQueryObjectui64v(query_ids(1), GL_QUERY_RESULT, time_start)
  call glGetQueryObjectui64v(query_ids(2), GL_QUERY_RESULT, time_end)
  
  gpu_time_ms = real(time_end - time_start, real64) / 1.0e6_real64 / 10.0_real64
  
  ! Read back results
  print *, "6. Reading results..."
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, out_ssbo)
  call glGetBufferSubData(GL_SHADER_STORAGE_BUFFER, &
                          0_c_size_t, &
                          int(min(100, size(output)) * 4, c_size_t), &
                          c_loc(output))
  
  print *, "   First few outputs:", output(1:min(5, size(output)))
  
  ! Calculate performance
  print *, ""
  print *, "=== Performance Results ==="
  print *, "GPU time per iteration: ", gpu_time_ms, " ms"
  
  ! FLOPs calculation
  
  total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
  gflops = real(total_flops, real64) / (gpu_time_ms * 1.0e6_real64)
  
  print *, "Total FLOPs: ", total_flops
  print *, "GFLOPS: ", gflops
  
  ! Memory bandwidth
  
  bytes_read = int(size(input) + size(weights), int64) * 4_int64
  bytes_written = int(size(output), int64) * 4_int64
  bandwidth_gb = real(bytes_read + bytes_written, real64) / (gpu_time_ms * 1.0e6_real64)
  
  print *, "Memory bandwidth: ", bandwidth_gb, " GB/s"
  print *, "Arithmetic intensity: ", real(total_flops, real64) / real(bytes_read + bytes_written, real64), " FLOPS/byte"
  
  ! Cleanup
  call glDeleteBuffers(1, in_ssbo)
  call glDeleteBuffers(1, weight_ssbo)
  call glDeleteBuffers(1, out_ssbo)
  call glDeleteBuffers(1, param_ssbo)
  call glDeleteProgram(program)
  call glDeleteShader(shader)
  call glDeleteQueries(2, query_ids)
  
  deallocate(input, weights, output)
  
end subroutine test_conv_gpu_real