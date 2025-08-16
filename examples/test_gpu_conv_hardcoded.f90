program test_gpu_conv_hardcoded
  ! Test with hardcoded GLSL that we know works
  use iso_fortran_env
  use iso_c_binding
  use gl_constants
  implicit none
  
  ! OpenGL objects
  integer(c_int) :: shader, program
  integer(c_int) :: in_ssbo, weight_ssbo, out_ssbo, param_ssbo
  integer(c_int) :: compile_status, link_status
  
  ! Test parameters
  integer, parameter :: N = 1, H = 32, W = 32, C = 3, K = 64
  integer, parameter :: kernel_size = 3, stride = 1, pad = 0
  integer, parameter :: H_out = (H - kernel_size) / stride + 1
  integer, parameter :: W_out = (W - kernel_size) / stride + 1
  
  ! Data
  real(c_float), allocatable, target :: input(:), weights(:), output(:)
  integer, target :: params(10)
  
  ! Timing
  real(real64) :: start_time, end_time, gpu_time_ms
  
  ! GLSL code - simple conv2d
  character(len=*), parameter :: glsl_code = &
'#version 430 core'//NEW_LINE('A')// &
'layout(local_size_x = 64) in;'//NEW_LINE('A')// &
''//NEW_LINE('A')// &
'layout(std430, binding = 0) readonly buffer InputBuffer {'//NEW_LINE('A')// &
'  float data[];'//NEW_LINE('A')// &
'} input_buf;'//NEW_LINE('A')// &
''//NEW_LINE('A')// &
'layout(std430, binding = 1) readonly buffer WeightBuffer {'//NEW_LINE('A')// &
'  float data[];'//NEW_LINE('A')// &
'} weight_buf;'//NEW_LINE('A')// &
''//NEW_LINE('A')// &
'layout(std430, binding = 2) writeonly buffer OutputBuffer {'//NEW_LINE('A')// &
'  float data[];'//NEW_LINE('A')// &
'} output_buf;'//NEW_LINE('A')// &
''//NEW_LINE('A')// &
'layout(std430, binding = 3) readonly buffer ParamBuffer {'//NEW_LINE('A')// &
'  int N, H, W, C, K;'//NEW_LINE('A')// &
'  int kernel_size, stride, pad;'//NEW_LINE('A')// &
'  int H_out, W_out;'//NEW_LINE('A')// &
'} params;'//NEW_LINE('A')// &
''//NEW_LINE('A')// &
'void main() {'//NEW_LINE('A')// &
'  uint idx = gl_GlobalInvocationID.x;'//NEW_LINE('A')// &
'  if (idx >= uint(params.N * params.K * params.H_out * params.W_out)) return;'//NEW_LINE('A')// &
'  '//NEW_LINE('A')// &
'  // Decode output position'//NEW_LINE('A')// &
'  int n = int(idx) / (params.K * params.H_out * params.W_out);'//NEW_LINE('A')// &
'  int k = (int(idx) / (params.H_out * params.W_out)) % params.K;'//NEW_LINE('A')// &
'  int h_out = (int(idx) / params.W_out) % params.H_out;'//NEW_LINE('A')// &
'  int w_out = int(idx) % params.W_out;'//NEW_LINE('A')// &
'  '//NEW_LINE('A')// &
'  float sum = 0.0;'//NEW_LINE('A')// &
'  '//NEW_LINE('A')// &
'  // Convolution'//NEW_LINE('A')// &
'  for (int c = 0; c < params.C; c++) {'//NEW_LINE('A')// &
'    for (int kh = 0; kh < params.kernel_size; kh++) {'//NEW_LINE('A')// &
'      for (int kw = 0; kw < params.kernel_size; kw++) {'//NEW_LINE('A')// &
'        int h_in = h_out * params.stride + kh;'//NEW_LINE('A')// &
'        int w_in = w_out * params.stride + kw;'//NEW_LINE('A')// &
'        '//NEW_LINE('A')// &
'        if (h_in < params.H && w_in < params.W) {'//NEW_LINE('A')// &
'          int in_idx = ((n * params.C + c) * params.H + h_in) * params.W + w_in;'//NEW_LINE('A')// &
'          int weight_idx = ((k * params.C + c) * params.kernel_size + kh) * params.kernel_size + kw;'//NEW_LINE('A')// &
'          sum += input_buf.data[in_idx] * weight_buf.data[weight_idx];'//NEW_LINE('A')// &
'        }'//NEW_LINE('A')// &
'      }'//NEW_LINE('A')// &
'    }'//NEW_LINE('A')// &
'  }'//NEW_LINE('A')// &
'  '//NEW_LINE('A')// &
'  output_buf.data[idx] = sum;'//NEW_LINE('A')// &
'}'

  character(len=1024), target :: info_log
  integer(c_int), target :: info_len, actual_len
  character(len=:,kind=c_char), allocatable, target :: c_source
  type(c_ptr) :: source_ptr
  type(c_ptr), target :: source_ptr_array(1)
  integer(c_int), target :: source_len_array(1)
  
  print *, "=== GPU Convolution Test (Hardcoded GLSL) ==="
  print *, "Shape: ", N, "x", H, "x", W, "x", C, " -> ", N, "x", H_out, "x", W_out, "x", K
  print *, ""
  
  ! Initialize data
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  
  input = 1.0
  weights = 0.1
  output = 0.0
  
  params = [N, H, W, C, K, kernel_size, stride, pad, H_out, W_out]
  
  ! Create and compile shader
  print *, "1. Compiling shader..."
  shader = glCreateShader(GL_COMPUTE_SHADER)
  
  c_source = glsl_code // c_null_char
  source_ptr = c_loc(c_source)
  source_ptr_array(1) = source_ptr
  source_len_array(1) = len(glsl_code)
  
  call glShaderSource(shader, 1, c_loc(source_ptr_array), c_loc(source_len_array))
  call glCompileShader(shader)
  
  call glGetShaderiv(shader, GL_COMPILE_STATUS, compile_status)
  if (compile_status == 0) then
    call glGetShaderiv(shader, GL_INFO_LOG_LENGTH, info_len)
    call glGetShaderInfoLog(shader, min(info_len, 1024), c_loc(actual_len), c_loc(info_log))
    print *, "ERROR: Shader compilation failed:"
    print *, info_log(1:actual_len)
    stop
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
    call glGetProgramInfoLog(program, min(info_len, 1024), c_loc(actual_len), c_loc(info_log))
    print *, "ERROR: Program linking failed:"
    print *, info_log(1:actual_len)
    stop
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
  
  ! Execute
  print *, "4. Executing on GPU..."
  call glUseProgram(program)
  
  call cpu_time(start_time)
  
  ! Dispatch enough threads for all output elements
  call glDispatchCompute((N * K * H_out * W_out + 63) / 64, 1, 1)
  call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
  call glFinish()
  
  call cpu_time(end_time)
  gpu_time_ms = (end_time - start_time) * 1000.0
  
  print *, "   Execution complete"
  
  ! Read back results
  print *, "5. Reading results..."
  call glBindBuffer(GL_SHADER_STORAGE_BUFFER, out_ssbo)
  call glGetBufferSubData(GL_SHADER_STORAGE_BUFFER, &
                          0_c_size_t, &
                          int(size(output) * 4, c_size_t), &
                          c_loc(output))
  
  print *, "   Output[1] = ", output(1)
  print *, "   Output sum = ", sum(output)
  print *, "   Expected = ", C * kernel_size * kernel_size * 0.1
  print *, ""
  print *, "=== RESULTS ==="
  print *, "GPU time (ms): ", gpu_time_ms
  print *, "Output shape: ", N, "x", K, "x", H_out, "x", W_out
  print *, "Total FLOPs: ", N * K * H_out * W_out * C * kernel_size * kernel_size * 2
  print *, "GFLOPS: ", (N * K * H_out * W_out * C * kernel_size * kernel_size * 2) / (gpu_time_ms * 1.0e6)
  
  ! Cleanup
  call glDeleteBuffers(1, in_ssbo)
  call glDeleteBuffers(1, weight_ssbo)
  call glDeleteBuffers(1, out_ssbo)
  call glDeleteBuffers(1, param_ssbo)
  call glDeleteProgram(program)
  call glDeleteShader(shader)
  
end program test_gpu_conv_hardcoded