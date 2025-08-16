#version 450
#extension GL_ARB_compute_shader : enable
#extension GL_ARB_shader_storage_buffer_object : enable

// RDNA3 optimizations: dual-issue capable
#extension GL_ARB_gpu_shader_fp64 : enable

layout(local_size_x = 64, local_size_y = 1, local_size_z = 1) in;

#define WAVE_SIZE 32

// Input buffer
layout(std430, binding = 0) readonly buffer InputBuffer {
  float data[];
} input_buf;

// Weight buffer
layout(std430, binding = 1) readonly buffer WeightBuffer {
  float data[];
} weight_buf;

// Output buffer
layout(std430, binding = 2) writeonly buffer OutputBuffer {
  float data[];
} output_buf;

// Convolution parameters
layout(std430, binding = 3) readonly buffer ParamBuffer {
  int N, H, W, C, K;
  int kernel_size, stride, pad;
  int H_out, W_out;
} params;

void main() {
  uint idx = gl_GlobalInvocationID.x;
  if (idx >= uint(params.N * params.K * params.H_out * params.W_out)) return;

  // Decode output position
  int n = int(idx) / (params.K * params.H_out * params.W_out);
  int k = (int(idx) / (params.H_out * params.W_out)) % params.K;
  int h_out = (int(idx) / params.W_out) % params.H_out;
  int w_out = int(idx) % params.W_out;

  float sum = 0.0;

  // Convolution
  for (int c = 0; c < params.C; c++) {
    for (int kh = 0; kh < params.kernel_size; kh++) {
      for (int kw = 0; kw < params.kernel_size; kw++) {
        int h_in = h_out * params.stride + kh - params.pad;
        int w_in = w_out * params.stride + kw - params.pad;

        if (h_in >= 0 && h_in < params.H && w_in >= 0 && w_in < params.W) {
          int in_idx = ((n * params.C + c) * params.H + h_in) * params.W + w_in;
          int weight_idx = ((k * params.C + c) * params.kernel_size + kh) * params.kernel_size + kw;
          sum += input_buf.data[in_idx] * weight_buf.data[weight_idx];
        }
      }
    }
  }

  output_buf.data[idx] = sum;
}

