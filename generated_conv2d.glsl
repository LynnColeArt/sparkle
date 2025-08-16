#version 310 es
layout(local_size_x = 64, local_size_y = 1, local_size_z = 1) in;
layout(std430, binding = 15) readonly buffer ParamBuffer {
  uint idx;
  uint N;
  uint H;
  uint W;
  uint C;
  uint K;
  uint kernel_size;
  uint stride;
  uint pad;
  uint H_out;
  uint W_out;
} params;

void main() {
  uint idx = gl_GlobalInvocationID.x;
  if (params.idx >= params.N * params.K * params.H_out * params.W_out) return;
  n = params.idx / (params.K * params.H_out * params.W_out);
  k = mod(params.idx / (params.H_out * params.W_out), params.K);
  h_out = mod(params.idx / params.W_out, params.H_out);
  w_out = mod(params.idx, params.W_out);
  sum = 0.0;
  do c = 0, params.C - 1;
  do kh = 0, params.kernel_size - 1;
  do kw = 0, params.kernel_size - 1;
  h_in = h_out * params.stride + kh - params.pad;
  w_in = w_out * params.stride + kw - params.pad;
  if (h_in >= 0 .and. h_in < params.H .and. w_in >= 0 .and. w_in < params.W) then;
  in_idx = ((n * params.C + c) * params.H + h_in) * params.W + w_in + 1;
  weight_idx = ((k * params.C + c) * params.kernel_size + kh) * params.kernel_size + kw + 1;
  sum = sum + input_data(in_idx) * weights(weight_idx);
  end if
  end do
  end do
  end do
  out_idx = params.idx + 1;
  output_data(out_idx) = sum;
}

