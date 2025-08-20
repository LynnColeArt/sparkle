#version 310 es
precision highp float;

// Summit kernel: 32x32 tiling, 4x4 outputs per thread
layout(local_size_x = 32, local_size_y = 4) in;

// Padded shared memory to avoid bank conflicts (stride 33)
shared float input_tile[34][33];
shared float kernel_tile[34][33];

layout(std430, binding = 0) readonly buffer InputBuffer {
    vec4 input_data[];  // Use vec4 for coalesced loads
};

layout(std430, binding = 1) readonly buffer KernelBuffer {
    vec4 kernel_data[];
};

layout(std430, binding = 2) writeonly buffer OutputBuffer {
    vec4 output_data[];
};

uniform int batch_size;
uniform int in_channels;
uniform int out_channels;
uniform int height;
uniform int width;
uniform int kernel_h;
uniform int kernel_w;

void main() {
    int tx = int(gl_LocalInvocationID.x);
    int ty = int(gl_LocalInvocationID.y);
    int bx = int(gl_WorkGroupID.x);
    int by = int(gl_WorkGroupID.y);
    int oc_base = int(gl_WorkGroupID.z) * 4;  // Process 4 output channels
    
    // Each thread computes 4x4 outputs
    float acc[4][4];
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            acc[i][j] = 0.0;
        }
    }
    
    // Global position for this tile
    int tile_row = by * 32;
    int tile_col = bx * 32;
    
    // Process all input channels
    for (int ic = 0; ic < in_channels; ic += 4) {  // Vec4 stride
        // Stage 1: Cooperative load input tile (with padding for kernel)
        barrier();
        
        if (ty < 2) {  // First 2 warps load the 34x34 tile
            int load_row = tile_row + tx - 1;  // -1 for kernel padding
            int load_col = tile_col + (ty * 16) - 1;
            
            if (load_row >= 0 && load_row < height && load_col >= 0 && load_col < width) {
                int idx = ((ic * height + load_row) * width + load_col) / 4;
                vec4 data = input_data[idx];
                input_tile[tx][ty*16] = data.x;
                input_tile[tx][ty*16+1] = data.y;
                input_tile[tx][ty*16+2] = data.z;
                input_tile[tx][ty*16+3] = data.w;
            }
        }
        
        // Load kernel weights for this channel
        if (tx < kernel_h && ty < kernel_w) {
            for (int oc_off = 0; oc_off < 4; oc_off++) {
                int oc = oc_base + oc_off;
                if (oc < out_channels) {
                    int k_idx = ((oc * in_channels + ic) * kernel_h + tx) * kernel_w + ty;
                    kernel_tile[oc_off][tx * kernel_w + ty] = kernel_data[k_idx / 4][k_idx % 4];
                }
            }
        }
        
        barrier();
        
        // Stage 2: Compute phase - each thread does 4x4 outputs
        #pragma unroll 12
        for (int kh = 0; kh < kernel_h; kh++) {
            #pragma unroll 3
            for (int kw = 0; kw < kernel_w; kw++) {
                // Load kernel values once
                float k_vals[4];
                for (int oc_off = 0; oc_off < 4; oc_off++) {
                    k_vals[oc_off] = kernel_tile[oc_off][kh * kernel_w + kw];
                }
                
                // Compute 4x4 outputs
                #pragma unroll 4
                for (int out_y = 0; out_y < 4; out_y++) {
                    #pragma unroll 4
                    for (int out_x = 0; out_x < 4; out_x++) {
                        int in_y = ty * 8 + out_y + kh;  // 8 = 32/4 threads
                        int in_x = tx + out_x * 8 + kw;
                        
                        float in_val = input_tile[in_y][in_x];
                        
                        // FMA for all 4 output channels
                        for (int oc_off = 0; oc_off < 4; oc_off++) {
                            acc[oc_off][out_y * 4 + out_x] = fma(in_val, k_vals[oc_off], acc[oc_off][out_y * 4 + out_x]);
                        }
                    }
                }
            }
        }
    }
    
    // Stage 3: Write outputs (vec4 stores)
    barrier();
    
    for (int oc_off = 0; oc_off < 4; oc_off++) {
        int oc = oc_base + oc_off;
        if (oc < out_channels) {
            for (int out_y = 0; out_y < 4; out_y++) {
                int global_y = tile_row + ty * 8 + out_y;
                if (global_y < height) {
                    for (int out_x = 0; out_x < 4; out_x += 4) {  // Vec4 writes
                        int global_x = tile_col + tx + out_x * 8;
                        if (global_x < width - 3) {
                            int out_idx = ((oc * height + global_y) * width + global_x) / 4;
                            vec4 result = vec4(
                                acc[oc_off][out_y * 4 + out_x],
                                acc[oc_off][out_y * 4 + out_x + 1],
                                acc[oc_off][out_y * 4 + out_x + 2],
                                acc[oc_off][out_y * 4 + out_x + 3]
                            );
                            output_data[out_idx] = result;
                        }
                    }
                }
            }
        }
    }
}