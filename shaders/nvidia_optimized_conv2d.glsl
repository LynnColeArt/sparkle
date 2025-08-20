#version 450

// Optimized NVIDIA convolution using discovered universal patterns
// Target: 70% efficiency (16.5 TFLOPS on A4500)

// Optimal work group from profiler: 128 threads
layout(local_size_x = 32, local_size_y = 4, local_size_z = 1) in;

// Storage buffers
layout(std430, binding = 0) readonly buffer InputBuffer {
    float input_data[];
};

layout(std430, binding = 1) readonly buffer KernelBuffer {
    float kernel_data[];
};

layout(std430, binding = 2) writeonly buffer OutputBuffer {
    float output_data[];
};

// Convolution parameters
uniform int batch;
uniform int in_channels;
uniform int out_channels;
uniform int height;
uniform int width;
uniform int kernel_h;
uniform int kernel_w;
uniform int out_height;
uniform int out_width;

// Shared memory for cooperative tiling
// 32x32 tile + padding for bank conflict avoidance
shared float tile_input[34][34];
shared float tile_kernel[32];

void main() {
    // Thread indices within work group
    const uint local_x = gl_LocalInvocationID.x;
    const uint local_y = gl_LocalInvocationID.y;
    const uint local_id = local_y * 32 + local_x;
    
    // Work group position in output
    const uint group_out_x = gl_WorkGroupID.x * 32;
    const uint group_out_y = gl_WorkGroupID.y * 32;
    const uint out_c = gl_WorkGroupID.z;
    
    // Each thread computes a 4x4 output tile
    const uint thread_out_x = group_out_x + (local_id % 8) * 4;
    const uint thread_out_y = group_out_y + (local_id / 8) * 4;
    
    // Accumulator registers for 4x4 outputs
    float acc[4][4];
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            acc[i][j] = 0.0;
        }
    }
    
    // Main convolution loop over input channels
    for (int in_c = 0; in_c < in_channels; in_c++) {
        // Loop over kernel positions
        for (int ky = 0; ky < kernel_h; ky++) {
            for (int kx = 0; kx < kernel_w; kx++) {
                
                // === PHASE 1: Cooperative load to shared memory ===
                barrier();
                
                // Load 34x34 input tile (32x32 + padding for 3x3 kernel)
                // Each thread loads ~9 elements for full coverage
                for (int load_idx = 0; load_idx < 9; load_idx++) {
                    uint element_id = local_id * 9 + load_idx;
                    if (element_id < 34 * 34) {
                        uint tile_y = element_id / 34;
                        uint tile_x = element_id % 34;
                        
                        int input_y = int(group_out_y + tile_y) + ky - 1;
                        int input_x = int(group_out_x + tile_x) + kx - 1;
                        
                        float value = 0.0;
                        if (input_y >= 0 && input_y < height && 
                            input_x >= 0 && input_x < width) {
                            uint input_idx = ((0 * in_channels + in_c) * height + input_y) * width + input_x;
                            value = input_data[input_idx];
                        }
                        tile_input[tile_y][tile_x] = value;
                    }
                }
                
                // Load kernel weight (single value, broadcast to all threads)
                if (local_id == 0) {
                    uint kernel_idx = ((out_c * in_channels + in_c) * kernel_h + ky) * kernel_w + kx;
                    tile_kernel[0] = kernel_data[kernel_idx];
                }
                
                barrier();
                
                // === PHASE 2: Compute from shared memory ===
                float k_value = tile_kernel[0];
                
                // Unroll 4x4 computation
                #pragma unroll
                for (int dy = 0; dy < 4; dy++) {
                    #pragma unroll  
                    for (int dx = 0; dx < 4; dx++) {
                        uint sy = (local_id / 8) * 4 + dy + 1; // +1 for padding
                        uint sx = (local_id % 8) * 4 + dx + 1; // +1 for padding
                        
                        if (sy < 34 && sx < 34) {
                            acc[dy][dx] += tile_input[sy][sx] * k_value;
                        }
                    }
                }
            }
        }
    }
    
    // === PHASE 3: Write outputs ===
    // Coalesced writes - threads write contiguous memory
    for (int dy = 0; dy < 4; dy++) {
        for (int dx = 0; dx < 4; dx++) {
            uint out_y = thread_out_y + dy;
            uint out_x = thread_out_x + dx;
            
            if (out_y < out_height && out_x < out_width) {
                uint output_idx = ((0 * out_channels + out_c) * out_height + out_y) * out_width + out_x;
                output_data[output_idx] = acc[dy][dx];
            }
        }
    }
}