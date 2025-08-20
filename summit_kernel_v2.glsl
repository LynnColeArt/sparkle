#version 450
// Summit Kernel V2: The Real Implementation
// 32×32 tiling, Ko=64 blocking, 256 threads, 4×4 outputs per thread

layout(local_size_x = 32, local_size_y = 8) in;  // 256 threads total

// Padded shared memory to avoid bank conflicts
shared float input_tile[34][33];   // 32+2 padding, stride 33
shared float kernel_cache[64][9];  // Ko=64 kernels cached

layout(std430, binding = 0) readonly buffer InputBuffer {
    vec4 input_data[];  // NHWC layout, vec4 aligned
};

layout(std430, binding = 1) readonly buffer KernelBuffer {
    vec4 kernel_data[];  // Packed weights
};

layout(std430, binding = 2) buffer OutputBuffer {
    vec4 output_data[];  // Output in NHWC
};

uniform int batch_size;
uniform int height;
uniform int width;
uniform int in_channels;
uniform int out_channels;

// Optimized convolution with Ko=64 blocking
void main() {
    // Thread and workgroup IDs
    uint tx = gl_LocalInvocationID.x;  // 0-31
    uint ty = gl_LocalInvocationID.y;  // 0-7
    uint bx = gl_WorkGroupID.x;
    uint by = gl_WorkGroupID.y;
    uint batch_id = gl_WorkGroupID.z;
    
    // Each workgroup processes a 32×32 output tile
    uint tile_start_x = bx * 32;
    uint tile_start_y = by * 32;
    
    // Each thread computes 4×4 outputs
    float acc[64][4][4];  // Ko=64, 4×4 spatial
    
    // Initialize accumulators
    for (int ko = 0; ko < 64; ko++) {
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                acc[ko][i][j] = 0.0;
            }
        }
    }
    
    // Process all input channels in blocks
    for (int ci_base = 0; ci_base < in_channels; ci_base += 4) {
        
        // Stage 1: Cooperatively load 34×34 input tile (with padding for 3×3 kernel)
        barrier();
        
        // Each thread loads multiple elements to fill the tile
        uint elements_per_thread = (34 * 34 + 255) / 256;  // Ceiling division
        for (uint elem = 0; elem < elements_per_thread; elem++) {
            uint linear_id = ty * 32 + tx + elem * 256;
            if (linear_id < 34 * 34) {
                uint tile_y = linear_id / 34;
                uint tile_x = linear_id % 34;
                
                // Global coordinates (with padding offset)
                int global_y = int(tile_start_y + tile_y) - 1;
                int global_x = int(tile_start_x + tile_x) - 1;
                
                if (global_y >= 0 && global_y < height && 
                    global_x >= 0 && global_x < width) {
                    // NHWC layout: [batch][height][width][channels]
                    uint idx = ((batch_id * height + global_y) * width + global_x) * in_channels + ci_base;
                    vec4 data = input_data[idx / 4];
                    
                    // Store in shared memory (handle vec4 components)
                    input_tile[tile_y][tile_x] = data[idx % 4];
                } else {
                    input_tile[tile_y][tile_x] = 0.0;  // Padding
                }
            }
        }
        
        // Load kernel weights for Ko=64 outputs
        if (ty < 2) {  // First 64 threads load kernels
            uint ko = ty * 32 + tx;
            if (ko < 64) {
                for (int k = 0; k < 9; k++) {
                    // 3×3 kernel weights for this output channel
                    uint kernel_idx = (ko * in_channels + ci_base) * 9 + k;
                    kernel_cache[ko][k] = kernel_data[kernel_idx / 4][kernel_idx % 4];
                }
            }
        }
        
        barrier();
        
        // Stage 2: Compute phase - each thread handles 4×4 outputs
        // Thread (tx,ty) processes outputs at positions:
        // x: [tx*1, tx*1+8, tx*1+16, tx*1+24]  (stride 8)
        // y: [ty*4, ty*4+1, ty*4+2, ty*4+3]     (4 consecutive)
        
        for (int ko = 0; ko < 64; ko++) {
            // Load kernel weights for this output channel
            float k00 = kernel_cache[ko][0];
            float k01 = kernel_cache[ko][1];
            float k02 = kernel_cache[ko][2];
            float k10 = kernel_cache[ko][3];
            float k11 = kernel_cache[ko][4];
            float k12 = kernel_cache[ko][5];
            float k20 = kernel_cache[ko][6];
            float k21 = kernel_cache[ko][7];
            float k22 = kernel_cache[ko][8];
            
            // Compute 4×4 outputs with full 3×3 convolution
            #pragma unroll
            for (int oy = 0; oy < 4; oy++) {
                uint sy = ty * 4 + oy + 1;  // +1 for padding offset
                
                #pragma unroll
                for (int ox = 0; ox < 4; ox++) {
                    uint sx = tx + ox * 8 + 1;  // Strided by 8, +1 for padding
                    
                    if (sx < 33 && sy < 33) {  // Bounds check
                        float sum = 0.0;
                        
                        // 3×3 convolution kernel
                        sum = fma(input_tile[sy-1][sx-1], k00, sum);
                        sum = fma(input_tile[sy-1][sx  ], k01, sum);
                        sum = fma(input_tile[sy-1][sx+1], k02, sum);
                        sum = fma(input_tile[sy  ][sx-1], k10, sum);
                        sum = fma(input_tile[sy  ][sx  ], k11, sum);
                        sum = fma(input_tile[sy  ][sx+1], k12, sum);
                        sum = fma(input_tile[sy+1][sx-1], k20, sum);
                        sum = fma(input_tile[sy+1][sx  ], k21, sum);
                        sum = fma(input_tile[sy+1][sx+1], k22, sum);
                        
                        acc[ko][oy][ox] += sum;
                    }
                }
            }
        }
    }
    
    // Stage 3: Write outputs (vec4 coalesced stores)
    barrier();
    
    // Each thread writes its 4×4×64 outputs
    for (int ko_base = 0; ko_base < 64; ko_base += 4) {
        for (int oy = 0; oy < 4; oy++) {
            uint global_y = tile_start_y + ty * 4 + oy;
            
            if (global_y < height) {
                for (int ox = 0; ox < 4; ox++) {
                    uint global_x = tile_start_x + tx + ox * 8;
                    
                    if (global_x < width && ko_base < out_channels) {
                        // Pack 4 output channels into vec4
                        vec4 output_vec;
                        output_vec.x = acc[ko_base + 0][oy][ox];
                        output_vec.y = (ko_base + 1 < out_channels) ? acc[ko_base + 1][oy][ox] : 0.0;
                        output_vec.z = (ko_base + 2 < out_channels) ? acc[ko_base + 2][oy][ox] : 0.0;
                        output_vec.w = (ko_base + 3 < out_channels) ? acc[ko_base + 3][oy][ox] : 0.0;
                        
                        // NHWC layout write
                        uint out_idx = ((batch_id * height + global_y) * width + global_x) * out_channels + ko_base;
                        output_data[out_idx / 4] = output_vec;
                    }
                }
            }
        }
    }
}