#version 310 es
// Summit Kernel V2 ES: OpenGL ES 3.1 Compatible Version
// 32×32 tiling, Ko=64 blocking, 256 threads, 4×4 outputs per thread

precision highp float;
precision highp int;

layout(local_size_x = 32, local_size_y = 8) in;  // 256 threads total

// Padded shared memory to avoid bank conflicts
shared float input_tile[34][33];   // 32+2 padding, stride 33
shared float kernel_cache[64][9];  // Ko=64 kernels cached

layout(std430, binding = 0) readonly buffer InputBuffer {
    float input_data[];  // NHWC layout
};

layout(std430, binding = 1) readonly buffer KernelBuffer {
    float kernel_data[];  // Packed weights
};

layout(std430, binding = 2) buffer OutputBuffer {
    float output_data[];  // Output in NHWC
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
    uint tile_start_x = bx * 32u;
    uint tile_start_y = by * 32u;
    
    // Each thread computes 4×4 outputs
    float acc[4][4];  // Simplified from [64][4][4] for ES compatibility
    
    // Initialize accumulators
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            acc[i][j] = 0.0;
        }
    }
    
    // Process Ko output channels in blocks
    for (int ko_base = 0; ko_base < min(64, out_channels); ko_base++) {
        
        // Process all input channels in blocks
        for (int ci_base = 0; ci_base < in_channels; ci_base += 4) {
            
            // Stage 1: Cooperatively load 34×34 input tile (with padding for 3×3 kernel)
            barrier();
            
            // Each thread loads multiple elements to fill the tile
            uint elements_per_thread = (34u * 34u + 255u) / 256u;  // Ceiling division
            for (uint elem = 0u; elem < elements_per_thread; elem++) {
                uint linear_id = ty * 32u + tx + elem * 256u;
                if (linear_id < 34u * 34u) {
                    uint tile_y = linear_id / 34u;
                    uint tile_x = linear_id % 34u;
                    
                    // Global coordinates (with padding offset)
                    int global_y = int(tile_start_y + tile_y) - 1;
                    int global_x = int(tile_start_x + tile_x) - 1;
                    
                    if (global_y >= 0 && global_y < height && 
                        global_x >= 0 && global_x < width) {
                        // NHWC layout: [batch][height][width][channels]
                        uint idx = uint((batch_id * uint(height) + uint(global_y)) * uint(width) + uint(global_x)) * uint(in_channels) + uint(ci_base);
                        input_tile[tile_y][tile_x] = input_data[idx];
                    } else {
                        input_tile[tile_y][tile_x] = 0.0;  // Padding
                    }
                }
            }
            
            // Load kernel weights for current ko output
            if (ty < 2u) {  // First 64 threads load kernels
                uint ko_thread = ty * 32u + tx;
                if (ko_thread < 64u && ko_base + int(ko_thread) < out_channels) {
                    for (int k = 0; k < 9; k++) {
                        // 3×3 kernel weights for this output channel
                        uint kernel_idx = uint((ko_base + int(ko_thread)) * in_channels + ci_base) * 9u + uint(k);
                        kernel_cache[ko_thread][k] = kernel_data[kernel_idx];
                    }
                }
            }
            
            barrier();
            
            // Stage 2: Compute phase - each thread handles 4×4 outputs for current ko
            // Load kernel weights for this output channel
            float k00 = kernel_cache[ko_base][0];
            float k01 = kernel_cache[ko_base][1];
            float k02 = kernel_cache[ko_base][2];
            float k10 = kernel_cache[ko_base][3];
            float k11 = kernel_cache[ko_base][4];
            float k12 = kernel_cache[ko_base][5];
            float k20 = kernel_cache[ko_base][6];
            float k21 = kernel_cache[ko_base][7];
            float k22 = kernel_cache[ko_base][8];
            
            // Compute 4×4 outputs with full 3×3 convolution
            for (int oy = 0; oy < 4; oy++) {
                uint sy = ty * 4u + uint(oy) + 1u;  // +1 for padding offset
                
                for (int ox = 0; ox < 4; ox++) {
                    uint sx = tx + uint(ox) * 8u + 1u;  // Strided by 8, +1 for padding
                    
                    if (sx < 33u && sy < 33u) {  // Bounds check
                        float sum = 0.0;
                        
                        // 3×3 convolution kernel
                        sum += input_tile[sy-1u][sx-1u] * k00;
                        sum += input_tile[sy-1u][sx   ] * k01;
                        sum += input_tile[sy-1u][sx+1u] * k02;
                        sum += input_tile[sy   ][sx-1u] * k10;
                        sum += input_tile[sy   ][sx   ] * k11;
                        sum += input_tile[sy   ][sx+1u] * k12;
                        sum += input_tile[sy+1u][sx-1u] * k20;
                        sum += input_tile[sy+1u][sx   ] * k21;
                        sum += input_tile[sy+1u][sx+1u] * k22;
                        
                        acc[oy][ox] += sum;
                    }
                }
            }
        }
        
        // Stage 3: Write outputs for current ko
        barrier();
        
        // Each thread writes its 4×4 outputs for current output channel
        for (int oy = 0; oy < 4; oy++) {
            uint global_y = tile_start_y + ty * 4u + uint(oy);
            
            if (global_y < uint(height)) {
                for (int ox = 0; ox < 4; ox++) {
                    uint global_x = tile_start_x + tx + uint(ox) * 8u;
                    
                    if (global_x < uint(width) && ko_base < out_channels) {
                        // NHWC layout write
                        uint out_idx = uint((batch_id * uint(height) + global_y) * uint(width) + global_x) * uint(out_channels) + uint(ko_base);
                        output_data[out_idx] = acc[oy][ox];
                    }
                }
            }
        }
        
        // Reset accumulators for next ko
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                acc[i][j] = 0.0;
            }
        }
    }
}