#version 310 es
// Summit Kernel Ko=4: True parallel Ko processing optimized for ES 3.1
// Process 4 output channels simultaneously with register-optimized design

precision highp float;
precision highp int;

layout(local_size_x = 32, local_size_y = 8) in;  // 256 threads

// Shared memory - padded to avoid bank conflicts
shared float input_tile[34][33];   // 32+2 padding, stride 33
shared float kernel_cache[4][9];   // Only 4 Ko channels in registers at once

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

void main() {
    uint tx = gl_LocalInvocationID.x;  // 0-31
    uint ty = gl_LocalInvocationID.y;  // 0-7
    uint bx = gl_WorkGroupID.x;
    uint by = gl_WorkGroupID.y;
    uint batch_id = gl_WorkGroupID.z;
    
    uint tile_start_x = bx * 32u;
    uint tile_start_y = by * 32u;
    
    // Process output channels in blocks of 4 for optimal register usage
    for (int ko_base = 0; ko_base < out_channels; ko_base += 4) {
        
        // Accumulators for 4 output channels, 4×4 spatial outputs each
        // Total: 64 registers (4 Ko × 4×4 outputs) - fits in register budget
        float acc0[4][4];  // Ko+0
        float acc1[4][4];  // Ko+1
        float acc2[4][4];  // Ko+2
        float acc3[4][4];  // Ko+3
        
        // Initialize accumulators
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                acc0[i][j] = 0.0;
                acc1[i][j] = 0.0;
                acc2[i][j] = 0.0;
                acc3[i][j] = 0.0;
            }
        }
        
        // Process all input channels
        for (int ci = 0; ci < in_channels; ci++) {
            
            // Stage 1: Cooperatively load input tile
            barrier();
            
            uint elements_per_thread = (34u * 34u + 255u) / 256u;
            for (uint elem = 0u; elem < elements_per_thread; elem++) {
                uint linear_id = ty * 32u + tx + elem * 256u;
                if (linear_id < 34u * 34u) {
                    uint tile_y = linear_id / 34u;
                    uint tile_x = linear_id % 34u;
                    
                    int global_y = int(tile_start_y + tile_y) - 1;
                    int global_x = int(tile_start_x + tile_x) - 1;
                    
                    if (global_y >= 0 && global_y < height && 
                        global_x >= 0 && global_x < width) {
                        // NHWC: [batch][height][width][channels]
                        uint idx = (batch_id * uint(height) + uint(global_y)) * uint(width) + uint(global_x);
                        idx = idx * uint(in_channels) + uint(ci);
                        input_tile[tile_y][tile_x] = input_data[idx];
                    } else {
                        input_tile[tile_y][tile_x] = 0.0;
                    }
                }
            }
            
            // Stage 2: Load 4 Ko kernel weights into shared memory
            if (tx < 4u && ty == 0u) {  // First 4 threads of first row
                int ko = ko_base + int(tx);
                if (ko < out_channels) {
                    for (int k = 0; k < 9; k++) {
                        uint kernel_idx = uint(ko * in_channels + ci) * 9u + uint(k);
                        kernel_cache[tx][k] = kernel_data[kernel_idx];
                    }
                }
            }
            
            barrier();
            
            // Stage 3: Compute 4 Ko channels in parallel
            // Each thread processes 4×4 outputs for each of the 4 Ko channels
            for (int oy = 0; oy < 4; oy++) {
                uint sy = ty * 4u + uint(oy) + 1u;  // +1 for padding
                
                for (int ox = 0; ox < 4; ox++) {
                    uint sx = tx + uint(ox) * 8u + 1u;  // Strided access, +1 for padding
                    
                    if (sx < 33u && sy < 33u) {
                        // Load input values once for all Ko channels
                        float i00 = input_tile[sy-1u][sx-1u];
                        float i01 = input_tile[sy-1u][sx];
                        float i02 = input_tile[sy-1u][sx+1u];
                        float i10 = input_tile[sy][sx-1u];
                        float i11 = input_tile[sy][sx];
                        float i12 = input_tile[sy][sx+1u];
                        float i20 = input_tile[sy+1u][sx-1u];
                        float i21 = input_tile[sy+1u][sx];
                        float i22 = input_tile[sy+1u][sx+1u];
                        
                        // Process Ko+0 (vectorized 3×3 convolution)
                        float sum0 = i00 * kernel_cache[0][0];
                        sum0 += i01 * kernel_cache[0][1];
                        sum0 += i02 * kernel_cache[0][2];
                        sum0 += i10 * kernel_cache[0][3];
                        sum0 += i11 * kernel_cache[0][4];
                        sum0 += i12 * kernel_cache[0][5];
                        sum0 += i20 * kernel_cache[0][6];
                        sum0 += i21 * kernel_cache[0][7];
                        sum0 += i22 * kernel_cache[0][8];
                        acc0[oy][ox] += sum0;
                        
                        // Process Ko+1
                        if (ko_base + 1 < out_channels) {
                            float sum1 = i00 * kernel_cache[1][0];
                            sum1 += i01 * kernel_cache[1][1];
                            sum1 += i02 * kernel_cache[1][2];
                            sum1 += i10 * kernel_cache[1][3];
                            sum1 += i11 * kernel_cache[1][4];
                            sum1 += i12 * kernel_cache[1][5];
                            sum1 += i20 * kernel_cache[1][6];
                            sum1 += i21 * kernel_cache[1][7];
                            sum1 += i22 * kernel_cache[1][8];
                            acc1[oy][ox] += sum1;
                        }
                        
                        // Process Ko+2
                        if (ko_base + 2 < out_channels) {
                            float sum2 = i00 * kernel_cache[2][0];
                            sum2 += i01 * kernel_cache[2][1];
                            sum2 += i02 * kernel_cache[2][2];
                            sum2 += i10 * kernel_cache[2][3];
                            sum2 += i11 * kernel_cache[2][4];
                            sum2 += i12 * kernel_cache[2][5];
                            sum2 += i20 * kernel_cache[2][6];
                            sum2 += i21 * kernel_cache[2][7];
                            sum2 += i22 * kernel_cache[2][8];
                            acc2[oy][ox] += sum2;
                        }
                        
                        // Process Ko+3
                        if (ko_base + 3 < out_channels) {
                            float sum3 = i00 * kernel_cache[3][0];
                            sum3 += i01 * kernel_cache[3][1];
                            sum3 += i02 * kernel_cache[3][2];
                            sum3 += i10 * kernel_cache[3][3];
                            sum3 += i11 * kernel_cache[3][4];
                            sum3 += i12 * kernel_cache[3][5];
                            sum3 += i20 * kernel_cache[3][6];
                            sum3 += i21 * kernel_cache[3][7];
                            sum3 += i22 * kernel_cache[3][8];
                            acc3[oy][ox] += sum3;
                        }
                    }
                }
            }
        }
        
        // Stage 4: Write outputs for this Ko block
        barrier();
        
        for (int oy = 0; oy < 4; oy++) {
            uint global_y = tile_start_y + ty * 4u + uint(oy);
            
            if (global_y < uint(height)) {
                for (int ox = 0; ox < 4; ox++) {
                    uint global_x = tile_start_x + tx + uint(ox) * 8u;
                    
                    if (global_x < uint(width)) {
                        // Write Ko+0
                        if (ko_base < out_channels) {
                            uint out_idx = (batch_id * uint(height) + global_y) * uint(width) + global_x;
                            out_idx = out_idx * uint(out_channels) + uint(ko_base);
                            output_data[out_idx] = acc0[oy][ox];
                        }
                        
                        // Write Ko+1
                        if (ko_base + 1 < out_channels) {
                            uint out_idx = (batch_id * uint(height) + global_y) * uint(width) + global_x;
                            out_idx = out_idx * uint(out_channels) + uint(ko_base + 1);
                            output_data[out_idx] = acc1[oy][ox];
                        }
                        
                        // Write Ko+2
                        if (ko_base + 2 < out_channels) {
                            uint out_idx = (batch_id * uint(height) + global_y) * uint(width) + global_x;
                            out_idx = out_idx * uint(out_channels) + uint(ko_base + 2);
                            output_data[out_idx] = acc2[oy][ox];
                        }
                        
                        // Write Ko+3
                        if (ko_base + 3 < out_channels) {
                            uint out_idx = (batch_id * uint(height) + global_y) * uint(width) + global_x;
                            out_idx = out_idx * uint(out_channels) + uint(ko_base + 3);
                            output_data[out_idx] = acc3[oy][ox];
                        }
                    }
                }
            }
        }
    }
}