#version 450

// EXTREME optimization for NVIDIA A4500
// Using every trick: shared memory, register blocking, prefetching, unrolling

layout(local_size_x = 32, local_size_y = 4, local_size_z = 1) in;

layout(std430, binding = 0) readonly buffer InputBuffer {
    float input_data[];
};

layout(std430, binding = 1) readonly buffer KernelBuffer {
    float kernel_data[];
};

layout(std430, binding = 2) writeonly buffer OutputBuffer {
    float output_data[];
};

uniform int batch;
uniform int in_channels;
uniform int out_channels;
uniform int height;
uniform int width;
uniform int kernel_h;
uniform int kernel_w;
uniform int out_height;
uniform int out_width;

// Maximum shared memory utilization
// Double buffering for input and kernel
shared float smem_input[2][34][34];  // Two 34x34 tiles
shared float smem_kernel[2][32][9];  // Two sets of 32 output channels x 9 kernel elements

void main() {
    const uint tid = gl_LocalInvocationID.y * 32 + gl_LocalInvocationID.x;
    const uint warp_id = tid / 32;
    const uint lane_id = tid % 32;
    
    // Output position for this work group
    const uint group_x = gl_WorkGroupID.x;
    const uint group_y = gl_WorkGroupID.y;
    const uint base_out_c = gl_WorkGroupID.z * 32; // Process 32 output channels
    
    // Each warp processes 8x32 outputs, each thread handles 1x8 outputs
    const uint warp_out_y = group_y * 32 + warp_id * 8;
    const uint thread_out_x = group_x * 32 + lane_id;
    
    // Accumulator registers - each thread accumulates 8 spatial x 8 channel outputs
    float acc[8][8];
    for (int i = 0; i < 8; ++i) {
        for (int j = 0; j < 8; ++j) {
            acc[i][j] = 0.0;
        }
    }
    
    // Double buffer index
    int buf = 0;
    
    // Prefetch first tile
    if (tid < 34 * 34 / 4) {
        for (int i = 0; i < 4; ++i) {
            uint elem = tid * 4 + i;
            uint ty = elem / 34;
            uint tx = elem % 34;
            if (ty < 34 && tx < 34) {
                int iy = int(group_y * 32 + ty) - 1;
                int ix = int(group_x * 32 + tx) - 1;
                float val = 0.0;
                if (iy >= 0 && iy < height && ix >= 0 && ix < width) {
                    val = input_data[iy * width + ix];
                }
                smem_input[0][ty][tx] = val;
            }
        }
    }
    
    // Main loop - process all input channels
    for (int ic = 0; ic < in_channels; ++ic) {
        
        // Process each kernel position
        for (int ky = 0; ky < 3; ++ky) {
            for (int kx = 0; kx < 3; ++kx) {
                
                barrier();
                
                // === Async prefetch next tile while computing ===
                int next_buf = 1 - buf;
                int next_ic = ic;
                int next_ky = ky;
                int next_kx = kx + 1;
                if (next_kx >= 3) {
                    next_kx = 0;
                    next_ky++;
                    if (next_ky >= 3) {
                        next_ky = 0;
                        next_ic++;
                    }
                }
                
                // Prefetch next input tile
                if (next_ic < in_channels && tid < 34 * 34 / 4) {
                    for (int i = 0; i < 4; ++i) {
                        uint elem = tid * 4 + i;
                        uint ty = elem / 34;
                        uint tx = elem % 34;
                        if (ty < 34 && tx < 34) {
                            int iy = int(group_y * 32 + ty) + next_ky - 1;
                            int ix = int(group_x * 32 + tx) + next_kx - 1;
                            float val = 0.0;
                            if (iy >= 0 && iy < height && ix >= 0 && ix < width) {
                                val = input_data[((0 * in_channels + next_ic) * height + iy) * width + ix];
                            }
                            smem_input[next_buf][ty][tx] = val;
                        }
                    }
                }
                
                // Load kernel weights for 32 output channels
                if (tid < 32 * 9 / 4) {
                    for (int i = 0; i < 4; ++i) {
                        uint elem = tid * 4 + i;
                        uint oc_off = elem / 9;
                        uint k_elem = elem % 9;
                        if (oc_off < 32 && base_out_c + oc_off < out_channels) {
                            uint idx = (((base_out_c + oc_off) * in_channels + ic) * 3 + (k_elem / 3)) * 3 + (k_elem % 3);
                            smem_kernel[buf][oc_off][k_elem] = kernel_data[idx];
                        }
                    }
                }
                
                barrier();
                
                // === Compute phase with aggressive unrolling ===
                
                // Process 8 output channels
                #pragma unroll 8
                for (int oc = 0; oc < 8; ++oc) {
                    float k0 = smem_kernel[buf][oc * 4][ky * 3 + kx];
                    float k1 = smem_kernel[buf][oc * 4 + 1][ky * 3 + kx];
                    float k2 = smem_kernel[buf][oc * 4 + 2][ky * 3 + kx];
                    float k3 = smem_kernel[buf][oc * 4 + 3][ky * 3 + kx];
                    
                    // Process 8 spatial positions
                    #pragma unroll 8
                    for (int sy = 0; sy < 8; ++sy) {
                        uint iy = warp_id * 8 + sy + ky;
                        uint ix = lane_id + kx;
                        
                        float input_val = smem_input[buf][iy][ix];
                        
                        // Accumulate for 4 channels at once
                        acc[sy][oc] += input_val * (
                            (oc % 4 == 0) ? k0 :
                            (oc % 4 == 1) ? k1 :
                            (oc % 4 == 2) ? k2 : k3
                        );
                    }
                }
                
                // Switch buffers
                buf = next_buf;
            }
        }
    }
    
    // === Write outputs with coalescing ===
    for (int oc = 0; oc < 8; ++oc) {
        uint out_c = base_out_c + oc * 4 + warp_id;
        if (out_c < out_channels) {
            for (int sy = 0; sy < 8; ++sy) {
                uint oy = warp_out_y + sy;
                uint ox = thread_out_x;
                
                if (oy < out_height && ox < out_width) {
                    uint idx = ((0 * out_channels + out_c) * out_height + oy) * out_width + ox;
                    output_data[idx] = acc[sy][oc];
                }
            }
        }
    }
}