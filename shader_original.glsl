#version 450
#extension GL_ARB_compute_shader : enable
#extension GL_ARB_shader_storage_buffer_object : enable

#define TILE_SIZE 16
layout(local_size_x = 16, local_size_y = 16, local_size_z = 1) in;

// Input buffer - im2col transformed
layout(std430, binding = 0) readonly buffer InputBuffer {
    float input[];
};

// Kernel weights
layout(std430, binding = 1) readonly buffer KernelBuffer {
    float kernel[];
};

// Output buffer
layout(std430, binding = 2) writeonly buffer OutputBuffer {
    float output[];
};

// Convolution dimensions
uniform int M; // Output height * width
uniform int N; // Output channels
uniform int K; // Kernel height * width * input channels

// Shared memory tiles for cooperative loading
shared float tile_A[TILE_SIZE][TILE_SIZE + 1]; // +1 to avoid bank conflicts
shared float tile_B[TILE_SIZE][TILE_SIZE + 1];

void main() {
    // Global thread indices
    int global_row = int(gl_GlobalInvocationID.y);
    int global_col = int(gl_GlobalInvocationID.x);

    // Local thread indices within workgroup
    int local_row = int(gl_LocalInvocationID.y);
    int local_col = int(gl_LocalInvocationID.x);

    // Check bounds
    if (global_row >= M || global_col >= N) return;

    // Accumulator for this output element
    float acc = 0.0;

    // Number of tiles needed
    int num_tiles = (K + TILE_SIZE - 1) / TILE_SIZE;

    // Tile-based matrix multiplication
    for (int tile = 0; tile < num_tiles; tile++) {
        // Collaborative loading of tiles
        int tile_k = tile * TILE_SIZE;

        // Load tile from input (A matrix)
        int a_row = global_row;
        int a_col = tile_k + local_col;
        if (a_row < M && a_col < K) {
            tile_A[local_row][local_col] = input[a_row * K + a_col];
        } else {
            tile_A[local_row][local_col] = 0.0;
        }

        // Load tile from kernel (B matrix)
        int b_row = tile_k + local_row;
        int b_col = global_col;
        if (b_row < K && b_col < N) {
            tile_B[local_row][local_col] = kernel[b_row * N + b_col];
        } else {
            tile_B[local_row][local_col] = 0.0;
        }

        // Synchronize to ensure all threads have loaded their data
        barrier();

        // Compute partial dot product for this tile
        // Unrolled for TILE_SIZE = 16
        #pragma unroll
        for (int k = 0; k < TILE_SIZE; k++) {
            acc += tile_A[local_row][k] * tile_B[k][local_col];
        }

        // Synchronize before loading next tile
        barrier();
    }

    // Write result
    output[global_row * N + global_col] = acc;
}

