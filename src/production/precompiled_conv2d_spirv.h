// Pre-compiled Conv2D SPIR-V Shader
// =================================
//
// This is a pre-compiled SPIR-V shader for conv2d to avoid runtime compilation
// Generated from the optimized GLSL compute shader

#ifndef PRECOMPILED_CONV2D_SPIRV_H
#define PRECOMPILED_CONV2D_SPIRV_H

#include <stdint.h>

// Simulated SPIR-V binary (minimal valid SPIR-V header)
// In production, this would be the actual compiled shader
static const uint32_t conv2d_spirv[] = {
    // SPIR-V Magic Number
    0x07230203,  // Magic number
    0x00010000,  // Version 1.0
    0x00000000,  // Generator (0 = unknown)
    0x00000001,  // Bound (1 ID)
    0x00000000,  // Schema (0)
    
    // OpCapability Shader
    0x00020011, 0x00000001,
    
    // OpMemoryModel Logical GLSL450
    0x00030016, 0x00000000, 0x00000001,
    
    // Note: This is a stub - real SPIR-V would be much larger
    // For testing, we'll detect this and use a fallback path
};

static const size_t conv2d_spirv_size = sizeof(conv2d_spirv);

// Function to check if we have a real SPIR-V compiler available
// Moved to glsl_to_spirv.c to avoid multiple definition issues

// Get pre-compiled SPIR-V data
const uint32_t* get_precompiled_conv2d_spirv(size_t* size_out) {
    *size_out = conv2d_spirv_size;
    return conv2d_spirv;
}

#endif // PRECOMPILED_CONV2D_SPIRV_H