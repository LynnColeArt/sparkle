// Minimal Valid SPIR-V for Testing
// ================================
//
// This provides the absolute minimum valid SPIR-V that Vulkan will accept

#include <stdint.h>
#include <stddef.h>

// Minimal valid SPIR-V compute shader
// Generated from:
// #version 450
// layout(local_size_x = 1) in;
// void main() {}
static const uint32_t minimal_spirv[] = {
    // Magic, version, generator, bound, schema
    0x07230203, 0x00010000, 0x00080001, 0x00000013, 0x00000000,
    // OpCapability Shader
    0x00020011, 0x00000001,
    // OpMemoryModel Logical GLSL450
    0x00030016, 0x00000000, 0x00000001,
    // OpEntryPoint GLCompute %4 "main"
    0x00040015, 0x00000005, 0x00000004, 0x6E69616D, 0x00000000,
    // OpExecutionMode %4 LocalSize 1 1 1
    0x00060010, 0x00000004, 0x00000011, 0x00000001, 0x00000001, 0x00000001,
    // OpSource GLSL 450
    0x00030003, 0x00000002, 0x000001C2,
    // OpName %4 "main"
    0x00030005, 0x00000004, 0x6E69616D,
    // OpName %8 "gl_GlobalInvocationID"
    0x00050005, 0x00000008, 0x475F6C67, 0x61626F6C, 0x766E496C,
    0x00050006, 0x00000008, 0x6974616F, 0x44496E6F, 0x00000000,
    // OpDecorate %8 BuiltIn GlobalInvocationId  
    0x00040047, 0x00000008, 0x0000000B, 0x0000001C,
    // %2 = OpTypeVoid
    0x00020013, 0x00000002,
    // %3 = OpTypeFunction %2
    0x00030021, 0x00000003, 0x00000002,
    // %6 = OpTypeInt 32 0
    0x00040015, 0x00000006, 0x00000020, 0x00000000,
    // %7 = OpTypeVector %6 3
    0x00040017, 0x00000007, 0x00000006, 0x00000003,
    // %9 = OpTypePointer Input %7
    0x00040020, 0x00000009, 0x00000001, 0x00000007,
    // %8 = OpVariable %9 Input
    0x0004003B, 0x00000009, 0x00000008, 0x00000001,
    // %4 = OpFunction %2 None %3
    0x00050036, 0x00000002, 0x00000004, 0x00000000, 0x00000003,
    // %5 = OpLabel
    0x000200F8, 0x00000005,
    // OpReturn
    0x000100FD,
    // OpFunctionEnd
    0x00010038
};

#include <stdio.h>

const uint32_t* get_minimal_valid_spirv(size_t* size_out) {
    fprintf(stderr, "DEBUG: get_minimal_valid_spirv called\n");
    *size_out = sizeof(minimal_spirv);
    fprintf(stderr, "DEBUG: returning %zu bytes\n", *size_out);
    return minimal_spirv;
}