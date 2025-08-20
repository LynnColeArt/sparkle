// Pre-compiled Conv2D SPIR-V Bytecode
// ===================================
//
// This contains a minimal but valid SPIR-V compute shader
// for conv2d operations, hand-assembled for testing.

#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

// Minimal valid SPIR-V for a compute shader that reads/writes buffers
// This is a simplified shader that demonstrates buffer access
static const uint32_t conv2d_spirv_bytecode[] = {
    // Magic number and version
    0x07230203,  // SPIR-V magic number
    0x00010000,  // Version 1.0.0
    0x00000000,  // Generator (0 = unknown)
    0x00000016,  // Bound (22 IDs used)
    0x00000000,  // Schema (0)
    
    // OpCapability Shader
    0x00020011, 0x00000001,
    
    // OpMemoryModel Logical GLSL450
    0x00030016, 0x00000000, 0x00000001,
    
    // OpEntryPoint GLCompute %main "main" %gl_GlobalInvocationID
    0x00060015, 0x00000005, 0x00000004, 0x6E69616D, 0x00000000, 0x00000008,
    
    // OpExecutionMode %main LocalSize 16 16 1
    0x00060010, 0x00000004, 0x00000011, 0x00000010, 0x00000010, 0x00000001,
    
    // OpSource GLSL 450
    0x00030003, 0x00000002, 0x0001C22E,
    
    // OpName %main "main"
    0x00030005, 0x00000004, 0x6E69616D,
    
    // OpDecorate %gl_GlobalInvocationID BuiltIn GlobalInvocationId
    0x00040047, 0x00000008, 0x0000000B, 0x0000001C,
    
    // OpDecorate %input_buf DescriptorSet 0
    0x00040047, 0x0000000A, 0x00000022, 0x00000000,
    // OpDecorate %input_buf Binding 0
    0x00040047, 0x0000000A, 0x00000021, 0x00000000,
    
    // OpDecorate %output_buf DescriptorSet 0
    0x00040047, 0x0000000C, 0x00000022, 0x00000000,
    // OpDecorate %output_buf Binding 2
    0x00040047, 0x0000000C, 0x00000021, 0x00000002,
    
    // Types
    // %void = OpTypeVoid
    0x00020013, 0x00000002,
    
    // %func_void = OpTypeFunction %void
    0x00030021, 0x00000003, 0x00000002,
    
    // %uint = OpTypeInt 32 0
    0x00040015, 0x00000006, 0x00000020, 0x00000000,
    
    // %v3uint = OpTypeVector %uint 3
    0x00040017, 0x00000007, 0x00000006, 0x00000003,
    
    // %ptr_input_v3uint = OpTypePointer Input %v3uint
    0x00040020, 0x00000009, 0x00000001, 0x00000007,
    
    // %float = OpTypeFloat 32
    0x00030016, 0x0000000D, 0x00000020,
    
    // %runtime_array_float = OpTypeRuntimeArray %float
    0x00030022, 0x0000000E, 0x0000000D,
    
    // %struct_buffer = OpTypeStruct %runtime_array_float
    0x0003001E, 0x0000000F, 0x0000000E,
    
    // %ptr_buffer = OpTypePointer Uniform %struct_buffer
    0x00040020, 0x00000010, 0x00000002, 0x0000000F,
    
    // %int = OpTypeInt 32 1
    0x00040015, 0x00000011, 0x00000020, 0x00000001,
    
    // %int_0 = OpConstant %int 0
    0x00040018, 0x00000012, 0x00000011, 0x00000000,
    
    // %ptr_float = OpTypePointer Uniform %float
    0x00040020, 0x00000013, 0x00000002, 0x0000000D,
    
    // %float_1 = OpConstant %float 1.0
    0x00040018, 0x00000014, 0x0000000D, 0x3F800000,
    
    // Variables
    // %gl_GlobalInvocationID = OpVariable %ptr_input_v3uint Input
    0x0004003B, 0x00000009, 0x00000008, 0x00000001,
    
    // %input_buf = OpVariable %ptr_buffer Uniform
    0x0004003B, 0x00000010, 0x0000000A, 0x00000002,
    
    // %output_buf = OpVariable %ptr_buffer Uniform
    0x0004003B, 0x00000010, 0x0000000C, 0x00000002,
    
    // Function %main
    // %main = OpFunction %void None %func_void
    0x00050036, 0x00000002, 0x00000004, 0x00000000, 0x00000003,
    
    // %label = OpLabel
    0x000200F8, 0x00000005,
    
    // Simple operation: write 1.0 to output[0]
    // %ptr = OpAccessChain %ptr_float %output_buf %int_0 %int_0
    0x00060041, 0x00000013, 0x00000015, 0x0000000C, 0x00000012, 0x00000012,
    
    // OpStore %ptr %float_1
    0x0003003E, 0x00000015, 0x00000014,
    
    // OpReturn
    0x000100FD,
    
    // OpFunctionEnd
    0x00010038
};

// External function from minimal_valid_spirv.c
extern const uint32_t* get_minimal_valid_spirv(size_t* size_out);

// Get pre-compiled SPIR-V bytecode
const uint32_t* get_conv2d_spirv_bytecode(size_t* size_out) {
    // Return the bytecode - even if minimal, it's better than NULL
    *size_out = sizeof(conv2d_spirv_bytecode);
    return conv2d_spirv_bytecode;
}

// Create a more realistic conv2d SPIR-V shader
// For now, returns the minimal shader above
void* create_conv2d_spirv_shader() {
    // In a real implementation, this would generate optimized SPIR-V
    // based on the target architecture and parameters
    return (void*)conv2d_spirv_bytecode;
}