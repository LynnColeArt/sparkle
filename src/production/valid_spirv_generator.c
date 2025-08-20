// Valid SPIR-V Generator for Vulkan
// =================================
//
// Generates a minimal but valid SPIR-V compute shader
// that Vulkan can accept without segfaulting

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

// SPIR-V opcodes we need
#define OpCapability          17
#define OpExtInstImport       12
#define OpMemoryModel         14
#define OpEntryPoint          15
#define OpExecutionMode       16
#define OpSource              3
#define OpName                5
#define OpDecorate            71
#define OpTypeVoid            19
#define OpTypeBool            20
#define OpTypeInt             21
#define OpTypeFloat           22
#define OpTypeVector          23
#define OpTypeArray           28
#define OpTypeRuntimeArray    29
#define OpTypeStruct          30
#define OpTypePointer         32
#define OpTypeFunction        33
#define OpConstant            43
#define OpConstantComposite   44
#define OpFunction            54
#define OpFunctionEnd         56
#define OpLabel               248
#define OpReturn              253
#define OpVariable            59
#define OpLoad                61
#define OpStore               62
#define OpAccessChain         65

// Decorations
#define DecorationDescriptorSet  34
#define DecorationBinding        33
#define DecorationBuiltIn        11
#define DecorationArrayStride    6
#define DecorationBlock          2

// Built-ins
#define BuiltInGlobalInvocationId  28

// Storage classes
#define StorageClassUniform        2
#define StorageClassInput          1
#define StorageClassStorageBuffer  12

// Create a minimal valid compute shader
uint32_t* create_minimal_compute_spirv(size_t* size_out) {
    // Allocate buffer for SPIR-V (we'll resize as needed)
    size_t capacity = 1024;
    uint32_t* spirv = malloc(capacity * sizeof(uint32_t));
    size_t pos = 0;
    
    // Helper to add words
    #define ADD_WORD(w) do { \
        if (pos >= capacity) { \
            capacity *= 2; \
            spirv = realloc(spirv, capacity * sizeof(uint32_t)); \
        } \
        spirv[pos++] = (w); \
    } while(0)
    
    // Header
    ADD_WORD(0x07230203);  // Magic
    ADD_WORD(0x00010000);  // Version 1.0
    ADD_WORD(0x00000000);  // Generator
    ADD_WORD(0);           // Bound (will update later)
    ADD_WORD(0x00000000);  // Schema
    
    size_t bound_pos = 3;  // Remember where bound is
    uint32_t next_id = 1;
    
    // IDs we'll use
    uint32_t id_void = next_id++;
    uint32_t id_void_func = next_id++;
    uint32_t id_main = next_id++;
    uint32_t id_label = next_id++;
    uint32_t id_glsl_std = next_id++;
    uint32_t id_uint = next_id++;
    uint32_t id_uvec3 = next_id++;
    uint32_t id_ptr_input_uvec3 = next_id++;
    uint32_t id_global_invocation = next_id++;
    uint32_t id_float = next_id++;
    uint32_t id_float_array = next_id++;
    uint32_t id_buffer_struct = next_id++;
    uint32_t id_ptr_buffer = next_id++;
    uint32_t id_input_var = next_id++;
    uint32_t id_weights_var = next_id++;
    uint32_t id_output_var = next_id++;
    
    // Update bound
    spirv[bound_pos] = next_id;
    
    // OpCapability Shader
    ADD_WORD((2 << 16) | OpCapability);
    ADD_WORD(1);  // Shader
    
    // OpExtInstImport "GLSL.std.450"
    ADD_WORD((6 << 16) | OpExtInstImport);
    ADD_WORD(id_glsl_std);
    ADD_WORD(0x534C4C47);  // "GLSL"
    ADD_WORD(0x6474732E);  // ".std"
    ADD_WORD(0x3035342E);  // ".450"
    ADD_WORD(0x00000000);  // null terminator
    
    // OpMemoryModel Logical GLSL450
    ADD_WORD((3 << 16) | OpMemoryModel);
    ADD_WORD(0);  // Logical
    ADD_WORD(1);  // GLSL450
    
    // OpEntryPoint GLCompute %main "main" %global_invocation
    ADD_WORD((5 << 16) | OpEntryPoint);
    ADD_WORD(5);  // GLCompute
    ADD_WORD(id_main);
    ADD_WORD(0x6E69616D);  // "main"
    ADD_WORD(id_global_invocation);
    
    // OpExecutionMode %main LocalSize 16 16 1
    ADD_WORD((6 << 16) | OpExecutionMode);
    ADD_WORD(id_main);
    ADD_WORD(17);  // LocalSize
    ADD_WORD(16);
    ADD_WORD(16);
    ADD_WORD(1);
    
    // OpSource GLSL 450
    ADD_WORD((3 << 16) | OpSource);
    ADD_WORD(2);  // GLSL
    ADD_WORD(450);
    
    // OpName %main "main"
    ADD_WORD((3 << 16) | OpName);
    ADD_WORD(id_main);
    ADD_WORD(0x6E69616D);  // "main"
    
    // Decorations
    // OpDecorate %global_invocation BuiltIn GlobalInvocationId
    ADD_WORD((4 << 16) | OpDecorate);
    ADD_WORD(id_global_invocation);
    ADD_WORD(DecorationBuiltIn);
    ADD_WORD(BuiltInGlobalInvocationId);
    
    // OpDecorate %buffer_struct Block
    ADD_WORD((3 << 16) | OpDecorate);
    ADD_WORD(id_buffer_struct);
    ADD_WORD(DecorationBlock);
    
    // OpDecorate %float_array ArrayStride 4
    ADD_WORD((4 << 16) | OpDecorate);
    ADD_WORD(id_float_array);
    ADD_WORD(DecorationArrayStride);
    ADD_WORD(4);
    
    // OpDecorate %input_var DescriptorSet 0
    ADD_WORD((4 << 16) | OpDecorate);
    ADD_WORD(id_input_var);
    ADD_WORD(DecorationDescriptorSet);
    ADD_WORD(0);
    
    // OpDecorate %input_var Binding 0
    ADD_WORD((4 << 16) | OpDecorate);
    ADD_WORD(id_input_var);
    ADD_WORD(DecorationBinding);
    ADD_WORD(0);
    
    // OpDecorate %weights_var DescriptorSet 0
    ADD_WORD((4 << 16) | OpDecorate);
    ADD_WORD(id_weights_var);
    ADD_WORD(DecorationDescriptorSet);
    ADD_WORD(0);
    
    // OpDecorate %weights_var Binding 1
    ADD_WORD((4 << 16) | OpDecorate);
    ADD_WORD(id_weights_var);
    ADD_WORD(DecorationBinding);
    ADD_WORD(1);
    
    // OpDecorate %output_var DescriptorSet 0
    ADD_WORD((4 << 16) | OpDecorate);
    ADD_WORD(id_output_var);
    ADD_WORD(DecorationDescriptorSet);
    ADD_WORD(0);
    
    // OpDecorate %output_var Binding 2
    ADD_WORD((4 << 16) | OpDecorate);
    ADD_WORD(id_output_var);
    ADD_WORD(DecorationBinding);
    ADD_WORD(2);
    
    // Types
    // %void = OpTypeVoid
    ADD_WORD((2 << 16) | OpTypeVoid);
    ADD_WORD(id_void);
    
    // %void_func = OpTypeFunction %void
    ADD_WORD((3 << 16) | OpTypeFunction);
    ADD_WORD(id_void_func);
    ADD_WORD(id_void);
    
    // %uint = OpTypeInt 32 0
    ADD_WORD((4 << 16) | OpTypeInt);
    ADD_WORD(id_uint);
    ADD_WORD(32);
    ADD_WORD(0);
    
    // %uvec3 = OpTypeVector %uint 3
    ADD_WORD((4 << 16) | OpTypeVector);
    ADD_WORD(id_uvec3);
    ADD_WORD(id_uint);
    ADD_WORD(3);
    
    // %ptr_input_uvec3 = OpTypePointer Input %uvec3
    ADD_WORD((4 << 16) | OpTypePointer);
    ADD_WORD(id_ptr_input_uvec3);
    ADD_WORD(StorageClassInput);
    ADD_WORD(id_uvec3);
    
    // %float = OpTypeFloat 32
    ADD_WORD((3 << 16) | OpTypeFloat);
    ADD_WORD(id_float);
    ADD_WORD(32);
    
    // %float_array = OpTypeRuntimeArray %float
    ADD_WORD((3 << 16) | OpTypeRuntimeArray);
    ADD_WORD(id_float_array);
    ADD_WORD(id_float);
    
    // %buffer_struct = OpTypeStruct %float_array
    ADD_WORD((3 << 16) | OpTypeStruct);
    ADD_WORD(id_buffer_struct);
    ADD_WORD(id_float_array);
    
    // %ptr_buffer = OpTypePointer StorageBuffer %buffer_struct
    ADD_WORD((4 << 16) | OpTypePointer);
    ADD_WORD(id_ptr_buffer);
    ADD_WORD(StorageClassStorageBuffer);
    ADD_WORD(id_buffer_struct);
    
    // Variables
    // %global_invocation = OpVariable %ptr_input_uvec3 Input
    ADD_WORD((4 << 16) | OpVariable);
    ADD_WORD(id_ptr_input_uvec3);
    ADD_WORD(id_global_invocation);
    ADD_WORD(StorageClassInput);
    
    // %input_var = OpVariable %ptr_buffer StorageBuffer
    ADD_WORD((4 << 16) | OpVariable);
    ADD_WORD(id_ptr_buffer);
    ADD_WORD(id_input_var);
    ADD_WORD(StorageClassStorageBuffer);
    
    // %weights_var = OpVariable %ptr_buffer StorageBuffer
    ADD_WORD((4 << 16) | OpVariable);
    ADD_WORD(id_ptr_buffer);
    ADD_WORD(id_weights_var);
    ADD_WORD(StorageClassStorageBuffer);
    
    // %output_var = OpVariable %ptr_buffer StorageBuffer
    ADD_WORD((4 << 16) | OpVariable);
    ADD_WORD(id_ptr_buffer);
    ADD_WORD(id_output_var);
    ADD_WORD(StorageClassStorageBuffer);
    
    // Function
    // %main = OpFunction %void None %void_func
    ADD_WORD((5 << 16) | OpFunction);
    ADD_WORD(id_void);
    ADD_WORD(id_main);
    ADD_WORD(0);  // None
    ADD_WORD(id_void_func);
    
    // %label = OpLabel
    ADD_WORD((2 << 16) | OpLabel);
    ADD_WORD(id_label);
    
    // OpReturn
    ADD_WORD((1 << 16) | OpReturn);
    
    // OpFunctionEnd
    ADD_WORD((1 << 16) | OpFunctionEnd);
    
    #undef ADD_WORD
    
    *size_out = pos * sizeof(uint32_t);
    return spirv;
}

// Get valid SPIR-V bytecode
const uint32_t* get_valid_conv2d_spirv(size_t* size_out) {
    static uint32_t* cached_spirv = NULL;
    static size_t cached_size = 0;
    
    if (!cached_spirv) {
        cached_spirv = create_minimal_compute_spirv(&cached_size);
    }
    
    *size_out = cached_size;
    return cached_spirv;
}