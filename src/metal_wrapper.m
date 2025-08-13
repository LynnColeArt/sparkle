// Metal C wrapper for Fortran interop
// Bridges Objective-C Metal API to C functions

#import <Metal/Metal.h>
#import <Foundation/Foundation.h>
#include <string.h>
#include <stdio.h>

// Context structure holding Metal objects
typedef struct {
    id<MTLDevice> device;
    id<MTLCommandQueue> queue;
    id<MTLLibrary> library;
} MetalContext;

// Check if Metal is available
int metal_available() {
    NSArray<id<MTLDevice>>* devices = MTLCopyAllDevices();
    int available = (devices.count > 0) ? 1 : 0;
    [devices release];
    return available;
}

// Create Metal context
void* metal_create_context() {
    @autoreleasepool {
        // Get default Metal device
        id<MTLDevice> device = MTLCreateSystemDefaultDevice();
        if (!device) {
            printf("Failed to create Metal device\n");
            return NULL;
        }
        
        // Create command queue
        id<MTLCommandQueue> queue = [device newCommandQueue];
        if (!queue) {
            printf("Failed to create command queue\n");
            [device release];
            return NULL;
        }
        
        // Allocate context
        MetalContext* ctx = (MetalContext*)malloc(sizeof(MetalContext));
        ctx->device = [device retain];
        ctx->queue = [queue retain];
        ctx->library = nil;
        
        return ctx;
    }
}

// Get device name
void metal_get_device_name(void* context, char* name, int len) {
    MetalContext* ctx = (MetalContext*)context;
    if (!ctx || !ctx->device) return;
    
    @autoreleasepool {
        NSString* deviceName = [ctx->device name];
        strncpy(name, [deviceName UTF8String], len - 1);
        name[len - 1] = '\0';
    }
}

// Create buffer
void* metal_create_buffer(void* context, size_t size) {
    MetalContext* ctx = (MetalContext*)context;
    if (!ctx || !ctx->device) return NULL;
    
    @autoreleasepool {
        id<MTLBuffer> buffer = [ctx->device newBufferWithLength:size
                                                        options:MTLResourceStorageModeShared];
        return [buffer retain];
    }
}

// Copy data to buffer
void metal_copy_to_buffer(void* buffer, const void* data, size_t size) {
    @autoreleasepool {
        id<MTLBuffer> mtlBuffer = (id<MTLBuffer>)buffer;
        memcpy([mtlBuffer contents], data, size);
    }
}

// Copy data from buffer
void metal_copy_from_buffer(void* buffer, void* data, size_t size) {
    @autoreleasepool {
        id<MTLBuffer> mtlBuffer = (id<MTLBuffer>)buffer;
        memcpy(data, [mtlBuffer contents], size);
    }
}

// Compile kernel from Metal source
void* metal_compile_kernel(void* context, const char* source, const char* function_name) {
    MetalContext* ctx = (MetalContext*)context;
    if (!ctx || !ctx->device) return NULL;
    
    @autoreleasepool {
        NSError* error = nil;
        
        // Create library from source
        NSString* sourceString = [NSString stringWithUTF8String:source];
        id<MTLLibrary> library = [ctx->device newLibraryWithSource:sourceString
                                                           options:nil
                                                             error:&error];
        if (error) {
            NSLog(@"Failed to compile Metal library: %@", error);
            return NULL;
        }
        
        // Get function from library
        NSString* funcName = [NSString stringWithUTF8String:function_name];
        id<MTLFunction> function = [library newFunctionWithName:funcName];
        if (!function) {
            NSLog(@"Failed to find function: %@", funcName);
            [library release];
            return NULL;
        }
        
        // Create compute pipeline
        id<MTLComputePipelineState> pipeline = [ctx->device newComputePipelineStateWithFunction:function
                                                                                           error:&error];
        if (error) {
            NSLog(@"Failed to create pipeline: %@", error);
            [function release];
            [library release];
            return NULL;
        }
        
        [function release];
        [library release];
        
        return [pipeline retain];
    }
}

// Dispatch kernel
void metal_dispatch_kernel(void* context, void* kernel,
                          void** buffers, int num_buffers,
                          const size_t* global_size, const size_t* local_size) {
    MetalContext* ctx = (MetalContext*)context;
    if (!ctx || !ctx->queue || !kernel) return;
    
    @autoreleasepool {
        id<MTLComputePipelineState> pipeline = (id<MTLComputePipelineState>)kernel;
        
        // Create command buffer and encoder
        id<MTLCommandBuffer> commandBuffer = [ctx->queue commandBuffer];
        id<MTLComputeCommandEncoder> encoder = [commandBuffer computeCommandEncoder];
        
        // Set pipeline
        [encoder setComputePipelineState:pipeline];
        
        // Set buffers
        for (int i = 0; i < num_buffers; i++) {
            id<MTLBuffer> buffer = (id<MTLBuffer>)buffers[i];
            [encoder setBuffer:buffer offset:0 atIndex:i];
        }
        
        // Calculate thread groups
        MTLSize threadsPerThreadgroup = MTLSizeMake(local_size[0], local_size[1], local_size[2]);
        MTLSize threadgroupCount = MTLSizeMake(
            (global_size[0] + local_size[0] - 1) / local_size[0],
            (global_size[1] + local_size[1] - 1) / local_size[1],
            (global_size[2] + local_size[2] - 1) / local_size[2]
        );
        
        // Dispatch
        [encoder dispatchThreadgroups:threadgroupCount
                threadsPerThreadgroup:threadsPerThreadgroup];
        
        // Finish encoding and commit
        [encoder endEncoding];
        [commandBuffer commit];
        [commandBuffer waitUntilCompleted];
    }
}

// Wait for GPU idle
void metal_wait_idle(void* context) {
    // Commands are synchronous in our implementation
    // (waitUntilCompleted in dispatch)
}

// Cleanup functions
void metal_destroy_buffer(void* buffer) {
    if (buffer) {
        @autoreleasepool {
            [(id<MTLBuffer>)buffer release];
        }
    }
}

void metal_destroy_kernel(void* kernel) {
    if (kernel) {
        @autoreleasepool {
            [(id<MTLComputePipelineState>)kernel release];
        }
    }
}

void metal_destroy_context(void* context) {
    MetalContext* ctx = (MetalContext*)context;
    if (ctx) {
        @autoreleasepool {
            [ctx->device release];
            [ctx->queue release];
            if (ctx->library) {
                [ctx->library release];
            }
        }
        free(ctx);
    }
}