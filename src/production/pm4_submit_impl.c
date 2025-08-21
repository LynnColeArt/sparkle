// PM4 Real Submission Implementation
// ==================================
// Replace stub with actual ioctl-based GPU command submission
// This is pm4_submit_impl.c (renamed to avoid conflict with pm4_submit.f90)

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <stdarg.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <drm/drm.h>
#include <drm/amdgpu_drm.h>

#include "pm4_submit.h"

// Logging levels
enum {
    SP_LOG_TRACE = 0,
    SP_LOG_INFO  = 1,
    SP_LOG_WARN  = 2,
    SP_LOG_ERROR = 3
};

static int g_log_level = SP_LOG_INFO;

// Simple logger (no emojis!)
static void sp_log(int level, const char* fmt, ...) {
    if (level < g_log_level) return;
    
    const char* prefix[] = {"[TRACE]", "[INFO]", "[WARN]", "[ERROR]"};
    fprintf(stderr, "%s ", prefix[level]);
    
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
}

// Initialize PM4 context
sp_pm4_ctx* sp_pm4_init(const char* device_path) {
    sp_pm4_ctx* ctx = calloc(1, sizeof(sp_pm4_ctx));
    if (!ctx) {
        sp_log(SP_LOG_ERROR, "Failed to allocate PM4 context");
        return NULL;
    }
    
    // Open render node (default to renderD129 for iGPU)
    ctx->fd = open(device_path ? device_path : "/dev/dri/renderD129", O_RDWR);
    if (ctx->fd < 0) {
        sp_log(SP_LOG_ERROR, "Failed to open device: %s", strerror(errno));
        free(ctx);
        return NULL;
    }
    
    // Verify this is amdgpu driver
    struct drm_version version = {0};
    char name[64] = {0};
    version.name = name;
    version.name_len = sizeof(name);
    
    if (ioctl(ctx->fd, DRM_IOCTL_VERSION, &version) == 0) {
        sp_log(SP_LOG_TRACE, "DRM driver: %s", name);
        if (strcmp(name, "amdgpu") != 0) {
            sp_log(SP_LOG_ERROR, "Not an amdgpu device: %s", name);
            close(ctx->fd);
            free(ctx);
            return NULL;
        }
    }
    
    // Get device info
    struct drm_amdgpu_info request = {0};
    struct drm_amdgpu_info_device dev_info = {0};
    
    request.return_pointer = (uintptr_t)&dev_info;
    request.return_size = sizeof(dev_info);
    request.query = AMDGPU_INFO_DEV_INFO;
    
    if (ioctl(ctx->fd, DRM_IOCTL_AMDGPU_INFO, &request) < 0) {
        sp_log(SP_LOG_ERROR, "Failed to get device info: %s", strerror(errno));
        close(ctx->fd);
        free(ctx);
        return NULL;
    }
    
    ctx->device_id = dev_info.device_id;
    ctx->num_compute_rings = 1;  // TODO: Query actual ring count
    
    sp_log(SP_LOG_INFO, "PM4 context initialized on device 0x%04x", ctx->device_id);
    sp_log(SP_LOG_INFO, "Compute rings available: %d", ctx->num_compute_rings);
    sp_log(SP_LOG_TRACE, "Device fd: %d", ctx->fd);
    
    // Create GPU context
    union drm_amdgpu_ctx ctx_args = {0};
    ctx_args.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    
    if (ioctl(ctx->fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args) < 0) {
        sp_log(SP_LOG_ERROR, "Failed to create GPU context: %s", strerror(errno));
        close(ctx->fd);
        free(ctx);
        return NULL;
    }
    
    ctx->gpu_ctx_id = ctx_args.out.alloc.ctx_id;
    sp_log(SP_LOG_TRACE, "GPU context created: %u", ctx->gpu_ctx_id);
    
    return ctx;
}

// Cleanup PM4 context
void sp_pm4_cleanup(sp_pm4_ctx* ctx) {
    if (!ctx) return;
    
    // Destroy GPU context
    if (ctx->gpu_ctx_id) {
        union drm_amdgpu_ctx ctx_args = {0};
        ctx_args.in.op = AMDGPU_CTX_OP_FREE_CTX;
        ctx_args.in.ctx_id = ctx->gpu_ctx_id;
        ioctl(ctx->fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args);
    }
    
    if (ctx->fd >= 0) {
        close(ctx->fd);
    }
    
    sp_log(SP_LOG_TRACE, "PM4 context cleaned up");
    free(ctx);
}

// Allocate GPU buffer
sp_bo* sp_buffer_alloc(sp_pm4_ctx* ctx, size_t size, uint32_t flags) {
    if (!ctx) {
        sp_log(SP_LOG_ERROR, "NULL context passed to sp_buffer_alloc");
        return NULL;
    }
    
    sp_log(SP_LOG_TRACE, "sp_buffer_alloc called: size=%zu, flags=0x%x, fd=%d", 
           size, flags, ctx->fd);
    
    sp_bo* bo = calloc(1, sizeof(sp_bo));
    if (!bo) return NULL;
    
    // Align size to page
    size = (size + 4095) & ~4095;
    bo->size = size;
    bo->flags = flags;
    
    // GEM allocation
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = size;
    gem_args.in.alignment = 4096;
    
    // For now, use GTT for all allocations (works on both APU and dGPU)
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    if (!(flags & SP_BO_DEVICE_LOCAL)) {
        gem_args.in.domain_flags = AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;
    } else {
        gem_args.in.domain_flags = 0;
    }
    
    if (ioctl(ctx->fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args) < 0) {
        sp_log(SP_LOG_ERROR, "GEM_CREATE failed: %s (domains=0x%x, size=%zu)", 
               strerror(errno), gem_args.in.domains, size);
        free(bo);
        return NULL;
    }
    
    bo->handle = gem_args.out.handle;
    
    // Simple VA allocator - use fixed offset for now
    static uint64_t next_va = 0x800000000ULL;  // Start at 32GB
    bo->gpu_va = next_va;
    next_va += (size + 0xFFFF) & ~0xFFFF;  // Align to 64KB
    
    // Map buffer to GPU VA
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = bo->handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    
    // Set flags based on buffer type
    if (flags & SP_BO_HOST_VISIBLE) {
        // IB buffers need executable permission
        va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_EXECUTABLE;
    } else {
        // Data buffers need read/write
        va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE;
    }
    
    va_args.va_address = bo->gpu_va;
    va_args.offset_in_bo = 0;
    va_args.map_size = size;
    
    if (ioctl(ctx->fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args) < 0) {
        sp_log(SP_LOG_ERROR, "Failed to map VA: %s (va=0x%lx, size=%zu)", 
               strerror(errno), bo->gpu_va, size);
        // Don't free here - causes double free
        struct drm_gem_close close_args = {0};
        close_args.handle = bo->handle;
        ioctl(ctx->fd, DRM_IOCTL_GEM_CLOSE, &close_args);
        free(bo);
        return NULL;
    }
    
    // VA already set above
    
    // CPU map if host visible
    if (!(flags & SP_BO_DEVICE_LOCAL)) {
        union drm_amdgpu_gem_mmap mmap_args = {0};
        mmap_args.in.handle = bo->handle;
        
        if (ioctl(ctx->fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args) == 0) {
            bo->cpu_ptr = mmap(NULL, size, PROT_READ | PROT_WRITE, 
                              MAP_SHARED, ctx->fd, mmap_args.out.addr_ptr);
            if (bo->cpu_ptr == MAP_FAILED) {
                bo->cpu_ptr = NULL;
            } else {
                sp_log(SP_LOG_TRACE, "Mapped buffer: mmap_offset=0x%lx", 
                       mmap_args.out.addr_ptr);
            }
        }
    }
    
    sp_log(SP_LOG_TRACE, "Allocated buffer: size=%zu, gpu_va=0x%lx, flags=0x%x", 
           size, bo->gpu_va, flags);
    
    return bo;
}

// Free GPU buffer
void sp_buffer_free(sp_pm4_ctx* ctx, sp_bo* bo) {
    if (!ctx || !bo) return;
    
    // Unmap CPU
    if (bo->cpu_ptr) {
        munmap(bo->cpu_ptr, bo->size);
    }
    
    // Unmap GPU VA
    if (bo->gpu_va) {
        struct drm_amdgpu_gem_va va_args = {0};
        va_args.handle = bo->handle;
        va_args.operation = AMDGPU_VA_OP_UNMAP;
        va_args.va_address = bo->gpu_va;
        va_args.map_size = bo->size;
        ioctl(ctx->fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    }
    
    // Free GEM object
    if (bo->handle) {
        struct drm_gem_close close_args = {0};
        close_args.handle = bo->handle;
        ioctl(ctx->fd, DRM_IOCTL_GEM_CLOSE, &close_args);
    }
    
    sp_log(SP_LOG_TRACE, "Freed buffer: gpu_va=0x%lx", bo->gpu_va);
    free(bo);
}

// Submit indirect buffer with data buffer
int sp_submit_ib_with_bo(sp_pm4_ctx* ctx, sp_bo* ib_bo, uint32_t ib_size_dw, 
                         sp_bo* data_bo, sp_fence* out_fence) {
    if (!ctx || !ib_bo || !out_fence) return -EINVAL;
    
    sp_log(SP_LOG_TRACE, "sp_submit_ib_with_bo: ctx=%p, ib_bo=%p, size_dw=%u, data_bo=%p", 
           ctx, ib_bo, ib_size_dw, data_bo);
    
    // Create BO list with IB and data buffer
    uint32_t bo_list_handle = 0;
    union drm_amdgpu_bo_list bo_list_args = {0};
    
    // Include both IB and data buffer in BO list
    int num_bos = data_bo ? 2 : 1;
    struct drm_amdgpu_bo_list_entry bo_info[2] = {0};
    
    bo_info[0].bo_handle = ib_bo->handle;
    bo_info[0].bo_priority = 0;
    
    if (data_bo) {
        bo_info[1].bo_handle = data_bo->handle;
        bo_info[1].bo_priority = 0;
    }
    
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    bo_list_args.in.bo_number = num_bos;
    bo_list_args.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
    bo_list_args.in.bo_info_ptr = (uintptr_t)bo_info;
    
    sp_log(SP_LOG_TRACE, "Creating BO list with %d buffers: IB=%u, data=%u", 
           num_bos, ib_bo->handle, data_bo ? data_bo->handle : 0);
    
    if (ioctl(ctx->fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args) < 0) {
        sp_log(SP_LOG_WARN, "Failed to create BO list: %s. Trying without...", 
               strerror(errno));
        bo_list_handle = 0;
    } else {
        bo_list_handle = bo_list_args.out.list_handle;
        sp_log(SP_LOG_TRACE, "Created BO list: handle=%u", bo_list_handle);
    }
    
    // Build CS submission - allocate on heap per Mini's suggestion
    struct drm_amdgpu_cs_chunk *chunks = calloc(1, sizeof(struct drm_amdgpu_cs_chunk));
    struct drm_amdgpu_cs_chunk_ib *ib_data = calloc(1, sizeof(struct drm_amdgpu_cs_chunk_ib));
    uint64_t *chunk_ptrs = calloc(1, sizeof(uint64_t));  // CRITICAL: Array of pointers!
    
    if (!chunks || !ib_data || !chunk_ptrs) {
        sp_log(SP_LOG_ERROR, "Failed to allocate CS structures");
        if (bo_list_handle > 0) {
            bo_list_args.in.operation = AMDGPU_BO_LIST_OP_DESTROY;
            bo_list_args.in.list_handle = bo_list_handle;
            ioctl(ctx->fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
        }
        free(chunks);
        free(ib_data);
        free(chunk_ptrs);
        return -ENOMEM;
    }
    
    // Fill IB info
    ib_data->_pad = 0;  // Explicit padding field
    ib_data->flags = 0;
    ib_data->va_start = ib_bo->gpu_va;
    ib_data->ib_bytes = ib_size_dw * 4;  // Size in BYTES, not dwords
    ib_data->ip_type = AMDGPU_HW_IP_COMPUTE;  // Use COMPUTE ring as Mini suggests
    ib_data->ip_instance = 0;
    ib_data->ring = 0;
    
    // CRITICAL: length_dw is the size of the STRUCT, not the IB!
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(struct drm_amdgpu_cs_chunk_ib) / 4;
    chunks[0].chunk_data = (uint64_t)(uintptr_t)ib_data;
    
    // CRITICAL FIX: Create array of pointers to chunks!
    chunk_ptrs[0] = (uint64_t)(uintptr_t)&chunks[0];
    
    sp_log(SP_LOG_TRACE, "Chunk setup: struct_size=%zu, length_dw=%u", 
           sizeof(struct drm_amdgpu_cs_chunk_ib), chunks[0].length_dw);
    
    union drm_amdgpu_cs cs_args = {0};
    cs_args.in.ctx_id = ctx->gpu_ctx_id;
    cs_args.in.bo_list_handle = bo_list_handle;
    cs_args.in.num_chunks = 1;
    cs_args.in.chunks = (uint64_t)(uintptr_t)chunk_ptrs;  // Pass pointer array!
    
    sp_log(SP_LOG_TRACE, "CS submit: ctx=%u, bo_list=%u, chunks=%u, ib_va=0x%lx, ib_bytes=%u",
           ctx->gpu_ctx_id, bo_list_handle, cs_args.in.num_chunks, 
           ib_data->va_start, ib_data->ib_bytes);
    sp_log(SP_LOG_TRACE, "  chunks ptr: %p, chunk[0] ptr: %p", 
           (void*)cs_args.in.chunks, chunks);
    sp_log(SP_LOG_TRACE, "  ib_data ptr: %p, chunk_ptrs[0]: 0x%lx", ib_data, chunk_ptrs[0]);
    sp_log(SP_LOG_TRACE, "  IP type: %u, instance: %u, ring: %u", 
           ib_data->ip_type, ib_data->ip_instance, ib_data->ring);
    
    int ret = ioctl(ctx->fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    
    // Cleanup BO list if we created one
    if (bo_list_handle > 0) {
        bo_list_args.in.operation = AMDGPU_BO_LIST_OP_DESTROY;
        bo_list_args.in.list_handle = bo_list_handle;
        ioctl(ctx->fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    }
    
    if (ret < 0) {
        sp_log(SP_LOG_ERROR, "CS submit failed: %s", strerror(errno));
        free(chunks);
        free(ib_data);
        free(chunk_ptrs);
        return -errno;
    }
    
    // Return fence info
    out_fence->ctx_id = ctx->gpu_ctx_id;
    out_fence->ip_type = AMDGPU_HW_IP_COMPUTE;  // Match the submission type
    out_fence->ring = 0;
    out_fence->fence = cs_args.out.handle;
    
    sp_log(SP_LOG_TRACE, "Submitted IB: %u dwords, fence=%lu", ib_size_dw, out_fence->fence);
    
    free(chunks);
    free(ib_data);
    free(chunk_ptrs);
    return 0;
}

// Submit indirect buffer (simple wrapper)
int sp_submit_ib(sp_pm4_ctx* ctx, sp_bo* ib_bo, uint32_t ib_size_dw, sp_fence* out_fence) {
    return sp_submit_ib_with_bo(ctx, ib_bo, ib_size_dw, NULL, out_fence);
}

// Wait for fence (blocking)
int sp_fence_wait(sp_pm4_ctx* ctx, sp_fence* fence, uint64_t timeout_ns) {
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = fence->fence;
    wait_args.in.ip_type = fence->ip_type;
    wait_args.in.ctx_id = fence->ctx_id;
    wait_args.in.timeout = timeout_ns;
    
    if (ioctl(ctx->fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args) < 0) {
        if (errno == ETIME) {
            return -ETIME;  // Timeout
        }
        sp_log(SP_LOG_ERROR, "Fence wait failed: %s", strerror(errno));
        return -errno;
    }
    
    return 0;
}

// Check fence status (non-blocking)
int sp_fence_check(sp_pm4_ctx* ctx, sp_fence* fence) {
    return sp_fence_wait(ctx, fence, 0);  // 0 timeout = poll
}

// Initialize logging from environment
__attribute__((constructor))
static void sp_init_logging(void) {
    const char* level = getenv("SPORKLE_LOG_LEVEL");
    if (level) {
        if (strcmp(level, "TRACE") == 0) g_log_level = SP_LOG_TRACE;
        else if (strcmp(level, "INFO") == 0) g_log_level = SP_LOG_INFO;
        else if (strcmp(level, "WARN") == 0) g_log_level = SP_LOG_WARN;
        else if (strcmp(level, "ERROR") == 0) g_log_level = SP_LOG_ERROR;
    }
}