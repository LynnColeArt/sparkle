#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <drm/amdgpu_drm.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <time.h>

// BO list operations
#define AMDGPU_BO_LIST_OP_CREATE 0
#define AMDGPU_BO_LIST_OP_DESTROY 1
#define AMDGPU_BO_LIST_OP_UPDATE 2

// Trace every step of PM4 submission
#define TRACE(fmt, ...) do { \
    struct timespec ts; \
    clock_gettime(CLOCK_MONOTONIC, &ts); \
    printf("[%ld.%09ld] " fmt "\n", ts.tv_sec, ts.tv_nsec, ##__VA_ARGS__); \
} while(0)

// Simple s_endpgm shader
static const uint32_t endpgm_shader[] = {
    0x000081BF  // s_endpgm (already swapped)
};

int main() {
    TRACE("=== PM4 Submission Trace ===");
    
    // 1. Open device
    TRACE("Opening /dev/dri/renderD129...");
    int fd = open("/dev/dri/renderD129", O_RDWR);
    if (fd < 0) {
        TRACE("Failed: %s", strerror(errno));
        return 1;
    }
    TRACE("✓ Opened fd=%d", fd);
    
    // 2. Create context
    TRACE("Creating GPU context...");
    union drm_amdgpu_ctx ctx_args = {0};
    ctx_args.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args);
    TRACE("ioctl(DRM_IOCTL_AMDGPU_CTX) = %d (errno=%d)", ret, errno);
    if (ret < 0) {
        TRACE("Failed to create context");
        close(fd);
        return 1;
    }
    uint32_t ctx_id = ctx_args.out.alloc.ctx_id;
    TRACE("✓ Context ID = %u", ctx_id);
    
    // 3. Query compute info
    TRACE("Querying compute HW IP info...");
    struct drm_amdgpu_info info_req = {0};
    info_req.query = AMDGPU_INFO_HW_IP_INFO;
    info_req.query_hw_ip.type = AMDGPU_HW_IP_COMPUTE;
    info_req.query_hw_ip.ip_instance = 0;
    
    struct drm_amdgpu_info_hw_ip hw_ip = {0};
    info_req.return_pointer = (uintptr_t)&hw_ip;
    info_req.return_size = sizeof(hw_ip);
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req);
    TRACE("ioctl(DRM_IOCTL_AMDGPU_INFO) = %d", ret);
    if (ret == 0) {
        TRACE("✓ Compute rings available: %d", hw_ip.available_rings);
        TRACE("  IB alignment: start=%d, size=%d", 
              hw_ip.ib_start_alignment, hw_ip.ib_size_alignment);
    }
    
    // 4. Create shader BO
    TRACE("Creating shader BO (4KB)...");
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = 4096;
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    gem_args.in.domain_flags = 0;
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    TRACE("ioctl(DRM_IOCTL_AMDGPU_GEM_CREATE) = %d", ret);
    if (ret < 0) {
        TRACE("Failed to create shader BO");
        close(fd);
        return 1;
    }
    uint32_t shader_handle = gem_args.out.handle;
    TRACE("✓ Shader BO handle = %u", shader_handle);
    
    // 5. Map and upload shader
    TRACE("Mapping shader BO...");
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = shader_handle;
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    TRACE("ioctl(DRM_IOCTL_AMDGPU_GEM_MMAP) = %d", ret);
    
    void* shader_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                           fd, mmap_args.out.addr_ptr);
    TRACE("mmap() = %p", shader_ptr);
    
    memcpy(shader_ptr, endpgm_shader, sizeof(endpgm_shader));
    TRACE("✓ Uploaded shader: 0x%08X (s_endpgm)", endpgm_shader[0]);
    munmap(shader_ptr, 4096);
    
    // 6. Get shader VA
    TRACE("Mapping shader to VA...");
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = shader_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_EXECUTABLE;
    va_args.va_address = 0x800000000;
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    TRACE("ioctl(DRM_IOCTL_AMDGPU_GEM_VA) = %d", ret);
    if (ret < 0) {
        TRACE("Failed to map VA: %s", strerror(errno));
        close(fd);
        return 1;
    }
    uint64_t shader_va = va_args.va_address;
    TRACE("✓ Shader VA = 0x%016lX", shader_va);
    
    // 7. Create minimal IB
    TRACE("Creating IB (just DISPATCH_DIRECT)...");
    uint32_t ib[] = {
        // CLEAR_STATE
        0xC0000012, 0x00000000,
        
        // SET_SH_REG_INDEX - COMPUTE_PGM_LO/HI with broadcast
        0xC002009B,  // PKT3(SET_SH_REG_INDEX, 2)
        0xC0000204,  // reg offset | broadcast flags
        (uint32_t)(shader_va >> 8),
        (uint32_t)(shader_va >> 40),
        
        // SET_SH_REG_INDEX - COMPUTE_PGM_RSRC1/2 with broadcast
        0xC002009B,
        0xC0000206,
        0x00000000,  // Minimal RSRC1
        0x00000000,  // Minimal RSRC2
        
        // SET_SH_REG_INDEX - COMPUTE_NUM_THREAD_X/Y/Z
        0xC003009B,
        0xC000020A,
        1, 1, 1,     // 1 thread
        
        // DISPATCH_DIRECT
        0xC0040015,  // PKT3(DISPATCH_DIRECT, 4)
        1, 1, 1,     // 1x1x1 workgroup
        0x00000001,  // Just COMPUTE_SHADER_EN
    };
    
    TRACE("IB size = %zu dwords", sizeof(ib)/4);
    
    // 8. Create IB BO and upload
    gem_args.in.bo_size = sizeof(ib);
    gem_args.in.alignment = 256;
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    TRACE("Create IB BO: ret=%d, handle=%u", ret, gem_args.out.handle);
    
    uint32_t ib_handle = gem_args.out.handle;
    
    // Map and upload IB
    mmap_args.in.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    void* ib_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                       fd, mmap_args.out.addr_ptr);
    memcpy(ib_ptr, ib, sizeof(ib));
    munmap(ib_ptr, 4096);
    
    // Get IB VA
    va_args.handle = ib_handle;
    va_args.va_address = 0x800001000;
    va_args.map_size = 4096;
    va_args.flags = AMDGPU_VM_PAGE_READABLE;
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    TRACE("Map IB VA: ret=%d, va=0x%lX", ret, va_args.va_address);
    
    // 9. Submit CS
    TRACE("Submitting CS...");
    
    // Create BO list using AMDGPU_BO_LIST ioctl
    struct drm_amdgpu_bo_list_entry bo_list[] = {
        {shader_handle, 0},
        {ib_handle, 0}
    };
    
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    bo_list_args.in.bo_number = 2;
    bo_list_args.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
    bo_list_args.in.bo_info_ptr = (uintptr_t)bo_list;
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    TRACE("ioctl(DRM_IOCTL_AMDGPU_BO_LIST) = %d", ret);
    if (ret < 0) {
        TRACE("Failed to create BO list");
        close(fd);
        return 1;
    }
    uint32_t bo_list_handle = bo_list_args.out.list_handle;
    TRACE("✓ BO list handle = %u", bo_list_handle);
    
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    ib_chunk._pad = 0;  // Important: padding field
    ib_chunk.flags = 0;
    ib_chunk.va_start = va_args.va_address;
    ib_chunk.ib_bytes = sizeof(ib);
    ib_chunk.ip_type = AMDGPU_HW_IP_COMPUTE;
    ib_chunk.ip_instance = 0;
    ib_chunk.ring = 0;
    
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(ib_chunk) / 4;
    chunks[0].chunk_data = (uintptr_t)&ib_chunk;
    
    // CRITICAL: Create array of pointers to chunks!
    uint64_t chunk_ptrs[1];
    chunk_ptrs[0] = (uintptr_t)&chunks[0];
    
    // Debug: Print chunk details
    TRACE("Chunk[0]: id=%u, length_dw=%u, data=%p", 
          chunks[0].chunk_id, chunks[0].length_dw, (void*)chunks[0].chunk_data);
    TRACE("IB chunk: va_start=0x%llx, ib_bytes=%u, ip_type=%u", 
          (unsigned long long)ib_chunk.va_start, ib_chunk.ib_bytes, ib_chunk.ip_type);
    
    union drm_amdgpu_cs cs_args = {0};
    cs_args.in.ctx_id = ctx_id;
    cs_args.in.bo_list_handle = bo_list_handle;
    cs_args.in.num_chunks = 1;
    cs_args.in.chunks = (uintptr_t)chunk_ptrs;  // Pass pointer array!
    
    TRACE("CS args: ctx=%u, bo_list=%u, chunks=%u, chunks_ptr=%p", 
          ctx_id, bo_list_handle, 1, (void*)cs_args.in.chunks);
    TRACE("  chunk_ptrs[0]=0x%llx -> chunk[0]=%p", 
          (unsigned long long)chunk_ptrs[0], &chunks[0]);
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    TRACE("ioctl(DRM_IOCTL_AMDGPU_CS) = %d (errno=%d)", ret, errno);
    
    if (ret == 0) {
        TRACE("✓ CS submitted! Sequence = %lu", cs_args.out.handle);
        
        // Wait for fence
        TRACE("Waiting for fence...");
        union drm_amdgpu_wait_cs wait_args = {0};
        wait_args.in.handle = cs_args.out.handle;
        wait_args.in.ip_type = AMDGPU_HW_IP_COMPUTE;
        wait_args.in.ip_instance = 0;
        wait_args.in.ring = 0;
        wait_args.in.ctx_id = ctx_id;
        wait_args.in.timeout = 1000000000;  // 1 second
        
        ret = ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
        TRACE("ioctl(DRM_IOCTL_AMDGPU_WAIT_CS) = %d", ret);
        TRACE("Fence status = %lld", wait_args.out.status);
    } else {
        TRACE("❌ CS submission failed!");
    }
    
    // Cleanup
    TRACE("Cleanup...");
    va_args.operation = AMDGPU_VA_OP_UNMAP;
    va_args.handle = shader_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    
    va_args.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    
    struct drm_gem_close close_args = {0};
    close_args.handle = shader_handle;
    ioctl(fd, DRM_IOCTL_GEM_CLOSE, &close_args);
    
    close_args.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_GEM_CLOSE, &close_args);
    
    ctx_args.in.op = AMDGPU_CTX_OP_FREE_CTX;
    ctx_args.in.ctx_id = ctx_id;
    ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args);
    
    close(fd);
    
    TRACE("=== Trace complete ===");
    
    return 0;
}