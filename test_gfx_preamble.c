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

// PM4 packets
#define PM4_SET_UCONFIG_REG     0x79
#define PM4_NOP                 0x10

// UCONFIG registers
#define GRBM_GFX_INDEX          0x30800
#define COMPUTE_STATIC_THREAD_MGMT_SE0 0x211
#define COMPUTE_STATIC_THREAD_MGMT_SE1 0x212
#define COMPUTE_STATIC_THREAD_MGMT_SE2 0x213
#define COMPUTE_STATIC_THREAD_MGMT_SE3 0x214

// Build GFX preamble to enable CUs
uint32_t build_gfx_preamble(uint32_t* ib) {
    uint32_t idx = 0;
    
    // Set GRBM_GFX_INDEX to broadcast mode
    ib[idx++] = (3 << 30) | (1 << 16) | PM4_SET_UCONFIG_REG;
    ib[idx++] = GRBM_GFX_INDEX >> 2;  // Register offset/4
    ib[idx++] = 0xE0000000;  // SE_BROADCAST_WRITES=1, SH_BROADCAST_WRITES=1
    
    // Enable all CUs on all SEs
    ib[idx++] = (3 << 30) | (4 << 16) | PM4_SET_UCONFIG_REG;
    ib[idx++] = COMPUTE_STATIC_THREAD_MGMT_SE0 >> 2;
    ib[idx++] = 0xFFFFFFFF;  // SE0
    ib[idx++] = 0xFFFFFFFF;  // SE1
    ib[idx++] = 0xFFFFFFFF;  // SE2
    ib[idx++] = 0xFFFFFFFF;  // SE3
    
    // Add some NOPs for padding
    ib[idx++] = (3 << 30) | (3 << 16) | PM4_NOP;
    ib[idx++] = 0;
    ib[idx++] = 0;
    ib[idx++] = 0;
    
    return idx;
}

int main() {
    printf("=== GFX Preamble Test ===\n");
    printf("Enabling CUs via GFX ring\n\n");
    
    // Open GPU
    int fd = open("/dev/dri/renderD129", O_RDWR);
    if (fd < 0) {
        fd = open("/dev/dri/renderD128", O_RDWR);
    }
    
    if (fd < 0) {
        perror("Failed to open GPU");
        return 1;
    }
    
    // Create GPU context
    union drm_amdgpu_ctx ctx_args = {0};
    ctx_args.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args) < 0) {
        perror("Failed to create GPU context");
        close(fd);
        return 1;
    }
    
    uint32_t gpu_ctx = ctx_args.out.alloc.ctx_id;
    printf("GPU context created: %u\n", gpu_ctx);
    
    // Create IB buffer
    union drm_amdgpu_gem_create ib_gem = {0};
    ib_gem.in.bo_size = 4096;
    ib_gem.in.alignment = 4096;
    ib_gem.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    ib_gem.in.domain_flags = 0;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &ib_gem) < 0) {
        perror("Failed to create IB buffer");
        close(fd);
        return 1;
    }
    
    // Map and build IB
    union drm_amdgpu_gem_mmap ib_mmap = {0};
    ib_mmap.in.handle = ib_gem.out.handle;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &ib_mmap) < 0) {
        perror("Failed to get IB mmap offset");
        close(fd);
        return 1;
    }
    
    uint32_t* ib_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED, 
                           fd, ib_mmap.out.addr_ptr);
    if (ib_ptr == MAP_FAILED) {
        perror("Failed to mmap IB buffer");
        close(fd);
        return 1;
    }
    
    uint32_t ib_size = build_gfx_preamble(ib_ptr);
    
    printf("IB contents (%d dwords):\n", ib_size);
    for (uint32_t i = 0; i < ib_size; i++) {
        printf("  [%02d] 0x%08X", i, ib_ptr[i]);
        if (i == 1) printf(" <- GRBM_GFX_INDEX offset");
        else if (i == 2) printf(" <- Broadcast mode");
        else if (i == 4) printf(" <- THREAD_MGMT_SE0 offset");
        else if (i == 5) printf(" <- Enable all CUs");
        printf("\n");
    }
    
    munmap(ib_ptr, 4096);
    
    // Get IB VA
    struct drm_amdgpu_gem_va ib_va_args = {0};
    ib_va_args.handle = ib_gem.out.handle;
    ib_va_args.operation = AMDGPU_VA_OP_MAP;
    ib_va_args.flags = AMDGPU_VM_PAGE_READABLE;
    ib_va_args.va_address = 0x800000000;  // Fixed address
    ib_va_args.offset_in_bo = 0;
    ib_va_args.map_size = 4096;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &ib_va_args) < 0) {
        perror("Failed to map IB VA");
        close(fd);
        return 1;
    }
    
    uint64_t ib_va = ib_va_args.va_address;
    
    // Submit CS on GFX ring
    struct drm_amdgpu_bo_list_entry bo_list[1] = {
        {ib_gem.out.handle, 0}
    };
    
    struct drm_amdgpu_cs_chunk chunks[2] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    struct drm_amdgpu_cs_chunk_data bo_list_chunk = {0};
    
    // IB chunk - NOTE: Using GFX ring!
    ib_chunk.va_start = ib_va;
    ib_chunk.ib_bytes = ib_size * 4;
    ib_chunk.ip_type = AMDGPU_HW_IP_GFX;  // GFX ring, not compute!
    ib_chunk.ip_instance = 0;
    ib_chunk.ring = 0;
    ib_chunk.flags = 0;
    
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(ib_chunk) / 4;
    chunks[0].chunk_data = (uintptr_t)&ib_chunk;
    
    // BO list chunk
    bo_list_chunk.bo_list_handle = (uintptr_t)bo_list;
    bo_list_chunk.bo_number = 1;
    
    chunks[1].chunk_id = AMDGPU_CHUNK_ID_BO_HANDLES;
    chunks[1].length_dw = sizeof(bo_list_chunk) / 4;
    chunks[1].chunk_data = (uintptr_t)&bo_list_chunk;
    
    union drm_amdgpu_cs cs_args = {0};
    cs_args.in.ctx_id = gpu_ctx;
    cs_args.in.num_chunks = 2;
    cs_args.in.chunks = (uintptr_t)chunks;
    
    printf("\nSubmitting GFX preamble...\n");
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args) < 0) {
        perror("Failed to submit CS");
        close(fd);
        return 1;
    }
    
    uint64_t seq = cs_args.out.handle;
    printf("GFX CS submitted, sequence: %lu\n", seq);
    
    // Wait for completion
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = seq;
    wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
    wait_args.in.ip_instance = 0;
    wait_args.in.ring = 0;
    wait_args.in.ctx_id = gpu_ctx;
    wait_args.in.timeout = 1000000000;  // 1 second
    
    printf("Waiting for completion...\n");
    if (ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args) < 0) {
        perror("Failed to wait for CS");
        close(fd);
        return 1;
    }
    
    if (wait_args.out.status == 0) {
        printf("✅ GFX preamble completed successfully\n");
        printf("CUs should now be enabled for compute workloads!\n");
    } else {
        printf("❌ GFX CS failed with status: %lld\n", wait_args.out.status);
    }
    
    // Cleanup
    ib_va_args.operation = AMDGPU_VA_OP_UNMAP;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &ib_va_args);
    
    struct drm_gem_close close_args = {0};
    close_args.handle = ib_gem.out.handle;
    ioctl(fd, DRM_IOCTL_GEM_CLOSE, &close_args);
    
    // Don't destroy context - keep it for compute work
    printf("\nContext %u ready for compute work\n", gpu_ctx);
    
    close(fd);
    
    return 0;
}