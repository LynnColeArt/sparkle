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

// PM4 packet headers
#define PM4_TYPE_3_HEADER(op, count) ((3 << 30) | ((count) << 16) | (op))
#define PM4_SET_UCONFIG_REG 0x79
#define PM4_SET_SH_REG      0x25
#define PM4_SET_SH_REG_INDEX 0x9B
#define PM4_DISPATCH_DIRECT 0x15
#define PM4_CLEAR_STATE     0x12

// UCONFIG registers
#define GRBM_GFX_INDEX                   0x30800
#define COMPUTE_STATIC_THREAD_MGMT_SE0   0x30844

// SH registers
#define COMPUTE_PGM_LO    0x2C00
#define COMPUTE_PGM_HI    0x2C01
#define COMPUTE_PGM_RSRC1 0x2C02
#define COMPUTE_PGM_RSRC2 0x2C03
#define COMPUTE_NUM_THREAD_X 0x2C09
#define COMPUTE_NUM_THREAD_Y 0x2C0A
#define COMPUTE_NUM_THREAD_Z 0x2C0B
#define COMPUTE_USER_DATA_0  0x2C40

// Broadcast mode
#define SE_BROADCAST_WRITES  (1 << 31)
#define SH_BROADCAST_WRITES  (1 << 29)
#define INSTANCE_BROADCAST   (1 << 30)
#define BROADCAST_ALL        (SE_BROADCAST_WRITES | SH_BROADCAST_WRITES | INSTANCE_BROADCAST)

// Minimal s_endpgm shader (already byte-swapped)
static const uint32_t endpgm_shader[] = {
    0x000081BF  // s_endpgm
};

// BO list operations
#define AMDGPU_BO_LIST_OP_CREATE 0
#define AMDGPU_BO_LIST_OP_DESTROY 1

int main() {
    printf("=== Test GFX Enable + Compute Submit ===\n\n");
    
    // 1. Open device
    int fd = open("/dev/dri/renderD129", O_RDWR);
    if (fd < 0) {
        perror("Failed to open GPU");
        return 1;
    }
    
    // 2. Create context
    union drm_amdgpu_ctx ctx_args = {0};
    ctx_args.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args) < 0) {
        perror("Failed to create context");
        close(fd);
        return 1;
    }
    uint32_t ctx_id = ctx_args.out.alloc.ctx_id;
    printf("✓ Context created: %u\n", ctx_id);
    
    // 3. Create shader BO
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = 4096;
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args) < 0) {
        perror("Failed to create shader BO");
        close(fd);
        return 1;
    }
    uint32_t shader_handle = gem_args.out.handle;
    
    // Map and upload shader
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = shader_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    void* shader_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                           fd, mmap_args.out.addr_ptr);
    memcpy(shader_ptr, endpgm_shader, sizeof(endpgm_shader));
    munmap(shader_ptr, 4096);
    
    // Map shader to VA
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = shader_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_EXECUTABLE;
    va_args.va_address = 0x800000000;
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    uint64_t shader_va = va_args.va_address;
    printf("✓ Shader at VA 0x%llx\n", (unsigned long long)shader_va);
    
    // === STEP 1: Enable CUs via GFX ring ===
    printf("\nStep 1: Enabling CUs via GFX ring...\n");
    
    uint32_t gfx_ib[] = {
        // Set GRBM_GFX_INDEX to broadcast mode
        PM4_TYPE_3_HEADER(PM4_SET_UCONFIG_REG, 1),
        (GRBM_GFX_INDEX - 0x30000) >> 2,
        BROADCAST_ALL,
        
        // Enable all CUs
        PM4_TYPE_3_HEADER(PM4_SET_UCONFIG_REG, 4),
        (COMPUTE_STATIC_THREAD_MGMT_SE0 - 0x30000) >> 2,
        0xFFFFFFFF,  // SE0
        0xFFFFFFFF,  // SE1
        0xFFFFFFFF,  // SE2
        0xFFFFFFFF,  // SE3
    };
    
    // Create GFX IB
    gem_args.in.bo_size = sizeof(gfx_ib);
    gem_args.in.alignment = 256;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t gfx_ib_handle = gem_args.out.handle;
    
    mmap_args.in.handle = gfx_ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    void* gfx_ib_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                           fd, mmap_args.out.addr_ptr);
    memcpy(gfx_ib_ptr, gfx_ib, sizeof(gfx_ib));
    munmap(gfx_ib_ptr, 4096);
    
    va_args.handle = gfx_ib_handle;
    va_args.va_address = 0x800001000;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    
    // Submit GFX IB
    struct drm_amdgpu_bo_list_entry bo_list[] = {
        {shader_handle, 0},
        {gfx_ib_handle, 0}
    };
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    bo_list_args.in.bo_number = 2;
    bo_list_args.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
    bo_list_args.in.bo_info_ptr = (uintptr_t)bo_list;
    ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    ib_chunk._pad = 0;
    ib_chunk.flags = 0;
    ib_chunk.va_start = va_args.va_address;
    ib_chunk.ib_bytes = sizeof(gfx_ib);
    ib_chunk.ip_type = AMDGPU_HW_IP_GFX;
    ib_chunk.ip_instance = 0;
    ib_chunk.ring = 0;
    
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(ib_chunk) / 4;
    chunks[0].chunk_data = (uintptr_t)&ib_chunk;
    
    uint64_t chunk_ptrs[1] = {(uintptr_t)&chunks[0]};
    
    union drm_amdgpu_cs cs_args = {0};
    cs_args.in.ctx_id = ctx_id;
    cs_args.in.bo_list_handle = bo_list_args.out.list_handle;
    cs_args.in.num_chunks = 1;
    cs_args.in.chunks = (uintptr_t)chunk_ptrs;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args) == 0) {
        printf("✓ GFX IB submitted\n");
        
        // Wait for GFX completion
        union drm_amdgpu_wait_cs wait_args = {0};
        wait_args.in.handle = cs_args.out.handle;
        wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
        wait_args.in.ip_instance = 0;
        wait_args.in.ring = 0;
        wait_args.in.ctx_id = ctx_id;
        wait_args.in.timeout = 1000000000;
        ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
        printf("✓ GFX fence signaled\n");
    } else {
        printf("❌ GFX submission failed: %s\n", strerror(errno));
    }
    
    // Destroy GFX BO list
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_DESTROY;
    bo_list_args.in.list_handle = bo_list_args.out.list_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    
    // === STEP 2: Submit compute work ===
    printf("\nStep 2: Submitting compute shader...\n");
    
    uint32_t compute_ib[] = {
        // CLEAR_STATE
        PM4_TYPE_3_HEADER(PM4_CLEAR_STATE, 0),
        0x00000000,
        
        // Set shader address
        PM4_TYPE_3_HEADER(PM4_SET_SH_REG, 2),
        (COMPUTE_PGM_LO - 0x2800) >> 2,
        (uint32_t)(shader_va >> 8),
        (uint32_t)(shader_va >> 40),
        
        // Set shader resources (minimal)
        PM4_TYPE_3_HEADER(PM4_SET_SH_REG, 2),
        (COMPUTE_PGM_RSRC1 - 0x2800) >> 2,
        0x00000000,  // Minimal RSRC1
        0x00000000,  // Minimal RSRC2
        
        // Set thread dimensions
        PM4_TYPE_3_HEADER(PM4_SET_SH_REG, 3),
        (COMPUTE_NUM_THREAD_X - 0x2800) >> 2,
        1, 1, 1,  // 1 thread
        
        // DISPATCH_DIRECT
        PM4_TYPE_3_HEADER(PM4_DISPATCH_DIRECT, 4),
        1, 1, 1,  // 1x1x1 workgroup
        0x00000001,  // COMPUTE_SHADER_EN
    };
    
    // Create compute IB
    gem_args.in.bo_size = sizeof(compute_ib);
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t compute_ib_handle = gem_args.out.handle;
    
    mmap_args.in.handle = compute_ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    void* compute_ib_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                               fd, mmap_args.out.addr_ptr);
    memcpy(compute_ib_ptr, compute_ib, sizeof(compute_ib));
    munmap(compute_ib_ptr, 4096);
    
    va_args.handle = compute_ib_handle;
    va_args.va_address = 0x800002000;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    
    // Submit compute IB
    bo_list[1].bo_handle = compute_ib_handle;
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    
    ib_chunk.va_start = va_args.va_address;
    ib_chunk.ib_bytes = sizeof(compute_ib);
    ib_chunk.ip_type = AMDGPU_HW_IP_COMPUTE;
    
    cs_args.in.bo_list_handle = bo_list_args.out.list_handle;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args) == 0) {
        printf("✓ Compute IB submitted! Sequence = %llu\n", 
               (unsigned long long)cs_args.out.handle);
        
        // Wait for compute completion
        union drm_amdgpu_wait_cs wait_args = {0};
        wait_args.in.handle = cs_args.out.handle;
        wait_args.in.ip_type = AMDGPU_HW_IP_COMPUTE;
        wait_args.in.ip_instance = 0;
        wait_args.in.ring = 0;
        wait_args.in.ctx_id = ctx_id;
        wait_args.in.timeout = 1000000000;
        
        int ret = ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
        printf("Wait result: %d, status: %lld\n", ret, (long long)wait_args.out.status);
        
        if (wait_args.out.status == 0) {
            printf("\n✅ Compute shader executed successfully!\n");
        } else {
            printf("\n❌ Compute execution may have failed\n");
        }
    } else {
        printf("❌ Compute submission failed: %s\n", strerror(errno));
    }
    
    // Cleanup
    printf("\nCleanup...\n");
    close(fd);
    
    return 0;
}