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
#define PM4_WRITE_DATA     0x37

// UCONFIG registers
#define GRBM_GFX_INDEX                   0x30800
#define COMPUTE_STATIC_THREAD_MGMT_SE0   0x30844
#define COMPUTE_STATIC_THREAD_MGMT_SE1   0x30848
#define COMPUTE_STATIC_THREAD_MGMT_SE2   0x3084c
#define COMPUTE_STATIC_THREAD_MGMT_SE3   0x30850

// Broadcast mode
#define SE_BROADCAST_WRITES  (1 << 31)
#define SH_BROADCAST_WRITES  (1 << 29)
#define INSTANCE_BROADCAST   (1 << 30)
#define SE_INDEX(n)          ((n) << 16)
#define SH_INDEX(n)          ((n) << 8)
#define BROADCAST_ALL        (SE_BROADCAST_WRITES | SH_BROADCAST_WRITES | INSTANCE_BROADCAST)

int main() {
    printf("=== Test GFX Ring CU Enable ===\n");
    
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
    
    // 3. Check GFX availability
    struct drm_amdgpu_info info_req = {0};
    info_req.query = AMDGPU_INFO_HW_IP_INFO;
    info_req.query_hw_ip.type = AMDGPU_HW_IP_GFX;  // GFX ring
    info_req.query_hw_ip.ip_instance = 0;
    
    struct drm_amdgpu_info_hw_ip hw_ip = {0};
    info_req.return_pointer = (uintptr_t)&hw_ip;
    info_req.return_size = sizeof(hw_ip);
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) < 0 || hw_ip.available_rings == 0) {
        printf("❌ No GFX rings available\n");
        close(fd);
        return 1;
    }
    printf("✓ GFX rings available: %d\n", hw_ip.available_rings);
    
    // 4. Create IB with CU enable commands
    uint32_t ib[] = {
        // Set GRBM_GFX_INDEX to broadcast mode
        PM4_TYPE_3_HEADER(PM4_SET_UCONFIG_REG, 1),
        (GRBM_GFX_INDEX - 0x30000) >> 2,
        BROADCAST_ALL,
        
        // Enable all CUs on all SEs
        PM4_TYPE_3_HEADER(PM4_SET_UCONFIG_REG, 4),
        (COMPUTE_STATIC_THREAD_MGMT_SE0 - 0x30000) >> 2,
        0xFFFFFFFF,  // SE0: all CUs
        0xFFFFFFFF,  // SE1: all CUs
        0xFFFFFFFF,  // SE2: all CUs  
        0xFFFFFFFF,  // SE3: all CUs
        
        // Write completion signal
        PM4_TYPE_3_HEADER(PM4_WRITE_DATA, 5),
        0x00120005,  // Write through L2, 32-bit write
        0x12345678,  // Signal address low (dummy)
        0x00000000,  // Signal address high
        0xDEADBEEF,  // Signal value
        0x00000000,  // Unused
    };
    
    // 5. Create IB BO
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = sizeof(ib);
    gem_args.in.alignment = 256;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args) < 0) {
        perror("Failed to create IB BO");
        close(fd);
        return 1;
    }
    uint32_t ib_handle = gem_args.out.handle;
    
    // 6. Map and upload IB
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    void* ib_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                       fd, mmap_args.out.addr_ptr);
    memcpy(ib_ptr, ib, sizeof(ib));
    munmap(ib_ptr, 4096);
    
    // 7. Map IB to VA
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = ib_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE;
    va_args.va_address = 0x800000000;
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args) < 0) {
        perror("Failed to map IB VA");
        close(fd);
        return 1;
    }
    
    // 8. Submit to GFX ring
    printf("\nSubmitting GFX IB to enable CUs...\n");
    
    // Create BO list
    struct drm_amdgpu_bo_list_entry bo_list[] = {{ib_handle, 0}};
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    bo_list_args.in.bo_number = 1;
    bo_list_args.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
    bo_list_args.in.bo_info_ptr = (uintptr_t)bo_list;
    ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    
    // Submit CS
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    ib_chunk._pad = 0;
    ib_chunk.flags = 0;
    ib_chunk.va_start = va_args.va_address;
    ib_chunk.ib_bytes = sizeof(ib);
    ib_chunk.ip_type = AMDGPU_HW_IP_GFX;  // GFX ring!
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
    
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    if (ret < 0) {
        printf("❌ GFX submission failed: %s\n", strerror(errno));
        close(fd);
        return 1;
    }
    
    printf("✓ GFX IB submitted! Sequence = %llu\n", (unsigned long long)cs_args.out.handle);
    
    // Wait for completion
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = cs_args.out.handle;
    wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
    wait_args.in.ip_instance = 0;
    wait_args.in.ring = 0;
    wait_args.in.ctx_id = ctx_id;
    wait_args.in.timeout = 1000000000;  // 1 second
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
    printf("Wait result: %d, status: %lld\n", ret, (long long)wait_args.out.status);
    
    if (wait_args.out.status == 0) {
        printf("\n✅ CUs should now be enabled!\n");
        printf("You can verify with:\n");
        printf("  sudo umr -r *.*.GRBM_GFX_INDEX\n");
        printf("  sudo umr -r *.*.COMPUTE_STATIC_THREAD_MGMT_SE*\n");
    }
    
    // Cleanup
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_DESTROY;
    bo_list_args.in.list_handle = bo_list_args.out.list_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    
    va_args.operation = AMDGPU_VA_OP_UNMAP;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    
    struct drm_gem_close close_args = {0};
    close_args.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_GEM_CLOSE, &close_args);
    
    ctx_args.in.op = AMDGPU_CTX_OP_FREE_CTX;
    ctx_args.in.ctx_id = ctx_id;
    ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args);
    
    close(fd);
    
    return 0;
}