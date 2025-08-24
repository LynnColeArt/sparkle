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
#define PM4_RELEASE_MEM     0x49

// EOP defines
#define EOP_DATA_SEL_VALUE_32BIT (1 << 29)
#define EOP_INT_SEL_NONE        (0 << 24)
#define EOP_DST_SEL_MEM         (0 << 16)
#define EOP_TC_ACTION_ENA       (1 << 17)
#define EOP_TC_WB_ACTION_ENA    (1 << 18)
#define EOP_TC_INV_ENA          (1 << 20)

// BO list operations
#define AMDGPU_BO_LIST_OP_CREATE 0
#define AMDGPU_BO_LIST_OP_DESTROY 1

int main() {
    printf("=== Minimal GFX EOP Test ===\n");
    printf("Testing GFX ring execution with ONLY EOP packet\n\n");
    
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
    
    // 3. Create signal BO in GTT
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = 4096;
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args) < 0) {
        perror("Failed to create signal BO");
        close(fd);
        return 1;
    }
    uint32_t signal_handle = gem_args.out.handle;
    printf("✓ Signal BO created: handle=%u\n", signal_handle);
    
    // Map signal BO
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = signal_handle;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args) < 0) {
        perror("Failed to mmap signal BO");
        close(fd);
        return 1;
    }
    
    uint32_t* signal_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                               fd, mmap_args.out.addr_ptr);
    if (signal_ptr == MAP_FAILED) {
        perror("mmap failed");
        close(fd);
        return 1;
    }
    
    // Clear signal
    signal_ptr[0] = 0;
    signal_ptr[1] = 0;
    printf("✓ Signal memory mapped and cleared\n");
    
    // Map signal to VA with READ|WRITE
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = signal_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE;
    va_args.va_address = 0x800000000;
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args) < 0) {
        perror("Failed to map signal VA");
        close(fd);
        return 1;
    }
    uint64_t signal_va = va_args.va_address;
    printf("✓ Signal VA mapped: 0x%llx\n", (unsigned long long)signal_va);
    
    // 4. Create minimal GFX IB with ONLY RELEASE_MEM EOP
    uint32_t gfx_ib[] = {
        // RELEASE_MEM packet - write immediate 32-bit value to memory
        PM4_TYPE_3_HEADER(PM4_RELEASE_MEM, 5),
        EOP_DATA_SEL_VALUE_32BIT |  // Write immediate 32-bit value
        EOP_INT_SEL_NONE |           // No interrupt
        EOP_DST_SEL_MEM |            // Write to memory
        EOP_TC_ACTION_ENA |          // Flush L2 cache
        EOP_TC_WB_ACTION_ENA |       // Write back L2
        EOP_TC_INV_ENA,              // Invalidate L2
        (uint32_t)signal_va,         // dst addr lo
        (uint32_t)(signal_va >> 32), // dst addr hi
        0xDEADBEEF,                  // immediate data
        0,                           // unused
    };
    
    printf("\nGFX IB contents (%zu dwords):\n", sizeof(gfx_ib)/4);
    for (size_t i = 0; i < sizeof(gfx_ib)/4; i++) {
        printf("  [%02zu] 0x%08X", i, gfx_ib[i]);
        if (i == 0) printf(" <- RELEASE_MEM header");
        else if (i == 1) printf(" <- Control flags");
        else if (i == 2) printf(" <- Signal addr low");
        else if (i == 3) printf(" <- Signal addr high");
        else if (i == 4) printf(" <- Immediate value");
        printf("\n");
    }
    
    // Create IB BO
    gem_args.in.bo_size = sizeof(gfx_ib);
    gem_args.in.alignment = 256;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args) < 0) {
        perror("Failed to create IB BO");
        close(fd);
        return 1;
    }
    uint32_t ib_handle = gem_args.out.handle;
    
    // Upload IB
    mmap_args.in.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    void* ib_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                       fd, mmap_args.out.addr_ptr);
    memcpy(ib_ptr, gfx_ib, sizeof(gfx_ib));
    munmap(ib_ptr, 4096);
    
    // Map IB to VA with READ
    va_args.handle = ib_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE;
    va_args.va_address = 0x800001000;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    
    // 5. Submit to GFX ring
    printf("\nSubmitting to GFX ring...\n");
    
    // Create BO list with JUST the handles as uint32_t array
    uint32_t bo_handles[2] = {ib_handle, signal_handle};
    
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    bo_list_args.in.bo_number = 2;
    bo_list_args.in.bo_info_size = sizeof(uint32_t);
    bo_list_args.in.bo_info_ptr = (uintptr_t)bo_handles;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args) < 0) {
        // Try with proper bo_list_entry structure
        struct drm_amdgpu_bo_list_entry bo_list[] = {
            {ib_handle, 0},
            {signal_handle, 0}
        };
        bo_list_args.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
        bo_list_args.in.bo_info_ptr = (uintptr_t)bo_list;
        
        if (ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args) < 0) {
            perror("Failed to create BO list");
            close(fd);
            return 1;
        }
    }
    
    printf("✓ BO list created: handle=%u\n", bo_list_args.out.list_handle);
    
    // Build CS submission
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
    
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    if (ret < 0) {
        printf("❌ GFX submission failed: %s (errno=%d)\n", strerror(errno), errno);
        close(fd);
        return 1;
    }
    
    printf("✓ GFX IB submitted! Sequence=%llu\n", (unsigned long long)cs_args.out.handle);
    
    // 6. Wait for completion
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = cs_args.out.handle;
    wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
    wait_args.in.ip_instance = 0;
    wait_args.in.ring = 0;
    wait_args.in.ctx_id = ctx_id;
    wait_args.in.timeout = 1000000000; // 1 second
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
    printf("Wait result: %d, status: %lld\n", ret, (long long)wait_args.out.status);
    
    // 7. Check signal
    printf("\nChecking signal value:\n");
    printf("  Signal[0] = 0x%08X (expected 0xDEADBEEF)\n", signal_ptr[0]);
    printf("  Signal[1] = 0x%08X\n", signal_ptr[1]);
    
    if (signal_ptr[0] == 0xDEADBEEF) {
        printf("\n✅ SUCCESS! GFX ring executed and EOP wrote signal!\n");
        printf("GFX ring path is confirmed working.\n");
    } else {
        printf("\n❌ EOP signal not written - GFX execution failed\n");
        printf("Check dmesg for GPU reset messages\n");
    }
    
    // Cleanup
    munmap(signal_ptr, 4096);
    
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_DESTROY;
    bo_list_args.in.list_handle = bo_list_args.out.list_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    
    close(fd);
    
    return (signal_ptr[0] == 0xDEADBEEF) ? 0 : 1;
}