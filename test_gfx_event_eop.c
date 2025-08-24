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
#define PM4_EVENT_WRITE_EOP 0x47
#define PM4_NOP             0x10

// Event types
#define CACHE_FLUSH_AND_INV_TS_EVENT 0x2F
#define EVENT_INDEX_EOP              5

// BO list operations
#define AMDGPU_BO_LIST_OP_CREATE 0
#define AMDGPU_BO_LIST_OP_DESTROY 1

int main() {
    printf("=== GFX EVENT_WRITE_EOP Test ===\n");
    printf("Testing with EVENT_WRITE_EOP packet instead of RELEASE_MEM\n\n");
    
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
    
    // Map and clear signal
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = signal_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    
    uint32_t* signal_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                               fd, mmap_args.out.addr_ptr);
    memset(signal_ptr, 0, 4096);
    printf("✓ Signal memory cleared\n");
    
    // Map signal to VA
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = signal_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE;
    va_args.va_address = 0x800000000;
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    uint64_t signal_va = va_args.va_address;
    printf("✓ Signal VA: 0x%llx\n", (unsigned long long)signal_va);
    
    // 4. Create GFX IB with EVENT_WRITE_EOP
    uint32_t gfx_ib[] = {
        // EVENT_WRITE_EOP packet
        PM4_TYPE_3_HEADER(PM4_EVENT_WRITE_EOP, 4),
        (EVENT_INDEX_EOP << 8) | CACHE_FLUSH_AND_INV_TS_EVENT,  // event_index | event_type
        (uint32_t)signal_va,         // addr_lo
        (uint32_t)(signal_va >> 32), // addr_hi
        0xDEADBEEF,                  // data_lo (for 32-bit write)
        0,                           // data_hi (unused for 32-bit)
        
        // Add some NOPs for padding
        PM4_TYPE_3_HEADER(PM4_NOP, 0),
        0,
        PM4_TYPE_3_HEADER(PM4_NOP, 0),
        0,
    };
    
    printf("\nGFX IB contents (%zu dwords):\n", sizeof(gfx_ib)/4);
    for (size_t i = 0; i < sizeof(gfx_ib)/4; i++) {
        printf("  [%02zu] 0x%08X", i, gfx_ib[i]);
        if (i == 0) printf(" <- EVENT_WRITE_EOP header");
        else if (i == 1) printf(" <- Event type/index");
        else if (i == 2) printf(" <- Signal addr low");
        else if (i == 3) printf(" <- Signal addr high");
        else if (i == 4) printf(" <- Data value");
        else if (i == 6 || i == 8) printf(" <- NOP");
        printf("\n");
    }
    
    // Create IB BO
    gem_args.in.bo_size = sizeof(gfx_ib);
    gem_args.in.alignment = 256;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t ib_handle = gem_args.out.handle;
    
    // Upload IB
    mmap_args.in.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    void* ib_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                       fd, mmap_args.out.addr_ptr);
    memcpy(ib_ptr, gfx_ib, sizeof(gfx_ib));
    munmap(ib_ptr, 4096);
    
    // Map IB to VA
    va_args.handle = ib_handle;
    va_args.flags = AMDGPU_VM_PAGE_READABLE;
    va_args.va_address = 0x800001000;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    
    // 5. Submit to GFX ring
    printf("\nSubmitting to GFX ring...\n");
    
    // Create BO list
    struct drm_amdgpu_bo_list_entry bo_list[] = {
        {ib_handle, 0},
        {signal_handle, 0}
    };
    
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    bo_list_args.in.bo_number = 2;
    bo_list_args.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
    bo_list_args.in.bo_info_ptr = (uintptr_t)bo_list;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args) < 0) {
        perror("Failed to create BO list");
        close(fd);
        return 1;
    }
    
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
        printf("❌ GFX submission failed: %s\n", strerror(errno));
        close(fd);
        return 1;
    }
    
    printf("✓ GFX IB submitted! Sequence=%llu\n", (unsigned long long)cs_args.out.handle);
    
    // 6. Wait and check
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = cs_args.out.handle;
    wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
    wait_args.in.ip_instance = 0;
    wait_args.in.ring = 0;
    wait_args.in.ctx_id = ctx_id;
    wait_args.in.timeout = 1000000000;
    
    ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
    
    // Check results
    printf("\nResults:\n");
    printf("  Signal[0] = 0x%08X (expected 0xDEADBEEF)\n", signal_ptr[0]);
    printf("  Signal[1] = 0x%08X\n", signal_ptr[1]);
    printf("  Signal[2] = 0x%08X\n", signal_ptr[2]);
    
    if (signal_ptr[0] == 0xDEADBEEF) {
        printf("\n✅ SUCCESS! EVENT_WRITE_EOP worked!\n");
    } else {
        printf("\n❌ No signal written\n");
        
        // Let's also try a different approach - WRITE_DATA packet
        printf("\nTrying WRITE_DATA packet as control test...\n");
    }
    
    // Cleanup
    munmap(signal_ptr, 4096);
    close(fd);
    
    return 0;
}