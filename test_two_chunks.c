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

// PM4 constants
#define PKT3                3u
#define IT_WRITE_DATA       0x37
#define IT_NOP              0x10

// Build PKT3 header
static inline uint32_t pkt3_header(uint32_t opcode, uint32_t count) {
    return (PKT3 << 30) | (opcode << 8) | count;
}

int main() {
    printf("=== Two Chunk Test (IB + BO_HANDLES) ===\n\n");
    
    // 1. Open device
    int fd = open("/dev/dri/renderD129", O_RDWR);
    if (fd < 0) {
        perror("Failed to open GPU");
        return 1;
    }
    
    // 2. Create context
    union drm_amdgpu_ctx ctx_args = {0};
    ctx_args.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args);
    uint32_t ctx_id = ctx_args.out.alloc.ctx_id;
    
    // 3. Create signal BO
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = 4096;
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t signal_handle = gem_args.out.handle;
    
    // Map and init signal
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = signal_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    uint32_t* signal_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                               fd, mmap_args.out.addr_ptr);
    signal_ptr[0] = 0x12345678;
    
    // Map signal VA
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = signal_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE;
    va_args.va_address = 0x800000000;
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    uint64_t signal_va = va_args.va_address;
    
    // 4. Create IB BO
    gem_args.in.bo_size = 128;
    gem_args.in.alignment = 128;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t ib_handle = gem_args.out.handle;
    
    // Build IB
    mmap_args.in.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    uint32_t* ib = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                       fd, mmap_args.out.addr_ptr);
    
    int idx = 0;
    // WRITE_DATA with correct count=4
    ib[idx++] = pkt3_header(IT_WRITE_DATA, 4);
    ib[idx++] = 0x00010000;  // Minimal control
    ib[idx++] = (uint32_t)(signal_va & 0xFFFFFFFF);
    ib[idx++] = (uint32_t)((signal_va >> 32) & 0xFFFF);
    ib[idx++] = 0xCAFEBABE;
    
    // Pad to 32 dwords
    ib[idx++] = pkt3_header(IT_NOP, 25);
    while (idx < 32) ib[idx++] = 0;
    
    munmap(ib, 4096);
    
    // Map IB VA
    va_args.handle = ib_handle;
    va_args.flags = AMDGPU_VM_PAGE_READABLE;
    va_args.va_address = 0x800001000;
    va_args.map_size = 128;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    
    // 5. Create BO list
    struct drm_amdgpu_bo_list_entry bo_list[2] = {
        {ib_handle, 0},
        {signal_handle, 0}
    };
    
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = 0; // AMDGPU_BO_LIST_OP_CREATE
    bo_list_args.in.bo_number = 2;
    bo_list_args.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
    bo_list_args.in.bo_info_ptr = (uintptr_t)bo_list;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args) < 0) {
        // Try with handles array
        uint32_t handles[2] = {ib_handle, signal_handle};
        bo_list_args.in.bo_info_size = sizeof(uint32_t);
        bo_list_args.in.bo_info_ptr = (uintptr_t)handles;
        ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    }
    
    // 6. Create chunks - TWO chunks as Mini said
    struct drm_amdgpu_cs_chunk chunks[2] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    // IB chunk
    ib_chunk.va_start = va_args.va_address;
    ib_chunk.ib_bytes = 128;
    ib_chunk.ip_type = AMDGPU_HW_IP_GFX;
    
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(struct drm_amdgpu_cs_chunk_ib) / 4;
    chunks[0].chunk_data = (uintptr_t)&ib_chunk;
    
    // BO_HANDLES chunk - Mini said this is critical!
    uint32_t handles[2] = {ib_handle, signal_handle};
    chunks[1].chunk_id = AMDGPU_CHUNK_ID_BO_HANDLES;
    chunks[1].length_dw = 2;
    chunks[1].chunk_data = (uintptr_t)handles;
    
    uint64_t chunk_ptrs[2] = {
        (uintptr_t)&chunks[0],
        (uintptr_t)&chunks[1]
    };
    
    union drm_amdgpu_cs cs_args = {0};
    cs_args.in.ctx_id = ctx_id;
    cs_args.in.bo_list_handle = bo_list_args.out.list_handle;
    cs_args.in.num_chunks = 2;  // TWO chunks!
    cs_args.in.chunks = (uintptr_t)chunk_ptrs;
    
    printf("Submitting with TWO chunks:\n");
    printf("  Chunk[0]: IB (va=0x%llx, size=%u)\n", 
           (unsigned long long)ib_chunk.va_start, ib_chunk.ib_bytes);
    printf("  Chunk[1]: BO_HANDLES (handles=%u,%u)\n", handles[0], handles[1]);
    
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    if (ret < 0) {
        printf("❌ Failed: %s\n", strerror(errno));
        return 1;
    }
    
    printf("✓ Submitted! seq=%llu\n", (unsigned long long)cs_args.out.handle);
    
    // Wait
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = cs_args.out.handle;
    wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
    wait_args.in.ctx_id = ctx_id;
    wait_args.in.timeout = 1000000000;
    ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
    
    printf("\nResult: Signal[0] = 0x%08X (expected 0xCAFEBABE)\n", signal_ptr[0]);
    
    if (signal_ptr[0] == 0xCAFEBABE) {
        printf("✅ SUCCESS! Two chunks worked!\n");
    } else {
        printf("❌ Still no execution\n");
    }
    
    close(fd);
    return 0;
}