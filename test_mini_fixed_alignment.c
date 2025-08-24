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
#define DST_SEL_MEM         0
#define ENGINE_ME           1

// Build PKT3 header
static inline uint32_t pkt3_header(uint32_t opcode, uint32_t count) {
    return (PKT3 << 30) | (opcode << 8) | count;
}

// Build WRITE_DATA control word
static inline uint32_t write_data_control() {
    uint32_t w = 0;
    w |= (DST_SEL_MEM & 0xF) << 8;      // DST_SEL at bits [11:8]
    w |= (1u << 16);                    // WR_ONE_ADDR at bit 16
    w |= (0u << 20);                    // WR_CONFIRM=0 at bit 20
    w |= (ENGINE_ME & 0x3) << 30;       // ENGINE at bits [31:30]
    return w;
}

// Build aligned 32-dword IB
void build_aligned_ib(uint32_t *ib, uint64_t signal_va) {
    int idx = 0;
    
    // 5 functional dwords
    ib[idx++] = pkt3_header(IT_WRITE_DATA, 4);  // COUNT=4 for WRITE_DATA!
    ib[idx++] = write_data_control();
    ib[idx++] = (uint32_t)(signal_va & 0xFFFFFFFF);
    ib[idx++] = (uint32_t)((signal_va >> 32) & 0xFFFF);
    ib[idx++] = 0xCAFEBABE;
    
    // Pad to 32 dwords with one big NOP
    // PKT3_NOP needs count = 32 - 6 (5 above + 1 NOP header) = 26
    ib[idx++] = pkt3_header(IT_NOP, 25);  // 25 payload dwords
    
    // Fill rest with zeros (NOP payload)
    while (idx < 32) {
        ib[idx++] = 0x00000000;
    }
    
    printf("Built 32-dword aligned IB:\n");
    for (int i = 0; i < 32; i++) {
        if (i < 5 || i == 5) {
            printf("  DW[%02d] = 0x%08X", i, ib[i]);
            if (i == 0) printf(" <- WRITE_DATA header (count=4)");
            else if (i == 1) printf(" <- Control");
            else if (i == 2) printf(" <- Address low");
            else if (i == 3) printf(" <- Address high");
            else if (i == 4) printf(" <- Data");
            else if (i == 5) printf(" <- NOP header (count=25)");
            printf("\n");
        } else if (i == 6) {
            printf("  DW[6..31] = 0x00000000 (NOP padding)\n");
            break;
        }
    }
}

// BO list operations
#define AMDGPU_BO_LIST_OP_CREATE 0
#define AMDGPU_BO_LIST_OP_DESTROY 1

int main() {
    printf("=== Mini's Fixed Alignment Test ===\n");
    printf("Fixing: count=4 and 32-dword alignment\n\n");
    
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
    signal_ptr[0] = 0x12345678;  // Initial value
    printf("✓ Signal initialized to 0x%08X\n", signal_ptr[0]);
    
    // Map signal to VA with READ|WRITE
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = signal_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE;
    va_args.va_address = 0x800000000;
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    uint64_t signal_va = va_args.va_address;
    printf("✓ Signal VA = 0x%016llX\n", (unsigned long long)signal_va);
    
    // 4. Create IB BO (must be 32-dword aligned size)
    gem_args.in.bo_size = 32 * 4;  // 32 dwords = 128 bytes
    gem_args.in.alignment = 128;   // 32-dword alignment
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t ib_handle = gem_args.out.handle;
    printf("\n✓ IB BO created: handle=%u, size=128 bytes\n", ib_handle);
    
    // Build and upload IB
    mmap_args.in.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    uint32_t* ib_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                           fd, mmap_args.out.addr_ptr);
    
    build_aligned_ib(ib_ptr, signal_va);
    munmap(ib_ptr, 4096);
    
    // Map IB to VA (ensure 32-dword aligned address)
    va_args.handle = ib_handle;
    va_args.flags = AMDGPU_VM_PAGE_READABLE;
    va_args.va_address = 0x800001000;  // This is 32-dword aligned
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    uint64_t ib_va = va_args.va_address;
    printf("✓ IB VA = 0x%016llX (32-dword aligned)\n", (unsigned long long)ib_va);
    
    // 5. Create BO list = [ib_bo, signal_bo]
    uint32_t handles[2] = {ib_handle, signal_handle};
    
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    bo_list_args.in.bo_number = 2;
    bo_list_args.in.bo_info_size = sizeof(uint32_t);
    bo_list_args.in.bo_info_ptr = (uintptr_t)handles;
    
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    if (ret < 0) {
        // Try with bo_list_entry
        struct drm_amdgpu_bo_list_entry bo_list[] = {
            {ib_handle, 0},
            {signal_handle, 0}
        };
        bo_list_args.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
        bo_list_args.in.bo_info_ptr = (uintptr_t)bo_list;
        ret = ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    }
    
    if (ret < 0) {
        perror("Failed to create BO list");
        return 1;
    }
    
    // 6. Submit CS
    printf("\nSubmitting CS:\n");
    
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    // IB chunk
    ib_chunk._pad = 0;
    ib_chunk.flags = 0;
    ib_chunk.va_start = ib_va;
    ib_chunk.ib_bytes = 32 * 4;  // 32 dwords = 128 bytes
    ib_chunk.ip_type = AMDGPU_HW_IP_GFX;
    ib_chunk.ip_instance = 0;
    ib_chunk.ring = 0;
    
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(struct drm_amdgpu_cs_chunk_ib) / 4;
    chunks[0].chunk_data = (uintptr_t)&ib_chunk;
    
    // Print CS details
    printf("  IB chunk: ip_type=GFX, ring=0\n");
    printf("  IB chunk: va_start=0x%llX, ib_bytes=%u\n", 
           (unsigned long long)ib_chunk.va_start, ib_chunk.ib_bytes);
    printf("  Chunk[0]: id=%u, length_dw=%u\n", 
           chunks[0].chunk_id, chunks[0].length_dw);
    
    uint64_t chunk_ptrs[1] = {(uintptr_t)&chunks[0]};
    
    union drm_amdgpu_cs cs_args = {0};
    cs_args.in.ctx_id = ctx_id;
    cs_args.in.bo_list_handle = bo_list_args.out.list_handle;
    cs_args.in.num_chunks = 1;
    cs_args.in.chunks = (uintptr_t)chunk_ptrs;
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    if (ret < 0) {
        printf("❌ CS submit failed: %s\n", strerror(errno));
        return 1;
    }
    
    printf("✓ CS submitted! seq=%llu\n", (unsigned long long)cs_args.out.handle);
    
    // Wait
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = cs_args.out.handle;
    wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
    wait_args.in.ip_instance = 0;
    wait_args.in.ring = 0;
    wait_args.in.ctx_id = ctx_id;
    wait_args.in.timeout = 1000000000;
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
    printf("Wait: ret=%d, status=%lld\n", ret, (long long)wait_args.out.status);
    
    // Check result
    printf("\nResult:\n");
    printf("  Signal[0] = 0x%08X (expected 0xCAFEBABE)\n", signal_ptr[0]);
    
    if (signal_ptr[0] == 0xCAFEBABE) {
        printf("\n✅ SUCCESS! GFX ring executed our packet!\n");
        printf("The fixes worked:\n");
        printf("  - count=4 (not 3)\n");
        printf("  - 32-dword IB alignment\n");
        return 0;
    } else {
        printf("\n❌ Still no execution\n");
        printf("Next: Check CP_HQD_PQ_RPTR/WPTR\n");
        return 1;
    }
}