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
#define DST_SEL_MEM         5
#define ENGINE_ME           1

// Build PKT3 header
static inline uint32_t pkt3_header(uint32_t opcode, uint32_t count) {
    return (PKT3 << 30) | (opcode << 8) | count;
}

// Build WRITE_DATA control with proper DST_SEL
static inline uint32_t write_data_control() {
    uint32_t control = 0;
    control |= (DST_SEL_MEM & 0xF) << 8;   // DST_SEL at bits [11:8]
    control |= (1u << 16);                  // WR_ONE_ADDR at bit 16
    control |= (0u << 20);                  // WR_CONFIRM at bit 20  
    control |= (ENGINE_ME & 0x3) << 30;     // ENGINE at bits [31:30]
    return control;
}

// Dump ring before/after
void dump_gfx_ring(const char* when) {
    printf("\n=== %s ===\n", when);
    // renderD129 (iGPU) is dri/1, not dri/0!
    system("sudo od -Ax -tx4 -N 256 -v /sys/kernel/debug/dri/1/amdgpu_ring_gfx_0.0.0 2>/dev/null | head -10");
}

int main() {
    printf("=== Mini's 4KB Fix Test ===\n");
    printf("Key fixes: 4KB IB mapping, legacy bo_list, DST_SEL=MEM\n\n");
    
    // 1. Open device
    int fd = open("/dev/dri/renderD129", O_RDWR);
    if (fd < 0) {
        perror("Failed to open GPU");
        return 1;
    }
    printf("✓ Opened renderD129 (iGPU)\n");
    
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
    
    // 3. Create signal BO (4KB, GTT)
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
    printf("✓ Signal BO: handle=%u, size=4KB, GTT\n", signal_handle);
    
    // Map and init signal
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = signal_handle;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args) < 0) {
        perror("Failed to mmap signal");
        close(fd);
        return 1;
    }
    uint32_t* signal_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                               fd, mmap_args.out.addr_ptr);
    signal_ptr[0] = 0x12345678;
    printf("✓ Signal initialized: 0x%08X\n", signal_ptr[0]);
    
    // Map signal VA (4KB aligned)
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = signal_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE;
    va_args.va_address = 0x800000000;  // 4KB aligned
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args) < 0) {
        perror("Failed to map signal VA");
        close(fd);
        return 1;
    }
    uint64_t signal_va = va_args.va_address;
    printf("✓ Signal VA: 0x%016llx (4KB aligned)\n", (unsigned long long)signal_va);
    
    // 4. Create IB BO (4KB, GTT) - CRITICAL FIX!
    gem_args.in.bo_size = 4096;  // Full page size
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args) < 0) {
        perror("Failed to create IB BO");
        close(fd);
        return 1;
    }
    uint32_t ib_handle = gem_args.out.handle;
    printf("✓ IB BO: handle=%u, size=4KB, GTT\n", ib_handle);
    
    // Build IB
    mmap_args.in.handle = ib_handle;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args) < 0) {
        perror("Failed to mmap IB");
        close(fd);
        return 1;
    }
    uint32_t* ib = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                       fd, mmap_args.out.addr_ptr);
    
    // Build WRITE_DATA packet with DST_SEL=MEM
    int idx = 0;
    ib[idx++] = 0xC0003704;               // PKT3_WRITE_DATA, count=4
    ib[idx++] = 0x40010500;               // ENGINE=ME | WR_ONE_ADDR | DST_SEL=MEM
    ib[idx++] = (uint32_t)(signal_va & 0xFFFFFFFF);
    ib[idx++] = (uint32_t)((signal_va >> 32) & 0xFFFF);
    ib[idx++] = 0xCAFEBABE;
    
    // Pad to 32 dwords with big NOP
    ib[idx++] = pkt3_header(IT_NOP, 25);  // 26 more dwords
    while (idx < 32) ib[idx++] = 0;
    
    printf("\n✓ IB content (32 dwords):\n");
    printf("  DW[0] = 0x%08X <- WRITE_DATA header\n", ib[0]);
    printf("  DW[1] = 0x%08X <- Control (DST_SEL=MEM)\n", ib[1]);
    printf("  DW[2] = 0x%08X <- VA low\n", ib[2]);
    printf("  DW[3] = 0x%08X <- VA high\n", ib[3]);
    printf("  DW[4] = 0x%08X <- Data\n", ib[4]);
    printf("  DW[5] = 0x%08X <- NOP padding\n", ib[5]);
    
    munmap(ib, 4096);
    
    // Map IB VA (4KB size, 4KB aligned) - CRITICAL FIX!
    memset(&va_args, 0, sizeof(va_args));
    va_args.handle = ib_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE;  // CP fetch
    va_args.va_address = 0x800004000;  // 4KB aligned, different from signal
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;  // Full page!
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args) < 0) {
        perror("Failed to map IB VA");
        close(fd);
        return 1;
    }
    uint64_t ib_va = va_args.va_address;
    printf("✓ IB VA: 0x%016llx (4KB aligned, 4KB size)\n", 
           (unsigned long long)ib_va);
    
    // 5. Create BO list (legacy pattern)
    uint32_t handles[2] = {ib_handle, signal_handle};
    
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = 0;  // AMDGPU_BO_LIST_OP_CREATE
    bo_list_args.in.bo_number = 2;
    bo_list_args.in.bo_info_size = sizeof(uint32_t);
    bo_list_args.in.bo_info_ptr = (uintptr_t)handles;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args) < 0) {
        perror("Failed to create BO list");
        close(fd);
        return 1;
    }
    printf("✓ BO list: handle=%u, contains {%u, %u}\n", 
           bo_list_args.out.list_handle, handles[0], handles[1]);
    
    // 6. Build CS submission (legacy - bo_list_handle, NO BO_HANDLES chunk)
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    // IB chunk
    ib_chunk._pad = 0;
    ib_chunk.flags = 0;
    ib_chunk.va_start = ib_va;
    ib_chunk.ib_bytes = 128;  // 32 dwords
    ib_chunk.ip_type = AMDGPU_HW_IP_GFX;
    ib_chunk.ip_instance = 0;
    ib_chunk.ring = 0;
    
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(struct drm_amdgpu_cs_chunk_ib) / 4;
    chunks[0].chunk_data = (uintptr_t)&ib_chunk;
    
    uint64_t chunk_ptrs[1] = {(uintptr_t)&chunks[0]};
    
    union drm_amdgpu_cs cs_args = {0};
    cs_args.in.ctx_id = ctx_id;
    cs_args.in.bo_list_handle = bo_list_args.out.list_handle;
    cs_args.in.num_chunks = 1;  // Only IB chunk!
    cs_args.in.chunks = (uintptr_t)chunk_ptrs;
    
    printf("\n=== CS SUBMISSION ===\n");
    printf("ctx_id = %u\n", cs_args.in.ctx_id);
    printf("bo_list_handle = %u\n", cs_args.in.bo_list_handle);
    printf("num_chunks = %u\n", cs_args.in.num_chunks);
    printf("chunks[0].chunk_id = %u (IB)\n", chunks[0].chunk_id);
    printf("chunks[0].length_dw = %u\n", chunks[0].length_dw);
    printf("ib_chunk.va_start = 0x%llx\n", (unsigned long long)ib_chunk.va_start);
    printf("ib_chunk.ib_bytes = %u\n", ib_chunk.ib_bytes);
    printf("ib_chunk.ip_type = %u (GFX)\n", ib_chunk.ip_type);
    
    // Check ring before submit
    dump_gfx_ring("BEFORE SUBMIT");
    
    printf("\nSubmitting CS...\n");
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    if (ret < 0) {
        printf("❌ CS submit failed: %s (errno=%d)\n", strerror(errno), errno);
        close(fd);
        return 1;
    }
    
    printf("✓ CS submitted! seq=%llu\n", (unsigned long long)cs_args.out.handle);
    
    // Check ring after submit
    dump_gfx_ring("AFTER SUBMIT");
    
    // Wait for completion
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = cs_args.out.handle;
    wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
    wait_args.in.ctx_id = ctx_id;
    wait_args.in.timeout = 1000000000;
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
    printf("✓ Wait completed: ret=%d, status=%lld\n", 
           ret, (long long)wait_args.out.status);
    
    // Check result
    printf("\n=== RESULT ===\n");
    printf("Signal[0] = 0x%08X (expected 0xCAFEBABE)\n", signal_ptr[0]);
    
    if (signal_ptr[0] == 0xCAFEBABE) {
        printf("\n✅ SUCCESS! Ring executed our packet!\n");
        printf("The 4KB IB mapping was the key!\n");
    } else {
        printf("\n❌ Still no execution\n");
        printf("Check the ring dump to see if our packet is there\n");
    }
    
    close(fd);
    return 0;
}