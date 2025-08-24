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

// Build WRITE_DATA control
static inline uint32_t write_data_control() {
    uint32_t control = 0;
    control |= (ENGINE_ME & 0x3) << 30;     // ENGINE at bits [31:30]
    control |= (1u << 16);                  // WR_ONE_ADDR at bit 16
    control |= (DST_SEL_MEM & 0xF) << 8;    // DST_SEL at bits [11:8]
    return control;
}

int main() {
    printf("=== Explicit Residency Test ===\n");
    printf("Testing with both BO list AND BO_HANDLES chunk\n\n");
    
    // 1. Open render node
    int fd = open("/dev/dri/renderD129", O_RDWR);
    if (fd < 0) {
        perror("Failed to open render node");
        return 1;
    }
    printf("✓ Opened renderD129\n");
    
    // 2. Create context
    union drm_amdgpu_ctx ctx_args = {0};
    ctx_args.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args) < 0) {
        perror("Failed to create context");
        close(fd);
        return 1;
    }
    uint32_t ctx_id = ctx_args.out.alloc.ctx_id;
    printf("✓ Context: %u\n", ctx_id);
    
    // 3. Create two BOs: one for IB, one for target
    union drm_amdgpu_gem_create gem_args = {0};
    
    // IB BO
    gem_args.in.bo_size = 4096;
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    gem_args.in.domain_flags = 0;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args) < 0) {
        perror("Failed to create IB BO");
        close(fd);
        return 1;
    }
    uint32_t ib_handle = gem_args.out.handle;
    printf("✓ IB BO: handle=%u\n", ib_handle);
    
    // Target BO
    gem_args = (union drm_amdgpu_gem_create){0};
    gem_args.in.bo_size = 4096;
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args) < 0) {
        perror("Failed to create target BO");
        close(fd);
        return 1;
    }
    uint32_t target_handle = gem_args.out.handle;
    printf("✓ Target BO: handle=%u\n", target_handle);
    
    // 4. Map BOs for CPU
    union drm_amdgpu_gem_mmap mmap_args = {0};
    
    // Map IB
    mmap_args.in.handle = ib_handle;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args) < 0) {
        perror("Failed to mmap IB");
        close(fd);
        return 1;
    }
    uint32_t* ib_cpu = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                           fd, mmap_args.out.addr_ptr);
    
    // Map target
    mmap_args.in.handle = target_handle;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args) < 0) {
        perror("Failed to mmap target");
        close(fd);
        return 1;
    }
    uint32_t* target_cpu = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                              fd, mmap_args.out.addr_ptr);
    
    // Initialize target
    target_cpu[0] = 0xDEADBEEF;
    printf("✓ Target[0] = 0x%08X\n", target_cpu[0]);
    
    // 5. Map to GPU VA - try lower addresses
    struct drm_amdgpu_gem_va va_args = {0};
    
    // Map IB at 0x400000000 (16GB)
    va_args.handle = ib_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE | AMDGPU_VM_PAGE_EXECUTABLE;
    va_args.va_address = 0x400000000;
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args) < 0) {
        perror("Failed to map IB VA");
        close(fd);
        return 1;
    }
    uint64_t ib_va = va_args.va_address;
    printf("✓ IB VA: 0x%016llx\n", (unsigned long long)ib_va);
    
    // Map target at 0x400001000
    va_args.handle = target_handle;
    va_args.va_address = 0x400001000;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args) < 0) {
        perror("Failed to map target VA");
        close(fd);
        return 1;
    }
    uint64_t target_va = va_args.va_address;
    printf("✓ Target VA: 0x%016llx\n", (unsigned long long)target_va);
    
    // 6. Build IB
    int idx = 0;
    
    // WRITE_DATA to target
    ib_cpu[idx++] = pkt3_header(IT_WRITE_DATA, 4);
    ib_cpu[idx++] = write_data_control();
    ib_cpu[idx++] = (uint32_t)(target_va & 0xFFFFFFFF);
    ib_cpu[idx++] = (uint32_t)((target_va >> 32) & 0xFFFF);
    ib_cpu[idx++] = 0xCAFEBABE;
    
    // Pad to 32 dwords
    ib_cpu[idx++] = pkt3_header(IT_NOP, 25);
    while (idx < 32) ib_cpu[idx++] = 0;
    
    printf("\n✓ IB built:\n");
    printf("  IB[0] = 0x%08X <- WRITE_DATA\n", ib_cpu[0]);
    printf("  IB[1] = 0x%08X <- Control\n", ib_cpu[1]);
    printf("  IB[2] = 0x%08X <- Target VA low\n", ib_cpu[2]);
    printf("  IB[3] = 0x%08X <- Target VA high\n", ib_cpu[3]);
    printf("  IB[4] = 0x%08X <- Data\n", ib_cpu[4]);
    
    // 7. Create BO list with BOTH BOs
    uint32_t handles[2] = {ib_handle, target_handle};
    
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    bo_list_args.in.bo_number = 2;
    bo_list_args.in.bo_info_size = sizeof(uint32_t);
    bo_list_args.in.bo_info_ptr = (uintptr_t)handles;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args) < 0) {
        perror("Failed to create BO list");
        close(fd);
        return 1;
    }
    printf("\n✓ BO list: handle=%u (contains both BOs)\n", bo_list_args.out.list_handle);
    
    // 8. Build CS with IB chunk AND BO_HANDLES chunk
    struct drm_amdgpu_cs_chunk chunks[2] = {0};
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
    
    // BO handles chunk
    chunks[1].chunk_id = AMDGPU_CHUNK_ID_BO_HANDLES;
    chunks[1].length_dw = 2;  // 2 handles
    chunks[1].chunk_data = (uintptr_t)handles;
    
    uint64_t chunk_ptrs[2] = {(uintptr_t)&chunks[0], (uintptr_t)&chunks[1]};
    
    union drm_amdgpu_cs cs_args = {0};
    cs_args.in.ctx_id = ctx_id;
    cs_args.in.bo_list_handle = bo_list_args.out.list_handle;
    cs_args.in.num_chunks = 2;
    cs_args.in.chunks = (uintptr_t)chunk_ptrs;
    
    printf("\n=== CS SUBMISSION ===\n");
    printf("Using BOTH bo_list AND BO_HANDLES chunk\n");
    printf("IB VA: 0x%016llx\n", (unsigned long long)ib_va);
    printf("Target VA: 0x%016llx\n", (unsigned long long)target_va);
    
    // Flush before submit
    msync(ib_cpu, 4096, MS_SYNC);
    msync(target_cpu, 4096, MS_SYNC);
    
    printf("\nSubmitting...\n");
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    if (ret < 0) {
        printf("❌ CS submit failed: %s\n", strerror(errno));
        close(fd);
        return 1;
    }
    
    printf("✓ CS submitted! seq=%llu\n", (unsigned long long)cs_args.out.handle);
    
    // 9. Wait
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = cs_args.out.handle;
    wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
    wait_args.in.ctx_id = ctx_id;
    wait_args.in.timeout = 1000000000;
    
    ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
    
    // 10. Check result
    printf("\n=== RESULT ===\n");
    printf("Target[0] = 0x%08X (expected 0xCAFEBABE)\n", target_cpu[0]);
    
    if (target_cpu[0] == 0xCAFEBABE) {
        printf("\n✅ SUCCESS! Explicit residency worked!\n");
    } else {
        printf("\n❌ FAILED: Still not executing\n");
    }
    
    close(fd);
    return 0;
}