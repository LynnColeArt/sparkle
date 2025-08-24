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
    printf("=== Single BO Self-Write Test (Mini's surgical repro) ===\n");
    printf("One BO serves as both IB and write target\n\n");
    
    // 1. Open render node (not card node!)
    int fd = open("/dev/dri/renderD129", O_RDWR);
    if (fd < 0) {
        perror("Failed to open render node");
        return 1;
    }
    printf("✓ Opened /dev/dri/renderD129\n");
    
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
    
    // 3. Create single BO (4KB, GTT)
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = 4096;
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args) < 0) {
        perror("Failed to create BO");
        close(fd);
        return 1;
    }
    uint32_t bo_handle = gem_args.out.handle;
    printf("✓ Single BO created: handle=%u, size=4KB\n", bo_handle);
    
    // 4. Map BO for CPU access
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = bo_handle;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args) < 0) {
        perror("Failed to mmap BO");
        close(fd);
        return 1;
    }
    
    uint32_t* bo_cpu = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                           fd, mmap_args.out.addr_ptr);
    if (bo_cpu == MAP_FAILED) {
        perror("mmap failed");
        close(fd);
        return 1;
    }
    
    // Initialize first dword (write target)
    bo_cpu[0] = 0xDEADBEEF;
    printf("✓ BO[0] initialized to: 0x%08X\n", bo_cpu[0]);
    
    // 5. Map BO to GPU VA (immediately, no delayed updates!)
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = bo_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE;
    va_args.va_address = 0x800000000;  // Conservative, 4KB aligned
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args) < 0) {
        perror("Failed to map BO VA");
        close(fd);
        return 1;
    }
    uint64_t bo_va = va_args.va_address;
    printf("✓ BO mapped to VA: 0x%016llx (immediate binding)\n", 
           (unsigned long long)bo_va);
    
    // 6. Build IB in the same BO (starting at offset 128)
    uint32_t* ib = &bo_cpu[32];  // Start IB at dword 32 (offset 128)
    int idx = 0;
    
    // WRITE_DATA to dword 0 of same BO
    ib[idx++] = pkt3_header(IT_WRITE_DATA, 4);
    ib[idx++] = write_data_control();
    ib[idx++] = (uint32_t)(bo_va & 0xFFFFFFFF);      // Target: start of BO
    ib[idx++] = (uint32_t)((bo_va >> 32) & 0xFFFF);
    ib[idx++] = 0xCAFEBABE;  // Data to write
    
    // Pad to 32 dwords
    ib[idx++] = pkt3_header(IT_NOP, 25);
    while (idx < 32) ib[idx++] = 0;
    
    printf("\n✓ IB built at offset 128:\n");
    printf("  IB[0] = 0x%08X <- WRITE_DATA header\n", ib[0]);
    printf("  IB[1] = 0x%08X <- Control (DST_SEL=MEM)\n", ib[1]);
    printf("  IB[2] = 0x%08X <- Target VA low (same BO!)\n", ib[2]);
    printf("  IB[3] = 0x%08X <- Target VA high\n", ib[3]);
    printf("  IB[4] = 0x%08X <- Data\n", ib[4]);
    
    uint64_t ib_va = bo_va + 128;  // IB starts at offset 128
    printf("  IB VA: 0x%016llx\n", (unsigned long long)ib_va);
    
    // 7. Create BO list (legacy mode - just this one BO)
    uint32_t handles[1] = {bo_handle};
    
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    bo_list_args.in.bo_number = 1;
    bo_list_args.in.bo_info_size = sizeof(uint32_t);
    bo_list_args.in.bo_info_ptr = (uintptr_t)handles;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args) < 0) {
        perror("Failed to create BO list");
        close(fd);
        return 1;
    }
    printf("\n✓ BO list created (legacy mode):\n");
    printf("  bo_list_handle = %u\n", bo_list_args.out.list_handle);
    printf("  Contains single BO: handle=%u\n", handles[0]);
    
    // 8. Build CS with ONLY IB chunk (no BO_HANDLES chunk!)
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    ib_chunk._pad = 0;
    ib_chunk.flags = 0;  // No special flags for now
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
    cs_args.in.num_chunks = 1;
    cs_args.in.chunks = (uintptr_t)chunk_ptrs;
    
    printf("\n=== CS SUBMISSION (Legacy BO list mode) ===\n");
    printf("ctx_id = %u\n", cs_args.in.ctx_id);
    printf("bo_list_handle = %u\n", cs_args.in.bo_list_handle);
    printf("num_chunks = 1 (IB only)\n");
    printf("IB chunk:\n");
    printf("  va_start = 0x%016llx\n", (unsigned long long)ib_chunk.va_start);
    printf("  ib_bytes = %u\n", ib_chunk.ib_bytes);
    printf("  ip_type = %u (GFX)\n", ib_chunk.ip_type);
    
    // Flush CPU caches before submit
    msync(bo_cpu, 4096, MS_SYNC);
    
    printf("\nSubmitting...\n");
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    if (ret < 0) {
        printf("❌ CS submit failed: %s (errno=%d)\n", strerror(errno), errno);
        close(fd);
        return 1;
    }
    
    printf("✓ CS submitted! seq=%llu\n", (unsigned long long)cs_args.out.handle);
    
    // 9. Wait for completion
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = cs_args.out.handle;
    wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
    wait_args.in.ctx_id = ctx_id;
    wait_args.in.timeout = 1000000000;  // 1 second
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
    printf("✓ Wait completed: ret=%d, status=%lld\n", 
           ret, (long long)wait_args.out.status);
    
    // 10. Check result
    printf("\n=== RESULT ===\n");
    printf("BO[0] = 0x%08X (expected 0xCAFEBABE)\n", bo_cpu[0]);
    
    if (bo_cpu[0] == 0xCAFEBABE) {
        printf("\n✅ SUCCESS! GPU executed our IB!\n");
        printf("The single-BO self-write worked!\n");
    } else if (bo_cpu[0] == 0xDEADBEEF) {
        printf("\n❌ FAILED: Value unchanged\n");
        printf("Job never made it to GPU (residency/VA/CS wiring issue)\n");
    } else {
        printf("\n⚠️  UNEXPECTED: Value changed but not to expected\n");
    }
    
    // Cleanup
    munmap(bo_cpu, 4096);
    close(fd);
    return 0;
}