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
#define IT_SET_UCONFIG_REG  0x79
#define IT_NOP              0x10

// CP_SCRATCH0 register
#define CP_SCRATCH0         0x34D0

// Build PKT3 header
static inline uint32_t pkt3_header(uint32_t opcode, uint32_t count) {
    return (PKT3 << 30) | (opcode << 8) | count;
}

int main() {
    printf("=== CP_SCRATCH Test - Prove GFX Ring Execution ===\n\n");
    
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
    printf("✓ Context: %u\n", ctx_id);
    
    // 3. Create IB BO (only need IB, no signal BO)
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = 128;  // 32 dwords
    gem_args.in.alignment = 128;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t ib_handle = gem_args.out.handle;
    
    // Build IB
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    uint32_t* ib = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                       fd, mmap_args.out.addr_ptr);
    
    int idx = 0;
    // SET_UCONFIG_REG CP_SCRATCH0 = 0xA5A5A5A5
    ib[idx++] = pkt3_header(IT_SET_UCONFIG_REG, 1);
    ib[idx++] = (CP_SCRATCH0 - 0x30000) >> 2;  // UCONFIG offset
    ib[idx++] = 0xA5A5A5A5;                   // Test value
    
    // Pad to 32 dwords with NOP
    ib[idx++] = pkt3_header(IT_NOP, 27);
    while (idx < 32) ib[idx++] = 0;
    
    printf("✓ IB built:\n");
    printf("  DW[0] = 0x%08X <- SET_UCONFIG_REG header\n", ib[0]);
    printf("  DW[1] = 0x%08X <- CP_SCRATCH0 offset\n", ib[1]);
    printf("  DW[2] = 0x%08X <- Test value\n", ib[2]);
    
    munmap(ib, 4096);
    
    // Map IB VA
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = ib_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE;
    va_args.va_address = 0x800001000;
    va_args.offset_in_bo = 0;
    va_args.map_size = 128;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    printf("✓ IB VA: 0x%llx\n", (unsigned long long)va_args.va_address);
    
    // 4. Create BO list (legacy pattern - only IB BO)
    uint32_t handles[1] = {ib_handle};
    
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = 0; // AMDGPU_BO_LIST_OP_CREATE
    bo_list_args.in.bo_number = 1;
    bo_list_args.in.bo_info_size = sizeof(uint32_t);
    bo_list_args.in.bo_info_ptr = (uintptr_t)handles;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args) < 0) {
        perror("Failed to create BO list");
        return 1;
    }
    printf("✓ BO list: %u\n", bo_list_args.out.list_handle);
    
    // 5. Submit CS (legacy pattern - bo_list_handle, no BO_HANDLES chunk)
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    // IB chunk
    ib_chunk.va_start = va_args.va_address;
    ib_chunk.ib_bytes = 128;
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
    
    printf("\nChecking CP_SCRATCH0 before submit...\n");
    system("umr -r *.*.CP_SCRATCH0 2>/dev/null | grep -E '(0x[0-9a-fA-F]+|CP_SCRATCH0)' || echo 'CP_SCRATCH0: Unable to read'");
    
    printf("\nSubmitting CS...\n");
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
    
    printf("\nChecking CP_SCRATCH0 after submit...\n");
    system("umr -r *.*.CP_SCRATCH0 2>/dev/null | grep -E '(0x[0-9a-fA-F]+|CP_SCRATCH0)' || echo 'CP_SCRATCH0: Unable to read'");
    
    printf("\nExpected: 0xA5A5A5A5\n");
    printf("\nIf CP_SCRATCH0 changed to 0xA5A5A5A5:\n");
    printf("  ✅ Ring is executing! Problem is in WRITE_DATA control word\n");
    printf("If CP_SCRATCH0 is unchanged:\n");
    printf("  ❌ Ring is not reading IB - deeper submission issue\n");
    
    close(fd);
    return 0;
}