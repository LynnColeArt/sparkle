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
#define IT_RELEASE_MEM      0x49
#define DST_SEL_MEM         5
#define ENGINE_ME           1

// GFX preamble packets
#define IT_CONTEXT_CONTROL  0x28
#define IT_CLEAR_STATE      0x12
#define IT_SET_CONFIG_REG   0x68

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
    printf("=== Preamble Init Test ===\n");
    printf("Testing if GFX ring needs initialization preamble\n\n");
    
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
    
    // 3. Create BO
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = 8192;  // Larger for preamble
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args) < 0) {
        perror("Failed to create BO");
        close(fd);
        return 1;
    }
    uint32_t bo_handle = gem_args.out.handle;
    printf("✓ BO: handle=%u, size=8KB\n", bo_handle);
    
    // 4. Map BO for CPU
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = bo_handle;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args) < 0) {
        perror("Failed to mmap");
        close(fd);
        return 1;
    }
    uint32_t* bo_cpu = mmap(NULL, 8192, PROT_READ | PROT_WRITE, MAP_SHARED,
                           fd, mmap_args.out.addr_ptr);
    
    // Initialize target area
    bo_cpu[0] = 0xDEADBEEF;
    printf("✓ BO[0] = 0x%08X\n", bo_cpu[0]);
    
    // 5. Map to GPU VA
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = bo_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE | AMDGPU_VM_PAGE_EXECUTABLE;
    va_args.va_address = 0x100000000;  // 4GB
    va_args.offset_in_bo = 0;
    va_args.map_size = 8192;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args) < 0) {
        perror("Failed to map VA");
        close(fd);
        return 1;
    }
    uint64_t bo_va = va_args.va_address;
    printf("✓ VA mapped: 0x%016llx\n", (unsigned long long)bo_va);
    
    // 6. Build IB with GFX preamble
    uint32_t* ib = &bo_cpu[512];  // IB at offset 2KB
    int idx = 0;
    
    printf("\nBuilding IB with initialization preamble...\n");
    
    // CONTEXT_CONTROL - set default state
    ib[idx++] = pkt3_header(IT_CONTEXT_CONTROL, 1);
    ib[idx++] = 0x80000000;  // LOAD_ENABLE
    ib[idx++] = 0x80000000;  // SHADOW_ENABLE
    
    // CLEAR_STATE - reset to known state
    ib[idx++] = pkt3_header(IT_CLEAR_STATE, 0);
    ib[idx++] = 0;  // cmd = 0
    
    // Simple WRITE_DATA
    ib[idx++] = pkt3_header(IT_WRITE_DATA, 4);
    ib[idx++] = write_data_control();
    ib[idx++] = (uint32_t)(bo_va & 0xFFFFFFFF);
    ib[idx++] = (uint32_t)((bo_va >> 32) & 0xFFFF);
    ib[idx++] = 0xCAFEBABE;
    
    // RELEASE_MEM to flush caches
    ib[idx++] = pkt3_header(IT_RELEASE_MEM, 6);
    ib[idx++] = 0x6;  // EVENT_TYPE = CS_DONE, EVENT_INDEX = 6
    ib[idx++] = 0;    // No interrupt
    ib[idx++] = 0;    // DST_SEL = 0 (no write)
    ib[idx++] = 0;    // ADDRESS_LO
    ib[idx++] = 0;    // ADDRESS_HI
    ib[idx++] = 0;    // DATA_LO
    ib[idx++] = 0;    // DATA_HI
    
    // Pad to 32 dwords
    while (idx < 32) {
        ib[idx++] = pkt3_header(IT_NOP, 0);
        ib[idx++] = 0;
    }
    
    uint64_t ib_va = bo_va + 2048;  // IB at 2KB offset
    
    printf("✓ IB built with %d dwords\n", idx);
    printf("  - CONTEXT_CONTROL\n");
    printf("  - CLEAR_STATE\n");
    printf("  - WRITE_DATA\n");
    printf("  - RELEASE_MEM (cache flush)\n");
    
    // 7. Create BO list
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
    printf("\n✓ BO list: handle=%u\n", bo_list_args.out.list_handle);
    
    // 8. Submit CS
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
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
    cs_args.in.num_chunks = 1;
    cs_args.in.chunks = (uintptr_t)chunk_ptrs;
    
    // Flush before submit
    msync(bo_cpu, 8192, MS_SYNC);
    
    printf("\nSubmitting CS with preamble...\n");
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
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
    if (ret < 0) {
        printf("Wait failed: %s\n", strerror(errno));
    } else {
        printf("✓ Wait completed, status=%lld\n", (long long)wait_args.out.status);
    }
    
    // 10. Check result
    printf("\n=== RESULT ===\n");
    printf("BO[0] = 0x%08X (expected 0xCAFEBABE)\n", bo_cpu[0]);
    
    if (bo_cpu[0] == 0xCAFEBABE) {
        printf("\n✅ SUCCESS! Preamble initialization worked!\n");
    } else {
        printf("\n❌ FAILED: Still not executing\n");
        printf("Even with GFX preamble, packets don't execute\n");
    }
    
    close(fd);
    return 0;
}