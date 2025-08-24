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

// SDMA v5 packet opcodes for Raphael/gfx1036
#define SDMA_OPCODE_WRITE           2
#define SDMA_WRITE_SUB_OPCODE_LINEAR 0

// Build SDMA v5 write packet
static void build_sdma_write_packet(uint32_t *ib, uint64_t dst_va, uint32_t data) {
    // SDMA v5 WRITE LINEAR packet format (6 dwords):
    // DW0: header
    // DW1: dst_addr_lo
    // DW2: dst_addr_hi
    // DW3: count - 1 (writing 1 dword = count 0)
    // DW4: reserved (0)
    // DW5: data
    
    // Header: opcode=WRITE(2), sub_opcode=LINEAR(0)
    ib[0] = (SDMA_OPCODE_WRITE << 0) | (SDMA_WRITE_SUB_OPCODE_LINEAR << 8);
    ib[1] = (uint32_t)(dst_va & 0xFFFFFFFF);
    ib[2] = (uint32_t)(dst_va >> 32);
    ib[3] = 0;  // count - 1 (writing 1 dword)
    ib[4] = 0;  // reserved
    ib[5] = data;
}

int main() {
    printf("=== SDMA Litmus Test (Mini's control experiment) ===\n");
    printf("Tests if VM/residency work by using SDMA instead of GFX\n\n");
    
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
    printf("✓ BO created: handle=%u\n", bo_handle);
    
    // 4. Map BO for CPU
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = bo_handle;
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args) < 0) {
        perror("Failed to mmap");
        close(fd);
        return 1;
    }
    
    uint32_t* bo_cpu = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                           fd, mmap_args.out.addr_ptr);
    
    // Initialize target
    bo_cpu[0] = 0x11111111;
    printf("✓ BO[0] = 0x%08X\n", bo_cpu[0]);
    
    // 5. Map BO to GPU VA
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = bo_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE;
    va_args.va_address = 0x800000000;
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args) < 0) {
        perror("Failed to map VA");
        close(fd);
        return 1;
    }
    uint64_t bo_va = va_args.va_address;
    printf("✓ VA mapped: 0x%016llx\n", (unsigned long long)bo_va);
    
    // 6. Build SDMA IB (at offset 64 in same BO)
    uint32_t* ib = &bo_cpu[16];  // Start at dword 16
    build_sdma_write_packet(ib, bo_va, 0xCAFEBABE);
    
    printf("\n✓ SDMA packet built:\n");
    printf("  IB[0] = 0x%08X <- Header (WRITE LINEAR)\n", ib[0]);
    printf("  IB[1] = 0x%08X <- Dst VA low\n", ib[1]);
    printf("  IB[2] = 0x%08X <- Dst VA high\n", ib[2]);
    printf("  IB[3] = 0x%08X <- Count-1\n", ib[3]);
    printf("  IB[4] = 0x%08X <- Reserved\n", ib[4]);
    printf("  IB[5] = 0x%08X <- Data\n", ib[5]);
    
    uint64_t ib_va = bo_va + 64;  // IB at offset 64
    
    // 7. Create BO list (legacy mode)
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
    
    // 8. Submit to SDMA (not GFX!)
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    ib_chunk.va_start = ib_va;
    ib_chunk.ib_bytes = 24;  // 6 dwords for SDMA packet
    ib_chunk.ip_type = AMDGPU_HW_IP_DMA;  // SDMA, not GFX!
    ib_chunk.ip_instance = 0;
    ib_chunk.ring = 0;
    ib_chunk.flags = 0;
    
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(struct drm_amdgpu_cs_chunk_ib) / 4;
    chunks[0].chunk_data = (uintptr_t)&ib_chunk;
    
    uint64_t chunk_ptrs[1] = {(uintptr_t)&chunks[0]};
    
    union drm_amdgpu_cs cs_args = {0};
    cs_args.in.ctx_id = ctx_id;
    cs_args.in.bo_list_handle = bo_list_args.out.list_handle;
    cs_args.in.num_chunks = 1;
    cs_args.in.chunks = (uintptr_t)chunk_ptrs;
    
    printf("\n=== SDMA SUBMISSION ===\n");
    printf("ip_type = %u (AMDGPU_HW_IP_DMA)\n", ib_chunk.ip_type);
    printf("ib_bytes = %u (6 dwords)\n", ib_chunk.ib_bytes);
    
    // Flush before submit
    msync(bo_cpu, 4096, MS_SYNC);
    
    printf("\nSubmitting to SDMA...\n");
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    if (ret < 0) {
        printf("❌ SDMA submit failed: %s\n", strerror(errno));
        close(fd);
        return 1;
    }
    
    printf("✓ SDMA submitted! seq=%llu\n", (unsigned long long)cs_args.out.handle);
    
    // 9. Wait
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = cs_args.out.handle;
    wait_args.in.ip_type = AMDGPU_HW_IP_DMA;
    wait_args.in.ctx_id = ctx_id;
    wait_args.in.timeout = 1000000000;
    
    ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
    
    // 10. Check result
    printf("\n=== RESULT ===\n");
    printf("BO[0] = 0x%08X (expected 0xCAFEBABE)\n", bo_cpu[0]);
    
    if (bo_cpu[0] == 0xCAFEBABE) {
        printf("\n✅ SDMA SUCCESS! VM and residency work!\n");
        printf("The problem is GFX-specific, not general VM/residency\n");
    } else {
        printf("\n❌ SDMA also failed\n");
        printf("Problem is in residency/VA/context wiring\n");
    }
    
    // Check SDMA ring
    printf("\nChecking SDMA ring:\n");
    system("sudo cat /sys/kernel/debug/dri/1/amdgpu_ring_sdma0 2>/dev/null | head -20 || echo 'Unable to read SDMA ring'");
    
    close(fd);
    return 0;
}