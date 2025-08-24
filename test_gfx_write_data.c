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
#define PM4_WRITE_DATA      0x37

// WRITE_DATA defines
#define WRITE_DATA_DST_SEL_MEM   (0 << 8)
#define WRITE_DATA_WR_CONFIRM    (1 << 20)
#define WRITE_DATA_ENGINE_ME     (1 << 30)

// BO list operations
#define AMDGPU_BO_LIST_OP_CREATE 0
#define AMDGPU_BO_LIST_OP_DESTROY 1

int main() {
    printf("=== Simple GFX WRITE_DATA Test ===\n");
    printf("Using the simplest possible packet - WRITE_DATA\n\n");
    
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
    
    // 3. Create signal BO
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
    signal_ptr[0] = 0x12345678;  // Set initial value
    signal_ptr[1] = 0xABCDEF00;
    printf("✓ Signal memory initialized: [0]=0x%08X [1]=0x%08X\n", 
           signal_ptr[0], signal_ptr[1]);
    
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
    
    // 4. Create minimal WRITE_DATA IB
    uint32_t gfx_ib[] = {
        // WRITE_DATA packet - write 32-bit immediate to memory
        PM4_TYPE_3_HEADER(PM4_WRITE_DATA, 3),
        WRITE_DATA_DST_SEL_MEM | WRITE_DATA_WR_CONFIRM | WRITE_DATA_ENGINE_ME,
        (uint32_t)signal_va,          // dst addr lo
        (uint32_t)(signal_va >> 32),  // dst addr hi
        0xDEADBEEF,                   // data to write
    };
    
    printf("\nGFX IB (%zu dwords):\n", sizeof(gfx_ib)/4);
    for (size_t i = 0; i < sizeof(gfx_ib)/4; i++) {
        printf("  [%zu] 0x%08X", i, gfx_ib[i]);
        if (i == 0) printf(" <- WRITE_DATA header");
        else if (i == 1) printf(" <- Control (DST=MEM, WR_CONFIRM, ENGINE=ME)");
        else if (i == 2) printf(" <- Addr low");
        else if (i == 3) printf(" <- Addr high");
        else if (i == 4) printf(" <- Data");
        printf("\n");
    }
    
    // Create IB BO
    gem_args.in.bo_size = 256;  // Minimum IB size
    gem_args.in.alignment = 256;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t ib_handle = gem_args.out.handle;
    
    // Upload IB with padding
    mmap_args.in.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    uint32_t* ib_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                            fd, mmap_args.out.addr_ptr);
    
    // Clear IB buffer first
    memset(ib_ptr, 0, 256);
    
    // Copy our commands
    memcpy(ib_ptr, gfx_ib, sizeof(gfx_ib));
    
    // Add NOPs to pad to minimum size (32 dwords = 128 bytes)
    for (int i = sizeof(gfx_ib)/4; i < 32; i++) {
        ib_ptr[i] = 0xC0000010;  // NOP packet
    }
    
    munmap(ib_ptr, 4096);
    
    // Map IB to VA
    va_args.handle = ib_handle;
    va_args.flags = AMDGPU_VM_PAGE_READABLE;
    va_args.va_address = 0x800001000;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    
    // 5. Submit
    printf("\nSubmitting IB (size=128 bytes)...\n");
    
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
    ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    
    // Submit
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    ib_chunk._pad = 0;
    ib_chunk.flags = 0;
    ib_chunk.va_start = va_args.va_address;
    ib_chunk.ib_bytes = 128;  // Padded size
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
    printf("CS submit: ret=%d", ret);
    if (ret < 0) {
        printf(" (errno=%d: %s)\n", errno, strerror(errno));
    } else {
        printf(" SUCCESS! seq=%llu\n", (unsigned long long)cs_args.out.handle);
    }
    
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
    printf("  Signal[0] = 0x%08X (expected 0xDEADBEEF)\n", signal_ptr[0]);
    printf("  Signal[1] = 0x%08X (should be unchanged)\n", signal_ptr[1]);
    
    if (signal_ptr[0] == 0xDEADBEEF) {
        printf("\n✅ SUCCESS! WRITE_DATA worked!\n");
        printf("GFX ring is functional - packets are executing.\n");
    } else {
        printf("\n❌ WRITE_DATA failed - no execution\n");
        printf("Even the simplest packet isn't working.\n");
        printf("\nPossible issues:\n");
        printf("1. GFX ring not initialized by kernel\n");
        printf("2. Wrong packet encoding\n");
        printf("3. VM/memory mapping issue\n");
        printf("4. Need firmware/microcode\n");
    }
    
    // Cleanup
    munmap(signal_ptr, 4096);
    close(fd);
    
    return 0;
}