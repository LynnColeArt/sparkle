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

// WRITE_DATA DST_SEL values
#define DST_SEL_REG         0
#define DST_SEL_MEM         5  // This is what we were missing!

// WRITE_DATA ENGINE values  
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

void check_hqd_pointers(const char* when) {
    printf("\n=== %s ===\n", when);
    system("sudo umr -r *.mmCP_HQD_PQ_WPTR_LO 2>/dev/null | grep -o '0x[0-9a-fA-F]*$' | head -1");
    system("sudo umr -r *.mmCP_HQD_PQ_RPTR 2>/dev/null | grep -o '0x[0-9a-fA-F]*$' | head -1");
}

int main() {
    printf("=== HQD Movement Test with Fixed WRITE_DATA ===\n");
    printf("Key fix: DST_SEL=MEM (5) instead of 0\n\n");
    
    // Check initial state
    check_hqd_pointers("INITIAL STATE");
    
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
    
    // Build IB with FIXED control word
    mmap_args.in.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    uint32_t* ib = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                       fd, mmap_args.out.addr_ptr);
    
    int idx = 0;
    // WRITE_DATA with DST_SEL=MEM
    ib[idx++] = pkt3_header(IT_WRITE_DATA, 4);
    ib[idx++] = write_data_control();  // Now has DST_SEL=MEM!
    ib[idx++] = (uint32_t)(signal_va & 0xFFFFFFFF);
    ib[idx++] = (uint32_t)((signal_va >> 32) & 0xFFFF);
    ib[idx++] = 0xCAFEBABE;
    
    // Debug print
    printf("WRITE_DATA packet:\n");
    printf("  Header  = 0x%08X\n", ib[0]);
    printf("  Control = 0x%08X (DST_SEL=MEM, WR_ONE_ADDR=1, ENGINE=ME)\n", ib[1]);
    printf("  VA_LO   = 0x%08X\n", ib[2]);
    printf("  VA_HI   = 0x%08X\n", ib[3]);
    printf("  Data    = 0x%08X\n", ib[4]);
    
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
    
    // 5. Create BO list (legacy pattern)
    uint32_t handles[2] = {ib_handle, signal_handle};
    
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = 0; // AMDGPU_BO_LIST_OP_CREATE
    bo_list_args.in.bo_number = 2;
    bo_list_args.in.bo_info_size = sizeof(uint32_t);
    bo_list_args.in.bo_info_ptr = (uintptr_t)handles;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args) < 0) {
        // Try with bo_list_entry
        struct drm_amdgpu_bo_list_entry bo_list[2] = {
            {ib_handle, 0},
            {signal_handle, 0}
        };
        bo_list_args.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
        bo_list_args.in.bo_info_ptr = (uintptr_t)bo_list;
        ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    }
    
    // 6. Submit CS
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    ib_chunk.va_start = va_args.va_address;
    ib_chunk.ib_bytes = 128;
    ib_chunk.ip_type = AMDGPU_HW_IP_GFX;
    
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(struct drm_amdgpu_cs_chunk_ib) / 4;
    chunks[0].chunk_data = (uintptr_t)&ib_chunk;
    
    uint64_t chunk_ptrs[1] = {(uintptr_t)&chunks[0]};
    
    union drm_amdgpu_cs cs_args = {0};
    cs_args.in.ctx_id = ctx_id;
    cs_args.in.bo_list_handle = bo_list_args.out.list_handle;
    cs_args.in.num_chunks = 1;
    cs_args.in.chunks = (uintptr_t)chunk_ptrs;
    
    printf("\nSignal before: 0x%08X\n", signal_ptr[0]);
    
    check_hqd_pointers("BEFORE SUBMIT");
    
    printf("\nSubmitting...\n");
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    if (ret < 0) {
        printf("❌ Failed: %s\n", strerror(errno));
        return 1;
    }
    
    printf("✓ Submitted! seq=%llu\n", (unsigned long long)cs_args.out.handle);
    
    check_hqd_pointers("AFTER SUBMIT");
    
    // Wait
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = cs_args.out.handle;
    wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
    wait_args.in.ctx_id = ctx_id;
    wait_args.in.timeout = 1000000000;
    ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
    
    check_hqd_pointers("AFTER WAIT");
    
    printf("\nSignal after: 0x%08X (expected 0xCAFEBABE)\n", signal_ptr[0]);
    
    if (signal_ptr[0] == 0xCAFEBABE) {
        printf("\n✅ SUCCESS! DST_SEL=MEM fixed it!\n");
        printf("The ring IS executing - we just had the wrong control word!\n");
    } else {
        printf("\n❌ Still no execution\n");
        printf("If HQD pointers moved: Ring read IB but packet was ignored\n");
        printf("If HQD pointers didn't move: Ring never saw our IB\n");
    }
    
    close(fd);
    return 0;
}