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

// PM4 constants from Mini's spec
#define PKT3                3u
#define IT_WRITE_DATA       0x37
#define DST_SEL_MEM         0     // MEM destination
#define ENGINE_ME           1     // ME (gfx) engine

// PKT3 header builder
static inline uint32_t pkt3_header(uint32_t itop, uint32_t count_dw) {
    return (PKT3 << 30) | (itop << 8) | (count_dw);
}

// WRITE_DATA control word builder (checking AMD headers for exact bits)
static inline uint32_t write_data_ctl(int wr_one_addr, int wr_confirm,
                                      uint32_t dst_sel, uint32_t engine_sel) {
    uint32_t w = 0;
    // Based on common PM4 encoding:
    // DST_SEL is bits [11:8]
    // WR_ONE_ADDR is bit 16
    // WR_CONFIRM is bit 20  
    // ENGINE_SEL is bits [31:30]
    w |= (dst_sel & 0xF) << 8;        // DST_SEL at bits [11:8]
    w |= (wr_one_addr ? 1u : 0u) << 16;
    w |= (wr_confirm ? 1u : 0u) << 20;
    w |= (engine_sel & 0x3) << 30;    // ENGINE at bits [31:30]
    return w;
}

void emit_write_data_mem(uint32_t *w, uint64_t va, uint32_t imm) {
    // 5 dwords total
    w[0] = pkt3_header(IT_WRITE_DATA, 3);  // count=3 for 4 following dwords
    w[1] = write_data_ctl(/*one addr*/1, /*confirm*/0, DST_SEL_MEM, ENGINE_ME);
    w[2] = (uint32_t)(va & 0xFFFFFFFFu);           // addr_lo
    w[3] = (uint32_t)((va >> 32) & 0xFFFFu);       // addr_hi (up to 48-bit VA)
    w[4] = imm;
    
    printf("Generated WRITE_DATA packet:\n");
    for (int i = 0; i < 5; i++) {
        printf("  DW[%d] = 0x%08X", i, w[i]);
        if (i == 0) printf(" <- Header (type=3, op=0x37, count=3)");
        else if (i == 1) printf(" <- Control (dst=MEM, wr_one_addr=1, engine=ME)");
        else if (i == 2) printf(" <- Address low");
        else if (i == 3) printf(" <- Address high");
        else if (i == 4) printf(" <- Immediate data");
        printf("\n");
    }
}

// BO list operations
#define AMDGPU_BO_LIST_OP_CREATE 0
#define AMDGPU_BO_LIST_OP_DESTROY 1

int main() {
    printf("=== Mini's Exact 5-dword WRITE_DATA Test ===\n\n");
    
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
    
    // 3. Create signal BO in GTT domain
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = 4096;
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;  // GTT as required
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args) < 0) {
        perror("Failed to create signal BO");
        close(fd);
        return 1;
    }
    uint32_t signal_handle = gem_args.out.handle;
    printf("✓ Signal BO created in GTT: handle=%u\n", signal_handle);
    
    // Map and initialize signal
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = signal_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    
    uint32_t* signal_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                               fd, mmap_args.out.addr_ptr);
    memset(signal_ptr, 0, 4096);
    signal_ptr[0] = 0x11111111;  // Initial value
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
    uint64_t signal_gpu_va = va_args.va_address;
    printf("✓ Signal mapped to GPU VA: 0x%016lX\n", signal_gpu_va);
    
    // 4. Create IB with exactly 5 dwords
    uint32_t gfx_ib[32] = {0};  // Padded to minimum size
    emit_write_data_mem(gfx_ib, signal_gpu_va, 0xCAFEBABE);
    
    // Pad with NOPs to minimum IB size
    for (int i = 5; i < 32; i++) {
        gfx_ib[i] = 0xC0000010;  // NOP
    }
    
    // Create IB BO
    gem_args.in.bo_size = 256;  // Minimum size
    gem_args.in.alignment = 256;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t ib_handle = gem_args.out.handle;
    printf("\n✓ IB BO created: handle=%u\n", ib_handle);
    
    // Upload IB
    mmap_args.in.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    void* ib_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                       fd, mmap_args.out.addr_ptr);
    memcpy(ib_ptr, gfx_ib, 256);
    munmap(ib_ptr, 4096);
    
    // Map IB to VA with READ
    va_args.handle = ib_handle;
    va_args.flags = AMDGPU_VM_PAGE_READABLE;
    va_args.va_address = 0x800001000;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    printf("✓ IB mapped to GPU VA: 0x%016lX\n", va_args.va_address);
    
    // 5. Submit with BO list = [ib_bo, signal_bo]
    printf("\nSubmitting to GFX ring...\n");
    
    // BO handles array (just handles as uint32_t)
    uint32_t bo_handles[2] = {ib_handle, signal_handle};
    
    // Create BO list
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    bo_list_args.in.bo_number = 2;
    bo_list_args.in.bo_info_size = sizeof(uint32_t);
    bo_list_args.in.bo_info_ptr = (uintptr_t)bo_handles;
    
    // Try first with handles array
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    if (ret < 0) {
        // Fall back to bo_list_entry structure
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
        close(fd);
        return 1;
    }
    printf("✓ BO list created: handle=%u\n", bo_list_args.out.list_handle);
    
    // Build CS chunks
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    // IB chunk setup
    ib_chunk._pad = 0;
    ib_chunk.flags = 0;
    ib_chunk.va_start = va_args.va_address;
    ib_chunk.ib_bytes = 256;  // Padded size
    ib_chunk.ip_type = AMDGPU_HW_IP_GFX;
    ib_chunk.ip_instance = 0;
    ib_chunk.ring = 0;
    
    // Chunk setup - critical to get right!
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(struct drm_amdgpu_cs_chunk_ib) / 4;
    chunks[0].chunk_data = (uintptr_t)&ib_chunk;
    
    printf("CS chunk setup:\n");
    printf("  chunk[0].chunk_id = %u (AMDGPU_CHUNK_ID_IB)\n", chunks[0].chunk_id);
    printf("  chunk[0].length_dw = %u (sizeof(ib_chunk)/4)\n", chunks[0].length_dw);
    printf("  chunk[0].chunk_data = %p (&ib_chunk)\n", (void*)chunks[0].chunk_data);
    
    // Array of pointers to chunks!
    uint64_t chunk_ptrs[1] = {(uintptr_t)&chunks[0]};
    
    // Submit
    union drm_amdgpu_cs cs_args = {0};
    cs_args.in.ctx_id = ctx_id;
    cs_args.in.bo_list_handle = bo_list_args.out.list_handle;
    cs_args.in.num_chunks = 1;
    cs_args.in.chunks = (uintptr_t)chunk_ptrs;
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    if (ret < 0) {
        printf("❌ CS submission failed: %s (errno=%d)\n", strerror(errno), errno);
        close(fd);
        return 1;
    }
    
    printf("✓ CS submitted! Sequence=%llu\n", (unsigned long long)cs_args.out.handle);
    
    // Wait for completion
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = cs_args.out.handle;
    wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
    wait_args.in.ip_instance = 0;
    wait_args.in.ring = 0;
    wait_args.in.ctx_id = ctx_id;
    wait_args.in.timeout = 1000000000;
    
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
    printf("Wait result: %d, status: %lld\n", ret, (long long)wait_args.out.status);
    
    // Check result
    printf("\nChecking signal value:\n");
    printf("  Signal[0] = 0x%08X (expected 0xCAFEBABE)\n", signal_ptr[0]);
    
    if (signal_ptr[0] == 0xCAFEBABE) {
        printf("\n✅ SUCCESS! Mini's 5-dword WRITE_DATA worked!\n");
        printf("The GFX ring is executing packets!\n");
        return 0;
    } else {
        printf("\n❌ Still no execution\n");
        printf("Next steps:\n");
        printf("1. Check CP_HQD_PQ_RPTR/WPTR\n");
        printf("2. Compare with working libdrm test\n");
        printf("3. Verify exact packet encoding\n");
        return 1;
    }
}