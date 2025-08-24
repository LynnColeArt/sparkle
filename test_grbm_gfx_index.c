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
#define PKT3                    3u
#define IT_SET_UCONFIG_REG      0x79
#define IT_NOP                  0x10

// GRBM_GFX_INDEX register
#define GRBM_GFX_INDEX          0x30800

// Build PKT3 header
static inline uint32_t pkt3_header(uint32_t opcode, uint32_t count) {
    return (PKT3 << 30) | (opcode << 8) | count;
}

// Check GRBM_GFX_INDEX value
void check_grbm_gfx_index(const char* when) {
    printf("\n%s:\n", when);
    printf("  GRBM_GFX_INDEX: ");
    fflush(stdout);
    system("sudo umr -r *.*.GRBM_GFX_INDEX 2>/dev/null | grep -o '0x[0-9a-fA-F]*' | head -1 || echo 'Unable to read'");
}

// Dump all CS fields for Mini
void dump_cs_details(union drm_amdgpu_ctx* ctx, 
                    union drm_amdgpu_bo_list* bo_list,
                    struct drm_amdgpu_cs_chunk* chunks,
                    struct drm_amdgpu_cs_chunk_ib* ib_chunk,
                    union drm_amdgpu_cs* cs) {
    printf("\n=== COMPLETE CS DUMP FOR MINI ===\n");
    
    printf("Context:\n");
    printf("  ctx_id from alloc = %u\n", ctx->out.alloc.ctx_id);
    
    printf("\nBO List:\n");
    printf("  bo_list_handle = %u\n", bo_list->out.list_handle);
    printf("  num_bos = 1 (just IB)\n");
    
    printf("\nCS Input:\n");
    printf("  cs.in.ctx_id = %u\n", cs->in.ctx_id);
    printf("  cs.in.bo_list_handle = %u\n", cs->in.bo_list_handle);
    printf("  cs.in.num_chunks = %u\n", cs->in.num_chunks);
    printf("  cs.in.chunks = %p\n", (void*)cs->in.chunks);
    
    printf("\nChunk[0] (IB chunk):\n");
    printf("  chunk_id = %u (AMDGPU_CHUNK_ID_IB)\n", chunks[0].chunk_id);
    printf("  length_dw = %u (sizeof(ib_chunk)/4)\n", chunks[0].length_dw);
    printf("  chunk_data = %p -> ib_chunk\n", (void*)chunks[0].chunk_data);
    
    printf("\nIB Chunk Contents:\n");
    printf("  ib.ip_type = %u (AMDGPU_HW_IP_GFX = 0)\n", ib_chunk->ip_type);
    printf("  ib.ip_instance = %u\n", ib_chunk->ip_instance);
    printf("  ib.ring = %u\n", ib_chunk->ring);
    printf("  ib.va_start = 0x%016llx (32-dw aligned)\n", 
           (unsigned long long)ib_chunk->va_start);
    printf("  ib.ib_bytes = %u (32 dwords)\n", ib_chunk->ib_bytes);
    printf("  ib.flags = 0x%08x\n", ib_chunk->flags);
    printf("  sizeof(ib_chunk) = %zu\n", sizeof(*ib_chunk));
    printf("  sizeof(ib_chunk)/4 = %zu\n", sizeof(*ib_chunk)/4);
}

int main() {
    printf("=== GRBM_GFX_INDEX Test (Mini's Litmus Test 1) ===\n");
    printf("This proves if the ring executes our IB\n\n");
    
    // Check initial value
    check_grbm_gfx_index("INITIAL VALUE");
    
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
    
    // 3. Create IB BO (4KB, GTT)
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = 4096;
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t ib_handle = gem_args.out.handle;
    
    // Build IB
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    uint32_t* ib = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                       fd, mmap_args.out.addr_ptr);
    
    // Build SET_UCONFIG_REG GRBM_GFX_INDEX = 0xE0000000 (broadcast)
    int idx = 0;
    ib[idx++] = pkt3_header(IT_SET_UCONFIG_REG, 1);
    ib[idx++] = (GRBM_GFX_INDEX - 0x30000) >> 2;  // UCONFIG offset
    ib[idx++] = 0xE0000000;  // Broadcast value
    
    // Pad to 32 dwords
    ib[idx++] = pkt3_header(IT_NOP, 27);
    while (idx < 32) ib[idx++] = 0;
    
    printf("\n✓ IB content:\n");
    printf("  DW[0] = 0x%08X <- SET_UCONFIG_REG\n", ib[0]);
    printf("  DW[1] = 0x%08X <- GRBM_GFX_INDEX offset\n", ib[1]);
    printf("  DW[2] = 0x%08X <- Broadcast value\n", ib[2]);
    
    munmap(ib, 4096);
    
    // Map IB VA (4KB)
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = ib_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE;
    va_args.va_address = 0x800004000;
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    printf("✓ IB VA: 0x%016llx\n", (unsigned long long)va_args.va_address);
    
    // 4. Create BO list (just IB)
    uint32_t handles[1] = {ib_handle};
    
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = 0;
    bo_list_args.in.bo_number = 1;
    bo_list_args.in.bo_info_size = sizeof(uint32_t);
    bo_list_args.in.bo_info_ptr = (uintptr_t)handles;
    ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    
    // 5. Submit CS
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    ib_chunk.va_start = va_args.va_address;
    ib_chunk.ib_bytes = 128;  // 32 dwords
    ib_chunk.ip_type = AMDGPU_HW_IP_GFX;
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
    
    // Dump everything for Mini
    dump_cs_details(&ctx_args, &bo_list_args, chunks, &ib_chunk, &cs_args);
    
    printf("\n=== SUBMITTING ===\n");
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    if (ret < 0) {
        printf("❌ CS submit failed: %s\n", strerror(errno));
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
    
    // Check result
    check_grbm_gfx_index("AFTER SUBMIT");
    
    printf("\nIf GRBM_GFX_INDEX changed to 0xE0000000:\n");
    printf("  ✅ Ring IS executing our IB!\n");
    printf("If GRBM_GFX_INDEX is unchanged:\n");
    printf("  ❌ IB never executed - CS wiring issue\n");
    
    close(fd);
    return 0;
}