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
    printf("=== VM Update Test ===\n");
    printf("Testing if we need explicit VM updates\n\n");
    
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
    gem_args.in.bo_size = 4096;
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    gem_args.in.domain_flags = 0;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args) < 0) {
        perror("Failed to create BO");
        close(fd);
        return 1;
    }
    uint32_t bo_handle = gem_args.out.handle;
    printf("✓ BO: handle=%u\n", bo_handle);
    
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
    
    // Initialize
    bo_cpu[0] = 0xDEADBEEF;
    printf("✓ BO[0] = 0x%08X\n", bo_cpu[0]);
    
    // 5. Map to GPU VA
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = bo_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE | AMDGPU_VM_PAGE_EXECUTABLE;
    va_args.va_address = 0x100000000;  // Try 4GB
    va_args.offset_in_bo = 0;
    va_args.map_size = 4096;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args) < 0) {
        perror("Failed to map VA");
        close(fd);
        return 1;
    }
    uint64_t bo_va = va_args.va_address;
    printf("✓ VA mapped: 0x%016llx\n", (unsigned long long)bo_va);
    
    // 6. Try VM update ioctl (might be needed?)
    // First, query VM status
    struct drm_amdgpu_info info_req = {0};
    struct drm_amdgpu_info_vm vm_info = {0};
    
    info_req.query = AMDGPU_INFO_VM_STAT;
    info_req.return_pointer = (uintptr_t)&vm_info;
    info_req.return_size = sizeof(vm_info);
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
        printf("\nVM Stats:\n");
        printf("  Evicted BOs: %u\n", vm_info.evicted_vram);
        printf("  Evicted visible: %u\n", vm_info.evicted_visible_vram);
        printf("  Relocated: %u\n", vm_info.relocated);
    }
    
    // 7. Build simple IB
    uint32_t* ib = &bo_cpu[32];  // IB at offset 128
    int idx = 0;
    
    // WRITE_DATA to start of BO
    ib[idx++] = pkt3_header(IT_WRITE_DATA, 4);
    ib[idx++] = write_data_control();
    ib[idx++] = (uint32_t)(bo_va & 0xFFFFFFFF);
    ib[idx++] = (uint32_t)((bo_va >> 32) & 0xFFFF);
    ib[idx++] = 0xCAFEBABE;
    
    // Pad to 32 dwords
    ib[idx++] = pkt3_header(IT_NOP, 25);
    while (idx < 32) ib[idx++] = 0;
    
    uint64_t ib_va = bo_va + 128;
    
    printf("\n✓ IB built at offset 128\n");
    printf("  Target: BO start (self-write)\n");
    
    // 8. Create BO list with priority hint
    struct drm_amdgpu_bo_list_entry list_entry = {0};
    list_entry.bo_handle = bo_handle;
    list_entry.bo_priority = 1;  // Higher priority
    
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    bo_list_args.in.bo_number = 1;
    bo_list_args.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
    bo_list_args.in.bo_info_ptr = (uintptr_t)&list_entry;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args) < 0) {
        // Fall back to simple handle array
        uint32_t handles[1] = {bo_handle};
        bo_list_args.in.bo_info_size = sizeof(uint32_t);
        bo_list_args.in.bo_info_ptr = (uintptr_t)handles;
        
        if (ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args) < 0) {
            perror("Failed to create BO list");
            close(fd);
            return 1;
        }
    }
    printf("✓ BO list: handle=%u\n", bo_list_args.out.list_handle);
    
    // 9. Submit CS
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    ib_chunk._pad = 0;
    ib_chunk.flags = 0;
    ib_chunk.va_start = ib_va;
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
    
    // Explicit sync before submit
    msync(bo_cpu, 4096, MS_SYNC);
    __sync_synchronize();  // Memory barrier
    
    printf("\nSubmitting CS...\n");
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    if (ret < 0) {
        printf("❌ CS submit failed: %s\n", strerror(errno));
        close(fd);
        return 1;
    }
    
    printf("✓ CS submitted! seq=%llu\n", (unsigned long long)cs_args.out.handle);
    
    // 10. Wait
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = cs_args.out.handle;
    wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
    wait_args.in.ctx_id = ctx_id;
    wait_args.in.timeout = 1000000000;
    
    ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
    
    // Force cache invalidation
    __builtin_ia32_clflush(bo_cpu);
    
    // 11. Check result
    printf("\n=== RESULT ===\n");
    printf("BO[0] = 0x%08X (expected 0xCAFEBABE)\n", bo_cpu[0]);
    
    if (bo_cpu[0] == 0xCAFEBABE) {
        printf("\n✅ SUCCESS! VM update worked!\n");
    } else {
        printf("\n❌ FAILED: Still not executing\n");
        
        // Check VM stats again
        if (ioctl(fd, DRM_IOCTL_AMDGPU_INFO, &info_req) == 0) {
            printf("\nPost-submit VM Stats:\n");
            printf("  Evicted BOs: %u\n", vm_info.evicted_vram);
            printf("  Evicted visible: %u\n", vm_info.evicted_visible_vram);
            printf("  Relocated: %u\n", vm_info.relocated);
        }
    }
    
    close(fd);
    return 0;
}