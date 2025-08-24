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
#define IT_SET_UCONFIG_REG  0x79

// CP_SCRATCH0 register
#define CP_SCRATCH0         0x34D0

// Minimal control word for WRITE_DATA
#define WRITE_DATA_CONTROL  0x00010000  // Only WR_ONE_ADDR=1, rest zero

// Build PKT3 header
static inline uint32_t pkt3_header(uint32_t opcode, uint32_t count) {
    return (PKT3 << 30) | (opcode << 8) | count;
}

// Check HQD pointers
void check_hqd_pointers(const char* when) {
    printf("\n%s:\n", when);
    printf("  CP_HQD_PQ_WPTR: ");
    fflush(stdout);
    system("sudo umr -r *.*.CP_HQD_PQ_WPTR 2>/dev/null | grep -o '0x[0-9a-fA-F]*' || echo 'N/A'");
    printf("  CP_HQD_PQ_RPTR: ");
    fflush(stdout);
    system("sudo umr -r *.*.CP_HQD_PQ_RPTR 2>/dev/null | grep -o '0x[0-9a-fA-F]*' || echo 'N/A'");
}

// Build CP_SCRATCH test IB
void build_scratch_ib(uint32_t *ib) {
    int idx = 0;
    
    // SET_UCONFIG_REG CP_SCRATCH0 = 0xA5A5A5A5
    ib[idx++] = pkt3_header(IT_SET_UCONFIG_REG, 1);
    ib[idx++] = (CP_SCRATCH0 - 0x30000) >> 2;  // Register offset
    ib[idx++] = 0xA5A5A5A5;                    // Value
    
    // Pad to 32 dwords with NOP
    ib[idx++] = pkt3_header(IT_NOP, 27);  // 28 remaining slots
    while (idx < 32) {
        ib[idx++] = 0x00000000;
    }
    
    printf("\nCP_SCRATCH test IB:\n");
    printf("  DW[0] = 0x%08X <- SET_UCONFIG_REG header\n", ib[0]);
    printf("  DW[1] = 0x%08X <- CP_SCRATCH0 offset\n", ib[1]);
    printf("  DW[2] = 0x%08X <- Test value\n", ib[2]);
    printf("  DW[3] = 0x%08X <- NOP padding\n", ib[3]);
}

// Build memory write IB
void build_write_data_ib(uint32_t *ib, uint64_t signal_va) {
    int idx = 0;
    
    // WRITE_DATA packet - 5 dwords total
    ib[idx++] = pkt3_header(IT_WRITE_DATA, 4);  // COUNT=4!
    ib[idx++] = WRITE_DATA_CONTROL;             // Minimal control
    ib[idx++] = (uint32_t)(signal_va & 0xFFFFFFFF);
    ib[idx++] = (uint32_t)((signal_va >> 32) & 0xFFFF);
    ib[idx++] = 0xCAFEBABE;
    
    // Pad to 32 dwords
    ib[idx++] = pkt3_header(IT_NOP, 25);
    while (idx < 32) {
        ib[idx++] = 0x00000000;
    }
    
    printf("\nWRITE_DATA IB:\n");
    printf("  DW[0] = 0x%08X <- Header (COUNT=4)\n", ib[0]);
    printf("  DW[1] = 0x%08X <- Control (minimal)\n", ib[1]);
    printf("  DW[2] = 0x%08X <- Addr low\n", ib[2]);
    printf("  DW[3] = 0x%08X <- Addr high\n", ib[3]);
    printf("  DW[4] = 0x%08X <- Data\n", ib[4]);
}

// BO list operations
#define AMDGPU_BO_LIST_OP_CREATE 0
#define AMDGPU_BO_LIST_OP_DESTROY 1

int main(int argc, char **argv) {
    int use_scratch = (argc > 1 && strcmp(argv[1], "scratch") == 0);
    
    printf("=== HQD Pointer Test ===\n");
    printf("Mode: %s\n", use_scratch ? "CP_SCRATCH (no memory)" : "WRITE_DATA (memory)");
    
    // Check initial HQD state
    check_hqd_pointers("BEFORE submit");
    
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
    
    // 3. Create IB BO
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = 128;  // 32 dwords
    gem_args.in.alignment = 128;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t ib_handle = gem_args.out.handle;
    
    // Map IB
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    uint32_t* ib_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                           fd, mmap_args.out.addr_ptr);
    
    // 4. Create signal BO (if using memory test)
    uint32_t signal_handle = 0;
    uint32_t* signal_ptr = NULL;
    uint64_t signal_va = 0;
    
    if (!use_scratch) {
        gem_args.in.bo_size = 4096;
        gem_args.in.alignment = 4096;
        ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
        signal_handle = gem_args.out.handle;
        
        mmap_args.in.handle = signal_handle;
        ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
        signal_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                         fd, mmap_args.out.addr_ptr);
        signal_ptr[0] = 0x12345678;
        
        // Map signal VA
        struct drm_amdgpu_gem_va va_args = {0};
        va_args.handle = signal_handle;
        va_args.operation = AMDGPU_VA_OP_MAP;
        va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE;
        va_args.va_address = 0x800000000;
        va_args.map_size = 4096;
        ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
        signal_va = va_args.va_address;
    }
    
    // 5. Build IB
    if (use_scratch) {
        build_scratch_ib(ib_ptr);
    } else {
        build_write_data_ib(ib_ptr, signal_va);
    }
    munmap(ib_ptr, 4096);
    
    // Map IB VA
    struct drm_amdgpu_gem_va va_args = {0};
    va_args.handle = ib_handle;
    va_args.operation = AMDGPU_VA_OP_MAP;
    va_args.flags = AMDGPU_VM_PAGE_READABLE;
    va_args.va_address = 0x800001000;
    va_args.map_size = 128;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    
    // 6. Create BO list
    uint32_t num_handles = use_scratch ? 1 : 2;
    uint32_t handles[2] = {ib_handle, signal_handle};
    
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    bo_list_args.in.bo_number = num_handles;
    bo_list_args.in.bo_info_size = sizeof(uint32_t);
    bo_list_args.in.bo_info_ptr = (uintptr_t)handles;
    
    int ret = ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    if (ret < 0) {
        struct drm_amdgpu_bo_list_entry bo_list[2] = {
            {ib_handle, 0},
            {signal_handle, 0}
        };
        bo_list_args.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
        bo_list_args.in.bo_info_ptr = (uintptr_t)bo_list;
        ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    }
    
    // 7. Build CS with TWO chunks!
    struct drm_amdgpu_cs_chunk chunks[2] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    // IB chunk
    ib_chunk._pad = 0;
    ib_chunk.flags = 0;
    ib_chunk.va_start = va_args.va_address;
    ib_chunk.ib_bytes = 128;
    ib_chunk.ip_type = AMDGPU_HW_IP_GFX;
    ib_chunk.ip_instance = 0;
    ib_chunk.ring = 0;
    
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(struct drm_amdgpu_cs_chunk_ib) / 4;
    chunks[0].chunk_data = (uintptr_t)&ib_chunk;
    
    // BO_HANDLES chunk - CRITICAL!
    chunks[1].chunk_id = AMDGPU_CHUNK_ID_BO_HANDLES;
    chunks[1].length_dw = num_handles;
    chunks[1].chunk_data = (uintptr_t)handles;
    
    printf("\nCS submission details:\n");
    printf("  num_chunks = 2\n");
    printf("  Chunk[0]: IB (id=%u, length_dw=%u)\n", 
           chunks[0].chunk_id, chunks[0].length_dw);
    printf("  Chunk[1]: BO_HANDLES (id=%u, length_dw=%u)\n",
           chunks[1].chunk_id, chunks[1].length_dw);
    
    uint64_t chunk_ptrs[2] = {
        (uintptr_t)&chunks[0],
        (uintptr_t)&chunks[1]
    };
    
    union drm_amdgpu_cs cs_args = {0};
    cs_args.in.ctx_id = ctx_id;
    cs_args.in.bo_list_handle = bo_list_args.out.list_handle;
    cs_args.in.num_chunks = 2;  // TWO chunks!
    cs_args.in.chunks = (uintptr_t)chunk_ptrs;
    
    // 8. Submit
    printf("\nSubmitting...\n");
    ret = ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args);
    if (ret < 0) {
        printf("❌ CS submit failed: %s\n", strerror(errno));
        return 1;
    }
    
    printf("✓ CS submitted! seq=%llu\n", (unsigned long long)cs_args.out.handle);
    
    // Check HQD after submit
    check_hqd_pointers("AFTER submit");
    
    // Wait
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = cs_args.out.handle;
    wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
    wait_args.in.ctx_id = ctx_id;
    wait_args.in.timeout = 1000000000;
    ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
    
    // Check HQD after wait
    check_hqd_pointers("AFTER wait");
    
    // Check result
    printf("\nResult:\n");
    if (use_scratch) {
        printf("  CP_SCRATCH0: ");
        fflush(stdout);
        system("sudo umr -r *.*.CP_SCRATCH0 2>/dev/null | grep -o '0x[0-9a-fA-F]*' || echo 'N/A'");
        printf("  Expected: 0xA5A5A5A5\n");
    } else {
        printf("  Signal[0] = 0x%08X (expected 0xCAFEBABE)\n", 
               signal_ptr ? signal_ptr[0] : 0);
    }
    
    close(fd);
    return 0;
}