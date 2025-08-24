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
#define PM4_SET_UCONFIG_REG 0x79
#define PM4_SET_SH_REG      0x25
#define PM4_SET_SH_REG_INDEX 0x9B
#define PM4_DISPATCH_DIRECT 0x15
#define PM4_CLEAR_STATE     0x12
#define PM4_RELEASE_MEM     0x49

// UCONFIG registers
#define GRBM_GFX_INDEX                   0x30800
#define COMPUTE_STATIC_THREAD_MGMT_SE0   0x30844
#define COMPUTE_STATIC_THREAD_MGMT_SE1   0x30848
#define COMPUTE_STATIC_THREAD_MGMT_SE2   0x3084c
#define COMPUTE_STATIC_THREAD_MGMT_SE3   0x30850

// SH registers
#define COMPUTE_PGM_LO    0x2C00
#define COMPUTE_PGM_HI    0x2C01
#define COMPUTE_PGM_RSRC1 0x2C02
#define COMPUTE_PGM_RSRC2 0x2C03
#define COMPUTE_NUM_THREAD_X 0x2C09
#define COMPUTE_NUM_THREAD_Y 0x2C0A
#define COMPUTE_NUM_THREAD_Z 0x2C0B
#define COMPUTE_USER_DATA_0  0x2C40

// Broadcast mode
#define SE_BROADCAST_WRITES  (1 << 31)
#define SH_BROADCAST_WRITES  (1 << 29)
#define INSTANCE_BROADCAST   (1 << 30)
#define BROADCAST_ALL        (SE_BROADCAST_WRITES | SH_BROADCAST_WRITES | INSTANCE_BROADCAST)

// SET_SH_REG_INDEX broadcast flags
#define SH_REG_INDEX_SE_BROADCAST (1 << 30)
#define SH_REG_INDEX_SH_BROADCAST (1 << 29)

// EOP defines
#define EOP_TC_ACTION_ENA   (1 << 17)
#define EOP_TC_WB_ACTION_ENA (1 << 18)
#define EOP_CACHE_POLICY(x) ((x) << 25)
#define EOP_DATA_SEL(x)     ((x) << 29)
#define DATA_SEL_VALUE_32BIT 1

// Mini's lane-0 probe shader (swapped)
static const uint32_t probe_shader[] = {
    0x8000004C,  // v_mbcnt_lo_u32_b32 v0, -1, 0
    0x8000024C,  // v_mbcnt_hi_u32_b32 v0, -1, v0  
    0x8000847D,  // v_cmp_eq_u32 vcc, v0, 0
    0x040086BF,  // s_cbranch_vccz skip
    0xFF02007E,  // v_mov_b32 v0, 0xDEADBEEF
    0xEFBEADDE,  // (immediate)
    0x0002027E,  // v_mov_b32 v1, s0
    0x0102047E,  // v_mov_b32 v2, s1
    0x000070DC,  // global_store_dword v[1:2], v0
    0x00010000,  // (offset)
    0x70008CBF,  // s_waitcnt
    0x000081BF   // s_endpgm
};

// BO list operations
#define AMDGPU_BO_LIST_OP_CREATE 0
#define AMDGPU_BO_LIST_OP_DESTROY 1

void check_cu_registers() {
    printf("\n=== Checking CU Enable Registers ===\n");
    system("sudo ./quick_cu_check.sh 2>/dev/null");
    printf("\n");
}

int main() {
    printf("=== CU Enable and Verify Test ===\n");
    
    // Check initial state
    printf("\nBEFORE GFX preamble:");
    check_cu_registers();
    
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
    
    // 3. Create signal BO for EOP
    union drm_amdgpu_gem_create gem_args = {0};
    gem_args.in.bo_size = 4096;
    gem_args.in.alignment = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;  // GTT for coherency
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args) < 0) {
        perror("Failed to create signal BO");
        close(fd);
        return 1;
    }
    uint32_t signal_handle = gem_args.out.handle;
    
    // Map signal BO
    union drm_amdgpu_gem_mmap mmap_args = {0};
    mmap_args.in.handle = signal_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    uint32_t* signal_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                               fd, mmap_args.out.addr_ptr);
    signal_ptr[0] = 0;  // Clear signal
    
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
    
    // === STEP 1: GFX Preamble with verification ===
    printf("\nStep 1: Submitting GFX preamble to enable ALL CUs...\n");
    
    uint32_t gfx_ib[] = {
        // Set GRBM_GFX_INDEX to broadcast mode
        PM4_TYPE_3_HEADER(PM4_SET_UCONFIG_REG, 1),
        (GRBM_GFX_INDEX - 0x30000) >> 2,
        BROADCAST_ALL,
        
        // Enable ALL CUs on ALL SEs (fixing SE0!)
        PM4_TYPE_3_HEADER(PM4_SET_UCONFIG_REG, 4),
        (COMPUTE_STATIC_THREAD_MGMT_SE0 - 0x30000) >> 2,
        0xFFFFFFFF,  // SE0: ALL CUs enabled
        0xFFFFFFFF,  // SE1: ALL CUs enabled
        0xFFFFFFFF,  // SE2: ALL CUs enabled
        0xFFFFFFFF,  // SE3: ALL CUs enabled
        
        // RELEASE_MEM to signal completion
        PM4_TYPE_3_HEADER(PM4_RELEASE_MEM, 5),
        EOP_TC_ACTION_ENA | EOP_TC_WB_ACTION_ENA | 
        EOP_DATA_SEL(DATA_SEL_VALUE_32BIT) | EOP_CACHE_POLICY(3),
        0,  // addr_lo
        0,  // addr_hi  
        (uint32_t)signal_va,
        (uint32_t)(signal_va >> 32),
        0xDEADBEEF,  // Signal value
        0,  // unused
    };
    
    // Create and submit GFX IB
    gem_args.in.bo_size = sizeof(gfx_ib);
    gem_args.in.alignment = 256;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t gfx_ib_handle = gem_args.out.handle;
    
    mmap_args.in.handle = gfx_ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    void* gfx_ib_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                           fd, mmap_args.out.addr_ptr);
    memcpy(gfx_ib_ptr, gfx_ib, sizeof(gfx_ib));
    munmap(gfx_ib_ptr, 4096);
    
    va_args.handle = gfx_ib_handle;
    va_args.va_address = 0x800001000;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    
    // Submit GFX IB
    struct drm_amdgpu_bo_list_entry bo_list[] = {
        {signal_handle, 0},
        {gfx_ib_handle, 0}
    };
    union drm_amdgpu_bo_list bo_list_args = {0};
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    bo_list_args.in.bo_number = 2;
    bo_list_args.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
    bo_list_args.in.bo_info_ptr = (uintptr_t)bo_list;
    ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    
    struct drm_amdgpu_cs_chunk chunks[1] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    
    ib_chunk._pad = 0;
    ib_chunk.flags = 0;
    ib_chunk.va_start = va_args.va_address;
    ib_chunk.ib_bytes = sizeof(gfx_ib);
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
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args) == 0) {
        printf("✓ GFX IB submitted\n");
        
        // Wait for completion
        union drm_amdgpu_wait_cs wait_args = {0};
        wait_args.in.handle = cs_args.out.handle;
        wait_args.in.ip_type = AMDGPU_HW_IP_GFX;
        wait_args.in.ip_instance = 0;
        wait_args.in.ring = 0;
        wait_args.in.ctx_id = ctx_id;
        wait_args.in.timeout = 1000000000;
        ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
        
        // Check signal
        if (signal_ptr[0] == 0xDEADBEEF) {
            printf("✓ GFX EOP signal received!\n");
        } else {
            printf("❌ No EOP signal (got 0x%08X)\n", signal_ptr[0]);
        }
    } else {
        printf("❌ GFX submission failed: %s\n", strerror(errno));
    }
    
    // Destroy GFX BO list
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_DESTROY;
    bo_list_args.in.list_handle = bo_list_args.out.list_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    
    // Check if registers changed
    printf("\nAFTER GFX preamble:");
    check_cu_registers();
    printf("SE0 should now be 0xFFFFFFFF (was 0x2049fecf)\n");
    
    // === STEP 2: Test compute with probe shader ===
    printf("\nStep 2: Testing compute with lane-0 probe shader...\n");
    
    // Create shader BO
    gem_args.in.bo_size = sizeof(probe_shader);
    gem_args.in.alignment = 4096;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t shader_handle = gem_args.out.handle;
    
    mmap_args.in.handle = shader_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    void* shader_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                           fd, mmap_args.out.addr_ptr);
    memcpy(shader_ptr, probe_shader, sizeof(probe_shader));
    munmap(shader_ptr, 4096);
    
    va_args.handle = shader_handle;
    va_args.va_address = 0x800002000;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_EXECUTABLE;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    uint64_t shader_va = va_args.va_address;
    
    // Create output BO
    gem_args.in.bo_size = 4096;
    gem_args.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t output_handle = gem_args.out.handle;
    
    mmap_args.in.handle = output_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    uint32_t* output_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                               fd, mmap_args.out.addr_ptr);
    memset(output_ptr, 0xCA, 4096);  // Fill with pattern
    
    va_args.handle = output_handle;
    va_args.va_address = 0x800003000;
    va_args.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    uint64_t output_va = va_args.va_address;
    
    // Clear signal for compute
    signal_ptr[1] = 0;
    
    // Build compute IB with SET_SH_REG_INDEX (broadcast)
    uint32_t compute_ib[] = {
        // CLEAR_STATE
        PM4_TYPE_3_HEADER(PM4_CLEAR_STATE, 0),
        0x00000000,
        
        // Set user data (output address) with broadcast
        PM4_TYPE_3_HEADER(PM4_SET_SH_REG_INDEX, 2),
        (COMPUTE_USER_DATA_0 - 0x2800) | SH_REG_INDEX_SE_BROADCAST | SH_REG_INDEX_SH_BROADCAST,
        (uint32_t)output_va,
        (uint32_t)(output_va >> 32),
        
        // Set shader address with broadcast
        PM4_TYPE_3_HEADER(PM4_SET_SH_REG_INDEX, 2),
        (COMPUTE_PGM_LO - 0x2800) | SH_REG_INDEX_SE_BROADCAST | SH_REG_INDEX_SH_BROADCAST,
        (uint32_t)(shader_va >> 8),
        (uint32_t)(shader_va >> 40),
        
        // Set shader resources with broadcast
        PM4_TYPE_3_HEADER(PM4_SET_SH_REG_INDEX, 2),
        (COMPUTE_PGM_RSRC1 - 0x2800) | SH_REG_INDEX_SE_BROADCAST | SH_REG_INDEX_SH_BROADCAST,
        0x00000040,  // 4 VGPRs
        0x00000014,  // 2 SGPRs, WGP_MODE=1
        
        // Set thread dimensions with broadcast
        PM4_TYPE_3_HEADER(PM4_SET_SH_REG_INDEX, 3),
        (COMPUTE_NUM_THREAD_X - 0x2800) | SH_REG_INDEX_SE_BROADCAST | SH_REG_INDEX_SH_BROADCAST,
        64, 1, 1,  // 64 threads as Mini suggested
        
        // DISPATCH_DIRECT with all flags Mini mentioned
        PM4_TYPE_3_HEADER(PM4_DISPATCH_DIRECT, 4),
        1, 1, 1,  // 1x1x1 workgroup
        0x00000045,  // COMPUTE_SHADER_EN | FORCE_START_AT_000 | ORDER_MODE
        
        // RELEASE_MEM for compute completion
        PM4_TYPE_3_HEADER(PM4_RELEASE_MEM, 5),
        EOP_TC_ACTION_ENA | EOP_TC_WB_ACTION_ENA | 
        EOP_DATA_SEL(DATA_SEL_VALUE_32BIT) | EOP_CACHE_POLICY(3),
        0,  // addr_lo
        0,  // addr_hi  
        (uint32_t)(signal_va + 4),
        (uint32_t)(signal_va >> 32),
        0xCAFEBABE,  // Different signal value
        0,  // unused
    };
    
    // Create and submit compute IB
    gem_args.in.bo_size = sizeof(compute_ib);
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &gem_args);
    uint32_t compute_ib_handle = gem_args.out.handle;
    
    mmap_args.in.handle = compute_ib_handle;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &mmap_args);
    void* compute_ib_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED,
                               fd, mmap_args.out.addr_ptr);
    memcpy(compute_ib_ptr, compute_ib, sizeof(compute_ib));
    munmap(compute_ib_ptr, 4096);
    
    va_args.handle = compute_ib_handle;
    va_args.va_address = 0x800004000;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &va_args);
    
    // Submit compute with all BOs
    struct drm_amdgpu_bo_list_entry compute_bo_list[] = {
        {signal_handle, 0},
        {shader_handle, 0},
        {output_handle, 0},
        {compute_ib_handle, 0}
    };
    bo_list_args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
    bo_list_args.in.bo_number = 4;
    bo_list_args.in.bo_info_ptr = (uintptr_t)compute_bo_list;
    ioctl(fd, DRM_IOCTL_AMDGPU_BO_LIST, &bo_list_args);
    
    ib_chunk.va_start = va_args.va_address;
    ib_chunk.ib_bytes = sizeof(compute_ib);
    ib_chunk.ip_type = AMDGPU_HW_IP_COMPUTE;
    
    cs_args.in.bo_list_handle = bo_list_args.out.list_handle;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args) == 0) {
        printf("✓ Compute IB submitted\n");
        
        // Wait for completion
        union drm_amdgpu_wait_cs wait_args = {0};
        wait_args.in.handle = cs_args.out.handle;
        wait_args.in.ip_type = AMDGPU_HW_IP_COMPUTE;
        wait_args.in.ip_instance = 0;
        wait_args.in.ring = 0;
        wait_args.in.ctx_id = ctx_id;
        wait_args.in.timeout = 1000000000;
        ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args);
        
        // Check results
        printf("\nResults:\n");
        printf("  Compute EOP signal: 0x%08X (expected 0xCAFEBABE)\n", signal_ptr[1]);
        printf("  Output[0]: 0x%08X (expected 0xDEADBEEF)\n", output_ptr[0]);
        printf("  Output[1]: 0x%08X\n", output_ptr[1]);
        
        if (output_ptr[0] == 0xDEADBEEF) {
            printf("\n✅ SUCCESS! Compute shader executed!\n");
        } else if (signal_ptr[1] == 0xCAFEBABE) {
            printf("\n⚠️  EOP signaled but no output write\n");
        } else {
            printf("\n❌ No execution detected\n");
        }
    } else {
        printf("❌ Compute submission failed: %s\n", strerror(errno));
    }
    
    munmap(signal_ptr, 4096);
    munmap(output_ptr, 4096);
    close(fd);
    
    return 0;
}