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

// Simplest possible shader - just s_endpgm
static const uint32_t nop_shader[] = {
    0xBF810000  // s_endpgm
};

// Write 0xDEADBEEF to s[0:1]
static const uint32_t deadbeef_shader[] = {
    // s_mov_b32 s2, 0xDEADBEEF  
    0xBE8200FF, 0xDEADBEEF,
    // s_waitcnt lgkmcnt(0)
    0xBF8CC07F,
    // s_store_dword s2, s[0:1], 0x0
    0xC0021C00, 0x00000200,
    // s_waitcnt lgkmcnt(0)
    0xBF8CC07F,
    // s_endpgm
    0xBF810000
};

// Minimal PM4 packet builder
uint32_t build_minimal_ib(uint32_t* ib, uint64_t shader_va, uint64_t output_va) {
    uint32_t idx = 0;
    
    // CLEAR_STATE
    ib[idx++] = 0xC0000012;
    ib[idx++] = 0x00000000;
    
    // CONTEXT_CONTROL 
    ib[idx++] = 0xC0010028;
    ib[idx++] = 0x80000000;
    ib[idx++] = 0x80000000;
    
    // SET_SH_REG - COMPUTE_USER_DATA_0 (s[0:1])
    ib[idx++] = 0xC0020076;  // PKT3(SET_SH_REG, 2)
    ib[idx++] = 0x240;       // COMPUTE_USER_DATA_0 offset
    ib[idx++] = (uint32_t)(output_va & 0xFFFFFFFF);
    ib[idx++] = (uint32_t)(output_va >> 32);
    
    // SET_SH_REG - COMPUTE_PGM_LO/HI
    ib[idx++] = 0xC0020076;  // PKT3(SET_SH_REG, 2)
    ib[idx++] = 0x204;       // COMPUTE_PGM_LO offset
    ib[idx++] = (uint32_t)(shader_va >> 8);   // bits [39:8]
    ib[idx++] = (uint32_t)(shader_va >> 40);  // bits [47:40]
    
    // SET_SH_REG - COMPUTE_PGM_RSRC1
    ib[idx++] = 0xC0020076;  // PKT3(SET_SH_REG, 2)
    ib[idx++] = 0x206;       // COMPUTE_PGM_RSRC1 offset
    ib[idx++] = 0x00140000;  // VGPR=1, SGPR=2, DX10_CLAMP=1, IEEE_MODE=1
    ib[idx++] = 0x00000084;  // USER_SGPR=1, USER_SGPR_COUNT=2
    
    // SET_SH_REG - COMPUTE_NUM_THREAD_X/Y/Z
    ib[idx++] = 0xC0030076;  // PKT3(SET_SH_REG, 3)
    ib[idx++] = 0x20A;       // COMPUTE_NUM_THREAD_X offset
    ib[idx++] = 64;          // 64 threads
    ib[idx++] = 1;
    ib[idx++] = 1;
    
    // DISPATCH_DIRECT
    ib[idx++] = 0xC0040015;  // PKT3(DISPATCH_DIRECT, 4)
    ib[idx++] = 1;           // dim_x = 1 workgroup
    ib[idx++] = 1;           // dim_y = 1
    ib[idx++] = 1;           // dim_z = 1
    ib[idx++] = 0x00000045;  // COMPUTE_SHADER_EN | FORCE_START_AT_000 | ORDER_MODE
    
    return idx;
}

int main() {
    printf("=== Mini's Simple PM4 Test ===\n\n");
    
    // Open GPU
    int fd = open("/dev/dri/renderD129", O_RDWR);
    if (fd < 0) {
        fd = open("/dev/dri/renderD128", O_RDWR);
    }
    
    if (fd < 0) {
        perror("Failed to open GPU");
        return 1;
    }
    
    // Create GPU context
    union drm_amdgpu_ctx ctx_args = {0};
    ctx_args.in.op = AMDGPU_CTX_OP_ALLOC_CTX;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args) < 0) {
        perror("Failed to create GPU context");
        close(fd);
        return 1;
    }
    
    uint32_t gpu_ctx = ctx_args.out.alloc.ctx_id;
    printf("GPU context created: %u\n", gpu_ctx);
    
    // Create shader buffer
    union drm_amdgpu_gem_create shader_gem = {0};
    shader_gem.in.bo_size = 4096;
    shader_gem.in.alignment = 4096;
    shader_gem.in.domains = AMDGPU_GEM_DOMAIN_VRAM;
    shader_gem.in.domain_flags = AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &shader_gem) < 0) {
        perror("Failed to create shader buffer");
        close(fd);
        return 1;
    }
    
    // Map and upload shader
    union drm_amdgpu_gem_mmap shader_mmap = {0};
    shader_mmap.in.handle = shader_gem.out.handle;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &shader_mmap) < 0) {
        perror("Failed to get shader mmap offset");
        close(fd);
        return 1;
    }
    
    void* shader_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED, 
                           fd, shader_mmap.out.addr_ptr);
    if (shader_ptr == MAP_FAILED) {
        perror("Failed to mmap shader buffer");
        close(fd);
        return 1;
    }
    
    // Copy shader
    memcpy(shader_ptr, deadbeef_shader, sizeof(deadbeef_shader));
    munmap(shader_ptr, 4096);
    
    // Get shader VA
    union drm_amdgpu_gem_va shader_va_args = {0};
    shader_va_args.in.handle = shader_gem.out.handle;
    shader_va_args.in.operation = AMDGPU_VA_OP_MAP;
    shader_va_args.in.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_EXECUTABLE;
    shader_va_args.in.va_address = 0x800010000;  // Fixed address
    shader_va_args.in.offset_in_bo = 0;
    shader_va_args.in.map_size = 4096;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &shader_va_args) < 0) {
        perror("Failed to map shader VA");
        close(fd);
        return 1;
    }
    
    uint64_t shader_va = shader_va_args.in.va_address;
    printf("Shader at VA: 0x%016lX\n", shader_va);
    
    // Create output buffer
    union drm_amdgpu_gem_create output_gem = {0};
    output_gem.in.bo_size = 4096;
    output_gem.in.alignment = 4096;
    output_gem.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    output_gem.in.domain_flags = AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &output_gem) < 0) {
        perror("Failed to create output buffer");
        close(fd);
        return 1;
    }
    
    // Map and initialize output
    union drm_amdgpu_gem_mmap output_mmap = {0};
    output_mmap.in.handle = output_gem.out.handle;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &output_mmap) < 0) {
        perror("Failed to get output mmap offset");
        close(fd);
        return 1;
    }
    
    uint32_t* output_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED, 
                               fd, output_mmap.out.addr_ptr);
    if (output_ptr == MAP_FAILED) {
        perror("Failed to mmap output buffer");
        close(fd);
        return 1;
    }
    
    // Initialize with pattern
    for (int i = 0; i < 16; i++) {
        output_ptr[i] = 0xCAFE0000 + i;
    }
    
    // Get output VA
    union drm_amdgpu_gem_va output_va_args = {0};
    output_va_args.in.handle = output_gem.out.handle;
    output_va_args.in.operation = AMDGPU_VA_OP_MAP;
    output_va_args.in.flags = AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE;
    output_va_args.in.va_address = 0x800020000;  // Fixed address
    output_va_args.in.offset_in_bo = 0;
    output_va_args.in.map_size = 4096;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &output_va_args) < 0) {
        perror("Failed to map output VA");
        close(fd);
        return 1;
    }
    
    uint64_t output_va = output_va_args.in.va_address;
    printf("Output at VA: 0x%016lX\n", output_va);
    
    // Create IB
    union drm_amdgpu_gem_create ib_gem = {0};
    ib_gem.in.bo_size = 4096;
    ib_gem.in.alignment = 4096;
    ib_gem.in.domains = AMDGPU_GEM_DOMAIN_GTT;
    ib_gem.in.domain_flags = 0;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_CREATE, &ib_gem) < 0) {
        perror("Failed to create IB buffer");
        close(fd);
        return 1;
    }
    
    // Map and build IB
    union drm_amdgpu_gem_mmap ib_mmap = {0};
    ib_mmap.in.handle = ib_gem.out.handle;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_MMAP, &ib_mmap) < 0) {
        perror("Failed to get IB mmap offset");
        close(fd);
        return 1;
    }
    
    uint32_t* ib_ptr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED, 
                           fd, ib_mmap.out.addr_ptr);
    if (ib_ptr == MAP_FAILED) {
        perror("Failed to mmap IB buffer");
        close(fd);
        return 1;
    }
    
    uint32_t ib_size = build_minimal_ib(ib_ptr, shader_va, output_va);
    
    printf("\nIB contents (%d dwords):\n", ib_size);
    for (uint32_t i = 0; i < ib_size; i++) {
        printf("  [%02d] 0x%08X\n", i, ib_ptr[i]);
    }
    
    munmap(ib_ptr, 4096);
    
    // Get IB VA
    union drm_amdgpu_gem_va ib_va_args = {0};
    ib_va_args.in.handle = ib_gem.out.handle;
    ib_va_args.in.operation = AMDGPU_VA_OP_MAP;
    ib_va_args.in.flags = AMDGPU_VM_PAGE_READABLE;
    ib_va_args.in.va_address = 0x800030000;  // Fixed address
    ib_va_args.in.offset_in_bo = 0;
    ib_va_args.in.map_size = 4096;
    
    if (ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &ib_va_args) < 0) {
        perror("Failed to map IB VA");
        close(fd);
        return 1;
    }
    
    uint64_t ib_va = ib_va_args.in.va_address;
    
    // Submit CS
    struct drm_amdgpu_bo_list_entry bo_list[3] = {
        {shader_gem.out.handle, 0},
        {output_gem.out.handle, 1},
        {ib_gem.out.handle, 2}
    };
    
    struct drm_amdgpu_cs_chunk chunks[2] = {0};
    struct drm_amdgpu_cs_chunk_ib ib_chunk = {0};
    struct drm_amdgpu_cs_chunk_data bo_list_chunk = {0};
    
    // IB chunk
    ib_chunk.va_start = ib_va;
    ib_chunk.ib_bytes = ib_size * 4;
    ib_chunk.ip_type = AMDGPU_HW_IP_COMPUTE;
    ib_chunk.ip_instance = 0;
    ib_chunk.ring = 0;
    ib_chunk.flags = 0;
    
    chunks[0].chunk_id = AMDGPU_CHUNK_ID_IB;
    chunks[0].length_dw = sizeof(ib_chunk) / 4;
    chunks[0].chunk_data = (uintptr_t)&ib_chunk;
    
    // BO list chunk
    bo_list_chunk.bo_list_handle = (uintptr_t)bo_list;
    bo_list_chunk.bo_number = 3;
    
    chunks[1].chunk_id = AMDGPU_CHUNK_ID_BO_HANDLES;
    chunks[1].length_dw = sizeof(bo_list_chunk) / 4;
    chunks[1].chunk_data = (uintptr_t)&bo_list_chunk;
    
    union drm_amdgpu_cs cs_args = {0};
    cs_args.in.ctx_id = gpu_ctx;
    cs_args.in.num_chunks = 2;
    cs_args.in.chunks = (uintptr_t)chunks;
    
    printf("\nSubmitting CS...\n");
    if (ioctl(fd, DRM_IOCTL_AMDGPU_CS, &cs_args) < 0) {
        perror("Failed to submit CS");
        close(fd);
        return 1;
    }
    
    uint64_t seq = cs_args.out.handle;
    printf("CS submitted, sequence: %lu\n", seq);
    
    // Wait for completion
    union drm_amdgpu_wait_cs wait_args = {0};
    wait_args.in.handle = seq;
    wait_args.in.ip_type = AMDGPU_HW_IP_COMPUTE;
    wait_args.in.ip_instance = 0;
    wait_args.in.ring = 0;
    wait_args.in.ctx_id = gpu_ctx;
    wait_args.in.timeout = 1000000000;  // 1 second
    
    printf("Waiting for completion...\n");
    if (ioctl(fd, DRM_IOCTL_AMDGPU_WAIT_CS, &wait_args) < 0) {
        perror("Failed to wait for CS");
        close(fd);
        return 1;
    }
    
    if (wait_args.out.status == 0) {
        printf("✅ CS completed successfully\n");
    } else {
        printf("❌ CS failed with status: %d\n", wait_args.out.status);
    }
    
    // Check output
    printf("\nOutput buffer:\n");
    for (int i = 0; i < 16; i++) {
        printf("  [%02d] 0x%08X", i, output_ptr[i]);
        if (output_ptr[i] == 0xDEADBEEF) {
            printf(" <- FOUND!");
        }
        printf("\n");
    }
    
    // Cleanup
    munmap(output_ptr, 4096);
    
    // Unmap VAs
    shader_va_args.in.operation = AMDGPU_VA_OP_UNMAP;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &shader_va_args);
    
    output_va_args.in.operation = AMDGPU_VA_OP_UNMAP;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &output_va_args);
    
    ib_va_args.in.operation = AMDGPU_VA_OP_UNMAP;
    ioctl(fd, DRM_IOCTL_AMDGPU_GEM_VA, &ib_va_args);
    
    // Close GEM handles
    struct drm_gem_close close_args = {0};
    close_args.handle = shader_gem.out.handle;
    ioctl(fd, DRM_IOCTL_GEM_CLOSE, &close_args);
    
    close_args.handle = output_gem.out.handle;
    ioctl(fd, DRM_IOCTL_GEM_CLOSE, &close_args);
    
    close_args.handle = ib_gem.out.handle;
    ioctl(fd, DRM_IOCTL_GEM_CLOSE, &close_args);
    
    // Destroy context
    ctx_args.in.op = AMDGPU_CTX_OP_FREE_CTX;
    ctx_args.in.ctx_id = gpu_ctx;
    ioctl(fd, DRM_IOCTL_AMDGPU_CTX, &ctx_args);
    
    close(fd);
    
    return 0;
}