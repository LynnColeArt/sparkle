#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <libdrm/amdgpu.h>
#include <libdrm/amdgpu_drm.h>

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
    printf("=== libdrm_amdgpu Minimal Test ===\n");
    printf("Using high-level API that handles initialization\n\n");
    
    // 1. Initialize device
    amdgpu_device_handle device;
    uint32_t major, minor;
    
    int fd = open("/dev/dri/renderD129", O_RDWR);
    if (fd < 0) {
        perror("Failed to open device");
        return 1;
    }
    
    int r = amdgpu_device_initialize(fd, &major, &minor, &device);
    if (r) {
        printf("Failed to initialize device: %d\n", r);
        close(fd);
        return 1;
    }
    printf("✓ Device initialized: %u.%u\n", major, minor);
    
    // 2. Create context
    amdgpu_context_handle context;
    r = amdgpu_cs_ctx_create(device, &context);
    if (r) {
        printf("Failed to create context: %d\n", r);
        return 1;
    }
    printf("✓ Context created\n");
    
    // 3. Allocate BO
    amdgpu_bo_handle bo;
    amdgpu_va_handle va_handle;
    uint64_t va;
    
    struct amdgpu_bo_alloc_request req = {0};
    req.alloc_size = 4096;
    req.phys_alignment = 4096;
    req.preferred_heap = AMDGPU_GEM_DOMAIN_GTT;
    
    amdgpu_bo_alloc_result result = {0};
    r = amdgpu_bo_alloc(device, &req, &result);
    if (r) {
        printf("Failed to allocate BO: %d\n", r);
        return 1;
    }
    bo = result.buf_handle;
    printf("✓ BO allocated\n");
    
    // 4. Map BO for CPU
    void *cpu_ptr;
    r = amdgpu_bo_cpu_map(bo, &cpu_ptr);
    if (r) {
        printf("Failed to map BO: %d\n", r);
        return 1;
    }
    uint32_t *bo_cpu = (uint32_t*)cpu_ptr;
    
    // Initialize
    bo_cpu[0] = 0xDEADBEEF;
    printf("✓ BO[0] = 0x%08X\n", bo_cpu[0]);
    
    // 5. Map to GPU VA
    r = amdgpu_va_range_alloc(device, amdgpu_gpu_va_range_general,
                              4096, 4096, 0, &va, &va_handle, 0);
    if (r) {
        printf("Failed to allocate VA: %d\n", r);
        return 1;
    }
    
    r = amdgpu_bo_va_op(bo, 0, 4096, va, 0, AMDGPU_VA_OP_MAP);
    if (r) {
        printf("Failed to map VA: %d\n", r);
        return 1;
    }
    printf("✓ VA mapped: 0x%016llx\n", (unsigned long long)va);
    
    // 6. Build IB
    uint32_t *ib = &bo_cpu[32];  // IB at offset 128
    int idx = 0;
    
    // WRITE_DATA
    ib[idx++] = pkt3_header(IT_WRITE_DATA, 4);
    ib[idx++] = write_data_control();
    ib[idx++] = (uint32_t)(va & 0xFFFFFFFF);
    ib[idx++] = (uint32_t)((va >> 32) & 0xFFFF);
    ib[idx++] = 0xCAFEBABE;
    
    // Pad to 32 dwords
    ib[idx++] = pkt3_header(IT_NOP, 25);
    while (idx < 32) ib[idx++] = 0;
    
    printf("\n✓ IB built\n");
    
    // 7. Submit CS
    struct amdgpu_cs_request cs_req = {0};
    struct amdgpu_cs_ib_info ib_info = {0};
    struct amdgpu_cs_fence fence = {0};
    
    cs_req.ip_type = AMDGPU_HW_IP_GFX;
    cs_req.ring = 0;
    cs_req.number_of_ibs = 1;
    cs_req.ibs = &ib_info;
    cs_req.resources = &bo;
    cs_req.num_resources = 1;
    cs_req.fence_info = &fence;
    
    ib_info.ib_mc_address = va + 128;  // IB at offset 128
    ib_info.size = 32;  // 32 dwords
    ib_info.flags = 0;
    
    printf("\nSubmitting CS...\n");
    r = amdgpu_cs_submit(context, 0, &cs_req, 1);
    if (r) {
        printf("❌ CS submit failed: %d\n", r);
        return 1;
    }
    printf("✓ CS submitted!\n");
    
    // 8. Wait for fence
    r = amdgpu_cs_query_fence_status(&fence, 1000000000, 0, NULL);
    if (r) {
        printf("Fence wait failed: %d\n", r);
    } else {
        printf("✓ Fence signaled\n");
    }
    
    // 9. Check result
    printf("\n=== RESULT ===\n");
    printf("BO[0] = 0x%08X (expected 0xCAFEBABE)\n", bo_cpu[0]);
    
    if (bo_cpu[0] == 0xCAFEBABE) {
        printf("\n✅ SUCCESS! libdrm_amdgpu works!\n");
        printf("The initialization was the missing piece!\n");
    } else {
        printf("\n❌ FAILED: Even with libdrm\n");
    }
    
    // Cleanup
    amdgpu_bo_cpu_unmap(bo);
    amdgpu_bo_free(bo);
    amdgpu_cs_ctx_free(context);
    amdgpu_device_deinitialize(device);
    close(fd);
    
    return 0;
}