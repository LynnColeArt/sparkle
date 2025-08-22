#ifndef SP_COMPUTE_SUBMIT_H
#define SP_COMPUTE_SUBMIT_H

#include <stdint.h>
#include <sys/types.h>

// Forward declarations
typedef struct sp_pm4_ctx sp_pm4_ctx;
typedef struct sp_bo sp_bo;
typedef struct sp_fence sp_fence;
typedef struct sp_device_info sp_device_info;

// Buffer flags
#define SP_BO_HOST_VISIBLE  (1 << 0)
#define SP_BO_DEVICE_LOCAL  (1 << 1)
#define SP_BO_CACHED        (1 << 2)

// Device info structure
struct sp_device_info {
    uint32_t device_id;
    uint32_t chip_rev;
    uint32_t num_compute_rings;
    uint64_t vram_size;
    uint64_t gtt_size;
    char name[64];
};

// PM4 Context Management
sp_pm4_ctx* sp_pm4_ctx_create(void);
void sp_pm4_ctx_destroy(sp_pm4_ctx* ctx);
int sp_pm4_get_device_info(sp_pm4_ctx* ctx, sp_device_info* info);

// Buffer Object Management
sp_bo* sp_bo_new(sp_pm4_ctx* ctx, size_t size);
void sp_bo_free(sp_bo* bo);
void* sp_bo_map(sp_bo* bo);
void sp_bo_unmap(sp_bo* bo);
uint64_t sp_bo_get_va(sp_bo* bo);

// Golden Submit Path - All BOs in one submission
int sp_submit_ib_with_bos(sp_pm4_ctx* ctx, 
                          sp_bo* ib_bo, 
                          uint32_t ib_size_dw,
                          sp_bo** data_bos, 
                          uint32_t num_data_bos, 
                          sp_fence* out_fence);

// Fence Management
int sp_fence_wait(sp_pm4_ctx* ctx, sp_fence* fence, uint64_t timeout_ns);
int sp_fence_get_status(sp_pm4_ctx* ctx, sp_fence* fence);

// Preamble Builders (from pm4/preamble.c)
uint32_t sp_build_compute_preamble(uint32_t* ib, uint32_t max_dwords);
uint32_t sp_set_shader_address(uint32_t* ib, uint64_t shader_va);
uint32_t sp_set_shader_resources(uint32_t* ib, uint32_t vgprs, uint32_t sgprs);
uint32_t sp_set_thread_dimensions(uint32_t* ib, uint32_t x, uint32_t y, uint32_t z);
uint32_t sp_build_dispatch_direct(uint32_t* ib, uint32_t dim_x, uint32_t dim_y, uint32_t dim_z);
uint32_t sp_build_compute_dispatch(uint32_t* ib, uint32_t max_dwords,
                                   uint64_t shader_va,
                                   uint32_t thread_x, uint32_t thread_y, uint32_t thread_z,
                                   uint32_t grid_x, uint32_t grid_y, uint32_t grid_z);

// Self-test entry point
int sp_pm4_selftest(sp_pm4_ctx* ctx);

// Opaque structure definitions (implementation details hidden)
struct sp_pm4_ctx {
    int fd;
    uint32_t device_id;
    uint32_t gpu_ctx_id;
    int num_compute_rings;
};

struct sp_bo {
    void* cpu_ptr;
    uint64_t gpu_va;
    size_t size;
    uint32_t handle;
    uint32_t flags;
    sp_pm4_ctx* ctx;  // Keep reference to context
};

struct sp_fence {
    uint32_t ctx_id;
    uint32_t ip_type;
    uint32_t ring;
    uint64_t fence;
};

#endif // SP_COMPUTE_SUBMIT_H