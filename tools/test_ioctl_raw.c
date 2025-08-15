#include <stdio.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <string.h>

// Test with raw ioctl values
int main() {
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) {
        perror("open");
        return 1;
    }
    
    // Create context first
    uint8_t ctx_buffer[32] = {0};
    uint32_t *ctx_u32 = (uint32_t*)ctx_buffer;
    ctx_u32[0] = 1;  // AMDGPU_CTX_OP_ALLOC_CTX
    
    if (ioctl(fd, 0xc0106442, ctx_buffer) < 0) {  // DRM_IOCTL_AMDGPU_CTX
        perror("ctx");
        close(fd);
        return 1;
    }
    
    // Extract context ID from response
    uint32_t ctx_id = ctx_u32[4];  // out.alloc.ctx_id
    printf("Got context: %u\n", ctx_id);
    
    // Build CS ioctl data
    uint8_t cs_buffer[24] = {0};
    uint32_t *cs_u32 = (uint32_t*)cs_buffer;
    uint64_t *cs_u64 = (uint64_t*)cs_buffer;
    
    // Test 1: All zeros except ctx_id
    cs_u32[0] = ctx_id;  // ctx_id
    cs_u32[1] = 0;       // bo_list_handle  
    cs_u32[2] = 0;       // num_chunks
    cs_u32[3] = 0;       // _pad
    cs_u64[2] = 0;       // chunks pointer
    
    printf("\nTest 1: Zero chunks\n");
    int ret = ioctl(fd, 0xc0186444, cs_buffer);  // DRM_IOCTL_AMDGPU_CS
    printf("Result: %d, errno: %d (%s)\n", ret, errno, strerror(errno));
    
    // Test 2: Set num_chunks=1 but null pointer
    cs_u32[2] = 1;       // num_chunks = 1
    cs_u64[2] = 0;       // chunks = NULL
    
    printf("\nTest 2: num_chunks=1, chunks=NULL\n");
    ret = ioctl(fd, 0xc0186444, cs_buffer);
    printf("Result: %d, errno: %d (%s)\n", ret, errno, strerror(errno));
    
    // Test 3: Invalid but non-null pointer
    cs_u32[2] = 1;       // num_chunks = 1
    cs_u64[2] = 0x1000;  // chunks = some low address
    
    printf("\nTest 3: num_chunks=1, chunks=0x1000\n");
    ret = ioctl(fd, 0xc0186444, cs_buffer);
    printf("Result: %d, errno: %d (%s)\n", ret, errno, strerror(errno));
    
    // Test 4: Valid pointer to stack
    uint8_t chunk_buffer[16] = {0};
    cs_u32[2] = 1;                          // num_chunks = 1
    cs_u64[2] = (uint64_t)(uintptr_t)chunk_buffer;  // chunks = stack pointer
    
    printf("\nTest 4: num_chunks=1, chunks=stack @ %p\n", chunk_buffer);
    ret = ioctl(fd, 0xc0186444, cs_buffer);
    printf("Result: %d, errno: %d (%s)\n", ret, errno, strerror(errno));
    
    close(fd);
    
    printf("\nConclusions:\n");
    printf("- EINVAL with num_chunks=0 is expected\n");
    printf("- EFAULT with invalid pointers is expected\n");
    printf("- If Test 4 gives EFAULT, something is very wrong\n");
    
    return 0;
}