#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>

// Try the absolute simplest possible command submission

#define DRM_AMDGPU_CS    0x04
#define DRM_IOCTL_BASE   'd'
#define DRM_COMMAND_BASE 0x40
#define DRM_IOWR(nr, type) _IOWR(DRM_IOCTL_BASE, DRM_COMMAND_BASE + (nr), type)

int main() {
    int fd = open("/dev/dri/card0", O_RDWR);
    if (fd < 0) {
        perror("open");
        return 1;
    }
    
    // Try absolute minimal CS ioctl with all zeros
    uint64_t cs_data[3] = {0};  // 24 bytes of zeros
    
    printf("Trying minimal CS ioctl with zeros...\n");
    int ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, uint64_t[3]), cs_data);
    printf("Result: %d, errno: %d (%s)\n", ret, errno, strerror(errno));
    
    // Try with just context ID 0
    cs_data[0] = 0;  // ctx_id=0, bo_list_handle=0
    cs_data[1] = 0;  // num_chunks=0, pad=0
    cs_data[2] = 0;  // chunks=NULL
    
    printf("\nTrying CS with ctx_id=0, no chunks...\n");
    ret = ioctl(fd, DRM_IOWR(DRM_AMDGPU_CS, uint64_t[3]), cs_data);
    printf("Result: %d, errno: %d (%s)\n", ret, errno, strerror(errno));
    
    close(fd);
    
    printf("\nThe EINVAL suggests the kernel is rejecting our input.\n");
    printf("This could mean:\n");
    printf("1. We need a valid context ID (not 0)\n");
    printf("2. We need at least one chunk\n");
    printf("3. The ioctl size calculation might be wrong\n");
    
    return 0;
}