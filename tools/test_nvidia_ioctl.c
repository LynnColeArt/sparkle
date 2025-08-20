#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>

// NVIDIA ioctl interface exploration
// Let's figure out the real interface!

#define NV_IOCTL_MAGIC 0x46  // 'F'
#define NV_IOCTL_BASE 0x64

// Try some known NVIDIA ioctl numbers
#define NV_ESC_CARD_INFO         0x00
#define NV_ESC_REGISTER_FD       0x01
#define NV_ESC_SYS_PARAMS        0x05
#define NV_ESC_CHECK_VERSION_STR 0x06
#define NV_ESC_RM_ALLOC          0x2a
#define NV_ESC_RM_FREE           0x29
#define NV_ESC_RM_CONTROL        0x2b

// ioctl encoding (from kernel)
#define _IOC_NRBITS     8
#define _IOC_TYPEBITS   8
#define _IOC_SIZEBITS   14
#define _IOC_DIRBITS    2

#define _IOC_NRSHIFT    0
#define _IOC_TYPESHIFT  (_IOC_NRSHIFT + _IOC_NRBITS)
#define _IOC_SIZESHIFT  (_IOC_TYPESHIFT + _IOC_TYPEBITS)
#define _IOC_DIRSHIFT   (_IOC_SIZESHIFT + _IOC_SIZEBITS)

#define _IOC_NONE  0U
#define _IOC_WRITE 1U
#define _IOC_READ  2U

#define _IOC(dir,type,nr,size) \
    (((dir)  << _IOC_DIRSHIFT) | \
     ((type) << _IOC_TYPESHIFT) | \
     ((nr)   << _IOC_NRSHIFT) | \
     ((size) << _IOC_SIZESHIFT))

#define _IOWR(type,nr,size) _IOC(_IOC_READ|_IOC_WRITE,(type),(nr),sizeof(size))

// NVIDIA specific ioctl macros
#define NV_IOCTL_CARD_INFO _IOWR(NV_IOCTL_MAGIC, NV_ESC_CARD_INFO, struct nv_card_info)

struct nv_card_info {
    uint32_t version;
    uint32_t flags;
    uint32_t gpu_id;
    uint32_t interrupt;
    uint64_t reg_address;
    uint64_t reg_size;
    uint64_t fb_address;
    uint64_t fb_size;
};

int main() {
    int fd;
    int ret;
    struct nv_card_info card_info = {0};
    
    printf("NVIDIA ioctl interface explorer\n");
    printf("================================\n\n");
    
    // Try opening different device nodes
    const char* devices[] = {
        "/dev/nvidia2",
        "/dev/nvidiactl",
        "/dev/nvidia-uvm",
        "/dev/dri/card3",
        "/dev/dri/renderD130",
        NULL
    };
    
    for (int i = 0; devices[i] != NULL; i++) {
        printf("Trying %s...\n", devices[i]);
        fd = open(devices[i], O_RDWR);
        
        if (fd < 0) {
            printf("  Failed to open: %s\n", strerror(errno));
            continue;
        }
        
        printf("  Opened successfully (fd=%d)\n", fd);
        
        // Try card info ioctl
        card_info.version = 1;
        ret = ioctl(fd, NV_IOCTL_CARD_INFO, &card_info);
        if (ret == 0) {
            printf("  Card info succeeded!\n");
            printf("    GPU ID: 0x%x\n", card_info.gpu_id);
            printf("    FB Size: %lu MB\n", card_info.fb_size / (1024*1024));
        } else {
            printf("  Card info failed: %s (errno=%d)\n", strerror(errno), errno);
            
            // Try a simple ioctl to see if the device responds
            unsigned int version = 0;
            ret = ioctl(fd, _IOWR('F', 0x00, unsigned int), &version);
            if (ret == 0) {
                printf("  Simple version ioctl succeeded: 0x%x\n", version);
            }
        }
        
        close(fd);
        printf("\n");
    }
    
    return 0;
}