#include <errno.h>
#include <string.h>
#include <stdio.h>

int main() {
    printf("Common errno values:\n");
    printf("EINVAL: %d (%s)\n", EINVAL, strerror(EINVAL));
    printf("EPERM: %d (%s)\n", EPERM, strerror(EPERM));
    printf("ENOMEM: %d (%s)\n", ENOMEM, strerror(ENOMEM));
    printf("EACCES: %d (%s)\n", EACCES, strerror(EACCES));
    printf("EFAULT: %d (%s)\n", EFAULT, strerror(EFAULT));
    printf("ENODEV: %d (%s)\n", ENODEV, strerror(ENODEV));
    return 0;
}