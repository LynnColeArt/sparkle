#include <stdio.h>
#include <string.h>

void test_c_string(const char* str) {
    printf("Received string: '%s'\n", str);
    printf("String length: %zu\n", strlen(str));
    
    // Print hex values of first 20 chars
    printf("Hex values: ");
    for (int i = 0; i < 20 && str[i] != '\0'; i++) {
        printf("%02x ", (unsigned char)str[i]);
    }
    printf("\n\n");
}