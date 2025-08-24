#include <stdio.h>
#include <stdint.h>

// Mini's minimal lane-0 PAL probe shader
static const uint32_t deadbeef_shader[] = {
    // v_mbcnt_lo_u32_b32 v0, -1, 0
    0x4C000080,  
    // v_mbcnt_hi_u32_b32 v0, -1, v0  
    0x4C020080,
    // v_cmp_eq_u32 vcc, v0, 0 (check if lane 0)
    0x7D840080,
    // s_cbranch_vccz to skip (if not lane 0)
    0xBF860004,
    // Lane 0 code:
    // v_mov_b32 v0, 0xDEADBEEF
    0x7E0002FF, 0xDEADBEEF,
    // v_mov_b32 v1, s0 (USER_DATA_0 low)
    0x7E020200,
    // v_mov_b32 v2, s1 (USER_DATA_0 high)
    0x7E040201,
    // global_store_dword v[1:2], v0, off
    0xDC700000, 0x00000100,
    // s_waitcnt vmcnt(0) lgkmcnt(0)
    0xBF8C0070,
    // s_endpgm
    0xBF810000
};

uint32_t swap32(uint32_t val) {
    return ((val & 0xFF000000) >> 24) |
           ((val & 0x00FF0000) >> 8) |
           ((val & 0x0000FF00) << 8) |
           ((val & 0x000000FF) << 24);
}

int main() {
    printf("Original shader:\n");
    for (int i = 0; i < sizeof(deadbeef_shader)/4; i++) {
        printf("  [%02d] 0x%08X", i, deadbeef_shader[i]);
        if (i == 4) printf(" (v_mov_b32 v0, 0xDEADBEEF - first part)");
        else if (i == 5) printf(" (0xDEADBEEF immediate)");
        else if (i == 9) printf(" (global_store_dword - second part)");
        printf("\n");
    }
    
    printf("\nSwapped shader:\n");
    for (int i = 0; i < sizeof(deadbeef_shader)/4; i++) {
        printf("  [%02d] 0x%08X\n", i, swap32(deadbeef_shader[i]));
    }
    
    return 0;
}