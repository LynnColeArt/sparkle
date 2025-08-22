// store_ud.s - Minimal PAL-style RDNA2 kernel
// ===========================================
// Reads pointer from s0:s1 (USER_DATA_0/1) and stores 0xDEADBEEF

        .text
        .globl store_ud
        .p2align 8
store_ud:
        v_mov_b32     v0, s0                // vaddr.lo = low(dst)
        v_mov_b32     v1, s1                // vaddr.hi = high(dst)
        v_mov_b32     v2, 0xDEADBEEF        // data = 0xDEADBEEF
        global_store_dword v[0:1], v2, off   // *(uint32_t*)addr = 0xDEADBEEF
        s_waitcnt     vmcnt(0) lgkmcnt(0)   // wait for store
        s_endpgm                            // end program