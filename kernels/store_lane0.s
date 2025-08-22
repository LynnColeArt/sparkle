// store_lane0.s - PAL-style RDNA2 kernel with lane 0 guard
// =========================================================
// Only lane 0 writes to avoid concurrent write issues

        .text
        .globl store_lane0
        .p2align 8
store_lane0:
        // Get lane ID and check if we're lane 0
        v_mbcnt_lo_u32_b32 v3, -1, 0       // v3 = lane_id (lower 32 lanes)
        v_mbcnt_hi_u32_b32 v3, -1, v3      // v3 = lane_id (all 64 lanes)
        v_cmp_eq_u32_e32 vcc, 0, v3         // vcc = (lane_id == 0)
        s_and_saveexec_b64 s[4:5], vcc      // exec = exec & vcc, save old exec
        s_cbranch_execz end_pgm             // if no lanes active, skip to end
        
        // Lane 0 does the store
        v_mov_b32       v0, s0              // vaddr.lo = low(dst)
        v_mov_b32       v1, s1              // vaddr.hi = high(dst)
        v_mov_b32       v2, 0xDEADBEEF      // data = 0xDEADBEEF
        global_store_dword v[0:1], v2, off  // *(uint32_t*)addr = 0xDEADBEEF
        s_waitcnt       vmcnt(0) lgkmcnt(0) // wait for store
        
end_pgm:
        s_or_b64        exec, exec, s[4:5]  // restore exec mask
        s_endpgm                            // end program