; Mini's one-lane PAL probe kernel for RDNA2
; Only lane 0 writes to avoid stampede

.text
.globl mini_probe
.type mini_probe,@function

mini_probe:
    ; Get lane ID into v3
    v_mbcnt_lo_u32_b32 v3, -1, 0
    
    ; Check if lane 0
    v_cmp_eq_u32_e32 vcc, 0, v3
    s_cbranch_vccz .Ldone
    
    ; Lane 0 only: load address from USER_DATA
    v_mov_b32 v0, s0        ; addr.lo from USER_DATA_0
    v_mov_b32 v1, s1        ; addr.hi from USER_DATA_1
    v_mov_b32 v2, 0xDEADBEEF
    
    ; Store to memory
    global_store_dword v[0:1], v2, off
    s_waitcnt vmcnt(0) lgkmcnt(0)
    
.Ldone:
    s_endpgm
.size mini_probe, .-mini_probe