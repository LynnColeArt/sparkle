; Simplest possible PAL kernel - just store
; No lane masking to avoid syntax issues

.text
.globl mini_simple
.type mini_simple,@function

mini_simple:
    ; Load address from USER_DATA
    v_mov_b32 v0, s0        ; addr.lo from USER_DATA_0
    v_mov_b32 v1, s1        ; addr.hi from USER_DATA_1
    v_mov_b32 v2, 0xDEADBEEF
    
    ; Store to memory
    global_store_dword v[0:1], v2, off
    s_waitcnt vmcnt(0) lgkmcnt(0)
    
    s_endpgm
.size mini_simple, .-mini_simple