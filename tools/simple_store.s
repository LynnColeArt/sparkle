; Simple RDNA 2 compute shader that stores 0xDEADBEEF to address in s[4:5]
; This matches our GCN3 shader functionality

.text
.globl simple_store
.p2align 8
.type simple_store,@function

simple_store:
    ; Load address from s[4:5] (kernel argument)
    s_load_dwordx2 s[0:1], s[4:5], 0x0
    
    ; Wait for load to complete
    s_waitcnt lgkmcnt(0)
    
    ; Move 0xDEADBEEF into v0
    v_mov_b32 v0, 0xDEADBEEF
    
    ; Store to global memory
    v_mov_b32 v1, 0  ; offset = 0
    global_store_dword v1, v0, s[0:1]
    
    ; Wait for store to complete
    s_waitcnt vmcnt(0)
    
    ; End program
    s_endpgm
    
.section .AMDGPU.config
.long 0xB848    ; COMPUTE_PGM_RSRC1 register
.long 0x000000C0 ; 6 SGPRs, 1 VGPR 
.long 0xB84C    ; COMPUTE_PGM_RSRC2 register  
.long 0x00000002 ; USER_SGPR = 2