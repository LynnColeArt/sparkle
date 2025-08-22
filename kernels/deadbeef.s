// DEADBEEF test kernel - writes 0xDEADBEEF to memory
// For GFX10/RDNA2 architecture

.text
.globl deadbeef_kernel
.p2align 8
.type deadbeef_kernel,@function

deadbeef_kernel:
    // Load output buffer address from kernarg
    s_load_dwordx2 s[0:1], s[0:1], 0x0     // s[0:1] = output buffer address
    
    // Write DEADBEEF
    s_mov_b32 s2, 0xDEADBEEF               // s2 = 0xDEADBEEF
    s_mov_b32 s3, 0                        // s3 = 0 (offset)
    s_waitcnt lgkmcnt(0)                   // wait for load
    s_store_dword s2, s[0:1], s3           // store to buffer
    
    // End program
    s_endpgm

.Lfunc_end0:
    .size deadbeef_kernel, .Lfunc_end0-deadbeef_kernel