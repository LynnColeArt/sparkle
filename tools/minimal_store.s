	.text
	.section	.AMDGPU.config,"",@progbits
	.long	47176
	.long	1622081861
	.long	47180
	.long	5005
	.long	47200
	.long	0
	.long	4
	.long	0
	.long	8
	.long	0
	.text
	.globl	minimal_store                   ; -- Begin function minimal_store
	.p2align	8
	.type	minimal_store,@function
minimal_store:                          ; @minimal_store
minimal_store$local:
	.type	minimal_store$local,@function
; %bb.0:
	s_mov_b32 s36, SCRATCH_RSRC_DWORD0
	v_lshlrev_b32_e32 v2, 20, v2
	v_lshlrev_b32_e32 v1, 10, v1
	s_mov_b32 s37, SCRATCH_RSRC_DWORD1
	s_mov_b32 s38, -1
	s_mov_b32 s39, 0x31c16000
	s_add_u32 s36, s36, s9
	s_addc_u32 s37, s37, 0
	s_mov_b64 s[34:35], s[2:3]
	v_or3_b32 v31, v0, v1, v2
	v_mov_b32_e32 v0, 0
	s_mov_b32 s14, s8
	s_mov_b64 s[10:11], s[4:5]
	s_add_u32 s8, s34, 44
	s_mov_b64 s[4:5], s[0:1]
	s_mov_b64 s[0:1], s[36:37]
	s_addc_u32 s9, s35, 0
	s_mov_b32 s12, s6
	s_mov_b32 s13, s7
	s_mov_b64 s[2:3], s[38:39]
	s_mov_b32 s32, 0
	s_getpc_b64 s[16:17]
	s_add_u32 s16, s16, _Z13get_global_idj@rel32@lo+4
	s_addc_u32 s17, s17, _Z13get_global_idj@rel32@hi+12
	v_mov_b32_e32 v40, 0
	s_swappc_b64 s[30:31], s[16:17]
	s_mov_b32 s0, exec_lo
	v_cmpx_eq_u64_e32 0, v[0:1]
	s_cbranch_execz .LBB0_2
; %bb.1:
	s_load_dwordx2 s[0:1], s[34:35], 0x24
	v_mov_b32_e32 v0, 0xdeadbeef
	s_waitcnt lgkmcnt(0)
	global_store_dword v40, v0, s[0:1]
.LBB0_2:
	s_endpgm
.Lfunc_end0:
	.size	minimal_store, .Lfunc_end0-minimal_store
	.size	minimal_store$local, .Lfunc_end0-minimal_store
                                        ; -- End function
	.section	.AMDGPU.csdata,"",@progbits
; Kernel info:
; codeLenInByte = 172
; NumSgprs: 42
; NumVgprs: 41
; ScratchSize: 0
; MemoryBound: 0
; FloatMode: 240
; IeeeMode: 1
; LDSByteSize: 0 bytes/workgroup (compile time only)
; SGPRBlocks: 5
; VGPRBlocks: 5
; NumSGPRsForWavesPerEU: 42
; NumVGPRsForWavesPerEU: 41
; Occupancy: 16
; WaveLimiterHint : 0
; COMPUTE_PGM_RSRC2:SCRATCH_EN: 1
; COMPUTE_PGM_RSRC2:USER_SGPR: 6
; COMPUTE_PGM_RSRC2:TRAP_HANDLER: 0
; COMPUTE_PGM_RSRC2:TGID_X_EN: 1
; COMPUTE_PGM_RSRC2:TGID_Y_EN: 1
; COMPUTE_PGM_RSRC2:TGID_Z_EN: 1
; COMPUTE_PGM_RSRC2:TIDIG_COMP_CNT: 2
	.hidden	__oclc_ABI_version              ; @__oclc_ABI_version
	.type	__oclc_ABI_version,@object
	.section	.rodata,"a",@progbits
	.weak	__oclc_ABI_version
	.p2align	2, 0x0
__oclc_ABI_version:
	.long	500                             ; 0x1f4
	.size	__oclc_ABI_version, 4

	.ident	"Ubuntu clang version 18.1.3 (1ubuntu1)"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.amd_amdgpu_isa "amdgcn----gfx1036"
