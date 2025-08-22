// Simple s_endpgm shader for gfx1036
.amdgcn_target "amdgcn-amd-amdhsa--gfx1036"

.text
.globl simple_endpgm
.p2align 8
.type simple_endpgm,@function

simple_endpgm:
    s_endpgm

.Lfunc_end0:
    .size simple_endpgm, .Lfunc_end0-simple_endpgm