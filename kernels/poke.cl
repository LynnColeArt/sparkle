// poke.cl - Minimal RDNA2 test kernel
// Write 0xDEADBEEF to prove compute works

__kernel void poke(__global uint *dst) {
    dst[0] = 0xDEADBEEF;
}