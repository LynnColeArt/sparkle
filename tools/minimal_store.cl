// Minimal OpenCL kernel that stores 0xDEADBEEF to a pointer
__kernel void minimal_store(__global uint* output) {
    if (get_global_id(0) == 0) {
        *output = 0xDEADBEEF;
    }
}