// SAXPY kernel: Y = alpha * X + Y
// OpenCL C kernel that can be compiled for any GPU architecture

__kernel void saxpy(__global float* Y,
                    __global const float* X,
                    const float alpha,
                    const int n)
{
    int tid = get_global_id(0);
    
    if (tid < n) {
        Y[tid] = alpha * X[tid] + Y[tid];
    }
}