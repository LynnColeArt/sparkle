module sporkle_metal_kernels
  ! Metal Shading Language kernels for GPU compute
  ! The Sporkle Way: Same math, different syntax!
  
  use kinds
  implicit none
  private
  
  public :: get_metal_vector_add, get_metal_saxpy, get_metal_gemm
  public :: get_metal_reduction, get_metal_complex
  
contains

  ! Vector addition: z = x + y
  function get_metal_vector_add() result(source)
    character(len=:), allocatable :: source
    
    source = &
      "#include <metal_stdlib>" // new_line('a') // &
      "using namespace metal;" // new_line('a') // &
      "" // new_line('a') // &
      "kernel void vector_add(" // new_line('a') // &
      "    device const float* x [[buffer(0)]]," // new_line('a') // &
      "    device const float* y [[buffer(1)]]," // new_line('a') // &
      "    device float* z [[buffer(2)]]," // new_line('a') // &
      "    uint idx [[thread_position_in_grid]]," // new_line('a') // &
      "    uint n [[threads_per_grid]])" // new_line('a') // &
      "{" // new_line('a') // &
      "    if (idx < n) {" // new_line('a') // &
      "        z[idx] = x[idx] + y[idx];" // new_line('a') // &
      "    }" // new_line('a') // &
      "}"
    
  end function get_metal_vector_add
  
  ! SAXPY: y = alpha*x + y  
  function get_metal_saxpy() result(source)
    character(len=:), allocatable :: source
    
    source = &
      "#include <metal_stdlib>" // new_line('a') // &
      "using namespace metal;" // new_line('a') // &
      "" // new_line('a') // &
      "struct SaxpyParams {" // new_line('a') // &
      "    float alpha;" // new_line('a') // &
      "    uint n;" // new_line('a') // &
      "};" // new_line('a') // &
      "" // new_line('a') // &
      "kernel void saxpy(" // new_line('a') // &
      "    device const float* x [[buffer(0)]]," // new_line('a') // &
      "    device float* y [[buffer(1)]]," // new_line('a') // &
      "    constant SaxpyParams& params [[buffer(2)]]," // new_line('a') // &
      "    uint idx [[thread_position_in_grid]])" // new_line('a') // &
      "{" // new_line('a') // &
      "    if (idx < params.n) {" // new_line('a') // &
      "        y[idx] = params.alpha * x[idx] + y[idx];" // new_line('a') // &
      "    }" // new_line('a') // &
      "}"
    
  end function get_metal_saxpy
  
  ! Tiled GEMM: C = A * B
  function get_metal_gemm() result(source)
    character(len=:), allocatable :: source
    
    source = &
      "#include <metal_stdlib>" // new_line('a') // &
      "using namespace metal;" // new_line('a') // &
      "" // new_line('a') // &
      "constant uint TILE_SIZE = 16;" // new_line('a') // &
      "" // new_line('a') // &
      "kernel void gemm(" // new_line('a') // &
      "    device const float* A [[buffer(0)]]," // new_line('a') // &
      "    device const float* B [[buffer(1)]]," // new_line('a') // &
      "    device float* C [[buffer(2)]]," // new_line('a') // &
      "    constant uint& M [[buffer(3)]]," // new_line('a') // &
      "    constant uint& N [[buffer(4)]]," // new_line('a') // &
      "    constant uint& K [[buffer(5)]]," // new_line('a') // &
      "    threadgroup float* tileA [[threadgroup(0)]]," // new_line('a') // &
      "    threadgroup float* tileB [[threadgroup(1)]]," // new_line('a') // &
      "    uint2 gid [[thread_position_in_grid]]," // new_line('a') // &
      "    uint2 tid [[thread_position_in_threadgroup]]," // new_line('a') // &
      "    uint2 tgid [[threadgroup_position_in_grid]])" // new_line('a') // &
      "{" // new_line('a') // &
      "    uint row = gid.y;" // new_line('a') // &
      "    uint col = gid.x;" // new_line('a') // &
      "    uint localRow = tid.y;" // new_line('a') // &
      "    uint localCol = tid.x;" // new_line('a') // &
      "    " // new_line('a') // &
      "    float sum = 0.0;" // new_line('a') // &
      "    " // new_line('a') // &
      "    // Process tiles" // new_line('a') // &
      "    for (uint t = 0; t < K; t += TILE_SIZE) {" // new_line('a') // &
      "        // Load tile from A" // new_line('a') // &
      "        uint aRow = tgid.y * TILE_SIZE + localRow;" // new_line('a') // &
      "        uint aCol = t + localCol;" // new_line('a') // &
      "        if (aRow < M && aCol < K) {" // new_line('a') // &
      "            tileA[localRow * TILE_SIZE + localCol] = A[aRow * K + aCol];" // new_line('a') // &
      "        } else {" // new_line('a') // &
      "            tileA[localRow * TILE_SIZE + localCol] = 0.0;" // new_line('a') // &
      "        }" // new_line('a') // &
      "        " // new_line('a') // &
      "        // Load tile from B" // new_line('a') // &
      "        uint bRow = t + localRow;" // new_line('a') // &
      "        uint bCol = tgid.x * TILE_SIZE + localCol;" // new_line('a') // &
      "        if (bRow < K && bCol < N) {" // new_line('a') // &
      "            tileB[localRow * TILE_SIZE + localCol] = B[bRow * N + bCol];" // new_line('a') // &
      "        } else {" // new_line('a') // &
      "            tileB[localRow * TILE_SIZE + localCol] = 0.0;" // new_line('a') // &
      "        }" // new_line('a') // &
      "        " // new_line('a') // &
      "        // Synchronize" // new_line('a') // &
      "        threadgroup_barrier(mem_flags::mem_threadgroup);" // new_line('a') // &
      "        " // new_line('a') // &
      "        // Compute partial dot product" // new_line('a') // &
      "        for (uint k = 0; k < TILE_SIZE; k++) {" // new_line('a') // &
      "            sum += tileA[localRow * TILE_SIZE + k] * " // new_line('a') // &
      "                   tileB[k * TILE_SIZE + localCol];" // new_line('a') // &
      "        }" // new_line('a') // &
      "        " // new_line('a') // &
      "        // Synchronize before next tile" // new_line('a') // &
      "        threadgroup_barrier(mem_flags::mem_threadgroup);" // new_line('a') // &
      "    }" // new_line('a') // &
      "    " // new_line('a') // &
      "    // Write result" // new_line('a') // &
      "    if (row < M && col < N) {" // new_line('a') // &
      "        C[row * N + col] = sum;" // new_line('a') // &
      "    }" // new_line('a') // &
      "}"
    
  end function get_metal_gemm
  
  ! Parallel reduction (sum)
  function get_metal_reduction() result(source)
    character(len=:), allocatable :: source
    
    source = &
      "#include <metal_stdlib>" // new_line('a') // &
      "using namespace metal;" // new_line('a') // &
      "" // new_line('a') // &
      "kernel void reduction(" // new_line('a') // &
      "    device float* data [[buffer(0)]]," // new_line('a') // &
      "    constant uint& n [[buffer(1)]]," // new_line('a') // &
      "    threadgroup float* sdata [[threadgroup(0)]]," // new_line('a') // &
      "    uint tid [[thread_position_in_threadgroup]]," // new_line('a') // &
      "    uint gid [[thread_position_in_grid]]," // new_line('a') // &
      "    uint blockIdx [[threadgroup_position_in_grid]]," // new_line('a') // &
      "    uint blockDim [[threads_per_threadgroup]])" // new_line('a') // &
      "{" // new_line('a') // &
      "    // Load data to shared memory" // new_line('a') // &
      "    sdata[tid] = (gid < n) ? data[gid] : 0.0;" // new_line('a') // &
      "    threadgroup_barrier(mem_flags::mem_threadgroup);" // new_line('a') // &
      "    " // new_line('a') // &
      "    // Tree reduction in shared memory" // new_line('a') // &
      "    for (uint s = blockDim / 2; s > 0; s >>= 1) {" // new_line('a') // &
      "        if (tid < s) {" // new_line('a') // &
      "            sdata[tid] += sdata[tid + s];" // new_line('a') // &
      "        }" // new_line('a') // &
      "        threadgroup_barrier(mem_flags::mem_threadgroup);" // new_line('a') // &
      "    }" // new_line('a') // &
      "    " // new_line('a') // &
      "    // Write result" // new_line('a') // &
      "    if (tid == 0) {" // new_line('a') // &
      "        data[blockIdx] = sdata[0];" // new_line('a') // &
      "    }" // new_line('a') // &
      "}"
    
  end function get_metal_reduction
  
  ! Complex computation: z = sqrt(x^2 + y^2)
  function get_metal_complex() result(source)
    character(len=:), allocatable :: source
    
    source = &
      "#include <metal_stdlib>" // new_line('a') // &
      "using namespace metal;" // new_line('a') // &
      "" // new_line('a') // &
      "kernel void complex_compute(" // new_line('a') // &
      "    device const float* x [[buffer(0)]]," // new_line('a') // &
      "    device const float* y [[buffer(1)]]," // new_line('a') // &
      "    device float* z [[buffer(2)]]," // new_line('a') // &
      "    uint idx [[thread_position_in_grid]]," // new_line('a') // &
      "    uint n [[threads_per_grid]])" // new_line('a') // &
      "{" // new_line('a') // &
      "    if (idx < n) {" // new_line('a') // &
      "        float x_val = x[idx];" // new_line('a') // &
      "        float y_val = y[idx];" // new_line('a') // &
      "        z[idx] = sqrt(x_val * x_val + y_val * y_val);" // new_line('a') // &
      "    }" // new_line('a') // &
      "}"
    
  end function get_metal_complex

end module sporkle_metal_kernels