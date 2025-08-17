program test_cpu_performance_reality
  use iso_fortran_env
  use sparkle_conv2d_v2
  use gemm_simd_optimized, only: gemm_simd_avx512
  implicit none
  
  real(real32), allocatable :: A(:), B(:), C(:)
  real(real64) :: start_time, end_time, gflops
  integer :: m, n, k, i
  integer(int64) :: flops
  
  print *, "=== CPU PERFORMANCE REALITY CHECK ==="
  print *, ""
  
  ! Test 1: Small GEMM that fits in L1 cache
  print *, "Test 1: Small GEMM (fits in L1 cache)"
  m = 64; n = 64; k = 64
  allocate(A(m*k), B(k*n), C(m*n))
  A = 1.0; B = 1.0; C = 0.0
  
  call cpu_time(start_time)
  do i = 1, 1000
    call gemm_simd_avx512(A, B, C, m, n, k, 1.0, 0.0)
  end do
  call cpu_time(end_time)
  
  flops = int(2, int64) * int(m, int64) * int(n, int64) * int(k, int64) * 1000_int64
  gflops = real(flops) / ((end_time - start_time) * 1.0e9)
  print '(A,I0,A,I0,A,I0,A,F8.1,A)', "  Size: ", m, "x", n, "x", k, " -> ", gflops, " GFLOPS"
  deallocate(A, B, C)
  
  ! Test 2: Medium GEMM that fits in L3 cache
  print *, ""
  print *, "Test 2: Medium GEMM (fits in L3 cache)"
  m = 512; n = 512; k = 512
  allocate(A(m*k), B(k*n), C(m*n))
  A = 1.0; B = 1.0; C = 0.0
  
  call cpu_time(start_time)
  do i = 1, 10
    call gemm_simd_avx512(A, B, C, m, n, k, 1.0, 0.0)
  end do
  call cpu_time(end_time)
  
  flops = int(2, int64) * int(m, int64) * int(n, int64) * int(k, int64) * 10_int64
  gflops = real(flops) / ((end_time - start_time) * 1.0e9)
  print '(A,I0,A,I0,A,I0,A,F8.1,A)', "  Size: ", m, "x", n, "x", k, " -> ", gflops, " GFLOPS"
  deallocate(A, B, C)
  
  ! Test 3: Large GEMM that spills to RAM
  print *, ""
  print *, "Test 3: Large GEMM (memory-bound)"
  m = 2048; n = 2048; k = 2048
  allocate(A(m*k), B(k*n), C(m*n))
  A = 1.0; B = 1.0; C = 0.0
  
  call cpu_time(start_time)
  call gemm_simd_avx512(A, B, C, m, n, k, 1.0, 0.0)
  call cpu_time(end_time)
  
  flops = int(2, int64) * int(m, int64) * int(n, int64) * int(k, int64)
  gflops = real(flops) / ((end_time - start_time) * 1.0e9)
  print '(A,I0,A,I0,A,I0,A,F8.1,A)', "  Size: ", m, "x", n, "x", k, " -> ", gflops, " GFLOPS"
  deallocate(A, B, C)
  
  ! Test 4: The actual convolution workload size
  print *, ""
  print *, "Test 4: Convolution-like workload (im2col size)"
  m = 576; n = 3136; k = 64  ! Typical ResNet layer after im2col
  allocate(A(m*k), B(k*n), C(m*n))
  A = 1.0; B = 1.0; C = 0.0
  
  call cpu_time(start_time)
  call gemm_simd_avx512(A, B, C, m, n, k, 1.0, 0.0)
  call cpu_time(end_time)
  
  flops = int(2, int64) * int(m, int64) * int(n, int64) * int(k, int64)
  gflops = real(flops) / ((end_time - start_time) * 1.0e9)
  print '(A,I0,A,I0,A,I0,A,F8.1,A)', "  Size: ", m, "x", n, "x", k, " -> ", gflops, " GFLOPS"
  
  print *, ""
  print *, "🧢 QA Beanie Analysis:"
  print *, "- L1-resident: High GFLOPS (compute-bound)"
  print *, "- L3-resident: Medium GFLOPS (cache-bound)"  
  print *, "- Memory-bound: Low GFLOPS (bandwidth-bound)"
  print *, "- Convolution workloads are typically memory-bound!"
  
  deallocate(A, B, C)
  
end program test_cpu_performance_reality