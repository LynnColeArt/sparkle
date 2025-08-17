! MERCILESS SIMD Verification Test
! =================================
!
! This test exposes the TRUTH about our SIMD optimization:
! 1. Does gemm_simd_avx512 ACTUALLY outperform gemm_universal_memory?
! 2. Is the SIMD path ACTUALLY being used in production?
! 3. What's the REAL overhead of im2col?
! 4. Are there any bugs or performance regressions?

program test_simd_verification
  use iso_fortran_env, only: real32, real64, int64
  use universal_memory_optimization, only: gemm_universal_memory
  use gemm_simd_optimized, only: gemm_simd_avx512
  use universal_memory_optimization, only: im2col_cache_optimal
  use cpu_conv2d_simd, only: conv2d_cpu_simd
  use sparkle_conv2d, only: conv2d_cpu
  implicit none

  ! Test matrix dimensions
  integer, parameter :: M = 512   ! Output channels
  integer, parameter :: N = 3136  ! Spatial dimensions (56x56)
  integer, parameter :: K = 2304  ! Input channels * kernel size (256*3*3)
  
  ! Convolution parameters
  integer, parameter :: batch = 1
  integer, parameter :: in_channels = 256
  integer, parameter :: out_channels = 512
  integer, parameter :: height = 56
  integer, parameter :: width = 56
  integer, parameter :: kernel_size = 3
  integer, parameter :: stride = 1
  integer, parameter :: pad = 1
  integer, parameter :: h_out = 56
  integer, parameter :: w_out = 56
  
  ! Test arrays
  real(real32), allocatable :: A(:), B(:), C_baseline(:), C_simd(:), C_conv(:)
  real(real32), allocatable :: input(:), weights(:), output(:)
  
  ! Timing
  real(real64) :: start_time, end_time
  real(real64) :: time_baseline, time_simd, time_conv
  integer(int64) :: gemm_flops, conv_flops
  real(real64) :: gflops_baseline, gflops_simd, gflops_conv
  real(real64) :: speedup_simd, overhead_im2col
  
  integer :: i, iterations
  real(real32) :: max_diff
  
  print *, "=================================================="
  print *, "     MERCILESS SIMD VERIFICATION TEST"
  print *, "=================================================="
  print *, ""
  
  ! Allocate matrices
  allocate(A(M * K))
  allocate(B(K * N))
  allocate(C_baseline(M * N))
  allocate(C_simd(M * N))
  
  ! Allocate convolution arrays
  allocate(input(batch * in_channels * height * width))
  allocate(weights(out_channels * in_channels * kernel_size * kernel_size))
  allocate(output(batch * out_channels * h_out * w_out))
  allocate(C_conv(M * N))
  
  ! Initialize with random data
  call random_number(A)
  call random_number(B)
  call random_number(input)
  call random_number(weights)
  
  A = A - 0.5
  B = B - 0.5
  input = input - 0.5
  weights = weights - 0.5
  
  ! FLOP calculations
  gemm_flops = int(M, int64) * int(N, int64) * int(K, int64) * 2_int64
  conv_flops = int(batch, int64) * int(out_channels, int64) * int(h_out, int64) * int(w_out, int64) * &
               int(in_channels, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
  
  print '(A,I0)', "Matrix dimensions: M=", M, ", N=", N, ", K=", K
  print '(A,I0)', "GEMM FLOPs: ", gemm_flops
  print '(A,I0)', "Conv FLOPs: ", conv_flops
  print *, ""
  
  iterations = 10
  
  ! ==============================================
  ! TEST 1: Baseline GEMM (gemm_universal_memory)
  ! ==============================================
  print *, "TEST 1: Baseline GEMM (gemm_universal_memory)"
  print *, "---------------------------------------------"
  
  ! Warmup
  do i = 1, 3
    C_baseline = 0.0
    call gemm_universal_memory(A, B, C_baseline, M, N, K, 1.0, 0.0)
  end do
  
  ! Benchmark
  time_baseline = 0.0
  do i = 1, iterations
    C_baseline = 0.0
    call cpu_time(start_time)
    call gemm_universal_memory(A, B, C_baseline, M, N, K, 1.0, 0.0)
    call cpu_time(end_time)
    time_baseline = time_baseline + (end_time - start_time)
  end do
  time_baseline = time_baseline / iterations
  
  gflops_baseline = real(gemm_flops, real64) / (time_baseline * 1.0e9_real64)
  print '(A,F8.3,A,F8.1,A)', "Average time: ", time_baseline * 1000.0, " ms, ", gflops_baseline, " GFLOPS"
  
  ! ==============================================
  ! TEST 2: SIMD GEMM (gemm_simd_avx512)
  ! ==============================================
  print *, ""
  print *, "TEST 2: SIMD GEMM (gemm_simd_avx512)"
  print *, "------------------------------------"
  
  ! Warmup
  do i = 1, 3
    C_simd = 0.0
    call gemm_simd_avx512(A, B, C_simd, M, N, K, 1.0, 0.0)
  end do
  
  ! Benchmark
  time_simd = 0.0
  do i = 1, iterations
    C_simd = 0.0
    call cpu_time(start_time)
    call gemm_simd_avx512(A, B, C_simd, M, N, K, 1.0, 0.0)
    call cpu_time(end_time)
    time_simd = time_simd + (end_time - start_time)
  end do
  time_simd = time_simd / iterations
  
  gflops_simd = real(gemm_flops, real64) / (time_simd * 1.0e9_real64)
  speedup_simd = time_baseline / time_simd
  
  print '(A,F8.3,A,F8.1,A)', "Average time: ", time_simd * 1000.0, " ms, ", gflops_simd, " GFLOPS"
  print '(A,F6.2,A)', "Speedup over baseline: ", speedup_simd, "x"
  
  ! Verify correctness
  max_diff = maxval(abs(C_simd - C_baseline))
  print '(A,E12.5)', "Max difference from baseline: ", max_diff
  if (max_diff > 1.0e-3) then
    print *, "⚠️  WARNING: Large difference detected!"
  else
    print *, "✅ Results match baseline"
  end if
  
  ! ==============================================
  ! TEST 3: Full convolution (conv2d_cpu_simd)
  ! ==============================================
  print *, ""
  print *, "TEST 3: Full convolution (conv2d_cpu_simd)"
  print *, "------------------------------------------"
  
  ! Warmup
  do i = 1, 3
    output = 0.0
    time_conv = conv2d_cpu_simd(input, weights, output, &
                               batch, in_channels, height, width, out_channels, &
                               kernel_size, stride, pad, h_out, w_out)
  end do
  
  ! Benchmark
  time_conv = 0.0
  do i = 1, iterations
    output = 0.0
    time_conv = time_conv + conv2d_cpu_simd(input, weights, output, &
                                           batch, in_channels, height, width, out_channels, &
                                           kernel_size, stride, pad, h_out, w_out)
  end do
  time_conv = time_conv / iterations  ! Already in ms
  
  gflops_conv = real(conv_flops, real64) / (time_conv * 1.0e6_real64)
  overhead_im2col = (time_conv / 1000.0 - time_simd) / time_simd * 100.0
  
  print '(A,F8.3,A,F8.1,A)', "Average time: ", time_conv, " ms, ", gflops_conv, " GFLOPS"
  print '(A,F6.1,A)', "im2col overhead: ", overhead_im2col, "%"
  
  ! Copy output to C_conv for comparison
  C_conv = output(1:M*N)
  
  ! ==============================================
  ! TEST 4: Production interface (sparkle_conv2d)
  ! ==============================================
  print *, ""
  print *, "TEST 4: Production interface (sparkle_conv2d)"
  print *, "---------------------------------------------"
  
  ! This should print its own performance info
  call conv2d_cpu(input, weights, output, &
                  batch, in_channels, height, width, out_channels, &
                  kernel_size, stride, pad, h_out, w_out)
  
  ! ==============================================
  ! PERFORMANCE SUMMARY
  ! ==============================================
  print *, ""
  print *, "=================================================="
  print *, "              PERFORMANCE SUMMARY"
  print *, "=================================================="
  print '(A,F8.1,A)', "Baseline GEMM:      ", gflops_baseline, " GFLOPS"
  print '(A,F8.1,A,F5.2,A)', "SIMD GEMM:          ", gflops_simd, " GFLOPS (", speedup_simd, "x speedup)"
  print '(A,F8.1,A)', "Full convolution:   ", gflops_conv, " GFLOPS"
  print *, ""
  
  ! MERCILESS ASSESSMENT
  if (gflops_simd < gflops_baseline * 1.5) then
    print *, "🔴 FAILURE: SIMD is NOT providing expected speedup!"
    print *, "   Expected at least 1.5x improvement over baseline"
  else if (gflops_simd < 150.0) then
    print *, "🟡 WARNING: SIMD performance below 150 GFLOPS target"
  else
    print *, "✅ SUCCESS: SIMD optimization is working!"
  end if
  
  if (gflops_conv < gflops_simd * 0.8) then
    print *, "🔴 FAILURE: im2col overhead is too high (>20%)!"
  else
    print *, "✅ SUCCESS: im2col overhead is acceptable"
  end if
  
  ! Note: Production interface reports its own performance, 
  ! we can't easily capture it for comparison here
  
  ! Cleanup
  deallocate(A, B, C_baseline, C_simd, C_conv)
  deallocate(input, weights, output)

end program test_simd_verification