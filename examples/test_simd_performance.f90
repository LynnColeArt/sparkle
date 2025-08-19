! Test SIMD Performance
! =====================
! Compare original vs SIMD-optimized GEMM

program test_simd_performance
  use kinds
  use omp_lib
  use universal_memory_optimization, only: gemm_universal_memory, im2col_cache_optimal
  use gemm_simd_optimized, only: gemm_simd_avx512
  implicit none
  
  real(sp), allocatable :: input(:), weights(:), output(:), output2(:), col_buffer(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  integer :: i, run
  integer :: clock_start, clock_end, clock_rate
  integer(i64) :: total_flops
  real(sp) :: time_ms, time_original, time_simd
  real(sp) :: gflops_original, gflops_simd
  real(sp) :: max_diff
  integer, parameter :: num_warmup = 3
  integer, parameter :: num_runs = 5
  
  print *, "🚀 SIMD Performance Comparison"
  print *, "=============================="
  print *, ""
  
  ! ResNet-50 first layer
  N = 1
  C = 3  
  H = 224
  W = 224
  K = 64
  kernel_size = 7
  stride = 2
  pad = 3
  H_out = 112
  W_out = 112
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(K * H_out * W_out))
  allocate(output2(K * H_out * W_out))
  allocate(col_buffer(C * kernel_size * kernel_size * H_out * W_out))
  
  ! Initialize test data
  do i = 1, size(input)
    input(i) = real(mod(i-1, 256)) / 256.0
  end do
  do i = 1, size(weights)
    weights(i) = real(mod(i-1, 128)) / 128.0 - 0.5
  end do
  
  ! Calculate total FLOPs
  total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Input: ", N, "×", C, "×", H, "×", W
  print '(A,I0,A,I0,A,I0,A,I0)', "Output: ", N, "×", K, "×", H_out, "×", W_out
  print '(A,F0.1,A)', "Total GFLOPS: ", real(total_flops) / 1.0e9
  
  !$OMP PARALLEL
  !$OMP SINGLE
  print '(A,I0)', "OpenMP threads: ", omp_get_num_threads()
  !$OMP END SINGLE
  !$OMP END PARALLEL
  
  ! Check CPU features
  print *, ""
  print *, "🔍 CPU Features Detection"
  print *, "========================"
  call system("lscpu | grep -E 'Model name|avx512' | head -3")
  print *, ""
  
  ! Prepare data
  call im2col_cache_optimal(input, col_buffer, N, C, H, W, &
                           kernel_size, stride, pad, H_out, W_out)
  
  ! Warmup
  print *, "🔥 Warming up..."
  do run = 1, num_warmup
    call gemm_universal_memory(weights, col_buffer, output, &
                              K, H_out * W_out, C * kernel_size * kernel_size, &
                              1.0, 0.0)
    call gemm_simd_avx512(weights, col_buffer, output2, &
                         K, H_out * W_out, C * kernel_size * kernel_size, &
                         1.0, 0.0)
  end do
  
  ! Test original implementation
  print *, ""
  print *, "📊 Original Implementation"
  print *, "========================="
  
  time_original = 0.0
  do run = 1, num_runs
    output = 0.0
    
    call system_clock(clock_start, clock_rate)
    
    call gemm_universal_memory(weights, col_buffer, output, &
                              K, H_out * W_out, C * kernel_size * kernel_size, &
                              1.0, 0.0)
    
    call system_clock(clock_end)
    
    time_ms = real(clock_end - clock_start, real32) * 1000.0 / real(clock_rate, real32)
    time_original = time_original + time_ms
    
    print '(A,I2,A,F8.2,A,F8.1,A)', "Run ", run, ": ", time_ms, " ms, ", &
           real(total_flops) / (time_ms * 1.0e6), " GFLOPS"
  end do
  
  time_original = time_original / real(num_runs)
  gflops_original = real(total_flops) / (time_original * 1.0e6)
  
  ! Test SIMD-optimized implementation
  print *, ""
  print *, "⚡ SIMD-Optimized Implementation"
  print *, "================================"
  
  time_simd = 0.0
  do run = 1, num_runs
    output2 = 0.0
    
    call system_clock(clock_start, clock_rate)
    
    call gemm_simd_avx512(weights, col_buffer, output2, &
                         K, H_out * W_out, C * kernel_size * kernel_size, &
                         1.0, 0.0)
    
    call system_clock(clock_end)
    
    time_ms = real(clock_end - clock_start, real32) * 1000.0 / real(clock_rate, real32)
    time_simd = time_simd + time_ms
    
    print '(A,I2,A,F8.2,A,F8.1,A)', "Run ", run, ": ", time_ms, " ms, ", &
           real(total_flops) / (time_ms * 1.0e6), " GFLOPS"
  end do
  
  time_simd = time_simd / real(num_runs)
  gflops_simd = real(total_flops) / (time_simd * 1.0e6)
  
  ! Summary
  print *, ""
  print *, "📈 Summary"
  print *, "========="
  print '(A,F8.2,A,F8.1,A)', "Original:      ", time_original, " ms, ", gflops_original, " GFLOPS"
  print '(A,F8.2,A,F8.1,A)', "SIMD-Optimized: ", time_simd, " ms, ", gflops_simd, " GFLOPS"
  print '(A,F5.2,A)', "Speedup: ", time_original / time_simd, "x"
  print '(A,F5.1,A)', "Performance gain: ", (gflops_simd - gflops_original) / gflops_original * 100.0, "%"
  
  ! Verify correctness
  print *, ""
  print *, "✅ Correctness Check"
  print *, "==================="
  
  max_diff = 0.0
  do i = 1, size(output)
    max_diff = max(max_diff, abs(output(i) - output2(i)))
  end do
  
  print '(A,E12.4)', "Maximum difference: ", max_diff
  if (max_diff < 1.0e-4) then
    print *, "✅ Results match!"
  else
    print *, "❌ Results differ!"
  end if
  
  if (gflops_simd > 40.0) then
    print *, ""
    print *, "🎉 ACHIEVED 40+ GFLOPS WITH SIMD!"
  end if
  
  ! Cleanup
  deallocate(input, weights, output, output2, col_buffer)
  
end program test_simd_performance