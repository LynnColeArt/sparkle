! Test Peak CPU Performance (Convolution Workload)
! ================================================
! NOTE: This tests convolution performance (~30 GFLOPS), not peak GEMM.
! For peak CPU performance (196+ GFLOPS), use test_simd_performance.f90
! Focus on measuring peak GFLOPS using the proven gemm_universal_memory

program test_peak_cpu_performance
  use kinds
  use omp_lib
  use universal_memory_optimization, only: gemm_universal_memory, im2col_cache_optimal
  implicit none
  
  real(sp), allocatable :: input(:), weights(:), output(:), col_buffer(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  integer :: i, run
  integer :: clock_start, clock_end, clock_rate
  integer(i64) :: total_flops
  real(sp) :: time_ms, best_time, worst_time, avg_time, gflops
  integer, parameter :: num_warmup = 5
  integer, parameter :: num_runs = 10
  
  print *, "🚀 Peak CPU Performance Test"
  print *, "============================"
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
  print *, ""
  
  ! Warmup runs
  print *, "🔥 Warming up..."
  do run = 1, num_warmup
    call im2col_cache_optimal(input, col_buffer, N, C, H, W, &
                             kernel_size, stride, pad, H_out, W_out)
    call gemm_universal_memory(weights, col_buffer, output, &
                              K, H_out * W_out, C * kernel_size * kernel_size, &
                              1.0, 0.0)
  end do
  
  ! Benchmark runs
  print *, ""
  print *, "📊 Benchmark Results"
  print *, "==================="
  
  best_time = 1.0e9
  worst_time = 0.0
  avg_time = 0.0
  
  do run = 1, num_runs
    output = 0.0
    
    call system_clock(clock_start, clock_rate)
    
    ! Full convolution operation
    call im2col_cache_optimal(input, col_buffer, N, C, H, W, &
                             kernel_size, stride, pad, H_out, W_out)
    call gemm_universal_memory(weights, col_buffer, output, &
                              K, H_out * W_out, C * kernel_size * kernel_size, &
                              1.0, 0.0)
    
    call system_clock(clock_end)
    
    time_ms = real(clock_end - clock_start, real32) * 1000.0 / real(clock_rate, real32)
    gflops = real(total_flops) / (time_ms * 1.0e6)
    
    print '(A,I2,A,F8.2,A,F8.1,A)', "Run ", run, ": ", time_ms, " ms, ", gflops, " GFLOPS"
    
    best_time = min(best_time, time_ms)
    worst_time = max(worst_time, time_ms)
    avg_time = avg_time + time_ms
  end do
  
  avg_time = avg_time / real(num_runs)
  
  print *, ""
  print *, "📈 Summary"
  print *, "========="
  print '(A,F8.2,A,F8.1,A)', "Best:    ", best_time, " ms, ", &
                             real(total_flops) / (best_time * 1.0e6), " GFLOPS"
  print '(A,F8.2,A,F8.1,A)', "Average: ", avg_time, " ms, ", &
                             real(total_flops) / (avg_time * 1.0e6), " GFLOPS"
  print '(A,F8.2,A,F8.1,A)', "Worst:   ", worst_time, " ms, ", &
                             real(total_flops) / (worst_time * 1.0e6), " GFLOPS"
  
  if (real(total_flops) / (best_time * 1.0e6) > 40.0) then
    print *, ""
    print *, "🎉 ACHIEVED 40+ GFLOPS!"
  end if
  
  ! Cleanup
  deallocate(input, weights, output, col_buffer)
  
end program test_peak_cpu_performance