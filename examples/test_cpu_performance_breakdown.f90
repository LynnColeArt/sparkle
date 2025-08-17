program test_cpu_performance_breakdown
  use iso_fortran_env, only: real32, real64, int64
  use universal_memory_optimization, only: im2col_cache_optimal
  use gemm_simd_optimized, only: gemm_simd_avx512
  use omp_lib
  implicit none
  
  ! Test parameters - same as our benchmark
  integer, parameter :: N = 1, C = 64, H = 56, W = 56
  integer, parameter :: K = 256, kernel_size = 3, stride = 1, pad = 1
  integer, parameter :: H_out = 56, W_out = 56
  
  real(real32), allocatable :: input(:), weights(:), output(:), col_buffer(:)
  integer :: input_size, weight_size, output_size, col_size
  integer :: im2col_rows, im2col_cols
  
  integer :: clock_start, clock_end, clock_rate
  real(real64) :: im2col_time, gemm_time, total_time
  integer(int64) :: total_flops
  real(real64) :: gflops
  integer :: i
  
  print *, "🔍 CPU Performance Breakdown Analysis"
  print *, "===================================="
  print *, ""
  
  ! Calculate sizes
  input_size = N * C * H * W
  weight_size = K * C * kernel_size * kernel_size
  output_size = N * K * H_out * W_out
  im2col_rows = C * kernel_size * kernel_size
  im2col_cols = N * H_out * W_out
  col_size = im2col_rows * im2col_cols
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Input: ", N, "×", C, "×", H, "×", W
  print '(A,I0,A,I0)', "Kernel: ", kernel_size, "×", kernel_size
  print '(A,I0,A,I0,A,I0,A,I0)', "Output: ", N, "×", K, "×", H_out, "×", W_out
  print '(A,I0,A,I0)', "Matrix multiplication: ", K, "×", im2col_cols, " = ", K, "×", H_out*W_out
  print '(A,I0,A,I0)', "im2col buffer: ", im2col_rows, "×", im2col_cols
  print *, ""
  
  ! Allocate arrays
  allocate(input(input_size))
  allocate(weights(weight_size))
  allocate(output(output_size))
  allocate(col_buffer(col_size))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(weights)
  output = 0.0
  
  ! Calculate total FLOPs
  total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
  
  print '(A,F0.1,A)', "Total FLOPs: ", real(total_flops) / 1.0e9, " billion"
  print *, ""
  
  ! Warm up
  print *, "🔥 Warming up..."
  do i = 1, 5
    call im2col_cache_optimal(input, col_buffer, N, C, H, W, &
                             kernel_size, stride, pad, H_out, W_out)
    call gemm_simd_avx512(weights, col_buffer, output, &
                         K, im2col_cols, im2col_rows, 1.0, 0.0)
  end do
  print *, ""
  
  ! Test 1: Time im2col alone
  print *, "⏱️  Test 1: im2col transformation time"
  call system_clock(clock_start, clock_rate)
  
  call im2col_cache_optimal(input, col_buffer, N, C, H, W, &
                           kernel_size, stride, pad, H_out, W_out)
  
  call system_clock(clock_end)
  im2col_time = real(clock_end - clock_start, real64) / real(clock_rate, real64)
  print '(A,F8.2,A)', "   im2col time: ", im2col_time * 1000.0, " ms"
  print *, ""
  
  ! Test 2: Time GEMM alone
  print *, "⏱️  Test 2: SIMD GEMM time"
  output = 0.0
  call system_clock(clock_start, clock_rate)
  
  call gemm_simd_avx512(weights, col_buffer, output, &
                       K, im2col_cols, im2col_rows, 1.0, 0.0)
  
  call system_clock(clock_end)
  gemm_time = real(clock_end - clock_start, real64) / real(clock_rate, real64)
  gflops = real(total_flops, real64) / (gemm_time * 1.0e9)
  print '(A,F8.2,A,F8.1,A)', "   GEMM time: ", gemm_time * 1000.0, " ms, ", gflops, " GFLOPS"
  print *, ""
  
  ! Test 3: Full convolution time
  print *, "⏱️  Test 3: Full convolution (im2col + GEMM)"
  output = 0.0
  call system_clock(clock_start, clock_rate)
  
  call im2col_cache_optimal(input, col_buffer, N, C, H, W, &
                           kernel_size, stride, pad, H_out, W_out)
  call gemm_simd_avx512(weights, col_buffer, output, &
                       K, im2col_cols, im2col_rows, 1.0, 0.0)
  
  call system_clock(clock_end)
  total_time = real(clock_end - clock_start, real64) / real(clock_rate, real64)
  gflops = real(total_flops, real64) / (total_time * 1.0e9)
  print '(A,F8.2,A,F8.1,A)', "   Total time: ", total_time * 1000.0, " ms, ", gflops, " GFLOPS"
  print *, ""
  
  ! Analysis
  print *, "📊 Performance Breakdown:"
  print *, "========================"
  print '(A,F5.1,A)', "   im2col: ", (im2col_time / total_time) * 100.0, "% of total time"
  print '(A,F5.1,A)', "   GEMM:   ", (gemm_time / total_time) * 100.0, "% of total time"
  print *, ""
  
  if (im2col_time > gemm_time) then
    print *, "❌ BOTTLENECK: im2col transformation is dominating performance!"
    print *, "   The 196.7 GFLOPS SIMD optimization only helps the GEMM part."
    print *, "   We need to optimize im2col or use a different approach."
  else
    print *, "✅ GEMM is the bottleneck, SIMD optimization is effective"
  end if
  
  print *, ""
  print *, "💡 Insights:"
  print *, "- GEMM alone achieves high GFLOPS with SIMD"
  print *, "- im2col transformation adds significant overhead"
  print *, "- End-to-end performance includes both operations"
  print *, "- GPU doesn't have this overhead (direct convolution in shader)"
  
  ! Cleanup
  deallocate(input, weights, output, col_buffer)
  
end program test_cpu_performance_breakdown