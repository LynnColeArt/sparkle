program test_zero_copy_large
  ! Zero-Copy Large Workload Test
  ! =============================
  !
  ! Tests with larger workloads to enable parallelism
  
  use kinds
  use iso_c_binding
  use gpu_opengl_zero_copy
  implicit none
  
  ! Larger test parameters for more GPU work
  integer, parameter :: N = 32       ! Larger batch size
  integer, parameter :: C = 128      ! More input channels  
  integer, parameter :: H = 112      ! Larger height
  integer, parameter :: W = 112      ! Larger width
  integer, parameter :: K = 256      ! More output channels
  integer, parameter :: kernel_size = 3
  integer, parameter :: stride = 1
  integer, parameter :: pad = 1
  integer, parameter :: H_out = H    ! Same due to padding
  integer, parameter :: W_out = W
  
  ! Parallelism parameters
  integer, parameter :: num_iterations = 50   ! Fewer iterations needed
  integer, parameter :: warmup_iters = 5      ! Warmup iterations
  
  ! Data arrays
  real(sp), allocatable :: input(:), weights(:), output(:)
  
  ! Timing
  integer(i64) :: start_time, end_time, clock_rate
  real(sp) :: total_time_ms, single_kernel_ms
  real(sp) :: time_per_kernel, aggregate_throughput
  integer :: i
  
  ! Performance stats
  integer(i64) :: total_flops_per_kernel
  real(sp) :: gflops_single
  
  print *, "🚀 Zero-Copy Large Workload Test"
  print *, "================================"
  print *, ""
  
  ! Calculate workload size
  total_flops_per_kernel = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                          int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
  
  print '(A,I0,A)', "Workload per kernel: ", total_flops_per_kernel / 1000000, " MFLOPS"
  print '(A,I0,A)', "Total workload: ", (total_flops_per_kernel * num_iterations) / 1000000, " MFLOPS"
  print '(A,I0,A,I0,A,I0,A,I0)', "Input: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0,A,I0)', "Conv: ", K, " filters, ", kernel_size, "x", kernel_size
  
  ! Calculate memory requirements
  block
    integer(i64) :: input_size, weight_size, output_size, total_size
    input_size = int(N * C * H * W * 4, int64)
    weight_size = int(K * C * kernel_size * kernel_size * 4, int64)  
    output_size = int(N * K * H_out * W_out * 4, int64)
    total_size = input_size + weight_size + output_size
    
    print '(A,F8.2,A)', "Memory footprint: ", real(total_size) / (1024.0**3), " GB"
  end block
  
  print *, ""
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  
  ! Initialize with test data
  call random_number(input)
  call random_number(weights)
  input = input - 0.5
  weights = weights - 0.5
  
  ! Initialize zero-copy GPU
  print *, "Initializing zero-copy GPU..."
  if (.not. gpu_init_zero_copy()) then
    print *, "❌ Failed to initialize zero-copy GPU"
    stop 1
  end if
  
  print *, ""
  print *, "Running warmup..."
  
  ! Warmup
  do i = 1, warmup_iters
    single_kernel_ms = gpu_execute_conv2d_zero_copy(input, weights, output, &
                                                   N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    if (single_kernel_ms < 0) then
      print *, "❌ Warmup failed!"
      stop 1
    end if
    print '(A,I0,A,F8.3,A)', "Warmup ", i, ": ", single_kernel_ms, " ms"
  end do
  
  ! Calculate single kernel performance
  gflops_single = real(total_flops_per_kernel) / (single_kernel_ms * 1.0e6)
  
  print *, ""
  print '(A,F8.1,A)', "Single kernel performance: ", gflops_single, " GFLOPS"
  print *, ""
  print *, "Measuring parallel throughput..."
  
  ! Measure aggregate throughput
  call system_clock(start_time, count_rate=clock_rate)
  
  do i = 1, num_iterations
    single_kernel_ms = gpu_execute_conv2d_zero_copy(input, weights, output, &
                                                   N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    if (single_kernel_ms < 0) then
      print *, "❌ Execution failed at iteration ", i
      stop 1
    end if
  end do
  
  call system_clock(end_time)
  
  ! Calculate aggregate performance
  total_time_ms = real(end_time - start_time) / real(clock_rate) * 1000.0
  time_per_kernel = total_time_ms / num_iterations
  aggregate_throughput = real(total_flops_per_kernel * num_iterations) / (total_time_ms * 1.0e6)
  
  print *, ""
  print *, "📊 Performance Results:"
  print *, "====================="
  print '(A,I0,A,F10.3,A)', "Total time for ", num_iterations, " iterations: ", total_time_ms, " ms"
  print '(A,F8.3,A)', "Average time per kernel: ", time_per_kernel, " ms"
  print *, ""
  print '(A,F8.1,A)', "Single kernel performance:    ", gflops_single, " GFLOPS"
  print '(A,F8.1,A)', "Aggregate throughput:         ", aggregate_throughput, " GFLOPS"
  print '(A,F8.2,A)', "Parallelism efficiency:       ", aggregate_throughput / gflops_single, "x"
  
  ! Analysis
  print *, ""
  print *, "💡 Parallelism Analysis:"
  print *, "======================="
  
  if (aggregate_throughput / gflops_single > 1.2) then
    print *, "✅ Good parallelism achieved!"
    print '(A,F5.2,A)', "   ", aggregate_throughput / gflops_single, "x speedup from overlapped execution"
    print *, "   Multiple kernels executing concurrently on GPU"
  else
    print *, "⚠️  Limited parallelism"
    print '(A,F8.3,A)', "   Kernel execution time (", time_per_kernel, " ms) is too short for overlap"
    print *, "   CPU dispatch overhead dominates"
  end if
  
  ! Memory bandwidth analysis
  block
    real(sp) :: memory_gb, bandwidth_gb
    memory_gb = real(N * C * H * W * 4 +                      & ! Input
                     K * C * kernel_size * kernel_size * 4 +  & ! Weights  
                     N * K * H_out * W_out * 4) / (1024.0**3)
    
    bandwidth_gb = memory_gb / (time_per_kernel / 1000.0)
    
    print *, ""
    print '(A,F8.2,A)', "Memory bandwidth achieved: ", bandwidth_gb, " GB/s"
    print '(A,F8.1,A)', "FLOPS/byte ratio: ", real(total_flops_per_kernel) / (memory_gb * 1024.0**3), " FLOPS/byte"
  end block
  
  ! Cleanup
  call gpu_cleanup_zero_copy()
  deallocate(input, weights, output)
  
  print *, ""
  print *, "🏁 Test complete!"
  
end program test_zero_copy_large