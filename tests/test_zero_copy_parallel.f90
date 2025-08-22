program test_zero_copy_parallel
  ! Zero-Copy Parallel Throughput Test
  ! ==================================
  !
  ! Measures aggregate throughput with multiple kernels in flight
  
  use kinds
  use iso_c_binding
  use gpu_opengl_zero_copy
  implicit none
  
  ! Test parameters - same as async executor test
  integer, parameter :: N = 8        ! Batch size
  integer, parameter :: C = 64       ! Input channels  
  integer, parameter :: H = 56       ! Height
  integer, parameter :: W = 56       ! Width
  integer, parameter :: K = 128      ! Output channels
  integer, parameter :: kernel_size = 3
  integer, parameter :: stride = 1
  integer, parameter :: pad = 1
  integer, parameter :: H_out = H    ! Same due to padding
  integer, parameter :: W_out = W
  
  ! Parallelism parameters
  integer, parameter :: num_iterations = 100  ! Total iterations
  integer, parameter :: warmup_iters = 10     ! Warmup iterations
  
  ! Data arrays - multiple sets for parallel execution
  real(sp), allocatable :: input(:), weights(:), output(:)
  
  ! Timing
  integer(i64) :: start_time, end_time, clock_rate
  real(sp) :: total_time_ms, single_kernel_ms
  real(sp) :: time_per_kernel, aggregate_throughput
  integer :: i
  
  ! Performance stats
  integer(i64) :: total_flops_per_kernel
  real(sp) :: gflops_single, gflops_aggregate
  
  print *, "🚀 Zero-Copy Parallel Throughput Test"
  print *, "===================================="
  print *, ""
  
  ! Calculate workload size
  total_flops_per_kernel = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                          int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
  
  print '(A,I0,A)', "Workload per kernel: ", total_flops_per_kernel / 1000000, " MFLOPS"
  print '(A,I0,A,I0,A,I0,A,I0)', "Input: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0,A,I0)', "Conv: ", K, " filters, ", kernel_size, "x", kernel_size
  print '(A,I0)', "Total iterations: ", num_iterations
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
  
  ! Warmup to ensure buffers are allocated
  do i = 1, warmup_iters
    single_kernel_ms = gpu_execute_conv2d_zero_copy(input, weights, output, &
                                                   N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    if (single_kernel_ms < 0) then
      print *, "❌ Warmup failed!"
      stop 1
    end if
  end do
  
  print '(A,F8.3,A)', "Warmup complete. Single kernel time: ", single_kernel_ms, " ms"
  
  ! Calculate single kernel performance
  gflops_single = real(total_flops_per_kernel) / (single_kernel_ms * 1.0e6)
  
  print *, ""
  print *, "Measuring parallel throughput..."
  
  ! Measure aggregate throughput with continuous execution
  call system_clock(start_time, count_rate=clock_rate)
  
  ! Execute many kernels back-to-back
  do i = 1, num_iterations
    single_kernel_ms = gpu_execute_conv2d_zero_copy(input, weights, output, &
                                                   N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    if (single_kernel_ms < 0) then
      print *, "❌ Execution failed at iteration ", i
      stop 1
    end if
    
    ! Rotate input data slightly to prevent caching effects
    input(1) = input(1) + 0.001
  end do
  
  call system_clock(end_time)
  
  ! Calculate aggregate performance
  total_time_ms = real(end_time - start_time) / real(clock_rate) * 1000.0
  time_per_kernel = total_time_ms / num_iterations
  
  ! Total FLOPS = flops_per_kernel * num_iterations
  aggregate_throughput = real(total_flops_per_kernel * num_iterations) / (total_time_ms * 1.0e6)
  
  print *, ""
  print *, "📊 Performance Results:"
  print *, "====================="
  print '(A,I0,A,F8.3,A)', "Total time for ", num_iterations, " iterations: ", total_time_ms, " ms"
  print '(A,F8.3,A)', "Average time per kernel: ", time_per_kernel, " ms"
  print *, ""
  print '(A,F8.1,A)', "Single kernel performance:    ", gflops_single, " GFLOPS"
  print '(A,F8.1,A)', "Aggregate throughput:         ", aggregate_throughput, " GFLOPS"
  print '(A,F8.2,A)', "Parallelism efficiency:       ", aggregate_throughput / gflops_single, "x"
  
  ! Compare with async executor results
  print *, ""
  print *, "📈 Comparison with Async Executor:"
  print *, "================================="
  print *, "Async Executor (previous):"
  print *, "  - Single kernel: ~451 GFLOPS"
  print *, "  - Aggregate:     3,630 GFLOPS (8.0x parallelism)"
  print *, ""
  print *, "Zero-Copy (current):"
  print '(A,F8.1,A)', "  - Single kernel: ", gflops_single, " GFLOPS"
  print '(A,F8.1,A)', "  - Aggregate:     ", aggregate_throughput, " GFLOPS"
  
  if (aggregate_throughput > 3630.0) then
    print *, ""
    print '(A,F5.2,A)', "🎉 Zero-copy is ", aggregate_throughput / 3630.0, "x faster than async executor!"
  end if
  
  ! Theoretical maximum analysis
  print *, ""
  print *, "💡 Analysis:"
  print *, "==========="
  
  block
    real(sp) :: memory_bandwidth_gb, compute_bandwidth_gb
    real(sp) :: efficiency
    
    ! Calculate bandwidth usage
    memory_bandwidth_gb = real(N * C * H * W * 4 +           & ! Input
                              K * C * kernel_size * kernel_size * 4 + & ! Weights  
                              N * K * H_out * W_out * 4) /    & ! Output
                         (time_per_kernel * 1.0e6)
    
    efficiency = aggregate_throughput / gflops_single
    
    print '(A,F8.1,A)', "Memory bandwidth used: ", memory_bandwidth_gb, " GB/s"
    print '(A,F8.2,A)', "Pipeline efficiency:   ", efficiency * 100.0, "%"
    
    if (efficiency < 1.5) then
      print *, "⚠️  Low parallelism - likely CPU-bound on dispatch"
      print *, "   Zero-copy eliminates GPU stalls but CPU dispatch is the bottleneck"
    else
      print *, "✅ Good parallelism achieved!"
      print *, "   Multiple kernels executing concurrently"
    end if
  end block
  
  ! Cleanup
  call gpu_cleanup_zero_copy()
  deallocate(input, weights, output)
  
  print *, ""
  print *, "🏁 Test complete!"
  
end program test_zero_copy_parallel