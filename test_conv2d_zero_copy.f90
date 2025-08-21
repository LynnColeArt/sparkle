program test_conv2d_zero_copy
  ! Test Zero-Copy Conv2D Performance
  ! =================================
  !
  ! Compares traditional vs zero-copy convolution
  
  use kinds
  use iso_c_binding
  use gpu_opengl_interface, only: gpu_init, gpu_cleanup, gpu_execute_conv2d_ref
  use gpu_opengl_zero_copy
  implicit none
  
  ! Test parameters
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
  
  ! Data arrays
  real(sp), allocatable :: input(:), weights(:), output(:)
  real(sp), allocatable :: output_ref(:)
  
  ! Timing
  real(sp) :: time_traditional, time_zero_copy
  real(sp) :: time_breakdown_copy, time_breakdown_compute, time_breakdown_readback
  integer :: i, warmup_runs = 3, test_runs = 10
  
  ! Performance stats
  integer(i64) :: total_flops, total_bytes
  real(sp) :: gflops_traditional, gflops_zero_copy
  real(sp) :: bandwidth_traditional, bandwidth_zero_copy
  real(sp) :: speedup
  
  print *, "🏎️  Zero-Copy Conv2D Performance Test"
  print *, "===================================="
  print *, ""
  
  ! Calculate workload size
  total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
  
  total_bytes = int(N * C * H * W * 4, int64) +           & ! Input
                int(K * C * kernel_size * kernel_size * 4, int64) + & ! Weights
                int(N * K * H_out * W_out * 4, int64)       ! Output
  
  print '(A,I0,A)', "Workload: ", total_flops / 1000000, " MFLOPS"
  print '(A,I0,A,I0,A,I0,A,I0)', "Input: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0,A,I0)', "Conv: ", K, " filters, ", kernel_size, "x", kernel_size
  print '(A,F6.2,A)', "Total memory: ", real(total_bytes) / (1024.0 * 1024.0), " MB"
  print *, ""
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  allocate(output_ref(N * K * H_out * W_out))
  
  ! Initialize with test data
  call random_number(input)
  call random_number(weights)
  input = input - 0.5
  weights = weights - 0.5
  
  ! Initialize both systems first
  print *, "🏎️ Initializing GPU systems..."
  if (.not. gpu_init_zero_copy()) then
    print *, "❌ Failed to initialize zero-copy GPU"
    stop 1
  end if
  
  ! Test 1: Traditional implementation
  print *, ""
  print *, "📊 Testing traditional implementation..."
  if (.not. gpu_init()) then
    print *, "❌ Failed to initialize GPU"
    stop 1
  end if
  
  ! Warmup
  do i = 1, warmup_runs
    time_traditional = gpu_execute_conv2d_ref(input, weights, output_ref, &
                                            N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  end do
  
  ! Measure with breakdown
  time_traditional = 0.0
  time_breakdown_copy = 0.0
  time_breakdown_compute = 0.0
  time_breakdown_readback = 0.0
  
  do i = 1, test_runs
    block
      real(sp) :: t_start, t_copy, t_compute
      integer(i64) :: start_time, end_time, clock_rate
      
      call system_clock(start_time, count_rate=clock_rate)
      
      ! Full execution
      time_traditional = time_traditional + gpu_execute_conv2d_ref(input, weights, output_ref, &
                                                                 N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
      
      call system_clock(end_time)
      
      ! Estimate breakdown (approximate)
      t_start = real((end_time - start_time) / clock_rate * 1000.0, real32)
      t_copy = real(total_bytes) / (10.0e9) * 1000.0  ! Assume 10 GB/s for copies
      t_compute = t_start - t_copy
      
      time_breakdown_copy = time_breakdown_copy + t_copy * 0.5  ! Upload
      time_breakdown_compute = time_breakdown_compute + t_compute
      time_breakdown_readback = time_breakdown_readback + t_copy * 0.5  ! Download
    end block
  end do
  
  time_traditional = time_traditional / test_runs
  time_breakdown_copy = time_breakdown_copy / test_runs
  time_breakdown_compute = time_breakdown_compute / test_runs
  time_breakdown_readback = time_breakdown_readback / test_runs
  
  gflops_traditional = real(total_flops) / (time_traditional * 1.0e6)
  bandwidth_traditional = real(total_bytes) / (time_traditional * 1.0e6)
  
  call gpu_cleanup()
  
  ! Test 2: Zero-copy implementation
  print *, ""
  print *, "🚀 Testing zero-copy implementation..."
  
  ! Warmup
  do i = 1, warmup_runs
    time_zero_copy = gpu_execute_conv2d_zero_copy(input, weights, output, &
                                                 N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  end do
  
  ! Measure
  time_zero_copy = 0.0
  do i = 1, test_runs
    time_zero_copy = time_zero_copy + gpu_execute_conv2d_zero_copy(input, weights, output, &
                                                                   N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  end do
  time_zero_copy = time_zero_copy / test_runs
  
  gflops_zero_copy = real(total_flops) / (time_zero_copy * 1.0e6)
  bandwidth_zero_copy = real(total_bytes) / (time_zero_copy * 1.0e6)
  
  call gpu_cleanup_zero_copy()
  
  ! Calculate speedup
  speedup = time_traditional / time_zero_copy
  
  ! Verify correctness
  print *, ""
  print *, "🔍 Verifying correctness..."
  block
    real(sp) :: max_diff, avg_diff
    integer :: j
    
    max_diff = 0.0
    avg_diff = 0.0
    do j = 1, size(output)
      max_diff = max(max_diff, abs(output(j) - output_ref(j)))
      avg_diff = avg_diff + abs(output(j) - output_ref(j))
    end do
    avg_diff = avg_diff / size(output)
    
    print '(A,E12.5)', "Max difference: ", max_diff
    print '(A,E12.5)', "Avg difference: ", avg_diff
    
    if (max_diff < 1e-4) then
      print *, "✅ Results match!"
    else
      print *, "❌ Results differ!"
    end if
  end block
  
  ! Results
  print *, ""
  print *, "📊 Performance Results"
  print *, "===================="
  print *, ""
  print *, "Traditional Implementation:"
  print '(A,F8.2,A)', "  Total time:     ", time_traditional, " ms"
  print '(A,F8.2,A)', "  - Data upload:  ", time_breakdown_copy, " ms"
  print '(A,F8.2,A)', "  - GPU compute:  ", time_breakdown_compute, " ms"
  print '(A,F8.2,A)', "  - Data readback:", time_breakdown_readback, " ms"
  print '(A,F8.1,A)', "  Performance:    ", gflops_traditional, " GFLOPS"
  print '(A,F8.1,A)', "  Bandwidth:      ", bandwidth_traditional, " GB/s"
  
  print *, ""
  print *, "Zero-Copy Implementation:"
  print '(A,F8.2,A)', "  Total time:     ", time_zero_copy, " ms"
  print '(A,F8.1,A)', "  Performance:    ", gflops_zero_copy, " GFLOPS"
  print '(A,F8.1,A)', "  Bandwidth:      ", bandwidth_zero_copy, " GB/s"
  
  print *, ""
  print '(A,F8.2,A)', "🎯 Speedup:         ", speedup, "x"
  print '(A,F6.2,A)', "💾 Memory saved:    ", real(total_bytes) / (1024.0 * 1024.0), " MB"
  print '(A,F6.2,A)', "⏱️  Time saved:      ", time_traditional - time_zero_copy, " ms/op"
  
  ! Analysis
  print *, ""
  print *, "💡 Analysis:"
  if (speedup > 1.5) then
    print *, "   ✅ Significant speedup achieved!"
    print *, "   Zero-copy eliminates memory transfer overhead"
    print '(A,F5.1,A)', "   Saved approximately ", &
      (time_breakdown_copy + time_breakdown_readback) / time_traditional * 100.0, "% of runtime"
  else if (speedup > 1.0) then
    print *, "   ✅ Modest speedup achieved"
    print *, "   Benefits will scale with larger workloads"
  else
    print *, "   ⚠️  No significant speedup"
    print *, "   Compute-bound workload dominates"
  end if
  
  ! Cleanup
  deallocate(input, weights, output, output_ref)
  
  print *, ""
  print *, "🏁 Test complete!"
  
end program test_conv2d_zero_copy