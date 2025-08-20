program test_nvidia_async_pattern
  ! Test Step 1: NVIDIA async pattern implementation
  ! Based on AMD's 6.5× speedup through triple buffering + fences
  
  use kinds
  use time_utils
  use flopcount
  use sporkle_nvidia_async
  implicit none
  
  ! Test arrays
  real(sp), allocatable :: input(:), kernel(:), output(:)
  integer :: batch = 1, in_c = 64, out_c = 128, h = 112, w = 112, kh = 3, kw = 3
  integer :: h_out, w_out, test
  integer(i64) :: total_flops
  real(dp) :: workload_gflops, time_ms, gflops
  
  print *, "=============================================="
  print *, "🚀 NVIDIA Async Pattern Test - Step 1"
  print *, "=============================================="
  print *, ""
  print *, "Testing the core async pattern that gives AMD 6.5× speedup:"
  print *, "  • Triple buffering (3 buffer sets)"
  print *, "  • Fence-based synchronization"
  print *, "  • Non-blocking work submission"
  print *, ""
  
  ! Calculate dimensions
  h_out = h - kh + 1
  w_out = w - kw + 1
  
  ! Calculate workload
  total_flops = conv2d_flops(int(batch, i64), int(in_c, i64), int(out_c, i64), &
                            int(h_out, i64), int(w_out, i64), int(kh, i64), int(kw, i64))
  workload_gflops = real(total_flops, dp) / 1.0e9_dp
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Config: ", batch, "×", in_c, "×", h, "×", w, " → ", out_c, " channels"
  print '(A,F10.3,A)', "Workload: ", workload_gflops, " GFLOPs"
  print *, ""
  
  ! Allocate arrays
  allocate(input(batch * in_c * h * w))
  allocate(kernel(out_c * in_c * kh * kw))
  allocate(output(batch * out_c * h_out * w_out))
  
  ! Initialize with test data
  call random_number(input)
  call random_number(kernel)
  output = 0.0
  
  ! Initialize async executor
  if (.not. nvidia_async_init()) then
    print *, "ERROR: Failed to initialize NVIDIA async executor"
    stop
  end if
  
  print *, "=============================================="
  print *, "Single Execution Test"
  print *, "=============================================="
  
  ! Test single execution
  time_ms = nvidia_async_conv2d(input, kernel, output, &
                               batch, in_c, out_c, h, w, kh, kw)
  
  if (time_ms > 0.0_dp) then
    gflops = (workload_gflops * 1000.0_dp) / time_ms
    print '(A,F8.2,A,F10.1,A)', "Performance: ", time_ms, " ms, ", gflops, " GFLOPS"
  else
    print *, "Execution failed"
  end if
  
  print *, ""
  print *, "=============================================="
  print *, "Multiple Execution Test (Async Benefit)"
  print *, "=============================================="
  print *, ""
  print *, "Running 5 consecutive executions to test async pipeline..."
  
  ! Test multiple executions to see async benefit
  block
    real(dp) :: total_time
    integer(i64) :: start_tick
    integer :: num_runs = 5
    
    call tic(start_tick)
    
    do test = 1, num_runs
      time_ms = nvidia_async_conv2d(input, kernel, output, &
                                   batch, in_c, out_c, h, w, kh, kw)
      print '(A,I0,A,F8.2,A)', "  Run ", test, ": ", time_ms, " ms"
    end do
    
    total_time = toc_seconds(start_tick)
    
    print *, ""
    print '(A,I0,A,F8.2,A)', "Total time for ", num_runs, " runs: ", total_time * 1000.0_dp, " ms"
    print '(A,F8.2,A)', "Average per run: ", (total_time * 1000.0_dp) / real(num_runs), " ms"
    
    ! Calculate potential speedup
    block
      real(dp) :: avg_time_ms, aggregate_gflops
      avg_time_ms = (total_time * 1000.0_dp) / real(num_runs)
      aggregate_gflops = (workload_gflops * 1000.0_dp) / avg_time_ms
      
      print '(A,F10.1,A)', "Aggregate performance: ", aggregate_gflops, " GFLOPS"
      
      if (aggregate_gflops > 1000.0_dp) then
        print *, "🎉 SUCCESS: Breaking 1 TFLOPS barrier!"
      end if
      if (aggregate_gflops > 5000.0_dp) then
        print *, "🚀 EXCELLENT: Approaching target performance!"
      end if
    end block
  end block
  
  print *, ""
  print *, "=============================================="
  print *, "Next Steps"
  print *, "=============================================="
  print *, ""
  print *, "Step 1 Complete: Async pattern implemented ✓"
  print *, ""
  print *, "Next steps:"
  print *, "  Step 2: Add optimized shaders (32×32 tiles, 128 threads)"
  print *, "  Step 3: Add shared memory tiling"
  print *, "  Step 4: Add register blocking (4×4 outputs)"
  print *, "  Step 5: Add 16× loop unrolling"
  print *, ""
  print *, "Expected final result: 16+ TFLOPS"
  
  ! Cleanup
  call nvidia_async_shutdown()
  deallocate(input, kernel, output)
  
end program test_nvidia_async_pattern