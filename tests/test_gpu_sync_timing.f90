program test_gpu_sync_timing
  ! Test GPU performance using fence sync for accurate timing
  
  use kinds
  use sporkle_nvidia_opengl
  use flopcount
  use time_utils
  implicit none
  
  ! Test configuration
  integer, parameter :: batch = 1, in_c = 256, out_c = 512
  integer, parameter :: h = 256, w = 256, kh = 3, kw = 3
  
  real(sp), allocatable :: input(:), kernel(:), output(:)
  integer :: h_out, w_out, i
  integer(i64) :: total_flops, t0, t1
  real(dp) :: elapsed_ms, best_time_ms
  real(dp) :: gflops
  logical :: success
  
  print *, "=== GPU Performance with Sync-Based Timing ==="
  print *, ""
  print *, "Using glFinish() to ensure GPU completion"
  print *, "This gives us accurate GPU+driver time"
  print *, ""
  
  ! Initialize GPU
  if (.not. nvidia_gl_init()) then
    print *, "Failed to initialize GPU"
    stop 1
  end if
  
  ! Calculate sizes
  h_out = h - kh + 1
  w_out = w - kw + 1
  
  ! Allocate arrays
  allocate(input(batch * in_c * h * w))
  allocate(kernel(out_c * in_c * kh * kw))
  allocate(output(batch * out_c * h_out * w_out))
  
  ! Initialize with test data
  call random_number(input)
  call random_number(kernel)
  output = 0.0
  
  ! Calculate workload
  total_flops = conv2d_flops(int(batch, i64), int(in_c, i64), int(out_c, i64), &
                            int(h_out, i64), int(w_out, i64), int(kh, i64), int(kw, i64))
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Workload: ", batch, "×", in_c, "×", h, "×", w
  print '(A,I0,A,I0)', "Conv: ", out_c, " filters, ", kh, "×", kw, " kernel"
  print '(A,F10.3,A)', "Total FLOPs: ", real(total_flops) / 1.0e9, " GFLOPs"
  print *, ""
  
  ! Warm-up runs
  print *, "Warming up GPU..."
  do i = 1, 3
    success = nvidia_gl_execute_conv2d(input, kernel, output, &
                                      batch, in_c, out_c, h, w, kh, kw)
  end do
  
  print *, ""
  print *, "Running timed tests (10 iterations)..."
  print *, ""
  
  best_time_ms = 1000000.0
  
  ! Timed runs
  do i = 1, 10
    ! Time just the GPU execution with proper sync
    call tic(t0)
    success = nvidia_gl_execute_conv2d(input, kernel, output, &
                                      batch, in_c, out_c, h, w, kh, kw)
    elapsed_ms = toc_seconds(t0) * 1000.0_dp
    
    if (elapsed_ms < best_time_ms) then
      best_time_ms = elapsed_ms
    end if
    
    gflops = real(total_flops, dp) / (elapsed_ms * 1.0e6)
    
    print '(A,I2,A,F8.3,A,F10.1,A)', "  Run ", i, ": ", elapsed_ms, " ms, ", gflops, " GFLOPS"
    
    ! Check for breakthroughs
    if (gflops > 10000.0 .and. i == 1) then
      print *, "    🔥 Breaking 10 TFLOPS!"
    end if
    if (gflops > 5000.0 .and. i == 1) then
      print *, "    🚀 Breaking 5 TFLOPS!"
    end if
    if (gflops > 1000.0 .and. i == 1) then
      print *, "    ✅ Breaking 1 TFLOPS!"
    end if
  end do
  
  ! Best performance
  gflops = real(total_flops, dp) / (best_time_ms * 1.0e6)
  
  print *, ""
  print *, "=== RESULTS ==="
  print '(A,F8.3,A)', "Best time: ", best_time_ms, " ms"
  print '(A,F10.1,A)', "Peak performance: ", gflops, " GFLOPS"
  print '(A,F6.2,A)', "Efficiency: ", (gflops / 16500.0) * 100.0, "% of theoretical peak"
  print *, ""
  
  ! Analysis based on timing
  if (best_time_ms < 10.0) then
    print *, "🎯 EXCELLENT: Sub-10ms execution achieved!"
    print *, "GPU is running efficiently with low overhead"
    
    if (gflops > 15000.0) then
      print *, ""
      print *, "🏔️ SUMMIT ACHIEVED!"
      print *, "We've reached the target 15+ TFLOPS performance!"
    end if
  else if (best_time_ms < 50.0) then
    print *, "✅ GOOD: Fast execution with room for optimization"
  else if (best_time_ms > 200.0) then
    print *, "⚠️ SLOW: High overhead or inefficient kernel"
    print *, "Consider kernel optimization or reducing launch overhead"
  end if
  
  ! Check output validity
  if (maxval(abs(output)) > 0.0) then
    print *, ""
    print '(A,F10.6)', "Output validation: max value = ", maxval(output)
  end if
  
  ! Clean up
  call nvidia_gl_shutdown()
  deallocate(input, kernel, output)
  
end program test_gpu_sync_timing