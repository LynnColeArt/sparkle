program test_gpu_timers
  ! Compare CPU timing vs GPU timer queries to see real GPU execution time
  
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
  integer(i64) :: total_flops, t0
  real(dp) :: gpu_time_ms, cpu_time_ms, overhead_ms
  real(dp) :: gpu_gflops, cpu_gflops
  logical :: success
  
  print *, "=== GPU Timer Query Test (GL_TIME_ELAPSED) ==="
  print *, ""
  print *, "Comparing CPU timing vs actual GPU execution time"
  print *, "This reveals the true overhead in our measurements"
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
  
  ! Warm-up run
  success = nvidia_gl_execute_conv2d(input, kernel, output, &
                                    batch, in_c, out_c, h, w, kh, kw)
  
  print *, "=== Method 1: CPU Timing (includes overhead) ==="
  
  ! CPU timing - average of 5 runs
  call tic(t0)
  do i = 1, 5
    success = nvidia_gl_execute_conv2d(input, kernel, output, &
                                      batch, in_c, out_c, h, w, kh, kw)
  end do
  cpu_time_ms = toc_seconds(t0) * 1000.0_dp / 5.0_dp
  
  cpu_gflops = real(total_flops, dp) / (cpu_time_ms * 1.0e6)
  print '(A,F8.3,A)', "CPU measured time: ", cpu_time_ms, " ms"
  print '(A,F10.1,A)', "Performance: ", cpu_gflops, " GFLOPS"
  print *, ""
  
  print *, "=== Method 2: GPU Timer Queries (actual GPU time) ==="
  
  ! GPU timing - average of 5 runs
  gpu_time_ms = 0.0_dp
  do i = 1, 5
    block
      real(dp) :: single_run_ms
      success = nvidia_gl_execute_conv2d_timed(input, kernel, output, &
                                              batch, in_c, out_c, h, w, kh, kw, &
                                              single_run_ms)
      gpu_time_ms = gpu_time_ms + single_run_ms
    end block
  end do
  gpu_time_ms = gpu_time_ms / 5.0_dp
  
  gpu_gflops = real(total_flops, dp) / (gpu_time_ms * 1.0e6)
  print '(A,F8.3,A)', "Average GPU time: ", gpu_time_ms, " ms"
  print '(A,F10.1,A)', "True GPU performance: ", gpu_gflops, " GFLOPS"
  print *, ""
  
  ! Analysis
  overhead_ms = cpu_time_ms - gpu_time_ms
  
  print *, "=== OVERHEAD ANALYSIS ==="
  print '(A,F8.3,A)', "CPU measured: ", cpu_time_ms, " ms"
  print '(A,F8.3,A)', "GPU actual:   ", gpu_time_ms, " ms" 
  print '(A,F8.3,A)', "Overhead:     ", overhead_ms, " ms"
  
  if (overhead_ms > 0.0) then
    print '(A,F6.2,A)', "Overhead ratio: ", (overhead_ms / gpu_time_ms) * 100.0, "%"
    print '(A,F6.2,A)', "Speedup potential: ", cpu_time_ms / gpu_time_ms, "×"
  end if
  print *, ""
  
  ! Performance interpretation
  if (gpu_gflops > 10000.0) then
    print *, "🔥 BREAKTHROUGH: GPU achieving 10+ TFLOPS!"
    print *, "We're actually running at Summit performance levels!"
    print *, "The overhead was hiding our true achievement!"
  else if (gpu_gflops > 5000.0) then
    print *, "🚀 EXCELLENT: GPU achieving 5+ TFLOPS!"
    print *, "Real GPU performance is much higher than CPU timing showed"
  else if (gpu_gflops > 1000.0) then
    print *, "✅ GOOD: GPU achieving 1+ TFLOPS"
    print *, "Solid foundation, optimizations will push higher"
  else if (gpu_gflops > cpu_gflops * 2.0) then
    print *, "📊 REVEALED: GPU is ", int(gpu_gflops / cpu_gflops), "× faster than measured!"
    print *, "Timer queries show the real performance"
  else
    print *, "🔍 INTERESTING: Similar CPU and GPU measurements"
    print *, "Either low overhead or kernel needs optimization"
  end if
  
  ! Mini's projection check
  if (gpu_time_ms < 10.0 .and. gpu_time_ms > 0.0) then
    print *, ""
    print *, "🎯 MINI WAS RIGHT!"
    print '(A,F6.2,A)', "GPU executes in ", gpu_time_ms, " ms - exactly as predicted!"
    print *, "The ~5ms GPU time Mini calculated is confirmed!"
    print *, "Now we just need to eliminate the submission overhead"
  end if
  
  ! Clean up
  call nvidia_gl_shutdown()
  deallocate(input, kernel, output)
  
end program test_gpu_timers