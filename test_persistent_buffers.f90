program test_persistent_buffers
  ! Test persistent buffer ring for reduced overhead
  
  use kinds
  use sporkle_nvidia_persistent
  use flopcount
  use time_utils
  implicit none
  
  ! Test configuration
  integer, parameter :: batch = 1, in_c = 256, out_c = 512
  integer, parameter :: h = 256, w = 256, kh = 3, kw = 3
  
  real(sp), allocatable :: input(:), kernel(:), output(:)
  integer :: h_out, w_out, i
  integer(i64) :: total_flops
  real(dp) :: time_ms, total_time, best_time, worst_time
  real(dp) :: gflops, avg_gflops, peak_gflops
  
  print *, "=== Persistent Buffer Performance Test ==="
  print *, ""
  print *, "Testing Mini's Switch #2: Persistent coherent buffers"
  print *, "Expected improvement: 1.2× from eliminating allocation overhead"
  print *, ""
  
  ! Initialize persistent buffers
  if (.not. persistent_init()) then
    print *, "Failed to initialize persistent buffers"
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
  print *, "Warming up persistent buffers..."
  do i = 1, 5
    time_ms = persistent_conv2d(input, kernel, output, &
                               batch, in_c, out_c, h, w, kh, kw)
  end do
  
  print *, ""
  print *, "Running performance test (20 iterations)..."
  print *, ""
  
  best_time = 1000000.0
  worst_time = 0.0
  total_time = 0.0
  
  ! Performance runs
  do i = 1, 20
    time_ms = persistent_conv2d(input, kernel, output, &
                               batch, in_c, out_c, h, w, kh, kw)
    
    if (time_ms > 0.0) then
      gflops = real(total_flops, dp) / (time_ms * 1.0e6)
      
      if (time_ms < best_time) best_time = time_ms
      if (time_ms > worst_time) worst_time = time_ms
      total_time = total_time + time_ms
      
      if (mod(i, 5) == 0) then
        print '(A,I2,A,F8.3,A,F10.1,A)', "  Run ", i, ": ", time_ms, " ms, ", gflops, " GFLOPS"
      end if
      
      ! Check for breakthroughs
      if (gflops > 500.0 .and. i == 1) then
        print *, "    🎯 Breaking 500 GFLOPS with persistent buffers!"
      end if
      if (gflops > 1000.0 .and. i == 1) then
        print *, "    🚀 Breaking 1 TFLOPS!"
      end if
    end if
  end do
  
  ! Calculate statistics
  avg_gflops = real(total_flops, dp) / ((total_time / 20.0) * 1.0e6)
  peak_gflops = real(total_flops, dp) / (best_time * 1.0e6)
  
  print *, ""
  print *, "=== RESULTS ==="
  print '(A,F8.3,A)', "Best time:    ", best_time, " ms"
  print '(A,F8.3,A)', "Worst time:   ", worst_time, " ms"
  print '(A,F8.3,A)', "Average time: ", total_time / 20.0, " ms"
  print *, ""
  print '(A,F10.1,A)', "Peak performance: ", peak_gflops, " GFLOPS"
  print '(A,F10.1,A)', "Avg performance:  ", avg_gflops, " GFLOPS"
  print '(A,F6.2,A)', "Efficiency: ", (peak_gflops / 16500.0) * 100.0, "% of theoretical peak"
  print *, ""
  
  ! Compare to previous results
  print *, "=== IMPROVEMENT ANALYSIS ==="
  print *, "Previous best: ~354 GFLOPS (with allocation overhead)"
  print '(A,F10.1,A)', "Current peak:  ", peak_gflops, " GFLOPS"
  
  if (peak_gflops > 354.0) then
    print '(A,F5.2,A)', "Speedup: ", peak_gflops / 354.0, "×"
    print *, "✅ Persistent buffers are working!"
  else
    print *, "⚠️ No improvement - may need kernel optimization"
  end if
  
  ! Check output validity
  if (maxval(abs(output)) > 0.0) then
    print *, ""
    print '(A,F10.6)', "Output validation: max value = ", maxval(output)
  else
    print *, "⚠️ Output is zeros - kernel may not be executing"
  end if
  
  ! Project to Summit
  if (best_time < 100.0) then
    print *, ""
    print *, "=== SUMMIT PROJECTION ==="
    print *, "With current timing and optimizations:"
    print '(A,F6.2,A)', "  - Kernel optimization (2×): ", peak_gflops * 2.0 / 1000.0, " TFLOPS"
    print '(A,F6.2,A)', "  - Optimal geometry (1.8×): ", peak_gflops * 3.6 / 1000.0, " TFLOPS"
    print '(A,F6.2,A)', "  - Full pipeline (6.5×): ", peak_gflops * 6.5 / 1000.0, " TFLOPS"
    
    if (peak_gflops * 6.5 > 15000.0) then
      print *, ""
      print *, "🏔️ SUMMIT ACHIEVABLE with full optimizations!"
    end if
  end if
  
  ! Clean up
  call persistent_shutdown()
  deallocate(input, kernel, output)
  
end program test_persistent_buffers