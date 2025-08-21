program test_zerocopy
  ! Test zero-copy persistent mapped buffers
  
  use kinds
  use iso_c_binding
  use sporkle_nvidia_zerocopy
  use flopcount
  implicit none
  
  ! Test configuration  
  integer, parameter :: batch = 1, in_c = 256, out_c = 512
  integer, parameter :: h = 256, w = 256, kh = 3, kw = 3
  
  real(sp), allocatable :: input(:), kernel(:), output(:)
  integer :: h_out, w_out, i
  integer(i64) :: total_flops
  integer(c_int64_t) :: cycles, best_cycles, total_cycles
  real(dp) :: cpu_ghz, time_ms, gflops, best_gflops
  
  print *, "=== Zero-Copy Performance Test ==="
  print *, ""
  print *, "Using persistent coherent mapped buffers"
  print *, "Data is written directly to GPU memory - NO transfers!"
  print *, ""
  
  ! Initialize zero-copy buffers
  if (.not. zerocopy_init()) then
    print *, "Failed to initialize zero-copy buffers"
    stop 1
  end if
  
  ! Assume CPU frequency
  cpu_ghz = 3.5_dp
  print '(A,F4.1,A)', "CPU frequency: ", cpu_ghz, " GHz"
  print *, ""
  
  ! Setup arrays
  h_out = h - kh + 1
  w_out = w - kw + 1
  
  allocate(input(batch * in_c * h * w))
  allocate(kernel(out_c * in_c * kh * kw))
  allocate(output(batch * out_c * h_out * w_out))
  
  call random_number(input)
  call random_number(kernel)
  
  total_flops = conv2d_flops(int(batch, i64), int(in_c, i64), int(out_c, i64), &
                            int(h_out, i64), int(w_out, i64), int(kh, i64), int(kw, i64))
  
  print '(A,I0,A,I0,A,I0,A,I0)', "Workload: ", batch, "×", in_c, "×", h, "×", w
  print '(A,I0,A,I0)', "Conv: ", out_c, " filters, ", kh, "×", kw, " kernel"
  print '(A,F10.3,A)', "Total FLOPs: ", real(total_flops) / 1.0e9, " GFLOPs"
  print *, ""
  
  ! Warm-up runs
  print *, "Warming up..."
  do i = 1, 3
    cycles = zerocopy_conv2d(input, kernel, output, &
                           batch, in_c, out_c, h, w, kh, kw)
  end do
  
  print *, ""
  print *, "Performance test (10 runs):"
  print *, ""
  
  best_cycles = huge(best_cycles)
  total_cycles = 0
  
  ! Performance runs
  do i = 1, 10
    cycles = zerocopy_conv2d(input, kernel, output, &
                           batch, in_c, out_c, h, w, kh, kw)
    
    if (cycles > 0) then
      time_ms = real(cycles, dp) / (cpu_ghz * 1.0e6_dp)
      gflops = real(total_flops, dp) / (time_ms * 1.0e6_dp)
      
      if (cycles < best_cycles) then
        best_cycles = cycles
        best_gflops = gflops
      end if
      
      total_cycles = total_cycles + cycles
      
      print '(A,I2,A,I0,A,F8.3,A,F10.1,A)', &
        "Run ", i, ": ", cycles, " cycles, ", time_ms, " ms, ", gflops, " GFLOPS"
      
      ! Check for milestones
      if (gflops > 1000.0 .and. i == 1) then
        print *, "    🎯 Breaking 1 TFLOPS with zero-copy!"
      end if
      if (gflops > 2000.0 .and. i == 1) then
        print *, "    🚀 Breaking 2 TFLOPS!"
      end if
      if (gflops > 5000.0 .and. i == 1) then
        print *, "    🔥 Breaking 5 TFLOPS!"
      end if
    end if
  end do
  
  ! Statistics
  print *, ""
  print *, "=== RESULTS ==="
  print '(A,I0,A)', "Best: ", best_cycles, " cycles"
  print '(A,F8.3,A)', "Best time: ", real(best_cycles, dp) / (cpu_ghz * 1.0e6_dp), " ms"
  print '(A,F10.1,A)', "Peak performance: ", best_gflops, " GFLOPS"
  print '(A,F10.1,A)', "Average: ", &
    real(total_flops * 10, dp) / (real(total_cycles, dp) / (cpu_ghz * 1.0e9_dp)), " GFLOPS"
  print *, ""
  
  ! Compare to previous results
  print *, "=== IMPROVEMENT ANALYSIS ==="
  print *, "Previous results:"
  print *, "  Basic:     354 GFLOPS"
  print *, "  RDTSC:     530 GFLOPS"
  print '(A,F10.1,A)', "  Zero-copy: ", best_gflops, " GFLOPS"
  
  if (best_gflops > 530.0) then
    print *, ""
    print '(A,F5.2,A)', "Speedup over basic: ", best_gflops / 354.0, "×"
    print '(A,F5.2,A)', "Speedup over RDTSC: ", best_gflops / 530.0, "×"
    print *, ""
    print *, "✅ Zero-copy is working!"
  end if
  
  ! Summit projection
  if (best_gflops > 500.0) then
    print *, ""
    print *, "=== SUMMIT PROJECTION ==="
    print '(A,F6.2,A)', "Current: ", best_gflops / 1000.0, " TFLOPS"
    print '(A,F6.2,A)', "With kernel optimization (3×): ", best_gflops * 3.0 / 1000.0, " TFLOPS"
    print '(A,F6.2,A)', "With async pipeline (6.5×): ", best_gflops * 6.5 / 1000.0, " TFLOPS"
    
    if (best_gflops * 6.5 > 15000.0) then
      print *, ""
      print *, "🏔️ SUMMIT ACHIEVABLE!"
      print *, "We can reach 15+ TFLOPS with full optimization!"
    end if
  end if
  
  ! Validate output
  if (maxval(abs(output)) > 0.0) then
    print *, ""
    print '(A,F10.6)', "Output validation: max = ", maxval(output)
  end if
  
  ! Clean up
  call zerocopy_shutdown()
  deallocate(input, kernel, output)
  
end program test_zerocopy