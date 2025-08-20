program test_nvidia_full_power
  ! Test NVIDIA A4500 with ALL AMD optimizations
  ! Target: 3,630+ GFLOPS!
  
  use kinds
  use sporkle_nvidia_optimized
  implicit none
  
  logical :: success
  
  print *, "=============================================="
  print *, "NVIDIA A4500 FULL POWER TEST"
  print *, "=============================================="
  print *, ""
  print *, "Applying ALL AMD optimizations to NVIDIA:"
  print *, "1. Triple-buffered async executor (6.5x speedup)"
  print *, "2. Shared memory tiling (16x16 tiles)"
  print *, "3. Thread-safe shader cache"
  print *, "4. Optimized work group sizes"
  print *, "5. Memory coalescing patterns"
  print *, ""
  
  ! Initialize with all optimizations
  print *, "Initializing NVIDIA with full optimizations..."
  success = nvidia_opt_init()
  
  if (.not. success) then
    print *, "Failed to initialize!"
    stop 1
  end if
  
  print *, "✓ Initialization successful!"
  print *, ""
  
  ! Run the benchmark
  call nvidia_opt_benchmark()
  
  ! Cleanup
  call nvidia_opt_shutdown()
  
  print *, ""
  print *, "=============================================="
  print *, "TEST COMPLETE"
  print *, "=============================================="
  
end program test_nvidia_full_power