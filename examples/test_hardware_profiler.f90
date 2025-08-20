program test_hardware_profiler
  ! Test the hardware profiler and see what parameters we SHOULD be using
  
  use kinds
  use sporkle_hardware_profiler
  implicit none
  
  type(hardware_characteristics) :: nvidia_hw, amd_hw, cpu_hw
  type(kernel_parameters) :: nvidia_params, amd_params, cpu_params
  
  print *, "=============================================="
  print *, "Sporkle Hardware Profiler Test"
  print *, "=============================================="
  print *, ""
  print *, "Discovering optimal parameters for peak performance..."
  print *, ""
  
  ! Profile NVIDIA GPU
  print *, "1. NVIDIA GPU PROFILE"
  print *, "====================="
  nvidia_hw = profile_nvidia_gpu()
  nvidia_params = derive_optimal_parameters(nvidia_hw)
  
  print *, ""
  print *, "2. AMD GPU PROFILE"
  print *, "=================="
  amd_hw = profile_amd_gpu()
  amd_params = derive_optimal_parameters(amd_hw)
  
  print *, ""
  print *, "3. CPU PROFILE"
  print *, "=============="
  cpu_hw = profile_cpu()
  cpu_params = derive_optimal_parameters(cpu_hw)
  
  ! Summary comparison
  print *, ""
  print *, "=============================================="
  print *, "PERFORMANCE COMPARISON SUMMARY"
  print *, "=============================================="
  print *, ""
  
  print '(A20,A15,A15,A15)', "Device", "Current", "Target", "Speedup"
  print '(A20,A15,A15,A15)', "------", "-------", "------", "-------"
  
  print '(A20,F10.1,A5,F10.1,A5,F10.1,A)', &
    "NVIDIA A4500", nvidia_hw%measured_gflops, " →", &
    nvidia_hw%peak_gflops * 0.85, " ", &
    (nvidia_hw%peak_gflops * 0.85) / nvidia_hw%measured_gflops, "×"
    
  print '(A20,F10.1,A5,F10.1,A5,F10.1,A)', &
    "AMD RX 7900 XT", amd_hw%measured_gflops, " →", &
    amd_hw%peak_gflops * 0.80, " ", &
    (amd_hw%peak_gflops * 0.80) / amd_hw%measured_gflops, "×"
    
  print '(A20,F10.1,A5,F10.1,A5,F10.1,A)', &
    "AMD Ryzen 7700X", cpu_hw%measured_gflops, " →", &
    cpu_hw%peak_gflops * 0.70, " ", &
    (cpu_hw%peak_gflops * 0.70) / cpu_hw%measured_gflops, "×"
  
  print *, ""
  print *, "=============================================="
  print *, "KEY INSIGHTS"
  print *, "=============================================="
  print *, ""
  
  print *, "NVIDIA needs:"
  print '(A,I0,A)', "  • Larger blocks: ", nvidia_params%block_size, " threads (not 256)"
  print '(A,I0,A,I0)', "  • Bigger tiles: ", nvidia_params%tile_size, "×", nvidia_params%tile_size, " (not 16×16)"
  print '(A,I0,A,I0)', "  • More work/thread: ", nvidia_params%outputs_per_thread, "×", &
                      nvidia_params%outputs_per_thread, " outputs"
  print '(A,I0,A)', "  • Aggressive unroll: ", nvidia_params%unroll_factor, "× (not 1×)"
  print *, ""
  
  print *, "AMD needs:"
  print '(A,I0,A)', "  • Wave-aware blocks: ", amd_params%block_size, " threads"
  print '(A,I0,A,I0)', "  • Infinity Cache tiles: ", amd_params%tile_size, "×", amd_params%tile_size
  print '(A,I0,A)', "  • Dual-issue unroll: ", amd_params%unroll_factor, "×"
  print *, ""
  
  print *, "CPU needs:"
  print '(A,I0,A)', "  • AVX-512 vectors: ", cpu_params%block_size, " elements"
  print '(A,I0,A,I0)', "  • L2-resident tiles: ", cpu_params%tile_size, "×", cpu_params%tile_size
  print '(A,I0,A)', "  • Deep unroll: ", cpu_params%unroll_factor, "×"
  print *, ""
  
  ! Calculate total potential
  block
    real(dp) :: total_current, total_target, total_speedup
    
    total_current = nvidia_hw%measured_gflops + amd_hw%measured_gflops + cpu_hw%measured_gflops
    total_target = nvidia_hw%peak_gflops * 0.85 + amd_hw%peak_gflops * 0.80 + cpu_hw%peak_gflops * 0.70
    total_speedup = total_target / total_current
    
    print *, "=============================================="
    print *, "TOTAL SYSTEM POTENTIAL"
    print *, "=============================================="
    print '(A,F10.1,A)', "  Current total: ", total_current, " GFLOPS"
    print '(A,F10.1,A)', "  Target total:  ", total_target, " GFLOPS"
    print '(A,F10.1,A)', "  Total speedup: ", total_speedup, "×"
    print *, ""
    print '(A,F10.1,A)', "  We're leaving ", total_target - total_current, " GFLOPS on the table!"
    print *, ""
  end block
  
  print *, "=============================================="
  print *, "NEXT STEPS"
  print *, "=============================================="
  print *, "1. Update kernels to use these parameters"
  print *, "2. Implement auto-tuning based on hardware profile"
  print *, "3. Achieve 80%+ efficiency on all devices"
  print *, "4. Prove universal patterns work optimally everywhere!"
  print *, ""
  
end program test_hardware_profiler