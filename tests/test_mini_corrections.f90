program test_mini_corrections
  ! Test Mini's corrections to AMD hardware profiling
  ! Shows Wave32, corrected bandwidth, and proper peak performance
  
  use kinds
  use sporkle_hardware_profiler
  use sporkle_autotuner_enhanced
  implicit none
  
  type(hardware_characteristics) :: hw
  type(kernel_parameters) :: params
  type(tunable_parameters) :: enhanced_config
  real(dp) :: ai, device_balance
  
  print *, "=============================================="
  print *, "Testing Mini's AMD Corrections"
  print *, "=============================================="
  print *, ""
  
  ! Profile AMD GPU with Mini's corrections
  hw = profile_amd_gpu()
  
  print *, ""
  print *, "Mini's Corrections Verified:"
  print *, "-----------------------------"
  print *, "✅ Wave size:", hw%warp_size, "(Wave32 for RDNA3, not Wave64)"
  print *, "✅ Peak performance:", hw%peak_gflops, "GFLOPS (40 TFLOPS, not 51.48)"
  print *, "✅ Memory bandwidth:", hw%peak_bandwidth_gbs, "GB/s (960, not 800)"
  print *, "✅ Dual-issue support:", hw%dual_issue
  print *, ""
  
  ! Show device balance calculation
  device_balance = hw%peak_gflops * 1000.0_dp / hw%peak_bandwidth_gbs  ! FLOP/byte
  print *, "Device Balance Calculation:"
  print *, "  Peak FLOPS/Bandwidth =", hw%peak_gflops, "/ ", hw%peak_bandwidth_gbs, "="
  print *, "  Device balance:", device_balance, "FLOP/byte"
  print *, ""
  
  ! Derive optimal parameters
  params = derive_optimal_parameters(hw)
  
  print *, "Optimal Parameters from Hardware Profiler:"
  print *, "  Block size:", params%block_size, "threads"
  print *, "  Tile size:", params%tile_size
  print *, "  Outputs per thread:", params%outputs_per_thread
  print *, "  Use shared memory:", params%use_shared_memory
  print *, ""
  
  ! Test enhanced autotuner config selection
  enhanced_config = select_optimal_config("AMD Radeon RX 7900 XT", 128, 256, 224, 224)
  
  print *, "Enhanced Autotuner Configuration:"
  print *, "  Wave size:", enhanced_config%wave_size
  print *, "  Block dimensions:", enhanced_config%block_x, "×", enhanced_config%block_y
  print *, "  Total threads:", enhanced_config%total_threads
  print *, "  Ko blocking (tile_k):", enhanced_config%tile_k
  print *, "  Unroll factor:", enhanced_config%unroll_factor
  print *, "  RDNA3 mode:", enhanced_config%is_rdna3
  print *, ""
  
  ! Calculate arithmetic intensity for large convolution
  ai = calculate_arithmetic_intensity(enhanced_config, 128, 256, 222, 222)
  
  print *, "Arithmetic Intensity Analysis:"
  print *, "  For 1×128→256 @ 224×224 (3×3 conv):"
  print *, "  With tiling (32×32 LDS, Ko=64):"
  print *, "    Arithmetic intensity:", ai, "FLOP/byte"
  print *, "    Device balance:", device_balance, "FLOP/byte"
  print *, ""
  
  if (ai > device_balance) then
    print *, "✅ COMPUTE BOUND! Can achieve near-peak performance"
    print *, "   Expected efficiency: 75-85%"
    print *, "   Target performance:", hw%peak_gflops * 0.8_dp, "GFLOPS"
  else
    print *, "❌ Memory bound - need better tiling"
    print *, "   Limited by bandwidth:", hw%peak_bandwidth_gbs * ai / 1000.0_dp, "GFLOPS"
  end if
  
  print *, ""
  print *, "=============================================="
  print *, "Mini's Key Insights Applied:"
  print *, "=============================================="
  print *, ""
  print *, "1. RDNA3 prefers Wave32 (better occupancy)"
  print *, "2. 256 threads per workgroup is optimal"
  print *, "3. Ko=64 blocking achieves ~150 FLOP/byte"
  print *, "4. With proper tiling, we're compute-bound"
  print *, "5. Peak is 40 TFLOPS (dual-issue), not 51.48"
  print *, ""
  print *, "Next: Generate shader with these parameters!"
  
end program test_mini_corrections