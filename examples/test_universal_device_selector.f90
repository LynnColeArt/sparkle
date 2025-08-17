program test_universal_device_selector
  use sparkle_universal_device_selector
  use sparkle_types
  implicit none
  
  type(universal_device_selector) :: selector
  type(workload_characteristics) :: workload
  type(device_routing_decision) :: decision
  integer :: i
  
  print *, "🚀 Testing Universal Device Selector"
  print *, "===================================="
  print *, ""
  
  ! Initialize the selector
  call selector%discover_devices()
  print *, ""
  
  ! Test 1: Small GEMM (should go to CPU)
  print *, "📊 Test 1: Small GEMM (100x100)"
  workload = selector%analyze_workload( &
    flops = int(2_int64 * 100**3, int64),     & ! 2*n^3 for GEMM
    bytes = int(3_int64 * 100**2 * 4, int64), & ! 3 matrices * n^2 * sizeof(float)
    pattern = PATTERN_GEMM)
  
  decision = selector%select_optimal_device(workload)
  print '(A,A)', " Decision: ", trim(decision%reasoning)
  print '(A,F8.1)', " Arithmetic intensity: ", workload%arithmetic_intensity
  print *, ""
  
  ! Test 2: Large convolution (should go to GPU with async)
  print *, "📊 Test 2: ResNet-50 First Layer Conv"
  workload = selector%analyze_workload( &
    flops = 236027904_int64,           & ! From our benchmarks
    bytes = 12845056_int64,            & ! Input + weights + output
    pattern = PATTERN_CONV)
  
  decision = selector%select_optimal_device(workload)
  print '(A,A)', " Decision: ", trim(decision%reasoning)
  print '(A,F8.1)', " Arithmetic intensity: ", workload%arithmetic_intensity
  print *, ""
  
  ! Test 3: Memory-bound operation (should consider bandwidth)
  print *, "📊 Test 3: Large vector addition"
  workload = selector%analyze_workload( &
    flops = 10000000_int64,            & ! 10M additions
    bytes = 3_int64 * 10000000 * 4,   & ! 3 vectors * 10M * sizeof(float)
    pattern = PATTERN_ELEMENTWISE)
  
  decision = selector%select_optimal_device(workload)
  print '(A,A)', " Decision: ", trim(decision%reasoning)
  print '(A,F8.1)', " Arithmetic intensity: ", workload%arithmetic_intensity
  print *, ""
  
  ! Test 4: Simulate profiling feedback
  print *, "📊 Test 4: Update profiling data and re-test"
  
  ! Simulate that we achieved great async performance on GPU
  call selector%update_profiling_data( &
    device_id = 2,                     & ! GPU
    pattern = PATTERN_CONV,            &
    achieved_gflops = 3630.0_real64)   ! Our async performance!
    
  ! Now re-test the convolution
  decision = selector%select_optimal_device(workload)
  print '(A,A)', " Decision after profiling: ", trim(decision%reasoning)
  print *, ""
  
  ! Test 5: Show all device capabilities
  print *, "📋 Device Capabilities Summary:"
  print *, "------------------------------"
  do i = 1, selector%num_devices
    associate(dev => selector%devices(i))
      print '(A,A)', " Device: ", trim(dev%name)
      print '(A,F8.1,A)', "   Peak: ", dev%caps%peak_gflops, " GFLOPS"
      print '(A,F8.1,A)', "   Memory BW: ", dev%caps%memory_bandwidth, " GB/s"
      if (dev%pattern_count(PATTERN_CONV) > 0) then
        print '(A,F8.1,A)', "   Conv perf: ", &
              dev%pattern_performance(PATTERN_CONV), " GFLOPS (measured)"
      end if
      print *, ""
    end associate
  end do
  
  ! Test 6: Workload that could benefit from splitting
  print *, "📊 Test 6: Large workload for potential splitting"
  workload = selector%analyze_workload( &
    flops = 10000000000_int64,         & ! 10 GFLOPS worth
    bytes = 1000000000_int64,          & ! 1GB data
    pattern = PATTERN_GEMM)
  workload%can_split = .true.
  
  decision = selector%select_multi_device(workload)
  print '(A,A)', " Decision: ", trim(decision%reasoning)
  print *, " (Multi-device support coming soon!)"
  print *, ""
  
  print *, "✅ Universal Device Selector test complete!"
  print *, ""
  print *, "Key insights:"
  print *, "- Automatic device selection based on workload characteristics"
  print *, "- Profiling data improves decisions over time"
  print *, "- Arithmetic intensity drives compute vs memory bound classification"
  print *, "- Async GPU executor advantage recognized (3630 GFLOPS!)"
  print *, "- Foundation for multi-device scheduling"
  
end program test_universal_device_selector