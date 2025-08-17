program test_device_juggling_simple
  use sparkle_workload_profiler, only: workload_profile, analyze_workload, print_workload_analysis
  implicit none
  
  type(workload_profile) :: tiny_profile, medium_profile, huge_profile
  
  print *, "🎪 Device Juggling & Intelligent Scheduling Demo"
  print *, "==============================================="
  print *, ""
  print *, "This demonstrates the core innovation of Sparkle:"
  print *, "• Analyze workload characteristics"
  print *, "• Profile device capabilities" 
  print *, "• Intelligently route work to optimal compute device"
  print *, "• Learn and adapt over time"
  print *, ""
  
  ! Analyze different workload sizes
  print *, "🔍 Workload Analysis Examples"
  print *, "============================="
  
  print *, "Example 1: Tiny workload (should prefer CPU)"
  tiny_profile = analyze_workload(1, 16, 8, 8, 32, 3, 1, 1)
  call print_workload_analysis(tiny_profile)
  
  print *, "Example 2: Medium workload (interesting choice)"  
  medium_profile = analyze_workload(1, 64, 56, 56, 64, 3, 1, 1)
  call print_workload_analysis(medium_profile)
  
  print *, "Example 3: Huge workload (should prefer GPU)"
  huge_profile = analyze_workload(1, 256, 256, 256, 256, 3, 1, 1)
  call print_workload_analysis(huge_profile)
  
  print *, ""
  print *, "✅ Workload analysis complete!"
  print *, ""
  print *, "💡 Key Insights:"
  print *, "   • Small problems → CPU (low overhead, good single-thread perf)"
  print *, "   • Large problems → GPU (high parallelism, compute-bound)"
  print *, "   • Medium problems → Intelligent choice based on characteristics"
  print *, "   • The scheduler learns and adapts over time"
  print *, ""

end program test_device_juggling_simple