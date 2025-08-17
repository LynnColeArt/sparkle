program test_device_juggling
  use sparkle_intelligent_scheduler, only: device_juggling_demo, scheduler_benchmark
  use sparkle_workload_profiler, only: analyze_workload, print_workload_analysis
  implicit none
  
  print *, "🎪 Device Juggling & Intelligent Scheduling Demo"
  print *, "==============================================="
  print *, ""
  print *, "This demonstrates the core innovation of Sparkle:"
  print *, "• Analyze workload characteristics"
  print *, "• Profile device capabilities" 
  print *, "• Intelligently route work to optimal compute device"
  print *, "• Learn and adapt over time"
  print *, ""
  print *, "The same scheduler will work with:"
  print *, "• CPU cores (8 cores, 16 threads)"
  print *, "• GPU compute units (RDNA3, 451 GFLOPS measured)"
  print *, "• Neural Engine (future: Apple Silicon)"
  print *, "• Custom accelerators (future extensibility)"
  print *, ""
  
  ! Demonstrate workload analysis first
  call demo_workload_analysis()
  
  ! Then show the full device juggling
  call device_juggling_demo()
  
  ! Finally benchmark the scheduler
  call scheduler_benchmark()
  
contains

  subroutine demo_workload_analysis()
    use sparkle_workload_profiler, only: workload_profile, analyze_workload, print_workload_analysis
    
    type(workload_profile) :: tiny_profile, medium_profile, huge_profile
    
    print *, "🔍 Workload Analysis Examples"
    print *, "============================="
    
    ! Analyze different workload sizes
    print *, "Example 1: Tiny workload (should prefer CPU)"
    tiny_profile = analyze_workload(1, 16, 8, 8, 32, 3, 1, 1)
    call print_workload_analysis(tiny_profile)
    
    print *, "Example 2: Medium workload (interesting choice)"  
    medium_profile = analyze_workload(1, 64, 56, 56, 64, 3, 1, 1)
    call print_workload_analysis(medium_profile)
    
    print *, "Example 3: Huge workload (should prefer GPU)"
    huge_profile = analyze_workload(1, 256, 256, 256, 256, 3, 1, 1)
    call print_workload_analysis(huge_profile)
    
  end subroutine demo_workload_analysis

end program test_device_juggling