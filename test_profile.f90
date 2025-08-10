program test_profile
  use iso_fortran_env, only: int64, real64
  use sparkle_mesh_types, only: mesh_topology, KIND_CPU
  use sparkle_discovery
  use sparkle_profile
  implicit none
  
  type(mesh_topology) :: mesh
  type(profile_result), allocatable :: profiles(:)
  integer :: i
  
  print *, "=== Sparkle Device Profiling Test ==="
  print *, ""
  
  ! Discover devices
  mesh = scan_devices()
  
  ! Test 1: Quick profile of all devices
  print *, "Test 1: Quick profiling (should take ~1 second per device)"
  profiles = profile_all_devices(mesh, PROFILE_QUICK)
  print *, ""
  
  ! Test 2: Detailed profile of first device
  if (mesh%num_devices > 0) then
    print *, "Test 2: Standard profiling of first device"
    block
      type(device_profiler) :: profiler
      type(profile_result) :: detailed_profile
      
      call profiler%init(0, "CPU-0")
      
      if (mesh%devices(1)%caps%kind == KIND_CPU) then
        detailed_profile = profiler%profile(mesh%devices(1), PROFILE_STANDARD)
        
        print *, ""
        print *, "Detailed Profile Results:"
        print '(A,F0.2,A)', "  Sequential bandwidth: ", &
              detailed_profile%bandwidth_sequential_gb_s, " GB/s"
        print '(A,F0.2,A)', "  Random bandwidth: ", &
              detailed_profile%bandwidth_random_gb_s, " GB/s"
        print '(A,E12.5,A)', "  Single precision: ", &
              detailed_profile%flops_single, " FLOPS"
        print '(A,E12.5,A)', "  Double precision: ", &
              detailed_profile%flops_double, " FLOPS"
        print '(A,F0.2,A)', "  Power efficiency: ", &
              detailed_profile%power_efficiency, " GFLOPS/Watt"
        print '(A,F0.1,A)', "  Efficiency score: ", &
              detailed_profile%efficiency_score, "/100"
        print '(A,F0.2,A)', "  Profile duration: ", &
              detailed_profile%profile_duration_sec, " seconds"
      end if
    end block
  end if
  
  print *, ""
  print *, "Test 3: Using profile data for scheduling decisions"
  if (allocated(profiles)) then
    block
      real(real64) :: total_compute_power
      real(real64), allocatable :: device_weights(:)
      
      allocate(device_weights(size(profiles)))
      total_compute_power = 0.0_real64
      
      ! Calculate relative compute power
      do i = 1, size(profiles)
        if (profiles(i)%is_valid) then
          device_weights(i) = profiles(i)%flops_double
          total_compute_power = total_compute_power + device_weights(i)
        else
          device_weights(i) = 0.0_real64
        end if
      end do
      
      ! Show work distribution based on actual performance
      if (total_compute_power > 0.0_real64) then
        print *, "Work distribution based on measured performance:"
        do i = 1, size(profiles)
          if (profiles(i)%is_valid) then
            print '(A,I0,A,F0.1,A)', "  Device ", i-1, " should get ", &
                  (device_weights(i) / total_compute_power) * 100.0_real64, &
                  "% of work"
          end if
        end do
      end if
    end block
  end if
  
  print *, ""
  print *, "=== Device profiling tests complete ==="
  print *, ""
  print *, "💡 Profile data helps Sparkle make intelligent scheduling decisions!"
  
end program test_profile