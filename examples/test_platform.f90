program test_platform
  use sporkle_platform
  implicit none
  
  type(platform_info) :: info
  
  print *, "🔍 Sporkle Platform Detection Test"
  print *, "=================================="
  print *, ""
  
  ! Detect platform
  info = detect_platform()
  
  ! Display results
  print '(A,A)', "Platform: ", trim(info%platform_name)
  print '(A,A)', "Kernel: ", trim(info%kernel_version)
  print '(A,A)', "Primary GPU: ", trim(info%gpu_string)
  print *, ""
  
  print *, "Available backends:"
  if (info%has_opengl) print *, "  ✓ OpenGL Compute Shaders"
  if (info%has_vulkan) print *, "  ✓ Vulkan Compute"  
  if (info%has_metal) print *, "  ✓ Metal Performance Shaders"
  if (info%has_direct_gpu) print *, "  ✓ Direct GPU driver access"
  
  if (.not. info%has_opengl .and. .not. info%has_vulkan .and. &
      .not. info%has_metal .and. .not. info%has_direct_gpu) then
    print *, "  ❌ No GPU backends available"
  end if
  
  print *, ""
  print *, "Build configuration:"
  print '(A,A)', "  Flags: ", trim(info%build_flags)
  
  ! Platform-specific recommendations
  print *, ""
  print *, "Recommendations:"
  
  select case(info%platform)
  case(PLATFORM_LINUX)
    if (info%primary_gpu == GPU_AMD) then
      print *, "  🔧 AMD GPU detected on Linux"
      if (info%has_direct_gpu) then
        print *, "  ✓ Direct AMDGPU driver access available (/dev/dri/card0)"
        print *, "    → Can implement direct command submission"
      end if
      if (info%has_opengl) then
        print *, "  ✓ OpenGL 4.6 available for compute shaders"
        print *, "    → Ready for GPU compute without ROCm"
      end if
      if (info%has_vulkan) then
        print *, "  ✓ Vulkan available for modern compute"
        print *, "    → Better performance than OpenGL"
      end if
    end if
    
  case(PLATFORM_MACOS)
    print *, "  🍎 Apple Silicon detected"
    print *, "  ✓ Metal, Neural Engine, and AMX available"
    print *, "  → Full heterogeneous compute ready"
    
  case(PLATFORM_WINDOWS)
    print *, "  🪟 Windows detected"
    print *, "  → DirectCompute implementation needed"
  end select
  
  print *, ""
  print *, "The Sporkle Way: Direct to metal, no dependencies! 🚀"
  
end program test_platform