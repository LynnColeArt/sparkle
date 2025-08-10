program test_gpu_backends
  use iso_fortran_env, only: int32, int64
  use sparkle_gpu_backend
  use sparkle_gpu_backend_detect
  use sparkle_gpu_safe_detect
  implicit none
  
  type(gpu_backend_info), allocatable :: backends(:)
  type(gpu_info_type), allocatable :: physical_gpus(:)
  type(opengl_info) :: gl_info
  character(len=32) :: version
  integer :: i
  logical :: any_available
  
  print *, "🔍 Sparkle GPU Backend Detection Test"
  print *, "====================================="
  print *, ""
  
  ! First detect physical GPUs
  print *, "Physical GPU Detection:"
  print *, "----------------------"
  physical_gpus = detect_gpu_safe()
  
  if (size(physical_gpus) == 0) then
    print *, "❌ No physical GPUs detected"
  else
    do i = 1, size(physical_gpus)
      print '(A,I0,A)', "GPU ", i, ":"
      print '(A,A)', "  Vendor: ", trim(physical_gpus(i)%vendor)
      print '(A,A)', "  Device: ", trim(physical_gpus(i)%device)
      print '(A,A)', "  Bus ID: ", trim(physical_gpus(i)%bus_id)
      print *, ""
    end do
  end if
  
  ! Detect available backends
  print *, "Backend Detection:"
  print *, "-----------------"
  backends = detect_gpu_backend()
  
  any_available = .false.
  do i = 1, size(backends)
    print '(A,A)', trim(backends(i)%name), ":"
    
    if (backends(i)%available) then
      print *, "  ✅ Available"
      print '(A,A)', "  Version: ", trim(backends(i)%version)
      any_available = .true.
    else
      print *, "  ❌ Not available"
      print '(A,A)', "  Reason: ", trim(backends(i)%reason)
    end if
    
    ! Additional checks for each backend
    select case(backends(i)%backend_type)
    case(GPU_BACKEND_OPENGL)
      gl_info = detect_opengl_version()
      if (gl_info%major > 0) then
        print '(A,I0,A,I0)', "  OpenGL Version: ", gl_info%major, ".", gl_info%minor
        print '(A,L)', "  Compute Shaders: ", gl_info%has_compute
        if (gl_info%has_compute) then
          print '(A,3(I0,1X))', "  Max Work Groups: ", gl_info%max_compute_groups
          print '(A,I0)', "  Max Invocations: ", gl_info%max_compute_invocations
        end if
      end if
      
    case(GPU_BACKEND_VULKAN)
      version = detect_vulkan_version()
      if (version /= "Unknown") then
        print '(A,A)', "  Detected: ", trim(version)
      end if
      
    case(GPU_BACKEND_ROCM)
      version = detect_rocm_version()
      if (version /= "Unknown") then
        print '(A,A)', "  ROCm Version: ", trim(version)
      end if
      
    case(GPU_BACKEND_CUDA)
      version = detect_cuda_version()
      if (version /= "Unknown") then
        print '(A,A)', "  ", trim(version)
      end if
    end select
    
    print *, ""
  end do
  
  ! Summary
  print *, "Summary:"
  print *, "--------"
  if (any_available) then
    print *, "🎉 At least one GPU backend is available!"
    print *, "   Sparkle can potentially use GPU acceleration."
  else
    print *, "😕 No GPU backends are currently available."
    print *, "   Sparkle will fall back to CPU execution."
  end if
  print *, ""
  
  ! Recommendations
  print *, "Recommendations:"
  print *, "----------------"
  if (size(physical_gpus) > 0 .and. .not. any_available) then
    print *, "⚠️  You have GPU hardware but no compute backends!"
    print *, "   Consider installing:"
    
    if (physical_gpus(1)%is_amd) then
      print *, "   - ROCm for native AMD compute"
      print *, "   - Mesa with OpenGL 4.3+ for compute shaders"
    else if (physical_gpus(1)%is_nvidia) then
      print *, "   - CUDA toolkit for native NVIDIA compute"
      print *, "   - Latest NVIDIA drivers with OpenGL 4.3+"
    else if (physical_gpus(1)%is_intel) then
      print *, "   - Intel oneAPI toolkit"
      print *, "   - Mesa with OpenGL 4.3+ for compute shaders"
    end if
  else if (size(physical_gpus) == 0) then
    print *, "💡 No GPU detected - CPU execution will be used"
    print *, "   This is fine for development and testing!"
  end if
  
  print *, ""
  print *, "The Sparkle Way: Use what you have! ✨"
  
  ! Cleanup
  if (allocated(backends)) deallocate(backends)
  if (allocated(physical_gpus)) deallocate(physical_gpus)
  
end program test_gpu_backends