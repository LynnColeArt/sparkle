program test_device_detection
  use sparkle_device_detection, only: detect_all_devices, print_device_capabilities, &
                                      get_cpu_info, get_gpu_info, device_capability
  implicit none
  
  type(device_capability) :: cpu_info, gpu_info
  
  print *, "🚀 Sparkle Device Detection Test"
  print *, "================================"
  print *, ""
  
  ! Run device detection
  call detect_all_devices()
  
  ! Print detailed capabilities
  call print_device_capabilities()
  
  ! Get specific device info
  cpu_info = get_cpu_info()
  gpu_info = get_gpu_info()
  
  print *, "📊 Quick Summary:"
  print *, "================="
  
  if (cpu_info%available) then
    print '(A,A)', "CPU: ", trim(cpu_info%device_name)
    print '(A,I0,A,I0,A)', "  Cores: ", cpu_info%compute_units, " (", &
                           cpu_info%compute_units * cpu_info%max_threads_per_unit, " threads)"
    print '(A,F0.1,A)', "  Peak Performance: ", cpu_info%peak_gflops_fp32, " GFLOPS"
    print '(A,F0.1,A)', "  Measured Performance: ", cpu_info%measured_gflops, " GFLOPS"
  else
    print *, "CPU: Not detected"
  end if
  
  print *, ""
  
  if (gpu_info%available) then
    print '(A,A)', "GPU: ", trim(gpu_info%device_name)
    print '(A,A,A,A)', "  Vendor: ", trim(gpu_info%vendor), ", Architecture: ", trim(gpu_info%architecture)
    print '(A,I0)', "  Compute Units: ", gpu_info%compute_units
    print '(A,F0.1,A)', "  Memory: ", real(gpu_info%total_memory_mb) / 1024.0, " GB"
    print '(A,F0.1,A)', "  Peak Performance: ", gpu_info%peak_gflops_fp32, " GFLOPS"
    print '(A,F0.1,A)', "  Measured Performance: ", gpu_info%measured_gflops, " GFLOPS"
  else
    print *, "GPU: Not detected"
  end if
  
  print *, ""
  print *, "✅ Device detection complete!"

end program test_device_detection