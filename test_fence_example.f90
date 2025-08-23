program test_fence_example
  ! Example demonstrating fence usage for GPU synchronization
  use iso_c_binding
  use kinds
  use sporkle_fence
  use pm4_fence_binding
  use device_capabilities
  use hardware_detection
  implicit none
  
  type(cpu_info_t) :: cpu_info
  type(gpu_info_t) :: gpu_info
  type(device_caps_t) :: cpu_caps, gpu_caps
  type(sporkle_fence_t) :: cpu_fence
  type(pm4_fence_t) :: gpu_fence
  logical :: fence_ready
  integer :: i
  
  print *, "=== Sparkle Fence Example ==="
  print *, ""
  
  ! Detect hardware
  print *, "Detecting hardware..."
  cpu_info = detect_cpu_info()
  gpu_info = detect_gpu_info(0)
  
  print *, "CPU: ", trim(cpu_info%name)
  print '(A,I0,A,I0,A)', "  Cores: ", cpu_info%num_cores, &
          ", Threads: ", cpu_info%num_threads
  print '(A,F5.2,A)', "  Clock: ", cpu_info%clock_ghz, " GHz"
  print *, ""
  
  if (gpu_info%vendor_id == int(z'1002')) then
    print *, "GPU: ", trim(gpu_info%name)
    print '(A,I0,A,I0)', "  CUs: ", gpu_info%num_cus, &
            ", Wave size: ", gpu_info%wave_size
    print '(A,F5.2,A)', "  Clock: ", gpu_info%clock_ghz, " GHz"
  else
    print *, "No AMD GPU detected"
  end if
  print *, ""
  
  ! Get capabilities
  cpu_caps = get_cpu_capabilities()
  print '(A,F8.1,A)', "CPU Peak: ", cpu_caps%peak_gflops, " GFLOPS"
  
  if (gpu_info%vendor_id == int(z'1002')) then
    gpu_caps = get_gpu_capabilities(0)
    print '(A,F8.1,A)', "GPU Peak: ", gpu_caps%peak_gflops, " GFLOPS"
  end if
  print *, ""
  
  ! Demonstrate CPU fence (software)
  print *, "Testing CPU fence (software implementation)..."
  cpu_fence = fence_create(initial_value=0_i64)
  
  ! Simulate async work with fence
  print *, "  Starting async work..."
  
  ! In real code, this would be in another thread
  block
    integer :: work_cycles
    work_cycles = 5
    
    do i = 1, work_cycles
      print '(A,I0,A)', "  Working... ", i, "/5"
      call execute_command_line("sleep 0.2", wait=.true.)
      
      ! Check if someone is waiting
      if (i == 3) then
        print *, "  Signaling fence..."
        call fence_signal(cpu_fence, int(i, i64))
      end if
    end do
  end block
  
  ! Wait for fence
  print *, "  Waiting for fence (should be already signaled)..."
  if (fence_wait(cpu_fence, 1000000000_i64) == 0) then
    print *, "  ✅ Fence signaled!"
  else
    print *, "  ❌ Fence timeout"
  end if
  
  call fence_destroy(cpu_fence)
  print *, ""
  
  ! Demonstrate optimal tile size selection
  print *, "Optimal tile sizes for 1024x1024 matrix multiply:"
  block
    integer :: m, n, k
    integer :: tile_m, tile_n, tile_k
    
    m = 1024
    n = 1024
    k = 1024
    
    print *, "  CPU:"
    call select_tile_sizes(cpu_caps, m, n, k, tile_m, tile_n, tile_k)
    print '(A,I0,A,I0,A,I0)', "    Tiles: ", tile_m, " x ", tile_n, " x ", tile_k
    print '(A,I0,A)', "    Working set: ", &
          (tile_m * tile_k + tile_k * tile_n + tile_m * tile_n) * 4 / 1024, " KB"
    
    if (gpu_info%vendor_id == int(z'1002')) then
      print *, "  GPU:"
      call select_tile_sizes(gpu_caps, m, n, k, tile_m, tile_n, tile_k)
      print '(A,I0,A,I0,A,I0)', "    Tiles: ", tile_m, " x ", tile_n, " x ", tile_k
      
      ! Workgroup size
      block
        integer :: workgroup_size
        workgroup_size = select_workgroup_size(gpu_caps, m * n)
        print '(A,I0)', "    Workgroup size: ", workgroup_size
      end block
    end if
  end block
  
  print *, ""
  print *, "✅ Test complete - universal hardware detection working!"
  
end program test_fence_example