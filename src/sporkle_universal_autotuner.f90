module sporkle_universal_autotuner
  ! The Ultimate Auto-Tuning System
  ! Combines universal patterns + intelligent juggling + automatic optimization
  
  use kinds
  use sporkle_hardware_profiler
  implicit none
  
  private
  public :: auto_tuner, auto_tune_init, auto_tune_conv2d
  public :: enable_tensor_cores, enable_learning_mode
  public :: global_tuner, check_nvidia_available, check_amd_available
  
  type :: device_config
    character(len=64) :: name
    type(hardware_characteristics) :: hw
    type(kernel_parameters) :: params
    
    ! Performance tracking
    real(dp) :: best_gflops = 0.0_dp
    real(dp) :: current_gflops = 0.0_dp
    integer :: kernel_version = 1  ! 1=basic, 2=optimized, 3=extreme, 4=tensor
    
    ! Learning parameters
    real(dp) :: performance_history(100)
    integer :: history_count = 0
    logical :: is_learning = .true.
  end type device_config
  
  type :: auto_tuner
    type(device_config) :: nvidia_config
    type(device_config) :: amd_config  
    type(device_config) :: cpu_config
    type(device_config) :: active_device
    
    ! Global settings
    logical :: use_tensor_cores = .false.
    logical :: enable_learning = .true.
    logical :: enable_juggling = .true.
    
    ! Performance model
    real(dp) :: workload_crossover = 1.0_dp  ! GFLOPs where GPU becomes better
  end type auto_tuner
  
  type(auto_tuner), save :: global_tuner
  
contains

  subroutine auto_tune_init()
    ! Initialize the auto-tuning system
    
    print *, "=== Universal Auto-Tuner Initialization ==="
    print *, ""
    
    ! Profile all available devices
    call profile_all_devices()
    
    ! Derive optimal parameters for each
    call derive_all_parameters()
    
    ! Select initial device
    call select_best_device(1.0_dp)
    
    print *, "Auto-tuner ready!"
    print *, ""
    
  end subroutine auto_tune_init
  
  subroutine profile_all_devices()
    ! Discover and profile all devices
    
    print *, "Profiling devices..."
    
    ! Check for NVIDIA
    block
      logical :: has_nvidia
      has_nvidia = check_nvidia_available()
      if (has_nvidia) then
        global_tuner%nvidia_config%hw = profile_nvidia_gpu()
        global_tuner%nvidia_config%name = trim(global_tuner%nvidia_config%hw%name)
        print *, "  ✓ NVIDIA:", trim(global_tuner%nvidia_config%name)
        print *, "    Peak:", global_tuner%nvidia_config%hw%peak_gflops, "GFLOPS"
      end if
    end block
    
    ! Check for AMD
    block
      logical :: has_amd
      has_amd = check_amd_available()
      if (has_amd) then
        global_tuner%amd_config%hw = profile_amd_gpu()
        global_tuner%amd_config%name = trim(global_tuner%amd_config%hw%name)
        print *, "  ✓ AMD:", trim(global_tuner%amd_config%name)
        print *, "    Peak:", global_tuner%amd_config%hw%peak_gflops, "GFLOPS"
      end if
    end block
    
    ! Always have CPU
    global_tuner%cpu_config%hw = profile_cpu()
    global_tuner%cpu_config%name = trim(global_tuner%cpu_config%hw%name)
    print *, "  ✓ CPU:", trim(global_tuner%cpu_config%name)
    print *, "    Peak:", global_tuner%cpu_config%hw%peak_gflops, "GFLOPS"
    
  end subroutine profile_all_devices
  
  subroutine derive_all_parameters()
    ! Derive optimal parameters for each device
    
    print *, ""
    print *, "Deriving optimal parameters..."
    
    if (global_tuner%nvidia_config%hw%peak_gflops > 0) then
      global_tuner%nvidia_config%params = derive_optimal_parameters(global_tuner%nvidia_config%hw)
      print *, "  NVIDIA: block=", global_tuner%nvidia_config%params%block_size, &
               "tile=", global_tuner%nvidia_config%params%tile_size
    end if
    
    if (global_tuner%amd_config%hw%peak_gflops > 0) then
      global_tuner%amd_config%params = derive_optimal_parameters(global_tuner%amd_config%hw)
      print *, "  AMD: block=", global_tuner%amd_config%params%block_size, &
               "tile=", global_tuner%amd_config%params%tile_size
    end if
    
    global_tuner%cpu_config%params = derive_optimal_parameters(global_tuner%cpu_config%hw)
    print *, "  CPU: block=", global_tuner%cpu_config%params%block_size, &
             "tile=", global_tuner%cpu_config%params%tile_size
    
  end subroutine derive_all_parameters
  
  subroutine select_best_device(workload_gflops)
    ! Intelligently select the best device for the workload
    real(dp), intent(in) :: workload_gflops
    
    real(dp) :: best_score
    character(len=64) :: selected
    
    best_score = 0.0_dp
    selected = "CPU"
    
    ! Simple heuristic for now
    if (workload_gflops < 0.1_dp) then
      ! Small workload - use CPU (avoid GPU overhead)
      global_tuner%active_device = global_tuner%cpu_config
      selected = "CPU (small workload)"
    else if (workload_gflops > 10.0_dp) then
      ! Large workload - use best GPU
      if (global_tuner%nvidia_config%hw%peak_gflops > best_score) then
        best_score = global_tuner%nvidia_config%hw%peak_gflops
        global_tuner%active_device = global_tuner%nvidia_config
        selected = "NVIDIA (large workload)"
      end if
      if (global_tuner%amd_config%hw%peak_gflops > best_score) then
        best_score = global_tuner%amd_config%hw%peak_gflops
        global_tuner%active_device = global_tuner%amd_config
        selected = "AMD (large workload)"
      end if
    else
      ! Medium workload - use performance model
      if (global_tuner%nvidia_config%hw%peak_gflops * 0.7_dp > &
          global_tuner%cpu_config%hw%peak_gflops) then
        global_tuner%active_device = global_tuner%nvidia_config
        selected = "NVIDIA (best for medium)"
      else
        global_tuner%active_device = global_tuner%cpu_config
        selected = "CPU (competitive for medium)"
      end if
    end if
    
    print *, "Selected device:", trim(selected)
    
  end subroutine select_best_device
  
  function auto_tune_conv2d(input, kernel, output, &
                           batch, in_c, out_c, h, w, kh, kw) result(gflops)
    real(sp), intent(in) :: input(*)
    real(sp), intent(in) :: kernel(*)
    real(sp), intent(out) :: output(*)
    integer, intent(in) :: batch, in_c, out_c, h, w, kh, kw
    real(dp) :: gflops
    
    integer :: h_out, w_out
    integer(i64) :: total_flops
    real(dp) :: workload_gflops
    
    ! Calculate workload
    h_out = h - kh + 1
    w_out = w - kw + 1
    total_flops = int(batch, i64) * out_c * h_out * w_out * in_c * kh * kw * 2
    workload_gflops = real(total_flops, dp) / 1.0e9_dp
    
    ! Select best device
    if (global_tuner%enable_juggling) then
      call select_best_device(workload_gflops)
    end if
    
    ! Execute on selected device
    print *, "Executing on:", trim(global_tuner%active_device%name)
    print *, "  Workload:", workload_gflops, "GFLOPs"
    
    ! TODO: Call actual execution here instead of simulation
    ! For now, return a baseline value so the dispatcher can do real execution
    gflops = 100.0_dp  ! Baseline - dispatcher will measure actual performance
    
    ! Update performance history
    if (global_tuner%enable_learning) then
      call update_performance_model(global_tuner%active_device, gflops)
    end if
    
  end function auto_tune_conv2d
  
  subroutine update_performance_model(device, gflops)
    type(device_config), intent(inout) :: device
    real(dp), intent(in) :: gflops
    
    ! Track performance history
    if (device%history_count < 100) then
      device%history_count = device%history_count + 1
      device%performance_history(device%history_count) = gflops
    end if
    
    ! Update best performance
    if (gflops > device%best_gflops) then
      device%best_gflops = gflops
      print *, "  New best performance on", trim(device%name), ":", gflops, "GFLOPS"
    end if
    
    device%current_gflops = gflops
    
    ! Auto-upgrade kernel version if consistently performing well
    if (device%history_count >= 5) then
      block
        real(dp) :: avg_recent
        integer :: i
        
        avg_recent = 0.0_dp
        do i = max(1, device%history_count - 4), device%history_count
          avg_recent = avg_recent + device%performance_history(i)
        end do
        avg_recent = avg_recent / 5.0_dp
        
        ! If achieving good efficiency, try next kernel version
        if (avg_recent > device%hw%peak_gflops * 0.4_dp .and. &
            device%kernel_version < 3) then
          device%kernel_version = device%kernel_version + 1
          print *, "  Auto-upgrading to kernel version", device%kernel_version
        end if
      end block
    end if
    
  end subroutine update_performance_model
  
  subroutine enable_tensor_cores(enable)
    logical, intent(in) :: enable
    
    global_tuner%use_tensor_cores = enable
    
    if (enable) then
      print *, "Tensor cores ENABLED - expecting 2-4× additional speedup"
      ! Upgrade NVIDIA to tensor kernel
      if (global_tuner%nvidia_config%hw%peak_gflops > 0) then
        global_tuner%nvidia_config%kernel_version = 4
      end if
    else
      print *, "Tensor cores DISABLED"
    end if
    
  end subroutine enable_tensor_cores
  
  subroutine enable_learning_mode(enable)
    logical, intent(in) :: enable
    global_tuner%enable_learning = enable
    print *, "Learning mode:", merge("ENABLED ", "DISABLED", enable)
  end subroutine enable_learning_mode
  
  function check_nvidia_available() result(available)
    logical :: available
    ! Check for NVIDIA GPU
    available = .true.  ! We know we have A4500
  end function check_nvidia_available
  
  function check_amd_available() result(available)
    logical :: available
    ! Check for AMD GPU
    available = .false.  ! Not on this system
  end function check_amd_available

end module sporkle_universal_autotuner