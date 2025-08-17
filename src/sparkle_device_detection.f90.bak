! Device Capability Detection System - Universal hardware discovery
! Detects and profiles all available compute devices for intelligent scheduling

module sparkle_device_detection
  use iso_fortran_env, only: real32, int32, int64
  implicit none
  
  private
  public :: device_capability, detect_all_devices, print_device_capabilities
  public :: get_cpu_info, get_gpu_info, get_neural_engine_info
  
  ! Device capability structure
  type :: device_capability
    character(len=32) :: device_name
    character(len=16) :: device_type        ! "cpu", "gpu", "neural", "accelerator"
    character(len=32) :: vendor             ! "amd", "nvidia", "intel", "apple"
    character(len=32) :: architecture       ! "rdna3", "ada_lovelace", "apple_m2", etc.
    
    ! Compute specifications
    integer :: compute_units               ! CUs, SMs, cores, etc.
    integer :: max_threads_per_unit
    real(real32) :: base_clock_mhz
    real(real32) :: boost_clock_mhz
    
    ! Memory specifications  
    integer(int64) :: total_memory_mb
    integer(int64) :: shared_memory_kb
    real(real32) :: memory_bandwidth_gbps
    
    ! Performance characteristics
    real(real32) :: peak_gflops_fp32
    real(real32) :: measured_gflops        ! Actual measured performance
    real(real32) :: efficiency_rating      ! 0.0-1.0 based on workload suitability
    
    ! Availability and features
    logical :: available
    logical :: supports_async
    logical :: supports_fp16
    logical :: supports_int8
    character(len=64) :: feature_flags
    
    ! Framework integration
    character(len=32) :: backend_module     ! Which module handles this device
    real(real32) :: overhead_ms            ! Framework overhead for this device
  end type
  
  ! Global device registry
  integer, parameter :: MAX_DEVICES = 16
  type(device_capability) :: detected_devices(MAX_DEVICES)
  integer :: num_detected_devices = 0
  logical :: detection_complete = .false.
  
contains

  ! Main device detection entry point
  subroutine detect_all_devices()
    logical :: cpu_detected, gpu_detected, neural_detected
    
    print *, "🔍 Sparkle Device Detection System"
    print *, "=================================="
    
    num_detected_devices = 0
    
    ! Detect CPU capabilities
    cpu_detected = detect_cpu_devices()
    if (cpu_detected) print *, "✅ CPU devices detected"
    
    ! Detect GPU capabilities (AMD, NVIDIA, Intel)
    gpu_detected = detect_gpu_devices()
    if (gpu_detected) print *, "✅ GPU devices detected"
    
    ! Detect Neural Engine / AI accelerators
    neural_detected = detect_neural_devices()
    if (neural_detected) print *, "✅ Neural processing devices detected"
    
    ! Detect other accelerators (FPGAs, custom ASICs, etc.)
    call detect_accelerator_devices()
    
    detection_complete = .true.
    
    print '(A,I0,A)', "📊 Total devices detected: ", num_detected_devices, ""
    print *, ""
    
  end subroutine detect_all_devices
  
  ! CPU detection and profiling
  function detect_cpu_devices() result(detected)
    logical :: detected
    type(device_capability) :: cpu_device
    
    detected = .false.
    
    ! Detect CPU architecture and capabilities
    cpu_device%device_name = "System CPU"
    cpu_device%device_type = "cpu"
    cpu_device%vendor = get_cpu_vendor()
    cpu_device%architecture = get_cpu_architecture()
    
    ! Get CPU specifications
    cpu_device%compute_units = get_cpu_core_count()
    cpu_device%max_threads_per_unit = get_cpu_threads_per_core()
    cpu_device%base_clock_mhz = get_cpu_base_frequency()
    cpu_device%boost_clock_mhz = get_cpu_boost_frequency()
    
    ! Memory characteristics
    cpu_device%total_memory_mb = get_system_memory_mb()
    cpu_device%shared_memory_kb = get_cpu_cache_size_kb()
    cpu_device%memory_bandwidth_gbps = get_memory_bandwidth()
    
    ! Performance estimation
    cpu_device%peak_gflops_fp32 = estimate_cpu_peak_gflops()
    cpu_device%measured_gflops = 25.0  ! From our Layer 4 measurements
    cpu_device%efficiency_rating = 0.85
    
    ! Feature support
    cpu_device%available = .true.
    cpu_device%supports_async = .true.  ! Via OpenMP
    cpu_device%supports_fp16 = check_cpu_fp16_support()
    cpu_device%supports_int8 = .true.
    cpu_device%feature_flags = get_cpu_feature_flags()
    
    ! Framework integration
    cpu_device%backend_module = "cpu_conv2d_reference"
    cpu_device%overhead_ms = 0.1  ! Minimal overhead for CPU
    
    ! Add to device registry
    call add_device_to_registry(cpu_device)
    detected = .true.
    
  end function detect_cpu_devices
  
  ! GPU detection (AMD, NVIDIA, Intel)
  function detect_gpu_devices() result(detected)
    logical :: detected
    logical :: amd_found, nvidia_found, intel_found
    
    detected = .false.
    
    ! Detect AMD GPUs
    amd_found = detect_amd_gpus()
    if (amd_found) detected = .true.
    
    ! Detect NVIDIA GPUs  
    nvidia_found = detect_nvidia_gpus()
    if (nvidia_found) detected = .true.
    
    ! Detect Intel GPUs
    intel_found = detect_intel_gpus()
    if (intel_found) detected = .true.
    
  end function detect_gpu_devices
  
  ! AMD GPU detection
  function detect_amd_gpus() result(detected)
    logical :: detected
    type(device_capability) :: amd_gpu
    
    detected = .false.
    
    ! Check for AMD GPU via OpenGL/EGL
    if (.not. check_amd_gpu_available()) return
    
    amd_gpu%device_name = get_amd_gpu_name()
    amd_gpu%device_type = "gpu"
    amd_gpu%vendor = "amd"
    amd_gpu%architecture = get_amd_gpu_architecture()  ! "rdna3", "rdna2", etc.
    
    ! AMD-specific specifications
    amd_gpu%compute_units = get_amd_compute_units()
    amd_gpu%max_threads_per_unit = 64  ! Wavefront size
    amd_gpu%base_clock_mhz = get_amd_base_clock()
    amd_gpu%boost_clock_mhz = get_amd_boost_clock()
    
    ! Memory specifications
    amd_gpu%total_memory_mb = get_amd_vram_mb()
    amd_gpu%shared_memory_kb = 64  ! LDS per CU
    amd_gpu%memory_bandwidth_gbps = get_amd_memory_bandwidth()
    
    ! Performance characteristics
    amd_gpu%peak_gflops_fp32 = estimate_amd_peak_gflops()
    amd_gpu%measured_gflops = 451.0  ! From our Layer 3 measurements
    amd_gpu%efficiency_rating = 0.92
    
    ! Feature support
    amd_gpu%available = .true.
    amd_gpu%supports_async = .true.
    amd_gpu%supports_fp16 = .true.  ! RDNA supports FP16
    amd_gpu%supports_int8 = .true.
    amd_gpu%feature_flags = "rdna_optimized,async_compute,fp16_accel"
    
    ! Framework integration
    amd_gpu%backend_module = "gpu_opengl_reference"
    amd_gpu%overhead_ms = 1.0  ! Transfer + setup overhead
    
    call add_device_to_registry(amd_gpu)
    detected = .true.
    
  end function detect_amd_gpus
  
  ! NVIDIA GPU detection
  function detect_nvidia_gpus() result(detected)
    logical :: detected
    type(device_capability) :: nvidia_gpu
    
    detected = .false.
    
    ! Check for NVIDIA GPU via CUDA/OpenGL
    if (.not. check_nvidia_gpu_available()) return
    
    nvidia_gpu%device_name = get_nvidia_gpu_name()
    nvidia_gpu%device_type = "gpu"
    nvidia_gpu%vendor = "nvidia"
    nvidia_gpu%architecture = get_nvidia_gpu_architecture()  ! "ada_lovelace", "ampere", etc.
    
    ! NVIDIA-specific specifications
    nvidia_gpu%compute_units = get_nvidia_sm_count()
    nvidia_gpu%max_threads_per_unit = 32  ! Warp size
    nvidia_gpu%base_clock_mhz = get_nvidia_base_clock()
    nvidia_gpu%boost_clock_mhz = get_nvidia_boost_clock()
    
    ! Memory specifications
    nvidia_gpu%total_memory_mb = get_nvidia_vram_mb()
    nvidia_gpu%shared_memory_kb = 48  ! Shared memory per SM (varies by arch)
    nvidia_gpu%memory_bandwidth_gbps = get_nvidia_memory_bandwidth()
    
    ! Performance characteristics  
    nvidia_gpu%peak_gflops_fp32 = estimate_nvidia_peak_gflops()
    nvidia_gpu%measured_gflops = 0.0  ! To be measured
    nvidia_gpu%efficiency_rating = 0.90
    
    ! Feature support
    nvidia_gpu%available = .true.
    nvidia_gpu%supports_async = .true.
    nvidia_gpu%supports_fp16 = check_nvidia_fp16_support()
    nvidia_gpu%supports_int8 = check_nvidia_int8_support()
    nvidia_gpu%feature_flags = "cuda_compatible,tensor_cores,async_compute"
    
    ! Framework integration
    nvidia_gpu%backend_module = "gpu_nvidia_reference"  ! To be implemented
    nvidia_gpu%overhead_ms = 1.2  ! Slightly higher than AMD due to CUDA overhead
    
    call add_device_to_registry(nvidia_gpu)
    detected = .true.
    
  end function detect_nvidia_gpus
  
  ! Intel GPU detection
  function detect_intel_gpus() result(detected)
    logical :: detected
    type(device_capability) :: intel_gpu
    
    detected = .false.
    
    ! Check for Intel GPU (Arc, Xe, UHD)
    if (.not. check_intel_gpu_available()) return
    
    intel_gpu%device_name = get_intel_gpu_name()
    intel_gpu%device_type = "gpu"
    intel_gpu%vendor = "intel"
    intel_gpu%architecture = get_intel_gpu_architecture()  ! "xe_hpg", "xe_lp", etc.
    
    ! Intel-specific specifications
    intel_gpu%compute_units = get_intel_eu_count()
    intel_gpu%max_threads_per_unit = 8   ! EU thread count
    intel_gpu%base_clock_mhz = get_intel_base_clock()
    intel_gpu%boost_clock_mhz = get_intel_boost_clock()
    
    ! Memory specifications
    intel_gpu%total_memory_mb = get_intel_vram_mb()
    intel_gpu%shared_memory_kb = 64  ! Shared local memory
    intel_gpu%memory_bandwidth_gbps = get_intel_memory_bandwidth()
    
    ! Performance characteristics
    intel_gpu%peak_gflops_fp32 = estimate_intel_peak_gflops()
    intel_gpu%measured_gflops = 0.0  ! To be measured
    intel_gpu%efficiency_rating = 0.75  ! Conservative estimate
    
    ! Feature support
    intel_gpu%available = .true.
    intel_gpu%supports_async = .true.
    intel_gpu%supports_fp16 = .true.
    intel_gpu%supports_int8 = .true.
    intel_gpu%feature_flags = "oneapi_compatible,dp4a_support"
    
    ! Framework integration
    intel_gpu%backend_module = "gpu_intel_reference"  ! To be implemented
    intel_gpu%overhead_ms = 1.5  ! Higher overhead due to less mature drivers
    
    call add_device_to_registry(intel_gpu)
    detected = .true.
    
  end function detect_intel_gpus
  
  ! Neural Engine / AI accelerator detection
  function detect_neural_devices() result(detected)
    logical :: detected
    
    detected = .false.
    
    ! Detect Apple Neural Engine
    if (detect_apple_neural_engine()) detected = .true.
    
    ! Detect other AI accelerators (TPU, IPU, etc.)
    if (detect_ai_accelerators()) detected = .true.
    
  end function detect_neural_devices
  
  ! Apple Neural Engine detection
  function detect_apple_neural_engine() result(detected)
    logical :: detected
    type(device_capability) :: neural_engine
    
    detected = .false.
    
    ! Check for Apple Silicon
    if (.not. check_apple_neural_engine_available()) return
    
    neural_engine%device_name = "Apple Neural Engine"
    neural_engine%device_type = "neural"
    neural_engine%vendor = "apple"
    neural_engine%architecture = get_apple_chip_generation()  ! "m1", "m2", "m3", etc.
    
    ! Neural Engine specifications
    neural_engine%compute_units = get_neural_engine_core_count()
    neural_engine%max_threads_per_unit = 1  ! Specialized units
    neural_engine%base_clock_mhz = 0.0  ! Not applicable
    neural_engine%boost_clock_mhz = 0.0
    
    ! Memory characteristics
    neural_engine%total_memory_mb = 0  ! Shares system memory
    neural_engine%shared_memory_kb = 0  ! Internal caches
    neural_engine%memory_bandwidth_gbps = get_unified_memory_bandwidth()
    
    ! Performance characteristics
    neural_engine%peak_gflops_fp32 = 0.0  ! Specialized for ML ops
    neural_engine%measured_gflops = 0.0   ! Different metric needed
    neural_engine%efficiency_rating = 0.95  ! Very efficient for ML
    
    ! Feature support
    neural_engine%available = .true.
    neural_engine%supports_async = .true.
    neural_engine%supports_fp16 = .true.
    neural_engine%supports_int8 = .true.
    neural_engine%feature_flags = "coreml_optimized,ml_compute,low_power"
    
    ! Framework integration
    neural_engine%backend_module = "apple_neural_reference"
    neural_engine%overhead_ms = 0.5  ! Low overhead
    
    call add_device_to_registry(neural_engine)
    detected = .true.
    
  end function detect_apple_neural_engine
  
  ! Other AI accelerator detection
  function detect_ai_accelerators() result(detected)
    logical :: detected
    
    detected = .false.
    ! TODO: Implement detection for:
    ! - Google TPU
    ! - Intel Neural Compute Stick
    ! - Graphcore IPU
    ! - Qualcomm AI Engine
    ! - Custom FPGA accelerators
    
  end function detect_ai_accelerators
  
  ! Generic accelerator detection
  subroutine detect_accelerator_devices()
    ! TODO: Implement detection for:
    ! - FPGA devices
    ! - Custom ASICs
    ! - Quantum processing units
    ! - Neuromorphic chips
  end subroutine detect_accelerator_devices
  
  ! Add device to registry
  subroutine add_device_to_registry(device)
    type(device_capability), intent(in) :: device
    
    if (num_detected_devices < MAX_DEVICES) then
      num_detected_devices = num_detected_devices + 1
      detected_devices(num_detected_devices) = device
    else
      print *, "Warning: Maximum device limit reached, device not registered"
    end if
  end subroutine add_device_to_registry
  
  ! Print comprehensive device capabilities
  subroutine print_device_capabilities()
    integer :: i
    
    if (.not. detection_complete) then
      call detect_all_devices()
    end if
    
    print *, ""
    print *, "🖥️  Detected Device Capabilities"
    print *, "================================"
    
    do i = 1, num_detected_devices
      call print_single_device(detected_devices(i))
      print *, ""
    end do
    
  end subroutine print_device_capabilities
  
  ! Print single device information
  subroutine print_single_device(device)
    type(device_capability), intent(in) :: device
    
    print '(A,A)', "Device: ", trim(device%device_name)
    print '(A,A,A,A)', "Type: ", trim(device%device_type), " (", trim(device%vendor), ")"
    print '(A,A)', "Architecture: ", trim(device%architecture)
    print '(A,I0)', "Compute Units: ", device%compute_units
    print '(A,F0.1,A)', "Peak Performance: ", device%peak_gflops_fp32, " GFLOPS"
    if (device%measured_gflops > 0.0) then
      print '(A,F0.1,A)', "Measured Performance: ", device%measured_gflops, " GFLOPS"
    end if
    print '(A,I0,A)', "Memory: ", device%total_memory_mb, " MB"
    print '(A,A)', "Backend: ", trim(device%backend_module)
    print '(A,L1)', "Available: ", device%available
    
  end subroutine print_single_device
  
  ! Accessor functions for detected devices
  function get_cpu_info() result(cpu_device)
    type(device_capability) :: cpu_device
    integer :: i
    
    ! Find CPU device in registry
    do i = 1, num_detected_devices
      if (detected_devices(i)%device_type == "cpu") then
        cpu_device = detected_devices(i)
        return
      end if
    end do
    
    ! Return empty device if not found
    cpu_device%available = .false.
  end function get_cpu_info
  
  function get_gpu_info() result(gpu_device)
    type(device_capability) :: gpu_device
    integer :: i
    
    ! Find first available GPU device
    do i = 1, num_detected_devices
      if (detected_devices(i)%device_type == "gpu" .and. &
          detected_devices(i)%available) then
        gpu_device = detected_devices(i)
        return
      end if
    end do
    
    gpu_device%available = .false.
  end function get_gpu_info
  
  function get_neural_engine_info() result(neural_device)
    type(device_capability) :: neural_device
    integer :: i
    
    ! Find neural processing device
    do i = 1, num_detected_devices
      if (detected_devices(i)%device_type == "neural") then
        neural_device = detected_devices(i)
        return
      end if
    end do
    
    neural_device%available = .false.
  end function get_neural_engine_info
  
  ! Hardware detection helper functions (platform-specific implementations needed)
  
  function get_cpu_vendor() result(vendor)
    character(len=32) :: vendor
    ! TODO: Implement CPU vendor detection
    vendor = "unknown"
  end function get_cpu_vendor
  
  function get_cpu_architecture() result(arch)
    character(len=32) :: arch
    ! TODO: Implement CPU architecture detection
    arch = "x86_64"
  end function get_cpu_architecture
  
  function get_cpu_core_count() result(cores)
    integer :: cores
    ! TODO: Implement core count detection
    cores = 8  ! Default from our system
  end function get_cpu_core_count
  
  function get_cpu_threads_per_core() result(threads)
    integer :: threads
    ! TODO: Implement thread detection
    threads = 2  ! Hyperthreading
  end function get_cpu_threads_per_core
  
  function get_cpu_base_frequency() result(freq)
    real(real32) :: freq
    ! TODO: Implement frequency detection
    freq = 3800.0  ! MHz
  end function get_cpu_base_frequency
  
  function get_cpu_boost_frequency() result(freq)
    real(real32) :: freq
    ! TODO: Implement boost frequency detection
    freq = 5400.0  ! MHz
  end function get_cpu_boost_frequency
  
  function get_system_memory_mb() result(memory)
    integer(int64) :: memory
    ! TODO: Implement memory detection
    memory = 32768  ! 32GB
  end function get_system_memory_mb
  
  function get_cpu_cache_size_kb() result(cache)
    integer(int64) :: cache
    ! TODO: Implement cache size detection
    cache = 32768   ! L3 cache
  end function get_cpu_cache_size_kb
  
  function get_memory_bandwidth() result(bandwidth)
    real(real32) :: bandwidth
    ! TODO: Implement bandwidth measurement
    bandwidth = 51.2  ! GB/s
  end function get_memory_bandwidth
  
  function estimate_cpu_peak_gflops() result(gflops)
    real(real32) :: gflops
    ! TODO: Implement peak GFLOPS estimation
    gflops = 350.0  ! Theoretical peak
  end function estimate_cpu_peak_gflops
  
  function check_cpu_fp16_support() result(supported)
    logical :: supported
    ! TODO: Implement FP16 capability check
    supported = .false.
  end function check_cpu_fp16_support
  
  function get_cpu_feature_flags() result(flags)
    character(len=64) :: flags
    ! TODO: Implement feature flag detection
    flags = "avx512,openmp,simd"
  end function get_cpu_feature_flags
  
  ! GPU detection helper functions (to be implemented)
  function check_amd_gpu_available() result(available)
    logical :: available
    available = .true.  ! Assume available for now
  end function check_amd_gpu_available
  
  function get_amd_gpu_name() result(name)
    character(len=32) :: name
    name = "AMD Radeon RX 7900 XT"
  end function get_amd_gpu_name
  
  function get_amd_gpu_architecture() result(arch)
    character(len=32) :: arch
    arch = "rdna3"
  end function get_amd_gpu_architecture
  
  function get_amd_compute_units() result(cus)
    integer :: cus
    cus = 84  ! RX 7900 XT
  end function get_amd_compute_units
  
  function get_amd_base_clock() result(clock)
    real(real32) :: clock
    clock = 1500.0  ! MHz
  end function get_amd_base_clock
  
  function get_amd_boost_clock() result(clock)
    real(real32) :: clock
    clock = 2400.0  ! MHz
  end function get_amd_boost_clock
  
  function get_amd_vram_mb() result(vram)
    integer(int64) :: vram
    vram = 20480  ! 20GB
  end function get_amd_vram_mb
  
  function get_amd_memory_bandwidth() result(bandwidth)
    real(real32) :: bandwidth
    bandwidth = 800.0  ! GB/s
  end function get_amd_memory_bandwidth
  
  function estimate_amd_peak_gflops() result(gflops)
    real(real32) :: gflops
    gflops = 500.0  ! Theoretical peak
  end function estimate_amd_peak_gflops
  
  ! NVIDIA detection helpers (stubs for implementation)
  function check_nvidia_gpu_available() result(available)
    logical :: available
    available = .false.  ! Not present on current system
  end function check_nvidia_gpu_available
  
  function get_nvidia_gpu_name() result(name)
    character(len=32) :: name
    name = "NVIDIA GPU"
  end function get_nvidia_gpu_name
  
  function get_nvidia_gpu_architecture() result(arch)
    character(len=32) :: arch
    arch = "unknown"
  end function get_nvidia_gpu_architecture
  
  function get_nvidia_sm_count() result(sms)
    integer :: sms
    sms = 0
  end function get_nvidia_sm_count
  
  function get_nvidia_base_clock() result(clock)
    real(real32) :: clock
    clock = 0.0
  end function get_nvidia_base_clock
  
  function get_nvidia_boost_clock() result(clock)
    real(real32) :: clock
    clock = 0.0
  end function get_nvidia_boost_clock
  
  function get_nvidia_vram_mb() result(vram)
    integer(int64) :: vram
    vram = 0
  end function get_nvidia_vram_mb
  
  function get_nvidia_memory_bandwidth() result(bandwidth)
    real(real32) :: bandwidth
    bandwidth = 0.0
  end function get_nvidia_memory_bandwidth
  
  function estimate_nvidia_peak_gflops() result(gflops)
    real(real32) :: gflops
    gflops = 0.0
  end function estimate_nvidia_peak_gflops
  
  function check_nvidia_fp16_support() result(supported)
    logical :: supported
    supported = .false.
  end function check_nvidia_fp16_support
  
  function check_nvidia_int8_support() result(supported)
    logical :: supported
    supported = .false.
  end function check_nvidia_int8_support
  
  ! Intel GPU detection helpers (stubs)
  function check_intel_gpu_available() result(available)
    logical :: available
    available = .false.
  end function check_intel_gpu_available
  
  function get_intel_gpu_name() result(name)
    character(len=32) :: name
    name = "Intel GPU"
  end function get_intel_gpu_name
  
  function get_intel_gpu_architecture() result(arch)
    character(len=32) :: arch
    arch = "unknown"
  end function get_intel_gpu_architecture
  
  function get_intel_eu_count() result(eus)
    integer :: eus
    eus = 0
  end function get_intel_eu_count
  
  function get_intel_base_clock() result(clock)
    real(real32) :: clock
    clock = 0.0
  end function get_intel_base_clock
  
  function get_intel_boost_clock() result(clock)
    real(real32) :: clock
    clock = 0.0
  end function get_intel_boost_clock
  
  function get_intel_vram_mb() result(vram)
    integer(int64) :: vram
    vram = 0
  end function get_intel_vram_mb
  
  function get_intel_memory_bandwidth() result(bandwidth)
    real(real32) :: bandwidth
    bandwidth = 0.0
  end function get_intel_memory_bandwidth
  
  function estimate_intel_peak_gflops() result(gflops)
    real(real32) :: gflops
    gflops = 0.0
  end function estimate_intel_peak_gflops
  
  ! Apple Neural Engine detection helpers (stubs)
  function check_apple_neural_engine_available() result(available)
    logical :: available
    available = .false.  ! Not on x86 systems
  end function check_apple_neural_engine_available
  
  function get_apple_chip_generation() result(gen)
    character(len=32) :: gen
    gen = "unknown"
  end function get_apple_chip_generation
  
  function get_neural_engine_core_count() result(cores)
    integer :: cores
    cores = 0
  end function get_neural_engine_core_count
  
  function get_unified_memory_bandwidth() result(bandwidth)
    real(real32) :: bandwidth
    bandwidth = 0.0
  end function get_unified_memory_bandwidth

end module sparkle_device_detection