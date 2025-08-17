module sparkle_universal_device_selector
  ! Universal device selection based on workload characteristics
  ! Generalizes the apple_orchestrator patterns for all architectures
  
  use iso_fortran_env
  use iso_c_binding
  use sparkle_types
  implicit none
  
  ! Device types (universal)
  integer, parameter :: UNI_DEVICE_CPU_PERFORMANCE = 1    ! High-perf cores
  integer, parameter :: UNI_DEVICE_CPU_EFFICIENCY = 2     ! Low-power cores  
  integer, parameter :: UNI_DEVICE_GPU_DISCRETE = 3       ! dGPU
  integer, parameter :: UNI_DEVICE_GPU_INTEGRATED = 4     ! iGPU
  integer, parameter :: UNI_DEVICE_NEURAL_ENGINE = 5      ! ANE/NPU/Tensor cores
  integer, parameter :: UNI_DEVICE_MATRIX_UNIT = 6        ! AMX/Matrix engines
  integer, parameter :: UNI_DEVICE_DSP = 7                ! Signal processors
  integer, parameter :: UNI_DEVICE_FPGA = 8               ! Programmable logic
  
  ! Operation characteristics
  integer, parameter :: OP_COMPUTE_BOUND = 1
  integer, parameter :: OP_MEMORY_BOUND = 2
  integer, parameter :: OP_LATENCY_SENSITIVE = 3
  integer, parameter :: OP_THROUGHPUT_ORIENTED = 4
  
  ! Workload patterns
  integer, parameter :: PATTERN_GEMM = 1
  integer, parameter :: PATTERN_CONV = 2
  integer, parameter :: PATTERN_REDUCTION = 3
  integer, parameter :: PATTERN_ELEMENTWISE = 4
  integer, parameter :: PATTERN_SCATTER_GATHER = 5
  integer, parameter :: PATTERN_FFT = 6
  integer, parameter :: PATTERN_ATTENTION = 7
  
  type :: compute_capability
    logical :: supports_fp64 = .false.
    logical :: supports_fp32 = .true.
    logical :: supports_fp16 = .false.
    logical :: supports_int8 = .false.
    logical :: supports_matrix_ops = .false.
    logical :: supports_tensor_ops = .false.
    logical :: supports_async = .false.
    integer :: vector_width = 1           ! SIMD width
    integer :: warp_size = 1              ! GPU warp/wavefront size
    real(real64) :: peak_gflops = 0.0
    real(real64) :: memory_bandwidth = 0.0  ! GB/s
  end type
  
  type :: universal_compute_unit
    character(len=64) :: name = ""
    integer :: device_type = 0
    integer :: device_id = 0              ! Physical device ID
    logical :: available = .false.
    logical :: busy = .false.
    
    ! Performance characteristics
    type(compute_capability) :: caps
    real(real64) :: current_load = 0.0    ! 0.0-1.0
    
    ! Profiling data per pattern
    real(real64) :: pattern_performance(7) = 0.0  ! GFLOPS per pattern
    integer :: pattern_count(7) = 0               ! Times executed
    
    ! Power/thermal
    real(real64) :: power_efficiency = 1.0  ! GFLOPS/watt
    real(real64) :: temperature = 0.0       ! Current temp
    real(real64) :: thermal_limit = 100.0   ! Max temp
  end type
  
  type :: workload_characteristics
    integer :: pattern = 0
    integer :: compute_intensity = 0      ! Compute vs memory bound
    integer(int64) :: total_flops = 0
    integer(int64) :: memory_traffic = 0  ! Bytes
    integer :: batch_size = 1
    logical :: can_split = .true.         ! Divisible workload?
    logical :: needs_coherence = .false.  ! Requires cache coherence?
    real(real64) :: arithmetic_intensity = 0.0  ! FLOPS/byte
  end type
  
  type :: device_routing_decision
    integer :: primary_device = 0
    integer, allocatable :: secondary_devices(:)  ! For split workloads
    real(real64) :: split_ratios(8) = 0.0       ! How to divide work
    real(real64) :: expected_gflops = 0.0
    character(len=256) :: reasoning = ""
  end type
  
  type :: universal_device_selector
    type(universal_compute_unit), allocatable :: devices(:)
    integer :: num_devices = 0
    
    ! System-wide view
    real(real64) :: total_compute_power = 0.0   ! GFLOPS
    real(real64) :: total_memory_bandwidth = 0.0 ! GB/s
    logical :: unified_memory = .false.
    
    ! Profiling and learning
    logical :: profile_mode = .true.
    logical :: adaptive_routing = .true.
    
    ! Decision history for ML-style optimization
    type(device_routing_decision), allocatable :: routing_history(:)
    integer :: history_size = 0
    
  contains
    procedure :: discover_devices
    procedure :: analyze_workload
    procedure :: select_optimal_device
    procedure :: select_multi_device
    procedure :: update_profiling_data
    procedure :: get_device_score
    procedure :: rebalance_load
  end type
  
contains

  subroutine discover_devices(this)
    class(universal_device_selector), intent(inout) :: this
    
    print *, "🔍 Discovering compute devices..."
    
    ! This would interface with platform-specific detection
    ! For now, we'll simulate a typical heterogeneous system
    
    ! Example: AMD system with CPU + dGPU + iGPU
    call discover_cpu_devices(this)
    call discover_gpu_devices(this)
    call discover_special_devices(this)
    
    ! Calculate total system capabilities
    call calculate_system_capabilities(this)
    
    print '(A,F8.1,A)', " Total compute power: ", this%total_compute_power, " GFLOPS"
    print '(A,F8.1,A)', " Total memory bandwidth: ", this%total_memory_bandwidth, " GB/s"
    
  end subroutine discover_devices
  
  function analyze_workload(this, flops, bytes, pattern) result(characteristics)
    class(universal_device_selector), intent(in) :: this
    integer(int64), intent(in) :: flops, bytes
    integer, intent(in), optional :: pattern
    type(workload_characteristics) :: characteristics
    
    characteristics%total_flops = flops
    characteristics%memory_traffic = bytes
    
    ! Calculate arithmetic intensity
    if (bytes > 0) then
      characteristics%arithmetic_intensity = real(flops, real64) / real(bytes, real64)
    else
      characteristics%arithmetic_intensity = 1000000.0  ! Effectively infinite
    end if
    
    ! Determine compute vs memory bound
    if (characteristics%arithmetic_intensity > 10.0) then
      characteristics%compute_intensity = OP_COMPUTE_BOUND
    else if (characteristics%arithmetic_intensity < 1.0) then
      characteristics%compute_intensity = OP_MEMORY_BOUND
    else
      characteristics%compute_intensity = OP_THROUGHPUT_ORIENTED
    end if
    
    ! Set pattern if provided
    if (present(pattern)) then
      characteristics%pattern = pattern
    else
      ! Infer pattern from characteristics
      characteristics%pattern = infer_pattern(characteristics)
    end if
    
  end function analyze_workload
  
  function select_optimal_device(this, workload) result(decision)
    class(universal_device_selector), intent(inout) :: this
    type(workload_characteristics), intent(in) :: workload
    type(device_routing_decision) :: decision
    
    integer :: i, best_device
    real(real64) :: best_score, score
    
    best_device = 0
    best_score = 0.0
    
    ! Score each device for this workload
    do i = 1, this%num_devices
      if (.not. this%devices(i)%available) cycle
      if (this%devices(i)%busy) cycle
      
      score = this%get_device_score(i, workload)
      
      if (score > best_score) then
        best_score = score
        best_device = i
      end if
    end do
    
    if (best_device > 0) then
      decision%primary_device = best_device
      decision%expected_gflops = calculate_expected_performance(this%devices(best_device), workload)
      write(decision%reasoning, '(A,A,A,F7.1,A)') &
        "Selected ", trim(this%devices(best_device)%name), &
        " for ", decision%expected_gflops, " GFLOPS"
    else
      decision%primary_device = 1  ! Fallback to CPU
      decision%reasoning = "No optimal device found, defaulting to CPU"
    end if
    
  end function select_optimal_device
  
  function get_device_score(this, device_idx, workload) result(score)
    class(universal_device_selector), intent(in) :: this
    integer, intent(in) :: device_idx
    type(workload_characteristics), intent(in) :: workload
    real(real64) :: score
    
    type(universal_compute_unit) :: device
    real(real64) :: pattern_score, load_factor, thermal_factor
    real(real64) :: bandwidth_score, compute_score
    
    device = this%devices(device_idx)
    
    ! Base score from device capabilities
    compute_score = device%caps%peak_gflops
    bandwidth_score = device%caps%memory_bandwidth
    
    ! Adjust for workload characteristics
    if (workload%compute_intensity == OP_COMPUTE_BOUND) then
      score = compute_score * 0.8 + bandwidth_score * 0.2
    else if (workload%compute_intensity == OP_MEMORY_BOUND) then
      score = compute_score * 0.2 + bandwidth_score * 0.8
    else
      score = compute_score * 0.5 + bandwidth_score * 0.5
    end if
    
    ! Pattern-specific performance (if we have profiling data)
    if (device%pattern_count(workload%pattern) > 0) then
      pattern_score = device%pattern_performance(workload%pattern)
      score = score * 0.3 + pattern_score * 0.7  ! Heavily weight actual performance
    end if
    
    ! Penalties
    load_factor = 1.0 - device%current_load
    thermal_factor = 1.0 - (device%temperature / device%thermal_limit)
    
    score = score * load_factor * thermal_factor
    
    ! Special device bonuses
    select case(device%device_type)
      case(UNI_DEVICE_NEURAL_ENGINE)
        if (workload%pattern == PATTERN_CONV .or. workload%pattern == PATTERN_ATTENTION) then
          score = score * 2.0  ! Neural engines excel at these
        end if
      case(UNI_DEVICE_MATRIX_UNIT)
        if (workload%pattern == PATTERN_GEMM) then
          score = score * 1.5  ! Matrix units optimized for GEMM
        end if
    end select
    
  end function get_device_score
  
  ! Helper routines
  subroutine discover_cpu_devices(this)
    class(universal_device_selector), intent(inout) :: this
    type(universal_compute_unit) :: cpu_device
    
    ! Performance cores
    cpu_device%name = "CPU Performance Cores"
    cpu_device%device_type = UNI_DEVICE_CPU_PERFORMANCE
    cpu_device%device_id = 0
    cpu_device%available = .true.
    cpu_device%caps%peak_gflops = 196.7  ! From our benchmarks!
    cpu_device%caps%memory_bandwidth = 50.0
    cpu_device%caps%supports_fp64 = .true.
    cpu_device%caps%supports_async = .true.
    cpu_device%caps%vector_width = 16  ! AVX-512
    
    call add_device(this, cpu_device)
    
  end subroutine discover_cpu_devices
  
  subroutine discover_gpu_devices(this)
    class(universal_device_selector), intent(inout) :: this
    type(universal_compute_unit) :: gpu_device
    
    ! Discrete GPU (e.g., RX 7900 XT)
    gpu_device%name = "AMD Radeon RX 7900 XT"
    gpu_device%device_type = UNI_DEVICE_GPU_DISCRETE
    gpu_device%device_id = 0
    gpu_device%available = .true.
    gpu_device%caps%peak_gflops = 451.0  ! Single kernel
    gpu_device%caps%memory_bandwidth = 960.0  ! GDDR6
    gpu_device%caps%supports_fp16 = .true.
    gpu_device%caps%supports_async = .true.
    gpu_device%caps%warp_size = 64  ! AMD wavefront
    
    ! Set async capability to 3630 GFLOPS!
    gpu_device%pattern_performance(PATTERN_CONV) = 3630.0  ! With async
    gpu_device%pattern_count(PATTERN_CONV) = 1
    
    call add_device(this, gpu_device)
    
    ! Integrated GPU
    gpu_device%name = "AMD Raphael iGPU"
    gpu_device%device_type = UNI_DEVICE_GPU_INTEGRATED
    gpu_device%device_id = 1
    gpu_device%caps%peak_gflops = 50.0  ! Estimate
    gpu_device%caps%memory_bandwidth = 50.0  ! Shares with CPU
    
    call add_device(this, gpu_device)
    
  end subroutine discover_gpu_devices
  
  subroutine discover_special_devices(this)
    class(universal_device_selector), intent(inout) :: this
    ! Placeholder for Neural Engines, DSPs, etc.
  end subroutine discover_special_devices
  
  subroutine add_device(this, device)
    class(universal_device_selector), intent(inout) :: this
    type(universal_compute_unit), intent(in) :: device
    type(universal_compute_unit), allocatable :: temp(:)
    
    if (.not. allocated(this%devices)) then
      allocate(this%devices(1))
      this%devices(1) = device
      this%num_devices = 1
    else
      ! Grow array
      allocate(temp(this%num_devices + 1))
      temp(1:this%num_devices) = this%devices
      temp(this%num_devices + 1) = device
      call move_alloc(temp, this%devices)
      this%num_devices = this%num_devices + 1
    end if
    
    print '(A,A,A,F8.1,A)', " ✓ Discovered ", trim(device%name), &
           " (", device%caps%peak_gflops, " GFLOPS)"
    
  end subroutine add_device
  
  subroutine calculate_system_capabilities(this)
    class(universal_device_selector), intent(inout) :: this
    integer :: i
    
    this%total_compute_power = 0.0
    this%total_memory_bandwidth = 0.0
    
    do i = 1, this%num_devices
      if (this%devices(i)%available) then
        this%total_compute_power = this%total_compute_power + &
                                   this%devices(i)%caps%peak_gflops
        ! Don't double-count shared memory bandwidth
        if (this%devices(i)%device_type /= UNI_DEVICE_GPU_INTEGRATED) then
          this%total_memory_bandwidth = this%total_memory_bandwidth + &
                                        this%devices(i)%caps%memory_bandwidth
        end if
      end if
    end do
    
  end subroutine calculate_system_capabilities
  
  function calculate_expected_performance(device, workload) result(gflops)
    type(universal_compute_unit), intent(in) :: device
    type(workload_characteristics), intent(in) :: workload
    real(real64) :: gflops
    
    real(real64) :: compute_time, memory_time, total_time
    
    ! Simple roofline model
    compute_time = real(workload%total_flops, real64) / (device%caps%peak_gflops * 1.0e9)
    memory_time = real(workload%memory_traffic, real64) / (device%caps%memory_bandwidth * 1.0e9)
    
    total_time = max(compute_time, memory_time)
    
    if (total_time > 0.0) then
      gflops = real(workload%total_flops, real64) / (total_time * 1.0e9)
    else
      gflops = device%caps%peak_gflops
    end if
    
  end function calculate_expected_performance
  
  function infer_pattern(workload) result(pattern)
    type(workload_characteristics), intent(in) :: workload
    integer :: pattern
    
    ! Simple heuristics - could be much smarter
    if (workload%arithmetic_intensity > 100.0) then
      pattern = PATTERN_GEMM  ! High compute intensity suggests GEMM
    else if (workload%arithmetic_intensity > 10.0) then
      pattern = PATTERN_CONV  ! Moderate suggests convolution
    else
      pattern = PATTERN_ELEMENTWISE  ! Low suggests memory-bound elementwise
    end if
    
  end function infer_pattern
  
  subroutine update_profiling_data(this, device_id, pattern, achieved_gflops)
    class(universal_device_selector), intent(inout) :: this
    integer, intent(in) :: device_id, pattern
    real(real64), intent(in) :: achieved_gflops
    
    ! Update running average of performance
    if (this%devices(device_id)%pattern_count(pattern) == 0) then
      this%devices(device_id)%pattern_performance(pattern) = achieved_gflops
    else
      ! Exponential moving average
      this%devices(device_id)%pattern_performance(pattern) = &
        this%devices(device_id)%pattern_performance(pattern) * 0.9 + &
        achieved_gflops * 0.1
    end if
    
    this%devices(device_id)%pattern_count(pattern) = &
      this%devices(device_id)%pattern_count(pattern) + 1
      
  end subroutine update_profiling_data
  
  function select_multi_device(this, workload) result(decision)
    class(universal_device_selector), intent(inout) :: this
    type(workload_characteristics), intent(in) :: workload
    type(device_routing_decision) :: decision
    
    ! TODO: Implement workload splitting across multiple devices
    ! For now, just use single device selection
    decision = this%select_optimal_device(workload)
    
  end function select_multi_device
  
  subroutine rebalance_load(this)
    class(universal_device_selector), intent(inout) :: this
    ! TODO: Implement dynamic load rebalancing
  end subroutine rebalance_load

end module sparkle_universal_device_selector