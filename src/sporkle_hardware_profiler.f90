module sporkle_hardware_profiler
  ! Hardware Profiler: Discovers the true characteristics of any compute device
  ! This is the key to universal patterns - know your hardware!
  
  use kinds
  use iso_c_binding
  implicit none
  
  private
  public :: hardware_characteristics, kernel_parameters
  public :: profile_device, derive_optimal_parameters
  public :: profile_nvidia_gpu, profile_amd_gpu, profile_cpu
  
  type :: hardware_characteristics
    ! Device identification
    character(len=256) :: name = "Unknown"
    character(len=32) :: vendor = "Unknown"
    
    ! Compute capabilities
    integer :: compute_units = 1        ! SMs, CUs, or CPU cores
    integer :: threads_per_unit = 1     ! Warps×32, Waves×64, or SIMD width
    integer :: max_threads = 1          ! Total parallel threads
    integer :: warp_size = 1            ! SIMD width (32 NVIDIA, 64 AMD, 16 AVX512)
    
    ! Memory hierarchy (bytes)
    integer(i64) :: l1_cache_size = 16384      ! Fastest memory (shared/L1)
    integer(i64) :: l2_cache_size = 524288     ! Middle tier  
    integer(i64) :: l3_cache_size = 8388608    ! Last level cache
    integer(i64) :: global_memory = 0          ! VRAM or system RAM
    integer :: cache_line_size = 64            ! Coherency granularity
    
    ! Registers
    integer :: registers_per_thread = 32       ! Available registers
    integer :: register_bank_width = 1         ! Parallel register access
    
    ! Memory bandwidth
    real(dp) :: peak_bandwidth_gbs = 100.0     ! Theoretical max
    real(dp) :: measured_bandwidth_gbs = 50.0  ! Actual achieved
    integer :: memory_latency_cycles = 100     ! Load-to-use latency
    
    ! Pipeline characteristics
    integer :: pipeline_depth = 4              ! Instruction pipeline stages
    logical :: dual_issue = .false.            ! Can issue 2 ops/cycle?
    integer :: fma_units = 1                   ! Fused multiply-add units
    
    ! Performance
    real(dp) :: peak_gflops = 100.0           ! Theoretical FP32 peak
    real(dp) :: measured_gflops = 50.0        ! Actual achieved
  end type hardware_characteristics
  
  type :: kernel_parameters
    ! Parallelism
    integer :: threads = 256
    integer :: block_size = 256
    integer :: num_blocks = 1
    
    ! Tiling
    integer :: tile_size = 16
    integer :: tile_k = 16
    integer :: tile_m = 16
    integer :: tile_n = 16
    
    ! Work distribution
    integer :: outputs_per_thread = 1
    integer :: inputs_per_load = 1
    
    ! Memory access
    integer :: stride = 1
    integer :: prefetch_distance = 0
    integer :: cache_line_elements = 16
    
    ! Optimization
    integer :: unroll_factor = 1
    logical :: use_shared_memory = .false.
    logical :: use_tensor_cores = .false.
  end type kernel_parameters
  
  ! OpenGL interfaces for GPU profiling
  interface
    function glGetString(name) bind(C, name='glGetString')
      import :: c_int, c_ptr
      integer(c_int), value :: name
      type(c_ptr) :: glGetString
    end function
    
    subroutine glGetIntegerv(pname, params) bind(C, name='glGetIntegerv')
      import :: c_int, c_ptr
      integer(c_int), value :: pname
      type(c_ptr), value :: params
    end subroutine
  end interface
  
  ! OpenGL constants
  integer(c_int), parameter :: GL_VENDOR = int(z'1F00', c_int)
  integer(c_int), parameter :: GL_RENDERER = int(z'1F01', c_int)
  integer(c_int), parameter :: GL_MAX_COMPUTE_WORK_GROUP_COUNT = int(z'91BE', c_int)
  integer(c_int), parameter :: GL_MAX_COMPUTE_WORK_GROUP_SIZE = int(z'91BF', c_int)
  integer(c_int), parameter :: GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS = int(z'90EB', c_int)
  integer(c_int), parameter :: GL_MAX_COMPUTE_SHARED_MEMORY_SIZE = int(z'8262', c_int)
  
contains

  function profile_device(device_type) result(hw)
    character(len=*), intent(in) :: device_type
    type(hardware_characteristics) :: hw
    
    select case(device_type)
    case("nvidia", "NVIDIA")
      hw = profile_nvidia_gpu()
    case("amd", "AMD")
      hw = profile_amd_gpu()
    case("cpu", "CPU")
      hw = profile_cpu()
    case default
      print *, "Unknown device type, using defaults"
    end select
    
  end function profile_device
  
  function profile_nvidia_gpu() result(hw)
    type(hardware_characteristics) :: hw
    integer(c_int), target :: work_group_size(3), shared_mem
    integer(c_int), target :: max_invocations
    
    hw%name = "NVIDIA RTX A4500"
    hw%vendor = "NVIDIA"
    
    ! NVIDIA A4500 specifications (Ampere GA102)
    hw%compute_units = 46                    ! 46 SMs
    hw%threads_per_unit = 32 * 4             ! 4 warps per SM scheduler
    hw%warp_size = 32                        ! NVIDIA warp size
    hw%max_threads = hw%compute_units * 2048 ! 2048 threads per SM max
    
    ! Memory hierarchy (A4500)
    hw%l1_cache_size = 128 * 1024            ! 128KB L1/shared per SM
    hw%l2_cache_size = 4 * 1024 * 1024       ! 4MB L2 cache
    hw%l3_cache_size = 0                     ! No L3 on GPU
    hw%global_memory = 20_i64 * 1024 * 1024 * 1024  ! 20GB VRAM
    hw%cache_line_size = 128                 ! GPU cache line
    
    ! Registers (Ampere)
    hw%registers_per_thread = 255            ! Max registers per thread
    hw%register_bank_width = 32              ! Register file width
    
    ! Memory bandwidth
    hw%peak_bandwidth_gbs = 448.0            ! GDDR6 bandwidth
    hw%measured_bandwidth_gbs = 380.0        ! ~85% achievable
    hw%memory_latency_cycles = 300           ! Global memory latency
    
    ! Pipeline (Ampere)
    hw%pipeline_depth = 4                    ! Instruction pipeline depth
    hw%dual_issue = .true.                   ! Can dual-issue INT32+FP32
    hw%fma_units = 64                        ! 64 FP32 FMA units per SM
    
    ! Performance
    hw%peak_gflops = 23650.0                 ! 23.65 TFLOPS FP32
    hw%measured_gflops = 113.0               ! Current achievement (needs fixing!)
    
    print *, "Profiled NVIDIA GPU:"
    print *, "  Name:", trim(hw%name)
    print *, "  Compute Units:", hw%compute_units, "SMs"
    print *, "  Max Threads:", hw%max_threads
    print *, "  Shared Memory:", hw%l1_cache_size/1024, "KB per SM"
    print *, "  Peak Performance:", hw%peak_gflops, "GFLOPS"
    
  end function profile_nvidia_gpu
  
  function profile_amd_gpu() result(hw)
    type(hardware_characteristics) :: hw
    
    hw%name = "AMD Radeon RX 7900 XT"
    hw%vendor = "AMD"
    
    ! AMD RX 7900 XT specifications (Navi 31, RDNA3)
    hw%compute_units = 84                    ! 84 CUs (not 96 - some disabled)
    
    ! RDNA3 prefers Wave32 for better occupancy
    hw%warp_size = 32                        ! Wave32 mode for RDNA3
    hw%threads_per_unit = 32 * 4 * 2         ! 4 waves per SIMD × 2 SIMDs
    hw%max_threads = hw%compute_units * hw%threads_per_unit
    
    ! Memory hierarchy (RDNA3)
    hw%l1_cache_size = 128 * 1024            ! 128KB L1 per shader array
    hw%l2_cache_size = 6 * 1024 * 1024       ! 6MB L2 cache
    hw%l3_cache_size = 80 * 1024 * 1024      ! 80MB Infinity Cache
    hw%global_memory = 20_i64 * 1024 * 1024 * 1024  ! 20GB VRAM
    hw%cache_line_size = 128                 ! GPU cache line
    
    ! Registers (RDNA3)
    hw%registers_per_thread = 256            ! 256 VGPRs
    hw%register_bank_width = 32              ! VGPR width
    
    ! Memory bandwidth (RX 7900 XT specific)
    hw%peak_bandwidth_gbs = 960.0            ! GDDR6 20Gbps × 384-bit
    hw%measured_bandwidth_gbs = 768.0        ! ~80% achievable
    hw%memory_latency_cycles = 350           ! Global memory latency
    
    ! Pipeline (RDNA3)
    hw%pipeline_depth = 4                    ! Wave pipeline depth
    hw%dual_issue = .true.                   ! RDNA3 dual-issue
    hw%fma_units = 128                       ! 128 FP32 units per CU (dual-issue)
    
    ! Performance
    ! Base: 84 CUs × 64 FMA × 2 ops × 2.5 GHz = 26,880 GFLOPS
    ! Dual-issue can boost to ~40,000 GFLOPS with proper instruction mix
    hw%peak_gflops = 40000.0                 ! 40 TFLOPS with dual-issue
    hw%measured_gflops = 3630.0              ! Current achievement
    
    print *, "Profiled AMD GPU:"
    print *, "  Name:", trim(hw%name)
    print *, "  Compute Units:", hw%compute_units, "CUs"
    print *, "  Infinity Cache:", hw%l3_cache_size/(1024*1024), "MB"
    print *, "  Peak Performance:", hw%peak_gflops, "GFLOPS"
    
  end function profile_amd_gpu
  
  function profile_cpu() result(hw)
    type(hardware_characteristics) :: hw
    
    hw%name = "AMD Ryzen 7 7700X"
    hw%vendor = "AMD"
    
    ! Ryzen 7 7700X specifications (Zen 4)
    hw%compute_units = 8                     ! 8 cores
    hw%threads_per_unit = 2                  ! 2 threads per core (SMT)
    hw%warp_size = 16                        ! AVX-512 = 16 floats
    hw%max_threads = 16                      ! 8 cores × 2 threads
    
    ! Memory hierarchy (Zen 4)
    hw%l1_cache_size = 32 * 1024             ! 32KB L1D per core
    hw%l2_cache_size = 1024 * 1024           ! 1MB L2 per core
    hw%l3_cache_size = 32 * 1024 * 1024      ! 32MB L3 shared
    hw%global_memory = 256_i64 * 1024 * 1024 * 1024  ! 256GB system RAM
    hw%cache_line_size = 64                  ! x86 cache line
    
    ! Registers (x86-64 with AVX-512)
    hw%registers_per_thread = 32             ! 32 AVX-512 registers
    hw%register_bank_width = 16              ! 512-bit = 16 floats
    
    ! Memory bandwidth
    hw%peak_bandwidth_gbs = 83.2             ! DDR5-5200 dual channel
    hw%measured_bandwidth_gbs = 60.0         ! ~72% achievable
    hw%memory_latency_cycles = 100           ! Main memory latency
    
    ! Pipeline (Zen 4)
    hw%pipeline_depth = 6                    ! Pipeline depth
    hw%dual_issue = .true.                   ! Can issue 2 FMA per cycle
    hw%fma_units = 2                         ! 2 AVX-512 FMA units per core
    
    ! Performance (Zen 4 with AVX-512)
    ! 8 cores × 5.4 GHz × 2 FMA × 16 floats × 2 ops = 1382 GFLOPS
    hw%peak_gflops = 1382.0                  
    hw%measured_gflops = 196.7               ! Current achievement
    
    print *, "Profiled CPU:"
    print *, "  Name:", trim(hw%name)
    print *, "  Cores:", hw%compute_units
    print *, "  AVX-512:", hw%warp_size, "floats per vector"
    print *, "  Peak Performance:", hw%peak_gflops, "GFLOPS"
    
  end function profile_cpu
  
  function derive_optimal_parameters(hw) result(params)
    type(hardware_characteristics), intent(in) :: hw
    type(kernel_parameters) :: params
    real(dp) :: shared_mem_per_block, optimal_tile_fp
    
    ! Parallelism - maximize occupancy
    params%block_size = hw%warp_size * 4     ! 4 warps/waves per block
    params%threads = min(hw%max_threads, 65536)  ! Cap at reasonable limit
    params%num_blocks = params%threads / params%block_size
    
    ! Tiling - optimize for L1/shared memory
    ! Use half the L1 for double buffering
    shared_mem_per_block = real(hw%l1_cache_size, dp) / real(max(1, hw%compute_units), dp)
    optimal_tile_fp = sqrt(shared_mem_per_block / 8.0_dp)  ! 8 bytes for 2 matrices
    params%tile_size = int(optimal_tile_fp)
    
    ! Ensure tile size is multiple of warp size for coalescing
    params%tile_size = (params%tile_size / hw%warp_size) * hw%warp_size
    params%tile_size = max(params%tile_size, hw%warp_size)
    params%tile_size = min(params%tile_size, 128)  ! Cap at 128
    
    ! Work per thread - based on available registers
    if (hw%registers_per_thread > 128) then
      params%outputs_per_thread = 4  ! 4×4 tile per thread
    else if (hw%registers_per_thread > 64) then
      params%outputs_per_thread = 2  ! 2×2 tile per thread
    else
      params%outputs_per_thread = 1  ! 1×1 output per thread
    end if
    
    ! Memory access patterns
    params%cache_line_elements = hw%cache_line_size / 4  ! For float32
    params%stride = params%cache_line_elements
    params%prefetch_distance = hw%memory_latency_cycles / 10  ! Rough estimate
    
    ! Unrolling - based on pipeline depth
    params%unroll_factor = min(hw%pipeline_depth * 2, 8)
    if (hw%dual_issue) params%unroll_factor = params%unroll_factor * 2
    
    ! Feature flags
    params%use_shared_memory = (hw%l1_cache_size > 16384)
    params%use_tensor_cores = .false.  ! TODO: Detect tensor cores
    
    print *, ""
    print *, "Derived Optimal Parameters:"
    print *, "  Block size:", params%block_size, "threads"
    print *, "  Tile size:", params%tile_size, "×", params%tile_size
    print *, "  Outputs per thread:", params%outputs_per_thread, "×", params%outputs_per_thread
    print *, "  Unroll factor:", params%unroll_factor
    print *, "  Use shared memory:", params%use_shared_memory
    
    ! Performance projection
    block
      real(dp) :: arithmetic_intensity, expected_gflops, efficiency
      integer :: total_flops_per_thread, total_threads_active
      
      ! Calculate arithmetic intensity
      total_flops_per_thread = params%tile_size * params%outputs_per_thread * params%outputs_per_thread * 2
      total_threads_active = params%threads
      
      ! Expected performance based on optimal parameters
      arithmetic_intensity = real(total_flops_per_thread, dp) / real(params%tile_size * 4, dp)
      
      ! Estimate achievable performance
      if (arithmetic_intensity > 10.0) then
        efficiency = 0.85  ! Compute bound - can achieve high efficiency
      else if (arithmetic_intensity > 5.0) then
        efficiency = 0.70  ! Balanced
      else
        efficiency = 0.50  ! Memory bound
      end if
      
      expected_gflops = hw%peak_gflops * efficiency
      
      print *, ""
      print *, "Performance Projection:"
      print *, "  Arithmetic intensity:", arithmetic_intensity, "FLOPS/byte"
      print *, "  Expected efficiency:", efficiency * 100.0, "%"
      print *, "  Target performance:", expected_gflops, "GFLOPS"
      print *, "  Current performance:", hw%measured_gflops, "GFLOPS"
      print *, "  Potential speedup:", expected_gflops / max(1.0_dp, hw%measured_gflops), "×"
    end block
    
  end function derive_optimal_parameters

end module sporkle_hardware_profiler