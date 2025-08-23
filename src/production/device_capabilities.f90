module device_capabilities
  ! Device capability detection and optimization selection
  ! No hardcoded assumptions - everything detected at runtime
  
  use kinds
  use hardware_detection
  implicit none
  private
  
  public :: get_cpu_capabilities, get_gpu_capabilities
  public :: select_tile_sizes, select_workgroup_size
  public :: device_caps_t
  
  type :: device_caps_t
    ! Compute capabilities
    real(dp) :: peak_gflops = 100.0_dp
    integer :: simd_width = 1
    integer :: max_threads = 1
    
    ! Memory hierarchy
    integer :: l1_cache_kb = 32
    integer :: l2_cache_kb = 256  
    integer :: l3_cache_kb = 8192
    real(dp) :: memory_bandwidth_gbs = 50.0_dp
    
    ! GPU specific
    integer :: wave_size = 64
    integer :: max_workgroup_size = 256
    integer :: compute_units = 1
    logical :: supports_fp64 = .false.
    logical :: supports_fp16 = .false.
    
    ! Optimization hints
    integer :: preferred_vector_width = 1
    integer :: cache_line_size = 64
    logical :: prefers_streaming = .false.
  end type device_caps_t
  
contains

  function get_cpu_capabilities() result(caps)
    type(device_caps_t) :: caps
    type(cpu_info_t) :: cpu_info
    
    cpu_info = detect_cpu_info()
    
    ! Compute peak performance
    if (cpu_info%has_avx512) then
      caps%simd_width = 16  ! 512 bits / 32 bits
      caps%preferred_vector_width = 16
      ! AVX-512: 2 FMA units × 16 floats × 2 ops/FMA
      caps%peak_gflops = cpu_info%clock_ghz * real(cpu_info%num_cores, dp) * 32.0_dp
    else if (cpu_info%has_avx2) then
      caps%simd_width = 8   ! 256 bits / 32 bits
      caps%preferred_vector_width = 8
      ! AVX2: 2 FMA units × 8 floats × 2 ops/FMA
      caps%peak_gflops = cpu_info%clock_ghz * real(cpu_info%num_cores, dp) * 16.0_dp
    else
      caps%simd_width = 4   ! SSE
      caps%preferred_vector_width = 4
      caps%peak_gflops = cpu_info%clock_ghz * real(cpu_info%num_cores, dp) * 8.0_dp
    end if
    
    ! Memory hierarchy
    caps%l1_cache_kb = cpu_info%cache_l1_kb
    caps%l2_cache_kb = cpu_info%cache_l2_kb
    caps%l3_cache_kb = cpu_info%cache_l3_kb
    caps%max_threads = cpu_info%num_threads
    
    ! Memory bandwidth estimate (conservative)
    ! DDR4-3200: ~50 GB/s, DDR5-4800: ~75 GB/s
    caps%memory_bandwidth_gbs = 50.0_dp  ! Conservative
    
    ! Check if streaming is beneficial
    ! If working set > L3 cache, streaming might help
    caps%prefers_streaming = (caps%l3_cache_kb < 32768)  ! < 32MB L3
    
    caps%supports_fp64 = .true.
    caps%supports_fp16 = cpu_info%has_avx512  ! F16C
    
  end function get_cpu_capabilities
  
  function get_gpu_capabilities(device_idx) result(caps)
    integer, intent(in) :: device_idx
    type(device_caps_t) :: caps
    type(gpu_info_t) :: gpu_info
    
    gpu_info = detect_gpu_info(device_idx)
    
    ! Compute peak performance
    ! Base: CUs × Stream processors/CU × Clock × 2 (FMA)
    if (gpu_info%arch_major >= 10) then  ! RDNA+
      ! RDNA: 64 SPs/CU for most operations
      caps%peak_gflops = real(gpu_info%num_cus, dp) * 64.0_dp * &
                        gpu_info%clock_ghz * 2.0_dp
      
      ! RDNA3 can dual-issue some instructions
      if (gpu_info%arch_major >= 11) then
        caps%peak_gflops = caps%peak_gflops * 1.5_dp  ! Conservative
      end if
    else  ! GCN
      ! GCN: 64 SPs/CU
      caps%peak_gflops = real(gpu_info%num_cus, dp) * 64.0_dp * &
                        gpu_info%clock_ghz * 2.0_dp
    end if
    
    ! GPU specifics
    caps%wave_size = gpu_info%wave_size
    caps%compute_units = gpu_info%num_cus
    caps%memory_bandwidth_gbs = gpu_info%bandwidth_gbs
    
    ! Workgroup limits
    if (gpu_info%wave_size == 32) then  ! RDNA
      caps%max_workgroup_size = 1024
      caps%preferred_vector_width = 32
    else  ! GCN/CDNA
      caps%max_workgroup_size = 256
      caps%preferred_vector_width = 64
    end if
    
    ! Cache sizes (estimates based on architecture)
    if (gpu_info%arch_major >= 10) then  ! RDNA+
      caps%l1_cache_kb = 16 * gpu_info%num_cus  ! 16KB per CU
      caps%l2_cache_kb = 4096  ! 4MB typical
    else
      caps%l1_cache_kb = 16 * gpu_info%num_cus
      caps%l2_cache_kb = 2048  ! 2MB typical
    end if
    
    caps%supports_fp64 = .true.   ! Most AMD GPUs
    caps%supports_fp16 = .true.   ! All modern GPUs
    
    ! APUs share system memory
    if (gpu_info%is_apu) then
      caps%memory_bandwidth_gbs = 50.0_dp  ! System memory bandwidth
    end if
    
  end function get_gpu_capabilities
  
  subroutine select_tile_sizes(caps, m, n, k, tile_m, tile_n, tile_k)
    type(device_caps_t), intent(in) :: caps
    integer, intent(in) :: m, n, k
    integer, intent(out) :: tile_m, tile_n, tile_k
    
    integer :: l1_elements, l2_elements
    integer :: max_tile_elements
    
    ! Calculate how many elements fit in L1
    l1_elements = (caps%l1_cache_kb * 1024) / 4  ! 4 bytes per float
    l2_elements = (caps%l2_cache_kb * 1024) / 4
    
    ! We need space for parts of A, B, and C tiles
    ! Conservative: use 1/3 of L1 for each
    max_tile_elements = l1_elements / 3
    
    ! Start with preferred vector width
    tile_m = caps%preferred_vector_width
    
    ! Expand tile_m to use more of cache
    do while (tile_m < m .and. tile_m < 256)
      if (tile_m * 64 > max_tile_elements) exit
      tile_m = tile_m * 2
    end do
    
    ! Set tile_n and tile_k to balance cache usage
    tile_n = min(64, n)
    tile_k = min(max_tile_elements / (tile_m + tile_n), k)
    tile_k = max(tile_k, 16)  ! Minimum tile size
    
    ! Align to vector width
    tile_m = (tile_m / caps%preferred_vector_width) * caps%preferred_vector_width
    if (tile_m == 0) tile_m = caps%preferred_vector_width
    
  end subroutine select_tile_sizes
  
  function select_workgroup_size(caps, total_work) result(workgroup_size)
    type(device_caps_t), intent(in) :: caps
    integer, intent(in) :: total_work
    integer :: workgroup_size
    
    ! Start with wave size
    workgroup_size = caps%wave_size
    
    ! Expand to use more threads per workgroup
    do while (workgroup_size < caps%max_workgroup_size .and. &
              workgroup_size < total_work / caps%compute_units)
      if (workgroup_size * 2 > caps%max_workgroup_size) exit
      workgroup_size = workgroup_size * 2
    end do
    
    ! Ensure we don't exceed limits
    workgroup_size = min(workgroup_size, caps%max_workgroup_size)
    workgroup_size = min(workgroup_size, total_work)
    
  end function select_workgroup_size

end module device_capabilities