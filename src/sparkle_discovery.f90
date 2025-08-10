module sparkle_discovery
  use sparkle_mesh_types
  use cpu_device_module
  use iso_c_binding
  implicit none
  private
  
  public :: scan_devices, profile_links, explain_devices
  
  ! Make CUDA discovery optional (dynamic loading)
  logical :: cuda_available = .false.
  
contains

  ! Scan all available compute devices
  function scan_devices() result(mesh)
    type(mesh_topology) :: mesh
    type(cpu_device) :: cpu
    type(device_handle) :: handle
    
    ! Always have at least CPU
    cpu = cpu_device(device_id=0)
    
    ! Convert CPU device to mesh handle
    handle%id = cpu%device_id
    handle%caps%kind = KIND_CPU
    handle%caps%cores = cpu%capabilities%compute_units
    handle%caps%unified_mem = .true.
    handle%caps%vram_mb = cpu%capabilities%memory_bytes / (1024 * 1024)
    handle%caps%peak_gflops = real(cpu%capabilities%compute_units, rk64) * &
                              real(cpu%capabilities%clock_speed_ghz, rk64) * &
                              8.0_rk64  ! Assume 8 FLOPS/cycle for modern CPU
    handle%caps%sustained_gflops = handle%caps%peak_gflops * 0.7_rk64  ! 70% efficiency typical
    handle%caps%mem_bw_gbs = 50.0_rk64  ! Typical DDR4 bandwidth
    handle%healthy = .true.
    handle%native = c_null_ptr  ! CPU doesn't need special handle
    
    ! Add instruction set info
    if (allocated(cpu%capabilities%instruction_set)) then
      handle%caps%driver_ver = cpu%capabilities%instruction_set
    else
      handle%caps%driver_ver = "unknown"
    end if
    
    call mesh%add_device(handle)
    
    ! Try to scan for NVIDIA GPUs
    call try_cuda_discovery(mesh)
    
    ! Try to scan for AMD GPUs
    call try_amd_discovery(mesh)
    
    ! Try to scan for Intel GPUs
    call try_intel_discovery(mesh)
    
    mesh%profiled = .false.
    
  end function scan_devices
  
  ! Profile interconnect links between devices
  subroutine profile_links(mesh)
    type(mesh_topology), intent(inout) :: mesh
    type(link_metrics) :: link
    integer :: i, j
    real(rk64) :: start_time, end_time
    
    ! For now, create basic host-memory links
    do i = 1, mesh%num_devices
      do j = 1, mesh%num_devices
        link%src_id = mesh%devices(i)%id
        link%dst_id = mesh%devices(j)%id
        
        if (i == j) then
          ! Self-link (no transfer needed)
          link%bw_gbs = 1000000.0_rk64  ! "Infinite"
          link%latency_us = 0.0_rk64
          link%direct = .true.
          link%hops = 0
        else
          ! Different devices - for now assume host staging
          link%bw_gbs = min(mesh%devices(i)%caps%mem_bw_gbs, &
                           mesh%devices(j)%caps%mem_bw_gbs) * 0.8_rk64  ! 80% efficiency
          link%latency_us = 10.0_rk64  ! Host memory latency
          link%direct = .false.
          link%hops = 2  ! src->host->dst
        end if
        
        link%reliability = 0.99_rk32  ! Assume 99% success rate
        call mesh%update_link(link)
      end do
    end do
    
    ! TODO: Actually profile with micro-benchmarks
    ! - Allocate test buffers
    ! - Time transfers of various sizes
    ! - Check for P2P capability
    
    mesh%profiled = .true.
    mesh%profile_timestamp = real(time(), rk64)
    
  contains
    ! Simple timer (would use system_clock in production)
    function time() result(t)
      integer :: t
      t = 0  ! Placeholder
    end function time
    
  end subroutine profile_links
  
  ! Explain discovered devices (introspection)
  subroutine explain_devices(mesh)
    type(mesh_topology), intent(in) :: mesh
    integer :: i
    
    print *, "=== Sparkle Device Discovery ==="
    print *, "Found", mesh%num_devices, "compute device(s):"
    print *, ""
    
    do i = 1, mesh%num_devices
      associate(dev => mesh%devices(i))
        print '(A,I0,A)', "Device ", dev%id, ":"
        print '(A,A)', "  Type: ", dev%caps%kind
        print '(A,I0)', "  Cores/SMs: ", dev%caps%cores
        print '(A,I0,A)', "  Memory: ", dev%caps%vram_mb, " MB"
        print '(A,F0.1,A)', "  Peak Performance: ", dev%caps%peak_gflops, " GFLOPS"
        print '(A,F0.1,A)', "  Memory Bandwidth: ", dev%caps%mem_bw_gbs, " GB/s"
        print '(A,L1)', "  Unified Memory: ", dev%caps%unified_mem
        print '(A,L1)', "  P2P Capable: ", dev%caps%p2p_direct
        if (allocated(dev%caps%driver_ver)) then
          print '(A,A)', "  Driver/ISA: ", dev%caps%driver_ver
        end if
        print *, ""
      end associate
    end do
    
    if (mesh%profiled) then
      print *, "Interconnect profiling: COMPLETE"
    else
      print *, "Interconnect profiling: PENDING"
    end if
    print *, ""
    
  end subroutine explain_devices
  
  ! Try to discover CUDA devices (gracefully handles missing CUDA)
  subroutine try_cuda_discovery(mesh)
    type(mesh_topology), intent(inout) :: mesh
    
    ! Use conditional compilation or dynamic loading
    ! For now, we'll create a simple stub that shows the approach
    
    interface
      function dlopen(filename, flag) bind(C, name="dlopen")
        import :: c_ptr, c_char, c_int
        character(c_char), intent(in) :: filename(*)
        integer(c_int), value :: flag
        type(c_ptr) :: dlopen
      end function dlopen
      
      function dlsym(handle, symbol) bind(C, name="dlsym")
        import :: c_ptr, c_char
        type(c_ptr), value :: handle
        character(c_char), intent(in) :: symbol(*)
        type(c_ptr) :: dlsym
      end function dlsym
    end interface
    
    type(c_ptr) :: cuda_lib
    integer, parameter :: RTLD_LAZY = 1
    
    ! Try to load CUDA library
    cuda_lib = dlopen("libcuda.so"//c_null_char, RTLD_LAZY)
    
    if (c_associated(cuda_lib)) then
      print *, "CUDA runtime detected, scanning for NVIDIA GPUs..."
      ! Would call cuda_discovery module here if library loads
      ! use cuda_discovery
      ! call scan_cuda_devices(mesh)
      
      ! For demo, add a message
      print *, "  (CUDA discovery not yet implemented in this build)"
    else
      ! CUDA not available - this is fine, not everyone has NVIDIA
      print *, "No CUDA runtime found (this is normal if you don't have NVIDIA GPUs)"
    end if
    
  end subroutine try_cuda_discovery
  
  ! Try to discover AMD devices (gracefully handles missing ROCm)
  subroutine try_amd_discovery(mesh)
    type(mesh_topology), intent(inout) :: mesh
    
    interface
      function dlopen(filename, flag) bind(C, name="dlopen")
        import :: c_ptr, c_char, c_int
        character(c_char), intent(in) :: filename(*)
        integer(c_int), value :: flag
        type(c_ptr) :: dlopen
      end function dlopen
    end interface
    
    type(c_ptr) :: hip_lib
    integer, parameter :: RTLD_LAZY = 1
    
    ! Try to load HIP/ROCm library
    hip_lib = dlopen("libamdhip64.so"//c_null_char, RTLD_LAZY)
    
    if (.not. c_associated(hip_lib)) then
      ! Try alternative name
      hip_lib = dlopen("libhip_hcc.so"//c_null_char, RTLD_LAZY)
    end if
    
    if (c_associated(hip_lib)) then
      print *, "ROCm runtime detected, scanning for AMD GPUs..."
      print *, "  (AMD discovery not yet implemented in this build)"
    else
      ! ROCm not available - normal if no AMD GPUs
      print *, "No ROCm runtime found (this is normal if you don't have AMD GPUs)"
    end if
    
  end subroutine try_amd_discovery
  
  ! Try to discover Intel GPUs (gracefully handles missing Level Zero)
  subroutine try_intel_discovery(mesh)
    type(mesh_topology), intent(inout) :: mesh
    
    interface
      function dlopen(filename, flag) bind(C, name="dlopen")
        import :: c_ptr, c_char, c_int
        character(c_char), intent(in) :: filename(*)
        integer(c_int), value :: flag
        type(c_ptr) :: dlopen
      end function dlopen
    end interface
    
    type(c_ptr) :: ze_lib
    integer, parameter :: RTLD_LAZY = 1
    
    ! Try to load Level Zero library
    ze_lib = dlopen("libze_loader.so"//c_null_char, RTLD_LAZY)
    
    if (c_associated(ze_lib)) then
      print *, "Intel GPU runtime detected, scanning for Intel GPUs..."
      print *, "  (Intel GPU discovery not yet implemented in this build)"
    else
      ! Level Zero not available
      print *, "No Intel GPU runtime found (this is normal if you don't have Intel GPUs)"
    end if
    
  end subroutine try_intel_discovery
  
end module sparkle_discovery