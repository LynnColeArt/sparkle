module cuda_discovery
  use sparkle_mesh_types
  use iso_c_binding
  implicit none
  private
  
  public :: scan_cuda_devices
  
  ! CUDA API status codes
  integer(c_int), parameter :: CUDA_SUCCESS = 0
  
  ! CUDA device properties structure (simplified)
  type, bind(C) :: cuda_device_prop
    character(c_char) :: name(256)
    integer(c_size_t) :: totalGlobalMem
    integer(c_int) :: multiProcessorCount
    integer(c_int) :: clockRate  ! in kHz
    integer(c_int) :: memoryClockRate  ! in kHz
    integer(c_int) :: memoryBusWidth  ! in bits
    integer(c_int) :: major  ! Compute capability major
    integer(c_int) :: minor  ! Compute capability minor
    integer(c_int) :: unifiedAddressing
    integer(c_int) :: canMapHostMemory
  end type cuda_device_prop
  
  ! CUDA C API interfaces
  interface
    function cudaGetDeviceCount(count) bind(C, name="cudaGetDeviceCount")
      import :: c_int
      integer(c_int), intent(out) :: count
      integer(c_int) :: cudaGetDeviceCount
    end function cudaGetDeviceCount
    
    function cudaGetDeviceProperties(prop, device) bind(C, name="cudaGetDeviceProperties")
      import :: cuda_device_prop, c_int
      type(cuda_device_prop), intent(out) :: prop
      integer(c_int), value :: device
      integer(c_int) :: cudaGetDeviceProperties
    end function cudaGetDeviceProperties
    
    function cudaSetDevice(device) bind(C, name="cudaSetDevice")
      import :: c_int
      integer(c_int), value :: device
      integer(c_int) :: cudaSetDevice
    end function cudaSetDevice
    
    function cudaDeviceCanAccessPeer(canAccess, device, peerDevice) bind(C, name="cudaDeviceCanAccessPeer")
      import :: c_int
      integer(c_int), intent(out) :: canAccess
      integer(c_int), value :: device, peerDevice
      integer(c_int) :: cudaDeviceCanAccessPeer
    end function cudaDeviceCanAccessPeer
  end interface
  
contains

  ! Scan for NVIDIA CUDA devices
  subroutine scan_cuda_devices(mesh)
    type(mesh_topology), intent(inout) :: mesh
    integer(c_int) :: device_count, status, i, j
    integer(c_int) :: can_access_peer
    type(cuda_device_prop) :: props
    type(device_handle) :: handle
    character(len=256) :: device_name
    real(rk64) :: memory_gb, clock_ghz, memory_bw
    
    ! Check if CUDA is available
    status = cudaGetDeviceCount(device_count)
    
    if (status /= CUDA_SUCCESS .or. device_count == 0) then
      ! No CUDA devices or CUDA not available
      return
    end if
    
    print '(A,I0,A)', "Found ", device_count, " NVIDIA GPU(s)"
    
    ! Enumerate each CUDA device
    do i = 0, device_count - 1
      status = cudaGetDeviceProperties(props, i)
      
      if (status == CUDA_SUCCESS) then
        ! Convert C string to Fortran string
        call c_string_to_fortran(props%name, device_name)
        
        ! Build device handle
        handle%id = mesh%num_devices + i  ! Continue numbering from existing devices
        handle%caps%kind = KIND_NVIDIA
        handle%caps%cores = props%multiProcessorCount
        handle%caps%sm_count = props%multiProcessorCount
        
        ! Memory in MB
        handle%caps%vram_mb = int(props%totalGlobalMem / (1024 * 1024), int32)
        memory_gb = real(props%totalGlobalMem, rk64) / (1024.0_rk64**3)
        
        ! Clock speed in GHz
        clock_ghz = real(props%clockRate, rk64) / 1.0e6_rk64
        
        ! Memory bandwidth = (memory_clock_mhz * bus_width / 8) * 2 (DDR)
        memory_bw = real(props%memoryClockRate, rk64) / 1000.0_rk64 * &
                   real(props%memoryBusWidth, rk64) / 8.0_rk64 * 2.0_rk64
        handle%caps%mem_bw_gbs = memory_bw
        
        ! Compute capability and features
        handle%caps%unified_mem = (props%unifiedAddressing /= 0)
        handle%caps%uvm_supported = (props%canMapHostMemory /= 0)
        
        ! Estimate GFLOPS based on compute capability
        ! Rough estimates: cores * clock * ops_per_clock
        select case(props%major)
          case(8)  ! Ampere (A100, RTX 30xx)
            handle%caps%peak_gflops = props%multiProcessorCount * clock_ghz * 128.0_rk64 * 1000.0_rk64
          case(7)  ! Turing/Volta (RTX 20xx, V100)
            handle%caps%peak_gflops = props%multiProcessorCount * clock_ghz * 64.0_rk64 * 1000.0_rk64
          case(6)  ! Pascal (GTX 10xx)
            handle%caps%peak_gflops = props%multiProcessorCount * clock_ghz * 32.0_rk64 * 1000.0_rk64
          case default
            handle%caps%peak_gflops = props%multiProcessorCount * clock_ghz * 16.0_rk64 * 1000.0_rk64
        end select
        
        handle%caps%sustained_gflops = handle%caps%peak_gflops * 0.7_rk64  ! 70% efficiency
        
        ! Set device info
        write(handle%caps%driver_ver, '(A,I0,A,I0)') "CUDA ", props%major, ".", props%minor
        handle%caps%pci_id = device_name
        handle%healthy = .true.
        handle%load = 0.0
        
        ! Add to mesh
        call mesh%add_device(handle)
        
        print '(A,I0,A,A)', "  GPU ", i, ": ", trim(device_name)
        print '(A,F0.1,A,F0.1,A)', "    Memory: ", memory_gb, " GB, Bandwidth: ", memory_bw, " GB/s"
        print '(A,I0,A,F0.1,A)', "    SMs: ", props%multiProcessorCount, ", Peak: ", &
                                  handle%caps%peak_gflops/1000.0_rk64, " TFLOPS"
      end if
    end do
    
    ! Check P2P capabilities between GPUs
    if (device_count > 1) then
      print *, "  Checking P2P capabilities..."
      do i = 0, device_count - 1
        do j = 0, device_count - 1
          if (i /= j) then
            status = cudaDeviceCanAccessPeer(can_access_peer, i, j)
            if (status == CUDA_SUCCESS .and. can_access_peer /= 0) then
              ! Update device capabilities
              ! Find devices in mesh and mark P2P capable
              block
                integer :: idx_i, idx_j
                idx_i = mesh%find_device(mesh%num_devices - device_count + i)
                idx_j = mesh%find_device(mesh%num_devices - device_count + j)
                if (idx_i > 0 .and. idx_j > 0) then
                  mesh%devices(idx_i)%caps%p2p_direct = .true.
                  print '(A,I0,A,I0)', "    GPU ", i, " <-> GPU ", j, " P2P enabled"
                end if
              end block
            end if
          end if
        end do
      end do
    end if
    
  contains
    
    ! Convert C string to Fortran string
    subroutine c_string_to_fortran(c_str, f_str)
      character(c_char), intent(in) :: c_str(*)
      character(len=*), intent(out) :: f_str
      integer :: i
      
      f_str = ''
      do i = 1, len(f_str)
        if (c_str(i) == c_null_char) exit
        f_str(i:i) = c_str(i)
      end do
    end subroutine c_string_to_fortran
    
  end subroutine scan_cuda_devices
  
end module cuda_discovery