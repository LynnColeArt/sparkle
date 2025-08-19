module amd_discovery
  use sporkle_mesh_types
  use iso_c_binding
  implicit none
  private
  
  public :: scan_amd_devices
  
  ! HIP/ROCm status codes
  integer(c_int), parameter :: HIP_SUCCESS = 0
  
  ! HIP device properties (simplified)
  type, bind(C) :: hip_device_prop
    character(c_char) :: name(256)
    integer(c_size_t) :: totalGlobalMem
    integer(c_int) :: multiProcessorCount
    integer(c_int) :: clockRate
    integer(c_int) :: memoryClockRate
    integer(c_int) :: memoryBusWidth
    integer(c_int) :: major
    integer(c_int) :: minor
  end type hip_device_prop
  
  ! HIP C API interfaces
  interface
    function hipGetDeviceCount(count) bind(C, name="hipGetDeviceCount")
      import :: c_int
      integer(c_int), intent(out) :: count
      integer(c_int) :: hipGetDeviceCount
    end function hipGetDeviceCount
    
    function hipGetDeviceProperties(prop, device) bind(C, name="hipGetDeviceProperties")
      import :: hip_device_prop, c_int
      type(hip_device_prop), intent(out) :: prop
      integer(c_int), value :: device
      integer(c_int) :: hipGetDeviceProperties
    end function hipGetDeviceProperties
  end interface
  
contains

  ! Scan for AMD ROCm devices
  subroutine scan_amd_devices(mesh)
    type(mesh_topology), intent(inout) :: mesh
    integer(c_int) :: device_count, status, i
    type(hip_device_prop) :: props
    type(device_handle) :: handle
    character(len=256) :: device_name
    real(rk64) :: memory_gb, clock_ghz, memory_bw
    
    ! Check if HIP/ROCm is available
    status = hipGetDeviceCount(device_count)
    
    if (status /= HIP_SUCCESS .or. device_count == 0) then
      return
    end if
    
    print '(A,I0,A)', "Found ", device_count, " AMD GPU(s)"
    
    ! Enumerate each device
    do i = 0, device_count - 1
      status = hipGetDeviceProperties(props, i)
      
      if (status == HIP_SUCCESS) then
        ! Convert C string
        call c_string_to_fortran(props%name, device_name)
        
        ! Build device handle
        handle%id = mesh%num_devices + i
        handle%caps%kind = KIND_AMD
        handle%caps%cores = props%multiProcessorCount
        handle%caps%sm_count = props%multiProcessorCount
        handle%caps%vram_mb = int(props%totalGlobalMem / (1024 * 1024), int32)
        
        memory_gb = real(props%totalGlobalMem, rk64) / (1024.0_rk64**3)
        clock_ghz = real(props%clockRate, rk64) / 1.0e6_rk64
        memory_bw = real(props%memoryClockRate, rk64) / 1000.0_rk64 * &
                   real(props%memoryBusWidth, rk64) / 8.0_rk64 * 2.0_rk64
        
        handle%caps%mem_bw_gbs = memory_bw
        handle%caps%unified_mem = .false.  ! AMD GPUs typically don't have unified memory
        
        ! Estimate GFLOPS (RDNA2 = 128 ops/CU/clock)
        handle%caps%peak_gflops = props%multiProcessorCount * clock_ghz * 128.0_rk64 * 1000.0_rk64
        handle%caps%sustained_gflops = handle%caps%peak_gflops * 0.65_rk64
        
        write(handle%caps%driver_ver, '(A,I0,A,I0)') "ROCm ", props%major, ".", props%minor
        handle%caps%pci_id = device_name
        handle%healthy = .true.
        handle%load = 0.0
        
        call mesh%add_device(handle)
        
        print '(A,I0,A,A)', "  GPU ", i, ": ", trim(device_name)
        print '(A,F0.1,A)', "    Memory: ", memory_gb, " GB"
        print '(A,I0,A,F0.1,A)', "    CUs: ", props%multiProcessorCount, &
                                 ", Peak: ", handle%caps%peak_gflops/1000.0_rk64, " TFLOPS"
      end if
    end do
    
  contains
    
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
    
  end subroutine scan_amd_devices
  
end module amd_discovery