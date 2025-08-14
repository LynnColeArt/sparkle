module sparkle_amdgpu_direct
  ! Direct AMDGPU kernel driver interface
  ! The Sparkle Way: Talk directly to the kernel!
  
  use iso_fortran_env, only: int32, int64, int8
  use iso_c_binding
  implicit none
  private
  
  ! Define unsigned types as aliases
  integer, parameter :: c_uint32_t = c_int32_t
  integer, parameter :: c_uint64_t = c_int64_t
  
  ! System constants
  integer(c_int), parameter :: O_RDWR = 2
  integer(c_int), parameter :: PROT_READ = 1
  integer(c_int), parameter :: PROT_WRITE = 2
  integer(c_int), parameter :: MAP_SHARED = 1
  
  public :: amdgpu_device, amdgpu_buffer, amdgpu_command_buffer
  public :: amdgpu_open_device, amdgpu_close_device
  public :: amdgpu_allocate_buffer
  public :: amdgpu_map_buffer
  
  ! ioctl command codes for AMDGPU
  ! From include/uapi/drm/amdgpu_drm.h
  integer(c_int), parameter :: DRM_AMDGPU_GEM_CREATE = 0
  integer(c_int), parameter :: DRM_AMDGPU_GEM_MMAP = 1
  integer(c_int), parameter :: DRM_AMDGPU_CS = 4
  integer(c_int), parameter :: DRM_AMDGPU_INFO = 5
  integer(c_int), parameter :: DRM_AMDGPU_GEM_WAIT_IDLE = 7
  
  ! DRM ioctl encoding
  integer(c_int), parameter :: DRM_IOCTL_BASE = int(z'64', c_int)
  integer(c_int), parameter :: DRM_COMMAND_BASE = int(z'40', c_int)
  
  ! Memory domains
  integer(c_int), parameter :: AMDGPU_GEM_DOMAIN_CPU = 1
  integer(c_int), parameter :: AMDGPU_GEM_DOMAIN_GTT = 2
  integer(c_int), parameter :: AMDGPU_GEM_DOMAIN_VRAM = 4
  
  ! Query info types
  integer(c_int), parameter :: AMDGPU_INFO_ACCEL_WORKING = int(z'00', c_int)
  integer(c_int), parameter :: AMDGPU_INFO_FW_VERSION = int(z'0e', c_int)
  integer(c_int), parameter :: AMDGPU_INFO_DEV_INFO = int(z'16', c_int)
  integer(c_int), parameter :: AMDGPU_INFO_MEMORY = int(z'19', c_int)
  
  ! Device info structure (simplified)
  type, bind(C) :: drm_amdgpu_info_device
    integer(c_int32_t) :: device_id
    integer(c_int32_t) :: chip_rev
    integer(c_int32_t) :: external_rev
    integer(c_int32_t) :: pci_rev
    integer(c_int32_t) :: family
    integer(c_int32_t) :: num_shader_engines
    integer(c_int32_t) :: num_shader_arrays_per_engine
    integer(c_int32_t) :: gpu_counter_freq
    integer(c_int64_t) :: max_engine_clock
    integer(c_int64_t) :: max_memory_clock
    integer(c_int32_t) :: cu_active_number
    integer(c_int32_t) :: cu_ao_mask
    integer(c_int32_t) :: cu_bitmap(4)
    ! ... more fields ...
  end type drm_amdgpu_info_device
  
  ! GEM create structure
  type, bind(C) :: drm_amdgpu_gem_create_in
    integer(c_int64_t) :: bo_size
    integer(c_int64_t) :: alignment
    integer(c_int64_t) :: domains
    integer(c_int64_t) :: domain_flags
  end type drm_amdgpu_gem_create_in
  
  type, bind(C) :: drm_amdgpu_gem_create_out
    integer(c_int32_t) :: handle
    integer(c_int32_t) :: pad
  end type drm_amdgpu_gem_create_out
  
  ! Our wrapper types
  type :: amdgpu_device
    integer :: fd = -1
    logical :: is_open = .false.
    character(len=256) :: device_path = ""
    type(drm_amdgpu_info_device) :: info
  end type amdgpu_device
  
  type :: amdgpu_buffer
    integer(c_int32_t) :: handle = 0
    integer(c_int64_t) :: size = 0
    integer(c_int64_t) :: gpu_addr = 0
    type(c_ptr) :: cpu_ptr = c_null_ptr
    logical :: is_mapped = .false.
  end type amdgpu_buffer
  
  type :: amdgpu_command_buffer
    integer(c_int32_t) :: ib_handle = 0
    integer(c_int64_t) :: ib_size = 0
    type(c_ptr) :: ib_cpu_ptr = c_null_ptr
    integer :: num_chunks = 0
  end type amdgpu_command_buffer
  
  ! C interfaces
  interface
    function open_device(path, flags) bind(C, name="open")
      import :: c_char, c_int
      character(kind=c_char), dimension(*) :: path
      integer(c_int), value :: flags
      integer(c_int) :: open_device
    end function open_device
    
    function close(fd) bind(C, name="close")
      import :: c_int
      integer(c_int), value :: fd
      integer(c_int) :: close
    end function close
    
    function ioctl(fd, request, argp) bind(C, name="ioctl")
      import :: c_int, c_long, c_ptr
      integer(c_int), value :: fd
      integer(c_long), value :: request
      type(c_ptr), value :: argp
      integer(c_int) :: ioctl
    end function ioctl
    
    function mmap(addr, length, prot, flags, fd, offset) bind(C, name="mmap")
      import :: c_ptr, c_size_t, c_int, c_long
      type(c_ptr), value :: addr
      integer(c_size_t), value :: length
      integer(c_int), value :: prot
      integer(c_int), value :: flags
      integer(c_int), value :: fd
      integer(c_long), value :: offset
      type(c_ptr) :: mmap
    end function mmap
    
    function munmap(addr, length) bind(C, name="munmap")
      import :: c_ptr, c_size_t, c_int
      type(c_ptr), value :: addr
      integer(c_size_t), value :: length
      integer(c_int) :: munmap
    end function munmap
  end interface
  
contains

  ! Open AMDGPU device
  function amdgpu_open_device(device_path) result(device)
    character(len=*), intent(in), optional :: device_path
    type(amdgpu_device) :: device
    character(len=256) :: path
    integer :: fd, status
    
    ! Default to card0 if not specified
    if (present(device_path)) then
      path = device_path
    else
      path = "/dev/dri/card0"
    end if
    
    ! Open device
    fd = open_device(trim(path) // c_null_char, O_RDWR)
    if (fd < 0) then
      print *, "❌ Failed to open AMDGPU device: ", trim(path)
      return
    end if
    
    device%fd = fd
    device%device_path = path
    device%is_open = .true.
    
    ! Query device info
    status = query_device_info(device)
    if (status /= 0) then
      print *, "⚠️ Failed to query device info"
    else
      print *, "✅ Opened AMDGPU device: ", trim(path)
      print '(A,Z8)', "   Device ID: 0x", device%info%device_id
      print '(A,I0)', "   Compute Units: ", device%info%cu_active_number
      print '(A,I0,A)', "   Max Engine Clock: ", device%info%max_engine_clock, " MHz"
      print '(A,I0,A)', "   Max Memory Clock: ", device%info%max_memory_clock, " MHz"
    end if
    
  end function amdgpu_open_device
  
  ! Query device information
  function query_device_info(device) result(status)
    type(amdgpu_device), intent(inout) :: device
    integer :: status
    
    type, bind(C) :: drm_amdgpu_info
      integer(c_int64_t) :: return_pointer
      integer(c_int32_t) :: return_size
      integer(c_int32_t) :: query
    end type drm_amdgpu_info
    
    type(drm_amdgpu_info), target :: info_req
    type(drm_amdgpu_info_device), target :: dev_info
    integer(c_long) :: request
    
    ! Setup info request
    info_req%return_pointer = int(loc(dev_info), c_int64_t)
    info_req%return_size = int(sizeof(dev_info), c_int32_t)
    info_req%query = AMDGPU_INFO_DEV_INFO
    
    ! Build ioctl request
    ! DRM_IOWR(DRM_COMMAND_BASE + DRM_AMDGPU_INFO, ...)
    ! _IOWR('d', 0x45, ...)
    request = int(z'C0206445', c_long)  ! Pre-calculated ioctl number
    
    ! Make ioctl call
    status = ioctl(device%fd, request, c_loc(info_req))
    
    if (status == 0) then
      device%info = dev_info
    end if
    
  end function query_device_info
  
  ! Allocate GPU buffer
  function amdgpu_allocate_buffer(device, size, domain) result(buffer)
    type(amdgpu_device), intent(in) :: device
    integer(c_int64_t), intent(in) :: size
    integer, intent(in), optional :: domain
    type(amdgpu_buffer) :: buffer
    
    type, bind(C) :: drm_amdgpu_gem_create
      type(drm_amdgpu_gem_create_in) :: in
      type(drm_amdgpu_gem_create_out) :: out
    end type drm_amdgpu_gem_create
    
    type(drm_amdgpu_gem_create), target :: gem_create
    integer(c_long) :: request
    integer :: status, mem_domain
    
    if (.not. device%is_open) then
      print *, "❌ Device not open"
      return
    end if
    
    ! Default to GTT (system memory accessible by GPU)
    mem_domain = AMDGPU_GEM_DOMAIN_GTT
    if (present(domain)) mem_domain = domain
    
    ! Setup gem create request
    gem_create%in%bo_size = size
    gem_create%in%alignment = 4096  ! Page aligned
    gem_create%in%domains = mem_domain
    gem_create%in%domain_flags = 0
    
    ! Build ioctl request
    ! DRM_IOWR(DRM_COMMAND_BASE + DRM_AMDGPU_GEM_CREATE, ...)
    ! _IOWR('d', 0x40, ...)
    request = int(z'C0206440', c_long)  ! Pre-calculated ioctl number
    
    ! Make ioctl call
    status = ioctl(device%fd, request, c_loc(gem_create))
    
    if (status == 0) then
      buffer%handle = gem_create%out%handle
      buffer%size = size
      print '(A,I0,A)', "✅ Allocated ", size, " bytes on GPU"
    else
      print *, "❌ Failed to allocate GPU buffer"
    end if
    
  end function amdgpu_allocate_buffer
  
  ! Map buffer for CPU access
  function amdgpu_map_buffer(device, buffer) result(status)
    type(amdgpu_device), intent(in) :: device
    type(amdgpu_buffer), intent(inout) :: buffer
    integer :: status
    
    type, bind(C) :: drm_amdgpu_gem_mmap
      integer(c_int32_t) :: handle
      integer(c_int32_t) :: pad
      integer(c_int64_t) :: offset
    end type drm_amdgpu_gem_mmap
    
    type(drm_amdgpu_gem_mmap), target :: mmap_req
    integer(c_long) :: request
    integer(c_int) :: prot, flags
    
    if (buffer%is_mapped) then
      status = 0
      return
    end if
    
    ! Setup mmap request
    mmap_req%handle = buffer%handle
    mmap_req%pad = 0
    
    ! Get mmap offset
    ! DRM_IOWR(DRM_COMMAND_BASE + DRM_AMDGPU_GEM_MMAP, ...)
    ! _IOWR('d', 0x41, ...)
    request = int(z'C0106441', c_long)  ! Pre-calculated ioctl number
    status = ioctl(device%fd, request, c_loc(mmap_req))
    
    if (status /= 0) then
      print *, "❌ Failed to get mmap offset"
      return
    end if
    
    ! Map the buffer
    prot = ior(PROT_READ, PROT_WRITE)
    flags = MAP_SHARED
    
    buffer%cpu_ptr = mmap(c_null_ptr, buffer%size, prot, flags, &
                         device%fd, int(mmap_req%offset, c_long))
    
    if (c_associated(buffer%cpu_ptr)) then
      buffer%is_mapped = .true.
      print *, "✅ Mapped GPU buffer to CPU"
      status = 0
    else
      print *, "❌ Failed to map GPU buffer"
      status = -1
    end if
    
  end function amdgpu_map_buffer
  
  ! Close device
  subroutine amdgpu_close_device(device)
    type(amdgpu_device), intent(inout) :: device
    integer :: status
    
    if (device%is_open) then
      status = close(device%fd)
      device%is_open = .false.
      device%fd = -1
      print *, "✅ Closed AMDGPU device"
    end if
    
  end subroutine amdgpu_close_device
  
end module sparkle_amdgpu_direct