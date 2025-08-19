module sporkle_types
  use iso_c_binding
  use kinds
  implicit none
  private
  
  ! Public parameters
  integer, parameter, public :: SPORKLE_SUCCESS = 0
  integer, parameter, public :: SPORKLE_ERROR = -1
  
  ! Device types enum
  type, public :: device_type_t
    integer :: value
  end type device_type_t
  
  type(device_type_t), parameter, public :: &
    DEVICE_CPU = device_type_t(1), &
    DEVICE_GPU_NVIDIA = device_type_t(2), &
    DEVICE_GPU_AMD = device_type_t(3), &
    DEVICE_GPU_INTEL = device_type_t(4), &
    DEVICE_FPGA = device_type_t(5), &
    DEVICE_MOBILE = device_type_t(6), &
    DEVICE_UNKNOWN = device_type_t(99)
  
  ! Device capabilities - what can this device do?
  type, public :: device_capabilities
    integer(i64) :: memory_bytes = 0
    integer(i32) :: compute_units = 0
    real(sp) :: clock_speed_ghz = 0.0
    real(sp) :: memory_bandwidth_gbps = 0.0
    logical :: supports_float64 = .false.
    logical :: supports_float16 = .false.
    logical :: supports_int8 = .false.
    logical :: supports_p2p = .false.
    logical :: supports_unified_memory = .false.
    character(len=:), allocatable :: instruction_set  ! AVX2, AVX512, NEON, etc
  end type device_capabilities
  
  ! Abstract base type for all compute devices
  type, abstract, public :: compute_device
    integer :: device_id = -1
    type(device_type_t) :: device_type = DEVICE_UNKNOWN
    character(len=:), allocatable :: name
    type(device_capabilities) :: capabilities
    logical :: is_available = .false.
    real(sp) :: current_load = 0.0  ! 0.0 to 1.0
  contains
    ! Abstract interfaces that each device must implement
    procedure(allocate_interface), deferred :: allocate
    procedure(deallocate_interface), deferred :: deallocate
    procedure(memcpy_interface), deferred :: memcpy
    procedure(execute_interface), deferred :: execute
    procedure(synchronize_interface), deferred :: synchronize
    procedure(get_info_interface), deferred :: get_info
    
    ! Concrete helper methods
    procedure :: can_access => device_can_access
    procedure :: estimate_performance => device_estimate_performance
  end type compute_device
  
  ! Memory buffer type
  type, public :: sporkle_buffer
    type(c_ptr) :: data = c_null_ptr
    integer(i64) :: size_bytes = 0
    integer :: owning_device = -1
    logical :: is_pinned = .false.
  end type sporkle_buffer
  
  ! Abstract interfaces
  abstract interface
    function allocate_interface(self, size_bytes, pinned) result(buffer)
      import :: compute_device, sporkle_buffer, i64
      class(compute_device), intent(inout) :: self
      integer(i64), intent(in) :: size_bytes
      logical, intent(in), optional :: pinned
      type(sporkle_buffer) :: buffer
    end function allocate_interface
    
    subroutine deallocate_interface(self, buffer)
      import :: compute_device, sporkle_buffer
      class(compute_device), intent(inout) :: self
      type(sporkle_buffer), intent(inout) :: buffer
    end subroutine deallocate_interface
    
    function memcpy_interface(self, dst, src, size_bytes) result(status)
      import :: compute_device, sporkle_buffer, i64, i32
      class(compute_device), intent(inout) :: self
      type(sporkle_buffer), intent(inout) :: dst
      type(sporkle_buffer), intent(in) :: src
      integer(i64), intent(in) :: size_bytes
      integer(i32) :: status
    end function memcpy_interface
    
    function execute_interface(self, kernel_name, args, grid_size, block_size) result(status)
      import :: compute_device, c_ptr, i32
      class(compute_device), intent(inout) :: self
      character(len=*), intent(in) :: kernel_name
      type(c_ptr), intent(in) :: args(:)
      integer(i32), intent(in) :: grid_size(3)
      integer(i32), intent(in) :: block_size(3)
      integer(i32) :: status
    end function execute_interface
    
    function synchronize_interface(self) result(status)
      import :: compute_device, i32
      class(compute_device), intent(inout) :: self
      integer(i32) :: status
    end function synchronize_interface
    
    subroutine get_info_interface(self)
      import :: compute_device
      class(compute_device), intent(inout) :: self
    end subroutine get_info_interface
  end interface
  
contains
  
  ! Check if this device can directly access another device's memory
  logical function device_can_access(self, other_device_id)
    class(compute_device), intent(in) :: self
    integer, intent(in) :: other_device_id
    
    ! By default, devices can only access their own memory
    ! Override in specific implementations for P2P support
    device_can_access = (self%device_id == other_device_id)
  end function device_can_access
  
  ! Estimate performance for a given operation size
  real(sp) function device_estimate_performance(self, flops) result(seconds)
    class(compute_device), intent(in) :: self
    integer(i64), intent(in) :: flops
    real(sp) :: tflops_per_sec
    
    ! Simple estimate based on compute units and clock speed
    ! Override in specific implementations for better accuracy
    tflops_per_sec = self%capabilities%compute_units * self%capabilities%clock_speed_ghz
    seconds = real(flops, real32) / (tflops_per_sec * 1.0e12)
  end function device_estimate_performance
  
end module sporkle_types