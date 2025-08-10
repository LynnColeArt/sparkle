module sparkle_types
  use iso_c_binding, only: c_ptr
  implicit none
  private
  
  ! Strong typing with kinds
  integer, parameter :: sp = selected_real_kind(6, 37)    ! single precision
  integer, parameter :: dp = selected_real_kind(15, 307)  ! double precision
  integer, parameter :: i32 = selected_int_kind(9)        ! 32-bit int
  integer, parameter :: i64 = selected_int_kind(18)       ! 64-bit int
  
  ! Custom types (like Python dataclasses)
  type, public :: device_info
    character(len=:), allocatable :: name
    integer(i32) :: device_id
    integer(i64) :: memory_bytes
    logical :: is_available
  end type device_info
  
  ! Generic interfaces (like Python protocols)
  type, abstract, public :: compute_device
  contains
    procedure(allocate_interface), deferred :: allocate
    procedure(deallocate_interface), deferred :: deallocate
    procedure(execute_interface), deferred :: execute
  end type compute_device
  
  ! Interface definitions
  abstract interface
    function allocate_interface(self, bytes) result(ptr)
      import :: compute_device, i64, c_ptr
      class(compute_device), intent(inout) :: self
      integer(i64), intent(in) :: bytes
      type(c_ptr) :: ptr
    end function
    
    subroutine deallocate_interface(self, ptr)
      import :: compute_device, c_ptr
      class(compute_device), intent(inout) :: self
      type(c_ptr), intent(in) :: ptr
    end subroutine
    
    subroutine execute_interface(self, kernel, args)
      import :: compute_device
      class(compute_device), intent(inout) :: self
      procedure() :: kernel
      class(*), intent(in) :: args(:)
    end subroutine
  end interface
  
  ! Type-safe enums
  type, public :: device_type
    integer :: value
  end type device_type
  
  type(device_type), parameter, public :: &
    DEVICE_CPU = device_type(1), &
    DEVICE_GPU = device_type(2), &
    DEVICE_FPGA = device_type(3)
    
end module sparkle_types

! Example usage with strong typing
program example
  use sparkle_types
  implicit none
  
  type(device_info) :: gpu
  real(dp) :: matrix(100, 100)  ! Explicitly double precision
  integer(i64) :: big_number
  
  ! Type safety enforced at compile time!
  gpu = device_info(name="RTX 4090", device_id=0_i32, &
                    memory_bytes=24000000000_i64, is_available=.true.)
  
end program example