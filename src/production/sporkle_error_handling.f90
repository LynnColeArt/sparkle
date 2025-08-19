module sporkle_error_handling
  ! Centralized error handling for Sporkle
  ! The Sporkle Way: Fail gracefully, inform clearly
  
  use kinds
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  private
  
  public :: sporkle_check, sporkle_error, sporkle_warning
  public :: safe_allocate_1d_real32, safe_allocate_1d_real64
  public :: safe_allocate_1d_int32, safe_allocate_1d_int64
  public :: safe_allocate_2d_real32, safe_allocate_2d_real64
  public :: safe_deallocate
  public :: check_bounds_1d, check_bounds_2d
  public :: SPORKLE_SUCCESS, SPORKLE_ERR_ALLOC, SPORKLE_ERR_BOUNDS
  public :: SPORKLE_ERR_INVALID, SPORKLE_ERR_NOT_IMPL, SPORKLE_ERR_IO
  public :: SPORKLE_ERR_OVERFLOW
  
  ! Error codes
  integer, parameter :: SPORKLE_SUCCESS = 0
  integer, parameter :: SPORKLE_ERR_ALLOC = -1
  integer, parameter :: SPORKLE_ERR_BOUNDS = -2
  integer, parameter :: SPORKLE_ERR_INVALID = -3
  integer, parameter :: SPORKLE_ERR_NOT_IMPL = -4
  integer, parameter :: SPORKLE_ERR_IO = -5
  integer, parameter :: SPORKLE_ERR_OVERFLOW = -6
  
  ! Maximum allocation size (8GB default)
  integer(i64), parameter :: MAX_ALLOC_SIZE = 8_int64 * 1024_int64**3
  
  interface safe_allocate_1d_real32
    module procedure safe_allocate_1d_real32
  end interface
  
  interface safe_allocate_1d_real64
    module procedure safe_allocate_1d_real64
  end interface
  
  interface safe_allocate_1d_int32
    module procedure safe_allocate_1d_int32
  end interface
  
  interface safe_allocate_1d_int64
    module procedure safe_allocate_1d_int64
  end interface
  
  interface safe_allocate_2d_real32
    module procedure safe_allocate_2d_real32
  end interface
  
  interface safe_allocate_2d_real64
    module procedure safe_allocate_2d_real64
  end interface
  
contains

  ! Check error code and handle if needed
  subroutine sporkle_check(error_code, message, fatal)
    integer, intent(in) :: error_code
    character(len=*), intent(in) :: message
    logical, intent(in), optional :: fatal
    
    logical :: is_fatal
    
    if (error_code == SPORKLE_SUCCESS) return
    
    is_fatal = .true.
    if (present(fatal)) is_fatal = fatal
    
    select case(error_code)
    case(SPORKLE_ERR_ALLOC)
      call sporkle_error("Memory allocation failed: " // message, is_fatal)
    case(SPORKLE_ERR_BOUNDS)
      call sporkle_error("Array bounds violation: " // message, is_fatal)
    case(SPORKLE_ERR_INVALID)
      call sporkle_error("Invalid parameter: " // message, is_fatal)
    case(SPORKLE_ERR_NOT_IMPL)
      call sporkle_error("Not implemented: " // message, is_fatal)
    case(SPORKLE_ERR_IO)
      call sporkle_error("I/O error: " // message, is_fatal)
    case(SPORKLE_ERR_OVERFLOW)
      call sporkle_error("Integer overflow: " // message, is_fatal)
    case default
      call sporkle_error("Unknown error: " // message, is_fatal)
    end select
    
  end subroutine sporkle_check
  
  ! Report error
  subroutine sporkle_error(message, fatal)
    character(len=*), intent(in) :: message
    logical, intent(in), optional :: fatal
    
    write(error_unit, '(A)') "❌ SPORKLE ERROR: " // message
    
    if (present(fatal)) then
      if (fatal) then
        write(error_unit, '(A)') "   Terminating program."
        error stop 1
      end if
    else
      error stop 1
    end if
    
  end subroutine sporkle_error
  
  ! Report warning
  subroutine sporkle_warning(message)
    character(len=*), intent(in) :: message
    
    write(error_unit, '(A)') "⚠️  SPORKLE WARNING: " // message
    
  end subroutine sporkle_warning
  
  ! Safe allocation for 1D real32 arrays
  function safe_allocate_1d_real32(n, array_name) result(ierr)
    use kinds
    integer(i64), intent(in) :: n
    character(len=*), intent(in), optional :: array_name
    integer :: ierr
    real(sp), allocatable :: dummy(:)
    character(len=:), allocatable :: name
    
    ierr = SPORKLE_SUCCESS
    name = "array"
    if (present(array_name)) name = array_name
    
    ! Check size
    if (n <= 0) then
      ierr = SPORKLE_ERR_INVALID
      call sporkle_error("Invalid size for " // name // ": size must be positive")
      return
    end if
    
    if (n * 4 > MAX_ALLOC_SIZE) then
      ierr = SPORKLE_ERR_OVERFLOW
      call sporkle_error("Allocation too large for " // name)
      return
    end if
    
    allocate(dummy(n), stat=ierr)
    if (ierr /= 0) then
      ierr = SPORKLE_ERR_ALLOC
      call sporkle_error("Failed to allocate " // name)
    end if
    
    if (allocated(dummy)) deallocate(dummy)
    
  end function safe_allocate_1d_real32
  
  ! Safe allocation for 1D real64 arrays
  function safe_allocate_1d_real64(n, array_name) result(ierr)
    use kinds
    integer(i64), intent(in) :: n
    character(len=*), intent(in), optional :: array_name
    integer :: ierr
    real(dp), allocatable :: dummy(:)
    character(len=:), allocatable :: name
    
    ierr = SPORKLE_SUCCESS
    name = "array"
    if (present(array_name)) name = array_name
    
    ! Check size
    if (n <= 0) then
      ierr = SPORKLE_ERR_INVALID
      call sporkle_error("Invalid size for " // name // ": size must be positive")
      return
    end if
    
    if (n * 8 > MAX_ALLOC_SIZE) then
      ierr = SPORKLE_ERR_OVERFLOW
      call sporkle_error("Allocation too large for " // name)
      return
    end if
    
    allocate(dummy(n), stat=ierr)
    if (ierr /= 0) then
      ierr = SPORKLE_ERR_ALLOC
      call sporkle_error("Failed to allocate " // name)
    end if
    
    if (allocated(dummy)) deallocate(dummy)
    
  end function safe_allocate_1d_real64
  
  ! Safe allocation for 1D int32 arrays
  function safe_allocate_1d_int32(n, array_name) result(ierr)
    integer(i64), intent(in) :: n
    character(len=*), intent(in), optional :: array_name
    integer :: ierr
    integer(i32), allocatable :: dummy(:)
    character(len=:), allocatable :: name
    
    ierr = SPORKLE_SUCCESS
    name = "array"
    if (present(array_name)) name = array_name
    
    ! Check size
    if (n <= 0) then
      ierr = SPORKLE_ERR_INVALID
      call sporkle_error("Invalid size for " // name // ": size must be positive")
      return
    end if
    
    if (n * 4 > MAX_ALLOC_SIZE) then
      ierr = SPORKLE_ERR_OVERFLOW
      call sporkle_error("Allocation too large for " // name)
      return
    end if
    
    allocate(dummy(n), stat=ierr)
    if (ierr /= 0) then
      ierr = SPORKLE_ERR_ALLOC
      call sporkle_error("Failed to allocate " // name)
    end if
    
    if (allocated(dummy)) deallocate(dummy)
    
  end function safe_allocate_1d_int32
  
  ! Safe allocation for 1D int64 arrays
  function safe_allocate_1d_int64(n, array_name) result(ierr)
    integer(i64), intent(in) :: n
    character(len=*), intent(in), optional :: array_name
    integer :: ierr
    integer(i64), allocatable :: dummy(:)
    character(len=:), allocatable :: name
    
    ierr = SPORKLE_SUCCESS
    name = "array"
    if (present(array_name)) name = array_name
    
    ! Check size
    if (n <= 0) then
      ierr = SPORKLE_ERR_INVALID
      call sporkle_error("Invalid size for " // name // ": size must be positive")
      return
    end if
    
    if (n * 8 > MAX_ALLOC_SIZE) then
      ierr = SPORKLE_ERR_OVERFLOW
      call sporkle_error("Allocation too large for " // name)
      return
    end if
    
    allocate(dummy(n), stat=ierr)
    if (ierr /= 0) then
      ierr = SPORKLE_ERR_ALLOC
      call sporkle_error("Failed to allocate " // name)
    end if
    
    if (allocated(dummy)) deallocate(dummy)
    
  end function safe_allocate_1d_int64
  
  ! Safe allocation for 2D real32 arrays
  function safe_allocate_2d_real32(m, n, array_name) result(ierr)
    use kinds
    integer(i64), intent(in) :: m, n
    character(len=*), intent(in), optional :: array_name
    integer :: ierr
    real(sp), allocatable :: dummy(:,:)
    character(len=:), allocatable :: name
    
    ierr = SPORKLE_SUCCESS
    name = "array"
    if (present(array_name)) name = array_name
    
    ! Check sizes
    if (m <= 0 .or. n <= 0) then
      ierr = SPORKLE_ERR_INVALID
      call sporkle_error("Invalid dimensions for " // name)
      return
    end if
    
    if (m * n * 4 > MAX_ALLOC_SIZE) then
      ierr = SPORKLE_ERR_OVERFLOW
      call sporkle_error("Allocation too large for " // name)
      return
    end if
    
    allocate(dummy(m,n), stat=ierr)
    if (ierr /= 0) then
      ierr = SPORKLE_ERR_ALLOC
      call sporkle_error("Failed to allocate " // name)
    end if
    
    if (allocated(dummy)) deallocate(dummy)
    
  end function safe_allocate_2d_real32
  
  ! Safe allocation for 2D real64 arrays
  function safe_allocate_2d_real64(m, n, array_name) result(ierr)
    use kinds
    integer(i64), intent(in) :: m, n
    character(len=*), intent(in), optional :: array_name
    integer :: ierr
    real(dp), allocatable :: dummy(:,:)
    character(len=:), allocatable :: name
    
    ierr = SPORKLE_SUCCESS
    name = "array"
    if (present(array_name)) name = array_name
    
    ! Check sizes
    if (m <= 0 .or. n <= 0) then
      ierr = SPORKLE_ERR_INVALID
      call sporkle_error("Invalid dimensions for " // name)
      return
    end if
    
    if (m * n * 8 > MAX_ALLOC_SIZE) then
      ierr = SPORKLE_ERR_OVERFLOW
      call sporkle_error("Allocation too large for " // name)
      return
    end if
    
    allocate(dummy(m,n), stat=ierr)
    if (ierr /= 0) then
      ierr = SPORKLE_ERR_ALLOC
      call sporkle_error("Failed to allocate " // name)
    end if
    
    if (allocated(dummy)) deallocate(dummy)
    
  end function safe_allocate_2d_real64
  
  ! Safe deallocation (generic)
  subroutine safe_deallocate(message)
    character(len=*), intent(in), optional :: message
    
    if (present(message)) then
      ! This is just a placeholder - actual deallocation happens in calling code
      ! This provides a consistent interface for logging
      print *, "Deallocating: ", message
    end if
    
  end subroutine safe_deallocate
  
  ! Check 1D array bounds
  function check_bounds_1d(index, array_size, array_name) result(ierr)
    integer(i64), intent(in) :: index, array_size
    character(len=*), intent(in), optional :: array_name
    integer :: ierr
    character(len=:), allocatable :: name
    
    ierr = SPORKLE_SUCCESS
    name = "array"
    if (present(array_name)) name = array_name
    
    if (index < 1 .or. index > array_size) then
      ierr = SPORKLE_ERR_BOUNDS
      write(error_unit, '(A,I0,A,I0,A,I0)') &
        "Bounds violation in ", name, ": index ", index, &
        " out of range [1,", array_size, "]"
    end if
    
  end function check_bounds_1d
  
  ! Check 2D array bounds
  function check_bounds_2d(i, j, m, n, array_name) result(ierr)
    integer(i64), intent(in) :: i, j, m, n
    character(len=*), intent(in), optional :: array_name
    integer :: ierr
    character(len=:), allocatable :: name
    
    ierr = SPORKLE_SUCCESS
    name = "array"
    if (present(array_name)) name = array_name
    
    if (i < 1 .or. i > m .or. j < 1 .or. j > n) then
      ierr = SPORKLE_ERR_BOUNDS
      write(error_unit, '(A,A,A,I0,A,I0,A,I0,A,I0,A)') &
        "Bounds violation in ", name, ": index (", i, ",", j, &
        ") out of range [(1,1),(", m, ",", n, ")]"
    end if
    
  end function check_bounds_2d

end module sporkle_error_handling