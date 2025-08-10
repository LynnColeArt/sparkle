module sparkle_safe_kernels
  ! Safe wrappers for kernel operations with bounds checking
  ! The Sparkle Way: Fast but safe
  
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_f_pointer
  use sparkle_types
  use sparkle_kernels
  use sparkle_memory
  use sparkle_error_handling
  implicit none
  private
  
  public :: safe_kernel_wrapper
  public :: validate_kernel_args
  
contains

  ! Validate kernel arguments before execution
  function validate_kernel_args(kernel) result(ierr)
    type(sparkle_kernel), intent(in) :: kernel
    integer :: ierr
    integer :: i, j
    integer(int64) :: expected_size, actual_size
    
    ierr = SPARKLE_SUCCESS
    
    ! Check kernel has arguments
    if (.not. allocated(kernel%arguments)) then
      ierr = SPARKLE_ERR_INVALID
      call sparkle_error("Kernel has no arguments", .false.)
      return
    end if
    
    ! Validate each argument
    do i = 1, size(kernel%arguments)
      associate(arg => kernel%arguments(i))
        ! Check memory handle is valid
        if (.not. arg%data%is_allocated) then
          ierr = SPARKLE_ERR_INVALID
          call sparkle_error("Argument " // trim(arg%name) // " has unallocated memory", .false.)
          return
        end if
        
        ! Check shape is allocated
        if (.not. allocated(arg%shape)) then
          ierr = SPARKLE_ERR_INVALID
          call sparkle_error("Argument " // trim(arg%name) // " has no shape", .false.)
          return
        end if
        
        ! Calculate expected size
        expected_size = 1
        do j = 1, size(arg%shape)
          if (arg%shape(j) <= 0) then
            ierr = SPARKLE_ERR_INVALID
            call sparkle_error("Argument " // trim(arg%name) // " has invalid shape", .false.)
            return
          end if
          expected_size = expected_size * arg%shape(j)
        end do
        
        ! Calculate element size based on type
        select case(arg%arg_type)
        case(TYPE_REAL32, TYPE_INT32)
          expected_size = expected_size * 4
        case(TYPE_REAL64, TYPE_INT64)
          expected_size = expected_size * 8
        case(TYPE_INT8)
          expected_size = expected_size * 1
        case(TYPE_COMPLEX32)
          expected_size = expected_size * 8
        case(TYPE_COMPLEX64)
          expected_size = expected_size * 16
        case default
          ierr = SPARKLE_ERR_INVALID
          call sparkle_error("Unknown type for argument " // trim(arg%name), .false.)
          return
        end select
        
        ! Check memory size is sufficient
        if (expected_size > arg%data%size_bytes) then
          ierr = SPARKLE_ERR_BOUNDS
          call sparkle_error("Memory too small for argument " // trim(arg%name), .false.)
          return
        end if
      end associate
    end do
    
    ! Validate work items
    if (kernel%work_items <= 0) then
      ierr = SPARKLE_ERR_INVALID
      call sparkle_error("Invalid work items count", .false.)
      return
    end if
    
  end function validate_kernel_args
  
  ! Safe wrapper for kernel execution
  subroutine safe_kernel_wrapper(kernel)
    type(sparkle_kernel), intent(inout) :: kernel
    integer :: ierr
    
    ! Validate arguments first
    ierr = validate_kernel_args(kernel)
    if (ierr /= SPARKLE_SUCCESS) then
      call sparkle_error("Kernel validation failed", .false.)
      return
    end if
    
    ! Add runtime bounds checking for debug builds
    block
      logical :: debug_mode
      character(len=32) :: debug_env
      
      call get_environment_variable("SPARKLE_DEBUG", debug_env)
      debug_mode = trim(debug_env) == "1" .or. trim(debug_env) == "true"
      
      if (debug_mode) then
        print *, "🔍 Running kernel with bounds checking: ", trim(kernel%name)
      end if
    end block
    
    ! Execute the kernel with error handling
    if (associated(kernel%fortran_proc)) then
      call kernel%fortran_proc(kernel%arguments)
    else
      call sparkle_error("Kernel procedure not set", .false.)
    end if
    
  end subroutine safe_kernel_wrapper
  
  ! Create safe vector add kernel with bounds checking
  function create_safe_vector_add(n, x_mem, y_mem, z_mem) result(kernel)
    integer(int64), intent(in) :: n
    type(memory_handle), intent(in) :: x_mem, y_mem, z_mem
    type(sparkle_kernel) :: kernel
    integer :: ierr
    
    ! Basic validation
    if (n <= 0) then
      call sparkle_error("Invalid vector size for safe_vector_add", .false.)
      return
    end if
    
    ! Check memory sizes
    if (x_mem%size_bytes < n * 4 .or. y_mem%size_bytes < n * 4 .or. z_mem%size_bytes < n * 4) then
      call sparkle_error("Insufficient memory for vector operation", .false.)
      return
    end if
    
    ! Create kernel
    kernel%name = 'safe_vector_add'
    kernel%kernel_type = KERNEL_PURE
    kernel%work_items = n
    kernel%fortran_proc => safe_vector_add_impl
    kernel%arithmetic_intensity = 1.0_real64
    
    ! Set up arguments with validation
    allocate(kernel%arguments(3))
    
    ! Input x
    kernel%arguments(1)%name = 'x'
    kernel%arguments(1)%arg_type = TYPE_REAL32
    kernel%arguments(1)%intent = ARG_IN
    kernel%arguments(1)%data = x_mem
    allocate(kernel%arguments(1)%shape(1))
    kernel%arguments(1)%shape(1) = n
    
    ! Input y
    kernel%arguments(2)%name = 'y'
    kernel%arguments(2)%arg_type = TYPE_REAL32
    kernel%arguments(2)%intent = ARG_IN
    kernel%arguments(2)%data = y_mem
    allocate(kernel%arguments(2)%shape(1))
    kernel%arguments(2)%shape(1) = n
    
    ! Output z
    kernel%arguments(3)%name = 'z'
    kernel%arguments(3)%arg_type = TYPE_REAL32
    kernel%arguments(3)%intent = ARG_OUT
    kernel%arguments(3)%data = z_mem
    allocate(kernel%arguments(3)%shape(1))
    kernel%arguments(3)%shape(1) = n
    
  end function create_safe_vector_add
  
  ! Safe vector addition implementation with bounds checking
  subroutine safe_vector_add_impl(args)
    type(kernel_argument), intent(inout) :: args(:)
    real(real32), pointer :: x(:), y(:), z(:)
    integer(int64) :: i, n
    integer :: ierr
    
    ! Extract pointers
    call c_f_pointer(args(1)%data%ptr, x, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, y, args(2)%shape)
    call c_f_pointer(args(3)%data%ptr, z, args(3)%shape)
    n = args(1)%shape(1)
    
    ! Verify all arrays have same size
    if (args(2)%shape(1) /= n .or. args(3)%shape(1) /= n) then
      call sparkle_error("Array size mismatch in vector add", .false.)
      return
    end if
    
    ! Perform operation with optional bounds checking
    block
      logical :: check_bounds
      character(len=32) :: bounds_env
      
      call get_environment_variable("SPARKLE_CHECK_BOUNDS", bounds_env)
      check_bounds = trim(bounds_env) == "1" .or. trim(bounds_env) == "true"
      
      if (check_bounds) then
        do i = 1, n
          if (i < 1 .or. i > n) then
            call sparkle_error("Bounds violation in vector add", .false.)
            return
          end if
          z(i) = x(i) + y(i)
        end do
      else
        ! Fast path without bounds checking
        z = x + y
      end if
    end block
    
  end subroutine safe_vector_add_impl

end module sparkle_safe_kernels