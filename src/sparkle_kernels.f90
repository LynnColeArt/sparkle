module sparkle_kernels
  use iso_fortran_env, only: int32, int64, real32, real64
  use sparkle_types
  use sparkle_memory
  use sparkle_mesh_types
  implicit none
  private
  
  public :: sparkle_kernel, kernel_argument, kernel_builder
  public :: KERNEL_PURE, KERNEL_ELEMENTAL, KERNEL_REDUCTION
  public :: ARG_IN, ARG_OUT, ARG_INOUT
  public :: TYPE_REAL32, TYPE_REAL64, TYPE_INT32, TYPE_INT64
  public :: kernel_set_argument_shapes
  
  ! Kernel types
  integer, parameter :: KERNEL_PURE = 1       ! Simple element-wise operations
  integer, parameter :: KERNEL_ELEMENTAL = 2  ! Fortran elemental procedures
  integer, parameter :: KERNEL_REDUCTION = 3  ! Reduction operations
  
  ! Argument intents
  integer, parameter :: ARG_IN = 1
  integer, parameter :: ARG_OUT = 2
  integer, parameter :: ARG_INOUT = 3
  
  ! Data type constants for kernel arguments
  integer, parameter :: TYPE_REAL32 = 1
  integer, parameter :: TYPE_REAL64 = 2
  integer, parameter :: TYPE_INT32 = 3
  integer, parameter :: TYPE_INT64 = 4
  integer, parameter :: TYPE_INT8 = 5
  integer, parameter :: TYPE_COMPLEX32 = 6
  integer, parameter :: TYPE_COMPLEX64 = 7
  
  ! Kernel argument descriptor
  type :: kernel_argument
    character(len=:), allocatable :: name
    integer :: arg_type = 0  ! Type kind (real32, real64, etc)
    integer :: intent = ARG_IN
    integer :: rank = 0      ! Array rank (0=scalar, 1=vector, 2=matrix)
    integer(int64), allocatable :: shape(:)
    type(memory_handle) :: data
  end type kernel_argument
  
  ! Sparkle kernel - represents pure Fortran computation
  type :: sparkle_kernel
    character(len=:), allocatable :: name
    integer :: kernel_type = KERNEL_PURE
    type(kernel_argument), allocatable :: arguments(:)
    
    ! The actual Fortran procedure (stored as procedure pointer)
    procedure(kernel_interface), pointer, nopass :: fortran_proc => null()
    
    ! Metadata for distribution
    integer(int64) :: work_items = 0  ! Total elements to process
    integer(int64) :: block_size = 256  ! For GPU execution
    
    ! Performance hints
    real(real64) :: arithmetic_intensity = 1.0_real64  ! FLOPS per byte
    logical :: is_memory_bound = .true.
    
  contains
    procedure :: validate => kernel_validate
    procedure :: estimate_flops => kernel_estimate_flops
  end type sparkle_kernel
  
  ! Generic kernel interface - users will implement this
  abstract interface
    subroutine kernel_interface(args)
      import :: kernel_argument
      type(kernel_argument), intent(inout) :: args(:)
    end subroutine kernel_interface
  end interface
  
  ! Builder pattern for creating kernels
  type :: kernel_builder
    private
    type(sparkle_kernel) :: kernel
  contains
    procedure :: with_name => builder_with_name
    procedure :: with_type => builder_with_type
    procedure :: add_argument => builder_add_argument
    procedure :: with_procedure => builder_with_procedure
    procedure :: with_work_items => builder_with_work_items
    procedure :: build => builder_build
  end type kernel_builder
  
contains

  ! Validate kernel is well-formed
  function kernel_validate(this) result(is_valid)
    class(sparkle_kernel), intent(in) :: this
    logical :: is_valid
    
    is_valid = .true.
    
    ! Must have a name
    if (.not. allocated(this%name)) then
      is_valid = .false.
      return
    end if
    
    ! Must have a procedure
    if (.not. associated(this%fortran_proc)) then
      is_valid = .false.
      return
    end if
    
    ! Must have at least one argument
    if (.not. allocated(this%arguments)) then
      is_valid = .false.
      return
    end if
    
    ! All arguments must be valid
    block
      integer :: i
      do i = 1, size(this%arguments)
        if (.not. allocated(this%arguments(i)%name)) then
          is_valid = .false.
          return
        end if
      end do
    end block
    
  end function kernel_validate
  
  ! Estimate FLOPS for this kernel
  function kernel_estimate_flops(this) result(flops)
    class(sparkle_kernel), intent(in) :: this
    real(real64) :: flops
    
    ! Basic estimate - can be overridden
    select case (this%kernel_type)
    case (KERNEL_PURE)
      ! Assume 1 FLOP per element for simple kernels
      flops = real(this%work_items, real64)
      
    case (KERNEL_ELEMENTAL)
      ! Assume 2 FLOPs per element
      flops = real(this%work_items, real64) * 2.0_real64
      
    case (KERNEL_REDUCTION)
      ! Reductions are ~2N FLOPs
      flops = real(this%work_items, real64) * 2.0_real64
      
    case default
      flops = real(this%work_items, real64)
    end select
    
    ! Adjust by arithmetic intensity
    flops = flops * this%arithmetic_intensity
    
  end function kernel_estimate_flops
  
  ! Set argument shapes from memory handles
  subroutine kernel_set_argument_shapes(kernel)
    type(sparkle_kernel), intent(inout) :: kernel
    integer :: i
    
    do i = 1, size(kernel%arguments)
      ! Only set shape if not already set
      if (kernel%arguments(i)%data%is_allocated .and. &
          .not. allocated(kernel%arguments(i)%shape)) then
        ! Default to 1D array if shape not specified
        allocate(kernel%arguments(i)%shape(1))
        kernel%arguments(i)%shape(1) = kernel%arguments(i)%data%size / 4  ! Assume real32
      end if
    end do
  end subroutine kernel_set_argument_shapes

  ! Builder methods
  function builder_with_name(this, name) result(builder)
    class(kernel_builder), intent(inout) :: this
    character(len=*), intent(in) :: name
    type(kernel_builder) :: builder
    
    this%kernel%name = name
    builder = this
  end function builder_with_name
  
  function builder_with_type(this, kernel_type) result(builder)
    class(kernel_builder), intent(inout) :: this
    integer, intent(in) :: kernel_type
    type(kernel_builder) :: builder
    
    this%kernel%kernel_type = kernel_type
    builder = this
  end function builder_with_type
  
  function builder_add_argument(this, name, arg_type, intent, rank) result(builder)
    class(kernel_builder), intent(inout) :: this
    character(len=*), intent(in) :: name
    integer, intent(in) :: arg_type, intent
    integer, intent(in), optional :: rank
    type(kernel_builder) :: builder
    
    type(kernel_argument) :: arg
    type(kernel_argument), allocatable :: temp(:)
    integer :: n
    
    ! Create argument
    arg%name = name
    arg%arg_type = arg_type
    arg%intent = intent
    if (present(rank)) then
      arg%rank = rank
    else
      arg%rank = 1  ! Default to vector
    end if
    
    ! Add to kernel
    if (allocated(this%kernel%arguments)) then
      n = size(this%kernel%arguments)
      allocate(temp(n + 1))
      temp(1:n) = this%kernel%arguments
      temp(n + 1) = arg
      call move_alloc(temp, this%kernel%arguments)
    else
      allocate(this%kernel%arguments(1))
      this%kernel%arguments(1) = arg
    end if
    
    builder = this
  end function builder_add_argument
  
  function builder_with_procedure(this, proc) result(builder)
    class(kernel_builder), intent(inout) :: this
    procedure(kernel_interface) :: proc
    type(kernel_builder) :: builder
    
    this%kernel%fortran_proc => proc
    builder = this
  end function builder_with_procedure
  
  function builder_with_work_items(this, n) result(builder)
    class(kernel_builder), intent(inout) :: this
    integer(int64), intent(in) :: n
    type(kernel_builder) :: builder
    
    this%kernel%work_items = n
    builder = this
  end function builder_with_work_items
  
  function builder_build(this) result(kernel)
    class(kernel_builder), intent(inout) :: this
    type(sparkle_kernel) :: kernel
    
    kernel = this%kernel
  end function builder_build
  
end module sparkle_kernels