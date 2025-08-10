# Sparkle Coding Standards & Style Guide

> "Code is read far more often than it is written." - Guido van Rossum

This guide ensures Sparkle code is elegant, strongly-typed, and maintainable. We write Fortran with Pythonic principles: explicit, simple, and readable.

## 🎯 Core Principles

1. **Strong Typing Always** - Every variable has an explicit type
2. **Explicit is Better than Implicit** - No implicit typing, ever
3. **Elegance Matters** - Beautiful code performs better
4. **Consistency** - Same patterns everywhere
5. **Self-Documenting** - Code should explain itself

## 📐 Module Structure

```fortran
module sparkle_example
  ! 1. Use statements (sorted: standard library, then project modules)
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_ptr, c_int
  use sparkle_types
  use sparkle_memory
  
  ! 2. Implicit none ALWAYS
  implicit none
  
  ! 3. Access control (private by default)
  private
  public :: public_type, public_function
  
  ! 4. Module parameters
  integer, parameter :: CACHE_LINE_SIZE = 64
  real(real64), parameter :: EPSILON = 1.0e-12_real64
  
  ! 5. Type definitions
  type :: example_type
    integer(int64) :: count = 0
    real(real64) :: value = 0.0_real64
    logical :: is_valid = .false.
  contains
    procedure :: init => example_init
    procedure :: compute => example_compute
  end type example_type
  
  ! 6. Interfaces (for generic procedures)
  interface compute
    module procedure compute_real32
    module procedure compute_real64
  end interface compute
  
  ! 7. External interfaces (C bindings)
  interface
    function c_function(ptr, size) bind(c, name="c_function")
      import :: c_ptr, c_size_t
      type(c_ptr), value :: ptr
      integer(c_size_t), value :: size
      integer(c_int) :: c_function
    end function
  end interface
  
contains
  ! Implementation here
end module sparkle_example
```

## 🔤 Naming Conventions

### Types
- **PascalCase** for derived types: `DeviceHandle`, `MemoryPool`
- Suffix `_t` only for enums: `device_type_t`

### Variables
- **snake_case** for all variables: `device_count`, `memory_size`
- Meaningful names, no abbreviations: `buffer_size` not `buf_sz`

### Functions/Subroutines
- **snake_case** for procedures: `allocate_memory`, `plan_shards`
- Verb-first naming: `create_device`, `destroy_handle`
- Predicates start with `is_` or `has_`: `is_valid`, `has_capability`

### Constants
- **SCREAMING_SNAKE_CASE** for parameters: `MAX_DEVICES`, `DEFAULT_TIMEOUT`
- Include units in names: `TIMEOUT_MS`, `BUFFER_SIZE_BYTES`

### Module Names
- **snake_case** with `sparkle_` prefix: `sparkle_memory`, `sparkle_scheduler`

## 💪 Strong Typing Rules

### Always Specify Kind
```fortran
! Good - explicit kinds
integer(int32) :: device_count
integer(int64) :: memory_size
real(real32) :: performance_score
real(real64) :: precision_value

! Bad - default kinds
integer :: count
real :: value
```

### Use Typed Constants
```fortran
! Good - typed constants
integer(int64), parameter :: GIGABYTE = 1024_int64**3
real(real64), parameter :: PI = 3.14159265359_real64

! Bad - untyped literals
size = 1073741824  ! What is this number?
angle = 3.14159    ! Single or double precision?
```

### Enumerations for States
```fortran
! Define clear enumerations
enum, bind(c)
  enumerator :: DEVICE_READY = 0
  enumerator :: DEVICE_BUSY = 1
  enumerator :: DEVICE_ERROR = 2
end enum

! Use them consistently
if (device%state == DEVICE_READY) then
  ! Clear what this means
end if
```

## 🎨 Code Layout

### Indentation
- 2 spaces per level (no tabs)
- Align related declarations

```fortran
type :: compute_kernel
  integer(int64)              :: work_items    = 0
  integer(int32)              :: block_size    = 256
  real(real64)                :: elapsed_time  = 0.0_real64
  character(len=:), allocatable :: name
end type
```

### Line Length
- Maximum 100 characters
- Break at logical points

```fortran
! Good - logical breaks
call complex_function( &
  input_array, size, &
  output_array, &
  options=compute_options, &
  status=ierr)

! Bad - arbitrary breaks
call complex_function(input_array, &
  size, output_array, options= &
  compute_options, status=ierr)
```

### Vertical Spacing
- One blank line between procedures
- Two blank lines between major sections
- Group related declarations

```fortran
module sparkle_arrays
  use iso_fortran_env
  implicit none
  
  ! Array operation types
  integer, parameter :: OP_ADD = 1
  integer, parameter :: OP_MUL = 2
  integer, parameter :: OP_DOT = 3
  
  ! Performance parameters  
  integer, parameter :: VECTOR_WIDTH = 8
  integer, parameter :: CACHE_SIZE = 32768
  
contains

  pure function vector_add(a, b) result(c)
    real(real64), intent(in) :: a(:), b(:)
    real(real64) :: c(size(a))
    
    c = a + b
  end function vector_add
  
  
  pure function vector_multiply(a, b) result(c)
    real(real64), intent(in) :: a(:), b(:)
    real(real64) :: c(size(a))
    
    c = a * b
  end function vector_multiply

end module
```

## 🛡️ Defensive Programming

### Always Check Allocations
```fortran
allocate(buffer(n), stat=ierr)
if (ierr /= 0) then
  error stop "Failed to allocate buffer"
end if
```

### Validate Inputs
```fortran
pure function safe_divide(a, b) result(c)
  real(real64), intent(in) :: a, b
  real(real64) :: c
  
  if (abs(b) < epsilon(b)) then
    c = 0.0_real64  ! Or handle error appropriately
  else
    c = a / b
  end if
end function
```

### Initialize Everything
```fortran
type :: device_stats
  integer(int64) :: total_memory    = 0
  integer(int64) :: used_memory     = 0  
  real(real32)   :: temperature     = 0.0_real32
  logical        :: is_initialized  = .false.
end type
```

## 📝 Documentation

### Module Documentation
```fortran
!> @brief High-performance memory pool for device allocations
!> 
!> This module provides thread-safe memory pooling with support
!> for both host and device memory. Features include:
!> - Aligned allocations for optimal performance
!> - Automatic defragmentation
!> - Usage statistics and profiling
module sparkle_memory_pool
```

### Procedure Documentation
```fortran
!> @brief Allocates aligned memory from the pool
!> @param size Number of bytes to allocate
!> @param alignment Required alignment (must be power of 2)
!> @return Handle to allocated memory, or null on failure
function pool_allocate(pool, size, alignment) result(handle)
```

### Inline Comments
```fortran
! Calculate optimal shard size based on cache hierarchy
shard_size = min(total_size / num_devices, L3_CACHE_SIZE)

! Ensure alignment for SIMD operations
shard_size = (shard_size / VECTOR_WIDTH) * VECTOR_WIDTH
```

## ⚡ Performance Patterns

### Array Operations
```fortran
! Good - let compiler optimize
result = a * b + c

! Bad - explicit loops when not needed
do i = 1, n
  result(i) = a(i) * b(i) + c(i)
end do
```

### Minimize Allocations
```fortran
! Good - reuse workspace
type :: compute_context
  real(real64), allocatable :: workspace(:)
contains
  procedure :: init => context_init
  procedure :: compute => context_compute
end type

! Bad - allocate in hot path
function compute(data)
  real(real64), allocatable :: temp(:)  ! Allocated every call
  allocate(temp(size(data)))
  ! ...
end function
```

## 🔍 Common Patterns

### Factory Functions
```fortran
function create_device(device_type, device_id) result(device)
  integer, intent(in) :: device_type
  integer, intent(in) :: device_id
  class(compute_device), allocatable :: device
  
  select case (device_type)
  case (DEVICE_CPU)
    allocate(cpu_device :: device)
  case (DEVICE_CUDA)
    allocate(cuda_device :: device)
  case default
    error stop "Unknown device type"
  end select
  
  call device%init(device_id)
end function
```

### Builder Pattern
```fortran
type :: kernel_builder
  private
  character(len=:), allocatable :: name
  integer(int32) :: block_size = 256
  integer(int32) :: grid_size = 0
contains
  procedure :: with_name => builder_with_name
  procedure :: with_block_size => builder_with_block_size
  procedure :: build => builder_build
end type
```

### RAII Pattern
```fortran
type :: scoped_timer
  private
  real(real64) :: start_time
  character(len=:), allocatable :: label
contains
  final :: timer_finalize
end type

! Usage - automatic timing
block
  type(scoped_timer) :: timer
  timer = scoped_timer("Critical Section")
  ! ... code to time ...
end block  ! Timer prints elapsed time
```

## ✅ Code Review Checklist

Before submitting code, ensure:

- [ ] All variables have explicit types with kinds
- [ ] `implicit none` in every module/procedure
- [ ] No magic numbers - use named constants
- [ ] Functions have clear input/output intent
- [ ] Error conditions are handled
- [ ] Memory is properly allocated/deallocated
- [ ] Code follows naming conventions
- [ ] Complex algorithms have explanatory comments
- [ ] New types have initialization defaults
- [ ] Performance-critical sections are optimized

## 🚀 Example: Well-Styled Module

```fortran
module sparkle_algorithms
  use iso_fortran_env, only: int32, int64, real32, real64
  use sparkle_types, only: compute_device
  use sparkle_memory, only: memory_handle
  implicit none
  private
  
  public :: parallel_reduce, reduction_op
  
  ! Reduction operations
  enum, bind(c)
    enumerator :: REDUCE_SUM = 0
    enumerator :: REDUCE_MAX = 1
    enumerator :: REDUCE_MIN = 2
  end enum
  
  ! Algorithm parameters
  integer(int32), parameter :: MIN_PARALLEL_SIZE = 1024
  integer(int32), parameter :: CACHE_LINE_ELEMENTS = 16
  
  type :: reduction_op
    integer :: operation = REDUCE_SUM
    logical :: is_commutative = .true.
  end type reduction_op
  
contains

  !> @brief Performs parallel reduction across devices
  !> @param devices Array of devices to use
  !> @param data Input data handle
  !> @param op Reduction operation
  !> @return Scalar result of reduction
  function parallel_reduce(devices, data, op) result(result)
    type(compute_device), intent(in) :: devices(:)
    type(memory_handle), intent(in) :: data
    type(reduction_op), intent(in) :: op
    real(real64) :: result
    
    integer(int64) :: chunk_size
    integer :: i, num_devices
    
    num_devices = size(devices)
    chunk_size = data%size / num_devices
    
    ! Ensure cache-friendly chunks
    chunk_size = align_to_cache(chunk_size)
    
    ! ... implementation ...
    
  contains
    
    pure function align_to_cache(size) result(aligned)
      integer(int64), intent(in) :: size
      integer(int64) :: aligned
      
      integer(int64), parameter :: CACHE_ALIGN = 64
      aligned = ((size + CACHE_ALIGN - 1) / CACHE_ALIGN) * CACHE_ALIGN
    end function align_to_cache
    
  end function parallel_reduce

end module sparkle_algorithms
```

## 📚 References

1. Fortran 2018 Standard
2. [Pythonic Principles](https://www.python.org/dev/peps/pep-0020/)
3. High Performance Fortran practices
4. Modern GPU programming patterns

---

Remember: We're not just writing code, we're crafting the foundation for democratized AI compute. Make it beautiful. Make it fast. Make it last.