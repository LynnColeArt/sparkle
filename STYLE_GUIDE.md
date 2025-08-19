# Sporkle Coding Standards & Style Guide

> "Code is read far more often than it is written." - Guido van Rossum  
> "Yeah, but let's make it fun to read." - The Sporkle Way

This guide ensures Sparkle code is elegant, strongly-typed, and actually enjoyable to work with. We write Fortran like it's 2024, not 1974.

## 🎯 Core Principles

1. **Strong Typing Always** - Every variable knows exactly what it is
2. **Explicit is Better than Implicit** - No guessing games, ever
3. **Elegance Without Elitism** - Beautiful code that a grad student can understand
4. **Consistency Without Conformity** - Same patterns, but room to breathe
5. **Self-Documenting** - If you need a decoder ring, rewrite it

## 📐 Module Structure (or: How to Not Write Spaghetti)

```fortran
module sporkle_example
  ! 1. Use statements (like a civilized person)
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_ptr, c_int
  use sporkle_types
  use sporkle_memory
  
  ! 2. Implicit none ALWAYS (because we're not psychics)
  implicit none
  
  ! 3. Access control (private by default, we're not exhibitionists)
  private
  public :: public_type, public_function
  
  ! 4. Module parameters (constants that actually make sense)
  integer, parameter :: CACHE_LINE_SIZE = 64
  real(real64), parameter :: EPSILON = 1.0e-12_real64  ! Not just "small number"
  
  ! 5. Type definitions (structs, but cooler)
  type :: example_type
    integer(int64) :: count = 0              ! Always initialize
    real(real64) :: value = 0.0_real64      ! Seriously, always
    logical :: is_valid = .false.            ! No uninitialized surprises
  contains
    procedure :: init => example_init
    procedure :: compute => example_compute
  end type example_type
  
contains
  ! Implementation here (the fun part)
end module sporkle_example
```

## 🔤 Naming Conventions (or: How to Not Name Things 'tmp2_final_v3')

### Types
- **PascalCase** for derived types: `DeviceHandle`, `MemoryPool`
- No cute abbreviations: `MessageQueue` not `MsgQ`
- Exception: `_t` suffix for enums only (because tradition)

### Variables
- **snake_case** for all variables: `device_count`, `memory_size`
- Full words: `buffer_size` not `buf_sz` or `bfr_sz` or whatever that is
- Boolean names that read like English: `is_ready`, `has_data`, `can_proceed`

### Functions/Subroutines
- **snake_case** that tells a story: `allocate_memory`, `plan_shards`
- Verb-first like you're giving orders: `create_device`, `destroy_empire`
- Questions get question names: `is_valid()`, `has_capability()`, `should_panic()`

### Constants
- **SCREAMING_SNAKE_CASE** because they're important: `MAX_DEVICES`, `DEFAULT_TIMEOUT`
- Include units or you're fired: `TIMEOUT_MS` not just `TIMEOUT`
- No magic numbers lurking around: `GIGABYTE` not `1073741824`

### Module Names
- **snake_case** with `sporkle_` prefix: `sporkle_memory`, `sporkle_wizardry`
- Say what it does: `sporkle_scheduler` not `sporkle_utils` (everything is utils)

## 💪 Strong Typing Rules (or: How to Not Shoot Yourself in the Foot)

### Always Specify Kind
```fortran
! Good - we know exactly what we're getting
integer(int32) :: device_count      ! 32-bit integer
integer(int64) :: memory_size       ! 64-bit for those big allocations
real(real32) :: performance_score   ! Single precision is fine here
real(real64) :: precision_value     ! Double for when it matters

! Bad - playing precision roulette
integer :: count      ! 16-bit? 32-bit? 64-bit? Who knows!
real :: value        ! Single? Double? Roll the dice!
```

### Use Typed Constants
```fortran
! Good - constants that explain themselves
integer(int64), parameter :: GIGABYTE = 1024_int64**3
real(real64), parameter :: PI = 3.14159265359_real64
real(real32), parameter :: GRAVITY = 9.81_real32  ! m/s²

! Bad - mystery numbers from the void
size = 1073741824  ! Is this... bytes? Megabytes? Bananas?
angle = 3.14159    ! Pi? 180 degrees? Your ex's birthday?
```

### Enumerations: Because Magic Numbers are Evil
```fortran
! Define states like a human would read them
enum, bind(c)
  enumerator :: DEVICE_READY = 0
  enumerator :: DEVICE_BUSY = 1  
  enumerator :: DEVICE_ON_FIRE = 2  ! It happens
end enum

! Use them so the code reads like English
if (device%state == DEVICE_ON_FIRE) then
  call evacuate_datacenter()
end if
```

## 🎨 Code Layout (or: How to Not Make Your Colleagues Cry)

### Indentation
- 2 spaces per level (tabs are for monsters)
- Align related declarations like a satisfaction video

```fortran
type :: compute_kernel
  integer(int64)                :: work_items    = 0
  integer(int32)                :: block_size    = 256
  real(real64)                  :: elapsed_time  = 0.0_real64
  character(len=:), allocatable :: name
  logical                       :: is_optimized  = .false.
end type
```

### Line Length
- 100 characters max (we have wide screens now, it's not 1980)
- Break at logical points, not randomly

```fortran
! Good - breaks that make sense
call ridiculously_long_function_name( &
  input_array, size, &
  output_array, &
  options=compute_options, &
  status=ierr)

! Bad - chaos breaks
call ridiculously_long_function_name(input_array, &
  size, output_array, options= &
  compute_options, status=ierr)  ! Why would you do this?
```

### Vertical Spacing
- One blank line between procedures (they need personal space)
- Two blank lines between major sections (like paragraph breaks)
- Group related stuff together (like organizing your desk)

```fortran
! Good spacing - room to breathe
module sporkle_arrays
  use iso_fortran_env
  implicit none
  
  ! Array operation types (grouped logically)
  integer, parameter :: OP_ADD = 1
  integer, parameter :: OP_MUL = 2
  integer, parameter :: OP_DOT = 3
  
  ! Performance parameters (another logical group)
  integer, parameter :: VECTOR_WIDTH = 8
  integer, parameter :: CACHE_SIZE = 32768
  
contains

  pure function vector_add(a, b) result(c)
    real(real64), intent(in) :: a(:), b(:)
    real(real64) :: c(size(a))
    
    c = a + b  ! Look how clean this is!
  end function vector_add
  
  
  pure function vector_multiply(a, b) result(c)
    real(real64), intent(in) :: a(:), b(:)
    real(real64) :: c(size(a))
    
    c = a * b  ! Fortran doing what it does best
  end function vector_multiply

end module
```

## 🛡️ Defensive Programming (or: How to Not Get Paged at 3 AM)

### Always Check Allocations
```fortran
! Because running out of memory shouldn't be a surprise party
allocate(buffer(n), stat=ierr)
if (ierr /= 0) then
  error stop "Failed to allocate buffer - probably need more RAM"
end if
```

### Validate Inputs Like You're Paranoid
```fortran
pure function safe_divide(a, b) result(c)
  real(real64), intent(in) :: a, b
  real(real64) :: c
  
  if (abs(b) < epsilon(b)) then
    c = 0.0_real64  ! Better than infinity
    ! In production, maybe log this somewhere
  else
    c = a / b
  end if
end function
```

### Initialize Everything (Seriously, Everything)
```fortran
type :: device_stats
  integer(int64) :: total_memory    = 0         ! Not garbage
  integer(int64) :: used_memory     = 0         ! Not random
  real(real32)   :: temperature     = 0.0_real32  ! Not on fire
  logical        :: is_initialized  = .false.   ! Explicitly not ready
end type
```

## 📝 Documentation (or: Future You Will Thank Present You)

### Module Documentation
```fortran
!> @brief High-performance memory pool for device allocations
!> 
!> This module provides thread-safe memory pooling that doesn't suck.
!> Features include:
!> - Aligned allocations (because cache lines matter)
!> - Automatic defragmentation (because fragmentation is sad)
!> - Usage statistics (for bragging rights)
!>
!> Example:
!>   pool = create_pool(gigabytes=16)
!>   mem = pool%allocate(megabytes=100)
!>   ! Do awesome compute stuff
!>   call pool%deallocate(mem)
module sporkle_memory_pool
```

### Inline Comments: Explain Why, Not What
```fortran
! BAD: Increment i by 1
i = i + 1

! GOOD: Skip header row in CSV
i = i + 1

! BETTER: Skip header row (parser expects data starting row 2)
i = i + 1
```

## ⚡ Performance Patterns (or: How to Go Fast Without Going Crazy)

### Let Fortran Be Fortran
```fortran
! Good - compiler knows what to do
result = a * b + c  ! Fused multiply-add, SIMD, all the goodies

! Bad - trying to outsmart the compiler
do i = 1, n
  result(i) = a(i) * b(i) + c(i)  ! Why did you do this?
end do
```

### Memory: Allocate Once, Compute Many
```fortran
! Good - reusable workspace
type :: compute_context
  real(real64), allocatable :: workspace(:)
  logical :: workspace_dirty = .true.
contains
  procedure :: ensure_workspace => context_ensure_workspace
  procedure :: compute => context_compute
end type

! Bad - malloc/free party
function compute(data)
  real(real64), allocatable :: temp(:)  ! Dies young
  allocate(temp(size(data)))            ! Born every call
  ! ... computations ...
  ! temp dies here, RIP
end function
```

## 🔍 Common Patterns (That Actually Work)

### Factory Functions (No Assembly Required)
```fortran
function create_device(device_type, device_id) result(device)
  integer, intent(in) :: device_type
  integer, intent(in) :: device_id
  class(compute_device), allocatable :: device
  
  select case (device_type)
  case (DEVICE_CPU)
    allocate(cpu_device :: device)
  case (DEVICE_AMD)
    allocate(amd_device :: device)
    ! No CUDA because we read The Sporkle Way
  case default
    error stop "Unknown device type - did you invent new hardware?"
  end select
  
  call device%init(device_id)
end function
```

### Builder Pattern (But Not Ridiculous)
```fortran
! For when constructors get out of hand
kernel = kernel_builder() &
  %with_name("matrix_multiply") &
  %with_block_size(256) &
  %with_inputs(a, b) &
  %with_output(c) &
  %build()
  
! So much cleaner than:
! kernel = create_kernel("matrix_multiply", 256, 0, a, b, c, null, null, 1, .false., 0)
```

## ✅ Code Review Checklist (The "Did You Really?" List)

Before submitting code, ensure:

- [ ] All variables have explicit types with kinds (no `integer :: i`)
- [ ] `implicit none` in every module (no exceptions, ever)
- [ ] No magic numbers (name your constants)
- [ ] Functions declare intent (in, out, inout - pick one)
- [ ] Errors handled (not just ignored)
- [ ] Memory freed (what goes up must come down)
- [ ] Names make sense (to humans, not just you)
- [ ] Comments explain why (code already shows what)
- [ ] Initialized all the things (no garbage values)
- [ ] Didn't try to outsmart the compiler (it's smarter than you)

## 🚀 Example: Code That Doesn't Suck

```fortran
module sporkle_algorithms
  use iso_fortran_env, only: int32, int64, real32, real64
  use sporkle_types, only: compute_device
  use sporkle_memory, only: memory_handle
  implicit none
  private
  
  public :: parallel_reduce, reduction_op
  
  ! Reduction operations (human-readable)
  enum, bind(c)
    enumerator :: REDUCE_SUM = 0
    enumerator :: REDUCE_MAX = 1
    enumerator :: REDUCE_MIN = 2
    enumerator :: REDUCE_SANITY = 3  ! Custom for debugging
  end enum
  
  ! Algorithm parameters (with actual explanations)
  integer(int32), parameter :: MIN_PARALLEL_SIZE = 1024    ! Below this, overhead > benefit
  integer(int32), parameter :: CACHE_LINE_ELEMENTS = 16    ! 64 bytes / 4 bytes per real32
  
  type :: reduction_op
    integer :: operation = REDUCE_SUM
    logical :: is_commutative = .true.
    character(len=:), allocatable :: name  ! For debugging/profiling
  end type reduction_op
  
contains

  !> Performs parallel reduction without making you cry
  !> 
  !> Takes your data, spreads it across devices like butter on toast,
  !> reduces it down to a single value. Works with any reduction op
  !> that makes mathematical sense.
  !>
  !> Example:
  !>   sum_of_all_things = parallel_reduce(devices, huge_array, sum_op)
  !>   ! Now you have the sum without melting your laptop
  function parallel_reduce(devices, data, op) result(result)
    type(compute_device), intent(in) :: devices(:)
    type(memory_handle), intent(in) :: data
    type(reduction_op), intent(in) :: op
    real(real64) :: result
    
    integer(int64) :: chunk_size
    integer :: i, num_devices
    
    ! Sanity checks (because users are creative)
    if (size(devices) == 0) then
      error stop "No devices provided - did you forget to initialize?"
    end if
    
    if (data%size == 0) then
      result = 0.0_real64  ! Empty sum is zero, fight me
      return
    end if
    
    num_devices = size(devices)
    chunk_size = data%size / num_devices
    
    ! Ensure cache-friendly chunks (your CPU will thank you)
    chunk_size = align_to_cache(chunk_size)
    
    ! ... rest of parallel magic here ...
    
  contains
    
    pure function align_to_cache(size) result(aligned)
      integer(int64), intent(in) :: size
      integer(int64) :: aligned
      
      integer(int64), parameter :: CACHE_ALIGN = 64  ! Standard cache line
      aligned = ((size + CACHE_ALIGN - 1) / CACHE_ALIGN) * CACHE_ALIGN
    end function align_to_cache
    
  end function parallel_reduce

end module sporkle_algorithms
```

## 📚 References (For the Curious)

1. Modern Fortran Explained (because it's not your grandpa's Fortran)
2. [The Zen of Python](https://www.python.org/dev/peps/pep-0020/) (we steal the good parts)
3. [The Sporkle Way](docs/the_sporkle_way.md) (required reading)
4. Your future self (who has to maintain this code)

---

Remember: We're not just writing code, we're building a compute revolution. Make it beautiful. Make it fast. Make it so clean that even physicists complain it's too readable.