# Production Implementations

## 🚀 WHAT USERS ACTUALLY USE 🚀

This directory contains the production interfaces that link to the current best implementations.

## Rules

1. **INTERFACES ONLY** - These modules just select implementations
2. **ALWAYS STABLE** - Changes must maintain API compatibility  
3. **REFERENCE BY DEFAULT** - Link to reference/ unless overridden
4. **SWITCHABLE** - Allow runtime selection of implementations

## Structure

Each production module should follow this pattern:

```fortran
module sporkle_conv2d
  use sporkle_conv2d_reference  ! Default to reference
  implicit none
  
  private
  public :: conv2d, conv2d_select_implementation
  
  ! Allow runtime switching
  procedure(conv2d_interface), pointer :: conv2d_impl => conv2d_reference
  
contains
  
  subroutine conv2d(...)
    call conv2d_impl(...)
  end subroutine
  
  subroutine conv2d_select_implementation(impl_name)
    character(len=*), intent(in) :: impl_name
    
    select case(impl_name)
    case("reference")
      conv2d_impl => conv2d_reference
    case("experimental_im2col")
      use sporkle_conv2d_im2col
      conv2d_impl => conv2d_im2col
    case default
      print *, "Unknown implementation, using reference"
      conv2d_impl => conv2d_reference
    end select
  end subroutine
  
end module
```

## Current Production Modules

- [ ] `sporkle_conv2d.f90` - Convolution interface
- [ ] `sporkle_matmul.f90` - Matrix multiplication interface
- [ ] `sporkle_memory.f90` - Memory management interface

## Testing Requirements

Production modules must:
1. Pass all reference implementation tests
2. Maintain performance within 5% of reference
3. Support all documented APIs
4. Handle errors gracefully