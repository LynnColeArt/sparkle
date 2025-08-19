! Mini's Hardening Kit - Safe C Pointer Utilities Module  
! ======================================================
!
! This module provides safe c_f_pointer operations that prevent
! kind mismatch bugs in array shape specifications.
!
! Key Features:
! - Automatic c_size_t to default integer conversion
! - Safe linear array mapping utilities
! - Eliminates c_f_pointer shape kind errors

module c_ptr_utils
  use iso_c_binding
  implicit none
  
  private
  public :: map_linear_real32, map_linear_real64
  public :: map_linear_int32, map_linear_int64

contains

  ! Safe c_f_pointer for real32 linear arrays
  subroutine map_linear_real32(cptr, n_c, fptr)
    type(c_ptr), intent(in) :: cptr
    integer(c_size_t), intent(in) :: n_c
    real(c_float), pointer, intent(out) :: fptr(:)
    integer :: n
    
    n = int(n_c, kind(n))          ! Convert to default int for shape & subscripts
    call c_f_pointer(cptr, fptr, [n])
  end subroutine map_linear_real32

  ! Safe c_f_pointer for real64 linear arrays
  subroutine map_linear_real64(cptr, n_c, fptr)
    type(c_ptr), intent(in) :: cptr
    integer(c_size_t), intent(in) :: n_c
    real(c_double), pointer, intent(out) :: fptr(:)
    integer :: n
    
    n = int(n_c, kind(n))          ! Convert to default int for shape & subscripts
    call c_f_pointer(cptr, fptr, [n])
  end subroutine map_linear_real64

  ! Safe c_f_pointer for int32 linear arrays
  subroutine map_linear_int32(cptr, n_c, fptr)
    type(c_ptr), intent(in) :: cptr
    integer(c_size_t), intent(in) :: n_c
    integer(c_int32_t), pointer, intent(out) :: fptr(:)
    integer :: n
    
    n = int(n_c, kind(n))          ! Convert to default int for shape & subscripts
    call c_f_pointer(cptr, fptr, [n])
  end subroutine map_linear_int32

  ! Safe c_f_pointer for int64 linear arrays
  subroutine map_linear_int64(cptr, n_c, fptr)
    type(c_ptr), intent(in) :: cptr
    integer(c_size_t), intent(in) :: n_c
    integer(c_int64_t), pointer, intent(out) :: fptr(:)
    integer :: n
    
    n = int(n_c, kind(n))          ! Convert to default int for shape & subscripts
    call c_f_pointer(cptr, fptr, [n])
  end subroutine map_linear_int64

end module c_ptr_utils