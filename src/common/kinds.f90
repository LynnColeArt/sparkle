! Mini's Hardening Kit - Safe Kinds & Literals Module
! ==================================================
! 
! This module provides safe kind parameters and utility functions
! to prevent mixed-kind arithmetic bugs that cause performance 
! regressions and numerical issues.
!
! Key Features:
! - Standardized kind aliases (sp, dp, i32, i64)
! - Safe literal conversion functions
! - Prevents accidental mixed-precision arithmetic

module kinds
  use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
  implicit none
  
  ! Standardized kind parameters
  integer, parameter :: i32 = int32, i64 = int64, sp = real32, dp = real64
  
  public :: i32, i64, sp, dp
  public :: r32, r64
  
contains

  ! Safe real32 literal conversion
  pure elemental function r32(x) result(y)
    real(sp), intent(in) :: x
    real(sp)             :: y
    y = x
  end function r32

  ! Safe real64 literal conversion  
  pure elemental function r64(x) result(y)
    real(dp), intent(in) :: x
    real(dp)             :: y
    y = x
  end function r64

end module kinds