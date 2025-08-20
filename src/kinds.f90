module kinds
  ! Standard kind definitions for Sporkle
  use iso_fortran_env, only: &
    int8, int16, int32, int64, &
    real32, real64, real128
  
  implicit none
  
  ! Integer kinds
  integer, parameter :: i8 = int8
  integer, parameter :: i16 = int16
  integer, parameter :: i32 = int32
  integer, parameter :: i64 = int64
  
  ! Real kinds
  integer, parameter :: sp = real32
  integer, parameter :: dp = real64
  integer, parameter :: qp = real128
  
  ! Legacy support
  integer, parameter :: rk32 = real32
  integer, parameter :: rk64 = real64
  
end module kinds