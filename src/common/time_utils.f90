! Mini's Hardening Kit - Safe Timing Utilities Module
! ===================================================
!
! This module provides correct system_clock math to prevent timing bugs.
! Fixes the common mistake of treating clock ticks as nanoseconds.
!
! Key Features:
! - Proper clock rate handling with caching
! - Safe tick-to-seconds conversion
! - Easy-to-use tic/toc interface
! - Eliminates system_clock arithmetic errors

module time_utils
  use kinds
  implicit none
  
  private
  public :: tic, toc_seconds, ticks_to_seconds, clock_rate_hz
  public :: tic_safe, toc_seconds_safe
  
  ! Cached clock rate to avoid repeated system calls
  integer(i64) :: clock_rate_cached = -1_int64

contains

  ! Get system clock rate (cached for performance)
  subroutine clock_rate_hz(rate)
    integer(i64), intent(out) :: rate
    integer :: r32
    
    if (clock_rate_cached < 0) then
      call system_clock(count_rate=r32)
      clock_rate_cached = int(r32, int64)
    end if
    rate = clock_rate_cached
  end subroutine clock_rate_hz

  ! Start timing - capture current tick count
  subroutine tic(t0)
    integer(i64), intent(out) :: t0
    integer :: c
    
    call system_clock(count=c)
    t0 = int(c, int64)
  end subroutine tic

  ! Safe timing with wraparound detection
  subroutine tic_safe(t0, success)
    integer(i64), intent(out) :: t0
    logical, intent(out) :: success
    integer :: c
    
    call system_clock(count=c)
    t0 = int(c, int64)
    success = .true.
    
    ! Note: Wraparound detection happens in toc_safe
  end subroutine tic_safe

  ! End timing - return elapsed seconds since tic()
  function toc_seconds(t0) result(s)
    integer(i64), intent(in) :: t0
    real(dp) :: s
    integer :: c
    integer(i64) :: rate, t1
    
    call system_clock(count=c)
    t1 = int(c, int64)
    call clock_rate_hz(rate)
    s = real(t1 - t0, real64) / real(rate, real64)
  end function toc_seconds

  ! Safe timing with wraparound detection
  function toc_seconds_safe(t0, success) result(s)
    integer(i64), intent(in) :: t0
    logical, intent(out) :: success
    real(dp) :: s
    integer :: c
    integer(i64) :: rate, t1
    
    call system_clock(count=c)
    t1 = int(c, int64)
    call clock_rate_hz(rate)
    
    ! Check for wraparound
    if (t1 < t0) then
      success = .false.
      s = 0.0_real64
      return
    end if
    
    success = .true.
    s = real(t1 - t0, real64) / real(rate, real64)
  end function toc_seconds_safe

  ! Convert raw tick count to seconds (for accumulated timing)
  ! Note: Not pure due to clock_rate_hz call - use with rate parameter for pure contexts
  function ticks_to_seconds(ticks) result(s)
    integer(i64), intent(in) :: ticks
    real(dp) :: s
    integer(i64) :: rate
    
    call clock_rate_hz(rate)
    s = real(ticks, real64) / real(rate, real64)
  end function ticks_to_seconds

end module time_utils