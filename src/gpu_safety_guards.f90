module gpu_safety_guards
  ! GPU Safety Guards - Prevent System Freezes
  ! ==========================================
  ! 
  ! This module provides safety checks to prevent dangerous GPU operations
  ! that could cause system hangs or kernel panics.
  
  use kinds
  implicit none
  private
  
  public :: gpu_safety_enabled, gpu_enable_safety_mode
  public :: gpu_validate_address, gpu_check_submission_safety
  
  ! Safety state
  logical, save :: safety_mode_enabled = .true.
  
  ! Safe address ranges (conservative)
  integer(i64), parameter :: MIN_SAFE_VA = int(z'100000', int64)    ! 1MB
  integer(i64), parameter :: MAX_SAFE_VA = int(z'1000000', int64)   ! 16MB
  
contains

  ! Check if safety mode is enabled
  logical function gpu_safety_enabled()
    gpu_safety_enabled = safety_mode_enabled
  end function gpu_safety_enabled
  
  ! Enable/disable safety mode (expert use only)
  subroutine gpu_enable_safety_mode(enabled)
    logical, intent(in) :: enabled
    
    if (.not. enabled) then
      print *, "⚠️ WARNING: GPU safety mode disabled"
      print *, "   This allows dangerous operations that may freeze the system"
      print *, "   Only disable for expert debugging with proper precautions"
    else
      print *, "✅ GPU safety mode enabled"
    end if
    
    safety_mode_enabled = enabled
  end subroutine gpu_enable_safety_mode
  
  ! Validate a GPU virtual address is safe
  logical function gpu_validate_address(va_addr, size, description)
    integer(i64), intent(in) :: va_addr
    integer(i64), intent(in) :: size
    character(len=*), intent(in) :: description
    
    gpu_validate_address = .false.
    
    if (.not. safety_mode_enabled) then
      gpu_validate_address = .true.
      return
    end if
    
    ! Check address range
    if (va_addr < MIN_SAFE_VA .or. va_addr > MAX_SAFE_VA) then
      print '(A,A,A,Z16)', "❌ SAFETY: ", description, " address unsafe: 0x", va_addr
      print '(A,Z16,A,Z16)', "   Safe range: 0x", MIN_SAFE_VA, " - 0x", MAX_SAFE_VA
      return
    end if
    
    ! Check size bounds
    if (va_addr + size > MAX_SAFE_VA) then
      print '(A,A)', "❌ SAFETY: ", description, " extends beyond safe range"
      return
    end if
    
    ! Check alignment
    if (mod(va_addr, 4096_int64) /= 0) then
      print '(A,A)', "❌ SAFETY: ", description, " not page-aligned"
      return
    end if
    
    print '(A,A,A,Z16)', "✅ SAFETY: ", description, " address validated: 0x", va_addr
    gpu_validate_address = .true.
    
  end function gpu_validate_address
  
  ! Check if command submission is safe
  logical function gpu_check_submission_safety()
    gpu_check_submission_safety = .false.
    
    if (safety_mode_enabled) then
      print *, "❌ SAFETY: Direct GPU command submission blocked"
      print *, "   Use gpu_enable_safety_mode(.false.) to override"
      print *, "   WARNING: This may cause system freezes!"
      return
    end if
    
    print *, "⚠️ SAFETY: Direct GPU submission allowed (safety mode disabled)"
    gpu_check_submission_safety = .true.
    
  end function gpu_check_submission_safety

end module gpu_safety_guards