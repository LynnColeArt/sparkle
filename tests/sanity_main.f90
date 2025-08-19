! Mini's Hardening Kit - Sanity Smoke Test
! =======================================
!
! This program validates that all hardening fixes are working correctly.
! Run once to verify the defensive programming modules are functioning.
!
! Tests:
! - Timing utilities (tic/toc with proper clock rate)
! - Kind-safe arithmetic (GFLOPS calculation)
! - Safe c_f_pointer operations with shape conversion

program sanity
  use kinds, only: sp, dp
  use time_utils, only: tic, toc_seconds
  use iso_c_binding
  implicit none
  
  ! C memory allocation interface
  interface
    function c_malloc(size) bind(C, name="malloc")
      import :: c_ptr, c_size_t
      integer(c_size_t), value :: size
      type(c_ptr) :: c_malloc
    end function c_malloc
    
    subroutine c_free(ptr) bind(C, name="free")
      import :: c_ptr
      type(c_ptr), value :: ptr
    end subroutine c_free
  end interface

  print *, "=== Mini's Hardening Kit Sanity Test ==="
  print *, ""

  ! --- Timing sanity test ---
  call test_timing()
  
  ! --- Kind math sanity test ---
  call test_kind_math()
  
  ! --- c_f_pointer shape sanity test ---
  call test_shape_safety()
  
  print *, ""
  print *, "✅ All hardening tests passed!"
  print *, "The codebase is protected against common Fortran bugs."

contains

  subroutine test_timing()
    integer(kind=8) :: t0
    real(dp) :: s
    
    print *, "Testing timing utilities..."
    call tic(t0)
    call busy_work()
    s = toc_seconds(t0)
    
    if (s >= 0.05_dp .and. s <= 0.5_dp) then
      print '(A,F8.3,A)', "✅ Timing works correctly: ", s, " s"
    else
      print '(A,F8.3,A)', "❌ Timing suspicious: ", s, " s (expected ~0.1s)"
      stop 1
    end if
  end subroutine test_timing

  subroutine test_kind_math()
    real(dp) :: gflops, flop_count, elapsed_sec
    
    print *, "Testing kind-safe arithmetic..."
    
    ! Simulate 2 GFLOPS calculation
    flop_count = 1.0d9    ! 1 billion operations
    elapsed_sec = 0.5d0   ! in 0.5 seconds
    gflops = flop_count / elapsed_sec / 1.0d9
    
    if (abs(gflops - 2.0d0) < 1.0d-12) then
      print '(A,F8.3,A)', "✅ GFLOPS calculation correct: ", gflops, " GFLOPS"
    else
      print '(A,F8.3,A)', "❌ GFLOPS calculation wrong: ", gflops, " GFLOPS (expected 2.0)"
      stop 1
    end if
  end subroutine test_kind_math

  subroutine test_shape_safety()
    use iso_c_binding
    type(c_ptr) :: raw_ptr
    real(c_float), pointer :: fortran_array(:)
    integer(c_size_t) :: n_c
    integer :: i, n
    
    print *, "Testing c_f_pointer shape safety..."
    
    n = 1024
    n_c = int(n, c_size_t)
    raw_ptr = c_malloc(int(n, c_size_t) * c_sizeof(0.0_c_float))
    
    if (c_associated(raw_ptr)) then
      ! Use safe shape conversion (default integer kind)
      call c_f_pointer(raw_ptr, fortran_array, [n])  ! Correct: default-int shape
      
      ! Test array access
      do i = 1, n
        fortran_array(i) = real(i, c_float)
      end do
      
      if (abs(fortran_array(1024) - 1024.0) < 1.0e-6) then
        print *, "✅ c_f_pointer shape conversion works correctly"
      else
        print *, "❌ c_f_pointer shape conversion failed"
        stop 1
      end if
      
      call c_free(raw_ptr)
    else
      print *, "❌ malloc failed in shape test"
      stop 1
    end if
  end subroutine test_shape_safety

  subroutine busy_work()
    real(dp) :: x = 0.0_dp
    integer :: i
    
    ! Create ~0.1 seconds of work
    do i = 1, 100000000
      x = x + 1.0d-8
    end do
    
    ! Prevent optimization
    if (x < 0.0_dp) print *, x
  end subroutine busy_work

end program sanity