! Mini's Accuracy Checklist - Stable Mathematical Operations
! =========================================================
!
! This module provides numerically stable implementations of common
! mathematical operations that prevent reduction order drift and
! maintain reproducible results across different execution environments.

module stable_math
  use kinds, only: dp, sp
  use kinds
  implicit none
  
  private
  public :: pairwise_sum, stable_dot_product, compensated_sum
  public :: stable_norm2, relative_error_l2, max_absolute_error
  public :: check_numeric_drift

  ! Tolerance constants for validation
  real(dp), parameter :: DEFAULT_REL_TOL = 1.0d-5
  real(dp), parameter :: DEFAULT_ABS_TOL = 1.0d-6

contains

  ! Pairwise sum - stable recursive summation (O(log N) error vs O(N))
  pure recursive function pairwise_sum_dp(x, l, r) result(s)
    real(dp), intent(in) :: x(:)
    integer, intent(in) :: l, r
    real(dp) :: s
    integer :: m
    
    if (r - l < 32) then
      ! Base case: direct summation for small arrays
      s = sum(x(l:r))
    else
      ! Recursive case: divide and conquer
      m = (l + r) / 2
      s = pairwise_sum_dp(x, l, m) + pairwise_sum_dp(x, m + 1, r)
    end if
  end function pairwise_sum_dp

  ! Public interface for pairwise sum
  pure function pairwise_sum(x) result(s)
    real(dp), intent(in) :: x(:)
    real(dp) :: s
    
    if (size(x) == 0) then
      s = 0.0_dp
    else
      s = pairwise_sum_dp(x, 1, size(x))
    end if
  end function pairwise_sum

  ! Stable dot product using pairwise summation
  pure function stable_dot_product(x, y) result(dot)
    real(dp), intent(in) :: x(:), y(:)
    real(dp) :: dot
    real(dp), allocatable :: products(:)
    integer :: i
    
    if (size(x) /= size(y)) then
      dot = 0.0_dp
      return
    end if
    
    allocate(products(size(x)))
    do i = 1, size(x)
      products(i) = x(i) * y(i)
    end do
    
    dot = pairwise_sum(products)
  end function stable_dot_product

  ! Kahan compensated summation for very long sequences
  pure function compensated_sum(x) result(s)
    real(dp), intent(in) :: x(:)
    real(dp) :: s, c, y, t
    integer :: i
    
    s = 0.0_dp
    c = 0.0_dp  ! Compensation for lost low-order bits
    
    do i = 1, size(x)
      y = x(i) - c        ! Compensated input
      t = s + y           ! New sum
      c = (t - s) - y     ! Compensation for next iteration
      s = t
    end do
  end function compensated_sum

  ! Stable norm calculation using pairwise summation
  pure function stable_norm2(x) result(norm)
    real(dp), intent(in) :: x(:)
    real(dp) :: norm
    real(dp), allocatable :: squares(:)
    integer :: i
    
    allocate(squares(size(x)))
    do i = 1, size(x)
      squares(i) = x(i) * x(i)
    end do
    
    norm = sqrt(pairwise_sum(squares))
  end function stable_norm2

  ! Relative L2 error between two arrays
  pure function relative_error_l2(y, y_ref) result(rel_error)
    real(dp), intent(in) :: y(:), y_ref(:)
    real(dp) :: rel_error
    real(dp) :: diff_norm, ref_norm
    real(dp), allocatable :: diff(:)
    integer :: i
    
    if (size(y) /= size(y_ref)) then
      rel_error = huge(rel_error)
      return
    end if
    
    allocate(diff(size(y)))
    do i = 1, size(y)
      diff(i) = y(i) - y_ref(i)
    end do
    
    diff_norm = stable_norm2(diff)
    ref_norm = stable_norm2(y_ref)
    
    ! Avoid division by zero
    rel_error = diff_norm / max(1.0d-30, ref_norm)
  end function relative_error_l2

  ! Maximum absolute error between two arrays
  pure function max_absolute_error(y, y_ref) result(max_err)
    real(dp), intent(in) :: y(:), y_ref(:)
    real(dp) :: max_err
    integer :: i
    
    max_err = 0.0_dp
    if (size(y) /= size(y_ref)) then
      max_err = huge(max_err)
      return
    end if
    
    do i = 1, size(y)
      max_err = max(max_err, abs(y(i) - y_ref(i)))
    end do
  end function max_absolute_error

  ! Check for numeric drift with configurable tolerances
  function check_numeric_drift(y, y_ref, rel_tol, abs_tol, operation_name) result(passed)
    real(dp), intent(in) :: y(:), y_ref(:)
    real(dp), intent(in), optional :: rel_tol, abs_tol
    character(len=*), intent(in), optional :: operation_name
    logical :: passed
    
    real(dp) :: rel_error, abs_error
    real(dp) :: rtol, atol
    character(len=50) :: op_name
    
    ! Set defaults
    rtol = DEFAULT_REL_TOL
    atol = DEFAULT_ABS_TOL
    op_name = "numeric operation"
    
    if (present(rel_tol)) rtol = rel_tol
    if (present(abs_tol)) atol = abs_tol
    if (present(operation_name)) op_name = operation_name
    
    rel_error = relative_error_l2(y, y_ref)
    abs_error = max_absolute_error(y, y_ref)
    
    passed = (rel_error <= rtol) .and. (abs_error <= atol)
    
    if (.not. passed) then
      print '(A,A)', "❌ Numeric drift detected in: ", trim(op_name)
      print '(A,E12.5,A,E12.5)', "   Relative L2 error: ", rel_error, " (tolerance: ", rtol, ")"
      print '(A,E12.5,A,E12.5)', "   Max absolute error: ", abs_error, " (tolerance: ", atol, ")"
    else
      print '(A,A)', "✅ Numeric validation passed: ", trim(op_name)
      print '(A,E12.5,A,E12.5)', "   Relative L2 error: ", rel_error, " Max abs error: ", abs_error
    end if
  end function check_numeric_drift

end module stable_math