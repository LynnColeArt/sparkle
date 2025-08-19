! Mini's Accuracy Checklist - 64-bit FLOP Counter Utility
! =======================================================
!
! This module provides overflow-safe FLOP counting for common operations.
! All calculations use 64-bit integers to prevent silent overflow.
!
! FLOP Policy: FMA (Fused Multiply-Add) counts as 2 FLOPs
! This is consistent across CPU and GPU implementations.

module flopcount
  use kinds
  implicit none
  
  private
  public :: gemm_flops, conv2d_flops, conv1d_flops
  public :: dot_product_flops, axpy_flops, reduction_flops
  public :: validate_flop_args

contains

  ! GEMM: C(MxN) = A(MxK) * B(KxN)
  ! Each output element requires K multiply-adds = K FMAs = 2*K FLOPs
  pure function gemm_flops(m, n, k) result(f)
    integer(i64), intent(in) :: m, n, k
    integer(i64) :: f
    
    f = 2_int64 * m * n * k
  end function gemm_flops

  ! Conv2D (NHWC format): 
  ! Each output element requires (C * Kh * Kw) multiply-adds
  ! Total output elements: N * H * W * K
  ! FLOP count: 2 * N * H * W * K * C * Kh * Kw
  pure function conv2d_flops(n, h, w, k, c, kh, kw) result(f)
    integer(i64), intent(in) :: n, h, w, k, c, kh, kw
    integer(i64) :: f
    
    f = 2_int64 * n * h * w * k * c * kh * kw
  end function conv2d_flops

  ! Conv1D: Similar to Conv2D but with 1D kernels
  ! FLOP count: 2 * N * L * K * C * Kl
  pure function conv1d_flops(n, l, k, c, kl) result(f)
    integer(i64), intent(in) :: n, l, k, c, kl
    integer(i64) :: f
    
    f = 2_int64 * n * l * k * c * kl
  end function conv1d_flops

  ! Dot product: sum(A[i] * B[i]) for i = 1..N
  ! N multiply-adds = N FMAs = 2*N FLOPs
  pure function dot_product_flops(n) result(f)
    integer(i64), intent(in) :: n
    integer(i64) :: f
    
    f = 2_int64 * n
  end function dot_product_flops

  ! AXPY: Y = A*X + Y (BLAS Level 1)
  ! N multiply-adds = N FMAs = 2*N FLOPs  
  pure function axpy_flops(n) result(f)
    integer(i64), intent(in) :: n
    integer(i64) :: f
    
    f = 2_int64 * n
  end function axpy_flops

  ! Reduction (sum): N-1 additions
  pure function reduction_flops(n) result(f)
    integer(i64), intent(in) :: n
    integer(i64) :: f
    
    f = max(0_int64, n - 1_int64)
  end function reduction_flops

  ! Validation helper: check for potential overflow in intermediate calculations
  pure function validate_flop_args(dims) result(valid)
    integer(i64), intent(in) :: dims(:)
    logical :: valid
    integer(i64) :: product
    integer :: i
    
    valid = .true.
    product = 1_int64
    
    ! Check for overflow in dimension products
    do i = 1, size(dims)
      if (dims(i) <= 0) then
        valid = .false.
        return
      end if
      
      ! Check if multiplication would overflow
      if (product > huge(product) / dims(i)) then
        valid = .false.
        return
      end if
      
      product = product * dims(i)
    end do
  end function validate_flop_args

end module flopcount