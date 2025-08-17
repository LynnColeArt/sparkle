! Simple reference GEMM for debugging
module gemm_reference
  use iso_fortran_env, only: real32
  implicit none
  
  private
  public :: gemm_ref
  
contains

  ! Reference GEMM: C = alpha * A * B + beta * C
  ! A is m x k, B is k x n, C is m x n
  ! All stored in column-major order
  subroutine gemm_ref(A, B, C, m, n, k, alpha, beta)
    real(real32), intent(in) :: A(:), B(:), alpha, beta
    real(real32), intent(inout) :: C(:)
    integer, intent(in) :: m, n, k
    
    integer :: i, j, kk
    real(real32) :: sum
    
    ! Scale C by beta
    if (beta == 0.0) then
      C = 0.0
    else
      C = beta * C
    end if
    
    ! Compute C = C + alpha * A * B
    do j = 1, n
      do i = 1, m
        sum = 0.0
        do kk = 1, k
          ! A(i,kk) = A((kk-1)*m + i)
          ! B(kk,j) = B((j-1)*k + kk) <- WRONG!
          ! B is k x n, so B(kk,j) = B((j-1)*k + kk) is correct
          sum = sum + A((kk-1)*m + i) * B((j-1)*k + kk)
        end do
        ! C(i,j) = C((j-1)*m + i)
        C((j-1)*m + i) = C((j-1)*m + i) + alpha * sum
      end do
    end do
    
  end subroutine gemm_ref

end module gemm_reference