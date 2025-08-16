! A simple test kernel to verify our parser
!@kernel(local_size_x=64, in=1, out=1)
pure subroutine simple_add(i, a, b, alpha)
  use iso_fortran_env
  integer(int32), value :: i
  real(real32), intent(in) :: a
  real(real32), intent(out) :: b
  integer(int32), value :: alpha
  
  b = a + real(alpha, real32)
end subroutine simple_add