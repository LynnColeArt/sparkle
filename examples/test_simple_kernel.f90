! A simple test kernel to verify our parser
!@kernel(local_size_x=64, in=1, out=1)
pure subroutine simple_add(i, a, b, alpha)
  use kinds
  integer(i32), value :: i
  real(sp), intent(in) :: a
  real(sp), intent(out) :: b
  integer(i32), value :: alpha
  
  b = a + real(alpha, real32)
end subroutine simple_add