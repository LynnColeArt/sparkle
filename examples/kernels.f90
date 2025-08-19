! Test kernels for Fortran shader system

!@kernel(local_size_x=64, out=1)
pure elemental subroutine store_deadbeef(i, out)
  use kinds
  integer(i32), value :: i
  integer(i32), intent(out) :: out
  
  out = int(z'DEADBEEF', int32)
end subroutine store_deadbeef

!@kernel(local_size_x=256, in=1, out=1)
pure subroutine saxpy(i, a, x, y)
  use kinds
  integer(i32), value :: i
  real(sp), value :: a
  real(sp), intent(in) :: x
  real(sp), intent(inout) :: y
  
  y = a * x + y
end subroutine saxpy

!@kernel(local_size_x=256, in=1, out=1)
pure subroutine vector_add(i, x, y)
  use kinds
  integer(i32), value :: i
  real(sp), intent(in) :: x
  real(sp), intent(inout) :: y
  
  y = x + y
end subroutine vector_add

!@kernel(local_size_x=256, in=1, out=1)
pure subroutine vector_scale(i, scale, x)
  use kinds
  integer(i32), value :: i
  real(sp), value :: scale
  real(sp), intent(inout) :: x
  
  x = scale * x
end subroutine vector_scale