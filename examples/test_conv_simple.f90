! Simple test kernel for convolution parameters
!@kernel(local_size_x=256, in=2, out=1)  
pure subroutine conv_simple(idx, input, weights, output, height, width, channels)
  use kinds
  integer(i32), value :: idx
  real(sp), intent(in) :: input
  real(sp), intent(in) :: weights
  real(sp), intent(out) :: output
  integer(i32), value :: height
  integer(i32), value :: width
  integer(i32), value :: channels
  
  ! Simple computation
  output = input * weights * real(height * width * channels, real32)
end subroutine conv_simple