! Simple test kernel for convolution parameters
!@kernel(local_size_x=256, in=2, out=1)  
pure subroutine conv_simple(idx, input, weights, output, height, width, channels)
  use iso_fortran_env
  integer(int32), value :: idx
  real(real32), intent(in) :: input
  real(real32), intent(in) :: weights
  real(real32), intent(out) :: output
  integer(int32), value :: height
  integer(int32), value :: width
  integer(int32), value :: channels
  
  ! Simple computation
  output = input * weights * real(height * width * channels, real32)
end subroutine conv_simple