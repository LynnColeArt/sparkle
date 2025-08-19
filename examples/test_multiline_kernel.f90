! Test kernel with multi-line signature
!@kernel(local_size_x=256, in=3, out=1)
pure subroutine multiline_test(idx, input1, input2, input3, output, &
                               height, width, channels, &
                               kernel_h, kernel_w, &
                               stride_h, stride_w, &
                               pad_h, pad_w)
  use kinds
  integer(i32), value :: idx
  real(sp), intent(in) :: input1, input2, input3
  real(sp), intent(out) :: output
  integer(i32), value :: height, width, channels
  integer(i32), value :: kernel_h, kernel_w
  integer(i32), value :: stride_h, stride_w  
  integer(i32), value :: pad_h, pad_w
  
  ! Simple test computation
  output = input1 * real(height * width, real32)
end subroutine multiline_test