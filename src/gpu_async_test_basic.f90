module gpu_async_test_basic
  use iso_c_binding
  use iso_fortran_env, only: real32, real64, int32, int64
  implicit none
  
  ! Test basic c_ptr
  type(c_ptr), parameter :: TEST_NULL = c_null_ptr
  
  ! Test simple type
  type :: buffer_set_simple
    integer :: input_buffer = 0
    type(c_ptr) :: fence = TEST_NULL
  end type buffer_set_simple

end module gpu_async_test_basic