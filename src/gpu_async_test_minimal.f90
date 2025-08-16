module gpu_async_test_minimal
  use iso_c_binding
  use iso_fortran_env, only: real32, real64, int32, int64
  implicit none
  
  ! Test with just the fence type
  type(c_ptr), parameter :: GL_SYNC_NULL = c_null_ptr
  
  ! Simple buffer state
  type :: buffer_set_test
    integer :: input_buffer = 0
    integer :: output_buffer = 0
    type(c_ptr) :: fence = GL_SYNC_NULL
  end type buffer_set_test

end module gpu_async_test_minimal