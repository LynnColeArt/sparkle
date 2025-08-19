program test_kernel_debug
  use kinds
  use iso_c_binding, only: c_f_pointer
  use sporkle_types
  use sporkle_memory
  use sporkle_kernels
  implicit none
  
  type(sporkle_kernel) :: kernel
  type(memory_handle) :: data_mem
  real(sp), pointer :: data(:)
  integer :: i
  
  print *, "🧪 Kernel Debug Test"
  print *, "==================="
  
  ! Allocate memory for 10 elements
  data_mem = create_memory(10_int64 * 4_int64)
  call c_f_pointer(data_mem%ptr, data, [10_int64])
  
  ! Initialize data
  do i = 1, 10
    data(i) = real(i, real32)
  end do
  
  print *, "Initial data:", data
  
  ! Build kernel manually
  kernel%name = 'test_kernel'
  kernel%kernel_type = KERNEL_PURE
  kernel%work_items = 10
  kernel%fortran_proc => simple_kernel
  
  allocate(kernel%arguments(1))
  kernel%arguments(1)%name = 'data'
  kernel%arguments(1)%arg_type = TYPE_REAL32
  kernel%arguments(1)%intent = ARG_INOUT
  kernel%arguments(1)%rank = 1
  allocate(kernel%arguments(1)%shape(1))
  kernel%arguments(1)%shape(1) = 10
  kernel%arguments(1)%data = data_mem
  
  ! Test direct call
  print *, ""
  print *, "Testing direct kernel call..."
  call kernel%fortran_proc(kernel%arguments)
  
  print *, "After kernel:", data
  
  ! Cleanup
  call destroy_memory(data_mem)
  
  print *, ""
  print *, "✓ Debug test complete!"

contains

  subroutine simple_kernel(args)
    type(kernel_argument), intent(inout) :: args(:)
    real(sp), pointer :: data(:)
    integer :: i
    
    print *, "  Inside kernel, num args:", size(args)
    print *, "  Arg 1 name:", args(1)%name
    print *, "  Arg 1 shape:", args(1)%shape
    
    call c_f_pointer(args(1)%data%ptr, data, args(1)%shape)
    
    ! Square each element
    do i = 1, int(args(1)%shape(1))
      data(i) = data(i) * data(i)
    end do
    
  end subroutine simple_kernel

end program test_kernel_debug