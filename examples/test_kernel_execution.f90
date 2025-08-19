program test_kernel_execution
  use kinds
  use iso_c_binding, only: c_f_pointer
  use sporkle_types
  use sporkle_memory
  use sporkle_mesh_types
  use sporkle_discovery
  use sporkle_kernels
  use sporkle_execute
  implicit none
  
  type(device_handle), allocatable :: devices(:)
  type(mesh_topology) :: mesh
  type(sporkle_kernel) :: kernel
  type(memory_handle) :: a, b, c
  real(sp), pointer :: a_ptr(:), b_ptr(:), c_ptr(:)
  integer :: num_devices, i
  integer(i64) :: n
  
  print *, "🧪 Testing Sporkle Kernel Execution"
  print *, "==================================="
  
  ! Discover devices  
  block
    type(mesh_topology) :: temp_mesh
    temp_mesh = scan_devices()
    num_devices = temp_mesh%num_devices
    allocate(devices(num_devices))
    devices = temp_mesh%devices
  end block
  print '(A,I0,A)', "Found ", num_devices, " devices"
  
  ! Create mesh
  mesh%num_devices = num_devices
  allocate(mesh%devices(num_devices))
  mesh%devices = devices
  
  ! Initialize links (fully connected for now)
  block
    integer :: i, j, link_count
    link_count = num_devices * (num_devices - 1)
    allocate(mesh%links(link_count))
    link_count = 0
    do i = 1, num_devices
      do j = 1, num_devices
        if (i /= j) then
          link_count = link_count + 1
          mesh%links(link_count)%src_id = devices(i)%id
          mesh%links(link_count)%dst_id = devices(j)%id
          mesh%links(link_count)%bw_gbs = 10.0_real64  ! Assume 10 GB/s
          mesh%links(link_count)%latency_us = 1.0_real64
        end if
      end do
    end do
  end block
  
  ! Problem size
  n = 1000000
  
  ! Allocate memory on host
  a = create_memory(n * 4_int64)  ! 4 bytes per float
  b = create_memory(n * 4_int64)
  c = create_memory(n * 4_int64)
  
  ! Get pointers for initialization
  call c_f_pointer(a%ptr, a_ptr, [n])
  call c_f_pointer(b%ptr, b_ptr, [n]) 
  call c_f_pointer(c%ptr, c_ptr, [n])
  
  ! Initialize data
  do i = 1, int(n)
    a_ptr(i) = real(i, real32)
    b_ptr(i) = real(i, real32) * 2.0_real32
    c_ptr(i) = 0.0_real32
  end do
  
  print *, ""
  print *, "📊 Vector Addition Example"
  print *, "-------------------------"
  print '(A,I0,A)', "Adding two vectors of ", n, " elements"
  
  ! Build kernel manually (builder pattern has issues with string literals)
  kernel%name = 'vector_add'
  kernel%kernel_type = KERNEL_PURE
  kernel%work_items = n
  kernel%fortran_proc => vector_add_kernel
  
  ! Set up arguments
  allocate(kernel%arguments(3))
  kernel%arguments(1)%name = 'a'
  kernel%arguments(1)%arg_type = TYPE_REAL32
  kernel%arguments(1)%intent = ARG_IN
  kernel%arguments(1)%rank = 1
  
  kernel%arguments(2)%name = 'b'
  kernel%arguments(2)%arg_type = TYPE_REAL32
  kernel%arguments(2)%intent = ARG_IN
  kernel%arguments(2)%rank = 1
  
  kernel%arguments(3)%name = 'c'
  kernel%arguments(3)%arg_type = TYPE_REAL32
  kernel%arguments(3)%intent = ARG_OUT
  kernel%arguments(3)%rank = 1
  
  ! Set the actual data
  kernel%arguments(1)%data = a
  kernel%arguments(2)%data = b
  kernel%arguments(3)%data = c
  
  ! Execute!
  call sporkle_run(kernel, mesh)
  
  ! Verify results
  print *, ""
  print *, "✅ Verifying results..."
  block
    logical :: correct
    real(sp) :: expected
    
    correct = .true.
    do i = 1, min(10_int64, n)
      expected = a_ptr(i) + b_ptr(i)
      if (abs(c_ptr(i) - expected) > 1.0e-6) then
        correct = .false.
        print '(A,I0,A,F10.3,A,F10.3)', &
          "ERROR at ", i, ": expected ", expected, " got ", c_ptr(i)
      end if
    end do
    
    if (correct) then
      print *, "✓ Results are correct!"
      print '(A,F10.3,A,F10.3,A,F10.3)', &
        "   First few results: c[1] = ", c_ptr(1), &
        ", c[2] = ", c_ptr(2), ", c[3] = ", c_ptr(3)
    end if
  end block
  
  ! Test reduction kernel
  print *, ""
  print *, "📊 Sum Reduction Example"
  print *, "-----------------------"
  
  ! Reset c to use as output for reduction
  c_ptr(1) = 0.0_real32
  
  ! Build reduction kernel manually
  kernel%name = 'sum_reduction'
  kernel%kernel_type = KERNEL_REDUCTION
  kernel%work_items = n
  kernel%fortran_proc => sum_reduction_kernel
  
  ! Reallocate arguments for new kernel
  deallocate(kernel%arguments)
  allocate(kernel%arguments(2))
  kernel%arguments(1)%name = 'input'
  kernel%arguments(1)%arg_type = TYPE_REAL32
  kernel%arguments(1)%intent = ARG_IN
  kernel%arguments(1)%rank = 1
  
  kernel%arguments(2)%name = 'sum'
  kernel%arguments(2)%arg_type = TYPE_REAL32
  kernel%arguments(2)%intent = ARG_OUT
  kernel%arguments(2)%rank = 0  ! Scalar output
  
  kernel%arguments(1)%data = a
  kernel%arguments(2)%data = c
  
  call sporkle_run(kernel, mesh)
  
  print '(A,ES12.5)', "Sum of all elements: ", c_ptr(1)
  block
    real(dp) :: expected_sum
    expected_sum = real(n, real64) * real(n + 1, real64) / 2.0_real64
    print '(A,ES12.5)', "Expected sum:        ", expected_sum
  end block
  
  ! Cleanup
  call destroy_memory(a)
  call destroy_memory(b)
  call destroy_memory(c)
  
  print *, ""
  print *, "🎉 Kernel execution test complete!"

contains

  ! Example kernel: Vector addition
  subroutine vector_add_kernel(args)
    type(kernel_argument), intent(inout) :: args(:)
    
    real(sp), pointer :: a(:), b(:), c(:)
    integer(i64) :: i, n
    
    ! Get data pointers
    ! In real implementation, these would be sliced based on chunk
    call c_f_pointer(args(1)%data%ptr, a, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, b, args(2)%shape)
    call c_f_pointer(args(3)%data%ptr, c, args(3)%shape)
    
    n = args(1)%shape(1)
    
    ! Pure Fortran array operation!
    do i = 1, n
      c(i) = a(i) + b(i)
    end do
    
  end subroutine vector_add_kernel
  
  ! Example kernel: Sum reduction
  subroutine sum_reduction_kernel(args)
    type(kernel_argument), intent(inout) :: args(:)
    
    real(sp), pointer :: input(:), sum_out(:)
    real(dp) :: sum
    integer(i64) :: i, n
    
    call c_f_pointer(args(1)%data%ptr, input, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, sum_out, [1_int64])
    
    n = args(1)%shape(1)
    
    ! Simple reduction - in real impl this would be chunked
    sum = 0.0_real64
    do i = 1, n
      sum = sum + real(input(i), real64)
    end do
    
    sum_out(1) = real(sum, real32)
    
  end subroutine sum_reduction_kernel

end program test_kernel_execution