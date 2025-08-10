program test_benchmarks
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_f_pointer
  use sparkle_types
  use sparkle_memory
  use sparkle_mesh_types
  use sparkle_discovery
  use sparkle_kernels
  use sparkle_execute
  use sparkle_benchmark
  implicit none
  
  type(device_handle), allocatable :: devices(:)
  type(mesh_topology) :: mesh
  type(sparkle_kernel), allocatable :: kernels(:)
  type(benchmark_suite) :: suite
  type(memory_handle) :: a, b, c
  real(real32), pointer :: a_ptr(:), b_ptr(:), c_ptr(:)
  integer :: num_devices, i
  integer(int64), parameter :: sizes(5) = [1024_int64, 10240_int64, 102400_int64, &
                                           1024000_int64, 10240000_int64]
  character(len=32) :: size_names(5) = ["1K   ", "10K  ", "100K ", "1M   ", "10M  "]
  
  print *, "🚀 Sparkle Hot/Cold Benchmarking"
  print *, "================================"
  print *, ""
  print *, "Demonstrating cache warming effects and performance measurement"
  print *, ""
  
  ! Setup devices
  block
    type(mesh_topology) :: temp_mesh
    temp_mesh = scan_devices()
    num_devices = temp_mesh%num_devices
    allocate(devices(num_devices))
    devices = temp_mesh%devices
  end block
  
  ! Create mesh
  mesh%num_devices = num_devices
  allocate(mesh%devices(num_devices))
  mesh%devices = devices
  
  ! Run benchmarks for different sizes
  do i = 1, 5
    print *, ""
    print '(A,A,A,I0,A)', "━━━ Testing size ", trim(size_names(i)), &
           " (", sizes(i), " elements) ━━━"
    
    suite = create_benchmark_suite("Vector ops @ " // trim(size_names(i)))
    
    ! Allocate memory for this size
    a = create_memory(sizes(i) * 4_int64)
    b = create_memory(sizes(i) * 4_int64)
    c = create_memory(sizes(i) * 4_int64)
    
    ! Initialize data
    call c_f_pointer(a%ptr, a_ptr, [sizes(i)])
    call c_f_pointer(b%ptr, b_ptr, [sizes(i)])
    call c_f_pointer(c%ptr, c_ptr, [sizes(i)])
    
    a_ptr = 1.0_real32
    b_ptr = 2.0_real32
    c_ptr = 0.0_real32
    
    ! Create test kernels
    allocate(kernels(3))
    
    ! Vector addition
    kernels(1) = create_vector_add_kernel(sizes(i), a, b, c)
    
    ! Vector scale
    kernels(2) = create_vector_scale_kernel(sizes(i), a, c)
    
    ! Vector dot product (reduction)
    kernels(3) = create_dot_product_kernel(sizes(i), a, b, c)
    
    ! Run benchmarks with different warmup settings
    print *, ""
    print *, "Testing with minimal warmup (2 runs)..."
    block
      type(benchmark_result) :: result
      result = benchmark_kernel(kernels(1), mesh, warmup_runs=2, bench_runs=20)
      print '(A,F8.2,A,F8.2,A,F5.1,A)', &
        "  Cold: ", result%cold_time_ms, " ms, Hot: ", result%hot_time_ms, &
        " ms (", result%speedup(), "x speedup)"
    end block
    
    print *, ""
    print *, "Testing with standard warmup (5 runs)..."
    block
      type(benchmark_result) :: result
      result = benchmark_kernel(kernels(1), mesh, warmup_runs=5, bench_runs=50)
      print '(A,F8.2,A,F8.2,A,F5.1,A)', &
        "  Cold: ", result%cold_time_ms, " ms, Hot: ", result%hot_time_ms, &
        " ms (", result%speedup(), "x speedup)"
    end block
    
    print *, ""
    print *, "Testing with extensive warmup (20 runs)..."
    block
      type(benchmark_result) :: result
      result = benchmark_kernel(kernels(1), mesh, warmup_runs=20, bench_runs=100)
      call result%print()
    end block
    
    ! Run full benchmark suite
    print *, "Running full kernel suite..."
    call suite%run(kernels, mesh)
    call suite%report()
    
    ! Cleanup
    call destroy_memory(a)
    call destroy_memory(b)
    call destroy_memory(c)
    deallocate(kernels)
  end do
  
  print *, ""
  print *, "🎯 Key Observations:"
  print *, "─────────────────────"
  print *, "• Cold runs include cache population and initialization overhead"
  print *, "• Hot runs show steady-state performance after warming"
  print *, "• Smaller data sizes may fit in L1/L2 cache showing huge speedups"
  print *, "• Larger data sizes stress memory bandwidth"
  print *, "• Standard deviation shows performance consistency"
  print *, ""
  print *, "The Sparkle Way: Measure twice, optimize once! ✨"

contains

  function create_vector_add_kernel(n, a_mem, b_mem, c_mem) result(kernel)
    integer(int64), intent(in) :: n
    type(memory_handle), intent(in) :: a_mem, b_mem, c_mem
    type(sparkle_kernel) :: kernel
    
    kernel%name = 'vector_add'
    kernel%kernel_type = KERNEL_PURE
    kernel%work_items = n
    kernel%fortran_proc => vector_add_impl
    kernel%arithmetic_intensity = 1.0_real64  ! 1 FLOP per 3 memory accesses
    
    allocate(kernel%arguments(3))
    kernel%arguments(1)%name = 'a'
    kernel%arguments(1)%arg_type = TYPE_REAL32
    kernel%arguments(1)%intent = ARG_IN
    kernel%arguments(1)%data = a_mem
    allocate(kernel%arguments(1)%shape(1))
    kernel%arguments(1)%shape(1) = n
    
    kernel%arguments(2)%name = 'b'
    kernel%arguments(2)%arg_type = TYPE_REAL32
    kernel%arguments(2)%intent = ARG_IN
    kernel%arguments(2)%data = b_mem
    allocate(kernel%arguments(2)%shape(1))
    kernel%arguments(2)%shape(1) = n
    
    kernel%arguments(3)%name = 'c'
    kernel%arguments(3)%arg_type = TYPE_REAL32
    kernel%arguments(3)%intent = ARG_OUT
    kernel%arguments(3)%data = c_mem
    allocate(kernel%arguments(3)%shape(1))
    kernel%arguments(3)%shape(1) = n
    
  end function create_vector_add_kernel
  
  function create_vector_scale_kernel(n, a_mem, c_mem) result(kernel)
    integer(int64), intent(in) :: n
    type(memory_handle), intent(in) :: a_mem, c_mem
    type(sparkle_kernel) :: kernel
    
    kernel%name = 'vector_scale'
    kernel%kernel_type = KERNEL_PURE
    kernel%work_items = n
    kernel%fortran_proc => vector_scale_impl
    kernel%arithmetic_intensity = 1.0_real64  ! 1 FLOP per 2 memory accesses
    
    allocate(kernel%arguments(2))
    kernel%arguments(1)%name = 'a'
    kernel%arguments(1)%arg_type = TYPE_REAL32
    kernel%arguments(1)%intent = ARG_IN
    kernel%arguments(1)%data = a_mem
    allocate(kernel%arguments(1)%shape(1))
    kernel%arguments(1)%shape(1) = n
    
    kernel%arguments(2)%name = 'c'
    kernel%arguments(2)%arg_type = TYPE_REAL32
    kernel%arguments(2)%intent = ARG_OUT
    kernel%arguments(2)%data = c_mem
    allocate(kernel%arguments(2)%shape(1))
    kernel%arguments(2)%shape(1) = n
    
  end function create_vector_scale_kernel
  
  function create_dot_product_kernel(n, a_mem, b_mem, c_mem) result(kernel)
    integer(int64), intent(in) :: n
    type(memory_handle), intent(in) :: a_mem, b_mem, c_mem
    type(sparkle_kernel) :: kernel
    
    kernel%name = 'dot_product'
    kernel%kernel_type = KERNEL_REDUCTION
    kernel%work_items = n
    kernel%fortran_proc => dot_product_impl
    kernel%arithmetic_intensity = 2.0_real64  ! 2 FLOPs per 2 memory reads
    
    allocate(kernel%arguments(3))
    kernel%arguments(1)%name = 'a'
    kernel%arguments(1)%arg_type = TYPE_REAL32
    kernel%arguments(1)%intent = ARG_IN
    kernel%arguments(1)%data = a_mem
    allocate(kernel%arguments(1)%shape(1))
    kernel%arguments(1)%shape(1) = n
    
    kernel%arguments(2)%name = 'b'
    kernel%arguments(2)%arg_type = TYPE_REAL32
    kernel%arguments(2)%intent = ARG_IN
    kernel%arguments(2)%data = b_mem
    allocate(kernel%arguments(2)%shape(1))
    kernel%arguments(2)%shape(1) = n
    
    kernel%arguments(3)%name = 'result'
    kernel%arguments(3)%arg_type = TYPE_REAL32
    kernel%arguments(3)%intent = ARG_OUT
    kernel%arguments(3)%data = c_mem
    allocate(kernel%arguments(3)%shape(1))
    kernel%arguments(3)%shape(1) = 1
    
  end function create_dot_product_kernel
  
  ! Kernel implementations
  subroutine vector_add_impl(args)
    type(kernel_argument), intent(inout) :: args(:)
    real(real32), pointer :: a(:), b(:), c(:)
    
    call c_f_pointer(args(1)%data%ptr, a, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, b, args(2)%shape)
    call c_f_pointer(args(3)%data%ptr, c, args(3)%shape)
    
    c = a + b
  end subroutine vector_add_impl
  
  subroutine vector_scale_impl(args)
    type(kernel_argument), intent(inout) :: args(:)
    real(real32), pointer :: a(:), c(:)
    real(real32), parameter :: scale = 3.14159_real32
    
    call c_f_pointer(args(1)%data%ptr, a, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, c, args(2)%shape)
    
    c = a * scale
  end subroutine vector_scale_impl
  
  subroutine dot_product_impl(args)
    type(kernel_argument), intent(inout) :: args(:)
    real(real32), pointer :: a(:), b(:), result(:)
    
    call c_f_pointer(args(1)%data%ptr, a, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, b, args(2)%shape)
    call c_f_pointer(args(3)%data%ptr, result, [1_int64])
    
    result(1) = dot_product(a, b)
  end subroutine dot_product_impl

end program test_benchmarks