module sparkle_parallel_kernels
  ! Parallel implementations of core kernels
  ! The Sparkle Way: Use all the cores, respect the system
  
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_f_pointer
  use sparkle_types
  use sparkle_kernels
  use sparkle_memory
  use sparkle_config
  implicit none
  private
  
  public :: parallel_vector_add_kernel, parallel_vector_scale_kernel
  public :: parallel_axpy_kernel, parallel_dot_product_kernel
  public :: parallel_reduction_kernel, parallel_complex_kernel
  
contains

  ! Helper to set safe thread count
  subroutine ensure_safe_threads()
    type(sparkle_config_type) :: config
    integer :: safe_threads, total_threads
    
    interface
      function omp_get_max_threads() bind(C, name="omp_get_max_threads")
        use iso_c_binding, only: c_int
        integer(c_int) :: omp_get_max_threads
      end function omp_get_max_threads
      
      subroutine omp_set_num_threads(num_threads) bind(C, name="omp_set_num_threads")
        use iso_c_binding, only: c_int
        integer(c_int), value :: num_threads
      end subroutine omp_set_num_threads
    end interface
    
    config = sparkle_get_config()
    total_threads = int(omp_get_max_threads())
    
    if (config%max_cpu_threads > 0) then
      safe_threads = min(config%max_cpu_threads, total_threads)
    else
      safe_threads = max(1, total_threads - config%thread_reserve)
    end if
    
    call omp_set_num_threads(safe_threads)
  end subroutine ensure_safe_threads

  ! Parallel vector addition: z = x + y
  subroutine parallel_vector_add_kernel(args)
    type(kernel_argument), intent(inout) :: args(:)
    
    real(real32), pointer :: x(:), y(:), z(:)
    integer(int64) :: i, n
    
    ! Set thread safety
    call ensure_safe_threads()
    
    ! Extract arguments
    call c_f_pointer(args(1)%data%ptr, x, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, y, args(2)%shape)
    call c_f_pointer(args(3)%data%ptr, z, args(3)%shape)
    n = args(1)%shape(1)
    
    ! Parallel execution with SIMD
    !$OMP PARALLEL DO SIMD
    do i = 1, n
      z(i) = x(i) + y(i)
    end do
    !$OMP END PARALLEL DO SIMD
    
  end subroutine parallel_vector_add_kernel
  
  ! Parallel vector scaling: y = alpha * x
  subroutine parallel_vector_scale_kernel(args)
    type(kernel_argument), intent(inout) :: args(:)
    
    real(real32), pointer :: x(:), y(:)
    real(real32) :: alpha
    integer(int64) :: i, n
    
    call ensure_safe_threads()
    
    call c_f_pointer(args(1)%data%ptr, x, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, y, args(2)%shape)
    n = args(1)%shape(1)
    alpha = 2.5_real32  ! Would come from args in real implementation
    
    !$OMP PARALLEL DO SIMD
    do i = 1, n
      y(i) = alpha * x(i)
    end do
    !$OMP END PARALLEL DO SIMD
    
  end subroutine parallel_vector_scale_kernel
  
  ! Parallel AXPY: y = alpha*x + y
  subroutine parallel_axpy_kernel(args)
    type(kernel_argument), intent(inout) :: args(:)
    
    real(real32), pointer :: x(:), y(:)
    real(real32) :: alpha
    integer(int64) :: i, n
    
    call ensure_safe_threads()
    
    call c_f_pointer(args(1)%data%ptr, x, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, y, args(2)%shape)
    n = args(1)%shape(1)
    alpha = 2.5_real32
    
    !$OMP PARALLEL DO SIMD
    do i = 1, n
      y(i) = alpha * x(i) + y(i)
    end do
    !$OMP END PARALLEL DO SIMD
    
  end subroutine parallel_axpy_kernel
  
  ! Parallel dot product with reduction
  subroutine parallel_dot_product_kernel(args)
    type(kernel_argument), intent(inout) :: args(:)
    
    real(real32), pointer :: x(:), y(:), result(:)
    real(real32) :: dot
    integer(int64) :: i, n
    
    call ensure_safe_threads()
    
    call c_f_pointer(args(1)%data%ptr, x, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, y, args(2)%shape)
    call c_f_pointer(args(3)%data%ptr, result, [1_int64])
    n = args(1)%shape(1)
    
    dot = 0.0_real32
    !$OMP PARALLEL DO REDUCTION(+:dot)
    do i = 1, n
      dot = dot + x(i) * y(i)
    end do
    !$OMP END PARALLEL DO
    
    result(1) = dot
    
  end subroutine parallel_dot_product_kernel
  
  ! Parallel reduction (sum, max, min)
  subroutine parallel_reduction_kernel(args)
    type(kernel_argument), intent(inout) :: args(:)
    
    real(real32), pointer :: x(:), result(:)
    real(real32) :: sum_val
    integer(int64) :: i, n
    
    call ensure_safe_threads()
    
    call c_f_pointer(args(1)%data%ptr, x, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, result, [1_int64])
    n = args(1)%shape(1)
    
    sum_val = 0.0_real32
    !$OMP PARALLEL DO REDUCTION(+:sum_val)
    do i = 1, n
      sum_val = sum_val + x(i)
    end do
    !$OMP END PARALLEL DO
    
    result(1) = sum_val
    
  end subroutine parallel_reduction_kernel
  
  ! Parallel complex computation: z = sqrt(x^2 + y^2)
  subroutine parallel_complex_kernel(args)
    type(kernel_argument), intent(inout) :: args(:)
    
    real(real32), pointer :: x(:), y(:), z(:)
    integer(int64) :: i, n
    
    call ensure_safe_threads()
    
    call c_f_pointer(args(1)%data%ptr, x, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, y, args(2)%shape)
    call c_f_pointer(args(3)%data%ptr, z, args(3)%shape)
    n = args(1)%shape(1)
    
    !$OMP PARALLEL DO SIMD
    do i = 1, n
      z(i) = sqrt(x(i)**2 + y(i)**2)
    end do
    !$OMP END PARALLEL DO SIMD
    
  end subroutine parallel_complex_kernel

end module sparkle_parallel_kernels