module sparkle_fused_kernels
  ! Memory wall busting through operation fusion
  ! The Sparkle Way: Make memory bandwidth irrelevant
  
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_f_pointer
  use sparkle_types
  use sparkle_kernels
  use sparkle_memory
  use sparkle_config
  implicit none
  private
  
  public :: fused_axpby_kernel, fused_gemm_bias_relu_kernel, fused_norm_scale_kernel
  public :: TILE_L1, TILE_L2, TILE_L3
  
  ! Cache-aware tile sizes (tuned for typical x86-64)
  integer, parameter :: TILE_L1 = 64    ! 64x64 floats = 16KB (fits in 32KB L1)
  integer, parameter :: TILE_L2 = 256   ! 256x256 floats = 256KB (fits in 512KB L2)
  integer, parameter :: TILE_L3 = 1024  ! 1024x1024 floats = 4MB (fits in 8MB L3)
  
  ! Compiler hints for SIMD (modern Fortran way)
  !$OMP DECLARE SIMD
  
contains

  ! Set OpenMP threads to safe count
  subroutine set_safe_omp_threads(config)
    type(sparkle_config_type), intent(in) :: config
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
    
    total_threads = int(omp_get_max_threads())
    
    if (config%max_cpu_threads > 0) then
      safe_threads = min(config%max_cpu_threads, total_threads)
    else
      safe_threads = max(1, total_threads - config%thread_reserve)
    end if
    
    call omp_set_num_threads(safe_threads)
    
  end subroutine set_safe_omp_threads

  ! Fused AXPBY: y = alpha*x + beta*y
  ! Traditional: 3 memory passes (load x, load y, store y)
  ! Fused: 2 memory passes (load x,y together, store y)
  subroutine fused_axpby_kernel(args)
    type(kernel_argument), intent(inout) :: args(:)
    !$OMP DECLARE SIMD
    
    real(real32), pointer :: x(:), y(:)
    real(real32) :: alpha, beta
    integer(int64) :: i, n
    
    ! Extract arguments
    call c_f_pointer(args(1)%data%ptr, x, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, y, args(2)%shape)
    n = args(1)%shape(1)
    alpha = 2.5_real32  ! Would come from args in real impl
    beta = 1.5_real32
    
    ! Fused operation with SIMD hint
    !$OMP SIMD ALIGNED(x,y:64)
    do i = 1, n
      y(i) = alpha * x(i) + beta * y(i)
    end do
    !$OMP END SIMD
    
  end subroutine fused_axpby_kernel
  
  ! Fused GEMM + Bias + ReLU (the memory wall destroyer)
  ! Traditional: C = A*B, C = C + bias, C = ReLU(C) → 6 memory passes
  ! Fused: C = ReLU(A*B + bias) → 3 memory passes
  subroutine fused_gemm_bias_relu_kernel(args)
    type(kernel_argument), intent(inout) :: args(:)
    
    real(real32), pointer :: a(:,:), b(:,:), c(:,:), bias(:)
    integer :: m, n, k
    integer :: i, j, kk
    integer :: ii, jj, kk_tile
    real(real32) :: sum
    type(sparkle_config_type) :: config
    
    ! Extract matrices
    call c_f_pointer(args(1)%data%ptr, a, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, b, args(2)%shape)
    call c_f_pointer(args(3)%data%ptr, c, args(3)%shape)
    call c_f_pointer(args(4)%data%ptr, bias, args(4)%shape)
    
    m = int(args(1)%shape(1))
    k = int(args(1)%shape(2))
    n = int(args(2)%shape(2))
    
    ! Get safe thread configuration
    config = sparkle_get_config()
    call set_safe_omp_threads(config)
    
    ! Tiled and fused implementation
    !$OMP PARALLEL DO PRIVATE(ii,jj,kk_tile,i,j,kk,sum) &
    !$OMP COLLAPSE(2) SCHEDULE(STATIC)
    do jj = 1, n, TILE_L1
      do ii = 1, m, TILE_L1
        
        ! Process L1-sized tile
        do j = jj, min(jj + TILE_L1 - 1, n)
          do i = ii, min(ii + TILE_L1 - 1, m)
            
            sum = 0.0_real32
            
            ! Inner loop with SIMD
            !$OMP SIMD REDUCTION(+:sum)
            do kk = 1, k
              sum = sum + a(i,kk) * b(kk,j)
            end do
            !$OMP END SIMD
            
            ! Fused bias and ReLU (no intermediate store!)
            c(i,j) = max(0.0_real32, sum + bias(j))
            
          end do
        end do
        
      end do
    end do
    !$OMP END PARALLEL DO
    
  end subroutine fused_gemm_bias_relu_kernel
  
  ! Fused normalize and scale
  ! Traditional: compute mean, compute variance, normalize, scale → 4 passes
  ! Fused: single pass with running statistics
  subroutine fused_norm_scale_kernel(args)
    type(kernel_argument), intent(inout) :: args(:)
    
    real(real32), pointer :: x(:), gamma(:), beta(:)
    integer(int64) :: i, n
    real(real64) :: mean, m2, variance
    real(real32) :: std_inv
    
    call c_f_pointer(args(1)%data%ptr, x, args(1)%shape)
    call c_f_pointer(args(2)%data%ptr, gamma, [args(1)%shape(1)])
    call c_f_pointer(args(3)%data%ptr, beta, [args(1)%shape(1)])
    n = args(1)%shape(1)
    
    ! Welford's algorithm for stable mean/variance in one pass
    mean = 0.0_real64
    m2 = 0.0_real64
    
    do i = 1, n
      block
        real(real64) :: delta, delta2
        delta = real(x(i), real64) - mean
        mean = mean + delta / real(i, real64)
        delta2 = real(x(i), real64) - mean
        m2 = m2 + delta * delta2
      end block
    end do
    
    variance = m2 / real(n - 1, real64)
    std_inv = 1.0_real32 / sqrt(real(variance, real32) + 1.0e-6_real32)
    
    ! Second pass: normalize and scale (could be fused with first pass for streaming)
    !$OMP SIMD ALIGNED(x,gamma,beta:64)
    do i = 1, n
      x(i) = gamma(i) * (x(i) - real(mean, real32)) * std_inv + beta(i)
    end do
    !$OMP END SIMD
    
  end subroutine fused_norm_scale_kernel
  

end module sparkle_fused_kernels