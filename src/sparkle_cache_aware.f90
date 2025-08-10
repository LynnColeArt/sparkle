module sparkle_cache_aware
  ! Cache-aware algorithms for breaking the memory wall
  ! The Sparkle Way: Work with the hardware, not against it
  
  use iso_fortran_env, only: int32, int64, real32, real64
  use sparkle_config, only: sparkle_get_config, sparkle_config_type
  implicit none
  private
  
  public :: cache_aware_gemm, cache_aware_reduction
  public :: get_optimal_tile_size
  public :: L1_SIZE, L2_SIZE, L3_SIZE
  
  ! Cache sizes (typical x86-64, will make configurable later)
  integer(int64), parameter :: L1_SIZE = 32_int64 * 1024_int64         ! 32 KB
  integer(int64), parameter :: L2_SIZE = 256_int64 * 1024_int64        ! 256 KB  
  integer(int64), parameter :: L3_SIZE = 8_int64 * 1024_int64 * 1024_int64  ! 8 MB
  integer(int64), parameter :: CACHE_LINE = 64_int64                   ! 64 bytes
  
contains

  ! Set OpenMP threads to safe count
  subroutine set_safe_num_threads(config)
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
    
  end subroutine set_safe_num_threads

  ! Determine optimal tile size for given cache level
  function get_optimal_tile_size(cache_size, element_size, dimensions) result(tile_size)
    integer(int64), intent(in) :: cache_size
    integer, intent(in) :: element_size  ! bytes per element
    integer, intent(in) :: dimensions    ! 1D, 2D, 3D
    integer :: tile_size
    
    integer(int64) :: elements_in_cache
    
    ! Leave 20% headroom for other data
    elements_in_cache = (cache_size * 8_int64) / (10_int64 * element_size)
    
    select case(dimensions)
    case(1)
      tile_size = int(elements_in_cache)
    case(2)
      tile_size = int(sqrt(real(elements_in_cache)))
    case(3)
      tile_size = int(real(elements_in_cache)**(1.0/3.0))
    case default
      tile_size = 64  ! fallback
    end select
    
    ! Align to cache line
    tile_size = (tile_size / 16) * 16  ! 16 floats per cache line
    
  end function get_optimal_tile_size
  
  ! Cache-aware matrix multiplication
  ! Optimized tiling for cache hierarchy
  subroutine cache_aware_gemm(a, b, c, m, n, k)
    real(real32), intent(in) :: a(m,k), b(k,n)
    real(real32), intent(inout) :: c(m,n)
    integer, intent(in) :: m, n, k
    
    integer :: tile_size
    integer :: i, j, kk, ii, jj, kk_tile
    real(real32) :: sum
    type(sparkle_config_type) :: config
    
    ! Get safe thread configuration
    config = sparkle_get_config()
    call set_safe_num_threads(config)
    
    ! Use a single tile size optimized for L2 cache
    tile_size = 64  ! 64x64 floats = 16KB, fits nicely in L2
    
    ! Zero output matrix
    c = 0.0_real32
    
    ! Tiled matrix multiplication with better loop order
    !$OMP PARALLEL DO PRIVATE(ii,jj,kk_tile,i,j,kk,sum) SCHEDULE(DYNAMIC,1)
    do jj = 1, n, tile_size
      do kk_tile = 1, k, tile_size
        do ii = 1, m, tile_size
          
          ! Process tile
          do j = jj, min(jj + tile_size - 1, n)
            do kk = kk_tile, min(kk_tile + tile_size - 1, k)
              !$OMP SIMD
              do i = ii, min(ii + tile_size - 1, m)
                c(i,j) = c(i,j) + a(i,kk) * b(kk,j)
              end do
              !$OMP END SIMD
            end do
          end do
          
        end do
      end do
    end do
    !$OMP END PARALLEL DO
    
  end subroutine cache_aware_gemm
  
  ! Micro-kernel that fits in L1 cache
  subroutine gemm_microkernel(a, b, c, lda, ldb, ldc, &
                              row_start, col_start, k_start, &
                              rows, cols, k_size)
    real(real32), intent(in) :: a(lda,*), b(ldb,*)
    real(real32), intent(inout) :: c(ldc,*)
    integer, intent(in) :: lda, ldb, ldc
    integer, intent(in) :: row_start, col_start, k_start
    integer, intent(in) :: rows, cols, k_size
    
    integer :: i, j, kk
    real(real32) :: sum
    
    ! Process the tile with correct bounds
    do j = col_start, min(col_start + cols - 1, ldb)
      do i = row_start, min(row_start + rows - 1, lda)
        sum = c(i,j)
        
        ! Inner loop - will be vectorized
        !$OMP SIMD REDUCTION(+:sum)
        do kk = k_start, min(k_start + k_size - 1, ldc)
          sum = sum + a(i,kk) * b(kk,j)
        end do
        !$OMP END SIMD
        
        c(i,j) = sum
      end do
    end do
    
  end subroutine gemm_microkernel
  
  ! Cache-aware reduction (sum, max, etc)
  ! Uses tree reduction to maximize cache reuse
  subroutine cache_aware_reduction(x, n, result, op)
    real(real32), intent(in) :: x(n)
    integer(int64), intent(in) :: n
    real(real32), intent(out) :: result
    character(len=*), intent(in) :: op  ! 'sum', 'max', 'min'
    
    integer :: block_size, num_blocks
    real(real32), allocatable :: partial(:)
    integer :: i, b, start_idx, end_idx
    real(real32) :: block_result
    
    ! Choose block size that fits in L1 cache
    block_size = int(L1_SIZE / 4_int64)  ! 4 bytes per float
    num_blocks = int((n + block_size - 1) / block_size)
    
    allocate(partial(num_blocks))
    
    ! First level: reduce each L1-sized block
    !$OMP PARALLEL DO PRIVATE(b,start_idx,end_idx,block_result)
    do b = 1, num_blocks
      start_idx = (b-1) * block_size + 1
      end_idx = min(b * block_size, int(n))
      
      select case(op)
      case('sum')
        block_result = 0.0_real32
        do i = start_idx, end_idx
          block_result = block_result + x(i)
        end do
      case('max') 
        block_result = x(start_idx)
        do i = start_idx + 1, end_idx
          if (x(i) > block_result) block_result = x(i)
        end do
      case('min')
        block_result = x(start_idx)
        do i = start_idx + 1, end_idx
          if (x(i) < block_result) block_result = x(i)
        end do
      end select
      
      partial(b) = block_result
    end do
    !$OMP END PARALLEL DO
    
    ! Second level: reduce the partial results
    select case(op)
    case('sum')
      result = 0.0_real32
      do b = 1, num_blocks
        result = result + partial(b)
      end do
    case('max')
      result = partial(1)
      do b = 2, num_blocks
        if (partial(b) > result) result = partial(b)
      end do
    case('min')
      result = partial(1)
      do b = 2, num_blocks
        if (partial(b) < result) result = partial(b)
      end do
    end select
    
    deallocate(partial)
    
  end subroutine cache_aware_reduction

end module sparkle_cache_aware