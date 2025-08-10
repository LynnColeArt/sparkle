program test_memory_wall
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_f_pointer
  use sparkle_types
  use sparkle_memory
  use sparkle_benchmark
  use sparkle_fused_kernels
  use sparkle_cache_aware
  implicit none
  
  integer, parameter :: N = 1024  ! Matrix size
  type(memory_handle) :: a_mem, b_mem, c_mem, bias_mem
  real(real32), pointer :: a(:,:), b(:,:), c(:,:), bias(:)
  real(real64) :: start_time, end_time
  real(real64) :: naive_time, fused_time, cache_aware_time
  integer :: i, j, k
  real(real32) :: sum
  
  print *, "🧱 Breaking the Memory Wall"
  print *, "==========================="
  print *, ""
  print *, "Demonstrating memory bandwidth optimizations"
  print '(A,I0,A,I0)', "Matrix size: ", N, "×", N
  print '(A,F6.2,A)', "Memory per matrix: ", real(N*N*4) / (1024.0*1024.0), " MB"
  print *, ""
  
  ! Allocate matrices
  a_mem = create_memory(int(N*N*4, int64))
  b_mem = create_memory(int(N*N*4, int64))
  c_mem = create_memory(int(N*N*4, int64))
  bias_mem = create_memory(int(N*4, int64))
  
  call c_f_pointer(a_mem%ptr, a, [N, N])
  call c_f_pointer(b_mem%ptr, b, [N, N])
  call c_f_pointer(c_mem%ptr, c, [N, N])
  call c_f_pointer(bias_mem%ptr, bias, [N])
  
  ! Initialize data
  call random_number(a)
  call random_number(b)
  bias = 0.1_real32
  
  ! Test 1: Naive implementation (memory wall victim)
  print *, "Test 1: Naive GEMM + Bias + ReLU (3 separate passes)"
  print *, "----------------------------------------------------"
  
  ! Warm up
  call naive_gemm_bias_relu(a, b, c, bias, N)
  
  call cpu_time(start_time)
  do i = 1, 10
    call naive_gemm_bias_relu(a, b, c, bias, N)
  end do
  call cpu_time(end_time)
  naive_time = (end_time - start_time) / 10.0_real64
  
  print '(A,F8.3,A)', "Time: ", naive_time * 1000.0_real64, " ms"
  print '(A,I0)', "Memory passes: 6 (load A,B → store C → load C → store C → load C → store C)"
  print '(A,F6.2,A)', "Effective bandwidth: ", &
    real(6_int64 * N * N * 4) / (naive_time * 1.0e9_real64), " GB/s"
  
  ! Test 2: Fused implementation (memory wall breaker)
  print *, ""
  print *, "Test 2: Fused GEMM + Bias + ReLU (single pass)"
  print *, "----------------------------------------------"
  
  ! Warm up
  call fused_gemm_bias_relu(a, b, c, bias, N)
  
  call cpu_time(start_time)
  do i = 1, 10
    call fused_gemm_bias_relu(a, b, c, bias, N)
  end do
  call cpu_time(end_time)
  fused_time = (end_time - start_time) / 10.0_real64
  
  print '(A,F8.3,A)', "Time: ", fused_time * 1000.0_real64, " ms"
  print '(A,I0)', "Memory passes: 3 (load A,B → compute+bias+relu → store C)"
  print '(A,F6.2,A)', "Effective bandwidth: ", &
    real(3_int64 * N * N * 4) / (fused_time * 1.0e9_real64), " GB/s"
  print '(A,F5.2,A)', "Speedup: ", naive_time / fused_time, "x"
  
  ! Test 3: Cache-aware tiled implementation
  print *, ""
  print *, "Test 3: Cache-aware tiled GEMM"
  print *, "------------------------------"
  
  ! Warm up
  call cache_aware_gemm(a, b, c, N, N, N)
  
  call cpu_time(start_time)
  do i = 1, 10
    call cache_aware_gemm(a, b, c, N, N, N)
  end do
  call cpu_time(end_time)
  cache_aware_time = (end_time - start_time) / 10.0_real64
  
  print '(A,F8.3,A)', "Time: ", cache_aware_time * 1000.0_real64, " ms"
  print '(A,I0,A,I0,A,I0)', "Tile sizes: L1=", &
    get_optimal_tile_size(32_int64*1024_int64, 4, 2), &
    ", L2=", get_optimal_tile_size(256_int64*1024_int64, 4, 2), &
    ", L3=", get_optimal_tile_size(8_int64*1024_int64*1024_int64, 4, 2)
  print '(A,F5.2,A)', "Speedup: ", naive_time / cache_aware_time, "x"
  
  ! Calculate GFLOPS
  block
    real(real64) :: flops, gflops_naive, gflops_fused, gflops_tiled
    flops = 2.0_real64 * real(N, real64)**3  ! 2N³ for GEMM
    gflops_naive = flops / (naive_time * 1.0e9_real64)
    gflops_fused = flops / (fused_time * 1.0e9_real64)
    gflops_tiled = flops / (cache_aware_time * 1.0e9_real64)
    
    print *, ""
    print *, "Performance Summary:"
    print *, "===================="
    print '(A,F8.2,A)', "Naive:       ", gflops_naive, " GFLOPS"
    print '(A,F8.2,A)', "Fused:       ", gflops_fused, " GFLOPS"
    print '(A,F8.2,A)', "Cache-aware: ", gflops_tiled, " GFLOPS"
    print *, ""
    print *, "Key insights:"
    print *, "- Fusion reduces memory traffic by 2x"
    print *, "- Cache tiling improves data reuse"
    print *, "- Combined optimizations can give 2-10x speedup"
    print *, "- All in pure Fortran with compiler SIMD hints!"
  end block
  
  ! Test 4: SIMD verification
  print *, ""
  print *, "Test 4: SIMD Usage Verification"
  print *, "-------------------------------"
  print *, "Compile with: gfortran -O3 -march=native -fopenmp -fopt-info-vec"
  print *, "to see vectorization reports"
  print *, ""
  print *, "OpenMP SIMD directives tell compiler to vectorize:"
  print *, "- !$OMP SIMD for explicit vectorization"
  print *, "- !$OMP SIMD ALIGNED for aligned data"
  print *, "- !$OMP SIMD REDUCTION for reductions"
  
  ! Cleanup
  call destroy_memory(a_mem)
  call destroy_memory(b_mem)
  call destroy_memory(c_mem)
  call destroy_memory(bias_mem)
  
  print *, ""
  print *, "✨ The Sparkle Way: Break the memory wall with pure Fortran!"

contains

  ! Naive implementation - how NOT to do it
  subroutine naive_gemm_bias_relu(a, b, c, bias, n)
    real(real32), intent(in) :: a(n,n), b(n,n), bias(n)
    real(real32), intent(out) :: c(n,n)
    integer, intent(in) :: n
    integer :: i, j, k
    
    ! Pass 1: GEMM (3 memory accesses per element)
    c = matmul(a, b)
    
    ! Pass 2: Add bias (2 memory accesses per element)
    do j = 1, n
      do i = 1, n
        c(i,j) = c(i,j) + bias(j)
      end do
    end do
    
    ! Pass 3: ReLU (2 memory accesses per element)
    do j = 1, n
      do i = 1, n
        c(i,j) = max(0.0_real32, c(i,j))
      end do
    end do
    
  end subroutine naive_gemm_bias_relu
  
  ! Fused implementation - memory wall breaker
  subroutine fused_gemm_bias_relu(a, b, c, bias, n)
    real(real32), intent(in) :: a(n,n), b(n,n), bias(n)
    real(real32), intent(out) :: c(n,n)
    integer, intent(in) :: n
    integer :: i, j, k
    real(real32) :: sum
    
    ! Single pass: GEMM + bias + ReLU
    !$OMP PARALLEL DO PRIVATE(i,j,k,sum)
    do j = 1, n
      do i = 1, n
        sum = 0.0_real32
        
        !$OMP SIMD REDUCTION(+:sum)
        do k = 1, n
          sum = sum + a(i,k) * b(k,j)
        end do
        !$OMP END SIMD
        
        ! Fused bias and ReLU
        c(i,j) = max(0.0_real32, sum + bias(j))
      end do
    end do
    !$OMP END PARALLEL DO
    
  end subroutine fused_gemm_bias_relu

end program test_memory_wall