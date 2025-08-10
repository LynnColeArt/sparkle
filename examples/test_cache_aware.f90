program test_cache_aware
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_f_pointer
  use sparkle_types
  use sparkle_memory
  use sparkle_cache_aware
  implicit none
  
  integer, parameter :: N = 1024  ! Matrix size
  type(memory_handle) :: a_mem, b_mem, c_mem
  real(real32), pointer :: a(:,:), b(:,:), c(:,:)
  real(real64) :: start_time, end_time
  real(real64) :: naive_time, cache_aware_time
  integer :: i, j, k
  real(real32) :: sum
  
  print *, "🧱 Breaking the Memory Wall with Cache-Aware Algorithms"
  print *, "======================================================"
  print *, ""
  print '(A,I0,A,I0)', "Matrix size: ", N, "×", N
  print '(A,F6.2,A)', "Memory per matrix: ", real(N*N*4) / (1024.0*1024.0), " MB"
  print '(A,F6.2,A)', "Total memory: ", real(3*N*N*4) / (1024.0*1024.0), " MB"
  print *, ""
  
  ! Show cache sizes
  print *, "Cache hierarchy:"
  print '(A,I0,A)', "  L1: ", int(L1_SIZE/1024), " KB"
  print '(A,I0,A)', "  L2: ", int(L2_SIZE/1024), " KB" 
  print '(A,I0,A)', "  L3: ", int(L3_SIZE/(1024*1024)), " MB"
  print *, ""
  
  ! Allocate matrices
  a_mem = create_memory(int(N*N*4, int64))
  b_mem = create_memory(int(N*N*4, int64))
  c_mem = create_memory(int(N*N*4, int64))
  
  call c_f_pointer(a_mem%ptr, a, [N, N])
  call c_f_pointer(b_mem%ptr, b, [N, N])
  call c_f_pointer(c_mem%ptr, c, [N, N])
  
  ! Initialize with random data
  call random_number(a)
  call random_number(b)
  
  ! Test 1: Naive implementation
  print *, "Test 1: Naive Matrix Multiplication"
  print *, "-----------------------------------"
  
  ! Warm up caches
  c = matmul(a, b)
  
  call cpu_time(start_time)
  do i = 1, 5
    c = matmul(a, b)
  end do
  call cpu_time(end_time)
  naive_time = (end_time - start_time) / 5.0_real64
  
  print '(A,F8.3,A)', "Time: ", naive_time * 1000.0_real64, " ms"
  print '(A,F8.2,A)', "GFLOPS: ", (2.0_real64 * real(N)**3) / (naive_time * 1.0e9_real64), " GFLOPS"
  print *, "Memory access pattern: Streaming (poor cache reuse)"
  
  ! Test 2: Cache-aware tiled implementation
  print *, ""
  print *, "Test 2: Cache-Aware Tiled Matrix Multiplication"
  print *, "-----------------------------------------------"
  
  ! Show optimal tile sizes
  block
    integer :: tile_l1, tile_l2, tile_l3
    tile_l1 = get_optimal_tile_size(L1_SIZE, 4, 2)
    tile_l2 = get_optimal_tile_size(L2_SIZE, 4, 2)
    tile_l3 = get_optimal_tile_size(L3_SIZE, 4, 2)
    
    print '(A,I0,A,I0,A)', "Optimal tile sizes: L1=", tile_l1, "×", tile_l1, " elements"
    print '(A,I0,A,I0,A)', "                    L2=", tile_l2, "×", tile_l2, " elements"
    print '(A,I0,A,I0,A)', "                    L3=", tile_l3, "×", tile_l3, " elements"
  end block
  
  ! Warm up
  call cache_aware_gemm(a, b, c, N, N, N)
  
  call cpu_time(start_time)
  do i = 1, 5
    call cache_aware_gemm(a, b, c, N, N, N)
  end do
  call cpu_time(end_time)
  cache_aware_time = (end_time - start_time) / 5.0_real64
  
  print '(A,F8.3,A)', "Time: ", cache_aware_time * 1000.0_real64, " ms"
  print '(A,F8.2,A)', "GFLOPS: ", (2.0_real64 * real(N)**3) / (cache_aware_time * 1.0e9_real64), " GFLOPS"
  print '(A,F5.2,A)', "Speedup: ", naive_time / cache_aware_time, "x"
  print *, "Memory access pattern: Tiled (maximizes cache reuse)"
  
  ! Test 3: Different matrix sizes
  print *, ""
  print *, "Test 3: Performance vs Matrix Size"
  print *, "----------------------------------"
  
  block
    integer, parameter :: sizes(5) = [64, 128, 256, 512, 1024]
    real(real64) :: naive_gflops, tiled_gflops
    integer :: n_test, s
    
    print *, "Size    Naive(GFLOPS)  Tiled(GFLOPS)  Speedup"
    print *, "----    -------------  -------------  -------"
    
    do s = 1, 5
      n_test = sizes(s)
      
      ! Skip if too large
      if (n_test > N) cycle
      
      ! Naive
      call cpu_time(start_time)
      c(1:n_test,1:n_test) = matmul(a(1:n_test,1:n_test), b(1:n_test,1:n_test))
      call cpu_time(end_time)
      naive_gflops = (2.0_real64 * real(n_test)**3) / ((end_time - start_time) * 1.0e9_real64)
      
      ! Tiled
      call cpu_time(start_time)
      call cache_aware_gemm(a, b, c, n_test, n_test, n_test)
      call cpu_time(end_time)
      tiled_gflops = (2.0_real64 * real(n_test)**3) / ((end_time - start_time) * 1.0e9_real64)
      
      print '(I4,4X,F13.2,2X,F13.2,2X,F7.2,A)', &
        n_test, naive_gflops, tiled_gflops, tiled_gflops/naive_gflops, "x"
    end do
  end block
  
  ! Test 4: Cache-aware reduction
  print *, ""
  print *, "Test 4: Cache-Aware Reduction"
  print *, "-----------------------------"
  
  block
    real(real32) :: result_naive, result_cache
    real(real64) :: reduce_time
    integer(int64) :: n_elements
    real(real32), allocatable :: flat_a(:)
    
    n_elements = int(N, int64) * int(N, int64)
    allocate(flat_a(n_elements))
    flat_a = reshape(a, [n_elements])
    
    ! Naive reduction
    call cpu_time(start_time)
    result_naive = 0.0_real32
    do i = 1, int(n_elements)
      result_naive = result_naive + flat_a(i)
    end do
    call cpu_time(end_time)
    reduce_time = end_time - start_time
    print '(A,F8.3,A)', "Naive sum time: ", reduce_time * 1000.0_real64, " ms"
    
    ! Cache-aware reduction
    call cpu_time(start_time)
    call cache_aware_reduction(flat_a, n_elements, result_cache, 'sum')
    call cpu_time(end_time)
    reduce_time = end_time - start_time
    print '(A,F8.3,A)', "Cache-aware sum time: ", reduce_time * 1000.0_real64, " ms"
    
    print '(A,ES12.5)', "Sum value: ", result_cache
    print '(A,ES12.5)', "Difference: ", abs(result_naive - result_cache)
    
    deallocate(flat_a)
  end block
  
  ! Cleanup
  call destroy_memory(a_mem)
  call destroy_memory(b_mem)
  call destroy_memory(c_mem)
  
  print *, ""
  print *, "Key Insights:"
  print *, "============="
  print *, "✓ Cache-aware tiling can give 2-5x speedup"
  print *, "✓ Optimal tile size depends on cache hierarchy"
  print *, "✓ Smaller matrices fit in cache → bigger speedup"
  print *, "✓ Pure Fortran + OpenMP gets great performance!"
  print *, ""
  print *, "Compile with: -O3 -march=native -fopenmp"
  print *, "for best vectorization and parallelization"
  print *, ""
  print *, "✨ The Sparkle Way: Beat the memory wall with smart algorithms!"

end program test_cache_aware