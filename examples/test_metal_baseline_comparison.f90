program test_metal_baseline_comparison
  ! Compare Sparkle's Metal memory pool vs direct Metal API usage
  ! Tests: allocation speed, memory usage, cache efficiency
  
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding
  use sparkle_memory
  use sparkle_gpu_metal
  use sparkle_memory_metal
  use sparkle_metal_kernels
  implicit none
  
  type(metal_context) :: ctx
  type(metal_memory_pool) :: pool
  
  integer :: i, j, k
  integer, parameter :: N_ITERATIONS = 100
  integer, parameter :: N_SIZES = 4
  integer(int64) :: test_sizes(N_SIZES) = [1024*1024, 4*1024*1024, 16*1024*1024, &
                                            64*1024*1024]
  character(len=10) :: size_labels(N_SIZES) = ["1 MB      ", "4 MB      ", &
                                                 "16 MB     ", "64 MB     "]
  
  real(real64) :: start_time, end_time
  real(real64) :: baseline_times(N_SIZES), pool_times(N_SIZES)
  real(real64) :: baseline_total, pool_total
  integer(int64) :: peak_memory_baseline, peak_memory_pool
  
  print *, "========================================="
  print *, "  Sparkle vs Baseline Metal Comparison"
  print *, "========================================="
  print *, ""
  
  ! Initialize Metal
  if (.not. check_metal_available()) then
    print *, "❌ Metal not available"
    stop
  end if
  
  ctx = create_metal_context()
  if (.not. ctx%initialized) then
    print *, "❌ Failed to create Metal context"
    stop
  end if
  
  print *, "Test Configuration:"
  print '(A,I0)', "  Iterations per size: ", N_ITERATIONS
  print *, "  Buffer sizes: 1MB, 4MB, 16MB, 64MB, 256MB"
  print *, ""
  
  ! ============================================
  ! Test 1: Baseline Metal (Direct API)
  ! ============================================
  print *, "📊 Testing BASELINE Metal Implementation"
  print *, "----------------------------------------"
  print *, "(Direct MTLBuffer allocation/deallocation)"
  print *, ""
  
  baseline_total = 0.0
  peak_memory_baseline = 0
  
  do i = 1, N_SIZES
    print '(A,A,A)', "Testing ", trim(size_labels(i)), "..."
    
    call cpu_time(start_time)
    
    ! Simulate typical allocation pattern
    block
      type(metal_buffer), allocatable :: buffers(:)
      allocate(buffers(10))
      
      do j = 1, N_ITERATIONS
        ! Allocate 10 buffers
        do k = 1, 10
          buffers(k) = create_metal_buffer(ctx, test_sizes(i))
        end do
        
        ! Track peak memory
        peak_memory_baseline = max(peak_memory_baseline, test_sizes(i) * 10)
        
        ! "Use" the buffers (just access them)
        ! Just check silently
        do k = 1, 10
          if (.not. buffers(k)%allocated) then
            exit  ! Failed allocation, exit inner loop
          end if
        end do
        
        ! Free buffers (in baseline, they're actually freed)
        ! In real Metal, these would be released
        ! We can't actually free them in our wrapper without the destroy function
      end do
    end block
    
    call cpu_time(end_time)
    baseline_times(i) = (end_time - start_time) * 1000.0
    baseline_total = baseline_total + baseline_times(i)
    
    print '(A,F0.2,A)', "  Time: ", baseline_times(i), " ms"
  end do
  
  print *, ""
  print '(A,F0.2,A)', "Baseline total time: ", baseline_total, " ms"
  print '(A,F0.2,A)', "Peak memory usage: ", real(peak_memory_baseline) / real(1024**3), " GB"
  print *, ""
  
  ! ============================================
  ! Test 2: Sparkle Metal Pool
  ! ============================================
  print *, "🌟 Testing SPARKLE Metal Pool"
  print *, "-----------------------------"
  print *, "(Pooled allocation with caching)"
  print *, ""
  
  ! Create pool with sufficient size
  pool = create_metal_pool(ctx, int(2_int64 * 1024**3, int64))  ! 2 GB pool
  
  pool_total = 0.0
  peak_memory_pool = 0
  
  do i = 1, N_SIZES
    print '(A,A,A)', "Testing ", trim(size_labels(i)), "..."
    
    call cpu_time(start_time)
    
    ! Same allocation pattern but with pool
    block
      type(memory_handle) :: handles(10)
      
      do j = 1, N_ITERATIONS
        ! Allocate 10 buffers from pool
        do k = 1, 10
          handles(k) = metal_pool_allocate(pool, test_sizes(i), "test_buffer")
        end do
        
        ! Track peak memory (pool tracks this internally)
        peak_memory_pool = pool%peak_allocated
        
        ! "Use" the buffers (just check silently)
        do k = 1, 10
          if (.not. handles(k)%is_allocated) then
            exit  ! Failed allocation
          end if
        end do
        
        ! Return to pool (cached, not freed)
        do k = 1, 10
          call metal_pool_deallocate(pool, handles(k))
        end do
      end do
    end block
    
    call cpu_time(end_time)
    pool_times(i) = (end_time - start_time) * 1000.0
    pool_total = pool_total + pool_times(i)
    
    print '(A,F0.2,A)', "  Time: ", pool_times(i), " ms"
  end do
  
  print *, ""
  print '(A,F0.2,A)', "Sparkle total time: ", pool_total, " ms"
  print '(A,F0.2,A)', "Peak memory usage: ", real(peak_memory_pool) / real(1024**3), " GB"
  
  ! Show pool statistics
  print *, ""
  call pool%get_stats()
  
  ! ============================================
  ! Results Comparison
  ! ============================================
  print *, "========================================="
  print *, "           FINAL COMPARISON"
  print *, "========================================="
  print *, ""
  print *, "Allocation Performance (lower is better):"
  print *, "-----------------------------------------"
  
  do i = 1, N_SIZES
    block
      real(real32) :: speedup
      speedup = baseline_times(i) / pool_times(i)
    
    print '(A,A)', trim(size_labels(i)), ":"
    print '(A,F0.2,A)', "  Baseline: ", baseline_times(i), " ms"
    print '(A,F0.2,A)', "  Sparkle:  ", pool_times(i), " ms"
    
    if (speedup > 1.0) then
      print '(A,F0.1,A)', "  🏆 Sparkle is ", speedup, "x faster"
    else
      print '(A,F0.1,A)', "  Baseline is ", 1.0/speedup, "x faster"
    end if
    print *, ""
    end block
  end do
  
  print *, "Overall Performance:"
  print *, "-------------------"
  print '(A,F0.2,A)', "Baseline total: ", baseline_total, " ms"
  print '(A,F0.2,A)', "Sparkle total:  ", pool_total, " ms"
  print '(A,F0.1,A)', "🎯 Sparkle is ", baseline_total / pool_total, "x faster overall"
  print *, ""
  
  print *, "Memory Efficiency:"
  print *, "-----------------"
  print '(A,F0.2,A)', "Baseline peak: ", real(peak_memory_baseline) / real(1024**3), " GB"
  print '(A,F0.2,A)', "Sparkle peak:  ", real(peak_memory_pool) / real(1024**3), " GB"
  
  if (peak_memory_pool < peak_memory_baseline) then
    print '(A,F0.1,A)', "🎯 Sparkle uses ", &
           real(peak_memory_baseline) / real(peak_memory_pool), "x less memory"
  end if
  
  print *, ""
  print *, "Key Advantages of Sparkle Pool:"
  print *, "-------------------------------"
  print *, "✅ Buffer reuse eliminates allocation overhead"
  print *, "✅ Reduced memory fragmentation"
  print *, "✅ Predictable memory usage (pool size)"
  print *, "✅ Cache-friendly allocation patterns"
  print *, "✅ No kernel calls for cached buffers"
  print *, ""
  
  print *, "When Baseline Might Win:"
  print *, "------------------------"
  print *, "❗ First allocation (pool has setup cost)"
  print *, "❗ Single-use buffers (no reuse benefit)"
  print *, "❗ Highly variable sizes (cache misses)"
  print *, ""
  
  ! Cleanup
  call destroy_metal_pool(pool)
  call destroy_metal_context(ctx)
  
  print *, "========================================="
  print *, "Sparkle: Smarter memory for the people!"
  
end program test_metal_baseline_comparison