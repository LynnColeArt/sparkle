program test_persistent_kernels
  use kinds
  use gpu_opengl_cached
  use gpu_program_cache
  implicit none
  
  ! Test data
  real(sp), allocatable :: input(:), weights(:), output(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  integer :: i, run
  real(sp) :: time_ms, total_time, avg_time
  integer(i64) :: total_flops
  real(sp) :: gflops
  
  ! Set up test parameters
  N = 1
  C = 128
  H = 28
  W = 28
  K = 256
  kernel_size = 3
  stride = 1
  pad = 1
  H_out = (H + 2*pad - kernel_size)/stride + 1
  W_out = (W + 2*pad - kernel_size)/stride + 1
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(weights)
  
  print *, "🚀 Testing Persistent Kernel Framework"
  print *, "====================================="
  print *, ""
  print '(A,I0,A,I0,A,I0,A,I0)', "Problem size: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0)', "Output: ", K, " filters, ", kernel_size, "x", kernel_size, " kernel"
  print *, ""
  
  ! Initialize GPU with caching
  if (.not. gpu_init_cached()) then
    print *, "❌ Failed to initialize GPU with cache!"
    stop 1
  end if
  
  print *, "🔥 Running 10 iterations to test cache effectiveness..."
  print *, ""
  
  total_time = 0.0
  
  ! Run multiple times to demonstrate cache benefits
  do run = 1, 10
    ! Execute convolution
    time_ms = gpu_execute_conv2d_cached(input, weights, output, &
                                       N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    
    total_time = total_time + time_ms
    
    ! Calculate performance
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    gflops = real(total_flops, real32) / (time_ms * 1.0e6)
    
    print '(A,I2,A,F8.2,A,F8.1,A)', "Run ", run, ": ", time_ms, " ms, ", gflops, " GFLOPS"
  end do
  
  avg_time = total_time / 10.0
  gflops = real(total_flops, real32) / (avg_time * 1.0e6)
  
  print *, ""
  print '(A,F8.2,A)', "Average time: ", avg_time, " ms"
  print '(A,F8.1,A)', "Average performance: ", gflops, " GFLOPS"
  print *, ""
  
  ! Show cache statistics
  print *, "📊 Cache Statistics:"
  call gpu_print_cache_stats()
  
  ! Cleanup
  call gpu_cleanup_cached()
  
  deallocate(input, weights, output)
  
  print *, ""
  print *, "✅ Persistent kernel test complete!"
  
  ! Key insights
  print *, ""
  print *, "🔑 Key Benefits of Persistent Kernels:"
  print *, "   1. No shader recompilation between runs"
  print *, "   2. Faster startup time for applications"
  print *, "   3. Lower memory pressure from repeated compilations"
  print *, "   4. Better performance predictability"
  print *, ""
  print *, "📈 Next Steps:"
  print *, "   - Phase 2: Binary persistence to disk"
  print *, "   - Phase 3: Advanced lifecycle management"
  print *, "   - Phase 4: Full async integration"
  
end program test_persistent_kernels