program test_metal_vs_mock
  ! Compare Metal GPU performance vs mock implementation
  ! The moment of truth: Can real GPU beat a print statement?
  
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding
  use sparkle_gpu_dispatch  ! Mock implementation
  use sparkle_gpu_metal     ! Real Metal implementation
  use sparkle_metal_kernels
  implicit none
  
  ! Test parameters
  integer, parameter :: N = 10000000  ! 10 million elements
  real(real32), allocatable, target :: x(:), y(:), z_mock(:), z_metal(:)
  
  ! Timing variables
  real(real64) :: start_time, end_time
  real(real64) :: mock_time, metal_time
  
  ! Metal objects
  type(metal_context) :: metal_ctx
  type(metal_buffer) :: x_buffer, y_buffer, z_buffer
  type(metal_kernel) :: add_kernel
  
  ! Mock objects
  type(gpu_device) :: mock_device
  type(gpu_memory) :: mock_x, mock_y, mock_z
  type(gpu_kernel) :: mock_kernel
  
  integer :: i
  logical :: metal_available
  
  print *, "========================================="
  print *, "   SPARKLE: Metal vs Mock Showdown!"
  print *, "========================================="
  print *, ""
  print '(A,I0,A,F0.1,A)', "Testing with ", N, " elements (", &
         real(N * 4 * 3) / real(1024**2), " MB total)"
  print *, ""
  
  ! Allocate and initialize test data
  allocate(x(N), y(N), z_mock(N), z_metal(N))
  
  do i = 1, N
    x(i) = real(i)
    y(i) = real(i * 2)
  end do
  z_mock = 0.0
  z_metal = 0.0
  
  ! ============================================
  ! Test 1: Mock Implementation
  ! ============================================
  print *, "📋 Testing MOCK Implementation..."
  print *, "--------------------------------"
  
  ! Initialize mock GPU
  mock_device = init_gpu_device()
  
  ! Create mock kernel
  mock_kernel = create_gpu_kernel("vector_add", "vector_add")
  
  ! Allocate mock GPU memory
  mock_x = gpu_malloc(int(N * 4, int64))
  mock_y = gpu_malloc(int(N * 4, int64))
  mock_z = gpu_malloc(int(N * 4, int64))
  
  ! Time mock execution (including all the print statements)
  call cpu_time(start_time)
  
  ! Copy to "GPU"
  call gpu_memcpy(mock_x, c_loc(x), int(N * 4, int64), HOST_TO_GPU)
  call gpu_memcpy(mock_y, c_loc(y), int(N * 4, int64), HOST_TO_GPU)
  
  ! Launch mock kernel
  call launch_gpu_kernel(mock_kernel, [mock_x, mock_y, mock_z], [N], [256])
  call gpu_synchronize()
  
  ! Copy back
  call gpu_memcpy(mock_z, c_loc(z_mock), int(N * 4, int64), GPU_TO_HOST)
  
  call cpu_time(end_time)
  mock_time = (end_time - start_time) * 1000.0  ! Convert to ms
  
  ! Cleanup mock
  call gpu_free(mock_x)
  call gpu_free(mock_y)
  call gpu_free(mock_z)
  
  print *, ""
  print '(A,F0.2,A)', "Mock time: ", mock_time, " ms"
  print *, ""
  
  ! ============================================
  ! Test 2: Metal Implementation (if available)
  ! ============================================
  metal_available = check_metal_available()
  
  if (metal_available) then
    print *, "🎮 Testing METAL Implementation..."
    print *, "--------------------------------"
    
    ! Create Metal context
    metal_ctx = create_metal_context()
    
    if (metal_ctx%initialized) then
      ! Create Metal buffers
      x_buffer = create_metal_buffer(metal_ctx, int(N * 4, int64))
      y_buffer = create_metal_buffer(metal_ctx, int(N * 4, int64))
      z_buffer = create_metal_buffer(metal_ctx, int(N * 4, int64))
      
      ! Compile Metal kernel
      add_kernel = compile_metal_kernel(metal_ctx, &
                                        get_metal_vector_add(), &
                                        "vector_add")
      
      if (add_kernel%compiled) then
        ! Time Metal execution
        call cpu_time(start_time)
        
        ! Copy to GPU
        call metal_memcpy(x_buffer, c_loc(x), int(N * 4, int64), .true.)
        call metal_memcpy(y_buffer, c_loc(y), int(N * 4, int64), .true.)
        
        ! Launch Metal kernel
        call dispatch_metal_kernel(metal_ctx, add_kernel, &
                                  [x_buffer, y_buffer, z_buffer], &
                                  [N], [256])
        
        ! Copy back
        call metal_memcpy(z_buffer, c_loc(z_metal), int(N * 4, int64), .false.)
        
        call cpu_time(end_time)
        metal_time = (end_time - start_time) * 1000.0  ! Convert to ms
        
        print *, ""
        print '(A,F0.2,A)', "Metal time: ", metal_time, " ms"
        
        ! Verify correctness (spot check)
        print *, ""
        print *, "Verification (first 5 elements):"
        do i = 1, min(5, N)
          print '(A,I0,A,F0.1,A,F0.1,A,F0.1)', &
                "  z[", i, "] = ", x(i), " + ", y(i), " = ", z_metal(i)
        end do
        
        ! Performance comparison
        print *, ""
        print *, "========================================="
        print *, "            RESULTS"
        print *, "========================================="
        print '(A,F0.2,A)', "Mock time:  ", mock_time, " ms"
        print '(A,F0.2,A)', "Metal time: ", metal_time, " ms"
        print *, ""
        
        if (metal_time < mock_time) then
          print '(A,F0.1,A)', "🏆 METAL WINS! ", &
                mock_time / metal_time, "x faster than mock!"
          print *, ""
          print *, "Real GPU execution beats print statements!"
        else
          print '(A,F0.1,A)', "😅 Mock wins by ", &
                metal_time / mock_time, "x"
          print *, ""
          print *, "Your hypothesis was correct - mocks are faster!"
          print *, "(All that Metal overhead for simple ops...)"
        end if
        
        ! Calculate effective bandwidth
        print *, ""
        print *, "Performance metrics:"
        print '(A,F0.1,A)', "  Effective bandwidth: ", &
              real(N * 4 * 3) / (metal_time * 1.0e6), " GB/s"
        print '(A,F0.1,A)', "  Throughput: ", &
              real(N) / (metal_time * 1.0e6), " GFLOPS"
        
      else
        print *, "❌ Failed to compile Metal kernel"
      end if
      
      ! Cleanup Metal
      call destroy_metal_context(metal_ctx)
    else
      print *, "❌ Failed to initialize Metal"
    end if
  else
    print *, "⚠️  Metal is not available on this system"
    print *, "   (Requires macOS with Metal-capable GPU)"
  end if
  
  ! Cleanup
  deallocate(x, y, z_mock, z_metal)
  
  print *, ""
  print *, "========================================="
  print *, "Test complete!"
  
end program test_metal_vs_mock