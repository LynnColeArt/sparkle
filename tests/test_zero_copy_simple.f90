program test_zero_copy_simple
  ! Simple Zero-Copy Conv2D Test
  ! ============================
  !
  ! Tests zero-copy implementation in isolation
  
  use kinds
  use iso_c_binding
  use gpu_opengl_zero_copy
  implicit none
  
  ! Small test parameters
  integer, parameter :: N = 1        ! Batch size
  integer, parameter :: C = 3        ! Input channels  
  integer, parameter :: H = 8        ! Height
  integer, parameter :: W = 8        ! Width
  integer, parameter :: K = 16       ! Output channels
  integer, parameter :: kernel_size = 3
  integer, parameter :: stride = 1
  integer, parameter :: pad = 1
  integer, parameter :: H_out = H    ! Same due to padding
  integer, parameter :: W_out = W
  
  ! Data arrays
  real(sp), allocatable :: input(:), weights(:), output(:)
  
  ! Timing
  real(sp) :: time_ms
  integer :: i
  
  ! Performance stats
  integer(i64) :: total_flops
  real(sp) :: gflops
  
  print *, "🧪 Simple Zero-Copy Test"
  print *, "======================="
  print *, ""
  
  ! Calculate workload size
  total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
  
  print '(A,I0,A)', "Workload: ", total_flops, " FLOPS"
  print '(A,I0,A,I0,A,I0,A,I0)', "Input: ", N, "x", C, "x", H, "x", W
  print '(A,I0,A,I0,A,I0)', "Conv: ", K, " filters, ", kernel_size, "x", kernel_size
  print *, ""
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  
  ! Initialize with simple test pattern
  do i = 1, size(input)
    input(i) = real(i, sp) * 0.01
  end do
  
  do i = 1, size(weights)
    weights(i) = real(i, sp) * 0.001
  end do
  
  ! Initialize zero-copy GPU
  print *, "Initializing GPU..."
  if (.not. gpu_init_zero_copy()) then
    print *, "❌ Failed to initialize zero-copy GPU"
    stop 1
  end if
  
  print *, ""
  print *, "Running conv2d..."
  
  ! Run convolution
  time_ms = gpu_execute_conv2d_zero_copy(input, weights, output, &
                                        N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  if (time_ms > 0) then
    gflops = real(total_flops) / (time_ms * 1.0e6)
    
    print *, ""
    print *, "✅ Success!"
    print '(A,F8.3,A)', "Time: ", time_ms, " ms"
    print '(A,F8.1,A)', "Performance: ", gflops, " GFLOPS"
    
    ! Show some output values
    print *, ""
    print *, "Sample output values:"
    do i = 1, min(10, size(output))
      print '(A,I3,A,F10.6)', "output(", i, ") = ", output(i)
    end do
  else
    print *, "❌ Execution failed!"
  end if
  
  ! Cleanup
  call gpu_cleanup_zero_copy()
  deallocate(input, weights, output)
  
  print *, ""
  print *, "Test complete!"
  
end program test_zero_copy_simple