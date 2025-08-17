program test_layer1_edge_cases
  use iso_fortran_env
  use sparkle_conv2d_v2
  implicit none
  
  real(real32), allocatable :: input(:), weights(:), output(:)
  integer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
  integer :: test_num
  character(len=100) :: test_name
  
  ! Initialize conv2d module
  call conv2d_init()
  
  print *, ""
  print *, "=== EVIL EDGE CASE TESTS FOR LAYER 1 ==="
  print *, ""
  
  ! Test 1: Tiny problem that fits in L1 cache
  test_num = 1
  test_name = "Tiny L1-resident problem"
  N = 1; C = 3; H = 8; W = 8; K = 8
  kernel_size = 3; stride = 1; pad = 1
  call run_test(test_num, test_name)
  
  ! Test 2: Medium problem that fits in L3 cache
  test_num = 2
  test_name = "Medium L3-resident problem"
  N = 1; C = 64; H = 56; W = 56; K = 64
  kernel_size = 3; stride = 1; pad = 1
  call run_test(test_num, test_name)
  
  ! Test 3: Large problem that spills to RAM
  test_num = 3
  test_name = "Large memory-bound problem"
  N = 8; C = 256; H = 56; W = 56; K = 256
  kernel_size = 3; stride = 1; pad = 1
  call run_test(test_num, test_name)
  
  ! Test 4: Pathological 1x1 convolution (all GEMM, no im2col)
  test_num = 4
  test_name = "1x1 convolution (pure GEMM)"
  N = 1; C = 512; H = 14; W = 14; K = 512
  kernel_size = 1; stride = 1; pad = 0
  call run_test(test_num, test_name)
  
  ! Test 5: Async with tiny problem (overhead test)
  test_num = 5
  test_name = "Async overhead on tiny problem"
  N = 1; C = 3; H = 4; W = 4; K = 8
  kernel_size = 3; stride = 1; pad = 1
  print *, ""
  print '(A,I0,A,A)', "Test ", test_num, ": ", trim(test_name)
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  input = 1.0; weights = 0.1; output = 0.0
  call conv2d(input, weights, output, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out, &
             device_hint="gpu", use_async=.true.)
  deallocate(input, weights, output)
  
  ! Test 6: Wrong environment variable values
  print *, ""
  print *, "Test 6: Invalid SPORKLE_GPU_ASYNC values"
  print *, "(Set SPORKLE_GPU_ASYNC='banana' externally to test)"
  
  ! Test 7: Rapidly switching between CPU and GPU
  print *, ""
  print *, "Test 7: Rapid device switching"
  N = 1; C = 64; H = 28; W = 28; K = 128
  kernel_size = 3; stride = 1; pad = 1
  H_out = (H + 2*pad - kernel_size) / stride + 1
  W_out = (W + 2*pad - kernel_size) / stride + 1
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output(N * K * H_out * W_out))
  input = 1.0; weights = 0.1
  
  print *, "  Switching: CPU->GPU->CPU->GPU..."
  output = 0.0
  call conv2d(input, weights, output, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out, device_hint="cpu")
  output = 0.0
  call conv2d(input, weights, output, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out, device_hint="gpu")
  output = 0.0
  call conv2d(input, weights, output, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out, device_hint="cpu")
  output = 0.0
  call conv2d(input, weights, output, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out, device_hint="gpu")
  
  deallocate(input, weights, output)
  
  ! Cleanup
  call conv2d_cleanup()
  
  print *, ""
  print *, "🧢 QA Beanie says: Did we survive all edge cases?"
  
contains

  subroutine run_test(test_id, name)
    integer, intent(in) :: test_id
    character(len=*), intent(in) :: name
    
    H_out = (H + 2*pad - kernel_size) / stride + 1
    W_out = (W + 2*pad - kernel_size) / stride + 1
    
    print *, ""
    print '(A,I0,A,A)', "Test ", test_id, ": ", trim(name)
    print '(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0)', &
      "  Problem: ", N, "x", C, "x", H, "x", W, " -> ", N, "x", K, "x", H_out, "x", W_out
    
    allocate(input(N * C * H * W))
    allocate(weights(K * C * kernel_size * kernel_size))
    allocate(output(N * K * H_out * W_out))
    
    input = 1.0
    weights = 0.1
    output = 0.0
    
    ! Test CPU
    print *, "  CPU:"
    call conv2d(input, weights, output, &
               N, C, H, W, K, kernel_size, stride, pad, H_out, W_out, &
               device_hint="cpu")
    
    ! Test GPU
    print *, "  GPU:"
    output = 0.0
    call conv2d(input, weights, output, &
               N, C, H, W, K, kernel_size, stride, pad, H_out, W_out, &
               device_hint="gpu")
    
    deallocate(input, weights, output)
    
  end subroutine run_test
  
end program test_layer1_edge_cases