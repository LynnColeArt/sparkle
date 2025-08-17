program test_adaptive_tiles
  use iso_fortran_env, only: real32, int64
  use cpu_conv2d_adaptive, only: conv2d_adaptive
  implicit none
  
  print *, "🎯 Testing Mini's Adaptive K×N Tiling - Phase 2"
  print *, "==============================================="
  print *, ""
  
  ! Test different workload sizes to validate adaptive tiling
  call test_workload("Small", 1, 64, 56, 56, 64)
  call test_workload("Medium", 1, 128, 64, 64, 128) 
  call test_workload("Large", 2, 256, 112, 112, 256)
  
contains

  subroutine test_workload(name, N, C, H, W, K)
    character(len=*), intent(in) :: name
    integer, intent(in) :: N, C, H, W, K
    
    real(real32), allocatable :: input(:), weights(:), output(:)
    integer :: H_out, W_out, input_size, weight_size, output_size
    integer, parameter :: kernel_size = 3, stride = 1, pad = 1
    real(real32) :: time_ms
    integer(int64) :: total_flops
    
    print *, "Testing ", trim(name), " workload:"
    print '(A,I0,A,I0,A,I0,A,I0,A,I0)', "  Config: ", N, "×", C, "×", H, "×", W, " → ", K, " channels"
    
    H_out = H; W_out = W
    input_size = N * C * H * W
    weight_size = K * C * kernel_size * kernel_size
    output_size = N * K * H_out * W_out
    
    allocate(input(input_size), weights(weight_size), output(output_size))
    
    call random_number(input)
    call random_number(weights)
    input = (input - 0.5) * 2.0
    weights = (weights - 0.5) * 0.1
    
    total_flops = int(N, int64) * int(K, int64) * int(H_out, int64) * int(W_out, int64) * &
                  int(C, int64) * int(kernel_size, int64) * int(kernel_size, int64) * 2_int64
    print '(A,F8.3,A)', "  Workload: ", real(total_flops) / 1.0e9, " GFLOPs"
    
    ! Test adaptive implementation  
    output = 0.0
    time_ms = conv2d_adaptive(input, weights, output, &
                             N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    
    print *, ""
    print *, "  📊 Results:"
    print '(A,F8.2,A,F8.1,A)', "    Adaptive: ", time_ms, " ms (", &
                                real(total_flops) / (time_ms * 1.0e6), " GFLOPS)"
    
    if (real(total_flops) / (time_ms * 1.0e6) > 35.0) then
      print *, "  ✅ Great performance! Approaching 50+ GFLOPS target"
    else if (real(total_flops) / (time_ms * 1.0e6) > 20.0) then
      print *, "  📈 Good improvement over baseline"
    else
      print *, "  ⚠️  Performance below baseline - need optimization"
    end if
    print *, "  " // repeat("-", 50)
    print *, ""
    
    deallocate(input, weights, output)
  end subroutine test_workload
  
end program test_adaptive_tiles