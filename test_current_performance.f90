program test_current_performance
  use iso_fortran_env, only: real32, int64
  use cpu_conv2d_fused_final, only: conv2d_fused_final
  implicit none
  
  print *, "🎯 Current CPU Performance - Mini's Baseline"
  print *, "==========================================="
  print *, ""
  
  ! Test different workload sizes
  call test_workload("Small", 1, 64, 56, 56, 64)
  call test_workload("Medium", 1, 128, 64, 64, 128) 
  call test_workload("Large", 4, 256, 112, 112, 256)
  
contains

  subroutine test_workload(name, N, C, H, W, K)
    character(len=*), intent(in) :: name
    integer, intent(in) :: N, C, H, W, K
    
    real(real32), allocatable :: input(:), weights(:), output(:)
    integer :: H_out, W_out, input_size, weight_size, output_size
    integer, parameter :: kernel_size = 3, stride = 1, pad = 1
    real(real32) :: time_1, time_8, speedup
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
    
    ! Test with 1 thread
    call set_omp_threads(1)
    output = 0.0
    time_1 = conv2d_fused_final(input, weights, output, &
                               N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    
    ! Test with 8 threads  
    call set_omp_threads(8)
    output = 0.0
    time_8 = conv2d_fused_final(input, weights, output, &
                               N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    
    speedup = time_1 / time_8
    
    print *, ""
    print *, "  📊 Results:"
    print '(A,F8.2,A,F8.1,A)', "    1 thread:  ", time_1, " ms (", &
                                real(total_flops) / (time_1 * 1.0e6), " GFLOPS)"
    print '(A,F8.2,A,F8.1,A,F5.2,A)', "    8 threads: ", time_8, " ms (", &
                                       real(total_flops) / (time_8 * 1.0e6), " GFLOPS) - ", &
                                       speedup, "x speedup"
    print *, ""
    
    if (real(total_flops) / (time_8 * 1.0e6) > 30.0) then
      print *, "  ✅ Good performance achieved"
    else
      print *, "  📈 Room for improvement (target: 50+ GFLOPS)"
    end if
    print *, "  " // repeat("-", 50)
    print *, ""
    
    deallocate(input, weights, output)
  end subroutine test_workload
  
  subroutine set_omp_threads(n)
    integer, intent(in) :: n
    interface
      subroutine omp_set_num_threads(n) bind(C, name="omp_set_num_threads")
        integer, value :: n
      end subroutine
    end interface
    call omp_set_num_threads(n)
  end subroutine
  
end program test_current_performance