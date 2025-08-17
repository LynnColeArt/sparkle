program test_simple_parallel
  use iso_fortran_env
  use sparkle_conv2d, only: conv2d_cpu
  use omp_lib
  implicit none
  
  ! Test parameters
  integer, parameter :: N = 1, C = 64, H = 56, W = 56, K = 64
  integer, parameter :: kernel_size = 3, stride = 1, pad = 1
  integer, parameter :: H_out = 56, W_out = 56
  
  real(real32), allocatable :: input(:), weights(:), output(:)
  integer :: input_size, weight_size, output_size
  real(real32) :: start_time, end_time, elapsed_time
  integer :: num_threads, i
  
  ! Calculate sizes
  input_size = N * C * H * W
  weight_size = K * C * kernel_size * kernel_size
  output_size = N * K * H_out * W_out
  
  ! Allocate arrays
  allocate(input(input_size), weights(weight_size), output(output_size))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(weights)
  input = (input - 0.5) * 2.0
  weights = (weights - 0.5) * 0.1
  
  print *, "🔧 Direct OpenMP Naive Implementation Test"
  print *, "=========================================="
  print *, ""
  
  ! Test different thread counts
  do i = 1, 5
    select case(i)
    case(1); num_threads = 1
    case(2); num_threads = 2 
    case(3); num_threads = 4
    case(4); num_threads = 8
    case(5); num_threads = 16
    end select
    
    call omp_set_num_threads(num_threads)
    
    ! Warmup
    output = 0.0
    call conv2d_cpu(input, weights, output, &
                    N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    
    ! Time the actual run
    call cpu_time(start_time)
    output = 0.0
    call conv2d_cpu(input, weights, output, &
                    N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    call cpu_time(end_time)
    
    elapsed_time = (end_time - start_time) * 1000.0  ! Convert to ms
    
    print '(A,I2,A,F8.2,A)', "Threads: ", num_threads, " → ", elapsed_time, " ms"
  end do
  
  deallocate(input, weights, output)
  
end program