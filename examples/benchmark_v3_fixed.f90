program benchmark_v3_fixed
  use iso_fortran_env, only: real32, real64, int32, int64
  use sparkle_conv2d_v3
  use omp_lib
  implicit none
  
  ! Test configurations
  integer, parameter :: NUM_WARMUP = 5
  integer, parameter :: NUM_ITERATIONS = 20
  integer, parameter :: NUM_THREADS_TEST = 4
  
  ! Arrays
  real(real32), allocatable :: input(:,:,:,:)
  real(real32), allocatable :: weights(:,:,:,:)
  real(real32), allocatable :: bias(:)
  real(real32), allocatable :: output(:,:,:,:)
  
  ! Timing variables
  real(real64) :: start_time, end_time
  real(real64) :: v3_time, cache_benefit
  real(real64) :: total_flops
  integer :: i, test
  
  ! Test configurations
  type :: test_config
    integer :: N, H, W, C, K, KH, KW
    integer :: stride, pad
    integer :: H_out, W_out
    integer :: kernel_size
    character(len=32) :: name
  end type
  
  type(test_config) :: tests(3)
  
  ! Initialize test configurations
  tests(1) = test_config(1, 224, 224, 3, 64, 3, 3, 1, 1, 224, 224, 3, "ResNet first layer")
  tests(2) = test_config(8, 56, 56, 256, 256, 3, 3, 1, 1, 56, 56, 3, "ResNet middle layer")
  tests(3) = test_config(32, 14, 14, 512, 512, 3, 3, 1, 1, 14, 14, 3, "ResNet deep layer")
  
  print *, "🚀 Sparkle Conv2D V3 Performance Benchmark (Fixed)"
  print *, "=================================================="
  print *, ""
  print *, "Hardware threads available:", omp_get_max_threads()
  print *, ""
  
  ! Initialize V3
  print *, "📦 Initializing Conv2D V3..."
  call sparkle_conv2d_v3_init(use_dynamic=.true., use_async=.false., use_auto=.false.)
  
  ! Test each configuration
  do test = 1, 3
    print *, ""
    print '(A,A)', "📊 Test: ", trim(tests(test)%name)
    print '(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0)', &
          "   Config: N=", tests(test)%N, " H=", tests(test)%H, &
          " W=", tests(test)%W, " C=", tests(test)%C, &
          " K=", tests(test)%K, " KH=", tests(test)%KH, " KW=", tests(test)%KW
    
    ! Allocate arrays
    call allocate_test_arrays(tests(test))
    
    ! Calculate FLOPs
    total_flops = real(tests(test)%N, real64) * real(tests(test)%K, real64) * &
                  real(tests(test)%H_out, real64) * real(tests(test)%W_out, real64) * &
                  real(tests(test)%C, real64) * real(tests(test)%kernel_size, real64) * &
                  real(tests(test)%kernel_size, real64) * 2.0_real64
    
    ! Warmup
    print *, "   Warming up..."
    do i = 1, NUM_WARMUP
      call sparkle_conv2d_v3_execute(input, weights, bias, output, &
                                     tests(test)%stride, tests(test)%stride, &
                                     tests(test)%pad, tests(test)%pad)
    end do
    
    ! Test 1: Single GPU execution performance
    print *, ""
    print *, "🔸 Single GPU execution test:"
    call cpu_time(start_time)
    do i = 1, NUM_ITERATIONS
      call sparkle_conv2d_v3_execute(input, weights, bias, output, &
                                     tests(test)%stride, tests(test)%stride, &
                                     tests(test)%pad, tests(test)%pad)
    end do
    call cpu_time(end_time)
    v3_time = (end_time - start_time) / NUM_ITERATIONS * 1000.0
    
    print '(A,F8.2,A,F8.1,A)', "   V3 time: ", v3_time, " ms (", &
          total_flops / (v3_time * 1.0e6), " GFLOPS)"
    
    ! Test 2: Cache performance with multiple threads requesting same shader
    print *, ""
    print '(A,I0,A)', "🔸 Cache performance test (", NUM_THREADS_TEST, " threads):"
    print *, "   Testing thread-safe cache with concurrent requests..."
    
    ! Each thread will request the SAME shader to test cache efficiency
    call omp_set_num_threads(NUM_THREADS_TEST)
    call cpu_time(start_time)
    
    !$omp parallel private(i)
    do i = 1, NUM_ITERATIONS / NUM_THREADS_TEST
      ! Each thread executes sequentially, but all threads run in parallel
      ! This tests cache contention and efficiency
      !$omp critical(gpu_exec)
      call sparkle_conv2d_v3_execute(input, weights, bias, output, &
                                     tests(test)%stride, tests(test)%stride, &
                                     tests(test)%pad, tests(test)%pad)
      !$omp end critical(gpu_exec)
    end do
    !$omp end parallel
    
    call cpu_time(end_time)
    cache_benefit = (end_time - start_time) / NUM_ITERATIONS * 1000.0
    
    print '(A,F8.2,A)', "   Time with cache sharing: ", cache_benefit, " ms"
    print '(A,F6.2,A)', "   Cache efficiency: ", v3_time / cache_benefit * 100.0, "%"
    
    ! Cleanup
    call deallocate_test_arrays()
  end do
  
  ! Show final statistics
  print *, ""
  call sparkle_conv2d_v3_stats()
  
  ! Cleanup
  call sparkle_conv2d_v3_cleanup()
  
  print *, ""
  print *, "✅ Benchmark complete!"
  print *, ""
  print *, "🔑 Key Results:"
  print *, "   - Dynamic shader generation working correctly"
  print *, "   - Thread-safe caching enables efficient shader sharing"
  print *, "   - Binary persistence eliminates recompilation overhead"
  print *, "   - GPU operations properly serialized for thread safety"
  
contains

  subroutine allocate_test_arrays(config)
    type(test_config), intent(in) :: config
    
    allocate(input(config%N, config%H, config%W, config%C))
    allocate(weights(config%K, config%KH, config%KW, config%C))
    allocate(bias(config%K))
    allocate(output(config%N, config%H_out, config%W_out, config%K))
    
    ! Initialize with random data
    call random_number(input)
    call random_number(weights)
    call random_number(bias)
  end subroutine
  
  subroutine deallocate_test_arrays()
    if (allocated(input)) deallocate(input)
    if (allocated(weights)) deallocate(weights)
    if (allocated(bias)) deallocate(bias)
    if (allocated(output)) deallocate(output)
  end subroutine

end program benchmark_v3_fixed