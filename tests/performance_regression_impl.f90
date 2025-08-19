module performance_regression_impl
  ! Actual implementations of performance benchmarks
  ! These call the real production code to measure performance
  
  use iso_fortran_env, only: real32, real64, int32, int64
  use time_utils, only: tic, toc_seconds
  use flopcount, only: conv2d_flops
  implicit none
  
  private
  public :: benchmark_cpu_simd, benchmark_gpu_single, benchmark_gpu_async
  
  ! Standard test parameters for consistency
  integer, parameter :: N = 1, C = 3, H = 224, W = 224, K = 64
  integer, parameter :: kernel_size = 7, stride = 2, pad = 3
  integer, parameter :: H_out = (H + 2*pad - kernel_size) / stride + 1
  integer, parameter :: W_out = (W + 2*pad - kernel_size) / stride + 1
  
contains

  function benchmark_cpu_simd() result(gflops)
    use cpu_conv2d_reference, only: conv2d_cpu_benchmark
    real(dp) :: gflops
    real(sp) :: time_ms
    integer(i64) :: total_flops
    
    ! Calculate total FLOPs for this workload
    total_flops = conv2d_flops(int(N,int64), int(H_out,int64), int(W_out,int64), &
                              int(K,int64), int(C,int64), &
                              int(kernel_size,int64), int(kernel_size,int64))
    
    ! Run the CPU benchmark (it handles warmup internally)
    time_ms = conv2d_cpu_benchmark(N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    
    ! Calculate GFLOPS
    gflops = real(total_flops, real64) / (real(time_ms, real64) * 1.0e6_real64)
    
  end function benchmark_cpu_simd
  
  function benchmark_gpu_single() result(gflops)
    use sporkle_gpu_dispatch, only: execute_conv2d_gpu
    real(dp) :: gflops
    real(sp), allocatable :: input(:), weights(:), output(:)
    real(sp) :: time_ms
    integer(i64) :: total_flops
    integer :: i, warmup_runs = 5, bench_runs = 10
    integer(i64) :: start_time
    real(dp) :: total_time
    
    ! Allocate arrays
    allocate(input(N*C*H*W))
    allocate(weights(K*C*kernel_size*kernel_size))
    allocate(output(N*K*H_out*W_out))
    
    ! Initialize with test data
    call random_number(input)
    call random_number(weights)
    
    ! Warmup runs
    do i = 1, warmup_runs
      time_ms = execute_conv2d_gpu(input, weights, output, &
                                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    end do
    
    ! Benchmark runs
    total_time = 0.0_real64
    do i = 1, bench_runs
      time_ms = execute_conv2d_gpu(input, weights, output, &
                                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
      total_time = total_time + real(time_ms, real64)
    end do
    
    ! Average time
    time_ms = real(total_time / real(bench_runs, real64), real32)
    
    ! Calculate total FLOPs
    total_flops = conv2d_flops(int(N,int64), int(H_out,int64), int(W_out,int64), &
                              int(K,int64), int(C,int64), &
                              int(kernel_size,int64), int(kernel_size,int64))
    
    ! Calculate GFLOPS
    gflops = real(total_flops, real64) / (real(time_ms, real64) * 1.0e6_real64)
    
    deallocate(input, weights, output)
    
  end function benchmark_gpu_single
  
  subroutine benchmark_gpu_async(gflops, speedup)
    use gpu_async_executor
    real(dp), intent(out) :: gflops, speedup
    real(sp), allocatable :: input(:), weights(:), output(:)
    real(sp) :: sync_time_ms, async_time_ms
    integer(i64) :: total_flops
    type(gpu_async_state) :: async_state
    integer :: batch_size = 10  ! Number of operations to pipeline
    integer :: i, status
    integer(i64) :: start_time
    real(dp) :: total_time
    
    ! Allocate arrays
    allocate(input(N*C*H*W))
    allocate(weights(K*C*kernel_size*kernel_size))
    allocate(output(N*K*H_out*W_out))
    
    ! Initialize test data
    call random_number(input)
    call random_number(weights)
    
    ! First, measure synchronous performance
    call tic(start_time)
    do i = 1, batch_size
      sync_time_ms = execute_conv2d_gpu(input, weights, output, &
                                       N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    end do
    total_time = toc_seconds(start_time) * 1000.0_real64
    sync_time_ms = real(total_time, real32)
    
    ! Initialize async executor
    status = gpu_async_executor_init(async_state, size(input), size(weights), size(output))
    if (status /= 0) then
      gflops = 0.0_real64
      speedup = 0.0_real64
      return
    end if
    
    ! Measure async performance
    call tic(start_time)
    
    ! Submit all work asynchronously
    do i = 1, batch_size
      call gpu_async_conv2d(async_state, input, weights, output, &
                           N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    end do
    
    ! Wait for all work to complete
    do i = 1, batch_size
      do while (.not. gpu_is_work_complete(async_state, i))
        ! Busy wait
      end do
    end do
    
    total_time = toc_seconds(start_time) * 1000.0_real64
    async_time_ms = real(total_time, real32)
    
    ! Cleanup
    call gpu_async_executor_cleanup(async_state)
    
    ! Calculate metrics
    total_flops = conv2d_flops(int(N,int64), int(H_out,int64), int(W_out,int64), &
                              int(K,int64), int(C,int64), &
                              int(kernel_size,int64), int(kernel_size,int64))
    total_flops = total_flops * batch_size  ! Multiple operations
    
    gflops = real(total_flops, real64) / (real(async_time_ms, real64) * 1.0e6_real64)
    speedup = real(sync_time_ms, real64) / real(async_time_ms, real64)
    
    deallocate(input, weights, output)
    
  end subroutine benchmark_gpu_async
  
end module performance_regression_impl