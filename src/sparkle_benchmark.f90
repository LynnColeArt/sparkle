module sparkle_benchmark
  ! Performance benchmarking with hot/cold measurements
  ! The Sparkle Way: Know thy performance
  
  use iso_fortran_env, only: int32, int64, real32, real64
  use sparkle_types
  use sparkle_kernels
  use sparkle_execute
  use sparkle_mesh_types
  implicit none
  private
  
  public :: benchmark_result, benchmark_suite
  public :: benchmark_kernel, create_benchmark_suite
  public :: BENCH_COLD, BENCH_HOT, BENCH_STATS
  
  ! Benchmark modes
  integer, parameter :: BENCH_COLD = 1   ! First run, cold caches
  integer, parameter :: BENCH_HOT = 2    ! After warm-up
  integer, parameter :: BENCH_STATS = 3  ! Statistical analysis
  
  type :: benchmark_result
    character(len=:), allocatable :: name
    real(real64) :: cold_time_ms = 0.0_real64      ! First run time
    real(real64) :: hot_time_ms = 0.0_real64       ! Best warm time
    real(real64) :: mean_time_ms = 0.0_real64      ! Average of hot runs
    real(real64) :: min_time_ms = huge(1.0_real64) ! Minimum time
    real(real64) :: max_time_ms = 0.0_real64       ! Maximum time
    real(real64) :: stddev_ms = 0.0_real64         ! Standard deviation
    real(real64) :: gflops = 0.0_real64            ! Performance in GFLOPS
    real(real64) :: bandwidth_gbs = 0.0_real64     ! Memory bandwidth
    integer(int64) :: work_items = 0
    integer :: num_runs = 0
    integer :: warmup_runs = 0
  contains
    procedure :: print => result_print
    procedure :: speedup => result_speedup
  end type benchmark_result
  
  type :: benchmark_suite
    type(benchmark_result), allocatable :: results(:)
    integer :: num_benchmarks = 0
    character(len=:), allocatable :: suite_name
    integer :: default_warmup_runs = 5
    integer :: default_bench_runs = 100
  contains
    procedure :: add => suite_add_benchmark
    procedure :: run => suite_run_all
    procedure :: report => suite_report
    procedure :: compare => suite_compare
  end type benchmark_suite
  
contains

  function benchmark_kernel(kernel, mesh, warmup_runs, bench_runs) result(result)
    type(sparkle_kernel), intent(inout) :: kernel
    type(mesh_topology), intent(inout) :: mesh
    integer, intent(in), optional :: warmup_runs, bench_runs
    type(benchmark_result) :: result
    
    integer :: n_warmup, n_bench, i
    real(real64) :: start_time, end_time
    real(real64), allocatable :: times(:)
    real(real64) :: total_time, total_squared
    
    ! Set run counts
    n_warmup = 5
    n_bench = 100
    if (present(warmup_runs)) n_warmup = warmup_runs
    if (present(bench_runs)) n_bench = bench_runs
    
    result%name = kernel%name
    result%work_items = kernel%work_items
    result%warmup_runs = n_warmup
    result%num_runs = n_bench
    
    allocate(times(n_bench))
    
    print '(A,A,A)', "Benchmarking '", kernel%name, "'..."
    
    ! Cold run (first execution)
    print *, "  Cold run..."
    call cpu_time(start_time)
    call sparkle_run(kernel, mesh)
    call cpu_time(end_time)
    result%cold_time_ms = (end_time - start_time) * 1000.0_real64
    
    ! Warm-up runs
    print '(A,I0,A)', "  Warming up (", n_warmup, " runs)..."
    do i = 1, n_warmup
      call sparkle_run_quiet(kernel, mesh)
    end do
    
    ! Hot runs for statistics
    print '(A,I0,A)', "  Benchmarking (", n_bench, " runs)..."
    do i = 1, n_bench
      call cpu_time(start_time)
      call sparkle_run_quiet(kernel, mesh)
      call cpu_time(end_time)
      times(i) = (end_time - start_time) * 1000.0_real64
      
      ! Track min/max
      if (times(i) < result%min_time_ms) result%min_time_ms = times(i)
      if (times(i) > result%max_time_ms) result%max_time_ms = times(i)
    end do
    
    ! Calculate statistics
    total_time = sum(times)
    result%mean_time_ms = total_time / real(n_bench, real64)
    result%hot_time_ms = result%min_time_ms  ! Best hot time
    
    ! Standard deviation
    total_squared = 0.0_real64
    do i = 1, n_bench
      total_squared = total_squared + (times(i) - result%mean_time_ms)**2
    end do
    result%stddev_ms = sqrt(total_squared / real(n_bench - 1, real64))
    
    ! Calculate performance metrics
    if (result%hot_time_ms > 0.0_real64) then
      ! Estimate FLOPS (kernel-specific)
      block
        real(real64) :: total_flops
        total_flops = kernel%estimate_flops()
        result%gflops = total_flops / (result%hot_time_ms * 1.0e6_real64)
      end block
      
      ! Estimate bandwidth (assumes each work item touches 3 floats)
      block
        real(real64) :: total_bytes
        total_bytes = real(kernel%work_items * 3 * 4, real64)  ! 3 floats per item
        result%bandwidth_gbs = total_bytes / (result%hot_time_ms * 1.0e6_real64)
      end block
    end if
    
    deallocate(times)
    
  end function benchmark_kernel
  
  subroutine result_print(this)
    class(benchmark_result), intent(in) :: this
    
    print *, ""
    print '(A,A)', "Benchmark: ", this%name
    print '(A)', "-------------------------------------"
    print '(A,I0)', "Work items:      ", this%work_items
    print '(A,I0)', "Warmup runs:     ", this%warmup_runs
    print '(A,I0)', "Benchmark runs:  ", this%num_runs
    print *, ""
    print '(A,F10.3,A)', "Cold time:       ", this%cold_time_ms, " ms (first run)"
    print '(A,F10.3,A)', "Hot time:        ", this%hot_time_ms, " ms (best)"
    print '(A,F10.3,A)', "Mean time:       ", this%mean_time_ms, " ms"
    print '(A,F10.3,A)', "Std deviation:   ", this%stddev_ms, " ms"
    print '(A,F10.3,A)', "Min time:        ", this%min_time_ms, " ms"
    print '(A,F10.3,A)', "Max time:        ", this%max_time_ms, " ms"
    print *, ""
    print '(A,F8.1)', "Cold/Hot speedup: ", this%speedup()
    print '(A,F10.3,A)', "Performance:     ", this%gflops, " GFLOPS"
    print '(A,F10.3,A)', "Bandwidth:       ", this%bandwidth_gbs, " GB/s"
    print *, ""
    
  end subroutine result_print
  
  function result_speedup(this) result(speedup)
    class(benchmark_result), intent(in) :: this
    real(real64) :: speedup
    
    if (this%hot_time_ms > 0.0_real64) then
      speedup = this%cold_time_ms / this%hot_time_ms
    else
      speedup = 0.0_real64
    end if
    
  end function result_speedup
  
  function create_benchmark_suite(name) result(suite)
    character(len=*), intent(in) :: name
    type(benchmark_suite) :: suite
    
    suite%suite_name = name
    suite%num_benchmarks = 0
    
  end function create_benchmark_suite
  
  subroutine suite_add_benchmark(this, result)
    class(benchmark_suite), intent(inout) :: this
    type(benchmark_result), intent(in) :: result
    
    type(benchmark_result), allocatable :: temp(:)
    
    if (allocated(this%results)) then
      allocate(temp(this%num_benchmarks + 1))
      temp(1:this%num_benchmarks) = this%results
      temp(this%num_benchmarks + 1) = result
      call move_alloc(temp, this%results)
    else
      allocate(this%results(1))
      this%results(1) = result
    end if
    
    this%num_benchmarks = this%num_benchmarks + 1
    
  end subroutine suite_add_benchmark
  
  subroutine suite_run_all(this, kernels, mesh)
    class(benchmark_suite), intent(inout) :: this
    type(sparkle_kernel), intent(inout) :: kernels(:)
    type(mesh_topology), intent(inout) :: mesh
    
    integer :: i
    type(benchmark_result) :: result
    
    print *, "🏃 Running benchmark suite: ", this%suite_name
    print *, "============================================"
    
    do i = 1, size(kernels)
      result = benchmark_kernel(kernels(i), mesh, &
                               this%default_warmup_runs, &
                               this%default_bench_runs)
      call this%add(result)
    end do
    
  end subroutine suite_run_all
  
  subroutine suite_report(this)
    class(benchmark_suite), intent(in) :: this
    
    integer :: i
    real(real64) :: total_cold, total_hot, total_gflops
    
    print *, ""
    print *, "📊 Benchmark Suite Report: ", this%suite_name
    print *, "================================================"
    print *, ""
    
    ! Summary table
    print '(A)', "Kernel            Cold(ms)  Hot(ms)  Speedup  GFLOPS   GB/s"
    print '(A)', "------            --------  -------  -------  ------   ----"
    
    total_cold = 0.0_real64
    total_hot = 0.0_real64
    total_gflops = 0.0_real64
    
    do i = 1, this%num_benchmarks
      print '(A16,2X,F8.2,2X,F7.2,2X,F7.1,A,2X,F6.1,3X,F6.1)', &
        adjustl(this%results(i)%name(1:min(16,len(this%results(i)%name)))), &
        this%results(i)%cold_time_ms, &
        this%results(i)%hot_time_ms, &
        this%results(i)%speedup(), "x", &
        this%results(i)%gflops, &
        this%results(i)%bandwidth_gbs
        
      total_cold = total_cold + this%results(i)%cold_time_ms
      total_hot = total_hot + this%results(i)%hot_time_ms
      total_gflops = total_gflops + this%results(i)%gflops
    end do
    
    print *, ""
    print '(A,F10.2,A)', "Total cold time:     ", total_cold, " ms"
    print '(A,F10.2,A)', "Total hot time:      ", total_hot, " ms"
    print '(A,F10.2)', "Overall speedup:     ", total_cold / total_hot
    print '(A,F10.2,A)', "Aggregate GFLOPS:    ", total_gflops, " GFLOPS"
    
  end subroutine suite_report
  
  subroutine suite_compare(this, other)
    class(benchmark_suite), intent(in) :: this
    type(benchmark_suite), intent(in) :: other
    
    integer :: i, j
    real(real64) :: speedup
    logical :: found
    
    print *, ""
    print '(A,A,A,A,A)', "📊 Comparing '", this%suite_name, "' vs '", other%suite_name, "'"
    print *, "================================================"
    
    do i = 1, this%num_benchmarks
      found = .false.
      do j = 1, other%num_benchmarks
        if (this%results(i)%name == other%results(j)%name) then
          found = .true.
          speedup = other%results(j)%hot_time_ms / this%results(i)%hot_time_ms
          
          print '(A,A)', "Kernel: ", this%results(i)%name
          print '(A,F8.2,A,F8.2,A,F6.2,A)', &
            "  This:  ", this%results(i)%hot_time_ms, " ms, ", &
            this%results(i)%gflops, " GFLOPS"
          print '(A,F8.2,A,F8.2,A,F6.2,A)', &
            "  Other: ", other%results(j)%hot_time_ms, " ms, ", &
            other%results(j)%gflops, " GFLOPS"
          
          if (speedup > 1.0_real64) then
            print '(A,F6.2,A)', "  ✅ This is ", speedup, "x faster!"
          else
            print '(A,F6.2,A)', "  ❌ Other is ", 1.0_real64/speedup, "x faster"
          end if
          print *, ""
          
          exit
        end if
      end do
      
      if (.not. found) then
        print '(A,A,A)', "Kernel '", this%results(i)%name, "' not found in other suite"
      end if
    end do
    
  end subroutine suite_compare

end module sparkle_benchmark