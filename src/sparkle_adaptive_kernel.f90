module sparkle_adaptive_kernel
  use iso_c_binding
  use sparkle_types
  implicit none
  
  private
  public :: adaptive_kernel, kernel_variant, select_optimal_variant
  public :: probe_all_variants, create_adaptive_kernel, cleanup_adaptive_kernel
  public :: add_kernel_variant, print_variant_stats, force_variant
  
  ! Define missing type parameters
  integer, parameter :: dp = kind(1.0d0)
  integer, parameter :: int64 = c_int64_t
  
  integer, parameter :: MAX_VARIANTS = 10
  integer, parameter :: REPROBE_INTERVAL = 3600  ! seconds
  real(dp), parameter :: WORKLOAD_CHANGE_THRESHOLD = 2.0_dp
  
  ! Kernel variant structure
  type :: kernel_variant
    character(len=32) :: name
    integer :: variant_type  ! 1=GLSL, 2=SPIR-V, 3=Direct
    procedure(kernel_execute_interface), pointer, nopass :: execute => null()
    real(dp) :: best_time
    real(dp) :: mean_time
    real(dp) :: std_dev
    integer :: measurement_count
    logical :: is_valid
    integer(int64) :: last_workload_size
  end type kernel_variant
  
  ! Adaptive kernel container
  type :: adaptive_kernel
    type(kernel_variant) :: variants(MAX_VARIANTS)
    integer :: num_variants
    integer :: selected_variant
    real(dp) :: last_probe_time
    integer(int64) :: last_workload_size
    character(len=64) :: kernel_name
  end type adaptive_kernel
  
  ! Interface for kernel execution
  abstract interface
    subroutine kernel_execute_interface(device, params, input, output, status)
      import :: c_ptr, c_int
      type(c_ptr), intent(in) :: device
      type(c_ptr), intent(in) :: params
      type(c_ptr), intent(in) :: input
      type(c_ptr), intent(inout) :: output
      integer(c_int), intent(out) :: status
    end subroutine kernel_execute_interface
  end interface
  
contains

  function create_adaptive_kernel(kernel_name) result(kernel)
    character(len=*), intent(in) :: kernel_name
    type(adaptive_kernel) :: kernel
    
    kernel%kernel_name = kernel_name
    kernel%num_variants = 0
    kernel%selected_variant = 0
    kernel%last_probe_time = 0.0_dp
    kernel%last_workload_size = 0
    
  end function create_adaptive_kernel
  
  subroutine add_kernel_variant(kernel, name, variant_type, execute_fn)
    type(adaptive_kernel), intent(inout) :: kernel
    character(len=*), intent(in) :: name
    integer, intent(in) :: variant_type
    procedure(kernel_execute_interface) :: execute_fn
    
    if (kernel%num_variants >= MAX_VARIANTS) then
      print *, "WARNING: Maximum variants reached, cannot add", name
      return
    end if
    
    kernel%num_variants = kernel%num_variants + 1
    kernel%variants(kernel%num_variants)%name = name
    kernel%variants(kernel%num_variants)%variant_type = variant_type
    kernel%variants(kernel%num_variants)%execute => execute_fn
    kernel%variants(kernel%num_variants)%is_valid = .true.
    kernel%variants(kernel%num_variants)%best_time = 1.0e9_dp  ! Large initial value
    kernel%variants(kernel%num_variants)%measurement_count = 0
    
  end subroutine add_kernel_variant

  function select_optimal_variant(kernel, workload_size) result(variant_idx)
    type(adaptive_kernel), intent(inout) :: kernel
    integer(int64), intent(in) :: workload_size
    integer :: variant_idx
    
    ! Check if we should reprobe
    if (should_reprobe(kernel, workload_size)) then
      call probe_all_variants(kernel, workload_size)
    end if
    
    ! Return selected variant
    variant_idx = kernel%selected_variant
    
    ! Default to first valid variant if none selected
    if (variant_idx == 0 .and. kernel%num_variants > 0) then
      do variant_idx = 1, kernel%num_variants
        if (kernel%variants(variant_idx)%is_valid) exit
      end do
      kernel%selected_variant = variant_idx
    end if
    
  end function select_optimal_variant
  
  logical function should_reprobe(kernel, workload_size)
    type(adaptive_kernel), intent(in) :: kernel
    integer(int64), intent(in) :: workload_size
    real(dp) :: current_time, elapsed_time
    real(dp) :: size_ratio
    
    call cpu_time(current_time)
    elapsed_time = current_time - kernel%last_probe_time
    
    ! First run
    if (kernel%selected_variant == 0) then
      should_reprobe = .true.
      return
    end if
    
    ! Periodic reprobing
    if (elapsed_time > REPROBE_INTERVAL) then
      should_reprobe = .true.
      return
    end if
    
    ! Workload size change
    if (kernel%last_workload_size > 0) then
      size_ratio = real(workload_size) / real(kernel%last_workload_size)
      if (size_ratio > WORKLOAD_CHANGE_THRESHOLD .or. &
          size_ratio < 1.0_dp / WORKLOAD_CHANGE_THRESHOLD) then
        should_reprobe = .true.
        return
      end if
    end if
    
    should_reprobe = .false.
    
  end function should_reprobe
  
  subroutine probe_all_variants(kernel, workload_size)
    type(adaptive_kernel), intent(inout) :: kernel
    integer(int64), intent(in) :: workload_size
    real(dp) :: times(MAX_VARIANTS)
    integer :: i, best_idx
    
    print *, "Probing kernel variants for", trim(kernel%kernel_name), &
             "with workload size", workload_size
    
    ! Initialize times
    times = 1.0e9_dp
    
    ! Benchmark each variant
    do i = 1, kernel%num_variants
      if (kernel%variants(i)%is_valid) then
        times(i) = benchmark_variant(kernel%variants(i), workload_size)
        print *, "  ", trim(kernel%variants(i)%name), ":", times(i) * 1000.0_dp, "ms"
      else
        print *, "  ", trim(kernel%variants(i)%name), ": invalid/disabled"
      end if
    end do
    
    ! Select best variant
    best_idx = 1
    do i = 2, kernel%num_variants
      if (times(i) < times(best_idx)) best_idx = i
    end do
    
    kernel%selected_variant = best_idx
    kernel%last_probe_time = get_current_time()
    kernel%last_workload_size = workload_size
    
    print *, "Selected variant:", trim(kernel%variants(best_idx)%name)
    print *, ""
    
  end subroutine probe_all_variants
  
  function benchmark_variant(variant, workload_size) result(best_time)
    type(kernel_variant), intent(inout) :: variant
    integer(int64), intent(in) :: workload_size
    real(dp) :: best_time
    real(dp) :: times(100), sum_time, sum_sq
    real(dp) :: start_time, end_time
    integer :: i, warmup_iters, measure_iters
    integer(c_int) :: status
    type(c_ptr) :: dummy_ptrs(4)
    
    ! Determine iteration counts based on workload size
    if (workload_size < 1000000) then
      warmup_iters = 10
      measure_iters = 100
    else
      warmup_iters = 5
      measure_iters = 20
    end if
    
    ! Warmup phase
    do i = 1, warmup_iters
      dummy_ptrs = c_null_ptr  ! In real use, these would be actual buffers
      call variant%execute(dummy_ptrs(1), dummy_ptrs(2), dummy_ptrs(3), dummy_ptrs(4), status)
      if (status /= 0) then
        variant%is_valid = .false.
        best_time = 1.0e9_dp
        return
      end if
    end do
    
    ! Measurement phase
    sum_time = 0.0_dp
    sum_sq = 0.0_dp
    best_time = 1.0e9_dp
    
    do i = 1, measure_iters
      call cpu_time(start_time)
      call variant%execute(dummy_ptrs(1), dummy_ptrs(2), dummy_ptrs(3), dummy_ptrs(4), status)
      call cpu_time(end_time)
      
      times(i) = end_time - start_time
      sum_time = sum_time + times(i)
      sum_sq = sum_sq + times(i)**2
      if (times(i) < best_time) best_time = times(i)
    end do
    
    ! Update statistics
    variant%best_time = best_time
    variant%mean_time = sum_time / measure_iters
    variant%std_dev = sqrt(sum_sq / measure_iters - variant%mean_time**2)
    variant%measurement_count = variant%measurement_count + measure_iters
    variant%last_workload_size = workload_size
    
  end function benchmark_variant
  
  function get_current_time() result(time)
    real(dp) :: time
    call cpu_time(time)
  end function get_current_time
  
  subroutine print_variant_stats(kernel)
    type(adaptive_kernel), intent(in) :: kernel
    integer :: i
    character(len=3) :: mark
    
    print *, "Kernel variants for", trim(kernel%kernel_name), ":"
    print *, "  Name                Best(ms)    Mean(ms)    StdDev(ms)  Status"
    print *, "  ----                --------    --------    ----------  ------"
    
    do i = 1, kernel%num_variants
      if (i == kernel%selected_variant) then
        mark = " *"
      else
        mark = "  "
      end if
      
      if (kernel%variants(i)%is_valid) then
        print '(A,A,A20,3F12.3,A)', mark, " ", &
          kernel%variants(i)%name, &
          kernel%variants(i)%best_time * 1000.0_dp, &
          kernel%variants(i)%mean_time * 1000.0_dp, &
          kernel%variants(i)%std_dev * 1000.0_dp, &
          "  valid"
      else
        print '(A,A,A20,A)', mark, " ", &
          kernel%variants(i)%name, &
          "         -           -           -     invalid"
      end if
    end do
    
    print *, ""
    print *, "* = currently selected variant"
    
  end subroutine print_variant_stats
  
  subroutine force_variant(kernel, variant_name)
    type(adaptive_kernel), intent(inout) :: kernel
    character(len=*), intent(in) :: variant_name
    integer :: i
    
    do i = 1, kernel%num_variants
      if (trim(kernel%variants(i)%name) == trim(variant_name)) then
        kernel%selected_variant = i
        print *, "Forced selection of variant:", variant_name
        return
      end if
    end do
    
    print *, "WARNING: Variant not found:", variant_name
    
  end subroutine force_variant
  
  subroutine cleanup_adaptive_kernel(kernel)
    type(adaptive_kernel), intent(inout) :: kernel
    
    ! Reset kernel state
    kernel%num_variants = 0
    kernel%selected_variant = 0
    
  end subroutine cleanup_adaptive_kernel

end module sparkle_adaptive_kernel