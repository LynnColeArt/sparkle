module sparkle_config
  ! Configuration module for Sparkle runtime settings
  ! The Sparkle Way: Sensible defaults, but let users tune if they want
  
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_int
  implicit none
  private
  
  public :: sparkle_get_config, sparkle_set_config
  public :: sparkle_config_type
  public :: SPARKLE_MAX_CPU_THREADS, SPARKLE_THREAD_RESERVE
  
  ! Configuration type
  type :: sparkle_config_type
    integer :: max_cpu_threads = 0     ! 0 = auto-detect
    integer :: thread_reserve = 2      ! Reserve N threads for system
    logical :: enable_gpu = .true.     ! Use GPU if available
    logical :: enable_network = .true. ! Use network devices
    logical :: verbose = .false.       ! Verbose output
    integer :: cache_l1_kb = 32       ! L1 cache size hint
    integer :: cache_l2_kb = 256      ! L2 cache size hint
    integer :: cache_l3_mb = 8        ! L3 cache size hint
  end type sparkle_config_type
  
  ! Global configuration (module variable)
  type(sparkle_config_type), save :: global_config
  
  ! Environment variable names
  character(len=*), parameter :: SPARKLE_MAX_CPU_THREADS = "SPARKLE_MAX_CPU_THREADS"
  character(len=*), parameter :: SPARKLE_THREAD_RESERVE = "SPARKLE_THREAD_RESERVE"
  
contains

  ! Get current configuration
  function sparkle_get_config() result(config)
    type(sparkle_config_type) :: config
    character(len=256) :: env_value
    integer :: status, value
    
    config = global_config
    
    ! Check environment variables
    call get_environment_variable(SPARKLE_MAX_CPU_THREADS, env_value, status=status)
    if (status == 0) then
      read(env_value, *, iostat=status) value
      if (status == 0 .and. value >= 0) then
        config%max_cpu_threads = value
      end if
    end if
    
    call get_environment_variable(SPARKLE_THREAD_RESERVE, env_value, status=status)
    if (status == 0) then
      read(env_value, *, iostat=status) value
      if (status == 0 .and. value >= 0) then
        config%thread_reserve = value
      end if
    end if
    
  end function sparkle_get_config
  
  ! Set configuration
  subroutine sparkle_set_config(config)
    type(sparkle_config_type), intent(in) :: config
    global_config = config
  end subroutine sparkle_set_config
  
  ! Get safe number of threads for OpenMP
  function get_safe_thread_count() result(num_threads)
    integer :: num_threads
    integer :: total_threads
    type(sparkle_config_type) :: config
    
    interface
      function omp_get_max_threads() bind(C, name="omp_get_max_threads")
        import :: c_int
        integer(c_int) :: omp_get_max_threads
      end function omp_get_max_threads
    end interface
    
    config = sparkle_get_config()
    
    ! Get total available threads
    total_threads = int(omp_get_max_threads())
    
    if (config%max_cpu_threads > 0) then
      ! User specified max threads
      num_threads = min(config%max_cpu_threads, total_threads)
    else
      ! Auto-detect: use all threads minus reserve
      num_threads = max(1, total_threads - config%thread_reserve)
    end if
    
    ! Sanity check
    if (num_threads < 1) num_threads = 1
    if (num_threads > total_threads) num_threads = total_threads
    
  end function get_safe_thread_count

end module sparkle_config