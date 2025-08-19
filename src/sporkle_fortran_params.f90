module sporkle_fortran_params
  ! Adaptive parameter passing for Fortran GPU kernels
  ! Philosophy: Try multiple approaches, measure, adapt!
  
  use iso_c_binding
  use sporkle_types
  use gl_constants
  implicit none
  
  private
  public :: param_method, PARAMS_UNIFORM, PARAMS_BUFFER, PARAMS_INLINE
  public :: param_strategy, create_param_strategy, destroy_param_strategy
  public :: PARAM_BUFFER_BINDING
  
  ! Parameter passing methods
  integer, parameter :: PARAMS_UNIFORM = 1  ! GL uniform variables
  integer, parameter :: PARAMS_BUFFER = 2   ! Dedicated buffer
  integer, parameter :: PARAMS_INLINE = 3   ! Inline in shader code
  
  ! Configurable binding slot for parameter buffer
  ! Can be changed based on application needs to avoid conflicts
  integer, parameter :: PARAM_BUFFER_BINDING = 15
  
  type :: param_method
    integer :: method
    real :: setup_time_ms
    real :: dispatch_time_ms
    logical :: is_supported
  end type
  
  type :: param_strategy
    type(param_method) :: methods(3)
    integer :: preferred_method
    logical :: benchmarked
  end type
  
contains

  function create_param_strategy() result(strategy)
    type(param_strategy) :: strategy
    
    ! Initialize all methods as potentially supported
    strategy%methods(PARAMS_UNIFORM)%method = PARAMS_UNIFORM
    strategy%methods(PARAMS_UNIFORM)%is_supported = .true.
    
    strategy%methods(PARAMS_BUFFER)%method = PARAMS_BUFFER  
    strategy%methods(PARAMS_BUFFER)%is_supported = .true.
    
    strategy%methods(PARAMS_INLINE)%method = PARAMS_INLINE
    strategy%methods(PARAMS_INLINE)%is_supported = .true.
    
    strategy%benchmarked = .false.
    strategy%preferred_method = PARAMS_BUFFER  ! Safe default
    
  end function create_param_strategy
  
  ! NOTE: Benchmarking moved to sporkle_gpu_benchmark module
  ! This module only defines parameter passing strategies and types
  ! Use gpu_benchmark_param_methods() for actual performance testing
  
  ! Implementation details for each parameter passing method moved to sporkle_gpu_benchmark
  ! This module focuses on strategy types and configuration only
  
  subroutine destroy_param_strategy(strategy)
    type(param_strategy), intent(inout) :: strategy
    ! Nothing to deallocate for now
    strategy%benchmarked = .false.
  end subroutine destroy_param_strategy

end module sporkle_fortran_params