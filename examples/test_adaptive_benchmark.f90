program test_adaptive_benchmark
  ! ========================================================================
  ! Test program for adaptive parameter passing benchmarking
  ! 
  ! This program creates a simple compute shader and benchmarks three
  ! different methods of passing scalar parameters to the GPU:
  ! 1. UNIFORM - Traditional GL uniforms
  ! 2. BUFFER - Packed into a single SSBO
  ! 3. INLINE - Compiled as constants into the shader
  !
  ! Note: This is still using mock implementations for actual GPU execution
  ! ========================================================================
  
  use kinds
  use sporkle_fortran_params
  use sporkle_shader_parser_v2
  implicit none
  
  type(param_strategy) :: strategy
  type(shader_kernel_v2) :: kernel
  character(len=:), allocatable :: glsl_source
  character(len=1024) :: simple_kernel_src
  integer :: i
  
  print *, "========================================================"
  print *, "Adaptive Parameter Passing Benchmark"
  print *, "========================================================"
  print *, ""
  
  ! Define a simple kernel source (this would normally come from parsing)
  simple_kernel_src = &
    "void main() {" // NEW_LINE('A') // &
    "  uint idx = gl_GlobalInvocationID.x;" // NEW_LINE('A') // &
    "  // Simulate work using parameters" // NEW_LINE('A') // &
    "}"
  
  ! Create parameter strategy
  strategy = create_param_strategy()
  
  ! Benchmark all three methods
  print *, "Running benchmarks..."
  print *, "Note: GPU execution is currently mocked"
  print *, ""
  
  call benchmark_param_methods(strategy, simple_kernel_src)
  
  ! Display results
  print *, "========================================================"
  print *, "Benchmark Results:"
  print *, "========================================================"
  print *, ""
  
  do i = 1, 3
    select case(i)
    case(PARAMS_UNIFORM)
      print *, "UNIFORM Method:"
    case(PARAMS_BUFFER) 
      print *, "BUFFER Method:"
    case(PARAMS_INLINE)
      print *, "INLINE Method:"
    end select
    
    if (strategy%methods(i)%is_supported) then
      print '(A,F8.3,A)', "  Setup time:    ", strategy%methods(i)%setup_time_ms, " ms"
      print '(A,F8.3,A)', "  Dispatch time: ", strategy%methods(i)%dispatch_time_ms, " ms"
      print '(A,F8.3,A)', "  Total time:    ", &
        strategy%methods(i)%setup_time_ms + strategy%methods(i)%dispatch_time_ms, " ms"
    else
      print *, "  Not supported"
    end if
    print *, ""
  end do
  
  print *, "========================================================"
  print *, "Recommended method: ", get_method_name(strategy%preferred_method)
  print *, "========================================================"
  
  ! Test with actual kernel parsing
  print *, ""
  print *, "Testing with parsed Fortran kernel..."
  kernel = parse_fortran_kernel_v2("examples/kernels_adaptive.f90", "scaled_add", strategy%preferred_method)
  glsl_source = generate_glsl_v2(kernel)
  
  print *, "Generated GLSL for ", trim(get_method_name(strategy%preferred_method)), " method:"
  print *, "------------------------------------------------------------"
  print *, trim(glsl_source)
  print *, "------------------------------------------------------------"
  
  ! Cleanup
  call destroy_param_strategy(strategy)
  
contains

  function get_method_name(method) result(name)
    integer, intent(in) :: method
    character(len=32) :: name
    
    select case(method)
    case(PARAMS_UNIFORM)
      name = "UNIFORM"
    case(PARAMS_BUFFER)
      name = "BUFFER"
    case(PARAMS_INLINE)
      name = "INLINE"
    case default
      name = "UNKNOWN"
    end select
  end function get_method_name

end program test_adaptive_benchmark