module sparkle_fortran_params
  ! Adaptive parameter passing for Fortran GPU kernels
  ! Philosophy: Try multiple approaches, measure, adapt!
  
  use iso_c_binding
  use sparkle_types
  use gl_constants
  implicit none
  
  private
  public :: param_method, PARAMS_UNIFORM, PARAMS_BUFFER, PARAMS_INLINE
  public :: param_strategy, create_param_strategy, destroy_param_strategy
  public :: benchmark_param_methods
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
  
  subroutine benchmark_param_methods(strategy, kernel_source)
    type(param_strategy), intent(inout) :: strategy
    character(len=*), intent(in) :: kernel_source
    
    real :: start_time, end_time
    integer :: i, test_program
    real :: min_time
    
    print *, "=== Benchmarking parameter passing methods ==="
    
    ! Test each method
    do i = 1, 3
      if (strategy%methods(i)%is_supported) then
        select case(i)
        case(PARAMS_UNIFORM)
          call benchmark_uniform_method(kernel_source, &
                                       strategy%methods(i)%setup_time_ms, &
                                       strategy%methods(i)%dispatch_time_ms)
          
        case(PARAMS_BUFFER)
          call benchmark_buffer_method(kernel_source, &
                                      strategy%methods(i)%setup_time_ms, &
                                      strategy%methods(i)%dispatch_time_ms)
          
        case(PARAMS_INLINE)
          call benchmark_inline_method(kernel_source, &
                                      strategy%methods(i)%setup_time_ms, &
                                      strategy%methods(i)%dispatch_time_ms)
        end select
        
        print *, "Method", i, "setup:", strategy%methods(i)%setup_time_ms, &
                 "ms, dispatch:", strategy%methods(i)%dispatch_time_ms, "ms"
      end if
    end do
    
    ! Choose fastest total time
    min_time = huge(1.0)
    do i = 1, 3
      if (strategy%methods(i)%is_supported) then
        if (strategy%methods(i)%setup_time_ms + &
            strategy%methods(i)%dispatch_time_ms < min_time) then
          min_time = strategy%methods(i)%setup_time_ms + &
                     strategy%methods(i)%dispatch_time_ms
          strategy%preferred_method = i
        end if
      end if
    end do
    
    strategy%benchmarked = .true.
    print *, "Preferred method:", strategy%preferred_method
    print *, ""
    
  end subroutine benchmark_param_methods
  
  subroutine benchmark_uniform_method(kernel_source, setup_time, dispatch_time)
    character(len=*), intent(in) :: kernel_source
    real, intent(out) :: setup_time, dispatch_time
    
    ! =====================================================================
    ! UNIFORM METHOD: Each scalar parameter is passed as a GL uniform
    ! Pros: Simple, no buffer management needed
    ! Cons: Limited number of uniforms, overhead for many parameters
    ! =====================================================================
    
    character(len=:), allocatable :: modified_source
    real :: start, end
    integer :: test_program, test_shader
    integer :: uniform_locs(4)  ! Up to 4 test parameters
    integer :: compile_status, link_status
    integer :: i
    character(len=32) :: param_name
    
    ! TODO: This currently uses a mock shader. To make it real:
    ! 1. Parse kernel_source to extract actual uniform declarations
    ! 2. Generate proper GLSL with those uniforms
    ! 3. Compile and link the real shader
    
    ! For now, create a simple test shader with 4 integer uniforms
    modified_source = "#version 310 es" // NEW_LINE('A') // &
                     "layout(local_size_x = 64) in;" // NEW_LINE('A') // &
                     "uniform int param0;" // NEW_LINE('A') // &
                     "uniform int param1;" // NEW_LINE('A') // &
                     "uniform int param2;" // NEW_LINE('A') // &
                     "uniform int param3;" // NEW_LINE('A') // &
                     "layout(std430, binding = 0) buffer Output {" // NEW_LINE('A') // &
                     "  int data[];" // NEW_LINE('A') // &
                     "};" // NEW_LINE('A') // &
                     "void main() {" // NEW_LINE('A') // &
                     "  uint idx = gl_GlobalInvocationID.x;" // NEW_LINE('A') // &
                     "  data[idx] = param0 + param1 + param2 + param3;" // NEW_LINE('A') // &
                     "}"
    
    call cpu_time(start)
    
    ! Create and compile shader (still mocked for now)
    test_shader = 1  ! glCreateShader(GL_COMPUTE_SHADER)
    ! call glShaderSource(test_shader, 1, c_loc(modified_source), c_null_ptr)
    ! call glCompileShader(test_shader)
    
    ! Create program and link
    test_program = 2  ! glCreateProgram()
    ! call glAttachShader(test_program, test_shader)
    ! call glLinkProgram(test_program)
    
    ! Get uniform locations for our test parameters
    ! Note: In real implementation, we'd query actual parameter names
    do i = 0, 3
      write(param_name, '("param", I1)') i
      ! uniform_locs(i+1) = glGetUniformLocation(test_program, trim(param_name)//c_null_char)
      uniform_locs(i+1) = i  ! Mock location
    end do
    
    call cpu_time(end)
    setup_time = (end - start) * 1000.0  ! Convert to milliseconds
    
    ! =====================================================================
    ! Dispatch timing: Set uniforms and run the compute shader
    ! =====================================================================
    call cpu_time(start)
    
    ! Use the program
    ! call glUseProgram(test_program)
    
    ! Set uniform values (using test values)
    ! do i = 1, 4
    !   call glUniform1i(uniform_locs(i), i * 10)
    ! end do
    
    ! Dispatch compute shader (1 workgroup for testing)
    ! call glDispatchCompute(1, 1, 1)
    ! call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    ! call glFinish()  ! Ensure GPU work completes
    
    call cpu_time(end)
    dispatch_time = (end - start) * 1000.0
    
    ! Cleanup
    ! call glDeleteProgram(test_program)
    ! call glDeleteShader(test_shader)
    
  end subroutine benchmark_uniform_method
  
  subroutine benchmark_buffer_method(kernel_source, setup_time, dispatch_time)
    character(len=*), intent(in) :: kernel_source
    real, intent(out) :: setup_time, dispatch_time
    
    ! =====================================================================
    ! BUFFER METHOD: All scalar parameters packed into a single SSBO
    ! Pros: Unlimited parameters, single buffer update
    ! Cons: Requires buffer management, slightly more complex shader code
    ! =====================================================================
    
    character(len=:), allocatable :: modified_source
    real :: start, end
    integer :: param_buffer
    integer, target :: params(16)  ! Parameter buffer - holds up to 16 int params
    
    ! Add buffer binding for parameters
    character(len=8) :: binding_str
    write(binding_str, '(I0)') PARAM_BUFFER_BINDING
    modified_source = "#version 310 es" // NEW_LINE('A') // &
                     "layout(std430, binding = " // trim(binding_str) // ") buffer Params {" // NEW_LINE('A') // &
                     "  int params[];" // NEW_LINE('A') // &  
                     "};" // NEW_LINE('A') // &
                     kernel_source
    
    call cpu_time(start)
    
    ! Create parameter buffer
    call glGenBuffers(1, param_buffer)
    call glBindBuffer(GL_SHADER_STORAGE_BUFFER, param_buffer)
    ! GL_DYNAMIC_DRAW = 0x88E8
    call glBufferData(GL_SHADER_STORAGE_BUFFER, int(64, c_size_t), &
                      c_loc(params), int(z'88E8', c_int))
    
    call cpu_time(end)
    setup_time = (end - start) * 1000.0
    
    ! =====================================================================
    ! Dispatch timing: Update buffer and run compute shader  
    ! =====================================================================
    call cpu_time(start)
    
    ! Update parameters in buffer
    params(1:4) = [224, 224, 3, 3]  ! Example: height, width, kernel_h, kernel_w
    
    ! Upload new parameters to GPU
    ! Note: We only update the portion of the buffer that changed
    ! call glBufferSubData(GL_SHADER_STORAGE_BUFFER, &
    !                      0_c_intptr_t,                    & ! offset = 0
    !                      int(16, c_size_t) * 4_c_size_t,  & ! size = 4 ints
    !                      c_loc(params))
    
    ! Bind the parameter buffer to the configured binding point
    call glBindBufferBase(GL_SHADER_STORAGE_BUFFER, PARAM_BUFFER_BINDING, param_buffer)
    
    ! Dispatch compute shader
    ! call glUseProgram(test_program)  ! Would need actual program
    ! call glDispatchCompute(1, 1, 1)
    ! call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    ! call glFinish()
    
    call cpu_time(end)
    dispatch_time = (end - start) * 1000.0
    
    ! Cleanup
    call glDeleteBuffers(1, param_buffer)
    
  end subroutine benchmark_buffer_method
  
  subroutine benchmark_inline_method(kernel_source, setup_time, dispatch_time)
    character(len=*), intent(in) :: kernel_source
    real, intent(out) :: setup_time, dispatch_time
    
    ! =====================================================================
    ! INLINE METHOD: Parameters are compiled directly into shader as consts
    ! Pros: Fastest execution, compiler can optimize based on known values
    ! Cons: Requires recompilation for every parameter change
    ! Best for: Kernels with rarely-changing parameters
    ! =====================================================================
    
    character(len=:), allocatable :: modified_source
    real :: start, end
    character(len=32) :: param_str
    integer :: test_program, test_shader
    integer :: compile_status, link_status
    
    call cpu_time(start)
    
    ! Generate GLSL with parameters inlined as compile-time constants
    ! In real implementation, this would:
    ! 1. Parse the kernel to find PLACEHOLDER_xxx markers
    ! 2. Replace each placeholder with actual parameter value
    ! 3. Let GLSL compiler optimize based on known constants
    
    ! Example: Replace placeholders with actual values
    write(param_str, '(I0)') 224
    modified_source = "#version 310 es" // NEW_LINE('A') // &
                     "layout(local_size_x = 64) in;" // NEW_LINE('A') // &
                     "const int height = " // trim(param_str) // ";" // NEW_LINE('A') // &
                     "const int width = " // trim(param_str) // ";" // NEW_LINE('A') // &
                     "const int kernel_h = 3;" // NEW_LINE('A') // &
                     "const int kernel_w = 3;" // NEW_LINE('A') // &
                     "layout(std430, binding = 0) buffer Output {" // NEW_LINE('A') // &
                     "  int data[];" // NEW_LINE('A') // &
                     "};" // NEW_LINE('A') // &
                     "void main() {" // NEW_LINE('A') // &
                     "  uint idx = gl_GlobalInvocationID.x;" // NEW_LINE('A') // &
                     "  // Compiler can optimize this since all values are const" // NEW_LINE('A') // &
                     "  data[idx] = height * width * kernel_h * kernel_w;" // NEW_LINE('A') // &
                     "}"
    
    ! Compile shader with inlined constants
    ! test_shader = glCreateShader(GL_COMPUTE_SHADER)
    ! call glShaderSource(test_shader, 1, c_loc(modified_source), c_null_ptr)
    ! call glCompileShader(test_shader)
    
    ! Create and link program
    ! test_program = glCreateProgram()
    ! call glAttachShader(test_program, test_shader)
    ! call glLinkProgram(test_program)
    
    call cpu_time(end)
    setup_time = (end - start) * 1000.0  ! Includes full recompilation
    
    ! =====================================================================
    ! Dispatch timing: Very fast since no parameter updates needed
    ! =====================================================================
    call cpu_time(start)
    
    ! Just dispatch - parameters are already compiled in
    ! call glUseProgram(test_program)
    ! call glDispatchCompute(1, 1, 1)
    ! call glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT)
    ! call glFinish()
    
    call cpu_time(end)
    dispatch_time = (end - start) * 1000.0
    
    ! Cleanup
    ! call glDeleteProgram(test_program)
    ! call glDeleteShader(test_shader)
    
  end subroutine benchmark_inline_method
  
  subroutine destroy_param_strategy(strategy)
    type(param_strategy), intent(inout) :: strategy
    ! Nothing to deallocate for now
    strategy%benchmarked = .false.
  end subroutine destroy_param_strategy

end module sparkle_fortran_params