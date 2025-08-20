module sporkle_gpu_timers
  ! GPU-side timing using OpenGL timer queries
  ! This measures actual GPU execution time, not CPU overhead
  
  use kinds
  use iso_c_binding
  implicit none
  
  private
  public :: gpu_timer_init, gpu_timer_start, gpu_timer_end
  public :: gpu_timer_get_result, gpu_timer_shutdown
  
  ! OpenGL constants for timer queries
  integer(c_int), parameter :: GL_TIMESTAMP = int(z'8E28', c_int)
  integer(c_int), parameter :: GL_TIME_ELAPSED = int(z'88BF', c_int)
  integer(c_int), parameter :: GL_QUERY_RESULT = int(z'8866', c_int)
  integer(c_int), parameter :: GL_QUERY_RESULT_AVAILABLE = int(z'8867', c_int)
  
  ! Timer state
  integer :: timer_query = 0
  logical :: timers_initialized = .false.
  
  ! OpenGL function interfaces
  interface
    subroutine glGenQueries(n, ids) bind(C, name='glGenQueries')
      import :: c_int, c_ptr
      integer(c_int), value :: n
      type(c_ptr), value :: ids
    end subroutine
    
    subroutine glBeginQuery(target, id) bind(C, name='glBeginQuery')
      import :: c_int
      integer(c_int), value :: target, id
    end subroutine
    
    subroutine glEndQuery(target) bind(C, name='glEndQuery')
      import :: c_int
      integer(c_int), value :: target
    end subroutine
    
    subroutine glGetQueryObjectiv(id, pname, params) bind(C, name='glGetQueryObjectiv')
      import :: c_int, c_ptr
      integer(c_int), value :: id, pname
      type(c_ptr), value :: params
    end subroutine
    
    subroutine glGetQueryObjectui64v(id, pname, params) bind(C, name='glGetQueryObjectui64v')
      import :: c_int, c_ptr
      integer(c_int), value :: id, pname
      type(c_ptr), value :: params
    end subroutine
    
    subroutine glDeleteQueries(n, ids) bind(C, name='glDeleteQueries')
      import :: c_int, c_ptr
      integer(c_int), value :: n
      type(c_ptr), value :: ids
    end subroutine
  end interface
  
contains

  function gpu_timer_init() result(success)
    logical :: success
    integer(c_int), target :: timer_id
    
    success = .false.
    
    if (timers_initialized) then
      success = .true.
      return
    end if
    
    print *, "=== GPU Timer Queries Initialization ==="
    print *, "Measuring actual GPU execution time (not CPU overhead)"
    
    ! Generate timer query object
    call glGenQueries(1, c_loc(timer_id))
    timer_query = timer_id
    
    if (timer_query == 0) then
      print *, "ERROR: Failed to create GPU timer query"
      return
    end if
    
    timers_initialized = .true.
    success = .true.
    
    print *, "✓ GPU timer queries ready"
    print *, "  Using GL_TIME_ELAPSED for precise GPU timing"
    print *, ""
    
  end function gpu_timer_init
  
  subroutine gpu_timer_start()
    if (.not. timers_initialized) then
      print *, "ERROR: GPU timers not initialized"
      return
    end if
    
    ! Start timing GPU execution
    call glBeginQuery(GL_TIME_ELAPSED, timer_query)
    
  end subroutine gpu_timer_start
  
  subroutine gpu_timer_end()
    if (.not. timers_initialized) then
      print *, "ERROR: GPU timers not initialized"
      return
    end if
    
    ! End timing GPU execution
    call glEndQuery(GL_TIME_ELAPSED)
    
  end subroutine gpu_timer_end
  
  function gpu_timer_get_result(wait_for_result) result(time_ns)
    logical, intent(in), optional :: wait_for_result
    integer(i64) :: time_ns
    
    integer(c_int), target :: available
    integer(c_int64_t), target :: elapsed_ns
    logical :: should_wait
    
    time_ns = -1
    
    if (.not. timers_initialized) then
      print *, "ERROR: GPU timers not initialized"
      return
    end if
    
    should_wait = .true.
    if (present(wait_for_result)) should_wait = wait_for_result
    
    if (should_wait) then
      ! Blocking wait for result
      call glGetQueryObjectui64v(timer_query, GL_QUERY_RESULT, c_loc(elapsed_ns))
      time_ns = elapsed_ns
    else
      ! Non-blocking check
      call glGetQueryObjectiv(timer_query, GL_QUERY_RESULT_AVAILABLE, c_loc(available))
      if (available /= 0) then
        call glGetQueryObjectui64v(timer_query, GL_QUERY_RESULT, c_loc(elapsed_ns))
        time_ns = elapsed_ns
      end if
    end if
    
  end function gpu_timer_get_result
  
  function gpu_timer_get_result_ms(wait_for_result) result(time_ms)
    logical, intent(in), optional :: wait_for_result
    real(dp) :: time_ms
    
    integer(i64) :: time_ns
    logical :: should_wait
    
    should_wait = .true.
    if (present(wait_for_result)) should_wait = wait_for_result
    
    time_ns = gpu_timer_get_result(should_wait)
    
    if (time_ns >= 0) then
      time_ms = real(time_ns, dp) / 1.0e6_dp  ! Convert nanoseconds to milliseconds
    else
      time_ms = -1.0_dp
    end if
    
  end function gpu_timer_get_result_ms
  
  subroutine gpu_timer_shutdown()
    integer(c_int), target :: timer_id
    
    if (timers_initialized) then
      if (timer_query /= 0) then
        timer_id = timer_query
        call glDeleteQueries(1, c_loc(timer_id))
        timer_query = 0
      end if
      timers_initialized = .false.
      print *, "✓ GPU timers shut down"
    end if
  end subroutine gpu_timer_shutdown

end module sporkle_gpu_timers