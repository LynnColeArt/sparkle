program test_program_cache
  use kinds
  use gpu_program_cache
  use gpu_opengl_interface
  implicit none
  
  type(program_cache) :: cache
  integer :: prog1, prog2, prog3, prog4
  integer :: i, test_prog
  character(len=64) :: cache_key
  logical :: all_tests_passed
  
  print *, "🧪 Testing GPU Program Cache"
  print *, "==========================="
  print *, ""
  
  all_tests_passed = .true.
  
  ! Initialize GPU first
  if (.not. gpu_init()) then
    print *, "❌ Failed to initialize GPU!"
    stop 1
  end if
  
  ! Test 1: Initialize cache
  print *, "Test 1: Initialize cache"
  call init_program_cache(cache, max_programs=3, cache_directory="test_cache/")
  if (cache%initialized) then
    print *, "   ✅ Cache initialized successfully"
  else
    print *, "   ❌ Cache initialization failed"
    all_tests_passed = .false.
  end if
  print *, ""
  
  ! Test 2: Cache miss and compilation
  print *, "Test 2: Cache miss triggers compilation"
  prog1 = get_cached_program(cache, get_test_shader(1), "test_shader_1", compile_test_shader)
  if (prog1 > 0 .and. cache%cache_misses == 1) then
    print *, "   ✅ Program compiled and cached"
  else
    print *, "   ❌ Compilation failed"
    all_tests_passed = .false.
  end if
  print *, ""
  
  ! Test 3: Cache hit
  print *, "Test 3: Cache hit avoids recompilation"
  prog2 = get_cached_program(cache, get_test_shader(1), "test_shader_1", compile_test_shader)
  if (prog2 == prog1 .and. cache%cache_hits == 1) then
    print *, "   ✅ Cache hit successful"
  else
    print *, "   ❌ Cache hit failed"
    all_tests_passed = .false.
  end if
  print *, ""
  
  ! Test 4: Reference counting
  print *, "Test 4: Reference counting"
  call release_program(cache, prog1)
  if (cache%entries(1)%ref_count == 1) then
    print *, "   ✅ Reference count correct (1 remaining)"
  else
    print *, "   ❌ Reference count incorrect"
    all_tests_passed = .false.
  end if
  print *, ""
  
  ! Test 5: Multiple programs
  print *, "Test 5: Multiple programs in cache"
  prog2 = get_cached_program(cache, get_test_shader(2), "test_shader_2", compile_test_shader)
  prog3 = get_cached_program(cache, get_test_shader(3), "test_shader_3", compile_test_shader)
  if (cache%num_entries == 3) then
    print *, "   ✅ All programs cached"
  else
    print *, "   ❌ Cache count incorrect"
    all_tests_passed = .false.
  end if
  print *, ""
  
  ! Test 6: LRU eviction
  print *, "Test 6: LRU eviction when cache full"
  ! Release all references to shader 1 (make it evictable)
  call release_program(cache, prog1)
  
  ! Add a 4th program to trigger eviction
  prog4 = get_cached_program(cache, get_test_shader(4), "test_shader_4", compile_test_shader)
  
  ! Check that shader 1 was evicted
  if (find_program(cache, "test_shader_1") == 0 .and. cache%num_entries == 3) then
    print *, "   ✅ LRU eviction worked"
  else
    print *, "   ❌ LRU eviction failed"
    all_tests_passed = .false.
  end if
  print *, ""
  
  ! Test 7: Performance test
  print *, "Test 7: Performance comparison"
  print *, "   Compiling 10 unique shaders..."
  
  ! Clear cache for fair comparison
  call cleanup_program_cache(cache)
  call init_program_cache(cache, max_programs=20)
  
  ! Time compilation of 10 shaders
  block
    integer(i64) :: start_time, end_time, compile_time, lookup_time
    real(dp) :: clock_rate
    
    call system_clock(start_time, count_rate=clock_rate)
    do i = 1, 10
      write(cache_key, '(A,I0)') "perf_shader_", i
      test_prog = get_cached_program(cache, get_test_shader(i), cache_key, compile_test_shader)
    end do
    call system_clock(end_time)
    compile_time = (end_time - start_time)
    
    print '(A,F8.2,A)', "   Initial compilation: ", &
          real(compile_time) / clock_rate * 1000.0, " ms"
    
    ! Now access them again (should be cached)
    call system_clock(start_time)
    do i = 1, 10
      write(cache_key, '(A,I0)') "perf_shader_", i
      test_prog = get_cached_program(cache, get_test_shader(i), cache_key, compile_test_shader)
    end do
    call system_clock(end_time)
    lookup_time = (end_time - start_time)
    
    print '(A,F8.2,A)', "   Cached access: ", &
          real(lookup_time) / clock_rate * 1000.0, " ms"
    
    print '(A,F6.1,A)', "   Speedup: ", &
          real(compile_time) / real(max(lookup_time, 1_int64)), "x"
    
    if (lookup_time < compile_time / 10) then
      print *, "   ✅ Cache provides significant speedup"
    else
      print *, "   ⚠️  Cache speedup less than expected"
    end if
  end block
  print *, ""
  
  ! Print final statistics
  call print_cache_stats(cache)
  
  ! Cleanup
  call cleanup_program_cache(cache)
  call gpu_cleanup()
  
  ! Summary
  if (all_tests_passed) then
    print *, "🎉 All tests PASSED!"
  else
    print *, "❌ Some tests FAILED!"
  end if
  
contains

  ! Get test shader source
  function get_test_shader(variant) result(source)
    integer, intent(in) :: variant
    character(len=:), allocatable :: source
    
    ! Simple compute shader that varies slightly
    source = "#version 430" // new_line('A') // &
             "layout(local_size_x = " // trim(str(variant * 64)) // ") in;" // new_line('A') // &
             "layout(std430, binding = 0) buffer Data { float data[]; };" // new_line('A') // &
             "void main() {" // new_line('A') // &
             "  uint idx = gl_GlobalInvocationID.x;" // new_line('A') // &
             "  data[idx] = data[idx] * " // trim(str(real(variant))) // ";" // new_line('A') // &
             "}"
  end function get_test_shader
  
  ! Compile test shader (wrapper for GPU interface)
  function compile_test_shader(source) result(prog_id)
    character(len=*), intent(in) :: source
    integer :: prog_id
    
    ! This would normally call the actual GPU compilation
    ! For testing, we'll use a simplified version
    block
      integer :: shader_id
      integer(i32) :: status
      
      ! In real implementation, this would call:
      ! prog_id = gpu_compile_compute_shader(source)
      
      ! For now, simulate compilation delay
      call sleep_ms(50)  ! 50ms compile time
      
      ! Return a fake but valid program ID
      prog_id = 1000 + len(source)  ! Unique based on source
    end block
  end function compile_test_shader
  
  ! Convert integer to string
  function str(val) result(string)
    class(*), intent(in) :: val
    character(len=32) :: string
    
    select type(val)
    type is (integer)
      write(string, '(I0)') val
    type is (real)
      write(string, '(F0.1)') val
    end select
    string = adjustl(string)
  end function str
  
  ! Sleep for milliseconds
  subroutine sleep_ms(ms)
    integer, intent(in) :: ms
    integer(i64) :: start_time, current_time
    real(dp) :: clock_rate
    
    call system_clock(start_time, count_rate=clock_rate)
    do
      call system_clock(current_time)
      if ((current_time - start_time) / clock_rate * 1000.0 >= ms) exit
    end do
  end subroutine sleep_ms

end program test_program_cache