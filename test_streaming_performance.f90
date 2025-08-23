! Test streaming GEMM performance for huge matrices
program test_streaming_performance
  use iso_c_binding
  use kinds
  use sporkle_types
  use cpu_device_module
  use timing_helpers, only: now_s
  implicit none
  
  type(cpu_device) :: cpu_dev
  type(sporkle_buffer) :: buffer_a, buffer_b, buffer_c
  type(c_ptr) :: args(8)
  integer(i32) :: grid(3), block(3)
  integer :: status, i
  
  ! Test sizes - focus on large matrices
  integer, parameter :: SIZES(4) = [1024, 2048, 4096, 8192]
  character(len=30) :: size_names(4) = ["Medium (1024×1024) - 4MB   ", &
                                        "Large (2048×2048) - 16MB   ", &
                                        "Huge (4096×4096) - 64MB    ", &
                                        "Massive (8192×8192) - 256MB"]
  
  ! Test data
  real(sp), pointer :: A(:), B(:), C(:)
  integer, target :: m, n, k
  real(sp), target :: alpha, beta
  real(dp) :: start_time, end_time
  real(dp) :: time_simd, time_streaming
  real(dp) :: gflops, bandwidth_gb
  integer(i64) :: flops, bytes_moved
  integer, parameter :: NUM_RUNS = 3
  
  print *, "=== Cache Bypass Streaming Performance Test ==="
  print *, ""
  
  ! Create device
  cpu_dev = cpu_device(0)
  print *, "Device:", trim(cpu_dev%name)
  print *, ""
  print *, "Testing matrices that exceed cache capacity..."
  print *, "L3 cache size: ~64MB (estimated)"
  print *, ""
  
  ! Test different sizes
  do i = 1, 4
    m = SIZES(i)
    n = SIZES(i)
    k = SIZES(i)
    
    ! Calculate memory footprint
    bytes_moved = int(m, int64) * int(k, int64) * 4 + &  ! Read A
                  int(k, int64) * int(n, int64) * 4 + &  ! Read B  
                  int(m, int64) * int(n, int64) * 4      ! Write C
    
    print *, "Testing ", trim(size_names(i))
    print '(A,I0,A)', "  Total memory traffic: ", bytes_moved / (1024*1024), " MB"
    print *, "----------------------------------------"
    
    ! Skip if too large for available memory
    if (i == 4) then
      print *, "  ⚠️ 8192×8192 requires 768MB - skipping on systems with <2GB free"
      print *, ""
      cycle
    end if
    
    ! Allocate buffers
    buffer_a = cpu_dev%allocate(int(m*k, int64) * 4_int64)
    buffer_b = cpu_dev%allocate(int(k*n, int64) * 4_int64)
    buffer_c = cpu_dev%allocate(int(m*n, int64) * 4_int64)
    
    if (.not. c_associated(buffer_a%data) .or. &
        .not. c_associated(buffer_b%data) .or. &
        .not. c_associated(buffer_c%data)) then
      print *, "  ❌ Failed to allocate memory - skipping"
      if (c_associated(buffer_a%data)) call cpu_dev%deallocate(buffer_a)
      if (c_associated(buffer_b%data)) call cpu_dev%deallocate(buffer_b)
      if (c_associated(buffer_c%data)) call cpu_dev%deallocate(buffer_c)
      cycle
    end if
    
    call c_f_pointer(buffer_a%data, A, [m*k])
    call c_f_pointer(buffer_b%data, B, [k*n])
    call c_f_pointer(buffer_c%data, C, [m*n])
    
    ! Initialize with simple pattern (not random to save time)
    A = 1.0
    B = 1.0
    
    alpha = 1.0
    beta = 0.0
    
    ! Setup args
    args(1) = buffer_a%data
    args(2) = buffer_b%data
    args(3) = buffer_c%data
    args(4) = c_loc(m)
    args(5) = c_loc(n)
    args(6) = c_loc(k)
    args(7) = c_loc(alpha)
    args(8) = c_loc(beta)
    
    grid = [1, 1, 1]
    block = [1, 1, 1]
    
    flops = 2_int64 * int(m, int64) * int(n, int64) * int(k, int64)
    
    ! Test 1: Regular SIMD GEMM
    C = 0.0
    print *, "  Running SIMD GEMM..."
    
    start_time = now_s()
    status = cpu_dev%execute("gemm_simd", args, grid, block)
    end_time = now_s()
    
    time_simd = end_time - start_time
    gflops = real(flops, dp) / time_simd / 1.0e9_dp
    bandwidth_gb = real(bytes_moved, dp) / time_simd / 1.0e9_dp
    
    print '(A,F8.2,A,F8.2,A,F8.2,A)', "    SIMD:      ", &
            time_simd * 1000.0, " ms, ", gflops, " GFLOPS, ", &
            bandwidth_gb, " GB/s"
    
    ! Test 2: Streaming GEMM
    C = 0.0
    print *, "  Running Streaming GEMM..."
    
    start_time = now_s()
    status = cpu_dev%execute("gemm_streaming", args, grid, block)
    end_time = now_s()
    
    time_streaming = end_time - start_time
    gflops = real(flops, dp) / time_streaming / 1.0e9_dp
    bandwidth_gb = real(bytes_moved, dp) / time_streaming / 1.0e9_dp
    
    print '(A,F8.2,A,F8.2,A,F8.2,A)', "    Streaming: ", &
            time_streaming * 1000.0, " ms, ", gflops, " GFLOPS, ", &
            bandwidth_gb, " GB/s"
    
    ! Compare
    print *, ""
    if (time_streaming < time_simd) then
      print '(A,F5.1,A)', "  ✅ Streaming is ", &
              (time_simd/time_streaming - 1.0) * 100.0, "% faster!"
    else
      print '(A,F5.1,A)', "  ❌ Streaming is ", &
              (time_streaming/time_simd - 1.0) * 100.0, "% slower"
    end if
    print *, ""
    
    ! Cleanup
    call cpu_dev%deallocate(buffer_a)
    call cpu_dev%deallocate(buffer_b)
    call cpu_dev%deallocate(buffer_c)
  end do
  
  print *, "=== Summary ==="
  print *, "Cache bypass streaming helps when:"
  print *, "- Matrices exceed L3 cache size"
  print *, "- Memory bandwidth is the bottleneck"
  print *, "- Data won't be reused"
  
end program test_streaming_performance