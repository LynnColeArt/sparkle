program test_pm4_wiring
  ! Test the PM4 GPU dispatch wiring
  use iso_c_binding
  use kinds
  use sporkle_gpu_dispatch_pm4
  implicit none
  
  type(gpu_device_pm4) :: device
  type(gpu_memory_pm4) :: buffer_a, buffer_b, buffer_c
  real(sp), pointer :: a_data(:), b_data(:), c_data(:)
  integer :: n, i, status
  
  print *, "=== Testing PM4 GPU Dispatch Wiring ==="
  print *, ""
  
  ! Initialize GPU
  device = init_gpu_pm4()
  if (.not. device%initialized) then
    print *, "❌ Failed to initialize GPU"
    stop 1
  end if
  
  ! Test parameters
  n = 1024
  
  ! Allocate GPU buffers
  print *, "Allocating GPU buffers..."
  buffer_a = gpu_malloc_pm4(int(n * 4, i64))
  buffer_b = gpu_malloc_pm4(int(n * 4, i64))
  buffer_c = gpu_malloc_pm4(int(n * 4, i64))
  
  if (.not. buffer_a%allocated .or. &
      .not. buffer_b%allocated .or. &
      .not. buffer_c%allocated) then
    print *, "❌ Failed to allocate buffers"
    call cleanup_gpu_pm4()
    stop 1
  end if
  
  ! Get CPU pointers
  call c_f_pointer(buffer_a%buffer%cpu_ptr, a_data, [n])
  call c_f_pointer(buffer_b%buffer%cpu_ptr, b_data, [n])
  call c_f_pointer(buffer_c%buffer%cpu_ptr, c_data, [n])
  
  ! Initialize data
  print *, "Initializing data..."
  do i = 1, n
    a_data(i) = real(i, sp)
    b_data(i) = real(i * 2, sp)
    c_data(i) = 0.0_sp
  end do
  
  ! Test vector add shader
  print *, "Executing vector_add shader..."
  block
    type(gpu_memory_pm4) :: buffers(3)
    buffers(1) = buffer_a
    buffers(2) = buffer_b
    buffers(3) = buffer_c
    
    status = gpu_execute_compute_pm4("vector_add", buffers, 3, &
                                    n/64, 1, 1)  ! 64 threads per workgroup
  end block
  
  if (status /= 0) then
    print *, "❌ Shader execution failed"
  else
    print *, "✅ Shader execution completed"
    
    ! Check first few results
    print *, ""
    print *, "First 10 results:"
    do i = 1, min(10, n)
      print '(A,I4,A,F8.2,A,F8.2,A,F8.2)', &
            "  c[", i, "] = ", c_data(i), " (expected ", a_data(i) + b_data(i), ")"
    end do
  end if
  
  ! Cleanup
  print *, ""
  print *, "Cleaning up..."
  call gpu_free_pm4(buffer_a)
  call gpu_free_pm4(buffer_b)
  call gpu_free_pm4(buffer_c)
  call cleanup_gpu_pm4()
  
  print *, "✅ Test complete"
  
end program test_pm4_wiring