program test_va_mapping_debug
  ! Debug VA mapping issues with AMDGPU
  ! ===================================
  
  use sporkle_amdgpu_direct
  use kinds
  use iso_c_binding
  implicit none
  
  type(amdgpu_device) :: device
  type(amdgpu_buffer) :: buffer
  integer :: status, ctx_id
  integer(i64) :: test_va_addr
  integer :: i
  
  ! Test addresses array
  integer(i64) :: test_addresses(6)
  
  print *, "🔍 VA Mapping Debug Test"
  print *, "========================"
  print *, ""
  
  ! Initialize test addresses
  test_addresses(1) = int(z'100000', int64)      ! 1MB
  test_addresses(2) = int(z'1000000', int64)     ! 16MB  
  test_addresses(3) = int(z'10000000', int64)    ! 256MB
  test_addresses(4) = int(z'40000000', int64)    ! 1GB
  test_addresses(5) = int(z'100000000', int64)   ! 4GB
  test_addresses(6) = int(z'200000000', int64)   ! 8GB
  
  ! Open device
  device = amdgpu_open_device("/dev/dri/renderD128")
  if (device%fd <= 0) then
    print *, "❌ Cannot open device"
    stop 1
  end if
  
  ! Create context
  ctx_id = amdgpu_create_context(device)
  if (ctx_id < 0) then
    print *, "❌ Cannot create context"
    stop 1
  end if
  
  ! Allocate a small buffer
  buffer = amdgpu_allocate_buffer(device, 4096_int64, 2)  ! GTT domain = 2
  if (buffer%handle == 0) then
    print *, "❌ Cannot allocate buffer"
    stop 1
  end if
  
  ! Try different VA addresses to find valid range
  print *, "🗺️ Testing different VA address ranges..."
  
  status = -1
  do i = 1, size(test_addresses)
    test_va_addr = test_addresses(i)
    print '(A,I0,A,Z16,A)', "Test ", i, ": VA address 0x", test_va_addr, " ..."
    
    status = amdgpu_map_va(device, buffer, test_va_addr)
    if (status == 0) then
      print *, "   ✅ SUCCESS! Valid VA address found"
      call amdgpu_unmap_va(device, buffer)
      exit
    else
      print *, "   ❌ Failed"
    end if
  end do
  
  ! If we found a working address, test with different sizes
  if (status == 0) then
    print *, ""
    print '(A,Z16)', "🎯 Found working VA base: 0x", test_va_addr
    print *, "   Testing alignment requirements..."
    
    ! Test alignment
    block
      integer(i64) :: aligned_addr
      integer :: alignment
      
      do alignment = 4096, 65536, 4096  ! Test 4KB to 64KB alignment
        aligned_addr = ((test_va_addr + alignment - 1) / alignment) * alignment
        
        status = amdgpu_map_va(device, buffer, aligned_addr)
        if (status == 0) then
          print '(A,I0,A)', "   ✅ ", alignment, " byte alignment works"
          call amdgpu_unmap_va(device, buffer)
          exit
        else
          print '(A,I0,A)', "   ❌ ", alignment, " byte alignment failed"
        end if
      end do
    end block
  end if
  
  ! Cleanup
  call amdgpu_destroy_context(device, ctx_id)
  call amdgpu_close_device(device)
  
  if (status == 0) then
    print *, ""
    print *, "🎉 VA mapping working! Use this configuration:"
    print '(A,Z16)', "   Base address: 0x", test_va_addr
  else
    print *, ""
    print *, "❌ Could not find working VA mapping"
    print *, "   This might require more driver investigation"
  end if

end program test_va_mapping_debug