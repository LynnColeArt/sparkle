program test_pm4_multi_submit
  ! PM4 Multi-Submit Test
  ! =====================
  ! Tests submitting same IB with different BO lists
  
  use kinds
  use iso_c_binding
  use pm4_submit
  use pm4_buffer_raii
  implicit none
  
  type(c_ptr) :: ctx_ptr
  type(gpu_buffer) :: output_buffer, signal_buffer, ib_buffer, shader_buffer
  type(sp_fence) :: fence1, fence2, fence3
  
  integer(c_int32_t), pointer :: ib_data(:), output_data(:), signal_data(:), shader_data(:)
  integer(i32) :: status, i, poll_count
  logical :: eop_signaled, shader_wrote
  
  print *, "======================================="
  print *, "PM4 Multi-Submit Test"
  print *, "======================================="
  print *, "Testing multiple submits with different BOs"
  print *, ""
  
  ctx_ptr = sp_pm4_init("/dev/dri/renderD129")
  if (.not. c_associated(ctx_ptr)) stop 1
  
  ! Allocate buffers
  call output_buffer%init(ctx_ptr, 4096_i64, SP_BO_HOST_VISIBLE)
  call signal_buffer%init(ctx_ptr, 256_i64, SP_BO_HOST_VISIBLE)
  call shader_buffer%init(ctx_ptr, 256_i64, SP_BO_HOST_VISIBLE)
  call ib_buffer%init(ctx_ptr, 8192_i64, SP_BO_HOST_VISIBLE)
  
  ! Map buffers
  call c_f_pointer(output_buffer%get_cpu_ptr(), output_data, [1024])
  call c_f_pointer(signal_buffer%get_cpu_ptr(), signal_data, [64])
  call c_f_pointer(shader_buffer%get_cpu_ptr(), shader_data, [64])
  call c_f_pointer(ib_buffer%get_cpu_ptr(), ib_data, [2048])
  
  ! Clear buffers
  output_data = int(z'CAFECAFE', i32)
  signal_data = 0
  
  ! Simple PAL store shader
  shader_data = 0
  shader_data(1) = int(z'7E000200', i32)  ! v_mov_b32 v0, s0
  shader_data(2) = int(z'7E020201', i32)  ! v_mov_b32 v1, s1
  shader_data(3) = int(z'7E0402FF', i32)  ! v_mov_b32 v2, 0xDEADBEEF
  shader_data(4) = int(z'DEADBEEF', i32)  ! literal
  shader_data(5) = int(z'DC700000', i32)  ! global_store_dword v[0:1], v2, off
  shader_data(6) = int(z'007D0200', i32)
  shader_data(7) = int(z'BF8C0070', i32)  ! s_waitcnt vmcnt(0)
  shader_data(8) = int(z'BF810000', i32)  ! s_endpgm
  
  ! Build command buffer
  call build_dispatch_ib(ib_data, output_buffer%get_va(), &
                         signal_buffer%get_va(), shader_buffer%get_va())
  
  ! Submit 1: With shader buffer
  print *, "Submit 1: IB + shader buffer"
  status = sp_submit_ib_with_bo(ctx_ptr, ib_buffer%bo_ptr, 150_i32, &
                                shader_buffer%bo_ptr, fence1)
  if (status /= 0) then
    print *, "ERROR: Submit 1 failed"
    stop 1
  end if
  
  ! Submit 2: With output buffer
  print *, "Submit 2: IB + output buffer"
  status = sp_submit_ib_with_bo(ctx_ptr, ib_buffer%bo_ptr, 150_i32, &
                                output_buffer%bo_ptr, fence2)
  if (status /= 0) then
    print *, "ERROR: Submit 2 failed"
    stop 1
  end if
  
  ! Submit 3: With signal buffer
  print *, "Submit 3: IB + signal buffer"
  status = sp_submit_ib_with_bo(ctx_ptr, ib_buffer%bo_ptr, 150_i32, &
                                signal_buffer%bo_ptr, fence3)
  if (status /= 0) then
    print *, "ERROR: Submit 3 failed"
    stop 1
  end if
  
  ! Wait for all fences
  status = sp_fence_wait(ctx_ptr, fence1, 1000000000_i64)
  print *, "✅ Fence 1 signaled"
  status = sp_fence_wait(ctx_ptr, fence2, 1000000000_i64)
  print *, "✅ Fence 2 signaled"
  status = sp_fence_wait(ctx_ptr, fence3, 1000000000_i64)
  print *, "✅ Fence 3 signaled"
  
  ! Poll for EOP
  eop_signaled = .false.
  do poll_count = 1, 1000
    if (signal_data(1) == 1) then
      eop_signaled = .true.
      exit
    end if
    call execute_command_line("sleep 0.001", wait=.true.)
  end do
  
  if (eop_signaled) then
    print '(A,I0,A)', "✅ EOP signaled after ", poll_count, " polls"
  else
    print *, "❌ No EOP signal"
  end if
  
  ! Check output
  shader_wrote = .false.
  do i = 1, 8
    if (output_data(i) == int(z'DEADBEEF', i32)) then
      shader_wrote = .true.
      print '(A,I0,A)', "🎉 Found 0xDEADBEEF at offset ", (i-1)*4, "!"
      exit
    end if
  end do
  
  if (.not. shader_wrote) then
    print *, "❌ No shader write detected"
    print '(A,Z8)', "First dword: 0x", output_data(1)
  end if
  
  call sp_pm4_cleanup(ctx_ptr)
  
contains

  subroutine build_dispatch_ib(ib_data, output_va, signal_va, shader_va)
    integer(c_int32_t), intent(inout) :: ib_data(:)
    integer(i64), intent(in) :: output_va, signal_va, shader_va
    integer(i32) :: idx
    
    idx = 1
    
    ! CLEAR_STATE
    ib_data(idx) = ior(ishft(3_i32, 30), PM4_CLEAR_STATE)
    ib_data(idx+1) = 0
    idx = idx + 2
    
    ! Enable all CUs
    ib_data(idx) = ior(ishft(3_i32, 30), ior(ishft(4_i32, 16), PM4_SET_SH_REG))
    ib_data(idx+1) = COMPUTE_STATIC_THREAD_MGMT_SE0
    ib_data(idx+2) = int(z'FFFFFFFF', i32)
    ib_data(idx+3) = int(z'FFFFFFFF', i32)
    ib_data(idx+4) = int(z'FFFFFFFF', i32)
    ib_data(idx+5) = int(z'FFFFFFFF', i32)
    idx = idx + 6
    
    ! Set shader (GFX10 format)
    ib_data(idx) = ior(ishft(3_i32, 30), ior(ishft(2_i32, 16), PM4_SET_SH_REG))
    ib_data(idx+1) = COMPUTE_PGM_LO
    ib_data(idx+2) = int(ishft(shader_va, -8), c_int32_t)
    ib_data(idx+3) = int(ishft(shader_va, -40), c_int32_t)
    idx = idx + 4
    
    ! RSRC1/2
    ib_data(idx) = ior(ishft(3_i32, 30), ior(ishft(2_i32, 16), PM4_SET_SH_REG))
    ib_data(idx+1) = COMPUTE_PGM_RSRC1
    ib_data(idx+2) = int(z'00000041', i32)  ! VGPR=1, SGPR=1
    ib_data(idx+3) = int(z'00000004', i32)  ! USER_SGPR=2
    idx = idx + 4
    
    ! USER_DATA
    ib_data(idx) = ior(ishft(3_i32, 30), ior(ishft(2_i32, 16), PM4_SET_SH_REG))
    ib_data(idx+1) = COMPUTE_USER_DATA_0
    ib_data(idx+2) = int(output_va, c_int32_t)
    ib_data(idx+3) = int(ishft(output_va, -32), c_int32_t)
    idx = idx + 4
    
    ! Workgroup and start
    ib_data(idx) = ior(ishft(3_i32, 30), ior(ishft(6_i32, 16), PM4_SET_SH_REG))
    ib_data(idx+1) = COMPUTE_NUM_THREAD_X
    ib_data(idx+2) = 64_i32
    ib_data(idx+3) = 1_i32
    ib_data(idx+4) = 1_i32
    ib_data(idx+5) = 0_i32  ! START_X
    ib_data(idx+6) = 0_i32  ! START_Y
    ib_data(idx+7) = 0_i32  ! START_Z
    idx = idx + 8
    
    ! Resource limits
    ib_data(idx) = ior(ishft(3_i32, 30), ior(ishft(1_i32, 16), PM4_SET_SH_REG))
    ib_data(idx+1) = COMPUTE_RESOURCE_LIMITS
    ib_data(idx+2) = 0
    idx = idx + 3
    
    ! DISPATCH_DIRECT with initiator
    ib_data(idx) = ior(ishft(3_i32, 30), ior(ishft(4_i32, 16), PM4_DISPATCH_DIRECT))
    ib_data(idx+1) = 1_i32  ! NUM_GROUPS_X
    ib_data(idx+2) = 1_i32  ! NUM_GROUPS_Y
    ib_data(idx+3) = 1_i32  ! NUM_GROUPS_Z
    ! COMPUTE_SHADER_EN (bit 0) | DATA_ATC (bit 12)
    ib_data(idx+4) = ior(int(z'00000001', i32), ishft(1_i32, 12))  ! 0x1001
    idx = idx + 5
    
    print *, "✓ DISPATCH_DIRECT with COMPUTE_SHADER_EN | DATA_ATC"
    
    ! RELEASE_MEM EOP
    ib_data(idx) = ior(ishft(3_i32, 30), ior(ishft(6_i32, 16), PM4_RELEASE_MEM))
    ib_data(idx+1) = ior(ishft(CACHE_FLUSH_AND_INV_TS_EVENT, 0), &
                         ior(ishft(EVENT_INDEX_EOP, 8), &
                         ior(ishft(1_i32, 17), ishft(1_i32, 18))))
    ib_data(idx+2) = ior(ishft(DST_SEL_MEM, 16), &
                         ior(ishft(INT_SEL_NONE, 24), &
                             ishft(DATA_SEL_IMMEDIATE_32, 29)))
    ib_data(idx+3) = int(signal_va, c_int32_t)
    ib_data(idx+4) = int(ishft(signal_va, -32), c_int32_t)
    ib_data(idx+5) = 1_i32
    ib_data(idx+6) = 0_i32
    idx = idx + 7
    
  end subroutine

end program test_pm4_multi_submit