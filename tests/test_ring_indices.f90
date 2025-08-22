program test_ring_indices
    use pm4_submit
    use iso_c_binding
    implicit none
    
    type(c_ptr) :: ctx
    type(sp_bo), target :: ib_bo
    type(c_ptr) :: ib_ptr
    integer(c_int32_t), pointer :: ib_data(:)
    integer :: i, idx, status
    type(sp_fence) :: fence
    integer :: ring_idx, instance_idx
    logical :: found_working = .false.
    
    ! Initialize context
    ctx = sp_pm4_ctx_create()
    if (.not. c_associated(ctx)) then
        print *, "Failed to create PM4 context"
        stop 1
    end if
    
    print *, "Testing different ring/instance combinations..."
    print *, ""
    
    ! Try different combinations
    do instance_idx = 0, 1
        do ring_idx = 0, 3
            print '(A,I1,A,I1,A)', "Testing instance=", instance_idx, ", ring=", ring_idx, "..."
            
            ! Create IB
            ib_bo = sp_bo_new(ctx, 4096_c_size_t)
            
            ! Map and write simple s_endpgm
            ib_ptr = sp_bo_map(ib_bo)
            call c_f_pointer(ib_ptr, ib_data, [1024])
            
            idx = 1
            ! Just s_endpgm
            ib_data(idx) = int(z'BF810000', c_int32_t)
            idx = idx + 1
            
            call sp_bo_unmap(ib_bo)
            
            ! Submit with specific ring/instance
            status = sp_submit_ib_ring(ctx, ib_bo, int(idx-1, c_int32_t), &
                                      int(instance_idx, c_int32_t), &
                                      int(ring_idx, c_int32_t), fence)
            
            if (status == 0) then
                ! Wait briefly
                status = sp_fence_wait(ctx, fence, int(100, c_int64_t))
                if (status == 0) then
                    print *, "  ✅ Fence signaled!"
                    found_working = .true.
                else
                    print *, "  ❌ Fence timeout"
                end if
            else
                print *, "  ❌ Submit failed with error:", status
            end if
            
            call sp_bo_free(ib_bo)
            print *, ""
        end do
    end do
    
    if (.not. found_working) then
        print *, "❌ No working ring/instance combination found!"
    end if
    
    call sp_pm4_ctx_destroy(ctx)
    
contains
    ! Custom submit function that allows specifying ring/instance
    function sp_submit_ib_ring(ctx, ib_bo, ib_size_dw, ip_instance, ring, out_fence) result(status)
        type(c_ptr), value :: ctx
        type(sp_bo), intent(in) :: ib_bo
        integer(c_int32_t), value :: ib_size_dw, ip_instance, ring
        type(sp_fence), intent(out) :: out_fence
        integer(c_int) :: status
        
        interface
            function sp_submit_ib_ring_c(ctx, ib_bo, ib_size_dw, ip_instance, ring, out_fence) &
                    bind(C, name="sp_submit_ib_ring") result(ret)
                import :: c_ptr, c_int, c_int32_t, sp_fence
                type(c_ptr), value :: ctx, ib_bo
                integer(c_int32_t), value :: ib_size_dw, ip_instance, ring
                type(sp_fence) :: out_fence
                integer(c_int) :: ret
            end function
        end interface
        
        status = sp_submit_ib_ring_c(ctx, ib_bo%ptr, ib_size_dw, ip_instance, ring, out_fence)
    end function

end program