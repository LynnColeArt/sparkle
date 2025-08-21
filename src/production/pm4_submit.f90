module pm4_submit
  ! PM4 Submit Fortran Interface
  ! ============================
  ! ISO C binding interface to real PM4 submission
  
  use iso_c_binding
  use kinds
  implicit none
  private
  
  ! Public types and interfaces
  public :: sp_pm4_ctx, sp_bo, sp_fence
  public :: sp_pm4_init, sp_pm4_cleanup
  public :: sp_buffer_alloc, sp_buffer_free
  public :: sp_submit_ib, sp_submit_ib_with_bo, sp_fence_wait, sp_fence_check
  public :: sp_build_canary_ib
  
  ! Buffer flags
  integer(c_int), parameter, public :: SP_BO_DEVICE_LOCAL = int(z'01', c_int)
  integer(c_int), parameter, public :: SP_BO_HOST_VISIBLE = int(z'02', c_int)
  
  ! PM4 opcodes
  integer(c_int32_t), parameter, public :: PM4_NOP = int(z'10', c_int32_t)
  integer(c_int32_t), parameter, public :: PM4_SET_SH_REG = int(z'76', c_int32_t)
  integer(c_int32_t), parameter, public :: PM4_DISPATCH_DIRECT = int(z'15', c_int32_t)
  integer(c_int32_t), parameter, public :: PM4_WRITE_DATA = int(z'37', c_int32_t)
  integer(c_int32_t), parameter, public :: PM4_COPY_DATA = int(z'40', c_int32_t)
  integer(c_int32_t), parameter, public :: PM4_RELEASE_MEM = int(z'49', c_int32_t)
  
  ! RELEASE_MEM event types
  integer(c_int32_t), parameter, public :: CS_DONE = int(z'9', c_int32_t)
  integer(c_int32_t), parameter, public :: BOTTOM_OF_PIPE_TS = int(z'4', c_int32_t)
  
  ! RELEASE_MEM field values (Mini's precise recipe)
  integer(c_int32_t), parameter, public :: EVENT_INDEX_EOP = int(z'5', c_int32_t)
  integer(c_int32_t), parameter, public :: DATA_SEL_TIMESTAMP = int(z'3', c_int32_t)
  integer(c_int32_t), parameter, public :: INT_SEL_NONE = int(z'0', c_int32_t)
  integer(c_int32_t), parameter, public :: DST_SEL_MEM = int(z'2', c_int32_t)
  
  ! Compute shader setup registers
  integer(c_int32_t), parameter, public :: COMPUTE_PGM_LO = int(z'20C', c_int32_t)
  integer(c_int32_t), parameter, public :: COMPUTE_PGM_HI = int(z'20D', c_int32_t)
  integer(c_int32_t), parameter, public :: COMPUTE_PGM_RSRC1 = int(z'212', c_int32_t)
  integer(c_int32_t), parameter, public :: COMPUTE_PGM_RSRC2 = int(z'213', c_int32_t)
  integer(c_int32_t), parameter, public :: COMPUTE_RESOURCE_LIMITS = int(z'215', c_int32_t)
  integer(c_int32_t), parameter, public :: COMPUTE_USER_DATA_0 = int(z'240', c_int32_t)
  
  ! Register offsets
  integer(c_int32_t), parameter, public :: COMPUTE_NUM_THREAD_X = int(z'219', c_int32_t)
  integer(c_int32_t), parameter, public :: COMPUTE_NUM_THREAD_Y = int(z'21A', c_int32_t)
  integer(c_int32_t), parameter, public :: COMPUTE_NUM_THREAD_Z = int(z'21B', c_int32_t)
  
  ! PM4 context
  type, bind(C) :: sp_pm4_ctx
    integer(c_int) :: fd
    integer(c_int32_t) :: gpu_ctx_id
    integer(c_int32_t) :: device_id
    integer(c_int32_t) :: num_compute_rings
  end type
  
  ! Buffer object
  type, bind(C) :: sp_bo
    integer(c_int32_t) :: handle
    type(c_ptr) :: cpu_ptr
    integer(c_size_t) :: size
    integer(c_int64_t) :: gpu_va
    integer(c_int32_t) :: flags
  end type
  
  ! Fence
  type, bind(C) :: sp_fence
    integer(c_int64_t) :: fence
    integer(c_int32_t) :: ctx_id
    integer(c_int32_t) :: ip_type
    integer(c_int32_t) :: ring
  end type
  
  ! C interfaces
  interface
    function sp_pm4_init_c(device_path) bind(C, name="sp_pm4_init")
      import :: c_ptr, c_char
      character(kind=c_char), intent(in) :: device_path(*)
      type(c_ptr) :: sp_pm4_init_c
    end function
    
    subroutine sp_pm4_cleanup_c(ctx) bind(C, name="sp_pm4_cleanup")
      import :: c_ptr
      type(c_ptr), value :: ctx
    end subroutine
    
    function sp_buffer_alloc_c(ctx, size, flags) bind(C, name="sp_buffer_alloc")
      import :: c_ptr, c_size_t, c_int32_t
      type(c_ptr), value :: ctx
      integer(c_size_t), value :: size
      integer(c_int32_t), value :: flags
      type(c_ptr) :: sp_buffer_alloc_c
    end function
    
    subroutine sp_buffer_free_c(ctx, bo) bind(C, name="sp_buffer_free")
      import :: c_ptr
      type(c_ptr), value :: ctx
      type(c_ptr), value :: bo
    end subroutine
    
    function sp_submit_ib_c(ctx, ib_bo, ib_size_dw, out_fence) bind(C, name="sp_submit_ib")
      import :: c_ptr, c_int32_t, c_int, sp_fence
      type(c_ptr), value :: ctx
      type(c_ptr), value :: ib_bo
      integer(c_int32_t), value :: ib_size_dw
      type(sp_fence), intent(out) :: out_fence
      integer(c_int) :: sp_submit_ib_c
    end function
    
    function sp_submit_ib_with_bo_c(ctx, ib_bo, ib_size_dw, data_bo, out_fence) &
         bind(C, name="sp_submit_ib_with_bo")
      import :: c_ptr, c_int32_t, c_int, sp_fence
      type(c_ptr), value :: ctx
      type(c_ptr), value :: ib_bo
      integer(c_int32_t), value :: ib_size_dw
      type(c_ptr), value :: data_bo
      type(sp_fence), intent(out) :: out_fence
      integer(c_int) :: sp_submit_ib_with_bo_c
    end function
    
    function sp_fence_wait_c(ctx, fence, timeout_ns) bind(C, name="sp_fence_wait")
      import :: c_ptr, c_int, c_int64_t, sp_fence
      type(c_ptr), value :: ctx
      type(sp_fence), intent(in) :: fence
      integer(c_int64_t), value :: timeout_ns
      integer(c_int) :: sp_fence_wait_c
    end function
    
    function sp_fence_check_c(ctx, fence) bind(C, name="sp_fence_check")
      import :: c_ptr, c_int, sp_fence
      type(c_ptr), value :: ctx
      type(sp_fence), intent(in) :: fence
      integer(c_int) :: sp_fence_check_c
    end function
  end interface
  
contains

  ! Initialize PM4 context
  function sp_pm4_init(device_path) result(ctx_ptr)
    character(len=*), intent(in), optional :: device_path
    type(c_ptr) :: ctx_ptr
    character(kind=c_char, len=:), allocatable :: c_path
    
    if (present(device_path)) then
      c_path = device_path // c_null_char
      ctx_ptr = sp_pm4_init_c(c_path)
    else
      ctx_ptr = sp_pm4_init_c(c_null_char)
    end if
  end function
  
  ! Cleanup PM4 context
  subroutine sp_pm4_cleanup(ctx_ptr)
    type(c_ptr), intent(in) :: ctx_ptr
    call sp_pm4_cleanup_c(ctx_ptr)
  end subroutine
  
  ! Allocate buffer
  function sp_buffer_alloc(ctx_ptr, size, flags) result(bo_ptr)
    type(c_ptr), intent(in) :: ctx_ptr
    integer(i64), intent(in) :: size
    integer, intent(in) :: flags
    type(c_ptr) :: bo_ptr
    
    bo_ptr = sp_buffer_alloc_c(ctx_ptr, int(size, c_size_t), int(flags, c_int32_t))
  end function
  
  ! Free buffer
  subroutine sp_buffer_free(ctx_ptr, bo_ptr)
    type(c_ptr), intent(in) :: ctx_ptr
    type(c_ptr), intent(in) :: bo_ptr
    
    call sp_buffer_free_c(ctx_ptr, bo_ptr)
  end subroutine
  
  ! Submit IB
  function sp_submit_ib(ctx_ptr, ib_bo_ptr, ib_size_dw, fence) result(status)
    type(c_ptr), intent(in) :: ctx_ptr
    type(c_ptr), intent(in) :: ib_bo_ptr
    integer, intent(in) :: ib_size_dw
    type(sp_fence), intent(out) :: fence
    integer :: status
    
    status = sp_submit_ib_c(ctx_ptr, ib_bo_ptr, int(ib_size_dw, c_int32_t), fence)
  end function
  
  ! Submit IB with data buffer
  function sp_submit_ib_with_bo(ctx_ptr, ib_bo_ptr, ib_size_dw, data_bo_ptr, fence) result(status)
    type(c_ptr), intent(in) :: ctx_ptr
    type(c_ptr), intent(in) :: ib_bo_ptr
    integer, intent(in) :: ib_size_dw
    type(c_ptr), intent(in) :: data_bo_ptr
    type(sp_fence), intent(out) :: fence
    integer :: status
    
    status = sp_submit_ib_with_bo_c(ctx_ptr, ib_bo_ptr, int(ib_size_dw, c_int32_t), &
                                    data_bo_ptr, fence)
  end function
  
  ! Wait for fence
  function sp_fence_wait(ctx_ptr, fence, timeout_ns) result(status)
    type(c_ptr), intent(in) :: ctx_ptr
    type(sp_fence), intent(in) :: fence
    integer(i64), intent(in) :: timeout_ns
    integer :: status
    
    status = sp_fence_wait_c(ctx_ptr, fence, int(timeout_ns, c_int64_t))
  end function
  
  ! Check fence (non-blocking)
  function sp_fence_check(ctx_ptr, fence) result(status)
    type(c_ptr), intent(in) :: ctx_ptr
    type(sp_fence), intent(in) :: fence
    integer :: status
    
    status = sp_fence_check_c(ctx_ptr, fence)
  end function
  
  ! Build canary IB that writes 0xCAFEBABE
  subroutine sp_build_canary_ib(ib_data, ib_size_dw, shader_va, buffer_va)
    integer(c_int32_t), intent(out) :: ib_data(:)
    integer, intent(out) :: ib_size_dw
    integer(i64), intent(in) :: shader_va
    integer(i64), intent(in) :: buffer_va
    
    integer :: idx
    
    idx = 1
    
    ! Set compute shader program address
    ib_data(idx) = ior(ishft(3, 30), ior(ishft(2, 16), PM4_SET_SH_REG))  ! Type 3, count 2
    idx = idx + 1
    ib_data(idx) = COMPUTE_NUM_THREAD_X  ! Register offset
    idx = idx + 1
    ib_data(idx) = 1  ! 1 thread X
    idx = idx + 1
    ib_data(idx) = 1  ! 1 thread Y
    idx = idx + 1
    
    ! Write canary value using WRITE_DATA packet
    ib_data(idx) = ior(ishft(3, 30), ior(ishft(4, 16), PM4_WRITE_DATA))  ! Type 3, count 4
    idx = idx + 1
    ib_data(idx) = ior(ishft(5, 8), 0)  ! DST_SEL=5 (memory), WR_CONFIRM=0
    idx = idx + 1
    ib_data(idx) = int(iand(buffer_va, z'FFFFFFFF'), c_int32_t)  ! Address low (proper mask)
    idx = idx + 1
    ib_data(idx) = int(iand(shiftr(buffer_va, 32), z'FFFFFFFF'), c_int32_t)  ! Address high
    idx = idx + 1
    ib_data(idx) = int(z'CAFEBABE', c_int32_t)  ! Canary value
    idx = idx + 1
    
    ! NOP padding
    ib_data(idx) = ior(ishft(3, 30), ior(ishft(0, 16), PM4_NOP))
    idx = idx + 1
    
    ib_size_dw = idx - 1
    
  end subroutine

end module pm4_submit