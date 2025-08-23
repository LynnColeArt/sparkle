module amd_device_mod
  use iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_f_pointer, c_loc
  use kinds
  use sporkle_types
  use sporkle_amdgpu_direct
  use sporkle_gpu_va_allocator
  implicit none
  private
  
  public :: amd_device
  
  type, extends(compute_device) :: amd_device
    type(amdgpu_device_handle) :: gpu_device
    logical :: use_gpu = .false.
  contains
    procedure :: allocate => amd_allocate
    procedure :: deallocate => amd_deallocate
    procedure :: memcpy => amd_memcpy
    procedure :: execute => amd_execute
    procedure :: synchronize => amd_synchronize
    procedure :: get_info => amd_get_info
    procedure :: initialize => amd_initialize
  end type amd_device
  
contains

  function amd_allocate(self, size_bytes, pinned) result(buffer)
    class(amd_device), intent(inout) :: self
    integer(i64), intent(in) :: size_bytes
    logical, intent(in), optional :: pinned
    type(sporkle_buffer) :: buffer
    
    buffer%size_bytes = size_bytes
    buffer%owning_device = self%device_id
    buffer%is_pinned = .false.
    if (present(pinned)) buffer%is_pinned = pinned
    
    if (self%use_gpu) then
      ! Use real GPU allocation
      block
        type(amdgpu_buffer) :: gpu_buffer
        integer :: domain
        
        ! Choose memory domain
        if (present(pinned) .and. pinned) then
          domain = AMDGPU_GEM_DOMAIN_GTT  ! System memory visible to GPU
        else
          domain = AMDGPU_GEM_DOMAIN_GTT  ! Use GTT for CPU visibility
        end if
        
        ! Allocate GPU buffer
        gpu_buffer = amdgpu_allocate_buffer(self%gpu_device, size_bytes, domain)
        
        if (gpu_buffer%handle == 0) then
          print *, "❌ Failed to allocate GPU buffer"
          buffer%data = c_null_ptr
          return
        end if
        
        ! Map to GPU VA if not already mapped
        if (gpu_buffer%va_addr == 0) then
          gpu_buffer%va_addr = allocate_gpu_va(size_bytes)
          if (amdgpu_map_va(self%gpu_device, gpu_buffer, gpu_buffer%va_addr) /= 0) then
            print *, "❌ Failed to map GPU VA"
            buffer%data = c_null_ptr
            return
          end if
        end if
        
        ! Map for CPU access
        if (amdgpu_map_buffer(self%gpu_device, gpu_buffer) /= 0) then
          print *, "❌ Failed to map buffer for CPU access"
          buffer%data = c_null_ptr
          return
        end if
        
        buffer%data = gpu_buffer%cpu_ptr
        
        ! Store GPU buffer handle in a global table for cleanup
        ! TODO: Need proper handle management
      end block
    else
      ! Fallback to host memory
      block
        character, pointer :: mem(:)
        allocate(mem(size_bytes))
        buffer%data = c_loc(mem(1))
      end block
    end if
    
  end function amd_allocate
  
  subroutine amd_deallocate(self, buffer)
    class(amd_device), intent(inout) :: self
    type(sporkle_buffer), intent(inout) :: buffer
    
    ! For now, just deallocate host memory
    if (c_associated(buffer%data)) then
      ! In a real implementation we'd deallocate properly
      ! For now just clear the pointer
      buffer%data = c_null_ptr
      buffer%size_bytes = 0
    end if
    
  end subroutine amd_deallocate
  
  function amd_memcpy(self, dst, src, size_bytes) result(status)
    class(amd_device), intent(inout) :: self
    type(sporkle_buffer), intent(inout) :: dst
    type(sporkle_buffer), intent(in) :: src
    integer(i64), intent(in) :: size_bytes
    integer(i32) :: status
    
    ! For now, just copy host memory
    ! Real implementation would use HIP memcpy
    block
      character, pointer :: src_bytes(:), dst_bytes(:)
      integer :: n  ! Convert int64 size_bytes to default integer for c_f_pointer shape
      
      if (.not. c_associated(src%data) .or. .not. c_associated(dst%data)) then
        status = -1
        return
      end if
      
      n = int(size_bytes, kind(n))
      call c_f_pointer(src%data, src_bytes, [n])
      call c_f_pointer(dst%data, dst_bytes, [n])
      dst_bytes = src_bytes
      status = 0
    end block
    
  end function amd_memcpy
  
  function amd_execute(self, kernel_name, args, grid_size, block_size) result(status)
    class(amd_device), intent(inout) :: self
    character(len=*), intent(in) :: kernel_name
    type(c_ptr), intent(in) :: args(:)
    integer(i32), intent(in) :: grid_size(3)
    integer(i32), intent(in) :: block_size(3)
    integer(i32) :: status
    
    ! Placeholder - real implementation would launch HIP/Vulkan kernel
    print *, "AMD device ", self%device_id, " would execute kernel: ", kernel_name
    status = 0
    
  end function amd_execute
  
  function amd_synchronize(self) result(status)
    class(amd_device), intent(inout) :: self
    integer(i32) :: status
    
    ! Placeholder - real implementation would sync device
    status = 0
    
  end function amd_synchronize
  
  subroutine amd_get_info(self)
    class(amd_device), intent(inout) :: self
    
    ! Fill in device information
    self%name = "AMD GPU (via kernel driver)"
    self%capabilities%supports_unified_memory = .false.
    self%capabilities%supports_p2p = .false.
    self%capabilities%supports_float64 = .true.
    self%capabilities%supports_float16 = .true.
    self%is_available = .true.
    
  end subroutine amd_get_info
  
  subroutine amd_initialize(self, enable_gpu)
    class(amd_device), intent(inout) :: self
    logical, intent(in), optional :: enable_gpu
    
    integer :: status
    
    self%use_gpu = .false.
    if (present(enable_gpu)) self%use_gpu = enable_gpu
    
    if (self%use_gpu) then
      ! Initialize AMDGPU
      status = amdgpu_init()
      if (status /= 0) then
        print *, "⚠️ Failed to initialize AMDGPU, falling back to CPU"
        self%use_gpu = .false.
        return
      end if
      
      ! Open device
      self%gpu_device = amdgpu_device_handle(self%device_id)
      
      print *, "✅ AMD GPU initialized via AMDGPU direct interface"
    else
      print *, "ℹ️ AMD device using CPU allocation"
    end if
    
  end subroutine amd_initialize

end module amd_device_mod