! Unified Device Abstraction - Universal compute interface
! Provides a single interface for all compute devices regardless of backend

module sparkle_unified_device
  use iso_fortran_env, only: real32, int32, int64
  use sparkle_device_detection, only: device_capability
  implicit none
  
  private
  public :: unified_device, create_unified_device, destroy_unified_device
  public :: execute_convolution, execute_matmul, execute_custom_kernel
  public :: device_memory_buffer, allocate_device_memory, free_device_memory
  public :: copy_to_device, copy_from_device, device_synchronize
  
  ! Unified device handle
  type :: unified_device
    character(len=32) :: device_id
    character(len=16) :: device_type      ! "cpu", "gpu", "neural"
    character(len=32) :: backend_name     ! Actual implementation module
    logical :: initialized = .false.
    logical :: available = .false.
    
    ! Device-specific context (opaque pointers)
    integer(int64) :: device_context = 0
    integer(int64) :: command_queue = 0
    integer(int64) :: memory_pool = 0
    
    ! Performance tracking
    real(real32) :: last_execution_time_ms = 0.0
    integer :: successful_executions = 0
    integer :: failed_executions = 0
    
    ! Capabilities
    type(device_capability) :: capabilities
  end type
  
  ! Unified memory buffer
  type :: device_memory_buffer
    integer(int64) :: size_bytes = 0
    integer(int64) :: device_ptr = 0     ! Device-specific pointer
    logical :: allocated = .false.
    character(len=16) :: memory_type     ! "device", "shared", "pinned"
    type(unified_device), pointer :: device => null()
  end type
  
  ! Execution context for operations
  type :: execution_context
    real(real32), pointer :: workspace(:) => null()
    integer(int64) :: workspace_size = 0
    logical :: async_execution = .false.
    integer(int64) :: completion_event = 0
  end type
  
contains

  ! Create unified device from detected capability
  function create_unified_device(capability) result(device)
    type(device_capability), intent(in) :: capability
    type(unified_device) :: device
    
    device%device_id = capability%device_name
    device%device_type = capability%device_type
    device%backend_name = capability%backend_module
    device%capabilities = capability
    
    ! Initialize device-specific backend
    select case(trim(capability%device_type))
    case("cpu")
      call initialize_cpu_backend(device)
    case("gpu")
      call initialize_gpu_backend(device, capability%vendor)
    case("neural")
      call initialize_neural_backend(device)
    case default
      print *, "Warning: Unknown device type: ", trim(capability%device_type)
      device%available = .false.
      return
    end select
    
    if (device%available) then
      device%initialized = .true.
      print '(A,A)', "✅ Unified device created: ", trim(device%device_id)
    else
      print '(A,A)', "❌ Failed to create device: ", trim(device%device_id)
    end if
    
  end function create_unified_device
  
  ! Destroy unified device and cleanup resources
  subroutine destroy_unified_device(device)
    type(unified_device), intent(inout) :: device
    
    if (.not. device%initialized) return
    
    ! Device-specific cleanup
    select case(trim(device%device_type))
    case("cpu")
      call cleanup_cpu_backend(device)
    case("gpu")
      call cleanup_gpu_backend(device)
    case("neural")
      call cleanup_neural_backend(device)
    end select
    
    device%initialized = .false.
    device%available = .false.
    
  end subroutine destroy_unified_device
  
  ! Universal convolution execution
  subroutine execute_convolution(device, input, weights, output, &
                                N, C, H, W, K, kernel_size, stride, pad, &
                                H_out, W_out, async, status)
    type(unified_device), intent(inout) :: device
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    logical, intent(in), optional :: async
    integer, intent(out), optional :: status
    
    real(real32) :: start_time, end_time
    logical :: async_exec
    integer :: exec_status
    
    ! Default parameters
    async_exec = .false.
    if (present(async)) async_exec = async
    exec_status = 0
    
    if (.not. device%available) then
      exec_status = -1
      if (present(status)) status = exec_status
      return
    end if
    
    ! Start timing
    call cpu_time(start_time)
    
    ! Dispatch to appropriate backend
    select case(trim(device%device_type))
    case("cpu")
      call execute_cpu_convolution(device, input, weights, output, &
                                  N, C, H, W, K, kernel_size, stride, pad, &
                                  H_out, W_out, exec_status)
    case("gpu")
      call execute_gpu_convolution(device, input, weights, output, &
                                  N, C, H, W, K, kernel_size, stride, pad, &
                                  H_out, W_out, async_exec, exec_status)
    case("neural")
      call execute_neural_convolution(device, input, weights, output, &
                                     N, C, H, W, K, kernel_size, stride, pad, &
                                     H_out, W_out, exec_status)
    case default
      exec_status = -2  ! Unsupported device type
    end select
    
    ! Update timing and statistics
    call cpu_time(end_time)
    device%last_execution_time_ms = (end_time - start_time) * 1000.0
    
    if (exec_status == 0) then
      device%successful_executions = device%successful_executions + 1
    else
      device%failed_executions = device%failed_executions + 1
    end if
    
    if (present(status)) status = exec_status
    
  end subroutine execute_convolution
  
  ! Universal matrix multiplication
  subroutine execute_matmul(device, A, B, C, M, N, K, async, status)
    type(unified_device), intent(inout) :: device
    real(real32), intent(in) :: A(:), B(:)
    real(real32), intent(out) :: C(:)
    integer, intent(in) :: M, N, K
    logical, intent(in), optional :: async
    integer, intent(out), optional :: status
    
    logical :: async_exec
    integer :: exec_status
    
    async_exec = .false.
    if (present(async)) async_exec = async
    exec_status = 0
    
    if (.not. device%available) then
      exec_status = -1
      if (present(status)) status = exec_status
      return
    end if
    
    ! Dispatch to appropriate backend
    select case(trim(device%device_type))
    case("cpu")
      call execute_cpu_matmul(device, A, B, C, M, N, K, exec_status)
    case("gpu")
      call execute_gpu_matmul(device, A, B, C, M, N, K, async_exec, exec_status)
    case("neural")
      call execute_neural_matmul(device, A, B, C, M, N, K, exec_status)
    case default
      exec_status = -2
    end select
    
    if (present(status)) status = exec_status
    
  end subroutine execute_matmul
  
  ! Custom kernel execution
  subroutine execute_custom_kernel(device, kernel_source, input_buffers, &
                                  output_buffers, params, async, status)
    type(unified_device), intent(inout) :: device
    character(len=*), intent(in) :: kernel_source
    type(device_memory_buffer), intent(in) :: input_buffers(:)
    type(device_memory_buffer), intent(inout) :: output_buffers(:)
    real(real32), intent(in) :: params(:)
    logical, intent(in), optional :: async
    integer, intent(out), optional :: status
    
    logical :: async_exec
    integer :: exec_status
    
    async_exec = .false.
    if (present(async)) async_exec = async
    exec_status = 0
    
    if (.not. device%available) then
      exec_status = -1
      if (present(status)) status = exec_status
      return
    end if
    
    ! Dispatch to appropriate backend
    select case(trim(device%device_type))
    case("gpu")
      call execute_gpu_custom_kernel(device, kernel_source, input_buffers, &
                                     output_buffers, params, async_exec, exec_status)
    case default
      exec_status = -3  ! Custom kernels not supported on this device
    end select
    
    if (present(status)) status = exec_status
    
  end subroutine execute_custom_kernel
  
  ! Memory management
  function allocate_device_memory(device, size_bytes, memory_type) result(buffer)
    type(unified_device), intent(in) :: device
    integer(int64), intent(in) :: size_bytes
    character(len=*), intent(in), optional :: memory_type
    type(device_memory_buffer) :: buffer
    
    character(len=16) :: mem_type
    
    ! Default memory type
    mem_type = "device"
    if (present(memory_type)) mem_type = memory_type
    
    buffer%size_bytes = size_bytes
    buffer%memory_type = mem_type
    buffer%device => null()  ! Would point to device in real implementation
    
    ! Dispatch to device-specific allocator
    select case(trim(device%device_type))
    case("cpu")
      call allocate_cpu_memory(buffer, size_bytes, mem_type)
    case("gpu")
      call allocate_gpu_memory(buffer, size_bytes, mem_type)
    case("neural")
      call allocate_neural_memory(buffer, size_bytes, mem_type)
    end select
    
  end function allocate_device_memory
  
  subroutine free_device_memory(buffer)
    type(device_memory_buffer), intent(inout) :: buffer
    
    if (.not. buffer%allocated) return
    
    ! Device-specific deallocation would go here
    buffer%allocated = .false.
    buffer%device_ptr = 0
    buffer%size_bytes = 0
    
  end subroutine free_device_memory
  
  ! Data transfer operations
  subroutine copy_to_device(device, host_data, device_buffer, async, status)
    type(unified_device), intent(in) :: device
    real(real32), intent(in) :: host_data(:)
    type(device_memory_buffer), intent(in) :: device_buffer
    logical, intent(in), optional :: async
    integer, intent(out), optional :: status
    
    logical :: async_copy
    integer :: copy_status
    
    async_copy = .false.
    if (present(async)) async_copy = async
    copy_status = 0
    
    ! Dispatch to device-specific copy
    select case(trim(device%device_type))
    case("cpu")
      call copy_to_cpu_memory(host_data, device_buffer, copy_status)
    case("gpu")
      call copy_to_gpu_memory(host_data, device_buffer, async_copy, copy_status)
    case("neural")
      call copy_to_neural_memory(host_data, device_buffer, copy_status)
    end select
    
    if (present(status)) status = copy_status
    
  end subroutine copy_to_device
  
  subroutine copy_from_device(device, device_buffer, host_data, async, status)
    type(unified_device), intent(in) :: device
    type(device_memory_buffer), intent(in) :: device_buffer
    real(real32), intent(out) :: host_data(:)
    logical, intent(in), optional :: async
    integer, intent(out), optional :: status
    
    logical :: async_copy
    integer :: copy_status
    
    async_copy = .false.
    if (present(async)) async_copy = async
    copy_status = 0
    
    ! Dispatch to device-specific copy
    select case(trim(device%device_type))
    case("cpu")
      call copy_from_cpu_memory(device_buffer, host_data, copy_status)
    case("gpu")
      call copy_from_gpu_memory(device_buffer, host_data, async_copy, copy_status)
    case("neural")
      call copy_from_neural_memory(device_buffer, host_data, copy_status)
    end select
    
    if (present(status)) status = copy_status
    
  end subroutine copy_from_device
  
  ! Device synchronization
  subroutine device_synchronize(device, status)
    type(unified_device), intent(in) :: device
    integer, intent(out), optional :: status
    
    integer :: sync_status
    
    sync_status = 0
    
    ! Dispatch to device-specific synchronization
    select case(trim(device%device_type))
    case("cpu")
      ! CPU operations are inherently synchronous
      sync_status = 0
    case("gpu")
      call synchronize_gpu_device(device, sync_status)
    case("neural")
      call synchronize_neural_device(device, sync_status)
    end select
    
    if (present(status)) status = sync_status
    
  end subroutine device_synchronize
  
  ! Backend initialization routines
  subroutine initialize_cpu_backend(device)
    type(unified_device), intent(inout) :: device
    
    ! CPU backend is always available
    device%available = .true.
    device%device_context = 1  ! Dummy context
    
    print '(A,A)', "  CPU backend initialized: ", trim(device%capabilities%architecture)
    
  end subroutine initialize_cpu_backend
  
  subroutine initialize_gpu_backend(device, vendor)
    type(unified_device), intent(inout) :: device
    character(len=*), intent(in) :: vendor
    
    select case(trim(vendor))
    case("amd")
      call initialize_amd_gpu_backend(device)
    case("nvidia")
      call initialize_nvidia_gpu_backend(device)
    case("intel")
      call initialize_intel_gpu_backend(device)
    case default
      device%available = .false.
    end select
    
  end subroutine initialize_gpu_backend
  
  subroutine initialize_neural_backend(device)
    type(unified_device), intent(inout) :: device
    
    select case(trim(device%capabilities%vendor))
    case("apple")
      call initialize_apple_neural_backend(device)
    case default
      device%available = .false.
    end select
    
  end subroutine initialize_neural_backend
  
  ! GPU vendor-specific initialization
  subroutine initialize_amd_gpu_backend(device)
    type(unified_device), intent(inout) :: device
    
    ! Use existing OpenGL backend
    device%available = .true.
    device%device_context = 2  ! OpenGL context ID
    
    print '(A,A)', "  AMD GPU backend initialized: ", trim(device%capabilities%architecture)
    
  end subroutine initialize_amd_gpu_backend
  
  subroutine initialize_nvidia_gpu_backend(device)
    type(unified_device), intent(inout) :: device
    
    ! TODO: Initialize CUDA backend
    device%available = .false.  ! Not implemented yet
    
    print *, "  NVIDIA GPU backend: Not implemented"
    
  end subroutine initialize_nvidia_gpu_backend
  
  subroutine initialize_intel_gpu_backend(device)
    type(unified_device), intent(inout) :: device
    
    ! TODO: Initialize Intel GPU backend (Level Zero or OpenCL)
    device%available = .false.  ! Not implemented yet
    
    print *, "  Intel GPU backend: Not implemented"
    
  end subroutine initialize_intel_gpu_backend
  
  subroutine initialize_apple_neural_backend(device)
    type(unified_device), intent(inout) :: device
    
    ! TODO: Initialize Apple Neural Engine backend
    device%available = .false.  ! Not implemented yet
    
    print *, "  Apple Neural Engine backend: Not implemented"
    
  end subroutine initialize_apple_neural_backend
  
  ! Execution implementations (stubs for now)
  subroutine execute_cpu_convolution(device, input, weights, output, &
                                    N, C, H, W, K, kernel_size, stride, pad, &
                                    H_out, W_out, status)
    type(unified_device), intent(in) :: device
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    integer, intent(out) :: status
    
    ! TODO: Call existing CPU implementation
    ! use sparkle_conv2d, only: conv2d_cpu
    ! call conv2d_cpu(input, weights, output, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    status = 0
    
  end subroutine execute_cpu_convolution
  
  subroutine execute_gpu_convolution(device, input, weights, output, &
                                    N, C, H, W, K, kernel_size, stride, pad, &
                                    H_out, W_out, async, status)
    type(unified_device), intent(in) :: device
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    logical, intent(in) :: async
    integer, intent(out) :: status
    
    ! TODO: Call existing GPU implementation
    ! use sparkle_conv2d, only: conv2d_gpu
    ! call conv2d_gpu(input, weights, output, N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    status = 0
    
  end subroutine execute_gpu_convolution
  
  subroutine execute_neural_convolution(device, input, weights, output, &
                                       N, C, H, W, K, kernel_size, stride, pad, &
                                       H_out, W_out, status)
    type(unified_device), intent(in) :: device
    real(real32), intent(in) :: input(:), weights(:)
    real(real32), intent(out) :: output(:)
    integer, intent(in) :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
    integer, intent(out) :: status
    
    ! TODO: Implement Neural Engine convolution
    status = -3  ! Not implemented
    
  end subroutine execute_neural_convolution
  
  ! Matrix multiplication stubs
  subroutine execute_cpu_matmul(device, A, B, C, M, N, K, status)
    type(unified_device), intent(in) :: device
    real(real32), intent(in) :: A(:), B(:)
    real(real32), intent(out) :: C(:)
    integer, intent(in) :: M, N, K
    integer, intent(out) :: status
    
    status = -3  ! Not implemented
  end subroutine execute_cpu_matmul
  
  subroutine execute_gpu_matmul(device, A, B, C, M, N, K, async, status)
    type(unified_device), intent(in) :: device
    real(real32), intent(in) :: A(:), B(:)
    real(real32), intent(out) :: C(:)
    integer, intent(in) :: M, N, K
    logical, intent(in) :: async
    integer, intent(out) :: status
    
    status = -3  ! Not implemented
  end subroutine execute_gpu_matmul
  
  subroutine execute_neural_matmul(device, A, B, C, M, N, K, status)
    type(unified_device), intent(in) :: device
    real(real32), intent(in) :: A(:), B(:)
    real(real32), intent(out) :: C(:)
    integer, intent(in) :: M, N, K
    integer, intent(out) :: status
    
    status = -3  ! Not implemented
  end subroutine execute_neural_matmul
  
  ! Custom kernel execution stub
  subroutine execute_gpu_custom_kernel(device, kernel_source, input_buffers, &
                                      output_buffers, params, async, status)
    type(unified_device), intent(in) :: device
    character(len=*), intent(in) :: kernel_source
    type(device_memory_buffer), intent(in) :: input_buffers(:)
    type(device_memory_buffer), intent(inout) :: output_buffers(:)
    real(real32), intent(in) :: params(:)
    logical, intent(in) :: async
    integer, intent(out) :: status
    
    status = -3  ! Not implemented
  end subroutine execute_gpu_custom_kernel
  
  ! Memory allocation stubs
  subroutine allocate_cpu_memory(buffer, size_bytes, memory_type)
    type(device_memory_buffer), intent(inout) :: buffer
    integer(int64), intent(in) :: size_bytes
    character(len=*), intent(in) :: memory_type
    
    buffer%allocated = .true.
    buffer%device_ptr = 1  ! Dummy pointer
  end subroutine allocate_cpu_memory
  
  subroutine allocate_gpu_memory(buffer, size_bytes, memory_type)
    type(device_memory_buffer), intent(inout) :: buffer
    integer(int64), intent(in) :: size_bytes
    character(len=*), intent(in) :: memory_type
    
    buffer%allocated = .true.
    buffer%device_ptr = 2  ! Dummy pointer
  end subroutine allocate_gpu_memory
  
  subroutine allocate_neural_memory(buffer, size_bytes, memory_type)
    type(device_memory_buffer), intent(inout) :: buffer
    integer(int64), intent(in) :: size_bytes
    character(len=*), intent(in) :: memory_type
    
    buffer%allocated = .true.
    buffer%device_ptr = 3  ! Dummy pointer
  end subroutine allocate_neural_memory
  
  ! Memory copy stubs
  subroutine copy_to_cpu_memory(host_data, buffer, status)
    real(real32), intent(in) :: host_data(:)
    type(device_memory_buffer), intent(in) :: buffer
    integer, intent(out) :: status
    
    status = 0  ! Success
  end subroutine copy_to_cpu_memory
  
  subroutine copy_to_gpu_memory(host_data, buffer, async, status)
    real(real32), intent(in) :: host_data(:)
    type(device_memory_buffer), intent(in) :: buffer
    logical, intent(in) :: async
    integer, intent(out) :: status
    
    status = 0  ! Success
  end subroutine copy_to_gpu_memory
  
  subroutine copy_to_neural_memory(host_data, buffer, status)
    real(real32), intent(in) :: host_data(:)
    type(device_memory_buffer), intent(in) :: buffer
    integer, intent(out) :: status
    
    status = 0  ! Success
  end subroutine copy_to_neural_memory
  
  subroutine copy_from_cpu_memory(buffer, host_data, status)
    type(device_memory_buffer), intent(in) :: buffer
    real(real32), intent(out) :: host_data(:)
    integer, intent(out) :: status
    
    status = 0  ! Success
  end subroutine copy_from_cpu_memory
  
  subroutine copy_from_gpu_memory(buffer, host_data, async, status)
    type(device_memory_buffer), intent(in) :: buffer
    real(real32), intent(out) :: host_data(:)
    logical, intent(in) :: async
    integer, intent(out) :: status
    
    status = 0  ! Success
  end subroutine copy_from_gpu_memory
  
  subroutine copy_from_neural_memory(buffer, host_data, status)
    type(device_memory_buffer), intent(in) :: buffer
    real(real32), intent(out) :: host_data(:)
    integer, intent(out) :: status
    
    status = 0  ! Success
  end subroutine copy_from_neural_memory
  
  ! Synchronization stubs
  subroutine synchronize_gpu_device(device, status)
    type(unified_device), intent(in) :: device
    integer, intent(out) :: status
    
    status = 0  ! Success
  end subroutine synchronize_gpu_device
  
  subroutine synchronize_neural_device(device, status)
    type(unified_device), intent(in) :: device
    integer, intent(out) :: status
    
    status = 0  ! Success
  end subroutine synchronize_neural_device
  
  ! Cleanup routines
  subroutine cleanup_cpu_backend(device)
    type(unified_device), intent(inout) :: device
    ! No cleanup needed for CPU
  end subroutine cleanup_cpu_backend
  
  subroutine cleanup_gpu_backend(device)
    type(unified_device), intent(inout) :: device
    ! TODO: Cleanup GPU resources
  end subroutine cleanup_gpu_backend
  
  subroutine cleanup_neural_backend(device)
    type(unified_device), intent(inout) :: device
    ! TODO: Cleanup neural engine resources
  end subroutine cleanup_neural_backend

end module sparkle_unified_device