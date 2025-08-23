module cpu_device_module
  use sporkle_types
  use iso_c_binding
  use kinds
  implicit none
  private
  
  ! CPU-specific device implementation
  type, extends(compute_device), public :: cpu_device
    integer :: num_threads = 1
    logical :: has_avx2 = .false.
    logical :: has_avx512 = .false.
    logical :: has_fma = .false.
  contains
    procedure :: allocate => cpu_allocate
    procedure :: deallocate => cpu_deallocate
    procedure :: memcpy => cpu_memcpy
    procedure :: execute => cpu_execute
    procedure :: synchronize => cpu_synchronize
    procedure :: get_info => cpu_get_info
    procedure :: detect_features => cpu_detect_features
  end type cpu_device
  
  ! Constructor
  interface cpu_device
    module procedure create_cpu_device
  end interface cpu_device
  
contains
  
  ! Create and initialize a CPU device
  function create_cpu_device(device_id) result(device)
    integer, intent(in), optional :: device_id
    type(cpu_device) :: device
    
    ! Set basic properties
    if (present(device_id)) then
      device%device_id = device_id
    else
      device%device_id = 0  ! Default CPU is device 0
    end if
    
    device%device_type = DEVICE_CPU
    device%name = "CPU"
    device%is_available = .true.
    
    ! Detect CPU features and capabilities
    call device%detect_features()
    call device%get_info()
    
  end function create_cpu_device
  
  ! Allocate memory on CPU (cache-line aligned)
  function cpu_allocate(self, size_bytes, pinned) result(buffer)
    class(cpu_device), intent(inout) :: self
    integer(i64), intent(in) :: size_bytes
    logical, intent(in), optional :: pinned
    type(sporkle_buffer) :: buffer
    logical :: is_pinned
    
    interface
      ! C function for aligned allocation
      function posix_memalign(memptr, alignment, size) bind(C, name="posix_memalign")
        import :: c_ptr, c_size_t, c_int
        type(c_ptr), intent(out) :: memptr
        integer(c_size_t), value :: alignment
        integer(c_size_t), value :: size
        integer(c_int) :: posix_memalign
      end function posix_memalign
    end interface
    
    integer(c_int) :: status
    integer(c_size_t), parameter :: CACHE_LINE_SIZE = 64
    
    is_pinned = .false.
    if (present(pinned)) is_pinned = pinned
    
    ! Allocate cache-line aligned memory
    status = posix_memalign(buffer%data, CACHE_LINE_SIZE, int(size_bytes, c_size_t))
    
    if (status == 0) then
      buffer%size_bytes = size_bytes
      buffer%owning_device = self%device_id
      buffer%is_pinned = is_pinned
    else
      buffer%data = c_null_ptr
      buffer%size_bytes = 0
    end if
    
  end function cpu_allocate
  
  ! Deallocate CPU memory
  subroutine cpu_deallocate(self, buffer)
    class(cpu_device), intent(inout) :: self
    type(sporkle_buffer), intent(inout) :: buffer
    
    interface
      subroutine free(ptr) bind(C, name="free")
        import :: c_ptr
        type(c_ptr), value :: ptr
      end subroutine free
    end interface
    
    if (c_associated(buffer%data)) then
      call free(buffer%data)
      buffer%data = c_null_ptr
      buffer%size_bytes = 0
      buffer%owning_device = -1
    end if
    
  end subroutine cpu_deallocate
  
  ! Memory copy (optimized for CPU)
  function cpu_memcpy(self, dst, src, size_bytes) result(status)
    class(cpu_device), intent(inout) :: self
    type(sporkle_buffer), intent(inout) :: dst
    type(sporkle_buffer), intent(in) :: src
    integer(i64), intent(in) :: size_bytes
    integer(i32) :: status
    
    interface
      subroutine memcpy(dst, src, n) bind(C, name="memcpy")
        import :: c_ptr, c_size_t
        type(c_ptr), value :: dst, src
        integer(c_size_t), value :: n
      end subroutine memcpy
    end interface
    
    status = SPORKLE_ERROR
    
    ! Validate buffers
    if (.not. c_associated(dst%data) .or. .not. c_associated(src%data)) return
    if (size_bytes > dst%size_bytes .or. size_bytes > src%size_bytes) return
    
    ! Use optimized memcpy
    call memcpy(dst%data, src%data, int(size_bytes, c_size_t))
    status = SPORKLE_SUCCESS
    
  end function cpu_memcpy
  
  ! Execute kernel on CPU
  function cpu_execute(self, kernel_name, args, grid_size, block_size) result(status)
    use cpu_conv2d_adaptive, only: conv2d_adaptive
    use gemm_simd_optimized_v2, only: gemm_simd_avx512_v2
    use gemm_simd_prefetch, only: gemm_simd_avx512_prefetch
    use gemm_simd_streaming, only: gemm_simd_streaming_large
    use universal_memory_optimization, only: gemm_universal_memory

    class(cpu_device), intent(inout) :: self
    character(len=*), intent(in) :: kernel_name
    type(c_ptr), intent(in) :: args(:)
    integer(i32), intent(in) :: grid_size(3)
    integer(i32), intent(in) :: block_size(3)
    integer(i32) :: status
    
    
    status = SPORKLE_ERROR
    
    select case(trim(kernel_name))
    case("conv2d", "convolution")
      ! CPU convolution using adaptive blocking
      status = dispatch_conv2d(args)
      
    case("gemm", "matmul")
      ! Basic GEMM using universal memory optimization
      status = dispatch_gemm(args)
      
    case("gemm_simd", "matmul_simd")
      ! SIMD-optimized GEMM
      status = dispatch_gemm_simd(args)
      
    case("gemm_prefetch", "matmul_prefetch")
      ! SIMD + prefetch optimized GEMM
      status = dispatch_gemm_prefetch(args)
      
    case("gemm_streaming", "matmul_streaming", "gemm_large", "matmul_large")
      ! Streaming (non-temporal) GEMM for huge matrices
      status = dispatch_gemm_streaming(args)
      
    case("vector_add", "saxpy")
      ! Simple vector operations
      status = dispatch_vector_op(args, "add")
      
    case default
      print *, "❌ Unknown kernel: ", trim(kernel_name)
      status = SPORKLE_ERROR
    end select
    
  contains
    
    ! Dispatch convolution 
    function dispatch_conv2d(args) result(status)
      type(c_ptr), intent(in) :: args(:)
      integer :: status
      real(sp), pointer :: input(:), weights(:), output(:)
      integer, pointer :: N, C, H, W, K, kernel_size, stride, pad, H_out, W_out
      real(dp) :: time_ms
      
      if (size(args) < 10) then
        print *, "❌ Conv2D requires 10 arguments"
        status = SPORKLE_ERROR
        return
      end if
      
      ! Extract pointers
      call c_f_pointer(args(1), input, [1])  ! Will be resized in conv2d_adaptive
      call c_f_pointer(args(2), weights, [1])
      call c_f_pointer(args(3), output, [1])
      call c_f_pointer(args(4), N)
      call c_f_pointer(args(5), C)
      call c_f_pointer(args(6), H)
      call c_f_pointer(args(7), W)
      call c_f_pointer(args(8), K)
      call c_f_pointer(args(9), kernel_size)
      call c_f_pointer(args(10), stride)
      ! Add more as needed
      
      ! Call optimized Conv2D (returns time in ms)
      time_ms = conv2d_adaptive(input, weights, output, &
                               N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
      
      status = SPORKLE_SUCCESS
    end function dispatch_conv2d
    
    ! Dispatch basic GEMM
    function dispatch_gemm(args) result(status)
      type(c_ptr), intent(in) :: args(:)
      integer :: status
      real(sp), pointer :: A(:), B(:), C(:)
      integer, pointer :: M, N, K
      real(sp), pointer :: alpha, beta
      
      if (size(args) < 8) then
        print *, "❌ GEMM requires 8 arguments"
        status = SPORKLE_ERROR
        return
      end if
      
      ! Extract dimensions first
      call c_f_pointer(args(4), M)
      call c_f_pointer(args(5), N)
      call c_f_pointer(args(6), K)
      call c_f_pointer(args(7), alpha)
      call c_f_pointer(args(8), beta)
      
      ! Now extract matrices with proper sizes
      call c_f_pointer(args(1), A, [M*K])
      call c_f_pointer(args(2), B, [K*N])
      call c_f_pointer(args(3), C, [M*N])
      
      ! Call universal memory optimization GEMM
      call gemm_universal_memory(A, B, C, M, N, K, alpha, beta)
      
      status = SPORKLE_SUCCESS
    end function dispatch_gemm
    
    ! Dispatch SIMD-optimized GEMM
    function dispatch_gemm_simd(args) result(status)
      type(c_ptr), intent(in) :: args(:)
      integer :: status
      real(sp), pointer :: A(:), B(:), C(:)
      integer, pointer :: M, N, K
      real(sp), pointer :: alpha, beta
      
      if (size(args) < 8) then
        print *, "❌ GEMM SIMD requires 8 arguments"
        status = SPORKLE_ERROR
        return
      end if
      
      ! Extract dimensions first
      call c_f_pointer(args(4), M)
      call c_f_pointer(args(5), N)
      call c_f_pointer(args(6), K)
      call c_f_pointer(args(7), alpha)
      call c_f_pointer(args(8), beta)
      
      ! Now extract matrices with proper sizes
      call c_f_pointer(args(1), A, [M*K])
      call c_f_pointer(args(2), B, [K*N])
      call c_f_pointer(args(3), C, [M*N])
      
      ! Call SIMD-optimized GEMM
      call gemm_simd_avx512_v2(A, B, C, M, N, K, alpha, beta)
      
      status = SPORKLE_SUCCESS
    end function dispatch_gemm_simd
    
    ! Dispatch prefetch-optimized GEMM
    function dispatch_gemm_prefetch(args) result(status)
      type(c_ptr), intent(in) :: args(:)
      integer :: status
      real(sp), pointer :: A(:), B(:), C(:)
      integer, pointer :: M, N, K
      real(sp), pointer :: alpha, beta
      
      if (size(args) < 8) then
        print *, "❌ GEMM prefetch requires 8 arguments"
        status = SPORKLE_ERROR
        return
      end if
      
      ! Extract dimensions first
      call c_f_pointer(args(4), M)
      call c_f_pointer(args(5), N)
      call c_f_pointer(args(6), K)
      call c_f_pointer(args(7), alpha)
      call c_f_pointer(args(8), beta)
      
      ! Now extract matrices with proper sizes
      call c_f_pointer(args(1), A, [M*K])
      call c_f_pointer(args(2), B, [K*N])
      call c_f_pointer(args(3), C, [M*N])
      
      ! Call prefetch-optimized GEMM
      call gemm_simd_avx512_prefetch(A, B, C, M, N, K, alpha, beta)
      
      status = SPORKLE_SUCCESS
    end function dispatch_gemm_prefetch
    
    ! Dispatch streaming GEMM for huge matrices
    function dispatch_gemm_streaming(args) result(status)
      type(c_ptr), intent(in) :: args(:)
      integer :: status
      real(sp), pointer :: A(:), B(:), C(:)
      integer, pointer :: M, N, K
      real(sp), pointer :: alpha, beta
      
      print *, "🔥 CPU DEVICE: Dispatching streaming GEMM!"
      
      if (size(args) < 8) then
        print *, "❌ GEMM streaming requires 8 arguments"
        status = SPORKLE_ERROR
        return
      end if
      
      ! Extract dimensions first
      call c_f_pointer(args(4), M)
      call c_f_pointer(args(5), N)
      call c_f_pointer(args(6), K)
      call c_f_pointer(args(7), alpha)
      call c_f_pointer(args(8), beta)
      
      ! Now extract matrices with proper sizes
      call c_f_pointer(args(1), A, [M*K])
      call c_f_pointer(args(2), B, [K*N])
      call c_f_pointer(args(3), C, [M*N])
      
      ! Call streaming GEMM
      call gemm_simd_streaming_large(A, B, C, M, N, K, alpha, beta)
      
      status = SPORKLE_SUCCESS
    end function dispatch_gemm_streaming
    
    ! Dispatch vector operations
    function dispatch_vector_op(args, op) result(status)
      type(c_ptr), intent(in) :: args(:)
      character(len=*), intent(in) :: op
      integer :: status
      
      ! Placeholder for vector operations
      print *, "🔧 Vector operation not implemented: ", op
      status = SPORKLE_ERROR
    end function dispatch_vector_op
    
  end function cpu_execute
  
  ! CPU synchronization (no-op for CPU)
  function cpu_synchronize(self) result(status)
    class(cpu_device), intent(inout) :: self
    integer(i32) :: status
    
    ! CPU operations are synchronous
    status = SPORKLE_SUCCESS
    
  end function cpu_synchronize
  
  ! Get CPU information
  subroutine cpu_get_info(self)
    class(cpu_device), intent(inout) :: self
    character(len=32) :: env_value
    integer :: env_status
    
    ! Get actual number of CPU cores from /proc/cpuinfo
    block
      integer :: unit, iostat, physical_cores, logical_cores
      character(len=256) :: line, cpu_model
      logical :: found_model
      
      logical_cores = 0
      physical_cores = 0
      found_model = .false.
      
      open(newunit=unit, file="/proc/cpuinfo", status="old", action="read", iostat=iostat)
      if (iostat == 0) then
        do
          read(unit, '(A)', iostat=iostat) line
          if (iostat /= 0) exit
          
          ! Count logical processors
          if (index(line, "processor") == 1) then
            logical_cores = logical_cores + 1
          end if
          
          ! Get CPU model name
          if (.not. found_model .and. index(line, "model name") > 0) then
            cpu_model = adjustl(line(index(line, ":") + 2:))
            found_model = .true.
          end if
          
          ! Count physical cores
          if (index(line, "cpu cores") > 0) then
            read(line(index(line, ":") + 2:), *, iostat=iostat) physical_cores
          end if
        end do
        close(unit)
        
        self%num_threads = max(1, logical_cores)
        
        ! Update device name with actual CPU model
        if (found_model .and. allocated(self%name)) then
          deallocate(self%name)
          self%name = trim(cpu_model)
        end if
      else
        ! Fallback to environment variable
        call get_environment_variable("OMP_NUM_THREADS", env_value, status=env_status)
        if (env_status == 0 .and. len_trim(env_value) > 0) then
          read(env_value, *) self%num_threads
        else
          self%num_threads = 1
        end if
      end if
    end block
    
    ! Update capabilities
    self%capabilities%compute_units = self%num_threads
    self%capabilities%supports_float64 = .true.
    self%capabilities%supports_unified_memory = .true.
    self%capabilities%clock_speed_ghz = 2.5  ! Typical modern CPU
    
    ! Get actual system memory from /proc/meminfo
    block
      integer :: unit, iostat
      character(len=256) :: line
      integer(i64) :: mem_kb
      
      open(newunit=unit, file="/proc/meminfo", status="old", action="read", iostat=iostat)
      if (iostat == 0) then
        do
          read(unit, '(A)', iostat=iostat) line
          if (iostat /= 0) exit
          
          if (index(line, "MemTotal:") == 1) then
            read(line(10:), *, iostat=iostat) mem_kb
            if (iostat == 0) then
              self%capabilities%memory_bytes = mem_kb * 1024_int64
            end if
            exit
          end if
        end do
        close(unit)
      else
        ! Fallback to 8GB default
        self%capabilities%memory_bytes = 8_int64 * 1024 * 1024 * 1024
      end if
    end block
    
    ! Set instruction set string
    if (self%has_avx512) then
      self%capabilities%instruction_set = "AVX512"
    else if (self%has_avx2) then
      self%capabilities%instruction_set = "AVX2"
    else
      self%capabilities%instruction_set = "SSE"
    end if
    
    ! Build device name
    if (allocated(self%name)) deallocate(self%name)
    allocate(character(len=50) :: self%name)
    write(self%name, '(A,I0,A)') "CPU (", self%num_threads, " threads)"
    
  end subroutine cpu_get_info
  
  ! Detect CPU features (simplified - would use CPUID in production)
  subroutine cpu_detect_features(self)
    class(cpu_device), intent(inout) :: self
    
    ! For now, assume modern CPU with AVX2
    ! In production, would use CPUID instruction
    self%has_avx2 = .true.
    self%has_fma = .true.
    self%has_avx512 = .false.
    
    ! Update capabilities based on features
    if (self%has_avx2) then
      self%capabilities%supports_float16 = .true.  ! F16C
    end if
    
  end subroutine cpu_detect_features
  
end module cpu_device_module