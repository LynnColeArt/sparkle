module sporkle_autotuner_enhanced
  ! Enhanced Auto-tuner with Mini's AMD Optimizations
  ! =================================================
  !
  ! Based on pod Claude's autotuner with corrections:
  ! - Wave32 support for RDNA3
  ! - Proper block size selection (256 threads)
  ! - Accurate performance calculations
  ! - Ko-blocking and LDS optimizations
  
  use kinds
  implicit none
  
  type :: tunable_parameters
    ! Wave/Warp configuration
    integer :: wave_size = 32              ! 32 for RDNA3, 64 for GCN/CDNA
    logical :: force_wave_mode = .false.   ! Let driver choose if false
    
    ! Thread block configuration  
    integer :: block_x = 16
    integer :: block_y = 16 
    integer :: block_z = 1
    integer :: total_threads = 256        ! Mini's recommendation
    
    ! Tiling parameters
    integer :: tile_m = 32                 ! Output tile height
    integer :: tile_n = 32                 ! Output tile width  
    integer :: tile_k = 64                 ! Ko blocking factor
    logical :: use_padding = .true.        ! Pad LDS to avoid conflicts
    integer :: pad_size = 1                ! Padding elements
    
    ! Work distribution
    integer :: outputs_x = 4               ! Outputs per thread X
    integer :: outputs_y = 4               ! Outputs per thread Y
    
    ! Memory and unrolling
    integer :: unroll_factor = 12          ! Inner loop unroll
    logical :: use_vec4 = .true.           ! Vectorized loads
    logical :: persistent_buffer = .true.   ! Keep buffers mapped
    integer :: ring_depth = 4              ! Async buffer depth
    
    ! Algorithm selection
    logical :: use_direct_conv = .true.    ! vs implicit GEMM
    logical :: use_winograd = .false.      ! F(2,3) for 3x3
    logical :: use_fft = .false.           ! For large kernels
    
    ! Device-specific flags
    logical :: is_rdna3 = .false.
    logical :: has_dual_issue = .false.
    logical :: has_matrix_cores = .false.
  end type tunable_parameters
  
  type :: performance_model
    real(dp) :: measured_gflops = 0.0_dp
    real(dp) :: bandwidth_gbs = 0.0_dp
    real(dp) :: arithmetic_intensity = 0.0_dp
    real(dp) :: occupancy = 0.0_dp
    integer :: active_warps_per_sm = 0
    integer :: register_usage = 0
    integer :: lds_usage_bytes = 0
    logical :: is_compute_bound = .false.
  end type performance_model
  
contains

  function calculate_arithmetic_intensity(params, C, K, H_out, W_out) result(ai)
    type(tunable_parameters), intent(in) :: params
    integer, intent(in) :: C, K, H_out, W_out
    real(dp) :: ai
    
    integer(i64) :: bytes_moved, flops
    integer :: tile_inputs, tile_outputs, tile_weights
    
    ! For tiled computation with Ko blocking
    ! Input tile: (tile_m + 2) × (tile_n + 2) × C × 4 bytes (with halo)
    tile_inputs = (params%tile_m + 2) * (params%tile_n + 2) * C * 4
    
    ! Weight tile: tile_k × C × 9 × 4 bytes (for 3x3)
    tile_weights = params%tile_k * C * 9 * 4
    
    ! Output tile: tile_m × tile_n × tile_k × 4 bytes
    tile_outputs = params%tile_m * params%tile_n * params%tile_k * 4
    
    ! Total bytes per tile (read once due to LDS reuse)
    bytes_moved = tile_inputs + tile_weights + tile_outputs
    
    ! FLOPs per tile
    flops = int(params%tile_m, i64) * params%tile_n * params%tile_k * C * 9 * 2
    
    ! Arithmetic intensity
    ai = real(flops, dp) / real(bytes_moved, dp)
    
  end function calculate_arithmetic_intensity
  
  function select_optimal_config(device_name, C, K, H, W) result(params)
    character(len=*), intent(in) :: device_name
    integer, intent(in) :: C, K, H, W
    type(tunable_parameters) :: params
    
    ! Base configuration
    params%total_threads = 256  ! Mini's universal recommendation
    
    select case(device_name)
    case("AMD Radeon RX 7900 XT", "AMD Radeon RX 7900 XTX")
      ! RDNA3 specific
      params%wave_size = 32
      params%is_rdna3 = .true.
      params%has_dual_issue = .true.
      params%block_x = 16
      params%block_y = 16
      params%tile_k = 64        ! Ko blocking
      params%unroll_factor = 12
      params%use_vec4 = .true.
      params%outputs_x = 4
      params%outputs_y = 4
      
    case("NVIDIA RTX 4090", "NVIDIA RTX 4080")
      ! Ada Lovelace
      params%wave_size = 32     ! Warp size
      params%has_matrix_cores = .true.
      params%block_x = 32
      params%block_y = 8
      params%tile_k = 32
      params%unroll_factor = 8
      
    case("AMD Radeon RX 6900 XT")
      ! RDNA2 - prefers Wave64
      params%wave_size = 64
      params%block_x = 32
      params%block_y = 8
      params%tile_k = 32
      
    case default
      ! Conservative defaults
      params%wave_size = 32
      params%block_x = 16
      params%block_y = 16
      params%tile_k = 32
    end select
    
    ! Adjust for small convolutions
    if (H < 64 .or. W < 64) then
      params%tile_m = 16
      params%tile_n = 16
      params%outputs_x = 2
      params%outputs_y = 2
    end if
    
    ! Check if we're compute bound
    block
      real(dp) :: ai, device_balance
      integer :: H_out, W_out
      
      H_out = H - 2  ! Assuming 3x3 kernel
      W_out = W - 2
      
      ai = calculate_arithmetic_intensity(params, C, K, H_out, W_out)
      
      ! Device balance (FLOP/byte)
      ! AMD 7900 XT: 27 TFLOPS / 960 GB/s = 28 FLOP/byte
      ! But with dual-issue: ~40 TFLOPS / 960 GB/s = 42 FLOP/byte
      if (params%is_rdna3 .and. params%has_dual_issue) then
        device_balance = 42.0_dp
      else
        device_balance = 28.0_dp
      end if
      
      if (ai > device_balance) then
        print '(A,F6.1,A,F6.1,A)', "Compute bound! AI=", ai, " > ", device_balance, " FLOP/byte"
      else
        print '(A,F6.1,A,F6.1,A)', "Memory bound. AI=", ai, " < ", device_balance, " FLOP/byte"
      end if
    end block
    
  end function select_optimal_config
  
  subroutine auto_tune_conv2d_sweep(best_config, C, K, H, W)
    type(tunable_parameters), intent(out) :: best_config
    integer, intent(in) :: C, K, H, W
    
    type(tunable_parameters) :: test_config
    real(dp) :: best_perf, test_perf
    integer :: wave_sizes(2) = [32, 64]
    integer :: block_configs(3,2) = reshape([16,16, 32,8, 8,32], [3,2])
    integer :: tile_k_sizes(3) = [32, 64, 128]
    integer :: unroll_factors(3) = [8, 12, 16]
    integer :: i, j, kt, l
    
    best_perf = 0.0_dp
    
    print *, "Auto-tuning convolution parameters..."
    print '(A,I0,A,I0,A,I0,A,I0)', "  Input: C=", C, " K=", K, " H=", H, " W=", W
    
    ! Sweep through configurations
    do i = 1, size(wave_sizes)
      test_config%wave_size = wave_sizes(i)
      
      do j = 1, size(block_configs, 1)
        test_config%block_x = block_configs(j, 1)
        test_config%block_y = block_configs(j, 2)
        test_config%total_threads = test_config%block_x * test_config%block_y
        
        ! Skip if not 256 threads (Mini's recommendation)
        if (test_config%total_threads /= 256) cycle
        
        do kt = 1, size(tile_k_sizes)
          test_config%tile_k = tile_k_sizes(kt)
          
          do l = 1, size(unroll_factors)
            test_config%unroll_factor = unroll_factors(l)
            
            ! Simulate performance (in real implementation, run kernel)
            test_perf = simulate_performance(test_config, C, K, H, W)
            
            if (test_perf > best_perf) then
              best_perf = test_perf
              best_config = test_config
              
              print '(A,F8.1,A)', "  New best: ", best_perf, " GFLOPS"
              print '(A,I0,A,I0,A,I0)', "    Config: wave=", test_config%wave_size, &
                    " block=", test_config%block_x, "x", test_config%block_y
              print '(A,I0,A,I0)', "    Tile_k=", test_config%tile_k, &
                    " unroll=", test_config%unroll_factor
            end if
          end do
        end do
      end do
    end do
    
    print *, "Auto-tuning complete!"
    
  end subroutine auto_tune_conv2d_sweep
  
  function simulate_performance(config, C, K, H, W) result(gflops)
    type(tunable_parameters), intent(in) :: config
    integer, intent(in) :: C, K, H, W
    real(dp) :: gflops
    
    real(dp) :: ai, occupancy, efficiency
    integer :: H_out, W_out
    integer(i64) :: total_flops
    
    H_out = H - 2
    W_out = W - 2
    
    ! Calculate arithmetic intensity
    ai = calculate_arithmetic_intensity(config, C, K, H_out, W_out)
    
    ! Estimate occupancy (simplified)
    ! Better with 256 threads and Wave32 on RDNA3
    if (config%wave_size == 32 .and. config%total_threads == 256) then
      occupancy = 0.85_dp  ! Good occupancy
    else
      occupancy = 0.65_dp  ! Lower occupancy
    end if
    
    ! Efficiency based on configuration
    efficiency = occupancy
    
    ! Boost for optimal unrolling
    if (config%unroll_factor >= 12 .and. config%unroll_factor <= 16) then
      efficiency = efficiency * 1.1_dp
    end if
    
    ! Boost for Ko blocking
    if (config%tile_k == 64) then
      efficiency = efficiency * 1.15_dp
    end if
    
    ! Cap at realistic efficiency
    efficiency = min(efficiency, 0.75_dp)
    
    ! Calculate performance
    ! Assuming 40 TFLOPS peak for 7900 XT with dual-issue
    gflops = 40000.0_dp * efficiency
    
  end function simulate_performance

end module sporkle_autotuner_enhanced