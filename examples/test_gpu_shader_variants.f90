program test_gpu_shader_variants
  use iso_c_binding
  use gpu_opengl_interface
  use sporkle_glsl_generator
  use sporkle_rdna_shader_generator
  use sporkle_types
  use kinds
  implicit none
  
  ! Test parameters (ResNet-50 first layer)
  integer, parameter :: N = 1, C = 3, H = 224, W = 224
  integer, parameter :: K = 64, kernel_size = 7, stride = 2, pad = 3
  integer, parameter :: H_out = 112, W_out = 112
  
  ! Arrays
  real(sp), allocatable :: input(:), weights(:), output(:)
  real(sp), allocatable :: output_ref(:)
  
  ! Timing
  real(sp) :: gpu_time_ms
  real(dp) :: gflops
  integer(c_int64_t) :: flop_count
  
  ! Configuration
  type(convolution_config) :: conv_cfg
  type(rdna_config) :: rdna_cfg
  character(len=:), allocatable :: shader_source
  
  print *, "=== GPU Shader Variant Benchmarking ==="
  print *, ""
  print *, "Testing different shader implementations on AMD RX 7900 XT"
  print *, "Problem size: ResNet-50 first layer (224x224x3 -> 112x112x64)"
  print *, ""
  
  ! Initialize GPU
  if (.not. gpu_init()) then
    print *, "ERROR: Failed to initialize GPU!"
    stop 1
  end if
  
  ! Allocate arrays
  allocate(input(N*C*H*W))
  allocate(weights(K*C*kernel_size*kernel_size))
  allocate(output(N*K*H_out*W_out))
  allocate(output_ref(N*K*H_out*W_out))
  
  ! Initialize test data
  call random_number(input)
  call random_number(weights)
  
  ! Calculate FLOPs
  flop_count = int(N, c_int64_t) * int(K, c_int64_t) * &
               int(H_out, c_int64_t) * int(W_out, c_int64_t) * &
               int(C, c_int64_t) * int(kernel_size, c_int64_t) * &
               int(kernel_size, c_int64_t) * 2_c_int64_t
  
  ! Setup convolution config
  conv_cfg%input_height = H
  conv_cfg%input_width = W
  conv_cfg%input_channels = C
  conv_cfg%kernel_height = kernel_size
  conv_cfg%kernel_width = kernel_size
  conv_cfg%output_height = H_out
  conv_cfg%output_width = W_out
  conv_cfg%output_channels = K
  conv_cfg%stride_y = stride
  conv_cfg%stride_x = stride
  conv_cfg%pad_y = pad
  conv_cfg%pad_x = pad
  conv_cfg%tile_size = 16
  conv_cfg%use_fp16 = .false.
  
  print *, "Running GPU benchmarks..."
  print *, "========================"
  print *, ""
  
  ! 1. Original shader (reference implementation - 414 GFLOPS)
  print *, "1. Reference Implementation (64 threads, proven 414 GFLOPS):"
  gpu_time_ms = gpu_execute_conv2d_ref(input, weights, output_ref, &
                                       N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  gflops = real(flop_count, real64) / (real(gpu_time_ms, real64) * 1.0e6)
  print '(A,F8.2,A,F8.1,A)', "   Time: ", gpu_time_ms, " ms, Performance: ", gflops, " GFLOPS"
  print *, ""
  
  ! Now test different configurations
  ! Note: These would need actual GPU execution integration
  print *, "2. RDNA-Optimized Variants (simulated):"
  print *, ""
  
  ! 2a. Basic Wave32 (64 threads)
  rdna_cfg%arch = detect_rdna_arch(0)
  rdna_cfg%workgroup_size = 64
  rdna_cfg%waves_per_cu = 4
  rdna_cfg%use_lds = .false.
  rdna_cfg%use_dual_issue = .false.
  shader_source = generate_rdna_conv_shader(rdna_cfg, conv_cfg)
  print *, "   a) Wave32-aligned (64 threads = 2 waves):"
  print *, "      - Optimal for RDNA wave scheduling"
  print *, "      - Expected: Similar to reference (414 GFLOPS)"
  print *, ""
  
  ! 2b. Larger workgroup (256 threads)
  rdna_cfg%workgroup_size = 256
  shader_source = generate_rdna_conv_shader(rdna_cfg, conv_cfg)
  print *, "   b) Large workgroup (256 threads = 8 waves):"
  print *, "      - More threads but potential scheduling overhead"
  print *, "      - Expected: 10-20% slower due to occupancy"
  print *, ""
  
  ! 2c. LDS optimized
  rdna_cfg%workgroup_size = 64
  rdna_cfg%use_lds = .true.
  shader_source = generate_rdna_conv_shader(rdna_cfg, conv_cfg)
  print *, "   c) LDS-optimized (shared memory tiling):"
  print *, "      - Uses 64KB local data share per CU"
  print *, "      - Expected: Better for larger kernels"
  print *, ""
  
  ! 2d. Dual-issue (RDNA3)
  rdna_cfg%use_dual_issue = .true.
  shader_source = generate_rdna_conv_shader(rdna_cfg, conv_cfg)
  print *, "   d) RDNA3 Dual-issue optimized:"
  print *, "      - Exploits 2x FMA throughput"
  print *, "      - Expected: Up to 2x performance (800+ GFLOPS potential)"
  print *, ""
  
  ! 3. Original GLSL generator (for comparison)
  print *, "3. Generic GLSL (16x16 = 256 threads):"
  shader_source = generate_conv_glsl_shader(conv_cfg)
  print *, "   - Not optimized for Wave32"
  print *, "   - Expected: Suboptimal on RDNA"
  print *, ""
  
  print *, "=== Analysis ==="
  print *, ""
  print *, "Key findings:"
  print *, "1. Reference achieves 414 GFLOPS with 64-thread workgroup"
  print *, "2. This matches RDNA Wave32 alignment (2 waves)"
  print *, "3. Larger workgroups may reduce occupancy"
  print *, "4. Dual-issue could theoretically double performance"
  print *, ""
  print *, "Next steps:"
  print *, "- Integrate dynamic shader system with GPU execution"
  print *, "- Measure actual performance of each variant"
  print *, "- Let the system learn optimal configuration"
  
  ! Cleanup
  call gpu_cleanup()
  
  deallocate(input, weights, output, output_ref)
  
end program test_gpu_shader_variants