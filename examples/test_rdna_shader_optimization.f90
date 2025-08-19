program test_rdna_shader_optimization
  use iso_c_binding
  use sporkle_rdna_shader_generator
  use sporkle_glsl_generator
  implicit none
  
  type(rdna_config) :: rdna_cfg
  type(convolution_config) :: conv_cfg
  character(len=:), allocatable :: shader_original
  character(len=:), allocatable :: shader_rdna_optimized
  character(len=:), allocatable :: shader_rdna_dual_issue
  
  print *, "=== RDNA Shader Optimization Test ==="
  print *, ""
  print *, "Applying the same architectural primitive as CPU SIMD:"
  print *, "Match the hardware's native execution width!"
  print *, ""
  
  ! Setup convolution parameters (ResNet-50 first layer)
  conv_cfg%input_height = 224
  conv_cfg%input_width = 224
  conv_cfg%input_channels = 3
  conv_cfg%kernel_height = 7
  conv_cfg%kernel_width = 7
  conv_cfg%output_height = 112
  conv_cfg%output_width = 112
  conv_cfg%output_channels = 64
  conv_cfg%stride_y = 2
  conv_cfg%stride_x = 2
  conv_cfg%pad_y = 3
  conv_cfg%pad_x = 3
  conv_cfg%tile_size = 16
  conv_cfg%use_fp16 = .false.
  
  ! Generate original shader (not RDNA-optimized)
  print *, "1. Original GLSL Generator (GCN-style):"
  shader_original = generate_conv_glsl_shader(conv_cfg)
  print *, "   - Workgroup size: 16x16 = 256 threads"
  print *, "   - Waves per workgroup: 256/64 = 4 (assuming Wave64)"
  print *, "   - Not optimized for RDNA Wave32"
  print *, ""
  
  ! Setup RDNA3 configuration
  rdna_cfg%arch = detect_rdna_arch(0)  ! 7900 XT
  
  ! Generate RDNA-optimized shader (matches reference: 64 threads)
  print *, "2. RDNA-Optimized (matches 414 GFLOPS reference):"
  rdna_cfg%workgroup_size = 64
  rdna_cfg%waves_per_cu = 4
  rdna_cfg%use_lds = .false.
  rdna_cfg%use_dual_issue = .false.
  rdna_cfg%vgpr_usage = 32
  
  shader_rdna_optimized = generate_rdna_conv_shader(rdna_cfg, conv_cfg)
  print *, "   - Workgroup size: 64 threads"
  print *, "   - Waves per workgroup: 64/32 = 2 (RDNA Wave32)"
  print *, "   - Matches working reference implementation"
  print *, ""
  
  ! Generate RDNA3 dual-issue optimized shader
  print *, "3. RDNA3 Dual-Issue Optimized:"
  rdna_cfg%use_dual_issue = .true.
  shader_rdna_dual_issue = generate_rdna_conv_shader(rdna_cfg, conv_cfg)
  print *, "   - Workgroup size: 64 threads"
  print *, "   - Dual-issue FMA exploitation"
  print *, "   - Theoretical 2x throughput on RDNA3"
  print *, ""
  
  ! Show key differences
  print *, "=== Key Architectural Insights ==="
  print *, ""
  print *, "GCN (Vega, older AMD):"
  print *, "  - Wave64: 64 threads per wavefront"
  print *, "  - Workgroup size typically 64, 128, 256"
  print *, ""
  print *, "RDNA1/2/3 (RX 5000/6000/7000):"
  print *, "  - Wave32: 32 threads per wavefront"
  print *, "  - Better cache utilization with smaller waves"
  print *, "  - RDNA3 adds dual-issue capability"
  print *, ""
  print *, "Our Discovery:"
  print *, "  - Same principle as CPU SIMD: match hardware width"
  print *, "  - CPU: AVX-512 = 16 floats/instruction"
  print *, "  - GPU: Wave32 = 32 threads/wave"
  print *, "  - Optimal workgroup = small multiple of wave size"
  print *, ""
  
  ! Save shaders for inspection
  call save_shader("shader_original.glsl", shader_original)
  call save_shader("shader_rdna_optimized.glsl", shader_rdna_optimized)
  call save_shader("shader_rdna_dual_issue.glsl", shader_rdna_dual_issue)
  
  print *, "Shaders saved for inspection:"
  print *, "  - shader_original.glsl"
  print *, "  - shader_rdna_optimized.glsl"
  print *, "  - shader_rdna_dual_issue.glsl"
  print *, ""
  
  print *, "Next steps:"
  print *, "1. Integrate RDNA shader generator with GPU execution"
  print *, "2. Benchmark different configurations"
  print *, "3. Apply same optimization discovery to other kernels"
  
contains

  subroutine save_shader(filename, shader_source)
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: shader_source
    integer :: unit
    
    open(newunit=unit, file=filename, status='replace')
    write(unit, '(A)') shader_source
    close(unit)
  end subroutine save_shader

end program test_rdna_shader_optimization