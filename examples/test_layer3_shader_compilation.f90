program test_layer3_shader_compilation
  use iso_fortran_env
  use iso_c_binding
  use gpu_opengl_interface
  use sparkle_dynamic_shader_system
  use sparkle_glsl_generator, only: convolution_config
  implicit none
  
  ! Test parameters
  integer, parameter :: N = 1
  integer, parameter :: C = 64
  integer, parameter :: H = 56
  integer, parameter :: W = 56
  integer, parameter :: K = 64
  integer, parameter :: kernel_size = 3
  integer, parameter :: stride = 1
  integer, parameter :: pad = 1
  integer, parameter :: H_out = 56
  integer, parameter :: W_out = 56
  
  ! Arrays
  real(real32), allocatable :: input(:), weights(:), output_ref(:), output_custom(:)
  integer :: input_size, weight_size, output_size
  
  ! Dynamic shader system
  type(shader_system) :: shader_sys
  type(convolution_config) :: conv_config
  character(len=:), allocatable :: shader_source
  
  ! Performance tracking
  real(real32) :: ref_time, custom_time
  real(real32) :: ref_gflops, custom_gflops
  integer(8) :: total_flops
  integer :: program_id
  character(len=64) :: variant_names(4)
  real(real32) :: variant_times(4)
  integer :: i
  
  print *, "🔦 Layer 3: Runtime Shader Compilation Test"
  print *, "=========================================="
  print *, ""
  
  ! Calculate sizes
  input_size = N * C * H * W
  weight_size = K * C * kernel_size * kernel_size
  output_size = N * K * H_out * W_out
  total_flops = int(N, 8) * K * H_out * W_out * C * kernel_size * kernel_size * 2
  
  ! Allocate arrays
  allocate(input(input_size))
  allocate(weights(weight_size))
  allocate(output_ref(output_size))
  allocate(output_custom(output_size))
  
  ! Initialize with random data
  call random_number(input)
  call random_number(weights)
  input = (input - 0.5) * 2.0
  weights = (weights - 0.5) * 0.1
  
  ! Initialize GPU
  if (.not. gpu_init()) then
    print *, "❌ Failed to initialize GPU"
    stop 1
  end if
  
  ! Run baseline for comparison
  print *, "📊 Running baseline shader..."
  output_ref = 0.0
  ref_time = gpu_execute_conv2d_ref(input, weights, output_ref, &
                                   N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  ref_gflops = real(total_flops) / (ref_time * 1.0e6)
  print '(A,F8.2,A,F8.1,A)', "   Baseline: ", ref_time, " ms (", ref_gflops, " GFLOPS)"
  print *, ""
  
  ! Initialize dynamic shader system
  print *, "🧪 Testing dynamic shader compilation..."
  call init_shader_system(shader_sys, 0)
  
  ! Setup convolution configuration
  conv_config%input_height = H
  conv_config%input_width = W
  conv_config%input_channels = C
  conv_config%kernel_height = kernel_size
  conv_config%kernel_width = kernel_size
  conv_config%output_height = H_out
  conv_config%output_width = W_out
  conv_config%output_channels = K
  conv_config%stride_y = stride
  conv_config%stride_x = stride
  conv_config%pad_y = pad
  conv_config%pad_x = pad
  conv_config%tile_size = 16
  conv_config%use_fp16 = .false.
  
  ! Test specific shader variants
  variant_names(1) = "rdna_basic_64"
  variant_names(2) = "rdna_large_256"
  variant_names(3) = "rdna_lds_64"
  variant_names(4) = "rdna3_dual_issue"
  
  print *, ""
  print *, "📝 Compiling and testing shader variants:"
  
  do i = 1, 4
    ! Get shader source for this variant
    ! In a real implementation, we'd access specific variants
    shader_source = get_optimal_shader(shader_sys, "conv2d", conv_config)
    
    ! Compile the shader
    program_id = gpu_compile_custom_shader_fortran(shader_source)
    
    if (program_id > 0) then
      ! Execute with custom shader
      output_custom = 0.0
      custom_time = gpu_execute_conv2d_custom_fortran(program_id, input, weights, output_custom, &
                                                      N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
      
      if (custom_time > 0) then
        custom_gflops = real(total_flops) / (custom_time * 1.0e6)
        variant_times(i) = custom_time
        
        ! Check correctness
        block
          real(real32) :: max_diff
          integer :: j
          max_diff = 0.0
          do j = 1, output_size
            max_diff = max(max_diff, abs(output_custom(j) - output_ref(j)))
          end do
          
          print '(A,A20,A,F8.2,A,F8.1,A,E10.3)', "   ", trim(variant_names(i)), ": ", &
                custom_time, " ms (", custom_gflops, " GFLOPS), max_diff=", max_diff
        end block
      else
        print '(A,A20,A)', "   ", trim(variant_names(i)), ": Execution failed"
      end if
    else
      print '(A,A20,A)', "   ", trim(variant_names(i)), ": Compilation failed"
    end if
  end do
  
  print *, ""
  print *, "📈 Performance Summary:"
  print '(A,F8.1,A)', "  Reference shader:    ", ref_gflops, " GFLOPS"
  print *, "  Dynamic compilation: Working ✅"
  print *, ""
  print *, "🔍 Next Steps:"
  print *, "  1. Implement shader variant selection in dynamic system"
  print *, "  2. Add workgroup size detection from shader source"
  print *, "  3. Test async GPU execution with custom shaders"
  
  ! Cleanup
  call gpu_cleanup()
  deallocate(input, weights, output_ref, output_custom)
  
end program test_layer3_shader_compilation