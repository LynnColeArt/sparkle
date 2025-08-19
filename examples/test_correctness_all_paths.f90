program test_correctness_all_paths
  use kinds
  use sporkle_conv2d
  use sporkle_conv2d_juggling
  use sporkle_conv2d_auto_selector
  use cpu_conv2d_adaptive
  use gpu_opengl_interface
  implicit none
  
  ! Test dimensions
  integer, parameter :: N = 1, C = 3, H = 32, W = 32
  integer, parameter :: K = 16, kernel_size = 3, stride = 1, pad = 1
  integer, parameter :: H_out = 32, W_out = 32
  
  ! Arrays
  real(sp), allocatable :: input(:), weights(:)
  real(sp), allocatable :: output_cpu(:), output_gpu(:), output_auto(:)
  real(sp), allocatable :: output_juggling(:), output_reference(:)
  
  ! Timing and accuracy
  real(sp) :: time_ms, max_diff, diff
  real(dp) :: total_flops
  integer :: i, num_errors
  logical :: all_tests_passed
  
  print *, "🧪 Comprehensive Correctness Test - All Execution Paths"
  print *, "====================================================="
  print *, ""
  
  ! Allocate arrays
  allocate(input(N * C * H * W))
  allocate(weights(K * C * kernel_size * kernel_size))
  allocate(output_cpu(N * K * H_out * W_out))
  allocate(output_gpu(N * K * H_out * W_out))
  allocate(output_auto(N * K * H_out * W_out))
  allocate(output_juggling(N * K * H_out * W_out))
  allocate(output_reference(N * K * H_out * W_out))
  
  ! Initialize test data with predictable patterns
  print *, "📝 Initializing test data with known patterns..."
  call init_test_data()
  
  all_tests_passed = .true.
  num_errors = 0
  
  ! Test 1: Direct CPU implementation
  print *, ""
  print *, "1️⃣  Testing Direct CPU Implementation (cpu_conv2d_adaptive)..."
  time_ms = conv2d_adaptive(input, weights, output_cpu, &
                           N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  print '(A,F8.2,A)', "   Time: ", time_ms, " ms"
  
  ! Use CPU as reference
  output_reference = output_cpu
  
  ! Test 2: Production CPU via sporkle_conv2d
  print *, ""
  print *, "2️⃣  Testing Production CPU (sporkle_conv2d)..."
  call conv2d_select_implementation("cpu", "adaptive")
  call conv2d_cpu(input, weights, output_cpu, &
                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  max_diff = maxval(abs(output_cpu - output_reference))
  print '(A,ES12.4)', "   Max diff from reference: ", max_diff
  if (max_diff > 1e-5) then
    print *, "   ❌ FAILED! Results differ from reference"
    all_tests_passed = .false.
    num_errors = num_errors + 1
  else
    print *, "   ✅ PASSED"
  end if
  
  ! Test 3: GPU implementation (if available)
  if (gpu_init()) then
    print *, ""
    print *, "3️⃣  Testing GPU Implementation..."
    
    ! Test synchronous GPU
    print *, "   a) Synchronous GPU..."
    call disable_async_gpu()
    time_ms = conv2d_auto_juggling(input, weights, output_gpu, &
                                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    max_diff = maxval(abs(output_gpu - output_reference))
    print '(A,ES12.4)', "      Max diff from reference: ", max_diff
    if (max_diff > 1e-4) then  ! Slightly looser tolerance for GPU
      print *, "      ❌ FAILED! Results differ from reference"
      all_tests_passed = .false.
      num_errors = num_errors + 1
    else
      print *, "      ✅ PASSED"
    end if
    
    ! Test async GPU
    print *, "   b) Async GPU..."
    call enable_async_gpu()
    time_ms = conv2d_auto_juggling(input, weights, output_gpu, &
                                  N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
    max_diff = maxval(abs(output_gpu - output_reference))
    print '(A,ES12.4)', "      Max diff from reference: ", max_diff
    if (max_diff > 1e-4) then
      print *, "      ❌ FAILED! Results differ from reference"
      all_tests_passed = .false.
      num_errors = num_errors + 1
    else
      print *, "      ✅ PASSED"
    end if
    
    call gpu_cleanup()
  else
    print *, ""
    print *, "3️⃣  GPU not available, skipping GPU tests"
  end if
  
  ! Test 4: Auto-selection
  print *, ""
  print *, "4️⃣  Testing Auto-Selection..."
  call init_auto_selector()
  time_ms = conv2d_auto_select(input, weights, output_auto, &
                              N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  max_diff = maxval(abs(output_auto - output_reference))
  print '(A,ES12.4)', "   Max diff from reference: ", max_diff
  if (max_diff > 1e-4) then
    print *, "   ❌ FAILED! Results differ from reference"
    all_tests_passed = .false.
    num_errors = num_errors + 1
  else
    print *, "   ✅ PASSED"
  end if
  call cleanup_auto_selector()
  
  ! Test 5: Device juggling
  print *, ""
  print *, "5️⃣  Testing Device Juggling..."
  call init_juggling_system()
  time_ms = conv2d_auto_juggling(input, weights, output_juggling, &
                                N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  max_diff = maxval(abs(output_juggling - output_reference))
  print '(A,ES12.4)', "   Max diff from reference: ", max_diff
  if (max_diff > 1e-4) then
    print *, "   ❌ FAILED! Results differ from reference"
    all_tests_passed = .false.
    num_errors = num_errors + 1
  else
    print *, "   ✅ PASSED"
  end if
  
  ! Test 6: Weight change detection
  print *, ""
  print *, "6️⃣  Testing Weight Change Detection..."
  print *, "   Modifying weights..."
  weights = weights * 0.5  ! Change weights
  
  time_ms = conv2d_auto_juggling(input, weights, output_juggling, &
                                N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  ! Recalculate reference with new weights
  time_ms = conv2d_adaptive(input, weights, output_reference, &
                           N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  
  max_diff = maxval(abs(output_juggling - output_reference))
  print '(A,ES12.4)', "   Max diff from reference: ", max_diff
  if (max_diff > 1e-4) then
    print *, "   ❌ FAILED! Weight change not handled correctly"
    all_tests_passed = .false.
    num_errors = num_errors + 1
  else
    print *, "   ✅ PASSED - Weight changes detected and handled"
  end if
  
  call cleanup_juggling_system()
  
  ! Test 7: Edge cases
  print *, ""
  print *, "7️⃣  Testing Edge Cases..."
  
  ! Test with all zeros
  print *, "   a) All zero input..."
  input = 0.0
  time_ms = conv2d_adaptive(input, weights, output_reference, &
                           N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  if (all(output_reference == 0.0)) then
    print *, "      ✅ PASSED - Zero input produces zero output"
  else
    print *, "      ❌ FAILED - Non-zero output from zero input"
    all_tests_passed = .false.
    num_errors = num_errors + 1
  end if
  
  ! Test with simple known pattern
  print *, "   b) Simple pattern test..."
  input = 1.0  ! All ones
  weights = 0.0
  weights(1:kernel_size*kernel_size) = 1.0 / real(kernel_size * kernel_size)  ! Averaging kernel for first filter
  time_ms = conv2d_adaptive(input, weights, output_reference, &
                           N, C, H, W, K, kernel_size, stride, pad, H_out, W_out)
  ! First output channel should average to close to 1.0 (but edges may be different due to padding)
  if (minval(output_reference(1:H_out*W_out)) > 0.0 .and. &
      maxval(output_reference(1:H_out*W_out)) <= 1.0) then
    print *, "      ✅ PASSED - Averaging kernel works correctly"
    print '(A,F8.4,A,F8.4)', "         Output range: ", minval(output_reference(1:H_out*W_out)), &
                             " to ", maxval(output_reference(1:H_out*W_out))
    print '(A,F8.4)', "         Average value: ", sum(output_reference(1:H_out*W_out)) / real(H_out*W_out)
  else
    print *, "      ❌ FAILED - Averaging kernel produces out-of-bounds values"
    print '(A,F8.4,A,F8.4)', "         Output range: ", minval(output_reference(1:H_out*W_out)), &
                             " to ", maxval(output_reference(1:H_out*W_out))
    all_tests_passed = .false.
    num_errors = num_errors + 1
  end if
  
  ! Summary
  print *, ""
  print *, "========================================"
  if (all_tests_passed) then
    print *, "🎉 ALL TESTS PASSED! ✅"
    print *, "All execution paths produce correct results"
  else
    print '(A,I0,A)', "❌ TESTS FAILED: ", num_errors, " errors found"
    print *, "Please check the implementations"
  end if
  print *, ""
  
  ! Cleanup
  deallocate(input, weights)
  deallocate(output_cpu, output_gpu, output_auto)
  deallocate(output_juggling, output_reference)
  
contains

  subroutine init_test_data()
    integer :: i, j, k
    real(sp) :: x, y
    
    ! Initialize input with a gradient pattern
    do i = 1, size(input)
      j = mod(i-1, W) + 1
      k = mod((i-1)/W, H) + 1
      x = real(j) / real(W)
      y = real(k) / real(H)
      input(i) = sin(x * 3.14159) * cos(y * 3.14159) + 0.5
    end do
    
    ! Initialize weights with a simple pattern
    do i = 1, size(weights)
      weights(i) = real(i) / real(size(weights)) - 0.5
    end do
    
  end subroutine init_test_data

end program test_correctness_all_paths