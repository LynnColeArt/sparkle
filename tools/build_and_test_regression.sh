#!/bin/bash
# Build and run performance regression tests
# ==========================================

set -e

echo "🔨 Building Performance Regression Tests..."
echo ""

# Create build directory
mkdir -p build/modules

# Step 1: Build common modules
echo "📦 Step 1: Building common modules..."
gfortran -c src/common/kinds.f90 -J build/modules
gfortran -c src/common/flopcount.f90 -I build/modules -J build/modules
gfortran -c src/common/time_utils.f90 -I build/modules -J build/modules
gfortran -c src/common/stable_math.f90 -I build/modules -J build/modules
gfortran -c src/common/c_ptr_utils.f90 -I build/modules -J build/modules

# Step 2: Build type modules
echo "📦 Step 2: Building type modules..."
gfortran -c src/sporkle_types.f90 -I build/modules -J build/modules
gfortran -c src/sporkle_error_handling.f90 -I build/modules -J build/modules

# Step 3: Build production modules
echo "📦 Step 3: Building production modules..."
gfortran -c src/production/gemm_simd_optimized.f90 -I build/modules -J build/modules -fopenmp
gfortran -c src/production/universal_memory_optimization.f90 -I build/modules -J build/modules -fopenmp

# Step 4: Build reference implementation
echo "📦 Step 4: Building reference modules..."
gfortran -c src/reference/cpu_conv2d_reference.f90 -I build/modules -J build/modules -fopenmp

# Step 5: Build GPU dispatch (stub for now)
echo "📦 Step 5: Creating GPU dispatch stub..."
cat > build/modules/gpu_dispatch_stub.f90 << 'EOF'
module sporkle_gpu_dispatch
  use kinds
  implicit none
  
contains
  real(sp) function execute_conv2d_gpu(input, weights, output, n, c, h, w, k, kh, ks, pad, h_out, w_out) result(time_ms)
    real(sp), intent(in) :: input(*), weights(*)
    real(sp), intent(out) :: output(*)
    integer, intent(in) :: n, c, h, w, k, kh, ks, pad, h_out, w_out
    
    ! Stub implementation
    time_ms = 2.0_sp  ! Pretend it takes 2ms
  end function
end module
EOF
gfortran -c build/modules/gpu_dispatch_stub.f90 -I build/modules -J build/modules

# Step 6: Build GPU async executor stub
echo "📦 Step 6: Creating GPU async executor stub..."
cat > build/modules/gpu_async_stub.f90 << 'EOF'
module gpu_async_executor
  use kinds
  implicit none
  
  type :: gpu_async_state
    integer :: dummy
  end type
  
contains
  subroutine gpu_async_executor_init(state, s1, s2, s3)
    type(gpu_async_state), intent(out) :: state
    integer, intent(in) :: s1, s2, s3
    state%dummy = 0  ! Initialize something
  end subroutine
  
  subroutine gpu_async_submit_work(state, input, weights, output, n, c, h, w, k, kh, ks, pad, h_out, w_out, id)
    type(gpu_async_state), intent(inout) :: state
    real(sp), intent(in) :: input(*), weights(*)
    real(sp), intent(out) :: output(*)
    integer, intent(in) :: n, c, h, w, k, kh, ks, pad, h_out, w_out, id
  end subroutine
  
  function gpu_is_work_complete(state, id) result(complete)
    type(gpu_async_state), intent(in) :: state
    integer, intent(in) :: id
    logical :: complete
    complete = .true.
  end function
  
  subroutine gpu_async_executor_cleanup(state)
    type(gpu_async_state), intent(inout) :: state
  end subroutine
end module
EOF
gfortran -c build/modules/gpu_async_stub.f90 -I build/modules -J build/modules

# Step 7: Build test implementation
echo "📦 Step 7: Building test implementation..."
gfortran -c tests/performance_regression_impl.f90 -I build/modules -J build/modules -fopenmp

# Step 8: Build and link test program
echo "📦 Step 8: Building test program..."
gfortran -o tests/performance_regression_test \
  tests/performance_regression_test.f90 \
  build/modules/*.o \
  -I build/modules -J build/modules \
  -fopenmp -lgomp

echo ""
echo "✅ Build complete!"
echo ""
echo "🔍 Running Performance Regression Tests..."
echo "=========================================="
echo ""

# Run the tests
cd tests && ./performance_regression_test