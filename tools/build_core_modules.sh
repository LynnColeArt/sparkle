#!/bin/bash
# Build core modules in dependency order
# ======================================

set -e

echo "🔨 Building core Sporkle modules..."
echo ""

# Create build directory
mkdir -p build/modules

# Compile common modules first
echo "📦 Building common modules..."
gfortran -c src/common/kinds.f90 -J build/modules -o build/modules/kinds.o
gfortran -c src/common/flopcount.f90 -I build/modules -J build/modules -o build/modules/flopcount.o
gfortran -c src/common/time_utils.f90 -I build/modules -J build/modules -o build/modules/time_utils.o
gfortran -c src/common/stable_math.f90 -I build/modules -J build/modules -o build/modules/stable_math.o
gfortran -c src/common/c_ptr_utils.f90 -I build/modules -J build/modules -o build/modules/c_ptr_utils.o

# Build GEMM module
echo "📦 Building GEMM module..."
gfortran -c src/production/gemm_simd_optimized.f90 -I build/modules -J build/modules -o build/modules/gemm_simd_optimized.o -fopenmp

# Build universal memory optimization
echo "📦 Building universal memory optimization..."
gfortran -c src/production/universal_memory_optimization.f90 -I build/modules -J build/modules -o build/modules/universal_memory_optimization.o -fopenmp

# Build CPU reference
echo "📦 Building CPU reference..."
gfortran -c src/reference/cpu_conv2d_reference.f90 -I build/modules -J build/modules -o build/modules/cpu_conv2d_reference.o -fopenmp

echo ""
echo "✅ Core modules built successfully!"
echo ""
echo "Module files in build/modules:"
ls -la build/modules/*.mod | wc -l
echo ""