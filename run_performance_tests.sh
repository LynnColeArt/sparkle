#!/bin/bash
# Performance Regression Test Runner
# ==================================
# 
# Run this before committing or as part of CI/CD to ensure
# performance hasn't regressed

set -e  # Exit on error

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

echo "🚀 Sparkle Performance Regression Tests"
echo "======================================"
echo ""

# Check if we're in CI environment
if [ -n "$CI" ]; then
    echo "📍 Running in CI/CD environment"
    MODE="--ci"
else
    echo "📍 Running in development environment"
    MODE=""
fi

# Build the tests if needed
if [ ! -f "tests/performance_regression_test" ]; then
    echo "🔨 Building regression tests..."
    make -C tests all
fi

# Run the tests
echo ""
echo "🔍 Running performance tests..."
echo ""

if tests/performance_regression_test $MODE; then
    echo ""
    echo "✅ All performance tests passed!"
    echo ""
    echo "Performance Summary:"
    echo "- CPU SIMD: ≥157 GFLOPS ✓"
    echo "- GPU Single: ≥356 GFLOPS ✓"
    echo "- GPU Async: ≥2900 GFLOPS ✓"
    echo "- Async Speedup: ≥5x ✓"
    echo ""
    echo "🎉 Your optimizations are safe!"
    exit 0
else
    echo ""
    echo "❌ PERFORMANCE REGRESSION DETECTED!"
    echo ""
    echo "One or more benchmarks fell below acceptable thresholds."
    echo "This could mean:"
    echo "1. Recent changes have impacted performance"
    echo "2. Compiler optimizations were disabled"
    echo "3. System is under heavy load"
    echo ""
    echo "Please investigate before committing!"
    exit 1
fi