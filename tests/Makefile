# Makefile for Performance Regression Tests

FC = gfortran
FFLAGS = -O3 -march=native -fopenmp -ftree-vectorize -ffast-math
INCLUDES = -I../src/common -I../src/reference -I../src/production -I../src

# Source files
COMMON_SRCS = ../src/common/kinds.f90 \
              ../src/common/time_utils.f90 \
              ../src/common/flopcount.f90 \
              ../src/common/stable_math.f90 \
              ../src/common/c_ptr_utils.f90

REGRESSION_SRCS = performance_regression_impl.f90 \
                  performance_regression_test.f90

# Dependency modules (simplified list - add more as needed)
DEPS = ../src/reference/cpu_conv2d_reference.f90 \
       ../src/production/sporkle_gpu_dispatch.f90 \
       ../src/gpu_async_executor.f90

.PHONY: all test clean

all: performance_regression_test

performance_regression_test: $(COMMON_SRCS) $(DEPS) $(REGRESSION_SRCS)
	$(FC) $(FFLAGS) $(INCLUDES) -o $@ $^ -lGL -lEGL

test: performance_regression_test
	@echo "🔍 Running Performance Regression Tests..."
	@echo "========================================"
	@./performance_regression_test

# Quick test with relaxed thresholds for development
test-dev: performance_regression_test
	@echo "🔧 Running Regression Tests (Dev Mode - Relaxed Thresholds)..."
	@echo "============================================================"
	@./performance_regression_test --dev

clean:
	rm -f *.o *.mod performance_regression_test

# CI/CD integration target
test-ci: performance_regression_test
	@echo "🤖 Running Regression Tests for CI/CD..."
	@./performance_regression_test --ci || (echo "❌ Performance regression detected!" && exit 1)

# Generate performance report
report: performance_regression_test
	@echo "📊 Generating Performance Report..."
	@./performance_regression_test --report > performance_report_$(shell date +%Y%m%d_%H%M%S).txt
	@echo "Report saved to performance_report_*.txt"

help:
	@echo "Performance Regression Test Targets:"
	@echo "  make test       - Run standard regression tests"
	@echo "  make test-dev   - Run with relaxed thresholds"
	@echo "  make test-ci    - Run for CI/CD (exits with error on failure)"
	@echo "  make report     - Generate detailed performance report"
	@echo "  make clean      - Remove build artifacts"