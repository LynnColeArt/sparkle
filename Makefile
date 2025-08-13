# Sparkle Makefile - The People's Build System
# Simple, explicit, no magic

FC = gfortran
CC = clang
FFLAGS = -O2 -Wall -fcheck=bounds
CFLAGS = -O2 -Wall
LDFLAGS = -framework Metal -framework Foundation -framework CoreGraphics

# Source directories
SRC_DIR = src
EXAMPLES_DIR = examples
BUILD_DIR = build

# Create build directory
$(shell mkdir -p $(BUILD_DIR))

# Fortran modules (order matters for dependencies!)
MODULES = \
	$(SRC_DIR)/sparkle_types.f90 \
	$(SRC_DIR)/sparkle_mesh_types.f90 \
	$(SRC_DIR)/sparkle_error_handling.f90 \
	$(SRC_DIR)/sparkle_config.f90 \
	$(SRC_DIR)/sparkle_memory.f90 \
	$(SRC_DIR)/sparkle_kernels.f90 \
	$(SRC_DIR)/sparkle_safe_kernels.f90 \
	$(SRC_DIR)/sparkle_profile.f90 \
	$(SRC_DIR)/sparkle_scheduler.f90 \
	$(SRC_DIR)/sparkle_execute.f90 \
	$(SRC_DIR)/sparkle_collectives.f90 \
	$(SRC_DIR)/cpu_device.f90 \
	$(SRC_DIR)/sparkle_discovery.f90 \
	$(SRC_DIR)/sparkle_gpu_kernels.f90 \
	$(SRC_DIR)/sparkle_gpu_safe_detect.f90 \
	$(SRC_DIR)/sparkle_gpu_dispatch.f90 \
	$(SRC_DIR)/sparkle_gpu_backend_detect.f90 \
	$(SRC_DIR)/sparkle_gpu_backend.f90 \
	$(SRC_DIR)/sparkle_metal_kernels.f90 \
	$(SRC_DIR)/sparkle_gpu_metal.f90 \
	$(SRC_DIR)/sparkle_memory_metal.f90 \
	$(SRC_DIR)/sparkle_amx.f90 \
	$(SRC_DIR)/sparkle_neural_engine.f90 \
	$(SRC_DIR)/sparkle_apple_orchestrator.f90

# Object files
OBJECTS = $(MODULES:$(SRC_DIR)/%.f90=$(BUILD_DIR)/%.o)
METAL_OBJ = $(BUILD_DIR)/metal_wrapper.o
COREML_OBJ = $(BUILD_DIR)/coreml_bridge.o

# Default target
all: info test_metal

# Test memory pool
test_memory_pool: $(OBJECTS) $(METAL_OBJ) $(EXAMPLES_DIR)/test_metal_memory_pool.f90
	@echo ""
	@echo "🔨 Building Metal memory pool test..."
	$(FC) $(FFLAGS) -I$(BUILD_DIR) $(OBJECTS) $(METAL_OBJ) \
		$(EXAMPLES_DIR)/test_metal_memory_pool.f90 \
		-o $(BUILD_DIR)/test_metal_memory_pool \
		$(LDFLAGS)
	@echo "✅ Build complete!"
	@echo "Run with: ./build/test_metal_memory_pool"

# Test baseline comparison
test_metal_baseline_comparison: $(OBJECTS) $(METAL_OBJ) $(EXAMPLES_DIR)/test_metal_baseline_comparison.f90
	@echo ""
	@echo "🔨 Building Metal baseline comparison test..."
	$(FC) $(FFLAGS) -I$(BUILD_DIR) $(OBJECTS) $(METAL_OBJ) \
		$(EXAMPLES_DIR)/test_metal_baseline_comparison.f90 \
		-o $(BUILD_DIR)/test_metal_baseline_comparison \
		$(LDFLAGS)
	@echo "✅ Build complete!"
	@echo "Run with: ./build/test_metal_baseline_comparison"

info:
	@echo "🌟 Building Sparkle - The People's AI Infrastructure"
	@echo "=================================================="
	@echo ""

# Compile Fortran modules
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90
	@echo "📦 Compiling $<..."
	$(FC) $(FFLAGS) -c $< -o $@ -J$(BUILD_DIR)

# Compile Metal wrapper (Objective-C)
$(BUILD_DIR)/metal_wrapper.o: $(SRC_DIR)/metal_wrapper.m
	@echo "🎮 Compiling Metal wrapper..."
	$(CC) $(CFLAGS) -c $< -o $@

# Compile CoreML bridge (Objective-C)
$(BUILD_DIR)/coreml_bridge.o: $(SRC_DIR)/coreml_bridge_simple.m
	@echo "🧠 Compiling CoreML bridge..."
	$(CC) $(CFLAGS) -fobjc-arc -c $< -o $@

# Build test program
test_metal: $(OBJECTS) $(METAL_OBJ) $(COREML_OBJ) $(EXAMPLES_DIR)/test_metal_vs_mock.f90
	@echo ""
	@echo "🔨 Building Metal vs Mock test..."
	$(FC) $(FFLAGS) -I$(BUILD_DIR) $(OBJECTS) $(METAL_OBJ) $(COREML_OBJ) \
		$(EXAMPLES_DIR)/test_metal_vs_mock.f90 \
		-o $(BUILD_DIR)/test_metal_vs_mock \
		$(LDFLAGS)
	@echo ""
	@echo "✅ Build complete!"
	@echo ""
	@echo "Run with: ./build/test_metal_vs_mock"
	@echo ""

# Run the test
run: test_metal
	@echo "🚀 Running Metal vs Mock benchmark..."
	@echo ""
	@./$(BUILD_DIR)/test_metal_vs_mock

# Build all examples
examples: $(OBJECTS)
	@echo "Building examples..."
	$(FC) $(FFLAGS) -I$(BUILD_DIR) $(OBJECTS) \
		$(EXAMPLES_DIR)/hello_sparkle.f90 -o $(BUILD_DIR)/hello_sparkle
	$(FC) $(FFLAGS) -I$(BUILD_DIR) $(OBJECTS) \
		$(EXAMPLES_DIR)/device_info.f90 -o $(BUILD_DIR)/device_info
	@echo "✅ Examples built!"

# Clean build artifacts
clean:
	@echo "🧹 Cleaning build artifacts..."
	rm -rf $(BUILD_DIR)
	rm -f *.mod
	@echo "✨ Clean!"

# Install (TODO)
install:
	@echo "Installation not yet implemented"
	@echo "The people's infrastructure doesn't need permission!"

.PHONY: all info test_metal run examples clean install