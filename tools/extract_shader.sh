#!/bin/bash
# Extract shader binary from ROCm compilation
# This script compiles a simple kernel using ROCm and extracts the GCN binary
# We acknowledge this uses AMD's ROCm compiler - we're focusing our innovation
# on the framework and orchestration, not shader compilation.

echo "=== Extracting GCN Shader Binary from ROCm ==="
echo "This uses AMD's ROCm compiler to generate the shader."
echo "Credit: AMD ROCm team for the excellent shader compiler!"
echo ""

# Check if ROCm is available
if ! command -v /opt/rocm/bin/clang &> /dev/null; then
    echo "ROCm not found. Please install ROCm to generate shader binaries."
    echo "We're borrowing their compiler for this specific task."
    exit 1
fi

# Create simple OpenCL kernel
cat > vector_add.cl << 'EOF'
__kernel void vector_add(__global const float* a,
                        __global const float* b,
                        __global float* c,
                        const unsigned int n) {
    int tid = get_global_id(0);
    if (tid < n) {
        c[tid] = a[tid] + b[tid];
    }
}
EOF

# Compile to LLVM BC
echo "Compiling OpenCL kernel to LLVM bitcode..."
/opt/rocm/bin/clang -x cl -Xclang -finclude-default-header \
    -target amdgcn-amd-amdhsa -mcpu=gfx900 \
    -c -emit-llvm \
    vector_add.cl -o vector_add.bc

# Link and generate code object
echo "Generating AMD code object..."
/opt/rocm/bin/clang \
    -target amdgcn-amd-amdhsa -mcpu=gfx900 \
    vector_add.bc -o vector_add.co

# Extract the actual shader binary
echo "Extracting shader binary..."
/opt/rocm/bin/llvm-objdump -s vector_add.co > vector_add.dump

# Extract just the .text section (the actual shader code)
# This is a bit hacky but works
echo "Extracting .text section..."
python3 << 'PYTHON'
import re
import sys

with open('vector_add.dump', 'r') as f:
    content = f.read()

# Find the .text section
text_section = re.search(r'Contents of section \.text:.*?(?=Contents of section|\Z)', content, re.DOTALL)
if not text_section:
    print("ERROR: Could not find .text section")
    sys.exit(1)

# Extract hex bytes
hex_lines = text_section.group(0).split('\n')[1:]  # Skip header
shader_bytes = []

for line in hex_lines:
    if not line.strip():
        continue
    # Format: "address hex hex hex hex  ASCII"
    parts = line.split()
    if len(parts) > 1:
        # Skip address, take hex bytes
        for i in range(1, len(parts)):
            if len(parts[i]) == 8:  # 4-byte hex values
                shader_bytes.append(parts[i])
            else:
                break  # Hit ASCII section

# Write as Fortran array
with open('shader_binary.f90', 'w') as out:
    out.write("! GCN shader binary extracted from ROCm compilation\n")
    out.write("! Credit: Generated using AMD ROCm compiler\n")
    out.write("! Kernel: vector_add (a[i] = b[i] + c[i])\n")
    out.write("! Target: gfx900 (Vega)\n\n")
    
    out.write("integer(c_int32_t), parameter :: vector_add_shader_size = %d\n" % (len(shader_bytes) * 4))
    out.write("integer(c_int32_t), parameter :: vector_add_shader_data(%d) = [ &\n" % len(shader_bytes))
    
    for i, byte_group in enumerate(shader_bytes):
        if i > 0:
            out.write(", &\n")
        out.write("    int(z'%s', c_int32_t)" % byte_group)
    
    out.write(" ]\n")

print(f"Extracted {len(shader_bytes)} dwords ({len(shader_bytes) * 4} bytes) of shader code")
print("Written to shader_binary.f90")
PYTHON

echo ""
echo "Shader extraction complete!"
echo "The binary is from ROCm's compiler - we're standing on the shoulders of giants here."