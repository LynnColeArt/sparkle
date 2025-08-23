! Test CPU Kernel Dispatch
! ========================
! Verifies that the CPU device can properly dispatch different kernels

program test_cpu_kernel_dispatch
  use kinds
  use iso_c_binding
  use cpu_device_module
  use sporkle_types, only: SPORKLE_SUCCESS, SPORKLE_ERROR
  implicit none
  
  type(cpu_device) :: cpu
  integer :: status, i
  
  ! Test data
  real(sp), target :: x(100), y(100), alpha
  real(sp), target :: A(16), B(16), C(16), beta
  integer, target :: n, M_val, N_val, K_val
  
  ! Conv2D test data
  real(sp), target :: input(3*32*32), weights(16*3*3*3), output(16*30*30)
  integer, target :: conv_params(8)
  
  type(c_ptr) :: args(8)
  integer :: grid(3), block(3)
  
  print *, "🧪 Testing CPU Kernel Dispatch"
  print *, "============================="
  print *, ""
  
  ! Initialize CPU device
  cpu = cpu_device(device_id=0)
  call cpu%get_info()
  
  ! Test 1: Vector Add (SAXPY)
  print *, "📋 Test 1: Vector Add (SAXPY)"
  print *, "-----------------------------"
  
  n = 100
  alpha = 2.0
  x = 1.0
  y = 2.0
  
  args(1) = c_loc(alpha)
  args(2) = c_loc(x)
  args(3) = c_loc(y)
  args(4) = c_loc(n)
  
  grid = [1, 1, 1]
  block = [100, 1, 1]
  
  status = cpu%execute("vector_add", args(1:4), grid, block)
  
  if (status == SPORKLE_SUCCESS) then
    ! Check result: y should now be alpha*x + y = 2*1 + 2 = 4
    if (all(abs(y - 4.0) < 1.0e-6)) then
      print *, "✅ Vector add successful! All values = 4.0"
    else
      print *, "❌ Vector add failed! Expected 4.0, got:", y(1)
    end if
  else
    print *, "❌ Kernel dispatch failed!"
  end if
  
  ! Test 2: GEMM (Matrix Multiply)
  print *, ""
  print *, "📋 Test 2: GEMM (Matrix Multiply)"
  print *, "---------------------------------"
  
  M_val = 4
  N_val = 4
  K_val = 4
  alpha = 1.0
  beta = 0.0
  
  ! Initialize matrices (4x4)
  A = 1.0  ! All ones
  B = 2.0  ! All twos
  C = 0.0  ! Result
  
  args(1) = c_loc(A)
  args(2) = c_loc(B)
  args(3) = c_loc(C)
  args(4) = c_loc(M_val)
  args(5) = c_loc(N_val)
  args(6) = c_loc(K_val)
  args(7) = c_loc(alpha)
  args(8) = c_loc(beta)
  
  status = cpu%execute("gemm", args(1:8), grid, block)
  
  if (status == SPORKLE_SUCCESS) then
    ! Each element of C should be 4*2 = 8 (dot product of row of 1s with column of 2s)
    if (all(abs(C - 8.0) < 1.0e-6)) then
      print *, "✅ GEMM successful! All values = 8.0"
    else
      print *, "❌ GEMM failed! Expected 8.0, got:", C(1)
    end if
  else
    print *, "❌ Kernel dispatch failed!"
  end if
  
  ! Test 3: Conv2D
  print *, ""
  print *, "📋 Test 3: Conv2D"
  print *, "-----------------"
  
  ! Small conv2d: 1x3x32x32 -> 1x16x30x30 with 3x3 kernel
  conv_params = [1, 3, 32, 32, 16, 3, 1, 0]  ! N,C,H,W,K,kernel_size,stride,pad
  
  input = 1.0
  weights = 0.1
  output = 0.0
  
  args(1) = c_loc(input)
  args(2) = c_loc(weights)
  args(3) = c_loc(output)
  args(4) = c_loc(conv_params)
  
  status = cpu%execute("conv2d", args(1:4), grid, block)
  
  if (status == SPORKLE_SUCCESS) then
    ! Each output should be sum of 3*3*3 = 27 values of 0.1 = 2.7
    if (abs(output(1) - 2.7) < 1.0e-5) then
      print *, "✅ Conv2D successful! First output = 2.7"
    else
      print *, "❌ Conv2D failed! Expected 2.7, got:", output(1)
    end if
  else
    print *, "❌ Kernel dispatch failed!"
  end if
  
  ! Test 4: Unknown kernel
  print *, ""
  print *, "📋 Test 4: Unknown Kernel (Should Fail)"
  print *, "--------------------------------------"
  
  status = cpu%execute("unknown_kernel", args(1:1), grid, block)
  
  if (status == SPORKLE_ERROR) then
    print *, "✅ Correctly rejected unknown kernel"
  else
    print *, "❌ Should have failed for unknown kernel!"
  end if
  
  print *, ""
  print *, "🎉 CPU Kernel Dispatch Test Complete!"
  
end program test_cpu_kernel_dispatch