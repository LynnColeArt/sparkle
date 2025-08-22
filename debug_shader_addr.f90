program debug_shader_addr
  use iso_c_binding
  implicit none
  
  integer(c_int64_t) :: addr
  integer(c_int32_t) :: lo, hi
  
  \! Example address
  addr = int(z'00007F1234567800', c_int64_t)
  
  print '(A,Z16)', "Original address: 0x", addr
  
  \! Our current encoding
  lo = int(ishft(addr, -8), c_int32_t)
  hi = int(ishft(addr, -40), c_int32_t)
  
  print '(A,Z8)', "LO (shift -8):  0x", lo
  print '(A,Z8)', "HI (shift -40): 0x", hi
  
  \! Reconstruct
  print '(A,Z16)', "Reconstructed:  0x", ior(ishft(int(lo, c_int64_t), 8), &
                                             ishft(int(hi, c_int64_t), 40))
end program
