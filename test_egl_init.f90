program test_egl_init
  ! Test if EGL initialization actually works now
  
  use sporkle_nvidia_opengl
  implicit none
  
  logical :: success
  character(len=256) :: device_info
  
  print *, "Testing fixed EGL initialization..."
  print *, ""
  
  ! Try to initialize
  success = nvidia_gl_init()
  
  if (success) then
    print *, "✅ SUCCESS: EGL initialized!"
    print *, ""
    
    ! Get device info
    device_info = nvidia_gl_get_device_info()
    print *, "Device: ", trim(device_info)
    
    ! Clean up
    call nvidia_gl_shutdown()
    print *, ""
    print *, "🎉 EGL INITIALIZATION FIXED!"
    print *, "We can now proceed with real GPU compute implementation"
  else
    print *, "❌ FAILURE: EGL still not initializing"
    print *, ""
    print *, "Possible issues:"
    print *, "1. No NVIDIA driver installed"
    print *, "2. Running in container without GPU access"
    print *, "3. Missing EGL/OpenGL libraries"
    print *, ""
    print *, "Let's check what's available:"
    call system("ls -la /dev/dri/ 2>/dev/null || echo 'No DRI devices'")
    call system("nvidia-smi 2>/dev/null || echo 'No NVIDIA driver'")
    call system("ldconfig -p | grep -E '(EGL|GL)' | head -5")
  end if
  
end program test_egl_init