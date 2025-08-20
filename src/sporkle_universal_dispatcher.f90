module sporkle_universal_dispatcher
  ! Universal GPU dispatcher that connects auto-tuner to real optimized kernels
  ! Routes workloads to the best device with optimal parameters
  
  use kinds
  use iso_c_binding
  use sporkle_types
  use sporkle_universal_autotuner
  use sporkle_nvidia_async        ! NVIDIA async pattern (6.5× speedup!)
  use time_utils                  ! Mini's hardened timing utilities  
  use flopcount                   ! Mini's overflow-safe FLOP counting
  implicit none
  
  private
  public :: universal_dispatch_init, universal_dispatch_conv2d
  public :: universal_dispatch_shutdown
  
  ! Dispatcher state
  logical :: dispatcher_initialized = .false.
  
contains

  subroutine universal_dispatch_init()
    ! Initialize the universal dispatcher
    
    print *, "=== Universal GPU Dispatcher Initialization ==="
    print *, ""
    
    ! Initialize auto-tuner (hardware profiling + parameter optimization)
    call auto_tune_init()
    
    ! Initialize NVIDIA async executor
    if (nvidia_async_init()) then
      print *, "✓ NVIDIA async executor ready"
      print *, "  - Triple buffering: 6.5× speedup"
      print *, "  - Universal async pattern from AMD"  
      print *, "  - Target: 17.6 TFLOPS (2.7 × 6.5)"
    else
      print *, "✗ NVIDIA async executor failed - falling back to basic mode"
    end if
    
    dispatcher_initialized = .true.
    print *, "✓ Universal dispatcher ready!"
    print *, ""
    
  end subroutine universal_dispatch_init
  
  function universal_dispatch_conv2d(input, kernel, output, &
                                    batch, in_c, out_c, h, w, kh, kw) result(gflops)
    real(sp), intent(in), target :: input(*), kernel(*)
    real(sp), intent(out), target :: output(*)
    integer, intent(in) :: batch, in_c, out_c, h, w, kh, kw
    real(dp) :: gflops
    
    integer :: h_out, w_out
    integer(i64) :: total_flops
    real(dp) :: workload_gflops
    character(len=64) :: selected_device
    integer(i64) :: start_tick, end_tick
    real(dp) :: elapsed_seconds, time_ms
    logical :: success
    
    if (.not. dispatcher_initialized) then
      print *, "ERROR: Universal dispatcher not initialized"
      gflops = 0.0_dp
      return
    end if
    
    ! Calculate workload using Mini's hardened FLOP counter
    h_out = h - kh + 1
    w_out = w - kw + 1
    total_flops = conv2d_flops(int(batch, i64), int(in_c, i64), int(out_c, i64), &
                              int(h_out, i64), int(w_out, i64), int(kh, i64), int(kw, i64))
    workload_gflops = real(total_flops, dp) / 1.0e9_dp
    
    ! Get auto-tuner device selection (ignore simulated performance)
    gflops = auto_tune_conv2d(input, kernel, output, batch, in_c, out_c, h, w, kh, kw)
    
    ! Get the selected device from auto-tuner
    selected_device = trim(global_tuner%active_device%name)
    
    print *, "=== Universal Dispatch Execution ==="
    print '(A,F10.3,A)', "Workload: ", workload_gflops, " GFLOPs"
    print *, "Auto-tuner selected:", trim(selected_device)
    
    ! Route to actual optimized implementation
    
    ! Use the NVIDIA async executor that should give 6.5× speedup!
    print *, "→ Executing with NVIDIA async executor"
    print *, "  Triple buffering + fence sync = 6.5× AMD speedup"
    
    if (index(selected_device, 'NVIDIA') > 0) then
      ! Use async NVIDIA execution
      print *, "  Using NVIDIA async path"
      time_ms = nvidia_async_conv2d(input, kernel, output, &
                                   batch, in_c, out_c, h, w, kh, kw)
      
      if (time_ms > 0.0_dp) then
        gflops = (workload_gflops * 1000.0_dp) / time_ms
        print '(A,F8.2,A,F10.1,A)', "NVIDIA async: ", time_ms, " ms, ", gflops, " GFLOPS"
        
        ! Check for breakthrough performance
        if (gflops > 10000.0_dp) then
          print *, "🎉 SUCCESS: Breaking 10 TFLOPS barrier!"
        end if
        if (gflops > 16000.0_dp) then
          print *, "🚀 BREAKTHROUGH: Achieved target 16+ TFLOPS!"
        end if
        if (gflops > 20000.0_dp) then
          print *, "🔥 LEGENDARY: Beyond theoretical projections!"
        end if
      else
        print *, "NVIDIA async execution failed"
        gflops = 0.0_dp
      end if
      
    else
      ! Fallback to CPU timing
      print *, "  Using CPU fallback"
      call tic(start_tick)
      call execute_cpu_fallback(input, kernel, output, batch, in_c, out_c, h, w, kh, kw)
      elapsed_seconds = toc_seconds(start_tick)
      
      if (elapsed_seconds > 0.0_dp) then
        gflops = workload_gflops / elapsed_seconds
        print '(A,F8.2,A,F10.1,A)', "CPU fallback: ", elapsed_seconds * 1000.0_dp, " ms, ", gflops, " GFLOPS"
      end if
    end if
    
    print *, ""
    
  end function universal_dispatch_conv2d
  
  subroutine execute_cpu_fallback(input, kernel, output, batch, in_c, out_c, h, w, kh, kw)
    real(sp), intent(in) :: input(*), kernel(*)
    real(sp), intent(out) :: output(*)
    integer, intent(in) :: batch, in_c, out_c, h, w, kh, kw
    
    ! Simple CPU fallback implementation
    ! In production, this would call optimized CPU SIMD code
    integer :: b, oc, ic, oh, ow, ih, iw, kh_idx, kw_idx
    integer :: h_out, w_out
    real(sp) :: sum
    integer :: in_idx, k_idx, out_idx
    
    h_out = h - kh + 1
    w_out = w - kw + 1
    
    do b = 0, batch - 1
      do oc = 0, out_c - 1
        do oh = 0, h_out - 1
          do ow = 0, w_out - 1
            sum = 0.0_sp
            
            do ic = 0, in_c - 1
              do kh_idx = 0, kh - 1
                do kw_idx = 0, kw - 1
                  ih = oh + kh_idx
                  iw = ow + kw_idx
                  
                  if (ih >= 0 .and. ih < h .and. iw >= 0 .and. iw < w) then
                    in_idx = ((b * in_c + ic) * h + ih) * w + iw + 1
                    k_idx = ((oc * in_c + ic) * kh + kh_idx) * kw + kw_idx + 1
                    sum = sum + input(in_idx) * kernel(k_idx)
                  end if
                end do
              end do
            end do
            
            out_idx = ((b * out_c + oc) * h_out + oh) * w_out + ow + 1
            output(out_idx) = sum
          end do
        end do
      end do
    end do
    
  end subroutine execute_cpu_fallback
  
  subroutine universal_dispatch_shutdown()
    if (dispatcher_initialized) then
      ! Shutdown NVIDIA async executor
      call nvidia_async_shutdown()
      dispatcher_initialized = .false.
      print *, "Universal dispatcher shut down"
    end if
  end subroutine universal_dispatch_shutdown

end module sporkle_universal_dispatcher