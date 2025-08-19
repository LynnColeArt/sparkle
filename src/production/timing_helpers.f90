! Timing helpers module - Mini's fix for FPE in GFLOPS calc
module timing_helpers
  use kinds
  use omp_lib
  implicit none
  
contains

  function now_s() result(t)
    real(dp) :: t
    t = omp_get_wtime()
  end function now_s

  pure function safe_gflops(flops_r, secs) result(gf)
    real(dp), intent(in) :: flops_r, secs
    real(dp) :: gf
    real(dp), parameter :: eps = 1.0e-6_real64   ! 1 microsecond guard
    gf = flops_r / max(secs, eps) / 1.0e9_real64
  end function safe_gflops

end module timing_helpers