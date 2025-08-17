! Timing helpers module - Mini's fix for FPE in GFLOPS calc
module timing_helpers
  use iso_fortran_env, only: real64
  use omp_lib
  implicit none
  
contains

  function now_s() result(t)
    real(real64) :: t
    t = omp_get_wtime()
  end function now_s

  pure function safe_gflops(flops_r, secs) result(gf)
    real(real64), intent(in) :: flops_r, secs
    real(real64) :: gf
    real(real64), parameter :: eps = 1.0e-6_real64   ! 1 microsecond guard
    gf = flops_r / max(secs, eps) / 1.0e9_real64
  end function safe_gflops

end module timing_helpers