!*******************************************************************************
!$RCSfile: ensm_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module ensm_inc
save

!------ large-scale (ensemble) turbulence parameters

real ZTROP_CLIMO! height of tropopause (m)
real ZBL_CLIMO  ! representative height of bl (m)
real DZBL_CLIMO ! transition depth above bl (m)
real SBL_CLIMO  ! length-scale for bl climatology (m)
real SUTROP_CLIMO       ! rms velocity at tropopause
real SUSURF_CLIMO       ! rms velocity at surface
real SUBL_CLIMO ! rms velocity in boundary layer
real TAU_FC     ! forecast error growth time-scale (s)
real TAU_PS     ! persistence error growth time-scale (s)
real TAU_D      ! persistence error diurnal growth (s)
real FRAC_D     ! fraction climatology in diurnal
real FRAC_DM    ! 1. - FRAC_D
real SLE_OBS    ! length scale / distance from obs
real SLZ_OBS    ! vertical length scale surface obs

parameter (ZTROP_CLIMO = 10000.)
parameter (ZBL_CLIMO   = 1000.)
parameter (DZBL_CLIMO  = 200.)
parameter (SBL_CLIMO   = 100.e3)
parameter (SUTROP_CLIMO = 12.)
parameter (SUSURF_CLIMO = 5.)
parameter (SUBL_CLIMO   = 4.)
parameter (TAU_FC  =  3.0*24.*3600.)
parameter (TAU_PS  =  1.5*24.*3600.)
parameter (TAU_D   = 0.25*24.*3600.)
parameter (FRAC_D  = 0.25)
parameter (FRAC_DM = 1.-FRAC_D)
parameter (SLE_OBS = 1.0)
parameter (SLZ_OBS = 1000.)
end module ensm_inc
