!*******************************************************************************
!$RCSfile: vd_slinn.f90,v $
!$Revision: 1.4 $
!$Date: 2007/02/06 23:33:01 $
!*******************************************************************************
subroutine vdep_dry(ustar,h_cnp,zruf,drat,dm,dbpx,vdry)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Get dry deposition rate for particles
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
! 01/31/2007 : Use constants from constants_fd -BC 
!*******************************************************************************
 
! --- MODULES
 
use constants_fd

implicit none


! --- ARGUMENTS

real ustar  !Friction velocity
real h_cnp  !Canopy height
real zruf   !Roughness height
real drat   !Settling velocity
real dm     !Mean particle diameter
real dbpx   !Particle diffusivity
real vdry   !Dry deposition velocity
 
! --- PARAMETERS

!------ Slinn (1982) parameters for Chamberlain's (1966,67) grass data

real ahat, acarat, fcarat, cvcd, efac
 
parameter (ahat   = 500.     )          !microns
parameter (acarat = 10.      )          !microns
parameter (fcarat = .01      )
parameter (cvcd   = 0.333333 )
parameter (efac   = 0.16     )          ! for fit to Slinn model


! --- LOCALS

real rpx, zref, zdispl, eb, tvpx, st, fac, aim, eim, ein
real eff, phi_r, udep

!------ no deposition if ustar=0

vdry = 0.

if (ustar == 0.) go to 9999

!-----  Set particle properties

rpx  = 0.5*dm

!-----  No canopy - use Lewellen & Sheng flat plate model

if (h_cnp <= 0.0) then

  zref   = 10.
  zdispl = 0.

  eb   = 0.8*(dbpx/rnu)**0.7
  tvpx = drat/g0
  st   = ustar*ustar*tvpx/rnu
  fac  = (1.+3.66*ustar*tvpx/zref)**2
  aim  = 0.08*st*(1.-exp(-0.424*st))/fac
  eim  = aim/(1.+aim)
  eff  = 1. - (1.-eb)*(1.-eim)

else

!------ Canopy - use parameterized Slinn model

  zref   = 2.0*h_cnp
  zdispl = 0.5*h_cnp

  tvpx = drat/g0
  eb   = cvcd*(dbpx/rnu)**0.7
  st   = tvpx*ustar/(ahat*1.e-6)
  ein  = cvcd*(fcarat*rpx/(rpx+acarat) &
                  + (1.0-fcarat)*rpx/(rpx+ahat))
  eim  = st*st/(1.+st*st)
  eff  = 1. - (1.-eim)*(1.-ein)*(1.-eb)
  eff  = eff / (efac + (1.-efac)*eff)

end if

phi_r  = alog(1.0 + (zref-zdispl)/zruf)
udep   = ustar*vonk/phi_r               ! = ustar**2/u_ref

vdry = udep*eff

9999    continue

return

end
