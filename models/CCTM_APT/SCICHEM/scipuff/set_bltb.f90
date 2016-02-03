!*******************************************************************************
!$RCSfile: set_bltb.f90,v $
!$Revision: 1.2 $
!$Date: 2006/11/22 21:50:46 $
!*******************************************************************************
real function f22(z)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:   Scaling function for the shear-driven component of the 
!             turbulent velocity correlations (v'v')
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS

real z  !  Non-dimensional height (z/zinv)

f22 = 2.5*(1.0-z)

return
end

real function f33(z)
!*******************************************************************************
!
! FUNCTION:  Scaling function for the shear-driven component of the 
!            turbulent velocity correlations (w'w')
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS

real z  !  Non-dimensional height (z/zinv)

f33 = 1.5*(1.0-z)

return
end

real function g22(z)
!*******************************************************************************
!
! FUNCTION:  Scaling function for the buoyancy-driven component of the 
!            turbulent velocity correlations (v'v')
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS

real z !  Non-dimensional height (z/zinv)

g22 = 0.13*(1.0+1.5*exp(-z))

return
end

real function g33(z)
!*******************************************************************************
!
! FUNCTION:  Scaling function for the buoyancy-driven component of the 
!            turbulent velocity correlations (w'w')
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS

real z !  Non-dimensional height (z/zinv)

g33 = 1.1*(z**0.6666667)*(1.05-z)

return
end

subroutine blturb(zarg,zi,ustar2,wstar2,uu,vv,ww,qq,sy,sz, &
                                         amet,bmet,hflux,zruf2d)
!*******************************************************************************
!
! FUNCTION:   Set the boundary layer turbulence profiles and vertical
!             diffusitivy
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                     f22                     g22                     f33
!                     g33
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met

implicit none

! --- ARGUMENTS
 
real zarg     ! Fraction of boundary layer grid
real zi       ! inversion height
real ustar2   ! horizontal velocity scale
real wstar2   ! vertical velocity scale
real uu       ! velocity variance
real vv       !    "  
real ww       !    "
real qq       ! turbulent velocity scale
real sy       ! puff sigma-y
real sz       ! puff sigma-z
real amet     ! numerator in vertical diffusivity calculation
real bmet     ! denominator in vertical diffusivity calculation
real hflux    ! heat flux
real zruf2d   ! 2D roughness

! --- LOCALS

real z, wt, rat, soq, uux
real f22, f33, g22, g33


z = zarg*zi

uu  = max( ustar2*f22(zarg) , wwtrop )
vv  = max( wstar2*g22(zarg) , wwtrop )
ww  = max( ustar2*f33(zarg) + wstar2*g33(zarg) , wwtrop )

wt  = max(hflux*(1.0-zarg),0.)

sz  = 0.65*(z+zruf2d)
sy  = 0.3*zi
sz  = sz*sy/sqrt(sz*sz+sy*sy)

uux = uu_calm - uu
if (vv < uux) then
  rat = vv/uux
  sy  = 1./(rat**1.5/sy + (1.-rat)**1.5/sl_calm)
  vv  = uux
end if

qq  = 2.*(uu + vv*(sz/sy)**0.6666667) + ww

soq  = sz/(sqrt(qq)+1.e-6)
bmet = eqf*soq**2
amet = soq/a*ww + bmet*wt
bmet = 2.*bmet                  ! equilibrium heat flux

return

end


real function ulog(zref,zruf,L)
!*******************************************************************************
!
! FUNCTION: Surface layer velocity profile shape function 
!           (log term + correction)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                     psi
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real zref  ! reference height
real zruf  ! rougness length
real L     ! Monin-Obukhov length

! --- LOCALS

real psi

ulog = alog(zref/zruf+1.) - psi(zref,L) + psi(zruf,L)

return

end


real function uzlog(zref,zruf,L)
!*******************************************************************************
!
! FUNCTION: Surface layer velocity gradient shape function
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                     phi
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real zref  ! reference height
real zruf  ! rougness length
real L     ! Monin-Obukhov length

! --- LOCALS

real phi, z

z = max(zref,zruf)

uzlog = phi(z,L)/z

return

end


real function psi(z,L)
!*******************************************************************************
!
! FUNCTION: Correction term to log profile
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real z  ! height
real L  ! Monin-Obukhov length

! --- PARAMETERS
 
real HPI
parameter (HPI = 3.141593/2.)

! --- LOCALS

real zeta, X

if (L == 0.) then
  psi = 0.
  return
end if

zeta = z/L

!------ neutral

if (abs(zeta) < 1.e-4) then

  psi = 0.

!------ unstable

else if (zeta < 0.) then

  X   = (1. - 15.*zeta)**0.25
  psi = 2.*alog((1.+X)/2.) + alog((1.+X*X)/2.) - 2.*atan(X) + HPI

!------ stable

else

  if (zeta < 0.5) then

    psi = -4.7*zeta

  else if (zeta < 10.) then

    psi = -6.85*alog(zeta) - 4.25/zeta + 0.5/zeta**2 - 0.598

  else

    psi = -6.435*alog(zeta) - 1.973

  end if

end if

return

end


real function phi(z,L)
!*******************************************************************************
!
! FUNCTION: Non-dimensional velocity gradient
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real z  ! height
real L  ! Monin-Obukhov length

! --- LOCALS

real zeta

if (L == 0.) then
  phi = 1.
  return
end if

zeta = z/L

!------ neutral

if (abs(zeta) < 1.e-4) then

  phi = 1.

!------ unstable

else if (zeta < 0.) then


  phi = 1./(1. - 15.*zeta)**0.25

!------ stable

else

  if (zeta < 0.5) then

    phi = 1. + 4.7*zeta                 ! standard function

  else if (zeta < 10.) then

    phi = 7.85 - 4.25/zeta + 1./zeta**2 ! from Carson & Richards (1978)

  else

    phi = 7.435                         ! held constant

  end if

end if

return

end



real function phiz(z,L)
!*******************************************************************************
!
! FUNCTION: Non-dimensional gradient of non-dimensional velocity gradient
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real z  ! height
real L  ! Monin-Obukhov length

! --- LOCALS

real zeta

if (L == 0.) then
  phiz = 0.
  return
end if

zeta = z/L

!------ neutral

if (abs(zeta) < 1.e-4) then

  phiz = 0.

!------ unstable

else if (zeta < 0.) then


  phiz = 3.75*zeta/(1. - 15.*zeta)

!------ stable

else

  if (zeta < 0.5) then

    phiz = 4.7*zeta/(1.+4.7*zeta)

  else if (zeta < 10.) then

    phiz = (4.25*zeta-2.)/(7.85*zeta**2 - 4.25*zeta + 1.)

  else

    phiz = 0.

  end if

end if

return

end


real function tlog(zref,zruf,L)
!*******************************************************************************
!
! FUNCTION: Surface layer temperature profile shape function 
!           (log term + correction)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   psi_h
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real zref  ! reference height
real zruf  ! roughness length
real L     ! Monin-Obukhov length

! --- PARAMETERS
 
real Pr
parameter (Pr = 0.74)

! --- LOCALS

real psi_h

tlog = Pr*(alog(zref/zruf+1.) - psi_h(zref,L) + psi_h(zruf,L))

return

end


real function psi_h(z,L)
!*******************************************************************************
!
! FUNCTION: Non-dimensional temperature gradient
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real z  ! height
real L  ! Monin-Obukhov length

! --- LOCALS

real zeta

!------ L = 0 (neutral)

if (L == 0.) then
  psi_h = 0.
  return
end if

zeta = z/L

!------ neutral

if (abs(zeta) < 1.e-4) then

  psi_h = 0.

!------ unstable

else if (zeta < 0.) then

  psi_h = 2.*alog(0.5*(1.+sqrt(1. -9.*zeta)))

!------ stable

else

  psi_h = -4.7*zeta

end if

return

end


real function dtdz_fun(hflux,ustar2,zref,L,zruf,lflag)
!*******************************************************************************
!
! FUNCTION: 
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   phi_h
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
real hflux              ! surface heat flux
real ustar2             ! friction velocity
real zref               ! reference height
real L                  ! Monin-Obukhov length
real zruf               ! roughness length
logical lflag           ! set hflux if lflag=T

! --- LOCALS

real thstar, zlim, phi_h


!------ neutral

if (L == 0.) then

  dtdz_fun = 0.

  if (lflag) hflux = 0.

!------ diabatic

else

  if (lflag) then
    hflux  = -sqrt(ustar2)**3 / (vonk*gt*L)
  end if

  thstar =  ustar2 / (vonk*gt*L)

  zlim = max(zref,zruf)

  dtdz_fun = phi_h(zlim,L) * thstar/(vonk*zlim)

end if

return

end


real function phi_h(z,L)
!*******************************************************************************
!
! FUNCTION: Non-dimensional temperature gradient
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real z  ! height
real L  ! Monin-Obukhov length

! --- PARAMETERS
 
real Pr
parameter (Pr = 0.74)

! --- LOCALS

real zeta

!------ L = 0 (neutral)

if (L == 0.) then
  phi_h = Pr
  return
end if

zeta = z/L

!------ neutral

if (abs(zeta) < 1.e-4) then

  phi_h = Pr

!------ unstable

else if (zeta < 0.) then

  phi_h = Pr/sqrt(1.-9.*zeta)

!------ stable

else

  phi_h    = Pr + 4.7*zeta

end if

return

end
