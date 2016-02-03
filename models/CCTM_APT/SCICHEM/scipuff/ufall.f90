!*******************************************************************************
!$RCSfile: ufall.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
real function ufall(rhoa,rhop,rmuair,d)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Calculate the terminal fall velocity of a particle
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real rhoa   !Density of air
real rhop   !Density of the particle
real rmuair !Viscocity of air
real d      !Particle diameter

! --- PARAMETERS

real G
parameter (G = 9.81)  !gravity

! --- LOCALS

real r, alp, u, re, dnu, fac, dua, fdash
real fval, un, vnu


if (rhop == 0. .or. d == 0.) then
  ufall = 0.
  go to 9999
end if

vnu = rmuair/rhoa
r   = 0.5*d
alp = sqrt(2.666667*rhop*r*G/rhoa)

!------ High-Re limit - u=alp/sqrt(cd) where cd=0.403

u  = alp*1.5752427
re = u*d/vnu

if (re >= 1000.) then
  ufall = u
  go to 9999
end if

dnu  = d/vnu
fac  = alp*alp*dnu/24.

1000    continue

dua   = 0.158*(u*dnu)**0.6666667
fdash = 1.0 + 1.6666667*dua
fval  = u*(1.0+dua) - fac
un    = u - fval/fdash

if (abs(un-u)/u < 1.e-4) then
  ufall = un
  go to 9999
end if
u = un

go to 1000

9999    continue

return

end
