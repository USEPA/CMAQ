!*******************************************************************************
!$RCSfile: us_atmos.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine stnd_atmos(ht,pr,t,tz,iflag)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Numerical approximation to 1962 U.S. Standard Atmosphere
!           Accurate to a height of 80 km.
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************

implicit none

real PFAC, KAPPA
parameter (PFAC  = -9.81/287.04)   ! -g/R
parameter (KAPPA = 2./7.)

! --- ARGUMENTS
 
real    ht     ! ht = height in meters
real    pr     ! pr = p/p0
real    t      ! t = pot. temp (iflag=0) or temp (iflag=1)
real    tz     ! tz = dt/dz
integer iflag  ! whether to output potential temp or not

! --- LOCALS

real z, fac

!------ make sure height is above sea level

z = max(ht,0.)

!------ define temperature in sections of constant gradient

if (z <= 11000.) then
  tz = -6.4891e-3
  t  = 288.15 + z*tz
  pr = (t/288.15)**(PFAC/tz)
else if (z <= 11500.) then
  tz = -2.400e-4
  t  = 216.77 + (z-11000.)*tz
  pr = 0.22332*(t/216.77)**(PFAC/tz)
else if (z <= 20000.) then
  tz = 0.
  t  = 216.65
  pr = 0.20639*exp(PFAC*(z-11500.)/t)
else if (z <= 20500.) then
  tz = 8.600e-4
  t  = 216.65 + (z-20000.)*tz
  pr = 5.3995e-2*(t/216.65)**(PFAC/tz)
else if (z <= 32000.) then
  tz = 1.0061e-3
  t  = 217.08 + (z-20500.)*tz
  pr = 4.9904e-2*(t/217.087)**(PFAC/tz)
else if (z <= 47000.) then
  tz = 2.8000e-3
  t  = 228.48 + (z-32000.)*tz
  pr = 8.5614e-3*(t/228.48)**(PFAC/tz)
else if (z <= 52000.) then
  tz = 0.
  t  = 270.65
  pr = 1.0915e-3*exp(PFAC*(z-47000.)/t)
else if (z <= 61000.) then
  tz = -2.000e-3
  t  = 270.65 + (z-52000.)*tz
  pr = 5.8051e-4*(t/270.65)**(PFAC/tz)
else if (z <= 69000.) then
  tz = -4.000e-3
  t  = 252.65 + (z-61000.)*tz
  pr = 1.7909e-4*(t/252.65)**(PFAC/tz)
else if (z <= 79000.) then
  tz = -3.000e-3
  t  = 220.65 + (z-69000.)*tz
  pr = 5.6304e-5*(t/220.65)**(PFAC/tz)
else
  tz = 0.
  t  = 190.65
  pr = 1.0654e-5*exp(PFAC*(z-79000.)/t)
end if

!------ compute potential temperature and gradient (if iflag=0)

if (iflag == 0) then
  fac = pr**(-KAPPA)
  t   = fac*t
  tz  = fac*(tz - KAPPA*PFAC)
end if

return

end
