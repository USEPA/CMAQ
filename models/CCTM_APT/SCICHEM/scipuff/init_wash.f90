!*******************************************************************************
!$RCSfile: init_wash.f90,v $
!$Revision: 1.3 $
!$Date: 2010/08/24 20:45:14 $
!*******************************************************************************
subroutine init_wash
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Initial washout for particle material types
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!       get_puff_material            get_wash_gas           get_wash_part
!                   ufall                   IsGas              IsParticle
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf
use files_inc

implicit none

! --- PARAMETERS

real       A_RAIN
parameter (A_RAIN = 7.317e-04)
real       B_RAIN
parameter (B_RAIN = 0.21)
real       A_SNOW
parameter (A_SNOW = 5.215e-04)
real       B_SNOW
parameter (B_SNOW = 0.25)
real       RHOW
parameter (RHOW   = 1.00e+03)
real       V_SNOW
parameter (V_SNOW = 1.1)
 
! --- LOCALS

type ( puff_material ) pmatl

integer i,j,icls

real pr(NWASH), dr(NWASH), ufall

logical IsGas, IsParticle

!-----  Set Rain Precipitation Groups (mm/hr)

pr(1)  =  0.5
pr(2)  =  3.5
pr(3)  = 25.0

!-----  SNOW

pr(4)  =   5.0
pr(5)  =  20.0
pr(6)  = 100.0

!-----  Set Precipitation fall velocity (m/s)

vwash(0) =  0.0                         !No precipitation
do i = 1,NRAIN
  dr(i)    =  A_RAIN*pr(i)**B_RAIN
  vwash(i) =  ufall(rhoair,RHOW,rmuair,dr(i))
end do
do i = NRAIN+1,NWASH
  dr(i)    =  A_SNOW*pr(i)**B_SNOW
  vwash(i) =  V_SNOW
end do

!-----  Calculate Scavenging Coefficients (s)

write(lun_log,*) 'particle wash-out timescale (s):'
do j = 1,ntypp

  icls = typeID(j)%icls

  call get_puff_material(j,material,mat_aux,pmatl)

  twash(0,j) = 0.                       !No precipitation

  if (IsGas(icls))then

    do i = 1,NWASH
      call get_wash_gas(twash(i,j))
    end do

  else if (IsParticle(icls))then

    do i = 1,NWASH
      call get_wash_part(i,icls,pmatl,pr(i),dr(i),vwash(i),twash(i,j))
    end do

  end if

end do

continue

return

end


subroutine get_wash_part(ipr,icls,pmatl,pr,dr,vr,tauwo)
!*******************************************************************************
!
! FUNCTION: Initialize washout timescale for individual particle sizes
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!         part_wash_param              IsParticle
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
integer ipr, icls            !Precipitation group, material class number
type ( puff_material ) pmatl !Puff material structure
real pr, dr                  !Precipitiation rate, droplet diameter
real vr, tauwo               !Settling velocity, washout timescale

! --- PARAMETERS

real       MUW
parameter (MUW    = 1.00e-03) !Viscosity of water
real       CNVFAC
parameter (CNVFAC = 4.167e-07)

! --- LOCALS

real dif, rhop
real dp, vp, sc3, tau
real re, re2, sc, st, sts, wi, h, e, e1, e2, e3, sc2

logical IsParticle

!-----  Get puff parameters

if (IsParticle(icls))then                       !Particle

  call part_wash_param(pmatl,dp,rhop,vp,dif)

end if

!-----  Calculate dimensionless groups

tau  = vp/G0
re   = (0.5*dr*vr*rhoair)/rmuair
sc   = rmuair/(rhoair*dif)
st   = 2.*tau*(vr - vp)/dr
if (ipr <= NRAIN) then
  wi = rmuair/MUW                               !RAIN
else
  wi = 0.                                       !SNOW
end if
h    = dp/dr

re2  = sqrt(re)
sc2  = sqrt(sc)
sc3  = sc**(1./3.)
!-----  Changed by RSG 10/16/96
!rsg    sts  = (1.2+(1./12.)*alog(1.+re))/(1.+alog(1.+re))
sts  = 0.0

!rsg    if (st > sts) then
!rsg      e2 = ((st-sts)/(st-sts+0.666667))**1.5
!rsg    else
!rsg      e2 = 1.0e-09
!rsg    end if

e1   = 4.*h*(wi + (1.+2.*re2)*h)
e2   = ((st-sts)/(st-sts+0.666667))**1.5
e3   = 4.*(1. + 0.4*re2*sc3 + 0.16*re2*sc2)/(re*sc)

!       e    = e1 + e2*(RHOW/rhop)**0.5 + e3
!rsg    e    = e1 + e2 + e3
e    = (e1 + e2 + e3)/3.0

tauwo= CNVFAC*pr*e/dr

return
end


subroutine part_wash_param(pmatl,dp,rhop,vp,dif)
!*******************************************************************************
!
! FUNCTION:  Set local parameters from particle material structure
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_material ) pmatl   !Puff material structure

real dp, rhop, vp, dif         !Particle diameter, density, 
                               !Settling vel, diffusivity

dp   = pmatl%param(3)     !dbar
rhop = pmatl%param(2)     !rho
vp   = pmatl%param(4)     !vd
dif  = pmatl%param(6)     !diff

return
end


subroutine get_wash_gas(tauwo)
!*******************************************************************************
!
! FUNCTION:  Set local parameters from gas material structure
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
 
real tauwo

tauwo = 0.

return
end
