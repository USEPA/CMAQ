!*******************************************************************************
!$RCSfile: ensm_sub.f90,v $
!$Revision: 1.3 $
!$Date: 2007/02/06 23:33:01 $
!
! REVISION HISTORY: 
!
! 01/31/2007 : Use constants from constants_fd -BC 
!*******************************************************************************
!------ large-scale horizontal ensemble variability subroutines------------

subroutine init_ensm_obs
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Initialization for obs met
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!           set_ensm_grid              horiz_turb
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_met

implicit none

!------ setup grid parameters for various large-scale variability types

if (ensm_type == 'OBS') then

  call set_ensm_grid

else if (ensm_type == 'MODEL') then

  call horiz_turb

else if (ensm_type == 'INPUT') then

!       nothing required ?

end if

return

end

subroutine uu_gifford(lat,uu,sl)
!*******************************************************************************
!
! FUNCTION:  Mesoscale velocity fluctuation and horizontal length scale
!            based on Gifford(1988), "A similarity theory of the tropospheric
!            turbulence energy spectrum", J. Atmos. Sci., 45, 1370-1379.
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_puf

implicit none

! --- ARGUMENTS

real lat  !latitude
real uu   !horizontal velocity flutuation (u'u')
real sl   !horizontal turbulent length scale

! --- LOCALS

real xlat, ylat, clat, slat, fcor

xlat = min(75.,abs(lat))*PI180
ylat = max(15.,abs(lat))*PI180
clat = cos(xlat)
slat = sin(ylat)
fcor = FCOR0*slat
uu   = epstrop/(FCOR0*clat)
sl   = sqrt(2.*uu)/fcor

return

end

subroutine horiz_turb
!*******************************************************************************
!
! FUNCTION:  Set horizontal turbulence variables 
!            for the mesoscale/synoptic scale
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!                  mapfac              uu_gifford
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_puf
use common_met

!------ upper air version

implicit none

! --- LOCALS

integer j

real slb0, uubj, slbj
real xcent, ycent, xmap, ymap

!------ if horizontal grid is 2d, scale is based on grid size

if (nxyb > 1) then

  xcent = 0.5*(xb(1)+xb(nxb))
  ycent = 0.5*(yb(1)+yb(nyb))
  call mapfac( xcent , ycent , xmap , ymap )
  slb0 = sle_fac*sqrt((dxb/xmap)**2 + (dyb/ymap)**2)

else

!------ otherwise, default to full spectrum

  slb0   = DEF_VAL_R

end if

!------ Gifford model (function of latitude)

if (lmap == I_LATLON) then

  do j = 1,nyb
    call uu_gifford(yb(j),uubj,slbj)    ! loop through lat. grid
    if (slbj > slb0 .and. slb0 /= DEF_VAL_R) then
      uubj = uubj*(slb0/slbj)**0.6666667
      slbj = slb0
    end if
    uu_lsv(j) = uubj
    sl_lsv(j) = slbj
  end do

else

  call uu_gifford(lat0,uubj,slbj)       ! use reference lat.
  if (slbj > slb0  .and. slb0 /= DEF_VAL_R) then
    uubj = uubj*(slb0/slbj)**0.6666667
    slbj = slb0
  end if
  do j = 1,nyb
    uu_lsv(j) = uubj
    sl_lsv(j) = slbj
  end do

end if

return

end

subroutine set_ensm_grid
!*******************************************************************************
!
! FUNCTION:  Set ensemble turbulence grid to met grid
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_met

implicit none

! --- LOCALS

integer i

nxe  = nxb
nye  = nyb
nxye = nxe*nye
nzbe = nzb

dxe = dxb
dye = dyb

do i = 1,nxb
  xbe(i) = xb(i)
end do

do i = 1,nyb
  ybe(i) = yb(i)
end do

do i = 1,nzb
  zbe(i) = zb(i)
end do

return

end
