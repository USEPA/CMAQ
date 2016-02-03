subroutine init_met_3d
!******************************************************************************
! Developed by Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION: Initialize variables for getting met fields for the grid cell
!           where a puff is located (rather than interpolating 3-D or 2-D
!           met fields to the puff location)
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: Version 1.0, May 2006, PK, AER
!
!******************************************************************************
 
! --- MODULES
 
use common_met

implicit none

!=== set grid to host model grid
x0m = xb(1) - 0.5*dxb  ! left edge of domain
y0m = yb(1) - 0.5*dyb  ! bottom edge of domain

xmaxm = x0m + dxb*nxb  ! right edge of domain
ymaxm = y0m + dyb*nyb  ! top edge of domain
zmaxm = zb(nzb)

return
end

subroutine get_met_xyz(x,y,z)
!******************************************************************************
!
! FUNCTION:  Get 3-D met variables at location x,y,z
!            (finds nearest grid point)
!            currently 3-D cloud fields only
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_topog
!
! REVISION HISTORY: Version 1.0, May 2006, PK, AER
!
! Jul 2006     : Updated calculation of cloud variables, PK, AER
!******************************************************************************
 
! --- MODULES
 
use common_met
use common_puf

implicit none

! --- ARGUMENTS
 
real x, y, z  !Location

! --- LOCALS

real xp,yp,zp
integer i, j, k, ix, iy, iz, ijk, icld
real dum1, dum2, dp, hp

! -- transform z
if (lter) then
  call get_topog(x,y,hp,dum1,dum2)
  dp = 1. - hp/zbtop
  zp = (z - hp)/dp
else
  zp = z
end if

! -- limit coordinates
xp = min(xmaxm,max(x0m,x))
yp = min(ymaxm,max(y0m,y))
zp = min(zmaxm,max(0.,zp))

! -- find spatial indices
ix = min(max(1,int((xp - x0m)/dxb) + 1),nxb)
iy = min(max(1,int((yp - y0m)/dyb) + 1),nyb)

do i = 1,nzb - 1
   if (zp >= zbw(i) .and. zp < zbw(i+1)) then
      iz = i
      go to 45
   end if
end do

iz = nzb

45   continue

! Convert ix, iy, iz to met grid ijk. Note: 1st met layer is in the ground
! so if puff is in layer 1, use 2nd met layer
ijk = iz * nxyb + (iy - 1) * nxb + ix

! -- get cloud fields for this location
cldall = 0.
cldallt = 0.
cldallp = 0.
if (cldflag) then
  do icld = 1, ncld
    cldall = cldall + cld_ua(ijk,icld)
    cldallt = cldallt + cldt_ua(ijk,icld)
    cldallp = cldallp + cldp_ua(ijk,icld)
  end do
end if

return
end

subroutine get_met_xy(x,y)
!******************************************************************************
!
! FUNCTION:  Get 2-D met variables at location x,y,z
!            (finds nearest grid point)
!            currently 2-D cloud fields only
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: Version 1.0, May 2006, PK, AER
!
! Jul 2006     : Updated to treat wet deposition by convective precipitation
!                correctly and to consider ratio of water in puff to total
!                water in grid column- PK, AER
!******************************************************************************
 
! --- MODULES
 
use common_met

implicit none

! --- ARGUMENTS
 
real x, y  !Location

! --- LOCALS

real xp,yp
integer i, j, ix, iy, ij

! -- limit coordinates
xp = min(xmaxm,max(x0m,x))
yp = min(ymaxm,max(y0m,y))

! -- find spatial indices
ix = min(max(1,int((xp - x0m)/dxb) + 1),nxb)
iy = min(max(1,int((yp - y0m)/dyb) + 1),nyb)

! Convert ix, iy to met grid ij.
ij = (iy - 1) * nxb + ix

! -- get cloud fields for this location
cldtop  = cldtop2(ij)
cldbot  = cldbot2(ij)
fcc     = fcc_bl(ij)
pratebl = prate_bl(ij)
wcbar   = lwc2(ij)

fprcpc  = fprcpc_bl(ij)

cmasst = cldmasst(ij)
cmassp = cldmassp(ij)

return
end
