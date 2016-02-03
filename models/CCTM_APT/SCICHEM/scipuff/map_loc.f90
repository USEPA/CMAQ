!*******************************************************************************
!$RCSfile: map_loc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine map_loc(map_in,xin,yin,map_out,xout,yout,xmap,ymap)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Returns location in output coordinate system (Cartesian or Lat/lon)
!       as well as the map transformation.  Cartesian coordinates are assumed
!       to be in km.  
!
! PRECONDITIONS REQUIRED:  The reference lat/lon must be defined prior to 
!                          calling this routine for lat/lon input/output.
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
 
integer map_in, map_out  !Coordinates to transform from and to

real xin,  yin           !X and Y location in map_in  coordinate system
real xout, yout          !X and Y location in map_out coordinate system
real xmap, ymap          !Conversion factors to go from map_in to map_out


!------ output location is same as input if coord. systems are identical

if (map_out == map_in .or. &
         map_out == I_CARTESIAN .and. map_in  == I_UTM .or. &
         map_in  == I_CARTESIAN .and. map_out == I_UTM) then

  xout = xin
  yout = yin
  xmap = 1.
  ymap = 1.

  go to 9999

end if

!------ check if reference location has been set for lat/lon input/output

if (map_in == I_LATLON .or. map_out == I_LATLON) then
  if (lon0 == NOT_SET_R .or. lat0 == NOT_SET_R) then
    nError   = IV_ERROR
    eRoutine = 'map_loc'
    eMessage = 'Must set map reference location'
    eInform  = 'Run / Met domains in different coord. systems'
    go to 9999
  end if
end if

!------ if output is Lat/lon  : xmap = dx(out)/dx(in) = 180/(pi*R*cos(lat0))
!                                 ymap = dy(out)/dy(in) = 180/(pi*R)
!                                 xout = lon0 + (xin-xref)*xmap
!                                 yout = lat0 + (yin-yref)*ymap

if (map_out == I_LATLON) then

  if (map_in == I_METERS) then
    xmap = 1.
  else
    xmap = 1.e3
  end if

  ymap = sphfacr * xmap
  xmap = ymap/cos(pi180*lat0)
  xout = lon0 + (xin - xref)*xmap
  yout = lat0 + (yin - yref)*ymap

else if (map_in == I_LATLON) then

!------ if input is Lat/lon and output is Cartesian:
!                               xmap = dx(out)/dx(in) = R*pi/180*cos(lat0)
!                                 ymap = dy(out)/dy(in) = R*pi/180
!                                 xout = xref + (xin-lon0)*xmap
!                                 yout = yref + (yin-lat0)*ymap

  if (map_out == I_METERS) then
    xmap = 1.
  else
    xmap = 1.e-3
  end if
  ymap = sphfac * xmap
  xmap = ymap*cos(pi180*lat0)
  xout = xref + (xin - lon0)*xmap
  yout = yref + (yin - lat0)*ymap

else if (map_in == I_CARTESIAN .or. map_in == I_UTM) then

!------ if input is Cartesian and output is Meters:

  xmap = 1.e3
  ymap = 1.e3
  xout = xin*xmap
  yout = yin*ymap

else

!------ if input is Meters and output is Cartesian: error

  nError   = IV_ERROR
  eRoutine = 'map_loc'
  eMessage = 'Cannot have meters input / kilometers output'
  go to 9999

end if

9999    continue

return

end


subroutine build_d
!*******************************************************************************
!
! FUNCTION: Build relative depth arrays and gradients
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  mapfac
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_met

implicit none

! --- LOCALS

integer i, j, is, ip, jp, ispx, ispy

real xmap, ymap, xbar, ybar, dxi, dyi

!------ d at grid "center" location

do i = 1,nxyb
  d(i) = 1.0 - hs(i)/zbtop
end do

dxi = 1./dxb
dyi = 1./dyb

!------ d and gradients at staggered (u,v) locations

do i = 1,nxb
  ip   = min0(i+1,nxb)
  xbar = 0.5*(xb(i)+xb(ip))
  do j = 1,nyb
    jp   = min0(j+1,nyb)
    is   = (j -1)*nxb + i
    ispx = (j -1)*nxb + ip
    ispy = (jp-1)*nxb + i
    ybar = 0.5*(yb(j)+yb(jp))
    call mapfac( xbar , yb(j) , xmap , ymap )
    d1(is)  = 0.5*(d(is)+d(ispx))
    ddx(is) = -(d(ispx) - d(is))*zbtop*dxi*xmap
    call mapfac( xb(i) , ybar , xmap , ymap )
    d2(is)  = 0.5*(d(ispy) + d(is))
    ddy(is) = -(d(ispy) - d(is))*zbtop*dyi*ymap
  end do
end do

!------ generate z-array (1d) for staggered grids only; set d1 = d2 = d
!       for unstaggered grids

if (lstagger) then
  do i = 1,nzb
    zz(i) = zbtop - zbw(i)
  end do
  zz(nzb+1) = -zz(nzb-1)
else
  do i = 1,nxyb
    d1(i) = d(i)
    d2(i) = d(i)
  end do
end if

return

end

