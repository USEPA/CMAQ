!*******************************************************************************
!$RCSfile: get_top.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine get_topog(x,y,h,hx,hy)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Find topography height, slope and stretch factor at (x,y)
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!                    frac         interp_bilinear
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_met

implicit none

! --- ARGUMENTS

real x, y      !location to find topography
real h, hx, hy !topography height, slope and stretch factor

! --- LOCALS

real ratx, raty
integer ig, jg, i


call frac(x,xb(1),dxb,nxb,ratx,ig)

call frac(y,yb(1),dyb,nyb,raty,jg)

call interp_bilinear(ratx,raty,hs,nxb,ig,jg,h)

i = (jg-1)*nxb + ig
hx  = (1.-raty)*ddx(i) + raty*ddx(i+nxb)
hy  = (1.-ratx)*ddy(i) + ratx*ddy(i+1)

return
end


subroutine get_smooth_topog(x,y,h,hx,hy,sx,sy)
!*******************************************************************************
!
! FUNCTION:  Find topography height, slope and stretch factor at (x,y)
!            smooth slope over area determined by length scales sx & sy
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!                    frac         interp_bilinear                  mapfac
!
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES
use common_met

implicit none

! --- ARGUMENTS

real x, y      !location to find topography
real h, hx, hy !topography height, slope and stretch factor
real sx, sy    !smoothed slope and stretch factor

! --- PARAMETERS

real SLEN
parameter (SLEN = 2.0)

! --- LOCALS

real ratx, raty, rym1, rxm1, xmap, ymap
integer ig, jg, i, nsmthy, nsmthx, nsmth, is, js, igs, jgs
integer ismthx, ismthy

call frac(x,xb(1),dxb,nxb,ratx,ig)

call frac(y,yb(1),dyb,nyb,raty,jg)

call interp_bilinear(ratx,raty,hs,nxb,ig,jg,h)

call mapfac(x,y,xmap,ymap)
nsmthx = int(SLEN*xmap*sx/dxb)
nsmthy = int(SLEN*ymap*sy/dyb)

rxm1 = 1.-ratx
rym1 = 1.-raty

nsmth = 0
hx    = 0.0
hy    = 0.0

do js = -nsmthy,nsmthy
  jgs = max0(1,min0(nyb-1,jg+js))
  ismthy = nsmthy+1-iabs(js)
  do is = -nsmthx,nsmthx
    igs = max0(1,min0(nxb-1,ig+is))
    i   = (jgs-1)*nxb + igs
    ismthx = nsmthx+1-iabs(is)
    hx  = hx + float(ismthx*ismthy)*(rym1*ddx(i) + raty*ddx(i+nxb))
    hy  = hy + float(ismthx*ismthy)*(rxm1*ddy(i) + ratx*ddy(i+1))
    nsmth = nsmth + ismthx*ismthy
  end do
end do

hx = hx/float(nsmth)
hy = hy/float(nsmth)

return
end

subroutine frac(x,x0,dx,nx,rat,i)
!*******************************************************************************
!
! FUNCTION:  Find 1D interpolation factor and nearest grid number for get_topog
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

real x, x0  !x or y location to be interpolated to and x or y origin
real dx     !x or y grid length
real rat    !1D interpolation factor
integer nx  !number of grid points total
integer i   !nearest grid number

! --- LOCALS

real xfrac

xfrac = (x-x0)/dx
xfrac = max(xfrac,0.0)
xfrac = min(xfrac,float(nx-1))
i    = min0(int(xfrac) + 1,nx-1)
rat  = xfrac - float(i-1)
rat  = max(0.,min(rat,1.))

return
end

subroutine interp_bilinear(rx,ry,hs,nx,i,j,h)
!*******************************************************************************
!
! FUNCTION:  Perform a bilinear interpolation for the terrain height
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

real rx, ry  !1D interpolation factors
real hs(*)   !2D terrain heights
real h       !interpolated terrain height
integer nx   !number of x grid points
integer i, j !grid numbers of nearest grid point

! --- LOCALS

integer ip
real c1, c2, c3, c4

c1 = (1.-rx)*(1.-ry)
c2 = (1.-rx)*ry
c3 = rx*ry
c4 = rx*(1.-ry)

ip = (j-1)*nx + i
h  = c1*hs(ip) + c2*hs(ip+nx) + c3*hs(ip+nx+1) + c4*hs(ip+1)

return
end
