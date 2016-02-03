!*******************************************************************************
!$RCSfile: set_grid.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine set_grid
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Setup calculation grid variables
!           Assumes uniform x and y grids
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 set_min                 set_max          WarningMessage
!                max_vres
!
! REVISION HISTORY: 
!
! 03 NOV 2000 : Added check on puff grid dimension - RIS
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc

implicit none
 
! --- LOCALS

logical lgrd

real dxx, dyy, hresx, hresy, zmin

integer ios, max_vres, max_grd

!------ Set default calculation domain

if (xmin == DEF_VAL_R) call set_min(xmin,xb,xbl,nxb,nxbl)

if (ymin == DEF_VAL_R) call set_min(ymin,yb,ybl,nyb,nybl)

if (xmax == DEF_VAL_R) call set_max(xmax,xb,xbl,nxb,nxbl)

if (ymax == DEF_VAL_R) call set_max(ymax,yb,ybl,nyb,nybl)

if (xmin == DEF_VAL_R .or. xmax == DEF_VAL_R .or. &
         ymin == DEF_VAL_R .or. ymax == DEF_VAL_R) then
  nError   = UK_ERROR
  eRoutine = 'set_grid'
  eMessage = 'Must set calculation domain'
  eInform  = 'File='//TRIM(file_inp)
  go to 9999
end if

!------ check puff and met background grids

if (nxb > 1 .and. nyb > 1) then
  lgrd = (xmin < xb(1)-dxb) .or. (xmax > xb(nxb)+dxb) .or. &
              (ymin < yb(1)-dyb) .or. (ymax > yb(nyb)+dyb)
else
  lgrd = .false.
end if

if (lgrd) then
  nError   = DM_ERROR
  eRoutine = 'set_grid'
  eMessage = 'Project domain extends beyond met/terrain domain'
  write(eInform,'(a,4f10.2)') &
               'Met Domain     : ',xb(1),xb(nxb),yb(1),yb(nyb)
  write(eAction,'(a,4f10.2)') &
               'Project Domain : ',xmin,xmax,ymin,ymax
  call WarningMessage(0,.true.)
  if (nError /= NO_ERROR) then
    nError   = WN_ERROR
    go to 9999
  else
    xmin = max(xmin,xb(1))
    xmax = min(xmax,xb(nxb))
    ymin = max(ymin,yb(1))
    ymax = min(ymax,yb(nyb))
    if (xmin > xmax .or. ymin > ymax) then
      nError   = DM_ERROR
      eMessage = 'Project domain is outside the met/terrain grid'
      go to 9999
    end if
  end if
end if

!------ Vertical resolution

zmin = 0.
nz   = min0(max0(nint(zmax/vres),1),max_vres())
dzg  = zmax/float(nz)

!------ Horizontal

if(hres == DEF_VAL_R)then
  if (max0(nxb,nxbl) == 1) then
    dxx = xmax - xmin
  else
    dxx = min(dxb,dxbl)
  end if
  if (max0(nyb,nybl) == 1) then
    dyy = ymax - ymin
  else
    dyy = min(dyb,dybl)
  end if
  hresx = dxx
  hresy = dyy
else
  hresx = hres
  hresy = hres
end if

hresx = hresx*(2**mgrd)
hresy = hresy*(2**mgrd)

if (xmax <= xmin) xmax = xmin + hresx
if (ymax <= ymin) ymax = ymin + hresy

nx  = max0(nint((xmax-xmin)/hresx + 0.499999),1)
dxg = (xmax-xmin)/float(nx)

ny  = max0(nint((ymax-ymin)/hresy + 0.499999),1)
dyg = (ymax-ymin)/float(ny)

if (nx == 1 .and. ny == 1) then
  delx2 = (1.e6*max(dxg,dyg))**2
else
  delx2 = (dxsplt*min(dxg,dyg))**2
end if
delz2 = (dzsplt*dzg)**2

!------ write out puff grid

write(lun_log,100,iostat=ios)
write(lun_log,101,iostat=ios) xmin,xmax,dxg
write(lun_log,102,iostat=ios) ymin,ymax,dyg
write(lun_log,103,iostat=ios) zmin,zmax,dzg
write(lun_log,*,iostat=ios)' '
if (ios /= 0) then
  nError   = WR_ERROR
  eRoutine = 'set_grid'
  eMessage = 'Error writing SCICHEM log file'
  eInform  = 'File='//TRIM(file_log)
end if

!------ check grid size for IPGRD

if (nx*ny > max_grd()) then
  nx = 2
  ny = 2
  dxg = (xmax-xmin)/float(nx)
  dyg = (ymax-ymin)/float(ny)
end if

9999    continue

return

!------ formats

100     format(/,'Computational domain')
101     format('xmin,xmax,dx: ',1p,3e12.4)
102     format('ymin,ymax,dy: ',1p,3e12.4)
103     format('zmin,zmax,dz: ',1p,3e12.4)

end


subroutine set_min(xmin,x1,x2,n1,n2)
!*******************************************************************************
!
! FUNCTION:  Set minimum for computational domain based on met and bl grids
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
 
real xmin      !minimum
real x1, x2    !met and bl grids
integer n1, n2 !number of grid points in the met and bl grids

dimension x1(*), x2(*)


if (n1 > 1) then
  if (n2 > 1) then
    xmin = min(x1(1),x2(1))
  else
    xmin = x1(1)
  end if
else
  if (n2 > 1) then
    xmin = x2(1)
  end if
end if

return
end


subroutine set_max(xmax,x1,x2,n1,n2)
!*******************************************************************************
!
! FUNCTION: Set maximum for computational domain based on met and bl grids
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
 
real xmax      !maximum
real x1, x2    !met and bl grids
integer n1, n2 !number of grid points in the met and bl grids

dimension x1(*), x2(*)


if (n1 > 1) then
  if (n2 > 1) then
    xmax = max(x1(n1),x2(n2))
  else
    xmax = x1(n1)
  end if
else
  if (n2 > 1) then
    xmax = x2(n2)
  end if
end if

return
end
