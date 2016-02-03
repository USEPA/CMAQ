!*******************************************************************************
!$RCSfile: accumsrf.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine accum_surf(x,y,lgrid,cgrd,ig,ng,nv,nvt,ccell, &
                             mlev,ncell,m0,MAXG,icell)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Accumulate mass on surface grid
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!              accum_cell
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use error_inc

implicit none

! -- ARGUMENTS

real x, y         !Grid location
real cgrd         !Surface grid data
real ccell(*)     !Puff contribution added to surface grid data
integer lgrid(*)  !Surface grid definition
integer ig(*)     !Array of surface fields that are accumulating
integer ng        !Number of surface fields that are accumulating
integer nv        !Not used
integer nvt       !Total number of variables on the grid
integer mlev      !Refinement level
integer ncell     !Cell number
integer m0        !Top level grid
integer MAXG      !Dimension, Max grid cells
integer icell     !Starting cell number (a hint)

dimension cgrd(MAXG,*)

! -- PARAMETERS

real       EPS
parameter (EPS = 1.0e-6)

! -- LOCALS

real, save  :: c0
real        :: xc, yc, ctst
integer     :: ix, iy, igx, ilev, iv, jcell

ctst = abs(ccell(1))
igx  = ig(1)

if (icell == 0) then

  xc = x
  yc = y

  ix = int(xc)
  iy = int(yc)
  jcell = iy*m0 + ix + 1

  c0  = cgrd(jcell,igx)
  if (ctst < EPS*abs(c0)) then
    go to 9999
  end if

  do ilev = 1,mlev

    if (lgrid(jcell) == 0) then
      if (ncell+4 > MAXG) then
        nError   = SZ_ERROR
        eRoutine = 'accum_surf'
        go to 9999
      end if
      call accum_cell(jcell,lgrid,cgrd,nvt,ncell,MAXG)
    end if

    xc = xc - float(ix)
    yc = yc - float(iy)

    xc = xc + xc
    yc = yc + yc

    ix = int(xc)
    iy = int(yc)

    jcell = lgrid(jcell) + iy + iy + ix

    c0 = c0 + cgrd(jcell,igx)
    if (ctst < EPS*abs(c0)) then
      go to 9999
    end if

  end do

  if (ix == 0) icell = jcell
  c0 = c0 - cgrd(jcell,igx)

else

  jcell = icell + 1
  icell = 0
  c0 = c0 + cgrd(jcell,igx)

  if (abs(ccell(1)) < EPS*abs(c0)) then
    go to 9999
  end if

end if

do iv = 1,ng
  cgrd(jcell,ig(iv)) = cgrd(jcell,ig(iv)) + ccell(iv)
end do

9999    continue

return
end

subroutine accum_cell(icell,lgrid,cgrd,nv,ncell,MAXD)
!*******************************************************************************
!
! FUNCTION:  Refine cell number ICELL of the adaptive grid
!            pointer list LGRID
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
!*******************************************************************************
!
!  ncell is the total number of cells in the grid

implicit none

! -- ARGUMENTS

integer  icell    !Current cell
integer  lgrid    !Surface grid definition
integer  nv       !Total number of variables
integer  ncell    !Number of grid cells
integer  MAXD     !Dimension
real     cgrd     !Surface grid data

dimension lgrid(*)
dimension cgrd(MAXD,*)

! -- LOCALS

integer  ii, iv

lgrid(icell) = ncell + 1

do ii = 1,4
  lgrid(ncell+ii) = 0
  do iv = 1,nv
    cgrd(ncell+ii,iv) = 0.0
  end do
end do

ncell = ncell + 4

return
end
