!*******************************************************************************
!$RCSfile: dezone.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine dezone(srf)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Combine refined cells if relative variations in mean fields
!            are less than EPS_DEZONE
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              check_cell           compress_grid
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use common_srf

implicit none

! --- ARGUMENTS

type ( grid_str ) srf  !Surface structure

! --- PARAMETERS

real        EPS_DEZONE
real        C_CUTOFF
parameter ( EPS_DEZONE  = 0.9)
parameter ( C_CUTOFF    = 0.0 )

! --- LOCALS

integer  lgridp(MAXSG/4)
integer  m0, n0, ncell, nvart, nvarc, mn0, i, ip
integer  ix, iy, icell0, j

real          c0(MAXSTYP)
integer ic_srf(MAXSTYP)

logical lock

!------ Set local variables

m0    = srf%nx
n0    = srf%ny
ncell = srf%ncells
nvart = srf%nvart

ip = srf%ipnam
nvarc = 0
do j = 1,nvart
  if (srfnam(ip+j)(1:1) == 'M' .or. srfnam(ip+j)(1:1) == 'D') then
    nvarc = nvarc + 1
    ic_srf(nvarc) = j
  end if
end do

!------ Set backward pointers

mn0 = m0*n0 - 3

do i = 1,ncell
  if (srfgrd(i,srf%ipgrd) > 0) then
    ip = (srfgrd(i,srf%ipgrd)-mn0)/4
    lgridp(ip) = i
  end if
end do

!------ Start on level-0 grid

do ix = 1,m0
  do iy = 1,n0

!------ Locate position on level-0

    icell0 = (iy-1)*m0 + ix

!------ Check for dezone

    if (srfgrd(icell0,srf%ipgrd) /= 0) then
      do i = 1,nvarc
        c0(i) = C_CUTOFF
      end do
      call check_cell(icell0,MAXSG,srfdat(1,srf%ipdat+1), &
                                        srfgrd(1,srf%ipgrd),lgridp, &
                             nvart,nvarc,ic_srf,mn0,EPS_DEZONE,lock,c0)
    end if

  end do
end do

!------ Dezone grid

call compress_grid(srf,lgridp)

return

end


recursive subroutine check_cell(icell0,maxd,cgrid,lgrid,lgridp,nvart,nvarc,ic_srf, &
                                                                     mn0,eps,lock,c0)
!*******************************************************************************
!
! FUNCTION:  Check if a cell can be dezoned
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              check_cell
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use param_inc

implicit none

! --- ARGUMENTS

integer icell0        !Current cell
integer maxd          !Dimension
integer lgrid, lgridp !Surface grids
integer nvart, nvarc  !Number of variables
integer ic_srf(*)     !Pointer to variable list
integer mn0           !Top level grid
real    eps           !Dezone controlling parameter
real    cgrid         !Surface grid data
real    c0            !Dezone concentration limit
logical lock          !Dezone refinement flag

dimension lgrid(*)
dimension lgridp(*)
dimension cgrid(maxd,*)
dimension c0(*)

! --- LOCALS

logical lockn

integer   i0, i, ic, icell, i1, i2, i3, ip
real      c1,cmax, cmin, cbar
dimension c1(MAXSTYP)

!------ Initialize

lock  = .false.
i0    = lgrid(icell0)
do i  = 1,nvarc
  ic = ic_srf(i)
  c1(i) = c0(i) + cgrid(icell0,ic)
end do

!------ Check the four sub-cells

do i = 0,3

  icell = i0 + i

  if (lgrid(icell) /= 0) then
    call check_cell(icell,maxd,cgrid,lgrid,lgridp,nvart,nvarc, &
                             ic_srf,mn0,eps, lockn,c1)
    lock = lock .or. lockn
  end if

end do

!------ If any of the 4 cells are still refined then can't dezone

if (lock) return

!------ No refinements on this level, so check all types/groups for dezone.
!       Dezone if data variation small, else set lock=T

i1 = i0 + 1
i2 = i0 + 2
i3 = i0 + 3

do i = 1,nvarc
  ic = ic_srf(i)
  cmax = max(cgrid(i0,ic),cgrid(i1,ic),cgrid(i2,ic),cgrid(i3,ic))
  cmin = min(cgrid(i0,ic),cgrid(i1,ic),cgrid(i2,ic),cgrid(i3,ic))
  cmin = cmin + c0(i)
  cmax = cmax + c0(i)
  if (cmax /= 0.) then
    if (cmin/cmax < eps) then
      lock = .true.
    end if
!         else
!          lock = .true.
  end if
end do

if (.not.lock) then
  ip   = (i0 - mn0)/4
  lgrid(icell0) = 0
  lgridp(ip)    = 0
  do i = 1,nvart
    cbar = 0.25*(cgrid(i0,i)+cgrid(i1,i)+cgrid(i2,i)+cgrid(i3,i))
    cgrid(icell0,i) = cgrid(icell0,i) + cbar
  end do
end if

return
end

subroutine compress_grid(srf,lgridp)
!*******************************************************************************
!
! FUNCTION:  Remove unused cells from "dezoned" grid
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_srf

implicit none

! --- ARGUMENTS

type ( grid_str ) srf  !Surface structure

integer  lgridp(*)     !Surface grid

! --- LOCALS

integer  m0, n0, ncell, nvart, mn0, i, ip
integer  nstart, iout, j, ic, ipo

m0    = srf%nx
n0    = srf%ny
ncell = srf%ncells
nvart = srf%nvart

nstart = m0*n0 + 1
iout   = nstart
mn0    = m0*n0 - 3

do i = nstart,ncell,4

  ip = (i - mn0)/4

  if (lgridp(ip) /= 0) then

    srfgrd(lgridp(ip),srf%ipgrd) = iout

    do j = 0,3
      srfgrd(iout,srf%ipgrd) = srfgrd(i+j,srf%ipgrd)
      do ic = 1,nvart
        srfdat(iout,srf%ipdat+ic) = srfdat(i+j,srf%ipdat+ic)
      end do
      if (srfgrd(iout,srf%ipgrd) /= 0) then
        ipo = (srfgrd(iout,srf%ipgrd)-mn0)/4
        lgridp(ipo) = iout
      end if
      iout = iout + 1
    end do

  end if

end do

srf%ncells = iout - 1

return
end

subroutine dezone_lev(srf)
!*******************************************************************************
!
! FUNCTION:  Remove lowest grid level cells
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!             get_max_lev          check_cell_lev           compress_grid
!                  mapfac
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use common_srf

implicit none

! --- ARGUMENTS

type ( grid_str ) srf !Surface structure

! --- LOCALS

integer  lgridp(MAXSG/4)
integer  m0, n0, ncell, nvart, mn0, i, ip
integer  ix, iy, icell0, j
integer  mlev, mxlev
real       xbar,ybar,xmap,ymap

integer ic_srf(MAXSTYP)


!------ Set local variables

m0    = srf%nx
n0    = srf%ny
ncell = srf%ncells
nvart = srf%nvart
mxlev = srf%maxlev
if (mxlev < 0) then
  call get_max_lev(srf,srfgrd(1,srf%ipgrd),mxlev)
end if

!------ Set flags for dezoning the variables

ip = srf%ipnam
do j = 1,nvart
  if (srfnam(ip+j)(1:1) == 'M') then
    ic_srf(j) = 1
  else if (srfnam(ip+j)(1:1) == 'V') then
    ic_srf(j) = 2
  else if (srfnam(ip+j)(1:1) == 'S') then
    ic_srf(j) = 3
  else if (srfnam(ip+j)(1:1) == 'D') then
    ic_srf(j) = 1
  else if (srfnam(ip+j)(1:1) == 'X') then
    ic_srf(j) = 2
  else
    ic_srf(j) = 0
  end if
end do

!------ Set backward pointers

mn0 = m0*n0 - 3

do i = 1,ncell
  if (srfgrd(i,srf%ipgrd) > 0) then
    ip = (srfgrd(i,srf%ipgrd)-mn0)/4
    lgridp(ip) = i
  end if
end do

!------ Start on level-0 grid

do ix = 1,m0
  do iy = 1,n0

!------ Locate position on level-0

    mlev   = 0
    icell0 = (iy-1)*m0 + ix

!------ Check for refinement

    if (srfgrd(icell0,srf%ipgrd) /= 0) then
      call check_cell_lev(icell0,MAXSG,srfdat(1,srf%ipdat+1), &
                             srfgrd(1,srf%ipgrd),lgridp, &
                             nvart,ic_srf,mn0,mlev,mxlev)
    end if

  end do
end do

!------ Dezone grid

call compress_grid(srf,lgridp)

srf%maxlev = mxlev - 1
xbar = srf%xmin + 0.5*(srf%nx-1)*srf%dx
ybar = srf%ymin + 0.5*(srf%ny-1)*srf%dy
call mapfac( xbar , ybar , xmap , ymap )
srf%delmin = min(srf%dx/xmap,srf%dy/ymap)*(0.5**srf%maxlev)

return

end


recursive subroutine check_cell_lev(icell0,maxd,cgrid,lgrid,lgridp,nvart, &
                                                        ic_srf,mn0,mlev,mxlev)
!*******************************************************************************
!
! FUNCTION:  Check cell level
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          check_cell_lev
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

implicit none

! --- ARGUMENTS

integer icell0          !Current cell
integer maxd            !Dimension
real    cgrid           !Surface grid data
integer lgrid, lgridp   !Surface grids
integer nvart           !Total number of surface variables
integer ic_srf(*)       !Pointer to surface variables
integer mn0             !Top grid level
integer mlev, mxlev     !Refinement levels

dimension lgrid(*)
dimension lgridp(*)
dimension cgrid(maxd,*)

! --- LOCALS

integer i0, i, ic, icell, ip, mlevn

real cbar, sum, vsum


!------ Initialize

i0    = lgrid(icell0)
mlevn = mlev + 1

!------ Check the four sub-cells

do i = 0,3

  icell = i0 + i

  if (lgrid(icell) /= 0) then
    call check_cell_lev(icell,maxd,cgrid,lgrid,lgridp,nvart, &
                             ic_srf,mn0,mlevn,mxlev)
  end if

end do

!------ If at level max lev, remove 4 cells; otherwise return

if (mlevn < mxlev) return

!------ Set the grid pointers

ip   = (i0 - mn0)/4
lgrid(icell0) = 0
lgridp(ip)    = 0

!------ Add the 4 subcells into the parent cell

do ic = 1,nvart

  sum = 0.

  do i = 0,3
    icell = i0 + i
    sum = sum + cgrid(icell,ic)
  end do

  cgrid(icell0,ic) = cgrid(icell0,ic) + 0.25*sum

  if (ic_srf(ic) == 1) then
    cbar = 0.25*sum
    vsum = 0.
    do i = 0,3
      icell = i0 + i
      vsum  = vsum + (cgrid(icell,ic)-cbar)**2
    end do
  else if (ic_srf(ic) == 2) then
    cgrid(icell0,ic) = cgrid(icell0,ic) + 0.25*vsum
    if (sum <= 1.e-30) then
      vsum = 0.
    else
      vsum = vsum/sum
    end if
  else if (ic_srf(ic) == 3) then
    cgrid(icell0,ic) = cgrid(icell0,ic) + 0.25*vsum*sum
  end if

end do

return
end
