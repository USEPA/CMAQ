!*******************************************************************************
!$RCSfile: set_ip.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine set_ip(n1,n2)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Setup ipgrd and linked lists
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  mapfac               dump_puff               puff_grid
!           get_grid_cell               get_ipgrd               set_ipgrd
!                    grid
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
integer n1, n2   !Puff numbers to add (between n1 and n2)

! --- LOCALS

integer grid, ipuf, igrd, ip, k, jpuf
real xx, yy, xmap, ymap

!------ loop over puffs

do ipuf = n1, n2

!------ find appropriate ipgrd location

  call mapfac(puff(ipuf)%xbar , puff(ipuf)%ybar , xmap , ymap)

  igrd = grid(puff(ipuf)%sxx,puff(ipuf)%syy,xmap,ymap)
  if(nError /= NO_ERROR)then
    eRoutine = 'set_ip'
    call dump_puff(ipuf,puff(ipuf))
    go to 9999
  end if

  call puff_grid(puff(ipuf),xx,yy,k)

!------- check for other puffs in grid

  call get_grid_cell(xx,yy,k,igrd,ip)

  call get_ipgrd(ip,2,k,jpuf)

!------- no other puff in box

  if (jpuf == 0) then

    puff(ipuf)%inxt = 0
    puff(ipuf)%iprv = -(1000*ip + k)
    puff(ipuf)%ipgd = igrd

!------- puff(s) in box already;  add to top of list

  else

    puff(ipuf)%inxt = jpuf
    puff(ipuf)%iprv = -(1000*ip + k)
    puff(ipuf)%ipgd = igrd
    puff(jpuf)%iprv = ipuf

  end if

  call set_ipgrd(ip,2,k,ipuf)

end do

9999    continue

return
end


subroutine zero_ip
!*******************************************************************************
!
! FUNCTION:   Clear the puff grid
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               set_npgrd               set_mxlev               clr_ipgrd
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- LOCALS

integer i, k

do k = 1,nz
  call set_npgrd(k,nx*ny)
  call set_mxlev(k,0)
  do i = 1,nx*ny
    call clr_ipgrd(i,k)
  end do
end do

return

end


subroutine find_cell(x,y,k,mlev,m0,n0,jpuf)
!*******************************************************************************
!
! FUNCTION:  Find the first puff of a linked-list in this grid cell
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_ipgrd
!
! REVISION HISTORY: 
!
!*******************************************************************************
implicit none

! --- ARGUMENTS
 
real     x, y     !X and Y location of search
integer  k        !Vertical index
integer  mlev     !Grid level
integer  m0, n0   !Number of grid points in x and y
integer  jpuf     !Puff number

! --- PARAMETERS

real      EPS
parameter (EPS = 1.e-6)

! --- LOCALS

integer icell, ix, iy, ilev, jcell

real    xc, yc

xc = max(0.,min(float(m0)-EPS,x))
yc = max(0.,min(float(n0)-EPS,y))

ix = int(xc)
iy = int(yc)
icell = iy*m0 + ix + 1

if (mlev > 0) then

  do ilev = 1,mlev

    call get_ipgrd(icell,1,k,jcell)

    if (jcell == 0) then
      jpuf = 0
      return
    end if

    xc = xc - float(ix)
    yc = yc - float(iy)

    xc = xc + xc
    yc = yc + yc

    ix = int(xc)
    iy = int(yc)

    icell = jcell + iy + iy + ix

  end do

end if

call get_ipgrd(icell,2,k,jpuf)

return
end


subroutine find_grid_cell(xx,yy,k,igrd,ip)
!*******************************************************************************
!
! FUNCTION:  Find the first puff of a linked-list in this grid cell
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_ipgrd
!
! REVISION HISTORY: 
!
!*******************************************************************************

use common_puf

implicit none

! --- ARGUMENTS
 
real    xx, yy        !Nondimensional grid location
integer k             !Vertical grid level
integer igrd          !Horizontal grid level
integer ip            !Grid cell number

! --- PARAMETERS

real      EPS
parameter (EPS = 1.e-6)

! --- LOCALS

integer ix, iy, ilev, jcell

real    xc, yc

xc = max(0.,min(float(nx)-EPS,xx))
yc = max(0.,min(float(ny)-EPS,yy))

ix = int(xc)
iy = int(yc)
ip = iy*nx + ix + 1

if (igrd > 0) then

  do ilev = 1,igrd

    call get_ipgrd(ip,1,k,jcell)

    if (jcell == 0) then
      ip = 0
      return
    end if

    xc = xc - float(ix)
    yc = yc - float(iy)

    xc = xc + xc
    yc = yc + yc

    ix = int(xc)
    iy = int(yc)

    ip = jcell + iy + iy + ix

  end do

end if

return
end


subroutine get_grid_cell(xx,yy,k,igrd,ip)
!*******************************************************************************
!
! FUNCTION:  Find a grid cell number
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               set_mxlev                get_cell             dezone_grid
!               get_mxgrd
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
real    xx, yy        !Nondimensional grid location
integer k             !Vertical grid level
integer igrd          !Horizontal grid level
integer ip            !Grid cell number

! --- LOCALS

integer get_mxgrd, get_mxlev
logical lerr

call set_mxlev(k,max0(get_mxlev(k),igrd))

call get_cell(xx,yy,k,igrd,ip,nx,ny,lerr)

do while (lerr)
  call dezone_grid(k)
  igrd = min0(igrd,get_mxgrd())
  call get_cell(xx,yy,k,igrd,ip,nx,ny,lerr)
end do

return

end


subroutine get_cell(x,y,k,mlev,icell,m0,n0,lerr)
!*******************************************************************************
!
! FUNCTION:   Find the cell for a given location and refinement level
!             If it does not exist, it will be created
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_ipgrd              build_cell             check_npgrd
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real x, y         !Horizontal location
integer  k        !Vertical level
integer mlev      !Maximim refinement level
integer icell     !Grid cell number
integer m0, n0    !Number of grid points
logical lerr      !Error flag

! --- PARAMETERS

real EPS
parameter (EPS = 1.e-6)

! --- LOCALS

real xc, yc
integer ix, iy, ilev, jcell
logical   check_npgrd

lerr  = .false.

xc = max(0.,min(float(m0)-EPS,x))
yc = max(0.,min(float(n0)-EPS,y))

ix = int(xc)
iy = int(yc)
icell = iy*m0 + ix + 1

if (mlev == 0) go to 9999

do ilev = 1,mlev

  call get_ipgrd(icell,1,k,jcell)

  if (jcell == 0) then
    if (check_npgrd(k)) then
      lerr = .true.
      go to 9999
    end if
    call build_cell(icell,k)
    call get_ipgrd(icell,1,k,jcell)
  end if

  xc = xc - float(ix)
  yc = yc - float(iy)

  xc = xc + xc
  yc = yc + yc

  ix = int(xc)
  iy = int(yc)

  icell = jcell + iy + iy + ix

end do

9999    continue

return
end


subroutine build_cell(icell,k)
!*******************************************************************************
!
! FUNCTION:  Refine cell number ICELL of the adaptive grid pointer list LGRID
!            NCELL is the total number of cells in the grid
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               set_ipgrd               clr_ipgrd               set_npgrd
!               get_npgrd
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
integer icell    !Grid cell number
integer k        !Vertical level

! --- LOCALS

integer ncell, ii, get_npgrd

ncell = get_npgrd(k)

call set_ipgrd(icell,1,k,ncell + 1)

do ii = 1,4
  call clr_ipgrd(ncell+ii,k)
end do

call set_npgrd(k,ncell + 4)

return
end


subroutine dezone_grid(k)
!*******************************************************************************
!
! FUNCTION:  Reduce the refinement level when the number of cells becomes
!            too large and rebuild the puff linked lists
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_ipgrd         check_grid_cell               set_ipgrd
!               mov_ipgrd               set_npgrd               set_mxlev
!               set_mxgrd               get_npgrd
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use ipgrd_inc

implicit none

! --- ARGUMENTS
 
integer k      !Vertical level

! --- LOCALS

integer lgridp(MAXG/4), get_mxlev, jcell, jpuf, get_npgrd, ncell
integer mxy, i, ix, iy, ip, icell0, nstart, iout, mlev, j, ipo

ncell = get_npgrd(k)  !Number of cells on this level

!------ Set backward pointers

mxy = nx*ny - 3

do i = 1,ncell
  call get_ipgrd(i,1,k,jcell)
  if (jcell > 0) then
    ip = (jcell-mxy)/4
    lgridp(ip) = i
  end if
end do

!------ Start on level-0 grid

do ix = 1,nx
  do iy = 1,ny

!------ Locate position on level-0

    icell0 = (iy-1)*nx + ix
    mlev   = 0

!------ Check for dezone

    call get_ipgrd(icell0,1,k,jcell)
    if (jcell /= 0) then
      call check_grid_cell(icell0,k,lgridp,mxy,mlev)
    end if

  end do
end do

!------ Dezone grid

nstart = nx*ny + 1
iout   = nstart

do i = nstart,ncell,4

  ip = (i - mxy)/4

  if (lgridp(ip) /= 0) then

    call set_ipgrd(lgridp(ip),1,k,iout)

    do j = 0,3
      call mov_ipgrd(i+j,iout,k)
      if(i+j > iout)then
        call set_ipgrd(i+j,2,k,0)
        call get_ipgrd(iout,2,k,jpuf)
        if (jpuf /= 0) then
          puff(jpuf)%iprv = -(1000*iout + k)
        end if
      end if
      call get_ipgrd(iout,1,k,jcell)
      if (jcell /= 0) then
        ipo = (jcell-mxy)/4
        lgridp(ipo) = iout
      end if
      iout = iout + 1
    end do

  end if

end do

call set_npgrd(k,iout-1)

call set_mxlev(k,get_mxlev(k)-1)
call set_mxgrd(get_mxlev(k))

return

end


recursive subroutine check_grid_cell(icell0,k,lgridp,mn0,mlev)
!*******************************************************************************
!
! FUNCTION:   Add to linked lists for puffs in cells at a higher refinement level
!             for "dezoning"
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_ipgrd         check_grid_cell               set_ipgrd
!               get_mxlev
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
integer icell0    !Cell number to check
integer k         !Vertical level
integer lgridp(*) !Backward pointer in the linked list
integer mn0       !Top level grid size (nxg*nyg)
integer mlev      !Current grid level

! --- LOCALS

integer maxlev, get_mxlev 
integer i0, i, icell, ip, mlevn, ipuf, jpuf, jpufn, jcell, kpuf

!------ Initialize

maxlev = get_mxlev(k)
call get_ipgrd(icell0,1,k,i0)

!------ Check the four sub-cells

mlevn = mlev + 1

do i = 0,3

  icell = i0 + i

  call get_ipgrd(icell,1,k,jcell)
  if (jcell /= 0) then
    call check_grid_cell(icell,k,lgridp,mn0,mlevn)
  end if

end do

!------ If at level max lev, remove 4 cells; otherwise return

if (mlevn < maxlev) return

ip   = (i0 - mn0)/4
call set_ipgrd(icell0,1,k,0)
lgridp(ip)        = 0

call get_ipgrd(icell0,2,k,jpufn)
do while (jpufn /= 0)
  jpuf = jpufn
  jpufn = puff(jpuf)%inxt
end do

do i = 0,3

  icell = i0 + i
  call get_ipgrd(icell,2,k,ipuf)

  if (ipuf /= 0) then
    call set_ipgrd(icell,2,k,0)
    call get_ipgrd(icell0,2,k,kpuf)
    if (kpuf == 0) then
      call set_ipgrd(icell0,2,k,ipuf)
      puff(ipuf)%iprv = -1000*(icell0-icell) + puff(ipuf)%iprv
    else
      puff(jpuf)%inxt = ipuf
      puff(ipuf)%iprv = jpuf
    end if

    jpufn = ipuf
    do while (jpufn /= 0)
    puff(jpufn)%ipgd = puff(jpufn)%ipgd - 1
      jpuf = jpufn
      jpufn = puff(jpuf)%inxt
    end do

  end if

end do

return
end


subroutine find_mxgrd(nz)
!*******************************************************************************
!
! FUNCTION:  Check for relaxing mxgrd
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               set_mxgrd               get_mxlev               get_npgrd
!                 max_grd
!
! REVISION HISTORY: 
!
!*******************************************************************************

implicit none

! --- ARGUMENTS
 
integer nz    !Number of vertical grid levels

! --- LOCALS

integer mxlev, mxcell, k, get_mxlev, get_npgrd, get_mxgrd, max_grd

mxlev  = 0
mxcell = 0

do k = 1,nz
  mxlev  = max0(get_mxlev(k),mxlev)
  mxcell = max0(get_npgrd(k),mxcell)
end do

if (mxcell < max_grd()/4) then
  call set_mxgrd(max0(get_mxgrd(),mxlev+1))
end if

return
end


integer function max_vres()
!*******************************************************************************
!
! FUNCTION:  Set maximum number of vertical puff grid levels 
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use ipgrd_inc

implicit none


max_vres = MAXGZ

return
end


integer function max_grd()
!*******************************************************************************
!
! FUNCTION:   Set maximum number of horizontal puff grid cells
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use ipgrd_inc

implicit none


max_grd = MAXG

return
end


subroutine set_mxgrd(ival)
!*******************************************************************************
!
! FUNCTION:  Set the maximum grid refinement level
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use ipgrd_inc

implicit none

! --- ARGUMENTS
 
integer ival   !Maximum refinement level


mxgrd = ival

return
end


integer function get_mxgrd()
!*******************************************************************************
!
! FUNCTION:  Get maximum grid refinement level out of common
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use ipgrd_inc

implicit none


get_mxgrd = mxgrd

return
end


integer function get_mxlev(k)
!*******************************************************************************
!
! FUNCTION:  Get maximum refinement level on vertical level k
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use ipgrd_inc

implicit none

! --- ARGUMENTS
 
integer k    !Vertical level


get_mxlev = mxlev_grd(k)

return
end


subroutine set_mxlev(k,mxlev)
!*******************************************************************************
!
! FUNCTION:  Set the max refinement level for this vertical level
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use ipgrd_inc

implicit none

! --- ARGUMENTS
 
integer k      !Vertical level
integer mxlev  !Maximum refinement level


mxlev_grd(k) = mxlev

return
end


integer function get_npgrd(k)
!*******************************************************************************
!
! FUNCTION: Get number of cells on this vertical level
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use ipgrd_inc

implicit none

! --- ARGUMENTS
 
integer k      !Vertical level


get_npgrd = npgrd(k)

return
end


subroutine set_npgrd(k,igrd)
!*******************************************************************************
!
! FUNCTION:  Set number of cells on this vertical level
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use ipgrd_inc

implicit none

! --- ARGUMENTS
 
integer k     !Vertical level
integer igrd  !Number of cells


npgrd(k) = igrd

return
end


subroutine clr_ipgrd(i,k)
!*******************************************************************************
!
! FUNCTION:   Clear ipgrd array
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use ipgrd_inc

implicit none

! --- ARGUMENTS
 
integer i   !Horizontal cell
integer k   !Vertical 


ipgrd(i,1,k) = 0
ipgrd(i,2,k) = 0

return
end


subroutine mov_ipgrd(i,j,k)
!*******************************************************************************
!
! FUNCTION:  Move first puff in linked list from one cell to another
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use ipgrd_inc

implicit none

! --- ARGUMENTS
 
integer i, j  !Cell numbers
integer k     !Vertical grid level


ipgrd(j,1,k) = ipgrd(i,1,k)
ipgrd(j,2,k) = ipgrd(i,2,k)

return
end


subroutine set_ipgrd(i,j,k,ival)
!*******************************************************************************
!
! FUNCTION:  Set first puff in linked list for cell i, grid level k
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use ipgrd_inc

implicit none

! --- ARGUMENTS
 
integer i     !Cell number
integer j     ! = 1 or 2
integer k     !Vertical grid level
integer ival  !First puff in linked list


ipgrd(i,j,k) = ival

return
end


subroutine get_ipgrd(i,j,k,ival)
!*******************************************************************************
!
! FUNCTION:   Get first puff in linked list for cell i, grid level k
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use ipgrd_inc

implicit none

! --- ARGUMENTS
 
integer i     !Cell number
integer j     ! = 1 or 2
integer k     !Vertical grid level
integer ival  !First puff in linked list

ival = ipgrd(i,j,k)

return
end


logical function check_npgrd(k)
!*******************************************************************************
!
! FUNCTION:  Check if refinement will create too many cells
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use ipgrd_inc

implicit none

! --- ARGUMENTS
 
integer k     !Vertical level

check_npgrd =  npgrd(k)+4 > MAXG

return
end
