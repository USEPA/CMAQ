!*******************************************************************************
!$RCSfile: merge.f90,v $
!$Revision: 1.7 $
!$Date: 2010/11/29 15:05:06 $
!*******************************************************************************
subroutine merge(cgrid)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Merge puffs, setup ipgrd and linked lists
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 zero_ip                  mapfac               dump_puff
!               puff_grid           get_grid_cell               get_ipgrd
!               set_ipgrd               copy_puff  update_static_pointers
!        search_and_merge              find_mxgrd                    grid
!            chkgrd_merge
!
! REVISION HISTORY: 
! Aug 2010: Update with changes by PKK, AER on May 2005 for CMAQ-APT-PM -BC(SAGE-MGT)
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use interface_definitions, only: chkgrd_merge

implicit none

logical lok

! --- ARGUMENTS
 
REAL :: CGRID( :,:,:,: )  !3D ambient concentration (for chemistry runs)

! --- LOCALS

integer grid
integer ilev, ipuf, igrd,  jpuf, ipaux
integer in, jn, id, jd, i, j
integer k, k0, kl, ku, nk
integer ip, ip0, iout, jpuf0

real xmap, ymap, fmass, rxx, ryy, xx, yy
real xc, yc, dfac

!------ zero out ipgrd

call zero_ip

!------ loop over puffs

ilev  = 0
iout  = 0
ipaux = 1

do ipuf = 1,npuf

  call mapfac(puff(ipuf)%xbar , puff(ipuf)%ybar , xmap , ymap)

  fmass  = puff(ipuf)%c

!------ find appropriate ipgrd location

  igrd = grid(puff(ipuf)%sxx,puff(ipuf)%syy,xmap,ymap)
  if(nError /= NO_ERROR)then
    eRoutine = 'Merge'
    call dump_puff(ipuf,puff(ipuf))
    go to 9999
  end if

  call puff_grid(puff(ipuf),xx,yy,k0)

  rxx  = puff(ipuf)%sxx*xmap*xmap/(dxg*dxg)
  ryy  = puff(ipuf)%syy*ymap*ymap/(dyg*dyg)

!------- check for deleting puff

  lok = (chkgrd_merge(xx,yy,rxx,ryy,puff(ipuf),cgrid) .and.  &
               fmass > cmin .and. puff(ipuf)%idtl /= I_REMOVE)

  if(nError /= NO_ERROR) go to 9999

  if (lok .or. puff(ipuf)%idtl == I_STATIC)then
    if ( puff(ipuf)%szz > delz2 )then
      nk = int(2.*sqrt(puff(ipuf)%szz)/dzg)
      ku = min(k0 + nk,nz)
      kl = max(k0 - nk,1)
    else
      ku = k0
      kl = k0
    end if

    dfac = 0.5**igrd

    xc = xx/dfac
    xc = xc - INT(xc)
    if (xc > 0.5) then
      if (xx+dfac > float(nx) ) then
        in = 0
      else
        in = 1
      end if
      id = 1
    else
      if (xx-dfac < 0.0) then
        in = 0
      else
        in = -1
      end if
      id = -1
    end if

    yc = yy/dfac
    yc = yc - INT(yc)
    if (yc > 0.5) then
      if (yy+dfac > float(ny) ) then
        jn = 0
      else
        jn = 1
      end if
      jd = 1
    else
      if (yy-dfac < 0.0) then
        jn = 0
      else
        jn = -1
      end if
      jd = -1
    end if

    Xloop: do i = 0,in,id
      xc = xx + float(i)*dfac

      Yloop: do j = 0,jn,jd
        yc = yy + float(j)*dfac

        Zloop: do k = kl,ku

          call find_grid_cell(xc,yc,k,igrd,ip)

          if (ip > 0) then
            call get_ipgrd(ip,2,k,jpuf)
          else
            jpuf = 0
          end if

!------- check other puffs in box

          if (jpuf > 0) then
            call search_and_merge(ipuf,jpuf,igrd,xmap,ymap,ilev,k,ip)
            if( jpuf == 0 )then
              jpuf0 = -1
              exit Xloop
            end if
          end if

          if( k == k0 .and. i==0 .and. j==0 )then   !Save local cell info
            jpuf0 = jpuf
            ip0   = ip
          end if

        end do Zloop
 
      end do Yloop

    end do Xloop

    if( jpuf0 == 0 )then   ! First puff in box

      if (ip0 == 0) then
        call get_grid_cell(xx,yy,k0,igrd,ip0)
        if (nError /= NO_ERROR) goto 9999
      end if

      iout = iout + 1
      call set_ipgrd(ip0,2,k0,iout)
      call copy_puff(puff(ipuf),puff(iout),ipaux)
      puff(iout)%inxt = 0
      puff(iout)%iprv = -(1000*ip0 + k0)
      puff(iout)%ipgd = igrd
      if (puff(ipuf)%idtl == I_STATIC .and. ipuf/=iout) then
        call update_static_pointers(iout)
      end if

    else if( jpuf0 > 0 )then ! No merge - add to list

      iout = iout + 1
      call copy_puff(puff(ipuf),puff(iout),ipaux)
      puff(jpuf0)%inxt = iout
      puff(iout)%inxt  = 0
      puff(iout)%iprv  = jpuf0
      puff(iout)%ipgd  = igrd
      if (puff(ipuf)%idtl == I_STATIC .and. ipuf/=iout) then
        call update_static_pointers(iout)
      end if
    end if
  end if
end do

npuf  = iout
npaux = ipaux

!------ check for relaxing mxgrd

call find_mxgrd(nz)

9999    continue

return
end

subroutine search_and_merge(ipuf,jpuf,igrd,xmap,ymap,ilev,k,ip)
!*******************************************************************************
!
! FUNCTION:  Search linked list, merge if deemed appropriate
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          get_overlp_arg, pmerge, puff_grid, get_grid_cell, get_ipgrd, set_ipgrd
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
integer ipuf, jpuf !2 puffs in same grid box
integer igrd       !Grid box level
integer ilev       !Puff time levels
real xmap, ymap    !Map factors
integer k          !Vertical grid level of search cell 
integer ip         !Search cell ID

! --- LOCALS

integer ityp, jtyp, jpufn, iprv, inxt, ipn, kn
real s1, s2, betx, bety, betz, arg, xn, yn
logical l1, l2

integer icount, ncount
integer, parameter :: MAXCOUNT=10000
integer idjpuf(MAXCOUNT)

jpufn = jpuf

!------ do not merge static puffs but still go to end of list

if (puff(ipuf)%idtl < ilev) then
  do while (jpufn /= 0)
    jpuf = jpufn
    jpufn = puff(jpuf)%inxt
  end do
  return
end if

ityp = puff(ipuf)%ityp
if (puff(ipuf)%cc < 0.0) ityp = -ityp

ncount = 0

do while (jpufn /= 0)

  jpuf = jpufn

  ncount = ncount + 1
  idjpuf(ncount) = jpuf

!------ check type

  jtyp = puff(jpuf)%ityp
  if (puff(jpuf)%cc < 0.0) jtyp = -jtyp
  

  if ((ityp == jtyp) .and. (puff(jpuf)%idtl >= ilev)) then

!------ check scale

    s1 = puff(ipuf)%si
    s2 = puff(jpuf)%si
    if (s1 <= simrge*s2 .and. s2 <= simrge*s1 ) then

!------ check for location relative to inversion height

      s1 = puff(ipuf)%zbar - puff(ipuf)%zi
      s2 = puff(jpuf)%zbar - puff(jpuf)%zi

      l1 = puff(ipuf)%zc <= simrge*puff(jpuf)%zc
      l2 = puff(jpuf)%zc <= simrge*puff(ipuf)%zc

      if (s1*s2 >= 0.0 .and. l1 .and. l2) then

!------ check overlap distance

        betx = (puff(jpuf)%xbar - puff(ipuf)%xbar)/xmap
        bety = (puff(jpuf)%ybar - puff(ipuf)%ybar)/ymap
        betz =  puff(jpuf)%zbar - puff(ipuf)%zbar

        call get_overlp_arg(puff(ipuf),puff(jpuf),betx,bety,betz,arg)

        if (arg <= rrmrge) then

!------ passed all tests; merge and return

          call pmerge(puff(ipuf),puff(jpuf),xmap,ymap)

          call puff_grid(puff(jpuf),xn,yn,kn)
          call get_grid_cell(xn,yn,kn,igrd,ipn)

          if (ip /= ipn .OR. k /= kn) then

!------ remove jpuf from current cell list

            iprv = puff(jpuf)%iprv
            inxt = puff(jpuf)%inxt
            if (iprv < 0) then
              call set_ipgrd(-iprv/1000,2,k,inxt)
            else
              puff(iprv)%inxt = inxt
            end if
            if (inxt > 0) puff(inxt)%iprv = iprv

!------ add jpuf to new cell list

            call get_ipgrd(ipn,2,kn,jpufn)
            call set_ipgrd(ipn,2,kn,jpuf )

            puff(jpuf)%iprv = -(1000*ipn + kn)
            puff(jpuf)%inxt = jpufn

            if( jpufn > 0 )puff(jpufn)%iprv = jpuf

            puff(jpuf)%ipgd = igrd

          end if

          jpuf = 0
          return

        end if

      end if

    end if                                                                          ! differ

  end if                                                                            ! differ

!------ unable to merge with jpuf; find next guy and check again

  jpufn = puff(jpuf)%inxt

  if (ncount >= MAXCOUNT) then
    do icount = 1,ncount-1
      if (jpuf == idjpuf(icount)) then
        write(*,*)'Warning: search_and_merge found recursive pointer ',jpuf,' in linked list'
        return
      end if
    end do  
    write(*,*)'Warning: MAXCOUNT ',MAXCOUNT,' exceeded for search_and_merge'
    return
  end if
  
end do



!------ no mergable puffs found; return

return
end

subroutine get_overlp_arg(p1,p2,betx,bety,betz,arg)
!*******************************************************************************
!
! FUNCTION:  Call overlp_merge (needed for change of puff structure)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  overlp_merge
!
! REVISION HISTORY: 
!
! 29 NOV 2000 : Improved vertical overlap - DSH
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p1, p2    !Puff structures

real betx,bety,betz            !Distance between centroids
real arg                       !Argument to the exponent in the overlap integral
real, external :: overlp_merge              !Overlap function

arg = overlp_merge(p1,p2,betx,bety,betz)

return
end

logical function chkgrd(x,y,rxx,ryy)
!*******************************************************************************
!
! FUNCTION:  Check if puff is still within the domain
!            Puff sigma's are used to see if puff is really all the way out
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
! 06 AUG 2003: Change distance to be 3 sigmas and comment out exclusion of puff outside
!              the domain in chkgrd -BC
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
real x, y      !Puff x- and y-location
real rxx, ryy  !Ratio of puff x- and y-sigma's to the grid size

! --- PARAMETERS

real, parameter :: RFAC = 9.0

! --- LOCALS

real  xfac, yfac
logical chkx, chky

chkx   = (x >= 0.) .and. (x <= float(nx))
chky   = (y >= 0.) .and. (y <= float(ny))
chkgrd = chkx .and. chky

if (.not.chkgrd) then
  if (.not.chkx) then
    !if ( (x < -1.) .or. (x > float(nx+1)) ) return
    xfac = (min(abs(x-float(nx)),abs(x)))**2 / rxx
    chkx = xfac <= RFAC
  end if
  if (.not.chky) then
    !if ( (y < -1.) .or. (y > float(ny+1)) ) return
    yfac = (min(abs(y-float(ny)),abs(y)))**2 / ryy
    chky = yfac <= RFAC
  end if
  chkgrd = chkx .and. chky
end if

return
end

integer function grid(sxx,syy,xmap,ymap)
!*******************************************************************************
!
! FUNCTION:  Find the grid level that a puff is in
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_mxgrd
!
! REVISION HISTORY: 
!
!------ NOTE - Move FAC from DXP to XX,YY so that GRDMIN represents a
!                 minimum grid size instead of a minimum sigma - SFP 10/3/95

!------ NOTE - use 2*GRDMIN so that GRDMIN represents the smallest allowed
!                 grid.  If you use GRDMIN then grid is allowed to go one level
!                 beyond or GRDMIN/2.  This makes is harder for the user to adjust
!                 GRDMIN from the warning message - SFP 10/3/95

!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
real sxx, syy   !Puff sigma's (second moment)
real xmap, ymap !Map factors

! --- PARAMETERS

integer, parameter :: MAX_N = 100

real, parameter :: FAC = 4.0

! --- LOCALS

real    dxp, dxx, xx, yy
integer n, get_mxgrd

xx  = max(FAC*sqrt(sxx),2.*grdmin)
yy  = max(FAC*sqrt(syy),2.*grdmin)
dxp = max(xx*xmap/dxg,yy*ymap/dyg)

dxx = 1.0
n   = 0

do while (dxp <= dxx .and. n < MAX_N)
  n   = n + 1
  dxx = 0.5*dxx
end do

if( n >= MAX_N)then
  nError = IV_ERROR
  eMessage = 'Program Error'
  eInform  = 'Invalid puff size'
  eAction  = 'Please file a Bug report'
end if

grid = min0(n,get_mxgrd())

return
end

subroutine pmerge(p1,p2,xmap,ymap)
!*******************************************************************************
!
! FUNCTION:   Merge two puffs together
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            get_dynamics               get_topog          dense_rot_norm
!             pmerge_psum                  siginv            dense_effect
!
!
! REVISION HISTORY: 
! 18 OCT 2006: Bug fix in pmerge. Added check to prevent the merged puff from
!              going below the terrain height. -BC
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p1, p2   !Puff structures
real xmap, ymap            !Map factors

! --- LOCALS

type ( puff_dynamics ) pd1, pd2
real tot, r1, r2, xbar, ybar, zbar
real ddx1, ddx2, ddy1, ddy2, ddz1, ddz2

integer i, ipaux, naux, jpaux

logical ldense, dense_effect

real h0, h1, h2, hx0, hx1, hx2, hy0, hy1, hy2

tot = p1%c + p2%c
if(tot > 0.0)then
  r1 = p1%c/tot
  r2 = p2%c/tot
else
  r1 = 0.5
  r2 = 0.5
end if

xbar = r1*p1%xbar + r2*p2%xbar
ybar = r1*p1%ybar + r2*p2%ybar
zbar = r1*p1%zbar + r2*p2%zbar

if (lter) then

  call get_topog(xbar,ybar,h0,hx0,hy0)
  call get_topog(p1%xbar,p1%ybar,h1,hx1,hy1)
  call get_topog(p2%xbar,p2%ybar,h2,hx2,hy2)

  if (zbar < h0) then
    p1%zbar = p1%zbar + (h0-zbar)
    p2%zbar = p2%zbar + (h0-zbar)
    zbar = h0
  end if

  if (dense_gas) then
    call get_dynamics(p1,pd1)
    call get_dynamics(p2,pd2)
    ldense = dense_effect(p1%zbar-h1,p1%sv,pd1%wcp) &
             .or. dense_effect(p2%zbar-h2,p2%sv,pd2%wcp)
    if (ldense) then
      call dense_rot_norm(hx1,hy1,hx0,hy0,p1)
      call dense_rot_norm(hx2,hy2,hx0,hy0,p2)
    end if
  end if

end if

ddx1 = (p1%xbar - xbar)/xmap
ddx2 = (p2%xbar - xbar)/xmap
ddy1 = (p1%ybar - ybar)/ymap
ddy2 = (p2%ybar - ybar)/ymap
ddz1 =  p1%zbar - zbar
ddz2 =  p2%zbar - zbar

p2%sxx = r1*(p1%sxx + ddx1*ddx1) &
            + r2*(p2%sxx + ddx2*ddx2)
p2%syy = r1*(p1%syy + ddy1*ddy1) &
            + r2*(p2%syy + ddy2*ddy2)
p2%szz = r1*(p1%szz + ddz1*ddz1) &
            + r2*(p2%szz + ddz2*ddz2)
p2%sxy = r1*(p1%sxy + ddx1*ddy1) &
            + r2*(p2%sxy + ddx2*ddy2)
p2%syz = r1*(p1%syz + ddy1*ddz1) &
            + r2*(p2%syz + ddy2*ddz2)
p2%sxz = r1*(p1%sxz + ddx1*ddz1) &
            + r2*(p2%sxz + ddx2*ddz2)

call pmerge_psum(p1,p2,r1,r2)

p2%idtl = max0(p1%idtl,p2%idtl)

p2%xbar = xbar
p2%ybar = ybar
p2%zbar = zbar

call siginv( p2 )

if(p2%iaux > 0)then
  naux = typeID(p2%ityp)%npaux
  ipaux = p2%iaux - 1
  jpaux = p1%iaux - 1
  do i = 1,naux
    if(puff_aux(ipaux+i) == NOT_SET_R )then
      if(puff_aux(jpaux+i) == NOT_SET_R )then
        puff_aux(ipaux+i) = NOT_SET_R
      else
        puff_aux(ipaux+i) = puff_aux(jpaux+i)
      end if
    else
      if(puff_aux(jpaux+i) /= NOT_SET_R )then
        puff_aux(ipaux+i) = puff_aux(ipaux+i) + puff_aux(jpaux+i)
      end if
    end if
  end do
end if

return
end

subroutine pmerge_psum( p1, p2 ,r1, r2)
!*******************************************************************************
!
! FUNCTION:   Merge the integral terms in the puff structure
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
 
type ( puff_str ) p1, p2   !Puff structures

real    r1, r2             !Weighting factors for each puff

p2%c    = p1%c    + p2%c
p2%cc   = p1%cc   + p2%cc
p2%xuc  = p1%xuc  + p2%xuc
p2%xvc  = p1%xvc  + p2%xvc
p2%yvc  = p1%yvc  + p2%yvc
p2%yvsc = p1%yvsc + p2%yvsc
p2%yvbc = p1%yvbc + p2%yvbc
p2%zwc  = p1%zwc  + p2%zwc
p2%wc   = p1%wc   + p2%wc
p2%ccb  = p1%ccb  + p2%ccb

p2%si  = r1*p1%si  + r2*p2%si
p2%si2 = r1*p1%si2 + r2*p2%si2
p2%sv  = r1*p1%sv  + r2*p2%sv
p2%sr  = r1*p1%sr  + r2*p2%sr
p2%cfo = r1*p1%cfo + r2*p2%cfo
p2%zi  = r1*p1%zi  + r2*p2%zi
p2%zc  = r1*p1%zc  + r2*p2%zc
p2%uo  = r1*p1%uo  + r2*p2%uo
p2%vo  = r1*p1%vo  + r2*p2%vo
p2%wo  = r1*p1%wo  + r2*p2%wo

return
end

subroutine siginv( p )
!*******************************************************************************
!
! FUNCTION:  Set the puff inverse spread tensor (asig) and the determinant(det)
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
 
type ( puff_str ) p  !Puff structure

! --- LOCALS

real d11, d12, d13, d22, d23, d33, dalp, ralp

d11 = p%syy*p%szz - p%syz*p%syz
d12 = p%sxy*p%szz - p%sxz*p%syz
d13 = p%sxy*p%syz - p%sxz*p%syy
d22 = p%sxx*p%szz - p%sxz*p%sxz
d23 = p%sxx*p%syz - p%sxz*p%sxy
d33 = p%sxx*p%syy - p%sxy*p%sxy

dalp = p%sxx*d11 - p%sxy*d12 + p%sxz*d13
ralp = 0.5/dalp

p%axx =  d11*ralp
p%axy = -d12*ralp
p%axz =  d13*ralp
p%ayy =  d22*ralp
p%ayz = -d23*ralp
p%azz =  d33*ralp

p%det = dalp

return
end

real function overlp(asigin,bsigin,betxin,betyin,betzin)
!*******************************************************************************
!
! FUNCTION:   Calculate the argument to the exponent in the overlap integral 
!             of two Guassian puffs (to see if they do overlap significantly)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
! 22Mar06 : Change syntax and rename variables for removing possible conflicts.
!           Add better error messaging.-BC
!*******************************************************************************

! --- MODULES

use inter_inc
use files_inc
 
implicit none

! --- ARGUMENTS
 
real, dimension(7) :: asigin, bsigin  !Inverse spread tensors for the two puffs
real     :: betxin, betyin, betzin    !Separation distances of the two puffs

! --- LOCALS

integer          i
double precision detd, x1d, y1d, z1d, rdet
double precision d11, d12, d13, d22, d23, d33d
double precision a0, a1, a2, a3
double precision a11, a12, a13, a22, a23, a33

a11 = asigin(1) + bsigin(1)
a12 = asigin(2) + bsigin(2)
a13 = asigin(3) + bsigin(3)
a22 = asigin(4) + bsigin(4)
a23 = asigin(5) + bsigin(5)
a33 = asigin(6) + bsigin(6)

if (asigin(7) > bsigin(7)) then
  a1 = (asigin(1)*betxin + asigin(2)*betyin + asigin(3)*betzin)
  a2 = (asigin(2)*betxin + asigin(4)*betyin + asigin(5)*betzin)
  a3 = (asigin(3)*betxin + asigin(5)*betyin + asigin(6)*betzin)
  a0 = (a1*betxin + a2*betyin + a3*betzin)
  iovlp = 2
else
  a1 = -(bsigin(1)*betxin + bsigin(2)*betyin + bsigin(3)*betzin)
  a2 = -(bsigin(2)*betxin + bsigin(4)*betyin + bsigin(5)*betzin)
  a3 = -(bsigin(3)*betxin + bsigin(5)*betyin + bsigin(6)*betzin)
  a0 = -(a1*betxin + a2*betyin + a3*betzin)
  iovlp = 1
end if

d11  = a22*a33 - a23*a23
d12  = a12*a33 - a13*a23
d13  = a12*a23 - a13*a22
d22  = a11*a33 - a13*a13
d23  = a11*a23 - a13*a12
d33d = a11*a22 - a12*a12
detd = a11*d11 - a12*d12 + a13*d13

rdet = 1.0/detd

x1d = -(d11*a1 - d12*a2 + d13*a3)*rdet
y1d =  (d12*a1 - d22*a2 + d23*a3)*rdet
z1d = -(d13*a1 - d23*a2 + d33d*a3)*rdet

overlp = sngl(a0 - a11*x1d*x1d - 2.d0*a12*x1d*y1d - 2.d0*a13*x1d*z1d &
                  - a22*y1d*y1d - 2.d0*a23*y1d*z1d - a33*z1d*z1d)

det = sngl(detd)
d33 = sngl(d33d)
x1  = sngl(x1d)
y1  = sngl(y1d)
z1  = sngl(z1d)

if (overlp < -1.0e-6) then

  WRITE(lun_log,*)'overlp < -1.e-6 '
  WRITE(lun_log,*)(asigin(i),i=1,7)
  WRITE(lun_log,*)(bsigin(i),i=1,7)
  WRITE(lun_log,*) betxin,betyin,betzin
  WRITE(lun_log,*) a11,a12,a13,a22,a23,a33
  WRITE(lun_log,*) a1, a2, a3, a0, iovlp
  WRITE(lun_log,*) d11, d12, d13, d22, d23, d33d
  WRITE(lun_log,*) detd, rdet, x1d ,y1d ,z1d, overlp

end if

return
end

real function overlp_merge(p1,p2,betx,bety,betz)
!*******************************************************************************
!
! FUNCTION:   Calculate the weighted argument to the exponent in the overlap  
!             integral of two Guassian puffs for merging (accounting for
!             improved vertical overlap
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
! 29 NOV 2000 : Initial implementation - DSH
! 05 MAY 2003 : Include changes to make program confirm to standard fortran.
!               Change declarations of p1/p2 to puff_str from puff_str_xc. Remove
!               asig and bsig intermediate variables from function and its call. -BC
!
!*******************************************************************************

! --- MODULES

use common_puf
 
implicit none

! --- ARGUMENTS
 
type ( puff_str ) p1, p2  !The two puffs
real betx, bety, betz     !Separation distances of the two puffs

! --- LOCALS

double precision det, x1, y1, z1, rdet
double precision d11, d12, d13, d22, d23, d33
double precision a0, a1, a2, a3
double precision a11, a12, a13, a22, a23, a33, betzz

betzz = betz / ASPLT_ZFAC

a11 = p1%axx + p2%axx
a12 = p1%axy + p2%axy
a13 = p1%axz + p2%axz
a22 = p1%ayy + p2%ayy
a23 = p1%ayz + p2%ayz
a33 = p1%azz + p2%azz

if (p1%det > p2%det) then
  a1 = (p1%axx*betx + p1%axy*bety + p1%axz*betzz)
  a2 = (p1%axy*betx + p1%ayy*bety + p1%ayz*betzz)
  a3 = (p1%axz*betx + p1%ayz*bety + p1%azz*betzz)
  a0 = (a1*betx + a2*bety + a3*betzz)
else
  a1 = -(p2%axx*betx + p2%axy*bety + p2%axz*betzz)
  a2 = -(p2%axy*betx + p2%ayy*bety + p2%ayz*betzz)
  a3 = -(p2%axz*betx + p2%ayz*bety + p2%azz*betzz)
  a0 = -(a1*betx + a2*bety + a3*betzz)
end if

d11 = a22*a33 - a23*a23
d12 = a12*a33 - a13*a23
d13 = a12*a23 - a13*a22
d22 = a11*a33 - a13*a13
d23 = a11*a23 - a13*a12
d33 = a11*a22 - a12*a12
det = a11*d11 - a12*d12 + a13*d13

rdet = 1.0/det

x1 = -(d11*a1 - d12*a2 + d13*a3)*rdet
y1 =  (d12*a1 - d22*a2 + d23*a3)*rdet
z1 = -(d13*a1 - d23*a2 + d33*a3)*rdet

overlp_merge = sngl(a0 - a11*x1*x1 - 2.d0*a12*x1*y1 - 2.d0*a13*x1*z1 &
                        - a22*y1*y1 - 2.d0*a23*y1*z1 - a33*z1*z1)

return
end

subroutine puff_grid(p,xx,yy,k)
!*******************************************************************************
!
! FUNCTION:  Find vertical grid point
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
 
type ( puff_str ) p   !Puff structure

real xx, yy           !Number of grid points away from xmin and ymin

integer k             !Vertical grid number

integer n

xx = (p%xbar-xmin)/dxg
yy = (p%ybar-ymin)/dyg

if (p%zbar <= p%zi .and. lsplitz) then
  k = 1
else
  if (p%szz > dzg*dzg) then
    n = int(sqrt(p%szz)/dzg) + 1
    k = n*int(p%zbar/(dzg*float(n))) + n/2
  else
    k = int(p%zbar/dzg) + 1
  end if
  k = min0(k,nz)
end if

return
end
