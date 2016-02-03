!*******************************************************************************
!$RCSfile: split.f90,v $
!$Revision: 1.3 $
!$Date: 2010/08/27 15:51:53 $
!*******************************************************************************
subroutine split(ipuf,lsplit,cgrid)
!******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Split if puffs get too big in the vertical and/or horizontal
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!         check_splitpuff                   zsplit                  mapfac
!                  xsplit                   ysplit                  get_mc
!          chk_chem_split               chem_split              init_error
!                IsHazard                  get_amb
!
! REVISION HISTORY: 
!
!28 OCT 2002 : Call new routine check_splitpuff to allow space for c_release 
!              to create new puffs. - BC 
! Aug 2010: Include update for March 2005 for Oct. 2004 CMAQ release by PKK, AER -BC(SAGE-MGT)
!******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc
use files_inc
use interface_definitions, only: get_amb

implicit none

! --- ARGUMENTS
 
integer ipuf      !puff number
logical lsplit    !split flag
REAL :: CGRID( :,:,:,: )  !3D ambient concentration

! --- PARAMETERS

real, parameter :: HAZ_FAC = 4.

! --- LOCALS

type ( puff_mc ) pm

integer mchk, jpuf, nchk, j, j0, naux, ityp, ios

real conc_min, cmin2, xmap, ymap, chkx, chky, chkz, facs

real zi, hp, hx, hy

logical IsHazard, lspltch, lcont

!------ Skip small puffs

if(puff(ipuf)%c <= cmin)return

!------ vertical split

ityp = puff(ipuf)%ityp

naux = typeID(ityp)%npaux

conc_min = material(typeID(ityp)%imat)%prop(3)

cmin2   = 2.*conc_min*puff(ipuf)%c

if(IsHazard(typeID(ityp)%icls))then
  facs = HAZ_FAC
else
  facs = 1.0
end if

lcont = .true.
j0 = npuf - 1
nchk = 1
chkz = facs*delz2

if( puff(ipuf)%zbar < puff(ipuf)%zi )then
!----- improve vertical resolution in "shallow" bl
  if( lter )then
    call get_topog( puff(ipuf)%xbar,puff(ipuf)%ybar,hp,hx,hy)
    zi = puff(ipuf)%zi - hp
  else
    zi = puff(ipuf)%zi
  end if
  if( puff(ipuf)%sv < zi .and. dzg > 0.25*zi )then
    chkz = chkz * (0.25*zi/dzg)**2
  end if
end if

do while (lcont)
  lcont = .false.
  mchk = nchk
  do j = 1, nchk
    if (j == 1) then
      jpuf = ipuf
    else
      jpuf = j0 + j
    end if
    if (puff(jpuf)%szz > chkz &
                          .and. min(puff(jpuf)%cc,10.*puff(jpuf)%ccb) > cmin2) then
      if (.not.lsplitz .or. (lsplitz .and. &
               puff(jpuf)%zbar > puff(jpuf)%zi)) then
        call check_splitpuff(naux)
        if(nError /= NO_ERROR)go to 9999
        npuf = npuf + 1
        call zsplit(puff(jpuf),puff(npuf))
        nchk = nchk + 1
!               lcont = .true.
      end if
    end if
  end do
end do

!------ x-direction split

call mapfac(puff(ipuf)%xbar,puff(ipuf)%ybar,xmap,ymap)

chkx = facs*delx2/(xmap*xmap)
chky = facs*delx2/(ymap*ymap)

mchk = nchk
do j = 1,mchk
  if (j== 1) then
    jpuf = ipuf
  else
    jpuf = j0 + j
  end if
  cmin2   = 2.*conc_min*puff(jpuf)%c
  if (puff(jpuf)%sxx > chkx .and. min(puff(jpuf)%cc,10.*puff(jpuf)%ccb) > cmin2)then
    call check_splitpuff(naux)
    if(nError /= NO_ERROR)go to 9999
    npuf = npuf + 1
    call xsplit(puff(jpuf),puff(npuf))
    nchk = nchk + 1
  end if
end do

!------ y-direction split

mchk = nchk
do j = 1,mchk
  if (j== 1) then
    jpuf = ipuf
  else
    jpuf = j0 + j
  end if
  cmin2   = 2.*conc_min*puff(jpuf)%c
  if (puff(jpuf)%syy > chky .and. min(puff(jpuf)%cc,10.*puff(jpuf)%ccb) > cmin2)then
    call check_splitpuff(naux)
    if(nError /= NO_ERROR)go to 9999
    npuf = npuf + 1
    call ysplit(puff(jpuf),puff(npuf))
    nchk = nchk + 1
  end if
end do

!------ chemistry criteria split

if (multicomp) then
  if (lchem_split) then
    do j = 1,nchk
      if (j== 1) then
        jpuf = ipuf
      else
        jpuf = j0 + j
      end if
      cmin2   = 2.*conc_min*puff(jpuf)%c
      if (lamb3d) call get_amb(puff(jpuf)%xbar, puff(jpuf)%ybar, &
                               puff(jpuf)%zbar, t, cgrid)
      call get_mc(puff(jpuf),pm)
      call chk_chem_split(pm,lspltch)
      if(nError /= NO_ERROR)go to 9999
      if (lspltch .and. min(puff(jpuf)%cc,10.*puff(jpuf)%ccb) > cmin2) then
        call check_splitpuff(naux)
        if(nError /= NO_ERROR)go to 9999
        npuf = npuf + 1
        call chem_split(puff(jpuf),puff(npuf),lspltch)
        if (.not. lspltch) npuf = npuf - 1
      end if
    end do
  end if
end if

return

9999    continue

!-----  Stop further splitting for the rest of the time step

lsplit = .false.
if(lsplit_report)then
  write(lun_log,'(a)',iostat=ios)'********** WARNING **********'
  write(lun_log,*,iostat=ios)'Time = ',t,'( ',t/3600.,' )'
  write(lun_log,*,iostat=ios)'Splitting halted temporarily'
  write(lun_log,*,iostat=ios)'Too many puffs'
  write(lun_log,*,iostat=ios)'Resolution may be compromised'
  write(lun_log,'(a)',iostat=ios)'*****************************'
  lsplit_report = .false.
end if
call init_error

return

end

subroutine zsplit(p1,p2)
!*******************************************************************************
!
! FUNCTION:  Vertical splitting of puff (split p1 into p1 and p2)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  csplit
!
! REVISION HISTORY: 
!
! 29 NOV 2000 : Improved vertical overlap/splitting - DSH
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p1, p2  !puff structures for original and then 2 new puffs

! --- LOCALS

real sz, delx, dely, delz
real aspltz, aspltzc

aspltz  = ASPLT_ZFAC * asplt
aspltzc = 1.0 - aspltz*aspltz

sz   = sqrt(p1%szz)
delz = aspltz*sz
delx = aspltz*p1%sxz/sz
dely = aspltz*p1%syz/sz

p2%sxx  = p1%sxx - delx*delx
p2%syy  = p1%syy - dely*dely
p2%sxy  = p1%sxy - delx*dely

p2%szz  = p1%szz*aspltzc
p2%sxz  = p1%sxz*aspltzc
p2%syz  = p1%syz*aspltzc

call csplit(p1, p2, delx, dely, delz)

return
end

subroutine xsplit(p1,p2)
!*******************************************************************************
!
! FUNCTION: Split in the x-direction of puff (split p1 into p1 and p2)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  csplit
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p1, p2  !puff structures for original and then 2 new puffs

! --- LOCALS

real sx, delx, dely, delz

sx   = sqrt(p1%sxx)
delx = asplt*sx
dely = asplt*p1%sxy/sx
delz = asplt*p1%sxz/sx

p2%syy  = p1%syy - dely*dely
p2%szz  = p1%szz - delz*delz
p2%syz  = p1%syz - dely*delz

p2%sxx  = p1%sxx*aspltc
p2%sxy  = p1%sxy*aspltc
p2%sxz  = p1%sxz*aspltc

call csplit(p1, p2, delx, dely, delz)

return
end

subroutine ysplit(p1,p2)
!*******************************************************************************
!
! FUNCTION: Split in the y-direction of puff (split p1 into p1 and p2)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  csplit
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p1, p2  !puff structures for original and then 2 new puffs

! --- LOCALS

real sy, delx, dely, delz

sy   = sqrt(p1%syy)
dely = asplt*sy
delz = asplt*p1%syz/sy
delx = asplt*p1%sxy/sy

p2%szz  = p1%szz - delz*delz
p2%sxx  = p1%sxx - delx*delx
p2%sxz  = p1%sxz - delx*delz

p2%syy  = p1%syy*aspltc
p2%syz  = p1%syz*aspltc
p2%sxy  = p1%sxy*aspltc

call csplit(p1, p2, delx, dely, delz)

return
end

subroutine chem_split(p1,p2,lflag)
!*******************************************************************************
!
! FUNCTION:  Split to resolve ozone wings for chemistry run
!            Split perpendicular to wind direction
!
! PRECONDITIONS REQUIRED:  Must use staged chemistry
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  csplit
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p1, p2  !puff structures for original and then 2 new puffs
logical lflag             !whether or not the puff was split

! --- PARAMETERS
 
real, parameter :: FRAC = 0.7

! --- LOCALS

type ( puff_str ) p1r, p2r

real wspd, cs, sn, cs2, sn2
real sy, delx, dely, delz, delxr, delyr

wspd = sqrt(p1%uo*p1%uo + p1%vo*p1%vo)
if (wspd /= 0.) then
  cs = p1%uo/wspd
  sn = p1%vo/wspd
else
  cs = 1.
  sn = 0.
end if

cs2 = cs*cs
sn2 = sn*sn

!------rotate p1 & check if ok to split
p1r%syy =  sn2*p1%sxx + cs2*p1%syy - 2.*cs*sn*p1%sxy
sy      = sqrt(p1r%syy)
if (sy < FRAC*p1%si) then
  lflag = .false.
  go to 9999
else
  lflag = .true.
end if

p1r%sxx =  cs2*p1%sxx + sn2*p1%syy + 2.*sn*cs*p1%sxy
p1r%sxy =  sn*cs*(p1%syy-p1%sxx) + (cs2-sn2)*p1%sxy
p1r%sxz =  cs*p1%sxz + sn*p1%syz
p1r%syz = -sn*p1%sxz + cs*p1%syz
p1r%szz =  p1%szz

delyr = asplt*sy
delz = asplt*p1r%syz/sy
delxr = asplt*p1r%sxy/sy

! -----find rotated p2
p2r%szz  = p1r%szz - delz*delz
p2r%sxx  = p1r%sxx - delxr*delxr
p2r%sxz  = p1r%sxz - delxr*delz

p2r%syy  = p1r%syy*aspltc
p2r%syz  = p1r%syz*aspltc
p2r%sxy  = p1r%sxy*aspltc

!------rotate p2 back
sn = -sn
p2%syy =  sn2*p2r%sxx + cs2*p2r%syy - 2.*cs*sn*p2r%sxy
p2%sxx =  cs2*p2r%sxx + sn2*p2r%syy + 2.*sn*cs*p2r%sxy
p2%sxy =  sn*cs*(p2r%syy-p2r%sxx) + (cs2-sn2)*p2r%sxy
p2%sxz =  cs*p2r%sxz + sn*p2r%syz
p2%syz = -sn*p2r%sxz + cs*p2r%syz
p2%szz =  p2r%szz

!------rotate delx and dely back
dely = cs*delyr - sn*delxr
delx = cs*delxr + sn*delyr

call csplit(p1, p2, delx, dely, delz)

9999  return
end

subroutine csplit(p1, p2, delx, dely, delz)
!*******************************************************************************
!
! FUNCTION:  Performs the puff splitting
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!             init_csplit                  mapfac               get_topog
!            get_dynamics           split_ter_chk               split_chk
!           split_cap_chk            dense_effect               BL_capped
!
!
! REVISION HISTORY: 
!
! 03NOV2000 :  Modified ground reflection. - RIS
! 26OCT2001 :  Changed  zrefl = p1%zi from p1%zc if BL_capped
!              based on bug report filed by PK. - BC
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p1, p2    !puff structures for original and then 2 new puffs
real delx, dely, delz       !distance between new centroid locations (+ and -)

! --- LOCALS

type ( puff_dynamics ) pd

real h0, hx0, hy0, xmap, ymap
real x0, y0, z0, dxx, dyy
real h, hx, hy
real zrefl

integer i,ipaux,naux,jpaux

logical lcap, lzinv, lrefl, ldense, dense_effect
logical BL_capped

call init_csplit(p1,p2)

p2%ityp = p1%ityp
p2%inxt = 0
p2%iprv = 0
p2%idtl = p1%idtl
p2%idtn = 0
p2%ipgd = p1%ipgd

call mapfac( p1%xbar , p1%ybar , xmap , ymap)

delx = delx*xmap
dely = dely*ymap

x0 = p1%xbar
y0 = p1%ybar
z0 = p1%zbar

if (lter) then
  call get_topog(p1%xbar,p1%ybar,h0,hx0,hy0)
  if (dense_gas) then
    call get_dynamics(p1,pd)
    ldense = ((pd%dudx+pd%dvdy)*p1%c*(p1%sv**2) > p1%zwc) &
                .and. dense_effect(p1%zbar-h0,p1%sv,pd%wcp)
  else
    ldense = .false.
  end if
else
  h0 = 0.
end if

lzinv = p1%zbar <= p1%zi
lcap  = p1%zbar <= p1%zc .and. p1%zc > 0.
if (lzinv) then
  if (BL_capped(p1%xbar,p1%ybar)) then
    lrefl = .true.
    zrefl = p1%zi
  else
    lrefl = lcap
    zrefl = p1%zc
  end if
else
  lrefl = lcap
  zrefl = p1%zc
end if

p2%xbar = p1%xbar + delx
p1%xbar = p1%xbar - delx
p2%ybar = p1%ybar + dely
p1%ybar = p1%ybar - dely
p2%zbar = p1%zbar + delz
p1%zbar = p1%zbar - delz

!------ check for splitting below the surface

if (lter) then
  call split_ter_chk(x0,y0,z0,h0,hx0,hy0,p1,ldense)
  call split_ter_chk(x0,y0,z0,h0,hx0,hy0,p2,ldense)
else ! --- Modify reflection to be along current puff direction
  dxx = xmap*delz*p1%sxz/p1%sxx
  dyy = ymap*delz*p1%syz/p1%syy
  call split_chk( dxx, dyy,delz,p1 )
  call split_chk(-dxx,-dyy,delz,p2 )
end if

!------ check for splitting across the cap

if (lrefl) then
  delx = p1%xbar - x0
  dely = p1%ybar - y0
  delz = p1%zbar - z0
  call split_cap_chk(p1,zrefl, delx, dely,delz)
  delx = p2%xbar - x0
  dely = p2%ybar - y0
  delz = p2%zbar - z0
!         call split_cap_chk(p2,zrefl,-delx,-dely,delz)
  call split_cap_chk(p2,zrefl,delx,dely,delz)
end if

!------ reset cap for terrain changes

if (lter .and. lcap) then
  call get_topog(p1%xbar,p1%ybar,h,hx,hy)
  p1%zc = max(p1%zc,h+sqrt(p1%szz))
  call get_topog(p2%xbar,p2%ybar,h,hx,hy)
  p2%zc = max(p2%zc,h+sqrt(p2%szz))
end if

!------ Auxiliary arrays

if(p1%iaux > 0)then
  p2%iaux = npaux
  naux = typeID(p1%ityp)%npaux
  jpaux = p1%iaux - 1
  ipaux = npaux - 1
  do i = 1,naux
    if (puff_aux(jpaux+i) /= NOT_SET_R) then
      puff_aux(ipaux+i) = 0.5*puff_aux(jpaux+i)
      puff_aux(jpaux+i) = puff_aux(ipaux+i)
    else
      puff_aux(ipaux+i) = NOT_SET_R
    end if
  end do
  npaux = npaux + naux
else
  p2%iaux = 0
end if

return
end

subroutine init_csplit(p1,p2)
!*******************************************************************************
!
! FUNCTION:  Initialize csplit
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use struct_inc

implicit none

! --- ARGUMENTS

type ( puff_str) p1, p2  !puff structures for original and then 2 new puffs
 
p1%sxx = p2%sxx
p1%sxy = p2%sxy
p1%sxz = p2%sxz
p1%syy = p2%syy
p1%syz = p2%syz
p1%szz = p2%szz

p2%c     = 0.5*p1%c
p1%c     = p2%c
p2%cc    = 0.5*p1%cc
p1%cc    = p2%cc 
p2%xuc   = 0.5*p1%xuc  
p1%xuc   = p2%xuc
p2%xvc   = 0.5*p1%xvc  
p1%xvc   = p2%xvc
p2%yvc   = 0.5*p1%yvc  
p1%yvc   = p2%yvc
p2%yvsc  = 0.5*p1%yvsc 
p1%yvsc  = p2%yvsc
p2%yvbc  = 0.5*p1%yvbc 
p1%yvbc  = p2%yvbc
p2%zwc   = 0.5*p1%zwc  
p1%zwc   = p2%zwc
p2%wc    = 0.5*p1%wc   
p1%wc    = p2%wc
p2%ccb   = 0.5*p1%ccb
p1%ccb   = p2%ccb

p2%si  = p1%si
p2%si2 = p1%si2
p2%sv  = p1%sv
p2%sr  = p1%sr
p2%cfo = p1%cfo
p2%zi  = p1%zi
p2%zc  = p1%zc
p2%uo  = p1%uo
p2%vo  = p1%vo
p2%wo  = p1%wo

return
end

subroutine split_ter_chk(x0,y0,z0,h0,hx0,hy0,p,ldense)
!*******************************************************************************
!
! FUNCTION: Check if new puffs need to be reflected off the terrain
!           (if they went below the ground)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_topog          dense_rot_norm          split_ter_refl
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p  !puff structure with new centroids

real x0, y0, z0      !original puff centroid
real h0, hx0, hy0    !terrain height and gradients
logical ldense       !treat as dense gas or not

! --- LOCALS

real hp, hx, hy

call get_topog(p%xbar,p%ybar,hp,hx,hy)

if (ldense) then
  call dense_rot_norm(hx0,hy0,hx,hy,p)
  p%zbar = hp + z0 - h0
else
  if (p%zbar < hp) then
    call split_ter_refl(x0,y0,z0,h0,hx0,hy0,hp,hx,hy,p)
  end if
end if

return
end

subroutine split_ter_refl(x0,y0,z0,h0,hx0,hy0,hp,hx,hy,p)
!*******************************************************************************
!
! FUNCTION:  Reflect split puffs off the terrain in case it goes below
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_topog
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p  !puff structure (new puff)

real x0, y0, z0      !original puff centroid
real h0, hx0, hy0    !original terrain height and gradients
real hp, hx, hy      !split puff terrain height and gradients

! --- LOCALS

real del0, del1, rat, rm1

del0  = z0 - h0
del1  = hp - p%zbar
rat   = (del0-del1) / (del0+del1)
rm1   = 1.0 - rat

p%xbar = rm1*x0 + rat*p%xbar
p%ybar = rm1*y0 + rat*p%ybar
p%zbar = rm1*z0 + rat*p%zbar

call get_topog(p%xbar,p%ybar,hp,hx,hy)

if (p%zbar < hp) then
  p%xbar = x0
  p%ybar = y0
  p%zbar = z0
  hp     = h0
  hx     = hx0
  hy     = hy0
else if (p%zc > 0. .and. p%zbar > p%zc) then
  p%zc = p%zc + p%zbar - z0
end if

return
end

subroutine split_chk(delx,dely,delz,p)
!*******************************************************************************
!
! FUNCTION:  Check if puff goes below the ground (more simple flat-terrain case)
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
 
type ( puff_str ) p     !puff structure (new puff)

real delx, dely, delz   !distance the centroids were moved

! --- LOCALS

real fac

if (p%zbar < 0.0) then
  fac = 2.*abs(p%zbar/delz)
  p%zbar = -p%zbar
  p%xbar =  p%xbar + fac*delx
  p%ybar =  p%ybar + fac*dely
end if

return
end

subroutine split_cap_chk(p,zrefl,delx,dely,delz)
!*******************************************************************************
!
! FUNCTION:   Check if a capped puff tries to split above the inversion
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_topog
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p    !puff structure

real zrefl             !inversion or capping height to reflect from
real delx, dely, delz  !distance the centroids were moved from original puff

! --- LOCALS

real fac, h, hx, hy

if (p%zbar > zrefl ) then
  fac = 2.*abs((p%zbar-zrefl)/delz)
  p%zbar = 2.*zrefl - p%zbar
  p%xbar = p%xbar + fac*delx
  p%ybar = p%ybar + fac*dely
  if (lter) then
    call get_topog(p%xbar,p%ybar,h,hx,hy)
  else
    h = 0.
  end if
  p%zbar = max(p%zbar,h+sqrt(p%szz))
end if

return

end
