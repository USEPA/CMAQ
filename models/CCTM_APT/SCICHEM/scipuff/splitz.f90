!*******************************************************************************
!$RCSfile: splitz.f90,v $
!$Revision: 1.3 $
!$Date: 2007/02/06 23:33:01 $
!*******************************************************************************
subroutine splitz(p,lev1,lev2,nchg)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Split puffs in the vertical
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               splitz_wm              splitz_nwm               copy_puff
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p   !Puff structure

integer lev1, lev2    !Time levels that are currently being stepped 
                      !(from lev1 to lev2)  
integer nchg          !Number of timestep changes

! --- LOCALS

integer iprv, inxt, idtn, iaux, il

!------ save variables to move into original puff

iprv = p%iprv
inxt = p%inxt
idtn = p%idtn
iaux = p%iaux
il   = p%idtl

!------ vertical split depends on whether puff is well-mixed

if (sz >= WMX*(p%zc-hp) .and. p%zc > 0.0) then
  call splitz_wm(p,lev1,lev2,nchg)
  if (nError /= NO_ERROR) go to 9999
else
  call splitz_nwm(p,lev1,lev2,nchg)
  if (nError /= NO_ERROR) go to 9999
end if

!------ move last split puff into original puff location

call copy_puff(puff(npuf),p,iaux)
p%iprv = iprv
p%inxt = inxt
p%idtn = idtn
npuf   = npuf - 1
if(p%iaux > 0)then
  npaux = npaux - typeID(p%ityp)%npaux
end if

if (il /= p%idtl) then
  nchg = nchg + 1
end if

9999    continue

return

end


subroutine splitz_nwm(p,lev1,lev2,nchg)
!*******************************************************************************
!
! FUNCTION: Split part of puff above zi if it previously was below zi and zi has
!           decreased - assumes puff is not well-mixed
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!           check_newpuff               copy_puff               chop_puff
!                set_puff              scale_psum
!
! REVISION HISTORY: 
!
! 24-JUL-2001 : Fixed bug decrementing npaux for frac = 0. (BC)
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p   !Puff structure

integer lev1, lev2    !Time levels that are currently being stepped 
                      !(from lev1 to lev2)  
integer nchg          !Number of timestep changes

! --- PARAMETERS
 
real ZFAC, R2, EPS
parameter (ZFAC = 3.0000000)    ! set to match value in step_p
parameter (R2   = 0.7071068)    ! square root of 2
parameter (EPS  = 1.e-2)        ! set to match value in gauss_int

! --- LOCALS

integer npufo, jpuf, i, nzp, naux, ipaux

real szz, dzz, tfac, zbot, zrfl
real zlow, ctot, frac, dzp, delz
real erfc

logical lrfl_top, lrfl_bot

npufo = npuf

!------ determine vertical range for newly created puffs

zbar = p%zbar
dzz  = ZFAC*sz
tfac = R2/sz
zrfl = p%zc

!------ top-most extent

if (zrfl >= zbar + dzz .or. zrfl == 0.0) then
  ztop = zbar + dzz
  lrfl_top = .false.
else
  ztop = zrfl
  lrfl_top = .true.
end if

!------ bottom-most extent

zbot = zbar-dzz
zlow = max(p%zi,zbot)

lrfl_bot = (zbar - hp) < dzz

!------ check if splitting will create puffs of significant (relative) mass

ctot = 0.5*(erfc((zlow-zbar)*tfac))

if (ctot < EPS) then

  naux = typeID(p%ityp)%npaux
  call check_newpuff(naux)
  if(nError /= NO_ERROR) go to 9999

  npuf = npuf + 1

  call copy_puff(p,puff(npuf),npaux)

  go to 1000

end if

!------ create puffs

if (lsplitz) then
  nzp  = max(int((ztop-zlow)/dzg+0.5),1)
else
  nzp = 1
end if
dzp  = (ztop-zlow)/float(nzp)

ctot = 0.

naux = typeID(p%ityp)%npaux
do i = 1,nzp

  ztop = zlow + dzp

  call check_newpuff(naux)
  if(nError /= NO_ERROR) go to 9999
  npuf = npuf + 1

  call chop_puff(p,sz,tfac,zlow,ztop,zrfl,hp,lrfl_top,lrfl_bot, &
                    frac,delz,szz)

  if (frac == 0.) then
    npuf = npuf - 1
  else
    call set_puff(p,szz,frac,delz,xmap,ymap,lev1,lev2,nchg)
  end if

  ctot = ctot + frac
  zlow = ztop

end do

!------ put remainder below current zi if necessary

if (zbot < p%zi) then

  call check_newpuff(naux)
  if(nError /= NO_ERROR) go to 9999
  npuf = npuf + 1

  zlow = hp
  ztop = p%zi

  call chop_puff(p,sz,tfac,zlow,ztop,zrfl,hp,lrfl_top,lrfl_bot, &
                    frac,delz,szz)

  if (frac == 0.) then
    npuf = npuf - 1
  else
    call set_puff(p,szz,frac,delz,xmap,ymap,lev1,lev2,nchg)
    szz  = 0.65*(ztop-zlow)**2          ! well-mixed
    if (puff(npuf)%szz > 0.8*szz) then
       puff(npuf)%szz  = szz
       puff(npuf)%idtl = p%idtl
    end if
    puff(npuf)%zc = zlim
  end if

  ctot = ctot + frac

end if

!------ check mass

if (ctot > 0.5) then
  frac = 1.0/ctot
  do jpuf = npufo+1,npuf
    call scale_psum(puff(jpuf),frac)
    if(naux > 0)then
      ipaux = puff(jpuf)%iaux-1
      do i = 1,naux
        if(puff_aux(ipaux+i) /= NOT_SET_R)then
          puff_aux(ipaux+i) = frac*puff_aux(ipaux+i)
        else
          puff_aux(ipaux+i) = NOT_SET_R
        end if
      end do
    end if
  end do
else
  nError   = UK_ERROR
  eMessage = 'Error partitioning puff mass about inversion'
  eInform  = 'Try reducing vertical resolution'
  go to 9999
end if

1000    return

9999    eRoutine = 'splitz_nwm'
return

end


subroutine splitz_wm(p,lev1,lev2,nchg)
!*******************************************************************************
!
! FUNCTION: Split part of puff above zi if it previously was below zi and zi has
!           decreased - assumes puff IS well-mixed
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!           check_newpuff             chop_puffwm              scale_psum
!
!
! REVISION HISTORY: 
!
! 22 JAN 2001 :  Fix bug in mass conservation check - DSH
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p   !Puff structure

integer lev1, lev2    !Time levels that are currently being stepped 
                      !(from lev1 to lev2)  
integer nchg          !Number of timestep changes

! --- LOCALS

integer jpuf, npufo, i, nzp, naux, ipaux

real dzz, zbot, zlow, dzp, ctot, frac

npufo = npuf

!------ determine vertical range for newly created puffs

ztop = p%zc

zbot = p%zi

dzz  = ztop - hp

!------ create puffs

if (lsplitz) then
  nzp  = max(int((ztop-zbot)/dzg+0.5),1)
else
  nzp = 1
end if
dzp  = (ztop-zbot)/float(nzp)
zlow = zbot

ctot = 0.

naux = typeID(p%ityp)%npaux
do i = 1,nzp

  ztop = zlow + dzp

  call check_newpuff(naux)
  if(nError /= NO_ERROR) go to 9999
  npuf = npuf + 1

  call chop_puffwm(p,dzz,zlow,ztop,xmap,ymap,lev1,lev2,nchg,frac)

  if (frac == 0.) then
    npuf = npuf - 1
    npaux = npaux - naux
  end if

  ctot = ctot + frac
  zlow = ztop

end do

!------ put remainder below current zi

call check_newpuff(naux)
if(nError /= NO_ERROR) go to 9999
npuf = npuf + 1

zlow = hp
ztop = p%zi

call chop_puffwm(p,dzz,zlow,ztop,xmap,ymap,lev1,lev2,nchg,frac)

if (frac == 0.) then
  npuf = npuf - 1
  npaux = npaux - naux
else

  ctot = ctot + frac

  puff(npuf)%szz  = 0.65*(ztop-zlow)**2         ! set well-mixed
  puff(npuf)%idtl = p%idtl
  puff(npuf)%zc   = zlim

end if

!------ check mass

if (ctot > 0.5) then
  frac = 1.0/ctot
  do jpuf = npufo+1,npuf
    call scale_psum(puff(jpuf),frac)
    if(naux > 0)then
      ipaux = puff(jpuf)%iaux-1
      do i = 1,naux
        if(puff_aux(ipaux+i) /= NOT_SET_R)then
          puff_aux(ipaux+i) = frac*puff_aux(ipaux+i)
        else
          puff_aux(ipaux+i) = NOT_SET_R
        end if
      end do
    end if
  end do
else
  nError   = UK_ERROR
  eMessage = 'Error partitioning puff mass about inversion'
  eInform  = 'Try reducing vertical resolution'
  go to 9999
end if

return

9999    continue
eRoutine = 'splitz_wm'
return

end


subroutine chop_puff(p,sz,tfac,zlow,ztop,zrfl,hp, &
                        lrfl_top,lrfl_bot,frac,delz,szz)
!*******************************************************************************
!
! FUNCTION:  Chop a puff
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               gauss_int                refl_int
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p    !Puff structure

real sz                !Puff sigma-z
real tfac              !Factor used in exp (1/sqrt(2*sigma-z))
real zlow              !Bottom-most extent of the puff
real ztop              !Top-most extent of the puff
real zrfl              !Capping height
real hp                !Terrain height
real frac              !Fraction of mass to go into second puff
real delz              !Vertical separation distance between new puff and orig
real szz               !Puff sigma-z squared

logical lrfl_top, lrfl_bot !Flags for reflection off cap and surface

! --- LOCALS

real zbar_top, zbar_bot, zbar

!------ compute section mass fraction and vertical moments

call gauss_int(p%zbar,zlow,ztop,sz,p%szz,tfac,frac,delz,szz)

if (frac == 0.) return

!------ reflections if necessary

if (lrfl_top) then

  zbar_top = 2.*zrfl - p%zbar
  call refl_int(zbar_top,p%zbar,zlow,ztop,sz,szz,delz,frac,tfac)

end if

if (lrfl_bot) then

  zbar_bot = 2.*hp - p%zbar
  call refl_int(zbar_bot,p%zbar,zlow,ztop,sz,szz,delz,frac,tfac)

  if (lrfl_top) then

    zbar = 2.*zrfl - zbar_bot
    call refl_int(zbar,p%zbar,zlow,ztop,sz,szz,delz,frac,tfac)

    zbar = 2.*hp - zbar_top
    call refl_int(zbar,p%zbar,zlow,ztop,sz,szz,delz,frac,tfac)

  end if

end if

return

end


subroutine chop_puffwm(p,dzz,zlow,ztop,xmap,ymap,lev1,lev2,nchg,frac)
!*******************************************************************************
!
! FUNCTION:  Chop a puff that is well-mixed
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                set_puff
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p   !Puff structure

integer lev1, lev2    !Time levels that are currently being stepped 
                      !(from lev1 to lev2)  
integer nchg          !Number of timestep changes

real zlow, ztop       !Bottom and top-most extent of puff
real dzz              !ztop - hp
real xmap, ymap       !Map factors
real frac             !Fraction of mass to go into second puff
 
! --- PARAMETERS
 
real SZFAC
parameter (SZFAC = 0.16)

! --- LOCALS

real dzzo, szz, delz

dzzo = ztop - zlow
frac = dzzo/dzz
szz  = SZFAC*dzzo*dzzo
delz = zlow + 0.5*dzzo - p%zbar

call set_puff(p,szz,frac,delz,xmap,ymap,lev1,lev2,nchg)

return

end


subroutine gauss_int(zbar,zlow,ztop,sz,szz,tfac,q0,delz,szzo)
!*******************************************************************************
!
! FUNCTION:   Compute section mass fraction and vertical moments
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
 
real zbar        !Puff vertical coordinate
real zlow, ztop  !Bottom and top-most extent of puff
real sz, szz     !Sigma-z and sigma-z squared
real tfac        !Factor used in exp (1/sqrt(2*sigma-z))
real q0          !Fraction of mass to go into second puff
real delz        !Vertical distance between new puff and old
real szzo        !Sigma-z squared for new puff

! --- PARAMETERS
 
real PIS, PI2S, EPS
parameter (PIS  = 1.7724539)
parameter (PI2S = 0.3989423)
parameter (EPS  = 1.e-2)

! --- LOCALS

real t1, t2, e1, e2, q1, q2
real erfc

t1   = (zlow-zbar)*tfac
t2   = (ztop-zbar)*tfac
e1   = exp(-t1**2)
e2   = exp(-t2**2)
q0   = 0.5*(erfc(t1) - erfc(t2))

if (q0 < EPS) then
  q0   = 0.
  delz = 0.
  szzo = 1.e+10
  go to 9999
end if

q1   = PI2S*(e1 - e2)
q2   = (t1*e1 - t2*e2)/PIS

delz = sz*q1/q0
delz = max(min(delz,ztop-zbar),zlow-zbar)
szzo = szz*(1.+q2/q0-(q1/q0)**2)
if (szzo <= 0.) then
  szzo = 0.0833333*(ztop-zlow)**2
end if

9999    continue

return

end


subroutine set_puff(p,szz,frac,delz,xmap,ymap,lev1,lev2,nchg)
!*******************************************************************************
!
! FUNCTION: Set new puff
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!             set_puff_xc
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p  !Puff structure
integer lev1, lev2   !Time levels being stepped
integer nchg         !Number of time level changes
real szz             !sigma-z squared (second moment)
real frac            !Weighting factor for puffs
real delz            !Vertical separatin distance
real xmap, ymap      !Map factors

! --- LOCALS

real delx, dely, temz, sxz, syz, dtk, del

integer il, naux

puff(npuf)%sxx = p%sxx
puff(npuf)%sxy = p%sxy
puff(npuf)%syy = p%syy

if (p%iaux > 0) then
  puff(npuf)%iaux = npaux
  naux = typeID(p%ityp)%npaux
  npaux = npaux + naux
else
  puff(npuf)%iaux = 0
end if
call set_puff_xc(puff(npuf), p, frac)

delx = p%sxz/p%szz * delz
dely = p%syz/p%szz * delz

puff(npuf)%xbar = p%xbar + delx*xmap
puff(npuf)%ybar = p%ybar + dely*ymap
puff(npuf)%zbar = p%zbar + delz

temz = sqrt(szz/p%szz)
sxz  = p%sxz*temz
syz  = p%syz*temz

puff(npuf)%sxz  = sxz
puff(npuf)%syz  = syz
puff(npuf)%szz  = szz

!------ check time level

dtk = 0.5*delz2/max(p%zwc/p%c,1.e-6)

del = delt
il  = 0

do while (dtk < del)
  il  = il + 1
  del = 0.5*del
end do

il = max(il,lev1)
il = max(il,p%idtl)

!------ reduce diffusivity if time step is too big

if (il > lev2) then
  puff(npuf)%zwc = puff(npuf)%zwc*2.**(lev2-il)
  il = lev2
end if

if (il /= p%idtl) then
  nchg = nchg + 1
end if

puff(npuf)%ityp = p%ityp
puff(npuf)%inxt = 0
puff(npuf)%iprv = 0
puff(npuf)%idtl = il
puff(npuf)%idtn = 0

return

end


subroutine set_puff_xc(p1, p2, frac)
!*******************************************************************************
!
! FUNCTION:  Scale puff
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            set_puff_aux
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use struct_inc

implicit none

! --- ARGUMENTS
 
type (puff_str) p1, p2  !Puff structures

real frac               !Scaling factor

! --- LOCALS

p1%c     = frac*p2%c
p1%cc    = frac*p2%cc 
p1%xuc   = frac*p2%xuc
p1%xvc   = frac*p2%xvc
p1%yvc   = frac*p2%yvc
p1%yvsc  = frac*p2%yvsc
p1%yvbc  = frac*p2%yvbc
p1%zwc   = frac*p2%zwc
p1%wc    = frac*p2%wc
p1%ccb   = frac*p2%ccb

p1%si  = p2%si
p1%si2 = p2%si2
p1%sv  = p2%sv
p1%sr  = p2%sr
p1%cfo = p2%cfo
p1%zi  = p2%zi
p1%zc  = p2%zc
p1%uo  = p2%uo
p1%vo  = p2%vo
p1%wo  = p2%wo

call set_puff_aux(p1, p2, frac)

return
end


subroutine set_puff_aux(p1, p2, frac)
!*******************************************************************************
!
! FUNCTION:  Scale puff auxilliaries
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
 
type (puff_str) p1, p2   !Puff structures
real frac                !Scaling factor

! --- LOCALS

integer i, i1aux, i2aux, naux

if (p2%iaux > 0) then
  naux = typeID(p2%ityp)%npaux
  i1aux = p1%iaux - 1
  i2aux = p2%iaux - 1
  do i = 1,naux
    if(puff_aux(i2aux+i) /= NOT_SET_R)then
      puff_aux(i1aux+i) = frac*puff_aux(i2aux+i)
    else
      puff_aux(i1aux+i) = NOT_SET_R
    end if
  end do
end if

return
end


subroutine refl_int(zbar,zbari,zlow,ztop,sz,szz,delz,frac,tfac)
!*******************************************************************************
!
! FUNCTION: Integrate reflected puff at z=zbar and combine with existing puff
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               gauss_int
!
! REVISION HISTORY: 
!
!*******************************************************************************

implicit none

! --- ARGUMENTS
 
real zbar       !New puff vertical coordinate
real zbari      !Original puff vertical coordinate
real zlow, ztop !Bottom and top-most extent of the puff
real sz, szz    !sigma-z and sigma-z squared
real delz       !Vertical distance between new puff and orig
real frac       !Fraction of mass to go into second puff
real tfac       !Factor used in exp (1/sqrt(2*sigma-z))

! --- LOCALS

real szzi, fracr, delr, szzr, ftot, temz

szzi = sz*sz
call gauss_int(zbar,zlow,ztop,sz,szzi,tfac,fracr,delr,szzr)

if (fracr > 0.) then

  ftot = frac + fracr
  delr = delr + zbar - zbari
  temz = (frac*delz + fracr*delr)/ftot
  szz  = (frac*(delz*delz+szz) + fracr*(delr*delr+szzr))/ftot &
                                                      - temz**2
  delz = temz

  frac = ftot

end if

return

end


subroutine split_zi(p,zinv)
!*******************************************************************************
!
! FUNCTION:  Split a puff above and below the inversion height
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!           check_newpuff            chop_puff_zi
!
! REVISION HISTORY: 
!
! 21-JAN-2007: Move increment of puff number to chop_puff_zi -BC
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use step_p_inc

implicit none

! --- ARGUMENTS
                               
real zinv                      !Inversion height
type ( puff_str ) p            !Puff structure

! --- PARAMETERS

real ZFAC, R2
parameter (ZFAC = 3.0000000)    ! set to match value in step_p
parameter (R2   = 0.7071068)

! --- LOCALS

integer naux

real dzz, tfac, zbot, zrfl
real temz, szz

logical lrfl_top, lrfl_bot

!------ check space for new puff

naux = typeID(p%ityp)%npaux
call check_newpuff(naux)
if(nError /= NO_ERROR) go to 9999

!------ determine vertical range for newly created puffs

zbar = p%zbar
dzz  = ZFAC*sz
tfac = R2/sz
zrfl = p%zc

!------ top-most extent

if (zrfl >= zbar + dzz .or. zrfl == 0.) then
  ztop = zbar + dzz
  lrfl_top = .false.
else
  ztop = zrfl
  lrfl_top = .true.
end if

!------ bottom-most extent

zbot = zbar - dzz
lrfl_bot = (zbar - hp) < dzz

!------ create new puff above zi with remainder below (in original puff)

call chop_puff_zi(p,sz,tfac,zinv,ztop,zbot,zrfl,hp,xmap,ymap, &
                               lrfl_top,lrfl_bot,fsplit,nsplit)

!----- limit capped puff to well-mixed value

temz = WMX*(zinv-hp)
szz  = min(p%szz,temz*temz)

temz  = sqrt(szz/p%szz)
p%sxz = p%sxz*temz
p%syz = p%syz*temz
p%szz = szz
sz    = sz*temz

9999    return

end


subroutine chop_puff_zi(p,sz,tfac,zinv,ztop,zbot,zrfl,hp,xmap,ymap, &
                                     lrfl_top,lrfl_bot,fsplit,nsplit)
!*******************************************************************************
!
! FUNCTION:  Chop a puff above and below the inversion
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               chop_puff             set_puff_xc
!
! REVISION HISTORY: 
!
! 21-JAN-2007: Increment puff number only if frac > 0.05 -BC
! 25-FEB-2015: Skip puffs that are much deeper than BL - PKK, ENVIRON
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p     !Puff structure

integer nsplit
real    fsplit
real    sz                 !Puff sigma-z
real    tfac               !Factor used in exp (1/sqrt(2*sigma-z))
real    zinv               !Inversion height
real    ztop, zbot         !Top and bottom extent of puff
real    zrfl               !Capping height
real    hp                 !Terrain height
real    xmap, ymap         !Map factors
real    frac               !Fraction of mass to go into second puff
logical lrfl_top, lrfl_bot !Flags for reflection off inversion and surface

! --- LOCALS

integer naux

real zlow
real szz, delz, temz, sxz, syz

!----- set puff above zi

!----- Don't do for puffs much deeper than the BL
IF( zinv < 0.1*p%sv )GO TO 9999

zlow = zinv
call chop_puff(p,sz,tfac,zlow,ztop,zrfl,hp,lrfl_top,lrfl_bot, &
                                                       frac,delz,szz)
if (frac <= 0.05) then
  nsplit = 0
  fsplit = 0.
  go to 9999
end if

npuf   = npuf + 1
nsplit = npuf

puff(npuf)%sxx = p%sxx
puff(npuf)%sxy = p%sxy
puff(npuf)%syy = p%syy

if (p%iaux > 0) then
  puff(npuf)%iaux = npaux
  naux = typeID(p%ityp)%npaux
  npaux = npaux + naux
else
  puff(npuf)%iaux = 0
end if
call set_puff_xc(puff(npuf), p, frac)

puff(npuf)%zc = p%zc

puff(npuf)%xbar = p%xbar
puff(npuf)%ybar = p%ybar
puff(npuf)%zbar = p%zbar + delz

temz = sqrt(szz/p%szz)
sxz  = p%sxz*temz
syz  = p%syz*temz

puff(npuf)%sxz  = sxz
puff(npuf)%syz  = syz
puff(npuf)%szz  = szz

puff(npuf)%ityp = p%ityp
puff(npuf)%inxt = 0
puff(npuf)%iprv = 0
puff(npuf)%idtl = p%idtl
puff(npuf)%idtn = p%idtn
puff(npuf)%ipgd = p%ipgd

!----- set puff below zi (don't move centroid or change second-moments)

frac = 1. - frac

call set_puff_xc(p, p, frac)

p%zc = zinv

9999  continue
return

end
