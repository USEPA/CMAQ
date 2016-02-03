!*******************************************************************************
!$RCSfile: inter_dyn.f90,v $
!$Revision: 1.5 $
!$Date: 2010/08/27 14:42:06 $
!******************************************************************************
subroutine inter(ilev,jlev,cgrid)
!******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Calculate the puff interactions
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!             clear_inter              inter_puff
!               set_ipgrd         sum_diagnostics             remove_tlev
!             set_cc_part            set_cc_inter
!
! REVISION HISTORY:
! Aug 2010: Updated with changes from March 2005 for Oct. 2004 CMAQ release by PKK, AER -BC(SAGE-MGT)
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use inter_inc
use diagnostics
use interface_definitions, only: reset_cstar

implicit none

! --- ARGUMENTS
 
integer ilev, jlev   !Min and max time levels that are being stepped
REAL :: CGRID( :,:,:,: ) ! ambient concentration of multicomponents

! --- LOCALS

integer ipuf, lev, inxt, jprv, k

logical reset_cc

if (npuf == 0) return

!------ clear interaction terms

reset_cc = .false.

do lev = ilev,jlev
  ipuf = itfrst(lev)
  do while (ipuf > 0)
    kl(ipuf) = -1
    call clear_inter(puff(ipuf),ptmp(ipuf))
    if (puff(ipuf)%cc < 0.0) reset_cc = .true.
    ipuf = puff(ipuf)%idtn
  end do
end do

if (multicomp) then
  call allocate_smax(.true.,npuf,1)
  if (nError /= NO_ERROR) go to 9999
end if

!------ compute mutual overlap between ipuf and its friendly neighbors

irfrst = 0
irlast = 0
nrlist = 0
do lev = ilev,jlev
  iprv = 0
  ipuf = itfrst(lev)
  do while (ipuf > 0)

    call inter_puff(ipuf,lev,ilev)
    if (nError /= NO_ERROR) go to 9999

  end do
end do

if (multicomp) then
  do lev = ilev,jlev
    ipuf = itfrst(lev)
    do while (ipuf > 0)
      call reset_cstar(puff(ipuf),ipuf,cgrid)
      ipuf = puff(ipuf)%idtn
    end do
  end do
end if

if (multicomp) then
  call allocate_smax(.false.,npuf,1)
  if (nError /= NO_ERROR) go to 9999
end if

!------ remove puffs : puffs with small mass or small self-interaction
!       The linked list was built by inter_puff
!       The linked list head is in irfrst
!       The linked list tail is in irlast
!       The linked list is in puff%cfo
!       The previous puff in the time list is in puff%sr

if( irfrst > 0)then
  ipuf = irfrst
  do while (ipuf > 0)

!---------- Remove from the ipgrd list

    iprv = puff(ipuf)%iprv
    inxt = puff(ipuf)%inxt

    if( iprv > 0 )then
      puff(iprv)%inxt = inxt
    else if( iprv < 0) then
      jprv = iabs(iprv)
      ip   = jprv/1000
      k    = jprv - 1000*ip
      call set_ipgrd(ip,2,k,inxt)
    end if

    if( inxt > 0)then
      puff(inxt)%iprv = iprv
    end if

!---------- Remove from the tlev list
!           Check to make sure that it wasn't already removed since inter_puff removes puffs
!           Check to make sure the "previous" puff hasn't already been removed as well

    lev  = puff(ipuf)%idtl
    if(lev >= 0)then
      puff(ipuf)%idtl = I_REMOVE
      if (multicomp) then
        call sum_diagnostics(bndry,puff(ipuf))
      end if
      iprv = nint(puff(ipuf)%sr)
      if(iprv /= 0)then
        do while (puff(iprv)%idtl == I_REMOVE)
          iprv = nint(puff(iprv)%sr)
        end do
      end if
      inxt = puff(ipuf)%idtn
      call remove_tlev(lev,ipuf,iprv,inxt)
    end if

    ipuf = nint(puff(ipuf)%cfo)
  end do
end if

!------ set cc for particle material puffs

do lev = ilev,jlev
  ipuf = itfrst(lev)
  do while (ipuf > 0)
    call set_cc_part(puff(ipuf),ptmp(ipuf))
    ipuf = puff(ipuf)%idtn
  end do
end do

!------ Reset <cc> for new puffs from secondary evaporation

if (reset_cc) then
  do lev = ilev,jlev
    ipuf = itfrst(lev)
    do while (ipuf > 0)
      if (puff(ipuf)%cc < 0.0) then
        call set_cc_inter(puff(ipuf))
      end if
      ipuf = puff(ipuf)%idtn
    end do
  end do
end if

9999    continue

return

end


subroutine test_refl(p,lrfl,zp,h,hx,hy)
!*******************************************************************************
!
! FUNCTION:  Check ground proximity
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
 
type ( puff_str ) p   !Puff structure
real zp               !Puff height relative to the terrain
real h                !Terrain height
real hx, hy           !Terrain gradients
logical lrfl          !Whether or not it should reflect off the ground


! --- LOCALS

real sx, sy, ztest

if (lter) then
  call get_topog(p%xbar,p%ybar,h,hx,hy)
  sx    = sqrt(p%sxx)
  sy    = sqrt(p%syy)
  zp    = p%zbar - h
  ztest = zp - fac_rfl*(abs(hx)*sx + abs(hy)*sy)
else
  h     = 0.
  zp    = p%zbar
  ztest = zp
end if

lrfl = ztest < fac_rfl*sqrt(p%szz)

return
end


subroutine limint(x,d,i1,i2,nx)
!*******************************************************************************
!
! FUNCTION:  Trying to go x+d and x-d, but limit indices to the grid boundaries
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
 
integer i1, i2 !indices
integer nx     !max number of points
real    x, d   !puff centroid location and grid size

if (x-d < 0.0) then
  i1 = 0
else
  i1 = -1
end if

if (x+d > float(nx)) then
  i2 = 0
else
  i2 = 1
end if

return
end


subroutine matrot(a,bsig)
!*******************************************************************************
!
! FUNCTION:   Matrix rotation
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  matmul                  trnsps
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real bsig(7) !Input array & Rotated output array
real a(3,3)  !Defines the rotation

! --- LOCALS

real b(3,3), c(3,3), at(3,3)

b(1,1) = bsig(1)
b(1,2) = bsig(2)
b(1,3) = bsig(3)
b(2,2) = bsig(4)
b(2,3) = bsig(5)
b(3,3) = bsig(6)
b(2,1) = b(1,2)
b(3,1) = b(1,3)
b(3,2) = b(2,3)

call xmatmul(a,b,c)
call trnsps(a,at)
call xmatmul(c,at,b)

bsig(1) = b(1,1)
bsig(2) = b(1,2)
bsig(3) = b(1,3)
bsig(4) = b(2,2)
bsig(5) = b(2,3)
bsig(6) = b(3,3)

return
end


subroutine inter_asig(p)
!*******************************************************************************
!
! FUNCTION:  Set the inverse spread tensor
!            (Account for  ground reflection if necessary)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 reflect
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use inter_inc
use refl_inc

implicit none

! --- ARGUMENTS

type ( puff_str ) p   !Puff structure

! --- LOCALS

real    vfac
integer i

call get_asig(p,asig)

if (lter) then
  call reflect(zp,zp,asig,hx,hy,.false.,xr,vfac)
  asig(1) = b_rfl(1,1)
  asig(2) = b_rfl(1,2)
  asig(3) = b_rfl(1,3)
  asig(4) = b_rfl(2,2)
  asig(5) = b_rfl(2,3)
  asig(6) = b_rfl(3,3)
  zp1     = zp*a_rfl(3,3)
  r_ipuf  = 0.5*zp1/(asig(7)*deth)
else
  a_rfl(3,3) = 1.
  deth    = asig(1)*asig(4) - asig(2)*asig(2)
  r_ipuf  = 0.5*zp/(p%det*deth)
  zp1     = zp
end if

return

end


subroutine inter_bsig(p)
!*******************************************************************************
!
! FUNCTION:  Set separation distances
!            (Account for ground reflection if necessary) 
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  matrot
!
! REVISION HISTORY:
!
! 09-MAY-2001 :  Changed normal distance zp2 to ensure positive values with 
!                terrain - RIS
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use inter_inc
use refl_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p   !Puff structure

! --- LOCALS

integer i

call get_asig(p,bsig)

if (lter) then
  call matrot(a_rfl,bsig)
  betx    = a_rfl(1,1)*delx+a_rfl(1,2)*dely+a_rfl(1,3)*delz
  bety    = a_rfl(2,1)*delx+a_rfl(2,2)*dely+a_rfl(2,3)*delz
  betz    = a_rfl(3,1)*delx+a_rfl(3,2)*dely+a_rfl(3,3)*delz
  zp2     = zp*a_rfl(3,3)
  deth    = bsig(1)*bsig(4) - bsig(2)*bsig(2)
  r_jpuf  = 0.5*zp2/(bsig(7)*deth)
else
  betx    = delx
  bety    = dely
  betz    = delz
  deth    = bsig(1)*bsig(4) - bsig(2)*bsig(2)
  r_jpuf  = 0.5*zp/(bsig(7)*deth)
  zp2     = zp
end if

return
end


subroutine inter_facr(p1,p2)
!*******************************************************************************
!
! FUNCTION:  Calculate the overlap integral (fac) while taking into account
!            ground reflection
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!             inter_error                  overlp
!
! REVISION HISTORY: 
!
! 02 MAR 2001 : Simplify reflection factor - DSH
! 04 JUN 2001 : Modify interactions to use EFUN to avoid EXP overflow - RIS
! 31 JAN 2007 : Force precedence of operations to avoid overflow - BC
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use inter_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p1, p2     !Puff structures

! --- LOCALS

real rat,zr1
character*32 routine         !Subroutine name
real,external ::  overlp, efun

routine = 'inter_facr'
fac = overlp(asig,bsig,betx,bety,betz)

if (fac < -1.0e-6) then
  call inter_error(p1,p2,routine)
  go to 9999
end if

if (fac < 10.) then

  b33 = det/d33
  s33 = -sqrt(b33)

  if (iovlp == 1) then
    zr1 = zp1 + z1
  else
    zr1 = zp2 + z1
  end if

  g_ipuf = -r_ipuf*(zr1 - 0.25*r_ipuf/b33)
  g_jpuf = -r_jpuf*(zr1 - 0.25*r_jpuf/b33)
  z_ipuf = zr1 - 0.5*r_ipuf/b33
  z_jpuf = zr1 - 0.5*r_jpuf/b33

  facn = efun(g_ipuf,s33*z_ipuf) + efun(g_jpuf,s33*z_jpuf)
  den  = PI3*sqrt(8.*(det*p1%det)*p2%det)
  fac  = 0.5*exp(-fac)/den
  if (lter) then
    rat  = hx*hx + hy*hy
    rat  = (rat-1.0)/(rat+1.0)
    facw = fac*(2.+rat*facn)
  else
    facw = fac*(2.-facn)
  end if
  fac  = fac*(2.+facn)

else

  fac  = 0.
  facw = 0.

end if

9999    continue

return
end


subroutine inter_faci(p1,p2)
!*******************************************************************************
!
! FUNCTION:  Calculate the inversion reflection factor for overlap integral
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
! 02 MAR 2001 : Created - DSH
! 09-MAY-2001 : Bug fix: use locals for reflection calculation so don't
!               overwrite surface reflection variables for ipuf - RIS
! 04 JUN 2001 : Modify interactions to use EFUN to avoid EXP overflow - RIS
! 27 SEP 2001 : Calculate interaction only if z1 is below cap - RIS
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use inter_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p1, p2     !Puff structures

! --- LOCALS

real del, faci, r_inv, g_inv, z_inv, zi1
real, external :: efun

logical lset

faci = 1.
lset = .false.

if (p1%zc > 0.) then

  del = p1%zc - p1%zbar

  if (del < fac_rfl*sqrt(p1%szz)) then

    lset = .true.

    b33 = det/d33
    s33 = -sqrt(b33)

    if (iovlp == 1) then
      zi1 = p1%zbar + z1
    else
      zi1 = p2%zbar + z1
    end if

    if (p1%zc >= zi1 ) then
      r_inv = 2.*del/p1%szz
      g_inv = -r_inv*(p1%zc - zi1 - 0.25*r_inv/b33)
      z_inv = p1%zc - zi1 - 0.5*r_inv/b33
      faci = faci + 0.5*efun(g_inv,s33*z_inv)
    end if
    
  end if

end if

if (p2%zc > 0.) then

  del = p2%zc - p2%zbar

  if (del < fac_rfl*sqrt(p2%szz)) then

    if (.not.lset) then
      b33 = det/d33
      s33 = -sqrt(b33)

      if (iovlp == 1) then
        zi1 = p1%zbar + z1
      else
        zi1 = p2%zbar + z1
      end if
    end if

    if (p2%zc >= zi1 ) then
      r_inv = 2.*del/p2%szz
      g_inv = -r_inv*(p2%zc - zi1 - 0.25*r_inv/b33)
      z_inv = p2%zc - zi1 - 0.5*r_inv/b33
      faci = faci + 0.5*efun(g_inv,s33*z_inv)
    end if

  end if

end if

fac  = fac*faci

continue

return
end


subroutine inter_error(p1,p2,routine)
!*******************************************************************************
!
! FUNCTION:   Report an error from routines in inter_dyn
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
use inter_inc
use files_inc

implicit none

! --- ARGUMENTS

type ( puff_str ) p1, p2  !Puff structures

character*32 routine         !Subroutine name
 
! --- LOCALS

integer i

nError = UK_ERROR
eRoutine = routine
eMessage = 'Interaction problem'
write(lun_log,*) 'Interaction problem in '//routine
write(lun_log,*) 'fac,betx,bety,betz:'
write(lun_log,*)fac,betx,bety,betz
write(lun_log,*)'Puff 1:'
call dump_puff(0,p1)
write(lun_log,*)'Puff 2:'
call dump_puff(0,p2)

return
end


subroutine inter_fac(p1,p2)
!*******************************************************************************
!
! FUNCTION:   Calculate the overlap integral (fac)  
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!             inter_error                  overlp
!
! REVISION HISTORY: 
!
! 31 JAN 2007 : Force precedence of operations to avoid overflow - BC
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use inter_inc

implicit none

! --- ARGUMENTS
type ( puff_str ) p1, p2   !Puff structures 

! --- LOCALS

real asig1(7), asig2(7)

character*32 routine       !Subroutine name
real, external :: overlp

routine = 'inter_fac'

betx  = delx
bety  = dely
betz  = p2%zbar - p1%zbar

call get_asig(p1,asig1)
call get_asig(p2,asig2)

fac = overlp(asig1,asig2,betx,bety,betz)
if (fac < -1.0e-6) then
  call inter_error(p1,p2,routine)
  go to 9999
end if

if (fac < 10.) then
  den = PI3*sqrt(8.*(det*p1%det)*p2%det)
  fac = exp(-fac)/den
else
  fac = 0.
end if

facw = fac

9999    continue

return
end


subroutine inter_proc(ipuf,jpuf,pdi,pti,pmi)
!*******************************************************************************
!
! FUNCTION:   Computes interaction between ipuf and jpuf
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               test_refl              inter_bsig              inter_facr
!               inter_fac            get_dynamics             inter_dense
!            put_dynamics             get_totalcc             put_totalcc
!                  get_mc                inter_mc                  put_mc
!         add_remove_list
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use inter_inc

implicit none

! --- ARGUMENTS
 
integer ipuf, jpuf         !Puff numbers

type ( puff_dynamics ) pdi !Puff dynamics structure
type ( puff_totalcc  ) pti !Puff total cc structure
type ( puff_mc       ) pmi !Puff multicomponent strucuture

! --- LOCALS

type ( puff_dynamics ) pdj
type ( puff_totalcc  ) ptj
type ( puff_mc       ) pmj
type ( puff_material ) pmat

real bfac

!----- puff separation

delx = (puff(jpuf)%xbar - puff(ipuf)%xbar)/xmap
dely = (puff(jpuf)%ybar - puff(ipuf)%ybar)/ymap
delz =  puff(jpuf)%zbar - puff(ipuf)%zbar

!------ check ground proximity for jpuf

call test_refl(puff(jpuf),lrfl_jpuf,zp,hp,hxp,hyp)

!------ compute overlap integral factor

if (lrfl_ipuf .and. lrfl_jpuf) then

  call inter_bsig(puff(jpuf))
  call inter_facr(puff(ipuf),puff(jpuf))
  if (nError /= NO_ERROR) go to 9999
else

  call inter_fac(puff(ipuf),puff(jpuf))
  if (nError /= NO_ERROR) go to 9999
end if

!------ set inversion reflection

if (fac > 0.) call inter_faci(puff(ipuf),puff(jpuf))
!------ set interaction factors

ctot  = puff(ipuf)%c
ctotp = puff(jpuf)%c

facc  = fac*ctot
faccp = fac*ctotp
facwp = facw*ctotp
facw  = facw*ctot

!------ interaction with puff of same type

if (ltyp) then
  puff(ipuf)%ccb = puff(ipuf)%ccb + facc*ctotp
  if(.not. lstatic)then
    puff(jpuf)%ccb = puff(jpuf)%ccb + faccp*ctot
  end if
end if

!------ interaction with dynamic puff

if(ldynj .or. (ldyni .and. .not.lstatic) )then
  call get_dynamics(puff(jpuf),pdj)
end if

if (ldynj) then
  pdi%wcb = pdi%wcb + facw*pdj%w
  pdi%ctb = pdi%ctb + facc*pdj%t
  if (buoy_gas) then
    bfac = buoy_fac(puff(jpuf)%ityp)
    pdi%bcb = pdi%bcb + facc*bfac*ctotp
  end if
  if (dense_gas) then
    call inter_dense(puff(jpuf),pdj,puff(ipuf),pdi,-delx,-dely)
  end if
end if

if(ldyni .and. .not.lstatic)then
  pdj%wcb = pdj%wcb + facwp*pdi%w
  pdj%ctb = pdj%ctb + faccp*pdi%t
  if (buoy_gas) then
    bfac = buoy_fac(puff(ipuf)%ityp)
    pdj%bcb = pdj%bcb + faccp*bfac*ctot
  end if
  if (dense_gas) then
    call inter_dense(puff(ipuf),pdi,puff(jpuf),pdj,delx,dely)
  end if
  call put_dynamics(puff(jpuf),pdj)
end if

!------ interaction with puff of same material for total

if (lmat .and. ltot) then
  pti%cctb = pti%cctb + facc*ctotp
  if(.not. lstatic)then
    call get_totalcc(puff(jpuf),ptj)
    ptj%cctb = ptj%cctb + faccp*ctot
    call put_totalcc(puff(jpuf),ptj)
  end if
end if

!------ interaction with puff of same material for multi-component

if (lmat .and. lmc) then
  call get_mc(puff(jpuf),pmj)
  call inter_mc(pmi,pmj,ipuf,jpuf)
  if(.not. lstatic)then
    call put_mc(puff(jpuf),pmj)
  end if
end if

!------ set puff for removal if mass or self-interaction is too small

if (ipuf == jpuf) then
  if ((puff(ipuf)%c <= cmin .or. puff(ipuf)%ccb == 0.))then
    call add_remove_list(ipuf)
  end if
end if

9999    continue

return
end

subroutine clear_inter(p,ptmp)
!*******************************************************************************
!
! FUNCTION:  Clear the interactions
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            get_dynamics           set_dense_gas            put_dynamics
!             get_totalcc             put_totalcc                  get_mc
!                clear_mc                  put_mc                   IsGas
!              IsParticle                 IsMulti
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p  !Puff structure
real ptmp            !Ratio of square of the mean concentration 
                     !for this size bin to total

! --- LOCALS

type ( puff_dynamics ) pd
type ( puff_totalcc ) pt
type ( puff_mc ) pmi

logical   IsParticle, IsMulti, IsGas

ptmp  = 0.0

if (p%iaux > 0) then

  if(dynamic)then
    call get_dynamics(p,pd)

    pd%wcb = 0.0
    pd%ctb = 0.0

    if (buoy_gas) then
      pd%bcb = 0.0
    end if

    if (dense_gas) then
      pd%u = 0.0
      pd%v = 0.0
      pd%dudx = 0.0
      pd%dudy = 0.0
      pd%dvdx = 0.0
      pd%dvdy = 0.0
      if (IsGas(typeID(p%ityp)%icls)) then
        call set_dense_gas(p,pd)
      end if
    end if

    call put_dynamics(p,pd)
  end if

  if(typeID(p%ityp)%ltot)then
    call get_totalcc(p,pt)
    if(IsParticle(typeID(p%ityp)%icls))then
      if (pt%cctb > 0.) then
        ptmp = p%ccb/pt%cctb
      else
        ptmp = 0.
      end if
    end if
    pt%cctb = 0.0
    call put_totalcc(p,pt)
  end if

  if(IsMulti(typeID(p%ityp)%icls))then
    call get_mc(p,pmi)
    call clear_mc(pmi)
    call put_mc(p,pmi)
  end if

end if

p%ccb = 0.0

return

end

subroutine inter_puff(ipuf,lev,ilev)
!*******************************************************************************
!
! FUNCTION: Finds the puffs that should be "interacted" and 
!           calls the interaction routines
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            get_dynamics             get_totalcc                  get_mc
!                  mapfac               puff_grid               test_refl
!              inter_asig                  limint               find_cell
!            set_lprocess              inter_proc            put_dynamics
!             put_totalcc                  put_mc             IsMultiStar

!                 IsMulti                IsHazard                   IsGas
!
!
! REVISION HISTORY: 
!
! 29 NOV 2000 : Add overlap for static puffs on lower grid level - DSH
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use inter_inc

implicit none

! --- ARGUMENTS
 
integer ipuf       !Puff number
integer ilev       !Minimum time level
integer lev        !Current time level


! --- LOCALS

integer jpuf                          
integer ityp, imat, jtyp, jmat, jgrd, igrd, i, j, k
integer k0, i1, i2, j1, j2, ig1, ig2, nk

real      xp0, yp0

type ( puff_dynamics ) pdi
type ( puff_totalcc  ) pti
type ( puff_mc       ) pmi

logical IsGas, IsMultiStar, IsHazard, IsMulti

!------ save IPUF material and type

ityp = puff(ipuf)%ityp
imat = typeID(ityp)%imat
ltot = typeID(ityp)%ltot
if (multicomp) then
  lmc  = IsMulti(typeID(ityp)%icls) .and. IsMultiStar()
else
  lmc = .false.
end if
lhazi = IsHazard(typeID(ityp)%icls)
ldyni = IsGas(typeID(ityp)%icls) .and. dynamic
if(dynamic)then
  call get_dynamics(puff(ipuf),pdi)
end if
if(ltot)then
  call get_totalcc(puff(ipuf),pti)
end if
if(lmc)then
  call get_mc(puff(ipuf),pmi)
end if

call mapfac( puff(ipuf)%xbar , puff(ipuf)%ybar , xmap , ymap)

call puff_grid(puff(ipuf),xp,yp,k0)

!------ check ground proximity for ipuf

call test_refl(puff(ipuf),lrfl_ipuf,zp,h,hx,hy)

if (lrfl_ipuf) then
  call inter_asig(puff(ipuf))
end if

xp0 = max(0.,min(float(nx)-1.e-6,xp))
yp0 = max(0.,min(float(ny)-1.e-6,yp))

nk = int(2.*sqrt(puff(ipuf)%szz)/dzg) + 1
ku(ipuf) = min0(k0 + nk,nz)
kl(ipuf) = max0(k0 - nk,1)

igrd = puff(ipuf)%ipgd

if (dynamic .or. lmc) then
  ig1 = 0
else
  ig1 = max0(0,igrd-1)
end if

ig2 = igrd+1

do jgrd = ig1,ig2

  dgrd  = 0.5**jgrd

  xp = (float(int(xp0/dgrd)) + 0.5)*dgrd
  yp = (float(int(yp0/dgrd)) + 0.5)*dgrd

  call limint(xp,dgrd,i1,i2,nx)
  call limint(yp,dgrd,j1,j2,ny)

  do k = kl(ipuf),ku(ipuf)
    do j = j1,j2

      yy = yp + float(j)*dgrd

      do i = i1,i2

        xx = xp + float(i)*dgrd

        call find_cell(xx,yy,k,jgrd,nx,ny,jpuf)

        do while (jpuf /= 0)

!-------------- check type

          jtyp  = puff(jpuf)%ityp
          jmat  = typeID(jtyp)%imat
          ldynj = IsGas(typeID(jtyp)%icls) .and. dynamic
          lhazj = IsHazard(typeID(jtyp)%icls)

          lmat  = imat == jmat .and. (ltot .or. lmc)
          ltyp  = ityp == jtyp
          lsame = ltyp .or. lmat

          call set_lprocess(ipuf,jpuf,lev,ilev,igrd,jgrd,k0)

          lprocess = lprocess .and. (lhazi .eqv. lhazj)

!------ compute interactions if necessary

          if ( lprocess ) then
            call inter_proc(ipuf,jpuf,pdi,pti,pmi)
            if (nError /= NO_ERROR) go to 9999
          end if

!------ find next guy and check again

          jpuf = puff(jpuf)%inxt

        end do

      end do            ! i=i1,i2
    end do              ! j=j1,j2
  end do                ! k=k1,i2
end do                 ! jgrd=igrd-1,igrd+1

!------ if puff has been removed, change pointer in tlev-list

if(dynamic)then
  call put_dynamics(puff(ipuf),pdi)
end if
if(ltot)then
  call put_totalcc(puff(ipuf),pti)
end if
if(lmc)then
  call put_mc(puff(ipuf),pmi)
end if
if (ilev >= 0) then
  iprv = ipuf
  ipuf = puff(ipuf)%idtn
end if

9999    return
end


subroutine set_lprocess(ipuf,jpuf,lev,ilev,igrd,jgrd,k0)
!*******************************************************************************
!
! FUNCTION: Set flag to calculate interaction between IPUF and JPUF
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                loverlap
!
! REVISION HISTORY: 
!
! 29 NOV 2000 : Add overlap for static puffs on lower grid level - DSH
! 27 MAR 2003 : Add overlap for multicomponent puffs on all grid levels - BC
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use inter_inc

!  Set flag to calculate interaction between IPUF and JPUF
!  Process   - higher/same time levels
!            - lower/same grid levels

implicit none

! --- ARGUMENTS
 
integer ipuf, jpuf   !Puff numbers
integer lev          !Current time level
integer ilev         !Min time level
integer igrd, jgrd   !Puff grid numbers
integer k0           !Vertical grid number

! --- LOCALS

logical loverlap


if (ilev >= 0) then

!------- Call from subroutine INTER

  if (jgrd > igrd) then                ! JPUF is on smaller grid
    if (puff(jpuf)%idtl>=ilev) then    ! don't process unless static 
      lprocess = .false.
      return
    end if
    lstatic  = .true.
  else                                 ! set static flag (i.e. don't update JPUF)
    lstatic  = (puff(jpuf)%idtl<ilev) .or. (ipuf==jpuf)
  end if

!---- JPUF is on same/lower grid level (larger than IPUF)

  if( jgrd < igrd-1 .and. .not.(ldynj.or.lmc) )then ! only process much larger puffs if dynamic or multicomponent
    lprocess = .false.
  else if( lsame .or. ldynj )then        ! same material or dynamic jpuf (only one multicomponent material)
    if( jgrd < igrd )then                ! process lower grid levels
      lprocess = .true.
      lstatic  = lstatic .or. (jgrd < igrd-1)
    else
      lprocess = lstatic .or. puff(jpuf)%idtl > lev
      if( .not.lprocess .and. puff(jpuf)%idtl == lev )then
        lprocess = loverlap( ipuf,jpuf,k0 )
      end if
    end if
  else if( ldyni .and. .not.lstatic )then ! dynamic ipuf, different materials
    if( puff(jpuf)%idtl == lev )then
      lprocess = loverlap( ipuf,jpuf,k0 )
    else
      lprocess = puff(jpuf)%idtl > lev  ! don't process lower time levels
    end if
  else
    lprocess = .false.
  end if

else

!------- Call from subroutine SET_CC

  lstatic  = (ipuf==jpuf)

  lprocess = lsame  .or. ldynj .or. ldyni
  lprocess = lprocess .and. (loverlap(ipuf,jpuf,k0))

end if

return
end


logical function loverlap(ipuf,jpuf,k0)
!*******************************************************************************
!
! FUNCTION:  Determine if two puffs overlap each other
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
use inter_inc

implicit none

! --- ARGUMENTS
 
integer ipuf, jpuf  !Puff numbers
integer k0          !Vertical grid number

integer nk

if (jpuf >= ipuf) then
  loverlap = .true.
else
  if (kl(jpuf) < 0) then  !Set kl, ku if not set yet
    nk = int(2.*sqrt(puff(jpuf)%szz)/dzg) + 1
    ku(jpuf) = min(k0 + nk,nz)
    kl(jpuf) = max(k0 - nk,1)
  end if

  if (puff(jpuf)%zbar < puff(ipuf)%zbar) then
    loverlap = ku(jpuf) < k0
  else
    loverlap = kl(jpuf) > k0
  end if
end if

return
end


subroutine set_cc_inter(p)
!*******************************************************************************
!
! FUNCTION:  Set <cc> based on concentration fluctuation ratio
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!             get_totalcc             put_totalcc
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p   !Puff structure

! --- LOCALS

real      cfr
type ( puff_totalcc  ) pt

cfr = 1.0 - p%cc/p%c

p%cc = cfr*p%ccb

if(typeID(p%ityp)%ltot)then
  call get_totalcc(p,pt)
  pt%cct = cfr*pt%cctb
  call put_totalcc(p,pt)
end if

return
end


subroutine inter_dense(p0,pd0,p,pd,xp,yp)
!*******************************************************************************
!
! FUNCTION:  Set dense gas velocity effects due to puff p0 for puff p
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
 
type ( puff_str ) p, p0         !Puff structures
type ( puff_dynamics ) pd, pd0  !Puff dynamics structures
real xp,yp                      !Puff separation distances

! --- LOCALS

real u0, sn, cs, x1, y1, r2, fac, xx, yy, xl, facx, facy
real u, v, dudx, dudy, dvdx, dvdy

if (pd0%u0 == 0.0) return

u0 = pd0%u0
sn = pd0%sn
cs = pd0%cs

xx = pd0%X**2
yy = pd0%Y**2
xl = sqrt(xx+yy)

!------ Rotate coordinates

x1 = ( cs*xp + sn*yp)/pd0%X
y1 = (-sn*xp + cs*yp)/pd0%Y

r2 = x1*x1 + y1*y1

fac = u0*exp(-r2)
facx = fac*pd0%Y/xl
facy = fac*pd0%X/xl

u = x1*facx
v = y1*facy

dudx = (1.0 - 2.*x1*x1)*facx/pd0%X
dvdy = (1.0 - 2.*y1*y1)*facy/pd0%Y

dudy = -2.*x1*y1*fac/xl
dvdx = dudy

!------ Rotate velocity and gradients back and accumulate

pd%u = pd%u + (cs*u - sn*v)
pd%v = pd%v + (sn*u + cs*v)

pd%dudx = pd%dudx + (cs*cs*dudx + sn*sn*dvdy - sn*cs*(dudy+dvdx))
pd%dvdy = pd%dvdy + (sn*sn*dudx + cs*cs*dvdy + sn*cs*(dudy+dvdx))
pd%dudy = pd%dudy + (cs*cs*dudy - sn*sn*dvdx + sn*cs*(dudx-dvdy))
pd%dvdx = pd%dvdx + (cs*cs*dvdx - sn*sn*dudy + sn*cs*(dudx-dvdy))

return
end


subroutine set_cc_part(p,ptmp)
!*******************************************************************************
!
! FUNCTION:  Set <cc> for particle materials
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!             get_totalcc             put_totalcc              IsParticle
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none


! --- ARGUMENTS
 
type ( puff_str ) p    !Puff structure
real ptmp              !Ratio of square of the mean concentration 
                       !for this size bin to total

! --- LOCALS

type ( puff_totalcc  ) pt
real fac, rat

integer imat
integer ityp

logical IsParticle

ityp = p%ityp
imat = typeID(ityp)%imat

if (IsParticle(material(imat)%icls) .and. ptmp > 0.) then
  call get_totalcc(p,pt)
  rat = p%ccb/pt%cctb
  fac = min(1.0,1.0-(1./ptmp-1./rat)/max(1./rat,1./ptmp))
  pt%cct = max(fac*pt%cct,p%cc,pt%cctb)
  call put_totalcc(p,pt)
end if

return

end


subroutine inter_self(p,ccs)
!*******************************************************************************
!
! FUNCTION: Calculates the self interaction
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               test_refl              inter_asig
!
! REVISION HISTORY: 
!
! 04 JUN 2001 : Modify interactions to use EFUN to avoid EXP overflow - RIS
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use inter_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p   !Puff structure
real ccs              !Square of the mean concentration for this puff

! --- LOCALS

real, external :: erfc, efun

!------ check ground proximity for ipuf

call test_refl(p,lrfl_ipuf,zp,h,hx,hy)

delx = 0.
dely = 0.
delz = 0.
fac = 1./(pi3*sqrt(8.*p%det))
if (lrfl_ipuf) then

  call inter_asig(p)

  b33 = p%det*(4.*(asig(1)*asig(4) - asig(2)**2))
  s33 = -sqrt(1./b33)

  z_ipuf = zp1 - 0.50*r_ipuf*b33
  z_jpuf = zp1 - 0.25*r_ipuf*b33
  z_kpuf = zp1 -      r_ipuf*b33

  facp = erfc(s33*zp1) + efun(-2.*r_ipuf*z_ipuf,s33*z_kpuf)
  facn = 2.*efun(-r_ipuf*z_jpuf,s33*z_ipuf)

end if

!------ interaction with puff of same type

ccs = fac*(p%c**2)

9999    return
end


subroutine add_remove_list(ipuf)
!*******************************************************************************
!
! FUNCTION:  Set puff for removal
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
use inter_inc

implicit none

! --- ARGUMENTS
 
integer ipuf     !Puff number

nrlist = nrlist + 1
if(irfrst == 0)then
  irfrst = ipuf
  irlast = ipuf
else
  puff(irlast)%cfo = float(ipuf)
  irlast           = ipuf
end if
puff(ipuf)%cfo = 0.0
puff(ipuf)%sr  = float(iprv)

return
end

real function efun( arg1,arg )
!*******************************************************************************
!
! FUNCTION:  Product of EXP(arg1)*ERFC(arg)
!
! PRECONDITIONS REQUIRED: None
!
! SUBROUTINES AND FUNCTIONS CALLED: None
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS

real, intent( in ) :: arg1,arg
 
real, parameter :: p  =  0.3275911
real, parameter :: a1 =  0.254829592
real, parameter :: a2 = -0.284496736
real, parameter :: a3 =  1.421413741
real, parameter :: a4 = -1.453152027
real, parameter :: a5 =  1.061405429

real t
 
t = 1./(1.+p*abs(arg))
 
efun = t*(a1+t*(a2+t*(a3+t*(a4+t*a5))))*exp(arg1-arg**2)
 
if( arg < 0.0 )then
  efun = 2.*exp(arg1) - efun
end if
 
return
end

!===============================================================================

logical function PuffRealizable(  sxx,sxy,sxz,syy,syz,szz )
!*******************************************************************************
!
! FUNCTION:  Check for puff realizability
!
! PRECONDITIONS REQUIRED: None
!
! SUBROUTINES AND FUNCTIONS CALLED: None
!
! REVISION HISTORY: 
!
! 10 DEC 2002 : Add new function - BNC
!*******************************************************************************
 
implicit none

real sxx,sxy,sxz,syy,syz,szz

real d11,d12,d13,dalp

PuffRealizable = .false.

if( sxy*sxy >= sxx*syy )goto 9999
if( sxz*sxz >= sxx*szz )goto 9999
if( syz*syz >= syy*szz )goto 9999

d11 = syy*szz - syz*syz
d12 = sxy*szz - sxz*syz
d13 = sxy*syz - sxz*syy

dalp = sxx*d11 - sxy*d12 + sxz*d13

if( dalp <= 0.0 )goto 9999

puffrealizable = .true.

9999 continue

return
end
