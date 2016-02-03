!*******************************************************************************
!$RCSfile: util.f90,v $
!$Revision: 1.5 $
!$Date: 2007/02/06 23:33:01 $
!
! 01/31/2007  : Move call of step_mc to step for multiprocessor code -BC 
!*******************************************************************************
integer function naux_matl(icls,igrp,ltot)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:   Number of auxilliary variables for a material
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   IsGas              IsParticle
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
integer icls  !Material class
integer igrp  !Material subgroup
logical ltot  !If total cc is saved in material

! --- LOCALS

integer naux_dens
logical IsGas, IsParticle

naux_matl = 0

if(dynamic)then
  if(IsGas(icls))then
    naux_matl = naux_matl + NAUX_DYNAMICS_GAS
    naux_dens = NAUX_DENSE_GAS
  else if(IsParticle(icls))then
    naux_matl = naux_matl + NAUX_DYNAMICS_PART
    naux_dens = NAUX_DENSE_PART
  end if
end if

if (buoy_gas) then
  naux_matl = naux_matl + NAUX_BUOY
end if

if (dense_gas) then
  naux_matl = naux_matl + naux_dens
end if

if(ltot)then
  naux_matl = naux_matl + NAUX_TOTALCC
end if

return
end

subroutine check_newpuff(naux)
!*******************************************************************************
!
! FUNCTION:  Check if it is ok to create a new puff
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
 
integer naux    !Number of Puff auxilliary variables

if (npuf+2*ncrel >= MAXPUF) then
  nError   = SZ_ERROR
  eMessage = 'Too many puffs'
  write(eInform,'(a,i5)') 'Maximum number is ',MAXPUF
  go to 9999
end if

if (npaux+ncaux+naux-2 > MAXPAUX) then  !npaux,ncaux are pointers, not offsets
  nError   = SZ_ERROR
  eMessage = 'Unable to create new puff. '// &
                'Too many auxiliary puff variables'
  write(eInform,'(a,i7,a,i7)') 'Maximum number is ',MAXPAUX, &
                                ' : Current number of puffs is ',npuf
  if(dense_gas)then
    eAction = 'Try running with dense gas dynamics turned off'
  else
    eAction = 'SCICHEM aborting run'
  end if
  go to 9999
end if

9999    return

end

subroutine check_splitpuff(naux)
!*******************************************************************************
!
! FUNCTION:  Check if it is ok to create a new puff from split
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
! 
! 28 OCT 2002 : Add new routine - BC
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
integer naux    !Number of Puff auxilliary variables
integer mxpuf

mxpuf = NINT(0.99*FLOAT(MAXPUF))
if (npuf+2*ncrel >= mxpuf) then
  nError   = SZ_ERROR
  eMessage = 'Too many puffs'
  write(eInform,'(a,i5)') 'Maximum number is ',mxpuf
  go to 9999
end if

if (npaux+ncaux+naux-2 > MAXPAUX) then  !npaux,ncaux are pointers, not offsets
  nError   = SZ_ERROR
  eMessage = 'Unable to create new puff. '// &
                'Too many auxiliary puff variables'
  write(eInform,'(a,i7,a,i7)') 'Maximum number is ',MAXPAUX, &
                                ' : Current number of puffs is ',npuf
  if(dense_gas)then
    eAction = 'Try running with dense gas dynamics turned off'
  else
    eAction = 'SCICHEM aborting run'
  end if
  go to 9999
end if

9999    return

end


subroutine get_dynamics(p,pd)
!*******************************************************************************
!
! FUNCTION:   Copy the dynamics data from the puff auxiliary array 
!             to the dynamics structure
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   IsGas
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p               !Puff data structure
type ( puff_dynamics ) pd   !Puff dynamics structure

! --- LOCALS

integer ipaux

logical IsGas

ipaux  = p%iaux - 1
pd%wcb = puff_aux(ipaux+1)
pd%ctb = puff_aux(ipaux+2)
pd%wcp = puff_aux(ipaux+3)
pd%ctp = puff_aux(ipaux+4)

if(IsGas(typeID(p%ityp)%icls))then
  pd%w = puff_aux(ipaux+5)
  pd%t = puff_aux(ipaux+6)
  ipaux = ipaux + NAUX_DYNAMICS_GAS
else
  pd%w  = 0.0
  pd%t  = 0.0
  ipaux = ipaux + NAUX_DYNAMICS_PART
end if

if (buoy_gas) then
  pd%bcb = puff_aux(ipaux+1)
  pd%bcp = puff_aux(ipaux+2)
  ipaux = ipaux + NAUX_BUOY
else
  pd%bcb = 0.0
  pd%bcp = 0.0
end if

if (dense_gas .and. p%c > 0.0) then
  pd%u  = puff_aux(ipaux+1) / p%c
  pd%v  = puff_aux(ipaux+2) / p%c
  pd%dudx  = puff_aux(ipaux+3) / p%c
  pd%dudy  = puff_aux(ipaux+4) / p%c
  pd%dvdx  = puff_aux(ipaux+5) / p%c
  pd%dvdy  = puff_aux(ipaux+6) / p%c
  if(IsGas(typeID(p%ityp)%icls))then
    pd%u0 = puff_aux(ipaux+7) / p%c
    pd%X  = puff_aux(ipaux+8) / p%c
    pd%Y  = puff_aux(ipaux+9) / p%c
    pd%sn = puff_aux(ipaux+10) / p%c
    pd%cs = puff_aux(ipaux+11) / p%c
  else
    pd%u0 = 0.0
    pd%X  = 0.0
    pd%Y  = 0.0
    pd%sn = 0.0
    pd%cs = 0.0    
  end if
else
  pd%u    = 0.0
  pd%v    = 0.0
  pd%dudx = 0.0
  pd%dudy = 0.0
  pd%dvdx = 0.0
  pd%dvdy = 0.0
end if

return
end

subroutine put_dynamics(p,pd)
!*******************************************************************************
!
! FUNCTION:   Copy the dynamics data from the dynamics structure
!             to the puff auxiliary array 
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   IsGas
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p               !Puff data structure
type ( puff_dynamics ) pd    !Puff dynamics structure

! --- LOCALS

integer ipaux

logical IsGas

ipaux  = p%iaux - 1
puff_aux(ipaux+1) = pd%wcb 
puff_aux(ipaux+2) = pd%ctb 
puff_aux(ipaux+3) = pd%wcp 
puff_aux(ipaux+4) = pd%ctp  

if(IsGas(typeID(p%ityp)%icls))then
  puff_aux(ipaux+5) = pd%w
  puff_aux(ipaux+6) = pd%t
  ipaux = ipaux + NAUX_DYNAMICS_GAS
else
  ipaux = ipaux + NAUX_DYNAMICS_PART
end if

if (buoy_gas) then
  puff_aux(ipaux+1) = pd%bcb 
  puff_aux(ipaux+2) = pd%bcp  
  ipaux = ipaux + NAUX_BUOY
end if

if (dense_gas) then
  puff_aux(ipaux+1) = pd%u*p%c
  puff_aux(ipaux+2) = pd%v*p%c
  puff_aux(ipaux+3) = pd%dudx*p%c
  puff_aux(ipaux+4) = pd%dudy*p%c
  puff_aux(ipaux+5) = pd%dvdx*p%c
  puff_aux(ipaux+6) = pd%dvdy*p%c
  if(IsGas(typeID(p%ityp)%icls))then
    puff_aux(ipaux+7)  = pd%u0*p%c
    puff_aux(ipaux+8)  = pd%X*p%c
    puff_aux(ipaux+9)  = pd%Y*p%c
    puff_aux(ipaux+10) = pd%sn*p%c
    puff_aux(ipaux+11) = pd%cs*p%c
  end if
end if

return
end

subroutine get_totalcc(p,pt)
!*******************************************************************************
!
! FUNCTION:   Copy the totalcc data from the puff auxiliary array 
!             to the totalcc structure
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   IsGas
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p               !Puff data structure
type ( puff_totalcc ) pt          !Puff total cc structure

! --- LOCALS

logical IsGas
integer nskp, ipaux, ndense

if(dynamic)then
  if(IsGas(typeID(p%ityp)%icls))then
    nskp   = NAUX_DYNAMICS_GAS
    ndense = NAUX_DENSE_GAS
  else
    nskp   = NAUX_DYNAMICS_PART
    ndense = NAUX_DENSE_PART
  end if
  if (dense_gas) nskp = nskp + ndense
  if (buoy_gas ) nskp = nskp + NAUX_BUOY
else
  nskp = 0
end if

ipaux = p%iaux+nskp

pt%cctb = puff_aux(ipaux)
pt%cct  = puff_aux(ipaux+1)

return
end

subroutine put_totalcc(p,pt)
!*******************************************************************************
!
! FUNCTION:   Copy the totalcc data from the totalcc structure
!             to the puff auxiliary array 
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                   IsGas
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p               !Puff data structure
type ( puff_totalcc ) pt          !Puff total cc structure

! --- LOCALS

logical IsGas
integer nskp, ipaux, ndense

if(dynamic)then
  if(IsGas(typeID(p%ityp)%icls))then
    nskp   = NAUX_DYNAMICS_GAS
    ndense = NAUX_DENSE_GAS
  else
    nskp   = NAUX_DYNAMICS_PART
    ndense = NAUX_DENSE_PART
  end if
  if (dense_gas) nskp = nskp + ndense
  if (buoy_gas ) nskp = nskp + NAUX_BUOY
else
  nskp = 0
end if

ipaux = p%iaux+nskp

puff_aux(ipaux)   = pt%cctb
puff_aux(ipaux+1) = pt%cct

return
end

subroutine get_static(p,ps)
!*******************************************************************************
!
! FUNCTION:   Copy the static puff data from the puff auxiliary array 
!             to the static puff structure
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
 
type ( puff_str ) p               !Puff data structure
type ( puff_static ) ps           !Puff statics structure

! --- LOCALS

integer ipaux

ipaux = p%iaux + typeID(p%ityp)%npaux

ps%sr    = puff_aux(ipaux)
ps%isnxt = nint(puff_aux(ipaux+1))
ps%isprv = nint(puff_aux(ipaux+2))

return
end

subroutine put_static(p,ps)
!*******************************************************************************
!
! FUNCTION:   Copy the static puff data from the static puff structure
!             to the puff auxiliary array 
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
 
type ( puff_str ) p               !Puff data structure
type ( puff_static ) ps           !Puff statics structure

! --- LOCALS

integer ipaux

ipaux = p%iaux + typeID(p%ityp)%npaux

puff_aux(ipaux)   = ps%sr
puff_aux(ipaux+1) = float(ps%isnxt)
puff_aux(ipaux+2) = float(ps%isprv)

return
end

subroutine get_mc(p,pm)
!*******************************************************************************
!
! FUNCTION:   Copy the multicomponent puff data from the puff auxiliary array 
!             to the multicomponent structure
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
 
type ( puff_str ) p               !Puff data structure
type ( puff_mc ) pm               !Puff multicomponents structure

! --- LOCALS

integer i, nskp, ipaux, nmc
integer nmc2(2)

nskp = typeID(p%ityp)%ipmc - 1

ipaux = p%iaux+nskp-1

nmc   = typeID(p%ityp)%nmc
nmc2(1) = nmc/65536
nmc2(2) = nmc - 65536*nmc2(1)

do i = 1,nmc2(2)
  pm%mc(i) = puff_aux(ipaux+i)
end do

if (nmc2(1) > 0) then
  do i = nmc2(2)+1,nmc2(1)+nmc2(2)
    pm%mc(i) = puff_aux(ipaux+i)/p%c
  end do
end if
if (nmc2(1)+nmc2(2) < MAX_MC2) then
  do i = nmc2(1)+nmc2(2)+1,MAX_MC2
    pm%mc(i) = 0.0
  end do
end if

return
end

subroutine put_mc(p,pm)
!*******************************************************************************
!
! FUNCTION:   Copy the multicomponent data from the multicomponent structure
!             to the puff auxiliary array 
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
 
type ( puff_str ) p               !Puff data structure
type ( puff_mc ) pm               !Puff multicomponents structure

! --- LOCALS

integer i, nskp, ipaux, nmc
integer nmc2(2)

nskp = typeID(p%ityp)%ipmc - 1

ipaux = p%iaux+nskp-1

nmc   = typeID(p%ityp)%nmc
nmc2(1) = nmc/65536
nmc2(2) = nmc - 65536*nmc2(1)

do i = 1,nmc2(2)
  puff_aux(ipaux+i) = pm%mc(i)
end do

if(nmc2(1) > 0) then
  do i = nmc2(2)+1,nmc2(1)+nmc2(2)
    puff_aux(ipaux+i) = pm%mc(i)*p%c
  end do
end if

return
end

subroutine put_mc_frac(p,pm,frac)
!*******************************************************************************
!
! FUNCTION:   Copy the multicomponent data from the multicomponent structure
!             to the puff auxiliary array with the splitting fraction frac 
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
 
type ( puff_str ) p               !Puff data structure
type ( puff_mc ) pm               !Puff multicomponents structure
real  frac                        !split fraction

! --- LOCALS

integer i, nskp, ipaux, nmc
integer nmc2(2)

nskp = typeID(p%ityp)%ipmc - 1

ipaux = p%iaux+nskp-1

nmc   = typeID(p%ityp)%nmc
nmc2(1) = nmc/65536
nmc2(2) = nmc - 65536*nmc2(1)

do i = 1,nmc2(2)
  if(pm%mc(i) /= NOT_SET_R)then
    puff_aux(ipaux+i) = frac*pm%mc(i)
  else
    puff_aux(ipaux+i) = NOT_SET_R
  end if
end do

if(nmc2(1) > 0) then
  do i = nmc2(2)+1,nmc2(1)+nmc2(2)
    if(pm%mc(i) /= NOT_SET_R)then
      puff_aux(ipaux+i) = frac*pm%mc(i)*p%c
    else
      puff_aux(ipaux+i) = NOT_SET_R
    end if
  end do
end if

return
end


subroutine get_mc_ps(p,pm)
!*******************************************************************************
!
! FUNCTION:   Copy the multicomponent puff data from the puff auxiliary array 
!             to the multicomponent structure not saved in ps
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
use multcomp_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p               !Puff data structure
type ( puff_mc ) pm               !Puff multicomponents structure

! --- LOCALS

integer i, nskp, ipaux, nmc, ioff
integer nmc2(2)

nskp = typeID(p%ityp)%ipmc - 1

ipaux = p%iaux+nskp-1

nmc     = typeID(p%ityp)%nmc
nmc2(1) = nmc/65536
nmc2(2) = nmc - 65536*nmc2(1)

! overlap concentration
ioff = nspectot+ncorrt
do i = 1,ncorrm
  pm%mc(ioff+i) = puff_aux(ipaux+ioff+i)/p%c
end do

! vol-1
ioff = 2*nspectot + ncorrm + ncorrt + 1
pm%mc(ioff) = puff_aux(ipaux+ioff)/p%c

ioff = ioff + 4
if (ioff <= MAX_MC2) then
  do i = ioff,MAX_MC2
    pm%mc(i) = 0.0
  end do
end if

return
end

subroutine copy_puff(pold,pnew,inew)
!*******************************************************************************
!
! FUNCTION:  Copy puff "pold" into "pnew"
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
 
type ( puff_str ) pnew,pold     !Puff structure
integer  inew                   !New auxilliary pointer

! --- LOCALS

integer maux, ipaux, jpaux, i

pnew = pold

maux = typeID(pold%ityp)%npaux
if (pold%idtl == I_STATIC) maux = maux + NAUX_STATIC

if(maux > 0)then
  pnew%iaux = inew
  ipaux = pold%iaux - 1
  jpaux = inew - 1
  do i = 1,maux
    puff_aux(jpaux+i) = puff_aux(ipaux+i)
  end do
  inew = inew + maux
else
  pnew%iaux = 0
end if

return
end


subroutine reset_static_puffs
!*******************************************************************************
!
! FUNCTION:  Remove all static puffs and set releases to NOT_SET_R
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!     remove_static_puffs
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- LOCALS

integer irel, ipuf

do irel = 1,ncrel

  call remove_static_puffs(irel)

  ipuf = MAXPUF + 2*(1-irel) - 1
  puff(ipuf)%c = NOT_SET_R

end do

return
end


subroutine remove_static_puffs(irel)
!*******************************************************************************
!
! FUNCTION: Remove all static puffs associated with release IREL
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              get_static            remove_ipgrd
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none


! --- ARGUMENTS
 
integer irel     !Release number

! --- LOCALS

integer i, ipuf

type ( puff_static ) ps

ipuf = MAXPUF + 2*(1-irel) - 1
i    = puff(ipuf)%inxt

do while (i > 0)
  puff(i)%idtl = I_REMOVE
  call get_static(puff(i),ps)
  call remove_ipgrd(i)
  i = ps%isnxt
end do

return
end


subroutine restart_static_puffs
!*******************************************************************************
!
! FUNCTION: Remove ALL static puffs for restarting
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

! --- LOCALS

integer i

do i = 1,npuf
  if (puff(i)%idtl == I_STATIC) puff(i)%idtl = I_REMOVE
end do

return
end



subroutine remove_ipgrd(ipuf)
!*******************************************************************************
!
! FUNCTION:  Remove a puff from ipgrd
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               set_ipgrd
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
integer ipuf  !Puff number

! --- LOCALS

integer iprv, inxt, jprv, ip, k

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

return
end


subroutine scale_puff(scale,p)
!*******************************************************************************
!
! FUNCTION:   Scale a puff
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              scale_psum
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
real scale           !Scale factor
type ( puff_str ) p  !Puff structure

! --- LOCALS

integer i,naux,jpaux

call scale_psum(p,scale)

if(p%iaux > 0)then
  naux = typeID(p%ityp)%npaux
  jpaux = p%iaux - 1
  do i = 1,naux
    if(puff_aux(jpaux+i)  /= NOT_SET_R)then
      puff_aux(jpaux+i) = scale*puff_aux(jpaux+i)
    else
      puff_aux(jpaux+i) = NOT_SET_R
    end if
  end do
end if

return
end


subroutine scale_psum(p,scale)
!*******************************************************************************
!
! FUNCTION:   Scale the psum portion of a puff  (integral values)
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
 
use struct_inc

implicit none

! --- ARGUMENTS
 
real scale              !Scale factor
type ( puff_str ) p     !Puff structure

p%c     = scale*p%c
p%cc    = scale*p%cc 
p%xuc   = scale*p%xuc
p%xvc   = scale*p%xvc
p%yvc   = scale*p%yvc
p%yvsc  = scale*p%yvsc
p%yvbc  = scale*p%yvbc
p%zwc   = scale*p%zwc
p%wc    = scale*p%wc
p%ccb   = scale*p%ccb


return
end


subroutine zero_puff(p)
!*******************************************************************************
!
! FUNCTION:   Zero out a puff
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
 
p%xbar  = 0.0
p%ybar  = 0.0
p%zbar  = 0.0
p%sxx   = 0.0
p%sxy   = 0.0
p%sxz   = 0.0
p%syy   = 0.0
p%syz   = 0.0
p%szz   = 0.0
p%axx   = 0.0
p%axy   = 0.0
p%axz   = 0.0
p%ayy   = 0.0
p%ayz   = 0.0
p%azz   = 0.0
p%det   = 0.0
p%c     = 0.0
p%cc    = 0.0
p%xuc   = 0.0
p%xvc   = 0.0
p%yvc   = 0.0
p%yvsc  = 0.0
p%yvbc  = 0.0
p%zwc   = 0.0
p%wc    = 0.0
p%ccb   = 0.0
p%si    = 0.0
p%si2   = 0.0
p%sv    = 0.0
p%sr    = 0.0
p%cfo   = 0.0
p%zi    = 0.0
p%zc    = 0.0
p%uo    = 0.0
p%vo    = 0.0
p%wo    = 0.0

p%ityp  = 0
p%inxt  = 0
p%iprv  = 0
p%ipgd  = 0
p%idtl  = 0
p%idtn  = 0
p%iaux  = 0

return
end


subroutine scale_dynamics(pd,rat)
!*******************************************************************************
!
! FUNCTION:  Scale the puff dynamic variables
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
 
type ( puff_dynamics ) pd     !Puff dynamics structure

real    rat                      !Scale factor

pd%wcb = rat*pd%wcb
pd%ctb = rat*pd%ctb
pd%wcp = rat*pd%wcp
pd%ctp = rat*pd%ctp
pd%w = rat*pd%w
pd%t = rat*pd%t

if (buoy_gas) then
  pd%bcb = rat*pd%bcb
  pd%bcp = rat*pd%bcp
end if

return
end


subroutine copy_rel(pold,pnew,inew)
!*******************************************************************************
!
! FUNCTION:  Copy continuous release puff    POLD -> PNEW
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
 
type ( puff_str ) pnew, pold  !Puff structure
integer i                     !New auxilliary pointer

! --- LOCALS

integer maux, ipaux, jpaux, inew

pnew = pold

maux = typeID(pold%ityp)%npaux
if(maux > 0)then
  pnew%iaux = MAXPAUX - inew - maux + 2
  ipaux = pold%iaux - 1
  jpaux = pnew%iaux - 1
  do i = maux,1,-1
    puff_aux(jpaux+i) = puff_aux(ipaux+i)
  end do
  inew = inew + maux
else
  pnew%iaux = 0
end if

return
end

subroutine update_static_pointers(iout)
!*******************************************************************************
!
! FUNCTION:   Update the static pointers
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              get_static              put_static
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
integer iout    !Puff number

! --- LOCALS

integer iprv, inxt, ipuf

type ( puff_static ) ps

call get_static(puff(iout),ps)

iprv = ps%isprv
inxt = ps%isnxt

if (iprv < 0) then
  ipuf = MAXPUF + 2*(1 + iprv) - 1
  puff(ipuf)%inxt = iout
else
  call get_static(puff(iprv),ps)
  ps%isnxt = iout
  call put_static(puff(iprv),ps)
end if
if (inxt < 0) then
  ipuf = MAXPUF + 2*(1 + inxt) - 1
  puff(ipuf)%iprv = iout
else
  call get_static(puff(inxt),ps)
  ps%isprv = iout
  call put_static(puff(inxt),ps)
end if

return
end

subroutine update_static_rel_pointers(ipuf,irel)
!*******************************************************************************
!
! FUNCTION:   Update the static release pointers
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              get_static              put_static
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
integer ipuf     !Puff number
integer irel     !Release number

! --- LOCALS

type ( puff_static ) ps

integer is

if (puff(ipuf)%inxt > 0) then
  is = puff(ipuf)%inxt
  call get_static(puff(is),ps)
  ps%isprv = -irel
  call put_static(puff(is),ps)
  is = puff(ipuf)%iprv
  call get_static(puff(is),ps)
  ps%isnxt = -irel
  call put_static(puff(is),ps)
end if

return
end

logical function IsGas(i)
!*******************************************************************************
!
! FUNCTION:  Is material "i" a gas?
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
 
use class_inc

implicit none

! --- ARGUMENTS
 
integer i      !Material class


IsGas = btest(i,MATID_GAS)

return
end

logical function IsParticle(i)
!*******************************************************************************
!
! FUNCTION: Is material "i" a particle?
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use class_inc

implicit none

! --- ARGUMENTS
 
integer i     !Material class


IsParticle = btest(i,MATID_PRT)

return
end

logical function IsHazard(i)
!*******************************************************************************
!
! FUNCTION: Is material "i" a hazard material?
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use class_inc

implicit none

! --- ARGUMENTS
 
integer i      !Material class


IsHazard = btest(i,MATID_HAZARD)

return
end

integer function SetClass(class)
!*******************************************************************************
!
! FUNCTION:  Set the material class
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use class_inc

implicit none

! --- ARGUMENTS
 
character*(*) class     !Material class


SetClass = 0

!====== Basic Gas Material

if(class == MAT_GAS)then

  SetClass = ibset(SetClass,MATID_GAS)

!====== Basic Particle Material

else if(class == MAT_PRT)then

  SetClass = ibset(SetClass,MATID_PRT)

end if

return
end

integer function SetClassHazard(iClass)
!*******************************************************************************
!
! FUNCTION:  Set the material class to Hazard
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
 
use class_inc

implicit none

! --- ARGUMENTS
 
integer iClass    !Material class


SetClassHazard = ibset(iClass,MATID_HAZARD)

return
end

integer function SetClassMulti(iClass)
!*******************************************************************************
!
! FUNCTION:  Set the material class to multicomponent
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
 
use class_inc

implicit none

! --- ARGUMENTS
 
integer iClass    !Material class


SetClassMulti = ibset(iClass,MATID_MULTI)

return
end

integer function ClearClassMulti(iClass)
!*******************************************************************************
!
! FUNCTION:    Unset a multicomponent material
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
 
use class_inc

implicit none

! --- ARGUMENTS
 
integer iClass   !Material class


ClearClassMulti = ibclr(iClass,MATID_MULTI)

return
end

integer function GetNdep(icls)
!*******************************************************************************
!
! FUNCTION:    Get the number of material groups that are depositing
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
 
use srfparam_inc

implicit none

! --- ARGUMENTS
 
integer icls       !Material class

GetNdep = NV_BASIC

return

end

integer function GetNdos(icls)
!*******************************************************************************
!
! FUNCTION:    Get the number of material groups that require a dose calculation
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
 
use srfparam_inc

implicit none

! --- ARGUMENTS
 
integer icls       !Material class

GetNdos = NV_BASIC

return

end


subroutine init_paux(p,naux)
!*******************************************************************************
!
! FUNCTION:   Initialize the puff auxilliary array
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
 
type ( puff_str ) p    !Puff structure

integer naux           !Number of auxilliary variables

! --- LOCALS

integer i

if(naux > 0)then
  p%iaux = npaux
  do i = 1,naux
    puff_aux(npaux+i-1) = 0.0
  end do
  npaux = npaux + naux
else
  p%iaux = 0
end if

return
end


logical function IsReady(id,data)
!*******************************************************************************
!
! FUNCTION:   Used by the GUI
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- ARGUMENTS
 
integer id,data     !opid and opmod(OPREADY_INDEX))

! --- LOCALS

integer  STATUS_INVALID
integer  STATUS_VALID
integer  STATUS_COMPLETE
integer  STATUS_INCOMPLETE
integer  STATUS_CUSTOMIZED
integer  STATUS_MODIFIED
integer  STATUS_EMPTY
integer  STATUS_STANDARD

parameter (STATUS_INVALID    = 0 )
parameter (STATUS_VALID      = 1 )
parameter (STATUS_COMPLETE   = 1 )
parameter (STATUS_INCOMPLETE = 2 )
parameter (STATUS_CUSTOMIZED = 4 )
parameter (STATUS_MODIFIED   = 4 )
parameter (STATUS_EMPTY      = 8 )
parameter (STATUS_STANDARD   = 8 )

IsReady = (IAND(data,STATUS_COMPLETE) == STATUS_COMPLETE) .or. &
           (id == 0)

return

end


logical function check_slope(hx,hy)
!*******************************************************************************
!
! FUNCTION:   Check if the terrain slope is significant
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real hx, hy    !Terrain gradients

! --- PARAMETERS
 
real HSMIN
parameter (HSMIN = 1.e-4)

check_slope = abs(hx) > HSMIN .or. abs(hy) > HSMIN

return

end

logical function IsMulti(i)
!*******************************************************************************
!
! FUNCTION:  Is material type multicomponent?
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use class_inc

implicit none

! --- ARGUMENTS
 
integer i        !Material class


IsMulti = btest(i,MATID_MULTI)

return
end


subroutine dump_puff(ipuf,p)
!*******************************************************************************
!
! FUNCTION:  Dump the puff parameters to the log file (called on an error)
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
use files_inc

implicit none

! --- ARGUMENTS
 
integer ipuf          !Puff number
type ( puff_str ) p   !Puff structure

! --- LOCALS

integer i, ios

write(lun_log,'(a)',iostat=ios)'******** PUFF DUMP ************'
write(lun_log,*,iostat=ios)'Time = ',t,'( ',t/3600.,' )'
write(lun_log,*,iostat=ios)'Npuf = ',npuf
write(lun_log,*,iostat=ios)'Puff = ',ipuf
write(lun_log,*,iostat=ios)'Xbar = ',p%xbar, p%ybar, p%zbar
write(lun_log,*,iostat=ios)'Sig  = ',p%sxx,p%sxy,p%sxz,p%syy, &
                                                       p%syz,p%szz
write(lun_log,*,iostat=ios)'Asig = ',p%axx,p%axy,p%axz,p%ayy, &
                                                p%ayz,p%azz,p%det
write(lun_log,*,iostat=ios)'Mass = ',p%c,p%cc,p%ccb,p%cfo
write(lun_log,*,iostat=ios)'hDiff= ',p%xuc,p%xvc,p%yvc,p%yvsc, &
                                                               p%yvbc
write(lun_log,*,iostat=ios)'vDiff= ',p%zwc,p%wc
write(lun_log,*,iostat=ios)'Scale= ',p%si,p%si2,p%sv,p%sr
write(lun_log,*,iostat=ios)'Zi   = ',p%zi,p%zc
write(lun_log,*,iostat=ios)'Vel  = ',p%uo,p%vo,p%wo
write(lun_log,*,iostat=ios)'type = ',p%ityp
write(lun_log,*,iostat=ios)'lev  = ',p%idtl,p%ipgd
write(lun_log,*,iostat=ios)'Point= ',p%inxt,p%iprv,p%idtn, &
                                                               p%iaux
if(p%iaux > 0)then
  write(lun_log,*,iostat=ios)'nAux = ',typeID(p%ityp)%npaux
  write(lun_log,*,iostat=ios)'Aux  = ', &
                 (puff_aux(i),i=p%iaux,p%iaux+typeID(p%ityp)%npaux-1)
end if
write(lun_log,'(a)',iostat=ios)'*******************************'

return
end

integer function set_lev(istep)
!*******************************************************************************
!
! FUNCTION:  Set the time level for the small timestep 
!            number that is being stepped
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
 
implicit none

! --- ARGUMENTS
 
integer istep       !Step number

! --- LOCALS

integer lev, itst

lev = 0
itst = istep
do while ( .not.btest(itst,0) )
  lev = lev + 1
  itst = ishft(itst,-1)
end do

set_lev = lev

return
end

real function trate(t,t_old,t_next)
!*******************************************************************************
!
! FUNCTION:   Find time interpolation factor (for met)
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
 
real t       !Current time
real t_old   !Previous time
real t_next  !Next time

if (t_next <= t_old) then
  trate = 1.0
else
  trate = (t - t_old)/(t_next - t_old)
  trate = amin1(1.,amax1(0.,trate))
end if

return

end

real function sind( x )
!*******************************************************************************
!
! function:   find sin with argument in degrees
!
! preconditions required: 
!
! subroutines and functions called: 
!
! revision history: 
!
!*******************************************************************************

implicit none

real, intent( in ) :: x
real, parameter :: pi180 = 3.141592653/180.

sind = sin( x*pi180 )

return
end

!==============================================================================

real function cosd( x )
implicit none

real, intent( in ) :: x
real, parameter :: pi180 = 3.141592653/180.

cosd = cos( x*pi180 )

return
end

!==============================================================================

real function tand( x )

implicit none

real, intent( in ) :: x
real, parameter :: pi180 = 3.141592653/180.

tand = tan( x*pi180 )

return
end

!==============================================================================

real function asind( x )

implicit none

real, intent( in ) :: x
real, parameter :: pi180 = 3.141592653/180.

asind = asin(x) / pi180

return
end
