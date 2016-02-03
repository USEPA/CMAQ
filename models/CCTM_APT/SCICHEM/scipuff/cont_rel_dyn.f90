!*******************************************************************************
!$RCSfile: cont_rel_dyn.F90,v $
!$Revision: 1.13 $
!$Date: 2010/08/27 14:42:06 $
!*******************************************************************************
!******************************************************************************
!Subroutine set_stack_rel_prise changed for consistency with plume rise
!routines in SMOKE 2.0
!PKK, AER, April 2005
!Aug 2010: Updated with following changes made by PKK,AER
!          a) Nov 2005 for consistency with CMAQ 4.5 dynamic vertical layer
!             allocation
!-BC(Sage-Mgt)
!Feb 2015: Put check for no. of releases > max. allowed, PKK, ENVIRON
!******************************************************************************
subroutine c_init(icrel,jrpuf)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Initialize a continuous release
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              set_c_plen                  mapfac               get_topog
!                 get_met           set_stack_rel
!           set_puff_cont              get_bounds
!                logn_bin          WarningMessage                  chkgrd
!                 IsMulti          num_puff_types
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf
use common_met
use files_inc
use cont_rel_inc
use relparam_fd

implicit none

! --- ARGUMENTS

integer icrel  ! - release number
integer jrpuf  ! - release puff

!    Normal call to initialize continuous release - icrel=jrpuf=0
!    Normal call to re-initialize continuous release - icrel=0,jrpuf<0

! --- LOCALS

logical ltot, linit
logical chkgrd, lmc, IsMulti

integer  i, imat, ityp, jtyp, irel, num_puff_rel
integer  ios, nsg, naux, irpuf, ihaz, nhaz, ityp_haz, icls

real zbar, xmap, ymap, xx, yy, rxx, ryy, szz, massfrac
real h, hx, hy
real weight(MAXSGP),pbounds(MAXSGP+1)

integer,external :: num_puff_types

!-----  Set parameters for simple continuous release

irpuf = max0(jrpuf,0)
linit = icrel <= 0

if(linit)then

!------ Update source data

  call set_c_plen(c_plen,ncrel)

!------ Check to make sure its still active

  if(tdur /= DEF_VAL_R .and. trel+tdur <= t)return

end if

call mapfac( xrel , yrel , xmap , ymap )

xx   = (xrel-xmin)/dxg
yy   = (yrel-ymin)/dyg
rxx  = (sigy*xmap/dxg)**2
ryy  = (sigy*ymap/dyg)**2

if (chkgrd(xx,yy,rxx,ryy)) then

  if (lter) then
    call get_topog(xrel,yrel,h,hx,hy)
    if (nError /= NO_ERROR) go to 9999
  else
    h = 0.
  end if

  zbar = zrel + h

!-------- Set release parameters for 'STACK' type

  ityp = nint(ctyp)
  icls = typeID(ityp)%icls
  lmc  = IsMulti(icls)

  lprcap = .false.
  wake   = .false.
  num_puff_rel = 1

  if (reltyp(2:2) == 'S') then
    szz = (max(0.01*sigy,0.01*zrel))**2
    call get_met(xrel,yrel,zbar,szz,0.,xmap,ymap,0,.false.)
    if (reltyp(3:3) == 'P') then
      if (LEN_TRIM(name_prime) /= 0) then
        call set_stack_rel_prime(lmc,xmap,ymap,zbar,massfrac)  !use prime
        if (nERROR /= NO_ERROR) go to 9999
      end if
      if (wake) then
        if (massfrac < 1.0) then
          num_puff_rel = 2
        else
          num_puff_rel = 1
        end if
      else
        call set_stack_rel_prise(lmc,xmap,ymap,zbar)  !use plume rise formula
        if (nERROR /= NO_ERROR) go to 9999
      end if 
    else
      call set_stack_rel(lmc)             !calc plume rise dynamically
    end if
  else if(lmc) then
    szz = (max(0.01*sigy,0.01*zrel))**2
    call get_met(xrel,yrel,zbar,szz,0.,xmap,ymap,0,.false.)
  end if

  ltot = typeID(ityp)%ltot
  naux = typeID(ityp)%npaux
  imat = typeID(ityp)%imat

  if (hazard == IHAZ_COMB) then
    nhaz = 2
  else
    nhaz = 1
  end if

  ityp_haz = 0
  do ihaz = 1,nhaz
    do irel = 1,num_puff_rel
      if (wake) then
        call load_prime_rel(irel,zbar)
      end if
  
      if (rel_dist <= 0 .or. .not. linit) then

!---------  Set single puff release

        call set_puff_cont(icrel,irpuf,ltot,zbar,xmap,ymap, &
                           cmass,ityp+ityp_haz,naux)
        if (nError /= NO_ERROR) go to 9999

      else

!---------  Set lognormal distribution for release

        call get_bounds(material(imat),mat_aux,nsg,pbounds)
        call logn_bin(pbounds,nsg, &
             rel_param(REL_MMD_INDX),rel_param(REL_SIGMA_INDX),weight)
        do i = 1,nsg
          jtyp = ityp + i - 1
          call set_puff_cont(icrel,irpuf,ltot,zbar,xmap,ymap, &
                             cmass*weight(i),jtyp+ityp_haz,naux)
          if (nError /= NO_ERROR) go to 9999
          if (cmass*weight(i) > 0.0) opid = -iabs(opid)
        end do
        opid = iabs(opid)
      end if

      if (linit) then
        write(lun_log,112,iostat=ios)'C-release activated at t =', &
                t/3600.,'hrs. with ncrel  = ',ncrel
112 format(a,f7.2,a,i4)
        if (ios /= 0) then
          nError   = WR_ERROR
          eRoutine = 'c_init'
          eMessage = 'Error writing SCICHEM log file'
          eInform  = 'File='//TRIM(file_log)
          go to 9999
        end if
      end if

    end do
    ityp_haz = num_puff_types(material(imat),mat_aux)
  end do

else

  if (linit) then
    write(lun_log,112,iostat=ios)'C-release at t =', &
                t/3600.,'hrs. is outside domain - ignored'
    eRoutine = 'c_init'
    if (ios /= 0) then
      nError   = WR_ERROR
      eMessage = 'Error writing SCICHEM log file'
      eInform  = 'File='//TRIM(file_log)
      go to 9999
    end if
    nError   = WN_ERROR
    write(eMessage,*) 'Release at t =',t/3600.,'hrs. is outside domain'
    eInform = 'Release will be ignored'
    call WarningMessage(0,.true.)
    if (nError /= NO_ERROR) go to 9999
  end if

end if

9999    continue

return
end

subroutine c_set_tlev
!*******************************************************************************
!
! FUNCTION: Set release puff time level
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

! --- LOCALS

integer i, ilev, ipuf

!------ Check release puffs for time step limits

!       Puffs are in pairs for release puffs
!       Original release in N, actual release (end of static phase if statics) in N-1
!       puff(N  )%ipgd=2  =>  puff(N-1) is a valid puff
!                     =0  =>  puff(N-1) not yet valid
!       puff(N-1)%ipgd=1  =>  puff(N-1) is static release
!                     =0  =>  puff(N-1) is a standard release

if (ncrel > 0) then
  do i = 1,ncrel
    ipuf = MAXPUF + 2*(1 - i)
    if (puff(ipuf)%ipgd == 2) ipuf = ipuf -1
    ilev = puff(ipuf)%idtl
    mxtlev = max0(mxtlev,ilev)
  end do
end if

return

end

subroutine c_set_puff(icls,ltot,lmc,irel,ipuf,dtx,dtr,lev1,lev2)
!*******************************************************************************
!
! FUNCTION:  Create the new puff from a continuous release
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               copy_puff                  mapfac             get_totalcc
!                  get_mc            get_dynamics                 get_met
!               get_topog               get_gamma              scale_psum
!              set_hscale    init_loc_uncertainty                  siginv
!            set_zcap_rel            put_dynamics             put_totalcc
!                c_set_mc            set_mc_tcorr                  put_mc
!               init_tlev         sum_diagnostics     sum_src_diagnostics
!                IsHazard
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf
use common_met
use cont_rel_inc
use diagnostics

implicit none

! --- ARGUMENTS

integer icls   !- material class
logical ltot   !- flag for totalcc
logical lmc    !- flag for multi-component
integer irel   !- release id
integer ipuf   !- release puf
real dtx       !- release time step ( see below for dtx<0 )
real dtr       !- partial step offset for multiple release steps
integer lev1   ! - time level for stepping ( minimal step for new puff )
integer lev2   ! - max time level so far

! --- LOCALS

integer  ilev, jpuf
logical IsHazard, initialize

type ( puff_dynamics ) pd
type ( puff_totalcc ) pt
type ( puff_mc    ) pm

real xmap, ymap, dt,  velp, sly, ut, vt, wt, wcb
real uubt, vvbt, uvbt, dtc, ux, vx, wx, zmet
real vel2, sxx, syy, gamma, xbar, ybar, zbar, h, dum, dum2, tau, sz

LOGICAL, EXTERNAL :: PuffRealizable

!-----  dtx<0 => this is the initialization of a static set, and
!       we do not need to move the release puff for a moving source

dt = abs(dtx)

!-----  Copy release puff (IPUF) into new puff (NPUF)

call copy_puff(puff(ipuf),puff(npuf),npaux)

!-----  Set flag to denote regular (non-static) release puff

initialize = (puff(npuf)%ipgd /= 1)

!-----  Store new puff location in locals for easy access

xbar = puff(npuf)%xbar
ybar = puff(npuf)%ybar
zbar = puff(npuf)%zbar

call mapfac(xbar,ybar,xmap,ymap)

!-----  Get auxiliary variables before puff mass is reset

if(ltot)then
  call get_totalcc(puff(npuf),pt)
end if

if(lmc)then
  call get_mc(puff(npuf),pm)
end if

if(dynamic)then
  call get_dynamics(puff(npuf),pd)
end if

call get_met(xbar,ybar,zbar,puff(npuf)%szz,0.0, &
                xmap,ymap,0,IsHazard(typeID(puff(npuf)%ityp)%icls))

if(dynamic)then
  if (initialize) then
    wcb = pd%wcb
  else
    wcb = pd%wcp/puff(npuf)%c
  end if
else
  wcb = 0.0
end if
if (lter) then
  call get_topog(xbar,ybar,h,dum,dum2)
  if (nError /= NO_ERROR) go to 9999
else
  h = 0.
end if
zmet = max(zbar-h, sqrt(puff(npuf)%szz))
dddz = difb/zmet
sz = (sqrt(wwbl)+dddz)*dt
if (zmet > sz) then
  tau = sbz/(a*sqrt(max(qqs,1.e-6)))
  if (dt/tau >= 0.00025) then
    puff(npuf)%szz = puff(npuf)%szz &
            + 2.*difb*(dt-tau*(1.-exp(-dt/tau)))
  end if
else
  puff(npuf)%szz = (sqrt(puff(npuf)%szz) + dddz*dt)**2
end if
if (dtdzs <= 0. .and. zbar <= zinv) then
  call get_gamma(xbar,ybar,zinv,gamma,0)
  if (gamma > 0.) then
    puff(npuf)%szz = min(puff(npuf)%szz,0.64*zinv**2)
  end if
end if

!-----  Scale all puff mass variables

call scale_psum(puff(npuf),dt)

!-----  Set puff location and sigmas

call set_hscale(uubt,vvbt,uvbt,sly)

ut = -ub + puff(ipuf)%uo
vt = -vb + puff(ipuf)%vo
wt = -wb + puff(ipuf)%wo + wcb

velp = sqrt(ut*ut+vt*vt+uubt+vvbt+wt*wt)
if (velp == 0.0) then
  nError   = IV_ERROR
  eRoutine = 'c_set_puff'
  eMessage = 'Continuous release with zero velocity'
  eInform  = 'Need a non-zero mean wind or dynamic source momentum'
  go to 9999
end if
c_plen(irel)  = min(sly , c_plen(irel)+0.5*velp*dt)

puff(npuf)%xbar= puff(npuf)%xbar + ub*dtr*xmap
puff(npuf)%ybar= puff(npuf)%ybar + vb*dtr*ymap
puff(npuf)%zbar= puff(npuf)%zbar + wb*dtr

if (initialize) then
  if (puff(npuf)%sxx == 0.) then
    syy  = puff(npuf)%syy
    sxx  = min(syy,puff(npuf)%szz)
    vel2 = ut*ut + vt*vt
    if(vel2 > 0)then
      puff(npuf)%sxx = (ut*ut*sxx + vt*vt*syy)/vel2
      puff(npuf)%syy = (ut*ut*syy + vt*vt*sxx)/vel2
      puff(npuf)%sxy =  ut*vt*(sxx-syy)/vel2
    else
      puff(npuf)%sxx = sxx
    end if
  else
    sxx = (ut*ut+uubt)*puff(npuf)%sxx + &
             (vt*vt+vvbt)*puff(npuf)%syy + &
              ut*vt*puff(npuf)%sxy + &
              wt*wt*puff(npuf)%szz
    sxx = sqrt(sxx)/velp
  end if

  velp = max(velp, 2.*sqrt(sxx)/(c_end(irel)-c_start(irel)))

  ut = 0.5*ut*dt
  vt = 0.5*vt*dt
  wt = 0.5*wt*dt
  puff(npuf)%sxx = ut*ut + puff(npuf)%sxx
  puff(npuf)%syy = vt*vt + puff(npuf)%syy
  puff(npuf)%sxy = ut*vt + puff(npuf)%sxy
  puff(npuf)%szz = wt*wt + puff(npuf)%szz
  puff(npuf)%sxz = ut*wt + puff(npuf)%sxz
  puff(npuf)%syz = vt*wt + puff(npuf)%syz

  call init_loc_uncertainty(puff(npuf))

end if

if (puff(npuf)%szz == 0.0) then
  nError   = IV_ERROR
  eRoutine = 'c_set_puff'
  eMessage = 'Continuous area release with zero vertical velocity'
  eInform  = 'Need vertical wind or dynamic source momentum'
  go to 9999
end if

!----- Calculate inverse sigmas

call siginv( puff(npuf) )

if (initialize) then
  puff(npuf)%ccb = puff(npuf)%cc/velp
  puff(npuf)%cc  = puff(npuf)%ccb
end if

puff(npuf)%si2 = max(c_plen(irel),puff(npuf)%si)

puff(npuf)%uo  = ub
puff(npuf)%vo  = vb
puff(npuf)%wo  = wb

puff(npuf)%zi  = zinv

if (puff(npuf)%zc == 0.) then
  call set_zcap_rel(puff(npuf)%xbar,puff(npuf)%ybar,puff(npuf)%zbar, &
                    puff(npuf)%szz,zinv,dtdzs,puff(npuf)%zc,lbl)
end if

!-----  Set auxialiary variables

if(dynamic)then
  pd%w = pd%w * dt
  pd%t = pd%t * dt
  if (initialize) then
    pd%wcb = pd%wcp*dt/velp
    pd%wcp = pd%wcb
    pd%ctb = pd%ctp*dt/velp
    pd%ctp = pd%ctb
    if (buoy_gas) then
      pd%bcb = pd%bcp*dt/velp
      pd%bcp = pd%bcb
    end if
  else
    pd%wcp = pd%wcp * dt
    pd%ctp = pd%ctp * dt
    pd%wcb = pd%wcb * dt
    pd%ctb = pd%ctb * dt
    if (buoy_gas) then
      pd%bcp = pd%bcp * dt
      pd%bcb = pd%bcb * dt
    end if
  end if
  call put_dynamics(puff(npuf),pd)
end if

if(ltot)then
  if (initialize) then
    pt%cctb = pt%cct*dt/velp
    pt%cct  = pt%cctb
  else
    pt%cctb = pt%cctb*dt
    pt%cct  = pt%cct *dt
  end if
  call put_totalcc(puff(npuf),pt)
end if

if(lmc)then
  call c_set_mc(pm,dt)
  if (initialize) then
    call set_mc_tcorr(pm)
  end if
  call put_mc(puff(npuf),pm)
end if

!------ Update release puff time level

call init_tlev(puff(npuf),pd,ilev,xmap,ymap)
if (nError /= NO_ERROR) go to 9999

lev2 = max0(lev2,ilev,puff(npuf)%idtl)
ilev = max0(ilev,lev1)
!       ilev = min0(ilev,lev2)
if(ilev /= puff(ipuf)%idtl)then
  if(.not.initialize)then
    dtc = (0.5*delt)**2*(0.25**ilev - 0.25**puff(ipuf)%idtl)
    ux = ub - puff(ipuf+1)%uo
    vx = vb - puff(ipuf+1)%vo
    wx = wb - puff(ipuf+1)%wo

    puff(ipuf)%sxx  = puff(ipuf)%sxx + ux*ux*dtc
    puff(ipuf)%sxy  = puff(ipuf)%sxy + ux*vx*dtc
    puff(ipuf)%sxz  = puff(ipuf)%sxz + ux*wx*dtc
    puff(ipuf)%syy  = puff(ipuf)%syy + vx*vx*dtc
    puff(ipuf)%syz  = puff(ipuf)%syz + vx*wx*dtc
    puff(ipuf)%szz  = puff(ipuf)%szz + wx*wx*dtc

    if( .NOT.PuffRealizable(puff(ipuf)%sxx,puff(ipuf)%sxy,puff(ipuf)%sxz, &
                        puff(ipuf)%syy,puff(ipuf)%syz,puff(ipuf)%szz) )then
      puff(ipuf)%sxx  = puff(ipuf)%sxx - ux*ux*dtc
      puff(ipuf)%sxy  = puff(ipuf)%sxy - ux*vx*dtc
      puff(ipuf)%sxz  = puff(ipuf)%sxz - ux*wx*dtc
      puff(ipuf)%syy  = puff(ipuf)%syy - vx*vx*dtc
      puff(ipuf)%syz  = puff(ipuf)%syz - vx*wx*dtc
      puff(ipuf)%szz  = puff(ipuf)%szz - wx*wx*dtc
    end if

  end if
  puff(ipuf)%idtl = ilev
end if

if (dtx >= 0.0) then

!------ Update release puff location for moving source

  puff(ipuf)%xbar = puff(ipuf)%xbar + puff(ipuf)%uo*dt*xmap
  puff(ipuf)%ybar = puff(ipuf)%ybar + puff(ipuf)%vo*dt*ymap
  puff(ipuf)%zbar = puff(ipuf)%zbar + puff(ipuf)%wo*dt

  jpuf = ipuf + 1
  puff(jpuf)%xbar = puff(jpuf)%xbar + puff(jpuf)%uo*dt*xmap
  puff(jpuf)%ybar = puff(jpuf)%ybar + puff(jpuf)%vo*dt*ymap
  puff(jpuf)%zbar = puff(jpuf)%zbar + puff(jpuf)%wo*dt
  if(initialize)then
    puff(jpuf)%idtl = ilev
  end if

! -- diagnostics

  if (multicomp) then
    call sum_diagnostics(statics,puff(npuf))
    call sum_src_diagnostics(emiss,puff(jpuf),dt)
  end if

end if

!------ Remove small puffs - just decrement the puff counter

if ((puff(npuf)%c <= cmin .or. puff(npuf)%ccb == 0.))then
  npuf = npuf - 1
end if

9999    continue

return

end

subroutine set_puff_cont(icrel,irpuf,ltot,zbar,xmap,ymap,xmass,ityp,naux)
!*******************************************************************************
!
! FUNCTION:  Initialize the actual source release puff - stored in the upper
!            part of the PUFF array. Two release puffs are stored for each
!            source to allow for static puffs.
!
!       Original release in N, actual release (end of static phase if statics) in N-1
!       puff(N  )%ipgd=2  =>  puff(N-1) is a valid puff
!                     =0  =>  puff(N-1) not yet valid
!       puff(N-1)%ipgd=1  =>  puff(N-1) is static release
!                     =0  =>  puff(N-1) is a standard release
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!           check_newpuff            set_puff_rel                copy_rel
!
! REVISION HISTORY:
!
!*******************************************************************************

! --- MODULES

use common_puf
use cont_rel_inc

implicit none

! --- ARGUMENTS

integer icrel      ! - release number
integer irpuf      ! - release puff number
logical ltot       ! - flag for totalcc
real    xmass      ! - release mass or release rate read from SCN file
real    zbar       ! - puff height (mean sea level, includes terrain)
real    xmap, ymap ! - map factors
integer ityp   ! - puff type
integer naux   ! - number of auxilliary variables

! --- PARAMETERS

integer, parameter :: NMOVP = 10

! --- LOCALS

integer nstp, ncpuf, mcrel, mcpuf, jtyp
real    vel, ymass

if (xmass <= 0.0) go to 9999

if(icrel == 0)then
  call check_newpuff(naux)
  if (nError /= NO_ERROR)then
    eRoutine = 'set_puff_cont'
    go to 9999
  end if

  lrupdate = .true.
  ncrel = ncrel + 1
  if (ncrel > MAX_ACTIVE_CREL) then
    nError   = IV_ERROR
    eRoutine = 'set_puff_cont'
    eMessage = 'No. of releases greater than max. allowed'
    eInform  = 'Increase max. and rebuild code'
    go to 9999
  end if
  ncpuf = MAXPUF + 2*(1 - ncrel)

  mcpuf = ncpuf
  mcrel = ncrel
  jtyp  = ityp
  ymass = xmass
else
  mcpuf = irpuf
  mcrel = icrel
  jtyp  = puff(mcpuf)%ityp
  ymass = puff(mcpuf)%c
end if

!------ Initialize actual release puff

call set_puff_rel(puff(mcpuf),ymass,zbar,xmap,ymap,jtyp,-icrel,naux)

!------ Copy release to adjacent puff for potential static releases but
!       mark as not set (need copy to define auxiliary space)

if(icrel == 0)then
  call copy_rel(puff(mcpuf),puff(mcpuf-1),ncaux)
  puff(mcpuf-1)%c = NOT_SET_R
  c_start(mcrel) = max(trel,t)
  c_opid (mcrel) = opid
  c_end (mcrel) = trel + tdur
  c_saux(mcrel) = 0
  if (c_plen(mcrel) == NOT_SET_R) then
    if (sigx == 0.) then
      c_plen(mcrel) = min(sigz,sigy)
    else
      c_plen(mcrel) = min(sigx,sigy)
    end if
  end if
  vel  = sqrt(urel*urel+vrel*vrel+wrel*wrel)
  if (sigx == 0.) then
    vel  = vel/min(sigy,sigz)
  else if (sigz == 0.) then
    vel  = vel/min(sigy,sigx)
  else
    vel  = vel/min(sigx,sigy,sigz)
  end if
  nstp = min0(NMOVP,int(vel*tdur)+1)
  nstp = max0(nstp,int(0.003*vel*tdur)+1)
  c_dt(mcrel)  = tdur/float(nstp)
  c_dtr(mcrel) = tdur/float(nstp)
end if

9999    return
end


subroutine set_cc_r
!*******************************************************************************
!
! FUNCTION: Set interaction terms
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            get_dynamics            put_dynamics             get_totalcc
!             put_totalcc                  get_mc                clear_mc
!                  put_mc                  mapfac               get_topog
!                 get_met                inter_mc               c_init_mc
!               init_tlev                 IsMulti             IsMultiStar
!                   IsGas                IsHazard
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf
use common_met
use cont_rel_inc
use inter_inc

implicit none

! --- LOCALS

integer ii, jj
integer imat, jmat, ityp, jtyp, ilev
integer i, j
real    del, faci, facj, facij, tem, um2, wb2, vel
real    xbar, ybar, zbar, ub2, vb2, sxx, syy

type ( puff_dynamics ) pdi, pdj
type ( puff_totalcc ) pti, ptj
type ( puff_mc ) pmi, pmj

logical IsGas, IsMultiStar, IsHazard, lxxi, lxxj, lzzi, lzzj
logical IsMulti

!-----  Clear interaction terms
!------ NB. Release puff interactions in 'cc' not 'ccb'

do i = 1,ncrel

  c_stat(i) = DEF_VAL_R

  j = MAXPUF + 2*(1 - i)

  puff(j)%cc = 0.0

  if (puff(j)%iaux > 0) then

    if(dynamic)then
      call get_dynamics(puff(j),pdj)
      pdj%wcp = 0.0
      pdj%ctp = 0.0
      if (buoy_gas) then
        pdj%bcp = 0.0
      end if
      call put_dynamics(puff(j),pdj)
    end if

    if(typeID(puff(j)%ityp)%ltot)then
      call get_totalcc(puff(j),ptj)
      ptj%cct = 0.0
      call put_totalcc(puff(j),ptj)
    end if

    if(IsMulti(typeID(puff(j)%ityp)%icls))then
      call get_mc(puff(j),pmi)
      call clear_mc(pmi)
      call put_mc(puff(j),pmi)
    end if

  end if

end do

!------ Compute release puff interactions

if (multicomp) then
  call allocate_smax(.true.,2*ncrel,MAXPUF-2*ncrel+1)
  if (nError /= NO_ERROR) go to 9999
end if

do ii = 1,ncrel

  i = MAXPUF + 2*(1 - ii)

  if(c_saux(ii) == 0)then

    ityp = puff(i)%ityp
    imat = typeID(ityp)%imat
    ltot = typeID(ityp)%ltot
    if (multicomp) then
      lmc  = IsMulti(typeID(ityp)%icls) .and. IsMultiStar()
    else
      lmc = .false.
    end if
    ldyni = IsGas(typeID(ityp)%icls) .and. dynamic
    lhazi = IsHazard(typeID(ityp)%icls)

    xbar = puff(i)%xbar
    ybar = puff(i)%ybar
    zbar = puff(i)%zbar

    call mapfac(xbar,ybar,xmap,ymap)

    if (lter) then
      call get_topog(xbar,ybar,h,hx,hy)
      if (nError /= NO_ERROR) go to 9999
    else
      h = 0.
    end if
    call get_met(xbar,ybar,zbar,puff(i)%szz,0.0, &
                  xmap,ymap,0,IsHazard(typeID(ityp)%icls))

    if(dynamic)then
      call get_dynamics(puff(i),pdi)
    end if
    if(ltot)then
      call get_totalcc(puff(i),pti)
    end if
    if(lmc)then
      call get_mc(puff(i),pmi)
    end if

    do jj = ii,ncrel

      if(c_saux(jj) == 0)then
        j = MAXPUF + 2*(1 - jj)

        jtyp = puff(j)%ityp
        jmat = typeID(jtyp)%imat
        ldynj = IsGas(typeID(jtyp)%icls) .and. dynamic
        lhazj = IsHazard(typeID(jtyp)%icls)

        lmat   = imat == jmat .and. (ltot .or. lmc)
        ltyp = ityp == jtyp
        lsame    = ltyp .or. lmat

        lprocess = ( lsame  .or. ldynj .or. ldyni )
        lprocess = lprocess .and. (lhazj .eqv. lhazi)

        lstatic = (i == j)

        del = ((xbar - puff(j)%xbar)/xmap)**2 + &
                   ((ybar - puff(j)%ybar)/ymap)**2 + &
                         (zbar - puff(j)%zbar)**2

        del = sqrt(del)

        if (lprocess .and. .not.lstatic) then
          c_stat(ii) = min(c_stat(ii),del)
          c_stat(jj) = min(c_stat(jj),del)
        end if

        del = (puff(i)%uo - puff(j)%uo)**2 + &
                   (puff(i)%vo - puff(j)%vo)**2 + &
                   (puff(i)%wo - puff(j)%wo)**2 + del

        if ( lprocess .and. del== 0.0) then
          lxxi = puff(i)%sxx == 0.
          lxxj = puff(j)%sxx == 0.
          lprocess = lxxi .eqv. lxxj
          lzzi = puff(i)%szz == 0.
          lzzj = puff(j)%szz == 0.
          lprocess = lprocess .and. (lzzi .eqv. lzzj)
        else
          lprocess = .false.
        end if

!------ compute interactions if necessary

        if ( lprocess ) then

          if (lxxi) then
            fac  = 2.*pi * sqrt((puff(i)%syy+puff(j)%syy)* &
                                          (puff(i)%szz+puff(j)%szz))
          else if (lzzi) then
            fac  = 2.*pi * sqrt((puff(i)%syy+puff(j)%syy)* &
                                          (puff(i)%sxx+puff(j)%sxx))
          else
            ub2 = (puff(i)%uo - ub)**2
            vb2 = (puff(i)%vo - vb)**2
            sxx = puff(i)%sxx + puff(j)%sxx
            syy = puff(i)%syy + puff(j)%syy
            if (ub2+vb2 > 1.e-6) then
              syy = syy*(ub2+vb2)/(ub2+vb2*syy/sxx)
            else
              syy = 0.5*(sxx+syy)
            end if
            fac  = 2.*pi * sqrt(syy*(puff(i)%szz+puff(j)%szz))
          end if

          faci  = puff(i)%c/fac
          facj  = puff(j)%c/fac
          facij = faci*puff(j)%c

          if (ltyp) then
            puff(i)%cc = puff(i)%cc + facij
            if(.not. lstatic)then
              puff(j)%cc = puff(j)%cc + facij
            end if
          end if

          if(ldyni .or. ldynj)then
            call get_dynamics(puff(j),pdj)
          end if

          if(ldynj)then
            pdi%wcp = pdi%wcp + pdj%w*faci
            pdi%ctp = pdi%ctp + pdj%t*faci
            if (buoy_gas) then
              pdi%bcp = pdi%bcp + buoy_fac(jtyp)*puff(j)%c*faci
            end if
          end if

          if(ldyni .and. .not. lstatic)then
            pdj%wcp = pdj%wcp + pdi%w*facj
            pdj%ctp = pdj%ctp + pdi%t*facj
!                   pdj%wcb = sqrt(pdj%wcp/puff(j)%c)
            tem = pdj%wcp/puff(j)%c
            um2 = (puff(j)%uo-ub)**2 + (puff(j)%vo-vb)**2 &
                   + (puff(j)%wo-wb)**2
            wb2 = 0.5*(sqrt(um2*um2+4.*tem*tem) - um2)
            vel = sqrt(um2+wb2)
            if(vel > 1.e-20)then
              pdj%ctb = pdj%ctp/puff(j)%c/vel
            else
              pdj%ctb = 0.0
            end if
            if (pdj%wcp > 0.0) then
              pdj%wcb = sqrt(wb2)
            else
              pdj%wcb = -sqrt(wb2)
            end if
            if (buoy_gas) then
              pdj%bcp = pdj%bcp + buoy_fac(ityp)*puff(i)%c*facj
              if(vel > 1.e-20)then
                pdj%bcb = pdj%bcp/puff(j)%c/vel
              else
                pdj%bcb = 0.0
              end if
            else
              pdj%bcb = 0.
            end if
            call put_dynamics(puff(j),pdj)
          end if

          if(lmat)then
            if(ltot)then
              call get_totalcc(puff(j),ptj)
              pti%cct = pti%cct + facij
              if(.not.lstatic)then
                ptj%cct = ptj%cct + facij
                call put_totalcc(puff(j),ptj)
              end if
            end if
            if(lmc)then
              call get_mc(puff(j),pmj)
              fac = 1.0/fac
              call inter_mc(pmi,pmj,i,j)
              if(.not.lstatic)then
                call put_mc(puff(j),pmj)
              end if
            end if
          end if

        end if
      end if

    end do

    if(dynamic)then
!             pdi%wcb = sqrt(pdi%wcp/puff(i)%c)
      tem = pdi%wcp/puff(i)%c
      um2 = (puff(i)%uo-ub)**2 + (puff(i)%vo-vb)**2 &
             + (puff(i)%wo-wb)**2
      wb2 = 0.5*(sqrt(um2*um2+4.*tem*tem) - um2)
      vel = sqrt(um2+wb2)
      if(vel > 1.e-20)then
        pdi%ctb = pdi%ctp/puff(i)%c/vel
      else
        pdi%ctb = 0.0
      end if
      if (pdi%wcp > 0.0) then
        pdi%wcb = sqrt(wb2)
      else
        pdi%wcb = -sqrt(wb2)
      end if
      if (buoy_gas .and. vel > 1.e-20) then
        pdi%bcb = pdi%bcp/puff(i)%c/vel
      else
        pdi%bcb = 0.0
      end if
      call put_dynamics(puff(i),pdi)
      pdi%wcp = pdi%wcb*puff(i)%c
      pdi%ctp = pdi%ctb*puff(i)%c
      pdi%bcp = pdi%bcb*puff(i)%c
    else
      vel = sqrt( (puff(i)%uo-ub)**2 + (puff(i)%vo-vb)**2 )
    end if

    if(ltot)then
      call put_totalcc(puff(i),pti)
    end if

    if(lmc)then
      if (lxxi) then
        fac  = 2.*pi * sqrt(puff(i)%syy*puff(i)%szz)
      else if (lzzi) then
        fac  = 2.*pi * sqrt(puff(i)%syy*puff(i)%sxx)
      else
        ub2 = (puff(i)%uo - ub)**2
        vb2 = (puff(i)%vo - vb)**2
        sxx = puff(i)%sxx
        syy = puff(i)%syy
        if (ub2+vb2 > 1.e-6) then
          syy = syy*(ub2+vb2)/(ub2+vb2*syy/sxx)
        else
          syy = 0.5*(sxx+syy)
        end if
        fac  = 2.*pi * sqrt(syy*puff(i)%szz)
      end if
      call c_init_mc(pmi,fac,vel)
      call put_mc(puff(i),pmi)
    end if

    puff(i)%axx = 0.0                                   !Flag for init_tlev
    call init_tlev(puff(i),pdi,ilev,xmap,ymap)
    if (nError /= NO_ERROR) go to 9999

 ! else

 !   call init_tlev_pool(puff(i),ilev)
 !   if (nError /= NO_ERROR) go to 9999

  end if

  puff(i)%idtl   = ilev
  c_lev(ii) = puff(i)%idtl
  mxtlev = max0(mxtlev,ilev)

!-------- Set time level for surrogate release puff (as yet uninitialized)

  puff(i-1)%idtl = ilev

end do

if (multicomp) then
  call allocate_smax(.false.,2*ncrel,MAXPUF-2*ncrel+1)
  if (nError /= NO_ERROR) go to 9999
end if

lrupdate = .false.

9999    continue

return
end

subroutine create_static_puffs(icrel,tstatic,t_srf,dt0,cgrid)
!******************************************************************************
!
! FUNCTION:  Create static puffs
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!           check_newpuff            get_dynamics               dump_puff
!              c_set_puff               copy_puff              put_static
!                  siginv            inter_static                  settle
!                  step_p                  get_mc              scale_psum
!          scale_dynamics           set_dense_gas            put_dynamics
!         scale_static_mc                  put_mc              get_static
!   create_static_release                 IsMulti              time_level
!                   IsGas
!
! REVISION HISTORY: 
!Aug 2010: Update with changes made by PKK,AER on Mar 2005 for Oct 2004 CMAQ release -BC (Sage-Mgt)
!******************************************************************************

! --- MODULES

use common_puf
use common_met
use files_inc
use step_p_inc
use interface_definitions, only: step_p, reset_cstar
use multcomp_mc_inc
use diagnostics, only: ddepos, wdepos, chem
use common_mpi

implicit none

! --- ARGUMENTS

integer icrel            ! release number
real    tstatic          ! duration of static calculation
real    t_srf            ! duration of surface integrals
real    dt0              ! initial timestep
REAL :: CGRID( :,:,:,: ) ! ambient concentration of multicomponents

! --- LOCALS

type ( puff_dynamics ) pd
type ( puff_static ) psc, pso
type ( puff_mc    ) pm

integer  mpuf, ipuf, ityp, icls, lev1, lev2, naux, nchg, ilev
integer  isprv, time_level, iaux
integer  ilevm, ios, i, j, ij, k

real tr, dt, ux, uy, uz, rat, chkx, chky, fac_srf, fac_diag
real ur, vr, wr, vel, sigm, dtm, uxo, uyo, uzo, qqx

logical lmc, lsplit, lmov, IsGas, IsMulti, lrealizable
logical, external :: PuffRealizable

mpuf = npuf
ipuf = MAXPUF + 2*(1 - icrel)

ityp = puff(ipuf)%ityp
ltot = typeID(ityp)%ltot
naux = typeID(ityp)%npaux + NAUX_STATIC
icls = typeID(ityp)%icls
lmc  = IsMulti(icls)

call check_newpuff(naux)
if (nError /= NO_ERROR)then
  eRoutine = 'create_static_puff'
  go to 9999
end if

npuf  = npuf + 1

ur = puff(ipuf)%uo
vr = puff(ipuf)%vo
wr = puff(ipuf)%wo

vel  = ur*ur + vr*vr + wr*wr
lmov = ( vel > 0.0 )

if (lmov) then
  vel  = sqrt(vel)
end if

if (dynamic) then
  call get_dynamics(puff(ipuf),pd)
  wdyn = pd%wcb
else
  wdyn = 0.0
end if

lev1 = 0
lev2 = 0
ilev = time_level(dt0)
if (nError /= NO_ERROR)then
  eRoutine = 'create_static_puff'
  write(lun_log,*)'******* TIME LEVEL ERROR ********'
  write(lun_log,*)TRIM(eRoutine)
  write(lun_log,*)TRIM(eInform)
  write(lun_log,*)'DT=',dt0
  write(lun_log,*)'DELT=',delt
  write(lun_log,*)'LEVEL=',ilev,MAXTLV
  call dump_puff(ipuf,puff(ipuf))
  go to 9999
end if

dt0  = delt/(2.**ilev)
call c_set_puff(icls,ltot,lmc,icrel,ipuf,-dt0,0.0,lev1,lev2)
if (nError /= NO_ERROR) go to 9999

if (npuf == mpuf) then  !no release; mass too small
  iaux = puff(ipuf-1)%iaux
  call copy_puff(puff(ipuf),puff(ipuf-1),iaux)
  puff(ipuf  )%ipgd = 2                        !Flag=2 says ipuf-1 is valid
  puff(ipuf-1)%ipgd = 0                        !Flag=0 says not a static release
  return
end if

puff(npuf)%idtl = ilev

uxo = 0.5*(ub-ur)*dt0
uyo = 0.5*(vb-vr)*dt0
uzo = 0.5*(wb-wr+wdyn)*dt0
qqx = qqb*dt0*dt0

tr   = dt0
rat  = 1.0
nchg = 0

lsplit = .false.

!------ Initialize aux pointer for static variables and update npaux
if (puff(npuf)%iaux == 0) then
  puff(npuf)%iaux = npaux
end if
npaux = npaux + NAUX_STATIC

psc%sr = 0.0
psc%isnxt = -icrel
psc%isprv = -icrel
call put_static(puff(npuf),psc)

call siginv(puff(npuf))

do while (tr <= tstatic .and. .not.lsplit)

  call check_newpuff(naux)
  if (nError /= NO_ERROR)then
    eRoutine = 'create_static_puff'
    go to 9999
  end if

!-------- save time level for stepping new puff

  ilev = puff(npuf)%idtl
  puff(npuf)%idtl = I_STATIC

!-------- compute interactions and create new puff

  call allocate_smax(.true.,1,npuf)

  call inter_static(npuf,uxo,uyo,uzo,qqx)
 
  call reset_cstar(puff(npuf),npuf,cgrid)

  call allocate_smax(.false.,1,npuf)

  call copy_puff(puff(npuf),puff(npuf+1),npaux)

  psc%isnxt = npuf + 1
  call put_static(puff(npuf),psc)

  npuf  = npuf + 1

  psc%isprv = npuf - 1
  psc%isnxt = -icrel
  call put_static(puff(npuf),psc)

  dt      = delt/2.**ilev
  lev1    = max0(0,ilev-1)
  fac_srf = t_srf/dt
  fac_diag = 0.  !(use this to not add up chem diagnostics for statics)

  call settle(npuf,0.0,puff(npuf)%sr,fac_rfl)

  nspuf = 1
  allocate(StepMCdat(nspuf),Splitdat(nspuf),STAT=ios)
  if (ios /= 0) then
    nError   = UK_ERROR
    eRoutine = 'create_static_puffs'
    eMessage = 'Error allocating StepMCdat'
    write(eInform,*)'nspuf = ',nspuf
  end if

  call step_p(dt,puff(npuf),npuf,lev1,lev2,nchg,fac_srf,fac_diag,cgrid)
  if (nError /= NO_ERROR) go to 9999

  pStepMCdat => StepMCdat(1)

  call step_mc()
  if (nError /= NO_ERROR) then
    eRoutine = 'create_static_puff'
    j = pStepMCdat%ipuf
    call dump_puff(j,puff(j))
    go to 9999
  end if

  ! update deposition and pm

  !====   deposition
  ij = pStepMCdat%ij
  do i = 1, nspecies
    ddepos2d(ij,i) = ddepos2d(ij,i) + pStepMCdat%ddepos(i) !2D dry deposition field
    wdepos2d(ij,i) = wdepos2d(ij,i) + pStepMCdat%wdepos(i) !2D wet deposition field
    chem(i)   = chem(i)   + pStepMCdat%chem(i)  
    ddepos(i) = ddepos(i) + pStepMCdat%ddepos(i) !2D dry deposition field
    wdepos(i) = wdepos(i) + pStepMCdat%wdepos(i) !2D dry deposition field
  end do

  j                       = pStepMCdat%ipuf
  ps(1:nspectot+nambient) = pStepMCdat%ps(1:nspectot+nambient)
  nbad_chem               = nbad_chem + pStepMCdat%nbd 
  ngd_chem                = ngd_chem + pStepMCdat%ngd 

  call set_mc_from_ps(pm)

  call get_mc_ps(puff(j),pm)

  !====   Put puff multi-component info
  if (SplitDat(1)%frac > 0.) then
    k = SplitDat(1)%npuf
    call put_mc_frac(puff(k),pm,SplitDat(1)%frac)
    call put_mc_frac(puff(j),pm,1.-SplitDat(1)%frac)
  else
    call put_mc(puff(j),pm)
  end if
 
  if(allocated(StepMCdat))deallocate(StepMCdat,SplitDat,STAT=ios)
  if (ios /= 0) then
    nError   = UK_ERROR
    eRoutine = 'create_static_puffs'
    eMessage = 'Error deallocating StepMCdat'
    write(eInform,*)'nspuf = ',nspuf
  end if

  ux = 0.5*(ub-ur)*dt
  uy = 0.5*(vb-vr)*dt
  uz = 0.5*(wb-wr+wdyn)*dt

  puff(npuf)%sxx  = puff(npuf)%sxx + ux*ux - uxo*uxo
  puff(npuf)%sxy  = puff(npuf)%sxy + ux*uy - uxo*uyo
  puff(npuf)%sxz  = puff(npuf)%sxz + ux*uz - uxo*uzo
  puff(npuf)%syy  = puff(npuf)%syy + uy*uy - uyo*uyo
  puff(npuf)%syz  = puff(npuf)%syz + uy*uz - uyo*uzo
  puff(npuf)%szz  = puff(npuf)%szz + uz*uz - uzo*uzo

!------ Check if puff is realizable; if not reset sigmas

  lrealizable = PuffRealizable( puff(npuf)%sxx,puff(npuf)%sxy,puff(npuf)%sxz, &
                                puff(npuf)%syy,puff(npuf)%syz,puff(npuf)%szz )
 
  if( .not.lrealizable )then
    puff(npuf)%sxx  = puff(npuf)%sxx - ux*ux + uxo*uxo
    puff(npuf)%sxy  = puff(npuf)%sxy - ux*uy + uxo*uyo
    puff(npuf)%sxz  = puff(npuf)%sxz - ux*uz + uxo*uzo
    puff(npuf)%syy  = puff(npuf)%syy - uy*uy + uyo*uyo
    puff(npuf)%syz  = puff(npuf)%syz - uy*uz + uyo*uzo
    puff(npuf)%szz  = puff(npuf)%szz - uz*uz + uzo*uzo
  end if

  puff(npuf)%si2  = puff(npuf)%si2 + 2.*sqrt(ux*ux+uy*uy)

  uxo = ux
  uyo = uy
  uzo = uz

  call siginv(puff(npuf))

  if (lmov) then
    sigm = max(puff(npuf)%axx,puff(npuf)%ayy,puff(npuf)%azz)
    dtm = sqrt(1.0/sigm)/(0.003*vel)
    ilevm = time_level(dtm)
    if (nError /= NO_ERROR)then
      eRoutine = 'create_static_puff'
      write(lun_log,*)'******* TIME LEVEL ERROR ********'
      write(lun_log,*)TRIM(eRoutine)
      write(lun_log,*)TRIM(eInform)
      write(lun_log,*)'DT=',dtm
      write(lun_log,*)'DELT=',delt
      write(lun_log,*)'LEVEL=',ilevm,MAXTLV
      write(lun_log,*)'vel=',vel
      call dump_puff(npuf,puff(npuf))
      go to 9999
    end if
    puff(npuf)%idtl = max0(puff(npuf)%idtl,ilevm)
  end if

  if (dynamic) then
    call get_dynamics(puff(npuf),pd)
  end if

  if (lmc) then
    call get_mc(puff(npuf),pm)
  end if

!-------- Scale puff integral variables for change in mass

  call scale_psum(puff(npuf),rat)

!-------- Scale dynamic variables for change in mass
!         N%B. dense variables are not mass integrals but
!              must be reset due to change in puff shape

  if (dynamic) then
    call scale_dynamics(pd,rat)
    if (dense_gas .and. IsGas(icls)) then
      call set_dense_gas(puff(npuf),pd)
    end if
    call put_dynamics(puff(npuf),pd)
  end if

!-------- Scale multi-component species integrals

  if (lmc) then
    call scale_static_mc(pm,rat,.false.)
    call put_mc(puff(npuf),pm)
  end if

  rat = 2.**(ilev-puff(npuf)%idtl)
  tr = tr + dt

  chkx = delx2/(xmap*xmap)
  chky = delx2/(ymap*ymap)
  lsplit = (puff(npuf)%szz > delz2) &
             .or. (puff(npuf)%sxx > chkx) &
             .or. (puff(npuf)%syy > chky)

!------ Update previous puffs if moving source

  if (lmov) then
    isprv = psc%isprv
    do while (isprv > 0)
      puff(isprv)%xbar = puff(isprv)%xbar + ur*dt*xmap
      puff(isprv)%ybar = puff(isprv)%ybar + vr*dt*ymap
      puff(isprv)%zbar = puff(isprv)%zbar + wr*dt
      call get_static(puff(isprv),pso)
      isprv = pso%isprv
    end do
  end if

end do

call create_static_release(puff(npuf),icrel,ub,vb,wb,dt)

!------ Set source puff pointers - initial/final static puffs

ipuf = ipuf - 1
puff(ipuf)%inxt = mpuf + 1
puff(ipuf)%iprv = npuf

puff(npuf)%idtl = I_STATIC

9999    continue

return
end

subroutine inter_static(ipuf,ux,uy,uz,qqx)
!*******************************************************************************
!
! FUNCTION:  Calculate overlap interactions for static puff IPUF
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!             clear_inter                  mapfac               test_refl
!              inter_asig            get_dynamics             get_totalcc
!                  get_mc              inter_proc            put_dynamics
!                  put_mc             put_totalcc         inter_static_mc
!                   IsGas             IsMultiStar                 IsMulti
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use common_puf
use inter_inc

implicit none

! --- ARGUMENTS

integer ipuf     ! - puff number
real ux, uy, uz  ! - 1/2 the length of the static section (vector quantity)
real qqx         ! - diffusion distance

! --- LOCALS

type ( puff_dynamics ) pdi
type ( puff_totalcc  ) pti
type ( puff_mc       ) pmi, pm0

integer ityp
real    cc0, wc0, ct0, bc0, dudx0, dudy0, dvdx0, dvdy0
real    xl, dum, sigl, facl

logical IsGas, IsMultiStar, IsMulti

!------ check ground proximity for ipuf

call clear_inter(puff(ipuf),dum)

ltyp  = .true.
lsame = .true.
ityp  = puff(ipuf)%ityp
ldyni = IsGas(typeID(ityp)%icls) .and. dynamic
ldynj = ldyni
lmat  = .true.
ltot  = .false.
if (multicomp) then
  lmc = IsMulti(typeID(ityp)%icls) .and. IsMultiStar()
else
  lmc = .false.
end if

lstatic = .true.

call mapfac( puff(ipuf)%xbar , puff(ipuf)%ybar , xmap , ymap)

call test_refl(puff(ipuf),lrfl_ipuf,zp,h,hx,hy)

if (lrfl_ipuf) then
  call inter_asig(puff(ipuf))
end if

if (dynamic) then
  call get_dynamics(puff(ipuf),pdi)
end if
if (typeID(ityp)%ltot) then
  call get_totalcc(puff(ipuf),pti)
end if
if(lmc)then
  call get_mc(puff(ipuf),pmi)
end if

call inter_proc(ipuf,ipuf,pdi,pti,pmi)
if (nError /= NO_ERROR) go to 9999

! == Determine if dominated by advection or diffusion

sigl = ux*ux*puff(ipuf)%sxx + 2.0*ux*uy*puff(ipuf)%sxy + &
                 2.0*ux*uz*puff(ipuf)%sxz + uy*uy*puff(ipuf)%syy + &
                 2.0*uy*uz*puff(ipuf)%syz + uz*uz*puff(ipuf)%szz

xl   = ux*ux + uy*uy + uz*uz
if (qqx < xl) then
  facl = 1.77*sqrt(sigl)/xl
  facl = (1. - facl)*(qqx/xl) + facl
else
  facl = 1.
end if

cc0 = puff(ipuf)%ccb
if (dynamic) then
  call put_dynamics(puff(ipuf),pdi)
  wc0 = pdi%wcb
  ct0 = pdi%ctb
  if (buoy_gas) then
    bc0 = pdi%bcb
  end if
  if (dense_gas) then
    dudx0 = pdi%dudx
    dudy0 = pdi%dudy
    dvdx0 = pdi%dvdx
    dvdy0 = pdi%dvdy
  end if
end if

if(lmc)then
  call put_mc(puff(ipuf),pmi)
  pm0 = pmi
end if

puff(ipuf)%ccb = facl*cc0

if (dynamic) then
  pdi%wcb = facl*wc0
  pdi%ctb = facl*ct0
  if (buoy_gas) then
    pdi%bcb = facl*bc0
  end if
  if (dense_gas) then
    pdi%u    = 0.0
    pdi%v    = 0.0
    pdi%dudx = 2.*pdi%dudx - dudx0
    pdi%dudy = 2.*pdi%dudy - dudy0
    pdi%dvdx = 2.*pdi%dvdx - dvdx0
    pdi%dvdy = 2.*pdi%dvdy - dvdy0
  end if
  call put_dynamics(puff(ipuf),pdi)
end if

if (typeID(ityp)%ltot) then
  pti%cctb = facl*cc0
  call put_totalcc(puff(ipuf),pti)
end if

if(lmc)then
  call static_scale_smax( facl/sqrt(2.0),ipuf )
  call inter_static_mc(pmi,pm0,facl)
  call put_mc(puff(ipuf),pmi)
end if

9999    continue
return
end

subroutine init_static_puffs(icrel,t_init,dts,cgrid)
!******************************************************************************
!
! FUNCTION:  Initialize static puffs
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  mapfac               get_topog                 get_met
!               reset_lsv     create_static_puffs               copy_puff
!                IsHazard
!
! REVISION HISTORY: 
! Aug 2010: Update with changes made by PKK,AER on Mar 2005 for Oct 2004 CMAQ release -BC (Sage-Mgt)
!
!******************************************************************************

! --- MODULES

use common_puf
use common_met
use cont_rel_inc
use interface_definitions, only: create_static_puffs

implicit none

! --- ARGUMENTS

integer icrel                   !release number
real    t_init                  !initialization time (relative to delt)
real    dts                     !initial release timestep
REAL :: CGRID( :,:,:,: )        ! ambient concentration of multicomponents

! --- LOCALS

real tstatic, t_srf, dtx, tstat, vel, ur, vr, wr
real remain_dur, remain_step
real xmap, ymap, xbar, ybar, zbar, h, hx, hy

integer ipuf, iaux, icls

logical IsHazard, do_static

!*******************************************************************************

ipuf = MAXPUF + 2*(1 - icrel)

icls = typeID(puff(ipuf)%ityp)%icls

if (c_saux(icrel) /= 0 .or. .not.static) then
  do_static = .false.                   ! No statics for pools
else
  remain_dur  = c_end(icrel)-t
  remain_step = delt-t_init
!  tstatic = remain_dur/10.
  tstatic = 360.
  if (c_stat(icrel) /= DEF_VAL_R) then
    xbar = puff(ipuf)%xbar
    ybar = puff(ipuf)%ybar
    zbar = puff(ipuf)%zbar
    call mapfac(xbar,ybar,xmap,ymap)
    if (lter) then
      call get_topog(xbar,ybar,h,hx,hy)
      if (nError /= NO_ERROR) go to 9999
    else
      h = 0.
    end if
    call get_met(xbar,ybar,zbar,puff(ipuf)%szz,0.0, &
                   xmap,ymap,0,IsHazard(icls))
    if (lsv_oper) call reset_lsv(puff(ipuf)%si)
    ur  = ub - puff(ipuf)%uo
    vr  = vb - puff(ipuf)%vo
    wr  = wb - puff(ipuf)%wo
    qqb = uub + vvb + 2.*(uubl+vvbl) + wwbh + 1.e-6
    vel = sqrt(ur*ur + vr*vr + wr*wr + qqb)
    tstat   = 2.*c_stat(icrel)/vel
    tstatic = min(tstatic,0.5*tstat)
  end if
  tstatic = min(0.25*remain_step,tstatic)
  dtx = delt/2.**puff(ipuf)%idtl
  do_static = (dtx <= 0.1*tstatic)
end if

if (do_static) then
  t_srf   = min(remain_dur,remain_step)
  call create_static_puffs(icrel,tstatic,t_srf,dtx,cgrid)
  if (nError /= NO_ERROR) go to 9999
else
  iaux = puff(ipuf-1)%iaux
  call copy_puff(puff(ipuf),puff(ipuf-1),iaux)
  puff(ipuf  )%ipgd = 2                        !Flag=2 says ipuf-1 is valid
  puff(ipuf-1)%ipgd = 0                        !Flag=0 says not a static release
end if
9999    continue

return

end

subroutine create_static_release(p,icrel,ub,vb,wb,dt)
!*******************************************************************************
!
! FUNCTION:  Create static release puffs
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               copy_puff            get_dynamics                  get_mc
!              scale_psum          scale_dynamics            put_dynamics
!         scale_static_mc                  put_mc                 IsMulti
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf
use cont_rel_inc

implicit none

! --- ARGUMENTS

type ( puff_str ) p  !puff structure
integer  icrel       !release number

real dt              !timestep (sec)
real ub, vb, wb      !background wind components

! --- LOCALS

integer  idum, ipuf
logical lmc, IsMulti
real dtr, dtx, ux, vx, wx, rat

type ( puff_dynamics ) pd
type ( puff_mc  ) pm

logical, external :: PuffRealizable

ipuf = MAXPUF + 2*(1 - icrel) - 1
if (multicomp) then
  lmc  = IsMulti(typeID(p%ityp)%icls)
else
  lmc = .false.
end if

idum = puff(ipuf)%iaux
call copy_puff(p,puff(ipuf),idum)

rat = 1.0/dt

if (dynamic) then
  call get_dynamics(puff(ipuf),pd)
end if

if (lmc) then
  call get_mc(puff(ipuf),pm)
end if

call scale_psum(puff(ipuf),rat)

if (dynamic) then
  call scale_dynamics(pd,rat)
  call put_dynamics(puff(ipuf),pd)
end if

if (lmc) then
  call scale_static_mc(pm,rat,.true.)
  call put_mc(puff(ipuf),pm)
end if

dtr = delt/(2.**p%idtl)

c_dtr(icrel) = 2.*delt

dtx = 0.25*(dtr*dtr-dt*dt)

!------ Pass release timestep back

dt = dtr

ux = ub - puff(ipuf+1)%uo
vx = vb - puff(ipuf+1)%vo
wx = wb - puff(ipuf+1)%wo

puff(ipuf)%sxx  = puff(ipuf)%sxx + ux*ux*dtx
puff(ipuf)%sxy  = puff(ipuf)%sxy + ux*vx*dtx
puff(ipuf)%sxz  = puff(ipuf)%sxz + ux*wx*dtx
puff(ipuf)%syy  = puff(ipuf)%syy + vx*vx*dtx
puff(ipuf)%syz  = puff(ipuf)%syz + vx*wx*dtx
puff(ipuf)%szz  = puff(ipuf)%szz + wx*wx*dtx

if( .not.PuffRealizable(puff(ipuf)%sxx,puff(ipuf)%sxy,puff(ipuf)%sxz, &
                        puff(ipuf)%syy,puff(ipuf)%syz,puff(ipuf)%szz) )then
  puff(ipuf)%sxx = puff(ipuf)%sxx - ux*ux*dtx
  puff(ipuf)%sxy = puff(ipuf)%sxy - ux*vx*dtx
  puff(ipuf)%sxz = puff(ipuf)%sxz - ux*wx*dtx
  puff(ipuf)%syy = puff(ipuf)%syy - vx*vx*dtx
  puff(ipuf)%syz = puff(ipuf)%syz - vx*wx*dtx
  puff(ipuf)%szz = puff(ipuf)%szz - wx*wx*dtx
end if

puff(ipuf)%ipgd = 1

puff(ipuf+1)%ipgd = 2

!------ Moving release velocity

puff(ipuf)%uo = puff(ipuf+1)%uo
puff(ipuf)%vo = puff(ipuf+1)%vo
puff(ipuf)%wo = puff(ipuf+1)%wo

return
end

subroutine set_stack_rel(lmc)
!*******************************************************************************
!
! FUNCTION:  Set true release parameters for STACK release
!            STACK release contains EXIT DIAMETER (m) in size
!                                   EXIT TEMPERATURE (C) in buoy
!                                   EXIT VELOCITY in wmom
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
use common_met

implicit none

! --- ARGUMENTS

logical lmc  ! - multicomponent release flag

! --- LOCALS

real tstack, wstack, rstack, astack, bfac

tab       = tb*(pb**0.285714)
tstack    = buoy + 273.15
wstack    = wmom
rstack    = 0.5*size_rel
astack    = pi*rstack*rstack
bfac      = tab/max(tstack,tab)
buoy      = astack*wstack*(tstack-tab)*bfac
wmom      = astack*wstack*wstack*bfac
sigx      = rstack
sigy      = sigx
sigz      = 0.0
size_rel  = 0.0

return
end

subroutine set_stack_rel_prise(lmc,xmap,ymap,zbar)
!******************************************************************************
!
! FUNCTION:  Set true release parameters for STACK release
!            Use plume rise formula
!            STACK release contains EXIT DIAMETER (m) in size
!                                   EXIT TEMPERATURE (C) in buoy
!                                   EXIT VELOCITY in wmom
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
! Aug 2010: Update with changes made by PKK,AER 
!           a) Mar 2005 for Oct 2004 CMAQ release
!           b) April 2005: Updated for consistency with SMOKE 2.0
! Mar 2012: Updated to use CMAQ 5.0 versions of plume rise and spread routines
!******************************************************************************

! --- MODULES

use common_puf
use common_met
use get_met_inc
use files_inc

implicit none

! --- ARGUMENTS

logical lmc     ! - multicomponent release flag
real xmap, ymap ! - map factors
real zbar       ! - puff height (includes plume rise on output)

! --- LOCALS

real tstack, tab_stk
real ddz, thv1, thv2

integer ipbl, istk, k, nzp, invflg  
     
real  dthdz(MAXZP), ta(MAXZP), wspd(MAXZP) 
real  zh(MAXZP), zf(0:MAXZP), zstk(MAXZP)
real  wstk, zplm, pltop, plbot, sigz0
real  hflx, ustar, zmix

!Updated March 2012, to use CMAQ 5.0 versions of plmris and plsprd instead of
!SCICHEM versions, PK, ENVIRON

!Need to define some variables and constants here, since CMAQ version of 
!plmris does not return inversion flag and plsprd does not return sigz0

! From CONST.EXT:
! mean gravitational acceleration [ m/sec**2 ]
! FSB: Value is mean of polar and equatorial values.
! Source: CRC Handbook (76th Ed) pp. 14-6
REAL, PARAMETER :: GRAV = 9.80622

! From plmris.F
REAL, PARAMETER :: HCRIT   = 1.0E-4 * 0.03  ! hfx min * tolerance
REAL :: HSTAR              ! convective scale at stack (m**2/s**3)

! From plsprd.f
REAL, PARAMETER :: SZ0FAC = 3.545     ! factor used to derive plume depth

INTERFACE
   SUBROUTINE  plmris( EMLAYS, LSTK, HFX, HMIX, &
                       STKDM, STKHT, STKTK, STKVE, &    
                       TSTK, USTAR, DTHDZ, TA, WSPD, & 
                       ZF, ZH, ZSTK, WSTK, ZPLM )
      INTEGER, INTENT (IN) :: EMLAYS    ! no. of emission layers
      INTEGER, INTENT (IN) :: LSTK      ! lyr of top of stack, = RADM's KSTK
      REAL   , INTENT (IN) :: HFX       ! sensible heat flux (M K / S )
      REAL   , INTENT (IN) :: HMIX      ! mixing height (m)
      REAL   , INTENT (IN) :: STKDM     ! stack diameter (m)
      REAL   , INTENT (IN) :: STKHT     ! stack height (m)
      REAL   , INTENT (IN) :: STKTK     ! exhaust temperature (deg K)
      REAL   , INTENT (IN) :: STKVE     ! exhaust velocity (m/s)
      REAL   , INTENT (IN) :: TSTK      ! tmptr at top of stack (deg K)
      REAL,    INTENT (IN) :: USTAR     ! friction velocity [m/s]
      REAL   , INTENT (IN) :: DTHDZ( EMLAYS ) ! gradient of THETV
      REAL   , INTENT (IN) :: TA   ( EMLAYS ) ! temperature (deg K)
      REAL   , INTENT (IN) :: WSPD ( EMLAYS ) ! wind speed (m/s)
      REAL   , INTENT (IN) :: ZF ( 0:EMLAYS ) ! layer surface height (m)
      REAL   , INTENT (IN) :: ZH   ( EMLAYS ) ! layer center height (m) 
      REAL   , INTENT (IN) :: ZSTK ( EMLAYS ) ! zf( l )   - stkht   (m)
      REAL, INTENT(IN OUT) :: WSTK      ! wind speed @ top of stack (m/s)
      REAL, INTENT(OUT)    :: ZPLM      ! effective plume height (m)

   END SUBROUTINE plmris

   SUBROUTINE plsprd( DTHDZ, ZF, KZ, CEFSTK, PLTOP, PLBOT )

      REAL,    INTENT (IN)   :: DTHDZ( KZ ) ! potential temperature lapsed rate
      REAL,    INTENT (IN )  :: ZF( 0:KZ )  ! elevation by layer
      INTEGER, INTENT (IN)   :: KZ          ! number of emissions layers
      REAL,    INTENT (IN)   :: CEFSTK      ! effective stack height
      REAL,    INTENT( OUT ) :: PLTOP       ! plume top (m)
      REAL,    INTENT( OUT ) :: PLBOT       ! plume bottom (m)

   END SUBROUTINE plsprd

END INTERFACE
!------ Set vertical grid (use existing one, if exists)

call set_plmrs_grid(zf,zh,zstk,nzp,ipbl,istk,hp)

! ---- Set stack parameters

tab_stk = tb*(pb**0.285714)
tstack  = buoy + 273.15
wstk    = sqrt(ub*ub + vb*vb)

! --- Set meteorological arrays for plume rise calculation

call get_met(xrel,yrel,0.,0.,0.,xmap,ymap,1,.false.) 
thv1 = (1. + 0.608*hb)*tb

call get_met(xrel,yrel,zh(1),0.,0.,xmap,ymap,1,.false.) 
wspd(1)  = sqrt(ub*ub + vb*vb)
ta(1)    = tb*(pb**0.285714)
thv2     = (1. + 0.608*hb)*tb
dthdz(1) = (thv2-thv1)/zh(1) 

do k = 2, nzp
  ddz = 1./(zh(k) - zh(k-1))
  thv1 = thv2
  call get_met(xrel,yrel,zh(k),0.,0.,xmap,ymap,1,.false.)
  wspd(k)  = sqrt(ub*ub + vb*vb)
  ta(k)    = tb*(pb**0.285714)
  thv2     = (1. + 0.608*hb)*tb
  dthdz(k) = ddz*(thv2-thv1)
end do

!------ interpolate 2d boundary layer variables: zinv, u* and hflx

call get_xyfac(xrel,yrel,xb(1),yb(1),dxb,dyb,nxb,nyb,xmap,ymap,mx,my,mxu,myv,lstagger)
call set_mxy(mx,my,mxy,nxb,nxyb)

call get_blvar(.false.)

hflx  = wtbl
ustar = sqrt(us2)
!debug
!write(*,*)'calling plmris'
!write(*,*)'nzp,istk,hflx,zinv,size_rel,zrel: ', &
!           nzp,istk,hflx,zinv,size_rel,zrel
!write(*,*)'tstack, wmom, tab_stk, ustar, wstk: ', &
!           tstack, wmom, tab_stk, ustar, wstk
!write(*,*)'dthdz: ',dthdz(1:nzp)
!write(*,*)'ta: ',ta(1:nzp)
!write(*,*)'wspd: ',wspd(1:nzp)
!write(*,*)'zf: ',zf(0:nzp)
!write(*,*)'zh: ',zh(1:nzp)
!write(*,*)'zstk: ', zstk(1:nzp)
!debug
call plmris( nzp, istk, hflx, zinv, &
             size_rel, zrel, tstack, wmom, & 
             tab_stk, ustar, dthdz(1:nzp), ta(1:nzp), wspd(1:nzp),& 
             zf(0:nzp), zh(1:nzp), zstk(1:nzp), wstk, zplm )
!debug
!write(*,*)'finished plmris; zplm: ',zplm
!debug
! Compute plume spread
!debug

!write(*,*)'calling plsprd'
!debug

call plsprd( dthdz(1:nzp), zf(0:nzp), nzp, zplm, pltop, plbot )
sigz0 = (pltop - plbot) / SZ0FAC
!debug
!write(*,*)'after plsprd'
!write(*,*)'pltop,plbot,dpth,sigz0: ',pltop,plbot,pltop-plbot,sigz0
!debug

buoy     = 0.
wmom     = 0.
sigy     = sigz0
sigz     = sigz0
sigx     = 0.
size_rel = 0.
zbar     = zplm + hp

!if (invflg == 1 .or. invflg == 2) then   !capped
!  if (zbar > zinv) then
!    write(lun_log,*) 'Limiting plume rise to inversion height: ',zbar,zinv
!    zbar = zinv
!  end if  
!  lprcap = .true.
!end if

! -- to be like SMOKE, comment above and uncomment this

! Find inversion penetration flag
invflg = -1
! Compute convective scale
hstar = GRAV * hflx / ta( 1 )   ! Using surface temperature is correct
if ( hstar > hcrit ) then       ! unstable case:
  zmix = zinv - zrel
  if ( zmix <= 0.0 ) then         ! Stack at or above mixing height:
    invflg = 0
  else
    invflg = 2
  end if
end if

!debug
!write(*,*)'invflg: ',invflg
!debug

if (invflg == 1 .or. invflg == 2 .and. zbar <= zinv) then   !capped
  lprcap = .true.
end if

if (zbar < zrel + hp) then

  nError = WN_ERROR
  eRoutine='set_stack_rel_prise'
  eMessage='Plume rise calculation failed, zbar<zrel'
  write(eInform,*)'Eff stack ht set equal to stack ht', zbar, zrel, zrel + hp, hp
  eAction  = char(0)
  
  call WarningMessage(0,.true.)
  if (nError /= NO_ERROR) go to 9999

  zbar = zrel + hp

end if

write(lun_log,*) 'Plume rise calculation (xrel,yrel,zrel,zeff,hp): ',&
                     xrel, yrel, zrel, zbar, hp

9999 return
end

subroutine set_plmrs_grid(zf,zh,zstk,nzp,ipbl,istk,hp)
!******************************************************************************
!
! FUNCTION:  Set the vertical grid to be used in the plume rise calculation
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
! Aug 2010: Updated with changes made by PKK, AER for consistency with Sep 2005 CMAQ release -BC (Sage-Mgt)
! Mar 2006: Correct setting up of grid as some uninitialised values were being used - BC
!******************************************************************************

! --- MODULES

use common_puf
use common_met

implicit none

! --- ARGUMENTS

real    zh(*)         !Layer center height
real    zf(0:MAXZP)   !Layer surface height
real    zstk(*)       !zf(k) - stack height
integer nzp           !Number of layers
integer ipbl          !Layer of inversion height
integer istk          !Layer of stack height
real    hp            !Terrain height

! --- PARAMETERS

real, parameter :: DELZ =500.

! --- LOCALS
 
integer k
real    dp, hx, hy

!------ Get topography

if (lter) then
  call get_topog(xrel,yrel,hp,hx,hy)
  dp = 1. - hp/zbtop
else
  dp = 1.
  hp = 0.
end if

!------ Set vertical grid  (to actual height and not terrain-following, but exclude terrain height)

zf(0)    = 0.
if (nzb > 1) then
  zh(1)    = zb(1)*dp + 0.01 !  add small amount since interp does not work right
  do k = 2, nzb
    zh(k)    = zb(k)*dp
    zf(k-1)  = 0.5*(zh(k-1)+zh(k))
    zstk(k-1)  = zf(k-1) - zrel
  end do
  zf(nzb)    = zf(nzb-1) + zh(nzb) - zh(nzb-1)
  zstk(nzb)  = zf(nzb) - zrel
  nzp = nzb
else
  nzp = nint(zmax/DELZ)
  zh(1)   = 0.5*DELZ
  zf(1)   = DELZ
  zstk(1) = zf(1) - zrel
  do k = 2, nzp
    zf(k)    = zf(k-1) + DELZ   
    zh(k)    = zh(k-1) + DELZ
    zstk(k)  = zf(k) - zrel
  end do
end if

!------ Set vertical levels of the inversion height and the stack height

ipbl = 1
do while (zf(ipbl) .lt. zinv - hp .and. ipbl .lt. nzp)
  ipbl = ipbl + 1
end do

istk = 1
do while (zf(istk) .lt. zrel .and. istk .lt. nzp)
  istk = istk + 1
end do

return
end
