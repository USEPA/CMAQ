!*******************************************************************************
!$RCSfile: puff_rel_dyn.f90,v $
!$Revision: 1.5 $
!$Date: 2010/08/24 21:32:22 $
!*******************************************************************************
subroutine init_tlev(p,pd,ilev,xmap,ymap)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  Initialize the time levels
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 get_met               reset_lsv            turb_rescale
!       get_puff_material               get_topog               dump_puff
!                IsHazard              IsParticle                   ufall
!              time_level
!
! REVISION HISTORY: 
!
!                  01/31/07 : use constants from constants_fd
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use common_srf
use files_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p        !Puff structure
type ( puff_dynamics ) pd  !Puff dynamics structure
real xmap, ymap            !Map factors
integer ilev               !Time level

! --- LOCALS

type ( part_material ) pmatpart

type ( puff_material ) pmat

equivalence ( pmat, pmatpart )

logical lzinv, lsrf, ldos, lscale, ltest

integer jtyp, icls, time_level, imat

real ufall, hp, hx, hy, ztest
real si, sv, qosi_cc, dtcc, rhoa
real drat, dtw, vel2, sigu, dta, dd, dtk, dtp, dtb
real delg, xlam, sx, sy
real wdyn, bdyn, wp2, vp2, qqvrtx, qpi, qosi_p, qqbi, dsrf
real slp, ri, sz, fac, qqp

logical IsParticle, IsHazard

jtyp = p%ityp
icls = typeID(jtyp)%icls
imat = typeID(jtyp)%imat

call get_met( p%xbar , p%ybar , p%zbar , p%szz , p%zc, &
                     xmap , ymap , 0 , IsHazard(icls) )

lzinv = (p%zbar <= zinv) .AND. lbl

if (lter) then
  call get_topog(p%xbar,p%ybar,hp,hx,hy)
  sx    = sqrt(p%sxx)
  sy    = sqrt(p%syy)
  ztest = p%zbar - hp - fac_rfl*(abs(hx)*sx + abs(hy)*sy)
else
  hp    = 0.0
  ztest = p%zbar
end if

vel2   = ub*ub + vb*vb

si  = p%si
sv  = p%sv

if (lsv_oper) call reset_lsv(si)

if (.not.IsHazard(icls)) then
  call turb_rescale(si,sv,vel2)
end if

qqb  = uub + vvb + 2.*(uubl + vvbl) + wwbh
qqbi = (uub     + vvb )*min(1.0,(si/sby )**0.6666667) &
          + (2.*uubl + wwbh)*min(1.0,(si/sbls)**0.6666667) &
          + (2.*vvbl       )*min(1.0,(si/sbl )**0.6666667)
if (p%sxx > 0.) then
  xlam = min(p%sxx,p%syy)
else
  xlam = p%syy
end if
sx = max(si,sqrt(xlam))
qosi_cc = max(0.5*sqrt(qqb)/sx,0.25*sqrt(qqbi)/si)

!------ account for dynamics

if(dynamic)then
  wdyn = pd%wcp/p%c
  bdyn = G0*min(abs((pd%ctp/tb - pd%bcp)/p%c),1.)
  if (bdyn > 0.) then
    dtb  = 0.5*sqrt(p%sv/bdyn)
  else
    dtb  = delt
  end if
else
  wdyn = 0.
  dtb  = delt
end if

vp2    = vel2 + uub + vvb + 1.e-6
wp2    = wdyn*wdyn

if(abs(wp2) > 1.0e-20)then
  qqvrtx = cvrtx*vp2*wp2/(vp2+wp2)
  sz = sqrt(p%szz)
  slp = min(si,0.65*max(p%zbar-hp,sz))
  if (abs(bdyn) > dtdz*slp) then
    ri  = G0/tb*abs(bdyn)*slp
    if (wp2 < 4.*ri ) then
      fac = 2.0
    else
      ri  = ri/wp2
      fac = 1.0 + 4.*ri
    end if
  else
    ri  = G0/tb*(dtdz*slp-abs(bdyn))*slp
    if ( wp2 < 1.0e-3*ri) then
      fac = 0.0
    else
      ri  = ri/wp2
      fac = 1.0/(1.0 + 4.*ri )
    end if
  end if
  qqp = fac*(cqb*wp2 + qqvrtx)
  qosi_cc = qosi_cc + 0.5*sqrt(qqp)/si
end if

if (qosi_cc > 0.0) then
  dtcc = 0.25/qosi_cc
else
  dtcc = delt
end if

if(IsParticle(icls))then
  call get_puff_material(jtyp,material,mat_aux,pmat)
  rhoa = rhoair*(pb**0.715)*273.2/tb
  drat = ufall(rhoa,pmatpart%rho,rmuair,pmatpart%dbar)
else
  drat = 0.
end if

if (drat > 0.0) then
  dtw = dzg/drat
else
  dtw = delt
end if

if (vel2 > 0.0) then
  lsrf   = material(imat)%lsrfg .or. material(imat)%lsrft
  ldos   = material(imat)%ldosg .or. material(imat)%ldost
  lscale = lsrf .or. ldos! .or. lsmp
  delg   = min(dxb/xmap,dyb/ymap)
  if (lscale) then
      ztest = ztest-fac_rfl*sqrt(p%szz)
      ltest = ztest+min(wdyn,0.)*delt < 0.
      if (ltest) then
        if(lsrf)then
          dsrf = srfdep%delmin
        else
          dsrf = 0.
        end if
        if(ldos)then
          dsrf = max(srfdos%delmin,dsrf)
        end if
!      end if
    end if
    if (ltest) then
      if(p%axx > 0.0)then
        sigu = ub*ub*p%axx + 2.*ub*vb*p%axy + vb*vb*p%ayy
      else
        sigu = vel2/p%syy
      end if
      dta  = sqrt(max(1./sigu,dsrf*dsrf/vel2))
    else
      dta = delg/sqrt(vel2)
    end if
  else
    dta = delg/sqrt(vel2)
  end if
else
  dta = delt
end if

if(lzinv)then
  dd = min(delz2,0.64*zinv**2)
else
  dd = delz2
end if
dtk = dd/(2.*max(difb,1.e-6))

dtp = min(dtcc, dtw, dta, dtk, dtb)

ilev = time_level(dtp)
if (nError /= NO_ERROR) then
  eRoutine = 'init_tlev'
  write(lun_log,*)'******* TIME LEVEL ERROR ********'
  write(lun_log,*)TRIM(eRoutine)
  write(lun_log,*)TRIM(eInform)
  write(lun_log,*)'DT(cc,w,a,k,b)=',dtcc,dtw,dta,dtk,dtb
  write(lun_log,*)'DELT=',delt
  write(lun_log,*)'LEVEL=',ilev,MAXTLV
  write(lun_log,*)'DZG,Drat,difb,vel2,qosi=', &
                                           dzg,drat,difb,vel2,qosi_cc
  call dump_puff(0,p)
  go to 9999
end if

9999    continue

return

end


subroutine set_cc(n1,n2)
!*******************************************************************************
!
! FUNCTION:  Initialize interactions
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  mapfac                  siginv             clear_inter
!              inter_puff    init_loc_uncertainty            get_dynamics
!            put_dynamics             get_totalcc             put_totalcc
!                  get_mc            set_mc_tcorr                  put_mc
!               init_tlev            set_zcap_rel                 IsMulti
!
! REVISION HISTORY: 
!
! 19 JUL 2002 :  Changed to allocate and deallocate species_smax - BC
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met

implicit none

! --- ARGUMENTS
 
integer n1, n2  !New puff numbers (between n1 and n2)

! --- LOCALS

integer ityp, ilev
integer i, j
real xmap, ymap
real dum

logical IsMulti

type ( puff_dynamics ) pdi
type ( puff_totalcc  ) pti
type ( puff_mc       ) pmi

call mapfac( puff(n1)%xbar , puff(n1)%ybar , xmap , ymap)

!----   Prepare to compute interactions

do i = n1,n2
  call siginv(puff(i))
  call clear_inter(puff(i),dum)
end do

!----   Allocate smax to compute interactions for new puffs

if (multicomp) then
  call allocate_smax(.true.,n2-n1+1,n1)
  if (nError /= NO_ERROR) go to 9999
end if

!----   Compute interactions for new puffs

do i = n1,n2
  j = i
  call inter_puff(j,-1,-1)
  if(nError /= NO_ERROR)go to 9999
end do


if (multicomp) then
  call allocate_smax(.false.,n2-n1+1,n1)
  if (nError /= NO_ERROR) go to 9999
end if

! ---- Loop over new puffs to initialize

do i = n1,n2

!-----    Initialize source location uncertainty first
  call init_loc_uncertainty(puff(i))

!-----    Initialize <cc>
  ityp = puff(i)%ityp
  puff(i)%cc  = puff(i)%ccb

!-----    Initialize dynamic correlations
  if(dynamic)then
    call get_dynamics(puff(i),pdi)
    pdi%wcp = pdi%wcb
    pdi%ctp = pdi%ctb
    pdi%bcp = pdi%bcb
    call put_dynamics(puff(i),pdi)
  end if

!-----    Initialize total variance correlations
  if (typeID(ityp)%ltot) then
    call get_totalcc(puff(i),pti)
    pti%cct = pti%cctb
    call put_totalcc(puff(i),pti)
  end if

!-----    Initialize multi-component correlations
  if (IsMulti(typeID(ityp)%icls)) then
    call get_mc(puff(i),pmi)
    call set_mc_tcorr(pmi)
    call put_mc(puff(i),pmi)
  end if

!-----    Initialize met variables
  call init_tlev(puff(i),pdi,ilev,xmap,ymap)
  if (nError /= NO_ERROR) go to 9999
  puff(i)%idtl = ilev
  puff(i)%uo   = ub
  puff(i)%vo   = vb
  puff(i)%wo   = wb
  puff(i)%zi   = zinv
  call set_zcap_rel(puff(i)%xbar,puff(i)%ybar,puff(i)%zbar, &
                             puff(i)%szz,zinv,dtdzs,puff(i)%zc,lbl)

  mxtlev = max0(mxtlev,puff(i)%idtl)

end do

9999    continue

return
end


subroutine init_loc_uncertainty(p)
!*******************************************************************************
!
! FUNCTION:  Initialize the uncertainty in the source location
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
 
type ( puff_str ) p   !Puff structure

! --- LOCALS

real h_unc, v_unc

h_unc = p%zi**2
v_unc = p%sr**2

p%sxx = p%sxx + h_unc
p%syy = p%syy + h_unc
p%szz = p%szz + v_unc

p%zi = 0.0
p%sr = 0.0

return
end


subroutine set_loc_uncertainty(p)
!*******************************************************************************
!
! FUNCTION:  Set the location uncertainty for puff "p"
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                IsHazard
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use relparam_fd

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p  !Puff structure

! --- LOCALS

logical IsHazard

p%zi = 0.0
p%sr = 0.0

if( IsHazard(typeID(p%ityp)%icls) .OR. hazard /= IHAZ_COMB )then
  if(rel_param(REL_H_UNC_INDX) /= NOT_SET_R .AND. &
             rel_param(REL_H_UNC_INDX) /= DEF_VAL_R )then
    p%zi = rel_param(REL_H_UNC_INDX)
  end if
  if( rel_param(REL_V_UNC_INDX) /= NOT_SET_R .AND. &
             rel_param(REL_V_UNC_INDX) /= DEF_VAL_R )then
    p%sr = rel_param(REL_V_UNC_INDX)
  end if
end if

return
end

subroutine set_zcap_rel(xbar,ybar,zbar,szz,zi,tzs,zc,bl_flag)
!*******************************************************************************
!
! FUNCTION:   Set the cap heigt for the release
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_gamma
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
implicit none

! --- ARGUMENTS
 
real xbar, ybar, zbar  !Puff centroid location
real szz               !Puff sigma-z squared
real tzs               !dtdz
real zi, zc            !Inversion height and capping height
logical bl_flag        !If there is a boundary layer or not

! --- LOCALS

real gamma

zc = 0.

if ( zbar < zi .and. tzs <= 0. .and. bl_flag ) then
  
  call get_gamma(xbar,ybar,zi,gamma,0)
  if (gamma > 0.) then
    zc = max(zi,zbar+2.*sqrt(szz))
  end if

end if

return

end


subroutine set_rel(mpuf,npuf)
!*******************************************************************************
!
! FUNCTION:   Set-up new release puffs
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          write_progress                  set_ip                  set_cc
!                add_tlev
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use error_inc

implicit none

! --- ARGUMENTS
 
integer mpuf, npuf  !New puff numbers (between n1 and n2)

! --- LOCALS

character*128 cmsg,cmsg2,cmsg3
character*6   ctmp

cmsg=char(0)
cmsg2=char(0)
write(ctmp,'(i6)') npuf-mpuf+1
cmsg3 = ' Initializing '//TRIM(ADJUSTL(ctmp))//' new puffs'
call write_progress(cmsg,cmsg2,cmsg3)
if(nError /= NO_ERROR)go to 9999

call set_ip(mpuf,npuf)
if (nError /= NO_ERROR) go to 9999

call set_cc(mpuf,npuf)
if (nError /= NO_ERROR) go to 9999

call add_tlev(mpuf,npuf)

9999    continue

return

end


subroutine set_puff_rel(p,xmass,zbar,xmap,ymap,ityp,irel,naux)
!*******************************************************************************
!
! FUNCTION:  Initialize a release puff (either a descriptor or actual)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               zero_puff            get_dynamics            put_dynamics
!              set_mc_rel                  put_mc     set_loc_uncertainty
!                 IsMulti
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met

implicit none

!--------------------------------------------------------------------
!   irel=0 => called from set_puff_cont to initialize a continuous
!             release descriptor puff
!   irel=1 => called from set_puff_inst to initialize a real puff
!--------------------------------------------------------------------

! --- ARGUMENTS
 
type ( puff_str      ) p   !Puff structure

real    xmass              !Puff mass
real    zbar               !Puff vertical coordinate (includes terrain height)
real    xmap, ymap         !Map factors
integer ityp               !Puff type
integer irel               !Puff release number
integer naux               !Number of auxilliary variables


! --- LOCALS

type ( puff_dynamics ) pd
type ( puff_mc       ) pm
integer i,iaux

logical IsMulti

call zero_puff(p)

if (irel == 0)then
  if (lprcap) p%zc = zinv
end if

p%c    = xmass
p%xbar = xrel
p%ybar = yrel
p%zbar = zbar
p%sxx  = sigx*sigx
p%syy  = sigy*sigy
p%szz  = sigz*sigz
if (sigx == 0.0) then
  p%si   = sigy
  p%si2  = sigy
else
  p%si   = min(sigx,sigy)
  p%si2  = max(sigx,sigy)
end if
if (sigz == 0.0) then
  p%sv   = min(sigx,sigy)
else
  p%sv   = sigz
end if
if (wake) then
  p%yvsc = xmass*kyprm
  p%zwc  = xmass*kzprm
end if
p%cfo  = 1.

p%ityp = ityp
p%inxt = 0
p%iprv = 0
p%ipgd = 0
p%idtn = 0
if(naux > 0)then
  if(irel == 0)then
    ncaux = ncaux + naux
    iaux = MAXPAUX - ncaux + 2
    p%iaux = iaux
  else if(irel > 0)then
    iaux = npaux
    npaux = npaux + naux
    p%iaux = iaux
  else
    iaux = p%iaux
  end if
  do i = 1,naux
    puff_aux(iaux+i-1) = 0.0
  end do
else
  p%iaux = 0
end if

if(dynamic)then
  call get_dynamics(p,pd)
  pd%w = wmom
  pd%t = buoy
  call put_dynamics(p,pd)
end if

if (irel <= 0) then
  p%uo   = urel
  p%vo   = vrel
  p%wo   = wrel
end if

if(IsMulti(typeID(p%ityp)%icls))then
  call set_mc_rel(p,pm)
  call put_mc(p,pm)
end if

!------ Store location uncertainties
call set_loc_uncertainty(p)

return

end


subroutine check_scn(nmat,mat,mataux)
!*******************************************************************************
!
! FUNCTION:  Check the scenario
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  cupper              IsParticle              sub_groups
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
 
integer nmat           !Number of materials
real    mataux(*)      !Material auxilliary array

type ( material_str ) mat(*)  !Material strucure

! --- LOCALS

integer i, ityp, nsg, sub_groups

logical IsParticle

write(eAction,'(a,f8.2)') 'Release time TREL=',trel

if (xrel >= DEF_VAL_R) then
  eMessage = 'Must set XREL in scenario input'
  go to 9998
end if

if (yrel >= DEF_VAL_R) then
  eMessage = 'Must set YREL in scenario input'
  go to 9998
end if

if (zrel >= DEF_VAL_R .and. opid <= 0) then
  eMessage = 'Must set ZREL in scenario input'
  go to 9998
end if

if (reltyp(1:1) == 'C') then

  if (tdur <= 0.) then
    eMessage = 'Must set TDUR for RELTYP=C in scenario input'
    go to 9998
  end if

  if (wrel < 0.0 .and. tdur*wrel < -zrel) then
    eMessage = 'End point of moving source trajectory below ground'
    go to 9998
  end if

end if

if (name_rel == ' ') then

  if (cmass <= 0.) then
    eMessage = 'Must set CMASS in scenario input'
    go to 9998
  end if

  if (zrel < 0. .and. tdur /= DEF_VAL_R) then
    eMessage = 'Must set ZREL >= 0 in scenario input'
    go to 9998
  end if

  if (size_rel <= 0.) then
    if (reltyp(1:1) == 'I') then
      if (sigx <= 0. .or. sigy <= 0. .or. sigz <= 0.) then
        eMessage = 'Must set SIZE or SIGX, SIGY, and SIGZ in'// &
                             ' scenario input'
        go to 9998
        end if
      if ((sigx >= DEF_VAL_R .or. sigy >= DEF_VAL_R .or. &
                         sigz >= DEF_VAL_R) .and. opid <= 0) then
        eMessage = 'Must set SIZE or SIGX, SIGY, and SIGZ in'// &
                             ' scenario input'
        go to 9998
        end if
    else
      if(tdur /= DEF_VAL_R)then
        if (sigy <= 0. ) then
          eMessage = 'Must set SIZE or SIGY for continuous release'
          go to 9998
           end if
        if ((sigy >= DEF_VAL_R .or. sigz >= DEF_VAL_R) .and. &
                opid <= 0) then
          eMessage = 'Must set SIZE or SIGY and SIGZ in scenario'// &
                                                            ' input'
          go to 9998
        end if
      end if
    end if
  else
    if (sigx > 0. .or. sigy > 0. .or. sigz > 0.) then
      eMessage = 'Cannot set SIZE and SIGX, SIGY or SIGZ in'// &
                             ' scenario input'
      go to 9998
    end if
    sigx = size_rel
    sigy = size_rel
    sigz = size_rel
  end if

  ityp = 0
  call cupper(relmat)

  do i = 1,nmat
    if(relmat == mat(i)%cmat)ityp = i
  end do

  if(ityp == 0)then
    eMessage = 'Unknown release material in scenario'
    go to 9998
  else
    if(IsParticle(mat(ityp)%icls))then
      nsg = sub_groups(mat(ityp),mataux)
      rel_dist = subgroup - nsg
      if (rel_dist > 0) then
        ctyp = float(mat(ityp)%ioffp + 1)
      else
        ctyp = float(mat(ityp)%ioffp + subgroup)
      end if
    else
      rel_dist = 0
      ctyp = float(mat(ityp)%ioffp + subgroup)
    end if
    if (rel_dist > 1 .or. subgroup <= 0) then
      eMessage = 'Non-existent subgroup for material '//TRIM(relmat)
      go to 9998
    end if
  end if

end if

eAction = char(0)

9999    continue

return

!------ set read errors and go to return

9998    continue
nError   = RD_ERROR
eRoutine = 'check_scn'
eInform  = 'File='//TRIM(file_scn)
go to 9999

end


subroutine update_scn
!*******************************************************************************
!
! FUNCTION:   Update the scenario
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          update_release               check_lon                 IsReady
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use files_inc

implicit none
 
! --- LOCALS

logical IsReady

if ( IsReady(opid,opmod(OPREADY_INDEX)) )then
!           opid = 0
else
  if(opid <= 0)then
    eMessage = 'opReady=F only valid for opID > 0'
    go to 9998
  end if
  call update_release(opid,0,0)
  if(nError /= NO_ERROR)go to 9999
  if (lmap == I_LATLON) then
    call check_lon(xrel,xmin,xmax)
    if (nError /= NO_ERROR) then
      eInform = 'Setting source location'
      go to 9999
    end if
  end if
end if

9999    continue

return

!------ set read errors and go to return

9998    continue
nError   = RD_ERROR
eRoutine = 'update_scn'
eInform  = 'File='//TRIM(file_scn)
go to 9999

      end


integer function time_level(dtp)
!*******************************************************************************
!
! FUNCTION:  Find time level through number of doublings
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
 
real dtp    !Time step

! --- LOCALS

real    del
integer ilev


del = dtp
ilev  = 0
do while (del < delt .and. ilev <= MAXTLV)
  ilev = ilev + 1
  del  = del + del
end do

if(ilev > MAXTLV)then
  nError   = SZ_ERROR
  eRoutine = 'time_level'
  del      = dtp * (2.0**MAXTLV)
  eMessage = 'Puff timestep too small'
  write(eInform,'(a,1p,e9.1,a)') &
              'Maximum Timestep must be less than ',del,' sec'
  if(del/delt > .001)then
    eAction='Reduce Maximum Timestep in the Advanced TIME editor'
  else
    eAction='Please submit a problem report about this project'
  end if
  go to 9999
end if

9999    continue

time_level = min0(ilev,MAXTLV)

return
end
