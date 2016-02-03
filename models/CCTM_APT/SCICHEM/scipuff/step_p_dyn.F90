!*******************************************************************************
!$RCSfile: step_p_dyn.F90,v $
!$Revision: 1.27 $
!$Date: 2010/12/07 20:12:34 $
!
! 01/31/2007 : a) Use constants from constants_fd 
!              b) Updates for multiprocessor code 
!                 -BC
! Updated for CMAQ 5.0 final, March 2012, PK, ENVIRON
! Stability fixes, Feb 2015, PK, ENVIRON
!*******************************************************************************
subroutine step_p(dt,p,ipuf,lev1,lev2,nchg,fac_srf,fac_diag,cgrid)
!******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:  This subroutine provides the ode's to be integrated from
!            the source for a dynamic 3d-puff, p.
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!       get_puff_material            get_dynamics             get_totalcc
!                  get_mc               step_init               reset_lsv
!            turb_rescale                 get_dep               step_turb
!                get_wash                step_dep              step_pturb
!            step_velcorr                set_diff                step_sig
!              step_scale           step_dynamics                 get_amb
!               dump_puff               get_topog                  get_zi
!               get_gamma         set_penetration             set_descend
!             ground_refl          dense_rot_norm               step_tlev
!            put_dynamics             put_totalcc              step_split
!                IsHazard                 IsMulti                   IsGas
!            dense_effect
!
! REVISION HISTORY: 
!
! 29 NOV 2000 : Limit vertical drift velocity in capped BL - DSH
! 01/31/2007  : Move call of step_mc to step for multiprocessor code -BC 
! August 2007 : Updates for multiprocessor code - BC
! August 2010 : 
!              a) Update for changes made by PKK, AER for Oct. 2004 CMAQ release
!                 -BC(SAGE-MGT)
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use files_inc
use step_p_inc
use multcomp_mc_inc

#ifdef parallel
use common_mpi
#endif

use interface_definitions, only: get_amb

implicit none

! --- ARGUMENTS
 
real dt              !Time step (sec)
type ( puff_str ) p  !Puff structure
integer ipuf         !Puff number
integer lev1, lev2   !Time levels stepped (between lev1 and lev2)
integer nchg         !Number of time level changes
real fac_srf         !Factor for surface integrals
real fac_diag        !Factor set to determine if step is included in diagnostics
REAL :: CGRID( :,:,:,: )  !3-D ambient species concentrations (for chemistry runs)

! --- LOCALS

type ( puff_material ) pmatl
type ( puff_dynamics ) pd
type ( puff_totalcc  ) pt
type ( puff_mc       ) pm   ! -- multicomponent structure

real znew, damp, prod, sx, sy, zp
real    dp, dum1, dum2
real    carea
integer i, j, ij, iz, ios, isec
integer icls, jtyp
logical IsGas, IsHazard, IsMulti
logical dense_effect

#ifdef parallel
if (myid == 0) then
#endif
!-----  Get material and size group

  jtyp = p%ityp
  icls = typeID(jtyp)%icls
  imat = typeID(jtyp)%imat
  call get_puff_material(jtyp,material,mat_aux,pmatl)

  hazflag = IsHazard(icls)

!-----  get dynamic puff parameters

  if(dynamic)then
    call get_dynamics(p,pd)
  end if

!-----  get total cc

  if (typeID(jtyp)%ltot) then
    call get_totalcc(p,pt)
  end if

!-----  Initialize met fields, terrain, and puff variables

  call step_init(p)
  if (nError /= NO_ERROR) go to 9999

  if (lsv_oper) call reset_lsv(si)

!------ scale turbulence for time-averaging

  qqfl  = 1.
  qqfb  = 1.
  qqfsh = 1.
  if (.not.hazflag .and. t_avg /= DEF_VAL_R) then
    call turb_rescale(si,sv,vel2)
  end if

!-----  set surface deposition and flag for calculating time scale

  lsrf   = material(imat)%lsrfg .or. material(imat)%lsrft
  ldos   = material(imat)%ldosg .or. material(imat)%ldost
  ltot   = material(imat)%lsrft .or. material(imat)%ldost
  lscale = lsrf .or. ldos .or. lsmp

!------ set deposition/settling rate

  call get_dep(p,pmatl,dt)

!-----  define background turbulence parameters

  call step_turb

!------ calculate washout

  if (lwash) then
    call get_wash(p,dt)
    if (nError /= NO_ERROR) go to 9999
  else
    fwash = 0.
  end if

!-----  advance mass

  p%c = p%c / (1.0 + tauc*dt)

!-----  activity decay

  if (ldecay) p%cfo = p%cfo * exp(-decay_rate(imat)*dt)

!-----  set surface deposition

  if (lsrf.or.ldos) then
    call step_dep(p,pt,ipuf,dt,fac_srf)
    if (nError /= NO_ERROR) go to 9999
  end if

  if(dynamic)then
    call step_pturb(p,pd,dt)
  else
    fdyn = 1.
    tauw = 0.
    wdyn = 0.
    udyn = 0.
    vdyn = 0.
    ddyn = 0.
    difp = 0.
  end if

!-----  advance velocity correlations

  call step_velcorr(p,dt)

!------ set diffusivities

  call set_diff(p)

!-----  advance CC

  prod = qosi_cc + tauc + sigvd/sv
  damp = exp(-prod*dt)
  prod = qosi_cc*p%ccb
  p%cc = (p%cc + prod*dt) * damp

  if (typeID(p%ityp)%ltot ) then
    prod   = qosi_cc + tauc + min(1.,p%ccb/pt%cctb)*sigvd/sv
    damp   = exp(-prod*dt)
    prod   = qosi_cc*pt%cctb
    pt%cct = (pt%cct + prod*dt) * damp
  end if

!-----  advance spatial moments

  call step_sig(p,dt,ipuf)

!-----  advance turbulence scales

  call step_scale(p,dt)

!-----  advance puff dynamics

  if(dynamic)then
    call step_dynamics(p,pd,dt,pmatl)
  end if

!-----  advance multi-components

  dtmc = delt+delt
  if(IsMulti(typeID(p%ityp)%icls))then

    iz = nzb

    !-----  get multi-components

    call get_mc(p,pm)

    carea = p%sr/csav

    call set_ps_from_mc(pm,carea,area_fac)
    if (nError /= NO_ERROR) go to 9999

    !====   set ambient concentrations

    if (lamb3d) call get_amb(p%xbar, p%ybar, p%zbar, t, cgrid)

    !====   Find layer no. corresponding to puff height
    if (lter) then
      call get_topog(p%xbar,p%ybar,hp,dum1,dum2)
      dp = 1. - hp/zbtop
      zp = (p%zbar - hp)/dp
    else
      zp = p%zbar
    end if

    zp = MIN(zb(nzb),MAX(0.,zp))

    do iz = 1,nzb
      if (zp <= zbw(iz)) exit
    end do
    iz = min(iz,nzb)

    call update_k( .false. ,p%xbar, p%ybar, p%zbar)
    if (nError /= NO_ERROR) go to 9999

    ! == location of puff

    i = min(nxb,max(1,nint((p%xbar-xmin)/dxb) + 1))
    j = min(nyb,max(1,nint((p%ybar-ymin)/dyb) + 1))
    ij = (j-1)*nxb + i

!debug
!    write(*,*)'x,y,i,j,ij,zp,iz: ',p%xbar,p%ybar,i,j,ij,zp,iz
!    write(*,*)'cldbot,cldtop: ',cldbot,cldtop
!debug
    StepMCdat(nspuf)%p          = p
    StepMCdat(nspuf)%ps(1:MAX_MC) = ps(1:MAX_MC)
    StepMCdat(nspuf)%ij         = ij
    StepMCdat(nspuf)%ipuf       = ipuf
    StepMCdat(nspuf)%istage     = istage
    StepMCdat(nspuf)%ngd        = 0
    StepMCdat(nspuf)%nbd        = 0
    StepMCdat(nspuf)%t          = t
    StepMCdat(nspuf)%dt         = dt
    StepMCdat(nspuf)%dtmc       = dtmc  
    StepMCdat(nspuf)%csav       = csav
    StepMCdat(nspuf)%vol        = vol
    StepMCdat(nspuf)%fac_diag   = fac_diag
    StepMCdat(nspuf)%zk         = zk
    StepMCdat(nspuf)%tab        = tab
    StepMCdat(nspuf)%pb         = pb
    StepMCdat(nspuf)%hb         = hb
    StepMCdat(nspuf)%cldall     = cldall
    StepMCdat(nspuf)%cldallt    = cldallt
    StepMCdat(nspuf)%cldallp    = cldallp
    StepMCdat(nspuf)%cmassp     = cmassp
    StepMCdat(nspuf)%cmasst     = cmasst
    StepMCdat(nspuf)%pratebl    = pratebl
    StepMCdat(nspuf)%fcc        = fcc
    StepMCdat(nspuf)%fprcpc     = fprcpc
    StepMCdat(nspuf)%radyn      = radyn
    StepMCdat(nspuf)%us2        = us2
    StepMCdat(nspuf)%ws2        = ws2  
    StepMCdat(nspuf)%lflag_dark = lflag_dark
    StepMCdat(nspuf)%kamb       = kamb  
    StepMCdat(nspuf)%corr       = corr  
    StepMCdat(nspuf)%ddepos(1:MAX_MC) = 0.
    StepMCdat(nspuf)%wdepos(1:MAX_MC) = 0.
    StepMCdat(nspuf)%chem(1:MAX_MC)   = 0.
    Splitdat(nspuf)%frac        = 0.

  end if

!-----  advance centroid

  p%xbar = p%xbar + (1.5*ub-0.5*p%uo+udyn)*dt*xmap
  p%ybar = p%ybar + (1.5*vb-0.5*p%vo+vdyn)*dt*ymap

  if (lter) then
    call get_topog(p%xbar,p%ybar,hp,hx,hy)
  else
    hp = 0.
  end if

!------ limit drift velocity to prevent exiting BL

  wpuff = p%wc/p%c
  if (lzinv) wpuff = amin1(wpuff,(zinv-zsav)/dt)

  wpuff = wpuff + wdyn + 1.5*wb - 0.5*p%wo - vfall
  znew  = p%zbar + wpuff*dt
  znew  = max(znew , hp + 0.01*sz)

!------ get new inversion height; check for capped boundary layer

  call get_zi( p%xbar , p%ybar , xmap , ymap )

  if (dtdzs <= 0.) then
    call get_gamma(p%xbar,p%ybar,zinv,gamma,0)
    lblcap = gamma > 0.
  else
    lblcap = .false.
  end if

!-----  cap previously uncapped puff if within convective BL

  if( p%zc == 0. .and. lblcap .and. p%zbar < zinv)then
    p%zc = p%zbar + 2.*sz
  end if

!------- check inversion penetration / descending (only in unstable BL)

  if (dynamic .and. lblcap) then

    if (lzinv .and. znew > zinv .and. wdyn > 0.) then

!-------- inversion penetration

      call set_penetration(p,pd,dt,znew)

    else if (.not.lzinv .and. lblcap .and. znew < zinv) then

!-------- descending from stable region

      call set_descend(p,pd,znew)

    else

      p%zbar = znew

    end if

  else

    p%zbar = znew

  end if

!------ prevent puffs from penetrating the ground

  if (lter) then
    call get_topog(p%xbar,p%ybar,hp,hx,hy)
    sx    = sqrt(p%sxx)
    sy    = sqrt(p%syy)
    zp    = p%zbar - hp
    ztest = zp - fac_rfl*(abs(hx)*sx + abs(hy)*sy)
    if (zp < 0.0) then
      write(lun_log,*) 'Ground_reflect: t = ',t
      write(lun_log,*) 'initial x,y,z,h: ',p%xbar,p%ybar,p%zbar,hp
      call ground_refl(p%xbar,p%ybar,p%zbar,hp,xsav,ysav,zsav,hsav &
                                               ,xmap,ymap)
      call get_topog(p%xbar,p%ybar,hp,hx,hy)
      if (p%zbar < hp) then
        p%zbar = hp + 0.1*sz
      end if
      write(lun_log,*) 'final x,y,z,h  : ',p%xbar,p%ybar,p%zbar,hp
      if (p%zbar > p%zc .and. p%zc > 0.) then
        p%zc = p%zc + p%zbar - zsav
      end if
      call get_zi( p%xbar , p%ybar , xmap , ymap )
    end if
  else
    p%zbar = abs(p%zbar)
    ztest  = p%zbar
  end if

!------ Adjust dense gas puff moments for change in terrain slope

  if (dense_gas .and. IsGas(icls)) then
    if (dense_effect(p%zbar-hp,p%sv,pd%wcp)) then
      call dense_rot_norm(hxsav,hysav,hx,hy,p)
    end if
  end if

!------ reflect puff off inversion for convective conditions if capped

!       if ((lblcap .and. p%zbar > zinv) .and. lcap) then
!         p%zbar = max( 2.*zinv - p%zbar , hp + sz)
!       end if

  p%zi = zinv

!------ reset time-level

  call step_tlev(p,dt,lev1,lev2,nchg)
  if (nError /= NO_ERROR) go to 9999

!------ save velocities for Adams-Bashforth scheme

  p%uo = ub
  p%vo = vb
  p%wo = wb

  if (dynamic) then
    call put_dynamics(p,pd)
  end if

!-----  put total cc

  if (typeID(p%ityp)%ltot ) then
    call put_totalcc(p,pt)
  end if

!------ if necessary, put tscale into p%sr

  if (lscale) then
    p%sr = tscale
  end if

!------ check for vertical splitting; set capping height

  if (p%c > cmin .and. fac_srf == 1.0) then
    fsplit = 0.
    nsplit = 0 
    call step_split(p,dt,lev1,lev2,nchg)
    if (nError /= NO_ERROR) go to 9999
    Splitdat(nspuf)%npuf = nsplit
    Splitdat(nspuf)%frac = fsplit
  else
    CALL set_zcap_static( p,dt )
  end if

#ifdef parallel
end if
#endif

9999    continue

return

end

subroutine ground_refl(x1,y1,z1,h1,x0,y0,z0,h0,xmap,ymap)
!*******************************************************************************
!
! FUNCTION:  Find location for puff reflection from the ground surface
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               intersect
!
! REVISION HISTORY: 
!
!*******************************************************************************

implicit none

! --- ARGUMENTS
 
real x1,y1,z1   !Puff centroid location
real h1         !Terrain height at puff location
real x0,y0,z0   !Original puff centroid
real h0         !Terrain height at original location
real xmap,ymap  !Map factors

! --- LOCALS

real xr1, xr2, xr3, hx, hy, h2, denr, xn1, xn2, xn3, del

!------ Find intersection with surface (approximate)

call intersect(x1,y1,z1,h1,x0,y0,z0,h0,xmap,ymap,xr1,xr2,xr3,hx,hy)

h2   = hx*hx + hy*hy
denr = 1./sqrt(1.+h2)

xn1 = -hx*denr
xn2 = -hy*denr
xn3 = denr

del = 2.*(xr1*xn1 + xr2*xn2 + xr3*xn3)

x1 = x1 + del*xn1*xmap
y1 = y1 + del*xn2*ymap
z1 = z1 + del*xn3

return
end

subroutine intersect(x1,y1,z1,h1,x0,y0,z0,h0,xmap,ymap,xr1,xr2,xr3,hx,hy)
!*******************************************************************************
!
! FUNCTION:  Find intersection with surface (approximate)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_topog
!
! REVISION HISTORY: 
!
!*******************************************************************************
implicit none

! --- ARGUMENTS
 
real x1,y1,z1      !Puff centroid location
real h1            !Terrain height at puff location
real x0,y0,z0      !Original puff centroid
real h0            !Terrain height at original location
real xmap, ymap    !Mapfactors
real xr1, xr2, xr3 !Intersection location
real hx, hy        !Terrain gradients

! --- PARAMETERS
 
integer, parameter :: MAX_ITER = 10

! --- LOCALS

real dx, dy, dz, dh, eps, xs, ys, zs, rat, hp

integer iter

!------ first approximation

dx = (x0-x1)/xmap
dy = (y0-y1)/ymap
dz = (z0-z1)

dh    = h0 - h1
rat   = (h1-z1)/(dz-dh)

eps  = 1.e-3*abs(dh)

iter = 0

!------ iterate to find intersection

1       continue

iter = iter + 1

xr1 = rat*dx
xr2 = rat*dy
xr3 = rat*dz

xs = x1 + xr1*xmap
ys = y1 + xr2*ymap
zs = z1 + xr3

call get_topog(xs,ys,hp,hx,hy)

xr3 = hp - z1

if (abs(hp-zs) <= eps) then
  return
end if

if (iter >= MAX_ITER) then
  return
else
  rat = (hp-z1 - hx*xr1 - hy*xr2)/(dz - hx*dx - hy*dy)
  go to 1
end if

end

subroutine set_sdep(sdat,c,rat,ratt,si,cmax)
!*******************************************************************************
!
! FUNCTION:  Save surface deposition data
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
 
real sdat(*)  !Surface depostion data array
real c        !Mean concentration
real rat      !Factor for group variance
real ratt     !Factor for total variance
real si       !Factor for timescale
real cmax     !Maximum concentration

sdat(ISRF_C  ) = c
sdat(ISRF_CC ) = c*rat
sdat(ISRF_CCT) = c*ratt
sdat(ISRF_SL ) = si
sdat(ISRF_C0 ) = cmax

return

end

subroutine chk_shr(s12,s21,sig11,sig22,dt)
!*******************************************************************************
!
! FUNCTION:  Set time-step based on the velocity shear
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
 
real s12, s21      !Shear terms (du1/dx2 and du2/dx1)
real sig11, sig22  !Second moments (spread)
real dt            !Current timestep

! --- PARAMETERS
 
real, parameter :: MIN_SHR = 1.e-10 !Minimum shear to affect timestep

if (abs(s12) >= MIN_SHR) then
  dt = min(dt,sqrt(0.25*sig11/sig22)/abs(s12))
end if
if (abs(s21) >= MIN_SHR) then
  dt = min(dt,sqrt(0.25*sig22/sig11)/abs(s21))
end if

return

end

subroutine turb_rescale(scl_i,scl_v,v2)
!*******************************************************************************
!
! FUNCTION:   Rescale background turbulence lengths
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              fscale_lsv
!
! REVISION HISTORY: 
!
! 01/31/2007 : Use constants from constants_fd -BC 
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
real scl_i   !horizontal cc dissipation length scale
real scl_v   !vertical   cc dissipation length scale
real v2      !square of the background velocity

! --- PARAMETERS
 
real, parameter :: ALPT = 0.03   ! Conditional avg. scale factor

! --- LOCALS

real fscale_lsv
real stb, stl, stbh, stbv, fact, soq, amet, bmet

stb  = ALPT * sqrt(v2 + 2.*uubl + 2.*vvbl + wwbl) * t_avg
stl  = ALPT * sqrt(v2 + uub  + vvb) * t_avg

stbh = max(scl_i,stb)
stbv = max(scl_v,stb)
stl  = max(scl_i,stl)

!----- Large-scale horizontal turbulence

if( stl < sby )then
  fact = ( stl/sby )**0.6666667
  fact = fact*fscale_lsv(stl,sby,sbl)
  uub  = uub * fact
  vvb  = vvb * fact
  uvb  = uvb * fact
  uubz = uubz * fact
  vvbz = vvbz * fact
  uvbz = uvbz * fact
  sby  = stl
  qqfl = (2. - fact)**2  !dissipation factor for large-scale turbulence
end if

!-----  Shear driven horizontal turbulence

if( stbh < sbls )then
  fact = ( stbh/sbls )**0.6666667
  uubl = uubl * fact
  wwbh = wwbh * fact
  sbls = stbh
  qqfsh = (2. - fact)**2  !dissipation factor for shear-driven turbulence
end if

!-----  Buoyancy driven horizontal turbulence

if( stbh < sbl )then
  fact = ( stbh/sbl )**0.6666667
  vvbl = vvbl * fact
  sbl  = stbh
  qqfb = (2. - fact)**2  !dissipation factor for buoyancy-driven turbulence
end if

!-----  Vertical turbulence

if( stbv < sbz )then
  fact = ( stbv/sbz )**0.6666667
  wwbl = wwbl * fact
  qqs  = qqs  * fact
  sbz  = stbv
  wtbl = wtbl * fact*fact
  soq  = sbz/max(sqrt(qqs),1.e-3)
  bmet = EQF*soq*soq
  amet = (wwbl*soq/a + bmet*wtbl)/(1.+2.*bmet*dtdz)
  dddz = amet*dddz/max(difb,1.e-6)
  difb = amet
end if

return
end

subroutine reset_lsv(si)
!*******************************************************************************
!
! FUNCTION:  Scale large-scale horizontal turbulence for operational LSV
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              fscale_lsv
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_met

implicit none

! --- ARGUMENTS
 
real si    !horizontal cc dissipation length scale

! --- LOCALS

real stl, fact, den, fscale_lsv

stl  = max(si,sbl)

if( stl < sb_lsv )then

  fact = ( stl/sb_lsv )**0.6666667
  fact = fact*fscale_lsv(stl,sb_lsv,sbl)
  uub  = uub - uu_haz
  vvb  = vvb - vv_haz
  uvb  = uvb - uv_haz
  uubz = uubz - uuz_haz
  vvbz = vvbz - vvz_haz
  uvbz = uvbz - uvz_haz

  uub  = uub * fact
  vvb  = vvb * fact
  uvb  = uvb * fact
  uubz = uubz * fact
  vvbz = vvbz * fact
  uvbz = uvbz * fact
  sby  = stl

  den  = uub + uu_haz + vvb + vv_haz
  if( den > 0.0 )then
    sby  = (sby*(uub+vvb) + sb_haz*(uu_haz+vv_haz)) / den
  end if
  uub  = uub + uu_haz
  vvb  = vvb + vv_haz
  uvb  = uvb + uv_haz
  uubz = uubz + uuz_haz
  vvbz = vvbz + vvz_haz
  uvbz = uvbz + uvz_haz

end if

return

end

function fscale_lsv(s,sl,sb)
!*******************************************************************************
!
! FUNCTION: 
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
 
real s, sl, sb

! --- LOCALS

real fscale_lsv

if (sl > sb) then
  fscale_lsv = min(1.,(s-sb)/min(sb,sl-sb))
  fscale_lsv = max(fscale_lsv,0.)
else
  fscale_lsv = 1.
end if

return
end

subroutine get_dep(p,pmatl,dt)
!*******************************************************************************
!
! FUNCTION:  Define deposition time scale for implicit step
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            set_gas_vdry           set_part_vdry                   IsGas
!              IsParticle
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use step_p_inc

implicit none

! --- ARGUMENTS
                              
type ( puff_str ) p           !Puff structure
type ( puff_material ) pmatl  !Puff material structure

real      dt                  !Time step

! --- LOCALS

integer jtyp, icls

logical IsGas, IsParticle

jtyp = p%ityp
icls = typeID(jtyp)%icls

if(IsGas(icls))then

!-----  GAS material

  vfall = 0.0
  sigvd = 0.0
  if (p%sr > 0.) then
    call set_gas_vdry(pmatl)
  else
    vdry = 0.
  end if

else if(IsParticle(icls))then

!-----  PARTICLE material

  call set_part_vdry(p,pmatl)

end if

!-----  Total deposition rate is gravitational plus dry

vdtot = vfall*area_fac + vdry

!-----  Define deposition time scale for implicit step

tauc  = p%sr*vdtot/csav

return
end

subroutine set_gas_vdry(pmatl)
!*******************************************************************************
!
! FUNCTION:  Set vdry for a gas material type
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
use step_p_inc

implicit none

! --- ARGUMENTS

type ( puff_material ) pmatl  !Puff material structure
 
vdry = pmatl%param(2)  ! vd

return
end

subroutine set_part_vdry(p,pmatl)
!*******************************************************************************
!
! FUNCTION:  Set vdry for a particle material type
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                vdep_dry                   ufall
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p              !Puff structure

type ( puff_material ) pmatl     !Puff material structure
 
! --- LOCALS

real rhoa, dm, pd, ufall

rhoa  = rhoair*(pb**0.715)*273.2/tb
rhod  = pmatl%param(1)  !rho
dm    = pmatl%param(3)  !dbar
vfall  = ufall(rhoa,rhod,rmuair,dm)
if ((zbar < 2.*sv+hp) .or. vfall == 0.0)then
  sigvd = 0.0
else
  sigvd = pmatl%param(5)*vfall/pmatl%param(4)   ! pmatl%sigvd*vfall/pmatl%vd
end if
if (p%sr > 0.) then
  pd  = pmatl%param(6)   !diff
  call vdep_dry(ustdep,h_cnp,zruf,vfall,dm,pd,vdry)
else
  vdry = 0.
end if

return
end

subroutine step_sig(p,dt,ipuf)
!*******************************************************************************
!
! FUNCTION:  Advance the puff second moments
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!          dense_rot_norm                 chk_shr            step_sym_shr
!             check_slope
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p   !Puff structure
real dt               !Time step (sec)
integer ipuf          !Puff number

! --- PARAMETERS
 
!real, parameter :: EPS_DET = 1.e-4     ! Relative limit for puff distortion
real, parameter :: EPS_DET = 3.e-2     ! Relative limit for puff distortion
real, parameter :: R2 = 1.414214       ! sqrt(2)
real, parameter :: FAC = 2.0
real, parameter :: FAC_LIM = 0.8660254 ! limit on centroid shift ( sqrt(3)/2 )
REAL, PARAMETER :: CC_MAX = 100.0

! --- LOCALS

real det_fac, sxx, sxy, sxz, syy, syz, szz
real smax, xfac, yfac, zfac, zwcs, tem
real del, rx, ry, sx, sy, dtx
real shx, shy, svv
real erfc, zp, hx0, hy0

logical check_slope

zwcs = zwct + sigvd*sz

!------ set local terrain slope for anisotropic diffusivity

sz  = sqrt(p%szz)
hx0 = 0.0
hy0 = 0.0

if (lter) then
  sx = sqrt(p%sxx)
  sy = sqrt(p%syy)
  zp = p%zbar - hp
  zp = zp - 3.0*(abs(hx)*sx + abs(hy)*sy)
  if (zp < 3.0*sz) then
    hx0 = hx
    hy0 = hy
  end if
end if

!-----  check for extreme puff distortion

det_fac = p%det/(p%sxx*p%syy*p%szz)

IF( det_fac < EPS_DET .OR. MIN(p%cc,CC_MAX*p%ccb) <= cmin2 )THEN !If distorted, only diffuse puffs

  call dense_rot_norm(hx0,hy0,0.,0.,p)

  if (p%zc > 0.0) then
    tem  = sqrt(p%szz)/WMX
    p%zc = max(p%zc,hp+tem)
  end if

  p%sxx = p%sxx + 2.*xuct*dt
  p%syy = p%syy + 2.*yvct*dt
  p%szz = p%szz + 2.*zwcs*dt
  p%sxy = p%sxy + 2.*xvct*dt 

  if (p%zc > 0.0) then
    tem   = WMX*(p%zc-hp)
    p%szz = min(p%szz,tem*tem)
  end if
  
  call dense_rot_norm(0.,0.,hx0,hy0,p)

  dts  = 1.e36

else

!-----  diagonal strain

  smax = max(abs(dudx),abs(dvdy),abs(dudx+dvdy),0.01/dt)
  dts  = 0.25/smax
  shx  = max(si*si,p%sxx)
  shy  = max(si*si,p%syy)
  svv  = max(sv*sv,p%szz)
  smax = 2.*max( xuct/shx,yvct/shy,zwcs/svv,0.01/dt)
  dts  = min(dts,1.0/smax)
  call chk_shr(dudy,dvdx,p%sxx,p%syy,dts)
  call chk_shr(dvdz,dwdy,p%syy,p%szz,dts)
  call chk_shr(dwdx,dudz,p%szz,p%sxx,dts)

  dtx = min(dt,dts)

  call dense_rot_norm(hx0,hy0,0.,0.,p)

  if (p%zc > 0.0) then
    tem  = sqrt(p%szz)/WMX
    p%zc = max(p%zc,hp+tem)
  end if

  p%sxx = p%sxx + 2.*xuct*dtx
  p%syy = p%syy + 2.*yvct*dtx
  p%szz = p%szz + 2.*zwcs*dtx
  p%sxy = p%sxy + 2.*xvct*dtx

  if (p%zc > 0.0) then
    tem   = WMX*(p%zc-hp)
    p%szz = min(p%szz,tem*tem)
  end if

  call dense_rot_norm(0.,0.,hx0,hy0,p)

  xfac = exp(dudx*dtx)
  yfac = exp(dvdy*dtx)
  zfac = 1.0/(xfac*yfac)

  sxx = p%sxx*xfac*xfac
  syy = p%syy*yfac*yfac
  szz = p%szz*zfac*zfac
  sxy = p%sxy/zfac
  sxz = p%sxz/yfac
  syz = p%syz/xfac

!-----  dudy,dvdx

  call step_sym_shr(dudy,dvdx,sxx,sxy,sxz,syy,syz,dtx)

!-----  dvdz, dwdy

  call step_sym_shr(dvdz,dwdy,syy,syz,sxy,szz,sxz,dtx)

!-----  dwdx, dudz

  call step_sym_shr(dwdx,dudz,szz,sxz,syz,sxx,sxy,dtx)

  p%sxx = sxx
  p%syy = syy
  p%szz = szz
  p%sxy = sxy
  p%sxz = sxz
  p%syz = syz

!------ reduce dynamic velocity if shear-limited

  wdyn = wdyn*dtx/dt

!------ limit vertical diffusion of "below ground" mass by moving centroid
!       and reducing horizontal spread

  sz = sqrt(p%szz)
  if (p%zbar-hp < 3.*sz) then
    if (.not.check_slope(hx,hy)) then
      sx     = sqrt(p%sxx)
      sy     = sqrt(p%syy)
      del    = min(max(p%sr*zwcs*dtx/(sz*p%c),0.),1.)
      del    = del*erfc((p%zbar-hp)/(R2*sz))
      rx     = min(max(p%sxz/(sx*sz),-1.),1.)
      ry     = min(max(p%syz/(sy*sz),-1.),1.)
      xfac   = min(max(FAC*del*rx,-FAC_LIM),FAC_LIM)
      yfac   = min(max(FAC*del*ry,-FAC_LIM),FAC_LIM)
      p%xbar = p%xbar + xfac*sx*xmap
      p%ybar = p%ybar + yfac*sy*ymap
      xfac   = 1. - xfac**2
      yfac   = 1. - yfac**2
      p%sxx  = p%sxx*xfac
      p%syy  = p%syy*yfac
      xfac   = sqrt(xfac)
      yfac   = sqrt(yfac)
      p%sxy  = p%sxy*xfac*yfac
      p%sxz  = p%sxz*xfac
      p%syz  = p%syz*yfac
    end if
  end if

end if

!-----  Change cap to contain the puff if necessary

if (p%zc > 0.0) p%zc = max(p%zc,hp+sz/WMX)

return

end

subroutine step_sym_shr(dudy,dvdx,sxx,sxy,sxz,syy,syz,dt)
!*******************************************************************************
!
! FUNCTION:  Account for velocity gradient term in the advancement
!            of the puff sigmas
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
 
real dudy, dvdx               !Velocity shear
real sxx, sxy, sxz, syy, syz  !Puff second moments
real dt                       !Time step

! --- LOCALS

real txx, txy, tyy, txz, tyz
real xdudy, xdvdx, fac, fac2

xdudy = dudy*dt
xdvdx = dvdx*dt

fac2  = 1.0/(1.0-xdudy*xdvdx)
fac   = sqrt(fac2)

txx   = xdudy*(sxy + sxy + xdudy*syy)
tyy   = xdvdx*(sxy + sxy + xdvdx*sxx)
txy   = xdudy*syy + xdvdx*sxx + xdudy*xdvdx*sxy
txz   = xdudy*syz
tyz   = xdvdx*sxz

sxx   = (sxx + txx)*fac2
syy   = (syy + tyy)*fac2
sxy   = (sxy + txy)*fac2
sxz   = (sxz + txz)*fac
syz   = (syz + tyz)*fac

return
end

subroutine step_init(p)
!*******************************************************************************
!
! FUNCTION:  Initialize the met fields, save local puf variables
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  mapfac               get_topog                 get_met
!               get_gamma
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p  !Puff structure

!------ set min mass factor

cmin2 = 2.*material(imat)%prop(3)*p%c

!-----  set map factors

call mapfac( p%xbar , p%ybar , xmap , ymap )

!-----  set terrain factors (height, slopes, and area fac)

if (lter) then
  call get_topog(p%xbar,p%ybar,hp,hx,hy)
  area_fac = 1./sqrt(1.+hx*hx+hy*hy)
else
  area_fac = 1.
  hp       = 0.
  hx       = 0.
  hy       = 0.
end if

!----- save puff variables at start of step

csav  = p%c
xsav  = p%xbar
ysav  = p%ybar
zsav  = p%zbar
hsav  = hp
hxsav = hx
hysav = hy
zisav = p%zi

!-----  define local puff variables

si   = p%si
si2  = p%si2
sv   = p%sv
zbar = p%zbar
sz   = sqrt(p%szz)

!----- calculate met-field values

call get_met(p%xbar,p%ybar,p%zbar,p%szz,p%zc,xmap,ymap,0,hazflag)

!-----  modified gravity factor, and Brunt_Vaisala frequency

dtdz = max( dtdz , 0.0 )
gt0  = G0/tb

!-----  save puff inversion and set flags

p%zi  = zinv
lzinv = (p%zbar <= zinv) .and. lbl

if (lzinv .and. dtdzs <= 0.) then
  call get_gamma(p%xbar,p%ybar,zinv,gamma,0)
  lcap = gamma > 0.
else
  lcap = .false.
end if

!-----  cap previously uncapped puff if within convective BL

if( p%zc == 0. .and. lcap )then
  p%zc = p%zbar + 2.*sz
end if

!-----  set Brunt-Vaisala frequency to damp dynamics - use overlying
!       stability for BL puffs near the inversion

if (lcap .and. zbar+sv > zinv) then
  bv = sqrt(gt0*max(gamma,dtdz))
else
  bv = sqrt(gt0*dtdz)
end if

!-----  velocity-squared for later computations

vel2  = ub*ub + vb*vb

return
end

subroutine step_dep(p,pt,ipuf,dt,fac_srf)
!*******************************************************************************
!
! FUNCTION:  Update the surface integrated fields - dose and deposition
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  settle                set_sdep            surface_dose
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_srf
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p        !Puff structure
type ( puff_totalcc ) pt   !Total CC structure

integer ipuf               !Puff number
real    dt                 !Time step
real    fac_srf            !Surface integral factor

! --- LOCALS

real vx, crat, cratt, rat, ratt, ratx, rattx
real cx, cdos, sdat(5)

logical dosflg, depflg

depflg = surface .and. (p%sr>0.0)

if(dose)then
if (z_dosage > 0.0) then
  call settle(ipuf,z_dosage,cdos,fac_rfl)
else
  cdos = p%sr
end if
  dosflg = cdos > 0.0
else
  dosflg = .false.
end if

if (.not.(dosflg .or. depflg .or. lwash)) go to 9999

crat = max((p%cc-p%ccb),0.01*p%ccb)/csav
rat  = tscale*crat
if (ltot) then
  cratt = max((pt%cct-pt%cctb),0.01*pt%cctb)/csav
  ratt  = tscale*cratt
else
  ratt = 0.
end if

!----- Surface mass deposition integral

if (depflg) then

  cx    = (1. - fwash)*(csav - p%c)*fac_srf
  ratx  = vdtot*rat
  rattx = vdtot*ratt
  call set_sdep(sdat,cx,ratx,rattx,p%si,p%cc/p%c)
  call surface_dose(ipuf,sdat,srfdep,0.0,1)
  if (nError /= NO_ERROR) go to 9999

end if

!----- Surface inhalation dose integral

if (dosflg) then

  cdos = cdos*dt*fac_srf
  call set_sdep(sdat,cdos,rat,ratt,p%si,p%cc/p%c)
  call surface_dose(ipuf,sdat,srfdos,z_dosage,0)
  if (nError /= NO_ERROR) go to 9999

end if

!-----  Surface washout mass deposition integral

if (fwash > 0.0) then

  cx    = fwash*(csav - p%c)*fac_srf
  vx    = taur*p%sv
  ratx  = vx*rat
  rattx = vx*ratt
  call set_sdep(sdat,cx,ratx,rattx,p%si,p%cc/p%c)
  call surface_dose(npuf+1,sdat,srfdep,0.0,1)
  if (nError /= NO_ERROR) go to 9999

end if

9999    continue

return
end

subroutine step_turb
!*******************************************************************************
!
! FUNCTION:  Define background turbulence parameters (variances and scales)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              set_hscale                  set_qi              set_tscale
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- LOCALS

real qi, qsh, qb, ql, qvx, qvi
real velc

!-----  define effective horizontal length scale

call set_hscale(uubt,vvbt,uvbt,sly)

!-----  velocity scale

qs  = sqrt(qqs)
qsh = sqrt(qqsh)
qb  = sqrt(qqb)
ql  = sqrt(qql)

!-----  calculate horizontal and vertical time scales

aqsosyt = a*qsh/sbls
aqbosyt = a*qb/sbl
aqlosyt = a*ql/sby
aqoszt  = a*qs/sbz

!-----  internal horizontal plume time and velocity scales

call set_qi(qi)

!-----  internal vertical plume time and velocity scales

if (lzinv .and. sv >= WMX*(zinv-hp))then
  qvi   = 0.
  svq   = 0.
  sigvd = 0.
else
  if (sv <= sbz) then
    qvi = ((sv/sbz)**0.3333333) * qs
  else
    qvi = (sbz/sv) * qs
  end if
  qvx = difb/bs/sv
  if (qvi > qvx) then
    qvi = qvx
    svq = difb/sv
  else
    svq = csi1*qvi
  end if
  qvi = qvi * sqrt(qqfsh)
end if

!-----  internal fluctuation damping time scale

qosi_cc = bs*(qi/si+qi/si2+qvi/sv)

!----- set time and space scales

if (lscale) then
  velc = sqrt(vel2 + qqsh + qqb + qql)
  call set_tscale(si,si2,sly,velc,tscale)
end if

return
end

subroutine set_qi(qi)
!*******************************************************************************
!
! FUNCTION: Define horizontal cc dissipation velocity scale
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              fscale_lsv              csi_interp
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
real qi    !horizontal cc dissipation velocity scale

! --- LOCALS

real uubi, fscale_lsv, csix, csi_interp
real checkl, checkb, checks, sl_outer

if (qql > 0.0) then
  if (si <= sby) then
    qi   = ((si/sby)**0.6666667) * qql
    qi   = qi*fscale_lsv(si,sby,sbl)
    siq  = (csi1**2) * qi
  else
    qi   = ((sby/si)**2) * qql
    qi   = qi*fscale_lsv(si,sby,sbl)
    siq  = (csi2**2) * qi
  end if
  qi = qi * qqfl
else
  qi  = 0.0
  siq = 0.0
end if

checkl = qql*sby*sby
checkb = qqb*sbl*sbl
checks = qqsh*sbls*sbls
if (checkl > checkb) then      ! L greater than B
  if (checkl > checks) then    ! L greater than S
    sl_outer = sby
  else                         ! S greater than L
    sl_outer = sbls
  end if
else                           ! B greater than L
  if (checkb > checks) then    ! B greater than S
    sl_outer = sbl
  else                         ! S greater than B
    sl_outer = sbls
  end if
end if

if (qqb > 0.0) then
  if (si <= sbl) then
!   cfac = 0.375 * (sqrt(a*a + 5.333333*vvbl/qqb) - a)
    uubi = ((si/sbl)**0.6666667) * qqb
    siq  = siq + (csi1**2) * uubi
  else
!   cfac = vvbl/qqb/a
    uubi = ((sbl/si)**2) * qqb
!   siq  = siq + (csi2**2) * uubi
    csix = csi_interp(si,sbl,sl_outer)
    siq  = siq + (csix**2) * uubi
  end if
  qi = qi + uubi*qqfb
end if

if (qqsh > 0.0) then
  if (si <= sbls) then
    uubi = ((si/sbls)**0.6666667) * qqsh
    siq  = siq + (csi1**2) * uubi
  else
    uubi = ((sbls/si)**2) * qqsh
!   siq  = siq + (csi2**2) * uubi
    csix = csi_interp(si,sbls,sl_outer)
    siq  = siq + (csix**2) * uubi
  end if
  qi = qi + uubi*qqfsh
end if

qi  = sqrt(qi)
siq = sqrt(siq)

return

end

real function csi_interp(si,sly,sl_outer)
!*******************************************************************************
!
! FUNCTION: 
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
 
real si, sly, sl_outer

! --- LOCALS

real sl_test

sl_test = min(1.5*sly,sl_outer)

if (si > sl_test) then
  csi_interp = csi2
else
  csi_interp = csi1+(csi2-csi1)*(si-sly)/(sl_test-sly)
end if

return
end

subroutine set_tscale(si,si2,sly,velc,tscale)
!*******************************************************************************
!
! FUNCTION:  Set puff time scale
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
 
real si, si2    !Internal puff length scale and length scale squared
real sly        !Background length scale
real velc       !Total background velocity scale
real tscale     !Puff time scale

! --- PARAMETERS
 
real, parameter :: ALG2 = 0.6931472   ! Log(2.)

! --- LOCALS

real fac, sig, arg

if(si >= sly)then
  sig = si
else
  arg = 1. + (sly*sly/si/si2)**0.333333
  sig = (sly*si*si2)**0.333333*alog(arg)/ALG2
!-- limit for short duration integrals
  fac = 2.5 - 1.5*min(1.0,(si2/sly)**2)
  sig = min(sig,fac*si2)
end if

tscale = 2.*0.7*sig/(velc)

return
end

subroutine step_pturb(p,pd,dt)
!*******************************************************************************
!
! FUNCTION:  Define dynamic puff turbulence parameters (variances and scales)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!        step_pturb_dense         step_pturb_buoy            dense_effect
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p         !Puff structure
type ( puff_dynamics ) pd   !Puff dynamics structure
real dt                     !Time step (sec)

! --- LOCALS

real qqp, slp, qpi, qpdiff, qosi_p

logical dense_effect

!----- Dynamic temperature excess

tdyn = pd%ctp/csav
bdyn = tdyn - tb*pd%bcp/csav

if (dense_effect(zbar-hp,sv,pd%wcp)) then
  ldense = .true.
  call step_pturb_dense(p,pd,qqp,slp,dt)
else
  ldense = .false.
  call step_pturb_buoy(p,pd,qqp,slp)
end if

!----- Return if no dynamic effects

if (wdyn == 0.0) then
  difp = 0.0
  return
end if

qpi    = fdyn*sqrt(qqp)

qpdiff = 0.15*qpi
difp   = qpdiff*slp
qosi_p = 2.*bs*(qpi/slp)*(1.0 + slp/si + slp/si2)

!----- Adjust internal velocity scales

siq = siq + qpdiff
svq = fdyn*svq + qpdiff

qosi_cc = qosi_cc + qosi_p

return
end

subroutine step_pturb_buoy(p,pd,qqp,slp)
!*******************************************************************************
!
! FUNCTION: Define buoyant dynamic puff turbulence parameters 
!           (variances and scales)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
! 15 OCT 2002 : Damp turbulence under stable conditions - RIS
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS

type ( puff_str ) p        !Puff structure
type ( puff_dynamics ) pd  !Puff dynamics structure
real qqp, slp              !Total background velocity and length scale
                           !(includes buoyancy induced entrainment effects)

! --- LOCALS

real vp2, wp2, qqvrtx, fac, ri

udyn = 0.
vdyn = 0.
wdyn = pd%wcp/csav
ddyn = 0.

!----- Dynamic turbulence

slp = min(si,0.65*max(zbar-hp,sz))

wp2    = wdyn*wdyn
if(abs(wp2) > 1.0e-20)then
  vp2    = vel2 + uub + vvb + 1.e-6
  qqvrtx = cvrtx*vp2*wp2/(vp2+wp2)

  if (abs(bdyn) > dtdz*slp) then
    ri  = gt0*abs(bdyn)*slp
    if (wp2 < 4.*ri ) then
      fac = 2.0
    else
      ri  = ri/wp2
      fac = 1.0 + 4.*ri
    end if
  else
    ri  = gt0*(dtdz*slp-abs(bdyn))*slp
    if ( wp2 < 1.0e-3*ri) then
      fac = 0.0
    else
      ri  = ri/wp2
      fac = 1.0/(1.0 + 4.*ri )
    end if
  end if

  qqp = fac*(cqb*wp2 + qqvrtx)
else
  qqp = 0.0
end if

tauw = 0.0
fdyn = 1.0

return
end

subroutine step_pturb_dense(p,pd,qqp,slp,dt)
!*******************************************************************************
!
! FUNCTION:  Define dense dynamic puff turbulence parameters 
!           (variances and scales)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               dense_rot             check_slope
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p        !Puff structure
type ( puff_dynamics ) pd  !Puff dynamics structure
real qqp, slp, dt          !Total background velocity and length scale
                           !(includes dense-gas effects)

! --- PARAMETERS

real, parameter :: FAC_DENSE = 20.0

! --- LOCALS

real vp2, wp2, fac, ri, alp

logical check_slope

alp = p%si2/(p%si+p%si2)
fac =(p%ccb/max(p%ccb,p%cc))**alp

if (check_slope(hx,hy)) then
  call dense_rot(pd,fac)
else
  dudx = dudx + pd%dudx*fac
  dudy = dudy + pd%dudy*fac
  dvdx = dvdx + pd%dvdx*fac
  dvdy = dvdy + pd%dvdy*fac
end if

ddyn = -(pd%dudx + pd%dvdy)
udyn = pd%u + pd%wcp*hx/(1.0+hx*hx)/csav
vdyn = pd%v + pd%wcp*hy/(1.0+hy*hy)/csav
wdyn = ddyn*(zbar-hp)   ! + udyn*hx + vdyn*hy

!----- Dynamic turbulence

vp2  = ub**2 + vb**2 + uub + vvb + 1.e-6
wp2  = udyn*udyn + vdyn*vdyn + (p%sxx+p%syy)*ddyn*ddyn + wdyn*wdyn
ri   = gt0*abs(bdyn)*sv/(4.*wp2+vp2)
fdyn = 1.0/(1.0 + FAC_DENSE*ri)
qqp = cqb*wp2
slp = sv

!----- Reduce velocity

fac = 0.7 + 0.3*fdyn
ub  = ub*fac
vb  = vb*fac
dudz = fdyn*dudz
dvdz = fdyn*dvdz

vel2  = ub*ub + vb*vb

!----- Surface exchange rate

fac  = alog(1.0 + slp/zruf)
tauw = (vonk/fac)**2*sqrt(vel2+uub+vvb+wp2)/slp

return
end

subroutine step_tlev(p,dt,lev1,lev2,nchg)
!*******************************************************************************
!
! FUNCTION: Reset time step for puff-p
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               dump_puff              time_level
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use common_srf
use step_p_inc
use files_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p    !Puff structure
real    dt             !Time step
integer lev1, lev2     !Time levels being stepped (from lev1 to lev2)
integer nchg           !Number of time level changes

! --- LOCALS

integer   il, time_level
real      delg, sigu, wtot
real      dtw, dtcc, dta, dtk, dtp, dsrf  ! dd

logical ltest

!------ promote low-mass puff to largest available timestep

if (p%cc <= cmin2) then
  if (p%idtl > lev1) nchg = nchg + 1
  p%idtl = lev1
  go to 9999
end if

!----- fluctuation dissipation time scale

if (qosi_cc > 0.0) then
  dtcc = 0.25/qosi_cc
else
  dtcc = delt
end if

!----- vertical velocity time scale

wtot = abs(wpuff)
if (wtot > 1.0e-10) then
  dtw = dzg/wtot
else
  dtw = delt
end if

!----- surface grid time scale

if (vel2 > 0.0) then
  delg = min(min(dxb,dxbl)/xmap,min(dyb,dybl)/ymap)
  if (lsrf .or. ldos) then
!  if (lscale) then
!    if (lsmp) then
!      ltest = .true.
!      dsrf  = 0.
!    else
      ztest = ztest-fac_rfl*sqrt(p%szz)
      ltest = ztest+min(wpuff,0.)*dt < 0.
      if (ltest) then
        if(lsrf)then
          dsrf = srfdep%delmin
        else
          dsrf = 0.
        end if
        if(ldos)then
          dsrf = max(srfdos%delmin,dsrf)
        end if
      end if
!    end if
    if (ltest) then
      sigu = ub*ub*p%axx + 2.*ub*vb*p%axy + vb*vb*p%ayy
      dta  = sqrt(max(1./sigu,dsrf*dsrf/vel2))
!             if (ztest > 0.) then
!               dta = max(dta,ztest/abs(wpuff))
!             end if
    else
      dta = delg/sqrt(vel2)
    end if
  else
    dta = delg/sqrt(vel2)
  end if
else
  dta = delt
end if

!----- vertical diffusivity time scale

!       if (p%zbar < zinv) then
!         dd = 0.25*(WMX*(zinv-hp))**2
!         dtk = 0.5*dd/max(zwct,1.e-6)
!       else
  dtk = delt
!       end if

!----- find minimum time step

dtp = min(dtcc, dtw, dta, dtk, dts, dtmc)

il  = time_level(dtp)

if(nError /= NO_ERROR) then
  eRoutine = 'step_tlev'
  write(lun_log,*,err=9998)'******* TIME LEVEL ERROR ********'
  write(lun_log,*,err=9998)TRIM(eRoutine)
  write(lun_log,*,err=9998)TRIM(eInform)
  write(lun_log,*,err=9998)'DT(cc,w,a,k,s,mc)=', &
                                            dtcc,dtw,dta,dtk,dts,dtmc
  write(lun_log,*,err=9998)'DELT=',delt
  write(lun_log,*,err=9998)'LEVEL=',il,MAXTLV
  write(lun_log,*,err=9998)'DZG,HP,Wpuff=',dzg,hp,wpuff
  call dump_puff(0,p)
  go to 9999
end if

lev2 = max0(lev2,il)
il = max0(il,lev1)
if (il /= p%idtl) then
  p%idtl = il
  nchg   = nchg + 1
end if

9999    continue

return

!------ set log write error and go to return

9998    continue
nError   = WR_ERROR
eRoutine = 'step_p'
eMessage = 'Error writing SCICHEM log file'
eInform  = 'File='//TRIM(file_log)
go to 9999

end

subroutine step_split(p,dt,lev1,lev2,nchg)
!*******************************************************************************
!
! FUNCTION: Set capping height / check for splitting above or below zcap
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  splitz                set_zcap
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p    !Puff structure
real    dt             !Time step
integer lev1, lev2     !Time levels being stepped (from lev1 to lev2)
integer nchg           !Number of time level changes

!-----  get inversion height and dtdzs at new puff position

!       call get_zi( p%xbar , p%ybar , xmap , ymap )

!------ check for splitting above zi

if (lsplitz .and. zisav >= zsav) then

  ztop = min(p%zbar+SZFAC*sz,max(zisav,p%zbar))

  if (ztop > max(hp+ZIFAC*(p%zi-hp),p%zi+ZIDEL)) then

!---------- set cap for local boundary layer

    if (lblcap) then
      zlim = zinv
    else
      zlim = p%zc
    end if

    call splitz(p,lev1,lev2,nchg)
    if (nError /= NO_ERROR) go to 9999

  else

    call set_zcap(p,dt)
    if (nError /= NO_ERROR) go to 9999

  end if

else

  call set_zcap(p,dt)
  if (nError /= NO_ERROR) go to 9999

end if

p%zi = zinv

9999    continue

return

end

subroutine step_scale(p,dt)
!*******************************************************************************
!
! FUNCTION: Advance puff length scales
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
use common_met
use step_p_inc

implicit none


! --- ARGUMENTS
 
type ( puff_str ) p  !Puff structure
real dt              !Time step (sec)

! --- LOCALS

real rat, fac, shear, sig_shr2

!----- define rms vertical shear

if (uub > 0.) then
  sig_shr2 = 0.25*(uubz*uubz/uub+vvbz*vvbz/vvb)
else
  sig_shr2  = 0.
end if

svq = svq + sigvd

!----- advance puff length scales

p%si  = p%si  + siq*dt

if (lzinv .and. sv >= WMX*(zinv-hp))then
  p%si2 = p%si2 + siq*dt*si/si2
  p%sv  = p%sv  + svq*dt
else
  shear = sqrt(dudz**2 + dvdz**2 + sig_shr2)
  rat   = min(sv/si2,1.0)
  fac   = (shear*sv*sv/max(difb,1.e-6))**0.3333333
  fac   = max(1.-fac*rat,0.)
  shear = 0.8660254*shear*dt
!         if(lzinv)rat = 0.5*rat
  if (lcap) rat = 0.5*rat*(1.- min(sv/(WMX*(zinv-hp)),1.0))
  p%si2 = p%si2 + siq*dt*si/si2 + shear*sv*fac
  p%sv  = (sv + svq*dt)/(1.0+shear*rat)
end if

if (ddyn /= 0.0) then
  fac   = min(1.0,0.5/(dt*abs(ddyn)))
  p%sv  = p%sv*exp(fac*ddyn*dt)
end if

return
end

subroutine step_dynamics(p,pd,dt,pmat)
!*******************************************************************************
!
! FUNCTION: Advance puff dynamics
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
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p         !Puff structure
type ( puff_dynamics ) pd   !Puff dynamics structure
type ( puff_material ) pmat !Puff material structure
real dt                     !Time step (sec)


! --- LOCALS

real tfac, denfac, a11, a12, a21, a22, b1, b2, dd, dtdzd
real bfac

logical IsGas

!-----  set temperature-dependent buoyant gas density factor

if (buoy_gas) then
  denfac = (tdyn+tb)/300.
  tfac   = tb*denfac
  denfac = 1.0/(1.0 + max(denfac*pd%bcp/csav-tdyn/tb,0.))
else
  tfac   = 0.0
  denfac = 1.0/(1.0 + max(-tdyn/tb,0.))
end if

!-----  set ambient temperature gradient zero for dense effects

if (ldense) then
  dtdzd = 0.0
else
  dtdzd = dtdz
end if

!-----  advance mean momentum and temperature excess for gases only

if(IsGas(typeID(p%ityp)%icls))then

  bfac = buoy_fac(p%ityp)

  a11 = 1. + (tauw+0.2*bv)*dt
  a22 = 1. + tauw*dt
  a12 = -gt0*denfac*dt
  a21 = dtdzd*dt
  dd  = a11*a22 - a12*a21
  b1  = pd%w - gt0*tfac*bfac*p%c*denfac*dt
  b2  = pd%t

  pd%w = (b1*a22 - b2*a12) / dd
  pd%t = (b2*a11 - b1*a21) / dd

end if

!-----  advance momentum and temperature excess correlations

pd%bcp = (pd%bcp + qosi_cc*pd%bcb*dt) / (1. + (qosi_cc+tauc)*dt)

a11 = 1. + (qosi_cc+tauc+tauw+0.2*bv)*dt
a12 = -gt0*denfac*dt
a22 = 1. + (qosi_cc+tauc+tauw)*dt
a21 = dtdzd*dt
dd  = a11*a22 - a12*a21
b1  = (qosi_cc*pd%wcb - tfac*gt0*denfac*pd%bcp)*dt + pd%wcp
b2  = qosi_cc*dt*pd%ctb + pd%ctp

pd%wcp = (b1*a22 - b2*a12) / dd
pd%ctp = (b2*a11 - b1*a21) / dd

if (ldense) then
  wdyn  = wdyn*(1.0 + max(-0.5,ddyn*dt))  + udyn*hx + vdyn*hy
else
  wdyn  = 0.5*(wdyn + pd%wcp/p%c)
end if

return
end

subroutine step_velcorr(p,dt)
!*******************************************************************************
!
! FUNCTION:  Advance velocity correlations
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                limit_wc
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p  !Puff structure
real dt              !Time step (sec)

! --- LOCALS

real damp, geq, keq, soq, wp2, fac

!------ adjust vertical diffusivity for non-equilibrium positive buoyancy

geq = wtbl - dtdz*difb
if (geq > 0.0) then
  soq   = sbz/max(qs,1.e-3)
  geq   = geq*soq/(2.*bs)
  keq   = max(difb,1.e-10)
  difb  = fdyn*(wwbl + gt*geq*min(p%zwc/keq/csav,1.))*soq/a
  dddz  = dddz*difb/keq
else
  difb = fdyn*difb
  dddz = fdyn*dddz
end if

!------ adjust diffusivity for relative motion

wp2  = wdyn*wdyn + vfall*vfall

if (wp2 > 0.) then
  fac  = sqrt(qqs/(qqs + wp2))
  difb = difb*fac
  dddz = dddz*fac
  if (qqsh > 0.) then
    aqsosyt = aqsosyt*sqrt((qqsh + wp2)/qqsh)
  end if
  if (qqb > 0.) then
    aqbosyt = aqbosyt*sqrt((qqb + wp2)/qqb)
  end if
  if (qql > 0.) then
    aqlosyt = aqlosyt*sqrt((qql + wp2)/qql)
  end if
end if

!-----  reduce diffusivity gradient for poorly resolved BL
!       (puffs are at least sz off ground and won't get much lower
!        so larger diffusivities near the surface are never seen)

if (lzinv .and. sz > 0.5*(zinv-hp)) then
  dddz = dddz * max(0.0,zinv-hp-sz)/(zinv-hp)
end if

!-----  advance vertical velocity correlations

damp  = (aqoszt+tauc)*dt
p%zwc = (p%zwc + damp*difb*p%c)/(1.0 + damp)
p%wc  = (p%wc  + damp*dddz*p%c)/(1.0 + damp)

!-----  Limit zwc no greater than equilibrium background value

if (.not.lzinv) then
  p%zwc = min(p%zwc,difb*p%c)
end if

!-----  limit vertical drift correlation

if (difb > 0.0) then
  fac   = p%zwc/difb
  call limit_wc(p%wc,dddz,fac)
else
  p%wc = 0.0
end if

!-----  advance horizontal velocity correlations

damp  = 1.0 + (aqlosyt+tauc)*dt
p%xuc = (p%xuc + uub*p%c*dt) / damp
p%xvc = (p%xvc + uvb*p%c*dt) / damp
p%yvc = (p%yvc + vvb*p%c*dt) / damp
damp  = 1.0 + (aqsosyt+tauc)*dt
p%yvsc = (p%yvsc + uubl*p%c*dt) / damp
damp  = 1.0 + (aqbosyt+tauc)*dt
p%yvbc = (p%yvbc + vvbl*p%c*dt) / damp

return
end

subroutine limit_wc(wc,dddz,fac)
!*******************************************************************************
!
! FUNCTION: Limit vertical drift velocity based on ambient diffusivity gradient
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
 
real wc    !Vertical drift velocity
real dddz  !Vertical diffusivity gradient
real fac   !p%zwc/difb

if (dddz > 0.0) then
  wc = max(0.0,min(wc,fac*dddz))
else
  wc = min(0.0,max(wc,fac*dddz))
end if

return
end

subroutine set_diff(p)
!*******************************************************************************
!
! FUNCTION: Set diffusivities
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
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p  !Puff structure

!------ Vertical diffusivity and shear - shut off when well mixed

if (p%zc > 0.0 .and. sz >= 0.999*WMX*(p%zc-hp))then
  p%wc  = 0.0
  zwct  = 0.0
  dudz  = 0.0
  dvdz  = 0.0
else
  zwct = p%zwc/p%c + difp
end if

!------ Horizontal diffusivities

xuct = (p%xuc+p%yvsc+p%yvbc)/p%c + difp
xvct =  p%xvc/p%c
yvct = (p%yvc+p%yvsc+p%yvbc)/p%c + difp

return
end

subroutine set_penetration(p,pd,dt,znew)
!*******************************************************************************
!
! FUNCTION: Compute penetration into overlying stable region
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 get_met               reset_lsv            turb_rescale
!               step_turb                limit_wc
!
! REVISION HISTORY: 
!
! 24-JUL-2001 : Removed wdyn = 0.0. Not used in step_p but used by static puffs. 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p        !Puff structure
type ( puff_dynamics ) pd  !Puff dynamics structure
real dt                    !Time step (sec)
real znew                  !Advanced vertical coordinate of puff (m)

! --- PARAMETERS
 
real, parameter :: A_JUMP = 0.143, ENT_FAC = 1.0

! --- LOCALS

real del_temp, deli, ke, bvi
real z_entrn, z_trap, damp, zp, xvc

!------ get temperature gradient at inversion

!       call get_temp(p%xbar,p%ybar,zinv,ti,gamma,0)

!------ estimate temperature jump

del_temp = A_JUMP*(zinv-hp)*gamma

!------ estimate height of penetration based on energy equation

deli  = gt0*(del_temp - tdyn + tb*pd%bcp/csav)
ke    = 0.5*wdyn**2
bvi   = gt0*gamma
zp    = 0.5*(-deli+sqrt(deli*deli+4.*bvi*ke))/bvi

!------ complete penetration if beyond the entrainment zone -
!       entrainment zone height estimate based on a fraction of the
!       mixed-layer depth or the horizontal scale of the cloud/plume

z_entrn = max(0.1*(zinv-hp),ENT_FAC*p%si)

if (zp > z_entrn) then

!------ complete penetration

  p%zbar = min(znew,zinv+zp)
  p%zc   = 0.
  lzinv  = .false.
  lcap   = .false.

!------ adjust temp change to account for vertical motion

  del_temp = del_temp + gamma*(p%zbar-zinv)

!------ remove temperature jump from buoyancy variables

  pd%ctp = pd%ctp - p%c*del_temp
  pd%t   = pd%t   - pd%w*del_temp/wdyn

!------ re-set diffusivities; get met background for new location

  call get_met(p%xbar,p%ybar,p%zbar,0.0,0.0,xmap,ymap,0,hazflag)
  if (lsv_oper) call reset_lsv(si)

  if (.not.hazflag .and. t_avg /= DEF_VAL_R) then
    call turb_rescale(si,sv,vel2)
  end if

  call step_turb

  p%zwc = min(p%zwc,difb*p%c)
  call limit_wc(p%wc,dddz,p%c)

  if (aqsosyt > 0.) then
    damp  = p%c/aqsosyt
    p%yvsc = min(p%yvsc,uubl*damp)
  else
    p%yvsc = 0.
  end if

  if (aqbosyt > 0.) then
    damp  = p%c/aqbosyt
    p%yvbc = min(p%yvbc,vvbl*damp)
  else
    p%yvbc = 0.
  end if

  if (aqlosyt > 0.) then
    damp  = p%c/aqlosyt
    if (p%xuc*p%yvc > 0.) then
      xvc   = p%xvc/sqrt(p%xuc*p%yvc)
    else
      xvc = 0.
    end if
    p%xuc = min(p%xuc,uub*damp)
!           p%xvc = min(p%xvc,uvb*damp)
    p%yvc = min(p%yvc,vvb*damp)
    p%xvc = xvc*sqrt(p%xuc*p%yvc)
  else
    p%xuc = 0.
    p%xvc = 0.
    p%yvc = 0.
  end if

else

!------ puff does not penetrate; turn off dynamic sand locate below inversion

  pd%w   = 0.
  pd%wcp = 0.

  z_trap = min(0.01*(zinv-hp),ENT_FAC*p%si)
  p%zbar  = zinv - z_trap
  p%zc    = zinv

end if

return

end

subroutine set_descend(p,pd,znew)
!*******************************************************************************
!
! FUNCTION:  Modify buoyancy for puffs descending from stable region into
!            mixed-layer below inversion
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 get_met               reset_lsv            turb_rescale
!               step_turb                limit_wc
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p        !Puff structure
type ( puff_dynamics ) pd  !Puff dynamics structure
real znew                  !Advanced vertical coordinate of puff (m)

! --- PARAMETERS
 
real, parameter :: A_JUMP  = 0.143

! --- LOCALS

real del_temp, damp, xvc

!------ get temperature gradient at inversion

!       call get_temp(p%xbar,p%ybar,zinv,ti,gamma,0)

!------ estimate temperature jump

del_temp = min(0.0,pd%ctp/p%c)
del_temp = min(A_JUMP*(zinv-hp)*gamma,-del_temp)

!------ add to buoyancy variables

pd%ctp  = pd%ctp + p%c*del_temp
if (wdyn < 0.) then
  pd%t = pd%t + pd%w/wdyn*del_temp
end if

!------ re-set diffusivities; get met background for new location

p%zbar = znew
lzinv  = .false.

call get_met(p%xbar,p%ybar,p%zbar,0.0,0.0,xmap,ymap,0,hazflag)
if (lsv_oper) call reset_lsv(si)

if (.not.hazflag .and. t_avg /= DEF_VAL_R) then
  call turb_rescale(si,sv,vel2)
end if

call step_turb

p%zwc = min(p%zwc,difb*p%c)
call limit_wc(p%wc,dddz,p%c)

if (aqsosyt > 0.) then
  damp  = p%c/aqsosyt
  p%yvsc = min(p%yvsc,uubl*damp)
else
  p%yvsc = 0.
end if

if (aqbosyt > 0.) then
  damp  = p%c/aqbosyt
  p%yvbc = min(p%yvbc,vvbl*damp)
else
  p%yvbc = 0.
end if

if (aqlosyt > 0.) then
  damp  = p%c/aqlosyt
  if (p%xuc*p%yvc > 0.) then
    xvc   = p%xvc/sqrt(p%xuc*p%yvc)
  else
    xvc = 0.
  end if
  p%xuc = min(p%xuc,uub*damp)
!         p%xvc = min(p%xvc,uvb*damp)
  p%yvc = min(p%yvc,vvb*damp)
  p%xvc = xvc*sqrt(p%xuc*p%yvc)
else
  p%xuc = 0.
  p%xvc = 0.
  p%yvc = 0.
end if

return

end

subroutine set_zcap(p,dt)
!*******************************************************************************
!
! FUNCTION: Set capping height
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                split_zi
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc
use files_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p        !Puff structure
real dt                    !Time step (sec)

! --- PARAMETERS
 
real, parameter :: SZ_FAC = 4.0, SZ_CAP = 1.5, WW_FAC = 0.01

if (lblcap .and. p%zbar <= zinv) then

  if (p%zbar + SZ_CAP*sz < zinv) then
    p%zc = zinv
  else
    if (p%zc == 0. .or. &
            p%zc > zinv + max(0.3*(zinv-hp),dzg)) then
      call split_zi(p,zinv)
      if (nError /= NO_ERROR) then !not enough space for new puff
        if (p%zc == 0.0) then
          p%zc = p%zbar + SZ_CAP*sz
        end if
        nError = NO_ERROR
      end if
    else
      p%zc = max(p%zc,zinv)
    end if
  end if

else

  if (p%zc > 0.) then
    if (p%zbar + SZ_FAC*sz > p%zc) then
      p%zc = p%zc + (1.5*wb - 0.5*p%wo + WW_FAC*sqrt(wwbl)) * dt
      p%zc = max(p%zc,hp+sz,zinv)
    else
      p%zc = 0.
    end if
  end if

end if

if (p%zbar > p%zc .and. p%zc > 0.0) then
 ! write(lun_log,*) 'Puff above cap',p%zbar,p%zc,p%zi,zinv
  p%zc = p%zbar + 0.01
end if

return

end

subroutine set_zcap_static(p,dt)
!*******************************************************************************
!
! FUNCTION: Set capping height
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                split_zi
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc
use files_inc

!------ set capping height for static puffs (no split_zi)

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p        !Puff structure
real dt                    !Time step (sec)

! --- PARAMETERS
 
real, parameter :: SZ_FAC = 4.0, SZ_CAP = 1.5, WW_FAC = 0.01

if (lblcap .and. p%zbar <= zinv) then

  if (p%zbar + SZ_CAP*sz < zinv) then
    p%zc = zinv
  else
    p%zc = max(p%zc,zinv)
  end if

else

  if (p%zc > 0.) then
    if (p%zbar + SZ_FAC*sz > p%zc) then
      p%zc = p%zc + (1.5*wb - 0.5*p%wo + WW_FAC*sqrt(wwbl)) * dt
      p%zc = max(p%zc,hp+sz,zinv)
    else
      p%zc = 0.
    end if
  end if

end if

if (p%zbar > p%zc .and. p%zc > 0.0) then
 ! write(lun_log,*) 'Puff above cap',p%zbar,p%zc,p%zi,zinv
  p%zc = p%zbar + 0.01
end if

return

end

subroutine set_hscale(uubt,vvbt,uvbt,sly)
!*******************************************************************************
!
! FUNCTION:  Compute total velocity variances (includes large-scale, buoyancy,
!            and shear) and the composite horizontal length scale
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
 
use common_met

implicit none

! --- ARGUMENTS
 
real uubt, vvbt, uvbt !Total velocity variances
real sly              !Composite horizontal scale

! --- LOCALS

real rat1, rat2, rat3
real wws, wwb

!------ set total turbulence values

uubt = uub + uubl + vvbl
vvbt = vvb + uubl + vvbl
uvbt = uvb    !+ uvbl

!------ partition vertical velocity

wws = wwbh
wwb = 0.

!-----  velocity scales

qqsh = 2.*uubl + wws
qqb  = 2.*vvbl + wwb
qql  = uub + vvb

!------ set composite horizontal scale

rat1 = 2.*uubl/(uubt+vvbt+1.e-10)
rat2 = 2.*vvbl/(uubt+vvbt+1.e-10)
rat3 = (uub+vvb)/(uubt+vvbt+1.e-10)
if (rat1+rat2+rat3 > 0.) then
  sly = rat1*sbls + rat2*sbl + rat3*sby
else                                    ! for bl_type = none
  sly = max(sbls,sbl,sby)
end if

return

end

subroutine dense_rot(pd,fac)
!*******************************************************************************
!
! FUNCTION:  Rotate puff velocity gradients
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              set_matrot                  trnsps                  matmul
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_dynamics ) pd  !Puff dynamics structure
real fac                   !(p%ccb/max(p%ccb,p%cc))**alp

! --- LOCALS

real amat(3,3), bmat(3,3), cmat(3,3), at(3,3)

!---- Rotate from local coordinates

call set_matrot(hx,hy,amat)

bmat = 0.0
bmat(1,1) = pd%dudx
bmat(2,2) = pd%dvdy
bmat(3,3) = -(pd%dudx+pd%dvdy)

call trnsps(amat,at)
call xmatmul(at,bmat,cmat)
call xmatmul(cmat,amat,bmat)

dudx = dudx + bmat(1,1)*fac
dudy = dudy + bmat(1,2)*fac
dudz = dudz + bmat(1,3)*fac
dvdx = dvdx + bmat(2,1)*fac
dvdy = dvdy + bmat(2,2)*fac
dvdz = dvdz + bmat(2,3)*fac
dwdx = dwdx + bmat(3,1)*fac
dwdy = dwdy + bmat(3,2)*fac
dwdz = dwdz + bmat(3,3)*fac

return

end

subroutine set_dense_gas(p,pd)
!*******************************************************************************
!
! FUNCTION: Set dense gas dynamics
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_topog           dense_rot_ter            dense_effect
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p        !Puff structure
type ( puff_dynamics ) pd  !Puff dynamics structure

! --- LOCALS

logical dense_effect

real hp, hx, hy

if (lter) then
  call get_topog(p%xbar,p%ybar,hp,hx,hy)
else
  hp = 0.0
  hx = 0.0
  hy = 0.0
end if

!------ Check for ground effects

if (.not.dense_effect(p%zbar-hp,p%sv,pd%wcp)) then

  pd%u0 = 0.0
  pd%X  = 0.0
  pd%Y  = 0.0
  pd%sn = 0.0
  pd%cs = 0.0

else

!------ Rotate into terrain coordinates

  call dense_rot_ter(hx,hy,p,pd)

end if

return
end

logical function dense_effect(zz,sv,w)
!*******************************************************************************
!
! FUNCTION:  Determine if dense effects need to be accounted for
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
 
real zz   !Puff height above terrain
real sv   !Vertical turbulence scale
real w    !Turbulent drift velocity

dense_effect = dense_gas .and. (zz < 2.*sv) .and. w <= 0

return
end

subroutine dense_rot_norm(hx0,hy0,hx1,hy1,p)
!*******************************************************************************
!
! FUNCTION: Rotate puff sigmas normal to terrain
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  trnsps                  matmul             check_slope
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p      !Puff structure
real hx0, hy0, hx1, hy1  !Terrain gradients

! --- LOCALS

real xn0, yn0, zn0, xn1, yn1, zn1, xt1, yt1, zt1, fac, cs, sn
real amat(3,3), bmat(3,3), cmat(3,3), at(3,3)

logical check_slope

if (.not.check_slope(hx1-hx0,hy1-hy0)) return

fac = 1.0/sqrt(1.0+hx0*hx0+hy0*hy0)
xn0 = -fac*hx0
yn0 = -fac*hy0
zn0 =  fac

fac = 1.0/sqrt(1.0+hx1*hx1+hy1*hy1)
xn1 = -fac*hx1
yn1 = -fac*hy1
zn1 =  fac

cs = xn0*xn1 + yn0*yn1 + zn0*zn1

xt1 = (yn0*zn1 - zn0*yn1)
yt1 = (zn0*xn1 - xn0*zn1)
zt1 = (xn0*yn1 - yn0*xn1)
sn = sqrt(xt1*xt1+yt1*yt1+zt1*zt1)

xt1 = xt1/sn
yt1 = yt1/sn
zt1 = zt1/sn

amat(1,1) = xn0
amat(1,2) = yn0
amat(1,3) = zn0
amat(2,1) = xt1
amat(2,2) = yt1
amat(2,3) = zt1
amat(3,1) = (yn0*zt1 - zn0*yt1)
amat(3,2) = (zn0*xt1 - xn0*zt1)
amat(3,3) = (xn0*yt1 - yn0*xt1)

bmat = 0.0
bmat(1,1) = cs
bmat(1,3) = sn
bmat(2,2) = 1.
bmat(3,1) = -sn
bmat(3,3) = cs

call trnsps(amat,at)
call xmatmul(bmat,amat,cmat)
call xmatmul(at,cmat,amat)

bmat(1,1) = p%sxx
bmat(1,2) = p%sxy
bmat(1,3) = p%sxz
bmat(2,1) = p%sxy
bmat(2,2) = p%syy
bmat(2,3) = p%syz
bmat(3,1) = p%sxz
bmat(3,2) = p%syz
bmat(3,3) = p%szz

call trnsps(amat,at)
call xmatmul(amat,bmat,cmat)
call xmatmul(cmat,at,bmat)

p%sxx = bmat(1,1)
p%sxy = bmat(1,2)
p%sxz = bmat(1,3)
p%syy = bmat(2,2)
p%syz = bmat(2,3)
p%szz = bmat(3,3)

return
end

subroutine dense_rot_ter(hx0,hy0,p,pd)
!*******************************************************************************
!
! FUNCTION:  Allow dense gases to account for the ground
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              set_matrot                  trnsps                  matmul
!             check_slope
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p       !Puff structure
type ( puff_dynamics ) pd !Puff dynamics structure
real hx0, hy0             !Terrain gradients

! --- PARAMETERS
 
real, parameter :: VELFAC_DENSE = 0.318310   ! 1./pi
real, parameter :: RFAC_DENSE   = 1.5

! --- LOCALS

real deth, tem, alp, emaj, emin, sxx, sxy, syy, xx, yy
real amat(3,3), bmat(3,3), cmat(3,3), at(3,3)

logical check_slope


if (check_slope(hx0,hy0)) then

  call set_matrot(hx0,hy0,amat)
  bmat(1,1) = p%sxx
  bmat(1,2) = p%sxy
  bmat(1,3) = p%sxz
  bmat(2,1) = p%sxy
  bmat(2,2) = p%syy
  bmat(2,3) = p%syz
  bmat(3,1) = p%sxz
  bmat(3,2) = p%syz
  bmat(3,3) = p%szz

  call trnsps(amat,at)
  call xmatmul(amat,bmat,cmat)
  call xmatmul(cmat,at,bmat)

  sxx = bmat(1,1)
  sxy = bmat(1,2)
  syy = bmat(2,2)

else

  sxx = p%sxx
  sxy = p%sxy
  syy = p%syy

end if

!------ Horizontal major and minor axes

deth = sxx*syy - sxy*sxy
tem  = 0.5*(sxx+syy)
alp  = sqrt(max(tem*tem-deth,0.))
emaj = tem + alp
emin = tem - alp

pd%X = RFAC_DENSE*sqrt(emaj)
pd%Y = RFAC_DENSE*sqrt(emin)

!-----  Rotation coefficients for principal axis system
!       X =  cs*x + sn*y                x = cs*X - sn*Y
!       Y = -sn*x + cs*y                y = sn*X + cs*Y

tem = emaj - sxx
if (tem > 0.0) then
  alp = sxy/tem
  tem = sqrt(1.+alp*alp)
  pd%cs = alp/tem
  pd%sn = 1.0/tem
else
  pd%sn = 0.0
  pd%cs = 1.0
end if

!-----  Dense gas velocity scale

xx    = pd%X**2
yy    = pd%Y**2
pd%u0 = -VELFAC_DENSE*pd%w*sqrt(xx+yy)/xx/yy

return
end

subroutine get_wash(p,dt)
!*******************************************************************************
!
! FUNCTION:   Get wash-out timescale for particle material types
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               rain_puff
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use step_p_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p  !Puff structure
real dt              !Time step (sec)

! --- PARAMETERS
 
real, parameter :: CLDMIN = 500.

! --- LOCALS

real vr
integer ityppr

!-----  Look up Raindrop Fall Velocity and Scavenging Coefficient
!-----  Interpolate Precipitation Rate from Specified Index
!-----  I = 0  --> PR =   0.0 mm/hr     !RAIN
!-----  I = 1  --> PR =   0.5 mm/hr
!-----  I = 2  --> PR =   3.5 mm/hr
!-----  I = 3  --> PR =  25.0 mm/hr
!-----  I = 4  --> PR =   5.0 mm/hr     !SNOW
!-----  I = 5  --> PR =  20.0 mm/hr
!-----  I = 6  --> PR = 100.0 mm/hr

if (p%zbar <= max(zinv,hp+CLDMIN)) then
  ityppr = nint(prbl)
else
  ityppr = 0
end if
vr     = vwash(ityppr)
taur   = twash(ityppr,p%ityp)

!-----  Create temporary rain puff for deposition calculation

if(taur == 0.0)then
  fwash = 0.0
else
  tauc   = tauc + taur
  fwash  = taur/tauc
  call rain_puff(p,vr,dt)
end if

return
end

subroutine rain_puff(p,vr,dt)
!*******************************************************************************
!
! FUNCTION: Creates a puff for rain deposition
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!           check_newpuff               get_topog                rot_norm
!             set_puff_xc
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use step_p_inc

implicit none

! --- ARGUMENTS

type ( puff_str ) p  !Puff structure
real vr              !Rain velocity (meters/sec)
real dt              !Time step (sec)

! --- LOCALS

real fac, frac, dtr, h1, hx1, hy1
integer naux, jtyp

fac  = 1.0 / (1.0 + tauc*dt)
frac = fwash*(1.0 - fac)

jtyp = p%ityp - typeID(p%ityp)%igrp + 1
naux = typeID(jtyp)%npaux

call check_newpuff(naux)
if(nError /= NO_ERROR)then
  eRoutine='RainPuff'
  go to 9999
end if

if(vr > 0.0)then
  dtr = p%zbar/vr
else
  dtr = 0.0
end if

puff(npuf+1)%xbar = p%xbar + p%uo*dtr*xmap
puff(npuf+1)%ybar = p%ybar + p%vo*dtr*ymap
puff(npuf+1)%zbar = 0.

puff(npuf+1)%axx = p%axx - p%axz*p%axz/p%azz
puff(npuf+1)%axy = p%axy - p%axz*p%ayz/p%azz
puff(npuf+1)%axz = 0.
puff(npuf+1)%ayy = p%ayy - p%ayz*p%ayz/p%azz
puff(npuf+1)%ayz = 0.
puff(npuf+1)%azz = p%azz

if (lter) then
  call get_topog(puff(npuf+1)%xbar,puff(npuf+1)%ybar,h1,hx1,hy1)
  call rot_norm(0.0,0.0,hx1,hy1,puff(npuf+1))
end if

if (p%iaux > 0) then
  puff(npuf+1)%iaux = npaux
else
  puff(npuf+1)%iaux = 0
end if
call set_puff_xc(puff(npuf+1),p,frac)

puff(npuf+1)%ityp = jtyp
puff(npuf+1)%inxt = 0
puff(npuf+1)%iprv = 0
puff(npuf+1)%idtl = p%idtl
puff(npuf+1)%idtn = 0

9999    continue

return
end

subroutine rot_norm(hx0,hy0,hx1,hy1,p)
!*******************************************************************************
!
! FUNCTION:  Rotate asig normal to the terrain (called by rain_puff)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  trnsps                  matmul             check_slope
!
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf

implicit none

! --- ARGUMENTS
 
real hx0, hy0, hx1, hy1  !Terrain gradients
type ( puff_str ) p      !Puff structure

! --- LOCALS

real xn0, yn0, zn0, xn1, yn1, zn1, xt1, yt1, zt1, fac, cs, sn
real amat(3,3), bmat(3,3), cmat(3,3), at(3,3)

logical check_slope

if (.not.check_slope(hx1-hx0,hy1-hy0)) return

fac = 1.0/sqrt(1.0+hx0*hx0+hy0*hy0)
xn0 = -fac*hx0
yn0 = -fac*hy0
zn0 =  fac

fac = 1.0/sqrt(1.0+hx1*hx1+hy1*hy1)
xn1 = -fac*hx1
yn1 = -fac*hy1
zn1 =  fac

cs = xn0*xn1 + yn0*yn1 + zn0*zn1

xt1 = (yn0*zn1 - zn0*yn1)
yt1 = (zn0*xn1 - xn0*zn1)
zt1 = (xn0*yn1 - yn0*xn1)
sn = sqrt(xt1*xt1+yt1*yt1+zt1*zt1)

xt1 = xt1/sn
yt1 = yt1/sn
zt1 = zt1/sn

amat(1,1) = xn0
amat(1,2) = yn0
amat(1,3) = zn0
amat(2,1) = xt1
amat(2,2) = yt1
amat(2,3) = zt1
amat(3,1) = (yn0*zt1 - zn0*yt1)
amat(3,2) = (zn0*xt1 - xn0*zt1)
amat(3,3) = (xn0*yt1 - yn0*xt1)

bmat  = 0.0
bmat(1,1) = cs
bmat(1,3) = sn
bmat(2,2) = 1.
bmat(3,1) = -sn
bmat(3,3) = cs

call trnsps(amat,at)
call xmatmul(bmat,amat,cmat)
call xmatmul(at,cmat,amat)

bmat(1,1) = p%axx
bmat(1,2) = p%axy
bmat(1,3) = p%axz
bmat(2,1) = p%axy
bmat(2,2) = p%ayy
bmat(2,3) = p%ayz
bmat(3,1) = p%axz
bmat(3,2) = p%ayz
bmat(3,3) = p%azz

call trnsps(amat,at)
call xmatmul(amat,bmat,cmat)
call xmatmul(cmat,at,bmat)

p%axx = bmat(1,1)
p%axy = bmat(1,2)
p%axz = bmat(1,3)
p%ayy = bmat(2,2)
p%ayz = bmat(2,3)
p%azz = bmat(3,3)

return
end
