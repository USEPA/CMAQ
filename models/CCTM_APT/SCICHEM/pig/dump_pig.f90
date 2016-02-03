logical function ldump_pig(p)
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Check if a puff should be transferred to the host model grid
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  mapfac
!
! REVISION HISTORY: 
!   07/25/00 Add chemical criteria (LPS) 
!   January 2004: Removed cgrid (PKK, AER)
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met
use multcomp_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p  !Puff structure

! --- LOCALS

real sizeh, xlam, dhgrd, xmap, ymap
real sizex, sizey, dxgrd, dygrd

logical lchem_criteria

!ldump_pig = .false. !no dumping
!return

! -- check if puff should be dumped onto host model grid

!turn off chemical criteria
ldump_chem = .false.

if (ldump_chem) then  ! use chemical criteria

  ldump_pig = lchem_criteria(p)

else  ! use physical criterion

  call mapfac(p%xbar , p%ybar , xmap , ymap)
  dhgrd = min(dxb/xmap,dyb/ymap)

  xlam = p%axx + p%ayy + sqrt((p%axx-p%ayy)**2+4.*p%axy**2)
  sizeh = 1./sqrt(xlam)

  ldump_pig = sizeh/dhgrd >= 0.25

  if (.not. ldump_pig) then
     sizex = SQRT(p%sxx)
     dxgrd = dxb/xmap
     sizey = SQRT(p%syy)
     dygrd = dyb/ymap
     ldump_pig = sizex/dxgrd >= 4 .or. sizey/dygrd >= 4
  end if

end if  

return
end

logical function lchem_criteria(p)
!******************************************************************************
!
! FUNCTION:  Check if a puff should be transferred to the host model grid
!            using chemical criteria
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  mapfac
!
! REVISION HISTORY: 
!
!   January 2004: Removed cgrid (PKK, AER)
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p  !Puff structure

! --- LOCALS

type ( puff_mc ) pm  !Puff multicomponent structure

real tNOx, tNO2, tO3, tOx, ratO3Ox
 
integer ioff, j

call get_mc(p,pm)

ioff = 2*nspectot + ncorrm + ncorrt + 2
istage = nint(pm%mc(ioff) + 0.45)
if (  .not. (istage == 1 .or. istage == 2 &
      .or. istage == 3) ) then
  nError = IV_ERROR
  eRoutine = 'lchem_criteria'
  eMessage = 'Istage is not 1, 2, or  3'
  eAction = ' Alert model developer'
  write(eInform,'(a,i4)') 'Istage is',istage
  lchem_criteria = .true. 
  go to 9999
end if

lchem_criteria = (istage == 3) 

! Must be in stage 3 and satisfy the following criteria

if (lchem_criteria) then

! -- retrieve end of chemistry concentrations (total)

  ioff  = nspectot + ncorrm + ncorrt    
  tO3   = pm%mc(ioff + ikey_spec(O3))
  tNO2  = pm%mc(ioff + ikey_spec(NO2))
  tOx  = tO3 + tNO2
  if (tOx > 0.) then
    ratO3Ox = tO3/tOx
  else
    ratO3Ox = 1.
  end if
  
  lchem_criteria = ratO3Ox >= (1. - param_chem)

  if (lchem_criteria) return

! --- also dump if tOx less than 1 ppb or tNOx less than 0.1 ppb

  tNOx = pm%mc(ioff + ikey_spec(NO)) + tNO2
  lchem_criteria = (tOx <= 1.0E-3 .or. tNOx <= 1.0E-4)

end if
    
9999 return
end

subroutine dump_pig(p,cgrid)
!******************************************************************************
!
! FUNCTION:   Transfer a puff to the host model grid
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  get_mc               get_topog                  mapfac
!       init_puff_val_pig        get_puff_val_pig                update_k
!         set_equilibrium            isMCParticle
!
! REVISION HISTORY:
!   12/08/99 changed FAC from 3.0 to 2.5 (LPS) 
!   07/06/00 set istage to nstage before calling set_equilibrium (LPS) 
!   Updated January 2004 for Sep. 2003 CMAQ release (PKK, AER)
!   Minor updates (Fortran 90), February 2004 (PKK, AER)
!   Updated April 2005 to handle particle species (PKK, AER)
!   Updated April 2011 for CMAQ 5.0 (PK, ENVIRON)
!******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc
use common_met
use files_inc
use diagnostics
USE CGRID_SPCS, only: nspcsd
use code_defines_inc
use interface_definitions, only: INIT_PUFF_VAL_PIG

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p                   !Puff structure
REAL :: CGRID( :,:,:,: )              !3D ambient from host model

! --- PARAMETERS

real, parameter :: FAC = 2.5

! --- LOCALS

type ( puff_mc ) pm

real, dimension(:,:,:,:),allocatable :: psmp

real xmap,ymap, rmeq(MAX_MC)
real grdvol, fracoff(MAX_MC), csum(MAX_MC),tmp
real hp, dp, dum1, dum2, zx, delz, xsmp, ysmp, zsmp
real x0, y0, ppmfactor, ctrans

integer ix, iy, nnx, nny, nnz, ig, jg
integer k1, k2, ios
integer i, j, k, i0, j0, k0, isp, ii, jj, ijk, ij, iflag

logical ltrans, zflag, lredo, xflag, yflag

logical IsMCParticle

lredo = .false.

ndump = ndump + 1

call get_mc(p,pm)

! ---- transform puff zbar to met coordinates

call get_topog(p%xbar,p%ybar,hp,dum1,dum2)
dp = 1. - hp/zbtop
zx = (p%zbar - hp)/dp
delz = FAC*sqrt(p%szz)/dp

! ---- find grid cells to be adjusted

ix = nint((p%xbar-xmin)/dxb) + 1
iy = nint((p%ybar-ymin)/dyb) + 1

if (ix <= 0) then
  x0 = xb(1) + float(ix-1)*dxb
else if (ix > nxb) then
  x0 = xb(nxb) + float(ix-nxb)*dxb
else
  x0 = xb(ix)
end if

if (iy <= 0) then
  y0 = yb(1) + float(iy-1)*dyb
else if (iy > nyb) then
  y0 = yb(nyb) + float(iy-nyb)*dyb
else
  y0 = yb(iy)
end if

call mapfac(p%xbar , p%ybar , xmap , ymap)

nnx =  nint(FAC*sqrt(p%sxx)/(dxb/xmap))
nny =  nint(FAC*sqrt(p%syy)/(dyb/ymap))

nnx = MIN( nnx, 10 )
nny = MIN( nny, 10 )

ig = min0(nxb,max0(1,ix))
jg = min0(nyb,max0(1,iy))

xflag = 3.*sqrt(p%sxx) < dxb
yflag = 3.*sqrt(p%syy) < dyb
if (xflag .or. yflag) write(6,*)&
    'xflag,yflag',xflag,yflag,3.*sqrt(p%sxx),xb(ix) - xb(ix-1),&
                    3.*sqrt(p%syy), yb(iy) - yb(iy-1)

! -- use real z's (at puff centroid)
do k1 = 1, nzb
 if (  z3d(ig,jg,k1) >= p%zbar - hp - delz ) exit
end do
k1 = min0(k1,nzb)

do k2 = k1, nzb
  if ( z3d(ig,jg,k2) >= p%zbar - hp + delz ) exit
end do
k2 = min0(k2,nzb)
nnz = k2 - k1 + 1
zflag = 3.*sqrt(p%szz) < z3d(ig,jg,k1+nnz/2) - z3d(ig,jg,k1+nnz/2-1)

! ---- allocate sampler array

allocate(psmp(nspectot+2,2*nnx+1,2*nny+1,nnz), stat = ios)
if (ios /= 0) then
  nError   = SZ_ERROR
  eRoutine = 'dump_pig'
  eMessage = 'Insufficient memory to allocate sampler species arrays'
  write(eInform,*) 'Bytes requested =',(nspectot+2)*(2*nnx+1)*(2*nny+1)*nnz*4
  go to 9999
end if

! ---- find concentrations at grid points and sum them up

call init_puff_val_pig(p,cgrid)

200 continue

do i = 1,nspectot+1
  csum(i) = 0.
end do

do i = -nnx, nnx
  i0 = i + nnx + 1
  ig = min0(nxb,max0(1,i+ix))
  xsmp = x0 + float(i)*dxb
  if (xflag) xsmp = p%xbar
  do j = -nny, nny
    j0 = j + nny + 1
    jg = min0(nyb,max0(1,j+iy))
    ysmp = y0 + float(j)*dyb
    if (yflag) ysmp = p%ybar

    call mapfac(xsmp , ysmp , xmap , ymap)
    hp = hs((jg-1)*nxb + ig)
    dp = 1. - hp/zbtop

    do k = k1, k2
      k0 = k - k1 + 1
      if (zflag) then
        zsmp = p%zbar &
             -(p%axz*(xsmp-p%xbar)/xmap + p%ayz*(ysmp-p%ybar)/ymap)/p%azz
      else
        zsmp = z3d(ig,jg,k) + hp
        !zsmp = zb(k)*dp + hp
      end if

      call get_puff_val_pig(xsmp,ysmp,zsmp,p,psmp(1,i0,j0,k0))
      !grdvol = dxb*dyb*(zbw(k+1) - zbw(k))*dp/(xmap*ymap)
      grdvol = dxb*dyb*(zw3d(ig,jg,k+1) - zw3d(ig,jg,k))/(xmap*ymap)
      do isp = 1, nspectot+1
        csum(isp) = csum(isp) + grdvol*psmp(isp,i0,j0,k0)
      end do

    end do
  end do
end do

! - check if lost good puff
if (csum(nspectot+1) == 0. .and. p%c /= 0.) then
  if (.not. zflag .and. .not. lredo) then
    zflag = .true.
    lredo = .true.
    write(6,*) 'Setting zflag in dump_pig and retrying',csum(nspectot+1),&
                p%c,sqrt(p%szz),z3d(ig,jg,k1+nnz/2),z3d(ig,jg,k1+nnz/2-1)
    go to 200
  else
    write(6,*) 'Lost a good puff: ',p%c,zflag,lredo
  end if
end if

! - set adjustment factor
do i = 1, nspectot
  if (csum(i) /= 0.) then
    fracoff(i) = pm%mc(i)/csum(i)
  else
    fracoff(i) = 1.
    remvd(i) = remvd(i) + pm%mc(i)
    if (abs(pm%mc(i)) > 1.) &
    write(6,*) 'removing:',species(i)%name,pm%mc(i),p%xbar,p%ybar,p%zbar
  end if
end do
jj = nspectot+1
if (csum(jj) /= 0.) then
  fracoff(jj) = p%c/csum(jj)
else
  fracoff(jj) = 1.
  remvd(jj) = remvd(jj) + p%c
  write(6,*) 'removing tracer:',&
           remvd(jj),p%c,x0,y0,nnx,nny,zw3d(ig,jg,k1),zw3d(ig,jg,k2),&
          sqrt(p%sxx),sqrt(p%syy),sqrt(p%szz),delz,p%xbar,p%ybar,p%zbar,&
          xflag,yflag,zflag
end if

! ---- adjust concentrations to conserve mass, add ambient and re-set cgrid

do i = -nnx, nnx
  i0 = i + nnx + 1
  ig = min0(nxb,max0(1,i+ix))
  xsmp = x0 + float(i)*dxb
  ltrans = xsmp >= xmin .and. xsmp <= xmax

  do j = -nny, nny
    j0 = j + nny + 1
    jg = min0(nyb,max0(1,j+iy))
    ysmp = y0 + float(j)*dyb
    ltrans = ltrans .and. ysmp >= ymin .and. ysmp <= ymax

    call mapfac(xsmp , ysmp , xmap , ymap)
    hp = hs((jg-1)*nxb + ig)
    dp = 1. - hp/zbtop

    do k = k1, k2
      k0 = k - k1 + 1
        zsmp = z3d(ig,jg,k) + hp
        grdvol = dxb*dyb*(zw3d(ig,jg,k+1) - zw3d(ig,jg,k))/(xmap*ymap)
      !zsmp = zb(k)*dp + hp
      !grdvol = dxb*dyb*(zbw(k+1) - zbw(k))*dp/(xmap*ymap)

      do isp = 1, nequilibrium
        rmeq(isp) = 0.
      end do

      ! - get the met
      ij = (jg-1)*nxb + ig
      ijk = (k-1)*nxyb + ij
      tb = t_ua(ijk)
      pb = p_ua(ijk)
      tab = tb*(pb**0.285714)
      hb = h_ua(ijk)
      wcbar  = lwc2(ij)
      cldtop = cldtop2(ij)
      cldbot =  cldbot2(ij)
      fcc = fcc_bl(ij)
      ppmfactor = tab/(298.*pb)  !scale STP conc to cgrid ppm

      do isp = 1, nspectot

        ii = i_spec_list(isp)

        ! - attempt to transfer concentration in ctrans
        ctrans = psmp(isp,i0,j0,k0)*fracoff(isp)

        ! - save amount changed by ppmfac (gases only)
        if ( .not. IsMCParticle(isp)) then
          if (ltrans) then
            ppmfac(isp) = ppmfac(isp) + ctrans*grdvol*(ppmfactor - 1.)
            ctrans = ctrans*ppmfactor
          end if
        end if

        ! - save amount lost through clipping in 'remvd'
        if (ctrans < -cgrid(ig,jg,k,ii)) then
          if (ltrans) then
            tmp = (ctrans + cgrid(ig,jg,k,ii))*grdvol  !thrown out
            remvd(isp) = remvd(isp) + tmp
            if (isp > nspecies) rmeq(isp-nspecies) = tmp
          end if
        end if

        ! - don't allow concentration to go negative
        ! - diagnostics - store conc transferred in csum
        csum(isp)  = max(-cgrid(ig,jg,k,ii),ctrans)

        select case ( TRIM( species(isp)%name ) )
          case ('NUMATKN','NUMACC','NUMCOR','SRFATKN','SRFACC','SRFCOR' )
!          case ('ASO4J','AORGPAJ','AECJ','A25J','ACORS','ASEAS','ASOIL', &
!                'AHGPJ','NUMACC','NUMCOR','SRFACC' )
            csum(isp)  = MAX(csum(isp),0.)
        end select

        if (ltrans) then
          trans(isp) = trans(isp) + csum(isp)*grdvol
          ! - reset cgrid
          cgrid(ig,jg,k,ii) = CODEPARAM*csum(isp) + cgrid(ig,jg,k,ii)
          ! - set ps%a for equilibrium calculation
          ps(isp)%a = cgrid(ig,jg,k,ii)
        else
          bndry(isp) = bndry(isp) + ctrans*grdvol
        end if

      end do

      ! - diagnostic for tracer
      jj = nspectot+1
      ctrans    = psmp(jj,i0,j0,k0)*fracoff(jj)
      if (ltrans) then
        trans(jj) = trans(jj) + ctrans*grdvol
        cdump(ig,jg,k) = cdump(ig,jg,k) + ctrans
      else
        bndry(jj) = bndry(jj) + ctrans*grdvol
      end if

      ! - reset equilibrium species
      if (nequilibrium > 0 .and. ltrans) then
        call update_k(.true.,xb(ig),yb(jg),zsmp)
        if (nError /= NO_ERROR) go to 9999
        istage = nstage
        call set_equilibrium(.true.)
        if (nError /= NO_ERROR) then
          nError = NO_ERROR
        else
          do isp = 1, nequilibrium
            ii = i_eq_list(isp)
            jj = isp + nspecies
         ! reset diagnostics
            tmp = (ps(jj)%a - cgrid(ig,jg,k,ii))*grdvol !new mass transferred
         ! add new mass and subtract previous additions (csum and rmeq)
            chem(jj)  = chem(jj)  + tmp - csum(jj)*grdvol - rmeq(isp)
            remvd(jj) = remvd(jj) - rmeq(isp)
            trans(jj) = trans(jj) + tmp - csum(jj)*grdvol
         ! reset cgrid
            cgrid(ig,jg,k,ii) = CODEPARAM*ps(isp+nspecies)%a + CODEPARAM2*cgrid(ig,jg,k,ii)
          end do
        end if
      end if

    end do

  end do

end do

deallocate(psmp)

9999  return
      end

module init_puff_val

  use common_puf
  use param_inc
  save
  real cfac_mc(MAX_MC), cstar(MAX_MC)
  real rat, xr(3), xnrm(3)
  real cfac, tfac, zfac
  real asig(7)
  real xmap, ymap, xbar, ybar, vbar
  real deth
  integer nmc
  type ( puff_mc ) pm

end module init_puff_val

subroutine init_puff_val_pig(p,cgrid)
!******************************************************************************
!
! FUNCTION:  Initialize puff dumping routine
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!            get_dynamics                  get_mc                  mapfac
!               get_topog       puff_grnd_reflect              zi_reflect
!
!
! REVISION HISTORY: 
!   07/07/00  Do not use top hat factor for equilibrium species (LPS) 
!   05/11/01  Removed rspec calculations (BC)
!   Updated January 2004 for Sep. 2003 CMAQ release (PKK, AER)
!   02/10/04  Update to revision 1.19 of sampler.f90 (BC)
!
!******************************************************************************
 
! --- MODULES
use common_puf
use multcomp_inc
use init_puff_val
use interface_definitions, only: GET_AMB

implicit none

 ! --- ARGUMENTS
 
type ( puff_str ) p  !Puff structure
REAL :: CGRID( :,:,:,: )  ! -- 3D ambient concentrations

! --- LOCALS

type (puff_dynamics) pd

integer isp, ioff, i

!------ calculate concentrations at sampler locations

vol  = pi3*sqrt(p%det)
cfac = p%cfo*p%c/vol
if (dynamic) then
  call get_dynamics(p,pd)
  tfac = pd%ctp/p%c
else
  tfac = 0.
end if
  
nmc   = typeID(p%ityp)%nmc
if (nmc > 0) then
  call get_mc(p,pm)
  ioff = nspectot + ncorrm + ncorrt
  do isp = 1, nspectot
    cfac_mc(isp) = pm%mc(isp)/vol
    cstar(isp)   = MAX(pm%mc(isp+ioff),0.)
  end do
end if

call mapfac( p%xbar , p%ybar , xmap , ymap )

xbar  = p%xbar
ybar  = p%ybar
vbar  = p%zbar

call get_amb(xbar,ybar,vbar,t,cgrid)

deth = p%axx*p%ayy - p%axy**2
rat  = 0.5/(p%det*deth)
call get_asig(p,asig)

return

end

subroutine get_puff_val_pig(xs,ys,vs,p,dsmp)
!******************************************************************************
!
! FUNCTION:  Get the contribution of puff at sampler location xs, ys, vs
!            (routine modified from get_puff_val in sampler.f)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!              zi_reflect               get_topog
!
! REVISION HISTORY: 
!
! 05/11/01  Removed rspec and facth from the calculations (BC)
! 02/10/04  Update to revision 1.19 of sampler.f90 (BC)
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use multcomp_inc
use init_puff_val

implicit none

! --- ARGUMENTS
 
type ( puff_str ) p  !Puff structure
real xs, ys, vs      !Sampling location
real dsmp(*)         !Output array that stores contribution

! --- PARAMETERS
 
real, parameter :: ARGMAX = 20.0, SPAR = 0.3

! --- LOCALS

real tsig(6)
real hz, hx, hy, zp, znrm
real xp, yp, faci, arg, zr, facs, fac, facr
real xn,yn,zn,dy0
real r, facspec
real alpha,beta,args,s
integer isp
logical lset

!------ calculate concentrations at sampler locations

zp = (vs-vbar)
call zi_reflect(vbar,p%zc,p%zc,vs,rat,faci)
xp = (xs-xbar)/xmap
yp = (ys-ybar)/ymap

if (lter) then
  call get_topog(p%xbar,p%ybar,hz,hx,hy)
  call grnd_reflect(vbar-hz,asig,hx,hy,xr,xnrm,deth,znrm)
  zfac = 0.5*znrm/(p%det*deth)
  call get_topog(xs,ys,hz,hx,hy)
end if

if (lter) then
  lset =  hz <= vs
else
  lset = .true.
end if

do isp = 1,nspectot+2
  dsmp(isp) = 0.0
end do

if (lset) then

  arg = p%axx*xp*xp+2.*p%axy*xp*yp+2.*p%axz*xp*zp &
           + p%ayy*yp*yp+2.*p%ayz*yp*zp+p%azz*zp*zp
  if ( arg < ARGMAX )then
    if (lter) then
      zr   = xnrm(1)*(xp-xr(1)) + xnrm(2)*(yp-xr(2)) &
                 + xnrm(3)*(zp-xr(3))
      zr   = max(zr,0.)
      facs = exp(zfac*zr)
    else 
      facs = exp(-vs*vbar*rat)
    end if
    fac  = exp(-arg)*(1.+facs)*(1.+faci)
    dsmp(nspectot+1) = fac*cfac !tracer
    dsmp(nspectot+2) = fac*tfac !excess temperature
    if (nmc > 0) then
      call puff_rot(xp,yp,zp,p%uo,p%vo,asig,tsig,xn,yn,zn,dy0)
      do isp = 1, nspectot
        facspec = fac  
        if (isp <= nspecies ) then             
          if (species(isp)%star /= 0 .and. ps(isp)%a /= 0.) then
            r =  min(1.,cstar(isp)/ps(isp)%a)
          else
            r = 1.0
          end if
        else 
          if (pm%mc(isp) >= 0.0) then
            r = 1.0
          else
            r = 0.0
          end if
        end if
        if (r < 1.0) then
          s = 1.0 + SPAR*(1.0 - r)
          alpha = 0.5*s*(s**2 + 1.0)
          beta  = s*(s**2 - 1.0)
          args  = tsig(1)*xn*xn &
                + 2.*tsig(2)*xn*(dy0+s*(yn-dy0))+2.*tsig(3)*xn*zn &
                + tsig(4)*(dy0+s*(yn-dy0))**2 &
                + 2.*tsig(5)*(dy0+s*(yn-dy0))*zn + tsig(6)*zn*zn
          facspec = (alpha + beta*(tsig(4)* &
                   (s*(yn-dy0))**2 -1.0))*exp(-args)*(1.+facs)*(1.+faci)            
        end if
        dsmp(isp) = facspec*cfac_mc(isp)
      end do
    end if ! (nmc > 0)
  end if   ! (arg < ARGMAX)
end if     ! (lset)

return

end
