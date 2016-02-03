!$RCSfile: get_met.f90,v $
!$Revision: 1.7 $
!$Date: 2010/10/27 21:10:48 $
!*****************************************************************************
!REVISION HISTORY:
! subroutine get_blvar changed to calculate aerodynamic resistances
! Added subroutine get_radyn - PK, AER, Jan/Feb 2005
! 01/31/2007 : Use constants from constants_fd - BC
! Aug 2010: Update additional changes for CMAQ-APT-PM made by PK, AER - BC(Sage-Mgt)
!*****************************************************************************

subroutine get_met(xp,yp,zp,szz,zc,xfac,yfac,iflag,hazflag)
!******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION: Define met variables at location (xp,yp,zp)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_xyfac                 set_mxy               get_blvar
!               set_topog               get_uafac               set_flags
!             get_uvwt_bl             get_uvwt_ua                get_sstb
!                grad_ter                get_ensm              get_ustdep
!            set_pressure              get_met_xy             get_met_xyz
!
! REVISION HISTORY: 
!  May 2006, PK, AER: added calls to new subroutines get_met_xy and
!                     get_met_xyz and modified routines get_blvar and
!                     get_uvwt_ua
!******************************************************************************
 
! --- MODULES

use common_puf
use common_met
use get_met_inc

implicit none

! --- ARGUMENTS

real xp, yp       !Puff horizontal coordinates
real zp           !Zp is above reference height (hmin) if iflag = 0
                  !                 or local elevation if iflag = 1
real szz          !Szz = sigma-z ** 2
real zc           !Zc  = puff cap
real xfac, yfac   !Map factors
                                     
integer iflag     !Iflag = 0,1,2,or 3, used for gradients on a staggered grid
logical hazflag   !If the material is a Hazard material

! --- LOCALS

real divc, zx, sz

!------ set interpolation factors

call get_xyfac(xp,yp,xb(1),yb(1),dxb,dyb,nxb,nyb,xfac,yfac,mx,my,mxu,myv,lstagger)
call set_mxy(mx,my,mxy,nxb,nxyb)

!------ interpolate 2d boundary layer variables: zinv, u*, w*, zsl, h0 & dtdzs

call get_blvar(hazflag)

!---- 2-D cloud fields
call get_met_xy(xp,yp)

!------ set terrain variables

sz = sqrt(szz)
call set_topog(xp,yp,zp,sz,zx,zm,dp,hp,hx,hy,gx,gy,iflag)

!------ determine if using upper air or surface fields for interpolation

call get_uafac(sz,zc)
call set_flags(zx)

!------ get mean velocities, temperature and gradients

if (lbl_interp) then

  call get_uvwt_bl(xp,yp,zx)

else

  call get_uvwt_ua(xp,yp,zx)

end if

!---- 3-D cloud fields
call get_met_xyz(xp,yp,zp)

!------ compute small scale turbulence

call get_sstb(zx,hazflag)

!------ velocity gradients in terrain-following coordinates on a staggered grid

if (lter .and. lstagger) then

  call grad_ter(xp,yp,xfac,yfac)

end if

!------ compute ensemble (large scale & met uncertainty) turbulence

call get_ensm(xp,yp,xfac,yfac,hazflag)

!------ u* for dry deposition

call get_ustdep(us2, ws2, zruf, zinv, ustdep)

!------ insure zero divergence

divc = -0.5*(dudx + dvdy + dwdz)
dudx = dudx + divc
dvdy = dvdy + divc

!------ compute standard atmosphere pressure ratio

zm = zx + hmin          ! add hmin (above sea level)
call set_pressure(zm,pb)

!------ add terrain height to zinv

zinv = zinv + hp

!------ get absolute temperature from potential

tab = tb*(pb**0.285714)

return
end

subroutine int_xyz(mh,mv,f,maxd,fb,dfdx,dfdy,dfdz,dp,gx,gy,iflag)
!*******************************************************************************
!
! FUNCTION: 3D interpolation
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
use metintrp_inc

implicit none

! --- ARGUMENTS

integer maxd           !Dimension on 3D variable
integer iflag          !Used for gradients on a staggered grid
type ( meth ) mh       !Horizontal interpolation structure
type ( metv ) mv       !Vertical interpolation structure
real f(maxd,1)         !3-D variable being interpolated
real fb                !Interpolated value at desired location
real dfdx, dfdy, dfdz  !Derivatives at desired location
real dp                !Factor for terrain-following (dp = 1. - hp/zbtop)
real gx, gy            !Factors for derivatives for terrain-following

! --- LOCALS

integer kp, nxi, km, ij, kk

real v1, v2, v3, v4, v5, v6, v7, v8, vm, vp
real rat, ratm

km = max0(1,iabs(mv%km))

!------ check if horizontal grid is 2D

if (mh%nxyi > 1) then

  ij  = mh%ij
  nxi = mh%nxi

  v5  = f( ij      , km )
  v6  = f( ij+nxi  , km )
  v7  = f( ij+nxi+1, km )
  v8  = f( ij+1    , km )
  if (mv%facm /= 0.0) then
    kk = km + 1
    ratm = mv%facm
    rat  = 1.0 - ratm
    v5  = rat*v5 + ratm*f( ij      , kk )
    v6  = rat*v6 + ratm*f( ij+nxi  , kk )
    v7  = rat*v7 + ratm*f( ij+nxi+1, kk )
    v8  = rat*v8 + ratm*f( ij+1    , kk )
  end if
  vm  = mh%cc1*v5 + mh%cc2*v6 + mh%cc3*v7 + mh%cc4*v8

!------ check vertical grid; compute mean and vertical gradient

  if (mv%km > 0) then
    kp  = mv%kp
    v1  = f( ij     , kp )
    v2  = f( ij+nxi  , kp )
    v3  = f( ij+nxi+1, kp )
    v4  = f( ij+1   , kp )
    if (mv%facp /= 0.0) then
      kk = kp - 1
      ratm = mv%facp
      rat  = 1.0 - ratm
      v1  = rat*v1 + ratm*f( ij      , kk )
      v2  = rat*v2 + ratm*f( ij+nxi  , kk )
      v3  = rat*v3 + ratm*f( ij+nxi+1, kk )
      v4  = rat*v4 + ratm*f( ij+1    , kk )
    end if
    vp  = mh%cc1*v1 + mh%cc2*v2 + mh%cc3*v3 + mh%cc4*v4

    fb   = mv%rzm1*vm + mv%ratz*vp
    dfdz = (vp-vm)*mv%dzr / dp

  else

    v1 = v5
    v2 = v6
    v3 = v7
    v4 = v8

    fb   = vm
    dfdz = 0.

  end if

  if (iflag > 0) then

!------ compute horizontal gradients (and possibly re-compute vertical gradient)

    vm   = mv%ratz*(mh%rym1*v1 + mh%raty*v2) &
                                + mv%rzm1*(mh%rym1*v5 + mh%raty*v6)
    vp   = mv%ratz*(mh%raty*v3 + mh%rym1*v4) &
                                + mv%rzm1*(mh%raty*v7 + mh%rym1*v8)
    dfdx = (vp-vm)*mh%dxr

    vm   = mv%ratz*(mh%rxm1*v1 + mh%ratx*v4) &
                                + mv%rzm1*(mh%rxm1*v5 + mh%ratx*v8)
    vp   = mv%ratz*(mh%rxm1*v2 + mh%ratx*v3) &
                                + mv%rzm1*(mh%rxm1*v6 + mh%ratx*v7)
    dfdy = (vp-vm)*mh%dyr

    if (lstagger) then

!------- compute special gradients on staggered grid

      if (iflag == 1) then

        if (mv%ratz < 0.5) then
          if (mh%raty < 0.5) then
            dfdx = (v8-v5)*mh%dxr
          else
            dfdx = (v7-v6)*mh%dxr
          end if
        else
          if (mh%raty < 0.5) then
            dfdx = (v4-v1)*mh%dxr
          else
            dfdx = (v3-v2)*mh%dxr
          end if
        end if

      else if (iflag == 2) then

        if (mv%ratz < 0.5) then
          if (mh%ratx < 0.5) then
            dfdy = (v6-v5)*mh%dyr
          else
            dfdy = (v7-v8)*mh%dyr
          end if
        else
          if (mh%ratx < 0.5) then
            dfdy = (v2-v1)*mh%dyr
          else
            dfdy = (v3-v4)*mh%dyr
          end if
        end if

      else if (iflag == 3 .and. mv%km > 0) then

        if (mh%ratx < 0.5) then
          if (mh%raty < 0.5) then
            dfdz = (v1-v5)*mv%dzr
          else
            dfdz = (v2-v6)*mv%dzr
          end if
        else
          if (mh%raty < 0.5) then
            dfdz = (v4-v8)*mv%dzr
          else
            dfdz = (v3-v7)*mv%dzr
          end if
        end if

        dfdz = dfdz / dp

      end if            ! if (iflag==1,2,3)

    end if              ! if (lstagger)

!------ compute cartesian gradients in terrain-following coordinates
!       set zero horizontal gradients when mv%km = 0 or -1 (below lowest
!       grid level)

    if (lter) then
        dfdx = dfdx + gx*dfdz
        dfdy = dfdy + gy*dfdz
    end if

  else

    dfdx = 0.
    dfdy = 0.

  end if                ! if (iflag/=0)

else

!------ compute for single horizontal location

  vm  = f( 1, km )
  if (mv%km > 0) then
    if (mv%facm /= 0.0) then
      vm  = (1.0-mv%facm)*vm + mv%facm*f(1,km+1)
    end if
    vp   = f( 1, mv%kp )
    if (mv%facp /= 0.0) then
      vp  = (1.0-mv%facp)*vp + mv%facp*f(1,mv%kp-1)
    end if
    fb   = mv%rzm1*vm + mv%ratz*vp
    dfdz = (vp-vm)*mv%dzr
  else
    fb   = vm
    dfdz = 0.
  end if

  dfdx = 0.
  dfdy = 0.

end if

return
end

subroutine intd_xyz(mh,mv,dtdz,aa,bb,maxd,wb,dwdz,dp)
!*******************************************************************************
!
! FUNCTION:  3D interpolation for w and T
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use metintrp_inc

implicit none

! --- ARGUMENTS

integer maxd           !Dimension on 3D variable
real    aa(maxd,1)     !Part of numerator for vertical diffusivity
real    bb(maxd,1)     !Part of denominator for vertical diffusivity
real    wb             !W at desired location
real    dwdz           !Vertical velocity gradient at desired location
real    dtdz           !Temperature gradient at desired location
real    dp             !Factor for terrain-following (dp = 1. - hp/zbtop)

type ( meth ) mh       !Horizontal interpolation structure
type ( metv ) mv       !Vertical interpolation structure

! --- LOCALS

integer kp, km, kk, nxi, ij

real      a1, a2, a3, a4, a5, a6, a7, a8, am, ap
real      b1, b2, b3, b4, b5, b6, b7, b8, bm, bp
real      wm, wp, ratm, rat

km = max0(iabs(mv%km),1)

!------ check if horizontal grid is 2D

if (mh%nxyi > 1) then

  ij  = mh%ij
  nxi = mh%nxi

  a5 = aa( ij      , km )
  a6 = aa( ij+nxi  , km )
  a7 = aa( ij+nxi+1, km )
  a8 = aa( ij+1    , km )

  b5 = bb( ij      , km )
  b6 = bb( ij+nxi  , km )
  b7 = bb( ij+nxi+1, km )
  b8 = bb( ij+1    , km )

  if (mv%facm /= 0.0) then
    kk = km + 1
    ratm = mv%facm
    rat  = 1.0 - ratm
    a5  = rat*a5 + ratm*aa( ij      , kk )
    a6  = rat*a6 + ratm*aa( ij+nxi  , kk )
    a7  = rat*a7 + ratm*aa( ij+nxi+1, kk )
    a8  = rat*a8 + ratm*aa( ij+1    , kk )
    b5  = rat*b5 + ratm*bb( ij      , kk )
    b6  = rat*b6 + ratm*bb( ij+nxi  , kk )
    b7  = rat*b7 + ratm*bb( ij+nxi+1, kk )
    b8  = rat*b8 + ratm*bb( ij+1    , kk )
  end if

  am = mh%cc1*a5 + mh%cc2*a6 + mh%cc3*a7 + mh%cc4*a8
  bm = mh%cc1*b5 + mh%cc2*b6 + mh%cc3*b7 + mh%cc4*b8

  wm = am/(1.0 + bm*dtdz)

!------ check vertical grid; compute mean and vertical gradient

  if (mv%km > 0) then
    kp = mv%kp
    a1 = aa( ij      , kp )
    a2 = aa( ij+nxi  , kp )
    a3 = aa( ij+nxi+1, kp )
    a4 = aa( ij+1    , kp )

    b1 = bb( ij      , kp )
    b2 = bb( ij+nxi  , kp )
    b3 = bb( ij+nxi+1, kp )
    b4 = bb( ij+1    , kp )

    if (mv%facp /= 0.0) then
      kk = kp - 1
      ratm = mv%facp
      rat  = 1.0 - ratm
      a1  = rat*a1 + ratm*aa( ij      , kk )
      a2  = rat*a2 + ratm*aa( ij+nxi  , kk )
      a3  = rat*a3 + ratm*aa( ij+nxi+1, kk )
      a4  = rat*a4 + ratm*aa( ij+1    , kk )
      b1  = rat*b1 + ratm*bb( ij      , kk )
      b2  = rat*b2 + ratm*bb( ij+nxi  , kk )
      b3  = rat*b3 + ratm*bb( ij+nxi+1, kk )
      b4  = rat*b4 + ratm*bb( ij+1    , kk )
    end if

    ap = mh%cc1*a1 + mh%cc2*a2 + mh%cc3*a3 + mh%cc4*a4
    bp = mh%cc1*b1 + mh%cc2*b2 + mh%cc3*b3 + mh%cc4*b4

    wp   = ap/(1.0 + bp*dtdz)
    wb   = mv%rzm1*wm + mv%ratz*wp
    dwdz = (wp-wm)*mv%dzr / dp

  else

    wb   = wm
    dwdz = 0.

  end if

else

!------ compute for single horizontal location

  am = aa( 1, km )
  bm = bb( 1, km )
  if (mv%facm /= 0.0) then
    ratm = mv%facm
    rat  = 1.0 - ratm
    am   = rat*am + ratm*aa(1,km+1)
    bm   = rat*bm + ratm*bb(1,km+1)
  end if
  wm = am/min(1.0 + bm*dtdz,7.)

  if (mv%km > 0) then
    kp = mv%kp
    ap = aa( 1, kp )
    bp = bb( 1, kp )
    if (mv%facp /= 0.0) then
      ratm = mv%facp
      rat  = 1.0 - ratm
      ap   = rat*ap + ratm*aa(1,kp-1)
      bp   = rat*bp + ratm*bb(1,kp-1)
    end if
    wp = ap/min(1.0 + bp*dtdz,7.)

    wb   = mv%rzm1*wm + mv%ratz*wp
    dwdz = (wp-wm)*mv%dzr

  else

    wb   = wm
    dwdz = 0.

  end if

end if

return
end

subroutine get_xyfac(xp,yp,x1,y1,dx,dy,nx,ny,xfac,yfac,mx,my,mxu,myv,lstagger)
!*******************************************************************************
!
! FUNCTION:  Get X and Y 1D interpolation factors
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_1dfac
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use metintrp_inc

implicit none

! --- ARGUMENTS

integer nx, ny     !Number of grid points in the met grid

real    xp, yp     !Puff horizontal coordinates
real    x1, y1     !Nearest grid points
real    dx, dy     !Meteorological grid size
real    xfac, yfac !Map factors

logical lstagger   !Staggered grid flag

type ( met1dh ) mx, mxu, my, myv  !Interpolation structures

call get_1dfac(xp,x1,dx,nx,xfac,mx)
call get_1dfac(yp,y1,dy,ny,yfac,my)

if (lstagger) then

  mxu = mx
  if (mx%rat > 0.5) then
    mxu%rat = mx%rat - 0.5
  else
    mxu%rat = 0.5 + mx%rat
    mxu%i   = mx%i - 1
    if (mxu%i == 0) then
      mxu%i   = 1
      mxu%rat = 0.
    end if
  end if
  mxu%rm1 = 1.-mxu%rat

  myv = my
  if (my%rat > 0.5) then
    myv%rat = my%rat - 0.5
  else
    myv%rat = 0.5 + my%rat
    myv%i   = my%i - 1
    if (myv%i == 0) then
      myv%i   = 1
      myv%rat = 0.
    end if
  end if
  myv%rm1 = 1.-myv%rat

end if

return
end

subroutine get_1dfac(xp,x1,dx,nx,xfac,mx)
!*******************************************************************************
!
! FUNCTION:  Get 1D interpolation factor
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use metintrp_inc

implicit none

! --- ARGUMENTS

integer nx       !Number of grid points

real xp          !Puff centroid
real x1          !Nearest grid point
real dx          !Grid size
real xfac        !Map factor

type ( met1dh ) mx   !Interpolation structure

! --- LOCALS

integer ig

real ratx, rxm1, frac

if (nx > 1) then

  frac = (xp-x1)/dx
  frac = max(frac,0.0)
  frac = min(frac,float(nx-1)-1.e-3)
  ig   = int(frac) + 1
  ratx = frac - float(ig-1)
  rxm1 = 1. - ratx
  mx%dxr = xfac/dx

else

  ig   = 1
  ratx = 0.
  rxm1 = 1.
  mx%dxr = 0.

end if

mx%rm1 = rxm1
mx%rat = ratx
mx%i   = ig

return

end

subroutine get_zfac(zm,z,nz,mv,sz,zcap,lstagger)
!*******************************************************************************
!
! FUNCTION:  Get vertical interpolation factor
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use metintrp_inc

implicit none

! --- ARGUMENTS

integer nz        !Number of vertical grid points
real    zm        !Puff location
real    z(nz)      !Vertical grid 
real    sz        !Puff sigma-z
real    zcap      !Cap height
logical lstagger  !Staggered grid flag
type ( metv ) mv  !Interpolation structure

! --- LOCALS

integer km, kp

real zplus, zminus, zl, zu

mv%km   = 0
mv%kp   = 0
mv%dzr  = 0.
mv%ratz = 0.
mv%rzm1 = 1.
mv%facm = 0.
mv%facp = 0.

if (nz > 1) then

  zl = max(zm - sz,0.5*sz)

  if (zcap > 0.0) then
    zu = min(zm + sz, zcap)
  else
    zu = zm + sz
  end if

  if (zu <= z(1)) then
    mv%km   = -1
    mv%dzr  = 1.0/(z(2) - z(1))
  else if (zl > z(nz)) then
    mv%km   = -nz
    mv%dzr  = 1.0/(z(nz) - z(nz-1))
  else
    km = 2
    do while (z(km) < zl )
      km = km + 1
    end do
    km = km - 1
    kp = km
    do while (z(kp) < zu .and. kp < nz)
      kp = kp + 1
    end do

! ----- Use met points if adjacent
    if (kp-km == 1) then
      zplus  = z(kp)
      zminus = z(km)
    else
      mv%facp = (z(kp)-zu)/(z(kp)-z(kp-1))
      zplus = zu
      mv%facm = (zl-z(km))/(z(km+1)-z(km))
      zminus = zl
    end if

! ----- Use top point but assign zu-height
    if (zu >= z(kp)) then
      mv%facp = 0.0
      zplus = zu
    end if
! ----- Use bottom point but do not assign zl-height
    if (zl <= z(1)) then
      mv%facm = 0.0
      zminus = z(1)
    end if

    mv%dzr  = 1.0/(zplus - zminus)
    mv%km   = km
    mv%kp   = kp
    mv%ratz = (zm - zminus)*mv%dzr
    mv%ratz = max(0.,min(mv%ratz,1.0))
    mv%rzm1 = 1.0 - mv%ratz
  end if

end if

if (lstagger) then
  if (mv%km > 0) then
    mv%km = mv%km + 1
    mv%kp = mv%kp + 1
  else
    mv%km = mv%km - 1
    mv%kp = mv%kp - 1
  end if
end if

return

end

subroutine int_xy(mh,var,var0)
!*******************************************************************************
!
! FUNCTION:  Perform 2D interpolation
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use metintrp_inc

implicit none

! --- ARGUMENTS

real var(*)       !2D variable
real var0         !Interpolated value
type ( meth ) mh  !Interpolation structure

! --- LOCALS

integer nxi, ij

ij = mh%ij

if (mh%cc2+mh%cc3+mh%cc4 /= 0.) then

  nxi = mh%nxi

  var0 = mh%cc1*var( ij      ) + &
              mh%cc2*var( ij+nxi  ) + &
              mh%cc3*var( ij+nxi+1) + &
              mh%cc4*var( ij+1    )

else

  var0 = var(ij)

end if

return
end

subroutine nint_xy(mh,var,var0)
!*******************************************************************************
!
! FUNCTION:   Get a 2D value by finding the nearest grid point
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use metintrp_inc

implicit none

! --- ARGUMENTS

real var(*)       !2D variable
real var0         !Interpolated value
type ( meth ) mh  !Interpolation structure

! --- LOCALS

integer nxi, ij

real cc

nxi = mh%nxi
ij  = mh%ij
cc  = mh%cc1

if (mh%cc2 > cc) then
  ij = mh%ij + nxi
  cc = mh%cc2
end if

if (mh%cc3 > cc) then
  ij = mh%ij + nxi + 1
  cc = mh%cc3
end if

if (mh%cc4 > cc) then
  ij = mh%ij + 1
  cc = mh%cc4
end if

var0 = var(ij)

return
end

subroutine get_temp(xp,yp,zp,tp,tz,iflag)
!*******************************************************************************
!
! FUNCTION:  Get the temperature and vertical temp gradient at xp, yp and zp
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               set_topog               get_xyfac                 set_mxy
!                get_zfac                 int_xyz              stnd_atmos
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_met
use metintrp_inc

implicit none

! --- ARGUMENTS

real xp, yp, zp   !Location to find temperature, pressure and temp gradient
real tp, tz       !Interpolated temperature and vertical temperature gradient

integer iflag     !Zp is relative to reference height (hmin) if iflag = 0
                                        !or  local elevation if iflag = 1

! --- LOCALS

type ( met1dh ) m1, m2, m1u, m2v
type ( meth   ) m
type ( metv   ) mv

real zx, zm, dp, hp, hx, hy, gx, gy
real xfac, yfac, dum, dum1, dum2, t0

!------ get local elevation

call set_topog(xp,yp,zp,0.0,zx,zm,dp,hp,hx,hy,gx,gy,iflag)

!------ interpolate upper air profiles if available

if (tflag) then

  xfac = 1.
  yfac = 1.

  call get_xyfac(xp,yp,xb(1),yb(1),dxb,dyb,nxb,nyb,xfac,yfac, &
                      m1,m2,m1u,m2v,.false.)
  call set_mxy(m1,m2,m,nxb,nxyb)
  call get_zfac(zm,zb,nzb,mv,0.,0.,lstagger)
  call int_xyz(m,mv,t_ua,nxyb,tp,dum1,dum2,tz,dp,gx,gy,0)
  call int_xyz(m,mv,p_ua,nxyb,pb,dum1,dum2,dum2,dp,gx,gy,0)

! below not needed for pig-no extrapolation
!  if (zm > zb(nzb) .or. nzb == 1) then ! extrapolate with standard atmosphere
!    zm = zb(nzb) + hmin
!    call stnd_atmos(zm,dum,t0,tz,0)
!    tp = tp - t0
!    zm = zx + hmin
!    call stnd_atmos(zm,dum,t0,tz,0)
!    tp = tp + t0
!  end if

else

!------ else use standard atmosphere

  zm = zx + hmin                ! add hmin (above sea level)
  call stnd_atmos(zm,dum,tp,tz,0)

end if

return
end

subroutine get_gamma(xp,yp,zp,tz,iflag)
!*******************************************************************************
!
! FUNCTION: Get temperature gradient above inversion height
!           (assumes zinv is already set into zp)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               set_topog                get_zfac              stnd_atmos
!               get_xyfac                 set_mxy                 int_xyz
!
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES
use common_met
use metintrp_inc


implicit none

! --- ARGUMENTS

real xp, yp, zp   !Location to find vertical temperature gradient
real tz           !Vertical temperature gradient

integer iflag     !Zp is relative to reference height (hmin) if iflag = 0
                                        !or  local elevation if iflag = 1

! --- LOCALS

type ( met1dh ) m1, m2, m1u, m2v
type ( meth   ) m
type ( metv   ) mv

real zx, zm, dp, hp, hx, hy, gx, gy
real dum, dum1, dum2

integer km, kp

!------ get local elevation and set height for interpolation

call set_topog(xp,yp,zp,0.0,zx,zm,dp,hp,hx,hy,gx,gy,iflag)

!------ interpolate upper air profiles if available

if (tflag) then

  call get_zfac(zm,zb,nzb,mv,0.,0.,.false.)

!------ reset height to give gradient above zp (zinv)

  km = iabs(mv%km) + 1
  kp = km + 1

  if (km >= nzb) then ! extrapolate with standard atmosphere

    zm = zx + hmin              ! add hmin (above sea level)
    call stnd_atmos(zm,dum1,dum2,tz,0)

  else

    zm = 0.5*(zb(km)+zb(kp))

    call get_zfac(zm,zb,nzb,mv,0.,0.,lstagger)
    call get_xyfac(xp,yp,xb(1),yb(1),dxb,dyb,nxb,nyb,1.0,1.0, &
                      m1,m2,m1u,m2v,.false.)
    call set_mxy(m1,m2,m,nxb,nxyb)

    call int_xyz(m,mv,t_ua,nxyb,dum,dum1,dum2,tz,dp,gx,gy,0)

  end if

else

!------ else use standard atmosphere

  zm = zx + hmin                ! add hmin (above sea level)
  call stnd_atmos(zm,dum1,dum2,tz,0)

end if

return
end

subroutine zero_w
!*******************************************************************************
!
! FUNCTION:   Set the vertical velocity to zero
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
use get_met_inc

implicit none

wb   = 0.
if (lter) then
  if (zm < zbtop) &
      wb   = (ub*hx + vb*hy)*(1.-zm/zbtop)
end if

dwdx = 0.0
dwdy = 0.0
dwdz = 0.0

return
end

subroutine grad_ter(xp,yp,xfac,yfac)
!*******************************************************************************
!
! FUNCTION: Compute "special" gradients on staggered grid based on definition
!           of divergence
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use common_met
use get_met_inc

implicit none

! --- ARGUMENTS

real xp, yp       !Puff location
real xfac, yfac   !Map factors

! --- LOCALS

integer i, j, kp, km, is, ip, im

real tem1, tem2, cz, dxi, dyi

if ( mzw%km > 0) then

  dxi = xfac/dxb
  dyi = yfac/dyb

  i  = min0(mxu%i+1,nxb)
  j  = min0(myv%i+1,nyb)
  is = (j-1)*nxb + i

  if (mz%km > 0) then

    km = mz%km
    kp = mz%kp

    ip = (kp-1)*nxyb + is
    im = (km-1)*nxyb + is

    tem1 = (d1(is)*u_ua(im)-d1(is-1)*u_ua(im-1))*dxi
    tem2 = (d1(is)*u_ua(ip)-d1(is-1)*u_ua(ip-1))*dxi
    dudx = mz%ratz*tem1 + mz%rzm1*tem2

    tem1 = (d1(is)*v_ua(im)-d1(is-nxb)*v_ua(im-nxb))*dyi
    tem2 = (d1(is)*v_ua(ip)-d1(is-nxb)*v_ua(ip-nxb))*dyi
    dvdy = mz%ratz*tem1 + mz%rzm1*tem2

  else

  km = iabs(mz%km)
  im = (km-1)*nxyb + is

  dudx = (d1(is)*u_ua(im)-d1(is-1)*u_ua(im-1))*dxi
  dvdy = (d1(is)*v_ua(im)-d1(is-nxb)*v_ua(im-nxb))*dyi

  end if

  km = mzw%km
  kp = mzw%kp

  ip = (kp-1)*nxyb + is
  im = (km-1)*nxyb + is

  cz = mzw%dzr

  tem1 = ddx(is  )*d1(is  )*(u_ua(im  )+u_ua(im+nxyb  )) &
            + ddx(is-1)*d1(is-1)*(u_ua(im-1)+u_ua(im-1+nxyb))
  tem1 = tem1*0.25*zz(km)/d(is)
  tem2 = ddx(is  )*d1(is  )*(u_ua(ip  )+u_ua(ip+nxyb  )) &
            + ddx(is-1)*d1(is-1)*(u_ua(ip-1)+u_ua(ip-1+nxyb))
  tem2 = tem2*0.25*zz(kp)/d(is)
  dudx = (dudx - (tem2-tem1)*cz/zbtop) / d(is)

  tem1 = ddy(is    )*d2(is    )*(v_ua(im    )+v_ua(im+nxyb    )) &
            + ddy(is-nxb)*d2(is-nxb)*(v_ua(im-nxb)+v_ua(im-nxb+nxyb))
  tem1 = tem1*0.25*zz(km)/d(is)
  tem2 = ddy(is    )*d2(is    )*(v_ua(ip    )+v_ua(ip+nxyb    )) &
            + ddy(is-nxb)*d2(is-nxb)*(v_ua(ip-nxb)+v_ua(ip-nxb+nxyb))
  tem2 = tem2*0.25*zz(kp)/d(is)
  dvdy = (dvdy - (tem2-tem1)*cz/zbtop) / d(is)

  dwdz = (w_ua(ip)-w_ua(im))*cz / d(is)

else

  dudx = 0.
  dvdy = 0.
  dwdz = 0.

end if

return

end

subroutine set_topog(xp,yp,zp,sz,zx,zm,dp,hp,hx,hy,gx,gy,iflag)
!*******************************************************************************
!
! FUNCTION: Set topography-related variables
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
use common_met

implicit none

! --- ARGUMENTS

real xp, yp, zp, sz   !Puff centroid location and sigma-z
real zx               !Mean sea level height
real zm               !Terrain following height
real dp               !Factor for terrain-following (dp = 1. - hp/zbtop)
real hp               !Terrain height
real hx, hy           !Terrain gradients
real gx, gy           !Used to transform flat gradients

integer iflag         !Zp is above reference height (hmin) if iflag = 0
                      !                 or local elevation if iflag = 1

! --- LOCALS

real zzm              

zx = zp

if (lter) then

  call get_topog(xp,yp,hp,hx,hy)
  dp = 1. - hp/zbtop
  if (iflag == 1) then
    zx = zx + hp
  end if
  if (zx < hp) zx = 2.*hp - zx
  zx  = max(zx,hp+sz)
  zx  = max(zx,hp+zruf)
  zm  = (zx-hp)/dp
  zzm =  min(zm/zbtop,1.) - 1.
  gx  = zzm*hx
  gy  = zzm*hy

else

  hp = 0.
  hx = 0.
  hy = 0.
  dp = 1.
  zx = max(abs(zx),sz)
  zx = max(zx,zruf)
  zm = zx
  gx = 0.
  gy = 0.

end if

return

end

subroutine get_uafac(sz,zc)
!*******************************************************************************
!
! FUNCTION: Get vertical interpolation factors for upper air fields
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                get_zfac
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_met
use get_met_inc

implicit none

! --- ARGUMENTS

real sz   !Puff sigma-z
real zc   !Puff capping height

! --- LOCALS

real szx, zcx

szx = sz/dp
zcx = (zc-hp)/dp

call get_zfac(zm,zb,nzb,mz,szx,zcx,lstagger)

if (wflag .and. lstagger) then
  call get_zfac(zm,zbw,nzb,mzw,szx,zcx,.false.)
end if

return

end

subroutine set_flags(zp)
!*******************************************************************************
!
! FUNCTION:   Set "below inversion" and "within surface layer" flags
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
use get_met_inc

implicit none

! --- ARGUMENTS

real zp     !Puff centroid height

! --- LOCALS

logical lprof, lua_in_bl

real zh

!------ if not doing PROF boundary layer, set "below inversion" and
!       "within surface layer" flags

lprof = bl_type == 'PROF'

zh = zp - hp

if (lbl .and. .not.lprof) then
  lzinv = zinv > 0. .and. zh <= zinv
  lsl   = zh <= zsl .and. lzinv
else
  lzinv = .false.
  lsl   = .false.
end if

!------ use boundary/surface layer fields to interpolate velocity, etc. if
!       below zinv (implying lbl==true) and vertical grid does not resolve
!       boundary layer (below ZINV) or surface layer (below ZSL if lsl==true)
!       or below lowest grid level

lua_in_bl = .false.
if ( lzinv .and. nzb > 1 ) then
  lua_in_bl = ( lsl .and. zb(2)*dp <= zsl ) .or. &
              ( .not.lsl .and. zb(2)*dp <= zinv ) 
end if

lbl_interp = (lzinv .and. .not.lua_in_bl) .or. nzb == 1 &
                     .or. zh < zb(1)*dp

if (lprof) lsl = lbl_interp

return

end

subroutine get_uvwt_bl(xp,yp,zp)
!*******************************************************************************
!
! FUNCTION:   Interpolate u, v, w and t using boundary layer/surface fields
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 set_mxy                 int_xyz                  zero_w
!              stnd_atmos               sat_humid                    ulog
!                   uzlog
!
! REVISION HISTORY: 
!
! 29 NOV 2000 : Fix temperature for surface obs only - DSH
!*******************************************************************************

! --- MODULES

use common_met
use get_met_inc

implicit none

! --- ARGUMENTS

real xp, yp, zp  !Puff centroid location

! --- LOCALS

type ( meth ) m
type ( metv ) mv

integer icld
real ufac, utot, ubl, alp
real ulog, uzlog, zh
real dum, dum1, dum2, pfac, hsx

!------ set vertical interpolation structure for horizontal interpolation

mv%km   = 0
mv%kp   = 0
mv%dzr  = 0.
mv%ratz = 0.
mv%rzm1 = 1.
mv%facm = 0.
mv%facp = 0.

!------ velocity at z >= surface layer height

if (lstagger) then
  call set_mxy(mxu,my,m,nxb,nxyb)
  call int_xyz(m,mv,u_sl,nxyb,ub,dudx,dudy,dudz,dp,gx,gy,1)
  call set_mxy(mx,myv,m,nxb,nxyb)
  call int_xyz(m,mv,v_sl,nxyb,vb,dvdx,dvdy,dvdz,dp,gx,gy,2)
else
  call int_xyz(mxy,mv,u_sl,nxyb,ub,dudx,dudy,dudz,dp,gx,gy,1)
  call int_xyz(mxy,mv,v_sl,nxyb,vb,dvdx,dvdy,dvdz,dp,gx,gy,2)
end if

!------ if in surface layer, scale velocity components and gradients

if (lsl) then

  zh   = zp - hp
  utot = sqrt(ub*ub + vb*vb)
  alp  = 1. - (zh/zsl)**2

  if (utot > 0.) then

!------ direction cosines

    ub   = ub / utot
    vb   = vb / utot

!------ surface layer wind speed and vertical gradient

    ufac = utot / ulog(zsl,zruf,xml)    ! ustar/vonk
    ubl  = ufac * ulog(zh,zruf,xml)     ! surface layer |u|
    dudz = ufac * uzlog(zh,zruf,xml)    ! ustar/vonk * phi/z (d|u|/dz)

!------ interpolate between surface layer and uniform wind above

    ubl  = (1.-alp)*utot + alp*ubl
    dudz = 2.*zh*(utot-ubl)/zsl**2 + alp*dudz

!------- wind and gradient components along x,y directions
!       (the order of the nex 4 statements is important!)

    dvdz = dudz*vb
    dudz = dudz*ub

    ub   = ubl*ub
    vb   = ubl*vb

!------- scale horizontal gradients

    ufac = ubl / utot
    dudx = ufac*dudx
    dudy = ufac*dudy
    dvdx = ufac*dvdx
    dvdy = ufac*dvdy

  end if

end if

call zero_w

!------ temperature & pressure

call get_temp(xp,yp,zp,tb,dtdz,0)

!------ humidity

if (hflag) then
  call int_xyz(mxy,mv,h_bl,nxyb,hb,dum,dum1,dum2,dp,gx,gy,0)
else
  call stnd_atmos(zp+hmin,pfac,dum1,dum2,1)
  pfac = 1013.25*pfac
  call sat_humid(tb,pfac,hsx,0)
  hb = 0.3*hsx                  ! default is 30% rel. humidity
end if

!------ cloud liquid water content

cldall = 0.
if (cldflag) then
  do icld = 1, ncld
    call int_xyz(mxy,mv,cld_bl(1,icld),nxyb,clwc(icld), &
                                   dum,dum1,dum2,dp,gx,gy,0)
    cldall = cldall + clwc(icld)
  end do
end if

!------ temperature gradient:  interpolate in surface layer

if (dtdzs > 0) then

  if (lsl) then

    dtdz  = alp*dtdzs + (1.0 - alp)* dtdz

  end if

else if (lzinv) then

  dtdz = 0.

end if

dtdz = max(dtdz,0.)

return

end

subroutine get_uvwt_ua(xp,yp,zp)
!*******************************************************************************
!
! FUNCTION:   Get u, v, w and t from upper-air fields
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 set_mxy                 int_xyz                    zero_w
!              stnd_atmos               sat_humid
!
! REVISION HISTORY: 
!
!*******************************************************************************

! --- MODULES

use common_met
use get_met_inc

implicit none

! --- ARGUMENTS

real xp, yp, zp  !Puff centroid location

! --- LOCALS

type ( meth ) m

integer icld
real dtdx, dtdy, ztmp, t0, p0, pr_stnd, dum, hsx, dum1, dum2

!------ velocity

if (lstagger) then
  call set_mxy(mxu,my,m,nxb,nxyb)
  call int_xyz(m,mz,u_ua,nxyb,ub,dudx,dudy,dudz,dp,gx,gy,1)
  call set_mxy(mx,myv,m,nxb,nxyb)
  call int_xyz(m,mz,v_ua,nxyb,vb,dvdx,dvdy,dvdz,dp,gx,gy,2)
  if (wflag) then
    call int_xyz(mxy,mzw,w_ua,nxyb,wb,dwdx,dwdy,dwdz,dp,gx,gy,3)
  else
    call zero_w
  end if
else
  call int_xyz(mxy,mz,u_ua,nxyb,ub,dudx,dudy,dudz,dp,gx,gy,1)
  call int_xyz(mxy,mz,v_ua,nxyb,vb,dvdx,dvdy,dvdz,dp,gx,gy,2)
  if (wflag) then
    call int_xyz(mxy,mz,w_ua,nxyb,wb,dwdx,dwdy,dwdz,dp,gx,gy,3)
  else
    call zero_w
  end if
end if

!------ temperature & pressure

if (tflag) then
  call int_xyz(mxy,mz,p_ua,nxyb,pb,dum,dum,dum,dp,gx,gy,0)
  call int_xyz(mxy,mz,t_ua,nxyb,tb,dtdx,dtdy,dtdz,dp,gx,gy,0)
!below not needed for pig-no extrapolation
!  if (zm > zb(nzb) .or. nzb == 1) then     ! extrapolate with standard atmosphere
!    ztmp = zb(nzb) + hmin
!    call stnd_atmos(ztmp,p0,t0,dtdz,0)
!    tb   = tb - t0
!    pb   = pb - p0
!    ztmp = zp + hmin
!    call stnd_atmos(ztmp,p0,t0,dtdz,0)
!    tb   = tb + t0
!    pb   = pb + p0
!  end if
else
  ztmp = zp + hmin
  call stnd_atmos(ztmp,pb,tb,dtdz,0)
end if

if (lzinv) then
  if (dtdzs > 0.) then
    dtdz = max(dtdz,0.01*dtdzs)
  else
    dtdz = 0.
  end if
else
  dtdz = max(dtdz,0.)
end if

!------ humidity

if (hflag) then
  call int_xyz(mxy,mz,h_ua,nxyb,hb,dum,dum1,dum2,dp,gx,gy,0)
else
  ztmp = zp + hmin
  call stnd_atmos(ztmp,pr_stnd,dum1,dum2,1)
  pr_stnd = pr_stnd*1013.25
  call sat_humid(tb,pr_stnd,hsx,0)
  hb = 0.3*hsx                  ! default is 30% rel. humidity
end if

!------ cloud liquid water content

! - sum all cloud droplet sizes
cldall = 0.
if (cldflag) then
  do icld = 1, ncld
    call int_xyz(mxy,mz,cld_ua(1,icld),nxyb,clwc(icld), &
                                   dum,dum1,dum2,dp,gx,gy,0)
    cldall = cldall + clwc(icld)
  end do
end if

if (zm < zb(1) .or. zm > zb(nzb)) then
  dudz = 0.
  dvdz = 0.
end if

return

end

subroutine get_ensm(xp,yp,xfac,yfac,hazflag)
!*******************************************************************************
!
! FUNCTION:    Get large-scale variability
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 set_mxy                  int_xy                 int_xyz
!               get_xyfac                get_zfac
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_met
use get_met_inc

implicit none

! --- ARGUMENTS

real xp, yp       !Horizontal puff coordinates
real xfac, yfac   !Map factors

logical hazflag   !Hazard material flag

! --- LOCALS

type ( met1dh ) m1, m2, m1u, m2v
type ( meth   ) m
type ( metv   ) mv

real uubx, uuby, vvbx, vvby, uvbx, uvby
real uux, uuy, vvx, vvy, uvx, uvy
real dum, dum1, dum2, fac, den

!------ initialize to zero

uubz = 0.
vvbz = 0.
uvbz = 0.
uub  = 0.
vvb  = 0.
uvb  = 0.
sby  = 1.e5

uu_haz  = 0.
vv_haz  = 0.
uv_haz  = 0.
uuz_haz = 0.
vvz_haz = 0.
uvz_haz = 0.
sb_haz  = 1.e5

!------ large-scale variability

if (lensm) then

  if (ensm_type == 'INPUT') then

    uub = uu_lsv(1)
    sby = sl_lsv(1)
    vvb = uub
    uvb = 0.

  else if (ensm_type == 'MODEL') then

    m1%rm1 = 1.
    m1%rat = 0.
    m1%i   = 1
    m1%dxr = 0.
    call set_mxy(m1,my,m,1,nyb)
    call int_xy(m,uu_lsv,uub)
    call int_xy(m,sl_lsv,sby)
    vvb  = uub
    uvb  = 0.

  else if (ensm_type == 'OBS') then

    call int_xyz(mxy,mz,uue_ua,nxyb,uub,uubx,uuby,uubz,dp,gx,gy,0)
    call int_xyz(mxy,mz,vve_ua,nxyb,vvb,vvbx,vvby,vvbz,dp,gx,gy,0)
    call int_xyz(mxy,mz,uve_ua,nxyb,uvb,uvbx,uvby,uvbz,dp,gx,gy,0)
    sby = sl_lsv(1)
  end if

  sb_lsv = sby

end if

!------ met uncertainty

if (hazflag) then

  call get_xyfac(xp,yp,xbe(1),ybe(1),dxe,dye, &
                     nxe,nye,xfac,yfac,m1,m2,m1u,m2v,.false.)
  call set_mxy(m1,m2,m,nxe,nxye)

  call get_zfac(zm,zbe,nzbe,mv,0.,0.,.false.)
  call int_xyz(m,mv,uue_ua,nxye,uu_haz,uux,uuy,uuz_haz,dp,gx,gy,0)
  call int_xyz(m,mv,vve_ua,nxye,vv_haz,vvx,vvy,vvz_haz,dp,gx,gy,0)
  call int_xyz(m,mv,uve_ua,nxye,uv_haz,uvx,uvy,uvz_haz,dp,gx,gy,0)
  if (lhaz_obs .and. met_type /= 'CLI3D') then
    sb_haz = sl_haz
  else
    call int_xyz(m,mv,sle_ua,nxye,sb_haz,dum,dum1,dum2,dp,gx,gy,0)
  end if

!------ combine large-scale variability and uncertainty

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

  if (uvb**2 > 0.81*uub*vvb) then
    fac  = 0.9*sqrt(uub*vvb)/abs(uvb)
    uvb  = fac*uvb
    uvbz = fac*uvbz
  end if

end if

return

end

subroutine get_sstb(zp,hazflag)
!*******************************************************************************
!
! FUNCTION:    Get boundary layer scale turbulence variables
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                 int_xyz                intd_xyz                     f33
!                     g33                     phi                    phiz
!
!
! REVISION HISTORY: 
!
! 29 NOV 2000 : Analytical vertical diffusivity - DSH
!*******************************************************************************
 
! --- MODULES

use common_puf
use common_met
use get_met_inc

implicit none

! --- ARGUMENTS

real zp          !Vertical puff coordinate

logical hazflag  !Hazard material flag

! --- LOCALS

type ( metv ) mt
type ( meth ) m

real frac, dum, dum1, dum2, amet, bmet, zarg, btem
real ustr, zs, phi, phiz

integer i

if (lzinv) then

!------ boundary layer turbulence

  mt%dzr = 1.0/(dzbl*zinv)
  frac   = (zp-hp)*mt%dzr

  zarg = frac*dzbl
    
  call blturb(zarg,zinv,us2,ws2,uubl,vvbl,wwbl,qqs,sbl,sbz, &
                                      amet,bmet,wtbl,zruf)
  btem = bmet*dtdz
  difb = amet / (1. + btem)
  wtbl = max(wtbl*(1.0-frac*dzbl),0.)

  if (frac >= 1.) then
    frac    = min(frac,float(nzbl-2))
    mt%km   = min(int(frac)+1,nzbl-2)
    mt%kp   = mt%km + 1
    mt%ratz = frac - float(mt%km-1)
    mt%rzm1 = 1.0 - mt%ratz
    mt%facm = 0.0
    mt%facp = 0.0
    call intd_xyz(mxy,mt,dtdz,aa_bl,bb_bl,nxyb,dum,dddz,1.)
  else
    dddz = difb / max((zp-hp),zruf)
    dddz = dddz * ( (amet+bmet*wtbl)/amet - 2.*btem/(1.+btem) ) 
  end if

else if (bl_type == 'PROF') then

!------ profile interpolation

  m = mxy

  call int_xyz(m,mz,uu_ua,1,uubl,dum,dum1,dum2,1.,0.,0.,0)
  call int_xyz(m,mz,vv_ua,1,vvbl,dum,dum1,dum2,1.,0.,0.,0)
  call int_xyz(m,mz,ww_ua,1,wwbl,dum,dum1,dum2,1.,0.,0.,0)
  call int_xyz(m,mz,sl_ua,1, sbl,dum,dum1,dum2,1.,0.,0.,0)
  call int_xyz(m,mz,sz_ua,1, sbz,dum,dum1,dum2,1.,0.,0.,0)
  call int_xyz(m,mz,qq_ua,1, qqs,dum,dum1,dum2,1.,0.,0.,0)
  call int_xyz(m,mz,wt_ua,1,wtbl,dum,dum1,dum2,1.,0.,0.,0)

  call intd_xyz(m,mz,dtdz,aa_ua,bb_ua,1,difb,dddz,1.)

  if (lsl) then
    ustr = sqrt(us2)
    zs   = zb(1)
    difb = vonk*ustr*zp/phi(zp,xml) * difb/(vonk*ustr*zs/phi(zs,xml))
    dddz = difb*(1.-phiz(zp,xml))/zp
    sbz  = 0.65*(zp+zruf)
  end if

else

!------ stable region above boundary layer

  i = (nzbl-1)*nxybl + 1
  uubl = uu_bl(i)
  vvbl = vv_bl(i)
  wwbl = ww_bl(i)
  sbl  = sl_bl(i)
  sbz  = sz_bl(i)
  qqs  = qq_bl(i)
  difb = aa_bl(i)/(1.0+bb_bl(i)*dtdz)
  dddz = 0.0
  wtbl = 0.0

end if

!------ set horizontal bl scale for shear component

wwbh = wwbl
sbls = sbz

return

end

subroutine get_ustdep(us2, ws2, zruf, zinv, ustdep)
!*******************************************************************************
!
! FUNCTION:  Set ustar for deposition calculations 
!            (includes term in case of free convection conditions)
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

real us2, ws2   !u* squared and w* squared
real zruf       !Roughness height
real zinv       !Inversion height
real ustdep     !u* for dry deposition calculations

! --- LOCALS

real fac

if (ws2 > 0.0) then
  fac = 0.46*(zruf/zinv)**0.16
  ustdep = sqrt(us2 + fac*fac*ws2)
else
  ustdep = sqrt(us2)
end if

return

end

subroutine get_radyn(ustar, xml, z1, zruf, radyn)
!******************************************************************************
!
! FUNCTION:  Calculates aerodynamic resistance for deposition calculations 
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
! Version 1.0, PKK, AER, January 2005, based on routines used in MCIP 2.3
!
!******************************************************************************
 
implicit none

! --- PBL Constants; Hogstrom (1988) 
REAL,          PARAMETER     :: VKAR  =  0.40
REAL,          PARAMETER     :: BETAH =  8.21
REAL,          PARAMETER     :: GAMAH = 11.60
REAL,          PARAMETER     :: PRO   =  0.95

! --- ARGUMENTS
real ustar      !Friction velocity (m/s)
real xml        !MO length (m)
real z1         !Height of first layer (m)
real zruf       !Roughness height (m)
real radyn      !aerodynamic resistance (s/m)

! --- LOCALS
real z1ol, zntol, alogz1z0, psih0, psih

z1ol     = z1 / xml
zntol    = zruf / xml
alogz1z0 = ALOG(z1/zruf)

if ( z1ol >= 0.0 ) then

  if ( z1ol > 1.0 ) then
    psih0 = 1.0 - BETAH - z1ol
  else
    psih0 = -BETAH * z1ol
  end if

  if ( zntol > 1.0 ) then
     psih = psih0 - (1.0 - BETAH - zntol)
  else
     psih = psih0 + BETAH * zntol
  end if

else

  psih = 2.0 * ALOG( (1.0 + SQRT(1.0 - GAMAH*z1ol)) /  &
                           (1.0 + SQRT(1.0 - GAMAH*zntol)) )

end if

radyn = PRO * ( alogz1z0 - psih ) / ( VKAR * ustar )

return

end

subroutine get_blvar(hazflag)
!******************************************************************************
!
! FUNCTION:  Get boundary layer variables
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                  int_xy                 nint_xy             set_pratebl
!                set_prbl               get_radyn
!
! REVISION HISTORY: 
!   Updated March 2005 for APT (PKK, AER)
!      Added aerodynamic resistance
!      Added cloud variables
!******************************************************************************
 
! --- MODULES

use common_puf
use common_met
use get_met_inc
use multcomp_inc

implicit none

! --- ARGUMENTS

logical hazflag   !Hazard material flag

! --- LOCALS

integer i
real dep

real ustr, zs

if (lbl) then                 

  if (bl_type == 'PROF') then
    zinv   = zi_bl(1)
    zruf   = zruf2(1)
    us2    = ustr2(1)
    ws2    = wstr2(1)
    dtdzs  = dtdz2(1)
    xml    = xmol2(1)
    zsl    = z_sl(1)
    fcc    = fcc_bl(1)
    fprcpc = fprcpc_bl(1)
    cldtop = cldtop2(1)
    cldbot = cldbot2(1)
    wcbar  = lwc2(1)

!------ aerodynamic resistance for dry deposition
    ustr = sqrt(us2)
    zs   = zb(1)
    call get_radyn(ustr, xml, zs, zruf, radyn)

  else

    call int_xy(mxy,zi_bl,zinv)
    call int_xy(mxy,zruf2,zruf)
    call int_xy(mxy,hflx_bl,wtbl)
    call int_xy(mxy,ustr2,us2)
    call int_xy(mxy,wstr2,ws2)
    call int_xy(mxy,z_sl,zsl)
    call int_xy(mxy,dtdz2,dtdzs)
    call int_xy(mxy,xmol2,xml)
    call int_xy(mxy,cldtop2,cldtop)
    call int_xy(mxy,cldbot2,cldbot)
    call int_xy(mxy,radyn2,radyn)
    if (multicomp .and. vdep2d(1,1) /= NOT_SET_R) then
      do i = 1, nspecies
        call int_xy(mxy,vdep2d(1,i),dep)
        species(i)%vdep = dep
      end do
    end if
    if (lwash) then
      if (lprcp)  then
        call nint_xy(mxy,prcp_bl,prbl)
        call set_pratebl(prbl,pratebl)
      else if (lprate) then
        call int_xy(mxy,prate_bl,pratebl)
        call set_prbl(pratebl,prbl)
      end if
    end if
    if (lfcc)   call int_xy(mxy,fcc_bl,fcc)
    call int_xy(mxy,lwc2,wcbar)
  end if

else                  !Boundary layer type = 'NONE'

  zinv  = 0.
  wtbl  = 0.
  us2   = 0.
  ws2   = 0.
  dtdzs = 0.
  prbl  = 0.
  pratebl = 0.
  cldtop  = 0.
  cldbot  = 0.
  fcc = 0.
  fprcpc = 0.
  radyn = 1.0E30
  wcbar = 0.

end if

return

end

subroutine set_pratebl(prbl,pratebl)
!*******************************************************************************
!
! FUNCTION: Set precipitation rate from precipitation index
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

real pratebl   !precipitation rate  (mm/hr)
real prbl      !precipitation index

! --- LOCALS

integer ityppr

!-----  Precipitation index
!-----  I = 0  --> PR =   0.0 mm/hr     !RAIN
!-----  I = 1  --> PR =   0.5 mm/hr
!-----  I = 2  --> PR =   3.5 mm/hr
!-----  I = 3  --> PR =  25.0 mm/hr
!-----  I = 4  --> PR =   5.0 mm/hr     !SNOW
!-----  I = 5  --> PR =  20.0 mm/hr
!-----  I = 6  --> PR = 100.0 mm/hr

ityppr = nint(prbl)
if (ityppr == 1) then
  pratebl = 0.5
else if (ityppr == 2) then
  pratebl = 3.5
else if (ityppr == 3) then
  pratebl = 25.
else
  pratebl = 0. !no snow in pratebl
end if

return
end

subroutine set_prbl(pratebl,prbl)
!*******************************************************************************
!
! FUNCTION:  Set precipitation index from precipitation rate
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- ARGUMENTS

real pratebl   !precipitation rate  (mm/hr)
real prbl      !precipitation index

! --- LOCALS

integer ityppr

!-----  Precipitation index
!-----  I = 0  --> PR =   0.0 mm/hr     !RAIN
!-----  I = 1  --> PR =   0.5 mm/hr
!-----  I = 2  --> PR =   3.5 mm/hr
!-----  I = 3  --> PR =  25.0 mm/hr
!-----  I = 4  --> PR =   5.0 mm/hr     !SNOW
!-----  I = 5  --> PR =  20.0 mm/hr
!-----  I = 6  --> PR = 100.0 mm/hr

if (pratebl >= 0.49999 .and. pratebl < 3.49999) then
  ityppr = 1
else if (pratebl >= 3.49999 .and. pratebl < 24.9999) then
  ityppr = 2
else if (pratebl >= 24.9999) then
  ityppr = 3
else
  ityppr = 0
end if

prbl = float(ityppr)

return
end

subroutine get_zi(xp,yp,xfac,yfac)
!*******************************************************************************
!
! FUNCTION:   Get the inversion height at the puff location
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_xyfac                 set_mxy                  int_xy
!               get_topog
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_puf
use common_met
use get_met_inc

implicit none

! --- ARGUMENTS

real xp, yp       !Horizontal puff coordinates
real xfac, yfac   !Map factors

! --- LOCALS

type ( met1dh ) m1, m2, m1u, m2v
type ( meth   ) m

if (lbl) then

  if (bl_type == 'PROF') then
    zinv  = zi_bl(1)
    ws2   = wstr2(1)
    dtdzs = dtdz2(1)
  else
    call get_xyfac(xp,yp,xb(1),yb(1),dxb,dyb,nxb,nyb,xfac,yfac, &
                        m1,m2,m1u,m2v,lstagger)
    call set_mxy(m1,m2,m,nxb,nxyb)
    call int_xy(m,zi_bl,zinv)
    call int_xy(m,wstr2,ws2)
    call int_xy(m,dtdz2,dtdzs)
  end if

else                 !No boundary layer

  zinv  = 0.
  dtdzs = 0.

end if

if (lter) then
  call get_topog(xp,yp,hp,hx,hy)
  zinv = zinv + hp    !Include terrain height
end if

return

end

logical function BL_capped(xp,yp)
!*******************************************************************************
!
! FUNCTION:  Determine if the boundary layer is capped by the inversion layer
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!               get_xyfac                 set_mxy                  int_xy
!               get_gamma
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use common_met
use get_met_inc

implicit none

! --- ARGUMENTS

real xp, yp      !Horizontal puff coordinates

! --- LOCALS

type ( met1dh ) m1, m2, m1u, m2v
type ( meth   ) m

real tz

if (lbl) then

  if (bl_type == 'PROF') then
    dtdzs = dtdz2(1)
    zinv  = zi_bl(1)
  else
    call get_xyfac(xp,yp,xb(1),yb(1),dxb,dyb,nxb,nyb,1.,1., &
                        m1,m2,m1u,m2v,lstagger)   !map factors are irrelevant
    call set_mxy(m1,m2,m,nxb,nxyb)
    call int_xy(m,dtdz2,dtdzs)
    call int_xy(m,zi_bl,zinv)
  end if

  call get_gamma(xp,yp,zinv,tz,0)

  BL_capped = dtdzs <= 0. .and. tz > 0.

else

  BL_capped = .false.

end if

return

end

subroutine set_mxy(m1,m2,m,nx,nxy)
!*******************************************************************************
!
! FUNCTION:  Set horizontal interpolation structures
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES

use metintrp_inc

implicit none

! --- ARGUMENTS

integer nx       !Number of 1D grid points
integer nxy      !Number of 2D grid points


type ( met1dh ) m1, m2   !1D horizontal interpolation structures
type ( meth ) m          !2D horizontal interpolation structure

m%cc1 = m1%rm1*m2%rm1
m%cc2 = m1%rm1*m2%rat
m%cc3 = m1%rat*m2%rat
m%cc4 = m1%rat*m2%rm1

m%ratx = m1%rat
m%rxm1 = m1%rm1
m%raty = m2%rat
m%rym1 = m2%rm1

m%nxyi = nxy
m%nxi  = nx
m%ij   = (m2%i-1)*nx + m1%i

m%dxr  = m1%dxr
m%dyr  = m2%dxr

return

end

subroutine sat_humid(theta,p,hs,iflag)
!*******************************************************************************
!
! FUNCTION: Set the saturation mass mixing ratio
!           From fits to Smithsonian Meteorological Tables 89 and 94 given in
!           Appendix A of A. Gill (1982), Atmosphere-Ocean Dynamics,
!           Academic Press, NY

!------    input: theta = pot. temp. (K) (iflag=0) or temp. (K) (iflag=1);
!                 p = pressure (mb)

!------    output: hs = 
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

real    theta     !Potential temperature or temperature (K)
real    p         !Pressure (mb)
real    hs        !saturation mass mixing ratio (output)
integer iflag     !temperature flag
                  !pot. temp. (K) (iflag=0) or temp. (K) (iflag=1)

! --- PARAMETERS

real, parameter :: MR = 0.62197   ! ratio molecular wts of water to air
real, parameter :: KAPPA = 0.285714
real, parameter :: PSURF = 1013.25     ! from U.S. Standard Atmos.

! --- LOCALS

real t, e, f

!------ set temperature in Celsius

if (iflag == 0) then
  t = theta*(p/PSURF)**KAPPA - 273.15
else
  t = theta - 273.15
end if

!------ saturation vapor pressure

if (t < -100.) then
  hs = 0.
  return
end if

e = (0.7859 + 0.03477*t) / (1. + 0.00412*t)
if (t <= 0.) then
  e = e + 0.00422*t
end if
f = 1. + (4.5 + 0.0006*t*t)*p*1.e-6
e = f*10.**e

!------ saturation mixing ratio

!       hs = MR*e / (p - (1.-MR)*e) this is specific humidity
hs = MR*e/max(p - e,1.e-20)

return

end
