!*******************************************************************************
!$RCSfile: settle.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
subroutine settle(i,zdos,sr,zrfac)
!*******************************************************************************
! Copyright 1994, The Titan Corporation. All rights reserved.
!
! FUNCTION:   Compute settling rate
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!        get_smooth_topog            puff_reflect
!
! REVISION HISTORY: 
!
!*******************************************************************************
 
! --- MODULES
 
use common_puf
use refl_inc

implicit none

! --- ARGUMENTS
 
real zdos  !height to calculate dose
real sr    !settling rate
real zrfac !reflection factor
integer i  !puff number

! --- LOCALS

real sx, sy, sz, zrefl, zp, ztest, xr, rat, faci, cmax
real vfac, xs, ys, zs, arg, tem, h, hx, hy, facs, conc_min

integer ityp

dimension xr(3)


ityp     = puff(i)%ityp
conc_min = material(typeID(ityp)%imat)%prop(3)
cmax     = puff(i)%cc/puff(i)%c

if (cmax < conc_min) then
  sr = 0.0
  return
end if

if (lter) then
  sx    = sqrt(puff(i)%sxx)
  sy    = sqrt(puff(i)%syy)
  call get_smooth_topog(puff(i)%xbar,puff(i)%ybar,h,hx,hy,sx,sy)
  zp    = puff(i)%zbar - (h+zdos)
  ztest = zp - zrfac*(abs(hx)*sx + abs(hy)*sy)
else
  zp    = puff(i)%zbar - zdos
  ztest = zp
  h     = 0.
end if

if (puff(i)%zbar <= puff(i)%zc .and. puff(i)%zc > 0.0 &
               .and. zdos+h > puff(i)%zc) then
  sr = 0.0
  return
end if

sz = sqrt(puff(i)%szz)

if (ztest < zrfac*sz) then

  if (lter) then
    zs = puff(i)%zbar - h
    call puff_reflect(zs,zp,puff(i),hx,hy,.true.,xr,vfac)
    vfac = pi/(pi3*sqrt(puff(i)%det)) * vfac
  else
    deth = puff(i)%axx*puff(i)%ayy - puff(i)%axy**2
    zs = -zp
    xs = -zs*(puff(i)%axz*puff(i)%ayy-puff(i)%ayz*puff(i)%axy)/deth
    ys = -zs*(puff(i)%ayz*puff(i)%axx-puff(i)%axz*puff(i)%axy)/deth
    arg = -(puff(i)%azz*zs*zs-puff(i)%axx*xs*xs-puff(i)%ayy*ys*ys &
             -2.*puff(i)%axy*xs*ys)
    if (arg > -30.0) then
      tem = exp(arg)
      if (tem*cmax < 0.25*conc_min) then     ! allow for reflections
        sr = 0.0
        return
      end if
    else
      tem = 0.0
    end if
    vfac = tem*pi/(pi3*sqrt(puff(i)%det*deth))          !Integral at z=zdos
    if(zdos == 0.0)then
      facs = 1.
    else
      zs   = -puff(i)%zbar
      facs = exp(0.5*zs*zdos/(puff(i)%det*deth))          !Surface reflection factor
    end if
    vfac = vfac * (1.+facs)
  end if

!------ compute inversion reflection factor

  zrefl = puff(i)%zc
  if( puff(i)%zbar > puff(i)%zc .or. puff(i)%zc ==0.0)then
    faci = 0.
  else
    rat = 0.5/(puff(i)%det*deth)
    arg = (zrefl-(h+zdos))*(zrefl-puff(i)%zbar)*rat
    faci = exp(-arg)
  end if
  vfac = vfac * (1.+faci)

!------ set centerline value

  sr = vfac*puff(i)%c

else

  sr = 0.0

end if

return
end


subroutine puff_reflect(zs,zp,p,hx,hy,flag,xr,vfac)
!*******************************************************************************
!
! FUNCTION:  Get the reflection factor
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
 
use struct_inc

implicit none

! --- ARGUMENTS
 
real zs       !puff height relative to the ground
real zp       !puff height relative to the dose height
real hx, hy   !terrain gradients
real xr(3)    !reflected position
real vfac     !reflection factor
logical flag  !compute surface integral factor

type ( puff_str ) p  !puff structure

real asig(7)

call get_asig(p,asig)

call reflect(zs,zp,asig,hx,hy,flag,xr,vfac)

return
end
