subroutine set_blturb
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION: Set small scale turbulence in boundary layer; 
!           set fixed values for stable region above
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                set_hflx                 set_zsl                 set_usl
!                  blturb                dtdz_fun
!
! REVISION HISTORY: 
!
!  Minor F90 Updates, Feb. 2004 (PKK, AER)
!
!  Use constants from constants_fd (in common_puf), April 2007, PK, AER,
!  based on update to SCICHEM by BC, L3COM, 01/31/2007 
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met

implicit none

! --- LOCALS

real hflux, soq, zi, zt, dtdz_fun, qqtrop, zbl

integer i, k, i0, is

!------ set heat flux

call set_hflx

!------ set surface layer height & velocity

call set_zsl

call set_usl

!------ set surface layer temperature gradient

do i = 1,nxybl
  zt       = max(0.5*zi_bl(i),z_sl(i))
  dtdz2(i) = dtdz_fun(hflx_bl(i),ustr2(i),zt,xmol2(i),zruf2(i),lpgt)
end do

!------ vertical profiles

do is = 1,nxybl
  if( xmol2(is) > 0. )then
    zi_bl(is) = min(zi_bl(is),5.*xmol2(is))
    z_sl(is)  = min(z_sl(is),zi_bl(is))
  end if
  zi = zi_bl(is)
  if (zi > 0.) then
    do k = 1,nzbl-1
      i = (k-1)*nxyb + is
      hflux = hflx_bl(is)
      zbl = float(k-1)*dzbl
      call blturb(zbl,zi,ustr2(is),wstr2(is), &
                       uu_bl(i),vv_bl(i),ww_bl(i),qq_bl(i), &
                       sl_bl(i),sz_bl(i),aa_bl(i),bb_bl(i), &
                       hflux,zruf2(is))
    end do
  end if
end do

!------ set tropopause values above inversion

i0 = (nzbl-1)*nxybl
do is = 1,nxybl
  i = i0 + is
  ww_bl(i) = wwtrop
  uu_bl(i) = wwtrop
  vv_bl(i) = uu_calm
  sz_bl(i) = sltrop
  sl_bl(i) = sl_calm
  qqtrop   = 3.*wwtrop
  soq      = sz_bl(i)/sqrt(max(qqtrop,1.e-6))
  qq_bl(i) = qqtrop
  aa_bl(i) = soq/a*wwtrop
  bb_bl(i) = 2.*EQF*soq**2
end do

9999  return

end

subroutine set_hflx
!******************************************************************************
!
! FUNCTION:  Set the surface heat flux from ustar and L
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met

implicit none

! --- LOCALS

real gt0, ustar

integer i

do i = 1, nxyb
  ustar = sqrt(ustr2(i))
  gt0 = G0/t_bl(i)
  hflx_bl(i) = -ustar**3/(vonk*gt0*xmol2(i))
end do

return

end

subroutine set_zsl
!******************************************************************************
!
! FUNCTION:  Set surface layer height based on L and Zi
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!  Minor F90 Updates, Feb. 2004 (PKK, AER)
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met

implicit none

! --- PARAMETERS
 
real, parameter :: ALP0 = 0.1, DALP = 0.9, R1   = 5.0

! --- LOCALS

integer i
real    alp, r

do i = 1,nxybl

  if (xmol2(i) <= 0.) then

!------ unstable or neutral

    z_sl(i) = ALP0*zi_bl(i)

  else

!------ stable

    r   = zi_bl(i)/xmol2(i)
    alp = ALP0 + DALP*r/(r+R1)
    z_sl(i) = min(alp*zi_bl(i),5.*xmol2(i))

  end if

  z_sl(i) = max(z_sl(i),10.*zruf2(i))
  z_sl(i) = min(z_sl(i),zi_bl(i))

end do

return

end

subroutine set_usl
!******************************************************************************
!
! FUNCTION:  Set velocity at surface layer height
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!                    ulog
!
! REVISION HISTORY: 
!
!******************************************************************************
 
! --- MODULES
 
use common_puf
use common_met

implicit none

! --- LOCALS

integer i
real    ufac, ulog, ustar, wspd

do i = 1,nxybl

  if (z_sl(i) > zref) then
    ufac     = ulog(z_sl(i),zruf2(i),xmol2(i))
    ustar    = sqrt(ustr2(i))
    wspd     = sqrt(u_bl(i)**2 + v_bl(i)**2)
    u_sl(i)  = ustar*ufac*u_bl(i)/(vonk*wspd)
    v_sl(i)  = ustar*ufac*v_bl(i)/(vonk*wspd)
  else
    u_sl(i)  = u_bl(i)
    v_sl(i)  = v_bl(i)
  end if

end do

return

end
