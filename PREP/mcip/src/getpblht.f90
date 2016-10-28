!------------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in           !
!  continuous development by various groups and is based on information        !
!  from these groups: Federal Government employees, contractors working        !
!  within a United States Government contract, and non-Federal sources         !
!  including research institutions.  These groups give the Government          !
!  permission to use, prepare derivative works of, and distribute copies       !
!  of their work in the CMAQ system to the public and to permit others         !
!  to do so.  The United States Environmental Protection Agency                !
!  therefore grants similar permission to use the CMAQ system software,        !
!  but users are requested to provide copies of derivative works or            !
!  products designed to operate in the CMAQ system to the United States        !
!  Government without restrictions as to use by others.  Software              !
!  that is used with the CMAQ system but distributed under the GNU             !
!  General Public License or the GNU Lesser General Public License is          !
!  subject to their copyright restrictions.                                    !
!------------------------------------------------------------------------------!

SUBROUTINE getpblht (c, r, wspd)

!-------------------------------------------------------------------------------
! Name:     Get PBL Height
! Purpose:  Calculates PBL height from bulk Richardson number.
! Notes:    This routine is used when PBL from input meteorology is 0.0.
!           Bulk Richardson number calculation taken from Byun's pblpkg.F
!           subroutine.  PBL height algorithm from Alapaty's VMM for MM5.
! Revised:  19 Sep 2001  Original version.  (T. Otte)
!           08 Jan 2002  Adjusted minimum PBL height to be height of lowest
!                        mid-layer rather than height of lowest full layer.
!                        (T. Otte)
!           12 Jul 2004  Added provision to specify PBL height if KTOP is 1.
!                        Changed local array allocation to occur only on
!                        initial call to subroutine to avoid memory
!                        fragmentation.  (T. Otte)
!           09 Apr 2007  Added IMPLICIT NONE.  Added explicit declarations for
!                        E_AERK, QSURF, and DTEMP.  Modified calculations of
!                        DTMPV, DTEMP, and RIBK to remove dependencies on
!                        function CVMGZ.  (T. Otte)
!           01 Sep 2011  Replaced statement functions with external routines.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE const
  USE xvars

  IMPLICIT NONE

  REAL                         :: apbl
  INTEGER,       INTENT(IN)    :: c
  REAL                         :: densd
  REAL                         :: densw
  REAL                         :: dtemp
  REAL,          PARAMETER     :: dtempmax   =  5.0 ! max sfc_T - air_T
  REAL,          PARAMETER     :: dtempmin   = -3.0 ! min sfc_t - air_T
  REAL                         :: dtmpv
  REAL,          EXTERNAL      :: e_aerk
  REAL                         :: fract1
  REAL                         :: fract2
  INTEGER                      :: k
  INTEGER                      :: ktop
  REAL                         :: psurf
  REAL,          EXTERNAL      :: ptemp
  REAL                         :: qratio
  REAL                         :: qsurf
  INTEGER,       INTENT(IN)    :: r
  REAL                         :: rhumid
  REAL,    SAVE, ALLOCATABLE   :: ribk       ( : )
  REAL                         :: tsurf
  REAL                         :: vpress
  REAL                         :: vptemp
  REAL,    SAVE, ALLOCATABLE   :: vptempk    ( : )
  REAL                         :: vptsurf
  REAL                         :: vsat
  REAL,          EXTERNAL      :: vtemp
  REAL,          INTENT(IN)    :: wspd       ( : )
  REAL                         :: zpbl

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( ribk    ) ) ALLOCATE ( ribk    ( metlay ) )
  IF ( .NOT. ALLOCATED ( vptempk ) ) ALLOCATE ( vptempk ( metlay ) )

!-------------------------------------------------------------------------------
! Compute difference of virtual temperature.  Algorithm taken from Byun's
! pblpkg.F.
!-------------------------------------------------------------------------------

  ! Saturation vapor pressure over water [mb]

  ! Estimating water vapor at surface with TSURF = 0.5*(TEMPM(1)+TEMPG)
  ! assuming it has the same relative humidity as the layer 1 air,
  ! but at the reference temperature

  vsat    = e_aerk( xtempm(c,r,1) - stdtemp )
  vpress  = xdenswm(c,r,1) * rwvap * xtempm(c,r,1)

  rhumid  = vpress / vsat

  psurf   = xpresm(c,r,1) + grav*xdensam(c,r,1)*x3htm(c,r,1)  ! [Pa]
  tsurf   = 0.5 * ( xtempm(c,r,1) + xtempg(c,r) )
  vsat    = e_aerk( tsurf - stdtemp )                         ! [Pa]

  vpress  = vsat * rhumid
  densw   = vpress / ( rwvap * tsurf )
  densd   = xdensaf(c,r,0)

  qsurf   = densw / densd

  ! Virtual temperature of ground and air above ground

  vptsurf = vtemp( tsurf, qsurf )
  vptsurf = ptemp( vptsurf, psurf )

  qratio  = xdenswm(c,r,1) / ( xdensam(c,r,1) - xdenswm(c,r,1) )

  vptemp  = vtemp( xtempm(c,r,1), qratio )
  vptemp  = ptemp( vptemp, xpresm(c,r,1) )

  dtmpv   = vptsurf - vptemp
  IF ( dtmpv < dtempmin ) dtmpv = dtempmin
  IF ( dtmpv > dtempmax ) dtmpv = dtempmax
  IF ( dtmpv == 0.0 ) THEN
    dtmpv = 1.0e-10
  ENDIF

!-------------------------------------------------------------------------------
! Calculate bulk Richardson number.  Algorithm taken from Byun's pblpkg.F.
!-------------------------------------------------------------------------------

  ribloop: DO k = 1, metlay

    vptempk(k) = vtemp( xtempm(c,r,k), qratio )
    vptempk(k) = ptemp( vptempk(k), xpresm(c,r,k) )

    dtemp = - ( dtmpv + vptempk(1) - vptempk(k) )
    IF ( dtemp == 0.0 ) THEN
      dtemp = 1.0e-10
    ENDIF

    ribk(k) = grav * x3htm(c,r,k) * dtemp / ( tsurf * wspd(k)**2 )
    IF ( ribk(k) == 0.0 ) THEN
      ribk(k) = -1.0e-10
    ENDIF

    IF ( ribk(k) >= 0.25 ) THEN
      ktop = k
      EXIT ribloop
    ENDIF

  ENDDO ribloop

!-------------------------------------------------------------------------------
! Calculate PBL height.  Algorithm adapted from Alapaty's VMM for Blackadar
! in MM5.
!-------------------------------------------------------------------------------

  IF ( ktop /= 1 ) THEN
    fract1 = ( 0.25 - ribk(ktop-1) ) / ( ribk(ktop) - ribk(ktop-1) )
    fract2 = 1.0 - fract1
    apbl   = x3htm(c,r,ktop) * fract1
    zpbl   = apbl + ( x3htm(c,r,ktop) * fract2 )
  ELSE
    zpbl   = x3htm(c,r,ktop)
  ENDIF

  IF ( zpbl < x3htm(c,r,1) ) THEN
    xpbl(c,r) = x3htm(c,r,1)
  ELSE
    xpbl(c,r) = zpbl
  ENDIF

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

! DEALLOCATE ( ribk    )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( vptempk )  ! commented out to avoid memory fragmentation

END SUBROUTINE getpblht
