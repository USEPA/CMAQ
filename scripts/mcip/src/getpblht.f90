
!***********************************************************************
!   Portions of Models-3/CMAQ software were developed or based on      *
!   information from various groups: Federal Government employees,     *
!   contractors working on a United States Government contract, and    *
!   non-Federal sources (including research institutions).  These      *
!   research institutions have given the Government permission to      *
!   use, prepare derivative works, and distribute copies of their      *
!   work in Models-3/CMAQ to the public and to permit others to do     *
!   so.  EPA therefore grants similar permissions for use of the       *
!   Models-3/CMAQ software, but users are requested to provide copies  *
!   of derivative works to the Government without restrictions as to   *
!   use by others.  Users are responsible for acquiring their own      *
!   copies of commercial software associated with Models-3/CMAQ and    *
!   for complying with vendor requirements.  Software copyrights by    *
!   the MCNC Environmental Modeling Center are used with their         *
!   permissions subject to the above restrictions.                     *
!***********************************************************************

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/work/rep/MCIP2/src/mcip2/getpblht.F,v 1.3 2007/08/03 20:48:55 tlotte Exp $ 


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
  REAL                         :: e_aerk
  REAL                         :: f_ptemp
  REAL                         :: f_vtemp
  REAL                         :: fract1
  REAL                         :: fract2
  INTEGER                      :: k
  INTEGER                      :: ktop
  REAL                         :: press
  REAL                         :: psurf
  REAL                         :: qmix
  REAL                         :: qratio
  REAL                         :: qsurf
  INTEGER,       INTENT(IN)    :: r
  REAL                         :: rhumid
  REAL,    SAVE, ALLOCATABLE   :: ribk       ( : )
  REAL                         :: temp
  REAL                         :: tempc
  REAL                         :: tsurf
  REAL                         :: vpress
  REAL                         :: vptemp
  REAL,    SAVE, ALLOCATABLE   :: vptempk    ( : )
  REAL                         :: vptsurf
  REAL                         :: vsat
  REAL,          INTENT(IN)    :: wspd       ( : )
  REAL                         :: zpbl

!-------------------------------------------------------------------------------
! Statement functions.
!-------------------------------------------------------------------------------

  ! Saturation vapor pressure [Pa]

  e_aerk(tempc) = vp0 * EXP( 17.625 * tempc / ( 243.04 + tempc ) )


  ! Virtual temperature [K]

  f_vtemp(temp,qmix) = temp * ( 1.0 + 0.6077 * qmix )


  ! Potential temperature [K]

  f_ptemp(temp,press) = temp * ( 100000.0 / press )**(2.0/7.0)

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

  vptsurf = f_vtemp( tsurf, qsurf )
  vptsurf = f_ptemp( vptsurf, psurf )

  qratio  = xdenswm(c,r,1) / ( xdensam(c,r,1) - xdenswm(c,r,1) )

  vptemp  = f_vtemp( xtempm(c,r,1), qratio )
  vptemp  = f_ptemp( vptemp, xpresm(c,r,1) )

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

    vptempk(k) = f_vtemp( xtempm(c,r,k), qratio )
    vptempk(k) = f_ptemp( vptempk(k), xpresm(c,r,k) )

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
