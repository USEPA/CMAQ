
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


SUBROUTINE wind (u, v, wspd, wdir, xlon, xlonc, xn)

!-------------------------------------------------------------------------------
! Name:     Calculate Wind from Components
! Purpose:  Calculate wind speed and direction from components.
! Revised:  01 Apr 2002  Original version in VEROBS.  (T. Otte)
!           30 Apr 2004  Modified for MCIP.  (T. Otte)
!           09 Apr 2007  Removed dependency on module CONST_METE and added
!                        dependency on module CONST.  Explicitly declared
!                        RAD2DEG, which was in CONST_METE.  (T. Otte)
!           12 Feb 2010  Corrected computation of wind direction when
!                        U-component wind is effectively zero.  Simplified
!                        code by eliminating unnecessary IF/THEN/ELSE block.
!                        Error and solution suggested by Talat Odman.  (T. Otte)
!-------------------------------------------------------------------------------

  USE const

  IMPLICIT NONE

  REAL                         :: diff
  REAL,          PARAMETER     :: rad2deg = REAL (1.8d2 / pi)
  REAL,          INTENT(IN)    :: u
  REAL,          INTENT(IN)    :: v
  REAL,          INTENT(OUT)   :: wdir
  REAL,          INTENT(OUT)   :: wspd
  REAL,          INTENT(IN)    :: xlon
  REAL,          INTENT(IN)    :: xlonc
  REAL,          INTENT(IN)    :: xn

!-------------------------------------------------------------------------------
! Compute wind speed by simple triangulation of components.
!-------------------------------------------------------------------------------

  wspd = SQRT ( ( u * u ) + ( v * v ) )

!-------------------------------------------------------------------------------
! Find wind direction using simple trigonometry.
! Modify wind direction so that it is earth-relative.
!-------------------------------------------------------------------------------

  wdir = 270.0 - ( ATAN2(v,u) * rad2deg )

  diff = (xlonc - xlon) * xn
  IF (diff >  180.0) diff = diff - 360.0
  IF (diff < -180.0) diff = diff + 360.0

  wdir = wdir - diff

  IF (wdir > 360.0) wdir = wdir - 360.0
  IF (wdir <   0.0) wdir = wdir + 360.0

END SUBROUTINE wind
