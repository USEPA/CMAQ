
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
! $Header: /project/work/rep/MCIP2/src/mcip2/julian.F,v 1.1.1.1 2002/03/09 14:48:01 yoj Exp $ 


FUNCTION julian (year, mnth, mday)

!-------------------------------------------------------------------------------
! Name:     Julian Day
! Purpose:  Returns the Julian day (1...365,366) corresponding to the date
!           MNTH-MDAY-YEAR.  NOTE:  This is NOT the Julian DATE -- only the
!           day-number.  To get the Julian date:
!              JDATE = 1000 * YEAR  +  JULIAN ( YEAR , MNTH , MDAY )
! Revised:  ?? May 1988  Modified for ROMNET.  (???)
!           ?? Aug 1990  Modified for ROM 2.2:  improved comments; improved
!                        algorithm using IF-THEN-ELSE IF construction. (???)
!           10 Sep 2001  Converted to free-form f90.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER                      :: julian   ! Julian day from arguments
  INTEGER                      :: l
  INTEGER                      :: m
  INTEGER,       INTENT(IN)    :: mday     ! day of month [1-31]
  INTEGER,       INTENT(IN)    :: mnth     ! month of year [1-12]
  INTEGER                      :: n
  INTEGER,       INTENT(IN)    :: year     ! four-digit calendar year

  m = MOD((mnth + 9), 12)
  n = (m * 153 + 2) / 5 + mday + 58

  IF ( MOD(year, 4) /= 0 ) THEN
    l = 365
  ELSE IF ( MOD(year, 100) /= 0 ) THEN
    l = 366
    n = 1 + n
  ELSE IF ( MOD(year, 400) /= 0 )  THEN
    l = 365
  ELSE 
    l = 366
    n = 1 + n
  END IF

  julian = 1 + MOD(n, l)

END FUNCTION julian
