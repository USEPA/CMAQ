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
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
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
