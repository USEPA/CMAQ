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

SUBROUTINE getversion

!-------------------------------------------------------------------------------
! Name:     Get Version
! Purpose:  Get version of MM5 by reading first character of file.
! Notes:    Assume input file will either be MM5v1/v2 or MM5v3 format.
! Revised:  06 Apr 2000  Original version.  (T. Otte)
!           17 Sep 2001  Updated with MCIP-specific commands.  (T. Otte)
!           09 Jan 2002  Changed "stop" statements to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           09 Apr 2007  Added error condition for MM5v2-formatted files.
!                        (T. Otte)
!           29 Aug 2011  Changed name of module FILE to FILES to avoid conflict
!                        with F90 protected intrinsic.  Improved error
!                        handling.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE files
  USE metinfo

  IMPLICIT NONE

  INTEGER                           :: ifirst
  INTEGER                           :: istat
  CHARACTER(LEN=16),  PARAMETER     :: pname     = 'GETVERSION'

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   MM5v2-FORMATTED DATA ARE NO LONGER SUPPORTED', &
    & /, 1x, '***     - CONSIDER CONVERTING DATA SET', &
    & /, 1x, '***       TO MM5v3 FORMAT USING UTILITY CODE', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   DOES NOT SEEM TO BE MM5 OUTPUT', &
    & /, 1x, '***   FIRST CHARACTER IN FILE (IFIRST) = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9200 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR READING MM5 FILE ON UNIT ', i4, &
    & /, 1x, '***   IOSTAT = ', i4, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Read first character of MM5 input file.  It should be an integer, if it's
! MM5v1/v2 or MM5v3 output format.  If MM5v1/v2, the first character is the
! program ID (MIF(1,1)), which is 1 or greater.  If MM5v3, the first character
! is the "big header flag", which is 0.
!-------------------------------------------------------------------------------

  READ (iutmm, IOSTAT=istat) ifirst
  IF ( istat == 0 ) THEN
    IF ( ifirst == 0 ) THEN      ! MM5v3 format
      met_iversion = 3
      WRITE (*, '(1x, a)') '- MM5v3 HEADER'
    ELSE IF ( ifirst > 0 ) THEN  ! MM5v2 format
      WRITE (*,f9000) TRIM(pname)
      CALL graceful_stop (pname)
    ELSE
      WRITE (*,f9100) TRIM(pname), ifirst
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    WRITE (*,f9200) TRIM(pname), iutmm, istat
    CALL graceful_stop (pname)
  ENDIF

  REWIND (iutmm)

END SUBROUTINE getversion
