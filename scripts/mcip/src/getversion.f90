
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
! $Header: /project/work/rep/MCIP2/src/mcip2/getversion.F,v 1.2 2007/08/03 20:49:09 tlotte Exp $ 


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
!-------------------------------------------------------------------------------

  USE file
  USE metinfo

  IMPLICIT NONE

  INTEGER                      :: ifirst
  INTEGER                      :: istat
  CHARACTER*16,  PARAMETER     :: pname     = 'GETVERSION'

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
      WRITE (6,9000)
      GOTO 1001
    ELSE
      WRITE (6,9100) ifirst
      GOTO 1001
    ENDIF
  ELSE
    WRITE (6,9200) iutmm, istat
    GOTO 1001
  ENDIF

  REWIND (iutmm)

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GETVERSION',                         &
              /, 1x, '***   MM5v2-FORMATTED DATA ARE NO LONGER SUPPORTED', &
              /, 1x, '***     - CONSIDER CONVERTING DATA SET',             &
              /, 1x, '***       TO MM5v3 FORMAT USING UTILITY CODE',       &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GETVERSION',                         &
              /, 1x, '***   DOES NOT SEEM TO BE MM5 OUTPUT',               &
              /, 1x, '***   FIRST CHARACTER IN FILE (IFIRST) = ', i4,      &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GETVERSION',                         &
              /, 1x, '***   ERROR READING MM5 FILE ON UNIT ', i4,          &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE getversion
