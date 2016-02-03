
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
! $Header: /project/work/rep/MCIP2/src/mcip2/close_files.F,v 1.1.1.1 2002/03/09 14:47:59 yoj Exp $ 


SUBROUTINE close_files

!-------------------------------------------------------------------------------
! Name:     Close Files
! Purpose:  Close I/O API files.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           09 Jan 2002  Changed "stop" statements to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!-------------------------------------------------------------------------------

  USE iodecl3

  IMPLICIT NONE

  CHARACTER*16,  PARAMETER     :: pname      = 'CLOSE_FILES'

  IF ( .NOT. shut3() ) THEN
    WRITE (6,9000)
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CLOSE_FILES',                        &
              /, 1x, '***   COULD NOT CLOSE I/O API OUTPUT FILES',         &
              /, 1x, 70('*'))

END SUBROUTINE close_files
