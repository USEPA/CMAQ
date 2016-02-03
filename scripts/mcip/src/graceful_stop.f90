
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
! $Header: /project/work/rep/MCIP2/src/mcip2/graceful_stop.F,v 1.1.1.1 2002/03/09 14:48:01 yoj Exp $ 


SUBROUTINE graceful_stop (pname)

!-------------------------------------------------------------------------------
! Name:     Graceful Stop
! Purpose:  Gracefully stop program and close I/O API files.
! Revised:  09 Jan 2002  Original version.  (T. Otte)
!-------------------------------------------------------------------------------

  USE date_time

  IMPLICIT NONE

  INTEGER,       PARAMETER     :: m3stat    = 6247  ! MCIP error
  CHARACTER*16,  INTENT(IN)    :: pname
  CHARACTER*80                 :: xmsg

  xmsg = 'ABNORMAL TERMINATION IN ' // TRIM(pname)
  CALL m3exit (pname, sdate, stime, xmsg, m3stat)

END SUBROUTINE graceful_stop
