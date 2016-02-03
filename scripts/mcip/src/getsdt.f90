
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
! $Header: /project/work/rep/MCIP2/src/mcip2/getsdt.F,v 1.1.1.1 2002/03/09 14:48:01 yoj Exp $ 


SUBROUTINE getsdt (hdate, sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Get SDATE and STIME
! Purpose:  Compute SDATE and STIME from MM5-type (and WRF-type) date.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           12 Feb 2010  Removed GMT from argument list and calculations.
!                        (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER                      :: dd
  CHARACTER*24,  INTENT(IN)    :: hdate    ! YYYY_MO_DD-HH:MI:SS.SSSS
  INTEGER                      :: hh
  INTEGER                      :: jjj
  INTEGER,       EXTERNAL      :: julian
  INTEGER                      :: mi
  INTEGER                      :: mo
  INTEGER,       INTENT(OUT)   :: sdate    ! YYYYJJJ
  INTEGER,       INTENT(OUT)   :: stime    ! HHMISS
  INTEGER                      :: yyyy

  READ ( hdate(1:4),   '(i4.4)' ) yyyy
  READ ( hdate(6:7),   '(i2.2)' ) mo
  READ ( hdate(9:10),  '(i2.2)' ) dd
  READ ( hdate(12:13), '(i2.2)' ) hh
  READ ( hdate(15:16), '(i2.2)' ) mi

  jjj   = julian (yyyy, mo, dd)

  sdate = ( yyyy * 1000 ) + jjj
  stime = ( hh * 10000 ) + ( mi * 100 )      ! assume seconds are 0

END SUBROUTINE getsdt
