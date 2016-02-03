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

SUBROUTINE getsdt (hdate, sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Get SDATE and STIME
! Purpose:  Compute SDATE and STIME from MM5-type (and WRF-type) date.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           12 Feb 2010  Removed GMT from argument list and calculations.
!                        (T. Otte)
!           30 Aug 2011  Changed F77 character declarations to F90 standard.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER                           :: dd
  CHARACTER(LEN=24),  INTENT(IN)    :: hdate    ! YYYY_MO_DD-HH:MI:SS.SSSS
  INTEGER                           :: hh
  INTEGER                           :: jjj
  INTEGER,            EXTERNAL      :: julian
  INTEGER                           :: mi
  INTEGER                           :: mo
  INTEGER,            INTENT(OUT)   :: sdate    ! YYYYJJJ
  INTEGER,            INTENT(OUT)   :: stime    ! HHMISS
  INTEGER                           :: yyyy

  READ ( hdate(1:4),   '(i4.4)' ) yyyy
  READ ( hdate(6:7),   '(i2.2)' ) mo
  READ ( hdate(9:10),  '(i2.2)' ) dd
  READ ( hdate(12:13), '(i2.2)' ) hh
  READ ( hdate(15:16), '(i2.2)' ) mi

  jjj   = julian (yyyy, mo, dd)

  sdate = ( yyyy * 1000 ) + jjj
  stime = ( hh * 10000 ) + ( mi * 100 )      ! assume seconds are 0

END SUBROUTINE getsdt
