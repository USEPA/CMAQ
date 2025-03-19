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

SUBROUTINE graceful_stop (pname)

!-------------------------------------------------------------------------------
! Name:     Graceful Stop
! Purpose:  Gracefully stop program and close I/O API files.
! Revised:  09 Jan 2002  Original version.  (T. Otte)
!           30 Aug 2011  Changed F77 character declarations to F90 standard.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE date_time

  IMPLICIT NONE

  INTEGER,            PARAMETER     :: m3stat    = 6247  ! MCIP error
  CHARACTER(LEN=16),  INTENT(IN)    :: pname
  CHARACTER(LEN=80)                 :: xmsg

  xmsg = 'ABNORMAL TERMINATION IN ' // TRIM(pname)
  CALL m3exit (pname, sdate, stime, xmsg, m3stat)

END SUBROUTINE graceful_stop
