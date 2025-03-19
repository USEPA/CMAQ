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

SUBROUTINE ctmout (mcip_now, sdate, stime)

!-------------------------------------------------------------------------------
! Name:     CTM Output -- Create output for CTM
! Purpose:  Output time-varying fields.
! Revised:  19 Dec 2018  Original version in MCIPv5.0.  Subsumes parts of
!                        metcro.f90, metdot.f90, soilcro.f90, and moscro.f90
!                        from MCIPv4.5.  (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm, ONLY: ioform

  IMPLICIT NONE

  CHARACTER(LEN=24),  INTENT(IN)    :: mcip_now
  INTEGER,            INTENT(IN)    :: sdate
  INTEGER,            INTENT(IN)    :: stime

!-------------------------------------------------------------------------------
! Write time-varying output fields.
!-------------------------------------------------------------------------------

  SELECT CASE ( ioform )

    CASE ( 1 )  ! Models-3 I/O API
      CALL outcm3io (sdate, stime)

    CASE ( 2 )  ! netCDF
      CALL outncf    (mcip_now, sdate, stime)
      CALL outncfbdy (mcip_now, sdate, stime)

  END SELECT

!-------------------------------------------------------------------------------
! Print sample output to log file.
!-------------------------------------------------------------------------------

  CALL outclog

END SUBROUTINE ctmout
