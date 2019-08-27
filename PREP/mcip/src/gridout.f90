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

SUBROUTINE gridout (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     GRID Output -- Create "grid" (or time-independent) output for CTM
! Purpose:  Output time-independent fields.
! Revised:  18 Dec 2018  Original version in MCIPv5.0.  Subsumes part of
!                        gridout.f90 from MCIPv4.5.  (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm, ONLY: ioform

  IMPLICIT NONE

  INTEGER,            INTENT(IN)    :: sdate
  INTEGER,            INTENT(IN)    :: stime

!-------------------------------------------------------------------------------
! Write time-independent output fields.
!-------------------------------------------------------------------------------

  SELECT CASE ( ioform )

    CASE ( 1 )  ! Models-3 I/O API
      CALL outgm3io (sdate, stime)

  END SELECT

!-------------------------------------------------------------------------------
! Write GRIDDESC file.
!-------------------------------------------------------------------------------

  CALL wrgdesc

!-------------------------------------------------------------------------------
! Print sample output to log file.
!-------------------------------------------------------------------------------

  CALL outglog

END SUBROUTINE gridout
