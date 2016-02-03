
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
! $Header: /project/work/rep/MCIP2/src/mcip2/alloc_lu.F


SUBROUTINE alloc_lu

!-------------------------------------------------------------------------------
! Name:     Allocate Land Use Variables
! Purpose:  Allocate arrays for land use variables.
! Revised:  25 Aug 2009  Original version.  (T. Otte)
!-------------------------------------------------------------------------------

  USE luvars

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Allocate land use arrays for all possible input classification systems.
!-------------------------------------------------------------------------------

  ALLOCATE ( lucatold    ( ncatold    ) )
  ALLOCATE ( lucatusgs24 ( ncatusgs24 ) )
  ALLOCATE ( lucatsib    ( ncatsib    ) )
  ALLOCATE ( lucatusgs33 ( ncatusgs33 ) )
  ALLOCATE ( lucatmod    ( ncatmod    ) )
  ALLOCATE ( lucatnlcd   ( ncatnlcd   ) )

END SUBROUTINE alloc_lu
