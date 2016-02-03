
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
! $Header: /project/work/rep/MCIP2/src/mcip2/vgrd_mod.F,v 1.2 2007/08/03 20:48:45 tlotte Exp $ 


MODULE vgrd

!-------------------------------------------------------------------------------
! Name:     Vertical Grid Dimension Parameters
! Purpose:  Contains vertical grid dimension parameters.
! Revised:  ?? ??? 19??  Original version.  (???)
!           10 Sep 2001  Converted to free-form f90.  Removed explicit
!                        definition of NLAYS, and changed X3FACE and X3MIDL
!                        to allocatable arrays.  (T. Otte)
!           09 Apr 2007  Added IMPLICIT NONE.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL,          ALLOCATABLE   :: x3face     ( : )  ! vert coord lyr sfc
  REAL,          ALLOCATABLE   :: x3midl     ( : )  ! vert coord lyr ctr

END MODULE vgrd
