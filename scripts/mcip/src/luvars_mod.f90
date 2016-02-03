
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
! $Header: /project/work/rep/MCIP2/src/mcip2/luvars_mod.F


MODULE luvars

!-------------------------------------------------------------------------------
! Name:     Land Use Variables
! Purpose:  Contains input land use classification arrays.
! Revised:  25 Aug 2009  Original version.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER,       PARAMETER     :: ncatold    = 13   ! MM5 "old"
  INTEGER,       PARAMETER     :: ncatusgs24 = 24   ! USGS 24-category
  INTEGER,       PARAMETER     :: ncatsib    = 16   ! SiB
  INTEGER,       PARAMETER     :: ncatusgs33 = 33   ! USGS 33-category
  INTEGER,       PARAMETER     :: ncatmod    = 33   ! MODIS-NOAH 33-category
  INTEGER,       PARAMETER     :: ncatnlcd   = 50   ! NCLD-MODIS 50 category

  CHARACTER*60,  ALLOCATABLE   :: lucatold    ( : )
  CHARACTER*60,  ALLOCATABLE   :: lucatusgs24 ( : )
  CHARACTER*60,  ALLOCATABLE   :: lucatsib    ( : )
  CHARACTER*60,  ALLOCATABLE   :: lucatusgs33 ( : )
  CHARACTER*60,  ALLOCATABLE   :: lucatmod    ( : )
  CHARACTER*60,  ALLOCATABLE   :: lucatnlcd   ( : )

END MODULE luvars
