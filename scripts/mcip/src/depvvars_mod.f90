
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
! $Header: /project/work/rep/MCIP2/src/mcip2/depvvars_mod.F,v 1.4 2007/08/03 20:50:43 tlotte Exp $


MODULE depvvars

!-------------------------------------------------------------------------------
! Name:     Dry Deposition Variables
! Purpose:  Contains arrays specific to dry deposition species.
! Revised:  19 Aug 2005  Original version.  (T. Otte and W. Hutzell)
!           01 Feb 2006  Added mesophyll resistance.  (D. Schwede)
!           15 Jun 2006  Added variable SCC_PR_23.  (T. Otte)
!           24 Jul 2007  Added IMPLICIT NONE.  Removed arrays for RADMdry.
!                        Added molecular weights for dry deposition species.
!                        (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Dry deposition arrays.
!-------------------------------------------------------------------------------

  CHARACTER*16, ALLOCATABLE :: xdepspc           ( : )  ! deposition species
  REAL,         ALLOCATABLE :: xvd       ( : , : , : )  ! dep velocity [m/s]
  REAL,         ALLOCATABLE :: vd_c      ( : , : , : )  ! dep velocity [m/s]

  REAL,         ALLOCATABLE :: a         ( : )  ! reactivity relative to HNO3
  REAL,         ALLOCATABLE :: alphs     ( : )  ! KH correction for aq dissoc
  REAL,         ALLOCATABLE :: dif0      ( : )  ! molecular diffusivity [cm2/s]
  REAL,         ALLOCATABLE :: dkhor     ( : )  ! enthalpy (activ'n energy) [K]
  REAL,         ALLOCATABLE :: kh        ( : )  ! Henry's law constant [M/atm]
  REAL,         ALLOCATABLE :: meso      ( : )  ! mesophyll resistance [s/m]
  REAL,         ALLOCATABLE :: molwt     ( : )  ! molecular weight
  REAL,         ALLOCATABLE :: scc_pr_23 ( : )  ! (SCC/PR)**0.66667, fn of DIF0
  CHARACTER*16, ALLOCATABLE :: subname   ( : )  ! for subroutine HLCONST

END MODULE depvvars
