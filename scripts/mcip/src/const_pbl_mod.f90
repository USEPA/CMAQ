
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
! $Header: /project/work/rep/MCIP2/src/mcip2/const_pbl_mod.F,v 1.2 2007/08/03 20:47:42 tlotte Exp $ 


MODULE const_pbl

!-------------------------------------------------------------------------------
! Name:     Constants for Planetary Boundary Layer
! Purpose:  Parametric constants for describing planetary boundary layer.
! Revised:  17 Jan 1997  Original version.  (D. Byun)
!           20 Jan 1997  Revised to reflect current Models-3 view that MKS
!                        units should be used wherever possible, and that
!                        sources be documented.  Some variables have been
!                        added, names changed, and values revised.  (D. Byun)
!           20 Sep 2001  Converted to free-form f90.  (T. Otte)
!           09 Apr 2007  Added IMPLICIT NONE.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  ! Limit at which similarity theory is allowed to ensure ( HPBL/Z0 >= 125 )

  REAL,          PARAMETER     :: similim = 0.008


  ! Surface layer similarity

  ! (1) simple - no distinction for momentum and heat
! REAL,          PARAMETER     :: vkar  =  0.4
! REAL,          PARAMETER     :: betam =  5.0
! REAL,          PARAMETER     :: betah =  5.0
! REAL,          PARAMETER     :: gamam = 16.0
! REAL,          PARAMETER     :: gamah = 16.0
! REAL,          PARAMETER     :: pro   =  1.0

  ! (2) Businger, et al. (1971)
! REAL,          PARAMETER     :: vkar  =  0.35
! REAL,          PARAMETER     :: betam =  4.7
! REAL,          PARAMETER     :: betah =  6.35
! REAL,          PARAMETER     :: gamam = 15.0
! REAL,          PARAMETER     :: gamah =  9.0
! REAL,          PARAMETER     :: pro   =  0.74

  ! (3) Hogstrom (1988)
  REAL,          PARAMETER     :: vkar  =  0.40
  REAL,          PARAMETER     :: betam =  6.00
  REAL,          PARAMETER     :: betah =  8.21
  REAL,          PARAMETER     :: gamam = 19.30
  REAL,          PARAMETER     :: gamah = 11.60
  REAL,          PARAMETER     :: pro   =  0.95


  ! Limits on bulk Richardson number for surface layer similarity

  REAL,          PARAMETER     :: ricr    =  0.25  ! critical Richardson num
  REAL,          PARAMETER     :: rimax   =  0.70  ! max. bulk Richardson num
  REAL,          PARAMETER     :: rimin   = -4.75  ! min. bulk Richardson num
  REAL,          PARAMETER     :: amolmin =  1.25  ! min allowed 1.0/ABS(MOL)


  ! Boundary layer theory

  REAL,          PARAMETER     :: alamdao = 0.07
  REAL,          PARAMETER     :: ch      = 0.80
  REAL,          PARAMETER     :: aneut   = 1.70
  REAL,          PARAMETER     :: bneut   = 4.50


  ! Reference height at which log wind profile is definitely satisfied

  REAL,          PARAMETER     :: zref = 10.0  ! meters


  ! The way we estimate emissions requires PBL ht. at least 50 m to
  ! have reasonable distributions of concentrations.  DWB (NOV 07, 97)

  REAL,          PARAMETER     :: pblmax     = 5000.0   ! [m]
  REAL,          PARAMETER     :: pblmin     =   50.0   ! [m]

END MODULE const_pbl
