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
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
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
