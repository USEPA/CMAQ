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
!           21 Aug 2015  Added variables from WRF/ACM2 to support updated
!                        calculation of Monin-Obukhov length within MCIP, which
!                        allows for "corrector" portion of predictor/corrector
!                        equation to be used in CMAQ.  Removed unused variables
!                        RICR, RIMAX, RIMIN, ALAMDAO, CH, ANEUT, BNEUT, ZREF,
!                        PBLMAX, PBLMIN, and SIMILIM.  (T. Spero)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

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


  REAL,          PARAMETER     :: amolmin =  1.25  ! min allowed 1.0/ABS(MOL)


  ! Constants used by PBLSUP that are consistent with those used in WRF/ACM2
  ! (adapted from WRF's module_model_constants.f90 as of WRFV3.7).

  REAL,          PARAMETER     :: r_d     = 287.0
  REAL,          PARAMETER     :: r_v     = 461.6
  REAL,          PARAMETER     :: cp      = 7.0 * r_d / 2.0
  REAL,          PARAMETER     :: ep_1    = r_v / r_d - 1.0

END MODULE const_pbl
