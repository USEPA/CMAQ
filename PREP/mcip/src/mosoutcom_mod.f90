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

MODULE mosoutcom

!-------------------------------------------------------------------------------
! Name:     Mosaic Cross-Point Output Module
! Purpose:  Contains MCIP mosaic cross-point output pointers and targets.
! Revised:  16 Mar 2018  Original version based on mcoutcom_mod.f90 from
!                        MCIPv4.4.  (T. Spero)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Mosaic cross arrays for CTM domain.  (MOSAIC_CRO)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: mos3index = 7

  REAL, ALLOCATABLE, TARGET :: mos3       ( : , : , : , : )

  REAL, POINTER :: lufrac2_c  ( : , : , : )  ! frac land use, ranked [%]
  REAL, POINTER :: moscatidx_c( : , : , : )  ! LU index for LUFRAC2 [dim'less]
  REAL, POINTER :: lai_mos_c  ( : , : , : )  ! leaf area index [area/area]
  REAL, POINTER :: rai_mos_c  ( : , : , : )  ! inv aerodynamic resistance [m/s]
  REAL, POINTER :: rsi_mos_c  ( : , : , : )  ! inv stomatal resistance [m/s]
  REAL, POINTER :: tsk_mos_c  ( : , : , : )  ! vegetation temperature [K]
  REAL, POINTER :: znt_mos_c  ( : , : , : )  ! roughness length [m]


  ! Header description.

  CHARACTER(LEN=16), PARAMETER :: mos3vname ( mos3index ) =     &
    (/ 'LUFRAC2',    'MOSCAT',     'LAI_MOS',    'RAI_MOS',     &
       'RSI_MOS',    'TSK_MOS',    'ZNT_MOS' /)

  CHARACTER(LEN=16), PARAMETER :: mos3units ( mos3index ) =     &
    (/ 'percent   ', 'category  ', 'AREA/AREA ', 'M/S       ',  &
       'M/S       ', 'K         ', 'M         ' /)

  CHARACTER(LEN=80), PARAMETER :: mos3vdesc ( mos3index ) =     &
    (/ 'ranked fractional land use                           ', &  !  1
       'land use category for LUFRAC2                        ', &  !  2
       'leaf area index (mosaic)                             ', &  !  3
       'inverse of aerodynamic resistance (mosaic)           ', &  !  4
       'inverse of stomatal resistance (mosaic)              ', &  !  5
       'vegetation temperature (mosaic)                      ', &  !  6
       'roughness length (mosaic)                            '  /) !  7

END MODULE mosoutcom
