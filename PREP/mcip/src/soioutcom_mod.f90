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

MODULE soioutcom

!-------------------------------------------------------------------------------
! Name:     Soil Cross-Point Output Module
! Purpose:  Contains MCIP soil cross-point output pointers and targets.
! Revised:  10 Feb 2018  Original version based on mcoutcom_mod.f90 from
!                        MCIPv4.4.  (T. Spero)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Soil cross arrays for CTM domain.  (SOI_CRO)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: soi3index = 2

  REAL, ALLOCATABLE, TARGET :: soi3       ( : , : , : , : )

  REAL, POINTER :: soit3d_c   ( : , : , : )  ! soil temperature [K]
  REAL, POINTER :: soim3d_c   ( : , : , : )  ! soil moisture [m^3/m^3]


  ! Header description.

  CHARACTER(LEN=16), PARAMETER :: soi3vname ( soi3index ) =     &
    (/ 'SOIT3D',     'SOIM3D' /)

  CHARACTER(LEN=16), PARAMETER :: soi3units ( soi3index ) =     &
    (/ 'K         ', 'M**3/M**3 ' /)

  CHARACTER(LEN=80), PARAMETER :: soi3vdesc ( soi3index ) =     &
    (/ 'soil temperature                                     ', &  !  1
       'soil moisture                                        '  /) !  2

END MODULE soioutcom
