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

MODULE luoutcom

!-------------------------------------------------------------------------------
! Name:     Land Use Fraction Cross-Point Output Module
! Purpose:  Contains MCIP land use fraction coss-point output pointers and
!           targets.
! Revised:  13 Feb 2018  Original version based on mcoutcom_mod.f90 from
!                        MCIPv4.4.  (T. Spero)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Land use fraction cross arrays for CTM domain.  (LUFRAC_CRO)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: lu3index = 1

  REAL, ALLOCATABLE, TARGET :: lu3       ( : , : , : , : )

  REAL, POINTER :: lufrac_c   ( : , : , : )  ! fractional land use [%]


  ! Header description.

  CHARACTER(LEN=16), PARAMETER :: lu3vname ( lu3index ) =       &
    (/ 'LUFRAC' /)

  CHARACTER(LEN=16), PARAMETER :: lu3units ( lu3index ) =       &
    (/ 'percent   ' /)

  CHARACTER(LEN=80), PARAMETER :: lu3vdesc ( lu3index ) =       &
    (/ 'fractional land use                                  '  /) !  1

END MODULE luoutcom
