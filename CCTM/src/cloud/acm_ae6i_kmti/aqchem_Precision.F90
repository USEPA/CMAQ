
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!
 

MODULE aqchem_Precision

!
! Definition of different levels of accuracy
! for REAL variables using KIND parameterization
!
! KPP SP - Single precision kind
  INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6,30)
! KPP DP - Double precision kind
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(14,300)
! KPP QP - Quadruple precision kind
  INTEGER, PARAMETER :: qp = SELECTED_REAL_KIND(18,400)

END MODULE aqchem_Precision


