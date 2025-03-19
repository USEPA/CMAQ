
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

!------------------------------------------------------------------------!
! This module contains key component from STK_PRMS.F to facilitate 
! centralized I/O implementation by avoiding cyclic data dependence

! Revision History:
!  01 Feb 2019 D.Wong initial implementation, 
!------------------------------------------------------------------------!

          module stack_group_data_module

            use UDTYPES, ONLY: IARRY1, RARRY1

            real, allocatable    :: XLOCA(:,:),
     &                              YLOCA(:,:)
            integer, ALLOCATABLE :: NSRC( : )
            integer, allocatable :: STKID(:,:)

            TYPE( RARRY1 ), ALLOCATABLE :: STKDIAM( : )
            TYPE( RARRY1 ), ALLOCATABLE :: STKHT  ( : )
            TYPE( RARRY1 ), ALLOCATABLE :: STKTK  ( : )
            TYPE( RARRY1 ), ALLOCATABLE :: STKVEL ( : )
            TYPE( RARRY1 ), ALLOCATABLE :: ACRES_BURNED( : )

            LOGICAL, ALLOCATABLE :: FIRE_ON( : )

          end module stack_group_data_module
