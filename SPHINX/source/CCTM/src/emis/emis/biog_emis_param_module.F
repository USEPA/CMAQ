
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
! This module contains key component from BIOG_EMIS.F to facilitate 
! centralized I/O implementation by avoiding cyclic data dependence

! Revision History:
!  01 Feb 2019 D.Wong initial implementation, 
!------------------------------------------------------------------------!

          module biog_emis_param_module

! Former include file  B3V12DIMS3.EXT
! dimensioning parameters for BEIS v3.12 

            INTEGER, PARAMETER :: NSEF     = 35 ! # of em class in Summer emis factor table
            INTEGER, PARAMETER :: NSEASONS = 2  ! number of seasons modeled (2 for now)
            INTEGER, PARAMETER :: NLAI     = 3  ! no. of different LAIs (ISOP and MBO)
            INTEGER, PARAMETER :: NSUMMER  = 1  ! summer flag
            INTEGER, PARAMETER :: NWINTER  = 2  ! winter flag

! Names for BEIS species, emitted model species
! DO NOT CHANGE THE ORDER OF THESE ARRAYS

            CHARACTER( 5 ), PARAMETER :: BIOTYPES( NSEF ) =
     &                          (/ 'ISOP ', 'MBO  ', 'METH ',
     &                             'APIN ', 'BPIN ', 'D3CAR', 'DLIM ',
     &                             'CAMPH', 'MYRC ', 'ATERP', 'BPHE ',
     &                             'SABI ', 'PCYM ', 'OCIM ', 'ATHU ',
     &                             'TRPO ', 'GTERP', 'ETHE ', 'PROPE',
     &                             'ETHO ', 'ACET ', 'HEXA ', 'HEXE ',
     &                             'HEXY ', 'FORM ', 'ACTAL', 'BUTE ',
     &                             'ETHA ', 'FORAC', 'ACTAC', 'BUTO ',
     &                             'CO   ', 'ORVOC', 'SESQT',
     &                             'NO   ' /)

            CHARACTER( 5 ), PARAMETER :: LAITYPES( NLAI ) =
     &                          (/ 'ISOP ', 'MBO  ', 'METH ' /)

            CHARACTER( 1 ), PARAMETER :: SEASON( NSEASONS ) =
     &                          (/ 'S', 'W' /)

          end module biog_emis_param_module
