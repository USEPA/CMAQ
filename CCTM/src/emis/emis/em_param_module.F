
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
! This module contains key component from EMIS_DEFN.F to facilitate 
! centralized I/O implementation by avoiding cyclic data dependence

! Revision History:
!  01 Feb 2019 D.Wong initial implementation, 
!------------------------------------------------------------------------!

          module em_param_module

            INTEGER :: N_EM_SRM   ! global number of emissions streams
            INTEGER :: N_EMIS_ISTR! global emissions instructions
            INTEGER :: N_EM_RGN   ! no. of region masks

            ! Define Data Structure for Storing Emission Control Namelist Options  
            TYPE EM_REG
                CHARACTER( 32 ) :: LABEL
                CHARACTER( 32 ) :: FILE
                CHARACTER( 32 ) :: VAR
                INTEGER         :: FILENUM
            END TYPE EM_REG

            TYPE( EM_REG ), ALLOCATABLE :: EM_REGIONS( : )
            REAL, ALLOCATABLE           :: EM_REG_FAC( :,:,: )

            ! Define Structire for Emission Scaling Rules
            TYPE EM_RULE_REG
                CHARACTER( 32 ) :: REGION
                CHARACTER( 32 ) :: STREAM
                CHARACTER( 16 ) :: SURR
                CHARACTER( 16 ) :: SPEC
                CHARACTER( 16 ) :: PHASE
                REAL            :: FAC
                CHARACTER( 4  ) :: BASIS
                CHARACTER( 1  ) :: OP
            END TYPE EM_RULE_REG
            INTEGER, PARAMETER  :: N_EM_RULE_REG = 2000
            TYPE( EM_RULE_REG ), ALLOCATABLE :: EM_NML( : )

            ! Define Structure for Area Normalization
            TYPE AREA_REG
                CHARACTER( 32 ) :: STREAM
                CHARACTER( 32 ) :: AREA
                CHARACTER( 32 ) :: ADJ
            END TYPE AREA_REG
            INTEGER, PARAMETER  :: N_AREA_REG = 100
            TYPE( AREA_REG ), ALLOCATABLE :: AREA_NML( : )

            ! Define Structure for Region Masks
            TYPE REGIONS_REG
                CHARACTER( 32 ) :: LABEL
                CHARACTER( 32 ) :: FILE
                CHARACTER( 32 ) :: VAR
            END TYPE REGIONS_REG
            INTEGER, PARAMETER  :: N_REGIONS_REG = 600
            TYPE( REGIONS_REG ), ALLOCATABLE :: RGN_NML( : )

            ! Define Structure for Size Distribution Rule      
            TYPE SD_RULE
                CHARACTER( 32 ) :: STREAM
                CHARACTER( 16 ) :: MODE
                CHARACTER( 16 ) :: MODE_REF
            END TYPE SD_RULE
            INTEGER, PARAMETER :: N_SD_REG = 200
            TYPE( SD_RULE ), ALLOCATABLE :: SD_NML( : )

          end module em_param_module
