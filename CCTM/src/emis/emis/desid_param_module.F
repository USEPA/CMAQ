
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

      module desid_param_module

        INTEGER :: DESID_N_SRM   ! number of DESID emissions streams
        INTEGER :: DESID_N_ISTR  ! number of emissions instruction stacks
        INTEGER :: DESID_N_REG   ! number of region masks
        INTEGER :: DESID_N_DIAG  ! number of DESID Diagnostic files

        INTEGER, ALLOCATABLE :: DESID_GRID_LAYS(:) ! Number of Grid Layers 
                                                   !   for each stream
        INTEGER              :: DESID_MAXLAYS = 0  ! Maximum Emission Layers

        ! Define Data Structure for Reading Emission Control Namelist Input
        TYPE DESID_RULES_TYPE
            CHARACTER( 32 ) :: REGION
            CHARACTER( 32 ) :: STREAM
            CHARACTER( 16 ) :: EMVAR
            CHARACTER( 16 ) :: SPEC
            CHARACTER( 16 ) :: PHASE
            REAL            :: FAC
            CHARACTER( 4  ) :: BASIS
            CHARACTER( 1  ) :: OP
        END TYPE DESID_RULES_TYPE
        INTEGER :: DESID_MAX_RULES
        TYPE( DESID_RULES_TYPE ), ALLOCATABLE :: DESID_RULES_NML(:)

        ! Define Structure for Reading Area Normalization Namelist
        ! input
        TYPE DESID_AREA_TYPE
            CHARACTER( 32 ) :: STREAM
            CHARACTER( 32 ) :: AREA
            CHARACTER( 32 ) :: ADJ
        END TYPE DESID_AREA_TYPE
        TYPE( DESID_AREA_TYPE ), ALLOCATABLE :: DESID_AREA_NML(:)

        ! Define Structure for Reading Ragion Mask Input
        TYPE DESID_REG_NML_TYPE
            CHARACTER( 32 ) :: LABEL
            CHARACTER( 32 ) :: FILE
            CHARACTER( 32 ) :: VAR
        END TYPE DESID_REG_NML_TYPE
        TYPE( DESID_REG_NML_TYPE ), ALLOCATABLE :: DESID_REG_NML(:)
        INTEGER                                 :: DESID_MAX_REG

        ! Define Structure for Storing Region Masks
        TYPE DESID_REG_TYPE
            CHARACTER( 32 ) :: LABEL
            CHARACTER( 32 ) :: FILE
            CHARACTER( 32 ) :: VAR
            INTEGER         :: FILENUM
        END TYPE DESID_REG_TYPE

        TYPE( DESID_REG_TYPE ), ALLOCATABLE :: DESID_REG( : )
        REAL, ALLOCATABLE           :: DESID_REG_FAC( :,:,: )
        LOGICAL, ALLOCATABLE        :: DESID_REG_SUB( :,: )

        ! Define Structure for Reading Size Distribution Namelist
        ! Input
        TYPE DESID_SD_TYPE
            CHARACTER( 32 ) :: STREAM
            CHARACTER( 16 ) :: MODE
            CHARACTER( 16 ) :: MODE_REF
        END TYPE DESID_SD_TYPE
        TYPE( DESID_SD_TYPE ), ALLOCATABLE :: DESID_SD_NML(:)

        ! Define Variables for Storing Emissions Diagnostic
        ! Parameters
        Integer                      :: Desid_Max_Diag_Streams = 20
        Integer                      :: Desid_Max_Diag_Spec = 80
        Character( 32 ), Allocatable :: Desid_Diag_Streams_Nml(:,:)
        Character( 6  ), Allocatable :: Desid_Diag_Fmt_Nml(:)       
        Character( 16 ), Allocatable :: Desid_Diag_Spec_Nml(:,:) 

      end module desid_param_module
