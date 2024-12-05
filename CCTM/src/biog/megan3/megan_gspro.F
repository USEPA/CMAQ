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
      MODULE MEGAN_GSPRO

!
! This module determines the speciation map files to use and populates 
! the MEGAN_NAMES array. This array is used in EMIS_DEFN.F via the 
! variable vdemis_me in the MIOG stream.
!

      IMPLICIT NONE
         
           INTEGER, SAVE :: NMGNSPC
           INTEGER, SAVE :: n_scon_spc 
           CHARACTER( 16 ), ALLOCATABLE :: MEGAN_NAMES(:)
           INTEGER, ALLOCATABLE ::  spmh_map(:),mech_map(:)   ! speciated species name

           REAL, ALLOCATABLE :: conv_fac(:)        
           REAL,ALLOCATABLE :: mech_mwt(:)
           CHARACTER( 16 ), ALLOCATABLE :: mech_spc(:)

            INCLUDE 'SPC_NOCONVER.EXT'
            INCLUDE 'SPC_CB05.EXT'
            INCLUDE 'SPC_CB6.EXT'
            INCLUDE 'SPC_CB6_AE7.EXT'
            INCLUDE 'SPC_RACM2.EXT'        ! new in MEGAN3
            INCLUDE 'SPC_CRACMM1.EXT'        ! new in CMAQ 5.4
            INCLUDE 'SPC_CRACMM2.EXT'        ! new in CMAQ 5.5
            INCLUDE 'MAP_CV2CB05.EXT'
            INCLUDE 'SPC_SAPRC07.EXT'      ! new in MEGAN3
            INCLUDE 'SPC_SAPRC07T.EXT'    ! new in MEGAN3
            INCLUDE 'MAP_CV2CB6.EXT'      
            INCLUDE 'MAP_CV2CB6_AE7.EXT'      
            INCLUDE 'MAP_CV2RACM2.EXT'
            INCLUDE 'MAP_CV2CRACMM1.EXT'
            INCLUDE 'MAP_CV2CRACMM2.EXT'
            INCLUDE 'MAP_CV2SAPRC07.EXT'
            INCLUDE 'MAP_CV2SAPRC07T.EXT'

         CONTAINS

         subroutine MEGAN_MAP

         USE RXNS_DATA, ONLY : MECHNAME
         USE RUNTIME_VARS, ONLY : LOGDEV
         USE UTILIO_DEFN

          IMPLICIT NONE

           CHARACTER( 16 ) :: MECHANISM              ! Mechanism name
           INTEGER ios,INDX
         INTEGER,        PARAMETER :: NMECHS = 17   ! dimension for number of mechanisms considered

         INTEGER I

         CHARACTER( 256 ) :: XMSG
         LOGICAL          :: EFLAG 

         TYPE MIOG_MECH_TYPE
              CHARACTER( 32 ) :: CHEMMECH
              CHARACTER( 16 ) :: MIOGMECH
         END TYPE MIOG_MECH_TYPE
           TYPE( MIOG_MECH_TYPE ) :: MIOG_MECH_MAP( NMECHS ) = (/
     &         MIOG_MECH_TYPE( 'CB05E51_AE6_AQ         ','CB05    '),
     &         MIOG_MECH_TYPE( 'CB05EH51_AE6_AQ        ','CB05    '),
     &         MIOG_MECH_TYPE( 'CB05MP51_AE6_AQ        ','CB05    '),
     &         MIOG_MECH_TYPE( 'CB05TUCL51_AE6_AQ      ','CB05    '),
     &         MIOG_MECH_TYPE( 'CB6R3_AE6_AQ           ','CB6     '),
     &         MIOG_MECH_TYPE( 'CB6MP_AE6_AQ           ','CB6     '),
     &         MIOG_MECH_TYPE( 'CB6R5HAP_AE7_AQ        ','CB6_ae7 '),
     &         MIOG_MECH_TYPE( 'CB6R3_AE7_AQ           ','CB6_ae7 '),
     &         MIOG_MECH_TYPE( 'CB6R5_AE7_AQ           ','CB6_ae7 '),
     &         MIOG_MECH_TYPE( 'CB6R5M_AE7_AQ          ','CB6_ae7 '),
     &         MIOG_MECH_TYPE( 'RACM2_AE6_AQ           ','RACM2   '),
     &         MIOG_MECH_TYPE( 'SAPRC07TC_AE6_AQ       ','SAPRC07T'),
     &         MIOG_MECH_TYPE( 'SAPRC07TIC_AE7I_AQ     ','SAPRC07T'),
     &         MIOG_MECH_TYPE( 'SAPRC07TIC_AE7I_AQKMT2 ','SAPRC07T'),     
     &         MIOG_MECH_TYPE( 'CRACMM1_AQ             ','CRACMM1 '),
     &         MIOG_MECH_TYPE( 'CRACMM1AMORE_AQ        ','CRACMM1 '),
     &         MIOG_MECH_TYPE( 'CRACMM2                ','CRACMM2 ') /)

            INDX = INDEX1( MECHNAME, NMECHS, MIOG_MECH_MAP%CHEMMECH )
            MECHANISM = MIOG_MECH_MAP( INDX )%MIOGMECH
            EFLAG = .FALSE.
 
          SELECT CASE ( TRIM(MECHANISM) )
            CASE ('SAPRC07')
              n_scon_spc = n_saprc07
              NMGNSPC = n_saprc07_spc
            CASE ('SAPRC07T')
              n_scon_spc = n_saprc07t
              NMGNSPC = n_saprc07t_spc
            CASE ('CB05')
              n_scon_spc = n_cb05
              NMGNSPC = n_cb05_spc
            CASE ('CB6')
              n_scon_spc = n_cb6  ! 145
              NMGNSPC = n_cb6_spc ! 34
            CASE ('RACM2')
              n_scon_spc = n_racm2
              NMGNSPC = n_racm2_spc
            CASE ('CB6_ae7')
              n_scon_spc = n_cb6_ae7
              NMGNSPC = n_cb6_ae7_spc
            CASE ('CRACMM1')
              n_scon_spc = n_cracmm1
              NMGNSPC = n_cracmm1_spc
            CASE ('CRACMM2')
              n_scon_spc = n_cracmm2
              NMGNSPC = n_cracmm2_spc
            CASE DEFAULT
              EFLAG = .TRUE.
              XMSG = "Mechanism," // TRIM( MECHNAME ) 
     &             // ", is not identified."
              WRITE(LOGDEV,'(A)')TRIM( XMSG )
          ENDSELECT
   
          ALLOCATE ( MEGAN_NAMES(NMGNSPC), STAT = ios )
          ALLOCATE ( spmh_map(n_scon_spc), STAT = ios )
          ALLOCATE ( mech_map(n_scon_spc), STAT = ios )
          ALLOCATE ( conv_fac(n_scon_spc), STAT = ios )
          ALLOCATE ( mech_spc(NMGNSPC ), STAT = ios )
          ALLOCATE ( mech_mwt(NMGNSPC ), STAT = ios )
  
          SELECT CASE ( TRIM(MECHANISM) )
  
            CASE ('CB05')
              spmh_map(1:n_scon_spc) = spmh_map_cb05(1:n_scon_spc)
              mech_map(1:n_scon_spc) = mech_map_cb05(1:n_scon_spc)
              conv_fac(1:n_scon_spc) = conv_fac_cb05(1:n_scon_spc)
              mech_spc(1:NMGNSPC)    = mech_spc_cb05(1:NMGNSPC)
              mech_mwt(1:NMGNSPC)    = mech_mwt_cb05(1:NMGNSPC)
              MEGAN_NAMES(1:NMGNSPC)     = mech_spc(1:NMGNSPC)
            CASE ('CB6')
              spmh_map(1:n_scon_spc) = spmh_map_cb6(1:n_scon_spc)
              mech_map(1:n_scon_spc) = mech_map_cb6(1:n_scon_spc)
              conv_fac(1:n_scon_spc) = conv_fac_cb6(1:n_scon_spc)
              mech_spc(1:NMGNSPC)    = mech_spc_cb6(1:NMGNSPC)
              mech_mwt(1:NMGNSPC)    = mech_mwt_cb6(1:NMGNSPC)
              MEGAN_NAMES(1:NMGNSPC)     = mech_spc(1:NMGNSPC)
            CASE ('RACM2')
              spmh_map(1:n_scon_spc) = spmh_map_racm2(1:n_scon_spc)
              mech_map(1:n_scon_spc) = mech_map_racm2(1:n_scon_spc)
              conv_fac(1:n_scon_spc) = conv_fac_racm2(1:n_scon_spc)
              mech_spc(1:NMGNSPC)    = mech_spc_racm2(1:NMGNSPC)
              mech_mwt(1:NMGNSPC)    = mech_mwt_racm2(1:NMGNSPC)
              MEGAN_NAMES(1:NMGNSPC)     = mech_spc(1:NMGNSPC)
            CASE ('SAPRC07')
              spmh_map(1:n_scon_spc) = spmh_map_saprc07(1:n_scon_spc)
              mech_map(1:n_scon_spc) = mech_map_saprc07(1:n_scon_spc)
              conv_fac(1:n_scon_spc) = conv_fac_saprc07(1:n_scon_spc)
              mech_spc(1:NMGNSPC)    = mech_spc_saprc07(1:NMGNSPC)
              mech_mwt(1:NMGNSPC)    = mech_mwt_saprc07(1:NMGNSPC)
              MEGAN_NAMES(1:NMGNSPC)     = mech_spc(1:NMGNSPC)
            CASE ('SAPRC07T')
              spmh_map(1:n_scon_spc) = spmh_map_saprc07t(1:n_scon_spc)
              mech_map(1:n_scon_spc) = mech_map_saprc07t(1:n_scon_spc)
              conv_fac(1:n_scon_spc) = conv_fac_saprc07t(1:n_scon_spc)
              mech_spc(1:NMGNSPC)    = mech_spc_saprc07t(1:NMGNSPC)
              mech_mwt(1:NMGNSPC)    = mech_mwt_saprc07t(1:NMGNSPC)
              MEGAN_NAMES(1:NMGNSPC)     = mech_spc(1:NMGNSPC)
            CASE ('CB6_ae7')
              spmh_map(1:n_scon_spc) = spmh_map_cb6_ae7(1:n_scon_spc)
              mech_map(1:n_scon_spc) = mech_map_cb6_ae7(1:n_scon_spc)
              conv_fac(1:n_scon_spc) = conv_fac_cb6_ae7(1:n_scon_spc)
              mech_spc(1:NMGNSPC)    = mech_spc_cb6_ae7(1:NMGNSPC)
              mech_mwt(1:NMGNSPC)    = mech_mwt_cb6_ae7(1:NMGNSPC)
              MEGAN_NAMES(1:NMGNSPC)     = mech_spc(1:NMGNSPC)
            CASE ('CRACMM1')
              spmh_map(1:n_scon_spc) = spmh_map_cracmm1(1:n_scon_spc)
              mech_map(1:n_scon_spc) = mech_map_cracmm1(1:n_scon_spc)
              conv_fac(1:n_scon_spc) = conv_fac_cracmm1(1:n_scon_spc)
              mech_spc(1:NMGNSPC)    = mech_spc_cracmm1(1:NMGNSPC)
              mech_mwt(1:NMGNSPC)    = mech_mwt_cracmm1(1:NMGNSPC)
              MEGAN_NAMES(1:NMGNSPC)     = mech_spc(1:NMGNSPC)
            CASE ('CRACMM2')
              spmh_map(1:n_scon_spc) = spmh_map_cracmm2(1:n_scon_spc)
              mech_map(1:n_scon_spc) = mech_map_cracmm2(1:n_scon_spc)
              conv_fac(1:n_scon_spc) = conv_fac_cracmm2(1:n_scon_spc)
              mech_spc(1:NMGNSPC)    = mech_spc_cracmm2(1:NMGNSPC)
              mech_mwt(1:NMGNSPC)    = mech_mwt_cracmm2(1:NMGNSPC)
              MEGAN_NAMES(1:NMGNSPC)     = mech_spc(1:NMGNSPC)
            CASE DEFAULT 
              EFLAG = .TRUE.
              XMSG = "Mapping for Mechanism," // TRIM( MECHNAME )
     &             // ", is unspecified."
              WRITE(LOGDEV,'(A)')TRIM( XMSG )
          ENDSELECT
          
          IF( EFLAG )THEN
             XMSG = 'Above Error(s) detected'
             CALL M3EXIT('MAP_MEGAN', 0, 0, XMSG, XSTAT1 )
          END IF


        End subroutine MEGAN_MAP
      END MODULE MEGAN_GSPRO
