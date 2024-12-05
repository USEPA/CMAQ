
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

c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      module cgrid_spcs

c CGRID gas chem, aerosol, non-reactive, and tracer species definitions
c based on namelist specifications
c Revision History:
c Jeff Young 31 Aug 09: created
c Bill Hutzell 8 Oct 10: added Gas Chemistry consistency check
C Shawn Roselle 16 Feb 11: replaced I/O API include files with UTILIO_DEFN
C 07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
C 21 Apr 16 D.Luecken:  increased spc_dim to 1000
C 07 Mar 2019 F. Sidi:  Split up ICBC for CMAQ species  namelist 
C 10 June 2021 G. Sarwar: Replacd CB6R3M_AE7_AQ with CB6R5M_AE7_AQ
c-----------------------------------------------------------------------
      USE RUNTIME_VARS
      Use rxns_data, only: mechname       ! chemical mechanism data
#ifdef mpas
      use util_module, only : junit
#endif

      implicit none

c main CGRID table
      integer,                      save :: n_gc_spc = 0, n_ae_spc = 0, n_nr_spc = 0, n_tr_spc = 0
      character( 16 ), allocatable, save :: gc_spc( : ),  ae_spc( : ),  nr_spc( : ),  tr_spc( : )
      real, allocatable,            save :: gc_molwt( : ), ae_molwt( : ), nr_molwt( : ), tr_molwt( : )

      integer,                      save :: n_gc_spcd = 0

c Starting and ending index of gas chemistry species in CGRID
      integer,                      save :: gc_strt = 1
      integer,                      save :: gc_fini = 0

c Starting and ending index of aerosol species in CGRID
      integer,                      save :: ae_strt = 0
      integer,                      save :: ae_fini = 0

c Starting and ending index of non-reactive species in CGRID
      integer,                      save :: nr_strt = 0
      integer,                      save :: nr_fini = 0

c Starting and ending index of tracer species in CGRID
      integer,                      save :: tr_strt = 0
      integer,                      save :: tr_fini = 0

c number of species in CGRID
      integer,                      save :: nspcsd = 1

c surrogate species
      integer,                      save :: n_gc_ic = 0,   n_ae_ic = 0,   n_nr_ic = 0,   n_tr_ic = 0,
     &                                      n_gc_bc = 0,   n_ae_bc = 0,   n_nr_bc = 0,   n_tr_bc = 0,
     &                                      n_gc_depv = 0, n_ae_depv = 0, n_nr_depv = 0, n_tr_depv = 0,
     &                                      n_gc_scav = 0, n_ae_scav = 0, n_nr_scav = 0, n_tr_scav = 0,
     &                                      n_gc_g2ae = 0,                n_nr_n2ae = 0, n_tr_t2ae = 0,
     &                                      n_gc_g2aq = 0, n_ae_a2aq = 0, n_nr_n2aq = 0, n_tr_t2aq = 0
      integer,                      save :: n_ae_optics = 0
      character( 16 ), allocatable, save :: gc_ic( : ),   ae_ic( : ),   nr_ic( : ),   tr_ic( : ), 
     &                                      gc_bc( : ),   ae_bc( : ),   nr_bc( : ),   tr_bc( : ),   
     &                                      gc_depv( : ), ae_depv( : ), nr_depv( : ), tr_depv( : ),
     &                                      gc_scav( : ), ae_scav( : ), nr_scav( : ), tr_scav( : ),
     &                                      gc_g2ae( : ),               nr_n2ae( : ), tr_t2ae( : ),
     &                                      gc_g2aq( : ), ae_a2aq( : ), nr_n2aq( : ), tr_t2aq( : )
      character( 16 ), allocatable, save :: ae_optics( : )
      integer, allocatable,         save :: gc_ic_map( : ),   ae_ic_map( : ),   nr_ic_map( : ),   tr_ic_map( : ),
     &                                      gc_bc_map( : ),   ae_bc_map( : ),   nr_bc_map( : ),   tr_bc_map( : ),  
     &                                      gc_depv_map( : ), ae_depv_map( : ), nr_depv_map( : ), tr_depv_map( : ),
     &                                      gc_scav_map( : ), ae_scav_map( : ), nr_scav_map( : ), tr_scav_map( : ),
     &                                      gc_g2ae_map( : ),                   nr_n2ae_map( : ), tr_t2ae_map( : ),
     &                                      gc_g2aq_map( : ), ae_a2aq_map( : ), nr_n2aq_map( : ), tr_t2aq_map( : )

      real, allocatable,            save :: gc_ic_fac( : ),   ae_ic_fac( : ),   nr_ic_fac( : ),   tr_ic_fac( : ),
     &                                      gc_bc_fac( : ),   ae_bc_fac( : ),   nr_bc_fac( : ),   tr_bc_fac( : ),
     &                                      gc_depv_fac( : ), ae_depv_fac( : ), nr_depv_fac( : ), tr_depv_fac( : ),
     &                                      gc_scav_fac( : ), ae_scav_fac( : ), nr_scav_fac( : ), tr_scav_fac( : )

c control species
      integer,                      save :: n_gc_trns = 0, n_ae_trns = 0, n_nr_trns = 0, n_tr_adv = 0, n_tr_diff = 0,
     &                                      n_gc_ddep = 0, n_ae_ddep = 0, n_nr_ddep = 0, n_tr_ddep = 0,
     &                                      n_gc_wdep = 0, n_ae_wdep = 0, n_nr_wdep = 0, n_tr_wdep = 0,
     &                                      n_gc_conc = 0, n_ae_conc = 0, n_nr_conc = 0, n_tr_conc = 0
      character( 16 ), allocatable, save :: gc_trns( : ), ae_trns( : ), nr_trns( : ), tr_adv( : ), tr_diff( : ),
     &                                      gc_ddep( : ), ae_ddep( : ), nr_ddep( : ), tr_ddep( : ),
     &                                      gc_wdep( : ), ae_wdep( : ), nr_wdep( : ), tr_wdep( : ),
     &                                      gc_conc( : ), ae_conc( : ), nr_conc( : ), tr_conc( : )
      integer, allocatable,         save :: gc_trns_map( : ), ae_trns_map( : ), nr_trns_map( : ), tr_adv_map( : ), tr_diff_map( : ),
     &                                      gc_ddep_map( : ), ae_ddep_map( : ), nr_ddep_map( : ), tr_ddep_map( : ),
     &                                      gc_wdep_map( : ), ae_wdep_map( : ), nr_wdep_map( : ), tr_wdep_map( : ),
     &                                      gc_conc_map( : ), ae_conc_map( : ), nr_conc_map( : ), tr_conc_map( : )

      LOGICAL :: LTRNS, LDDEP, LWDEP, LCONC, LADVC, LDIFF
      LOGICAL, ALLOCATABLE :: LAIT(:), LACC(:), LCOR(:)


      INTEGER, SAVE              :: N_CGRID_SPC
      INTEGER, SAVE              :: N_SPC_WDEP
      INTEGER, SAVE              :: N_SPC_DEPV
      INTEGER, SAVE              :: RHOJ_LOC
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: CGRID_NAME( : )
      REAL,    ALLOCATABLE, SAVE :: CGRID_MW       ( : )
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: CGRID_UNITS    ( : )
      LOGICAL, ALLOCATABLE, SAVE :: CGRID_MASK_GAS ( : )
      LOGICAL, ALLOCATABLE, SAVE :: CGRID_MASK_AERO( : )
      LOGICAL, ALLOCATABLE, SAVE :: CGRID_MASK_NUM ( : )
      LOGICAL, ALLOCATABLE, SAVE :: CGRID_MASK_SRF ( : )
      LOGICAL, ALLOCATABLE, SAVE :: CGRID_MASK_NR  ( : )
      LOGICAL, ALLOCATABLE, SAVE :: CGRID_MASK_TRAC( : )
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: CGRID_CONC_UNIT ( : )
      CHARACTER( 256), ALLOCATABLE, SAVE :: CGRID_CONC_DESC ( : )
      INTEGER, ALLOCATABLE, SAVE :: MAP_DEPVtoCGRID( : )
      INTEGER, ALLOCATABLE, SAVE :: MAP_WDEPtoCGRID( : )

#ifdef mpas
      character( 16 ), allocatable, save :: all_spc( : )
      integer,                      save :: n_all_spc = 0

      integer, allocatable :: indx_mpas(:)
#endif

      type, public :: base_type
         private
         character( 16 ) :: name
         real            :: mw
      end type
      type, public :: surr1_type
         private
         character( 16 ) :: name
         integer         :: map
         real            :: fac
      end type
      type, public :: surr2_type
         private
         character( 16 ) :: name
         integer         :: map
      end type
      type, public :: ctrl_type
         private
         character( 16 ) :: name
         integer         :: map
      end type
      
      contains

c init routine to allocate the above allocatables
c !! NO   and to populate the allocated arrays from the stnd gc_spc.ext  !!! NO

         function cgrid_spcs_init() result ( success )
            use UTILIO_DEFN
            use stm_vars
            use cgrid_spcs_types

            implicit none

            logical success
            integer, save :: dev_gc_nml, dev_ae_nml, dev_nr_nml, dev_tr_nml
            integer i, ios, IGC, IAE, INR, ITR
            character(  1 ), parameter :: bl = ' '
            integer, parameter :: spc_dim = 1000
            logical :: order = .true., found = .true.
            character( 120 ) :: xmsg
            INTEGER :: ISPC, JSPC, IGRID, IWDEP, IDEPV

#ifdef mpas
            integer :: i1, i2, i3, i4   
#endif
            TYPE (SPECIES_DATA_REG_1), ALLOCATABLE :: GC_SPECIES_DATA(:)
            TYPE (SPECIES_DATA_REG_1), ALLOCATABLE :: NR_SPECIES_DATA(:)
            TYPE (SPECIES_DATA_REG_3), ALLOCATABLE :: TR_SPECIES_DATA(:)

            namelist / GC_NML / GC_SPECIES_DATA
            namelist / AE_NML / AE_SPECIES_DATA
            namelist / NR_NML / NR_SPECIES_DATA
            namelist / TR_NML / TR_SPECIES_DATA

c----------------------------------------------------------------------------------------
            success = .true.

            CALL LOG_HEADING( LOGDEV, "Retrieve Species Namelist" )

            ! Gas (GC) Namelist
            ALLOCATE( GC_SPECIES_DATA( SPC_DIM ), STAT=IOS )
            GC_SPECIES_DATA%SPECIES_NAME = ''
            GC_SPECIES_DATA%MOLWT = 0.
            GC_SPECIES_DATA%IC_SURR = ''
            GC_SPECIES_DATA%IC_FAC = 0.
            GC_SPECIES_DATA%BC_SURR = ''
            GC_SPECIES_DATA%BC_FAC = 0.
            GC_SPECIES_DATA%DEPV_SURR = ''
            GC_SPECIES_DATA%DEPV_FAC = 0.
            GC_SPECIES_DATA%SCAV_SURR = ''
            GC_SPECIES_DATA%SCAV_FAC = 0.
            GC_SPECIES_DATA%AERO_SURR = ''
            GC_SPECIES_DATA%CLOUD_SURR = ''
            GC_SPECIES_DATA%TRNS_FLAG = ''
            GC_SPECIES_DATA%DDEP_FLAG = ''
            GC_SPECIES_DATA%WDEP_FLAG = ''
            GC_SPECIES_DATA%CONC_FLAG = ''
                                                                                             
            dev_gc_nml = junit()                                                             
            open( file = trim( gc_namelist ), unit = dev_gc_nml,                             
     &            status = 'old', position = 'rewind' )                                      
            read( nml = GC_nml, unit = dev_gc_nml )                                          
                                                                                             
            ! Determine Number of Gas Species                                                
            N_GC_SPC = 0                                                                     
            DO IGC = 1,SPC_DIM                                                              
                IF ( GC_SPECIES_DATA( IGC )%SPECIES_NAME .EQ. '' ) EXIT                      
                N_GC_SPC = IGC
                IF ( GC_SPECIES_DATA( IGC )%IC_FAC .EQ. -1. )
     &               GC_SPECIES_DATA( IGC )%IC_FAC = 0.
                IF ( GC_SPECIES_DATA( IGC )%BC_FAC .EQ. -1. )
     &               GC_SPECIES_DATA( IGC )%BC_FAC = 0.
                IF ( GC_SPECIES_DATA( IGC )%DEPV_FAC .EQ. -1. )
     &               GC_SPECIES_DATA( IGC )%DEPV_FAC = 0.
                IF ( GC_SPECIES_DATA( IGC )%SCAV_FAC .EQ. -1. )
     &               GC_SPECIES_DATA( IGC )%SCAV_FAC = 0.
            END DO                                                                           
                                                                                             
            ! Allocate and Initialize Permanent Variables
            allocate ( 
     &      gc_spc( n_gc_spc+1 ), gc_molwt( n_gc_spc+1 ),
     &      gc_ic( n_gc_spc+1 ),   gc_ic_map( n_gc_spc+1 ),   gc_ic_fac( n_gc_spc+1 ),
     &      gc_bc( n_gc_spc+1 ),   gc_bc_map( n_gc_spc+1 ),   gc_bc_fac( n_gc_spc+1 ),
     &      gc_depv( n_gc_spc+1 ), gc_depv_map( n_gc_spc+1 ), gc_depv_fac( n_gc_spc+1 ),
     &      gc_scav( n_gc_spc+1 ), gc_scav_map( n_gc_spc+1 ), gc_scav_fac( n_gc_spc+1 ),
     &      gc_g2ae( n_gc_spc+1 ), gc_g2ae_map( n_gc_spc+1 ),
     &      gc_g2aq( n_gc_spc+1 ), gc_g2aq_map( n_gc_spc+1 ),
     &      gc_trns( n_gc_spc+1 ), gc_trns_map( n_gc_spc+1 ),
     &      gc_ddep( n_gc_spc+1 ), gc_ddep_map( n_gc_spc+1 ),
     &      gc_wdep( n_gc_spc+1 ), gc_wdep_map( n_gc_spc+1 ),
     &      gc_conc( n_gc_spc+1 ), gc_conc_map( n_gc_spc+1 ),
#ifdef mpas
     &      all_spc( spc_dim ),
#endif
     &      stat = ios )

#ifdef mpas
            all_spc = bl
#endif

            gc_spc = bl; gc_molwt = 0.0                        !  .
            gc_ic = bl;   gc_ic_map = 0;   gc_ic_fac = 0.0     !  .
            gc_bc = bl;   gc_bc_map = 0;   gc_bc_fac = 0.0     !  .
            gc_depv = bl; gc_depv_map = 0; gc_depv_fac = 0.0   !  .
            gc_scav = bl; gc_scav_map = 0; gc_scav_fac = 0.0   ! array
            gc_g2ae = bl; gc_g2ae_map = 0                      !  .
            gc_g2aq = bl; gc_g2aq_map = 0                      ! assignments
            gc_trns = bl; gc_trns_map = 0                      !  .
            gc_ddep = bl; gc_ddep_map = 0                      !  .
            gc_wdep = bl; gc_wdep_map = 0                      !  .
            gc_conc = bl; gc_conc_map = 0                      !  .


            ! Pass Data from GC Namelist to Internal Variables
            N_GC_IC = 0; N_GC_BC = 0; N_GC_DEPV = 0; N_GC_SCAV = 0
            N_GC_G2AE = 0; N_GC_G2AQ = 0; N_GC_TRNS = 0; N_GC_DDEP = 0; N_GC_WDEP = 0
            N_GC_CONC = 0

            DO IGC = 1,N_GC_SPC
                
                GC_SPC( IGC )      = GC_SPECIES_DATA( IGC )%SPECIES_NAME
                GC_MOLWT( IGC )    = GC_SPECIES_DATA( IGC )%MOLWT

                ! Gas ICs
                IF ( GC_SPECIES_DATA( IGC )%IC_SURR .NE. '' ) THEN
                    N_GC_IC = N_GC_IC + 1
                    GC_IC( N_GC_IC )     = GC_SPECIES_DATA( IGC )%IC_SURR 
                    GC_IC_MAP( N_GC_IC ) = IGC
                    GC_IC_FAC( N_GC_IC ) = GC_SPECIES_DATA( IGC )%IC_FAC
                END IF

                ! Gas BCs
                IF ( GC_SPECIES_DATA( IGC )%BC_SURR .NE. '' ) THEN
                    N_GC_BC = N_GC_BC + 1
                    GC_BC( N_GC_BC )     = GC_SPECIES_DATA( IGC )%BC_SURR 
                    GC_BC_MAP( N_GC_BC ) = IGC
                    GC_BC_FAC( N_GC_BC ) = GC_SPECIES_DATA( IGC )%BC_FAC
                END IF
                
                ! Gas Dry Deposition Surrogates
                IF ( GC_SPECIES_DATA( IGC )%DEPV_SURR .NE. '' ) THEN
                    N_GC_DEPV = N_GC_DEPV + 1
                    GC_DEPV( N_GC_DEPV )     = GC_SPECIES_DATA( IGC )%DEPV_SURR 
                    GC_DEPV_MAP( N_GC_DEPV ) = IGC
                    GC_DEPV_FAC( N_GC_DEPV ) = GC_SPECIES_DATA( IGC )%DEPV_FAC
                END IF
                 
                ! Gas Wet Scavenging Surrogates
                IF ( GC_SPECIES_DATA( IGC )%SCAV_SURR .NE. '' ) THEN
                    N_GC_SCAV = N_GC_SCAV + 1
                    GC_SCAV( N_GC_SCAV )     = GC_SPECIES_DATA( IGC )%SCAV_SURR 
                    GC_SCAV_MAP( N_GC_SCAV ) = IGC
                    GC_SCAV_FAC( N_GC_SCAV ) = GC_SPECIES_DATA( IGC )%SCAV_FAC
                END IF
                
                ! Gas -to-Aerosol Module Surrogates
                IF ( GC_SPECIES_DATA( IGC )%AERO_SURR .NE. '' ) THEN
                    N_GC_G2AE = N_GC_G2AE + 1
                    GC_G2AE( N_GC_G2AE )     = GC_SPECIES_DATA( IGC )%AERO_SURR 
                    GC_G2AE_MAP( N_GC_G2AE ) = IGC
                END IF
                
                ! Gas -to-Cloud Chemistry Surrogates
                IF ( GC_SPECIES_DATA( IGC )%CLOUD_SURR .NE. '' ) THEN
                    N_GC_G2AQ = N_GC_G2AQ + 1
                    GC_G2AQ( N_GC_G2AQ )     = GC_SPECIES_DATA( IGC )%CLOUD_SURR 
                    GC_G2AQ_MAP( N_GC_G2AQ ) = IGC
                END IF
                
                ! Gas - Perform Transport
                LTRNS = GET_LOGICAL_NML( GC_SPECIES_DATA( IGC )%TRNS_FLAG )
                IF ( LTRNS ) THEN
                    N_GC_TRNS = N_GC_TRNS + 1
                    GC_TRNS( N_GC_TRNS )     = GC_SPECIES_DATA( IGC )%SPECIES_NAME
                    GC_TRNS_MAP( N_GC_TRNS ) = IGC
                END IF
                
                ! Gas - Output Deposition Velocity
                LDDEP = GET_LOGICAL_NML( GC_SPECIES_DATA( IGC )%DDEP_FLAG )
                IF ( LDDEP ) THEN
                    N_GC_DDEP = N_GC_DDEP + 1
                    GC_DDEP( N_GC_DDEP )     = GC_SPECIES_DATA( IGC )%SPECIES_NAME
                    GC_DDEP_MAP( N_GC_DDEP ) = IGC
                END IF
                
                ! Gas - Output Wet Scavenging Flux
                LWDEP = GET_LOGICAL_NML( GC_SPECIES_DATA( IGC )%WDEP_FLAG )
                IF ( LWDEP ) THEN
                    N_GC_WDEP = N_GC_WDEP + 1
                    GC_WDEP( N_GC_WDEP )     = GC_SPECIES_DATA( IGC )%SPECIES_NAME
                    GC_WDEP_MAP( N_GC_WDEP ) = IGC
                END IF
                
                ! Gas - Output Concentration Field
                LCONC = GET_LOGICAL_NML( GC_SPECIES_DATA( IGC )%CONC_FLAG )
                IF ( LCONC ) THEN
                    N_GC_CONC = N_GC_CONC + 1
                    GC_CONC( N_GC_CONC )     = GC_SPECIES_DATA( IGC )%SPECIES_NAME
                    GC_CONC_MAP( N_GC_CONC ) = IGC
                END IF
                
            END DO

            GC_FINI   = GC_STRT + N_GC_SPC - 1
            N_GC_SPCD = GC_FINI + 1
            GC_SPC( N_GC_SPCD ) = 'AIR_DENSITY'
            RHOJ_LOC  = N_GC_SPCD


            ! Aerosol (AE) Namelist
            ALLOCATE( AE_SPECIES_DATA( SPC_DIM ), STAT=IOS )
            AE_SPECIES_DATA%SPECIES_NAME = ''
            AE_SPECIES_DATA%MOLWT = 0.
            AE_SPECIES_DATA%LAIT = .False.
            AE_SPECIES_DATA%LACC = .False.
            AE_SPECIES_DATA%LCOR = .False.
            AE_SPECIES_DATA%IC_SURR = ''
            AE_SPECIES_DATA%IC_FAC = 0.
            AE_SPECIES_DATA%BC_SURR = ''
            AE_SPECIES_DATA%BC_FAC = 0.
            AE_SPECIES_DATA%OPTICS_SURR = ''
            AE_SPECIES_DATA%DEPV_SURR = ''
            AE_SPECIES_DATA%DEPV_FAC = 0.
            AE_SPECIES_DATA%SCAV_SURR = ''
            AE_SPECIES_DATA%SCAV_FAC = 0.
            AE_SPECIES_DATA%CLOUD_SURR = ''
            AE_SPECIES_DATA%TRNS_FLAG = ''
            AE_SPECIES_DATA%DDEP_FLAG = ''
            AE_SPECIES_DATA%WDEP_FLAG = ''
            AE_SPECIES_DATA%CONC_FLAG = ''
                                                                                             

            dev_ae_nml = junit()
            open( file = trim( ae_namelist ), unit = dev_ae_nml,
     &            status = 'old', position = 'rewind' )
            read( nml = AE_nml, unit = dev_ae_nml ) 

            ! First Expand Aerosol Namelist to Add Modes for Each
            ! Species based on Aitken, Accum, and Coarse columns in the AE
            ! namelist
            CALL AERO_NML_MODES()

            ! Determine Number of Aerosol Species
            N_AE_SPC = 0
            DO IAE = 1,SPC_DIM
                IF ( AE_SPECIES_DATA( IAE )%SPECIES_NAME .EQ. '' ) EXIT
                N_AE_SPC = IAE
                IF ( AE_SPECIES_DATA( IAE )%IC_FAC .EQ. -1. )
     &               AE_SPECIES_DATA( IAE )%IC_FAC = 0.
                IF ( AE_SPECIES_DATA( IAE )%BC_FAC .EQ. -1. )
     &               AE_SPECIES_DATA( IAE )%BC_FAC = 0.
                IF ( AE_SPECIES_DATA( IAE )%DEPV_FAC .EQ. -1. )
     &               AE_SPECIES_DATA( IAE )%DEPV_FAC = 0.
                IF ( AE_SPECIES_DATA( IAE )%SCAV_FAC .EQ. -1. )
     &               AE_SPECIES_DATA( IAE )%SCAV_FAC = 0.
            END DO

            IF ( STM ) THEN
               IF ( ( N_AE_SPC + N_STM_AE_SPC ) .GT. SPC_DIM ) THEN
                  WRITE( LOGDEV,* ) '*** Allocated dimension exceeded for AE ', 
     &                              ' species in Sulfur Tracking Model' 
                  SUCCESS = .FALSE.
                  RETURN
               END IF
               
               DO IAE = 1, N_STM_AE_SPC
                  N_AE_SPC = N_AE_SPC + 1
                  AE_SPECIES_DATA( N_AE_SPC )%SPECIES_NAME = STM_AE_SPECIES_DATA( IAE )%SPECIES_NAME
                  AE_SPECIES_DATA( N_AE_SPC )%MOLWT        = STM_AE_SPECIES_DATA( IAE )%MOLWT
                  AE_SPECIES_DATA( N_AE_SPC )%IC_SURR      = STM_AE_SPECIES_DATA( IAE )%IC_SURR
                  AE_SPECIES_DATA( N_AE_SPC )%IC_FAC       = STM_AE_SPECIES_DATA( IAE )%IC_FAC
                  AE_SPECIES_DATA( N_AE_SPC )%BC_SURR      = STM_AE_SPECIES_DATA( IAE )%BC_SURR
                  AE_SPECIES_DATA( N_AE_SPC )%BC_FAC       = STM_AE_SPECIES_DATA( IAE )%BC_FAC
                  AE_SPECIES_DATA( N_AE_SPC )%DEPV_SURR    = STM_AE_SPECIES_DATA( IAE )%DEPV_SURR
                  AE_SPECIES_DATA( N_AE_SPC )%DEPV_FAC     = STM_AE_SPECIES_DATA( IAE )%DEPV_FAC
                  AE_SPECIES_DATA( N_AE_SPC )%SCAV_SURR    = STM_AE_SPECIES_DATA( IAE )%SCAV_SURR
                  AE_SPECIES_DATA( N_AE_SPC )%SCAV_FAC     = STM_AE_SPECIES_DATA( IAE )%SCAV_FAC
                  AE_SPECIES_DATA( N_AE_SPC )%CLOUD_SURR   = STM_AE_SPECIES_DATA( IAE )%CLOUD_SURR
                  AE_SPECIES_DATA( N_AE_SPC )%TRNS_FLAG    = STM_AE_SPECIES_DATA( IAE )%TRNS_FLAG
                  AE_SPECIES_DATA( N_AE_SPC )%DDEP_FLAG    = STM_AE_SPECIES_DATA( IAE )%DDEP_FLAG
                  AE_SPECIES_DATA( N_AE_SPC )%WDEP_FLAG    = STM_AE_SPECIES_DATA( IAE )%WDEP_FLAG
                  AE_SPECIES_DATA( N_AE_SPC )%CONC_FLAG    = STM_AE_SPECIES_DATA( IAE )%CONC_FLAG
               END DO

               If ( ( Index( mechname, 'SAPRC07TIC_AE6I' ) .Gt. 0 ) .OR.
     &              ( Index( mechname, 'SAPRC07TIC_AE7I' ) .Gt. 0 ) .OR.
     &              ( Index( mechname, 'CRACMM1_'        ) .Gt. 0 ) .OR.
     &              ( Index( mechname, 'CRACMM2'         ) .Gt. 0 ) .OR.
     &              ( Index( mechname, 'CRACMM1AMORE_'   ) .Gt. 0 ) .OR.
     &              ( Index( mechname, 'CB6R3_AE7'       ) .Gt. 0 ) .OR.
     &              ( Index( mechname, 'CB6R5_AE7'       ) .Gt. 0 ) .OR.
     &              ( Index( mechname, 'CB6R5M_AE7'      ) .Gt. 0 ) ) Then
                     
                  ORGANOSULF = .TRUE.

                  IF ( ( N_AE_SPC + N_STM_OAE_SPC ) .GT. SPC_DIM ) THEN
                     WRITE( LOGDEV,* ) '*** Allocated dimension exceeded for AE ', 
     &                                 ' species in Sulfur Tracking Model' 
                     SUCCESS = .FALSE.
                     RETURN
                  END IF
                  
                  DO IAE = 1, N_STM_OAE_SPC
                     N_AE_SPC = N_AE_SPC + 1
                     AE_SPECIES_DATA( N_AE_SPC )%SPECIES_NAME = STM_OAE_SPECIES_DATA( IAE )%SPECIES_NAME
                     AE_SPECIES_DATA( N_AE_SPC )%MOLWT        = STM_OAE_SPECIES_DATA( IAE )%MOLWT
                     AE_SPECIES_DATA( N_AE_SPC )%IC_SURR      = STM_OAE_SPECIES_DATA( IAE )%IC_SURR
                     AE_SPECIES_DATA( N_AE_SPC )%IC_FAC       = STM_OAE_SPECIES_DATA( IAE )%IC_FAC
                     AE_SPECIES_DATA( N_AE_SPC )%BC_SURR      = STM_OAE_SPECIES_DATA( IAE )%BC_SURR
                     AE_SPECIES_DATA( N_AE_SPC )%BC_FAC       = STM_OAE_SPECIES_DATA( IAE )%BC_FAC
                     AE_SPECIES_DATA( N_AE_SPC )%DEPV_SURR    = STM_OAE_SPECIES_DATA( IAE )%DEPV_SURR
                     AE_SPECIES_DATA( N_AE_SPC )%DEPV_FAC     = STM_OAE_SPECIES_DATA( IAE )%DEPV_FAC
                     AE_SPECIES_DATA( N_AE_SPC )%SCAV_SURR    = STM_OAE_SPECIES_DATA( IAE )%SCAV_SURR
                     AE_SPECIES_DATA( N_AE_SPC )%SCAV_FAC     = STM_OAE_SPECIES_DATA( IAE )%SCAV_FAC
                     AE_SPECIES_DATA( N_AE_SPC )%CLOUD_SURR   = STM_OAE_SPECIES_DATA( IAE )%CLOUD_SURR
                     AE_SPECIES_DATA( N_AE_SPC )%TRNS_FLAG    = STM_OAE_SPECIES_DATA( IAE )%TRNS_FLAG
                     AE_SPECIES_DATA( N_AE_SPC )%DDEP_FLAG    = STM_OAE_SPECIES_DATA( IAE )%DDEP_FLAG
                     AE_SPECIES_DATA( N_AE_SPC )%WDEP_FLAG    = STM_OAE_SPECIES_DATA( IAE )%WDEP_FLAG
                     AE_SPECIES_DATA( N_AE_SPC )%CONC_FLAG    = STM_OAE_SPECIES_DATA( IAE )%CONC_FLAG
                  END DO

               End If
            END IF

            ! Allocate and Initialize Permanent Variables
            allocate ( 
     &      ae_spc( n_ae_spc ), ae_molwt( n_ae_spc ),
     &      ae_ic( n_ae_spc ),   ae_ic_map( n_ae_spc ),   ae_ic_fac( n_ae_spc ),
     &      ae_bc( n_ae_spc ),   ae_bc_map( n_ae_spc ),   ae_bc_fac( n_ae_spc ),
     &      ae_optics( n_ae_spc ),
     &      ae_depv( n_ae_spc ), ae_depv_map( n_ae_spc ), ae_depv_fac( n_ae_spc ),
     &      ae_scav( n_ae_spc ), ae_scav_map( n_ae_spc ), ae_scav_fac( n_ae_spc ),
     &      ae_a2aq( n_ae_spc ), ae_a2aq_map( n_ae_spc ),
     &      ae_trns( n_ae_spc ), ae_trns_map( n_ae_spc ),
     &      ae_ddep( n_ae_spc ), ae_ddep_map( n_ae_spc ),
     &      ae_wdep( n_ae_spc ), ae_wdep_map( n_ae_spc ),
     &      ae_conc( n_ae_spc ), ae_conc_map( n_ae_spc ),
     &      stat = ios )

            ae_spc = bl; ae_molwt = 0.0                        !  .
            ae_optics = bl
            ae_ic = bl;   ae_ic_map = 0;   ae_ic_fac = 0.0     !  .
            ae_bc = bl;   ae_bc_map = 0;   ae_bc_fac = 0.0     !  .
            ae_depv = bl; ae_depv_map = 0; ae_depv_fac = 0.0   !  .
            ae_scav = bl; ae_scav_map = 0; ae_scav_fac = 0.0   ! array
            ae_a2aq = bl; ae_a2aq_map = 0                      !  .
            ae_trns = bl; ae_trns_map = 0                      ! assignments
            ae_ddep = bl; ae_ddep_map = 0                      !  .
            ae_wdep = bl; ae_wdep_map = 0                      !  .
            ae_conc = bl; ae_conc_map = 0                      !  .

            ! Pass Data from AE Namelist to Internal Variables
            N_AE_OPTICS = 0;N_AE_IC = 0; N_AE_BC = 0; N_AE_DEPV = 0; N_AE_SCAV = 0
            N_AE_A2AQ = 0; N_AE_TRNS = 0; N_AE_DDEP = 0; N_AE_WDEP = 0
            N_AE_CONC = 0

            DO IAE = 1,N_AE_SPC

                AE_SPC( IAE )      = AE_SPECIES_DATA( IAE )%SPECIES_NAME
                AE_MOLWT( IAE )    = AE_SPECIES_DATA( IAE )%MOLWT

                ! Aerosol ICs
                IF ( AE_SPECIES_DATA( IAE )%IC_SURR .NE. '' ) THEN
                    N_AE_IC = N_AE_IC + 1
                    AE_IC( N_AE_IC )     = AE_SPECIES_DATA( IAE )%IC_SURR 
                    AE_IC_MAP( N_AE_IC ) = IAE
                    AE_IC_FAC( N_AE_IC ) = AE_SPECIES_DATA( IAE )%IC_FAC
                END IF

                ! Aerosol BCs
                IF ( AE_SPECIES_DATA( IAE )%BC_SURR .NE. '' ) THEN
                    N_AE_BC = N_AE_BC + 1
                    AE_BC( N_AE_BC )     = AE_SPECIES_DATA( IAE )%BC_SURR 
                    AE_BC_MAP( N_AE_BC ) = IAE
                    AE_BC_FAC( N_AE_BC ) = AE_SPECIES_DATA( IAE )%BC_FAC
                END IF

                ! Aerosol Refractive Indices, blank if using default values in AERO_DATA module
                IF ( AE_SPECIES_DATA( IAE )%OPTICS_SURR .NE. '' ) THEN
                    N_AE_OPTICS = N_AE_OPTICS + 1
                    AE_OPTICS( IAE ) = AE_SPECIES_DATA( IAE )%OPTICS_SURR
                END IF 
                
                ! Aerosol Dry Deposition Surrogates
                IF ( AE_SPECIES_DATA( IAE )%DEPV_SURR .NE. '' ) THEN
                    N_AE_DEPV = N_AE_DEPV + 1
                    AE_DEPV( N_AE_DEPV )     = AE_SPECIES_DATA( IAE )%DEPV_SURR 
                    AE_DEPV_MAP( N_AE_DEPV ) = IAE
                    AE_DEPV_FAC( N_AE_DEPV ) = AE_SPECIES_DATA( IAE )%DEPV_FAC
                END IF
                 
                ! Aerosol Wet Scavenging Surrogates
                IF ( AE_SPECIES_DATA( IAE )%SCAV_SURR .NE. '' ) THEN
                    N_AE_SCAV = N_AE_SCAV + 1
                    AE_SCAV( N_AE_SCAV )     = AE_SPECIES_DATA( IAE )%SCAV_SURR 
                    AE_SCAV_MAP( N_AE_SCAV ) = IAE
                    AE_SCAV_FAC( N_AE_SCAV ) = AE_SPECIES_DATA( IAE )%SCAV_FAC
                END IF
                
                ! Aerosol -to-Cloud Chemistry Surrogates
                IF ( AE_SPECIES_DATA( IAE )%CLOUD_SURR .NE. '' ) THEN
                    N_AE_A2AQ = N_AE_A2AQ + 1
                    AE_A2AQ( N_AE_A2AQ )     = AE_SPECIES_DATA( IAE )%CLOUD_SURR 
                    AE_A2AQ_MAP( N_AE_A2AQ ) = IAE
                END IF
                
                ! Aerosol - Perform Transport
                LTRNS = GET_LOGICAL_NML( AE_SPECIES_DATA( IAE )%TRNS_FLAG )
                IF ( LTRNS ) THEN
                    N_AE_TRNS = N_AE_TRNS + 1
                    AE_TRNS( N_AE_TRNS )     = AE_SPECIES_DATA( IAE )%SPECIES_NAME
                    AE_TRNS_MAP( N_AE_TRNS ) = IAE
                END IF
                
                ! Aerosol - Output Deposition Velocity
                LDDEP = GET_LOGICAL_NML( AE_SPECIES_DATA( IAE )%DDEP_FLAG )
                IF ( LDDEP ) THEN
                    N_AE_DDEP = N_AE_DDEP + 1
                    AE_DDEP( N_AE_DDEP )     = AE_SPECIES_DATA( IAE )%SPECIES_NAME
                    AE_DDEP_MAP( N_AE_DDEP ) = IAE
                END IF
                
                ! Aerosol - Output Wet Scavenging Flux
                LWDEP = GET_LOGICAL_NML( AE_SPECIES_DATA( IAE )%WDEP_FLAG )
                IF ( LWDEP ) THEN
                    N_AE_WDEP = N_AE_WDEP + 1
                    AE_WDEP( N_AE_WDEP )     = AE_SPECIES_DATA( IAE )%SPECIES_NAME
                    AE_WDEP_MAP( N_AE_WDEP ) = IAE
                END IF
                
                ! Aerosol - Output Concentration Field
                LCONC = GET_LOGICAL_NML( AE_SPECIES_DATA( IAE )%CONC_FLAG )
                IF ( LCONC ) THEN
                    N_AE_CONC = N_AE_CONC + 1
                    AE_CONC( N_AE_CONC )     = AE_SPECIES_DATA( IAE )%SPECIES_NAME
                    AE_CONC_MAP( N_AE_CONC ) = IAE
                END IF
                
            END DO

            AE_STRT   = GC_FINI + 2
            AE_FINI   = AE_STRT + N_AE_SPC - 1
 


            ! Nonreactive (NR) Namelist
            ALLOCATE( NR_SPECIES_DATA( SPC_DIM ), STAT=IOS )
            NR_SPECIES_DATA%SPECIES_NAME = ''
            NR_SPECIES_DATA%MOLWT = 0.
            NR_SPECIES_DATA%IC_SURR = ''
            NR_SPECIES_DATA%IC_FAC = 0.
            NR_SPECIES_DATA%BC_SURR = ''
            NR_SPECIES_DATA%BC_FAC = 0.
            NR_SPECIES_DATA%DEPV_SURR = ''
            NR_SPECIES_DATA%DEPV_FAC = 0.
            NR_SPECIES_DATA%SCAV_SURR = ''
            NR_SPECIES_DATA%SCAV_FAC = 0.
            NR_SPECIES_DATA%AERO_SURR = ''
            NR_SPECIES_DATA%CLOUD_SURR = ''
            NR_SPECIES_DATA%TRNS_FLAG = ''
            NR_SPECIES_DATA%DDEP_FLAG = ''
            NR_SPECIES_DATA%WDEP_FLAG = ''
            NR_SPECIES_DATA%CONC_FLAG = ''

            dev_nr_nml = junit()
            open( file = trim( nr_namelist ), unit = dev_nr_nml,
     &            status = 'old', position = 'rewind' )
            read( nml = NR_nml, unit = dev_nr_nml ) 
           
            ! Determine Number of Gas Species
            N_NR_SPC = 0
            DO INR = 1,SPC_DIM
                IF ( NR_SPECIES_DATA( INR )%SPECIES_NAME .EQ. '' ) EXIT
                N_NR_SPC = INR
                IF ( NR_SPECIES_DATA( INR )%IC_FAC .EQ. -1. )
     &               NR_SPECIES_DATA( INR )%IC_FAC = 0.
                IF ( NR_SPECIES_DATA( INR )%BC_FAC .EQ. -1. )
     &               NR_SPECIES_DATA( INR )%BC_FAC = 0.
                IF ( NR_SPECIES_DATA( INR )%DEPV_FAC .EQ. -1. )
     &               NR_SPECIES_DATA( INR )%DEPV_FAC = 0.
                IF ( NR_SPECIES_DATA( INR )%SCAV_FAC .EQ. -1. )
     &               NR_SPECIES_DATA( INR )%SCAV_FAC = 0.
            END DO

            IF ( STM ) THEN
               IF ( ( N_NR_SPC + N_STM_NR_SPC ) .GT. SPC_DIM ) THEN
                  WRITE( LOGDEV,* ) '*** Allocated dimension exceeded for NR ', 
     &                              ' species in Sulfur Tracking Model' 
                  SUCCESS = .FALSE.
                  RETURN
               END IF
               
               DO INR = 1, N_STM_NR_SPC
                  N_NR_SPC = N_NR_SPC + 1
                  NR_SPECIES_DATA( N_NR_SPC )%SPECIES_NAME = STM_NR_SPECIES_DATA( INR )%SPECIES_NAME
                  NR_SPECIES_DATA( N_NR_SPC )%MOLWT        = STM_NR_SPECIES_DATA( INR )%MOLWT
                  NR_SPECIES_DATA( N_NR_SPC )%IC_SURR      = STM_NR_SPECIES_DATA( INR )%IC_SURR
                  NR_SPECIES_DATA( N_NR_SPC )%IC_FAC       = STM_NR_SPECIES_DATA( INR )%IC_FAC
                  NR_SPECIES_DATA( N_NR_SPC )%BC_SURR      = STM_NR_SPECIES_DATA( INR )%BC_SURR
                  NR_SPECIES_DATA( N_NR_SPC )%BC_FAC       = STM_NR_SPECIES_DATA( INR )%BC_FAC
                  NR_SPECIES_DATA( N_NR_SPC )%DEPV_SURR    = STM_NR_SPECIES_DATA( INR )%DEPV_SURR
                  NR_SPECIES_DATA( N_NR_SPC )%DEPV_FAC     = STM_NR_SPECIES_DATA( INR )%DEPV_FAC
                  NR_SPECIES_DATA( N_NR_SPC )%SCAV_SURR    = STM_NR_SPECIES_DATA( INR )%SCAV_SURR
                  NR_SPECIES_DATA( N_NR_SPC )%SCAV_FAC     = STM_NR_SPECIES_DATA( INR )%SCAV_FAC
                  NR_SPECIES_DATA( N_NR_SPC )%AERO_SURR    = STM_NR_SPECIES_DATA( INR )%AERO_SURR
                  NR_SPECIES_DATA( N_NR_SPC )%CLOUD_SURR   = STM_NR_SPECIES_DATA( INR )%CLOUD_SURR
                  NR_SPECIES_DATA( N_NR_SPC )%TRNS_FLAG    = STM_NR_SPECIES_DATA( INR )%TRNS_FLAG
                  NR_SPECIES_DATA( N_NR_SPC )%DDEP_FLAG    = STM_NR_SPECIES_DATA( INR )%DDEP_FLAG
                  NR_SPECIES_DATA( N_NR_SPC )%WDEP_FLAG    = STM_NR_SPECIES_DATA( INR )%WDEP_FLAG
                  NR_SPECIES_DATA( N_NR_SPC )%CONC_FLAG    = STM_NR_SPECIES_DATA( INR )%CONC_FLAG
               END DO
            END IF ! stm

            ! Allocate and Initialize Permanent Variables
            allocate ( 
     &      NR_spc( n_nr_spc ), NR_molwt( n_nr_spc ),
     &      NR_ic( n_nr_spc ),   NR_ic_map( n_nr_spc ),   NR_ic_fac( n_nr_spc ),
     &      NR_bc( n_nr_spc ),   NR_bc_map( n_nr_spc ),   NR_bc_fac( n_nr_spc ),
     &      NR_depv( n_nr_spc ), NR_depv_map( n_nr_spc ), NR_depv_fac( n_nr_spc ),
     &      NR_scav( n_nr_spc ), NR_scav_map( n_nr_spc ), NR_scav_fac( n_nr_spc ),
     &      NR_n2ae( n_nr_spc ), NR_n2ae_map( n_nr_spc ),
     &      NR_n2aq( n_nr_spc ), NR_n2aq_map( n_nr_spc ),
     &      NR_trns( n_nr_spc ), NR_trns_map( n_nr_spc ),
     &      NR_ddep( n_nr_spc ), NR_ddep_map( n_nr_spc ),
     &      NR_wdep( n_nr_spc ), NR_wdep_map( n_nr_spc ),
     &      NR_conc( n_nr_spc ), NR_conc_map( n_nr_spc ),
     &      stat = ios )

            NR_spc = bl; NR_molwt = 0.0                        !  .
            NR_ic = bl;   NR_ic_map = 0;   NR_ic_fac = 0.0     !  .
            NR_bc = bl;   NR_bc_map = 0;   NR_bc_fac = 0.0     !  .
            NR_depv = bl; NR_depv_map = 0; NR_depv_fac = 0.0   !  .
            NR_scav = bl; NR_scav_map = 0; NR_scav_fac = 0.0   ! array
            NR_n2ae = bl; NR_n2ae_map = 0                      !  .
            NR_n2aq = bl; NR_n2aq_map = 0                      !  .
            NR_trns = bl; NR_trns_map = 0                      ! assignments
            NR_ddep = bl; NR_ddep_map = 0                      !  .
            NR_wdep = bl; NR_wdep_map = 0                      !  .
            NR_conc = bl; NR_conc_map = 0                      !  .

            ! Pass Data from NR Namelist to Internal Variables
            N_NR_IC = 0; N_NR_BC = 0; N_NR_DEPV = 0; N_NR_SCAV = 0
            N_NR_N2AE = 0; N_NR_N2AQ = 0; N_NR_TRNS = 0; N_NR_DDEP = 0; N_NR_WDEP = 0
            N_NR_CONC = 0

            DO INR = 1,N_NR_SPC

                NR_SPC( INR )      = NR_SPECIES_DATA( INR )%SPECIES_NAME
                NR_MOLWT( INR )    = NR_SPECIES_DATA( INR )%MOLWT

                ! Nonreactive ICs
                IF ( NR_SPECIES_DATA( INR )%IC_SURR .NE. '' ) THEN
                    N_NR_IC = N_NR_IC + 1
                    NR_IC( N_NR_IC )     = NR_SPECIES_DATA( INR )%IC_SURR 
                    NR_IC_MAP( N_NR_IC ) = INR
                    NR_IC_FAC( N_NR_IC ) = NR_SPECIES_DATA( INR )%IC_FAC
                END IF

                ! Nonreactive BCs
                IF ( NR_SPECIES_DATA( INR )%BC_SURR .NE. '' ) THEN
                    N_NR_BC = N_NR_BC + 1
                    NR_BC( N_NR_BC )     = NR_SPECIES_DATA( INR )%BC_SURR 
                    NR_BC_MAP( N_NR_BC ) = INR
                    NR_BC_FAC( N_NR_BC ) = NR_SPECIES_DATA( INR )%BC_FAC
                END IF
                
                ! Nonreactive Dry Deposition Surrogates
                IF ( NR_SPECIES_DATA( INR )%DEPV_SURR .NE. '' ) THEN
                    N_NR_DEPV = N_NR_DEPV + 1
                    NR_DEPV( N_NR_DEPV )     = NR_SPECIES_DATA( INR )%DEPV_SURR 
                    NR_DEPV_MAP( N_NR_DEPV ) = INR
                    NR_DEPV_FAC( N_NR_DEPV ) = NR_SPECIES_DATA( INR )%DEPV_FAC
                END IF
                 
                ! Nonreactive Wet Scavenging Surrogates
                IF ( NR_SPECIES_DATA( INR )%SCAV_SURR .NE. '' ) THEN
                    N_NR_SCAV = N_NR_SCAV + 1
                    NR_SCAV( N_NR_SCAV )     = NR_SPECIES_DATA( INR )%SCAV_SURR 
                    NR_SCAV_MAP( N_NR_SCAV ) = INR
                    NR_SCAV_FAC( N_NR_SCAV ) = NR_SPECIES_DATA( INR )%SCAV_FAC
                END IF
 
                ! Nonreactive -to-Aerosol Module Surrogates
                IF ( NR_SPECIES_DATA( INR )%AERO_SURR .NE. '' ) THEN
                    N_NR_N2AE = N_NR_N2AE + 1
                    NR_N2AE( N_NR_N2AE )     = NR_SPECIES_DATA( INR )%AERO_SURR 
                    NR_N2AE_MAP( N_NR_N2AE ) = INR
                END IF
                
                ! Nonreactive -to-Cloud Chemistry Surrogates
                IF ( NR_SPECIES_DATA( INR )%CLOUD_SURR .NE. '' ) THEN
                    N_NR_N2AQ = N_NR_N2AQ + 1
                    NR_N2AQ( N_NR_N2AQ )     = NR_SPECIES_DATA( INR )%CLOUD_SURR 
                    NR_N2AQ_MAP( N_NR_N2AQ ) = INR
                END IF
                
                ! Nonreactive - Perform Transport
                LTRNS = GET_LOGICAL_NML( NR_SPECIES_DATA( INR )%TRNS_FLAG )
                IF ( LTRNS ) THEN
                    N_NR_TRNS = N_NR_TRNS + 1
                    NR_TRNS( N_NR_TRNS )     = NR_SPECIES_DATA( INR )%SPECIES_NAME
                    NR_TRNS_MAP( N_NR_TRNS ) = INR
                END IF
                
                ! Nonreactive - Output Deposition Velocity
                LDDEP = GET_LOGICAL_NML( NR_SPECIES_DATA( INR )%DDEP_FLAG )
                IF ( LDDEP ) THEN
                    N_NR_DDEP = N_NR_DDEP + 1
                    NR_DDEP( N_NR_DDEP )     = NR_SPECIES_DATA( INR )%SPECIES_NAME
                    NR_DDEP_MAP( N_NR_DDEP ) = INR
                END IF
                
                ! Nonreactive - Output Wet Scavenging Flux
                LWDEP = GET_LOGICAL_NML( NR_SPECIES_DATA( INR )%WDEP_FLAG )
                IF ( LWDEP ) THEN
                    N_NR_WDEP = N_NR_WDEP + 1
                    NR_WDEP( N_NR_WDEP )     = NR_SPECIES_DATA( INR )%SPECIES_NAME
                    NR_WDEP_MAP( N_NR_WDEP ) = INR
                END IF
                
                ! Nonreactive - Output Concentration Field
                LCONC = GET_LOGICAL_NML( NR_SPECIES_DATA( INR )%CONC_FLAG )
                IF ( LCONC ) THEN
                    N_NR_CONC = N_NR_CONC + 1
                    NR_CONC( N_NR_CONC )     = NR_SPECIES_DATA( INR )%SPECIES_NAME
                    NR_CONC_MAP( N_NR_CONC ) = INR
                END IF
                
            END DO

            NR_STRT   = AE_FINI + 1
            NR_FINI   = NR_STRT + N_NR_SPC - 1
 

            ! Tracer (TR) Namelists
            ALLOCATE( TR_SPECIES_DATA( SPC_DIM ), STAT=IOS )
            TR_SPECIES_DATA%SPECIES_NAME = ''
            TR_SPECIES_DATA%MOLWT = 0.
            TR_SPECIES_DATA%IC_SURR = ''
            TR_SPECIES_DATA%IC_FAC = 0.
            TR_SPECIES_DATA%BC_SURR = ''
            TR_SPECIES_DATA%BC_FAC = 0.
            TR_SPECIES_DATA%DEPV_SURR = ''
            TR_SPECIES_DATA%DEPV_FAC = 0.
            TR_SPECIES_DATA%SCAV_SURR = ''
            TR_SPECIES_DATA%SCAV_FAC = 0.
            TR_SPECIES_DATA%AERO_SURR = ''
            TR_SPECIES_DATA%CLOUD_SURR = ''
            TR_SPECIES_DATA%ADVC_FLAG = ''
            TR_SPECIES_DATA%DIFF_FLAG = ''
            TR_SPECIES_DATA%DDEP_FLAG = ''
            TR_SPECIES_DATA%WDEP_FLAG = ''
            TR_SPECIES_DATA%CONC_FLAG = ''



            dev_tr_nml = junit()
            open( file = trim( tr_namelist ), unit = dev_tr_nml,
     &            status = 'old', position = 'rewind' )
            read( nml = TR_nml, unit = dev_tr_nml )

            ! Determine Number of Gas Species
            N_TR_SPC = 0
            DO ITR = 1,SPC_DIM
                IF ( TR_SPECIES_DATA( ITR )%SPECIES_NAME .EQ. '' ) EXIT
                N_TR_SPC = ITR
                IF ( TR_SPECIES_DATA( ITR )%IC_FAC .EQ. -1. )
     &               TR_SPECIES_DATA( ITR )%IC_FAC = 0.
                IF ( TR_SPECIES_DATA( ITR )%BC_FAC .EQ. -1. )
     &               TR_SPECIES_DATA( ITR )%BC_FAC = 0.
                IF ( TR_SPECIES_DATA( ITR )%DEPV_FAC .EQ. -1. )
     &               TR_SPECIES_DATA( ITR )%DEPV_FAC = 0.
                IF ( TR_SPECIES_DATA( ITR )%SCAV_FAC .EQ. -1. )
     &               TR_SPECIES_DATA( ITR )%SCAV_FAC = 0.
            END DO

            ! Allocate and Initialize Permanent Variables
            allocate ( 
     &      TR_spc( n_tr_spc ), TR_molwt( n_tr_spc ),
     &      TR_ic( n_tr_spc ),   TR_ic_map( n_tr_spc ),   TR_ic_fac( n_tr_spc ),
     &      TR_bc( n_tr_spc ),   TR_bc_map( n_tr_spc ),   TR_bc_fac( n_tr_spc ),
     &      TR_depv( n_tr_spc ), TR_depv_map( n_tr_spc ), TR_depv_fac( n_tr_spc ),
     &      TR_scav( n_tr_spc ), TR_scav_map( n_tr_spc ), TR_scav_fac( n_tr_spc ),
     &      TR_t2ae( n_tr_spc ), TR_t2ae_map( n_tr_spc ),
     &      TR_t2aq( n_tr_spc ), TR_t2aq_map( n_tr_spc ),
     &      TR_adv( n_tr_spc ),  TR_adv_map( n_tr_spc ),
     &      TR_diff( n_tr_spc ), TR_diff_map( n_tr_spc ),
     &      TR_ddep( n_tr_spc ), TR_ddep_map( n_tr_spc ),
     &      TR_wdep( n_tr_spc ), TR_wdep_map( n_tr_spc ),
     &      TR_conc( n_tr_spc ), TR_conc_map( n_tr_spc ),
     &      stat = ios )

            TR_spc = bl; TR_molwt = 0.0                        !  .
            TR_ic = bl;   TR_ic_map = 0;   TR_ic_fac = 0.0     !  .
            TR_bc = bl;   TR_bc_map = 0;   TR_bc_fac = 0.0     !  .
            TR_depv = bl; TR_depv_map = 0; TR_depv_fac = 0.0   !  .
            TR_scav = bl; TR_scav_map = 0; TR_scav_fac = 0.0   ! array
            TR_t2ae = bl; TR_t2ae_map = 0                      !  .
            TR_t2aq = bl; TR_t2aq_map = 0                      !  .
            TR_adv  = bl; TR_adv_map = 0                       ! assignments
            TR_diff = bl; TR_diff_map = 0                      ! assignments
            TR_ddep = bl; TR_ddep_map = 0                      !  .
            TR_wdep = bl; TR_wdep_map = 0                      !  .
            TR_conc = bl; TR_conc_map = 0                      !  .

            ! Pass Data from TR Namelist to Internal Variables
            N_TR_IC = 0; N_TR_BC = 0; N_TR_DEPV = 0; N_TR_SCAV = 0
            N_TR_T2AE = 0; N_TR_T2AQ = 0; N_TR_ADV = 0;  N_TR_DDEP = 0; N_TR_WDEP = 0
            N_TR_CONC = 0; N_TR_DIFF = 0

            DO ITR = 1,N_TR_SPC

                TR_SPC( ITR )      = TR_SPECIES_DATA( ITR )%SPECIES_NAME
                TR_MOLWT( ITR )    = TR_SPECIES_DATA( ITR )%MOLWT

                ! Tracer IC
                IF ( TR_SPECIES_DATA( ITR )%IC_SURR .NE. '' ) THEN
                    N_TR_IC = N_TR_IC + 1
                    TR_IC( N_TR_IC )     = TR_SPECIES_DATA( ITR )%IC_SURR 
                    TR_IC_MAP( N_TR_IC ) = ITR
                    TR_IC_FAC( N_TR_IC ) = TR_SPECIES_DATA( ITR )%IC_FAC
                END IF

                ! Tracer BC
                IF ( TR_SPECIES_DATA( ITR )%BC_SURR .NE. '' ) THEN
                    N_TR_BC = N_TR_BC + 1
                    TR_BC( N_TR_BC )     = TR_SPECIES_DATA( ITR )%BC_SURR 
                    TR_BC_MAP( N_TR_BC ) = ITR
                    TR_BC_FAC( N_TR_BC ) = TR_SPECIES_DATA( ITR )%BC_FAC
                END IF
                                
                ! Tracer Dry Deposition Surrogates
                IF ( TR_SPECIES_DATA( ITR )%DEPV_SURR .NE. '' ) THEN
                    N_TR_DEPV = N_TR_DEPV + 1
                    TR_DEPV( N_TR_DEPV )     = TR_SPECIES_DATA( ITR )%DEPV_SURR 
                    TR_DEPV_MAP( N_TR_DEPV ) = ITR
                    TR_DEPV_FAC( N_TR_DEPV ) = TR_SPECIES_DATA( ITR )%DEPV_FAC
                END IF
                 
                ! Tracer Wet Scavenging Surrogates
                IF ( TR_SPECIES_DATA( ITR )%SCAV_SURR .NE. '' ) THEN
                    N_TR_SCAV = N_TR_SCAV + 1
                    TR_SCAV( N_TR_SCAV )     = TR_SPECIES_DATA( ITR )%SCAV_SURR 
                    TR_SCAV_MAP( N_TR_SCAV ) = ITR
                    TR_SCAV_FAC( N_TR_SCAV ) = TR_SPECIES_DATA( ITR )%SCAV_FAC
                END IF
 
                ! Tracer -to-Aerosol Module Surrogates
                IF ( TR_SPECIES_DATA( ITR )%AERO_SURR .NE. '' ) THEN
                    N_TR_T2AE = N_TR_T2AE + 1
                    TR_T2AE( N_TR_T2AE )     = TR_SPECIES_DATA( ITR )%AERO_SURR 
                    TR_T2AE_MAP( N_TR_T2AE ) = ITR
                END IF
                
                ! Tracer -to-Cloud Chemistry Surrogates
                IF ( TR_SPECIES_DATA( ITR )%CLOUD_SURR .NE. '' ) THEN
                    N_TR_T2AQ = N_TR_T2AQ + 1
                    TR_T2AQ( N_TR_T2AQ )     = TR_SPECIES_DATA( ITR )%CLOUD_SURR 
                    TR_T2AQ_MAP( N_TR_T2AQ ) = ITR
                END IF
                
                ! Tracer - Perform Advection?
                LADVC = GET_LOGICAL_NML( TR_SPECIES_DATA( ITR )%ADVC_FLAG )
                IF ( LADVC ) THEN
                    N_TR_ADV = N_TR_ADV + 1
                    TR_ADV( N_TR_ADV )     = TR_SPECIES_DATA( ITR )%SPECIES_NAME
                    TR_ADV_MAP( N_TR_ADV ) = ITR
                END IF
                
                ! Tracer - Perform Dispersion (i.e. Diffusion)?
                LDIFF = GET_LOGICAL_NML( TR_SPECIES_DATA( ITR )%DIFF_FLAG )
                IF ( LDIFF ) THEN
                    N_TR_DIFF = N_TR_DIFF + 1
                    TR_DIFF( N_TR_DIFF )     = TR_SPECIES_DATA( ITR )%SPECIES_NAME
                    TR_DIFF_MAP( N_TR_DIFF ) = ITR
                END IF
                
                ! Tracer - Output Deposition Velocity
                LDDEP = GET_LOGICAL_NML( TR_SPECIES_DATA( ITR )%DDEP_FLAG )
                IF ( LDDEP ) THEN
                    N_TR_DDEP = N_TR_DDEP + 1
                    TR_DDEP( N_TR_DDEP )     = TR_SPECIES_DATA( ITR )%SPECIES_NAME
                    TR_DDEP_MAP( N_TR_DDEP ) = ITR
                END IF
                
                ! Tracer - Output Wet Scavenging Flux
                LWDEP = GET_LOGICAL_NML( TR_SPECIES_DATA( ITR )%WDEP_FLAG )
                IF ( LWDEP ) THEN
                    N_TR_WDEP = N_TR_WDEP + 1
                    TR_WDEP( N_TR_WDEP )     = TR_SPECIES_DATA( ITR )%SPECIES_NAME
                    TR_WDEP_MAP( N_TR_WDEP ) = ITR
                END IF
                
                ! Tracer - Output Concentration Field
                LCONC = GET_LOGICAL_NML( TR_SPECIES_DATA( ITR )%CONC_FLAG )
                IF ( LCONC ) THEN
                    N_TR_CONC = N_TR_CONC + 1
                    TR_CONC( N_TR_CONC )     = TR_SPECIES_DATA( ITR )%SPECIES_NAME
                    TR_CONC_MAP( N_TR_CONC ) = ITR
                END IF
                
            END DO

            TR_STRT   = NR_FINI + 1
            TR_FINI   = TR_STRT + N_TR_SPC - 1
 
            
            !--------------------------------------------------------------
            ! Process Chemical Namelist Informaiton to CGRID Vectors
            !--------------------------------------------------------------

            ! Sum Up All Species Across Phases and Types
            NSPCSD = N_GC_SPCD + N_AE_SPC + N_NR_SPC + N_TR_SPC

#ifdef mpas
            n_all_spc = n_gc_spc + n_ae_spc + n_nr_spc + n_tr_spc
            do i1 = 1, n_gc_spc
               all_spc(i1) = gc_spc(i1)
            end do

            i1 = i1 - 1
            do i2 = 1, n_ae_spc
               all_spc(i1+i2) = ae_spc(i2)
            end do

            i1 = i1 + i2 - 1
            do i3 = 1, n_nr_spc
               all_spc(i1+i3) = nr_spc(i3)
            end do

            i1 = i1 + i3 - 1
            do i4 = 1, n_tr_spc
               all_spc(i1+i4) = nr_spc(i4)
            end do
#endif

            
            ! Move Data To CGRID_xxx Master Arrays
            N_CGRID_SPC = NSPCSD
            ALLOCATE( CGRID_NAME ( N_CGRID_SPC ),
     &                CGRID_MW   ( N_CGRID_SPC ),
     &                CGRID_UNITS( N_CGRID_SPC ),
     &                CGRID_MASK_GAS( N_CGRID_SPC ),
     &                CGRID_MASK_AERO( N_CGRID_SPC ),
     &                CGRID_MASK_NUM( N_CGRID_SPC ),
     &                CGRID_MASK_SRF( N_CGRID_SPC ),
     &                CGRID_MASK_NR( N_CGRID_SPC ),
     &                CGRID_MASK_TRAC( N_CGRID_SPC ),
     &                CGRID_CONC_UNIT( N_CGRID_SPC ),
     &                CGRID_CONC_DESC( N_CGRID_SPC ),
     &                MAP_DEPVtoCGRID( N_CGRID_SPC ),
     &                MAP_WDEPtoCGRID( N_CGRID_SPC ) )
            CGRID_NAME = ''
            CGRID_MW   = 0.
            CGRID_MASK_GAS = .FALSE.
            CGRID_MASK_AERO= .FALSE.
            CGRID_MASK_NUM = .FALSE.
            CGRID_MASK_SRF = .FALSE.
            CGRID_MASK_NR  = .FALSE.
            CGRID_MASK_TRAC= .FALSE.
            CGRID_CONC_UNIT= ''
            CGRID_CONC_DESC= ''
            MAP_DEPVtoCGRID= 0
            MAP_WDEPtoCGRID= 0
            CGRID_UNITS = ''

            IGRID = 0
            IWDEP = 0
            IDEPV = 0
            ! Set Masks for Reactive Gases
            DO ISPC = gc_strt,n_gc_spc
                IGRID = IGRID + 1
                CGRID_NAME( IGRID ) = GC_SPC( ISPC )
                CGRID_MW( IGRID )   = GC_MOLWT( ISPC )
                CGRID_MASK_GAS( IGRID ) = .TRUE.
                CGRID_CONC_UNIT( IGRID )  = 'ppmV'
                CGRID_UNITS( IGRID ) = 'ppmV'
                CGRID_CONC_DESC( IGRID )  = 'Molar Mixing Ratio of ' // CGRID_NAME(IGRID)
                DO JSPC = 1,N_GC_DEPV
                  IF ( GC_DEPV_MAP( JSPC ) .EQ. ISPC ) THEN
                    IDEPV = IDEPV + 1
                    MAP_DEPVtoCGRID( IDEPV ) = IGRID
                  END IF
                END DO
                DO JSPC = 1,N_GC_WDEP
                  IF ( GC_WDEP_MAP( JSPC ) .EQ. ISPC ) THEN
                    IWDEP = IWDEP + 1
                    MAP_WDEPtoCGRID( IWDEP ) = IGRID
                  END IF
                ENDDO
            END DO

            ! Set Masks for Density x Jacobian/MSFX2
            CGRID_NAME( RHOJ_LOC ) = 'RHOJ'
            CGRID_MW( RHOJ_LOC )   = 28.96
            CGRID_MASK_GAS( RHOJ_LOC ) = .TRUE.
            IGRID = IGRID + 1

            ! Set Masks for Aerosols
            DO ISPC = 1,n_ae_spc
                IGRID = IGRID + 1
                CGRID_NAME( IGRID ) = AE_SPC( ISPC )
                CGRID_MW( IGRID )   = AE_MOLWT( ISPC )
                CGRID_MASK_AERO( IGRID )   = .TRUE.
                CGRID_CONC_UNIT( IGRID )  = 'ug m-3'
                CGRID_CONC_DESC( IGRID )  = 'PM Mass Concentration of ' // CGRID_NAME(IGRID)
                IF ( INDEX( AE_SPC( ISPC ),'NUM') .NE. 0 ) THEN
                    CGRID_MASK_NUM( IGRID )   = .TRUE.
                    CGRID_CONC_UNIT( IGRID )  = 'm-3'
                    CGRID_CONC_DESC( IGRID )  = 'PM Number Concentration of ' // CGRID_NAME(IGRID)
                END IF
                IF ( INDEX( AE_SPC( ISPC ),'SRF') .NE. 0 ) THEN
                    CGRID_MASK_SRF( IGRID )   = .TRUE.
                    CGRID_CONC_UNIT( IGRID )  = 'm2 m-3'
                    CGRID_CONC_DESC( IGRID )  = 'PM Surface Area Concentration of ' // CGRID_NAME(IGRID)
                END IF
                DO JSPC = 1,N_AE_DEPV
                  IF ( AE_DEPV_MAP( JSPC ) .EQ. ISPC ) THEN
                    IDEPV = IDEPV + 1
                    MAP_DEPVtoCGRID( IDEPV ) = IGRID
                  END IF
                END DO
                DO JSPC = 1,N_AE_WDEP
                  IF ( AE_WDEP_MAP( JSPC ) .EQ. ISPC ) THEN
                    IWDEP = IWDEP + 1
                    MAP_WDEPtoCGRID( IWDEP ) = IGRID
                  END IF
                ENDDO
                CGRID_UNITS( IGRID ) = 'ug m-3'
                IF ( INDEX( AE_SPC( ISPC ),'NUM') .NE. 0 ) THEN 
                    CGRID_MASK_NUM( IGRID )   = .TRUE.
                    CGRID_UNITS( IGRID ) = 'm-3'
                END IF
                IF ( INDEX( AE_SPC( ISPC ),'SRF') .NE. 0 ) THEN
                    CGRID_MASK_SRF( IGRID )   = .TRUE.
                    CGRID_UNITS( IGRID ) = 'm2 m-3'
                END IF
            END DO
            
            ! Set Masks for Nonreactive Variables
            DO ISPC = 1,n_nr_spc
                IGRID = IGRID + 1
                CGRID_NAME( IGRID ) = NR_SPC( ISPC )
                CGRID_UNITS( IGRID ) = 'ppmV'
                CGRID_MW( IGRID )   = NR_MOLWT( ISPC )
                CGRID_MASK_NR( IGRID )   = .TRUE.
                CGRID_CONC_UNIT( IGRID )  = 'ppmV'
                CGRID_UNITS( IGRID ) = 'ppmV'
                CGRID_CONC_DESC( IGRID )  = 'Molar Mixing Ratio of ' // CGRID_NAME(IGRID)
                DO JSPC = 1,N_NR_DEPV
                  IF ( NR_DEPV_MAP( JSPC ) .EQ. ISPC ) THEN
                    IDEPV = IDEPV + 1
                    MAP_DEPVtoCGRID( IDEPV ) = IGRID
                  END IF
                END DO
                DO JSPC = 1,N_NR_WDEP
                  IF ( NR_WDEP_MAP( JSPC ) .EQ. ISPC ) THEN
                    IWDEP = IWDEP + 1
                    MAP_WDEPtoCGRID( IWDEP ) = IGRID
                  END IF
                ENDDO
            END DO

            ! Set Masks for Tracers
            DO ISPC = 1,n_tr_spc
                IGRID = IGRID + 1
                CGRID_NAME( IGRID ) = TR_SPC( ISPC )
                CGRID_UNITS( IGRID ) = 'ppmV'
                CGRID_MW( IGRID )   = TR_MOLWT( ISPC )
                CGRID_MASK_TRAC( IGRID )   = .TRUE.
                CGRID_CONC_UNIT( IGRID )  = 'ppmV'
                CGRID_UNITS( IGRID ) = 'ppmV'
                CGRID_CONC_DESC( IGRID )  = 'Molar Mixing Ratio of ' // CGRID_NAME(IGRID)
                DO JSPC = 1,N_TR_DEPV
                  IF ( TR_DEPV_MAP( JSPC ) .EQ. ISPC ) THEN
                    IDEPV = IDEPV + 1
                    MAP_DEPVtoCGRID( IDEPV ) = IGRID
                  END IF
                END DO
                DO JSPC = 1,N_TR_WDEP
                  IF ( TR_WDEP_MAP( JSPC ) .EQ. ISPC ) THEN
                    IWDEP = IWDEP + 1
                    MAP_WDEPtoCGRID( IWDEP ) = IGRID
                  END IF
                ENDDO
            END DO

            N_SPC_WDEP = IWDEP
            N_SPC_DEPV = IDEPV

         return

         end function cgrid_spcs_init

c----------------------------------------------------------------------------------------
         FUNCTION GET_LOGICAL_NML( OP_FLAG ) RESULT ( LFLAG )

         IMPLICIT NONE

         CHARACTER( 16 ) :: OP_FLAG
         LOGICAL         :: LFLAG

         LFLAG = .FALSE.

         IF ( OP_FLAG .EQ. 'yes' .OR. OP_FLAG .EQ. 'Yes' .OR.
     &        OP_FLAG .EQ. 'YES' .OR. OP_FLAG .EQ. 'Y'   .OR.
     &        OP_FLAG .EQ. 'y'   .OR. OP_FLAG .EQ. 'T'   .OR.
     &        OP_FLAG .EQ. 't'   .OR. OP_FLAG .EQ. 'on'  .OR.
     &        OP_FLAG .EQ. 'On'  .OR. OP_FLAG .EQ. 'ON'  .OR.
     &        OP_FLAG .EQ. '1'        ) 
     &     LFLAG = .TRUE.
   
         RETURN

         END FUNCTION GET_LOGICAL_NML

c----------------------------------------------------------------------------------------
            subroutine bubble ( n, ia, aa )

            use UTILIO_DEFN

            implicit none

            integer, intent( in ) :: n
            integer, intent( in out ) :: ia( n )
            character( * ), intent( in out ) :: aa( n )
            character( 8 ) :: at
            integer i, it
            logical :: no_swap

            do
               no_swap = .true.
               do i = 1, n - 1
                   if ( ia( i ) .gt. ia( i+1 ) ) then
                      it = ia( i ); ia( i ) = ia( i+1 ); ia( i+1 ) = it
                      at = aa( i ); aa( i ) = aa( i+1 ); aa( i+1 ) = at
                      no_swap = .false.
                   end if
               end do
               if ( no_swap ) exit
             end do

            it = 0
            do i = 1, n
               if ( ia( i ) .gt. 0 ) then
                  it = it + 1
                  ia( i ) = it
               else
                  aa( i ) = ' '
               end if
            end do

c compress list; move blanks to end
            it = 1
            do i = 1, n
               if ( aa( i ) .ne. ' ' ) then
                  aa( it ) = aa( i )
                  ia( it ) = ia( i )
                  it = it + 1
               end if
            end do
            do i = it, n
               aa( i ) = ' '
               ia( i ) = 0
            end do

            return
            end subroutine bubble

c----------------------------------------------------------------------------------------
            integer function index1a ( name, n, nlist )
            implicit none
            character( * ) name        ! character string being searched for
            integer n                  ! length of array to be searched
            character( * ) nlist( : )  ! array to be searched

            integer i

            do i = 1, n
               if ( name .eq. nlist( i ) ) then
                  index1a = i
                  return
               end if
           end do
           index1a = 0
           return

           end function index1a

      end module cgrid_spcs

