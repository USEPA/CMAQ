
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
C 10 Apr 19 F. Sidi  : Split up ICBC for CMAQ namelist species
c-----------------------------------------------------------------------
      
      !USE RUNTIME_VARS

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
      character( 16 ), allocatable, save :: gc_ic( : ),   ae_ic( : ),   nr_ic( : ),   tr_ic( : ),
     &                                      gc_bc( : ),   ae_bc( : ),   nr_bc( : ),   tr_bc( : ),
     &                                      gc_depv( : ), ae_depv( : ), nr_depv( : ), tr_depv( : ),
     &                                      gc_scav( : ), ae_scav( : ), nr_scav( : ), tr_scav( : ),
     &                                      gc_g2ae( : ),               nr_n2ae( : ), tr_t2ae( : ),
     &                                      gc_g2aq( : ), ae_a2aq( : ), nr_n2aq( : ), tr_t2aq( : )
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


      INTEGER, SAVE              :: N_CGRID_SPC
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: CGRID_NAME     ( : )
      REAL,    ALLOCATABLE, SAVE :: CGRID_MW       ( : )
      LOGICAL, ALLOCATABLE, SAVE :: CGRID_MASK_GAS ( : )
      LOGICAL, ALLOCATABLE, SAVE :: CGRID_MASK_AERO( : )
      LOGICAL, ALLOCATABLE, SAVE :: CGRID_MASK_NUM ( : )
      LOGICAL, ALLOCATABLE, SAVE :: CGRID_MASK_SRF ( : )
      LOGICAL, ALLOCATABLE, SAVE :: CGRID_MASK_NR  ( : )
      LOGICAL, ALLOCATABLE, SAVE :: CGRID_MASK_TRAC( : )


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
             
            USE GET_ENV_VARS
            USE MECHANISM_DATA
            
            implicit none

            integer, save :: logdev = 6

            logical success
            integer, save :: dev_gc_nml, dev_ae_nml, dev_nr_nml, dev_tr_nml
            integer i, i1, j, ios, IGC, IAE, INR, ITR
            character(  1 ), parameter :: bl = ' '
            integer, parameter :: spc_dim = 1000
            logical :: order = .true., found = .true.
            character( 120 ) :: xmsg
            INTEGER :: ISPC, IGRID

c namelist variables
            ! Namelist Registry Format for Gases and Nonreactives 
            TYPE SPECIES_DATA_REG_1
                CHARACTER( 16 ) :: SPECIES_NAME  ! CMAQ Species Name
                REAL            :: MOLWT         ! g mol-1
                CHARACTER( 16 ) :: IC_SURR       ! IC Surrogate
                REAL            :: IC_FAC        ! IC Scale Factor
                CHARACTER( 16 ) :: BC_SURR       ! BC Surrogate
                REAL            :: BC_FAC        ! BC Scale Factor       
                CHARACTER( 16 ) :: DEPV_SURR     ! Dry Deposition Surrogate
                REAL            :: DEPV_FAC      ! Dry Deposition Scale Factor
                CHARACTER( 16 ) :: SCAV_SURR     ! Wet Scavenging Surrogate
                REAL            :: SCAV_FAC      ! Wet Scavenging Scale Factor
                CHARACTER( 16 ) :: AERO_SURR     ! Aerosol Module Surrogate
                CHARACTER( 16 ) :: CLOUD_SURR    ! Cloud Chemistry Surrogate
                CHARACTER( 16 ) :: TRNS_FLAG     ! Do Transport?
                CHARACTER( 16 ) :: DDEP_FLAG     ! Output Dry Deposition Velocities
                CHARACTER( 16 ) :: WDEP_FLAG     ! Output Wet Scavenging Fluxes
                CHARACTER( 16 ) :: CONC_FLAG     ! Output Concentration
            END TYPE SPECIES_DATA_REG_1

            ! Namelist Registry Format for Aerosols
            TYPE SPECIES_DATA_REG_2
                CHARACTER( 16 ) :: SPECIES_NAME  ! CMAQ Species Name
                REAL            :: MOLWT         ! g mol-1
                CHARACTER( 16 ) :: IC_SURR       ! IC Surrogate
                REAL            :: IC_FAC        ! IC Scale Factor
                CHARACTER( 16 ) :: BC_SURR       ! BC Surrogate
                REAL            :: BC_FAC        ! BC Scale Factor
                CHARACTER( 16 ) :: DEPV_SURR     ! Dry Deposition Surrogate
                REAL            :: DEPV_FAC      ! Dry Deposition Scale Factor
                CHARACTER( 16 ) :: SCAV_SURR     ! Wet Scavenging Surrogate
                REAL            :: SCAV_FAC      ! Wet Scavenging Scale Factor
                CHARACTER( 16 ) :: CLOUD_SURR    ! Cloud Chemistry Surrogate
                CHARACTER( 16 ) :: TRNS_FLAG     ! Do Transport?
                CHARACTER( 16 ) :: DDEP_FLAG     ! Output Dry Deposition Velocities
                CHARACTER( 16 ) :: WDEP_FLAG     ! Output Wet Scavenging Fluxes
                CHARACTER( 16 ) :: CONC_FLAG     ! Output Concentration
            END TYPE SPECIES_DATA_REG_2
 
            ! Namelist Registry Format for Tracers
            TYPE SPECIES_DATA_REG_3
                CHARACTER( 16 ) :: SPECIES_NAME  ! CMAQ Species Name
                REAL            :: MOLWT         ! g mol-1
                CHARACTER( 16 ) :: IC_SURR       ! IC Surrogate
                REAL            :: IC_FAC        ! IC Scale Factor
                CHARACTER( 16 ) :: BC_SURR       ! BC Surrogate
                REAL            :: BC_FAC        ! BC Scale Factor
                CHARACTER( 16 ) :: DEPV_SURR     ! Dry Deposition Surrogate
                REAL            :: DEPV_FAC      ! Dry Deposition Scale Factor
                CHARACTER( 16 ) :: SCAV_SURR     ! Wet Scavenging Surrogate
                REAL            :: SCAV_FAC      ! Wet Scavenging Scale Factor
                CHARACTER( 16 ) :: AERO_SURR     ! Aerosol Module Surrogate
                CHARACTER( 16 ) :: CLOUD_SURR    ! Cloud Chemistry Surrogate
                CHARACTER( 16 ) :: ADVC_FLAG     ! Do Advection?
                CHARACTER( 16 ) :: DIFF_FLAG     ! Do Dispersion (i.e. Diffusion)?
                CHARACTER( 16 ) :: DDEP_FLAG     ! Output Dry Deposition Velocities
                CHARACTER( 16 ) :: WDEP_FLAG     ! Output Wet Scavenging Fluxes
                CHARACTER( 16 ) :: CONC_FLAG     ! Output Concentration
            END TYPE SPECIES_DATA_REG_3
 
            character( 16 ) :: gc_matrix = 'gc_matrix_nml'
            character( 16 ) :: ae_matrix = 'ae_matrix_nml'
            character( 16 ) :: nr_matrix = 'nr_matrix_nml'
            character( 16 ) :: tr_matrix = 'tr_matrix_nml'
            character( 256 ) :: eqname

            integer, external :: junit
            integer, external :: index1
            
            character( 16 ), allocatable     :: nml_spc  ( : )
            character(  2 ), allocatable     :: nml_type ( : )
            integer,         allocatable     :: nml_index( : )
            logical,         allocatable     :: nml_convert( : )
            real,            allocatable     :: nml_molwt( : )

            TYPE (SPECIES_DATA_REG_1), ALLOCATABLE :: GC_SPECIES_DATA(:)
            TYPE (SPECIES_DATA_REG_2), ALLOCATABLE :: AE_SPECIES_DATA(:)
            TYPE (SPECIES_DATA_REG_1), ALLOCATABLE :: NR_SPECIES_DATA(:)
            TYPE (SPECIES_DATA_REG_3), ALLOCATABLE :: TR_SPECIES_DATA(:)
 
            namelist / GC_NML / GC_SPECIES_DATA
            namelist / AE_NML / AE_SPECIES_DATA
            namelist / NR_NML / NR_SPECIES_DATA
            namelist / TR_NML / TR_SPECIES_DATA

c----------------------------------------------------------------------------------------
            success = .true.

            write( logdev,* ) "Retrieve Species Namelist" 

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

            call value_name( gc_matrix, eqname )

            write( logdev,* ) ' '
            write( logdev,* ) '    GC Species Namelist: ', trim( eqname )
                                                                                             
            dev_gc_nml = junit()                                                             
            open( file = trim( eqname ), unit = dev_gc_nml,                             
     &            status = 'old', position = 'rewind' )                                      
            read( nml = GC_nml, unit = dev_gc_nml )                                          
                                                                                             
            ! Determine Number of Gas Species                                                
            N_GC_SPC = 0                                                                     
            DO IGC = 1,SPC_DIM                                                              
                IF ( GC_SPECIES_DATA( IGC )%SPECIES_NAME .EQ. '' ) EXIT                      
                N_GC_SPC = IGC                                                               
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
     &      stat = ios )

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
                
            END DO

            GC_FINI   = GC_STRT + N_GC_SPC - 1
            N_GC_SPCD = GC_FINI + 1



            ! Aerosol (AE) Namelist
            ALLOCATE( AE_SPECIES_DATA( SPC_DIM ), STAT=IOS )
            AE_SPECIES_DATA%SPECIES_NAME = ''
            AE_SPECIES_DATA%MOLWT = 0.
            AE_SPECIES_DATA%IC_SURR = ''
            AE_SPECIES_DATA%IC_FAC = 0.
            AE_SPECIES_DATA%BC_SURR = ''
            AE_SPECIES_DATA%BC_FAC = 0.
            AE_SPECIES_DATA%DEPV_SURR = ''
            AE_SPECIES_DATA%DEPV_FAC = 0.
            AE_SPECIES_DATA%SCAV_SURR = ''
            AE_SPECIES_DATA%SCAV_FAC = 0.
            AE_SPECIES_DATA%CLOUD_SURR = ''
            AE_SPECIES_DATA%TRNS_FLAG = ''
            AE_SPECIES_DATA%DDEP_FLAG = ''
            AE_SPECIES_DATA%WDEP_FLAG = ''
            AE_SPECIES_DATA%CONC_FLAG = ''
                                                                                             

            call value_name( ae_matrix, eqname )

            write( logdev,* ) ' '
            write( logdev,* ) '    AE Species Namelist: ', trim( eqname )

!           open( file = 'ae_matrix.nml', unit = dev_ae_nml,
            dev_ae_nml = junit()
            open( file = trim( eqname ), unit = dev_ae_nml,
     &            status = 'old', position = 'rewind' )
            read( nml = AE_nml, unit = dev_ae_nml ) 

            ! Determine Number of Gas Species
            N_AE_SPC = 0
            DO IAE = 1,SPC_DIM
                IF ( AE_SPECIES_DATA( IAE )%SPECIES_NAME .EQ. '' ) EXIT
                N_AE_SPC = IAE
            END DO

            ! Allocate and Initialize Permanent Variables
            allocate ( 
     &      ae_spc( n_ae_spc ), ae_molwt( n_ae_spc ),
     &      ae_ic( n_ae_spc ),   ae_ic_map( n_ae_spc ),   ae_ic_fac( n_ae_spc ),
     &      ae_bc( n_ae_spc ),   ae_bc_map( n_ae_spc ),   ae_bc_fac( n_ae_spc ),
     &      ae_depv( n_ae_spc ), ae_depv_map( n_ae_spc ), ae_depv_fac( n_ae_spc ),
     &      ae_scav( n_ae_spc ), ae_scav_map( n_ae_spc ), ae_scav_fac( n_ae_spc ),
     &      ae_a2aq( n_ae_spc ), ae_a2aq_map( n_ae_spc ),
     &      ae_trns( n_ae_spc ), ae_trns_map( n_ae_spc ),
     &      ae_ddep( n_ae_spc ), ae_ddep_map( n_ae_spc ),
     &      ae_wdep( n_ae_spc ), ae_wdep_map( n_ae_spc ),
     &      ae_conc( n_ae_spc ), ae_conc_map( n_ae_spc ),
     &      stat = ios )

            ae_spc = bl; ae_molwt = 0.0                        !  .
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
            N_AE_IC = 0; N_AE_BC = 0; N_AE_DEPV = 0; N_AE_SCAV = 0
            N_AE_A2AQ = 0; N_AE_TRNS = 0; N_AE_DDEP = 0; N_AE_WDEP = 0
            N_AE_CONC = 0

            DO IAE = 1,N_AE_SPC

                AE_SPC( IAE )      = AE_SPECIES_DATA( IAE )%SPECIES_NAME
                AE_MOLWT( IAE )    = AE_SPECIES_DATA( IAE )%MOLWT
                
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

            call value_name( nr_matrix, eqname )

            write( logdev,* ) ' '
            write( logdev,* ) '    NR Species Namelist: ', trim( eqname )

!           open( file = 'nr_matrix.nml', unit = dev_nr_nml,
            dev_nr_nml = junit()
            open( file = trim( eqname ), unit = dev_nr_nml,
     &            status = 'old', position = 'rewind' )
            read( nml = NR_nml, unit = dev_nr_nml ) 
           
            ! Determine Number of Gas Species
            N_NR_SPC = 0
            DO INR = 1,SPC_DIM
                IF ( NR_SPECIES_DATA( INR )%SPECIES_NAME .EQ. '' ) EXIT
                N_NR_SPC = INR
            END DO

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

            call value_name( tr_matrix, eqname )

            write( logdev,* ) ' '
            write( logdev,* ) '    TR Species Namelist: ', trim( eqname )

!           open( file = 'tr_matrix.nml', unit = dev_tr_nml,
            dev_tr_nml = junit()
            open( file = trim( eqname ), unit = dev_tr_nml,
     &            status = 'old', position = 'rewind' )
            read( nml = TR_nml, unit = dev_tr_nml )

            ! Determine Number of Gas Species
            N_TR_SPC = 0
            DO ITR = 1,SPC_DIM
                IF ( TR_SPECIES_DATA( ITR )%SPECIES_NAME .EQ. '' ) EXIT
                N_TR_SPC = ITR
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
            N_TR_T2AE = 0; N_TR_T2AQ = 0; N_TR_ADV = 0; N_TR_DDEP = 0; N_TR_WDEP = 0
            N_TR_CONC = 0; N_TR_DIFF = 0

            DO ITR = 1,N_TR_SPC

                TR_SPC( ITR )      = TR_SPECIES_DATA( ITR )%SPECIES_NAME
                TR_MOLWT( ITR )    = TR_SPECIES_DATA( ITR )%MOLWT
                
            END DO

            TR_STRT   = NR_FINI + 1
            TR_FINI   = TR_STRT + N_TR_SPC - 1
 

            ! Sum Up All Species Across Phases and Types
            NSPCSD = N_GC_SPCD + N_AE_SPC + N_NR_SPC + N_TR_SPC
            
            ! Move Data To CGRID_xxx Master Arrays
            N_CGRID_SPC = NSPCSD
            ALLOCATE( CGRID_NAME( N_CGRID_SPC ),
     &                CGRID_MW  ( N_CGRID_SPC ),
     &                CGRID_MASK_GAS( N_CGRID_SPC ),
     &                CGRID_MASK_AERO( N_CGRID_SPC ),
     &                CGRID_MASK_NUM( N_CGRID_SPC ),
     &                CGRID_MASK_SRF( N_CGRID_SPC ),
     &                CGRID_MASK_NR( N_CGRID_SPC ),
     &                CGRID_MASK_TRAC( N_CGRID_SPC ) )
            CGRID_NAME = ''
            CGRID_MW   = 0.
            CGRID_MASK_GAS = .FALSE.
            CGRID_MASK_AERO= .FALSE.
            CGRID_MASK_NUM = .FALSE.
            CGRID_MASK_SRF = .FALSE.
            CGRID_MASK_NR  = .FALSE.
            CGRID_MASK_TRAC= .FALSE.

            IGRID = 0
            DO ISPC = gc_strt,n_gc_spcd
                IGRID = IGRID + 1
                CGRID_NAME( IGRID ) = GC_SPC( ISPC )
                CGRID_MW( IGRID )   = GC_MOLWT( ISPC )
                CGRID_MASK_GAS( IGRID )   = .TRUE.
            END DO
            DO ISPC = 1,n_ae_spc
                IGRID = IGRID + 1
                CGRID_NAME( IGRID ) = AE_SPC( ISPC )
                CGRID_MW( IGRID )   = AE_MOLWT( ISPC )
                CGRID_MASK_AERO( IGRID )   = .TRUE.
                IF ( INDEX( AE_SPC( ISPC ),'NUM') .NE. 0 ) CGRID_MASK_NUM( IGRID )   = .TRUE.
                IF ( INDEX( AE_SPC( ISPC ),'SRF') .NE. 0 ) CGRID_MASK_SRF( IGRID )   = .TRUE.
            END DO
            DO ISPC = 1,n_nr_spc
                IGRID = IGRID + 1
                CGRID_NAME( IGRID ) = NR_SPC( ISPC )
                CGRID_MW( IGRID )   = NR_MOLWT( ISPC )
                CGRID_MASK_NR( IGRID )   = .TRUE.
            END DO
            DO ISPC = 1,n_tr_spc
                IGRID = IGRID + 1
                CGRID_NAME( IGRID ) = TR_SPC( ISPC )
                CGRID_MW( IGRID )   = TR_MOLWT( ISPC )
                CGRID_MASK_TRAC( IGRID )   = .TRUE.
            END DO

            allocate ( cgrid_spc( nspcsd - 1 ), nml_spc( nspcsd - 1 ), 
     &                 nml_index( nspcsd - 1 ), nml_type( nspcsd - 1 ), 
     &                 nml_molwt( nspcsd - 1 ), type_index( nspcsd - 1 ),
     &                 nml_convert( nspcsd - 1 ), stat = ios )
           
            allocate ( species_molwt( nspcsd - 1 ), convert_conc( nspcsd - 1 ), stat = ios )

     
            j = 0
            cgrid_spc  = 'BLANK'
            nml_index  = -1
            type_index = -1
            nml_type   = '??'
            nml_molwt  = -1.0
            convert_conc = .False.
            
            do i = 1, n_gc_spc ! load GC names and indices 
               j = j + 1
               cgrid_spc( i )   = gc_spc( i )
               nml_index( j )  = i + gc_strt -1
               type_index( j ) = i
               nml_type( j )   = 'GC'
               nml_molwt( j ) = gc_molwt( i )
            end do  

            do i = 1, n_ae_spc ! load AE names and indices 
               j = j + 1
               cgrid_spc( j )    = ae_spc( i )
               nml_index( j )    = i + ae_strt - 1
               nml_type( j )     = 'AE'
               type_index( j )   = i
               nml_molwt( j )    = ae_molwt( i )
               nml_convert( j )  = .True.
!              convert_conc( j ) = .True.
            end do  

            do i = 1, n_nr_spc ! load NR names and indices 
               j = j + 1
               cgrid_spc( j )   = nr_spc( i )
               nml_index( j )   = i + nr_strt - 1
               nml_type( j )    = 'NR'
               type_index( j )  = i
               nml_molwt( j ) = nr_molwt( i )
            end do  

            do i = 1, n_tr_spc ! load TR names and indices 
               j = j + 1
               cgrid_spc( j )   = tr_spc( i )
               nml_index( j )   = i + tr_strt - 1
               nml_type( j )    = 'TR'
               type_index( j )  = i
               nml_molwt( j )   = tr_molwt( i )
            end do  

            nml_spc( 1:(nspcsd-1) ) = cgrid_spc( 1:(nspcsd-1) )
            
            
C determine if mechanism species are in cgrid species            

            do i = 1, numb_mech_spcs
               i1 = index1a( mechanism_spc( i ), (nspcsd-1), cgrid_spc )
               if ( i1 .lt. 1 ) then
                  found = .false.
               else
                  found = .true.
                  cgrid_index( i )   = nml_index( i1 )
                  species_type( i )  = nml_type ( i1 )
                  species_molwt( i ) = nml_molwt( i1 )
                  convert_conc( i )  = nml_convert( i1 )
!                 if(   Trim( species_type( i ) ) .eq. 'AE' )then
!                       convert_conc( i ) = .True.
!                 else
!                       convert_conc( i ) = .False.
!                 end if
!                 print*,' i, mechanism_spc( i ),species_type( i ),convert_conc( i ): ',i,mechanism_spc( i ),
!    &            species_type( i ),convert_conc( i )
               end if
               if( index( mechanism_spc( i ), 'SRF') .gt. 0 )then
                   found = .false.
                   xmsg = '*** reactions cannot use modal aerosol surface area as species'
                   write( logdev,'( /5x, a )' ) trim( xmsg )
                   xmsg = trim( mechanism_spc( i ) )
                   write( logdev,'( 9x, i4, 2x, a )' ) i, trim( xmsg )
               end if
               if( index( mechanism_spc( i ), 'NUM') .gt. 0 )then
                   found = .false.
                   xmsg = '*** reactions cannot use modal aerosol number density as species'
                   write( logdev,'( /5x, a )' ) trim( xmsg )
                   xmsg = trim( mechanism_spc( i ) )
                   write( logdev,'( 9x, i4, 2x, a )' ) i, trim( xmsg )
               end if
               if ( .Not. found ) then
                  xmsg = 'Fatal error: Mechanism Species found not in species namelist:'
                  write( logdev,'( /5x, a )', ADVANCE = 'NO' ) trim( xmsg )
                  xmsg = trim( mechanism_spc( i ) )
                  write( logdev,'( 9x, i4, 2x, a )' ) i, trim( xmsg )
                  success = found
               end if
            end do


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
         subroutine get_list( ic, str_in, base_out,
     &                        n_surr1, surr1_out, n_surr2, surr2_out, n_ctrl, ctrl_out )

c assume chem mechanism types are in the order of:
c 1) CGRID species, CGRID species molecular weight <- base type
c 2) IC, BC, DEPV, SCAV  [any order]               <- surrogate type 1 (surr name, surr fac)
c 3) x2AE, x2AQ  [any order]                       <- surrogate type 2 (surr name only )
c 4) DIFF, DDEP, WDEP, CONC  [any order]           <- control type

            !use UTILIO_DEFN

            implicit none


            integer,            intent ( in out ) :: ic
            character( * ),     intent (  in ) :: str_in
            type( base_type ),  intent ( out ) :: base_out
            integer,            intent (  in ) :: n_surr1
            type( surr1_type ), intent ( out ) :: surr1_out( n_surr1 )
            integer,            intent (  in ) :: n_surr2
            type( surr2_type ), intent ( out ) :: surr2_out( n_surr2 )
            integer,            intent (  in ) :: n_ctrl
            type( ctrl_type ),  intent ( out ) :: ctrl_out( n_ctrl )

            integer, parameter :: nfld = 20
            character(  1 ), parameter :: dc = ':', bl = ' '  ! namelist field delimiting character, blank character
            character( 32 ) :: str( nfld ) = bl               ! field substring
            character(  1 ) :: chr, lastchr
            integer ip, is, n
            integer fn                                        ! field number
            integer :: jp( nfld ), kp( nfld )                 ! field starting and ending position
            logical :: nbf( nfld )                            ! non-blank field
            integer maxlen
!           integer, save :: ic = 0   ! subroutine call count

            ic = ic + 1
            maxlen = len_trim( str_in )
            fn = 1; ip = 0

101         continue    ! consume leading blanks in field
            nbf( fn ) = .true.   ! posit non-blank field
            ip = ip + 1
            if ( ip .gt. maxlen ) go to 301   ! end of line
            chr = str_in( ip:ip )
            if ( chr .eq. bl ) go to 101   ! skip leading blanks before valid char
            lastchr = chr
            jp( fn ) = ip   ! 1st non-blank char in field

201         continue
            if ( chr .ne. dc ) then
               if ( chr .ne. bl ) lastchr = chr
               ip = ip + 1
               if ( ip .gt. maxlen ) go to 301   ! end of line
               chr = str_in( ip:ip )
               go to 201
            else
               if ( lastchr .eq. dc ) nbf( fn ) = .false.   ! we've got a blank field
               kp( fn ) = ip - 1
               fn = fn + 1
            end if
            go to 101

301         continue
            kp( fn ) = ip - 1
            if ( chr .eq. dc ) nbf( fn ) = .false.

            do n = 1, fn
#ifdef Verbose
               write( logdev,'(3i4,l)' ) n, jp( n ), kp( n ), nbf( n )
               if ( kp( n ) .lt. jp( n ) ) then   ! don't depend on this - use bf( n )
                  write( logdev,* ) n, 'end of string < start'
               end if
#endif
               if ( nbf( n ) ) then
                  str( n ) = str_in( jp( n ):kp( n ) )
#ifdef Verbose
                  write( logdev,* ) n, len_trim( str( n ) ), str( n )
#endif
               end if
            end do

c assumed group order: {CGRID spc, mw},
c                      {IC, BC, DEPV, SCAV} group,
c                      {x2AE, x2AQ} group,
c                      {TRNS, DDEP, WDEP, CONC} group
c str( 1 ) = CGRID name, str( 2 ) = CGRID name molec. wt.

            read( str( 1 ),'( a16 )' )  base_out%name
            if ( index( str( 2 ), '.' ) .le. 0 ) then   ! integer
               read( str( 2 ),'( i7 )' ) is
               base_out%mw = real( is )
            else
               read( str( 2 ),'( f7.2 )' ) base_out%mw
            end if

c type 1 surrogate (surr1)
c if there is a surrogate name and the factor field is blank, assume fac = 1.0
            ip = 3
            do n = 1, n_surr1
               if ( nbf( ip ) ) then
                  read( str( ip ),'( a16 )' ) surr1_out( n )%name
                  surr1_out( n )%map = ic
                  if ( nbf( ip+1 ) ) then
                     if ( index( str( ip+1 ), '.' ) .le. 0 ) then   ! integer
                        read( str( ip+1 ),'( i7 )' ) is
                        surr1_out( n )%fac = real( is )
                     else
                        read( str( ip+1 ),'( f7.2 )' ) surr1_out( n )%fac
                     end if
                  else
                     surr1_out( n )%fac = 1.0
                  end if
               else
                  surr1_out( n )%name = bl
                  surr1_out( n )%map = 0
                  surr1_out( n )%fac = 0.0
               end if
               ip = ip + 2
            end do

c type 2 surrogate (surr2)
            do n = 1, n_surr2
               if ( nbf( ip ) ) then
                  surr2_out( n )%map = ic
                  read( str( ip ),'( a16 )' ) surr2_out( n )%name
               else
                  surr2_out( n )%name = bl
                  surr2_out( n )%map = 0
               end if
               ip = ip + 1
            end do

            do n = 1, n_ctrl
               if ( nbf( ip ) ) then
                  if ( str( ip ) .eq. 'yes' .or.
     &                 str( ip ) .eq. 'Yes' .or.
     &                 str( ip ) .eq. 'YES' ) then  ! use call upcase
                     read( str( 1 ),'( a16 )' ) ctrl_out( n )%name
                     ctrl_out( n )%map = ic
                  else
                     ctrl_out( n )%name = bl
                  end if
               else
                  ctrl_out( n )%name = bl
                  ctrl_out( n )%map = 0
               end if
               ip = ip + 1
            end do

            return
            end subroutine get_list

c----------------------------------------------------------------------------------------
            subroutine bubble ( n, ia, aa )

            implicit none

            integer, intent( in ) :: n
            integer, intent( in out ) :: ia( n )
            character( * ), intent( inout ) :: aa( n )
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

