
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CMAQ_DRIVER ( MODEL_STDATE, MODEL_STTIME, MODEL_TSTEP,
     $                         MODEL_JDATE, MODEL_JTIME, LAST_STEP,
     $                         COUPLE_TSTEP, NCOLS_IN, NLAYS_IN)

C-----------------------------------------------------------------------
C Function:
C    CMAQ CTM driver
 
C Preconditions:
C    Initialized file CONCFILE for output; completed
C    files HISTORY containing initial conditions, SPCCONST for
C    conversion of concentration field from computational units
C    to output units.
 
C Subroutines and functions called:
C    INITSCEN, ADVSTEP, M3EXIT, WRITE3
C    science processes SCIPROC, PA_OUTPUT
 
C Revision History:
C    prototype 6/92 by CJC for proof-of-concept
C    Revised   2/93 by CJC for initial LCM Prototype Alpha
 
C    31 August, 1995 by M. Talat Odman at NCSC: special version for one 
C    single grid
 
C    16 April 1995 by M. Talat Odman at NCSC: write (or rewrite if restart)
C    initial conditions to the output file

C    Jeff
C    18 June 98 - put load of mechanism common here because of ping/ping_noop,
C    chem/chem_noop options

C    2 October, 1998 by Al Bourgeois at LM: parallel implementation
C    Jeff - Dec 00 - move CGRID_MAP into f90 module, re-order cols/rows in
C    call to PAR_INIT
C    Jeff - Jul 01 - enable integral average conc data
C    Sep 01  J. Young        Dyn Alloc - Use HGRD_DEFN

C    3 Sep 01 David wong
C      -- removed M3IO SHUT3 call which is done in PAR_TERM
C      -- removed SET_CTMFILE call

C   23 Jun 03 J.Young: for layer dependent advection tstep
C   18 Aug 03 J. Pleim - move vdiff before advection
C   07 Dec 04 J.Young: for layer dyn alloc - Use VGRD_DEFN
C   30 May 05 J.Young: mass-conserving advection (yamo)
C   20 Jan 06 J.Young: add circular buffer CGRID state file
C   24 May 06 J.Young: par_init/pio_init col/row order check
C    6 Sep 06 J.Young: one-write cgrid file; SGRID in module
C   27 May 09 J.Young: re-do parallel processing initialization
C   21 Jun 10 J.Young: convert for Namelist redesign
C   20 Jul 10 J.Young: re-do serial processing termination (eliminate par_noop)
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   11 May 11 D.Wong: incorporated twoway model implementation
C   24 Aug 11 D.Wong: eliminated data and geo orientation in se_init call
C    8 Jan 13 C.Nolte: fixed load AGRID bug if TSTEP(1) .ne. 010000 hhmmss
C   24 Sep 13 D.Wong:  Computed AGRID at the output mark of the model
C                      indicated in TSTEP(1) for twoway model
C   21 Apr 14 D.Wong:  Removed M3EXIT call in SHUT3 block
C   07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
C   10 Aug 15 D.Wong:  Replaced MYPE with IO_PE_INCLUSIVE for parallel
C                      I/O implementation
C   10 Dec 15 D.Wong:  Moved the code which determines which processors are involved
C                      in I/O processing in front of routine PIO_RE_INIT and passed
C                      that information into PIO_RE_INIT
C   26 Jan 16 J.Young: Consolidate PIO_INIT, use keywords for optional arguments
C   28 Jan 16 D.Wong:  Add SAVE attribute to TSTEP for the two-way model implementation
C   16 Sep 16 J.Young: update for inline procan (IRR)
C   29 Nov 17 D. Wong: removed all SWAP routines and replaced with SE_COMM
C   29 Oct 18 L.Zhou, S.Napelenok: isam implementation
C   31 Jan 19 D. Wong: adopted the idea to process all twoway related environment
C                      variables in one place
C   01 Feb 19 D. Wong: made this as a subroutine rather than the main program to
C                      accommodate interface with global, regional and offlice mode,
C                      implemented centralized I/O approach, removed all MY_N clauses,
C                      with new re-structure of LUS_DEFN, most of the data declaration 
C                      has been moved to lus_data_module (model_data_module.f) and to
C                      call lus_setup to setup land use information according to land
C                      use scheme
C   02 Aug 19 F. Sidi: Restored serial processing option
C   03 AUG 19 D.Wong:  Modified code to work with two-way model
C   09 Sep 19 F. Sidi: Replaced L_CONC_WVEL & L_ACONC_WVEL (depreciated) with W_VEL
C   21 Nov 19 S.L.Napelenok: Include DDM3D for versions 5.3.1
C-----------------------------------------------------------------------

      USE RXNS_DATA             ! chemical mechanism data
      USE CENTRALIZED_IO_MODULE
      USE ASX_DATA_MOD, Only : MET_DATA, INIT_MET, GET_MET
      USE LSM_MOD
      USE WVEL_DEFN             ! derived vertical velocity component
      USE BUDGET_DEFN
      USE AERO_DATA
      USE ELMO_DATA, ONLY : L_ELMO, ELMO_NSTEP
      USE ELMO_PROC, ONLY : ELMO_DRIVER, WRITE_ELMO, MAP_ELMO
      USE UTILIO_DEFN
      USE util_Family_module
      USE RUNTIME_VARS
      USE STD_CONC              ! standard CONC
      USE CGRID_SPCS            ! CGRID mechanism species
#ifdef mpas
      USE HGRD_DEFN, ONLY: MYPE
      use coupler_module
      use VGRD_DEFN, ONLY : NLAYS
      use HGRD_DEFN, ONLY : NCOLS, NROWS

      use mydata_module
      use get_env_module
      use lus_defn
      use mio_module
#else
      USE PCGRID_DEFN           ! inherits GRID_CONF
      USE AVG_CONC              ! integral average CONC
      USE PA_DEFN, Only: LIPR, LIRR  ! Process Anaylsis control and data variables
      USE PAGRD_DEFN            ! Process Anaylsis horiz domain specs

      USE BIDI_MOD, Only : INIT_BIDI

#ifdef isam
c     USE SA_LAYERS
      USE SA_DEFN
      USE PISAM_DEFN            ! SA array definition (borrowed )
c     USE PMFRC_DEFN            ! MAPFRAC array definition ( adapted )
#endif

#ifdef sens
      USE DDM3D_DEFN
      USE S_PCGRID_DEFN
#endif

#ifdef parallel
      USE VERTEXT_MODULE
#endif
      USE lus_data_module

#ifdef twoway
      use twoway_data_module
      use sd_time_series_module
#endif

#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_INIT_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_INIT_MODULE)
#endif
#endif   ! end mpas

      IMPLICIT NONE

      INTEGER, INTENT( IN )  :: MODEL_STDATE, MODEL_STTIME, MODEL_TSTEP
      INTEGER, INTENT( OUT ) :: MODEL_JDATE, MODEL_JTIME
      LOGICAL, INTENT( IN )  :: LAST_STEP
      INTEGER, INTENT( IN ), OPTIONAL :: COUPLE_TSTEP
      INTEGER, INTENT( IN ), OPTIONAL :: NCOLS_IN, NLAYS_IN

#ifdef mpas
      real, parameter :: cmin = 1.0E-30

      integer :: ncols_gl
#endif

C Include Files:
      INCLUDE SUBST_FILES_ID    ! I/O definitions and declarations

#ifdef parallel
!     INCLUDE SUBST_MPI         ! MPI definitions and parameters
      INCLUDE 'mpif.h'
#endif

C Local variables:

      INTEGER, SAVE :: TSTEP( 3 ) ! time step vector (HHMMSS)
                                  ! TSTEP(1) = local output step
                                  ! TSTEP(2) = sciproc sync. step (chem)
                                  ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                  !            step and wrf/cmaq call frequency

      INTEGER, ALLOCATABLE, SAVE :: ASTEP( : )
      INTEGER, SAVE :: NREPS    ! number of model time steps per output step
      INTEGER          ISTEP    ! current output time step number
      INTEGER          IREP     ! model step number within this output step
      INTEGER, SAVE :: JDATE    ! current model date, coded YYYYDDD
      INTEGER, SAVE :: JTIME    ! current model time, coded HHMMSS
      INTEGER          C, R, L, K, S, V     ! loop induction variables
      INTEGER          ALLOCSTAT
      INTEGER          NFILE, IFILE
      LOGICAL          EXST, OPD
      CHARACTER( 1000 ) :: CFILE
      CHARACTER(   16 ) :: ACT
      REAL( 8 )         :: CPU_TIME_START, CPU_TIME_FINISH
      REAL              :: REAL_TIME


      CHARACTER(  2 ) :: COLROW = 'CR'  ! col/row arg list order
      CHARACTER( 16 ) :: PNAME = 'DRIVER'
      CHARACTER( 96 ) :: XMSG = ' '

      REAL, SAVE, POINTER     :: CGRID( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: AGRID( :,:,:,: )
      REAL    DIVFAC      ! trapezoidal average factor

      LOGICAL, SAVE :: FIRST_RUN = .TRUE.  ! used for twoway model
      LOGICAL       :: WFLG = .TRUE.       ! turn on write subdmap in pio_init

      INTEGER, SAVE :: myNREPS = 0
      INTEGER       :: STATUS

#ifdef mpas

      integer :: io_mode

      INTEGER      SPC_STRT, SPC_FINI, J
      LOGICAL      LSTAT
      INTEGER      STAT                       ! Status reported by Aerosol Dist Checker

      INTEGER      LMODE    !Identifies the problematic mode from
                                !the BC Check routine
      REAL         AER_PAR( 2, N_MODE,5 )  !Modal parameter after the BC 
                                           !check (N, dg, sg)
                                           !      (N, M2, M3) -
                                           !      Before
                                           !      (N, M2, M3) -
                                           !      After
      REAL,ALLOCATABLE :: AECON( : )

      REAL( 8 ) :: BEGTIME     ! Wall-clock time (sec) at MPI initialization
      LOGICAL       :: L_WRITE_WARNING


#endif

      INTERFACE
         SUBROUTINE INITSCEN ( CGRID, TSTEP )
            REAL, POINTER            :: CGRID( :,:,:,: )
            INTEGER, INTENT( OUT )   :: TSTEP( 3 )
         END SUBROUTINE INITSCEN
#ifndef mpas
         SUBROUTINE ADVSTEP ( JDATE, JTIME, TSTEP, ASTEP, NREPS )
            INTEGER, INTENT( IN )    :: JDATE, JTIME
            INTEGER, INTENT( INOUT ) :: TSTEP( 3 )
            INTEGER, INTENT( OUT )   :: ASTEP( : )
            INTEGER, INTENT( OUT )   :: NREPS
         END SUBROUTINE ADVSTEP
         SUBROUTINE CKSUMMER ( PRNAME, CGRID, JDATE, JTIME )
            CHARACTER( * ), INTENT( IN ) :: PRNAME
            REAL, POINTER            :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )    :: JDATE, JTIME
         END SUBROUTINE CKSUMMER
         SUBROUTINE PA_INIT ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER            :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )    :: JDATE, JTIME, TSTEP( 3 )
         END SUBROUTINE PA_INIT
         SUBROUTINE WR_ACONC ( AGRID, JDATE, JTIME, TSTEP )
            REAL,    INTENT( IN )    :: AGRID( :,:,:,: )
            INTEGER, INTENT( IN )    :: JDATE, JTIME, TSTEP
         END SUBROUTINE WR_ACONC
         SUBROUTINE WR_CGRID ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER            :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )    :: JDATE, JTIME, TSTEP
         END SUBROUTINE WR_CGRID
         SUBROUTINE PA_OUTPUT ( CGRID, JDATE, JTIME )
            REAL, POINTER            :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )    :: JDATE, JTIME
         END SUBROUTINE PA_OUTPUT
#ifdef isam
         SUBROUTINE WR_SA ( JDATE, JTIME, TSTEP, NSTEPS )
            IMPLICIT NONE
            INTEGER                  :: JDATE, JTIME, TSTEP( 3 )
            INTEGER                  :: NSTEPS
         END SUBROUTINE WR_SA
         SUBROUTINE WR_AVG_SA ( JDATE, JTIME, TSTEP )
            IMPLICIT NONE
            INTEGER                  :: JDATE, JTIME, TSTEP
         END SUBROUTINE WR_AVG_SA
         SUBROUTINE WR_SA_CGRID ( JDATE, JTIME, TSTEP )
            IMPLICIT NONE
            INTEGER                  :: JDATE, JTIME, TSTEP
         END SUBROUTINE WR_SA_CGRID
#endif
#ifdef sens
         SUBROUTINE WR_SENGRID ( JDATE, JTIME, TSTEP )
            IMPLICIT NONE
            INTEGER, INTENT( IN )    :: JDATE, JTIME, TSTEP
         END SUBROUTINE WR_SENGRID
         SUBROUTINE WR_ASENS ( JDATE, JTIME, TSTEP )
            IMPLICIT NONE
            INTEGER, INTENT( IN )    :: JDATE, JTIME, TSTEP
         END SUBROUTINE WR_ASENS
#endif
#endif

         SUBROUTINE SCIPROC ( CGRID, JDATE, JTIME, TSTEP, ASTEP )
            REAL, POINTER            :: CGRID( :,:,:,: )
            INTEGER, INTENT( INOUT ) :: JDATE, JTIME
            INTEGER, INTENT( IN )    :: TSTEP( 3 ), ASTEP( : )
         END SUBROUTINE SCIPROC
#ifdef mpas
         SUBROUTINE UNLOAD_CGRID ( CGRID )
            REAL, INTENT(IN)  :: CGRID( :,:,:,: )
         END SUBROUTINE UNLOAD_CGRID
#endif
      END INTERFACE

C-----------------------------------------------------------------------

      IF ( FIRST_RUN ) THEN

         TSTEP = 0
#ifdef mpas
         CALL INIT_ENV_VARS( 0, 0 )
#else
         STDATE     = MODEL_STDATE
         STTIME     = MODEL_STTIME
#endif

#ifdef twoway
         TSTEP( 1 ) = LOCAL_TSTEP
         TSTEP( 3 ) = SEC2TIME(MODEL_TSTEP)
#else
         TSTEP( 1 ) = MODEL_TSTEP
#endif

         IF (PRESENT (COUPLE_TSTEP)) THEN
            TSTEP( 3 ) = SEC2TIME( COUPLE_TSTEP )
            MPAS_TSTEP = TSTEP( 3 )
         END IF

         IF (PRESENT(NCOLS_IN)) THEN
            NCOLS = NCOLS_IN
            NROWS = 1
            NLAYS = NLAYS_IN
            TSTEP( 2 ) = SEC2TIME( COUPLE_TSTEP )
         END IF

#ifdef mpas
         CALL MPCOMM_INIT( NPROCS, MYPE, BEGTIME, LMODE )

         IO_PE_INCLUSIVE = .TRUE.

         call mpi_allreduce (ncols, ncols_gl, 1, mpi_int, mpi_sum, mpi_comm_world, status)

         call mio_init (NPROCS, 1, ncols_gl, nrows, logdev=logdev)

C Set CGRID mechanism
         IF ( .NOT. CGRID_SPCS_INIT() ) THEN
            XMSG = 'Error in CGRID_SPCS:CGRID_SPCS_INIT'
            call M3EXIT (PNAME, JDATE, JTIME, XMSG, 1)
         END IF

         CALL GET_ENV ( LAND_SCHEME, 'LAND_SCHEME', 'NLCD40', LOGDEV )

         ALLOCATE (CGRID ( NCOLS,NROWS,NLAYS,NSPCSD ), STAT = ALLOCSTAT)

         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'CGRID memory allocation failed'
            call M3EXIT (PNAME, JDATE, JTIME, XMSG, 1)
         END IF

C Set up horizontal domain, calculate processor-to-subdomain maps
C and define vertical layer structure (in module GRID_CONF)
         IF ( .NOT. GRID_INIT ( NPROCS, MYPE ) ) THEN
            XMSG = '*** Failure defining domain configuration'
            call M3EXIT (PNAME, JDATE, JTIME, XMSG, 1)
         END IF

#else
#ifdef twoway
C Initialize Environment Variables
         JDATE = 0
         JTIME = 0
         CALL INIT_ENV_VARS( JDATE, JTIME )
#endif

C Set up horizontal domain, calculate processor-to-subdomain maps
C and define vertical layer structure (in module GRID_CONF)
         IF ( .NOT. GRID_INIT ( NPROCS, MYPE ) ) THEN
            XMSG = '*** Failure defining domain configuration'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

#ifdef verbose_driver
         write( logdev,* ) ' MYPE -> NPROCS:   ', mype, nprocs
         write( logdev,* ) ' MYPE -> NPCOL:    ', mype, npcol
         write( logdev,* ) ' MYPE -> NPROW:    ', mype, nprow
         write( logdev,* ) ' MYPE -> MY_NCOLS: ', mype, ncols
         write( logdev,* ) ' MYPE -> MY_NROWS: ', mype, nrows
         write( logdev,* ) ' MYPE -> GL_NCOLS: ', mype, gl_ncols
         write( logdev,* ) ' MYPE -> GL_NROWS: ', mype, gl_nrows
         write( logdev,* ) ' MYPE -> NLAYS:    ', mype, nlays
         write( logdev,* ) ' MYPE -> NBNDY:    ', mype, nbndy
#endif

C Set CGRID mechanism
         IF ( .NOT. CGRID_SPCS_INIT() ) THEN
            XMSG = 'Error in CGRID_SPCS:CGRID_SPCS_INIT'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, 1 )
         END IF

#ifdef sens
C Initialize ddm3d calculations
         CALL INIT_DDM3D
#endif

        CALL MAP_AERO()

#ifdef verbose_driver
         write( logdev,* ) ' MYPE -> NSPCS:    ', mype, nspcsd
#endif

#ifdef parallel_io
         IF ( MOD( MYPE, NPCOL ) .EQ. 0 ) THEN
#else
         IF ( MYPE .EQ. 0 ) THEN
#endif
            IO_PE_INCLUSIVE = .TRUE.
         ELSE
            IO_PE_INCLUSIVE = .FALSE.
         END IF

#ifdef parallel
C Initialize PARIO
         IF ( .NOT. PIO_INIT( COLROW, GL_NCOLS, GL_NROWS, NLAYS, NTHIK,
     &                        NCOLS, NROWS, NPCOL, NPROW, NPROCS, MYPE,
     &                        wflg = WFLG, io_pe_inclusive = IO_PE_INCLUSIVE ) ) THEN
            XMSG = 'Failed to initialize parallel I/O library.'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

#endif
C Initialize stencil exchange
         CALL SUBST_SE_INIT( NPROCS, NPCOL, NPROW, GL_NCOLS, GL_NROWS, NLAYS,
     &                       NSPCSD, MYPE, MNDIS, MEDIS, MSDIS, MWDIS )


#ifdef verbose_driver
         write( logdev,* ) ' MYPE -> MNDIS:    ', mype, mndis
         write( logdev,* ) ' MYPE -> MEDIS:    ', mype, medis
         write( logdev,* ) ' MYPE -> MSDIS:    ', mype, msdis
         write( logdev,* ) ' MYPE -> MWDIS:    ', mype, mwdis
#endif

C Generate the process analysis data: load PA_DEFN module
         CALL PA_DATAGEN( )

C Abort if the number of output variables exceeds the maximum allowed
C by I/O-API
         IF ( NSPCSD .GT. MXVARS3 ) THEN
            WRITE( XMSG,'(5X, A, I5, A)' ) 'The number of variables,', NSPCSD,
     &      ' to be written to the State CGRID File'
            CALL LOG_MESSAGE( LOGDEV, XMSG )
            WRITE( XMSG,'(5X, A, I5)' ) 'exceeds the I/O-API limit:', MXVARS3
            CALL LOG_MESSAGE( LOGDEV, XMSG )
            XMSG = 'Recompile with an I/O-API lib having a larger MXVARS3'
            CALL LOG_MESSAGE( LOGDEV, XMSG )
            CALL M3EXIT( PNAME, JDATE, JTIME, ' ', XSTAT1 )
         END IF

C Initialize PCGRID
         IF ( .NOT. PCGRID_INIT () ) THEN
            XMSG = 'Failure defining horizontal domain'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2  )
         END IF
      
         CGRID => PCGRID( 1:NCOLS,1:NROWS,:,: )   ! required for PinG

C Initalize CONC definitions (in STD_CONC F90 module)
         CALL CONC_DEFN ()

C Get avg CONC definitions, species and layer pointers (in AVG_CONC F90 module)
         CALL A_CONC_DEFN ()

C Miscellaneous Configuration Operations
         WRITE( LOGDEV, * )
         CALL LOG_HEADING( LOGDEV, "Configure Scenario" )

#ifdef isam
c        CALL GET_SA_LAYS ()
         CALL SA_DIM()

C Initialize PISAM
         IF ( .NOT. PISAM_INIT ( NSPC_SA, NTAG_SA ) ) THEN
            XMSG = 'Failure in defining SA horizontal domain'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2  )
         END IF
         ISAM => PISAM( 1:NCOLS,1:NROWS,:,:,: )

#endif

C Initialize optional derived vertical velocity writes to conc file
         IF ( .NOT. WVEL_INIT () ) THEN
            XMSG = 'Failure initializing derived vertical velocity writes'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2  )
         END IF

#ifdef sens
C Initialize ddm3d calculations
         IF ( .NOT. S_PCGRID_IN( NPMAX ) ) THEN
            XMSG = 'Failure defining horizontal sens domain'
            CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2  )
         END IF

         SENGRID => S_PCGRID(1:NCOLS,1:NROWS,:,:,:)
         SENGRID_TEMP => S_PCGRID_TEMP(1:NCOLS,1:NROWS,:,:)

         ALLOCATE ( SAGRID( NCOLS,NROWS,A_NLYS,NPMAX,N_ASPCS ), STAT = STATUS)
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'SAGRID memory allocation failed'
            CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF

         SAGRID = 0.0
#endif


#endif  ! end of mpas


C Initialize Global Arrays for Storing Chemical, Regional, Process,
C and Emission Stream Families
         call read_families

C Initiliaze IO Arrays and Open Files
#ifdef mpas
         call centralized_io_init (NCOLS_IN)
#else
         call centralized_io_init
#endif

C Initialize ELMO Arrays and Maps
         call map_elmo

C Initialize conc field: Copy IC's to CONC file as step 0
C Convention: the input file concentration units are always ppmV.

         CALL INITSCEN ( CGRID, TSTEP )
         JDATE = STDATE; JTIME = STTIME

#ifdef sens
         STARTDATE = STDATE   ! save variable into DDM3D module
#endif

#ifndef mpas
         CALL CKSUMMER ( 'INITSCEN', CGRID, JDATE, JTIME )

         IF ( LIPR .OR. LIRR ) CALL PA_INIT ( CGRID, JDATE, JTIME, TSTEP )

C Verify input file header consistency and run duration
!         CALL FLCHECK ( JDATE, JTIME, TSTEP( 1 ) )

         ALLOCATE ( AGRID( NCOLS,NROWS,A_NLYS,N_ASPCS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'AGRID memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( ASTEP( NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'ASTEP memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

#ifdef isam
         ALLOCATE ( AISAM( NCOLS,NROWS,NLAYS,NSPC_SA,NTAG_SA ),STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'AISAM memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         AISAM = 0.
#endif



#ifdef twoway
         IF ( SD_TIME_SERIES ) THEN
            CALL SD_TIME_SERIES_INIT ( LOGDEV, TSTEP( 3 ) )
            CALL OUTPUT_SD_TIME_SERIES ( CGRID, JDATE, JTIME )
         END IF

         IF ( CMAQ_WRF_FEEDBACK ) THEN
            CALL FEEDBACK_SETUP ( JDATE, JTIME, TSTEP( 3 ) )
         END IF
#endif
#else
!       if (.not. lus_init (mminlu_mpas, lufrac_data(:,:,1)) ) then
!          CALL M3EXIT( 'DRIVER',JDATE,JTIME,'ERROR: Cannot initialize Land Use Category', 2)
!       end if

        if (ncd_64bit_offset) then
            io_mode = ior (nf90_noclobber, nf90_64bit_offset)
         else
            io_mode = nf90_noclobber
         end if

        call mio_setfile ('GR_EMIS_001')

        if (L_ELMO) then
           call mio_fcreate (CTM_ELMO_1, io_mode)
        end if

         call get_env (mpas_diag, 'mpas_diag', .false.)

         if (mpas_diag) then
            call mio_fcreate ('CTM_OUT', io_mode)
         end if


!       call mio_fcreate (CTM_DRY_DEP_1, io_mode)
!       call mio_fcreate (CTM_WET_DEP_1, io_mode)

        if ( depv_diag ) then
            call mio_fcreate (CTM_DEPV_DIAG, io_mode)
        end if

        if ( cld_diag ) then
            call mio_fcreate (CTM_WET_DEP_2, io_mode)
        end if

!       if (photdiag) then
!          call mio_fcreate (CTM_RJ_1, io_mode)
!          call mio_fcreate (CTM_RJ_2, io_mode)
!       end if

#endif

C Initialize Meteorology Structures
         CALL INIT_LSM( JDATE, JTIME )
#ifndef mpas
         CALL INIT_BIDI( )
#endif
         CALL INIT_MET( JDATE, JTIME )

         CALL GET_MET ( JDATE, JTIME, 0 )

         IF ( W_VEL ) CALL GET_WVEL( JDATE, JTIME ) ! If the user wants vertical velocity written to file, retrieve it.

         FIRST_RUN = .FALSE.

C Main processing loop:
         IF ( MYPE .EQ. 0 ) WRITE( OUTDEV, * )
         IF ( MYPE. EQ. 0 ) CALL LOG_HEADING( OUTDEV, "Time Integration" )

      END IF ! first_run

      MODEL_JDATE = JDATE
      MODEL_JTIME = JTIME

#ifdef mpas

      mpas_cmaq_last_step = LAST_STEP

      v = size(cgrid, 4)

      DO L = 1, NLAYS
         DO C = 1, NCOLS
            CGRID( C,1,L,1:N_GC_SPC ) = cmaq_species( c,1,l,1:N_GC_SPC )
            CGRID( C,1,L,N_GC_SPC+2:v ) = cmaq_species( c,1,l,N_GC_SPC+1:v-1 )
         END DO
      END DO


      ! If this run is not a restart, then check the initial conditions
      ! to make sure they are physically realistic. Check Aerosol Size 
      ! Distributions and Warn the User if They Are Not Robust.
      call get_env (NEW_START, 'NEW_START', .false.)
      IF ( NEW_START ) THEN
         ALLOCATE( AECON( N_AE_SPC ) )
         SPC_STRT = AE_STRT
         SPC_FINI = AE_STRT + N_AE_SPC - 1
         LSTAT    = .FALSE.
         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  AECON( 1:N_AE_SPC ) = CGRID( C,R,L,SPC_STRT:SPC_FINI )
                  CALL CHECK_AERO_ICBC( AECON, .FALSE., .TRUE., .FALSE., L_WRITE_WARNING, C, R, L)
                  CGRID( C,R,L,SPC_STRT:SPC_FINI ) = AECON( 1:N_AE_SPC )
                  IF ( STAT .GT. 0 ) THEN
                     LSTAT = .TRUE.
                  ENDIF
               END DO
            END DO
         END DO
    
         !Print warning if any aerosol ICs violated the size
         !distribution parameters
         IF ( LSTAT ) THEN
            WRITE( XMSG, '(A,A)' ),
     &         'Applying fix to aerosol Initial Conditions for aerosol',
     &         ' modes.'
            print *, trim(xmsg)
         END IF
      END IF
 
      NREPS = 1
#else
C Get synchronization and advection time steps, TSTEP(2), ASTEP(L) and NREPS
      CALL ADVSTEP ( JDATE, JTIME, TSTEP, ASTEP, NREPS )

      IF ( MOD( TIME2SEC( JTIME ), TIME2SEC( TSTEP( 1 ) ) ) .EQ. 0 ) THEN
         DO V = 1, N_ASPCS
            S = AVG_CONC_MAP( V )
            AGRID( :,:,:,V ) = CGRID( :,:,ACONC_BLEV:ACONC_ELEV,S )
#ifdef sens
            DO NP = 1, NPMAX
               SAGRID( :,:,:,NP,V ) = SENGRID( :,:,ACONC_BLEV:ACONC_ELEV,NP,S )
            END DO
#endif if
         END DO
#ifdef isam
            IF ( AISAM_ELEV .EQ. -1 ) AISAM_ELEV = NLAYS

            DO ITAG = 1, NTAG_SA ! average isam
               DO V = 1, NSPC_SA
                  L = 0
                  DO K = AISAM_BLEV, AISAM_ELEV
                     L = L + 1
                     DO R = 1, NROWS
                        DO C = 1, NCOLS
                           AISAM( C,R,L,V,ITAG ) = ISAM( C,R,K,V,ITAG )
                        END DO
                     END DO
                  END DO
               END DO
            END DO
#endif

         IF ( W_VEL )
     &      AVG_WVEL( :,:,: ) = WVEL( :,:,ACONC_BLEV:ACONC_ELEV )
         IF ( L_ACONC_RH )
     &      AVG_RH( :,:,: ) = MET_DATA%RH( :,:,ACONC_BLEV:ACONC_ELEV)
         IF ( L_ACONC_TA )
     &      AVG_TA( :,:,: ) = MET_DATA%TA( :,:,ACONC_BLEV:ACONC_ELEV)
         IF ( L_ACONC_PRES )
     &      AVG_PRES( :,:,: ) = MET_DATA%PRES( :,:,ACONC_BLEV:ACONC_ELEV )
      END IF
#endif

C Compute ELMO Values at Beginning of Main Time Step
      IF ( L_ELMO ) THEN
        IF ( JDATE .EQ. STDATE .AND. JTIME .EQ. STTIME ) THEN

             ! Calculate all ELMO variables for both Instantaneous and
             ! Average parameters
             CALL ELMO_DRIVER( CGRID, JDATE, JTIME, TSTEP, INIT_TIME=.TRUE.)
             ! Print Instantaneous Values at first time step
             CALL WRITE_ELMO( JDATE, JTIME, TSTEP, INIT_TIME=.TRUE. )

        ELSE IF ( ELMO_NSTEP .LT. 1.0 ) THEN
             ! Calculate all ELMO Variables for averaging but do not output them
             CALL ELMO_DRIVER( CGRID, JDATE, JTIME, TSTEP, INIT_TIME=.FALSE. )
        END IF
      END IF

C Initialize Budget Output File and Concentration Array
      IF ( BUDGET_DIAG ) CALL BUDGET_INIT( CGRID, JDATE, JTIME, TSTEP )

C science process sequence:
         
      myNREPS = myNREPS + NREPS

      DO IREP = 1, NREPS

         CALL SCIPROC ( CGRID, JDATE, JTIME, TSTEP, ASTEP )

#ifndef mpas
C Use trapezoidal rule to time-average data

         DO V = 1, N_ASPCS
            S = AVG_CONC_MAP( V )
            AGRID( :,:,:,V ) = AGRID( :,:,:,V )
     &            + 2.0 * CGRID( :,:,ACONC_BLEV:ACONC_ELEV,S )
#ifdef sens
            DO NP = 1, NPMAX
               SAGRID( :,:,:,NP,V ) = SAGRID( :,:,:,NP,V )
     &            + 2.0 * SENGRID( :,:,ACONC_BLEV:ACONC_ELEV,NP,S )
            END DO
#endif 
         END DO
         IF ( W_VEL )
     &      AVG_WVEL( :,:,: ) = AVG_WVEL + 2.0 * WVEL( :,:,ACONC_BLEV:ACONC_ELEV )
         IF ( L_ACONC_RH )
     &      AVG_RH( :,:,: ) = AVG_RH + 2.0 * MET_DATA%RH( :,:,ACONC_BLEV:ACONC_ELEV )
         IF ( L_ACONC_TA )
     &      AVG_TA( :,:,: ) = AVG_TA + 2.0 * MET_DATA%TA( :,:,ACONC_BLEV:ACONC_ELEV )
         IF ( L_ACONC_PRES )
     &      AVG_PRES( :,:,: ) = AVG_PRES + 2.0 * MET_DATA%PRES( :,:,ACONC_BLEV:ACONC_ELEV )

#ifdef isam
         ! average isam
         DO ITAG = 1, NTAG_SA
            DO V = 1, NSPC_SA
               L = 0
               DO K = AISAM_BLEV, AISAM_ELEV
                  L = L + 1
                  DO R = 1, NROWS
                     DO C = 1, NCOLS
                        AISAM( C,R,L,V,ITAG ) = AISAM( C,R,L,V,ITAG )
     &                                        + 2.0 * ISAM( C,R,K,V,ITAG )
                     END DO
                  END DO
               END DO
            END DO
         END DO
#endif

#endif
         ! OUTPUT DIAGNOSTIC INFORMATION
         IF ( L_ELMO ) CALL ELMO_DRIVER( CGRID, JDATE, JTIME, TSTEP, INIT_TIME=.FALSE. )

      END DO

#ifndef mpas
      IF ( MOD( TIME2SEC( JTIME ), TIME2SEC( TSTEP( 1 ) ) ) .EQ. 0 ) THEN
         DIVFAC = 0.5 / FLOAT( myNREPS )
         myNREPS = 0

         DO V = 1, N_ASPCS
            S = AVG_CONC_MAP( V )
            AGRID( :,:,:,V ) = DIVFAC * ( AGRID( :,:,:,V )
     &           - CGRID( :,:,ACONC_BLEV:ACONC_ELEV,S ) )
#ifdef sens
            DO NP = 1, NPMAX
               SAGRID( :,:,:,NP,V ) = DIVFAC * ( SAGRID( :,:,:,NP,V )
     &           - SENGRID( :,:,ACONC_BLEV:ACONC_ELEV,NP,S ) )
            END DO
#endif
         END DO
         IF ( W_VEL )
     &         AVG_WVEL( :,:,: ) = DIVFAC * ( AVG_WVEL( :,:,: ) - WVEL( :,:,ACONC_BLEV:ACONC_ELEV ) )
         IF ( L_ACONC_RH )
     &         AVG_RH( :,:,: ) = DIVFAC * ( AVG_RH( :,:,: ) - MET_DATA%RH( :,:,ACONC_BLEV:ACONC_ELEV ) )
         IF ( L_ACONC_TA )
     &         AVG_TA( :,:,: ) = DIVFAC * ( AVG_TA - MET_DATA%TA( :,:,ACONC_BLEV:ACONC_ELEV ) )
         IF ( L_ACONC_PRES )
     &         AVG_PRES( :,:,: ) = DIVFAC * ( AVG_PRES - MET_DATA%PRES( :,:,ACONC_BLEV:ACONC_ELEV ) )

#ifdef isam
         DO ITAG = 1, NTAG_SA ! average isam
            DO V = 1, NSPC_SA
               L = 0
               DO K = AISAM_BLEV, AISAM_ELEV
                  L = L + 1
                  DO R = 1, NROWS
                     DO C = 1, NCOLS
                        AISAM( C,R,L,V,ITAG ) = DIVFAC * ( AISAM(C,R,L,V,ITAG )
     &                                        -            ISAM( C,R,K,V,ITAG ) )
                     END DO
                  END DO
               END DO
            END DO
         END DO
#endif

      END IF

      DO V = 1, N_CSPCS
         S = CONC_MAP( V )
         SGRID( :,:,:,V ) = CGRID( :,:,CONC_BLEV:CONC_ELEV,S )
      END DO

C write conc fields

      IF ( MOD( TIME2SEC( JTIME ), TIME2SEC( TSTEP( 1 ) ) ) .EQ. 0 ) THEN
#ifdef parallel
         CPU_TIME_START =  MPI_WTIME()
#else
         CALL CPU_TIME( REAL_TIME )
         CPU_TIME_START = REAL( REAL_TIME,8 )
#endif
         CALL WR_CONC ( JDATE, JTIME, TSTEP( 1 ) )
#ifdef parallel
         IF ( LVEXT ) CALL WR_VEXT ( CGRID, JDATE, JTIME, TSTEP( 1 ) )
#endif
         CALL WR_ACONC ( AGRID, JDATE, JTIME, TSTEP( 1 ) )
#ifdef isam
         CALL WR_SA     ( JDATE, JTIME, TSTEP, 1 )
         CALL WR_AVG_SA ( JDATE, JTIME, TSTEP( 1 ) )
#endif

#ifdef sens
         CALL WR_ASENS ( JDATE, JTIME, TSTEP( 1 ) )
#endif
         IF ( L_ELMO ) CALL WRITE_ELMO( JDATE, JTIME, TSTEP, INIT_TIME=.FALSE. )

         IF ( LIPR .OR. LIRR ) CALL PA_OUTPUT ( CGRID, JDATE, JTIME )
         IF ( BUDGET_DIAG ) CALL WRITE_BUDGET ( CGRID, JDATE, JTIME, TSTEP )
         IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 3 )
         
      END IF

#ifdef twoway
      IF ( SD_TIME_SERIES ) THEN
         CALL OUTPUT_SD_TIME_SERIES ( CGRID, JDATE, JTIME )
      END IF
#endif

#else
      CALL UNLOAD_CGRID (CGRID)
#endif

      IF ( LAST_STEP ) THEN
#ifdef mpas
         call mio_shutdown()
#else
C write CGRID state file for subsequent runs
         CALL WR_CGRID ( CGRID, JDATE, JTIME, TSTEP( 1 ) )
#ifdef isam
         CALL WR_SA_CGRID ( JDATE, JTIME, TSTEP( 1 ) )
#endif
#ifdef sens
         CALL WR_SENGRID ( JDATE, JTIME, TSTEP( 1 )  )
#endif

C Shut down IOAPI
         IF ( SHUT3() ) THEN
            WRITE( LOGDEV, * )
            CALL LOG_HEADING( LOGDEV, 'Program Completed Successfully' )
            WRITE( XMSG, '(A,A,A,I7,A,I6.6,A)' ) 'Date and time ',
     &             DT2STR( JDATE, JTIME ), ' (',JDATE,':',JTIME,')' 
            CALL LOG_MESSAGE( LOGDEV, XMSG ) 

            IF ( MYPE .EQ. 0 ) WRITE( OUTDEV, * )
            IF ( MYPE .EQ. 0 ) CALL LOG_HEADING( OUTDEV, 'Program Completed Successfully' )
            IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
         ELSE
            CALL LOG_MESSAGE( LOGDEV, ' *** FATAL ERROR shutting down Models-3 I/O *** ' )
         END IF
#endif
      END IF

      END SUBROUTINE CMAQ_DRIVER
