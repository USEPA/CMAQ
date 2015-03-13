
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


! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/yoj/arc/CCTM/src/phot/phot_inline/phot.F,v 1.7 2011/10/21 16:11:28 yoj Exp $

! what(1) key, module and SID; SCCS file; date and time of last delta:
! %W% %P% %G% %U%

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PHOT ( MDATE, MTIME, JDATE, JTIME, RJ )

!-----------------------------------------------------------------------
!
! Function:  Calculates the photolysis rate constant to be used by the
!     chemical solver.  It calculates these rates at each gridcell using
!     codes adapted from JPROC.  Cloud correction now called within the
!     loops over MY-ROW & MY_COLS
!
! Preconditions: HGRD_INIT() called from PAR_INIT, which is called from
!     DRIVER
!
! Subroutines/Functions called: INIT3, M3EXIT, SUBHFILE, CGRID_MAP,
!     OPPHOT, LOAD_CSQY_DATA, LOAD_OPTICS_DATA, INITIALIZE_ALBEDO, 
!     GET_PHOT_MET, UPDATE_SUN, GET_ALBEDO, GET_DROPLET_OPTICS, 
!     GET_ICE_OPTICS, GET_AGGREGATE_OPTICS, CLEAR_HYDROMETEOR_OPTICS, 
!     GET_AERO_DATA, O3TOTCOL, and NEW_OPTICS
!
! Revision History.
!     Started 10/08/2004 with existing PHOT and JPROC coded by
!         Dr. Francis S. Binkowski
!         Carolina Environmental Program
!         University of North Carolina at Chapel Hill
!         email: frank_binkowski@unc.edu
!     August 2005, Sarav Arunachalam, CEP, UNC-CH
!       - Minor revisions while integrating with CMAQ
!       - Error check for NPHOTS added (this version works only for SAPRC-99)
!       - Added creation of new file CTM_RJ_1 to write out RJ values
!         for O3 and NO2 (both clear sky and cloud effects), and
!         ETOT_SFC, TAU_AERO, TAU_TOT and TAUO3_TOP values for 7 wavelengths
!     June 2007, David Wong
!       -- inline with CMAQ
!       - declare RJ as assumed shape array to match with the caller routine
!       - allow PE 0 only to open the output file
!       - output species: NO2_CLOUD and O3_CLOUD with AMISS value when all cells
!         are dark and JTIME_CHK = 0
!       - output species: NO2_CLOUD and O3_CLOUD with AMISS value when CLDATT is
!         0 and JTIME_CHK = 0
!     December 2007, Francis Binkowski
!         code has been modified to call the new on-line version that
!         has the cloud effects built in.  new photolysis routine to
!         replace PHOT in CMAQ
!     January 2008, Shawn Roselle
!       - reformatted for inclusion in CMAQ
!       - added additional 3-d photolysis rate diagnostic file
!       - moved code for opening the diagnostic files to a separate subroutine
!       - moved aerosol pointer evaluation to a FORTRAN module
!       - simplified code for writing the diagnostic file
!       - changed code to call NEW_OPTICS twice, once for clear sky and
!         another time for the cloudy fraction of the grid cell.  RJ's are
!         computed based on the cloud fraction weighting.
!      March 2011, Bill Hutzell
!       - enable wavelength dependent arrays to have an allocatable number
!         of wavelength bins
!       - added data structure and algorithm to compute a surface albedo that
!         depends on time and landuse catagory based on work by John Striecher
!         (AMAD/USEPA)
!       - revised writing to RJ1 file to include surface albedo
!       - moved photolysis and opacity data from CSQY module to an ASCII input
!         file
!       - added routine called LOAD_REF_DATA (inside the PHOT_MOD module) that i
!         reads this input file
!       - added call to a routine called AERO_PHOTDATA that returns opacity data
!         on the aerosol distribution
!       - revised NEW_OPTICS' arguments based on  aerosol redesign in CMAQ
!         version 5.0
!     March 29, 2011 S.Roselle
!       - Replaced I/O API include files with UTILIO_DEFN
!     07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
!     26 Sep 14 B.Hutzell: 1) moved calculation of surface albedo to its own
!                             fortran module
!                          2) changed loading procedure for loading optical data;
!                             two files now used
!                          3) reading and calculation of met and geo data
!                             now acomplished by a fortran module
!                          4) changed description and accounting of cloud effects
!                             from 2D liquid water clouds to 3D resolved and subgrid 
!                             clouds with multi-phases of water
!                          5) inserted calculation of aerosol optical properites via
!                             fortran module to improve efficiency in radiative
!                             transfer solution
!                          6) moved the O3TOTCOL routine from the PHOT_MOD to simplify
!                             the NEW_OPTICS routine
!                          7) Several miscellaneous changes attempting to improve efficiency
!----------------------------------------------------------------------

C...modules

      USE RXNS_DATA            ! chemistry varaibles and data
      USE CGRID_SPCS           ! CGRID species number and offsets
      USE PCGRID_DEFN          ! get cgrid
      USE UTILIO_DEFN

#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
      USE AERO_DATA            ! describes aerosol distribution
      USE PHOT_MOD             ! photolysis in-line module
      USE AERO_PHOTDATA        ! arrays and routines for aerosol dimensions and refractive indices
      USE PHOTOLYSIS_ALBEDO    ! surface albedo data and routines
      USE PHOT_MET_DATA        ! Met and Grid data
      USE CLOUD_OPTICS         ! data and routines for optics of cloud hydrometeors

      IMPLICIT NONE

!...include files

      INCLUDE SUBST_FILES_ID   ! file name parameters
!     INCLUDE SUBST_CONST      ! physical constants--moved to PHOT_MOD.

!...arguments

      INTEGER, INTENT( IN ) :: MDATE          ! "centered" Julian date (YYYYDDD)
      INTEGER, INTENT( IN ) :: MTIME          ! "centered" time (HHMMSS)
      INTEGER, INTENT( IN ) :: JDATE          ! current Julian date (YYYYDDD)
      INTEGER, INTENT( IN ) :: JTIME          ! current time (HHMMSS)

!     REAL RJ( NCOLS,NROWS,NLAYS, NPHOTAB )
      REAL,    INTENT( OUT ) :: RJ(:,:,:,:)   ! gridded J-values  (1/min units)

!     REAL CGRID( NCOLS,NROWS,NLAYS, * )  ! Conc array
      REAL, SAVE, POINTER :: CGRID( :,:,:,: ) ! species concentrations

!...parameters

      LOGICAL, PARAMETER :: CLDATT = .TRUE.   ! include cloud attenuation

      REAL, PARAMETER :: DENS_CONV = ( 1.0E+03 * AVO / MWAIR ) * 1.0E-06  ! convert from kg/m**3 to #/cc
      REAL, PARAMETER :: PPM_MCM3  = 1.0E-06  ! convert from ppm to molecules / cc mol_Spec/mol_Air = ppm * 1E-06
      REAL, PARAMETER :: PRES_CONV = 1.0 / STDATMPA ! conversion factor Pa to atm
      REAL, PARAMETER :: ZTOA      = 50.0E3   ! height of top of atmosphere [ m ] (=50km)
                                              ! based a 2005 WRF model Documentation
                                                 
      REAL, PARAMETER   :: EPSLON  = 1.0E-30  ! Small number

!...external functions: none

!...local variables

      LOGICAL, SAVE :: FIRSTIME = .TRUE.  ! Flag for first call to PHOT
      LOGICAL, SAVE :: PHOTDIAG           ! Flag for PHOTDIAG file

      LOGICAL, SAVE :: CALL_INIT_ALBEDO = .TRUE.
      LOGICAL, SAVE :: CALL_GET_ALBEDO  = .TRUE.
      
      
      LOGICAL       :: ZERO_ICE

      CHARACTER(   3 ), ALLOCATABLE, SAVE :: WLTXT( : )
      CHARACTER(  16 )                    :: VARNM
      CHARACTER(  16 ), SAVE              :: PNAME = 'PHOT'
      CHARACTER(  16 ), SAVE              :: CTM_PHOTDIAG = 'CTM_PHOTDIAG'

      CHARACTER(  80 ) :: VARDESC  ! environment variable description
      CHARACTER( 240 ) :: XMSG = ' '

      INTEGER,    SAVE :: LOGDEV
      INTEGER,    SAVE :: LGC_O3    ! pointer to O3 in CGRID
      INTEGER,    SAVE :: LGC_NO2   ! pointer to NO2 in CGRID
      INTEGER,    SAVE :: TSTEP     ! current timestep

      INTEGER ESTAT              ! status from environment var check
      INTEGER JTIME_CHK          ! To check for JTIME to write RJ values
      INTEGER IPHOT              ! photolysis rate loop index
      INTEGER NDARK              ! Number of level 1 cells in darkness
      INTEGER ROW
      INTEGER COL
      INTEGER LEV
      INTEGER SPC
      INTEGER IWL
      INTEGER L
      INTEGER V, N, MODE
      INTEGER DTIME         ! difference between date and times
      INTEGER ODATE         ! output date
      INTEGER OTIME         ! output time 
      INTEGER ALLOCSTAT

      INTEGER, SAVE :: GXOFF, GYOFF        ! global origin offset from file
! for INTERPX
      INTEGER, SAVE :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2
      INTEGER, SAVE :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

      INTEGER, SAVE :: LDATE         ! date at previous diagnostic output
      INTEGER, SAVE :: LTIME         ! time at previous diagnostic output

      REAL CURRHR          ! current GMT hour
      REAL JULIAN_DAY      ! time of year [days]
      REAL CURRHR_LST      ! local standard time at each grid cell
      REAL CTOP            ! cloud top in single dimension
      REAL CBASE           ! cloud base in single dimension
      REAL ZLEV            ! height in single dimension
      REAL ZEN             ! cosine of zenith angle
      REAL SINLAT          ! sine of latitude
      REAL COSLAT          ! cosine of latitude
      REAL RSQD            ! square of soldist
      REAL ZSFC            ! surface height (msl) [ m ]
      REAL EQT             ! equation of time
      REAL SOLDIST         ! solar distance [ au ]
      REAL SINDEC          ! sine of the solar declination
      REAL COSDEC          ! cosine of the solar declination
      REAL COSZEN          ! working cosine of the solar zenith angle
      REAL SINZEN          ! working sine of the solar zenith angle
      REAL LATCR           ! local latitude
      REAL LONCR           ! local longitude
      REAL OWATER_FRAC     ! Open water fraction
      REAL SNOW_FRAC       ! Snow fractional coverage
      REAL SEAICE_FRAC     ! Sea Ice fraction
      REAL RES_SKY_REFLECT ! reflection coefficient based on resolved sky
      REAL RES_SKY_TRANS   ! diffuse transmission coefficient based on resolved sky
      REAL RES_SKY_TRANSD  ! direct transmission coefficient based on resolved sky

      REAL( 4 )               :: TOTAL_O3_COLUMN ! total ozone column density, DU
      
      REAL,              SAVE :: JYEAR  ! year
      REAL,              SAVE :: JDSTRT ! current Julian day (DDD)

      REAL, ALLOCATABLE, SAVE :: ETOT_SFC ( : )    ! total downward irradiance at sfc [ Watts / m**2  ]
      REAL, ALLOCATABLE, SAVE :: TAUO3_TOP( : )    ! optical depth of ozone above model domain
      REAL, ALLOCATABLE, SAVE :: TAU_RAY  ( : )    ! Rayleigh optical depth above model domain
      REAL, ALLOCATABLE, SAVE :: TAUC_AERO( :,: )  ! aerosol optical depth at layer bottom
      REAL, ALLOCATABLE, SAVE :: TAU_TOT  ( :,: )  ! total optical depth at layer bottom
      REAL, ALLOCATABLE, SAVE :: TAU_CLOUD( :,: )  ! cloud optical depth at layer bottom

      REAL, ALLOCATABLE, SAVE :: SSA      ( : ) ! aerosol single scattering albedo, column average

      REAL MSCALE          ! combined factor to scale ppm to Molecules / cm**3
                           ! and correct for ambient temperaure and pressure

! FSB new arrays for new on-line cloud version

      REAL, ALLOCATABLE, SAVE :: LWC    ( : )      ! cloud liquid water content [ g / m**3 ]
      REAL, ALLOCATABLE, SAVE :: RWC    ( : )      ! rain water content [ g / m**3 ]
      REAL, ALLOCATABLE, SAVE :: IWC    ( : )      ! ice liquid water content [ g / m**3 ]
      REAL, ALLOCATABLE, SAVE :: SWC    ( : )      ! snow content [ g / m**3 ]
      REAL, ALLOCATABLE, SAVE :: GWC    ( : )      ! graupel content [ g / m**3 ]
      REAL, ALLOCATABLE, SAVE :: CLDFRAC( : )      ! fractional cloud cover
      REAL, ALLOCATABLE, SAVE :: BLKPRS ( : )      ! Air pressure in [ Pa ]
      REAL, ALLOCATABLE, SAVE :: BLKTA  ( : )      ! Air temperature [ K ]
      REAL, ALLOCATABLE, SAVE :: BLKDENS( : )      ! Air density  [ molecules / m**3 ]
      REAL, ALLOCATABLE, SAVE :: BLKZH  ( : )      ! layer half-height [ m ]
      REAL, ALLOCATABLE, SAVE :: BLKO3  ( : )      ! O3 concentration [ molecules / cm**3 ]
      REAL, ALLOCATABLE, SAVE :: BLKNO2 ( : )      ! NO2 concentration [ molecules / cm**3 ]
      REAL, ALLOCATABLE, SAVE :: BLKZF  ( : )      ! layer full-height [ m ]

      REAL, ALLOCATABLE, SAVE :: BLKRJ_RES( :, : ) ! photolysis rates
      REAL, ALLOCATABLE, SAVE :: BLKRJ_ACM( :, : ) ! photolysis rates
      
      LOGICAL, ALLOCATABLE, SAVE :: CLOUDS( : )    ! Does layer have clouds?
      LOGICAL                    :: NEW_PROFILE    ! Has atmospheric temperature and density profile changed?
      
!...Variables for diagnostic outputs

      REAL, ALLOCATABLE, SAVE :: TOC( :,: )              ! total ozone column
      REAL, ALLOCATABLE, SAVE :: TRANSMIS_DIFFUSE( :,: ) ! diffuse transmission coefficient at surface
      REAL, ALLOCATABLE, SAVE :: TRANSMIS_DIRECT( :,: )  ! direct transmission coefficient at surface
      REAL, ALLOCATABLE, SAVE :: REFLECT_COEFF ( :,: )   ! reflection coefficient at top of atmosphere
      REAL, ALLOCATABLE, SAVE :: ETOT_SFC_WL ( :,:,: )   ! total downward irradiance at sfc [ Watts / m**2  ]
      REAL, ALLOCATABLE, SAVE :: TAU_AERO_WL ( :,:,: )   ! total aerosol optical depth
      REAL, ALLOCATABLE, SAVE :: TAU_CLOUD_WL( :,:,: )   ! total cloud optical depth
#ifdef phot_research      
      REAL, ALLOCATABLE, SAVE :: CLD_TRANSMISSION( :,: ) ! transmission coefficient of clouds
      REAL, ALLOCATABLE, SAVE :: CLD_REFLECTION  ( :,: ) ! reflection coefficient of cloud
#endif      
#ifdef phot_debug      
      REAL, ALLOCATABLE, SAVE :: ASY_CLOUD_WL( :,:,: ) ! columm average of cloud asymmetry factor
      REAL, ALLOCATABLE, SAVE :: SSA_CLOUD_WL( :,:,: ) ! columm average of cloud single scattering albedo
#endif      
      REAL, ALLOCATABLE, SAVE :: TAU_TOT_WL  ( :,:,: ) ! total optical depth
      REAL, ALLOCATABLE, SAVE :: TAUO3_TOP_WL( :,:,: ) ! optical depth of ozone above model domain

      REAL, ALLOCATABLE, SAVE :: AERO_SSA  ( :,:,:,: ) ! aerosol single scattering albedo
      REAL, ALLOCATABLE, SAVE :: AERO_ASYM ( :,:,:,: ) ! aerosol asymmetry factor
      REAL, ALLOCATABLE, SAVE :: TAU       ( :,:,:,: ) ! optical depth
      REAL, ALLOCATABLE, SAVE :: TAU_AERO  ( :,:,:,: ) ! aerosol optical depth
      REAL, ALLOCATABLE, SAVE :: ACTINIC_FX( :,:,:,: ) ! net actinic flux [ Watts / m**2  ]
      REAL, ALLOCATABLE, SAVE :: BUFFER    ( :,:,: )   ! IO buffer

      INTERFACE
         SUBROUTINE O3TOTCOL ( LATITUDE, LONGITUDE, JDATE, OZONE )
            USE UTILIO_DEFN
            IMPLICIT NONE
            INTEGER,   INTENT(IN)    :: JDATE      ! Julian day of the year (yyyyddd)
            REAL( 4 ), INTENT(IN)    :: LATITUDE   ! latitude of point on earth's surface
            REAL( 4 ), INTENT(IN)    :: LONGITUDE  ! longitude of point on earth's surface
            REAL( 4 ), INTENT(INOUT)   :: OZONE      ! total column ozone [DU]
         END SUBROUTINE O3TOTCOL
      END INTERFACE

! ----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.
         LOGDEV   = INIT3()
         STDATE = JDATE
         STTIME = JTIME
         STRTHR = FLOAT( JTIME / 10000 )
         JDSTRT = FLOAT( MOD( JDATE, 1000 ) )
         JYEAR  = FLOAT( JDATE / 1000 )   !   Check this more carefully

         TSTEP  = 010000  ! output timestep for photolysis diagnostic files
         VARDESC = 'Output timestep (HHMMSS) for photolysis diagnostics'  
         TSTEP = ENVINT( 'CTM_TSTEP', VARDESC, TSTEP, ESTAT )
         IF ( ESTAT .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( ESTAT .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         ELSE IF ( ESTAT .EQ. -1 ) THEN
            XMSG =
     &          'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         ELSE IF ( ESTAT .EQ. -2 ) THEN
            XMSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         END IF

         CGRID => PCGRID( 1:MY_NCOLS,1:MY_NROWS,:,: )

!...Get photolysis rate diagnostic file flag

         PHOTDIAG = .FALSE.         ! default
         VARDESC= 'Flag for writing the photolysis rate diagnostic file'
         PHOTDIAG = ENVYN( CTM_PHOTDIAG, VARDESC, PHOTDIAG, ESTAT )
         IF ( ESTAT .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( ESTAT .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         ELSE IF ( ESTAT .EQ. -1 ) THEN
            XMSG =
     &          'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         ELSE IF ( ESTAT .EQ. -2 ) THEN
            XMSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         END IF

!...Get met file offsets

         CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )
         CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )
         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )
 
         CALL LOAD_CSQY_DATA( )

         CALL LOAD_OPTICS_DATA( )         
 
!...Allocate array needed to calculation aerosol and cloud optical properties

         CALL INIT_AERO_DATA(  )
         
         CALL INIT_CLOUD_OPTICS(  )

!...Initialize Surface albedo method

         IF ( .NOT. INITIALIZE_ALBEDO( JDATE, JTIME, LOGDEV ) ) THEN
              XMSG = 'Failure initializing photolysis surface albedo algorithm'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF


         ALLOCATE( ETOT_SFC ( NWL ) )

         ALLOCATE( LWC    ( NLAYS ) ) 
         ALLOCATE( RWC    ( NLAYS ) ) 
         ALLOCATE( IWC    ( NLAYS ) ) 
         ALLOCATE( SWC    ( NLAYS ) ) 
         ALLOCATE( GWC    ( NLAYS ) ) 
         ALLOCATE( BLKPRS ( NLAYS ) ) 
         ALLOCATE( BLKTA  ( NLAYS ) ) 
         ALLOCATE( BLKDENS( NLAYS ) ) 
         ALLOCATE( BLKZH  ( NLAYS ) ) 
         ALLOCATE( BLKO3  ( NLAYS ) ) 
         ALLOCATE( BLKNO2 ( NLAYS ) ) 
         ALLOCATE( BLKZF  ( NLAYS+1 ) )
         ALLOCATE( CLOUDS ( NLAYS ) ) 
         ALLOCATE( CLDFRAC( NLAYS ) ) 

         ALLOCATE( BLKRJ_RES( NLAYS,NPHOTAB ) )
         ALLOCATE( BLKRJ_ACM( NLAYS,NPHOTAB ) )

         ALLOCATE( TAUO3_TOP( NWL ) )
         ALLOCATE( TAU_RAY  ( NWL ) )
         ALLOCATE( SSA      ( NWL ) )

         ALLOCATE( TAU_CLOUD( NLAYS,NWL ) )
         ALLOCATE( TAUC_AERO( NLAYS,NWL ) )
         ALLOCATE( TAU_TOT  ( NLAYS,NWL ) )

         ALLOCATE( TOC      ( NCOLS,NROWS ) )

         IF ( PHOTDIAG ) THEN
            ALLOCATE( TRANSMIS_DIFFUSE( NCOLS,NROWS ) )
            ALLOCATE( TRANSMIS_DIRECT ( NCOLS,NROWS ) )
            ALLOCATE( REFLECT_COEFF   ( NCOLS,NROWS ) )
#ifdef phot_research   
            ALLOCATE( CLD_TRANSMISSION( NCOLS,NROWS ) )
            ALLOCATE( CLD_REFLECTION  ( NCOLS,NROWS ) )
#endif            
            ALLOCATE( ETOT_SFC_WL     ( NCOLS,NROWS,NWL ) )
            ALLOCATE( TAU_AERO_WL     ( NCOLS,NROWS,NWL ) )
            ALLOCATE( TAU_CLOUD_WL    ( NCOLS,NROWS,NWL ) )
#ifdef phot_debug            
            ALLOCATE( SSA_CLOUD_WL( NCOLS,NROWS,NWL ) )
            ALLOCATE( ASY_CLOUD_WL( NCOLS,NROWS,NWL ) )
#endif            
            ALLOCATE( TAU_TOT_WL  ( NCOLS,NROWS,NWL ) )
            ALLOCATE( TAUO3_TOP_WL( NCOLS,NROWS,NWL ) )

            DIAG_WVL( 1 ) = 1
            DIAG_WVL( N_DIAG_WVL ) = NWL

            ALLOCATE ( AERO_ASYM ( NCOLS,NROWS,NLAYS,N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D AERO_ASYM'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( AERO_SSA ( NCOLS,NROWS,NLAYS,N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D AERO_SSA'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( TAU_AERO ( NCOLS,NROWS,NLAYS,N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D TAU_AERO'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( BUFFER  ( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating IO BUFFER'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( TAU     ( NCOLS,NROWS,NLAYS,N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D TAU'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( ACTINIC_FX ( NCOLS,NROWS,NLAYS,NWL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ACTINIC_FX'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF


!...write wavelength data to a character array

            ALLOCATE ( WLTXT( NWL ) )

            DO IWL = 1, NWL
               WRITE( WLTXT( IWL ),'(I3.3)') INT( WAVELENGTH( IWL ) )
            END DO

         END IF  ! photdiag

!...set pointers to species O3 and NO2 in CGRID

         VARNM = 'O3'
         LGC_O3 = INDEX1( VARNM, N_GC_SPC, GC_SPC )
         IF ( LGC_O3 .LE. 0 ) THEN
            XMSG = 'Could not find ' // VARNM // 'in species table'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         VARNM = 'NO2'
         LGC_NO2 = INDEX1( VARNM, N_GC_SPC, GC_SPC )
         IF ( LGC_NO2 .LE. 0 ) THEN
            XMSG = 'Could not find ' // VARNM // 'in species table'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF
         
         DTIME = 000000
         ODATE = JDATE
         OTIME = JTIME
         LDATE = ODATE
         LTIME = OTIME

!...open the photolysis rate diagnostic files

         IF ( MYPE .EQ. 0 ) CALL OPPHOT ( ODATE, OTIME, TSTEP )

      ELSE

         DTIME = SEC2TIME( 2 * SECSDIFF( JDATE, JTIME, MDATE, MTIME ) )
         ODATE = JDATE
         OTIME = JTIME
         CALL NEXTIME( ODATE, OTIME, DTIME )
         
      END IF  ! firstime


!...Get Met data

      CALL GET_PHOT_MET( JDATE, JTIME, MDATE, MTIME )

!...Get cosine of solar parameters and compute NDARK

      CALL UPDATE_SUN( JDATE, JTIME, MDATE, MTIME )
      
      RSQD = DIST_TO_SUN * DIST_TO_SUN

      IF ( MINVAL( COSINE_ZENITH ) .LE. 0.0 ) THEN
         NDARK = MY_NROWS * MY_NCOLS
      ELSE
         NDARK = 0
      END IF

!...set surface albedos

      CALL GET_ALBEDO( MDATE, MTIME, LOGDEV, COSINE_ZENITH, LAT, LON )

!...SA  Write COSINE_ZENITH array at the end of each hour

      DTIME = SEC2TIME( SECSDIFF( LDATE, LTIME, ODATE, OTIME ) )
      JTIME_CHK = MOD( DTIME, TSTEP )

      IF ( PHOTDIAG .AND. JTIME_CHK .EQ. 0 ) THEN

         LDATE = ODATE
         LTIME = OTIME

         VARNM = 'COSZENS'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME,
     &                      COSINE_ZENITH ) ) THEN
             XMSG = 'Error writing variable ' // VARNM
             CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      END IF ! JTIME_CHK .EQ. 0

!...If sun below horizon at all cells, zero photolysis rates & exit
!...  (assumes sun below horizon at *all* levels!)

      IF ( SUBST_SUM_CHK( NDARK, 'EQ', GL_NCOLS * GL_NROWS ) ) THEN

         RJ = 0.0

!...write to the log file, CTM_RJ_1 file and return

         WRITE( LOGDEV, 1003 ) JDATE, JTIME
1003     FORMAT( 8X, 'In darkness at ', I8.7, ':', I6.6,
     &           1X, 'GMT - no photolysis')

!...Initialize ETOT_SFC, TAU_AERO, TAU_TOT, TAUO3_TOP to 0.0

!...Write data to output diagnostic file

         TOC = 0.0

         IF ( PHOTDIAG .AND. JTIME_CHK .EQ. 0 ) THEN

            ETOT_SFC_WL  = 0.0
            TAU_AERO_WL  = 0.0
            TAU_CLOUD_WL = 0.0
#ifdef phot_debug            
            SSA_CLOUD_WL = 0.0
            ASY_CLOUD_WL = 0.0
#endif            
            TAU_TOT_WL        = 0.0
            TAUO3_TOP_WL      = 0.0
            ACTINIC_FX        = 0.0
            TRANSMIS_DIFFUSE  = 0.0
            REFLECT_COEFF     = 0.0
#ifdef phot_research
            CLD_TRANSMISSION  = 0.0
            CLD_REFLECTION    = 0.0
#endif
            BUFFER = 0.0

            VARNM = 'OZONE_COLUMN'
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, TOC ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'TRANS_DIFFUSE'
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, TRANSMIS_DIFFUSE ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'TRANS_DIRECT'
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, TRANSMIS_DIRECT ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            
            VARNM = 'REFLECTION'
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, REFLECT_COEFF ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
#ifdef phot_research            
            VARNM = 'CLD_TRANSMISS'
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, CLD_TRANSMISSION ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'CLD_REFLECTION'
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, CLD_REFLECTION ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
#endif
            VARNM = 'JNO2'
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, BUFFER ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'JO3O1D'
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, BUFFER ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'RESOLVED_CFRAC'
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, CFRAC_2D ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'RESOLVED_WBAR'
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, AVE_HYDROMETEORS ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            IF ( USE_ACM_CLOUD ) THEN
                VARNM = 'SUBGRID_CFRAC'
                IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, ACM_CLOUDS ) ) THEN
                   XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                END IF
                VARNM = 'SUBGRID_WBAR'
                IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, ACM_AVE_H2O ) ) THEN
                   XMSG = 'Error writing variable ' // VARNM
                   CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                END IF
            END IF
            
            DO IWL = 1, NWL

               VARNM = 'ETOT_SFC_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'TAU_AERO_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'TAU_CLOUD_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
#ifdef phot_debug
               VARNM = 'SSA_CLOUD_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'ASY_CLOUD_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
#endif
               VARNM = 'TAU_TOT_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'TAUO3_TOP_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'ALBEDO_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, SURFACE_ALBEDO( IWL,:,: ) ) ) THEN
!!!!!!!!!!! >>> gcc Fortran runtime warning: An array temporary was created

                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            END DO  ! iwl

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &             'RJ Values written to', CTM_RJ_1,
     &             'for date and time', ODATE, OTIME

            DO IPHOT = 1, NPHOTAB
               IF ( .NOT. WRITE3( CTM_RJ_2, PHOTAB( IPHOT ), ODATE, OTIME, BUFFER ) ) THEN
                  XMSG = 'Could not write ' // CTM_RJ_2 // ' file'
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            END DO

            DO IWL = 1, NWL
               VARNM = 'ACTINIC_FX_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            END DO

            VARNM = 'CFRAC_3D'
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, CFRAC_3D ) ) THEN
                XMSG = 'Could not write ' // TRIM( VARNM ) // ' to ' // CTM_RJ_2 // ' file'
                CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            DO L = 1, N_DIAG_WVL

               IWL = DIAG_WVL( L )

               VARNM = 'AERO_SSA_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'AERO_ASYM_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'TAU_AERO_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

               VARNM = 'TAU_W' // WLTXT( IWL )
               IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, BUFFER ) ) THEN
                  XMSG = 'Error writing variable ' // VARNM
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF

            END DO

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &             'RJ and Optical Data written to', CTM_RJ_2,
     &             'for date and time', ODATE, OTIME

         END IF ! if photdiag .and. jtime_chk .eq. 0

         RETURN
      END IF  ! all cells dark

!...MAIN loop over all rows and columns
      LOOP_ROWS: DO ROW = 1, MY_NROWS
         LOOP_COLS: DO COL = 1, MY_NCOLS
         
            PHOT_COL = COL
            PHOT_ROW = ROW

            COSZEN = COSINE_ZENITH( COL,ROW ) ! local cosine of solar zenith angle

            IF ( COSZEN .LE. 0.0 ) THEN
!...the cell is dark: set variables to zero and cycle
                RJ( COL,ROW, :, : ) = 0.0

                IF ( PHOTDIAG ) THEN
                   TOC( COL,ROW )             = 0.0                            
                   ETOT_SFC_WL ( COL,ROW, : ) = 0.0
                   TAUO3_TOP_WL( COL,ROW, : ) = 0.0
                   TAU_AERO_WL ( COL,ROW, : ) = 0.0
                   TAU_CLOUD_WL( COL,ROW, : ) = 0.0
#ifdef phot_debug                   
                   SSA_CLOUD_WL( COL,ROW, : ) = 0.0
                   ASY_CLOUD_WL( COL,ROW, : ) = 0.0
#endif                   
                   TAU_TOT_WL  ( COL,ROW, : ) = 0.0
                   TAU      ( COL,ROW, :, : ) = 0.0
                   TAU_AERO ( COL,ROW, :, : ) = 0.0
                   AERO_SSA ( COL,ROW, :, : ) = 0.0
                   AERO_ASYM( COL,ROW, :, : ) = 0.0
                   ACTINIC_FX(COL,ROW, :, : ) = 0.0
                   
                   TRANSMIS_DIFFUSE( COL,ROW ) = 0.0
                   TRANSMIS_DIRECT ( COL,ROW ) = 0.0
                   REFLECT_COEFF   ( COL,ROW ) = 0.0
#ifdef phot_research                   
                   CLD_TRANSMISSION( COL,ROW ) = 0.0
                   CLD_REFLECTION  ( COL,ROW ) = 0.0 
#endif
                END IF

                CYCLE LOOP_COLS

            END IF

!...initialize BLKRJ using F90 array operations.

            BLKRJ_RES = 0.0
            BLKRJ_ACM = 0.0

!...Set height of lowest level to zero

            BLKZF( 1 ) = 0.0

            ZSFC   = HT( COL,ROW ) ! surface height [m]
            SINZEN = SQRT( 1.0 - COSZEN * COSZEN ) ! sine of zenith angle

!...local latitude and longitude

!            LATCR = LAT( COL,ROW )
!            LONCR = LON( COL,ROW )

!...get total ozone column based on OMI observations
            CALL O3TOTCOL ( LAT( COL,ROW ), LON( COL,ROW ), JDATE, TOTAL_O3_COLUMN )
            
            IF ( USE_ACM_CLOUD .OR. CLDATT ) THEN
               OWATER_FRAC = MAX( ( 1.0 - SEAICE( COL,ROW ) ), 0.0 )
     &                     * WATER_FRACTION( COL,ROW )
               SEAICE_FRAC = SEAICE( COL,ROW ) * WATER_FRACTION( COL,ROW )
               SNOW_FRAC   = SNOCOV( COL,ROW )
               COL_CLOUD   = COL 
               ROW_CLOUD   = ROW
            END IF

!...loop over vertical layers ambient air conditions and gas concentration
            DO L = 1, NLAYS
!...Fetch the grid cell ambient data at each layer.

               BLKTA  ( L )   = TA   ( COL,ROW,L ) ! temperature [K]
               BLKPRS ( L )   = PRES ( COL,ROW,L ) / STDATMPA  ! [atmospheres]
               BLKDENS( L )   = DENS ( COL,ROW,L ) * DENS_CONV ! [molecules / cm**3]
               BLKZH  ( L )   = ZM   ( COL,ROW,L ) ! mid layer height [m]
               BLKZF  ( L+1 ) = ZFULL( COL,ROW,L ) ! full layer height [m]

!...set scale factor for [ppm] -> [molecule / cm**3]
!...  To go from ppm to molecule/cc:
!...  molecule/cc = ppm *  1.0E-06 * DENS (given in molecule/cc)

               MSCALE = BLKDENS( L ) * PPM_MCM3

!...fetch ozone and no2 and convert to [ molecules / cm **3 ]
!...  and adjust the volume for ambient temperature and pressure.

               BLKO3 ( L ) = CGRID( COL,ROW,L,LGC_O3  ) * MSCALE
               BLKNO2( L ) = CGRID( COL,ROW,L,LGC_NO2 ) * MSCALE
               ZLEV = BLKZF( L )
            END DO ! loop on layers ambient conditions and gases

            IF ( CLDATT .AND. CFRAC_2D( COL,ROW ) .GT. 0.0 ) THEN
               DO L = 1, NLAYS
                 
                  IF ( CFRAC_3D( COL,ROW,L ) .GT. 0.0 ) THEN
                     CLOUDS ( L )        = .TRUE.
                     CLOUD_LAYERING( L ) = .TRUE.
                     CLDFRAC( L )        = CFRAC_3D( COL,ROW,L )
!... set hydrometeor concentrations for resolved cloud
                     MSCALE   = 1.0E+3 * DENS ( COL,ROW,L )
                     IWC( L ) = MSCALE * QI( COL,ROW,L )
                     GWC( L ) = MSCALE * QG( COL,ROW,L )
                     SWC( L ) = MSCALE * QS( COL,ROW,L )    
                     LWC( L ) = MSCALE * QC( COL,ROW,L )
                     RWC( L ) = MSCALE * QR( COL,ROW,L )
                  ELSE
                     CLOUDS ( L )        = .FALSE.
                     CLOUD_LAYERING( L ) = .FALSE.
                     CLDFRAC( L )        = 0.0
                     IWC( L ) = 0.0
                     GWC( L ) = 0.0
                     SWC( L ) = 0.0    
                     LWC( L ) = 0.0
                     RWC( L ) = 0.0
                  END IF
               END DO ! loop on layers clouds
! get optical properties of resolved cloud hydrometeors
               CALL GET_DROPLET_OPTICS( NLAYS, BLKTA, OWATER_FRAC, SEAICE_FRAC, SNOW_FRAC, LWC )     
               CALL GET_ICE_OPTICS( NLAYS, BLKTA, IWC )
               CALL GET_AGGREGATE_OPTICS( NLAYS, RWC, SWC, GWC )     
            ELSE
               CLOUDS         = .FALSE.
               CLOUD_LAYERING = .FALSE.
               CLDFRAC        = 0.0
!  hydrometeor concentrations
               LWC = 0.0
               IWC = 0.0
               RWC = 0.0
               SWC = 0.0
               RWC = 0.0
               CALL CLEAR_HYDROMETEOR_OPTICS()
            END IF
            
!..calculate needed aerosol properties in column

!           IF ( CORE_SHELL ) THEN
               CALL GET_AERO_DATA ( COL,ROW, NLAYS, CGRID )
!           ELSE
!              CALL AERO_OPTICS_INTERNAL( COL,ROW, NLAYS, CGRID )
!           END IF

! set surface albedo

            FORALL ( IWL = 1:NWL )
               ALB( IWL ) = SURFACE_ALBEDO( IWL, COL,ROW )
            END FORALL

!...calculate resolved-sky photolysis rates at all layers:

            NEW_PROFILE    = .TRUE. 
            ONLY_SOLVE_RAD = .FALSE. 
                             
            CALL NEW_OPTICS ( JDATE, JTIME, NLAYS, 
     &                        BLKTA, BLKPRS, BLKDENS, BLKZH, BLKZF,
     &                        BLKO3, BLKNO2,
     &                        ZSFC, COSZEN, SINZEN, RSQD,
     &                        NEW_PROFILE, CLOUDS, CLDFRAC,
     &                        BLKRJ_RES, TAUC_AERO, TAU_TOT, TAUO3_TOP,
     &                        TAU_RAY, SSA, TAU_CLOUD, TOTAL_O3_COLUMN )

!...load diagnostic file arrays

            IF ( PHOTDIAG  ) THEN
               TOC( COL,ROW ) = REAL( TOTAL_O3_COLUMN )
               TRANSMIS_DIFFUSE( COL,ROW ) = TRANSMISSION
               TRANSMIS_DIRECT( COL,ROW )  = TRANS_DIRECT
               REFLECT_COEFF ( COL,ROW )   = REFLECTION
               FORALL( IWL = 1:NWL )
                  ETOT_SFC_WL ( COL,ROW,IWL ) = IRRADIANCE( 1,IWL )
                  TAUO3_TOP_WL( COL,ROW,IWL ) = TAUO3_TOP( IWL )
                  TAU_AERO_WL ( COL,ROW,IWL ) = TAUC_AERO( 1,IWL )
                  TAU_TOT_WL  ( COL,ROW,IWL ) = TAU_TOT  ( 1,IWL )
                  TAU_CLOUD_WL( COL,ROW,IWL ) = TAU_CLOUD( 1,IWL )
#ifdef phot_debug                      
                  SSA_CLOUD_WL( COL,ROW,IWL )  = AVE_SSA_CLD  ( IWL )
                  ASY_CLOUD_WL( COL,ROW,IWL )  = AVE_ASYMM_CLD( IWL )
#endif                      
               END FORALL
               FORALL ( LEV = 1:NLAYS, IWL = 1:NWL  )
                     ACTINIC_FX( COL,ROW,LEV,IWL ) = ACTINIC_FLUX( LEV,IWL )
               END FORALL
                  
               DO L = 1, N_DIAG_WVL
                  IWL = DIAG_WVL( L )
                  FORALL ( LEV = 1:NLAYS )
                     TAU     ( COL,ROW,LEV,L ) = TAU_TOT  ( LEV,IWL )
                     TAU_AERO( COL,ROW,LEV,L ) = TAUC_AERO( LEV,IWL )
                  END FORALL
                  FORALL ( LEV = 1:NLAYS, AERO_EXTI_COEF( LEV,IWL ) .GT. EPSLON )
                      AERO_SSA ( COL,ROW,LEV,L ) = AERO_SCAT_COEF( LEV,IWL )
     &                                           / AERO_EXTI_COEF( LEV,IWL )
                      AERO_ASYM( COL,ROW,LEV,L ) = AERO_ASYM_FAC( LEV,IWL )
                  END FORALL
                  FORALL ( LEV = 1:NLAYS, AERO_EXTI_COEF( LEV,IWL ) .LE. EPSLON )
                      AERO_SSA ( COL,ROW,LEV,L ) = 1.0
                      AERO_ASYM( COL,ROW,LEV,L ) = 0.0
                  END FORALL
               END DO
            END IF

!Set Photolysis rates to resolved sky values
            FORALL ( L = 1:NLAYS, IPHOT = 1:NPHOTAB )
               RJ( COL,ROW, L,IPHOT ) = 60.0 * BLKRJ_RES( L,IPHOT ) 
            END FORALL  ! Loop on layers and NPHOTAB

            IF ( USE_ACM_CLOUD ) THEN
               IF ( ACM_CLOUDS( COL,ROW ) .GT. 0.0 ) THEN
! save resolved sky reflection and transmission coefficients for possible latter use
                  RES_SKY_REFLECT = REFLECTION
                  RES_SKY_TRANS   = TRANSMISSION
                  RES_SKY_TRANSD  = TRANS_DIRECT
!...find the highest layer of the sub-grid (convective) cloud
                  DO LEV = NLAYS, 1, -1
                     IF ( ACM_CFRAC( LEV, COL,ROW ) .GT. 0.0 )EXIT
                  END DO
!...replace the lower layers with sub-grid cloud properties
                  DO L = 1, LEV
                     SWC( L ) = 0.0
                     IF ( ACM_CFRAC( L,COL,ROW ) .GT. 0.0 ) THEN
                        CLOUDS ( L ) = .TRUE.
                        CLDFRAC( L ) = 1.0
                        MSCALE = 1.0E+3 * DENS ( COL,ROW, L )
                        LWC( L ) = MSCALE * ACM_QC( L,COL,ROW )
                        IWC( L ) = MSCALE * ACM_QI( L,COL,ROW )
                        RWC( L ) = MSCALE * ACM_QR( L,COL,ROW )
                        GWC( L ) = MSCALE * ACM_QG( L,COL,ROW )
                     ELSE
                        CLOUDS( L )  = .FALSE.
                        CLDFRAC( L ) = 0.0
                        LWC( L )     = 0.0
                        IWC( L )     = 0.0
                        RWC( L )     = 0.0
                        GWC( L )     = 0.0
                     END IF
                     CLOUD_LAYERING( L ) = .FALSE.
                  END DO
!                 write(logdev,*)'ACM cloud present fraction, cloud lwc(lev),iwc(lev),rwc(1),gwc(1) = ',
!     &           ACM_CLOUDS( COL,ROW ),lwc(lev),iwc(lev),rwc(1),gwc(1)
                 
! get optical properties of of subgrid cloud hydrometeors
                  CALL GET_DROPLET_OPTICS( LEV, BLKTA, OWATER_FRAC, SEAICE_FRAC, SNOW_FRAC, LWC )     
                  CALL GET_ICE_OPTICS( LEV, BLKTA, IWC )     
                  CALL GET_AGGREGATE_OPTICS( LEV, RWC, SWC, GWC )     

!...calculate the acm-cloud photolysis rates for all layers:
                  NEW_PROFILE = .FALSE.   
                  CALL NEW_OPTICS ( JDATE, JTIME, NLAYS, 
     &                              BLKTA, BLKPRS, BLKDENS, BLKZH, BLKZF,
     &                              BLKO3, BLKNO2,
     &                              ZSFC, COSZEN, SINZEN, RSQD,
     &                              NEW_PROFILE, CLOUDS, CLDFRAC,
     &                              BLKRJ_ACM, TAUC_AERO, TAU_TOT, TAUO3_TOP,
     &                              TAU_RAY, SSA, TAU_CLOUD, TOTAL_O3_COLUMN )

!...load diagnostic file arrays
!...compute a cloud-fraction weighted average of ETOT_SFC and TAU_TOT
!...  note that both TAUC_AERO and TAUO3_TOP are the same for clear and
!...  cloudy regions
                  MSCALE = MAX( 1.0 - ACM_CLOUDS( COL,ROW ), 0.0 )
                  
                  IF ( PHOTDIAG ) THEN

                     TRANSMIS_DIRECT( COL,ROW )  = MSCALE * TRANSMIS_DIRECT( COL,ROW )
     &                                           + ACM_CLOUDS( COL,ROW ) * TRANS_DIRECT
                     TRANSMIS_DIFFUSE( COL,ROW ) = MSCALE * TRANSMIS_DIFFUSE( COL,ROW )
     &                                           + ACM_CLOUDS( COL,ROW ) * TRANSMISSION
                     REFLECT_COEFF( COL,ROW )    = MSCALE * REFLECT_COEFF( COL,ROW )
     &                                           + ACM_CLOUDS( COL,ROW ) * REFLECTION
                     FORALL ( IWL = 1:NWL )
                        ETOT_SFC_WL ( COL,ROW,IWL ) = MSCALE * ETOT_SFC_WL( COL,ROW,IWL )
     &                                              + ACM_CLOUDS( COL,ROW ) * IRRADIANCE( 1,IWL )
                        TAU_TOT_WL  ( COL,ROW,IWL ) = MSCALE * TAU_TOT_WL( COL,ROW,IWL )
     &                                              + ACM_CLOUDS( COL,ROW ) * TAU_TOT( 1,IWL )
                        TAU_CLOUD_WL( COL,ROW,IWL ) = MSCALE * TAU_CLOUD_WL( COL,ROW,IWL )
     &                                              + ACM_CLOUDS( COL,ROW ) * TAU_CLOUD( 1,IWL )
#ifdef phot_debug     
                        SSA_CLOUD_WL( COL,ROW,IWL ) = MSCALE * SSA_CLOUD_WL( COL,ROW,IWL )
     &                                              + ACM_CLOUDS( COL,ROW ) * AVE_SSA_CLD ( IWL )
                        ASY_CLOUD_WL( COL,ROW,IWL ) = MSCALE * ASY_CLOUD_WL( COL,ROW,IWL )
     &                                              + ACM_CLOUDS( COL,ROW ) * AVE_ASYMM_CLD( IWL )
#endif     
                     END FORALL   ! iwl
                     FORALL ( LEV = 1:NLAYS, IWL = 1:NWL )
                        ACTINIC_FX( COL,ROW,LEV,IWL ) = MSCALE * ACTINIC_FX( COL,ROW,LEV,IWL )
     &                                                + ACM_CLOUDS( COL,ROW ) * ACTINIC_FLUX( LEV,IWL )
                     END FORALL ! lev and iwl
                  
                     DO L = 1, N_DIAG_WVL
                        IWL = DIAG_WVL( L )
                        FORALL ( LEV = 1:NLAYS)
                           TAU( COL,ROW,LEV,L ) = MSCALE * TAU( COL,ROW,LEV,L )
     &                                          + ACM_CLOUDS( COL,ROW ) * TAU_TOT( LEV,IWL )
                        END FORALL
                     END DO
                  END IF  ! photdiag
!Photolysis rates become a weighted average of the values from resolved and ACM skies
                  FORALL ( L = 1:NLAYS, IPHOT = 1:NPHOTAB )
                      RJ( COL,ROW, L, IPHOT ) = 60.0 * ACM_CLOUDS( COL,ROW ) * BLKRJ_ACM( L,IPHOT ) 
     &                                         + MSCALE * RJ( COL,ROW,L,IPHOT )
                  END FORALL  ! Loop on layers and PHOT
               END IF 
            END IF ! not USE_ACM_CLOUD and  ACM_CLOUDS > 0

#ifdef phot_research
! Not ready. The correct calculation requires further research.
! Also, significantly increases runtime because of the additional call to NEW_OPTICS
            IF ( PHOTDIAG ) THEN ! compute cloud reflectiion and transmission coefficients
               IF ( ANY( CLOUDS ) ) THEN
                  IF ( CFRAC_2D( COL,ROW ) .GT. 0.0 ) THEN
                     CLOUDS         = .FALSE.
                     NEW_PROFILE    = .FALSE.
                     ONLY_SOLVE_RAD = .TRUE.  
                     CALL NEW_OPTICS ( JDATE, JTIME, NLAYS, 
     &                                 BLKTA, BLKPRS, BLKDENS, BLKZH, BLKZF,
     &                                 BLKO3, BLKNO2,
     &                                 ZSFC, COSZEN, SINZEN, RSQD,
     &                                 NEW_PROFILE, CLOUDS, CLDFRAC,
     &                                 BLKRJ_RES, TAUC_AERO, TAU_TOT, TAUO3_TOP,
     &                                 TAU_RAY, SSA, TAU_CLOUD, TOTAL_O3_COLUMN)
                     CLD_REFLECTION  ( COL,ROW ) = REFLECT_COEFF( COL,ROW ) - REFLECTION
                     CLD_TRANSMISSION( COL,ROW ) = 1.0 + TRANSMISSION 
     &                                           - CLD_REFLECTION( COL,ROW )
     &                                           - TRANSMIS_DIFFUSE( COL,ROW )           
                  ELSE
                     CLD_REFLECTION  ( COL,ROW ) = REFLECT_COEFF( COL,ROW ) - RES_SKY_REFLECT
                     CLD_TRANSMISSION( COL,ROW ) = 1.0 + RES_SKY_TRANS
     &                                           - CLD_REFLECTION( COL,ROW )
     &                                           - TRANSMIS_DIFFUSE( COL,ROW )           
                  END IF
               ELSE
                  CLD_REFLECTION  ( COL,ROW ) = 0.0                             
                  CLD_TRANSMISSION( COL,ROW ) = 1.0
               END IF
            END IF
#endif
         END DO LOOP_COLS
      END DO LOOP_ROWS

!...write diagnostic data to output file at the end of every hour

      IF ( PHOTDIAG .AND. JTIME_CHK .EQ. 0 ) THEN

         VARNM = 'OZONE_COLUMN'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, TOC ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'TRANS_DIFFUSE'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, TRANSMIS_DIFFUSE ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'REFLECTION'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, REFLECT_COEFF ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'TRANS_DIRECT'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, TRANSMIS_DIRECT ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
#ifdef phot_research         
         VARNM = 'CLD_TRANSMISS'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, CLD_TRANSMISSION ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'CLD_REFLECTION'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, CLD_REFLECTION ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
#endif
         VARNM = 'JNO2'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, RJ( :,:,1, LNO2 ) ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'JO3O1D'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, RJ( :,:,1,LO3O1D ) ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'RESOLVED_CFRAC'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, CFRAC_2D ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'RESOLVED_WBAR'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, AVE_HYDROMETEORS ) ) THEN
             XMSG = 'Error writing variable ' // VARNM
             CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         BUFFER = 0.0

         IF ( USE_ACM_CLOUD ) THEN
             VARNM = 'SUBGRID_CFRAC'
             IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, ACM_CLOUDS ) ) THEN
                XMSG = 'Error writing variable ' // VARNM
                CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF
             VARNM = 'SUBGRID_WBAR'
             IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, ACM_AVE_H2O ) ) THEN
                XMSG = 'Error writing variable ' // VARNM
                CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF
         END IF

         DO IWL = 1, NWL

            VARNM = 'ETOT_SFC_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE,
     &                         OTIME, ETOT_SFC_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, ODATE, OTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'TAU_AERO_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE,
     &                         OTIME, TAU_AERO_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'TAU_CLOUD_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE,
     &                         OTIME, TAU_CLOUD_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

#ifdef phot_debug
            VARNM = 'SSA_CLOUD_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE,
     &                         OTIME, SSA_CLOUD_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'ASY_CLOUD_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE,
     &                         OTIME, ASY_CLOUD_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
#endif

            VARNM = 'TAU_TOT_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE,
     &                         OTIME, TAU_TOT_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'TAUO3_TOP_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE,
     &                         OTIME, TAUO3_TOP_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'ALBEDO_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME,
     &                         SURFACE_ALBEDO( IWL,:,: ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         END DO  ! iwl

         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &          'RJ Values written to', CTM_RJ_1,
     &          'for date and time', ODATE, OTIME

         DO IPHOT = 1, NPHOTAB
            IF ( .NOT. WRITE3( CTM_RJ_2, PHOTAB( IPHOT ), ODATE,
     &                         OTIME, RJ( :,:,:,IPHOT ) ) ) THEN
               XMSG = 'Could not write ' // CTM_RJ_2 // ' file'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END DO

         VARNM = 'CFRAC_3D'
         IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, CFRAC_3D ) ) THEN
              XMSG = 'Could not write ' // TRIM( VARNM ) // ' to ' // CTM_RJ_2 // ' file'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         
         DO IWL =  1, NWL
            FORALL( COL = 1:MY_NCOLS, ROW = 1:MY_NROWS, LEV = 1:NLAYS )
                BUFFER( COL,ROW,LEV ) =  ACTINIC_FX( COL,ROW,LEV,IWL )
            END FORALL 
            VARNM = 'ACTINIC_FX_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, BUFFER ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END DO

         DO L = 1, N_DIAG_WVL
            IWL = DIAG_WVL( L )

            FORALL( COL = 1:MY_NCOLS, ROW = 1:MY_NROWS, LEV = 1:NLAYS )
                BUFFER( COL,ROW,LEV ) =  AERO_SSA( COL,ROW,LEV,L )
            END FORALL 

            VARNM = 'AERO_SSA_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, BUFFER ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            FORALL( COL = 1:MY_NCOLS, ROW = 1:MY_NROWS, LEV = 1:NLAYS )
                BUFFER( COL,ROW,LEV ) =  AERO_ASYM( COL,ROW,LEV,L )
            END FORALL 

            VARNM = 'AERO_ASYM_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, BUFFER ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            FORALL( COL = 1:MY_NCOLS, ROW = 1:MY_NROWS, LEV = 1:NLAYS )
                BUFFER( COL,ROW,LEV ) =  TAU_AERO( COL,ROW,LEV,L )
            END FORALL 

            VARNM = 'TAU_AERO_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, BUFFER ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            FORALL( COL = 1:MY_NCOLS, ROW = 1:MY_NROWS, LEV = 1:NLAYS )
                BUFFER( COL,ROW,LEV ) =  TAU( COL,ROW,LEV,L )
            END FORALL 

            VARNM = 'TAU_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, BUFFER ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END DO

         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &          'RJ and Optical Data written to', CTM_RJ_2,
     &          'for date and time', ODATE, OTIME

      END IF   ! if photdiag .and. jtime_chk .eq. 0

      RETURN
      END SUBROUTINE PHOT
