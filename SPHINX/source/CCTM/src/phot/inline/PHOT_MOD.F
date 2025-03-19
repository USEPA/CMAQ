
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
      MODULE PHOT_MOD

C-----------------------------------------------------------------------
C
C  FSB This version has NO internal write statements
C  FSB This version has the code for XR96 added.
C  FSB change indices from L to II in newOptics loop 08/17/2006
C  FSB This version has all write statements commented out.(08/03/2006)
C
C  FSB NOTE - this code assumes that the top of the modeling domain
C  is about 100 [mb] or 10 [kPa] ~ 16 [km] in altitude. If a
C  higher altitude top is used , the method of calculating the
C  ozone column and the ozone optical depth will be necessary.
C
C  FSB This version has the addition of Rayleigh optical depth for the
C  stratosphere as well as the calculation of single scattering
C  albedo for the AOD calculation. (01/17/2006)
C  FSB This version has deleted the JPROC values of Cs and Qy as well as
C  the default aerosol.  It also contains the fast optics
C  routines.
C  FSB This module supports the SAPRC99 Chemical mechanism within
C  CMAQ.
C  FSB This version calls a fast optical routine for aerosol
C  extinction and scattering
C  FSB This version uses a set of constant refractive indices
C  The new subroutine GETNEWPAR now sets up the refractive indices.
C  
C  Bill Hutzell(Mar 2011) moved determining refractive indices to a
C  separate file and new subroutine called AERO_PHOTDATA.
C  
C  Bill Hutzell(Jun 2011) modified TWOSTREAM_S subroutine to account for 
C  GAM2 equal to zero in the Toon et al. (1989) solution to the two stream
C  of the radiative transfer equation based on how the NCAR TUV model 
C  implements the approximation
C
C  Bill Hutzell(May 2013) modified optical depth agruments to give vetical
C  profile rather than surface values. Note that TAU_TOT now includes 
C  stratospheric values.
! Bill Hutzell(Mar 2014) modified calculation of aerosol and cloud optical
! properites as well as their calculated optical depths. The changes employ
! FORTRAN modules that contain the layer level of the optical properties.
C  07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
C  10/10/14 - DJL added references to IUPAC10 to NO2 and O3 photo rates
C  23Jun15   B.Hutzell: made TWOSTREAM and TRIDIAGONAL routine use REAL(8) variables
C  30Jul15  J.Young: REAL(4) -> REAL for code portability
C-----------------------------------------------------------------------

      USE RUNTIME_VARS    
      USE CSQY_DATA
      USE PHOT_MET_DATA, ONLY: CFRAC_2D_AVE => CFRAC_2D

      IMPLICIT NONE

!***include files

      INCLUDE SUBST_CONST      ! physical constants

!***parameters

      REAL, PARAMETER :: SMALL = 1.0E-36    ! a small number

!***Fundamental Constants: ( Source: CRC76, pp 1-1 to 1-6)

      REAL, PARAMETER :: PLANCK_C = 6.62606876E-34 ! Planck's Constant [Js]
      REAL, PARAMETER :: LIGHT_SPEED = 299792458.0 ! speed of light in a vacuum

      REAL, PARAMETER :: DU_TO_CONC = 2.6879E16        ! factor from [DU] to [molecules/cm^2]
      REAL, PARAMETER :: CONC_TO_DU = 1.0 / DU_TO_CONC 
      
      LOGICAL, PARAMETER :: ADJUST_OZONE = .FALSE.     ! Flag to correct tropospheric ozone optical depth based
                                                       ! on climatology

       REAL :: MIN_STRATO3_FRAC ! minimum fraction of O3 column in statosphere
       REAL :: MAX_TROPOO3_FRAC ! maximum fraction of O3 column in troposphere

!      REAL, PARAMETER :: MIN_STRATO3_FRAC = 0.55                   ! minimum fraction of O3 column in statosphere
                                                                    !  if PTOP = 50 mb
!      REAL, PARAMETER :: MAX_TROPOO3_FRAC = 1.0 - MIN_STRATO3_FRAC ! maximum fraction of O3 column in troposphere

!***LOGDEV for NEW_OPTICS and supporting routines
      
      INTEGER, SAVE :: NEW_OPTICS_LOG

      INTEGER, SAVE              :: N_DIAG_WVL    ! number of diagnostic wavelengths in PHOTDIAG3 file
      INTEGER, ALLOCATABLE, SAVE :: DIAG_WVL( : ) ! pointers to diagnostic wavelengths
      
      INTEGER            :: N_TROPO_O3_TOGGLE      ! number of adjustments to ozone extinction 
 
      REAL, ALLOCATABLE  :: RJ      ( :, :, :, : ) ! average grid cell J-values (min-1)
      REAL, ALLOCATABLE  :: RJ_RES  ( :, :, :, : ) ! resolved cloud J-values (min-1)
      REAL, ALLOCATABLE  :: RJ_SUB  ( :, :, :, : ) ! subgrid cloud J-values (min-1)
      REAL, ALLOCATABLE  :: ETOT_SFC_WL ( :,:,: )  ! total downward irradiance at sfc [ Watts / m**2  ]
      
      REAL, ALLOCATABLE  :: BLKHCHO( : )           ! formaldehyde concentration [ molecules/cm**3 ]
      REAL, ALLOCATABLE  :: BLKCO  ( : )           ! CO concentration [ molecules/cm**3 ]
      REAL, ALLOCATABLE  :: BLKSO2 ( : )           ! SO2 concentration [ molecules/cm**3 ]
      REAL, ALLOCATABLE  :: BLKDZ  ( : )           ! layer thicknesses [ m ]
      REAL, ALLOCATABLE  :: GAS_EXTINCTION( :,: )  ! extinction from gases per layer [1/m]
      REAL, ALLOCATABLE  :: EXTINCTION    ( :,: )  ! total extinction per layer [1/m]
      REAL, ALLOCATABLE  :: ACTINIC_FLUX  ( :,: )  ! actinic fluxes, initially [Photons/(cm^2s)] then  [Watts/m^2] 
      REAL, ALLOCATABLE  :: IRRADIANCE    ( :,: )  ! total downward irradiance [Watts/m^2]
      REAL               :: REFLECTION             ! broad band reflection coefficient (diffuse) at model top 
      REAL               :: TRANSMISSION           ! broad band transmission coefficient (diffuse) at surface
      REAL               :: TRANS_DIRECT           ! broad band direct transmission coefficient at surface
      REAL               :: TROPO_O3_COLUMN        ! ozone column density in the troposphere [Dobson Units]
      REAL               :: TROPO_O3_TOGGLE        ! factor correcting tropospheric ozone column 
      REAL               :: O3_TOGGLE_AVE          ! average of nonunity factors adjusting ozone extinction 
      REAL               :: O3_TOGGLE_MIN          ! Max of nonunity factors adjusting ozone extinction 
      REAL               :: COS85                  ! cosine of 85 degrees
  
      ! Summary Aerosol Metrics Needed by the PM Diagnostic Module
      REAL, ALLOCATABLE, SAVE :: TAU_AERO_550( :,: )   ! total aerosol optical depth at 550 nm (Angstrom Interpolation)
      REAL, ALLOCATABLE, SAVE :: AERO_EXT_550( :,:,: ) ! aerosol extinction at 550 nm for layer [1/m]
      REAL, ALLOCATABLE, SAVE :: TAU_ABS_AERO_550( :,: ) ! sbsorption aerosol optical depth at 550 nm (Angstrom Interpolation)
      REAL, ALLOCATABLE, SAVE :: AERO_ABS_550( :,:,: )   ! aerosol absorption coeff at 550 nm for layer [1/m]

      LOGICAL            :: ONLY_SOLVE_RAD             ! only compute fluxes     
      LOGICAL            :: OBEY_STRATO3_MINS = .TRUE. ! Has stratospheric O3 column not violated 
                                                       ! climatological minimums, yet?
      LOGICAL            :: STRATO3_MINS_MET           ! Does the call to NEW_OPTICS meet the stratospheric O3 column
                                                       ! climatological minimums?

           
      CHARACTER( 133 )   :: PHOT_MOD_MSG
      
      INTEGER            :: PHOT_COL   ! cell column of routine calling module routine
      INTEGER            :: PHOT_ROW   ! cell row of routine calling module routine


      CONTAINS
      
      SUBROUTINE INIT_PHOT_SHARED()

          USE RXNS_DATA            ! chemistry variables and data
          USE GRID_CONF            ! horizontal & vertical domain specifications
          USE UTILIO_DEFN
! Purpose: initialize data and arrays shared by other science processes      
          IMPLICIT NONE
!Arguments: None

!Local:          
          INTEGER          :: ALLOCSTAT
          CHARACTER( 240 ) :: XMSG = ' '
          LOGICAL, SAVE    :: INITIALIZED = .FALSE.
          
          IF ( INITIALIZED ) RETURN
          
          INITIALIZED = .TRUE.

          CALL LOAD_CSQY_DATA( )

          CALL LOAD_OPTICS_DATA( )

          ALLOCATE( RJ      ( NCOLS, NROWS, NLAYS, NPHOTAB ),
     &              RJ_RES  ( NCOLS, NROWS, NLAYS, NPHOTAB ),
     &              RJ_SUB  ( NCOLS, NROWS, NLAYS, NPHOTAB ), STAT = ALLOCSTAT )
          IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating photolysis rate arrays'
              CALL M3EXIT ( 'INIT_PHOT_SHARED', 0, 0, XMSG, XSTAT1 )
          END IF

          ALLOCATE( ETOT_SFC_WL( NCOLS,NROWS,NWL ), STAT = ALLOCSTAT )
          IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating irradiance rate array'
              CALL M3EXIT ( 'INIT_PHOT_SHARED', 0, 0, XMSG, XSTAT1 )
          END IF

          RJ = 0.0; RJ_RES = 0.0; RJ_SUB = 0.0
          ETOT_SFC_WL = 0.0
          
      END SUBROUTINE INIT_PHOT_SHARED

C///////////////////////////////////////////////////////////////////////
      SUBROUTINE NEW_OPTICS ( JDATE, JTIME, NLAYS, 
     &                        BLKTA, BLKPRS, BLKDENS, BLKZH, BLKZF,
     &                        BLKO3, BLKNO2,
     &                        ZSFC, COSZEN, SINZEN, RSQD,
     &                        NEW_PROFILE, CLOUDS, CLDFRC,
     &                        BLKRJ, TAUC_AERO, TAU_TOT, TAUO3_TOP,
     &                        TAU_RAY, SSA_AERO, TAU_CLOUD, TOTAL_O3_COLUMN )
C-----------------------------------------------------------------------
C
C  FSB  NOTE new call vector <<<<<<<<<<<<< **********
C
C  FSB This version has clouds
C  FSB calculates the photolysis rates as a function of species and height
C
C  first coded 10/19/2004 by Dr. Francis S. Binkowski
C     Carolina Environmental Program
C     University of North Carolina at Chapel Hill
C     email: frank_binkowski@unc.edu
C  modified by FSB  July 29, 2005, 01/19/2006  by FSB
C
C  Mar 2011 Bill Hutzell
C      -revised arguement to account for aerosol redesign in 
C       CMAQ version 5.0
C      -change array declaration to allow flexible number of
C       wavelength bins 
C  Apr 2012 Bill Hutzell
C      -revised error checking to needed photolysis data 
C      -modified case statement for RACM2 photolysis rates
C      -moved aerosol optics to its own module
C  07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
C-----------------------------------------------------------------------

      USE UTILIO_DEFN
      USE RXNS_DATA            ! chemical mechanism data
      USE CLOUD_OPTICS         ! data and routines for optics of cloud hydrometeors

      USE AERO_PHOTDATA
      
      IMPLICIT NONE

!***arguments
      INTEGER, INTENT(IN) :: JDATE   ! julian date YYYYDDD
      INTEGER, INTENT(IN) :: JTIME   ! TIME HHMMSS
      INTEGER, INTENT(IN) :: NLAYS   ! # of vertical layers
      
      REAL,    INTENT(IN) :: BLKPRS ( : )       ! Air pressure in [ atm ]
      REAL,    INTENT(IN) :: BLKTA  ( : )       ! Air temperature [ K ]
      REAL,    INTENT(IN) :: BLKDENS( : )       ! Air density  [ molecules / cm**3 ]
      REAL,    INTENT(IN) :: BLKZH  ( : )       ! layer half-height [ m ]
      REAL,    INTENT(IN) :: BLKZF  ( : )       ! layer full height[ m ]
      REAL,    INTENT(IN) :: BLKO3  ( : )       ! O3 concentration [ molecules / cm**3 ]
      REAL,    INTENT(IN) :: BLKNO2 ( : )       ! NO2 concentration [ molecules / cm**3 ]
      REAL,    INTENT(IN) :: ZSFC               ! surface height (msl) [ m ]
      REAL,    INTENT(IN) :: COSZEN, SINZEN     ! sine and cosine of the zenith angle
      REAL,    INTENT(IN) :: RSQD               ! square of  solar distance [ au**2 ]

      LOGICAL, INTENT(IN) :: NEW_PROFILE     ! Has the atmospheric profile changed since last call?
      LOGICAL, INTENT(IN) :: CLOUDS( : )  ! Does layer have clouds
      REAL,    INTENT(IN) :: CLDFRC( : )  ! fraction of gridcell covered by cloud
      

      REAL,    INTENT(OUT) :: BLKRJ( :,: )  ! photolysis rates [ 1 / sec ]

      REAL,    INTENT(OUT) :: TAUC_AERO( :,: )  ! aerosol optical depth, bottom of layer
      REAL,    INTENT(OUT) :: TAU_TOT  ( :,: )  ! total optical depth, bottom of layer
      REAL,    INTENT(OUT) :: TAU_CLOUD( :,: )  ! cloud optical depth, bottom of layer

      REAL,    INTENT(INOUT) :: TAUO3_TOP( : )  ! optical depth of ozone above model domain
      REAL,    INTENT(INOUT) :: TAU_RAY  ( : )  ! Rayleigh optical depth above model domain
      REAL,    INTENT(OUT)   :: SSA_AERO ( : )  ! single scatering albedo for aerosol column
      
      REAL, INTENT(INOUT) :: TOTAL_O3_COLUMN  ! total ozone colum density [ DU ]

!***internal
      REAL, PARAMETER :: ONE_OVER_PI = 1.0 / PI 
      REAL, PARAMETER :: STRAT_TEMP  = 225.0    ! stratospheric temperature
      REAL, PARAMETER :: ZTOA        = 50.0E3   ! top of the atmosphere [ m ]

      INTEGER L, I, IWL, II, ILEV, IPHOT, MODE ! loop indices

      INTEGER NLEVEL
      REAL SOLAR_FLUX        ! solar flux at atmosphere top in a wavelength band, [photons/(cm^2*s)]
      REAL INSOLATION        ! downward solar flux at atmosphere top summed over wavelength bands, [photons/(cm^2*s)]

      REAL DELTA_O3_COLUMN   ! change in ozone column density [molecules/cm2]
      REAL STRAT_O3_COLUMN   ! ozone column density in the stratosphere [molecules/cm2]
      REAL STRAT_O3_COLMIN   ! ozone minium column density in the stratosphere [molecules/cm2]
      REAL TAU_O3            ! optical depth of stratospheric ozone [ m ]
      REAL DENSTOM           ! estimated air density at top of model [ molecules / cm**3 ]
      REAL LAMDA             ! wavelength  [ nm ]
      REAL INV_LAMBDA        ! reciprocal of wavelength [ 1/nm ]
      REAL LAMDA_UM          ! wavelength  [ um ]

!***working absorption cross sections [ cm**2 ]. These have been corrected
!***  for ambient ( pressure and temperature ) conditions.

      REAL AO3
      REAL ANO2
      REAL BETA_M     ! molecular scattering coefficient [ 1/m ]
      REAL BEXT       ! total aerosol extinction coefficient [ 1/m ]
      REAL VFAC, BSC  ! unit correction factors
      REAL BSCAT      ! total aerosol scattering coefficient [ 1/m ]
      REAL G_BAR      ! total aerosol asymmetry factor

!***FSB The following variable is aq switch that allows a fast version of
!***  aerosol optics to be used when set to .TRUE.

!***scattering and absorption for the layer

      REAL DTABS_A, DTABS_M, DTSCAT_A, DTSCAT_M, DTSCAT, DTABS

!***variables describing the layer heights and slants
!     REAL DJ, DF
      REAL ZTOM                               ! top of model  [ m ]
      REAL, ALLOCATABLE, SAVE :: DSDH_TD( : ) ! slant path function from top down
      REAL, ALLOCATABLE, SAVE :: DSDH( : )    ! slant path function
      REAL, SAVE              :: DSDH_TOP     ! slantpath function from ZTOM to ZTOA

!***Increment of optical depth

      REAL, ALLOCATABLE, SAVE :: DTAU    ( : )   ! total depth at level
      REAL, ALLOCATABLE, SAVE :: DT_AERO ( : )   ! aerosol contribution at level
      REAL, ALLOCATABLE, SAVE :: DT_CLOUD( : )   ! cloud contribution at level

!***single scattering albedo for layer

      REAL, ALLOCATABLE, SAVE :: OM( : )

!***asymmetry factor

      REAL, ALLOCATABLE, SAVE :: G( : )

!***arrays for fluxes and irradiances used in

!***delta-Eddington code

      REAL, ALLOCATABLE, SAVE :: FDIR( : )   ! direct actinic flux
      REAL, ALLOCATABLE, SAVE :: FUP ( : )   ! diffuse upward actinic flux
      REAL, ALLOCATABLE, SAVE :: FDN ( : )   ! diffuse downward flux
      REAL, ALLOCATABLE, SAVE :: EDIR( : )   ! direct irradiance
      REAL, ALLOCATABLE, SAVE :: EUP ( : )   ! diffuse upward irradiance
      REAL, ALLOCATABLE, SAVE :: EDN ( : )   ! diffuse downward irradiance

!***surface albedo

      REAL RSFC

      REAL FX
      REAL, ALLOCATABLE, SAVE :: ESUM( : )   ! total downward irradiance
      REAL, ALLOCATABLE, SAVE :: FSUM( : )   ! total actinic flux

!***needed for stratospheric Raleigh optical depth
      REAL, PARAMETER :: R_G = 100.0 * RDGAS / GRAV  ! dry air gas constant
                                                     ! divided by gravitational
                                                     ! acceleration [cm/K] NOTE: cgs units

      REAL HSCALE               ! Scale height [cm] ! NOTE: cgs units

      REAL NBAR                 ! total number of air molecules [ # /cm**2 ]
                                ! above top of model domain


!***FSB Cloud properties.
!***  FSB These properties are taken fro HU & Stamnes,1993,
!***  An accurate parameterizationof the radiative properties of
!***  water clouds suitable for use in climate models, Journal of
!***  Climate, vol. 6, pp. 728-742. The values in the data statements
!***  were calculated with an equivalent radius of 10 micrometers.
!***  Note: Hu &Stamnes give beta in [ 1 / km/ for LWC in [ g / m**3 ]
!***  the values for beta/ LWC also give beta in [1/m] with LWC in [g/m **3]

      REAL G_CLOUD              ! local cloud asymmetry factor
      REAL OM_CLOUD             ! local cloud single scattering albedo
      REAL DTSCAT_CLOUD         ! level increment in cloud scattering optical
      REAL TAU_SCAT_CLD         ! total scattering optical depth of cloud
      REAL LAYERING_FACTOR      ! correction factor for cloud layering
      REAL STOZONE

      LOGICAL, SAVE :: FIRST = .TRUE.  ! Flag for first call
      LOGICAL       :: SUCCESS

!***arrays for fluxes and irradiances used in
      REAL, ALLOCATABLE, SAVE :: SRAYL( : )        ! Molecular scattering cross sections [ cm ** 2]
      REAL, ALLOCATABLE, SAVE :: TAU_SCAT( : )     ! aerosol scattering optical depth
      REAL, ALLOCATABLE, SAVE :: CONV_WM2( : )     ! conversion factor [photons/(cm**2 s )] to [Watts/m**2]

!***three-dimensional array for Cs and Qy
!***  (temperature, wavelength, species)
!***(layer, wavelength species)

      REAL, ALLOCATABLE, SAVE :: CSZ( :,:,: )
      REAL, ALLOCATABLE, SAVE :: QYZ( :,:,: )

      IF ( FIRST ) THEN

         ALLOCATE( CONV_WM2( NWL ) )
         ALLOCATE( SRAYL   ( NWL ) )
         ALLOCATE( TAU_SCAT( NWL ) )
         ALLOCATE( CSZ( NLAYS,NWL,NPHOTAB ) )
         ALLOCATE( QYZ( NLAYS,NWL,NPHOTAB ) )
         
         ALLOCATE( GAS_EXTINCTION( NLAYS,NWL ) )
         ALLOCATE( EXTINCTION    ( NLAYS,NWL ) )
         ALLOCATE( ACTINIC_FLUX  ( NLAYS,NWL ) )
         ALLOCATE( IRRADIANCE    ( NLAYS,NWL ) )
         
         ALLOCATE( DSDH_TD ( NLAYS+1 ),
     &             DSDH    ( NLAYS ),
     &             DTAU    ( NLAYS+1 ),
     &             DT_AERO ( NLAYS+1 ),
     &             DT_CLOUD( NLAYS+1 ),
     &             OM      ( NLAYS+1 ),
     &             G       ( NLAYS+1 ),
     &             FDIR    ( NLAYS+1 ),
     &             FUP     ( NLAYS+1 ),
     &             FDN     ( NLAYS+1 ),
     &             EDIR    ( NLAYS+1 ),
     &             EUP     ( NLAYS+1 ),
     &             EDN     ( NLAYS+1 ),
     &             ESUM    ( NLAYS ),
     &             FSUM    ( NLAYS ) )

!***FSB Set up conversion factor for
!***  [photons / ( cm**2 s) ] to [Watts / m**2 ]
!***  THE 1.0E13 FACTO IS 1.0E9 * 1.0 E4
!***  The 1.0e9 is for the wavelength [ nm ] -> [ m ]
!***  The 1.0e4 is for the area [ cm **2 ] -> [ m**2 ]

         DO IWL = 1, NWL
            LAMDA = WAVELENGTH( IWL )
            CONV_WM2( IWL ) = 1.0E13 * ( PLANCK_C * LIGHT_SPEED ) / LAMDA
         END DO

!         COS85 = COS( 85.0 * PI180 )

!***get molecular scattering cross sections

         CALL GETSRAY ( NWL, WAVELENGTH, SRAYL )

         FIRST = .FALSE.

      END IF   ! FIRSTIME

!***initialize BLKRJ and other layer variables

      BLKRJ          = 0.0
      GAS_EXTINCTION = 0.0
      EXTINCTION     = 0.0
      ACTINIC_FLUX   = 0.0
      IRRADIANCE     = 0.0
      REFLECTION     = 0.0
      TRANSMISSION   = 0.0
      TRANS_DIRECT   = 0.0
      INSOLATION     = 0.0
      TROPO_O3_TOGGLE  = 1.0
      STRATO3_MINS_MET = .TRUE.
!***Initialize sums or set default values for outputs: 
!   TAUC_AERO, TAU_TOT, TAUO3_TOP, TAU_RAY, SSA_AERO, etc.

      TAUC_AERO = 0.0
      TAU_TOT   = 0.0
      TAU_CLOUD = 0.0
      TAU_SCAT  = 0.0
      SSA_AERO  = 0.0
      TOTAL_TAU_CLD = 0.0
#ifdef phot_debug      
      AVE_SSA_CLD   = 0.0
      AVE_ASYMM_CLD = 0.0
#endif
!***Test zenith angle. If coszen is zero or negative, zenith angle is
!***  equal to or greater than 90 degrees, i.e. before sunrise or
!***  after sunset at the surface.
!***  Return  all photolysis rates set to zero. Ignore possible twilight
!***  processes in upper troposphere.

!***FSB NOTE: tests of the algorithm for slant path show that the
!***  critical zenith angle for the tropospheric slant path is 88 degrees,
!***  but the critical zenith angle for the stratospheric slant path is
!***  85 degrees.  Thus, the code returns zeros for angles greater then or
!***  equalt to 85 degrees. cos( 85 degrees ) equals 8.715574e-02.

      IF ( COSZEN .LE. COS85 ) THEN
         TAUO3_TOP = 0.0
         TAU_RAY   = 0.0
         TROPO_O3_COLUMN = 0.0
         TROPO_O3_TOGGLE = 1.0
!        BLKDZ           = 0.0
         CALL SLANTPATH2 ( NLAYS, BLKZF, ZSFC, REARTH, SINZEN, BLKDZ, DSDH )
         RETURN
      END IF

      IF ( NEW_PROFILE ) THEN  ! update based on new temperature and density profile
!***Adjust cross sections and quantum yields for ambient conditions

         CALL GET_CSQY ( BLKTA, BLKDENS, CSZ, QYZ )

!***calculate scale height from top of model domain

         HSCALE = R_G * BLKTA( NLAYS )

!***estimate air density at top of model domain

         DENSTOM = BLKDENS( NLAYS )
     &           * EXP( -100.0 * ( BLKZF( NLAYS + 1 ) - BLKZH( NLAYS ) )
     &           / HSCALE )

!***calculate the total number of air molecules [ # / cm**2 ]
!***  above top of model domain.

         NBAR = HSCALE * DENSTOM

!***set top of modeling domain

         ZTOM = BLKZF( NLAYS + 1 )

!***get layer thicknesses and slantpath starting at the TOP

!        BLKDZ = 0.0
         CALL SLANTPATH2 ( NLAYS, BLKZF, ZSFC, REARTH, SINZEN, BLKDZ, DSDH )

!***get slantpath from ZTOM to ZTOA

         CALL SLANTPATHTOP ( ZTOM, ZTOA, ZSFC, REARTH, SINZEN, DSDH_TOP )

C*** find ozone column density for atmosphere, stratosphere, and troposphere
         STRAT_O3_COLUMN = DU_TO_CONC * REAL( TOTAL_O3_COLUMN )
!         STRAT_O3_COLMIN = 0.10 * STRAT_O3_COLUMN
         STRAT_O3_COLMIN = MIN_STRATO3_FRAC * STRAT_O3_COLUMN
         SUCCESS = .TRUE.
         TROPO_O3_COLUMN = 0.0
         DO L = NLAYS, 1, -1
            DELTA_O3_COLUMN = 100.0 * BLKO3( L ) * BLKDZ( L )
            STRAT_O3_COLUMN = STRAT_O3_COLUMN - DELTA_O3_COLUMN
            TROPO_O3_COLUMN = TROPO_O3_COLUMN + DELTA_O3_COLUMN
            IF ( STRAT_O3_COLUMN .LT. STRAT_O3_COLMIN  .AND. SUCCESS ) THEN
               IF( OBEY_STRATO3_MINS )THEN
                   WRITE( LOGDEV,'( /A, F5.2, A, 3(/A), I3, A,  F8.3, A , 2(I4,1X) )' )
     &             'PHOT WARNING: First Occurance where computed stratospheric O3 column < ',
     &              100.0*MIN_STRATO3_FRAC,'%',
     &             'observed total column. The percentage is a global minimum based on ',
     &             'climatological ozone profiles. ',
     &             'The Error accumulates downward from layer = ', L, ' or alt= ', 
     &              0.001*BLKZF( L ),' Km for col,row = ', PHOT_COL, PHOT_ROW
               END IF
               SUCCESS = .FALSE.
            END IF
         END DO

         STRAT_O3_COLUMN = CONC_TO_DU * STRAT_O3_COLUMN
         TROPO_O3_COLUMN = CONC_TO_DU * TROPO_O3_COLUMN

#ifdef verbose_PHOT_MOD
         IF( PHOT_COL .EQ. 1 .AND. PHOT_ROW .EQ. 1 )THEN
              WRITE( LOGDEV,*)'TOTAL_O3_COLUMN, TROPO_O3_COLUMN = ',TOTAL_O3_COLUMN, TROPO_O3_COLUMN
         END IF
#endif

         IF ( .NOT. SUCCESS ) THEN
            TROPO_O3_TOGGLE   = MAX_TROPOO3_FRAC * TOTAL_O3_COLUMN
     &                        / TROPO_O3_COLUMN
            N_TROPO_O3_TOGGLE = N_TROPO_O3_TOGGLE + 1 
            O3_TOGGLE_AVE     = O3_TOGGLE_AVE + TROPO_O3_TOGGLE
            O3_TOGGLE_MIN     = MIN( O3_TOGGLE_MIN, TROPO_O3_TOGGLE)
            STRATO3_MINS_MET  = .FALSE. 
            STRAT_O3_COLUMN = CONC_TO_DU * STRAT_O3_COLMIN
           IF( OBEY_STRATO3_MINS )THEN ! write to PE log for first occurance
               WRITE( LOGDEV, 99983)STRAT_O3_COLUMN
               IF( ADJUST_OZONE ) WRITE( LOGDEV, 99984)TROPO_O3_TOGGLE
               WRITE( LOGDEV, 99887)
               WRITE( LOGDEV, 99888)TOTAL_O3_COLUMN, TROPO_O3_COLUMN, MAX_TROPOO3_FRAC
               WRITE( LOGDEV, 99999)
               OBEY_STRATO3_MINS = .FALSE.
            END IF
            IF( .NOT. ADJUST_OZONE ) TROPO_O3_TOGGLE = 1.0 ! reset toggle to one 
         ELSE
            TROPO_O3_TOGGLE = 1.0
         END IF


99983    FORMAT( 'Corrective Action: 1) Stratospheric O3 column set to ',F8.3,' DU' ) 
99984    FORMAT( 'and 2) Extinction from Model Domain O3 multiplied by ',F9.6 )
99887    FORMAT(/'Check TROPO_O3_EXCEED and N_EXCEED_TROPO3 in PHOTDIAG1 file for '
     &          /'values greater than zero to assess the extent of the '
     &          /'problem. TROPO_O3_EXCEED and N_EXCEED_TROPO3 are the average '
     &          /'exceedance and their number over file time step for each grid cell,'
     &          /'respectively. Exceedance depends on the predicted tropospheric'
     &          /'fraction over the maximum allowed fraction of the total ozone column.'
     &          /'Its value equals the ratio minus one if ratio is greater than one and'
     &          /'zero if the ratio is less than or equal to one. N_EXCEED_TROPO3 '
     &          /'counts the number of nonzero values per timestep')
99888    FORMAT(/'Direct Cause: Predicted O3 tropospheric Column exceeds maximum allowed '
     &          /'fraction of total OMI column.',
     &          /'OMI Total O3 Column = ',F8.3,' DU: Model Tropospheric O3 Column = ',F8.3,' DU',
     &          /'Climatological Expected Tropospheric Fraction = ',F9.6)
99999    FORMAT(/'ULTIMATE causes include boundary condition and meteorological input files. '
     &          /'Check the former for unrealistic concentrations of ozone and its precursors.'
     &          /'Check the latter for unrealistic advection and diffusion parameters.')           
         
         DO IWL = 1, NWL
!***Get optical depth for stratospheric ozone column
!***Note that stratosphere ozone coluumn assumed to exist above model domain
            CALL GET_TAUO3 ( IWL, STRAT_O3_COLUMN, STRAT_TEMP, TAUO3_TOP( IWL ) )
!***get Rayleigh optical depth for stratosphere
            TAU_RAY( IWL ) = NBAR * SRAYL( IWL )         
         END DO
      END IF ! for NEW_PROFILE

!***loop over wavelengths
      DO IWL = 1, NWL           ! outermost loop

!        RSFC       = ALB( IWL )        ! surface albedo

!***set scaling factor for reducing extraterrestrial flux
!***  add ozone and Rayleigh optical depths. Use the
!***  pseudospherical correction for the stratosphere.

         SOLAR_FLUX = FEXT( IWL ) / RSQD

!*** initialize tau, delta tau's, other variables and loop over layers

         DTAU         = 0.0
         DT_AERO      = 0.0
         DT_CLOUD     = 0.0
         DTSCAT_CLOUD = 0.0
         TAU_SCAT_CLD = 0.0
         
         DO L = 2, NLAYS + 1
            II = NLAYS + 2 - L  ! from top to bottom

!***in the following statements the factor of 100.0 converts
!***  converts [ 1 / cm ] to [ 1 / m ]

            BETA_M = SRAYL( IWL )         * BLKDENS( II ) * 100.0
            AO3    = CSZ( II,IWL,LO3O3P ) * BLKO3  ( II ) * 100.0
            AO3    = TROPO_O3_TOGGLE      * AO3
            ANO2   = CSZ( II,IWL,LNO2   ) * BLKNO2 ( II ) * 100.0

!***set up aerosol optical properties

            G_BAR = AERO_ASYM_FAC ( II,IWL )
            BEXT  = AERO_EXTI_COEF( II,IWL )
            BSCAT = AERO_SCAT_COEF( II,IWL )

!***calculate total absorption and scattering contributions
!***to optical depth

!***The contributions to scattering and absorption from molecules and particles
!***  are calculated separately to facilitate the calculation
!***  of the total single scatering albedo of the column of aerosols
!***  as measured by satellites.

            DTSCAT_M = BETA_M * BLKDZ( II ) ! molecular scattering
            DTSCAT_A = BSCAT  * BLKDZ( II ) ! particle scattering

            DTSCAT_M = MAX( DTSCAT_M, SMALL )
            DTSCAT_A = MAX( DTSCAT_A, SMALL )


            DTABS_M  = ( AO3 + ANO2  )  * BLKDZ( II ) ! molecular absorption
            DTABS_A  = ( BEXT - BSCAT ) * BLKDZ( II ) ! particle absorption

            DTABS_M  = MAX( DTABS_M, SMALL )
            DTABS_A  = MAX( DTABS_A, SMALL )

            GAS_EXTINCTION( II,IWL ) = BETA_M + AO3 + ANO2        ! gas extinction
            EXTINCTION( II,IWL )     = BETA_M + BEXT + AO3 + ANO2 ! gas and aerosol extinction
            
            IF ( CLOUDS( II ) ) THEN 
     
               DT_CLOUD( L ) = ( CLOUD_LIQUID_EXT( II,IWL ) 
     &                       +   CLOUD_ICE_EXT( II,IWL )
     &                       +   CLOUD_AGGREG_EXT( II,IWL ) ) * BLKDZ( II )
               DTSCAT_CLOUD  = ( CLOUD_LIQUID_SCAT( II,IWL )
     &                       +   CLOUD_ICE_SCAT( II,IWL )
     &                       +   CLOUD_AGGREG_SCAT( II,IWL ) ) * BLKDZ( II )

!Adjust DT_CLOUD for cloud fraction by 1/2 power of CLDFRC to approximate cloud overlap.
!Note that the power results because the resolved cloud conentrations are averaged over
!the grid cell so the net overlap correction equal cfrac**(3/2) from Briegleb (1992) times
!cfrac**(-1) for actual in-cloud concentrations (see Voulgarakis et al., 2009, Geosci Model
!Dev., vol. 2, pp. 59-72.

               IF ( CLOUD_LAYERING( II ) ) THEN
                  LAYERING_FACTOR = SQRT( CLDFRC( II ) )
               ELSE
                  LAYERING_FACTOR = CLDFRC( II )
               END IF
               DT_CLOUD( L ) = DT_CLOUD( L ) * LAYERING_FACTOR
               DTSCAT_CLOUD  = DTSCAT_CLOUD  * LAYERING_FACTOR
 
               EXTINCTION( II,IWL ) = EXTINCTION( II,IWL )
     &                              + ( CLOUD_LIQUID_EXT( II,IWL ) 
     &                              +   CLOUD_ICE_EXT( II,IWL )
     &                              +   CLOUD_AGGREG_EXT( II,IWL ) ) * LAYERING_FACTOR
    
               TAU_SCAT_CLD = TAU_SCAT_CLD + DTSCAT_CLOUD

               IF ( DT_CLOUD( L ) .GT. 1.0E-6 ) THEN
                  OM_CLOUD = MAX( 0.000001, MIN( DTSCAT_CLOUD /DT_CLOUD( L ), 0.99999) )
                  IF ( OM_CLOUD .LT. 0.0 .OR. OM_CLOUD .GT. 1.0 .OR. OM_CLOUD .NE. OM_CLOUD) THEN
                     WRITE( LOGDEV,'(A,I3,A,ES12.4,A)',ADVANCE = 'NO')
     &               'OM_CLOUD( L = ', L, ' ) = ', OM_CLOUD,' resetting to '
                     OM_CLOUD = MAX( 0.000001, MIN( OM_CLOUD, 0.99999))
                     WRITE( LOGDEV,'(ES12.4)')OM_CLOUD
                     WRITE( LOGDEV,'(A,2(ES12.4,1X))')'DT_CLOUD, DTSCAT_CLOUD = ',
     &               DT_CLOUD, DTSCAT_CLOUD
                     WRITE( LOGDEV,'(A,2(ES12.4,1X))')'LIQUID_EXT, LIQUID_SCAT = ',
     &               CLOUD_LIQUID_EXT( II,IWL ), CLOUD_LIQUID_SCAT( II,IWL )
                     WRITE( LOGDEV,'(A,2(ES12.4,1X))')'ICE_EXT, ICE_SCAT = ',
     &               CLOUD_ICE_EXT( II,IWL ), CLOUD_ICE_SCAT( II,IWL )
                     WRITE( LOGDEV,'(A,2(ES12.4,1X))')'AGGREG_EXT, AGGREG_SCAT = ',
     &               CLOUD_AGGREG_EXT( II,IWL ), CLOUD_AGGREG_SCAT( II,IWL )
                     CALL M3EXIT( 'NEW_OPTICS', JDATE, JTIME, ' ', XSTAT1 )
                  END IF
               ELSE 
                  OM_CLOUD = 1.0
               END IF


               IF ( DTSCAT_CLOUD .GT. 1.0E-6 ) THEN
               
                  G_CLOUD = ( (CLOUD_LIQUID_ASY( II,IWL ) * CLOUD_LIQUID_SCAT( II,IWL ))
     &                    +   (CLOUD_ICE_ASY( II,IWL )    * CLOUD_ICE_SCAT( II,IWL ))
     &                    +   (CLOUD_AGGREG_ASY( II,IWL ) * CLOUD_AGGREG_SCAT( II,IWL )) )
     &                    *   BLKDZ( II ) * LAYERING_FACTOR


#ifdef phot_debug
                  IF ( .NOT. ONLY_SOLVE_RAD ) THEN
                     AVE_ASYMM_CLD( IWL ) = AVE_ASYMM_CLD( IWL ) + G_CLOUD
                     IF ( AVE_ASYMM_CLD( IWL ) .GT. TAU_SCAT_CLD ) THEN
                        WRITE( LOGDEV,'(A,I3,2(A,ES12.4))' )
     &                  'Sum for AVE_ASYMM_CLD at L (', L,') = ', AVE_ASYMM_CLD( IWL ),
     &                  ' Sum for TAU_SCAT_CLD = ',TAU_SCAT_CLD 
                        WRITE( LOGDEV,'(A,2(ES12.4,1X))' )
     &                  'AVE_ASYMM_CLD Increment = ', G_CLOUD
                        WRITE( LOGDEV,'(A,2(ES12.4,1X))' )
     &                  'TAU_SCAT_CLD Increment = ', DTSCAT_CLOUD
                     END IF
                  END IF
#endif                   

                  G_CLOUD  = G_CLOUD / DTSCAT_CLOUD

                  IF ( G_CLOUD .GE. 1.0 .OR. G_CLOUD .LE. -1.0 .OR. G_CLOUD .NE. G_CLOUD ) THEN
                     WRITE( LOGDEV,'(A,I3,A,ES12.4,A)',ADVANCE = 'NO' )
     &                  'G_CLOUD( L = ', L, ' ) = ', G_CLOUD,' resetting to '
                        G_CLOUD = MIN( 0.9999, MAX( G_CLOUD, -0.9999) )
                     WRITE( LOGDEV,'(ES12.4)') G_CLOUD
                     WRITE( LOGDEV,'(A,2(ES12.4,1X))' )
     &                  'LIQUID_ASY, LIQUID_SCAT = ',
     &                  CLOUD_LIQUID_ASY( II,IWL ), CLOUD_LIQUID_SCAT( II,IWL )
                     WRITE( LOGDEV,'(A,2(ES12.4,1X))' )
     &                  'ICE_ASY, ICE_SCAT = ',
     &                  CLOUD_ICE_ASY( II,IWL ), CLOUD_ICE_SCAT( II,IWL )
                     WRITE( LOGDEV,'(A,2(ES12.4,1X))' )
     &                  'AGGREG_ASY, AGGREG_SCAT = ',
     &                  CLOUD_AGGREG_ASY( II,IWL ), CLOUD_AGGREG_SCAT( II,IWL )
                     CALL M3EXIT( 'NEW_OPTICS', JDATE, JTIME, ' ', XSTAT1 )
                  END IF
               ELSE
                  G_CLOUD = 0.0
               END IF            
            ELSE
               DTSCAT_CLOUD = 0.0
               G_CLOUD      = 0.0
               OM_CLOUD     = 1.0
            END IF

!***calculate total absorption and scattering contributions
!***to optical depth

            DTSCAT = DTSCAT_M + DTSCAT_A + DTSCAT_CLOUD
            DTABS  = DTABS_M  + DTABS_A + MAX(( 1.0 - OM_CLOUD ), 0.0) * DT_CLOUD( L )

!***set aerosol optical depth for later use

            DT_AERO ( L ) = BEXT * BLKDZ( II )

!***Now calculate the vertical profiles of optical depth,
!***  single scattering albedo, asymmetry factor
!***  and DSDH starting at the top.

            DTAU( L ) = DTSCAT + DTABS
            OM  ( L ) = DTSCAT / ( DTSCAT + DTABS )
            G   ( L ) = ( G_BAR * DTSCAT_A + G_CLOUD * DTSCAT_CLOUD ) / DTSCAT

            IF ( G( L ) .GE. 1.0 .OR. G( L ) .LE. -1.0 .OR. G( L ) .NE. G( L ) ) THEN
               WRITE( LOGDEV,'(A,ES12.4,A)',ADVANCE = 'NO' )
     &         'G( L ) = ', G( L ),' resetting to '
               G( L ) = MIN( 0.9999, MAX( G( L ), -0.9999) )
               WRITE( LOGDEV,'(ES12.4)')G( L )
               WRITE( LOGDEV,'(A,10(1X,ES12.4))' )
     &         'DTSCAT_M, DTSCAT_A, DTSCAT_CLOUD, G_BAR, G_CLOUD = ',
     &          DTSCAT_M, DTSCAT_A, DTSCAT_CLOUD, G_BAR, G_CLOUD
            END IF

            IF ( OM( L ) .GT. 1.0 .OR. OM( L ) .LE. 0.0 .OR. OM( L ) .NE. OM( L ) ) THEN
               WRITE( LOGDEV,'(A,ES12.4,A)',ADVANCE = 'NO' )
     &         'OM( L ) = ', OM( L ),' resetting to '
               OM( L ) = MIN( 0.9999, MAX( OM( L ), 0.0001) )
#ifdef phot_debug
               WRITE( LOGDEV,'(ES12.4)' ) OM( L )
               WRITE( LOGDEV,'(A,4(1X,ES12.4))' )
     &         'DTSCAT, DTABS, ( DTSCAT + DTABS) = ',
     &         DTSCAT, DTABS, ( DTSCAT + DTABS )
               WRITE( LOGDEV,'(A,4(1X,ES12.4))' )
     &         'DTSCAT_M, DTSCAT_A, DTSCAT_CLOUD = ',
     &         DTSCAT_M, DTSCAT_A, DTSCAT_CLOUD
               WRITE( LOGDEV,'(A,4(1X,ES12.4))' )
     &         'DDTABS_M, DTABS_A, MAX(( 1.0-OM_CLOUD ), 0.0) * DT_CLOUD( L ) = ',
     &         DTABS_M, DTABS_A, MAX(( 1.0 - OM_CLOUD ), 0.0) * DT_CLOUD( L )
               WRITE( LOGDEV,'(A,4(1X,ES12.4))' )
     &         ' AO3, ANO2,AERO_BEXT, AERO_BSCAT = ',
     &         AO3, ANO2,BEXT, BSCAT
#endif
            ELSE
#ifdef phot_debug
               IF ( OM( L ) .EQ. 1.0 ) THEN
                  WRITE( LOGDEV,'(A,4(1X,ES12.4))' )
     &            'DTSCAT, DTABS, ( DTSCAT + DTABS ) = ',
     &             DTSCAT, DTABS, (DTSCAT + DTABS)
                  WRITE( LOGDEV,'(A,4(1X,ES12.4))' )
     &            'DTSCAT_M, DTSCAT_A, DTSCAT_CLOUD = ',
     &             DTSCAT_M, DTSCAT_A, DTSCAT_CLOUD
                  WRITE( LOGDEV,'(A,4(1X,ES12.4))' )
     &            'DDTABS_M, DTABS_A, MAX(( 1.0-OM_CLOUD ), 0.0) * DT_CLOUD( L ) = ',
     &            DTABS_M, DTABS_A, MAX(( 1.0 - OM_CLOUD ), 0.0 ) * DT_CLOUD( L)
                  WRITE( LOGDEV,'(A,4(1X,ES12.4))' )
     &            'AO3, ANO2,AERO_BEXT, AERO_BSCAT = ',
     &            AO3, ANO2,BEXT, BSCAT
               END IF
#endif
               OM( L ) = MIN( 0.9999,  OM( L ) )
            END IF

            DSDH_TD( L ) = DSDH( L - 1 )

            IF ( ONLY_SOLVE_RAD ) CYCLE
!***FSB get sums of unscaled optical depths

            TAU_SCAT( IWL ) = TAU_SCAT ( IWL ) + DTSCAT_A

!***initialize optical depth profiles to the layer increment
     
            TAUC_AERO( II,IWL ) = DT_AERO( L )  ! aerosol optical depth
            TAU_TOT  ( II,IWL ) = DTAU( L )     ! total optical depth
            TAU_CLOUD( II,IWL ) = DT_CLOUD( L ) ! cloud optical depth
!***change extinction units from meters to Megameters
            EXTINCTION( II,IWL ) = 1000.0 * EXTINCTION( II,IWL )

         END DO                 ! loop over layers

!***set values for the stratosphere

         OM     ( 1 ) = TAU_RAY( IWL ) / ( TAU_RAY( IWL ) + TAUO3_TOP( IWL ) )
         G      ( 1 ) = 0.05
         DTAU   ( 1 ) = TAUO3_TOP( IWL ) + TAU_RAY( IWL )
         DSDH_TD( 1 ) = DSDH_TOP

         NLEVEL = NLAYS + 1

         IF ( .NOT. ONLY_SOLVE_RAD ) THEN
!***calculate optical depth profiles
            TAU_TOT  ( NLAYS,IWL ) = TAU_TOT  ( NLAYS,IWL ) + DTAU( 1 )
            TAUC_AERO( NLAYS,IWL ) = TAUC_AERO( NLAYS,IWL ) + DT_AERO( 1 )  
            TAU_CLOUD( NLAYS,IWL ) = TAU_CLOUD( NLAYS,IWL ) + DT_CLOUD( 1 ) 

            DO L =  NLAYS-1, 1, -1
               TAU_TOT  ( L,IWL ) = TAU_TOT  ( L,IWL ) + TAU_TOT  ( L+1,IWL )
               TAUC_AERO( L,IWL ) = TAUC_AERO( L,IWL ) + TAUC_AERO( L+1,IWL ) 
               TAU_CLOUD( L,IWL ) = TAU_CLOUD( L,IWL ) + TAU_CLOUD( L+1,IWL )
            END DO
         END IF
         
!***Set fluxes to zero

         FDIR = 0.0
         FUP  = 0.0
         FDN  = 0.0
         EDIR = 0.0
         EUP  = 0.0
         EDN  = 0.0

!***calculate fluxes and irradiances
         CALL TWOSTREAM_S ( NLEVEL, COSZEN, ALB( IWL ), DTAU, OM, G, DSDH_TD,
     &                      FDIR, FUP, FDN, EDIR, EUP, EDN )

         DO L = 1, NLAYS
            II = NLAYS + 2 - L
            FSUM( L ) = FDIR( II ) + FDN( II ) + FUP( II ) ! actinic flux
            ESUM( L ) = EDIR( II ) + EDN( II )             ! downward irradiance
         END DO                 ! loop over layers

! add diffusion and direct components for calculating reflectivity and transmissivity
         INSOLATION    = INSOLATION    + SOLAR_FLUX 
         REFLECTION    = REFLECTION    + SOLAR_FLUX * EUP( 1 )
         TRANSMISSION  = TRANSMISSION  + SOLAR_FLUX * EDN( NLEVEL )
         TRANS_DIRECT  = TRANS_DIRECT  + SOLAR_FLUX * EDIR( NLEVEL ) 

         IF ( ONLY_SOLVE_RAD ) CYCLE

!***FSB Calculate column averaged scattering albedo and asymmetry factor

         IF ( TAUC_AERO( 1,IWL ) .GT. 1.0E-30 ) THEN
            SSA_AERO( IWL ) = TAU_SCAT( IWL ) / TAUC_AERO( 1,IWL )
         END IF

         TOTAL_TAU_CLD( IWL ) = TAU_CLOUD( 1,IWL )

#ifdef phot_debug
         IF ( TAU_CLOUD( 1,IWL ) .GT. 1.0E-20 ) THEN
            IF ( AVE_ASYMM_CLD( IWL ) .GT. TAU_SCAT_CLD ) THEN
               WRITE( LOGDEV,'(A,I3,2(A,ES12.4))' )
     &         'Sum for AVE_ASYMM_CLD at L(', 1,') = ', AVE_ASYMM_CLD( IWL ),
     &         'Sum for TAU_SCAT_CLD = ',TAU_SCAT_CLD 
               WRITE( LOGDEV,'(A,2(ES12.4,1X))' )
     &         'AVE_ASYMM_CLD Increment = ', G_CLOUD
               WRITE( LOGDEV,'(A,2(ES12.4,1X))' )
     &         'TAU_SCAT_CLD Increment = ',
     &         DTSCAT_CLOUD
            END IF
            IF ( TAU_SCAT_CLD .GT. 1.0E-20 ) THEN
               AVE_ASYMM_CLD( IWL ) = AVE_ASYMM_CLD( IWL ) / TAU_SCAT_CLD
               AVE_SSA_CLD  ( IWL ) = TAU_SCAT_CLD / TAU_CLOUD( 1,IWL )
            ELSE
               AVE_ASYMM_CLD( IWL ) = 0.0
               AVE_SSA_CLD  ( IWL ) = 0.0
            END IF
            IF ( ABS( AVE_ASYMM_CLD( IWL ) ) .GE.  1.0 ) THEN
               WRITE( LOGDEV,'(A,I3,2(A,ES12.4))' )
     &         'Sum for AVE_ASYMM_CLD at L(', 1,') = ', AVE_ASYMM_CLD( IWL )*TAU_SCAT_CLD,
     &         'Sum for TAU_SCAT_CLD = ',TAU_SCAT_CLD 
               WRITE( LOGDEV,'(A,2(ES12.4,1X))' )
     &         'AVE_ASYMM_CLD Increment = ', G_CLOUD
               WRITE( LOGDEV,'(A,2(ES12.4,1X))' )
     &         'TAU_SCAT_CLD Increment = ', DTSCAT_CLOUD
            END IF
         ELSE
            TOTAL_TAU_CLD( IWL ) = 0.0
            AVE_SSA_CLD  ( IWL ) = 0.0
            AVE_ASYMM_CLD( IWL ) = 0.0
         END IF
#endif
         
!***FSB capture the total downward irradiance at the surface [ W / m**2]
!
!        ETOT_SFC( IWL ) = CONV_WM2( IWL ) * FLXSCALE * FEXT( IWL )
!     &                  * ESUM( 1 )

         FORALL( L = 1:NLAYS )
!***multiply by the solar flux at the domain top for 
!***actinic flux and irradiance; keeping actinic flux in photons/(cm^2*s)
            ACTINIC_FLUX( L,IWL ) = SOLAR_FLUX * FSUM( L )
            IRRADIANCE  ( L,IWL ) = SOLAR_FLUX * CONV_WM2( IWL ) * ESUM( L )
         END FORALL
      END DO    ! loop over wavelengths

! normalize reflection and transmission coefficients 
      INSOLATION   = 1.0 / ( COSZEN * INSOLATION )
      TRANS_DIRECT = TRANS_DIRECT * INSOLATION
      REFLECTION   = ONE_OVER_PI * REFLECTION * INSOLATION 
      TRANSMISSION = ONE_OVER_PI * TRANSMISSION * INSOLATION

      IF ( ONLY_SOLVE_RAD ) RETURN 
               
! compute photolysis rates
      DO IPHOT = 1, NPHOTAB
         DO IWL = 1, NWL
            DO L = 1, NLAYS 
               BLKRJ( L,IPHOT ) = BLKRJ( L,IPHOT ) 
     &                          + ACTINIC_FLUX( L,IWL ) 
     &                          * CSZ( L,IWL,IPHOT ) * QYZ( L,IWL,IPHOT ) ! [ 1 / sec ]
            END DO
         END DO
      END DO  ! loop on layers, wavelength,  IPHOT
! convert actinic flux to watts/m^2    
      FORALL( L = 1:NLAYS, IWL=1:NWL  )
         ACTINIC_FLUX( L,IWL ) = ACTINIC_FLUX( L,IWL ) * CONV_WM2( IWL )
      END FORALL

!***compute rate of photolysis (j-values) for each reaction        

9503  FORMAT('LAYER = ',I3,' MODE = ',I3,' LAMBDA(nm)  = ',ES12.4,' DGN_CORE(m) = ',ES12.4,
     &       ' DGN_SHELL(m) = ', ES12.4 / ' REFRACT_IDX_SHELL(NR,NI) = ', 2(ES12.4,1X),
     &       ' REFRACT_IDX_CORE(NR,NI) = ', 2(ES12.4,1X) / ' LN(GEO.STD.DEV.) = ',
     &       ES12.4)
9504  FORMAT('LAYER = ',I3,' MODE = ',I3,' LAMBDA(nm)  = ',ES12.4,' DGN(m) = ',ES12.4,
     &       ' REFRACT_IDX(NR,NI) = ', 2(ES12.4,1X) / ' VOL.DENS. = ', ES12.4,
     &       ' LN(GEO.STD.DEV.) = ', ES12.4)

99985 FORMAT('ERROR: Modeled Troposheric Ozone Column downward from layer ',I3,1X)
99986 FORMAT('exceeds Top Ozone Column based on OMI.data file. Negative Optical Depths ')
99987 FORMAT('but are physically unlikey.')
99988 FORMAT(' SETTING O3 Column ABOVE PTOP TO 25% of OMI.dat value  ')
99989 FORMAT(' FOR ROW/COL = ',2(1X,I4))

      RETURN
      END SUBROUTINE NEW_OPTICS

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE GETSRAY ( NWL, LAMDA, SRAYL )
C-----------------------------------------------------------------------
C  calculate molecular (Rayleigh) scattering cross section, srayl
C
C  coded 09/08/2004 by Dr. Francis S. Binkowski
C     Carolina Environmental Program
C     University of North Carolina at Chapel Hill
C     email: frank_binkowski@unc.edu
C
C  Reference:
C     Nicolet, M., On the molecular scattering in the terrestrial
C     atmosphere: An empirical formula for its calculation in the
C     homoshpere, Planetary and Space Science. Vol. 32,No. 11,
C     Pages 1467-1468, November 1984.
C-----------------------------------------------------------------------

      IMPLICIT NONE

!***arguments

      INTEGER, INTENT( IN )  :: NWL           ! number of wavelength bins
      REAL,    INTENT( IN )  :: LAMDA( : )  ! wavelengths  [nm]
      REAL,    INTENT( OUT ) :: SRAYL( : )  ! molecular scattering cross sections [cm**2]

!***Internal variables

      INTEGER I
      REAL    WMICRN               ! wavelenght in micrometers
      REAL    WMICRN1              ! 1 / wmicrn
      REAL    XX                   ! variable in Nicolet method

!***get molecular scattering cross section. This is a fixed
!***  function of wavelength.

      DO I = 1, NWL
         WMICRN = 1.0E-3 * LAMDA( I ) ! wavelength in micrometers
         WMICRN1 = 1.0 / WMICRN

         IF ( WMICRN .LE. 0.55 ) THEN
            XX = 3.6772 + 0.389 * WMICRN + 0.09426 * WMICRN1
         ELSE
            XX = 4.04
         END IF

         SRAYL( I ) = 4.02E-28 * WMICRN1**XX    ! in [cm**2]

      END DO

      RETURN
      END SUBROUTINE GETSRAY


      SUBROUTINE GET_TAUO3 ( IWL, STOZONE, STRAT_TEMP, TAU_O3 )
C-----------------------------------------------------------------------
C  subroutine to calculate the optical depth of ozone in the
C     stratosphere
C
C  special cross sections for calculating stratospheric ozone
C     optical depth
C
C  the following temperatures and cross sections are from
C     Fast-J
C     REFERENCE:
C     Wild, O., X. Zhu, and M.J. Prather, Fast-J: Accurate simulation
C     of in- and below-clolud photolysis in tropospheric chemical
C     models,
C     Journal of Atmospheric Chemistry, Vol. 37, pp 245-282, 2000
C
C  coded 10/20/2004 by Dr. Francis S. Binkowski
C     Carolina Environmental Program
C     University of North Carolina at Chapel Hill
C     email: frank_binkowski@unc.edu
C     Updated to Fast-JX version 5.0
C  Mar 2011 Bill Hutzell
C     revised interpolation method for a general number of
C     interpolation points
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

!***arguments

      INTEGER, INTENT( IN )  :: IWL         ! wavelenth index

      REAL,    INTENT( IN )  :: STOZONE     ! ozone column amount [ DU ]
      REAL,    INTENT( IN )  :: STRAT_TEMP  ! average temperature for stratosphere [ K ]
      REAL,    INTENT( OUT ) :: TAU_O3      ! optical depth for statosphere

!***Local

      INTEGER IXT, IXTEMP

      REAL OZONE_CS        ! interpolated ozone absorption cross section
      REAL YTT             ! interpolation variable

!***Find temperature range:

      IF ( STRAT_TEMP .LE. TEMP_O3_STRAT( 1 ) ) IXTEMP = 0

      DO IXT = 1, NTEMP_STRAT - 1
         IF ( STRAT_TEMP .GT. TEMP_O3_STRAT( IXT ) .AND.
     &        STRAT_TEMP .LT. TEMP_O3_STRAT( IXT + 1 ) ) THEN
            IXTEMP = IXT
            YTT = ( STRAT_TEMP - TEMP_O3_STRAT( IXT ) )
     &          / ( TEMP_O3_STRAT( IXT + 1 ) - TEMP_O3_STRAT( IXT ) )
         END IF
      END DO

      IF ( STRAT_TEMP .GE. TEMP_O3_STRAT( NTEMP_STRAT ) ) THEN
         IXTEMP = NTEMP_STRAT
         YTT = 0.0
      END IF

!***do linear interpolation

      IF ( IXTEMP .EQ. 0 ) THEN
         OZONE_CS = XO3CS( 1, IWL )
      ELSE IF ( IXTEMP .GE. 1 .AND. IXTEMP .LT. NTEMP_STRAT ) THEN
         OZONE_CS = XO3CS( IXTEMP, IWL ) +
     &            ( XO3CS( IXTEMP+1, IWL ) - XO3CS( IXTEMP, IWL ) ) * YTT
      ELSE IF ( IXTEMP .EQ. NTEMP_STRAT ) THEN
         OZONE_CS = XO3CS( IXTEMP, IWL )
      END IF

      TAU_O3 = DU_TO_CONC * STOZONE * OZONE_CS

      RETURN
      END SUBROUTINE GET_TAUO3

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE O3AMT ( XLAT, XLONG, MDAY, OZONE )
C-----------------------------------------------------------------------
C  This subroutine implements an algorithm for the annual behavior
C     of total ozone ( taken here to be stratospheric) from
C     climatology
C  Reference:
C     Van Heuklon, Thomas K., Estimating atmospheric ozone for solar
C     radiation models, Solar Energy, Vol. 22, pp 63-68, 1979.
C  updated from an earlier version by
C     Dr. Francis S. Binkowski, The Carolina Environmental Program,
C     The University of North Carolina at Chapel Hill.
C     Email: frank_binkowski@unc.edu
C     November 03. 2004.
C  Only Northern Hemisphere is implemented.
C-----------------------------------------------------------------------

      IMPLICIT NONE

!***arguments

      INTEGER, INTENT( IN )  :: MDAY  ! Day number during the year
                                      ! Jan 1st = 1.0, Feb 1st = 32, etc.

      REAL,    INTENT( IN )  :: XLAT  ! latitude of point on earth's surface
      REAL,    INTENT( IN )  :: XLONG ! longitude of point on earth's surface
      REAL,    INTENT( OUT ) :: OZONE ! Total column amount of ozone [ DU ]

!***Internal:

!***The following parameters are from Table 1 of Van Heuklon (1979).

      REAL, SAVE ::  A, B, C, D, F, G, H, FJ
      DATA A/150.0/, B/1.28/, C/40.0/, D/0.9865/, F/-30.0/, G/20.0/,
     &     H/3.0/, FJ/235.0/

!***FSB FJ is the equatorial annual average of atmospheric ozone
!***  content, as noted on page 65 of Nav Heulklon (1979). This value
!***  sets the basic background for ozone.

      REAL, PARAMETER :: RD = 0.017453   ! degrees to radians

!***Variables of convenience

      REAL E, FI, BPHI, DEF, HLI, SINB, SINB2

!***set the day

      E = FLOAT( MDAY )
      FI = 20.0
      IF ( XLONG .LT. 0.0 ) FI = 0.0
      BPHI  = B * XLAT * RD
      DEF   = D * ( E + F ) * RD
      HLI   = H * ( XLONG + FI ) * RD
      SINB  = SIN( BPHI )
      SINB2 = SINB * SINB

!***the following equation implements equation (4) of VanHeuklon (1979)

      OZONE  = FJ + ( A + C * SIN( DEF ) + G * SIN( HLI ) ) * SINB2

      RETURN
      END SUBROUTINE O3AMT

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE SLANTPATH2 ( NLAYS, Z, ZSFC, REARTH, SINZEN, DZ, DSDH )
C-----------------------------------------------------------------------
C  PURPOSE:
C     Calculate slant path, ds/dh, over vertical depth in spherical
C     geometry also calculates the layer thicknesses.
C     NOTE!!!
C     This version is restricted to zenith angle less than 90 degrees
C-----------------------------------------------------------------------
C  ARGUMENTS:
C     INPUT:
C       NLAYS   - INTEGER, number of specified altitude levels
C       z       - REAL, altitude (agl) [m] <<<    meters
C       This is from file ZF ( full layers ) from METCRO3D
C       Z(1) is zero.
C       zsfc    - REAL, ground elevation (msl) [m]
C       rearth  - REAL, radius of the earth [m]
C       sinzen  - REAL, sine of solar zenith angle
C
C     OUTPUT:
C       dz      - REAL, layer thicknesses [ m ]
C       dsdh    - REAL, slant path of direct beam through each layer
C       when travelling from the top of the atmosphere downward
C-----------------------------------------------------------------------
C  EDIT HISTORY:
C     Inspired by sphers from TUV
C     09/08/2004 modified to specialize for CMAQ application
C     by Dr. Francis S. Binkowski
C     Environmental Modeling for Policy Development group,
C     The Carolina Environmental Program
C     The University of North Carolina-Chapel Hill
C     Email: frank_binkowski@unc.edu
C
C-----------------------------------------------------------------------
C  REFERENCE:
C     Dahlback, A. and K. Stamnes, A new spherical model for computing
C     the radiation field available for photolysis and heating at
C     twilight, Planetary and Space Sciences, Vol. 39, No. 5,
C     pp 671-683, 1991.
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

!***arguments

      INTEGER, INTENT( IN )  :: NLAYS

      REAL,    INTENT( IN )  :: Z   ( : )
      REAL,    INTENT( IN )  :: ZSFC
      REAL,    INTENT( IN )  :: REARTH
      REAL,    INTENT( IN )  :: SINZEN
      REAL,    INTENT( OUT ) :: DZ  ( : )  ! layer thicknesses counting from surface upward
      REAL,    INTENT( OUT ) :: DSDH( : )

!***Internal

      INTEGER I, J, K           ! loop indices
      REAL RE
      REAL DSJ                  ! slant path length [m]
      REAL DHJ                  ! layer thickness [m]
      REAL( 8 ) :: RJ, RJP1
      REAL( 8 ) :: RPSINZ            ! rpsinz = (re + zd(i)) * sinzen
      REAL( 8 ) :: RPSINZ2           ! rpsinz * rpsinz
      REAL( 8 ) :: GA, GB            ! see usage
      REAL      :: ZE( NLAYS + 1 )   ! altitudes MSL
      REAL      :: ZD( NLAYS + 1 )   ! array of altitudes indexed from top
      REAL      :: DZI( NLAYS )      ! layer thicknesses counting downward from the top

C-----------------------------------------------------------------------

!***re include the altitude above sea level to the radius of the earth

      RE = REARTH + ZSFC

!***ze is the altitude above msl

      DO K = 1, NLAYS + 1
         ZE( K ) = Z( K )
!!sjr    ZE(K) = Z(K) - ZSFC
      END DO

!***  DZ(1) = ZE(2) - ZE(1)
!***  DZI(1) = ZE(NLAYS + 1) - ZE(NLAYS)

!***calculate dz

      DO K = 1, NLAYS
         DZ( K ) = ZE( K + 1 ) - ZE( K )
      END DO

!***zd, dzi are inverse coordinates of ze & dz

      DO K = 1, NLAYS + 1
         J = NLAYS + 1 - K + 1
         ZD( J ) = ZE( K )
      END DO

      DO K = 1, NLAYS
         J = NLAYS + 1 - K
         DZI( J ) = DZ( K )
      END DO

!***initialize dsdh

      DO I = 1, NLAYS
         DSDH( I ) = 0.0
      END DO

!***FSB The following code is a direct implementation of appendix B
!***  of Dahlbeck and Stamnes (1991) for the case of solar zenith
!***  angle less than 90 degree.

!***calculate ds/dh of every layer starting at the top

      DO J = 1, NLAYS
!***  K = NLAYS - J +1
         RPSINZ  = REAL( ( RE + ZD( J ) ) * SINZEN , 8 )
         RPSINZ2 = RPSINZ * RPSINZ

         IF ( J .LT. NLAYS ) THEN
            RJ   = REAL( RE + ZD( J ), 8 )
            RJP1 = REAL( RE + ZD( J + 1 ), 8 )
            DHJ  = DZI( J )
         ELSE
            RJ   = REAL( RE + ZD( J ), 8)
            RJP1 = REAL( RE, 8 )
            DHJ  = DZI( J )
         END IF

!***define GA and GB

         GB = SQRT( MAX( 0.0D0, RJ * RJ     - RPSINZ2 ) )
         GA = SQRT( MAX( 0.0D0, RJP1 * RJP1 - RPSINZ2 ) )

!***This is equation B1 from Dahlbeck and Stamnes (1991)

         DSJ = ABS( REAL(GB - GA, 4 ) )

!***this is the slant path (Chapman) function.

         DSDH( J ) = DSJ / DHJ    ! Note dsdh is on a top to bottom grid.

      END DO   ! loop over altitude

      RETURN
      END SUBROUTINE SLANTPATH2

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE SLANTPATHTOP ( ZTOM, ZTOA, ZSFC, REARTH, SINZEN,
     &                          DSDHTOP )
C-----------------------------------------------------------------------
C  FSB This is a SPECIAL version to get the slant path from the top of
C    the modeling domain (ztom) to the top of the atmosphere (ztoa).
C-----------------------------------------------------------------------
C  PURPOSE:
C     Calculate slant path, ds/dh, over vertical depth in spherical
C     geometry also calculates the layer thicknesses.
C     NOTE!!!
C     This version is restricted to zenith angle less than 90 degrees
C-----------------------------------------------------------------------
C  ARGUMENTS:
C     INPUT:
C       ztom    - REAL, altitude (agl) of top of modeling domain [m] <<<meters
C       This is from file ZF ( full layers ) from METCRO3D
C       Z(1) is zero.
C       ztoa    - REAL altitude (msl) of top of atmosphere [ m ]
C       zsfc    - REAL, ground elevation (msl) [m]
C       rearth  - REAL, radius of the earth [m]
C       sinzen  - REAL, sine of solar zenith angle
C
C     OUTPUT:
C       dsdhtop   - REAL, slant path of direct beam through each layer
C       when travelling from the top of the atmosphere downward
C       to the top of top model
C-----------------------------------------------------------------------
C  EDIT HISTORY:
C     Inspired by sphers from TUV
C     09/08/2004 modified  to specialize for CMAQ application
C     11/11/2004 modified to do just the one layer from ztom to ztoa.
C     by Dr. Francis S. Binkowski
C     Environmental Modeling for Policy Development group,
C     The Carolina Environmental Program
C     The University of North Carolina-Chapel Hill
C     Email: frank_binkowski@unc.edu
C
C-----------------------------------------------------------------------
C  REFERENCE:
C     Dahlback, A. and K. Stamnes, A new spherical model for computing
C     the radiation field available for photolysis and heating at
C     twilight, Planetary and Space Sciences, Vol. 39, No. 5,
C     pp 671-683, 1991.
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

!***arguments

      REAL, INTENT(IN)  :: ZTOM
      REAL, INTENT(IN)  :: ZTOA
      REAL, INTENT(IN)  :: ZSFC
      REAL, INTENT(IN)  :: REARTH
      REAL, INTENT(IN)  :: SINZEN
      REAL, INTENT(OUT) :: DSDHTOP

!***Internal

      INTEGER :: I, J, K  ! loop indices
      REAL    :: RE
      REAL    :: DSJ      ! slant path length [m]
      REAL    :: DHJ      ! layer thickness [m]
      REAL(8) :: RJ, RJP1
      REAL(8) :: RPSINZ   ! rpsinz = (re + zd(i)) * sinzen
      REAL(8) :: RPSINZ2  ! rpsinz * rpsinz
      REAL(8) :: GA, GB   ! see usage

C-----------------------------------------------------------------------

!***re include the altitude above sea level to the radius of the earth

      RE = REARTH + ZSFC

!!sjr DHJ = ZTOA - ZTOM
      DHJ = ZTOA - ( ZTOM + ZSFC )

!***FSB The following code is a direct implementation of appendix B
!***  of Dahlbeck and Stamnes (1991) for the case of solar zenith
!***  angle less than 90 degree.

!***  calculate ds/dh of every layer starting at the top

      RPSINZ  = REAL( ( REARTH + ZTOA ) * SINZEN, 8 )
      RPSINZ2 = RPSINZ * RPSINZ

!!sjr RJ = RE + ZTOA
      RJ   = REAL( REARTH + ZTOA, 8 )
      RJP1 = REAL( RE + ZTOM, 8 )

!***define GA and GB

      GB = SQRT( MAX( 0.0D0, RJ * RJ     - RPSINZ2 ) )
      GA = SQRT( MAX( 0.0D0, RJP1 * RJP1 - RPSINZ2 ) )

!***This is equation B1 from Dahlbeck and Stamnes (1991)

      DSJ = REAL( GB - GA, 4 )

!***this is the slant path (Chapman) function.

      DSDHTOP = DSJ / DHJ

      RETURN
      END SUBROUTINE SLANTPATHTOP

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE TWOSTREAM_S ( NLEVEL, MU, RSFC, TAUU, OMU, GU, DSDH,
     &                         FDR, FUP, FDN, EDR, EUP, EDN )
C-----------------------------------------------------------------------
C  PURPOSE:
C     Solve two-stream equations for multiple layers.  The subroutine is
C     based on equations from:  Toon et al., 1989.
C     It contains only the Delta Eddington method.
C     A pseudo-spherical correction has also been added.
C     FSB This version is restricted to solar zenith angle LESS THAN 90
C     degrees
C-----------------------------------------------------------------------
C  ARGUMENTS:
C     INPUT:
C       nlevel - INTEGER, number of specified altitude levels in the
C                working grid
C       mu     - REAL, cosine of solar senith angle
C       rsfc   - REAL, surface albedo at current wavelength
C       tauu   - REAL, unscaled optical depth of each layer
C       omu    - REAL, unscaled single scattering albedo of each layer
C       gu     - REAL, unscaled asymmetry parameter of each layer
C       dsdh   - REAL, slant path of direct beam through each layer
C                crossed when travelling from the top of the atmosphere
C                to layer
C     OUTPUT:
C       fdr - REAL, contribution of the direct component to the total
C             actinic flux at each altitude level
C       fup - REAL, contribution of the diffuse upwelling component to
C             the total actinic flux at each altitude level
C       fdn - REAL, contribution of the diffuse downwelling component to
C             the total actinic flux at each altitude level
C       edr - REAL, contribution of the direct component to the total
C             spectral irradiance at each altitude level
C       eup - REAL, contribution of the diffuse upwelling component to
C             the total spectral irradiance at each altitude level
C       edn - REAL, contribution of the diffuse downwelling component to
C             the total spectral irradiance at each altitude level
C-----------------------------------------------------------------------
C  EDIT HISTORY:
C     This is a modification of ps2str.f from TUV
C     this routine has been modified from the original TUV code by
C     Dr. Francis S. Binkowski, Carolina Environmental Program
C     09/2004 removed various two-stream methods (FSB)
C     09/2004 made mu an input (FSB)
C     09/2004 simplified for case of solar zenith angle less than
C             90 degrees
C-----------------------------------------------------------------------
C  References:
C
C     Joseph, J.H., W.J. Wiscombe, and J.A. Weinman, The delta-Eddington
C     Approximation for radiative flux transfer, Jour. Atmos. Res.,
C     Vol.33, No. 12, pages 2452 - 2459, December , 1976.
C     (the method implemented here)
C
C     Toon, O.B., C.P. McKay, T.P. Ackerman, and K. Santhanam, Rapid
C     calculation of radiative heating rates and photodissociation rates
C     in inhomogeneous multiple scattering atmospheres, J. Geophys. Res.
C     Vol. 94, No. D13, Pages 16,287 - 16,301, November 20, 1989.
C     (all citations for equation numbers and page numbers are to this
C     reference)
C
C     Zeng, J., S. Madronich, and K. Stamnes, A note on the use of the
C     two-stream delta-scaling approximation for calculating atmospheric
C     photolysis rate coefficients, Journal of Geophysical Research,
C     vol 101, D9, pp 14,525 - 14530, June 20, 1996.
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, PARAMETER :: KZ = 100
      INTEGER, PARAMETER :: NROWS = 2 * KZ

!***arguments

      INTEGER, INTENT( IN ) :: NLEVEL

      REAL,    INTENT(IN )  :: MU
      REAL,    INTENT(IN )  :: RSFC
      REAL,    INTENT(IN )  :: TAUU( : )
      REAL,    INTENT(IN )  :: OMU ( : )
      REAL,    INTENT(IN )  :: GU  ( : )
      REAL,    INTENT(IN )  :: DSDH( : )
      REAL,    INTENT(OUT ) :: FUP ( : )
      REAL,    INTENT(OUT ) :: FDN ( : )
      REAL,    INTENT(OUT ) :: FDR ( : )
      REAL,    INTENT(OUT ) :: EUP ( : )
      REAL,    INTENT(OUT ) :: EDN ( : )
      REAL,    INTENT(OUT ) :: EDR ( : )

!***local:

      REAL( 8 ) :: TAUC  ( 0:KZ )        ! optical depth variable
      REAL( 8 ) :: TAUSLA( 0:KZ )        ! slant path optical depth
      REAL( 8 ) :: MU2   ( 0:KZ )        ! replaces mu1 for slant path

!***internal coefficients and matrix

      REAL( 8 ) :: LAM( KZ ), TAUN( KZ ), BGAM( KZ )
      REAL( 8 ) :: E1( KZ ), E2( KZ ), E3( KZ ), E4( KZ )
      REAL( 8 ) :: CUP( KZ ), CDN( KZ ), CUPTN( KZ ), CDNTN( KZ )
      REAL( 8 ) :: A( NROWS ), B( NROWS ), D( NROWS ), E( NROWS ), Y( NROWS )

!***other:

      REAL( 8 ) :: GI( KZ ), OMI( KZ ), TEMPG
      REAL( 8 ) :: PIFS, FDN0
      REAL( 8 ) :: F, G, OM
      REAL( 8 ) :: GAM1, GAM2, GAM3, GAM4

      REAL( 8 ) :: EXPON0, EXPON1
      REAL( 8 ) :: EXPON, DIVISR, TEMP, UP, DN
      REAL( 8 ) :: SSFC

      REAL( 8 ) :: MU1  ! reciprocal of constant from delta_Eddington method

      INTEGER   :: ROW
      INTEGER   :: I, J
      INTEGER   :: NLAYER, MROWS, LEV

!***Some additional program constants:

!!!!from above      REAL, PARAMETER :: PI          = 3.1415926535898  ! pi
      REAL( 8 ), PARAMETER :: LARGEST     = 1.0D+36 ! largest machine number
      REAL( 8 ), PARAMETER :: SMALLEST    = 1.0D-36 ! smallest machine number
      REAL( 8 ), PARAMETER :: SQRTLRGST   = 1.0D+18 ! sqrt(largest)
      REAL( 8 ), PARAMETER :: SQRTLRGSTM1 = 1.0D-18 ! 1/sqrt(largest)
      REAL( 8 ), PARAMETER :: EPS         = 1.0D-03
      REAL( 8 ), PARAMETER :: PRECIS      = ( 1.0D0 - 1.0D-07 )

      REAL( 8 ), PARAMETER :: LOG_DBLE_SMALLEST = -709.090848126508D0
      REAL( 8 ), PARAMETER :: LOG_DBLE_LARGEST  =  709.090848126508D0

C-----------------------------------------------------------------------

!***boundary conditions:

      PIFS = 1.0D0                ! solar flux is set to unity here.
      FDN0 = 0.0D0                ! no downward diffuse flux

      NLAYER = NLEVEL - 1

      DO J = 0, KZ
         TAUC  ( J ) = 0.0D0
         TAUSLA( J ) = 0.0D0
         MU2   ( J ) = SQRTLRGST
      END DO

!***scaling for delta-Eddington approximation

!***start diagnostic prinout

      DO I = 1, NLAYER
      
!         print*,'ACM TWSTREAM GU, F = ', F, GU(I)
         F = REAL( GU(I) * GU(I), 8 )
         GI  (I) = ( REAL( GU(I), 8 ) - F ) / ( 1.0D0 - F )
         OMI (I) = ( 1.0D0 - F ) * REAL( OMU(I), 8 ) 
     &           / ( 1.0D0 - REAL( OMU(I), 8 ) * F )
         TAUN(I) = ( 1.0D0 - REAL( OMU(I), 8 ) * F ) 
     *           *  REAL( TAUU(I), 8)
      END DO

!***set  tausla to be slant optical path contribution for each layer.

      DO I = 1, NLAYER
         TAUSLA(I) = TAUN(I) * REAL( DSDH(I), 8 )
      END DO

      TAUC(1)   = TAUN(1)

      DO I = 1, NLAYER
         TAUN(I)   = TAUN(I)
         TAUSLA(I) = TAUSLA(I-1) + TAUSLA(I) ! NOTE redefinition of tausla
                                             ! to be a sum over altitude.
         TAUC(I)   = TAUC(I-1) + TAUN(I)

!***FSB calculate MU2(i). This is the substitute for mu ( = 1/ coszen) for the
!***  pseudo spherical approximation. It is ther ratio of vertical optical
!***  depth to the slant optical depth.
!***  This has been simplified from TUV ps2str because only
!***  zenith angles < or = 90 degrees are considered.

!         IF ( TAUSLA(I) .EQ. TAUSLA(I-1) ) THEN
!            MU2(I) = SQRTLRGST
!         ELSE
!            MU2(I) = (TAUC(I) - TAUC(I-1)) / (TAUSLA(I) - TAUSLA(I-1))
!            MU2(I) = SIGN( MAX( ABS( MU2(I)), SQRTLRGSTM1 ), MU2(I) )
!         END IF
!         MU2(I) = 1.0D0 / MU2(I)
!      END DO

         IF ( TAUSLA(I) .EQ. TAUSLA(I-1) ) THEN
            MU2(I) = SQRTLRGSTM1
         ELSE
            MU2(I) =  (TAUSLA(I) - TAUSLA(I-1)) / (TAUC(I) - TAUC(I-1)) 
            MU2(I) = SIGN( MIN( ABS( MU2(I)), SQRTLRGST ), MU2(I) )
         END IF
      END DO

!***compute coefficients for each layer:
!***  gam1 - gam4 = 2-stream coefficients
!***  expon0 = calculation of e when tau is zero
!***  expon1 = calculation of e when tau is taun
!***  cup and cdn = calculation when tau is zero
!***  cuptn and cdntn = calc. when tau is taun
!***  divisr = prevents division by zero

      DO 11, I = 1, NLAYER

         G = GI(I)
         OM = OMI(I)
!***stay away from 1 by precision.  For g, also stay away from -1

!        TEMPG = MIN( ABS( G ), 1.0D0 - PRECIS )
         TEMPG = MIN( ABS( G ), PRECIS )
         G = SIGN( TEMPG, G )
!        OM = MIN( OM, 1.0D0 - PRECIS )
         OM = MIN( OM, PRECIS )

!***calculate the gamma values from line 1 Table 1, page 16,289

         GAM1 =  ( 7.0D0 - OM * ( 4.0D0 + 3.0D0 * G ) ) * 0.25D0
         GAM2 = -( 1.0D0 - OM * ( 4.0D0 - 3.0D0 * G ) ) * 0.25D0
         GAM3 =  ( 2.0D0 - 3.0D0 * G * REAL( MU, 8 )  ) * 0.25D0
         GAM4 =  1.0D0 - GAM3
         MU1  =  2.0D0

!***lambda = pg 16,290 equation 21
!***big gamma = pg 16,290 equation 22

         IF ( ABS( GAM2 ) .LE. SMALLEST ) THEN
            WRITE( LOGDEV, 2609) I, GI(I), OMI(I), G, OM, GAM1, GAM2
            WRITE( LOGDEV, 2610)
         END IF

         IF ( ( GAM1*GAM1 - GAM2*GAM2 ) .LT. 0.0D0 ) THEN
            WRITE( LOGDEV, 2609) I, GI(I), OMI(I), G, OM, GAM1, GAM2
            WRITE( LOGDEV, 2611)
         END IF
 
     
         LAM(I) = SQRT( GAM1*GAM1 - GAM2*GAM2 )

         IF ( ABS( GAM2 ) .LE. SMALLEST ) THEN 
!***adjustment based on NCAR TUV model
            BGAM(I) = 0.0D0
         ELSE
            BGAM(I) = ( GAM1 - LAM(I) ) / GAM2
         END IF

         EXPON = EXP( MAX( LOG_DBLE_SMALLEST, -LAM(I) * TAUN(I) ) ) 

!***e1 - e4 = pg 16,292 equation 44

         E1(I) = 1.0D0 + BGAM(I) * EXPON
         E2(I) = 1.0D0 - BGAM(I) * EXPON
         E3(I) = BGAM(I) + EXPON
         E4(I) = BGAM(I) - EXPON

!***the following sets up for the C equations 23, and 24
!***  found on page 16,290
!***  prevent division by zero (if LAMBDA = 1 / MU,
!***  shift 1/MU^2 by  EPS = 1.E-3
!***  which is approx equiv to shifting MU by 0.5*EPS* (MU)**3

         DIVISR = LAM(I) * LAM(I) - MU2(I) * MU2(I)
         TEMP = MAX( EPS, ABS( DIVISR ) )
         DIVISR = SIGN( TEMP , DIVISR )
         UP = OM * PIFS *
     &        ( ( GAM1 - MU2(I) ) * GAM3 + GAM4 * GAM2 ) / DIVISR


         DN = OM * PIFS *
     &        ( ( GAM1 + MU2(I) ) * GAM4 + GAM2 * GAM3 ) / DIVISR

!***cup and cdn are when tau is equal to zero
!***cuptn and cdntn are when tau is equal to taun

         EXPON0 = EXP( MAX( LOG_DBLE_SMALLEST, -TAUSLA(I-1) ) )
         EXPON1 = EXP( MAX( LOG_DBLE_SMALLEST, -TAUSLA(I) ) )

         CUP(I)   = UP * EXPON0
         CDN(I)   = DN * EXPON0
         CUPTN(I) = UP * EXPON1
         CDNTN(I) = DN * EXPON1

 11   CONTINUE                  ! loop on layer

!***set up matrix
!*** ssfc = pg 16,292 equation 37  where pi Fs is one (unity).
      SSFC   = REAL( RSFC * MU, 8 ) * PIFS
     &       * EXP( MAX( LOG_DBLE_SMALLEST, -TAUSLA( NLAYER ) ) ) 

!***MROWS = the number of rows in the matrix

      MROWS = 2 * ( NLAYER )

!*** the following are from pg 16,292  equations 39 - 43.
!*** set up first row of matrix:

      I = 1
      A(1) = 0.0D0
      B(1) = E1(I)
      D(1) = -E2(I)
      E(1) = FDN0 - CDN(I)

      ROW = 1

!***set up odd rows 3 thru (MROWS - 1):

      I = 0
      DO 20, ROW = 3, MROWS - 1, 2
         I = I + 1
         A(ROW) = E2(I) * E3(I) - E4(I) * E1(I)
         B(ROW) = E1(I) * E1(I + 1) - E3(I) * E3(I + 1)
         D(ROW) = E3(I) * E4(I + 1) - E1(I) * E2(I + 1)
         E(ROW) = E3(I) * ( CUP(I + 1) - CUPTN(I) ) +
     &            E1(I) * ( CDNTN(I) - CDN(I + 1) )
 20   CONTINUE

!***set up even rows 2 thru (MROWS - 2):

      I = 0
      DO 30, ROW = 2, MROWS - 2, 2
         I = I + 1
         A(ROW) = E2(I + 1) * E1(I) - E3(I) * E4(I + 1)
         B(ROW) = E2(I) * E2(I + 1) - E4(I) * E4(I + 1)
         D(ROW) = E1(I + 1) * E4(I + 1) - E2(I + 1) * E3(I + 1)
         E(ROW) = ( CUP(I + 1) - CUPTN(I) ) * E2(I + 1) -
     &            ( CDN(I + 1) - CDNTN(I) ) * E4(I + 1)
 30   CONTINUE

!***set up last row of matrix at MROWS:

      ROW = MROWS
      I = NLAYER
      F = REAL( RSFC, 8 )
      A(ROW) = E1(I) - F * E3(I)
      B(ROW) = E2(I) - F * E4(I)
      D(ROW) = 0.0D0
      E(ROW) = SSFC - CUPTN(I) + F * CDNTN(I)

!***solve tri-diagonal matrix:
      CALL TRIDIAGONAL ( A, B, D, E, Y, MROWS )

!*** unfold solution of matrix, compute output fluxes:

      ROW = 1
      LEV = 1
      J = 1

!***the following equations are from pg 16,291  equations 31 & 32

      FDR(LEV) = 1.0    ! this the downward flux at the top of the model
      EDR(LEV) = MU * REAL( FDR(LEV) )
      EDN(LEV) = REAL( FDN0 )
      EUP(LEV) = REAL( Y(ROW) * E3(J) - Y(ROW + 1) * E4(J) + CUP(J) )
      FDN(LEV) = EDN(LEV) * REAL( MU1 )
      FUP(LEV) = EUP(LEV) * REAL( MU1 )

      DO 60, LEV = 2, NLAYER + 1
         FDR(LEV) = REAL( EXP( MAX( LOG_DBLE_SMALLEST, -TAUSLA(LEV-1) )  ) )
!         FDR(LEV) = REAL( EXPONS(LEV-1) ) ! REAL( EXP( MAX( LOG_DBLE_SMALLEST, -TAUSLA(LEV-1) )  ) )
         EDR(LEV) = MU * FDR(LEV)
         EDN(LEV) = REAL( Y(ROW) * E3(J) + Y(ROW + 1) * E4(J) + CDNTN(J) )
         EUP(LEV) = REAL( Y(ROW) * E1(J) + Y(ROW + 1) * E2(J) + CUPTN(J) )
         FDN(LEV) = EDN(LEV) * REAL( MU1 )
         FUP(LEV) = EUP(LEV) * REAL( MU1 )

         ROW = ROW + 2
         J = J + 1
 60   CONTINUE

2609  FORMAT(/ 'PHOT_MOD: Instability in Two Stream RadTran Subroutine'
     &       / 'Layer: I = ', I3
     &       / 'Asymmetry Factor: GI = ', ES12.4
     &       / 'Single Scattering Albedo: OMI = ', ES12.4
     &       / 'SIGN( MIN( ABS( GI ), 1.0 - 1.0E-7), GI ): G = ', ES12.4
     &       / 'MIN( OMI, 1.0 - 1.0E-7 ): OM = ', ES12.4
     &       / '0.25*( 7.0 - OM * ( 4.0 + 3.0 * G ) ): GAM1 = ', ES12.4
     &       / '0.25*( OM * ( 4.0 + 3.0 * G ) - 1.0 ): GAM2 = ', ES12.4  /)

2610  FORMAT(/ 'Setting (GAM1 - SQRT(GAM1**2 - GAM2**2))/GAM2 to zero' /)

2611  FORMAT(/ ' ( GAM1**2 - GAM2**2 ) < 0: NaNs introduced into solution ' /)

      RETURN
      END SUBROUTINE TWOSTREAM_S


      SUBROUTINE TRIDIAGONAL ( A, B, D, E, Y, N )
C-----------------------------------------------------------------------
C  This version has the same variable names as in twostream_s, that
C  is D is the superdiagonal and E is the right hand side, and Y is
C  the  solution. The size of A,B,D,E and Y is now N, the
C  number of rows in the matrix.
C-----------------------------------------------------------------------
C
C  Function:
C     Solves tridiagonal system by Thomas algorithm.  Algorithm fails
C     if first pivot is zero.  In that case, rewrite the
C     equation as a set of order N-1, with U(2) trivially eliminated.
C     The associated tri-diagonal system is stored in 3 arrays
C     B: diagonal
C     A: sub-diagonal
C     D: super-diagonal
C     E: right hand side function
C     U : return solution from tridiagonal solver
C
C     [ B(1) D(1) 0    0    0 ...       0     ]
C     [ A(2) B(2) D(2) 0    0 ...       .     ]
C     [ 0    A(3) B(3) D(3) 0 ...       .     ]
C     [ .       .     .     .           .     ] Y(i) = E(i)
C     [ .             .     .     .     0     ]
C     [ .                   .     .     .     ]
C     [ 0                           A(N) B(N) ]
C
C  Preconditions:
C
C  Subroutines and Functions Called:
C
C  Revision History:
C     NO.   DATE     WHO      WHAT
C     __    ____     ___      ____
C     5     11/09/04  FSB  Changed symbols to match twostream_s
C                          made arrays variable.
C     4     4/3/96    SJR  copied code and modified for use in JPROC
C     3     8/16/94   XKX  configuration management include statements
C     2     3/15/92   CJC  For use in Models-3 LCM.
C     1     10/19/89  JKV  converted for use on IBM
C     0      3/89     BDX  Initial version
C     yoj
C-----------------------------------------------------------------------

      IMPLICIT NONE

!***Arguments:

      INTEGER,   INTENT( IN )  :: N        ! number of rows in matrix
      REAL( 8 ), INTENT( IN )  :: A( : )   ! subdiagonal
      REAL( 8 ), INTENT( IN )  :: B( : )   ! diagonal
      REAL( 8 ), INTENT( IN )  :: D( : )   ! superdiagonal
      REAL( 8 ), INTENT( IN )  :: E( : )   ! R.H. side
      REAL( 8 ), INTENT( OUT ) :: Y( : )   ! solution

!***Local Variables:

      INTEGER J   ! loop index

      REAL( 8 ) :: BET        !
      REAL( 8 ) :: GAM( N )   !

!***Decomposition and forward substitution:

      BET = 1.0D0 / B( 1 )
      Y( 1 ) = BET * E( 1 )

      DO J = 2, N
         GAM( J ) = BET * D( J - 1 )
         BET = 1.0D0 / ( B( J ) - A( J ) * GAM( J ) )
         Y( J ) = BET * ( E( J ) - A( J ) * Y( J - 1) )
      END DO

!***Back-substitution:

      DO J = N - 1, 1, -1
         Y( J ) = Y( J ) - GAM( J + 1 ) * Y( J + 1 )
      END DO

      RETURN

      END SUBROUTINE TRIDIAGONAL

C///////////////////////////////////////////////////////////////////////

      INTEGER FUNCTION INDEXR0 ( NAME1, N, NAME2 )
C-----------------------------------------------------------------------
C
C  Function:
C     This routine searches for NAME1 in list NAME2
C
C  Revision History:
C     5/88   Modified for ROMNET
C     July 29, 2005 by FSB
C     Changed name to avoid conflict FSB
C     copied from CMAQ routine INDEX2 to allow internal use
C
C  Argument List Description:
C
C  Input Arguments:
C     NAME1       Character string being searched for
C     N           Length of array to be searched
C     NAME2       Character array to be searched
C
C  Output Arguments:
C     INDEX1      The position within the NAME2 array that NAME1
C                 found. If string was not found, INDEX1 = 0
C
C  Local Variable Description:
C     None
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: N

      CHARACTER*(*), INTENT(IN) :: NAME1
      CHARACTER*(*), INTENT(IN) :: NAME2(*)

      INTEGER I

!***Assume NAME1 is not in list NAME2

      INDEXR0 = 0

      DO I = 1, N
         IF ( INDEX( NAME2( I ), NAME1 ) .EQ. 1 ) THEN
            INDEXR0 = I
            RETURN
         END IF
      END DO

      RETURN
      END FUNCTION INDEXR0

      END MODULE PHOT_MOD
