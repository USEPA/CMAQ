
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



!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE CSQY_DATA

      USE RUNTIME_VARS

      IMPLICIT NONE

      CHARACTER( 32 ), PUBLIC :: JTABLE_REF

      INTEGER, PUBLIC :: NPHOT_REF  ! # ref phot reactions
      INTEGER, PUBLIC :: NTEMP_REF  ! # ref temperatures
      INTEGER, PUBLIC :: NWL_REF    ! # ref wavelengths

!...Names of the mapped photolysis reactions (available to chemical)
!... mechanisms) and their pointers to the reference photolysis rxn

      CHARACTER( 16 ), ALLOCATABLE, PUBLIC :: PNAME_REF( : )

!...Setup the Mapping from CMAQ chemical reactions to the reference data

      INTEGER, PUBLIC :: NPHOT_MAP  ! #  phot mapped reactions

      CHARACTER( 16 ), ALLOCATABLE, PUBLIC :: PNAME_MAP( : )
      INTEGER, ALLOCATABLE,         PUBLIC :: PHOT_MAP ( : )

      REAL, PUBLIC, ALLOCATABLE :: STWL_REF ( : )
      REAL, PUBLIC, ALLOCATABLE :: EFFWL_REF( : )
      REAL, PUBLIC, ALLOCATABLE :: ENDWL_REF( : )

      REAL, ALLOCATABLE, PUBLIC :: CLD_BETA_REF    ( : )  ! cloud extinction coef divided by LWC
      REAL, ALLOCATABLE, PUBLIC :: CLD_COALBEDO_REF( : )  ! cloud coalbedo
      REAL, ALLOCATABLE, PUBLIC :: CLD_G_REF       ( : )  ! cloud asymmetry factor

      REAL, ALLOCATABLE, PUBLIC :: FSOLAR_REF( : )        ! initial solar flux [photons*cm-2*s-1]

      REAL, ALLOCATABLE, PUBLIC :: TEMP_BASE ( : )        ! reference temperatures
      REAL, ALLOCATABLE, PUBLIC :: TEMP_REF( :,: )        ! reference temperatures

      REAL, ALLOCATABLE, PUBLIC :: CS_REF ( :,:,: )       ! effective cross sections
      REAL, ALLOCATABLE, PUBLIC :: QY_REF ( :,:,: )       ! effective quantum yields
      REAL, ALLOCATABLE, PUBLIC :: ECS_REF( :,:,: )       ! CS*QY averaged UCI Solar Flux

      INTEGER,           PUBLIC :: NTEMP_STRAT_REF        ! number of stratos temperatures
      REAL, ALLOCATABLE, PUBLIC :: TEMP_STRAT_REF( : )    ! temperature for stratos O3 xcross, K
      REAL, ALLOCATABLE, PUBLIC :: O3_CS_STRAT_REF( :,: ) ! ozone xcross at stratos temperatures, cm2

!...    effective quantum yields were computed by performing separate
!...    interval integrations for the cross sections and for the
!...    effective cross sections (cs*qy) (calculated on the finer
!...    wavelength grid.  The effective quantum yield values
!...    were then calculated for the 7 wavelength intervals by
!...    dividing the effective cross sections by the interval average
!...    cross sections (eQY=eCS/CS).

      REAL, ALLOCATABLE, PUBLIC :: EQY_REF( :,:,: ) ! eCS/CS averaged 77 bins in UCI Model

      INTEGER, PUBLIC :: NUM_REFRACTIVE

      TYPE MODAL_COMPLEX
         CHARACTER( 16 ) :: NAME                           ! name of complex property
         REAL, ALLOCATABLE, DIMENSION( :, : ) :: REAL_PART ! real part
         REAL, ALLOCATABLE, DIMENSION( :, : ) :: IMAG_PART ! imaginary part
      END TYPE MODAL_COMPLEX

      TYPE( MODAL_COMPLEX ), ALLOCATABLE, PUBLIC :: REFRACTIVE_INDEX( : )

      INTEGER, PUBLIC :: IWLR  ! wavelength loop variable
      INTEGER, PUBLIC :: ITTR  ! temperature loop variable

! arrays for the size and optical properties of liquid droplets. The latter
! is a function of radius and wavelength
      INTEGER, PUBLIC  :: NRADIUS_LIQUID

      REAL, ALLOCATABLE, PUBLIC  ::  RADIUS_LIQUID( : )       ! droplet radius, um
      REAL, PUBLIC               ::  INIT_RADIUS_LIQUID
      REAL, PUBLIC               ::  MAXI_RADIUS_LIQUID
      REAL, PUBLIC               ::  MINI_RADIUS_LIQUID
      REAL, PUBLIC               ::  FREQ_RADIUS_LIQUID

      REAL, ALLOCATABLE, PUBLIC ::  LIQUID_EXTINCT( :, : )   ! extinction coefficient, m**3/g
      REAL, ALLOCATABLE, PUBLIC :: LIQUID_ASYMFACT( :, : )   ! asymmetery factor, dimensionaless
      REAL, ALLOCATABLE, PUBLIC :: LIQUID_COALBEDO( :, : )   ! One minus single scattering albebo, dimensionaless

! arrays for the size and optical properties of ice particles. The latter
! is a function of effective diameter and wavelength

      INTEGER, PUBLIC  :: NDIAMETER_ICE

      REAL, ALLOCATABLE, PUBLIC ::  DIAMETER_ICE( : )     ! particle effective diameter, um
      REAL, PUBLIC              ::  INIT_DIAMETER_ICE
      REAL, PUBLIC              ::  MAXI_DIAMETER_ICE
      REAL, PUBLIC              ::  MINI_DIAMETER_ICE
      REAL, PUBLIC              ::  FREQ_DIAMETER_ICE

      REAL, ALLOCATABLE, PUBLIC ::  ICE_EXTINCT( :, : )   ! extinction coefficient, m**3/g
      REAL, ALLOCATABLE, PUBLIC :: ICE_ASYMFACT( :, : )   ! asymmetery factor, dimensionaless
      REAL, ALLOCATABLE, PUBLIC :: ICE_COALBEDO( :, : )   ! One minus single scattering albebo, dimensionaless
      REAL, ALLOCATABLE, PUBLIC :: ICE_DELTRANS( :, : )   ! Delta Transmission Function at zero scattering angle, dimensionaless

      PUBLIC            :: LOAD_CSQY_DATA, LOAD_OPTICS_DATA, GET_CSQY


!***Information for photolysis

      INTEGER, SAVE       :: NWL     ! number of wavelengths
!     INTEGER, PARAMETER  :: NWL_INLINE_METHOD = 7

      INTEGER JWAVE             ! index use for wavelength
      INTEGER ITEMP             ! index for temperature
      INTEGER IRRXN

      REAL, ALLOCATABLE :: WAVELENGTH( : )  ! effective wavelengths [nm ]
      REAL, ALLOCATABLE :: WAVENUMBER( : )  ! effective wavenumbers [cm-1]

      REAL, ALLOCATABLE :: FEXT( : )   ! downward solar direct flux at the top of
                                             ! of the Atmosphere.  [ photons / ( cm **2 s) ]

!***surface albedo

      REAL, ALLOCATABLE :: ALB( : )  ! set in subroutine PHOT

!**Cloud albedo values from JPROC

      REAL, ALLOCATABLE :: CLOUD_BETA_LWC( : ) ! cloud extinction coef divided by LWC
      REAL, ALLOCATABLE :: CLOUD_COALBEDO( : ) ! cloud coalbedo
      REAL, ALLOCATABLE :: CLOUD_G( : )        ! cloud asymmetry factor

      INTEGER            :: NTEMP_STRAT
      REAL, ALLOCATABLE  :: XO3CS( :,: )       !
      REAL, ALLOCATABLE  :: TEMP_O3_STRAT( : ) ! temperature for XO3CS, K

!***arrays for reference data for needed photolysis rates

      REAL, ALLOCATABLE :: XXCS( :,:,: )  ! absorption cross sections
      REAL, ALLOCATABLE :: XXQY( :,:,: )  ! quantum yield
      REAL, ALLOCATABLE :: RTEMP_S( :,: )

      INTEGER                     :: MECHANISM_RATES
      CHARACTER(16), ALLOCATABLE  :: PHOTOLYSIS_RATE( : ) ! subset of photolysis rates from CSQY DATA

!***Indices for special case photolysis cross sections

      INTEGER :: LNO2
      INTEGER :: LO3O1D
      INTEGER :: LO3O3P
      INTEGER :: LACETONE
      INTEGER :: LKETONE
      INTEGER :: LMGLY_ADJ
      INTEGER :: LMGLY_ABS
      INTEGER :: LHCHOR_06
      INTEGER :: LH2O2
      INTEGER :: LHNO3
      INTEGER :: LACETONE_CO
      INTEGER :: LACETONE_CH3CO

      LOGICAL :: ACETONE_CHANNELS

! integer pointer for specific density correction to cross-section
! and/or quantum yield values
            INTEGER, PARAMETER :: ACETALDEHYDE           =  1
            INTEGER, PARAMETER :: HIGHER_ALDEHYDES       =  2
            INTEGER, PARAMETER :: METHYL_VINYL_KETONE    =  3
            INTEGER, PARAMETER :: METHYL_ACROLEIN        =  4
            INTEGER, PARAMETER :: METHYL_ETHYL_KETONE    =  5
            INTEGER, PARAMETER :: METHYL_GLYOXAL_IUPAC04 =  6
            INTEGER, PARAMETER :: ACROLEIN               =  7
            INTEGER, PARAMETER :: FORMALDEHYDE_MOLECULAR =  8
            INTEGER, PARAMETER :: ACETONE                =  9
            INTEGER, PARAMETER :: KETONE_LEGACY          = 10
            INTEGER, PARAMETER :: KETONE_RACM2           = 11
            INTEGER, PARAMETER :: GLYOXAL_RACM2          = 12
            INTEGER, PARAMETER :: METHYL_GLYOXAL_LEGACY  = 13
            INTEGER, PARAMETER :: NBUTYRALDEHYDE         = 14
            INTEGER, PARAMETER :: BIACETYL               = 15
            INTEGER, PARAMETER :: ACETONE_CH3CO          = 16
            INTEGER, PARAMETER :: GLYOXAL_IUPAC_2013     = 17
            INTEGER, PARAMETER :: CH3CHO_IUPAC2013       = 18

            CHARACTER( 32 ) :: CSQY_ADJUSTMENTS( 0:18 )
            DATA CSQY_ADJUSTMENTS /
     &                             'NO_ADJUSTMENT',
     &                             'ACETALDEHYDE',
     &                             'HIGHER_ALDEHYDES',
     &                             'METHYL_VINYL_KETONE',
     &                             'METHYL_ACROLEIN',
     &                             'METHYL_ETHYL_KETONE',
     &                             'METHYL_GLYOXAL_IUPAC04',
     &                             'ACROLEIN',
     &                             'FORMALDEHYDE_MOLECULAR',
     &                             'ACETONE',
     &                             'KETONE_LEGACY',
     &                             'KETONE_RACM2',
     &                             'GLYOXAL_RACM2',
     &                             'METHYL_GLYOXAL_LEGACY',
     &                             'NBUTYRALDEHYDE',
     &                             'BIACETYL',
     &                             'ACETONE_CH3CO',
     &                             'GLYOXAL_IUPAC_2013',
     &                             'CH3CHO_IUPAC2013'      /

! integer pointer for density correction to a photolysis rates' cross-section
! and/or quantum yield values
            INTEGER, ALLOCATABLE :: CSQY_ADJUST( : )

            REAL,    ALLOCATABLE :: IPHI0_MGLY( : ) ! Reciprocal Methyl Glyoxal Quantum Yields at zero pressure
            REAL,    ALLOCATABLE :: KMGLY    ( : )  ! Quenching Coefficient for Methyl Glyoxal Quantum Yields

! coefficients used correct acetaldehyde Quantum Yields based on IUPAC 2013 recommendations
            REAL,    ALLOCATABLE :: IPHIS_CH3CHO( : ) ! Reciprocal acetaldehyde Quantum Yields at surface pressure
            REAL,    ALLOCATABLE :: IPHI0_CH3CHO( : ) ! Reciprocal acetaldehyde Quantum Yields at zero pressure
            REAL,    ALLOCATABLE :: KCH3CHO     ( : ) ! Quenching Coefficient for acetaldehyde Quantum Yields



      INTEGER :: IREFTEMPS  ! number of ref. temperatures

      INTEGER :: NUMB_LANDUSE_REF
      INTEGER :: INDEX_GRASSLAND_REF
      INTEGER :: INDEX_OCEAN_REF
      INTEGER :: INDEX_SEA_ICE

      CHARACTER(30), ALLOCATABLE :: LANDUSE_REF( : )
      REAL,          ALLOCATABLE :: ZENITH_COEFF_REF( : )
      REAL,          ALLOCATABLE :: SEASON_COEFF_REF( : )
      REAL,          ALLOCATABLE :: SNOW_COEFF_REF( : )
      REAL,          ALLOCATABLE :: SPECTRAL_ALBEDO_REF( :,: )

      INTEGER, PARAMETER         :: NUMB_EXPECT_NLCD50  = 50
      INTEGER                    :: NUMB_LANDUSE_NLCD50
      CHARACTER(60), ALLOCATABLE :: LANDUSE_NLCD50( : )
      INTEGER,       ALLOCATABLE :: ALBMAP_REF2NLCD50( : )
      REAL,          ALLOCATABLE :: ALBFAC_REF2NLCD50( : )

      INTEGER, PARAMETER         :: NUMB_EXPECT_NLCD40  = 40
      INTEGER, SAVE              :: NUMB_LANDUSE_NLCD40
      CHARACTER(60), ALLOCATABLE :: LANDUSE_NLCD40( : )
      INTEGER,       ALLOCATABLE :: ALBMAP_REF2NLCD40( : )
      REAL,          ALLOCATABLE :: ALBFAC_REF2NLCD40( : )

      INTEGER, PARAMETER         :: NUMB_EXPECT_USGS  = 24
      INTEGER                    :: NUMB_LANDUSE_USGS
      CHARACTER(60), ALLOCATABLE :: LANDUSE_USGS( : )
      INTEGER,       ALLOCATABLE :: ALBMAP_REF2USGS( : )
      REAL,          ALLOCATABLE :: ALBFAC_REF2USGS( : )

      INTEGER, PARAMETER         :: NUMB_EXPECT_MODIS = 33
      INTEGER                    :: NUMB_LANDUSE_MODIS
      CHARACTER(60), ALLOCATABLE :: LANDUSE_MODIS( : )
      INTEGER,       ALLOCATABLE :: ALBMAP_REF2MODIS( : )
      REAL,          ALLOCATABLE :: ALBFAC_REF2MODIS( : )

      LOGICAL      :: NO_NLCD40
      LOGICAL      :: WRITE_CELL

!***special information for acetone
!***  Reference:
!***     Cameron-Smith, P., Incorporation of non-linear
!***     effective cross section parameterization into a
!***     fast photolysis computation  code (Fast-J)
!***     Journal of Atmospheric Chemistry, Vol. 37,
!***     pp 283-297, 2000.

      INTEGER, PARAMETER :: NWL_ACETONE_FJX = 7

      REAL :: OP0( 2, NWL_ACETONE_FJX ) ! variable needed for acetone

      DATA ( OP0( 1, JWAVE ), JWAVE = 1, NWL_ACETONE_FJX ) /
     &     2.982E-20, 1.301E-20, 4.321E-21, 1.038E-21,
     &     5.878E-23, 1.529E-25, 0.0/

      DATA ( OP0( 2, JWAVE ), JWAVE = 1, NWL_ACETONE_FJX ) /
     &     3.255E-20, 1.476E-20, 5.179E-21, 1.304E-21,
     &     9.619E-23, 2.671E-25, 0.0 /

      REAL :: YY30( NWL_ACETONE_FJX )   ! variable needed for acetone

      DATA YY30 / 5.651E-20, 1.595E-19, 2.134E-19,
     &     1.262E-19, 1.306E-19, 1.548E-19, 0.0 /

      REAL :: OPTT                ! variable needed for acetone

      CONTAINS

      SUBROUTINE LOAD_CSQY_DATA ( )
!-----------------------------------------------------------------------
!  Purpose: read input file for
!           -wavelength bin and temperature structure.
!           -photolysis cross-sections and quantum
!
!  Revision History:
!   31 Jan 2014 B.Hutzell: Initial Version based on LOAD_REF_DATA in
!   CMAQ version 5.0
!   07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
!-----------------------------------------------------------------------

      USE UTILIO_DEFN
      USE RXNS_DATA           ! chemical mechanism data

      IMPLICIT NONE

!***arguments

C     NONE

!***local

      LOGICAL :: WRITE_LOG = .TRUE.

      CHARACTER(  32 ) :: PNAME = 'LOAD_CSQY_DATA'
      CHARACTER(  16 ) :: CSQY_FILE = 'CSQY_DATA' ! CSQY_DATA i/o logical name
      CHARACTER(  16 ) :: PHOT_EXPECT
      CHARACTER(  30 ) :: LAND_EXPECT
      CHARACTER( 120 ) :: MSG                     ! buffer for messages to output
      CHARACTER( 240 ) :: FILE_LINE

      CHARACTER(  16 ),  ALLOCATABLE :: AE_RERACT_REF( : )

!     INTEGER, INTENT(OUT) :: NWL_PHOT    ! # of wavelengths used in PHOT_MOD.F
      INTEGER :: NWL_PHOT    ! # of wavelengths used in PHOT_MOD.F
      INTEGER :: IOST        ! IOST returned from OPEN function
      INTEGER :: JDATE = 0
      INTEGER :: PHOTAB_UNIT
      INTEGER :: IPHOT, IPHOT_LOAD ! loop indices
      INTEGER :: ITT, ITT_LOAD     ! loop indices
      INTEGER :: IP_MAP, IP_REF    ! photolysis reaction indicies
      INTEGER :: IWL, IWL_LOAD
      INTEGER :: STRT, FINI


      INTEGER :: NAE_REFRACT_REF

      REAL,       ALLOCATABLE :: AE_IMAG_REFRACT( :, : )
      REAL,       ALLOCATABLE :: AE_REAL_REFRACT( :, : )

      LOGICAL                  :: ERROR_FLAG = .FALSE.

!***external functions: none

      PHOTAB_UNIT = GETEFILE( CSQY_FILE, .TRUE., .TRUE., PNAME )

      IF ( PHOTAB_UNIT .LT. 0 ) THEN
         MSG = 'Error opening the CSQY data file: ' // TRIM( CSQY_FILE )
         CALL M3WARN ( PNAME, 0, 0, MSG )
         ERROR_FLAG = .TRUE.
      END IF

C...begin read

      READ( PHOTAB_UNIT,'(22X,A32)' ) JTABLE_REF

      IF ( JTABLE_REF .NE. MECHNAME ) THEN
         MSG =  'WARNING: JTABLE mechanism is for ' // JTABLE_REF
     &       // ' but gas chemistry name is '       // MECHNAME
         CALL M3WARN( PNAME, 0, 0, MSG )
      END IF

      READ( PHOTAB_UNIT,'(10X,I4)' ) NPHOT_MAP

      IF ( NPHOT_MAP .LT. NPHOTAB ) THEN
         WRITE( MSG,'( A,1X,I4,1X,A,1X,I4)')
     &   'Error: CSQY data file has',NPHOT_MAP,
     &   'rates but the need number is',NPHOTAB
          CALL M3WARN( PNAME, 0, 0, MSG )
         ERROR_FLAG = .TRUE.
      END IF


#ifdef verbose_phot
      write( LOGDEV,'(A,a32)' )'JTABLE_REF = ',trim(jtable_ref)
      write( LOGDEV,'(A,10x,i4)' )'NPHOT_MAP = ', nphot_map
#endif

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

      ALLOCATE( PNAME_MAP( NPHOT_MAP ) )
      ALLOCATE( PNAME_REF( NPHOT_MAP ) )
      ALLOCATE( PHOT_MAP ( NPHOT_MAP ) )

      DO IPHOT_LOAD = 1, NPHOT_MAP
         READ( PHOTAB_UNIT,'(A16)' ) PNAME_REF( IPHOT_LOAD )

#ifdef verbose_phot
         write( LOGDEV,'(i3,1x,a16)' ) iphot_load, pname_ref( iphot_load )
#endif

         PNAME_MAP( IPHOT_LOAD ) = PNAME_REF( IPHOT_LOAD )
         PHOT_MAP ( IPHOT_LOAD ) = IPHOT_LOAD
      END DO

      READ( PHOTAB_UNIT,'(10X,I3)' ) NTEMP_REF

#ifdef verbose_phot
      write( LOGDEV,'(10x,i3)' ) ntemp_ref
#endif

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

#ifdef verbose_phot
      write( LOGDEV,* ) trim( file_line )
#endif

      IREFTEMPS = NTEMP_REF

      ALLOCATE( TEMP_BASE( NTEMP_REF ) )

      DO ITT_LOAD = 1, NTEMP_REF
         READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

#ifdef verbose_phot
         write( LOGDEV,* ) trim( file_line )
#endif

         READ( FILE_LINE,* ) IPHOT_LOAD, TEMP_BASE( ITT_LOAD )

#ifdef verbose_phot
         write( LOGDEV,'(4x,f6.2)' ) temp_base( itt_load )
#endif

      END DO

      ALLOCATE( TEMP_REF( NTEMP_REF, NPHOT_MAP) )

      DO ITT_LOAD = 1, 15 ! skip next 15 lines
         READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( LOGDEV, '(I2,1X,A)' )ITT_LOAD,TRIM(FILE_LINE)
#endif
      END DO

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

#ifdef verbose_phot
      write( LOGDEV, '(A)' )TRIM(FILE_LINE)
#endif
      READ( FILE_LINE, 4999) NWL_REF

#ifdef verbose_phot
      write( LOGDEV,'(17x,i3)' ) nwl_ref
#endif


4999  FORMAT(17X,I3,2X,17X,I3)


      NWL       = NWL_REF
      NWL_PHOT  = NWL


      IF ( NWL_REF .LT. 1 ) THEN
         WRITE( LOGDEV,* ) 'NWL_REF  = ', NWL_REF
         MSG = 'NWL_REF in ' // CSQY_FILE
     &       // ' has the bad value, written above. '
         CALL M3EXIT( PNAME, 0, 0, MSG, -1 )
      END IF

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( LOGDEV, * )FILE_LINE
#endif

      IF( .NOT. ALLOCATED( STWL_REF   ) ) ALLOCATE( STWL_REF  ( NWL_REF ) )
      IF( .NOT. ALLOCATED( ENDWL_REF  ) ) ALLOCATE( ENDWL_REF ( NWL_REF ) )
      IF( .NOT. ALLOCATED( FSOLAR_REF ) ) ALLOCATE( FSOLAR_REF( NWL_REF ) )
      IF( .NOT. ALLOCATED( EFFWL_REF  ) ) ALLOCATE( EFFWL_REF ( NWL_REF ) )
      IF( .NOT. ALLOCATED( FEXT       ) ) ALLOCATE( FEXT      ( NWL_REF ) )
      IF( .NOT. ALLOCATED( WAVELENGTH ) ) ALLOCATE( WAVELENGTH( NWL_REF ) )
      IF( .NOT. ALLOCATED( WAVENUMBER ) ) ALLOCATE( WAVENUMBER( NWL_REF ) )

      DO IWL_LOAD = 1, NWL_REF
!         READ( PHOTAB_UNIT,'(4X,3(F8.3,2X),2X,ES12.4,2X,2(F8.3,2X),ES12.4,2X)' )
         READ( PHOTAB_UNIT, * )iphot_load,
     &         STWL_REF( IWL_LOAD ), EFFWL_REF( IWL_LOAD ),
     &         ENDWL_REF( IWL_LOAD ), FSOLAR_REF( IWL_LOAD )

#ifdef verbose_phot
         write( LOGDEV,'(4x,3(f8.3,2x),2x,2(es12.4,2x),f8.3,2x,12(es12.4,2x))' )
     &          stwl_ref( iwl_load ), effwl_ref( iwl_load ),
     &          endwl_ref( iwl_load ),fsolar_ref( iwl_load )
#endif
         WAVELENGTH( IWL_LOAD ) = EFFWL_REF ( IWL_LOAD )
         WAVENUMBER( IWL_LOAD ) = 1.0E7 / EFFWL_REF ( IWL_LOAD )
         FEXT      ( IWL_LOAD ) = FSOLAR_REF( IWL_LOAD )
      END DO


      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

      ALLOCATE( CS_REF (  NWL_REF, NTEMP_REF, NPHOT_MAP ) )
      ALLOCATE( QY_REF (  NWL_REF, NTEMP_REF, NPHOT_MAP ) )
      ALLOCATE( EQY_REF(  NWL_REF, NTEMP_REF, NPHOT_MAP ) )
      ALLOCATE( ECS_REF(  NWL_REF, NTEMP_REF, NPHOT_MAP ) )

      CS_REF = 0.0
      QY_REF  = 0.0
      EQY_REF = 0.0
      ECS_REF = 0.0

      DO IPHOT_LOAD = 1, NPHOT_MAP
         DO ITT_LOAD = 1, NTEMP_REF
            READ( PHOTAB_UNIT,'(A16,7X,F8.3,1X,40(1PE12.6,2X))' )
     &            PHOT_EXPECT, TEMP_REF( ITT_LOAD, IPHOT_LOAD),
     &            ( CS_REF( IWL_LOAD, ITT_LOAD, IPHOT_LOAD ), IWL_LOAD = 1, NWL_REF )

#ifdef verbose_phot
            write( LOGDEV,'(a16,7x,f8.3,1x,40(1pe13.6,2x))' )
     &             phot_expect, temp_ref( itt_load, iphot_load),
     &             ( cs_ref( iwl_load, itt_load, iphot_load ), iwl_load = 1, nwl_ref )
#endif

            IF ( PHOT_EXPECT .NE. PNAME_REF( IPHOT_LOAD ) ) THEN
                MSG =  'CS for ' // TRIM( PHOT_EXPECT )
     &              // ' does match the order the PHOT_MAP array.'
                CALL M3EXIT( PNAME, 0, 0, MSG, -1 )
            END IF

            READ( PHOTAB_UNIT,'(A16,7X,F8.3,1X,40(1PE12.6,2X))' )
     &            PHOT_EXPECT, TEMP_REF( ITT_LOAD, IPHOT_LOAD),
     &            ( EQY_REF( IWL_LOAD, ITT_LOAD, IPHOT_LOAD ), IWL_LOAD = 1, NWL_REF )

            QY_REF( 1:NWL_REF, ITT_LOAD, IPHOT_LOAD ) = EQY_REF( 1:NWL_REF, ITT_LOAD, IPHOT_LOAD )

#ifdef verbose_phot
            write( LOGDEV,'(a16,7x,f8.3,1x,40(1pe13.6,2x))' )
     &             phot_expect, temp_ref( itt_load, iphot_load),
     &             ( qy_ref( iwl_load, itt_load, iphot_load ), iwl_load = 1, nwl_ref )
#endif

            IF ( PHOT_EXPECT .NE. PNAME_REF(IPHOT_LOAD) ) THEN
               MSG =  'EQY for ' // TRIM( PHOT_EXPECT )
     &             // ' does match the order the PHOT_MAP array.'
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
         END DO
      END DO

      DO ITT_LOAD = 1, 3 ! skip next 3 lines
         READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
      END DO

      READ( PHOTAB_UNIT,'(15X,I3)' ) NTEMP_STRAT_REF

#ifdef verbose_phot
      write( LOGDEV,'(16x,i3)' ) ntemp_strat_ref
#endif

      ALLOCATE( TEMP_STRAT_REF ( NTEMP_STRAT_REF ) )
      ALLOCATE( O3_CS_STRAT_REF( NWL_REF, NTEMP_STRAT_REF ) )

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

      DO ITT_LOAD = 1, NTEMP_STRAT_REF
         READ( PHOTAB_UNIT,'(A16,7X,F8.3,1X,40(1PE12.6,2X))' )
     &         PHOT_EXPECT, TEMP_STRAT_REF( ITT_LOAD ),
     &         ( O3_CS_STRAT_REF( IWL_LOAD, ITT_LOAD ), IWL_LOAD = 1, NWL_REF )

#ifdef verbose_phot
         write( LOGDEV,'(a16,7x,f8.3,1x,40(1pe13.6,2x))' )
     &          phot_expect, temp_strat_ref( itt_load ),
     &          ( o3_cs_strat_ref( iwl_load, itt_load ), iwl_load = 1, nwl_ref )
#endif

         IF ( PHOT_EXPECT .NE. 'O3_STRAT' ) THEN
            MSG = 'O3_STRAT not found at expected location in CSQY_FILE. ' //
     &            TRIM( PHOT_EXPECT ) // ' found.'
            CALL M3WARN( PNAME, 0, 0, MSG )
            ERROR_FLAG = .TRUE.
         END IF
      END DO


      NTEMP_STRAT = NTEMP_STRAT_REF
      ALLOCATE( TEMP_O3_STRAT( NTEMP_STRAT_REF ) )
      ALLOCATE( XO3CS        ( NTEMP_STRAT_REF, NWL_PHOT ) )

      DO ITT_LOAD = 1, NTEMP_STRAT_REF
         TEMP_O3_STRAT( ITT_LOAD ) = TEMP_STRAT_REF( ITT_LOAD )
         DO IWL_LOAD = 1, NWL_PHOT
            XO3CS( ITT_LOAD, IWL_LOAD ) = O3_CS_STRAT_REF( IWL_LOAD, ITT_LOAD )
         END DO
      END DO

!***initialize pointers for mandatory photolysis rates

      LNO2      = 0
      LO3O1D    = 0
      LO3O3P    = 0
      LACETONE  = 0
      LKETONE   = 0
      LMGLY_ADJ = 0
      LMGLY_ABS = 0
      LHCHOR_06 = 0
      LH2O2     = 0
      LHNO3     = 0

! initialized pointers and flag if specific acetone channel are used
      LACETONE_CO    = 0
      LACETONE_CH3CO = 0
      ACETONE_CHANNELS = .FALSE.

!***get needed photolysis data for the model chemistry from the
!***CSQY_DATA

       ALLOCATE( PHOTOLYSIS_RATE ( NPHOTAB ) )
       ALLOCATE( XXCS( IREFTEMPS, NWL, NPHOTAB ) )
       ALLOCATE( XXQY( IREFTEMPS, NWL, NPHOTAB ) )
       ALLOCATE( RTEMP_S( IREFTEMPS, NPHOTAB ) )

       MECHANISM_RATES = NPHOTAB


       DO IPHOT = 1, NPHOTAB
          IP_MAP = INDEXR( PHOTAB( IPHOT ), NPHOT_MAP, PNAME_MAP )
          IF ( IP_MAP .LE. 0 ) THEN
             MSG = 'FATAL ERROR: photolysis reaction ' // TRIM( PHOTAB( IPHOT ) )
     &          // ' not found in ' //
     &             'the reference data! '
             ERROR_FLAG = .TRUE.
             CALL M3WARN ( PNAME, 0, 0, MSG )
             CYCLE
          END IF
          IP_REF = PHOT_MAP( IP_MAP )
          PHOTOLYSIS_RATE( IPHOT ) = PNAME_MAP( IP_MAP )

!***check to see if this photolysis reaction is a special case that
!***  is referenced in other sections of the code.  if so, then set
!***  the appropriate pointers for later processing

           SELECT CASE ( PHOTOLYSIS_RATE( IPHOT ) )
              CASE( 'O3O3P', 'O3O3P_SAPRC99', 'O3O3P_06', 'O3_O3P_IUPAC04', 'O3O3P_NASA06', 'O3_O3P_IUPAC10' )
                    LO3O3P = IPHOT
              CASE( 'NO2', 'NO2_SAPRC99', 'NO2_06', 'NO2_RACM2', 'NO2_IUPAC10' )
                    LNO2 = IPHOT
              CASE( 'O3O1D',  'O3O1D_SAPRC99' , 'O3O1D_06', 'O3_O1D_IUPAC04', 'O3O1D_NASA06', 'O3_O1D_IUPAC10' )
                    LO3O1D = IPHOT
              CASE( 'KETONE', 'KET_RACM2', 'KET_JGR19' )
                    LKETONE   = IPHOT
              CASE( 'MGLY_ADJ' )
                    LMGLY_ADJ = IPHOT
              CASE(  'MGLY_ABS' )
                    LMGLY_ABS = IPHOT
              CASE( 'ACETONE', 'CH3COCH3_RACM2', 'ACET_IUPAC10' )
                    IF( NWL .EQ. NWL_ACETONE_FJX ) LACETONE  = IPHOT
              CASE( 'HCHOR_06', 'HCHO_R_SAPRC99', 'HCHO_RAD_RACM2', 'FORM_R_IUPAC10', 
     &              'FORM_R_IUPAC13', 'HCHO_R_MCMv32', 'HCHO_RAD_JPL19' )
                    LHCHOR_06 = IPHOT
              CASE( 'ACET_CH3CO_CRI', 'CH3COCH3A_JPL19' )
                    LACETONE_CH3CO   = IPHOT
                    ACETONE_CHANNELS = .TRUE.
              CASE( 'H2O2', 'H2O2_SAPRC99', 'H2O2_RACM2', 'H2O2_IUPAC10' )
                   LH2O2 = IPHOT
              CASE( 'HNO3', 'HNO3_IUPAC04', 'HNO3_IUPAC10', 'HNO3_RACM2' )
                   LHNO3 = IPHOT
           END SELECT



!***load the local cross section & quantum yield data from the reference
!***  dataset for this photolysis reaction

            DO ITT = 1, IREFTEMPS
               RTEMP_S( ITT, IPHOT ) = TEMP_REF( ITT, IP_REF )
               DO IWL = 1, NWL
                  XXCS( ITT, IWL, IPHOT ) = CS_REF( IWL, ITT, IP_REF )
                  XXQY( ITT, IWL, IPHOT ) = QY_REF( IWL, ITT, IP_REF )
               END DO   ! iwl
            END DO   ! itt

       END DO   ! iphot

       IF ( LNO2   .EQ. 0 ) THEN
          MSG = 'NO2 cross-section not found in the CSQY data! '
          ERROR_FLAG = .TRUE.
          CALL M3WARN ( PNAME, 0, 0, MSG )
       END IF
       IF ( LO3O1D .EQ. 0 ) THEN
          MSG = 'O3(1D) production not found in the CSQY data! '
          CALL M3WARN ( 'NEW_OPTICS', 0, 0, MSG )
       END IF
       IF ( LO3O3P .EQ. 0 ) THEN
          MSG = 'O3 cross-section not found in the CSQY data! '
          ERROR_FLAG = .TRUE.
          CALL M3WARN ( PNAME, 0, 0, MSG )
       END IF
       IF ( LH2O2 .EQ. 0 ) THEN
          MSG = 'H2O2 cross-section not found in the CSQY data! '
          ERROR_FLAG = .TRUE.
          CALL M3WARN ( PNAME, 0, 0, MSG )
       END IF
       IF ( LHNO3 .EQ. 0 ) THEN
          MSG = 'HNO3 cross-section not found in the CSQY data! '
          ERROR_FLAG = .TRUE.
          CALL M3WARN ( PNAME, 0, 0, MSG )
       END IF

       IF( ERROR_FLAG )THEN
         MSG = 'The above fatal error(s) found in CSQY data! '
         CALL M3EXIT( PNAME, 0, 0, MSG, -1 )
       END IF

      CLOSE(PHOTAB_UNIT)

5012  FORMAT( 4X,A30,1X,3(F8.3,2X) )
5013  FORMAT( 22X,I3 )
5016  FORMAT( 4X,A60,1X,I3,2X,3(F8.3,2X) )

#ifdef verbose_phot
6009  format( a3,', ',8(a,', ') )
6013  format( a22,1x,i3 )
6016  format( i3,1x,a60,1x,i3,2x,3(f8.3,2x) )
#endif

      RETURN
      END SUBROUTINE LOAD_CSQY_DATA



      SUBROUTINE LOAD_OPTICS_DATA()
!-----------------------------------------------------------------------
!  Purpose: read input file for
!           -wavelength bin for cross check against
!           -size dependent optical data for liquid droplets and ice
!            ice particles
!           -landuse type data for surface alebdo
!
!  Revision History:
!   31 Jan 2014 B.Hutzell: Initial Version based on LOAD_REF_DATA in
!   CMAQ version 5.0
!-----------------------------------------------------------------------

      USE UTILIO_DEFN
      USE AERO_DATA,   ONLY: N_MODE

      IMPLICIT NONE

!***arguments

      REAL, PARAMETER  :: EPSILON = 1.0E-6     ! small number

!***local

      LOGICAL :: WRITE_LOG = .TRUE.

      CHARACTER(  32 ) :: PNAME         = 'LOAD_OPTICS_DATA'
      CHARACTER(  16 ) :: OPTICS_FILE   = 'OPTICS_DATA'      ! OPTICS_DATA i/o logical name
      CHARACTER(  16 ) :: OPTICS_EXPECT
      CHARACTER(  16 ) :: QUANTITY
      CHARACTER(  30 ) :: LAND_EXPECT
      CHARACTER( 120 ) :: MSG                               ! buffer for messages to output
      CHARACTER( 240 ) :: FILE_LINE

      CHARACTER(  16 ),  ALLOCATABLE :: AE_RERACT_REF( : )

!     INTEGER, INTENT(OUT) :: NWL_OPTICS    ! # of wavelengths used in PHOT_MOD.F
      INTEGER :: NWL_OPTICS    ! # of wavelengths used in PHOT_MOD.F
      INTEGER :: IOST        ! IOST returned from OPEN function
      INTEGER :: JDATE = 0
      INTEGER :: OPTICS_UNIT
      INTEGER :: IPHOT, IPHOT_LOAD ! loop indices
      INTEGER :: ITT, ITT_LOAD     ! loop indices
      INTEGER :: IP_MAP, IP_REF    ! photolysis reaction indicies
      INTEGER :: IWL_LOAD
      INTEGER :: STRT, FINI

      INTEGER :: NAE_REFRACT_REF

      REAL,       ALLOCATABLE :: AE_IMAG_REFRACT( :, : )
      REAL,       ALLOCATABLE :: AE_REAL_REFRACT( :, : )
      REAL                    :: DELTA

      LOGICAL                  :: ERROR_FLAG = .FALSE.

!***external functions: none

      OPTICS_UNIT = GETEFILE( OPTICS_FILE, .TRUE., .TRUE., PNAME )


      READ( OPTICS_UNIT,'(A)' ) FILE_LINE

#ifdef verbose_phot
      write( LOGDEV, '(A)' )TRIM(FILE_LINE)
#endif

      READ( FILE_LINE, 4999) NWL_REF
#ifdef verbose_phot
      write( LOGDEV,'(17x,i3)' ) nwl_ref
#endif

      NWL_OPTICS = NWL_REF

      DO ITT_LOAD = 1, 15 ! skip next 15 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( LOGDEV, '(A)' )TRIM(FILE_LINE)
#endif
      END DO

      ALLOCATE( CLD_BETA_REF    ( NWL_REF ), CLOUD_BETA_LWC( NWL_REF ) )
      ALLOCATE( CLD_COALBEDO_REF( NWL_REF ), CLOUD_COALBEDO( NWL_REF ) )
      ALLOCATE( CLD_G_REF       ( NWL_REF ), CLOUD_G       ( NWL_REF ) )

      IF( .NOT. ALLOCATED( STWL_REF   ) ) ALLOCATE( STWL_REF  ( NWL_REF ) )
      IF( .NOT. ALLOCATED( ENDWL_REF  ) ) ALLOCATE( ENDWL_REF ( NWL_REF ) )
      IF( .NOT. ALLOCATED( FSOLAR_REF ) ) ALLOCATE( FSOLAR_REF( NWL_REF ) )
      IF( .NOT. ALLOCATED( EFFWL_REF  ) ) ALLOCATE( EFFWL_REF ( NWL_REF ) )
      IF( .NOT. ALLOCATED( FEXT       ) ) ALLOCATE( FEXT      ( NWL_REF ) )
      IF( .NOT. ALLOCATED( WAVELENGTH ) ) ALLOCATE( WAVELENGTH( NWL_REF ) )

      DO IWL_LOAD = 1, NWL_REF
         READ( OPTICS_UNIT, * )IPHOT_LOAD,
     &   STWL_REF( IWL_LOAD ), EFFWL_REF( IWL_LOAD ), ENDWL_REF( IWL_LOAD ),
     &   CLD_BETA_REF( IWL_LOAD ), CLD_G_REF( IWL_LOAD ), CLD_COALBEDO_REF( IWL_LOAD )


#ifdef verbose_phot
         write( LOGDEV, 99946 )
     &   stwl_ref( iwl_load ), effwl_ref( iwl_load ), endwl_ref( iwl_load ),
     &   cld_beta_ref( iwl_load ), cld_g_ref( iwl_load ), cld_coalbedo_ref( iwl_load )
#endif

         WAVELENGTH( IWL_LOAD ) = EFFWL_REF ( IWL_LOAD )
         FEXT      ( IWL_LOAD ) = FSOLAR_REF( IWL_LOAD )

         CLOUD_BETA_LWC( IWL_LOAD ) = CLD_BETA_REF    ( IWL_LOAD )
         CLOUD_COALBEDO( IWL_LOAD ) = CLD_COALBEDO_REF( IWL_LOAD )
         CLOUD_G       ( IWL_LOAD ) = CLD_G_REF       ( IWL_LOAD )

      END DO

      DO ITT_LOAD = 1, 7 ! skip next 7 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( LOGDEV, '(A)' )TRIM(FILE_LINE)
#endif
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( LOGDEV, '(A)' )TRIM(FILE_LINE)
#endif

      READ( FILE_LINE, 4999)NAE_REFRACT_REF

#ifdef verbose_phot
      write( LOGDEV, * )' NAE_REFRACT_REF = ', NAE_REFRACT_REF
#endif

      NUM_REFRACTIVE = NAE_REFRACT_REF
      
      ALLOCATE( AE_RERACT_REF   ( NAE_REFRACT_REF ) )

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE

#ifdef verbose_phot
      write( LOGDEV, '(a)')TRIM(FILE_LINE)
#endif

      STRT = SCAN(FILE_LINE, '=', BACK = .TRUE.) + 1
      FINI =  LEN(FILE_LINE)

      READ( FILE_LINE( STRT:FINI ), * )( AE_RERACT_REF( ITT_LOAD ),
     &                                   ITT_LOAD = 1, NAE_REFRACT_REF )

#ifdef verbose_phot
      write( LOGDEV, 99947)'REFRACTIVE_INDICES'
      write( LOGDEV, 99948 )(AE_RERACT_REF( ITT_LOAD ),ITT_LOAD = 1, 
     &                                 NAE_REFRACT_REF )
#endif


      ALLOCATE( AE_REAL_REFRACT ( NAE_REFRACT_REF, NWL_REF ) )
      ALLOCATE( AE_IMAG_REFRACT ( NAE_REFRACT_REF, NWL_REF ) )
      ALLOCATE( REFRACTIVE_INDEX( NAE_REFRACT_REF ) )

      DO ITT_LOAD = 1, NAE_REFRACT_REF
! set up refractive indices used by aero_photdata routine

          REFRACTIVE_INDEX( ITT_LOAD )%NAME = AE_RERACT_REF( ITT_LOAD )
          ALLOCATE( REFRACTIVE_INDEX( ITT_LOAD )%REAL_PART( N_MODE, NWL_REF ) )
          ALLOCATE( REFRACTIVE_INDEX( ITT_LOAD )%IMAG_PART( N_MODE, NWL_REF )  )

#ifdef verbose_phot
          write( LOGDEV, '(i3, 1x, a16)')itt_load, refractive_index( itt_load )%name
#endif

      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( LOGDEV, '(A)' )TRIM(FILE_LINE)
#endif

      DO IWL_LOAD = 1, NWL_REF
         READ( OPTICS_UNIT, * )iphot_load,
     &   STWL_REF( IWL_LOAD ), EFFWL_REF( IWL_LOAD ), ENDWL_REF( IWL_LOAD ),
     &   (AE_REAL_REFRACT( ITT_LOAD, IWL_LOAD ), AE_IMAG_REFRACT( ITT_LOAD, IWL_LOAD ),
     &    ITT_LOAD = 1, NAE_REFRACT_REF)

               DO ITT_LOAD = 1, NAE_REFRACT_REF
                  REFRACTIVE_INDEX( ITT_LOAD )%REAL_PART( 1:N_MODE, IWL_LOAD )
     &                                      = AE_REAL_REFRACT( ITT_LOAD, IWL_LOAD )
                  REFRACTIVE_INDEX( ITT_LOAD )%IMAG_PART( 1:N_MODE, IWL_LOAD )
     &                                      = AE_IMAG_REFRACT( ITT_LOAD, IWL_LOAD )
               END DO
#ifdef verbose_phot
         write( LOGDEV, 99949 )
     &          stwl_ref( iwl_load ), effwl_ref( iwl_load ),
     &          endwl_ref( iwl_load ),fsolar_ref( iwl_load ),
     &          ( ae_real_refract( itt_load, iwl_load ),
     &            ae_imag_refract( itt_load, iwl_load ), itt_load = 1, nae_refract_ref )
#endif

      END DO

      DO ITT_LOAD = 1, 8 ! skip next 8 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif
      END DO

! read optical data for liquid droplets

      READ( FILE_LINE, 4999)NRADIUS_LIQUID

#ifdef verbose_phot
      write( LOGDEV, '(a,i4)' )'NRADIUS_LIQUID = ',NRADIUS_LIQUID
#endif

      ALLOCATE(RADIUS_LIQUID( NRADIUS_LIQUID ))

      ALLOCATE( LIQUID_EXTINCT(NRADIUS_LIQUID, NWL_OPTICS),
     &         LIQUID_ASYMFACT(NRADIUS_LIQUID, NWL_OPTICS),
     &         LIQUID_COALBEDO(NRADIUS_LIQUID, NWL_OPTICS))

      QUANTITY = 'LIQ_EXT'

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif

      DO ITT_LOAD = 1, NRADIUS_LIQUID
         READ( OPTICS_UNIT, * )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_EXTINCT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( LOGDEV, 99950 )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_EXTINCT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( OPTICS_EXPECT .NE. QUANTITY ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO


      QUANTITY = 'LIQ_ASY'

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif
      DO ITT_LOAD = 1, NRADIUS_LIQUID
         READ( OPTICS_UNIT, * )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_ASYMFACT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( LOGDEV, 99950 )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_ASYMFACT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( OPTICS_EXPECT .NE. QUANTITY ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif

      QUANTITY = 'LIQ_COA'

      DO ITT_LOAD = 1, NRADIUS_LIQUID
         READ( OPTICS_UNIT, * )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_COALBEDO( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( LOGDEV, 99950 )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_COALBEDO( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( OPTICS_EXPECT .NE. QUANTITY ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO


      DO ITT_LOAD = 1, 7 ! skip next 7 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif
      END DO

! read optical data for liquid droplets

      READ( FILE_LINE, 4999)NDIAMETER_ICE

#ifdef verbose_phot
      write( LOGDEV, '(a,i4)' )'NDIAMETER_ICE = ',NDIAMETER_ICE
#endif

      ALLOCATE(DIAMETER_ICE( NDIAMETER_ICE ))

      ALLOCATE( ICE_EXTINCT(NDIAMETER_ICE, NWL_OPTICS),
     &         ICE_ASYMFACT(NDIAMETER_ICE, NWL_OPTICS),
     &         ICE_COALBEDO(NDIAMETER_ICE, NWL_OPTICS),
     &            ICE_DELTRANS(NDIAMETER_ICE, NWL_OPTICS))

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif

      QUANTITY = 'ICE_EXT'

      DO ITT_LOAD = 1, NDIAMETER_ICE
         READ( OPTICS_UNIT, * )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_EXTINCT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( LOGDEV, 99950 )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_EXTINCT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( OPTICS_EXPECT .NE. QUANTITY ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif

      QUANTITY = 'ICE_ASY'

      DO ITT_LOAD = 1, NDIAMETER_ICE
         READ( OPTICS_UNIT, * )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_ASYMFACT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( LOGDEV, 99950 )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_ASYMFACT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( OPTICS_EXPECT .NE. QUANTITY ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif

      QUANTITY = 'ICE_COA'

      DO ITT_LOAD = 1, NDIAMETER_ICE
         READ( OPTICS_UNIT, * )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_COALBEDO( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( LOGDEV, 99950 )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_COALBEDO( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( OPTICS_EXPECT .NE. QUANTITY ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif

      QUANTITY = 'ICE_DEL'

      DO ITT_LOAD = 1, NDIAMETER_ICE
         READ( OPTICS_UNIT, * )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_DELTRANS( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( LOGDEV, 99950 )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_DELTRANS( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( OPTICS_EXPECT .NE. QUANTITY ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO

! Determine limit and frequencies of liquid and ice inputs
      MAXI_RADIUS_LIQUID = MAXVAL( RADIUS_LIQUID )
      MINI_RADIUS_LIQUID = MINVAL( RADIUS_LIQUID )

      DELTA = (RADIUS_LIQUID( 2 ) - RADIUS_LIQUID( 1 ))

      IF( DELTA  .LE. EPSILON )THEN
         WRITE( MSG, 99951)DELTA
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
      ELSE
         FREQ_RADIUS_LIQUID = 1.0 / DELTA
         INIT_RADIUS_LIQUID = MINI_RADIUS_LIQUID
     &                      - DELTA
      END IF

      MAXI_DIAMETER_ICE = MAXVAL( DIAMETER_ICE )
      MINI_DIAMETER_ICE = MINVAL( DIAMETER_ICE )

      DELTA = (DIAMETER_ICE( 2 ) - DIAMETER_ICE( 1 ))

      IF( DELTA  .LE. EPSILON )THEN
         WRITE( MSG, 99952)DELTA
         CALL M3WARN( PNAME, 0, 0, MSG )
         ERROR_FLAG = .TRUE.
      ELSE
         FREQ_DIAMETER_ICE = 1.0 / DELTA
         INIT_DIAMETER_ICE = MINI_DIAMETER_ICE
     &                     - DELTA
      END IF

!  read data for calculating surface albedo

      DO ITT_LOAD = 1, 6 ! skip next 6 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif
      END DO

      READ( OPTICS_UNIT,5013 ) NUMB_LANDUSE_REF

      DO ITT_LOAD = 1, 3 ! skip next 3 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif
      END DO

      READ( OPTICS_UNIT,5013 ) INDEX_GRASSLAND_REF
      READ( OPTICS_UNIT,5013 ) INDEX_OCEAN_REF
      READ( OPTICS_UNIT,5013 ) INDEX_SEA_ICE

#ifdef verbose_phot
      write( LOGDEV,6013 )'NUMB_LANDUSE_REF    = ', numb_landuse_ref
      write( LOGDEV,6013 )'INDEX_GRASSLAND_REF = ', index_grassland_ref
      write( LOGDEV,6013 )'INDEX_OCEAN_REF     = ', index_ocean_ref
      write( LOGDEV,6013 )'INDEX_SEA_ICE       = ', index_sea_ice
#endif

      ALLOCATE( LANDUSE_REF     ( NUMB_LANDUSE_REF ) )
      ALLOCATE( ZENITH_COEFF_REF( NUMB_LANDUSE_REF ) )
      ALLOCATE( SEASON_COEFF_REF( NUMB_LANDUSE_REF ) )
      ALLOCATE( SNOW_COEFF_REF  ( NUMB_LANDUSE_REF ) )
      ALLOCATE( SPECTRAL_ALBEDO_REF( NWL_OPTICS, NUMB_LANDUSE_REF ) )

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line
#ifdef verbose_phot
      write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif

      DO ITT_LOAD = 1, NUMB_LANDUSE_REF
         READ( OPTICS_UNIT,5012 ) LANDUSE_REF( ITT_LOAD ),
     &                            ZENITH_COEFF_REF( ITT_LOAD ),
     &                            SEASON_COEFF_REF( ITT_LOAD ),
     &                            SNOW_COEFF_REF( ITT_LOAD )
#ifdef verbose_phot
         write( LOGDEV,5012 ) landuse_ref( itt_load ),
     &                          zenith_coeff_ref( itt_load ),
     &                          season_coeff_ref( itt_load ),
     &                          snow_coeff_ref( itt_load )
#endif
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line
#ifdef verbose_phot
      write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif

      DO ITT_LOAD = 1, NUMB_LANDUSE_REF
         READ( OPTICS_UNIT,'(A30,1X,40(1PE12.6,2X))' ) LAND_EXPECT,
     &        ( SPECTRAL_ALBEDO_REF(IWL_LOAD, ITT_LOAD), IWL_LOAD = 1, NWL_REF )

#ifdef verbose_phot
         write( LOGDEV,'(a30,1x,40(1pe13.6,2x))' ) trim( land_expect ),
     &        ( spectral_albedo_ref(iwl_load, itt_load), iwl_load = 1, nwl_ref )
#endif

      END DO

      DO ITT_LOAD = 1, 3 ! skip next 3 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif
      END DO

      READ( OPTICS_UNIT,5013 ) NUMB_LANDUSE_NLCD50
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line

#ifdef verbose_phot
      write( LOGDEV,6013 ) 'NUMB_NLCD50_MODIS = ', numb_landuse_NLCD50
      write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif

      ALLOCATE( LANDUSE_NLCD50( NUMB_LANDUSE_NLCD50 )  )
      ALLOCATE( ALBMAP_REF2NLCD50( NUMB_LANDUSE_NLCD50 )  )
      ALLOCATE( ALBFAC_REF2NLCD50( NUMB_LANDUSE_NLCD50 )  )

      DO ITT_LOAD = 1, NUMB_LANDUSE_NLCD50
         READ( OPTICS_UNIT,5016 ) LANDUSE_NLCD50( ITT_LOAD ),
     &                            ALBMAP_REF2NLCD50( ITT_LOAD ),
     &                            ALBFAC_REF2NLCD50( ITT_LOAD )

#ifdef verbose_phot
         write( LOGDEV,6016 ) itt_load, landuse_NLCD50( itt_load ),
     &                          albmap_ref2NLCD50( itt_load ),
     &                          albfac_ref2NLCD50( itt_load )
#endif

      END DO

      READ( OPTICS_UNIT,5013 ) NUMB_LANDUSE_USGS
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line
#ifdef verbose_phot
      write( LOGDEV,6013 ) 'NUMB_USGS = ', numb_landuse_usgs
      write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif

      ALLOCATE( LANDUSE_USGS   ( NUMB_LANDUSE_USGS ) )
      ALLOCATE( ALBMAP_REF2USGS( NUMB_LANDUSE_USGS ) )
      ALLOCATE( ALBFAC_REF2USGS( NUMB_LANDUSE_USGS ) )

      DO ITT_LOAD = 1, NUMB_LANDUSE_USGS
         READ( OPTICS_UNIT,5016 ) LANDUSE_USGS( ITT_LOAD ),
     &                            ALBMAP_REF2USGS( ITT_LOAD ),
     &                            ALBFAC_REF2USGS( ITT_LOAD )

#ifdef verbose_phot
         write( LOGDEV,6016 ) itt_load, landuse_usgs( itt_load ),
     &                          albmap_ref2usgs( itt_load ),
     &                          albfac_ref2usgs( itt_load )
#endif

      END DO

      READ( OPTICS_UNIT,5013 ) NUMB_LANDUSE_MODIS
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line
#ifdef verbose_phot
      write( LOGDEV,6013 ) 'NUMB_MODIS = ', numb_landuse_modis
      write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif

      ALLOCATE( LANDUSE_MODIS   ( NUMB_LANDUSE_MODIS ) )
      ALLOCATE( ALBMAP_REF2MODIS( NUMB_LANDUSE_MODIS ) )
      ALLOCATE( ALBFAC_REF2MODIS( NUMB_LANDUSE_MODIS ) )

      DO ITT_LOAD = 1, NUMB_LANDUSE_MODIS
         READ( OPTICS_UNIT,5016 ) LANDUSE_MODIS( ITT_LOAD ),
     &                            ALBMAP_REF2MODIS( ITT_LOAD ),
     &                            ALBFAC_REF2MODIS( ITT_LOAD )

#ifdef verbose_phot
         write( LOGDEV,6016 ) itt_load, landuse_modis( itt_load ),
     &                          albmap_ref2modis( itt_load ),
     &                          albfac_ref2modis( itt_load )
#endif

      END DO

      NO_NLCD40 = .TRUE.  ! default condition that file does not contain NLCD40 Landuse data

      READ( OPTICS_UNIT,5013, END = 101 ) NUMB_LANDUSE_NLCD40
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line
#ifdef verbose_phot
      write( LOGDEV,6013 ) 'NUMB_NLCD40_MODIS = ', numb_landuse_NLCD40
      write( LOGDEV, '(a)' )TRIM(FILE_LINE)
#endif

      ALLOCATE( LANDUSE_NLCD40( NUMB_LANDUSE_NLCD40 )  )
      ALLOCATE( ALBMAP_REF2NLCD40( NUMB_LANDUSE_NLCD40 )  )
      ALLOCATE( ALBFAC_REF2NLCD40( NUMB_LANDUSE_NLCD40 )  )

      DO ITT_LOAD = 1, NUMB_LANDUSE_NLCD40
         READ( OPTICS_UNIT,5016 ) LANDUSE_NLCD40( ITT_LOAD ),
     &                            ALBMAP_REF2NLCD40( ITT_LOAD ),
     &                            ALBFAC_REF2NLCD40( ITT_LOAD )

#ifdef verbose_phot
         write( LOGDEV,6016 ) itt_load, landuse_NLCD40( itt_load ),
     &                          albmap_ref2NLCD40( itt_load ),
     &                          albfac_ref2NLCD40( itt_load )
#endif

      END DO

      NO_NLCD40 = .FALSE.

101   IF( NO_NLCD40 )THEN
          MSG = TRIM( PNAME ) // ':'
     &       // TRIM( OPTICS_FILE )
     &       // ' does not contain data for NLCD40 land use and'
     &       // ' corresponds to CMAQ version 5.01.'
          CALL M3MESG( MSG )
      END IF

! set the default values for surface albedo
      ALLOCATE( ALB( NWL_OPTICS ) )

      DO IWL_LOAD = 1, NWL_OPTICS
         IF ( WAVELENGTH( IWL_LOAD ) .LE. 380.1 ) THEN
            ALB( IWL_LOAD ) = 0.05
         ELSE
            ALB( IWL_LOAD ) = 0.10
         END IF
      END DO



      IF( ERROR_FLAG )THEN
         MSG = 'The above fatal error(s) found in CSQY data! '
         CALL M3EXIT( PNAME, 0, 0, MSG, -1 )
      END IF


      CLOSE(OPTICS_UNIT)

4999  FORMAT(17X,I3,2X,17X,I3)
5012  FORMAT( 4X,A30,1X,3(F8.3,2X) )
5013  FORMAT( 22X,I3 )
5016  FORMAT( 4X,A60,1X,I3,2X,3(F8.3,2X) )
99946 FORMAT(4x,3(f8.3,2x),2x,2(es12.4,2x),f8.3,2x,12(es12.4,2x))
99947 FORMAT(a3, 1x, a16)
99948 FORMAT(10(a16,1x))
99949 FORMAT(4x,3(f8.3,2x),2x,2(es12.4,2x),f8.3,2x,12(es12.4,2x))
99950 FORMAT(a8,1x,f10.3,40(1x,1pe13.6))
99951 FORMAT('Too Small Differences in Liquid Droplet Radii = ',1PE12.4,' um ')
99952 FORMAT('Too Small Differences in Ice Particle Sizes   = ',1PE12.4,' um ')
#ifdef verbose_phot
6009  format( a3,', ',8(a,', ') )
6013  format( a22,1x,i3 )
6016  format( i3,1x,a60,1x,i3,2x,3(f8.3,2x) )
#endif

      RETURN
      END SUBROUTINE LOAD_OPTICS_DATA

C///////////////////////////////////////////////////////////////////////
       INTEGER FUNCTION INDEXR ( NAME1, N, NAME2 )
C-----------------------------------------------------------------------
C
C  FUNCTION:
C     This routine searches for NAME1 in list NAME2
C
C  REVISION HISTORY:
C     5/88   Modified for ROMNET
C     July 29, 2005 by FSB
C     Changed name to avoid conflict FSB
C     copied from CMAQ routine INDEX2 to allow internal use
C
C  ARGUMENT LIST DESCRIPTION:
C
C  Input arguments:
C     NAME1       Character string being searched for
C     N           Length of array to be searched
C     NAME2       Character array to be searched
C
C  Output arguments:
C     INDEX1      The position within the NAME2 array that NAME1
C                 found.  If string was not found, INDEX1 = 0
C
C  LOCAL VARIABLE DESCRIPTION:
C     None
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: N

      CHARACTER*(*), INTENT(IN) :: NAME1
      CHARACTER*(*), INTENT(IN) :: NAME2(*)

      INTEGER I

!***Assume NAME1 is not in list NAME2

      INDEXR = 0

      DO I = 1, N
         IF ( INDEX( NAME2( I ), NAME1 ) .EQ. 1 ) THEN
            INDEXR = I
            RETURN
         END IF
      END DO

      RETURN
      END FUNCTION INDEXR
      SUBROUTINE GET_CSQY ( TEMP, DENS, CSZ, QYZ )
!-----------------------------------------------------------------------
!  Purpose: Calculate values of absorption cross
!     section and quantum yield, given, temperature and air
!     pressure and density.
!
!  reference for acetone:
!     Cameron-Smith, Philip J., Incorporating non_linear computation
!     code (Fast-J), Journal of Atmospheric Chemistry, Vol. 37,
!     pp 283-297, 2000)
!
!  Mar 2011: Bill Hutzell
!     - revised interpolation method for a general number of
!     interpolation points
!     - revised density corrections for specific photolysis reactions
!  Apr 2014: Bill Hutzell
!     -revised method to determine what density corrections for a
!      specific photolysis reactions from name select to index selection
!     -revision agrument an variable names to use data available in CSQY_DATA
!      and GRID_CONF modules
!  Sept 2014: Bill Hutzell
!     -revised fortrans looping constructs attempting to improve computational
!      efficiency
!     -corrected several density or pressure adjustment for quantum yield based on
!      re-consulting source and cited references
!-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications

      IMPLICIT NONE

!***Arguments


      REAL, INTENT(IN)  :: TEMP( : )      ! air temperature [K]
      REAL, INTENT(IN)  :: DENS( : )      ! air density [molecules/cm**3]
      REAL, INTENT(OUT) :: CSZ( :, :, : ) ! abs cross sections
      REAL, INTENT(OUT) :: QYZ( :, :, : ) ! quantum yields

!***Internal:

      INTEGER  :: IT, IWL, LAYS, IPHOT
      REAL     :: XTEMP            ! local temperature
      REAL     :: YTEMP            ! temperature difference ratio

      REAL,    ALLOCATABLE, SAVE :: DELTA_REFT( :, : )
      INTEGER, ALLOCATABLE, SAVE :: ITEMP( :, : )

      REAL, PARAMETER :: TTX1 = 235.0
      REAL, PARAMETER :: TTX2 = 298.0
      REAL, PARAMETER :: DTTX = TTX2 - TTX1

      REAL            :: PRESSURE          ! units vary
      REAL            :: FACTOR            ! scratch variable for yield
      REAL            :: ALPHA, BETA       ! scratch variables
      REAL            :: PHI_CO            ! CO channel of acetone QYZ
      REAL            :: PHI_CH3CO         ! CH3CO channel of acetone QYZ

      LOGICAL, SAVE   :: FIRSTCALL = .TRUE.

      IF ( FIRSTCALL ) THEN
          ALLOCATE( ITEMP( NLAYS, MECHANISM_RATES ) )
          ALLOCATE( DELTA_REFT( (IREFTEMPS - 1), MECHANISM_RATES ) )
          DO IT = 1, IREFTEMPS - 1
             DO IPHOT = 1, MECHANISM_RATES
                DELTA_REFT( IT, IPHOT ) = RTEMP_S( IT + 1, IPHOT ) -  RTEMP_S( IT, IPHOT )
             END DO
          END DO
          CALL DEFINE_CSQY_ADJUST()
          FIRSTCALL = .FALSE.
      END IF

!***determine and save where layer temperatures fall within range of CS and QY data
      DO IPHOT = 1, MECHANISM_RATES
         DO LAYS = 1, NLAYS
             IF ( TEMP( LAYS ) .LE. RTEMP_S( 1, IPHOT ) )  THEN
                ITEMP( LAYS, IPHOT ) = 0
             ELSE IF ( TEMP( LAYS ) .GE. RTEMP_S( IREFTEMPS, IPHOT ) ) THEN
                ITEMP( LAYS, IPHOT ) = IREFTEMPS
             ELSE
                 LOOP_FINDT: DO IT = 1, IREFTEMPS - 1
                    IF ( TEMP( LAYS ) .GT. RTEMP_S( IT, IPHOT ) .AND. TEMP( LAYS ) .LE. RTEMP_S( IT + 1, IPHOT ) ) THEN
                       ITEMP( LAYS, IPHOT ) = IT
                       EXIT LOOP_FINDT
                    END IF
                 END DO LOOP_FINDT
            END IF
         END DO
      END DO


      CSZ = 0.0
      QYZ = 1.0
!***loop over rates for temperature corrections
      DO IPHOT = 1, MECHANISM_RATES
!***loop over wavelengths
        DO IWL = 1, NWL_REF
!***Loop over layers:
           DO LAYS = 1, NLAYS
!***fetch temperature data
               XTEMP = TEMP( LAYS )
               IT    = ITEMP( LAYS, IPHOT )

               IF ( IT .EQ. 0 ) THEN
!***if the ambient temperature is cooler than the minimum
!***  reference temperature, then use the value at the minimum reference temperature
                  CSZ( LAYS, IWL, IPHOT ) = XXCS( IT+1, IWL, IPHOT )
                  QYZ( LAYS, IWL, IPHOT ) = XXQY( IT+1, IWL, IPHOT )
               ELSE IF ( IT .GE. 1 .AND. IT .LT. IREFTEMPS ) THEN
!***for the next case use linear interpolation
                  YTEMP = ( XTEMP - RTEMP_S( IT, IPHOT ) )
     &                  /   DELTA_REFT( IT, IPHOT )

                  CSZ( LAYS, IWL, IPHOT ) =   XXCS(   IT, IWL, IPHOT )
     &                                    + ( XXCS( IT+1, IWL, IPHOT )
     &                                      - XXCS(   IT, IWL, IPHOT ) )
     &                                    * YTEMP
                  QYZ( LAYS, IWL, IPHOT ) =   XXQY(   IT, IWL, IPHOT )
     &                                    + ( XXQY( IT+1, IWL, IPHOT )
     &                                      - XXQY(   IT, IWL, IPHOT ) )
     &                                    * YTEMP
               ELSE
!***if the ambient temperature is warmer than the maximum
!***  reference temperature, then use the value at the maximum reference temperature
                  CSZ( LAYS, IWL, IPHOT ) = XXCS( IT, IWL, IPHOT )
                  QYZ( LAYS, IWL, IPHOT ) = XXQY( IT, IWL, IPHOT )
               END IF
           END DO
        END DO
      END DO

!***Make specific temperature and/or density corrections if needed
!***Note specific acetone channels are treated outside these three nested loops
      DO IPHOT = 1, MECHANISM_RATES

        IF( CSQY_ADJUST( IPHOT ) .LT. 1 )CYCLE

        SELECT CASE (  CSQY_ADJUST( IPHOT ) )
           CASE ( ACETALDEHYDE )      ! 'CH3CHO -> CH3 + HCO'
               FORALL( IWL = 1:NWL_REF )
                  FORALL( LAYS = 1:NLAYS, QYZ( LAYS, IWL, IPHOT ) .GT. 1.0E-5 )
                     QYZ( LAYS, IWL, IPHOT ) = 1.0
     &                                       / ( 1.0 + ( 1.0 / QYZ( LAYS, IWL, IPHOT ) - 1.0 ) * DENS( LAYS ) * 4.0568E-20 )
                     QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                  END FORALL
               END FORALL
           CASE( CH3CHO_IUPAC2013 ) ! IUPAC 2013 correction for acetaldehyde
! density correction to quantum yield recommended in
! http://iupac.pole-ether.fr/htdocs/datasheets/pdf/P2_CH3CHO+hv.pdf dated June 2013
! and based on
! Warneck, P. and Moortgat, G.K. (2012). Quantum yields and photodissociation coefficients of
! acetaldehyde in the troposphere, Atmos. Environ., 62, 153-163.
               FORALL( IWL = 1:NWL_REF )
                  FORALL( LAYS = 1:NLAYS, QYZ( LAYS, IWL, IPHOT ) .GT. 1.0E-5 )
                     QYZ( LAYS, IWL, IPHOT ) = QYZ( LAYS, IWL, IPHOT )
     &                                       * IPHIS_CH3CHO( IWL )
     &                                       / ( IPHI0_CH3CHO( IWL ) + KCH3CHO( IWL ) * DENS( LAYS ) )
                     QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                  END FORALL
               END FORALL
!      IF ( FIRSTCALL )THEN
!             DO LAYS = 1, NLAYS
!                WRITE(LOGDEV, 99951)'DENS, CH3CHO_IUPAC2013 QYZ factor =  ', ' ',DENS( LAYS ),
!     &          (IPHIS_CH3CHO( IWL )/( IPHI0_CH3CHO( IWL ) + KCH3CHO( IWL ) * DENS( LAYS ) ),IWL=1,NWL_REF)
!             END DO
!      END IF
           CASE ( HIGHER_ALDEHYDES )  ! C3 and some higher aldehydes
!***density correction to quantum yield
               FORALL( IWL = 1:NWL_REF )
                  FORALL( LAYS = 1:NLAYS, QYZ( LAYS, IWL, IPHOT ) .GT. 1.0E-5 )
                     QYZ( LAYS, IWL, IPHOT ) = 1.0
     &                                       / ( 1.0 + ( 1.0 / QYZ( LAYS, IWL, IPHOT ) - 1.0 ) * DENS( LAYS ) * 4.0568E-20 )
                     QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                  END FORALL
               END FORALL
           CASE( NBUTYRALDEHYDE ) ! both photolysis channels for n-C3H7CHO
! based on recommendation in
! IUPAC Recommendation
! http://iupac.pole-ether.fr/htdocs/datasheets/pdf/P11_nC3H7CHO+hv.pdf dated 2002
               FORALL( IWL = 1:NWL_REF )
                  FORALL( LAYS = 1:NLAYS, QYZ( LAYS, IWL, IPHOT ) .GT. 1.0E-5 )
                     QYZ( LAYS, IWL, IPHOT ) = QYZ( LAYS, IWL, IPHOT )
     &                                       * ( 1.81 + 4.919E-3 * TEMP( LAYS ) )
     &                                       / ( 1.81 + 2.000E-22 * DENS( LAYS ) * TEMP( LAYS ) )
                     QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                  END FORALL
               END FORALL
           CASE ( METHYL_VINYL_KETONE )
!***quantum yield from
!***  Gierczak, T., J. B. Burkholder, R. K. Talukdar, A. Mellouki, S. B. Barone,
!***  and A. R. Ravishankara, Atmospheric fate of methyl vinyl ketone and methacrolein,
!***  J. Photochem. Photobiol A: Chemistry, 110 1-10, 1997.
!***  depends on pressure and wavelength, set upper limit to 1.0
!***  However, chamber evaluations for SAPRC07T require a pressure correction where
!***  number density coefficient is five times higher.
!***density correction to quantum yield
!***remove wavelength dependence
              FORALL( LAYS = 1:NLAYS, IWL = 1:NWL_REF )
                     QYZ( LAYS, IWL, IPHOT ) = QYZ( LAYS, IWL, IPHOT ) *  118.4
     &                                       / ( 5.5 + 4.6E-19 * DENS( LAYS ) )
                     QYZ( LAYS, IWL, IPHOT ) = MAX(0.0, MIN( QYZ( LAYS, IWL, IPHOT ), 1.0 ) )
              END FORALL
           CASE ( METHYL_ACROLEIN )
!***quantum yield based on 2.76 times MVK from
!***  Gierczak, T., J. B. Burkholder, R. K. Talukdar, A. Mellouki, S. B. Barone,
!***  and A. R. Ravishankara, Atmospheric fate of methyl vinyl ketone and methacrolein,
!***  J. Photochem. Photobiol A: Chemistry, 110 1-10, 1997.
!***  depends on pressure and wavelength, set upper limit to 1.0
!***  However, chamber evaluations for SAPRC07T require a pressure correction where
!***  number density coefficient is five times higher.
!***density correction to quantum yield
!***remove wavelength dependence
              FORALL( LAYS = 1:NLAYS, IWL = 1:NWL_REF )
                 QYZ( LAYS, IWL, IPHOT ) = QYZ( LAYS, IWL, IPHOT ) * 118.4
     &                                   / ( 5.5 + 4.6E-19 * DENS( LAYS ) )
                 QYZ( LAYS, IWL, IPHOT ) = MAX(0.0, MIN( QYZ( LAYS, IWL, IPHOT ), 1.0 ) )
              END FORALL
           CASE ( METHYL_ETHYL_KETONE )
!***Quantum Yields from
!***  Raber, W.H. (1992) PhD Thesis, Johannes Gutenberg-Universitaet, Mainz, Germany.
!***  other channels assumed negligible (less than 10%).
!***  Total quantum yield  = 0.38 at 760 Torr.
!***  Ttemperature/Density correction to quantum yield in
!***  Stern-Volmer form :  1/phi = 0.96 + 2.22e-3*P(torr)
!***  Using relative correction to quantum yields based on above formula
               FORALL( IWL = 1:NWL_REF )
                   FORALL( LAYS = 1:NLAYS, QYZ( LAYS, IWL, IPHOT ) .GT. 0.0 )
! skips SAPRC07T cases where qy is set to one and photolysis rate is scaled in mech.def
!***  Carter adjusted to 0.175 based on chamber tests and sets the values in
!***  mechanism definition file. The photolysis data submitted for SAPRC07T sets this
!***  quantum yield to one.
!                     IF( QYZ( LAYS, IWL, IPHOT ) .GE.  1.0 )THEN
!                          CYCLE
!                     END IF
!                     PRESSURE = ( 1.03547E-19 * DENS( LAYS ) * TEMP( LAYS ) ) ! TORR
!                     IF ( PRESSURE  .LT. 18.02 ) THEN
!                        QYZ( LAYS, IWL, IPHOT ) =  1.0
!                     ELSE
!                        QYZ( LAYS, IWL, IPHOT ) =  1.0 / ( 0.96 + 2.22E-3 * PRESSURE )
!                     END IF
                     QYZ( LAYS, IWL, IPHOT ) =  QYZ( LAYS, IWL, IPHOT )
     &                                       * ( 0.96 + 5.66639E-03 * TEMP( LAYS ) )
     &                                       / ( 0.96 + 2.29874E-22 * DENS( LAYS ) * TEMP( LAYS ) )
                     QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                   END FORALL
               END FORALL
           CASE ( BIACETYL )
! Biacetyl quantum yield (CH3CO + CH3CO) = 0.158 for wavelength less than 460 nm
! Pressure correction based on phi(z=infi) and ph(z=0) values based on
! http://iupac.pole-ether.fr/htdocs/datasheets/pdf/P23_Biacetyl+hv.pdf dated 2011
! by solving the below for kq
! phi(z=infi)/ph(z=0) = (0.76/0.16) = 1.0 + kq*Temp(z=0)*Number_Density(z=0)
! where Temp(z=0) = 298.15K and Number_Density(z=0) = 2.46E19 molecules/cm3
               FORALL( IWL = 1:NWL_REF )
                   FORALL( LAYS = 1:NLAYS, QYZ( LAYS, IWL, IPHOT ) .GT. 0.0 )
                     QYZ( LAYS, IWL, IPHOT ) =  QYZ( LAYS, IWL, IPHOT )
     &                                       * ( 1.0 + 1.37662E-02 * TEMP( LAYS ) )
     &                                       / ( 1.0 + 5.19481E-22 * DENS( LAYS ) * TEMP( LAYS ) )
                     QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                   END FORALL
               END FORALL
           CASE ( METHYL_GLYOXAL_IUPAC04 )
               DO IWL = 1, NWL_REF
                   IF( WAVELENGTH( IWL ) .GE. 500.0 .OR. WAVELENGTH( IWL ) .LE. 240.0 )CYCLE
                   DO LAYS = 1, NLAYS
                        IF( QYZ( LAYS, IWL, IPHOT ) .LE. 0.0 )CYCLE
! Replaced the following method used in CMAQ 5.01
!***  Pressure dependence based on Koch and Moortgat (1998),
!***  J. Phys. Chem. A, vol 102, pages 9142. The application contradicts
!***  NASA (2006) & IUPAC (2005) and is used based recommendations for
!***  SAPRC07T photolysis rates by William Carter (2009)
!                       PRESSURE = MIN( 472.0, 1.03547E-19 * DENS( LAYS ) * TEMP( LAYS )  ) ! in TORRs
!***remove wavelength dependence
!                       QYZ( LAYS, IWL, IPHOT ) = 6.4192E10 ! 1.36E8 * ( 472.0 )
!     &                                         / ( 1.0 / QYZ( LAYS, IWL, IPHOT ) - 1.0 )
!                       QYZ( LAYS, IWL, IPHOT ) = QYZ( LAYS, IWL, IPHOT )
!     &                                         / ( QYZ( LAYS, IWL, IPHOT ) + 1.36E8 * PRESSURE )
! by using the relative correction recommended in NASA JPL (2011).  Note that linearily interpolated
! values are assumed to be at air number density equal to 2.465E19 molecules/cm3. Pressure units are Torrs.
! New correction is based on recommendation in
! http://iupac.pole-ether.fr/htdocs/datasheets/pdf/P6_CH3COCHO+hv.pdf dated 2003
! but places lower limit on pressure of 4 Torrs based on source of recommendation:
!     Y. Chen, W. Wang and L. Zhu, J. Phys. Chem. A, 104 11126 (2000).
                       PRESSURE = 1.03547E-19 * TEMP( LAYS ) * DENS( LAYS )
                       FACTOR   = 2.5524 * TEMP( LAYS ) ! pressure for dens at 2.465e19 molec/cm3
                       IF( WAVELENGTH( IWL ) .LE. 370.0 )THEN
                           PRESSURE = MAX( 400.0, PRESSURE )
                           FACTOR   = MAX( 400.0, FACTOR   )
                       END IF
                       QYZ( LAYS, IWL, IPHOT ) =  QYZ( LAYS, IWL, IPHOT )
     &                                         * ( IPHI0_MGLY( IWL ) + KMGLY( IWL ) * FACTOR )
     &                                         / ( IPHI0_MGLY( IWL ) + KMGLY( IWL ) * PRESSURE )
                       QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                   END DO
               END DO
           CASE (  ACROLEIN )
!***density correction quantum yield
               DO IWL = 1, NWL_REF
                   DO LAYS = 1, NLAYS
!***Number density dependence based on Gardner et. al (1987),
!***  J. Phys. Chem., vol 91, pages 1922. The application uses
!***  the quantum yields set in in cross-section file.
!***  For SAPRC07T CSQY data, yields set approximately four times NASA (2006)
!***  because the mechanism developer sums over all possible channels and
!***  Gardner et. al may support this conclusion.
                     IF ( DENS( LAYS ) .GE. 8.0E+17 ) THEN
                        QYZ( LAYS, IWL, IPHOT ) = 153.5 * QYZ( LAYS, IWL, IPHOT )
     &                                          * ( 4.0E-3 + 1.0 / ( 8.6E-2 + 1.613E-17 * DENS( LAYS ) ) )
                     ELSE IF ( DENS( LAYS ) .LT. 8.0E+17 ) THEN
                        QYZ( LAYS, IWL, IPHOT ) = 12.431 * QYZ( LAYS, IWL, IPHOT )
                     END IF

                     QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                   END DO
               END DO
           CASE ( FORMALDEHYDE_MOLECULAR )  ! 'CH2O -> H2 + CO'
!***temperature/density correction to quantum yield
! based on
! Sander, S. P., Friedl, R. R., Abbatt, J. P. D., Barker, J. R.,
! Burkholder, J. B., Golden, D. M., Kolb, C. E., Kurylo, M. J.,
! Moortgat, G. K., Wine, P. H., Huie, R. E., and Orkin, V. L.:
! Chemical kinetics and photochemical data for use in atmospheric
! studies. Evaluation number 17, JPL-Publication 10-6, Pasadena,
! 2011.
              IF ( LHCHOR_06 .LE. 0 )CYCLE
              DO IWL = 1, NWL_REF
                 IF ( WAVELENGTH( IWL ) .LE. 329.0 ) CYCLE
                 DO LAYS = 1, NLAYS
                    IF ( QYZ( LAYS, IWL, IPHOT ) .LE. 0.0 ) CYCLE

                    IF( QYZ( LAYS, IWL, LHCHOR_06 ) .GT. 0.9999999 )THEN
                        WRITE(6,*)"QYZ( LAYS, IWL, LHCHOR_06 ) = ",QYZ( LAYS, IWL, LHCHOR_06 ) 
                        CALL M3EXIT( 'GET_CSQY', 0, 0, 'CHECK master log for information', -1 )
                    END IF
                         
                    BETA = 1.0 / ( 1.0 - QYZ( LAYS, IWL, LHCHOR_06 ) )
                    IF ( TEMP( LAYS ) .LT. 300.0 .AND. TEMP( LAYS ) .GT. 220.0 ) THEN
                        PRESSURE = 1.36312E-22  * DENS( LAYS ) * TEMP( LAYS )   ! pressure units, atm
                        ALPHA = ( 1.0 / QYZ( LAYS, IWL, IPHOT ) - BETA )
     &                        * ( 1.0 + 0.05 * ( WAVELENGTH( IWL ) - 329.0 )
     &                        * ( ( TEMP( LAYS ) - 80.0 ) * 0.0125 ) )
                    ELSE IF ( TEMP( LAYS ) .LE. 220.0 ) THEN
                        PRESSURE = 3.0E-20 * DENS( LAYS )
                        ALPHA = ( 1.0 / QYZ( LAYS, IWL, IPHOT ) - BETA )
     &                        * ( 1.0 + 0.0875 * ( WAVELENGTH( IWL ) - 329.0 ) )
                    ELSE IF ( TEMP( LAYS ) .GE. 300.0 ) THEN
                        PRESSURE = 4.09E-20 * DENS( LAYS )
                        ALPHA = ( 1.0 / QYZ( LAYS, IWL, IPHOT ) - BETA )
     &                        * ( 1.0 + 0.1375 * ( WAVELENGTH( IWL ) - 329.0 ) )
                    END IF
!***use relative change assuming that air density for interpolated value is 2.465e19 molecules/cm3
                    QYZ( LAYS, IWL, IPHOT ) = QYZ( LAYS, IWL, IPHOT )
     &                                      * ( BETA + 3.3601E-3 * TEMP( LAYS ) * ALPHA ) 
     &                                      / ( BETA + PRESSURE * ALPHA )                    
!      IF ( FIRSTCALL )THEN 
!                WRITE(LOGDEV, 99951)'TEMP, DENS, HCHO_M QYZ factor =  ', ' ',TEMP( LAYS ), DENS( LAYS ),
!     &          ( BETA + 3.3601E-3 * TEMP( LAYS ) * ALPHA )/( BETA + PRESSURE * ALPHA )
!      END IF

                    QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                 END DO
              END DO
           CASE (  ACETONE ) ! 'CH3COCH3 -> products' total quantum yield
!***temperature/density correction to quantum yield
              IF ( IPHOT .EQ. LACETONE ) THEN
!***OPTT is the effective cross section ( Cs * QY )
!***This is an implementation of eq(21) of Cameron-Smith (2000)
!***special information for acetone. Reference:
!***  Cameron-Smith, P., Incorporation of non-linear effective cross section
!***  parameterization into a fast photolysis computation  code (Fast-J)
!***  Journal of Atmospheric Chemistry, Vol. 37, pp 283-297, 2000.
                 FORALL( LAYS = 1:NLAYS, IWL = 1:NWL_REF )
                       CSZ( LAYS, IWL, LACETONE ) = ( ( TTX2 - TEMP( LAYS ) ) * OP0( 1, IWL )
     &                                            + ( TEMP( LAYS ) - TTX1 ) * OP0( 2, IWL ) )
     &                                            / ( DTTX * ( 1.0 + YY30( IWL ) * DENS( LAYS ) ) )
                       QYZ( LAYS, IWL, LACETONE ) = 1.0
                 END FORALL
             ELSE ! approximate relative correction based on effective wavelengths
                 DO IWL = 1, NWL_REF
                      DO LAYS = 1, NLAYS
                        QYZ( LAYS, IWL, IPHOT ) = RQUANTUM_ACETONE( TEMP( LAYS ), DENS( LAYS ), IWL )
     &                                          * QYZ( LAYS, IWL, IPHOT )
                        QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                   END DO
                END DO
             END IF
         CASE (  ACETONE_CH3CO  )
             DO IWL = 1, NWL_REF
                DO LAYS = 1, NLAYS
                   QYZ( LAYS, IWL, IPHOT ) = RQY_ACETONE_CH3CO( TEMP( LAYS ), DENS( LAYS ), IWL )
     &                                     * QYZ( LAYS, IWL, IPHOT )
                   QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                END DO
             END DO
         CASE( GLYOXAL_IUPAC_2013 )
             DO IWL = 1, NWL_REF
                DO LAYS = 1, NLAYS
                   QYZ( LAYS, IWL, IPHOT ) = RQY_GLYOXAL( TEMP( LAYS ), DENS( LAYS ), IWL )
     &                                     * QYZ( LAYS, IWL, IPHOT )
                   QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                END DO
             END DO
         CASE (  KETONE_LEGACY  )
             FORALL( LAYS = 1:NLAYS, IWL = 1:NWL_REF )
                 CSZ( LAYS, IWL, IPHOT ) = CSZ( LAYS, IWL, IPHOT ) / ( 1.0 + 0.80E-19 * DENS( LAYS ) )
             END FORALL
         CASE ( KETONE_RACM2 ) ! Ketone treatement from W. Stockwell sbox
             FORALL( IWL = 1:NWL_REF )
                FORALL( LAYS = 1:NLAYS, QYZ( LAYS, IWL, IPHOT ) .GT. 1.0E-5 .AND. QYZ( LAYS, IWL, IPHOT ) .LT. 0.9999 )
                   QYZ( LAYS, IWL, IPHOT ) =  1.0
     &                                     / (1.0 + 4.057E-20 * DENS( LAYS ) * ( 1.0 / QYZ(LAYS, IWL, IPHOT ) - 1.0 ))
                   QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                END FORALL
             END FORALL
         CASE ( GLYOXAL_RACM2  ) ! glyoxal treatement from W. Stockwell sbox
             FORALL( IWL = 1:NWL_REF )
                FORALL( LAYS = 1:NLAYS, QYZ( LAYS, IWL, IPHOT ) .GT. 1.0E-5 .AND. QYZ( LAYS, IWL, IPHOT ) .LT. 0.9999 )
                   QYZ( LAYS, IWL, IPHOT ) =  1.0
     &                                     / ( 1.0 + 4.057E-20 * DENS( LAYS ) * ( 1.0 / QYZ(LAYS, IWL, IPHOT ) - 1.0 ))
                   QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                END FORALL
             END FORALL
         CASE (  METHYL_GLYOXAL_LEGACY )
             FORALL( LAYS = 1:NLAYS, IWL = 1:NWL_REF )
                   CSZ( LAYS, IWL, IPHOT ) = CSZ( LAYS, IWL, IPHOT ) / ( 1.0 + 1.67E-19 * DENS( LAYS ) )
             END FORALL
         END SELECT

      END DO   ! loop on IPHOT


!      IF ( FIRSTCALL )THEN
!          DO IPHOT = 1, MECHANISM_RATES
!             DO LAYS = 1, NLAYS
!                WRITE(LOGDEV, 99951)'CSZ for ', PHOTOLYSIS_RATE( IPHOT ),(CSZ( LAYS, IWL, IPHOT ),IWL=1,NWL_REF)
!                WRITE(LOGDEV, 99951)'QYZ for ', PHOTOLYSIS_RATE( IPHOT ),(QYZ( LAYS, IWL, IPHOT ),IWL=1,NWL_REF)
!             END DO
!          END DO
!          FIRSTCALL = .FALSE.
!      END IF

99951 FORMAT(A,A16,40(1X,ES12.4))
      RETURN
      END SUBROUTINE GET_CSQY
      SUBROUTINE DEFINE_CSQY_ADJUST()
! Purpose determine whether a photolysis rate uses specific corrections to
! CSQY data
!          Apr  2014 B.Hutzell Initial version
!          Sept 2014 B.Hutzell corrected acetaldehyde case to include CCHO_R_SAPRC99,
!                    the photolysis rate used in original CB05 mechanism

               IMPLICIT NONE

               INTEGER :: IPHOT, IWL

               REAL( 8 ) :: ILAMBDA

               ALLOCATE( CSQY_ADJUST( MECHANISM_RATES ) )

               DO IPHOT = 1, MECHANISM_RATES
                  SELECT CASE ( TRIM( PHOTOLYSIS_RATE( IPHOT ) ) )
                     CASE ( 'CCHO_R', 'CCHO_R_SAPRC99', 'CH3CHO_RACM2', 'ALD2_R_IUPAC10',
     &                      'ALD2_R_IUPAC13' )                                             ! 'CH3CHO -> CH3 + HCO'
                        CSQY_ADJUST( IPHOT ) = ACETALDEHYDE
                     CASE ( 'CCHO_R1_MCMv32', 'CCHO_R2_MCMv32' )      ! 'CH3CHO -> CH3 + HCO'
                        CSQY_ADJUST( IPHOT ) = CH3CHO_IUPAC2013
                        IF( .NOT. ALLOCATED( KCH3CHO ) ) ALLOCATE( KCH3CHO( NWL_REF ) )
                        IF( .NOT. ALLOCATED( IPHI0_CH3CHO ) ) ALLOCATE( IPHI0_CH3CHO( NWL_REF ) )
                        IF( .NOT. ALLOCATED( IPHIS_CH3CHO ) ) ALLOCATE( IPHIS_CH3CHO( NWL_REF ) )
                        DO IWL = 1, NWL_REF
                           IF(  WAVELENGTH( IWL ) .LE. 608.0 )THEN
                             ILAMBDA = REAL( 1.0D0 / WAVELENGTH( IWL ), 8 )
                             KCH3CHO( IWL )   = REAL( 1.0D-21
     &                                        * (1.04D27*EXP(-1.792D4*ILAMBDA) + 1.48D6*EXP(-3.211D3*ILAMBDA)))
                             IPHI0_CH3CHO( IWL ) = 1.0 + EXP( 0.2627801*(  WAVELENGTH( IWL ) - 320.56 ) )
                             IPHIS_CH3CHO( IWL ) = IPHI0_CH3CHO( IWL ) + 2.465E19 *  KCH3CHO( IWL )
                           ELSE ! use values for wavelength equals 608 nm
                             KCH3CHO( IWL )      = 1.64731E-07
                             IPHI0_CH3CHO( IWL ) = 5.50947E+32
                             IPHIS_CH3CHO( IWL ) = 5.50946E+32
                           END IF                               
!                WRITE(LOGDEV, 99951)'DENS, CH3CHO_IUPAC2013 QYZ factor =  ', ' ',2.465E19,
!     &          IPHIS_CH3CHO( IWL ),IPHI0_CH3CHO( IWL ),KCH3CHO( IWL )
                        END DO
                     CASE ( 'NC3CHO_R_MCMv32', 'NC3CHO_M_MCMv32' ) ! n-C3H7CHO (n-butyraldehyde, n-butanal)
                        CSQY_ADJUST( IPHOT ) = NBUTYRALDEHYDE
                     CASE ( 'C2CHO', 'ALD_RACM2', 'BALD_RACM2', 'UALD_RACM2', 'ALDX_R_IUPAC10', 'ALDX_R_IUPAC13'  )  ! C3 and higher aldehydes
                        CSQY_ADJUST( IPHOT ) = HIGHER_ALDEHYDES
                     CASE ( 'MVK_06' )
                        CSQY_ADJUST( IPHOT ) = METHYL_VINYL_KETONE
                     CASE ( 'MACR_06', 'MACR_RACM2', 'MACR_MCMv32' )
                        CSQY_ADJUST( IPHOT ) = METHYL_ACROLEIN
                     CASE ( 'MEK_06', 'MEK_MCMv32' )
                        CSQY_ADJUST( IPHOT ) = METHYL_ETHYL_KETONE
                     CASE ( 'MGLY_06' , 'BACL_07', 'MGLY_IUPAC04', 'MGLY_IUPAC10' )
                        CSQY_ADJUST( IPHOT ) = METHYL_GLYOXAL_IUPAC04
                        IF( .NOT. ALLOCATED( KMGLY ) ) ALLOCATE( KMGLY( NWL_REF ) )
                        IF( .NOT. ALLOCATED( IPHI0_MGLY ) ) ALLOCATE( IPHI0_MGLY( NWL_REF ) )
                        WHERE( WAVELENGTH .LT. 380.41 )
                            IPHI0_MGLY = 1.0
                            KMGLY      = 7.08021E-03
                        ELSE WHERE ! maximum value correspond to wavelength = 711.8954 nm
                            IPHI0_MGLY =  MAX( 2.755E+06 * EXP( -5639.0 / WAVELENGTH ), 1.0E+03 )
                            KMGLY       = MAX(  1.93E+04 * EXP( -5639.0 / WAVELENGTH ), 7.00590 )
                        END WHERE
                     CASE (  'ACRO_09' )
                        CSQY_ADJUST( IPHOT ) = ACROLEIN
                     CASE (  'BIACET_MCMv32' )
                        CSQY_ADJUST( IPHOT ) = BIACETYL
                     CASE ( 'HCHO_M_MCMv32', 'HCHOM_06', 'HCHO_M_SAPRC99', 'HCHO_MOL_RACM2', 'FORM_M_IUPAC10',
     &                      'FORM_M_IUPAC13', 'HCHO_MOL_JPL19' )                ! 'CH2O -> H2 + CO'
                        CSQY_ADJUST( IPHOT ) = FORMALDEHYDE_MOLECULAR
                     CASE (  'ACET_06', 'ACETONE', 'CH3COCH3_RACM2', 'ACET_IUPAC10' ) ! 'CH3COCH3 -> products'
                        CSQY_ADJUST( IPHOT ) = ACETONE
                     CASE (  'KETONE'  )
                        CSQY_ADJUST( IPHOT ) = KETONE_LEGACY
                     CASE ( 'KET_RACM2', 'KET_JGR19', 'HKET_RACM2', 'MEK_RACM2', 'MEK_JGR19', 'MVK_RACM2' ) ! Ketone treatement from W. Stockwell sbox
                        CSQY_ADJUST( IPHOT ) = KETONE_RACM2
                     CASE ( 'GLYH2_RACM2', 'GLYF_RACM2', 'GLYHX_RACM2', 'MGLY_RACM2'  ) ! glyoxal treatement from W. Stockwell sbox
                        CSQY_ADJUST( IPHOT ) = GLYOXAL_RACM2
                     CASE ( 'GLYOX_R_CRI', 'GLYOX_M_CRI', 'GLY_R_IUPAC10', 'GLY_R_IUPAC13'  ) ! glyoxal treatement based on IUPAC 2013 datasheet
                        CSQY_ADJUST( IPHOT ) = GLYOXAL_IUPAC_2013
                     CASE (  'MGLY_ADJ',  'MGLY_ABS' )
                        CSQY_ADJUST( IPHOT ) = METHYL_GLYOXAL_LEGACY
                     CASE ( 'ACET_R2_CRI', 'CH3COCH3A_JPL19' )
                        CSQY_ADJUST( IPHOT ) = ACETONE_CH3CO
                     CASE DEFAULT
                        CSQY_ADJUST( IPHOT ) =  0
                     END SELECT
!                      WRITE(LOGDEV,'(A)')'For rate ' // TRIM( PHOTOLYSIS_RATE( IPHOT ) ) 
!     &               // ' using CSQY_ADJUSTMENT ' // TRIM( CSQY_ADJUSTMENTS( CSQY_ADJUST( IPHOT ) ) )
               END DO
              RETURN
99951 FORMAT(A9,A16,40(1X,ES12.4))
      END SUBROUTINE DEFINE_CSQY_ADJUST
      REAL FUNCTION QUANTUM_ACETONE( TEMP, DENS_NUMB, LAMBDA )

C-----------------------------------------------------------------------
C Computes acetone quantum yields according to:
C IUPAC (2013) recommendation based on
C Blitz, M. A., D. E. Heard, M. J. Pilling, S. R. Arnold, and M. P. Chipperfield
C       (2004), Pressure and temperature-dependent quantum yields for the
C       photodissociation of acetone between 279 and 327.5 nm, Geophys.
C       Res. Lett., 31, L06111, doi:10.1029/2003GL018793.
C-----------------------------------------------------------------------

      IMPLICIT NONE

!***arguments

      REAL, INTENT(IN) :: TEMP        ! air temperature, K
      REAL, INTENT(IN) :: DENS_NUMB   ! air number density, 1/cm^3
      REAL, INTENT(IN) :: LAMBDA      ! wavelength, nm

!***local
      REAL, PARAMETER  :: ONE_OVER_295K  = 1.0 / 295  ! 1/K

      REAL A0           ! 1st coef for qy
      REAL A1           ! 2nd coef for qy
      REAL A2           ! 3rd coef for qy
      REAL A3           ! 4th coef for qy
      REAL A4           ! 5th coef for qy

      REAL PHI_CO       ! CO branch of IUPAC (2013) acetone QYZ
      REAL PHI_CH3CO    ! CH3CO branch of IUPAC (2013) acetone QYZ
      REAL AA           ! scratch variable for IUPAC (2013) acetone QYZ
      REAL BB           ! scratch variable for IUPAC (2013) acetone QYZ
      REAL CC           ! scratch variable for IUPAC (2013) acetone QYZ

      REAL TEMP_OVER_295K   ! temperature divided by 295 K
      REAL ONE_OVER_LAMBDA  ! reciprocal of wavelength, 1E7/nm or 1/cm

      TEMP_OVER_295K  = TEMP * ONE_OVER_295K
      ONE_OVER_LAMBDA = 1.0E7 / LAMBDA

      IF ( LAMBDA .GE. 248.0 .AND. LAMBDA .LE. 349.0 ) THEN

         AA = 0.350 * ( TEMP_OVER_295K )**(-1.28)
         BB = 0.068 * ( TEMP_OVER_295K )**(-2.65)
         A0 = ( AA / ( 1.0 - AA ) ) * EXP( BB * ( LAMBDA - 248.0 ) )
         PHI_CO = 1.0 / ( 1.0 + A0 )

         IF ( LAMBDA .LE. 302.0 ) THEN

!***wavelengths 248-302 nm

            AA = 1.600E-19 * ( TEMP_OVER_295K )**(-2.38)
            BB =  0.55E-03 * ( TEMP_OVER_295K )**(-3.19)
            A1 = AA * EXP( -BB * ( ( ONE_OVER_LAMBDA ) - 33113.0 ) )
            PHI_CH3CO = ( 1.0 - PHI_CO ) / ( 1.0 + A1*DENS_NUMB )

!***wavelengths 302-349 nm

         ELSE

            AA = 1.62E-17 * ( TEMP_OVER_295K )**(-10.03)
            BB = 1.79E-03  * ( TEMP_OVER_295K )**(-1.364)
            A2 = AA * EXP( -BB * ( ( ONE_OVER_LAMBDA ) - 30488.0 ) )

            AA = 26.29 * ( TEMP_OVER_295K )**(-6.59)
            BB = 5.72E-7 * ( TEMP_OVER_295K )**(-2.93)
            CC = ( 30006.0 )   * ( TEMP_OVER_295K )**(-0.064)
            A3 = AA * EXP( -BB * ( ( ONE_OVER_LAMBDA ) - CC )**2.0 )

            AA = 1.67E-15 * ( TEMP_OVER_295K )**(-7.25)
            BB = 2.08E-03  * ( TEMP_OVER_295K )**(-1.16)
            A4 = AA * EXP( -BB * ( ( ONE_OVER_LAMBDA ) - 30488.0 ) )

            PHI_CH3CO = ( 1.0 - PHI_CO )
     &                * ( 1.0 + A4 * DENS_NUMB + A3 )
     &                / ( ( 1.0 + A2 * DENS_NUMB + A3 )
     &                *   ( 1.0 + A4 * DENS_NUMB ) )
         END IF

         QUANTUM_ACETONE = MAX( 0.0, MIN( 1.0, (PHI_CO+PHI_CH3CO) ) )

      ELSE IF ( LAMBDA .LT. 248.0 .AND. LAMBDA .GT. 0.0 ) THEN ! set QY to 1.0

!***based on IUPAC (2013) data sheet

         QUANTUM_ACETONE = 1.0

      ELSE IF ( LAMBDA .GT. 349.0 ) THEN

         QUANTUM_ACETONE = 0.0

      END IF


      RETURN
      END FUNCTION QUANTUM_ACETONE
      SUBROUTINE QY_ACETONE_CHANNELS( TEMP, DENS_NUMB, LAMBDA, PHI_CO, PHI_CH3CO )

C-----------------------------------------------------------------------
C Computes acetone quantum yields according to:
C IUPAC (2013) recommendation based on
C Blitz, M. A., D. E. Heard, M. J. Pilling, S. R. Arnold, and M. P. Chipperfield
C       (2004), Pressure and temperature-dependent quantum yields for the
C       photodissociation of acetone between 279 and 327.5 nm, Geophys.
C       Res. Lett., 31, L06111, doi:10.1029/2003GL018793.
C-----------------------------------------------------------------------

      IMPLICIT NONE

!***arguments

      REAL, INTENT(IN)  :: TEMP        ! air temperature, K
      REAL, INTENT(IN)  :: DENS_NUMB   ! air number density, 1/cm^3
      REAL, INTENT(IN)  :: LAMBDA      ! wavelength, nm
      REAL, INTENT(OUT) :: PHI_CO      ! CO branch of IUPAC (2013) acetone QYZ
      REAL, INTENT(OUT) :: PHI_CH3CO   ! CH3CO branch of IUPAC (2013) acetone QYZ

!***local
      REAL, PARAMETER  :: ONE_OVER_295K  = 1.0 / 295.0  ! 1/K

      REAL A0           ! 1st coef for qy
      REAL A1           ! 2nd coef for qy
      REAL A2           ! 3rd coef for qy
      REAL A3           ! 4th coef for qy
      REAL A4           ! 5th coef for qy

      REAL AA           ! scratch variable for IUPAC (2013) acetone QYZ
      REAL BB           ! scratch variable for IUPAC (2013) acetone QYZ
      REAL CC           ! scratch variable for IUPAC (2013) acetone QYZ

      REAL TEMP_OVER_295K   ! temperature divided by 295 K
      REAL ONE_OVER_LAMBDA  ! reciprocal of wavelength, 1E7/nm or 1/cm

      TEMP_OVER_295K  = TEMP * ONE_OVER_295K
      ONE_OVER_LAMBDA = 1.0E7 / LAMBDA

      IF ( LAMBDA .GE. 248.0 .AND. LAMBDA .LE. 349.0 ) THEN

         AA = 0.350 * ( TEMP_OVER_295K )**(-1.28)
         BB = 0.068 * ( TEMP_OVER_295K )**(-2.65)
         A0 = ( AA / ( 1.0 - AA ) ) * EXP( BB * ( LAMBDA - 248.0 ) )
         PHI_CO = 1.0 / ( 1.0 + A0 )

         PHI_CO = MAX( 0.0, MIN( 1.0, PHI_CO ) )

         IF ( LAMBDA .LE. 302.0 ) THEN

!***wavelengths 248-302 nm

            AA = 1.60E-19 * ( TEMP_OVER_295K )**(-2.38)
            BB = 0.55E-03 * ( TEMP_OVER_295K )**(-3.19)
            A1 = AA * EXP( -BB * ( ( ONE_OVER_LAMBDA ) - 33113.0 ) )
            PHI_CH3CO = ( 1.0 - PHI_CO ) / ( 1.0 + A1*DENS_NUMB )

!***wavelengths 302-349 nm

         ELSE

            AA = 1.62E-17 * ( TEMP_OVER_295K )**(-10.03)
            BB = 1.79E-03  * ( TEMP_OVER_295K )**(-1.364)
            A2 = AA * EXP( -BB * ( ( ONE_OVER_LAMBDA ) - 30488.0 ) )

            AA = 26.29 * ( TEMP_OVER_295K )**(-6.59)
            BB = 5.72E-7 * ( TEMP_OVER_295K )**(-2.93)
            CC = 30006.0 * ( TEMP_OVER_295K )**(-0.064)
            A3 = AA * EXP( -BB * ( ( ONE_OVER_LAMBDA ) - CC )**2.0 )

            AA = 1.67E-15 * ( TEMP_OVER_295K )**(-7.25)
            BB = 2.08E-03  * ( TEMP_OVER_295K )**(-1.16)
            A4 = AA * EXP( -BB * ( ( ONE_OVER_LAMBDA ) - 30488.0 ) )

            PHI_CH3CO = ( 1.0 - PHI_CO )
     &                * ( 1.0 + A4 * DENS_NUMB + A3 )
     &                / ( ( 1.0 + A2 * DENS_NUMB + A3 )
     &                *   ( 1.0 + A4 * DENS_NUMB ) )
         END IF

         PHI_CH3CO = MAX( 0.0, MIN( 1.0, PHI_CH3CO ) )

      ELSE IF ( LAMBDA .LT. 248.0 .AND. LAMBDA .GT. 0.0 ) THEN ! set QY to 1.0

!***based on IUPAC (2013) data sheet

         PHI_CO    = 0.45
         PHI_CH3CO = 0.55

      ELSE IF ( LAMBDA .GT. 349.0 ) THEN

         PHI_CO    = 0.0
         PHI_CH3CO = 0.0

      END IF

      RETURN
      END SUBROUTINE QY_ACETONE_CHANNELS
#ifdef preprocessor
      REAL FUNCTION RQY_ACETONE_CH3CO( TEMP, DENS_NUMB, LAMBDA )
#else
      REAL FUNCTION RQY_ACETONE_CH3CO( TEMP, DENS_NUMB, IWAVE )
#endif
C-----------------------------------------------------------------------
C Computes correction to acetone CH3CO quantum yields at (TEMP, DENS_NUMB)  relative to
C quantum yields at (TEMP, DENS_NUMB = 2.46E19) according to:
C IUPAC (2013) recommendation based on
C Blitz, M. A., D. E. Heard, M. J. Pilling, S. R. Arnold, and M. P. Chipperfield
C       (2004), Pressure and temperature-dependent quantum yields for the
C       photodissociation of acetone between 279 and 327.5 nm, Geophys.
C       Res. Lett., 31, L06111, doi:10.1029/2003GL018793.
C-----------------------------------------------------------------------

      IMPLICIT NONE

!***arguments

      REAL,    INTENT(IN)  :: TEMP        ! air temperature, K
      REAL,    INTENT(IN)  :: DENS_NUMB   ! air number density, molcules/cm^3
#ifdef preprocessor
      REAL,     INTENT(IN) :: LAMBDA      ! wavelength, nm
#else
      INTEGER, INTENT(IN)  :: IWAVE       !  wavelength index
#endif

!***local
      REAL, PARAMETER  :: ONE_OVER_295K  = 1.0 / 295.0  ! 1/K
      REAL, PARAMETER  :: DENS0          = 2.46E19      ! air number at STP, molecules/cm^3

      REAL A0           ! 1st coef for qy
      REAL A1           ! 2nd coef for qy
      REAL A2           ! 3rd coef for qy
      REAL A3           ! 4th coef for qy
      REAL A4           ! 5th coef for qy

      REAL PHI_CO       ! CO qy  at (TEMP, DENS_NUMB)
      REAL PHI_CH3CO    ! CH3CO qy at (TEMP, DENS_NUMB)
      REAL PHI_COS      ! CO qy branch at (TEMP, DENS0)
      REAL PHI_CH3COS   ! inverse of CH3CO qy at (TEMP, DENS0)
      REAL AA           ! scratch variable for acetone QY
      REAL BB           ! scratch variable for acetone QY
      REAL CC           ! scratch variable for acetone QY

      REAL TEMP_OVER_295K   ! temperature divided by 295 K
      REAL ONE_OVER_LAMBDA  ! wavenumber, 10E7/nm or 1/cm
#ifndef preprocessor
      REAL LAMBDA           ! wavelength, nm

      LAMBDA          = WAVELENGTH( IWAVE )
      ONE_OVER_LAMBDA = WAVENUMBER( IWAVE )
#else
      ONE_OVER_LAMBDA = 1.0E7 / LAMBDA
#endif

      TEMP_OVER_295K  = TEMP * ONE_OVER_295K

      IF ( LAMBDA .GE. 248.0 .AND. LAMBDA .LE. 349.0 ) THEN

         IF ( LAMBDA .LE. 302.0 ) THEN

!***wavelengths 248-302 nm

            AA = 1.60E-19 * ( TEMP_OVER_295K )**(-2.38)
            BB = 0.55E-03 * ( TEMP_OVER_295K )**(-3.19)
            A1 = AA * EXP( -BB * ( ONE_OVER_LAMBDA - 33113.0 ) )
            RQY_ACETONE_CH3CO = ( 1.0 + A1*DENS0 ) / ( 1.0 + A1*DENS_NUMB )

!***wavelengths 302-349 nm

         ELSE

            AA = 1.62E-17 * ( TEMP_OVER_295K )**(-10.03)
            BB = 1.79E-03  * ( TEMP_OVER_295K )**(-1.364)
            A2 = AA * EXP( -BB * ( ONE_OVER_LAMBDA - 30488.0 ) )

            AA = 26.29 * ( TEMP_OVER_295K )**(-6.59)
            BB = 5.72E-7 * ( TEMP_OVER_295K )**(-2.93)
            CC = 30006.0 * ( TEMP_OVER_295K )**(-0.064)
            A3 = AA * EXP( -BB * ( ONE_OVER_LAMBDA - CC )**2.0 )

            AA = 1.67E-15 * ( TEMP_OVER_295K )**(-7.25)
            BB = 2.08E-03  * ( TEMP_OVER_295K )**(-1.16)
            A4 = AA * EXP( -BB * ( ONE_OVER_LAMBDA - 30488.0 ) )

!***below qy_ch3co values are normalized by (1 - qy_co) which does not depend on
!***number density
            PHI_CH3CO  = ( 1.0 + A4 * DENS_NUMB + A3 )
     &                 / ( ( 1.0 + A2 * DENS_NUMB + A3 ) * ( 1.0 + A4 * DENS_NUMB ) )

            PHI_CH3COS = ( ( 1.0 + A2 * DENS0 + A3 ) * ( 1.0 + A4 * DENS0 ) )
     &                 / ( 1.0 + A4 * DENS0 + A3 )

            RQY_ACETONE_CH3CO = PHI_CH3CO * PHI_CH3COS

         END IF

      ELSE ! set RQY to 1.0 for 248.0 > LAMBDA or LAMBDA 349.0

         RQY_ACETONE_CH3CO = 1.0

      END IF

      RETURN
      END FUNCTION RQY_ACETONE_CH3CO
#ifdef preprocessor
      REAL FUNCTION RQUANTUM_ACETONE( TEMP, DENS_NUMB, LAMBDA )
#else
      REAL FUNCTION RQUANTUM_ACETONE( TEMP, DENS_NUMB, IWAVE )
#endif


C-----------------------------------------------------------------------
C Computes total acetone quantum yields at (TEMP, DENS_NUMB)  relative to
C quantum yields at (TEMP, DENS_NUMB = 2.46E19) according to
C IUPAC (2013) recommendation based on
C Blitz, M. A., D. E. Heard, M. J. Pilling, S. R. Arnold, and M. P. Chipperfield
C       (2004), Pressure and temperature-dependent quantum yields for the
C       photodissociation of acetone between 279 and 327.5 nm, Geophys.
C       Res. Lett., 31, L06111, doi:10.1029/2003GL018793.
C-----------------------------------------------------------------------

      IMPLICIT NONE

!***arguments

      REAL,    INTENT(IN) :: TEMP        ! air temperature, K
      REAL,    INTENT(IN) :: DENS_NUMB   ! air number density, 1/cm^3
#ifdef preprocessor
      REAL,    INTENT(IN) :: LAMBDA      ! wavelength, nm
#else
      INTEGER, INTENT(IN) :: IWAVE       !  wavelength index
#endif

!***local
      REAL, PARAMETER  :: ONE_OVER_295K  = 1.0 / 295.0  ! 1/K
      REAL, PARAMETER  :: DENS0          = 2.46E19      ! air number at STP, molecules/cm^3

      REAL A0           ! 1st coef for qy
      REAL A1           ! 2nd coef for qy
      REAL A2           ! 3rd coef for qy
      REAL A3           ! 4th coef for qy
      REAL A4           ! 5th coef for qy

      REAL PHI_CO       ! CO branch of IUPAC (2013) acetone QYZ
      REAL DEL_PHI_CO   ! one minus CO branch of IUPAC (2013) acetone QYZ
      REAL PHI_CH3CO    ! CH3CO branch of IUPAC (2013) acetone QYZ
      REAL PHI_CH3CO0   ! CH3CO branch of IUPAC (2013) acetone QYZ at DENS0
      REAL AA           ! scratch variable for IUPAC (2013) acetone QYZ
      REAL BB           ! scratch variable for IUPAC (2013) acetone QYZ
      REAL CC           ! scratch variable for IUPAC (2013) acetone QYZ

      REAL TEMP_OVER_295K   ! temperature divided by 295 K
      REAL ONE_OVER_LAMBDA  ! reciprocal of wavelength, 1E7/nm or 1/cm
#ifndef preprocessor
      REAL LAMBDA           ! wavelength, nm

      LAMBDA          = WAVELENGTH( IWAVE )
      ONE_OVER_LAMBDA = WAVENUMBER( IWAVE )
#else
      ONE_OVER_LAMBDA = 1.0E7 / LAMBDA
#endif

      TEMP_OVER_295K  = TEMP * ONE_OVER_295K

      IF ( LAMBDA .GE. 248.0 .AND. LAMBDA .LE. 349.0 ) THEN

         AA = 0.350 * ( TEMP_OVER_295K )**(-1.28)
         BB = 0.068 * ( TEMP_OVER_295K )**(-2.65)
         A0 = ( AA / ( 1.0 - AA ) ) * EXP( BB * ( LAMBDA - 248.0 ) )
         PHI_CO     = 1.0 / ( 1.0 + A0 )
         DEL_PHI_CO = MAX(0.0, 1.0 - PHI_CO)

         IF ( LAMBDA .LE. 302.0 ) THEN

!***wavelengths 248-302 nm

            AA = 1.600E-19 * ( TEMP_OVER_295K )**(-2.38)
            BB =  0.55E-03 * ( TEMP_OVER_295K )**(-3.19)
            A1 = AA * EXP( -BB * ( ( ONE_OVER_LAMBDA ) - 33113.0 ) )
            PHI_CH3CO  = DEL_PHI_CO / ( 1.0 + A1*DENS_NUMB )
            PHI_CH3CO0 = DEL_PHI_CO / ( 1.0 + A1*DENS0 )

!***wavelengths 302-349 nm

         ELSE

            AA = 1.62E-17 * ( TEMP_OVER_295K )**(-10.03)
            BB = 1.79E-03  * ( TEMP_OVER_295K )**(-1.364)
            A2 = AA * EXP( -BB * ( ONE_OVER_LAMBDA - 30488.0 ) )

            AA = 26.29 * ( TEMP_OVER_295K )**(-6.59)
            BB = 5.72E-7 * ( TEMP_OVER_295K )**(-2.93)
            CC = ( 30006.0 )   * ( TEMP_OVER_295K )**(-0.064)
            A3 = AA * EXP( -BB * ( ONE_OVER_LAMBDA - CC )**2.0 )

            AA = 1.67E-15 * ( TEMP_OVER_295K )**(-7.25)
            BB = 2.08E-03  * ( TEMP_OVER_295K )**(-1.16)
            A4 = AA * EXP( -BB * ( ONE_OVER_LAMBDA - 30488.0 ) )

            PHI_CH3CO = DEL_PHI_CO
     &                * ( 1.0 + A4 * DENS_NUMB + A3 )
     &                / ( ( 1.0 + A2 * DENS_NUMB + A3 )
     &                *   ( 1.0 + A4 * DENS_NUMB ) )

            PHI_CH3CO0 = DEL_PHI_CO
     &                * ( 1.0 + A4 * DENS0 + A3 )
     &                / ( ( 1.0 + A2 * DENS0 + A3 )
     &                *   ( 1.0 + A4 * DENS0 ) )

         END IF

         IF( (PHI_CO + PHI_CH3CO0) .GT. 1.0E-10 )THEN
             RQUANTUM_ACETONE = ( PHI_CO + PHI_CH3CO ) / ( PHI_CO + PHI_CH3CO0 )
         ELSE
             RQUANTUM_ACETONE = 1.0
         END IF

      ELSE

         RQUANTUM_ACETONE = 1.0

      END IF


      RETURN
      END FUNCTION RQUANTUM_ACETONE
#ifdef preprocessor
      REAL FUNCTION RQY_GLYOXAL( TEMP, DENS_NUMB, LAMBDA )
#else
      REAL FUNCTION RQY_GLYOXAL( TEMP, DENS_NUMB, IWAVE )
#endif

!-----------------------------------------------------------------------
! Computes total glyoxal (CHOCHO) quantum yield at (TEMP, DENS_NUMB)
! relative to total yield at (TEMP0, DENS_NUMB = 2.46E19) according to
!  IUPAC (2013) recommendation
!  http://iupac.pole-ether.fr/htdocs/datasheets/pdf/P4_%28CHO%292+hv.pdf
! that is based on
!  Salter, R. J., Blitz, M. A., Heard, D. E., Kovacs, T., Pilling, M. J.,
!  Rickard, A. R. and Seakins, P. W. (2013), Quantum yields for the photolysis
!  of glyoxal below 350 nm and parameterisations for its photolysis rate in
!  the troposphere, Phys. Chem. Chem. Phys., 15, 4984-4994,
!  doi:10.1039/c3cp43597k.
!-----------------------------------------------------------------------

        IMPLICIT NONE

!***arguments
        REAL,  INTENT(IN) :: TEMP        ! air temperature, K
        REAL,  INTENT(IN) :: DENS_NUMB   ! air number density, 1/cm^3
#ifdef preprocessor
      REAL,    INTENT(IN) :: LAMBDA      ! wavelength, nm
#else
      INTEGER, INTENT(IN) :: IWAVE       !  wavelength index
#endif
!***local
        REAL,      PARAMETER  :: ONE_OVER_295K  = 1.0 / 295.0  ! 1/K
        REAL( 8 ), PARAMETER  :: DENS0          = 2.46D19      ! air number at STP, molecules/cm^3

        REAL( 8 ) TEMP_OVER_295K   ! temperature divided by 295 K

        REAL( 8 ) AA           ! scratch variable for qy
        REAL( 8 ) BB           ! scratch variable for qy
        REAL( 8 ) A1           ! 2nd coef for qy
        REAL( 8 ) A2           ! 3rd coef for qy
        REAL( 8 ) A3           ! 4th coef for qy
        REAL( 8 ) WN_OFFSET    ! adjusted wavenumber, 1/cm
        REAL( 8 ) R8_DENS      ! air number density, 1/cm^3

        REAL ONE_OVER_LAMBDA  ! wavenumber, 10E7/nm or 1/cm
        REAL QY_DENS_NUMB     ! total qy at DENS_NUMB
        REAL IQY_DENS0        ! reciprocal of total qy at DENS0
#ifndef preprocessor
        REAL LAMBDA           ! wavelength, nm

        LAMBDA    = WAVELENGTH( IWAVE )
        WN_OFFSET = REAL( WAVENUMBER( IWAVE ) - 23800.0, 8 )
#else
        WN_OFFSET = REAL( 1.0E7 / LAMBDA - 23800.0, 8 )
#endif

        TEMP_OVER_295K  = REAL( TEMP * ONE_OVER_295K, 8 )
        R8_DENS         = REAL( DENS_NUMB, 8 )


         IF( LAMBDA .LT. 460.0 .AND. LAMBDA .GT. 250.0 )THEN
            AA = 6.48D-19 * TEMP_OVER_295K**(-1.83D0)
            BB = 7.60D-04 * TEMP_OVER_295K**(-0.515D0)
            A1 = AA * EXP( -BB * WN_OFFSET )

            AA = 1.128D02 * TEMP_OVER_295K**(-1.53D0)
            BB = 4.61D-03 * TEMP_OVER_295K**(-0.507D0)
            A2 = AA * EXP( -BB * WN_OFFSET  )

            AA = 2.25D-16 * TEMP_OVER_295K**(-9.18D0)
            BB = 7.80D-04 * TEMP_OVER_295K**(-7.03D0)
            A3 = AA * EXP( -BB * WN_OFFSET )

!*** note that values are normalized by total qy at DENS = 0.0 but
!    its values equals 1.0

            IQY_DENS0 = REAL( (1.0 + A2 + A3*DENS0 )
     &                / ((1.0 + A1*DENS0 + A2)*(1.0 + A3*DENS0)) )

            QY_DENS_NUMB  = REAL( (1.0 + A2 + A3*R8_DENS )
     &                / ((1.0 + A1*R8_DENS + A2)*(1.0 + A3*R8_DENS)) )


            RQY_GLYOXAL  = QY_DENS_NUMB * IQY_DENS0

          ELSE

            RQY_GLYOXAL  = 1.0

          END IF

          RETURN
       END FUNCTION RQY_GLYOXAL
      END MODULE CSQY_DATA
