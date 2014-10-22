
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

      INTEGER, PARAMETER, PUBLIC :: NUM_REFRACTIVE = 5

      TYPE MODAL_COMPLEX
         CHARACTER( 16 ) :: NAME                           ! name of complex property
         REAL, ALLOCATABLE, DIMENSION( :, : ) :: REAL_PART ! real part
         REAL, ALLOCATABLE, DIMENSION( :, : ) :: IMAG_PART ! imaginary part
      END TYPE MODAL_COMPLEX

      TYPE( MODAL_COMPLEX ), PUBLIC :: REFRACTIVE_INDEX( NUM_REFRACTIVE )

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

            CHARACTER( 32 ) :: CSQY_ADJUSTMENTS( 0:13 )
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
     &                             'METHYL_GLYOXAL_LEGACY'  /

! integer pointer for density correction to a photolysis rates' cross-section 
! and/or quantum yield values            
            INTEGER, ALLOCATABLE :: CSQY_ADJUST( : ) 
            
            REAL,    ALLOCATABLE :: KMGLY( : )         ! Quenching Coefficient for Methyl Glyoxal Quantum Yields
             
             


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
      
      INTEGER, PRIVATE :: CSQY_DATA_LOG
      
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
      INTEGER :: LOG_UNIT
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

      LOG_UNIT = INIT3()
      CSQY_DATA_LOG = INIT3()
      
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
      write( log_unit,'(A,a32)' )'JTABLE_REF = ',trim(jtable_ref)
      write( log_unit,'(A,10x,i4)' )'NPHOT_MAP = ', nphot_map
#endif

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

      ALLOCATE( PNAME_MAP( NPHOT_MAP ) )
      ALLOCATE( PNAME_REF( NPHOT_MAP ) )
      ALLOCATE( PHOT_MAP ( NPHOT_MAP ) )

      DO IPHOT_LOAD = 1, NPHOT_MAP
         READ( PHOTAB_UNIT,'(A16)' ) PNAME_REF( IPHOT_LOAD )

#ifdef verbose_phot
         write( log_unit,'(i3,1x,a16)' ) iphot_load, pname_ref( iphot_load )
#endif

         PNAME_MAP( IPHOT_LOAD ) = PNAME_REF( IPHOT_LOAD )
         PHOT_MAP ( IPHOT_LOAD ) = IPHOT_LOAD
      END DO

      READ( PHOTAB_UNIT,'(10X,I3)' ) NTEMP_REF

#ifdef verbose_phot
      write( log_unit,'(10x,i3)' ) ntemp_ref
#endif

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

#ifdef verbose_phot
      write( log_unit,* ) trim( file_line )
#endif

      IREFTEMPS = NTEMP_REF

      ALLOCATE( TEMP_BASE( NTEMP_REF ) )

      DO ITT_LOAD = 1, NTEMP_REF
         READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

#ifdef verbose_phot
         write( log_unit,* ) trim( file_line )
#endif

         READ( FILE_LINE,* ) IPHOT_LOAD, TEMP_BASE( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,'(4x,f6.2)' ) temp_base( itt_load )
#endif

      END DO

      ALLOCATE( TEMP_REF( NTEMP_REF, NPHOT_MAP) )

      DO ITT_LOAD = 1, 15 ! skip next 15 lines
         READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( log_unit, '(I2,1X,A)' )ITT_LOAD,TRIM(FILE_LINE)
#endif
      END DO

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

#ifdef verbose_phot
      write( log_unit, '(A)' )TRIM(FILE_LINE)
#endif
      READ( FILE_LINE, 4999) NWL_REF

#ifdef verbose_phot
      write( log_unit,'(17x,i3)' ) nwl_ref
#endif

      
4999  FORMAT(17X,I3,2X,17X,I3)

      
      NWL       = NWL_REF
      NWL_PHOT  = NWL


      IF ( NWL_REF .LT. 1 ) THEN
         WRITE( LOG_UNIT,* ) 'NWL_REF  = ', NWL_REF
         MSG = 'NWL_REF in ' // CSQY_FILE
     &       // ' has the bad value, written above. '
         CALL M3EXIT( PNAME, 0, 0, MSG, -1 )
      END IF

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( log_unit, * )FILE_LINE
#endif

      IF( .NOT. ALLOCATED( STWL_REF   ) ) ALLOCATE( STWL_REF  ( NWL_REF ) )
      IF( .NOT. ALLOCATED( ENDWL_REF  ) ) ALLOCATE( ENDWL_REF ( NWL_REF ) )
      IF( .NOT. ALLOCATED( FSOLAR_REF ) ) ALLOCATE( FSOLAR_REF( NWL_REF ) )
      IF( .NOT. ALLOCATED( EFFWL_REF  ) ) ALLOCATE( EFFWL_REF ( NWL_REF ) )
      IF( .NOT. ALLOCATED( FEXT       ) ) ALLOCATE( FEXT      ( NWL_REF ) )
      IF( .NOT. ALLOCATED( WAVELENGTH ) ) ALLOCATE( WAVELENGTH( NWL_REF ) )

      DO IWL_LOAD = 1, NWL_REF
!         READ( PHOTAB_UNIT,'(4X,3(F8.3,2X),2X,ES12.4,2X,2(F8.3,2X),ES12.4,2X)' )
         READ( PHOTAB_UNIT, * )iphot_load,
     &         STWL_REF( IWL_LOAD ), EFFWL_REF( IWL_LOAD ),
     &         ENDWL_REF( IWL_LOAD ), FSOLAR_REF( IWL_LOAD )
     
#ifdef verbose_phot
         write( log_unit,'(4x,3(f8.3,2x),2x,2(es12.4,2x),f8.3,2x,12(es12.4,2x))' )
     &          stwl_ref( iwl_load ), effwl_ref( iwl_load ),
     &          endwl_ref( iwl_load ),fsolar_ref( iwl_load )
#endif
         WAVELENGTH( IWL_LOAD ) = EFFWL_REF ( IWL_LOAD )
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
            write( log_unit,'(a16,7x,f8.3,1x,40(1pe12.6,2x))' )
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
            write( log_unit,'(a16,7x,f8.3,1x,40(1pe12.6,2x))' )
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
      write( log_unit,'(16x,i3)' ) ntemp_strat_ref
#endif

      ALLOCATE( TEMP_STRAT_REF ( NTEMP_STRAT_REF ) )
      ALLOCATE( O3_CS_STRAT_REF( NWL_REF, NTEMP_STRAT_REF ) )

      READ( PHOTAB_UNIT,'(A)' ) FILE_LINE

      DO ITT_LOAD = 1, NTEMP_STRAT_REF
         READ( PHOTAB_UNIT,'(A16,7X,F8.3,1X,40(1PE12.6,2X))' )
     &         PHOT_EXPECT, TEMP_STRAT_REF( ITT_LOAD ),
     &         ( O3_CS_STRAT_REF( IWL_LOAD, ITT_LOAD ), IWL_LOAD = 1, NWL_REF )

#ifdef verbose_phot
         write( log_unit,'(a16,7x,f8.3,1x,40(1pe12.6,2x))' )
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

!***get needed photolysis data for the model chemistry from the
!***CSQY_DATA

       ALLOCATE( PHOTOLYSIS_RATE ( NPHOTAB ) )
       ALLOCATE( XXCS( IREFTEMPS, NWL, NPHOTAB ) )
       ALLOCATE( XXQY( IREFTEMPS, NWL, NPHOTAB ) )
       ALLOCATE( RTEMP_S( IREFTEMPS, NPHOTAB ) )
       
       MECHANISM_RATES = NPHOTAB

       WRITE(6,'(i4,1x,a)')(IPHOT,PHOTAB( IPHOT ),IPHOT=1,NPHOTAB)
       
       DO IPHOT = 1, NPHOTAB
          IP_MAP = INDEXR( PHOTAB( IPHOT ), NPHOT_MAP, PNAME_MAP )
          IF ( IP_MAP .LE. 0 ) THEN
             MSG = 'FATAL ERROR: photolysis reaction ' // TRIM( PHOTAB( IPHOT ) )
     &          // ' not found in ' //
     &             'the reference data! '
             ERROR_FLAG = .TRUE.
             CALL M3WARN ( PNAME, 0, 0, MSG )
          END IF
          IP_REF = PHOT_MAP( IP_MAP )
          PHOTOLYSIS_RATE( IPHOT ) = PNAME_MAP( IP_MAP )

!***check to see if this photolysis reaction is a special case that
!***  is referenced in other sections of the code.  if so, then set
!***  the appropriate pointers for later processing

           SELECT CASE ( TRIM( PHOTOLYSIS_RATE( IPHOT ) ) )
              CASE( 'O3O3P', 'O3O3P_SAPRC99', 'O3O3P_06', 'O3_O3P_IUPAC04', 'O3O3P_NASA06', 'O3_O3P_IUPAC10' )
                    LO3O3P = IPHOT
              CASE( 'NO2', 'NO2_SAPRC99', 'NO2_06', 'NO2_RACM2', 'NO2_IUPAC10' )
                    LNO2 = IPHOT
              CASE( 'O3O1D',  'O3O1D_SAPRC99' , 'O3O1D_06', 'O3_O1D_IUPAC04', 'O3O1D_NASA06', 'O3_O1D_IUPAC10' )
                    LO3O1D = IPHOT
              CASE( 'KETONE', 'KET_RACM2' )
                    LKETONE   = IPHOT
              CASE( 'MGLY_ADJ' )
                    LMGLY_ADJ = IPHOT
              CASE(  'MGLY_ABS' )
                    LMGLY_ABS = IPHOT
              CASE( 'ACETONE', 'CH3COCH3_RACM2' )
                    LACETONE  = IPHOT
              CASE( 'HCHOR_06', 'HCHO_R_SAPRC99', 'HCHO_RAD_RACM2', 'FORM_R_IUPAC10' )
                    LHCHOR_06 = IPHOT
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

       IF( ERROR_FLAG )THEN
         MSG = 'The above fatal error(s) found in CSQY data! '
         CALL M3EXIT( PNAME, 0, 0, MSG, -1 )
       END IF

      WRITE( LOG_UNIT,* ) 'Sucessfully Loaded CSQY_DATA file'
      
!!!      CLOSE(LOG_UNIT)
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
      INTEGER :: LOG_UNIT
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

      LOG_UNIT = INIT3()

      OPTICS_UNIT = GETEFILE( OPTICS_FILE, .TRUE., .TRUE., PNAME )


      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
      
#ifdef verbose_phot
      write( log_unit, '(A)' )TRIM(FILE_LINE)
#endif

      READ( FILE_LINE, 4999) NWL_REF
#ifdef verbose_phot
      write( log_unit,'(17x,i3)' ) nwl_ref
#endif
      
      NWL_OPTICS = NWL_REF

      DO ITT_LOAD = 1, 15 ! skip next 15 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( log_unit, '(A)' )TRIM(FILE_LINE)
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
         write( log_unit, 99946 )
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
         write( log_unit, '(A)' )TRIM(FILE_LINE)
#endif
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( log_unit, '(A)' )TRIM(FILE_LINE)
#endif

      READ( FILE_LINE, 4999)NAE_REFRACT_REF

#ifdef verbose_phot
      write( log_unit, * )' NAE_REFRACT_REF = ', NAE_REFRACT_REF
#endif

      IF( NAE_REFRACT_REF .NE. NUM_REFRACTIVE )THEN
         WRITE( LOG_UNIT,* ) 'NAE_REFRACT_REF  = ', NAE_REFRACT_REF
         MSG = 'NAERO_REFRACT used in ' // OPTICS_FILE
     &       // ' does not equal NUM_REFRACTIVE in CSQY_DATA.F file. '
         CALL M3WARN( PNAME, 0, 0, MSG )
         ERROR_FLAG = .TRUE.
      END IF
      
      ALLOCATE( AE_RERACT_REF   ( NAE_REFRACT_REF ) )

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
                
#ifdef verbose_phot
      write( log_unit, '(a)')TRIM(FILE_LINE)
#endif

      STRT = SCAN(FILE_LINE, '=', BACK = .TRUE.) + 1
      FINI =  LEN(FILE_LINE)

      READ( FILE_LINE( STRT:FINI ), * )( AE_RERACT_REF( ITT_LOAD ), 
     &                                   ITT_LOAD = 1, NAE_REFRACT_REF )

#ifdef verbose_phot
      write( log_unit, 99947)'REFRACTIVE_INDICES'
      write( log_unit, 99948 )(AE_RERACT_REF( ITT_LOAD ),ITT_LOAD = 1, 
     &                                 NAE_REFRACT_REF )
#endif


      ALLOCATE( AE_REAL_REFRACT ( NAE_REFRACT_REF, NWL_REF ) )
      ALLOCATE( AE_IMAG_REFRACT ( NAE_REFRACT_REF, NWL_REF ) )

      DO ITT_LOAD = 1, NAE_REFRACT_REF 
! set up refractive indices used by aero_photdata routine

          REFRACTIVE_INDEX( ITT_LOAD )%NAME = AE_RERACT_REF( ITT_LOAD )
          ALLOCATE( REFRACTIVE_INDEX( ITT_LOAD )%REAL_PART( N_MODE, NWL_REF ) )         
          ALLOCATE( REFRACTIVE_INDEX( ITT_LOAD )%IMAG_PART( N_MODE, NWL_REF )  ) 
           
#ifdef verbose_phot
          write( log_unit, '(i3, 1x, a16)')itt_load, refractive_index( itt_load )%name
#endif

      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( log_unit, '(A)' )TRIM(FILE_LINE)
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
         write( log_unit, 99949 )
     &          stwl_ref( iwl_load ), effwl_ref( iwl_load ),
     &          endwl_ref( iwl_load ),fsolar_ref( iwl_load ),
     &          ( ae_real_refract( itt_load, iwl_load ),
     &            ae_imag_refract( itt_load, iwl_load ), itt_load = 1, nae_refract_ref )
#endif

      END DO

      DO ITT_LOAD = 1, 8 ! skip next 8 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif
      END DO

! read optical data for liquid droplets

      READ( FILE_LINE, 4999)NRADIUS_LIQUID

#ifdef verbose_phot
      write( log_unit, '(a,i4)' )'NRADIUS_LIQUID = ',NRADIUS_LIQUID
#endif

      ALLOCATE(RADIUS_LIQUID( NRADIUS_LIQUID ))
      
      ALLOCATE( LIQUID_EXTINCT(NRADIUS_LIQUID, NWL_OPTICS),
     &         LIQUID_ASYMFACT(NRADIUS_LIQUID, NWL_OPTICS),
     &         LIQUID_COALBEDO(NRADIUS_LIQUID, NWL_OPTICS))

      QUANTITY = 'LIQ_EXT'

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif
      
      DO ITT_LOAD = 1, NRADIUS_LIQUID
         READ( OPTICS_UNIT, * )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_EXTINCT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( log_unit, 99950 )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_EXTINCT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( TRIM( OPTICS_EXPECT ) .NE. TRIM( QUANTITY ) ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO


      QUANTITY = 'LIQ_ASY'
      
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif
      DO ITT_LOAD = 1, NRADIUS_LIQUID
         READ( OPTICS_UNIT, * )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_ASYMFACT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( log_unit, 99950 )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_ASYMFACT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( TRIM( OPTICS_EXPECT ) .NE. TRIM( QUANTITY ) ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif

      QUANTITY = 'LIQ_COA'
      
      DO ITT_LOAD = 1, NRADIUS_LIQUID
         READ( OPTICS_UNIT, * )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_COALBEDO( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( log_unit, 99950 )
     &         OPTICS_EXPECT, RADIUS_LIQUID( ITT_LOAD ),
     &         ( LIQUID_COALBEDO( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( TRIM( OPTICS_EXPECT ) .NE. TRIM( QUANTITY ) ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO


      DO ITT_LOAD = 1, 7 ! skip next 7 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif
      END DO

! read optical data for liquid droplets

      READ( FILE_LINE, 4999)NDIAMETER_ICE

#ifdef verbose_phot
      write( log_unit, '(a,i4)' )'NDIAMETER_ICE = ',NDIAMETER_ICE
#endif

      ALLOCATE(DIAMETER_ICE( NDIAMETER_ICE ))
      
      ALLOCATE( ICE_EXTINCT(NDIAMETER_ICE, NWL_OPTICS),
     &         ICE_ASYMFACT(NDIAMETER_ICE, NWL_OPTICS),
     &         ICE_COALBEDO(NDIAMETER_ICE, NWL_OPTICS),
     &            ICE_DELTRANS(NDIAMETER_ICE, NWL_OPTICS))

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif

      QUANTITY = 'ICE_EXT'
      
      DO ITT_LOAD = 1, NDIAMETER_ICE
         READ( OPTICS_UNIT, * )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_EXTINCT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( log_unit, 99950 )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_EXTINCT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( TRIM( OPTICS_EXPECT ) .NE. TRIM( QUANTITY ) ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO
 
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif

      QUANTITY = 'ICE_ASY'
      
      DO ITT_LOAD = 1, NDIAMETER_ICE
         READ( OPTICS_UNIT, * )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_ASYMFACT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( log_unit, 99950 )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_ASYMFACT( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( TRIM( OPTICS_EXPECT ) .NE. TRIM( QUANTITY ) ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif

      QUANTITY = 'ICE_COA'
      
      DO ITT_LOAD = 1, NDIAMETER_ICE
         READ( OPTICS_UNIT, * )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_COALBEDO( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( log_unit, 99950 )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_COALBEDO( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( TRIM( OPTICS_EXPECT ) .NE. TRIM( QUANTITY ) ) THEN
               MSG =  'Optical quantity read ' // TRIM( OPTICS_EXPECT )
     &             // ' does match expected quantity, ' // TRIM( QUANTITY )
               CALL M3WARN( PNAME, 0, 0, MSG )
               ERROR_FLAG = .TRUE.
            END IF
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
      write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif

      QUANTITY = 'ICE_DEL'
      
      DO ITT_LOAD = 1, NDIAMETER_ICE
         READ( OPTICS_UNIT, * )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_DELTRANS( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )

#ifdef verbose_phot
          write( log_unit, 99950 )
     &         OPTICS_EXPECT, DIAMETER_ICE( ITT_LOAD ),
     &         ( ICE_DELTRANS( ITT_LOAD, IWL_LOAD), IWL_LOAD = 1, NWL_OPTICS )
#endif
            IF ( TRIM( OPTICS_EXPECT ) .NE. TRIM( QUANTITY ) ) THEN
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
         write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif
      END DO

      READ( OPTICS_UNIT,5013 ) NUMB_LANDUSE_REF

      DO ITT_LOAD = 1, 3 ! skip next 3 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif
      END DO

      READ( OPTICS_UNIT,5013 ) INDEX_GRASSLAND_REF
      READ( OPTICS_UNIT,5013 ) INDEX_OCEAN_REF
      READ( OPTICS_UNIT,5013 ) INDEX_SEA_ICE

#ifdef verbose_phot
      write( log_unit,6013 )'NUMB_LANDUSE_REF    = ', numb_landuse_ref
      write( log_unit,6013 )'INDEX_GRASSLAND_REF = ', index_grassland_ref
      write( log_unit,6013 )'INDEX_OCEAN_REF     = ', index_ocean_ref
      write( log_unit,6013 )'INDEX_SEA_ICE       = ', index_sea_ice
#endif

      ALLOCATE( LANDUSE_REF     ( NUMB_LANDUSE_REF ) )
      ALLOCATE( ZENITH_COEFF_REF( NUMB_LANDUSE_REF ) )
      ALLOCATE( SEASON_COEFF_REF( NUMB_LANDUSE_REF ) )
      ALLOCATE( SNOW_COEFF_REF  ( NUMB_LANDUSE_REF ) )
      ALLOCATE( SPECTRAL_ALBEDO_REF( NWL_OPTICS, NUMB_LANDUSE_REF ) )

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line
#ifdef verbose_phot
      write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif

      DO ITT_LOAD = 1, NUMB_LANDUSE_REF
         READ( OPTICS_UNIT,5012 ) LANDUSE_REF( ITT_LOAD ),
     &                            ZENITH_COEFF_REF( ITT_LOAD ),
     &                            SEASON_COEFF_REF( ITT_LOAD ),
     &                            SNOW_COEFF_REF( ITT_LOAD )
#ifdef verbose_phot
         write( log_unit,5012 ) landuse_ref( itt_load ),
     &                          zenith_coeff_ref( itt_load ),
     &                          season_coeff_ref( itt_load ),
     &                          snow_coeff_ref( itt_load )
#endif
      END DO

      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line
#ifdef verbose_phot
      write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif

      DO ITT_LOAD = 1, NUMB_LANDUSE_REF
         READ( OPTICS_UNIT,'(A30,1X,40(1PE12.6,2X))' ) LAND_EXPECT,
     &        ( SPECTRAL_ALBEDO_REF(IWL_LOAD, ITT_LOAD), IWL_LOAD = 1, NWL_REF )

#ifdef verbose_phot
         write( log_unit,'(a30,1x,40(1pe12.6,2x))' ) trim( land_expect ),
     &        ( spectral_albedo_ref(iwl_load, itt_load), iwl_load = 1, nwl_ref )
#endif

      END DO

      DO ITT_LOAD = 1, 3 ! skip next 3 lines
         READ( OPTICS_UNIT,'(A)' ) FILE_LINE
#ifdef verbose_phot
         write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif
      END DO

      READ( OPTICS_UNIT,5013 ) NUMB_LANDUSE_NLCD50
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line

#ifdef verbose_phot
      write( log_unit,6013 ) 'NUMB_NLCD50_MODIS = ', numb_landuse_NLCD50
      write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif

      ALLOCATE( LANDUSE_NLCD50( NUMB_LANDUSE_NLCD50 )  )
      ALLOCATE( ALBMAP_REF2NLCD50( NUMB_LANDUSE_NLCD50 )  )
      ALLOCATE( ALBFAC_REF2NLCD50( NUMB_LANDUSE_NLCD50 )  )

      DO ITT_LOAD = 1, NUMB_LANDUSE_NLCD50
         READ( OPTICS_UNIT,5016 ) LANDUSE_NLCD50( ITT_LOAD ),
     &                            ALBMAP_REF2NLCD50( ITT_LOAD ),
     &                            ALBFAC_REF2NLCD50( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,6016 ) itt_load, landuse_NLCD50( itt_load ),
     &                          albmap_ref2NLCD50( itt_load ),
     &                          albfac_ref2NLCD50( itt_load )
#endif

      END DO

      READ( OPTICS_UNIT,5013 ) NUMB_LANDUSE_USGS
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line
#ifdef verbose_phot
      write( log_unit,6013 ) 'NUMB_USGS = ', numb_landuse_usgs
      write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif

      ALLOCATE( LANDUSE_USGS   ( NUMB_LANDUSE_USGS ) )
      ALLOCATE( ALBMAP_REF2USGS( NUMB_LANDUSE_USGS ) )
      ALLOCATE( ALBFAC_REF2USGS( NUMB_LANDUSE_USGS ) )

      DO ITT_LOAD = 1, NUMB_LANDUSE_USGS
         READ( OPTICS_UNIT,5016 ) LANDUSE_USGS( ITT_LOAD ),
     &                            ALBMAP_REF2USGS( ITT_LOAD ),
     &                            ALBFAC_REF2USGS( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,6016 ) itt_load, landuse_usgs( itt_load ),
     &                          albmap_ref2usgs( itt_load ),
     &                          albfac_ref2usgs( itt_load )
#endif

      END DO

      READ( OPTICS_UNIT,5013 ) NUMB_LANDUSE_MODIS
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line
#ifdef verbose_phot
      write( log_unit,6013 ) 'NUMB_MODIS = ', numb_landuse_modis
      write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif

      ALLOCATE( LANDUSE_MODIS   ( NUMB_LANDUSE_MODIS ) )
      ALLOCATE( ALBMAP_REF2MODIS( NUMB_LANDUSE_MODIS ) )
      ALLOCATE( ALBFAC_REF2MODIS( NUMB_LANDUSE_MODIS ) )

      DO ITT_LOAD = 1, NUMB_LANDUSE_MODIS
         READ( OPTICS_UNIT,5016 ) LANDUSE_MODIS( ITT_LOAD ),
     &                            ALBMAP_REF2MODIS( ITT_LOAD ),
     &                            ALBFAC_REF2MODIS( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,6016 ) itt_load, landuse_modis( itt_load ),
     &                          albmap_ref2modis( itt_load ),
     &                          albfac_ref2modis( itt_load )
#endif

      END DO

      NO_NLCD40 = .TRUE.  ! default condition that file does not contain NLCD40 Landuse data
      
      READ( OPTICS_UNIT,5013, END = 101 ) NUMB_LANDUSE_NLCD40
      READ( OPTICS_UNIT,'(A)' ) FILE_LINE ! skip line
#ifdef verbose_phot
      write( log_unit,6013 ) 'NUMB_NLCD40_MODIS = ', numb_landuse_NLCD40
      write( log_unit, '(a)' )TRIM(FILE_LINE)
#endif

      ALLOCATE( LANDUSE_NLCD40( NUMB_LANDUSE_NLCD40 )  )
      ALLOCATE( ALBMAP_REF2NLCD40( NUMB_LANDUSE_NLCD40 )  )
      ALLOCATE( ALBFAC_REF2NLCD40( NUMB_LANDUSE_NLCD40 )  )

      DO ITT_LOAD = 1, NUMB_LANDUSE_NLCD40
         READ( OPTICS_UNIT,5016 ) LANDUSE_NLCD40( ITT_LOAD ),
     &                            ALBMAP_REF2NLCD40( ITT_LOAD ),
     &                            ALBFAC_REF2NLCD40( ITT_LOAD )

#ifdef verbose_phot
         write( log_unit,6016 ) itt_load, landuse_NLCD40( itt_load ),
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

      WRITE( LOG_UNIT,* ) 'Sucessfully Loaded OPTICS_DATA file'
      

4999  FORMAT(17X,I3,2X,17X,I3)
5012  FORMAT( 4X,A30,1X,3(F8.3,2X) )
5013  FORMAT( 22X,I3 )
5016  FORMAT( 4X,A60,1X,I3,2X,3(F8.3,2X) )
99946 FORMAT(4x,3(f8.3,2x),2x,2(es12.4,2x),f8.3,2x,12(es12.4,2x))
99947 FORMAT(a3, 1x, a16)
99948 FORMAT(10(a16,1x))
99949 FORMAT(4x,3(f8.3,2x),2x,2(es12.4,2x),f8.3,2x,12(es12.4,2x))
99950 FORMAT(a8,1x,f10.3,40(1x,1pe12.6))
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


      REAL, INTENT(IN)  :: TEMP( : )    ! air temperature [K]
      REAL, INTENT(IN)  :: DENS( : )    ! air density [molecules/cm**2]
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

      REAL            :: PRESSURE       ! units vary
      REAL            :: QDUM           ! scratch variable for yield
      REAL            :: ALPHA, BETA    ! scratch variables

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


!***loop over rates
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
!***Make specific temperature and/or density corrections if needed

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
           CASE ( HIGHER_ALDEHYDES )  ! C3 and higher aldehydes
***density correction to quantum yield
               FORALL( IWL = 1:NWL_REF )
                  FORALL( LAYS = 1:NLAYS, QYZ( LAYS, IWL, IPHOT ) .GT. 1.0E-5 ) 
                     QYZ( LAYS, IWL, IPHOT ) = 1.0
     &                                       / ( 1.0 + ( 1.0 / QYZ( LAYS, IWL, IPHOT ) - 1.0 ) * DENS( LAYS ) * 4.0568E-20 )
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
                   FORALL( LAYS = 1:NLAYS, QYZ( LAYS, IWL, IPHOT ) .LT. 1.0 )
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
           CASE ( METHYL_GLYOXAL_IUPAC04 )
               DO IWL = 1, NWL_REF
                   IF( WAVELENGTH( IWL ) .GE. 500.0 .OR. WAVELENGTH( IWL ) .LE. 240.0 )CYCLE
                   DO LAYS = 1, NLAYS
                        IF( QYZ( LAYS, IWL, IPHOT ) .LE. 0.0 .OR. QYZ( LAYS, IWL, IPHOT ) .GE. 1.0 )CYCLE
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
! with recommendation in NASA JPL (2011).  Note that linearily interpolated values are assumed to be 
! at air number density equal to 2.465E19 molecules/cm3. Also that computed pressures are differences 
! between layer pressures and pressure of interpolated values. Pressure units are Torrs.
                       IF( WAVELENGTH( IWL ) .LE. 370.0 )THEN                        
                           PRESSURE = MAX( 400.0, 1.03547E-19 * DENS( LAYS ) * TEMP( LAYS )  )
     &                              - 2.5524 * TEMP( LAYS )
                       ELSE
                           PRESSURE = 1.03547E-19 * TEMP( LAYS ) * ( DENS( LAYS ) - 2.465E19 )
                       END IF
                       QYZ( LAYS, IWL, IPHOT ) = 1.0 
     &                                         /( 1.0 / QYZ( LAYS, IWL, IPHOT ) + KMGLY( IWL ) * PRESSURE )
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
              IF ( LHCHOR_06 .LE. 0 )CYCLE
              DO IWL = 1, NWL_REF
                 IF ( WAVELENGTH( IWL ) .LE. 329.0 ) CYCLE
                 DO LAYS = 1, NLAYS
                    IF ( QYZ( LAYS, IWL, IPHOT ) .LE. 0.0 ) CYCLE

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
     
                    QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                 END DO
              END DO
           CASE (  ACETONE ) ! 'CH3COCH3 -> products'
!***temperature/density correction to quantum yield
              IF ( IPHOT .EQ. LACETONE .AND. NWL .EQ. NWL_ACETONE_FJX ) THEN
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
             ELSE
                 DO IWL = 1, NWL_REF
                      DO LAYS = 1, NLAYS
                        QYZ( LAYS, IWL, IPHOT ) = QUANTUM_ACETONE( TEMP( LAYS ), DENS( LAYS ), WAVELENGTH( IWL ) )
!                        QYZ( LAYS, IWL, IPHOT ) = MAX( 0.0, MIN( 1.0, QYZ( LAYS, IWL, IPHOT ) ) )
                   END DO
                END DO
             END IF
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
 
         IF ( FIRSTCALL )THEN

         END IF
      END DO   ! loop on IPHOT   

!      IF ( FIRSTCALL )THEN 
!          DO IPHOT = 1, MECHANISM_RATES
!             DO LAYS = 1, NLAYS
!                WRITE(CSQY_DATA_LOG, 99951)'CSZ for ', PHOTOLYSIS_RATE( IPHOT ),(CSZ( LAYS, IWL, IPHOT ),IWL=1,NWL_REF)
!                WRITE(CSQY_DATA_LOG, 99951)'QYZ for ', PHOTOLYSIS_RATE( IPHOT ),(QYZ( LAYS, IWL, IPHOT ),IWL=1,NWL_REF)
!             END DO
!          END DO
!          FIRSTCALL = .FALSE.
!      END IF
      
99951 FORMAT(A9,A16,40(1X,ES12.4))
      RETURN
      END SUBROUTINE GET_CSQY
      SUBROUTINE DEFINE_CSQY_ADJUST()
! Purpose determine whether a phototlysis rate uses specific corrections to 
! CSQY data 
!          Apr  2014 B.Hutzell Initial version 
!          Sept 2014 B.Hutzell corrected acetaldehyde case to include CCHO_R_SAPRC99,
!                    the photolysis rate used in original CB05 mechanism

               IMPLICIT NONE
            
               INTEGER :: IPHOT, IWL
               
               ALLOCATE( CSQY_ADJUST( MECHANISM_RATES ) )
                        
               DO IPHOT = 1, MECHANISM_RATES
                  SELECT CASE ( TRIM( PHOTOLYSIS_RATE( IPHOT ) ) )
                     CASE ( 'CCHO_R', 'CCHO_R_SAPRC99', 'CH3CHO_RACM2', 'ALD2_R_IUPAC10' )      ! 'CH3CHO -> CH3 + HCO'
                        CSQY_ADJUST( IPHOT ) = ACETALDEHYDE
                     CASE ( 'C2CHO', 'ALD_RACM2', 'BALD_RACM2', 'UALD_RACM2', 'ALDX_R_IUPAC10'  )  ! C3 and higher aldehydes
                        CSQY_ADJUST( IPHOT ) = HIGHER_ALDEHYDES
                     CASE ( 'MVK_06' )
                        CSQY_ADJUST( IPHOT ) = METHYL_VINYL_KETONE
                     CASE ( 'MACR_06', 'MACR_RACM2' )
                        CSQY_ADJUST( IPHOT ) = METHYL_ACROLEIN
                     CASE ( 'MEK_06' )
                        CSQY_ADJUST( IPHOT ) = METHYL_ETHYL_KETONE
                     CASE ( 'MGLY_06' , 'BACL_07', 'MGLY_IUPAC04', 'MGLY_IUPAC10' )
                        CSQY_ADJUST( IPHOT ) = METHYL_GLYOXAL_IUPAC04
                        IF( .NOT. ALLOCATED( KMGLY ) ) ALLOCATE( KMGLY( NWL_REF ) )
                        FORALL( IWL = 1:NWL_REF )KMGLY( IWL ) = 1.93E4 * EXP( -5639.0 / WAVELENGTH( IWL ) )
                     CASE (  'ACRO_09' )
                        CSQY_ADJUST( IPHOT ) = ACROLEIN
                     CASE ( 'HCHOM_06', 'HCHO_M_SAPRC99', 'HCHO_MOL_RACM2', 'FORM_R_IUPAC10' )  ! 'CH2O -> H2 + CO'
                        CSQY_ADJUST( IPHOT ) = FORMALDEHYDE_MOLECULAR
                     CASE (  'ACET_06', 'ACETONE', 'CH3COCH3_RACM2' ) ! 'CH3COCH3 -> products'
                        CSQY_ADJUST( IPHOT ) = ACETONE
                     CASE (  'KETONE'  )
                        CSQY_ADJUST( IPHOT ) = KETONE_LEGACY
                     CASE ( 'KET_RACM2', 'HKET_RACM2', 'MEK_RACM2', 'MVK_RACM2' ) ! Ketone treatement from W. Stockwell sbox
                        CSQY_ADJUST( IPHOT ) = KETONE_RACM2
                     CASE ( 'GLYH2_RACM2', 'GLYF_RACM2', 'GLYHX_RACM2', 'MGLY_RACM2'  ) ! glyoxal treatement from W. Stockwell sbox
                        CSQY_ADJUST( IPHOT ) = GLYOXAL_RACM2
                     CASE (  'MGLY_ADJ',  'MGLY_ABS' )
                        CSQY_ADJUST( IPHOT ) = METHYL_GLYOXAL_LEGACY
                     CASE DEFAULT
                        CSQY_ADJUST( IPHOT ) =  0
                     END SELECT
!                     WRITE(CSQY_DATA_LOG,'(A)')'For rate ' // TRIM( PHOTOLYSIS_RATE( IPHOT ) ) 
!     &               // ' using CSQY_ADJUSTMENT ' // TRIM( CSQY_ADJUSTMENTS( CSQY_ADJUST( IPHOT ) ) )
               END DO
              RETURN
      END SUBROUTINE DEFINE_CSQY_ADJUST
      REAL FUNCTION QUANTUM_ACETONE( TEMP, DENS_NUMB, LAMBDA )

C-----------------------------------------------------------------------
C Computes acetone quantum yields according to:
C IUPAC (2005) recommendation based on
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
      REAL A5           ! 6th coef for qy
      REAL A6           ! 7th coef for qy

      REAL PHI_CO       ! CO branch of IUPAC (2005) acetone QYZ
      REAL PHI_CH3CO    ! CH3CO branch of IUPAC (2005) acetone QYZ
      REAL AA           ! scratch variable for IUPAC (2005) acetone QYZ
      REAL BB           ! scratch variable for IUPAC (2005) acetone QYZ
      REAL CC           ! scratch variable for IUPAC (2005) acetone QYZ
      
      REAL TEMP_OVER 295K   ! temperature divided by 295 K
      REAL ONE_OVER_LAMBDA  ! reciporcal of wavelenght, 10E7/nm or 1/cm

      TEMP_OVER 295K  = TEMP * ONE_OVER_295K
      ONE_OVER_LAMBDA = 1.0E7 / LAMBDA
      
      IF ( LAMBDA .GE. 248.0 .AND. LAMBDA .LE. 349.0 ) THEN

         AA = 0.350 * ( TEMP_OVER 295K )**(-1.28)
         BB = 0.068 * ( TEMP_OVER 295K )**(-2.65)
         A0 = ( AA / ( 1.0 - AA ) ) * EXP( BB * ( LAMBDA - 248.0 ) )
         PHI_CO = 1.0 / ( 1.0 + A0 )

         IF ( LAMBDA .LE. 302.0 ) THEN

!***wavelengths 248-302 nm

            AA = 1.600 * 1.0E-19 * ( TEMP_OVER 295K )**(-2.38)
            BB =  0.55 * 1.0E-03 * ( TEMP_OVER 295K )**(-3.19)
            A1 = AA * EXP( -BB * ( ( ONE_OVER_LAMBDA ) - 33113.0 ) )
            PHI_CH3CO = ( 1.0 - PHI_CO ) / ( 1.0 + A1*DENS_NUMB )

!***wavelengths 302-349 nm

         ELSE

            AA = 1.62 * 1.0E-17 * ( TEMP_OVER 295K )**(-10.03)
            BB = 1.79 * 1.0E-3  * ( TEMP_OVER 295K )**(-1.364)
            A2 = AA * EXP( -BB * ( ( ONE_OVER_LAMBDA ) - 30488.0 ) )

            AA = 26.29 * ( TEMP_OVER 295K )**(-6.59)
            BB = 5.72 * 1.0E-7 * ( TEMP_OVER 295K )**(-2.93)
            CC = ( 30006.0 )   * ( TEMP_OVER 295K )**(-0.064)
            A3 = AA * EXP( -BB * ( ( ONE_OVER_LAMBDA ) - CC )**2.0 )

            AA = 1.67 * 1.0E-15 * ( TEMP_OVER 295K )**(-7.25)
            BB = 2.08 * 1.0E-3  * ( TEMP_OVER 295K )**(-1.16)
            A4 = AA * EXP( -BB * ( ( ONE_OVER_LAMBDA ) - 30488.0 ) )

            PHI_CH3CO = ( 1.0 - PHI_CO )
     &                * ( 1.0 + A4 * DENS_NUMB + A3 )
     &                / ( ( 1.0 + A2 * DENS_NUMB + A3 )
     &                *   ( 1.0 + A4 * DENS_NUMB ) )
         END IF

         QUANTUM_ACETONE = PHI_CO + PHI_CH3CO

      ELSE IF ( LAMBDA .LT. 248.0 .AND. LAMBDA .GT. 0.0 ) THEN ! set QY to 1.0

!***based on IUPAC (2005) data sheet

         PHI_CO    = 0.05
         PHI_CH3CO = 0.95
         QUANTUM_ACETONE = PHI_CO + PHI_CH3CO

      ELSE IF ( LAMBDA .GT. 349.0 ) THEN

         QUANTUM_ACETONE = 0.0

      END IF

      QUANTUM_ACETONE = MAX( 0.0, MIN( 1.0, QUANTUM_ACETONE ) )

      RETURN
      END FUNCTION QUANTUM_ACETONE                  
      END MODULE CSQY_DATA
