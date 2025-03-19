
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
      MODULE AERO_PHOTDATA

      IMPLICIT NONE

      REAL( 8 ), ALLOCATABLE :: SHELL_MOMENT3( :,: )  ! modal shell third moment conc.
      REAL( 8 ), ALLOCATABLE :: CORE_MOMENT3 ( :,: )  ! model core third moment conc.

      REAL, ALLOCATABLE :: BLK_AE_VOL  ( :,: )     ! aerosol modal volumes [ m**3/m**3 ]
      REAL, ALLOCATABLE :: BLK_AE_LSG  ( :,: )     ! aerosol modal log of geo. stand. dev
      REAL, ALLOCATABLE :: BLK_AE_DGN  ( :,: )     ! aerosol modal mean geometeric diam. [ m ]
      REAL, ALLOCATABLE :: AE_DGN_CORE ( :,: )   ! inner core modal mean geometeric diam. [ m ]
      REAL, ALLOCATABLE :: AE_DGN_SHELL( :,: )   ! modal mean geometeric diam. [ m ]
      REAL, ALLOCATABLE :: BLK_AE_CONC ( :,:,: ) ! aerosol species concentrations [ ug/m**3]

      REAL, ALLOCATABLE :: AE_NR_SHELL ( :,:,: ) ! real part of refractive index for outer shell
      REAL, ALLOCATABLE :: AE_NI_SHELL ( :,:,: ) ! imaginary part of refractive index for outer shell
      REAL, ALLOCATABLE :: AE_NR_CORE  ( :,:,: ) ! real part of refractive index for inner core
      REAL, ALLOCATABLE :: AE_NI_CORE  ( :,:,: ) ! imaginary part of refractive index for inner core
      REAL, ALLOCATABLE :: BLK_AE_NR   ( :,:,: ) ! mean aerosol real part of refractive index
      REAL, ALLOCATABLE :: BLK_AE_NI   ( :,:,: ) ! mean aerosol imaginary part of refractive index

      REAL, ALLOCATABLE :: AERO_ASYM_FAC ( :,: ) ! aerosol modal averaged asymmetry factor
      REAL, ALLOCATABLE :: AERO_EXTI_COEF( :,: ) ! aerosol modal averaged extinction coeff., 1/m
      REAL, ALLOCATABLE :: AERO_SCAT_COEF( :,: ) ! aerosol modal averaged scattering coeff., 1/m
      REAL, ALLOCATABLE :: AERO_ABSO_COEF( :,: ) ! aerosol modal averaged sbsorption coeff., 1/m
      REAL, ALLOCATABLE :: AERO_EXTI_550  ( : )  ! aerosol modal averaged extinction coeff. at 550nm, 1/m
      REAL, ALLOCATABLE :: AERO_ABSO_550  ( : )  ! aerosol modal averaged extinction coeff. at 550nm, 1/m
 
      PUBLIC            :: AERO_ASYM_FAC, AERO_EXTI_COEF, AERO_SCAT_COEF, AERO_ABSO_COEF,
     &                     INIT_AERO_DATA, GET_AERO_DATA

      INTEGER,   ALLOCATABLE, PRIVATE :: REFRACT_INDX_MAP( : )  ! map array for refactive index

      REAL,      ALLOCATABLE, PRIVATE :: VOL_MINS( : )          ! minmum volume permode
      REAL( 8 ), ALLOCATABLE, PRIVATE :: M3_FACTOR( : )         ! convert mass to portion of 3rd moment
      LOGICAL,   ALLOCATABLE, PRIVATE :: IS_CORE_SPCS( :,: )    ! flag denoting species is in aerosol core

      INTEGER,   PARAMETER, PRIVATE   :: NUMB_COR_SPCS = 3      ! number species in core

      LOGICAL :: CALCULATE_EXT_550   = .FALSE. ! flag to get extinction at 550 nm
      LOGICAL :: USE_ANGSTROM_INTERP = .FALSE. ! flag to use angstrom exponents for 550 nm
      
      INTEGER   :: IWL_ANGSTROM_LOWER  = 0       ! index for wavelength less than 550 nm
      INTEGER   :: IWL_ANGSTROM_UPPER  = 0       ! index for wavelength greater than 550 nm
      REAL( 8 ) :: ANGSTROM_RATIO      = 1.0D0   ! wavelength less than 550 nm divided by 550 nm
      REAL( 8 ) :: LOG_ANGSTROM_RATIO  = 0.0D0    
      REAL( 8 ) :: ANGSTROM_SPAN       = 1.0D0   ! reciprocal of log ((wavelength < 550 nm)/(wavelength > 550 nm)
      REAL( 8 ) :: ANGSTROM_EXPONENT   = 1.0D0   ! Angstrom exponent used to interpolate extinction at 550 nm
          
C *** Species in aerosol core

      CHARACTER( 16 ), PARAMETER, PRIVATE :: CORE_SPCS( NUMB_COR_SPCS ) =
     &                                       (/ 'AECI', 'AECJ', 'AECK' /)
      
      CONTAINS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE INIT_AERO_DATA( )

C Allocate and define arrays used to calculate aerosol optical properties

         USE GRID_CONF, ONLY: NROWS, NCOLS, NLAYS  ! horizontal & vertical domain specifications
         USE UTILIO_DEFN
      
         USE AERO_DATA           ! aero variable data
         USE SOA_DEFN            ! gas soa data
         USE AEROMET_DATA        ! constants and met data
      
         USE CSQY_DATA, ONLY : NWL => NWL_REF, NUM_REFRACTIVE, REFRACTIVE_INDEX,
     &                         WAVELENGTH => EFFWL_REF !!! , FIRST_DAY => NEW_START

         USE WRF_FAST_MIE, ONLY : FAST_MIEAER_MODAL, EFLAG_WRF_FAST_MIE

         IMPLICIT NONE

C Arguments:

C Local:
         CHARACTER(  32 ) :: PNAME             = 'INIT_AERO_DATA'
         CHARACTER( 120 ) :: XMSG 

         INTEGER          :: ALLOCSTAT
         INTEGER          :: ESTAT          ! status from environment var check
         INTEGER          :: JDATE, JTIME
         INTEGER          :: M, N, SPC, V   ! loop counters
      
         LOGICAL, SAVE    :: INITIALIZED  = .FALSE.

         REAL    :: MIE_PARAMETER = 1.0E-3
         COMPLEX :: REFINDX       = (1.0,0.0)

         REAL :: EXTAER,SCATAER,GAER
         REAL :: BSCOEF

#ifdef verbose_phot
         character( 26 ), allocatable :: lambda_list( : )
#endif

         IF ( INITIALIZED ) THEN
            RETURN
         END IF
          
         JDATE  = 0
         JTIME  = 0


C...Allocate needed arrays

         ALLOCATE ( VOL_MINS( N_MODE ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating VOL_MINS'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         
         ALLOCATE ( BLK_AE_CONC( N_AEROSPC,N_MODE,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating BLK_AE_CONC'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( SHELL_MOMENT3( N_MODE,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating SHELL_MOMENT3'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( BLK_AE_VOL( N_MODE,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating BLK_AE_VOL'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( BLK_AE_LSG( N_MODE,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating BLK_AE_LSG'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( BLK_AE_DGN( N_MODE,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating BLK_AE_DGN'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( AE_DGN_SHELL( N_MODE,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating AE_DGN_SHELL'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( AE_NR_SHELL( N_MODE,NLAYS,NWL ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating  AE_NR_SHELL'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( AE_NI_SHELL( N_MODE,NLAYS,NWL ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating  AE_NI_SHELL'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( AERO_ASYM_FAC( NLAYS,NWL ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating AERO_ASYM_FAC'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( AERO_EXTI_COEF( NLAYS,NWL ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating AERO_EXTI_FAC'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( AERO_SCAT_COEF( NLAYS,NWL ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating AERO_EXTI_FAC'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( AERO_ABSO_COEF( NLAYS,NWL ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating AERO_ABSO_FAC'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         AERO_SCAT_COEF = 0.0
         AERO_EXTI_COEF = 0.0
         AERO_ASYM_FAC  = 0.0
         AERO_ABSO_COEF = 0.0

         IF( CALCULATE_EXT_550 )THEN
            ALLOCATE ( AERO_EXTI_550( NLAYS ),
     &                 AERO_ABSO_550( NLAYS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating AERO_EXTI_550'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            AERO_EXTI_550  = 0.0
            AERO_ABSO_550  = 0.0
         END IF   

         CALL MAP_AERO()
                  
         ALLOCATE( M3_FACTOR( N_AEROSPC ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating  M3_FACTOR'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         
         ALLOCATE( REFRACT_INDX_MAP( N_AEROSPC ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating REFRACT_INDX_MAP'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         
!        IF( CORE_SHELL )THEN
            ALLOCATE( IS_CORE_SPCS( N_AEROSPC,N_MODE ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating IS_CORE_SPCS'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         
            IS_CORE_SPCS = .FALSE.

            ALLOCATE ( CORE_MOMENT3( N_MODE,NLAYS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating CORE_MOMENT3'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
      
            ALLOCATE ( AE_DGN_CORE( N_MODE,NLAYS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating AE_DGN_CORE'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
      
            ALLOCATE ( AE_NR_CORE( N_MODE,NLAYS,NWL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating AE_NR_CORE'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
      
            ALLOCATE ( AE_NI_CORE( N_MODE,NLAYS,NWL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating AE_NI_CORE'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
!        END IF
C ***    Create map for refactive index for each species

#ifdef verbose_phot
         allocate( lambda_list( nwl ) )
         do v = 1, nwl
            write( lambda_list( v ), 5000)int(wavelength( v )), 
     &      int(wavelength( v ))
         end do
         write( logdev, 5001)(lambda_list( v ), v = 1, nwl)
#endif 

         WRITE(LOGDEV,*)
         CALL M3MESG('METHOD FOR AEROSOL OPTICAL PROPERTIES')
         xmsg = 'METHOD FOR AEROSOL OPTICAL PROPERTIES'
         IF ( .NOT. CORE_SHELL ) THEN
            XMSG = 'CORE-SHELL mixing model option is set to off so'
            WRITE(LOGDEV,'(A)')TRIM( XMSG )
         ELSE
            XMSG = 'CORE-SHELL mixing model used when aerosol mode has significant black carbon'
            WRITE(LOGDEV,'(A)')TRIM( XMSG )
            XMSG = 'When aerosol mode does not have significant black carbon,'
            WRITE(LOGDEV,'(A)')TRIM( XMSG )
         END IF
         SELECT CASE ( AEROSOL_OPTICS )
           CASE( 1 )
             XMSG = 'VOLUME MIXING model and Tabular Mie Method used'
             WRITE(LOGDEV,'(A)')TRIM( XMSG )
             CALL FAST_MIEAER_MODAL( MIE_PARAMETER,REFINDX,EXTAER,SCATAER,GAER,BSCOEF )
             IF ( EFLAG_WRF_FAST_MIE ) THEN
                XMSG = 'Above Failure Initializing Tabular Mie Method for aerosol optics'
                CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF
           CASE( 2 )
             XMSG = 'VOLUME MIXING model and Mie calculation used'
             WRITE(LOGDEV,'(A)')TRIM( XMSG )
           CASE( 3 )
             XMSG = 'VOLUME MIXING model and FAST_OPTICS used'
             WRITE(LOGDEV,'(A)')TRIM( XMSG )
             XMSG = '-Approximations to Mie Theory based on Mie paramter and refractive index '
             WRITE(LOGDEV,'(A)')TRIM( XMSG )
         END SELECT
         WRITE(LOGDEV,*)

         FORALL ( SPC = 1:N_MODE ) VOL_MINS( SPC ) = PI6 * AEROMODE( SPC )%MIN_M3CONC

         DO SPC = 1, N_AEROSPC

            M3_FACTOR( SPC ) = REAL( 1.0E-9 * F6DPI / AEROSPC( SPC )%DENSITY, 8 )

            DO M = 1, NUM_REFRACTIVE
               IF ( AEROSPC( SPC )%OPTIC_SURR .EQ. REFRACTIVE_INDEX( M )%NAME ) THEN
                  REFRACT_INDX_MAP( SPC ) = M
                  DO N = 1, N_MODE

                     IF ( CORE_SHELL ) THEN
                        DO V = 1, NUMB_COR_SPCS
                           IF ( AEROSPC( SPC )%NAME( N ) .EQ. CORE_SPCS( V ) ) THEN
                               IS_CORE_SPCS( SPC, N ) = .TRUE.
                           END IF
                        END DO
                     END IF
#ifdef verbose_phot
                     if ( aero_missing(spc,n) ) cycle
                     write( logdev, 5002)trim( aerospc( spc )%name( n ) ), 
     &               trim( refractive_index( m )%name ),is_core_spcs( spc,n ),
     &               (refractive_index( m )%real_part( n,v ), 
     &                refractive_index( m )%imag_part( n,v ), v = 1, nwl)
#endif                  
                  END DO 
                  EXIT
               END IF
            END DO
            IF ( REFRACT_INDX_MAP( SPC ) .LT. 1 ) THEN
               DO M = 1, N_MODE
                  IF ( aero_missing(spc,n) ) CYCLE
                  XMSG = 'Species ' // TRIM( AEROSPC( SPC )%NAME( M ) )
     &                 // ' does not use defined refractive index.'
                  CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT3 )
               END DO
            END IF
         END DO

         IF( CALCULATE_EXT_550 )THEN ! locate 550 nm in wavebands and set-up interpolation 
            IF ( WAVELENGTH( 1 ) .GE. 550.0 )  THEN
                IWL_ANGSTROM_LOWER = 1
            ELSE IF ( WAVELENGTH( NWL ) .LE. 550.0 ) THEN
                IWL_ANGSTROM_LOWER = NWL
            ELSE
                LOOP_FINDW: DO V = 1, NWL - 1
                   IF ( WAVELENGTH( V ) .LT. 550.0 .AND. WAVELENGTH( V+1 ) .GT. 550.0 ) THEN
                      IWL_ANGSTROM_LOWER = V
                      IWL_ANGSTROM_UPPER = V+1
                      ANGSTROM_SPAN      = REAL( 1.0 / LOG( WAVELENGTH( V ) / WAVELENGTH( V+1 ) ), 8 )
                      ANGSTROM_RATIO     = REAL( (WAVELENGTH( V ) / 550.0), 8)
!                     ANGSTROM_RATIO     = REAL( (WAVELENGTH( V ) / WAVELENGTH( V+1 )), 8)
                      LOG_ANGSTROM_RATIO = LOG( ANGSTROM_RATIO )
!                      WRITE(LOGDEV,'(A,2(F7.3,1X))')'Angstrom Interpolation Wavelengths: ', 
!     &                WAVELENGTH( IWL_ANGSTROM_LOWER ), WAVELENGTH( IWL_ANGSTROM_UPPER  )
!                      WRITE(LOGDEV,'(A,2(ES12.4,1X))')'Angstrom Span, Ratio: ', 
!     &                ANGSTROM_SPAN, ANGSTROM_RATIO
                      USE_ANGSTROM_INTERP  = .TRUE.
                      EXIT LOOP_FINDW
                   ELSE IF ( WAVELENGTH( V ) .EQ. 550.0 ) THEN
                      IWL_ANGSTROM_LOWER = V
                      EXIT LOOP_FINDW
                   END IF
                END DO LOOP_FINDW
            END IF
            IF( .NOT. USE_ANGSTROM_INTERP )THEN
               WRITE(LOGDEV,'(A,2(F7.3,1X))')'No Angstrom Inpolation Used 550 Extinction used at ', 
     &         WAVELENGTH( IWL_ANGSTROM_LOWER )
            END IF
         END IF

      RETURN

5000  FORMAT(2X,'NR_',I3.3,7X,'NI_',I3.3,5X)
5002  FORMAT(2(A16,1X),L12,1X,200(ES12.4,1X))                    
5001  FORMAT('     AE_SPC     ',1X,'  REFRACT_INDX  ',1X,'PART_OF_CORE',1X,200(A26))

      END SUBROUTINE INIT_AERO_DATA

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GET_AERO_DATA ( COL, ROW, NLAYS, DENS, CGRID )

C-----------------------------------------------------------------------
C  FUNCTION:  This subroutine calculates the volume, the natural logs of 
C     geometric standard of all 3 modes. It assumes that aerosols have inner 
C     core with an outer shell to compute a core and total diameter per mode.  
C     The code also calculates the average modal aerosol refractive indices 
C     for the inner core and outer shell.
C
C     Internally mixed wet aerosols are assumed.
C
C     NOTE
C       2nd moment concentrations (M2) are passed into GETPAR in the
C       CBLK array.  The GETPAR calculations implicitly assume
C       that the input value of M2 is up to date
C       If, for example, the input M2 value was calculated
C       for a "dry" aerosol and the Wet_Moments_FLAG is .TRUE., GETPAR would
C       incorrectly adjust the M2 concentrations!
C
C  KEY SUBROUTINES/FUNCTIONS CALLED:  EXTRACT_AERO, EXTRACT_SOA, GETPAR
C                                     AERO_OPTICAL2, AERO_OPTICAL_CS  
C  REVISION HISTORY
C     06/13/13 B.Hutzell - initial version created for aero_photdata.F 
C                          from version 5.01
C     05/2016 H. Pye and B. Murphy - updated treatment of aerosol
C                          moments
C     05/23/16 D.Wong - replaced RRTMG_AERO_OPTICAL_UTIL_MODULE with
C                       CMAQ_RRTMG_AERO_OPTICAL_UTIL_MODULE to avoid
C                       duplication of the same module name on WRF side
C                       of the two-way model
C
C  REFERENCES:
C     Horvath, H., Size segrated light absorption coefficient for
C     the atmospheric aerosol, Atmos. Environ., Vol. 29, No. 8,
C     pp 875-883, 1995.
C
C     Beyer, K.D. A.R. Ravishankara, and E. R. Lovejoy, Meanurements
C     of H2SO4/H2O and H2SO4/HNO3/H2O solutions, J. Geophys. Res. Vol.
C     101, D9, pp 14519-14524, 1996.
C
C-----------------------------------------------------------------------

!     USE GRID_CONF           ! horizontal & vertical domain specifications
      USE RXNS_DATA
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN

      USE AERO_DATA           ! aero variable data
      USE SOA_DEFN            ! gas soa data
      USE AEROMET_DATA        ! constants and met data
      
      USE CSQY_DATA,  ONLY : NWL => NWL_REF, NUM_REFRACTIVE, REFRACTIVE_INDEX,
     &                       WAVELENGTH => EFFWL_REF

      USE CMAQ_RRTMG_AERO_OPTICAL_UTIL_MODULE, ONLY : AERO_OPTICAL2, AERO_OPTICAL_CS

      USE WRF_FAST_MIE, ONLY : AERO_OPTICS_TABULAR

      IMPLICIT NONE

C Includes:

C Arguments:

      INTEGER, INTENT( IN ) :: COL               ! specified column index
      INTEGER, INTENT( IN ) :: ROW               ! specified row index
      INTEGER, INTENT( IN ) :: NLAYS             ! # of vertical layers
      REAL,    INTENT( IN ) :: DENS( :,:,: )
      REAL,    POINTER      :: CGRID( :,:,:,: )

C Parameters:

      REAL( 8 ), PARAMETER :: NEGLECT_FRAC = 1.0D-9      ! skip if below this volume fraction
      REAL( 8 ), PARAMETER :: ONE_THIRD    = 1.0D0/3.0D0 
      REAL,      PARAMETER :: ONE_OVER_PI  = 1.0 / PI

C Local saved variables:

      CHARACTER( 16 ), SAVE :: AE_VRSN                  ! Aerosol version name
      CHARACTER( 32 ), SAVE :: PNAME = 'GET_AERO_DATA'  ! routine name

C Local variables:

      CHARACTER( 16 ) :: VNAME            ! varable name
      CHARACTER( 96 ) :: XMSG = ' '

      REAL( 8 ) :: FACTOR
      REAL( 8 ) :: FRACTION_CORE
      REAL( 8 ) :: FRACTION_SHELL
      REAL( 8 ) :: MOMENT3_PORTION
      REAL( 8 ) :: SUMRI_R_SHELL
      REAL( 8 ) :: SUMRI_I_SHELL
      REAL( 8 ) :: SUMRI_R_CORE
      REAL( 8 ) :: SUMRI_I_CORE
      REAL( 8 ) :: SUMRI_R
      REAL( 8 ) :: SUMRI_I

      REAL DGN_CORE       ! geometric mean diameters of aerosol core
      REAL VOL            ! Total Modal aerosol volume
      REAL VOL_CORE       ! Modal aerosol volume of aerosol core
      REAL DGN_SHELL      ! geometric mean diameters of aerosol core
      REAL VOL_SHELL      ! Modal aerosol volume of aerosol core
      REAL XXLSG          ! geometric standard deviation
      REAL MIE_PARAMETER  ! Modal Mie parameter

C***  variables for calculating modal averaged properties per layer and lambda

      COMPLEX NR_SHELL
      COMPLEX NI_SHELL
      COMPLEX NR_CORE
      COMPLEX NI_CORE

      REAL BEXT_MODE  ! total aerosol extinction coefficient [ 1/m ]
      REAL BSCAT_MODE ! total aerosol scattering coefficient [ 1/m ]
      REAL GBAR_MODE  ! total aerosol asymmetry factor
      REAL BEXT       ! total aerosol extinction coefficient [ 1/m ]
      REAL VFAC, BSC  ! unit correction factors
      REAL BSCAT      ! total aerosol scattering coefficient [ 1/m ]
      REAL G_BAR      ! total aerosol asymmetry factor
      REAL INV_LAMBDA ! reciprocal of wavelength [ 1/m ]
      REAL LAMDA_UM   ! wavelength  [ um ]

      INTEGER L, V, N, M            ! loop counters
      INTEGER SPC                   ! species loop counter
      INTEGER MODE                  ! aerosol mode loop counter
      LOGICAL SUCCESS
      LOGICAL TROUBLE
      LOGICAL, SAVE    :: FIRSTCALL  = .TRUE.


#ifdef verbose_phot
      character( 26 ), allocatable :: lambda_list( : )
#endif
      
C-----------------------------------------------------------------------

      IF ( N_AE_SPC .LE. 0 ) RETURN

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Put the grid cell physical data in the block arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C *** initialize internal and external dimensions of aerosols

      CORE_MOMENT3  = 0.0D+0
      SHELL_MOMENT3 = 0.0D+0
      BLK_AE_VOL    = 0.0
      BLK_AE_CONC   = 0.0
      AE_DGN_SHELL  = 0.0
      AE_DGN_CORE   = 0.0
      SUCCESS       = .TRUE.

      LAY_LOOP1: DO L = 1, NLAYS

C *** extract grid cell concentrations of aero species from CGRID into aerospc_conc 
C *** in aero_data module
C     Also converts dry surface area to wet second moment
#ifdef sens
         CALL EXTRACT_AERO ( CGRID( COL,ROW,L,: ), .TRUE., CGRID( COL,ROW,:,: ), .FALSE. )
#else
         CALL EXTRACT_AERO ( CGRID( COL,ROW,L,: ), .TRUE. )
#endif

C *** extract soa concentrations from CGRID
         AIRDENS  = DENS ( COL,ROW,L )
#ifdef sens
         CALL EXTRACT_SOA ( CGRID( COL,ROW,L,: ), CGRID( COL,ROW,:,: ), .FALSE. )
#else
         CALL EXTRACT_SOA ( CGRID( COL,ROW,L,: ) )
#endif

C *** Calculate geometric mean diameters and standard deviations of "wet" size distribution
         CALL GETPAR ( .FALSE. )

#ifdef verbose_phot
         if(col .eq. 1 .and. row .eq. 1 .and. l .eq. 1 )then
            do m = 1, n_mode
               write(logdev, 4999)l, m, aeromode_diam( m ),pi6*moment3_conc( m ),
     &         aeromode_lnsg( m )
4999           format('lay = ',i3,' mode = ',i3,' diam = ',es12.4,' pi6*moment3 = ', 
     &         es12.4,' ae_lsg = ', es12.4)
           end do
         end if
#endif

         DIME_LOOP: DO MODE = 1, N_MODE

            FRACTION_CORE  = 0.0D+0
            FRACTION_SHELL = 0.0D+0
            
            SPC_LOOP1: DO SPC = 1, N_AEROSPC
               IF ( AEROSPC( SPC )%TRACER ) CYCLE      ! skip tracer species
               IF ( AERO_MISSING( SPC,MODE ) ) CYCLE   ! skip undefined species
               BLK_AE_CONC( SPC,MODE,L ) = AEROSPC_CONC( SPC,MODE )

               MOMENT3_PORTION = M3_FACTOR( SPC ) * REAL( AEROSPC_CONC( SPC,MODE ), 8 )
               IF ( IS_CORE_SPCS( SPC,MODE ) ) THEN  
                  CORE_MOMENT3( MODE,L )  = CORE_MOMENT3( MODE,L )  + MOMENT3_PORTION
#ifdef verbose_phot
                  if ( col .eq. 1 .and. row .eq. 1 .and. l .eq. 1 ) then
                     write(logdev,5007)aerospc( spc )%name( mode ),moment3_portion,
     &               core_moment3( mode, l )
5007                 format(a16,1x,'moment3_portion = ',es12.4,1x,'moment3_core = ',es12.4 )
                  end if
#endif 
               ELSE 
                  SHELL_MOMENT3( MODE,L ) = SHELL_MOMENT3( MODE,L ) + MOMENT3_PORTION
#ifdef verbose_phot
                  if ( col .eq. 1 .and. row .eq. 1 .and. l .eq. 1 ) then
                     write(logdev,5008)aerospc( spc )%name( mode ),moment3_portion,
     &               shell_moment3( mode, l )
5008                 format(a16,1x,'moment3_portion = ',es12.4,1x,'moment3_shell = ',es12.4 )
                  end if
#endif 
               END IF 
            END DO SPC_LOOP1


            FRACTION_CORE  = CORE_MOMENT3(  MODE,L ) / REAL( MOMENT3_CONC( MODE ), 8 )
            FRACTION_SHELL = SHELL_MOMENT3( MODE,L ) / REAL( MOMENT3_CONC( MODE ), 8 )

            BLK_AE_LSG  ( MODE,L ) = AEROMODE_LNSG( MODE )
            AE_DGN_SHELL( MODE,L ) = AEROMODE_DIAM( MODE )

            IF ( FRACTION_CORE .LT. NEGLECT_FRAC ) THEN
               AE_DGN_CORE( MODE,L ) = 0.0
            ELSE
               FACTOR = FRACTION_CORE ** ONE_THIRD
               AE_DGN_CORE( MODE,L ) = REAL( FACTOR * AEROMODE_DIAM( MODE ) )
            END IF

            IF ( MOMENT3_CONC( MODE ) .GT. AEROMODE( MODE )%MIN_M3CONC ) THEN
               BLK_AE_VOL( MODE,L ) = PI6 * MOMENT3_CONC( MODE )
            END IF

#ifdef verbose_phot
            if ( col .eq. 1 .and. row .eq. 1 .and. l .eq. 1 ) then
               write(logdev, 5006)l, mode, ae_dgn_core( mode,l ), ae_dgn_shell( mode,l ),
     &         blk_ae_vol( mode,l ),blk_ae_lsg( mode,l) 
5006           format('lay = ',i3,' mode = ',i3,' dgn_core = ', es12.4, ' dgn_shell = ', 
     &         es12.4,' ae_vol = ', es12.4,' ae_lsg = ', es12.4)
            end if
#endif

         END DO DIME_LOOP

      END DO LAY_LOOP1

C *** Initialize refractive indices then compute

      AE_NR_SHELL = 0.0
      AE_NI_SHELL = 0.0
      AE_NR_CORE  = 0.0
      AE_NI_CORE  = 0.0

      LOOP_WAVE: DO V = 1, NWL

         LAMDA_UM   = 1.0E-3 * WAVELENGTH( V ) ! convert to micro-meters
         INV_LAMBDA = 1.0E9 / WAVELENGTH( V )

         LAY_LOOP2: DO L = 1, NLAYS

            REFRAC_LOOP: DO MODE = 1, N_MODE

               SUMRI_R_SHELL = 0.0D+0
               SUMRI_I_SHELL = 0.0D+0
               SUMRI_R_CORE  = 0.0D+0
               SUMRI_I_CORE  = 0.0D+0

               SPC_LOOP2: DO SPC = 1, N_AEROSPC
                  IF ( AEROSPC( SPC )%TRACER ) CYCLE              ! neglect tracer species
                  IF ( AERO_MISSING( SPC,MODE ) ) CYCLE  ! neglect undefined species

                  M = REFRACT_INDX_MAP( SPC )

                  MOMENT3_PORTION = M3_FACTOR( SPC ) * REAL( BLK_AE_CONC( SPC,MODE,L ), 8)
     
                  IF ( IS_CORE_SPCS( SPC, MODE ) ) THEN  
                  
                     SUMRI_R_CORE = SUMRI_R_CORE + MOMENT3_PORTION
     &                            * REAL( REFRACTIVE_INDEX( M )%REAL_PART( MODE,V ), 8)
                     SUMRI_I_CORE = SUMRI_I_CORE + MOMENT3_PORTION
     &                            * REAL( REFRACTIVE_INDEX( M )%IMAG_PART( MODE,V ), 8)

                  ELSE

                     SUMRI_R_SHELL = SUMRI_R_SHELL + MOMENT3_PORTION
     &                             * REAL( REFRACTIVE_INDEX( M )%REAL_PART( MODE,V ), 8)
                     SUMRI_I_SHELL = SUMRI_I_SHELL + MOMENT3_PORTION
     &                             * REAL( REFRACTIVE_INDEX( M )%IMAG_PART( MODE,V ), 8)

                  END IF
                  
               END DO SPC_LOOP2

C ***       Update output array values
               
               IF ( CORE_MOMENT3( MODE,L ) .GT. 0.0D0 ) THEN
                   AE_NR_CORE ( MODE,L,V ) = REAL( SUMRI_R_CORE / CORE_MOMENT3( MODE,L ) )
                   AE_NI_CORE ( MODE,L,V ) = REAL( SUMRI_I_CORE / CORE_MOMENT3( MODE,L ) )
               END IF
               
               IF ( SHELL_MOMENT3( MODE,L ) .GT. 0.0D0 ) THEN
                   AE_NR_SHELL ( MODE,L,V ) = REAL( SUMRI_R_SHELL / SHELL_MOMENT3( MODE,L ) )
                   AE_NI_SHELL ( MODE,L,V ) = REAL( SUMRI_I_SHELL / SHELL_MOMENT3( MODE,L ) )
               END IF
               
            END DO REFRAC_LOOP

C***loop over mode again for mean modal extinction and scattering properties
        
            G_BAR = 0.0
            BEXT  = 0.0
            BSCAT = 0.0

#ifdef verbose_phot_extra
            if ( L .eq. 1 .and. V .eq. 1 ) then

               write(LOGDEV, 9501)L, wavelength( V ), ae_dgn_core(  m,l ), ae_dgn_shell( m, l ), 
     &         ae_nr_shell( m,L,V ), ae_ni_shell( m,L,V ), ae_nr_core( m,L,V ), 
     &         ae_ni_core( m,L,V )

9501           format('layer = ',i2,' lambda  = ',es12.4,' dgn_core = ',es12.4,' dgn_shell = ',
     &                 es12.4,' refract_shell(nr,ni) = ', 2(es12.4,1x),' refract_core = ',
     &                 2(es12.4,1x))

               end if
#endif            

            OPTICS_LOOP: DO MODE = 1, N_MODE
               TROUBLE = .FALSE.
            
               DGN_CORE  = AE_DGN_CORE(  MODE,L )
               DGN_SHELL = AE_DGN_SHELL( MODE,L )
         
               VOL = BLK_AE_VOL( MODE,L )
 
               NR_SHELL = CMPLX( AE_NR_SHELL( MODE,L,V ), AE_NI_SHELL( MODE,L,V ) )

               IF ( AE_NR_SHELL( MODE,L,V ) .LE. 0.0 ) SUCCESS = .FALSE.
               IF ( AE_NI_SHELL( MODE,L,V ) .LE. 0.0 ) SUCCESS = .FALSE.
        
               XXLSG = EXP( BLK_AE_LSG( MODE,L ) )

C***calculate the extinction and scattering coefficients in [ 1 / m ] and the asymmetry 
C*** factor for aerosols.

               IF ( DGN_CORE .GT. 0.0 .AND. DGN_CORE / DGN_SHELL .LT. 0.999 ) THEN

                  VOL_CORE  = PI6 * REAL( CORE_MOMENT3( MODE,L ), 4 )
                  VOL_SHELL = PI6 * REAL( SHELL_MOMENT3( MODE,L ), 4 )

                  NR_CORE  = CMPLX( AE_NR_CORE( MODE,L,V ), AE_NI_CORE( MODE,L,V ) )

                  IF ( AE_NR_CORE( MODE,L,V ) .LE. 0.0 ) SUCCESS = .FALSE.
                  IF ( AE_NI_CORE( MODE,L,V ) .LE. 0.0 ) SUCCESS = .FALSE.

                  IF ( VOL_SHELL .LE. 0.0 ) THEN
                     SUCCESS = .FALSE.
!                    WRITE(6,*)TRIM(PNAME),': VOL_SHELL(',MODE,') = ',VOL_SHELL
                  END IF

                  IF ( VOL_CORE .LE. 0.0 ) THEN
                     SUCCESS = .FALSE.
!                    WRITE(6,*)TRIM(PNAME),':  VOL_CORE(',MODE,') = ',VOL_CORE
                  END IF

                  IF ( DGN_SHELL .LE. 0.0 ) SUCCESS = .FALSE.
                   
C *** Core-Shell case for optics         
                  IF ( SUCCESS ) CALL AERO_OPTICAL_CS( LAMDA_UM, NR_CORE, NR_SHELL,  
     &                                   VOL_CORE, VOL_SHELL, DGN_CORE,      
     &                                   DGN_SHELL, XXLSG,
     &                                   BEXT_MODE, BSCAT_MODE, GBAR_MODE, SUCCESS )

                  VFAC  = 1.0 ! correction factor 

                  IF ( .NOT. SUCCESS ) THEN
     
                     WRITE( LOGDEV,9503 ) L, MODE, WAVELENGTH( V ), 
     &               AE_DGN_CORE( MODE,L ), AE_DGN_SHELL( MODE,L ),
     &               AE_NR_SHELL( MODE,L,V ), AE_NI_SHELL( MODE,L,V ),
     &               AE_NR_CORE( MODE,L,V ),  AE_NI_CORE( MODE,L,V ),
     &               BLK_AE_LSG( MODE,L )
     
                     XMSG =  'FAILURE IN AERO_OPTICAL_CS: core with shell case '
                     CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                     
                  END IF

C***sum to get total extinction and scattering
C***  and contribution to the overall asymmetry factor

                  BEXT  = BEXT  + BEXT_MODE
                  BSCAT = BSCAT + BSCAT_MODE
                  G_BAR = G_BAR + BSCAT_MODE * GBAR_MODE

               ELSE IF ( VOL .GT. 0.0 ) THEN

!*** FSB Internal Volume mixture case for optics 
                SELECT CASE( AEROSOL_OPTICS )
                  CASE( 1 )
                      CALL AERO_OPTICS_TABULAR( MODE,V,NR_SHELL, VOL, DGN_SHELL,
     &                                   XXLSG, BEXT_MODE, BSCAT_MODE, GBAR_MODE)
                      VFAC  = 1.0 ! correction factor
                  CASE( 2 )
                     IF ( SUCCESS ) CALL AERO_OPTICAL2( LAMDA_UM, NR_SHELL, VOL, 
     &                                   DGN_SHELL, XXLSG,
     &                                   BEXT_MODE, BSCAT_MODE, GBAR_MODE, SUCCESS )
                     VFAC  = 1.0 ! correction factor 
                  CASE DEFAULT ! AEROSOL_OPTICS equals 3
! use old fast optics method
                     IF ( SUCCESS ) CALL FAST_OPTICS( AE_NR_SHELL( MODE, L, V ),  
     &                                       AE_NI_SHELL( MODE, L, V ), 
     &                                       WAVELENGTH( V ), DGN_SHELL, XXLSG, 
     &                                       BEXT_MODE, BSCAT_MODE, GBAR_MODE )
                     VFAC  = VOL * INV_LAMBDA
! updated fast optics method; has problems from eposidically infinite values of aerosol optical depth
!                    MIE_PARAMETER = PI * DGN_SHELL * INV_LAMBDA
!     &                            * EXP( 3.0 * BLK_AE_LSG( MODE, L ) * BLK_AE_LSG( MODE, L ) )
!                    IF( SUCCESS ) CALL FASTER_OPTICS( AE_NR_SHELL( MODE, L, V ),  
!     &                                       AE_NI_SHELL( MODE, L, V ), 
!     &                                       MIE_PARAMETER, BLK_AE_LSG( MODE, L ), 
!     &                                       BEXT_MODE, BSCAT_MODE, GBAR_MODE )
!                    VFAC  = VOL * INV_LAMBDA
                  END SELECT
                   
                  IF ( BEXT_MODE .LT. 0.0 .OR. BEXT_MODE .LT. BSCAT_MODE .OR. BSCAT_MODE .LT. 0.0 ) THEN
                     TROUBLE = .TRUE.
                  END IF

                  IF ( BEXT_MODE .NE. BEXT_MODE .OR. BSCAT_MODE .NE. BSCAT_MODE ) THEN
                     TROUBLE = .TRUE.
                  END IF

                  IF ( GBAR_MODE .NE. GBAR_MODE .OR. ABS( GBAR_MODE ) .GT. 0.9999999 ) THEN
                     TROUBLE = .TRUE.
                  END IF

9402              format('layer = ',i2,' lambda  = ',es12.4,' dgn_core = ',es12.4,' dgn_shell = ',
     &                 es12.4,' ae_bext = ',es12.4,' ae_bscat = ',es12.4,' ae_gbar = ',es12.4,
     &                 ' refract_shell(nr,ni) = ', 2(es12.4,1x),' refract_core = ', 2(es12.4,1x))

                  IF ( TROUBLE ) THEN

                     write(logdev, 9402)l, wavelength( V ), dgn_core, dgn_shell, bext_mode, 
     &               bscat_mode, gbar_mode, ae_nr_shell( mode, l, v ), ae_ni_shell( mode, l, v ),
     &               ae_nr_core( mode, l, v ), ae_ni_core( mode, l, v )
!                    write(6, 9402)l, wavelength( V ), dgn_core, dgn_shell, bext_mode, 
!    &               bscat_mode, gbar_mode, ae_nr_shell( mode, l, v ), ae_ni_shell( mode, l, v ),
!    &               ae_nr_core( mode, l, v ), ae_ni_core( mode, l, v )

                     WRITE( LOGDEV, 9504 ) L, MODE, WAVELENGTH( V ), 
     &               AE_DGN_SHELL( MODE,L ), AE_NR_SHELL( MODE,L,V ),
     &               AE_NI_SHELL( MODE,L,V ), BLK_AE_VOL( MODE,L ),
     &               BLK_AE_LSG( MODE,L )
     
                     XMSG =  'FAILURE IN AERO_OPTICAL2: shell only case '
                     CALL M3WARN( PNAME, 0, 0, XMSG )
                     
                  END IF

                  IF ( .NOT. SUCCESS ) THEN

                     write(logdev, 9402)l, wavelength( V ), dgn_core, dgn_shell, bext_mode, 
     &               bscat_mode, gbar_mode, ae_nr_shell( mode, l, v ), ae_ni_shell( mode, l, v ),
     &               ae_nr_core( mode, l, v ), ae_ni_core( mode, l, v )
!                    write(6, 9402)l, wavelength( V ), dgn_core, dgn_shell, bext_mode, 
!    &               bscat_mode, gbar_mode, ae_nr_shell( mode, l, v ), ae_ni_shell( mode, l, v ),
!    &               ae_nr_core( mode, l, v ), ae_ni_core( mode, l, v )

                     WRITE( LOGDEV, 9504) L, MODE, WAVELENGTH( V ), 
     &               AE_DGN_SHELL( MODE,L ), AE_NR_SHELL( MODE,L,V ),
     &               AE_NI_SHELL( MODE,L,V ), BLK_AE_VOL( MODE,L ),
     &               BLK_AE_LSG( MODE,L )
     
                     XMSG =  'FAILURE IN AERO_OPTICAL2: shell only case '
                     CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
                     
                  END IF

C***sum to get total extinction and scattering
C***  and contribution to the overall asymmetry factor

                  BEXT  = BEXT + VFAC * BEXT_MODE
                  BSC   = VFAC * BSCAT_MODE
                  BSCAT = BSCAT + BSC
                  G_BAR = G_BAR + BSC * GBAR_MODE

               END IF       

            END DO OPTICS_LOOP

            AERO_SCAT_COEF( L,V ) = MAX( BSCAT,1.0E-30 )
            AERO_EXTI_COEF( L,V ) = MAX( BEXT, 1.0E-30 )
            AERO_ABSO_COEF( L,V ) = MAX( (BEXT-BSCAT), 1.0E-30 )
            AERO_ASYM_FAC ( L,V ) = G_BAR / MAX( BSCAT, 1.0E-30 )

#ifdef verbose_phot_extra
            if( l .eq. 1 .and. v .eq. 1)then
               write(logdev, 9502)l, wavelength( V ), dgn_core, dgn_shell, bext_mode, 
     &         bscat_mode, gbar_mode, ae_nr_shell( l, mode, v ), ae_ni_shell( v, mode, v ),
     &         ae_nr_core( l, mode, iv ), ae_ni_core( l, mode, v )
9502           format('layer = ',i2,' lambda  = ',es12.4,' dgn_core = ',es12.4,' dgn_shell = ',
     &                 es12.4,' ae_bext = ',es12.4,' ae_bscat = ',es12.4,' ae_gbar = ',es12.4,
     &                 ' refract_shell(nr,ni) = ', 2(es12.4,1x),' refract_core = ', 2(es12.4,1x))
            end if
#endif            

         END DO LAY_LOOP2         

      END DO LOOP_WAVE

      IF( CALCULATE_EXT_550 )THEN
!         IF( USE_ANGSTROM_INTERP .AND. FIRSTCALL )THEN
!          WRITE(LOGDEV,'(A,2(F7.3,1X))')'Angstrom Interpolation Wavelengths: ', 
!     &     WAVELENGTH( IWL_ANGSTROM_LOWER ), WAVELENGTH( IWL_ANGSTROM_UPPER  )
!           WRITE(LOGDEV,'(A,2(ES12.4,1X))')'Angstrom Span, Ratio: ', 
!     &     ANGSTROM_SPAN, ANGSTROM_RATIO
!         END IF
         LOOP_550NM: DO L = 1, NLAYS
              IF( USE_ANGSTROM_INTERP )THEN
!                ANGSTROM_EXPONENT  =  - REAL( LOG( AERO_EXTI_COEF( L,IWL_ANGSTROM_LOWER ) 
!    &                                         / AERO_EXTI_COEF( L,IWL_ANGSTROM_UPPER ) ), 8 )

                 ANGSTROM_EXPONENT  =  - REAL( LOG( MAX( AERO_EXTI_COEF( L,IWL_ANGSTROM_LOWER ),1.0E-30 )
     &                              /               MAX( AERO_EXTI_COEF( L,IWL_ANGSTROM_UPPER ),1.0E-30 ) ), 8 )
     &                              *  ANGSTROM_SPAN

                 AERO_EXTI_550( L ) =  AERO_EXTI_COEF( L,IWL_ANGSTROM_LOWER ) 
     &                              *  REAL( ANGSTROM_RATIO ** ANGSTROM_EXPONENT, 4 )

                 ANGSTROM_EXPONENT  =  - REAL( LOG( MAX( AERO_SCAT_COEF( L,IWL_ANGSTROM_LOWER ),1.0E-30 )
     &                              /               MAX( AERO_SCAT_COEF( L,IWL_ANGSTROM_UPPER ),1.0E-30 ) ), 8 )
     &                              *  ANGSTROM_SPAN

                 AERO_ABSO_550( L ) =  MAX( AERO_EXTI_550( L ) 
     &                              -  AERO_SCAT_COEF( L,IWL_ANGSTROM_LOWER )
     &                              *  REAL( ANGSTROM_RATIO ** ANGSTROM_EXPONENT, 4 ), 0.0 )

!                 IF ( FIRSTCALL ) THEN
!                     WRITE( LOGDEV,'(I3,A,7(ES12.4,1X))')L,
!     &              ' AERO_EXT_LOWER,AERO_EXT_UPPER,EXPONENT,EXPONENT*LOG(RATIO),EXT_550_1,EXT_550_2: ',
!     &               AERO_EXTI_COEF( L,IWL_ANGSTROM_LOWER ), AERO_EXTI_COEF( L,IWL_ANGSTROM_UPPER ),
!     &               ANGSTROM_EXPONENT,REAL( ANGSTROM_EXPONENT * LOG_ANGSTROM_RATIO,4),AERO_EXTI_550( L ),
!     &               AERO_EXTI_COEF( L,IWL_ANGSTROM_LOWER)*ANGSTROM_RATIO ** ANGSTROM_EXPONENT
!                 END IF
              ELSE
                 AERO_EXTI_550( L ) =  AERO_EXTI_COEF( L,IWL_ANGSTROM_LOWER )
              END IF
         END DO LOOP_550NM
!         IF ( FIRSTCALL ) FIRSTCALL = .FALSE.
      END IF 
      
#ifdef verbose_phot
      if ( col .eq. 1 .and. row .eq. 1 ) then
         l = 1
         v = 1
         do mode = 1, n_mode
            write(logdev, 5005)l, mode, v, ae_nr_core ( mode, l, v ), ae_ni_core ( mode, l, v ),
     &      ae_nr_shell ( mode, l, v ), ae_ni_shell ( mode, l, v )
5005        format('lay = ',i3,' mode = ',i3,' wlv index = ',i3,' ae_core(nr,ni) = ',
     &      2(es12.4,1x), ' ae_shell(nr,ni) = ', 2(es12.4,1x))
         end do
      end if
#endif

9503  FORMAT('LAYER = ',I3,' MODE = ',I3,' LAMBDA(nm)  = ',ES12.4,' DGN_CORE(m) = ',ES12.4,
     &       ' DGN_SHELL(m) = ', ES12.4 / ' REFRACT_IDX_SHELL(NR,NI) = ', 2(ES12.4,1X),
     &       ' REFRACT_IDX_CORE(NR,NI) = ', 2(ES12.4,1X) / ' LN(GEO.STD.DEV.) = ',
     &       ES12.4)
9504  FORMAT('LAYER = ',I3,' MODE = ',I3,' LAMBDA(nm)  = ',ES12.4,' DGN(m) = ',ES12.4,
     &       ' REFRACT_IDX(NR,NI) = ', 2(ES12.4,1X) / ' VOL.DENS. = ', ES12.4,
     &       ' LN(GEO.STD.DEV.) = ', ES12.4)

      RETURN
      END SUBROUTINE GET_AERO_DATA
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE FASTER_OPTICS ( NR, NI, ALPHV, XLNSIG, BETA_EXT, BETA_SCAT, G )
C-----------------------------------------------------------------------
C  A parameterization of the aerosol extinction and scattering code
C     Coded by Dr. Francis S. Binkowski
C     CEMP, The Institute for the Environment
C     The University of North Carolina at Chapel Hill
C     email: frank_binkowski@unc.edu
C     Code dates. Began February 25, 2005, current March 18, 2005
C     modified by FSB May 10, 2005 to calculate asymmetry factor by the
C     semi-empirical method of Hanna and Mathur. Note that
C     the normalized scattering coefficient (non-dimensional) is
C     interpreted as an average of  scattering efficiency over the
C     size distribution
C     Modified 05/25/2005 to ensure that if ni = 0.0, that
C     beta_scat = beta_ext.
C     Modified 03/17/2008 to ensure match with reference calculation for
C     all cases of sigma_g.
C     Modified 10/25/2009 to be consistent with research version of code.
C     The modifications include better representation of the
C     asymmetry factor.
C     The form of the mathematical expression for extinction
C     and scattering is based upon that of
C     Heintzenberg & Baker (1976), and further inspired by the work
C     of Willeke and Brockmann(1977) who showed the basic shape of
C     the extinction resulting from an integration of the Mie extinction
C     efficiency over a log normal particle size distribution.
C     This current code has been developed from a direct calculation of
C     extinction and scattering using a Mie code integrated over
C     a log normal distribution. The results of that calculation
C     replicate the Willee and Brockmann (1977) results.
C     The parameterization is based upon a fit to the standard CMAQ
C     radiative transfer code for visibility. to which a module for
C     absorption was added so that absorption and scattering would also be
C     available.
C
C     The  shape has been modified for absorption by small particles
C     by using the formulae of Penndorf (1962). For large particles,
C     the shape has been altered by using the large sphere values from
C     Table I of  Irvine (1965)
C
C     Results are consistent with those of Willeke and Brockmann (1977).
C
C     The asymmetry factor is calculated by a new algorithm FSB - 04/04/2008
C     Sept 2014: J.Young-change arguments and usage of Mie Paramater attempting
C                to optimize runtimes or efficiency
C     Oct  2014: B.Hutzell-Added mininum values for scatttering efficiency for small
C                highly absorbing particles such as Aitken particle that
C                are mostly composed of elemental carbon
C
C  References:
C
C     Heintzenberg, J. and M. Baker, Spherical particle populations:
C     approximate analytic relationship between size distribution
C     parameters and integral optical properties, Applied Optics,
C     Volume 15, Number 5 pp 1178-1181, May 1976.
C
C     Irvine, W. M. Light Scattering by spherical particles: Radiation
C     pressure, asymmetry factor, and extinction cross section,
C     Journal of eh Optical Society of Amerioca, Vol. 55, NO. 1,
C     January ,1965
C
C     Penndorf, R. Scattering and Extinction Coefficeints for small
C     spherical particles, J. Atmospheric Sciences, Volume 19, p 193,
C     March 1962.
C
C     Willeke, K. and  J. E. Brockmann, Extinction coefficients for
C     multmodal atmospheric particle size distributions, Atmospheric
C     Environment, vol. 11, pp 95-999, 1977.
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE SUBST_CONST      ! physical constants

C***arguments

      REAL, INTENT( IN )  :: NR, NI     ! indices of refraction
      REAL, INTENT( IN )  :: ALPHV      ! Mie parameter for dgv
      REAL, INTENT( IN )  :: XLNSIG     ! log of geometric standard deviation

      REAL, INTENT( OUT ) :: BETA_EXT   ! normalized extinction coefficient
      REAL, INTENT( OUT ) :: BETA_SCAT  ! normalized scattering coefficient
      REAL, INTENT( OUT ) :: G          ! asymmetry factor

C***internal variables

      REAL NR1, NR2, TAU
      REAL C, CEXT, CSCAT
      REAL B, BEXT, BSCAT
      REAL BBFAC
      REAL ALPHA_I
      REAL A, LOGX2, XLNSIG2, MM1

      REAL, PARAMETER :: SIGMA_G_TWO = 2.0
      REAL, PARAMETER :: LOGSIG_TWO = 0.693147
      REAL, PARAMETER :: LOGSIG_TWO_SQD = LOGSIG_TWO * LOGSIG_TWO
      REAL, PARAMETER :: LOGSIG_105 = 1.102500
      REAL, PARAMETER :: DIFFSIG = SIGMA_G_TWO - 1.05
      REAL, PARAMETER :: A_TWO = 0.5 / LOGSIG_TWO_SQD
      REAL LARGESCAT  ! large sphere limit for scattering
      REAL LARGEEXT   ! large sphere limit for extinction
      REAL SMALL_G, LARGE_G

      REAL ALPHV2, ALPHV3
      REAL X_ALPHA, X_ALPHA2, X_ALPHA3
      REAL SIGMA_G, FCORR
      REAL EXPFAC2, EXPFAC3
      REAL EFAC, EFAC_EXT, EFAC_SCAT
      REAL DEN1, FAC1, FAC2
      REAL F1, F2, F3
      REAL G1, G2, G3, G4, G5
      REAL NN, TT

      REAL T1F1, T2F1, T1F2, T2F2, T1F3, T2F3
      REAL T1G1, T2G1, T1G2, T2G2, T1G3, T2G3, T1G4, T2G4
      REAL T1G5, T2G5, T1P1, T2P1

C***the following are for calculating the Penndorff Coefficients

      REAL PENN1, A1, A2, A3, PENN2
      REAL XNR, XNI, XNR2, XNI2, XNRI, XNRI2, XNRMI
      REAL XRI, XRI2, XRI36, XNX, XNX2
      REAL Z1, Z12, Z2, XC1

C***coefficients for polynomials

      REAL, PARAMETER :: F1A0 = -1.309193E-1
      REAL, PARAMETER :: F1A1 =  2.565668E+0

      REAL, PARAMETER :: F2A0 =  1.489233E+1
      REAL, PARAMETER :: F2A1 = -5.311351E+1
      REAL, PARAMETER :: F2A2 =  8.180334E+1
      REAL, PARAMETER :: F2A3 = -4.549854E+1

      REAL, PARAMETER :: F3A0 =  5.549359E-1
      REAL, PARAMETER :: F3A1 =  2.571002E-1
      REAL, PARAMETER :: F3A2 =  9.212703E-1
      REAL, PARAMETER :: F3A3 =  5.155047E-2

      REAL, PARAMETER :: G1A0 =  1.000000E+0
      REAL, PARAMETER :: G1A1 = -7.636121E-1
      REAL, PARAMETER :: G1A2 =  1.285532E+0
      REAL, PARAMETER :: G1A3 =  1.629161E-2

      REAL, PARAMETER :: G2A0 =  1.000000E+0
      REAL, PARAMETER :: G2A1 = -2.507954E+0
      REAL, PARAMETER :: G2A2 =  5.170246E+0
      REAL, PARAMETER :: G2A3 = -4.945515E+0

      REAL, PARAMETER :: G3A0 =  1.000000E+0
      REAL, PARAMETER :: G3A1 = -2.650134E+0
      REAL, PARAMETER :: G3A2 =  4.515327E+0
      REAL, PARAMETER :: G3A3 = -4.027442E+0

      REAL, PARAMETER :: G4A0 =  1.000000E+0
      REAL, PARAMETER :: G4A1 = -8.532647E-1
      REAL, PARAMETER :: G4A2 =  6.766073E-1

      REAL, PARAMETER :: G5A0 =  1.0
      REAL, PARAMETER :: G5A1 = -6.674946E-1
      REAL, PARAMETER :: G5A2 = -5.426378E-1

C *** Coefficients for quadratic fit for <cos> large particles
C     from Irvine (1965) Table I
      REAL, PARAMETER :: IRVA0 =  1.076232E+0
      REAL, PARAMETER :: IRVA1 = -4.891972E-2
      REAL, PARAMETER :: IRVA2 = -2.243449E-2


C FSB Coefficients for SMALL_G  calculation
      REAL, PARAMETER :: XXA0 =  3.392224E-2
      REAL, PARAMETER :: XXA1 =  8.276532E-1
      REAL, PARAMETER :: XXA2 = -3.784926E-1
      REAL, PARAMETER :: XXA3 =  5.853108E-2
      REAL, PARAMETER :: G_ALPHA_I = XXA0 + XXA1 + XXA2 + XXA3

C FSB Coefficients for LARGE_G calculation
      REAL, PARAMETER :: YYA0 =  6.776173E-1
      REAL, PARAMETER :: YYA1 =  2.385339E-2
      REAL, PARAMETER :: YYA2 = -7.952263E-4
      REAL, PARAMETER :: YYA3 =  9.410370E-6

C FSB Coefficientf for adjusting asymmetry factor calculation
      REAL, PARAMETER :: CXG0 = -5.845196E-2
      REAL, PARAMETER :: CXG1 =  3.229183E-1
      REAL, PARAMETER :: CXG2 =  2.764716E-1
      REAL, PARAMETER :: CXG3 = -8.790411E-2

C***FSB added for asymmetry factor calculation

      REAL QSCAT_AVG

      REAL, PARAMETER :: AA0 =  88.466
      REAL, PARAMETER :: AA1 = -61.628
      REAL, PARAMETER :: AA2 =  34.483
      REAL, PARAMETER :: AA3 =  -8.543
      REAL, PARAMETER :: AA4 =   0.77435

      REAL GMAX
      REAL QANGLE
      REAL QQ, QF1, QF2, QF3
      REAL QQSUM, QQF1,QQF2, QQF3, QQCORR

      REAL, PARAMETER :: DEGTORAD = PI180
      REAL, PARAMETER :: THREE_PI_TWO = 3.0 * PI / 2.0

C***FSB start calculation
      SIGMA_G = EXP( XLNSIG )
C FSB check range of SIGMA_G
C *** Maximum value of SIGMA_G allowed is 2.0
      SIGMA_G = MIN( 2.0, SIGMA_G )
C *** Minimum allowed value fo SIGMA_F is 1.05
      SIGMA_G = MAX( 1.05, SIGMA_G )

      XLNSIG2 = XLNSIG * XLNSIG
      A = 0.5 / XLNSIG2

      NR1 = NR - 1.0
      NR2 = NR * NR

C***evaluate polynomials
C***  optimize for piplined microprocessor
C***  appproach recommended by Dr. Carlie Coats

      NN = NR1 * NR1

      T1F1 = F1A0 + F1A1 * NR1
      T1F2 = F2A0 + F2A1 * NR1
      T2F2 = F2A2 + F2A3 * NR1
      T1F3 = F3A0 + F3A1 * NR1
      T2F3 = F3A2 + F3A3 * NR1

      F1 = T1F1                 ! linear
      F2 = T1F2 + T2F2 * NN     ! cubic
      F3 = T1F3 + T2F3 * NN     ! cubic

      C = F1

C FSB correct for values of SIGMA_ less than 2.0.
C     The smallest value of SIGMA_G allowed is 1.05
C     2.0 - 1.05 = 0.95
C      FCORR   = (1.0 - 0.13 * (SIGMA_G - 1.05)  / 0.95 )
C     1.0 / 0.95 = 1.052632.  1.0 / 0.95 * 0.13 = 0.136842

      FCORR   = 1.0 - 0.136842 * ( SIGMA_G - 1.05 )

      CEXT    = C * FCORR
      CSCAT   = CEXT
      B       = F3 * A_TWO
      ALPHA_I = F2
      BEXT    = B
      BSCAT   = B
      PENN1   = 0.0
      PENN2   = 0.0

      ALPHV2 = ALPHV * ALPHV
      ALPHV3 = ALPHV2 * ALPHV

      IF ( NI .GT. 0.0 ) THEN

         TAU = NI / NR1

C***evaluate more polynomials
C***  optimize for piplined microprocessor
C***  appproach recommended by Dr. Carlie Coats

         TT = TAU * TAU
         T1G1 = G1A0 + G1A1 * TAU
         T2G1 = G1A2 + G1A3 * TAU
         T1G2 = G2A0 + G2A1 * TAU
         T2G2 = G2A2 + G2A3 * TAU
         T1G3 = G3A0 + G3A1 * TAU
         T2G3 = G3A2 + G3A3 * TAU
         T1G4 = G4A0 + G4A1 * TAU
         T2G4 = G4A2            ! quadratic
         T1G5 = G5A0 + G5A1 * TAU
         T2G5 = G5A2            ! quadratic
         G1   = T1G1 + T2G1 * TT
         G2   = T1G2 + T2G2 * TT
         G3   = T1G3 + T2G3 * TT
         G4   = T1G4 + T2G4 * TT
         G5   = T1G5 + T2G5 * TT

C *** adjust the variables

         CEXT    = CEXT    * G1
         CSCAT   = CSCAT   * G2
         BEXT    = BEXT    * G3
         BSCAT   = BSCAT   * G4
         ALPHA_I = ALPHA_I * G5

C*** Calculate the Penndorf Coefficients for the small particle limit

         XNR   = NR
         XNI   = NI
         XNR2  = XNR   * XNR
         XNI2  = XNI   * XNI
         XNRI  = XNR2  + XNI2
         XNRI2 = XNRI  * XNRI
         XNRMI = XNR2  - XNI2
         XRI   = XNR   * XNI
         XRI2  = XRI   * XRI
         XRI36 = 36.0  * XRI2
         XNX   = XNRI2 + XNRMI - 2.0
         XNX2  = XNX   * XNX

         Z1    = XNRI2 + 4.0 * XNRMI + 4.0
         Z12   = Z1    * Z1
         Z2    = 4.0   * XNRI2 + 12.0 * XNRMI + 9.0
         XC1   = 8.0   / ( 3.0 * Z12 )
         A1    = 24.0  * XRI / Z1

         A2    = 4.0   * XRI / 15.0 + 20.0 * XRI / ( 3.0 * Z2 ) +
     &           4.8   * XRI * (  7.0 * XNRI2 +
     &           4.0   * ( XNRMI - 5.0 ) ) / Z12

         A3    = XC1   * ( XNX2 - XRI36 )

!        ALPHV2 = ALPHV * ALPHV
!        ALPHV3 = ALPHV2 * ALPHV
         EXPFAC2 = EXP( 2.0 * XLNSIG2 )
         EXPFAC3 = EXP( 4.5 * XLNSIG2 )

         T1P1 = A1 + A2 * ALPHV2 * EXPFAC2
         T2P1 = A3 * ALPHV3 * EXPFAC3

C***PENN1 is the analytic integral of the Pendorff formulae over
C***   a log normal particle size distribution.

         PENN1 = THREE_PI_TWO * ( T1P1 + T2P1 )
         PENN2 = THREE_PI_TWO * T2P1

      END IF   ! test for ni > 0.0

      X_ALPHA  = ALPHV / ALPHA_I

!     LOGX2    = LOG( X_ALPHA )** 2
      LOGX2    = LOG( X_ALPHA )
      LOGX2    = LOGX2 * LOGX2

      BBFAC    = BEXT * A  / ( BEXT + A )

      EFAC_EXT = EXP( -BBFAC * LOGX2 )

C***FSB calculate normalized extinction and scattering coefficients

      BETA_EXT = THREE_PI_TWO * CEXT
     &         * SQRT( A / ( BEXT + A ) ) * EFAC_EXT

      BETA_SCAT = BETA_EXT      ! NI = 0.0 case

C *** Check for NI > 0.0

      IF ( NI .GT. 0.0 ) THEN

!        BBFAC = 1.5 * BBFAC ! reset BBFAC for scattering
         BBFAC = 1.1 * BBFAC ! reset BBFAC for scattering

         EFAC_SCAT = EXP( -BBFAC * LOGX2 )

C *** recalculate the normalized scattering coefficient

         BETA_SCAT = THREE_PI_TWO * CSCAT
     &             * SQRT( A / ( BSCAT + A ) ) * EFAC_SCAT

C *** Adjust beta_ext for small particle absorption:

         IF ( X_ALPHA .LT. 0.13 ) THEN

            FAC1 = ALPHV + 0.6 * ALPHV2

            BETA_EXT = MAX( BETA_EXT,
     &                    ( BETA_EXT  * FAC1 + ( 1.0 - FAC1 ) * PENN1 ) )

            BETA_SCAT = MAX( BETA_SCAT,
     &                     ( BETA_SCAT * FAC1 + ( 1.0 - FAC1 ) * PENN2 ) )

         END IF ! test for   XALPHA < 0.13

! small highly absorbing  particles such as a Tyndall scatterers such as
! described in Jacobson (1999) in Fundmentals of Atmospheric Modeling. This
! is approximately consistent with Penndorf (1962).

        BETA_SCAT = MAX( 0.001 * BETA_EXT, BETA_SCAT )

      END IF  ! test for NI > 0.0

C *** Calculate large sphere limits(Irvine, 1965)

      MM1 = EXP( 0.5 * XLNSIG2 ) / ALPHV

C FSB large sphere limit - scattering

      LARGESCAT = THREE_PI_TWO * ( 8.652439E-01 + 1.501772E-01 * NR ) * MM1

C FSB large spnere limit for extinction ( no edge effects )

      LARGEEXT = THREE_PI_TWO * ( 2.0 * MM1 )

C FSB Adjust for large sphere limits

      IF ( ALPHV .GE. 10.0 ) THEN

         BETA_EXT  = MAX( BETA_EXT, LARGEEXT )

         BETA_SCAT = MAX( BETA_SCAT, LARGESCAT )

      END IF ! test for 10.0 < ALPHV

C***FSB now calculate the asymmetry factor
C***  using the Hanna-Mathur quasi-empirical method
!     GMAX = 0.0

!     QSCAT_AVG = BETA_SCAT
!     QANGLE = AA0

!     IF ( QSCAT_AVG .GT. 1.0E-4 ) THEN
!        QQ  = QSCAT_AVG * QSCAT_AVG
!        QF1 = AA0 + AA1 * QSCAT_AVG
!        QF2 = AA2 + AA3 * QSCAT_AVG
!        QF3 = AA4 * QSCAT_AVG * QSCAT_AVG
!        QANGLE = QF1 + QF2 * QQ + QF3 * QQ
!     END IF

!     QANGLE = DEGTORAD * QANGLE
!     G      = COS( QANGLE)     ! asymmetry factor

C***FSB now calculate the asymmetry factor
C***   using a parametric fit to Mie calculations

      X_ALPHA2 = X_ALPHA * X_ALPHA
      X_ALPHA3 = X_ALPHA2 * X_ALPHA

      SMALL_G = 0.0
      LARGE_G = 0.0

      IF ( ALPHV .LT. 2.6 ) THEN

C FSB Calculate SMALL_G for sigma_g = 2.0

         QF1 = XXA0 + XXA1 * ALPHV
         QF2 = XXA2 * ALPHV2
         QF3 = XXA3 * ALPHV3
         SMALL_G = QF1 + QF2 + QF3 ! valid at sigma_g = 2.0

C FSB adjust SMALL_G for values less than 2.0
C     The smallest sigma_g allowed is 1.05

         QQF1 = CXG0 + CXG1 * ALPHV
         QQF2 = CXG2 * ALPHV2
         QQF3 = CXG3 * ALPHV3
         QQSUM = QQF1 + QQF2 + QQF3
         FAC1 = 1.052632 * ( 2.0 - SIGMA_G )
         QQSUM = MAX( 0.0, QQSUM )
         QQCORR = ( 1.0 - FAC1 ) + FAC1 * QQSUM
         SMALL_G = SMALL_G * QQCORR
         G = SMALL_G

      ELSE IF ( ALPHV .LT. 100.0 ) THEN

         QF1 = YYA0 + YYA1 * X_ALPHA
         QF2 = YYA2 * X_ALPHA2
         QF3 = YYA3 * X_ALPHA3
         LARGE_G = QF1 + QF2 + QF3
         G = LARGE_G

      ELSE

C     Very large sphere limit from fit to Irvine (1965).

         G = 1.124484 - 1.153869E-01 * NR

      END IF

      RETURN
      END SUBROUTINE FASTER_OPTICS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE FAST_OPTICS( NR, NI, LAMBDA, DGN, SIGMA_G, BETA_EXT, BETA_SCAT, G )
C-----------------------------------------------------------------------
C  A parameterization of the aerosol extinction and scattering code
C     Coded by Dr. Francis S. Binkowski
C     CEMP, The Institute for the Environment
C     The University of North Carolina at Chapel Hill
C     email: frank_binkowski@unc.edu
C     Code dates. Began February 25, 2005, current March 18, 2005
C     modified by FSB May 10, 2005 to calculate asymmetry factor by the
C     semi-empirical method of Hanna and Mathur. Note that
C     the normalized scattering coefficient (non-dimensional) is
C     interpreted as an average of  scattering efficiency over the
C     size distribution
C     Modified 05/25/2005 to ensure that if ni = 0.0, that
C     beta_scat = beta_ext.
C     Modified 03/17/2008 to ensure match with reference calculation for
C     all cases of sigma_g.
C     Modified 10/25/2009 to be consistent with research version of code.
C     The modifications include better representation of the
C     asymmetry factor.
C     The form of the mathematical expression for extinction
C     and scattering is based upon that of
C     Heintzenberg & Baker (1976), and further inspired by the work
C     of Willeke and Brockmann(1977) who showed the basic shape of
C     the extinction resulting from an integration of the Mie extinction
C     efficiency over a log normal particle size distribution.
C     This current code has been developed from a direct calculation of
C     extinction and scattering using a Mie code integrated over
C     a log normal distribution. The results of that calculation
C     replicate the Willee and Brockmann (1977) results.
C     The parameterization is based upon a fit to the standard CMAQ
C     radiative transfer code for visibility. to which a module for
C     absorption was added so that absorption and scattering would also be
C     available.
C
C     The  shape has been modified for absorption by small particles
C     by using the formulae of Penndorf (1962). For large particles,
C     the shape has been altered by using the large sphere values from
C     Table I of  Irvine (1965)
C
C     Results are consistent with those of Willeke and Brockmann (1977).
C
C     The asymmetry factor is calculated by a new algorithm FSB - 04/04/2008
C
C     Oct  2014: B.Hutzell-Added mininum values for scatttering efficiency for small
C                highly absorbing particles such as Aitken particle that
C                are mostly composed of elemental carbon
C
C  References:
C
C     Heintzenberg, J. and M. Baker, Spherical particle populations:
C     approximate analytic relationship between size distribution
C     parameters and integral optical properties, Applied Optics,
C     Volume 15, Number 5 pp 1178-1181, May 1976.
C
C     Irvine, W. M. Light Scattering by spherical particles: Radiation
C     pressure, asymmetry factor, and extinction cross section,
C     Journal of eh Optical Society of Amerioca, Vol. 55, NO. 1,
C     January ,1965
C
C     Penndorf, R. Scattering and Extinction Coefficeints for small
C     spherical particles, J. Atmospheric Sciences, Volume 19, p 193,
C     March 1962.
C
C     Willeke, K. and  J. E. Brockmann, Extinction coefficients for
C     multmodal atmospheric particle size distributions, Atmospheric
C     Environment, vol. 11, pp 95-999, 1977.
C-----------------------------------------------------------------------

      IMPLICIT NONE

C***include files

      INCLUDE SUBST_CONST      ! physical constants

C***arguments

      REAL, INTENT( IN )     :: NR, NI     ! indices of refraction
      REAL, INTENT( IN )     :: LAMBDA     ! wavelength, nm
      REAL, INTENT( IN )     :: DGN        ! geometric mean diameter, m
      REAL, INTENT( INOUT )  :: SIGMA_G     ! geometric standard deviation

      REAL, INTENT( OUT )    :: BETA_EXT   ! normalized extinction coefficient
      REAL, INTENT( OUT )    :: BETA_SCAT  ! normalized scattering coefficient
      REAL, INTENT( OUT )    :: G          ! asymmetry factor

C***internal variables

      REAL NR1, NR2, TAU
      REAL C, CEXT, CSCAT
      REAL B, BEXT, BSCAT
      REAL BBFAC
      REAL ALPHV
      REAL ALPHA_I
      REAL A, LOGX2, XLNSIG, XLNSIG2, MM1

      REAL, PARAMETER :: SIGMA_G_TWO = 2.0
      REAL, PARAMETER :: LOGSIG_TWO = 0.693147
      REAL, PARAMETER :: LOGSIG_TWO_SQD = LOGSIG_TWO * LOGSIG_TWO
      REAL, PARAMETER :: LOGSIG_105 = 1.102500
      REAL, PARAMETER :: DIFFSIG = SIGMA_G_TWO - 1.05
      REAL, PARAMETER :: A_TWO = 0.5 / LOGSIG_TWO_SQD
      REAL LARGESCAT  ! large sphere limit for scattering
      REAL LARGEEXT   ! large sphere limit for extinction
      REAL SMALL_G, LARGE_G

      REAL ALPHV2, ALPHV3
      REAL X_ALPHA, X_ALPHA2, X_ALPHA3
      REAL FCORR
      REAL EXPFAC2, EXPFAC3
      REAL EFAC, EFAC_EXT, EFAC_SCAT
      REAL DEN1, FAC1, FAC2
      REAL F1, F2, F3
      REAL G1, G2, G3, G4, G5
      REAL NN, TT

      REAL T1F1, T2F1, T1F2, T2F2, T1F3, T2F3
      REAL T1G1, T2G1, T1G2, T2G2, T1G3, T2G3, T1G4, T2G4
      REAL T1G5, T2G5, T1P1, T2P1

C***the following are for calculating the Penndorff Coefficients

      REAL PENN1, A1, A2, A3, PENN2
      REAL XNR, XNI, XNR2, XNI2, XNRI, XNRI2, XNRMI
      REAL XRI, XRI2, XRI36, XNX, XNX2
      REAL Z1, Z12, Z2, XC1

C***coefficients for polynomials

      REAL, PARAMETER :: F1A0 = -1.309193E-1
      REAL, PARAMETER :: F1A1 =  2.565668E+0

      REAL, PARAMETER :: F2A0 =  1.489233E+1
      REAL, PARAMETER :: F2A1 = -5.311351E+1
      REAL, PARAMETER :: F2A2 =  8.180334E+1
      REAL, PARAMETER :: F2A3 = -4.549854E+1

      REAL, PARAMETER :: F3A0 =  5.549359E-1
      REAL, PARAMETER :: F3A1 =  2.571002E-1
      REAL, PARAMETER :: F3A2 =  9.212703E-1
      REAL, PARAMETER :: F3A3 =  5.155047E-2

      REAL, PARAMETER :: G1A0 =  1.000000E+0
      REAL, PARAMETER :: G1A1 = -7.636121E-1
      REAL, PARAMETER :: G1A2 =  1.285532E+0
      REAL, PARAMETER :: G1A3 =  1.629161E-2

      REAL, PARAMETER :: G2A0 =  1.000000E+0
      REAL, PARAMETER :: G2A1 = -2.507954E+0
      REAL, PARAMETER :: G2A2 =  5.170246E+0
      REAL, PARAMETER :: G2A3 = -4.945515E+0

      REAL, PARAMETER :: G3A0 =  1.000000E+0
      REAL, PARAMETER :: G3A1 = -2.650134E+0
      REAL, PARAMETER :: G3A2 =  4.515327E+0
      REAL, PARAMETER :: G3A3 = -4.027442E+0

      REAL, PARAMETER :: G4A0 =  1.000000E+0
      REAL, PARAMETER :: G4A1 = -8.532647E-1
      REAL, PARAMETER :: G4A2 =  6.766073E-1

      REAL, PARAMETER :: G5A0 =  1.0
      REAL, PARAMETER :: G5A1 = -6.674946E-1
      REAL, PARAMETER :: G5A2 = -5.426378E-1

C *** Coefficients for quadratic fit for <cos> large particles
C     from Irvine (1965) Table I
      REAL, PARAMETER :: IRVA0 =  1.076232E+0
      REAL, PARAMETER :: IRVA1 = -4.891972E-2
      REAL, PARAMETER :: IRVA2 = -2.243449E-2


C FSB Coefficients for SMALL_G  calculation
      REAL, PARAMETER :: XXA0 =  3.392224E-2
      REAL, PARAMETER :: XXA1 =  8.276532E-1
      REAL, PARAMETER :: XXA2 = -3.784926E-1
      REAL, PARAMETER :: XXA3 =  5.853108E-2
      REAL, PARAMETER :: G_ALPHA_I = XXA0 + XXA1 + XXA2 + XXA3

C FSB Coefficients for LARGE_G calculation
      REAL, PARAMETER :: YYA0 =  6.776173E-1
      REAL, PARAMETER :: YYA1 =  2.385339E-2
      REAL, PARAMETER :: YYA2 = -7.952263E-4
      REAL, PARAMETER :: YYA3 =  9.410370E-6

C FSB Coefficientf for adjusting asymmetry factor calculation
      REAL, PARAMETER :: CXG0 = -5.845196E-2
      REAL, PARAMETER :: CXG1 =  3.229183E-1
      REAL, PARAMETER :: CXG2 =  2.764716E-1
      REAL, PARAMETER :: CXG3 = -8.790411E-2

C***FSB added for asymmetry factor calculation

      REAL QSCAT_AVG

      REAL, PARAMETER :: AA0 =  88.466
      REAL, PARAMETER :: AA1 = -61.628
      REAL, PARAMETER :: AA2 =  34.483
      REAL, PARAMETER :: AA3 =  -8.543
      REAL, PARAMETER :: AA4 =   0.77435

      REAL GMAX
      REAL QANGLE
      REAL QQ, QF1, QF2, QF3
      REAL QQSUM, QQF1,QQF2, QQF3, QQCORR

      REAL, PARAMETER :: DEGTORAD = PI180
      REAL, PARAMETER :: THREE_PI_TWO = 3.0 * PI / 2.0


      REAL, PARAMETER :: SCALE = 1.00E+9

C FSB check range of SIGMA_G
C *** Maximum value of SIGMA_G allowed is 2.0
      SIGMA_G = MIN( 2.0, SIGMA_G )
C *** Minimum allowed value fo SIGMA_F is 1.05
      SIGMA_G = MAX( 1.05, SIGMA_G )
C***FSB start calculation
      XLNSIG = LOG( SIGMA_G ) 

      ALPHV = SCALE * PI * DGN * EXP( 3.0 * XLNSIG * XLNSIG ) / LAMBDA
      ALPHV2 = ALPHV * ALPHV
      ALPHV3 = ALPHV * ALPHV * ALPHV

      XLNSIG2 = XLNSIG * XLNSIG
      A = 0.5 / XLNSIG2

      NR1 = NR - 1.0
      NR2 = NR * NR

C***evaluate polynomials
C***  optimize for piplined microprocessor
C***  appproach recommended by Dr. Carlie Coats

      NN = NR1 * NR1

      T1F1 = F1A0 + F1A1 * NR1
      T1F2 = F2A0 + F2A1 * NR1
      T2F2 = F2A2 + F2A3 * NR1
      T1F3 = F3A0 + F3A1 * NR1
      T2F3 = F3A2 + F3A3 * NR1

      F1 = T1F1                 ! linear
      F2 = T1F2 + T2F2 * NN     ! cubic
      F3 = T1F3 + T2F3 * NN     ! cubic

      C       = F1

C FSB correct for values of SIGMA_ less than 2.0.
C     The smallest value of SIGMA_G allowed is 1.05
C     2.0 - 1.05 = 0.95
C      FCORR   = (1.0 - 0.13 * (SIGMA_G - 1.05)  / 0.95 )
C     1.0 / 0.95 = 1.052632.  1.0 / 0.95 * 0.13 = 0.136842

      FCORR   = 1.0 - 0.136842 * ( SIGMA_G - 1.05 )

      CEXT    = C * FCORR
      CSCAT   = CEXT
      B       = F3 * A_TWO
      ALPHA_I = F2
      BEXT    = B
      BSCAT   = B
      PENN1   = 0.0
      PENN2   = 0.0

      IF ( NI .GT. 0.0 ) THEN

         TAU = NI / NR1

C***evaluate more polynomials
C***  optimize for piplined microprocessor
C***  appproach recommended by Dr. Carlie Coats

         TT = TAU * TAU
         T1G1 = G1A0 + G1A1 * TAU
         T2G1 = G1A2 + G1A3 * TAU
         T1G2 = G2A0 + G2A1 * TAU
         T2G2 = G2A2 + G2A3 * TAU
         T1G3 = G3A0 + G3A1 * TAU
         T2G3 = G3A2 + G3A3 * TAU
         T1G4 = G4A0 + G4A1 * TAU
         T2G4 = G4A2            ! quadratic
         T1G5 = G5A0 + G5A1 * TAU
         T2G5 = G5A2            ! quadratic
         G1   = T1G1 + T2G1 * TT
         G2   = T1G2 + T2G2 * TT
         G3   = T1G3 + T2G3 * TT
         G4   = T1G4 + T2G4 * TT
         G5   = T1G5 + T2G5 * TT

C *** adjust the variables

         CEXT    = CEXT    * G1
         CSCAT   = CSCAT   * G2
         BEXT    = BEXT    * G3
         BSCAT   = BSCAT   * G4
         ALPHA_I = ALPHA_I * G5

C*** Calculate the Penndorf Coefficients for the small particle limit

         XNR   = NR
         XNI   = NI
         XNR2  = XNR   * XNR
         XNI2  = XNI   * XNI
         XNRI  = XNR2  + XNI2
         XNRI2 = XNRI  * XNRI
         XNRMI = XNR2  - XNI2
         XRI   = XNR   * XNI
         XRI2  = XRI   * XRI
         XRI36 = 36.0  * XRI2
         XNX   = XNRI2 + XNRMI - 2.0
         XNX2  = XNX   * XNX

         Z1    = XNRI2 + 4.0 * XNRMI + 4.0
         Z12   = Z1    * Z1
         Z2    = 4.0   * XNRI2 + 12.0 * XNRMI + 9.0
         XC1   = 8.0   / ( 3.0 * Z12 )
         A1    = 24.0  * XRI / Z1

         A2    = 4.0   * XRI / 15.0 + 20.0 * XRI / ( 3.0 * Z2 ) +
     &           4.8   * XRI * (  7.0 * XNRI2 +
     &           4.0   * ( XNRMI - 5.0 ) ) / Z12

         A3    = XC1   * ( XNX2 - XRI36 )

         EXPFAC2 = EXP( 2.0 * XLNSIG2 )
         EXPFAC3 = EXP( 4.5 * XLNSIG2 )

         T1P1 = A1 + A2 * ALPHV2 * EXPFAC2
         T2P1 = A3 * ALPHV3 * EXPFAC3

C***PENN1 is the analytic integral of the Pendorff formulae over
C***   a log normal particle size distribution.

         PENN1 = THREE_PI_TWO * ( T1P1 + T2P1 )
         PENN2 = THREE_PI_TWO * T2P1

      END IF                    ! test of ni > 0.0

      X_ALPHA  = ALPHV / ALPHA_I

      LOGX2    = LOG( X_ALPHA )** 2

      BBFAC    = BEXT * A  / ( BEXT + A )

      EFAC_EXT = EXP( -BBFAC * LOGX2 )

C***FSB calculate normalized extinction and scattering coefficients

      BETA_EXT = THREE_PI_TWO * CEXT *
     &                      SQRT( A / ( BEXT + A ) ) * EFAC_EXT

      BETA_SCAT = BETA_EXT      ! NI = 0.0 case

C *** Check for 0.0 < NI

      IF ( NI .GT. 0.0 ) THEN

!        BBFAC = 1.5 * BBFAC ! reset BBFAC for scattering
         BBFAC = 1.1 * BBFAC ! reset BBFAC for scattering

         EFAC_SCAT = EXP( -BBFAC * LOGX2 )

C *** recalculate the normalized scattering coefficient

         BETA_SCAT = THREE_PI_TWO * CSCAT *
     &                       SQRT( A / ( BSCAT + A ) ) * EFAC_SCAT

C *** Adjust beta_ext for small particle absorption:

         IF ( X_ALPHA .LT. 0.13 ) THEN

            FAC1 = ALPHV + 0.6 * ALPHV2

            BETA_EXT = MAX( BETA_EXT,
     &                 ( BETA_EXT  * FAC1 + ( 1.0 - FAC1 ) * PENN1 ) )

            BETA_SCAT = MAX( BETA_SCAT,
     &                 ( BETA_SCAT * FAC1 + ( 1.0 - FAC1 ) * PENN2 ) )

         END IF ! test for   XALPHA < 0.13
        
! small highly absorbing  particles such as a Tyndall scatterers such as
! described in Jacobson (1999) in Fundmentals of Atmospheric Modeling. This
! is approximately consistent with Penndorf (1962).

         BETA_SCAT = MAX( 0.001 * BETA_EXT, BETA_SCAT )
          
      END IF  ! test for 0.0  < NI
C *** Calculate large sphere limits(Irvine, 1965)

      MM1 = EXP( 0.5 * XLNSIG2 ) / ALPHV

C FSB large sphere limit - scattering

      LARGESCAT = THREE_PI_TWO * ( 8.652439E-1 + 1.501772E-1 * NR ) * MM1

C FSB large spnere limit for extinction ( no edge effectss )

      LARGEEXT = THREE_PI_TWO * ( 2.0 * MM1 )

C FSB Adjust for large sphere limits

      IF ( ALPHV .GE. 10.0 ) THEN

         BETA_EXT  = MAX( BETA_EXT, LARGEEXT )

         BETA_SCAT = MAX( BETA_SCAT, LARGESCAT )

      END IF ! test for 10.0 < ALPHV

C***FSB now calculate the asymmetry factor
C***  using the Hanna-Mathur quasi-empirical method
!     GMAX = 0.0

!     QSCAT_AVG = BETA_SCAT
!     QANGLE = AA0

!     IF ( QSCAT_AVG .GT. 1.0E-4 ) THEN
!        QQ  = QSCAT_AVG * QSCAT_AVG
!        QF1 = AA0 + AA1 * QSCAT_AVG
!        QF2 = AA2 + AA3 * QSCAT_AVG
!        QF3 = AA4 * QSCAT_AVG * QSCAT_AVG
!        QANGLE = QF1 + QF2 * QQ + QF3 * QQ
!     END IF

!     QANGLE = DEGTORAD * QANGLE
!     G      = COS( QANGLE)     ! asymmetry factor

C***FSB now calculate the asymmetry factor
C***   using a parametric fit to Mie calculations

      X_ALPHA2 = X_ALPHA * X_ALPHA
      X_ALPHA3 = X_ALPHA * X_ALPHA * X_ALPHA

      SMALL_G = 0.0
      LARGE_G = 0.0

      IF ( ALPHV .LT. 2.6 ) THEN

C FSB Calculate SMALL_G for sigma_g = 2.0

         QF1 = XXA0 + XXA1 * ALPHV
         QF2 = XXA2 * ALPHV2
         QF3 = XXA3 * ALPHV3
         SMALL_G = QF1 + QF2 + QF3 ! valid at sigma_g = 2.0

C FSB adjust SMALL_G for values less than 2.0
C     The smallest sigma_g allowed is 1.05

         QQF1 = CXG0 + CXG1 * ALPHV
         QQF2 = CXG2 * ALPHV2
         QQF3 = CXG3 * ALPHV3
         QQSUM = QQF1 + QQF2 + QQF3
         FAC1 = 1.052632 * ( 2.0 - SIGMA_G)
         QQSUM = MAX( 0.0, QQSUM )
         QQCORR =   ( 1.0 - FAC1 ) + FAC1 * QQSUM
         SMALL_G = SMALL_G * QQCORR
         G = SMALL_G

      ELSE IF ( ALPHV .LT. 100.0 ) THEN

         QF1 =  YYA0 + YYA1 * X_ALPHA
         QF2 = YYA2 * X_ALPHA2
         QF3 = YYA3 * X_ALPHA3
         LARGE_G = QF1 + QF2 + QF3
         G = LARGE_G

      ELSE

C     Very large sphere limit from fit to Irvine (1965).

         G = 1.124484 - 1.153869E-1 * NR

      END IF
C Simple and gross fix if appromation exceed realistic bounds
      G = MAX( -0.9999, MIN( G, 0.9999))
      RETURN
      END SUBROUTINE FAST_OPTICS

      END MODULE AERO_PHOTDATA
