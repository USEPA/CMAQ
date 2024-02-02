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
      MODULE BUDGET_DEFN

      USE PA_DEFN               ! Process Anaylsis control and data variables
      USE GRID_CONF             ! horizontal & vertical domain configuration
      USE RUNTIME_VARS
      USE UTILIO_DEFN           ! inherits PARUTILIO
      USE CGRID_SPCS, ONLY : CGRID_MASK_GAS, CGRID_MASK_AERO,
     &    CGRID_MASK_NUM, CGRID_MASK_SRF, CGRID_MASK_NR, CGRID_MASK_TRAC,
     &    CGRID_MW, N_CGRID_SPC, CGRID_NAME, NSPCSD, RHOJ_LOC
      USE VDIFF_MAP, ONLY : N_SPC_DIFF, DIFF_SPC, DIFF_MW,
     &                       DIFF_MASK_AERO, DIFF_MASK_NUM, DIFF_MASK_SRF
      USE CENTRALIZED_IO_MODULE
      USE XY_BUDGET


#ifndef mpas
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE, SE_DATA_COPY_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE, NOOP_DATA_COPY_MODULE)
#endif
#endif

      INTEGER, PARAMETER      :: BDGC0_ID = -1
      INTEGER, PARAMETER      :: BDGSAVE_ID = 0

      PUBLIC BUDGET_INIT, STORE_BUDGET, STORE_BUDGET_DDEP, 
     &       WRITE_BUDGET, BDGC0_ID, BUDGETVARIABLES, MAX_BUDGET_VARS_NML


      INTEGER, SAVE         :: BDG_UNIT
      INTEGER, PARAMETER      :: BDGCF_ID = -2

      REAL, ALLOCATABLE, SAVE :: BDG_PROC( :,: )
      REAL, ALLOCATABLE, SAVE :: BDG_BURDEN( :,: )

      INTEGER, SAVE :: BDG_JDATE0, BDG_JTIME0
      INTEGER, SAVE :: BDG_JDATE1, BDG_JTIME1
 
      CHARACTER( 16 ),ALLOCATABLE, SAVE :: BDGSPEC( : )
      INTEGER, ALLOCATABLE, SAVE :: MAP_toCGRID( : )
      INTEGER, ALLOCATABLE, SAVE :: MAP_toBDG( : )

      INTEGER, SAVE :: N_BDG_VAR, N_BDG_PAIRS

      !CHARACTER( 9 )  :: tab
      CHARACTER( 1 ), SAVE  :: tab = ','

      CONTAINS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE BUDGET_INIT( CGRID, JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Function: Initialize the budget variables and output file
 
C Preconditions: None
 
C Key Subroutines/Functions Called: None
 
C-----------------------------------------------------------------------
      USE UTIL_FAMILY_MODULE
#ifdef mpas
      use util_module, only : junit, index1, nextime
#endif
 
      IMPLICIT NONE 

      ! Includes:
      INCLUDE SUBST_CONST       ! Constants
      
      REAL, POINTER :: CGRID(:,:,:,:)
      INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP(3)


      ! Local Variables:
      REAL, ALLOCATABLE :: CONC( :,:,:,: )
      CHARACTER( 90 ) :: CMAQ_HEADER( 200 )
      INTEGER         :: NCMAQ_HEAD
      CHARACTER( 400 ):: XMSG, BDG_FILE
      CHARACTER( 50 ) :: FM
      INTEGER         :: ASTAT
      INTEGER         :: I, J
      LOGICAL, SAVE   :: FIRST_TIME = .TRUE.
      INTEGER         :: SDATE, STIME

      CHARACTER( 16 ) :: BDGSPEC_TMP( 500 ), BDGVAR
      INTEGER :: MAP_toCGRID_TMP( 1000 )
      INTEGER :: MAP_toBDG_TMP( 1000 )
      INTEGER :: N_BDG_REG
      LOGICAL :: EXPAND_SPEC
      LOGICAL, ALLOCATABLE :: CGRID_VEC( : )

!-----------------------------------------------------------------------

      IF ( FIRST_TIME ) THEN
          FIRST_TIME = .FALSE.

          ! Initialize Variables
          ALLOCATE( CONC( NCOLS,NROWS,NLAYS,N_CGRID_SPC ),
     &              CGRID_VEC( N_CGRID_SPC ), STAT=ASTAT )
          CONC = CGRID
          
          ALLOCATE( BDG_PROC( N_CGRID_SPC,NPRCS ),
     &              BDG_BURDEN( N_CGRID_SPC,3),
     &              F_WEST_IN( NLAYS,N_CGRID_SPC ),
     &              F_WEST_OUT( NLAYS,N_CGRID_SPC ),
     &              F_EAST_IN( NLAYS,N_CGRID_SPC ),
     &              F_EAST_OUT( NLAYS,N_CGRID_SPC ),
     &              F_SOUTH_IN( NLAYS,N_CGRID_SPC ),
     &              F_SOUTH_OUT( NLAYS,N_CGRID_SPC ),
     &              F_NORTH_IN( NLAYS,N_CGRID_SPC ),
     &              F_NORTH_OUT( NLAYS,N_CGRID_SPC ),
     &              CSAV( NCOLS,NROWS,NLAYS,N_CGRID_SPC ),
     &              STAT=ASTAT )
          BDG_PROC    = 0.0
          BDG_JDATE0  = JDATE
          BDG_JTIME0  = JTIME
          F_WEST_IN   = 0.0
          F_WEST_OUT  = 0.0
          F_EAST_IN   = 0.0
          F_EAST_OUT  = 0.0
          F_SOUTH_IN  = 0.0
          F_SOUTH_OUT = 0.0
          F_NORTH_IN  = 0.0
          F_NORTH_OUT = 0.0
          CSAV        = CGRID
          

          SDATE = JDATE
          STIME = JTIME
          CALL NEXTIME ( SDATE, STIME, TSTEP( 2 ) )
          CALL STORE_BUDGET( BDGC0_ID, CONC, JDATE, JTIME, .FALSE. )
          DEALLOCATE( CONC )
          
          ! Open Output tab-separated file and print header
          IF ( MYPE .EQ. 0 ) THEN
              IF ( BUDGET_FNAME .EQ. 'CCTM_BUDGET' ) THEN
                 IF ( OUTDIR .EQ. '' ) THEN
                   BDG_FILE = "CCTM_BUDGET_" // TRIM(APPL_NAME) // ".txt"
                 ELSE
                   BDG_FILE = TRIM(OUTDIR) // "/CCTM_BUDGET_" // TRIM(APPL_NAME) // ".txt"
                 END IF
              ELSE
                 J = INDEX( BUDGET_FNAME, ".txt" ) 
                 IF ( J .NE. 0 ) THEN
                    BDG_FILE = BUDGET_FNAME(1:J+3)
                 ELSE
                    BDG_FILE = TRIM( BUDGET_FNAME ) // ".txt"
                 END IF
              END IF
              BDG_UNIT = JUNIT()
              OPEN( UNIT = BDG_UNIT, FILE = BDG_FILE, STATUS = "REPLACE" )
          
              ! Write CMAQ Header
              CALL LOAD_HEADER( CMAQ_HEADER, NCMAQ_HEAD )
              WRITE( BDG_UNIT, '(A)' ), CMAQ_HEADER( 1:NCMAQ_HEAD )
              WRITE( BDG_UNIT, '(A1)' ),'#'
          
              ! Write Execution ID and GridName
              WRITE( BDG_UNIT, '(A1,6x,A,A)' ),'#',"EXEC_ID: ",TRIM(EXECUTION_ID)
              WRITE( BDG_UNIT, '(A1,6x,A,A)' ),'#',"GRIDNAME: ",TRIM(GRID_NAME)
              WRITE( BDG_UNIT, '(A1)' ),'#'
          
              WRITE( BDG_UNIT, '(A1,6x,A,A)' ),'#','All date-times are in ',
     &             'YYYY-mm-ddTHH:MM:SSZ format and in UTC time zone.'
              WRITE( BDG_UNIT, '(A1,6x,A)' ),'#','Delta T (time) Units are hours.' 
              WRITE( BDG_UNIT, '(A1,6x,A)' ),'#','Gas and Aerosol Mass Units are kg'
              WRITE( BDG_UNIT, '(A1,6x,A)' ),'#','Aerosol Number Units are total N'
              WRITE( BDG_UNIT, '(A1,6x,A)' ),'#','Aerosol Surface Area Units are m2'
              WRITE( BDG_UNIT, '(A1)' ),'#'
          
              ! Write Header
              WRITE( BDG_UNIT, '(55(A))' ), 'SPECIES',tab,'T_START',tab,
     &         'T_FINAL',tab,'T_DELTA',tab,'M_START',tab,'M_FINAL',tab,
     &         'M_DELTA',tab,'WEST_IN',tab,'WEST_OUT',tab,'EAST_IN',tab,
     &         'EAST_OUT',tab,'SOUTH_IN',tab,'SOUTH_OUT',tab,'NORTH_IN',tab,
     &         'NORTH_OUT', 
     &         ( tab, TRIM(PROCNAME(i)),i=3,NPRCS),tab,'RESID'
          
          END IF
          
          ! Map Budget Variables to CGRID Species
          N_BDG_REG = INDEX1( '', MAX_BUDGET_VARS_NML, BudgetVariables ) - 1
          IF ( N_BDG_REG .LE. 0 ) THEN
             WRITE( LOGDEV, * )
             WRITE( XMSG, '(A,A)' ),
     &           'No Budget Variables have been selected. All Variables',
     &           'will be output to the budget file.'
             CALL LOG_MESSAGE( LOGDEV, XMSG )     
             N_BDG_REG = 1
             BudgetVariables( 1 ) = '*ALL'
          END IF

          ! Now Error Check and Expand the CMAQ Species Field
          N_BDG_VAR   = 0
          N_BDG_PAIRS = 0
          DO I = 1,N_BDG_REG
             BDGVAR = BudgetVariables( I )
             Expand_Spec = .FALSE.
             IF ( BDGVAR(1:1) .EQ. '*' ) THEN
                Expand_Spec = .TRUE.
                BDGVAR(1:15) = BDGVAR(2:16)
             END IF
             IF ( TRIM(BDGVAR) .EQ. 'ALL' ) THEN
                Expand_Spec = .TRUE.
             END IF

             ! Retrieve logical vector, DIFF_VEC, indicating diffused species
             ! relevant for DiagSpec(I)
             CALL MAP_CHEM_FAMILIES( BDGVAR, CGRID_NAME, N_CGRID_SPC, CGRID_VEC )
         
             ! Save Map to translate each pair to diffused species and
             ! diagnostic species
             IF ( EXPAND_SPEC ) THEN 
                ! Add a Diagnostic Species for every expanded species
                DO J = 1,N_CGRID_SPC
                   IF ( CGRID_VEC( J ) ) THEN
                       N_BDG_VAR = N_BDG_VAR + 1
                       BDGSPEC_TMP( N_BDG_VAR ) = CGRID_NAME( J )
         
                       N_BDG_PAIRS = N_BDG_PAIRS + 1
                       MAP_toCGRID_TMP( N_BDG_PAIRS ) = J
                       MAP_toBDG_TMP( N_BDG_PAIRS ) = N_BDG_VAR
                   END IF
                END DO
             ELSE
                ! Keep only 1 diagnostic species and map all of the diffused
                ! species to it
                IF ( ANY( CGRID_VEC ) ) THEN
                   N_BDG_VAR = N_BDG_VAR + 1
                   BDGSPEC_TMP( N_BDG_VAR ) = BDGVAR
                END IF
                DO J = 1,N_CGRID_SPC
                   IF ( CGRID_VEC( J ) ) THEN
                       N_BDG_PAIRS = N_BDG_PAIRS + 1
                       MAP_toCGRID_TMP( N_BDG_PAIRS ) = J
                       MAP_toBDG_TMP( N_BDG_PAIRS ) = N_BDG_VAR
                   END IF
                END DO
             END IF
          END DO

          ALLOCATE( MAP_toCGRID( N_BDG_PAIRS ),
     &              MAP_toBDG( N_BDG_PAIRS ),
     &              BDGSPEC( N_BDG_VAR ) )
          MAP_toCGRID = MAP_toCGRID_TMP( 1:N_BDG_PAIRS )
          MAP_toBDG   = MAP_toBDG_TMP( 1:N_BDG_PAIRS )
          BDGSPEC     = BDGSPEC_TMP( 1:N_BDG_VAR )

      END IF    

      RETURN
      END SUBROUTINE BUDGET_INIT

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  Store Budget Data
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE STORE_BUDGET( IPR_ID, CONC, JDATE, JTIME, LCOUPLE )

      USE CENTRALIZED_IO_MODULE, ONLY : MSFX2

      IMPLICIT NONE

      INCLUDE SUBST_CONST       ! Constants

      REAL                  :: CONC( :,:,:,: )
      INTEGER, INTENT( IN ) :: IPR_ID            ! Process ID
      INTEGER, INTENT( IN ) :: JDATE, JTIME
      LOGICAL, INTENT( IN ) :: LCOUPLE           ! Flag for whther or not to use 
                                                 ! rhoj from the CGRID array for 
                                                 ! the density calculation for gases

      REAL, ALLOCATABLE, SAVE :: BDG_MASS( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: BDG_MASS_SAVE( :,:,:,: )
      REAL, PARAMETER :: MWAIR_SI = MWAIR / 1.0E+03 ! kg mol-1
      REAL, ALLOCATABLE, SAVE :: DENS( :,:,: )   ! air density (kg m-3)
      REAL, ALLOCATABLE, SAVE :: RHOJ( :,:,: )   ! air density * Jacobian (kg m-3)
      REAL, ALLOCATABLE, SAVE :: JACOBM( :,:,: ) ! Jacobian
      REAL, ALLOCATABLE, SAVE :: ZF( :,:,: )     ! height of layer top
      REAL, ALLOCATABLE, SAVE :: CELLVOL( :,:,: )! cell volume
      REAL                    :: AERO_NORM
      
      LOGICAL :: FIRST_TIME = .TRUE.
      INTEGER ASTAT
      INTEGER L, I, LAYS, R, C

      ! First time through, allocate Budget Concentration conversion
      ! array
      IF ( FIRST_TIME ) THEN
          FIRST_TIME = .FALSE.
          ALLOCATE ( BDG_MASS( NCOLS,NROWS,NLAYS,N_CGRID_SPC ),
     &               BDG_MASS_SAVE( NCOLS,NROWS,NLAYS,N_CGRID_SPC ),
     &                DENS( NCOLS,NROWS,NLAYS ),
     &                RHOJ( NCOLS,NROWS,NLAYS ),
     &                JACOBM( NCOLS,NROWS,NLAYS ),
     &                ZF( NCOLS,NROWS,NLAYS ),
     &                CELLVOL( NCOLS,NROWS,NLAYS ),
     &                STAT = ASTAT )
      END IF

      ! Retrieve Cell Variables for converting concentrations to burden
#ifdef mpas
      CELLVOL( :,1,: ) = cell_vol(:,1,:)
#else
      call interpolate_var ('ZF', jdate, jtime, ZF) ! height of layer top
      CELLVOL( :,:,1 ) = REAL( XCELL_GD * YCELL_GD, 4 ) * ZF( :,:,1 ) / MSFX2(:,:)
      DO L = 2,NLAYS
         CELLVOL( :,:,L ) = REAL( XCELL_GD * YCELL_GD,4) / MSFX2( :,: ) * 
     &                        ( ZF( :,:,L ) - ZF( :,:,L-1) )  ! m3
      END DO
#endif

      ! Calculate Density for Conversion of Gases from ppm to kg. If 
      IF ( LCOUPLE ) THEN
          ! If LCOUPLE is true, then the gases are already in mol mol-1 kg m-2, 
          ! which are the units of ppm * [air density] * [Jacobian]. In 
          ! general, the Jacobian usedin CMAQ includes the map scale 
          ! factor squared: JACOBM = J / msfx2
          ! Aerosols are in units of ug m-2
#ifdef mpas
          JACOBM = 1.0
#else
          call interpolate_var ('JACOBM', jdate, jtime, JACOBM) ! Jacobian
#endif
          DENS = 1.0 / MWAIR_SI * 1.0E-6 * 1.0E-3 ! mol kg-1 air
          CELLVOL = CELLVOL / JACOBM ! m2
          AERO_NORM = 1.0
      ELSE
          ! If LCOUPLE is false, then gases are in ppm and aerosols in 
          ! ug m-3.
          call interpolate_var ('DENS', jdate, jtime, DENS) ! kg m-3
          DENS  = DENS / MWAIR_SI * 1.0E-6 * 1.0E-3 ! umol m-3 air
          AERO_NORM = 1.0E-9
      END IF

      ! Convert Process Units to kg (mass), N (number), and m2 (surface
      ! area). Input gases are in ppm, aerosols in ug m-3, number in N
      ! m-3 and surface area in m2 m-3.
      LAYS = NLAYS
      IF ( IPR_ID .EQ. IPR_DDEP ) LAYS = 1

      DO I = 1,N_CGRID_SPC
         ! Gas - convert ppm to kg
         IF ( ( CGRID_MASK_GAS( I ) .OR.
     &          CGRID_MASK_NR( I )  .OR.
     &          CGRID_MASK_TRAC( I ) ) .AND.
     &          I .NE. RHOJ_LOC ) THEN
            DO L = 1,LAYS
            DO R = 1,NROWS
            DO C = 1,NCOLS
               BDG_MASS(C,R,L,I) = CONC(C,R,L,I) * DENS(C,R,L) 
     &                             * CGRID_MW(I) * CELLVOL(C,R,L)
            END DO
            END DO
            END DO
         END IF

         ! Aerosol Mass:  ug m-3 -> kg
         IF ( CGRID_MASK_AERO( I ) .AND.
     &        .NOT. CGRID_MASK_NUM( I ) .AND.
     &        .NOT. CGRID_MASK_SRF( I )  ) THEN
            DO L = 1,LAYS
            DO R = 1,NROWS
            DO C = 1,NCOLS
               BDG_MASS(C,R,L,I) = CONC(C,R,L,I) * AERO_NORM
     &                             * CELLVOL(C,R,L) 
            END DO
            END DO
            END DO
         END IF

         ! Aerosol Number: N m-3 -> N 
         ! Aerosol Surface Area: m2 m-3 -> m2 
         IF ( CGRID_MASK_NUM( I ) ) THEN
            DO L = 1,LAYS
               BDG_MASS(:,:,L,I) = CONC(:,:,L,I) * CELLVOL(:,:,L) 
            END DO
         END IF
         IF ( CGRID_MASK_SRF( I ) ) THEN
            DO L = 1,LAYS
               BDG_MASS(:,:,L,I) = CONC(:,:,L,I) * CELLVOL(:,:,L) 
            END DO
         END IF
      
      END DO

      ! Sum and Store Process Change 
      IF ( IPR_ID .EQ. BDGC0_ID ) THEN
          BDG_BURDEN( :,1 ) = SUM( SUM( SUM( BDG_MASS(:,:,:,:),1 ),1 ),1 ) 
          BDG_MASS_SAVE = BDG_MASS
      ELSE IF ( IPR_ID .EQ. BDGCF_ID ) THEN
          BDG_BURDEN( :,2 ) = SUM( SUM( SUM( BDG_MASS(:,:,:,:),1 ),1 ),1 ) 
      ELSE IF ( IPR_ID .EQ. IPR_DDEP ) THEN
          BDG_PROC( :,IPR_ID ) = BDG_PROC( :,IPR_ID ) + SUM( SUM( BDG_MASS(:,:,1,:),1 ),1 )
      ELSE IF ( IPR_ID .EQ. IPR_EMIS .OR. IPR_ID .EQ. IPR_XADV .OR. 
     &          IPR_ID .EQ. IPR_YADV .OR. IPR_ID .EQ. IPR_COAG .OR.
     &          IPR_ID .EQ. IPR_COND .OR. IPR_ID .EQ. IPR_NPF  .OR.
     &          IPR_ID .EQ. IPR_GROW ) THEN
          BDG_PROC( :,IPR_ID ) = BDG_PROC( :,IPR_ID ) 
     &                          +SUM( SUM( SUM( BDG_MASS(:,:,:,:),1 ),1 ),1 )
      ELSE IF ( IPR_ID .EQ. BDGSAVE_ID ) THEN
          ! Just Save the Burden
          BDG_MASS_SAVE = BDG_MASS
      ELSE
          BDG_PROC( :,IPR_ID ) = BDG_PROC( :,IPR_ID ) 
     &       + SUM( SUM( SUM( BDG_MASS - BDG_MASS_SAVE,1 ),1 ),1 )
          BDG_MASS_SAVE = BDG_MASS
      END IF

      RETURN
      END SUBROUTINE STORE_BUDGET
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write Budget Output to CSV File
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE WRITE_BUDGET( CONC, JDATE, JTIME, TSTEP )

#ifdef mpas
      use util_module, only : daymon
#endif

      IMPLICIT NONE

      INTEGER I, J
      INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP(3)
      REAL                  :: CONC( :,:,:,: )
      REAL,ALLOCATABLE,SAVE :: RESID( : ), RESID_F(:)
      LOGICAL, SAVE :: FIRST_TIME = .TRUE.

      REAL DT
      INTEGER DTHR, DTMIN, DTSEC
      INTEGER BDG_YEAR0, BDG_MONTH0, BDG_DAY0, BDG_YYYYMMDD0
      INTEGER BDG_YEAR1, BDG_MONTH1, BDG_DAY1, BDG_YYYYMMDD1
      INTEGER BDG_HOUR0, BDG_MIN0, BDG_SEC0
      INTEGER BDG_HOUR1, BDG_MIN1, BDG_SEC1
      CHARACTER(20) :: BDG_TIME0, BDG_TIME1

      REAL, ALLOCATABLE, SAVE :: BDG_BURDEN_OUT(:,:), BDG_PROC_OUT(:,:),
     &                           ADV_FLUXES_OUT(:,:), ADV_FLUXES(:,:)
      REAL :: TMP, TMP2

      IF ( FIRST_TIME ) THEN
          FIRST_TIME = .FALSE.
          ALLOCATE ( RESID( N_CGRID_SPC ), RESID_F( N_BDG_VAR ),
     &               BDG_BURDEN_OUT( N_BDG_VAR,3 ),
     &               BDG_PROC_OUT( N_BDG_VAR,NPRCS ),
     &               ADV_FLUXES( N_CGRID_SPC,8 ), 
     &               ADV_FLUXES_OUT( N_BDG_VAR,8 ) )
      END IF


      CALL STORE_BUDGET( BDGCF_ID, CONC, JDATE, JTIME, .FALSE. )
      CSAV = CONC

      ! Correct VDIFF by subtracting emissions and dry deposition
      BDG_PROC( :,IPR_VDIF ) = BDG_PROC( :,IPR_VDIF ) - BDG_PROC( :,IPR_EMIS ) - BDG_PROC( :,IPR_DDEP )
      
      ! Calculate total mass change across output time step
      BDG_BURDEN( :,3 ) = BDG_BURDEN( :,2 ) - BDG_BURDEN( :,1 )

      ! Convert units for vapor ADV_FLUXES from 10^-6 mol to kg
      ! Units for aerosol mass should already be kg
      ! Units for aerosol number and surface area should already be N and m2
      ADV_FLUXES( :,1 ) =  SUM( F_WEST_IN, 1 )
      ADV_FLUXES( :,2 ) = -1.0 * SUM( F_WEST_OUT, 1 )
      ADV_FLUXES( :,3 ) =  SUM( F_EAST_IN, 1 )
      ADV_FLUXES( :,4 ) = -1.0 * SUM( F_EAST_OUT, 1 )
      ADV_FLUXES( :,5 ) =  SUM( F_SOUTH_IN, 1 )
      ADV_FLUXES( :,6 ) = -1.0 * SUM( F_SOUTH_OUT, 1 )
      ADV_FLUXES( :,7 ) =  SUM( F_NORTH_IN, 1 )
      ADV_FLUXES( :,8 ) = -1.0 * SUM( F_NORTH_OUT, 1 )
      DO I = 1,N_CGRID_SPC
         IF ( .NOT.CGRID_MASK_AERO( I ) ) 
     &      ADV_FLUXES( I,: ) = ADV_FLUXES( I,: ) * 1.0E-9 * CGRID_MW( I ) 
      END DO

      ! Calculate Date, Time, and Length of Time Interval
      BDG_JDATE1 = JDATE
      BDG_JTIME1 = JTIME
      DTHR = TSTEP(1)/10000
      DTMIN = ( TSTEP(1) - DTHR*10000 ) / 100
      DTSEC = TSTEP(1) - DTHR*10000 - DTMIN*100
      DT = REAL( DTHR,4) + REAL( DTMIN,4 )/60.0 + REAL( DTSEC,4 )/3600.0
      
      ! Format Beginning and Ending Date-Times
      IF ( MYPE .EQ. 0 ) THEN
        BDG_YEAR0 = BDG_JDATE0 / 1000
        BDG_YEAR1 = BDG_JDATE1 / 1000
        CALL DAYMON( BDG_JDATE0, BDG_MONTH0, BDG_DAY0 )
        CALL DAYMON( BDG_JDATE1, BDG_MONTH1, BDG_DAY1 )
        BDG_YYYYMMDD0 = BDG_YEAR0*10000 + BDG_MONTH0*100 + BDG_DAY0
        BDG_YYYYMMDD1 = BDG_YEAR1*10000 + BDG_MONTH1*100 + BDG_DAY1
        BDG_HOUR0 = BDG_JTIME0 / 10000
        BDG_HOUR1 = BDG_JTIME1 / 10000
        BDG_MIN0  = ( BDG_JTIME0 - BDG_HOUR0*10000 ) / 100
        BDG_MIN1  = ( BDG_JTIME1 - BDG_HOUR1*10000 ) / 100
        BDG_SEC0  =   BDG_JTIME0 - BDG_HOUR0*10000 - BDG_MIN0*100
        BDG_SEC1  =   BDG_JTIME1 - BDG_HOUR1*10000 - BDG_MIN1*100
      
        WRITE( BDG_TIME0, '(I4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1)' ), 
     &         BDG_YEAR0,'-',BDG_MONTH0,'-',BDG_DAY0,'T',BDG_HOUR0,':',
     &         BDG_MIN0,':',BDG_SEC0,'Z'
        WRITE( BDG_TIME1, '(I4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1)' ), 
     &         BDG_YEAR1,'-',BDG_MONTH1,'-',BDG_DAY1,'T',BDG_HOUR1,':',
     &         BDG_MIN1,':',BDG_SEC1,'Z'
      END IF
      
      ! Map CGRID Species to Budget Output Species
      BDG_PROC_OUT = 0.0
      BDG_BURDEN_OUT = 0.0
      ADV_FLUXES_OUT = 0.0
      DO I = 1,N_BDG_PAIRS
          BDG_PROC_OUT( MAP_toBDG( I ),: ) = BDG_PROC_OUT( MAP_toBDG( I ),: ) 
     &             + BDG_PROC( MAP_toCGRID( I ),: )
          BDG_BURDEN_OUT( MAP_toBDG( I ),: ) = BDG_BURDEN_OUT( MAP_toBDG( I ),: ) 
     &             + BDG_BURDEN( MAP_toCGRID( I ),: )
          ADV_FLUXES_OUT( MAP_toBDG( I ),: ) = ADV_FLUXES_OUT( MAP_toBDG( I ),: ) 
     &             + ADV_FLUXES( MAP_toCGRID( I ),: )
      END DO

#ifndef mpas
#ifdef parallel
      ! Sum Changes Across All Processors
      DO J = 1,3
         DO I = 1,N_BDG_VAR
           BDG_BURDEN_OUT(I,J) = SUBST_GLOBAL_SUM( BDG_BURDEN_OUT(I,J) )
         END DO 
      END DO
      DO J = 1,NPRCS
         DO I = 1,N_BDG_VAR
           BDG_PROC_OUT(I,J) = SUBST_GLOBAL_SUM( BDG_PROC_OUT(I,J) )
         END DO
      END DO
      DO J = 1,8
         DO I = 1,N_BDG_VAR
           ADV_FLUXES_OUT(I,J) = SUBST_GLOBAL_SUM( ADV_FLUXES_OUT(I,J) )
         END DO
      END DO
#endif
#endif

      IF ( MYPE .EQ. 0 ) THEN
         ! Positive Residual means sum of processes is greater 
         ! than net change in mass
      !   RESID_F(:) = SUM( BDG_PROC_OUT(:,1:NPRCS),2 ) 
      !&               - BDG_BURDEN_OUT( :,3 ) 
         RESID_F(:) = SUM( BDG_PROC_OUT(:,3:NPRCS),2 ) + SUM( ADV_FLUXES_OUT,2)
     &               - BDG_BURDEN_OUT( :,3 )  ! Use Adv Fluxes instead of total XADV and YADV

         ! Write Species Process Changes to tab-separated file
         DO I = 1,N_BDG_VAR
            ! Default
            WRITE( BDG_UNIT,'(3(A,A),F7.3,A,24(E15.8,A) )' ),TRIM(BDGSPEC(I)),tab, 
     &         BDG_TIME0,tab,BDG_TIME1,tab, DT,tab, (BDG_BURDEN_OUT(I,J),tab,J=1,3 ),
     &         ( ADV_FLUXES_OUT(I,J),tab,J=1,8 ),
     &         ( BDG_PROC_OUT(I,J),tab,J=3,NPRCS ), RESID_F(I)

            ! Debugging
!            WRITE( BDG_UNIT,'(3(A,A),F7.3,A,21(E11.4,A) )' ),TRIM(BDGSPEC(I)),tab, 
!     &         BDG_TIME0,tab,BDG_TIME1,tab, DT,tab, (BDG_BURDEN_OUT(I,J),tab,J=1,3 ),
!     &         SUM( ADV_FLUXES_OUT(I,1:4)),tab, SUM( ADV_FLUXES_OUT(I,5:8)),tab,
!     &         ( BDG_PROC_OUT(I,J),tab,J=1,NPRCS ), RESID_F(I), tab, RESID(I)
         END DO
      END IF

      ! Assign Initial Concentration
      ! and zero Out all Rates, etc
      BDG_PROC = 0.0
      BDG_BURDEN( :,1 ) = BDG_BURDEN( :,2 )
      BDG_BURDEN( :,2 ) = 0
      BDG_BURDEN( :,3 ) = 0

      BDG_JDATE0 = BDG_JDATE1
      BDG_JTIME0 = BDG_JTIME1

      F_WEST_IN = 0
      F_WEST_OUT = 0
      F_EAST_IN = 0
      F_EAST_OUT = 0
      F_SOUTH_IN = 0
      F_SOUTH_OUT = 0
      F_NORTH_IN = 0
      F_NORTH_OUT = 0

      RETURN

      END SUBROUTINE WRITE_BUDGET
 
      END MODULE BUDGET_DEFN
