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
     &    CGRID_MW, N_CGRID_SPC, CGRID_NAME
      USE VDIFF_MAP, ONLY : N_SPC_DIFF, DIFF_SPC, DIFF_MW,
     &                       DIFF_MASK_AERO, DIFF_MASK_NUM, DIFF_MASK_SRF
      USE CENTRALIZED_IO_MODULE
      USE XY_BUDGET


#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE, SE_DATA_COPY_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE, NOOP_DATA_COPY_MODULE)
#endif

      INTEGER, PARAMETER      :: BDGC0_ID = -1

      PUBLIC BUDGET_INIT, STORE_BUDGET, STORE_BUDGET_DDEP, 
     &       WRITE_BUDGET, BDGC0_ID, BUDGETVARIABLES, N_BUDGET_VARS_REG


      INTEGER, SAVE         :: BDG_UNIT
      INTEGER, PARAMETER      :: BDGCF_ID = -2

      REAL, ALLOCATABLE, SAVE :: BDG_PROC( :,: )
      REAL, ALLOCATABLE, SAVE :: BDG_MASS( :,: )

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
      SUBROUTINE BUDGET_INIT( CGRID, JDATE, JTIME )

C-----------------------------------------------------------------------
C Function: Initialize the budget variables and output file
 
C Preconditions: None
 
C Key Subroutines/Functions Called: None
 
C-----------------------------------------------------------------------
      IMPLICIT NONE 

      ! Includes:
      INCLUDE SUBST_CONST       ! Constants
      
      REAL, POINTER :: CGRID(:,:,:,:)
      INTEGER, INTENT( IN ) :: JDATE, JTIME


      ! Local Variables:
      REAL, ALLOCATABLE :: CONC( :,:,:,: )
      CHARACTER( 90 ) :: CMAQ_HEADER( 200 )
      INTEGER         :: NCMAQ_HEAD
      CHARACTER( 400 ):: XMSG, BDG_FILE
      CHARACTER( 50 ) :: FM
      INTEGER         :: ASTAT
      INTEGER         :: I, J
      LOGICAL, SAVE   :: FIRST_TIME = .TRUE.

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
     &              BDG_MASS( N_CGRID_SPC,3),
     &              F_WEST_IN( NLAYS,N_CGRID_SPC ),
     &              F_WEST_OUT( NLAYS,N_CGRID_SPC ),
     &              F_EAST_IN( NLAYS,N_CGRID_SPC ),
     &              F_EAST_OUT( NLAYS,N_CGRID_SPC ),
     &              F_SOUTH_IN( NLAYS,N_CGRID_SPC ),
     &              F_SOUTH_OUT( NLAYS,N_CGRID_SPC ),
     &              F_NORTH_IN( NLAYS,N_CGRID_SPC ),
     &              F_NORTH_OUT( NLAYS,N_CGRID_SPC ),
     &              STAT=ASTAT )
          BDG_PROC = 0
          BDG_JDATE0 = JDATE
          BDG_JTIME0 = JTIME
          F_WEST_IN = 0
          F_WEST_OUT = 0
          F_EAST_IN = 0
          F_EAST_OUT = 0
          F_SOUTH_IN = 0
          F_SOUTH_OUT = 0
          F_NORTH_IN = 0
          F_NORTH_OUT = 0
          
          CALL STORE_BUDGET( BDGC0_ID, CONC, JDATE, JTIME )
          
          ! Open Output tab-separated file and print header
          IF ( MYPE .EQ. 0 ) THEN 
              BDG_FILE = TRIM(OUTDIR) // "/CCTM_BUDGET_" // TRIM(APPL_NAME) // ".txt"
              BDG_UNIT = JUNIT()
              OPEN( UNIT = BDG_UNIT, FILE = BDG_FILE, STATUS = "REPLACE" )
          
              ! Write CMAQ Header
              CALL LOAD_HEADER( CMAQ_HEADER, NCMAQ_HEAD )
              WRITE( BDG_UNIT, '(A)' ), CMAQ_HEADER( 1:NCMAQ_HEAD )
              WRITE( BDG_UNIT, * )
          
              ! Write Execution ID and GridName
              XMSG = "EXEC_ID: " // EXECUTION_ID
              CALL LOG_MESSAGE( BDG_UNIT, XMSG )
              XMSG = "GRIDNAME: " // GRID_NAME
              CALL LOG_MESSAGE( BDG_UNIT, XMSG )
              WRITE( BDG_UNIT, * )
          
              CALL LOG_MESSAGE( BDG_UNIT, 'All date-times are in '//
     &             'YYYYmmdd:HHMMSS format and in UTC time zone.' )
              CALL LOG_MESSAGE( BDG_UNIT, 'Delta T (time) Units are hours.' )
              CALL LOG_MESSAGE( BDG_UNIT, 'Gas and Aerosol Mass Units are kg' )
              CALL LOG_MESSAGE( BDG_UNIT, 'Aerosol Number Units are total N' )
              CALL LOG_MESSAGE( BDG_UNIT, 'Aerosol Surface Area Units are m2' )
              WRITE( BDG_UNIT, * )
          
          
              ! Write Header
!              WRITE( BDG_UNIT, '(53(A))' ), 'SPECIES',tab,'T_START',tab,
!     &         'T_FINAL',tab,'T_DELTA',tab,'M_START',tab,'M_FINAL',tab,
!     &         'M_DELTA',tab,'WEST_IN',tab,'WEST_OUT',tab,'EAST_IN',tab,
!     &         'EAST_OUT',tab,'SOUTH_IN',tab,'SOUTH_OUT',tab,'NORTH_IN',tab,
!     &         'NORTH_OUT',tab, TRIM(PROCNAME(1)),
!     &         ( tab, TRIM(PROCNAME(i)),i=4,NPRCS),tab,'RESID'
          
              ! Debugging
              WRITE( BDG_UNIT, '(46(A))' ), 'SPECIES',tab,'T_START',tab,
     &         'T_FINAL',tab,'T_DELTA',tab,'M_START',tab,'M_FINAL',tab,
     &         'M_DELTA',tab,'WEST_EAST',tab,'SOUTH_NORTH',
     &         ( tab,TRIM(PROCNAME(i)),i=1,NPRCS),tab,'RESID_F',tab,'RESID_ADV'
          
          END IF
          
          DEALLOCATE( CONC )
          
          ! Map Budget Variables to CGRID Species
          N_BDG_REG = INDEX1( '', N_BUDGET_VARS_REG, BudgetVariables ) - 1
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
      SUBROUTINE STORE_BUDGET( IPR_ID, CONC, JDATE, JTIME )



      IMPLICIT NONE

      INCLUDE SUBST_CONST       ! Constants

      REAL                  :: CONC( :,:,:,: )
      INTEGER, INTENT( IN ) :: IPR_ID            ! Process ID
      INTEGER, INTENT( IN ) :: JDATE, JTIME

      REAL, ALLOCATABLE, SAVE :: BDG_CONC( :,:,:,: )
      REAL, PARAMETER :: MWAIR_SI = MWAIR / 1.0E+03 ! kg mol-1
      REAL, ALLOCATABLE, SAVE :: DENS( :,:,: )   ! air density (kg m-3)
      REAL, ALLOCATABLE, SAVE :: MSFX2( :,:,: )  ! map scale factor
      REAL, ALLOCATABLE, SAVE :: ZF( :,:,: )     ! height of layer top
      REAL, ALLOCATABLE, SAVE :: CELLVOL( :,:,: )! cell volume
      LOGICAL :: FIRST_TIME = .TRUE.
      INTEGER ASTAT
      INTEGER L, I, LAYS, R, C

      ! First time through, allocate Budget Concentration conversion
      ! array
      IF ( FIRST_TIME ) THEN
          FIRST_TIME = .FALSE.
          ALLOCATE ( BDG_CONC( NCOLS,NROWS,NLAYS,N_CGRID_SPC ),
     &                DENS( NCOLS,NROWS,NLAYS ),
     &                MSFX2( NCOLS,NROWS,NLAYS ),
     &                ZF( NCOLS,NROWS,NLAYS ),
     &                CELLVOL( NCOLS,NROWS,NLAYS ),
     &                STAT = ASTAT )
      END IF

      ! Retrieve Cell Variables for converting concentrations to burden
      call interpolate_var ('DENS', jdate, jtime, DENS) ! kg m-3
      DENS = DENS / MWAIR_SI  ! mol m-3
      !call interpolate_var ('MSFX2', jdate, jtime, MSFX2) ! map scale factor
      call interpolate_var ('ZF', jdate, jtime, ZF) ! height of layer top

      CELLVOL( :,:,1 ) = REAL( XCELL_GD * YCELL_GD, 4 ) * ZF( :,:,1 )
      DO L = 2,NLAYS
         CELLVOL( :,:,L ) = XCELL_GD * YCELL_GD * 
     &                        ( ZF( :,:,L ) - ZF( :,:,L-1) )  ! m3
      END DO
      DENS = DENS * CELLVOL * 1.0E-6 * 1.0E-3 ! mol air

      ! Convert Process Units to kg (mass), N (number), and m2 (surface
      ! area). Input gases are in ppm, aerosols in ug m-3, number in N
      ! m-3 and surface area in m2 m-3.
      LAYS = NLAYS
      IF ( IPR_ID .EQ. IPR_DDEP ) LAYS = 1

      DO I = 1,N_CGRID_SPC
         ! Gas - convert ppm to kg
         IF ( CGRID_MASK_GAS( I ) .OR.
     &        CGRID_MASK_NR( I )  .OR.
     &        CGRID_MASK_TRAC( I ) ) THEN
            DO L = 1,LAYS
            DO R = 1,NROWS
            DO C = 1,NCOLS
               BDG_CONC(C,R,L,I) = CONC(C,R,L,I) * DENS(C,R,L) * CGRID_MW(I)
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
               BDG_CONC(C,R,L,I) = CONC(C,R,L,I) * CELLVOL(C,R,L) * 1.0E-09
            END DO
            END DO
            END DO
         END IF

         ! Aerosol Number: N m-3 -> N 
         ! Aerosol Surface Area: m2 m-3 -> m2 
         IF ( CGRID_MASK_NUM( I ) ) THEN
            DO L = 1,LAYS
               BDG_CONC(:,:,L,I) = CONC(:,:,L,I) * CELLVOL(:,:,L)
            END DO
         END IF
         IF ( CGRID_MASK_SRF( I ) ) THEN
            DO L = 1,LAYS
               BDG_CONC(:,:,L,I) = CONC(:,:,L,I) * CELLVOL(:,:,L)
            END DO
         END IF
      
      END DO

      ! Sum and Store Process Change 
      IF ( IPR_ID .EQ. BDGC0_ID ) THEN
          BDG_MASS( :,1 ) = SUM( SUM( SUM( BDG_CONC(:,:,:,:),1 ),1 ),1 ) 
      ELSE IF ( IPR_ID .EQ. BDGCF_ID ) THEN
          BDG_MASS( :,2 ) = SUM( SUM( SUM( BDG_CONC(:,:,:,:),1 ),1 ),1 ) 
      ELSE IF ( IPR_ID .EQ. IPR_DDEP ) THEN
          BDG_PROC( :,IPR_ID ) = SUM( SUM( BDG_CONC(:,:,1,:),1 ),1 )
      ELSE
          BDG_PROC( :,IPR_ID ) = SUM( SUM( SUM( BDG_CONC(:,:,:,:),1 ),1 ),1 )
      END IF

      RETURN
      END SUBROUTINE STORE_BUDGET
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write Budget Output to CSV File
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE WRITE_BUDGET( CONC, JDATE, JTIME, TSTEP )

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
      CHARACTER(15) :: BDG_TIME0, BDG_TIME1

      REAL, ALLOCATABLE, SAVE :: BDG_MASS_OUT(:,:), BDG_PROC_OUT(:,:),
     &                           ADV_FLUXES_OUT(:,:), ADV_FLUXES(:,:)
      REAL :: TMP, TMP2

      IF ( FIRST_TIME ) THEN
          FIRST_TIME = .FALSE.
          ALLOCATE ( RESID( N_CGRID_SPC ), RESID_F( N_CGRID_SPC ),
     &               BDG_MASS_OUT( N_BDG_VAR,3 ),
     &               BDG_PROC_OUT( N_BDG_VAR,NPRCS ),
     &               ADV_FLUXES( N_CGRID_SPC,8 ), 
     &               ADV_FLUXES_OUT( N_BDG_VAR,8 ) )
      END IF


      CALL STORE_BUDGET( BDGCF_ID, CONC, JDATE, JTIME )
      CSAV = CONC

      ! Correct VDIFF by subtracting emissions and dry deposition
      BDG_PROC( :,IPR_VDIF ) = BDG_PROC( :,IPR_VDIF ) - BDG_PROC( :,IPR_EMIS ) - BDG_PROC( :,IPR_DDEP )
      
      ! Calculate total mass change across output time step
      BDG_MASS( :,3 ) = BDG_MASS( :,2 ) - BDG_MASS( :,1 )

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
      
      BDG_YEAR0 = BDG_JDATE0 / 1000
      BDG_YEAR1 = BDG_JDATE1 / 1000
      CALL DAYMON( BDG_JDATE0, BDG_MONTH0, BDG_DAY0 )
      CALL DAYMON( BDG_JDATE1, BDG_MONTH1, BDG_DAY1 )
      BDG_YYYYMMDD0 = BDG_YEAR0*10000 + BDG_MONTH0*100 + BDG_DAY0
      BDG_YYYYMMDD1 = BDG_YEAR1*10000 + BDG_MONTH1*100 + BDG_DAY1
      
      WRITE( BDG_TIME0, '(I8,A1,I6.6)' ), BDG_YYYYMMDD0,':',BDG_JTIME0
      WRITE( BDG_TIME1, '(I8,A1,I6.6)' ), BDG_YYYYMMDD1,':',BDG_JTIME1
      
      ! Map CGRID Species to Budget Output Species
      BDG_PROC_OUT = 0.0
      BDG_MASS_OUT = 0.0
      ADV_FLUXES_OUT = 0.0
      DO I = 1,N_BDG_PAIRS
          BDG_PROC_OUT( MAP_toBDG( I ),: ) = BDG_PROC_OUT( MAP_toBDG( I ),: ) 
     &             + BDG_PROC( MAP_toCGRID( I ),: )
          BDG_MASS_OUT( MAP_toBDG( I ),: ) = BDG_MASS_OUT( MAP_toBDG( I ),: ) 
     &             + BDG_MASS( MAP_toCGRID( I ),: )
          ADV_FLUXES_OUT( MAP_toBDG( I ),: ) = ADV_FLUXES_OUT( MAP_toBDG( I ),: ) 
     &             + ADV_FLUXES( MAP_toCGRID( I ),: )
      END DO

#ifdef parallel
      ! Sum Changes Across All Processors
      DO J = 1,3
         DO I = 1,N_BDG_VAR
           BDG_MASS_OUT(I,J) = SUBST_GLOBAL_SUM( BDG_MASS_OUT(I,J) )
         END DO 
      END DO
      DO J = 1,NPRCS
         DO I = 1,N_BDG_VAR
           BDG_PROC_OUT(I,J) = SUBST_GLOBAL_SUM( BDG_PROC_OUT(I,J) )
         END DO
      END DO
      DO J = 1,8
         DO I = 1,N_BDG_VAR
           ADV_FLUXES_OUT(I,J) = SUBST_GLOBAL_SUM( ADV_FLUXES_OUT(I,J) )/ 10.0
         END DO
      END DO
#endif

      IF ( MYPE .EQ. 0 ) THEN
         ! Positive Residual means sum of processes is greater 
         ! than net change in mass
         BDG_PROC_OUT( :,IPR_XADV ) = BDG_PROC_OUT( :,IPR_XADV ) * 10.0
         BDG_PROC_OUT( :,IPR_YADV ) = BDG_PROC_OUT( :,IPR_YADV ) * 10.0

         RESID( : ) = SUM( BDG_PROC_OUT(:,:),2 ) - BDG_MASS_OUT( :,3 )  

         RESID_F(:) = SUM( BDG_PROC_OUT(:,3:NPRCS),2 ) + SUM( ADV_FLUXES_OUT,2)
     &               - BDG_MASS_OUT( :,3 )

         ! Write Species Process Changes to tab-separated file
         DO I = 1,N_BDG_VAR
            ! Default
!            WRITE( BDG_UNIT,'(3(A,A),F7.3,A,23(E11.4,A) )' ),TRIM(BDGSPEC(I)),tab, 
!     &         BDG_TIME0,tab,BDG_TIME1,tab, DT,tab, (BDG_MASS_OUT(I,J),tab,J=1,3 ),
!     &         ( ADV_FLUXES_OUT(I,J),tab,J=1,8 ),
!     &         BDG_PROC_OUT(I,1),tab,
!     &         ( BDG_PROC_OUT(I,J),tab,J=4,NPRCS ), RESID(I)

            ! Debugging
            WRITE( BDG_UNIT,'(3(A,A),F7.3,A,20(E11.4,A) )' ),TRIM(BDGSPEC(I)),tab, 
     &         BDG_TIME0,tab,BDG_TIME1,tab, DT,tab, (BDG_MASS_OUT(I,J),tab,J=1,3 ),
     &         SUM( ADV_FLUXES_OUT(I,1:4)),tab, SUM( ADV_FLUXES_OUT(I,5:8)),tab,
     &         ( BDG_PROC_OUT(I,J),tab,J=1,NPRCS ), RESID_F(I), tab, RESID(I)
         END DO
      END IF

      ! Assign Initial Concentration
      ! and zero Out all Rates, etc
      BDG_PROC = 0.0
      BDG_MASS( :,1 ) = BDG_MASS( :,2 )
      BDG_MASS( :,2 ) = 0
      BDG_MASS( :,3 ) = 0

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
 
! ----------------------------------------------------------------------
      subroutine map_chem_families( species0, spec_vec, nvec, out_vec )
! ----------------------------------------------------------------------
!     Return a vector outvec of size nvec, equal to the size of specvec.
!     Outvec is a logical which maps the input species name "species" 
!     to specvec, expanding any chemical families or keywords.
! ----------------------------------------------------------------------

      USE AERO_DATA, ONLY: N_MODE, MODESUFF, AEROSPC, N_AEROSPC
      USE RUNTIME_VARS, ONLY: LOGDEV
      USE UTILIO_DEFN
      USE EM_PARAM_MODULE

      IMPLICIT NONE

      CHARACTER(16), INTENT(IN) :: SPECIES0
      INTEGER      , INTENT(IN) :: NVEC
      CHARACTER(16), INTENT(IN) :: SPEC_VEC( NVEC )
      LOGICAL      , INTENT(OUT):: OUT_VEC( NVEC )

      CHARACTER(16) :: SPECIES

      INTEGER IFAM, ICHEM, IDX, JDX, IM, KDX, NCHEM
      CHARACTER(16) :: CHEM_NAME( 150 ), SN
      CHARACTER(200) :: XMSG

      ! Initialize Emissions Species Array
      SPECIES = SPECIES0
      CALL UPCASE( SPECIES )
      OUT_VEC = .FALSE.


      ! Find Indices of Species Relevant for "species"
      IF ( SPECIES .EQ. 'ALL' ) THEN
         ! Expand to Apply to All Species
         OUT_VEC = .TRUE.
      ELSE     
         ! Determine if the Species Label Refers to A Family and if So, 
         ! Apply the Rule to all members of that Family
         IFAM = INDEX1( SPECIES, NChemFamilies, ChemFamilyName )
         IF ( IFAM .EQ. 0 ) THEN
            NCHEM = 1
            CHEM_NAME(1) = SPECIES
         ELSE
            NCHEM = ChemFamilyNum( IFAM )
            CHEM_NAME(1:NCHEM) = ChemFamilyMembers( IFAM,1:NCHEM )
         END IF
      
         DO ICHEM = 1,NCHEM
           ! Find the Specific Species this Rule Identifies
           IDX = INDEX1( CHEM_NAME( ICHEM ), NVEC, SPEC_VEC )
           JDX = INDEX1( CHEM_NAME( ICHEM ), N_AEROSPC,  AEROSPC( : )%BULKNAME )
           IF ( IDX .NE. 0 ) THEN
             OUT_VEC( IDX ) = .TRUE.
           ELSE IF ( JDX .NE. 0 ) THEN
             ! This is an aerosol species, and it is being
             ! identified with a bulk name (no mode suffix). 
             ! We need to allow for all possible DIFF_SPC with
             ! all used suffixes
             SN = CHEM_NAME( ICHEM )
             DO IM = 1,N_MODE
               KDX = INDEX1( TRIM( SN )//MODESUFF( IM ), NVEC, SPEC_VEC )
               IF ( KDX .NE. 0 ) OUT_VEC( KDX ) = .TRUE.
             END DO
           ELSE
             WRITE( LOGDEV, '(/,3A,/,A,/,A,/,A,/,A)' ),
     &         'Species ',TRIM(SPECIES),' was used in the Emissions',
     &         ' Control Instructions Namelist but it is not a valid CMAQ ',
     &         'transported species or family. Please add it to one of the ',
     &         'input chemical namelists (ie. GC, AE, etc). Note that aerosol',
     &         'Number and Surface Area Species are not valid for scaling.'
             XMSG = 'Error in Emissions Map Processing.'
             CALL M3EXIT( 'Map_Chem_Families', 0, 0, XMSG, XSTAT1 )
            END IF
         END DO
      END IF 
 
      end subroutine map_chem_families
 

      END MODULE BUDGET_DEFN
