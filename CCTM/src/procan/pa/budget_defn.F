
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

      INTEGER, PARAMETER      :: BDGC0_ID = -1

      PUBLIC BUDGET_INIT, STORE_BUDGET, STORE_BUDGET_DDEP, 
     &       WRITE_BUDGET, BDGC0_ID


      INTEGER         :: BDG_UNIT
      INTEGER, PARAMETER      :: BDGCF_ID = -2

      REAL, ALLOCATABLE, SAVE :: BDG( :,: )
      REAL, ALLOCATABLE, SAVE :: BDG_MASS( :,: )

      INTEGER, SAVE :: BDG_JDATE0, BDG_JTIME0
      INTEGER, SAVE :: BDG_JDATE1, BDG_JTIME1
 
      CHARACTER( 9 )  :: tab

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
      INTEGER         :: I

!-----------------------------------------------------------------------

      ! Initialize Variables
      ALLOCATE( CONC( NCOLS,NROWS,NLAYS,N_CGRID_SPC ), STAT=ASTAT )
      CONC = CGRID

      ALLOCATE( BDG( N_CGRID_SPC,NPRCS ),
     &          BDG_MASS( N_CGRID_SPC,3),
     &          STAT=ASTAT )
      BDG = 0
      BDG_JDATE0 = JDATE
      BDG_JTIME0 = JTIME

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
     &         'YYYYmmdd:HHMMSS format and in UTC time zone.' )
          CALL LOG_MESSAGE( BDG_UNIT, 'Delta T (time) Units are hours.' )
          CALL LOG_MESSAGE( BDG_UNIT, 'Gas and Aerosol Mass Units are kg' )
          CALL LOG_MESSAGE( BDG_UNIT, 'Aerosol Number Units are total N' )
          CALL LOG_MESSAGE( BDG_UNIT, 'Aerosol Surface Area Units are m2' )
          WRITE( BDG_UNIT, * )


          ! Write Header
          !WRITE( FM, '( )' ), 
          WRITE( BDG_UNIT, '(41(A))' ), 'SPECIES',tab,'T_START',tab,
     &     'T_FINAL',tab,'T_DELTA',tab,'M_START',tab,'M_FINAL',tab,
     &     'M_DELTA',( tab, PROCNAME(i),i=1,NPRCS),tab,'RESID'

      END IF

      DEALLOCATE( CONC )

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
      REAL, ALLOCATABLE :: DENS( :,:,: )   ! air density (kg m-3)
      REAL, ALLOCATABLE :: MSFX2( :,:,: )  ! map scale factor
      REAL, ALLOCATABLE :: ZF( :,:,: )     ! height of layer top
      REAL, ALLOCATABLE :: CELLVOL( :,:,: )! cell volume
      LOGICAL :: FIRST_TIME = .TRUE.
      INTEGER ASTAT
      INTEGER L, I

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

      CELLVOL( :,:,1 ) = XCELL_GD * YCELL_GD * ZF( :,:,1 )
      DO L = 2,NLAYS
         CELLVOL( :,:,L ) = XCELL_GD * YCELL_GD * 
     &                        ( ZF( :,:,L ) - ZF( :,:,L-1) )
      END DO

      ! Convert Process Units to kg (mass), N (number), and m2 (surface
      ! area). Input gases are in ppm, aerosols in ug m-3, number in N
      ! m-3 and surface area in m2 m-3.
      DO I = 1,N_CGRID_SPC
         ! Gas - convert ppm to kg
         IF ( CGRID_MASK_GAS( I ) .OR.
     &        CGRID_MASK_NR( I )  .OR.
     &        CGRID_MASK_TRAC( I ) )
     &      BDG_CONC(:,:,:,I) = CONC(:,:,:,I) * DENS(:,:,:) * CELLVOL(:,:,:)
     &                          * CGRID_MW(I) / 1000.0

         ! Aerosol Mass:  ug m-3
         IF ( CGRID_MASK_AERO( I ) .AND.
     &        .NOT. CGRID_MASK_NUM( I ) .AND.
     &        .NOT. CGRID_MASK_SRF( I )  )
     &      BDG_CONC(:,:,:,I) = CONC(:,:,:,I) * CELLVOL 

         ! Aerosol Number: N m-3 -> N
         ! Aerosol Surface Area: m2 m-3 -> m2
         IF ( CGRID_MASK_NUM( I ) .OR.
     &        CGRID_MASK_SRF( I )      )
     &      BDG_CONC(:,:,:,I) = CONC(:,:,:,I) * CELLVOL
      
      END DO

      ! Sum and Store Process Change 
      IF ( IPR_ID .EQ. BDGC0_ID ) THEN
          BDG_MASS( :,1 ) = SUM( SUM( SUM( BDG_CONC(:,:,:,:),1 ),1 ),1 ) 
      ELSE IF ( IPR_ID .EQ. BDGCF_ID ) THEN
          BDG_MASS( :,2 ) = SUM( SUM( SUM( BDG_CONC(:,:,:,:),1 ),1 ),1 ) 
      ELSE
          BDG( :,IPR_ID ) = SUM( SUM( SUM( BDG_CONC(:,:,:,:),1 ),1 ),1 )
      END IF

      RETURN
      END SUBROUTINE STORE_BUDGET
 
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  Store Budget Data for Dry Deposition
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE STORE_BUDGET_DDEP( IPR_ID, DDEP, JDATE, JTIME )



      IMPLICIT NONE

      INCLUDE SUBST_CONST       ! Constants

      REAL                  :: DDEP( :,:,: )
      INTEGER, INTENT( IN ) :: IPR_ID            ! Process ID
      INTEGER, INTENT( IN ) :: JDATE, JTIME

      REAL, ALLOCATABLE, SAVE :: BDG_CONC( :,:,:,: )
      REAL, PARAMETER :: MWAIR_SI = MWAIR / 1.0E+03 ! kg mol-1
      REAL, ALLOCATABLE :: DENS( :,:,: )   ! air density (kg m-3)
      REAL, ALLOCATABLE :: MSFX2( :,:,: )  ! map scale factor
      REAL, ALLOCATABLE :: ZF( :,:,: )     ! height of layer top
      REAL, ALLOCATABLE :: CELLVOL( :,: )! cell volume
      LOGICAL :: FIRST_TIME = .TRUE.
      INTEGER ASTAT
      INTEGER L, I

      ! First time through, allocate Budget Concentration conversion
      ! array
      IF ( FIRST_TIME ) THEN
          FIRST_TIME = .FALSE.
          ALLOCATE ( DENS( NCOLS,NROWS,NLAYS ),
     &                MSFX2( NCOLS,NROWS,NLAYS ),
     &                ZF( NCOLS,NROWS,NLAYS ),
     &                CELLVOL( NCOLS,NROWS ),
     &                STAT = ASTAT )
      END IF

      ! Retrieve Cell Variables for converting concentrations to burden
      call interpolate_var ('DENS', jdate, jtime, DENS) ! kg m-3
      DENS = DENS / MWAIR_SI  ! mol m-3
      !call interpolate_var ('MSFX2', jdate, jtime, MSFX2) ! map scale factor
      call interpolate_var ('ZF', jdate, jtime, ZF) ! height of layer top

      CELLVOL( :,: ) = XCELL_GD * YCELL_GD * ZF( :,:,1 )

      ! Convert Process Units to kg (mass), N (number), and m2 (surface
      ! area). Input gases are in ppm, aerosols in ug m-3, number in N
      ! m-3 and surface area in m2 m-3.
      DO I = 1,N_CGRID_SPC
         ! Gas - convert ppm to kg
         IF ( CGRID_MASK_GAS( I ) .OR.
     &        CGRID_MASK_NR( I )  .OR.
     &        CGRID_MASK_TRAC( I ) )
     &      BDG_CONC(:,:,1,I) = DDEP(:,:,I) * DENS(:,:,1) * CELLVOL
     &                          * CGRID_MW(I) / 1000.0

         ! Aerosol Mass:  ug m-3
         IF ( CGRID_MASK_AERO( I ) .AND.
     &        .NOT. CGRID_MASK_NUM( I ) .AND.
     &        .NOT. CGRID_MASK_SRF( I )  )
     &      BDG_CONC(:,:,1,I) = DDEP(:,:,I) * CELLVOL 

         ! Aerosol Number: N m-3 -> N
         ! Aerosol Surface Area: m2 m-3 -> m2
         IF ( CGRID_MASK_NUM( I ) .OR.
     &        CGRID_MASK_SRF( I )      )
     &      BDG_CONC(:,:,1,I) = DDEP(:,:,I) * CELLVOL
      
      END DO

      ! Sum and Store Process Change 
      BDG( :,IPR_ID ) = SUM( SUM( BDG_CONC(:,:,1,:),1 ),1 )

      RETURN
      END SUBROUTINE STORE_BUDGET_DDEP

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write Budget Output to CSV File
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE WRITE_BUDGET( CONC, JDATE, JTIME, TSTEP )

      IMPLICIT NONE

      INTEGER I, J
      INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP(3)
      REAL                  :: CONC( :,:,:,: )
      REAL,ALLOCATABLE,SAVE :: RESID( : )
      LOGICAL, SAVE :: FIRST_TIME = .TRUE.

      INTEGER DT
      INTEGER BDG_YEAR0, BDG_MONTH0, BDG_DAY0, BDG_YYYYMMDD0
      INTEGER BDG_YEAR1, BDG_MONTH1, BDG_DAY1, BDG_YYYYMMDD1
      CHARACTER(15) :: BDG_TIME0, BDG_TIME1

      IF ( FIRST_TIME ) THEN
          FIRST_TIME = .FALSE.
          ALLOCATE ( RESID( N_CGRID_SPC ) )
      END IF


      CALL STORE_BUDGET( BDGCF_ID, CONC, JDATE, JTIME )

      ! Find Residual, Correct VDIFF
      BDG( :,IPR_VDIF ) = BDG( :,IPR_VDIF ) - BDG( :,IPR_EMIS ) + BDG( :,IPR_DDEP )
      
      BDG_MASS( :,3 ) = BDG_MASS( :,2 ) - BDG_MASS( :,1 )

      ! Positive Residual means sum of processes is greater 
      ! than net change in mass
      RESID( : ) = SUM( BDG,2 ) - BDG_MASS( :,3 )  

      ! Calculate Date, Time, and Length of Time Interval
      BDG_JDATE1 = JDATE
      BDG_JTIME1 = JTIME
      DT = TSTEP(1)

      BDG_YEAR0 = BDG_JDATE0 / 1000
      BDG_YEAR1 = BDG_JDATE1 / 1000
      CALL DAYMON( BDG_JDATE0, BDG_MONTH0, BDG_DAY0 )
      CALL DAYMON( BDG_JDATE1, BDG_MONTH1, BDG_DAY1 )
      BDG_YYYYMMDD0 = BDG_YEAR0*10000 + BDG_MONTH0*100 + BDG_DAY0
      BDG_YYYYMMDD1 = BDG_YEAR1*10000 + BDG_MONTH1*100 + BDG_DAY1

      WRITE( BDG_TIME0, '(I,A1,I)' ), BDG_YYYYMMDD0,':',BDG_JTIME0
      WRITE( BDG_TIME1, '(I,A1,I)' ), BDG_YYYYMMDD1,':',BDG_JTIME1

      ! Map CGRID Species to Budget Output Species


      ! Write Species Process Changes to tab-separated file
      DO I = 1,N_CGRID_SPC
        WRITE( BDG_UNIT,'(A,A )' ),CGRID_NAME(I),tab, BDG_TIME0,tab,
     &     BDG_TIME1,tab, DT,tab, (BDG_MASS(I,1),tab,J=1,3 ),
     &     ( BDG(I,J),tab,J=1,NPRCS ), RESID(I)
      END DO

      ! Assign Initial Concentration
      ! and zero Out all Rates, etc
      BDG = 0.0
      BDG_MASS( :,3 ) = 0
      BDG_MASS( :,1 ) = BDG_MASS( :,2 )
      BDG_MASS( :,2 ) = 0

      RETURN

      END SUBROUTINE WRITE_BUDGET

      END MODULE BUDGET_DEFN
