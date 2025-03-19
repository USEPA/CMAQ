
      MODULE ENV_VARS

      USE GET_ENV_FUNCTIONS           ! routines that retrieve environment data

      IMPLICIT NONE
      
      CHARACTER(  15 )  :: CR_DATE     ! Run date
!      CHARACTER(  16 )  :: AE_TAG      ! aerosol name used in MECHNAME
!      CHARACTER(  16 )  :: GC_TAG      ! gas chem name used in MECHNAME
!      CHARACTER(  16 )  :: AQ_TAG      ! aq chem name used in MECHNAME

      CHARACTER(  16 )  :: MECH_NO     ! Mech name for NO
      CHARACTER(  16 )  :: MECH_NO2    ! Mech name for NO2
      CHARACTER(  16 )  :: MECH_O3     ! Mech name for O3
      CHARACTER(  16 )  :: MECH_O3P    ! Mech name for O3P
      CHARACTER(  16 )  :: MECH_O1D    ! Mech name for O1D
      CHARACTER(  16 )  :: MECH_OH     ! Mech name for OH
      CHARACTER(  16 )  :: MECH_HO2    ! Mech name for HO2
      CHARACTER(  16 )  :: MECH_HONO   ! Mech name for HONO
      CHARACTER(  16 )  :: MECH_HNO4   ! Mech name for HNO4
      CHARACTER(  16 )  :: MECH_PAN    ! Mech name for PAN
      CHARACTER(  16 )  :: MECH_C2O3   ! Mech name for C2O3
      CHARACTER(  16 )  :: MECH_N2O5   ! Mech name for N2O5
      CHARACTER(  16 )  :: MECH_NO3    ! Mech name for NO3
      CHARACTER(  16 )  :: MECH_NO2EX  = 'NO2EX' ! Mech name for NO2EX

      CHARACTER( 586 )  :: OUTPATH     ! Output directory
      CHARACTER( 586 )  :: TMPLPATH    ! Output directory
      
      LOGICAL  :: L_ALL_MECHS          ! Flag for all mechs (F=1 mech)

      LOGICAL  :: LWR_COPY             ! Flag to include copyright notice
      LOGICAL  :: LWR_CVS_HDR          ! Flag to include CVS headr line
      LOGICAL  :: LPAR_NEG             ! Flag for negative PAR stoich (cb4)
      LOGICAL  :: LNO2EX_CYCLE         ! Flag for to include NO2EX cycle in groups 1 & 2
      LOGICAL  :: LDEGRADE_SUBS        ! Flag for include degrade codes for HAPs

      REAL     :: TIME_STEP = 2.5      ! maximum solver time step in minutes
      
      INTEGER, PARAMETER :: LOGDEV = 6
      
      CONTAINS

         SUBROUTINE GET_ENVS
C*************************************************************************
C
C        FUNCTION: Gets environment variables
C             
C        PRECONDITIONS: None
C 
C        KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C        REVISION HISTORY: Created by Jerry Gipson, February, 2004
C                          Modified Nov 04 by JG to get group species names
C                            vi environment variables
C                          Modified Dec 09 by JG to remove reset of LERROR to
C                             false for each species name retrieval
C                          Modified Dec 09 by JG to allow for inclusion
C                             of NO2EX cycle in Groups 1 and 2
C                   
C*************************************************************************

        IMPLICIT NONE

C..INCLUDES: None
      
C..ARGUMENTS: None
    
C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:

         CHARACTER(  16 ) :: PNAME = 'GET_ENVS'     ! Program Name

         CHARACTER(   5 ) :: TZ          ! Time zone
         CHARACTER(  16 ) :: ENV_DFLT    ! Environment variable default value
         CHARACTER(  24 ) :: TIMNDAT     ! Wall clock time and date
         CHARACTER(  80 ) :: ENV_DESC    ! Environment variable description
         CHARACTER(  80 ) :: MSG         ! Message text
         CHARACTER( 256 ) :: RET_VAL     ! Returned value of env var

c........env var names 
         CHARACTER(  16 ) :: ALL_MECHS     = 'ALL_MECHS'
         CHARACTER(  16 ) :: GC_NAME       = 'GC_NAME'
         CHARACTER(  16 ) :: AE_NAME       = 'AE_NAME'
         CHARACTER(  16 ) :: AQ_NAME       = 'AQ_NAME'
         CHARACTER(  16 ) :: OUTDIR        = 'OUTDIR'
         CHARACTER(  16 ) :: TMPLDIR       = 'TMPLDIR'
         CHARACTER(  16 ) :: COPYRT_FLAG   = 'COPYRT_FLAG'
         CHARACTER(  16 ) :: CVS_HDR_FLAG  = 'CVS_HDR_FLAG'
         CHARACTER(  16 ) :: PAR_NEG_FLAG  = 'PAR_NEG_FLAG'
         CHARACTER(  16 ) :: NO2EX_FLAG    = 'NO2EX_CYCLE'
         CHARACTER(  16 ) :: DEGRADE_SUBS  = 'DEGRADE_SUBS'
         CHARACTER(  16 ) :: SOLVER_DELT   = 'SOLVER_DELT'

         INTEGER :: EPOS                  ! String end position
         INTEGER :: STATUS                ! Status code
         INTEGER :: IDATE                 ! Wall-clock date
         INTEGER :: ITIME                 ! Wall clock time

         LOGICAL :: LERROR                ! Error Flag

         REAL    :: DELT         
C**********************************************************************


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the time and date to be used as the time stamp for all outputs;
c  Convert from GMT to EDT or EST
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       CALL GET_DDMONYY(CR_DATE)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get flags for versions of mechanism to create
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         L_ALL_MECHS  = .FALSE.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get names to use for the mechanism
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!        ENV_DFLT = ' '
!        ENV_DESC = 'gas mechanism name'        
!        CALL GET_ENV_STRING( GC_NAME, ENV_DESC, ENV_DFLT, GC_TAG, STATUS )
!        IF( STATUS .NE. 0 .AND. LEN(GC_TAG) .GT. 1 ) THEN
!          MSG = TRIM( PNAME ) // ': Gas phase mechanism not defined' 
!          WRITE(LOGDEV,'(a)')TRIM( MSG )
!          STOP
!        END IF

!        ENV_DFLT = ' '
!        ENV_DESC = 'aerosol mechanism name'        
!        CALL GET_ENV_STRING( AE_NAME, ENV_DESC, ENV_DFLT, AE_TAG, STATUS )
!        IF( STATUS .NE. 0 .AND. LEN(AE_TAG) .GT. 1 ) THEN
!          MSG = TRIM( PNAME ) // ':  Aerosol phase mechanism not defined' 
!          WRITE(LOGDEV,'(a)')TRIM( MSG )
!          STOP
!        END IF

!        ENV_DFLT = ' '
!        ENV_DESC = 'aq chem mechanism name'        
!        CALL GET_ENV_STRING( AQ_NAME, ENV_DESC, ENV_DFLT, AQ_TAG, STATUS )
!        IF( STATUS .NE. 0 .AND. LEN(AQ_TAG) .GT. 1 ) THEN
!          MSG = TRIM( PNAME ) // ': Aqueous phase mechanism not defined' 
!          WRITE(LOGDEV,'(a)')TRIM( MSG )
!          STOP
!        END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the name of the output directory and template directory
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ENV_DFLT = './output'
         ENV_DESC = 'Output directory'        
         CALL GET_ENV_STRING( OUTDIR, ENV_DESC, ENV_DFLT, OUTPATH, STATUS)

         IF( STATUS .GT. 0 ) THEN
            MSG = 'ERROR: no path assigned to OUTDIR'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
         END IF

         ENV_DFLT = './tmpldir'
         ENV_DESC = 'Template directory'        
         CALL GET_ENV_STRING( TMPLDIR, ENV_DESC, ENV_DFLT, TMPLPATH, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: no path assigned to TMPLDIR'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
         END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the flags for writing copyright and CVS header
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LWR_COPY  = .FALSE.
!         ENV_DESC = 'Flag to write copyright notice'
!         LWR_COPY = GET_ENV_FLAG( COPYRT_FLAG, ENV_DESC, LWR_COPY, STATUS )

         LWR_CVS_HDR  = .FALSE.
!         ENV_DESC = 'Flag to write cvs hear notice'
!         LWR_CVS_HDR = GET_ENV_FLAG( CVS_HDR_FLAG, ENV_DESC, LWR_CVS_HDR, STATUS )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the flag for including PAR negative stoichiometry
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LPAR_NEG  = .FALSE.
         ENV_DESC = 'Flag to include PAR negative stoichiometry'
         LPAR_NEG = GET_ENV_FLAG( PAR_NEG_FLAG, ENV_DESC, LPAR_NEG, STATUS )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the flag for including Degradation routines
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LDEGRADE_SUBS  = .FALSE.
         ENV_DESC = 'Flag to include call to HAPs degradation routines'
         LDEGRADE_SUBS = GET_ENV_FLAG( DEGRADE_SUBS, ENV_DESC, LDEGRADE_SUBS, STATUS )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the flag for including NO2EX in groups 1 and 2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LNO2EX_CYCLE  = .FALSE.
!         ENV_DESC = 'Flag to include PAR negative stoichiometry'
!         LNO2EX_CYCLE  = GET_ENV_FLAG( NO2EX_FLAG, ENV_DESC, LNO2EX_CYCLE, STATUS )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the value for maximum solver timestep
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ENV_DESC  = 'Maximum EBI Solver TimeStep (minutes)'
         TIME_STEP = GET_ENV_REAL( SOLVER_DELT, ENV_DESC, TIME_STEP, STATUS )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the mechanism names for group species
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         LERROR = .FALSE.

c..Group 1 species
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for nitrogen dioxide (NO2)'        
         CALL GET_ENV_STRING( 'MECH_NO2', ENV_DESC, ENV_DFLT, MECH_NO2, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_NO2 not found'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for nitric oxide (NO)'        
         CALL GET_ENV_STRING( 'MECH_NO', ENV_DESC, ENV_DFLT, MECH_NO, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_NO not found'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
            LERROR = .TRUE.
         END IF


c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for ozone (O3)'        
         CALL GET_ENV_STRING( 'MECH_O3', ENV_DESC, ENV_DFLT, MECH_O3, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_O3 not found'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for excited-state atomic oxygen (O1D)'
         CALL GET_ENV_STRING( 'MECH_O1D', ENV_DESC, ENV_DFLT, MECH_O1D, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_O1D not found'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for ground-state atomic oxygen (O3P)'
         CALL GET_ENV_STRING( 'MECH_O3P', ENV_DESC, ENV_DFLT, MECH_O3P, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_O3P not found'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
            LERROR = .TRUE.
         END IF

!         IF( LNO2EX_CYCLE ) THEN
!            ENV_DFLT = ' '
!            ENV_DESC = 'Mechanism name for excited NO2 oxygen (NO2EX)'
!            CALL GET_ENV_STRING( 'MECH_NO2EX', ENV_DESC, ENV_DFLT, MECH_NO2EX, STATUS)
!
!            IF( STATUS .NE. 0 ) THEN
!               MSG = 'ERROR: Environment variable MECH_NO2EX not found'
!               WRITE(LOGDEV,'(a)')TRIM( MSG )
!               LERROR = .TRUE.
!            END IF
!         END IF


c..Group 2 species
c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for hydroxyl radical (OH)'
         CALL GET_ENV_STRING( 'MECH_OH', ENV_DESC, ENV_DFLT, MECH_OH, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_OH not found'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for hydroperoxy radical (HO2)'
         CALL GET_ENV_STRING( 'MECH_HO2', ENV_DESC, ENV_DFLT, MECH_HO2, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_HO2 not found'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for nitrous acid (HONO)'
         CALL GET_ENV_STRING( 'MECH_HONO', ENV_DESC, ENV_DFLT, MECH_HONO, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_HONO not found'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for pernitric acid (HNO4)'
         CALL GET_ENV_STRING( 'MECH_HNO4', ENV_DESC, ENV_DFLT, MECH_HNO4, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_HNO4 not found'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
            LERROR = .TRUE.
         END IF

c..Group 3 species
c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for peroxy acetyl nitrate (PAN)'
         CALL GET_ENV_STRING( 'MECH_PAN', ENV_DESC, ENV_DFLT, MECH_PAN, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_PAN not found'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for acetyl percoxy radical (C2O3)'
         CALL GET_ENV_STRING( 'MECH_C2O3', ENV_DESC, ENV_DFLT, MECH_C2O3, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_C2O3 not found'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
            LERROR = .TRUE.
         END IF

c..Group 4 species
c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for nitrate radical (NO3)'
         CALL GET_ENV_STRING( 'MECH_NO3', ENV_DESC, ENV_DFLT, MECH_NO3, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_NO3 not found'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for nitrogen pentoxide (N2O5)'
         CALL GET_ENV_STRING( 'MECH_N2O5', ENV_DESC, ENV_DFLT, MECH_N2O5, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_N2O5 not found'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
            LERROR = .TRUE.
         END IF

         IF( LERROR ) THEN
           MSG = TRIM( PNAME ) // ': Stopping because of error in defining group species'
           WRITE(LOGDEV,'(a)')TRIM( MSG )
           STOP
         END IF


         RETURN

92000    FORMAT( /2X, 'The following mechanism versions will be created:' 
     &           / A / 1X )

         END SUBROUTINE GET_ENVS
      END MODULE ENV_VARS
