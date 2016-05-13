
      MODULE ENV_VARS

      IMPLICIT NONE
      
      CHARACTER(  15 )  :: CR_DATE     ! Run date
      CHARACTER(  16 )  :: AE_TAG      ! aerosol name used in MECHNAME
      CHARACTER(  16 )  :: GC_TAG      ! gas chem name used in MECHNAME
      CHARACTER(  16 )  :: AQ_TAG      ! aq chem name used in MECHNAME

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
      CHARACTER(  16 )  :: MECH_NO2EX  ! Mech name for NO2EX

      CHARACTER( 256 )  :: OUTPATH     ! Output directory
      CHARACTER( 256 )  :: TMPLPATH    ! Output directory
      
      LOGICAL  :: L_ALL_MECHS          ! Flag for all mechs (F=1 mech)

      LOGICAL  :: LWR_COPY             ! Flag to include copyright notice
      LOGICAL  :: LWR_CVS_HDR          ! Flag to include CVS headr line
      LOGICAL  :: LPAR_NEG             ! Flag for negative PAR stoich (cb4)
      LOGICAL  :: LNO2EX_CYCLE         ! Flag for to include NO2EX cycle in groups 1 & 2
      LOGICAL  :: LDEGRADE_SUBS        ! Flag for include degrade codes for HAPs


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


!       USE M3UTILIO           ! IOAPI parameters and functions declarations

        IMPLICIT NONE

C..INCLUDES:  
!         INCLUDE SUBST_IOPARMS   ! IOAPI parameters
!         INCLUDE SUBST_IODECL    ! IOAPI declarations
      
C..ARGUMENTS: None
    
C..PARAMETERS: None

C..EXTERNAL FUNCTIONS:


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

         INTEGER :: EPOS                  ! String end position
         INTEGER :: STATUS                ! Status code
         INTEGER :: IDATE                 ! Wall-clock date
         INTEGER :: ITIME                 ! Wall clock time

         LOGICAL :: LERROR                ! Error Flag


C**********************************************************************


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the time and date to be used as the time stamp for all outputs;
c  Convert from GMT to EDT or EST
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      CALL GETDTTIME( IDATE, ITIME )

 !     IF( ISDSTIME( IDATE ) ) THEN
 !        CALL NEXTIME( IDATE, ITIME, -40000 )
         TZ = ' EDT '
 !     ELSE
 !        CALL NEXTIME( IDATE, ITIME, -50000 )
 !       TZ = ' EST '
 !     END IF

!      TIMNDAT = DT2STR( IDATE, ITIME )

!      CR_DATE = ADJUSTL( TIMNDAT( 10 : 24 ) )

       CALL GET_DDMONYY(CR_DATE)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get flags for versions of mechanism to create
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         L_ALL_MECHS  = .FALSE.
!         ENV_DESC = 'Flag do all mechanisms or one'
!         L_ALL_MECHS = ENVYN( ALL_MECHS, ENV_DESC, L_ALL_MECHS, STATUS )


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get names to use for the mechanism
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ENV_DFLT = ' '
         ENV_DESC = 'gas mechanism name'        
         CALL GET_ENV_STRING( GC_NAME, ENV_DESC, ENV_DFLT, GC_TAG, STATUS )
         IF( STATUS .NE. 0 .AND. LEN(GC_TAG) .GT. 1 ) THEN
           MSG = TRIM( PNAME ) // ': Gas phase mechanism not defined' 
           WRITE(LOGDEV,'(a)')TRIM( MSG )
           STOP
         END IF

         ENV_DFLT = ' '
         ENV_DESC = 'aerosol mechanism name'        
         CALL GET_ENV_STRING( AE_NAME, ENV_DESC, ENV_DFLT, AE_TAG, STATUS )
         IF( STATUS .NE. 0 .AND. LEN(AE_TAG) .GT. 1 ) THEN
           MSG = TRIM( PNAME ) // ':  Aerosol phase mechanism not defined' 
           WRITE(LOGDEV,'(a)')TRIM( MSG )
           STOP
         END IF

         ENV_DFLT = ' '
         ENV_DESC = 'aq chem mechanism name'        
         CALL GET_ENV_STRING( AQ_NAME, ENV_DESC, ENV_DFLT, AQ_TAG, STATUS )
         IF( STATUS .NE. 0 .AND. LEN(AQ_TAG) .GT. 1 ) THEN
           MSG = TRIM( PNAME ) // ': Aqueous phase mechanism not defined' 
           WRITE(LOGDEV,'(a)')TRIM( MSG )
           STOP
         END IF

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
         ENV_DESC = 'Flag to write copyright notice'
         LWR_COPY = GET_ENV_FLAG( COPYRT_FLAG, ENV_DESC, LWR_COPY, STATUS )

         LWR_CVS_HDR  = .FALSE.
         ENV_DESC = 'Flag to write copyright notice'
         LWR_CVS_HDR = GET_ENV_FLAG( CVS_HDR_FLAG, ENV_DESC, LWR_CVS_HDR, STATUS )

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
         ENV_DESC = 'Flag to include PAR negative stoichiometry'
         LNO2EX_CYCLE  = GET_ENV_FLAG( NO2EX_FLAG, ENV_DESC, LNO2EX_CYCLE, STATUS )

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

         IF( LNO2EX_CYCLE ) THEN
            ENV_DFLT = ' '
            ENV_DESC = 'Mechanism name for excited NO2 oxygen (NO2EX)'
            CALL GET_ENV_STRING( 'MECH_NO2EX', ENV_DESC, ENV_DFLT, MECH_NO2EX, STATUS)

            IF( STATUS .NE. 0 ) THEN
               MSG = 'ERROR: Environment variable MECH_NO2EX not found'
               WRITE(LOGDEV,'(a)')TRIM( MSG )
               LERROR = .TRUE.
            END IF
         END IF


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
         SUBROUTINE GET_ENV_STRING( VAR_NAME, VAR_DESC, VAR_DEFAULT, VAR_VALUE, STATUS )
           IMPLICIT NONE
!arguments
           CHARACTER*(*), INTENT(  IN ) :: VAR_NAME
           CHARACTER*(*), INTENT(  IN ) :: VAR_DESC
           CHARACTER*(*), INTENT(  IN ) :: VAR_DEFAULT
           CHARACTER*(*), INTENT( OUT ) :: VAR_VALUE
           INTEGER,       INTENT( OUT ) :: STATUS 
!local
           CHARACTER( 120 ) :: MSG         ! Message text

           CALL GET_ENVIRONMENT_VARIABLE( NAME=VAR_NAME, VALUE= VAR_VALUE, STATUS=STATUS, TRIM_NAME=.TRUE.)

           IF( STATUS .LT. 0 ) THEN
              MSG = 'ERROR in environment value for ' // TRIM( VAR_NAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
           ELSE IF( STATUS .EQ. 1 )THEN
              STATUS  = 0
              VAR_VALUE = TRIM( VAR_DEFAULT ) 
           ELSE IF( STATUS .GT. 1 )THEN
              STATUS  = 1
              MSG = 'ERROR in environment value for ' // TRIM( VAR_NAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
              MSG = 'ERROR Processor does not support environment variables '
              WRITE(LOGDEV,'(a)')
           END IF
           IF( STATUS .EQ. 0 )THEN
               MSG = '     Value for ' // TRIM( VAR_NAME ) // ': ' // TRIM( VAR_VALUE )
               WRITE(LOGDEV,'(a)')TRIM( MSG )
           END IF

         END SUBROUTINE GET_ENV_STRING
         LOGICAL FUNCTION GET_ENV_FLAG( VAR_NAME, VAR_DESC, VAR_DEFAULT, STATUS )
           IMPLICIT NONE
!arguments
           CHARACTER*(*), INTENT(  IN ) :: VAR_NAME
           CHARACTER*(*), INTENT(  IN ) :: VAR_DESC
           LOGICAL,       INTENT(  IN ) :: VAR_DEFAULT
           INTEGER,       INTENT( OUT ) :: STATUS 
!local
           CHARACTER( 120 ) :: MSG         ! Message text
           CHARACTER(  1  ) :: VAR_VALUE
           CHARACTER(  5  ) :: REPLY

           CALL GET_ENVIRONMENT_VARIABLE( NAME=VAR_NAME, VALUE= VAR_VALUE, STATUS=STATUS, TRIM_NAME=.TRUE.)

           IF( STATUS .LT. 0 ) THEN
              MSG = 'ERROR in environment value for ' // TRIM( VAR_NAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
              STATUS = 1
           ELSE IF( STATUS .EQ. 1 )THEN
              STATUS  = -1
              GET_ENV_FLAG = VAR_DEFAULT
           ELSE IF( STATUS .GT. 1 )THEN
              STATUS  = 1
              MSG = 'ERROR in environment value for ' // TRIM( VAR_NAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
              MSG = 'ERROR Processor does not support environment variables '
              WRITE(LOGDEV,'(a)')
           ELSE
              STATUS = 0 
              IF( VAR_VALUE(1:1) .EQ. 'T' .OR. VAR_VALUE(1:1) .EQ. 'Y' )THEN
                  GET_ENV_FLAG = .TRUE.
              ELSE IF( VAR_VALUE(1:1) .EQ. 't' .OR. VAR_VALUE(1:1) .EQ. 'y' )THEN
                  GET_ENV_FLAG = .TRUE.
              ELSE IF(  VAR_VALUE(1:1) .EQ. 'F' .OR. VAR_VALUE(1:1) .EQ. 'N' )THEN
                  GET_ENV_FLAG = .FALSE.
              ELSE IF(  VAR_VALUE(1:1) .EQ. 'f' .OR. VAR_VALUE(1:1) .EQ. 'n' )THEN
                  GET_ENV_FLAG = .FALSE.
              ELSE 
                  WRITE(LOGDEV,' (A,L)')'Environment Variable '
     &            // TRIM( VAR_NAME ) // ' must equal T, Y, F, or N.'
     &            // ' Using default value of ',VAR_DEFAULT
                  STATUS = -1
                  GET_ENV_FLAG = VAR_DEFAULT
              END IF
           END IF
           IF( STATUS .EQ. 0 )THEN
              IF( GET_ENV_FLAG )THEN
                  REPLY = 'TRUE '
              ELSE
                  REPLY = 'FALSE'
              END IF
              WRITE(MSG,'(a,L1,a)')'     Value for ' // TRIM( VAR_NAME ) // ': ',GET_ENV_FLAG,
     &        ' returning ' // REPLY
              WRITE(LOGDEV,'(a)')TRIM( MSG )
           END IF 

         END FUNCTION GET_ENV_FLAG
         SUBROUTINE GET_DDMONYY(DATE)

            IMPLICIT NONE
            CHARACTER(*), INTENT(INOUT) :: DATE

            CHARACTER( 2 ) :: DD
            CHARACTER( 3 ) :: MONS(1:12)
            CHARACTER( 4 ) :: YYYY
            INTEGER        :: STIME, VALUES(8)

            MONS = (/'Jan','Feb','Mar','Apr','May','Jun',
     &               'Jul','Aug','Sep','Oct','Nov','Dec' /)

           CALL DATE_AND_TIME(VALUES=VALUES)

           WRITE(  DD,'(I2)') VALUES(3)
!	   DD = '00' 
           WRITE(YYYY,'(I4)') (0 + VALUES(1))

          PRINT*,MONS(VALUES(2)) // DD // YYYY(1:4) 
!	  print*,VALUES(:)
          DATE = MONS(VALUES(2)) // ' ' // DD // ', ' // YYYY(1:4)

        END SUBROUTINE GET_DDMONYY
        INTEGER FUNCTION NAME_INDEX( NAME, N_NAMES, NAMES )
           IMPLICIT NONE
         
           CHARACTER(*), INTENT( IN ) :: NAME
           CHARACTER(*), INTENT( IN ) :: NAMES( : )
           INTEGER,      INTENT( IN ) :: N_NAMES

           INTEGER :: N,M

           NAME_INDEX = 0

           M =  SIZE( NAMES )
           IF( M .LT. 1 )RETURN

           DO N = 1, M
              IF( NAME .EQ. NAMES( N ) )THEN
                  NAME_INDEX = N
                  RETURN
              END IF
           END DO
           RETURN
         END FUNCTION NAME_INDEX

      END MODULE ENV_VARS
