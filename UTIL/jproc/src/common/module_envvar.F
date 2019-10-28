
      MODULE GET_ENV_VARS

         IMPLICIT NONE
         PUBLIC :: GET_ENV_STRING, GET_ENV_FLAG, GET_ENV_INT, GET_ENV_REAL,
     &             GET_ENVLIST, VALUE_NAME, JUNIT 
              
         INTEGER, PARAMETER, PRIVATE :: LOGDEV = 6
         INTEGER, PARAMETER, PRIVATE :: MAX_LEN_WORD = 16
      
      CONTAINS

         SUBROUTINE VALUE_NAME( VAR_NAME, VAR_VALUE )
           IMPLICIT NONE
           CHARACTER*(*), INTENT(  IN ) :: VAR_NAME
           CHARACTER*(*), INTENT( OUT ) :: VAR_VALUE

           INTEGER :: STATUS
           
            CALL GET_ENV_STRING( VAR_NAME, " ", VAR_NAME, VAR_VALUE, STATUS )

         END SUBROUTINE VALUE_NAME
         SUBROUTINE GET_ENV_STRING( VAR_NAME, VAR_DESC, VAR_DEFAULT, VAR_VALUE, STATUS )
           IMPLICIT NONE
!arguments
           CHARACTER*(*), INTENT(  IN ) :: VAR_NAME
           CHARACTER*(*), INTENT(  IN ) :: VAR_DESC
           CHARACTER*(*), INTENT(  IN ) :: VAR_DEFAULT
           CHARACTER*(*), INTENT( OUT ) :: VAR_VALUE
           INTEGER,       INTENT( OUT ) :: STATUS 
!local
           CHARACTER( 586 ) :: MSG         ! Message text

           CALL GET_ENVIRONMENT_VARIABLE( NAME=VAR_NAME, VALUE= VAR_VALUE, STATUS=STATUS, TRIM_NAME=.TRUE.)

           IF( STATUS .LT. 0 ) THEN
              MSG = 'ERROR in environment value for ' // TRIM( VAR_NAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
           ELSE IF( STATUS .EQ. 1 )THEN
              STATUS  = -1
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
           CHARACTER( 586 ) :: MSG         ! Message text
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
                  WRITE(LOGDEV,' (A,L)')'Environment Variable '
     &            // TRIM( VAR_NAME ) // ' missing.'
     &            // ' Using default value of ',VAR_DEFAULT
                  STATUS = -1
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
         INTEGER FUNCTION GET_ENV_INT( VAR_NAME, VAR_DESC, VAR_DEFAULT, STATUS )
           IMPLICIT NONE
!arguments
           CHARACTER*(*), INTENT(  IN ) :: VAR_NAME
           CHARACTER*(*), INTENT(  IN ) :: VAR_DESC
           INTEGER,       INTENT(  IN ) :: VAR_DEFAULT
           INTEGER,       INTENT( OUT ) :: STATUS 
!local
           CHARACTER( 120 ) :: MSG         ! Message text
           CHARACTER( 120 ) :: VAR_VALUE
           CHARACTER(  5  ) :: REPLY
           
           INTEGER          :: READ_VALUE

           CALL GET_ENVIRONMENT_VARIABLE( NAME=VAR_NAME, VALUE= VAR_VALUE, STATUS=STATUS, TRIM_NAME=.TRUE.)

           IF( STATUS .LT. 0 ) THEN
              MSG = 'ERROR in environment value for ' // TRIM( VAR_NAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
              STATUS = 1
           ELSE IF( STATUS .EQ. 1 )THEN
              STATUS  = -1
              GET_ENV_INT = VAR_DEFAULT
              WRITE(LOGDEV,' (A,I8)')'Environment Variable '
     &        // TRIM( VAR_NAME ) // ' missing.'
     &        // ' Using default value of ',VAR_DEFAULT
           ELSE IF( STATUS .GT. 1 )THEN
              STATUS  = 1
              MSG = 'ERROR in environment value for ' // TRIM( VAR_NAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
              MSG = 'ERROR Processor does not support environment variables '
              WRITE(LOGDEV,'(a)')
           ELSE
              STATUS = 0 
              READ(VAR_VALUE , *)READ_VALUE
              GET_ENV_INT = READ_VALUE
              WRITE(MSG,'(a,I8)')'     Value for ' // TRIM( VAR_NAME ) // ': ',
     &        GET_ENV_INT
              WRITE(LOGDEV,'(a)')TRIM( MSG )
           END IF

         END FUNCTION GET_ENV_INT
         REAL FUNCTION GET_ENV_REAL( VAR_NAME, VAR_DESC, VAR_DEFAULT, STATUS )
           IMPLICIT NONE
!arguments
           CHARACTER*(*), INTENT(  IN ) :: VAR_NAME
           CHARACTER*(*), INTENT(  IN ) :: VAR_DESC
           REAL,          INTENT(  IN ) :: VAR_DEFAULT
           INTEGER,       INTENT( OUT ) :: STATUS 
!local
           CHARACTER( 120 ) :: MSG         ! Message text
           CHARACTER( 120 ) :: VAR_VALUE
           CHARACTER(  5  ) :: REPLY

           CALL GET_ENVIRONMENT_VARIABLE( NAME=VAR_NAME, VALUE= VAR_VALUE, STATUS=STATUS, TRIM_NAME=.TRUE.)

           IF( STATUS .LT. 0 ) THEN
              MSG = 'ERROR in environment value for ' // TRIM( VAR_NAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
              STATUS = 1
           ELSE IF( STATUS .EQ. 1 )THEN
              STATUS  = -1
              GET_ENV_REAL = VAR_DEFAULT
              WRITE(LOGDEV,' (A,ES13.6)')'Environment Variable '
     &        // TRIM( VAR_NAME ) // ' missing.'
     &        // ' Using default value of ',VAR_DEFAULT
           ELSE IF( STATUS .GT. 1 )THEN
              STATUS  = 1
              MSG = 'ERROR in environment value for ' // TRIM( VAR_NAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
              MSG = 'ERROR Processor does not support environment variables '
              WRITE(LOGDEV,'(a)')
           ELSE
              STATUS = 0 
              READ(VAR_VALUE,*)GET_ENV_REAL
              WRITE(MSG,'(a,ES12.4)')'     Value for ' // TRIM( VAR_NAME ) // ': ',
     &        GET_ENV_REAL
              WRITE(LOGDEV,'(a)')TRIM( MSG )
           END IF

         END FUNCTION GET_ENV_REAL
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
!          DD = '00' 
           WRITE(YYYY,'(I4)') (0 + VALUES(1))

          PRINT*,MONS(VALUES(2)) // DD // YYYY(1:4) 
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
         SUBROUTINE GET_ENVLIST ( ENV_VAR, NVARS, VAL_LIST, STATUS )

C get a list env var (quoted string of items delimited by white space,
C commas or semi-colons) and parse out the items into variables. Two data
C types: character strings and integers (still represented as strings in
C the env var vaules).
C Examples:
C 1)   setenv AVG_CONC_VARS "O3 NO NO2"
C 2)   setenv AVG_CONC_LAYS "2 5"          < start at two, end at 5
C 3)   setenv NPCOLSXNPROWS "4 3"
C 4)   setenv BCOL_ECOL "3 8"
C 5)   setenv BROW_EROW "2 10"
C 6)   setenv BLAY_ELAY "1 5"

C In example (1), not only parse out the named items "O3", "NO" and "NO2",
C but also obtain the count on the number of itmes (=3).

! Revision: 2013/02/11 David Wong: increased the max env var length from 256 to 1000
! 13 Dec 2013 J.Young: 1000 breaks BUFLEN in IOAPI's envgets.c. Change to 512.
! 17 Jun 2016 J.Young:  IOAPI's envgets.c BUFLEN has been increased to 10000.
! 20 Jun 2016 J.Young:  Forget IOAPI's envgets.c: use Fortran GETENV
! 16 Mar 2018 B.Hutzell: Removed IOAPI, changed from GOTO to Do loop, and 
!                        from GETENV to GET_ENVIRONMENT_VARIABLE intrinsic
           IMPLICIT NONE
           
           CHARACTER( * ),  INTENT ( IN )  :: ENV_VAR
           INTEGER,         INTENT ( OUT ) :: NVARS
           CHARACTER( * ),  INTENT ( OUT ) :: VAL_LIST( : )
           INTEGER,         INTENT ( OUT ) :: STATUS 
           
           INTEGER                          :: MAX_LEN 
           INTEGER                          :: LEN_EVAL
           CHARACTER( 16 )                  :: PNAME = 'GET_ENVLIST'
           CHARACTER(  1 )                  :: CHR
           CHARACTER( 132)                  :: XMSG

           CHARACTER( MAX_LEN_WORD*SIZE( VAL_LIST ) ) :: E_VAL
           
           INTEGER :: JP( MAX_LEN_WORD*SIZE( VAL_LIST ) )
           INTEGER :: KP( MAX_LEN_WORD*SIZE( VAL_LIST ) )
           INTEGER :: IP, V
           
           MAX_LEN = MAX_LEN_WORD * ( SIZE( VAL_LIST ) + 1 ) ! multiple by 17 to allow deliminator
C                    env_var_name
C                         |   env_var_value
C                         |        |
!           CALL GETENV( ENV_VAR, E_VAL )
C                                          env_var_name
C                                                |       env_var_value
C                                                |             |
           CALL GET_ENVIRONMENT_VARIABLE( NAME=ENV_VAR, VALUE=E_VAL, STATUS=STATUS, TRIM_NAME=.TRUE.)
           IF( STATUS .LT. 0 ) THEN
              XMSG = 'ERROR in environment value for ' // TRIM( ENV_VAR )
              WRITE(LOGDEV,'(a)')TRIM( XMSG )
              STATUS = 1
              STOP 
           ELSE IF( STATUS .EQ. 1 )THEN
              STATUS  = -1
              RETURN
           ELSE IF( STATUS .GT. 1 )THEN
              STATUS  = 1
              XMSG = 'ERROR in environment value for ' // TRIM( ENV_VAR )
              WRITE(LOGDEV,'(a)')TRIM( XMSG )
              XMSG = 'ERROR Processor does not support environment variables '
              WRITE(LOGDEV,'(a)')
              STOP
           END IF
           
           IF ( E_VAL .EQ. " " ) THEN
              STATUS = 1
              XMSG = 'Environment variable ' // ENV_VAR // ' not set'
              WRITE(LOGDEV,'(A,I8)')TRIM( XMSG ), SIZE( VAL_LIST )
              NVARS = 0
              RETURN
           END IF
           STATUS = 0

C Parse:

           NVARS = 1

C don't count until 1st char in string
           
           IP = 0
           LEN_EVAL = LEN_TRIM( E_VAL ) 
           IF ( LEN_EVAL .GT. MAX_LEN ) THEN
              XMSG = TRIM( PNAME ) // ': The Environment variable, '
     &            // TRIM( ENV_VAR ) // ',  has too long, greater than ' 
              WRITE(LOGDEV,'(A,I8)')TRIM( XMSG ), MAX_LEN
              STOP
           END IF
101        LOOP_101: DO  ! read list
              IP = IP + 1
              IF ( IP .GT. LEN_EVAL ) EXIT LOOP_101
              CHR = E_VAL( IP:IP )
              IF ( CHR .EQ. ' ' .OR. ICHAR ( CHR ) .EQ. 09 ) CYCLE LOOP_101
              IF( NVARS .GT. SIZE( VAL_LIST ) )THEN
                 XMSG = TRIM( PNAME ) // ':ERROR: Number of values in List, ' 
     &                //  TRIM( ENV_VAR ) 
     &                // ', greater than the size of its storage array, '
                      WRITE(LOGDEV,'(A,I4)')TRIM( XMSG ), SIZE( VAL_LIST )
                 STOP           
              END IF
              JP( NVARS ) = IP   ! 1st char           
201           LOOP_201: DO ! read word
                 IP = IP + 1
                 IF ( IP .GT. LEN_EVAL ) EXIT LOOP_101
                 CHR = E_VAL( IP:IP )
                 IF ( CHR .NE. ' ' .AND.
     &                CHR .NE. ',' .AND.
     &                CHR .NE. ';' .OR.
     &                ICHAR ( CHR ) .EQ. 09 ) THEN  ! 09 = horizontal tab
                    CYCLE LOOP_201
                 ELSE                               ! last char in word
                    V = JP( NVARS ) - KP( NVARS ) + 1
                    KP( NVARS ) = IP - 1 
                    IF( V .GT. MAX_LEN_WORD )THEN
                      XMSG =  'The word, ' // E_VAL( JP(NVARS):KP(NVARS) ) 
     &                     // ', in environment list, ' // TRIM( ENV_VAR )
     &                     // ', is too long, greater than '
                      WRITE(LOGDEV,'(A,I2)')TRIM( XMSG ), MAX_LEN_WORD
                      STOP
                    END IF
                    NVARS = NVARS + 1
                    EXIT LOOP_201
                 END IF 
              END DO LOOP_201
           END DO LOOP_101
           
           NVARS = NVARS - 1
           IF( NVARS .GT. SIZE( VAL_LIST ) )THEN
              XMSG = TRIM( PNAME ) // ':ERROR: Number of values in List, ' 
     &             //  TRIM( ENV_VAR ) // ', greater than '
              WRITE(LOGDEV,'(A,I4)')TRIM( XMSG ), SIZE( VAL_LIST )
              STOP           
           END IF
           
           DO V = 1, NVARS
              VAL_LIST( V ) = E_VAL( JP( V ):KP( V ) )
           END DO

           RETURN 
         END SUBROUTINE GET_ENVLIST 
         INTEGER FUNCTION JUNIT ()

C JUNIT uses an internal INQUIRE call to determine if a unit number is
C currently attached to a file.
C In the call INQUIRE(UNIT = IUN, NAMED = NMD), NMD is .TRUE. if unit (IUN)
C is connected to a file with a name, else it is .FALSE.

            IMPLICIT NONE
            
            INTEGER, PARAMETER :: LUNOUT = 6
            INTEGER, PARAMETER :: MAXUN = 99
            INTEGER, SAVE :: IUN
            LOGICAL NMD
            LOGICAL, SAVE :: AVAIL( MAXUN ) = .TRUE.   ! array
            INTEGER, SAVE :: IGO = 1
            
            GO TO ( 10001, 20001 ) IGO
10001       CONTINUE

C set unit numbers that are not available for I/O
            AVAIL( 1 ) = .FALSE.
            AVAIL( 5 ) = .FALSE.
            AVAIL( 6 ) = .FALSE.

C begin loop on unit numbers
            IUN = 10
10100       CONTINUE
            IF ( IUN .GT. MAXUN ) GO TO 30100
            INQUIRE ( UNIT = IUN, NAMED = NMD )
            IF ( NMD ) THEN
               AVAIL( IUN ) = .FALSE.
               GO TO 20100
            ELSE IF ( .NOT. AVAIL( IUN ) ) THEN
               GO TO 20100
            END IF
            JUNIT = IUN
            AVAIL( IUN ) = .FALSE.
            
            IGO = 2
            RETURN
20001       CONTINUE
            
20100       CONTINUE
            IUN = IUN + 1
            GO TO 10100
            
30100       CONTINUE
            WRITE( LUNOUT,2001 )
2001        FORMAT( / 1X, '*** ERROR ABORT in JUNIT ***' /
     &       1X, 'No more unit numbers available for I/O' )
            WRITE( LUNOUT,2003 ) ( IUN, AVAIL( IUN ), IUN = 1, MAXUN )
2003        FORMAT( 1X, 'Available unit numbers are: ' /
     &       3( 1X, 20( I2, '-', L1, 2X) / ) )
            STOP
         END FUNCTION JUNIT 
***********************************************************************
         SUBROUTINE JULIAN_YEAR_MONTH_DAY (JULDAY,YEAR,MONTH,DAY)

c     input year, month, day
c     input julian_date day (julday), year fraction (yrfrac)

            IMPLICIT NONE
            INTEGER, INTENT( IN     ) :: JULDAY
            INTEGER, INTENT( INOUT  ) :: YEAR, DAY, MONTH
            
            INTEGER   JDAY,YRLENGTH,LEAP,M4,M100,M400,
     &                I, J
            
            INTEGER, SAVE :: DAYTAB(13,2) 
            
            DATA  ( DAYTAB(I,1), I = 1, 13 ) /
     &                    0,31,28,31,30,31,30,31,31,30,31,30,31 /
            DATA  ( DAYTAB(I,2), I = 1, 13 ) /     
     &                    0,31,29,31,30,31,30,31,31,30,31,30,31 /
            
            LOGICAL, SAVE :: INITIALIZE = .TRUE.
            
            
            IF ( INITIALIZE ) THEN
               DO I = 1, 2
                  DO J = 2, 13
                     DAYTAB(J,I) = DAYTAB(J,I) + DAYTAB(J-1,I)
                  END DO
               END DO
               INITIALIZE = .FALSE.
            END IF
            
            LEAP = 1
            YRLENGTH = 365
            YEAR     = INT(JULDAY/1000)
            JDAY     = JULDAY - 1000*YEAR
            
            M4       = MOD(YEAR,4  )
            M100     = MOD(YEAR,100)
            M400     = MOD(YEAR,400)
            IF(((M4.EQ.0).AND.(M100.NE.0)).OR.(M400.EQ.0))THEN
             LEAP = 2
             YRLENGTH = 366
            ENDIF
            
            DO I= 2, 13
               IF( JDAY .GT. DAYTAB(I-1,LEAP) .AND. JDAY .LE. DAYTAB(I,LEAP) )THEN
                   MONTH = I-1
                   DAY = JDAY - DAYTAB(I-1,LEAP)
                   EXIT
               END IF
            END DO 
            
            RETURN
         END SUBROUTINE JULIAN_YEAR_MONTH_DAY
      END MODULE GET_ENV_VARS
