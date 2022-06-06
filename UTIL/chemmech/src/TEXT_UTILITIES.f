       MODULE TEXT_UTILITIES
       
         IMPLICIT NONE

         INTEGER, PARAMETER, PRIVATE :: MAX_LEN_WORD = 16
         INTEGER, PARAMETER, PRIVATE :: LOGDEV       =  6
       
       CONTAINS
       
           SUBROUTINE UCASE ( STR )
C***********************************************************************
C  Routine to change character string to upper characters
C***********************************************************************
              IMPLICIT NONE
              
              CHARACTER, INTENT( INOUT ) :: STR*( * )
              INTEGER I
              INTEGER K
              
              DO I = 1, LEN(STR)
                K = ICHAR(STR(I:I))
                IF ( ( K .GE. 97 ) .AND. ( K .LE. 122 ) )
     &            STR( I:I ) = CHAR( K - 32 )
              END DO
              
              RETURN
           END SUBROUTINE UCASE
 
          SUBROUTINE  CONVERT_CASE ( BUFFER, UPPER )
C***********************************************************************

C  subroutine body starts at line  41
C
C  FUNCTION:  converts to upcase or lower the text in BUFFER
C             based on values of logic flag UPPER
C
C  PRECONDITIONS REQUIRED:  text is ASCII
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:  prototype 1/91 by CJC
C
C***********************************************************************
            IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
            CHARACTER*(*), INTENT( INOUT ) :: BUFFER
            LOGICAL,       INTENT( IN )    :: UPPER
C...........   PARAMETER:  ASCII for 'a', 'z', 'A'
            INTEGER, PARAMETER :: IA    = 97
            INTEGER, PARAMETER :: IZ    = 122
            INTEGER, PARAMETER :: AADIF = 32
C...........   SCRATCH LOCAL VARIABLES and their descriptions:
            INTEGER       I, L
            INTEGER       C
            INTEGER       FACTOR
            INTEGER       STRT, FINI
C***********************************************************************
C   begin body of subroutine  UPCASE

            L  =  LEN_TRIM ( BUFFER )
            IF( UPPER )THEN
                FACTOR =  - AADIF
                STRT   =    IA
                FINI   =    IZ
            ELSE
                FACTOR =    AADIF
                STRT   =    IA - AADIF
                FINI   =    IZ - AADIF
            END IF
            
            DO  I = 1 , L
                C = ICHAR ( BUFFER ( I:I ) )
                IF ( C .GE. STRT  .AND.  C .LE. FINI ) THEN
                    BUFFER ( I:I ) = CHAR ( C + FACTOR )
                END IF
111         END DO        !  end loop on I
            
            RETURN
          END SUBROUTINE CONVERT_CASE
      
          FUNCTION Replace_Text (s,text,rep)  RESULT(outs)
             Implicit None
          
             CHARACTER*(*), Intent( In ) :: s,text,rep
             CHARACTER(LEN(S))           :: outs     ! provide outs with extra 100 char len
             INTEGER                     :: i, nt, nr
          
             outs = s
             nt = LEN_TRIM(text)
             nr = LEN_TRIM(rep)
             DO
                i = INDEX(outs,text(:nt)) 
                IF (i .Eq. 0) EXIT
                outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
             END DO
          END FUNCTION Replace_Text
          FUNCTION Count_Text (s,text)  RESULT(icount)
             Implicit None
             CHARACTER*(*), Intent( In ) :: s,text
             CHARACTER(LEN(S))           :: outs     ! provide outs with extra 100 char len
             INTEGER                     :: i, nt, nr
             INTEGER                     :: icount
             
          
             outs = s
          
             nt = LEN_TRIM(text)
             icount = 0
          
             DO
                i = INDEX(outs,text(:nt)) 
                IF (i .Eq. 0) EXIT
                outs = outs(:i-1) // outs(i+nt:)
                icount = icount + 1
             END DO
          END FUNCTION Count_Text
          FUNCTION Tailing_Comment (s,text)  RESULT(outs)
             Implicit None
          
             CHARACTER*(*), Intent( In ) :: s,text
             CHARACTER(LEN(S))           :: tail,outs     ! provide outs with extra 100 char len
             INTEGER                     :: i, j, nt, nr,icount
          
             nt = LEN_TRIM(s)
             nr = nt / LEN_TRIM(text)
             icount = 0
             outs = ' '
             i = INDEX(s,text,BACK=.TRUE.) 
             IF ( i.Eq.0 .OR. i.Eq.nt ) THEN
                 RETURN
             ELSE
                 outs = ADJUSTL( s(i+1:nt) )
             END IF
          END FUNCTION Tailing_Comment
! --------------------------------------------------------------------------------
        SUBROUTINE PARSE_STRING ( ENV_VAL, NVARS, VAL_LIST )

! takes a string of items delimited by white space,
! commas or semi-colons) and parse out the items into variables. Two data
! types: character strings and integers (still represented as strings in
! the env var vaules).
          
          IMPLICIT NONE

          CHARACTER( * ), INTENT ( IN )           :: ENV_VAL
          INTEGER,        INTENT ( OUT )          :: NVARS
          CHARACTER( * ), INTENT ( OUT )          :: VAL_LIST( : )

          INTEGER             :: MAX_LEN
          INTEGER             :: LEN_EVAL
          CHARACTER( 16 )     :: PNAME = 'PARSE_STRING'
          CHARACTER(  1 )     :: CHR
          CHARACTER( 96 )     :: XMSG

          INTEGER :: JP( MAX_LEN_WORD*SIZE( VAL_LIST ) )
          INTEGER :: KP( MAX_LEN_WORD*SIZE( VAL_LIST ) )
          INTEGER :: STATUS
          INTEGER :: IP, V
          INTEGER :: ICOUNT
 
          MAX_LEN = MAX_LEN_WORD * ( SIZE( VAL_LIST ) + 1 ) ! extra character allows deliminator
C Parse:

           NVARS = 0

C don't count until 1st char in string
           
           IP = 0
           KP = 1
           JP = 1
           LEN_EVAL = LEN_TRIM( ENV_VAL ) 
           IF ( LEN_EVAL .GT. MAX_LEN ) THEN
              XMSG = TRIM( PNAME ) // ': The Environment variable, '
     &            // TRIM( ENV_VAL ) // ',  has too long, greater than ' 
              WRITE(LOGDEV,'(A,I8)')TRIM( XMSG ), MAX_LEN
              XMSG = 'Above fatal error encountered '
              WRITE(LOGDEV,'(A)')TRIM( XMSG )
           END IF
101        LOOP_101: DO  ! read list
              IP = IP + 1
              IF ( IP .GT. LEN_EVAL ) EXIT LOOP_101
              CHR = ENV_VAL( IP:IP )
              IF ( CHR .EQ. ' ' .OR. ICHAR ( CHR ) .EQ. 09 ) CYCLE LOOP_101
              IF ( CHR .EQ. ',' .OR. CHR .EQ. ';' ) CYCLE LOOP_101
              IF( NVARS .GT. SIZE( VAL_LIST ) )THEN
                 XMSG = TRIM( PNAME ) // ':ERROR: Number of values in List, ' 
     &                //  TRIM( ENV_VAL ) 
     &                // ', greater than the size of its storage array, '
                  WRITE(LOGDEV,'(A,I4)')TRIM( XMSG ), SIZE( VAL_LIST )
                  XMSG = 'Above fatal error encountered '
                  WRITE(LOGDEV,'(A)')TRIM( XMSG )
              END IF
              NVARS = NVARS + 1
              JP( NVARS ) = IP   ! 1st char
              IF( IP .EQ. LEN_EVAL )THEN ! word one character long          
                  KP( NVARS ) = IP
                  V = 1
                  EXIT LOOP_101
              END IF     
201           LOOP_201: DO ! read word
                 IP = IP + 1
                 CHR = ENV_VAL( IP:IP )
                 IF ( CHR .NE. ' ' .AND.
     &                CHR .NE. ',' .AND.
     &                CHR .NE. ';' .OR.
     &                ICHAR ( CHR ) .EQ. 09 ) THEN  ! 09 = horizontal tab
                    CYCLE LOOP_201
                 ELSE                               ! last char in word
                    KP( NVARS ) = IP - 1 
                    V = JP( NVARS ) - IP
                    IF( V .GT. MAX_LEN_WORD )THEN
                      XMSG =  'The word, ' // ENV_VAL( JP(NVARS):KP(NVARS) ) 
     &                     // ', in list, ' // TRIM( ENV_VAL )
     &                     // ', is too long, '
                      WRITE(LOGDEV,'(A,1X,I2,A,I2)')TRIM( XMSG ), V, ' max allowed ',
     &                MAX_LEN_WORD
                      XMSG = 'Above fatal error encountered '
                      WRITE(LOGDEV,'(A)')TRIM( XMSG )
                    END IF
                    EXIT LOOP_201
                 END IF 
                 IF ( IP .GE. LEN_EVAL ) EXIT LOOP_101
              END DO LOOP_201
           END DO LOOP_101
           
           IF( NVARS .GT. SIZE( VAL_LIST ) )THEN
              XMSG = TRIM( PNAME ) // ':ERROR: Number of values in List, ' 
     &             //  TRIM( ENV_VAL ) // ', greater than '
              WRITE(LOGDEV,'(A,I4)')TRIM( XMSG ), SIZE( VAL_LIST )
              XMSG = 'Above fatal error encountered '
              WRITE(LOGDEV,'(A)')TRIM( XMSG )
           END IF
           
           ICOUNT = 0
           DO V = 1, NVARS
!              IF ( TRIM( ENV_VAL( JP( V ):KP( V ) ) ) .NE. ' ' .AND.
!     &                 TRIM( ENV_VAL( JP( V ):KP( V ) ) ) .NE. ',' .AND.
!     &                 TRIM( ENV_VAL( JP( V ):KP( V ) ) ) .NE. ';' .AND.
!     &                 ICHAR ( CHR ) .NE. 09 ) THEN
                  ICOUNT = ICOUNT + 1
                  VAL_LIST( ICOUNT ) = ENV_VAL( JP( V ):KP( V ) )
!               END IF
           END DO
           
           NVARS = ICOUNT

           RETURN
           
        END SUBROUTINE PARSE_STRING
! --------------------------------------------------------------------------------
        SUBROUTINE PARSE_COMMAS ( ENV_VAL, NVARS, VAL_LIST )

! takes a string of items delimited only by commas 
! and parse out the items into variables. Two data
! types: character strings and integers (still represented as strings in
! the env var vaules).
          
          IMPLICIT NONE

          CHARACTER( * ), INTENT ( IN )           :: ENV_VAL
          INTEGER,        INTENT ( OUT )          :: NVARS
          CHARACTER( * ), INTENT ( OUT )          :: VAL_LIST( : )

          INTEGER             :: MAX_LEN
          INTEGER             :: LEN_EVAL
          CHARACTER( 16 )     :: PNAME = 'PARSE_STRING'
          CHARACTER(  1 )     :: CHR
          CHARACTER( 96 )     :: XMSG

          INTEGER :: JP( MAX_LEN_WORD*SIZE( VAL_LIST ) )
          INTEGER :: KP( MAX_LEN_WORD*SIZE( VAL_LIST ) )
          INTEGER :: STATUS
          INTEGER :: IP, V
          INTEGER :: ICOUNT
          INTEGER :: LEN_LIST_WORD
 
          MAX_LEN = MAX_LEN_WORD * ( SIZE( VAL_LIST ) + 1 ) ! extra character allows deliminator
C Parse:

           NVARS = 0
           LEN_LIST_WORD = LEN( VAL_LIST( 1 ) )
C don't count until 1st char in string
           
           IP = 0
           KP = 1
           JP = 1
           LEN_EVAL = LEN_TRIM( ENV_VAL ) 
           IF ( LEN_EVAL .GT. MAX_LEN ) THEN
              XMSG = TRIM( PNAME ) // ': The Environment variable, '
     &            // TRIM( ENV_VAL ) // ',  has too long, greater than ' 
              WRITE(LOGDEV,'(A,I8)')TRIM( XMSG ), MAX_LEN
              XMSG = 'Above fatal error encountered '
              WRITE(LOGDEV,'(A)')TRIM( XMSG )
           END IF
101        LOOP_101: DO  ! read list
              IP = IP + 1
              IF ( IP .GT. LEN_EVAL ) EXIT LOOP_101
              CHR = ENV_VAL( IP:IP )
              IF ( CHR .EQ. ' ' .OR. ICHAR ( CHR ) .EQ. 09 ) CYCLE LOOP_101
              IF ( CHR .EQ. ',' ) CYCLE LOOP_101
              IF( NVARS .GT. SIZE( VAL_LIST ) )THEN
                 XMSG = TRIM( PNAME ) // ':ERROR: Number of values in List, ' 
     &                //  TRIM( ENV_VAL ) 
     &                // ', greater than the size of its storage array, '
                  WRITE(LOGDEV,'(A,I4)')TRIM( XMSG ), SIZE( VAL_LIST )
                  XMSG = 'Above fatal error encountered '
                  WRITE(LOGDEV,'(A)')TRIM( XMSG )
              END IF
              NVARS = NVARS + 1
              JP( NVARS ) = IP   ! 1st char
              IF( IP .EQ. LEN_EVAL )THEN ! word one character long          
                  KP( NVARS ) = IP
                  V = 1
                  EXIT LOOP_101
              END IF     
201           LOOP_201: DO ! read word
                 IP = IP + 1
                 CHR = ENV_VAL( IP:IP )
                 IF ( CHR .NE. ',' .AND. IP .LT. LEN_EVAL ) THEN  ! 09 = horizontal tab
                    CYCLE LOOP_201
                 ELSE 
                    IF ( IP .EQ. LEN_EVAL ) THEN 
                       KP( NVARS ) = IP        ! last char in word
                    ELSE 
                       KP( NVARS ) = IP - 1    ! last char in word
                    END IF
                    V = JP( NVARS ) - IP
                    IF( V .GT. MAX_LEN_WORD )THEN
                      XMSG =  'The word, ' // ENV_VAL( JP(NVARS):KP(NVARS) ) 
     &                     // ', in list, ' // TRIM( ENV_VAL )
     &                     // ', is too long, '
                      WRITE(LOGDEV,'(A,1X,I2,A,I2)')TRIM( XMSG ), V, ' max allowed ',
     &                MAX_LEN_WORD
                      XMSG = 'Above fatal error encountered '
                      WRITE(LOGDEV,'(A)')TRIM( XMSG )
                    END IF
                    EXIT LOOP_201
                 END IF 
                 IF ( IP .GE. LEN_EVAL ) EXIT LOOP_101
              END DO LOOP_201
           END DO LOOP_101
           
           IF( NVARS .GT. SIZE( VAL_LIST ) )THEN
              XMSG = TRIM( PNAME ) // ':ERROR: Number of values in List, ' 
     &             //  TRIM( ENV_VAL ) // ', greater than '
              WRITE(LOGDEV,'(A,I4)')TRIM( XMSG ), SIZE( VAL_LIST )
              XMSG = 'Above fatal error encountered '
              WRITE(LOGDEV,'(A)')TRIM( XMSG )
           END IF
           
           ICOUNT = 0
           DO V = 1, NVARS
!              IF ( TRIM( ENV_VAL( JP( V ):KP( V ) ) ) .NE. ' ' .AND.
!     &                 TRIM( ENV_VAL( JP( V ):KP( V ) ) ) .NE. ',' .AND.
!     &                 TRIM( ENV_VAL( JP( V ):KP( V ) ) ) .NE. ';' .AND.
!     &                 ICHAR ( CHR ) .NE. 09 ) THEN
                  ICOUNT = ICOUNT + 1
                  VAL_LIST( ICOUNT ) = ENV_VAL( JP( V ):KP( V ) )
!               END IF
           END DO
           
           NVARS = ICOUNT

           RETURN
           
        END SUBROUTINE PARSE_COMMAS
        INTEGER FUNCTION SIZE_TEXT ( LD, TR, CHAR )
        
C returns length of CHAR with leading blanks:   LD = 'LEADING', etc.
C returns length of CHAR with trailing blanks:  TR = 'TRAILING', etc.
C returns length of CHAR with leading and trailing blanks:  both
C returns length of CHAR minus leading and trailing blanks:  neither

           IMPLICIT NONE
           
           CHARACTER( * ), INTENT( IN ) :: LD
           CHARACTER( * ), INTENT( IN ) :: TR
           CHARACTER( * ), INTENT( IN ) :: CHAR
           
           INTEGER            :: START, FINI, INDX
           LOGICAL            :: NO_LDNG, NO_TRLNG
           CHARACTER(LEN(LD)) :: LEADING
           CHARACTER(LEN(TR)) :: TRAILING

           
           START = 1
           FINI = LEN( CHAR )
           NO_LDNG = .TRUE.
           NO_TRLNG = .TRUE.
           
           LEADING = ADJUSTL( LD )
           CALL UCASE( LEADING )
           TRAILING = ADJUSTL( TR )
           CALL UCASE( TRAILING )
           IF ( LEADING( 1:2 )  .EQ. 'LE' ) NO_LDNG = .FALSE.
           IF ( TRAILING( 1:2 ) .EQ. 'TR' ) NO_TRLNG = .FALSE.
           
           IF ( NO_TRLNG ) THEN
              DO INDX = FINI, START, -1
                 IF ( CHAR( INDX:INDX ) .NE. ' ' ) EXIT
              END DO
              FINI = INDX
           END IF
           IF ( NO_LDNG ) THEN
              DO INDX = 1, FINI
                 IF ( CHAR( INDX:INDX ) .NE. ' ' ) EXIT
              END DO
              START = INDX
           END IF
           SIZE_TEXT = FINI - START + 1
           
           RETURN
         END FUNCTION SIZE_TEXT
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         INTEGER FUNCTION INDEXES ( NAME, LENGTH, LIST )

C searchs array LIST to determine number of occurance for string NAME

           IMPLICIT NONE
           
           INTEGER,         INTENT( IN ) :: LENGTH
           CHARACTER*( * ), INTENT( IN ) :: NAME
           CHARACTER*( * ), INTENT( IN ) :: LIST( * )
           
           INTEGER :: INDX
           
           INDEXES = 0
           DO INDX = 1,LENGTH
              IF ( TRIM(NAME) .EQ. TRIM( LIST(INDX) ) ) THEN   ! NAME is in LIST
                 INDEXES = INDEXES + 1
              END IF
           END DO
           
           RETURN

         END FUNCTION INDEXES
       
       END MODULE TEXT_UTILITIES