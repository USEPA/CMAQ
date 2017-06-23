      SUBROUTINE GET_TIMOUT( DATEOUT, TIMEOUT )

C*************************************************************************
C
C  FUNCTION: Loads arrays with 2 different date/time formats
C            IOAPI - YYYYDDD & HHMMSS
C            SAS   - DD-MMM-YYYY & HH:MM:SS
C             
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by Jerry Gipson, September, 2000
C                   
C*************************************************************************
      USE M3UTILIO
      USE ENV_VARS
      USE TIME_STEP

      IMPLICIT NONE     

C..ARGUMENTS:
      CHARACTER*11  DATEOUT( NFLSTEPS )
      CHARACTER*10  TIMEOUT( NFLSTEPS )

C..PARAMETERS: NONE

!C..EXTERNAL FUNCTIONS:
!      CHARACTER*10 HHMMSS         ! Convert M3 time to string HH:MM:SS
!
!      LOGICAL ISDSTIME            ! Daylight savings time check
 
C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*3   MONTH( 12 ) 

      CHARACTER*10   TMP_TIME      ! Temporary time
      CHARACTER*16    PNAME        ! Program Name

      INTEGER   JDATE          ! Time step date YYYYDDD
      INTEGER   JTIME          ! Time step time HHMMSS
      INTEGER   MDAY           ! day of month
      INTEGER   MNTH           ! Month 1-12
      INTEGER   N              ! Loop index
      INTEGER   YEAR           ! 4-digit year

      
  
C**********************************************************************
      DATA PNAME / 'GET_TIMOUT' /
      DATA MONTH / 'JAN' , 'FEB' , 'MAR' , 'APR' , 'MAY' , 'JUN' ,
     &             'JUL' , 'AUG' , 'SEP' , 'OCT' , 'NOV' , 'DEC' /

      DO N = 1, NFLSTEPS

         JDATE = STEP_DATE( N )
         JTIME = STEP_TIME( N )

         IF( TIME_CONV .EQ. 'EST' ) CALL NEXTIME( JDATE, JTIME, -050000 )
        
         IF( OUTFORMAT( 1 : 3 ) .NE. 'SAS' ) THEN
        
            WRITE( DATEOUT( N ), 92000 ) JDATE
            WRITE( TIMEOUT( N ), 92020 ) JTIME

         ELSE

            CALL DAYMON( JDATE, MNTH, MDAY )

            YEAR = JDATE / 1000

            WRITE( DATEOUT( N ), 92040 ) MDAY, MONTH( MNTH ), YEAR

            TIMEOUT( N ) = HHMMSS( JTIME )

            IF( TIMEOUT( N ) ( 2 : 2 ) .EQ. ':' ) THEN
               TMP_TIME = '0' // TIMEOUT( N ) ( 1 : 7 )
               TIMEOUT( N ) = TMP_TIME
            ENDIF

         ENDIF


      ENDDO

      RETURN

92000 FORMAT( I7, '    ' )
92020 FORMAT( I6.6, '    ' ) 
92040 FORMAT( I2.2, '-', A3, '-', I4 )

      END


