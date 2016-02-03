      SUBROUTINE CLOSE_FILES( CONC_IN, CONC_OUT, JDATE, JTIME )

!     USE HOST_INC
      USE M3UTILIO

      IMPLICIT NONE

      CHARACTER( 16 ) :: CONC_IN
      CHARACTER( 16 ) :: CONC_OUT

      INTEGER         JDATE     !  current model date, coded YYYYDDD
      INTEGER         JTIME     !  current model time, coded HHMMSS

      CHARACTER( 16 ) :: PNAME = 'CLOSE_FILES'
      CHARACTER( 120 ) :: XMSG = ' '

      write(6,*) 'Closing files'

      IF ( .NOT. CLOSE3( CONC_IN ) ) THEN

         XMSG = 'Could not close ' // CONC_IN // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      IF ( .NOT. CLOSE3( CONC_OUT ) ) THEN

         XMSG = 'Could not close ' // CONC_OUT // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      RETURN
      END
