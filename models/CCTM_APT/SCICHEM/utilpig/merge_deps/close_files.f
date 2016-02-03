      SUBROUTINE CLOSE_FILES( DDEP_IN, WDEP_IN, DDEP_OUT, WDEP_OUT,
     &                        JDATE, JTIME )

!     USE HOST_INC
      USE M3UTILIO

      IMPLICIT NONE

      CHARACTER( 16 ) :: DDEP_IN, WDEP_IN, DDEP_OUT, WDEP_OUT

      INTEGER         JDATE     !  current model date, coded YYYYDDD
      INTEGER         JTIME     !  current model time, coded HHMMSS

      CHARACTER( 16 ) :: PNAME = 'CLOSE_FILES'
      CHARACTER( 120 ) :: XMSG = ' '

      write(6,*) 'Closing files'

      IF ( .NOT. CLOSE3( DDEP_IN ) ) THEN

         XMSG = 'Could not close ' // DDEP_IN // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      IF ( .NOT. CLOSE3( WDEP_IN ) ) THEN

         XMSG = 'Could not close ' // WDEP_IN // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      IF ( .NOT. CLOSE3( DDEP_OUT ) ) THEN

         XMSG = 'Could not close ' // DDEP_OUT // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      IF ( .NOT. CLOSE3( WDEP_OUT ) ) THEN

         XMSG = 'Could not close ' // WDEP_OUT // ' file'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

      END IF

      RETURN
      END
