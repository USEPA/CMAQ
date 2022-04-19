
      INTEGER FUNCTION  JUNIT()

C***********************************************************************
C  function body starts at line 60
C
C  FUNCTION:
C
C    Routine returns next available FORTRAN unit number
C
C  REVISION HISTORY:
C
C    3/88  Maximum number of I/O unit numbers was increased from 50
C          to 75 due to increased file I/O requirements.
C    5/88  Modified for ROMNET
C    7/90  Modified for ROM 2.2 -- uses EXWST for error abort.
C    8/90  Algorithm simplification:  replaced IF-GOTO loop by DO loop.
C    8/90  Algorithm simplification:  counting algorithm instead of table
C          of flags; uses POSIX standards-approved unit numbers 11-99
C    3/92  Models-3 Prototype version (eliminate EXWST)
C    8/96  Modified by CJC -- On counting-algorithm failure, performs
C          INQUIREs to find available unit.
C    2/97  conditional definition of EXIT under AIX
C
C  ARGUMENT LIST DESCRIPTION:  empty argument list
C
C  RETURN VALUE:  JUNIT        Unit number selected
C
C  LOCAL VARIABLE DESCRIPTION:
C
C    IUNIT    state variable:  counts through available units
C    BOT      parameter:  first POSIX-approved unit number for FORTRAN I/O
C    TOP      parameter:  last   ...
C
C***********************************************************************


      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

      INTEGER      BOT          !  1 less than initial unit number
      INTEGER      TOP          !  final unit number
      PARAMETER  ( BOT = 10, TOP = 99 )


C...........   SAVED LOCAL VARIABLES and their descriptions:

      INTEGER      IUNIT, J
      LOGICAL      FLAG
      DATA         IUNIT / TOP /      !  current highest unit number
      SAVE         IUNIT

C............................................................................
C.......   begin body of JUNIT:

      IF ( IUNIT .GE. BOT ) THEN
          JUNIT = IUNIT
          IUNIT = IUNIT - 1
      ELSE
          DO  11  J = BOT, TOP
              INQUIRE( UNIT=J, OPENED=FLAG )
              IF ( .NOT. FLAG ) THEN
                  JUNIT = J
                  RETURN
              END IF 
11        CONTINUE
  
C.........   If you get to here: failure

          WRITE (*,91001) BOT, TOP, IUNIT
          CALL EXIT( 2 )
      END IF
      RETURN


C*************************  FORMAT  STATEMENTS  **************************

C  Error and warning message formats     91xxx


91001 FORMAT (///, 1X, '*** ERROR ABORT IN ROUTINE JUNIT ***',
     &          /, 5X, 'NO MORE UNIT NUMBERS AVAILABLE FOR I/O',
     &          /, 5X, 'First POSIX-approved unit:', I4 ,
     &          /, 5X, 'Last  POSIX-approved unit:', I4 ,
     &          /, 5X, 'Current unit:             ', I4 ,
     &          //)

      END
