
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

      SUBROUTINE PA_DATAGEN( )
 
C----------------------------------------------------------------------
C Function: Create the Process Analysis control data
 
C Preconditions: None
 
C Key Subroutines/Functions Called:
 
C  Revision History:
C  27 Jul 16 J.Young: inline PA ctl program
C----------------------------------------------------------------------

      USE UTILIO_DEFN
      USE PA_VARS, Only: NFAMLYS, NRXSUMS, NCYCLES, NIRROUT
      USE PA_DEFN            ! Process Anaylsis control and data variables

      IMPLICIT NONE 

C..Includes: None
C..Arguments: None
C..Parameters: None
C..External Functions: None
C..Local Variables: None
      CHARACTER(  16 ) :: PNAME      = 'PA_DATAGEN'
      
      CHARACTER(  16 ) :: CTM_PROCAN = 'CTM_PROCAN'
      CHARACTER(  80 ) :: VARDESC                   ! environment variable description
      CHARACTER( 240 ) :: XMSG = ' '

      INTEGER          :: JDATE, JTIME
      INTEGER          :: ESTAT         ! status from environment var check
      INTEGER          :: LOGDEV
        
      LOGICAL          :: PROCAN        ! flag for process analysis

C----------------------------------------------------------------------

      LOGDEV   = INIT3()

!...Get flag to use process analysis

      JDATE = 0; JTIME = 0
      PROCAN = .FALSE.         ! default
      VARDESC= 'Flag for using process analysis'
      PROCAN = ENVYN( CTM_PROCAN, VARDESC, PROCAN, ESTAT )
      IF ( ESTAT .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
      IF ( ESTAT .EQ. 1 ) THEN
         XMSG = 'Environment variable improperly formatted'
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      ELSE IF ( ESTAT .EQ. -1 ) THEN
         XMSG = 'Environment variable set, but empty ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
      ELSE IF ( ESTAT .EQ. -2 ) THEN
         XMSG = 'Environment variable not set ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
      END IF

      IF ( PROCAN ) THEN
         XMSG = TRIM( PNAME ) // ': Process Analysis is activated'
         CALL M3MESG( XMSG )
      ELSE
         XMSG = TRIM( PNAME ) // ': Process Analysis is not activated'
         CALL M3MESG( XMSG )
         RETURN
      END IF
        
      CALL PA_READ
      CALL PA_ERRCHECK
      CALL PA_SETUP_IPR
      CALL PA_SETUP_IRR
      CALL PA_COMPMECH
!     CALL PA_WRTPADEFN
      CALL PA_REPORT
      CALL M3MESG( 'End of initial Process Analysis Control data generation' )
      CALL M3MESG( ' ' )

      NFAMLYS = 0
      NRXSUMS = 0
      NCYCLES = 0
      NIRROUT = 0

      RETURN
      END SUBROUTINE PA_DATAGEN
