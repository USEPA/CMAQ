
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


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/depv/m3dry/DEPV_DEFN.F,v 1.12 2012/01/19 14:22:45 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE RUNTIME_VARS_MOD

C-----------------------------------------------------------------------
C Function: Collection of run time variables for model options and outputs
C
C Revision History:
C   4 Dec 2017 J.Bash: initial implementation 
C-----------------------------------------------------------------------

      IMPLICIT NONE

      LOGICAL, SAVE :: ILDEPV    = .TRUE.  ! flag in-lining depv
      LOGICAL, SAVE :: SFC_HONO  = .FALSE. ! flag for HONO interaction with surfaces within in-lining depv
      LOGICAL, SAVE :: ABFLUX    = .FALSE. ! flag for ammonia bi-directional flux with in-lining depv
      LOGICAL, SAVE :: MOSAIC    = .FALSE. ! flag for mosaic - luc specific deposition - within in-lining depv
      LOGICAL, SAVE :: FST       = .FALSE. ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL, SAVE :: PX_LSM    = .TRUE.  ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL, SAVE :: CLM_LSM   = .FALSE. ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL, SAVE :: NOAH_LSM  = .TRUE.  ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv
      LOGICAL, SAVE :: DEPV_DIAG = .TRUE.  ! flag for mosaic - luc specific deposition - stomatal flux only - within in-lining depv

      CONTAINS                                 
      SUBROUTINE INIT_VARS( JDATE, JTIME, logdev )

      USE UTILIO_DEFN

         IMPLICIT NONE                         

      INTEGER, INTENT( In )  :: jdate       
      INTEGER, INTENT( In )  :: jtime  
      INTEGER, INTENT( In )  :: logdev  

      LOGICAL SUCCESS
      CHARACTER( 240 )       :: XMSG = ' '  
      CHARACTER(  16 ), SAVE :: PNAME = 'Init_Vars'

      CHARACTER( 16 )  :: CTM_ILDEPV = 'CTM_ILDEPV      ' ! environment variable for in-line DEPV
      CHARACTER( 16 )  :: CTM_ABFLUX = 'CTM_ABFLUX      ' ! environment variable for ammonia bi-directional flux
      CHARACTER( 16 )  :: CTM_SFC_HONO = 'CTM_SFC_HONO    ' ! environment variable for HONO interaction with surfaces
      CHARACTER( 16 )  :: CTM_MOSAIC = 'CTM_MOSAIC      '! environment variable for mosaic - luc specific deposition velocities
      CHARACTER( 16 )  :: CTM_FST = 'CTM_FST         '         ! environment variable for mosaic - luc specific stomatal flux
      CHARACTER( 16 )  :: PX_VERSION   = 'PX_VERSION      '     ! environment variable to sync meteorological dependent emissions and deposition to WRF LSM 
      CHARACTER( 16 )  :: CLM_VERSION  = 'CLM_VERSION     '     
      CHARACTER( 16 )  :: NOAH_VERSION = 'NOAH_VERSION     ' 
      CHARACTER( 16 )  :: CTM_DEPV_FILE = 'CTM_DEPV_FILE'! environment variable for diagnostic DEPV file

C variables for deposition velocities diagnostic file
      INTEGER          :: STATUS      ! ENV... status
      CHARACTER( 80 )  :: VARDESC     ! environment variable description

C In-line deposition velocities calculation?
         ILDEPV = .TRUE.
         VARDESC = 'Flag for in-line deposition velocities'
         ILDEPV = ENVYN( CTM_ILDEPV, VARDESC, ILDEPV, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         ELSE IF ( STATUS .EQ. -1 ) THEN
            XMSG = 'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            XMSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         END IF

         IF ( ILDEPV ) THEN

C Pleim`s ammonia bi-directional flux with in-line deposition velocities calculation?
            ABFLUX = .FALSE.
            VARDESC = 'Flag for ammonia bi-directional flux calculation'
            ABFLUX = ENVYN( CTM_ABFLUX, VARDESC, ABFLUX, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               XMSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               XMSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            END IF
C PX LSM?
            PX_LSM = .TRUE.
            VARDESC = 'Flag for WRF PX LSM'
            PX_LSM = ENVYN( PX_VERSION, VARDESC, PX_LSM, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               XMSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               XMSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            END IF

C CLM LSM?
            CLM_LSM = .FALSE.
            VARDESC = 'Flag for WRF CLM LSM'
            CLM_LSM = ENVYN( CLM_VERSION, VARDESC, CLM_LSM, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               XMSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               XMSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            END IF

C NOAH LSM?
            NOAH_LSM = .FALSE.
            VARDESC = 'Flag for WRF NOAH LSM'
            NOAH_LSM = ENVYN( NOAH_VERSION, VARDESC, NOAH_LSM, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               XMSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               XMSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            END IF

C Sarwar`s HONO interaction with leaf and building surfaces?
            SFC_HONO = .TRUE.
            VARDESC = 'Flag for HONO interaction with surfaces'
            SFC_HONO = ENVYN( CTM_SFC_HONO, VARDESC, SFC_HONO, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               XMSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               XMSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            END IF
            
C Mosaic method to get land-use specific deposition velocities?
            MOSAIC = .FALSE.
            VARDESC = 'Flag for land-use specific deposition velocity calculation'
            MOSAIC = ENVYN( CTM_MOSAIC, VARDESC, MOSAIC, STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               XMSG = 'Environment variable set, but empty ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               XMSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            END IF
         END IF   ! in-line depv
C Get diagnostic file flag.
         DEPV_DIAG = .FALSE.         ! default
         VARDESC = 'Flag for writing the DEPV diagnostic file'
         DEPV_DIAG = ENVYN( CTM_DEPV_FILE, VARDESC, DEPV_DIAG, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 0 ) THEN
            WRITE( LOGDEV,'( 5X, A )' ) VARDESC
            XMSG = TRIM( PNAME ) // ': writes GAS DEPV values to '
     &                           // TRIM( CTM_DEPV_FILE )
            WRITE( LOGDEV,'( 5X, A )' ) XMSG
         ELSE IF ( STATUS .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         ELSE IF ( STATUS .EQ. -1 ) THEN
            XMSG =
     &      'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV,'( 5X, A, I9)' ) XMSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            XMSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV,'( 5X, A, I9 )' ) XMSG, JTIME
         END IF
         RETURN
         END SUBROUTINE INIT_VARS
         END MODULE RUNTIME_VARS_MOD
