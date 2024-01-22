
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE WVEL_DEFN

C Function: capture the derived vertical velocity component to write to the
C           conc file
C
C Revision history:
C   1 Feb 19 David Wong: Implemented centralized I/O approach, removed
C                        all MY_N clauses
C   6 Sep 19 F. Sidi   : Removed USE STD_CONC and USE AVG_CONC statements
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE :: WVEL( :,:,: )
      REAL, ALLOCATABLE, SAVE :: WY( :,:,: ) ! Diagnosed vert vel ala yamo

      CONTAINS

         FUNCTION WVEL_INIT () RESULT ( SUCCESS )

         USE GRID_CONF             ! horizontal & vertical domain specifications
         USE UTILIO_DEFN

         LOGICAL SUCCESS

         CHARACTER( 16 ) :: PNAME = 'WVEL_INIT'
         CHARACTER( 16 ) :: CTM_WVEL = 'CTM_WVEL'
         CHARACTER( 96 ) :: XMSG = ' '

         INTEGER ALLOCSTAT, IERR
         INTEGER :: JDATE = 0, JTIME = 0

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         IF ( W_VEL ) THEN

            ALLOCATE ( WVEL( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'WVEL memory allocation failed'
               CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            WVEL = 0.0

         END IF

         END FUNCTION WVEL_INIT

C-----------------------------------------------------------------------

      SUBROUTINE GET_WVEL( JDATE, JTIME )

      USE GRID_CONF
      USE UTILIO_DEFN
      Use CENTRALIZED_IO_MODULE, only : MSFX2, JACOBF_AVAIL, interpolate_var

      include SUBST_FILES_ID

      INTEGER     JDATE         ! current model date, coded YYYYDDD
      INTEGER     JTIME         ! current model time, coded HHMMSS 
      
      INTEGER       MTIME, MDATE, LVL
      LOGICAL, SAVE :: FIRST_TIME  = .TRUE.

      INTEGER       ALLOCSTAT
      CHARACTER( 16 ) :: PNAME = 'GET_WVEL'
      CHARACTER( 200 ) :: XMSG
      REAL, ALLOCATABLE, SAVE :: DBUFF ( :,:,: )  ! input for W_VEL
      REAL, ALLOCATABLE, SAVE :: JACF  ( :,:,: )  ! full-layer Jacobian
                                         
      IF ( FIRST_TIME ) THEN
            FIRST_TIME = .FALSE.

            ALLOCATE ( JACF( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )

            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating JACF'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE( WY( NLAYS,NCOLS,NROWS ) )
            WY = 0.0
      END IF
 
#ifdef mpas
      WVEL = 0.0
#else
      IF (JACOBF_AVAIL) THEN 
         ! Time-Dependent Part of Vertical Velocity Retrieval
         call interpolate_var ('JACOBF', jdate, jtime, JACF)
      ELSE
         IF ( .NOT. ALLOCATED( DBUFF ) ) THEN
            ALLOCATE ( DBUFF( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating DBUFF'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF

         call interpolate_var ('JACOBM', jdate, jtime, DBUFF)

         JACF( :,:,1:NLAYS-1 ) = 0.5 * ( DBUFF( :,:,1:NLAYS-1 ) 
     &                              + DBUFF( :,:,2:NLAYS ) )
         JACF( :,:,NLAYS ) = 0.4 * JACF( :,:,NLAYS-1 )
     &                              + DBUFF( :,:,NLAYS )
      END IF

      ! convert from contravariant vertical velocity component to true wind
      DO LVL = 1, NLAYS
         WVEL( :,:,LVL ) = JACF( :,:,LVL )
     &                     * MSFX2( :,: ) * WY( LVL,:,: )
      END DO 
#endif

      END SUBROUTINE GET_WVEL    

      END MODULE WVEL_DEFN
