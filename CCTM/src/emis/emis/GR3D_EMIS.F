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

      SUBROUTINE GR3D ( JDATE, JTIME, TSTEP, DESID_FNAME, STREAM_LAYS, 
     &                  FSTREAM, VDEMIS, L_DESID_DIAG )

      USE RUNTIME_VARS
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN
      USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
      USE CENTRALIZED_IO_MODULE, ONLY: interpolate_var
      USE DESID_VARS
      USE GRID_CONF           ! horizontal & vertical domain specifications      

      !USE EMIS_UTIL
      !USE UTILIO_DEFN

      IMPLICIT NONE

      ! Includes:
      INCLUDE SUBST_FILES_ID  ! file name parameters

      ! Arguments:
      INTEGER, INTENT( IN ) :: JDATE, JTIME         ! date (YYYYDDD), time (HHMMSS)
      INTEGER, INTENT( IN ) :: FSTREAM              ! Stream Counter
      CHARACTER( 100 ), INTENT( IN ) :: DESID_FNAME ! Stream Filename
      INTEGER, INTENT( IN ) :: TSTEP( 3 )           ! time step vector (HHMMSS)
      LOGICAL, INTENT( IN ) :: L_DESID_DIAG         ! Flag for diagnostic operation

      ! Output:
      REAL, INTENT( OUT )  :: VDEMIS( :,:,:,: )     ! Emission Rate Array

      ! Local Variables:
      INTEGER   C, R, L, N, S, V, ISTR 
      INTEGER   S_STRT, S_END, STREAM_LAYS, NDATE, NTIME

      REAL, ALLOCATABLE, SAVE :: BUFF( :,:,: )
      CHARACTER( 16 ) :: VNAME
      CHARACTER( 16 ) :: PNAME = 'GRIDEMIS'
      CHARACTER( 200 ) :: XMSG = ' '
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, ALLOCATABLE, SAVE :: LOC_STDATE(:)
      LOGICAL         :: L_NEWDAY

!-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         ! Get Domain Information for Emissions Read Routine

         ! Allocate Persistent Variables
         ALLOCATE( BUFF( NCOLS,NROWS,MAXVAL( DESID_GRID_LAYS ) ) )
         ALLOCATE( LOC_STDATE( SIZE(DESID_STREAM_DATE) ) )

         LOC_STDATE = STDATE
      END IF    !FirstTime
 
      ! Ensure that the model and emissions timestamp dates stay synchronized
      L_NEWDAY = .FALSE.
      IF (LOC_STDATE(FSTREAM) .NE. JDATE ) THEN 
         NDATE = JDATE; NTIME = JTIME
         CALL NEXTIME( NDATE, NTIME, -TSTEP( 1 ) )       ! go back one output tstep
         CALL NEXTIME( DESID_STREAM_DATE( FSTREAM ), NTIME, TSTEP( 1 ) ) ! advance the start date one time step
         LOC_STDATE(FSTREAM) = DESID_STREAM_DATE( FSTREAM )
         L_NEWDAY = .TRUE.
      END IF

      ! Read & Interpolate Emissions 
      VDEMIS = 0.0

      DO ISTR = 1, N_EMIS_ISTR 
        VNAME = DESID_EMVAR( ISTR, FSTREAM )
        IF ( VNAME .EQ. '' ) CYCLE

        CALL INTERPOLATE_VAR (VNAME, DESID_STREAM_DATE( FSTREAM ), JTIME, BUFF, DESID_FNAME)

        ! Store all emissions in mol/sec or g/sec and convert to ppmv/s later
        IF ( DESID_LAYS .GE. STREAM_LAYS ) THEN 
          DO L = 1, STREAM_LAYS
             VDEMIS( ISTR,L,:,: ) = BUFF( :,:,L )
          END DO
        ELSE
          DO L = 1, DESID_LAYS
             VDEMIS( ISTR,L,:,: ) = BUFF( :,:,L )
          END DO
          DO L = DESID_LAYS+1,STREAM_LAYS
             VDEMIS( ISTR,DESID_LAYS,:,: ) = VDEMIS( ISTR,DESID_LAYS,:,: ) + BUFF( :,:,L )
          END DO
        END IF

      END DO   ! ISTR

      ! Reset the date of the gridded file if this is diagnostic mode
      ! and the day has advanced
      IF ( L_NEWDAY .AND. L_DESID_DIAG ) THEN
         CALL NEXTIME( DESID_STREAM_DATE( FSTREAM ), NTIME, -TSTEP(1) )
         LOC_STDATE(FSTREAM) = DESID_STREAM_DATE( FSTREAM )
      END IF

      RETURN

      END SUBROUTINE GR3D

