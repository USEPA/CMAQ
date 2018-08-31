
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
       SUBROUTINE PVO3( CGRID, JDATE, JTIME )

C-----------------------------------------------------------------------
C  Function: Scales O3 in free-troposphere to potential vorticity 
 
C  Revision History:
C  Prototype, Rohit Mathur, Nov. 2007
C             Scaling only at top layer; scaling factor based on 2006 IONS O3 and 
C             PV estimated from NAM at ~100mb,  Rohit Mathur, Nov. 2008
C             Incorporation in CMAQv4.7.1,  Rohit Mathur, Jan.2010
C-----------------------------------------------------------------------

      USE CGRID_SPCS          ! CGRID mechanism species
      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE UTILIO_DEFN

      IMPLICIT NONE

C..Includes:
      INCLUDE SUBST_FILES_ID  ! CMAQ files
      INCLUDE SUBST_CONST     ! CMAQ constants

C..Parameters:
      REAL, PARAMETER :: PSFC = 100000.0  ! generic surface pres. [Pa]

C To scale O3 with PV at specified altitudes, set highest pressure level to exclude
C PV scaling:
      REAL, PARAMETER :: PPVT = 11000.0   ! [Pa] (~14-16km or X3 > 0.93)

      REAL, PARAMETER :: AX = 203.53
      REAL, PARAMETER :: BX = -13.622
      REAL, PARAMETER :: CX =  5.4157E-1
      REAL, PARAMETER :: DX = -9.4264E-3
      REAL, PARAMETER :: EX =  7.299E-5
      REAL, PARAMETER :: FX = -2.0214E-7

      REAL, PARAMETER :: AY = -2.1902E-2
      REAL, PARAMETER :: BY =  4.5507E-4
      REAL, PARAMETER :: CY = -2.4557E-6

C..Arguments:
      REAL, POINTER :: CGRID( :,:,:,: )   ! Species concentrations
      INTEGER, INTENT( IN ) :: JDATE      ! Current date (YYYYDDD)
      INTEGER, INTENT( IN ) :: JTIME      ! Current time (HHMMSS)

C..Saved Local Variables:
      CHARACTER( 16 ), SAVE :: PNAME = 'PVO3'     ! Program name
      LOGICAL, SAVE :: LFIRST = .TRUE.  ! Flag for first call to this subroutine
      INTEGER, SAVE :: VO3       ! ozone
      INTEGER, SAVE :: VO3T = 0  ! tracer
      INTEGER, SAVE :: KPV

C..Scratch Local Variables:
      CHARACTER( 132 ) :: MSG       ! Message text
      CHARACTER(  16 ) :: VNAME     ! Name of I/O API data variable
      CHARACTER( 120 ) :: XMSG  = ' '    ! Exit status message

      INTEGER C, L, R        ! Loop indices
      INTEGER GXOFF, GYOFF   ! global origin offset from file
      INTEGER       :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
      INTEGER       ALLOCSTAT

      INTEGER YEAR
      INTEGER JDAY
      REAL    CSTAR          ! dynamic PV coefficiency
      REAL    FC, GC         ! cstar = fc * gc
      REAL    LATABS         ! absolute latitude
      REAL    MFRC           ! month fraction
      REAL    PLAY           ! pressure for layer applied

      REAL, ALLOCATABLE, SAVE :: LAT( :,: )    ! grid lat (deg) -90<= LAT <= 90
      REAL, ALLOCATABLE, SAVE :: CSTARZ( :,: ) ! cstar at 58hPa estimated by polynomial fit
      REAL, ALLOCATABLE, SAVE :: CXX( :,: )    ! intermediate quadratic (< 0 for any lat.)
      REAL PV( NCOLS,NROWS,NLAYS )             ! potential vorticity
      REAL PRES( NCOLS,NROWS,NLAYS )           ! Air pressure [ Pa ]

C First time: set up parameters
      IF ( LFIRST ) THEN
         LFIRST = .FALSE.

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

         VO3 = INDEX1( 'O3', N_GC_SPC, GC_SPC )
         WRITE( LOGDEV,92000 ) N_GC_SPC, VO3

C Get number of species, and starting indices for CGRID array.
         VO3T = INDEX1( 'O3PV', N_TR_SPC, TR_SPC )
         IF ( VO3T .GT. 0 ) then
            XMSG = '     Option used: a tracer Namelist file with species O3PV'
            CALL M3MESG ( XMSG )
            VO3T = TR_STRT - 1 + VO3T
            WRITE( LOGDEV,92001 ) TR_STRT-1, N_TR_SPC, VO3T
         END IF

C Determine first layer above PPVT to scale O3 to PV
         DO L = 0, NLAYS
            PLAY = PSFC - ( PSFC - VGTOP_GD ) * X3FACE_GD( L )
            IF ( PLAY .LT. PPVT ) THEN
               KPV = L; EXIT
            END IF
         END DO
C Scale top 3 layers O3 to PV
!        KPV = NLAYS - 2
C Scale only top layer O3 to PV
!        KPV = NLAYS
         WRITE( LOGDEV,92002 ) KPV, X3FACE_GD( KPV )

         CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )

         ALLOCATE ( LAT( NCOLS,NROWS ),
     &              CSTARZ( NCOLS,NROWS ),
     &              CXX( NCOLS,NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating LAT, CSTARZ, or CXX'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'LAT'
         IF ( .NOT. INTERPX( GRID_CRO_2D, VNAME, PNAME,
     &                       STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2,
     &                       1, 1, 0, 0, LAT ) ) THEN
            XMSG = 'Could not read LAT from GRID_CRO_2D'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      END IF      ! First time

C.. Get PV 
      VNAME = 'PV'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    JDATE, JTIME, PV ) ) THEN
         MSG = 'Could not read PV from MET_CRO_3D'
         CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      END IF

C.. Get PRES
      VNAME = 'PRES'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    JDATE, JTIME, PRES ) ) THEN
         MSG = 'Could not read PV from MET_CRO_3D'
         CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )

      END IF

C.. Calculate month fraction
      YEAR = INT( FLOAT( JDATE ) / 1000.0 )
      JDAY = JDATE - YEAR * 1000

      IF ( MOD( YEAR, 4 ) .EQ. 0 ) THEN
         MFRC = FLOAT( JDAY ) / 366.0
      ELSE
         MFRC = FLOAT( JDAY ) / 365.0
      END IF

      GC = 1.0 + 0.22 * SIN( PI180 * 12.0 * ( MFRC * 30.0 + 2.0 ) )

C.. Scale O3 in free-trop based on PV
C   O3 in ppb = 30*PV or O3 in ppm = .03*PV
C   This constant of proportionality is determined based on examining the relationship
C   between NAM PV and average observed O3 at the 2006 IONS sites for the topmost layer
C   of a 22-layer configuration

      DO R = 1, MY_NROWS
         DO C = 1, MY_NCOLS
            LATABS = ABS( LAT( C,R ) )
            CSTARZ( C,R ) =            AX
     &                    + LATABS * ( BX
     &                    + LATABS * ( CX
     &                    + LATABS * ( DX
     &                    + LATABS * ( EX
     &                    + LATABS * ( FX ) ) ) ) )

            CXX( C,R )    =            AY
     &                    + LATABS * ( BY
     &                    + LATABS * ( CY ) )
         END DO
      END DO

      DO L = KPV, NLAYS
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS

               FC     = CSTARZ( C,R ) + ( PRES( C,R,L ) - 5856.0 ) * CXX( C,R )

               CSTAR  = MAX ( 30.0, ABS( FC * GC ) )

               CGRID( C,R,L,VO3 ) = 0.001 * CSTAR * ABS( PV( C,R,L ) )
               IF ( VO3T .GT. 0 ) THEN
                  CGRID( C,R,L,VO3T ) = CGRID( C,R,L,VO3 )
               END IF
            END DO
         END DO
      END DO
               
      RETURN

92000 FORMAT( / 10X, 'In Subroutine PVO3: setting index for O3 '
     &        / 10X, 'Number of gas phase species:  ', I4
     &        / 10X, 'Species index for O3:         ', I4 )
92001 FORMAT( / 10X, 'In Subroutine PVO3: setting index for O3 '
     &        / 10X, 'CGRID offset for tracer species: ', I4
     &        / 10X, 'Number of tracer phase species:  ', I4
     &        / 10X, 'Species index for O3PV tracer:         ', I4 )
92002 FORMAT( / 10X, 'PV Scaling at Layers Starting from: ', I4
     &        / 10X, 'X3 coordinate value at starting layer: ', F10.7 / )

      END
