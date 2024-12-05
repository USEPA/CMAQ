
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
C  12 Mar 2019 D. Wong: Implemented centralized I/O approach and removed all MY_N clauses
C   1 Apr 2019 D. Wong: Remove call to SUBHFILE
C   March 2023 R.Mathur: removed sigma-coordinate dependence in determing layers
C               above which PV-scaling is applied
C-----------------------------------------------------------------------

      USE CGRID_SPCS          ! CGRID mechanism species
      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE UTILIO_DEFN
      USE CENTRALIZED_IO_MODULE, only : interpolate_var, lat

#ifdef isam
      USE SA_DEFN, ONLY: ISAM, N_SPCTAG, NTAG_SA, ITAG, OTHRTAG,
     &                   ISAM_PVO3_MAP, ISAM_SPEC, L_OZONE,
     &                   ISAMRGN_NUM, ISAMRGN_MAP, NSPC_SA
      USE desid_param_module, ONLY: DESID_REG_FAC, DESID_N_REG
#endif

#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX, SENGRID, IPT
#endif

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
      INTEGER       ALLOCSTAT

      INTEGER YEAR
      INTEGER JDAY
      REAL    CSTAR          ! dynamic PV coefficiency
      REAL    FC, GC         ! cstar = fc * gc
      REAL    LATABS         ! absolute latitude
      REAL    MFRC           ! month fraction
      REAL    PLAY           ! pressure for layer applied

      REAL, ALLOCATABLE, SAVE :: CSTARZ( :,: ) ! cstar at 58hPa estimated by polynomial fit
      REAL, ALLOCATABLE, SAVE :: CXX( :,: )    ! intermediate quadratic (< 0 for any lat.)
      REAL ::  PV( NCOLS,NROWS,NLAYS )   ! potential vorticity
      REAL PRES( NCOLS,NROWS,NLAYS )           ! Air pressure [ Pa ]

#ifdef isam
      INTEGER, SAVE           :: S_O3 = 0
      REAL                    :: PVO3_STORE = 1.0
      INTEGER                 :: RGN = 0
#endif 

C First time: set up parameters
      IF ( LFIRST ) THEN
         LFIRST = .FALSE.

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

C Scale top 3 layers O3 to PV
!        KPV = NLAYS - 2
C Scale only top layer O3 to PV
!        KPV = NLAYS
C        WRITE( LOGDEV,92002 ) KPV, X3FACE_GD( KPV )
         WRITE( LOGDEV,92003 ) PPVT

         ALLOCATE ( CSTARZ( NCOLS,NROWS ),
     &              CXX( NCOLS,NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating CSTARZ, or CXX'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

#ifdef isam
        IF ( L_OZONE ) S_O3  = INDEX1( 'O3', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )
#endif

      END IF      ! First time

C.. Get PV 
      call interpolate_var ('PV', jdate, jtime, pv)

C.. Get PRES
      call interpolate_var ('PRES', jdate, jtime, PRES)

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

      DO R = 1, NROWS
         DO C = 1, NCOLS
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

C     DO L = KPV, NLAYS
      DO L = 1, NLAYS
         DO R = 1, NROWS
            DO C = 1, NCOLS

             IF (PRES(C,R,L).LT.PPVT ) THEN ! scale O3 above PPVT

               FC     = CSTARZ( C,R ) + ( PRES( C,R,L ) - 5856.0 ) * CXX( C,R )

               CSTAR  = MAX ( 30.0, ABS( FC * GC ) )

               CGRID( C,R,L,VO3 ) = 0.001 * CSTAR * ABS( PV( C,R,L ) )
#ifdef isam
               IF ( L_OZONE ) THEN
                 ISAM( C,R,L,S_O3,: ) = 1E-30
                 PVO3_STORE = 1.0
                 IF ( SUM( ISAM_PVO3_MAP(:) ) .EQ. 0 ) THEN  ! no PV tracking specified, toss it into OTHER
                   ISAM( C,R,L,S_O3,OTHRTAG ) = CGRID( C,R,L,VO3 )
                 ELSE IF ( SUM( ISAM_PVO3_MAP(:) ) .GE. 1 ) THEN ! at least 1 PVO3 tag
                   DO ITAG = 1, NTAG_SA-3
                     IF( ISAM_PVO3_MAP(ITAG) .EQ. 1 ) THEN ! PV for this tag
                        IF ( ISAMRGN_NUM( ITAG ) .LT. 1 ) THEN ! full domain
                          ISAM( C,R,L,S_O3,ITAG ) = CGRID( C,R,L,VO3 )
                          PVO3_STORE = 0.0
                        ELSE ! split into regions and other
                          DO RGN = 1, ISAMRGN_NUM( ITAG )
                            ISAM( C,R,L,S_O3,ITAG ) = ISAM( C,R,L,S_O3,ITAG )
     &                            + CGRID( C,R,L,VO3 )
     &                            * DESID_REG_FAC(C,R,ISAMRGN_MAP(ITAG,RGN))
                            PVO3_STORE = PVO3_STORE
     &                            - DESID_REG_FAC(C,R,ISAMRGN_MAP(ITAG,RGN))
                          END DO
                        END IF 
                     END IF
                   END DO
                   IF ( PVO3_STORE .LT. 1E-6 .AND. PVO3_STORE .GT. 0.0 ) PVO3_STORE = 0.0 ! round off precision error
                   IF ( PVO3_STORE .GE. 0.0 ) THEN ! put the rest into OTHER tag
                     ISAM( C,R,L,S_O3,OTHRTAG ) = PVO3_STORE * CGRID( C,R,L,VO3 )
                   ELSE
                     XMSG = ' ERROR: Possible overspecification of ' //
     &                      'ISAM tags for PV'
                     CALL M3EXIT ( 'ISAM_PV', JDATE, JTIME, XMSG, XSTAT1 )
                   ENDIF
                 ELSE
                   XMSG = ' ERROR in ISAM PV definitions '
                   CALL M3EXIT ( 'ISAM_PV', JDATE, JTIME, XMSG, XSTAT1 )
                 END IF
               END IF
#endif

#ifdef sens
               DO NP = 1, NPMAX
                  IF ( IPT( NP ) .EQ. 6 ) THEN
                     SENGRID( C, R, L, NP, VO3 ) = CGRID( C, R, L, VO3 )
                  END IF
               END DO
#endif

               IF ( VO3T .GT. 0 ) THEN
                  CGRID( C,R,L,VO3T ) = CGRID( C,R,L,VO3 )
               END IF

             END IF ! check PPVT

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
92003 FORMAT( / 10X, 'PV Scaling applied above ', F10.2, 'Pa')

      END
