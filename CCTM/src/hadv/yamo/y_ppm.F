
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
      SUBROUTINE Y_PPM ( CGRID, FDATE, FTIME, TSTEP, LVL, BCON )

C-----------------------------------------------------------------------
C Function:
C   Piecewise Parabolic Method advection in the Y-direction

C Preconditions:

C Subroutines and functions called:

C Revision history:
C  28 Jun 2004: Jeff Young

C   1 Nov 06: Jeff Young - Following Glenn Hammond, moved all communication
C   out of HPPM to this level; using "swap_sandia" communication; update only
C   local values in the CGRID array within a time step, discarding previous
C   ghost values.
C   11 May 2009: Jeff Young: Simplify - assume constant cell widths, DS( i )
C   11 May 2010: Jeff Young: New hppm fix for PGI compiler by David Wong
C   21 Jun 2010: Jeff Young: convert for Namelist redesign
C   16 Feb 2011: Shawn Roselle: replaced I/O API include files with UTILIO_DEFN
C   19 Oct 2015: Jeff Young: Remove stmnt func to resolve PGI compiler error
C   29 Nov 17 David Wong: removed all SWAP routines and replaced with SE_COMM
C-----------------------------------------------------------------------

      USE HGRD_DEFN             ! horizontal domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_COMM_MODULE, SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_COMM_MODULE, NOOP_UTIL_MODULE)
#endif

      IMPLICIT NONE

C Includes:

      INCLUDE SUBST_CONST       ! constants
      INCLUDE SUBST_PE_COMM     ! PE communication displacement and direction

C Arguments:

      REAL, POINTER :: CGRID( :,:,:,: )
      INTEGER, INTENT( IN ) :: FDATE         ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: FTIME         ! current model time, coded HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP         ! time step (HHMMSS)
      INTEGER, INTENT( IN ) :: LVL           ! layer
!     REAL,    INTENT( IN ) :: BCON( NBNDY,1: ) ! boundary concentrations
      REAL,    INTENT( IN ) :: BCON( :,: )      ! boundary concentrations

C External Functions:

      REAL,    EXTERNAL :: ZFDBC

C Parameters:

C Advected species dimension

      INTEGER, SAVE :: N_SPC_ADV

! #ifdef parallel
      INTEGER, PARAMETER :: SWP = 3
! #else
!     INTEGER, PARAMETER :: SWP = 1
! #endif

C File Variables:

      REAL         VHAT( NCOLS+1,NROWS+1 )       ! x1-component CX-velocity

C Local Variables:

      CHARACTER( 16 ) :: PNAME = 'Y_PPM'
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      CHARACTER( 96 ) :: XMSG = ' '


      REAL,    SAVE :: DX2                      ! dx2 (meters)
      INTEGER, SAVE :: ASPC                     ! RHOJ index in CGRID

      REAL, ALLOCATABLE, SAVE :: VELY( : ),     ! Velocities along a column
     &                           CONY( :,: )    ! Conc's along a column

      REAL          DT                          ! TSTEP in sec
      INTEGER       ALLOCSTAT

      INTEGER, ALLOCATABLE, SAVE :: ADV_MAP( : )  ! global adv map to CGRID

      CHARACTER( 16 ) :: X2VEL = 'X2VEL'

      INTEGER      COL, ROW, SPC, VAR           ! loop counters
      INTEGER      A2C

      INTEGER MY_TEMP
!     INTEGER, SAVE :: STARTROW, ENDROW
      LOGICAL, SAVE :: BNDY_PE_LOY, BNDY_PE_HIY

#ifdef parallel
      INTEGER NORTH_ROW
      REAL, ALLOCATABLE, SAVE :: HALO_NORTH( :,:,: )
      REAL, ALLOCATABLE, SAVE :: HALO_SOUTH( :,:,: )
      REAL, ALLOCATABLE, SAVE :: BUF_NS( :,:,: )
#endif

      INTEGER, SAVE :: SFX    ! fixed parameter for southern boundary
      INTEGER, SAVE :: NFX    ! fixed parameter for northern boundary
C Required interface for allocatable array dummy arguments

      INTERFACE
         SUBROUTINE HCONTVEL ( FDATE, FTIME, TSTEP, LVL, UORV, UHAT )
            INTEGER,         INTENT( IN )     :: FDATE, FTIME, TSTEP, LVL
            CHARACTER( 16 ), INTENT( IN )     :: UORV
            REAL,            INTENT( OUT )    :: UHAT( :,: )
         END SUBROUTINE HCONTVEL
         SUBROUTINE HPPM ( NI, NJ, CON, VEL, DT, DS, ORI )
! #ifdef parallel
            INTEGER, PARAMETER                :: SWP = 3
! #else
!           INTEGER, PARAMETER                :: SWP = 1
! #endif
            INTEGER,         INTENT( IN )     :: NI, NJ
            REAL,            INTENT( IN OUT ) :: CON( 1-SWP:,1: )
            REAL,            INTENT( IN )     :: VEL( : )
            REAL,            INTENT( IN )     :: DT
            REAL,            INTENT( IN )     :: DS
            CHARACTER,       INTENT( IN )     :: ORI
         END SUBROUTINE HPPM
      END INTERFACE
C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         SFX = 0
         NFX = MY_NCOLS + MY_NROWS + 3

C Get dx2 from HGRD_DEFN module

         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX2 = DG2M * YCELL_GD   ! in m.
         ELSE
            DX2 = YCELL_GD          ! in m.
         END IF

         N_SPC_ADV = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV + 1
                                                  ! add 1 for advecting RHOJ

         ALLOCATE ( CONY( 1-SWP:MY_NROWS+SWP,N_SPC_ADV ),
     &              VELY( MY_NROWS+1 ), STAT = ALLOCSTAT ) ! Vel along a col
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating VELY, or CONY'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( ADV_MAP( N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating ADV_MAP'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF

#ifdef parallel
         ALLOCATE ( HALO_NORTH( MY_NCOLS,SWP,N_SPC_ADV ),
     &              HALO_SOUTH( MY_NCOLS,SWP,N_SPC_ADV ),
     &              BUF_NS    ( MY_NCOLS,SWP,N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating HALO_NORTH, HALO_SOUTH, or BUF_NS'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF
         HALO_NORTH = 0.0   ! array
         HALO_SOUTH = 0.0   ! array
         BUF_NS     = 0.0   ! array
#endif

         ASPC = GC_STRT - 1 + N_GC_SPCD

C Create global map to CGRID

         SPC = 0
         DO VAR = 1, N_GC_TRNS
            SPC = SPC + 1
            ADV_MAP( SPC ) = GC_STRT - 1 + GC_TRNS_MAP( VAR )
         END DO
         DO VAR = 1, N_AE_TRNS
            SPC = SPC + 1
            ADV_MAP( SPC ) = AE_STRT - 1 + AE_TRNS_MAP( VAR )
         END DO
         DO VAR = 1, N_NR_TRNS
            SPC = SPC + 1
            ADV_MAP( SPC ) = NR_STRT - 1 + NR_TRNS_MAP( VAR )
         END DO
         DO VAR = 1, N_TR_ADV
            SPC = SPC + 1
            ADV_MAP( SPC ) = TR_STRT - 1 + TR_ADV_MAP( VAR )
         END DO

         ADV_MAP( N_SPC_ADV ) = ASPC

!        CALL SUBST_LOOP_INDEX ( 'R', 1, MY_NROWS, 1, MY_TEMP,
!    &                           STARTROW, ENDROW )

         CALL SUBST_HI_LO_BND_PE ( 'R', BNDY_PE_LOY, BNDY_PE_HIY )

         END IF                    ! if firstime

      DT = FLOAT ( TIME2SEC ( TSTEP ) )

C Do the computation for y advection

C Get the contravariant x2 velocity component

      CALL HCONTVEL ( FDATE, FTIME, TSTEP, LVL, X2VEL, VHAT )

      CALL SUBST_COMM (VHAT, DSPL_N1_E0_S0_W0, DRCN_N)

#ifdef parallel
      NORTH_ROW = MY_NROWS - SWP
      DO SPC = 1, N_SPC_ADV
         A2C = ADV_MAP( SPC )
         DO ROW = 1, SWP
            DO COL = 1, MY_NCOLS
               HALO_SOUTH( COL,ROW,SPC ) = CGRID( COL,ROW,LVL,A2C )
               HALO_NORTH( COL,ROW,SPC ) = CGRID( COL,NORTH_ROW+ROW,LVL,A2C )
!              M = M + 1
               BUF_NS( COL,ROW,SPC ) = HALO_NORTH( COL,ROW,SPC )
            END DO
         END DO
      END DO

      CALL SUBST_COMM (HALO_SOUTH, HALO_NORTH, DSPL_N1_E0_S0_W0, DRCN_N)
      CALL SUBST_COMM (BUF_NS,     HALO_SOUTH, DSPL_N0_E0_S1_W0, DRCN_S)
#endif

      DO 233 COL = 1, MY_NCOLS

!        DO ROW = STARTROW, ENDROW         !    DO ROW = 1, MY_NROWS+1
         DO ROW = 1, MY_NROWS+1
            VELY( ROW ) = VHAT( COL,ROW )
         END DO

         DO SPC = 1, N_SPC_ADV

            A2C = ADV_MAP( SPC )
            DO ROW = 1, MY_NROWS
               CONY( ROW,SPC ) = CGRID( COL,ROW,LVL,A2C )
            END DO

#ifdef parallel
            DO ROW = 1, SWP
               CONY( ROW-SWP,SPC )      = HALO_SOUTH( COL,ROW,SPC )
               CONY( MY_NROWS+ROW,SPC ) = HALO_NORTH( COL,ROW,SPC )
            END DO
#endif

C South boundary

            IF ( BNDY_PE_LOY ) THEN
               IF ( VELY( 1 ) .LT. 0.0 ) THEN          ! outflow
                  CONY( 1-SWP:0,SPC) =
     &               ZFDBC ( CONY( 1,SPC ), CONY( 2,SPC ),
     &                       VELY( 1 ),     VELY( 2 ) )
               ELSE    ! inflow
                  CONY( 1-SWP:0,SPC ) = BCON( SFX+COL,SPC )
               END IF
            END IF

C North boundary

            IF ( BNDY_PE_HIY ) THEN
               IF ( VELY( MY_NROWS+1 ) .GT. 0.0 ) THEN     ! outflow
                  CONY( MY_NROWS+1:MY_NROWS+SWP,SPC ) =
     &               ZFDBC ( CONY( MY_NROWS,SPC ), CONY( MY_NROWS-1,SPC ),
     &                       VELY( MY_NROWS+1 ),   VELY( MY_NROWS ) )
               ELSE    ! inflow
                  CONY( MY_NROWS+1:MY_NROWS+SWP,SPC ) = BCON( NFX+COL,SPC )
               END IF
            END IF

         END DO

C PPM scheme

         CALL HPPM ( MY_NROWS, MY_NCOLS, CONY, VELY, DT, DX2, 'R' )

         DO SPC = 1, N_SPC_ADV
            A2C = ADV_MAP( SPC )
            DO ROW = 1, MY_NROWS
               CGRID( COL,ROW,LVL,A2C ) = CONY( ROW,SPC )
            END DO
         END DO

!019  format( 'y_ppm# date, time, col, row, lvl, cony: ',
!    &         2I7, 3I4, 1pe12.3 )
2019  format( 'y_ppm# time, c, r, l, s, cony: ',
     &         I7.6, 4I4, 1pe12.3 )

233   CONTINUE

      RETURN
      END
