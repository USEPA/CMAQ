
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
      SUBROUTINE X_PPM ( CGRID, FDATE, FTIME, TSTEP, LVL, BCON )

C-----------------------------------------------------------------------
C Function:
C   Piecewise Parabolic Method advection in the X-direction

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
      USE CGRID_SPCS            ! CGRID mechanism species
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

      REAL,    POINTER      :: CGRID( :,:,:,: )
      INTEGER, INTENT( IN ) :: FDATE         ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: FTIME         ! current model time, coded HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP         ! time step (HHMMSS)
      INTEGER, INTENT( IN ) :: LVL           ! layer
!     REAL,    INTENT( IN ) :: BCON( NBNDY,1: ) ! boundary concentrations
      REAL,    INTENT( IN ) :: BCON( :,: )      ! boundary concentrations

C External Functions not declared in IODECL3.EXT:

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

      REAL         UHAT( NCOLS+1,NROWS+1 )       ! x1-component CX-velocity

C Local Variables:

      CHARACTER( 16 ) :: PNAME = 'X_PPM'
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      CHARACTER( 96 ) :: XMSG = ' '

      REAL,    SAVE :: DX1                      ! dx1 (meters)
      INTEGER, SAVE :: ASPC                     ! RHOJ index in CGRID

      REAL, ALLOCATABLE, SAVE :: VELX( : ),     ! Velocities along a row
     &                           CONX( :,: )    ! Conc's along a row

      REAL          DT                          ! TSTEP in sec
      INTEGER       ALLOCSTAT

      INTEGER, ALLOCATABLE, SAVE :: ADV_MAP( : ) ! global adv map to CGRID

      CHARACTER( 16 ) :: X1VEL = 'X1VEL'

      INTEGER      COL, ROW, SPC, VAR           ! loop counters
      INTEGER      A2C

      INTEGER MY_TEMP
!     INTEGER, SAVE :: STARTCOL, ENDCOL
      LOGICAL, SAVE :: BNDY_PE_LOX, BNDY_PE_HIX

#ifdef parallel
      INTEGER EAST_COL
      REAL, ALLOCATABLE, SAVE :: HALO_EAST( :,:,: )
      REAL, ALLOCATABLE, SAVE :: HALO_WEST( :,:,: )
      REAL, ALLOCATABLE, SAVE :: BUF_EW( :,:,: )
#endif
      INTEGER, SAVE :: EFX    ! fixed parameter for eastern boundary
      INTEGER, SAVE :: WFX    ! fixed parameter for western boundary

C Required interface for allocatable array dummy arguments

      INTERFACE
         SUBROUTINE HCONTVEL( FDATE, FTIME, TSTEP, LVL, UORV, UHAT )
            INTEGER, INTENT( IN )         :: FDATE, FTIME, TSTEP, LVL
            CHARACTER( 16 ), INTENT( IN ) :: UORV
            REAL,    INTENT( OUT )        :: UHAT( :,: )
         END SUBROUTINE HCONTVEL
         SUBROUTINE HPPM ( NI, NJ, CON, VEL, DT, DS, ORI )
! #ifdef parallel
            INTEGER, PARAMETER         :: SWP = 3
! #else
!           INTEGER, PARAMETER         :: SWP = 1
! #endif
            INTEGER,   INTENT( IN )    :: NI, NJ
            REAL,      INTENT( INOUT ) :: CON( 1-SWP:,1: )
            REAL,      INTENT( IN )    :: VEL( : )
            REAL,      INTENT( IN )    :: DT
            REAL,      INTENT( IN )    :: DS
            CHARACTER, INTENT( IN )    :: ORI
         END SUBROUTINE HPPM
      END INTERFACE
C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         EFX = MY_NCOLS + 1
         WFX = 2 * MY_NCOLS + MY_NROWS + 4

C Get dx1 from HGRD_DEFN module

         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX1 = DG2M * XCELL_GD
     &          * COS( PI180*( YORIG_GD + YCELL_GD*FLOAT( GL_NROWS/2 ))) ! in m.
         ELSE
            DX1 = XCELL_GD        ! in m.
         END IF

         N_SPC_ADV = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV + 1
                                                  ! add 1 for advecting RHOJ

         ALLOCATE ( CONX( 1-SWP:MY_NCOLS+SWP,N_SPC_ADV ),
     &              VELX( MY_NCOLS+1 ), STAT = ALLOCSTAT ) ! Vel along a row
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating VELX, or CONX'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( ADV_MAP( N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating ADV_MAP'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF
#ifdef parallel
         ALLOCATE ( HALO_EAST( SWP,MY_NROWS,N_SPC_ADV ),
     &              HALO_WEST( SWP,MY_NROWS,N_SPC_ADV ),
     &              BUF_EW   ( SWP,MY_NROWS,N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating HALO_EAST, HALO_WEST, or BUF_EW'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF
         HALO_EAST = 0.0   ! array
         HALO_WEST = 0.0   ! array
         BUF_EW    = 0.0   ! array
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

         CALL SUBST_HI_LO_BND_PE ( 'C', BNDY_PE_LOX, BNDY_PE_HIX )

         END IF                    ! if firstime

      DT = FLOAT ( TIME2SEC ( TSTEP ) )

C Do the computation for x advection

C Get the contravariant x1 velocity component

      CALL HCONTVEL ( FDATE, FTIME, TSTEP, LVL, X1VEL, UHAT )

      CALL SUBST_COMM (UHAT, DSPL_N0_E1_S0_W0, DRCN_E)
#ifdef parallel
      EAST_COL = MY_NCOLS - SWP
      DO SPC = 1, N_SPC_ADV
         A2C = ADV_MAP( SPC )
         DO ROW = 1, MY_NROWS
            DO COL = 1, SWP
               HALO_WEST( COL,ROW,SPC ) = CGRID( COL,ROW,LVL,A2C )
               HALO_EAST( COL,ROW,SPC ) = CGRID( EAST_COL+COL,ROW,LVL,A2C )
               BUF_EW( COL,ROW,SPC ) = HALO_EAST( COL,ROW,SPC )
            END DO
         END DO
      END DO

      CALL SUBST_COMM (HALO_WEST, HALO_EAST, DSPL_N0_E1_S0_W0, DRCN_E)
      CALL SUBST_COMM (BUF_EW,    HALO_WEST, DSPL_N0_E0_S0_W1, DRCN_W)
#endif
      DO 233 ROW = 1, MY_NROWS

!        DO COL = STARTCOL, ENDCOL         !    DO COL = 1, MY_NCOLS+1
         DO COL = 1, MY_NCOLS+1
            VELX( COL ) = UHAT( COL,ROW )
         END DO

         DO SPC = 1, N_SPC_ADV

            A2C = ADV_MAP( SPC )
            DO COL = 1, MY_NCOLS
               CONX( COL,SPC ) = CGRID( COL,ROW,LVL,A2C )
            END DO
#ifdef parallel
            DO COL = 1, SWP
               CONX( COL-SWP,SPC )      = HALO_WEST( COL,ROW,SPC )
               CONX( MY_NCOLS+COL,SPC ) = HALO_EAST( COL,ROW,SPC )
            END DO
#endif
C West boundary

            IF ( BNDY_PE_LOX ) THEN
               IF ( VELX( 1 ) .LT. 0.0 ) THEN          ! outflow
                  CONX( 1-SWP:0,SPC ) =
     &               ZFDBC ( CONX( 1,SPC ), CONX( 2,SPC ),
     &                       VELX( 1 ),     VELX( 2 ) )
               ELSE    ! inflow
                  CONX( 1-SWP:0,SPC ) = BCON( WFX+ROW,SPC )
               END IF
            END IF

C East boundary

            IF ( BNDY_PE_HIX ) THEN
               IF ( VELX( MY_NCOLS+1 ) .GT. 0.0 ) THEN     ! outflow
                  CONX( MY_NCOLS+1:MY_NCOLS+SWP,SPC ) =
     &               ZFDBC ( CONX( MY_NCOLS,SPC ), CONX( MY_NCOLS-1,SPC ),
     &                       VELX( MY_NCOLS+1 ),   VELX( MY_NCOLS ) )
               ELSE    ! inflow
                  CONX( MY_NCOLS+1:MY_NCOLS+SWP,SPC ) = BCON( EFX+ROW,SPC )
               END IF
            END IF

         END DO

C PPM scheme

         CALL HPPM ( MY_NCOLS, MY_NROWS, CONX, VELX, DT, DX1, 'C' )

         DO SPC = 1, N_SPC_ADV
            A2C = ADV_MAP( SPC )
            DO COL = 1, MY_NCOLS
               CGRID( COL,ROW,LVL,A2C ) = CONX( COL,SPC )
            END DO
         END DO

!019  format( 'x_ppm# date, time, col, row, lvl, conx: ',
!    &         2I7, 3I4, 1pe12.3 )
2019  format( 'x_ppm# time, c, r, l, s, conx: ',
     &         I7.6, 4I4, 1pe12.3 )

233   CONTINUE

      RETURN
      END
