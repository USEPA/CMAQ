
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
C   16 NOv 2018 S.Napelenok: ISAM implementation
C    1 Feb 19 David Wong: removed all MY_N clauses
C   11 Dec 19 S.L.Napelenok: ddm-3d implementation for version 5.3.1
C-----------------------------------------------------------------------

      USE HGRD_DEFN             ! horizontal domain specifications
      USE GRID_CONF, ONLY: NLAYS
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE XY_BUDGET, ONLY : F_NORTH_IN, F_NORTH_OUT, F_SOUTH_IN, F_SOUTH_OUT
      USE PA_DEFN, ONLY : BUDGET_DIAG, BUDGET_HPPM
      use CENTRALIZED_IO_MODULE, only : interpolate_var, MSFX2
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_COMM_MODULE, SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_COMM_MODULE, NOOP_UTIL_MODULE)
#endif

#ifdef isam
      USE SA_DEFN, Only: ISAM, N_SPCTAG, S_SPCTAG, T_SPCTAG,
     &                   TRANSPORT_SPC, BCON_SPC, MAP_ADVtoSA
#endif

#ifdef sens
      USE DDM3D_DEFN, ONLY:SENGRID, NPMAX, NP, BSEN, BCS, DATENUM, IPT, IDATE, IPARM, IREGION, CKTIME, STARTDATE
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

      REAL,    SAVE :: DX2, DX1, AX             ! dx2 (meters), dx1 (m), ax (m2)

      REAL, ALLOCATABLE, SAVE :: VELY( : ),     ! Velocities along a column
     &                           CONY( :,: )    ! Conc's along a column
#ifdef isam
      REAL, ALLOCATABLE, SAVE :: SA_CONY( :,: )
#endif

      REAL          DT                          ! TSTEP in sec
      INTEGER       ALLOCSTAT

      INTEGER, ALLOCATABLE, SAVE :: ADV_MAP( : )  ! global adv map to CGRID

      CHARACTER( 16 ) :: X2VEL = 'X2VEL'

      INTEGER      COL, ROW, SPC, VAR, I           ! loop counters
      INTEGER      A2C

      REAL     :: JACOBM( NCOLS,NROWS,NLAYS )  !"total" Jacobian
      REAL     :: ZF( NCOLS,NROWS,NLAYS )      !Grid cell height
      REAL, PARAMETER :: MWAIR_SI = 0.02897    ! kg mol-1
      REAL     :: VOL, VOL2                    ! Grid Volume

      LOGICAL, SAVE :: BNDY_PE_LOY, BNDY_PE_HIY
      REAL, ALLOCATABLE, SAVE :: F_LO_IN( : )
      REAL, ALLOCATABLE, SAVE :: F_LO_OUT( : )
      REAL, ALLOCATABLE, SAVE :: F_HI_IN( : )
      REAL, ALLOCATABLE, SAVE :: F_HI_OUT( : )

#ifdef parallel
      INTEGER NORTH_ROW
      REAL, ALLOCATABLE, SAVE :: HALO_NORTH( :,:,: )
      REAL, ALLOCATABLE, SAVE :: HALO_SOUTH( :,:,: )
      REAL, ALLOCATABLE, SAVE :: BUF_NS( :,:,: )
#ifdef isam
      REAL, ALLOCATABLE, SAVE :: SA_HALO_NORTH( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SA_HALO_SOUTH( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SA_BUF_NS( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SA_F_LO_IN( : )
      REAL, ALLOCATABLE, SAVE :: SA_F_LO_OUT( : )
      REAL, ALLOCATABLE, SAVE :: SA_F_HI_IN( : )
      REAL, ALLOCATABLE, SAVE :: SA_F_HI_OUT( : )
#endif
#endif
      INTEGER, SAVE :: SFX    ! fixed parameter for southern boundary
      INTEGER, SAVE :: NFX    ! fixed parameter for northern boundary

#ifdef sens
      LOGICAL TIMEFLAG                  ! checks if within desired time
      REAL, ALLOCATABLE, SAVE :: SENY( :,: )    ! Sens along a col
      REAL,    EXTERNAL :: S_ZFDBC   ! similar to zfdbc, for sens
#ifdef parallel
      REAL, ALLOCATABLE, SAVE :: S_HALO_NORTH( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: S_HALO_SOUTH( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: S_BUF_NS( :,:,:,: )
#endif
#endif

C Required interface for allocatable array dummy arguments

      INTERFACE
         SUBROUTINE HCONTVEL ( FDATE, FTIME, TSTEP, LVL, UORV, UHAT )
            INTEGER,         INTENT( IN )     :: FDATE, FTIME, TSTEP, LVL
            CHARACTER( 16 ), INTENT( IN )     :: UORV
            REAL,            INTENT( OUT )    :: UHAT( :,: )
         END SUBROUTINE HCONTVEL
         SUBROUTINE HPPM ( NI, NJ, CON, VEL, DT, DS, ORI,
     &               F_LO_IN, F_LO_OUT, F_HI_IN, F_HI_OUT )
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
            REAL           , INTENT( OUT)     :: F_LO_IN( : )
            REAL           , INTENT( OUT)     :: F_LO_OUT( : )
            REAL           , INTENT( OUT)     :: F_HI_IN( : )
            REAL           , INTENT( OUT)     :: F_HI_OUT( : )
         END SUBROUTINE HPPM
      END INTERFACE
C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         SFX = 0
         NFX = NCOLS + NROWS + 3

C Get dx2 from HGRD_DEFN module

         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX1 = DG2M * XCELL_GD
     &          * COS( PI180*( YORIG_GD + YCELL_GD*FLOAT( GL_NROWS/2 ))) ! in m.
            DX2 = DG2M * YCELL_GD   ! in m.

         ELSE
            DX1 = XCELL_GD          ! in m.
            DX2 = YCELL_GD          ! in m.
         END IF
         AX = DX1 * DX2

         N_SPC_ADV = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_ADV + 1
                                                  ! add 1 for advecting RHOJ

         ALLOCATE ( CONY( 1-SWP:NROWS+SWP,N_SPC_ADV ),
#ifdef isam
     &              SA_CONY( 1-SWP:NROWS+SWP,N_SPCTAG ),
#endif
     &              VELY( NROWS+1 ), STAT = ALLOCSTAT ) ! Vel along a col
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating VELY, or CONY'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF
#ifdef isam
         SA_CONY = 0.0
#endif
         ALLOCATE ( ADV_MAP( N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating ADV_MAP'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF

#ifdef parallel
         ALLOCATE ( HALO_NORTH( NCOLS,SWP,N_SPC_ADV ),
     &              HALO_SOUTH( NCOLS,SWP,N_SPC_ADV ),
     &              BUF_NS    ( NCOLS,SWP,N_SPC_ADV ),
#ifdef isam
     &              SA_HALO_NORTH( NCOLS,SWP,N_SPCTAG ),
     &              SA_HALO_SOUTH( NCOLS,SWP,N_SPCTAG ),
     &              SA_BUF_NS    ( NCOLS,SWP,N_SPCTAG ),
#endif
     &              STAT = ALLOCSTAT )

         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating HALO_NORTH, HALO_SOUTH, or BUF_NS'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF
         HALO_NORTH = 0.0   ! array
         HALO_SOUTH = 0.0   ! array
         BUF_NS     = 0.0   ! array
#ifdef isam
         SA_HALO_NORTH = 0.0   ! array
         SA_HALO_SOUTH = 0.0   ! array
         SA_BUF_NS     = 0.0   ! array
#endif
#endif

#ifdef sens
         ALLOCATE ( SENY( 1-SWP:NROWS+SWP,N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating SENY'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
            END IF
#ifdef parallel
         ALLOCATE ( S_HALO_NORTH( NCOLS,SWP,N_SPC_ADV,NPMAX ),
     &              S_HALO_SOUTH( NCOLS,SWP,N_SPC_ADV,NPMAX ),
     &              S_BUF_NS    ( NCOLS,SWP,N_SPC_ADV,NPMAX ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG =
     &      'Failure allocating S_HALO_NORTH, S_HALO_SOUTH, or S_BUF_NS'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF
         S_HALO_NORTH = 0.0   ! array
         S_HALO_SOUTH = 0.0   ! array
         S_BUF_NS     = 0.0   ! array
#endif
#endif

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

         ADV_MAP( N_SPC_ADV ) = RHOJ_LOC

         CALL SUBST_HI_LO_BND_PE ( 'R', BNDY_PE_LOY, BNDY_PE_HIY )

         ! Allocate FLuxes for Budget Tool
         ALLOCATE( F_LO_IN(N_SPC_ADV),
     &             F_LO_OUT(N_SPC_ADV),
     &             F_HI_IN(N_SPC_ADV),
     &             F_HI_OUT(N_SPC_ADV) )

#ifdef isam
         ALLOCATE( SA_F_LO_IN(N_SPCTAG),
     &             SA_F_LO_OUT(N_SPCTAG),
     &             SA_F_HI_IN(N_SPCTAG),
     &             SA_F_HI_OUT(N_SPCTAG) )
#endif


         END IF                    ! if firstime

      DT = FLOAT ( TIME2SEC ( TSTEP ) )

#ifdef sens
      ! index that will be used by IDATE
      DATENUM = 1 + FDATE - STARTDATE
#endif

C Do the computation for y advection

C Get the contravariant x2 velocity component

      CALL HCONTVEL ( FDATE, FTIME, TSTEP, LVL, X2VEL, VHAT )

      CALL SUBST_COMM (VHAT, DSPL_N1_E0_S0_W0, DRCN_N)

#ifdef parallel
      NORTH_ROW = NROWS - SWP
      DO SPC = 1, N_SPC_ADV
         A2C = ADV_MAP( SPC )
         DO ROW = 1, SWP
            DO COL = 1, NCOLS
               HALO_SOUTH( COL,ROW,SPC ) = CGRID( COL,ROW,LVL,A2C )
               HALO_NORTH( COL,ROW,SPC ) = CGRID( COL,NORTH_ROW+ROW,LVL,A2C )
               BUF_NS( COL,ROW,SPC ) = HALO_NORTH( COL,ROW,SPC )
#ifdef sens
               DO NP = 1, NPMAX
                 S_HALO_SOUTH( COL,ROW,SPC,NP ) = SENGRID( COL,ROW,LVL,NP,A2C )
                 S_HALO_NORTH( COL,ROW,SPC,NP ) = SENGRID( COL,NORTH_ROW+ROW,LVL,NP,A2C )
                 S_BUF_NS( COL,ROW,SPC,NP ) = S_HALO_NORTH( COL,ROW,SPC,NP )
               END DO
#endif
            END DO
         END DO
      END DO

      CALL SUBST_COMM (HALO_SOUTH, HALO_NORTH, DSPL_N1_E0_S0_W0, DRCN_N)
      CALL SUBST_COMM (BUF_NS,     HALO_SOUTH, DSPL_N0_E0_S1_W0, DRCN_S)
#ifdef sens
      DO NP = 1, NPMAX
         CALL SUBST_COMM( S_HALO_SOUTH(:,:,:,NP), S_HALO_NORTH(:,:,:,NP),
     &                    DSPL_N1_E0_S0_W0, DRCN_N )
         CALL SUBST_COMM( S_BUF_NS(:,:,:,NP),         S_HALO_SOUTH(:,:,:,NP),
     &                    DSPL_N0_E0_S1_W0, DRCN_S )
      END DO
#endif


#ifdef isam
      DO SPC = 1, N_SPCTAG
        IF( TRANSPORT_SPC( SPC ) )THEN      
           DO ROW = 1, SWP
             DO COL = 1, NCOLS
               SA_HALO_SOUTH( COL,ROW,SPC ) =
     &                  ISAM( COL,ROW,LVL,S_SPCTAG( SPC ),T_SPCTAG( SPC ) )
               SA_HALO_NORTH( COL,ROW,SPC ) =
     &                  ISAM( COL,NORTH_ROW+ROW,LVL,S_SPCTAG( SPC ),T_SPCTAG( SPC ) )
               SA_BUF_NS( COL,ROW,SPC ) = SA_HALO_NORTH( COL,ROW,SPC )
             END DO
           END DO
        END IF
      END DO

      CALL SUBST_COMM (SA_HALO_SOUTH, SA_HALO_NORTH, DSPL_N1_E0_S0_W0, DRCN_N)
      CALL SUBST_COMM (SA_BUF_NS,     SA_HALO_SOUTH, DSPL_N0_E0_S1_W0, DRCN_S)

#endif

#endif

      DO 233 COL = 1, NCOLS

         DO ROW = 1, NROWS+1
            VELY( ROW ) = VHAT( COL,ROW )
         END DO

         DO SPC = 1, N_SPC_ADV

            A2C = ADV_MAP( SPC )
            DO ROW = 1, NROWS
               CONY( ROW,SPC ) = CGRID( COL,ROW,LVL,A2C )
            END DO

#ifdef parallel
            DO ROW = 1, SWP
               CONY( ROW-SWP,SPC )      = HALO_SOUTH( COL,ROW,SPC )
               CONY( NROWS+ROW,SPC ) = HALO_NORTH( COL,ROW,SPC )
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
               IF ( VELY( NROWS+1 ) .GT. 0.0 ) THEN     ! outflow
                  CONY( NROWS+1:NROWS+SWP,SPC ) =
     &               ZFDBC ( CONY( NROWS,SPC ), CONY( NROWS-1,SPC ),
     &                       VELY( NROWS+1 ),   VELY( NROWS ) )
               ELSE    ! inflow
                  CONY( NROWS+1:NROWS+SWP,SPC ) = BCON( NFX+COL,SPC )
               END IF
            END IF

         END DO

#ifdef isam
         DO SPC = 1, N_SPCTAG

            IF( TRANSPORT_SPC( SPC ) )THEN

                DO ROW = 1, NROWS
                   SA_CONY( ROW,SPC ) = ISAM( COL,ROW,LVL,S_SPCTAG( SPC ),T_SPCTAG( SPC ) )
                END DO
                
#ifdef parallel
                DO ROW = 1, SWP
                   SA_CONY( ROW-SWP,SPC ) = SA_HALO_SOUTH( COL,ROW,SPC )
                   SA_CONY( NROWS+ROW,SPC ) = SA_HALO_NORTH( COL,ROW,SPC )
                END DO
#endif

C South boundary
                IF ( BNDY_PE_LOY ) THEN
                   IF ( VELY( 1 ) .LT. 0.0 ) THEN   ! outflow
                      SA_CONY( 1-SWP:0,SPC ) =
     &                ZFDBC ( SA_CONY( 1,SPC ), SA_CONY( 2,SPC ), VELY( 1 ), VELY( 2 ) )
                   ELSE    ! inflow
                      IF ( BCON_SPC( SPC ) ) THEN
                         SA_CONY( 1-SWP:0,SPC ) = BCON( SFX+COL,MAP_ADVtoSA( SPC )  )
                      ELSE
                         SA_CONY( 1-SWP:0,SPC ) = 0.0
                      END IF 
                   END IF   ! vely < 0 ?
                END IF   ! bndy_pe_loy ?

C North boundary
                IF ( BNDY_PE_HIY ) THEN
                   IF ( VELY( NROWS+1 ) .GT. 0.0 ) THEN     ! outflow
                      SA_CONY( NROWS+1:NROWS+SWP,SPC ) =
     &                ZFDBC ( SA_CONY( NROWS,SPC ), SA_CONY( NROWS-1,SPC ),
     &                        VELY( NROWS+1 ), VELY( NROWS ) )
                   ELSE    ! inflow
                      IF ( BCON_SPC( SPC ) ) THEN
                         SA_CONY( NROWS+1:NROWS+SWP,SPC ) = BCON( NFX+COL,MAP_ADVtoSA( SPC ) )
                      ELSE
                         SA_CONY( NROWS+1:NROWS+SWP,SPC ) = 0.0
                      END IF
                   END IF   ! vely > 0 ?
                END IF   ! bndy_pe_hiy ?
            END IF  
         END DO   ! SPC loop
#endif

C PPM scheme
         F_LO_IN = 0.
         F_LO_OUT= 0.
         F_HI_IN = 0.
         F_HI_OUT= 0.

         IF ( BUDGET_DIAG ) BUDGET_HPPM = .TRUE.
         CALL HPPM ( NROWS, NCOLS, CONY, VELY, DT, DX2, 'R',
     &               F_LO_IN, F_LO_OUT, F_HI_IN, F_HI_OUT )
 
         ! Store Boundary Fluxes for Budget Diagnostic
         ! Flux units are converted:
         !     vapors: [Jacobian x rho] x [ppm] --> umol
         !     aerosol mass: [Jacobian] x [kg m-3] --> kg
         !     aerosol number: [Jacobian] x [N m-3] --> N
         !     aerosol surface area: [Jacobian] x [m2 m-3] --> m2
         IF ( BUDGET_HPPM ) THEN
            IF ( BNDY_PE_LOY ) THEN
                call interpolate_var ('JACOBM', fdate, ftime, JACOBM)
                call interpolate_var ('ZF', fdate, ftime, ZF)
                IF ( LVL .EQ. 1 ) THEN
                    VOL = AX * ZF( COL,1,1 ) / MSFX2( COL,1 )
                ELSE
                    VOL = AX * ( ZF( COL,1,LVL ) - ZF( COL,1,LVL-1 ) ) / MSFX2(COL,1)
                END IF
                DO I = 1,N_SPC_ADV
                   A2C = ADV_MAP( I )
                   VOL2 = VOL
                   IF ( .NOT. CGRID_MASK_AERO( A2C ) ) VOL2 = VOL / MWAIR_SI
                   F_SOUTH_IN( LVL,A2C )  = F_SOUTH_IN( LVL,A2C ) 
     &                      + F_LO_IN(I) / JACOBM( COL,1,LVL ) * VOL2
                   F_SOUTH_OUT( LVL,A2C ) = F_SOUTH_OUT( LVL,A2C ) 
     &                      + F_LO_OUT(I) / JACOBM( COL,1,LVL ) * VOL2
                END DO
            END IF
            IF ( BNDY_PE_HIY ) THEN
                call interpolate_var ('JACOBM', fdate, ftime, JACOBM)
                call interpolate_var ('ZF', fdate, ftime, ZF)
                IF ( LVL .EQ. 1 ) THEN
                    VOL = AX * ZF( COL,NROWS,1 ) / MSFX2( COL,NROWS )
                ELSE
                    VOL = AX * ( ZF( COL,NROWS,LVL ) - ZF( COL,NROWS,LVL-1 ) ) / MSFX2(COL,NROWS)
                END IF
                DO I = 1,N_SPC_ADV
                   A2C = ADV_MAP( I )
                   VOL2 = VOL
                   IF ( .NOT. CGRID_MASK_AERO( A2C ) ) VOL2 = VOL / MWAIR_SI
                   F_NORTH_IN( LVL,A2C )  = F_NORTH_IN( LVL,A2C ) 
     &                      + F_HI_IN(I) / JACOBM( COL,NROWS,LVL ) * VOL2
                   F_NORTH_OUT( LVL,A2C ) = F_NORTH_OUT( LVL,A2C ) 
     &                      + F_HI_OUT(I) / JACOBM( COL,NROWS,LVL ) * VOL2
                END DO
            END IF
            BUDGET_HPPM = .FALSE.
         END IF
         
#ifdef isam
         CALL HPPM ( NROWS, NCOLS, SA_CONY, VELY, DT, DX2, 'R',
     &               SA_F_LO_IN, SA_F_LO_OUT, SA_F_HI_IN, SA_F_HI_OUT )
#endif

         DO SPC = 1, N_SPC_ADV
            A2C = ADV_MAP( SPC )
            DO ROW = 1, NROWS
               CGRID( COL,ROW,LVL,A2C ) = CONY( ROW,SPC )
            END DO
         END DO

#ifdef isam
        DO SPC = 1, N_SPCTAG
           IF( TRANSPORT_SPC( SPC ) )THEN
              DO ROW = 1, NROWS
                 ISAM( COL,ROW,LVL,S_SPCTAG( SPC ),T_SPCTAG( SPC ) ) = SA_CONY( ROW,SPC )
              END DO
           END IF   
        END DO
#endif


#ifdef sens
         DO NP = 1, NPMAX

           CALL CKTIME( FDATE,FTIME,NP,TIMEFLAG) ! Check if the current time is within the time range

           DO SPC = 1, N_SPC_ADV
             A2C = ADV_MAP( SPC )
             DO ROW = 1, NROWS
               SENY( ROW,SPC ) = SENGRID( COL,ROW,LVL,NP,A2C )
             END DO

#ifdef parallel
             DO ROW = 1, SWP
               SENY( ROW-SWP,SPC )   = S_HALO_SOUTH( COL,ROW,SPC,NP )
               SENY( NROWS+ROW,SPC ) = S_HALO_NORTH( COL,ROW,SPC,NP )
             END DO
#endif

C South boundary
             IF ( BNDY_PE_LOY ) THEN
               IF ( VELY( 1 ) .LT. 0.0 ) THEN          ! outflow
                 IF ( CONY( 0, SPC ) .EQ. 0.0 ) THEN
                   SENY( 1-SWP:0, SPC ) = 0.0
                 ELSE ! Zero-flux divergence boundary condition for sens
                   SENY( 1-SWP:0,SPC ) =
     &               S_ZFDBC ( SENY( 1,SPC ), SENY( 2,SPC ),
     &                         VELY( 1 ),     VELY( 2 ) )
                 END IF
               ELSE    ! inflow
                 IF ( BCS ) THEN
                   SENY( 1-SWP:0,SPC ) = BSEN( SFX+COL,SPC,NP )
                 ELSE IF ( ( IPT( NP ) .EQ. 2 ) .AND. ( TIMEFLAG ) ) THEN
                 ! Insert boundary condition to SEN iff time, date,
                 ! species, and region match and senstype is BOUN
                   SENY( 1-SWP:0,SPC ) = BCON( SFX+COL,SPC )
     &                                 * IREGION( COL, 1, LVL, NP )
     &                                 * REAL ( IDATE ( NP, DATENUM ) )
     &                                 * REAL ( IPARM ( NP, A2C     ) )
                 ELSE
                 ! Otherwise, set to 0
                   SENY( 1-SWP:0, SPC ) = 0.0
                 END IF
               END IF
             END IF

C North boundary
             IF ( BNDY_PE_HIY ) THEN
               IF ( VELY( NROWS+1 ) .GT. 0.0 ) THEN     ! outflow
                 IF ( CONY( NROWS+1, SPC ) .EQ. 0.0 ) THEN
                   SENY( NROWS+1:NROWS+SWP, SPC ) = 0.0
                 ELSE
                 ! Zero-flux divergence boundary condition for sens
                   SENY( NROWS+1:NROWS+SWP,SPC ) =
     &               S_ZFDBC ( SENY( NROWS,SPC ), SENY( NROWS-1,SPC ),
     &                         VELY( NROWS+1 ),   VELY( NROWS ) )
                 END IF
               ELSE    ! inflow
                 IF ( BCS ) THEN
                   SENY( NROWS+1:NROWS+SWP,SPC ) = BSEN( NFX+COL,SPC,NP )
                 ELSE IF ( ( IPT( NP ) .EQ. 2 ) .AND. ( TIMEFLAG ) ) THEN
                 ! Insert boundary condition to SEN iff time, date,
                 ! species, and region match and senstype is BOUN
                   SENY( NROWS+1:NROWS+SWP,SPC ) = BCON( NFX+COL,SPC )
     &                               * IREGION( COL, NROWS, LVL, NP )
     &                               * REAL ( IDATE ( NP, DATENUM ) )
     &                               * REAL ( IPARM ( NP, A2C     ) )
                 ELSE
                   SENY( NROWS+1:NROWS+SWP,SPC ) = 0.0
                 END IF
               END IF
             END IF

           END DO

C PPM scheme
           CALL HPPM ( NROWS, NCOLS, SENY, VELY, DT, DX2, 'R',
     &               F_LO_IN, F_LO_OUT, F_HI_IN, F_HI_OUT )

           DO SPC = 1, N_SPC_ADV
             A2C = ADV_MAP( SPC )
             DO ROW = 1, NROWS
               SENGRID( COL,ROW,LVL,NP,A2C ) = SENY( ROW,SPC )
             END DO
           END DO

         END DO   ! NP
#endif

233   CONTINUE

      RETURN
      END
