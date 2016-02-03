
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
C $Header: /project/yoj/arc/CCTM/src/hadv/yamo/x_ppm.F,v 1.3 2011/10/21 16:11:20 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#ifdef isam
      SUBROUTINE X_PPM ( CGRID, FDATE, FTIME, TSTEP, LVL, BCON, ISAM )
#else
      SUBROUTINE X_PPM ( CGRID, FDATE, FTIME, TSTEP, LVL, BCON )
#endif

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
C   19 Jul 13 R.Kwok: isam
C-----------------------------------------------------------------------

      USE HGRD_DEFN             ! horizontal domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE SUBST_MODULES         ! stenex
!     USE SUBST_COMM_MODULE     ! stenex
!     USE SUBST_UTIL_MODULE     ! stenex
#ifdef isam
      USE SA_DEFN               ! 20120816
      USE OZVOC_DEFN            ! 20120925
#endif

#ifdef parallel
      USE SWAP_SANDIA
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
#ifdef isam
      REAL,    POINTER      :: ISAM( :,:,:,:,: )
#endif

C External Functions not declared in IODECL3.EXT:

      REAL,    EXTERNAL :: ZFDBC

C Parameters:

C Advected species dimension

      INTEGER, SAVE :: N_SPC_ADV

#ifdef parallel
      INTEGER, PARAMETER :: SWP = 3
#else
      INTEGER, PARAMETER :: SWP = 1
#endif

C File Variables:

      REAL         UHAT( NCOLS+1,NROWS+1 )       ! x1-component CX-velocity

C Local Variables:

      CHARACTER( 16 ) :: PNAME = 'X_PPM'
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      CHARACTER( 96 ) :: XMSG = ' '

      integer, save :: logdev

      REAL,    SAVE :: DX1                      ! dx1 (meters)
      INTEGER, SAVE :: ASPC                     ! RHOJ index in CGRID

      REAL, ALLOCATABLE, SAVE :: VELX( : ),     ! Velocities along a row
     &                           CONX( :,: )    ! Conc's along a row
#ifdef isam
      REAL, ALLOCATABLE, SAVE :: SA_CONX( :,: )
      INTEGER  IBGN, ILAST, S
#endif

      REAL          DT                          ! TSTEP in sec
      INTEGER       ALLOCSTAT

      INTEGER, ALLOCATABLE, SAVE :: ADV_MAP( : ) ! global adv map to CGRID
#ifdef isam
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: NAME_ADV( : )  ! corresponding species name - KRT
      INTEGER, SAVE ::    INDX_O3                      ! 20120818
      INTEGER             JV                           ! 20120925 for VOC
      REAL                BC_VOC                       ! 20120925 for VOC
#endif

      CHARACTER( 16 ) :: X1VEL = 'X1VEL'

      INTEGER      COL, ROW, SPC, VAR           ! loop counters
      INTEGER      A2C

      INTEGER MY_TEMP
!     INTEGER, SAVE :: STARTCOL, ENDCOL
      LOGICAL, SAVE :: BNDY_PE_LOX, BNDY_PE_HIX

#ifdef parallel
      INTEGER EAST_COL, M
!     REAL    HALO_EAST( SWP,NROWS,N_SPC_ADV )
!     REAL    HALO_WEST( SWP,NROWS,N_SPC_ADV )
!     REAL    BUF_EW   ( SWP*NROWS*N_SPC_ADV )
      REAL, ALLOCATABLE, SAVE :: HALO_EAST( :,:,: )
      REAL, ALLOCATABLE, SAVE :: HALO_WEST( :,:,: )
      REAL, ALLOCATABLE, SAVE :: BUF_EW( : )
#ifdef isam
      REAL, ALLOCATABLE, SAVE :: SA_HALO_EAST( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SA_HALO_WEST( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SA_BUF_EW( : )
#endif
#endif

C Statement functions:

      INTEGER, SAVE :: EFX    ! fixed parameter for eastern boundary
      INTEGER, SAVE :: WFX    ! fixed parameter for western boundary

      REAL    BCCN            ! boundary concentrations stmt fn

      INTEGER BFX             ! dummy positional parameter
      INTEGER CR              ! row or column index
      INTEGER SS              ! species index

      BCCN ( BFX, CR, SS ) = BCON( BFX + CR, SS )

C Required interface for allocatable array dummy arguments

      INTERFACE
         SUBROUTINE HCONTVEL( FDATE, FTIME, TSTEP, LVL, UORV, UHAT )
            IMPLICIT NONE
            INTEGER, INTENT( IN )         :: FDATE, FTIME, TSTEP, LVL
            CHARACTER( 16 ), INTENT( IN ) :: UORV
            REAL,    INTENT( OUT )        :: UHAT( :,: )
         END SUBROUTINE HCONTVEL
         SUBROUTINE HPPM ( NI, NJ, CON, VEL, DT, DS, ORI )
            IMPLICIT NONE
#ifdef parallel
            INTEGER, PARAMETER         :: SWP = 3
#else
            INTEGER, PARAMETER         :: SWP = 1
#endif
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
         logdev = init3 ()

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

#ifdef isam
         ALLOCATE ( CONX( 1-SWP:MY_NCOLS+SWP,N_SPC_ADV ),
     &              SA_CONX( 1-SWP:MY_NCOLS+SWP,N_SPCTAG ), ! 20120816
     &              VELX( MY_NCOLS+1 ), STAT = ALLOCSTAT ) ! Vel along a row
#else
         ALLOCATE ( CONX( 1-SWP:MY_NCOLS+SWP,N_SPC_ADV ),
     &              VELX( MY_NCOLS+1 ), STAT = ALLOCSTAT ) ! Vel along a row
#endif
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating VELX, or CONX'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( ADV_MAP( N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating ADV_MAP'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF
#ifdef isam
         ALLOCATE ( NAME_ADV( N_SPC_ADV ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating NAME_ADV'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF
#endif

#ifdef parallel
#ifdef isam
         ALLOCATE ( HALO_EAST( SWP,MY_NROWS,N_SPC_ADV ),
     &              HALO_WEST( SWP,MY_NROWS,N_SPC_ADV ),
     &              BUF_EW   ( SWP*MY_NROWS*N_SPC_ADV ),
     &              SA_HALO_EAST( SWP,MY_NROWS,N_SPCTAG ),
     &              SA_HALO_WEST( SWP,MY_NROWS,N_SPCTAG ),
     &              SA_BUF_EW   ( SWP*MY_NROWS*N_SPCTAG ), STAT = ALLOCSTAT )
#else
         ALLOCATE ( HALO_EAST( SWP,MY_NROWS,N_SPC_ADV ),
     &              HALO_WEST( SWP,MY_NROWS,N_SPC_ADV ),
     &              BUF_EW   ( SWP*MY_NROWS*N_SPC_ADV ), STAT = ALLOCSTAT )
#endif
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating HALO_EAST, HALO_WEST, or BUF_EW'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF
         HALO_EAST = 0.0   ! array
         HALO_WEST = 0.0   ! array
         BUF_EW    = 0.0   ! array
#ifdef isam
         SA_HALO_EAST = 0.0   ! KRT array
         SA_HALO_WEST = 0.0   ! KRT array
         SA_BUF_EW    = 0.0   ! KRT array
#endif
#endif

         ASPC = GC_STRT - 1 + N_GC_SPCD

C Create global map to CGRID

         SPC = 0
         DO VAR = 1, N_GC_TRNS
            SPC = SPC + 1
            ADV_MAP( SPC ) = GC_STRT - 1 + GC_TRNS_MAP( VAR )
#ifdef isam
            NAME_ADV( SPC ) = GC_TRNS( VAR )  ! KRT
#endif
         END DO
         DO VAR = 1, N_AE_TRNS
            SPC = SPC + 1
            ADV_MAP( SPC ) = AE_STRT - 1 + AE_TRNS_MAP( VAR )
#ifdef isam
            NAME_ADV( SPC ) = AE_TRNS( VAR )  ! KRT
#endif
         END DO
         DO VAR = 1, N_NR_TRNS
            SPC = SPC + 1
            ADV_MAP( SPC ) = NR_STRT - 1 + NR_TRNS_MAP( VAR )
#ifdef isam
            NAME_ADV( SPC ) = NR_TRNS( VAR )  ! KRT
#endif
         END DO
         DO VAR = 1, N_TR_ADV
            SPC = SPC + 1
            ADV_MAP( SPC ) = TR_STRT - 1 + TR_ADV_MAP( VAR )
#ifdef isam
            NAME_ADV( SPC ) = TR_ADV( VAR )  ! KRT
#endif
         END DO

         ADV_MAP( N_SPC_ADV ) = ASPC

#ifdef isam
         INDX_O3 = INDEX1( 'O3', N_SPC_ADV, NAME_ADV )
#endif

!        CALL SUBST_LOOP_INDEX ( 'C', 1, MY_NCOLS, 1, MY_TEMP,
!    &                           STARTCOL, ENDCOL )

!        write( logdev,* ) 'mype, my_ncols, startcol, endcol: ',
!    &                      mype, my_ncols, startcol, endcol

         CALL SUBST_HI_LO_BND_PE ( 'C', BNDY_PE_LOX, BNDY_PE_HIX )

         END IF                    ! if firstime

      DT = FLOAT ( TIME2SEC ( TSTEP ) )

C Do the computation for x advection

C Get the contravariant x1 velocity component

      CALL HCONTVEL ( FDATE, FTIME, TSTEP, LVL, X1VEL, UHAT )

#ifdef parallel
C SWAPnD interface (same array in both procs)
C                                                     SWAP1D SWAP2D SWAP3D
C 1st value of array to be sent,                         X     X      X
C 1st value of array to be received,                     X     X      X
C number of values to send in 1st dimension of array,    X     X      X
C number of values to send in 2nd dimension of array,          X      X
C number of values to send in 3rd dimension of array,                 X
C size of 1st dimension in array,                              X      X
C size of 2nd dimension in array,                                     X
C direction to receive from: NORTH,SOUTH,EAST,WEST       X     X      X

      CALL SWAP2D( UHAT( 1,1 ), UHAT( NCOLS+1,1 ), 1, NROWS+1, NCOLS+1, EAST )
      M = 0
      EAST_COL = MY_NCOLS - SWP
      DO SPC = 1, N_SPC_ADV
         A2C = ADV_MAP( SPC )
         DO ROW = 1, MY_NROWS
            DO COL = 1, SWP
               HALO_WEST( COL,ROW,SPC ) = CGRID( COL,ROW,LVL,A2C )
               HALO_EAST( COL,ROW,SPC ) = CGRID( EAST_COL+COL,ROW,LVL,A2C )
               M = M + 1
               BUF_EW( M ) = HALO_EAST( COL,ROW,SPC )
            END DO
         END DO
      END DO

      CALL SWAP3D( HALO_WEST, HALO_EAST, SWP, MY_NROWS, N_SPC_ADV,
     &             SWP, MY_NROWS, EAST )
      CALL SWAP3D( BUF_EW,    HALO_WEST, SWP, MY_NROWS, N_SPC_ADV,
     &             SWP, MY_NROWS, WEST )

#ifdef isam
      M = 0
      DO SPC = 1, N_SPCTAG
         DO ROW = 1, MY_NROWS
            DO COL = 1, SWP
               SA_HALO_WEST( COL,ROW,SPC ) =
     &                 ISAM( COL,ROW,LVL,S_SPCTAG( SPC ),T_SPCTAG ( SPC ) )
               SA_HALO_EAST( COL,ROW,SPC ) =
     &                 ISAM( EAST_COL+COL,ROW,LVL,S_SPCTAG( SPC ),T_SPCTAG( SPC ) )
               M = M + 1
               SA_BUF_EW( M ) = SA_HALO_EAST( COL,ROW,SPC )
            END DO
         END DO
      END DO
      CALL SWAP3D( SA_HALO_WEST, SA_HALO_EAST, SWP, MY_NROWS, N_SPCTAG,
     &             SWP, MY_NROWS, EAST )
      CALL SWAP3D( SA_BUF_EW,    SA_HALO_WEST, SWP, MY_NROWS, N_SPCTAG,
     &             SWP, MY_NROWS, WEST )
#endif

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
                  CONX( 1-SWP:0,SPC ) = BCCN( WFX,ROW,SPC )
               END IF
            END IF

C East boundary

            IF ( BNDY_PE_HIX ) THEN
               IF ( VELX( MY_NCOLS+1 ) .GT. 0.0 ) THEN     ! outflow
                  CONX( MY_NCOLS+1:MY_NCOLS+SWP,SPC ) =
     &               ZFDBC ( CONX( MY_NCOLS,SPC ), CONX( MY_NCOLS-1,SPC ),
     &                       VELX( MY_NCOLS+1 ),   VELX( MY_NCOLS ) )
               ELSE    ! inflow
                  CONX( MY_NCOLS+1:MY_NCOLS+SWP,SPC ) = BCCN( EFX,ROW,SPC )
               END IF
            END IF

         END DO

#ifdef isam
         DO SPC = 1, N_SPCTAG
            IBGN  = INDEX( VNAM_SPCTAG( SPC ), '_', .FALSE. ) - 1
            ILAST = LEN_TRIM( VNAM_SPCTAG( SPC ) )

            DO COL = 1, MY_NCOLS
               SA_CONX( COL, SPC  ) = ISAM( COL,ROW,LVL,S_SPCTAG( SPC ),T_SPCTAG( SPC ) )
            END DO
#ifdef parallel
            DO COL = 1, SWP
               SA_CONX( COL-SWP,SPC ) = SA_HALO_WEST( COL,ROW,SPC )
               SA_CONX( MY_NCOLS+COL,SPC ) = SA_HALO_EAST( COL,ROW,SPC )
            END DO
#endif

C West boundary
            IF ( BNDY_PE_LOX ) THEN
               IF ( VELX( 1 ) .LT. 0.0 ) THEN   ! outflow
                  SA_CONX( 1-SWP:0,SPC ) =
     &            ZFDBC( SA_CONX( 1,SPC ), SA_CONX( 2,SPC ), VELX( 1 ), VELX( 2 ) )
               ELSE    ! inflow
                  IF ( VNAM_SPCTAG( SPC )( ILAST-3:ILAST ) .EQ. 'BCON' ) THEN
                     S = INDEX1( VNAM_SPCTAG( SPC )( 1:IBGN ), N_SPC_ADV, NAME_ADV )
                     IF ( S .GT. 0 ) THEN
                        SA_CONX( 1-SWP:0,SPC ) = BCCN( WFX,ROW,S )
                     ELSE IF ( VNAM_SPCTAG( SPC )( 1:IBGN ) .EQ. 'O3A' ) THEN
                        IF ( INDX_O3 .GT. 0 ) SA_CONX( 1-SWP:0,SPC ) =
     &                                        BCCN( WFX,ROW,INDX_O3 )
                     ELSE IF ( VNAM_SPCTAG( SPC )( 1:IBGN ) .EQ. 'O3V' .OR.
     &                         VNAM_SPCTAG( SPC )( 1:IBGN ) .EQ. 'O3N' ) THEN
                        IF ( INDX_O3 .GT. 0 ) SA_CONX( 1-SWP:0,SPC ) =
     &                                        0.5 * BCCN( WFX,ROW,INDX_O3 )
                     END IF   ! tracking species exist in west
                  ELSE   ! non-bcon tags ?
                     SA_CONX( 1-SWP:0,SPC ) = 0.0
                  END IF   ! ending with 'bcon' ?
               END IF   ! velx < 0 ?
            END IF   ! bndy_pe_lox ?

C East boundary
            IF ( BNDY_PE_HIX ) THEN
               IF ( VELX( MY_NCOLS+1 ) .GT. 0.0 ) THEN     ! outflow
                  SA_CONX( MY_NCOLS+1:MY_NCOLS+SWP,SPC ) =
     &            ZFDBC ( SA_CONX( MY_NCOLS,SPC ), SA_CONX( MY_NCOLS-1,SPC ),
     &                    VELX( MY_NCOLS+1 ), VELX( MY_NCOLS ) )
               ELSE    ! inflow
                  IF ( VNAM_SPCTAG(SPC)(ILAST-3:ILAST) .EQ. 'BCON' ) THEN
                     S = INDEX1( VNAM_SPCTAG( SPC )( 1:IBGN ), N_SPC_ADV, NAME_ADV )
                     IF ( S .GT. 0 ) THEN
                        SA_CONX( MY_NCOLS+1:MY_NCOLS+SWP,SPC ) = BCCN( EFX,ROW,S )
                     ELSE IF ( VNAM_SPCTAG( SPC )( 1:IBGN ) .EQ. 'O3A' ) THEN
                        IF ( INDX_O3 .GT. 0 ) SA_CONX( MY_NCOLS+1:MY_NCOLS+SWP,SPC ) =
     &                                        BCCN( EFX,ROW,INDX_O3 )
                     ELSE IF ( VNAM_SPCTAG( SPC )( 1:IBGN ) .EQ. 'O3V' .OR.
     &                         VNAM_SPCTAG( SPC )( 1:IBGN ) .EQ. 'O3N' ) THEN
                        IF ( INDX_O3 .GT. 0 ) SA_CONX( MY_NCOLS+1:MY_NCOLS+SWP,SPC ) =
     &                                        0.5* BCCN( EFX,ROW,INDX_O3 )
                     END IF   ! tracking species exist in east
                  ELSE   ! non-bcon tags ?
                     SA_CONX( MY_NCOLS+1:MY_NCOLS+SWP,SPC ) = 0.0
                  END IF   ! ending with 'bcon' ?
               END IF   ! velx > 0 ?
            END IF   ! bndy_pe_hix ?

         END DO  ! SPC loop
#endif

C PPM scheme

         CALL HPPM ( MY_NCOLS, MY_NROWS, CONX, VELX, DT, DX1, 'C' )
#ifdef isam
         CALL HPPM ( MY_NCOLS, MY_NROWS, SA_CONX, VELX, DT, DX1, 'C' )
#endif

         DO SPC = 1, N_SPC_ADV
            A2C = ADV_MAP( SPC )
            DO COL = 1, MY_NCOLS
               CGRID( COL,ROW,LVL,A2C ) = CONX( COL,SPC )
!              if ( conx( col,spc ) .le. 0.0 )
!    &            write( logdev,2019 ) ftime, col, row, lvl, spc, conx(col,spc )
            END DO
         END DO

!        do col = 1, my_ncols
!           if ( conx( col,n_spc_adv ) .le. 0.0 ) then
!              write( logdev,2019 ) ftime, col, row, lvl,
!    &                              conx( col,n_spc_adv )
!           end if
!        end do
!019  format( 'x_ppm# date, time, col, row, lvl, conx: ',
!    &         2I7, 3I4, 1pe12.3 )
2019  format( 'x_ppm# time, c, r, l, s, conx: ',
     &         I7.6, 4I4, 1pe12.3 )

#ifdef isam
         DO SPC = 1, N_SPCTAG
            DO COL = 1, MY_NCOLS
               ISAM( COL,ROW,LVL,S_SPCTAG( SPC ),T_SPCTAG( SPC ) ) = SA_CONX( COL,SPC )
            END DO
         END DO
#endif

233   CONTINUE

      RETURN
      END
