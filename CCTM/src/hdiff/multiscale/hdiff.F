
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
      SUBROUTINE HDIFF ( CGRID, JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C Function:
C   Horizontal diffusion with constant eddy diffusivity - gen. coord.
C   The process time step is set equal to TSTEP(2). Boundary concentrations
C   are set using a Dirichlet (no flux) condition
      
C Preconditions:
C   Dates and times represented YYYYDDD:HHMMSS.
C   No "skipped" dates and times.  All boundary input variables have the
C   same boundary perimeter structure with a thickness of 1
C   CGRID in ppm units or micro-g/m**3, #/m**3 for aerosols
      
C Subroutines and functions called:
C   TIME2SEC, SEC2TIME, CGRID_MAP, NEXTIME, RHO_J, 
C   HCDIFF3D
 
C Revision history:
C   Jeff - 5 Nov 97, 1 Jan 98
C   DWB  - 1 Feb 98, use simple B/C (no conc gradient at domain boundary)

C   David Wong Sep. 1998
C     -- parallelized the code
C     -- removed the intermediate constant CRHOJ_Q and placed the answer of
C        the calculation directly into CGRID. Removed the next immediate
C        loop completely.

C   David Wong 1/19/99
C      -- add a loop_index call
C      -- change loop index ending point to avoid accessing invalid region.
C         (reason to do this is to prevent using boundary data from PINTERP,
C          which sets pseudo-boundary data to 0)
 
C   Daewon Byun 10/10/2000
C      -- generalized 3d horizontal diffusivity module
C      -- accomdates 3d hdiff values

C    15 Dec 00 J.Young: PE_COMM3 -> Dave Wong's f90 stenex COMM 
C     6 Aug 01 J.Young: Use HGRD_DEFN
C    25 Mar 04 G.Hammond: RK11/RK22 ghost cell updates moved outside main loop;
C                         use explicit boundary arrays for CGRID ghost cells;
C                         use SNL's "swap3d".
C    31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                       domain specifications in one module
C    17 Dec 09 J.Young: fix CONC initializing error for sub-cycling timesteps (DO 344)
C                       reported by Talat Odman and Yongtao Hu of GA tech.
C    21 Jun 10 J.Young: convert for Namelist redesign
C    16 Feb 11 S. Roselle: replaced I/O-API include files w/UTILIO_DEFN
C    11 May 11 D.Wong: incorporated twoway model implementation
C    29 Nov 17 D.Wong: removed all SWAP routines and replaced with SE_COMM
C    18 Nov 18 S. Napelenok: ISAM implementation
C    01 Feb 19 David Wong: removed all MY_N clauses
C    11 Dec 19 S.L.Napelenok: ddm-3d implementation for version 5.3.1
C-----------------------------------------------------------------------
      
      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN

#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_COMM_MODULE, SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_COMM_MODULE, NOOP_UTIL_MODULE)
#endif

#ifdef isam
      USE SA_DEFN, Only: ISAM, N_SPCTAG, S_SPCTAG, T_SPCTAG, TRANSPORT_SPC
#endif

#ifdef sens
      USE DDM3D_DEFN, ONLY: SENGRID, NPMAX, NP
#endif

      IMPLICIT NONE

C Includes:

      INCLUDE SUBST_CONST       ! constants
      INCLUDE SUBST_PE_COMM     ! PE communication displacement and direction

C Arguments:
      
      REAL, POINTER :: CGRID( :,:,:,: )
      INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP( 3 ) ! time step vector (HHMMSS)
                                          ! TSTEP(1) = local output step
                                          ! TSTEP(2) = sciproc sync. step (chem)
                                          ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                          !            step and wrf/cmaq call frequency

C External Functions: None
           
C Parameters:

C Advected species dimension

      INTEGER, SAVE :: N_SPC_DIFF
 
C File Variables:
 
      REAL          CONC  ( 0:NCOLS+1,0:NROWS+1 )   ! conc working array
      REAL          RHOJ  ( 0:NCOLS+1,0:NROWS+1,NLAYS ) ! density X Jacobian
      CHARACTER( 8 ), SAVE :: COMMSTR                 ! for both CONC and RHOJ
      REAL          RK11  (   NCOLS+1,NROWS+1,NLAYS ) ! initially used as RHOJ
                              ! at x1 cell face, then finally as 11 eddy diff. factor
      REAL          RK22  (   NCOLS+1,NROWS+1,NLAYS ) ! initially used as RHOJ
                              ! at x2 cell face, then finally as 22 eddy diff. factor
      REAL          K11BAR3D ( NCOLS+1,NROWS+1,NLAYS ) ! ave. Cx11 eddy diff
      REAL          K22BAR3D ( NCOLS+1,NROWS+1,NLAYS ) ! ave. Cx22 eddy diff
      REAL          DT                          ! diffusion time step
      REAL          CRHOJ_Q                     ! intermediate, coupled conc.

C Local Variables:

      CHARACTER( 16 ) :: PNAME = 'HDIFF'
      
      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      REAL          DX1                         ! dx1 (meters)
      REAL          DX2                         ! dx2 (meters)
      REAL, SAVE :: RDX1S                       ! reciprocal dx1*dx1
      REAL, SAVE :: RDX2S                       ! reciprocal dx2*dx2
      
      REAL          DTDX1S                      ! dt/dx1**2
      REAL          DTDX2S                      ! dt/dx2**2
      REAL          DTSEC                       ! model time step in seconds
      INTEGER       NSTEPS                      ! diffusion time steps
      INTEGER       STEP                        ! FIX dt
      INTEGER       FDATE                       ! interpolation date
      INTEGER       FTIME                       ! interpolation time

      REAL,    ALLOCATABLE, SAVE :: HALO_SOUTH( :,:,: )
      REAL,    ALLOCATABLE, SAVE :: HALO_NORTH( :,:,: )
      REAL,    ALLOCATABLE, SAVE :: HALO_WEST ( :,:,: )
      REAL,    ALLOCATABLE, SAVE :: HALO_EAST ( :,:,: )
      REAL,    ALLOCATABLE, SAVE :: BUF_NS    ( :,:,: )
      REAL,    ALLOCATABLE, SAVE :: BUF_EW    ( :,:,: )

      INTEGER, ALLOCATABLE, SAVE :: DIFF_MAP( : )   ! global diff map to CGRID

      INTEGER      C, R, L, S, V, N            ! loop counters
      INTEGER      D2C, IOS

      CHARACTER( 96 ) :: XMSG = ' '
     
      INTEGER MY_TEMP
      INTEGER, SAVE :: STARTROW, ENDROW
      INTEGER, SAVE :: STARTCOL, ENDCOL

#ifdef isam
      INTEGER      JSPCTAG
      REAL         SA_CONC ( 0:NCOLS+1,0:NROWS+1 )   ! sa_conc working array
      REAL, ALLOCATABLE, SAVE :: SA_HALO_SOUTH( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SA_HALO_NORTH( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SA_HALO_WEST ( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SA_HALO_EAST ( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SA_BUF_NS    ( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SA_BUF_EW    ( :,:,: )
#endif

#ifdef sens
      REAL, ALLOCATABLE, SAVE :: S_HALO_SOUTH( :,:,:,: ) ! ( NCOLS,NLAYS,N_SPC_DIFF,NPMAX )
      REAL, ALLOCATABLE, SAVE :: S_HALO_NORTH( :,:,:,: ) ! ( NCOLS,NLAYS,N_SPC_DIFF,NPMAX )
      REAL, ALLOCATABLE, SAVE :: S_HALO_WEST ( :,:,:,: ) ! ( NROWS,NLAYS,N_SPC_DIFF,NPMAX )
      REAL, ALLOCATABLE, SAVE :: S_HALO_EAST ( :,:,:,: ) ! ( NROWS,NLAYS,N_SPC_DIFF,NPMAX )
      REAL, ALLOCATABLE, SAVE :: S_BUF_NS    ( :,:,:,: ) ! ( NCOLS,NLAYS,N_SPC_DIFF,NPMAX )
      REAL, ALLOCATABLE, SAVE :: S_BUF_EW    ( :,:,:,: ) ! ( NROWS,NLAYS,N_SPC_DIFF,NPMAX )
      REAL SENSWORK  ( 0:NCOLS+1,0:NROWS+1,NPMAX )   ! sens working array
#endif



      INTERFACE
         SUBROUTINE RHO_J ( JDATE, JTIME, TSTEP, RHOJ )
            INTEGER, INTENT( IN )  :: JDATE, JTIME, TSTEP( 3 )
            REAL,    INTENT( OUT ) :: RHOJ( :,:,: )
         END SUBROUTINE RHO_J
         SUBROUTINE HCDIFF3D ( JDATE, JTIME, K11BAR, K22BAR, DT )
            INTEGER, INTENT( IN )  :: JDATE, JTIME
            REAL,    INTENT( OUT ) :: K11BAR( :,:,: ), K22BAR( :,:,: )
            REAL,    INTENT( OUT ) :: DT
         END SUBROUTINE HCDIFF3D
      END INTERFACE
 
C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

C Get dx1 from COORD include file

         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX1 = DG2M * XCELL_GD
     &          * COS( PI180*( YORIG_GD + YCELL_GD*FLOAT( GL_NROWS/2 ))) ! in m.
            DX2 = DG2M * YCELL_GD   ! in m.
         ELSE
            DX1 = XCELL_GD          ! in m.
            DX2 = YCELL_GD          ! in m.
         END IF

         RDX1S = 1.0 / ( DX1 * DX1 )
         RDX2S = 1.0 / ( DX2 * DX2 )

         N_SPC_DIFF = N_GC_TRNS + N_AE_TRNS + N_NR_TRNS + N_TR_DIFF

         ALLOCATE ( HALO_SOUTH( NCOLS,NLAYS,N_SPC_DIFF ),
     &              HALO_NORTH( NCOLS,NLAYS,N_SPC_DIFF ),
     &              HALO_WEST ( NROWS,NLAYS,N_SPC_DIFF ),
     &              HALO_EAST ( NROWS,NLAYS,N_SPC_DIFF ),
     &              BUF_NS    ( NCOLS,NLAYS,N_SPC_DIFF ),
     &              BUF_EW    ( NROWS,NLAYS,N_SPC_DIFF ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            XMSG = 'Failure allocating HALO_SOUTH, HALO_NORTH, HALO_WEST, HALO_EAST, BUF_NS, or BUF_EW'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF

#ifdef isam
         ALLOCATE ( SA_HALO_SOUTH( NCOLS,NLAYS,N_SPCTAG ),
     &              SA_HALO_NORTH( NCOLS,NLAYS,N_SPCTAG ),
     &              SA_HALO_WEST ( NROWS,NLAYS,N_SPCTAG ),
     &              SA_HALO_EAST ( NROWS,NLAYS,N_SPCTAG ),
     &              SA_BUF_NS    ( NCOLS,NLAYS,N_SPCTAG ),
     &              SA_BUF_EW    ( NROWS,NLAYS,N_SPCTAG ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            XMSG = 'Failure allocating SA_HALO_SOUTH, SA_HALO_NORTH,'
     &           // ' SA_HALO_WEST, SA_HALO_EAST, SA_BUF_NS, or SA_BUF_EW'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF
#endif

#ifdef sens
         ALLOCATE ( S_HALO_SOUTH( NCOLS,NLAYS,N_SPC_DIFF,NPMAX ),
     &              S_HALO_NORTH( NCOLS,NLAYS,N_SPC_DIFF,NPMAX ),
     &              S_HALO_WEST ( NROWS,NLAYS,N_SPC_DIFF,NPMAX ),
     &              S_HALO_EAST ( NROWS,NLAYS,N_SPC_DIFF,NPMAX ),
     &              S_BUF_NS    ( NCOLS,NLAYS,N_SPC_DIFF,NPMAX ),
     &              S_BUF_EW    ( NROWS,NLAYS,N_SPC_DIFF,NPMAX ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            XMSG = 'Failure allocating S_HALO_SOUTH, S_HALO_NORTH,'
     &           // ' S_HALO_WEST, S_HALO_EAST, S_BUF_NS, or S_BUF_EW'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF
#endif


         ALLOCATE ( DIFF_MAP( N_SPC_DIFF ), STAT = IOS )
         IF ( IOS .NE. 0 ) THEN
            XMSG = 'Failure allocating DIFF_MAP'
            CALL M3EXIT ( PNAME, FDATE, FTIME, XMSG, XSTAT1 )
         END IF

C Create global map to CGRID
 
         S = 0
         DO V = 1, N_GC_TRNS
            S = S + 1
            DIFF_MAP( S ) = GC_STRT - 1 + GC_TRNS_MAP( V )
         END DO
         DO V = 1, N_AE_TRNS
            S = S + 1
            DIFF_MAP( S ) = AE_STRT - 1 + AE_TRNS_MAP( V )
         END DO
         DO V = 1, N_NR_TRNS
            S = S + 1
            DIFF_MAP( S ) = NR_STRT - 1 + NR_TRNS_MAP( V )
         END DO
         DO V = 1, N_TR_DIFF
            S = S + 1
            DIFF_MAP( S ) = TR_STRT - 1 + TR_DIFF_MAP( V )
         END DO
 
C Get file start and end indices for subdomain
 
         CALL SUBST_LOOP_INDEX ( 'C', 1, NCOLS, 1, MY_TEMP, STARTCOL, ENDCOL )
         CALL SUBST_LOOP_INDEX ( 'R', 1, NROWS, 1, MY_TEMP, STARTROW, ENDROW )

         WRITE( COMMSTR,'(4I2)' )  1, 0, 2, 0

      END IF                    ! if firstime
                                     
      DTSEC = FLOAT( TIME2SEC( TSTEP( 2 ) ) )
      FDATE = JDATE
      FTIME = JTIME
 
C Get the computational grid ( rho X Jacobian ) for this step

      CALL RHO_J ( FDATE, FTIME, TSTEP, RHOJ )

      CALL SUBST_COMM ( RHOJ, DSPL_N0_E0_S1_W1, DRCN_S_W, COMMSTR )

C initialize RK11, RK22 with face values for RHOJ (assumes dx1 = dx2)

      RK11 = 0.0   ! array assignment
      RK22 = 0.0   ! array assignment
      DO L = 1, NLAYS
         DO R = STARTROW, ENDROW        !  DO R = 1, NROWS + 1
            DO C = STARTCOL, ENDCOL     !     DO C = 1, NCOLS + 1
               RK11( C,R,L ) = 0.5 * ( RHOJ( C,R,L ) + RHOJ( C-1,R,  L ) )
               RK22( C,R,L ) = 0.5 * ( RHOJ( C,R,L ) + RHOJ( C,  R-1,L ) )
            END DO
         END DO
      END DO

C Do the gridded computation for horizontal diffusion

C Get the contravariant eddy diffusivities

      CALL HCDIFF3D ( FDATE, FTIME, K11BAR3D, K22BAR3D, DT )

C get number of steps based on eddy time 
 
      NSTEPS = INT ( DTSEC / DT ) + 1
      DT = DTSEC / FLOAT( NSTEPS )
 
      DTDX1S = DT * RDX1S
      DTDX2S = DT * RDX2S

      DO L = 1, NLAYS
         DO R = STARTROW, ENDROW        !  DO R = 1, NROWS + 1
            DO C = STARTCOL, ENDCOL     !     DO C = 1, NCOLS + 1
               RK11( C,R,L ) = RK11( C,R,L ) * K11BAR3D( C,R,L )
               RK22( C,R,L ) = RK22( C,R,L ) * K22BAR3D( C,R,L )
            END DO
         END DO
      END DO

      CALL SUBST_COMM ( RK11, DSPL_N0_E1_S0_W0, DRCN_E )
      CALL SUBST_COMM ( RK22, DSPL_N1_E0_S0_W0, DRCN_N )

      DO S = 1, N_SPC_DIFF
         D2C = DIFF_MAP( S )
         DO L = 1, NLAYS
            DO C = 1, NCOLS
               HALO_SOUTH( C,L,S ) = CGRID( C,1,L,D2C ) / RHOJ( C,1,L )
               HALO_NORTH( C,L,S ) = CGRID( C,NROWS,L,D2C ) / RHOJ( C,NROWS,L )
               BUF_NS( C,L,S ) = HALO_NORTH( C,L,S )
            END DO
         END DO
      END DO

      CALL SUBST_COMM (HALO_SOUTH, HALO_NORTH, DSPL_N1_E0_S0_W0, DRCN_N)
      CALL SUBST_COMM (BUF_NS,     HALO_SOUTH, DSPL_N0_E0_S1_W0, DRCN_S)

#ifdef sens
      DO NP = 1, NPMAX

        DO S = 1, N_SPC_DIFF
           D2C = DIFF_MAP( S )
           DO L = 1, NLAYS
              DO C = 1, NCOLS
                 S_HALO_SOUTH( C,L,S,NP ) = SENGRID( C,1,L,NP,D2C ) / RHOJ( C,1,L )
                 S_HALO_NORTH( C,L,S,NP ) = SENGRID( C,NROWS,L,NP,D2C ) / RHOJ( C,NROWS,L )
                 S_BUF_NS( C,L,S,NP ) = S_HALO_NORTH( C,L,S,NP )
              END DO
           END DO
        END DO

        CALL SUBST_COMM (S_HALO_SOUTH(:,:,:,NP), S_HALO_NORTH(:,:,:,NP),
     &                   DSPL_N1_E0_S0_W0, DRCN_N)
        CALL SUBST_COMM (S_BUF_NS(:,:,:,NP),     S_HALO_SOUTH(:,:,:,NP),
     &                   DSPL_N0_E0_S1_W0, DRCN_S)

      END DO
#endif

      DO S = 1, N_SPC_DIFF
         D2C = DIFF_MAP( S )
         DO L = 1, NLAYS
            DO R = 1, NROWS
               HALO_WEST( R,L,S ) = CGRID( 1,R,L,D2C ) / RHOJ( 1,R,L )
               HALO_EAST( R,L,S ) = CGRID( NCOLS,R,L,D2C ) / RHOJ( NCOLS,R,L )
               BUF_EW( R,L,S ) = HALO_EAST( R,L,S )
            END DO
         END DO
      END DO

      CALL SUBST_COMM (HALO_WEST, HALO_EAST, DSPL_N0_E1_S0_W0, DRCN_E)
      CALL SUBST_COMM (BUF_EW,    HALO_WEST, DSPL_N0_E0_S0_W1, DRCN_W)

#ifdef sens
      DO NP = 1, NPMAX

        DO S = 1, N_SPC_DIFF
           D2C = DIFF_MAP( S )
           DO L = 1, NLAYS
              DO R = 1, NROWS
                 S_HALO_WEST( R,L,S,NP ) = SENGRID( 1,R,L,NP,D2C ) / RHOJ( 1,R,L )
                 S_HALO_EAST( R,L,S,NP ) = SENGRID( NCOLS,R,L,NP,D2C ) / RHOJ( NCOLS,R,L )
                 S_BUF_EW( R,L,S,NP ) = S_HALO_EAST( R,L,S,NP )
              END DO
           END DO
        END DO

        CALL SUBST_COMM (S_HALO_WEST(:,:,:,NP), S_HALO_EAST(:,:,:,NP),
     &                   DSPL_N0_E1_S0_W0, DRCN_E)
        CALL SUBST_COMM (S_BUF_EW(:,:,:,NP),    S_HALO_WEST(:,:,:,NP),
     &                   DSPL_N0_E0_S0_W1, DRCN_W)

      END DO
#endif

#ifdef isam
      DO JSPCTAG = 1, N_SPCTAG
         DO L = 1, NLAYS
            DO C = 1, NCOLS
               SA_HALO_SOUTH( C,L,JSPCTAG ) =
     &            ISAM( C,1,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) ) / RHOJ( C,1,L )
               SA_HALO_NORTH( C,L,JSPCTAG ) =
     &            ISAM( C,NROWS,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) ) / RHOJ( C,NROWS,L )
               SA_BUF_NS( C,L,JSPCTAG ) = SA_HALO_NORTH( C,L,JSPCTAG )
             END DO
         END DO
      END DO

      CALL SUBST_COMM (SA_HALO_SOUTH, SA_HALO_NORTH, DSPL_N1_E0_S0_W0, DRCN_N)
      CALL SUBST_COMM (SA_BUF_NS,     SA_HALO_SOUTH, DSPL_N0_E0_S1_W0, DRCN_S)

      DO JSPCTAG = 1, N_SPCTAG
         DO L = 1, NLAYS
            DO R = 1, NROWS
               SA_HALO_WEST( R,L,JSPCTAG ) =
     &            ISAM( 1,R,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) ) / RHOJ( 1,R,L )
               SA_HALO_EAST( R,L,JSPCTAG ) =
     &            ISAM( NCOLS,R,L,S_SPCTAG( JSPCTAG ),T_SPCTAG( JSPCTAG ) ) / RHOJ( NCOLS,R,L )
               SA_BUF_EW( R,L,JSPCTAG ) = SA_HALO_EAST( R,L,JSPCTAG )
            END DO
         END DO
      END DO

      CALL SUBST_COMM (SA_HALO_WEST, SA_HALO_EAST, DSPL_N0_E1_S0_W0, DRCN_E)
      CALL SUBST_COMM (SA_BUF_EW,    SA_HALO_WEST, DSPL_N0_E0_S0_W1, DRCN_W)

#endif

C Loop over species, layers, nsteps

      DO 366 S = 1, N_SPC_DIFF
         D2C = DIFF_MAP( S )

         DO 355 L = 1, NLAYS

            DO 344 N = 1, NSTEPS

C Load working array (CGRID is coupled, CONC is mixing ratio)

               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     CONC( C,R ) = CGRID( C,R,L,D2C ) / RHOJ( C,R,L )
                  END DO
               END DO

#ifdef sens
               DO NP = 1, NPMAX
                  DO R = 1, NROWS
                     DO C = 1, NCOLS
                        SENSWORK( C,R,NP ) = SENGRID( C,R,L,NP,D2C ) / RHOJ( C,R,L )
                     END DO
                  END DO
               END DO
#endif

               DO C = 1, NCOLS
                  CONC( C,0 )       = HALO_SOUTH( C,L,S )
                  CONC( C,NROWS+1 ) = HALO_NORTH( C,L,S )
               END DO

               DO R = 1, NROWS
                  CONC( 0,R )       = HALO_WEST( R,L,S )
                  CONC( NCOLS+1,R ) = HALO_EAST( R,L,S )
               END DO

#ifdef sens
               DO NP = 1, NPMAX
                  DO C = 1, NCOLS
                     SENSWORK( C,0,NP ) = S_HALO_SOUTH( C,L,S,NP )
                     SENSWORK( C,NROWS+1,NP ) = S_HALO_NORTH( C,L,S,NP )
                  END DO

                  DO R = 1, NROWS
                     SENSWORK( 0,R,NP ) = S_HALO_WEST( R,L,S,NP )
                     SENSWORK( NCOLS+1,R,NP ) = S_HALO_EAST( R,L,S,NP )
                  END DO
               END DO
#endif

C Update CGRID

               DO R = 1, NROWS
                  DO C = 1, NCOLS

                     CGRID( C,R,L,D2C ) = RHOJ( C,R,L ) * CONC( C,R )
     &                                  + DTDX1S
     &                                  * ( RK11( C+1,R,L )
     &                                  * ( CONC( C+1,R ) - CONC( C,R ) )
     &                                  - RK11( C,R,L )
     &                                    * ( CONC( C,R )   - CONC( C-1,R ) ) )
     &                                  + DTDX2S
     &                                  * ( RK22( C,R+1,L )
     &                                    * ( CONC( C,R+1 ) - CONC( C,R ) )
     &                                    - RK22( C,R,L )
     &                                    * ( CONC( C,R )   - CONC( C,R-1 ) ) )

#ifdef sens
                     DO NP = 1, NPMAX
                        SENGRID( C,R,L,NP,D2C ) = RHOJ( C,R,L ) * SENSWORK( C,R,NP )
     &                                          + DTDX1S
     &                                          * ( RK11( C+1,R,L )
     &                                          * ( SENSWORK( C+1,R,NP ) - SENSWORK( C,R,NP ) )
     &                                          - RK11( C,R,L )
     &                                          * ( SENSWORK( C,R,NP ) - SENSWORK( C-1,R,NP )))
     &                                          + DTDX2S
     &                                          * ( RK22( C,R+1,L )
     &                                          * ( SENSWORK( C,R+1,NP ) - SENSWORK( C,R,NP ) )
     &                                          - RK22( C,R,L )
     &                                          * ( SENSWORK( C,R,NP ) - SENSWORK( C,R-1,NP )))
                     END DO

#endif
                  END DO
               END DO

344         CONTINUE

355      CONTINUE
366   CONTINUE

#ifdef isam
      DO 766 JSPCTAG = 1, N_SPCTAG
        IF( TRANSPORT_SPC( JSPCTAG ) )THEN
          DO 755 L = 1, NLAYS
            DO 744 N = 1, NSTEPS

C Load working array (ISAM is coupled, SA_CONC is mixing ratio)

               DO R = 1, NROWS
                   DO C = 1, NCOLS
                      SA_CONC( C,R ) =
     &                ISAM( C,R,L,S_SPCTAG(JSPCTAG),T_SPCTAG(JSPCTAG) ) / RHOJ( C,R,L )
                   END DO ! C
               END DO ! R

C Fill 4 boundaries of working array SA_CONC: south, north, west, east
               DO C = 1, NCOLS
                  SA_CONC( C,0 )       = SA_HALO_SOUTH( C,L,JSPCTAG )
                  SA_CONC( C,NROWS+1 ) = SA_HALO_NORTH( C,L,JSPCTAG )
               END DO ! C

               DO R = 1, NROWS
                  SA_CONC( 0,R )       = SA_HALO_WEST( R,L,JSPCTAG )
                  SA_CONC( NCOLS+1,R ) = SA_HALO_EAST( R,L,JSPCTAG )
               END DO ! R

C Update ISAM
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     ISAM( C,R,L,S_SPCTAG(JSPCTAG),T_SPCTAG(JSPCTAG) ) =
     &                                RHOJ( C,R,L ) * SA_CONC( C,R )
     &                                + DTDX1S
     &                                * ( RK11( C+1,R,L )
     &                                * ( SA_CONC( C+1,R ) - SA_CONC( C,R ) )
     &                                - RK11( C,R,L )
     &                                * ( SA_CONC( C,R )   - SA_CONC( C-1,R ) ) )
     &                                + DTDX2S
     &                                * ( RK22( C,R+1,L )
     &                                * ( SA_CONC( C,R+1 ) - SA_CONC( C,R ) )
     &                                - RK22( C,R,L )
     &                                * ( SA_CONC( C,R )   - SA_CONC( C,R-1 ) ) )

                  END DO
               END DO

744         CONTINUE
755       CONTINUE
        END IF
766   CONTINUE
#endif


      RETURN

1001  FORMAT( 5X, 'Negative concentrations reset to', 1PE11.3 )
1003  FORMAT( 1X, 4I7, 9X, 1PE11.3)

      END
