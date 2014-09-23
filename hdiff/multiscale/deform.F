
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
C $Header: /project/yoj/arc/CCTM/src/hdiff/multiscale/deform.F,v 1.5 2011/10/21 16:11:22 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE DEFORM ( JDATE, JTIME, DEFORM3D )
      
C-----------------------------------------------------------------------
C Function:
C    Computes wind deformation based on the contravariant horizontal
C    velocity components.
 
C Preconditions:
C    This routine can be used only for conformal map coordinates 
C    in the horizontal.
C    Dates and times should be represented YYYYDDD:HHMMSS.
 
C Subroutines and functions called:
C    INTERP3, M3EXIT, TIME2SEC, SEC2TIME, NEXTIME
      
C Revision history:
C
C    Oct 10, 2000  Initial development (code adapted from hcontvel.F)
C      Daewon Byun and Avi Lacser

C    26 Dec 00 J.Young: GLOBAL_RMAX -> Dave Wong's f90 stenex GLOBAL_MAX
C                       PE_COMM3 -> Dave Wong's f90 stenex COMM
C
C    11 Jan 01 David Wong: -- Introduced two new local variable LOC_UWIND and
C                             LOC_VWIND: because of INTERP3, The file buffer
C                             UWIND, neccessarily does not have dimensions for
C                             a row ghost region. the same is true for VWIND
C                             with respect to a column ghost region.
C                          -- invoked SE_LOOP_INDEX to compute correct loop
C                             index for the local processor
C                          -- corrected communication pattern for DENSJ
C     7 Aug 01 J.Young: dyn alloc - Use HGRD_DEFN; replace INTERP3 with INTERPX
C                       and INTERPB; allocatable arrays
C                       Not developed for other than NTHIK = 1
C    25 MAr 04 G.Hammond: move wind velocity ghost cell updates outside layer
C                         loop. Use SNL "swap3d".
C    31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                       domain specifications in one module
C    16 Feb 11 S. Roselle: replaced I/O-API include files w/UTILIO_DEFN
C    11 May 11 D.Wong: incorporated twoway model implementation
C    28 Jul 11 David Wong: set REVERT to .false. for twoway model case since
C                          buffered file has only two time steps data
C-----------------------------------------------------------------------
      
      USE GRID_CONF            ! horizontal & vertical domain specifications
      USE UTILIO_DEFN
#ifdef parallel
      USE SE_MODULES           ! stenex (using SE_GLOBAL_MAX_MODULE, SE_COMM_MODULE)
      USE SWAP_SANDIA
#else
      USE NOOP_MODULES         ! stenex (using NOOP_GLOBAL_MAX_MODULE, NOOP_COMM_MODULE)
#endif

      IMPLICIT NONE

C Includes:

      INCLUDE SUBST_FILES_ID   ! file name parameters
      INCLUDE SUBST_CONST      ! constants
      INCLUDE SUBST_PE_COMM    ! PE communication displacement and direction
 
C Parameters:

C Arguments:
      
      INTEGER, INTENT( IN )  :: JDATE             ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN )  :: JTIME             ! current model time, coded HHMMSS
      REAL,    INTENT( OUT ) :: DEFORM3D( :,:,: ) ! Wind deformation
      
C Parameters:

C file variables:
      
      REAL      DENSJ_BUF( NCOLS,NROWS,NLAYS ) ! Jacobian * air density
      REAL      DENSJ_BND( NBNDY,NLAYS )       ! boundary Jacobian * air density
 
C External Functions: None
      
C local variables:
      
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, SAVE :: LOGDEV

      INTEGER, SAVE :: MLAYS
       
      INTEGER   ROW               ! Row index
      INTEGER   COL               ! Column index
      INTEGER   LVL               ! Layer index
      INTEGER   MDATE             ! mid-advection date
      INTEGER   MTIME             ! mid-advection time
!     INTEGER   STEP              ! advection time step in seconds
      INTEGER, SAVE :: LDATE( 3 ) ! last date for data on file
      INTEGER, SAVE :: LTIME( 3 ) ! last time for data on file
      LOGICAL   REVERT            ! recover last time step if true
      REAL      DJ                ! temporary Jacobian * air density
 
      CHARACTER( 16 ) :: VNAME
      CHARACTER( 16 ) :: PNAME = 'DEFORM'
      CHARACTER( 16 ) :: AMSG
      CHARACTER( 96 ) :: XMSG = ' '
 
C Jacobian * air density
      REAL         DENSJ    ( 0:NCOLS+1,0:NROWS+1,NLAYS )

      REAL         UWIND    (   NCOLS+1,  NROWS+1,NLAYS ) ! ContrVar x1-velocity 
      REAL         LOC_UWIND(   NCOLS+1,0:NROWS+1,NLAYS ) ! local CV x1-velocity 
      REAL         VWIND    (   NCOLS+1,  NROWS+1,NLAYS ) ! ContrVar x2-velocity 
      REAL         LOC_VWIND( 0:NCOLS+1,  NROWS+1,NLAYS ) ! local CV x2-velocity 
      REAL         DUDX     (   NCOLS,    NROWS )
      REAL         DUDY     (   NCOLS,    NROWS )
      REAL         DVDX     (   NCOLS,    NROWS )
      REAL         DVDY     (   NCOLS,    NROWS )
      CHARACTER( 8 ), SAVE :: COMMSTR

      REAL, SAVE :: DX1, DX2       ! X1 & X2 grid size
      REAL, SAVE :: RDX1, RDX2     ! inverse of DX1 & DX2
!     REAL, SAVE :: RDX1O2, RDX2O2 ! half of inverse of DX1 & DX2
      REAL, SAVE :: RDX1O4, RDX2O4 ! quarter of inverse of DX1 & DX2
      REAL    UBAR1, UBAR2         ! U average at X point (Avi)
      REAL    VBAR1, VBAR2         ! V average at X point (Avi)
      REAL    DF1, DF2             ! deformation components
      INTEGER C, R, L              ! notations for COL, ROW, LVL
      INTEGER C1, R1               ! C1 = C+1, R1 = R+1  (Avi)
      INTEGER C2, R2               ! C2 = C-1, R2 = R-1  (Avi)
!     INTEGER C1, R1               ! C1 = MAX(1, C-1), R1 = MAX(1, R-1) (Daewon)
!     INTEGER C2, R2               ! C2 = MIN(C+1, NCOLS), R2 = MIN(R+1, NROWS) (DBX)
      INTEGER COUNT                ! Counter for constructing density array.
      REAL    DEFMAX               ! max deformation (dianostic)

      INTEGER MY_TEMP
      INTEGER, SAVE :: FRSTROW, LASTROW, FRSTCOL, LASTCOL

      INTEGER   GXOFF, GYOFF            ! global origin offset from file
      LOGICAL, SAVE :: WINDOW = .FALSE. ! posit same file and global
                                        ! processing domain
      INTEGER, SAVE :: NCOLSDENS, NROWSDENS       ! local for DENSJ_BUF
C for INTERPX
      INTEGER, SAVE :: STRTCOL,   ENDCOL,   STRTROW,   ENDROW
      INTEGER       :: STRTCOLMC, ENDCOLMC, STRTROWMC, ENDROWMC
      INTEGER, SAVE :: STRTCOLMD, ENDCOLMD, STRTROWMD, ENDROWMD

C-----------------------------------------------------------------------
 
      IF ( FIRSTIME ) THEN
 
         FIRSTIME = .FALSE.
         LOGDEV = INIT3()

         MLAYS = SIZE ( DEFORM3D,3 )
 
         CALL LSTEPF( MET_CRO_3D, LDATE( 1 ), LTIME( 1 ) )
!        CALL LSTEPF( MET_BDY_3D, LDATE( 2 ), LTIME( 2 ) )
         CALL LSTEPF( MET_DOT_3D, LDATE( 3 ), LTIME( 3 ) )
 
!        LDATE( 1 ) = MIN( LDATE( 1 ), LDATE( 2 ), LDATE( 3 ) )
!        LTIME( 1 ) = SEC2TIME( MIN(
!    &                         TIME2SEC( LTIME( 1 ) ),
!    &                         TIME2SEC( LTIME( 2 ) ),
!    &                         TIME2SEC( LTIME( 3 ) )
!    &                         ) )

         LDATE( 1 ) = MIN( LDATE( 1 ), LDATE( 3 ) )
         LTIME( 1 ) = SEC2TIME( MIN(
     &                         TIME2SEC( LTIME( 1 ) ),
     &                         TIME2SEC( LTIME( 3 ) )
     &                         ) )

         WRITE( COMMSTR,'(4I2)' )  1, 0, 2, 0

C Get/compute DX1 & DX2
 
         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX1 = DG2M * XCELL_GD ! in m.
            DX2 = DG2M * YCELL_GD *
     &         COS( PI180*( YORIG_GD + YCELL_GD * FLOAT( GL_NROWS/2 ))) !in m
         ELSE
            DX1 = XCELL_GD        ! in m.
            DX2 = YCELL_GD        ! in m.
         END IF                                                              

         RDX1 = 1.0 / DX1
         RDX2 = 1.0 / DX2
!        RDX1O2 = 0.5 / DX1
!        RDX2O2 = 0.5 / DX2
         RDX1O4 = 0.25 / DX1
         RDX2O4 = 0.25 / DX2

         CALL SUBST_LOOP_INDEX ( 'R', 2, MY_NROWS, -1, MY_TEMP,
     &                           FRSTROW, LASTROW )
         CALL SUBST_LOOP_INDEX ( 'C', 2, MY_NCOLS, -1, MY_TEMP, 
     &                           FRSTCOL, LASTCOL )

         CALL SUBHFILE ( MET_DOT_3D, GXOFF, GYOFF,
     &                   STRTCOLMD, ENDCOLMD, STRTROWMD, ENDROWMD )
         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC, ENDCOLMC, STRTROWMC, ENDROWMC )

         NCOLSDENS = ENDCOLMC - STRTCOLMC + 1
         NROWSDENS = ENDROWMC - STRTROWMC + 1

         IF ( NCOLSDENS .NE. MY_NCOLS .OR.
     &        NROWSDENS .NE. MY_NROWS ) THEN
            WRITE( XMSG,'( A, 4I8 )' ) 'Local Columns or Rows incorrect',
     &      NCOLSDENS, MY_NCOLS, NROWSDENS, MY_NROWS
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C currently not implemented: case where only one origin component matches file's
         IF ( GXOFF .NE. 0 .AND. GYOFF .NE. 0 ) THEN
            WINDOW = .TRUE. ! windowing from file
            STRTCOL = STRTCOLMC - 1
            ENDCOL  = ENDCOLMC  + 1
            STRTROW = STRTROWMC - 1
            ENDROW  = ENDROWMC  + 1
         ELSE
            STRTCOL = STRTCOLMC
            ENDCOL  = ENDCOLMC
            STRTROW = STRTROWMC
            ENDROW  = ENDROWMC
         END IF

      END IF  ! if firstime
 
      MDATE  = JDATE
      MTIME  = JTIME
!     STEP   = TIME2SEC( TSTEP )
!     CALL NEXTIME( MDATE, MTIME, SEC2TIME( STEP / 2 ) )

#ifdef twoway
      REVERT = .FALSE.
#else
      IF ( MDATE .LT. LDATE( 1 ) ) THEN
         REVERT = .FALSE.
      ELSE IF ( MDATE .EQ. LDATE( 1 ) ) THEN
         IF ( MTIME .LE. LTIME( 1 ) ) THEN
            REVERT = .FALSE.
         ELSE
            REVERT = .TRUE.
         END IF
      ELSE   ! MDATE .GT. LDATE
         REVERT = .TRUE.
      END IF
#endif
 
      IF ( REVERT ) THEN
         XMSG = 'Current scenario interpolation step not available in all of '
     &        // TRIM( MET_CRO_3D ) // ', '
     &        // TRIM( MET_BDY_3D ) // ' and '
     &        // TRIM( MET_DOT_3D )
         CALL M3MESG( XMSG )
!        CALL NEXTIME( MDATE, MTIME, -SEC2TIME( STEP / 2 ) )
         WRITE( AMSG,'( 2I8 )' ) LDATE( 1 ), LTIME( 1 )
         XMSG = 'Using data for last file step: ' // AMSG
         CALL M3MESG( XMSG )
         MDATE = LDATE( 1 )
         MTIME = LTIME( 1 )
      END IF
 
C Interpolate Jacobian X Air Density

      IF ( WINDOW ) THEN

         VNAME = 'DENSA_J'
         IF ( .NOT. INTERPX ( MET_CRO_3D, VNAME, PNAME,
     &                        STRTCOL,ENDCOL, STRTROW,ENDROW, 1,MLAYS,
     &                        MDATE, MTIME, DENSJ ) ) THEN
            XMSG = 'Could not read ' // VNAME // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

      ELSE ! need to extend data from bndy file

         VNAME = 'DENSA_J'
         IF ( .NOT. INTERPX ( MET_CRO_3D, VNAME, PNAME,
     &                        STRTCOL,ENDCOL, STRTROW,ENDROW, 1,NLAYS,
     &                        MDATE, MTIME, DENSJ_BUF ) ) THEN
            XMSG = 'Could not read ' // VNAME // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'DENSA_J'
         IF ( .NOT. INTERPB ( MET_BDY_3D, VNAME, PNAME,
     &                        MDATE, MTIME, NBNDY*MLAYS,
     &                        DENSJ_BND ) ) THEN
            XMSG = 'Could not read ' // VNAME // ' from ' // MET_BDY_3D
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

C Load DENSJ array

         DO LVL = 1, MLAYS
            DO ROW = 1, MY_NROWS
               DO COL = 1, MY_NCOLS
                  DENSJ( COL,ROW,LVL ) = DENSJ_BUF( COL,ROW,LVL )
               END DO
            END DO
         END DO

C Fill in DENSJ array for boundaries

         DO LVL = 1, MLAYS
            COUNT = 0
            DO ROW = 0, 0
               DO COL = 1, MY_NCOLS + 1
                  COUNT = COUNT + 1
                  DENSJ( COL,ROW,LVL ) = DENSJ_BND( COUNT,LVL )  ! South
               END DO
            END DO
            DO ROW = 1, MY_NROWS + 1
               DO COL = MY_NCOLS + 1, MY_NCOLS + 1
                  COUNT = COUNT + 1
                  DENSJ( COL,ROW,LVL ) = DENSJ_BND( COUNT,LVL )  ! East
               END DO
            END DO
            DO ROW = MY_NROWS + 1, MY_NROWS + 1
               DO COL = 0, MY_NCOLS
                  COUNT = COUNT + 1
                  DENSJ( COL,ROW,LVL ) = DENSJ_BND( COUNT,LVL )  ! North
               END DO
            END DO
            DO ROW = 0, MY_NROWS
               DO COL = 0, 0
                  COUNT = COUNT + 1
                  DENSJ( COL,ROW,LVL ) = DENSJ_BND( COUNT,LVL )  ! West
               END DO
            END DO
         END DO

      END IF   ! WINDOW
 
C Interpolate Contravariant Velocity components (already at flux points)
C X Jacobian X Air Density

      VNAME = 'UHAT_JD'   ! x1 component of CV-vel * Jacobian * air density
      IF ( .NOT. INTERPX ( MET_DOT_3D, VNAME, PNAME,
     &                     STRTCOLMD,ENDCOLMD, STRTROWMD,ENDROWMD, 1,MLAYS,
     &                     MDATE, MTIME, UWIND ) ) THEN
         XMSG = 'Could not read ' // VNAME // ' from ' // MET_DOT_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

      VNAME = 'VHAT_JD'   ! x2 component of CV-vel * Jacobian * air density
      IF ( .NOT. INTERPX ( MET_DOT_3D, VNAME, PNAME,
     &                     STRTCOLMD,ENDCOLMD, STRTROWMD,ENDROWMD, 1,MLAYS,
     &                     MDATE, MTIME, VWIND ) ) THEN
         XMSG = 'Could not read ' // VNAME // ' from ' // MET_DOT_3D
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

C Obtain flux point values of Jacobian * air density and retrieve
C contravariant velocities 

C create U/RhoJ - update ghost regions for RhoJ

      CALL SUBST_COMM ( DENSJ, DSPL_N0_E1_S0_W1, DRCN_E_W, COMMSTR )

      LOC_UWIND = 0.0   ! array assignment
      DO LVL = 1, MLAYS
         DO ROW = 1, MY_NROWS
            DO COL = 1, MY_NCOLS + 1
               DJ = 0.5*( DENSJ( COL,ROW,LVL ) + DENSJ( COL-1,ROW,LVL ) )
               LOC_UWIND( COL,ROW,LVL ) = UWIND( COL,ROW,LVL ) / DJ
            END DO
         END DO
      END DO

C create V/RhoJ - update ghost regions for RhoJ

      CALL SUBST_COMM ( DENSJ, DSPL_N1_E0_S1_W0, DRCN_N_S, COMMSTR )

      LOC_VWIND = 0.0   ! array assignment
      DO LVL = 1, MLAYS
         DO ROW = 1, MY_NROWS + 1
            DO COL = 1, MY_NCOLS
               DJ = 0.5*( DENSJ( COL,ROW,LVL ) + DENSJ( COL,ROW-1,LVL ) )
               LOC_VWIND( COL,ROW,LVL ) = VWIND( COL,ROW,LVL ) / DJ
            END DO
         END DO
      END DO

C Compute wind deformation
 
C initialize deformation arrays
C deformation at all boundary cells are defined to be zero
      DO L = 1, MLAYS
         DO R = 1, MY_NROWS + 1
            DO C = 1, MY_NCOLS + 1
               DEFORM3D( C,R,L ) = 0.0
            END DO
         END DO
      END DO

#ifdef parallel
      CALL SWAP3D( LOC_UWIND( 1,1,1 ), LOC_UWIND( 1,NROWS+1,1 ),
     &             NCOLS+1, 1, NLAYS, NCOLS+1, NROWS+2, NORTH )
      CALL SWAP3D( LOC_UWIND( 1,NROWS,1 ), LOC_UWIND( 1,0,1 ),
     &             NCOLS+1, 1, NLAYS, NCOLS+1, NROWS+2, SOUTH )
      CALL SWAP3D( LOC_UWIND( 1,0,1 ), LOC_UWIND( NCOLS+1,0,1 ),
     &             1, NROWS+2, NLAYS, NCOLS+1, NROWS+2, EAST )

      CALL SWAP3D( LOC_VWIND( 1,1,1 ), LOC_VWIND( NCOLS+1,1,1 ),
     &             1, NROWS+1, NLAYS, NCOLS+2, NROWS+1, EAST )
      CALL SWAP3D( LOC_VWIND( NCOLS,1,1 ), LOC_VWIND( 0,1,1 ),
     &             1, NROWS+1, NLAYS, NCOLS+2, NROWS+1, WEST )
      CALL SWAP3D( LOC_VWIND( 0,1,1 ), LOC_VWIND( 0,NROWS+1,1 ),
     &             NCOLS+2, 1, NLAYS, NCOLS+2, NROWS+1, NORTH )
#endif

      DO 101 L = 1, MLAYS

         DEFMAX = 0.0
C initialize wind shear components (inner domain only dimensioned)
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               DUDX( C,R ) = 0.0
               DUDY( C,R ) = 0.0
               DVDX( C,R ) = 0.0
               DVDY( C,R ) = 0.0
            END DO
         END DO

C ORIGINAL by Daewon October 2000
C Compute gradients only at inner domain
!        DO R = 1, MY_NROWS
!           DO C = 1, MY_NCOLS
!              C1 = MAX( 1,C-1 )
!              R1 = MAX( 1,R-1 )
!              C2 = MIN( C+1,MY_NCOLS )
!              R2 = MIN( R+1,MY_NROWS )

!              DUDX( C,R ) = ( UWIND( C,R,L )  - UWIND( C1,R,L ) ) * RDX1
!              DUDY( C,R ) = ( UWIND( C,R2,L ) - UWIND( C,R1,L ) ) * RDX2O2
!              DVDX( C,R ) = ( VWIND( C2,R,L ) - VWIND( C1,R,L ) ) * RDX1O2
!              DVDY( C,R ) = ( VWIND( C,R,L )  - VWIND( C,R1,L ) ) * RDX2
!           END DO
!        END DO

C SUGGESTED by Avi October 2000
C for whole domain (DUDX, DVDY)
         DO R = 1, MY_NROWS
            R1 = R + 1
            DO C = 1, MY_NCOLS
               C1 = C + 1
               DUDX(C,R) = ( LOC_UWIND( C1,R,L ) - LOC_UWIND( C,R,L ) ) * RDX1
               DVDY(C,R) = ( LOC_VWIND( C,R1,L ) - LOC_VWIND( C,R,L ) ) * RDX2
            END DO
         END DO

!        WRITE( LOGDEV,1003 )
1003     FORMAT( / '@1@Layer', 4X, 'Max Deform',
     &             5X, 'DUDX(4,5)',
     &             5X, 'DUDY(4,5)',
     &             5X, 'DVDX(4,5)',
     &             5X, 'DVDY(4,5)' )

#ifndef parallel
         CALL SUBST_COMM ( LOC_UWIND, DSPL_N1_E1_S1_W0, DRCN_N_NE_SE_S, '2 0' )
#endif

C for DUDY inside domain (compute the gradient of the averages)
         DO R = FRSTROW, LASTROW
            R1 = R + 1
            R2 = R - 1
            DO C = 1, MY_NCOLS
               C1 = C + 1
               UBAR1 = LOC_UWIND( C,R1,L ) + LOC_UWIND( C1,R1,L )
               UBAR2 = LOC_UWIND( C,R2,L ) + LOC_UWIND( C1,R2,L )
               DUDY(C,R) = ( UBAR1 - UBAR2 ) * RDX2O4
               END DO
            END DO
 
#ifndef parallel
         CALL SUBST_COMM ( LOC_VWIND, DSPL_N1_E1_S0_W1, DRCN_NE_E_W_NW, '1 0' )
#endif

C for DVDX inner domain (compute the gradient of the averages)
         DO R = 1, MY_NROWS
            R1 = R + 1
            DO C = FRSTCOL, LASTCOL
               C1 = C + 1
               C2 = C - 1
               VBAR1 = LOC_VWIND( C1,R1,L ) + LOC_VWIND( C1,R,L )
               VBAR2 = LOC_VWIND( C2,R1,L ) + LOC_VWIND( C2,R,L )
               DVDX(C,R) = ( VBAR1 - VBAR2 ) * RDX1O4
               END DO
            END DO

C DUDY = 0 for R=1 and MY_NROWS for all MY_NCOLS
C DVDX = 0 for C=1 and MY_NCOLS for all MY_NROWS

C END of section done by Avi

C Deformation only at inner domain
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               DF1 = DUDX( C,R ) - DVDY( C,R )
               DF2 = DVDX( C,R ) + DUDY( C,R )
               DEFORM3D( C,R,L ) = SQRT( DF1 * DF1 + DF2 * DF2 ) 
               DEFMAX = MAX( DEFMAX, DEFORM3D( C,R,L ) )
               END DO
            END DO
!        WRITE( LOGDEV,1005 ) L, SUBST_GLOBAL_MAX ( DEFMAX ), 
!    &                        DUDX(4,5), DUDY(4,5), DVDX(4,5), DVDY(4,5)
1005     FORMAT( '@1@ ', I3, 2X, 5( 1PE14.6 ) )

101   CONTINUE   ! MLAYS

      RETURN
      END
