
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
      SUBROUTINE HVELOC ( JDATE, JTIME, TSTEP, UWIND, VWIND, HDIV )
      
C-----------------------------------------------------------------------
C Function:
C    This subroutine reads physical velocities in the x1 and x2 directions
C    and returns the contravariant velocities and the horizontal divergence
 
C Preconditions:
C    This routine can be used only for conformal map coordinates 
C    in the horizontal.
C    Dates and times should be represented YYYYDDD:HHMMSS.
 
C Subroutines and functions called:
C    INTERPX, INTERPB, M3EXIT, TIME2SEC, SEC2TIME, NEXTIME
      
C Revision history:
C   January 30, 1996 by Clint L. Ingram at NCSC: created for
C   RADM-coordinates

C   22 Apr 97 Jeff:
C    7 Aug 97 Jeff: for NTHIK = 1
C    4 Feb 98 Jeff: deal with end-of-scenario
C   20 Sep 98 David Wong: parallelized the code
C                         -- adjust the data declaration for DENSJ
C                         -- remove indirect index reference, and re-adapt to
C                            a general case
C                         -- invoke stencil exchange library
C   21 Nov 00 J.Young: PE_COMM3 -> Dave Wong's f90 stenex COMM
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN; replace INTERP3 with INTERPX
C    6 Apr 01 J.Young: Eliminate NTHIN confusion (assumes NTHIK = 1)
C   12 Apr 01 J.Young: Use PINTERPB for boundary data
C   23 Jun 03 J.Young: for layer dependent advection tstep
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   15 Mar 10 J.Young: revert back to 3D arrays and calculate the horiz. div.
C                      (similar to the deform.F code)
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   11 May 11 D.Wong: incorporated twoway model implementation
C   28 Jul 11 David Wong: set REVERT to .false. for twoway model case since
C                         buffered file has only two time steps data
C   01 Feb 19 David Wong: Implemented centralized I/O approach, removed all MY_N
C                         clauses
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE UTILIO_DEFN
#ifndef mpas
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_COMM_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_COMM_MODULE)
#endif
#endif
      USE CENTRALIZED_IO_MODULE, only : interpolate_var, window
!    &                              STRTCOLMC2,  ENDCOLMC2,  STRTROWMC2,  ENDROWMC2


      IMPLICIT NONE

C Includes:
      INCLUDE SUBST_FILES_ID    ! file name parameters
      INCLUDE SUBST_CONST       ! constants
      INCLUDE SUBST_PE_COMM     ! PE communication displacement and direction
 
C Parameters
      
C Arguments:
      INTEGER, INTENT( IN )  :: JDATE        ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN )  :: JTIME        ! current model time, coded HHMMSS
      INTEGER, INTENT( IN )  :: TSTEP        ! time step (HHMMSS)
!     REAL         WIND( NCOLS+1,NROWS+1 ) ! CX xi-velocity 
      REAL,    INTENT( OUT ) :: UWIND( :,:,: )  ! CX xi-velocity 
      REAL,    INTENT( OUT ) :: VWIND( :,:,: )  ! CX yi-velocity 
      REAL,    INTENT( OUT ) :: HDIV ( :,:,: )  ! horizontal divergence
      
C file variables:
!     REAL      DENSJ_BUF( NCOLS,NROWS,NLAYS )     ! Jacobian * air density
!     REAL      DENSJ_BND( NBNDY,NLAYS )           ! bndy Jacobian * air density
!     REAL      DENSJ( 0:NCOLS+1,0:NROWS+1,NLAYS )
      REAL, ALLOCATABLE, SAVE :: DENSJ_BUF( :,:,: )  ! Jacobian * air density
      REAL, ALLOCATABLE, SAVE :: DENSJ_BND( :,: )    ! bndy Jacobian * air density
      REAL, ALLOCATABLE, SAVE :: DENSJ( :,:,: )      ! Jacobian * air density
 
C External Functions:

C local variables:
      
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
       
      INTEGER   MDATE             ! mid-advection date
      INTEGER   MTIME             ! mid-advection time
      INTEGER   STEP              ! advection time step in seconds
      INTEGER, SAVE :: LDATE( 3 ) ! last date for data on file
      INTEGER, SAVE :: LTIME( 3 ) ! last time for data on file
      LOGICAL   REVERT            ! recover last time step if true
      REAL      DJ                ! temporary Jacobian * air density
      REAL       :: DX1, DX2      ! X1 & X2 grid size
      REAL, SAVE :: RDX1, RDX2    ! inverse of DX1 & DX2

      INTEGER   C, C1, R, R1, L   ! induction variables
      INTEGER   BND               ! cell index for constructing density array.
      INTEGER   ALLOCSTAT
 
      CHARACTER( 16 ) :: VNAME
      CHARACTER( 16 ) :: PNAME = 'HVELOC'
      CHARACTER( 16 ) :: AMSG
      CHARACTER( 96 ) :: XMSG = ' '
 
      CHARACTER( 8 ), SAVE :: COMMSTR

!     INTEGER       :: NCOLSDENS, NROWSDENS       ! local for DENSJ_BUF
C for INTERPX
!     INTEGER, SAVE :: STRTCOL,   ENDCOL,   STRTROW,   ENDROW
!     INTEGER       :: STRTCOLMC, ENDCOLMC, STRTROWMC, ENDROWMC
!     INTEGER, SAVE :: STRTCOLMD, ENDCOLMD, STRTROWMD, ENDROWMD

C-----------------------------------------------------------------------

#ifndef mpas
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

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

         WRITE( COMMSTR,'(4I2)' )  1, 1-NTHIK, 2, 1-NTHIK  ! ' 1 0 2 0'

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

         ALLOCATE ( DENSJ( 0:NCOLS+1,0:NROWS+1,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DENSJ'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. WINDOW ) THEN

            ALLOCATE ( DENSJ_BUF( ncols, nrows, NLAYS ),STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating DENSJ_BUF'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( DENSJ_BND( NBNDY,NLAYS ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating DENSJ_BND'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         END IF

      END IF     ! if firstime
 
      MDATE  = JDATE
      MTIME  = JTIME
      STEP   = TIME2SEC( TSTEP )
      CALL NEXTIME( MDATE, MTIME, SEC2TIME( STEP / 2 ) )

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
         WRITE( AMSG,'( 2I8 )' ) LDATE( 1 ), LTIME( 1 )
         XMSG = 'Using data for last file step: ' // TRIM( AMSG )
         CALL M3MESG( XMSG )
         MDATE = LDATE( 1 )
         MTIME = LTIME( 1 )
      END IF
 
C Interpolate Jacobian X Air Density
 
      IF ( WINDOW ) THEN

         call interpolate_var ('DENSA_J', mdate, mtime, DENSJ)

      ELSE  ! need to extend data from bndy file

         call interpolate_var ('DENSA_J', mdate, mtime, DENSJ_BUF)

         call interpolate_var ('DENSA_J', mdate, mtime, DENSJ_BND, 'b')

C Load DENSJ array

         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  DENSJ( C,R,L ) = DENSJ_BUF( C,R,L )
               END DO
            END DO
         END DO

C Fill in DENSJ array for boundaries

         DO L = 1, NLAYS
            BND = 0
            DO R = 0, 0
               DO C = 1, NCOLS+1
                  BND = BND + 1
                  DENSJ( C,R,L ) = DENSJ_BND( BND,L )  ! South
               END DO
            END DO
            DO R = 1, NROWS+1
               DO C = NCOLS+1, NCOLS+1
                  BND = BND + 1
                  DENSJ( C,R,L ) = DENSJ_BND( BND,L )  ! East
               END DO
            END DO
            DO R = NROWS+1, NROWS+1
               DO C = 0, NCOLS
                  BND = BND + 1
                  DENSJ( C,R,L ) = DENSJ_BND( BND,L )  ! North
               END DO
            END DO
            DO R = 0, NROWS
               DO C = 0, 0
                  BND = BND + 1
                  DENSJ( C,R,L ) = DENSJ_BND( BND,L )  ! West
               END DO
            END DO
         END DO

      END IF   ! WINDOW

C Interpolate Contravariant Velocity components (already at flux points)
C X Jacobian X Air Density

      call interpolate_var ('UHAT_JD', mdate, mtime, UWIND)

      call interpolate_var ('VHAT_JD', mdate, mtime, VWIND)

C Obtain flux point values of Jacobian * air density and retrieve
C contravariant velocities 

C store actual north, east, south, and west displacement
C define communication pattern

      CALL SUBST_COMM ( DENSJ, DSPL_N0_E0_S0_W1, DRCN_W, COMMSTR )
      DO L = 1, NLAYS
         DO R = 1, NROWS
            DO C = 1, NCOLS+1
               DJ = 0.5 * ( DENSJ( C,R,L ) + DENSJ( C-1,R,L ) )
               UWIND( C,R,L ) = UWIND( C,R,L ) / DJ
            END DO
         END DO
      END DO

C store actual north, east, south, and west displacement
C define communication pattern

      CALL SUBST_COMM ( DENSJ, DSPL_N0_E0_S1_W0, DRCN_S, COMMSTR )
      DO L = 1, NLAYS
         DO R = 1, NROWS+1
            DO C = 1, NCOLS
               DJ = 0.5 * ( DENSJ( C,R,L ) + DENSJ( C,R-1,L ) )
               VWIND( C,R,L ) = VWIND( C,R,L ) / DJ
            END DO
         END DO
      END DO

C Compute horizontal divergence

      CALL SUBST_COMM ( UWIND, DSPL_N0_E1_S0_W0, DRCN_E_W )
      CALL SUBST_COMM ( VWIND, DSPL_N1_E0_S0_W0, DRCN_N_S )

      DO L = 1, NLAYS
         DO R = 1, NROWS
            R1 = R + 1
            DO C = 1, NCOLS
               C1 = C + 1
               HDIV( C,R,L ) = ( UWIND( C1,R,L ) - UWIND( C,R,L ) ) * RDX1
     &                       + ( VWIND( C,R1,L ) - VWIND( C,R,L ) ) * RDX2
            END DO
         END DO
      END DO
#endif

      RETURN
      END
