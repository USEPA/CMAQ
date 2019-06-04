
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
      SUBROUTINE HCONTVEL ( JDATE, JTIME, TSTEP, LVL, UORV, WIND )
      
C-----------------------------------------------------------------------
C Function:
C    This subroutine reads physical velocities in the x1 or x2 directions
C    and returns the contravariant velocities.
 
C Preconditions:
C    This routine can be used only for conformal map coordinates 
C    in the horizontal.
C    Dates and times should be represented YYYYDDD:HHMMSS.
C    Must call for U-Winds first (UORV = UFLAG)
 
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
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                      removed deprecated TRIMLEN
C   11 May 11 D.Wong: incorporated twoway model implementation
C   28 Jul 11 David Wong: set REVERT to .false. for twoway model case since
C                         buffered file has only two time steps data
C   01 Feb 19 David Wong: Implemented centralized I/O approach, removed all MY_N
C                         clauses
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE UTILIO_DEFN
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_COMM_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_COMM_MODULE)
#endif
      use CENTRALIZED_IO_MODULE, only : interpolate_var, window

      IMPLICIT NONE

C Includes:

      INCLUDE SUBST_FILES_ID    ! file name parameters
      INCLUDE SUBST_PE_COMM     ! PE communication displacement and direction
 
C Arguments:
      
      INTEGER, INTENT(  IN ) :: JDATE        ! current model date, coded YYYYDDD
      INTEGER, INTENT(  IN ) :: JTIME        ! current model time, coded HHMMSS
      INTEGER, INTENT(  IN ) :: TSTEP        ! time step (HHMMSS)
      INTEGER, INTENT(  IN ) :: LVL          ! layer
      CHARACTER( 16 ), INTENT(  IN ) :: UORV ! flag for velocity component
!     REAL         WIND( NCOLS+1,NROWS+1 )
      REAL,    INTENT( OUT ) :: WIND( :,: )  ! CX xi-velocity 
      
C Parameters:

      CHARACTER( 16 ), PARAMETER :: UFLAG = 'X1VEL'
      CHARACTER( 16 ), PARAMETER :: VFLAG = 'X2VEL'

C file variables:
      
!     REAL      DENSJ_BUF( NCOLS,NROWS,NLAYS )     ! Jacobian * air density
      REAL, ALLOCATABLE, SAVE :: DENSJ_BUF( :,:,: ) ! Jacobian * air density
!     REAL, ALLOCATABLE, SAVE :: DENSJ_BUF( :,: )   ! Jacobian * air density
!     REAL      DENSJ_BND( NBNDY,NLAYS )           ! bndy Jacobian * air density
      REAL, ALLOCATABLE, SAVE :: DENSJ_BND( :,: )  ! bndy Jacobian * air density
!     REAL    DENSJ( 0:NCOLS+1,0:NROWS+1,NLAYS )
      REAL, ALLOCATABLE, SAVE :: DENSJ( :,: )      ! Jacobian * air density

C External Functions: None
      
C local variables:
      
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
       
      INTEGER   ROW               ! Row index
      INTEGER   COL               ! Column index
      INTEGER   MDATE             ! mid-advection date
      INTEGER   MTIME             ! mid-advection time
      INTEGER, SAVE :: LDATE( 3 ) ! last date for data on file
      INTEGER, SAVE :: LTIME( 3 ) ! last time for data on file
      LOGICAL   REVERT            ! recover last time step if true
      INTEGER   STEP              ! advection time step in seconds
      REAL      DJ                ! temporary Jacobian * air density
      INTEGER   ESTAT
 
      CHARACTER( 16 ) :: VNAME
      CHARACTER( 16 ) :: PNAME = 'HCONTVEL'
      CHARACTER( 16 ) :: AMSG
      CHARACTER( 14 ) :: MSG1 = 'Error reading '
      CHARACTER( 96 ) :: XMSG = ' '
 
      CHARACTER( 8 ), SAVE :: COMMSTR

      INTEGER COUNT     ! Counter for constructing density array.

      LOGICAL, SAVE :: CSTAGUV = .TRUE.      ! Winds are available on C staggered grid?

      integer, save :: old_time = -9

C-----------------------------------------------------------------------

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

         VNAME3D = ' '   ! array assignment
         IF ( .NOT. DESC3( MET_DOT_3D ) ) THEN
            XMSG = 'Could not get '
     &           // TRIM( MET_DOT_3D ) // ' file description'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'UWINDC'
         IF ( INDEX1( VNAME, NVARS3D, VNAME3D ) .LE. 0 ) THEN
            XMSG = 'Could not find ' // VNAME // ' in ' // MET_DOT_3D
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            CSTAGUV = .FALSE.
         END IF

         IF ( .NOT. CSTAGUV ) THEN

            ALLOCATE ( DENSJ( 0:NCOLS+1,0:NROWS+1 ), STAT = ESTAT )
            IF ( ESTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating DENSJ'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            IF ( .NOT. WINDOW ) THEN
               ALLOCATE ( DENSJ_BUF( ncols,nrows,NLAYS ),STAT = ESTAT )
!              ALLOCATE ( DENSJ_BUF( NCOLSDENS,NROWSDENS ),      STAT = ESTAT )
               IF ( ESTAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating DENSJ_BUF'
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
               ALLOCATE ( DENSJ_BND( NBNDY,NLAYS ), STAT = ESTAT )
               IF ( ESTAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating DENSJ_BND'
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
            END IF

         END IF

      END IF   ! if firstime
 
      MDATE = JDATE
      MTIME = JTIME
      STEP  = TIME2SEC( TSTEP )
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
         XMSG = 'Using data for last file step: ' // AMSG
         CALL M3MESG( XMSG )
         MDATE = LDATE( 1 )
         MTIME = LTIME( 1 )
      END IF

      IF ( LVL .GT. NLAYS ) THEN
         WRITE( XMSG,'( "layer", I4, " greater than NLAYS" )') LVL
         CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      END IF

C Try to read u-component wind on C-staggered grid from variable UWINDC.
C (First available in MCIPv3.5, Fall 2009.)  If UWINDC is not in MET_DOT_3D,
C try to read u-component wind on B-staggered grid as UWIND.

      IF ( UORV .EQ. UFLAG ) THEN

         IF ( CSTAGUV ) THEN
            call interpolate_var ('UWINDC', mdate, mtime, WIND, SLAY=LVL)

            RETURN
         END IF

      ELSE IF ( UORV .EQ. VFLAG ) THEN

C If u-component wind was C-staggered, read v-component wind on C-staggered
C grid.  Otherwise, read v-component wind from B-staggered grid.

         IF ( CSTAGUV ) THEN
            call interpolate_var ('VWINDC', mdate, mtime, WIND, SLAY=LVL)

            RETURN
         END IF

      ELSE

         XMSG = 'UORV Flag, not set correctly'
         CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT2 )

      END IF   ! if UORV

C Interpolate Jacobian X Air Density
 
      VNAME = 'DENSA_J'
      IF ( WINDOW ) THEN

         call interpolate_var ('DENSA_J', mdate, mtime, DENSJ, slay=lvl)

      ELSE ! need to extend data from bndy file

         IF (old_time .ne. mtime) THEN
           call interpolate_var (VNAME, mdate, mtime, DENSJ_BUF)
           old_time = mtime

           call interpolate_var (VNAME, mdate, mtime, DENSJ_BND, 'b')
         END IF

C Load DENSJ array

         DO ROW = 1, NROWS
            DO COL = 1, NCOLS
               DENSJ( COL,ROW ) = DENSJ_BUF( COL,ROW,LVL )
            END DO
         END DO

C Fill in DENSJ array for boundaries

         COUNT = 0
         DO ROW = 0, 0
            DO COL = 1, NCOLS+1
               COUNT = COUNT + 1
               DENSJ( COL,ROW ) = DENSJ_BND( COUNT,LVL )  ! South
            END DO
         END DO
         DO ROW = 1, NROWS+1
            DO COL = NCOLS+1, NCOLS+1
               COUNT = COUNT + 1
               DENSJ( COL,ROW ) = DENSJ_BND( COUNT,LVL )  ! East
            END DO
         END DO
         DO ROW = NROWS+1, NROWS+1
            DO COL = 0, NCOLS
               COUNT = COUNT + 1
               DENSJ( COL,ROW ) = DENSJ_BND( COUNT,LVL )  ! North
            END DO
         END DO
         DO ROW = 0, NROWS
            DO COL = 0, 0
               COUNT = COUNT + 1
               DENSJ( COL,ROW ) = DENSJ_BND( COUNT,LVL )  ! West
            END DO
         END DO

      END IF   ! WINDOW

C Interpolate Contravariant Velocity components (already at flux points)
C X Jacobian X Air Density

      IF ( UORV .EQ. UFLAG ) THEN

         call interpolate_var ('UHAT_JD', mdate, mtime, WIND, SLAY=LVL)

C get west-direction RhoJ in halo cells and retrieve contravariant velocities

         CALL SUBST_COMM ( DENSJ, DSPL_N0_E0_S0_W1, DRCN_W, COMMSTR )
         DO ROW = 1, NROWS
            DO COL = 1, NCOLS+1
               DJ = 0.5 * ( DENSJ( COL,ROW) + DENSJ( COL-1,ROW ) )
               WIND( COL,ROW ) = WIND( COL,ROW ) / DJ
            END DO
         END DO

      ELSE

         call interpolate_var ('VHAT_JD', mdate, mtime, WIND, SLAY=LVL)

C get south-direction RhoJ in halo cells and retrieve contravariant velocities

         CALL SUBST_COMM ( DENSJ, DSPL_N0_E0_S1_W0, DRCN_S, COMMSTR )
         DO ROW = 1, NROWS+1
            DO COL = 1, NCOLS
               DJ = 0.5 * ( DENSJ( COL,ROW ) + DENSJ( COL,ROW-1 ) )
               WIND( COL,ROW ) = WIND( COL,ROW ) / DJ
            END DO
         END DO

      END IF

      RETURN

      END
