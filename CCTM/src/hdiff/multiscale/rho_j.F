
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
C $Header: /project/yoj/arc/CCTM/src/hdiff/multiscale/rho_j.F,v 1.5 2011/10/21 16:11:22 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE RHO_J ( JDATE, JTIME, TSTEP, RHOJI )
      
C-----------------------------------------------------------------------
C Function:
C   Get Air Density X SqRDMT for computational grid including boundary
C   where SqRDMT = Sq. Root [det ( metric tensor )]
C   = Jacobian / (map scale factor)**2
 
C Preconditions:
C   Can be used only for conformal map coordinates in the horizontal.
C   Dates and times should be represented YYYYDDD:HHMMSS.
 
C Subroutines and functions called:
C    INTERP3, M3EXIT, TIME2SEC, SEC2TIME, NEXTIME, CHECK3
      
C Revision history:
C    5 Nov 97 Jeff

C    Sep. 1998 David Wong
C      -- parallelized the code
C      -- removed the indirect index reference

C    6 Aug 01 J.Young: dyn alloc - Use HGRD_DEFN; replace INTERP3 with
C                      INTERPX and INTERPB; allocatable arrays ...
C                      Since F90 does not preserve dummy argument array
C                      indices, RHOJI( 1:NCOLS+2,, ) is copied into local array
C                      RHOJI( 0:NCOLS+1,, ).
C                      The caller of RHO_J dimensions the actual argument,
C                      as RHOJ( 0:NCOLS+1,, ).
C                      NOTE: NTHIK must be = 1
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   16 Feb 11 S. Roselle: replaced I/O-API include files w/UTILIO_DEFN
C   11 May 11 D.Wong: incorporated twoway model implementation
C   28 Jul 11 David Wong: set REVERT to .false. for twoway model case since
C                         buffered file has only two time steps data
C    1 Feb 19 David Wong: Implemented centralized I/O approach, removed all MY_N
C                         clauses
C    1 Aug 19 David Wong: Bug fixed -calling interpolate_var use RHOJ rather 
C                         than DENSJ_BUF in WINDOW scenario
C-----------------------------------------------------------------------
      
      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE UTILIO_DEFN
      use CENTRALIZED_IO_MODULE

      IMPLICIT NONE

C Includes:
      
!     INCLUDE SUBST_HGRD_ID     ! horizontal dimensioning parameters
!     INCLUDE SUBST_VGRD_ID     ! horizontal dimensioning parameters
      INCLUDE SUBST_FILES_ID    ! file name parameters
 
C Parameters

C Arguments:
      
      INTEGER, INTENT( IN )  :: JDATE        ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN )  :: JTIME        ! current model time, coded HHMMSS
      INTEGER, INTENT( IN )  :: TSTEP( 3 )   ! time step vector (HHMMSS)
                                             ! TSTEP(1) = local output step
                                             ! TSTEP(2) = sciproc sync. step (chem)
                                             ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                             !            step and wrf/cmaq call frequency
!     REAL        RHOJ( 0:NCOLS+1,0:NROWS+1,NLAYS )  ! Jacobian * air density
      REAL,    INTENT( OUT ) :: RHOJI( :,:,: )
      
C Parameters:

!     INTEGER, PARAMETER :: NTHIN = NTHIK - 1

C file variables:
      
!     REAL        DENSJ_BUF( NCOLS,NROWS,NLAYS ) ! Jacobian * air density
!     REAL        DENSJ_BND( NBNDY,NLAYS )    ! boundary Jacobian * air density
      REAL, ALLOCATABLE :: DENSJ_BUF( :,:,: ) ! Jacobian * air density
      REAL, ALLOCATABLE :: DENSJ_BND( :,: )    ! boundary Jacobian * air density
 
C External Functions: None
       
C local variables:
      
      CHARACTER( 16 ) :: VNAME
      CHARACTER( 16 ) :: PNAME = 'RHO_J'
      CHARACTER( 16 ) :: AMSG
      CHARACTER( 96 ) :: XMSG = ' '
 
      REAL, ALLOCATABLE :: RHOJ( :,:,: )

      INTEGER, SAVE :: MLAYS

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
       
      INTEGER     ALLOCSTAT
      INTEGER     COL, ROW, LVL ! column, row, layer indices
      INTEGER     MDATE         ! mid-advection date
      INTEGER     MTIME         ! mid-advection time
      INTEGER     STEP          ! advection time step in seconds
      INTEGER, SAVE :: LDATE( 2 )    ! last date for data on file
      INTEGER, SAVE :: LTIME( 2 )    ! last time for data on file
      LOGICAL     REVERT        ! recover last time step if true
 
      INTEGER COUNT

      INTEGER, SAVE :: LCB, HCB, LRB, HRB ! convenience pointers

C-----------------------------------------------------------------------
 
      IF ( FIRSTIME ) THEN
 
         FIRSTIME = .FALSE.
 
         LCB = 1 - NTHIK
         HCB = NCOLS + NTHIK
         LRB = 1 - NTHIK
         HRB = NROWS + NTHIK
         MLAYS = SIZE ( RHOJI,3 )

         CALL LSTEPF( MET_CRO_3D, LDATE( 1 ), LTIME( 1 ) )
!        CALL LSTEPF( MET_BDY_3D, LDATE( 2 ), LTIME( 2 ) )

!        LDATE( 1 ) = MIN( LDATE( 1 ), LDATE( 2 ) )
!        LTIME( 1 ) = SEC2TIME( MIN(
!    &                              TIME2SEC( LTIME( 1 ) ),
!    &                              TIME2SEC( LTIME( 2 ) )
!    &                              ) )

         END IF                    ! if firstime
 
      MDATE  = JDATE
      MTIME  = JTIME
      STEP   = TIME2SEC( TSTEP( 2 ) )
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
         XMSG = 'Current scenario interpolation step not available in both '
     &        // TRIM( MET_CRO_3D ) // 'and '
     &        // TRIM( MET_BDY_3D )
         CALL M3MESG( XMSG )
!        CALL NEXTIME( MDATE, MTIME, -SEC2TIME( STEP / 2 ) )
         WRITE( AMSG,'( 2I8 )' ) LDATE( 1 ), LTIME( 1 )
         XMSG = 'Using data for last file step: ' // AMSG
         CALL M3MESG( XMSG )
         MDATE = LDATE( 1 )
         MTIME = LTIME( 1 )
         END IF
 
C Interpolate Jacobian X Air Density

      ALLOCATE ( RHOJ( LCB:HCB,LRB:HRB,MLAYS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating RHOJ'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      IF ( WINDOW ) THEN

         call interpolate_var ('DENSA_J', mdate, mtime, RHOJ)

         ELSE ! need to extend data from bndy file

         ALLOCATE ( DENSJ_BUF( ncols,nrows,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DENSJ_BUF'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         call interpolate_var ('DENSA_J', mdate, mtime, DENSJ_BUF)

         ALLOCATE ( DENSJ_BND( NBNDY,MLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DENSJ_BND'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         call interpolate_var ('DENSA_J', mdate, mtime, DENSJ_BND, 'b')

C Load core of RHOJ array

         DO LVL = 1, MLAYS
            DO ROW = 1, NROWS
               DO COL = 1, NCOLS
                  RHOJ( COL,ROW,LVL ) = DENSJ_BUF( COL,ROW,LVL )
                  END DO
               END DO
            END DO

C Fill in DENSJ array for boundaries

         DO LVL = 1, MLAYS
            COUNT = 0
            DO ROW = 1-NTHIK, 0
               DO COL = 1, NCOLS+NTHIK
                  COUNT = COUNT + 1
                  RHOJ( COL,ROW,LVL ) = DENSJ_BND( COUNT,LVL )  ! South
               END DO
            END DO
            DO ROW = 1, NROWS+NTHIK
               DO COL = NCOLS+1, NCOLS+NTHIK
                  COUNT = COUNT + 1
                  RHOJ( COL,ROW,LVL ) = DENSJ_BND( COUNT,LVL )  ! East
               END DO
            END DO
            DO ROW = NROWS+1, NROWS+NTHIK
               DO COL = 1-NTHIK, NCOLS
                  COUNT = COUNT + 1
                  RHOJ( COL,ROW,LVL ) = DENSJ_BND( COUNT,LVL )  ! North
               END DO
            END DO
            DO ROW = 1-NTHIK, NROWS
               DO COL = 1-NTHIK, 0
                  COUNT = COUNT + 1
                  RHOJ( COL,ROW,LVL ) = DENSJ_BND( COUNT,LVL )  ! West
               END DO
            END DO
         END DO

         DEALLOCATE ( DENSJ_BUF )
         DEALLOCATE ( DENSJ_BND )

         END IF   ! WINDOW

C Adjust for argument offset (f90 dummy arguments assumed 1-based)

      DO LVL = 1, MLAYS
         DO ROW = 0, NROWS + 1
            DO COL = 0, NCOLS + 1
               RHOJI( COL+1,ROW+1,LVL ) = RHOJ( COL,ROW,LVL )
               END DO
            END DO
         END DO

      DEALLOCATE ( RHOJ )

      RETURN
      END
