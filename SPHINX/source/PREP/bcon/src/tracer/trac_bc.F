
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
C $Header: /project/yoj/arc/BCON/src/tracer/trac_bc.F,v 1.2 2011/10/21 16:52:37 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE TRAC_BC ( TRNAME, TCOUT )

C***********************************************************************
 
C Function: Compute BCs for special tracer tests
              
C Preconditions: None
  
C Key Subroutines/Functions Called:   
 
C Revision History:
C    Prototype created by Daewon Byun
C    Modified for implementation in Models-3 ICON by Jerry Gipson, January, 1998
C    02/25/02 Steve Howard (Jeff Young) - dynamic allocation
C    20 Jul 11 J.Young: Convert for Namelist redesign
C    23 May 12 J.Young: Replaced BC_PARMS include file with an F90 module
 
C***********************************************************************

      USE HGRD_DEFN    ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN    ! vertical layer specifications
      USE M3UTILIO     ! IOAPI module
      USE BC_PARMS     ! BCON parameters

      IMPLICIT NONE     

C Include Files:

C Arguments: 
      CHARACTER( 16 ), INTENT( IN ) :: TRNAME     !  Tracer name
      REAL, INTENT( OUT ) :: TCOUT( :,: )

C Parameters:
      INTEGER, PARAMETER :: CELL_RAD = 3   ! Radius for superposition tracers
!     INTEGER, PARAMETER :: COL_PEAK = 10  ! Location of center of peak for superposition tracers
!     INTEGER, PARAMETER :: ROW_PEAK = 10  ! Location of center of peak for superposition tracers
      REAL,    PARAMETER :: CMAX = 50.0    ! Max tracer conc for superposition tracers
      REAL,    PARAMETER :: CMIN = 50.0    ! Min tracer conc for superposition tracers

C External Functions: None

C Saved Local Variables:
      LOGICAL, SAVE :: LFIRST = .TRUE.     ! Flag for first call
      REAL, SAVE, ALLOCATABLE :: SHAPE( : )   ! Shape factor for superposition tracers

C Local Variables:
      CHARACTER( 16 ) :: PNAME = 'TRAC_BC' ! Procedure name
      CHARACTER( 80 ) :: MSG               ! Log message

      INTEGER COL_PEAK   ! Location of center of peak for superposition tracers
      INTEGER ROW_PEAK   ! Location of center of peak for superposition tracers

      INTEGER C, R, L    ! Grid loop indices
      INTEGER N          ! Boundary cell counter
      INTEGER ALLOCSTAT  ! Status returned from array allocation

      INTEGER, SAVE, ALLOCATABLE :: BCOL( : )  ! Map from boundary cell no. to grid column
      INTEGER, SAVE, ALLOCATABLE :: BROW( : )  ! Map from boundary cell no. to grid row

      REAL DISTSQ     ! Distance squared for for superposition tracers
      REAL RADSQ      ! Hill radius squared for superposition tracers
      REAL XDIST      ! x-distance for for superposition tracers
      REAL YDIST      ! y-distance for for superposition tracers
                       
C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Compute shape factors for superposition tracers on first call
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN
         LFIRST = .FALSE.

C Allocate memory for Saved array
         ALLOCATE( SHAPE( NBNDY ), BCOL( NBNDY ), BROW( NBNDY ),
     &             STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            MSG = 'Failure allocating SHAPE, BCOL, BROW'
            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
         END IF

         N = 0

C South edge
         DO C = 1, NCOLS
            N = N + 1
            BCOL( N ) = C
            BROW( N ) = 1
         END DO
         N = N + 1
         BCOL( N ) = NCOLS
         BROW( N ) = 1

C East edge
         DO R = 1, NROWS
            N = N + 1
            BCOL( N ) = NCOLS
            BROW( N ) = R
         END DO
         N = N + 1
         BCOL( N ) = NCOLS
         BROW( N ) = NROWS

C North edge
         N = N + 1
         BCOL( N ) = 1
         BROW( N ) = NROWS
         DO C = 1, NCOLS
            N = N + 1
            BCOL( N ) = C
            BROW( N ) = NROWS
         END DO

C West edge
         N = N + 1
         BCOL( N ) = 1
         BROW( N ) = 1
         DO R = 1, NROWS
            N = N + 1
            BCOL( N ) = 1
            BROW( N ) = R
         END DO

         RADSQ = FLOAT( CELL_RAD ) ** 2

         COL_PEAK = NCOLS / 2
         ROW_PEAK = NROWS / 2

         DO N = 1, NBNDY
            SHAPE( N ) = 0.0
            XDIST = ABS( FLOAT( BCOL( N ) - COL_PEAK ) )
            YDIST = ABS( FLOAT( BROW( N ) - ROW_PEAK ) )
            DISTSQ = XDIST * XDIST + YDIST * YDIST + RADSQ
            SHAPE( N ) = RADSQ / DISTSQ
         END DO

      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Compute the ICs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( TRNAME .EQ. TRAC_NAME( 1 ) ) THEN           ! UN_IC1_BC0

         DO L = 1, NLAYS
            DO N = 1, NBNDY
               TCOUT( N,L ) = 0.0
            END DO
         END DO

      ELSE IF ( TRNAME .EQ. TRAC_NAME( 2 ) ) THEN        ! UN_IC1_BC1

         DO L = 1, NLAYS
            DO N = 1, NBNDY
               TCOUT( N,L ) = 1.0
            END DO
         END DO

      ELSE IF ( TRNAME .EQ. TRAC_NAME( 3 ) ) THEN        ! UN_IC0_BC1

         DO L = 1, NLAYS
            DO N = 1, NBNDY
               TCOUT( N,L ) = 1.0
            END DO
         END DO

      ELSE IF ( TRNAME .EQ. TRAC_NAME( 4 ) ) THEN        ! STREET

         DO L = 1, NLAYS
            DO N = 1, NBNDY
               TCOUT( N,L ) = 0.0
            END DO
         END DO

      ELSE IF ( TRNAME .EQ. TRAC_NAME( 5 ) ) THEN        ! CHECKERBOARD

         DO L = 1, NLAYS
            DO N = 1, NBNDY
               TCOUT( N,L ) = 0.0
            END DO
         END DO

      ELSE IF ( TRNAME .EQ. TRAC_NAME( 6 ) ) THEN        ! SPOS_SIG_A

         DO L = 1, NLAYS
            DO N = 1, NBNDY
               TCOUT( N,L ) = 1.0 * CMAX * ( 1.0 + SHAPE( N ) ) + CMIN
            END DO
         END DO


      ELSE IF ( TRNAME .EQ. TRAC_NAME( 7 ) ) THEN        ! SPOS_SIG_B

         DO L = 1, NLAYS
            DO N = 1, NBNDY
               TCOUT( N,L ) = 2.0 * CMAX * ( 1.0 + SHAPE( N ) ) - CMIN
            END DO
         END DO

      ELSE IF ( TRNAME .EQ. TRAC_NAME( 8 ) ) THEN        ! SPOS_SIG_C

         DO L = 1, NLAYS
            DO N = 1, NBNDY
               TCOUT( N,L ) = - 1.0 * CMAX * ( 1.0 + SHAPE( N ) ) + 2.0 * CMIN
            END DO
         END DO

      END IF

      RETURN

      END
