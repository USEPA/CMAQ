
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
C $Header: /project/yoj/arc/ICON/src/tracer/trac_ic.F,v 1.2 2011/10/21 16:41:58 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE TRAC_IC ( TRNAME, TCOUT )

C***********************************************************************
 
C Function: Compute ICs for special tracer species
              
C Preconditions: None
  
C Key Subroutines/Functions Called:   
 
C Revision History:
C  Prototype created by Daewon Byun
C  Modified for implementation in Models-3 ICON by Jerry Gipson, January, 1998
C  01/24/02 Steve Howard (Jeff Young) - dynamic allocation
C  19 Jul 11 J.Young: Convert for Namelist redesign
C  21 May 12 J.Young: Replaced IC_PARMS include file with an F90 module
 
C***********************************************************************

      USE HGRD_DEFN    ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN    ! vertical layer specifications
      USE M3UTILIO     ! IOAPI module
      USE IC_PARMS     ! ICON paramters

      IMPLICIT NONE     

C Include FILES:

C Arguments: 
      CHARACTER( 16 ), INTENT( IN ) :: TRNAME     !  Tracer name

      REAL, INTENT( OUT ) :: TCOUT( :,:,: )

C Parameters:
      INTEGER, PARAMETER :: CELL_RAD = 3   ! Radius for superposition tracers
!     INTEGER, PARAMETER :: COL_PEAK = 10  ! Location of center of peak for superposition tracers
!     INTEGER, PARAMETER :: ROW_PEAK = 10  ! Location of center of peak for superposition tracers
      REAL,    PARAMETER :: CMAX = 50.0    ! Max tracer conc for superposition tracers
      REAL,    PARAMETER :: CMIN = 50.0    ! Min tracer conc for superposition tracers

C External Functions: None
 
C Saved Local Variables:
      LOGICAL, SAVE :: LFIRST = .TRUE.    ! Flag for first call
      REAL, SAVE, ALLOCATABLE :: SHAPE( :,: )   ! Shape factor for superposition tracers

C Local Variables:
      CHARACTER( 16 ) :: PNAME = 'TRAC_IC'      ! Procedure name
      CHARACTER( 80 ) :: MSG               ! Log message

      INTEGER COL_PEAK   ! Location of center of peak for superposition tracers
      INTEGER ROW_PEAK   ! Location of center of peak for superposition tracers

      INTEGER C, R, L    ! Grid loop indices
      INTEGER ALLOCSTAT  ! Status returned from array allocation

      REAL    DISTSQ     ! Distance squared for for superposition tracers
      REAL    RADSQ      ! Hill radius squared for superposition tracers
      REAL    XDIST      ! x-distance for for superposition tracers
      REAL    YDIST      ! y-distance for for superposition tracers
                           
C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Compute shape factors for superposition tracers on first call
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN 
         LFIRST = .FALSE.

         ALLOCATE( SHAPE( NCOLS,NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            MSG = 'Failure allocating SHAPE'
            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
         END IF

         DO R = 1, NROWS
            DO C = 1, NCOLS
               SHAPE( C,R ) = 0.0
            END DO
         END DO
         
         RADSQ = FLOAT( CELL_RAD ) ** 2

         COL_PEAK = NCOLS / 2
         ROW_PEAK = NROWS / 2

         DO R = 1, NROWS
            YDIST = ABS( FLOAT( R - ROW_PEAK ) )
            DO C = 1, NCOLS
               XDIST = ABS( FLOAT( C - COL_PEAK ) )
               DISTSQ = XDIST * XDIST + YDIST * YDIST + RADSQ
               SHAPE( C,R ) = RADSQ / DISTSQ
            END DO
         END DO

      END IF   ! LFIRST

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Compute the ICs
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( TRNAME .EQ. TRAC_NAME( 1 ) ) THEN            ! UN_IC1_BC0

         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  TCOUT( C,R,L ) = 1.0
               END DO
            END DO
         END DO

      ELSE IF ( TRNAME .EQ. TRAC_NAME( 2 ) ) THEN       ! UN_IC1_BC1

         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  TCOUT( C,R,L ) = 1.0
               END DO
            END DO
         END DO

      ELSE IF ( TRNAME .EQ. TRAC_NAME( 3 ) ) THEN       ! UN_IC0_BC1

         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  TCOUT( C,R,L ) = 0.0
               END DO
            END DO
         END DO

      ELSE IF ( TRNAME .EQ. TRAC_NAME( 4 ) ) THEN       ! STREET

         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  IF ( MOD( C, 2 ) .EQ. 0 .AND. MOD( R, 2 ) .EQ. 0 ) THEN
                     TCOUT( C,R,L ) = 1.0
                  ELSE
                     TCOUT( C,R,L ) = 0.0
                  END IF
               END DO
            END DO
         END DO

      ELSE IF ( TRNAME .EQ. TRAC_NAME( 5 ) ) THEN       ! CHECKERBOARD

         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  IF ( MOD( C, 2 ) .EQ. MOD( R, 2 ) ) THEN
                     TCOUT( C,R,L ) = 1.0
                  ELSE
                     TCOUT( C,R,L ) = 0.0
                  END IF
               END DO
            END DO
         END DO

      ELSE IF ( TRNAME .EQ. TRAC_NAME( 6 ) ) THEN       ! SPOS_SIG_A

         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  TCOUT( C,R,L ) = 1.0 * CMAX * ( 1.0 + SHAPE( C,R ) ) + CMIN
               END DO
            END DO
         END DO

      ELSE IF ( TRNAME .EQ. TRAC_NAME( 7 ) ) THEN       ! SPOS_SIG_B

         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  TCOUT( C,R,L ) = 2.0 * CMAX * ( 1.0 + SHAPE( C,R ) ) - CMIN
               END DO
            END DO
         END DO

      ELSE IF ( TRNAME .EQ. TRAC_NAME( 8 ) ) THEN       ! SPOS_SIG_C

         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  TCOUT( C,R,L ) = - 1.0 * CMAX * ( 1.0 + SHAPE( C,R ) ) + 2.0 * CMIN
               END DO
            END DO
         END DO

      END IF

      RETURN

      END
