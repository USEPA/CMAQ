
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
C $Header: /project/work/rep/PARIO/src/subdmap.f,v 1.8 2011/03/30 18:13:07 sjr Exp $

      SUBROUTINE SUBDMAP ( NUMPROCS, NCOLS, NROWS, NLAYS, NPCOL, NPROW,
     &                     NCOLS_PE, NROWS_PE, COLSX_PE, ROWSX_PE,
     &                     MAXCELLS, IERR )
C ....................................................................
 
C  PURPOSE:   Subroutine to construct the horizontal
C             processor-to-subdomain map.
 
C  REVISION HISTORY: 
C       Original version  3/96 by Al Bourgeois for parallel implementation.
C       Modified 6/98 by AJB to improve error detection.
C       Modified 07/08/1998 by AJB to add NPCOL and NPROW to argument list.
C       Modified 07/29/1998 by AJB to use M3WARN instead of EXTMSG.
C       Modified 08/31/1998 by AJB to remove MYCELLS, MAXBNDRY from arg. list.
C       Modified 09/03/1998 by AJB to remove call to WRSUBMAP, and remove
C                arguments IO_PE and MY_PE.
C       Modified 09/12/1998 by AJB to increase MAXCELLS for DOT grid.
C       Modified 12/07/1998 by Al Bourgeois to add EXTERNAL declarations.
C       Modified 08/06/1999 by Al Bourgeois to remove error flag from argument
C                list and to exit with PM3EXIT on allocation errors.
C       Modified 02/02/2004 by David Wong
C         -- uses f90 syntax to allocate memory rather than using DYNMEM library
C       Modified 02/23/2011 by Shawn Roselle
C         -- Replaced I/O API include files with M3UTILIO
 
C  ARGUMENT LIST DESCRIPTION:
C M1 = PIOMAPS_MODULE
C  IN:
C        INTEGER    NUMPROCS                  ! Number of processors
C        INTEGER    NCOLS                     ! Total number of columns in grid
C        INTEGER    NROWS                     ! Total number of rows in grid
C        INTEGER    NLAYS                     ! Total number of layers in grid
C        INTEGER    NPCOL                     ! Number of PEs across grid cols
C        INTEGER    NPROW                     ! Number of PEs across grid rows
C  OUT:
C        INTEGER    NCOLS_PE( NUMPROCS )   M1 ! Number of columns for each PE
C        INTEGER    NROWS_PE( NUMPROCS )   M1 ! Number of rows for each PE
C        INTEGER    COLSX_PE( 2,NUMPROCS ) M1 ! Column index range for each PE
C        INTEGER    ROWSX_PE( 2,NUMPROCS ) M1 ! Row index range for each PE
C        INTEGER    MAXCELLS                  ! Maximum subdomain size over PEs
C        INTEGER    IERR
 
C  LOCAL VARIABLE DESCRIPTION:  see below
 
C  NOTES:  Uses dynamic memory allocation for NCOLS_WE and NROWS_SN
 
C .......................................................................

!     USE PIOMAPS_MODULE
      USE M3UTILIO              ! i/o api

      IMPLICIT  NONE

C ARGUMENTS:

      INTEGER    NUMPROCS               ! Number of processors
      INTEGER    NCOLS                  ! Total number of columns in grid
      INTEGER    NROWS                  ! Total number of rows in grid
      INTEGER    NLAYS                  ! Total number of layers in grid
      INTEGER    NPCOL                  ! Number of PEs across grid columns
      INTEGER    NPROW                  ! Number of PEs across grid rows
      INTEGER    NCOLS_PE( NUMPROCS )   ! Number of columns for each PE
      INTEGER    NROWS_PE( NUMPROCS )   ! Number of rows for each PE
      INTEGER    COLSX_PE( 2,NUMPROCS ) ! Column index range for each PE
      INTEGER    ROWSX_PE( 2,NUMPROCS ) ! Row index range for each PE
      INTEGER    MAXCELLS               ! Maximum subdomain size over PEs
      INTEGER    IERR                   ! return error code

C EXTERNAL FUNCTIONS:

C LOCAL VARIABLES: 

      INTEGER    I               ! Loop counter.
      INTEGER    NDX             ! Temporary index for processors row, column.
      INTEGER    NCOLX           ! Used for computing columns per domain.
      INTEGER    NROWX           ! Used for computing rows per domain.
      INTEGER    ICELLS          ! Temporary variable for computing MAXCELLS.
      INTEGER    DOT             ! For allocation of dot-grid variables.
      CHARACTER( 80 ) :: MSG     ! Message issued from M3WARN routine.

C Dynamic arrays.

      INTEGER, ALLOCATABLE :: NCOLS_WE( : )     ! Number columns in west-to-east subdomains.
      INTEGER, ALLOCATABLE :: NROWS_SN( : )     ! Number rows in south-to-north subdomains.
 
C .......................................................................

C Initialize.
      DOT = 1

C Allocate arrays.

      ALLOCATE ( NCOLS_WE( NPCOL ), STAT=IERR )
      IF ( IERR .NE. 0 ) THEN
         MSG = 'Error allocating NCOLS_WE.'
         CALL M3WARN ( 'SUBDMAP', 0, 0, MSG )
         RETURN
      END IF

      ALLOCATE ( NROWS_SN( NPROW ), STAT=IERR )
      IF ( IERR .NE. 0 ) THEN
         MSG = 'Error allocating NCOLS_SN.'
         CALL M3WARN ( 'SUBDMAP', 0, 0, MSG )
         RETURN
      END IF

C Construct the processor-to-subdomain map.

      NCOLX = NCOLS / NPCOL
      NROWX = NROWS / NPROW

      DO I = 1 , NPCOL
         NCOLS_WE( I ) = NCOLX
      END DO

      DO I = 1 , NPROW
         NROWS_SN( I ) = NROWX
      END DO

      DO I = 1, NCOLS - NPCOL*NCOLX       ! Spread out remaining columns
         NCOLS_WE( I ) = NCOLS_WE( I ) + 1
      END DO

      DO I = 1, NROWS - NPROW*NROWX
         NROWS_SN( I ) = NROWS_SN( I ) + 1     ! Spread out remaining rows
      END DO
       
C Assign the number of rows and columns for each PE subdomain, 
C and calculate the index range into the global domain.

C                                                    NPCOL
C  Example subdomain layout         _____________________________________
C  for 8 processors with           |         |         |        |        |
C  NPCOL=4, NPROW=2.               |         |         |        |        |
C                                  |    5    |    6    |    7   |    8   |
C                                  |         |         |        |        |
C                           NPROW  |_________|_________|________|________|
C                                  |         |         |        |        |
C                                  |         |         |        |        |
C                                  |    1    |    2    |    3   |    4   |
C                                  |         |         |        |        |
C                                  |         |         |        |        |
C                                  |_________|_________|________|________|

      DO I = 1, NUMPROCS

C Set NDX to the subdomain column index for processor I.
          NDX = MOD ( I,NPCOL )
          IF ( NDX .EQ. 0 ) NDX = NPCOL

C Assign the number of columns in this PE.
          NCOLS_PE( I ) = NCOLS_WE( NDX )

C Calculate column range of this PE in the global domain.
          IF ( NDX .EQ. 1 ) THEN
             COLSX_PE( 1,I ) = 1
             COLSX_PE( 2,I ) = NCOLS_PE( I )
          ELSE
             COLSX_PE( 1,I ) = COLSX_PE( 2,I-1 ) + 1
             COLSX_PE( 2,I ) = COLSX_PE( 2,I-1 ) + NCOLS_PE( I ) 
          END IF

C Set NDX to the subdomain row number for processor I.
          NDX = ( I - 1 ) / NPCOL + 1

C Calculate number of rows in this PE.
          NROWS_PE( I ) = NROWS_SN( NDX )

C Calculate row range of this PE in the global domain.
          IF ( I .LE. NPCOL ) THEN
             ROWSX_PE( 1,I ) = 1
             ROWSX_PE( 2,I ) = NROWS_PE( I )
          ELSE
             ROWSX_PE( 1,I ) = ROWSX_PE( 2,I-NPCOL ) + 1
             ROWSX_PE( 2,I ) = ROWSX_PE( 2,I-NPCOL ) + NROWS_PE( I ) 
          END IF

      END DO

C Compute the largest subdomain and the largest boundary.
      MAXCELLS = 0
      DO I = 1, NUMPROCS
         ICELLS = ( DOT + NCOLS_PE( I ) ) * ( DOT + NROWS_PE( I ) )
         IF ( MAXCELLS .LT. ICELLS ) MAXCELLS = ICELLS
      END DO
      MAXCELLS = MAXCELLS * NLAYS

C Free memory allocated for dynamic arrays.

      DEALLOCATE ( NCOLS_WE, STAT=IERR )
      IF ( IERR .NE. 0 ) THEN
         MSG = 'Error deallocating NCOLS_WE.'
         CALL M3WARN ( 'SUBDMAP', 0, 0, MSG )
      END IF

      DEALLOCATE ( NROWS_SN, STAT=IERR )
      IF ( IERR .NE. 0 ) THEN
         MSG = 'Error deallocating NROWS_SN.'
         CALL M3WARN ( 'SUBDMAP', 0, 0, MSG )
      END IF

      RETURN
      END

