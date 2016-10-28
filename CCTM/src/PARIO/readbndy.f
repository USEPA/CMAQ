
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
C $Header: /project/work/rep/PARIO/src/readbndy.f,v 1.7 2011/03/30 18:13:07 sjr Exp $
     
      LOGICAL FUNCTION READBNDY ( FILNAME, VARNAME, VX, NBNDY, NLAYS,
     &                            DATE, TIME, RFLAG, FLIP ) 
C ....................................................................
 
C  PURPOSE:   This function serves the Models-3 parallel interpolation
C             routine PINTERPB. It performs a read/interpolation operation
C             for boundary file variables.  Each processor does its own file 
C             reading and no communication is needed. The local processor
C             boundary for VARNAME is constructed and stored in a circular
C             file variable buffer used by function PINTERPB.
 
C  RETURN VALUE:  The function fails if READ3 fails.
 
C  REVISION HISTORY:
C       Original version 09/01/1998 by Al Bourgeois.
C       Modified 09/23/1998 by Al Bourgeois to change comments.
C       Modified 12/07/1998 by Al Bourgeois to add EXTERNAL declarations.
C       Modified 05/15/1999 by Al Bourgeois to clarify comments.
C       Modified 06/16/1999 by Al Bourgeois to remove interprocessor
C          synchronization. This removes the guarantee that all processors
C          return the same error code, and a "hang" state can occur if
C          PM3EXIT is not called on the condition that this function fails.
C       Modified 06/21/1999 by Al Bourgeois to synchronize I/O processors
C          with their target (non-I/O) processors. If an I/O processor fails
C          on READ3, it and its target processors return a value of FALSE.
C          There is still the potential of a "hang" state if PM3EXIT is not
C          called after this routine, but this takes care of the situation
C          that a non-existent file is attempted to be read.
C       Modified 06/24/1999 by Al Bourgeois to allow single processor calls.
C          The argument ALL_PE_MODE was added for this purpose.
C       Modified 07/15/1999 to remove clearing of message buffer, which is
C          now done in pinterp3 and pread3.
C       Modified 02/06/2004 by David Wong
C          -- to modify the code so it no longer uses DYNMEM library
C       Modified 02/23/2011 by Shawn Roselle
C          -- Replaced I/O API include files with M3UTILIO; removed
C             deprecated TRIMLEN
 
C  ARGUMENT LIST DESCRIPTION:
C  IN:
C    CHARACTER*16   FILNAME       ! Name of file containing variable VARNAME.
C    CHARACTER*16   VARNAME       ! Name of file variable to read.
C    INTEGER        VX            ! Index for file variable.
C    INTEGER        NBNDY         ! Boundary dimension of variable.
C    INTEGER        NLAYS         ! Layer dimension of variable.
C    INTEGER        DATE(2)       ! Start DATE(1):TIME(1), end DATE(2):TIME(2),
C    INTEGER        TIME(2)       ! for current buffered data.
C    INTEGER        RFLAG         ! Number of time records to read.
C    INTEGER        FLIP          ! Toggle for order of read buffers (0 or 1).
 
C    COMMON BLOCK PIOVARS:
C    INTEGER  MY_PE        ! Local processor id.
  
C    COMMON BLOCK PIOGRID:
C    INTEGER  NPCOLD       ! The number of processors across grid columns.
C    INTEGER  GNROWS       ! Number of rows in global grid.
C    INTEGER  GNCOLS       ! Number of columns in global grid.
C    INTEGER  BTHICK       ! Cell thickness of grid boundary.
C    INTEGER  NGB_PE(8)    ! PE neighborhood, first north then clockwise.
C    INTEGER  MY_NROWS     ! Local number of grid rows actually used.
C    INTEGER  MY_NCOLS     ! Local number of grid columns actually used.
C    INTEGER  MAXCELLS     ! Maximum subdomain size (# cells) over PEs.
 
C    INTEGER  ROWSX_PE(2,NUMPROCS) ! Row range for each PE.
C    INTEGER  COLSX_PE(2,NUMPROCS) ! Column range for each PE.
 
C  OUT:
C    COMMON BLOCK PINTERP3_REAL:
C    REAL     BUFFERHD     ! Circular file variable buffer.
 
C  LOCAL VARIABLE DESCRIPTION:  see below
 
C  CALLS: BOUNDARY, READ3, M3WARN
 
C  NOTES: Dimension checking has already been done in function GTNDXHDV.
C .......................................................................

      USE PIOMAPS_MODULE
      USE PINTERPB_MODULE
      USE M3UTILIO              ! i/o api

      IMPLICIT  NONE

C INCLUDE FILES

      INCLUDE 'PIOGRID.EXT'      ! Parallel grid dimensions.
      INCLUDE 'PIOVARS.EXT'      ! Parameters for parallel implementation.
!     INCLUDE 'PIOMAPS.EXT'      ! Parallel processor-to-subdomain maps.
!     INCLUDE 'PINTERPB.EXT'     ! Variables for parallel file reading.

C ARGUMENTS:

      CHARACTER( 16 ) :: FILNAME ! Name of file containing variable VARNAME.
      CHARACTER( 16 ) :: VARNAME ! Name of file variable to read.
      INTEGER        VX          ! Index for file variable.
      INTEGER        NBNDY       ! Boundary dimension of variable.
      INTEGER        NLAYS       ! Layer dimension of variable.
      INTEGER        DATE( 2 )   ! Dates for current buffered data.
      INTEGER        TIME( 2 )   ! Times for current buffered data.
      INTEGER        RFLAG       ! Number of time records to read.
      INTEGER        FLIP        ! Toggle for order of read buffers (0 or 1).

C EXTERNAL FUNCTIONS:

      EXTERNAL      BOUNDARY     ! Parallel M3IO library.

C LOCAL VARIABLES:

      INTEGER        GNBNDY      ! Size of global grid boundary.
      INTEGER        IPE         ! Loop counter over processors.
      INTEGER        LPE         ! Processor index.
      INTEGER        IT          ! Loop counter over time samples.
      CHARACTER( 80 ) :: MSG     ! Message issued from M3WARN routine.

      INTEGER        LOC, LOC3

C .......................................................................

C Initialize return value and error code

      READBNDY = .TRUE.

C Calculate global grid boundary size

      GNBNDY = ( 2*BTHICK * ( GNCOLS + GNROWS + 2*BTHICK ) )

C Set processor index

      LPE = MY_PE + 1            ! LPE range is 1 to NUMPROCS.

C Read (up to) two time samples of boundary data

      LOC3 = BUFPOSHD( VX )

      DO IT = 3 - RFLAG, 2        ! Loop over time samples

C Read boundary data for the global grid
  
         IF ( .NOT. READ3 ( FILNAME, VARNAME, ALLAYS3,
     &                      DATE( IT ), TIME( IT ), MSGBUFHD ) ) THEN
            MSG = 'Failed to read '
     &          // TRIM( VARNAME ) //
     &            ' from file '// TRIM( FILNAME )
            CALL M3WARN( 'READBNDY', DATE( IT ), TIME( IT ), MSG )
            READBNDY = .FALSE.; RETURN
         END IF

C Each processor constructs its local boundary from the global boundary

         LOC = IT - FLIP - 1

         CALL BOUNDARY( GNBNDY, NBNDY, NLAYS, BTHICK, NGB_PE,
     &                  COLSX_PE( 1,LPE ), ROWSX_PE( 1,LPE ),
     &                  MY_NCOLS, MY_NROWS, GNCOLS, GNROWS, MSGBUFHD, 
     &                  BUFFERHD( LOC3 )%MEM( LOC )%DATA_PTR )
 
      END DO     ! End loop over time samples

      RETURN
      END    ! Subroutine READBNDY

C                 no read  read 1 rec  read 2 rec
C           RFLAG    0          1           2
C           IT       3,2        2           1,2
C FLIP = 0
C           LOC      2,1        1           0,1
C FLIP = 1
C           LOC      1,0        0          -1,0
C
C
