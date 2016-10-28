
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
C $Header: /project/work/rep/PARIO/src/pwrgrdd.f,v 1.11 2012/01/26 20:14:28 sjr Exp $

      LOGICAL FUNCTION PWRGRDD( FILNAME, VARNAME, DATE, TIME, BUFFER,
     &                          NCOLS3D, NROWS3D, NLAYS3D,
     &                          NCOLS, NROWS, NP )
C.....................................................................
 
C  PURPOSE:   Perform Models-3 file-write operation in a parallel
C             environment. Values of variable VARNAME on each processor
C             subdomain region are collected, via MPI calls, by the
C             primary I/O processor and the full grid of values are
C             written to file.
 
C  RETURN VALUE:  The function fails if M3IO routine WRITE3 fails. If
C       an MPI error occurs, the program is aborted with a call to
C       PM3EXIT. 
 
C  REVISION  HISTORY:
C       07/1998 Original version by Al Bourgeois for parallel implementation.
C       Renamed  10/07/1998 by AJB from PWRITE3 to PRDGRDD, to be called
C                by PWRITE3.
C       Modified 11/09/1998 by AJB to save RBUFSIZ.
C       Modified 12/04/1998 by Al Bourgeois to add EXTERNAL declarations.
C       Modified 01/26/1999 by Al Bourgeois to allow writing to a subgrid.
C       Modified 06/22/1999 by Al Bourgeois to synchronize I/O processors
C          with their target (non-I/O) processors. If the I/O processor fails
C          on WRITE3, all processors will return a value of FALSE. This
C          routine now calls pm3exit if an MPI error occurs.
C       Modified 02/05/2004 by David Wong
C          -- use f90 allocatable structure, collect all and write once
C       Modified 09/08/2005 by David Wong
C          -- re-allocate WRITBUF when the size is changed so a subgrid data
C             can be output correctly
C       6 May 06 J.Young
C       Modified 02/23/2011 by Shawn Roselle
C          -- Replaced I/O API include files with M3UTILIO; removed
C             deprecated TRIMLEN
C       Modified 01/24/2012 by David Wong
C          -- Only let IO PE to allocate WRITBUF and RECVBUF to reduce overall
C             memory requirement
C       Modified 12/05/2015 by David Wong
C          -- Fixed bug so the code handle dot file correctly
 
C  ARGUMENT LIST DESCRIPTION:
C  M1 in PIOMAPS_MODULE
C  M2 in ALLOC_DATA_MODULE
C  E3 in PIOVARS.EXT
C  IN:
C    CHARACTER*(*)  FILNAME        ! Name of file containing variable VARNAME
C    CHARACTER*(*)  VARNAME        ! Name of file variable to write
C    INTEGER        DATE           ! Date, formatted YYYYDDD
C    INTEGER        TIME           ! Time, formatted HHMMSS
C    REAL           BUFFER(NCOLS, NROWS, NLAYS3D)
C                                  ! Buffer holding (local) array to be written
C    INTEGER  NCOLS3D              ! Column dimension of file variables
C    INTEGER  NROWS3D              ! Row dimension of file variables
C    INTEGER  NLAYS3D              ! Layer dimension of file variables
C    INTEGER  NCOLS                ! Column dimension of local-processor arrays
C    INTEGER  NROWS                ! Row dimension of local-processor arrays
C    INTEGER  NP                   ! Number of processors
C x  INTEGER  WR_NCOLS_PE(NP)   M1 ! No. cols of each PE subgrid to write
C x  INTEGER  WR_NROWS_PE(NP)   M1 ! No. rows of each PE subgrid to write
C x  INTEGER  WR_COLSX_PE(2,NP) M1 ! Col range of each PE subgrid to write
C x  INTEGER  WR_ROWSX_PE(2,NP) M1 ! Row range of each PE subgrid to write
C
C x  REAL WRITBUF(NCOLS3D, NROWS3D, NLAYS3D) M2
C                                  ! Buffer for writing an array.
C
C x  REAL RECVBUF(NCOLS, NROWS, NLAYS3D)   M2
C                                  ! Buffer for message passing an array.
C
C x  INTEGER  MY_PE    E3          ! Local processor id, ranging 0 to NP-1.
C x  INTEGER  IO_PE    E3          ! Id of primary processor used for file I/O.
 
C  OUT: none
 
C  LOCAL VARIABLE DESCRIPTION:  see below
 
C  CALLS: WRITE3, PM3WARN, PM3EXIT, TRIMLEN, MPI_SEND, MPI_RECV, MPI_BCAST
 
C  NOTES: (1) Only the primary I/O processor does the file writing. Input
C             arguments FILNAME, VARNAME, DATE, and TIME are meaningful
C             only to the I/O processor.
 
C         (2) This routine handles only gridded variables. The BUFFER is
C             assumed to be declared as BUFFER( NCOLS, NROWS, NLAYS3D ),
C             where NCOLS and NROWS are the local PE grid subdomain
C             dimensions and NLAYS3D is the file variable layer dimension.
C             BUFFER is assumed to be filled as BUFFER(1:C,1:R,1:NLAYS3D),
C             where C = WR_COLSX_PE(2,MY_PE+1), R = WR_ROWSX_PE(2,MY_PE+1).
C-----------------------------------------------------------------------

      USE PIOMAPS_MODULE
      USE ALLOC_DATA_MODULE
      USE M3UTILIO, ONLY : WRITE3              ! i/o api

      IMPLICIT NONE

C Include Files

      INCLUDE 'mpif.h'            ! MPI definitions and parameters
      INCLUDE 'PIOVARS.EXT'

C Arguments

      CHARACTER( * ) :: FILNAME   ! Name of file containing variable VARNAME
      CHARACTER( * ) :: VARNAME   ! Name of file variable to write
      INTEGER  DATE               ! Date, formatted YYYYDDD
      INTEGER  TIME               ! Time, formatted HHMMSS 
      INTEGER  NCOLS3D            ! Column dimension of file variables
      INTEGER  NROWS3D            ! Row dimension of file variables
      INTEGER  NLAYS3D            ! Layer dimension of file variables
      INTEGER  NCOLS              ! Column dimension of local-processor arrays
      INTEGER  NROWS              ! Row dimension of local-processor arrays
      INTEGER  NP                 ! Number of processors
 
      REAL BUFFER( NCOLS,NROWS,NLAYS3D )   ! Buffer holding (local) array 
                                           ! to be written

      INTEGER, SAVE :: WRITBUF_SIZE = 0
      INTEGER, SAVE :: RECVBUF_SIZE = 0
      INTEGER :: WSIZE, RSIZE

C External Functions:

      EXTERNAL PM3WARN        ! Parallel M3IO library

C Local Variables:

      INTEGER        MSGSIZE       ! Message size of subgrid to receive
      INTEGER        IPE           ! For loop over processors
      INTEGER        WHO           ! For identifying sending processor
      INTEGER        STATUS( MPI_STATUS_SIZE )   ! MPI status code
      INTEGER        IERROR        ! MPI error code
      LOGICAL        LERROR        ! LOCAL ERROR
      LOGICAL        RERROR        ! LOCAL MPI ALLREDUCE ERROR
      INTEGER        IR            ! Loop counter over grid rows
      INTEGER        IC            ! Loop counter over grid columns
      INTEGER        IL            ! Loop counter over grid layers
      INTEGER        C0            ! First column in global grid
      INTEGER        R0            ! First row in global grid
      INTEGER        NC            ! Number of columns in local grid
      INTEGER        NR            ! Number of rows in local grid
      CHARACTER( 16 ) :: FIL16     ! Scratch area for file-name
      CHARACTER( 16 ) :: VAR16     ! Scratch area for vble-name
      CHARACTER( 80 ) :: MSG       ! Message issued from PM3WARN routine

      INTEGER, PARAMETER :: TAG1 = 901 ! MPI message tag for processor ID
      INTEGER, PARAMETER :: TAG2 = 902 ! MPI message tag for data array.

      INTEGER  LOC

      LOGICAL :: DOT_FILE
      INTEGER :: DOT

C........................................................................

C Initialize return value and error code
      PWRGRDD = .TRUE.
      LERROR  = .FALSE.
      IERROR = 0

      IF ( ( NCOLS3D - PIO_GL_NCOLS .EQ. 1 ) .AND. 
     &     ( NROWS3D - PIO_GL_NROWS .EQ. 1 ) ) THEN
         DOT_FILE = .TRUE.
         DOT = 1
      ELSE
         DOT_FILE = .FALSE.
         DOT = 0
      END IF

      IF ( MY_PE .EQ. IO_PE ) THEN    ! I/O processor collects and writes data
         WSIZE = NCOLS3D * NROWS3D * NLAYS3D 
         RSIZE = ( NCOLS + DOT ) * ( NROWS + DOT ) * NLAYS3D

         IF ( WRITBUF_SIZE .NE. WSIZE ) THEN
            IF ( ALLOCATED ( WRITBUF ) ) DEALLOCATE ( WRITBUF )
            ALLOCATE ( WRITBUF  ( NCOLS3D, NROWS3D, NLAYS3D ), STAT = IERROR )
            IF ( IERROR .NE. 0 ) THEN
               MSG = 'Failure allocating WRITBUF '
               CALL M3EXIT( 'PWRGRDD', DATE, TIME, MSG, 1 )
            END IF
            WRITBUF_SIZE = WSIZE
         END IF

         IF ( RECVBUF_SIZE .LT. RSIZE ) THEN
            IF ( ALLOCATED ( RECVBUF ) ) DEALLOCATE ( RECVBUF )
            ALLOCATE ( RECVBUF  ( RSIZE ), STAT = IERROR )
            IF ( IERROR .NE. 0 ) THEN
               MSG = 'Failure allocating RECVBUF '
               CALL M3EXIT( 'PWRGRDD', DATE, TIME, MSG, 1 )
            END IF
            RECVBUF_SIZE = RSIZE
         END IF

C Gather the array and write it to file.

!     IF ( MY_PE .EQ. IO_PE ) THEN    ! I/O processor collects and writes data

C I/O PE copies its own local array into output buffer

         C0 = WR_COLSX_PE( 1,IO_PE+1 )
         R0 = WR_ROWSX_PE( 1,IO_PE+1 )
         NC = WR_NCOLS_PE( IO_PE+1 )
         NR = WR_NROWS_PE( IO_PE+1 )

         DO IL = 1, NLAYS3D
            DO IR = 1, NR
               DO IC = 1, NC 
                  WRITBUF( C0+IC-1,R0+IR-1,IL ) = BUFFER( IC,IR,IL )
               END DO    
            END DO    
         END DO

C I/O PE receives array from all other processors and copies it to the output
C buffer. Arrays are received in a first-come-first-serve order.  

         DO IPE = 1, NP - 1

            CALL MPI_RECV( WHO, 1, MPI_INTEGER, MPI_ANY_SOURCE, 
     &                     TAG1, MPI_COMM_WORLD, STATUS, IERROR )

            IF ( IERROR .NE. 0 ) THEN
               MSG = 'MPI error receiving processor id WHO.'
               CALL PM3WARN( 'PWRGRDD', DATE, TIME, MSG )
               LERROR = .TRUE.
            END IF

            C0 = WR_COLSX_PE( 1,WHO+1 )
            R0 = WR_ROWSX_PE( 1,WHO+1 )
            NC = WR_NCOLS_PE( WHO+1 )
            NR = WR_NROWS_PE( WHO+1 )

            MSGSIZE = NC * NR * NLAYS3D

            CALL MPI_RECV( RECVBUF, MSGSIZE, MPI_REAL, WHO,
     &                     TAG2, MPI_COMM_WORLD, STATUS, IERROR )
            IF ( IERROR .NE. 0 ) THEN
               MSG = 'MPI error receiving data array RECVBUF.'
               CALL PM3WARN( 'PWRGRDD', DATE, TIME, MSG )
               LERROR = .TRUE.
            END IF

            LOC = 0
            DO IL = 1, NLAYS3D
               DO IR = 1, NR
                  DO IC = 1, NC
                     LOC = LOC + 1
                     WRITBUF( C0+IC-1,R0+IR-1,IL ) = RECVBUF( LOC )
                  END DO    
               END DO    
            END DO

         END DO

C Write the accumulated array to file

         FIL16 = FILNAME
         VAR16 = VARNAME
         IF ( .NOT. WRITE3( FIL16, VAR16, DATE, TIME, WRITBUF ) ) THEN
            MSG = 'Could not write '
     &            // TRIM( VARNAME ) //
     &            ' to file '// TRIM( FIL16 )

            CALL PM3WARN( 'PWRGRDD', DATE, TIME, MSG )
            LERROR = .TRUE.
         END IF

      ELSE      ! Non-I/O processors send data

C Each processor, except for the I/O processor, sends its local array to
C the I/O processor.

         WHO = MY_PE
         MSGSIZE = NCOLS * NROWS * NLAYS3D

         CALL MPI_SEND( WHO, 1, MPI_INTEGER, IO_PE, TAG1,
     &                  MPI_COMM_WORLD, IERROR )

         IF ( IERROR .NE. 0 ) THEN
            MSG = 'MPI error sending processor id WHO.'
            CALL PM3WARN( 'PWRGRDD', DATE, TIME, MSG )
            LERROR = .TRUE.
         END IF

         CALL MPI_SEND( BUFFER, MSGSIZE, MPI_REAL, IO_PE, TAG2,
     &                  MPI_COMM_WORLD, IERROR )

         IF ( IERROR .NE. 0 ) THEN
            MSG = 'MPI error sending data array BUFFER.'
            CALL PM3WARN( 'PWRGRDD', DATE, TIME, MSG )
            LERROR = .TRUE.
         END IF

      END IF     ! if( my_pe .eq. io_pe )

C Notify non-I/O processors of failure

      CALL MPI_ALLREDUCE( LERROR, RERROR, 1, MPI_LOGICAL, MPI_LAND, 
     &                    MPI_COMM_WORLD, IERROR )

      IF ( IERROR .NE. 0 ) THEN
         MSG = 'MPI Allreduce error.'
         CALL PM3WARN( 'PWRGRDD', DATE, TIME, MSG )
         LERROR = .TRUE.
      END IF

      IF ( RERROR ) THEN
         MSG = 'Failed to write '
     &        // TRIM( VARNAME ) //
     &        ' from file '// TRIM( FILNAME )
         CALL PM3WARN( 'PWRGRDD', DATE, TIME, MSG )
         PWRGRDD = .FALSE.
         RETURN        ! Write failed, so return.
      END IF

      RETURN
      END
