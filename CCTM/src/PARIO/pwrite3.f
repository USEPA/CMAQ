
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

        LOGICAL FUNCTION PWRITE3( FNAME, VNAME, JDATE, JTIME, BUFFER )

C-----------------------------------------------------------------------
C Function: Writes grided data BUFFER to the Models-3 data file with logical 
C           name FNAME by collecting from each PE.

C Return Value: TRUE iff the operation succeeds

C Preconditions: FNAME is a Models-3 data file already opened for write
C                access by OPEN3() or by CREATE3()

C Postconditions: Subsequent call to SHUT3() to flush the file to disk

C Revision History:
C   prototype 08/01 by David Wong, Lockheed Martin
C             10/01 by David Wong
C             -- expanded functionality to handle PING file
C             -- adjusted SIZE according to VID
C             -- modified the routine to accommodate writing sub-grid data
C             12/02 by David Wong
C             -- exteneded to write dot file
C             01/03 by David Wong
C             -- inserted WRTFLAG function to output time step info
C             02/04 by David Wong
C             -- collect all subdomain data and call WRITE3 once
C             02/11 by Shawn Roselle
C             -- Replaced I/O API include files with M3UTILIO; removed
C                deprecated TRIMLEN
C             08/11 by David Wong
C             -- extended to handle window met_cro_3d file
C             09/14 by David Wong
C             -- Removed redundant INCLUDE NETCDF.EXT statement
C-----------------------------------------------------------------------

      USE PIOMAPS_MODULE
      USE M3UTILIO              ! i/o api
      USE RUNTIME_VARS , ONLY : PWRTFLAG

      IMPLICIT NONE

C Includes:

      INCLUDE 'STATE3.EXT'

      INCLUDE 'PIOVARS.EXT'      ! Parameters for parallel implementation.
      INCLUDE 'PIOGRID.EXT'      ! Parallel grid dimensions.
!     INCLUDE 'PIOMAPS.EXT'      ! Parallel processor-to-subdomain maps.

      INCLUDE 'mpif.h'

C Arguments:

      CHARACTER( * ) :: FNAME       ! logical file name
      CHARACTER( * ) :: VNAME       ! logical file name
      INTEGER           JDATE       ! date, formatted YYYYDDD
      INTEGER           JTIME       ! time, formatted HHMMSS
      REAL              BUFFER( * ) ! output buffer array

C Local Variables:

      INTEGER       IDUM            ! holds return value for INIT3()
      INTEGER       FID             ! file-subscript for STATE3 arrays
      INTEGER       STEP            ! time step record number
      INTEGER       STEP2           ! 1 or 2, according to step mod 2
      INTEGER       TFLAG( 2 )      ! ( JDATE=yyyyddd, JTIME=hhmmss )
      INTEGER       IERR            ! netCDF error status return
      CHARACTER( 16 ) :: FIL16      ! scratch file-name     buffer
      CHARACTER( 16 ) :: VAR16      ! scratch variable-name buffer

C Static Variables:
      LOGICAL        FLCHANGE      ! File-changed indicator.

C File written to on previous call to pwrite3.
      CHARACTER( 16 ), SAVE :: PREVFILE = '????????????????'
      CHARACTER( 80 ) :: MSG       ! Buffer for building error messages.

      INTEGER        ERROR
      INTEGER        NVARS         ! Number of file variables to read.
      INTEGER        IV            ! Loop counter over file variables.
      INTEGER        BUFSTEP       ! Buffer stride length.
      INTEGER        BP            ! Position in buffer for specific variable.

      INTEGER        PACKAGE( 4 )  ! MPI broadcast data

      CHARACTER( 16 ) :: VNAM16    ! Scratch area for variable name.

      LOGICAL, EXTERNAL :: PWRGRDD ! Parallel write for gridded data files.

C-----------------------------------------------------------------------

C Check that Models-3 I/O has been initialized:

      PWRITE3 = .TRUE.

      IF ( .NOT. FINIT3 ) THEN
         IDUM = INIT3()
         WRITE( LOGDEV,91010 ) 'WRITE3():  I/O API not yet initialized.'
         PWRITE3 = .FALSE.
         RETURN
      END IF

C Find netCDF index for the file, and check time step availability:

      IF ( LEN( FNAME ) .GT. 16 ) THEN
         WRITE( LOGDEV,91001 )
     &       'File "', FNAME, '"',
     &       'Max file name length 16; actual:', LEN( FNAME )
         PWRITE3 = .FALSE.
         RETURN
      END IF

      IF ( LEN( VNAME ) .GT. 16 ) THEN
         WRITE( LOGDEV,91002 )
     &       'File "', FNAME, '"',
     &       'Variable "', VNAME, '"',
     &       'Max variable name length 16; actual:', LEN( VNAME )
         PWRITE3 = .FALSE.
         RETURN
      END IF

      VAR16 = VNAME   ! fixed-length-16 scratch copy of name
      FIL16 = FNAME   ! fixed-length-16 scratch copy of name
      FID   = INDEX1 ( FIL16, COUNT3, FLIST3 )

      IF ( FNAME .EQ. PREVFILE ) THEN
         FLCHANGE = .FALSE.
      ELSE
         FLCHANGE = .TRUE.
         PREVFILE = FNAME
      END IF

      IF ( MY_PE .EQ. IO_PE ) THEN
         STEP = JSTEP3 ( JDATE, JTIME, SDATE3( FID ), STIME3( FID ),
     &                   ABS( TSTEP3( FID ) ) )
      END IF

      CALL MPI_BCAST ( STEP, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ERROR )

      IF ( STEP .LT. 0 ) THEN      
         WRITE( LOGDEV,91020 )
     &       'Time step error writing file:  ' // FIL16 ,
     &       'Requested date & time:    ', JDATE, JTIME ,
     &       'File starting date & time:',
     &       SDATE3( FID ), STIME3( FID ),
     &       'File time step:           ', TSTEP3( FID )
         PWRITE3 = .FALSE.
         RETURN
      END IF
            
      IF ( MY_PE .EQ. IO_PE ) THEN

         IF ( TSTEP3( FID ) .LT. 0 ) THEN
            STEP2 = 1 + MOD ( STEP-1, 2 )
            TFLAG( 1 ) = JDATE
            TFLAG( 2 ) = JTIME
         ELSE IF ( TSTEP3( FID ) .EQ. 0 ) THEN
            STEP2 = STEP
            TFLAG( 1 ) = 0
            TFLAG( 2 ) = 0
         ELSE
            STEP2 = STEP
            TFLAG( 1 ) = JDATE
            TFLAG( 2 ) = JTIME
         END IF

C Get file description.
         IF ( .NOT. DESC3( FNAME ) ) THEN
             MSG = 'Could not get '// TRIM( FNAME )
     &           // ' file description'
             CALL M3WARN( 'PWRITE3', JDATE, JTIME, MSG )
         END IF

      END IF

      CALL MPI_BCAST ( UPNAM3D, 16, MPI_CHARACTER, 0, MPI_COMM_WORLD, ERROR )
      CALL MPI_BCAST ( FTYPE3D, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ERROR )

C Operation valid only for gridded files.
      IF ( FTYPE3D .EQ. GRDDED3 ) THEN

         IF ( UPNAM3D .EQ. 'PDM' .OR.
     &        UPNAM3D .EQ. 'READ_PTX2'.OR.
     &        UPNAM3D .EQ. 'FAKE_GRIDDED' ) THEN

            IF ( MY_PE .EQ. IO_PE ) THEN
               IF ( .NOT. WRITE3 ( FNAME, VNAME, JDATE, JTIME, BUFFER ) ) THEN
                  MSG = 'WRITE3 failed writing variable '
     &                // TRIM( VNAME )
     &                // ' to file '// TRIM( FNAME )
                  CALL M3WARN( 'PWRITE3', JDATE, JTIME, MSG )
                  PWRITE3 = .FALSE.
               END IF
            END IF

         ELSE
      
            IF ( MY_PE .EQ. IO_PE ) THEN
               PACKAGE( 1 ) = NCOLS3D
               PACKAGE( 2 ) = NROWS3D
               PACKAGE( 3 ) = NLAYS3D
               PACKAGE( 4 ) = NVARS3D
            END IF

            CALL MPI_BCAST ( PACKAGE, 4, MPI_INTEGER, 0, MPI_COMM_WORLD, ERROR )

            IF ( MY_PE .NE. IO_PE ) THEN
               NCOLS3D = PACKAGE( 1 )
               NROWS3D = PACKAGE( 2 )
               NLAYS3D = PACKAGE( 3 )
               NVARS3D = PACKAGE( 4 )
            END IF

            IF ( VAR16 .EQ. ALLVAR3 ) THEN
               NVARS = NVARS3D
            ELSE
               NVARS = 1
            END IF

            IF ( FLCHANGE ) THEN
               CALL GET_WRITE_MAP ( NUMPROCS, NPCOLD, NPROWD,
     &                              NCOLS3D, NROWS3D, NLAYS3D )
            END IF 

C Calculate buffer stride.
!           BUFSTEP = NUMCOLS * NUMROWS * NLAYS3D
            BUFSTEP =  WR_NCOLS_PE( MY_PE+1 ) * WR_NROWS_PE( MY_PE+1 ) * NLAYS3D

C Loop over file variables.
            DO IV = 1, NVARS

               IF ( VAR16 .EQ. ALLVAR3 ) THEN
                  VNAM16 = VNAME3D( IV )
               ELSE
                  VNAM16 = VAR16
               END IF

C Calculate buffer position.
               BP = BUFSTEP * ( IV-1 ) + 1

               IF ( .NOT. PWRGRDD( FIL16, VNAM16, JDATE, JTIME, BUFFER( BP ),
     &                             NCOLS3D, NROWS3D, NLAYS3D,
     &                             WR_NCOLS_PE( MY_PE+1 ), 
     &                             WR_NROWS_PE( MY_PE+1 ), NUMPROCS ) ) THEN
!    &                             NUMCOLS, NUMROWS, NUMPROCS ) ) THEN
                  MSG = 'PWRGRDD failed writing variable '
     &                  // TRIM( VNAM16 ) //
     &                  ' to file '// TRIM( FIL16 )
                  CALL PM3WARN( 'PWRITE3', JDATE, JTIME, MSG )
                  PWRITE3 = .FALSE.
                  RETURN
               END IF

            END DO

         END IF   ! fake-gridded

      ELSE     ! This is not a gridded file.

         MSG = 'PWRITE3 can not handle this type of data.'
         CALL PM3WARN( 'PWRITE3', JDATE, JTIME, MSG )
         PWRITE3 = .FALSE.
         RETURN

      END IF   ! gridded file

      CALL MPI_BCAST ( PWRITE3, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ERROR )

      IF ( .NOT. PWRITE3 ) THEN            !  failure
         IF ( MY_PE .EQ. IO_PE ) THEN
            IF ( TSTEP3( FID ) .EQ. 0 ) THEN
               WRITE( LOGDEV, 92030 ) 'Error writing', VNAME, 'to', FNAME
            ELSE
               WRITE( LOGDEV, 92030 ) 
     &         'Error writing', VNAME, 'to', FNAME, 'for', JDATE, JTIME
            END IF
         END IF
         RETURN
      END IF                        !  if failure
        
      IF ( MY_PE .EQ. IO_PE ) THEN
         MXREC3( FID ) = MAX( MXREC3( FID ), ABS( STEP ) )

         IF ( PWRTFLAG ) THEN   ! log successful writes
            IF ( VAR16 .NE. ALLVAR3 ) THEN   ! single variable written
               IF ( TSTEP3( FID ) .EQ. 0 ) THEN
                  WRITE( LOGDEV,92020 ) VNAME, 'written to', FNAME
               ELSE
                  WRITE( LOGDEV, 92020 ) 
     &                   VNAME, 'written to', FNAME, 'for', JDATE, JTIME
               END IF
            ELSE            ! vid <= 0:  entire timestamp written
               IF ( TSTEP3( FID ) .EQ. 0 ) THEN
                  WRITE( LOGDEV,92010 ) 'Record written to', FNAME
               ELSE
                  WRITE( LOGDEV,92010 ) 'Timestep written to', FNAME,
     &                     'for date and time', JDATE, JTIME
               END IF
            END IF          ! if single variable write, or timestamp write
         END IF          ! if PWRTFLAG
      END IF

      RETURN

C-----------------------------------------------------------------------

C Error and warning message formats... 91xxx

91001 FORMAT ( //5X , '>>> WARNING in subroutine WRITE3 <<<',
     &          /5X , 3A, /5X , A , I5, // )
91002 FORMAT ( //5X , '>>> WARNING in subroutine WRITE3 <<<',
     &          /5X , 3A, /5X , 3A, /5X , A , I5, // )
91010 FORMAT ( //5X , '>>> WARNING in subroutine WRITE3 <<<',
     &          3 ( /5X , A , : ) , I5, // )
91020 FORMAT ( //5X , '>>> WARNING in subroutine WRITE3 <<<',
     &          /5X , A , 2 ( /5X , A , :, I9, :, ':' , I6.6 ),
     &          /5X , A , 10X, I6.6,  // )
91030 FORMAT ( //5X , '>>> WARNING in subroutine WRITE3 <<<',
     &          /5X , A , I5 , : , /5X , A )

C Log message formats... 91xxx

92010 FORMAT ( /5X, 3( A, :, 1X ), I8, ':', I6.6 )
92020 FORMAT ( /5X, 4( A, :, 1X ), I8, ':', I6.6 )
92030 FORMAT ( /5X, 5( A, :, 1X ), I8, ':', I6.6 )

        END
