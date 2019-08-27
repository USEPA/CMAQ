
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
C $Header: /project/work/rep/PARIO/src/pshut3.f,v 1.2 2011/03/30 18:13:05 sjr Exp $ 

        LOGICAL FUNCTION  PSHUT3 ( )

C***********************************************************************
C  FUNCTION:  Flushes and closes down all Models-3 files currently open by
C             PE 0 only.
C
C  RETURN VALUE:  TRUE iff it succeeds.
C
C  PRECONDITIONS:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:  
C       prototype 09/02 by David Wong
C       Modified 02/23/2011 by Shawn Roselle
C          -- Replaced I/O API include files with M3UTILIO
C       Modified 09/10/2014 by David Wong
C          -- Removed redundant INCLUDE NETCDF.EXT statement
C       Modified 12/09/2015 by David Wong
C          -- Introduced a new variable PARIO_IO_PE_INCLUSIVE to determine
C             which procesor involves in file closing process
C***********************************************************************

      USE M3UTILIO              ! i/o api
      USE PIOMAPS_MODULE

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'STATE3.EXT'
        INCLUDE 'PIOVARS.EXT'
        INCLUDE "mpif.h"

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         FILE            !  loop counter over files
        INTEGER         V               !  loop counter over vbles
        INTEGER         IERR            !  netCDF error status return
        INTEGER         ERROR           !  MPI error status return
        INTEGER         FID
        INTEGER         LOC_MPIGRD3

        INTEGER, EXTERNAL :: NF_CLOSE
        INTEGER, EXTERNAL :: NFMPI_CLOSE

C***********************************************************************

        PSHUT3 = .TRUE.
        IF (.NOT. FINIT3) THEN
            RETURN
        END IF

#ifdef parallel_io
        LOC_MPIGRD3 = MPIGRD3
#else
        LOC_MPIGRD3 = IMISS3
#endif

        DO FILE = 1, COUNT3

           FID = CDFID3(FILE)

           IF ((FID .GE. 0) .AND. (FLIST3(FILE) .NE. CMISS3)) THEN

!             IF (MY_PE .EQ. 0) THEN
!                CALL NCCLOS( CDFID3( FILE ), IERR )
!                IF (IERR .NE. 0) THEN
!                   WRITE (LOGDEV,91010)
!    &                     'Error closing netCDF file ',
!    &                     'File name:  ' // FLIST3( FILE ) ,
!    &                     'netCDF error number', IERR
!                   PSHUT3 = .FALSE.
!                END IF
!             END IF

              IF( FTYPE3( FILE ) .EQ. LOC_MPIGRD3 ) THEN

#ifdef parallel_io
                 IF( PARIO_IO_PE_INCLUSIVE ) THEN
                   IERR =  NFMPI_CLOSE( FID )
                   IF ( IERR .NE. 0 ) THEN
                       WRITE( LOGDEV,91010 )
     &                 'Error closing PnetCDF file ',
     &                 'File name:  ' // FLIST3( FILE ) ,
     &                 'PnetCDF error number', IERR
                       PSHUT3 = .FALSE.
                   END IF
                 END IF
#endif

              ELSE IF( FTYPE3( FILE ) .GE. 0 ) THEN

                   IERR =  NF_CLOSE( FID )
                   IF ( IERR .NE. 0 ) THEN
                       WRITE( LOGDEV,91010 )
     &                 'Error closing netCDF file ',
     &                 'File name:  ' // FLIST3( FILE ) ,
     &                 'netCDF error number', IERR
                       PSHUT3 = .FALSE.
                   END IF
              END IF
           END IF

           CALL BUFDEL3 (FILE)
           FLIST3(FILE) = CMISS3
           DO V = 1, NVARS3( FILE )
              ILAST3(V,FILE) = IMISS3
              LDATE3(V,FILE) = IMISS3
              LTIME3(V,FILE) = IMISS3
              NDATE3(V,FILE) = IMISS3
              NTIME3(V,FILE) = IMISS3
           END DO

        END DO      !  end loop on files

        COUNT3 = IMISS3
        FINIT3 = .FALSE.

        CALL MPI_BCAST (PSHUT3, 1, MPI_LOGICAL, 0 ,MPI_COMM_WORLD, ERROR)

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine SHUT3 <<<',
     &            3 ( /5X , A , : ) , I5, // )

        END FUNCTION PSHUT3
