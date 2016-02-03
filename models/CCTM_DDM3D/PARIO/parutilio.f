C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/PARIO/src/parutilio.f,v 1.2 2012/04/11 13:47:45 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C....................................................................
C  CONTAINS:  Interface for PAR I/O procedures
C
C  REVISION HISTORY:
C       Original version 02/11 by Shawn Roselle
C....................................................................

      MODULE PARUTILIO

         IMPLICIT NONE

C...Declare PWRITE3 outside the interface block:  with check interface
C...  option, compilers complained about type mismatches for buffer 
C...  array passed to function PWRITE3 (when included in interface).

         LOGICAL, EXTERNAL :: PWRITE3

C...Interface

         INTERFACE

            SUBROUTINE BOUNDARY ( GNBNDY, NBNDY, NLAYS, BTHICK, NEIGHBOR,
     &                            MY_COL1, MY_ROW1, MY_NCOL, MY_NROW,
     &                            GNCOLS, GNROWS, GARRAY, LARRAY )
               INTEGER :: NLAYS
               INTEGER :: NBNDY
               INTEGER :: GNBNDY
               INTEGER :: BTHICK
               INTEGER :: NEIGHBOR(8)
               INTEGER :: MY_COL1
               INTEGER :: MY_ROW1
               INTEGER :: MY_NCOL
               INTEGER :: MY_NROW
               INTEGER :: GNCOLS
               INTEGER :: GNROWS
               REAL    :: GARRAY( GNBNDY,NLAYS )
               REAL    :: LARRAY( NBNDY,NLAYS )
            END SUBROUTINE BOUNDARY

            SUBROUTINE GET_WRITE_MAP ( NP, NPC, NPR, NCOLS3D, NROWS3D, NLAYS3D )
               INTEGER :: NP
               INTEGER :: NPC
               INTEGER :: NPR
               INTEGER :: NCOLS3D
               INTEGER :: NROWS3D
               INTEGER :: NLAYS3D
            END SUBROUTINE GET_WRITE_MAP

            LOGICAL FUNCTION GROWBUF( PTR_ARRAY, END )
               USE PINTERPB_MODULE
               TYPE (MEM_TYPE) :: PTR_ARRAY
               INTEGER, INTENT(IN) :: END
            END FUNCTION GROWBUF

            LOGICAL FUNCTION GTNDXHDV( FILE, VAR, JDATE, JTIME, VARSIZE, NBVS,
     &                                 ENDBUF, VX, NEWVAR )
               CHARACTER(16) :: FILE
               CHARACTER(16) :: VAR
               INTEGER :: JDATE
               INTEGER :: JTIME
               INTEGER :: VARSIZE
               INTEGER :: NBVS
               INTEGER :: ENDBUF
               INTEGER :: VX
               LOGICAL :: NEWVAR
            END FUNCTION GTNDXHDV

            LOGICAL FUNCTION INTERPOL( DATE, TIME, DATE1, TIME1, DATE2, TIME2,
     &                                 NUMVALS, VALSIN1, VALSIN2, VALSOUT )
               INTEGER :: NUMVALS
               INTEGER :: DATE
               INTEGER :: TIME
               INTEGER :: DATE1
               INTEGER :: TIME1
               INTEGER :: DATE2
               INTEGER :: TIME2
               REAL    :: VALSIN1( NUMVALS )
               REAL    :: VALSIN2( NUMVALS )
               REAL    :: VALSOUT( NUMVALS )
            END FUNCTION INTERPOL

            LOGICAL FUNCTION PINTERPB( FILNAME, VARNAME, CALLER, JDATE, JTIME,
     &                                  VSIZE, VARRAY )
               INTEGER       :: VSIZE
               CHARACTER(16) :: FILNAME
               CHARACTER(*)  :: VARNAME
               CHARACTER(*)  :: CALLER
               INTEGER       :: JDATE
               INTEGER       :: JTIME
               REAL          :: VARRAY( VSIZE )
            END FUNCTION PINTERPB

            LOGICAL FUNCTION PIO_INIT( COLROW, GL_NCOLS, GL_NROWS, NLAYS,
     &                                 NTHIK, NCOLS, NROWS, NPCOL, NPROW,
     &                                 NPROCS, MYPE )
               CHARACTER(2), INTENT(INOUT) :: COLROW
               INTEGER, INTENT(IN) :: GL_NCOLS
               INTEGER, INTENT(IN) :: GL_NROWS
               INTEGER, INTENT(IN) :: NLAYS
               INTEGER, INTENT(IN) :: NTHIK
               INTEGER, INTENT(IN) :: NCOLS
               INTEGER, INTENT(IN) :: NROWS
               INTEGER, INTENT(IN) :: NPCOL
               INTEGER, INTENT(IN) :: NPROW
               INTEGER, INTENT(IN) :: NPROCS
               INTEGER, INTENT(IN) :: MYPE
            END FUNCTION PIO_INIT

            LOGICAL FUNCTION PIO_RE_INIT( COLROW, GL_NCOLS, GL_NROWS, NLAYS,
     &                                    NTHIK, NCOLS, NROWS, NPCOL, NPROW,
     &                                    NPROCS, MYPE, WFLG )
               CHARACTER(2), INTENT(INOUT) :: COLROW
               INTEGER, INTENT(IN) :: GL_NCOLS
               INTEGER, INTENT(IN) :: GL_NROWS
               INTEGER, INTENT(IN) :: NLAYS
               INTEGER, INTENT(IN) :: NTHIK
               INTEGER, INTENT(IN) :: NCOLS
               INTEGER, INTENT(IN) :: NROWS
               INTEGER, INTENT(IN) :: NPCOL
               INTEGER, INTENT(IN) :: NPROW
               INTEGER, INTENT(IN) :: NPROCS
               INTEGER, INTENT(IN) :: MYPE
               LOGICAL, INTENT(IN) :: WFLG
            END FUNCTION PIO_RE_INIT

            SUBROUTINE PM3ERR ( CALLER, JDATE, JTIME, ERRTXT, FATAL )
               CHARACTER(*) :: CALLER
               INTEGER      :: JDATE
               INTEGER      :: JTIME
               CHARACTER(*) :: ERRTXT
               LOGICAL      :: FATAL
            END SUBROUTINE PM3ERR

            SUBROUTINE PM3EXIT ( CALLER, JDATE, JTIME, MSGTXT, EXITSTAT )
               CHARACTER(*) :: CALLER
               INTEGER      :: JDATE
               INTEGER      :: JTIME
               CHARACTER(*) :: MSGTXT
               INTEGER      :: EXITSTAT
            END SUBROUTINE PM3EXIT

            SUBROUTINE PM3WARN ( CALLER, JDATE, JTIME, ERRTXT )
               CHARACTER(*) :: CALLER
               INTEGER      :: JDATE
               INTEGER      :: JTIME
               CHARACTER(*) :: ERRTXT
            END SUBROUTINE PM3WARN

            LOGICAL FUNCTION PSHUT3 ()
            END FUNCTION PSHUT3

            LOGICAL FUNCTION PTRWRITE3( FNAME, VNAME, JDATE, JTIME, BUFFER )
               CHARACTER(*)  :: FNAME
               CHARACTER(*)  :: VNAME
               INTEGER       :: JDATE
               INTEGER       :: JTIME
               REAL, POINTER :: BUFFER( :,:,:,: )
            END FUNCTION PTRWRITE3

            LOGICAL FUNCTION PWRGRDD( FILNAME, VARNAME, DATE, TIME, BUFFER,
     &                                NCOLS3D, NROWS3D, NLAYS3D, NCOLS, NROWS,
     &                                NP )
               INTEGER      :: NROWS
               INTEGER      :: NCOLS
               INTEGER      :: NLAYS3D
               CHARACTER(*) :: FILNAME
               CHARACTER(*) :: VARNAME
               INTEGER      :: DATE
               INTEGER      :: TIME
               REAL         :: BUFFER( NCOLS,NROWS,NLAYS3D )
               INTEGER      :: NCOLS3D
               INTEGER      :: NROWS3D
               INTEGER      :: NP
            END FUNCTION PWRGRDD

            LOGICAL FUNCTION READBNDY( FILNAME, VARNAME, VX, NBNDY, NLAYS,
     &                                 DATE, TIME, RFLAG, FLIP )
               CHARACTER(16) :: FILNAME
               CHARACTER(16) :: VARNAME
               INTEGER :: VX
               INTEGER :: NBNDY
               INTEGER :: NLAYS
               INTEGER :: DATE(2)
               INTEGER :: TIME(2)
               INTEGER :: RFLAG
               INTEGER :: FLIP
            END FUNCTION READBNDY

            SUBROUTINE SUBDMAP ( NUMPROCS, NCOLS, NROWS, NLAYS, NPCOL, NPROW,
     &                           NCOLS_PE, NROWS_PE, COLSX_PE, ROWSX_PE,
     &                           MAXCELLS, IERR )
               INTEGER :: NUMPROCS
               INTEGER :: NCOLS
               INTEGER :: NROWS
               INTEGER :: NLAYS
               INTEGER :: NPCOL
               INTEGER :: NPROW
               INTEGER :: NCOLS_PE( NUMPROCS )
               INTEGER :: NROWS_PE( NUMPROCS )
               INTEGER :: COLSX_PE( 2,NUMPROCS )
               INTEGER :: ROWSX_PE( 2,NUMPROCS )
               INTEGER :: MAXCELLS
               INTEGER :: IERR
            END SUBROUTINE SUBDMAP

            SUBROUTINE WRSUBMAP ( NUMPROCS, NCOLS_PE, NROWS_PE, COLSX_PE,
     &                            ROWSX_PE )
               INTEGER :: NUMPROCS
               INTEGER :: NCOLS_PE(*)
               INTEGER :: NROWS_PE(*)
               INTEGER :: COLSX_PE(2,*)
               INTEGER :: ROWSX_PE(2,*)
            END SUBROUTINE WRSUBMAP

         END INTERFACE

      END MODULE PARUTILIO

