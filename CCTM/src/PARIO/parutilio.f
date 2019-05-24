
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
C $Header: /project/work/rep/PARIO/src/parutilio.f,v 1.2 2012/04/11 13:47:45 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C....................................................................
C  CONTAINS:  Interface for PAR I/O procedures
C
C  REVISION HISTORY:
C       Original version 02/11 by Shawn Roselle
C       Modified 12/09/2015 by David Wong
C          -- Added an optional argument in PIO_RE_INIT routine
C       Modified 01/26/2016 by Jeff Young
C          -- Change WFLG to an optional argument, eliminate PIO_RE_INIT, and make
C          PIO_INIT the only routine with a new optional IO_PE_INCLUSIVE argument;
C          replace WRSUBMAP with the new WRSUBDMAP
C       Modified 02/01/2019 by David Wong
C          removed unnecessary function declaration
C....................................................................

      MODULE PARUTILIO

         IMPLICIT NONE

C...Declare PWRITE3 outside the interface block:  with check interface
C...  option, compilers complained about type mismatches for buffer 
C...  array passed to function PWRITE3 (when included in interface).

         LOGICAL, EXTERNAL :: PWRITE3

C...Interface

         INTERFACE

!           SUBROUTINE BOUNDARY ( GNBNDY, NBNDY, NLAYS, BTHICK, NEIGHBOR,
!    &                            MY_COL1, MY_ROW1, NCOL, NROW,
!    &                            GNCOLS, GNROWS, GARRAY, LARRAY )
!              INTEGER :: NLAYS
!              INTEGER :: NBNDY
!              INTEGER :: GNBNDY
!              INTEGER :: BTHICK
!              INTEGER :: NEIGHBOR(8)
!              INTEGER :: MY_COL1
!              INTEGER :: MY_ROW1
!              INTEGER :: NCOL
!              INTEGER :: NROW
!              INTEGER :: GNCOLS
!              INTEGER :: GNROWS
!              REAL    :: GARRAY( GNBNDY,NLAYS )
!              REAL    :: LARRAY( NBNDY,NLAYS )
!           END SUBROUTINE BOUNDARY

            SUBROUTINE GET_WRITE_MAP ( NP, NPC, NPR, NCOLS3D, NROWS3D, NLAYS3D )
               INTEGER :: NP
               INTEGER :: NPC
               INTEGER :: NPR
               INTEGER :: NCOLS3D
               INTEGER :: NROWS3D
               INTEGER :: NLAYS3D
            END SUBROUTINE GET_WRITE_MAP

!           LOGICAL FUNCTION GROWBUF( PTR_ARRAY, END )
!              USE PINTERPB_MODULE
!              TYPE (MEM_TYPE) :: PTR_ARRAY
!              INTEGER, INTENT(IN) :: END
!           END FUNCTION GROWBUF

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
     &                                 NUMVALS, VALSIN1, VALSIN2, VALSOUT, S_IND, E_IND )
               INTEGER, INTENT(IN) :: NUMVALS
               INTEGER, INTENT(IN) :: DATE
               INTEGER, INTENT(IN) :: TIME
               INTEGER, INTENT(IN) :: DATE1
               INTEGER, INTENT(IN) :: TIME1
               INTEGER, INTENT(IN) :: DATE2
               INTEGER, INTENT(IN) :: TIME2
               REAL, INTENT(IN)    :: VALSIN1( NUMVALS )
               REAL, INTENT(IN)    :: VALSIN2( NUMVALS )
               REAL, INTENT(OUT)   :: VALSOUT( NUMVALS )
               INTEGER, INTENT(IN), OPTIONAL :: S_IND, E_IND
            END FUNCTION INTERPOL

!           LOGICAL FUNCTION PINTERPB( FILNAME, VARNAME, CALLER, JDATE, JTIME,
!    &                                  VSIZE, VARRAY, LVL )
!              INTEGER, INTENT(IN)       :: VSIZE
!              CHARACTER(16), INTENT(IN) :: FILNAME
!              CHARACTER(*), INTENT(IN)  :: VARNAME
!              CHARACTER(*), INTENT(IN)  :: CALLER
!              INTEGER, INTENT(IN)       :: JDATE
!              INTEGER, INTENT(IN)       :: JTIME
!              REAL, INTENT(OUT)         :: VARRAY( VSIZE )
!              INTEGER, INTENT(IN), OPTIONAL :: LVL
!           END FUNCTION PINTERPB

            LOGICAL FUNCTION PIO_INIT( colrow, gl_ncols, gl_nrows, nlays,
     &                                 nthik, ncols, nrows, npcol, nprow,
     &                                 nprocs, mype, wflg, io_pe_inclusive )
               CHARACTER(2), INTENT(INOUT) :: colrow
               INTEGER, INTENT(IN) :: gl_ncols
               INTEGER, INTENT(IN) :: gl_nrows
               INTEGER, INTENT(IN) :: nlays
               INTEGER, INTENT(IN) :: nthik
               INTEGER, INTENT(IN) :: ncols
               INTEGER, INTENT(IN) :: nrows
               INTEGER, INTENT(IN) :: npcol
               INTEGER, INTENT(IN) :: nprow
               INTEGER, INTENT(IN) :: nprocs
               INTEGER, INTENT(IN) :: mype
               LOGICAL, INTENT(IN), OPTIONAL :: wflg
               LOGICAL, INTENT(IN), OPTIONAL :: io_pe_inclusive
            END FUNCTION PIO_INIT
C Note: more than one optional argument requires that the caller calls by name

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

            SUBROUTINE WRSUBDMAP ( NUMPROCS, NCOLS_PE, NROWS_PE, COLSX_PE,
     &                             ROWSX_PE )
               INTEGER :: NUMPROCS
               INTEGER :: NCOLS_PE(*)
               INTEGER :: NROWS_PE(*)
               INTEGER :: COLSX_PE(2,*)
               INTEGER :: ROWSX_PE(2,*)
            END SUBROUTINE WRSUBDMAP

         END INTERFACE

      END MODULE PARUTILIO

