
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/par/mpi/mpcomm_init.F,v 1.2 2011/04/01 15:41:51 sjr Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE MPCOMM_INIT( NPROCS, MYPE, CLOCK, IERR )
C.......................................................................
C
C Purpose:   Initialization for parallel execution of CMAQ
 
C Revision History: 
C      Original version  5/09 Jeff Young - modified from par_init
C      Modified 02/23/2011 by Shawn Roselle
C          -- Replaced I/O API include files with UTILIO_DEFN
 
C Argument List Description:
C  In:
C     INTEGER NLAYS             ! Number of layers in entire grid
C     INTEGER NSPCS             ! Number of species in CGRID
 
C  Out:
C     REAL    CLOCK             ! Wall-clock time (sec) at MPI initialization
C     INTEGER IERR              ! Error code: 0 for ok, 1 for fail
 
C Local Variable Description:
 
C  Other Local Variable Descriptions:  see below

C Calls: M3WARN, MPI_INIT, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_WTIME
 
C........................................................................

      USE UTILIO_DEFN

      IMPLICIT  NONE

C Include Files

      INCLUDE SUBST_MPI         ! MPI definitions and parameters

C Arguments

      INTEGER, INTENT( OUT ) :: NPROCS  ! number of parallel processors
      INTEGER, INTENT( OUT ) :: MYPE    ! this parallel processor
      REAL,    INTENT( OUT ) :: CLOCK   ! Wall-clock time (sec) at MPI initialization
      INTEGER, INTENT( OUT ) :: IERR    ! error code

C Local Variables 

      INTEGER FLAG

      CHARACTER( 80 ) :: XMSG     ! Message issued from M3WARN routine
      CHARACTER( 16 ) :: PNAME = 'MPCOMM_INIT'

C------------------------------------------------------------------------

      IERR = 0

C Start up MPI
      CALL MPI_INIT( FLAG )
      IF ( FLAG .NE. 0 ) THEN
         WRITE( XMSG,'(I6, A)' ) FLAG, 'Error in MPI_INIT.'
         CALL M3WARN ( PNAME, 0, 0, XMSG )
         IERR = 1; RETURN
      END IF

C Get wall-clock time
      CLOCK = REAL( MPI_WTIME() )

C Get number of processors
      CALL MPI_COMM_SIZE( MPI_COMM_WORLD, NPROCS, FLAG )
      IF ( FLAG .NE. 0 ) THEN
         WRITE( XMSG,'(I6, A)' ) FLAG, 'Error in MPI_COMM_SIZE.'
         CALL M3WARN ( PNAME, 0, 0, XMSG )
         IERR = 1; RETURN
      END IF

C Get my processor rank (i.e., get ID of this processor)
      CALL MPI_COMM_RANK( MPI_COMM_WORLD, MYPE, FLAG )
      IF ( FLAG .NE. 0 ) THEN
         WRITE( XMSG,'(I6, A)' ) FLAG, 'Error in MPI_COMM_RANK.'
         CALL M3WARN ( PNAME, 0, 0, XMSG )
         IERR = 1; RETURN
      END IF

#ifdef cluster
C Distribute run time environment variables to other machines - by bo wang
      CALL distr_env ( MYPE, NPROCS )
#endif

      RETURN
      END
