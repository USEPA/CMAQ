
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
C $Header: /project/yoj/arc/CCTM/src/par/mpi/mpcomm_init.F,v 1.4 2011/10/21 16:11:25 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE MPCOMM_INIT( NPROCS, MYPE, CLOCK, IERR )
C.......................................................................
C
C Purpose:   Initialization for parallel execution of CMAQ
 
C Revision History: 
C      Original version  5/09 Jeff Young - modified from par_init
C      Modified 02/23/2011 by Shawn Roselle
C          -- Replaced I/O API include files with UTILIO_DEFN
C      Modified 08/10/2011 by David Wong
C          -- Called MPI_INIT if it is a regular CMAQ run
C      Modified 08/2011 by David Wong
C          -- Replaced SUBST_MPI with mpif.h
 
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

!     INCLUDE SUBST_MPI         ! MPI definitions and parameters
      include 'mpif.h'

C Arguments

      INTEGER, INTENT( OUT ) :: NPROCS  ! number of parallel processors
      INTEGER, INTENT( OUT ) :: MYPE    ! this parallel processor
      REAL(8), INTENT( OUT ) :: CLOCK   ! Wall-clock time (sec) at MPI initialization
      INTEGER, INTENT( OUT ) :: IERR    ! error code

C Local Variables 

      INTEGER FLAG

      CHARACTER( 80 ) :: XMSG     ! Message issued from M3WARN routine
      CHARACTER( 16 ) :: PNAME = 'MPCOMM_INIT'

C------------------------------------------------------------------------

      IERR = 0

C Start up MPI
#if !defined(twoway) && !defined(mpas)
      CALL MPI_INIT( FLAG )
      IF ( FLAG .NE. 0 ) THEN
         WRITE( XMSG,'(I6, A)' ) FLAG, 'Error in MPI_INIT.'
         CALL M3WARN ( PNAME, 0, 0, XMSG )
         IERR = 1; RETURN
      END IF
#endif

C Get wall-clock time
      CLOCK = MPI_WTIME()

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
