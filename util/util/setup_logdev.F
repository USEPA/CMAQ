
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
C $Header: /project/yoj/arc/CCTM/src/util/util/setup_logdev.F,v 1.2 2011/02/23 20:57:16 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      FUNCTION SETUP_LOGDEV ()  RESULT ( LOGDEV )

C Purpose
C   Establish ancillary run time log files for each parallel processor
C   Effectively no operation, if serial

C Revision History:
C   ???      J.Young - initial
C   26 Dec, 01 David Wong
C      -- removed MPI_INIT call since the same function has been called in
C         subroutine PAR_INIT
C   11 Jun 02 J.Young - minor mods
C   23 Feb 11 J.Young - SETENVVAR is a function, not a subroutine

      IMPLICIT NONE
#include      SUBST_IODECL      # I/O definitions and declarations

#ifdef parallel      
      INCLUDE SUBST_MPI
#endif

      CHARACTER( 48 ) :: EQNAME
      CHARACTER(  8 ) :: PRESTR = 'CTM_LOG_'
      CHARACTER(  3 ) :: CMYPE
      CHARACTER( 96 ) :: IOLOGEQ
      CHARACTER( 16 ) :: APPL = 'CTM_APPL'

      INTEGER, EXTERNAL :: TRIMLEN
      LOGICAL, EXTERNAL :: SETENVVAR
      INTEGER :: ERROR, MYPE
      INTEGER :: LOGDEV

#ifdef parallel
      CALL MPI_COMM_RANK ( MPI_COMM_WORLD, MYPE, ERROR )
#else
      MYPE = 0
#endif

      IF ( MYPE .NE. 0 ) THEN
         CALL NAMEVAL ( APPL, EQNAME )
         WRITE ( CMYPE, '(I3.3)' ) MYPE
         IF ( APPL .NE. EQNAME(1:16 ) ) THEN
            IOLOGEQ = PRESTR // CMYPE // '.' // EQNAME( 1:TRIMLEN( EQNAME ) )
         ELSE
            IOLOGEQ = PRESTR // CMYPE
         END IF
         IF ( .NOT. SETENVVAR ( 'LOGFILE', IOLOGEQ ) ) THEN
            WRITE( *,* ) '*** Could not set environment variable for ' // IOLOGEQ
            STOP
         END IF

      END IF

      LOGDEV = INIT3()

      END FUNCTION SETUP_LOGDEV 
