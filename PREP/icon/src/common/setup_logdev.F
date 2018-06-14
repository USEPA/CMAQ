
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
C $Header: /project/yoj/arc/ICON/src/common/setup_logdev.F,v 1.2 2011/10/21 16:41:52 yoj Exp $

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
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                        removed deprecated TRIMLEN

      USE UTILIO_DEFN

      IMPLICIT NONE

#ifdef parallel      
      INCLUDE SUBST_MPI
#endif

      CHARACTER( 48 ) :: EQNAME
      CHARACTER(  9 ) :: PRESTR = 'ICON_LOG_'
      CHARACTER(  3 ) :: CMYPE
      CHARACTER( 96 ) :: IOLOGEQ
      CHARACTER( 16 ) :: APPL = 'ICON_APPL'

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
            IOLOGEQ = PRESTR // CMYPE // '.' // TRIM( EQNAME )
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
