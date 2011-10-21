
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
C $Header: /project/yoj/arc/CCTM/src/par/par_noop/par_term_noop.F,v 1.2 2011/10/21 16:11:26 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE PAR_TERM ( CALLER, JDATE, JTIME, BEGTIME )
C .....................................................................
 
C  PURPOSE:    Performs no-operation for parallel termination. 
 
C  REVISION HISTORY: 
C       Original version 10/1998 by Al Bourgeois for parallel implementaion.
C       Modified 03/29/11 S.Roselle: Replaced I/O API include files with
C                UTILIO_DEFN
 
C  ARGUMENT LIST DESCRIPTION: 
C  IN:
C      CHARACTER*16    CALLER    ! Program name.
C      INTEGER         JDATE     ! Current model date, coded YYYYDDD.
C      INTEGER         JTIME     ! Current model time, coded HHMMSS.
C      REAL            BEGTIME   ! Wall-clock time (sec) at initialization.
C                      ^^^^^^^ not used
C  OUT: none
 
C  LOCAL VARIABLE DESCRIPTION:  see below
 
C  CALLS: None 
 
C........................................................................

      USE UTILIO_DEFN

      IMPLICIT  NONE

C Include Files: None

C Arguments:

      CHARACTER( 16 ) :: CALLER     ! Program name.
      INTEGER JDATE      ! Current model date, coded YYYYDDD.
      INTEGER JTIME      ! Current model time, coded HHMMSS.
      REAL    BEGTIME    ! Wall-clock time (sec) at initialization.

      CHARACTER( 80 ) :: XMSG

C........................................................................
C begin function PAR_TERM( CALLER, JDATE, JTIME, BEGTIME )

C Shut down IOAPI     

      IF ( SHUT3() ) THEN
         XMSG = '>>---->  Program completed successfully  <----<<'
         WRITE ( *,'( //5X, A, // )' ) XMSG
         STOP
      ELSE
         XMSG = ' *** FATAL ERROR shutting down Models-3 I/O *** '
         WRITE ( *,'( //5X, A, // )' ) XMSG
         CALL M3EXIT ( 'PAR_TERM', 0, 0, XMSG, 1 )
      END IF

      RETURN
      END
