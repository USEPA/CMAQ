
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
C $Header: /project/yoj/arc/CCTM/src/gas/ros3/find_degraded.F,v 1.1 2011/09/22 18:41:19 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

         SUBROUTINE FIND_DEGRADED( JDATE, JTIME, LOGDEV, CALL_DEGRADE )

C**********************************************************************
C
C  Function:  call routine degrade data and 
C             Check decay and degradation rates for negative values.
C
C  CALLED BY: HRDRIVER
C
C**********************************************************************

           USE DEGRADE_SETUP_TOX

           IMPLICIT NONE

C.....INCLUDES: NONE

C.....ARGUMENTS:

           INTEGER, INTENT( IN )  :: JDATE        ! current model date , coded YYYYDDD
           INTEGER, INTENT( IN )  :: JTIME        ! current model time , coded HHMMSS
           INTEGER, INTENT( IN )  :: LOGDEV       ! unit number for the log file
           LOGICAL, INTENT( OUT ) :: CALL_DEGRADE ! whether to call degradation routines

C.....LOCAL VARIABLES:

           CHARACTER( 144 )        :: XMSG                    ! Message text
           CHARACTER( 16  ), SAVE  :: PNAME = 'FIND_DEGRADED' ! Routine name

           CALL DEGRADE_DATA()

           CALL DEGRADE_MAP( JDATE, JTIME, LOGDEV )

           IF( N_REACT_FOUND .GT. 0 )THEN
               CALL_DEGRADE = .TRUE.
           ELSE
               CALL_DEGRADE = .FALSE.
           ENDIF

           RETURN

         END SUBROUTINE FIND_DEGRADED
