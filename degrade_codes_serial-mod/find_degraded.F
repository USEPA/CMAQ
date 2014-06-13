C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

         SUBROUTINE FIND_DEGRADED( JDATE, JTIME, LOGDEV, CALL_DEGRADE )

C**********************************************************************
C
C  Function:  Set Logic Flag in whether to call degradation routines
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
