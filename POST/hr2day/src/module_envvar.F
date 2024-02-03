
!-----------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in    !
!  continuous development by various groups and is based on information !
!  from these groups: Federal Government employees, contractors working !
!  within a United States Government contract, and non-Federal sources  !
!  including research institutions.  These groups give the Government   !
!  permission to use, prepare derivative works of, and distribute copies!
!  of their work in the CMAQ system to the public and to permit others  !
!  to do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software, !
!  but users are requested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is   !
!  subject to their copyright restrictions.                             !
!-----------------------------------------------------------------------!


C*************************************************************************
C
C  MODULE:  defines and gets environment variables
C             
C*************************************************************************
      MODULE ENV_VARS

      INTEGER        START_DATE      ! starting date of period to process
      INTEGER        END_DATE        ! ending date of period to process
      INTEGER        STARTHR         ! starting hour of the daily time period to process
      INTEGER        ENDHR           ! ending hour of the daily time period to process
      INTEGER        HOURS_8HRMAX    ! Number of hours to use for 8HR Max

      LOGICAL        PARTDAY         ! Compute partial day for last day
      LOGICAL        USELOCAL        ! Use local time
      LOGICAL        USEDST          ! Daylight Savings flag

      CHARACTER*(16) OZONEVAR        ! Ozone variable
      CHARACTER*(16) TEMPVAR         ! Temperature variable

      CONTAINS

         SUBROUTINE GET_ENVS
C*************************************************************************
C
C  FUNCTION:  To get environment variables
C             
C        
C*************************************************************************
         USE M3UTILIO
         USE TIME_STEP

         IMPLICIT NONE 

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
         CHARACTER*16    PNAME            ! Program Name

         CHARACTER*80    MSG              ! Error message
         INTEGER   STATUS                 ! Status code

         INTEGER I

C**********************************************************************
         DATA  PNAME           / 'GET_ENVS'        /
   


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the starting and ending dates and times
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C...get starting date and time
      start_date = ENVINT('START_DATE','Start date for output period', STEP_DATE(1), STATUS)

C...get ending date and time
      end_date = ENVINT('END_DATE','End date for output period', STEP_DATE(NSTEPS), STATUS)


      if( start_date .gt. end_date ) then
        MSG = '**Error** Invalid time window defined, START_DATE > END_DATE'
        CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. ) 
        Stop
      Endif

C... get starting and ending hours for time window
      startHr = ENVINT('START_HOUR',"Starting Hour of daily time window", 0, status)
      endHr = ENVINT('END_HOUR',"Ending Hour of  daily time window", 23, status)

      if( startHr .gt. endHr ) then
        MSG = '**Error** Invalid time window defined, START_HOUR > END_HOUR'
        CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. ) 
        Stop
      Endif


C... get compute partial day switch
      partDay = ENVYN('PARTIAL_DAY',"Compute partial day values", .FALSE., status)

C... get use local time switch
      useLocal = ENVYN('USELOCAL',"Use local time", .FALSE., status)

C... get use daylight savings time switch id uselocal
      useDST = .false.
      if( useLocal ) then
        useDST = ENVYN('USEDST',"Use Daylight Savings time", .FALSE., status)
        endif

C... Get the HOURS_8HRMAX environment variable (default is 24)                                                          
       HOURS_8HRMAX = ENVINT( 'HOURS_8HRMAX', 
     &  'Number of 8hr values to use when computing DM8HR', 24, STATUS)  

       if ( ( HOURS_8HRMAX .NE. 24) .AND. ( HOURS_8HRMAX .NE. 17) ) THEN                                

        MSG = '**Error** Invalid value for HOURS_8HRMAX, use 24 or 17'
        CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. ) 
        Stop
       Endif


C... get temperature variable

      Call ENVSTR('TEMPERATURE','Temperature species used in @MAXT operation',
     &              'TEMP2', tempvar, status)  

C... get ozone variable

      Call ENVSTR('OZONE','Ozone species used in @8HRMAXO3 operation',
     &              'O3', ozonevar, status)  


      RETURN

      END SUBROUTINE GET_ENVS

      END MODULE ENV_VARS
