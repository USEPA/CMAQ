
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

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /home/sjr/cvs2git/TOOLS/src/sitecmp/module_envvar.F,v 1.5 2011/10/21 14:41:32 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C*************************************************************************
C
C  MODULE:  defines and gets environment variables
C             
C*************************************************************************
      MODULE ENV_VARS

      CHARACTER*256  SITE_FILE       ! Site info file

      CHARACTER*16   TABLE_TYPE      ! Table type on input (IMPROVE, CASTNET, MDN, NADP, STN)

      CHARACTER*256  IN_TABLE        ! Input table of actual values  

      CHARACTER*256  OUT_TABLE       ! Output table containing both actual and modeled values 

      INTEGER        START_DATE      ! starting date of period to process
      INTEGER        START_TIME      ! starting time of period to process
      INTEGER        END_DATE        ! ending date of period to process
      INTEGER        END_TIME        ! ending time of period to process
      INTEGER        TIME_SHIFT      ! hours to adjust model data

      LOGICAL        APPLYDLS        ! Daylight Savings flag

      CHARACTER*16   PRECIP_FIELD    ! name of precip species in wet concentration calculations

      CHARACTER*10   MISSING_VALUE   ! indicates missing value in output

      CONTAINS

         SUBROUTINE GET_ENVS
C*************************************************************************
C
C  FUNCTION:  To get environment variables
C             
C        
C*************************************************************************
         USE M3UTILIO

         IMPLICIT NONE 

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
         CHARACTER*16    PNAME            ! Program Name
         CHARACTER*16    TABLETYPE        ! Env var for table type
         CHARACTER*16    INTABLE          ! Env var for input table file name
         CHARACTER*16    OUTTABLE         ! Env var for output table file name
         CHARACTER*16    APPLY_DLS        ! Env var that indicates if DLS should apply
         CHARACTER*16    MISSING          ! Env var to use to indicate missing values  

         CHARACTER*80    MSG              ! Error message
         CHARACTER*256   RET_VAL          ! Returned value of env var
         CHARACTER*16    ENV_DFLT         ! default env value 
         CHARACTER*16    ENV_DESC         ! message string
         INTEGER   STATUS                 ! Status code
         LOGICAL   LERROR                 ! Error flag

         CHARACTER*16    VARS(20)         ! Env var for list of variables
         INTEGER I

C**********************************************************************
         DATA  PNAME           / 'GET_ENVS'        /
         DATA  TABLETYPE       / 'TABLE_TYPE'      /
         DATA  INTABLE         / 'IN_TABLE'        /
         DATA  OUTTABLE        / 'OUT_TABLE'       /  
         DATA  APPLY_DLS       / 'APPLY_DLS'       /  
         DATA  MISSING         / 'MISSING'         /  

         LERROR = .FALSE.
   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the table type
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ENV_DFLT = ''
         ENV_DESC = 'Table type (AVG_DAILY or AVG_PERIOD)'        
         CALL ENVSTR( TABLETYPE, ENV_DESC, ENV_DFLT, TABLE_TYPE, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'WARNING: No table type assigned'
            CALL M3MESG( MSG )
         ENDIF

         IF( TABLE_TYPE.ne.'IMPROVE' .and.
     &       TABLE_TYPE.ne.'CASTNET' .and.
     &       TABLE_TYPE.ne.'SEARCH'  .and.
     &       TABLE_TYPE.ne.'STN'     .and.
     &       TABLE_TYPE.ne.'MDN'     .and.
     &       TABLE_TYPE.ne.'MET'     .and.
     &       TABLE_TYPE.ne.'OUTPUT'  .and.
     &       TABLE_TYPE.ne.'AIRMON'  .and.
     &       TABLE_TYPE.ne.'DEARS'   .and.
     &       TABLE_TYPE.ne.'NADP') THEN
               MSG = 'ERROR: Invalid table type defined, must be '//
     &               '(IMPROVE, CASTNET, SEARCH, STN, MDN, MET, NADP, DEARS, AIRMON, OUTPUT)'
               CALL M3MESG( MSG )
               LERROR = .TRUE.
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the name of the input table file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ENV_DFLT = 'INTABLE'
         ENV_DESC = 'Input Table name'        
         CALL ENVSTR( INTABLE, ENV_DESC, ENV_DFLT, IN_TABLE, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: no file name assigned to '//INTABLE
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the name of the site file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ENV_DFLT = 'SITEFILE'
         ENV_DESC = 'Site file name'        
         CALL ENVSTR( 'SITE_FILE', ENV_DESC, ENV_DFLT, SITE_FILE, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: no file name assigned to SITEFILE '
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         ENDIF


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the name of the output table file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ENV_DFLT = 'OUTTABLE'
         ENV_DESC = 'Output Table name'        
         CALL ENVSTR( OUTTABLE, ENV_DESC, ENV_DFLT, OUT_TABLE, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: no file name assigned to '//OUTTABLE
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the starting and ending dates and times
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C...get starting date and time
      start_date = ENVINT('START_DATE','Start date for average period', 0, STATUS)
      start_time = ENVINT('START_TIME','Start time for average period', 0, STATUS)

C...get ending date and time
      end_date = ENVINT('END_DATE','End date for average period', 0, STATUS)
      end_time = ENVINT('END_TIME','End time for average period', 0, STATUS)


      if(( start_date .gt. end_date ) .or.
     &   ( start_date .eq. end_date .and. start_time .gt. end_time )) then
        MSG = '**Error** Invalid time window defined, start_time > end_time'
        CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. ) 
        Stop
        Endif


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the TIME Shift adjustment in hours    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
         ENV_DESC = 'Hours added to model data read'        
         TIME_SHIFT= ENVINT( 'TIME_SHIFT', ENV_DESC, 0, STATUS)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the DLS apply flag (default is false)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
         ENV_DESC = 'Flag to apply Daylight Saving adjustment'        
         APPLYDLS  = ENVYN( APPLY_DLS, ENV_DESC, .FALSE., STATUS)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the Precip species to use in Volume_Weight calculations           
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ENV_DESC = 'Precip species used in wet concentrations'        
         CALL ENVSTR('PRECIP', ENV_DESC, 'Precip', PRECIP_FIELD, STATUS)
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the Character string to use for missing values
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
         ENV_DESC = 'String to indicate missing values'        
         CALL ENVSTR( MISSING, ENV_DESC, '-999', MISSING_VALUE, STATUS)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Stop if errors detected
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         IF( LERROR ) THEN
            MSG = 'Error in assigning environment variables -- stopping'
            CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. ) 
         ENDIF


         RETURN

         END SUBROUTINE GET_ENVS

      END MODULE ENV_VARS
