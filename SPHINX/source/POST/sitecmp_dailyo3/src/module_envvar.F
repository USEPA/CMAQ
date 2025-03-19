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

      MODULE ENV_VARS

C*************************************************************************
C
C  FUNCTION:  To get environment variables
C             
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, July, 1999
C                   
C*************************************************************************

      CHARACTER*256  IN_TABLE        ! Input table of actual values  

      CHARACTER*256  OUT_TABLE       ! Output table containing both actual and modeled values 
      CHARACTER*256  AVG_TABLE       ! Output table containing averages over time period

      INTEGER        START_DATE      ! starting date of period to process
      INTEGER        START_TIME      ! starting time of period to process
      INTEGER        END_DATE        ! ending date of period to process
      INTEGER        END_TIME        ! ending time of period to process

      INTEGER        HOUR_START      ! starting hour for partial day
      INTEGER        HOUR_END        ! ending hour for partial day     

      INTEGER        HOURS_8HRMAX    ! number of 8hr values to compute 8hr max     

      INTEGER        TIME_SHIFT      ! hours to adjust model data                                  
 
      LOGICAL        APPLYDLS        ! Daylight Savings flag

!      LOGICAL        COMPUTE_MAX     ! Output maximun hourly values flag
!      LOGICAL        COMPUTE_SUM06   ! Output SUM06 values flag

      LOGICAL        LAMBXY          ! Output Lambert x,y flag

      CHARACTER*16   MISSING_VALUE   ! indicates missing value in output

!      CHARACTER*16   NETWORK_OBS     ! indicates observational network (AQS or CASTNET)

      CHARACTER*16   OBS_SPECIES_NAME! name of the observed species in the header line

      CHARACTER*16   QA_FLAG_NAME    ! name of the qa flag field in the header line

      CHARACTER*16   QA_FLAG_STRING  ! string of QA flags indicating invalid obs

      LOGICAL        FILL_MISSING    ! Fill total period even if observed data is missing 

      LOGICAL        MISS_CHECK      ! check for incomplete number of values in day       

      LOGICAL        QAFLAG_CHECK    ! does the observation file include a QA flag?      

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

C..INCLUDES:  
C         INCLUDE SUBST_IOPARMS     ! IOAPI parameters
C         INCLUDE SUBST_IOFDESC     ! IOAPI file description
C         INCLUDE SUBST_IODECL      ! IOAPI declarations
      

C..EXTERNAL FUNCTIONS:
C         INTEGER INDEX1
C         INTEGER ENVINT
C         LOGICAL ENVYN
C         LOGICAL STRLIST
         INTEGER getParsedNumber

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
         CHARACTER*16    PNAME            ! Program Name
         CHARACTER*16    INTABLE          ! Env var for input table file name
         CHARACTER*16    OUTTABLE         ! Env var for output table file name
         CHARACTER*16    APPLY_DLS        ! Env var that indicates if DLS should apply
         CHARACTER*16    MISSING          ! Env var to use to indicate missing values  
         CHARACTER*16    NETWORK          ! Env var to use to indicate network  
         CHARACTER*16    PARTIAL_DAY      ! Env var to use to define hours for partial day
         
         CHARACTER*16   OBS_SPECIES       ! name of the observed species in the header line
         CHARACTER*16   QA_FLAG_CHECK     ! name of the qa flag field in the header line
         CHARACTER*16   QA_FLAG_HEADER    ! name of the qa flag field in the header line
         CHARACTER*16   QA_FLAG_VALUES    ! string of QA flags indicating invalid obs

         CHARACTER*80    MSG              ! Error message
         CHARACTER*256   RET_VAL          ! Returned value of env var
         CHARACTER*16    ENV_DFLT         ! default env value 
         CHARACTER*16    ENV_DESC         ! message string
         INTEGER   STATUS                 ! Status code
         LOGICAL   LERROR                 ! Error flag

         CHARACTER*80    RECORD          
         CHARACTER*16    FIELD           

         CHARACTER*16    VARS(20)         ! Env var for list of variables
         INTEGER I

C**********************************************************************
         DATA  PNAME           / 'GET_ENVS'        /
         DATA  INTABLE         / 'IN_TABLE'        /
         DATA  OUTTABLE        / 'OUT_TABLE'       /  
         DATA  APPLY_DLS       / 'APPLY_DLS'       /  
         DATA  MISSING         / 'MISSING'         /  
         DATA  NETWORK         / 'NETWORK'         /  
         DATA  PARTIAL_DAY     / 'PARTIAL_DAY'     /  
         DATA  OBS_SPECIES     / 'OBS_SPECIES'  /  
         DATA  QA_FLAG_HEADER  / 'QA_FLAG_HEADER'  /  
         DATA  QA_FLAG_VALUES  / 'QA_FLAG_VALUES'  /  
         DATA  QA_FLAG_CHECK   / 'QA_FLAG_CHECK'   /  

         LERROR = .FALSE.
   


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
c  Get the name of the output table file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ENV_DFLT = 'out.csv'
         ENV_DESC = 'Output Table name'        
         CALL ENVSTR( OUTTABLE, ENV_DESC, ENV_DFLT, OUT_TABLE, STATUS)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the Character string to indicate the name of the obs variable
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
         ENV_DFLT = 'O3'
         ENV_DESC = 'String to indicate observed species name'        
         CALL ENVSTR( OBS_SPECIES, ENV_DESC, ENV_DFLT, OBS_SPECIES_NAME, STATUS)

         IF( STATUS .NE. 0 ) THEN
           MSG = 'WARNING: no value assigned to '//OBS_SPECIES
           CALL M3MESG( MSG )
           MSG = 'USING DEFAULT '//ENV_DFLT
           CALL M3MESG( MSG )
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
c  Get the HOURS_8HRMAX environment variable (default is 24)                                                          
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc                           
       ENV_DESC = 'Number of 8hr values to use when computing DM8HR'                                               
       HOURS_8HRMAX= ENVINT( 'HOURS_8HRMAX', ENV_DESC, 24, STATUS)  
	 
       if ( ( HOURS_8HRMAX .NE. 24) .AND. ( HOURS_8HRMAX .NE. 17) ) THEN                                

        MSG = '**Error** Invalid value for HOURS_8HRMAX, use 24 or 17'
        CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. ) 
        Stop
       Endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the DLS apply flag (default is false)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
         ENV_DESC = 'Flag to apply Daylight Saving adjustment'        
         APPLYDLS  = ENVYN( APPLY_DLS, ENV_DESC, .FALSE., STATUS)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the COMPUTE_MAX flag (default is false)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
c         ENV_DESC = 'Flag to output maximun hourly values'        
c         COMPUTE_MAX  = ENVYN( 'COMPUTE_MAX', ENV_DESC, .FALSE., STATUS)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
c  Get the HOUR_VALUES flag (default is false)                                
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
c         ENV_DESC = 'Flag to output SUM06 values'                    
c         COMPUTE_SUM06  = ENVYN( 'COMPUTE_SUM06', ENV_DESC, .FALSE., STATUS)  

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the LAMBXY flag (default is false)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
         ENV_DESC = 'Flag to output Lambert X,Y values'        
         LAMBXY  = ENVYN( 'LAMBXY', ENV_DESC, .FALSE., STATUS)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the Character string to use for missing values
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
         ENV_DESC = 'String to indicate missing values'        
         CALL ENVSTR( MISSING, ENV_DESC, 'm', MISSING_VALUE, STATUS)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the Character string to indicate CASTNET or AQS observations (default AQS)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
c         ENV_DFLT = 'AQS'
c         ENV_DESC = 'String to indicate network (AQS or CASTNET)'        
c         CALL ENVSTR( NETWORK, ENV_DESC, ENV_DFLT, NETWORK_OBS, STATUS)

c         if(( TRIM(NETWORK_OBS) .ne. 'AQS' ) .and.
c     &      ( TRIM(NETWORK_OBS) .ne. 'CASTNET' )) then
c          MSG = '**Error** NETWORK MUST BE SET TO AQS OR CASTNET'
c          CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. ) 
c          Stop
c         Endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the flag to see if a QA field is present in obs file (default is false)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
         ENV_DESC = 'Flag to indicate whether QA flag field is present'        
         QAFLAG_CHECK = ENVYN( QA_FLAG_CHECK, ENV_DESC, .FALSE., STATUS)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the Character strings for the QA flag header and QA flag strings
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
         IF (QAFLAG_CHECK) THEN
         
          ENV_DFLT = 'OZONE_F'
          ENV_DESC = 'Header string to indicate QA flag'        
          CALL ENVSTR( QA_FLAG_HEADER, ENV_DESC, ENV_DFLT, QA_FLAG_NAME, STATUS)

          IF( STATUS .NE. 0 ) THEN
            MSG = 'WARNING: no value assigned to '//QA_FLAG_HEADER
            CALL M3MESG( MSG )
            MSG = 'USING DEFAULT '//ENV_DFLT
            CALL M3MESG( MSG )
          ENDIF

          ENV_DFLT = 'BCDFIMP'
          ENV_DESC = 'String containing the QA flag values to indicate bad data'        
          CALL ENVSTR( QA_FLAG_VALUES, ENV_DESC, ENV_DFLT, QA_FLAG_STRING, STATUS)

          IF( STATUS .NE. 0 ) THEN
            MSG = 'WARNING: no value assigned to '//QA_FLAG_VALUES
            CALL M3MESG( MSG )
            MSG = 'USING DEFAULT '//ENV_DFLT
            CALL M3MESG( MSG )
          ENDIF

         ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the switch for filling total time period
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
         ENV_DESC = 'Flag to indicate to fill missing data records'        
         FILL_MISSING = ENVYN('FILL_MISSING', ENV_DESC, .FALSE., STATUS)
         MISS_CHECK = ENVYN('MISS_CHECK', 'Set Incomplete days to missing', .TRUE., STATUS)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
c  Get the partial day hours
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
         ENV_DFLT = ''                                
         ENV_DESC = 'partial day start and end hours'         
         CALL ENVSTR( PARTIAL_DAY, ENV_DESC, ENV_DFLT, RECORD, STATUS)      
         
         HOUR_START = -1
         HOUR_END = -1                     
 
         if( getParsedNumber(record, ',') .eq. 2 ) Then
           Call getParsedField( record, ',', 1, field, .false. )
           read(field,*,iostat=status) hour_start
           if(status.ne.0) hour_start = -1

           Call getParsedField( record, ',', 2, field, .false. ) 
           read(field,*,iostat=status) hour_end 
           if(status.ne.0) hour_end = -1 

           IF( hour_end .lt. hour_start ) THEN
             MSG = 'Error invalid partial day hours -- stopping'    
             CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )            
             ENDIF 
           endif
  
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
