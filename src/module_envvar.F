
      MODULE ENV_VARS

      IMPLICIT NONE
      
      CHARACTER(  15 )  :: CR_DATE     ! Run date

      character(256)  :: OMI_FILE_LIST 
      character(256)  :: OMI_FULL_DAT 
      character(256)  :: OMI_CMAQ_DAT 

      CHARACTER( 256 )  :: OUTPATH     ! Output directory
      
      INTEGER :: NPOINTS_LAT = 17
      INTEGER :: NPOINTS_LON = 17
      
      REAL    :: LAT_BORDER = 10.0

      LOGICAL  :: LUSE_PREV_DATE       ! Flag to replace missing with previous date
      LOGICAL  :: LUSE_NEIGHBORS       ! Flag to use nearest neighbor average
      LOGICAL  :: CREATE_FULL_FILES    ! Flag to create ASCII and Netcdf files with 
                                       ! Lat/Lon resolution of observations


      INTEGER, PARAMETER :: LOGDEV = 6
      
      CONTAINS

         SUBROUTINE GET_ENVS
C*************************************************************************
C
C        FUNCTION: Gets environment variables
C             
C        PRECONDITIONS: None
C 
C        KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C        REVISION HISTORY: Created by Jerry Gipson, February, 2004
C                          Modified Nov 04 by JG to get group species names
C                            vi environment variables
C                          Modified Dec 09 by JG to remove reset of LERROR to
C                             false for each species name retrieval
C                          Modified Dec 09 by JG to allow for inclusion
C                             of NO2EX cycle in Groups 1 and 2
C                   
C*************************************************************************


       USE GET_ENV_VARS           ! IOAPI parameters and functions declarations

        IMPLICIT NONE

C..INCLUDES: None
      
C..ARGUMENTS: None
    
C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:

         CHARACTER(  16 ) :: PNAME = 'GET_ENVS'     ! Program Name

         CHARACTER(   5 ) :: TZ          ! Time zone
         CHARACTER(  16 ) :: ENV_DFLT    ! Environment variable default value
         CHARACTER(  24 ) :: TIMNDAT     ! Wall clock time and date
         CHARACTER(  80 ) :: ENV_DESC    ! Environment variable description
         CHARACTER(  80 ) :: MSG         ! Message text
         CHARACTER( 256 ) :: RET_VAL     ! Returned value of env var

c........env var names 
!        CHARACTER(  16 ) :: OUTDIR        = 'OUTDIR'
         CHARACTER(  16 ) :: USE_PREV_DATE = 'PREV_DATE'
         CHARACTER(  16 ) :: USE_NEIGHBORS = 'NEIGHBORS'
         CHARACTER(  16 ) :: FULL_FILES    = 'FULL_FILES'
         character(  16 ) :: OMI_FILE      = 'OMI_FILE_LIST'
         character(  16 ) :: OMI_FULL      = 'OMI_FULL_DAT'
         character(  16 ) :: OMI_CMAQ      = 'OMI_CMAQ_DAT'

         character(  16 ) :: NLAT_OMI      = 'NLAT_OMI'
         character(  16 ) :: NLON_OMI      = 'NLON_OMI'
         character(  16 ) :: BORDER_LAT    = 'LAT_BORDER'

         INTEGER :: EPOS                  ! String end position
         INTEGER :: IO_STATUS                ! Status code
         INTEGER :: IDATE                 ! Wall-clock date
         INTEGER :: ITIME                 ! Wall clock time

         LOGICAL :: LERROR                ! Error Flag


C**********************************************************************

         LERROR = .FALSE.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the time and date to be used as the time stamp for all outputs;
c  Convert from GMT to EDT or EST
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         CALL GET_DDMONYY(CR_DATE)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the name of the output directory and template directory
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!        ENV_DFLT = './output'
!        ENV_DESC = 'Output directory'        
!        CALL GET_ENV_STRING( OUTDIR, ENV_DESC, ENV_DFLT, OUTPATH, IO_STATUS)

!        IF( IO_STATUS .GT. 0 ) THEN
!           MSG = 'ERROR: no path assigned to OUTDIR'
!           WRITE(LOGDEV,'(a)')TRIM( MSG )
!        END IF

         ENV_DFLT = './omi_file.txt'
         ENV_DESC = 'file list OMI data file'        
         CALL GET_ENV_STRING( OMI_FILE, ENV_DESC, ENV_DFLT, OMI_FILE_LIST, IO_STATUS)

         IF( IO_STATUS .NE. 0 ) THEN
            MSG = 'ERROR: no path assigned to file listing omi files'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
            LERROR = .TRUE.
         END IF

         CREATE_FULL_FILES = .FALSE.
         ENV_DESC          = 'Output files include ASCII and IOAPI files at Observed Lat/Lon Resolution.'
         CREATE_FULL_FILES = GET_ENV_FLAG( FULL_FILES, ENV_DESC, CREATE_FULL_FILES, IO_STATUS )
         
         IF( CREATE_FULL_FILES )THEN
            ENV_DFLT = './omi_full.dat'
            ENV_DESC = 'full resolution OMI data in ascii format'        
            CALL GET_ENV_STRING( OMI_FULL, ENV_DESC, ENV_DFLT, OMI_FULL_DAT, IO_STATUS)
            IF( IO_STATUS .NE. 0 ) THEN
               MSG = 'ERROR: no path assigned to full resolution OMI data file'
               WRITE(LOGDEV,'(a)')TRIM( MSG )
            END IF
         END IF

!        ENV_DFLT = './omi_full.ncf'
!        ENV_DESC = 'full resolution OMI data in IOAPI format'        
!        CALL GET_ENV_STRING( 'OMI_FULL_NCF', ENV_DESC, ENV_DFLT, OMI_FULL_NCF, IO_STATUS)

!        IF( IO_STATUS .NE. 0 ) THEN
!           MSG = 'ERROR: no path assigned to full resolution IOAPI OMI file'
!           WRITE(LOGDEV,'(a)')TRIM( MSG )
!        END IF

         ENV_DFLT = './omi_cmaq.dat'
         ENV_DESC = 'subset of OMI data for CMAQ'        
         CALL GET_ENV_STRING( OMI_CMAQ, ENV_DESC, ENV_DFLT, OMI_CMAQ_DAT, IO_STATUS)

         IF( IO_STATUS .NE. 0 ) THEN
            MSG = 'ERROR: no path assigned to subset OMI data file'
            WRITE(LOGDEV,'(a)')TRIM( MSG )
         END IF

         ENV_DESC = 'Number of latitude points in output file'        
         NPOINTS_LAT = GET_ENV_INT( NLAT_OMI, ENV_DESC, NPOINTS_LAT, IO_STATUS )
         IF( IO_STATUS .NE. 0 ) THEN
            MSG = 'ERROR: in environment variable: ' // TRIM( NLAT_OMI )
            WRITE(LOGDEV,'(a)')TRIM( MSG )
         END IF

         ENV_DESC = 'Number of longitude points in output file'        
         NPOINTS_LON = GET_ENV_INT( NLON_OMI, ENV_DESC, NPOINTS_LON, IO_STATUS )
         IF( IO_STATUS .NE. 0 ) THEN
            MSG = 'ERROR: in environment variable: ' // TRIM( NLON_OMI )
            WRITE(LOGDEV,'(a)')TRIM( MSG )
         END IF

         ENV_DESC = 'Distance of first and last latitude point from nearest pole (degrees)'        
         LAT_BORDER = GET_ENV_REAL( BORDER_LAT, ENV_DESC, LAT_BORDER, IO_STATUS )
         IF( IO_STATUS .NE. 0 ) THEN
            MSG = 'ERROR: in environment variable: ' // TRIM( BORDER_LAT )
            WRITE(LOGDEV,'(a)')TRIM( MSG )
         END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the flags for replacing missing values
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LUSE_PREV_DATE  = .TRUE.
         ENV_DESC        = 'replace missing values with last available value'
         LUSE_PREV_DATE  = GET_ENV_FLAG( USE_PREV_DATE, ENV_DESC, LUSE_PREV_DATE, IO_STATUS )

         LUSE_NEIGHBORS  = .FALSE.
         ENV_DESC = 'replace missing values with weighted average of nearest neighbors'
         LUSE_NEIGHBORS  = GET_ENV_FLAG( USE_NEIGHBORS, ENV_DESC, LUSE_NEIGHBORS, IO_STATUS )

         IF( LERROR )THEN
            WRITE(LOGDEV,'(a)')'See above error in environment variables'
            STOP
         END IF

         RETURN

92000    FORMAT( /2X, 'The following mechanism versions will be created:' 
     &           / A / 1X )

         END SUBROUTINE GET_ENVS

      END MODULE ENV_VARS
