
      MODULE ENV_VARS

      IMPLICIT NONE
      
      CHARACTER(  15 )  :: CR_DATE     ! Run date
      CHARACTER(  16 )  :: AE_TAG      ! aerosol name used in MECHNAME
      CHARACTER(  16 )  :: GC_TAG      ! gas chem name used in MECHNAME
      CHARACTER(  16 )  :: AQ_TAG      ! aq chem name used in MECHNAME

      CHARACTER(  16 )  :: MECH_NO     ! Mech name for NO
      CHARACTER(  16 )  :: MECH_NO2    ! Mech name for NO2
      CHARACTER(  16 )  :: MECH_O3     ! Mech name for O3
      CHARACTER(  16 )  :: MECH_O3P    ! Mech name for O3P
      CHARACTER(  16 )  :: MECH_O1D    ! Mech name for O1D
      CHARACTER(  16 )  :: MECH_OH     ! Mech name for OH
      CHARACTER(  16 )  :: MECH_HO2    ! Mech name for HO2
      CHARACTER(  16 )  :: MECH_HONO   ! Mech name for HONO
      CHARACTER(  16 )  :: MECH_HNO4   ! Mech name for HNO4
      CHARACTER(  16 )  :: MECH_PAN    ! Mech name for PAN
      CHARACTER(  16 )  :: MECH_C2O3   ! Mech name for C2O3
      CHARACTER(  16 )  :: MECH_N2O5   ! Mech name for N2O5
      CHARACTER(  16 )  :: MECH_NO3    ! Mech name for NO3
      CHARACTER(  16 )  :: MECH_NO2EX  ! Mech name for NO2EX

      CHARACTER( 256 )  :: OUTPATH     ! Output directory
      CHARACTER( 256 )  :: TMPLPATH    ! Output directory
      
      LOGICAL  :: L_ALL_MECHS          ! Flag for all mechs (F=1 mech)

      LOGICAL  :: LWR_COPY             ! Flag to include copyright notice
      LOGICAL  :: LWR_CVS_HDR          ! Flag to include CVS headr line
      LOGICAL  :: LPAR_NEG             ! Flag for negative PAR stoich (cb4)
      LOGICAL  :: LNO2EX_CYCLE         ! Flag for to include NO2EX cycle in groups 1 & 2


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

         IMPLICIT NONE

C..INCLUDES:  
         INCLUDE SUBST_IOPARMS   ! IOAPI parameters
         INCLUDE SUBST_IODECL    ! IOAPI declarations
      
C..ARGUMENTS: None
    
C..PARAMETERS: None

C..EXTERNAL FUNCTIONS:
         CHARACTER*24 DT2STR      ! Function to get character date and time
         INTEGER ENVINT           ! get value of integer environment variable
         LOGICAL ENVYN            ! get value of logical environment variable
         LOGICAL ISDSTIME         ! True if DST in effect


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
         CHARACTER(  16 ) :: ALL_MECHS     = 'ALL_MECHS'
         CHARACTER(  16 ) :: GC_NAME       = 'GC_NAME'
         CHARACTER(  16 ) :: AE_NAME       = 'AE_NAME'
         CHARACTER(  16 ) :: AQ_NAME       = 'AQ_NAME'
         CHARACTER(  16 ) :: OUTDIR        = 'OUTDIR'
         CHARACTER(  16 ) :: TMPLDIR       = 'TMPLDIR'
         CHARACTER(  16 ) :: COPYRT_FLAG   = 'COPYRT_FLAG'
         CHARACTER(  16 ) :: CVS_HDR_FLAG  = 'CVS_HDR_FLAG'
         CHARACTER(  16 ) :: PAR_NEG_FLAG  = 'PAR_NEG_FLAG'
         CHARACTER(  16 ) :: NO2EX_FLAG    = 'NO2EX_CYCLE'

         INTEGER :: EPOS                  ! String end position
         INTEGER :: LOGDEV                ! Unit no. for output log
         INTEGER :: STATUS                ! Status code
         INTEGER :: IDATE                 ! Wall-clock date
         INTEGER :: ITIME                 ! Wall clock time

         LOGICAL :: LERROR                ! Error Flag


C**********************************************************************

         LOGDEV = INIT3()

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the time and date to be used as the time stamp for all outputs;
c  Convert from GMT to EDT or EST
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL GETDTTIME( IDATE, ITIME )

c..glg work around on 3/19/09
c      IF( ISDSTIME( IDATE ) ) THEN
c         CALL NEXTIME( IDATE, ITIME, -40000 )
c         TZ = ' EDT '
c      ELSE
c         CALL NEXTIME( IDATE, ITIME, -50000 )
c         TZ = ' EST '
c      END IF

          CALL NEXTIME( IDATE, ITIME, -40000 )
          TZ = ' EDT'
c..glg work around on 3/19/09

      TIMNDAT = DT2STR( IDATE, ITIME )
      CR_DATE = ADJUSTL( TIMNDAT( 10 : 24 ) )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get flags for versions of mechanism to create
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         L_ALL_MECHS  = .TRUE.
         ENV_DESC = 'Flag do all mechanisms or one'
         L_ALL_MECHS = ENVYN( ALL_MECHS, ENV_DESC, L_ALL_MECHS, STATUS )


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get names to use for the mechanism
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ENV_DFLT = ' '
         ENV_DESC = 'gas mechanism name'        
         CALL ENVSTR( GC_NAME, ENV_DESC, ENV_DFLT, GC_TAG, STATUS )
         IF( STATUS .NE. 0 ) THEN
           MSG = 'Gas phase mechanism not defined' 
           CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
         END IF

         ENV_DFLT = ' '
         ENV_DESC = 'aerosol mechanism name'        
         CALL ENVSTR( AE_NAME, ENV_DESC, ENV_DFLT, AE_TAG, STATUS )
         IF( STATUS .NE. 0 ) THEN
           MSG = 'Aerosol phase mechanism not defined' 
           CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
         END IF

         ENV_DFLT = ' '
         ENV_DESC = 'aq chem mechanism name'        
         CALL ENVSTR( AQ_NAME, ENV_DESC, ENV_DFLT, AQ_TAG, STATUS )
         IF( STATUS .NE. 0 ) THEN
           MSG = 'Aerosol phase mechanism not defined' 
           CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
         END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the name of the output directory and template directory
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ENV_DFLT = './output'
         ENV_DESC = 'Output directory'        
         CALL ENVSTR( OUTDIR, ENV_DESC, ENV_DFLT, OUTPATH, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: no path assigned to OUTDIR'
            CALL M3MESG( MSG )
         END IF

         ENV_DFLT = './tmpldir'
         ENV_DESC = 'Template directory'        
         CALL ENVSTR( TMPLDIR, ENV_DESC, ENV_DFLT, TMPLPATH, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: no path assigned to TMPLDIR'
            CALL M3MESG( MSG )
         END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the flags for writing copyright and CVS header
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LWR_COPY  = .FALSE.
         ENV_DESC = 'Flag to write copyright notice'
         LWR_COPY = ENVYN( COPYRT_FLAG, ENV_DESC, LWR_COPY, STATUS )

         LWR_CVS_HDR  = .FALSE.
         ENV_DESC = 'Flag to write copyright notice'
         LWR_CVS_HDR = ENVYN( CVS_HDR_FLAG, ENV_DESC, LWR_CVS_HDR, STATUS )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the flag for including PAR negative stoichiometry
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LPAR_NEG  = .FALSE.
         ENV_DESC = 'Flag to include PAR negative stoichiometry'
         LPAR_NEG = ENVYN( PAR_NEG_FLAG, ENV_DESC, LPAR_NEG, STATUS )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the flag for including NO2EX in groups 1 and 2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LNO2EX_CYCLE  = .FALSE.
         ENV_DESC = 'Flag to include PAR negative stoichiometry'
         LNO2EX_CYCLE  = ENVYN( NO2EX_FLAG, ENV_DESC, LNO2EX_CYCLE, STATUS )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the mechanism names for group species
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         LERROR = .FALSE.

c..Group 1 species
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for nitrogen dioxide (NO2)'        
         CALL ENVSTR( 'MECH_NO2', ENV_DESC, ENV_DFLT, MECH_NO2, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_NO2 not found'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for nitric oxide (NO)'        
         CALL ENVSTR( 'MECH_NO', ENV_DESC, ENV_DFLT, MECH_NO, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_NO not found'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF


c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for ozone (O3)'        
         CALL ENVSTR( 'MECH_O3', ENV_DESC, ENV_DFLT, MECH_O3, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_O3 not found'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for excited-state atomic oxygen (O1D)'
         CALL ENVSTR( 'MECH_O1D', ENV_DESC, ENV_DFLT, MECH_O1D, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_O1D not found'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for ground-state atomic oxygen (O3P)'
         CALL ENVSTR( 'MECH_O3P', ENV_DESC, ENV_DFLT, MECH_O3P, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_O3P not found'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

         IF( LNO2EX_CYCLE ) THEN
            ENV_DFLT = ' '
            ENV_DESC = 'Mechanism name for excited NO2 oxygen (NO2EX)'
            CALL ENVSTR( 'MECH_NO2EX', ENV_DESC, ENV_DFLT, MECH_NO2EX, STATUS)

            IF( STATUS .NE. 0 ) THEN
               MSG = 'ERROR: Environment variable MECH_NO2EX not found'
               CALL M3MESG( MSG )
               LERROR = .TRUE.
            END IF
         END IF


c..Group 2 species
c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for hydroxyl radical (OH)'
         CALL ENVSTR( 'MECH_OH', ENV_DESC, ENV_DFLT, MECH_OH, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_OH not found'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for hydroperoxy radical (HO2)'
         CALL ENVSTR( 'MECH_HO2', ENV_DESC, ENV_DFLT, MECH_HO2, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_HO2 not found'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for nitrous acid (HONO)'
         CALL ENVSTR( 'MECH_HONO', ENV_DESC, ENV_DFLT, MECH_HONO, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_HONO not found'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for pernitric acid (HNO4)'
         CALL ENVSTR( 'MECH_HNO4', ENV_DESC, ENV_DFLT, MECH_HNO4, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_HNO4 not found'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

c..Group 3 species
c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for peroxy acetyl nitrate (PAN)'
         CALL ENVSTR( 'MECH_PAN', ENV_DESC, ENV_DFLT, MECH_PAN, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_PAN not found'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for acetyl percoxy radical (C2O3)'
         CALL ENVSTR( 'MECH_C2O3', ENV_DESC, ENV_DFLT, MECH_C2O3, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_C2O3 not found'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

c..Group 4 species
c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for nitrate radical (NO3)'
         CALL ENVSTR( 'MECH_NO3', ENV_DESC, ENV_DFLT, MECH_NO3, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_NO3 not found'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

c         LERROR = .FALSE.
         ENV_DFLT = ' '
         ENV_DESC = 'Mechanism name for nitrogen pentoxide (N2O5)'
         CALL ENVSTR( 'MECH_N2O5', ENV_DESC, ENV_DFLT, MECH_N2O5, STATUS)

         IF( STATUS .NE. 0 ) THEN
            MSG = 'ERROR: Environment variable MECH_N2O5 not found'
            CALL M3MESG( MSG )
            LERROR = .TRUE.
         END IF

         IF( LERROR ) THEN
           MSG = 'Stopping because of error in defining group species'
           CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
         END IF


         RETURN

92000    FORMAT( /2X, 'The following mechanism versions will be created:' 
     &           / A / 1X )

         END SUBROUTINE GET_ENVS

      END MODULE ENV_VARS
