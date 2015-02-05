      MODULE PHOT_MET_DATA

        IMPLICIT NONE

           REAL, ALLOCATABLE, PUBLIC :: QV   ( :, :, : )  ! water vapor content (kg/kg)
           REAL, ALLOCATABLE, PUBLIC :: QC   ( :, :, : )  ! cloud water content (kg/kg)
           REAL, ALLOCATABLE, PUBLIC :: QG   ( :, :, : )  ! graupel content (kg/kg)
           REAL, ALLOCATABLE, PUBLIC :: QI   ( :, :, : )  ! ice content (kg/kg)
           REAL, ALLOCATABLE, PUBLIC :: QR   ( :, :, : )  ! rain water content (kg/kg)
           REAL, ALLOCATABLE, PUBLIC :: QS   ( :, :, : )  ! snow content (kg/kg)
           
           INTEGER,            PUBLIC :: HYDROMETERS       ! denotes type of hydrometers available
!                                                     ! zero  if cloud water and rain
!                                                     ! one   if cloud water, ice and rain
!                                                     ! two   if cloud water, rain and snow
!                                                     ! three if cloud water, ice, rain and snow
!                                                     ! four  if cloud water, rain, and graupel
!                                                     ! five  if cloud water, ice, rain and graupel
!                                                     ! six   if cloud water, rain, snow and graupel
!                                                     ! seven if cloud water, ice, rain, snow and graupel


           REAL, ALLOCATABLE, PUBLIC :: CFRAC   ( :, : )         ! total fractional cloud coverage
           REAL, ALLOCATABLE, PUBLIC :: CFRAC_3D( :, :, : )      ! three dimensional fractional cloud coverage
           REAL, ALLOCATABLE, PUBLIC :: CFRAC_2D( :, : )         ! averaged three dimensional fractional cloud coverage
           REAL, ALLOCATABLE, PUBLIC :: AVE_HYDROMETERS( :, : )  ! Cloud Hydrometer Content averaged over cloudy layers, g/m3

! properties of ACM unresolved cloud
           LOGICAL, PUBLIC           :: USE_ACM_CLOUD = .TRUE.
           
           REAL, ALLOCATABLE, PUBLIC :: ACM_CFRAC( :, :, : )  ! total fractional cloud coverage
           REAL, ALLOCATABLE, PUBLIC :: ACM_QC   ( :, :, : )  ! cloud water content (kg/kg)
           REAL, ALLOCATABLE, PUBLIC :: ACM_QG   ( :, :, : )  ! graupel content (kg/kg)
           REAL, ALLOCATABLE, PUBLIC :: ACM_QI   ( :, :, : )  ! ice content (kg/kg)
           REAL, ALLOCATABLE, PUBLIC :: ACM_QR   ( :, :, : )  ! rain water content (kg/kg)           
           REAL, ALLOCATABLE, PUBLIC :: ACM_CLOUDS(   :, : )  ! subgrid cloud fractions averaged over cloudy layer
           REAL, ALLOCATABLE, PUBLIC :: ACM_AVE_H2O(  :, : )  ! subgrid cloud water content averaged over cloudy layer, g/m3


           REAL, ALLOCATABLE, PUBLIC :: LAT          ( :,: ) ! north lat in deg (cross pt.)
           REAL, ALLOCATABLE, PUBLIC :: LON          ( :,: ) ! west long in deg (cross pt.)
           REAL, ALLOCATABLE, PUBLIC :: HT           ( :,: ) ! ground elevation msl (meters)
           REAL, ALLOCATABLE, PUBLIC :: COSINE_ZENITH( :,: ) ! cosine of solar zenith angle
           
           REAL, ALLOCATABLE, PUBLIC :: DENS   ( :, :, : ) ! air density [ Kg / m **3 ]
           REAL, ALLOCATABLE, PUBLIC :: TA     ( :, :, : ) ! Air temperature [ K ]
           REAL, ALLOCATABLE, PUBLIC :: PRES   ( :, :, : ) ! Air pressure [ Pa ]
           REAL, ALLOCATABLE, PUBLIC :: ZM     ( :, :, : ) ! layer half height agl [ m ]
           REAL, ALLOCATABLE, PUBLIC :: ZFULL  ( :, :, : ) ! layer full height agl [ m ]
           REAL, ALLOCATABLE, PUBLIC :: DELTA_Z( :, :, : ) ! layer thickness [ m ]
          
           REAL,               PUBLIC :: DIST_TO_SUN        ! solar distance [ au ]

           LOGICAL,            PUBLIC :: QV_AVAIL       = .TRUE.   ! flag for QV (vapor) available on file
           LOGICAL,            PUBLIC :: QC_AVAIL       = .TRUE.   ! flag for QC (liquid) available on file
           LOGICAL,            PUBLIC :: QR_AVAIL       = .TRUE.   ! flag for QR (rain) available on file
           LOGICAL,            PUBLIC :: QG_AVAIL       = .TRUE.   ! flag for QG (graupel) available on file
           LOGICAL,            PUBLIC :: QI_AVAIL       = .TRUE.   ! flag for QI (ice) available on file
           LOGICAL,            PUBLIC :: QS_AVAIL       = .TRUE.   ! flag for QS (snow) available on file
           LOGICAL,            PUBLIC :: CFRAC_3D_AVAIL = .FALSE.  ! flag for CFRAC_3D available on file
           LOGICAL,            PUBLIC :: REFF_QV_AVAIL  = .FALSE.  ! flag for liquid droplet effective radius (um) in file
           LOGICAL,            PUBLIC :: REFF_QR_AVAIL  = .FALSE.  ! flag for rain droplet effective radius (um) in file
           LOGICAL,            PUBLIC :: DEFF_QI_AVAIL  = .FALSE.  ! flag for ice particle effective diameter (um) in file
           LOGICAL,            PUBLIC :: DEFF_QS_AVAIL  = .FALSE.  ! flag for snowflake effective diameter (um) in file
           LOGICAL,            PUBLIC :: DEFF_QG_AVAIL  = .FALSE.  ! flag for graupel effective diameter (um) in file
           
           PUBLIC GET_PHOT_MET, UPDATE_SUN, CLEAR_ACM_CLOUD, CAPTURE_ACM_CLOUD
           
           PRIVATE
        
!...........Private Variables:

          LOGICAL       :: FIRSTIME = .TRUE.   ! flag for first pass thru

          CHARACTER( 16 )        ::  VARNM               ! variable name for IOAPI to get
          CHARACTER( 16 )        ::  VNAME_RN
          CHARACTER( 120 )       ::  XMSG        = ' '    ! Exit status message


          INTEGER         :: LOGDEV           ! output log unit number
          INTEGER         :: LDATE            ! last jdate MET data updated
          INTEGER         :: LTIME            ! last jtime MET data updated
          INTEGER         :: STDATE           ! jdate MET module initiated
          INTEGER         :: STTIME           ! jtime MET module initiated
! Saved variables:             
          INTEGER         :: ACM_DATE = -1     ! last date routine called
          INTEGER         :: ACM_TIME = -1     ! last time routine called
          
          INTEGER         :: GXOFF, GYOFF              ! global origin offset from file

C grid offsets for INTERPX to MET_CRO_3D, METCRO_CRO_2D files
          INTEGER              :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
          INTEGER              :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2
          
          REAL, ALLOCATABLE    :: SINLATS( :,: ) ! sine of latitude
          REAL, ALLOCATABLE    :: COSLATS( :,: ) ! cosine of latitude
          REAL                 :: STRTHR         ! starting GMT hour

          LOGICAL              :: SET_ACM_ARRAYS = .TRUE.

          
        CONTAINS 
       
        SUBROUTINE INIT_PHOT_MET( JDATE, JTIME )


        USE GRID_CONF           ! horizontal & vertical domain specifications
        USE UTILIO_DEFN

        IMPLICIT NONE


!...........Includes:

       INCLUDE SUBST_CONST                ! constants
       INCLUDE SUBST_FILES_ID             ! file name parameters
       
!...........Inputs:       

       INTEGER, INTENT( IN ) :: JDATE            ! current model date, coded YYYYDDD
       INTEGER, INTENT( IN ) :: JTIME            ! current model time, coded HHMMSS
       
!...........Local:
       INTEGER                :: COL                 ! column loop counter
       INTEGER                :: ROW                 ! row loop counter
       INTEGER                :: VAR                 ! variable name array index
       INTEGER                :: ALLOCSTAT           ! memory allocation status

       CHARACTER( 16 )        :: PNAME =  'INIT_PHOT_MET' 
       
       LOGICAL,  SAVE         :: INITIALIZED = .FALSE.
       
       IF( INITIALIZED )RETURN
       
       INITIALIZED = .TRUE.
       
       LOGDEV = INIT3()     

! open GRID_CRO_3D file and read time independent grid variables
        
        IF ( .NOT. OPEN3( GRID_CRO_2D, FSREAD3, PNAME ) ) THEN
          XMSG = 'Could not open '// GRID_CRO_2D // ' file'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                 STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )
     
       ALLOCATE ( HT( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
       IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating HT'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
       END IF

       ALLOCATE ( LAT( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
       IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating LAT'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
       END IF
       
       ALLOCATE ( LON( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
       IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating SINLON'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
       END IF

       VARNM = 'HT'
       IF ( .NOT. INTERPX( GRID_CRO_2D, VARNM, PNAME,
     &                     STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                     1,1, JDATE, JTIME, HT ) ) THEN
          XMSG = 'Could not read ' // TRIM(VARNM) // ' from ' // GRID_CRO_2D
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
       END IF

       VARNM = 'LON'
       IF ( .NOT. INTERPX( GRID_CRO_2D, VARNM, PNAME,
     &                     STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                     1,1, JDATE, JTIME, LON ) ) THEN
          XMSG = 'Could not read ' // TRIM(VARNM) // ' from ' // GRID_CRO_2D
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
       END IF

       VARNM = 'LAT'
       IF ( .NOT. INTERPX( GRID_CRO_2D, VARNM, PNAME,
     &                     STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                     1,1, JDATE, JTIME, LAT ) ) THEN
          XMSG = 'Could not read ' // TRIM(VARNM) // ' from ' // GRID_CRO_2D
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
       END IF

! set up variables for calculating cosine of solar zenith angle

       ALLOCATE ( SINLATS( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
       IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating SINLATS'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
       END IF

       ALLOCATE ( COSLATS( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
       IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating COSLATS'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
       END IF

       DO COL = 1, MY_NCOLS
           DO ROW = 1, MY_NROWS
              SINLATS( COL, ROW ) = SIN( PI180 * LAT ( COL, ROW ) )
              COSLATS( COL, ROW ) = COS( PI180 * LAT ( COL, ROW ) )
           END DO
       END DO

       ALLOCATE ( COSINE_ZENITH( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
       IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating COSINE_ZENITH'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
       END IF


! open MET_CRO_2D and set its offsets

        IF ( .NOT. OPEN3( MET_CRO_2D, FSREAD3, PNAME ) ) THEN
          XMSG = 'Could not open '// MET_CRO_2D // ' file'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF


        CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                  STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )


        ALLOCATE ( CFRAC( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating CFRAC'
           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

!...open MET_CRO_3D and set its offsets

        IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
          XMSG = 'Could not open '// MET_CRO_3D // ' file'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

!...get description from the met file

         IF ( .NOT. DESC3( MET_CRO_3D ) ) THEN
           XMSG = 'Could not get ' // MET_CRO_3D //' file description'
           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( DENS( MY_NCOLS,MY_NROWS, NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DENS'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( TA( MY_NCOLS,MY_NROWS, NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating TA'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( PRES( MY_NCOLS,MY_NROWS, NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating PRES'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( ZM( MY_NCOLS,MY_NROWS, NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating ZM'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         
         ALLOCATE ( ZFULL( MY_NCOLS,MY_NROWS, NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating ZFULL'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( DELTA_Z( MY_NCOLS,MY_NROWS, NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DELTA_Z'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
                

         ALLOCATE ( QV( MY_NCOLS,MY_NROWS, NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating QV'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( QC( MY_NCOLS,MY_NROWS, NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating QC'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( QR( MY_NCOLS,MY_NROWS, NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating QR'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         
         HYDROMETERS = 0

! test for which optional hydrometers are available

        VAR = INDEX1( 'QI', NVARS3D, VNAME3D )
        
        IF ( VAR .EQ. 0 ) THEN
          QI_AVAIL = .FALSE.
          XMSG = 'Parameter QI (cloud ice) was not found on file '
     &           // MET_CRO_3D
          CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
          WRITE( LOGDEV, '(3(/10X,A),(/10X,3(A,1X)),(/10X,A))' )
     &           'YOU SHOULD VERIFY that the cloud microphysics scheme used',
     &           'in the Meteorological Model did not include ice/snow.  If',
     &           'it did, then you need to reprocess the meteorological data',
     &           'through MCIP and pass QI to file ',
     &           TRIM( MET_CRO_3D ), ' to avoid',
     &           'errors in the wet deposition.'
          WRITE( LOGDEV, '((/5X,A),/)' )
     &           'Processing will continue with QI set to ZERO.  <<---<<'
        ELSE
          QI_AVAIL = .TRUE.
          HYDROMETERS = 1 + HYDROMETERS
        END IF

        ALLOCATE ( QI( MY_NCOLS,MY_NROWS, NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating QI'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        QI = 0.0

        VAR = INDEX1( 'QS', NVARS3D, VNAME3D )

        IF ( VAR .EQ. 0 ) THEN
          QS_AVAIL = .FALSE.
          XMSG = 'Parameter QS (snow) was not found on file ' // MET_CRO_3D
          CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
          WRITE( LOGDEV, '(3(/10X,A),(/10X,3(A,1X)),(/10X,A))' )
     &           'YOU SHOULD VERIFY that the cloud microphysics scheme used',
     &           'in the Meteorological Model did not include ice/snow.  If',
     &           'it did, then you need to reprocess the meteorological data',
     &           'through MCIP and pass QS to file ',
     &           TRIM( MET_CRO_3D ), ' to avoid',
     &           'errors in the wet deposition.'
          WRITE( LOGDEV, '((/5X,A),/)' )
     &           'Processing will continue with QS set to ZERO.  <<--<<'


        ELSE
           QS_AVAIL = .TRUE.
           HYDROMETERS = 2 + HYDROMETERS
        END IF

        ALLOCATE ( QS( MY_NCOLS,MY_NROWS, NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating QS'
           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        QS = 0.0

        VAR = INDEX1( 'QG', NVARS3D, VNAME3D )

        IF ( VAR .EQ. 0 ) THEN
          QG_AVAIL    = .FALSE.
         ELSE
           QG_AVAIL    = .TRUE.
           HYDROMETERS = 4 + HYDROMETERS
        END IF
 
        ALLOCATE ( QG( MY_NCOLS,MY_NROWS, NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating QG'
           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF        
        QG = 0.0
        
        IF( SET_ACM_ARRAYS .AND. USE_ACM_CLOUD ) THEN
            CALL INIT_ACM_CLOUD( 0, 0 )
            SET_ACM_ARRAYS = .FALSE.
        END IF

        
        VAR = INDEX1( 'CFRAC_3D', NVARS3D, VNAME3D )
        IF ( VAR .EQ. 0 ) THEN
          CFRAC_3D_AVAIL = .FALSE.
          XMSG = 'Parameter CFRAC_3D (cloud fraction) was not found on file ' // MET_CRO_3D
          CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
          WRITE( LOGDEV, '(3(/10X,A),(/10X,3(A,1X)),(/10X,A))' )
     &           'Model will diagnose layer cloud fraction using Randall (1995) and Hong (1998)',
     &           'based on mixing ratios of water vapor and its condensed states.',
     &           'You may want to reprocess the meteorological data',
     &           'to obtain layer cloud fractions if possible.'

        END IF
        
        ALLOCATE (CFRAC_3D( MY_NCOLS,MY_NROWS, NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating CFRAC_3D'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE (CFRAC_2D( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating CFRAC_2D'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE (AVE_HYDROMETERS( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating AVE_HYDROMETERS'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

! report out hydrometers found
       SELECT CASE( HYDROMETERS )
            CASE( 0 )
                 WRITE(LOGDEV,"(A)")TRIM(PNAME) // ": CLOUD has QC and QR"
            CASE( 1 )
                 WRITE(LOGDEV,"(A)")TRIM(PNAME) // ": Cloud has QC, QR and QI."
            CASE( 2 )
                 WRITE(LOGDEV,"(A)")TRIM(PNAME) // ": Cloud has QC, QR and QS."
            CASE( 3 )
                 WRITE(LOGDEV,"(A)")TRIM(PNAME) // ": Cloud has QC, QR, QI and QS."
            CASE( 4 )
                 WRITE(LOGDEV,"(A)")TRIM(PNAME) // ": Cloud has QC, QR, and QG."
            CASE( 5 )
                 WRITE(LOGDEV,"(A)")TRIM(PNAME) // ": Cloud has QC, QR, QG and QI."
            CASE( 6 )
                 WRITE(LOGDEV,"(A)")TRIM(PNAME) // ": Cloud has QC, QR, QG and QS."
            CASE( 7 )
                 WRITE(LOGDEV,"(A)")TRIM(PNAME) // ": Cloud has QC, QR, QG, QI and QS."
        END SELECT        
        
        LDATE  = 0
        LTIME  = 0

        STDATE = JDATE
        STTIME = JTIME
        STRTHR = FLOAT( JTIME / 10000 )

        FIRSTIME = .FALSE.
        
        END  SUBROUTINE INIT_PHOT_MET
!        
        SUBROUTINE GET_PHOT_MET(JDATE, JTIME, MDATE, MTIME )
        
        USE GRID_CONF           ! horizontal & vertical domain specifications
        USE UTILIO_DEFN         ! IO routines & utilties

        IMPLICIT NONE

!...........Includes:

        INCLUDE SUBST_CONST                ! constants
        INCLUDE SUBST_FILES_ID             ! file name parameters

        INTEGER, INTENT( IN ) :: JDATE            ! current model date, coded YYYYDDD
        INTEGER, INTENT( IN ) :: JTIME            ! current model time, coded HHMMSS
        INTEGER, INTENT( IN ) :: MDATE            ! date for calculation/interpolation(yyyyddd)
        INTEGER, INTENT( IN ) :: MTIME            ! time for calculation/interpolation (hhmmss)

!..........Local:

        CHARACTER( 16 ), SAVE :: PNAME =  'GET_PHOT_MET'
        
        INTEGER                :: COL                 ! column loop counter
        INTEGER                :: ROW                 ! row loop counter
        INTEGER                :: LAY                 ! layer loop counter
        
        INTEGER                :: ALLOCSTAT           ! allocation status measure
        
        REAL                   :: SUM_WEIGTHS
        REAL                   :: WEIGTH
        REAL                   :: WATER
       
       
        IF( FIRSTIME )THEN
            CALL INIT_PHOT_MET( JDATE, JTIME )
        END IF

!...store met file time, date, and step information and compute
!...  the met timestep in hours

        IF( LDATE .NE. JDATE .AND. LTIME .NE. JTIME )RETURN
        
        LDATE = JDATE
        LTIME = JTIME


!...  Interpolate time dependent layered input variables

!...Get resolved cloud fractions

       VARNM = 'CFRAC'
       IF ( .NOT. INTERPX( MET_CRO_2D, VARNM, PNAME,
     &                     STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2,
     &                     1,1, MDATE, MTIME, CFRAC ) ) THEN
          XMSG = 'Could not read ' // TRIM(VARNM) // ' from ' // GRID_CRO_2D
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
       END IF

!...Get resolved water vapor mixing ratio (kg H2O / kg air)

        VARNM = 'QV'
        IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                      STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                      MDATE, MTIME, QV ) ) THEN
          XMSG = 'Could not read QV from ' // MET_CRO_3D
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

!...Get resolved cloud water mixing ratio (kg H2O / kg air)

        VARNM = 'QC'
        IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                      STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                      MDATE, MTIME, QC ) ) THEN
          XMSG = 'Could not read QC from ' // MET_CRO_3D
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

!...Get resolved rain water mixing ratio (kg H2O / kg air)

        VARNM = 'QR'
        IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                      STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                      MDATE, MTIME, QR ) ) THEN
          XMSG = 'Could not read QR from ' // MET_CRO_3D
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

!...read resolved ice mixing ratio (kg H2O / kg air) from the met
!...  file if it is available

        IF ( QI_AVAIL ) THEN
          VARNM = 'QI'
          IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                        STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                        MDATE, MTIME, QI ) ) THEN
            XMSG = 'Could not read QI from ' // MET_CRO_3D
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          END IF
        END IF

!...read resolved snow mixing ratio (kg H2O / kg air) from the met
!...  file if it is available

        IF ( QS_AVAIL ) THEN
          VARNM = 'QS'
          IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                        STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                        MDATE, MTIME, QS ) ) THEN
            XMSG = 'Could not read QS from ' // MET_CRO_3D
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          END IF
        END IF

!...read graupel mixing ratio (kg H2O / kg air) from the met
!...  file if it is available

        IF ( QG_AVAIL ) THEN
          VARNM = 'QG'
          IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                        STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                        MDATE, MTIME, QG ) ) THEN
            XMSG = 'Could not read QG from ' // MET_CRO_3D
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          END IF
        END IF

        
        VARNM = 'ZH' ! midlayer height
        IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                      STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                      1,NLAYS, MDATE, MTIME, ZM ) ) THEN
           XMSG = 'Could not read ' // TRIM( VARNM ) // ' from ' // MET_CRO_3D
           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        VARNM = 'ZF' ! full layer height
        IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                      STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                      1,NLAYS, MDATE, MTIME, ZFULL ) ) THEN
           XMSG = 'Could not read ' // TRIM( VARNM ) // ' from ' // MET_CRO_3D
           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

!...Get air density (kg/m3)

        VARNM = 'DENS'
        IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                      STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                      MDATE, MTIME, DENS ) ) THEN
          XMSG = 'Could not read ' // TRIM( VARNM ) // ' from ' // MET_CRO_3D
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

!...get temperature [K]

        VARNM = 'TA'
        IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                      STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3,
     &                      1,NLAYS, MDATE, MTIME, TA ) ) THEN
           XMSG = 'Could not interpolate '// TRIM( VARNM ) // ' from MET_CRO_3D '
           CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
        END IF

!...pressure [Pa]

        VARNM = 'PRES'
        IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                      STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3,
     &                      1, NLAYS, MDATE, MTIME, PRES ) ) THEN
           XMSG = 'Could not read ' // TRIM( VARNM ) // ' from ' // MET_CRO_3D
           CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
        END IF


! floor values of QC, QR, QI, QS, and QG to zero

         DO LAY = 1, NLAYS
          DO ROW = 1, MY_NROWS
             DO COL = 1, MY_NCOLS
               QC( COL, ROW, LAY ) = MAX( QC( COL, ROW, LAY ), 0.0 )
               QR( COL, ROW, LAY ) = MAX( QR( COL, ROW, LAY ), 0.0 )
               IF ( QI_AVAIL )QI( COL, ROW, LAY ) = MAX( QI( COL, ROW, LAY ), 0.0 )
               IF ( QS_AVAIL )QS( COL, ROW, LAY ) = MAX( QS( COL, ROW, LAY ), 0.0 )
               IF ( QG_AVAIL )QG( COL, ROW, LAY ) = MAX( QG( COL, ROW, LAY ), 0.0 )
            END DO
          END DO
        END DO

        IF ( CFRAC_3D_AVAIL ) THEN  ! read from file
          VARNM = 'CFRAC_3D'
          IF ( .NOT. INTERPX( MET_CRO_3D, VARNM, PNAME,
     &                        STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                        MDATE, MTIME, CFRAC_3D ) ) THEN
            XMSG = 'Could not read CFRAC_3D from ' // MET_CRO_3D
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          END IF
        ELSE  ! diagnose from available data
            CALL CAL_CFRAC_3D()
        END IF


        DO ROW = 1, MY_NROWS 
           DO COL = 1, MY_NCOLS
              DELTA_Z( COL, ROW, 1 ) = ZFULL( COL, ROW, 1 )
              FORALL ( LAY = 2:NLAYS )
                 DELTA_Z( COL, ROW, LAY ) = (ZFULL( COL, ROW, LAY ) - ZFULL( COL, ROW, LAY-1 ))
              END FORALL
           END DO
        END DO

! compute averages for resolved cloud                
!       CFRAC_2D = 0.0
        DO ROW = 1, MY_NROWS
           DO COL = 1, MY_NCOLS
                SUM_WEIGTHS = 0.0
                CFRAC_2D( COL, ROW ) = 0.0
                AVE_HYDROMETERS( COL, ROW ) = 0.0
                DO LAY = 1, NLAYS
                   IF( CFRAC_3D( COL, ROW, LAY ) .LE. 0.0 )CYCLE
                   WEIGTH = DELTA_Z( COL, ROW, LAY ) 
                   CFRAC_2D( COL, ROW ) = CFRAC_2D( COL, ROW )
     &                                  + WEIGTH * CFRAC_3D( COL, ROW, LAY ) 
                   SUM_WEIGTHS          = SUM_WEIGTHS + WEIGTH

                   WATER = QC( COL, ROW, LAY ) + QR( COL, ROW, LAY ) + QG( COL, ROW, LAY )
     &                   + QI( COL, ROW, LAY ) + QS( COL, ROW, LAY )

                   AVE_HYDROMETERS(COL, ROW ) = AVE_HYDROMETERS(COL, ROW )
     &                                        + DENS( COL, ROW, LAY ) * WATER * WEIGTH
#ifdef phot_debug     
                    IF( CFRAC_2D( COL, ROW ) .NE. CFRAC_2D( COL, ROW ) .OR.
     &                                                  SUM_WEIGTHS .NE. SUM_WEIGTHS ) THEN
                        write(logdev,"(A,40ES12.4)")"WEIGTH, CFRAC_3D = ",WEIGTH, CFRAC_3D( COL, ROW, LAY ) 
                        XMSG = 'Floating point exception for CFRAC_2D'
                        CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT1 )
                    END IF
#endif              
                END DO
                IF( SUM_WEIGTHS .LT. 1.0 )CYCLE
                CFRAC_2D( COL, ROW )        = CFRAC_2D( COL, ROW ) / SUM_WEIGTHS
                AVE_HYDROMETERS( COL, ROW ) = 1.0E+3 * AVE_HYDROMETERS( COL, ROW ) / SUM_WEIGTHS                    
          END DO
        END DO
       
        IF( USE_ACM_CLOUD )THEN ! compute averages for subgrid clouds                
           DO ROW = 1, MY_NROWS
              DO COL = 1, MY_NCOLS
                   SUM_WEIGTHS  = 0.0
!                   ACM_CLOUDS( COL, ROW )  = 0.0
                   ACM_AVE_H2O( COL, ROW ) = 0.0
                   DO LAY = 1, NLAYS
                      IF( ACM_CFRAC( LAY, COL, ROW ) .LE. 0.0 )CYCLE

                      WEIGTH      = DELTA_Z( COL, ROW, LAY ) 
                      SUM_WEIGTHS = SUM_WEIGTHS + WEIGTH
! acm clouds do not contain snow
                      WATER = ACM_QC( LAY, COL, ROW ) + ACM_QI( LAY, COL, ROW ) 
     &                      + ACM_QG( LAY, COL, ROW ) + ACM_QR( LAY, COL, ROW )  

!assume acm cloud is uniform or constant if it does not equal zero so set in  
!subroutine CAPTURE_ACM_CLOUD
!                      ACM_CLOUDS( COL, ROW ) = ACM_CLOUDS( COL, ROW )
!     &                                       + WEIGTH * ACM_CFRAC( LAY, COL, ROW ) 

                      ACM_AVE_H2O( COL, ROW ) = ACM_AVE_H2O( COL, ROW )
     &                                        + DENS( COL, ROW, LAY ) * WEIGTH * WATER
                   END DO
                   IF( SUM_WEIGTHS .LT. 1.0 )CYCLE
!                   ACM_CLOUDS( COL, ROW )  = ACM_CLOUDS( COL, ROW ) / SUM_WEIGTHS
                   ACM_AVE_H2O( COL, ROW ) = 1.0E+3 * ACM_AVE_H2O( COL, ROW ) / SUM_WEIGTHS
              END DO
           END DO
        END IF
            

        END SUBROUTINE GET_PHOT_MET
        
        SUBROUTINE UPDATE_SUN( JDATE, JTIME, MDATE, MTIME )

          USE GRID_CONF           ! horizontal & vertical domain specifications
          USE UTILIO_DEFN         ! IO routines & utilties
!          USE PHOT_MOD            ! contains solar distance and zenith routines
  
          IMPLICIT NONE     
          
!***include files

          INCLUDE SUBST_CONST      ! physical constants

           
! Inputs:  
          INTEGER, INTENT( IN ) :: JDATE            ! current model date, coded YYYYDDD
          INTEGER, INTENT( IN ) :: JTIME            ! current model time, coded HHMMSS
          INTEGER, INTENT( IN ) :: MDATE            ! date for calculation/interpolation(yyyyddd)
          INTEGER, INTENT( IN ) :: MTIME            ! time for calculation/interpolation (hhmmss)

! Local:
          INTEGER, SAVE          :: PDATE = 0           ! previous date called (yyyyddd)
          INTEGER, SAVE          :: PTIME = 0           ! previous time called (hhmmss)
          INTEGER                :: COL                 ! column loop counter
          INTEGER                :: ROW                 ! row loop counter

          REAL                   :: SINDEC              ! sine of the solar declination
          REAL                   :: COSDEC              ! cosine of the solar declination
          REAL                   :: JYEAR               ! year
          REAL                   :: JDAY                ! current Julian day (DDD)
          REAL                   :: EQUATION_OF_TIME    ! equation of time
          REAL                   :: CURRHR              ! current GMT hour
          REAL                   :: CURRHR_LST          ! local standard time at each grid cell
        
          REAL, PARAMETER       :: ONE_OVER_15    = 1.0 / 15.0
          REAL, PARAMETER       :: ONE_OVER_3600  = 1.0 / 3600     

          IF( FIRSTIME )CALL INIT_PHOT_MET( JDATE, JTIME )

          IF( PDATE .NE. JDATE .AND. PTIME .NE. JTIME )RETURN
        
          PDATE = JDATE
          PTIME = JTIME
   
! Compute distance to sun and set solar declination variables

          JDAY   = FLOAT( MOD( JDATE, 1000 ) )
          JYEAR  = FLOAT( JDATE / 1000  )        

          CURRHR = STRTHR
     &          + FLOAT( SECSDIFF( STDATE, STTIME, MDATE, MTIME ) )
     &          * ONE_OVER_3600

          CALL SOLEFM3 ( JYEAR, JDAY, EQUATION_OF_TIME, SINDEC, COSDEC, DIST_TO_SUN )
        
          DO ROW = 1, MY_NROWS
             DO COL = 1, MY_NCOLS

!...correct  CURRHR for current *positive* West longitude convention
!...to obtain LST.
!...this convention on longititude should be reexamined for different domains

                CURRHR_LST = CURRHR + LON( COL, ROW ) * ONE_OVER_15

                CALL GETZEN2 ( CURRHR_LST , SINLATS( COL, ROW ), COSLATS( COL, ROW ),
     &                         SINDEC, COSDEC, EQUATION_OF_TIME, 
     &                         COSINE_ZENITH( COL, ROW ) )

#ifdef verbose_testing
            if( row .eq. 1 .and. col .eq. 1 )then
                write(logdev,"(A,40ES12.4)")"CLD:  JYEAR, JDSTRT, SOLDIST = ",
     &          JYEAR, JDAY, DIST_TO_SUN
                write(logdev,"(A,40ES12.4)")"CLD:  CURRHR_LST , SINLAT, COSLAT, " //
     &                     "SINDEC, COSDEC, EQT, COSZEN = ", CURRHR_LST , SINLATS( COL, ROW ), COSLATS( COL, ROW ),
     &                     SINDEC, COSDEC, EQUATION_OF_TIME, COSINE_ZENITH( COL, ROW )
            end if
#endif            

            END DO
          END DO
               
        END SUBROUTINE UPDATE_SUN
        SUBROUTINE SOLEFM3 ( YEAR, DAY, EQT, SINDEC, COSDEC, SOLDIST )
!-----------------------------------------------------------------------
!  FSB Coded September 9, 2004 by Dr. Francis S. Binkowski
!     Environmental Modeling for Policy Development group,
!     The Carolina Environmental Program
!     The University of North Carolina-Chapel Hill
!     Email: frank_binkowski@unc.edu
!
!  REFERENCE:The code is a modification of that  from
!     J.J. Michalsky, The Astronomical Almanac's algorithm for
C     approximate solar position (1950-2050)
C     Solar Energy vol. 40, No. 3, pp 227-235, 1988.
C     who based it upon
C
C     The Astronomical Almanac, U.S. Gov't. Printing Office,
C     Washington DC. Page c24 has the algorithm
C
C  The stated accuracy is 0.01 degree for the epoch ( 1950 - 2050).
C
C  This subroutine calculates the sine and cosine of the solar
C     declination and the equation of time, and solar distance
C     using an approximation to equations used to generate the
C     tables in  The Astronomical Almanac.
C
C  input:
C     year = year (e.g. 1998)
C     day  = day of the year (e.g. Feb. 2 is 33)
C     lat  = latitude in degrees (north is positive)
C     long = longitude in degrees (east is positive)
C
C  output:
C     EQT    = Equation of Time
C     sindec = sine of the solar declination
C     cosdec = cosine of the solar declination
C     soldist = distance of sun in astronomical units
C-----------------------------------------------------------------------

      IMPLICIT NONE

!***arugments

      REAL, INTENT(IN)  :: YEAR, DAY
      REAL, INTENT(OUT) :: EQT, SINDEC, COSDEC, SOLDIST

!***include files

      INCLUDE SUBST_CONST      ! physical constants


!***parameters

!!!from above      REAL, PARAMETER :: PI = 3.1415927
      REAL, PARAMETER :: TWOPI = 2.0 * PI
!!!from above      REAL, PARAMETER :: RAD   = PI / 180.0
      REAL, PARAMETER :: RAD   = PI180
      REAL, PARAMETER :: RADM1 = 1.0 / RAD

!***FSB inline documentation defines the variables.

      REAL DELTA, LEAP, JD, TIME, L, G, LAMDA, EPSILON
      REAL ALPHA
      REAL SING, SIN2G, COSG, COS2G
      REAL(8) NUM, DEN

!***start code
!***get current julian date, jd
!***  the offset year of 1949 is for convenience of having all
!***  positive numbers for the epoch (1950 - 2050)

      DELTA = YEAR - 1949.0
      LEAP  = AINT( DELTA / 4.0 )

!***jd = 32916.5 + 365.0 * delta + leap + hour / 24.0
!***set hour = 12.0 and 12.0 / 24.0 -> 0.5

      JD = 2432916.5 + 365.0 * DELTA + LEAP + DAY + 0.5

!***first number is jd for midnight 0 Jan 1949
!***  leap is leap days since 1949.

!***  calculate ecliptic conditions:

      TIME = JD - 2451545.0     ! 51545.0 is noon Jan 1, 2000

!***calculate mean solar longitude (L)

      L = 280.460 + 0.985674 * TIME ! mean solar longitude

!***now force L to be between 0.0 and 360. degrees

      L = MODULO( L, 360.0 )
      IF ( L .lt. 0.0 ) L = L + 360.0

!***calculate mean anomaly (g) in radians between 0 and twopi

      G = 357.528 + 0.9856003 * TIME
      G = MODULO( G, 360.0 )
      IF ( G .LT. 0.0 ) G = G + 360.0

      G = RAD * G    ! in radians now

!***calculate trig functions of g using identities
!***  this speeds up the calculations

      SING = SIN( G )
      COSG = COS( G )
      SIN2G = 2.0 * SING * COSG
      COS2G = COSG * COSG - SING * SING

!***calculate ecliptic longitude ( Lamda) and
!***  obliquity (epsilon) of ecliptic in radians

      LAMDA = L +  1.915 * SING  + 0.020 * SIN2G
      LAMDA = MODULO( LAMDA, 360.0 )
      IF ( LAMDA .LT. 0.0 ) LAMDA = LAMDA + 360.0

!***calculate obliquity (epsilon)

      EPSILON = 23.429 - 4.0E-7 * TIME
      LAMDA   = RAD * LAMDA    ! in radians
      EPSILON = RAD * EPSILON  ! in radians

!***now calculate right ascension (Alpha) Use this method to get the
!***  correct quadrant.

      NUM = COS( EPSILON ) * SIN( LAMDA ) ! numerator
      DEN = COS( LAMDA )          ! denominator
      ALPHA = ATAN( NUM / DEN )

!***force right ascension to be between 0 and twopi

      IF ( DEN .LT. 0.0D0 ) THEN
         ALPHA = ALPHA + PI
      ELSE IF ( NUM .LT. 0.0D0 ) THEN
         ALPHA = ALPHA + TWOPI
      END IF

!***Alpha = atan2(num,den)
!***  Alpha = MOD(Alpha, 360.0)
!***  IF ( Alpha .lt. 0.0)  Alpha = Alpha + 360.0

      ALPHA = RADM1 * ALPHA     ! Alpha now in degrees

!***calculate the sine of the declination
!***  (sindec) cosine of the declination (cosdec)

      SINDEC = SIN( EPSILON ) * SIN( LAMDA )
      COSDEC = SQRT( 1.0 - SINDEC * SINDEC )

!***calculate the Equation of Time( EQT) in degrees.

      EQT = L - ALPHA
      IF ( EQT .GT. 350.0 ) EQT = EQT - 360.0 ! catch a problem

!***calculate solar distance

      SOLDIST = 1.00014 - 0.01671 * COSG - 0.00014 * COS2G

      RETURN
      END SUBROUTINE SOLEFM3

C///////////////////////////////////////////////////////////////////////

      SUBROUTINE GETZEN2 ( TIME, SINLAT, COSLAT, SINDEC, COSDEC, EQT,
     &                     COSZEN )
C-----------------------------------------------------------------------
C  calculate the zenith angle information at the present time step
C
C  input:
C     time    = LST in decimal hours <<NOTE: LST
C     sinlat  = sine of the latitude
C     coslat  = cosine of the latitude
C     sindec  = sine of the solar declination
C     cosdec  = cosine of the solar declination
C     EQT     = equation of time in degrees
C
C  Output:
C     coszen  = cosine of the zenith angle
C
C  Revision history:
C     FSB Coded September 10, 2004 by Dr. Francis S. Binkowski
C     FSB modified February 15, 2005 to increase efficiency
C     Environmental Modeling for Policy Development group,
C     The Carolina Environmental Program
C     The University of North Carolina-Chapel Hill
C     Email: frank_binkowski@unc.edu
C
C  Note: because this routine will be called frequently
C     during a simulation, the sines and cosines of the
C     latitude should be calculated once at the beginning of
C     simulation.
C-----------------------------------------------------------------------

      IMPLICIT NONE

!***include files

      INCLUDE SUBST_CONST      ! physical constants


!***arguments

      REAL, INTENT(IN)  :: TIME, SINLAT, COSLAT
      REAL, INTENT(IN)  :: SINDEC, COSDEC, EQT
      REAL, INTENT(OUT) :: COSZEN

!***internal variables

      REAL HANGLE          ! local hour angle in degrees

!!!from above      REAL, PARAMETER :: PI = 3.1415927
!!!from above      REAL, PARAMETER :: RAD = PI / 180.0
      REAL, PARAMETER :: RAD = PI180

!***start code.

      HANGLE = 15.0 * ( TIME - 12.0 )  + EQT
      COSZEN = SINDEC * SINLAT + COSLAT * COSDEC * COS( RAD * HANGLE )

!***do not allow zenith angle to be lower than 1 degree (COSZEN = 0.999998)
!***when COSZEN is 1.0, gas-phase chemistry solver does not work properly 
      IF (COSZEN .GT. 0.999998) COSZEN = 0.999998

      RETURN
      END SUBROUTINE GETZEN2        
        SUBROUTINE cal_cfrac_3d( F_ICE_PHY, F_RAIN_PHY )
! DESCRIPTION:
! Compute cloud fraction from input ice and cloud water fields
! if provided.
!
! Whether QI or QC is active or not is determined from the indices of
! the fields into the 4D scalar arrays in WRF. These indices are 
! P_QI and P_QC, respectively, and they are passed in to the routine
! to enable testing to see if QI and QC represent active fields in
! the moisture 4D scalar array carried by WRF.
! 
! If a field is active its index will have a value greater than or
! equal to PARAM_FIRST_SCALAR, which is also an input argument to 
! this routine.
!
! References:
! 1) Randall, D. A., 1995: Parameterizing fractional cloudiness produced
!    by cumulus entrainment. Preprints, Workshop on Cloud Microphysics
!    Parameterizations in Global Atmospheric Circulation
!    Models, Kananaskis, AB, Canada, WMO, 1–16.
! 2) Hong et al. (1998). Implementation of Prognostic Cloud Scheme for a
!    Regional Spectral Model. Monhtly Weather Review, vol. 126, pp 2621-
!    2639.
! Routine copied from module_radiation_driver.F in WRF 3.5 and modified for
! CMAQ applications

           USE GRID_CONF           ! horizontal & vertical domain specifications
           USE UTILIO_DEFN         ! IO routines & utilties

!---------------------------------------------------------------------
           IMPLICIT NONE
!---------------------------------------------------------------------

! agruments:
           REAL, DIMENSION( :, :, : ), OPTIONAL, INTENT( IN ) :: F_ICE_PHY
           REAL, DIMENSION( :, :, : ), OPTIONAL, INTENT( IN ) :: F_RAIN_PHY
!  Local:
          INTEGER :: i, j, k   ! loop counters
          
          REAL    :: RHUM       ! relative humidty   (fraction)
          REAL    :: tc         ! temp               (degrees Celuis)
          REAL    :: esw        ! water vapor liquid saturaturion vapor pressure (Pa)
          REAL    :: esi        ! water vapor ice saturaturion vapor pressure (Pa)
          REAL    :: weight     ! weigthing factor between liquid and ice content
          REAL    :: qvsw       ! water vapor saturation mixing ratio (Kg/Kg)
          REAL    :: qvsi       ! water ice saturation mixing ratio (Kg/Kg) 
          REAL    :: qvs_weight ! weigthed average of liquid and ice mixing ratios
          REAL    :: QIMID      ! layer ice mixing ratio
          REAL    :: QWMID      ! layer liquid water mixing ratio  
          REAL    :: QCLD       ! cloud water mixing ratio
          REAL    :: DENOM      ! scratch varaible
          REAL    :: ARG        ! scratch varaible
          REAL    :: SUBSAT     ! scratch varaible
! Parameters:
! Used for saturation mixing ratio weighted based on fractions of water and ice. 
! Following:
!   Murray, F.W. 1966. ``On the computation of Saturation Vapor Pressure,'
!   J. Appl. Meteor.,  6, p. 204.
! For saturation vapor pressure for each water phase
! es (in mb) = 6.1078exp[ a(T-273.16)/ (T-b) ]
!       over ice        over water
! a =   21.8745584      17.2693882
! b =   7.66            35.86
          REAL, PARAMETER ::  SVP1  = 0.61078
          REAL, PARAMETER ::  SVP2  = 17.2693882
          REAL, PARAMETER ::  SVPI2 = 21.8745584
          REAL, PARAMETER ::  SVP3  = 35.86
          REAL, PARAMETER ::  SVPI3 = 7.66
          REAL, PARAMETER ::  SVPT0 = 273.15
!Note: ep_2=287.0/461.6 Rd/Rv
! Note: R_D=287.0
          REAL, PARAMETER ::  R_D     = 287.0
          REAL, PARAMETER ::  R_V     = 461.6
          REAL, PARAMETER ::  EP_2    = R_D/R_V
!-----------------------------------------------------------------------
!---  Parameters for GRID-SCALE CLOUD COVER FOR RADIATION
!     (modified by Ferrier, Feb '02)
!
!---  Cloud fraction parameterization follows Randall, 1994
!     (see Hong et al., 1998)
!-----------------------------------------------------------------------
          REAL, PARAMETER  :: ALPHA0  = 100.0
          REAL, PARAMETER  :: GAMMA   = 0.49
          REAL, PARAMETER  :: QCLDMIN = 1.E-12
          REAL, PARAMETER  :: PEXP    = 0.25
          REAL, PARAMETER  :: RHGRID  = 1.0

! Alternative calculation for critical RH for grid saturation
!     RHGRID=0.90+.08*((100.-DX)/95.)**.5

   
!---------------------------------------------------------------------

         CFRAC_3D = 0.0
         
         DO J = 1, NLAYS
           DO K = 1, MY_NROWS
             DO I = 1, MY_NCOLS
          
               TC   = TA(I,K,J) - SVPT0
               ESW  = 1000.0 * SVP1 * EXP( SVP2  * TC / ( TA(I,K,J) - SVP3  ) )
               ESI  = 1000.0 * SVP1 * EXP( SVPI2 * TC / ( TA(I,K,J) - SVPI3 ) )
               QVSW = EP_2 * ESW / ( PRES(I,K,J) - ESW )
               QVSI = EP_2 * ESI / ( PRES(I,K,J) - ESI )
               QCLD = 0.0
                
               IF( QI_AVAIL .AND. QC_AVAIL .AND. QS_AVAIL .AND. QG_AVAIL ) THEN
                    QCLD = QI(I,K,J)+QC(I,K,J)+QS(I,K,J)+QG(I,K,J)
                    IF (QCLD .LT. QCLDMIN) THEN
                       WEIGHT = 0.0
                    ELSE
                       WEIGHT = (QI(I,K,J)+QS(I,K,J)+QG(I,K,J)) / QCLD
                    END IF
               ELSE IF ( QI_AVAIL .AND. QC_AVAIL .AND. QS_AVAIL .AND. .NOT. QG_AVAIL ) THEN
                    QCLD = QI(I,K,J)+QC(I,K,J)+QS(I,K,J)
                    IF (QCLD .LT. QCLDMIN) THEN
                       WEIGHT = 0.0
                    ELSE
                       WEIGHT = (QI(I,K,J)+QS(I,K,J)) / QCLD
                    END IF
               ELSE IF ( QC_AVAIL .AND. .NOT. QI_AVAIL .AND. .NOT. QS_AVAIL ) THEN
                    QCLD = QC(I,K,J)
                    IF (QCLD .LT. QCLDMIN) THEN
                       WEIGHT = 0.0
                    ELSE
                      IF (TA(I,K,J) .GT. 273.15) WEIGHT = 0.0
                      IF (TA(I,K,J) .LE. 273.15) WEIGHT = 1.0
                    END IF
               ELSE IF ( QC_AVAIL .AND. .NOT. QI_AVAIL .AND. QS_AVAIL .AND. PRESENT(F_ICE_PHY) ) THEN
! Mixing ratios of cloud water & total ice (cloud ice + snow).
! Mixing ratios of rain are not considered in this scheme.
! F_ICE is fraction of ice
! F_RAIN is fraction of rain
                    QIMID = QS(I,K,J)
                    QWMID = QC(I,K,J)
!--- Total "cloud" mixing ratio, QCLD.  Rain is not part of cloud,
!    only cloud water + cloud ice + snow
                    QCLD=QWMID+QIMID
                    IF (QCLD .LT. QCLDMIN) THEN
                       WEIGHT = 0.0
                    ELSE
                       WEIGHT = F_ICE_PHY(I,K,J)
                    END IF
               ELSE IF( QC_AVAIL .AND. .NOT. QI_AVAIL .AND. .NOT. QS_AVAIL )THEN
                    QIMID = 0.0
                    QWMID = QC(I,K,J)
                    QCLD=QWMID+QIMID
                    IF(QCLD .LT. QCLDMIN) THEN
                      WEIGHT = 0.0
                    ELSE
                      WEIGHT = F_ICE_PHY(I,K,J)
                    END IF
                ELSE          
                    CFRAC_3D(I,K,J)=0.0
                    CYCLE
                END IF !  IF ( QI_AVAIL .AND. QC_AVAIL .AND. QS_AVAIL)

                QVS_WEIGHT = ( 1.0 - WEIGHT ) * QVSW + WEIGHT * QVSI
                RHUM       = QV(I,K,J) / QVS_WEIGHT   !--- RELATIVE HUMIDITY
!--- Determine cloud fraction (modified from original algorithm)
                IF ( QCLD .LT. QCLDMIN ) THEN
!--- Assume zero cloud fraction if there is no cloud mixing ratio
                   CFRAC_3D(I,K,J)=0.0
                   CYCLE
                ELSE IF( RHUM .GE. RHGRID )THEN
!--- Assume cloud fraction of unity if near saturation and the cloud
!    mixing ratio is at or above the minimum threshold
                    CFRAC_3D(I,K,J)=1.0
                   CYCLE
                ELSE
!--- Adaptation of original algorithm (Randall, 1994; Zhao, 1995)
!    modified based on assumed grid-scale saturation at RH=RHgrid.
                    SUBSAT = MAX( 1.E-10, RHGRID*QVS_WEIGHT-QV(i,k,j))
                    DENOM  = (SUBSAT)**GAMMA
                    ARG    = MAX(-6.9, -ALPHA0*QCLD/DENOM)    ! <-- EXP(-6.9)=.001
! prevent negative values  (new)
!                   RHUM   = MAX( 1.E-10, MIN( RHUM, RHGRID ) ) 
!                   RHUM   = MAX( 1.E-10, RHUM ) 
                    CFRAC_3D(I,K,J) = (RHUM/RHGRID)**PEXP*(1.-EXP(ARG))
!!!!!!!!!!!!!       ARG=-1000*QCLD/(RHUM-RHGRID)
!!!!!!!!!!!!!       ARG=MAX(ARG, ARGMIN)
!!!!!!!!!!!!!       CFRAC_3D(i,k,j)=(RHUM/RHGRID)*(1.-EXP(ARG))
                 END IF          !--- End IF (QCLD .LT. QCLDMIN) ...

                 IF (CFRAC_3D(I,K,J) .LT. 0.01) CFRAC_3D(I,K,J) = 0.0
              
                 IF( CFRAC_3D(I,K,J) .NE. CFRAC_3D(I,K,J) .OR. CFRAC_3D(I,K,J) .GT. 1.0 )THEN 
                      WRITE(LOGDEV,'(A,40ES12.4)')' TA, PRES = ',TA(I,K,J), PRES(I,K,J)
                      WRITE(LOGDEV,'(A,40ES12.4)')'QV, QC, QI, QS = ', QV(I,K,J), QC(I,K,J), QI(I,K,J), QS(I,K,J)
                      WRITE(LOGDEV,'(A,40ES12.4)')'ESW, ESI, QVSW, QVSI = ', ESW, ESI, QVSW, QVSI
                      WRITE(LOGDEV,'(A,40ES12.4)')'WEIGH, QVS_WEIGH = ', WEIGHT, QVS_WEIGHT 
                      WRITE(LOGDEV,'(A,40ES12.4)')'SUBSAT, RHUM, RHGRID, ARG = ', SUBSAT, RHUM, RHGRID, ARG
                      XMSG = 'Floating point exception for CFRAC_3D'
                      CALL M3EXIT ( 'cal_cfrac_3d', 0, 0, XMSG, XSTAT1 )
                 END IF

                  
                ENDDO          !--- End DO i
              ENDDO          !--- End DO k
            ENDDO          !--- End DO j

          END SUBROUTINE cal_cfrac_3d

          SUBROUTINE CAPTURE_ACM_CLOUD( JDATE, JTIME, ROW, COLUMN, BOTTOM, TOP, FRACTION, WATER, ICE )

              USE GRID_CONF           ! horizontal & vertical domain specifications
              USE CLOUD_OPTICS,  ONLY : CLDMIN
         
              IMPLICIT NONE
! Purpose:
!          -Save properties determined by ACM subgrid cloud at vertical column into
!           for radiation transfer calculations
! Arguments:
             INTEGER, INTENT( IN )  :: JDATE      
             INTEGER, INTENT( IN )  :: JTIME                 
             INTEGER, INTENT( IN )  :: ROW           ! array row index
             INTEGER, INTENT( IN )  :: COLUMN        ! array column index
             INTEGER, INTENT( IN )  :: TOP           ! vertical layer index
             INTEGER, INTENT( IN )  :: BOTTOM        ! vertical layer index
             REAL,    INTENT( IN )  :: FRACTION      ! cloud fraction of ACM cloud
             REAL,    INTENT( IN )  :: WATER( : )    ! liquid water droplet mixing ratio
             REAL,    INTENT( IN )  :: ICE( : )      ! ice partical mixing ratio
! Parameters:
!             None
! Local variablese:             
             INTEGER                :: LAYER         ! loop index
             INTEGER                :: ALLOCSTAT     ! memory allocation status
             
            

             CALL CLEAR_ACM_CLOUD( JDATE, JTIME )
  
             DO LAYER = BOTTOM, TOP
                IF( WATER( LAYER ) + ICE( LAYER ) .LT. CLDMIN )CYCLE
                ACM_CFRAC( LAYER,COLUMN,ROW ) = FRACTION
                ACM_QC( LAYER,COLUMN,ROW ) = WATER( LAYER )
                ACM_QI( LAYER,COLUMN,ROW ) =   ICE( LAYER )
             END DO
                     
             DO LAYER = 1, (BOTTOM-1)
                IF( WATER( LAYER ) + ICE( LAYER ) .LT. CLDMIN )CYCLE
                ACM_CFRAC( LAYER,COLUMN,ROW ) = FRACTION
!                ACM_QR( LAYER,COLUMN,ROW ) =  ( WATER( LAYER ) + ICE( LAYER ) )
                ACM_QR( LAYER,COLUMN,ROW ) =   WATER( LAYER )
                ACM_QG( LAYER,COLUMN,ROW ) =   ICE( LAYER )
             END DO

              ACM_CLOUDS( COLUMN,ROW )  = FRACTION             
             
!             WRITE(LOGDEV,'(A,2(I3,1X),10(ES12.4,1X))')'COLUMN,ROW,ACM_CFRAC = ',COLUMN,ROW,
!     &       ACM_CFRAC( TOP,COLUMN,ROW ), ACM_QC( TOP,COLUMN,ROW ),ACM_QI( TOP,COLUMN,ROW )            
             
             RETURN
          END SUBROUTINE CAPTURE_ACM_CLOUD
          SUBROUTINE INIT_ACM_CLOUD( JDATE, JTIME)

             USE GRID_CONF           ! horizontal & vertical domain specifications
             USE UTILIO_DEFN         ! IO routines & utilties

!Arguments:
             INTEGER, INTENT( IN )  :: JDATE      
             INTEGER, INTENT( IN )  :: JTIME
! Local variables:             
             INTEGER                :: ALLOCSTAT     ! memory allocation status
             
             CHARACTER( 18 ), PARAMETER  :: PNAME = 'INIT_ACM_CLOUD'
                       
             CALL M3MESG('IMPORTANT NOTE: ATTEMPT TO INCLUDE SUBGRID CLOUD EFFECTS')
             CALL M3MESG('PHOTOLYSIS RATES INCLUDE ACM CLOUD EFFECTS')
             CALL M3MESG('EXAMINE DIAGNOSTIC CLOUD FRACTIONS IF REQUESTED')
             
             ALLOCATE ( ACM_CFRAC( NLAYS,MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
             IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ACM_CFRAC'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF
             ALLOCATE ( ACM_QC( NLAYS,MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
             IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating _ACM_QC'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF
             ALLOCATE ( ACM_QI( NLAYS,MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
             IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating _ACM_QI'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF
             ALLOCATE ( ACM_QR( NLAYS,MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
             IF ( ALLOCSTAT .NE. 0 ) THEN
                XMSG = 'Failure allocating _ACM_QR'
                CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF
             ALLOCATE ( ACM_QG( NLAYS,MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
             IF ( ALLOCSTAT .NE. 0 ) THEN
                XMSG = 'Failure allocating _ACM_QG'
                CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF
             ALLOCATE ( ACM_CLOUDS( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
             IF ( ALLOCSTAT .NE. 0 ) THEN
                XMSG = 'Failure allocating ACM_CLOUDS'
                CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF
             ALLOCATE ( ACM_AVE_H2O( MY_NCOLS,MY_NROWS ), STAT = ALLOCSTAT )
             IF ( ALLOCSTAT .NE. 0 ) THEN
                XMSG = 'Failure allocating ACM_AVE_H2O'
                CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF
 
             ACM_CLOUDS  = 0.0
             ACM_CFRAC   = 0.0
             ACM_AVE_H2O = 0.0
             
             RETURN
          END SUBROUTINE INIT_ACM_CLOUD        
          SUBROUTINE CLEAR_ACM_CLOUD( JDATE, JTIME )
          
             IMPLICIT NONE
! Purpose:
!          -Determine if acm cloud propeties have been set
!          -Zero out their array if not
! Arguments:
             INTEGER, INTENT( IN )  :: JDATE      
             INTEGER, INTENT( IN )  :: JTIME                 
! Local Variables: None


             IF( SET_ACM_ARRAYS )THEN
                 CALL INIT_ACM_CLOUD( JDATE, JTIME)
                 SET_ACM_ARRAYS = .FALSE.
             END IF

             IF( JDATE .NE. ACM_DATE .OR. JTIME .NE. ACM_TIME )THEN
                  ACM_CFRAC = 0.0
                  ACM_QC    = 0.0
                  ACM_QI    = 0.0
                  ACM_QR    = 0.0
                  ACM_QG    = 0.0
                  ACM_CLOUDS  = 0.0
                  ACM_AVE_H2O = 0.0
                  ACM_DATE    = JDATE
                  ACM_TIME    = JTIME
             END IF
             RETURN
          END SUBROUTINE CLEAR_ACM_CLOUD
       END MODULE PHOT_MET_DATA
