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

! Revision history: 01 Feb, 19 D. Wong: Implemented centralized I/O approach, 
!                                       removed all MY_N clauses

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      MODULE PHOT_MET_DATA

         IMPLICIT NONE

         REAL, ALLOCATABLE :: QC( :,:,: )  ! cloud water content [kg/kg]
         REAL, ALLOCATABLE :: QR( :,:,: )  ! rain water content [kg/kg]
         REAL, ALLOCATABLE :: QI( :,:,: )  ! ice content [kg/kg]
         REAL, ALLOCATABLE :: QS( :,:,: )  ! snow content [kg/kg]
         REAL, ALLOCATABLE :: QG( :,:,: )  ! graupel content [kg/kg]
         REAL, ALLOCATABLE :: QV( :,:,: )  ! water vapor content [kg/kg]
           
         REAL, ALLOCATABLE :: CFRAC_3D( :,:,: ) ! 3D fractional cloud coverage
         REAL, ALLOCATABLE :: CFRAC_2D( :,: )   ! layer averaged 3D fractional cloud coverage
         REAL, ALLOCATABLE :: AVE_HYDROMETEORS( :,: ) ! Cloud Hydrometeor Content averaged over cloudy layers, [g/m3]

! properties of ACM unresolved cloud
         LOGICAL           :: USE_ACM_CLOUD = .TRUE.
           
         REAL, ALLOCATABLE :: ACM_CFRAC( :,:,: )   ! total fractional cloud coverage
         REAL, ALLOCATABLE :: ACM_QC   ( :,:,: )   ! cloud water content [kg/kg]
         REAL, ALLOCATABLE :: ACM_QR   ( :,:,: )   ! rain water content [kg/kg]
         REAL, ALLOCATABLE :: ACM_QI   ( :,:,: )   ! ice content [kg/kg]
         REAL, ALLOCATABLE :: ACM_QG   ( :,:,: )   ! graupel content [kg/kg]
         REAL, ALLOCATABLE :: ACM_CLOUDS ( :,: )   ! subgrid cloud fractions averaged over cloudy layer
         REAL, ALLOCATABLE :: ACM_AVE_H2O( :,: )   ! subgrid cloud water content averaged over cloudy layer, [g/m3]

         REAL, ALLOCATABLE :: COSINE_ZENITH( :,: ) ! cosine of solar zenith angle
           
         REAL, ALLOCATABLE :: DENS   ( :,:,: )     ! air density [Kg/m **3]
         REAL, ALLOCATABLE :: TA     ( :,:,: )     ! air temperature [K]
         REAL, ALLOCATABLE :: PRES   ( :,:,: )     ! air pressure [Pa]
         REAL, ALLOCATABLE :: ZM     ( :,:,: )     ! layer half height agl [m]
         REAL, ALLOCATABLE :: ZFULL  ( :,:,: )     ! layer full height agl [m]
          
         REAL              :: DIST_TO_SUN          ! solar distance [au]

! public variables:
         PUBLIC :: QC, QR, QI, QS, QG, QV,
     &             CFRAC_2D, CFRAC_3D, AVE_HYDROMETEORS,
     &             USE_ACM_CLOUD, ACM_CFRAC, ACM_QC, ACM_QR, ACM_QI, ACM_QG,
     &             ACM_CLOUDS, ACM_AVE_H2O,
     &             COSINE_ZENITH,
     &             DENS, TA, PRES, ZM, ZFULL, DIST_TO_SUN

! public procedures:
         PUBLIC :: GET_PHOT_MET, UPDATE_SUN, CLEAR_ACM_CLOUD, 
     &             CAPTURE_ACM_CLOUD
           
         PRIVATE

         INTEGER           :: HYDROMETEORS ! denotes mix of hydrometeors available
!        value   cloud water  rain       ice        snow     graupel
!          1        x          x
!          2        x          x          x          x
!          3        x          x          x          x          x

         REAL, ALLOCATABLE :: CFRAC   ( :,: )      ! total fractional cloud coverage
         REAL, ALLOCATABLE :: DELTA_Z( :,:,: )     ! layer thickness [m]

! flags for hydrometeors available on file:
!        LOGICAL           :: QV_AVAIL       = .TRUE.   ! QV (vapor)
!        LOGICAL           :: QC_AVAIL       = .TRUE.   ! QC (liquid)
!        LOGICAL           :: QR_AVAIL       = .TRUE.   ! QR (rain)
!        LOGICAL           :: QI_AVAIL       = .TRUE.   ! QI (ice)
!        LOGICAL           :: QS_AVAIL       = .TRUE.   ! QS (snow)
!        LOGICAL           :: QG_AVAIL       = .TRUE.   ! QG (graupel)
!        LOGICAL           :: CFRAC_3D_AVAIL = .TRUE.   ! CFRAC_3D

         LOGICAL           :: FIRSTIME = .TRUE. ! flag for first pass thru

         CHARACTER( 16 )   :: VARNM            ! variable name for IOAPI to get
         CHARACTER( 16 )   :: VNAME_RN
         CHARACTER( 300 )  :: XMSG = ' '       ! Exit status message

         INTEGER           :: LDATE             ! last jdate MET data updated
         INTEGER           :: LTIME             ! last jtime MET data updated
         INTEGER           :: STDATE            ! jdate MET module initiated
         INTEGER           :: STTIME            ! jtime MET module initiated
         INTEGER           :: ACM_DATE = -1     ! last date routine called
         INTEGER           :: ACM_TIME = -1     ! last time routine called
          
         INTEGER           :: GXOFF, GYOFF      ! global origin offset from file

         REAL, ALLOCATABLE :: SINLATS( :,: ) ! sine of latitude
         REAL, ALLOCATABLE :: COSLATS( :,: ) ! cosine of latitude
         REAL              :: STRTHR         ! starting GMT hour

         LOGICAL           :: SET_ACM_ARRAYS = .TRUE.

         CONTAINS 
       
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE INIT_PHOT_MET( JDATE, JTIME )

         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE UTILIO_DEFN
         USE CENTRALIZED_IO_MODULE

         IMPLICIT NONE

!...........Includes:

         INCLUDE SUBST_CONST                ! constants
         INCLUDE SUBST_FILES_ID             ! file name parameters
       
!...........Inputs:       

         INTEGER, INTENT( IN ) :: JDATE     ! current model date, coded YYYYDDD
         INTEGER, INTENT( IN ) :: JTIME     ! current model time, coded HHMMSS
       
!...........Local:
         INTEGER               :: COL       ! column loop counter
         INTEGER               :: ROW       ! row loop counter
         INTEGER               :: ASTAT     ! memory allocation status

         CHARACTER( 16 )       :: PNAME = 'INIT_PHOT_MET' 
       
         LOGICAL,  SAVE        :: INITIALIZED = .FALSE.
       
         IF ( INITIALIZED ) RETURN
       
         INITIALIZED = .TRUE.
       
! set up variables for calculating cosine of solar zenith angle

         ALLOCATE ( SINLATS( NCOLS,NROWS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating SINLATS'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( COSLATS( NCOLS,NROWS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating COSLATS'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         DO ROW = 1, NROWS
            DO COL = 1, NCOLS
                SINLATS( COL,ROW ) = SIN( PI180 * LAT ( COL,ROW ) )
                COSLATS( COL,ROW ) = COS( PI180 * LAT ( COL,ROW ) )
             END DO
         END DO

         ALLOCATE ( COSINE_ZENITH( NCOLS,NROWS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating COSINE_ZENITH'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( CFRAC( NCOLS,NROWS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating CFRAC'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( DENS( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DENS'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( TA( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating TA'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( PRES( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating PRES'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( ZM( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating ZM'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         
         ALLOCATE ( ZFULL( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating ZFULL'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( DELTA_Z( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DELTA_Z'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( QV( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating QV'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( QC( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating QC'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( QR( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating QR'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         
         HYDROMETEORS = 1   ! always cloud water and rain

! test for which optional hydrometeors are available
! For WRF/MCIP: if GQ is available, all hydrometeors are available
!               else if QI or QS is available, QC and QR are available

         IF ( QG_AVAIL ) THEN
            HYDROMETEORS = 3
         ELSE
            WRITE( LOGDEV, '(3(/10X,A),(/10X,3(A,1X)),(/10X,A))' )
     &           'YOU SHOULD VERIFY that the cloud microphysics scheme used',
     &           'in the Meteorological Model did not include graupel.  If',
     &           'it did, then you need to reprocess the meteorological data',
     &           'through MCIP and pass QG to file ',
     &           TRIM( MET_CRO_3D ), ' to avoid',
     &           'errors in the photolysis simulation.'
            WRITE( LOGDEV, '((/5X,A),/)' )
     &           'Processing will continue with QG set to ZERO.  <<--<<'
            IF ( QI_AVAIL ) THEN
               HYDROMETEORS = 2
            ELSE
               WRITE( LOGDEV, '(3(/10X,A),(/10X,3(A,1X)),(/10X,A))' )
     &              'YOU SHOULD VERIFY that the cloud microphysics scheme used',
     &              'in the Meteorological Model did not include ice/snow.  If',
     &              'it did, then you need to reprocess the meteorological data',
     &              'through MCIP and pass QI to file ',
     &              TRIM( MET_CRO_3D ), ' to avoid',
     &              'errors in the photolysis simulation.'
               WRITE( LOGDEV, '((/5X,A),/)' )
     &              'Processing will continue with QI set to ZERO.  <<---<<'
            END IF
         END IF

         ALLOCATE ( QG( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating QG'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF        
         QG = 0.0
        
         ALLOCATE ( QI( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating QI'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         QI = 0.0

         ALLOCATE ( QS( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating QS'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         QS = 0.0

         IF ( SET_ACM_ARRAYS .AND. USE_ACM_CLOUD ) THEN
            CALL INIT_ACM_CLOUD( 0, 0 )
            SET_ACM_ARRAYS = .FALSE.
         END IF

         IF ( .NOT. CFRAC_3D_AVAIL ) THEN
            XMSG = 'Variable CFRAC_3D (cloud fraction) not found in ' // MET_CRO_3D
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            WRITE( LOGDEV, * )
            WRITE( XMSG, '(4A)' )
     &           'Model will diagnose layer cloud fraction using Randall (1995) ',
     &           'and Hong (1998), based on mixing ratios of water vapor and its ',
     &           'condensed states. You may want to reprocess the meteorological ',
     &           'data to obtain layer cloud fractions if possible.'
            CALL LOG_MESSAGE( LOGDEV, XMSG )

         END IF
        
         ALLOCATE ( CFRAC_3D( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating CFRAC_3D'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         CFRAC_3D = 0.0

         ALLOCATE ( CFRAC_2D( NCOLS,NROWS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating CFRAC_2D'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( AVE_HYDROMETEORS( NCOLS,NROWS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating AVE_HYDROMETEORS'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

! report out hydrometeors found
         SELECT CASE( HYDROMETEORS )
            CASE( 1 )
                 WRITE( LOGDEV,2021 ) TRIM( PNAME ), "QC and QR"
            CASE( 2 )
                 WRITE( LOGDEV,2021 ) TRIM( PNAME ), "QC, QR, QI and QS"
            CASE( 3 )
                 WRITE( LOGDEV,2021 ) TRIM( PNAME ), "QC, QR, QG, QI and QS"
         END SELECT        
2021     FORMAT( /10X, A, ": Cloud has ", A )
        
         LDATE  = 0
         LTIME  = 0

         STDATE = JDATE
         STTIME = JTIME
         STRTHR = FLOAT( JTIME / 10000 )

         FIRSTIME = .FALSE.
        
         END SUBROUTINE INIT_PHOT_MET

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE GET_PHOT_MET( JDATE, JTIME, MDATE, MTIME )
        
         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE UTILIO_DEFN         ! IO routines & utilties
         USE CENTRALIZED_IO_MODULE

         IMPLICIT NONE

!  REVISION  HISTORY:
!  B.Hutzell Oct 13, 2015 Changed computation of effective resolved fraction, CFRAC_2D,
!             to be consisitent with random cloud overlap model that in used in the 
!             radiative transfer calculation. Consult Tain and Curry (1989), JGR, vol.94,
!             9925-9935 for more information.
!...........Includes:

         INCLUDE SUBST_CONST                ! constants
         INCLUDE SUBST_FILES_ID             ! file name parameters

         INTEGER, INTENT( IN ) :: JDATE     ! current model date, coded YYYYDDD
         INTEGER, INTENT( IN ) :: JTIME     ! current model time, coded HHMMSS
         INTEGER, INTENT( IN ) :: MDATE     ! date for calculation/interpolation(yyyyddd)
         INTEGER, INTENT( IN ) :: MTIME     ! time for calculation/interpolation (hhmmss)

!..........Local:

         CHARACTER( 16 ), SAVE :: PNAME =  'GET_PHOT_MET'
        
         INTEGER               :: COL       ! column loop counter
         INTEGER               :: ROW       ! row loop counter
         INTEGER               :: LAY       ! layer loop counter
        
         INTEGER               :: ASTAT           ! allocation status measure
        
         REAL                  :: SUM_WEIGHTS
         REAL                  :: WEIGHT
         REAL                  :: WATER
       
       
         IF ( FIRSTIME ) THEN
            CALL INIT_PHOT_MET( JDATE, JTIME )
         END IF

!...store met file time, date, and step information and compute
!...  the met timestep in hours

         IF ( LDATE .EQ. MDATE .AND. LTIME .EQ. MTIME ) RETURN
        
         LDATE = MDATE
         LTIME = MTIME

!...  Interpolate time dependent layered input variables

         VARNM = 'ZH' ! midlayer height
         call interpolate_var (VARNM, mdate, mtime, ZM)

         VARNM = 'ZF' ! full layer height
         call interpolate_var (VARNM, mdate, mtime, ZFULL)

!...Get air density (kg/m3)

         VARNM = 'DENS'
         call interpolate_var (VARNM, mdate, mtime, DENS)

!...get temperature [K]

         VARNM = 'TA'
         call interpolate_var (VARNM, mdate, mtime, TA)
 
!...pressure [Pa]

         VARNM = 'PRES'
         call interpolate_var (VARNM, mdate, mtime, PRES)

!...Get resolved cloud fractions

         VARNM = 'CFRAC'
         call interpolate_var (VARNM, mdate, mtime, CFRAC)

!...Get resolved water vapor mixing ratio (kg H2O / kg air)

         VARNM = 'QV'
         call interpolate_var (VARNM, mdate, mtime, QV)

!...Get resolved cloud water mixing ratio (kg H2O / kg air)
         VARNM = 'QC'
         call interpolate_var (VARNM, mdate, mtime, QC)

!...Get resolved rain water mixing ratio (kg H2O / kg air)
         VARNM = 'QR'
         call interpolate_var (VARNM, mdate, mtime, QR)

         IF ( QG_AVAIL ) THEN
!...read resolved graupel and the other hydrometeor mixing ratios (kg H2O / kg air)
!...from the met file if available
            VARNM = 'QG'
            call interpolate_var (VARNM, mdate, mtime, QG)

!...read resolved ice and snow mixing ratios (kg H2O / kg air)
            VARNM = 'QI'
            call interpolate_var (VARNM, mdate, mtime, QI)

            VARNM = 'QS'
            call interpolate_var (VARNM, mdate, mtime, QS)

         ELSE IF ( QI_AVAIL ) THEN
!...read resolved ice and snow mixing ratios (kg H2O / kg air) from the met
!...from the met file if available
            VARNM = 'QI'
            call interpolate_var (VARNM, mdate, mtime, QI)

            VARNM = 'QS'
            call interpolate_var (VARNM, mdate, mtime, QS)

         END IF
         
! floor values of QC, QR, QI, QS, and QG to zero

         IF ( QG_AVAIL ) THEN
            DO LAY = 1, NLAYS
               DO ROW = 1, NROWS
                  DO COL = 1, NCOLS
                     QC( COL,ROW,LAY ) = MAX( QC( COL,ROW,LAY ), 0.0 )
                     QR( COL,ROW,LAY ) = MAX( QR( COL,ROW,LAY ), 0.0 )
                     QI( COL,ROW,LAY ) = MAX( QI( COL,ROW,LAY ), 0.0 )
                     QS( COL,ROW,LAY ) = MAX( QS( COL,ROW,LAY ), 0.0 )
                     QG( COL,ROW,LAY ) = MAX( QG( COL,ROW,LAY ), 0.0 )
                  END DO
               END DO
            END DO
         ELSE IF ( QI_AVAIL ) THEN
            DO LAY = 1, NLAYS
               DO ROW = 1, NROWS
                  DO COL = 1, NCOLS
                     QC( COL,ROW,LAY ) = MAX( QC( COL,ROW,LAY ), 0.0 )
                     QR( COL,ROW,LAY ) = MAX( QR( COL,ROW,LAY ), 0.0 )
                     QI( COL,ROW,LAY ) = MAX( QI( COL,ROW,LAY ), 0.0 )
                     QS( COL,ROW,LAY ) = MAX( QS( COL,ROW,LAY ), 0.0 )
                  END DO
               END DO
            END DO
         ELSE
            DO LAY = 1, NLAYS
               DO ROW = 1, NROWS
                  DO COL = 1, NCOLS
                     QC( COL,ROW,LAY ) = MAX( QC( COL,ROW,LAY ), 0.0 )
                     QR( COL,ROW,LAY ) = MAX( QR( COL,ROW,LAY ), 0.0 )
                  END DO
               END DO
            END DO
         END IF

         DO ROW = 1, NROWS 
            DO COL = 1, NCOLS
               DELTA_Z( COL,ROW,1 ) = ZFULL( COL,ROW,1 )
               FORALL ( LAY = 2:NLAYS )
                  DELTA_Z( COL,ROW,LAY ) = ZFULL( COL,ROW,LAY ) - ZFULL( COL,ROW,LAY-1 )
               END FORALL
            END DO
         END DO

         IF ( CFRAC_3D_AVAIL ) THEN  ! read from file
            VARNM = 'CFRAC_3D'
            call interpolate_var (VARNM, mdate, mtime, CFRAC_3D)
 
         ELSE  ! diagnose CFRAC_3D from available data

            CALL CAL_CFRAC_3D()

         END IF

! compute averages for resolved cloud                

         DO ROW = 1, NROWS
            DO COL = 1, NCOLS
               SUM_WEIGHTS = 0.0
               CFRAC_2D( COL,ROW ) = 1.0
               AVE_HYDROMETEORS( COL,ROW ) = 0.0
               DO LAY = 1, NLAYS
                  IF ( CFRAC_3D( COL,ROW,LAY ) .LE. 0.0 ) CYCLE
                  WEIGHT = DELTA_Z( COL,ROW,LAY ) 
                  CFRAC_2D( COL,ROW ) = CFRAC_2D( COL,ROW )
     &                                * ( 1.0 - CFRAC_3D( COL,ROW,LAY ) )
                  SUM_WEIGHTS = SUM_WEIGHTS + WEIGHT

                  WATER = QC( COL,ROW,LAY ) + QR( COL,ROW,LAY ) + QI( COL,ROW,LAY )
     &                  + QS( COL,ROW,LAY ) + QG( COL,ROW,LAY )

                  AVE_HYDROMETEORS( COL,ROW ) = AVE_HYDROMETEORS( COL,ROW )
     &                                        + DENS( COL,ROW,LAY ) * WATER * WEIGHT
#ifdef phot_debug     
                  if ( cfrac_2d( col,row ) .ne. cfrac_2d( col,row ) .or.
     &                 sum_weights .ne. sum_weights ) then
                     write( logdev,"(a,2es12.4)" ) "WEIGHT, CFRAC_3D = ",
     &                                             weight, cfrac_3d( col,row,lay ) 
                     xmsg = 'Floating point exception for CFRAC_2D'
                     call m3exit ( pname, jdate, jtime, xmsg, xstat1 )
                  end if
#endif              
               END DO
               IF ( SUM_WEIGHTS .LT. 1.0 ) THEN 
                    CFRAC_2D( COL,ROW ) = 0.0
               ELSE
                    CFRAC_2D( COL,ROW ) = 1.0 - CFRAC_2D( COL,ROW )
                    AVE_HYDROMETEORS( COL,ROW ) = 1.0E+3 * AVE_HYDROMETEORS( COL,ROW ) / SUM_WEIGHTS
               END IF
            END DO
         END DO

         IF ( USE_ACM_CLOUD ) THEN ! compute averages for subgrid clouds                
            DO ROW = 1, NROWS
               DO COL = 1, NCOLS
                  SUM_WEIGHTS  = 0.0
!                 ACM_CLOUDS( COL,ROW )  = 0.0
                  ACM_AVE_H2O( COL,ROW ) = 0.0
                  DO LAY = 1, NLAYS
                     IF ( ACM_CFRAC( LAY,COL,ROW ) .LE. 0.0 ) CYCLE
                     WEIGHT      = DELTA_Z( COL,ROW,LAY ) 
                     SUM_WEIGHTS = SUM_WEIGHTS + WEIGHT
! acm clouds do not contain snow
                     WATER = ACM_QC( LAY,COL,ROW ) + ACM_QI( LAY,COL,ROW ) 
     &                     + ACM_QG( LAY,COL,ROW ) + ACM_QR( LAY,COL,ROW )  

! assume acm cloud is uniform or constant if it does not equal zero so set in  
! subroutine CAPTURE_ACM_CLOUD
!                    ACM_CLOUDS( COL,ROW ) = ACM_CLOUDS( COL,ROW )
!     &                                    + WEIGHT * ACM_CFRAC( LAY,COL,ROW ) 

                     ACM_AVE_H2O( COL,ROW ) = ACM_AVE_H2O( COL,ROW )
     &                                      + DENS( COL,ROW,LAY ) * WEIGHT * WATER
                  END DO
                  IF ( SUM_WEIGHTS .LT. 1.0 ) CYCLE
!                    ACM_CLOUDS( COL,ROW )  = ACM_CLOUDS( COL,ROW ) / SUM_WEIGHTS
                     ACM_AVE_H2O( COL,ROW ) = 1.0E+3 * ACM_AVE_H2O( COL,ROW ) / SUM_WEIGHTS
               END DO
            END DO
         END IF

         END SUBROUTINE GET_PHOT_MET

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE UPDATE_SUN( JDATE, JTIME, MDATE, MTIME )

         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE UTILIO_DEFN         ! IO routines & utilties
         USE CENTRALIZED_IO_MODULE
  
         IMPLICIT NONE     
          
!***include files

         INCLUDE SUBST_CONST      ! physical constants
           
! Inputs:  
         INTEGER, INTENT( IN ) :: JDATE            ! current model date, coded YYYYDDD
         INTEGER, INTENT( IN ) :: JTIME            ! current model time, coded HHMMSS
         INTEGER, INTENT( IN ) :: MDATE            ! date for calculation/interpolation(yyyyddd)
         INTEGER, INTENT( IN ) :: MTIME            ! time for calculation/interpolation (hhmmss)

! Local:
         INTEGER, SAVE         :: PDATE = 0        ! previous date called (yyyyddd)
         INTEGER, SAVE         :: PTIME = 0        ! previous time called (hhmmss)
         INTEGER               :: COL              ! column loop counter
         INTEGER               :: ROW              ! row loop counter

         REAL                  :: SINDEC           ! sine of the solar declination
         REAL                  :: COSDEC           ! cosine of the solar declination
         REAL                  :: JYEAR            ! year
         REAL                  :: JDAY             ! current Julian day (DDD)
         REAL                  :: EQUATION_OF_TIME ! equation of time
         REAL                  :: CURRHR           ! current GMT hour
         REAL                  :: CURRHR_LST       ! local standard time at each grid cell
        
         REAL, PARAMETER       :: ONE_OVER_15   = 1.0 / 15.0
         REAL, PARAMETER       :: ONE_OVER_3600 = 1.0 / 3600     

         IF ( FIRSTIME ) CALL INIT_PHOT_MET( JDATE, JTIME )

         IF ( PDATE .EQ. JDATE .AND. PTIME .EQ. JTIME ) RETURN
        
         PDATE = JDATE
         PTIME = JTIME
   
! Compute distance to sun and set solar declination variables

         JDAY   = FLOAT( MOD( JDATE, 1000 ) )
         JYEAR  = FLOAT( JDATE / 1000  )        

         CURRHR = STRTHR
     &          + FLOAT( SECSDIFF( STDATE, STTIME, MDATE, MTIME ) )
     &          * ONE_OVER_3600

         CALL SOLEFM3 ( JYEAR, JDAY, EQUATION_OF_TIME, SINDEC, COSDEC, DIST_TO_SUN )
        
         DO ROW = 1, NROWS
            DO COL = 1, NCOLS

!...correct  CURRHR for current *positive* West longitude convention
!...to obtain LST.
!...this convention on longititude should be reexamined for different domains

               CURRHR_LST = CURRHR + LON( COL,ROW ) * ONE_OVER_15

               CALL GETZEN2 ( CURRHR_LST , SINLATS( COL,ROW ), COSLATS( COL,ROW ),
     &                        SINDEC, COSDEC, EQUATION_OF_TIME, 
     &                        COSINE_ZENITH( COL,ROW ) )

#ifdef verbose_update_sun
               if ( row .eq. 1 .and. col .eq. 1 ) then
                  write(logdev,"(a,3es12.4)") "PHT: JYEAR, JDSTRT, SOLDIST = ",
     &                                        jyear, jday, dist_to_sun
                  write(logdev,"(a,7es12.4)") "PHT: CURRHR_LST, SINLAT, COSLAT, " //
     &                                        "SINDEC, COSDEC, EQT, COSZEN = ",
     &                                         currhr_lst , sinlats( col,row ),
     &                                         coslats( col,row ), sindec, cosdec,
     &                                         equation_of_time, cosine_zenith( col,row )
               end if
#endif            

            END DO
         END DO
               
         END SUBROUTINE UPDATE_SUN

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE SOLEFM3 ( YEAR, DAY, EQT, SINDEC, COSDEC, SOLDIST )

C-----------------------------------------------------------------------
C  This subroutine calculates the sine and cosine of the solar
C     declination and the equation of time, and solar distance
C     using an approximation to equations used to generate the
C     tables in  The Astronomical Almanac.
 
C  input:
C     year = year (e.g. 1998)
C     day  = day of the year (e.g. Feb. 2 is 33)
C     lat  = latitude in degrees (north is positive)
C     long = longitude in degrees (east is positive)
 
C  output:
C     EQT    = Equation of Time
C     sindec = sine of the solar declination
C     cosdec = cosine of the solar declination
C     soldist = distance of sun in astronomical units

C  Revision history:
C  FSB Coded September 9, 2004 by Dr. Francis S. Binkowski
C     Environmental Modeling for Policy Development group,
C     The Carolina Environmental Program
C     The University of North Carolina-Chapel Hill
C     Email: frank_binkowski@unc.edu
 
C  REFERENCE:The code is a modification of that  from
C     J.J. Michalsky, The Astronomical Almanac`s algorithm for
C     approximate solar position (1950-2050)
C     Solar Energy vol. 40, No. 3, pp 227-235, 1988.
C     who based it upon
C     The Astronomical Almanac, U.S. Gov`t. Printing Office,
C     Washington DC. Page c24 has the algorithm
 
C  The stated accuracy is 0.01 degree for the epoch ( 1950 - 2050).
C-----------------------------------------------------------------------

         IMPLICIT NONE

!***arugments

         REAL, INTENT( IN )  :: YEAR, DAY
         REAL, INTENT( OUT ) :: EQT, SINDEC, COSDEC, SOLDIST

!***include files

         INCLUDE SUBST_CONST      ! physical constants

!***parameters

! from SUBST_CONST:   REAL, PARAMETER :: PI = 3.1415927
         REAL, PARAMETER :: TWOPI = 2.0 * PI
! from SUBST_CONST:   REAL, PARAMETER :: PI180   = PI / 180.0
         REAL, PARAMETER :: RAD   = PI180
         REAL, PARAMETER :: RADM1 = 1.0 / RAD

!***FSB inline documentation defines the variables.

         REAL    :: DELTA, LEAP, JD, TIME, L, G, LAMDA, EPSILON
         REAL    :: ALPHA
         REAL    :: SING, SIN2G, COSG, COS2G
         REAL(8) :: NUM, DEN

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

         NUM = REAL( COS( EPSILON ) * SIN( LAMDA ), 8 ) ! numerator
         DEN = REAL( COS( LAMDA ), 8 )                  ! denominator
         ALPHA = REAL( ATAN( NUM / DEN ), 4)

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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE GETZEN2 ( TIME, SINLAT, COSLAT, SINDEC, COSDEC, EQT,
     &                        COSZEN )
C-----------------------------------------------------------------------
C  calculate the zenith angle information at the present time step
 
C  input:
C     time    = LST in decimal hours <<NOTE: LST
C     sinlat  = sine of the latitude
C     coslat  = cosine of the latitude
C     sindec  = sine of the solar declination
C     cosdec  = cosine of the solar declination
C     EQT     = equation of time in degrees
 
C  Output:
C     coszen  = cosine of the zenith angle
 
C  Revision history:
C     FSB Coded September 10, 2004 by Dr. Francis S. Binkowski
C     FSB modified February 15, 2005 to increase efficiency
C     Environmental Modeling for Policy Development group,
C     The Carolina Environmental Program
C     The University of North Carolina-Chapel Hill
C     Email: frank_binkowski@unc.edu
 
C  Note: because this routine will be called frequently
C     during a simulation, the sines and cosines of the
C     latitude should be calculated once at the beginning of
C     simulation.
C-----------------------------------------------------------------------

         IMPLICIT NONE

!***include files

         INCLUDE SUBST_CONST      ! physical constants

!***arguments

         REAL, INTENT( IN )  :: TIME, SINLAT, COSLAT
         REAL, INTENT( IN )  :: SINDEC, COSDEC, EQT
         REAL, INTENT( OUT ) :: COSZEN

!***internal variables

         REAL HANGLE          ! local hour angle in degrees

! from SUBST_CONST:  REAL, PARAMETER :: PI = 3.1415927
! from SUBST_CONST:  REAL, PARAMETER :: PI180 = PI / 180.0
         REAL, PARAMETER :: RAD = PI180

         HANGLE = 15.0 * ( TIME - 12.0 )  + EQT
         COSZEN = SINDEC * SINLAT + COSLAT * COSDEC * COS( RAD * HANGLE )

!***do not allow zenith angle to be lower than 1 degree (COSZEN = 0.999998)
!***when COSZEN is 1.0, gas-phase chemistry solver does not work properly 
         COSZEN = MIN( COSZEN, 0.999998 )

         RETURN
         END SUBROUTINE GETZEN2        

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE CAL_CFRAC_3D ()

!---------------------------------------------------------------------
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
! 1) Randall, D. A. (1995), Parameterizing fractional cloudiness produced
!    by cumulus entrainment. Preprints, Workshop on Cloud Microphysics
!    Parameterizations in Global Atmospheric Circulation
!    Models, Kananaskis, AB, Canada, WMO, 1–16.
! 2) Xu, K. And D.A. Randall (1996), A semiempircal cloudiness 
!    parameterization for use in climate models, J. Atm. Sci., vol 53(21),
!    pp 3084-3102.
! 3) Hong et al. (1998). Implementation of Prognostic Cloud Scheme for a
!    Regional Spectral Model. Monhtly Weather Review, vol. 126, pp 2621-
!    2639.
! Routine copied from module_radiation_driver.F in WRF 3.5 and modified for
! CMAQ applications (Bill Hutzell), Revised somewhat by J. Young Nov,2014
!---------------------------------------------------------------------

         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE UTILIO_DEFN         ! IO routines & utilties
         USE CENTRALIZED_IO_MODULE

         IMPLICIT NONE

! includes:
         INCLUDE SUBST_CONST     ! physical and mathematical constants

! arguments:

!  Local:
         INTEGER :: I, J, K   ! loop counters
          
         REAL    :: RHUM       ! relative humidty   (fraction)
         REAL    :: TC         ! temp               (degrees Celuis)
         REAL    :: ESW        ! water vapor liquid saturaturion vapor pressure (Pa)
         REAL    :: ESI        ! water vapor ice saturaturion vapor pressure (Pa)
         REAL    :: QVSW       ! water vapor saturation mixing ratio (Kg/Kg)
         REAL    :: QVSI       ! water ice saturation mixing ratio (Kg/Kg) 
         REAL    :: QVS_WEIGHT ! weighted average of liquid and ice mixing ratios
         REAL    :: DENOM      ! scratch varaible
         REAL    :: ARG        ! scratch varaible
         REAL    :: SUBSAT     ! scratch varaible
         REAL, ALLOCATABLE, SAVE :: QCLD  ( :,:,: ) ! cloud water mixing ratio
         REAL, ALLOCATABLE, SAVE :: WEIGHT( :,:,: ) ! weighting factor between liquid and ice content
         REAL    :: WHT        ! intermediate weighting factor
         INTEGER :: ASTAT      ! memory allocation status
         LOGICAL, SAVE :: FRSTCALL = .TRUE.

! Parameters:
! Used for saturation mixing ratio weighted based on fractions of water and ice. 
! Following:
!   Murray, F.W. 1966. ``On the computation of Saturation Vapor Pressure,'
!   J. Appl. Meteor.,  6, p. 204.
! For saturation vapor pressure for each water phase
! es (in mb) = 6.1078exp[ a(T-273.16)/ (T-b) ], 1 mb = 100 Pa
!     over water   over ice  
! a = 17.2693882   21.8745584
! b = 35.86         7.66 
!        REAL, PARAMETER :: SVP1  =   0.61078
         REAL, PARAMETER :: SVP1  = 610.78      ! [ Pa ]
         REAL, PARAMETER :: SVP2  =  17.2693882
         REAL, PARAMETER :: SVP3  =  35.86
         REAL, PARAMETER :: SVPI2 =  21.8745584
         REAL, PARAMETER :: SVPI3 =   7.66
!        REAL, PARAMETER :: SVPT0 = 273.15  STDTEMP in CONST.EXT
!        REAL, PARAMETER :: R_D   = 287.0   RDGAS in CONST.EXT
!        REAL, PARAMETER :: R_V   = 461.6   RWVAP in CONST.EXT
         REAL, PARAMETER :: SVPT0 = STDTEMP
         REAL, PARAMETER :: R_D   = RDGAS
         REAL, PARAMETER :: R_V   = RWVAP
         REAL, PARAMETER :: EP_2  = R_D / R_V
!-----------------------------------------------------------------------
!---  Parameters for GRID-SCALE CLOUD COVER FOR RADIATION
!     (modified by Ferrier, Feb '02)
!
!---  Cloud fraction parameterization follows Randall, 1994
!     (see Hong et al., 1998)
!-----------------------------------------------------------------------
         REAL, PARAMETER :: ALPHA0  = 1.0E+02
         REAL, PARAMETER :: GAMMA   = 0.49
         REAL, PARAMETER :: QCLDMIN = 1.0E-12
         REAL, PARAMETER :: PEXP    = 0.25
         REAL, PARAMETER :: RHGRID  = 1.0
   
         IF ( FRSTCALL ) THEN
            FRSTCALL = .FALSE.
            ALLOCATE ( QCLD  ( NCOLS,NROWS,NLAYS ),
     &                 WEIGHT( NCOLS,NROWS,NLAYS ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating QCLD or WEIGHT'
               CALL M3EXIT ( 'CAL_CFRAC_3D', 0, 0, XMSG, XSTAT1 )
            END IF
         END IF

! Alternative calculation for critical RH for grid saturation
!     RHGRID = 0.90 + .08*((100.-DX)/95.)**.5
   
!---------------------------------------------------------------------

!--- Total "cloud" mixing ratio, QCLD.  Rain is not part of cloud,
!    only cloud water + cloud ice + snow
! Mixing ratios of rain are not considered in this scheme.

         CFRAC_3D = 0.0   ! array assignment
         
         IF ( QG_AVAIL ) THEN
            DO K = 1, NLAYS
               DO J = 1, NROWS
                  DO I = 1, NCOLS
                     QCLD(I,J,K) = QC(I,J,K) + QI(I,J,K) + QS(I,J,K) + QG(I,J,K)
                     IF ( QCLD(I,J,K) .GE. QCLDMIN ) THEN
                        WEIGHT(I,J,K) = ( QI(I,J,K) + QS(I,J,K) + QG(I,J,K) ) / QCLD(I,J,K)
                     ELSE
                        WEIGHT(I,J,K) = 0.0
                     END IF
                  END DO
               END DO
            END DO
         ELSE IF ( QI_AVAIL ) THEN
            DO K = 1, NLAYS
               DO J = 1, NROWS
                  DO I = 1, NCOLS
                     QCLD(I,J,K) = QC(I,J,K) + QI(I,J,K) + QS(I,J,K)
                     IF ( QCLD(I,J,K) .GE. QCLDMIN ) THEN
                        WEIGHT(I,J,K) = ( QI(I,J,K) + QS(I,J,K) ) / QCLD(I,J,K)
                     ELSE
                        WEIGHT(I,J,K) = 0.0
                     END IF
                  END DO
               END DO
            END DO
         ELSE IF ( QC_AVAIL ) THEN
            DO K = 1, NLAYS
               DO J = 1, NROWS
                  DO I = 1, NCOLS
                     QCLD(I,J,K) = QC(I,J,K)
                     IF ( QCLD(I,J,K) .GE. QCLDMIN ) THEN
                        IF ( TA(I,J,K) .GT. 273.15 ) WEIGHT(I,J,K) = 0.0
                        IF ( TA(I,J,K) .LE. 273.15 ) WEIGHT(I,J,K) = 1.0
                     ELSE
                        WEIGHT(I,J,K) = 0.0
                     END IF
                  END DO
               END DO
            END DO
         ELSE          
            RETURN
         END IF !  IF ( QG_AVAIL )

         DO K = 1, NLAYS
            DO J = 1, NROWS
               DO I = 1, NCOLS
!--- Determine cloud fraction (modified from original algorithm)
                  IF ( QCLD(I,J,K) .LT. QCLDMIN ) THEN
!--- Assume zero cloud fraction if there is no cloud mixing ratio
                     CFRAC_3D(I,J,K) = 0.0
                     CYCLE
                  END IF
                  
                  TC   = TA(I,J,K) - SVPT0
                  ESW  = SVP1 * EXP( SVP2  * TC / ( TA(I,J,K) - SVP3  ) )
                  ESI  = SVP1 * EXP( SVPI2 * TC / ( TA(I,J,K) - SVPI3 ) )
                  QVSW = EP_2 * ESW / ( PRES(I,J,K) - ESW )
                  QVSI = EP_2 * ESI / ( PRES(I,J,K) - ESI )
                  WHT  = WEIGHT(I,J,K)
!                 QVS_WEIGHT = ( 1.0 - WHT ) * QVSW + WHT * QVSI
                  QVS_WEIGHT = QVSW + WHT * ( QVSI - QVSW )
                  RHUM = QV(I,J,K) / QVS_WEIGHT   !--- Relative Humidity                  
                  
                  IF ( RHUM .GE. RHGRID ) THEN
!--- Assume cloud fraction of unity if near saturation and the cloud
!    mixing ratio is at or above the minimum threshold
                     CFRAC_3D(I,J,K) = 1.0
                     CYCLE
                  ELSE
!--- Adaptation of original algorithm (Randall, 1994; Zhao, 1995)
!    modified based on assumed grid-scale saturation at RH=RHgrid.
                     SUBSAT = MAX( 1.E-10, RHGRID * QVS_WEIGHT - QV(I,J,K) )
                     DENOM  = SUBSAT ** GAMMA
                     ARG    = MAX( -6.9, -ALPHA0 * QCLD(I,J,K) / DENOM )   ! <-- EXP(-6.9)=.001
                     CFRAC_3D(I,J,K) = ( RHUM / RHGRID ) ** PEXP * ( 1.0 - EXP( ARG ) )
                  END IF          !--- END IF (QCLD(I,J,K) .LT. QCLDMIN)

                  IF ( CFRAC_3D(I,J,K) .LT. 0.01 ) CFRAC_3D(I,J,K) = 0.0

                  IF ( CFRAC_3D(I,J,K) .NE. CFRAC_3D(I,J,K) .OR.
     &                 CFRAC_3D(I,J,K) .GT. 1.0 ) THEN 
                     WRITE( LOGDEV,'(/2X,A,3I6)' ) 'COL, ROW, LAY = ', I, J, K
                     WRITE( LOGDEV,'(/2X,A,4ES12.4)' )
     &                     'TA, PRES = ', TA(I,J,K), PRES(I,J,K)
                     WRITE( LOGDEV,'(/2X,A,4ES12.4)' )
     &                     'QV, QC, QI, QS = ', QV(I,J,K), QC(I,J,K), QI(I,J,K), QS(I,J,K)
                     WRITE( LOGDEV,'(/2X,A,4ES12.4)' )
     &                     'ESW, ESI, QVSW, QVSI = ', ESW, ESI, QVSW, QVSI
                     WRITE( LOGDEV,'(/2X,A,4ES12.4)' )
     &                     'WEIGH, QVS_WEIGH = ', WHT, QVS_WEIGHT 
                     WRITE( LOGDEV,'(/2X,A,4ES12.4)' )
     &                     'SUBSAT, RHUM, RHGRID, ARG = ', SUBSAT, RHUM, RHGRID, ARG
                     XMSG = 'Floating point exception for CFRAC_3D'
                     CALL M3EXIT ( 'CAL_CFRAC_3D', 0, 0, XMSG, XSTAT1 )
                  END IF

               END DO   ! i
            END DO   ! j
         END DO   ! k

         END SUBROUTINE CAL_CFRAC_3D

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE CAPTURE_ACM_CLOUD( JDATE, JTIME, COL, ROW, BOTTOM, TOP,
     &                                 FRACTION, WATER, ICE )

            USE CLOUD_OPTICS, ONLY : CLDMIN
         
            IMPLICIT NONE
! Purpose:
!   Save properties determined by ACM subgrid cloud at vertical column into
!   for radiation transfer calculations
!  REVISION  HISTORY:
!  12Jun15: BHutzell: comment out call to CLEAR_ACM_CLOUD because CONVCLD_ACM routine
!                     calls CLEAR_ACM_CLOUD at every synchronization time if acm 
!                     convection algorithm engaged, i.e., CONVCLD = .TRUE.
! Arguments:
            INTEGER, INTENT( IN )  :: JDATE      
            INTEGER, INTENT( IN )  :: JTIME                 
            INTEGER, INTENT( IN )  :: COL           ! array column index
            INTEGER, INTENT( IN )  :: ROW           ! array row index
            INTEGER, INTENT( IN )  :: TOP           ! vertical layer index
            INTEGER, INTENT( IN )  :: BOTTOM        ! vertical layer index
            REAL,    INTENT( IN )  :: FRACTION      ! cloud fraction of ACM cloud
            REAL,    INTENT( IN )  :: WATER( : )    ! liquid water droplet mixing ratio
            REAL,    INTENT( IN )  :: ICE( : )      ! ice partical mixing ratio

! Parameters:
!           None

! Local variables:
            INTEGER                :: LAY           ! loop index

            DO LAY = BOTTOM, TOP
               IF ( WATER( LAY ) + ICE( LAY ) .LT. CLDMIN ) CYCLE
               ACM_CFRAC( LAY,COL,ROW ) = FRACTION
               ACM_QC   ( LAY,COL,ROW ) = WATER( LAY )
               ACM_QI   ( LAY,COL,ROW ) =   ICE( LAY )
            END DO
                     
            DO LAY = 1, BOTTOM - 1
               IF ( WATER( LAY ) + ICE( LAY ) .LT. CLDMIN ) CYCLE
               ACM_CFRAC( LAY,COL,ROW ) = FRACTION
               ACM_QR   ( LAY,COL,ROW ) = WATER( LAY )
               ACM_QG   ( LAY,COL,ROW ) =   ICE( LAY )
            END DO

            ACM_CLOUDS( COL,ROW )  = FRACTION             
             
!           WRITE(LOGDEV,'(A,2(I3,1X),10(ES12.4,1X))')'COL,ROW,ACM_CFRAC = ',COL,ROW,
!     &     ACM_CFRAC( TOP,COL,ROW ), ACM_QC( TOP,COL,ROW ),ACM_QI( TOP,COL,ROW )            
            RETURN

         END SUBROUTINE CAPTURE_ACM_CLOUD

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE INIT_ACM_CLOUD( JDATE, JTIME )

            USE GRID_CONF           ! horizontal & vertical domain specifications
            USE UTILIO_DEFN         ! IO routines & utilties

!Arguments:
            INTEGER, INTENT( IN )  :: JDATE      
            INTEGER, INTENT( IN )  :: JTIME
! Local variables:             
            INTEGER                :: ASTAT     ! memory allocation status
             
            CHARACTER( 18 ), PARAMETER  :: PNAME = 'INIT_ACM_CLOUD'
                       
            WRITE( XMSG, '(3A)' ), 'ATTENTION: Attempt to include subgrid ',
     &             'cloud effects: photolysis rates include ACM CLOUD effects. ',
     &             'It would be prudent to examine diagnostic cloud fractions.'
            CALL LOG_MESSAGE( LOGDEV, XMSG)
            WRITE( LOGDEV, * )
             
            ALLOCATE ( ACM_CFRAC( NLAYS,NCOLS,NROWS ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ACM_CFRAC'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            ALLOCATE ( ACM_QC( NLAYS,NCOLS,NROWS ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ACM_QC'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            ALLOCATE ( ACM_QI( NLAYS,NCOLS,NROWS ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ACM_QI'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            ALLOCATE ( ACM_QR( NLAYS,NCOLS,NROWS ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ACM_QR'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            ALLOCATE ( ACM_QG( NLAYS,NCOLS,NROWS ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ACM_QG'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            ALLOCATE ( ACM_CLOUDS( NCOLS,NROWS ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ACM_CLOUDS'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
            ALLOCATE ( ACM_AVE_H2O( NCOLS,NROWS ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ACM_AVE_H2O'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
 
            ACM_CLOUDS  = 0.0
            ACM_CFRAC   = 0.0
            ACM_AVE_H2O = 0.0
            ACM_QC      = 0.0
            ACM_QI      = 0.0
            ACM_QR      = 0.0
            ACM_QG      = 0.0
             
            RETURN

         END SUBROUTINE INIT_ACM_CLOUD        

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         SUBROUTINE CLEAR_ACM_CLOUD( JDATE, JTIME )
          
            IMPLICIT NONE
! Purpose:
!          -Determine if acm cloud propeties have been set
!          -Zero out their array if not
! Arguments:
            INTEGER, INTENT( IN )  :: JDATE      
            INTEGER, INTENT( IN )  :: JTIME                 
! Local Variables: None

            IF ( SET_ACM_ARRAYS )THEN
               CALL INIT_ACM_CLOUD( JDATE, JTIME)
               SET_ACM_ARRAYS = .FALSE.
            END IF

            IF ( JDATE .NE. ACM_DATE .OR. JTIME .NE. ACM_TIME )THEN
               ACM_CFRAC   = 0.0   ! array assignment
               ACM_QC      = 0.0   !   "       "
               ACM_QI      = 0.0   !   "       "
               ACM_QR      = 0.0   !   "       "
               ACM_QG      = 0.0   !   "       "
               ACM_CLOUDS  = 0.0   !   "       "
               ACM_AVE_H2O = 0.0   !   "       "
               ACM_DATE    = JDATE
               ACM_TIME    = JTIME
            END IF

            RETURN

         END SUBROUTINE CLEAR_ACM_CLOUD
      END MODULE PHOT_MET_DATA
