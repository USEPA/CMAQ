!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in
!  continuous development by various groups and is based on information
!  from these groups: Federal Government employees, contractors working
!  within a United States Government contract, and non-Federal sources
!  including research institutions.  These groups give the Government
!  permission to use, prepare derivative works of, and distribute copies
!  of their work in the CMAQ system to the public and to permit others
!  to do so.  The United States Environmental Protection Agency
!  therefore grants similar permission to use the CMAQ system software,
!  but users are requested to provide copies of derivative works or
!  products designed to operate in the CMAQ system to the United States
!  Government without restrictions as to use by others.  Software
!  that is used with the CMAQ system but distributed under the GNU
!  General Public License or the GNU Lesser General Public License is
!  subject to their copyright restrictions.
!------------------------------------------------------------------------!
C Adopted from CMAQ files and modified for MEGAN3.1 by Ling Huang
C 2019/07/15
C Adopted from MEGAN3.1 and modified for CMAQ 5.4 by Jeff Willison

      MODULE BDSNP_MOD
 
      USE centralized_io_util_module
      USE HGRD_DEFN
      USE ASX_DATA_MOD, ONLY: MET_DATA,GRID_DATA
      USE RUNTIME_VARS, ONLY: NEW_START, PX_LSM, MGN_ONLN_DEP, IGNORE_SOILINP
      
      IMPLICIT NONE
      
      CONTAINS
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE HRNOBDSNP( JDATE, JTIME, TSTEP,MONTH,  
     &             L_DESID_DIAG, SOILM, SOILT, RSTYP,LAI,
     &                       BDSNP_NO )

  
      USE RUNTIME_VARS, ONLY: STTIME,STDATE,RUNLEN,LOGDEV
      USE PHOT_MOD,     ONLY: CFRAC_2D_AVE

      USE UTILIO_DEFN
#ifdef mpas
      use mio_module
      use util_module, only : TIME2SEC, secsdiff, nextime
#else
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif


      USE centralized_io_module, only: bdsnp_ndep,bdsnp_landtype,
     &                                 bdsnp_arid,bdsnp_nonarid,bdsnp_fert,
     &                                 soilmprev,ndepres,ndeprate,dryperiod,pfactor
     
      IMPLICIT NONE

C Arguments:

      INTEGER, INTENT( IN )  :: JDATE             ! current simulation date (YYYYDDD)
      INTEGER, INTENT( IN )  :: JTIME             ! current simulation time (HHMMSS)
      INTEGER, INTENT( IN )  :: MONTH             ! current simulation month

      INTEGER, INTENT( IN )  :: TSTEP( 3 )        ! time step vector (HHMMSS)
      LOGICAL, INTENT( IN ) :: L_DESID_DIAG
      INTEGER, INTENT( IN ) :: RSTYP  ( NCOLS,NROWS )        ! soil type
      REAL,    INTENT( IN ) :: SOILM ( NCOLS,NROWS )         ! soil moisture [m3/m3] (PX)
      REAL,    INTENT( IN ) :: SOILT  ( NCOLS,NROWS )        ! soil temperature [K] (PX)

      REAL,    INTENT( IN ) :: LAI  ( NCOLS,NROWS )        ! leaf area index (m2/m2)
      REAL,    INTENT( OUT ) :: BDSNP_NO ( NCOLS,NROWS ) ! output NO emissions in nanomol/m^2/s

C Parameters:
      ! Value calculated by running the 2x2.5 GEOS-Chem model
      REAL*8,  PARAMETER :: TAU_MONTHS   = 6. ! this is the decay time for dep. N reservoir, fert is 4 months
      REAL*8,  PARAMETER :: SECPERDAY    = 86400.d0
      REAL*8,  PARAMETER :: DAYSPERMONTH = 30.
      REAL*8,  PARAMETER :: TAU_SEC      = TAU_MONTHS * DAYSPERMONTH * SECPERDAY
      
      ! New soil biomes based on Steinkamp et al., 2011
      INTEGER, PARAMETER :: NSOIL    = 24

      ! Canopy wind extinction coefficients
      ! (cf. Yienger & Levy [1995], Sec 5), now a function of the MODIS/KOPPEN biometype (J.D. Maasakkers)
       REAL*8,  PARAMETER :: SOILEXC(NSOIL)    = (/ 
     &  0.10, 0.50, 0.10, 0.10, 0.10,
     &  0.10, 0.10, 0.10, 0.10, 1.00,
     &  1.00, 1.00, 1.00, 2.00, 4.00,
     &  4.00, 4.00, 4.00, 4.00, 4.00,
     &  4.00, 2.00, 0.10, 2.00                  /)
      ! Steinkamp and Lawrence, 2011 A values, wet biome coefficients
      ! for each of the 24 soil biomes [ng N/m2/s] 
      REAL*8,  PARAMETER :: A_BIOME(NSOIL)          =                (/ 
     &   0.00, 0.00, 0.00, 0.00, 0.00, 0.06, 0.09, 0.09, 0.01, 
     &   0.84, 0.84, 0.24, 0.42, 0.62, 0.03, 0.36, 0.36, 0.35, 
     &   1.66, 0.08, 0.44, 0.57, 0.57, 0.57     /)
        
C Saturation values from ASX. For PX see:
C Jacquemin B. and Noilhan J. (1990), Bound.-Layer Meteorol., 52, 93-134.

C Local Variables:

      CHARACTER( 16 ), SAVE :: SOILINSTATE = 'SOILINSTATE' ! logical name for input NO soil data, restart file
      CHARACTER( 16 ), SAVE :: SOILOUT = 'BDSNPOUT' ! logical name for output NO soil data - same format as soilinstate
      integer, save :: output_step, half_syn_step  ! values are in seconds
      CHARACTER (20) :: TIME_STAMP

C Land use files for BDSNP: both time independant in CMAQ sense and absolutely - e.g. fertilizer does not vary with year

      CHARACTER( 16 ) :: VAR        ! variable name

      REAL,    ALLOCATABLE, SAVE :: FERT     ( :,: )  ! "ng N m-2" already - reservoir
C Gridded Canopy NOx reduction factor for BDSNP Soil NO calculations
      REAL,    ALLOCATABLE, SAVE :: CRF   ( :,: )     ! 0-1
      REAL,    ALLOCATABLE, SAVE :: CFRAC ( :,: )  ! 0-1

C --- diagnostic variables, can be removed in final version      
      REAL,    ALLOCATABLE, SAVE :: CRFAVG   ( :,: )  ! 0-1
      REAL,    ALLOCATABLE, SAVE :: PULSEAVG   ( :,: )  ! 1+
      REAL,    ALLOCATABLE, SAVE :: BASESUM   ( :,: )  ! used in calculating the above two averages
      REAL,    ALLOCATABLE, SAVE :: THETA_DIAG( :,: )  ! diagnositc theta
      REAL,    ALLOCATABLE, SAVE :: WET_DIAG ( :,: )  ! diagnositc wet term
      REAL,    ALLOCATABLE, SAVE :: TEMP_DIAG ( :,: )  ! diagnositc temp term
      REAL,    ALLOCATABLE, SAVE :: A_DIAG ( :,: )  ! diagnositc biome base emissions term
      REAL,    ALLOCATABLE, SAVE :: AFERT_DIAG ( :,: )  ! diagnositc fert emissions term
      REAL,    ALLOCATABLE, SAVE :: NRES_FERT_DIAG ( :,: )  ! diagnositc nres fert
      REAL,    ALLOCATABLE, SAVE :: NRES_DEP_DIAG ( :,: )  ! diagnositc nres  dep    
C ---------------------------------------------------------------------------      

      REAL,                 SAVE :: EMPOL,EMPOLSUM, EMPOLAVG       ! use to check reasonableness of results, g/hr
      REAL,                 SAVE :: TIMECHECK ! use to output CPU_TIME(TIMECHECK) to see if this section of code is running unreasonably long
      
      
      INTEGER, SAVE :: EDATE     ! end scenario date
      INTEGER, SAVE :: ETIME     ! end scenario time
      INTEGER, SAVE :: NDATE     ! test date to update rainfall
      INTEGER, SAVE :: NTIME     ! test time to update rainfall
      INTEGER, SAVE :: SDATE     ! scenario start date
      INTEGER, SAVE :: STIME     ! scenario start time
        
      LOGICAL, SAVE :: PX_VERSION         ! true: use PX version of MCIP; should always be true

      INTEGER          SOILCAT            ! soil category
      INTEGER          NSTEPS             ! run duration (HHMMSS)
      INTEGER, SAVE :: MSTEPS             ! run no. of steps
      INTEGER          I, J, K, R, C, L   ! counters
      CHARACTER*3      CHARDAY
      CHARACTER*2      CHARMON
      LOGICAL          OK
      INTEGER          IOS                ! IO or memory allocation status
      
C      REAL,    SAVE :: EFAC
       REAL            TEMP_TERM
       REAL            WET_TERM
       REAL            PULSE
       REAL            A_FERT
       REAL            CRF_TERM
       REAL            SOILNOX, FERTDIAG
C      REAL             CFNO               ! NO correction factor
C      REAL             CFNOGRASS          ! NO correction factor for grasslands
C      REAL             TAIR               ! surface temperature
C      REAL             TSOI               ! soil temperature
       REAL             THETA              ! water filled pore space
       REAL             THETAPREV

C      REAL             CFNOWET,  THETA
C      REAL             FAC1, FAC2, FAC3, FAC4

      LOGICAL, SAVE :: USE_SOILT = .TRUE. ! use soil temperature in PX version
                                          ! rather than estimate as in BEIS2

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      CHARACTER( 256 ) :: MESG            ! message buffer
      CHARACTER( 16 )  :: PNAME = 'BDSNPHRNO'  ! procedure name

C-----------------------------------------------------------------------
      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.
         ! we need to initialize and allocate:
         ! pulse
         ! length of dry period
         ! soil moisture of previous time step
         ! N reservoir, deposition only
         ! These values can be provided from a restart file. The restart file is 'timeless'.
         ! This means CMAQ isn't checking to see if the restart file is actually from
         ! the immediately prior timstep.

C Determine last timestamp
         EDATE = STDATE; ETIME = STTIME
         CALL NEXTIME( EDATE, ETIME, RUNLEN )   ! end date & time

C     ! make sure it only runs with PX version
         IF( .NOT. PX_LSM ) THEN
            MESG = "BDSNP Soil NO is only compatible with PX version"
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2)
         END IF

C Allocate memory for data and read
#ifdef mpas
         output_step   = time2sec(tstep(1))
         half_syn_step = time2sec(tstep(2)) / 2
#endif

         ALLOCATE( FERT( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'FERT', PNAME )

         ALLOCATE( CFRAC( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'CFRAC', PNAME )

         ALLOCATE( CRF( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'CRF', PNAME )

         ALLOCATE( CRFAVG( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'CRFAVG', PNAME )

         ALLOCATE( PULSEAVG( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'PULSEAVG', PNAME )

         ALLOCATE( BASESUM( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'BASESUM', PNAME )


C ------ Diagnostics -----------------------------------
         ALLOCATE( THETA_DIAG( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'THETA_DIAG', PNAME )

         ALLOCATE( WET_DIAG( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'WET_DIAG', PNAME )

         ALLOCATE( TEMP_DIAG( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'TEMP_DIAG', PNAME )

         ALLOCATE( A_DIAG( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'A_DIAG(', PNAME )

         ALLOCATE( AFERT_DIAG( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'AFERT_DIAG', PNAME )

         ALLOCATE( NRES_FERT_DIAG( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'NRES_FERT_DIAG', PNAME )

         ALLOCATE( NRES_DEP_DIAG( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'NRES_DEP_DIAG', PNAME )

C-----------------------------------------------------------------------------
C Initial run if the model hasnt been run before, otherwise use a restart file
C to determine DRYPERIOD, pulse state, prev. timestep soil moisture, and N reservoir.

        
C If initial run, initialize some variables, otherwise get them from file
         IF ( NEW_START .or. IGNORE_SOILINP) THEN

            PFACTOR   = 1d0   ! array
            DRYPERIOD = 0.01  ! array initialized non-zero to avoid log(0)
            CFRAC     = 0. 
            SOILMPREV = 0d0   ! array
            FERT      = 0d0   ! array
            EMPOL  = 0d0
            EMPOLSUM  = 0d0
            EMPOLAVG  = 0d0
            BASESUM = 0.0
            CRFAVG = 0.0
            PULSEAVG = 0.0
            NDEPRES   = 0d0   ! array

C open nitrogen deposition file

           if (.not. MGN_ONLN_DEP) THEN 
             ndeprate = bdsnp_ndep(:,:,month)  ! optional for using offline NDEP
                                               !         instead of online NDEP
            ! When using online deposition there is no attempt to shorten the
            ! spin up time so we do not initialize this.
           end if
                                        
            !attempt to use steady state condition to reduce spin up time by setting dN/dt = 0
            !or NDEPRES = Dep rate * tau, the decay time
            DO R = 1, NROWS
                DO C = 1, NCOLS
                NDEPRES( C,R ) = NDEPRATE( C,R )*TAU_SEC
C           check for negatives
          IF( NDEPRES(C,R) .lt. 0.0 ) THEN
            Write(MESG,*) 'NDEPRES negative', NDEPRES, ' ',NDEPRATE(C,R)
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
          ELSE IF( NDEPRATE(C,R) .lt. 0.0 ) THEN
            Write(MESG,*) 'NDEPRATE negative', NDEPRES,' ',NDEPRATE(C,R)
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
          END IF
              END DO
           END DO

        ELSE ! SOILINSTATE file available for NDEPRES
            BASESUM = 0.0
            CRFAVG = 0.0
            PULSEAVG = 0.0
            EMPOLSUM  = 0d0
           if (.not. MGN_ONLN_DEP) THEN 
             ndeprate = bdsnp_ndep(:,:,month)  ! For using offline NDEP
                                               ! instead of online NDEP
           end if

        END IF  ! initial run check

      END IF ! FIRSTIME
#ifdef mpas
      IF ( MOD((TIME2SEC( JTIME ) - half_syn_step), output_step) .EQ. 0 .and. .not. l_desid_diag) GOTO 9999
#else
      IF (SECSDIFF(JDATE,JTIME,EDATE,ETIME) .LE. TIME2SEC(TSTEP(2)) .and. .not. L_DESID_DIAG) GOTO 9999
#endif
    
#ifdef twoway
       ! CFRAC for offline comes from MCIP but that's unavailable in twoway mode
       if (.not. allocated(CFRAC_2D_AVE)) then
         CFRAC = Met_Data%CFRAC
!       write(6,'(a,es14.2)')'MaxVal cfrac = ',MAXVAL(cfrac)
!       write(logdev,'(a,es14.2)')'MaxVal cfrac = ',MAXVAL(cfrac)
       else
         CFRAC = CFRAC_2D_AVE
       end if
#else
       CFRAC = Met_Data%CFRAC
#endif 

   
C    read day dependant fertilizer reservoir e.g. Potter et al 2010
C get the days fertilizer
        fert = bdsnp_fert

C Fertilizer N reservoir already calculated and read from file, update deposition reservoir from dep rate
      DO R = 1, NROWS
        DO C = 1, NCOLS
            CALL GET_NDEPRES( TSTEP, NDEPRES( C,R ),
     &                                            TAU_SEC, C, R,L_DESID_DIAG)
        END DO
      END DO


C Calculate temporal non-speciated soil NO emissions to EMPOL

C     If False Dont do any calculations to test - replicate 0 output
      IF( .TRUE. ) THEN

      CALL GET_CANOPY_NOX(JDATE, JTIME, Met_Data%COSZEN,
     & MET_DATA%TEMP2, MET_DATA%RGRND, met_data%PRSFC, 
     & BDSNP_LANDTYPE, LAI, Met_Data%SNOCOV, CFRAC, Met_Data%WSPD10, CRF)

      DO R = 1, NROWS
         DO C = 1, NCOLS

            SOILNOX  = 0d0
            FERTDIAG = 0d0

            K = BDSNP_LANDTYPE( C,R ) !Skip LANDTYPE not present
            ! Temperature-dependent term of soil NOx emissions
            ! [unitless]
            ! Uses PX soil temperature instead of inferring from air
            ! temperature
            TEMP_TERM = SOILTEMP( SOILT(C,R) )

            ! Use THETA instead of boolean wet/dry climate
            SOILCAT = INT( RSTYP( C,R ) )
            IF ( SOILCAT .NE. 14) THEN !not water
               THETA =  SOILM( C,R ) / Grid_Data%WSAT( C,R )
               THETAPREV = SOILMPREV( C,R ) / Grid_Data%WSAT( C,R )
               ! Soil moisture scaling of soil NOx emissions
               WET_TERM = SOILWET( THETA , BDSNP_ARID( C,R ), BDSNP_NONARID( C,R ))
            ELSE
               WET_TERM = 0d0
               THETA = 0d0
               THETAPREV = 0d0
            END IF
            ! Cumulative multiplication factor (over baseline emissions)
            ! that accounts for soil pulsing
C            PFACTOR( C,R ) = PULSING( THETA, TSTEP, THETAPREV,
C     &                       PFACTOR( C,R ), DRYPERIOD( C,R ) )
            PULSE = PULSING( L_DESID_DIAG, THETA, TSTEP, THETAPREV,
     &                       PFACTOR( C,R ), DRYPERIOD( C,R ) )

            A_FERT = FERTADD( FERT( C,R ) , NDEPRES( C,R ) ) !adds reservoirs returns emission rates

            ! Canopy reduction factor
            CRF_TERM  = CRF( C,R )
C                  CRF_TERM  = SOILCRF( K, LAI,
C     &                        R_CANOPY(K),
C     &                               WINDSQR, SUNCOS )

         !  SOILNOX includes fertilizer
            SOILNOX   = ( A_BIOME(K) + A_FERT )  !don't forget to check parenthesis when uncommenting
     &             * ( TEMP_TERM * WET_TERM * PULSE )
     &             * ( 1.d0 - CRF_TERM  )

         !  FERTDIAG, only used for the fertilizer diagnostic, note
         !  includes DEP
         !  not actually used for anything at the moment, only
         !  diagnostics
            FERTDIAG  = ( A_FERT )
     &             * ( TEMP_TERM * WET_TERM * PULSE )
     &             * ( 1.d0 - CRF_TERM  )

C            END IF !LANDTYPE check

C            ENDDO

           !scale emissions
           EMPOL = SOILNOX *  3600.0 * 1.0e-9 ! [ng N/m2/s] *  s/hr * g/ng
           BDSNP_NO(C,R) = SOILNOX / 14 ! [nmol/m2/s]
           ! sum various quantities for daily averaging
           EMPOLSUM = EMPOLSUM + EMPOL
           BASESUM(C,R) = BASESUM(C,R) + ( A_BIOME(K) + A_FERT ) 
     &             * ( TEMP_TERM * WET_TERM)
     &             *  3600.0 * 1.0e-9 ! [ng N/m2/s] *  s/hr * g/ng
           PULSEAVG(C,R) = PULSEAVG(C,R) + ( A_BIOME(K) + A_FERT ) 
     &             * ( TEMP_TERM * WET_TERM * PULSE )
     &             *  3600.0 * 1.0e-9 ! [ng N/m2/s] *  s/hr * g/ng
           CRFAVG(C,R) = CRFAVG(C,R) + ( A_BIOME(K) + A_FERT ) 
     &             * ( TEMP_TERM * WET_TERM )
     &             * ( 1.d0 - CRF_TERM  )
     &             *  3600.0 * 1.0e-9 ! [ng N/m2/s] *  s/hr * g/ng


C--------- MORE DIAGNOSTICS  ---------------------------------
           A_DIAG( C,R ) = A_BIOME(K)
           AFERT_DIAG( C,R ) = A_FERT
           NRES_FERT_DIAG( C,R ) = FERT( C,R )
           NRES_DEP_DIAG( C,R )  = NDEPRES( C,R )
           WET_DIAG( C,R ) = WET_TERM
           THETA_DIAG( C,R ) = THETA
           TEMP_DIAG( C,R ) = TEMP_TERM

C -----------------------------------------------------
        END DO ! columns
      END DO ! rows
#ifdef mpas
        IF ( MOD( NTIME, 10000 ) .EQ. 0 ) THEN    ! at the hourly mark

             call mio_time_format_conversion (jdate, jtime, time_stamp)

             call mio_fwrite('BDSNP_DIAG','A_DIAG','BDSNP_MOD',
     &                             a_diag(:,1),time_stamp)
             call mio_fwrite('BDSNP_DIAG','AFERT_DIAG','BDSNP_MOD',
     &                             afert_diag(:,1),time_stamp)
             call mio_fwrite('BDSNP_DIAG','NRES_FERT_DIAG','BDSNP_MOD',
     &                             nres_fert_diag(:,1),time_stamp)
             call mio_fwrite('BDSNP_DIAG','NRES_DEP_DIAG','BDSNP_MOD',
     &                             nres_dep_diag(:,1),time_stamp)
             call mio_fwrite('BDSNP_DIAG','WET_DIAG','BDSNP_MOD',
     &                             wet_diag(:,1),time_stamp)
             call mio_fwrite('BDSNP_DIAG','TEMP_DIAG','BDSNP_MOD',
     &                             temp_diag(:,1),time_stamp)
       ENDIF
#endif

      ELSE ! add things until it dies
      WRITE( MESG,*) 'BDSNP testing terms'
      
      !!CALL M3MESG(MESG)
         DO R = 1, NROWS
         DO C = 1, NCOLS
            K = BDSNP_LANDTYPE( C,R ) !Skip LANDTYPE not present
            ! Temperature-dependent term of soil NOx emissions
            ! [unitless]
            ! Uses PX soil temperature instead of inferring from air
            ! temperature
            TEMP_TERM = SOILTEMP( SOILT(C,R) )
            ! Use THETA instead of boolean wet/dry climate
            SOILCAT = INT( RSTYP( C,R ) )
            IF ( SOILCAT .NE. 14) THEN !not water
               THETA = SOILM( C,R ) / Grid_Data%WSAT(C,R)
               THETAPREV =  SOILMPREV( C,R ) / Grid_Data%WSAT(C,R)
               ! Soil moisture scaling of soil NOx emissions
               WET_TERM = SOILWET( THETA , BDSNP_ARID( C,R ), BDSNP_NONARID( C,R ))
            ELSE
               WET_TERM = 0d0
               THETA = 0d0
               THETAPREV = 0d0
            END IF
            ! Cumulative multiplication factor (over baseline emissions)
            ! that accounts for soil pulsing
C            PFACTOR( C,R ) = PULSING( THETA, TSTEP, THETAPREV,
C     &                       PFACTOR( C,R ), DRYPERIOD( C,R ) )
            PULSE = PULSING( L_DESID_DIAG, THETA, TSTEP, THETAPREV,
     &                       PFACTOR( C,R ), DRYPERIOD( C,R ) )

         END DO
         END DO
      END IF ! end do nothing test if

      SOILMPREV = SOILM !save soilM array to soilMprev for next time step
      MESG = 'BDSNP calculated emissions'
      !CALL M3MESG(MESG)
      CALL CPU_TIME(TIMECHECK)
      WRITE(MESG,*)  'PROCESS TOOK:', TIMECHECK, 'SECONDS'
      !CALL M3MESG(MESG)
      EMPOLAVG = EMPOLSUM/FLOAT(NCOLS*NROWS)
      WRITE( MESG,*) 'average value:', EMPOLAVG
      !CALL M3MESG(MESG)
      EMPOLSUM = 0d0 !array
  
      RETURN
!      IF ( SECSDIFF( JDATE,JTIME, EDATE,ETIME ) .GT. TIME2SEC( TSTEP( 2 ) ) .OR. L_DESID_DIAG) RETURN
9999   CONTINUE


C Create soil NO state save file at the end of the run for restart purposes

C Final timestamp
      NDATE = EDATE; NTIME = ETIME

!       Avoid divide by zero over water where BASESUM = 0
             WHERE ( BASESUM .eq. 0) BASESUM = 1.0  
             CRFAVG   = CRFAVG/BASESUM
             PULSEAVG = PULSEAVG/BASESUM
             WHERE ( CRFAVG   .gt. 100 ) CRFAVG   = 0.0  
             WHERE ( PULSEAVG .gt. 100 ) PULSEAVG = 0.0  

#ifdef mpas
             call julian_to_mpas_date_time (jdate, jtime, time_stamp)

             call mio_fwrite('BDSNPOUT','PFACTOR', pname,
     &                             PFACTOR(:,1),time_stamp)
             call mio_fwrite('BDSNPOUT','DRYPERIOD', pname,
     &                             DRYPERIOD(:,1),time_stamp)
             call mio_fwrite('BDSNPOUT','NDEPRES', pname,
     &                             NDEPRES(:,1),time_stamp)
             call mio_fwrite('BDSNPOUT','SOILMPREV', pname,
     &                             SOILMPREV(:,1),time_stamp)
             call mio_fwrite('BDSNPOUT','NDEPRATE_DIAG', pname,
     &                           NDEPRATE(:,1),time_stamp)
#else
!C Build description for, and create/open soil NO emissions output file

      FTYPE3D = GRDDED3
      SDATE3D = NDATE
      STIME3D = NTIME
      TSTEP3D = 0   ! make it a time-independent file
      NCOLS3D = GL_NCOLS
      NROWS3D = GL_NROWS
      NLAYS3D = 1
      NVARS3D = 14
      MXREC3D = 1
!
      VNAME3D = ' '
      VNAME3D( 1 ) = 'PFACTOR'
      VNAME3D( 2 ) = 'DRYPERIOD'
      VNAME3D( 3 ) = 'NDEPRES'
      VNAME3D( 4 ) = 'SOILMPREV'

!C --- DIAGNOSTICS ---------------------------
      VNAME3D( 5 ) = 'THETA_DIAG'
      VNAME3D( 6 ) = 'WET_TERM_DIAG'
      VNAME3D( 7 ) = 'TEMP_DIAG'
      VNAME3D( 8 ) = 'TEMP_TERM_DIAG'
      VNAME3D( 9 ) = 'A_DIAG'
      VNAME3D( 10 ) = 'NRES_FERT_DIAG'
      VNAME3D( 11 ) = 'AFERT_DIAG'
      VNAME3D( 12 ) = 'NDEPRATE_DIAG'
      VNAME3D( 13 ) = 'CRFAVG'
      VNAME3D( 14 ) = 'PULSEAVG'
!c -------------------------------------------
      UNITS3D = ' '
      UNITS3D( 1 ) = 'REAL'
      UNITS3D( 2 ) = 'REAL'
      UNITS3D( 3 ) = 'REAL'
      UNITS3D( 4 ) = 'REAL'
      UNITS3D( 5 ) = 'REAL'
      UNITS3D( 6 ) = 'REAL'
      UNITS3D( 7 ) = 'REAL'
      UNITS3D( 8 ) = 'REAL'
      UNITS3D( 9 ) = 'REAL'
      UNITS3D( 10 ) = 'REAL'
      UNITS3D( 11 ) = 'REAL'
      UNITS3D( 12 ) = 'REAL'
      UNITS3D( 13 ) = 'REAL'
      UNITS3D( 14 ) = 'REAL'


      VDESC3D( 1 ) = 'NO emission current pulse factor'
      VDESC3D( 2 ) = 'length of the dry period in hours'
      VDESC3D( 3 ) = 'soil N reservoir from deposition'
      VDESC3D( 4 ) = 'Soil moisture prev. timestep m3/m3'
      VDESC3D( 5 ) = 'moisture WFPS 0-1'
      VDESC3D( 6 ) = 'moisture scale factore diagnostic'
      VDESC3D( 7 ) = 'temperature diagnostic'
      VDESC3D( 8 ) = 'temperature scale factor diagnostic'
      VDESC3D( 9 ) = 'biome base emission diagnostic'
      VDESC3D( 10 ) = 'NRES fert only diagnostic'
      VDESC3D( 11 ) = 'fertilizer emission factor diagnostic'
      VDESC3D( 12 ) = 'N deposition rate diagnostic'
      VDESC3D( 13 ) = 'canopy reduction factor diagnostic'
      VDESC3D( 14 ) = 'pulse factor diagnostic'

      VTYPE3D = 0
      VTYPE3D( 1:14 ) = M3REAL

      FDESC3D = ' '
      FDESC3D( 1 ) = 'Gridded soil state data for soil NO emissions'
      FDESC3D( 2 ) = '/From/ ' // PNAME
      FDESC3D( 3 ) = '/Version/ MEGAN3.1'
!

C Open NO rain data save file
      IF ( IO_PE_INCLUSIVE ) THEN
         IF ( .NOT. OPEN3( SOILOUT, FSNEW3, PNAME ) ) THEN
            MESG = 'Could not open "' // TRIM( SOILOUT ) // '" file'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT1 )
         END IF
      END IF

#ifdef parallel_io
      IF ( IO_PE_INCLUSIVE ) THEN
         IF ( .NOT. FLUSH3 ( SOILOUT ) ) THEN
            MESG = 'Could not sync to disk ' // TRIM( SOILOUT )
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF
      END IF
      CALL SE_BARRIER
      IF ( .NOT. IO_PE_INCLUSIVE ) THEN
         IF ( .NOT. OPEN3( SOILOUT, FSREAD3, PNAME ) ) THEN
            MESG = 'Could not open ' // TRIM( SOILOUT )
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF
      END IF
#endif

      IF ( .NOT. WRITE3(SOILOUT,'PFACTOR',NDATE,NTIME,PFACTOR)) THEN
        CALL NAMEVAL (SOILOUT, MESG)  ! get input file name and path
        MESG = 'Error writing to file: '//TRIM(MESG)
        CALL M3EXIT(PNAME,JDATE,JTIME,MESG,XSTAT2)
      ENDIF
!
      IF ( .NOT. WRITE3(SOILOUT,'DRYPERIOD',NDATE,NTIME,DRYPERIOD )) THEN
        CALL NAMEVAL (SOILOUT, MESG)  ! get input file name and path
        MESG = 'Error writing to file: '//TRIM(MESG)
        CALL M3EXIT(PNAME,JDATE,JTIME,MESG,XSTAT2)
      ENDIF

      IF ( .NOT. WRITE3(SOILOUT,'NDEPRES',NDATE,NTIME,NDEPRES )) THEN
        CALL NAMEVAL (SOILOUT, MESG)  ! get input file name and path
        MESG = 'Error writing to file: '//TRIM(MESG)
        CALL M3EXIT(PNAME,JDATE,JTIME,MESG,XSTAT2)
      ENDIF

      IF ( .NOT. WRITE3(SOILOUT,'SOILMPREV',NDATE,NTIME,SOILM )) THEN
        CALL NAMEVAL (SOILOUT, MESG)  ! get input file name and path
        MESG = 'Error writing to file: '//TRIM(MESG)
        CALL M3EXIT(PNAME,JDATE,JTIME,MESG,XSTAT2)
      ENDIF
!
!C ---- DIAGNOSTICS -------------------------------------------------------------------
      IF ( .NOT. WRITE3(SOILOUT,'THETA_DIAG',NDATE,NTIME,THETA_DIAG )) THEN ! diagnostic theta
        CALL NAMEVAL (SOILOUT, MESG)  ! get input file name and path
        MESG = 'Error writing THETA_DIAG to file: '//TRIM(MESG)
        CALL M3EXIT(PNAME,JDATE,JTIME,MESG,XSTAT2)
      ENDIF

      IF ( .NOT. WRITE3(SOILOUT,'WET_TERM_DIAG',NDATE,NTIME,WET_DIAG )) THEN ! diagnostic theta
!        CALL NAMEVAL (SOILOUT, MESG)  ! get input file name and path
         MESG = 'Could not write "' // TRIM( 'WET_TERM_DIAG' ) //
     &          '" to file "' // TRIM( SOILOUT ) // '"'
         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
      ENDIF


      IF ( .NOT. WRITE3(SOILOUT,'TEMP_DIAG',NDATE,NTIME,SOILT)) THEN ! diagnostic theta
        CALL NAMEVAL (SOILOUT, MESG)  ! get input file name and path
        MESG = 'Error writing TEMP_DIAG to file: '//TRIM(MESG)
        CALL M3EXIT(PNAME,JDATE,JTIME,MESG,XSTAT2)
      ENDIF

      IF ( .NOT.WRITE3(SOILOUT,'TEMP_TERM_DIAG',NDATE,NTIME,TEMP_DIAG)) THEN ! diagnostic theta
        CALL NAMEVAL (SOILOUT, MESG)  ! get input file name and path
        MESG = 'Error writing TEMP_TERM_DIAG to file: '//TRIM(MESG)
        CALL M3EXIT(PNAME,JDATE,JTIME,MESG,XSTAT2)
      ENDIF

      IF ( .NOT.WRITE3(SOILOUT,'A_DIAG',NDATE,NTIME,A_DIAG)) THEN ! diagnostic theta
        CALL NAMEVAL (SOILOUT, MESG)  ! get input file name and path
        MESG = 'Error writing A_DIAG to file: '//TRIM(MESG)
        CALL M3EXIT(PNAME,JDATE,JTIME,MESG,XSTAT2)
      ENDIF

      IF ( .NOT.WRITE3(SOILOUT,'AFERT_DIAG',NDATE,NTIME,AFERT_DIAG)) THEN ! diagnostic theta
        CALL NAMEVAL (SOILOUT, MESG)  ! get input file name and path
        MESG = 'Error writing AFERT_DIAG to file: '//TRIM(MESG)
        CALL M3EXIT(PNAME,JDATE,JTIME,MESG,XSTAT2)
      ENDIF

      IF ( .NOT.WRITE3(SOILOUT,'NRES_FERT_DIAG',NDATE,NTIME,
     &                                  NRES_FERT_DIAG)) THEN ! diagnostic theta
        CALL NAMEVAL (SOILOUT, MESG)  ! get input file name and path
        MESG = 'Error writing NRES_FERT_DIAG to file: '//TRIM(MESG)
        CALL M3EXIT(PNAME,JDATE,JTIME,MESG,XSTAT2)
      ENDIF

      IF ( .NOT.WRITE3(SOILOUT,'NDEPRATE_DIAG',NDATE,NTIME,
     &                                  NDEPRATE)) THEN !diagnostic theta
        CALL NAMEVAL (SOILOUT, MESG)  ! get input file name and path
        MESG = 'Error writing NDEPRATE_DIAG to file: '//TRIM(MESG)
        CALL M3EXIT(PNAME,JDATE,JTIME,MESG,XSTAT2)
      ENDIF

      IF ( .NOT.WRITE3(SOILOUT,'CRFAVG',NDATE,NTIME,
     &                                  CRFAVG)) THEN !diagnostic theta
        CALL NAMEVAL (SOILOUT, MESG)  ! get input file name and path
        MESG = 'Error writing CRFAVG to file: '//TRIM(MESG)
        CALL M3EXIT(PNAME,JDATE,JTIME,MESG,XSTAT2)
      ENDIF

      IF ( .NOT.WRITE3(SOILOUT,'PULSEAVG',NDATE,NTIME,
     &                                  PULSEAVG)) THEN !diagnostic theta
        CALL NAMEVAL (SOILOUT, MESG)  ! get input file name and path
        MESG = 'Error writing PULSEAVG to file: '//TRIM(MESG)
        CALL M3EXIT(PNAME,JDATE,JTIME,MESG,XSTAT2)
      ENDIF
#endif


C ------------------------------------------------------------------------------------
      WRITE( MESG,* )
     &      'Timestep written to', SOILOUT,
     &      'for date and time', NDATE, NTIME

      RETURN

94040 FORMAT( 5X, 3( A, :, 1X ), I8, ":", I6.6)

      END SUBROUTINE HRNOBDSNP
C----------------------------------------------------------------------------------

         REAL FUNCTION PULSING( L_DESID_DIAG, THETA, TSTEP, THETAPREV,
     &                       PFACTOR, DRYPERIOD )!_____
! !DESCRIPTION: Function PULSING calculates the increase (or "pulse") of
!  soil NOx emission that happens after preciptiation falls on dry soil.
!\\
!\\
!  According to  Yan et al., [2005] , this pulsing process is thought to
!  be due to a release of inorganic nitrogen trapped on top of the dry soil
!  and a subsequent reactivation of water-stressed bacteria, which then
!  metabolize the excess nitrogen. This can happen in seasonally dry
!  grasslands and savannahs or over freshly fertilized fields.
!  Soil NOx emissions consist of baseline emissions plus discrete "pulsing"
!  episodes.  We follow the Yan et al., [2005] algorithm, where the pulse
!  (relative to the flux pre wetting) is determined by the antecedent dry
!  period, with a simple logarithmic relationship,
!
!  PFACTOR = 13.01 ln ( DRYPERIOD ) -  53.6
!
!  ,where PFACTOR is the magnitude of peak flux relative to prewetting flux,
!  and DRYPERIOD  is the length of the antecedent dry period in hours.
!
!  The pulse decays with
!
!  PFACTOR = PFACTOR * EXP( -0.068d0 * TSTEP(HOURS) )

#ifdef mpas
      use util_module, only : TIME2SEC
#endif

         IMPLICIT NONE

#ifndef mpas
         INTEGER, EXTERNAL       ::   TIME2SEC
#endif

C Function arguments:
         INTEGER, INTENT( IN )    :: TSTEP( 3 )        ! time step vector (HHMMSS)
         LOGICAL, INTENT( IN )    :: L_DESID_DIAG
         REAL,    INTENT( IN )    :: THETA, THETAPREV  ! only avilable if PX version
         REAL,    INTENT( INOUT ) :: DRYPERIOD
         REAL,    INTENT( INOUT ) :: PFACTOR
C Local Variables
         REAL MOISTDIFF
         REAL DTHOURS
         DTHOURS = TIME2SEC(TSTEP(2))/3600.0
         IF (L_DESID_DIAG) then 
            DTHOURS = 0.
         END IF
         ! If soil moisture less than 0.3 and no pulse is taking place
         IF ( THETA < 0.3D0 .and. PFACTOR == 1.D0) THEN

            ! Get change in soil moisture since previous timestep
            MOISTDIFF = ( THETA - THETAPREV )

            ! If change in soil moisture is > 0.01 (rains)
            IF ( MOISTDIFF > 0.01 ) THEN

               !Initialize new pulse factor (dry period hours)
               PFACTOR = 13.01 * LOG( DRYPERIOD ) - 53.6

               ! If dry period < ~3 days then no pulse
               IF ( PFACTOR < 1.0 ) PFACTOR = 1.0

                  ! Reinitialize dry period
                  DRYPERIOD = 0.001

                ! If no rain (i.e.,  change in soil moisture is < 0.01)
               ELSE
                ! Add one timestep to dry period
                DRYPERIOD = DRYPERIOD + DTHOURS

            ENDIF

         ! If box is already pulsing , then decay pulse one timestep
         ELSEIF ( PFACTOR /= 1.d0) THEN

            ! Decay pulse
            PFACTOR   = PFACTOR * EXP( -0.068d0 * DTHOURS )

            ! Update dry period
            IF ( THETA < 0.3D0 ) DRYPERIOD = DRYPERIOD + DTHOURS

            ! If end of pulse
            IF ( PFACTOR < 1.d0 ) PFACTOR = 1.d0

         ENDIF
         PULSING = PFACTOR
         RETURN

         END FUNCTION PULSING!_____

C---------------------------------------------------------------------------------------------
      
         SUBROUTINE GET_NDEPRES( TSTEP, NDEPRES, TAU_SEC,C,R,L_DESID_DIAG)
C Get the deposition rate of the appropriate species for the appropriate timestep, add to reservoir and decay.
C Return reservoir amount.



         USE centralized_io_module, ONLY: ndeprate
         USE RUNTIME_VARS, ONLY:  MGN_ONLN_DEP
#ifdef mpas
         USE utilio_defn
         use util_module, only : TIME2SEC
#endif

         IMPLICIT NONE

#ifndef mpas
         INTEGER, EXTERNAL       ::   TIME2SEC
#endif

C Function arguments:

         INTEGER, INTENT( IN )  :: TSTEP( 3 )        ! time step vector (HHMMSS)
         INTEGER, INTENT( IN )  :: C
         INTEGER, INTENT( IN )  :: R
         REAL*8,  INTENT( IN )  :: TAU_SEC
         LOGICAL, INTENT( IN ) :: L_DESID_DIAG
         REAL,    INTENT( INOUT ) :: NDEPRES

C Local Variables
         CHARACTER( 256 ) :: MESG            ! message buffer
         CHARACTER( 16 )  :: PNAME = 'GET_NDEPRES'  ! procedure name

         REAL*8  :: C1 ! a factor
         REAL*8  :: C2  ! another one
         REAL*8  :: TS_SEC ! time step in seconds
         real NDEPTEMP
C           check for negatives
          IF( NDEPRES < 0.0 ) THEN
          WRITE(MESG,*) 'NDEPRES negative'
          Write(*,*) 'In GET_NDEPRES:'
          Write(*,*) 'NDEPRES negative', NDEPRES,' '
          write(*,*) 'TS, TAU, C1, C2:', TS_SEC, TAU_SEC,C1,C2
          CALL M3EXIT( PNAME, 0, 0, MESG, 2)
          ELSE IF( NDEPRATE(c,r) < 0.0 ) THEN
          MESG = 'NDEPRATE negative'
          Write(*,*) 'In GET_NDEPRES:'
          Write(*,*) 'NDEPRATE negative', NDEPRATE(c,r)
          CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
          END IF

         ! takes the NDEPRATE and uses it to update NDEPRES before
         ! clearing it.

         !Do mass balance (see Intro to Atm Chem Chap. 3)
         !m(t) = m(0) * exp(-t/tau) + Source * tau * (1 - exp(-t/tau))
         TS_SEC = TIME2SEC(TSTEP(2))
         C1 = EXP( - TS_SEC / TAU_SEC)
         C2 = 1.d0 - C1
               NDEPTEMP = NDEPRES
               NDEPRES = NDEPRES*C1+NDEPRATE(c,r)*TAU_SEC*C2
C           check for negatives
          IF( NDEPRES < 0.0 ) THEN
          MESG = 'negative'
          Write(*,*) 'In GET_NDEPRES:'
          Write(*,*) 'NDEPRES negative', NDEPRES
          write(*,*) 'TS, TAU, C1, C2:', TS_SEC, TAU_SEC,C1,C2
          CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
          END IF
         ! clear NDEPRATE for use during next time step
          
         IF (.not. L_DESID_DIAG .and. MGN_ONLN_DEP) THEN 
                                      ! need this not to be zero'd 
                                      ! out on last time step
                                      ! and don't want it zero'd if using
                                      ! offline values
          NDEPRATE(c,r) = 0.0 
         END IF
          

         RETURN

         END SUBROUTINE GET_NDEPRES

C -----------------------------------------------------------------------------

        SUBROUTINE GET_N_DEP( SPEC,DEP,C,R )
            USE UTILIO_DEFN       
            USE centralized_io_module, only: ndeprate

            IMPLICIT NONE
            CHARACTER( 256 ) :: MESG            ! message buffer
            CHARACTER( 8 ), INTENT( IN ) :: SPEC  !  dep species
            REAL,      INTENT( IN ) :: DEP !  deposition rate in kg/ha/s 
            INTEGER,   INTENT( IN ) :: C
            INTEGER,   INTENT( IN ) :: R
            REAL, PARAMETER :: HAOM2   = 1.0e-4 ! ha/m^2 conversion
            REAL, PARAMETER :: MWNH3   = 17.031 ! molecular weight of NH3
            REAL, PARAMETER :: MWNH4   = 18.039 ! molecular weight of NH4
            REAL, PARAMETER :: MWHNO3  = 63.013 ! molecular weight of HNO3
            REAL, PARAMETER :: MWNO3   = 62.005 ! molecular weight of NO3
            REAL, PARAMETER :: MWNO2   = 46.006 ! molecular weight of NO2
            REAL, PARAMETER :: MWPAN   = 121.05 ! molecular weight of Peroxyacyl nitrate
            REAL, PARAMETER :: MWN     = 14.007 ! molecular weight of Nitrogen
            REAL, PARAMETER :: NGOKG   = 1.0e12 ! ng/kg conversion
            
            ! takes Kg/hectare/s and converts to ng N / m^2/s

            IF( INDEX(TRIM( SPEC ), 'NH3') .NE. 0 ) THEN
               NDEPRATE( C,R ) = NDEPRATE( C,R ) + DEP*HAOM2*NGOKG*MWN/MWNH3 
            ELSE IF( INDEX(TRIM( SPEC ), 'NH4') .NE. 0 ) THEN
               NDEPRATE( C,R ) = NDEPRATE( C,R ) + DEP*HAOM2*NGOKG*MWN/MWNH4
            ELSE IF( INDEX(TRIM( SPEC ), 'HNO3') .NE. 0 ) THEN
               NDEPRATE( C,R ) = NDEPRATE( C,R ) + DEP*HAOM2*NGOKG*MWN/MWHNO3
            ELSE IF( INDEX(TRIM( SPEC ), 'NO3') .NE. 0) THEN
               NDEPRATE( C,R ) = NDEPRATE( C,R ) + DEP*HAOM2*NGOKG*MWN/MWNO3
            ELSE IF( INDEX(TRIM( SPEC ), 'NO2') .NE. 0 ) THEN
               NDEPRATE( C,R ) = NDEPRATE( C,R ) + DEP*HAOM2*NGOKG*MWN/MWNO2
            ELSE IF( INDEX(TRIM( SPEC ), 'PAN') .NE. 0 ) THEN
               NDEPRATE( C,R ) = NDEPRATE( C,R ) + DEP*HAOM2*NGOKG*MWN/MWPAN
            Else
               MESG = 'Invalid Species Name in Get_N_Dep: "' // SPEC // '"'
               !CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
            END IF
            
            IF( (DEP<0.0) .OR. (NDEPRATE( C,R )<0.0) ) then
            Write(*,*) 'DEP or sum negative',DEP, ' ',NDEPRATE( C,R)
            MESG = 'negative'
               CALL M3EXIT( 'GET_N_DEP', 0, 0, MESG, XSTAT2 )
            END if

         RETURN

         
         END SUBROUTINE GET_N_DEP  

C -----------------------------------------------------------------------------
         REAL FUNCTION SOILTEMP( SOILT )
C Calculate the soil temperature factor

         IMPLICIT NONE
C Function arguments:
         REAL, INTENT( IN )       :: SOILT !kelvin, soil temperature
C Local Variables
         REAl SOILTC !temperature in degrees celsius
         CHARACTER( 256 ) :: MESG            ! message buffer
         CHARACTER( 16 )  :: PNAME = 'SOILTEMP'  ! procedure name
         SOILTC = SOILT - 273.16

         IF ( SOILTC <= 0d0 ) THEN
         ! No soil emissions if temp below freezing
         SOILTEMP = 0d0
C         BENCHMARKING:
C         MESG = 'temperature less than 0 in august florida?'
C         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT1 )

         ELSE

         ! Caps temperature response at 30C
         IF ( SOILTC >= 30.d0 ) SOILTC = 30.d0

         SOILTEMP =  EXP( 0.103 * SOILTC )

         ENDIF
         RETURN

         END FUNCTION SOILTEMP

C ---------------------------------------------------------------------------------------------------------
         REAL FUNCTION FERTADD( FERT , DEPN )
C Add fertilizer reservoir to deposition reservoir and create N driven
C emission factor
         IMPLICIT NONE
C Function arguments:
         REAL, INTENT( IN )       :: FERT !fertilizer reservoir [ngN/m2]
         REAL, INTENT( IN )       :: DEPN !deposition reservoir [ngN/m2]

C Local Variables
         REAL*8,  PARAMETER :: SECPERYEAR    = 86400.d0 * 365.
         ! Scale factor so that fertilizer emission = 1.8 Tg N/yr
         ! (Stehfest and Bouwman, 2006)
         ! before canopy reduction
         REAL*8, PARAMETER :: FERT_SCALE = 0.0068 ! [yr -1]
         ! Value calculated by running the 2x2.5 GEOS-Chem model
         ! (J.D. Maasakkers)
         FERTADD = FERT + DEPN
         FERTADD = FERTADD / SECPERYEAR * FERT_SCALE

         RETURN

         END FUNCTION FERTADD

C -------------------------------------------------------------------------------------

C Local Variables

         REAL FUNCTION SOILWET( THETA , ARID, NONARID)
C Calculate the soil moisture factor

         IMPLICIT NONE
C Function arguments:
         REAL, INTENT( IN )       :: THETA !0-1 soil moisture
         INTEGER, INTENT( IN )    :: ARID !1 indicates arid cell
         INTEGER, INTENT( IN )    :: NONARID !1 indicates nonarid cell, if both 0 then
C Local Variables

         IF ( ARID .EQ. 1 ) THEN !ARID, Max poison at theta = .2
         SOILWET = 8.24*THETA*EXP(-12.5*THETA*THETA)
         ELSE IF (NONARID .EQ. 1 ) THEN !NONARID Max Poisson at theta =.3
         SOILWET = 5.5*THETA*EXP(-5.55*THETA*THETA)
         ELSE !neither arid nor nonarid, water or non-emitting cell
         SOILWET = 0.0
         END IF

         RETURN

         END FUNCTION SOILWET

C -------------------------------------------------------------------

      SUBROUTINE GET_CANOPY_NOX(JDATE, JTIME, COSZEN,
     & TASFC, SSOLAR, PRES, LANDTYPE, LAI, SNOCOV, CFRAC, WSPD, CRF) ! called tmpbeis, change called BDSNP, add K argument

      IMPLICIT NONE

C Arguments
      INTEGER, INTENT( IN )  :: JDATE             ! current simulation date (YYYYDDD)
      INTEGER, INTENT( IN )  :: JTIME             ! current simulation time (HHMMSS)
C     These are arrays 
      REAL,    INTENT( IN ) :: COSZEN( NCOLS,NROWS )        ! cosine of zenith angle
      REAL,    INTENT( IN ) :: TASFC ( NCOLS,NROWS )        ! surface air temperature [K]
      REAL,    INTENT( IN ) :: SSOLAR( NCOLS,NROWS )        ! surface radiation [w/m**2]
      REAL,    INTENT( IN ) :: PRES  ( NCOLS,NROWS )        ! surface pressure [Pa]
      INTEGER, INTENT( IN ) :: LANDTYPE( NCOLS,NROWS )     ! the biome type in each cell
      REAL,    INTENT( IN ):: LAI  ( NCOLS,NROWS )        ! leaf area index (m2/m2)
      REAL,    INTENT( IN ):: SNOCOV  ( NCOLS,NROWS )        ! snow cover
      REAL,    INTENT( IN ):: CFRAC  ( NCOLS,NROWS )        ! cloud fraction
      REAL,    INTENT( IN ):: WSPD  ( NCOLS,NROWS )        ! cloud fraction
      REAL,    INTENT( OUT ):: CRF  ( NCOLS,NROWS )        ! outputs the canopy reduction factor
      ! !LOCAL VARIABLES:
      !
      CHARACTER( 16 )  :: PNAME = 'CANOPY_NOX'  ! procedure name
      INTEGER          IOS                ! IO or memory allocation status
      CHARACTER( 256 ) :: MESG            ! message buffer
      ! Scalars
      INTEGER :: C, R, K, KK, MY_NCOLS, MY_NROWS
      REAL*8  :: F0,     HSTAR, XMW              
      REAL*8  :: DTMP1,  DTMP2, DTMP3,  DTMP4, GFACT, GFACI
      REAL*8  :: RT,     RAD0,  RIX,    RIXX,  RDC,   RLUXX
      REAL*8  :: RGSX,   RCLX,  TEMPK,  TEMPC, WINDSQR
      REAL*8 :: VFNEW
      
      LOGICAL, SAVE          :: FIRSTCANOPY = .TRUE. 

      ! Arrays
      REAL*8  :: RI  (24)       
      REAL*8  :: RLU (24)      
      REAL*8  :: RAC (24)      
      REAL*8  :: RGSS(24)     
      REAL*8  :: RGSO(24)     
      REAL*8  :: RCLS(24)     
      REAL*8  :: RCLO(24)
! !DEFINED PARAMETERS:
!      
      INTEGER, PARAMETER :: SNIRI(24) = (/9999, 200, 9999, 9999, 9999, 9999, 
     & 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 400, 400, 
     & 200, 200, 200, 9999, 200/)

      INTEGER, PARAMETER :: SNIRLU(24) = (/9999, 9000, 9999, 9999, 9999, 
     & 9999, 9000, 9000, 9000, 9000, 9000, 9000, 9000, 9000, 9000, 1000, 
     & 9000, 9000, 9000, 9000, 1000, 9000, 9999, 9000/)

      INTEGER, PARAMETER :: SNIRAC(24) = (/0, 300, 0, 0, 0, 0, 100, 100, 
     & 100, 100, 100, 100, 100, 100, 2000, 2000, 2000, 2000, 2000, 2000, 
     & 2000, 200, 100, 200/)

      INTEGER, PARAMETER :: SNIRGSS(24) = (/0, 0, 100, 1000, 100, 1000, 350, 
     & 350, 350, 350, 350, 350, 350, 350, 500, 200, 500, 500, 500, 500, 
     & 200, 150, 400, 150/)

      INTEGER, PARAMETER :: SNIRGSO(24) = (/2000, 1000, 3500, 400, 3500, 
     & 400, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 
     & 200, 200, 200, 150, 300, 150/)

      INTEGER, PARAMETER :: SNIRCLS(24) = (/9999, 2500, 9999, 9999, 9999, 
     & 9999, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 9999, 
     & 2000, 2000, 2000, 2000, 9999, 2000, 9999, 2000/)
    
      INTEGER, PARAMETER :: SNIRCLO(24) = (/9999, 1000, 1000, 9999, 1000, 
     & 9999, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 9999, 
     & 1000, 1000, 1000, 1000, 9999, 1000, 9999, 1000/)

      INTEGER, PARAMETER :: SNIVSMAX(24) = (/10, 100, 100, 10, 100, 10, 100, 
     & 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 
     & 100, 100, 100, 100/)   

      REAL, PARAMETER :: DRYCOEFF(20) = (/-0.358, 3.02, 3.85, -0.0978, -3.66, 
     & 12.0, 0.252, -7.8, 0.226, 0.274, 1.14, -2.19, 0.261, -4.62, 0.685, 
     & -0.254, 4.37, -0.266, -0.159, -0.206 /)   

      ! Canopy wind extinction coefficients
      ! (cf. Yienger & Levy [1995], Sec 5), now a function of the MODIS/KOPPEN biometype (J.D. Maasakkers)
       REAL*8,  PARAMETER :: SOILEXC(24)    = (/ 
     &  0.10, 0.50, 0.10, 0.10, 0.10,
     &  0.10, 0.10, 0.10, 0.10, 1.00,
     &  1.00, 1.00, 1.00, 2.00, 4.00,
     &  4.00, 4.00, 4.00, 4.00, 4.00,
     &  4.00, 2.00, 0.10, 2.00                  /)     
      

      ! Molecular weight of water [kg]
      REAL*8, PARAMETER :: XMWH2O = 18d-3
      !
      ! Ventilation velocity for NOx, day & night values [m/s]
      REAL*8,  PARAMETER :: VFDAY   = 1.0d-2
      REAL*8,  PARAMETER :: VFNIGHT = 0.2d-2 
      REAL*8, PARAMETER :: PRESS  = 1.5d5

      ! Set physical parameters
      HSTAR = 0.01d0              ! Henry's law constant
      F0    = 0.1d0               ! Reactivity factor for biological oxidation 
      XMW   = 46d-3               ! Molecular wt of NO2 (kg)

      IF( FIRSTCANOPY ) THEN
        FIRSTCANOPY = .FALSE.
      END IF
   
      CRF = 0d0 ! array
      
      ! begin calculating canopy reduction factor
      DO C=1, NCOLS
        DO R=1, NROWS
          IF(LAI(C,R) > 0.0) THEN
            TEMPC = TASFC(C,R) - 273.15d0 ! convert kelvin to Celsius
      ! Compute bulk surface resistance for gases.    
         !                                  
         !  Adjust external surface resistances for temperature; 
         !  from Wesely [1989], expression given in text on p. 1296.        
            RT = 1000.0D0 * EXP( -TEMPC - 4.0d0 )
         
                
         !--------------------------------------------------------------
         ! Get surface resistances - loop over biome types K
         !
         ! The land types within each grid square are defined using the 
         ! Olson land-type database.  Each of the Olson land types is 
         ! assigned a corresponding "deposition land type" with 
         ! characteristic values of surface resistance components.  
         ! There are 74 Olson land-types but only 11 deposition 
         ! land-types (i.e., many of the Olson land types share the 
         ! same deposition characteristics).  Surface resistance 
         ! components for the "deposition land types" are from Wesely 
         ! [1989] except for tropical forests [Jacob and Wofsy, 1990] 
         ! and for tundra [Jacob et al., 1992].  All surface resistance 
         ! components are normalized to a leaf area index of unity.
         !--------------------------------------------------------------
	!Set biometype
         
            K = LANDTYPE( C,R )
            
            ! Set second loop variable to K to allow snow/ice correction
	     KK = K

            ! If the surface is snow or ice, then set K=3
            IF ( SNOCOV(C,R) .EQ. 1 ) KK = 3

		!USE new MODIS/KOPPEN Biometypes to read data

            ! Read the internal resistance RI (minimum stomatal resistance 
            ! for water vapor, per unit area of leaf) from the IRI array; 
            ! a '9999' value means no deposition to stomata so we impose a 
            ! very large value for RI.
            RI(K) = DBLE( SNIRI(KK) )
            IF ( RI(K) >= 9999.D0 ) RI(K)= 1.D12
            
            ! Cuticular resistances IRLU read in from 'drydep.table'
            ! are per unit area of leaf; divide them by the leaf area index 
            ! to get a cuticular resistance for the bulk canopy.  If IRLU is 
            !'9999' it means there are no cuticular surfaces on which to 
            ! deposit so we impose a very large value for RLU.
            IF ( SNIRLU(KK) >= 9999 .OR. LAI(C,R) <= 0d0 ) THEN
               RLU(K)  = 1.D6
            ELSE
               RLU(K)= DBLE( SNIRLU(KK) ) / LAI(C,R) + RT
            ENDIF

            ! The following are the remaining resistances for the Wesely
            ! resistance-in-series model for a surface canopy
            ! (see Atmos. Environ. paper, Fig.1).  
            RAC(K)  = MAX( DBLE( SNIRAC(KK)  ),      1d0 )
            RGSS(K) = MAX( DBLE( SNIRGSS(KK) ) + RT, 1d0 )
            RGSO(K) = MAX( DBLE( SNIRGSO(KK) ) + RT, 1d0 ) 
            RCLS(K) =      DBLE( SNIRCLS(KK) ) + RT           
            RCLO(K) =      DBLE( SNIRCLO(KK) ) + RT 

            IF (  RAC(K) >= 9999.D0 ) RAC(K)  = 1d12
            IF ( RGSS(K) >= 9999.D0 ) RGSS(K) = 1d12
            IF ( RGSO(K) >= 9999.D0 ) RGSO(K) = 1d12
            IF ( RCLS(K) >= 9999.D0 ) RCLS(K) = 1d12         
            IF ( RCLO(K) >= 9999.D0 ) RCLO(K) = 1d12

            !-------------------------------------------------------------
            ! Adjust stomatal resistances for insolation and temperature:  
            ! 
            ! Temperature adjustment is from Wesely [1989], equation (3).
            ! 
            ! Light adjustment by the function BIOFIT is described by Wang 
            ! [1996].  It combines:
            !
            ! - Local dependence of stomal resistance on the intensity I 
            !   of light impinging the leaf; this is expressed as a 
            !   multiplicative factor I/(I+b) to the stomatal resistance 
            !   where b = 50 W m-2
            !   (equation (7) of Baldocchi et al. [1987])
            ! - Radiative transfer of direct and diffuse radiation in the 
            !   canopy using equations (12)-(16) from Guenther et al. 
            !   [1995]
            ! - Separate accounting of sunlit and shaded leaves using
            !   equation (12) of Guenther et al. [1995]
            ! - Partitioning of the radiation at the top of the canopy 
            !   into direct and diffuse components using a 
            !   parameterization to results from an atmospheric radiative 
            !   transfer model [Wang, 1996]
            !
            ! The dependent variables of the function BIOFIT are the leaf 
            ! area index (XYLAI), the cosine of zenith angle (SUNCOS) and 
            ! the fractional cloud cover (CFRAC).  The factor GFACI 
            ! integrates the light dependence over the canopy depth; so
            ! be scaled by LAI to yield a bulk canopy value because that's 
            ! already done in the GFACI formulation.
            !-------------------------------------------------------------

            ! Radiation @ sfc [W/m2]
            RAD0 = SSOLAR(C,R)
            
            ! Internal resistance
            RIX  = RI(K)

            ! Skip the following block if the resistance RIX is high
            IF ( RIX < 9999d0 ) THEN
               GFACT = 100.0D0

               IF ( TEMPC > 0.D0 .AND. TEMPC < 40.D0) THEN
                  GFACT = 400.D0 / TEMPC / ( 40.0D0 - TEMPC )
               ENDIF

               GFACI = 100.D0

               IF ( RAD0 > 0d0 .AND. LAI(C,R) > 0d0 ) THEN
                  GFACI= 1d0 / 
     &                   BIOFIT( DRYCOEFF,       LAI(C,R),
     &                           COSZEN(C,R), CFRAC(C,R)    )
               ENDIF
            
               RIX = RIX * GFACT * GFACI
            ENDIF
            
            ! Compute aerodynamic resistance to lower elements in lower 
            ! part of the canopy or structure, assuming level terrain - 
            ! equation (5) of Wesely [1989].                     
            RDC = 100.D0*(1.0D0+1000.0D0/(RAD0 + 10.D0))

            ! Loop over species; species-dependent corrections to resistances
            ! are from equations (6)-(9) of Wesely [1989].
            !
            ! NOTE: here we only consider NO2 (bmy, 6/22/09)
            RIXX   = RIX * DIFFG( TASFC(C,R), PRESS, XMWH2O ) /
     &                     DIFFG( TASFC(C,R), PRESS, XMW    )
     &             + 1.D0 / ( HSTAR/3000.D0 + 100.D0*F0  )

            RLUXX  = 1.D12

            IF ( RLU(K) < 9999.D0 ) THEN
               RLUXX = RLU(K) / ( HSTAR / 1.0D+05 + F0 )
            ENDIF
            
            ! To prevent virtually zero resistance to species with huge HSTAR, 
            ! such as HNO3, a minimum value of RLUXX needs to be set. 
            ! The rationality of the existence of such a minimum is 
            ! demonstrated by the observed relationship between Vd(NOy-NOx) 
            ! and Ustar in Munger et al.[1996]; Vd(HNO3) never exceeds 2 cm/s 
            ! in observations. The corresponding minimum resistance is 50 s/m.
            ! was introduced by J.Y. Liang on 7/9/95.
            RGSX = 1d0 / ( HSTAR/1d5/RGSS(K) + F0/RGSO(K) )
            RCLX = 1d0 / ( HSTAR/1d5/RCLS(K) + F0/RCLO(K) )

            ! Get the bulk surface resistance of the canopy
            ! from the network of resistances in parallel and in series 
            ! (Fig. 1 of Wesely [1989])
            DTMP1 = 1.D0 / RIXX
            DTMP2 = 1.D0 / RLUXX
            DTMP3 = 1.D0 / ( RAC(K) + RGSX )
            DTMP4 = 1.D0 / ( RDC      + RCLX )

            ! Save the within canopy depvel of NOx, used in calculating 
            ! the canopy reduction factor for soil emissions [1/s]
            CRF(C,R) = DTMP1 + DTMP2 + DTMP3 + DTMP4
            
            ! Pick proper ventilation velocity for day or night
            IF ( COSZEN( C,R ) > 0d0 ) THEN
               VFNEW = VFDAY              
            ELSE 
               VFNEW = VFNIGHT            
            ENDIF

      ! If the leaf area index and the bulk surface resistance
      ! of the canopy to NOx deposition are both nonzero ...
            IF (CRF(C,R) > 0d0 ) THEN

         ! Adjust the ventilation velocity.  
         ! NOTE: SOILEXC(21) is the canopy wind extinction 
         ! coefficient for the tropical rainforest biome.
              WINDSQR=WSPD(C,R)*WSPD(C,R)
              VFNEW    = (VFNEW * SQRT( WINDSQR/9d0 * 7d0/LAI(C,R)) *
     &                          ( SOILEXC(21)  / SOILEXC(K) ))

         ! Soil canopy reduction factor
              CRF(C,R) = CRF(C,R) / ( CRF(C,R) + VFNEW )
         
C         IF( CRF(C,R) > 1.0 ) THEN
C         write(*,*) 'CANOPY NOX REDUCTION FACTOR TOO HIGH'
C         write(*,*) 'C,R,k,crf,vfnew,dt1,dt2,dt3,dt4'
C         write(*,*) C,R,k, CRF(C,R), VFNEW, DTMP1, DTMP2, DTMP3, DTMP4 
C         write(*,*) 'windsqr, cos, lai'
C         write(*,*) WINDSQR, COSZEN(C,R),LAI(C,R)
C         write(*,*) 'soil21/soilk'
C         write(*,*) SOILEXC(21)/SOILEXC(K) 
C         write(*,*) 'rdc,rclx,rac,rgsx,rluxx,rixx,rclo'
C         write(*,*) RDC, RCLX, RAC(K), RGSX, RLUXX, RIXX, RCLO(K)
C         write(*,*) 'RCLS(K), RGSO(K), RGSS(K), RLU(K)'
C         write(*,*) RCLS(K), RGSO(K), RGSS(K), RLU(K)
C         write(*,*) 'DIFFG(h20)/DIFFG(no2)'        
c         write(*,*) DIFFG( TASFC(C,R), PRESS, XMWH2O ) /
C     &                     DIFFG( TASFC(C,R), PRESS, XMW    )
C        write(*,*) DIFFG( TASFC(C,R), PRESS, XMWH2O )
C         write(*,*) DIFFG( TASFC(C,R), PRESS, XMW    )
C         write(*,*) TASFC(C,R), PRESS, XMW, XMWH2O
C         write(*,*) '1.D0 / ( HSTAR/3000.D0 + 100.D0*F0  )'
C         write(*,*) 1.D0 / ( HSTAR/3000.D0 + 100.D0*F0  )
C         write(*,*) 'RAD0, GFACT, GFACI, BIOFIT'
C         write(*,*) RAD0, GFACT, GFACI, BIOFIT( DRYCOEFF, LAI(C,R),
C     &                           COSZEN(C,R), CFRAC(C,R))
C         write(*,*) 'TEMPC, CFRAC(C,R), DRYCOEFF(K), RT'
C         write(*,*) TEMPC, CFRAC(C,R), DRYCOEFF(K), RT
C         write(*,*) 'RI(K),RLU(K),RAC(K),RGSS(K),RGSO(K),RCLS(K),RCLO(K)'
C         write(*,*) RI(K), RLU(K),RAC(K),RGSS(K),RGSO(K),RCLS(K),RCLO(K)
C         MESG = 'CRF too high'
C         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT1 )
C         END IF   

            ELSE ! CRF < 0.0
     
         ! Otherwise set the soil canopy reduction factor to zero
              CRF(C,R) = 0d0

            END IF

            IF( CRF(C,R) .LT. 0.0) THEN
            
            MESG = 'CRF Less than 0'
!            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
            
            ELSE IF( CRF(C,R) .GT. 1.0) THEN
            
            MESG = 'CRF Greater than one'
!            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 ) 
            
            END IF
            
          ELSE
            CRF(C,R) = 0.0
          END IF !lai check

        END DO !row loop
      END DO !col loop
            
      END SUBROUTINE GET_CANOPY_NOX
      
      FUNCTION DIFFG( TK, PRESS, XM ) RESULT( DIFF_G )
! !DESCRIPTION: Function DIFFG calculates the molecular diffusivity [m2/s] in 
!  air for a gas X of molecular weight XM [kg] at temperature TK [K] and 
!  pressure PRESS [Pa].
!\\
!\\
!  We specify the molecular weight of air (XMAIR) and the hard-sphere molecular
!  radii of air (RADAIR) and of the diffusing gas (RADX).  The molecular
!  radius of air is given in a Table on p. 479 of Levine [1988].  The Table
!  also gives radii for some other molecules.  Rather than requesting the user
!  to supply a molecular radius we specify here a generic value of 2.E-10 m for
!  all molecules, which is good enough in terms of calculating the diffusivity
!  as long as molecule is not too big.
!
! !INPUT PARAMETERS:
!
      REAL, INTENT(IN) :: TK      ! Temperature [K]
      REAL*8, INTENT(IN) :: PRESS   ! Pressure [Pa]
      REAL*8, INTENT(IN) :: XM      ! Molecular weight of gas [kg]
!
! !RETURN VALUE:
!
      REAL*8             :: DIFF_G  ! Molecular diffusivity [m2/s]
!
! !REVISION HISTORY:
!     22 Jun 2009 - R. Yantosca - Copied from "drydep_mod.f"
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      REAL*8             :: AIRDEN, Z, DIAM, FRPATH, SPEED            
!
! !DEFINED PARAMETERS:
!
      REAL*8, PARAMETER  :: XMAIR  = 28.8d-3 
      REAL*8, PARAMETER  :: RADAIR = 1.2d-10
      REAL*8, PARAMETER  :: PI     = 3.1415926535897932d0
      REAL*8, PARAMETER  :: RADX   = 1.5d-10
      REAL*8, PARAMETER  :: RGAS   = 8.32d0
      REAL*8, PARAMETER  :: AVOGAD = 6.023d23

      !=================================================================
      ! DIFFG begins here!
      !=================================================================

      ! Air density
      AIRDEN = ( PRESS * AVOGAD ) / ( RGAS * TK )

      ! DIAM is the collision diameter for gas X with air.
      DIAM   = RADX + RADAIR

      ! Calculate the mean free path for gas X in air: 
      ! eq. 8.5 of Seinfeld [1986];
      Z      = XM  / XMAIR
      FRPATH = 1d0 /( PI * SQRT( 1d0 + Z ) * AIRDEN*( DIAM**2 ) )

      ! Calculate average speed of gas X; eq. 15.47 of Levine [1988]
      SPEED  = SQRT( 8d0 * RGAS * TK / ( PI * XM ) )

      ! Calculate diffusion coefficient of gas X in air; 
      ! eq. 8.9 of Seinfeld [1986]
      DIFF_G = ( 3d0 * PI / 32d0 ) * ( 1d0 + Z ) * FRPATH * SPEED

      ! Return to calling program
      END FUNCTION DIFFG
      
      SUBROUTINE SUNPARAM(X)

      IMPLICIT NONE


!===============================================
! the sequence is lai,suncos,cloud fraction
!===============================================
!  NN = number of variables (lai,suncos,cloud fraction)
      INTEGER NN
      PARAMETER(NN=3)
!  ND = scaling factor for each variable
      INTEGER ND(NN),I
      DATA ND /55,20,11/
!  X0 = maximum for each variable
      REAL X(NN),X0(NN),XLOW
      DATA X0 /11.,1.,1./

      DO I=1,NN
        X(I)=MIN(X(I),X0(I))
! XLOW = minimum for each variable
        IF (I.NE.3) THEN
          XLOW=X0(I)/REAL(ND(I))
        ELSE
          XLOW= 0.
        END IF
        X(I)=MAX(X(I),XLOW)
        X(I)=X(I)/X0(I)
      END DO

      RETURN
      END SUBROUTINE SUNPARAM
      
      REAL*8 FUNCTION BIOFIT(COEFF1,XLAI1,SUNCOS1,CFRAC1)

      IMPLICIT NONE

!===============================================
! Calculate the light correction
!===============================================
!* BIOFIT and SUNPARAM were written by Y.H. Wang.   
!*             !-------------------------------------------------------------
            ! Adjust stomatal resistances for insolation and temperature:  
            ! 
            ! Temperature adjustment is from Wesely [1989], equation (3).
            ! 
            ! Light adjustment by the function BIOFIT is described by Wang 
            ! [1996].  It combines:
            !
            ! - Local dependence of stomal resistance on the intensity I 
            !   of light impinging the leaf; this is expressed as a 
            !   multiplicative factor I/(I+b) to the stomatal resistance 
            !   where b = 50 W m-2
            !   (equation (7) of Baldocchi et al. [1987])
            ! - Radiative transfer of direct and diffuse radiation in the 
            !   canopy using equations (12)-(16) from Guenther et al. 
            !   [1995]
            ! - Separate accounting of sunlit and shaded leaves using
            !   equation (12) of Guenther et al. [1995]
            ! - Partitioning of the radiation at the top of the canopy 
            !   into direct and diffuse components using a 
            !   parameterization to results from an atmospheric radiative 
            !   transfer model [Wang, 1996]
            !
            ! The dependent variables of the function BIOFIT are the leaf 
            ! area index (XYLAI), the cosine of zenith angle (SUNCOS) and 
            ! the fractional cloud cover (CFRAC).  The factor GFACI 
            ! integrates the light dependence over the canopy depth; so
            ! be scaled by LAI to yield a bulk canopy value because that's 
            ! already done in the GFACI formulation.
!*************************************************************
      INTEGER KK
      PARAMETER (KK=4)
      REAL COEFF1(20),TERM(KK),REALTERM(20)
      REAL XLAI1,SUNCOS1,CFRAC1
      INTEGER K,K1,K2,K3

      TERM(1)=1.
      TERM(2)=XLAI1
      TERM(3)=SUNCOS1
      TERM(4)=CFRAC1
      CALL SUNPARAM(TERM(2))
      K=0
      DO K3=1,KK
        DO K2=K3,KK
          DO K1=K2,KK
            K=K+1
            REALTERM(K)=TERM(K1)*TERM(K2)*TERM(K3)
          END DO
        END DO
      END DO
      BIOFIT=0
      DO K=1,20
        BIOFIT=BIOFIT+COEFF1(K)*REALTERM(K)
      END DO
      IF (BIOFIT.LT.0.1) BIOFIT=0.1

      RETURN
      END FUNCTION BIOFIT
      
!  References:
!  ============================================================================
!  (1 ) Baldocchi, D.D., B.B. Hicks, and P. Camara, "A canopy stomatal
!        resistance model for gaseous deposition to vegetated surfaces",
!        Atmos. Environ. 21, 91-101, 1987.
!  (2 ) Brutsaert, W., "Evaporation into the Atmosphere", Reidel, 1982.
!  (3 ) Businger, J.A., et al., "Flux-profile relationships in the atmospheric 
!        surface layer", J. Atmos. Sci., 28, 181-189, 1971.
!  (4 ) Dwight, H.B., "Tables of integrals and other mathematical data",
!        MacMillan, 1957.
!  (5 ) Guenther, A., and 15 others, A global model of natural volatile
!         organic compound emissions, J. Geophys. Res., 100, 8873-8892, 1995.
!  (6 ) Hicks, B.B., and P.S. Liss, "Transfer of SO2 and other reactive
!        gases across the air-sea interface", Tellus, 28, 348-354, 1976.
!  (7 ) Jacob, D.J., and S.C. Wofsy, "Budgets of reactive nitrogen,
!        hydrocarbons, and ozone over the Amazon forest during the wet season",
!        J.  Geophys. Res., 95, 16737-16754, 1990.
!  (8 ) Jacob, D.J., et al, "Deposition of ozone to tundra", J. Geophys. Res., 
!        97, 16473-16479, 1992.
!  (9 ) Levine, I.N., "Physical Chemistry, 3rd ed.", McGraw-Hill, 
!        New York, 1988.
!  (10) Munger, J.W., et al, "Atmospheric deposition of reactive nitrogen 
!        oxides and ozone in a temperate deciduous forest and a sub-arctic 
!        woodland", J. Geophys. Res., in press, 1996.
!  (11) Walcek, C.J., R.A. Brost, J.S. Chang, and M.L. Wesely, "SO2, sulfate, 
!        and HNO3 deposition velocities computed using regional landuse and
!        meteorological data", Atmos. Environ., 20, 949-964, 1986.
!  (12) Wang, Y.H., paper in preparation, 1996.
!  (13) Wesely, M.L, "Improved parameterizations for surface resistance to
!        gaseous dry deposition in regional-scale numerical models", 
!        Environmental Protection Agency Report EPA/600/3-88/025,
!        Research Triangle Park (NC), 1988.
!  (14) Wesely, M. L., Parameterization of surface resistance to gaseous dry 
!        deposition in regional-scale numerical models.  Atmos. Environ., 23
!        1293-1304, 1989. 
!  (15) Price, H., L. Jaeglé, A. Rice, P. Quay, P.C. Novelli, R. Gammon, 
!        Global Budget of Molecular Hydrogen and its Deuterium Content: 
!        Constraints from Ground Station, Cruise, and Aircraft Observations,
!        submitted to J. Geophys. Res., 2007.      
      END MODULE BDSNP_MOD
      
