
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
      MODULE MEGAN_HRNO_MOD

      CONTAINS
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE MEGAN_HRNO( JDATE, JTIME, TSTEP, L_DESID_DIAG, PRECADJ)

C-----------------------------------------------------------------------
C Description:
C Similar to HRNO.F, this module outputs hourly rainfall to
C a soilout file at the end of the day. It also outputs LAI for daily 
C growth calculations that are needed by MEGAN, and hourly
C shortwave radiation and 2m temperature for daily averaging.
C Lastly, it calculates PRECADJ for use in megan_fx.f90. The 
C soil NO emissions are calculated in megan_fx.f90 and the YL95
C implementation varies in minor ways from in BEIS. 
C There are three parts to this subroutine:
C  Part 1: keeping track of rainfall pulses
C  Part 2: calculating precadj
C  Part 3: writing to the soilout file
C Please see documentation for more information.    
C-----------------------------------------------------------------------
      USE RUNTIME_VARS
      USE HGRD_DEFN             ! horizontal domain specifications
      USE BIOG_EMIS, ONLY: NSEF ! beis
      USE ASX_DATA_MOD
      USE UTILIO_DEFN
#ifndef mpas
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif
      USE centralized_io_module

      IMPLICIT NONE
        
C Includes:

C Arguments:
      INTEGER, INTENT( IN )  :: JDATE           ! current simulation date (YYYYDDD)
      INTEGER, INTENT( IN )  :: JTIME           ! current simulation time (HHMMSS)
      INTEGER, INTENT( IN )  :: TSTEP( 3 )      ! time step vector (HHMMSS)
      LOGICAL, INTENT( IN )  :: L_DESID_DIAG
      REAL,    INTENT( OUT ) :: PRECADJ( :,: )  ! output precip adjustment
#ifdef mpas
      integer, save :: output_step, half_syn_step  ! values are in seconds
#endif



C External Functions
      LOGICAL,         EXTERNAL :: CHKGRID

C Parameters:
      INTEGER, PARAMETER :: MXRHRS = 24     ! no. of rainfall hours for YL95 algorithm
      INTEGER, PARAMETER :: LSM_WATER = 14
        
C Saturation values for 11 soil types from pxpbl.F  (MCIP PX version)
C In LSM_MOD:WSAT
C Pleim-Xiu Land-Surface and PBL Model (PX-LSM)
C See Jacquemin B. and Noilhan J. (1990), Bound.-Layer Meteorol., 52, 93-134.

C Local Variables:

      CHARACTER( 16 ), SAVE :: MNAME   ! logical name for MET_CRO_2D
      CHARACTER( 16 ), SAVE :: SOILINP ! logical name for input NO soil data
      CHARACTER( 16 ), SAVE :: SOILOUT      = 'MEGAN_SOILOUT' ! logical name for output NO soil data
      CHARACTER( 33 ), SAVE :: DESCSTR      = 'hrly cnv. & non-cnv. rainfall for'
      CHARACTER( 33 ), SAVE :: DESCSTRSW    = 'hrly instantaneous rgrnd for'
      CHARACTER( 33 ), SAVE :: DESCSTRT2M   = 'hrly instantaneous 2m temp for'
      CHARACTER( 33 ), SAVE :: DESCSTRLAI   = 'LAI for day'


      CHARACTER( 16 ) :: VAR        ! variable name

      INTEGER, SAVE :: IHR       ! current simulation hour
      INTEGER          NDX       ! RAINFALL array timestep index


      REAL,    ALLOCATABLE, SAVE :: C_RAINFALL ( :,: ) ! rainfall for current hour
      REAL,    ALLOCATABLE, SAVE :: RNTOT    ( :,: )  ! RN + RC
      INTEGER, SAVE :: RHOURS    ! SOILINP(OUT) file no. of RAINFALL hour variables
      INTEGER, SAVE :: RDATE     ! date to update rainfall
      INTEGER, SAVE :: RTIME     ! time to update rainfall
      INTEGER, SAVE :: EDATE     ! end scenario date
      INTEGER, SAVE :: ETIME     ! end scenario time
      INTEGER, SAVE :: NDATE     ! test date to update rainfall
      INTEGER, SAVE :: NTIME     ! test time to update rainfall
        
      LOGICAL, SAVE :: INITIAL_DAY = .FALSE.  ! true: 1st 24 hours; no previous data
                                              ! false: previous 24 hours of rainfall
                                              ! are available for HRNO

      INTEGER          SOILCAT            ! soil category
      INTEGER, SAVE :: MSTEPS             ! run no. of steps
      INTEGER          I, J, K, R, C, L   ! counters
      INTEGER          IOS                ! IO or memory allocation status
      INTEGER, SAVE :: METSTEP            ! met_cro_2d time step
      
      REAL             FAC2

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      CHARACTER( 256 ) :: MESG            ! message buffer
      CHARACTER( 16 )  :: PNAME = 'MEG_HRNO'  ! procedure name

#ifdef mpas
      integer :: io_mode
      CHARACTER( 20 ) :: time_stamp
#endif

      LOGICAL, EXTERNAL :: FLUSH3


C-----------------------------------------------------------------------------
C--- Part 1: Keeping track of rainfall pulses
C-----------------------------------------------------------------------------

      PRECADJ = 0.0

      IF ( FIRSTIME ) THEN
!        FIRSTIME = .FALSE.

C Determine last timestamp
         EDATE = STDATE; ETIME = STTIME
         CALL NEXTIME( EDATE, ETIME, RUNLEN )   ! end date & time
         MSTEPS = TIME2SEC( RUNLEN ) / TIME2SEC( TSTEP( 1 ) )

#ifdef mpas
         if (ncd_64bit_offset) then
            io_mode = ior (nf90_noclobber, nf90_64bit_offset)
         else
            io_mode = nf90_noclobber
         end if

         call mio_fcreate (SOILOUT, io_mode)
         METSTEP = TSTEP(3)

         output_step   = time2sec(tstep(1))
         half_syn_step = time2sec(tstep(2)) / 2
#else

C Open met file
         MNAME = PROMPTMFILE(
     &           'Enter name for gridded met input file',
     &           FSREAD3, 'MET_CRO_2D', PNAME )

C Get description of met file
         IF ( .NOT. DESC3( MNAME ) ) THEN
            MESG = 'Could not get description of file "'
     &           // TRIM( MNAME ) // '"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF

         METSTEP = TSTEP3D

C Check that grid description matches B3GRD file
         IF ( .NOT. CHKGRID( MNAME ) ) THEN
            MESG = 'Grid in file "' // TRIM( MNAME )
     &           // '" does not match grid in file ' // TRIM( MNAME ) // '"'
#ifdef twoway
            CALL M3WARN( PNAME, 0, 0, MESG )
#else
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
#endif
         END IF
#endif

         ALLOCATE( RNTOT( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'RNTOT', PNAME )

C Initial run until a full 24 hours has been recorded on the SOIL(OUT/INP) file
C for the Yienger and Levy algorithm
         WRITE( LOGDEV,'(/5X, A)' ) 'Temporal BEIS ...'
         RHOURS = MXRHRS

C If initial run, initialize some variables, otherwise get them from file
         IF ( NEW_START .or. IGNORE_SOILINP ) THEN

            PULSEDATE = 0   ! array
            PULSETIME = 0   ! array
            PTYPE     = 0   ! array

         END IF   ! initial run

         ALLOCATE( C_RAINFALL( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'RAINFALL', PNAME )
         C_RAINFALL = 0.0 ! array

         RDATE = STDATE; RTIME = STTIME
!        IHR = 0

      END IF   ! FIRSTIME

C Non-convective (RN) and convective (RC) rain is the total amount for the met
C preprocessor's (typically MCIP) output timestep (typically one hour). It doesn't
C make sense to time-interpolate these values, since rain generally does not fall
C at a constant rate for an output timestep.
      IF ( .NOT. CURRSTEP( JDATE, JTIME, STDATE, STTIME, METSTEP,
     &                     NDATE, NTIME ) ) THEN
         MESG = 'Cannot get step date and time'
         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT3 )
      END IF

C Store current time step rainfall totals
      IF ( NDATE .EQ. RDATE .AND. NTIME .EQ. RTIME ) THEN   ! on the METSTEP

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            IHR = MOD( RTIME / 10000 + 23, 24 )  ! previous hour bin,
                                                 ! to accommodate non-zero start hour
         END IF

C For the first hour of the simulation day, use the previous 24 hour
C data to compute precip adjustment

         NDX = 1 + MOD( IHR, MXRHRS )
         C_RAINFALL = C_RAINFALL + MET_DATA%RN + MET_DATA%RC
         WRITE( DDTTM( NDX ),'(I8,":",I6.6)' ) RDATE, RTIME

#ifdef mpas
            call mio_time_format_conversion (ndate, ntime, time_stamp)
#endif
         IF ( MOD( NTIME, 10000 ) .EQ. 0 ) THEN    ! at the hourly mark
            RAINFALL( :,:,NDX ) = C_RAINFALL
            C_RAINFALL = 0.0
            HRNO_SW( :,:,NDX )  = MET_DATA%RGRND
            HRNO_T2M( :,:,NDX ) = MET_DATA%TEMP2
            IHR = IHR + 1
#ifdef mpas
            call mio_fwrite ('MEGAN_SOILOUT', 'RAINFALL', pname,RAINFALL(:,1,ndx), time_stamp)
            call mio_fwrite ('MEGAN_SOILOUT', 'T24', pname, HRNO_T2M(:,1,ndx),time_stamp)
            call mio_fwrite ('MEGAN_SOILOUT', 'SW24', pname, HRNO_SW(:,1,ndx),time_stamp)
#endif
         END IF

!        CALL NEXTIME( RDATE, RTIME, TSTEP( 1 ) )
         CALL NEXTIME( RDATE, RTIME, METSTEP )

         RNTOT = 0.0   ! array assignment
         IF ( NEW_START .or. IGNORE_SOILINP) THEN
            IF ( IHR .LT. MXRHRS ) THEN
               INITIAL_DAY = .TRUE.
            ELSE
               INITIAL_DAY = .FALSE.
            END IF
         ELSE   ! store accumulated rain in RNTOT array
            DO I = 1, MXRHRS
               RNTOT = RNTOT + RAINFALL( :,:,I )
            END DO
         END IF

         write( logdev,* ) 'hrno - INITIAL_DAY, IHR: ', initial_day, ihr
#ifdef verbose_hrno

         write( logdev,* ) 'hrno - INITIAL_DAY, IHR: ', initial_day, ihr
#endif

      END IF   ! on the METSTEP


C-----------------------------------------------------------------------------
C--- Part 2: CALCULATING PRECADJ
C-----------------------------------------------------------------------------
      ! just calculate FAC2 always. MEGAN YL95 will work out if it's growseason

         DO R = 1, NROWS
            DO C = 1, NCOLS

               IF ( PX_LSM .OR. CLM_LSM .OR. NOAH_LSM ) THEN

                  IF ( INITIAL_DAY ) THEN
                     FAC2 = 1.0
                     PTYPE( C,R ) = 0
                     PULSEDATE( C,R ) = 0
                     PULSETIME( C,R ) = 0
                  ELSE
                     FAC2 = PRECIP_ADJ_PX( JDATE, JTIME, RNTOT( C,R ),
     &                                     MET_DATA%SOIM1( C,R ),
     &                                     Grid_Data%WSAT( C,R ), PTYPE( C,R ), 
     &                                     PULSEDATE( C,R ), PULSETIME( C,R ) )
                     PRECADJ( C,R) = FAC2
                  END IF

               ELSE

                  IF ( INITIAL_DAY ) THEN
                     FAC2 = 1.0
                     PTYPE( C,R ) = 0
                     PULSEDATE( C,R ) = 0
                     PULSETIME( C,R ) = 0
                  ELSE
                     FAC2 = PRECIP_ADJ( JDATE, JTIME, RNTOT( C,R ),
     &                                  PTYPE( C,R ), PULSEDATE( C,R ),
     &                                  PULSETIME( C,R ) )
                     PRECADJ( C,R) = FAC2
                  END IF

               END IF  ! PX version check

            END DO  ! columns
         END DO  ! rows

C-----------------------------------------------------------------------------
C--- Part 3: Writing to soilout file
C-----------------------------------------------------------------------------
#ifdef mpas
      IF ( MOD((TIME2SEC( JTIME ) - half_syn_step), output_step) .EQ. 0 .and. .not. l_desid_diag) then
      ! at the hourly mark
      ! continue to write
      else
        RETURN
      end if
#else
       IF ( SECSDIFF( JDATE,JTIME, EDATE,ETIME ) .GT. TIME2SEC( TSTEP( 2 ) ) .OR. L_DESID_DIAG ) RETURN
#endif


C Create rain data file for soil NO

C Final timestamp
      NDATE = EDATE; NTIME = ETIME

#ifndef mpas

C Build description for, and create/open soil NO emissions output file
      FTYPE3D = GRDDED3
      SDATE3D = NDATE
      STIME3D = NTIME
      TSTEP3D = 0   ! make it a time-independent file
      NCOLS3D = GL_NCOLS
      NROWS3D = GL_NROWS
      NLAYS3D = 1
      NVARS3D = 52 + RHOURS
      MXREC3D = 1
      NTHIK3D = 1
      GDTYP3D = GDTYP_GD
      P_ALP3D = P_ALP_GD
      P_BET3D = P_BET_GD
      P_GAM3D = P_GAM_GD
      XORIG3D = XORIG_GD
      YORIG3D = YORIG_GD
      XCENT3D = XCENT_GD
      YCENT3D = YCENT_GD
      XCELL3D = XCELL_GD
      YCELL3D = YCELL_GD
      VGTYP3D = VGTYP_GD
      VGTOP3D = VGTOP_GD
      DO L = 1, NLAYS3D + 1
         VGLVS3D( L ) = VGLVS_GD( L )
      END DO
      GDNAM3D = GRID_NAME  ! from HGRD_DEFN

      VNAME3D = ' '
      VNAME3D( 1 ) = 'PTYPE'
      VNAME3D( 2 ) = 'PULSEDATE'
      VNAME3D( 3 ) = 'PULSETIME'

      DO I = 1, RHOURS
         WRITE( VAR, '(A8,I2.2)' ) 'RAINFALL', I
         VNAME3D( I+3 ) = VAR
         WRITE( VAR, '(A2,I2.2)' ) 'SW', I
         VNAME3D( I+27 ) = VAR
         WRITE( VAR, '(A3,I2.2)' ) 'T2M', I
         VNAME3D( I+51 ) = VAR
      END DO

         VNAME3D( 52+RHOURS ) = 'LAI'

      UNITS3D = ' '
      UNITS3D( 1 ) = 'INTEGER'
      UNITS3D( 2 ) = 'YYYYDDD'
      UNITS3D( 3 ) = 'HHMMSS'
      UNITS3D( 4:RHOURS+3 ) = 'cm'

      VDESC3D( 1 ) = 'NO emission pulse type'
      VDESC3D( 2 ) = 'CMAQ starting date for NO emission pulse'
      VDESC3D( 3 ) = 'CMAQ starting time for NO emission pulse'
      VDESC3D( 4:RHOURS+3 ) = 'hourly convective and non-convective rainfall'
      DO I = 1, RHOURS
         VDESC3D( I+3 )  = DESCSTR // DDTTM( I )
         VDESC3D( I+27 ) = DESCSTRSW // DDTTM( I )
         VDESC3D( I+51 ) = DESCSTRT2M // DDTTM( I )
      END DO
         VDESC3D( I+52 ) = DESCSTRLAI

      VTYPE3D = 0
      VTYPE3D( 1 ) = M3INT
      VTYPE3D( 2 ) = M3INT
      VTYPE3D( 3 ) = M3INT
      VTYPE3D( 4:RHOURS+52 ) = M3REAL

      FDESC3D = ' '
      FDESC3D( 1 ) = 'Gridded rainfall data for soil NO emissions'
      FDESC3D( 2 ) = '/From/ ' // PNAME
      FDESC3D( 3 ) = '/Version/ CMAQ'

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

C Write soil NO rain data file

      VAR = 'PTYPE'
      IF ( .NOT. WRITE3( SOILOUT, VAR, NDATE, NTIME, PTYPE ) ) THEN
         MESG = 'Could not write "' // TRIM( VAR ) //
     &          '" to file "' // TRIM( SOILOUT ) // '"'
         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
      END IF

      VAR = 'PULSEDATE'
      IF ( .NOT. WRITE3( SOILOUT, VAR, NDATE, NTIME, PULSEDATE ) ) THEN
         MESG = 'Could not write "' // TRIM( VAR ) //
     &          '" to file "' // TRIM( SOILOUT ) // '"'
         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
      END IF

      VAR = 'PULSETIME'
      IF ( .NOT. WRITE3( SOILOUT, VAR, NDATE, NTIME, PULSETIME ) ) THEN
         MESG = 'Could not write "' // TRIM( VAR ) //
     &          '" to file "' // TRIM( SOILOUT ) // '"'
         CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
      END IF

      NDX = 1 + MOD( IHR, MXRHRS )
      RAINFALL( :,:,NDX ) = C_RAINFALL
      DO I = 1, RHOURS
         WRITE( VAR, '(A8,I2.2)' ) 'RAINFALL', I
         IF ( .NOT. WRITE3( SOILOUT, VAR, NDATE, NTIME, RAINFALL( :,:,I ) ) ) THEN
            MESG = 'Could not write "' // TRIM( VAR ) //
     &             '" to file "' // TRIM( SOILOUT ) // '"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF
         WRITE( VAR, '(A2,I2.2)' ) 'SW', I
         IF ( .NOT. WRITE3( SOILOUT, VAR, NDATE, NTIME, HRNO_SW( :,:,I ) ) ) THEN
            MESG = 'Could not write "' // TRIM( VAR ) //
     &             '" to file "' // TRIM( SOILOUT ) // '"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF
         WRITE( VAR, '(A3,I2.2)' ) 'T2M', I
         IF ( .NOT. WRITE3( SOILOUT, VAR, NDATE, NTIME, HRNO_T2M( :,:,I ) ) ) THEN
            MESG = 'Could not write "' // TRIM( VAR ) //
     &             '" to file "' // TRIM( SOILOUT ) // '"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF
      END DO

         IF ( .NOT. WRITE3( SOILOUT, 'LAI', NDATE, NTIME, Met_data%LAI )) THEN
            MESG = 'Could not write "' // TRIM( VAR ) //
     &             '" to file "' // TRIM( SOILOUT ) // '"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, XSTAT2 )
         END IF

#else
         ! write MPAS end of day variables ==jwilliso
            call mio_fwrite ('MEGAN_SOILOUT', 'PTYPE', pname,PTYPE(:,1),time_stamp)
            call mio_fwrite ('MEGAN_SOILOUT', 'PULSEDATE', pname, PULSEDATE(:,1),time_stamp)
            call mio_fwrite ('MEGAN_SOILOUT', 'PULSETIME', pname, PULSETIME(:,1),time_stamp)
            call mio_fwrite ('MEGAN_SOILOUT', 'LAI', pname,Met_data%LAI(:,1),time_stamp)
#endif

      WRITE( LOGDEV,94040 )
     &      'Timestep written to', SOILOUT,
     &      'for date and time', NDATE, NTIME

      RETURN

94010 FORMAT( A, F10.2, 1X, A, I3, ',', I3 )
94040 FORMAT( /5X, 3( A, :, 1X ), I8, ":", I6.6 )

C-----------------------------------------------------------------------

      CONTAINS

         REAL FUNCTION PRECIP_ADJ_PX( JDATE, JTIME, RAIN, SOILM, WSAT,
     &                                PTYPE, PULSEDATE, PULSETIME )

C-----------------------------------------------------------------------
 
C Description:
   
C    Compute precipitation adjustment factor for estimate of NO emissions 
C    Uses: julian day, time, soil moisture
C    Requires the use of three arrays that are re-used each time step:
C    PTYPE, PULSEDATE, PULSETIME 
C    These arrays store the type of NO pulse initiated by the rainfall
C    and the starting date and time of the pulse.
 
C Preconditions:
C    Soil Moisture current time, Soil Moisture previous time,
C    Soil type, Land Use, PTYPE, PULSEDATE, PULSETIME 
 
C Subroutines and Functions Called:
C    precipfact - computes precip adjustment factor from rainrate and time
C                 since pulse initiation
C    pulsetype  - determines type & duration of NO emission pulse from rainrate
 
C Revision History:
C    11/01 : Prototype by GAP
C    3/05  : create separate functions for PX vs non-PX versions
C    1/10  : J.Young - restructure
C    7/31/19 J. Pleim : Corrected Soil Types and Simplified Code
C-----------------------------------------------------------------------

         USE UTILIO_DEFN

         IMPLICIT NONE

C Function arguments:
         INTEGER, INTENT( IN )    :: JDATE, JTIME
         REAL,    INTENT( IN )    :: RAIN
         REAL,    INTENT( IN )    :: SOILM     ! only avilable if PX version
         REAL,    INTENT( IN )    :: WSAT      ! only tested for PX and CLM versions         
         INTEGER, INTENT( INOUT ) :: PTYPE     ! pulse type
         INTEGER, INTENT( INOUT ) :: PULSEDATE ! date of pulse start
         INTEGER, INTENT( INOUT ) :: PULSETIME ! date of pulse end

C External functions:
         
C Parameters:
         REAL, PARAMETER :: SAT_THRES = 0.95

C Local variables:
         INTEGER SOILCAT     ! soil type category
         INTEGER PTYPE_TEST

C-----------------------------------------------------------------------

C Summary of algorithm
C   1. compute rate of change of soil moisture from soil moisture
C   2. estimate rainrate from soil moisture and soil moisture rate
C   3. compute adjustment using pulsetype, rainrate, ptype, and date/time
C        if stronger NO pulse compared to previous time step, then
C        start a new NO emission pulse,
C        otherwise continue present NO pulse
C   4. override adjustment for saturated soils 

         SOILCAT = GRID_DATA%SLTYP( C,R )
         IF ( SOILCAT .NE. LSM_WATER  ) THEN 
            IF ( SOILM .GE. SAT_THRES * WSAT ) THEN
               PRECIP_ADJ_PX = 0.0
            ELSE
               PTYPE_TEST = PULSETYPE( RAIN )
               IF ( PTYPE_TEST .GT. PTYPE ) THEN ! Rainfall class type increases
                  PULSEDATE = JDATE              ! (NO emission pulse generated)
                  PULSETIME = JTIME
                  PTYPE = PTYPE_TEST
               END IF
               PRECIP_ADJ_PX = PRECIPFAC( JDATE, JTIME, PULSEDATE, PULSETIME, PTYPE )
            END IF
         ELSE
            PRECIP_ADJ_PX = 0.0
         END IF

         RETURN
         
         END FUNCTION PRECIP_ADJ_PX
         
C-----------------------------------------------------------------------

         REAL FUNCTION PRECIP_ADJ( JDATE, JTIME, RAIN,
     &                             PTYPE, PULSEDATE, PULSETIME )

C-----------------------------------------------------------------------
C Description:
   
C    Compute precipitation adjustment factor for estimate of NO emissions 
C    Uses: julian day, time, soil moisture
C    Requires the use of three arrays that are re-used each time step:
C    PTYPE, PULSEDATE, PULSETIME 
C    These arrays store the type of NO pulse initiated by the rainfall
C    and the starting date and time of the pulse.
 
C Preconditions:
C    Soil Moisture current time, Soil Moisture previous time,
C    Soil type, Land Use, PTYPE, PULSEDATE, PULSETIME 
 
C Subroutines and Functions Called:
C    precipfact - computes precip adjustment factor from rainrate and time
C                 since pulse initiation
C    pulsetype  - determines type & duration of NO emission pulse from rainrate
 
C Revision History:
C    11/01 : Prototype by GAP
C    3/05  : created a non-PX version of this function 
C    1/10  : J.Young - restructure
  
C-----------------------------------------------------------------------

         USE UTILIO_DEFN

         IMPLICIT NONE

C Function arguments:
         INTEGER, INTENT( IN )    :: JDATE, JTIME
         REAL,    INTENT( IN )    :: RAIN
         INTEGER, INTENT( INOUT ) :: PTYPE     ! pulse type
         INTEGER, INTENT( INOUT ) :: PULSEDATE ! date of pulse start
         INTEGER, INTENT( INOUT ) :: PULSETIME ! time of pulse start

C External functions:

C Local variable
         INTEGER PTYPE_TEST

C-----------------------------------------------------------------------

C Summary of algorithm
C    1. if no rainfall or new rainfall class less than current one, continue
C       existing NO emission pulse
C    2. if new rainfall that increases rainfall class, then create new NO
C       emission pulse using pulsetype, rainrate, ptype, and date/time -
C       if stronger NO pulse compared to previous time step, then start
C       a new NO emission pulse

         PTYPE_TEST = PULSETYPE( RAIN )
         IF ( PTYPE_TEST .GT. PTYPE ) THEN ! Rainfall class type increases
            PULSEDATE = JDATE              ! (NO emission pulse generated)
            PULSETIME = JTIME
            PTYPE = PTYPE_TEST
         END IF

         PRECIP_ADJ = PRECIPFAC( JDATE, JTIME, PULSEDATE, PULSETIME, PTYPE )

         RETURN
         
         END FUNCTION PRECIP_ADJ

C-----------------------------------------------------------------------

         REAL FUNCTION PRECIPFAC( JDATE, JTIME, PDATE, PTIME, PTYPE )

C Compute a precipitation adjustment factor from a previous 24 hour rainfall
C based on YL 1995
C The pulse type is an integer ranging from 0 to 3 indicating the type of
C rainfall rate:
C If rainfall < 0.1 cm in last 24 hr, "reset"
C Else if rainfall < 0.5 cm in last 24 hr, and time since last pulse is .ge. 2 days,
C    reset; else, precipfact=11.19*...
C Else if rainfall < 1.5 cm in last 24 hr, and time since last pulse is .ge. 6 days,
C    reset; else, precipfact=14.68*...
C Else if rainfall >=1.5 cm in last 24 hr, and time since last pulse is .ge. 13 days,
C    reset; else, precipfact=18.46*...

         USE UTILIO_DEFN

         IMPLICIT NONE
         
C Function arguments:
         INTEGER, INTENT( IN )    :: JDATE, JTIME, PDATE, PTIME
         INTEGER, INTENT( INOUT ) :: PTYPE
         
C External functions:

C Parameters:
         REAL, PARAMETER :: DAYPERSEC = 1.0 / ( 24.0 * 3600.0 ) ! = 0.000011574074074

C Local variables:
         REAL DAYDIFF, DAYDIF1
         
C-----------------------------------------------------------------------

         DAYDIFF = FLOAT( SECSDIFF( PDATE, PTIME, JDATE, JTIME ) ) * DAYPERSEC
         DAYDIF1 = DAYDIFF + 1.0
         
         SELECT CASE( PTYPE )
         CASE( 0 )
            PRECIPFAC = 1.0
         CASE( 1 )
            IF ( ( DAYDIFF ) .LT. 2.0 ) THEN
               PRECIPFAC = 11.19 * EXP( -0.805 * DAYDIF1 )
            ELSE
               PTYPE = 0
               PRECIPFAC = 1.0
            END IF
         CASE( 2 )
            IF ( ( DAYDIFF ) .LT. 6.0 ) THEN
               PRECIPFAC = 14.68 * EXP( -0.384 * DAYDIF1 )
            ELSE
               PTYPE = 0
               PRECIPFAC = 1.0
            END IF
         CASE( 3 )
            IF ( ( DAYDIFF ) .LT. 13.0 ) THEN
               PRECIPFAC = 18.46 * EXP( -0.208 * DAYDIF1 )
            ELSE
               PTYPE = 0
               PRECIPFAC = 1.0
            END IF
         CASE DEFAULT
            WRITE( MESG,'( A, I6 )' ) 'Invalid Pulse Type specified ',
     &                                 PTYPE
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
         END SELECT
         
         RETURN
         
         END FUNCTION PRECIPFAC
    
C-----------------------------------------------------------------------

         INTEGER FUNCTION PULSETYPE( RAIN )

C Compute the pulse type from the rainfall rate (see YL 1995).

         IMPLICIT NONE
         
C Function arguments
         REAL, INTENT( IN ) :: RAIN   ! [cm/24hr]
         
C-----------------------------------------------------------------------

         IF ( RAIN .LT. 0.1 ) THEN
            PULSETYPE = 0
         ELSE IF ( RAIN .LT. 0.5 ) THEN
            PULSETYPE = 1
         ELSE IF ( RAIN .LT. 1.5 ) THEN
            PULSETYPE = 2
         ELSE
            PULSETYPE = 3
         END IF
         
         RETURN
         
         END FUNCTION PULSETYPE

C-----------------------------------------------------------------------

      END SUBROUTINE MEGAN_HRNO

      END MODULE MEGAN_HRNO_MOD
