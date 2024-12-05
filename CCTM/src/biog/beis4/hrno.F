
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

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE HRNO( JDATE, JTIME, TSTEP, NOPOL, L_DESID_DIAG )

C-----------------------------------------------------------------------
C Description:
   
C    Uses new NO algorithm NO = Normalized*Tadj*Padj*Fadj*Cadj
C    to estimate NO emissions 
C    Information needed to estimate NO emissions:
C       Julian Day          (integer)    JDATE
C       Surface Temperature (MCIP field) TA    (K)
C       Rainfall    (MCIP derived field) RAIN  (cm)
C       Soil Moisture       (MCIP field) SOILM (M**3/M**3) (PX_LSM)
C            (ratio of volume of water per volume of soil)
C       Soil Temperature    (MCIP field) SOILT (K)         (PX_LSM)
C       Soil Type           (MCIP field) ISLTYP            (PX_LSM)
C       Saturation values for soil types (constants)       (PX_LSM)
C    FOR PX Version, the Temperature adjustment factor accounts for wet and dry
C    soils and the precipitation adjustment factor accounts for saturated soils
C    FOR the non-PX version, the basic algorithm remains with a temperature
C    adjustment factor (dry soil) and no adjustment for saturated soils
 
C    The following arrays are potentially updated after a call to HRNO:
C       PTYPE     type of NO emission pulse 
C       PULSEDATE julian date for the beginning of an NO pulse 
C       PULSETIME        time for the beginning of an NO pulse
   
C    The calculation are based on the following paper:
C    J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,
C    11447-11464, 1995
 
C    The Temperature Adjustment Factor is based on section 4.2 for wet and dry
C    soils with the following modification (PX version):
C       Instead of classifying soils as either 'wet' or 'dry', the wet and dry
C       adjustment is calculated at each grid cell.  A linear interpolation between
C       the wet and dry adjustment factor is made using the relative amount of soil
C       moisture in the top layer (1cm) as the interpolating factor.  The relative
C       amount of soil moisture is determined by taking the MCIP soil moisture field
C       and dividing by the saturation value defined for each soil type in the PX
C       version of MCIP. The soil temperature is used in PX version
 
C    The Precipation Adjustment factor is based on section 4.1 with the following
C    modifications:
C       The rainrate is computed from the MCIP directly using a 24 hr daily total. 
C       The types of Pulses as described in YL95 were used to estimate the NO
C       emission rate.  
 
C    Also see the following paper for more information:
C    Proceedings of the Air and Waste Management Association/U.S. Environmental
C    Protection Agency EMission Inventory Conference, Raleigh October 26-28, 1999
C    Raleigh NC by Tom Pierce and Lucille Bender       
 
C    References:
 
C    Jacquemin B. and Noilhan J. (1990), Bound.-Layer Meteorol., 52, 93-134.
C    J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,
C    11447-11464, 1995
C    T. Pierce and L. Bender, Examining the Temporal Variability of Ammonia and
C    Nitric Oxide Emissions from Agricultural Processes Proceedings of the Air and
C    Waste Management Association/U.S. Environmental Protection Agency Emission
C    Inventory Conference, Raleigh October 26-28, 1999 Raleigh NC

C Preconditions:
C     Normalized NO emissions, Surface Temperature, Soil Moisture, Soil type,
C     NO emission pulse type, soil moisture from previous time step, julian date
C     of NO emission pulse start, time of NO emission pulse start,
C     soil type, SOIL TYPES, Land use data
 
C Subroutines and Functions Called (directly or indirectly):
C     PRECIP_ADJ     computes precipitation adjustment factor
C     FERTILIZER_ADJ computes fertlizer adjustment factor
C     VEG_ADJ        computes vegetation adjustment factor
C     GROWSEASON     computes Julian day of growing season
C     PRECIPFAC      computes precip adjustment factor from rainfall in last 24 hrs
C                    and time since pulse initiation
C     PULSETYPE      determines type & duration of NO emission pulse from rainrate
      
C Revision History:
C    10/01: Prototype by GAP
C    10/03: modified transition to non growing season for jul-oct of the year
C    08/04: Converted to SMOKE code style by C Seppanen
C   Mar 07: Restructure; J.Young
C   Jan 26: J.Young - move input data reads from tmpbeis;
C                     remove ck & report if TAIR > 315;
C                     restructure growing season, col/row loops;
C                     restructure PRECIP_ADJ
C   Jan 27: D. Wong - Eliminate potential race condition with mype = 0 and
C                     barrier implementation - not needed anyway.
C    02/11: S.Roselle-Replaced I/O API include files with UTILIO_DEFN
C    05/11: D. Wong - incorporated twoway model implementation
C    05/12: J.Young - make PX_LSM the default
C    05/13: D.Wong  - replaced M3EXIT with M3WARN when CHKGRID is called in the twoway model
C    09/13: D.Wong  - made the code more robust by
C                     * allowing simulation start time at any hour rather than 0
C                     * allowing finer met data time step and maintaining an hourly
C                       rainfall bucket
C    07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C    Aug 15 D.Wong:    Replaced MYPE with IO_PE_INCLUSIVE for parallel I/O 
C                      implementation and added code to handle parallel I/O
C    09 Jan 15 J.Young: fix bug of not calculating NOPOL for last sync step of last
C                       run output step; get saturation from LSM module; put PX_LSM
C                       as first conditional clause
C    01 Feb 19 D.Wong: Implemented centralized I/O approach
C    30 June 19 J. Pleim: Corrected Soil Types to 16 and simplified code 
C-----------------------------------------------------------------------
C Modified from:

C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling System
C File: @(#)$Id: hrno.F,v 1.6 2011/10/21 16:10:18 yoj Exp $
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C smoke@unc.edu
C Pathname: $Source: /project/yoj/arc/CCTM/src/biog/beis3/hrno.F,v $
C Last updated: $Date: 2011/10/21 16:10:18 $ 
C-----------------------------------------------------------------------
      USE RUNTIME_VARS
      USE HGRD_DEFN             ! horizontal domain specifications
      USE BIOG_EMIS, ONLY: NSEF ! beis
      USE ASX_DATA_MOD
      USE UTILIO_DEFN
#ifdef mpas
      use mio_module
      USE util_module, only : NEXTIME, TIME2SEC, SECSDIFF, CURRSTEP, julian
#else
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
      INTEGER, INTENT( IN )  :: JDATE         ! current simulation date (YYYYDDD)
      INTEGER, INTENT( IN )  :: JTIME         ! current simulation time (HHMMSS)
      INTEGER, INTENT( IN )  :: TSTEP( 3 )    ! time step vector (HHMMSS)
      REAL,    INTENT( OUT ) :: NOPOL( :,: )  ! output NO emissions
      LOGICAL, INTENT( IN )  :: L_DESID_DIAG  ! flag determining whether or not DESID
                                              !   is in diagnostic mode              
C External Functions
      LOGICAL,         EXTERNAL :: CHKGRID

C Parameters:
      INTEGER, PARAMETER :: MXRHRS = 24     ! no. of rainfall hours for YL95 algorithm
      INTEGER, PARAMETER :: LSM_WATER = 14
      REAL,    PARAMETER :: CFNODRYFC = ( 1.0 / 3.0 ) * ( 1.0 / 30.0 )
        
C Saturation values for 11 soil types from pxpbl.F  (MCIP PX version)
C In LSM_MOD:WSAT
C Pleim-Xiu Land-Surface and PBL Model (PX-LSM)
C See Jacquemin B. and Noilhan J. (1990), Bound.-Layer Meteorol., 52, 93-134.

C Local Variables:

      CHARACTER( 16 ), SAVE :: MNAME   ! logical name for MET_CRO_2D
      CHARACTER( 16 ), SAVE :: SOILINP ! logical name for input NO soil data
      CHARACTER( 16 ), SAVE :: SOILOUT = 'BEIS_SOILOUT' ! logical name for output NO soil data
      CHARACTER( 33 ), SAVE :: DESCSTR = 'hrly cnv. & non-cnv. rainfall for'

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
      LOGICAL          OK
      INTEGER          IOS                ! IO or memory allocation status
      INTEGER, SAVE :: METSTEP            ! met_cro_2d time step
      
      REAL,    SAVE :: EFAC
      REAL             CFNO               ! NO correction factor
      REAL             CFNOGRASS          ! NO correction factor for grasslands
      REAL             TAIR               ! surface temperature
      REAL             TSOI               ! soil temperature
      REAL             CFNOWET, CFNODRY, RATIO
      REAL             FAC1, FAC2, FAC3, FAC4

      LOGICAL, SAVE :: USE_SOILT = .TRUE. ! use soil temperature in PX version
                                          ! rather than estimate as in BEIS2

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      LOGICAL, SAVE :: FIRSTHR  = .TRUE.
      LOGICAL, SAVE :: FIRST_WRITE = .TRUE.
      CHARACTER( 256 ) :: MESG            ! message buffer
      CHARACTER( 16 )  :: PNAME = 'HRNO'  ! procedure name

      INTEGER      GXOFF, GYOFF           ! global origin offset from file
C for INTERPX
      INTEGER       :: STRTCOLSIN, ENDCOLSIN, STRTROWSIN, ENDROWSIN  ! SOILINP

      CHARACTER( 16 ), SAVE :: VNAME_RC, VNAME_RN
      INTEGER :: SPC

#ifdef mpas
      integer :: io_mode
#endif

      LOGICAL, EXTERNAL :: FLUSH3

#ifdef verbose_hrno
      integer mxptype, ncfno0
      real    mxrntot, mxfac1, avgfac2, mxfac3, mxfac4
      real    mxtair, mxcfno
#endif
C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
!        FIRSTIME = .FALSE.

#ifdef mpas
         if (ncd_64bit_offset) then
            io_mode = ior (nf90_noclobber, nf90_64bit_offset)
         else
            io_mode = nf90_noclobber
         end if

         call mio_fcreate ('SOILOUT', io_mode)
#endif

C Determine last timestamp
         EDATE = STDATE; ETIME = STTIME
         CALL NEXTIME( EDATE, ETIME, RUNLEN )   ! end date & time
         MSTEPS = TIME2SEC( RUNLEN ) / TIME2SEC( TSTEP( 1 ) )

#ifdef mpas
         METSTEP = tstep(3)
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

C Check that grid description matches MET_CRO_2D file
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
         IF ( NEW_START ) THEN

            PULSEDATE = 0   ! array
            PULSETIME = 0   ! array
            PTYPE     = 0   ! array

         END IF   ! initial run

         ALLOCATE( C_RAINFALL( NCOLS,NROWS ), STAT=IOS )
         CALL CHECKMEM( IOS, 'RAINFALL', PNAME )
         C_RAINFALL = 0.0 ! array

         RDATE = STDATE; RTIME = STTIME
!        IHR = 0
         EFAC = EXP( -0.103 * 30.0 )

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

         IF ( MOD( NTIME, 10000 ) .EQ. 0 ) THEN    ! at the hourly mark
            RAINFALL( :,:,NDX ) = C_RAINFALL
            C_RAINFALL = 0.0
            IHR = IHR + 1
         END IF

!        CALL NEXTIME( RDATE, RTIME, TSTEP( 1 ) )
         CALL NEXTIME( RDATE, RTIME, METSTEP )

         RNTOT = 0.0   ! array assignment
         IF ( NEW_START ) THEN
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

#ifdef verbose_hrno
         write( logdev,* ) 'hrno - INITIAL_DAY, IHR: ', initial_day, ihr
#endif

      END IF   ! on the METSTEP

C Calculate temporal non-speciated soil NO emissions
 
      IF ( GROWSEASON( JDATE ) .EQ. 0 ) THEN   ! not growing season

#ifdef verbose_hrno
         write( logdev,* ) ' not growing season'
         mxtair = 0.0; mxcfno = 0.0; ncfno0 = 0
#endif

         DO R = 1, NROWS
            DO C = 1, NCOLS
            
               TAIR = MET_DATA%TEMP2( C,R )   ! unit in degree K

C Check min bounds for temperature and limit to 303 deg K
               IF ( TAIR .LT. 200.0 ) THEN
                  WRITE( MESG, 94010 ) 'TAIR=', TAIR,
     &                 'out of range at (C,R)=', C, R
                  CALL M3MESG( MESG )
                  Write( MESG,94010 ) 'Max TAIR: ', Maxval( MET_DATA%TEMP2 ),
     &                 'at (C,R)', Maxloc( MET_DATA%TEMP2 )
                  CALL M3MESG( MESG )
                  Write( MESG,94010 ) 'Min TAIR: ', Minval( MET_DATA%TEMP2 ),
     &                 'at (C,R)', Minloc( MET_DATA%TEMP2 )
                  CALL M3MESG( MESG )
                  CALL M3EXIT( PNAME, JDATE, JTIME, '*** ABORT', 2 )
               END IF
               TAIR = MIN( TAIR, 303.0 )

               IF ( TAIR .GT. 268.8690 ) THEN
                  CFNO = EXP( 0.04686 * TAIR - 14.30579 ) ! grass (from BEIS2)
               ELSE
                  CFNO = 0.0
#ifdef verbose_hrno
                  ncfno0 = ncfno0 + 1
#endif
               END IF

#ifdef verbose_hrno
               mxtair = max( mxtair, tair )
               mxcfno = max( mxcfno, cfno )
#endif

               NOPOL( C,R ) = CFNO * ( NGROWAGNO( C,R )  ! agriculture
     &                             +   NONAGNO( C,R ) )  ! non-agriculture

            END DO  ! columns
         END DO  ! rows

#ifdef verbose_hrno
      write( logdev,2009 ) ncfno0, mxtair, mxcfno
2009  format( ' hrno - NCFNO0, MAX TAIR,CFNO: ', i7, 2f10.5 )
#endif

      ELSE   ! growing season

#ifdef verbose_hrno
         mxptype = 0; mxrntot = 0.0
         mxfac1 = 0.0; avgfac2 = 0.0; mxfac3 = 0.0; mxfac4 = 0.0
#endif

         DO R = 1, NROWS
            DO C = 1, NCOLS
            
               TAIR = MET_DATA%TEMP2( C,R )   ! unit in degree K

C Check min bounds for temperature and limit max to 303 deg K
               IF ( TAIR .LT. 200.0 ) THEN
                  WRITE( MESG, 94010 ) 'TAIR=', TAIR,
     &                 'out of range at (C,R)=', C, R
                  CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
               END IF
               TAIR = MIN( TAIR, 303.0 )

C Calculate NO emissions by going thru temperature cases

               IF ( PX_LSM .OR. CLM_LSM .OR. NOAH_LSM ) THEN

                  SOILCAT = GRID_DATA%SLTYP( C,R )
                  TSOI = 0.0
                  IF( SOILCAT .NE. LSM_WATER  ) THEN
                     RATIO = MET_DATA%SOIM1( C,R ) / Grid_Data%WSAT( C,R )
                     IF ( USE_SOILT ) THEN
                        TSOI = MET_DATA%SOIT1( C,R )
                     ELSE
                        TSOI = 0.72 * TAIR + 82.28
                     END IF
                     TSOI = MIN( MAX( TSOI, 273.16 ), 303.16 ) - 273.16   ! deg C
                     CFNODRY = CFNODRYFC * TSOI ! see YL 1995 Eqn 9a p. 11452

                     IF ( TSOI .LE. 10.0 ) THEN ! see YL 1995 Eqn 7b
                        CFNOWET = 0.28 * EFAC * TSOI   ! linear cold case
                     ELSE
                        CFNOWET = EFAC * EXP( 0.103 * TSOI )   ! exponential case
                     END IF

                     CFNO = CFNODRY + RATIO * ( CFNOWET - CFNODRY )
                     FAC1 = GROWAGNO( C,R ) * CFNO
     &                    * FERTILIZER_ADJ( JDATE )
     &                    * VEG_ADJ( JDATE )

                  ELSE
                     FAC1 = 0.0
                  END IF

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
                  END IF

               ELSE

                  TSOI = 0.72 * TAIR + 82.28
                  TSOI = MIN( MAX( TSOI, 273.16 ), 303.16 ) - 273.16   ! deg C
                  CFNODRY = CFNODRYFC * TSOI ! see YL 1995 Eqn 9a p. 11452
                  IF ( TSOI .LE. 10.0 ) THEN ! see YL 1995 Eqn 7b
                     CFNOWET = 0.28 * EFAC * TSOI   ! linear cold case
                  ELSE
                     CFNOWET = EFAC * EXP( 0.103 * TSOI )   ! exponential case
                  END IF
                  CFNO = 0.5 * ( CFNOWET + CFNODRY )

                  FAC1 = GROWAGNO( C,R ) * CFNO
     &                 * FERTILIZER_ADJ( JDATE )
     &                 * VEG_ADJ( JDATE )
     
                  IF ( INITIAL_DAY ) THEN
                     FAC2 = 1.0
                     PTYPE( C,R ) = 0
                     PULSEDATE( C,R ) = 0
                     PULSETIME( C,R ) = 0
                  ELSE
                     FAC2 = PRECIP_ADJ( JDATE, JTIME, RNTOT( C,R ),
     &                                  PTYPE( C,R ), PULSEDATE( C,R ),
     &                                  PULSETIME( C,R ) )
                  END IF

               END IF  ! PX version check

               IF ( TAIR .GT. 268.8690 ) THEN  
                  CFNOGRASS = EXP( 0.04686 * TAIR - 14.30579 ) ! grass (from BEIS2)
                  FAC3 = NGROWAGNO( C,R ) * CFNOGRASS 
                  FAC4 = NONAGNO( C,R ) * CFNOGRASS
               ELSE
                  FAC3 = 0.0
                  FAC4 = 0.0
               END IF

#ifdef verbose_hrno
               mxptype = max( mxptype, ptype( c,r ) )
               mxrntot = max( mxrntot, rntot( c,r ) )
               mxfac1 = max( mxfac1, fac1 )
               avgfac2 = avgfac2 + fac2
               mxfac3 = max( mxfac3, fac3 )
               mxfac4 = max( mxfac4, fac4 )
#endif
               IF( TSOI .LE. 0.0 ) THEN
                  NOPOL( C,R ) = 0.0
               ELSE 
                  NOPOL( C,R ) = MAX( ( FAC1 * FAC2 ), FAC3 ) + FAC4
               END IF

            END DO  ! columns
         END DO  ! rows

#ifdef verbose_hrno
      write( logdev,2011 ) mxptype, mxrntot, avgfac2/(ncols*nrows), mxfac1, mxfac3, mxfac4
2011  format( ' hrno MAX PTYPE,RNTOT,AVGFAC2,FAC1,3,4: ', I2, 2f8.3 , 3f10.3 )
#endif

      END IF  ! growing season check

      IF ( SECSDIFF( JDATE,JTIME, EDATE,ETIME ) .GT. TIME2SEC( TSTEP( 2 ) ) .OR. L_DESID_DIAG ) RETURN

#ifdef mpas
      var = 'PTYPE'
      call mio_fwrite ('SOILOUT', var, pname, ptype)

      var = 'PULSEDATE'
      call mio_fwrite ('SOILOUT', var, pname, pulsedate)

      var = 'PULSETIME'
      call mio_fwrite ('SOILOUT', var, pname, pulsetime)

      do i = 1, rhours
         write( var, '(a8,i2.2)' ) 'RAINFALL', I
         call mio_fwrite ('SOILOUT', var, pname, rainfall(1,1,i))
      end do
#else

C Create rain data file for soil NO

C Final timestamp
      NDATE = EDATE; NTIME = ETIME

C Build description for, and create/open soil NO emissions output file
      FTYPE3D = GRDDED3
      SDATE3D = NDATE
      STIME3D = NTIME
      TSTEP3D = 0   ! make it a time-independent file
      NCOLS3D = GL_NCOLS
      NROWS3D = GL_NROWS
      NLAYS3D = 1
      NVARS3D = 3 + RHOURS
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
      END DO

      UNITS3D = ' '
      UNITS3D( 1 ) = 'INTEGER'
      UNITS3D( 2 ) = 'YYYYDDD'
      UNITS3D( 3 ) = 'HHMMSS'
      UNITS3D( 4:RHOURS+3 ) = 'cm'

      VDESC3D( 1 ) = 'NO emission pulse type'
      VDESC3D( 2 ) = 'CMAQ starting date for NO emission pulse'
      VDESC3D( 3 ) = 'CMAQ starting time for NO emission pulse'
!     VDESC3D( 4:RHOURS+3 ) = 'hourly convective and non-convective rainfall'
      DO I = 1, RHOURS
         VDESC3D( I+3 ) = DESCSTR // DDTTM( I )
      END DO

      VTYPE3D = 0
      VTYPE3D( 1 ) = M3INT
      VTYPE3D( 2 ) = M3INT
      VTYPE3D( 3 ) = M3INT
      VTYPE3D( 4:RHOURS+3 ) = M3REAL

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
      END DO

      WRITE( LOGDEV,94040 )
     &      'Timestep written to', SOILOUT,
     &      'for date and time', NDATE, NTIME
#endif

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

         REAL FUNCTION FERTILIZER_ADJ( DATE )

C Compute a fertilizer adjustment factor for the given date in yyyyddd format.
C If it is not growing season, the adjustment factor is 0; otherwise, it
C ranges from 0.0 to 1.0.

         IMPLICIT NONE
         
C Function arguments:
         INTEGER, INTENT( IN ) :: DATE

C Local variables:
         INTEGER  GDAY

C-----------------------------------------------------------------------

         GDAY = GROWSEASON( DATE )
         
         IF ( GDAY .EQ. 0 ) THEN
            FERTILIZER_ADJ = 0.0
         ELSE IF ( GDAY .GE. 1 .AND. GDAY .LT. 30 ) THEN  ! first month of growing season
            FERTILIZER_ADJ = 1.0
         ELSE IF ( GDAY .GE. 30 ) THEN
            FERTILIZER_ADJ = 1.0 + 30.0 / 184.0 - FLOAT( GDAY ) / 184.0
         ELSE
            WRITE( MESG,94010 ) 'Invalid date specified; date = ', DATE,
     &                          'growing season day = ', GDAY
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
         END IF

94010    FORMAT( A, I8, 1X, A, I3 )
         
         RETURN
         
         END FUNCTION FERTILIZER_ADJ

C-----------------------------------------------------------------------

         REAL FUNCTION VEG_ADJ( DATE )

C Compute a vegetation adjustment factor for the given date in yyyyddd format.
C The adjustment factor ranges from 0.5 to 1.0.

         IMPLICIT NONE
         
C Function arguments:
         INTEGER, INTENT( IN ) :: DATE

C Local variables:
         INTEGER  GDAY

C-----------------------------------------------------------------------

         GDAY = GROWSEASON( DATE )
         
         IF ( GDAY .LE. 30 ) THEN
            VEG_ADJ = 1.0
         ELSE IF ( GDAY .GT. 30 .AND. GDAY .LT. 60 ) THEN
            VEG_ADJ = 1.5 - ( FLOAT( GDAY ) / 60.0 )
         ELSE IF ( GDAY .GE. 60 ) THEN
            VEG_ADJ = 0.5
         ELSE
            WRITE( MESG,94010 ) 'Invalid date specified; date = ', DATE,
     &                          'growing season day = ', GDAY
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
         END IF

94010    FORMAT( A, I8, 1X, A, I3 )

         RETURN
      
         END FUNCTION VEG_ADJ
         
C-----------------------------------------------------------------------

         INTEGER FUNCTION GROWSEASON( DATE )

C Compute the day of the growing season corresponding to the given date
C in yyyyddd format.

         USE UTILIO_DEFN

         IMPLICIT NONE
         
C Function arguments:
         INTEGER, INTENT( IN ) :: DATE   ! YYYYDDD

C External functions:

C Parameters:
         INTEGER, PARAMETER :: GSTART_MONTH  = 04     ! April
         INTEGER, PARAMETER :: GSTART_DAY    = 01
         INTEGER, PARAMETER :: GEND_MONTH    = 10     ! October
         INTEGER, PARAMETER :: GEND_DAY      = 31

C Local variables:
         INTEGER  YEAR, MONTH, DAY
         INTEGER  JDAY, GDAY
         INTEGER  GSJULIAN_START
         INTEGER  GSJULIAN_END
         
C-----------------------------------------------------------------------

         YEAR = INT( FLOAT( DATE ) / 1000.0 )
         JDAY = DATE - YEAR * 1000
         
         GSJULIAN_START = JULIAN( YEAR, GSTART_MONTH, GSTART_DAY )
         GSJULIAN_END = JULIAN( YEAR, GEND_MONTH, GEND_DAY )
         
         IF ( JDAY .GE. GSJULIAN_START .AND. JDAY .LE. GSJULIAN_END ) THEN
            GROWSEASON = JDAY - GSJULIAN_START + 1  ! growing season
         ELSE IF ( JDAY .GE. 1 .AND. JDAY .LE. 366 ) THEN
            GROWSEASON = 0                          ! before or after growing season
         ELSE
            WRITE( MESG,94010 ) 'Invalid date specified; date = ', DATE,
     &                          'jday = ', JDAY
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
         END IF

94010    FORMAT( A, I8, 1X, A, I3 )

         RETURN

         END FUNCTION GROWSEASON

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

      END SUBROUTINE HRNO

