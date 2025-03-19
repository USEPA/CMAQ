
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
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       SUBROUTINE CONVCLD_PROP_ACM( JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C
C  FUNCTION: Convective cloud processor Models-3 science process:
C       MAIN ROUTINE calculates cloud characteristics, and uses them
C       to generate cloud top, bottom, water, ice and fractions.
C
C       ICLDTYPE = 1 => computes raining cloud physics
C       ICLDTYPE = 2 => does the same for non-precip clouds utilizing saved
C                       info from RNCLD in the case of co-existing clouds
C
C  PRECONDITIONS REQUIRED:
C       Dates and times represented YYYYDDD:HHMMSS.
C
C
C  REVISION  HISTORY:
C       Adapted 3/93 by CJC from science module template
C       Version 3/3/93 with complete LCM aqueous chem by JNY.
C       Modified 6/3-7/93 by CJC & JNY to correct treatment of half layers
C       vs. full layers in loop 255:  calculation of DTDP centered at
C       quarter-layers using PSTAR; corresponding revisions to TLCL, TSAT.
C       Uses 4th order R-K solver there.
C       Version 6/5/93 by CJC using relative rainout rates.
C       Version 7/6/93 by CJC using INTERP3()
C       Adapted from LCM aqueous chemistry, initial version, 9/93
C              by JNY and CJC
C       Completion of EM cloud mixing, JNY 12/93
C       Inclusion of EM aqueous chemistry JNY 12/93
C       UPGRADE TO FULL RADM CLOUD MODULE EMULATION, JNY 4/94
C       8/16/94 by Dongming Hwang Configuration management template
C       Adapted 10/96 by S.Roselle for Models-3
C       1/97 s.roselle added McHenry`s well mixed assumption code
C       8/97 S.Roselle revised cgrid units, pressure units, rainfall
C              to hourly amounts, built indices for wet dep species,
C              scavenged species, and aqueous species, built wrapper
C              around aqueous chemistry module
C       10/97 S.Roselle removed McHenry`s well mixed assumption code
C              and put back the below cloud concentration scaling
C       11/97 S.Roselle moved the wet deposition output to the calling
C              routine--CLDPROC
C       01/98 S.Roselle moved indexing code to AQINTER, also
C              moved scavenging to SCAVWDEP
C       03/98 S.Roselle read sub-hourly rainfall data
C       12/98 David Wong at LM:
C             -- changed division of 8000, 2, 1000 to its corresponding
C                reciprocal
C              -- added INT in the expression STEP * 0.5 when calling SEC2TIME
C       03/99 David Wong at LM:
C             -- replaced "/ FRAC * .001" by "/ ( FRAC * 1000.0 )" to minimize
C                lost of significant digits in calculation
C       Jeff - Dec 00 - move CGRID_MAP into f90 module
C       Jeff - Sep 01 - Dyn Alloc - Use HGRD_DEFN
C       4/02 S.Roselle changed minimum horizontal resolution for subgrid
C             clouds from 12km to 8km.
C       1/05 J.Young: dyn alloc - establish both horizontal & vertical
C                     domain specifications in one module
C       5/05 J.Pleim Replaced cloud mixing algorithm with ACM
C       6/05 S.Roselle added new cloud diagnostic variables
C       7/05 J.Young: clean up and mod for CMAQ-F
C       8/10 J.Young: replace chem mechanism include files with namelists
C                    and accomodate Shawn Roselle`s, Sergey Napelenok`s
C                    and Steve Howard`s aerosol reengineering
C       3/11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C       5/11/11 D.Wong: incorporated twoway model implementation
C       7/11 G. Sarwar: calculate zenith angle to determine daytime and nightime 
C                    needed for sulfur oxidation via metal catalysis
C       9/11 S.Roselle: enable CMAQ subgrid cloud model only when met. driver
C             uses a convective cloud parameterization (removed minimum
C             horizontal grid resolution restriction)
C       02Aug12 S.Roselle:  instrumented to calculate and return
C                           transmissivity for convective clouds
C       04Apr14 B.Hutzell:  Added routine call to capture cloud fractions,
C                           water, and ice mixing ratios
C       11Feb15 J.Young: Updated call to czangle.F which uses the ASX_DATA_MOD shared
C                        data module (Implemented by J.Bash on 07 Nov 14)
C       09/04/15 D.Wong: - Made variable declaration method consistent in the caller
C                          and calling routines
C                        - Used a variable rather than an array in calculation to
C                          reduce memory footprint and to increase code efficiency
C       28May15 J.Young: cleanup
C       12Jun15 B.Hutzell: Moved call to CLEAR_ACM_CLOUD to after FIRSTIME block to
C                          insure results from previous time step are removed
C       12Jan16 D.Wong: Fixed a bug that causes different result when code run with 
C                       different domain decomposition
C       4Apr16 J.Bash      Calculate the Sundqvist et al. 1989 threshold humidities 
C                          for cloud formation based on Mocko and Cotton (1995) to be
C                          More consistent with WRF
C       01Feb19 D.Wong: Implemented centralized I/O approach, removed all MY_N
C                       clauses
C       01AUG19 D.Wong: Modified code to work with two-way model
C       11 Nov 19 F. Sidi: Changed MSTEP to accomdate Centralized I/O changes
C-----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE HGRD_DEFN,     ONLY: MYPE
      USE UTILIO_DEFN
      USE PHOT_MET_DATA, ONLY: USE_ACM_CLOUD, CLEAR_ACM_CLOUD, CAPTURE_ACM_CLOUD
      USE ASX_DATA_MOD,  ONLY: INIT_MET, GRID_DATA
      USE CENTRALIZED_IO_MODULE,  ONLY: RCA_AVAIL, cio_model_sdate,
#ifdef mpas
     &                                  cell_area,
#endif
     &                                  cio_model_stime, 
     &                                  interpolate_var,
     &                                  file_tstep, f_met 

     

      IMPLICIT NONE

C...........INCLUDES

      INCLUDE SUBST_CONST               ! constants
      INCLUDE SUBST_FILES_ID            ! file name parameters

C...........Arguments

      INTEGER, INTENT( IN )    :: JDATE
      INTEGER, INTENT( IN )    :: JTIME
      INTEGER, INTENT( IN )    :: TSTEP( 3 )


C...........Parameters

#ifdef mpas
C critical rel humidity for land (fraction)
      REAL, ALLOCATABLE, SAVE      :: RCRITL(:,:)

C critical rel humidity for water (fraction)
      REAL, ALLOCATABLE, SAVE      :: RCRITW(:,:)
#else
C critical rel humidity for land (fraction)
      REAL, SAVE      :: RCRITL

C critical rel humidity for water (fraction)
      REAL, SAVE      :: RCRITW
#endif

C intermediate factor
      REAL            :: XKM

C param contlng sidewall entrainment function for raining clouds
      REAL, PARAMETER :: SIDEFAC = 0.5

C storm rainout efficiency
      REAL, PARAMETER :: STORME  = 0.3

C emp sat vapor press constant from RADM
      REAL, PARAMETER :: C303 = 19.83

C emp sat vapor press constant from RADM
      REAL, PARAMETER :: C302 = 5417.4

C g/kg
      REAL, PARAMETER :: GPKG = 1.0E+03

C 1 hectare = 1.0e4 m**2
      REAL, PARAMETER :: M2PHA = 1.0E+04

C subgrid scale temp perturb (deg K)
      REAL, PARAMETER :: PERT = 1.5

C wvp mix ratio perturb (dimensionless)
      REAL, PARAMETER :: PERQ = 1.5E-3

C rainfall threshold (mm/hr)
      REAL, PARAMETER :: RTHRESH = 0.1

C vapor press of water at 0 C (Pa)
      REAL, PARAMETER :: VP0PA = 611.2

C 1.0 / (vapor press of water @ 0 C) (1/Pa)
      REAL, PARAMETER :: VPINV = 1.0 / VP0PA

C converg. crit. for entrainment solver
      REAL, PARAMETER :: TST = 0.01

C assumed cloud lifetime for convective clouds (sec)
      REAL, PARAMETER :: TCLIFE = 3600.0

C ratio of mol wt of water vapor to mol wt of air
      REAL, PARAMETER :: MVOMA = MWWAT / MWAIR

C ratio of dry gas const to specific heat
      REAL, PARAMETER :: ROVCP = RDGAS / CPD

C ratio of latent heat of vap to specific heat
      REAL, PARAMETER :: LVOCP = LV0 / CPD

C dry adiabatic lapse rate (deg K/m)
      REAL, PARAMETER :: DALR = GRAV / CPD

C Number of species in CGRID
      INTEGER, SAVE :: MXSPCS

C parameter to control frequency of convective cloud processing
C   SYNCCLD=.TRUE.  : every synchronization timestep
C   SYNCCLD=.FALSE. : every hour on the half hour
      LOGICAL, PARAMETER :: SYNCCLD = .TRUE. ! default to sync timestep

      INTEGER       ICLDTYPE            ! 1: raining, 2: either CNP or PFW

C...........Local Variables

C-------for ACM version - jp 2/05        REAL DPB
#ifndef mpas
      REAL, ALLOCATABLE, SAVE :: SIGF( : )
#endif
C-------------------------------------------

      LOGICAL, SAVE :: FIRSTIME = .TRUE. ! flag for first pass thru
      LOGICAL, SAVE :: CONVCLD = .TRUE.  ! flag for modeling convective clds

      CHARACTER( 16 ) :: PNAME = 'CONVCLD_ACM'   ! prcess name
      CHARACTER( 16 ) :: VARNM          ! variable name for IOAPI to get

      CHARACTER( 16 ), SAVE :: RC_NAME  ! RC name: old is RC and new is RCA

      INTEGER          ATIME            ! time diff from half-hour
      INTEGER          CLTOP            ! model LAY containing cloud top
      INTEGER          COL              ! column loop counter
      INTEGER          ROW              ! row loop counter
      INTEGER          CTOP             ! dummy variable for cloud top layer
      INTEGER          FINI             ! ending position
      INTEGER          I599C            ! entrainment solver iteration counter
      INTEGER          LAY              ! layer loop counter
      INTEGER          MDATE            ! process date
      INTEGER          MTIME            ! process time (half-hour)
      INTEGER, SAVE :: MSTEP            ! met file time step (hhmmss)
      INTEGER, SAVE :: SDATE            ! met file start date
      INTEGER          SPC              ! liquid species loop counter
      INTEGER          STEP             ! step loop counter
      INTEGER          STRT             ! starting position
      INTEGER, SAVE :: STIME            ! met file start time
      INTEGER          VAR              ! variable loop counter

      INTEGER          CLBASE           ! cld base layer
      INTEGER          CLTOPUSTBL       ! unstable cld top layer
      INTEGER          ISOUND           ! flag for sounding stability
      INTEGER          SRCLAY           ! cloud source level vert index

      REAL             AIRM             ! total air mass (mol/m2) in cloudy air
      REAL             AIRMB0           ! mol/m2 air below cloud
      REAL             AIRMBI           ! inverse mol/m2 air below cloud
      REAL             ALFA0            ! aitken mode number scavenging coef
      REAL             ALFA2            ! aitken mode sfc area scavenging coef
      REAL             ALFA3            ! aitken mode mass scavenging coef
      REAL             ARPRES           ! ave cloud pres in atm
      REAL             CONDIS           !
      REAL             CTHK             ! cloud thickness (m)
      REAL             CTHK1            ! aq chem calc cloud thickness
      REAL             DAMDP            ! dry adiabatic minus dew point lapse rate
      REAL             DP               ! pressure increment along moist adiabat
      REAL             DPLR             ! dew point lapse rate
      REAL             DQI              ! change in ice mix ratio due to melting caused by entrainment
      REAL             DQL              ! change in liq wat mix ratio due to evap caused by entrainment
      REAL             DTCLD            ! cloud integration timestep (s)
      REAL             DTDP             ! moist adiabatic lapse rate
      REAL             DZLCL            ! height increment to LCL above source level
      REAL             ZLCL             ! height of LCL above ground
      REAL             EMAX             ! water vapor pressure at source level
      REAL             EQTH             ! parcel equivalent potential temperature
      REAL             EQTHM            ! parcel equivalent potential temp
      REAL             FA               ! entrainment functional value at TEMPA
      REAL             FB               ! entrainment functional value at TEMPB
      REAL             FRAC             ! cloud fractional coverage
      REAL             FTST             ! functional product in Walcek bisection solver
      REAL             HTST             ! temp diff in Walcek bisection solver
      REAL, SAVE    :: METSTEP          ! reciprocal of timestep on the met file, 1/hr
      REAL             P1               ! intermediate pressure used in calculating WL
      REAL             P2               ! intermediate pressure used in calculating WL
      REAL             P3               ! intermediate pressure used in calculating WL
      REAL             PBAR             ! mean pressure in vertical increments up from LCL along moist adiabat
      REAL             PBARC            ! mean cloud pressure (Pa)
      REAL             PMAX             ! parcel pressure
      REAL             PP               ! scratch pressure variable
      REAL             PRATE            ! total rainfall (mm/hr)
      REAL             PRATE1           ! storm rainfall rate (mm/hr)
      REAL             QENT             ! wat vap mix ratio due to cld sidewall entrainmt
      REAL             QP               ! perturbed water vap mix ratio of parcel
      REAL             QXS              ! int. excess wat ov grid cell needed for rainout
      REAL             REMOVAC          ! variable storing H+ deposition
      REAL             RHOAIR           ! air density in kg/m3
      REAL             RLH              ! relative humidity
      REAL             RLHSRC           ! relative humidity at cld src level
      REAL             RTCH             ! chemical gas const times temp
      REAL             T1               ! perturbed temp to calc neutral buoyancy also used as max temp in cell comparing cloud with environment
      REAL             TBAR             ! mean temp in vertical increments up from LCL along moist adiabat
      REAL             TBARC            ! mean cloud temp (K)
      REAL             TBASE            ! iterative temp along moist adiabat
      REAL             TDMAX            ! dew point at source level
      REAL             TEMPA            ! lower limit on temp for entrainment solver
      REAL             TEMPB            ! upper limit on temp for entrainment solver
      REAL             TEMPC            ! scratch temp solved for cloudy air parcel
      REAL             TENT             ! temp accounting for cld sidewall entrainment
      REAL             THMAX            ! parcel potential temperature
      REAL             TI               ! init temp of cloud air before evap of water
      REAL             TLCL             ! temp at LCL
      REAL             TMAX             ! perturbed temp of parcel
      REAL             TP               ! perturbed temp of parcel
      REAL             TTOP             ! scr vbl used in application of Eq. 7, W&T
      REAL             TWC              ! tot wat cont in cloud (kg H2O/m3 air)
      REAL             WCBAR            ! liq water content of cloud (kg/m3)
      REAL             WL               ! Warner profile (an earlier version appears appears in Walcek and Taylor (JAS, 1986)
      REAL             WTBAR            ! total wat cont (kg/m2) int. thru cloud depth
      REAL             X1               ! intermediate vbles in lapse rate calculation X1 also reused as scratch vble in mixing
      REAL             QDIF             ! scratch vbl used in entrainment solver
      REAL             CLOD
      REAL             LWP
      REAL             STRNS            ! intermediate to set subgrid cld transmissivity
      REAL          :: DENSL( NLAYS )      ! air density (kg/m3)
      REAL          :: F    ( NLAYS )      ! cloud entrainment fraction to be solved for
      REAL          :: FSIDE( NLAYS )      ! sidewall entrainment vertical profile
      REAL          :: LWC  ( NLAYS )      ! liq wat cont of cloud in kg H2O/m3 air
      REAL, ALLOCATABLE, SAVE :: QICE ( : )      ! ice mixing ratio in cloud
      REAL, ALLOCATABLE, SAVE :: QLQD ( : )      ! actual liq. wat. mix ratio in cloud
      REAL          :: QVC  ( NLAYS )      ! saturation wat vap mix ratio at T1
      REAL          :: QWAT ( NLAYS )      ! liq wat mix rat, taken as total condensed water (ice + liq) profile (Eq.4, W&T)
      REAL          :: RHOM2( NLAYS )      ! mol/m2 air
      REAL          :: TCLD ( NLAYS )      ! temp of cloudy air parcel

      REAL             FRACMAX             ! max frac cov for NP cld
      REAL             PLCL                ! pressure at LCL
      REAL             QMAX                ! pertbd w.. mix rat of parcel
      REAL          :: RAIN( NCOLS,NROWS ) ! this timestep rainfall (mm/hr)
      REAL             BCLDWT              ! below cloud weighting function

      REAL          :: RC   ( NCOLS,NROWS )        ! hourly convective rainfall (cm)
      REAL          :: PBL  ( NCOLS,NROWS )        ! PBL height (m)
      REAL          :: DZZ  ( NCOLS,NROWS,NLAYS )  ! computed gridded vble
      REAL          :: DZZL ( NLAYS )              ! grid cell delta Z
      REAL          :: PRES ( NCOLS,NROWS,NLAYS )  ! file gridded vble
      REAL          :: PRESL( NLAYS )              ! grid cell pressure
      REAL          :: QAD  ( NLAYS )              ! moist adiab. sat. mix ratio
      REAL          :: QV   ( NCOLS,NROWS,NLAYS )  ! input gridded vble
      REAL          :: QVL  ( NLAYS )              ! grid cell sp. hum.
      REAL          :: TA   ( NCOLS,NROWS,NLAYS )  ! input gridded vble
      REAL          :: TAL  ( NLAYS )              ! grid cell temp
      REAL          :: TSAT ( NLAYS )              ! parcel temp along moist adiabat @ half levels
      REAL          :: ZH   ( NCOLS,NROWS,NLAYS )  ! mid-layer height (m)
      REAL          :: ZF   ( NCOLS,NROWS,NLAYS )  ! level/layer-face height (m)

      INTEGER         ALLOCSTAT
      INTEGER         STATUS

      CHARACTER( 120 ) :: XMSG = ' '   ! Exit status message

C...........Statement Functions

      REAL             ESAT            ! sat vap pres (Pa) as fn of T (deg K)
      REAL             QSAT            ! sat water vapor mixing ratio

      REAL             T               ! temperature dummy arg
      REAL             E               ! sat vapor pressure dummy arg
      REAL             P               ! pressure dummy arg

      ESAT( T ) = VP0PA * EXP( C303 - ( C302 / T ) )

      QSAT( E, P ) = MVOMA * ( E / ( P - E ) )

C-----------------------------------------------------------------------
C  begin body of subroutine CONVCLD_ACM

C...INITIALIZATION for the CONVCLD_ACM module:
C...  event-statistics variables.

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

        CALL INIT_MET( JDATE, JTIME )

C...check the grid resolution from the MET_CRO_2D and set an appropriate
C...  flag as to whether convective clouds should be run for the given
C...  resolution

C...open MET_CRO_3D

!       IF ( .NOT. OPEN3( MET_CRO_3D, FSREAD3, PNAME ) ) THEN
!         XMSG = 'Could not open '// MET_CRO_3D // ' file'
!         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!       END IF

C...get description from the met file

!       IF ( .NOT. DESC3( MET_CRO_2D ) ) THEN
!         XMSG = 'Could not get ' // MET_CRO_2D //' file description'
!         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!       END IF

C cccccccccccccccccccc enable backward compatiblity ccccccccccccccccccccc

        IF ( RCA_AVAIL ) THEN
           RC_NAME = 'RCA'
        ELSE
           RC_NAME = 'RC'
        END IF

C...store met file time, date, and step information and compute
C...  the met timestep in hours

        SDATE = cio_model_sdate
        STIME = cio_model_stime
        MSTEP = file_tstep(f_met) 
        
        METSTEP = 3600.0 / FLOAT( TIME2SEC( MSTEP ) ) ! convert to 1/hours

C...check convective precipitation on met files to determine if WRF used
C...  a convective parameterization

        call interpolate_var (RC_NAME, sdate, stime, RC)

C...in coordination with MCIPv4.0, negative values will be loaded into the RC
C...  field if a convective parameterization was not used in the WRF simulation

        IF ( MAXVAL( RC ) .LT. 0.0 ) THEN
          CONVCLD = .FALSE.
          XMSG = 'MCIP files indicate no convective parameterization was '
     &        // 'used in the WRF simulation'
          CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
          XMSG = 'Processing will continue without subgrid clouds'
          CALL M3MESG ( XMSG )
          RETURN
        END IF

C...allocate saved arrays
#ifndef mpas
        ALLOCATE ( SIGF( 0:NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating SIGF'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        DO LAY = 1, NLAYS
          SIGF( LAY ) = 1.0 - X3FACE_GD( LAY )
        END DO
        SIGF( 0 ) = 1.0
#endif
        ALLOCATE ( QLQD   ( NLAYS ), 
     &             QICE   ( NLAYS ), 
     &             STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating BMOL, CBASE0, CBASEF, CEND, POLC, REMOV,'
     &         // 'QLQD or QICE'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF


C...Calculate the Sundqvist et al. 1989 threshold humidities for cloud formation based on 
C...Mocko and Cotton (1995)
#ifdef mpas
       ALLOCATE ( RCRITL( NCOLS,NROWS ),
     &             RCRITW( NCOLS,NROWS ),
     &             STAT = ALLOCSTAT )
          IF ( ALLOCSTAT .NE. 0 ) THEN
             XMSG = 'EXIT: Failure allocating RCRITL, RCRITW'
             call prog_interrupt (PNAME, JDATE, JTIME, XMSG, 1)
          END IF
        DO ROW = 1, NROWS
          DO COL = 1, NCOLS
            ! Reformulate using cell_area to replace XKM
            RCRITW(COL,ROW) = 0.879 + SQRT( 1.0 / ( 100.0 + 1.0E-6*cell_area(col,row)  ))
            RCRITL(COL,ROW) = 0.839 + SQRT( 1.0 / ( 50.0 + 0.5 * 1.0E-9*(cell_area(col,row))**1.5 ) )
          END DO
        END DO

#else
        XKM = REAL( XCELL_GD / 1000 )
        RCRITW = 0.879 + SQRT( 1.0 / ( 100.0 + XKM * XKM ) )
        RCRITL = 0.839 + SQRT( 1.0 / ( 50.0 + 0.5 * XKM ** 3 ) )
#endif

      END IF   ! Firstime

      IF ( .NOT. CONVCLD ) RETURN

C...check option for processing clouds on the synchronization timestep

      MDATE = JDATE
      MTIME = JTIME

C...set the cloud timestep (=adv timestep)

      STEP  = TIME2SEC( TSTEP( 2 ) )         ! syncronization timestep
      DTCLD = REAL( STEP )

C...set time to the midpoint of this timestep for data interpolation

      CALL NEXTIME ( MDATE, MTIME, SEC2TIME( STEP / 2 ) )

C...clear arrays that capture ACM cloud results

      IF ( USE_ACM_CLOUD ) CALL CLEAR_ACM_CLOUD( JDATE, JTIME )

C...ACTUAL SCIENCE PROCESS (loop on internal process time steps):
C...  Interpolate time dependent layered input variables
C...  (reading those variables for which it is necessary)

      call interpolate_var ('TA', mdate, mtime, TA)

      call interpolate_var ('QV', mdate, mtime, QV)

      call interpolate_var ('ZF', mdate, mtime, ZF)

      call interpolate_var ('ZH', mdate, mtime, ZH)

C...Get pressure (Pa)
      call interpolate_var ('PRES', mdate, mtime, PRES)

C...compute layer thicknesses (m)
      DO ROW = 1, NROWS
        DO COL = 1, NCOLS
          DZZ( COL,ROW, 1 ) = ZF( COL,ROW, 1 )
          DO LAY = 2, NLAYS
            DZZ( COL,ROW,LAY ) = ZF( COL,ROW,LAY ) - ZF( COL,ROW,LAY - 1 )
          END DO
        END DO
      END DO

C...Get PBL height (m)
      call interpolate_var ('PBL', mdate, mtime, PBL)

C...advance the MDATE and MTIME to the next time on the met file
C...  to get ready to read the precipitation amounts.
C...  Precipitation data WILL NOT BE INTERPOLATED!  Precipitation data
C...  on the input file are amounts within the metfiles timestep.

      IF ( .NOT. CURRSTEP( JDATE, JTIME, SDATE, STIME, MSTEP,
     &                     MDATE, MTIME ) ) THEN
        XMSG = 'Cannot get step-starting date and time'
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
      END IF

      CALL NEXTIME ( MDATE, MTIME, MSTEP )  ! set mdate:mtime to the hour

C...Get convective precipitation amount (cm)

      call interpolate_var (RC_NAME, mdate, mtime, RC)

C...Convert the rainfall rate into mm/hr, then set a flag noting the
C...  presence of raining clouds if the rainfall is above the specified
C...  threshold

      DO ROW = 1, NROWS
        DO COL = 1, NCOLS
          RAIN( COL,ROW ) = 10.0 * RC( COL,ROW ) * METSTEP
        END DO
      END DO
      IF ( MINVAL( RAIN ) .LT. 0.0 ) THEN
        XMSG = 'NEGATIVE RAIN...PROBABLY BAD MET DATA... in' // MET_CRO_2D
        CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF

C...Loop through all grid cells

      DO 311 ROW = 1, NROWS
        DO 301 COL = 1, NCOLS

          QLQD = 0.0
          QICE = 0.0
          FRAC = 0.0


          CLBASE     = NLAYS
          CLTOP      = CLBASE
          CLTOPUSTBL = NLAYS
          QMAX       = 0.0
          PLCL       = 0.0
          SRCLAY     = NLAYS

          DO LAY = 1, NLAYS
            QAD( LAY ) = 0.0
            PRESL( LAY ) = PRES( COL,ROW,LAY )
            TAL( LAY )   = TA( COL,ROW,LAY )
            QVL( LAY )   = QV( COL,ROW,LAY )
            DZZL( LAY )  = DZZ( COL,ROW,LAY )
            DENSL( LAY ) = PRESL( LAY ) / ( RDGAS * TAL( LAY ) )
          END DO

C...Test for raining clouds
C...If the rainfall amount is below the specified threshold, then set
C...  values for some of the parameters which will be used when the
C...  routine is called again for non-precipitating clouds...then
C...  skip to the next grid cell.

          IF ( RAIN( COL,ROW ) .GE. RTHRESH ) THEN
            ICLDTYPE = 1
            PRATE = RAIN( COL,ROW )
            FRACMAX = 0.0
          ELSE
            ICLDTYPE = 2
            FRACMAX = 0.5
          END IF

C...Determine cloud source level by determining equivalent
C...   potential temperature profile given perturbed temperature
C...   and water vapor to account for local hot spots which
C...   initiate convection.  Layer with maximum equivalent
C...   potential temperature is cloud source layer.

          SRCLAY = 1
          TMAX  = TAL( 1 ) + PERT
          QMAX  = QVL( 1 ) + PERQ
          PMAX  = PRESL( 1 )
          THMAX = TMAX * ( 1.0E+05 / PMAX ) ** ROVCP
          EQTHM = THMAX * EXP( LVOCP * QMAX / TMAX )

          DO LAY = 2, NLAYS

            PP = PRESL( LAY )

            IF ( ZH( COL,ROW,LAY ) .GT. 3000.0 ) EXIT   ! 650 mb

            TP = TAL( LAY ) + PERT
            QP = QVL( LAY ) + PERQ
            THMAX = TP * ( 1.0E+05 / PP ) ** ROVCP
            EQTH = THMAX * EXP( LVOCP * QP / TP )

            IF ( EQTH .GT. EQTHM ) THEN
              TMAX = TP
              SRCLAY = LAY
              QMAX  = QP
              PMAX = PP
              EQTHM = EQTH
            END IF

          END DO

C...Equivalent potential temp max is now known between LAY 1
C...   and 650 mb. We now proceed to compute lifting condensation
C...   level. First, compute vapor pressure at the source level.
C...   Find dewpoint using empirical relationship, avoiding
C...   supersaturation. Then compute dew point lapse rate -
C...   see Walcek and Taylor, 1986.

          EMAX  = QMAX * PMAX / ( MVOMA + QMAX )
          TDMAX = C302 / ( C303 - LOG( EMAX * VPINV ) )
          TDMAX = MIN( TDMAX, TMAX )
          DPLR  = ( GRAV * TDMAX * TDMAX ) / ( MVOMA * LV0 * TMAX )

c...Compute difference between dry adiabatic and dew point lapse
C...   rate, height increment above source level to reach LCL,
C...   then calculate value of pressure at LCL.  Save result
C...   in CONV_DEP( *,*,N_SPC_WDEP+2 ).

          DAMDP = DALR - DPLR

          IF ( DAMDP .LE. 0.0 ) THEN

            DZLCL = 0.0
            PLCL = PMAX
C...walcek formula
            TLCL = TMAX
C...walcek formula

          ELSE

            DZLCL = ( TMAX - TDMAX ) / DAMDP
C...walcek formula
            TLCL = TMAX - DALR * DZLCL
C...walcek formula
            TBAR = TMAX - 0.5 * DALR * DZLCL   !  midpt of TMAX, TLCL
            TBAR = MAX( TBAR , 150.0 )
            PLCL = PMAX * EXP( -( GRAV / RDGAS ) * DZLCL / TBAR )
            ZLCL = DZLCL + ZH( COL,ROW, SRCLAY )

          END IF

C...Determine cloud base at LAY in  which LCL resides,
C...  but not below layer 2.

C...plcl above middle of top layer

          IF ( PRESL( NLAYS ) .GE. PLCL ) THEN
            PLCL = PRESL( NLAYS )
            CLBASE = NLAYS
            CLTOP = CLBASE
            WRITE( LOGDEV,* ) ' WARNING: PLCL above top: Continuing'

C...search loop to find CLBASE

          ELSE

            DO LAY = 2, NLAYS
              IF ( PRESL( LAY ) .LE. PLCL ) THEN
                CLBASE = LAY
                GO TO 245
              END IF
            END DO

            CLBASE = NLAYS   ! if you get here base never found

245         CONTINUE

          END IF      ! if plcl < ptop or , or ...

C...CLBASE is LAY of LCL. Now, determine cloud top by following
C...   moist adiabat up from CLBASE. Assume a stable sounding
C...   (ISOUND=0) at first.  Moist adiabat solver calculates
C...   saturation temperatures TF at the full levels and TSAT( COL,ROW,LAY )
C...   at the half-levels, using a 2nd order Runge method employing
C...   temperatures and pressures at the quarter-levels.

          ISOUND = 0
          DO 255 LAY = CLBASE, NLAYS

C...walcek formulas

            DP   = PRESL( LAY - 1 ) - PRESL( LAY )
            PBAR = PRESL( LAY - 1 ) - DP * 0.5
            IF ( LAY .EQ. CLBASE ) THEN
              DP    = PLCL - PRESL( LAY )
              PBAR  = PLCL - DP * 0.5
              TBASE = TLCL
            END IF

            TBAR = MAX( TBASE - 0.00065 * DP, 150.0 )
            X1 = LV0 * QSAT( ESAT( TBAR ), PBAR ) / ( RDGAS * TBAR ) ! Walcek's
            DTDP = ( ( RDGAS * TBAR ) / ( PBAR * CPD ) )             ! original
     &           * ( ( 1.0 + X1 )                                    ! formulas
     &           / ( 1.0 + ( 0.622 * LVOCP / TBAR ) * X1 ) )
            TSAT( LAY ) = MAX( TBASE - DP * DTDP, 150.0 )
            QAD ( LAY ) = QSAT( ESAT( TSAT( LAY ) ), PRESL( LAY ) )
            TBASE = TSAT( LAY )

C...end Walcek formulas

C...QAD is the moist adiabatic saturation mixing ratio, needed
C...  for the entrainment solver
C...  Now make choice on stability of sounding, comparing parcel
C...  temperature TSAT with environmental temperature TA.
C...  ISOUND is index for sounding stability. If ISOUND=0,
C...  moist adiabat never warmer than environment (stable).
C...  ISOUND=1, moist adiabat becomes warmer than environment
C...  (unstable).

            IF ( ISOUND .EQ. 0 ) THEN
              IF ( TSAT( LAY ) .GT. TAL( LAY ) ) ISOUND = 1
            ELSE           ! cloud top determined by neutral bouyancy
              T1 = TSAT( LAY ) ! - 0.5 * PERT
              IF ( T1 .LT. TAL( LAY ) ) THEN
                CLTOP = LAY - 1
                GO TO 256
              END IF
            END IF

255       CONTINUE            !  end loop following moist adiabat

          CLTOP = NLAYS - 1   !  if you get here:  cloud stable or no top

256       CONTINUE

C...At this point, if ISOUND has not been set to 1, we have a
C...  "stable" cloud. In this case, we find cloud top by relative
C...   humidity criterion, or, not let cloud top go above 600mb.

          IF ( ISOUND .EQ. 0 ) THEN
            IF ( ICLDTYPE .NE. 1 ) GO TO 299

            DO 265 LAY = CLBASE + 1, NLAYS
              IF ( PRESL( LAY ) .LE. 60000.0 ) THEN
                CLTOP = LAY - 1
                GO TO 267        ! loop exit
              END IF
              RLH = QVL( LAY ) / QSAT( ESAT( TAL( LAY ) ), PRESL( LAY ) )
              IF ( RLH .LT. 0.65 ) THEN
                CLTOP = LAY - 1
                GO TO 267        ! loop exit
              END IF
265         CONTINUE

            CLTOP = NLAYS - 1   ! if you get here:  top never found

          ELSE

            CLTOPUSTBL = CLTOP  ! store unstable cloud top

          END IF

267       CONTINUE   ! loop exit target

          IF ( ICLDTYPE .NE. 1 ) THEN   !  get cloud top for either CNP or PFW

            IF ( ZLCL .GT. PBL( COL,ROW ) ) GO TO 299

C...compute relative humidity at the cloud source level

            RLHSRC = MIN( 1.0, QVL( SRCLAY )
     &                        / QSAT( ESAT( TAL( SRCLAY ) ), PRESL( SRCLAY ) ) )

C...If all tests pass, then a CNP or PFW cloud exists
C...  Proceed to find CLTOP for CNP or PFW; don`t allow
C...  cloud top to exceed 500mb, or, when RH falls below
C...  65%, cloud top found

C...Distiguish between CNP and PFW by whether rain is falling
C...  in the cell; if PFW, limit depth and find new CLTOP,
C...  else leave CLTOP alone

            IF ( CLTOP .EQ. CLBASE ) THEN
              GO TO 322
            ELSE                   ! confine PFW to 1500 meters
              CTOP = CLTOP

              DO LAY = CTOP, CLBASE, -1
                IF ( ZH( COL,ROW,LAY ) - ZH( COL,ROW,CLBASE ) .LE. 3000.0 ) THEN
                  CLTOP = LAY
                  GO TO 322   ! exit loop
                END IF
              END DO

            END IF

322         CONTINUE     ! loop exit for PFW cloud

C...If unstable CNP or PFW, limit CLTOP to CLTOPUSTBL so that
C...  QAD profile is known through cloud depth for entrainment
C...  solver

            IF ( ISOUND .EQ. 1 ) CLTOP = MIN( CLTOP, CLTOPUSTBL )

C...Now compute fractional coverage for either CNP or PFW:
C...Now based on Sunqdvist et al. 1989 DOI: 10.1175/1520-0493(1989)117<1641:CACPSW>2.0.CO;2 
            FRAC = 0.0
            IF ( GRID_DATA%LWMASK( COL,ROW ) .EQ. 1.0 ) THEN   ! land
#ifdef mpas
               IF ( RLHSRC .GE. RCRITL( COL,ROW) )
     &            FRAC = 1.0 - SQRT( ( 1.0 - RLHSRC ) / ( 1.0 - RCRITL( COL,ROW ) ) )
#else
               IF ( RLHSRC .GE. RCRITL )
     &            FRAC = 1.0 - SQRT( ( 1.0 - RLHSRC ) / ( 1.0 - RCRITL ) )
#endif
            ELSE   ! water
#ifdef mpas
               IF ( RLHSRC .GE. RCRITW( COL,ROW ) )
     &            FRAC = 1.0 - SQRT( ( 1.0 - RLHSRC ) / ( 1.0 - RCRITW( COL,ROW ) ) )
#else
               IF ( RLHSRC .GE. RCRITW )
     &            FRAC = 1.0 - SQRT( ( 1.0 - RLHSRC ) / ( 1.0 - RCRITW ) )
#endif
            END IF
            FRAC = MAX( 0.0, MIN( FRAC, 0.95 ) )

            IF ( FRAC .LT. 0.01 ) GO TO 299


          END IF  ! end of existence, depth and frac cov calc for
                  ! either PFW or CNP clouds

C...Now cloud existence is established, initialize various
C...  variables needed for rest of computations

C...First, get mol air/m2 at each layer, initialize FSIDE

          DO LAY = 1, NLAYS
            RHOM2( LAY ) = PRESL( LAY ) * DZZL( LAY )
     &                   * 1.0E3 / ( RDGAS * MWAIR * TAL( LAY ) )
            FSIDE( LAY ) = 0.0
          END DO

C...Initialize variables needed for entrainment and in-cloud properties solver

          QXS =   0.0  ! integrated excess water over grid cell nec. for rnout
          AIRM =  0.0  ! total air mass (mol/m2) in cloudy layers
          PBARC = 0.0  ! in-cloud average pressure
          CTHK  = 0.0  ! cloud thickness (m)
          WCBAR = 0.0  ! condensed wat cont (kg/m2) integ. thru cloud depth
          WTBAR = 0.0  ! total wat cont (kg/m2) integrated thru cloud depth
          TBARC = 0.0  ! cloud mean temp (K)

C...Determine condensed water content and entrainment at each cloud level
C...  Determine FSIDE profile for raining clouds; side entrainment
C...  only for PFW and CNP clouds

          IF ( ICLDTYPE .EQ. 1 ) THEN   ! raining cloud

            IF ( CLBASE .EQ. CLTOP ) THEN
              FSIDE( CLBASE ) = 1.0
            ELSE

              DO LAY = CLBASE, CLTOP
                FSIDE( LAY ) = 1.0
              END DO

            END IF

          ELSE                    ! CNP or PFW

            DO LAY = CLBASE, CLTOP
              FSIDE( LAY ) = 1.0
            END DO

          END IF

C...Use Warner profile to close system of conservation and
C...  thermodynamic equations solved iteratively, using Secant solver

          DO LAY = CLBASE, CLTOP
            WL = 0.7 * EXP( ( PRESL( LAY ) - PLCL ) * 0.000125 ) + 0.2

            IF ( LAY .EQ. CLBASE ) THEN
              P1 = 0.5 * ( PRESL( LAY ) + PRESL( LAY - 1 ) )

              IF ( PLCL .LT. P1 ) THEN
                P2 = 0.5 * ( PRESL( LAY + 1 ) + PRESL( LAY ) )
                P3 = ( P2 + PLCL ) * 0.5
                WL = 0.7 * EXP( ( P3 - PLCL ) * 0.000125 ) + 0.2
              END IF

            END IF

c...original Walcek bisection solver

            QWAT( LAY ) = WL * ( QMAX - QAD( LAY ) )
            QWAT( LAY ) = MAX( QWAT( LAY ), 1.0E-20 )

            TEMPA = TSAT( LAY ) - 20.0
            TEMPB = TSAT( LAY ) + 10.0

            QENT = FSIDE( LAY ) * QVL( LAY )
     &           + ( 1.0 - FSIDE( LAY ) ) * QVL( CLTOP )
            QDIF = QENT - QMAX
            IF ( QDIF .EQ. 0.0 ) QDIF = 1.0E-10
            F( LAY ) = ( QSAT( ESAT( TEMPA ), PRESL( LAY ) )
     &               + QWAT( LAY ) - QMAX ) / QDIF
            F( LAY ) = MIN( F( LAY ), 1.0 )
            F( LAY ) = MAX( F( LAY ), 0.0 )

            TTOP = TAL( CLTOP ) * ( PRESL( LAY ) / PRESL( CLTOP ) ) ** ROVCP
            TENT = TTOP * ( 1.0 - FSIDE( LAY ) ) + TAL( LAY ) * FSIDE( LAY )

            TI = TSAT( LAY ) * ( 1.0 - F( LAY ) ) + TENT * F( LAY )
            DQL = ( QMAX - QAD( LAY ) ) * ( 1.0 - F( LAY ) - WL )
            DQI = 0.0

            IF ( TEMPA .LT. 273.15 ) THEN
              DQI = -QWAT( LAY ) * ( TEMPA - 273.15 ) / 18.0
              IF ( TEMPA .LE. 255.15 ) DQI = QWAT( LAY )
            END IF

            FA = CPD * ( TEMPA - TI ) + LV0 * DQL + LF0 * DQI

C...test for convergence, then cut the interval in half

            I599C = 0

599         CONTINUE

            HTST = TEMPB - TEMPA
            IF ( HTST .LT. TST ) GO TO 595   ! convergence
            I599C = I599C + 1

            IF ( I599C .GT. 1000 ) THEN
              WRITE( XMSG, 91010 )
     &             'NO CONVERGENCE IN ENTRAINMENT SOLVER AT COL= ',
     &             COL, ' ROW= ',  ROW, ' ICLDTYPE= ', ICLDTYPE
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

            TEMPC = ( TEMPA + TEMPB ) * 0.5
            QENT = FSIDE( LAY ) * QVL( LAY )
     &           + ( 1.0 - FSIDE( LAY ) ) * QVL( CLTOP )
            QDIF = QENT - QMAX
            IF ( QDIF .EQ. 0.0 ) QDIF = 1.0E-10
            F( LAY ) = ( QSAT( ESAT( TEMPC ), PRESL( LAY ) )
     &               + QWAT( LAY ) - QMAX ) / QDIF
            F( LAY ) = MIN( F( LAY ), 0.99 )
            F( LAY ) = MAX( F( LAY ), 0.01 )
            TTOP = TAL( CLTOP ) * ( PRESL( LAY ) / PRESL( CLTOP ) ) ** ROVCP
            TENT = TTOP * ( 1.0 - FSIDE( LAY ) ) + TAL( LAY ) * FSIDE( LAY )
            TI = TSAT( LAY ) * ( 1.0 - F( LAY ) ) + TENT * F( LAY )
            DQL = ( QMAX - QAD( LAY ) ) * ( 1.0 - F( LAY ) - WL )
            DQI = 0.0

            IF ( TEMPC .LT. 273.15 ) THEN
              DQI = -QWAT( LAY ) * ( TEMPC - 273.15 ) / 18.0
              IF ( TEMPC .LE. 255.15 ) DQI = QWAT( LAY )
            END IF

            FB = CPD * ( TEMPC - TI ) + LV0 * DQL + LF0 * DQI

            FTST = FA * FB

C...if fa*fb < 0 then zero lies between ta & tc
C...if fa*fb > 0 then zero lies between tc & tb

            IF ( FTST .LE. 0.0 ) THEN
              TEMPB = TEMPC
            ELSE
              TEMPA = TEMPC
            END IF
            GO TO 599

595         CONTINUE   ! exit from iterator, convergence achieved

C...we have obtained parcel temp TEMPC at layer LAY
C...and entrainment fraction F(LAY)

C...end of Walcek bisection solver

            TCLD( LAY ) = MAX( TEMPC, 150.0 )

C...ice load in cloud is a function of temperature below freezing

            IF ( TCLD( LAY ) .LT. 273.15 ) THEN
              QICE( LAY ) = -QWAT( LAY ) * ( TCLD( LAY ) - 273.15 ) / 18.0
              IF ( TCLD( LAY ) .LE. 255.15 ) QICE( LAY ) = QWAT( LAY )
            END IF

C...After determining the ice fraction, compute the actual
C...  liquid water mixing ratio:

            QLQD( LAY ) = QWAT( LAY ) - QICE( LAY )

C...compute the Liquid Water Content (LWC) by taking the
C...  product of the liquid wat mix ratio and the air density
C...  LWC in kg H2O per m**3 air:

            RHOAIR = PRESL( LAY ) / ( RDGAS * TCLD( LAY ) )
            LWC( LAY ) = QLQD( LAY ) * RHOAIR
            LWC( LAY ) = MAX( 5.0E-6, LWC( LAY ) )  ! lower limit
            TWC = QWAT( LAY ) * RHOAIR         ! total water content

C...Now perform vertical integration, weighting by liquid water
C...  content so that averaged quantities (used in Aqueous
C...  Chemistry) get the greatest weight where the liquid
C...  water content is greatest.

C...weighted cloud temp
            TBARC = TBARC + TCLD( LAY ) * DZZL( LAY ) * LWC( LAY )

C...weighted cloud pres
            PBARC = PBARC + PRESL( LAY ) * DZZL( LAY ) * LWC( LAY )

C...integrated liquid water content (kg/m3)
            WCBAR = WCBAR + DZZL( LAY ) * LWC( LAY )

C...integrated total water content
            WTBAR = WTBAR + DZZL( LAY ) * TWC
            CTHK = CTHK + DZZL( LAY )   ! Cloud thickness

C...Now compute integrated excess water over grid cell
C...  average necessary for rainout, through cloud depth.
C...  First, get max temp in the cell (either in cloud or env.)

            T1 = MAX( TCLD( LAY ), TAL( LAY ) )

C...get saturation water vapor mixing ratio at that temp:

            QVC( LAY ) = QSAT( ESAT( T1 ), PRESL( LAY ) )

C...excess water is the sum of total condensed and saturated
C...  vapor minus grid cell average mixing ratio: QXS in kg/m2:
C...  integrated through cloud depth

            QXS = QXS
     &          + ( QWAT( LAY ) + QVC( LAY ) - QVL( LAY ) )
     &          * RHOAIR * DZZL( LAY )

C...get total air mass in cloudy layers:

            AIRM = AIRM + RHOM2( LAY )

          END DO

C...Now begin to split calculations for non-raining and raining
C...  clouds depending on inner loop index ICLDTYPE (1 = raining,
C...  2 = nonraining: either CNP of PFW:)

          IF ( ICLDTYPE .EQ. 2 ) THEN   ! no precip or excess water
            PRATE1 = 1.0E-30
            PRATE  = 1.0E-30
            QXS    = 1.0E-30
            GO TO 7000       ! branch for further CNP or PFW calculations
          END IF

C...continue here for raining cloud...

C...get PRATE1, storm rainout rate in mm/hour, noting that 1 kg
C...  of water occupies a 1 mm thick layer of water in a square meter
C...  of ground (accounts for density of water = 1000 kg/m3)

          PRATE1 = STORME * QXS * 3600.0 / TCLIFE
          IF ( PRATE1 .LE. 1.001 * PRATE ) THEN
            FRAC = 0.999                ! Changed back to .999 - jp 6/05
            PRATE1 = PRATE / FRAC
          ELSE
            FRAC = PRATE / PRATE1
          END IF
          IF ( FRAC .LT. 0.01 ) GO TO 299

C...for raining cloud, compute water properties of interest
C...  below cloud base. First, parameterize total water content

          TWC = ( 0.067 * PRATE ** ( 0.846 ) ) / ( FRAC * 1000.0 ) ! tot wat cont kg/m3

          DO LAY = 1, CLBASE - 1
            TCLD( LAY ) = TAL( LAY )
            RHOAIR = PRESL( LAY ) / ( RDGAS * TCLD( LAY ) )
            QWAT( LAY ) = TWC / RHOAIR    ! kg H2O / kg air

C...again partition into ice and liquid

            IF ( TCLD( LAY ) .LT. 273.15 ) THEN
              QICE( LAY) = -QWAT( LAY ) * ( TCLD( LAY ) - 273.15 ) / 18.0
              IF ( TCLD( LAY ) .LE. 255.15 ) QICE( LAY ) = QWAT( LAY )
            END IF

            QLQD( LAY ) = QWAT( LAY ) - QICE( LAY )
            LWC ( LAY ) = QLQD( LAY ) * RHOAIR
            LWC ( LAY ) = MAX( 5.0E-06, LWC( LAY ) )         ! lower limit
            PBARC = PBARC + PRESL( LAY ) * DZZL( LAY ) * LWC( LAY )
            TBARC = TBARC + TCLD( LAY ) * DZZL( LAY ) * LWC( LAY )
            WCBAR = WCBAR + DZZL( LAY ) * LWC( LAY )
            WTBAR = WTBAR + DZZL( LAY ) * TWC
            CTHK = CTHK + DZZL( LAY )

C...excess water is all rain

            QXS = QXS + QWAT( LAY ) * RHOAIR * DZZL( LAY )
            
          END DO

C...Final calc of storm rainfall rate and frac area (raining clds)

          PRATE1 = STORME * QXS * 3600.0 / TCLIFE

          IF ( PRATE1 .LE. 1.001 * PRATE ) THEN
            FRAC = 0.999        ! Changed back to .999 - jp 6/05
            PRATE1 = PRATE / FRAC
          ELSE
            FRAC = PRATE / PRATE1
          END IF
          IF ( FRAC .LT. 0.01 ) GO TO 299
          
7000      CONTINUE                        ! target of cloudtype split

C...Capture cloud information for both cloud types

          IF ( USE_ACM_CLOUD ) THEN
!            WRITE(6,'(A,7(I8,1X),3(ES12.4,1X))')
!     &      'MYPE, JDATE, JTIME, COL, ROW, SUBGRID CLD CLBASE, CLTOP,FRAC, SUM(QLQD), SUM(QICE) = ',
!     &       MYPE, JDATE, JTIME, COL, ROW, CLBASE, CLTOP,FRAC, SUM(QLQD), SUM(QICE)
            CALL CAPTURE_ACM_CLOUD( JDATE, JTIME, COL, ROW, CLBASE, CLTOP,
     &                              FRAC, QLQD, QICE )
          END IF

299     CONTINUE

301     CONTINUE        !  end loop on columns COL
311   CONTINUE        !  end loop on rows    ROW

      RETURN          !  from main routine CLDPROC

91010 FORMAT( 3( A, :, I3, : ) )
      END SUBROUTINE CONVCLD_PROP_ACM
