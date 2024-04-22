
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
       SUBROUTINE CONVCLD_ACM ( CGRD, JDATE, JTIME, TSTEP,
     &                         CONV_DEP, SUBTRANS )

C-----------------------------------------------------------------------
C
C  FUNCTION: Convective cloud processor Models-3 science process:
C       MAIN ROUTINE calculates cloud characteristics, and uses them
C       to generate cumulative and net timestep deposition, cloud top,
C       cloud bottom, and pressure at the lifting level.
C
C       ICLDTYPE = 1 => computes raining cloud physics, mixing, chemistry,
C                       wet dep
C       ICLDTYPE = 2 => does the same for non-precip clouds utilizing saved
C                       info from RNCLD in the case of co-existing clouds
C
C  PRECONDITIONS REQUIRED:
C       Dates and times represented YYYYDDD:HHMMSS.
C
C  IDEA:   Aqueous chemistry operates on the half-hour for an internal
C          time step of one hour.
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
C       7/06 S.Roselle Modified for sulfate tracking model
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
C       7May 18 D. Schwede: Removed call to CZANGLE. COSZEN now calculated in ASX_DATA_MOD
C       Aug 2018 J. Pleim: convert to Z coords
C       Oct 2018 D. Wong: Declared array, F as allocatable and added subroutine ACMCLD 
C                         in the interface block
C       26 Nov 2018 S. Napelenok: ISAM implementation
C       Feb 2019 D. Wong: Implemented centralized I/O approach, removed all MY_N
C                         clauses
C       01 AUG 19 D.Wong: Modified code to work with two-way model and 
C       11 Nov 19 F. Sidi: Changed MSTEP to accomdate Centralized I/O changes       
C       30 Dec 19 S.L.Napelenok:  ddm-3d implementaiton for version 5.3.1
C       10 Jun 21 G. Sarwar: Replaced CB6R3M with CB6R5M

C-----------------------------------------------------------------------

      USE RUNTIME_VARS, ONLY: STM, CONVECTIVE_SCHEME
      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE CGRID_SPCS          ! CGRID mechanism species
      USE UTILIO_DEFN
      USE AQ_DATA
      USE AERO_DATA, ONLY : ASO4_IDX, ASO4AQH2O2_IDX, ASO4AQO3_IDX,
     &                      ASO4AQFEMN_IDX, ASO4AQMHP_IDX, ASO4AQPAA_IDX,
     &                      ASO4GAS_IDX, ASO4EMIS_IDX, ASO4ICBC_IDX,
     &                      OSO4AQH2O2_IDX, OSO4AQO3_IDX, OSO4AQFEMN_IDX,
     &                      OSO4AQMHP_IDX, OSO4AQPAA_IDX, OSO4_IDX, 
     &                      OSO4GAS_IDX, OSO4EMIS_IDX, OSO4ICBC_IDX,
     &                      AEROSPC_MAP, N_MODE, AE6ISOA, MAP_AERO
      USE PRECURSOR_DATA, ONLY: SULF_IDX, PRECURSOR_MAP, MAP_PRECURSOR
      USE RXNS_DATA, ONLY: MECHNAME       ! chemical mechanism data
      USE ASX_DATA_MOD,  ONLY: GRID_DATA, MET_DATA
      USE PHOT_MOD,      ONLY: RJ, RJ_RES, RJ_SUB, LH2O2_PHOTOLYSIS => LH2O2,
     &                   LHNO3_PHOTOLYSIS => LHNO3
      USE CENTRALIZED_IO_MODULE
#ifdef isam
      USE SA_DEFN, ONLY: ISAM, NSPC_SA, NTAG_SA, MAP_SAtoCGR, OTHRTAG,
     &                   ISAM_SPEC, DEPSUM_SAVE, DS4_SAVE, REMOV_SAVE,
     &                   ITAG, CONV_SADEP,
     &                   DEPSUM_AORGC_SAVE, DGLY1_SAVE, DMGLY1_SAVE,
     &                   REMOV_AORGC_SAVE
#endif

#ifdef sens
      USE DDM3D_DEFN, ONLY: SENGRID, NP, NPMAX, S_CONDEP, S_POLC,
     &                      S_CEND, S_REMOV, S_REMOVAC, S_CONDEP, 
     &                      S_TOTDEP, S_CCR, S_CBELOW, IPT,
     &                      S_CONC, S_BMOL, S_CBASE0, S_CBASEF,
     &                      S_BCLDWT, S_INCLOUD, S_OUTCLOUD, S_PCLD,
     &                      S_CONDIS, DDM3D_CONCMINL
#endif

#ifdef mpas
      use util_module, only : nextime, time2sec, sec2time, secsdiff, currstep
#endif

      IMPLICIT NONE

C...........INCLUDES

      INCLUDE SUBST_CONST               ! constants
      INCLUDE SUBST_FILES_ID            ! file name parameters

C...........Arguments

      REAL, INTENT( INOUT )    :: CGRD( :,:,:,: )
      INTEGER, INTENT( IN )    :: JDATE
      INTEGER, INTENT( IN )    :: JTIME
      INTEGER, INTENT( IN )    :: TSTEP( 3 )

#ifdef mpas
      integer, save :: mpas_cmaq_freq
#endif
      REAL,    INTENT( INOUT ) :: CONV_DEP( :,:,: )
      REAL,    INTENT( OUT )   :: SUBTRANS( :,:,: )

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

C factor convert 1/min to 1/sec
      REAL, PARAMETER :: MINPERSEC = 1.0 / 60.0 

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
      REAL, PARAMETER :: M2PHA_OVER_GPKG = 10.0

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
      REAL, ALLOCATABLE, SAVE :: DZH( : )
      REAL, ALLOCATABLE, SAVE :: CCR ( :,: )
      REAL, ALLOCATABLE, SAVE :: CONC( :,: )
      REAL, ALLOCATABLE, SAVE :: CBELOW( : )
C-------------------------------------------

      LOGICAL, SAVE :: FIRSTIME = .TRUE. ! flag for first pass thru

      CHARACTER( 16 ) :: PNAME = 'CONVCLD_ACM'   ! process name
      CHARACTER( 16 ) :: VARNM          ! variable name for IOAPI to get

      CHARACTER( 16 ), SAVE :: RC_NAME  ! RC name: old is RC and new is RCA

      INTEGER, ALLOCATABLE, SAVE :: SURCLDMX( : ) ! cloud mixing surrogate for stm

      INTEGER          ATIME            ! time diff from half-hour
      INTEGER          CLTOP            ! model LAY containing cloud top
      INTEGER          COL              ! column loop counter
      INTEGER          ROW              ! row loop counter
      INTEGER          CTOP             ! dummy variable for cloud top layer
      INTEGER          FINI             ! ending position
      INTEGER          I599C            ! entrainment solver iteration counter
      INTEGER          LAY              ! layer loop counter
      INTEGER          M                ! mode index
      INTEGER          MDATE            ! process date
      INTEGER          MTIME            ! process time (half-hour)
      INTEGER, SAVE :: MSTEP            ! met file time step (hhmmss)
      INTEGER, SAVE :: SDATE            ! met file start date
      INTEGER          SPC              ! liquid species loop counter
      INTEGER          STEP             ! step loop counter
      INTEGER          STRT             ! starting position
      INTEGER, SAVE :: STIME            ! met file start time
      INTEGER          VAR              ! variable loop counter
      INTEGER          I                ! variable loop counter

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
      REAL             JH2O2_BAR        ! mean H2O2 photolysis rate in subgrid cloud, 1/min
      REAL             JHNO3_BAR        ! mean HNO3 photolysis rate in subgrid cloud, 1/min
      REAL, SAVE    :: METSTEP          ! timestep on the met file
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
      REAL, ALLOCATABLE, SAVE :: AECONCMIN( : ) ! array of minimum concentrations
      REAL, ALLOCATABLE, SAVE :: BMOL   ( : )   ! mol/m2 species below cloud
      REAL, ALLOCATABLE, SAVE :: CBASE0 ( : )   ! initial ave trace gas mix rat below cld
      REAL, ALLOCATABLE, SAVE :: CBASEF ( : )   ! final ave trac gas mix rat blw cld (mol/mol)
      REAL, ALLOCATABLE, SAVE :: CEND   ( : )   ! ending equiv gas phase conc (mol/mol)
      REAL, ALLOCATABLE, SAVE :: POLC   ( : )   ! ave vert conc incloud mol sp/m2 and mol sp/ mol air
      REAL, ALLOCATABLE, SAVE :: REMOV  ( : )   ! mol/m2 or mm*mol/lit scavenged
      REAL          :: DENSL( NLAYS )           ! air density (kg/m3)
      REAL, ALLOCATABLE, SAVE :: F      ( : )   ! cloud entrainment fraction to be solved for
      REAL          :: FSIDE( NLAYS )           ! sidewall entrainment vertical profile
      REAL          :: LWC  ( NLAYS )           ! liq wat cont of cloud in kg H2O/m3 air
      REAL, ALLOCATABLE, SAVE :: QICE ( : )     ! ice mixing ratio in cloud
      REAL, ALLOCATABLE, SAVE :: QLQD ( : )     ! actual liq. wat. mix ratio in cloud
      REAL          :: QVC  ( NLAYS )      ! saturation wat vap mix ratio at T1
      REAL          :: QWAT ( NLAYS )      ! liq wat mix rat, taken as total condensed water (ice + liq) profile (Eq.4, W&T)
      REAL          :: RHOM2( NLAYS )      ! mol/m2 air
      REAL          :: TCLD ( NLAYS )      ! temp of cloudy air parcel

      REAL             FRACMAX             ! max frac cov for NP cld
      REAL             PLCL                ! pressure at LCL
      REAL             QMAX                ! pertbd w.. mix rat of parcel
      REAL          :: RAIN( NCOLS,NROWS ) ! this timestep rainfall (mm/hr)
      REAL             BCLDWT              ! below cloud weighting function
      REAL, ALLOCATABLE, SAVE :: CONCMINL( :,: ) ! minimum concentrations for each species and layer
      REAL             INCLOUD        ! final conc in cloud after mix and chem [mol/mol]
      REAL             OUTCLOUD       ! final conc outside    "    "   "   "     "
      REAL, ALLOCATABLE, SAVE :: PCLD    ( :,: ) ! mol sp/mol air in cloud

      REAL          :: RC   ( NCOLS,NROWS )        ! hourly convective rainfall (cm)
      REAL          :: PBL  ( NCOLS,NROWS )        ! PBL height (m)
      REAL          :: DZZ  ( NCOLS,NROWS,NLAYS )  ! computed gridded vble
      REAL          :: DZZL ( NLAYS )              ! grid cell delta Z
      REAL          :: PRES ( NCOLS,NROWS,NLAYS )  ! file gridded vble
      REAL          :: PRESL( NLAYS )              ! grid cell pressure
      REAL          :: JH2O2( NLAYS )              ! H2O2 photolysis rate (1/min)   
      REAL          :: JHNO3( NLAYS )              ! HNO3 photolysis rate (1/min)   
      REAL          :: QAD  ( NLAYS )              ! moist adiab. sat. mix ratio
      REAL          :: QV   ( NCOLS,NROWS,NLAYS )  ! input gridded vble
      REAL          :: QVL  ( NLAYS )              ! grid cell sp. hum.
      REAL          :: TA   ( NCOLS,NROWS,NLAYS )  ! input gridded vble
      REAL          :: TAL  ( NLAYS )              ! grid cell temp
      REAL          :: TSAT ( NLAYS )              ! parcel temp along moist adiabat @ half levels
      REAL          :: ZH   ( NCOLS,NROWS,NLAYS )  ! mid-layer height (m)
      REAL          :: ZF   ( NCOLS,NROWS,NLAYS )  ! level/layer-face height (m)

      INTEGER         ALLOCSTAT

C Gridded meteorology data: Golam Sarwar, July 1, 2011 
C Latitude and longitude for zenith angle calculation:     
      REAL             COSZ                        ! local cosine of zenith angle

      CHARACTER( 120 ) :: XMSG = ' '   ! Exit status message


C...........Statement Functions

      REAL             ESAT            ! sat vap pres (Pa) as fn of T (deg K)
      REAL             QSAT            ! sat water vapor mixing ratio

      REAL             T               ! temperature dummy arg
      REAL             E               ! sat vapor pressure dummy arg
      REAL             P               ! pressure dummy arg

#ifdef isam
      REAL, ALLOCATABLE, SAVE :: SA_BASE0  ( :,: )
      REAL, ALLOCATABLE, SAVE :: SA_POLC   ( :,: )
      REAL, ALLOCATABLE, SAVE :: SA_CEND   ( :,: )
      REAL, ALLOCATABLE, SAVE :: SA_PCLD   ( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SA_BASEF  ( :,: )
      REAL, ALLOCATABLE, SAVE :: SA_CCR    ( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SA_CBELOW ( :,: )
      REAL, ALLOCATABLE, SAVE :: SA_REMOV  ( :,: )
      REAL, ALLOCATABLE, SAVE :: SA_BMOL   ( :,: )
      REAL, ALLOCATABLE, SAVE :: SA_CONC   ( :,:,: )
      REAL, ALLOCATABLE, SAVE :: SA_DS4    ( : )
      REAL, ALLOCATABLE, SAVE :: SA_INCLOUD( : )

      INTEGER, SAVE           :: S_SO2, S_SO4J, S_SULF, S_N2O5
      INTEGER, SAVE           :: C_SO2, C_SO4J, C_SULF
      INTEGER, SAVE           :: S_GLY, S_MGLY, S_AORGCJ
      INTEGER, SAVE           :: C_GLY, C_MGLY, C_AORGCJ
      REAL, ALLOCATABLE, SAVE :: SA_DCSOA_GLY    ( : )
      REAL, ALLOCATABLE, SAVE :: SA_DCSOA_MGLY   ( : )
      INTEGER CSPC

      REAL BLNC
      REAL                    :: SA_SUM
#endif


      INTERFACE
        SUBROUTINE ACMCLD ( NSP, NLAYS, F, C, DZH, CBELOW, CLBASE, CLTOP,
     &                      FRAC, TCLIFE, DTCLD )
          INTEGER, INTENT( IN ) :: NSP
          INTEGER, INTENT( IN ) :: NLAYS
          REAL, INTENT( IN )    :: F( : )
          REAL, INTENT( INOUT ) :: C( :, : )
          REAL, INTENT( IN )    :: DZH( : )
          REAL, INTENT( INOUT ) :: CBELOW( : )
          INTEGER, INTENT( IN ) :: CLBASE
          INTEGER, INTENT( IN ) :: CLTOP
          REAL, INTENT( IN )    :: FRAC
          REAL, INTENT( IN )    :: TCLIFE
          REAL, INTENT( IN )    :: DTCLD
        END SUBROUTINE ACMCLD

        SUBROUTINE SCAVWDEP ( JDATE, JTIME, WTBAR, WCBAR, TBARC, PBARC,
     &                        CTHK1, AIRM, PRATE1, TAUCLD, POLC, CEND,
     &                        REMOV, REMOVAC, ALFA0, ALFA2, ALFA3 )
          INTEGER, INTENT( IN )  :: JDATE, JTIME
          REAL,    INTENT( IN )  :: WTBAR, WCBAR, TBARC, PBARC,
     &                              CTHK1, AIRM, PRATE1, TAUCLD
          REAL,    INTENT( IN )  :: POLC ( : )
          REAL,    INTENT( OUT ) :: CEND( : ), REMOV( : )
          REAL,    INTENT( OUT ) :: REMOVAC
          REAL,    INTENT( OUT ) :: ALFA0, ALFA2, ALFA3
        END SUBROUTINE SCAVWDEP

        SUBROUTINE AQ_MAP( JDATE, JTIME, WTBAR, WCBAR, TBARC, PBARC,
     &                     CTHK1, AIRM, PRATE1, TAUCLD, POLC, CEND,
     &                     REMOV, REMOVAC, ALFA0, ALFA2, ALFA3, COSZ )
          INTEGER, INTENT( IN )    :: JDATE, JTIME
          REAL,    INTENT( IN )    :: WTBAR, WCBAR, TBARC, PBARC,
     &                                CTHK1, AIRM, PRATE1, TAUCLD
          REAL,    INTENT( IN )    :: POLC ( : )
          REAL,    INTENT( INOUT ) :: CEND( : ), REMOV( : )
          REAL,    INTENT( INOUT ) :: REMOVAC
          REAL,    INTENT( IN )    :: ALFA0, ALFA2, ALFA3, COSZ
        END SUBROUTINE AQ_MAP
      END INTERFACE

      ESAT( T ) = VP0PA * EXP( C303 - ( C302 / T ) )

      QSAT( E, P ) = MVOMA * ( E / ( P - E ) )

C-----------------------------------------------------------------------
C  begin body of subroutine CONVCLD_ACM

C...INITIALIZATION for the CONVCLD_ACM module:
C...  event-statistics variables.

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

C...Sulfur tracking
        IF ( STM ) THEN
          CALL MAP_AERO()
          CALL MAP_PRECURSOR()
        END IF

        IF ( N_AE_SPC .GT. 0 ) THEN
          ALLOCATE ( AECONCMIN( N_AE_SPC ), STAT = ALLOCSTAT )
          IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating AECONCMIN'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          END IF
          CALL SET_AECONCMIN ( AECONCMIN )

        END IF

C cccccccccccccccccccc enable backward compatiblity ccccccccccccccccccccc

         IF (RCA_AVAIL) THEN
           RC_NAME = 'RCA'
        ELSE
           RC_NAME = 'RC'
        END IF

C...store met file time, date, and step information and compute
C...  the met timestep in hours

        SDATE = cio_model_sdate
        STIME = cio_model_stime
        MSTEP = file_tstep(f_met) 

#ifdef mpas
          call get_env (mpas_cmaq_freq, 'mpas_cmaq_freq', 1)
          METSTEP = (FLOAT( TIME2SEC( MSTEP ) )/FLOAT(mpas_cmaq_freq)) / 3600.0
          !metstep is coupling frequency but rain is per mpas time step
#else

        METSTEP = FLOAT( TIME2SEC( MSTEP ) ) / 3600.0
#endif

C...check convective precipitation on met files to determine if WRF used
C...  a convective parameterization
C...in coordination with MCIPv4.0, negative values will be loaded into the RC
C...  field if a convective parameterization was not used in the WRF simulation

        call interpolate_var (RC_NAME, sdate, stime, RC)

        IF ( .NOT. CONVECTIVE_SCHEME) RETURN

C...allocate saved arrays

        ALLOCATE ( F( NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating F'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE ( DZH( NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating DZH'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        MXSPCS = NSPCSD

        ALLOCATE ( CCR   ( MXSPCS,NLAYS ),
     &             CONC  ( MXSPCS,NLAYS ),
     &             CBELOW( MXSPCS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating CCR, CONC or CBELOW'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE ( BMOL   ( MXSPCS ),
     &             CBASE0 ( MXSPCS ),
     &             CBASEF ( MXSPCS ),
     &             CEND   ( MXSPCS ),
     &             POLC   ( MXSPCS ),
     &             REMOV  ( MXSPCS ), 
     &             QLQD   ( NLAYS ), 
     &             QICE   ( NLAYS ), 
     &             STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating BMOL, CBASE0, CBASEF, CEND, POLC, REMOV,'
     &         // ' QLQD or QICE'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE ( CONCMINL( MXSPCS,NLAYS ),
     &             PCLD    ( MXSPCS,NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating CONCMINL or PCLD'
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
          XKM = (cell_area(COL,ROW)**0.5)/1000
          RCRITW(COL,ROW) = 0.879 + SQRT( 1.0 / ( 100.0 + XKM * XKM  ))
          RCRITL(COL,ROW) = 0.839 + SQRT( 1.0 / ( 50.0 + 0.5 * XKM ** 3 ) )
        END DO
      END DO
#else
        XKM = REAL( XCELL_GD / 1000 )
        RCRITW = 0.879 + SQRT( 1.0 / ( 100.0 + XKM * XKM ) )
        RCRITL = 0.839 + SQRT( 1.0 / ( 50.0 + 0.5 * XKM ** 3 ) )
#endif

C...Sulfur tracking
        IF ( STM ) THEN

C...initialize surrogate array
          ALLOCATE ( SURCLDMX( MXSPCS ), STAT = ALLOCSTAT )
          DO SPC = 1, NSPCSD
            SURCLDMX( SPC ) = SPC
          END DO

C...set cloud mixing surrogates for the sulfate tracking species

          DO M = 1, N_MODE

            SURCLDMX( AEROSPC_MAP( ASO4GAS_IDX, M ) ) = AEROSPC_MAP( ASO4_IDX,M )
            SURCLDMX( AEROSPC_MAP( ASO4EMIS_IDX,M ) ) = AEROSPC_MAP( ASO4_IDX,M )
            SURCLDMX( AEROSPC_MAP( ASO4ICBC_IDX,M ) ) = AEROSPC_MAP( ASO4_IDX,M )

C...  for the accumulation mode, add aqueous tracked species

            IF ( M .EQ. 2 ) THEN

              SURCLDMX( AEROSPC_MAP( ASO4AQH2O2_IDX,M ) ) = AEROSPC_MAP( ASO4_IDX,M )
              SURCLDMX( AEROSPC_MAP( ASO4AQO3_IDX,  M ) ) = AEROSPC_MAP( ASO4_IDX,M )
              SURCLDMX( AEROSPC_MAP( ASO4AQFEMN_IDX,M ) ) = AEROSPC_MAP( ASO4_IDX,M )
              SURCLDMX( AEROSPC_MAP( ASO4AQMHP_IDX, M ) ) = AEROSPC_MAP( ASO4_IDX,M )
              SURCLDMX( AEROSPC_MAP( ASO4AQPAA_IDX, M ) ) = AEROSPC_MAP( ASO4_IDX,M )

            END IF

            IF ( ( AE6ISOA ) .OR.
     &           ( INDEX( MECHNAME, 'CRACMM'     ) .GT. 0 ) .OR. 
     &           ( INDEX( MECHNAME, 'CB6R3_AE7'  ) .GT. 0 ) .OR.
     &           ( INDEX( MECHNAME, 'CB6R5_AE7'  ) .GT. 0 ) .OR.
     &           ( INDEX( MECHNAME, 'CB6R5M_AE7' ) .GT. 0 ) ) THEN
    
C...  for the accumulation mode, add aqueous tracked species

              IF ( M .EQ. 2 ) THEN

                SURCLDMX( AEROSPC_MAP( OSO4GAS_IDX,   M ) ) = AEROSPC_MAP( OSO4_IDX,M )
                SURCLDMX( AEROSPC_MAP( OSO4EMIS_IDX,  M ) ) = AEROSPC_MAP( OSO4_IDX,M )
                SURCLDMX( AEROSPC_MAP( OSO4ICBC_IDX,  M ) ) = AEROSPC_MAP( OSO4_IDX,M )
                SURCLDMX( AEROSPC_MAP( OSO4AQH2O2_IDX,M ) ) = AEROSPC_MAP( OSO4_IDX,M )
                SURCLDMX( AEROSPC_MAP( OSO4AQO3_IDX,  M ) ) = AEROSPC_MAP( OSO4_IDX,M )
                SURCLDMX( AEROSPC_MAP( OSO4AQFEMN_IDX,M ) ) = AEROSPC_MAP( OSO4_IDX,M )
                SURCLDMX( AEROSPC_MAP( OSO4AQMHP_IDX, M ) ) = AEROSPC_MAP( OSO4_IDX,M )
                SURCLDMX( AEROSPC_MAP( OSO4AQPAA_IDX, M ) ) = AEROSPC_MAP( OSO4_IDX,M )

              END IF

            END IF

          END DO

          SPC = 0
          STRT = AE_STRT
          FINI = AE_STRT - 1 + N_AE_SPC
          DO VAR = STRT, FINI
            SPC = SPC + 1
            WRITE( XMSG, '(i6,1x,A16,1x,i6,1x,a16)') VAR, AE_SPC( SPC ), SURCLDMX( VAR ), AE_SPC( SURCLDMX( VAR )-AE_STRT+1 )
             CALL M3MESG ( XMSG )
          END DO

        END IF ! stm

#ifdef isam
C Move this to a better place later
        S_SO2  = INDEX1( 'SO2', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )
        S_SO4J = INDEX1( 'ASO4J', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )
        S_SULF = INDEX1( 'SULF', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )
        S_N2O5 = INDEX1( 'N2O5', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )

        C_SO2  = INDEX1( 'SO2',   N_GC_SPC, GC_SPC )
        C_SO4J = INDEX1( 'ASO4J', N_AE_SPC, AE_SPC ) + 1 + N_GC_SPC
        C_SULF = INDEX1( 'SULF',  N_GC_SPC, GC_SPC )

        ALLOCATE ( SA_BASE0  ( NSPC_SA, NTAG_SA ),
     &             SA_BASEF  ( NSPC_SA, NTAG_SA ),
     &             SA_POLC   ( NSPC_SA, NTAG_SA ),
     &             SA_CEND   ( NSPC_SA, NTAG_SA ),
     &             SA_PCLD   ( NSPC_SA, NLAYS, NTAG_SA ),
     &             SA_CCR    ( NSPC_SA, NLAYS, NTAG_SA ),
     &             SA_CBELOW ( NSPC_SA, NTAG_SA ),
     &             SA_REMOV  ( NSPC_SA, NTAG_SA ),
     &             SA_BMOL   ( NSPC_SA, NTAG_SA ),
     &             SA_CONC   ( NSPC_SA, NLAYS, NTAG_SA ),
     &             SA_DS4    ( NTAG_SA ),
     &             SA_INCLOUD( NTAG_SA ),
     &             STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating SA arrays'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        S_GLY    = INDEX1( 'GLY', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )
        S_MGLY   = INDEX1( 'MGLY', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )
        S_AORGCJ = INDEX1( 'AORGCJ', NSPC_SA, ISAM_SPEC(:,OTHRTAG) )

        C_GLY    = INDEX1( 'GLY',   N_GC_SPC, GC_SPC )
        C_MGLY   = INDEX1( 'MGLY', N_GC_SPC, GC_SPC )
        C_AORGCJ = INDEX1( 'AORGCJ',  N_AE_SPC, AE_SPC ) + 1 + N_GC_SPC
        ALLOCATE ( SA_DCSOA_GLY ( NTAG_SA ),
     &             SA_DCSOA_MGLY ( NTAG_SA ),
     &             STAT = ALLOCSTAT )
#endif

#ifdef sens
        IF ( .NOT. ALLOCATED( S_CEND ) ) ALLOCATE ( S_CEND ( NPMAX, MXSPCS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_CEND'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        IF ( .NOT. ALLOCATED( S_POLC ) ) ALLOCATE ( S_POLC ( NPMAX, MXSPCS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_POLC'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        IF ( .NOT. ALLOCATED( S_REMOV ) ) ALLOCATE ( S_REMOV ( NPMAX, MXSPCS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_REMOV'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        IF ( .NOT. ALLOCATED( S_REMOVAC ) ) ALLOCATE ( S_REMOVAC ( NPMAX ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_REMOVAC'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        S_REMOVAC = 0.0

        IF ( .NOT. ALLOCATED( S_CCR ) ) ALLOCATE ( S_CCR ( NPMAX,MXSPCS,NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_CCR'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        S_CCR = 0.0D0

        IF ( .NOT. ALLOCATED( S_CBELOW ) ) ALLOCATE ( S_CBELOW ( NPMAX,MXSPCS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_REMOVAC'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        S_CBELOW = 0.0D0

        ALLOCATE ( S_CONC  ( NPMAX,MXSPCS,NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_CONC'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE ( S_BMOL   ( NPMAX,MXSPCS ),
     &             S_CBASE0 ( NPMAX,MXSPCS ),
     &             S_CBASEF ( NPMAX,MXSPCS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_BMOL, S_CBASE0, or S_CBASEF'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE ( S_BCLDWT  ( NPMAX,MXSPCS,NLAYS ),
     &             S_INCLOUD ( NPMAX,MXSPCS,NLAYS ),
     &             S_OUTCLOUD( NPMAX,MXSPCS,NLAYS ),
     &             S_PCLD    ( NPMAX,MXSPCS,NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating S_BCLDWT, S_INCLOUD, S_OUTCLOUD or S_PCLD'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
#endif

      END IF   ! Firstime

      IF ( .NOT. CONVECTIVE_SCHEME) RETURN

C...check option for processing clouds on the synchronization timestep

      IF ( SYNCCLD ) THEN

        MDATE = JDATE
        MTIME = JTIME

C...set the cloud timestep (=adv timestep)

        STEP  = TIME2SEC( TSTEP( 2 ) )         ! synchronization timestep
        DTCLD = REAL( STEP )

C...set time to the midpoint of this timestep for data interpolation

        CALL NEXTIME ( MDATE, MTIME, SEC2TIME( STEP / 2 ) )

C...otherwise, revert back to processing convective cloud once per hour
C...  on the half hour

      ELSE

C...Check to see if this time step contains the half-hour
C...  if it does not, then return

        MDATE = JDATE
        MTIME = 10000 * ( JTIME / 10000 )      ! on the current hour
        STEP  = TIME2SEC( TSTEP( 2 ) )         ! synchronization timestep
        DTCLD = TCLIFE

C...  set mdate:mtime to one-half step before the half-hour

        CALL NEXTIME ( MDATE, MTIME, SEC2TIME( 1800 - ( STEP / 2 ) ) )

        ATIME = SECSDIFF( MDATE, MTIME, JDATE, JTIME )

        IF ( ( ATIME .LT. 0 ) .OR. ( ATIME .GE. STEP ) ) RETURN

C...the current timestep overlaps the half hour point
C...  set the time to the half hour for data interpolation

        MTIME = 10000 * ( JTIME / 10000 ) + 3000
      
      END IF

C...clear arrays that capture ACM cloud results

C...ACTUAL SCIENCE PROCESS (loop on internal process time steps):
C...  Interpolate time dependent layered input variables
C...  (reading those variables for which it is necessary)

C...  Get ambient temperature (K)
      call interpolate_var ('TA', mdate, mtime, TA)

C...Get specific humidity (kg H2O / kg air)
      call interpolate_var ('QV', mdate, mtime, QV)

C...Get level heights / layer faces (m)
      call interpolate_var ('ZF', mdate, mtime, ZF)

C...Get mid-layer heights (m)
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
          RAIN( COL,ROW ) = 10.0 * RC( COL,ROW ) / METSTEP
        END DO
      END DO
      IF ( MINVAL( RAIN ) .LT. 0.0 ) THEN
        XMSG = 'NEGATIVE RAIN...PROBABLY BAD MET DATA... in' // MET_CRO_2D
        CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
      END IF
      WHERE ( RAIN .GE. RTHRESH ) CONV_DEP( :,:,N_SPC_WDEP + 6 ) = 1.0

C...Loop through all grid cells

      DO ROW = 1, NROWS
        DO COL = 1, NCOLS

          QLQD = 0.0
          QICE = 0.0

          CLBASE = NLAYS
          CLTOPUSTBL = NLAYS
          QMAX  = 0.0
          PLCL  = 0.0
          SRCLAY = NLAYS

          DO LAY = 1, NLAYS
            QAD( LAY ) = 0.0
            PRESL( LAY ) = PRES( COL,ROW,LAY )
            TAL( LAY )   = TA( COL,ROW,LAY )
            QVL( LAY )   = QV( COL,ROW,LAY )
            DZZL( LAY )  = DZZ( COL,ROW,LAY )
            DENSL( LAY ) = PRESL( LAY )
     &                   / ( RDGAS * TAL( LAY ) * ( 1.0 + RWVAP * QVL( LAY ) / RDGAS ) )
            JH2O2( LAY ) = RJ_SUB( COL,ROW,LAY,LH2O2_PHOTOLYSIS )
            JHNO3( LAY ) = RJ_SUB( COL,ROW,LAY,LHNO3_PHOTOLYSIS )
          END DO

C...load aerosol minimum concentrations into the "CONCMINL" array
C...  initialize all species to CMIN

!         CONCMINL = CMIN
          CONCMINL = 1.0E-25

#ifdef sens
          DDM3D_CONCMINL = 1.0D-15
#endif

C...  set minimum for aerosol species

          SPC = 0
          STRT = AE_STRT
          FINI = AE_STRT - 1 + N_AE_SPC
          DO VAR = STRT, FINI
            SPC = SPC + 1
            DO LAY = 1, NLAYS
              CONCMINL( VAR,LAY ) = AECONCMIN( SPC ) / DENSL( LAY )
            END DO
          END DO

          DO LAY = 1, NLAYS
            DO SPC = 1, NSPCSD
              CONC( SPC,LAY ) = MAX( CGRD( COL,ROW,LAY,SPC ),
     &                                  CONCMINL( SPC,LAY ) )
#ifdef sens
              DO NP = 1, NPMAX
                IF ( CGRD( COL,ROW,LAY,SPC ) .LT. CONCMINL( SPC, LAY ) ) THEN
                  S_CONC( NP, SPC, LAY ) = 0.0D0
                ELSE
                  S_CONC( NP, SPC, LAY ) = REAL ( SENGRID( COL, ROW, LAY, NP, SPC ), 8 )
                END IF
              END DO
#endif
            END DO
          END DO

#ifdef isam         
          DO SPC = 1, NSPC_SA
            CSPC = MAP_SAtoCGR(SPC)
            DO ITAG = 1, NTAG_SA
              DO LAY = 1, NLAYS
                SA_CONC( SPC,LAY,ITAG ) = MAX( ISAM( COL,ROW,LAY,SPC,ITAG ),
c    &                                    CONCMINL( CSPC,LAY ) )
     &                                    1.0E-30 )
              END DO
            END DO
          END DO
#endif


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

          CONV_DEP( COL,ROW, N_SPC_WDEP + 2 ) = PLCL

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

            CLBASE = NLAYS
            DO LAY = NLAYS, 2, -1
              IF ( PRESL( LAY ) .LE. PLCL ) CLBASE = LAY
            END DO

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
            IF ( ICLDTYPE .NE. 1 ) CYCLE ! Loop to next row-column coordinate

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

          CONV_DEP( COL,ROW, N_SPC_WDEP + 3 ) = FLOAT( CLBASE )

          IF ( ICLDTYPE .EQ. 1 ) THEN    !  store raining cloud top and proceed

            CONV_DEP( COL,ROW, N_SPC_WDEP + 4 ) = FLOAT( CLTOP )

          ELSE                      !  get cloud top for either CNP or PFW

            IF ( ZLCL .GT. PBL( COL,ROW ) ) CYCLE ! Loop to next row-column coordinate

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
               IF ( RLHSRC .GE. RCRITL(COL,ROW) )
     &            FRAC = 1.0 - SQRT( ( 1.0 - RLHSRC ) / ( 1.0 - RCRITL(COL,ROW) ) )
            ELSE   ! water
               IF ( RLHSRC .GE. RCRITW(COL,ROW) )
     &            FRAC = 1.0 - SQRT( ( 1.0 - RLHSRC ) / ( 1.0 - RCRITW(COL,ROW) ) )
#else
               IF ( RLHSRC .GE. RCRITL )
     &            FRAC = 1.0 - SQRT( ( 1.0 - RLHSRC ) / ( 1.0 - RCRITL ) )
            ELSE   ! water
               IF ( RLHSRC .GE. RCRITW )
     &            FRAC = 1.0 - SQRT( ( 1.0 - RLHSRC ) / ( 1.0 - RCRITW ) )
#endif
            END IF
            FRAC = MAX( 0.0, MIN( FRAC, 0.95 ) )

            IF ( FRAC .LT. 0.01 ) CYCLE  ! Loop to next row-column coordinate

            CONV_DEP( COL,ROW, N_SPC_WDEP + 5 ) = FLOAT( CLTOP ) ! store NP cloud top
            CONV_DEP( COL,ROW, N_SPC_WDEP + 8 ) = FRAC

          END IF  ! end of existence, depth and frac cov calc for
                  ! either PFW or CNP clouds

C...Now cloud existence is established, initialize various
C...  variables needed for rest of computations

C...First, get mol air/m2 at each layer, initialize FSIDE

          DO LAY = 1, NLAYS
            RHOM2( LAY ) = DENSL( LAY ) * DZZL( LAY ) * 1.0E3 / MWAIR
            FSIDE( LAY ) = 0.0
          END DO

          DO SPC = 1, NSPCSD
            REMOV( SPC ) = 0.0  ! mol/m2 or mm*mol/lit scavenged
            CEND ( SPC ) = 0.0  ! ending equiv gas phase conc (mol/mol)
            BMOL ( SPC ) = 0.0  ! mol/m2 species below cloud
            POLC ( SPC ) = 0.0  ! mol/m2 species in cloud

            DO LAY = 1, NLAYS
              PCLD( SPC,LAY ) = 0.0  ! mol sp/mol air in cloud
            END DO

          END DO

#ifdef sens
          S_REMOV  = 0.0D0
          S_CEND   = 0.0D0
          S_BMOL   = 0.0D0
          S_POLC   = 0.0D0
          S_BCLDWT = 0.0D0
          S_PCLD   = 0.0D0
#endif

C...compute no. of moles air below cloud base and inverse

          AIRMB0 = 0.0
          DO LAY = 1, CLBASE - 1
            AIRMB0 = AIRMB0 + RHOM2( LAY )
          END DO

C...take the inverse

          AIRMBI = 1.0 / AIRMB0

C...below cloud base

          DO LAY = 1, CLBASE - 1

C...determine no. of mol/m2 of trace gas

            DO SPC = 1, NSPCSD
              BMOL( SPC ) = BMOL( SPC ) + CONC( SPC,LAY ) * RHOM2( LAY )
#ifdef sens
              DO NP = 1, NPMAX
                S_BMOL( NP,SPC ) = S_BMOL( NP,SPC )
     &                    + S_CONC( NP,SPC,LAY ) * REAL ( RHOM2( LAY ), 8 )
              END DO
#endif
            END DO

          END DO

C...determine average trace gas mixing ratio below cloud level

          DO SPC = 1, NSPCSD
            CBASE0( SPC ) = BMOL( SPC ) * AIRMBI
            CBASEF( SPC ) = CBASE0( SPC )
#ifdef sens
            DO NP = 1, NPMAX
              S_CBASE0( NP,SPC ) = S_BMOL( NP,SPC ) * REAL ( AIRMBI, 8 )
              S_CBASEF( NP,SPC ) = S_CBASE0( NP,SPC )
            END DO
#endif
          END DO

#ifdef isam
          SA_BMOL  = 0.0
          SA_REMOV = 0.0
          SA_CEND  = 0.0
          SA_PCLD  = 0.0
          SA_POLC  = 0.0

          DO ITAG = 1, NTAG_SA
            DO LAY = 1, CLBASE - 1
              DO SPC = 1, NSPC_SA
                SA_BMOL( SPC,ITAG ) = SA_BMOL( SPC,ITAG ) +
     &                              + SA_CONC( SPC,LAY,ITAG )
     &                              * RHOM2( LAY )
              END DO
            END DO
          END DO

          DO ITAG = 1, NTAG_SA
            DO SPC = 1, NSPC_SA
              SA_BASE0(SPC,ITAG) = SA_BMOL( SPC,ITAG ) * AIRMBI
              SA_BASEF(SPC,ITAG) = SA_BASE0(SPC,ITAG)
            END DO
          END DO
#endif


C...Initialize variables needed for entrainment and in-cloud properties solver

          QXS =   0.0  ! integrated excess water over grid cell nec. for rnout
          AIRM =  0.0  ! total air mass (mol/m2) in cloudy layers
          PBARC = 0.0  ! in-cloud average pressure
          CTHK  = 0.0  ! cloud thickness (m)
          WCBAR = 0.0  ! condensed wat cont (kg/m2) integ. thru cloud depth
          WTBAR = 0.0  ! total wat cont (kg/m2) integrated thru cloud depth
          TBARC = 0.0  ! cloud mean temp (K)
          JH2O2_BAR = 0.0 ! cloud H2O2 photolysis, 1/min
          JHNO3_BAR = 0.0 ! cloud HNO3 photolysis, 1/min


C...Determine condensed water content and entrainment at each cloud level
C...Determine FSIDE profile for raining clouds; side entrainment
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

C...weighted photolysis rate 
            JH2O2_BAR = JH2O2_BAR + JH2O2( LAY ) * DZZL( LAY ) * LWC( LAY )     
            JHNO3_BAR = JHNO3_BAR + JHNO3( LAY ) * DZZL( LAY ) * LWC( LAY )     

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
          ELSE

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
            IF ( FRAC .LT. 0.01 ) CYCLE ! Loop to next row-column coordinate

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
              JH2O2_BAR = JH2O2_BAR + JH2O2( LAY ) * DZZL( LAY ) * LWC( LAY )
              JHNO3_BAR = JHNO3_BAR + JHNO3( LAY ) * DZZL( LAY ) * LWC( LAY )
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
            IF ( FRAC .LT. 0.01 ) CYCLE ! Loop to next row-column coordinate
            
            CONV_DEP( COL,ROW, N_SPC_WDEP + 7 ) = FRAC

          END IF                        ! target of cloudtype split


C...Begin mixing section, perform first for raining clouds using
C...  modified form of original Walcek mixing for RADM: mixing
C...  limited to 1 layer above cloud top; next for CNP or PFW clouds
C...  using direct exchange mixing mechanism by McHenry.

          DO SPC = 1, MXSPCS
            DO LAY = 1, NLAYS
              CCR( SPC, LAY ) = CONC( SPC,LAY )
#ifdef sens
              DO NP = 1, NPMAX
                S_CCR( NP,SPC, LAY ) = S_CONC( NP,SPC,LAY )
              END DO
#endif
            END DO
            CBELOW( SPC ) = CBASE0( SPC )
#ifdef sens
            DO NP = 1, NPMAX
              S_CBELOW( NP,SPC ) = S_CBASE0( NP,SPC )
            END DO
#endif 
          END DO

! -- Need to use dens * delz for ACM mixing
          DO LAY = 1, NLAYS
            DZH(LAY) = RHOM2( LAY )
          ENDDO
          LAY = CLBASE - 1

          DZH(LAY) = AIRMB0

#ifdef isam
          DO ITAG = 1, NTAG_SA
            DO SPC = 1, NSPC_SA
              DO LAY = 1, NLAYS
                SA_CCR( SPC, LAY,ITAG ) = SA_CONC( SPC,LAY,ITAG )
              END DO
              SA_CBELOW( SPC,ITAG ) = SA_BASE0( SPC, ITAG )
            END DO
          END DO
#endif

          CALL ACMCLD ( NSPCSD, NLAYS, F, CCR, DZH, CBELOW, CLBASE, CLTOP,
     &                  FRAC, TCLIFE, DTCLD )

#ifdef isam
          DO ITAG = 1, NTAG_SA
            CALL ACMCLD ( NSPC_SA, NLAYS, F, SA_CCR(:,:,ITAG), DZH, 
     &                    SA_CBELOW(:,ITAG), CLBASE, CLTOP,
     &                    FRAC, TCLIFE, DTCLD )
          END DO
#endif

          DO SPC = 1, MXSPCS
            CBASEF( SPC ) =  CBELOW( SPC ) 
#ifdef sens
            DO NP = 1, NPMAX
              S_CBASEF( NP,SPC ) =  S_CBELOW( NP,SPC )
            END DO
#endif
          END DO

#ifdef isam
          DO ITAG = 1, NTAG_SA
            DO SPC = 1, NSPC_SA
              SA_BASEF( SPC,ITAG ) =  SA_CBELOW( SPC,ITAG )
            END DO
          END DO
#endif

          DO LAY = CLTOP, CLBASE, -1
            DO SPC = 1, NSPCSD
              CONDIS = CONC( SPC,LAY )
              PCLD( SPC,LAY ) = F( LAY ) * ( FSIDE( LAY ) * CONDIS )
     &                        + ( 1.0 - F( LAY ) ) * CBASE0( SPC )
              PCLD( SPC,LAY ) = MIN( PCLD( SPC,LAY ), CCR( SPC,LAY ) / FRAC )

#ifdef sens
              DO NP = 1, NPMAX
                S_CONDIS = S_CONC( NP, SPC, LAY )
                S_PCLD( NP, SPC, LAY ) = F( LAY ) * ( FSIDE( LAY ) * S_CONDIS )
     &                         + ( 1.0 - F( LAY ) ) * S_CBASE0( NP,SPC )
                IF ( PCLD( SPC, LAY ) .EQ. ( CCR( SPC, LAY ) / FRAC ) ) THEN
                   S_PCLD( NP, SPC, LAY ) = S_CCR( NP, SPC, LAY ) / FRAC
                END IF

              END DO
#endif

C...POLC in mol sp/m2

              POLC( SPC ) = POLC( SPC )
     &                    + PCLD( SPC,LAY ) * RHOM2( LAY )
#ifdef isam
              CSPC = 0
              CSPC = INDEXINT1(SPC,NSPC_SA,MAP_SAtoCGR(:))

              IF ( CSPC .NE. 0) THEN
                DO ITAG = 1, NTAG_SA
                  CONDIS = SA_CONC( CSPC,LAY,ITAG )

                  SA_PCLD( CSPC,LAY,ITAG ) = F( LAY ) * ( FSIDE( LAY ) * CONDIS )
     &                                     + ( 1.0 - F( LAY ) ) * SA_BASE0( CSPC,ITAG )
c                 IF ( ( CCR( SPC,LAY ) / FRAC) .EQ. PCLD( SPC,LAY ) ) THEN
c                   SA_PCLD( CSPC,LAY,ITAG ) = SA_CCR( CSPC,LAY,ITAG ) / FRAC 
c                 END IF

                  SA_PCLD( CSPC,LAY,ITAG ) = MIN( SA_PCLD( CSPC,LAY,ITAG ), SA_CCR( CSPC,LAY,ITAG ) / FRAC )

                  SA_POLC( CSPC,ITAG ) = SA_POLC( CSPC,ITAG )
     &                              + SA_PCLD( CSPC,LAY,ITAG ) * RHOM2( LAY )
                END DO
              END IF
#endif

#ifdef sens
              DO NP = 1, NPMAX
                S_POLC( NP, SPC ) = S_POLC( NP, SPC )
     &                            + S_PCLD( NP, SPC, LAY ) * REAL ( RHOM2( LAY ), 8 )
              END DO
#endif

            END DO
          END DO

C...Now compute for raining region below cloud which is also considered
C...  to be part of the aqueous reaction chamber

            DO LAY = 1, CLBASE - 1
              AIRM = AIRM + RHOM2( LAY )

              DO SPC = 1, NSPCSD
                IF ( CBASE0( SPC ) .GT. 0.0 ) THEN
                  BCLDWT = CONC( SPC,LAY ) / MAX( CBASE0( SPC ), 1.0E-30 )
#ifdef sens
                  DO NP = 1, NPMAX
                     IF ( CBASE0( SPC ) .GT. 1.0E-30 ) THEN

                        IF ( S_CBASE0( NP, SPC ) .LT. DDM3D_CONCMINL ) THEN
c                         S_BCLDWT( NP, SPC, LAY ) = 1.0D0
                          S_BCLDWT( NP, SPC, LAY ) =  0.0D0
                        ELSE
                          S_BCLDWT( NP, SPC, LAY ) = S_CONC( NP, SPC,LAY ) 
     &                                            / S_CBASE0( NP, SPC )
                        END IF
                     ELSE
c                       S_BCLDWT( NP, SPC, LAY ) = S_CONC( NP, SPC, LAY)
c    &                                           / DDM3D_CONCMINL
c                       S_BCLDWT( NP, SPC, LAY ) = 1.0D0
                        S_BCLDWT( NP, SPC, LAY ) =  0.0D0
                     END IF
                  END DO
#endif          
                ELSE
                  BCLDWT = 1.0
#ifdef sens
                  DO NP = 1, NPMAX
                     S_BCLDWT( NP, SPC, LAY ) =  1.0
                  END DO
#endif
                END IF

                IF ( STM ) THEN
                  IF ( SPC .NE. SURCLDMX( SPC ) ) THEN      ! use its mixing surrogate
                    IF ( CBASE0( SURCLDMX( SPC ) ) .GT. 0.0 ) THEN
                      BCLDWT = CONC( SURCLDMX( SPC ),LAY ) / MAX( CBASE0( SURCLDMX( SPC ) ), 1.0E-30 )
                    ELSE
                      BCLDWT = 1.0
                    END IF
                  END IF
                END IF ! stm
  
                PCLD( SPC,LAY ) = BCLDWT * CBASEF( SPC )

#ifdef sens
                DO NP = 1, NPMAX
                   S_PCLD( NP, SPC, LAY ) = S_CBASEF( NP, SPC ) * S_BCLDWT( NP, SPC, LAY )
                END DO
#endif

C...Necessary because CBASEF and CBASE0 are the ending vertical averages
C...  below cloud concentrations in mol sp/mol air

                IF ( ICLDTYPE .EQ. 1 ) THEN
                  POLC( SPC ) = POLC( SPC ) + PCLD( SPC, LAY ) * RHOM2( LAY )
#ifdef sens
                  DO NP = 1, NPMAX
                    S_POLC( NP, SPC ) = S_POLC( NP, SPC )
     &                        + S_PCLD( NP, SPC, LAY ) * REAL ( RHOM2( LAY ), 8 )
                  END DO
#endif
                END IF

#ifdef isam
                CSPC = 0
                CSPC = INDEXINT1(SPC,NSPC_SA,MAP_SAtoCGR(:))
                IF ( CSPC .NE. 0) THEN
                  DO ITAG = 1, NTAG_SA

c                    BCLDWT = SA_CONC( CSPC,LAY,ITAG ) / MAX( SA_BASE0( CSPC, ITAG ),1.0E-30 )
                     SA_PCLD( CSPC,LAY, ITAG ) = BCLDWT * SA_BASEF( CSPC,ITAG )

                     IF ( ICLDTYPE .EQ. 1 ) THEN
                       SA_POLC( CSPC, ITAG ) = SA_POLC( CSPC, ITAG )
     &                                       + SA_PCLD( CSPC, LAY, ITAG)
     &                                       * RHOM2( LAY )
                     END IF
                  END DO
                END IF
#endif

              END DO

            END DO

C...Compute cloud mean quantities

          AIRM  = MAX( AIRM, 1.0E-30 )   ! tot. air mass in cloudy layers in mol/m2

          WCBAR = MAX( WCBAR, 1.0E-30 )  ! liq.wat. content in kg/m3 * CTHK

          WTBAR = MAX( WTBAR, 1.0E-30 )  ! condensed wat cnt: kg/m3 * CTHK

          CTHK  = MAX( CTHK, 1.0E-30 )   ! cloud thickness, meters

          TBARC = TBARC / WCBAR          ! deg K (note WCBAR has hidden factor CTHK in it)
          PBARC = PBARC / WCBAR          ! avg cloud pres, Pa

          JH2O2_BAR = JH2O2_BAR / WCBAR  ! avg H2O2 photolysis, 1/min
          JHNO3_BAR = JHNO3_BAR / WCBAR  ! avg HNO3 photolysis, 1/min

          WCBAR = WCBAR / CTHK           ! avg liq wat content in kg/m3

          WTBAR = WTBAR / CTHK           ! avg con wat content in kg/m3

C...Calculate the cloud optical depth using a formula derived from
C...  Stephens (1978), JAS(35), pp2111-2132.
C...  only calculate the cloud optical depth when the liquid water
C...  path is >= 10 g/m2

          LWP = WTBAR * CTHK * 1000.0    ! converts to g/m2
          IF ( LWP .GE. 10.0 ) THEN
             CLOD = 10.0 ** ( 0.2633 + 1.7095 * LOG( LOG10( LWP ) ) )
          ELSE
             CLOD = 0.0
          END IF

C...If no cloud or optical depth < 5, set clear sky values.
C...  (i.e. don`t do anything)

          IF ( CLOD .GE. 5.0 ) THEN

                STRNS = 1.0 + FRAC * ( ( 5.0 - EXP ( -CLOD ) )
     &                                 / ( 4.0 + 0.42 * CLOD ) - 1.0 )

             IF ( ICLDTYPE .EQ. 1 ) THEN  ! precipitating cloud
                SUBTRANS( COL,ROW,1 ) = STRNS
             ELSE
                SUBTRANS( COL,ROW,2 ) = STRNS
             END IF
              
          END IF

C...Finally, get in cloud pollutant concentrations in mol sp per mol air

          DO SPC = 1, NSPCSD
            POLC ( SPC ) = POLC( SPC ) / AIRM
            CEND ( SPC ) = POLC( SPC )
            REMOV( SPC ) = 0.0
#ifdef sens
            DO NP = 1, NPMAX
              S_POLC ( NP, SPC ) = S_POLC( NP, SPC ) / REAL ( AIRM, 8 )
              S_CEND ( NP, SPC ) = S_POLC( NP, SPC )
              S_REMOV( NP, SPC ) = 0.0D0
            END DO
#endif
          END DO

          REMOVAC = 0.0
#ifdef sens
          S_REMOVAC = 0.0D0
#endif

          ARPRES = PBARC / STDATMPA
          RTCH = ( MOLVOL / STDTEMP ) * TBARC
          CTHK1 = AIRM * RTCH / ( ARPRES * 1000.0 )

#ifdef isam
          DO SPC = 1, NSPC_SA
            DO ITAG = 1, NTAG_SA
              SA_POLC( SPC, ITAG ) = SA_POLC( SPC, ITAG ) / AIRM
              SA_CEND( SPC, ITAG ) = SA_POLC( SPC, ITAG )
            END DO
          END DO
          SA_REMOV = 0.0
#endif 

          CALL SCAVWDEP ( JDATE, JTIME, WTBAR,
     &                    WCBAR, TBARC, PBARC,
     &                    CTHK1, AIRM, PRATE1, DTCLD, POLC, CEND,
     &                    REMOV, REMOVAC, ALFA0, ALFA2, ALFA3 )

#ifdef isam
          DO SPC = 1, NSPC_SA
             CSPC = MAP_SAtoCGR(SPC)
             SA_SUM = SUM ( SA_POLC( SPC,: ) )
c            IF ( POLC( CSPC ) .GT. 1.0E-30 .AND. CEND( CSPC ) .GT. 1.0E-09 ) THEN
c            IF ( POLC( CSPC ) .GT. 1.0E-30 ) THEN 
             IF ( SA_SUM .GT. 1.0E-25 ) THEN
                DO ITAG = 1, NTAG_SA
                   SA_CEND( SPC, ITAG ) = SA_POLC( SPC, ITAG )
     &                                  * ( CEND( CSPC )
c    &                                    / POLC( CSPC ) )
     &                                    / SA_SUM )
                   SA_REMOV( SPC, ITAG ) = SA_POLC( SPC, ITAG )
     &                                   * ( REMOV( CSPC )
c    &                                     / POLC( CSPC ) )
     &                                     / SA_SUM )
                END DO
             ELSE 
                DO ITAG = 1, NTAG_SA
                   SA_CEND( SPC, ITAG )  = 0.0
                   SA_REMOV( SPC, ITAG ) = 0.0
                END DO
             END IF
          END DO
#endif

C...if the liquid water content is above the specified threshold
C...  then perform the aqueous chemistry within the cloud and
C...  re-adjust the ending and removed amounts for those species
C...  that participated in cloud chemistry

          IF ( WCBAR .GT. 1.0E-05 ) THEN
 
C...  determine day or night from cosine of zenith angle for the cell; Golam Sarwar

            COSZ = MET_DATA%COSZEN( COL,ROW )

            IF ( COSZ .LE. 0.0 ) THEN
C...set mean photolysis rates to zero
              JH2O2_HYDROMETEORS = 0.0D0 
              JHNO3_HYDROMETEORS = 0.0D0
            ELSE
C...convert mean photolysis rates to 1/sec
              JH2O2_HYDROMETEORS = REAL( JH2O2_BAR * MINPERSEC, 8 ) 
              JHNO3_HYDROMETEORS = REAL( JHNO3_BAR * MINPERSEC, 8 )
            END IF

C...in aqchem, H2SO4 gas is added to ASO4J
C...  mimic this for the ASO4GASJ tracking species
            IF ( STM ) THEN
              POLC( AEROSPC_MAP( ASO4GAS_IDX, 2 ) ) = POLC( AEROSPC_MAP( ASO4GAS_IDX, 2 ) )
     &                                              + POLC( PRECURSOR_MAP( SULF_IDX ) )
              CEND( AEROSPC_MAP( ASO4GAS_IDX, 2 ) ) = POLC( AEROSPC_MAP( ASO4GAS_IDX, 2 ) )
            END IF

            CALL AQ_MAP ( JDATE, JTIME, WTBAR, WCBAR, TBARC, PBARC,
     &                    CTHK1, AIRM, PRATE1, DTCLD, POLC, CEND,
     &                    REMOV, REMOVAC, ALFA0, ALFA2, ALFA3, COSZ )


#ifdef isam
            DO SPC = 1, NSPC_SA ! general case
              CSPC = MAP_SAtoCGR(SPC)
              SA_SUM = SUM ( SA_POLC( SPC,: ) )
              DO ITAG = 1, NTAG_SA
c               IF ( POLC( CSPC ) .GT. 1.0E-30 ) THEN
                IF ( SA_SUM .GT. 1.0E-25 ) THEN
                  SA_CEND( SPC,ITAG ) = SA_POLC( SPC,ITAG )
     &                                * ( CEND( CSPC )
c    &                                  / MAX( 1.0E-26,POLC( CSPC ) ) )
     &                                  / SA_SUM )
                  SA_REMOV( SPC,ITAG ) = SA_POLC( SPC, ITAG )
     &                                 * ( REMOV( CSPC )
c    &                                   / MAX( 1.0E-25, POLC( CSPC ) ))
     &                                   / SA_SUM )
                ELSE
                  SA_CEND( SPC,ITAG )  = 0.0
                  SA_REMOV( SPC,ITAG ) = 0.0
                END IF
              END DO
            END DO
            IF ( S_N2O5 .NE. 0 ) THEN
              DO ITAG = 1, NTAG_SA
                SA_CEND( S_N2O5,ITAG )  = 0.0
              END DO
            END IF
            IF (S_SO4J .NE. 0 ) THEN ! sulfate case

              DO ITAG = 1, NTAG_SA  ! sulfate from H2SO4
                SA_DS4( ITAG ) = SA_POLC( S_SULF,ITAG )
                SA_CEND( S_SULF,ITAG )  = 1.0E-30
              END DO

              DO ITAG = 1, NTAG_SA
                SA_DS4( ITAG ) = SA_DS4( ITAG ) + DS4_SAVE ! total sulfate produced
     &                         * ( SA_POLC( S_SO2,ITAG )
     &                           / SUM ( SA_POLC( S_SO2,: ) ) )
                SA_CEND( S_SO4J,ITAG ) = SA_POLC( S_SO4J,ITAG )  ! sulfate before removal
     &                                       + SA_DS4( ITAG )
              END DO

              SA_SUM = SUM ( SA_CEND( S_SO4J,: ) ) ! total apportioned sulfate before removal
 
              DO ITAG = 1, NTAG_SA ! final sulfate removal and concentration
                SA_REMOV( S_SO4J,ITAG ) = REMOV_SAVE
     &                                  * ( SA_CEND( S_SO4J,ITAG )
     &                                    / SA_SUM )
                SA_CEND( S_SO4J,ITAG ) = SA_CEND( S_SO4J,ITAG )
     &                                 - DEPSUM_SAVE
     &                                 * ( SA_CEND( S_SO4J,ITAG )
     &                                   / SA_SUM )
                SA_CEND( S_SO4J,ITAG ) = MAX ( SA_CEND( S_SO4J,ITAG ), 1.0E-30 )
              END DO

            END IF

            IF (S_AORGCJ .NE. 0 ) THEN ! AORGCJ case
               SA_DCSOA_GLY = 0.0
               SA_DCSOA_MGLY = 0.0
               DO ITAG = 1, NTAG_SA
                  IF ( S_GLY .NE. 0 ) THEN
                    SA_DCSOA_GLY( ITAG ) = DGLY1_SAVE
     &                               * SA_POLC( S_GLY,ITAG )
     &                               / MAX( 1.0E-25, SUM ( SA_POLC( S_GLY,: ) ) )
                  END IF
                  IF ( S_MGLY .NE. 0 ) THEN
                     SA_DCSOA_MGLY( ITAG ) = DMGLY1_SAVE
     &                               * SA_POLC( S_MGLY,ITAG )
     &                               / MAX( 1.0E-25, SUM ( SA_POLC( S_MGLY,: ) ) )
                  END IF
                  SA_CEND( S_AORGCJ,ITAG ) = SA_POLC( S_AORGCJ,ITAG ) ! AORGCJ before removal
     &                                         + SA_DCSOA_GLY( ITAG )
     &                                         + SA_DCSOA_MGLY( ITAG )
               END DO
               SA_SUM = MAX( 1.0E-25, SUM ( SA_CEND( S_AORGCJ,: ) ) ) ! total apportioned AORGCJ before removal
               DO ITAG = 1, NTAG_SA ! final AORGCJ removal and concentration
                  SA_REMOV( S_AORGCJ,ITAG ) = REMOV_AORGC_SAVE
     &                                        * SA_CEND( S_AORGCJ,ITAG )
     &                                        / SA_SUM
                  SA_CEND( S_AORGCJ,ITAG ) = SA_CEND( S_AORGCJ,ITAG )
     &                                       - DEPSUM_AORGC_SAVE
     &                                       * SA_CEND( S_AORGCJ,ITAG )
     &                                       / SA_SUM
                  SA_CEND( S_AORGCJ,ITAG ) = MAX ( SA_CEND( S_AORGCJ,ITAG ), 1.0E-30 )
               END DO
            END IF
#endif
          END IF

          DO SPC = 1, N_CGRID_SPC
            IF ( CEND( SPC ) .LT. 0.0 ) 
     &                               WRITE( LOGDEV,* ) ' CEND,R,C,SP=',
     &                               CEND( SPC ), ROW, COL, SPC
          END DO

C...weight the removed amount by the cloud fraction and convert
C...  from mol/m2 to kg/m2 and kg/m2 to kg/hectare

C...convert removal change from moles/m**2 to kg/m**2 and kg/m**2 to kg/hectare

                DO I = 1,N_CGRID_SPC
                  IF ( CGRID_MASK_NUM( I ) .OR.
     &                 CGRID_MASK_SRF( I ) ) THEN
                     ! Aerosol Number (N m-2 -> N ha-1)
                     ! Aerosol Surface Area (m2 m-2 -> m2 ha-1)
                     REMOV( I ) = REMOV( I ) * M2PHA * FRAC
#ifdef sens
                     DO NP = 1, NPMAX
                       S_REMOV( NP,I ) = S_REMOV( NP,I ) * REAL( M2PHA * FRAC, 8 )
                     END DO
#endif
                     ! ISAM does not track aerosol number or surface
                     ! area

                  ELSE
                      
                     ! Gas and Aerosol Mass (moles m-2 -> kg ha-1)
                     REMOV( I ) = REMOV( I ) * CGRID_MW( I ) * M2PHA_OVER_GPKG * FRAC
#ifdef sens
                     DO NP = 1, NPMAX
                       S_REMOV( NP,I ) = S_REMOV( NP,I ) * CGRID_MW( I ) * M2PHA_OVER_GPKG * FRAC
                     END DO
#endif
#ifdef isam
                     CSPC = 0
                     CSPC = INDEXINT1(I,NSPC_SA,MAP_SAtoCGR(:))
                     IF ( CSPC .GT. 0 ) THEN
                       DO ITAG = 1, NTAG_SA
                         SA_REMOV( CSPC,ITAG ) = SA_REMOV( CSPC,ITAG ) * CGRID_MW( I )
     &                                          * M2PHA_OVER_GPKG * FRAC
                       END DO
                     END IF
#endif
                  END IF


                END DO

C...add deposition amounts into the DEP array

          DO VAR = 1, N_SPC_WDEP
            CONV_DEP( COL,ROW,VAR ) = CONV_DEP( COL,ROW,VAR )
     &                              + REMOV( MAP_WDEPtoCGRID( VAR ) )
#ifdef sens
            DO NP = 1, NPMAX
              S_CONDEP( COL, ROW, NP, VAR ) = S_CONDEP( COL, ROW, NP,VAR )
     &                                      + S_REMOV( NP, MAP_WDEPtoCGRID( VAR ) )
            END DO
#endif
          END DO

C...  and load H+ concentration into the deposition array as well

          CONV_DEP( COL,ROW,N_SPC_WDEP+1 ) = CONV_DEP( COL,ROW,N_SPC_WDEP+1 )
     &                                     + REMOVAC

#ifdef sens
          DO NP = 1, NPMAX
            S_CONDEP( COL,ROW,NP,N_SPC_WDEP+1 ) = S_CONDEP( COL,ROW,NP,N_SPC_WDEP+1 )
     &                                          + S_REMOVAC( NP )
          END DO
#endif


#ifdef isam
          DO ITAG = 1, NTAG_SA
            DO SPC = 1, NSPC_SA
              CONV_SADEP( COL,ROW,SPC,ITAG ) = CONV_SADEP( COL,ROW,SPC,ITAG )
     &                                       + SA_REMOV( SPC,ITAG )
            END DO
          END DO
#endif

C...Compute concentration changes in the grid column resulting
C...  from subgrid scale vertical mixing:

C...first, below cloud base,
C...   include raining region below cld base

          IF ( ICLDTYPE .EQ. 1 ) THEN   ! raining cloud
            DO LAY = 1, CLBASE - 1
              DO SPC = 1, NSPCSD
                IF ( SPC .NE. N_GC_SPCD ) THEN
                  INCLOUD = PCLD( SPC,LAY ) * CEND( SPC )
     &                    / MAX( POLC( SPC ), CONCMINL( SPC,LAY ) )

                  IF ( STM ) THEN
                    IF ( SPC .NE. SURCLDMX( SPC ) ) THEN       ! use its surrogate
                      IF ( POLC( SURCLDMX( SPC ) ) .GT. 0.0 ) THEN
                        INCLOUD = PCLD( SURCLDMX( SPC ),LAY ) * CEND( SPC )
     &                          / MAX( POLC( SURCLDMX( SPC ) ), CONCMINL( SURCLDMX( SPC ),LAY ) )
                      ELSE
                        INCLOUD = CEND( SPC )
                      END IF
                    END IF
                  END IF ! stm

                  OUTCLOUD = PCLD( SPC,LAY )
                  CGRD( COL,ROW,LAY,SPC ) = FRAC * INCLOUD
     &                                     + ( 1.0 - FRAC ) * OUTCLOUD

#ifdef sens
                  DO NP = 1, NPMAX
                    IF( ABS( S_POLC( NP, SPC ) ) .LT. DDM3D_CONCMINL ) THEN
                      S_INCLOUD( NP, SPC, LAY ) = 0.0D0
                    ELSE
                      S_INCLOUD( NP, SPC, LAY ) = S_PCLD( NP,SPC,LAY ) * S_CEND( NP,SPC )
     &                                        / S_POLC( NP, SPC )
                    ENDIF
                    S_OUTCLOUD( NP, SPC, LAY ) = S_PCLD( NP,SPC, LAY )
                    SENGRID( COL, ROW, LAY, NP, SPC ) =
     &                        S_INCLOUD ( NP, SPC, LAY ) * FRAC
     &                      + S_OUTCLOUD( NP, SPC, LAY ) * ( 1.0 - FRAC )
                  END DO
#endif
                END IF
              END DO
#ifdef isam
              DO SPC = 1, NSPC_SA ! general case
                CSPC = MAP_SAtoCGR(SPC)
 
                IF ( CEND( CSPC ) .GT. 1.0E-09) THEN
                DO ITAG = 1, NTAG_SA
                  INCLOUD = SA_PCLD( SPC, LAY, ITAG )
     &                    * ( SA_CEND( SPC, ITAG )
     &                      / MAX( SA_POLC( SPC, ITAG ), CONCMINL( CSPC,LAY ) ) )
                  OUTCLOUD = SA_PCLD( SPC, LAY, ITAG )
                  ISAM( COL,ROW,LAY,SPC,ITAG ) = FRAC * INCLOUD
     &                                         + ( 1.0 - FRAC ) * OUTCLOUD
                END DO
                END IF
              END DO

              IF (S_SO4J .NE. 0 ) THEN ! sulfate case

                CONDIS = 0.0
                DO ITAG = 1, NTAG_SA
                  SA_INCLOUD( ITAG ) = PCLD( C_SO4J,LAY ) * ( SA_CEND( S_SO4J,ITAG ) 
     &                    / MAX( POLC( C_SO4J ), CONCMINL( C_SO4J,LAY ) ) )
     &                    - ( PCLD( C_SO4J,LAY ) * CEND( C_SO4J ) / MAX( POLC( C_SO4J ), CONCMINL( C_SO4J,LAY ) ) )
     &                    * SA_POLC( S_SO4J,ITAG ) / MAX( POLC( C_SO4J ), CONCMINL( C_SO4J,LAY ) )
     &                    + SA_PCLD( S_SO4J,LAY,ITAG ) * CEND( C_SO4J ) / MAX( POLC( C_SO4J ), CONCMINL( C_SO4J,LAY ) )
                  IF ( SA_INCLOUD( ITAG ) .LT. 0.0 ) THEN
                    CONDIS = CONDIS + SA_INCLOUD( ITAG )
                    SA_INCLOUD( ITAG )  = 0.0
                  END IF
                END DO

                IF ( CONDIS .NE. 0.0 ) THEN
                 BLNC = ( CONDIS + SUM( SA_INCLOUD( : ) ) ) / MAX( SUM( SA_INCLOUD( : ) ), CONCMINL( C_SO4J,LAY ) )
                  DO ITAG = 1, NTAG_SA
                    IF ( SA_INCLOUD( ITAG ) .GT. 0.0 ) THEN
                      SA_INCLOUD( ITAG ) = BLNC * SA_INCLOUD( ITAG )
                    END IF
                  END DO
                END IF

                DO ITAG = 1, NTAG_SA
                  OUTCLOUD = SA_PCLD( S_SO4J,LAY,ITAG )
                  ISAM( COL,ROW,LAY,S_SO4J,ITAG ) = FRAC * SA_INCLOUD( ITAG )
     &                                       + ( 1.0 - FRAC ) * OUTCLOUD
                  ISAM( COL,ROW,LAY,S_SO4J,ITAG ) = MAX( ISAM( COL,ROW,LAY,S_SO4J,ITAG ), 1.0E-30 )
                END DO

              END IF

              IF (S_AORGCJ .NE. 0 ) THEN ! AORGCJ case

                CONDIS = 0.0
                DO ITAG = 1, NTAG_SA
                  SA_INCLOUD( ITAG ) = PCLD( C_AORGCJ,LAY ) * ( SA_CEND( S_AORGCJ,ITAG ) 
     &                    / MAX( POLC( C_AORGCJ ), CONCMINL( C_AORGCJ,LAY ) ) )
     &                    - ( PCLD( C_AORGCJ,LAY ) * CEND( C_AORGCJ ) / MAX( POLC( C_AORGCJ ), CONCMINL( C_AORGCJ,LAY ) ) )
     &                    * SA_POLC( S_AORGCJ,ITAG ) / MAX( POLC( C_AORGCJ ), CONCMINL( C_AORGCJ,LAY ) )
     &                    + SA_PCLD( S_AORGCJ,LAY,ITAG ) * CEND( C_AORGCJ ) / MAX( POLC( C_AORGCJ ), CONCMINL( C_AORGCJ,LAY ) )
                  IF ( SA_INCLOUD( ITAG ) .LT. 0.0 ) THEN
                    CONDIS = CONDIS + SA_INCLOUD( ITAG )
                    SA_INCLOUD( ITAG )  = 0.0
                  END IF
                END DO

                IF ( CONDIS .NE. 0.0 ) THEN
                 BLNC = ( CONDIS + SUM( SA_INCLOUD( : ) ) ) / MAX( SUM( SA_INCLOUD( : ) ), CONCMINL( C_AORGCJ,LAY ) )
                  DO ITAG = 1, NTAG_SA
                    IF ( SA_INCLOUD( ITAG ) .GT. 0.0 ) THEN
                      SA_INCLOUD( ITAG ) = BLNC * SA_INCLOUD( ITAG )
                    END IF
                  END DO
                END IF

                DO ITAG = 1, NTAG_SA
                  INCLOUD = SA_PCLD( S_AORGCJ,LAY,ITAG ) * SA_CEND( S_AORGCJ,ITAG )
     &                  / MAX( SA_POLC( S_AORGCJ,ITAG ), CONCMINL( SPC,LAY ) )
c                 INCLOUD = MAX( INCLOUD, CONCMINL( C_AORGCJ,LAY ) )
                  OUTCLOUD = SA_PCLD( S_AORGCJ,LAY,ITAG )
                  ISAM( COL,ROW,LAY,S_AORGCJ,ITAG ) = FRAC * SA_INCLOUD( ITAG )
     &                                       + ( 1.0 - FRAC ) * OUTCLOUD
                  ISAM( COL,ROW,LAY,S_AORGCJ,ITAG ) = MAX( ISAM( COL,ROW,LAY,S_AORGCJ,ITAG ), 1.0E-30 )
                END DO

              END IF

#endif
            END DO
          ELSE
            DO LAY = 1, CLBASE - 1
              DO SPC = 1, NSPCSD
                CGRD( COL,ROW,LAY,SPC ) = PCLD( SPC,LAY )
#ifdef sens
                DO NP = 1, NPMAX
                  SENGRID( COL, ROW, LAY, NP, SPC ) = REAL ( S_PCLD( NP, SPC, LAY ), 4 )
                END DO
#endif 
              END DO
#ifdef isam
              DO SPC = 1, NSPC_SA
c               IF (PCLD( MAP_SAtoCGR(SPC),LAY ) .GT. 1.0E-09) THEN
                DO ITAG = 1, NTAG_SA
                  ISAM( COL,ROW,LAY,SPC,ITAG ) = SA_PCLD( SPC, LAY, ITAG )
                END DO
c               END IF
              END DO
#endif
            END DO
          END IF

C...Now do changes in cloudy layers:

          DO LAY = CLBASE, CLTOP
            DO SPC = 1, NSPCSD
              IF ( SPC .NE. N_GC_SPCD ) THEN
                INCLOUD = PCLD( SPC,LAY ) * CEND( SPC )
     &                  / MAX( POLC( SPC ), CONCMINL( SPC,LAY ) )

                IF ( STM ) THEN
                  IF ( SPC .NE. SURCLDMX( SPC ) ) THEN       ! use its surrogate
                    IF ( POLC( SURCLDMX( SPC ) ) .GT. 0.0 ) THEN
                      INCLOUD = PCLD( SURCLDMX( SPC ),LAY ) * CEND( SPC )
     &                        / MAX( POLC( SURCLDMX( SPC ) ), CONCMINL( SURCLDMX( SPC ),LAY ) )
                    ELSE
                      INCLOUD = CEND( SPC )
                    END IF
                  END IF
                END IF ! stm

                OUTCLOUD = ( CCR( SPC,LAY ) - FRAC * PCLD( SPC,LAY ) )
     &                   / ( 1.0 - FRAC )
                OUTCLOUD = MAX( OUTCLOUD, CONCMINL( SPC,LAY ) )
                CGRD( COL,ROW,LAY,SPC ) = FRAC * INCLOUD 
     &                                   + ( 1.0 - FRAC ) * OUTCLOUD
#ifdef sens
                DO NP = 1, NPMAX
                  IF( ABS( S_POLC( NP, SPC ) ) .LT. DDM3D_CONCMINL ) THEN
                    S_INCLOUD( NP, SPC, LAY ) = 0.0D0
                  ELSE
                    S_INCLOUD( NP, SPC, LAY ) = S_PCLD( NP,SPC,LAY ) * S_CEND( NP,SPC )
     &                                      / S_POLC( NP, SPC )
                  END IF
                  S_OUTCLOUD( NP, SPC, LAY ) = ( S_CCR( NP, SPC, LAY ) - S_PCLD( NP, SPC,LAY ) * REAL( FRAC, 8 ) )
     &                                       / ( 1.0D0 - REAL ( FRAC, 8 ) )
                  IF ( OUTCLOUD .EQ. CONCMINL( SPC, LAY ) ) THEN
                    S_OUTCLOUD( NP, SPC, LAY ) = 0.0D0
                  END IF
                  SENGRID( COL, ROW, LAY, NP, SPC ) =
     &                        S_INCLOUD ( NP, SPC, LAY ) * FRAC
     &                      + S_OUTCLOUD( NP, SPC, LAY ) * ( 1.0 - FRAC )
                END DO
#endif
              END IF
            END DO
#ifdef isam
            DO SPC = 1, NSPC_SA ! general case
              CSPC = MAP_SAtoCGR(SPC)
c             IF (CEND( MAP_SAtoCGR(SPC) ) .GT. 1.0E-09) THEN
              DO ITAG = 1, NTAG_SA
                INCLOUD = SA_PCLD( SPC,LAY,ITAG ) * ( SA_CEND( SPC,ITAG )
     &                  / MAX( SA_POLC( SPC,ITAG ), CONCMINL( CSPC,LAY ) ) )

                OUTCLOUD = ( SA_CCR( SPC,LAY,ITAG ) - FRAC * SA_PCLD( SPC,LAY,ITAG ) )
     &                   / ( 1.0 - FRAC )

                OUTCLOUD = MAX( OUTCLOUD, CONCMINL( CSPC,LAY ) )
                ISAM( COL,ROW,LAY,SPC,ITAG ) = FRAC * INCLOUD
     &                                       + ( 1.0 - FRAC ) * OUTCLOUD
              END DO
c             END IF
            END DO

            IF (S_SO4J .NE. 0 ) THEN ! sulfate case
              DO ITAG = 1, NTAG_SA

                INCLOUD = PCLD( C_SO4J,LAY ) * SA_CEND( S_SO4J,ITAG ) / MAX( POLC( C_SO4J ), CONCMINL( C_SO4J,LAY ) )
     &                  - ( PCLD( C_SO4J,LAY ) * CEND( C_SO4J ) / MAX( POLC( C_SO4J ), CONCMINL( C_SO4J,LAY ) ) )
     &                  * SA_POLC( S_SO4J,ITAG ) / MAX( POLC( C_SO4J ), CONCMINL( C_SO4J,LAY ) ) 
     &                  + SA_PCLD( S_SO4J,LAY,ITAG ) * CEND( C_SO4J ) / MAX( POLC( C_SO4J ), CONCMINL( C_SO4J,LAY ) )
                INCLOUD = MAX( INCLOUD, CONCMINL( C_SO4J,LAY ) )

                OUTCLOUD = ( SA_CCR( S_SO4J,LAY,ITAG ) - FRAC * SA_PCLD( S_SO4J,LAY,ITAG ) )
     &                   / ( 1.0 - FRAC )
                OUTCLOUD = MAX( OUTCLOUD, CONCMINL( CSPC,LAY ) )

                ISAM( COL,ROW,LAY,S_SO4J,ITAG ) = FRAC * INCLOUD
     &                                       + ( 1.0 - FRAC ) * OUTCLOUD
              END DO
            END IF


            IF (S_AORGCJ .NE. 0 ) THEN ! AORGCJ case
              DO ITAG = 1, NTAG_SA
                INCLOUD = PCLD( C_AORGCJ,LAY ) * SA_CEND( S_AORGCJ,ITAG ) / MAX( POLC( C_AORGCJ ), CONCMINL( C_AORGCJ,LAY ) )
     &                  - ( PCLD( C_AORGCJ,LAY ) * CEND( C_AORGCJ ) / MAX( POLC( C_AORGCJ ), CONCMINL( C_AORGCJ,LAY ) ) )
     &                  * SA_POLC( S_AORGCJ,ITAG ) / MAX( POLC( C_AORGCJ ), CONCMINL( C_AORGCJ,LAY ) ) 
     &                  + SA_PCLD( S_AORGCJ,LAY,ITAG ) * CEND( C_AORGCJ ) / MAX( POLC( C_AORGCJ ), CONCMINL( C_AORGCJ,LAY ) )

                INCLOUD = SA_PCLD( S_AORGCJ,LAY,ITAG ) * SA_CEND( S_AORGCJ,ITAG )
     &                  / MAX( SA_POLC( S_AORGCJ,ITAG ), CONCMINL( SPC,LAY ) )
c               INCLOUD = MAX( INCLOUD, CONCMINL( C_AORGCJ,LAY ) )

                OUTCLOUD = ( SA_CCR( S_AORGCJ,LAY,ITAG ) - FRAC * SA_PCLD( S_AORGCJ,LAY,ITAG ) )
     &                   / ( 1.0 - FRAC )
                OUTCLOUD = MAX( OUTCLOUD, CONCMINL( CSPC,LAY ) )

                ISAM( COL,ROW,LAY,S_AORGCJ,ITAG ) = FRAC * INCLOUD
     &                                       + ( 1.0 - FRAC ) * OUTCLOUD

              END DO
            END IF
#endif
          END DO

        END DO        !  end loop on columns COL
      END DO        !  end loop on rows    ROW

      RETURN          !  from main routine CLDPROC

91010 FORMAT( 3( A, :, I3, : ) )

      END
