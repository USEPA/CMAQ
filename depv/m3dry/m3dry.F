
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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/depv/m3dry/m3dry.F,v 1.10 2011/10/21 16:10:32 yoj Exp $

C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE m3dry ( jdate, jtime, dt, abflux, sfc_hono, mosaic, fst, cgridl1, depvel_gas, 
     &                   pvd, cmp )

C-------------------------------------------------------------------------------
C Name:     Models-3 Dry Deposition
C Purpose:  Computes dry deposition velocities using Rst and Ra, and
C           elements of ADOM DD model.
C Revised:  21 Jan 1998  Original version.  (J. Pleim and A. Bourgeois)
C           18 Sep 2001  Made general for USGS 24-category system.
C                        (T. Otte, J. Pleim, and W. Hutzell)
C           14 Jan 2002  Added temperature dependence to Henry's Law
C                        constants.  Added temperature and pressure
C                        dependence to diffusivity.  Added new dry
C                        deposition species, methanol.  (Y. Wu and T. Otte)
C           18 Jan 2002  Changed the reference wet cuticle resistance.
C                        (J. Pleim)
C           09 Jun 2003  Added logic for modeling snow covered surfaces.
C                        Changed the reactivities for SO2, HNO3 and NH3.
C                        Changed pH values to have an east-west variation.
C                        Using the Henry's law constant function from CMAQ in
C                        place of local code.  Also changed the code for
C                        deposition to water to use a pH of 8.1 and the
C                        temperature of water in calculating the Henry's law
C                        constant.  Adjusted values of RSNOW0 = 1000 and
C                        A(NH3) = 20.  Added new dry deposition species: N2O5,
C                        NO3, Generic_aldehyde.  Corrected diffusivities of
C                        chemicals and water and viscosity of air to all be at
C                        the same temperature (273.15K).  Temperature and
C                        pressure adjustments to the values are not needed
C                        because the diffusivities and viscosity are always used
C                        as ratios, so the temperature-pressure dependence was
C                        removed.  Removed dry deposition species, ATRA and
C                        ATRAP, from output.  (D. Schwede, J. Pleim, and
C                        T. Otte)
C           28 Feb 2005  Added optional dry deposition species for chlorine
C                        and mercury.  (G. Sarwar, R. Bullock, and T. Otte)
C           02 Feb 2006  Added mesophyll resistance to dry deposition velocity
C                        calculation, and defined non-zero value for mercury.
C                        (D. Schwede, J. Pleim, and R. Bullock)
C           01 Aug 2007  Added a non-zero mesophyll resistance for NO, NO2, and
C                        CO.  Restored wet cuticle resistance for O3 based on
C                        field study measurements.  Added wet ground resistance.
C                        Changed ground resistance to include partitioning of
C                        wet and dry ground.  Updated pH of rain water for
C                        eastern United States and outside of North America.
C                        Changed reactivity for PAN.  Removed dry deposition
C                        velocity calculations for obsolete chlorine species
C                        ICL1 and ICL2.  Corrected error in the calculation of
C                        surface resistance over water where (Sc/Pr)**(2/3) had
C                        been inadvertently omitted from the numerator.
C                        Surface resistance over water is now a function of
C                        species.  Surface resistance over water now uses wet
C                        bulb temperature rather than ground (water) temperature
C                        in the calculation of the effective Henry's law
C                        constant, and the algorithm has been updated.  Changed
C                        (Sc/Pr)**(2/3) over water to a species-dependent,
C                        meteorologically dependent variable.  Effective Henry's
C                        law constant over land now uses 2-m temperature rather
C                        than layer 1 temperature. Changed ES
C                        into ES_AIR and ES_GRND, and changed QSS into QSS_AIR
C                        and QSS_GRND to clarify usage.  (J. Pleim, E. Cooter,
C                        J. Bash, T. Otte, and G. Sarwar)
C           07 Dec 2007  Add into CMAQ for in-line deposition velocities.
C                        (W. Hutzell, J. Young and T. Otte)
C           07 Jan 2008  Changed the value of d3, the scaling parameter used to
C                        estimate the friction velocity in surface waters from
C                        the atmospheric friction velocity to a value following
C                        Slinn et al. (1978) and Fairall et al. (2007).
C                        (J. Bash)
C           01 Feb 2008  Added bidirectional NH3 flux calculations. (J. Pleim and
C                        J. Young)
C           20 Mar 2008  Added a trap for undefined dry deposition velocities
C                        (e.g., NaN's).  (T. Otte)
C           21 Mar 2008  Added heterogeneous reaction for HONO. It affects HONO, NO2
C                        and HNO3 (G. Sarwar)
C           30 Apr 2008  Added five air toxic species to output.  (W. Hutzell
C                        and T. Otte)
C           August 2008  Applied a minimum value to ustg (0.001 m/s) to prevent 
C                        negative rbg values in the bidi calculation (J. Pleim) 
C           05 Oct 2009  Added condition that vegetation fraction must be
C                        greater than zero to be considered a land point.  This
C                        works around intermittent inconsistencies in surface
C                        fields in some WRF data sets.  (T. Otte)
C           Dec 2009     Revised bidirectional NH3 flux calculations to use soil 
C                        Gamma values read from gridded file.  Bidi flux calcs
C                        based on comparisons with Lillington, NC corn data 2007 (J. Pleim)
C           30 Mar 2010  Modified to output the NH3 bidi stomtal, cuticle and soil component 
C                        fluxes and chaged NH3 bidi variables used in estimating the compensation
C                        point double. (J. Bash)
C           16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C           20 May 2011  D.Schwede: add MOSAIC processing
C           14 Jul 2011  Replaced dw25 calculation with Hayduk and Laudie method.
C                        LeBas molar volumes are from the Schroeder additive method
C                        with the exception of HGIIGAS (modeled as HgCl2) which was 
C                        obtained using the Tyn and Calus method. Also, ICL1 and ICL2
C                        were removed. (D. Schwede)
C           27 Jul 2011 J.Bash: Parmaterized the mesophyll resistance as a function
C                        of solubility following Wesely 1989 Atmos Environ.
C           15 Aug 2011  Modified HONO calculation so that deposition velocity for NO2
C                        that is output in DEPV file does not include the loss due to
C                        the heterogeneous reaction. This additional loss is now
C                        accounted for in vdiff.F (D. Schwede and G. Sarwar)
C           29 Aug 2011 Added NH3 bidirectional flux variables and modules and integrated
C                       NH3 bidi algorithms with MOSAIC algorithms. NH3 bidi routines now
C                       read in foratted EPIC output and maintain a soil NH4 budget and 
C                       fluxes are calculated for individual land cover types. 
C                       (J. Bash and D. Schwede)
C           22 Sep 2011 -- incorporated twoway model implementation
C                       -- removed non-use dluse array
C                          (David Wong)
C           26 Sep 2011 -- made the number of actual and dummy arguments the same in
C                          calling subroutine Init_ABFlux
C                          (David Wong)
C-------------------------------------------------------------------------------

      USE HGRD_DEFN      ! horizontal grid specifications
      USE DEPVVARS
      USE VGRD_DEFN      ! to get VGTYP_GD
      USE UTILIO_DEFN   
      USE SUBST_MODULES  ! stenex
      Use ABFlux_Mod     ! bidi NH3 exchange routines
      Use LSM_Mod        ! land surface model variables needed for mosaic and bidi
      Use Mosaic_Mod      ! contains mosaic routines
      Use Bidi_Mod, Only: lufrac
#ifdef twoway
      USE twoway_data_module, only : num_land_cat
#endif

      IMPLICIT NONE
C Includes:

      INCLUDE SUBST_CONST     ! constants
      INCLUDE SUBST_FILES_ID  ! file name parameters
      INCLUDE SUBST_PE_COMM   ! PE communication displacement and direction

C Arguments:

      INTEGER,     INTENT( IN )    :: jdate
      INTEGER,     INTENT( IN )    :: jtime
      Integer,     Intent( IN )    :: dt(3)      !model time step 
      LOGICAL,     INTENT( IN )    :: abflux, sfc_hono, mosaic, fst
      REAL,        INTENT( IN )    :: cgridl1( :,:,: )    ! layer 1 concentrations
      REAL,        INTENT( OUT )   :: depvel_gas( :,:,: )
      REAL,        INTENT( OUT )   :: pvd( :,:,: )
      REAL,        INTENT( OUT )   :: cmp( :,:,: ) ! Bidi component fluxes JOB
      
C Local Variables:

      REAL,            PARAMETER :: a0         = 8.0     ! [dim'less]
      INTEGER                    :: c
      REAL                       :: cp_air               ! specific heat of moist air
      REAL                       :: ctemp2               ! temp2 [C]

      REAL,            PARAMETER :: d3         = 1.38564e-2 ! [dim'less]
                                               ! k*sqrt(rhoair/rhowater) from Slinn 78
      REAL,    SAVE, ALLOCATABLE :: delta     ( :,: )
      REAL                       :: dw
      REAL                       :: dw25       ! diffusivity of water at 298.15 k
      REAL,            PARAMETER :: dwat       = 0.2178  ! [cm^2/s] at 273.15K
      LOGICAL                    :: effective  ! true=compute effective Henry's Law const
      INTEGER                    :: elapsedsec
      INTEGER, SAVE              :: endcol
      INTEGER, SAVE              :: endrow
      INTEGER                    :: endcol_g
      INTEGER                    :: endrow_g
      REAL                       :: es_air
      REAL                       :: es_grnd
      LOGICAL, SAVE              :: first_call = .TRUE.
      INTEGER                    :: gxoff                ! global origin offset from file
      INTEGER                    :: gyoff                ! global origin offset from file
      REAL                       :: hcan
      REAL                       :: heff                 ! effective Henry's Law constant
      REAL                       :: heff_ap              ! Henry's Law constant for leaf apoplast M/atm
      REAL,            EXTERNAL  :: hlconst              ! [M / atm]
      REAL                       :: hplus
      REAL,            PARAMETER :: hplus_ap   = 1.0e-6  ! pH=6.0 leaf apoplast solution Ph (Massad et al 2008)      
      REAL,            PARAMETER :: hplus_def  = 1.0e-5  ! pH=5.0
      REAL,            PARAMETER :: hplus_east = 1.0e-5  ! pH=5.0
      REAL,            PARAMETER :: hplus_h2o  = 7.94328e-9 ! 10.0**(-8.1)
      REAL,            PARAMETER :: hplus_west = 3.16228e-6 ! 10.0**(-5.5)
      LOGICAL, SAVE              :: ifq2       = .FALSE. ! Q2 in metcro2d?
      INTEGER                    :: ifsnow               ! 1=snow
      LOGICAL, SAVE              :: ifwr       = .FALSE. ! canopy wetness from LSM?
      REAL,            PARAMETER :: kvis       = 0.132   ! [cm^2 / s] at 273.15K
      REAL                       :: kvisw      ! kinematic viscosity of water [cm^2/s]
      INTEGER                    :: l
      REAL                       :: laicr                ! cell leaf area index
      INTEGER, SAVE              :: logdev               ! unit number for the log file
      INTEGER, SAVE, ALLOCATABLE :: lstwetdate( :,: )
      INTEGER, SAVE, ALLOCATABLE :: lstwettime( :,: )
      REAL                       :: lv                   ! latent heat of vaporization
      INTEGER                    :: n
      CHARACTER( 16 ), PARAMETER :: pname      = 'M3DRY'
      REAL,            PARAMETER :: pr         = 0.709   ! [dim'less]
      REAL                       :: q2p0cr               ! cell 2.0 m water vapor mix ratio
      REAL                       :: qss_air
      REAL                       :: qss_grnd
      INTEGER                    :: r
      REAL                       :: rac
      REAL                       :: rbc
      REAL                       :: rbsulf
      REAL                       :: rbw
      REAL                       :: rci
      REAL                       :: rcut
      REAL,            PARAMETER :: rcut0      = 3000.0  ! [s/m]
      REAL,            PARAMETER :: rcw0       = 125000.0 ! acc'd'g to Padro and
                                                          ! adapted from Slinn 78
      REAL,            PARAMETER :: resist_min = 1.0e-30 ! minimum resistance
      REAL,            PARAMETER :: rg0        = 1000.0  ! [s/m]
      REAL                       :: rgnd
      REAL                       :: rgndc
      REAL                       :: rgw                  ! resist for water-covered sfc
      REAL,            PARAMETER :: rgwet0     = 25000.0 ! [s/m]
      REAL                       :: rh_air               ! rel humidity (air)
      REAL                       :: rh_grnd              ! rel humidity (ground)
      REAL                       :: rinc
      REAL,            PARAMETER :: rsndiff    = 10.0    ! snow diffusivity fac
      REAL                       :: rsnow
      REAL,            PARAMETER :: rsnow0     = 1000.0
      REAL                       :: rstom
      REAL                       :: rsurf
      REAL                       :: rwet                 ! wet sfc resist (cuticle or grnd)
      REAL                       :: rwetsfc
      REAL                       :: scw_pr_23            ! (scw/pr)**2/3
      INTEGER, SAVE              :: strtcol
      INTEGER, SAVE              :: strtrow
      INTEGER                    :: strtcol_g
      INTEGER                    :: strtrow_g
      REAL,            PARAMETER :: svp2       = 17.67   ! from MM5 and WRF
      REAL,            PARAMETER :: svp3       = 29.65   ! from MM5 and WRF
      REAL,            PARAMETER :: rt25inK    = 1.0/(stdtemp + 25.0) ! 298.15K = 25C
      REAL                       :: temp2p0cr            ! cell 2.0-m temp
      REAL                       :: tempgcr              ! cell ground temp
      REAL                       :: tw                   ! wet bulb temp.
      REAL,            PARAMETER :: twothirds  = 2.0 / 3.0
      REAL                       :: ustarcr              ! cell friction velocity
      REAL                       :: vegcr                ! cell veg coverage fraction
      CHARACTER( 16 )            :: vname
      REAL                       :: wrmax
      REAL,    SAVE              :: xcent
      REAL                       :: xm                   ! liquid water mass frac
      CHARACTER( 96 )            :: xmsg = ' '
      CHARACTER( 16 ), SAVE      :: vname_ra, vname_rc, vname_rn, vname_rs
      REAL,    SAVE              :: ycent

!     REAL,    SAVE, ALLOCATABLE :: dluse     ( :,: )
      REAL,    SAVE, ALLOCATABLE :: lon       ( :,: )
!      REAL,    SAVE, ALLOCATABLE :: lwmask    ( :,: ) moved to LSM_MOD
      REAL                       :: lai       ( ncols,nrows ) ! leaf area index [area/area]
      REAL                       :: prsfc     ( ncols,nrows ) ! surface pressure [Pa]
      REAL                       :: q2        ( ncols,nrows ) ! 2-m water vapor mix rat [kg/kg]
      REAL                       :: ra        ( ncols,nrows ) ! aerodynamic resistance [s/m]
      REAL                       :: rc        ( ncols,nrows ) ! convective rain [cm]
      REAL                       :: rgrnd     ( ncols,nrows ) ! SW radiation at ground [W/m2]
      REAL                       :: rn        ( ncols,nrows ) ! nonconvective rain [cm]
      REAL                       :: rs        ( ncols,nrows ) ! stomatal resistance [s/m]
      REAL                       :: snocov    ( ncols,nrows ) ! snow cover [1=yes, 0=no]
      REAL                       :: temp2p0   ( ncols,nrows ) ! 2.0-m temperature [K]
      REAL                       :: tempg     ( ncols,nrows ) ! ground surface temperature [K]
      REAL                       :: ustar     ( ncols,nrows ) ! friction velocity [m/s]
      REAL                       :: veg       ( ncols,nrows ) ! vegetation coverage [fraction]
      REAL                       :: wr        ( ncols,nrows ) ! precip intercepted by canopy [m]
      REAL                       :: wspd10    ( ncols,nrows ) ! 10-m wind speed [m/s]
      REAL                       :: wstar     ( ncols,nrows ) ! convective velocity scale [m/s]
      REAL                       :: zruf      ( ncols,nrows ) ! roughness length [m]

      INTEGER, SAVE              :: n_spc_m3dry = ltotg       ! from DEPVVARS module

      REAL                       :: ar       ( ltotg )        ! reactivity relative to HNO3
      REAL                       :: dif0     ( ltotg )        ! molecular diffusivity [cm2/s]
      REAL                       :: lebas    ( ltotg )        ! Le Bas molar volume [cm3/mol ]
      REAL                       :: meso     ( ltotg )        ! Exception for species that 
                                                              ! react with cell walls. fo in 
                                                              ! Wesely 1989 eq 6.
      REAL, SAVE                 :: scc_pr_23( ltotg )        ! (SCC/PR)**2/3, fn of DIF0
      CHARACTER( 16 )            :: subname  ( ltotg )        ! for subroutine HLCONST

      INTEGER :: SPC

C-------------------------------------------------------------------------------
C For heterogenous hono on surfaces
      REAL                       :: conc_no2                  ! concentration of NO2
      REAL                       :: kno2                      ! first order rate constant for the heterogenous reaction [1/s]  
      REAL,    SAVE, ALLOCATABLE :: purb      ( :,: )         ! urban percent [%]
      REAL                       :: surf_bldg                 ! Surface area of bldgs to volume of air [-]
      REAL                       :: surf_leaf                 ! Surface area of leaves to volume of air [-]
      REAL,    SAVE, ALLOCATABLE :: zf        ( :,: )         ! layer height [m]      
      
C-------------------------------------------------------------------------------
C For Ammonia bi-directional flux                      
      REAL                       :: sltyp     ( ncols,nrows ) ! soil type
      REAL                       :: wg        ( ncols,nrows ) ! soil moisture in top 1 cm (vol frc)
      REAL                       :: w2        ( ncols,nrows ) ! soil moisture in top 1 m (vol frc)
      Real                       :: cnh3         ! Layer 1 NH3 concentration from CGRID [ppm]
      Real                       :: tpvd         ! temp pvd variable
      Real                       :: f_stom       ! stomatal flux component
      Real                       :: f_cut        ! cuticular flux component
      Real                       :: f_soil       ! soil flux component
      Real                       :: f_emis       ! Emission flux component
      Real                       :: f_dep        ! Deposition flux component
      Real                       :: f_ag         ! flux from agriculture
      Real                       :: f_nat        ! flux from natural systems
      Real                       :: f_wat        ! direct flux to surface water
      Real                       :: lnh3         ! loss part of ammonia flux
                                                 ! Note that lnh3 is stored in DEPVEL_GAS(nh3) for use in vdiff
C-------------------------------------------------------------------------------
C For Mosaic with Ammonia bi-directional flux 
      REAL, PARAMETER           :: betah = 8.21  ! MM5 v3.7.4 uses Hogstrom
      REAL, PARAMETER           :: gamah = 11.6
      REAL, PARAMETER           :: f3min = 0.25
      REAL, PARAMETER           :: ftmin = 0.0000001             ! m/s
      REAL, PARAMETER           :: nscat = 16
      REAL, PARAMETER           :: rsmax = 5000.0                ! s/m
      Real                      :: soit2     ( ncols,nrows ) ! soil temperature in top m [K]
      Real                      :: qv        ( ncols,nrows ) ! layer 1 water vapor mix rat [kg/kg]
      Real                      :: pres      ( ncols,nrows ) ! layer 1 pressure [Pa]
      Real                      :: zh        ( ncols,nrows ) ! layer 1 water vapor mix rat [kg/kg]
      Real                      :: moli      ( ncols,nrows ) ! inverse of Monin-Obukhov length [m]
      Real                      :: ta        ( ncols,nrows ) ! layer 1 temperature [K]      
      Real                      :: uwind     ( ncols+1,nrows+1 ) ! dot grid horizontal wind            
      Real                      :: vwind     ( ncols+1,nrows+1 ) ! dot grid cross wind                   
      Real                      :: v_cross
      Real                      :: u_cross
      Real                      :: rich
      Real                      :: rsj      
      Real                      :: psihmm5      
      Real                      :: presmb ! pressure in mb
      Real                      :: qvs                   ! specific humidity layer 1
      Real                      :: soit2cr
      Real                      :: theta
      Real                      :: thetav
      Real                      :: betap
      Real                      :: hu
      Real                      :: dthvdz
      Real                      :: radl
      Real                      :: radf
      Real                      :: f1
      Real                      :: f2 
      Real                      :: f3 
      Real                      :: f4
      Real                      :: ftot  
      Real                      :: fshelt
      Real                      :: gs  
      Real                      :: ga
      Real                      :: racj
      Real                      :: rcij
      Real                      :: raw
      Real                      :: rgwwat        
      Real                      :: w2avail
      Real                      :: w2mxav            
      Real                      :: wfccr      
      Real                      :: zrufcr
      Real                      :: zol   ! zh*moli
      Real                      :: zntol ! z0*moli
      Integer                   :: soilcat
      Integer                   :: j
      Integer                   :: jday
!      Integer, Save              :: n_spc_m3dry = ltotg       ! from DEPVVARS module
                                                 
C-------------------------------------------------------------------------------
C-- Chemical-Dependent Parameters (Original Source: Modified ADOM - Padro)
C
C                                                        at 298.15 K
C     Species   Dif0(cm2/s) Alphastar   Reactivity     -DKHOR [K]  KH0 [M/atm]
C     _______   ___________ _________   __________     _________  __________
C  1   SO2      0.1089 ~    1000.       8.00            3100.0 ~     1.2e00 ~
C  2   SULF      --       --           --                --           --
C  3   NO2      0.1361 ~    1.00      8.00 2.00*        2500.0 ~     1.2e-2 ~
C  4   NO       0.1802 ~    1         2.0+              1500.0 ~     1.9e-3 ~
C  5   O3       0.1444 ~    10.00       15.0  8@        2700.0 ~     1.2e-2 ~
C  6   HNO3     0.1628 ~    1E9       18.0 800* 8000**  8700.0 ~     2.6e+6 ~
C  7   H2O2     0.2402 ~    1.00      12.0 30*          6600.0 ~     9.7e+4 ~
C  8   ACET ALD 0.1525 ~    1         10.+              5700.0 ~     1.3e+1 ~
C  9   HCHO     0.1877 ~    1.00      10.+              6400.0 ~     7.0e+3 ~
C 10   OP       0.1525 ~    1         10.0+             5200.0 ~     3.1e+2 ~
C 11   PAA      0.1220 ~    1         20+               5300.0 ~     8.4e+2 ~
C 12   ORA      0.1525 ~    1         20+               5700.0 ~     3.7e+3 ~
C 13   NH3      0.1978 ~    1E5       10.0              4100.0 ~     5.8e+1 ~
C 14   PAN      0.0938 ~    1         16.0~~            5900.0 ~     2.9e00 ~
C 15   HONO     0.1525 ~    1         20+               4800.0 ~     4.9e+1 ~
C 16   CO       0.1807 ~    1         5.0               1600.0 ~     9.5e-4 ~
C 17   METHANOL 0.1363 ~    1.0 ~       2.0 ~           5200.0 ~     2.2e+2 ~
C --   CO2      0.1381 ~                                2400.0 ~     3.4e-2 ~
C 18   N2O5     0.0808 ^^             5000.0**
C 19   NO3      0.1153 ^^             5000.0**
C 20   GEN ALD  0.1525 ##   1.0       10.0##
C 21   CL2      0.1080 %              10.0 %
C 22   HOCL     0.1300 %              10.0 %
C 23   HCL      0.1510 %              8000.0 %
C 24   FMCL     0.1094 %              10.0 %
C 27   HG       0.1194 $              0.1 $
C 28   HGIIGAS  0.0976 $              8000.0 $
C 50   HEXAMETHYLENE DIISOCYANATE       10.0 <>
C 51   HYDRAZINE                        20.0 <>
C 52   MALEIC ANHYDRIDE                 10.0 <>
C 53   TOLUENE DIISOCYANATE             10.0 <>
C 54   TRIETHYLAMINE                    10.0 <>
C
C---------Notes
C  * Updates based on literature review 7/96 JEP
C  # Diff and H based on Wesely (1988) same as RADM
C  + Estimated by JEP 2/97
C  @ Updated by JEP 9/01
C  ~ Added by YW 1/02.  Dif0 based on Massman (1998).  Henry's Law constant
C    is defined here as: h=cg/ca, where cg is the concentration of a species
C    in gas-phase, and ca is its aqueous-phase concentration.  The smaller h,
C    the larger solubility.  Henry's Law constant in another definition (KH):
C    KH = ca/pg [M/atm], KH = KH0 * exp(-DKH/R(1/T-1/T0)), where KH0 and -DKH
C    values are from Rolf Sander (1999).  h=1/(KH*R*T).
C ** Update by DBS based on estimates by JEP 1/03
C ^^ From Bill Massman, personal communication 4/03
C ## Diffusivity calculated by SPARC, reactivity = other aldehydes
C ++ Dif0 in Massman is diffusivity at temperature 0C and 1 atm (101.325kPa), so
C    chemicals that were not in Massman's paper need to be adjusted.  We assume
C    JEP's original values were for 25C and 1 atm.
C  % Added by G. Sarwar (10/04)
C  $ Added by R. Bullock (02/05) HG diffusivity is from Massman (1999).
C    HGIIGAS diffusivity calculated from the HG value and a mol. wt. scaling
C    factor of MW**(-2/3) from EPA/600/3-87/015. ORD, Athens, GA.  HGIIGAS
C    mol.wt. used is that of HgCl2.  Reactivity of HG is 1/20th of NO and NO2
C    values based on general atmospheric lifetimes of each species.  Reactivity
C    of HGIIGAS is based on HNO3 surrogate.
C @@ Mesophyll resistances for NO, NO2, and CO added by J. Pleim (07/07) based
C    on values in Pleim, Venkatram, and Yamartino, 1984:  ADOM/TADAP Model
C    Development Program, Volume 4, The Dry Deposition Module.  ERT, Inc.,
C    Concord, MA (peer reviewed).
C ~~ Reactivity for PAN changed from 4.0 to 16.0 by J. Pleim (07/07) based on
C    comparisons with Turnipseed et al., JGR, 2006.
C %% Species ICL1 and ICL2 are removed, not used in CB05.  G. Sarwar (07/07)
C <> Hazardous Air Pollutants that are believed to undergo significant dry
C    deposition. Hydrazine and triethylamine reactivities are based on analogies
C    to NH3. Maleic anhydride reactivity is assumed similar to aldehydes.
C    Toluene diisocyanate and hexamethylene diisocyanate reactivities are
C    assumed to be similar to SO2. Diffusivities are calculated with standard
C    formulas.  W. Hutzell (04/08)
C-------------------------------------------------------------------------------

      DATA subname( 1), dif0( 1), ar( 1), meso( 1), lebas( 1) / 'SO2             ', 0.1089,   10.0,      0.0,  35.0/
      DATA subname( 2), dif0( 2), ar( 2), meso( 2), lebas( 2) / 'SULFATE         ', 0.0001,    0.0,      0.0,  49.0/
      DATA subname( 3), dif0( 3), ar( 3), meso( 3), lebas( 3) / 'NO2             ', 0.1361,    2.0,      0.1,  21.0/
      DATA subname( 4), dif0( 4), ar( 4), meso( 4), lebas( 4) / 'NO              ', 0.1802,    2.0,      0.0,  14.0/
      DATA subname( 5), dif0( 5), ar( 5), meso( 5), lebas( 5) / 'O3              ', 0.1444,    8.0,      1.0,  21.0/
      DATA subname( 6), dif0( 6), ar( 6), meso( 6), lebas( 6) / 'HNO3            ', 0.1067, 8000.0,      0.0,  35.0/
      DATA subname( 7), dif0( 7), ar( 7), meso( 7), lebas( 7) / 'H2O2            ', 0.1300,   30.0,      1.0,  28.0/
      DATA subname( 8), dif0( 8), ar( 8), meso( 8), lebas( 8) / 'ACETALDEHYDE    ', 0.1111,   10.0,      0.0,  56.0/
      DATA subname( 9), dif0( 9), ar( 9), meso( 9), lebas( 9) / 'FORMALDEHYDE    ', 0.1554,   10.0,      0.0,  35.0/
      DATA subname(10), dif0(10), ar(10), meso(10), lebas(10) / 'METHYLHYDROPEROX', 0.1179,   10.0,      0.1,  49.0/
      DATA subname(11), dif0(11), ar(11), meso(11), lebas(11) / 'PEROXYACETIC_ACI', 0.0868,   20.0,      0.1,  70.0/
      DATA subname(12), dif0(12), ar(12), meso(12), lebas(12) / 'ACETIC_ACID     ', 0.0944,   20.0,      0.0,  63.0/
      DATA subname(13), dif0(13), ar(13), meso(13), lebas(13) / 'NH3             ', 0.1978,   20.0,      0.0,  28.0/
      DATA subname(14), dif0(14), ar(14), meso(14), lebas(14) / 'PAN             ', 0.0687,   16.0,      0.1,  91.0/
      DATA subname(15), dif0(15), ar(15), meso(15), lebas(15) / 'HNO2            ', 0.1349,   20.0,      0.1,  28.0/
      DATA subname(16), dif0(16), ar(16), meso(16), lebas(16) / 'CO              ', 0.1807,    5.0,      0.0,  14.0/
      DATA subname(17), dif0(17), ar(17), meso(17), lebas(17) / 'METHANOL        ', 0.1329,    2.0,      0.0,  42.0/
      DATA subname(18), dif0(18), ar(18), meso(18), lebas(18) / 'N2O5            ', 0.0808, 5000.0,      0.0,  49.0/
      DATA subname(19), dif0(19), ar(19), meso(19), lebas(19) / 'NO3             ', 0.1153, 5000.0,      0.0,  28.0/
      DATA subname(20), dif0(20), ar(20), meso(20), lebas(20) / 'GENERIC_ALDEHYDE', 0.0916,   10.0,      0.0,  56.0/
      DATA subname(21), dif0(21), ar(21), meso(21), lebas(21) / 'CL2             ', 0.1080,   10.0,      0.0,  49.0/
      DATA subname(22), dif0(22), ar(22), meso(22), lebas(22) / 'HOCL            ', 0.1300,   10.0,      0.0,  38.5/
      DATA subname(23), dif0(23), ar(23), meso(23), lebas(23) / 'HCL             ', 0.1510, 8000.0,      0.0,  31.5/
      DATA subname(24), dif0(24), ar(24), meso(24), lebas(24) / 'FMCL            ', 0.1094,   10.0,      0.0,  45.5/
      DATA subname(25), dif0(25), ar(25), meso(25), lebas(25) / 'HG              ', 0.1194,    0.1,      0.0,  14.8/   ! lebas not used
      DATA subname(26), dif0(26), ar(26), meso(26), lebas(26) / 'HGIIGAS         ', 0.0976, 8000.0,      0.0,  95.0/   ! estimation from back calculating to get dw25 = 1.04e-5 (Garland et al, 1965)
      DATA subname(27), dif0(27), ar(27), meso(27), lebas(27) / 'TECDD_2378      ', 0.0525,    2.0,      0.0, 217.0/
      DATA subname(28), dif0(28), ar(28), meso(28), lebas(28) / 'PECDD_12378     ', 0.0508,    2.0,      0.0, 234.5/
      DATA subname(29), dif0(29), ar(29), meso(29), lebas(29) / 'HXCDD_123478    ', 0.0494,    2.0,      0.0, 252.0/
      DATA subname(30), dif0(30), ar(30), meso(30), lebas(30) / 'HXCDD_123678    ', 0.0494,    2.0,      0.0, 252.0/
      DATA subname(31), dif0(31), ar(31), meso(31), lebas(31) / 'HXCDD_123478    ', 0.0494,    2.0,      0.0, 252.0/
      DATA subname(32), dif0(32), ar(32), meso(32), lebas(32) / 'HPCDD_1234678   ', 0.0480,    2.0,      0.0, 269.5/
      DATA subname(33), dif0(33), ar(33), meso(33), lebas(33) / 'OTCDD           ', 0.0474,    2.0,      0.0, 287.0/
      DATA subname(34), dif0(34), ar(34), meso(34), lebas(34) / 'TECDF_2378      ', 0.0534,    2.0,      0.0, 210.0/
      DATA subname(35), dif0(35), ar(35), meso(35), lebas(35) / 'PECDF_12378     ', 0.0517,    2.0,      0.0, 227.5/
      DATA subname(36), dif0(36), ar(36), meso(36), lebas(36) / 'PECDF_23478     ', 0.0517,    2.0,      0.0, 227.5/
      DATA subname(37), dif0(37), ar(37), meso(37), lebas(37) / 'HXCDF_123478    ', 0.0512,    2.0,      0.0, 245.0/
      DATA subname(38), dif0(38), ar(38), meso(38), lebas(38) / 'HXCDF_123678    ', 0.0512,    2.0,      0.0, 245.0/
      DATA subname(39), dif0(39), ar(39), meso(39), lebas(39) / 'HXCDF_234678    ', 0.0512,    2.0,      0.0, 245.0/
      DATA subname(40), dif0(40), ar(40), meso(40), lebas(40) / 'HXCDF_123789    ', 0.0512,    2.0,      0.0, 245.0/
      DATA subname(41), dif0(41), ar(41), meso(41), lebas(41) / 'HPCDF_1234678   ', 0.0487,    2.0,      0.0, 262.5/
      DATA subname(42), dif0(42), ar(42), meso(42), lebas(42) / 'HPCDF_1234789   ', 0.0487,    2.0,      0.0, 262.5/
      DATA subname(43), dif0(43), ar(43), meso(43), lebas(43) / 'OTCDF           ', 0.0474,    2.0,      0.0, 280.0/
      DATA subname(44), dif0(44), ar(44), meso(44), lebas(44) / 'NAPHTHALENE     ', 0.0778,    4.0,      0.0, 119.0/
      DATA subname(45), dif0(45), ar(45), meso(45), lebas(45) / '1NITRONAPHTHALEN', 0.0692,    4.0,      0.0, 133.0/
      DATA subname(46), dif0(46), ar(46), meso(46), lebas(46) / '2NITRONAPHTHALEN', 0.0692,    4.0,      0.0, 133.0/
      DATA subname(47), dif0(47), ar(47), meso(47), lebas(47) / '14NAPHTHOQUINONE', 0.0780,    4.0,      0.0, 119.0/
      DATA subname(48), dif0(48), ar(48), meso(48), lebas(48) / 'HEXAMETHYLE_DIIS', 0.0380,   10.0,      0.0, 196.0/
      DATA subname(49), dif0(49), ar(49), meso(49), lebas(49) / 'HYDRAZINE       ', 0.4164,   20.0,      0.0,  42.0/
      DATA subname(50), dif0(50), ar(50), meso(50), lebas(50) / 'MALEIC_ANHYDRIDE', 0.0950,   10.0,      0.0,  70.0/
      DATA subname(51), dif0(51), ar(51), meso(51), lebas(51) / '24-TOLUENE_DIIS ', 0.0610,   10.0,      0.0, 154.0/
      DATA subname(52), dif0(52), ar(52), meso(52), lebas(52) / 'TRIETHYLAMINE   ', 0.0881,   20.0,      0.0, 154.0/

      IF ( first_call ) THEN
         first_call = .FALSE.
         logdev  = init3()                 

         DO l = 1, n_spc_m3dry
            IF ( dif0( l ) > 0.0 ) THEN
               scc_pr_23( l ) = ( ( kvis / dif0( l ) ) / pr ) ** twothirds
            ELSE
               scc_pr_23( l ) = 0.0
            END IF
         END DO
         
         IF ( .NOT. desc3( met_cro_2d ) ) THEN
            xmsg = 'Could not get  met_cro_2d  file description'
            CALL m3exit( pname, jdate, jtime, xmsg, xstat2 )
         END IF                          

! ccccccccccccccccccccc enable backward compatiblity ccccccccccccccccccccc

         SPC = INDEX1( 'RA', NVARS3D, VNAME3D )
         IF (SPC .GT. 0) THEN
            vname_ra = 'RA'
         ELSE
            vname_ra = 'RADYNI'
         END IF

         SPC = INDEX1( 'RCA', NVARS3D, VNAME3D )
         IF (SPC .GT. 0) THEN
            vname_rc = 'RCA'
         ELSE
            vname_rc = 'RC'
         END IF

         SPC = INDEX1( 'RS', NVARS3D, VNAME3D )
         IF (SPC .GT. 0) THEN
            vname_rs = 'RS'
         ELSE
            vname_rs = 'RSTOMI'
         END IF

         SPC = INDEX1( 'RNA', NVARS3D, VNAME3D )
         IF (SPC .GT. 0) THEN
            vname_rn = 'RNA'
         ELSE
            vname_rn = 'RN'
         END IF

         xcent = xcent3d
         ycent = ycent3d        

         DO l = 1, nvars3d
            IF ( vname3d( l ) == 'WR' ) THEN   ! canopy wetness is in METCRO2D
               ifwr = .TRUE.
               WRITE( logdev,6501 )
               EXIT
            END IF
         END DO

         DO l = 1, nvars3d
            IF ( vname3d( l ) == 'Q2' ) THEN   ! 2-m mixing ratio is in METCRO2D
               ifq2 = .TRUE.
               WRITE( logdev,6503 )
               EXIT
            END IF
         END DO

         CALL subhfile ( grid_cro_2d, gxoff, gyoff, strtcol_g, endcol_g, strtrow_g, endrow_g )

         CALL subhfile ( met_cro_2d, gxoff, gyoff, strtcol, endcol, strtrow, endrow )
         
!         Write(logdev,*) 'Before read lufrac'
!         Write(logdev,*) 'n_lufrac', n_lufrac
         If( abflux .OR. mosaic ) Then                                 
            IF ( .NOT. ALLOCATED ( lufrac ) ) THEN
               ALLOCATE ( lufrac( n_lufrac,my_ncols,my_nrows ) )
            END IF
            DO l = 1, n_lufrac
               WRITE( vname,'( "LUFRAC_",I2.2 )' ) l
!              IF ( .NOT. interpx (grid_cro_2d, vname, pname, strtcol, endcol,
!    &                             strtrow, endrow, 1, 1, jdate, jtime,
               IF ( .NOT. interpx (grid_cro_2d, vname, pname, strtcol_g, endcol_g,
     &                             strtrow_g, endrow_g, 1, 1, jdate, jtime,
     &                             lufrac( l,:,: )) ) THEN   
                  WRITE( xmsg,9001 ) vname, grid_cro_2d
                  GO TO 1001
               END IF
            END DO                      
         End If        
             
!         Write(logdev,*) 'Before init_ABFlux'
         If ( abflux ) Then         
            CALL Init_ABFlux(jdate, jtime)
         EndIf         

C-------------------------------------------------------------------------------
C Read fields from grid_cro_2d.
C-------------------------------------------------------------------------------

         IF ( .NOT. ALLOCATED ( lon ) ) THEN
            ALLOCATE ( lon ( my_ncols,my_nrows ) )
         END IF

         IF ( .NOT. ALLOCATED ( lwmask ) ) THEN
            ALLOCATE ( lwmask ( my_ncols,my_nrows ) )
         END IF
 
!        IF ( .NOT. ALLOCATED ( dluse ) ) THEN
!           ALLOCATE ( dluse ( my_ncols,my_nrows ) )
!        END IF

         vname = 'LON'
!        IF ( .NOT. interpx (grid_cro_2d, vname, pname, strtcol, endcol,
!    &                strtrow, endrow, 1, 1, jdate, jtime, lon) ) THEN
         IF ( .NOT. interpx (grid_cro_2d, vname, pname, strtcol_g, endcol_g,
     &                strtrow_g, endrow_g, 1, 1, jdate, jtime, lon) ) THEN
            WRITE( xmsg,9001 ) vname, grid_cro_2d
            GO TO 1001
         END IF

         vname = 'LWMASK'
!        IF ( .NOT. interpx (grid_cro_2d, vname, pname, strtcol, endcol,
!    &                strtrow, endrow, 1, 1, jdate, jtime, lwmask) ) THEN
         IF ( .NOT. interpx (grid_cro_2d, vname, pname, strtcol_g, endcol_g,
     &                strtrow_g, endrow_g, 1, 1, jdate, jtime, lwmask) ) THEN
            WRITE( xmsg,9001 ) vname, grid_cro_2d
            GO TO 1001
         END IF
 
!        vname = 'DLUSE'
!        IF ( .NOT. interpx (grid_cro_2d, vname, pname, strtcol, endcol,
!    &                strtrow, endrow, 1, 1, jdate, jtime, dluse) ) THEN
!           WRITE( xmsg,9001 ) vname, grid_cro_2d
!           GO TO 1001
!        END IF        

         IF ( .NOT. ALLOCATED ( delta ) ) THEN
            ALLOCATE ( delta( my_ncols,my_nrows ) )
            delta( :,: ) = 0.0
         END IF

         IF ( .NOT. ALLOCATED ( lstwetdate ) .AND. .NOT. ifwr ) THEN
            ALLOCATE ( lstwetdate( my_ncols,my_nrows ) )
            lstwetdate( :,: ) = 0
         END IF

         IF ( .NOT. ALLOCATED ( lstwettime ) .AND. .NOT. ifwr ) THEN
            ALLOCATE ( lstwettime( my_ncols,my_nrows ) )
            lstwettime( :,: ) = 0
         END IF

         IF ( sfc_hono ) THEN
            IF ( .NOT. ALLOCATED ( zf ) ) THEN
               ALLOCATE ( zf( my_ncols,my_nrows ) )
            END IF
            IF ( .NOT. ALLOCATED ( purb ) ) THEN
               ALLOCATE ( purb( my_ncols,my_nrows ) )
            END IF
            vname = 'PURB'
!           IF ( .NOT. interpx (grid_cro_2d, vname, pname, strtcol, endcol,
!    &                   strtrow, endrow, 1, 1, jdate, jtime, purb) ) THEN
            IF ( .NOT. interpx (grid_cro_2d, vname, pname, strtcol_g, endcol_g,
     &                   strtrow_g, endrow_g, 1, 1, jdate, jtime, purb) ) THEN
               WRITE( xmsg,9001 ) vname, grid_cro_2d
               GO TO 1001
            END IF
         END IF   ! src_hono                  

      END IF   ! first_call

C-------------------------------------------------------------------------------
C Read fields from met_cro_2d.
C-------------------------------------------------------------------------------
      
      vname = 'LAI'
      IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,  
     &                strtrow, endrow, 1, 1, jdate, jtime, lai) ) THEN
         WRITE( xmsg,9001 ) vname, met_cro_2d
         GO TO 1001
      END IF

      vname = 'PRSFC'
      IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,  
     &                strtrow, endrow, 1, 1, jdate, jtime, prsfc) ) THEN
         WRITE( xmsg,9001 ) vname, met_cro_2d
         GO TO 1001
      END IF

      IF ( ifq2 ) THEN
         vname = 'Q2'
         IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
     &                 strtrow, endrow, 1, 1, jdate, jtime, q2) ) THEN
            WRITE( xmsg,9001 ) vname, met_cro_2d
            GO TO 1001
         END IF
      END IF

!     vname = 'RADYNI'
!     IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
      IF ( .NOT. interpx (met_cro_2d, vname_ra, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, ra) ) THEN
!        WRITE( xmsg,9001 ) vname, met_cro_2d
         WRITE( xmsg,9001 ) vname_ra, met_cro_2d
         GO TO 1001
      END IF
!     ELSE
      IF (vname_ra .EQ. 'RADYNI') THEN
         DO r = 1, my_nrows
            DO c = 1, my_ncols
               ra( c,r ) = 1.0 / MAX( ra( c,r ), resist_min )
            END DO
         END DO
      END IF

!     vname = 'RC'
!     IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
      IF ( .NOT. interpx (met_cro_2d, vname_rc, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, rc) ) THEN
         WRITE( xmsg,9001 ) vname_rc, met_cro_2d
         GO TO 1001
      END IF

      vname = 'RGRND'
      IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, rgrnd) ) THEN
         WRITE( xmsg,9001 ) vname, met_cro_2d
         GO TO 1001
      END IF

!     vname = 'RN'
!     IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
      IF ( .NOT. interpx (met_cro_2d, vname_rn, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, rn) ) THEN
         WRITE( xmsg,9001 ) vname_rn, met_cro_2d
         GO TO 1001
      END IF

!     vname = 'RSTOMI'
!     IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
      IF ( .NOT. interpx (met_cro_2d, vname_rs, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, rs) ) THEN
         WRITE( xmsg,9001 ) vname_rs, met_cro_2d
         GO TO 1001
      END IF
!     ELSE
      IF (vname_rs .EQ. 'RSTOMI') THEN
         DO r = 1, my_nrows
            DO c = 1, my_ncols
               rs( c,r ) = 1.0 / MAX( rs( c,r ), resist_min )
            END DO
         END DO
      END IF

      vname = 'SNOCOV'
      IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, snocov) ) THEN
         WRITE( xmsg,9001 ) vname, met_cro_2d
         GO TO 1001
      END IF

      vname = 'TEMP2'
      IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, temp2p0) ) THEN
         WRITE( xmsg,9001 ) vname, met_cro_2d
         GO TO 1001
      END IF

      vname = 'TEMPG'
      IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, tempg) ) THEN
         WRITE( xmsg,9001 ) vname, met_cro_2d
         GO TO 1001
      END IF

      vname = 'USTAR'
      IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, ustar) ) THEN
         WRITE( xmsg,9001 ) vname, met_cro_2d
         GO TO 1001
      END IF

      vname = 'VEG'
      IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, veg) ) THEN
         WRITE( xmsg,9001 ) vname, met_cro_2d
         GO TO 1001
      END IF

      IF ( ifwr ) THEN
         vname = 'WR'
         IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
     &                 strtrow, endrow, 1, 1, jdate, jtime, wr) ) THEN
            WRITE( xmsg,9001 ) vname, met_cro_2d
            GO TO 1001
         END IF
      END IF

      vname = 'WSPD10'
      IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, wspd10) ) THEN
         WRITE( xmsg,9001 ) vname, met_cro_2d
         GO TO 1001
      END IF

      vname = 'WSTAR'
      IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, wstar) ) THEN
         WRITE( xmsg,9001 ) vname, met_cro_2d
         GO TO 1001
      END IF

      vname = 'ZRUF'
      IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, zruf) ) THEN
         WRITE( xmsg,9001 ) vname, met_cro_2d
         GO TO 1001
      END IF
      
      vname = 'SLTYP'
      IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, sltyp) ) THEN
         WRITE( xmsg,9001 ) vname, met_cro_2d
         GO TO 1001
      END IF        
               
!      Write(logdev,*) 'Before read metcro2d variables'
      If( abflux .OR. mosaic ) Then               
         vname = 'SOIM1'
         IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, wg) ) THEN
            WRITE( xmsg,9001 ) vname, met_cro_2d
            GO TO 1001
         END IF
            
         vname = 'SOIM2'
         IF ( .NOT. interpx (met_cro_2d, vname, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, w2) ) THEN
            WRITE( xmsg,9001 ) vname, met_cro_2d
            GO TO 1001
         END IF
                                    
         vname = 'MOLI'
         IF ( .NOT. interpx (met_crO_2d, vname, pname, strtcol, endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, moli) ) THEN
             WRITE( xmsg,9001 ) vname, met_cro_2d
             GO TO 1001
         END IF             

         vname = 'SOIT2'
         IF ( .NOT. interpx( met_cro_2d, vname, pname, strtcol,  endcol,
     &                strtrow, endrow, 1, 1, jdate, jtime, soit2) ) THEN
           WRITE( XMSG,9001 ) vname, met_cro_2d
           GO TO 1001
         END IF   
      EndiF
C-------------------------------------------------------------------------------
C Read fields from met_cro_3d.
C-------------------------------------------------------------------------------

      IF ( .NOT. ifq2 ) THEN  ! Q2 not in METCRO2D; substitute layer-1 QV
         vname = 'QV'
         IF ( .NOT. interpx (met_cro_3d, vname, pname, strtcol,  endcol,
     &                       strtrow, endrow, 1, 1, jdate, jtime, q2) ) THEN
            WRITE( xmsg,9001 ) vname, met_cro_3d
            GO TO 1001
         END IF
      END IF

      IF ( sfc_hono ) THEN
         vname = 'ZF'
         IF ( .NOT. interpx ( met_cro_3d, vname, pname, strtcol,  endcol,
     &                        strtrow, endrow, 1, 1, jdate, jtime, zf) ) THEN
            WRITE( xmsg,9001 ) vname, met_cro_3d
            GO TO 1001
         END IF
      END IF
      
!      Write(logdev,*) 'Before read metcro3d variables'
      IF ( abflux .OR. mosaic ) Then
         vname = 'ZH'
         IF ( .NOT. interpx ( met_cro_3d, vname, pname, strtcol,  endcol,
     &                     strtrow, endrow, 1, 1, jdate, jtime, zh) ) THEN
            WRITE( xmsg,9001 ) vname, met_cro_3d
            GO TO 1001
         END IF

         vname = 'QV'
         IF ( .NOT. interpx (met_cro_3d, vname, pname, strtcol,  endcol,
     &                    strtrow, endrow, 1, 1, jdate, jtime, qv) ) THEN
            WRITE( xmsg,9001 ) vname, met_cro_3d
            GO TO 1001
         END IF

         vname = 'TA'
         IF ( .NOT. interpx (met_cro_3d, vname, pname, strtcol,  endcol,
     &                    strtrow, endrow, 1, 1, jdate, jtime, ta) ) THEN
            WRITE( xmsg,9001 ) vname, met_cro_3d
            GO TO 1001
         END IF

         vname = 'PRES'
         IF ( .NOT. interpx (met_cro_3d, vname, pname, strtcol,  endcol,
     &                    strtrow, endrow, 1, 1, jdate, jtime, pres) ) THEN
            WRITE( xmsg,9001 ) vname, met_cro_3d
            GO TO 1001
         END IF      

         vname = 'UWIND'
         IF ( .NOT. interpx (met_dot_3d, vname, pname, strtcol,  endcol+1,
     &                    strtrow, endrow+1, 1, 1, jdate, jtime, uwind) ) THEN
            WRITE( xmsg,9001 ) vname, met_dot_3d
            GO TO 1001
         END IF
         vname = 'VWIND'
         IF ( .NOT. interpx (met_dot_3d, vname, pname, strtcol,  endcol+1,
     &                    strtrow, endrow+1, 1, 1, jdate, jtime, vwind) ) THEN
            WRITE( xmsg,9001 ) vname, met_dot_3d
            GO TO 1001
         END IF      
      EndIf
      
C-------------------------------------------------------------------------------
C Loop over grid cells and calculate dry deposition.
C-------------------------------------------------------------------------------

      effective = .TRUE.

      depvel_gas( :,:,: ) = 0.0  ! initialize for this time period
      pvd       ( :,:,: ) = 0.0  !     "       "    "    "    "
      cmp       ( :,:,: ) = 0.0  !     "       "    "    "    " JOB
      
!      Write(logdev,*) 'Before devel_gasj'
      IF ( mosaic .OR. abflux ) THEN
         depvel_gasj( :,:,:,: ) = 0.0 !     "       "    "    "    "
         ustarj     ( :,:,: )   = 0.0 !     "       "    "    "    "
         IF ( fst ) then
            rcanj      ( :,:,:,: ) = 1.0e30 !     "       "    "    "    "
            vd_fst_gasj( :,:,:,: ) = 0.0 !     "       "    "    "    "
         END IF
         CALL SUBST_COMM ( UWIND, DSPL_N0_E1_S0_W0, DRCN_E )
         CALL SUBST_COMM ( VWIND, DSPL_N1_E0_S0_W0, DRCN_N )         
      END IF

      jday = jdate - 1000 * nint(jdate/1000.)
            
!      WRITE(LOGDEV,*) 'my_nrows: ', my_nrows
!      WRITE(LOGDEV,*) 'my_ncols: ', my_ncols      
      
      DO r = 1, my_nrows
      DO c = 1, my_ncols

         laicr     = lai    ( c,r )
         q2p0cr    = q2     ( c,r )
         temp2p0cr = temp2p0( c,r )
         tempgcr   = tempg  ( c,r )
         ustarcr   = ustar  ( c,r )
         vegcr     = veg    ( c,r )
         soit2cr   = soit2  ( c,r )
         zrufcr    = zruf   ( c,r ) 
         soilcat   = NINT(sltyp( c,r ))
         ifsnow    = MAX( 0, NINT( snocov( c,r ) ) )
         
!         Write(logdev,*) 'before lufraccr'
         IF ( abflux .OR. mosaic ) THEN                       
            Do j = 1, n_lufrac 
               lufraccr( j ) = lufrac( j,c,r )
            End Do
         
            IF ( ( NINT(lwmask( c,r )) /= 0 ) .AND. ( vegcr > 0.0 ) ) THEN  ! land
!               Write(logdev,*) 'Before call calc_lai'
               CALL Calc_Lai(jday, jtime, soit2cr, lufraccr, laicr, zrufcr, 
     &                       laijcr, z0jcr, vegjcr, logdev, c, r )
               DO j=1,n_lufrac
                  laij(j,c,r) = laijcr(j)
                  z0j(j,c,r)  = z0jcr(j)
                  vegj(j,c,r) = vegjcr( j )
               END DO
            ELSE        
               DO j=1,n_lufrac
                  laij(j,c,r)   = 0.0
                  z0j(j,c,r)    = 0.000001
                  vegj( j,c,r ) = 0.0
               END DO
            END IF 
         EndIf ! mosaic loop

! Calculate the relative humidity of air and ground.

         IF ( temp2p0cr <= stdtemp ) THEN
            es_air = vp0 * EXP( 22.514 - (6.15e3 / temp2p0cr) )
         ELSE
            es_air = vp0 * EXP( svp2 * (temp2p0cr - stdtemp) /
     &                                 (temp2p0cr - svp3) )
         END IF

         qss_air = es_air * 0.622 / (prsfc( c,r ) - es_air)
         rh_air  = 100.0 * q2p0cr / qss_air
         rh_air  = MIN( 100.0, rh_air )

         IF ( tempgcr <= stdtemp ) THEN
            es_grnd = vp0 * EXP( 22.514 - (6.15e3 / tempgcr) )
         ELSE
            es_grnd = vp0 * EXP( svp2 * (tempgcr - stdtemp) /
     &                                  (tempgcr - svp3) )
         END IF

         qss_grnd = es_grnd * 0.622 / (prsfc( c,r ) - es_grnd)
         rh_grnd  = 100.0 * q2p0cr / qss_grnd
         rh_grnd  = MIN( 100.0, rh_grnd )
        
!         Write(logdev,*) 'Before OM simularity vars'
         IF ( mosaic .OR. abflux ) THEN
            zol   = zh(c,r) * moli(c,r)
            presmb = pres(c,r)/100.    ! convert layer 1 preesure from Pa to mb
            qvs = qv(c,r)/(1. + qv(c,r))
            theta = ta(c,r) * (1000./presmb)**(287.04/1004.E0)
            thetav = theta*(1+0.61*qv(c,r))

            IF ( ( NINT(lwmask( c,r )) /= 0 ) .AND. ( vegcr > 0.0 ) ) THEN  ! land
! raj is estimated using stability correction based on MOL for grid cell for now
               DO j = 1, n_lufrac

                  U_cross = 0.25 * (UWIND(C,R) + UWIND(C+1,R) +
     &                              UWIND(C,R+1) + UWIND(C+1,R+1))
                  V_cross = 0.25 * (VWIND(C,R) + VWIND(C+1,R) +
     &                              VWIND(C,R+1) + VWIND(C+1,R+1))

                  IF (lufraccr( j ) > 0.0 ) THEN
                     ustarj( j,c,r ) = ustarcr * SQRT( LOG( zh( c,r ) / Zrufcr )
     &                                               / LOG( zh( c,r ) / z0j( j,c,r ) ) )
                     zntol = z0j(j,c,r) * moli(c,r)

! ra calculation depends on met model used
                     Select Case(vgtyp_gd)
                        Case(7)
!                           Write(logdev,*) 'Before call ra_wrf'
                           Call ra_wrf(moli(c,r),zol,zntol,zh(c,r),ustarj(j,c,r),z0j(j,c,r),j,c,r)
                        Case(2)
                           If (Land_Scheme .EQ. 'USGS24') then              
                              Select Case(j)
                                 Case(16) ! water
                                    wfccr = -999.
                                 Case Default ! Land
                                    wfccr = wfc(soilcat)
                              End Select
                           End If
                           If (Land_Scheme .EQ. 'NLCD-MODIS') then              
                              Select Case(j)
                                 Case(1,31,48) ! water
                                    wfccr = -999.
                                 Case Default ! Land
                                    wfccr = wfc(soilcat)
                              End Select
                           End If
                          If (Land_Scheme .EQ. 'MODIS') then              
                             Select Case(j)
                                Case(17) ! water
                                   wfccr = -999.
                                Case Default ! Land
                                   wfccr = wfc(soilcat)
                             End Select
                           End If                        
!                           Write(logdev,*) 'Before call ra_mm5'
                           Call ra_mm5(wspd10(c,r),u_cross,v_cross,theta,thetav,tempgcr,qv(c,r),qss_grnd,
     &                          snocov(c,r),prsfc(c,r),lwmask(c,r),
     &                          wg(c,r),wfccr,zh(c,r),z0j(j,c,r),ustarj(j,c,r),raj(j,c,r),rich,
     &                          psihmm5,betap,hu,dthvdz)
                        Case Default
                           xmsg = 'met file format not recognized for mosaic'
                           CALL m3exit( pname, jdate, jtime, xmsg, xstat1 )
                     End Select
                  END IF ! lufraccr > 0
               END DO
            ELSE    ! water - no need to do mosaic
               DO j=1, n_lufrac
                  ustarj (j,c,r ) = ustarcr
                  raj (j,c,r ) = ra(c,r)
                  z0j (j,c,r ) = zrufcr
               END DO
            END IF !land or water     
         EndIf

         IF ( ( NINT(lwmask( c,r )) /= 0 ) .AND. ( vegcr > 0.0 ) ) THEN  ! land

            IF ( .NOT. ifwr ) THEN  ! approx canopy wetness - dew from Wesely

            ! canopy is wet if > trace precip. or moist with light winds

               IF ( ( rn( c,r ) + rc( c,r ) > 0.025 ) .OR.
     &              ( (0.6 + wspd10( c,r ))*(100.0-rh_grnd) <= 19.0 ) ) THEN

                  delta( c,r )      = 1.0
                  lstwetdate( c,r ) = jdate
                  lstwettime( c,r ) = jtime

               ELSE

                  IF ( rgrnd( c,r ) > 5.0 ) THEN  ! day (if at night, persist delta)

                  ! Determine if canopy was recently wet.

                     IF ( ( lstwetdate( c,r ) > 0 ) .AND.
     &                    ( lstwettime( c,r ) > 0 ) ) THEN  ! canopy recently wet

                        elapsedsec = secsdiff ( lstwetdate( c,r ),
     &                                          lstwettime( c,r ),
     &                                          jdate, jtime )

                        IF ( ( elapsedsec >     0 ) .AND.     ! assume canopy stays
     &                       ( elapsedsec <= 7200 ) ) THEN    ! wet for 2 h
                           delta( c,r ) = 1.0
                        ELSE IF ( ( elapsedsec >  7200 ) .AND.     ! ramp down DELTA
     &                            ( elapsedsec < 10800 ) ) THEN    ! between 2 & 3 h
                           delta( c,r ) = ( 10800.0 - FLOAT( elapsedsec ) ) / 3600.0
                        ELSE
                           delta( c,r )      = 0.0
                           lstwetdate( c,r ) = 0
                           lstwettime( c,r ) = 0
                        END IF

                     END IF

                  END IF

               END IF
               
!               Write(logdev,*) 'Before load deltaj'
               If( mosaic .OR. abflux ) Then
                  Do j=1,n_lufrac
                      deltaj(j,c,r) = delta(c,r)
                  End Do
               EndIf

            ELSE  ! Already have canopy wetness explicitly from met model

               wrmax = 0.2e-3 * vegcr * laicr   ! [m]
               IF ( wr( c,r ) <= 0.0 ) THEN
                  delta( c,r ) = 0.0
               ELSE
                  delta( c,r ) = wr( c,r ) / wrmax   ! refer to SiB model
                  delta( c,r ) = MIN( delta( c,r ), 1.0 )
               END IF
                              
!               Write(logdev,*) 'Before deltaj calc'
               If( mosaic .OR. abflux ) Then
                  Do j=1,n_lufrac
                     wrmax = 0.2e-3 * vegj(j,c,r) * laij(j,c,r)   ! [m] changed to vegj for mosaic JOB
                     If ( (wr( c,r ) <= 0.0) .or. (laij(j,c,r) <= 0)) Then
                        deltaj( j,c,r ) = 0.0
                     Else
                        deltaj( j,c,r ) = wr( c,r ) / wrmax   ! refer to SiB model
                        deltaj( j,c,r ) = MIN( deltaj( j,c,r ), 1.0 )
                     End If
                  End Do               
               EndIf
               
            END IF   ! canopy wetness

      ! Assign a pH for rain water based on longitude if US simulation.
      ! Otherwise use default pH.  Use pH value in HPLUS calculation.

            IF ( ( ycent >=   30.0 ) .AND. ( ycent <=  45.0 ) .AND.
     &           ( xcent >= -120.0 ) .AND. ( xcent <= -70.0 ) ) THEN
               IF ( lon( c,r ) > -100.0 ) THEN
                  hplus = hplus_east
               ELSE
                  hplus = hplus_west
               END IF
            ELSE
               hplus = hplus_def
            END IF

         ELSE   ! water
      ! Calculate the water surface film temperature: wet bulb temperature.
      ! Wet bulb temperature based on eqn in Fritschen and Gay (1979).

            ctemp2 = temp2p0cr - stdtemp
            lv     = lv0 - dlvdt * ctemp2
            cp_air = 1004.67 * ( 1.0 + 0.84 * q2p0cr )               ! [J/kg/K]
            tw     = ( ( 4.71e4 * cp_air / lv ) - 0.870 ) + stdtemp  ! [K]

         END IF  ! land or water

      ! Loop over species to calculate dry deposition velocities.

         n = 0
         ddloop: DO l = 1, n_spc_m3dry

            IF ( .NOT. use_depspc( l ) ) CYCLE ddloop

            n = n + 1

            IF ( TRIM( depspc( l ) ) == 'SULF' ) THEN  ! Sulfate (SULF)

         ! Sulfate calculation follows Wesely (1985), Eqn. 11.

               rbsulf = 1.0 / ( 0.002*(ustarcr**2 + 0.24*wstar( c,r )**2 ) /
     &                  ustarcr )
               depvel_gas( n,c,r ) = 1.0 / ( ra( c,r ) + rbsulf )
               
!               Write(logdev,*) 'Before calc rbsulf and depvel_gasj'
               IF ( mosaic ) THEN
                  DO j = 1, n_lufrac
                     IF ( lufraccr( j ) .GT. 0.0 ) THEN
                        rbsulf = 1.0 / ( 0.002 * ( ustarj( j,c,r )**2
     &                                  + 0.24 * wstar( c,r )**2 ) / ustarj( j,c,r ) )
                        depvel_gasj( j,l,c,r ) = 1.0 / ( raj( j,c,r ) + rbsulf )
                     END IF
                  END DO
               END IF ! mosaic

            ELSE

               IF ( ( NINT(lwmask( c,r )) == 0 ) .OR. ( vegcr == 0.0 ) ) THEN  ! water                 
                                

         ! Use CMAQ function for calculating the effective Henry's Law
         ! constant.  Note that original M3DRY wants inverse, non-
         ! dimensional Henry's Law (caq/cg).   Water pH is different
         ! than rain, and we need to use the water temperature.

                  heff  = hlconst( subname( l ), tw, effective, hplus_h2o )

         ! Make Henry's Law constant non-dimensional.

                  heff  = heff * 0.08205 * tw

         ! from Hayduk and Laudie
                  dw25 = 13.26e-5 / ( 0.8904**1.14 * lebas( l )**0.589 )
                  kvisw = 0.017 * EXP( -0.025 * ( tw - stdtemp ) )
                  dw    = dw25 * ( tw * rt25inK ) * ( 0.009025 / kvisw )
                  scw_pr_23 = ( ( kvisw / dw ) / pr ) ** twothirds
                  rgw   = scw_pr_23 / ( heff * d3 * ustarcr )
                  rsurf = rgw                  
!                  Write(logdev,*) 'Before call rsurfj'
                  IF ( mosaic ) THEN
                     DO j = 1, n_lufrac
                        IF ( lufraccr( j ) .GT. 0.0 ) THEN
                           rsurfj( j,l,c,r ) = rsurf
                        END IF
                     END DO
                  END IF ! mosaic

               ELSE   ! land

         ! Use CMAQ function for calculating the effective Henry's Law
         ! constant.  Note that original M3DRY wants inverse,
         ! non-dimensional Henry's Law (caq/cg).

                  heff  = hlconst( subname( l ), temp2p0cr, effective, hplus )

         ! Make Henry's Law constant non-dimensional.

                  heff  = heff * 0.08205 * temp2p0cr                  

         ! Wet surface resistance.  (Note DELTA = CWC in ADOM lingo.)
         ! This now applies to cuticle and ground.

                  IF ( TRIM( depspc( l ) ) /= 'O3' ) THEN
                     rwet = rcw0 / heff      ! wet cuticle
                  ELSE
                  ! Set RCW/LAI = 200 s/m on basis of Keysburg exp for O3
                     rwet = 1250.0    ! s/m
                  END IF

                  rgw  = rgwet0 / heff    ! wet ground

         ! Dry snow resistance.

                  rsnow = rsnow0 * a0 / ar( l )

         ! If the surface is cold and wet, use dry snow.

                  IF ( tempgcr < stdtemp ) THEN
                     rwetsfc = rsnow
                  ELSE
                     rwetsfc = rwet
                  END IF

         ! Dry cuticle resistance.

                  IF ( TRIM( depspc( l ) ) /= 'NH3' ) THEN
                     rcut = rcut0 * a0 / ar( l )
                  ELSE
                     rcut = 4000.0 * EXP( -0.054 * rh_air )
                  END IF

         ! Dry ground resistance.  (revised according to Erisman)

                  hcan  = zruf( c,r ) * 10.0
                  rinc  = 14.0 * laicr * hcan / ustarcr
                  rgnd  = rg0 * a0 / ar( l )
                  rgndc = 1.0 / ( ( 1.0 - delta( c,r ) ) / rgnd + delta( c,r ) / rgw )
     &                  + rinc          ! Add in-canopy part
!                  Write(logdev,*) 'Before call rdndcj'
                  If( mosaic .OR. abflux ) Then
                     DO j=1,n_lufrac
                        IF (lufraccr( j ) > 0.0 ) THEN
                           hcanj ( j ) = Z0j( j,c,r ) * 10.0
                           rincj( j,c,r )  = 14.0 * laij( j,c,r ) * hcanj( j )
     &                                     / ustarj( j,c,r )
                           rgndcj( j,c,r ) = 1.0 / ( ( 1.0 - deltaj( j,c,r ) )
     &                                     / rgnd + deltaj( j,c,r ) / rgw )
     &                                     + rincj( j,c,r )      ! Add in-canopy part
                        END IF
                     END DO
                  EndIf
                  
         ! Determine the snow liquid water mass fraction (0.0 to 0.5).

                  xm = 0.02 * ( temp2p0cr - ( stdtemp - 1.0 ) )**2
                  xm = MIN (xm, 0.5)
                  xm = MAX (xm, 0.0)
                  IF ( temp2p0cr < ( stdtemp - 1.0 ) ) xm = 0.0                  

         ! Bulk stomatal resistance; include mesophyll resistance.

                  heff_ap = hlconst( subname( l ), temp2p0cr, effective, hplus_ap )

                  rstom = rs( c,r ) * dwat / dif0( l )
     &                  + 1.0 / ( heff_ap / 3000.0 + 100.0 * meso( l ) ) / laicr

! Recalculate stomal resistance for each land use category
!                  Write(logdev,*) 'Before calc rstomj'
                  IF ( mosaic .OR. abflux ) THEN
                     DO j=1,n_lufraC
                        IF (lufraccr( j ) > 0.0 ) THEN
                           if ((Land_scheme .eq. 'USGS24' .and. j .ne. 16) .or. 
     &                         (Land_scheme .eq. 'NLCD-MODIS' .and. j.ne. 1 .and. j 
     &                          .ne. 31 .and. j.ne. 48) .or. 
     &                         (Land_scheme .eq. 'MODIS' .and. j .ne. 17 )) then
                              IF ( laij( j,c,r ) .LT. 0.00001 ) THEN
                                 rstomj( j,l,c,r ) = rsmax
                              ELSE
!-RADIATION
                                 IF ( rsmin( j ) .GT. 130.0 ) THEN
                                    radl = 30.0                       ! W/M2
                                 ELSE
                                    radl = 100.0                      ! W/M2
                                 END IF
                                 radf = 1.1 * rgrnd( c,r ) / ( radl * laij( j,c,r ) )     ! NP89 - EQN34
                                 f1   = ( rsmin( j ) / rsmax + radf ) / ( 1.0 + radf )

!-SOIL MOISTURE
                                 w2avail = w2( c,r ) - wwlt( soilcat )
                                 w2mxav  = wfc( soilcat ) - wwlt( soilcat )
                                 f2      = 1.0 / ( 1.0 + EXP( -5.0 * ( w2avail / w2mxav -
     &                                   ( w2mxav / 3.0 + wwlt( soilcat ) ) ) ) )    ! according JP, 9/94

!-AIR TEMP
!... according to Avissar (1985) and AX 7/95

                                 IF ( ta( c,r ) .LE. 302.15 ) THEN
                                    f4 = 1.0 / ( 1.0 + EXP( -0.41 * (ta( c,r ) - 282.05 ) ) )
                                 ELSE
                                    f4 = 1.0 / ( 1.0 + EXP( 0.5 * (ta( c,r ) - 314.0 ) ) )
                                 END IF

                                 ftot = laij( j,c,r ) * f1 * f2 * f4
                                 ftot = MAX( ftot,ftmin )
                                 fshelt = 1.0   ! go back to NP89
                                 gs     = ftot / ( rsmin( j ) * fshelt )
                                 raw = raj( j,c,r ) + 4.503 / ustarj( j,c,r )
                                 ga     = 1.0 / raw

!-- Compute humidity effect according to RH at leaf surf
                                 f3 = 0.5 * ( gs - ga + SQRT( ga * ga + ga * gs
     &                                    * ( 4.0 * qv( c,r ) / qss_grnd - 2.0 ) + gs * gs ) ) / gs
                                 f3 = MIN ( MAX( f3, f3min ), 1.0 )
                                 rsj = 1.0 / ( gs * f3 )
                                 rstomj( j,l,c,r ) = rsj * dwat / dif0( l )
     &                               + 1.0 / ( heff_ap / 3000.0 + 100.0 * meso( l ) ) / laij( j,c,r )
                              END IF
                           END IF
                        END IF
                     END DO   ! n_lufrac
                  END IF   ! mosaic                  

         ! Bulk surface resistance.

                  rci = vegcr
     &                * ( 1.0/rstom + (1.0-delta( c,r )) * laicr / rcut
     &                +   ( delta( c,r ) * laicr / rwetsfc ) + 1.0 / rgndc )
     &                + (1-ifsnow) * ( (1.0 - vegcr) * ( (1.0-delta( c,r )) /
     &                                  rgnd + delta( c,r ) / rgw ) )
     &                + ifsnow * ( (1.0 - xm) / rsnow + xm / (rsndiff + rgw) )

                  rsurf = 1.0 / rci
                  
!                  Write(logdev,*) 'Before call rsurfj'
                  IF ( mosaic ) THEN
                     DO j = 1, n_lufrac

                        IF ( lufraccr( j ) .GT. 0.0 ) THEN
!                          IF ( j .eq. iwat ) THEN ! water -- code duplicate from above as it
                                              ! would only have been calculated above if it is a water cell
                           IF ( ( Land_scheme .EQ. 'USGS24' .AND. J .EQ. 16 ) .OR.
     &                          ( Land_scheme .EQ. 'NLCD-MODIS' .AND. J .EQ. 1 .OR.
     &                            J .EQ. 31 .OR. J.EQ. 48 ).OR.
     &                          ( Land_scheme .EQ. 'MODIS' .AND. J .EQ. 17 ) ) THEN   ! water
                              ctemp2 = temp2p0cr - stdtemp
                              lv     = lv0 - dlvdt * ctemp2
                              cp_air = 1004.67 * ( 1.0 + 0.84 * q2p0cr )               ! [J/kg/K]
                              tw     = ( ( 4.71E4 * cp_air / lv ) - 0.870 ) + stdtemp  ! [K]
                              IF ( TRIM( depspc( l ) ) .EQ. 'HG' ) THEN  ! elemental mercury gas
                                 rsurfj( j,l,c,r ) = 1.0E30
                              ELSE
                                 heff  = hlconst( subname( l ), tw, effective, hplus_h2o )
                                 heff  = heff * 0.08205 * tw
! from Hayduk and Laudie
                                 dw25 = 13.26e-5 / ( 0.8904**1.14 * lebas( l )**0.589 )
                                 kvisw = 0.017 * EXP( -0.025 * ( tw - stdtemp ) )
                                 dw    = dw25 * ( tw * rt25inK ) * ( 0.009025 / kvisw )
                                 scw_pr_23 = ( ( kvisw / dw ) / pr ) ** twothirds                                                                                           
                                 rgwwat   = 1.0 / ( heff * d3 * ustarj( j,c,r ) )
                                 rsurfj( j,l,c,r ) = rgwwat
                              END IF

                           ELSE  ! land category
                              rcij = vegj( j,c,r )
     &                             * ( 1.0 / rstomj( j,l,c,r ) + ( 1.0 - deltaj( j,c,r ) )
     &                             * laij( j,c,r ) / rcut
     &                             + ( deltaj( j,c,r ) * laij( j,c,r ) / rwetsfc )
     &                             + 1.0 / rgndcj( j,c,r ) )
     &                             + ( 1 - ifsnow) * ( ( 1.0 - vegj( j,c,r ))
     &                                  * ( ( 1.0 - deltaj( j,c,r ) ) / rgnd
     &                             +                deltaj( j,c,r ) / rgw ) )
     &                             + ifsnow * ( ( 1.0 - xm) / rsnow + xm / ( rsndiff + rgw ) )
                              rsurfj(j,l,c,r) = 1.0 / rcij
                           END IF
                        END IF
                     END DO
                  END IF ! mosaic

               END IF   ! land or water cell

         ! Compute dry deposition velocity.

               rbc = 5.0 / ustarcr * scc_pr_23( l )
               rac = ra( c,r ) + rbc

               depvel_gas( n,c,r ) = 1.0 / ( rsurf + rac )
               
!               Write(logdev,*) 'Before calc devel_gasj'
               IF ( mosaic ) THEN
                  DO j = 1, n_lufrac 
                     IF ( lufraccr( j ) .GT. 0.0 ) THEN
                        
                        rbcj(j) = 5.0 / ustarj( j,c,r ) * scc_pr_23( l )
                        racj = raj( j,c,r ) + rbcj(j)
                        depvel_gasj( j,l,c,r) = 1.0 / ( rsurfj( j,l,c,r ) + racj )
                        IF ( depvel_gasj( j,l,c,r ) .LT. 0.0 .OR.
     &                       depvel_gasj( j,l,c,r ) .NE. depvel_gasj( j,l,c,r ) ) THEN
                           WRITE( logdev,9033 ) c, r, j, TRIM( subname( l ) ),
     &                                          depvel_gasj( j,l,c,r ), laij( j,c,r ),
     &                                          z0j( j,c,r ), ustarj( j,c,r ), rgndcj( j,c,r ),
     &                                          raj( j,c,r ), rbcj(j), rsurfj( j,l,c,r ),
     &                                          rstomj( j,l,c,r ), rcut, rwetsfc, rgnd, rgw,
     &                                          deltaj( j,c,r ), vegj(j,c,r), xm, rsndiff, ifsnow
                           xmsg = 'ABORT'
                           CALL m3exit( pname, jdate, jtime, xmsg, xstat1 )
                        END IF
                        IF ( fst ) THEN
                           IF ((( Land_scheme .EQ. 'USGS24' .AND. J .EQ. 16 )   .OR.
     &                          ( Land_scheme .EQ. 'NLCD-MODIS' .AND. J .EQ. 1  .OR.
     &                            J .EQ. 31 .OR. J.EQ. 48 )                     .OR.
     &                          ( Land_scheme .EQ. 'MODIS' .AND. J .EQ. 17 ) )  .OR. 
     &                          ( lwmask( c,r ) .EQ. 0.0  ) ) THEN                         ! water
                              vd_fst_gasj( j,l,c,r) = 0.0
                           ELSE
                              vd_fst_gasj(j,l,c,r) = (1.0/(racj+rsurfj(j,l,c,r)))*(rsurfj(j,l,c,r)/rstomj(j,l,c,r))

                              IF  ( vd_fst_gasj( j,l,c,r ) > depvel_gasj( j,l,c,r ) ) THEN

#ifdef Verbose
                                 write( logdev,* ) 'vd_fst > vd_mos at ', jdate, jtime
                                 write( logdev,* ) 'vd_fst, vd_mos: ', vd_fst_gasj( j,l,c,r ),
     &                                                                 depvel_gasj( j,l,c,r )
                                 write( logdev,* ) ' '
                                 WRITE( logdev,9034 ) c, r, j, TRIM( subname( l ) ),
     &                                                vd_fst_gasj( j,l,c,r ),racj,rcanj(j,l,c,r),rstomj(j,l,c,r)
#endif
                                 vd_fst_gasj( j,l,c,r ) = depvel_gasj( j,l,c,r ) ! best we can do with limitations of methid
                              END IF              
                           END IF
                        END IF   ! fst
                     END IF
                  END DO
               END IF  ! mosaic
C--------------------------------------------------------------------------               
               IF ( abflux ) THEN   ! Ammonia Bidirectional Flux

                  IF ( TRIM( depspc( l ) ) == 'NH3' ) THEN
                     cnh3  = cgridl1( n,c,r )

!                     Write(logdev,*) 'Before call Get_flux'
                     IF ( NINT( lwmask( c,r ) ) /= 0 ) THEN  ! land
                        Call Get_Flux(tempgcr,rh_air,cnh3,rwetsfc,rgw,wg(c,r),w2(c,r),
     &                                sltyp(c,r),dif0(l),r,c,l,tpvd,lnh3,
     &                                f_stom,f_cut,f_soil,f_emis,f_dep,f_ag,f_nat, f_wat,
     &                                dt(2))
                        pvd( n,c,r ) = tpvd
                        depvel_gas( n,c,r ) = lnh3
                        
                        
                        cmp(1,c,r) = f_emis
                        cmp(2,c,r) = f_dep
                        cmp(3,c,r) = f_stom
                        cmp(4,c,r) = f_cut
                        cmp(5,c,r) = f_soil
                        cmp(6,c,r) = f_ag
                        cmp(7,c,r) = f_nat
                        cmp(8,c,r) = f_wat
                        
                     Else   ! water
                        cmp(8,c,r) = depvel_gas( n,c,r )* cnh3
                     End If
                  END IF   ! 'NH3'

               END IF   ! abflux

C--------------------------------------------------------------------------
               IF ( sfc_hono ) THEN

C HONO production via heterogeneous reaction on ground surfaces,
C 2NO2 = HONO + HNO3
C Rate constant for the reaction = (3.0E-3/60)* (A/V),
C where A/V is surface area/volume ratio
C HONO is produced and released into the atmosphere
C NO2 is lost via chemical reaction
C HNO3 is sticky and stays on the surfaces

C Calculate A/V for leaves.
C LAI was multiplied by 2 to account for the fact that surface area
C is provided by both sides of the leaves.
C Matthews Jones, Ammonia deposition to semi-natural vegetation,
C PhD dissertation, University of Dundee, Scotland, 2006

                  surf_leaf = 2.0 * laicr / zf( c,r )

C Calculate A/V for buildings and other structures.
C Buildings and other structures can provide additional surfaces in
C urban areas for the heterogeneous reaction to occur. However, such
C information is not readily available; in the absence of such information,
C it is scaled to purb(c,r). Svensson et al., (1987) suggests a typical value
C of 0.2 for A/V for buildings in urban environments. A maximum value of 0.2
C for A/V for buildings is assigned to the grid cell containing the highest
C purb(c,r) i.e., 100.0. A/V for buildings for other grid-cell is calculated
C as purb(c,r)*(0.2/100.0); Cai et al. (2006) used a value of 1.0 for their
C study at New York (total A/V)

                  surf_bldg = purb( c,r ) * 0.002

C Calculate rate constant for the reaction (psudeo-first order reaction,
C unit per second). Calculate pseudo-first order rate constant using Eq 1
C of Vogel et al. (2003).  Unit of KNO2 is in 1/min in the paper; divide it
C by 60 to convert it into 1/sec.

!                 kno2 = MAX( 0.0, 3.0E-3 * (surf_leaf + surf_bldg) / 60.0 )
                  kno2 = MAX( 0.0, 5.0E-5 * (surf_leaf + surf_bldg) )

C Determine NO2 concentration needed for HONO production term.

                  IF ( TRIM( depspc( l ) ) == 'NO2' ) THEN
                     conc_no2 = cgridl1( n,c,r )

C Loss of NO2 via the heterogeneous reaction is accounted as additional
C depositional loss. Add the loss of NO2 via the heterogeneous reaction
C to the regular deposition velocity (increased dep. vel.).  This will
C reduce the NO2 conc. in the atmosphere. Dep vel is adjusted back to the
C original value in vdiffacm2 after NO2 conc is reduced but before calculating
C depositional loss.

!                     depvel_gas( n,c,r ) = depvel_gas( n,c,r ) + 2.0 * kno2 * zf( c,r )

                  END IF

C Calculate production (pvd) for HONO; unit = ppm * m/s

                  IF ( TRIM( depspc( l ) ) == 'HONO' )
     &               pvd( n,c,r ) = kno2 * conc_no2 * zf( c,r )

               END IF
C--------------------------------------------------------------------------

               ! Check for negative values or NaN's

               IF ( depvel_gas( n,c,r ) < 0.0 .OR.
     &              depvel_gas( n,c,r ) /= depvel_gas( n,c,r ) ) GO TO 999

            END IF   ! special condition for sulfate (SULF)

         END DO ddloop                  
      
      END DO   ! c
      END DO   ! r                  

      RETURN

C-------------------------------------------------------------------------------
C Error-handling section.
C-------------------------------------------------------------------------------

999   CONTINUE
      WRITE( logdev,9003 ) c, r, TRIM( subname( l ) ), depvel_gas( n,c,r )
      xmsg = 'ABORT'

1001  CONTINUE
      CALL m3exit( pname, jdate, jtime, xmsg, xstat1 )

      RETURN

C-------------------------------------------------------------------------------
C Format statements.
C-------------------------------------------------------------------------------

6501  FORMAT(/ 1x, 70('='),
     &       / 1x, '--- Subroutine: M3DRY',
     &       / 1x, '---   Found canopy wetness (WR) in MET_CRO_2D ',
     &       / 1x, 70('=') /)

6503  FORMAT(/ 1x, 70('='),
     &       / 1x, '--- Subroutine: M3DRY',
     &       / 1x, '---   Found 2-m water vapor mixing ratio (Q2) in MET_CRO_2D',
     &       / 1x, 70('=') /)

9001  FORMAT( 'Failure reading ', a, 1x, 'from ', a )

9003  FORMAT(/ 1x, 70('*'),
     &       / 1x, '*** Subroutine: M3DRY',
     &       / 1x, '***   NEGATIVE or UNDEFINED Dry Deposition Velocity',
     &       / 1x, '***   Point   = ', 2i5,
     &       / 1x, '***   Species = ', a,
     &       / 1x, '***   Vd      = ', e13.6,
     &       / 1x, 70('*') /)

9005  FORMAT(/ 1x, 70('*'),
     &       / 1x, '*** Subroutine: M3DRY',
     &       / 1x, '***   Bidirectional flux for ammonia assumes ',
     &       / 1x, '***   USGS 24-category land use.',
     &       / 1x, '***   Need to update N_LUFRAC, LUF_FAC, and other places',
     &       / 1x, '***   Land use description is: ', a,
     &       / 1x, 70('*') /)
9033  FORMAT(/ 1x, 70('*'),
     &       / 1x, '*** Subroutine: M3DRY',
     &       / 1x, '***   NEGATIVE or UNDEFINED Land use specific Dry Deposition Velocity',
     &       / 1x, '***   Column  = ', i5,
     &       / 1x, '***   Row     = ', i5,
     &       / 1x, '***   Luc     = ', i5,
     &       / 1x, '***   Species = ', a,
     &       / 1x, '***   Vd      = ', e13.6,
     &       / 1x, '***   lai     = ', g10.3,
     &       / 1x, '***   z0      = ', g10.3,
     &       / 1x, '***   ustar   = ', g10.3,
     &       / 1x, '***   rgndc   = ', g10.3,
     &       / 1x, '***   Ra      = ', g10.3,
     &       / 1x, '***   Rbc     = ', g10.3,
     &       / 1x, '***   Rsurf   = ', g10.3,
     &       / 1x, '***   Rstom   = ', g10.3,
     &       / 1x, '***   Rcut    = ', g10.3,
     &       / 1x, '***   Rwetsfc = ', g10.3,
     &       / 1x, '***   Rgnd    = ', g10.3,
     &       / 1x, '***   Rgw     = ', g10.3,
     &       / 1x, '***   Delta   = ', g10.3,
     &       / 1x, '***   Veg     = ', g10.3,
     &       / 1x, '***   Xm      = ', g10.3,
     &       / 1x, '***   Rsndiff = ', g10.3,
     &       / 1x, '***   ifsnow  = ', i6
     &       / 1x, 70('*') /)

9034  FORMAT(/ 1x, 70('*'),
     &       / 1x, '*** Subroutine: M3DRY',
     &       / 1x, '***   NEGATIVE or UNDEFINED Land use specific Dry Deposition Velocity',
     &       / 1x, '***   Column  = ', i5,
     &       / 1x, '***   Row     = ', i5,
     &       / 1x, '***   Luc     = ', i5,
     &       / 1x, '***   Species = ', a,
     &       / 1x, '***   Vdfstj  = ', e13.6,
     &       / 1x, '***   Rcanj    = ', e13.6,
     &       / 1x, '***   Rstcj   = ', e13.6,
     &       / 1x, '***   Rstomj  = ', e13.6,
     &       / 1x, 70('*') /)
     

      END SUBROUTINE m3dry
