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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module ASX_DATA_MOD

C-----------------------------------------------------------------------
C Function: User-defined types

C Revision History:
C     19 Aug 2014 J.Bash: initial implementation
C     17 July 2015 H.Foroutan: Updated the calculation of MOL, MOLI, HOL, and WSTAR
C     25 Aug 2015 H. Pye: Added IEPOX, HACET surrogates
C                         modified PROPNN and H2O2
C                         Increased ar for ozone from 8 to 12.
C                         Change meso from 0.1 to 0 for some org. nitrates
C                         Changes based on Nguyen et al. 2015 PNAS and SOAS
C     07 May 2018 D. Schwede: Added call to CZANGLE here and removed call 
C                         to CZANGLE in other modules
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
C %% G. Sarwar: added data for iodine and bromine species (03/2016)
C %% B. Hutzell: added dry deposition data for methane, acrylic acid, methyl chloride,
C    and acetonitrile (09/2016)  
C    G. Sarwar: added ClNO3 and FMBR, and updated INO3 and BRNO3 name changes (07/2018)
C    G. Sarwar: removed NACL (07/2018)
C    G. Sarwar: made minor changes to halogen species and added several iodine species (12/2018)
C    D. Wong: Implemented centralized I/O approach, removed all MY_N clauses, removed
C             unnecessary SAVE attribute (02/2019)
C    G. Sarwar: Removed CH3BR (03/2021)
C    R. Gilliam: Include PX soil texture information when available (03/2022)
C-------------------------------------------------------------------------------
      Use RUNTIME_VARS
      Use GRID_CONF           ! horizontal & vertical domain specifications
      Use LSM_MOD             ! Land surface data
      Use DEPVVARS, Only: ltotg
      Use CENTRALIZED_IO_MODULE
#ifdef mpas
      use coupler_module
#endif

      Implicit None

      Include SUBST_CONST     ! constants

      Type :: MET_Type
!> 2-D meteorological fields:
         Real,    Allocatable :: RDEPVHT  ( :,: )  ! reciprocal layer 1 height [m-1]
         Real,    Allocatable :: DENS1    ( :,: )  ! layer 1 air density
         Real,    Allocatable :: PRSFC    ( :,: )  ! surface pressure [Pa]
         Real,    Allocatable :: Q2       ( :,: )  ! 2 meter water vapor mixing ratio [kg/kg]
         Real,    Allocatable :: QSS_GRND ( :,: )  ! ground saturation water vapor mixing ratio [kg/kg]
         Real,    Allocatable :: RH2      ( :,: )  ! relative humidity [percent]
         Real,    Allocatable :: RA       ( :,: )  ! aerodynamic resistance [s/m]
         Real,    Allocatable :: RS       ( :,: )  ! stomatal resistance [s/m]
         Real,    Allocatable :: RC       ( :,: )  ! convective precipitation [cm]
         Real,    Allocatable :: RN       ( :,: )  ! non-convective precipitation [cm]
         Real,    Allocatable :: RGRND    ( :,: )  ! Solar radiation at the ground [W/m**2]
         Real,    Allocatable :: HFX      ( :,: )  ! Sensible heat flux [W/m**2]
         Real,    Allocatable :: LH       ( :,: )  ! Latent heat flux [W/m**2]
         Real,    Allocatable :: SNOCOV   ( :,: )  ! Snow cover [1=yes, 0=no]
         Real,    Allocatable :: TEMP2    ( :,: )  ! two meter temperature [K]
         Real,    Allocatable :: TEMPG    ( :,: )  ! skin temperature [K]
         Real,    Allocatable :: TSEASFC  ( :,: )  ! SST [K]
         Real,    Allocatable :: USTAR    ( :,: )  ! surface friction velocity [m/s]
         Real,    Allocatable :: VEG      ( :,: )  ! fractional vegetation coverage [ratio]
         Real,    Allocatable :: LAI      ( :,: )  ! grid cell leaf area index [m**2/m**2]
         Real,    Allocatable :: WR       ( :,: )  ! precip intercepted by canopy [m]
         Real,    Allocatable :: WSPD10   ( :,: )  ! 10-m wind speed [m/s]
         Real,    Allocatable :: WSTAR    ( :,: )  ! convective velocity scale [m/s]
         Real,    Allocatable :: Z0       ( :,: )  ! roughness length [m]
         Real,    Allocatable :: SOIM1    ( :,: )  ! 1 cm soil moisture [m**3/m**3]
         Real,    Allocatable :: SOIM2    ( :,: )  ! 1 m soil moisture  [m**3/m**3]
         Real,    Allocatable :: CLDFRAC  ( :,: )  ! Total cloud fraction including cu
         Real,    Allocatable :: SOIT1    ( :,: )  ! 1 cm soil temperature [K]
         Real,    Allocatable :: SOIT2    ( :,: )  ! 1 m soil temperature [K]
         Real,    Allocatable :: SEAICE   ( :,: )  ! Sea ice coverage [%]
         Real,    Allocatable :: MOL      ( :,: )  ! Monin-Obukhov length [m]
         Real,    Allocatable :: MOLI     ( :,: )  ! inverse of Monin-Obukhov length [m]
         Real,    Allocatable :: HOL      ( :,: )  ! PBL over Obukhov length
         Real,    Allocatable :: XPBL     ( :,: )  ! PBL sigma height
         Integer, Allocatable :: LPBL     ( :,: )  ! PBL layer
         Logical, Allocatable :: CONVCT   ( :,: )  ! convection flag
         Real,    Allocatable :: PBL      ( :,: )  ! pbl height (m)
!         Real,    Allocatable :: NACL_EMIS( :,: )  ! NACL mass emission rate of particles with d <10 um (g/m2/s)
         Real,    Allocatable :: COSZEN   ( :,: )  ! Cosine of the zenith angle
         Real,    Allocatable :: CFRAC    ( :,: )  ! cloud fraction
!> U and V wind components on the cross grid points
         Real,    Allocatable :: UWIND    ( :,:,: )  ! [m/s]
         Real,    Allocatable :: VWIND    ( :,:,: )  ! [m/s]                    
!> 3-D meteorological fields:
         Real,    Allocatable :: KZMIN    ( :,:,: )  ! minimum Kz [m**2/s]
         Real,    Allocatable :: PRES     ( :,:,: )  ! pressure [Pa]
         Real,    Allocatable :: QV       ( :,:,: )  ! water vapor mixing ratio
         Real,    Allocatable :: QC       ( :,:,: )  ! cloud water mixing ratio
         Real,    Allocatable :: THETAV   ( :,:,: )  ! potential temp
         Real,    Allocatable :: TA       ( :,:,: )  ! temperature (K)
         Real,    Allocatable :: RH       ( :,:,: )  ! relative humidity [ratio]
         Real,    Allocatable :: ZH       ( :,:,: )  ! mid-layer height above ground [m]
         Real,    Allocatable :: ZF       ( :,:,: )  ! layer height [m]
         Real,    Allocatable :: DZF      ( :,:,: )  ! layer thickness
         Real,    Allocatable :: DENS     ( :,:,: )  ! air density
         Real,    Allocatable :: RJACM    ( :,:,: )  ! reciprocal mid-layer Jacobian
         Real,    Allocatable :: RJACF    ( :,:,: )  ! reciprocal full-layer Jacobian
         Real,    Allocatable :: RRHOJ    ( :,:,: )  ! reciprocal density X Jacobian
      End Type MET_Type
      
      Type :: GRID_Type
!> Grid infomation:
!> Vertical information
         Real,    Allocatable :: DX3F   ( : )     ! sigma layer surface thickness ! vdiffacmx.F
         Real,    Allocatable :: RDX3F  ( : )     ! reciprocal sigma layer thickness ! EMIS_DEFN.F, sedi.F, vdiffacmx.F, vdiffproc.F
         Real,    Allocatable :: RDX3M  ( : )     ! reciprocal sigma midlayer thickness ! vdiffproc.F
!> Horizontal Information:
         Real,    Allocatable :: RMSFX4 ( :,: )  ! inverse map scale factor ** 4
         Real,    Allocatable :: LON    ( :,: )  ! longitude
         Real,    Allocatable :: LAT    ( :,: )  ! latitude
         Real,    Allocatable :: LWMASK ( :,: )  ! land water mask
         Real,    Allocatable :: OCEAN  ( :,: )  ! Open ocean fraction
         Real,    Allocatable :: SZONE  ( :,: )  ! Surf zone fraction
         Real,    Allocatable :: PURB   ( :,: )  ! percent urban [%]
         Integer, Allocatable :: SLTYP  ( :,: )  ! soil type [category]
         Real,    Allocatable :: WSAT   ( :,: )  ! volumetric soil saturation concentration
         Real,    Allocatable :: WWLT   ( :,: )  ! soil wilting point
         Real,    Allocatable :: BSLP   ( :,: )  ! B Slope 
         Real,    Allocatable :: WRES   ( :,: )  ! Soil residual moisture point
         Real,    Allocatable :: WFC    ( :,: )  ! soil field capacity 
         Real,    Allocatable :: CLAY_PX  ( :,: )  ! Clay fraction from PX LSM 
         Real,    Allocatable :: CSAND_PX ( :,: )  ! Coarse sand fraction from PX LSM
         Real,    Allocatable :: FMSAND_PX( :,: )  ! Fine-medium sand fraction from PX LSM
!        Real,    Allocatable :: RHOB   ( :,: )  ! soil bulk density
         Real,    Allocatable :: LUFRAC  ( :,:,: ) ! land use fraction (col,row,lu_type)[ratio]
C Land use information:
         Character( 16 ), Allocatable   :: NAME    ( : )     ! LU name
         Character( 16 ), Allocatable   :: LU_Type ( : )     ! general land use type e.g. water, forest, etc.
      End Type GRID_Type


      Type( MET_Type )    :: Met_Data 
      Type( GRID_Type )   :: Grid_Data 

      Integer, Save   :: n_spc_m3dry = ltotg       ! from DEPVVARS module
!> M3 asx constants
      Real, Parameter :: a0         = 8.0        ! [dim'less]
      Real, Parameter :: d3         = 1.38564e-2 ! [dim'less]
      Real, Parameter :: dwat       = 0.2178     ! [cm^2/s] at 273.15K
      Real, Parameter :: hplus_ap   = 1.0e-6     ! pH=6.0 leaf apoplast solution Ph (Massad et al 2008)      
      Real, Parameter :: hplus_def  = 1.0e-5     ! pH=5.0
      Real, Parameter :: hplus_east = 1.0e-5     ! pH=5.0
      Real, Parameter :: hplus_h2o  = 7.94328e-9 ! 10.0**(-8.1)
      Real, Parameter :: hplus_west = 3.16228e-6 ! 10.0**(-5.5)
      Real, Parameter :: kvis       = 0.132      ! [cm^2 / s] at 273.15K
      Real, Parameter :: pr         = 0.709      ! [dim'less]
      Real, Parameter :: rcut0      = 3000.0     ! [s/m]
      Real, Parameter :: rcw0       = 125000.0   ! acc'd'g to Padro and
      Real, Parameter :: resist_max = 1.0e30     ! maximum resistance
      Real, Parameter :: rg0        = 1000.0     ! [s/m]
      Real, Parameter :: rgwet0     = 25000.0    ! [s/m]
      Real, Parameter :: rsndiff    = 10.0       ! snow diffusivity fac
      Real, Parameter :: rsnow0     = 10000.0    !  Changed from 1000 to 10000 - Helmig et al 2012
      Real, Parameter :: svp2       = 17.67      ! from MM5 and WRF
      Real, Parameter :: svp3       = 29.65      ! from MM5 and WRF
      Real, Parameter :: rt25inK    = 1.0/(stdtemp + 25.0) ! 298.15K = 25C
      Real, Parameter :: twothirds  = 2.0 / 3.0
      Real, Parameter :: betah      = 5.0       ! WRF 3.6 px uses Dyer
      Real, Parameter :: gamah      = 16.0
      Real, Parameter :: pr0        = 0.95
      Real, Parameter :: karman     = 0.40
      Real, Parameter :: f3min      = 0.25
      Real, Parameter :: ftmin      = 0.0000001  ! m/s
      Real, Parameter :: nscat      = 16.0
      Real, Parameter :: rsmax      = 5000.0     ! s/m

      Real            :: ar       ( ltotg )        ! reactivity relative to HNO3
      Real            :: dif0     ( ltotg )        ! molecular diffusivity [cm2/s]
      Real            :: lebas    ( ltotg )        ! Le Bas molar volume [cm3/mol ]
      Real            :: meso     ( ltotg )        ! Exception for species that 
                                                   ! react with cell walls. fo in 
                                                   ! Wesely 1989 eq 6.
      Character( 16 ) :: subname  ( ltotg )        ! for subroutine HLCONST

      Logical, Save :: MET_INITIALIZED = .false.
      Logical, Save :: CSTAGUV            ! Winds are available with C stagger?
!      Logical, Save :: ifwr     = .false.

      Public        :: INIT_MET, GET_MET

      Integer,         Private :: C, R, L, S               ! loop induction variables
      Integer,         Private :: SPC
      Character( 16 ), Private, Save :: vname_rc, vname_rn, vname_uc, vname_vc

      Real, allocatable, private :: BUFF1D( : )     ! 1D temp var number of layers
      Real, allocatable, private :: BUFF2D( :,: )   ! 2D temp var
      Real, allocatable, private :: BUFF3D( :,:,: ) ! 3D temp var


      DATA subname(  1), dif0(  1), ar(  1), meso(  1), lebas(  1) / 'SO2             ', 0.1089,   10.0, 0.0,  35.0 /
      DATA subname(  2), dif0(  2), ar(  2), meso(  2), lebas(  2) / 'H2SO4           ', 0.1091, 8000.0, 0.0,  49.0 /
      DATA subname(  3), dif0(  3), ar(  3), meso(  3), lebas(  3) / 'NO2             ', 0.1361,    2.0, 0.1,  21.0 /
      DATA subname(  4), dif0(  4), ar(  4), meso(  4), lebas(  4) / 'NO              ', 0.1802,    2.0, 0.0,  14.0 /
      DATA subname(  5), dif0(  5), ar(  5), meso(  5), lebas(  5) / 'O3              ', 0.1444,   12.0, 1.0,  21.0 /
      DATA subname(  6), dif0(  6), ar(  6), meso(  6), lebas(  6) / 'HNO3            ', 0.1067, 8000.0, 0.0,  35.0 /
      DATA subname(  7), dif0(  7), ar(  7), meso(  7), lebas(  7) / 'H2O2            ', 0.1300,34000.0, 1.0,  28.0 /   !ar=34,000 such that r_cut=0.7 s/m as in Nguyen et al. 2015
      DATA subname(  8), dif0(  8), ar(  8), meso(  8), lebas(  8) / 'ACETALDEHYDE    ', 0.1111,   10.0, 0.0,  56.0 /
      DATA subname(  9), dif0(  9), ar(  9), meso(  9), lebas(  9) / 'FORMALDEHYDE    ', 0.1554,   10.0, 0.0,  35.0 /
      DATA subname( 10), dif0( 10), ar( 10), meso( 10), lebas( 10) / 'METHYLHYDROPEROX', 0.1179,   10.0, 0.3,  49.0 /   !meso change from 0.1 to 0.3, Wolfe and Thornton 2011 ACP per J. Bash
      DATA subname( 11), dif0( 11), ar( 11), meso( 11), lebas( 11) / 'PEROXYACETIC_ACI', 0.0868,   20.0, 0.1,  70.0 /
      DATA subname( 12), dif0( 12), ar( 12), meso( 12), lebas( 12) / 'ACETIC_ACID     ', 0.0944,   20.0, 0.0,  63.0 /
      DATA subname( 13), dif0( 13), ar( 13), meso( 13), lebas( 13) / 'NH3             ', 0.1978,   20.0, 0.0,  28.0 /
      DATA subname( 14), dif0( 14), ar( 14), meso( 14), lebas( 14) / 'PAN             ', 0.0687,   16.0, 0.1,  91.0 /
      DATA subname( 15), dif0( 15), ar( 15), meso( 15), lebas( 15) / 'HNO2            ', 0.1349,   20.0, 0.1,  28.0 /
      DATA subname( 16), dif0( 16), ar( 16), meso( 16), lebas( 16) / 'CO              ', 0.1807,    5.0, 0.0,  14.0 /
      DATA subname( 17), dif0( 17), ar( 17), meso( 17), lebas( 17) / 'METHANOL        ', 0.1329,    2.0, 0.0,  42.0 /
      DATA subname( 18), dif0( 18), ar( 18), meso( 18), lebas( 18) / 'N2O5            ', 0.0808, 5000.0, 0.0,  49.0 /
      DATA subname( 19), dif0( 19), ar( 19), meso( 19), lebas( 19) / 'NO3             ', 0.1153, 5000.0, 0.0,  28.0 /
      DATA subname( 20), dif0( 20), ar( 20), meso( 20), lebas( 20) / 'GENERIC_ALDEHYDE', 0.0916,   10.0, 0.0,  56.0 /
      DATA subname( 21), dif0( 21), ar( 21), meso( 21), lebas( 21) / 'CL2             ', 0.1080,   10.0, 0.0,  49.0 /
      DATA subname( 22), dif0( 22), ar( 22), meso( 22), lebas( 22) / 'HOCL            ', 0.1300,   10.0, 0.0,  38.5 / ! used value of HCL
      DATA subname( 23), dif0( 23), ar( 23), meso( 23), lebas( 23) / 'HCL             ', 0.1510, 8000.0, 0.0,  31.5 /
      DATA subname( 24), dif0( 24), ar( 24), meso( 24), lebas( 24) / 'FMCL            ', 0.1094,   10.0, 0.0,  45.5 / ! default value
      DATA subname( 25), dif0( 25), ar( 25), meso( 25), lebas( 25) / 'HG              ', 0.1194,    0.1, 0.0,  14.8 / ! lebas not used
      DATA subname( 26), dif0( 26), ar( 26), meso( 26), lebas( 26) / 'HGIIGAS         ', 0.0976, 8000.0, 0.0,  95.0 / ! estimation from back calculating to get dw25 = 1.04e-5 (Garland et al, 1965)
      DATA subname( 27), dif0( 27), ar( 27), meso( 27), lebas( 27) / 'TECDD_2378      ', 0.0525,    2.0, 0.0, 217.0 /
      DATA subname( 28), dif0( 28), ar( 28), meso( 28), lebas( 28) / 'PECDD_12378     ', 0.0508,    2.0, 0.0, 234.5 /
      DATA subname( 29), dif0( 29), ar( 29), meso( 29), lebas( 29) / 'HXCDD_123478    ', 0.0494,    2.0, 0.0, 252.0 /
      DATA subname( 30), dif0( 30), ar( 30), meso( 30), lebas( 30) / 'HXCDD_123678    ', 0.0494,    2.0, 0.0, 252.0 /
      DATA subname( 31), dif0( 31), ar( 31), meso( 31), lebas( 31) / 'HXCDD_123478    ', 0.0494,    2.0, 0.0, 252.0 /
      DATA subname( 32), dif0( 32), ar( 32), meso( 32), lebas( 32) / 'HPCDD_1234678   ', 0.0480,    2.0, 0.0, 269.5 /
      DATA subname( 33), dif0( 33), ar( 33), meso( 33), lebas( 33) / 'OTCDD           ', 0.0474,    2.0, 0.0, 287.0 /
      DATA subname( 34), dif0( 34), ar( 34), meso( 34), lebas( 34) / 'TECDF_2378      ', 0.0534,    2.0, 0.0, 210.0 /
      DATA subname( 35), dif0( 35), ar( 35), meso( 35), lebas( 35) / 'PECDF_12378     ', 0.0517,    2.0, 0.0, 227.5 /
      DATA subname( 36), dif0( 36), ar( 36), meso( 36), lebas( 36) / 'PECDF_23478     ', 0.0517,    2.0, 0.0, 227.5 /
      DATA subname( 37), dif0( 37), ar( 37), meso( 37), lebas( 37) / 'HXCDF_123478    ', 0.0512,    2.0, 0.0, 245.0 /
      DATA subname( 38), dif0( 38), ar( 38), meso( 38), lebas( 38) / 'HXCDF_123678    ', 0.0512,    2.0, 0.0, 245.0 /
      DATA subname( 39), dif0( 39), ar( 39), meso( 39), lebas( 39) / 'HXCDF_234678    ', 0.0512,    2.0, 0.0, 245.0 /
      DATA subname( 40), dif0( 40), ar( 40), meso( 40), lebas( 40) / 'HXCDF_123789    ', 0.0512,    2.0, 0.0, 245.0 /
      DATA subname( 41), dif0( 41), ar( 41), meso( 41), lebas( 41) / 'HPCDF_1234678   ', 0.0487,    2.0, 0.0, 262.5 /
      DATA subname( 42), dif0( 42), ar( 42), meso( 42), lebas( 42) / 'HPCDF_1234789   ', 0.0487,    2.0, 0.0, 262.5 /
      DATA subname( 43), dif0( 43), ar( 43), meso( 43), lebas( 43) / 'OTCDF           ', 0.0474,    2.0, 0.0, 280.0 /
      DATA subname( 44), dif0( 44), ar( 44), meso( 44), lebas( 44) / 'NAPHTHALENE     ', 0.0778,    4.0, 0.0, 119.0 / !Julin et al. 2014 doi:10.1021/es501816h 
      DATA subname( 45), dif0( 45), ar( 45), meso( 45), lebas( 45) / '1NITRONAPHTHALEN', 0.0692,    4.0, 0.0, 133.0 /
      DATA subname( 46), dif0( 46), ar( 46), meso( 46), lebas( 46) / '2NITRONAPHTHALEN', 0.0692,    4.0, 0.0, 133.0 /
      DATA subname( 47), dif0( 47), ar( 47), meso( 47), lebas( 47) / '14NAPHTHOQUINONE', 0.0780,    4.0, 0.0, 119.0 /
      DATA subname( 48), dif0( 48), ar( 48), meso( 48), lebas( 48) / 'HEXAMETHYLE_DIIS', 0.0380,   10.0, 0.0, 196.0 /
      DATA subname( 49), dif0( 49), ar( 49), meso( 49), lebas( 49) / 'HYDRAZINE       ', 0.4164,   20.0, 0.0,  42.0 /
      DATA subname( 50), dif0( 50), ar( 50), meso( 50), lebas( 50) / 'MALEIC_ANHYDRIDE', 0.0950,   10.0, 0.0,  70.0 /
      DATA subname( 51), dif0( 51), ar( 51), meso( 51), lebas( 51) / '24-TOLUENE_DIIS ', 0.0610,   10.0, 0.0, 154.0 /
      DATA subname( 52), dif0( 52), ar( 52), meso( 52), lebas( 52) / 'TRIETHYLAMINE   ', 0.0881,   20.0, 0.0, 154.0 /
      DATA subname( 53), dif0( 53), ar( 53), meso( 53), lebas( 53) / 'ORG_NTR         ', 0.0607,   16.0, 0.0, 160.0 /  ! assumes 58.2% C5H11O4N and 41.8% C5H11O3N
      DATA subname( 54), dif0( 54), ar( 54), meso( 54), lebas( 54) / 'HYDROXY_NITRATES', 0.0609,   16.0, 0.0, 156.1 /
      DATA subname( 55), dif0( 55), ar( 55), meso( 55), lebas( 55) / 'MPAN            ', 0.0580,   16.0, 0.1, 133.0 /
      DATA subname( 56), dif0( 56), ar( 56), meso( 56), lebas( 56) / 'PPN             ', 0.0631,   16.0, 0.1, 118.2 /
      DATA subname( 57), dif0( 57), ar( 57), meso( 57), lebas( 57) / 'METHACROLEIN    ', 0.0810,    8.0, 1.0,  88.8 /
      DATA subname( 58), dif0( 58), ar( 58), meso( 58), lebas( 58) / 'DINTR           ', 0.0617,   16.0, 0.1, 169.8 /
      DATA subname( 59), dif0( 59), ar( 59), meso( 59), lebas( 59) / 'NTR_ALK         ', 0.0688,   16.0, 0.1, 133.0 /
      DATA subname( 60), dif0( 60), ar( 60), meso( 60), lebas( 60) / 'NTR_OH          ', 0.0665,   16.0, 0.1, 140.4 /
      DATA subname( 61), dif0( 61), ar( 61), meso( 61), lebas( 61) / 'HYDROXY_NITRATES', 0.0646,   16.0, 0.0, 147.8 /
      DATA subname( 62), dif0( 62), ar( 62), meso( 62), lebas( 62) / 'PROPNN          ', 0.0677,   16.0, 0.0, 133.0 /
      DATA subname( 63), dif0( 63), ar( 63), meso( 63), lebas( 63) / 'NITRYL_CHLORIDE ', 0.0888,    8.0, 0.0,  45.5 /  ! dif0 estimated following Erickson III et al., JGR, 104, D7, 8347-8372, 1999
      DATA subname( 64), dif0( 64), ar( 64), meso( 64), lebas( 64) / 'ISOPNN          ',0.0457,  275.9, 0.0,  206.8 /
      DATA subname( 65), dif0( 65), ar( 65), meso( 65), lebas( 65) / 'MTNO3           ',0.0453,   96.2, 0.0,  251.2 /
      DATA subname( 66), dif0( 66), ar( 66), meso( 66), lebas( 66) / 'IEPOX           ',0.0579,    8.0, 0.0,  110.8 /
      DATA subname( 67), dif0( 67), ar( 67), meso( 67), lebas( 67) / 'HACET           ',0.1060,    8.0, 0.0,   72.6 /  ! dif0 from Nguyen 2015 PNAS
      DATA subname( 68), dif0( 68), ar( 68), meso( 68), lebas( 68) / 'SVALK1          ',0.0514, 4572.8, 0.0,  280.5 /  ! Pye et al. doi:10.5194/acp-17-343-2017; rel. reactivity per J. Bash
      DATA subname( 69), dif0( 69), ar( 69), meso( 69), lebas( 69) / 'SVALK2          ',0.0546,   12.9, 0.0,  275.6 /
      DATA subname( 70), dif0( 70), ar( 70), meso( 70), lebas( 70) / 'SVBNZ1          ',0.0642,20671.2, 0.0,  134.1 /
      DATA subname( 71), dif0( 71), ar( 71), meso( 71), lebas( 71) / 'SVBNZ2          ',0.0726,   52.5, 0.0,  127.5 /
      DATA subname( 72), dif0( 72), ar( 72), meso( 72), lebas( 72) / 'SVISO1          ',0.0733,   50.6, 0.0,  126.3 /
      DATA subname( 73), dif0( 73), ar( 73), meso( 73), lebas( 73) / 'SVISO2          ',0.0729,10009.0, 0.0,  123.8 /
      DATA subname( 74), dif0( 74), ar( 74), meso( 74), lebas( 74) / 'SVPAH1          ',0.0564,  772.1, 0.0,  235.7 /
      DATA subname( 75), dif0( 75), ar( 75), meso( 75), lebas( 75) / 'SVPAH2          ',0.0599,    4.8, 0.0,  231.5 /
      DATA subname( 76), dif0( 76), ar( 76), meso( 76), lebas( 76) / 'SVSQT           ',0.0451,   10.3, 0.0,  346.5 /
      DATA subname( 77), dif0( 77), ar( 77), meso( 77), lebas( 77) / 'SVTOL1          ',0.0637, 1928.3, 0.0,  153.7 /
      DATA subname( 78), dif0( 78), ar( 78), meso( 78), lebas( 78) / 'SVTOL2          ',0.0607,  111.6, 0.0,  194.1 /
      DATA subname( 79), dif0( 79), ar( 79), meso( 79), lebas( 79) / 'SVTRP1          ',0.0603,  160.1, 0.0,  194.9 /
      DATA subname( 80), dif0( 80), ar( 80), meso( 80), lebas( 80) / 'SVTRP2          ',0.0559,   13.0, 0.0,  218.8 /
      DATA subname( 81), dif0( 81), ar( 81), meso( 81), lebas( 81) / 'SVXYL1          ',0.0610, 3586.7, 0.0,  154.6 /
      DATA subname( 82), dif0( 82), ar( 82), meso( 82), lebas( 82) / 'SVXYL2          ',0.0585,   72.2, 0.0,  194.6 /
      DATA subname( 83), dif0( 83), ar( 83), meso( 83), lebas( 83) / 'IO              ',0.1004,    8.0, 0.0,   45.5 /
      DATA subname( 84), dif0( 84), ar( 84), meso( 84), lebas( 84) / 'OIO             ',0.0940,    8.0, 0.0,   52.5 /
      DATA subname( 85), dif0( 85), ar( 85), meso( 85), lebas( 85) / 'I2O2            ',0.0734,    8.0, 0.0,   91.0 /
      DATA subname( 86), dif0( 86), ar( 86), meso( 86), lebas( 86) / 'I2O3            ',0.0709,    8.0, 0.0,   98.0 /
      DATA subname( 87), dif0( 87), ar( 87), meso( 87), lebas( 87) / 'I2O4            ',0.0686,    8.0, 0.0,  105.0 /
      DATA subname( 88), dif0( 88), ar( 88), meso( 88), lebas( 88) / 'HI              ',0.1047,    8.0, 0.0,   45.5 /
      DATA subname( 89), dif0( 89), ar( 89), meso( 89), lebas( 89) / 'HOI             ',0.0974,    8.0, 0.0,   52.5 /
      DATA subname( 90), dif0( 90), ar( 90), meso( 90), lebas( 90) / 'INO             ',0.0885,    8.0, 0.0,   52.5 /
      DATA subname( 91), dif0( 91), ar( 91), meso( 91), lebas( 91) / 'INO2            ',0.0835,   20.0, 0.0,   59.5 /
      DATA subname( 92), dif0( 92), ar( 92), meso( 92), lebas( 92) / 'INO3            ',0.0794,    8.0, 0.0,   66.5 /
      DATA subname( 93), dif0( 93), ar( 93), meso( 93), lebas( 93) / 'BRO             ',0.1146,    1.0, 0.0,   38.5 /
      DATA subname( 94), dif0( 94), ar( 94), meso( 94), lebas( 94) / 'HOBR            ',0.1104,    1.0, 0.0,   45.5 /
      DATA subname( 95), dif0( 95), ar( 95), meso( 95), lebas( 95) / 'HBR             ',0.1219,    2.0, 0.0,   38.5 /
      DATA subname( 96), dif0( 96), ar( 96), meso( 96), lebas( 96) / 'BRNO3           ',0.0871,    1.0, 0.0,   59.5 /
      DATA subname( 97), dif0( 97), ar( 97), meso( 97), lebas( 97) / 'BRNO2           ',0.0922,    1.0, 0.0,   52.5 /
      DATA subname( 98), dif0( 98), ar( 98), meso( 98), lebas( 98) / 'BRCL            ',0.0968,    1.0, 0.0,   56.0 /
      DATA subname( 99), dif0( 99), ar( 99), meso( 99), lebas( 99) / 'DMS             ',0.0896,    2.0, 0.0,   77.0 /
      DATA subname(100), dif0(100), ar(100), meso(100), lebas(100) / 'MSA             ',0.0844,    2.0, 0.0,   77.0 /
      DATA subname(101), dif0(101), ar(101), meso(101), lebas(101) / 'METHANE         ',0.2107,    2.0, 0.0,   29.6 / ! dif0, eqn 9-22 Schwarzenbach Gschwend & Imboden (1993) Env Org Chem
      DATA subname(102), dif0(102), ar(102), meso(102), lebas(102) / 'ACRYACID        ',0.0908,    2.0, 0.0,   63.2 / 
      DATA subname(103), dif0(103), ar(103), meso(103), lebas(103) / 'CARBSULFIDE     ',0.1240,    5.0, 0.0,   51.5 / 
      DATA subname(104), dif0(104), ar(104), meso(104), lebas(104) / 'ACETONITRILE    ',0.1280,    5.0, 0.0,   52.3 / 
      DATA subname(105), dif0(105), ar(105), meso(105), lebas(105) / '6_NITRO_O_CRESOL',0.0664,   16.0, 0.0,  155.0 / ! dif0, eqn 9-22 Schwarzenbach Gschwend & Imboden (1993) Env Org Chem
      DATA subname(106), dif0(106), ar(106), meso(106), lebas(106) / 'GENERIC_ALDEHYDE',0.0646,   10.0, 0.0,   56.0 / ! PCVOC
      DATA subname(107), dif0(107), ar(107), meso(107), lebas(107) / 'NTR_OH          ',0.0722,   16.0, 0.1,  140.4 / ! INTR
      DATA subname(108), dif0(108), ar(108), meso(108), lebas(108) / 'METHYLHYDROPEROX',0.0853,   10.0, 0.3,   49.0 / ! ISPX diffusion should be ~ 0.0710 according to Wolfe and thornton 2011 ACP
      DATA subname(109), dif0(109), ar(109), meso(109), lebas(109) / 'METHYLHYDROPEROX',0.1371,   10.0, 0.3,   49.0 / ! ROOH diffusion should be ~ 0.0710 according to Wolfe and thornton 2011 ACP
      DATA subname(110), dif0(110), ar(110), meso(110), lebas(110) / 'ADIPIC_ACID     ',0.0646,90000.0, 0.0,   63.0 / ! LVPCSOG
      DATA subname(111), dif0(111), ar(111), meso(111), lebas(111) / 'ADIPIC_ACID     ',0.0456,    4.2, 0.0,   63.0 / ! VIVPO1
      DATA subname(112), dif0(112), ar(112), meso(112), lebas(112) / 'ADIPIC_ACID     ',0.0766,71624.8, 0.0,   63.0 / ! VLVOO1
      DATA subname(113), dif0(113), ar(113), meso(113), lebas(113) / 'ADIPIC_ACID     ',0.0766, 9042.0, 0.0,   63.0 / ! VLVOO2
      DATA subname(114), dif0(114), ar(114), meso(114), lebas(114) / 'ADIPIC_ACID     ',0.0533,13818.0, 0.0,   63.0 / ! VLVPO1
      DATA subname(115), dif0(115), ar(115), meso(115), lebas(115) / 'ADIPIC_ACID     ',0.0771, 1133.9, 0.0,   63.0 / ! VSVOO1
      DATA subname(116), dif0(116), ar(116), meso(116), lebas(116) / 'ADIPIC_ACID     ',0.0771,   18.1, 0.0,   63.0 / ! VSVOO2
      DATA subname(117), dif0(117), ar(117), meso(117), lebas(117) / 'ADIPIC_ACID     ',0.0775,    2.3, 0.0,   63.0 / ! VSVOO3
      DATA subname(118), dif0(118), ar(118), meso(118), lebas(118) / 'ADIPIC_ACID     ',0.0511, 1830.5, 0.0,   63.0 / ! VSVPO1
      DATA subname(119), dif0(119), ar(119), meso(119), lebas(119) / 'ADIPIC_ACID     ',0.0493,  241.0, 0.0,   63.0 / ! VSVPO2
      DATA subname(120), dif0(120), ar(120), meso(120), lebas(120) / 'ADIPIC_ACID     ',0.0474,   31.8, 0.0,   63.0 / ! VSVPO3
      DATA subname(121), dif0(121), ar(121), meso(121), lebas(121) / 'FORMIC_ACID     ',0.1411,   20.0, 0.0,   63.0 / ! FACD
      DATA subname(122), dif0(122), ar(122), meso(122), lebas(122) / 'MEK             ',0.0859,    1.0, 0.0,  108.2 / ! KET different in different mechanisms
      DATA subname(123), dif0(123), ar(123), meso(123), lebas(123) / 'ETHENE          ',0.1366,    1.0, 0.0,   58.1 / ! ETH
      DATA subname(124), dif0(124), ar(124), meso(124), lebas(124) / 'HNO4            ',0.1233,    1.0, 0.0,   45.2 / ! PNA
      DATA subname(125), dif0(125), ar(125), meso(125), lebas(125) / 'GLYOXAL         ',0.1188,    1.0, 0.0,   56.2 / ! GLY
      DATA subname(126), dif0(126), ar(126), meso(126), lebas(126) / 'GLYOXAL         ',0.1181,    1.0, 0.0,   56.4 / ! GLYD
      DATA subname(127), dif0(127), ar(127), meso(127), lebas(127) / 'METHYL_GLYOXAL  ',0.1038,    1.0, 0.0,   72.5 / ! MGLY
      DATA subname(128), dif0(128), ar(128), meso(128), lebas(128) / 'ETHANE          ',0.1312,    1.0, 0.0,   61.5 / ! ETHA
      DATA subname(129), dif0(129), ar(129), meso(129), lebas(129) / 'ETHANOL         ',0.1213,    1.0, 0.0,   59.1 / ! ETOH
      DATA subname(130), dif0(130), ar(130), meso(130), lebas(130) / 'ETHANE          ',0.0870,    1.0, 0.0,  111.1 / ! PAR as Pentane
      DATA subname(131), dif0(131), ar(131), meso(131), lebas(131) / 'ACETONE         ',0.1057,    1.0, 0.0,   75.2 / ! ACET
      DATA subname(132), dif0(132), ar(132), meso(132), lebas(132) / 'PROPANE         ',0.1095,    1.0, 0.0,   78.1 / ! PRPA
      DATA subname(133), dif0(133), ar(133), meso(133), lebas(133) / 'ACETYLENE       ',0.1523,    1.0, 0.0,   45.8 / ! ETHY
      DATA subname(134), dif0(134), ar(134), meso(134), lebas(134) / 'ETHENE          ',0.1135,    1.0, 0.0,   73.1 / ! OLE as Propene
      DATA subname(135), dif0(135), ar(135), meso(135), lebas(135) / 'ETHENE          ',0.0990,    1.0, 0.0,   89.5 / ! IOLE as Isobutene
      DATA subname(136), dif0(136), ar(136), meso(136), lebas(136) / 'MEK             ',0.0852,    1.0, 0.0,  101.2 / ! IEPOX different scavenging H in CB05 and CB06
      DATA subname(137), dif0(137), ar(137), meso(137), lebas(137) / 'BENZENE         ',0.0942,    1.0, 0.0,   89.4 / ! BENZENE
      DATA subname(138), dif0(138), ar(138), meso(138), lebas(138) / '2-CRESOL        ',0.0850,    1.0, 0.0,  108.1 / ! CRES
      DATA subname(139), dif0(139), ar(139), meso(139), lebas(139) / 'TOLUENE         ',0.0860,    1.0, 0.0,  105.7 / ! TOL
      DATA subname(140), dif0(140), ar(140), meso(140), lebas(140) / 'O-XYLENE        ',0.0796,    1.0, 0.0,  122.0 / ! XYLMN
      DATA subname(141), dif0(141), ar(141), meso(141), lebas(141) / 'O-XYLENE        ',0.0777,    1.0, 0.0,  123.5 / ! NAPH
      DATA subname(142), dif0(142), ar(142), meso(142), lebas(142) / 'PHENOL          ',0.0844,    1.0, 0.0,  102.6 / ! CAT1
      DATA subname(143), dif0(143), ar(143), meso(143), lebas(143) / 'PINENE          ',0.0545,    1.0, 0.0,  251.5 / ! SESQ
      DATA subname(144), dif0(144), ar(144), meso(144), lebas(144) / 'PINENE          ',0.0700,    1.0, 0.0,  136.2 / ! TERP
      DATA subname(145), dif0(145), ar(145), meso(145), lebas(145) / 'ISOPRENE        ',0.0913,    1.0, 0.0,  136.2 / ! ISOP
      DATA subname(146), dif0(146), ar(146), meso(146), lebas(146) / 'METHACROLEIN    ',0.1033,    1.0, 0.0,   69.6 / ! OPEN C4H4O2
      DATA subname(147), dif0(147), ar(147), meso(147), lebas(147) / 'MEK             ',0.0950,    1.0, 0.0,   81.7 / ! XOPN C5H6O2
      DATA subname(148), dif0(148), ar(148), meso(148), lebas(148) / 'DECANE          ',0.0739,    1.0, 0.0,  142.8 / ! SOAALK as Propylcyclopentane
      DATA subname(149), dif0(149), ar(149), meso(149), lebas(149) / '13-BUTADIENE    ',0.1019,    1.0, 0.0,   84.8 / ! BUTADIENE13
      DATA subname(150), dif0(150), ar(150), meso(150), lebas(150) / 'ACROLEIN        ',0.1092,    1.0, 0.0,   70.5 /
      DATA subname(151), dif0(151), ar(151), meso(151), lebas(151) / 'SVMT1           ',0.0424,   20.0,      0.0, 355.2/ ! see Xu et al., 2018 ACPD: doi:10.5194/acp-2017-1109
      DATA subname(152), dif0(152), ar(152), meso(152), lebas(152) / 'SVMT2           ',0.0556,   20.0,      0.0, 236.8/
      DATA subname(153), dif0(153), ar(153), meso(153), lebas(153) / 'SVMT3           ',0.0583,   20.0,      0.0, 214.6/
      DATA subname(154), dif0(154), ar(154), meso(154), lebas(154) / 'SVMT4           ',0.0587,   20.0,      0.0, 229.4/
      DATA subname(155), dif0(155), ar(155), meso(155), lebas(155) / 'SVMT5           ',0.0619,   20.0,      0.0, 207.2/
      DATA subname(156), dif0(156), ar(156), meso(156), lebas(156) / 'SVMT6           ',0.0624,   20.0,      0.0, 222.0/
      DATA subname(157), dif0(157), ar(157), meso(157), lebas(157) / 'SVMT7           ',0.0661,   20.0,      0.0, 199.8/
      DATA subname(158), dif0(158), ar(158), meso(158), lebas(158) / 'SVAVB1          ',0.0560,100388.0,     0.0, 163.1/
      DATA subname(159), dif0(159), ar(159), meso(159), lebas(159) / 'SVAVB2          ',0.0600,  1461.2,     0.0, 163.2/
      DATA subname(160), dif0(160), ar(160), meso(160), lebas(160) / 'SVAVB3          ',0.0620,   175.2,     0.0, 163.0/
      DATA subname(161), dif0(161), ar(161), meso(161), lebas(161) / 'SVAVB4          ',0.0650,    20.8,     0.0, 162.7/
      DATA subname(162), dif0(162), ar(162), meso(162), lebas(162) / 'CLNO3           ',0.0902,    8.0,      0.0,  52.5/
      DATA subname(163), dif0(163), ar(163), meso(163), lebas(163) / 'FMBR            ',0.0965,   10.0,      0.0,  52.5/
      DATA subname(164), dif0(164), ar(164), meso(164), lebas(164) / 'I2              ',0.0795,    4.0,      0.0,  77.0/
      DATA subname(165), dif0(165), ar(165), meso(165), lebas(165) / 'CH3I            ',0.0881,    2.0,      0.0,  66.5/
      DATA subname(166), dif0(166), ar(166), meso(166), lebas(166) / 'ICL             ',0.0878,    4.0,      0.0,  63.0/
      DATA subname(167), dif0(167), ar(167), meso(167), lebas(167) / 'IBR             ',0.0851,    4.0,      0.0,  70.0/
      DATA subname(168), dif0(168), ar(168), meso(168), lebas(168) / 'MI2             ',0.0713,    2.0,      0.0,  98.0/
      DATA subname(169), dif0(169), ar(169), meso(169), lebas(169) / 'MIB             ',0.0753,    2.0,      0.0,  91.0/
      DATA subname(170), dif0(170), ar(170), meso(170), lebas(170) / 'MIC             ',0.0773,    2.0,      0.0,  84.0/
      DATA subname(171), dif0(171), ar(171), meso(171), lebas(171) / 'BR2             ',0.0925,    2.0,      0.0,  63.0/
      DATA subname(172), dif0(172), ar(172), meso(172), lebas(172) / 'MB3             ',0.0705,    2.0,      0.0, 108.5/
      DATA subname(173), dif0(173), ar(173), meso(173), lebas(173) / 'MB2             ',0.0804,    2.0,      0.0,  84.0/
      DATA subname(174), dif0(174), ar(174), meso(174), lebas(174) / 'MB2C            ',0.0720,    2.0,      0.0, 101.5/
      DATA subname(175), dif0(175), ar(175), meso(175), lebas(175) / 'MBC2            ',0.0739,    2.0,      0.0,  94.5/
      DATA subname(176), dif0(176), ar(176), meso(176), lebas(176) / 'MBC             ',0.0834,    2.0,      0.0,  77.0/
      DATA subname(177), dif0(177), ar(177), meso(177), lebas(177) / 'CLO             ',0.1288,    8.0,      0.0,  31.5/
      DATA subname(178), dif0(178), ar(178), meso(178), lebas(178) / 'ACETALDEHYDE    ',0.0975,    1.0,      0.0,  58.9/    
      DATA subname(179), dif0(179), ar(179), meso(179), lebas(179) / 'ACETYLENE       ',0.1212,    1.0,      0.0,  45.8/    
      DATA subname(180), dif0(180), ar(180), meso(180), lebas(180) / 'ACROOPERA       ',0.0869,    1.0,      0.0,  70.5/    
      DATA subname(181), dif0(181), ar(181), meso(181), lebas(181) / 'ACETONE         ',0.0842,    1.0,      0.0,  75.2/    
      DATA subname(182), dif0(182), ar(182), meso(182), lebas(182) / 'APIOPERA        ',0.0560,    1.0,      0.0, 154.9/    
      DATA subname(183), dif0(183), ar(183), meso(183), lebas(183) / 'BENZALDEHYDE    ',0.0688,    1.0,      0.0, 101.1/    
      DATA subname(184), dif0(184), ar(184), meso(184), lebas(184) / 'BDE13OPERA      ',0.0812,    1.0,      0.0,  84.8/    
      DATA subname(185), dif0(185), ar(185), meso(185), lebas(185) / 'BENOPERA        ',0.0751,    1.0,      0.0,  89.4/    
      DATA subname(186), dif0(186), ar(186), meso(186), lebas(186) / 'CSLOPERA        ',0.0590,    1.0,      0.0, 137.1/    
      DATA subname(187), dif0(187), ar(187), meso(187), lebas(187) / 'METHACROLEIN    ',0.0696,    1.0,      0.0, 100.6/    
      DATA subname(188), dif0(188), ar(188), meso(188), lebas(188) / 'METHACROLEIN    ',0.0647,    1.0,      0.0, 115.4/    
      DATA subname(189), dif0(189), ar(189), meso(189), lebas(189) / 'METHACROLEIN    ',0.0768,    1.0,      0.0,  82.8/    
      DATA subname(190), dif0(190), ar(190), meso(190), lebas(190) / 'ETHANOL         ',0.0965,    1.0,      0.0,  59.1/    
      DATA subname(191), dif0(191), ar(191), meso(191), lebas(191) / 'ETHENE          ',0.1085,    1.0,      0.0,  58.1/    
      DATA subname(192), dif0(192), ar(192), meso(192), lebas(192) / 'ETHYLENEGLYCOL  ',0.0931,    1.0,      0.0,  56.6/
      DATA subname(193), dif0(193), ar(193), meso(193), lebas(193) / 'FURANOPERA      ',0.0751,    1.0,      0.0,  83.9/    
      DATA subname(194), dif0(194), ar(194), meso(194), lebas(194) / 'FURANONEOPERA   ',0.0820,    1.0,      0.0,  66.5/    
      DATA subname(195), dif0(195), ar(195), meso(195), lebas(195) / 'HC10OPERA       ',0.0505,    1.0,      0.0, 194.0/    
      DATA subname(196), dif0(196), ar(196), meso(196), lebas(196) / 'HC3OPERA        ',0.0872,    1.0,      0.0,  78.1/    
      DATA subname(197), dif0(197), ar(197), meso(197), lebas(197) / 'HC5OPERA        ',0.0694,    1.0,      0.0, 111.0/    
      DATA subname(198), dif0(198), ar(198), meso(198), lebas(198) / 'HYDROXY-ACETONE ',0.0823,    1.0,      0.0,  72.7/
      DATA subname(199), dif0(199), ar(199), meso(199), lebas(199) / 'METHACROLEIN    ',0.0663,    1.0,      0.3, 107.6/    
      DATA subname(200), dif0(200), ar(200), meso(200), lebas(200) / 'ISOOPERA        ',0.0728,    1.0,      0.0, 101.0/    
      DATA subname(201), dif0(201), ar(201), meso(201), lebas(201) / '2NITRO_1BUTNL   ',0.0609,    1.0,      0.1, 125.4/    
      DATA subname(202), dif0(202), ar(202), meso(202), lebas(202) / 'LIMOPERA        ',0.0547,    1.0,      0.0, 163.0/    
      DATA subname(203), dif0(203), ar(203), meso(203), lebas(203) / 'UALDOPERA       ',0.0511,    1.0,      0.0, 183.3/    
      DATA subname(204), dif0(204), ar(204), meso(204), lebas(204) / 'METHACROLEIN    ',0.0772,    1.0,      0.0,  86.8/    
      DATA subname(205), dif0(205), ar(205), meso(205), lebas(205) / 'METHACROLEIN    ',0.0745,    1.0,      0.3,  84.1/    
      DATA subname(206), dif0(206), ar(206), meso(206), lebas(206) / 'MCTOPERA        ',0.0672,    1.7,      0.0, 103.0/    
      DATA subname(207), dif0(207), ar(207), meso(207), lebas(207) / 'MEK             ',0.0752,    1.0,      0.0,  91.7/    
      DATA subname(208), dif0(208), ar(208), meso(208), lebas(208) / 'METHANOL        ',0.1182,    1.0,      0.0,  42.5/    
      DATA subname(209), dif0(209), ar(209), meso(209), lebas(209) / 'MVK             ',0.0772,    1.0,      0.0,  86.8/    
      DATA subname(210), dif0(210), ar(210), meso(210), lebas(210) / '2NITRO_1BUTNL   ',0.0766,    1.0,      0.1,  78.0/    
      DATA subname(211), dif0(211), ar(211), meso(211), lebas(211) / 'OLIOPERA        ',0.0717,    1.0,      0.0, 104.0/    
      DATA subname(212), dif0(212), ar(212), meso(212), lebas(212) / 'OLTOPERA        ',0.0904,    1.0,      0.0,  73.1/    
      DATA subname(213), dif0(213), ar(213), meso(213), lebas(213) / 'MPAN            ',0.0647,    1.0,      0.1, 114.0/    
      DATA subname(214), dif0(214), ar(214), meso(214), lebas(214) / 'METHYLHYDROPEROX',0.1030,    1.0,      0.3,  48.9/    
      DATA subname(215), dif0(215), ar(215), meso(215), lebas(215) / 'METHYLHYDROPEROX',0.0881,    1.0,      0.3,  65.4/    
      DATA subname(216), dif0(216), ar(216), meso(216), lebas(216) / 'METHYLHYDROPEROX',0.0535,    1.0,      0.3, 162.4/    
      DATA subname(217), dif0(217), ar(217), meso(217), lebas(217) / 'ORA1OPERA       ',0.1119,    1.0,      0.0,  39.9/    
      DATA subname(218), dif0(218), ar(218), meso(218), lebas(218) / 'ORA2OPERA       ',0.0939,    1.0,      0.0,  56.2/    
      DATA subname(219), dif0(219), ar(219), meso(219), lebas(219) / 'PHENOPERA       ',0.0731,    3.4,      0.0,  86.3/    
      DATA subname(220), dif0(220), ar(220), meso(220), lebas(220) / 'GENERIC_ALDEHYDE',0.0521,    1.0,      0.0, 175.5/
      DATA subname(221), dif0(221), ar(221), meso(221), lebas(221) / 'PROGOPERA       ',0.0816,    1.0,      0.0,  73.4/    
      DATA subname(222), dif0(222), ar(222), meso(222), lebas(222) / 'ROCIOXYOPERA    ',0.0354,    1.0,      0.0, 384.0/    
      DATA subname(223), dif0(223), ar(223), meso(223), lebas(223) / 'ADIPIC_ACID     ',0.0382,    1.0,      0.0, 326.0/    
      DATA subname(224), dif0(224), ar(224), meso(224), lebas(224) / 'ADIPIC_ACID     ',0.0528,    1.0,      0.0, 172.0/    
      DATA subname(225), dif0(225), ar(225), meso(225), lebas(225) / 'ADIPIC_ACID     ',0.0431,    1.0,      0.0, 260.0/    
      DATA subname(226), dif0(226), ar(226), meso(226), lebas(226) / 'ADIPIC_ACID     ',0.0465,    1.0,      0.0, 221.0/    
      DATA subname(227), dif0(227), ar(227), meso(227), lebas(227) / 'ADIPIC_ACID     ',0.0483,    1.0,      0.0, 207.0/    
      DATA subname(228), dif0(228), ar(228), meso(228), lebas(228) / 'ADIPIC_ACID     ',0.0447,    1.0,      0.0, 243.0/    
      DATA subname(229), dif0(229), ar(229), meso(229), lebas(229) / 'ADIPIC_ACID     ',0.0484,    1.0,      0.0, 205.0/    
      DATA subname(230), dif0(230), ar(230), meso(230), lebas(230) / 'ADIPIC_ACID     ',0.0530,    1.0,      0.0, 174.0/    
      DATA subname(231), dif0(231), ar(231), meso(231), lebas(231) / 'N-PROPANOL      ',0.0836,    1.0,      0.0,  75.6/    
      DATA subname(232), dif0(232), ar(232), meso(232), lebas(232) / 'SLOWROCOPERA    ',0.1041,    1.0,      0.1,  38.9/    
      DATA subname(233), dif0(233), ar(233), meso(233), lebas(233) / '2NITRO_1BUTNL   ',0.0597,   18.6,      0.1, 123.5/    
      DATA subname(234), dif0(234), ar(234), meso(234), lebas(234) / 'UALDOPERA       ',0.0704,    1.0,      0.0, 102.0/    
      DATA subname(235), dif0(235), ar(235), meso(235), lebas(235) / 'XYEOPERA        ',0.0636,    1.0,      0.0, 122.0/    
      DATA subname(236), dif0(236), ar(236), meso(236), lebas(236) / 'XYMOPERA        ',0.0636,    1.0,      0.0, 122.0/    
      DATA subname(237), dif0(237), ar(237), meso(237), lebas(237) / 'ELHOLM          ',0.0464,49000.0,      0.3, 237.0/    
      DATA subname(238), dif0(238), ar(238), meso(238), lebas(238) / 'HOLM            ',0.0534,  711.0,      0.3, 157.7/    
      DATA subname(239), dif0(239), ar(239), meso(239), lebas(239) / 'METHYLHYDROPEROX',0.0551,  783.0,      0.3, 153.1/    
      DATA subname(240), dif0(240), ar(240), meso(240), lebas(240) / 'ADIPIC_ACID     ',0.0303,  218.0,      0.0, 508.0/    
      DATA subname(241), dif0(241), ar(241), meso(241), lebas(241) / 'ADIPIC_ACID     ',0.0365, 8450.0,      0.0, 353.0/    
      DATA subname(242), dif0(242), ar(242), meso(242), lebas(242) / 'ADIPIC_ACID     ',0.0465, 2550.0,      0.0, 216.0/    
      DATA subname(243), dif0(243), ar(243), meso(243), lebas(243) / 'ADIPIC_ACID     ',0.0557,  182.0,      0.0, 147.7/    
      DATA subname(244), dif0(244), ar(244), meso(244), lebas(244) / 'ADIPIC_ACID     ',0.0298,28400.0,      0.0, 524.0/    
      DATA subname(245), dif0(245), ar(245), meso(245), lebas(245) / 'ADIPIC_ACID     ',0.0405,  696.0,      0.0, 285.4/    
      DATA subname(246), dif0(246), ar(246), meso(246), lebas(246) / 'ADIPIC_ACID     ',0.0485, 1440.0,      0.0, 197.0/    
      DATA subname(247), dif0(247), ar(247), meso(247), lebas(247) / 'ADIPIC_ACID     ',0.0588,2060000.,     0.0, 130.0/    
      DATA subname(248), dif0(248), ar(248), meso(248), lebas(248) / 'ADIPIC_ACID     ',0.0308,   36.8,      0.0, 491.0/    
      DATA subname(249), dif0(249), ar(249), meso(249), lebas(249) / 'ADIPIC_ACID     ',0.0433,  130.0,      0.0, 251.0/    
      DATA subname(250), dif0(250), ar(250), meso(250), lebas(250) / 'ADIPIC_ACID     ',0.0505, 5520.0,      0.0, 183.0/    
      DATA subname(251), dif0(251), ar(251), meso(251), lebas(251) / 'ADIPIC_ACID     ',0.0314,    6.8,      0.0, 474.0/    
      DATA subname(252), dif0(252), ar(252), meso(252), lebas(252) / 'ADIPIC_ACID     ',0.0394,  583.0,      0.0, 304.0/    
      DATA subname(253), dif0(253), ar(253), meso(253), lebas(253) / 'ADIPIC_ACID     ',0.0483, 2820.0,      0.0, 202.0/    
      DATA subname(254), dif0(254), ar(254), meso(254), lebas(254) / 'ADIPIC_ACID     ',0.0332,    1.1,      0.0, 425.0/    
      DATA subname(255), dif0(255), ar(255), meso(255), lebas(255) / 'ADIPIC_ACID     ',0.0464,    9.6,      0.0, 221.0/    
      DATA subname(256), dif0(256), ar(256), meso(256), lebas(256) / 'ADIPIC_ACID     ',0.0355,    1.0,      0.0, 375.0/    
      DATA subname(257), dif0(257), ar(257), meso(257), lebas(257) / 'ADIPIC_ACID     ',0.0489,    1.0,      0.0, 198.7/
      DATA subname(258), dif0(258), ar(258), meso(258), lebas(258) / 'IPNOPERA        ',0.0652,   86.6,      0.3, 104.8/ ! VD_IPN   
      DATA subname(259), dif0(259), ar(259), meso(259), lebas(259) / 'IPCOPERA        ',0.0687,    1.0,      0.3,  99.3/ ! VD_IPC       
      DATA subname(260), dif0(260), ar(260), meso(260), lebas(260) / 'EBZOPERA        ',0.0636,    1.0,      0.0, 122.0/
      DATA subname(261), dif0(261), ar(261), meso(261), lebas(261) / 'XYLOPERA        ',0.0636,    1.0,      0.0, 122.0/
      DATA subname(262), dif0(262), ar(262), meso(262), lebas(262) / 'STYOPERA        ',0.0700,    1.0,      0.0, 115.4/
      DATA subname(263), dif0(263), ar(263), meso(263), lebas(263) / 'IPXOPERA        ',0.0716,    1.0,      0.3,  99.3/
      DATA subname(264), dif0(264), ar(264), meso(264), lebas(264) / 'INALDOPERA      ',0.0671,    1.0,      0.1,  78.0/
      DATA subname(265), dif0(265), ar(265), meso(265), lebas(265) / '2NITRO_1BUTNL   ',0.0517,216903.,      0.1, 152.2/ ! HONIT


      CONTAINS

C=======================================================================
         Subroutine INIT_MET ( JDATE, JTIME )

C-----------------------------------------------------------------------
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN; replace INTERP3 with INTERPX;
C                      allocatable RDEPVHT, RJACM, RRHOJ
C   14 Nov 03 J.Young: add reciprocal vertical Jacobian product for full and
C                      mid-layer
C   Tanya took JACOBF out of METCRO3D! Improvise
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   25 Jul 19 D.Wong: used N_SOIL_TYPE defined in LSM_Mod to handle
C                     various number of soil type from different WRF version
C-----------------------------------------------------------------------

         Use UTILIO_DEFN
         Use LSM_Mod, only : N_SOIL_TYPE

         Implicit None
 
         Include SUBST_FILES_ID  ! file name parameters

C Arguments:
         Integer, Intent( IN ) :: JDATE, JTIME      ! internal simulation date&time

C Local variables:
         Character( 16 ) :: PNAME = 'INIT_MET'
         Character( 16 ) :: VNAME
         CHARACTER( 16 ) :: UNITSCK
         CHARACTER( 30 ) :: MSG1 = ' Error interpolating variable '
         Character( 96 ) :: XMSG = ' '

         Integer V
         Integer ALLOCSTAT

C-----------------------------------------------------------------------

         If( MET_INITIALIZED )Return

!> Allocate buffers
         ALLOCATE ( BUFF1D( NLAYS ),
     &              BUFF2D( NCOLS,NROWS ),
     &              BUFF3D( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating Buffers'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         BUFF1D = 0.0
         BUFF2D = 0.0
         BUFF3D = 0.0

!> Allocate shared arrays
!> Met_Data
         ALLOCATE( Met_Data%RDEPVHT  ( NCOLS,NROWS ),
     &             Met_Data%DENS1    ( NCOLS,NROWS ),
     &             Met_Data%PRSFC    ( NCOLS,NROWS ),
     &             Met_Data%Q2       ( NCOLS,NROWS ),
     &             Met_Data%QSS_GRND ( NCOLS,NROWS ),
     &             Met_Data%RH2      ( NCOLS,NROWS ),
     &             Met_Data%RA       ( NCOLS,NROWS ),
     &             Met_Data%RS       ( NCOLS,NROWS ),
     &             Met_Data%RC       ( NCOLS,NROWS ),
     &             Met_Data%RN       ( NCOLS,NROWS ),
     &             Met_Data%RGRND    ( NCOLS,NROWS ),
     &             Met_Data%HFX      ( NCOLS,NROWS ),
     &             Met_Data%LH       ( NCOLS,NROWS ),
     &             Met_Data%SNOCOV   ( NCOLS,NROWS ),
     &             Met_Data%TEMP2    ( NCOLS,NROWS ),
     &             Met_Data%TEMPG    ( NCOLS,NROWS ),
     &             Met_Data%TSEASFC  ( NCOLS,NROWS ),
     &             Met_Data%USTAR    ( NCOLS,NROWS ),
     &             Met_Data%VEG      ( NCOLS,NROWS ),
     &             Met_Data%LAI      ( NCOLS,NROWS ),
     &             Met_Data%WR       ( NCOLS,NROWS ),
     &             Met_Data%WSPD10   ( NCOLS,NROWS ),
     &             Met_Data%WSTAR    ( NCOLS,NROWS ),
     &             Met_Data%Z0       ( NCOLS,NROWS ),
     &             Met_Data%SOIM1    ( NCOLS,NROWS ),
     &             Met_Data%SOIT1    ( NCOLS,NROWS ),
     &             Met_Data%SEAICE   ( NCOLS,NROWS ),
     &             Met_Data%MOL      ( NCOLS,NROWS ),
     &             Met_Data%MOLI     ( NCOLS,NROWS ),
     &             Met_Data%HOL      ( NCOLS,NROWS ),
     &             Met_Data%XPBL     ( NCOLS,NROWS ),
     &             Met_Data%LPBL     ( NCOLS,NROWS ),
     &             Met_Data%CONVCT   ( NCOLS,NROWS ),
     &             Met_Data%PBL      ( NCOLS,NROWS ),
!     &             Met_Data%NACL_EMIS( NCOLS,NROWS ),
     &             Met_Data%COSZEN   ( NCOLS,NROWS ),
     &             Met_Data%CFRAC    ( NCOLS,NROWS ),
     &             Met_Data%UWIND    ( NCOLS+1,NROWS+1,NLAYS ),
     &             Met_Data%VWIND    ( NCOLS+1,NROWS+1,NLAYS ),
     &             Met_Data%KZMIN    ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%PRES     ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%QV       ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%QC       ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%THETAV   ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%TA       ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%RH       ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%ZH       ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%ZF       ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%DZF      ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%DENS     ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%RJACM    ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%RJACF    ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%RRHOJ    ( NCOLS,NROWS,NLAYS ),
     &             STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating met vars'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         ALLOCATE( Grid_Data%DX3F    ( NLAYS ),
     &             Grid_Data%RDX3F   ( NLAYS ),
     &             Grid_Data%RDX3M   ( NLAYS ),
     &             Grid_Data%RMSFX4  ( NCOLS,NROWS ),
     &             Grid_Data%LON     ( NCOLS,NROWS ),
     &             Grid_Data%LAT     ( NCOLS,NROWS ),
     &             Grid_Data%LWMASK  ( NCOLS,NROWS ),
     &             Grid_Data%OCEAN   ( NCOLS,NROWS ),
     &             Grid_Data%SZONE   ( NCOLS,NROWS ),
     &             Grid_Data%PURB    ( NCOLS,NROWS ),
     &             Grid_Data%SLTYP   ( NCOLS,NROWS ),
     &             Grid_Data%WSAT    ( NCOLS,NROWS ),
     &             Grid_Data%WWLT    ( NCOLS,NROWS ),
     &             Grid_Data%WFC     ( NCOLS,NROWS ),   !We now use WFC whether BIDI is on or off
     &             Grid_Data%CLAY_PX   ( NCOLS,NROWS ),
     &             Grid_Data%CSAND_PX  ( NCOLS,NROWS ),
     &             Grid_Data%FMSAND_PX ( NCOLS,NROWS ),
     &             Grid_Data%NAME    ( n_lufrac ),
     &             Grid_Data%LU_Type ( n_lufrac ),
     &             Grid_Data%LUFRAC ( NCOLS,NROWS,n_lufrac ),
     &             STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating grid vars'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         Grid_Data%NAME      = name_lu
         Grid_Data%LU_Type   = cat_lu
         Grid_Data%WWLT      = 0.0
         Grid_Data%WSAT      = 0.0
         Grid_Data%WFC       = 0.0
         Grid_Data%CLAY_PX   = 0.0
         Grid_Data%CSAND_PX  = 0.0
         Grid_Data%FMSAND_PX = 0.0
         
         If ( BIOGEMIS_BEIS ) Then
            ALLOCATE( Met_Data%SOIT2    ( NCOLS,NROWS ),
     &                STAT = ALLOCSTAT )
            If ( ALLOCSTAT .Ne. 0 ) Then
               XMSG = 'Failure allocating layer 2 soil temperature'
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If
         End If

         If ( ABFLUX .or. BIOGEMIS_MEGAN  ) Then
            ALLOCATE( Met_Data%SOIM2    ( NCOLS,NROWS ),
     &                STAT = ALLOCSTAT )
            If ( ALLOCSTAT .Ne. 0 ) Then
               XMSG = 'Failure allocating layer 2 soil moisture'
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If
         END IF
         
         If ( ABFLUX .or. HGBIDI ) Then

            ALLOCATE( Grid_Data%BSLP   ( NCOLS,NROWS ),
     &                Grid_Data%WRES   ( NCOLS,NROWS ),
     &                STAT = ALLOCSTAT )
            If ( ALLOCSTAT .Ne. 0 ) Then
               XMSG = 'Failure allocating Soil grid vars'
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If

            Grid_Data%WRES    = 0.0
            Grid_Data%BSLP    = 0.0
         End If

!> ccccccccccccccccccccc enable backward compatiblity ccccccccccccccccccccc

         IF (RCA_AVAIL) THEN
            vname_rc = 'RCA'
         Else
            vname_rc = 'RC'
         End If

         IF (RNA_AVAIL) THEN
            vname_rn = 'RNA'
         Else
            vname_rn = 'RN'
         End If

         If (UWINDC_AVAIL) Then
            vname_uc = 'UWINDC'
            CSTAGUV  = .TRUE.
         Else
            vname_uc = 'UWIND'
            CSTAGUV  = .FALSE.
         End If

         If (VWINDC_AVAIL) Then
            vname_vc = 'VWINDC'
         Else
            vname_vc = 'VWIND'
         End If

         If ( .Not. MINKZ ) Then
            XMSG = 'This run uses Kz0UT, *NOT* KZMIN in subroutine edyintb.'
            Write( LOGDEV,'(/5X, A, /)' ) XMSG
         End If

#ifndef mpas
!> Get sigma coordinate variables
         Do L = 1, NLAYS
            Grid_Data%DX3F( L )  = X3FACE_GD( L ) - X3FACE_GD( L-1 )
            Grid_Data%RDX3F( L ) = 1.0 / Grid_Data%DX3F( L )
            BUFF1D( L ) = 0.5 * ( X3FACE_GD( L ) + X3FACE_GD( L-1 ) )
         End Do
         Do L = 1, NLAYS - 1
            Grid_Data%RDX3M( L ) = 1.0 / ( BUFF1D( L+1 ) - BUFF1D( L ) )
         End Do
         Grid_Data%RDX3M( NLAYS ) = 0.0
#endif

!> reciprocal of msfx2**2
#ifdef mpas
         Grid_Data%RMSFX4 = 1.0
         Grid_Data%PURB = g2ddata(:,:,purb_ind)
         Grid_Data%SLTYP = INT( g2ddata(:,:,sltyp_ind) )
         Grid_Data%LUFRAC = LUFRAC

         DO R = 1, NROWS
            DO C = 1, NCOLS
               IF (Grid_Data%SLTYP(C,R) <=  N_SOIL_TYPE) THEN
                  Grid_Data%WSAT( C,R ) = WSAT( Grid_Data%SLTYP( C,R ) )
                  Grid_Data%WFC ( C,R ) = WFC ( Grid_Data%SLTYP( C,R ) )
               ELSE
                  Grid_Data%WSAT( C,R ) = WSAT( N_SOIL_TYPE )
                  Grid_Data%WFC ( C,R ) = WFC ( N_SOIL_TYPE )
               END IF
            END DO
         END DO

#else

         Grid_Data%RMSFX4 = 1.0 / ( MSFX2**2 )

         Grid_Data%PURB = PURB

         Grid_Data%SLTYP = NINT( SOILCAT_A )

         Grid_Data%LUFRAC = LUFRAC

         If ( (ABFLUX .or. HGBIDI .or. BIOGEMIS_MEGAN .or. BIOGEMIS_BEIS) .and. .not. PXSOIL_AVAIL) Then
            Forall( C = 1:NCOLS, R = 1:NROWS, Grid_Data%SLTYP(C,R) .Le. N_SOIL_TYPE )
               Grid_Data%WWLT( C,R ) = WWLT( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WSAT( C,R ) = WSAT( Grid_Data%SLTYP( C,R ) )
            End Forall
         End If

         If (.not. PXSOIL_AVAIL) Then
            Forall( C = 1:NCOLS, R = 1:NROWS, Grid_Data%SLTYP(C,R) .Le. N_SOIL_TYPE )
               Grid_Data%WFC ( C,R ) = WFC ( Grid_Data%SLTYP( C,R ) )
            End Forall
         End If

         If ( ABFLUX .or. HGBIDI ) Then
            Forall( C = 1:NCOLS, R = 1:NROWS, Grid_Data%SLTYP(C,R) .Le. N_SOIL_TYPE )
               Grid_Data%WRES( C,R ) = WRES( Grid_Data%SLTYP( C,R ) )
               Grid_Data%BSLP( C,R ) = BSLP( Grid_Data%SLTYP( C,R ) )
            End Forall
         End If
#endif

         Grid_Data%LON = LON

         Grid_Data%LAT = LAT

         Grid_Data%LWMASK = LWMASK

         Grid_Data%OCEAN = ocean

         Grid_Data%SZONE = szone
         
         MET_INITIALIZED = .true.

         Return
         End Subroutine INIT_MET

C=======================================================================
         Subroutine GET_MET ( JDATE, JTIME, TSTEP )

C-----------------------------------------------------------------------
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN; replace INTERP3 with INTERPX;
C                      allocatable RDEPVHT, RJACM, RRHOJ
C   14 Nov 03 J.Young: add reciprocal vertical Jacobian product for full and
C                      mid-layer
C   Tanya took JACOBF out of METCRO3D! Improvise
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C-----------------------------------------------------------------------

         USE GRID_CONF       ! horizontal & vertical domain specifications
         Use UTILIO_DEFN
#ifndef mpas
#ifdef parallel
         USE SE_MODULES      ! stenex (using SE_COMM_MODULE)
#else
         USE NOOP_MODULES    ! stenex (using NOOP_COMM_MODULE)
#endif
#endif

         Implicit None
 
         Include SUBST_FILES_ID  ! file name parameters
         Include SUBST_PE_COMM   ! PE communication displacement and direction

C Arguments:

         Integer, Intent( IN ) :: JDATE, JTIME, TSTEP      ! internal simulation date&time

C Parameters:
         Real, Parameter :: cond_min = 1.0 / resist_max ! minimum conductance [m/s]
         Real, Parameter :: KZMAXL = 500.0    ! upper limit for min Kz [m]
         Real, Parameter :: KZ0UT  = 1.0      ! minimum eddy diffusivity [m**2/sec] KZ0
         Real, Parameter :: KZL    = 0.01     ! lowest KZ
         Real, Parameter :: KZU    = 1.0      ! 2.0  ! highest KZ
         Real, Parameter :: EPS    = 1.0E-08  ! small number for temperature difference

C Local variables:
         Real    FINT
         Real    CPAIR, LV, QST 
         Real    TMPFX, TMPVTCON, TST, TSTV
         Integer LP
         Integer C, R, L         ! loop induction variables

         Character( 16 ) :: PNAME = 'GET_MET'
         Character( 16 ) :: VNAME
         CharactER( 30 ) :: MSG1 = ' Error interpolating variable '
         Character( 96 ) :: XMSG = ' '

C-----------------------------------------------------------------------
C Interpolate file input variables and format for output
C-------------------------------- MET_CRO_3D --------------------------------
      
         call interpolate_var ('ZH', jdate, jtime, Met_Data%ZH)

         call interpolate_var ('PRES', jdate, jtime, Met_Data%PRES)

         call interpolate_var ('ZF', jdate, jtime, Met_Data%ZF)

         call interpolate_var ('DENS', jdate, jtime, Met_Data%DENS)

         Met_Data%DENS1 = Met_Data%DENS( :,:,1 )
     
#ifdef mpas
         Met_Data%RJACM = 1.0

         Met_Data%RJACF = 1.0
#else
         call interpolate_var ('JACOBM', jdate, jtime, Met_Data%RJACM)

         Met_Data%RJACM = 1.0 / Met_Data%RJACM

         call interpolate_var ('JACOBF', jdate, jtime, Met_Data%RJACF)

         Met_Data%RJACF = 1.0 / Met_Data%RJACF
#endif

         call interpolate_var ('DENSA_J', jdate, jtime, Met_Data%RRHOJ)

         Met_Data%RRHOJ = 1.0 / Met_Data%RRHOJ

         call interpolate_var ('TA', jdate, jtime, Met_Data%TA)

         call interpolate_var ('QV', jdate, jtime, Met_Data%QV)

         call interpolate_var ('QC', jdate, jtime, Met_Data%QC)

C-------------------------------- MET_CRO_2D --------------------------------
C Vegetation and surface vars
         call interpolate_var ('LAI', jdate, jtime, Met_Data%LAI)

         call interpolate_var ('VEG', jdate, jtime, Met_Data%VEG)

         call interpolate_var ('ZRUF', jdate, jtime, Met_Data%Z0)

C Soil vars
#ifdef mpas
         Met_Data%SOIM1 = smois_data(:,:,1)
         Met_Data%SOIM2 = smois_data(:,:,2)
         Met_Data%CLDFRAC = g2ddata(:,:,cfrac2dt_ind) ! for BDSNP soil NO
#else
         call interpolate_var ('SOIM1', jdate, jtime, Met_Data%SOIM1)

         If ( ABFLUX .or. BIOGEMIS_MEGAN) Then
            call interpolate_var ('SOIM2', jdate, jtime, Met_Data%SOIM2)
         End If
#endif

         call interpolate_var ('SOIT1', jdate, jtime, Met_Data%SOIT1)

         If ( BIOGEMIS_BEIS ) Then
            call interpolate_var ('SOIT2', jdate, jtime, Met_Data%SOIT2)
         End If

         call interpolate_var ('SEAICE', jdate, jtime, Met_Data%SEAICE)

C met vars

         call interpolate_var ('PRSFC', jdate, jtime, Met_Data%PRSFC)

         call interpolate_var ('RGRND', jdate, jtime, Met_Data%RGRND)

         call interpolate_var ('SNOCOV', jdate, jtime, Met_Data%SNOCOV)

         Where( Met_Data%SNOCOV .Lt. 0.0 )
            Met_Data%SNOCOV = 0.0
         End Where

         call interpolate_var ('TEMP2', jdate, jtime, Met_Data%TEMP2)

         call interpolate_var ('TEMPG', jdate, jtime, Met_Data%TEMPG)

         call interpolate_var ('USTAR', jdate, jtime, Met_Data%USTAR)

         call interpolate_var ('WSPD10', jdate, jtime, Met_Data%WSPD10)

         call interpolate_var ('HFX', jdate, jtime, Met_Data%HFX)

         If ( LH_AVAIL ) Then
            call interpolate_var ('LH', jdate, jtime, Met_Data%LH)
         Else   ! for backward compatibility
            call interpolate_var ('QFX', jdate, jtime, Met_Data%LH)
         End If

         call interpolate_var ('PBL', jdate, jtime, Met_Data%PBL)

        ! Update for WRFV4.1+ PX LSM runs that have soil texture in output for
        !  CMAQ dust scheme. These are initialized to 0 if not present in MCIP.
        !  DUST_EMIS.F will use table lookup values if 0 (old WRF or other LSMs).  
         If ( PXSOIL_AVAIL ) Then
            call interpolate_var ('CLAY_PX',   jdate, jtime, Grid_Data%CLAY_PX)
            call interpolate_var ('CSAND_PX',  jdate, jtime, Grid_Data%CSAND_PX)
            call interpolate_var ('FMSAND_PX', jdate, jtime, Grid_Data%FMSAND_PX)
            call interpolate_var ('WSAT_PX', jdate, jtime, Grid_Data%WSAT)
            call interpolate_var ('WFC_PX',  jdate, jtime, Grid_Data%WFC)
            call interpolate_var ('WWLT_PX', jdate, jtime, Grid_Data%WWLT)
         End If


C Met_cro_2D variables that have recently changed due to MCIP or WRF/CMAQ

         call interpolate_var (vname_rn, jdate, jtime, Met_Data%RN)

         call interpolate_var (vname_rc, jdate, jtime, Met_Data%RC)

         call interpolate_var ('CFRAC', jdate, jtime, Met_Data%CFRAC)

         If ( WR_AVAIL ) Then
            call interpolate_var ('WR', jdate, jtime, Met_Data%WR)
         End If

         If ( TSEASFC_AVAIL ) Then
            call interpolate_var ('TSEASFC', jdate, jtime, Met_Data%TSEASFC)
         Else
            Met_Data%TSEASFC = Met_Data%TEMPG
         End If

         If ( .not. RA_RS_AVAIL ) Then
            call interpolate_var ('RADYNI', jdate, jtime, Met_Data%RA)

            Where( Met_Data%RA .Gt. cond_min ) 
               Met_Data%RA = 1.0/Met_Data%RA
            Elsewhere
               Met_Data%RA = resist_max
            End Where

            call interpolate_var ('RSTOMI', jdate, jtime, Met_Data%RS)

            Where( Met_Data%RS .Gt. cond_min ) 
               Met_Data%RS = 1.0 / Met_Data%RS
            Elsewhere 
               Met_Data%RS = resist_max
            End Where

         Else 

            call interpolate_var ('RA', jdate, jtime, Met_Data%RA)

            call interpolate_var ('RS', jdate, jtime, Met_Data%RS)

         End If

         If ( Q2_AVAIL ) Then  ! Q2 in METCRO2D
            call interpolate_var ('Q2', jdate, jtime, Met_Data%Q2)
         Else
            Met_Data%Q2 = Met_Data%QV( :,:,1 )
         End If
         
         Where( Met_Data%TEMPG .Lt. stdtemp )
            BUFF2D = vp0 *Exp( 22.514 - ( 6.15e3 / Met_Data%TEMPG ) )
         Elsewhere
            BUFF2D = vp0 *Exp( svp2 * ( Met_Data%TEMPG -stdtemp ) / ( Met_Data%TEMPG -svp3 ) ) 
         End Where
         Met_Data%QSS_GRND = BUFF2D * 0.622 / ( Met_Data%PRSFC - BUFF2D )

         Where( Met_Data%TEMP2 .Lt. stdtemp )
            BUFF2D = vp0 *Exp( 22.514 - ( 6.15e3 / Met_Data%TEMP2 ) )
         Elsewhere
            BUFF2D = vp0 *Exp( svp2 * ( Met_Data%TEMP2 -stdtemp ) / ( Met_Data%TEMP2 -svp3 ) )
         End Where

         ! Calculate Relative Humidity at 2m
         Met_Data%RH2 = Met_Data%Q2 / ( BUFF2D * 0.622 / ( Met_Data%PRSFC - BUFF2D ) ) * 100.0
         Where( Met_Data%RH2 .Gt. 100.0 )
            Met_Data%RH2 = 100.0
         Elsewhere( Met_Data%RH2 .lt. 0.0 )
            Met_Data%RH2 = 0.0
         End Where

         ! Calculate 3D Relative Humidity at Grid Scale
         MET_DATA%RH = MET_DATA%QV * MET_DATA%PRES / ( MET_DATA%QV + 0.622015 ) /
     &                 ( 610.94 * EXP( 17.625 * ( MET_DATA%TA - 273.15 ) / 
     &                                 ( MET_DATA%TA - 273.15 + 243.04 ) ) )
         MET_DATA%RH = MIN( 0.9999, MAX( 0.001, MET_DATA%RH ) )

#ifndef mpas
C-------------------------------- MET_DOT_3D --------------------------------
         call interpolate_var (vname_uc, jdate, jtime, Met_Data%UWIND)

         call interpolate_var (vname_vc, jdate, jtime, Met_Data%VWIND)

C get ghost values for wind fields in case of free trop.
         CALL SUBST_COMM ( Met_Data%UWIND, DSPL_N0_E1_S0_W0, DRCN_E )
         CALL SUBST_COMM ( Met_Data%VWIND, DSPL_N1_E0_S0_W0, DRCN_N )
#endif

C-------------------------------- Calculated Variables --------------------------------
         Met_Data%DZF = Met_Data%ZF - EOSHIFT(Met_Data%ZF, Shift = -1, Boundary = 0.0, Dim = 3)

         Met_Data%RDEPVHT = 1.0 / Met_Data%ZF( :,:,1 )

         IF ( MINKZ ) THEN
            Met_Data%KZMIN = KZL
            DO L = 1, NLAYS
               Where( Met_Data%ZF( :,:,L ) .LE. KZMAXL )
                  Met_Data%KZMIN( :,:,L ) = KZL + ( KZU - KZL ) * 0.01 * Grid_data%PURB
               End Where
            End Do
         ELSE
            Met_Data%KZMIN = KZ0UT
         END IF

         BUFF3D = Met_Data%TA * ( 1.0 + 0.608 * Met_Data%QV )
         Met_Data%THETAV = BUFF3D * ( P0 / Met_Data%PRES ) ** 0.286

#ifdef mpas
         Met_Data%MOL = 1./g2ddata(:,:,rmol_ind)
#endif

C------  Updating MOL, then WSTAR, MOLI, HOL 
         DO R = 1, NROWS
            DO C = 1, NCOLS
#ifndef mpas
 !             CPAIR    = 1004.67 * ( 1.0 + 0.84 * Met_Data%QV( C,R,1 ) )   ! J/(K KG)
               CPAIR    = CPD * ( 1.0 + 0.84 * Met_Data%QV( C,R,1 ) )   ! J/(K KG)
               TMPFX    = Met_Data%HFX( C,R ) / ( CPAIR * Met_Data%DENS( C,R,1 ) )
               TMPVTCON = 1.0 + 0.608 * Met_Data%QV( C,R,1 )  ! Conversion factor for virtual temperature
               TST      = -TMPFX / Met_Data%USTAR( C,R )
               IF ( Met_Data%TA( C,R,1 ) .GT. STDTEMP ) THEN
                  LV = LV0 - ( 0.00237 * ( Met_Data%TA( C,R,1 ) - STDTEMP ) ) * 1.0E6
               ELSE
                  LV = 2.83E6     ! Latent heat of sublimation at 0C from Stull (1988) (J/KG)
               END IF
               QST  = -( Met_Data%LH( C,R ) / LV )
     &              / ( Met_Data%USTAR( C,R ) * Met_Data%DENS( C,R,1 ) )
               TSTV = TST * TMPVTCON + Met_Data%THETAV( C,R,1 ) * 0.608 * QST
               IF ( ABS( TSTV ) .LT. 1.0E-6 ) THEN
                  TSTV = SIGN( 1.0E-6, TSTV )
               END IF
               Met_Data%MOL( C,R ) = Met_Data%THETAV( C,R,1 )
     &                             * Met_Data%USTAR( C,R ) ** 2 / ( karman * GRAV * TSTV )
#endif
               IF ( Met_Data%MOL( C,R ) .LT. 0.0 ) THEN
                  Met_Data%WSTAR( C,R ) = Met_Data%USTAR( C,R ) * ( Met_Data%PBL( C,R )
     &                                  / ( karman * ABS( Met_Data%MOL( C,R ) ) ) ) ** 0.333333
               ELSE
                  Met_Data%WSTAR( C,R ) = 0.0
               END IF

            END DO
         END DO
   
         Met_Data%MOLI  = 1.0 / Met_Data%MOL 
         Met_Data%HOL   = Met_Data%PBL / Met_Data%MOL
C------

         Met_Data%CONVCT = .FALSE.
         DO R = 1, NROWS
            DO C = 1, NCOLS
               DO L = 1, NLAYS
                  IF ( Met_Data%PBL( C,R ) .LT. Met_Data%ZF( C,R,L ) ) THEN
                     LP = L; EXIT
                  END IF
               END DO

               Met_Data%LPBL( C,R ) = LP
#ifndef mpas
               If ( LP .Eq. 1 ) Then
                  FINT = ( Met_Data%PBL( C,R ) )
     &                 / ( Met_Data%ZF( C,R,LP ) )
                  Met_Data%XPBL( C,R ) = FINT * ( X3FACE_GD( LP ) - X3FACE_GD( LP-1 ) )
     &                        + X3FACE_GD( LP-1 )
               Else
                  FINT = ( Met_Data%PBL( C,R )   - Met_Data%ZF( C,R,LP-1 ) )
     &                 / ( Met_Data%ZF( C,R,LP ) - Met_Data%ZF( C,R,LP-1 ) )
                  Met_Data%XPBL( C,R ) = FINT * ( X3FACE_GD( LP ) - X3FACE_GD( LP-1 ) )
     &                        + X3FACE_GD( LP-1 )
               End If
#endif
            END DO
         END DO
         Where( Met_Data%THETAV( :,:,1 ) - Met_Data%THETAV( :,:,2 ) .Gt. EPS .And.
     &          Met_Data%HOL .Lt. -0.02 .And. Met_Data%LPBL .Gt. 3 )
            Met_Data%CONVCT = .True.
         End Where

!        Calculate the cosine of the zenith angle 
         CALL CZANGLE(JDATE, JTIME, NCOLS, NROWS )

         Return
         End Subroutine GET_MET

      End Module ASX_DATA_MOD
