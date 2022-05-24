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
C    D. Wong: Implemented centralized I/O approach, removed all MY_N clauses (02/2019)
C    R. Gilliam: Include PX soil texture information when available (03/2022)
C-------------------------------------------------------------------------------

      Use GRID_CONF           ! horizontal & vertical domain specifications
      Use LSM_MOD             ! Land surface data
      use centralized_io_module

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
         Real,    Allocatable :: NACL_EMIS( :,: )  ! NACL mass emission rate of particles with d <10 um (g/m2/s)
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
         Real,    Allocatable :: RH       ( :,:,: )  ! relative humidity (ratio)
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
         Real,    Allocatable :: OCEAN  ( :,: )  ! Open ocean
         Real,    Allocatable :: SZONE  ( :,: )  ! Surf zone
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
         Real,    Allocatable :: RHOB   ( :,: )  ! soil bulk density
         Real,    Allocatable :: LUFRAC ( :,:,: ) ! land use fraction (col,row,lu_type)[ratio]
C Land use information:
!         Character( 16 ), Allocatable   :: NAME    ( : )     ! LU name
!         Character( 16 ), Allocatable   :: LU_Type ( : )     ! general land use type e.g. water, forest, etc.
      End Type GRID_Type

      Type :: MOSAIC_Type                 ! (col,row,lu)
         Character( 16 ), Allocatable :: NAME    ( : ) ! LU name
         Character( 16 ), Allocatable :: LU_Type ( : ) ! general land use type e.g. water, forest, etc.
!> Sub grid cell meteorological variables:
         Real, Allocatable :: USTAR ( :,:,: )   ! surface friction velocity [m/s]
         Real, Allocatable :: LAI   ( :,:,: )   ! leaf area index [m**2/m**2]
         Real, Allocatable :: VEG   ( :,:,: )   ! vegetation fraction [ratio]
         Real, Allocatable :: Z0    ( :,:,: )   ! momentum roughness length [m]
         Real, Allocatable :: DELTA ( :,:,: )   ! Surface wetness [ratio]
!> Sub grid cell resistances
         Real, Allocatable :: RA    ( :,:,: )    ! aerodynamic resistance [s/m]
         Real, Allocatable :: RSTW  ( :,:,: )    ! Stomatal Resistance of water [s/m]
      End Type MOSAIC_Type

      Type( MET_Type ),     Save :: Met_Data 
      Type( GRID_Type ),    Save :: Grid_Data 
      Type( MOSAIC_Type ),  Save :: Mosaic_Data

!> asx constants
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
      Real, Parameter :: rsnow0     = 1000.0
      Real, Parameter :: svp2       = 17.67      ! from MM5 and WRF
      Real, Parameter :: svp3       = 29.65      ! from MM5 and WRF
      Real, Parameter :: rt25inK    = 1.0/(stdtemp + 25.0) ! 298.15K = 25C
      Real, Parameter :: twothirds  = 2.0 / 3.0
      Real, Parameter :: betah      = 5.0       ! MOST coefficient for stability correction for stable conditions 
      Real, Parameter :: gamah      = 16.0      ! MOST coefficient for stability correction for unstable conditions
      Real, Parameter :: karman     = 0.40      ! von Karman constant

      Logical, Save :: MET_INITIALIZED = .false.
      Real,    Save :: zsoil1             ! Layer 1 soil depth
      Real,    Save :: zsoil2             ! Layer 2 soil depth
      Logical, Save :: CSTAGUV            ! Winds are available with C stagger?

      Public        :: INIT_MET, GET_MET

      Integer,         Private :: C, R, L, S               ! loop induction variables
      Integer,         Private :: SPC
      Character( 16 ), Private, Save :: vname_rc, vname_rn, vname_uc, vname_vc

      Real, allocatable, Private :: BUFF1D( : )     ! 1D temp var number of layers
      Real, allocatable, Private :: BUFF2D( :,: )   ! 2D temp var
      Real, allocatable, Private :: BUFF3D( :,:,: ) ! 3D temp var

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
         Use MOSAIC_MOD, Only: Init_Mosaic, Tile_Data
         Use LSM_Mod, only : N_SOIL_TYPE

         Implicit None
 
         Include SUBST_FILES_ID  ! file name parameters

C Arguments:
         Integer, Intent( IN ) :: JDATE, JTIME      ! internal simulation date&time

C File variables:
         Real, Pointer    :: SOILCAT ( :,: )

C Local variables:
         Character( 16 ) :: PNAME = 'INIT_MET'
         Character( 16 ) :: VNAME
         CHARACTER( 16 ) :: UNITSCK
         CHARACTER( 30 ) :: MSG1 = ' Error interpolating variable '
         Character( 96 ) :: XMSG = ' '

C for INTERPX
         Integer V
         Integer ALLOCSTAT

C-----------------------------------------------------------------------

         If ( MET_INITIALIZED ) Return

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
     &             Met_Data%NACL_EMIS( NCOLS,NROWS ),
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


         ALLOCATE( Grid_Data%DX3F      ( NLAYS ),
     &             Grid_Data%RDX3F     ( NLAYS ),
     &             Grid_Data%RDX3M     ( NLAYS ),
     &             Grid_Data%RMSFX4    ( NCOLS,NROWS ),
     &             Grid_Data%LON       ( NCOLS,NROWS ),
     &             Grid_Data%LAT       ( NCOLS,NROWS ),
     &             Grid_Data%LWMASK    ( NCOLS,NROWS ),
     &             Grid_Data%OCEAN     ( NCOLS,NROWS ),
     &             Grid_Data%SZONE     ( NCOLS,NROWS ),
     &             Grid_Data%PURB      ( NCOLS,NROWS ),
     &             Grid_Data%SLTYP     ( NCOLS,NROWS ),
     &             Grid_Data%WSAT      ( NCOLS,NROWS ),
     &             Grid_Data%WWLT      ( NCOLS,NROWS ),
     &             Grid_Data%BSLP      ( NCOLS,NROWS ),
     &             Grid_Data%WRES      ( NCOLS,NROWS ),
     &             Grid_Data%WFC       ( NCOLS,NROWS ),
     &             Grid_Data%CLAY_PX   ( NCOLS,NROWS ),
     &             Grid_Data%CSAND_PX  ( NCOLS,NROWS ),
     &             Grid_Data%FMSAND_PX ( NCOLS,NROWS ),
     &             Grid_Data%RHOB      ( NCOLS,NROWS ),
     &             Grid_Data%LUFRAC    ( NCOLS,NROWS,n_lufrac ),

     &             STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating grid vars'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         Grid_Data%WSAT    = 0.0
         Grid_Data%WWLT    = 0.0
         Grid_Data%WFC     = 0.0
         Grid_Data%CLAY_PX   = 0.0
         Grid_Data%CSAND_PX  = 0.0
         Grid_Data%FMSAND_PX = 0.0
         Grid_Data%RHOB    = 0.0
         Grid_Data%WRES    = 0.0
         Grid_Data%BSLP    = 0.0

         ALLOCATE( Met_Data%SOIM2    ( NCOLS,NROWS ),
     &             Met_Data%SOIT2    ( NCOLS,NROWS ),
     &             STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating mosaic met vars'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         Grid_Data%WSAT    = 0.0
         Grid_Data%WWLT    = 0.0
         Grid_Data%WFC     = 0.0
         Grid_Data%RHOB    = 0.0
         Grid_Data%WRES    = 0.0
         Grid_Data%BSLP    = 0.0

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

!> Open the met files
        
         Do L = 1, NLAYS
            Grid_Data%DX3F( L )  = X3FACE_GD( L ) - X3FACE_GD( L-1 )
            Grid_Data%RDX3F( L ) = 1.0 / Grid_Data%DX3F( L )
            BUFF1D( L ) = 0.5 * ( X3FACE_GD( L ) + X3FACE_GD( L-1 ) )
         End Do
         Do L = 1, NLAYS - 1
            Grid_Data%RDX3M( L ) = 1.0 / ( BUFF1D( L+1 ) - BUFF1D( L ) )
         End Do
         Grid_Data%RDX3M( NLAYS ) = 0.0

!> reciprical of msfx2**2
         Grid_Data%RMSFX4 = 1.0 / ( MSFX2**2 )

         Grid_Data%LON = LON

         Grid_Data%LAT = LAT

         Grid_Data%LWMASK = LWMASK

         Grid_Data%PURB = PURB

         Grid_Data%SLTYP = NINT( SOILCAT_A )

         Grid_Data%LUFRAC = LUFRAC

         If( PX_LSM ) Then
            Write(Logdev,*) 'MCIP/WRF used the PX LSM'
            zsoil1 = 0.01
            zsoil2 = 0.05 ! For abflux estimated from 1cm and 1 meter 
! Estimate a constistent soil hydrolic suction at saturation using the JN90 properties to
! calculate soil hydrolic proerties constistant with JN90 data. Note that the suction at 
! feild capacity is typically 33 kPa, wilting point is 1500 kPa and residual water content
! is between 1.0e5 and 1.0e6 kPa and 3.0e5 kPa is used here following Campbell and Norman 1998             
            Psi_Sat =1500.0 * ( WWLT_PX / WSAT_PX )**BSLP_PX 
            WFC_PX  = WSAT_PX * ( 33.0 / Psi_sat )**(-1.0/BSLP_PX)
            WRES_PX = WSAT_PX * ( 3.0e5 / Psi_sat )**(-1.0/BSLP_PX)

            Forall( C = 1:NCOLS, R = 1:NROWS, Grid_Data%SLTYP(C,R) .Le. N_SOIL_TYPE )
               Grid_Data%WSAT( C,R ) = WSAT_PX( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WWLT( C,R ) = WWLT_PX( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WFC ( C,R ) = WFC_PX ( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WRES( C,R ) = WRES_PX( Grid_Data%SLTYP( C,R ) )
               Grid_Data%BSLP( C,R ) = BSLP_PX( Grid_Data%SLTYP( C,R ) )
               Grid_Data%RHOB( C,R ) = RHOB_PX( Grid_Data%SLTYP( C,R ) )
            End Forall
! for cases where the default soil is water but fractional land use may not be water
! use sandy loam
            Forall( C = 1:NCOLS, R = 1:NROWS, Grid_Data%SLTYP(C,R) .Gt. N_SOIL_TYPE )
               Grid_Data%WSAT( C,R ) = WSAT_PX( 3 )
               Grid_Data%WWLT( C,R ) = WWLT_PX( 3 )
               Grid_Data%WFC ( C,R ) = WFC_PX ( 3 )
               Grid_Data%WRES( C,R ) = WRES_PX( 3 )
               Grid_Data%BSLP( C,R ) = BSLP_PX( 3 )
               Grid_Data%RHOB( C,R ) = RHOB_PX( 3 )
            End Forall
         Else If( Noah_LSM ) Then
            Write(Logdev,*) 'MCIP/WRF used the Noah LSM'
            zsoil1 = 0.01 ! Adjusted based on Darmenova et al. 2009 doi:10.1029/2008JD011236
            zsoil2 = 0.05 ! For abflux estimated from 1cm and 1 meter 
            Forall( C = 1:NCOLS, R = 1:NROWS )
               Grid_Data%WSAT( C,R ) = WSAT_NOAH( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WWLT( C,R ) = WWLT_NOAH( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WFC ( C,R ) = WFC_NOAH ( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WRES( C,R ) = WRES_NOAH( Grid_Data%SLTYP( C,R ) )
               Grid_Data%BSLP( C,R ) = BSLP_NOAH( Grid_Data%SLTYP( C,R ) )
               Grid_Data%RHOB( C,R ) = RHOB_NOAH( Grid_Data%SLTYP( C,R ) )
            End Forall
         Else If( CLM_LSM ) Then
            Write(Logdev,*) 'MCIP/WRF used the CLM LSM'
            zsoil1 = 0.0175
            zsoil2 = 0.0451
            Forall( C = 1:NCOLS, R = 1:NROWS )
               Grid_Data%WSAT( C,R ) = WSAT_CLM( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WWLT( C,R ) = WWLT_CLM( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WFC ( C,R ) = WFC_CLM ( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WRES( C,R ) = WRES_CLM( Grid_Data%SLTYP( C,R ) )
               Grid_Data%BSLP( C,R ) = BSLP_CLM( Grid_Data%SLTYP( C,R ) )
               Grid_Data%RHOB( C,R ) = RHOB_CLM( Grid_Data%SLTYP( C,R ) )
            End Forall                            
        End If

         Grid_Data%OCEAN = ocean

         Grid_Data%SZONE = szone

         CALL INIT_MOSAIC( JDATE, JTIME, Grid_Data%LUFRAC )

         ALLOCATE( Mosaic_Data%USTAR   ( NCOLS,NROWS,Tile_Data%n_lufrac ),
     &             Mosaic_Data%LAI     ( NCOLS,NROWS,Tile_Data%n_lufrac ),
     &             Mosaic_Data%DELTA   ( NCOLS,NROWS,Tile_Data%n_lufrac ),
     &             Mosaic_Data%VEG     ( NCOLS,NROWS,Tile_Data%n_lufrac ),
     &             Mosaic_Data%Z0      ( NCOLS,NROWS,Tile_Data%n_lufrac ),
     &             Mosaic_Data%RA      ( NCOLS,NROWS,Tile_Data%n_lufrac ),
     &             Mosaic_Data%RSTW    ( NCOLS,NROWS,Tile_Data%n_lufrac ),
     &             Mosaic_Data%NAME    ( Tile_Data%n_lufrac ),
     &             Mosaic_Data%LU_Type ( Tile_Data%n_lufrac ),
     &             STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating mosaic vars'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         Mosaic_Data%USTAR   = 0.0
         Mosaic_Data%LAI     = 0.0
         Mosaic_Data%DELTA   = 0.0
         Mosaic_Data%VEG     = 0.0
         Mosaic_Data%Z0      = 0.0
         Mosaic_Data%RSTW    = 1.0e6
         Mosaic_Data%RA      = 1.0e6
         Mosaic_Data%NAME    = Tile_Data%name_lu
         Mosaic_Data%LU_Type = Tile_Data%cat_lu     

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
         USE MOSAIC_MOD
#ifdef parallel
         USE SE_MODULES      ! stenex (using SE_COMM_MODULE)
#else
         USE NOOP_MODULES    ! stenex (using NOOP_COMM_MODULE)
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
C Check consistency with WRF LSM and CMAQ options
C-----------------------------------------------------------------------
         If( .Not. PX_LSM .And. .Not. CLM_LSM .And. .Not. NOAH_LSM ) Then
            XMSG = 'WRF LSM is not supported. PX, CLM and NOAH are supported'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            If( ABFLUX ) Then
               XMSG = 'NH3 bidirectional exchange is not supported using this WRF LSM option. Set CTM_ABFLUX to N.'
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End IF
            If( HGBIDI ) Then
               XMSG = 'Hg bidirectional exchange is not supported using this WRF LSM option. Set CTM_HGBIDI to N.'
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End IF
         End IF

C-----------------------------------------------------------------------
C Interpolate file input variables and format for output
C-------------------------------- MET_CRO_3D --------------------------------
      
         call interpolate_var ('ZH', jdate, jtime, Met_Data%ZH)

         call interpolate_var ('PRES', jdate, jtime, Met_Data%PRES)

         call interpolate_var ('ZF', jdate, jtime, Met_Data%ZF)

         call interpolate_var ('DENS', jdate, jtime, Met_Data%DENS)

         Met_Data%DENS1 = Met_Data%DENS( :,:,1 )

         call interpolate_var ('JACOBM', jdate, jtime, Met_Data%RJACM)
     
         Met_Data%RJACM = 1.0 / Met_Data%RJACM

         call interpolate_var ('JACOBF', jdate, jtime, Met_Data%RJACF)

         Met_Data%RJACF = 1.0 / Met_Data%RJACF

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
         call interpolate_var ('SOIM1', jdate, jtime, Met_Data%SOIM1)

         call interpolate_var ('SOIM2', jdate, jtime, Met_Data%SOIM2)

         call interpolate_var ('SOIT2', jdate, jtime, Met_Data%SOIT2)

         call interpolate_var ('SOIT1', jdate, jtime, Met_Data%SOIT1)

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

C Update for WRFV4.1+ PX LSM runs that have soil texture in output for
C  CMAQ dust scheme. These are initialized to 0 if not present in MCIP.
C  DUST_EMIS.F will use table lookup values if 0 (old WRF or other LSMs).  
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
         Else 
            Met_Data%WR = 0.0 
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
 


C-------------------------------- MET_DOT_3D --------------------------------
         call interpolate_var (vname_uc, jdate, jtime, Met_Data%UWIND)

         call interpolate_var (vname_vc, jdate, jtime, Met_Data%VWIND)

C get ghost values for wind fields in case of free trop.
         CALL SUBST_COMM ( Met_Data%UWIND, DSPL_N0_E1_S0_W0, DRCN_E )
         CALL SUBST_COMM ( Met_Data%VWIND, DSPL_N1_E0_S0_W0, DRCN_N )

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

C------  Updating MOL, then WSTAR, MOLI, HOL 
         DO R = 1, NROWS
            DO C = 1, NCOLS
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
            END DO
         END DO
         Where( Met_Data%THETAV( :,:,1 ) - Met_Data%THETAV( :,:,2 ) .Gt. EPS .And.
     &          Met_Data%HOL .Lt. -0.02 .And. Met_Data%LPBL .Gt. 3 )
            Met_Data%CONVCT = .True.
         End Where

C-------------------------------- TILE_CRO_2D --------------------------------
C read in land use specific meteorology from WRF Noah or Ruc mosaic options

         CALL CALC_LAI( JDATE, JTIME, MET_DATA%SOIT2, MET_DATA%LAI,
     &                  MET_DATA%VEG, MOSAIC_DATA%VEG, MOSAIC_DATA%LAI, MOSAIC_DATA%Z0 )

         CALL ra_wrf( MET_DATA%MOLI, MET_DATA%ZH, MET_DATA%RA, MET_DATA%Z0, 
     &                MOSAIC_DATA%Z0, MET_DATA%USTAR, MOSAIC_DATA%USTAR,
     &                MOSAIC_DATA%RA, gamah, betah, karman )       

         CALL MOS_RSTW( MOSAIC_DATA%LAI, Met_Data%RGRND, Met_Data%SOIM2, 
     &                  Grid_Data%WWLT, Grid_Data%WFC, Met_Data%TEMP2, MOSAIC_DATA%RA, MOSAIC_DATA%USTAR, 
     &                  MET_DATA%QSS_GRND, MET_DATA%QV, Met_Data%RS, Mosaic_Data%RSTW )

         CALL MOS_CanWat( MOSAIC_DATA%VEG, MOSAIC_DATA%LAI, Met_Data%WR, Mosaic_Data%DELTA, 
     &                    WR_AVAIL, Met_Data%Q2, Met_Data%QSS_GRND, Met_Data%RN, Met_Data%RC, Met_Data%WSPD10, 
     &                    Met_Data%RGRND, jdate, jtime)

!        Calculate the cosine of the zenith angle 
         CALL CZANGLE( JDATE, JTIME, NCOLS, NROWS )

         Return
         End Subroutine GET_MET

      End Module ASX_DATA_MOD
