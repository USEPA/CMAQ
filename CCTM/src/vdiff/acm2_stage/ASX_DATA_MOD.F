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
C-------------------------------------------------------------------------------

      Use GRID_CONF           ! horizontal & vertical domain specifications
      Use LSM_MOD             ! Land surface data

      Implicit None

      Include SUBST_CONST     ! constants

      Type :: MET_Type
!> 2-D meteorological fields:
         Real,    Allocatable :: RDEPVHT  ( :,: )  ! reciprocal layer 1 height [m-1]
         Real,    Allocatable :: DENS1    ( :,: )  ! layer 1 air density
         Real,    Allocatable :: PRSFC    ( :,: )  ! surface pressure [Pa]
         Real,    Allocatable :: Q2       ( :,: )  ! 2 meter water vapor mixing ratio [kg/kg]
         Real,    Allocatable :: QSS_GRND ( :,: )  ! ground saturation water vapor mixing ratio [kg/kg]
         Real,    Allocatable :: RH       ( :,: )  ! relative humidity [ratio]
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
         Real,    Allocatable :: RHOB   ( :,: )  ! soil bulk density
         Real,    Allocatable :: LUFRAC ( :,:,: ) ! land use fraction (col,row,lu_type)[ratio]
C Land use information:
         Character( 16 ), Allocatable   :: NAME    ( : )     ! LU name
         Character( 16 ), Allocatable   :: LU_Type ( : )     ! general land use type e.g. water, forest, etc.
      End Type GRID_Type

      Type :: MOSAIC_Type                 ! (col,row,lu)
         Character( 16 ), Allocatable :: NAME    ( : ) ! LU name
         Character( 16 ), Allocatable :: LU_Type ( : ) ! general land use type e.g. water, forest, etc.
!> Sub grid cell meteorological variables:
         Real, Allocatable :: USTAR ( :,:,: )   ! surface friction velocity [m/s]
         Real, Allocatable :: WSTAR ( :,:,: )   ! convective velocity scale [m/s]
         Real, Allocatable :: LAI   ( :,:,: )   ! leaf area index [m**2/m**2]
         Real, Allocatable :: VEG   ( :,:,: )   ! vegetation fraction [ratio]
         Real, Allocatable :: Z0    ( :,:,: )   ! vegetation fraction [ratio]
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
      Real, Parameter :: betah      = 5.0       ! WRF 3.6 px uses Dyer
      Real, Parameter :: gamah      = 16.0
      Real, Parameter :: karman     = 0.40

      Logical, Save :: MET_INITIALIZED = .false.
      Real,    Save :: zsoil1             ! Layer 1 soil depth
      Real,    Save :: zsoil2             ! Layer 2 soil depth
      Real,    Save :: CONVPA             ! Pressure conversion factor file units to Pa
      Logical, Save :: CSTAGUV            ! Winds are available with C stagger?
      Logical, Save :: ifwr     = .false.

      Public        :: INIT_MET, GET_MET

      Logical, Private, Save :: ifsst    = .false.
      Logical, Private, Save :: ifq2     = .false.
      Logical, Private, Save :: rinv     = .True.
      Logical, Private, Save :: iflh     = .false.
      
      Integer,         Private :: C, R, L, S               ! loop induction variables
      Integer,         Private :: SPC
      Character( 16 ), Private, Save :: vname_rc, vname_rn, vname_uc, vname_vc
      Real,            Private, Save :: P0    ! reference pressure (100000.0 Pa) for Potential Temperature, note that in meteorology they do not use the SI 1 ATM.  

      Integer, Private, Save :: GXOFF, GYOFF              ! global origin offset from file
      Integer, Private, Save :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 ! MET_CRO_3D
      Integer, Private, Save :: STRTCOLMD3, ENDCOLMD3, STRTROWMD3, ENDROWMD3 ! MET_DOT_3D
      Integer, Private, Save :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 ! MET_CRO_2D
      Integer, Private, Save :: STRTCOL_O1, ENDCOL_O1, STRTROW_O1, ENDROW_O1 ! OCEAN_1

      Real, Pointer, Private :: BUFF1D( : )     ! 1D temp var number of layers
      Real, Pointer, Private :: BUFF2D( :,: )   ! 2D temp var
      Real, Pointer, Private :: BUFF3D( :,:,: ) ! 3D temp var

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
C-----------------------------------------------------------------------

         Use UTILIO_DEFN
         Use MOSAIC_MOD, Only: Init_Mosaic

         Implicit None
 
         Include SUBST_FILES_ID  ! file name parameters

C Arguments:
         Integer, Intent( IN ) :: JDATE, JTIME      ! internal simulation date&time

C File variables:
         Real, Pointer    :: MSFX2   ( :,: )
         Real, Pointer    :: SOILCAT ( :,: )
         Real, Pointer    :: X3M     ( : )

C Local variables:
         Character( 16 ) :: PNAME = 'INIT_MET'
         Character( 16 ) :: VNAME
         CHARACTER( 16 ) :: UNITSCK
         CHARACTER( 30 ) :: MSG1 = ' Error interpolating variable '
         Character( 96 ) :: XMSG = ' '

C for INTERPX
         Integer STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2
         Integer V
         Integer ALLOCSTAT

C-----------------------------------------------------------------------

         If ( MET_INITIALIZED ) Return

         CALL INIT_MOSAIC( JDATE, JTIME )

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
     &             Met_Data%RH       ( NCOLS,NROWS ),
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
     &             Met_Data%UWIND    ( NCOLS+1,NROWS+1,NLAYS ),
     &             Met_Data%VWIND    ( NCOLS+1,NROWS+1,NLAYS ),
     &             Met_Data%KZMIN    ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%PRES     ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%QV       ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%QC       ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%THETAV   ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%TA       ( NCOLS,NROWS,NLAYS ),
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
     &             Grid_Data%BSLP    ( NCOLS,NROWS ),
     &             Grid_Data%WRES    ( NCOLS,NROWS ),
     &             Grid_Data%WFC     ( NCOLS,NROWS ),
     &             Grid_Data%RHOB    ( NCOLS,NROWS ),
     &             Grid_Data%LUFRAC  ( NCOLS,NROWS,n_lufrac ),
     &             Grid_Data%NAME    ( n_lufrac ),
     &             Grid_Data%LU_Type ( n_lufrac ),
     &             STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating grid vars'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         Grid_Data%NAME    = name_lu
         Grid_Data%LU_Type = cat_lu
         Grid_Data%WSAT    = 0.0
         Grid_Data%WWLT    = 0.0
         Grid_Data%WFC     = 0.0
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

         ALLOCATE( Mosaic_Data%USTAR   ( NCOLS,NROWS,n_lufrac ),
     &             Mosaic_Data%WSTAR   ( NCOLS,NROWS,n_lufrac ), 
     &             Mosaic_Data%LAI     ( NCOLS,NROWS,n_lufrac ),
     &             Mosaic_Data%DELTA   ( NCOLS,NROWS,n_lufrac ),
     &             Mosaic_Data%VEG     ( NCOLS,NROWS,n_lufrac ),
     &             Mosaic_Data%Z0      ( NCOLS,NROWS,n_lufrac ),
     &             Mosaic_Data%RA      ( NCOLS,NROWS,n_lufrac ),
     &             Mosaic_Data%RSTW    ( NCOLS,NROWS,n_lufrac ),
     &             Mosaic_Data%NAME    ( n_lufrac ),
     &             Mosaic_Data%LU_Type ( n_lufrac ),
     &             STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating mosaic vars'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         Mosaic_Data%USTAR   = 0.0
         Mosaic_Data%LAI     = 0.0
         Mosaic_Data%DELTA   = 0.0
         Mosaic_Data%VEG     = 0.0
         Mosaic_Data%Z0      = 0.000001
         Mosaic_Data%RSTW    = 1.0e10
         Mosaic_Data%RA      = 1.0e10
         Mosaic_Data%NAME    = name_lu
         Mosaic_Data%LU_Type = cat_lu     

!> ccccccccccccccccccccc enable backward compatiblity ccccccccccccccccccccc

         If ( .Not. desc3( met_cro_2d ) ) Then
            xmsg = 'Could not get ' // MET_CRO_2D // ' file description'
            Call m3exit( pname, JDATE, JTIME, xmsg, xstat2 )
         End If                          

         SPC = INDEX1( 'RA', NVARS3D, VNAME3D )
         If (SPC .Gt. 0) rinv = .FALSE. ! Ra and Rst are in units s/m

         SPC = INDEX1( 'WR', NVARS3D, VNAME3D )
         If (SPC .Gt. 0) ifwr = .True. ! canopy wetness is in METCRO2D

         SPC = INDEX1( 'Q2', NVARS3D, VNAME3D )
         If (SPC .Gt. 0) ifq2 = .True. ! two meter mixing ratio in METCRO2D

         SPC = INDEX1( 'TSEASFC', NVARS3D, VNAME3D )
         If (SPC .Gt. 0) ifsst = .True. ! two meter SST in METCRO2D

         SPC = INDEX1( 'LH', NVARS3D, VNAME3D )
         If (SPC .Gt. 0) iflh = .True. ! LH in METCRO2D

         SPC = INDEX1( 'RCA', NVARS3D, VNAME3D )
         If (SPC .Gt. 0) Then
            vname_rc = 'RCA'
         Else
            vname_rc = 'RC'
         End If

         SPC = INDEX1( 'RNA', NVARS3D, VNAME3D )
         If (SPC .Gt. 0) Then
            vname_rn = 'RNA'
         Else
            vname_rn = 'RN'
         End If

         If ( .Not. desc3( met_dot_3d ) ) Then
            xmsg = 'Could not get ' // MET_DOT_3D // ' file description'
            Call m3exit( pname, JDATE, JTIME, xmsg, xstat2 )
         End If                          

         SPC = INDEX1( 'UWINDC', NVARS3D, VNAME3D )
         If (SPC .Gt. 0) Then
            vname_uc = 'UWINDC'
            CSTAGUV  = .TRUE.
         Else
            vname_uc = 'UWIND'
            CSTAGUV  = .FALSE.
         End If

         SPC = INDEX1( 'VWINDC', NVARS3D, VNAME3D )
         If (SPC .Gt. 0) Then
            vname_vc = 'VWINDC'
         Else
            vname_vc = 'VWIND'
         End If

         If ( .Not. desc3( met_cro_3d ) ) Then
            xmsg = 'Could not get ' // MET_CRO_3D // ' file description'
            Call m3exit( pname, JDATE, JTIME, xmsg, xstat2 )
         End If

         V = INDEX1( 'PRES', NVARS3D, VNAME3D )
         If ( V .Ne. 0 ) Then
            UNITSCK = UNITS3D( V )
         Else
            XMSG = 'Could not get variable PRES from ' // MET_CRO_3D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         Select Case (UNITSCK)
            Case ( 'PASCAL','pascal','Pascal','PA','pa','Pa' )
               CONVPA = 1.0
               P0     = 100000.0
            Case ( 'MILLIBAR','millibar','Millibar','MB','mb','Mb' )
               CONVPA = 1.0E-02
               P0     = 100000.0 * CONVPA
            Case ( 'CENTIBAR','centibar','Centibar','CB','cb','Cb' )
               CONVPA = 1.0E-03
               P0     = 100000.0 * CONVPA
            Case Default
               XMSG = 'PRES units incorrect on ' // MET_CRO_3D
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End Select

         If ( .Not. MINKZ ) Then
            XMSG = 'This run uses Kz0UT, *NOT* KZMIN in subroutine edyintb.'
            Write( LOGDEV,'(/5X, A, /)' ) XMSG
         End If

!> Open the met files
        
         Call SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )
         Call SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )
         Call SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )
         Call SUBHFILE ( MET_DOT_3D, GXOFF, GYOFF,
     &                   STRTCOLMD3, ENDCOLMD3, STRTROWMD3, ENDROWMD3 )
         CALL SUBHFILE ( OCEAN_1, GXOFF, GYOFF,
     &                   STRTCOL_O1, ENDCOL_O1, STRTROW_O1, ENDROW_O1 )
!> Get sigma coordinate variables
         X3M => BUFF1D
         Do L = 1, NLAYS
            Grid_Data%DX3F( L )  = X3FACE_GD( L ) - X3FACE_GD( L-1 )
            Grid_Data%RDX3F( L ) = 1.0 / Grid_Data%DX3F( L )
            X3M( L ) = 0.5 * ( X3FACE_GD( L ) + X3FACE_GD( L-1 ) )
         End Do
         Do L = 1, NLAYS - 1
            Grid_Data%RDX3M( L ) = 1.0 / ( X3M( L+1 ) - X3M( L ) )
         End Do
         Grid_Data%RDX3M( NLAYS ) = 0.0
!> nullify pointer
         Nullify( X3M )

!> reciprical of msfx2**2
!> assign MSFX2
         MSFX2 => BUFF2D
         VNAME = 'MSFX2'
         If ( .Not. INTERPX( GRID_CRO_2D, VNAME, PNAME,
     &                       STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2, 1,1,
     &                       JDATE, JTIME, MSFX2 ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // GRID_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         Grid_Data%RMSFX4 = 1.0 / ( MSFX2**2 )
!> nullify pointer
         Nullify( MSFX2 )

         VNAME = 'LON'
         If ( .Not. INTERPX( GRID_CRO_2D, VNAME, PNAME,
     &                       STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2, 1,1,
     &                       JDATE, JTIME, Grid_Data%LON ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // GRID_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'LAT'
         If ( .Not. INTERPX( GRID_CRO_2D, VNAME, PNAME,
     &                       STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2, 1,1,
     &                       JDATE, JTIME, Grid_Data%LAT ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // GRID_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'LWMASK'
         If ( .Not. INTERPX( GRID_CRO_2D, VNAME, PNAME,
     &                       STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2, 1,1,
     &                       JDATE, JTIME, Grid_Data%LWMASK ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // GRID_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'PURB'
         If ( .Not. INTERPX( GRID_CRO_2D, VNAME, PNAME,
     &                       STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2, 1,1,
     &                       JDATE, JTIME, Grid_Data%PURB ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // GRID_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         SOILCAT => BUFF2D
         VNAME = 'SLTYP'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, SOILCAT ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         Grid_Data%SLTYP = NINT( SOILCAT )
         Nullify( SOILCAT )

         Do l = 1, n_lufrac
            Write( vname,'( "LUFRAC_",I2.2 )' ) l
            If ( .Not. INTERPX( GRID_CRO_2D, VNAME, PNAME,
     &                          STRTCOLGC2,ENDCOLGC2, STRTROWGC2,ENDROWGC2, 1,1,
     &                          JDATE, JTIME, Grid_Data%LUFRAC( :,:,l ) ) ) Then
               XMSG = MSG1 // TRIM( VNAME ) // ' from ' // GRID_CRO_2D
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If
         End Do
         If( PX_LSM ) Then
            Write(Logdev,*) 'MCIP/WRF used the PX LSM'
            zsoil1 = 0.01
            zsoil2 = 0.05 ! For abflux estimated from 1cm and 1 meter 
            Forall( C = 1:MY_NCOLS, R = 1:MY_NROWS, Grid_Data%SLTYP(C,R) .Le. 11 )
               Grid_Data%WSAT( C,R ) = WSAT_PX( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WWLT( C,R ) = WWLT_PX( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WFC ( C,R ) = WFC_PX ( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WRES( C,R ) = WRES_PX( Grid_Data%SLTYP( C,R ) )
               Grid_Data%BSLP( C,R ) = BSLP_PX( Grid_Data%SLTYP( C,R ) )
               Grid_Data%RHOB( C,R ) = RHOB_PX( Grid_Data%SLTYP( C,R ) )
            End Forall
! for cases where the default soil is water but fractional land use may not be water
! use sandy loam
            Forall( C = 1:MY_NCOLS, R = 1:MY_NROWS, Grid_Data%SLTYP(C,R) .Gt. 11 )
               Grid_Data%WSAT( C,R ) = WSAT_PX( 3 )
               Grid_Data%WWLT( C,R ) = WWLT_PX( 3 )
               Grid_Data%WFC ( C,R ) = WFC_PX ( 3 )
               Grid_Data%WRES( C,R ) = WRES_PX( 3 )
               Grid_Data%BSLP( C,R ) = BSLP_PX( 3 )
               Grid_Data%RHOB( C,R ) = RHOB_PX( 3 )
            End Forall
! Use Noah values that are hydraulically consistant
            Forall( C = 1:MY_NCOLS, R = 1:MY_NROWS, Grid_Data%SLTYP(C,R) .Le. 4 )
               Grid_Data%WSAT( C,R ) = WSAT_NOAH( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WWLT( C,R ) = WWLT_NOAH( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WFC ( C,R ) = WFC_NOAH ( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WRES( C,R ) = WRES_NOAH( Grid_Data%SLTYP( C,R ) )
               Grid_Data%BSLP( C,R ) = BSLP_NOAH( Grid_Data%SLTYP( C,R ) )
               Grid_Data%RHOB( C,R ) = RHOB_NOAH( Grid_Data%SLTYP( C,R ) )
            End Forall
! Use Noah values that are hydraulically consistant
            Forall( C = 1:MY_NCOLS, R = 1:MY_NROWS, Grid_Data%SLTYP(C,R) .gt. 4 )
               Grid_Data%WSAT( C,R ) = WSAT_NOAH( Grid_Data%SLTYP( C,R ) + 1 )
               Grid_Data%WWLT( C,R ) = WWLT_NOAH( Grid_Data%SLTYP( C,R ) + 1 )
               Grid_Data%WFC ( C,R ) = WFC_NOAH ( Grid_Data%SLTYP( C,R ) + 1 )
               Grid_Data%WRES( C,R ) = WRES_NOAH( Grid_Data%SLTYP( C,R ) + 1 )
               Grid_Data%BSLP( C,R ) = BSLP_NOAH( Grid_Data%SLTYP( C,R ) + 1 )
               Grid_Data%RHOB( C,R ) = RHOB_NOAH( Grid_Data%SLTYP( C,R ) + 1 )
            End Forall

! for cases where the default soil is water but fractional land use may not be water
! use sandy loam
            Forall( C = 1:MY_NCOLS, R = 1:MY_NROWS, Grid_Data%SLTYP(C,R) .Gt. 11 )
               Grid_Data%WSAT( C,R ) = WSAT_NOAH( 3 )
               Grid_Data%WWLT( C,R ) = WWLT_NOAH( 3 )
               Grid_Data%WFC ( C,R ) = WFC_NOAH ( 3 )
               Grid_Data%WRES( C,R ) = WRES_NOAH( 3 )
               Grid_Data%BSLP( C,R ) = BSLP_NOAH( 3 )
               Grid_Data%RHOB( C,R ) = RHOB_NOAH( 3 )
            End Forall

         Else If( Noah_LSM ) Then
            Write(Logdev,*) 'MCIP/WRF used the Noah LSM'
            zsoil1 = 0.01 ! Adjusted based on Darmenova et al. 2009 doi:10.1029/2008JD011236
            zsoil2 = 0.05 ! For abflux estimated from 1cm and 1 meter 
            Forall( C = 1:MY_NCOLS, R = 1:MY_NROWS )
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
            Forall( C = 1:MY_NCOLS, R = 1:MY_NROWS )
               Grid_Data%WSAT( C,R ) = WSAT_CLM( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WWLT( C,R ) = WWLT_CLM( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WFC ( C,R ) = WFC_CLM ( Grid_Data%SLTYP( C,R ) )
               Grid_Data%WRES( C,R ) = WRES_CLM( Grid_Data%SLTYP( C,R ) )
               Grid_Data%BSLP( C,R ) = BSLP_CLM( Grid_Data%SLTYP( C,R ) )
               Grid_Data%RHOB( C,R ) = RHOB_CLM( Grid_Data%SLTYP( C,R ) )
            End Forall                            
        End If

!> Read fractional seawater and surf-zone coverage from the OCEAN file.
!> Store results in the OCEAN and SZONE arrays.
         IF ( .NOT. OPEN3( OCEAN_1, FSREAD3, PNAME ) ) THEN
            XMSG = 'Open failure for ' // OCEAN_1
            CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'OPEN'
         If ( .Not. INTERPX( OCEAN_1, VNAME, PNAME,
     &                       STRTCOL_O1,ENDCOL_O1, STRTROW_O1,ENDROW_O1,
     &                       1,1,JDATE, JTIME, Grid_Data%OCEAN ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // OCEAN_1
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'SURF'
         If ( .Not. INTERPX( OCEAN_1, VNAME, PNAME,
     &                       STRTCOL_O1,ENDCOL_O1, STRTROW_O1,ENDROW_O1,
     &                       1,1,JDATE, JTIME, Grid_Data%SZONE ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // OCEAN_1
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

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
         USE VGRD_DEFN, Only: VGTYP_GD
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
         Real, Pointer    :: Es_Grnd ( :,: )
         Real, Pointer    :: Es_Air  ( :,: )
         Real, Pointer    :: TV      ( :,:,: )
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
      
         VNAME = 'ZH'
         If ( .Not. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       JDATE, JTIME, Met_Data%ZH ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'PRES'
         If ( .Not. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       JDATE, JTIME, Met_Data%PRES ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If 

         VNAME = 'ZF'
         If ( .Not. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       JDATE, JTIME, Met_Data%ZF ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If            

         VNAME = 'DENS'
         If ( .Not. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       JDATE, JTIME, Met_Data%DENS ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT 1 )
         End If

         Met_Data%DENS1 = Met_Data%DENS( :,:,1 )
     
         VNAME = 'JACOBM'
         If ( .Not. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       JDATE, JTIME, Met_Data%RJACM ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         Met_Data%RJACM = 1.0 / Met_Data%RJACM

         VNAME = 'JACOBF'
         If ( .Not. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       JDATE, JTIME, Met_Data%RJACF ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         Met_Data%RJACF = 1.0 / Met_Data%RJACF

         VNAME = 'DENSA_J'
         If ( .Not. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       JDATE, JTIME, Met_Data%RRHOJ ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         Met_Data%RRHOJ = 1.0 / Met_Data%RRHOJ

         VNAME = 'TA'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       JDATE, JTIME, Met_Data%TA ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'QV'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       JDATE, JTIME, Met_Data%QV ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VNAME = 'QC'
         IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                       JDATE, JTIME, Met_Data%QC ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C-------------------------------- MET_CRO_2D --------------------------------
C Vegetation and surface vars
         VNAME = 'LAI'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%LAI ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'VEG'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%VEG ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'ZRUF'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%Z0 ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

C Soil vars
         VNAME = 'SOIM1'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%SOIM1 ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If         

         VNAME = 'SOIM2'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%SOIM2 ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'SOIT2'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%SOIT2 ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If         

         VNAME = 'SOIT1'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%SOIT1 ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'SEAICE'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%SEAICE ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

C met vars

         VNAME = 'PRSFC'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%PRSFC ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'RGRND'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%RGRND ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'SNOCOV'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%SNOCOV ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If
         Where( Met_Data%SNOCOV .Lt. 0.0 )
            Met_Data%SNOCOV = 0.0
         End Where

         VNAME = 'TEMP2'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%TEMP2 ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'TEMPG'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%TEMPG ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'USTAR'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%USTAR ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'WSPD10'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%WSPD10 ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         VNAME = 'HFX'
         If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%HFX ) ) Then
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         If ( iflh ) Then
            VNAME = 'LH'
            If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                          STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                          JDATE, JTIME, Met_Data%LH ) ) Then
               XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If
         Else   ! for backward compatibility
            VNAME = 'QFX'
            If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%LH ) ) Then
               XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If
         End If

         VNAME = 'PBL'
         IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%PBL ) ) THEN
            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C Met_cro_2D variables that have recently changed due to MCIP or WRF/CMAQ

         If ( .Not. INTERPX( MET_CRO_2D, vname_rn, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%RN ) ) Then
            XMSG = MSG1 // TRIM( vname_rn ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         If ( .Not. INTERPX( MET_CRO_2D, vname_rc, PNAME,
     &                       STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                       JDATE, JTIME, Met_Data%RC ) ) Then
            XMSG = MSG1 // TRIM( vname_rc ) // ' from ' // MET_CRO_2D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If         

         If ( ifwr ) Then
            VNAME = 'WR'
            If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                          STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                          JDATE, JTIME, Met_Data%WR ) ) Then
               XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If
         End If

         If ( ifsst ) Then
            VNAME = 'TSEASFC'
            If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                          STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                          JDATE, JTIME, Met_Data%TSEASFC ) ) Then
               XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If
         Else
            Met_Data%TSEASFC = Met_Data%TEMPG
         End If

         If ( rinv ) Then
            VNAME = 'RADYNI'
            If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                          STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                          JDATE, JTIME, Met_Data%RA ) ) Then
               XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If

            Where( Met_Data%RA .Gt. cond_min ) 
               Met_Data%RA = 1.0/Met_Data%RA
            Elsewhere
               Met_Data%RA = resist_max
            End Where

            VNAME = 'RSTOMI'
            If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                          STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                          JDATE, JTIME, Met_Data%RS ) ) Then
               XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If

            Where( Met_Data%RS .Gt. cond_min ) 
               Met_Data%RS = 1.0 / Met_Data%RS
            Elsewhere 
               Met_Data%RS = resist_max
            End Where

         Else 

            VNAME = 'RA'
            If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                          STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                          JDATE, JTIME, Met_Data%RA ) ) Then
               XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If

            VNAME = 'RS'
            If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                          STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                          JDATE, JTIME, Met_Data%RS ) ) Then
               XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If

         End If

         If ( ifq2 ) Then  ! Q2 in METCRO2D
            VNAME = 'Q2'
            If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                          STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                          JDATE, JTIME, Met_Data%Q2 ) ) Then
               XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_2D
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If
         Else
            Met_Data%Q2 = Met_Data%QV( :,:,1 )
         End If
         
         Es_Grnd => BUFF2D
         Where( Met_Data%TEMPG .Lt. stdtemp )
            Es_Grnd = vp0 *Exp( 22.514 - ( 6.15e3 / Met_Data%TEMPG ) )
         Elsewhere
            Es_Grnd = vp0 *Exp( svp2 * ( Met_Data%TEMPG -stdtemp ) / ( Met_Data%TEMPG -svp3 ) ) 
         End Where
         Met_Data%QSS_GRND = Es_Grnd * 0.622 / ( Met_Data%PRSFC - Es_Grnd )
         Nullify( Es_Grnd )

         Es_Air => BUFF2D
         Where( Met_Data%TEMP2 .Lt. stdtemp )
            Es_Air = vp0 *Exp( 22.514 - ( 6.15e3 / Met_Data%TEMP2 ) )
         Elsewhere
            Es_Air = vp0 *Exp( svp2 * ( Met_Data%TEMP2 -stdtemp ) / ( Met_Data%TEMP2 -svp3 ) ) 
         End Where
         Met_Data%RH = Met_Data%Q2 / ( Es_Air * 0.622 / ( Met_Data%PRSFC - Es_Air ) ) * 100.0
         Where( Met_Data%RH .Gt. 100.0 )
            Met_Data%RH = 100.0
         Elsewhere( Met_Data%RH .lt. 0.0 )
            Met_Data%RH = 0.0
         End Where
         Nullify( Es_Air )

C-------------------------------- MET_DOT_3D --------------------------------
         If ( .Not. INTERPX( MET_DOT_3D, vname_uc, PNAME,
     &                       STRTCOLMD3,ENDCOLMD3, STRTROWMD3,ENDROWMD3, 1,NLAYS,
     &                       JDATE, JTIME, Met_Data%UWIND ) ) Then
            XMSG = MSG1 // TRIM( vname_uc ) // ' from ' // MET_DOT_3D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT 1 )
         End If

         If ( .Not. INTERPX( MET_DOT_3D, vname_vc, PNAME,
     &                       STRTCOLMD3,ENDCOLMD3, STRTROWMD3,ENDROWMD3, 1,NLAYS,
     &                       JDATE, JTIME, Met_Data%VWIND ) ) Then
            XMSG = MSG1 // TRIM( vname_vc ) // ' from ' // MET_DOT_3D
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT 1 )
         End If

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

         TV => BUFF3D
         TV = Met_Data%TA * ( 1.0 + 0.608 * Met_Data%QV )
         Met_Data%THETAV = TV * ( P0 / Met_Data%PRES ) ** 0.286
         Nullify( TV )

C------  Updating MOL, then WSTAR, MOLI, HOL 
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
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
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
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

!         VNAME = 'LAI'
!         IF ( .NOT. INTERPX( TILE_CRO_2D, VNAME, PNAME,
!     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,n_lufrac,
!     &                       JDATE, JTIME,  MOSAIC_DATA%LAI ) ) THEN
!            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
         CALL CALC_LAI( JDATE, JTIME, MET_DATA%SOIT2, GRID_DATA%LUFRAC,
     &                  MET_DATA%LAI, MOSAIC_DATA%VEG, MOSAIC_DATA%LAI, MOSAIC_DATA%Z0 )
!         END IF

!         VNAME = 'VEG'
!         IF ( .NOT. INTERPX( TILE_CRO_2D, VNAME, PNAME,
!     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,n_lufrac,
!     &                       JDATE, JTIME,  MOSAIC_DATA%VEG ) ) THEN
!            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
!         END IF

!         VNAME = 'Z0'
!         IF ( .NOT. INTERPX( TILE_CRO_2D, VNAME, PNAME,
!     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,n_lufrac,
!     &                       JDATE, JTIME,  MOSAIC_DATA%Z0 ) ) THEN
!            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
!         END IF

         Do L = 1, n_lufrac
            Where( GRID_DATA%LUFRAC( :,:,L ) .EQ. 1.0 )
                   MOSAIC_DATA%USTAR( :,:,L ) = MET_DATA%USTAR
                   MOSAIC_DATA%Z0( :,:,L )    = MET_DATA%Z0
            Elsewhere( GRID_DATA%LUFRAC( :,:,L ) .Gt. 0.0 )
                   MOSAIC_DATA%USTAR( :,:,L ) = MET_DATA%USTAR * SQRT( LOG( MET_DATA%ZH( :,:,1 ) / MET_DATA%Z0 )
     &                                        / LOG( MET_DATA%ZH( :,:,1 ) / MOSAIC_DATA%Z0( :,:,L ) ) )
            End Where
            MOSAIC_DATA%WSTAR = 0.0
            Where( GRID_DATA%LUFRAC( :,:,L ) .EQ. 1.0 )
                   MOSAIC_DATA%WSTAR( :,:,L ) = MET_DATA%WSTAR
            ElseWhere( Met_Data%MOL .Lt. 0.0 )
                   MOSAIC_DATA%WSTAR( :,:,L ) = MOSAIC_DATA%USTAR( :,:,L ) * ( Met_Data%PBL
     &                                  / ( karman * ABS( Met_Data%MOL ) ) ) ** 0.333333
            End Where
         End Do  
!         END IF

!         VNAME = 'MOS_RA'
!         IF ( .NOT. INTERPX( TILE_CRO_2D, VNAME, PNAME,
!     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,n_lufrac,
!     &                       JDATE, JTIME,  MOSAIC_DATA%RA ) ) THEN
!            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
         SELECT CASE( vgtyp_gd )
            CASE( 7 )
               CALL ra_wrf( MET_DATA%MOLI, MET_DATA%ZH, MET_DATA%Z0, GRID_DATA%LWMASK, MET_DATA%VEG,
     &                      GRID_DATA%LUFRAC, MET_DATA%RA, MOSAIC_DATA%Z0, MOSAIC_DATA%USTAR, MOSAIC_DATA%RA, 
     &                      gamah, betah, karman )
            CASE( 2 )
               CALL ra_mm5( MET_DATA%UWIND, MET_DATA%VWIND, GRID_DATA%LWMASK, MET_DATA%SNOCOV, 
     &                      MET_DATA%SOIM1, GRID_DATA%WFC, MET_DATA%QV, MET_DATA%Q2, MET_DATA%QSS_GRND, 
     &                      MET_DATA%TEMPG, MET_DATA%PRSFC, Met_Data%THETAV, MET_DATA%ZH, 
     &                      GRID_DATA%LUFRAC, MET_DATA%VEG, MET_DATA%RA, MOSAIC_DATA%Z0, 
     &                      MOSAIC_DATA%USTAR, MOSAIC_DATA%RA, karman )
            CASE DEFAULT
               xmsg = 'met file format not recognized for mosaic'
               CALL m3exit( pname, jdate, jtime, xmsg, xstat1 )
         END SELECT
!         END IF
!         VNAME = 'MOS_RST'
!         IF ( .NOT. INTERPX( TILE_CRO_2D, VNAME, PNAME,
!     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,n_lufrac,
!     &                       JDATE, JTIME,  MOSAIC_DATA%RSTW ) ) THEN
!            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
         CALL MOS_RSTW( GRID_DATA%LUFRAC, GRID_DATA%LWMASK, MOSAIC_DATA%LAI, Met_Data%RGRND, Met_Data%SOIM2, 
     &                  Grid_Data%WWLT, Grid_Data%WFC, Met_Data%TEMP2, MOSAIC_DATA%RA, MOSAIC_DATA%USTAR, 
     &                  MET_DATA%QSS_GRND, MET_DATA%QV, Met_Data%RS, Mosaic_Data%RSTW )
!         END IF

!         END IF
!         VNAME = 'MOS_WR'
!         IF ( .NOT. INTERPX( TILE_CRO_2D, VNAME, PNAME,
!     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,n_lufrac,
!     &                       JDATE, JTIME,  MOSAIC_DATA%DELTA ) ) THEN
!            XMSG = MSG1 // TRIM( VNAME ) // ' from ' // MET_CRO_3D
         CALL MOS_CanWat( GRID_DATA%LUFRAC, MOSAIC_DATA%VEG, MOSAIC_DATA%LAI, Met_Data%WR, Mosaic_Data%DELTA )
!         END IF

!        Calculate the cosine of the zenith angle 
         CALL CZANGLE( JDATE, JTIME, MY_NCOLS, MY_NROWS )

         Return
         End Subroutine GET_MET

      End Module ASX_DATA_MOD
