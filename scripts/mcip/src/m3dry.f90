
!***********************************************************************
!   Portions of Models-3/CMAQ software were developed or based on      *
!   information from various groups: Federal Government employees,     *
!   contractors working on a United States Government contract, and    *
!   non-Federal sources (including research institutions).  These      *
!   research institutions have given the Government permission to      *
!   use, prepare derivative works, and distribute copies of their      *
!   work in Models-3/CMAQ to the public and to permit others to do     *
!   so.  EPA therefore grants similar permissions for use of the       *
!   Models-3/CMAQ software, but users are requested to provide copies  *
!   of derivative works to the Government without restrictions as to   *
!   use by others.  Users are responsible for acquiring their own      *
!   copies of commercial software associated with Models-3/CMAQ and    *
!   for complying with vendor requirements.  Software copyrights by    *
!   the MCNC Environmental Modeling Center are used with their         *
!   permissions subject to the above restrictions.                     *
!***********************************************************************

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/work/rep/MCIP2/src/mcip2/m3dry.F,v 1.8 2007/08/03 20:53:37 tlotte Exp $ 


SUBROUTINE m3dry (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Models-3 Dry Deposition
! Purpose:  Computes dry deposition velocities using Rst and Ra, and
!           elements of ADOM DD model.
! Revised:  21 Jan 1998  Original version.  (J. Pleim and A. Bourgeois)
!           18 Sep 2001  Converted to free-form f90.  Modified significant
!                        sections of code.  Added characteristics for
!                        dry deposition of atrazine and atrazine product.
!                        Made general for USGS 24-category system.  
!                        (T. Otte, J. Pleim, and W. Hutzell)
!           14 Jan 2002  Added temperature dependence to Henry's Law
!                        constants.  Added temperature and pressure
!                        dependence to diffusivity.  Added new dry
!                        deposition species, methanol.  (Y. Wu and T. Otte)
!           18 Jan 2002  Changed the reference wet cuticle resistance.
!                        (J. Pleim)
!           23 Jan 2002  Changed missing value on XRADYN, XRBNDY, and XRSTOM
!                        to BADVAL3.  (T. Otte)
!           26 Feb 2002  Changed logic to define water point using dominant
!                        land use category.  (T. Otte)
!           09 Jun 2003  Added logic for modeling snow covered surfaces.
!                        Changed the reactivities for SO2, HNO3 and NH3.
!                        Changed pH values to have an east-west variation.
!                        Using the Henry's law constant function from CMAQ in
!                        place of local code.  Also changed the code for
!                        deposition to water to use a pH of 8.1 and the
!                        temperature of water in calculating the Henry's law
!                        constant.  Adjusted values of RSNOW0 = 1000 and
!                        A(NH3) = 20.  Added new dry deposition species: N2O5,
!                        NO3, Generic_aldehyde.  Corrected diffusivities of
!                        chemicals and water and viscosity of air to all be at
!                        the same temperature (273.15K).  Temperature and
!                        pressure adjustments to the values are not needed
!                        because the diffusivities and viscosity are always used
!                        as ratios, so the temperature-pressure dependence was
!                        removed.  Removed dry deposition species, ATRA and
!                        ATRAP, from output.  (D. Schwede, J. Pleim, and
!                        T. Otte)
!           04 Aug 2004  Added provisions to prevent negative dry deposition
!                        velocities from occurring in output.  Removed XFLAGS.
!                        Changed XWIND10 to XWSPD10.  (T. Otte)
!           28 Feb 2005  Added optional dry deposition species for chlorine
!                        and mercury.  (G. Sarwar, R. Bullock, and T. Otte)
!           21 Jul 2005  Added provision to alternatively use XPBL to check for
!                        meteorology model initialization time.  (T. Otte)
!           11 Aug 2005  Replaced species pointers from XDEPIDX with species
!                        names from XDEPSPC.  Changed XRH to a local scalar.
!                        Moved declarations of A, ALPHS, DIF0, DKHOR, KH, and
!                        SUBNAME to DEPVVARS_MOD.  (T. Otte and W. Hutzell)
!           02 Feb 2006  Added mesophyll resistance to dry deposition velocity
!                        calculation, and defined non-zero value for mercury.
!                        (D. Schwede, J. Pleim, and R. Bullock)
!           18 Aug 2006  Optimized calculations of constants that use
!                        exponential computations by adapting ideas from
!                        PREMAQ in AQF system.  Corrected bugs in calculation
!                        of XLSTWET for non-PX runs.  Removed dependency on
!                        module LRADMDAT.  Moved XDELTA and XLSTWET to be
!                        local variables rather than part of module XVARS.
!                        Removed unused variables H, RCI_OLD, RGWET0, and
!                        RSURF_OLD.  Added SDATE and STIME to argument list.
!                        Use TG instead of TA in non-PX WR, and ramp down
!                        DELTA to 0 between 2 and 3 h.  Use land-water mask
!                        array rather than dominant land use array to determine
!                        water points.  (T. Otte)
!           01 Aug 2007  Added a non-zero mesophyll resistance for NO, NO2, and
!                        CO.  Restored wet cuticle resistance for O3 based on
!                        field study measurements.  Added wet ground resistance.
!                        Changed ground resistance to include partitioning of
!                        wet and dry ground.  Updated pH of rain water for
!                        eastern United States and outside of North America.
!                        Changed reactivity for PAN.  Removed dry deposition
!                        velocity calculations for obsolete chlorine species
!                        ICL1 and ICL2.  Corrected error in the calculation of
!                        surface resistance over water where (Sc/Pr)**(2/3) had
!                        been inadvertently omitted from the numerator.
!                        Surface resistance over water is now a function of
!                        species.  Surface resistance over water now uses wet
!                        bulb temperature rather than ground (water) temperature
!                        in the calculation of the effective Henry's law
!                        constant, and the algorithm has been updated.  Changed
!                        (Sc/Pr)**(2/3) over water to a species-dependent,
!                        meteorologically dependent variable.  Effective Henry's
!                        law constant over land now uses 2-m temperature rather
!                        than layer 1 temperature.  Changed XUSTAR and XRADYN
!                        to 2D arrays without a dimension for fractional land
!                        use that was required for RADMdry.  Removed XRBNDY and
!                        RBW.  Changed near-surface air temperature from
!                        (computed) 1.5-m temperature to 2-m temperature in
!                        near-ground moisture calculations.  Moved call to
!                        RESISTCALC to PBLSUP.  Removed references to logical
!                        variable "PX" to make code more general.  Changed ES
!                        into ES_AIR and ES_GRND, and changed QSS into QSS_AIR
!                        and QSS_GRND to clarify usage.  (J. Pleim, E. Cooter,
!                        J. Bash, T. Otte, and G. Sarwar)
!           30 May 2008  Changed the value of D3, the scaling parameter used to
!                        estimate the friction velocity in surface waters from
!                        the atmospheric friction velocity to a value following
!                        Slinn et al. (1978) and Fairall et al. (2007).  Added
!                        five air toxic species to output.  Added an exception
!                        for elemental mercury deposition to wet soil surfaces
!                        as most measurements show evasion from recently wet
!                        soil surfaces.  Changed calculations to use 2-m mixing
!                        ratio rather than layer-1 mixing ratio (even though
!                        they could be the same) because Q2 is more
!                        appropriate.  Added a trap for undefined dry
!                        deposition velocities (e.g., NaN).  Updated some
!                        comments.  (J. Bash, W. Hutzell, and T. Otte)
!           29 Oct 2009  Added condition that vegetation fraction must be
!                        greater than zero to be considered a land point.  This
!                        works around intermittent inconsistencies in surface
!                        fields in some WRF data sets.  Changed read number
!                        comparisons from "equivalences" to "less than
!                        tolerances".  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE depvvars
  USE const
  USE parms3, ONLY: badval3
  USE metinfo

  IMPLICIT NONE

  REAL,          PARAMETER     :: a0         = 8.0     ! [dim'less]
  INTEGER                      :: c
  REAL                         :: cpair                ! specific ht of moist air
  REAL,          PARAMETER     :: d3         = 1.38564e-2 ! k*(rhoA/rhoW)^0.5
  REAL, SAVE,    ALLOCATABLE   :: delta      ( : , : )
  REAL                         :: dw
  REAL                         :: dw25                 ! diffu. of H2O @ 25C
  REAL,          PARAMETER     :: dwat       = 0.2178  ! [cm^2 / s] at 273.15K
  LOGICAL                      :: effective            ! calc eff. Henry's Law const?
  INTEGER                      :: elapsedsec
  REAL                         :: es_air
  REAL                         :: es_grnd
  LOGICAL                      :: first      = .TRUE.
  REAL                         :: hcan
  REAL                         :: heff                 ! effective Henry's Law constant
  REAL,          EXTERNAL      :: hlconst              ! [M / atm]
  REAL                         :: hplus
  INTEGER                      :: ifsnow               ! 1=snow
  REAL,          PARAMETER     :: kvis       = 0.132   ! [cm^2 / s] at 273.15K
  REAL                         :: kvisw                ! kinem. visc. of H2O
  INTEGER                      :: l
  INTEGER, SAVE, ALLOCATABLE   :: lstwetdate ( : , : )
  INTEGER, SAVE, ALLOCATABLE   :: lstwettime ( : , : )
  REAL                         :: lv                   ! latent heat of vap'n
  CHARACTER*16,  PARAMETER     :: pname      = 'M3DRY'
  REAL,          PARAMETER     :: pr         = 0.709   ! [dim'less]
  REAL                         :: q2                   ! 2-m mixing ratio
  REAL                         :: qss_air
  REAL                         :: qss_grnd
  INTEGER                      :: r
  REAL                         :: rac
  REAL                         :: rbc
  REAL                         :: rbsulf
  REAL                         :: rci
  REAL                         :: rcut
  REAL,          PARAMETER     :: rcut0      = 3000.0  ! [s/m]
  REAL,          PARAMETER     :: rcw0       = 125000.0 ! acc'd'g to Padro and
                                                       ! adapted from Slinn 78
  REAL,          PARAMETER     :: rg0        = 1000.0  ! [s/m]
  REAL                         :: rgnd
  REAL                         :: rgndc
  REAL                         :: rgw                  ! resist for water-covered sfc
  REAL,          PARAMETER     :: rgwet0     = 25000.0 ! [s/m]
  REAL                         :: rh_air               ! rel humidity (air)
  REAL                         :: rh_grnd              ! rel humidity (ground)
  REAL                         :: rinc
  REAL,          PARAMETER     :: rsndiff    = 10.0    ! snow diffusivity fac
  REAL                         :: rsnow
  REAL,          PARAMETER     :: rsnow0     = 1000.0
  REAL                         :: rstom
  REAL                         :: rsurf
  REAL,          PARAMETER     :: rt25ink    = 1.0/(stdtemp + 25.0) ! 1/25C in K
  REAL                         :: rwet       ! wet sfc resist (cuticle or grd)
  REAL                         :: rwetsfc
  REAL,          PARAMETER     :: scw        = 0.6060606 ! kvis / dwat
  REAL                         :: scw_pr_23              ! (kvisw/dw/Pr)**(2/3)
  INTEGER,       INTENT(IN)    :: sdate
  INTEGER,       EXTERNAL      :: secsdiff
  REAL,          PARAMETER     :: smallnum   = 1.0e-7
  INTEGER,       INTENT(IN)    :: stime
  REAL,          PARAMETER     :: svp2       = 17.67   ! from MM5 and WRF
  REAL,          PARAMETER     :: svp3       = 29.65   ! from MM5 and WRF
  REAL                         :: tw                   ! wet bulb temperature
  REAL,          PARAMETER     :: twothirds  = 2.0/3.0
  REAL                         :: wrmax
  REAL                         :: xm                   ! liquid water mass frac

  REAL,          PARAMETER     :: hplus_def  = 1.0e-5      ! 10**(-5.0) ! pH=5.0
  REAL,          PARAMETER     :: hplus_east = 1.0e-5      ! 10**(-5.0) ! pH=5.0
  REAL,          PARAMETER     :: hplus_h2o  = 7.94328e-9  ! 10**(-8.1) ! pH=8.1
  REAL,          PARAMETER     :: hplus_west = 3.16228e-6  ! 10**(-5.5) ! pH=5.5

!-------------------------------------------------------------------------------
! Soil Characteristics by Type
!
!   #  SOIL TYPE  WSAT  WFC  WWLT    B   CGSAT   JP   AS   C2R  C1SAT
!   _  _________  ____  ___  ____  ____  _____   ___  ___  ___  _____
!   1  SAND       .395 .135  .068  4.05  3.222    4  .387  3.9  .082
!   2  LOAMY SAND .410 .150  .075  4.38  3.057    4  .404  3.7  .098
!   3  SANDY LOAM .435 .195  .114  4.90  3.560    4  .219  1.8  .132
!   4  SILT LOAM  .485 .255  .179  5.30  4.418    6  .105  0.8  .153
!   5  LOAM       .451 .240  .155  5.39  4.111    6  .148  0.8  .191
!   6  SND CLY LM .420 .255  .175  7.12  3.670    6  .135  0.8  .213
!   7  SLT CLY LM .477 .322  .218  7.75  3.593    8  .127  0.4  .385
!   8  CLAY LOAM  .476 .325  .250  8.52  3.995   10  .084  0.6  .227
!   9  SANDY CLAY .426 .310  .219 10.40  3.058    8  .139  0.3  .421
!  10  SILTY CLAY .482 .370  .283 10.40  3.729   10  .075  0.3  .375
!  11  CLAY       .482 .367  .286 11.40  3.600   12  .083  0.3  .342
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!-- Chemical-Dependent Parameters (Original Source: Modified ADOM - Padro)
!
!                                                          at 298.15 K
!     Species   Dif0(cm2/s) Alphastar   Reactivity     -DKHOR [K]  KH0 [M/atm]
!     _______   ___________ _________   __________     _________  __________
!  1   SO2      0.1089 ~    1000.       8.00              3100.0 ~     1.2e00 ~
!  2   SULF      --         --           --                --           --
!  3   NO2      0.1361 ~    1.00        8.00 2.00*        2500.0 ~     1.2e-2 ~
!  4   NO       0.1802 ~    1           2.0+              1500.0 ~     1.9e-3 ~
!  5   O3       0.1444 ~    10.00       15.0  8@          2700.0 ~     1.2e-2 ~
!  6   HNO3     0.1628 ~    1E9         18.0 800* 8000**  8700.0 ~     2.6e+6 ~
!  7   H2O2     0.2402 ~    1.00        12.0 30*          6600.0 ~     9.7e+4 ~
!  8   ACET ALD 0.1525 ~    1           10.+              5700.0 ~     1.3e+1 ~
!  9   HCHO     0.1877 ~    1.00        10.+              6400.0 ~     7.0e+3 ~
! 10   OP       0.1525 ~    1           10.0+             5200.0 ~     3.1e+2 ~
! 11   PAA      0.1220 ~    1           20+               5300.0 ~     8.4e+2 ~
! 12   ORA      0.1525 ~    1           20+               5700.0 ~     3.7e+3 ~
! 13   NH3      0.1978 ~    1E5         10.0              4100.0 ~     5.8e+1 ~
! 14   PAN      0.0938 ~    1           16.0~~            5900.0 ~     2.9e00 ~
! 15   HONO     0.1525 ~    1           20+               4800.0 ~     4.9e+1 ~
! 16   CO       0.1807 ~    1           5.0               1600.0 ~     9.5e-4 ~
! 17   METHANOL 0.1363 ~    1.0 ~       2.0 ~             5200.0 ~     2.2e+2 ~
! --   CO2      0.1381 ~                                  2400.0 ~     3.4e-2 ~
! 18   N2O5     0.0808 ^^               5000.0**
! 19   NO3      0.1153 ^^               5000.0**
! 20   GEN ALD  0.1525 ##   1.0         10.0##
! 21   CL2      0.1080 %                10.0 %
! 22   HOCL     0.1300 %                10.0 %
! 23   HCL      0.1510 %                8000.0 %
! 24   FMCL     0.1094 %                10.0 %
! 25   HG       0.1194 $                0.1 $
! 26   HGIIGAS  0.0976 $                8000.0 $
! 27   HEXAMETHYLENE DIISOCYANATE        10.0 :)
! 28   HYDRAZINE                         20.0 :)
! 29   MALEIC ANHYDRIDE                  10.0 :)
! 30   TOLUENE DIISOCYANATE              10.0 :)
! 31   TRIETHYLAMINE                     10.0 :)
!
!---------Notes
!  * Updates based on literature review 7/96 JEP
!  # Diff and H based on Wesely (1988) same as RADM
!  + Estimated by JEP 2/97 
!  @ Updated by JEP 9/01
!  ~ Added by YW 1/02.  Dif0 based on Massman (1998).  Henry's Law constant
!    is defined here as: h=cg/ca, where cg is the concentration of a species
!    in gas-phase, and ca is its aqueous-phase concentration.  The smaller h,
!    the larger solubility.  Henry's Law constant in another definition (KH):
!    KH = ca/pg [M/atm], KH = KH0 * exp(-DKH/R(1/T-1/T0)), where KH0 and -DKH
!    values are from Rolf Sander (1999).  h=1/(KH*R*T).
! ** Update by DBS based on estimates by JEP 1/03
! ^^ From Bill Massman, personal communication 4/03
! ## Diffusivity calculated by SPARC, reactivity = other aldehydes
! ++ Dif0 in Massman is diffusivity at temperature 0C and 1 atm (101.325kPa), so
!    chemicals that were not in Massman's paper need to be adjusted.  We assume
!    JEP's original values were for 25C and 1 atm.
!  % Added by G. Sarwar (10/04)
!  $ Added by R. Bullock (02/05) HG diffusivity is from Massman (1999).  
!    HGIIGAS diffusivity calculated from the HG value and a mol. wt. scaling 
!    factor of MW**(-2/3) from EPA/600/3-87/015. ORD, Athens, GA.  HGIIGAS 
!    mol.wt. used is that of HgCl2.  Reactivity of HG is 1/20th of NO and NO2 
!    values based on general atmospheric lifetimes of each species.  Reactivity 
!    of HGIIGAS is based on HNO3 surrogate.
! @@ Mesophyll resistances for NO, NO2, and CO added by J. Pleim (07/07) based
!    on values in Pleim, Venkatram, and Yamartino, 1984:  ADOM/TADAP Model
!    Development Program, Volume 4, The Dry Deposition Module.  ERT, Inc.,
!    Concord, MA (peer reviewed).
! ~~ Reactivity for PAN changed from 4.0 to 16.0 by J. Pleim (07/07) based on
!    comparisons with Turnipseed et al., JGR, 2006.
! %% Species ICL1 and ICL2 are not used in CB05.  G. Sarwar (07/07)
! :) Hazardous Air Pollutants that are believed to undergo significant dry
!    deposition. Hydrazine and triethylamine reactivities are based on analogies
!    to NH3. Maleic anhydride reactivity is assumed similar to aldehydes. 
!    Toluene diisocyanate and hexamethylene diisocyanate reactivities are
!    assumed to be similar to SO2. Diffusivities are calculated with standard
!    formulas.  W. Hutzell (04/08)
!-------------------------------------------------------------------------------

  subname( 1) = 'SO2             '  ;  dif0( 1) = 0.1089  ;  a( 1) =   10.0  ;  meso( 1) = 0.0
  subname( 2) = 'SULFATE         '  ;  dif0( 2) = 0.0000  ;  a( 2) =    0.0  ;  meso( 2) = 0.0
  subname( 3) = 'NO2             '  ;  dif0( 3) = 0.1361  ;  a( 3) =    2.0  ;  meso( 3) = 500.0
  subname( 4) = 'NO              '  ;  dif0( 4) = 0.1802  ;  a( 4) =    2.0  ;  meso( 4) = 9400.0
  subname( 5) = 'O3              '  ;  dif0( 5) = 0.1444  ;  a( 5) =    8.0  ;  meso( 5) = 0.0
  subname( 6) = 'HNO3            '  ;  dif0( 6) = 0.1067  ;  a( 6) = 8000.0  ;  meso( 6) = 0.0
  subname( 7) = 'H2O2            '  ;  dif0( 7) = 0.1300  ;  a( 7) =   30.0  ;  meso( 7) = 0.0
  subname( 8) = 'ACETALDEHYDE    '  ;  dif0( 8) = 0.1111  ;  a( 8) =   10.0  ;  meso( 8) = 0.0
  subname( 9) = 'FORMALDEHYDE    '  ;  dif0( 9) = 0.1554  ;  a( 9) =   10.0  ;  meso( 9) = 0.0
  subname(10) = 'METHYLHYDROPEROX'  ;  dif0(10) = 0.1179  ;  a(10) =   10.0  ;  meso(10) = 0.0
  subname(11) = 'PEROXYACETIC_ACI'  ;  dif0(11) = 0.0868  ;  a(11) =   20.0  ;  meso(11) = 0.0
  subname(12) = 'ACETIC_ACID     '  ;  dif0(12) = 0.0944  ;  a(12) =   20.0  ;  meso(12) = 0.0
  subname(13) = 'NH3             '  ;  dif0(13) = 0.1978  ;  a(13) =   20.0  ;  meso(13) = 0.0
  subname(14) = 'PAN             '  ;  dif0(14) = 0.0687  ;  a(14) =   16.0  ;  meso(14) = 0.0
  subname(15) = 'HNO2            '  ;  dif0(15) = 0.1349  ;  a(15) =   20.0  ;  meso(15) = 0.0
  subname(16) = 'CO              '  ;  dif0(16) = 0.1807  ;  a(16) =    5.0  ;  meso(16) = 100000.0
  subname(17) = 'METHANOL        '  ;  dif0(17) = 0.1329  ;  a(17) =    2.0  ;  meso(17) = 0.0
  subname(18) = 'N2O5            '  ;  dif0(18) = 0.0808  ;  a(18) = 5000.0  ;  meso(18) = 0.0
  subname(19) = 'NO3             '  ;  dif0(19) = 0.1153  ;  a(19) = 5000.0  ;  meso(19) = 0.0
  subname(20) = 'GENERIC_ALDEHYDE'  ;  dif0(20) = 0.0916  ;  a(20) =   10.0  ;  meso(20) = 0.0
  subname(21) = 'CL2             '  ;  dif0(21) = 0.1080  ;  a(21) =   10.0  ;  meso(21) = 0.0
  subname(22) = 'HOCL            '  ;  dif0(22) = 0.1300  ;  a(22) =   10.0  ;  meso(22) = 0.0
  subname(23) = 'HCL             '  ;  dif0(23) = 0.1510  ;  a(23) = 8000.0  ;  meso(23) = 0.0
  subname(24) = 'FMCL            '  ;  dif0(24) = 0.1094  ;  a(24) =   10.0  ;  meso(24) = 0.0
  subname(25) = 'HG              '  ;  dif0(25) = 0.1194  ;  a(25) =    0.1  ;  meso(25) = 5000.0
  subname(26) = 'HGIIGAS         '  ;  dif0(26) = 0.0976  ;  a(26) = 8000.0  ;  meso(26) = 0.0
  subname(27) = 'HEXAMETHYLE_DIIS'  ;  dif0(27) = 0.0380  ;  a(27) = 10.0    ;  meso(27) = 0.0
  subname(28) = 'HYDRAZINE       '  ;  dif0(28) = 0.4164  ;  a(28) = 20.0    ;  meso(28) = 0.0
  subname(29) = 'MALEIC_ANHYDRIDE'  ;  dif0(29) = 0.0950  ;  a(29) = 10.0    ;  meso(29) = 0.0
  subname(30) = '2,4-TOLUENE_DIIS'  ;  dif0(30) = 0.0610  ;  a(30) = 10.0    ;  meso(30) = 0.0
  subname(31) = 'TRIETHYLAMINE   '  ;  dif0(31) = 0.0881  ;  a(31) = 20.0    ;  meso(31) = 0.0

  IF ( first ) THEN
    DO l = 1, ltotg
      IF ( dif0(l) > 0.0 ) THEN
        scc_pr_23(l) = ( (kvis / dif0(l)) / pr )**twothirds
      ELSE
        scc_pr_23(l) = 0.0
      ENDIF
    ENDDO
    first = .FALSE.
  ENDIF

!-------------------------------------------------------------------------------
! For the time period that corresponds to a meteorology model initialization
! time, many PBL variables are not defined.  At the initialization time for the
! meteorology model, the XUSTAR array may contain all 0.0 values or the XPBL
! array may contain all 0.0 values.  In either case, set place-holder values for
! variables that would otherwise be calculated in this routine.
!-------------------------------------------------------------------------------

  effective = .TRUE.

  xvd(:,:,:) = 0.0

  IF ( ( ABS(MAXVAL(xustar)) < smallnum ) .OR.  &
       ( ABS(MAXVAL(xpbl))   < smallnum ) ) THEN  ! assume initialization period

    xradyn(:,:) = badval3   ! inverse taken in metcro.F
    xrstom(:,:) = badval3   ! inverse taken in metcro.F

  ELSE

!-------------------------------------------------------------------------------
! Loop over grid cells and calculate dry deposition by species.
!-------------------------------------------------------------------------------

    IF ( .NOT. ALLOCATED ( delta ) ) THEN
      ALLOCATE ( delta (ncols_x, nrows_x) )
      delta(:,:) = 0.0
    ENDIF

    IF ( .NOT. ALLOCATED ( lstwetdate ) .AND. .NOT. (ifwr) ) THEN
      ALLOCATE ( lstwetdate (ncols_x, nrows_x) )
      lstwetdate(:,:) = 0
    ENDIF

    IF ( .NOT. ALLOCATED ( lstwettime ) .AND. .NOT. (ifwr) ) THEN
      ALLOCATE ( lstwettime (ncols_x, nrows_x) )
      lstwettime(:,:) = 0
    ENDIF

    DO r = 1, nrows_x
      DO c = 1, ncols_x

        ! Calculate the relative humidity of air and ground.

        q2 = xq2(c,r)

        IF ( xtemp2(c,r) <= stdtemp ) THEN
          es_air = vp0 * EXP( 22.514 - (6.15e3 / xtemp2(c,r)) )
        ELSE
          es_air = vp0 * EXP( svp2 * (xtemp2(c,r) - stdtemp) /   &
                              (xtemp2(c,r) - svp3) )
        ENDIF

        qss_air = es_air * 0.622 / (xprsfc(c,r) - es_air)
        rh_air  = 100.0 * q2 / qss_air
        rh_air  = MIN( 100.0, rh_air )


        IF ( xtempg(c,r) <= stdtemp ) THEN
          es_grnd = vp0 * EXP( 22.514 - (6.15e3 / xtempg(c,r)) )
        ELSE
          es_grnd = vp0 * EXP( svp2 * (xtempg(c,r) - stdtemp) /   &
                               (xtempg(c,r) - svp3) )
        ENDIF

        qss_grnd = es_grnd * 0.622 / (xprsfc(c,r) - es_grnd)
        rh_grnd  = 100.0 * q2 / qss_grnd
        rh_grnd  = MIN( 100.0, rh_grnd )


        IF ( ( NINT(xlwmask(c,r)) /= 0   ) .AND.  &
             (      xveg(c,r)     >  0.0 ) ) THEN  ! land

          IF ( .NOT. ifwr ) THEN  ! approx canopy wetness - dew from Wesely

            ! Canopy is wet if >trace precip, or moist with light winds

            IF ( ( xrainn(c,r) + xrainc(c,r) > 0.025 ) .OR.  &
                 ( (0.6 + xwspd10(c,r))*(100.0-rh_grnd) <= 19.0 ) ) THEN

              delta(c,r)      = 1.0
              lstwetdate(c,r) = sdate
              lstwettime(c,r) = stime

            ELSE

              IF ( xrgrnd(c,r) > 5.0 ) THEN  ! day (at night, persist DELTA)

                IF ( ( lstwetdate(c,r) > 0 ) .AND.  &
                     ( lstwettime(c,r) > 0 ) ) THEN  ! canopy recently wet

                  elapsedsec = secsdiff (lstwetdate(c,r), lstwettime(c,r),  &
                                         sdate,           stime)

                  IF ( ( elapsedsec >     0 ) .AND.  &  ! assume canopy stays
                       ( elapsedsec <= 7200 ) ) THEN    ! wet for 2 h

                    delta(c,r) = 1.0
                  ELSE IF ( ( elapsedsec >  7200 ) .AND.  &  ! ramp down DELTA
                            ( elapsedsec < 10800 ) ) THEN    ! between 2 & 3 h
                    delta(c,r) = ( 10800.0 - FLOAT(elapsedsec) ) / 3600.0
                  ELSE
                    delta(c,r)      = 0.0
                    lstwetdate(c,r) = 0
                    lstwettime(c,r) = 0
                  ENDIF

                ENDIF

              ENDIF

            ENDIF

          ELSE  ! Already have canopy wetness explicitly from met model.

            wrmax = 0.2e-3 * xveg(c,r) * xlai(c,r)   ! [m]
            IF ( xwr(c,r) <= 0.0 ) THEN
              delta(c,r) = 0.0
            ELSE
              delta(c,r) = xwr(c,r) / wrmax   ! refer to SiB model
              delta(c,r) = MIN( delta(c,r), 1.0 )
            ENDIF

          ENDIF  ! canopy wetness


          ! Assign a pH for rain water based on longitude if US simulation.
          ! Otherwise use default pH.  Use pH value in HPLUS calculation.

          IF ( ( met_y_centd >=   30.0 ) .AND.  &
               ( met_y_centd <=   45.0 ) .AND.  &
               ( met_x_centd >= -120.0 ) .AND.  &
               ( met_x_centd <=  -70.0 ) ) THEN
            IF ( xlonc(c,r) > -100.0 ) THEN
              hplus = hplus_east
            ELSE
              hplus = hplus_west
            ENDIF
          ELSE
            hplus = hplus_def
          ENDIF

        ELSE  ! water

          ! Calculate the water surface film temperature: wet bulb temperature.
          ! Wet bulb temperature based on eqn in Fritschen and Gay (1979).

          lv    = lv0 - dlvdt * ( xtemp2(c,r) - stdtemp )
          cpair = 1004.67 * (1.0 + 0.84 * q2)   ! [J / kg K]
          tw    = ( ( 4.71e4 * cpair / lv ) - 0.870 ) + stdtemp  ! [K]

        ENDIF  ! land or water calcs for all species


        ! Loop over species to calculate dry deposition velocities.

        ddloop: DO l = 1, ltotg

          IF ( TRIM(xdepspc(l)) == 'SULF' ) THEN  ! Sulfate (SULF)

            ! Sulfate calculation follows Wesely (1985), Eqn. 11.

            rbsulf = 1.0 / (0.002*(xustar(c,r)**2 + 0.24*xwstar(c,r)**2) /   &
                     xustar(c,r))
            xvd(c,r,l) = 1.0 / (xradyn(c,r) + rbsulf)

          ELSE

            IF ( ( NINT(xlwmask(c,r)) == 0   ) .OR.  &
                 (      xveg(c,r)     == 0.0 ) ) THEN  ! water

              IF ( TRIM(xdepspc(l)) == 'HG' ) THEN  ! elemental mercury gas

                ! Water bodies are usually supersaturated with dissolved
                ! elemental mercury, so set the surface resistance to a
                ! large value.

                ! Note:  This exception case is not in the inline dry
                ! deposition code in CMAQ because there is a significant
                ! bidirectional (evasion) component of the flux for
                ! elemental mercury.

                rsurf = 1.0e30

              ELSE  ! any other species

                ! Use CMAQ function for calculating the effective Henry's Law
                ! constant.  Note that original M3DRY was inverse, non-
                ! dimensional Henry's Law (caq/cg).   Water pH is different
                ! than rain, and we need to use the water temperature.

                heff  = hlconst(subname(l), tw, effective, hplus_h2o)

                ! Make Henry's Law constant non-dimensional.

                heff  = heff * 0.08205 * tw

                dw25  = (-2.41712 * LOG10(molwt(l)) + 6.28752) * 1.0e-5
                kvisw = 0.017 * EXP(-0.025 * (tw - stdtemp))
                dw    = dw25 * tw * rt25ink * 0.009025 / kvisw
                scw_pr_23 = ( (kvisw / dw) / pr )**twothirds
                rgw   = scw_pr_23 / (heff * d3 * xustar(c,r))
                rsurf = rgw

              ENDIF  ! elemental mercury exception

            ELSE  ! land

              ! Use CMAQ function for calculating the effective Henry's Law
              ! constant.  Note that original M3DRY was inverse,
              ! non-dimensional Henry's Law (caq/cg).

              heff  = hlconst(subname(l), xtemp2(c,r), effective, hplus)

              ! Make Henry's Law constant non-dimensional.

              heff  = heff * 0.08205 * xtemp2(c,r)


              ! Wet surface resistance.  (Note DELTA = CWC in ADOM lingo.)
              ! This now applies to cuticle and ground.

              IF ( TRIM( xdepspc(l) ) == 'O3' ) THEN
                ! Set RCW/LAI = 200 s/m on basis of Keysburg exp for O3.
                rwet = 1250.0         ! [s/m]
              ELSE
                rwet = rcw0 / heff    ! wet cuticle
              ENDIF

              IF ( TRIM( xdepspc(l) ) == 'HG' ) THEN
                ! Nevada STORMS experiments showed increased Hg(0) evasion from
                ! wet soil surfaces, and X. Lee (2000) showed evasive peaks
                ! during snow melt.  Thus the evasive flux should be larger
                ! than deposition.  (J. Bash, 30 May 2008)
                rgw = 1.0e30          ! [s/m]
              ELSE
                rgw  = rgwet0 / heff    ! wet ground
              ENDIF


              ! Dry snow resistance.

              rsnow = rsnow0 * a0 / a(l)


              ! If the surface is cold and wet, use dry snow.

              IF ( xtempg(c,r) < stdtemp ) THEN
                rwetsfc = rsnow
              ELSE
                rwetsfc = rwet
              ENDIF


              ! Dry cuticle resistance.

              IF ( TRIM(xdepspc(l)) == 'NH3' ) THEN
                rcut = 4000.0 * EXP( -0.054 * rh_air )
              ELSE
                rcut = rcut0 * a0 / a(l)
              ENDIF


              ! Dry ground resistance.  (revised according to Erisman)

              hcan  = xzruf(c,r) * 10.0
              rinc  = 14.0 * xlai(c,r) * hcan / xustar(c,r)
              rgnd  = rg0 * a0 / a(l)
              rgndc = 1.0 / ( (1.0-delta(c,r))/rgnd + delta(c,r)/rgw )   &
                      + rinc  ! Add in-canopy part

              ! Determine the snow liquid water mass fraction (0.0 to 0.5).

              xm = 0.02 * (xtemp2(c,r) - (stdtemp - 1.0))**2
              xm = MIN (xm, 0.5)
              xm = MAX (xm, 0.0)
              IF ( xtemp2(c,r) < (stdtemp - 1.0) ) xm = 0.0

              ifsnow = NINT(xsnocov(c,r))
              ifsnow = MAX(0, ifsnow)


              ! Bulk stomatal resistance; include mesophyll resistance

              rstom = xrstom(c,r) * dwat / dif0(l) + meso(l) / xlai(c,r)

              ! Bulk surface resistance.

              rci = xveg(c,r) * (1.0/rstom + (1.0-delta(c,r)) * xlai(c,r)/rcut +  &
                    ( delta(c,r) * xlai(c,r) / rwetsfc ) + (1.0/rgndc)) +         &
                    (1-ifsnow) * ( (1.0 - xveg(c,r)) * ( (1.0-delta(c,r)) /       &
                    rgnd + delta(c,r) / rgw ) ) +                                 &
                    ifsnow * ( (1.0 - xm) / rsnow + xm / (rsndiff + rgw) )

              rsurf = 1.0 / rci

            ENDIF  ! land or water cell

            ! Compute dry deposition velocity.

            rbc = 5.0 / xustar(c,r) * scc_pr_23(l)
            rac = xradyn(c,r) + rbc

            xvd(c,r,l) = 1.0 / (rsurf + rac)

            ! Check for negative values and NaN's.

            IF ( ( xvd(c,r,l) < 0.0 ) .OR.  &
                 ( xvd(c,r,l) /= xvd(c,r,l) ) ) GOTO 8000

          ENDIF  ! special condition for sulfate

        ENDDO ddloop

      ENDDO
    ENDDO

  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) c, r, TRIM(subname(l)), xvd(c,r,l)
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                               &
              /, 1x, '*** SUBROUTINE: M3DRY',                               &
              /, 1x, '***   NEGATIVE OR UNDEFINED DRY DEPOSITION VELOCITY', &
              /, 1x, '***   POINT   = ', 2i5,                               &
              /, 1x, '***   SPECIES = ', a,                                 &
              /, 1x, '***   Vd      = ', e13.6,                             &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE m3dry
