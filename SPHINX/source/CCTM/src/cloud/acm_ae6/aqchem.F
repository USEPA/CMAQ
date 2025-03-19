
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
      SUBROUTINE AQCHEM ( JDATE, JTIME, TEMP, PRES_PA, TAUCLD, PRCRATE,
     &                    WCAVG, WTAVG, AIRM, ALFA0, ALFA2, ALFA3, GAS,
     &                    AEROSOL, GASWDEP, AERWDEP, HPWDEP, BETASO4, COSZ,
     &                    FRACTR, FRACPOA, FRACPRI, FRACSOA )

C-----------------------------------------------------------------------
C  Description:
C    Compute concentration changes in cloud due to aqueous chemistry,
C    scavenging and wet deposition amounts.
C
C  Revision History:
C      No   Date   Who  What
C      -- -------- ---  -----------------------------------------
C      0  / /86    CW   BEGIN PROGRAM - Walceks's Original Code
C      1  / /86    RB   INCORPORATE INTO RADM
C      2  03/23/87 DH   REFORMAT
C      3  04/11/88 SJR  STREAMLINED CODE - ADDED COMMENTS
C      4  08/27/88 SJR  COMMENTS, MODIFIED FOR RPM
C      4a 03/15/96 FSB  Scanned hard copy to develop Models3
C                       Version.
C      5  04/24/96 FSB  Made into Models3 Format
C      6  02/18/97 SJR  Revisions to link with Models3
C      7  08/12/97 SJR  Revised for new concentration units (moles/mole)
C                       and new treatment of nitrate and nitric acid
C      8  01/15/98 sjr  revised to add new aitken mode scavenging
C                       and aerosol number scavenging
C      9  12/15/98 David Wong at LM:
C             -- change division of XL, TEMP to multiplication of XL, TEMP
C                reciprocal, respectively
C             -- change / TOTOX / TSIV to / ( TOTOX * TSIV )
C     10  03/18/99 David Wong at LM:
C             -- removed "* 1.0" redundant calculation at TEMP1 calculation
C     11  04/27/00 sjr  Added aerosol surface area as modeled species
C     12  12/02    sjr  changed calls to HLCONST and updated the dissociation
C                       constants
C     13  06/26/03 sjr  revised calculations of DTW based on CMAS website
C                       discussions
C     14  08/05/03 sjr  revision made to the coarse aerosol number washout
C     15  04/20/05  us  revisions to add sea salt species in the fine and
C                       coarse aerosol modes, and HCl dissolution/dissociation
C         08/01/05 sjr  Modified for sulfate tracking model
C     16  10/13/05 sjr  fixed bug in the integration time step calculation
C                       (reported by Bonyoung Koo)
C     17  03/01/06 sjr  added elemental carbon aerosol; organic aerosols
C                       replaced with primary, secondary biogenic, and
C                       secondary anthropogenic; fixed 3rd moment calc to
C                       include EC and primary organics (not secondary);
C                       re-arranged logic for setting Cl & Na ending conc;
C                       added pointers/indirect addressing for arrays WETDEP
C                       and LIQUID
C     16  03/30/07 sjr  Limit integration timestep by cloud washout time
C     17  04/10/07 sjr  increased loop limits as follows: I20C <10000,
C                       I7777C <10000, I30C <10000, ICNTAQ <60000
C     18  01/10/07 agc  added organic chemistry for GLY and MGLY oxidation
C     19  09/10/07 sln  updated SOA species list for AE5
C     20  01/29/08 agc  updated DOHDT calculation
C     21  04/14/08 jtk  added coding for coarse NH4 and scavenging of
c                       coarse surface area
C     22  05/20/08 agc  for CB05, use the Henry's Law constant for glyoxal
C                       as a surrogate for methyl glyoxal
C     23  04/15/09 sjr& Several changes made to improve mass conservation in the
C                  agc  solver.  (1) OH concentration is now considered to be
C                       steady state; (2) only allow sulfur oxidation to affect
C                       time step; (3) implemented mass conservation checks -
C                       limit oxidation rates by the available mass for the
C                       specified timestep.
C   10 Oct 10 J.Young:  update to use aero_reeng by Steve Howard, Prakash Bhave,
C                       Jeff Young, Sergey Napelenok, and Shawn Roselle
C   01 Mar 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C    9 Mar 11 S.Napelenok: update for AE6 - pH calculation now expanded to
C                       include Ca Mg K SOIL CORS SEAS
C   23 May 11 G.Sarwar: update S(VI) production rate via H2O2, O3, MHP, PAA
C                       pathways (Jacobson 1997)
C   23 May 11 G.Sarwar: update S(VI) production rate via O2 pathway (metal
C                       catalysis) (Martin and Goodman, 1991)
C   01 Jul 11 G.Sarwar: Incorporate day and night dependent Fe III oxidation
C                       state (Alexander et al.,  2009)
C   12 Aug 11 G.Sarwar: Revise Fe and Mn solubility based on
C                       Alexander et al., 2009
C
C    8 Mar 12 J.Bash:   FE_OX and MN_OX were calculated from FE and MN before
C                       a floor value of 0.0 was established for these
C                       concentrations sometimes resulting in negative
C                       concentrations and model crashes. The code used to
C                       estimate FE_OX and MN_OX was moved to be after a floor
C                       value for FE and MN was set. Also the washout rate was
C                       removed from the calculation of the estimate for doubling
C                       the time step based on sulfur oxidized < 5%.
C   28 Nov 12 G.Sarwar: Sulfate inhibition effect is implemented in the metal catalysis pathway
C   07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
C   12 Feb 15 B.Hutzell: reduced number of exp(...) calculations for scavenging aitken
C                        aerosols to improve efficiency
C   15 Jun 15 J.Young:  Fixed bug found by Martin Otte in calculations for scavenging
C                       aitken aerosols
C   15 Apr 16 J.Young:  Use aerosol factors from AERO_DATA module named constants
C   19 Apr 18 K.Fahey:  For species with both gas phase and coarse mode aerosol components, avoid
C                       introducing extra mass when the coarse mode concentration is greater than 
C                       the total amount left in the aqueous phase after redistribution between the 
C                       phases. 
C   26 Nov 18 S.Napelenok: ISAM implementation
C   31 Dec 19 S.Napelenok: DDM-3D implementation for version 5.3.1

C
C  References:
C     Walcek & Taylor, 1986, A theoretical Method for computing
C        vertical distributions of acidity and sulfate within cumulus
C        clouds, J. Atmos Sci.,  Vol. 43, no. 4 pp 339 - 355
C     Carlton, A.G., B.J. Turpin, K.E. Altieri, S.P. Seitzinger, R. Mathur,
C        S.J. Roselle, and R.J. Weber, CMAQ Model Performance Enhanced When
C        In-Cloud Secondary Organic Aerosol is Included:  Comparison of Organic
C        Carbon Predictions with Measurements, Environ. Sci. Technol., 42(23),
C        8798-8802, 2008.
C     Jacobson, M., Development and application of a new air pollution modeling
C        system II. Aerosol module structure and design, Atmospheric
C        Environment, 31, 131-144, 1997
C     Martin, R.L. and T.W. Good, catalyzed oxidation of sulfur dioxide in
C        solution: the iron-manganese synercism, Atmospheric Environment, 25A,
C        2395-2399, 1991
C     Alexander, B., R.J. Park, D.J. jacob, S. Gong, Transition metal-catalyzed
C        oxidation of atmospheric sulfur: global implications for the sulfur
C        budget, GRL, 114, D02309, 2009

C  Called by:  AQMAP

C  Calls the following subroutines:  none

C  Calls the following functions:  HLCONST

C  Arguments     Type      I/O       Description
C  ---------     ----  ------------  --------------------------------
C  GAS(ngas)     real  input&output  Concentration for species i=1,15
C  GASWDEP(ngas) real     output     wet deposition for species

C  AEROSOL(naer,nmodes) real input&output   Concentration for species i=1,51
C  AERWDEP(naer,nmodes) real     output     wet deposition for species
C-----------------------------------------------------------------------

      USE RXNS_DATA       ! chemical mechanism data
      USE AQ_DATA         ! doesn't inherit; gets only n_aerospc, conmin from AERO_DATA
      USE AERO_DATA
      USE UTILIO_DEFN
#ifdef isam
      USE SA_DEFN, ONLY: DEPSUM_SAVE, DS4_SAVE, REMOV_SAVE,
     &                   DEPSUM_AORGC_SAVE, DGLY1_SAVE, DMGLY1_SAVE,
     &                   REMOV_AORGC_SAVE
#endif

#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX, S_POLC, S_CEND, S_REMOV,
     &                       S_REMOVAC, S_HPWDEP, S_GAS, S_GASWDEP,
     &                       S_AEROSOL, S_AERWDEP, IHIGH, IPT
#endif

      IMPLICIT NONE

      INCLUDE SUBST_CONST          ! constants

      CHARACTER( 120 ) :: XMSG = ' '  ! Exit status message

C...........Parameters:

      INTEGER, PARAMETER :: NUMOX = 5          ! number of oxidation reactions

      REAL( 8 ), PARAMETER :: H2ODENS = 1000.0D0   ! water density at 20 C and 1 ATM (kg/m3)
      REAL( 8 ), PARAMETER :: SEC2HR  = 1.0D0 / 3600.0D0 ! convert seconds to hours
      REAL( 8 ), PARAMETER :: SCVEFF  = 100.0D0    ! Scavenging efficiency (%)

C...........Arguments:

      INTEGER,   INTENT( IN )  :: JDATE  ! current model date, coded YYYYDDD
      INTEGER,   INTENT( IN )  :: JTIME  ! current model time, coded HHMMSS

      REAL,      INTENT( IN )  :: AIRM      ! total air mass in cloudy layers (mol/m2)
      REAL,      INTENT( IN )  :: ALFA0     ! scav coef for aitken aerosol number
      REAL,      INTENT( IN )  :: ALFA2     ! scav coef for aitken aerosol sfc area
      REAL,      INTENT( IN )  :: ALFA3     ! scav coef for aitken aerosol mass
      REAL,      INTENT( OUT ) :: HPWDEP    ! hydrogen wet deposition (mm mol/liter)
      REAL( 8 ), INTENT( OUT ) :: BETASO4
      REAL,      INTENT( IN )  :: PRCRATE   ! precip rate (mm/hr)
      REAL,      INTENT( IN )  :: PRES_PA   ! pressure (Pa)
      REAL,      INTENT( IN )  :: TAUCLD    ! timestep for cloud (s)
      REAL,      INTENT( IN )  :: TEMP      ! temperature (K)
      REAL,      INTENT( IN )  :: WCAVG     ! liquid water content (kg/m3)
      REAL,      INTENT( IN )  :: WTAVG     ! total water content (kg/m3)
      REAL( 8 ), INTENT( INOUT ) :: GAS    ( : )   ! gas phase concentrations (mol/molV)
      REAL( 8 ), INTENT( INOUT ) :: AEROSOL( :,: ) ! aerosol concentrations (mol/molV)
      REAL( 8 ), INTENT( INOUT ) :: GASWDEP( : )   ! gas phase wet deposition array (mm mol/liter)
      REAL( 8 ), INTENT( INOUT ) :: AERWDEP( :,: ) ! aerosol wet deposition array (mm mol/liter)
      REAL,      INTENT( IN )    :: COSZ     ! cosine solar zenith angle
      REAL( 8 ), INTENT( OUT ) :: FRACTR    ! Fraction of J mode tracer scavenged from I mode 
      REAL( 8 ), INTENT( OUT ) :: FRACPOA   ! Fraction of J mode poa scavenged from I mode 
      REAL( 8 ), INTENT( OUT ) :: FRACPRI   ! Fraction of J mode pri scavenged from I mode 
      REAL( 8 ), INTENT( OUT ) :: FRACSOA   ! Fraction of J mode soa scavenged from I mode
      

C...........Local Variables (scalars):

      LOGICAL, SAVE :: FIRSTIME = .TRUE. ! flag for first pass thru
      LOGICAL :: DARK

      CHARACTER( 16 ), SAVE :: PNAME = 'AQCHEM'             ! driver program name
      CHARACTER( 16 ), SAVE :: MGLYSUR = 'METHYL_GLYOXAL  ' ! Henry's law surrogate for MGLY

      INTEGER      ISPC            ! loop counter for species
      INTEGER      I20C            ! loop counter for do loop 20
      INTEGER      I30C            ! loop counter for do loop 30
      INTEGER      ITERAT          ! # iterations of aqueous chemistry solver
      INTEGER      I7777C          ! aqueous chem iteration counter
      INTEGER      ICNTAQ          ! aqueous chem iteration counter
      INTEGER      LIQ             ! loop counter for liquid species
      INTEGER      IGAS            ! loop counter for gas species
      INTEGER      IOX             ! index over oxidation reactions

      REAL( 8 ) :: DEPSUM
      REAL( 8 ) :: A               ! iron's anion concentration
      REAL( 8 ) :: HPLUS           ! H+ concentration in cloudwater (mol/liter)
      REAL( 8 ) :: ACT1            ! activity correction factor, single ions
      REAL( 8 ) :: ACT2            ! activity factor correction, double ions
      REAL( 8 ) :: ACTB            !
      REAL( 8 ) :: AE              ! guess for H+ conc in cloudwater (mol/liter)
      REAL( 8 ) :: B               ! manganese's anion concentration
      REAL( 8 ) :: PRES_ATM        ! pressure (Atm)
      REAL( 8 ) :: BB              ! lower limit guess of cloudwater pH
      REAL( 8 ) :: CA              ! Calcium conc in cloudwater (mol/liter)
      REAL( 8 ) :: CL              ! total Cl-  conc in cloudwater (mol/liter)
      REAL( 8 ) :: CLACC           ! fine Cl- in cloudwater (mol/liter)
      REAL( 8 ) :: CLCOR           ! coarse Cl-  conc in cloudwater (mol/liter)
      REAL( 8 ) :: CO2H            ! Henry's Law constant for CO2
      REAL( 8 ) :: CO21            ! First dissociation constant for CO2
      REAL( 8 ) :: CO22            ! Second dissociation constant for CO2
      REAL( 8 ) :: CO212           ! CO21*CO22
      REAL( 8 ) :: CO212H          ! CO2H*CO21*CO22
      REAL( 8 ) :: CO21H           ! CO2H*CO21
      REAL( 8 ) :: CO2L            ! CO2 conc in cloudwater (mol/liter)
      REAL( 8 ) :: CO3             ! CO3= conc in cloudwater (mol/liter)
      REAL( 8 ) :: CTHK1           ! cloud thickness (m)
      REAL( 8 ) :: DSIV_SCALE      ! mass conservation scale factor for S(IV)
      REAL( 8 ) :: DTRMV           !
      REAL( 8 ) :: DTS6            !
      REAL( 8 ) :: DGLYDT          ! change in GLY (mol/liter/sec)
      REAL( 8 ) :: DMGLYDT         ! change in MGLY (mol/liter/sec)
!     REAL( 8 ) :: DOHDT           ! change in OH
      REAL( 8 ) :: DGLY1           ! change due to Rxn. in GLY for DTW(0) time step
      REAL( 8 ) :: DMGLY1          ! change due to Rxn. in MGLY for DTW(0) time step
!     REAL( 8 ) :: DOH1            ! change in OH for DTW(0) time step
      REAL( 8 ) :: DORGC           ! change in ORGC for DTW(0) time step (mol/liter)
      REAL( 8 ) :: EBETASO4T       ! EXP( -BETASO4 * TAUCLD )
      REAL( 8 ) :: EALFA0T         ! EXP( -ALFA0 * TAUCLD )
      REAL( 8 ) :: EALFA2T         ! EXP( -ALFA2 * TAUCLD )
      REAL( 8 ) :: EALFA3T         ! EXP( -ALFA3 * TAUCLD )
      REAL( 8 ) :: EC              ! elemental carbon acc+akn aerosol in cloudwater (mol/liter)
      REAL( 8 ) :: FA              ! functional value ??
      REAL( 8 ) :: FB              ! functional value ??
      REAL( 8 ) :: FCLCOR          ! frac weight of coarse CL to (acc+coarse) CL
      REAL( 8 ) :: FE              ! Fe+++ conc in cloudwater (mol/liter)
      REAL( 8 ) :: FHNO3           ! frac weight of HNO3 to total NO3
      REAL( 8 ) :: FNH3            ! frac weight of NH3 to total ammonia
      REAL( 8 ) :: FNH4ACC         ! frac weight of NH4 acc to total ammonia
      REAL( 8 ) :: FNH4COR         ! frac weight of coarse NH4 to (acc+coarse) NH4
      REAL( 8 ) :: FNO3ACC         ! frac weight of NO3 acc to total NO3     
      REAL( 8 ) :: FNO3COR         ! frac weight of coarse NO3 to (acc+coarse) NO3
      REAL( 8 ) :: FRACLIQ         ! fraction of water in liquid form
      REAL( 8 ) :: FOA1            ! First dissociation constant for FOA (Formic Acid)
      REAL( 8 ) :: FOAH            ! Henry's Law constant for FOA
      REAL( 8 ) :: FOA1H           ! FOAH*FOA1
      REAL( 8 ) :: FOAL            ! FOA conc in cloudwater (mol/liter)
      REAL( 8 ) :: FTST            !
      REAL( 8 ) :: GLYH            ! Henry's Law constant for glyoxal
      REAL( 8 ) :: GLYL            ! glyoxal conc in cloud water (mol/liter)
      REAL( 8 ) :: GM              !
      REAL( 8 ) :: GM1             !
      REAL( 8 ) :: GM1LOG          !
      REAL( 8 ) :: GM2             ! activity correction factor
      REAL( 8 ) :: GM2LOG          !
      REAL( 8 ) :: HA              !
      REAL( 8 ) :: HB              !
      REAL( 8 ) :: H2OW            !
      REAL( 8 ) :: H2O2H           ! Henry's Law Constant for H2O2
      REAL( 8 ) :: H2O2L           ! H2O2 conc in cloudwater (mol/liter)
      REAL( 8 ) :: HCLH            ! Henry's Law Constant for HCL
      REAL( 8 ) :: HCL1            ! First dissociation constant for HCL
      REAL( 8 ) :: HCL1H           ! HCL1*HCLH
      REAL( 8 ) :: HCLL            ! HCl  conc in  cloudwater (mol/liter)
      REAL( 8 ) :: HCO2            ! HCO2 conc in cloudwater (mol/liter)
      REAL( 8 ) :: HCO3            ! HCO3 conc in cloudwater (mol/liter)
      REAL( 8 ) :: HNO3H           ! Henry's Law Constant for HNO3
      REAL( 8 ) :: HNO31           ! First dissociation constant for HNO3
      REAL( 8 ) :: HNO31H          !
      REAL( 8 ) :: HNO3L           ! HNO3 conc in cloudwater (mol/liter)
      REAL( 8 ) :: HOH             ! Henry's Law Constant for HO
      REAL( 8 ) :: HSO3            ! HSO3 conc in cloudwater (mol/liter)
      REAL( 8 ) :: HSO4            ! HSO4 concn in cloudwater (mol/liter)
      REAL( 8 ) :: HSO4ACC         ! accumulation mode HSO4 concn in cloudwater (mol/liter)
      REAL( 8 ) :: HSO4COR         ! coarse HSO4 concn in cloudwater (mol/liter)
      REAL( 8 ) :: HTST            !
      REAL( 8 ) :: K               ! K conc in cloudwater (mol/liter)
      !REAL( 8 ) :: LGTEMP          ! log of TEMP
      REAL( 8 ) :: MG              !
      REAL( 8 ) :: MGLYH           ! Henry's Law Constant for methylglyoxal
      REAL( 8 ) :: MGLYL           ! MGLY conc in cloud water (mol/liter)
      REAL( 8 ) :: MHPH            ! Henry's Law Constant for MHP
      REAL( 8 ) :: MHPL            ! MHP conc in cloudwater (mol/liter)
      REAL( 8 ) :: MN              ! Mn++ conc in cloudwater (mol/liter)
      REAL( 8 ) :: NA              ! Na conc in cloudwater (mol/liter)
      REAL( 8 ) :: NAACC           ! Na in cloudwater (mol/liter)
      REAL( 8 ) :: NACOR           ! coarse Na in cloudwater (mol/liter)
      REAL( 8 ) :: NH31            ! First dissociation constant for NH3
      REAL( 8 ) :: NH3H            ! Henry's Law Constant for NH3
      REAL( 8 ) :: NH3DH20         !
      REAL( 8 ) :: NH31HDH         !
      REAL( 8 ) :: NH3L            ! NH3 conc in cloudwater (mol/liter)
      REAL( 8 ) :: NH4             ! NH4+ conc in cloudwater (mol/liter)
      REAL( 8 ) :: NH4ACC          ! NH4 acc conc in cloudwater (mol/liter)
      REAL( 8 ) :: NH4COR          ! NH4 coarse conc in cloudwater (mol/liter)
      !REAL( 8 ) :: NITAER          ! total aerosol nitrate
      REAL( 8 ) :: NO3             ! NO3 conc in cloudwater (mol/liter)
      REAL( 8 ) :: NO3ACC          ! NO3 acc conc in cloudwater (mol/liter)
      REAL( 8 ) :: NO3COR          ! NO3 coarse conc in cloudwater (mol/liter)
      REAL( 8 ) :: NUMCOR          ! coarse aerosol number in cloudwater (mol/liter)
      REAL( 8 ) :: O3H             ! Henry's Law Constant for O3
      REAL( 8 ) :: O3L             ! O3 conc in cloudwater (mol/liter)
      REAL( 8 ) :: OH              ! OH conc in cloudwater (mol/liter)
      REAL( 8 ) :: OHL             ! OH radical conc in cloudwater (mol/liter)
      REAL( 8 ) :: SOA             ! secondary organic aerosol in cloudwater (mol/liter)
      REAL( 8 ) :: ORGC            ! cloud-produced SOA in cloudwater (treated as primary)
      REAL( 8 ) :: POA             ! primary organic aerosol in cloudwater (mol/liter)
      REAL( 8 ) :: PAAH            ! Henry's Law Constant for PAA
      REAL( 8 ) :: PAAL            ! PAA conc in cloudwater (mol/liter)
      REAL( 8 ) :: PCO2F           ! gas only CO2 partial pressure (atm)
      REAL( 8 ) :: PFOAF           ! gas only ORGANIC ACID partial press (atm)
      REAL( 8 ) :: PGLYF           ! gas only GLY partial pressure (atm)
      REAL( 8 ) :: PH2O2F          ! gas only H2O2 partial pressure (atm)
      REAL( 8 ) :: PHCLF           ! gas only HCL partial pressure (atm)
      REAL( 8 ) :: PHNO3F          ! gas only HNO3 partial pressure (atm)
      REAL( 8 ) :: PHOF            ! gas only HO partial pressure (atm)
      REAL( 8 ) :: PMGLYF          ! gas only MGLY parital pressure (atm)
      REAL( 8 ) :: PMHPF           ! gas only MHP partial pressure (atm)
      REAL( 8 ) :: PNH3F           ! gas only NH3 partial pressure (atm)
      REAL( 8 ) :: PO3F            ! gas only O3 partial pressure (atm)
      REAL( 8 ) :: PPAAF           ! gas only PAA partial pressure (atm)
      REAL( 8 ) :: PRIM            ! PRIMARY acc+akn aerosol in cloudwater (mol/liter)
!     REAL( 8 ) :: PRIMCOR         ! PRIMARY coarse aerosol in cloudwater (mol/liter)
      REAL( 8 ) :: PSO2F           ! gas only SO2 partial pressure (atm)
      !REAL( 8 ) :: RATE            !
      REAL( 8 ) :: RECIPA1         !
      REAL( 8 ) :: RECIPA2         !
      REAL( 8 ) :: RECIPAP1        ! one over pressure (/atm)
      REAL( 8 ) :: RGLY3           ! liter/(mol sec)
      REAL( 8 ) :: RH2O2           !
      REAL( 8 ) :: RMGLY3          ! liter/(mol sec)
      REAL( 8 ) :: RMHP            !
      REAL( 8 ) :: RPAA            !
      REAL( 8 ) :: RT              ! gas const * temperature (liter atm/mol)
      REAL( 8 ) :: SIV             ! dissolved so2 in cloudwater (mol/liter)
      REAL( 8 ) :: SK6             !
      REAL( 8 ) :: SK6TS6          !
      REAL( 8 ) :: SO21            ! First dissociation constant for SO2
      REAL( 8 ) :: SO22            ! Second dissociation constant for SO2
      REAL( 8 ) :: SO2H            ! Henry's Law Constant for SO2
      REAL( 8 ) :: SO212           ! SO21*SO22
      REAL( 8 ) :: SO212H          ! SO21*SO22*SO2H
      REAL( 8 ) :: SO21H           ! SO21*SO2H
      REAL( 8 ) :: SO2L            ! SO2 conc in cloudwater (mol/liter)
      REAL( 8 ) :: SO3             ! SO3= conc in cloudwater (mol/liter)
      REAL( 8 ) :: SO4             ! SO4= conc in cloudwater (mol/liter)
      REAL( 8 ) :: SO4ACC          ! accumulation mode SO4= conc in cloudwater (mol/liter)
      REAL( 8 ) :: SO4COR          ! coarse SO4= conc in cloudwater (mol/liter)
      REAL( 8 ) :: STION           ! ionic strength
      REAL( 8 ) :: TAC             !
      REAL( 8 ) :: TCLa            ! sum of accumulation and coarse mode chloride
      REAL( 8 ) :: TEMP1           ! (1/T) - (1/298) (1/K)
      REAL( 8 ) :: TIMEW           ! cloud chemistry clock (sec)
!     REAL( 8 ) :: THO             ! total hydroxyl radical available for oxidation
      REAL( 8 ) :: TGLY            ! total glyoxal available for oxidation
      REAL( 8 ) :: TMGLY           ! total methylglyoxal available for oxidation
      !REAL( 8 ) :: TOTOX           !
      REAL( 8 ) :: TH2O2
      REAL( 8 ) :: TO3
      REAL( 8 ) :: TMHP      
      REAL( 8 ) :: TNH4a           ! sum of accumulation and coarse mode ammonium
      REAL( 8 ) :: TNO3a           ! sum of accumulation and coarse mode nitrate      
      REAL( 8 ) :: TPAA
      REAL( 8 ) :: TOTAMM          ! total ammonium
      REAL( 8 ) :: TOTNIT          ! total nitrate (excluding coarse mode)
      REAL( 8 ) :: TS6             ! SO4 conc in cloudwater (mol/liter)
      REAL( 8 ) :: TS6ACC          ! SO4 acc conc in cloudwater (mol/liter)
      REAL( 8 ) :: TS6COR          ! coarse SO4 conc in cloudwater   (mol/liter)
C...for sulfur tracking
      REAL( 8 ) :: TS6AQH2O2       ! SO4 conc from reaction 1 (mol/liter)
      REAL( 8 ) :: TS6AQO3         ! SO4 conc from reaction 2 (mol/liter)
      REAL( 8 ) :: TS6AQFEMN       ! SO4 conc from reaction 3 (mol/liter)
      REAL( 8 ) :: TS6AQMHP        ! SO4 conc from reaction 4 (mol/liter)
      REAL( 8 ) :: TS6AQPAA        ! SO4 conc from reaction 5 (mol/liter)

      REAL( 8 ) :: TSIV            ! total S(iv) available for oxidation
      REAL( 8 ) :: TST             !
      REAL( 8 ) :: TWASH           ! washout time for clouds (sec)
      REAL( 8 ) :: WETFAC          ! converts mol/l to mm-mol/l based on precip
      REAL( 8 ) :: XC1             ! (/mm)
      REAL( 8 ) :: XC2             ! (liter-atm/mol/mm)
      REAL( 8 ) :: XL              ! conversion factor (liter-atm/mol)
      REAL( 8 ) :: ONE_OVER_XL     ! 1.0 / XL
      REAL( 8 ) :: PRES_ATM_OVER_XL ! PRES_ATM / XL
      REAL( 8 ) :: SCAVENGED       ! aitken scavenging factor by cloud water
      REAL( 8 ) :: XLCO2           !
      REAL( 8 ) :: XLH2O2          !
      REAL( 8 ) :: XLHCL           ! const in calc of HCL final partial pres
      REAL( 8 ) :: XLHNO3          !
      REAL( 8 ) :: XLMHP           !
      REAL( 8 ) :: XLNH3           !
      REAL( 8 ) :: XLO3            !
      REAL( 8 ) :: XLPAA           !
      REAL( 8 ) :: XLSO2           !
      REAL( 8 ) :: CAACC           ! accumulation mode Calcium (AE6) SLN 16March2011
      REAL( 8 ) :: MGACC           ! accumulation mode Magnesium (AE6) SLN 16March2011
      REAL( 8 ) :: KACC            ! accumulation mode Potassium (AE6) SLN 16March2011
      REAL( 8 ) :: CACOR           ! coarse mode Calcium (AE6) SLN 16March2011
      REAL( 8 ) :: MGCOR           ! coarse mode Magnesium (AE6) SLN 16March2011
      REAL( 8 ) :: KCOR            ! coarse mode Potassium (AE6) SLN 16March2011
      REAL( 8 ) :: SOILCOR         ! coarse mode SOIL (AE6) SLN 16March2011
      REAL( 8 ) :: ANTHCOR         ! coarse mode CORS (AE6) SLN 16March2011
      REAL( 8 ) :: SEASCOR         ! coarse mode SEAS (AE6) SLN 16March2011
      REAL( 8 ) :: FEACC           ! accumulation mode Fe (AE6) SLN 22March2011
      REAL( 8 ) :: MNACC           ! accumulation mode Mn (AE6) SLN 22March2011
      REAL( 8 ) :: FECOR           ! coarse mode Fe (AE6) SLN 22March2011
      REAL( 8 ) :: MNCOR           ! coarse mode Mn (AE6) SLN 22March2011
      REAL( 8 ) :: FE_OX           ! Fe(III) available for sulfate oxidation
      REAL( 8 ) :: MN_OX           ! Mn(II) available for sulfate oxidation
      REAL( 8 ) :: FE_III          ! Fractional Fe(III) partitioning, GS - July 1, 2011
      REAL( 8 ) :: MN_II           ! Fractional Mn(II) partitioning, GS - July 1, 2011

      REAL( 8 ) :: FE_SOL          ! Fractional Fe solubility, GS - July 1, 2011
      REAL( 8 ) :: MN_SOL          ! Fractional Mn solubility, GS - July 1, 2011
					    
      REAL( 8 ) :: TRACI, POAI, PRII, SOAI

      REAL( 8 ), SAVE :: SOIL_FE_FAC  ! Fe molar fraction of ASOIL
      REAL( 8 ), SAVE :: CORS_FE_FAC  ! Fe molar fraction of ACORS
      REAL( 8 ), SAVE :: SOIL_MN_FAC  ! etc.
      REAL( 8 ), SAVE :: CORS_MN_FAC
      REAL( 8 ), SAVE :: SEAS_NA_FAC  ! Na molar fraction of ASEACAT
      REAL( 8 ), SAVE :: SOIL_NA_FAC
      REAL( 8 ), SAVE :: CORS_NA_FAC
      REAL( 8 ), SAVE :: SEAS_MG_FAC
      REAL( 8 ), SAVE :: SOIL_MG_FAC
      REAL( 8 ), SAVE :: CORS_MG_FAC
      REAL( 8 ), SAVE :: SEAS_CA_FAC
      REAL( 8 ), SAVE :: SOIL_CA_FAC
      REAL( 8 ), SAVE :: CORS_CA_FAC
      REAL( 8 ), SAVE :: SEAS_K_FAC
      REAL( 8 ), SAVE :: SOIL_K_FAC
      REAL( 8 ), SAVE :: CORS_K_FAC

C...........Local Variables (arrays):

      REAL( 8 ) :: LOADING( MAX_NAER, NMODES ) ! aerosol loading (mol/liter)
      REAL( 8 ) :: INITGAS( NGAS ) ! initial gas partial pressure (atm)
      REAL( 8 ) :: LIQUID( NLIQS ) ! wet deposition array (mm mol/liter)
      REAL( 8 ) :: WETDEP( NLIQS ) ! wet deposition array (mm mol/liter)
      REAL( 8 ) :: DSIVDT( 0:NUMOX ) ! rate of so2 oxid incloud (mol/liter/sec)
      REAL( 8 ) :: DS4   ( 0:NUMOX ) ! S(IV) oxidized over timestep DTW(0)
      REAL( 8 ) :: DTW   ( 0:NUMOX ) ! cloud chemistry timestep (sec)

      REAL( 8 ) :: ONE_OVER_TEMP     ! 1.0 / TEMP

C...........External Functions:

      REAL, EXTERNAL :: HLCONST

!For Varaible used by TXHG Version
      !LOGICAL, SAVE :: TRUST_TXHG_CHEM = .TRUE.  ! allow effects for TXHG version on ion and ph
      REAL( 8 ) :: TRACER           ! TRACER acc+akn aerosol in cloudwater (mol/liter)
      REAL( 8 ) :: TRACERCOR        ! TRACER coarse aerosol in cloudwater (mol/liter)
      REAL( 8 ) :: HGFINE           ! mercury PM acc+akn aerosol in cloudwater (mol/liter)
      REAL( 8 ) :: HGCOR            ! mercury PM coarse aerosol in cloudwater (mol/liter)


#ifdef sens
      INCLUDE 'AQCHEM_DDM3D.EXT'   ! ddm-3d variables 
#endif


C*********************************************************************

C...Initialization

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

C... set MW ratios and speciation factors for molar concentrations of coarse
C... soluble aerosols

        SOIL_FE_FAC = ASOIL_FE_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( AFE_IDX ), 8 ) / ASOIL_RENORM
        CORS_FE_FAC = ACORS_FE_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( AFE_IDX ), 8 ) / ACORSEM_RENORM

        SOIL_MN_FAC = ASOIL_MN_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( AMN_IDX ), 8 ) / ASOIL_RENORM
        CORS_MN_FAC = ACORS_MN_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( AMN_IDX ), 8 ) / ACORSEM_RENORM

        SEAS_NA_FAC = ASCAT_NA_FAC * REAL( AEROSPC_MW( ASEACAT_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( ANA_IDX ), 8 )
        SOIL_NA_FAC = ASOIL_NA_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( ANA_IDX ), 8 ) / ASOIL_RENORM
        CORS_NA_FAC = ACORS_NA_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( ANA_IDX ), 8 ) / ACORSEM_RENORM

        SEAS_MG_FAC = ASCAT_MG_FAC * REAL( AEROSPC_MW( ASEACAT_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( AMG_IDX ), 8 )
        SOIL_MG_FAC = ASOIL_MG_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( AMG_IDX ), 8 ) / ASOIL_RENORM
        CORS_MG_FAC = ACORS_MG_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( AMG_IDX ), 8 ) / ACORSEM_RENORM

        SEAS_CA_FAC = ASCAT_CA_FAC * REAL( AEROSPC_MW( ASEACAT_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( ACA_IDX ), 8 )
        SOIL_CA_FAC = ASOIL_CA_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( ACA_IDX ), 8 ) / ASOIL_RENORM
        CORS_CA_FAC = ACORS_CA_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( ACA_IDX ), 8 ) / ACORSEM_RENORM

        SEAS_K_FAC = ASCAT_K_FAC * REAL( AEROSPC_MW( ASEACAT_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( AK_IDX ), 8 )
        SOIL_K_FAC = ASOIL_K_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( AK_IDX ), 8 ) / ASOIL_RENORM
        CORS_K_FAC = ACORS_K_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )
     &                             / REAL( AEROSPC_MW( AK_IDX ), 8 ) / ACORSEM_RENORM

      END IF    ! FIRSTIME

      ONE_OVER_TEMP = 1.0D0 / TEMP

C...check for bad temperature, cloud air mass, or pressure

      IF ( TEMP .LE. 0.0D0 .OR. AIRM .LE. 0.0D0 .OR. PRES_PA .LE. 0.0D0 ) THEN
        XMSG = 'MET DATA ERROR'
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      END IF

C...  determine day or night from cosine of zenith angle for the cell; Golam Sarwar

            IF ( COSZ .LE. 0.0 ) THEN
              DARK = .TRUE.   ! night
            ELSE
              DARK = .FALSE.  ! day
            END IF

C...initialize counters and compute several conversion factors

      ICNTAQ     = 0
      ITERAT     = 0
      DSIV_SCALE = 1.0D0
      RT         = ( MOLVOL / STDTEMP ) * TEMP         ! R * T (liter atm / mol)
      PRES_ATM   = PRES_PA /  STDATMPA                 ! pressure (atm)
      CTHK1      = AIRM * RT / ( PRES_ATM * 1000.0D0 ) ! cloud thickness (m)
      XL         = WCAVG * RT / H2ODENS                ! conversion factor (l-atm/mol)
      ONE_OVER_XL      = 1.0D0 / XL
      PRES_ATM_OVER_XL = PRES_ATM / XL
      TST       = 0.999D0
      GM        = SCVEFF / 100.0D0
      ACT1      = 1.0D0
      ACT2      = 1.0D0
      GM2       = 1.0D0
      TIMEW     = 0.0D0
      RECIPAP1  = 1.0D0 / PRES_ATM
      XC1       = 1.0D0 / ( WCAVG * CTHK1 )
      XC2       = RT / ( 1000.0D0 * CTHK1 )
      FRACLIQ   = WCAVG / WTAVG
      TWASH     = WTAVG * 1000.0D0 * CTHK1 * 3600.0D0
     &          / ( H2ODENS * MAX( 1.0D-20, REAL( PRCRATE,8 ) ) )

C...set equilibrium constants as a function of temperature
C...   Henry`s law constants

      SO2H  = HLCONST( 'SO2             ', TEMP, .FALSE., 0.0 )
      CO2H  = HLCONST( 'CO2             ', TEMP, .FALSE., 0.0 )
      NH3H  = HLCONST( 'NH3             ', TEMP, .FALSE., 0.0 )
      H2O2H = HLCONST( 'H2O2            ', TEMP, .FALSE., 0.0 )
      O3H   = HLCONST( 'O3              ', TEMP, .FALSE., 0.0 )
      HCLH  = HLCONST( 'HCL             ', TEMP, .FALSE., 0.0 )
      HNO3H = HLCONST( 'HNO3            ', TEMP, .FALSE., 0.0 )
      MHPH  = HLCONST( 'METHYLHYDROPEROX', TEMP, .FALSE., 0.0 )
      PAAH  = HLCONST( 'PEROXYACETIC_ACI', TEMP, .FALSE., 0.0 )
      FOAH  = HLCONST( 'FORMIC_ACID     ', TEMP, .FALSE., 0.0 )
      GLYH  = HLCONST( 'GLYOXAL         ', TEMP, .FALSE., 0.0 )
      MGLYH = HLCONST( MGLYSUR,            TEMP, .FALSE., 0.0 )
      HOH   = HLCONST( 'OH              ', TEMP, .FALSE., 0.0 )

      TEMP1 = ONE_OVER_TEMP - 1.0D0 / 298.0D0

C...dissociation constants

      FOA1  = 1.80D-04 * EXP( -2.00D+01 * TEMP1 )      ! Martell and Smith (1977)
      SK6   = 1.02D-02 * EXP(  2.72D+03 * TEMP1 )      ! Smith and Martell (1976)
      SO21  = 1.30D-02 * EXP(  1.96D+03 * TEMP1 )      ! Smith and Martell (1976)
      SO22  = 6.60D-08 * EXP(  1.50D+03 * TEMP1 )      ! Smith and Martell (1976)
      CO21  = 4.30D-07 * EXP( -1.00D+03 * TEMP1 )      ! Smith and Martell (1976)
      CO22  = 4.68D-11 * EXP( -1.76D+03 * TEMP1 )      ! Smith and Martell (1976)
      H2OW  = 1.00D-14 * EXP( -6.71D+03 * TEMP1 )      ! Smith and Martell (1976)
      NH31  = 1.70D-05 * EXP( -4.50D+02 * TEMP1 )      ! Smith and Martell (1976)
      HCL1  = 1.74D+06 * EXP(  6.90D+03 * TEMP1 )      ! Marsh and McElroy (1985)
      HNO31 = 1.54D+01 * EXP(  8.70D+03 * TEMP1 )      ! Schwartz (1984)

C...Kinetic oxidation rates

C...   From Jacobson  (1997)

      RH2O2 = 7.45D+07 * EXP( -15.96D0 * ( ( 298.0D0 / TEMP )  - 1.0D0 ) )

C...   From Jacobson, 1997

      RMHP = 1.90D+07 * EXP( -12.75D0 * ( ( 298.0D0 / TEMP )  - 1.0D0 ) )
      RPAA = 3.60D+07 * EXP( -13.42D0 * ( ( 298.0D0 / TEMP )  - 1.0D0 ) )

C...From Carlton et al. (2007)

      RGLY3  = 3.0D+10   ! rate constant measured at 298K
      RMGLY3 = 3.0D+10   ! assumed to be the same as GLY

C...make initializations

      WETDEP  = 0.0D0
      LOADING = 0.0D0
      INITGAS = 0.0D0

      DSIVDT = 0.0D0
      DTW    = 0.0D0
      DS4    = 0.0D0

      DGLY1  = 0.0D0
      DMGLY1 = 0.0D0
      DORGC  = 0.0D0
!     DOH1   = 0.0

#ifdef sens 
      S_WETDEP = 0.0D0
      S_LOADING = 0.0D0
      S_INITGAS = 0.0D0

      S_DSIVDT = 0.0D0
      S_DS4    = 0.0D0

      S_DGLY1  = 0.0D0
      S_DMGLY1 = 0.0D0
      S_DORGC  = 0.0D0
#endif

#ifdef isam
      DGLY1_SAVE = 0.0D0
      DMGLY1_SAVE = 0.0D0
#endif

C...compute fractional weights for several species

      TOTNIT = GAS( LHNO3 ) + AEROSOL( LNO3, ACC )
      IF ( TOTNIT .GT. 0.0D0 ) THEN
        FHNO3   = GAS( LHNO3 ) / TOTNIT
        FNO3ACC = AEROSOL( LNO3, ACC ) / TOTNIT
      ELSE
        FHNO3   = 1.0D0
        FNO3ACC = 0.0D0
      END IF

      TOTAMM = GAS( LNH3 ) + AEROSOL( LNH4, ACC )
      IF ( TOTAMM .GT. 0.0D0 ) THEN
        FNH3    = GAS( LNH3 ) / TOTAMM
        FNH4ACC = AEROSOL( LNH4, ACC ) / TOTAMM
      ELSE
        FNH3    = 1.0D0
        FNH4ACC = 0.0D0
      END IF

      TNO3a = AEROSOL( LNO3, ACC ) + AEROSOL( LNO3, COR ) 
      IF ( TNO3a .GT. 0.0D0) THEN
         FNO3COR = AEROSOL( LNO3, COR ) / TNO3a
      ELSE
         FNO3COR = 0.0D0
      END IF
      
      TNH4a = AEROSOL( LNH4, ACC ) + AEROSOL( LNH4, COR )
      IF ( TNH4a .GT. 0.0D0) THEN
         FNH4COR = AEROSOL( LNH4, COR ) / TNH4a
      ELSE
         FNH4COR = 0.0D0
      END IF
      
      TCLa  = AEROSOL( LCL, ACC )  + AEROSOL( LCL, COR )
      IF ( TCLa .GT. 0.0D0) THEN
         FCLCOR = AEROSOL( LCL, COR ) / TCLa
      ELSE
         FCLCOR = 0.0D0
      END IF

#ifdef sens
      DO NP = 1, NPMAX

        S_TOTNIT( NP ) = S_GAS( NP, LHNO3 ) + S_AEROSOL( NP, LNO3, ACC )
        IF ( S_TOTNIT( NP ) .NE. 0.0D0 ) THEN
          S_FHNO3( NP )   = S_GAS( NP,LHNO3 ) / S_TOTNIT( NP )
          S_FNO3ACC( NP ) = S_AEROSOL( NP,LNO3, ACC ) / S_TOTNIT( NP )
        ELSE
          S_FHNO3( NP )   = 1.0D0
          S_FNO3ACC( NP ) = 0.0D0
        END IF

        S_TOTAMM( NP ) = S_GAS( NP, LNH3 ) + S_AEROSOL( NP, LNH4, ACC )
        IF ( S_TOTAMM( NP ) .NE. 0.0D0 ) THEN
          S_FNH3( NP )    = S_GAS( NP, LNH3 ) / S_TOTAMM( NP )
          S_FNH4ACC( NP ) = S_AEROSOL( NP, LNH4, ACC ) / S_TOTAMM( NP )
        ELSE
          S_FNH3( NP )    = 1.0D0
          S_FNH4ACC( NP ) = 0.0D0
        END IF

      END DO
#endif
      
C...Assign fraction partitioning of FE(III) and MN(II)

      IF ( DARK ) THEN
        FE_III = 0.9D0  ! Night time, GS 01July2011
      ELSE
        FE_III = 0.1D0  ! Day time, GS 01July2011
      END IF

      MN_II = 1.0D0                     ! Same for day and night, GS  01July2011

C...Assign solubility of Fe and Mn

      FE_SOL = 0.1D0                   ! GS 01July2011
      MN_SOL = 0.5D0                   ! GS 28July2011

C...initial concentration from accumulation-mode aerosol loading (mol/liter)
C...  an assumption is made that all of the accumulation-mode
C...  aerosol mass in incorporated into the cloud droplets

      DO ISPC = 1, NAER
        LOADING( ISPC, ACC ) = AEROSOL( ISPC, ACC ) * PRES_ATM_OVER_XL
      END DO

      LOADING( LSO4, ACC ) = ( AEROSOL( LSO4, ACC ) + GAS( LH2SO4 ) ) * PRES_ATM_OVER_XL

#ifdef sens
      DO NP = 1, NPMAX
        DO ISPC = 1, NAER
           S_LOADING( NP,ISPC, ACC ) = S_AEROSOL( NP,ISPC, ACC ) * PRES_ATM_OVER_XL
        END DO
        S_LOADING( NP,LSO4, ACC ) = ( S_AEROSOL( NP,LSO4, ACC ) + S_GAS( NP,LH2SO4 ) ) * PRES_ATM_OVER_XL
      END DO
#endif

C...initial concentration from coarse-mode aerosol loading (mol/liter)
C...  an assumption is made that all of the coarse-mode
C...  aerosol mass in incorporated into the cloud droplets

      DO ISPC = 1, NAER
        LOADING( ISPC, COR ) = AEROSOL( ISPC, COR ) * PRES_ATM_OVER_XL
      END DO

!     LOADING( LCACO3, COR ) = ( AEROSOL( LCACO3, COR ) + AEROSOL( LMGCO3, COR ) )
!    &                       * PRES_ATM_OVER_XL

#ifdef sens
      DO NP = 1, NPMAX
        DO ISPC = 1, NAER
          S_LOADING( NP, ISPC, COR ) = S_AEROSOL( NP,ISPC, COR ) * PRES_ATM_OVER_XL
        END DO
      END DO
#endif

C...set constant factors that will be used in later multiplications (moles/atm)

      XLH2O2  = H2O2H * XL
      XLO3    = O3H   * XL
      XLMHP   = MHPH  * XL
      XLPAA   = PAAH  * XL
      XLSO2   = SO2H  * XL
      XLNH3   = NH3H  * XL
      XLHCL   = HCLH  * XL
      XLHNO3  = HNO3H * XL
      XLCO2   = CO2H  * XL

      SO212   = SO21  * SO22
      SO21H   = SO21  * SO2H
      SO212H  = SO212 * SO2H
      CO212   = CO21  * CO22
      CO21H   = CO21  * CO2H
      CO212H  = CO22  * CO21H
      NH3DH20 = NH31  / H2OW
      NH31HDH = NH3H  * NH3DH20
      FOA1H   = FOA1  * FOAH
      HCL1H   = HCL1  * HCLH
      HNO31H  = HNO31 * HNO3H

C...loop If kinetic calculations are made, return to this point

      DO I20C = 1, 10001

        IF ( I20C .GE. 10000 ) THEN
          XMSG = 'EXCESSIVE LOOPING AT I20C'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
        END IF

C...set aitken-mode aerosol loading (mol/liter)

        SCAVENGED = PRES_ATM_OVER_XL * ( 1.0D0 - EXP( -REAL( ALFA3, 8 ) * TIMEW ) )
        DO ISPC = 1, NAER
          LOADING( ISPC, AKN ) = AEROSOL( ISPC, AKN ) * SCAVENGED
        END DO

#ifdef sens
        DO NP = 1, NPMAX
          DO ISPC = 1, NAER
            S_LOADING( NP, ISPC, AKN ) = S_AEROSOL( NP, ISPC, AKN ) * SCAVENGED
          END DO
        END DO
#endif

C...Initial gas phase partial pressures (atm)
C...   = initial partial pressure - amount deposited partial pressure

        INITGAS( LSO2 )  = GAS( LSO2  ) * PRES_ATM
     &                   + DS4( 0 ) * XL
     &                   - ( WETDEP( LSO3L ) + WETDEP( LHSO3L ) + WETDEP( LSO2L ) ) * XC2
        INITGAS( LNH3 )  = GAS( LNH3  ) * PRES_ATM
     &                   + ( LOADING( LNH4, ACC ) + LOADING( LNH4, COR ) + LOADING( LNH4, AKN ) ) * XL
     &                   - ( WETDEP( LNH4ACCL ) + WETDEP( LNH3L ) + WETDEP( LNH4CORL ) ) * XC2
        INITGAS( LHNO3 ) = ( GAS( LHNO3 ) + 2.0 * GAS( LN2O5 ) ) * PRES_ATM
     &                   + ( LOADING( LNO3, ACC ) + LOADING( LNO3, COR ) + LOADING( LNO3, AKN ) ) * XL
     &                   - ( WETDEP( LNO3ACCL ) + WETDEP( LHNO3L ) + WETDEP( LNO3CORL ) ) * XC2
        INITGAS( LHCL )  = GAS(  LHCL ) * PRES_ATM
     &                   + ( LOADING( LCL, ACC ) + LOADING( LCL, COR ) + LOADING( LCL, AKN ) ) * XL ! new for sea salt
     &                   - ( WETDEP( LCLACCL ) + WETDEP( LHCLL ) + WETDEP( LCLCORL ) ) * XC2
        INITGAS( LH2O2 ) = GAS( LH2O2 ) * PRES_ATM - WETDEP( LH2O2L ) * XC2
        INITGAS( LO3 )   = GAS( LO3   ) * PRES_ATM - WETDEP( LO3L   ) * XC2
        INITGAS( LFOA )  = GAS( LFOA  ) * PRES_ATM
     &                   - ( WETDEP( LFOAL ) + WETDEP( LHCO2L ) ) * XC2
        INITGAS( LMHP )  = GAS( LMHP  ) * PRES_ATM - WETDEP( LMHPL  ) * XC2
        INITGAS( LPAA )  = GAS( LPAA  ) * PRES_ATM - WETDEP( LPAAL  ) * XC2
        INITGAS( LCO2 )  = GAS( LCO2  ) * PRES_ATM
!    &                   + ( LOADING( LCACO3, COR ) + LOADING( LMGCO3, COR ) ) * XL
     &                   - ( WETDEP( LCO3L ) + WETDEP( LHCO3L ) + WETDEP( LCO2L ) ) * XC2
        INITGAS( LGLY )  = GAS( LGLY  ) * PRES_ATM
     &                   + DGLY1 * XL
     &                   - WETDEP( LGLYL ) * XC2
        INITGAS( LMGLY ) = GAS( LMGLY  ) * PRES_ATM
     &                   + DMGLY1 * XL
     &                   - WETDEP( LMGLYL ) * XC2
        INITGAS( LHO )   = GAS( LHO ) * PRES_ATM
!steadystate     &                   + DOH1 * XL
!steadystate     &                   - WETDEP( LOHL ) * XC2

#ifdef sens
        DO NP = 1, NPMAX
          S_INITGAS( NP, LSO2 )  = S_GAS( NP, LSO2  ) * PRES_ATM
     &                           + S_DS4( NP, 0 ) * XL
     &                           - ( S_WETDEP( NP, LSO3L ) + S_WETDEP( NP, LHSO3L ) + S_WETDEP( NP, LSO2L ) ) * XC2
          S_INITGAS( NP, LNH3 )  = S_GAS( NP,LNH3  ) * PRES_ATM
     &              + ( S_LOADING( NP, LNH4, ACC ) + S_LOADING( NP, LNH4, COR ) + S_LOADING( NP, LNH4, AKN ) ) * XL
     &              - ( S_WETDEP( NP, LNH4ACCL ) + S_WETDEP( NP, LNH3L ) + S_WETDEP( NP, LNH4CORL ) ) * XC2
          S_INITGAS( NP, LHNO3 ) = ( S_GAS( NP, LHNO3 ) + 2.0 * S_GAS( NP, LN2O5 ) ) * PRES_ATM
     &              + ( S_LOADING( NP, LNO3, ACC ) + S_LOADING( NP, LNO3, COR ) + S_LOADING( NP, LNO3, AKN ) ) * XL
     &              - ( S_WETDEP( NP, LNO3ACCL ) + S_WETDEP( NP, LHNO3L ) + S_WETDEP( NP, LNO3CORL ) ) * XC2
          S_INITGAS( NP, LHCL )  = S_GAS(  NP, LHCL ) * PRES_ATM
     &              + ( S_LOADING( NP, LCL, ACC ) + S_LOADING( NP, LCL, COR ) + S_LOADING( NP, LCL, AKN ) ) * XL ! new for sea salt
     &              - ( S_WETDEP( NP, LCLACCL ) + S_WETDEP( NP, LHCLL ) + S_WETDEP( NP, LCLCORL ) ) * XC2
          S_INITGAS( NP, LH2O2 ) = S_GAS( NP, LH2O2 ) * PRES_ATM - S_WETDEP( NP, LH2O2L ) * XC2
          S_INITGAS( NP, LO3 )   = S_GAS( NP, LO3   ) * PRES_ATM - S_WETDEP( NP, LO3L   ) * XC2
          S_INITGAS( NP, LFOA )  = S_GAS( NP, LFOA  ) * PRES_ATM
     &                           - ( S_WETDEP( NP, LFOAL ) + S_WETDEP( NP, LHCO2L ) ) * XC2
          S_INITGAS( NP, LMHP )  = S_GAS( NP, LMHP  ) * PRES_ATM - S_WETDEP( NP, LMHPL  ) * XC2
          S_INITGAS( NP, LPAA )  = S_GAS( NP, LPAA  ) * PRES_ATM - S_WETDEP( NP, LPAAL  ) * XC2
          S_INITGAS( NP, LCO2 )  = S_GAS( NP, LCO2  ) * PRES_ATM
     &                           - ( S_WETDEP( NP, LCO3L ) + S_WETDEP( NP, LHCO3L ) + S_WETDEP( NP, LCO2L ) ) * XC2
          S_INITGAS( NP, LGLY )  = S_GAS( NP, LGLY  ) * PRES_ATM
     &                           + S_DGLY1( NP ) * XL
     &                           - S_WETDEP( NP, LGLYL ) * XC2
          S_INITGAS( NP, LMGLY ) = S_GAS( NP, LMGLY  ) * PRES_ATM
     &                           + S_DMGLY1( NP ) * XL
     &                           - S_WETDEP( NP, LMGLYL ) * XC2
          S_INITGAS( NP, LHO )   = S_GAS( NP, LHO ) * PRES_ATM
        END DO
#endif


C...don`t allow gas concentrations to go below zero

C...Molar concentrations of soluble aerosols
C...   = Initial amount - amount deposited  (mol/liter)

        TS6COR  = MAX( LOADING( LSO4,  COR ) - WETDEP( LTS6CORL ) * XC1, 0.0D0 )
        NO3COR  = MAX( LOADING( LNO3,  COR ) - WETDEP( LNO3CORL ) * XC1, 0.0D0 )
!       NACOR   = MAX( LOADING( LNA,   COR ) - WETDEP( LNACORL  ) * XC1, 0.0D0 ) ! SLN 29March2011
        CLCOR   = MAX( LOADING( LCL,   COR ) - WETDEP( LCLCORL  ) * XC1, 0.0D0 )
        NH4COR  = MAX( LOADING( LNH4,  COR ) - WETDEP( LNH4CORL ) * XC1, 0.0D0 )
        SOILCOR = MAX( LOADING( LSOILC,COR ) - WETDEP( LSOILCL  ) * XC1, 0.0D0 ) ! SLN 16March2011
        ANTHCOR = MAX( LOADING( LANTHC,COR ) - WETDEP( LANTHCL  ) * XC1, 0.0D0 ) ! SLN 16March2011
        SEASCOR = MAX( LOADING( LSEASC,COR ) - WETDEP( LSEASCL  ) * XC1, 0.0D0 ) ! SLN 16March2011
        FECOR   = SOIL_FE_FAC * SOILCOR + CORS_FE_FAC * ANTHCOR     ! SLN 22Mar2011
        MNCOR   = SOIL_MN_FAC * SOILCOR + CORS_MN_FAC * ANTHCOR
        NACOR   = SEAS_NA_FAC * SEASCOR + SOIL_NA_FAC * SOILCOR + CORS_NA_FAC * ANTHCOR
        MGCOR   = SEAS_MG_FAC * SEASCOR + SOIL_MG_FAC * SOILCOR + CORS_MG_FAC * ANTHCOR
        CACOR   = SEAS_CA_FAC * SEASCOR + SOIL_CA_FAC * SOILCOR + CORS_CA_FAC * ANTHCOR
        KCOR    = SEAS_K_FAC  * SEASCOR + SOIL_K_FAC  * SOILCOR + CORS_K_FAC  * ANTHCOR

        TS6     = LOADING( LSO4,  AKN ) + LOADING( LSO4, ACC ) + TS6COR
     &          - ( WETDEP( LSO4ACCL ) + WETDEP( LHSO4ACCL ) ) * XC1
     &          - DS4( 0 )
        IF ( STM ) THEN
          TS6AQH2O2 = LOADING( LSO4AQH2O2, ACC ) - WETDEP( LTS6AQH2O2L ) * XC1
     &              - DS4( 1 )
          TS6AQO3   = LOADING( LSO4AQO3, ACC ) - WETDEP( LTS6AQO3L ) * XC1
     &              - DS4( 2 )
          TS6AQFEMN = LOADING( LSO4AQFEMN, ACC ) - WETDEP( LTS6AQFEMNL ) * XC1
     &              - DS4( 3 )
          TS6AQMHP  = LOADING( LSO4AQMHP, ACC ) - WETDEP( LTS6AQMHPL ) * XC1
     &              - DS4( 4 )
          TS6AQPAA  = LOADING( LSO4AQPAA, ACC ) - WETDEP( LTS6AQPAAL ) * XC1
     &              - DS4( 5 )
        END IF

        NA      = LOADING( LNA,   ACC ) + LOADING( LNA, AKN ) + NACOR
     &          - WETDEP( LNAACCL ) * XC1
!       CA      = LOADING( LCACO3,COR ) - WETDEP( LCAL ) * XC1
!       MG      = LOADING( LMGCO3,COR ) - WETDEP( LMGL ) * XC1
!       K       = LOADING( LK,    COR ) - WETDEP( LKL  ) * XC1
!       FE      = LOADING( LA3FE, COR ) - WETDEP( LFEL ) * XC1
!       MN      = LOADING( LB2MN, COR ) - WETDEP( LMNL ) * XC1
        CA      = LOADING( LCAACC, ACC) - WETDEP( LCAACCL ) * XC1 + CACOR
        MG      = LOADING( LMGACC, ACC) - WETDEP( LMGACCL ) * XC1 + MGCOR
        K       = LOADING( LKACC,  ACC) - WETDEP( LKACCL  ) * XC1 + KCOR
        FE      = LOADING( LFEACC, ACC) - WETDEP( LFEACCL ) * XC1 + FECOR
        MN      = LOADING( LMNACC, ACC) - WETDEP( LMNACCL ) * XC1 + MNCOR
        SOA     = LOADING( LSOA,  ACC ) + LOADING( LSOA, AKN ) - WETDEP( LSOAL  ) * XC1
        ORGC    = LOADING( LORGC, ACC ) + DORGC - WETDEP( LORGCL ) * XC1             ! new in-cloud organic
        POA     = LOADING( LPOA,  ACC ) + LOADING( LPOA, AKN ) - WETDEP( LPOAL ) * XC1
        EC      = LOADING( LEC,   ACC ) + LOADING( LEC,   AKN ) - WETDEP( LECL   ) * XC1
        PRIM    = LOADING( LPRI,  ACC ) + LOADING( LPRI,  AKN ) - WETDEP( LPRIML ) * XC1
!       PRIMCOR = LOADING( LPRICOR, COR ) - WETDEP( LPRIMCORL ) * XC1
        NUMCOR  = LOADING( LNUM,  COR ) - WETDEP( LNUMCORL  ) * XC1
!       A       = 3.0D0 * FE
!       B       = 2.0D0 * MN
	
        TRACI = LOADING( LTRACER_AKN, AKN ) - WETDEP( LTRACIL ) * XC1
	POAI = LOADING( LPOA, AKN ) - WETDEP( LPOAIL ) * XC1
	PRII = LOADING( LPRI, AKN ) - WETDEP( LPRIIL ) * XC1
	SOAI = LOADING( LSOA, AKN ) - WETDEP( LSOAIL ) * XC1

!       FE_OX = 0.5D0 * 0.62D0 * FE      ! SLN 28March2011
!       MN_OX = 1.0D0 * 0.84D0 * MN      ! SLN 28March2011

# ifdef sens
        DO NP = 1, NPMAX
          S_TS6COR(NP)  = S_LOADING( NP, LSO4,  COR ) - S_WETDEP( NP, LTS6CORL ) * XC1
          S_NO3COR(NP)  = S_LOADING( NP, LNO3,  COR ) - S_WETDEP( NP, LNO3CORL ) * XC1
          S_CLCOR(NP)   = S_LOADING( NP, LCL,   COR ) - S_WETDEP( NP, LCLCORL  ) * XC1
          S_NH4COR(NP)  = S_LOADING( NP, LNH4,  COR ) - S_WETDEP( NP, LNH4CORL ) * XC1
          S_SOILCOR(NP) = S_LOADING( NP, LSOILC,COR ) - S_WETDEP( NP, LSOILCL  ) * XC1
          S_ANTHCOR(NP) = S_LOADING( NP, LANTHC,COR ) - S_WETDEP( NP, LANTHCL  ) * XC1
          S_SEASCOR(NP) = S_LOADING( NP, LSEASC,COR ) - S_WETDEP( NP, LSEASCL  ) * XC1
          S_FECOR(NP)   = SOIL_FE_FAC * S_SOILCOR( NP ) + CORS_FE_FAC * S_ANTHCOR( NP )
          S_MNCOR(NP)   = SOIL_MN_FAC * S_SOILCOR( NP ) + CORS_MN_FAC * S_ANTHCOR( NP )
          S_NACOR(NP)   = SEAS_NA_FAC * S_SEASCOR( NP ) + SOIL_NA_FAC * S_SOILCOR( NP ) + CORS_NA_FAC * S_ANTHCOR( NP )
          S_MGCOR(NP)   = SEAS_MG_FAC * S_SEASCOR( NP ) + SOIL_MG_FAC * S_SOILCOR( NP ) + CORS_MG_FAC * S_ANTHCOR( NP )
          S_CACOR(NP)   = SEAS_CA_FAC * S_SEASCOR( NP ) + SOIL_CA_FAC * S_SOILCOR( NP ) + CORS_CA_FAC * S_ANTHCOR( NP )
          S_KCOR(NP)    = SEAS_K_FAC  * S_SEASCOR( NP ) + SOIL_K_FAC  * S_SOILCOR( NP ) + CORS_K_FAC  * S_ANTHCOR( NP )

          S_TS6(NP)     = S_LOADING( NP, LSO4,  AKN ) + S_LOADING( NP, LSO4, ACC ) + S_TS6COR( NP )
     &            - ( S_WETDEP( NP, LSO4ACCL ) + S_WETDEP( NP, LHSO4ACCL ) ) * XC1
     &            - S_DS4( NP, 0 )
          S_NA(NP)      = S_LOADING( NP, LNA,   ACC ) + S_LOADING( NP, LNA, AKN ) + S_NACOR( NP )
     &            - S_WETDEP( NP, LNAACCL ) * XC1
          S_CA(NP)      = S_LOADING( NP, LCAACC, ACC) - S_WETDEP( NP, LCAACCL ) * XC1 + S_CACOR( NP )
          S_MG(NP)      = S_LOADING( NP, LMGACC, ACC) - S_WETDEP( NP, LMGACCL ) * XC1 + S_MGCOR( NP )
          S_K(NP)       = S_LOADING( NP, LKACC,  ACC) - S_WETDEP( NP, LKACCL  ) * XC1 + S_KCOR( NP )
          S_FE(NP)      = S_LOADING( NP, LFEACC, ACC) - S_WETDEP( NP, LFEACCL ) * XC1 + S_FECOR( NP )
          S_MN(NP)      = S_LOADING( NP, LMNACC, ACC) - S_WETDEP( NP, LMNACCL ) * XC1 + S_MNCOR( NP )
          S_SOA(NP)     = S_LOADING( NP, LSOA,  ACC ) + S_LOADING( NP, LSOA, AKN ) - S_WETDEP( NP, LSOAL  ) * XC1
          S_ORGC(NP)    = S_LOADING( NP, LORGC, ACC ) + S_DORGC( NP ) - S_WETDEP( NP, LORGCL ) * XC1
          S_POA(NP)     = S_LOADING( NP, LPOA,  ACC ) + S_LOADING( NP, LPOA, AKN ) - S_WETDEP( NP, LPOAL ) * XC1
          S_EC(NP)      = S_LOADING( NP, LEC,   ACC ) + S_LOADING( NP, LEC,   AKN ) - S_WETDEP( NP, LECL   ) * XC1
          S_PRIM(NP)    = S_LOADING( NP, LPRI,  ACC ) + S_LOADING( NP, LPRI,  AKN ) - S_WETDEP( NP, LPRIML ) * XC1
          S_NUMCOR(NP)  = S_LOADING( NP, LNUM,  COR ) - S_WETDEP( NP, LNUMCORL  ) * XC1

        END DO
#endif


C...don't allow aerosol concentrations to go below zero

        TS6     = MAX( TS6,     0.0D0 )
        IF ( STM ) THEN
          TS6AQH2O2 = MAX( TS6AQH2O2, 0.0D0 )
          TS6AQO3   = MAX( TS6AQO3,   0.0D0 )
          TS6AQFEMN = MAX( TS6AQFEMN, 0.0D0 )
          TS6AQMHP  = MAX( TS6AQMHP,  0.0D0 )
          TS6AQPAA  = MAX( TS6AQPAA,  0.0D0 )
        END IF
        NA      = MAX( NA,      0.0D0 )
        CA      = MAX( CA,      0.0D0 )
        MG      = MAX( MG,      0.0D0 )
        K       = MAX( K,       0.0D0 )
        FE      = MAX( FE,      0.0D0 )
        MN      = MAX( MN,      0.0D0 )
        SOA     = MAX( SOA,     0.0D0 )
        ORGC    = MAX( ORGC,    0.0D0 )
        POA     = MAX( POA,     0.0D0 )
        EC      = MAX( EC,      0.0D0 )
        PRIM    = MAX( PRIM,    0.0D0 )
!       PRIMCOR = MAX( PRIMCOR, 0.0D0 )
        NUMCOR  = MAX( NUMCOR,  0.0D0 )
	
	TRACI    = MAX( TRACI,     0.0D0 )
	POAI     = MAX( POAI,     0.0D0 )
	PRII     = MAX( PRII,     0.0D0 )
	SOAI     = MAX( SOAI,     0.0D0 )
	
        FE_OX = FE_III * FE_SOL * FE     ! GS 01July2011
        MN_OX = MN_II  * MN_SOL * MN     ! GS 01July2011

        A = 3.0D0 * FE_OX
        B = 2.0D0 * MN_OX

        SK6TS6 = SK6 * TS6

#ifdef sens
        IF ( TS6 .EQ. 0.0D0 ) THEN
          DO NP =1 , NPMAX
            S_TS6( NP ) = 0.0D0
          END DO
        END IF

        IF ( NA .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_NA( NP ) = 0.0D0
          END DO
        END IF

        IF ( CA .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_CA( NP ) = 0.0D0
          END DO
        END IF

        IF ( MG .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_MG( NP ) = 0.0D0
          END DO
        END IF

        IF ( K .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_K( NP ) = 0.0D0
          END DO
        END IF

        IF ( FE .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_FE( NP ) = 0.0D0
          END DO
        END IF

        IF ( MN .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_MN( NP ) = 0.0D0
          END DO
        END IF

        IF ( SOA .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_SOA( NP ) = 0.0D0
          END DO
        END IF

        IF ( ORGC .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_ORGC( NP ) = 0.0D0
          END DO
        END IF

        IF ( POA .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_POA( NP ) = 0.0D0
          END DO
        END IF

        IF ( EC .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_EC( NP ) = 0.0D0
          END DO
        END IF

        IF ( PRIM .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_PRIM( NP ) = 0.0D0
          END DO
        END IF

        IF ( NUMCOR .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_NUMCOR( NP ) = 0.0D0
          END DO
        END IF

        DO NP = 1, NPMAX

          S_FE_OX( NP ) = FE_III * FE_SOL * S_FE( NP )
          S_MN_OX( NP ) = MN_II  * MN_SOL * S_MN( NP )

          S_A( NP ) = 3.0D0 * S_FE_OX( NP )
          S_B( NP ) = 2.0D0 * S_MN_OX( NP )

          S_SK6TS6( NP ) = SK6 * S_TS6( NP )

        END DO
#endif


C...find solution of the equation using a method of reiterative
C...  bisections Make initial guesses for pH:   between .01  to  10.

        HA =  0.01D0
        HB = 10.0D0

C...don't allow gas concentrations to go below zero

        DO IGAS = 1, NGAS
          INITGAS( IGAS ) = MAX( INITGAS( IGAS ), 0.0D0 )
        END DO

#ifdef sens
        DO IGAS = 1, NGAS
          IF ( INITGAS( IGAS ) .EQ. 0.0D0 ) THEN
            DO NP = 1, NPMAX
              S_INITGAS( NP, IGAS ) = 0.0D0
            END DO
          END IF
        END DO
#endif

C...Aerosol specific to TXHG Versions

        TRACER     = LOADING( LTRACER_ACC,  ACC ) + LOADING( LTRACER_AKN,  AKN )
     &             - WETDEP( LTRACERL ) * XC1
        TRACER    = MAX( TRACER,    0.0D0 )
        
        TRACERCOR  = LOADING( LTRACER_COR,  COR ) - WETDEP( LTRACERCORL ) * XC1
        TRACERCOR = MAX( TRACERCOR, 0.0D0 )
 
        HGFINE    = LOADING( LPHG_ACC,  ACC ) + LOADING( LPHG_AKN,  AKN )
     &             - WETDEP( LPHGFINEL ) * XC1
        HGFINE    = MAX( HGFINE,    0.0D0 )

        HGCOR     = LOADING( LPHG_COR, COR ) - WETDEP( LPHGCORL ) * XC1
        HGCOR     = MAX( HGCOR ,     0.0D0 )

        DO I7777C = 1, 10001

          IF ( I7777C .GE. 10000 ) THEN
            XMSG = 'EXCESSIVE LOOPING AT I7777C'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
          END IF

          HA = MAX( HA - 0.8D0, 0.1D0 )
          HB = MIN( HB + 0.8D0, 9.9D0 )
          AE = 10.0D0 ** ( -HA )

          RECIPA1 = 1.0D0 / ( AE * ACT1 )
          RECIPA2 = 1.0D0 / ( AE * AE * ACT2 )

C...calculate final gas phase partial pressure of SO2, NH3, HNO3
C...  HCOOH, and CO2 (atm)

          PSO2F = INITGAS( LSO2 ) / ( 1.0D0 + XLSO2 * ( 1.0D0 + SO21 * RECIPA1
     &          + SO212 * RECIPA2 ) )

          PNH3F = INITGAS( LNH3 ) / ( 1.0D0 + XLNH3 * ( 1.0D0 + NH3DH20 * AE ) )

          PHCLF = INITGAS( LHCL ) / ( 1.0D0 + XLHCL *  ( 1.0D0 + HCL1 * RECIPA1 ) )

          PFOAF = INITGAS( LFOA ) / ( 1.0D0 + XL * ( FOAH + FOA1H * RECIPA1 ) )

          PHNO3F = INITGAS( LHNO3 ) / ( 1.0D0 + XLHNO3 * ( 1.0D0 + HNO31 * RECIPA1 ) )

          PCO2F = INITGAS( LCO2 ) / ( 1.0D0 + XLCO2 * ( 1.0D0 + CO21 * RECIPA1
     &          + CO212 * RECIPA2 ) )

C...calculate liquid phase concentrations (moles/liter)

          SO4  = SK6TS6 / ( AE * GM2 + SK6 )
          HSO4 = TS6 - SO4
          SO3  = SO212H  * PSO2F  * RECIPA2
          HSO3 = SO21H   * PSO2F  * RECIPA1
          CO3  = CO212H  * PCO2F  * RECIPA2
          HCO3 = CO21H   * PCO2F  * RECIPA1
          OH   = H2OW    * RECIPA1
          NH4  = NH31HDH * PNH3F  * AE
          HCO2 = FOA1H   * PFOAF  * RECIPA1
          NO3  = HNO31H  * PHNO3F * RECIPA1
          CL   = HCL1H   * PHCLF  * RECIPA1 ! new for sea salt

C...compute functional value

!         FA = AE + NH4 + NA + 2.0D0 * ( CA + MG - CO3 - SO3 - SO4 )
!    &       - OH - HCO3 - HSO3 - NO3 - HSO4 - HCO2 - CL
          FA = AE + NH4 + NA + K + 2.0D0 * ( CA + MG - CO3 - SO3 - SO4 )  ! SLN 16March2011
     &       - OH - HCO3 - HSO3 - NO3 - HSO4 - HCO2 - CL

C...Start iteration and bisection ****************<<<<<<<
          DO I30C = 1, 10000

            IF ( I30C .GE. 10000 ) THEN
              XMSG = 'EXCESSIVE LOOPING AT I30C'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

            BB = ( HA + HB ) / 2.0D0
            AE = 10.0D0 ** ( -BB )

            ICNTAQ = ICNTAQ + 1
            IF ( ICNTAQ .GE. 60000 ) THEN
              XMSG = 'Maximum AQCHEM total iterations exceeded'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

            RECIPA1 = 1.0D0 / ( AE * ACT1 )
            RECIPA2 = 1.0D0 / ( AE * AE * ACT2 )

C...calculate final gas phase partial pressure of SO2, NH3, HCL, HNO3
C...  HCOOH, and CO2 (atm)

            PSO2F = INITGAS( LSO2 ) / ( 1.0D0 + XLSO2
     &            * ( 1.0D0 + SO21 * RECIPA1 + SO212 * RECIPA2 ) )

            PNH3F = INITGAS( LNH3 ) / ( 1.0D0 + XLNH3 * ( 1.0D0 + NH3DH20 * AE ) )

            PHCLF = INITGAS( LHCL ) / ( 1.0D0 + XLHCL *  ( 1.0D0 + HCL1 * RECIPA1 ) )

            PHNO3F = INITGAS( LHNO3 ) / ( 1.0D0 + XLHNO3 * ( 1.0D0 + HNO31 * RECIPA1 ) )

            PFOAF = INITGAS( LFOA ) / ( 1.0D0 + XL * ( FOAH + FOA1H * RECIPA1 ) )

            PCO2F = INITGAS( LCO2 ) / ( 1.0D0 + XLCO2 * ( 1.0D0 + CO21 * RECIPA1
     &            + CO212 * RECIPA2 ) )

C...calculate liquid phase concentrations (moles/liter)

            SO4  = SK6TS6 / ( AE * GM2 + SK6 )
            HSO4 = TS6 - SO4
            SO3  = SO212H  * PSO2F  * RECIPA2
            HSO3 = SO21H   * PSO2F  * RECIPA1
            CO3  = CO212H  * PCO2F  * RECIPA2
            HCO3 = CO21H   * PCO2F  * RECIPA1
            OH   = H2OW    * RECIPA1
            NH4  = NH31HDH * PNH3F  * AE
            HCO2 = FOA1H   * PFOAF  * RECIPA1
            NO3  = HNO31H  * PHNO3F * RECIPA1
            CL   = HCL1H   * PHCLF  * RECIPA1 ! new for sea salt

C...compute functional value
!           FB = AE + NH4 + NA + 2.0D0 * ( CA + MG - CO3 - SO3 - SO4 )
!    &         - OH - HCO3 - HSO3 - NO3 - HSO4 - HCO2 - CL
            FB = AE + NH4 + NA + K + 2.0D0 * ( CA + MG - CO3 - SO3 - SO4 )  ! SLN 16March2011
     &         - OH - HCO3 - HSO3 - NO3 - HSO4 - HCO2 - CL

C...Calculate and check the sign of the product of the two functional values

            FTST = FA * FB
            IF ( FTST .LE. 0.0D0 ) THEN
              HB = BB
            ELSE
              HA = BB
              FA = FB
            END IF

C...Check convergence of solutions

            HTST = HA / HB
            IF ( HTST .GT. TST ) EXIT  ! exit loop I30C
          END DO   ! I30C

C...end of zero-finding routine ****************<<<<<<<<<<<<

C...compute Ionic strength and activity coefficient by the Davies equation

          STION = 0.5D0
     &          * ( AE + NH4 + OH + HCO3 + HSO3
     &              + 4.0D0 * ( SO4 + CO3 + SO3 + CA + MG + MN_OX )
     &              + NO3 + HSO4 + 9.0D0 * FE_OX + NA + K + CL + A + B + HCO2 ) ! KMF 08September2011
C     &              + 4.0D0 * ( SO4 + CO3 + SO3 + CA + MG + MN )
C     &              + NO3 + HSO4 + 9.0D0 * FE + NA + K + CL + A + B + HCO2 )
          GM1LOG = -0.509D0 * ( SQRT( STION )
     &           / ( 1.0D0 + SQRT( STION ) ) - 0.2D0 * STION )
          GM2LOG = GM1LOG * 4.0D0
          GM1  = 10.0D0 ** GM1LOG
          GM2  = MAX( 10.0D0 ** GM2LOG, 1.0D-30 )
          ACTB = ACT1
          ACT1 = MAX( GM1 * GM1, 1.0D-30 )
          ACT2 = MAX( GM1 * GM1 * GM2, 1.0D-30 )

#ifdef verbose
          if ( stion .gt. 1.0 ) then
             write( logdev,'( /5x, a, 2i4, i10.6 )' )
     &                     'aqchem-I7777C,I20C: ', i7777c, i20c, jtime
             write( logdev,'( 5x, a, e10.3 )' ) 'stion: ', stion
             write( logdev,'( 5x, a, e10.3 )' ) 'AE:   ', ae
             write( logdev,'( 5x, a, e10.3 )' ) 'NH4:  ', nh4
             write( logdev,'( 5x, a, e10.3 )' ) 'OH:   ', oh
             write( logdev,'( 5x, a, e10.3 )' ) 'HCO3: ', hco3
             write( logdev,'( 5x, a, e10.3 )' ) 'HSO3: ', hso3
             write( logdev,'( 5x, a, e10.3 )' ) 'SO4:  ', so4
             write( logdev,'( 5x, a, e10.3 )' ) 'CO3:  ', co3
             write( logdev,'( 5x, a, e10.3 )' ) 'SO3:  ', so3
             write( logdev,'( 5x, a, e10.3 )' ) 'CA:   ', ca
             write( logdev,'( 5x, a, e10.3 )' ) 'MG:   ', mg
             write( logdev,'( 5x, a, e10.3 )' ) 'MN:   ', mn
             write( logdev,'( 5x, a, e10.3 )' ) 'NO3:  ', no3
             write( logdev,'( 5x, a, e10.3 )' ) 'HSO4: ', hso4
             write( logdev,'( 5x, a, e10.3 )' ) 'FE:   ', fe
             write( logdev,'( 5x, a, e10.3 )' ) 'NA:   ', na
             write( logdev,'( 5x, a, e10.3 )' ) 'K:    ', k
             write( logdev,'( 5x, a, e10.3 )' ) 'CL:   ', cl
             write( logdev,'( 5x, a, e10.3 )' ) 'A:    ', a
             write( logdev,'( 5x, a, e10.3 )' ) 'B:    ', b
             write( logdev,'( 5x, a, e10.3 )' ) 'HCO2: ', hco2
             write( logdev,'( 5x, a, e10.3 )' ) 'gm1log:', gm1log
             write( logdev,'( 5x, a, e10.3 )' ) 'gm2log:', gm2log
             write( logdev,'( 5x, a, e10.3 )' ) 'gm1:   ', gm1
             write( logdev,'( 5x, a, e10.3 )' ) 'gm2:   ', gm2
             write( logdev,'( 5x, a, e10.3 )' ) 'actb:  ', actb
             write( logdev,'( 5x, a, e10.3 )' ) 'act1:  ', act1
             write( logdev,'( 5x, a, e10.3 )' ) 'act2:  ', act2
          end if
#endif

C...check for convergence and possibly go to I7777C, to recompute
C...  Gas and liquid phase concentrations

          TAC = ABS( ACTB - ACT1 ) / ACTB
          IF ( TAC .LT. 1.0D-2 ) EXIT    ! exit loop I7777C
        END DO     ! end of do loop I7777C

C...return an error if the pH is not in range

        IF ( ( HA .LT. 0.1D0 ) .OR. ( HA .GT. 9.9D0 ) ) THEN
!         write( logdev,* ) ha
          XMSG = 'PH VALUE OUT OF RANGE'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
        END IF

#ifdef sens
        DO NP = 1, NPMAX
C...calculate final gas phase partial pressure of SO2, NH3, HCL, HNO3
C...  HCOOH, and CO2 (atm)
            S_PSO2F( NP ) = S_INITGAS( NP, LSO2 ) / ( 1.0D0 + XLSO2
     &                    * ( 1.0D0 + SO21 * RECIPA1 + SO212 * RECIPA2 ) )
            S_PNH3F( NP ) = S_INITGAS( NP, LNH3 ) / ( 1.0D0 + XLNH3 * ( 1.0D0 + NH3DH20 * AE ) )
            S_PHCLF( NP ) = S_INITGAS( NP, LHCL ) / ( 1.0D0 + XLHCL *  ( 1.0D0 + HCL1 * RECIPA1 ) )
            S_PHNO3F( NP ) = S_INITGAS( NP, LHNO3 ) / ( 1.0D0 + XLHNO3 * ( 1.0D0 + HNO31 * RECIPA1 ) )
            S_PFOAF( NP ) = S_INITGAS( NP, LFOA ) / ( 1.0D0 + XL * ( FOAH + FOA1H * RECIPA1 ) )
            S_PCO2F( NP ) = S_INITGAS( NP, LCO2 ) / ( 1.0D0 + XLCO2 * ( 1.0D0 + CO21 * RECIPA1
     &                    + CO212 * RECIPA2 ) )
C...calculate liquid phase concentrations (moles/liter)
            S_SO4( NP )  = S_SK6TS6( NP ) / ( AE * GM2 + SK6 )
            S_HSO4( NP ) = S_TS6( NP ) - S_SO4( NP )
            S_SO3( NP )  = SO212H  * S_PSO2F( NP )  * RECIPA2
            S_HSO3( NP ) = SO21H   * S_PSO2F( NP )  * RECIPA1
            S_CO3( NP )  = CO212H  * S_PCO2F( NP )  * RECIPA2
            S_HCO3( NP ) = CO21H   * S_PCO2F( NP )  * RECIPA1
            S_NH4( NP )  = NH31HDH * S_PNH3F( NP )  * AE
            S_HCO2( NP ) = FOA1H   * S_PFOAF( NP )  * RECIPA1
            S_NO3( NP )  = HNO31H  * S_PHNO3F( NP ) * RECIPA1
            S_CL( NP )   = HCL1H   * S_PHCLF( NP )  * RECIPA1
C...compute functional value
c           FB = AE + NH4 + NA + K + 2.0D0 * ( CA + MG - CO3 - SO3 - SO4 )
c    &         - OH - HCO3 - HSO3 - NO3 - HSO4 - HCO2 - CL
        END DO
#endif

C...Make those concentration calculations which can be made outside
C...  of the function.

        SO2L = SO2H * PSO2F
        HPLUS = 10.0D0 ** ( -BB )
        SIV = SO3 + HSO3 + SO2L

C...Calculate final gas phase concentrations of oxidants (atm)

        PH2O2F = ( INITGAS( LH2O2 ) + XL * DS4( 1 ) ) / ( 1.0D0 + XLH2O2 )
        PO3F   = ( INITGAS( LO3 )   + XL * DS4( 2 ) ) / ( 1.0D0 + XLO3   )
        PMHPF  = ( INITGAS( LMHP )  + XL * DS4( 4 ) ) / ( 1.0D0 + XLMHP  )
        PPAAF  = ( INITGAS( LPAA )  + XL * DS4( 5 ) ) / ( 1.0D0 + XLPAA  )
        PGLYF  = ( INITGAS( LGLY )                  ) / ( 1.0D0 + GLYH * XL )
        PMGLYF = ( INITGAS( LMGLY )                 ) / ( 1.0D0 + MGLYH * XL )
        PHOF   = ( INITGAS( LHO )                   ) / ( 1.0D0 + HOH * XL)

        PH2O2F = MAX( PH2O2F, 0.0D0 )
        PO3F   = MAX( PO3F,   0.0D0 )
        PMHPF  = MAX( PMHPF,  0.0D0 )
        PPAAF  = MAX( PPAAF,  0.0D0 )

C...Calculate liquid phase concentrations of oxidants (moles/liter)

        H2O2L = PH2O2F * H2O2H
        O3L   = PO3F   * O3H
        MHPL  = PMHPF  * MHPH
        PAAL  = PPAAF  * PAAH
        FOAL  = PFOAF  * FOAH
        NH3L  = PNH3F  * NH3H
        CO2L  = PCO2F  * CO2H
        HCLL  = PHCLF  * HCLH
        HNO3L = PHNO3F * HNO3H
        GLYL  = PGLYF  * GLYH
        MGLYL = PMGLYF * MGLYH
        OHL   = PHOF   * HOH

C...compute modal concentrations

        SO4COR  = SK6 * TS6COR / ( AE * GM2 + SK6 )
        HSO4COR = MAX( TS6COR - SO4COR, 0.0D0 )

        TS6ACC  = MAX( TS6  - TS6COR,   0.0D0 )
        SO4ACC  = MAX( SO4  - SO4COR,   0.0D0 )
        HSO4ACC = MAX( HSO4 - HSO4COR,  0.0D0 )
        NAACC   = MAX( NA   - NACOR,    0.0D0 )
        CAACC   = MAX( CA   - CACOR,    0.0D0 ) ! AE6
        MGACC   = MAX( MG   - MGCOR,    0.0D0 ) ! AE6
        KACC    = MAX( K    - KCOR,     0.0D0 ) ! AE6
        FEACC   = MAX( FE   - FECOR,    0.0D0 ) ! AE6
        MNACC   = MAX( MN   - MNCOR,    0.0D0 ) ! AE6

#ifdef sens
        DO NP = 1, NPMAX
          S_SO2L( NP )    = SO2H * S_PSO2F( NP )
          S_SIV( NP )     = S_SO3( NP ) + S_HSO3( NP ) + S_SO2L( NP )

          S_PH2O2F( NP )  = ( S_INITGAS( NP, LH2O2 ) + XL * S_DS4( NP, 1 ) ) / ( 1.0D0 + XLH2O2 )
          S_PO3F( NP )    = ( S_INITGAS( NP, LO3 )   + XL * S_DS4( NP, 2 ) ) / ( 1.0D0 + XLO3   )
          S_PMHPF( NP )   = ( S_INITGAS( NP, LMHP )  + XL * S_DS4( NP, 4 ) ) / ( 1.0D0 + XLMHP  )
          S_PPAAF( NP )   = ( S_INITGAS( NP, LPAA )  + XL * S_DS4( NP, 5 ) ) / ( 1.0D0 + XLPAA  )
          S_PGLYF( NP )   = ( S_INITGAS( NP, LGLY ) ) / ( 1.0D0 + GLYH * XL )
          S_PMGLYF( NP )  = ( S_INITGAS( NP, LMGLY ) ) / ( 1.0D0 + MGLYH * XL )
          S_PHOF( NP )    = ( S_INITGAS( NP, LHO ) ) / ( 1.0D0 + HOH * XL)
        END DO

        IF ( PH2O2F .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_PH2O2F( NP ) = 0.0D0
          END DO
        END IF

        IF ( PO3F .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_PO3F( NP ) = 0.0D0
          END DO
        END IF

        IF ( PMHPF .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_PMHPF( NP ) = 0.0D0
          END DO
        END IF

        IF ( PPAAF .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_PPAAF( NP ) = 0.0D0
          END DO
        END IF

        DO NP = 1, NPMAX
          S_H2O2L( NP )   = S_PH2O2F( NP ) * H2O2H
          S_O3L( NP )     = S_PO3F( NP )   * O3H
          S_MHPL( NP )    = S_PMHPF( NP )  * MHPH
          S_PAAL( NP )    = S_PPAAF( NP )  * PAAH
          S_FOAL( NP )    = S_PFOAF( NP )  * FOAH
          S_NH3L( NP )    = S_PNH3F( NP )  * NH3H
          S_CO2L( NP )    = S_PCO2F( NP )  * CO2H
          S_HCLL( NP )    = S_PHCLF( NP )  * HCLH
          S_HNO3L( NP )   = S_PHNO3F( NP ) * HNO3H
          S_GLYL( NP )    = S_PGLYF( NP )  * GLYH
          S_MGLYL( NP )   = S_PMGLYF( NP ) * MGLYH
          S_OHL( NP )     = S_PHOF( NP )   * HOH

          S_SO4COR( NP )  = SK6 * S_TS6COR( NP ) / ( AE * GM2 + SK6 )
          S_HSO4COR( NP ) = S_TS6COR( NP ) - S_SO4COR( NP )
          S_TS6ACC( NP )  = S_TS6( NP )    - S_TS6COR( NP )
          S_SO4ACC( NP )  = S_SO4( NP )    - S_SO4COR( NP )
          S_HSO4ACC( NP ) = S_HSO4( NP )   - S_HSO4COR( NP )
          S_NO3ACC( NP )  = S_NO3( NP )    - S_NO3COR( NP )
          S_NAACC( NP )   = S_NA( NP )     - S_NACOR( NP )
          S_CLACC( NP )   = S_CL( NP )     - S_CLCOR( NP )
          S_NH4ACC( NP )  = S_NH4( NP )    - S_NH4COR( NP )
          S_CAACC( NP )   = S_CA( NP )     - S_CACOR( NP )
          S_MGACC( NP )   = S_MG( NP )     - S_MGCOR( NP )
          S_KACC( NP )    = S_K( NP )      - S_KCOR( NP )
          S_FEACC( NP )   = S_FE( NP )     - S_FECOR( NP )
          S_MNACC( NP )   = S_MN( NP )     - S_MNCOR( NP )
        END DO

c       IF ( HSO4COR .EQ. 0.0D0 ) THEN
c         DO NP = 1, NPMAX
c           S_HSO4COR( NP ) = 0.0D0
c         END DO
c       END IF

        IF ( TS6ACC .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_TS6ACC( NP ) = 0.0D0
          END DO
        END IF

        IF ( SO4ACC .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_SO4ACC( NP ) = 0.0D0
          END DO
        END IF

        IF ( HSO4ACC .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_HSO4ACC( NP ) = 0.0D0
          END DO
        END IF

c       IF ( NO3ACC .EQ. 0.0D0 ) THEN
c         DO NP = 1, NPMAX
c           S_NO3ACC( NP ) = 0.0D0
c         END DO
c       END IF

        IF ( NAACC .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_NAACC( NP ) = 0.0D0
          END DO
        END IF

c       IF ( CLACC .EQ. 0.0D0 ) THEN
c         DO NP = 1, NPMAX
c           S_CLACC( NP ) = 0.0D0
c         END DO
c       END IF

c       IF ( NH4ACC .EQ. 0.0D0 ) THEN
c         DO NP = 1, NPMAX
c           S_NH4ACC( NP ) = 0.0D0
c         END DO
c       END IF

        IF ( CAACC .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_CAACC( NP ) = 0.0D0
          END DO
        END IF

        IF ( MGACC .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_MGACC( NP ) = 0.0D0
          END DO
        END IF

        IF ( KACC .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_KACC( NP ) = 0.0D0
          END DO
        END IF

        IF ( FEACC .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_FEACC( NP ) = 0.0D0
          END DO
        END IF

        IF ( MNACC .EQ. 0.0D0 ) THEN
          DO NP = 1, NPMAX
            S_MNACC( NP ) = 0.0D0
          END DO
        END IF
#endif

C...Avoid adding mass when the coarse mode concentration is greater
C...  than the total amount left in the aqueous phase after redistribution
C...  of a species between the gas/aqueous phases

        IF ( NO3COR .GT. NO3 ) then
           NO3ACC = (1.0D0 - FNO3COR) * NO3
           NO3COR = FNO3COR * NO3
        ELSE
           NO3ACC  = MAX( NO3  - NO3COR,   0.0D0 )
        END IF

        IF ( CLCOR .GT. CL ) then
           CLACC = (1.0D0 - FCLCOR) * CL
           CLCOR = FCLCOR * CL
        ELSE
           CLACC   = MAX( CL   - CLCOR,    0.0D0 )
        END IF

        IF ( NH4COR .GT. NH4 ) THEN
           NH4ACC = (1.0D0 - FNH4COR) * NH4
           NH4COR = FNH4COR * NH4
        ELSE
           NH4ACC  = MAX( NH4  - NH4COR,   0.0D0 )
        END IF

C...load the liquid concentration array with current values

        LIQUID( LACL      ) = HPLUS
        LIQUID( LNH4ACCL  ) = NH4ACC
        LIQUID( LCACORL   ) = CACOR
        LIQUID( LNAACCL   ) = NAACC
        LIQUID( LOHL      ) = OHL
        LIQUID( LSO4ACCL  ) = SO4ACC
        LIQUID( LHSO4ACCL ) = HSO4ACC
        LIQUID( LSO3L     ) = SO3
        LIQUID( LHSO3L    ) = HSO3
        LIQUID( LSO2L     ) = SO2L
        LIQUID( LCO3L     ) = CO3
        LIQUID( LHCO3L    ) = HCO3
        LIQUID( LCO2L     ) = CO2L
        LIQUID( LNO3ACCL  ) = NO3ACC
        LIQUID( LNH3L     ) = NH3L
        LIQUID( LCLACCL   ) = CLACC
        LIQUID( LH2O2L    ) = H2O2L
        LIQUID( LO3L      ) = O3L
        LIQUID( LFECORL   ) = FECOR
        LIQUID( LMNCORL   ) = MNCOR
        LIQUID( LAL       ) = A
        LIQUID( LFOAL     ) = FOAL
        LIQUID( LHCO2L    ) = HCO2
        LIQUID( LMHPL     ) = MHPL
        LIQUID( LPAAL     ) = PAAL
        LIQUID( LHCLL     ) = HCLL
        LIQUID( LPRIML    ) = PRIM
        LIQUID( LMGCORL   ) = MGCOR
        LIQUID( LKCORL    ) = KCOR
        LIQUID( LBL       ) = B
        LIQUID( LHNO3L    ) = HNO3L
!       LIQUID( LPRIMCORL ) = PRIMCOR
        LIQUID( LNUMCORL  ) = NUMCOR
        LIQUID( LTS6CORL  ) = TS6COR
        LIQUID( LNACORL   ) = NACOR
        LIQUID( LCLCORL   ) = CLCOR
        LIQUID( LNO3CORL  ) = NO3COR
        LIQUID( LNH4CORL  ) = NH4COR
        LIQUID( LPOAL     ) = POA
        LIQUID( LECL      ) = EC
        LIQUID( LSOAL     ) = SOA
        LIQUID( LORGCL    ) = ORGC
        LIQUID( LGLYL     ) = GLYL
        LIQUID( LMGLYL    ) = MGLYL
        LIQUID( LCAACCL   ) = CAACC   ! AE6 - SLN 16March2011
        LIQUID( LMGACCL   ) = MGACC   ! AE6 - SLN 16March2011
        LIQUID( LKACCL    ) = KACC    ! AE6 - SLN 16March2011
        LIQUID( LSOILCL   ) = SOILCOR ! AE6 - SLN 16March2011
        LIQUID( LANTHCL   ) = ANTHCOR ! AE6 - SLN 16March2011
        LIQUID( LSEASCL   ) = SEASCOR ! AE6 - SLN 16March2011
        LIQUID( LFEACCL   ) = FEACC   ! AE6 - SLN 22March2011
        LIQUID( LMNACCL   ) = MNACC   ! AE6 - SLN 22March2011
	
	LIQUID( LTRACIL ) = TRACI
	LIQUID( LPOAIL ) = POAI
	LIQUID( LPRIIL ) = PRII
	LIQUID( LSOAIL ) = SOAI
        
	IF ( STM ) THEN
          LIQUID( LTS6AQH2O2L   ) = TS6AQH2O2
          LIQUID( LTS6AQO3L     ) = TS6AQO3
          LIQUID( LTS6AQFEMNL   ) = TS6AQFEMN
          LIQUID( LTS6AQMHPL    ) = TS6AQMHP
          LIQUID( LTS6AQPAAL    ) = TS6AQPAA
        END IF

#ifdef sens
        S_LIQUID = 0.0D0
        DO NP = 1, NPMAX
c         S_LIQUID( NP, LACL      ) = S_AC( NP )    
          S_LIQUID( NP, LNH4ACCL  ) = S_NH4ACC( NP)
          S_LIQUID( NP, LCACORL   ) = S_CACOR( NP )
          S_LIQUID( NP, LNAACCL   ) = S_NAACC( NP )
          S_LIQUID( NP, LOHL      ) = S_OHL( NP )
          S_LIQUID( NP, LSO4ACCL  ) = S_SO4ACC( NP )
          S_LIQUID( NP, LHSO4ACCL ) = S_HSO4ACC( NP )
          S_LIQUID( NP, LSO3L     ) = S_SO3( NP )
          S_LIQUID( NP, LHSO3L    ) = S_HSO3( NP )
          S_LIQUID( NP, LSO2L     ) = S_SO2L( NP )
          S_LIQUID( NP, LCO3L     ) = S_CO3( NP )
          S_LIQUID( NP, LHCO3L    ) = S_HCO3( NP )
          S_LIQUID( NP, LCO2L     ) = S_CO2L( NP )
          S_LIQUID( NP, LNO3ACCL  ) = S_NO3ACC( NP )
          S_LIQUID( NP, LNH3L     ) = S_NH3L( NP )
          S_LIQUID( NP, LCLACCL   ) = S_CLACC( NP )
          S_LIQUID( NP, LH2O2L    ) = S_H2O2L( NP )
          S_LIQUID( NP, LO3L      ) = S_O3L( NP )
          S_LIQUID( NP, LFECORL   ) = S_FECOR( NP )
          S_LIQUID( NP, LMNCORL   ) = S_MNCOR( NP )
c         S_LIQUID( NP, LAL       ) = S_A( NP )
          S_LIQUID( NP, LFOAL     ) = S_FOAL( NP )
          S_LIQUID( NP, LHCO2L    ) = S_HCO2( NP )
          S_LIQUID( NP, LMHPL     ) = S_MHPL( NP )
          S_LIQUID( NP, LPAAL     ) = S_PAAL( NP )
          S_LIQUID( NP, LHCLL     ) = S_HCLL( NP )
          S_LIQUID( NP, LPRIML    ) = S_PRIM( NP )
          S_LIQUID( NP, LMGCORL   ) = S_MGCOR( NP )
          S_LIQUID( NP, LKCORL    ) = S_KCOR( NP )
c         S_LIQUID( NP, LBL       ) = S_B( NP )
          S_LIQUID( NP, LHNO3L    ) = S_HNO3L( NP )
          S_LIQUID( NP, LNUMCORL  ) = S_NUMCOR( NP )
          S_LIQUID( NP, LTS6CORL  ) = S_TS6COR( NP )
          S_LIQUID( NP, LNACORL   ) = S_NACOR( NP )
          S_LIQUID( NP, LCLCORL   ) = S_CLCOR( NP )
          S_LIQUID( NP, LNO3CORL  ) = S_NO3COR( NP )
          S_LIQUID( NP, LNH4CORL  ) = S_NH4COR( NP )
          S_LIQUID( NP, LPOAL     ) = S_POA( NP )
          S_LIQUID( NP, LECL      ) = S_EC( NP )
          S_LIQUID( NP, LSOAL     ) = S_SOA( NP )
          S_LIQUID( NP, LORGCL    ) = S_ORGC( NP )
          S_LIQUID( NP, LGLYL     ) = S_GLYL( NP )
          S_LIQUID( NP, LMGLYL    ) = S_MGLYL( NP )
          S_LIQUID( NP, LCAACCL   ) = S_CAACC( NP )
          S_LIQUID( NP, LMGACCL   ) = S_MGACC( NP )
          S_LIQUID( NP, LKACCL    ) = S_KACC( NP )
          S_LIQUID( NP, LSOILCL   ) = S_SOILCOR( NP )
          S_LIQUID( NP, LANTHCL   ) = S_ANTHCOR( NP )
          S_LIQUID( NP, LSEASCL   ) = S_SEASCOR( NP )
          S_LIQUID( NP, LFEACCL   ) = S_FEACC( NP )
          S_LIQUID( NP, LMNACCL   ) = S_MNACC( NP )
        END DO
#endif

C...Load array variable TXHG Version

        LIQUID( LTRACERL    )  = TRACER
        LIQUID( LTRACERCORL )  = TRACERCOR
        LIQUID( LPHGFINEL   )  = HGFINE
        LIQUID( LPHGCORL    )  = HGCOR


C...if the maximum cloud lifetime has not been reached, then compute
C...  the next timestep, else exit loop 20.

        IF ( TIMEW .GE. TAUCLD ) EXIT   ! exit 20 loop

C...make kinetics calculations
C...  note: DS4(i) and DSIV(I) are negative numbers!

        DTRMV = TAUCLD / 3.0D0
        IF ( ( CTHK1 .GT. 1.0D-10 ) .AND. ( PRCRATE .GT. 1.0D-10 ) )
     &     DTRMV = 3.6D0 * WTAVG * 1000.0D0 * CTHK1 / PRCRATE  ! <<<uma found bug, was .36
        DTRMV = MIN( DTRMV, 300.0D0 )
        ITERAT = ITERAT + 1

C...Define the total S(iv) available for oxidation

        TSIV = INITGAS( LSO2 ) * ONE_OVER_XL

#ifdef sens
        DO NP = 1, NPMAX
          S_TSIV( NP ) = S_INITGAS( NP,LSO2 ) * ONE_OVER_XL
        END DO
#endif

C...Calculate sulfur iv oxidation rate due to H2O2 (Jacobson, 1997)

        DSIVDT( 1 ) = -RH2O2 * H2O2L * HSO3 * HPLUS / ( 1.0D0 + 13.0D0 * HPLUS )
        TH2O2 = INITGAS( LH2O2 ) * ONE_OVER_XL
        IF ( ( DSIVDT( 1 ) .EQ. 0.0D0 ) .OR.
     &       ( TSIV  .LE. CONMIN ) .OR.
     &       ( TH2O2 .LE. CONMIN ) ) THEN
          DTW( 1 ) = DTRMV
        ELSE
          DTW( 1 ) = -0.05D0 * MIN( TH2O2, TSIV ) / DSIVDT( 1 )
        END IF

#ifdef sens
C... no sensitivity to pH now
        DO NP = 1, NPMAX
          S_TH2O2( NP ) = S_INITGAS( NP, LH2O2 ) * ONE_OVER_XL
          C1 = -RH2O2 * HPLUS / ( 1.0D0 + 13.0D0 * HPLUS )
          IF ( IPT(NP) .EQ. 4 ) THEN ! hddm
            IP1 = IHIGH(NP,1)
            IP2 = IHIGH(NP,2)
            S_DSIVDT( NP,1 ) = C1 * ( S_H2O2L( NP )  * HSO3          + S_H2O2L( IP1 ) * S_HSO3( IP2 )
     &                              + S_H2O2L( IP2 ) * S_HSO3( IP1 ) + H2O2L          * S_HSO3( NP )  )
          ELSE
            S_DSIVDT( NP,1 ) = C1 * ( S_H2O2L( NP ) * HSO3 + H2O2L * S_HSO3( NP ) )
          END IF
        END DO
#endif

C...Calculate sulfur iv oxidation rate due to O3 (Jacobson, 1997)

        DSIVDT( 2 ) = -( 2.4D4 * SO2L                                          +
     &                   3.7D5 * EXP( -18.56 * ( ( 298.0D0 / TEMP ) - 1.0D0 ) ) * HSO3 +
     &                   1.5D9 * EXP( -17.72 * ( ( 298.0D0 / TEMP ) - 1.0D0 ) ) * SO3 ) * O3L

        TO3 = INITGAS( LO3 ) * ONE_OVER_XL
        IF ( ( DSIVDT( 2 ) .EQ. 0.0D0 ) .OR.
     &       ( TSIV  .LE. CONMIN ) .OR.
     &       ( TO3 .LE. CONMIN ) ) THEN
          DTW( 2 ) = DTRMV
        ELSE
          DTW( 2 ) = -0.01D0 * MIN( TO3, TSIV ) / DSIVDT( 2 )
        END IF

#ifdef sens
        C1 = 2.4D4
        C2 = 3.7D5 * EXP( -18.56 * ( ( 298.0D0 / TEMP ) - 1.0D0 ) )
        C3 = 1.5D9 * EXP( -17.72 * ( ( 298.0D0 / TEMP ) - 1.0D0 ) )
        DO NP = 1, NPMAX
          S_TO3( NP ) = S_INITGAS( NP, LO3 ) * ONE_OVER_XL
          IF ( IPT(NP) .EQ. 4 ) THEN ! hddm
            IP1 = IHIGH(NP,1)
            IP2 = IHIGH(NP,2)
            S_DSIVDT( NP,2 ) =  -C1 * ( S_SO2L( NP )  * O3L          + S_SO2L( IP1 ) * S_O3L( IP2 )
     &                                + S_SO2L( IP2 ) * S_O3L( IP1 ) + SO2L          * S_O3L( NP ) )
     &                         - C2 * ( S_HSO3( NP )  * O3L          + S_HSO3( IP1 ) * S_O3L( IP2 )
     &                                + S_HSO3( IP2 ) * S_O3L( IP1 ) + HSO3          * S_O3L( NP ) )
     &                         - C3 * ( S_SO3 ( NP )  * O3L          + S_SO3 ( IP1 ) * S_O3L( IP2 )
     &                                + S_SO3 ( IP2 ) * S_O3L( IP1 ) + SO3           * S_O3L( NP ) )
          ELSE
            S_DSIVDT( NP,2 ) =  -C1 * ( S_SO2L( NP ) * O3L + SO2L * S_O3L( NP ) )
     &                         - C2 * ( S_HSO3( NP ) * O3L + HSO3 * S_O3L( NP ) )
     &                         - C3 * ( S_SO3 ( NP ) * O3L + SO3  * S_O3L( NP ) )
          END IF
        END DO
#endif

C...Calculate sulfur iv oxidation rate due to 02 catalyzed by Mn++ and Fe+++
C...(Martin and Goodman, 1991)
C...Implement sulfate inhibition based on Martin and Good, 1991

        DSIVDT( 3 ) = - ( 750.0D0  * MN_OX * SIV +                 ! GS 4May2011
     &                    2600.0D0 * FE_OX * SIV +                 ! GS 4May2011
     &                    1.0D10   * MN_OX * FE_OX * SIV )         ! GS 4May2011
     &                / ( 1.0D0 + 75.0D0 *( TS6**0.67D0 ))         ! GS 28Nov2012     

        IF ( ( DSIVDT( 3 ) .EQ. 0.0D0 ) .OR. ( TSIV .LE. CONMIN ) ) THEN
          DTW( 3 ) = DTRMV
        ELSE
          DTW( 3 ) = -0.1D0 * TSIV / DSIVDT( 3 )
        END IF

#ifdef sens
        DO NP = 1, NPMAX
          IF ( IPT(NP) .EQ. 4 ) THEN ! hddm
            IP1 = IHIGH(NP,1)
            IP2 = IHIGH(NP,2)
            IF (TS6 .GT. 0.0) THEN
              pwr1D = -(0.33D0*TS6**(-1.33D0)*S_TS6(IP2))
            ELSE
              pwr1D = 0.0
            END IF
            pwr1 = TS6**(-0.33D0)
            S_DSIVDT( NP, 3 ) = -((((750.0D0*(S_MN_OX(NP)*SIV+S_MN_OX(IP1)*S_SIV(IP2)+S_MN_OX(IP2)*S_SIV(IP1)+
     &        MN_OX*S_SIV(NP))+2600.0D0*(S_FE_OX(NP)*SIV+S_FE_OX(IP1)*S_SIV(IP2)+S_FE_OX(IP2)*S_SIV(IP1)+
     &        FE_OX*S_SIV(NP))+1.0D10*((S_MN_OX(NP)*FE_OX+S_MN_OX(IP1)*S_FE_OX(IP2)+S_MN_OX(IP2)*
     &        S_FE_OX(IP1)+MN_OX*S_FE_OX(NP))*SIV+(S_MN_OX(IP1)*FE_OX+MN_OX*S_FE_OX(IP1))*S_SIV(IP2)+(
     &        S_MN_OX(IP2)*FE_OX+MN_OX*S_FE_OX(IP2))*S_SIV(IP1)+MN_OX*FE_OX*S_SIV(NP)))*(1.0D0+
     &        75.0D0*TS6**0.67D0)+(750.0D0*(S_MN_OX(IP1)*SIV+MN_OX*S_SIV(IP1))+2600.0D0*(
     &        S_FE_OX(IP1)*SIV+FE_OX*S_SIV(IP1))+1.0D10*((S_MN_OX(IP1)*FE_OX+MN_OX*S_FE_OX(IP1))*SIV+
     &        MN_OX*FE_OX*S_SIV(IP1)))*75.0D0*0.67D0*TS6**(-0.33D0)*S_TS6(IP2)-75.0D0*
     &        0.67D0*((750.0D0*(S_MN_OX(IP2)*SIV+MN_OX*S_SIV(IP2))+2600.0D0*(S_FE_OX(IP2)*SIV
     &        +FE_OX*S_SIV(IP2))+1.0D10*((S_MN_OX(IP2)*FE_OX+MN_OX*S_FE_OX(IP2))*SIV+MN_OX*
     &        FE_OX*S_SIV(IP2)))*pwr1*S_TS6(IP1)+(750.0D0*MN_OX*SIV+2600.0D0*FE_OX*SIV+
     &        1.0D10*MN_OX*FE_OX*SIV)*(pwr1D*S_TS6(IP1)+pwr1*S_TS6(NP))))*(1.0D0+75.0D0*
     &        TS6**0.67D0)**2-((750.0D0*(S_MN_OX(IP1)*SIV+MN_OX*S_SIV(IP1))+2600.0D0*(
     &        S_FE_OX(IP1)*SIV+FE_OX*S_SIV(IP1))+1.0D10*((S_MN_OX(IP1)*FE_OX+MN_OX*S_FE_OX(IP1))*SIV+
     &        MN_OX*FE_OX*S_SIV(IP1)))*(1.0D0+75.0D0*TS6**0.67D0)-(750.0D0*MN_OX*SIV
     &        +2600.0D0*FE_OX*SIV+1.0D10*MN_OX*FE_OX*SIV)*75.0D0*0.67D0*pwr1*
     &        S_TS6(IP1))*2*(1.0D0+75.0D0*TS6**0.67D0)*75.0D0*0.67D0*TS6**(-0.33D0)*
     &        S_TS6(IP2))/((1.0D0+75.0D0*TS6**0.67D0)**2)**2)
          ELSE
            S_DSIVDT( NP, 3 ) = -(((750.0D0*(S_MN_OX(NP)*SIV+MN_OX*S_SIV(NP))+2600.0D0*(S_FE_OX(NP)*SIV
     &        +FE_OX*S_SIV(NP))+1.0D10*((S_MN_OX(NP)*FE_OX+MN_OX*S_FE_OX(NP))*SIV+MN_OX*FE_OX
     &        *S_SIV(NP)))*(1.0D0+75.0D0*TS6**0.67D0)-(750.0D0*MN_OX*SIV+2600.0D0*
     &        FE_OX*SIV+1.0D10*MN_OX*FE_OX*SIV)*75.0D0*0.67D0*TS6**(-0.33D0)*
     &        S_TS6(NP))/(1.0D0+75.0D0*TS6**0.67D0)**2)
          END IF
        END DO
#endif

C...Calculate sulfur oxidation rate due to MHP (Jacobson,  1997)

        DSIVDT( 4 ) = -RMHP * HPLUS * MHPL * HSO3
        TMHP = INITGAS( LMHP ) * ONE_OVER_XL
        IF ( ( DSIVDT( 4 ) .EQ. 0.0D0 ) .OR.
     &       ( TSIV  .LE. CONMIN ) .OR.
     &       ( TMHP .LE. CONMIN ) ) THEN
          DTW( 4 ) = DTRMV
        ELSE
          DTW( 4 ) = -0.1D0 * MIN( TMHP, TSIV ) / DSIVDT( 4 )
        END IF

#ifdef sens
c... no sensitivity to pH now
        DO NP = 1, NPMAX
          S_TMHP( NP ) = S_INITGAS( NP, LMHP ) * ONE_OVER_XL
          C1 = -RMHP * HPLUS
          IF ( IPT(NP) .EQ. 4 ) THEN ! hddm
            IP1 = IHIGH(NP,1)
            IP2 = IHIGH(NP,2)
            S_DSIVDT( NP, 4 ) = C1 * ( S_MHPL( NP )  * HSO3          + S_MHPL( IP1 ) * S_HSO3( IP2 )
     &                               + S_MHPL( IP2 ) * S_HSO3( IP1 ) + MHPL          * S_HSO3( NP )  )
          ELSE
            S_DSIVDT( NP, 4 ) = C1 * ( S_MHPL( NP ) * HSO3 + MHPL * S_HSO3( NP ) )
          END IF
        END DO
#endif

C...Calculate sulfur oxidation due to PAA (Jacobson,  1997)

        DSIVDT( 5 ) = -( RPAA * HPLUS + 7.00D2 ) * HSO3 * PAAL
        TPAA = INITGAS( LPAA ) * ONE_OVER_XL
        IF ( ( DSIVDT( 5 ) .EQ. 0.0D0 ) .OR.
     &       ( TSIV  .LE. CONMIN ) .OR.
     &       ( TPAA .LE. CONMIN ) ) THEN
          DTW( 5 ) = DTRMV
        ELSE
          DTW( 5 ) = -0.1D0 * MIN( TPAA, TSIV ) / DSIVDT( 5 )
        END IF

#ifdef sens
c... no sensitivity to pH now
        DO NP = 1, NPMAX
          S_TPAA( NP ) = S_INITGAS( NP, LPAA ) * ONE_OVER_XL
          IF ( IPT(NP) .EQ. 4 ) THEN ! hddm
            IP1 = IHIGH(NP,1)
            IP2 = IHIGH(NP,2)
            C1 = -( RPAA * HPLUS + 7.00D2 )
            S_DSIVDT( NP, 5 ) = C1 * ( S_HSO3( NP )  * PAAL          + S_HSO3( IP1 ) * S_PAAL( IP2 )
     &                               + S_HSO3( IP2 ) * S_PAAL( IP1 ) + HSO3          * S_PAAL( NP )  )
          ELSE
            S_DSIVDT( NP, 5 ) = C1 * ( S_HSO3( NP ) * PAAL + HSO3 * S_PAAL( NP ) )
          END IF
        END DO
#endif

C...Calculate total sulfur iv oxidation rate

        DSIVDT( 0 ) = 0.0D0
        DO IOX = 1, NUMOX
          DSIVDT( 0 ) = DSIVDT( 0 ) + DSIVDT( IOX )
        END DO

#ifdef sens
        S_DSIVDT( :, 0 ) = 0.0D0
        DO NP = 1, NPMAX
          DO IOX = 1, NUMOX
            S_DSIVDT( NP,0 ) = S_DSIVDT( NP, 0 ) + S_DSIVDT( NP, IOX )
          END DO
        END DO
#endif

C...Calculate a minimum time step required

        DTW( 0 ) = MIN( DTW( 1 ), DTW( 2 ), DTW( 3 ),
     &                  DTW( 4 ), DTW( 5 ) )

C...check for large time step

        IF ( DTW( 0 ) .GT. 8.0D+37 ) THEN
          WRITE(LOGDEV,1001) PRCRATE, DSIVDT(0), TS6, DTW(0), CTHK1, WTAVG
        ELSE

C...CALCULATE IN-CLOUD SOA PRODUCTION
C...  Reference:  Carlton, A.G., B.J. Turpin, K.E. Altieri, A. Reff,
C...  S. Seitzinger, H.J. Lim, and B. Ervens (2007), Atmospheric Oxalic
C...  Acid and SOA Production from Glyoxal: Results of Aqueous
C...  Photooxidation Experiments, Atmos. Environ., 41(35), 7588-7602.

C...Define the total glyoxal available for oxidation

          TGLY = INITGAS( LGLY ) * ONE_OVER_XL

C...Calculate GLY oxidation due to OH

          DGLYDT = -RGLY3 * GLYL * OHL

C...Define the total methylglyoxal available for oxidation

          TMGLY = INITGAS( LMGLY ) * ONE_OVER_XL

C...Calculate MGLY oxidation due to OH

          DMGLYDT = -RMGLY3 * MGLYL * OHL

!ccC...Define the total OH available for oxidation
!cc
!cc          THO = PHO0 * ONE_OVER_XL

C...Calculate OH consumption

!steadystate          DOHDT = -( RGLY3 * GLYL + RMGLY3 * MGLYL ) * OHL

#ifdef sens
          DO NP = 1, NPMAX
            S_TGLY( NP )  = S_INITGAS( NP, LGLY ) * ONE_OVER_XL
            S_TMGLY( NP ) = S_INITGAS( NP, LMGLY ) * ONE_OVER_XL
c           IF ( IPT(NP) .EQ. 4 ) THEN ! hddm
c             IP1 = IHIGH(NP,1)
c             IP2 = IHIGH(NP,2)
c             S_DGLYDT( NP ) = -RGLY3 * ( S_GLYL( NP )  * OHL          + S_GLYL( IP1 ) * S_OHL( IP2 )
c    &                                  + S_GLYL( IP2 ) * S_OHL( IP1 ) + GLYL          * S_OHL( NP ) )
c             S_DMGLYDT( NP ) = -RMGLY3 * ( S_MGLYL( NP )  * OHL + S_MGLYL( IP1 ) * S_OHL( IP2 )
c    &                                    + S_MGLYL( IP2 ) * S_OHL( IP1 ) + MGLYL          * S_OHL( NP ) )
c           ELSE
c             S_DGLYDT( NP ) = -RGLY3 * ( S_GLYL( NP ) * OHL + GLYL * S_OHL( NP ) )
c             S_DMGLYDT( NP ) = -RMGLY3 * ( S_MGLYL( NP ) * OHL + MGLYL * S_OHL( NP ) )
c           ENDIF

c assume no OH sensitivity for now, because it causes instability in sensitvities
            S_DGLYDT( NP ) = -RGLY3 * S_GLYL( NP ) * OHL
            S_DMGLYDT( NP ) = -RMGLY3 * S_MGLYL( NP ) * OHL

          END DO
#endif

C...calculate the change in sulfur iv for this time step

60        CONTINUE
          DTS6 = ABS( DTW( 0 ) * DSIVDT( 0 ) )

C...If DSIV(0), sulfur iv oxidized during this time step would be
C... less than 5% of sulfur oxidized since time 0, then double DT

          IF ( DTW( 0 ) .LE. TAUCLD ) THEN
            IF ( DTS6 .LT. 0.05D0 * TS6 ) THEN
              DTW( 0 ) = DTW( 0 ) * 2.0D0
              GO TO 60
            END IF
          END IF
        END IF
        DTW( 0 ) = MIN( DTW( 0 ), DTRMV )

C...Limit the timestep to prevent negative SO2 concentrations and mass creation
C...  for sulfate (suggested by Bonyoung Koo)

        IF ( DSIVDT( 0 ) .LT. 0.0D0 ) THEN
!         DTW( 0 ) = MIN( DTW( 0 ), -TSIV * 1.00001 / DSIVDT( 0 ) )
          DTW( 0 ) = MIN( DTW( 0 ), -TSIV / DSIVDT( 0 ) )
        END IF
!       IF ( DGLYDT .LT. 0.0 ) THEN
!         DTW( 0 ) = MIN( DTW( 0 ), -TGLY * 1.00001 / DGLYDT )
!       END IF
!       IF ( DMGLYDT .LT. 0.0 ) THEN
!         DTW( 0 ) = MIN( DTW( 0 ), -TMGLY * 1.00001 / DMGLYDT )
!       END IF
!       IF ( DOHDT .LT. 0.0 ) THEN
!         DTW( 0 ) = MIN( DTW( 0 ), -THO * 1.00001 / DOHDT )
!       END IF

C...If the total time after this time increment will be greater than
C...  TAUCLD sec., then set DTW(0) so that total time will be TAUCLD

        IF ( TIMEW + DTW( 0 ) .GT. TAUCLD ) DTW( 0 ) = TAUCLD - TIMEW
!       IF ( TS6 .LT. 1.0D-11 ) DTW( 0 ) = TAUCLD - TIMEW
!       IF ( ITERAT .GT. 100 ) DTW( 0 ) = TAUCLD - TIMEW
        IF ( ITERAT .GT. 100 ) DTW( 0 ) = MAX( 1.0D0, DTW( 0 ) )

C...limit timestep to no more than the washout time

        DTW( 0 ) = MIN( DTW( 0 ), TWASH )

C...force mass balance for the specified timestep
C...  for GLY and MGLY, assume that OH is in steady state

        DGLYDT  = MAX( DGLYDT,  -TGLY  / DTW( 0 ) )
        DMGLYDT = MAX( DMGLYDT, -TMGLY / DTW( 0 ) )

C...  for S(IV), also limit by oxidants (except assume O2 in steady state)

        DSIVDT( 1 ) = MAX( DSIVDT( 1 ), -MIN( TSIV, TH2O2 ) / DTW( 0 ) )
        DSIVDT( 2 ) = MAX( DSIVDT( 2 ), -MIN( TSIV, TO3   ) / DTW( 0 ) )
        DSIVDT( 3 ) = MAX( DSIVDT( 3 ), -TSIV / DTW( 0 ) )
        DSIVDT( 4 ) = MAX( DSIVDT( 4 ), -MIN( TSIV, TMHP  ) / DTW( 0 ) )
        DSIVDT( 5 ) = MAX( DSIVDT( 5 ), -MIN( TSIV, TPAA  ) / DTW( 0 ) )

#ifdef sens
        DO NP = 1, NPMAX

          IF ( DGLYDT .EQ. -TGLY  / DTW( 0 ) ) THEN
            S_DGLYDT( NP ) = -S_TGLY( NP ) / DTW( 0 )
          END IF

          IF ( DMGLYDT .EQ. -TMGLY / DTW( 0 ) ) THEN
            S_DMGLYDT( NP ) = - S_TMGLY( NP ) / DTW( 0 )
          END IF

          IF ( DSIVDT( 1 ) .EQ. -MIN( TSIV, TH2O2 ) / DTW( 0 ) ) THEN
              S_DSIVDT( NP, 1 ) = 0.D0
c           IF ( TSIV .LT. TH2O2 ) THEN
c             S_DSIVDT( NP, 1 ) = -S_TSIV( NP ) / DTW( 0 )
c           ELSE
c             S_DSIVDT( NP, 1 ) = -S_TH2O2( NP ) / DTW( 0 )
c           END IF
          END IF

          IF ( DSIVDT( 2 ) .EQ. -MIN( TSIV, TO3   ) / DTW( 0 ) ) THEN
              S_DSIVDT( NP, 2 ) = 0.D0
c           IF ( TSIV .LT. TO3 ) THEN
c             S_DSIVDT( NP, 2 ) = -S_TSIV( NP ) / DTW( 0 )
c           ELSE
c             S_DSIVDT( NP, 2 ) = -S_TO3( NP ) / DTW( 0 )
c           END IF
          END IF

          IF ( DSIVDT( 3 ) .EQ. -TSIV / DTW( 0 ) ) THEN
              S_DSIVDT( NP, 3 ) = 0.D0
c           S_DSIVDT( NP, 3 ) = -S_TSIV( NP ) / DTW( 0 )
          END IF

          IF ( DSIVDT( 4 ) .EQ. -MIN( TSIV, TMHP  ) / DTW( 0 ) ) THEN
              S_DSIVDT( NP, 4 ) = 0.D0
c           IF ( TSIV .LT. TMHP ) THEN
c             S_DSIVDT( NP, 4 ) = -S_TSIV( NP ) / DTW( 0 )
c           ELSE
c             S_DSIVDT( NP, 4 ) = -S_TMHP( NP ) / DTW( 0 )
c           END IF
          END IF

          IF ( DSIVDT( 5 ) .EQ. -MIN( TSIV, TPAA  ) / DTW( 0 ) ) THEN
              S_DSIVDT( NP, 5 ) = 0.D0
c           IF ( TSIV .LT. TPAA ) THEN
c             S_DSIVDT( NP, 5 ) = -S_TSIV( NP ) / DTW( 0 )
c           ELSE
c             S_DSIVDT( NP, 5 ) = -S_TPAA( NP ) / DTW( 0 )
c           END IF
          END IF

        END DO
#endif

C...  recalculate the total S(iv) oxidation rate

        DSIVDT( 0 ) = 0.0
        DO IOX = 1, NUMOX
          DSIVDT( 0 ) = DSIVDT( 0 ) + DSIVDT( IOX )
        END DO

#ifdef sens
        S_DSIVDT( :, 0 ) = 0.0D0
        DO NP = 1, NPMAX
          DO IOX = 1, NUMOX
            S_DSIVDT( NP,0 ) = S_DSIVDT( NP, 0 ) + S_DSIVDT( NP, IOX )
          END DO
        END DO
#endif

C...  if the total S(iv) oxidation rate over the timestep exceeds the amount of
C...    S(iv) available then scale the rates to conserve mass

        IF ( -DSIVDT( 0 ) * DTW( 0 ) .GT. TSIV ) THEN
          DSIV_SCALE = TSIV / ( -DSIVDT( 0 ) * DTW( 0 ) )
          DSIVDT( 0 ) = DSIVDT( 0 ) * DSIV_SCALE
          DSIVDT( 1 ) = DSIVDT( 1 ) * DSIV_SCALE
          DSIVDT( 2 ) = DSIVDT( 2 ) * DSIV_SCALE
          DSIVDT( 3 ) = DSIVDT( 3 ) * DSIV_SCALE
          DSIVDT( 4 ) = DSIVDT( 4 ) * DSIV_SCALE
          DSIVDT( 5 ) = DSIVDT( 5 ) * DSIV_SCALE
#ifdef sens
          DO NP = 1, NPMAX
            S_DSIVDT( NP, 0 ) = S_DSIVDT( NP, 0 ) * DSIV_SCALE
            S_DSIVDT( NP, 1 ) = S_DSIVDT( NP, 1 ) * DSIV_SCALE
            S_DSIVDT( NP, 2 ) = S_DSIVDT( NP, 2 ) * DSIV_SCALE
            S_DSIVDT( NP, 3 ) = S_DSIVDT( NP, 3 ) * DSIV_SCALE
            S_DSIVDT( NP, 4 ) = S_DSIVDT( NP, 4 ) * DSIV_SCALE
            S_DSIVDT( NP, 5 ) = S_DSIVDT( NP, 5 ) * DSIV_SCALE
          END DO
#endif
        END IF

C...Set DSIV(I), I = 0,NUMOX, the amount of S(IV) oxidized by each
C... individual oxidizing agent, as well as the total.

        DO IOX = 0, NUMOX
          DS4( IOX ) = DS4( IOX ) + DTW( 0 ) * DSIVDT( IOX )
        END DO

        DGLY1  = DGLY1  + DTW( 0 ) * DGLYDT

        DMGLY1 = DMGLY1 + DTW( 0 ) * DMGLYDT

csteadystate        DOH1   = DOH1   + DTW( 0 ) * DOHDT

C...Calculate AORGC Production:  4% SOAcld (ORGC) yield from glyoxal
C...  and methylglyoxal is assumed

        DORGC = DORGC - ( 0.04D0 * ( DGLYDT + DMGLYDT ) * DTW( 0 ) )

#ifdef isam
        DGLY1_SAVE  = DGLY1_SAVE  - 0.04D0 * DGLYDT  * DTW( 0 )
        DMGLY1_SAVE = DMGLY1_SAVE - 0.04D0 * DMGLYDT * DTW( 0 )
#endif


#ifdef sens
        DO NP = 1, NPMAX

          DO IOX = 0, NUMOX
            S_DS4( NP, IOX ) = S_DS4( NP, IOX ) + DTW( 0 ) * S_DSIVDT( NP, IOX )
          END DO

          S_DGLY1( NP )  = S_DGLY1( NP )  + DTW( 0 ) * S_DGLYDT( NP )

          S_DMGLY1( NP ) = S_DMGLY1( NP ) + DTW( 0 ) * S_DMGLYDT( NP )

          S_DORGC( NP )  = S_DORGC( NP ) - ( 0.04D0 * ( S_DGLYDT( NP ) + S_DMGLYDT( NP ) ) * DTW( 0 ) )

        END DO
#endif

C...Compute depositions and concentrations for each species

!!!     WETFAC = PRCRATE * FRACLIQ * DTW( 0 ) * SEC2HR     !!!numerical integration errors with this expression
        WETFAC = ( 1.0D0 - EXP( -DTW( 0 ) / TWASH ) ) / XC1  !!!new analytic soln to be used in CMAQv5.0

        DO LIQ = 1, NLIQS
          WETDEP( LIQ ) = WETDEP( LIQ ) + LIQUID( LIQ ) * WETFAC
        END DO

        TIMEW = TIMEW + DTW( 0 )

#ifdef sens
        DO NP = 1, NPMAX
          DO LIQ = 1, NLIQS
            S_WETDEP( NP, LIQ ) = S_WETDEP( NP, LIQ ) + S_LIQUID( NP, LIQ ) * WETFAC
          END DO
        END DO
#endif

      END DO     ! I20C loop

C...At this point, TIMEW=TAUCLD
C...  compute the scavenging coefficient for SO4 which will be used for
C...  scavenging aerosol number in the accumulation mode

      DEPSUM = ( WETDEP( LSO4ACCL ) + WETDEP( LHSO4ACCL ) ) * XC1

      IF ( ( LOADING( LSO4, ACC ) + LOADING( LSO4, AKN ) - DS4( 0 ) ) .NE. 0.0D0 ) THEN
        BETASO4 = DEPSUM / ( ( LOADING( LSO4, ACC ) + LOADING( LSO4, AKN ) - DS4( 0 ) ) * TAUCLD )
      ELSE
        BETASO4 = 0.0D0
      END IF

      EBETASO4T = EXP( -BETASO4 * TAUCLD )
      EALFA0T   = EXP( -ALFA0 * TAUCLD )
      EALFA2T   = EXP( -ALFA2 * TAUCLD )
      EALFA3T   = EXP( -ALFA3 * TAUCLD )

C...Compute the output concentrations and wet deposition amounts

      TOTAMM = ( PNH3F  + ( NH4ACC + NH3L  ) * XL ) * RECIPAP1
      TOTNIT = ( PHNO3F + ( NO3ACC + HNO3L ) * XL ) * RECIPAP1

#ifdef sens
      DO NP = 1, NPMAX
         S_TOTAMM( NP ) = ( S_PNH3F( NP )  + ( S_NH4ACC( NP ) + S_NH3L( NP )  ) * XL ) * RECIPAP1
         S_TOTNIT( NP ) = ( S_PHNO3F( NP ) + ( S_NO3ACC( NP ) + S_HNO3L( NP ) ) * XL ) * RECIPAP1
      END DO
#endif

C...gas-phase species wet deposition (mm mol/lit)

      GASWDEP( LSO2   ) = WETDEP( LSO3L  ) + WETDEP( LHSO3L )
     &                  + WETDEP( LSO2L  )
      GASWDEP( LNH3   ) = WETDEP( LNH3L  )
      GASWDEP( LH2O2  ) = WETDEP( LH2O2L )
      GASWDEP( LO3    ) = WETDEP( LO3L   )
      GASWDEP( LCO2   ) = WETDEP( LCO3L  ) + WETDEP( LHCO3L )
     &                  + WETDEP( LCO2L  )
      GASWDEP( LFOA   ) = WETDEP( LFOAL  ) + WETDEP( LHCO2L )
      GASWDEP( LMHP   ) = WETDEP( LMHPL  )
      GASWDEP( LPAA   ) = WETDEP( LPAAL  )
      GASWDEP( LHCL   ) = WETDEP( LHCLL  )
      GASWDEP( LHNO3  ) = WETDEP( LHNO3L )
      GASWDEP( LN2O5  ) = 0.0D0
      GASWDEP( LH2SO4 ) = 0.0D0
      GASWDEP( LGLY   ) = WETDEP( LGLYL  )
      GASWDEP( LMGLY  ) = WETDEP( LMGLYL )
!     GASWDEP( LHO    ) = WETDEP( LOHL   )

#ifdef sens
      DO NP = 1, NPMAX
        S_GASWDEP( NP, LSO2   ) = S_WETDEP( NP, LSO3L  ) + S_WETDEP( NP, LHSO3L )
     &                          + S_WETDEP( NP, LSO2L  )
        S_GASWDEP( NP, LNH3   ) = S_WETDEP( NP, LNH3L  )
        S_GASWDEP( NP, LH2O2  ) = S_WETDEP( NP, LH2O2L )
        S_GASWDEP( NP, LO3    ) = S_WETDEP( NP, LO3L   )
        S_GASWDEP( NP, LCO2   ) = S_WETDEP( NP, LCO3L  ) + S_WETDEP( NP, LHCO3L )
     &                          + S_WETDEP( NP, LCO2L  )
        S_GASWDEP( NP, LFOA   ) = S_WETDEP( NP, LFOAL  ) + S_WETDEP( NP, LHCO2L )
        S_GASWDEP( NP, LMHP   ) = S_WETDEP( NP, LMHPL  )
        S_GASWDEP( NP, LPAA   ) = S_WETDEP( NP, LPAAL  )
        S_GASWDEP( NP, LHCL   ) = S_WETDEP( NP, LHCLL  )
        S_GASWDEP( NP, LHNO3  ) = S_WETDEP( NP, LHNO3L )
        S_GASWDEP( NP, LN2O5  ) = 0.0D0
        S_GASWDEP( NP, LH2SO4 ) = 0.0D0
        S_GASWDEP( NP, LGLY   ) = S_WETDEP( NP, LGLYL  )
        S_GASWDEP( NP, LMGLY  ) = S_WETDEP( NP, LMGLYL )
      END DO
#endif

C...gas concentrations (mol/molV)

      GAS( LSO2   ) = ( PSO2F  + XL *  SIV )   * RECIPAP1
      GAS( LH2O2  ) = ( PH2O2F + XL *  H2O2L ) * RECIPAP1
      GAS( LO3    ) = ( PO3F   + XL *  O3L )   * RECIPAP1
      GAS( LCO2   ) = ( PCO2F  + XL *  CO2L )  * RECIPAP1
      GAS( LFOA   ) = ( PFOAF  + XL * ( FOAL + HCO2 ) ) * RECIPAP1
      GAS( LMHP   ) = ( PMHPF  + XL *  MHPL )  * RECIPAP1
      GAS( LPAA   ) = ( PPAAF  + XL *  PAAL )  * RECIPAP1
      GAS( LHCL   ) = ( PHCLF  + XL *  HCLL )  * RECIPAP1
      GAS( LGLY   ) = ( PGLYF  + XL *  GLYL )  * RECIPAP1
      GAS( LMGLY  ) = ( PMGLYF + XL *  MGLYL)  * RECIPAP1
!     GAS( LHO    ) = ( PHOF   + XL *  OHL  )  * RECIPAP1

      GAS( LNH3   ) = FNH3  * TOTAMM
      GAS( LHNO3  ) = FHNO3 * TOTNIT
      GAS( LN2O5  ) = 0.0D0 ! assume all into aerosol
      GAS( LH2SO4 ) = 0.0D0 ! assume all into aerosol

#ifdef sens
      DO NP = 1, NPMAX
        S_GAS( NP, LSO2   ) = ( S_PSO2F( NP )  + XL *  S_SIV( NP ) )   * RECIPAP1
c       S_GAS( NP, LH2O2  ) = ( S_PH2O2F( NP ) + XL *  S_H2O2L( NP ) ) * RECIPAP1
c       S_GAS( NP, LO3    ) = ( S_PO3F( NP )   + XL *  S_O3L( NP ) )   * RECIPAP1
c       S_GAS( NP, LCO2   ) = ( S_PCO2F( NP )  + XL *  S_CO2L( NP ) )  * RECIPAP1
c       S_GAS( NP, LFOA   ) = ( S_PFOAF( NP )  + XL * ( S_FOAL( NP ) + S_HCO2( NP ) ) ) * RECIPAP1
c       S_GAS( NP, LMHP   ) = ( S_PMHPF( NP )  + XL *  S_MHPL( NP ) )  * RECIPAP1
c       S_GAS( NP, LPAA   ) = ( S_PPAAF( NP )  + XL *  S_PAAL( NP ) )  * RECIPAP1
c       S_GAS( NP, LHCL   ) = ( S_PHCLF( NP )  + XL *  S_HCLL( NP ) )  * RECIPAP1
        S_GAS( NP, LGLY   ) = ( S_PGLYF( NP )  + XL *  S_GLYL( NP ) )  * RECIPAP1
        S_GAS( NP, LMGLY  ) = ( S_PMGLYF( NP ) + XL *  S_MGLYL( NP ))  * RECIPAP1
c       S_GAS( NP, LNH3   ) = S_FNH3( NP )  * S_TOTAMM( NP )
c       S_GAS( NP, LHNO3  ) = S_FHNO3( NP ) * S_TOTNIT( NP )
        S_GAS( NP, LN2O5  ) = 0.0D0 ! assume all into aerosol
        S_GAS( NP, LH2SO4 ) = 0.0D0 ! assume all into aerosol
      END DO
#endif

C...aerosol species wet deposition (mm mol/lit)
C...  there is no wet deposition of aitken particles, they attached
C...  to the accumulation mode particles

      AERWDEP( LSO4, AKN ) = 0.0D0
      AERWDEP( LNH4, AKN ) = 0.0D0
      AERWDEP( LNO3, AKN ) = 0.0D0
      AERWDEP( LEC,  AKN ) = 0.0D0
      AERWDEP( LPRI, AKN ) = 0.0D0

      AERWDEP( LPOA, AKN ) = 0.0D0
      AERWDEP( LSOA, AKN ) = 0.0D0

      AERWDEP( LSO4, ACC ) = WETDEP( LSO4ACCL ) + WETDEP( LHSO4ACCL )
      IF ( STM ) THEN
        AERWDEP( LSO4AQH2O2, ACC ) = WETDEP( LTS6AQH2O2L )
        AERWDEP( LSO4AQO3,   ACC ) = WETDEP( LTS6AQO3L   )
        AERWDEP( LSO4AQFEMN, ACC ) = WETDEP( LTS6AQFEMNL )
        AERWDEP( LSO4AQMHP,  ACC ) = WETDEP( LTS6AQMHPL  )
        AERWDEP( LSO4AQPAA,  ACC ) = WETDEP( LTS6AQPAAL  )
      END IF
      AERWDEP( LNH4, ACC ) = WETDEP( LNH4ACCL )
      AERWDEP( LNO3, ACC ) = WETDEP( LNO3ACCL )
      AERWDEP( LEC,  ACC ) = WETDEP( LECL     )
      AERWDEP( LPRI, ACC ) = WETDEP( LPRIML   )

      AERWDEP( LSOA,  ACC ) = WETDEP( LSOAL  )
      AERWDEP( LORGC, ACC ) = WETDEP( LORGCL )
      AERWDEP( LPOA,  ACC ) = WETDEP( LPOAL )

      AERWDEP( LSO4, COR ) = WETDEP( LTS6CORL  )
      AERWDEP( LNO3, COR ) = WETDEP( LNO3CORL  )
      AERWDEP( LNH4, COR ) = WETDEP( LNH4CORL  )
!     AERWDEP( LPRICOR, COR ) = WETDEP( LPRIMCORL )

      AERWDEP( LNA, AKN  ) = 0.0D0
      AERWDEP( LCL, AKN  ) = 0.0D0
      AERWDEP( LNA, ACC  ) = WETDEP( LNAACCL )
      AERWDEP( LCL, ACC  ) = WETDEP( LCLACCL )
!     AERWDEP( LNA, COR  ) = WETDEP( LNACORL )
      AERWDEP( LCL, COR  ) = WETDEP( LCLCORL )

!     AERWDEP( LK,     COR ) = WETDEP( LKCORL  )
!     AERWDEP( LA3FE,  COR ) = WETDEP( LFECORL )
!     AERWDEP( LB2MN,  COR ) = WETDEP( LMNCORL )
!     AERWDEP( LCACO3, COR ) = WETDEP( LCACORL )
!     AERWDEP( LMGCO3, COR ) = WETDEP( LMGCORL )

      AERWDEP( LCAACC, ACC ) = WETDEP( LCAACCL )  ! AE6 - SLN 16March2011
      AERWDEP( LMGACC, ACC ) = WETDEP( LMGACCL )  ! AE6 - SLN 16March2011
      AERWDEP( LKACC,  ACC ) = WETDEP( LKACCL  )  ! AE6 - SLN 16March2011
      AERWDEP( LSOILC, COR ) = WETDEP( LSOILCL )  ! AE6 - SLN 16March2011
      AERWDEP( LANTHC, COR ) = WETDEP( LANTHCL )  ! AE6 - SLN 16March2011
      AERWDEP( LSEASC, COR ) = WETDEP( LSEASCL )  ! AE6 - SLN 16March2011
      AERWDEP( LFEACC, ACC ) = WETDEP( LFEACCL )  ! AE6 - SLN 22March2011
      AERWDEP( LMNACC, ACC ) = WETDEP( LMNACCL )  ! AE6 - SLN 22March2011

!     AERWDEP( LNUM, AKN ) = 0.0D0
!     AERWDEP( LNUM, ACC ) = 0.0D0
!     AERWDEP( LNUM, COR ) = 0.0D0
!     AERWDEP( LSRF, AKN ) = 0.0D0
!     AERWDEP( LSRF, ACC ) = 0.0D0
!     AERWDEP( LSRF, COR ) = 0.0D0

#ifdef sens
      DO NP = 1, NPMAX
        S_AERWDEP( NP, LSO4, AKN )   = 0.0D0
        S_AERWDEP( NP, LNH4, AKN )   = 0.0D0
        S_AERWDEP( NP, LNO3, AKN )   = 0.0D0
        S_AERWDEP( NP, LEC,  AKN )   = 0.0D0
        S_AERWDEP( NP, LPRI, AKN )   = 0.0D0
        S_AERWDEP( NP, LPOA, AKN )   = 0.0D0
	S_AERWDEP( NP, LSOA, AKN )   = 0.0D0
        S_AERWDEP( NP, LSO4, ACC )   = S_WETDEP( NP, LSO4ACCL ) + S_WETDEP( NP, LHSO4ACCL )
        S_AERWDEP( NP, LNH4, ACC )   = S_WETDEP( NP, LNH4ACCL )
        S_AERWDEP( NP, LNO3, ACC )   = S_WETDEP( NP, LNO3ACCL )
        S_AERWDEP( NP, LEC,  ACC )   = S_WETDEP( NP, LECL     )
        S_AERWDEP( NP, LPRI, ACC )   = S_WETDEP( NP, LPRIML   )
        S_AERWDEP( NP, LSOA,  ACC )  = S_WETDEP( NP, LSOAL  )
        S_AERWDEP( NP, LORGC, ACC )  = S_WETDEP( NP, LORGCL )
        S_AERWDEP( NP, LPOA,  ACC )  = S_WETDEP( NP, LPOAL )
        S_AERWDEP( NP, LSO4, COR )   = S_WETDEP( NP, LTS6CORL  )
        S_AERWDEP( NP, LNO3, COR )   = S_WETDEP( NP, LNO3CORL  )
        S_AERWDEP( NP, LNH4, COR )   = S_WETDEP( NP, LNH4CORL  )
        S_AERWDEP( NP, LNA, AKN  )   = 0.0D0
        S_AERWDEP( NP, LCL, AKN  )   = 0.0D0
        S_AERWDEP( NP, LNA, ACC  )   = S_WETDEP( NP, LNAACCL )
        S_AERWDEP( NP, LCL, ACC  )   = S_WETDEP( NP, LCLACCL )
        S_AERWDEP( NP, LCL, COR  )   = S_WETDEP( NP, LCLCORL )
        S_AERWDEP( NP, LCAACC, ACC ) = S_WETDEP( NP, LCAACCL )
        S_AERWDEP( NP, LMGACC, ACC ) = S_WETDEP( NP, LMGACCL )
        S_AERWDEP( NP, LKACC,  ACC ) = S_WETDEP( NP, LKACCL  )
        S_AERWDEP( NP, LSOILC, COR ) = S_WETDEP( NP, LSOILCL )
        S_AERWDEP( NP, LANTHC, COR ) = S_WETDEP( NP, LANTHCL )
        S_AERWDEP( NP, LSEASC, COR ) = S_WETDEP( NP, LSEASCL )
        S_AERWDEP( NP, LFEACC, ACC ) = S_WETDEP( NP, LFEACCL )
        S_AERWDEP( NP, LMNACC, ACC ) = S_WETDEP( NP, LMNACCL )
      END DO
#endif

C...Compute for Aerosol species in  TXHG Version

      AERWDEP( LTRACER_AKN, AKN ) = 0.0D0
      AERWDEP( LTRACER_ACC, ACC ) = WETDEP( LTRACERL )
      AERWDEP( LTRACER_COR, COR ) = WETDEP( LTRACERCORL )
      AERWDEP( LPHG_AKN, AKN ) = 0.0D0
      AERWDEP( LPHG_ACC, ACC ) = WETDEP( LPHGFINEL )
      AERWDEP( LPHG_COR, COR ) = WETDEP( LPHGCORL )

C...aerosol concentrations (mol/molV)

      AEROSOL( LSO4, AKN ) = AEROSOL( LSO4, AKN ) * EALFA3T
      AEROSOL( LNH4, AKN ) = AEROSOL( LNH4, AKN ) * EALFA3T
      AEROSOL( LNO3, AKN ) = AEROSOL( LNO3, AKN ) * EALFA3T
      AEROSOL( LEC,  AKN ) = AEROSOL( LEC,  AKN ) * EALFA3T
      AEROSOL( LPRI, AKN ) = AEROSOL( LPRI, AKN ) * EALFA3T

      AEROSOL( LPOA, AKN ) = AEROSOL( LPOA, AKN ) * EALFA3T
      AEROSOL( LSOA, AKN ) = AEROSOL( LSOA, AKN ) * EALFA3T

      AEROSOL( LSO4, ACC ) = TS6ACC * XL * RECIPAP1
      IF ( STM ) THEN
        AEROSOL( LSO4AQH2O2, ACC ) = TS6AQH2O2  * XL * RECIPAP1
        AEROSOL( LSO4AQO3,   ACC ) = TS6AQO3    * XL * RECIPAP1
        AEROSOL( LSO4AQFEMN, ACC ) = TS6AQFEMN  * XL * RECIPAP1
        AEROSOL( LSO4AQMHP,  ACC ) = TS6AQMHP   * XL * RECIPAP1
        AEROSOL( LSO4AQPAA,  ACC ) = TS6AQPAA   * XL * RECIPAP1
      END IF
      AEROSOL( LEC,  ACC ) = EC     * XL * RECIPAP1
      AEROSOL( LPRI, ACC ) = PRIM   * XL * RECIPAP1

      AEROSOL( LSOA,  ACC ) = SOA  * XL * RECIPAP1
      AEROSOL( LORGC, ACC ) = ORGC * XL * RECIPAP1
      AEROSOL( LPOA,  ACC ) = POA  * XL * RECIPAP1

      AEROSOL( LNH4, ACC ) = FNH4ACC * TOTAMM
      AEROSOL( LNO3, ACC ) = FNO3ACC * TOTNIT

      AEROSOL( LSO4, COR )    = TS6COR * XL * RECIPAP1
      AEROSOL( LNO3, COR )    = NO3COR * XL * RECIPAP1
      AEROSOL( LNH4, COR )    = NH4COR * XL * RECIPAP1
!     AEROSOL( LPRICOR, COR ) = PRIMCOR* XL * RECIPAP1
!     AEROSOL( LK, COR )      = KCOR   * XL * RECIPAP1
!     AEROSOL( LA3FE, COR )   = FECOR  * XL * RECIPAP1
!     AEROSOL( LB2MN, COR )   = MNCOR  * XL * RECIPAP1
!     AEROSOL( LCACO3, COR )  = CACOR  * XL * RECIPAP1
!     AEROSOL( LMGCO3, COR )  = MGCOR  * XL * RECIPAP1

      AEROSOL( LNA, AKN  ) = AEROSOL( LNA, AKN ) * EALFA3T
      AEROSOL( LCL, AKN  ) = AEROSOL( LCL, AKN ) * EALFA3T
      AEROSOL( LNA, ACC  ) = NAACC * XL * RECIPAP1
      AEROSOL( LCL, ACC  ) = CLACC * XL * RECIPAP1
!     AEROSOL( LNA, COR  ) = NACOR * XL * RECIPAP1
      AEROSOL( LCL, COR  ) = CLCOR * XL * RECIPAP1

      AEROSOL( LNUM, AKN ) = AEROSOL( LNUM, AKN ) * EALFA0T
      AEROSOL( LNUM, ACC ) = AEROSOL( LNUM, ACC ) * EBETASO4T
      AEROSOL( LNUM, COR ) = AEROSOL( LNUM, COR ) * EXP(-TAUCLD / TWASH )

      AEROSOL( LCAACC, ACC ) = CAACC   * XL * RECIPAP1 ! AE6 - SLN 16March2011
      AEROSOL( LMGACC, ACC ) = MGACC   * XL * RECIPAP1 ! AE6 - SLN 16March2011
      AEROSOL( LKACC,  ACC ) = KACC    * XL * RECIPAP1 ! AE6 - SLN 16March2011
      AEROSOL( LSOILC, COR ) = SOILCOR * XL * RECIPAP1 ! AE6 - SLN 16March2011
      AEROSOL( LANTHC, COR ) = ANTHCOR * XL * RECIPAP1 ! AE6 - SLN 16March2011
      AEROSOL( LSEASC, COR ) = SEASCOR * XL * RECIPAP1 ! AE6 - SLN 16March2011
      AEROSOL( LFEACC, ACC ) = FEACC   * XL * RECIPAP1 ! AE6 - SLN 22March2011
      AEROSOL( LMNACC, ACC ) = MNACC   * XL * RECIPAP1 ! AE6 - SLN 22March2011

#ifdef sens
      DO NP =1, NPMAX
        S_AEROSOL( NP, LSO4, AKN )   = S_AEROSOL( NP, LSO4, AKN ) * EALFA3T
        S_AEROSOL( NP, LNH4, AKN )   = S_AEROSOL( NP, LNH4, AKN ) * EALFA3T
        S_AEROSOL( NP, LNO3, AKN )   = S_AEROSOL( NP, LNO3, AKN ) * EALFA3T
        S_AEROSOL( NP, LEC,  AKN )   = S_AEROSOL( NP, LEC,  AKN ) * EALFA3T
        S_AEROSOL( NP, LPRI, AKN )   = S_AEROSOL( NP, LPRI, AKN ) * EALFA3T
        S_AEROSOL( NP, LPOA, AKN )   = S_AEROSOL( NP, LPOA, AKN ) * EALFA3T
	S_AEROSOL( NP, LSOA, AKN )   = S_AEROSOL( NP, LSOA, AKN ) * EALFA3T
        S_AEROSOL( NP, LSO4, ACC )   = S_TS6ACC( NP ) * XL * RECIPAP1
        S_AEROSOL( NP, LEC,  ACC )   = S_EC( NP )     * XL * RECIPAP1
        S_AEROSOL( NP, LPRI, ACC )   = S_PRIM( NP )   * XL * RECIPAP1
        S_AEROSOL( NP, LSOA,  ACC )  = S_SOA( NP )  * XL * RECIPAP1
        S_AEROSOL( NP, LORGC, ACC )  = S_ORGC( NP ) * XL * RECIPAP1
        S_AEROSOL( NP, LPOA,  ACC )  = S_POA( NP )  * XL * RECIPAP1
c       S_AEROSOL( NP, LNH4, ACC )   = S_FNH4ACC( NP ) * S_TOTAMM( NP )
c       S_AEROSOL( NP, LNO3, ACC )   = S_FNO3ACC( NP ) * S_TOTNIT( NP )
        S_AEROSOL( NP, LSO4, COR )   = S_TS6COR( NP ) * XL * RECIPAP1
c       S_AEROSOL( NP, LNO3, COR )   = S_NO3COR( NP ) * XL * RECIPAP1
c       S_AEROSOL( NP, LNH4, COR )   = S_NH4COR( NP ) * XL * RECIPAP1
c       S_AEROSOL( NP, LNA, AKN  )   = S_AEROSOL( NP, LNA, AKN ) * EALFA3T
c       S_AEROSOL( NP, LCL, AKN  )   = S_AEROSOL( NP, LCL, AKN ) * EALFA3T
c       S_AEROSOL( NP, LNA, ACC  )   = S_NAACC( NP ) * XL * RECIPAP1
c       S_AEROSOL( NP, LCL, ACC  )   = S_CLACC( NP ) * XL * RECIPAP1
c       S_AEROSOL( NP, LCL, COR  )   = S_CLCOR( NP ) * XL * RECIPAP1
c       S_AEROSOL( NP, LNUM, AKN )   = S_AEROSOL( NP, LNUM, AKN ) * EALFA0T
c       S_AEROSOL( NP, LNUM, ACC )   = S_AEROSOL( NP, LNUM, ACC ) * EBETASO4T
c       S_AEROSOL( NP, LNUM, COR )   = S_NUMCOR( NP ) * XL * RECIPAP1
c       S_AEROSOL( NP, LCAACC, ACC ) = S_CAACC( NP )   * XL * RECIPAP1
c       S_AEROSOL( NP, LMGACC, ACC ) = S_MGACC( NP )   * XL * RECIPAP1
c       S_AEROSOL( NP, LKACC,  ACC ) = S_KACC( NP )    * XL * RECIPAP1
c       S_AEROSOL( NP, LSOILC, COR ) = S_SOILCOR( NP ) * XL * RECIPAP1
c       S_AEROSOL( NP, LANTHC, COR ) = S_ANTHCOR( NP ) * XL * RECIPAP1
c       S_AEROSOL( NP, LSEASC, COR ) = S_SEASCOR( NP ) * XL * RECIPAP1
c       S_AEROSOL( NP, LFEACC, ACC ) = S_FEACC( NP )   * XL * RECIPAP1
c       S_AEROSOL( NP, LMNACC, ACC ) = S_MNACC( NP )   * XL * RECIPAP1
      END DO
#endif

C...Compute for Aerosol species in  TXHG Version

      AEROSOL( LTRACER_AKN, AKN ) = AEROSOL( LTRACER_AKN, AKN ) * EALFA3T
      AEROSOL( LTRACER_ACC, ACC ) = TRACER    * XL * RECIPAP1
      AEROSOL( LTRACER_COR, COR ) = TRACERCOR * XL * RECIPAP1
      AEROSOL( LPHG_AKN, AKN )    = AEROSOL( LPHG_AKN, AKN ) * EALFA3T
      AEROSOL( LPHG_ACC, ACC )    = HGFINE * XL * RECIPAP1
      AEROSOL( LPHG_COR, COR )    = HGCOR * XL * RECIPAP1
      
      IF( AEROSOL(LTRACER_ACC, ACC) .GT. 0.d0 ) THEN
         FRACTR  = MIN((TRACI  * XL * RECIPAP1) / 
     &             AEROSOL(LTRACER_ACC, ACC), 1.0D0) 
      ELSE 
         FRACTR = 0.d0
      END IF
      
      IF( AEROSOL(LPOA, ACC) .GT. 0.d0 ) THEN
         FRACPOA = MIN((POAI  * XL * RECIPAP1) / 
     &             AEROSOL(LPOA, ACC), 1.0D0)
      ELSE 
         FRACPOA = 0.d0
      END IF 
      
      IF( AEROSOL(LPRI, ACC) .GT. 0.d0 ) THEN
         FRACPRI = MIN((PRII  * XL * RECIPAP1) / 
     &             AEROSOL(LPRI, ACC), 1.0D0)
      ELSE 
         FRACPRI = 0.d0
      END IF 
      
      IF( AEROSOL(LSOA, ACC) .GT. 0.d0 ) THEN
         FRACSOA = MIN((SOAI  * XL * RECIPAP1) / 
     &             AEROSOL(LSOA, ACC), 1.0D0)
      ELSE 
         FRACSOA = 0.d0
      END IF 

C...store the amount of hydrogen deposition

      HPWDEP = REAL( WETDEP( LACL ), 4 )
#ifdef sens
      DO NP =1, NPMAX
        S_HPWDEP( NP ) = S_WETDEP( NP, LACL )
      END DO
#endif

#ifdef isam
C... store sulfate production/loss quantities
      DEPSUM_SAVE =        DEPSUM * XL * RECIPAP1
      DS4_SAVE    = -1.0 * DS4(0) * XL * RECIPAP1
      REMOV_SAVE  = WETDEP( LSO4ACCL ) + WETDEP( LHSO4ACCL )

C... store AORGC production/loss quantities      
      DEPSUM_AORGC_SAVE = WETDEP( LORGCL ) * XC1 * XL * RECIPAP1
      DGLY1_SAVE        = DGLY1_SAVE  * XL * RECIPAP1
      DMGLY1_SAVE       = DMGLY1_SAVE * XL * RECIPAP1
      REMOV_AORGC_SAVE  = WETDEP( LORGCL )
#endif

      RETURN

C...formats

1001  FORMAT ( 1X, 'STORM RATE=', F6.3, 'DSIVDT(0) =', F10.5,
     &       'TS6=', F10.5, 'DTW(0)=', F10.5, 'CTHK1=', F10.5,
     &       'WTAVG=', F10.5 )

C1002  FORMAT( 65(1X, ES12.4, 4X) )
      END
