
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
      SUBROUTINE AQCHEM ( JDATE, JTIME, TEMP2, PRES_PA, TAUCLD, PRCRATE,   &
                          WCAVG, WTAVG, AIRM, ALFA0, ALFA2, ALFA3, GAS,    &
                          AEROSOL, GASWDEP, AERWDEP, HPWDEP, BETASO4, COSZEN, &
                          FRACTR, FRACPOA, FRACPRI, FRACSOA )      
!-----------------------------------------------------------------------
!  Description:
!    Compute concentration changes in cloud due to aqueous chemistry,
!    scavenging and wet deposition amounts.  This is the "KMT" version
!    of AQCHEM, which includes the treatment of kinetic mass transfer 
!    between the gas and aqueous phases and the implementation of the 
!    RODAS3 solver to simultaneously integrate phase transfer, scavenging, 
!    deposition, dissociation, and chemical kinetic processes. 
!    The additional subroutines of "AQCHEM-KMT" used in the integration of 
!    the system of aqueous ODEs were generated using the Kinetic 
!    PreProcessor, version 2.2.3 (Damian et al., 2002).
!
!  Revision History:
!      No   Date   Who  What
!      -- -------- ---  -----------------------------------------
!      0  / /86    CW   BEGIN PROGRAM - Walceks's Original Code
!      1  / /86    RB   INCORPORATE INTO RADM
!      2  03/23/87 DH   REFORMAT
!      3  04/11/88 SJR  STREAMLINED CODE - ADDED COMMENTS
!      4  08/27/88 SJR  COMMENTS, MODIFIED FOR RPM
!      4a 03/15/96 FSB  Scanned hard copy to develop Models3
!                       Version.
!      5  04/24/96 FSB  Made into Models3 Format
!      6  02/18/97 SJR  Revisions to link with Models3
!      7  08/12/97 SJR  Revised for new concentration units (moles/mole)
!                       and new treatment of nitrate and nitric acid
!      8  01/15/98 sjr  revised to add new aitken mode scavenging
!                       and aerosol number scavenging
!      9  12/15/98 David Wong at LM:
!             -- change division of XL, TEMP to multiplication of XL, TEMP
!                reciprocal, respectively
!             -- change / TOTOX / TSIV to / ( TOTOX * TSIV )
!     10  03/18/99 David Wong at LM:
!             -- removed "* 1.0" redundant calculation at TEMP1 calculation
!     11  04/27/00 sjr  Added aerosol surface area as modeled species
!     12  12/02    sjr  changed calls to HLCONST and updated the dissociation
!                       constants
!     13  06/26/03 sjr  revised calculations of DTW based on CMAS website
!                       discussions
!     14  08/05/03 sjr  revision made to the coarse aerosol number washout
!     15  04/20/05  us  revisions to add sea salt species in the fine and
!                       coarse aerosol modes, and HCl dissolution/dissociation
!     16  10/13/05 sjr  fixed bug in the integration time step calculation
!                       (reported by Bonyoung Koo)
!     17  03/01/06 sjr  added elemental carbon aerosol; organic aerosols
!                       replaced with primary, secondary biogenic, and
!                       secondary anthropogenic; fixed 3rd moment calc to
!                       include EC and primary organics (not secondary);
!                       re-arranged logic for setting Cl & Na ending conc;
!                       added pointers/indirect addressing for arrays WETDEP
!                       and LIQUID
!     16  03/30/07 sjr  Limit integration timestep by cloud washout time
!     17  04/10/07 sjr  increased loop limits as follows: I20C <10000,
!                       I7777C <10000, I30C <10000, ICNTAQ <60000
!     18  01/10/07 agc  added organic chemistry for GLY and MGLY oxidation
!     19  09/10/07 sln  updated SOA species list for AE5
!     20  01/29/08 agc  updated DOHDT calculation
!     21  04/14/08 jtk  added coding for coarse NH4 and scavenging of
!                       coarse surface area
!     22  05/20/08 agc  for CB05, use the Henry's Law constant for glyoxal
!                       as a surrogate for methyl glyoxal
!     23  04/15/09 sjr& Several changes made to improve mass conservation in the
!                  agc  solver.  (1) OH concentration is now considered to be
!                       steady state; (2) only allow sulfur oxidation to affect
!                       time step; (3) implemented mass conservation checks -
!                       limit oxidation rates by the available mass for the
!                       specified timestep.
!   10 Oct 10 J.Young:  update to use aero_reeng by Steve Howard, Prakash Bhave,
!                       Jeff Young, Sergey Napelenok, and Shawn Roselle
!   01 Mar 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
!    9 Mar 11 S.Napelenok: update for AE6 - pH calculation now expanded to 
!                       include Ca Mg K SOIL CORS SEAS
!   23 May 11 G.Sarwar: update S(VI) production rate via H2O2, O3, MHP, PAA 
!                       pathways (Jacobson 1997)
!   23 May 11 G.Sarwar: update S(VI) production rate via O2 pathway (metal 
!                       catalysis) (Martin and Goodman, 1991)
!   01 Jul 11 G.Sarwar: Incorporate day and night dependent Fe III oxidation
!                       state (Alexander et al.,  2009)
!   12 Aug 11 G.Sarwar: Revise Fe and Mn solubility based on 
!                       Alexander et al., 2009
!    8 Mar 12 J.Bash:   FE_OX and MN_OX were calculated from FE and MN before
!                       a floor value of 0.0 was established for these 
!                       concentrations sometimes resulting in negative 
!                       concentrations and model crashes. The code used to 
!                       estimate FE_OX and MN_OX was moved to be after a floor 
!                       value for FE and MN was set. Also the washout rate was
!                       removed from the calculation of the estimate for doubling
!                       the time step based on sulfur oxidized < 5%.
!   28 Nov 12 G.Sarwar: Sulfate inhibition effect is implemented in the metal catalysis pathway
!   04 Mar 14 K. Fahey: Used the Kinetic PreProcessor to generate the RODAS3 solver
!                       for the CMAQ aqueous phase chemistry mechanism (Damian et al., 2002). 
!                       Aitken scavenging, mass transfer between the phases, dissociation, 
!                       chemical kinetics, and wet deposition are solved dynamically 
!                       and simultaneously.  The mass transfer between the phases is
!                       based on the resistance model of Schwartz (Schwartz, 1986). The SIV-O3
!                       oxidation reaction has been corrected for potential aqueous 
!                       diffusion limitations.
!  07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
!  15 Jul 14 K. Fahey:  Added IEPOX/MAE SOA chemistry based on Pye et al., 2013
!  31 Mar 16 K. Fahey:  Replaced yield-based SOA parameterization from GLY/MGLY + OH with the SOA 
!                       chemistry scheme provided by Neha Sareen and Annmarie Carlton (based on 
!                       Lim et al., 2005)
!  03 May 16 K.Fahey:   Reinstated usage of HLCONST function to calculate Henry's law
!                       coefficients (rather than calculating in mass transfer coefficient 
!                       function) [changes in aqchem_kmt, aqchem_Initialize, aqchem_Global]; 
!                       updated to use AERO_DATA constants in calculation of coarse cations 
!                       following J. Young updates in AQCHEM.F [changes in aqchem_kmt, 
!                       aqchem_Initialize]
!  26 May 16 K.Fahey:   Added Hg/toxic tracers to be consistent with AQCHEM.F updates
!  31 Aug 17 K.Fahey:   Incorporated additional reactions for S, N, O-H, and C species 
!                       (Leriche et al., 2013; Lim et al., 2005; Ervens et al., 2003) 
!
!  References:
!     Walcek & Taylor, 1986, A theoretical Method for computing
!        vertical distributions of acidity and sulfate within cumulus
!        clouds, J. Atmos Sci.,  Vol. 43, no. 4 pp 339 - 355
!     Carlton, A.G., B.J. Turpin, K.E. Altieri, S.P. Seitzinger, R. Mathur,
!        S.J. Roselle, and R.J. Weber, CMAQ Model Performance Enhanced When
!        In-Cloud Secondary Organic Aerosol is Included:  Comparison of Organic
!        Carbon Predictions with Measurements, Environ. Sci. Technol., 42(23),
!        8798-8802, 2008.
!     Jacobson, M., Development and application of a new air pollution modeling 
!        system II. Aerosol module structure and design, Atmospheric 
!        Environment, 31, 131-144, 1997
!     Martin, R.L. and T.W. Good, catalyzed oxidation of sulfur dioxide in 
!        solution: the iron-manganese synergism, Atmospheric Environment, 25A, 
!        2395-2399, 1991
!     Alexander, B., R.J. Park, D.J. Jacob, S. Gong, Transition metal-catalyzed  
!        oxidation of atmospheric sulfur: global implications for the sulfur
!        budget, GRL, 114, D02309, 2009
!     Damian, V., A. Sandu, M. Damian, F. Potra, and G.R. Carmichael, The Kinetic 
!        PreProcessor KPP -- A Software Environment for Solving Chemical Kinetics,
!        Computers and Chemical Engineering, 26(11), 1567-1579, 2002.
!     Schwartz, S.E., Mass transport considerations pertinent to aqueous-phase
!        reactions of gases in liquid water clouds. In Chemistry of multiphase
!        atmospheric systems, NATO ASI Series, G6, 415-471, 1986. 
!     Leriche, M., J.-P. Pinty, C. Mari, and D. Gazen, A cloud chemistry module for
!        the 3-D cloud-resolving mesoscale model Meso-NH with application to idealized
!        cases, Geosci. Model Dev., 6, 1275-1298, 2013
!     Lim, H.-J., A.G. Carlton, and B.J. Turpin, Isoprene forms secondary organic
!        aerosol through cloud processing: model simulations, Environ. Sci. Technol.,
!        39, 4441-4446, 2005
!     Ervens,B., C. George, J.E. Williams, G.V. Buxton, G.A. Salmon, M. Bydder, 
!        F. Wilkinson, F. Dentener, P. Mirabel, and H. Herrmann, CAPRAM 2.4 (MODAC 
!        mechanism): An extended and condensed tropospheric aqueous phase mechanism 
!        and its application, J. Geophys. Res., 108 (D14), 4426, 2003  
!
!  Called by:  AQMAP
!
!  Calls the following subroutines:  Initialize, Update_RCONST, INTEGRATE
!
!  Calls the following functions: none
!-----------------------------------------------------------------------

      USE RXNS_DATA           ! chemical mechanism data
      USE AQ_DATA
      USE AERO_DATA
      USE UTILIO_DEFN
   
      USE aqchem_Model
      USE aqchem_Initialize, ONLY: Initialize  

      IMPLICIT NONE

      CHARACTER( 120 ) :: XMSG = ' '  ! Exit status message

!..........Parameters:

!...........Arguments:

      INTEGER,   INTENT( IN )  :: JDATE                     ! current model date, coded YYYYDDD
      INTEGER,   INTENT( IN )  :: JTIME                     ! current model time, coded HHMMSS

      REAL,      INTENT( IN )  :: AIRM                      ! total air mass in cloudy layers (mol/m2)
      REAL,      INTENT( IN )  :: ALFA0                     ! scav coef for aitken aerosol number
      REAL,      INTENT( IN )  :: ALFA2                     ! scav coef for aitken aerosol sfc area
      REAL,      INTENT( IN )  :: ALFA3                     ! scav coef for aitken aerosol mass
      REAL,      INTENT( OUT ) :: HPWDEP                    ! hydrogen wet deposition (mm mol/liter)
      REAL( 8 ), INTENT( OUT ) :: BETASO4  
      REAL,      INTENT( IN )  :: PRCRATE                   ! precip rate (mm/hr)
      REAL,      INTENT( IN )  :: PRES_PA                   ! pressure (Pa)
      REAL,      INTENT( IN )  :: TAUCLD                    ! timestep for cloud (s)
      REAL,      INTENT( IN )  :: TEMP2                     ! temperature (K)
      REAL,      INTENT( IN )  :: WCAVG                     ! liquid water content (kg/m3)
      REAL,      INTENT( IN )  :: WTAVG                     ! total water content (kg/m3)
      
      REAL,      INTENT( IN )  :: COSZEN                    ! Cosine solar zenith angle      
      
!      LOGICAL,   INTENT( IN )  :: DARK                      ! DARK = TRUE is night,  DARK = FALSE is day

      REAL( 8 ), INTENT( INOUT ) :: GAS    ( : )            ! gas phase concentrations (mol/molV)
      REAL( 8 ), INTENT( INOUT ) :: AEROSOL( :, : )         ! aerosol concentrations (mol/molV)
      REAL( 8 ), INTENT( INOUT ) :: GASWDEP( : )            ! gas phase wet deposition array (mm mol/liter)
      REAL( 8 ), INTENT( INOUT ) :: AERWDEP( :, : )         ! aerosol wet deposition array (mm mol/liter)
      
      REAL( 8 ), INTENT( OUT ) :: FRACTR    ! Fraction of J mode tracer scavenged from I mode 
      REAL( 8 ), INTENT( OUT ) :: FRACPOA   ! Fraction of J mode poa scavenged from I mode 
      REAL( 8 ), INTENT( OUT ) :: FRACPRI   ! Fraction of J mode pri scavenged from I mode
      REAL( 8 ), INTENT( OUT ) :: FRACSOA   ! Fraction of J mode soa scavenged from I mode       
      
      REAL( 8 ), SAVE :: SOIL_FE_FAC                        ! Fe molar fraction of ASOIL
      REAL( 8 ), SAVE :: CORS_FE_FAC                        ! Fe molar fraction of ACORS
      REAL( 8 ), SAVE :: SOIL_MN_FAC                        ! Mn molar fraction of ASOIL
      REAL( 8 ), SAVE :: CORS_MN_FAC                        ! Fe molar fraction of ACORS
      REAL( 8 ), SAVE :: SEAS_NA_FAC                        ! Na molar fraction of ASEACAT
      REAL( 8 ), SAVE :: SOIL_NA_FAC                        ! Fe molar fraction of ASOIL
      REAL( 8 ), SAVE :: CORS_NA_FAC                        ! Fe molar fraction of ACORS
      REAL( 8 ), SAVE :: SEAS_MG_FAC                        ! Na molar fraction of ASEACAT
      REAL( 8 ), SAVE :: SOIL_MG_FAC                        ! Fe molar fraction of ASOIL
      REAL( 8 ), SAVE :: CORS_MG_FAC                        ! Fe molar fraction of ACORS
      REAL( 8 ), SAVE :: SEAS_CA_FAC                        ! Na molar fraction of ASEACAT
      REAL( 8 ), SAVE :: SOIL_CA_FAC                        ! Fe molar fraction of ASOIL
      REAL( 8 ), SAVE :: CORS_CA_FAC                        ! Fe molar fraction of ACORS
      REAL( 8 ), SAVE :: SEAS_K_FAC                         ! Na molar fraction of ASEACAT
      REAL( 8 ), SAVE :: SOIL_K_FAC                         ! Fe molar fraction of ASOIL
      REAL( 8 ), SAVE :: CORS_K_FAC                         ! Fe molar fraction of ACORS           

!...........Local Variables (scalars):

      LOGICAL, SAVE :: FIRSTIME = .TRUE. ! flag for first pass thru
      LOGICAL, SAVE :: AEI = .TRUE.      ! flag for AE6I and AE7I mechanisms
      LOGICAL, SAVE :: STIC = .FALSE.    ! flag for SAPRC07TIC mechanisms
      
      LOGICAL       :: DARK                      ! DARK = TRUE is night,  DARK = FALSE is day

      CHARACTER( 16 ), SAVE :: PNAME = 'AQCHEM'             ! Driver program name
      CHARACTER( 16 ), SAVE :: MGLYSUR = 'METHYL_GLYOXAL  ' ! Henry's law surrogate for MGLY
      
      REAL( 8 ) :: CTHK1                                    ! Cloud thickness (m)
      REAL( 8 ) :: ONE_OVER_TEMP                            ! 1 / Temperature (1/K)
      REAL( 8 ) :: WFACTOR                                  ! Convert dyn wet dep values to mm mol / L 
      REAL( 8 ) :: INVCFAC                                  ! Conversion: molec/cm3 air --> mol/mol
      REAL( 8 ) :: DEPSUM                                   ! SO4 deposition, used to calculate SO4 scav coefficient
      REAL( 8 ) :: EXPWET                                   ! EXP( -WETFAC_KPP * TAUCLD )
      
      REAL( 8 ) :: TOTNIT, TOTAMM, TOTCL                    ! Total nitrate, ammonium, and chloride (excluding coarse mode)
      REAL( 8 ) :: FNH3, FNH4ACC                            ! Initial fraction NH3 gas of TOTAMM, fraction acc mode of aerosol NH4
      REAL( 8 ) :: FHNO3, FNO3ACC                           ! Initial fraction HNO3 gas of TOTNIT, fraction acc mode of aerosol NO3
      REAL( 8 ) :: FHCL, FCLACC                             ! Initial fraction HCl gas of TOTCL, fraction acc mode of aerosol CL
      
      REAL( 8 ) :: NACOR, CACOR, MGCOR, KCOR, FECOR, MNCOR                ! Coarse crustal cation concentrations
      REAL( 8 ) :: WDNACOR, WDCACOR, WDMGCOR, WDKCOR, WDFECOR, WDMNCOR    ! Coarse crustal cation wet deposition
      REAL( 8 ) :: WDPYRAC, APYRAC                          ! Pyruvic acid deposited and evaporated as aerosol species (gas mech dependent)                        
      
      REAL( 8 ) :: STARTM(4), ENDM(4), MBAL(4)
      
      REAL( 8 ) :: OLIGGLY, OLIGMGLY                        ! If considering oligomerization, fraction of GLY/MGLY that remains in aerosol 
                                                            ! upon droplet evaporation
							    
      REAL( 8 ) :: POAIinit, PRIIinit, TRACIinit, POAJinit, SOAIinit 

      REAL(kind=dp) :: T, DVAL(NSPEC)                       ! KPP integrator variables
      REAL(kind=dp) :: RSTATE(20)                           ! KPP integrator variables

      INTEGER :: I, IGAS, IAER, IMOD, count, J, OLIG

!...........External Functions:

      REAL, EXTERNAL :: HLCONST

!*********************************************************************

!...Initialization

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.

!...Is an AE6I or AE7I version of the mechanism is being used?  
!...This will include IETET, IMGA, etc. (rather than ISO3) and the IEPOX, 
!...IMAE, etc., precursors (rather than just EPOX)

        IF ( ( INDEX ( MECHNAME, 'AE6I' ) .LE. 0 ) .AND.   &
             ( INDEX ( MECHNAME, 'AE7I' ) .LE. 0 ) ) THEN
           AEI = .FALSE.
        END IF

!...Is a SAPRC07TIC or CB6 mechanism is being used?
 
         IF ( INDEX ( MECHNAME, 'CB6' ) .LE. 0  .AND. &
            ( INDEX ( MECHNAME, 'SAPRC07TIC' ) .LE. 0 ) ) THEN
            XMSG = 'This version of AQCHEM requires SAPRC07TIC or a CB6 gas mech'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF 
 
         IF ( INDEX ( MECHNAME, 'SAPRC07TIC' ) .GT. 0 ) THEN
            STIC = .TRUE.
         END IF 

!...Make sure STM option is not set

         IF ( STM ) THEN
            XMSG = 'STM option not implemented in KMT AQCHEM'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

#ifdef isam
        XMSG = 'Source Apportionment is not implemented in KMT AQCHEM'
        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
#endif        

!... set MW ratios and speciation factors for molar concentrations of coarse
!... soluble aerosols

        SOIL_FE_FAC = ASOIL_FE_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( AFE_IDX ), 8 ) / ASOIL_RENORM
        CORS_FE_FAC = ACORS_FE_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( AFE_IDX ), 8 ) / ACORSEM_RENORM

        SOIL_MN_FAC = ASOIL_MN_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( AMN_IDX ), 8 ) / ASOIL_RENORM
        CORS_MN_FAC = ACORS_MN_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( AMN_IDX ), 8 ) / ACORSEM_RENORM

        SEAS_NA_FAC = ASCAT_NA_FAC * REAL( AEROSPC_MW( ASEACAT_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( ANA_IDX ), 8 )
        SOIL_NA_FAC = ASOIL_NA_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( ANA_IDX ), 8 ) / ASOIL_RENORM
        CORS_NA_FAC = ACORS_NA_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( ANA_IDX ), 8 ) / ACORSEM_RENORM

        SEAS_MG_FAC = ASCAT_MG_FAC * REAL( AEROSPC_MW( ASEACAT_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( AMG_IDX ), 8 )
        SOIL_MG_FAC = ASOIL_MG_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( AMG_IDX ), 8 ) / ASOIL_RENORM
        CORS_MG_FAC = ACORS_MG_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( AMG_IDX ), 8 ) / ACORSEM_RENORM

        SEAS_CA_FAC = ASCAT_CA_FAC * REAL( AEROSPC_MW( ASEACAT_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( ACA_IDX ), 8 )
        SOIL_CA_FAC = ASOIL_CA_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( ACA_IDX ), 8 ) / ASOIL_RENORM
        CORS_CA_FAC = ACORS_CA_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( ACA_IDX ), 8 ) / ACORSEM_RENORM

        SEAS_K_FAC = ASCAT_K_FAC * REAL( AEROSPC_MW( ASEACAT_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( AK_IDX ), 8 )
        SOIL_K_FAC = ASOIL_K_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( AK_IDX ), 8 ) / ASOIL_RENORM
        CORS_K_FAC = ACORS_K_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )  &
                                  / REAL( AEROSPC_MW( AK_IDX ), 8 ) / ACORSEM_RENORM 
  
      END IF    ! FIRSTIME
      
!...Set Henry's Law coefficients and other options

      SO2H   = HLCONST( 'SO2             ', TEMP2, .FALSE., 0.0 )
      CO2H   = HLCONST( 'CO2             ', TEMP2, .FALSE., 0.0 )
      NH3H   = HLCONST( 'NH3             ', TEMP2, .FALSE., 0.0 )
      H2O2H  = HLCONST( 'H2O2            ', TEMP2, .FALSE., 0.0 )
      O3H    = HLCONST( 'O3              ', TEMP2, .FALSE., 0.0 )
      HCLH   = HLCONST( 'HCL             ', TEMP2, .FALSE., 0.0 )
      HNO3H  = HLCONST( 'HNO3            ', TEMP2, .FALSE., 0.0 )
      MHPH   = HLCONST( 'METHYLHYDROPEROX', TEMP2, .FALSE., 0.0 )
      PAAH   = HLCONST( 'PEROXYACETIC_ACI', TEMP2, .FALSE., 0.0 )
      FOAH   = HLCONST( 'FORMIC_ACID     ', TEMP2, .FALSE., 0.0 )
      GLYH   = HLCONST( 'GLYOXAL         ', TEMP2, .FALSE., 0.0 )
      MGLYH  = HLCONST( MGLYSUR,            TEMP2, .FALSE., 0.0 )
      HOH    = HLCONST( 'OH              ', TEMP2, .FALSE., 0.0 ) 
      GCOLH  = 4.1D+04 * EXP( 4.6D+03 * ( ( 298.D0 - TEMP2 ) / ( 298.D0 * TEMP2 ) ) )  ! Sander (2015)
      CCOOHH = HLCONST( 'ACETIC_ACID     ', TEMP2, .FALSE., 0.0 )
      HCHOH  = 2.5D0   !HLCONST( 'FORMALDEHYDE    ', TEMP2, .FALSE., 0.0 )             ! Seinfeld and Pandis (2016)
      HO2H   = HLCONST( 'HO2             ', TEMP2, .FALSE., 0.0 ) 
      NO2H  = HLCONST( 'NO2             ', TEMP2, .FALSE., 0.0 )
      HONOH = HLCONST( 'HNO2            ', TEMP2, .FALSE., 0.0 )  
      HNO4H = HLCONST( 'HNO4            ', TEMP2, .FALSE., 0.0 ) 
      HIEPOX  = HLCONST( 'IEPOX           ', TEMP2, .FALSE., 0.0 )
      HMAE    = HLCONST( 'IMAE            ', TEMP2, .FALSE., 0.0 )
      HHMML   = HLCONST( 'IMAE            ', TEMP2, .FALSE., 0.0 )   
      NO3H    = HLCONST( 'NO3             ', TEMP2, .FALSE., 0.0 )
      CH3O2H  = 2.7D0 * EXP( 2.03D+03 * ( ( 298.D0 - TEMP2 ) / ( 298.D0 * TEMP2 ) ) )  ! Leriche et al., 2013 
      PYRACH  = HLCONST( 'PYRUVIC_ACID    ', TEMP2, .FALSE., 0.0 )     

      ONE_OVER_TEMP = 1.0D0 / TEMP2
      
      JH2O2 = jh2o2_hydrometeors    ! H2O2 photolysis rate calculated for gas phase chemistry

!...AE6I and AE7I includes AIETETJ, AIEOSJ, ADIMJ, AIMGAJ, and AIMOSJ species. AE6/7 uses AISO3J to represent IEPOX SOA.
     
      ISPC8 = 0
      IF(AEI) ISPC8 = 1 

!...SAPRC07TIC includes pyruvic acid in the gas phase mech.  For that mech, allow pyruvic acid to transfer between phases
     
      MTPYRAC = 0
      IF(STIC) MTPYRAC = 1
      
!...Flag to keep a fraction of aqueous glyoxal and methylglyoxal in aerosol phase upon droplet evaporation.       
       
      OLIG = 0 !OLIGOMERIZATION OF GLY/MGLY UPON DROPLET EVAPORATION 
               !1= ON, 0 = OFF -- Default = 0
               !Could consider if SOA species were explicitly tracked

      OLIGGLY =  OLIG * 3.3D-1  !(De Haan et al., 2009; Liu et al., 2012)
      OLIGMGLY = OLIG * 1.9D-1
      
!...Flag to consider a simply estimated photolysis rate for those rates not previously calculated for gas phase chemistry

      PHOTO = 0 ! =1 to estimate photolysis rate(s) with a single value modulated by COSZEN
                ! =0 to ignore photolysis rates not calculated externally in photolysis module (default)

!...Check for bad temperature, cloud air mass, or pressure

      IF ( TEMP2 .LE. 0.0D0 .OR. AIRM .LE. 0.0D0 .OR. PRES_PA .LE. 0.0D0 ) THEN
         XMSG = 'MET DATA ERROR'
         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      END IF
      
      JDATEKPP = JDATE
      JTIMEKPP = JTIME

!...Compute fractional weights for several species     

      TOTNIT = GAS( LHNO3 ) + AEROSOL( LNO3, ACC )
      IF ( TOTNIT .GT. 0.0D0 ) THEN
         FHNO3   = GAS( LHNO3 ) / TOTNIT
      ELSE
         FHNO3   = 1.0D0
      END IF
      
      IF ( AEROSOL( LNO3, ACC ) + AEROSOL( LNO3, COR ) .GT. 0.0D0 ) THEN
         FNO3ACC = AEROSOL( LNO3, ACC ) / (AEROSOL( LNO3, ACC ) + AEROSOL( LNO3, COR ))  !just aerosol
      ELSE
         FNO3ACC = 1.d0
      END IF

      TOTAMM = GAS( LNH3 ) + AEROSOL( LNH4, ACC )
      IF ( TOTAMM .GT. 0.0D0 ) THEN
         FNH3    = GAS( LNH3 ) / TOTAMM
      ELSE
         FNH3    = 1.0D0
      END IF
      
      IF ( AEROSOL( LNH4, ACC ) + AEROSOL( LNH4, COR ) .GT. 0.0D0 ) THEN      
         FNH4ACC = AEROSOL( LNH4, ACC ) / (AEROSOL( LNH4, ACC ) + AEROSOL( LNH4, COR ))  !just aerosol
      ELSE
         FNH4ACC = 1.d0
      END IF
      
      TOTCL = GAS( LHCL ) + AEROSOL( LCL, ACC )
      IF ( TOTCL .GT. 0.0D0 ) THEN
         FHCL    = GAS( LHCL ) / TOTCL
      ELSE
         FHCL    = 1.0D0
      END IF
      
      IF ( AEROSOL( LCL, ACC ) + AEROSOL( LCL, COR ) .GT. 0.0D0 ) THEN            
         FCLACC = AEROSOL( LCL, ACC ) / (AEROSOL( LCL, ACC ) + AEROSOL( LCL, COR ))  !just aerosol
      ELSE
         FCLACC = 1.d0
      END IF
      
      IF ( COSZEN .LE. 0.0 ) THEN
         DARK = .TRUE.   ! night
      ELSE
         DARK = .FALSE.  ! day
      END IF
      
!...Mass balance check - start
        STARTM = 0.d0
        ENDM   = 0.d0
        MBAL   = 0.d0  
          
        STARTM(1) = (GAS(LSO2) + GAS(LH2SO4)) * 32.06
        STARTM(2) = (GAS(LHNO3) + 2*GAS(LN2O5) + GAS(LNO2) + GAS(LHONO) + &
                    GAS(LHNO4) + GAS(LNO3RAD))*14.007  !
        STARTM(3) = GAS(LNH3)*14.007     
        STARTM(4) = GAS(LHCL)*35.5 
                  
        DO I =  1,NMODES
           STARTM(1) = STARTM(1) + AEROSOL(LSO4, I)*32.06
           STARTM(2) = STARTM(2) + AEROSOL(LNO3, I)*14.007
           STARTM(3) = STARTM(3) + AEROSOL(LNH4, I)*14.007
           STARTM(4) = STARTM(4) + AEROSOL(LCL, I)*35.5
        ENDDO   
	
	POAIinit = AEROSOL(LPOA, AKN)	
	PRIIinit = AEROSOL(LPRI, AKN)	
	TRACIinit = AEROSOL(LTRACER_AKN, AKN)
	SOAIinit = AEROSOL(LSOA, AKN)
	
	POAJinit = AEROSOL(LPOA, ACC)

     
!...Initialize dynamic species, rel/abs tolerances, and other specifications before calling integrator

      CTHK1 = AIRM * TEMP2 * 0.08206D0 / ( PRES_PA / 101325.D0 * 1000.0D0 ) ! cloud thickness (m)
      
      CALL Initialize( TEMP2, PRES_PA, TAUCLD, PRCRATE,       &
                       WCAVG, WTAVG, AIRM, ALFA0, ALFA3,      &
                       GAS, AEROSOL, CTHK1, DARK, COSZEN,     &
                       SOIL_FE_FAC, CORS_FE_FAC, SOIL_MN_FAC, &
                       CORS_MN_FAC, SEAS_NA_FAC, SOIL_NA_FAC, &
                       CORS_NA_FAC, SEAS_MG_FAC, SOIL_MG_FAC, &
                       CORS_MG_FAC, SEAS_CA_FAC, SOIL_CA_FAC, &
                       CORS_CA_FAC, SEAS_K_FAC, SOIL_K_FAC, &
                       CORS_K_FAC )
                   
      INVCFAC = 1.d0 / CFACTOR
      
      STEPMIN = 0.0d0
      
!...Integrate equations describing mass transfer, scavenging, dissociation, kinetics, and deposition

      T = TSTART
kron: DO WHILE (T < TEND)

         TIME = T

         CALL Update_RCONST()

         CALL INTEGRATE( TIN = T, TOUT = T+DT, RSTATUS_U = RSTATE, &
            ICNTRL_U = (/ 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 /)) !, &
!           RCNTRL_U= (/ 0.d0,0.d0,1.d-1,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0, &
!           0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0 /))  ! rodas3

         T = RSTATE(1)

      END DO kron

      TIME = T

!...Convert units and redistribute dynamic species to gas, aerosol, and deposition arrays and return

      GASWDEP = 0.d0
      AERWDEP = 0.d0
     
      WFACTOR = WCAVG * CTHK1 * PHI2 !Conversion factor for wet deposition species
      EXPWET = EXP( -WETFAC_KPP * TAUCLD )
      
!...AEROSOL species, Aitken Mode

      AEROSOL( LNO3, AKN ) = VAR( ind_A_NO3AKN ) * INVCFAC
      AEROSOL( LNH4, AKN ) = VAR( ind_A_NH4AKN ) * INVCFAC
      AEROSOL( LCL, AKN )  = VAR( ind_A_CLAKN ) * INVCFAC
      AEROSOL( LNA, AKN )  = VAR( ind_A_NAAKN ) * INVCFAC
      AEROSOL( LSO4, AKN ) = VAR( ind_A_SO4AKN ) * INVCFAC
      AEROSOL( LEC, AKN )  = VAR( ind_A_PECAKN ) * INVCFAC
      AEROSOL( LPOA, AKN ) = VAR( ind_A_POAAKN ) * INVCFAC
      AEROSOL( LPRI, AKN ) = VAR( ind_A_PRIAKN ) * INVCFAC
      AEROSOL( LNUM, AKN ) = AEROSOL( LNUM, AKN ) * EXP(-ALFA0 * TAUCLD) 
      
!...Simple treatment for Hg/toxic tracer species    
      
      AEROSOL( LTRACER_ACC, ACC ) = AEROSOL( LTRACER_ACC, ACC ) + &
                                    AEROSOL( LTRACER_AKN, AKN ) * (1.d0-EXP(-ALFA3 * TAUCLD))  
      AEROSOL( LPHG_ACC, ACC )    = AEROSOL( LPHG_ACC, ACC ) + &
                                    AEROSOL( LPHG_AKN, AKN ) * (1.d0-EXP(-ALFA3 * TAUCLD))
				    
      AEROSOL( LTRACER_AKN, AKN ) = AEROSOL( LTRACER_AKN, AKN ) * EXP(-ALFA3 * TAUCLD)  
      AEROSOL( LPHG_AKN, AKN )    = AEROSOL( LPHG_AKN, AKN ) * EXP(-ALFA3 * TAUCLD)				       
      AERWDEP( LTRACER_ACC, ACC ) = AEROSOL( LTRACER_ACC,ACC ) * ( 1.d0 - EXPWET ) * CFACTOR 
      AERWDEP( LPHG_ACC, ACC )    = AEROSOL( LPHG_ACC,ACC ) * ( 1.d0 - EXPWET ) * CFACTOR  
      AERWDEP( LTRACER_COR, COR ) = AEROSOL( LTRACER_COR,COR ) * ( 1.d0 - EXPWET ) * CFACTOR 
      AERWDEP( LPHG_COR, COR )    = AEROSOL( LPHG_COR,COR ) * ( 1.d0 - EXPWET ) * CFACTOR  
         
      AEROSOL( LTRACER_ACC, ACC ) = AEROSOL( LTRACER_ACC, ACC ) * EXPWET
      AEROSOL( LPHG_ACC, ACC )    = AEROSOL( LPHG_ACC, ACC ) * EXPWET
      
      AEROSOL( LTRACER_COR, COR ) = AEROSOL( LTRACER_COR, COR ) * EXPWET
      AEROSOL( LPHG_COR, COR )    = AEROSOL( LPHG_COR, COR ) * EXPWET 
      
      
!...SOA (it now can have an AKN mode)
            
      AEROSOL( LSOA, ACC ) = AEROSOL( LSOA, ACC ) + &
                                    AEROSOL( LSOA, AKN ) * (1.d0-EXP(-ALFA3 * TAUCLD))
      AEROSOL( LSOA, AKN ) = AEROSOL( LSOA, AKN ) * EXP(-ALFA3 * TAUCLD)     
      AERWDEP( LSOA, ACC ) = AEROSOL( LSOA, ACC ) * ( 1.d0 - EXPWET ) * CFACTOR 
      AEROSOL( LSOA, ACC ) = AEROSOL( LSOA, ACC ) * EXPWET      
   
        
! As in standard "AQCHEM", the assumption is made here that final coarse mode
! concentrations are updated due to wet deposition alone (i.e., no mass
! change due to chemistry or phase transfer)           
     
!...AERWDEP species, coarse mode

      AERWDEP( LSOILC, COR ) = AEROSOL( LSOILC,COR ) * ( 1.d0 - EXPWET ) * CFACTOR      
      AERWDEP( LSEASC, COR ) = AEROSOL( LSEASC,COR ) * ( 1.d0 - EXPWET ) * CFACTOR     
      AERWDEP( LANTHC, COR ) = AEROSOL( LANTHC,COR ) * ( 1.d0 - EXPWET ) * CFACTOR      
      AERWDEP( LSO4, COR )   = AEROSOL( LSO4,COR ) * ( 1.d0 - EXPWET ) * CFACTOR
      AERWDEP( LNH4, COR )   = AEROSOL( LNH4,COR ) * ( 1.d0 - EXPWET ) * CFACTOR
      AERWDEP( LNO3, COR )   = AEROSOL( LNO3,COR ) * ( 1.d0 - EXPWET ) * CFACTOR
      AERWDEP( LCL, COR )    = AEROSOL( LCL,COR ) * ( 1.d0 - EXPWET ) * CFACTOR

!...AEROSOL species, coarse mode 

      AEROSOL( LNUM, COR )   = AEROSOL( LNUM, COR ) * EXPWET 
      AEROSOL( LSOILC, COR ) = AEROSOL( LSOILC, COR ) * EXPWET
      AEROSOL( LSEASC, COR ) = AEROSOL( LSEASC, COR ) * EXPWET
      AEROSOL( LANTHC, COR ) = AEROSOL( LANTHC, COR ) * EXPWET
      AEROSOL( LSO4, COR )   = AEROSOL( LSO4, COR ) * EXPWET
      AEROSOL( LNH4, COR )   = AEROSOL( LNH4, COR ) * EXPWET
      AEROSOL( LNO3, COR )   = AEROSOL( LNO3, COR ) * EXPWET
      AEROSOL( LCL, COR )    = AEROSOL( LCL, COR ) * EXPWET   

!...AERWDEP species, accumulation mode 

!      AERWDEP( LSOA, ACC ) = AEROSOL( LSOA, ACC ) * ( 1 - EXPWET) * CFACTOR ! SOA is only impacted by wet dep process 
!                                                                            ! and not included in the list of dynamic 
!                                                                            ! species, VAR      
      
      WDFECOR   = SOIL_FE_FAC * AERWDEP( LSOILC, COR ) + CORS_FE_FAC * AERWDEP( LANTHC, COR )
      WDMNCOR   = SOIL_MN_FAC * AERWDEP( LSOILC, COR ) + CORS_MN_FAC * AERWDEP( LANTHC, COR )     
      WDNACOR   = SEAS_NA_FAC * AERWDEP( LSEASC, COR ) + SOIL_NA_FAC * AERWDEP( LSOILC, COR )  &
                + CORS_NA_FAC * AERWDEP( LANTHC, COR )
      WDMGCOR   = SEAS_MG_FAC * AERWDEP( LSEASC, COR ) + SOIL_MG_FAC * AERWDEP( LSOILC, COR )  &
                + CORS_MG_FAC * AERWDEP( LANTHC, COR )
      WDCACOR   = SEAS_CA_FAC * AERWDEP( LSEASC, COR ) + SOIL_CA_FAC * AERWDEP( LSOILC, COR )  &
                + CORS_CA_FAC * AERWDEP( LANTHC, COR )
      WDKCOR    = SEAS_K_FAC  * AERWDEP( LSEASC, COR ) + SOIL_K_FAC  * AERWDEP( LSOILC, COR )  &
                + CORS_K_FAC  * AERWDEP( LANTHC, COR )

!     For aerosol species with both accumulation mode and coarse mode components, the accumulation
!     mode wet deposition amount is determined by subtracting the analytically determined
!     coarse mode deposition amount from the total (accumulation+coarse mode) species wet 
!     deposition amount       
      
      AERWDEP( LFEACC, ACC ) = MAX( ( VAR( ind_WD_FEPLUS3 ) / FE_III / FE_SOL) - WDFECOR, 0.0d0 )
      AERWDEP( LMNACC, ACC ) = MAX( ( VAR( ind_WD_MNPLUS2 ) / MN_II / MN_SOL) - WDMNCOR, 0.0d0 )                  
      AERWDEP( LNA,  ACC )   = MAX( VAR( ind_WD_NAPLUS ) - WDNACOR, 0.0d0 )
      AERWDEP( LCAACC, ACC ) = MAX( VAR( ind_WD_CAPLUS2 ) - WDCACOR, 0.0d0 )
      AERWDEP( LMGACC, ACC ) = MAX( VAR( ind_WD_MGPLUS2 ) - WDMGCOR, 0.0d0 )
      AERWDEP( LKACC, ACC )  = MAX( VAR( ind_WD_KPLUS ) - WDKCOR, 0.0d0 )
      
      AERWDEP( LSO4, ACC )   = MAX( VAR( ind_WD_H2SO4 ) - AERWDEP( LSO4, COR ), 0.0d0 )
      AERWDEP( LNH4, ACC )   = MAX( VAR( ind_WD_NH4PLUS ) - AERWDEP( LNH4, COR ), 0.0d0 )
      AERWDEP( LNO3, ACC )   = MAX( VAR( ind_WD_NO3MIN ) - AERWDEP( LNO3, COR ), 0.0d0 )
      AERWDEP( LCL, ACC )    = MAX( VAR( ind_WD_CLMIN ) - AERWDEP( LCL, COR ), 0.0d0 )
      
      AERWDEP( LPRI, ACC )   = VAR( ind_WD_PRIACC )
      AERWDEP( LEC, ACC )    = VAR( ind_WD_PECACC )
      AERWDEP( LORGC, ACC )  = VAR( ind_WD_ORGC )
      AERWDEP( LPOA, ACC )   = VAR( ind_WD_POAACC ) 
      
      IF(ISPC8 .gt. 0) THEN        
         AERWDEP( LIETET, ACC ) = VAR( ind_WD_IETET )
         AERWDEP( LIEOS, ACC )  = VAR( ind_WD_IEOS )
         AERWDEP( LDIMER, ACC ) = VAR( ind_WD_DIMER )
         AERWDEP( LIMGA, ACC )  = VAR( ind_WD_IMGA )
         AERWDEP( LIMOS, ACC )  = VAR( ind_WD_IMOS )
      ELSE   
         AERWDEP( LISO3, ACC )  = VAR( ind_WD_IETET )
      END IF
           
!     For volatile species represented in the coarse mode -- make sure you are 
!     not depositing more mass from the coarse mode than was calculated for the total

      IF( AERWDEP( LNH4, COR ) .GT. VAR( ind_WD_NH4PLUS ) ) THEN
          AERWDEP( LNH4, COR ) = ( 1.0d0 - FNH4ACC ) * VAR( ind_WD_NH4PLUS )
          AERWDEP( LNH4, ACC ) = FNH4ACC * VAR( ind_WD_NH4PLUS )
          AEROSOL( LNH4, COR ) = ( 1.0d0 - FNH4ACC ) * ( VAR( ind_L_NH4OH ) + &
                                   VAR( ind_L_NH4PLUS ) ) * INVCFAC
      END IF
      
      IF( AERWDEP( LNO3, COR ) .GT. VAR( ind_WD_NO3MIN ) ) THEN
          AERWDEP( LNO3, COR ) = ( 1.0d0 - FNO3ACC ) * VAR( ind_WD_NO3MIN )
          AERWDEP( LNO3, ACC ) = FNO3ACC * VAR( ind_WD_NO3MIN )
          AEROSOL( LNO3, COR ) = ( 1.0d0 - FNO3ACC ) * ( VAR( ind_L_HNO3 ) + &
                                   VAR( ind_L_NO3MIN ) ) * INVCFAC
      END IF
      
      IF( AERWDEP( LCL, COR) .GT. VAR( ind_WD_CLMIN ) ) THEN
          AERWDEP( LCL, COR) = ( 1.0d0 - FCLACC ) * VAR( ind_WD_CLMIN )
          AERWDEP( LCL, ACC) = FCLACC * VAR( ind_WD_CLMIN )
          AEROSOL( LCL, COR) = ( 1.0d0 - FCLACC ) * VAR( ind_L_CLMIN ) * INVCFAC
      END IF      
        
!...AEROSOL species, accumulation mode
           
      AEROSOL( LPRI, ACC )  = VAR( ind_L_PRIACC ) * INVCFAC
      AEROSOL( LEC, ACC )   = VAR( ind_L_PECACC ) * INVCFAC 
      AEROSOL( LORGC, ACC ) = VAR( ind_L_ORGC ) * INVCFAC 
      AEROSOL( LPOA, ACC )  = VAR( ind_L_POAACC ) * INVCFAC 
!      AEROSOL( LSOA, ACC )  = AEROSOL( LSOA, ACC ) * EXPWET   ! SOA is only impacted by wet dep process 
!                                                              ! and not included in the list of dynamic 
!                                                              ! species, VAR             
		       
		       
      
      IF( AEROSOL(LTRACER_ACC, ACC) .GT. 0.d0 ) THEN      
         FRACTR = MIN(((TRACIinit - AEROSOL(LTRACER_AKN, AKN))*EXPWET) /  &
	 AEROSOL( LTRACER_ACC, ACC ), 1.0D0) 
      ELSE 
         FRACTR = 0.d0
      END IF
      
      IF( AEROSOL(LPOA, ACC) .GT. 0.d0 ) THEN      
         FRACPOA = MIN(((POAIinit - AEROSOL(LPOA, AKN))*EXPWET) / &
	 AEROSOL( LPOA, ACC ), 1.0D0) 
      ELSE 
         FRACPOA = 0.d0
      END IF
      
      IF( AEROSOL(LPRI, ACC) .GT. 0.d0 ) THEN      
         FRACPRI = MIN(((PRIIinit - AEROSOL(LPRI, AKN))*EXPWET) / &
	 AEROSOL( LPRI, ACC ), 1.0D0) 
      ELSE 
         FRACPRI = 0.d0
      END IF
      
      IF( AEROSOL(LSOA, ACC) .GT. 0.d0 ) THEN      
         FRACSOA = MIN(((SOAIinit - AEROSOL(LSOA, AKN))*EXPWET) / &
	 AEROSOL( LSOA, ACC ), 1.0D0) 
      ELSE 
         FRACSOA = 0.d0
      END IF
      

      IF(ISPC8 .gt. 0) THEN                                                                                               
         AEROSOL( LIETET, ACC ) = VAR( ind_L_IETET ) * INVCFAC
         AEROSOL( LIEOS, ACC )  = VAR( ind_L_IEOS ) * INVCFAC
         AEROSOL( LDIMER, ACC ) = VAR( ind_L_DIMER ) * INVCFAC
         AEROSOL( LIMGA, ACC )  = VAR( ind_L_IMGA ) * INVCFAC
         AEROSOL( LIMOS, ACC )  = VAR( ind_L_IMOS ) * INVCFAC 
      ELSE
         AEROSOL( LISO3, ACC )  = ( VAR( ind_L_ISO3 ) ) * INVCFAC
      END IF
      
      FECOR   = SOIL_FE_FAC * AEROSOL( LSOILC, COR ) + CORS_FE_FAC * AEROSOL( LANTHC, COR )
      MNCOR   = SOIL_MN_FAC * AEROSOL( LSOILC, COR ) + CORS_MN_FAC * AEROSOL( LANTHC, COR )
      NACOR   = SEAS_NA_FAC * AEROSOL( LSEASC, COR ) + SOIL_NA_FAC * AEROSOL( LSOILC, COR )  &
              + CORS_NA_FAC * AEROSOL( LANTHC, COR )
      MGCOR   = SEAS_MG_FAC * AEROSOL( LSEASC, COR ) + SOIL_MG_FAC * AEROSOL( LSOILC, COR)  &
              + CORS_MG_FAC * AEROSOL( LANTHC, COR )
      CACOR   = SEAS_CA_FAC * AEROSOL( LSEASC, COR ) + SOIL_CA_FAC * AEROSOL( LSOILC, COR)  &
              + CORS_CA_FAC * AEROSOL( LANTHC, COR )
      KCOR    = SEAS_K_FAC  * AEROSOL( LSEASC, COR ) + SOIL_K_FAC  * AEROSOL( LSOILC, COR )  &
              + CORS_K_FAC  * AEROSOL( LANTHC, COR )    
      
      AEROSOL( LFEACC, ACC ) = MAX( ( VAR( ind_L_FEPLUS3 ) / FE_III / FE_SOL - FECOR * CFACTOR ) * & 
                                      INVCFAC, 0.0d0 )
      AEROSOL( LMNACC, ACC ) = MAX( ( VAR( ind_L_MNPLUS2 ) / MN_II / MN_SOL - MNCOR * CFACTOR ) *  &
                                      INVCFAC, 0.0d0 )                  
      AEROSOL( LNA, ACC )    = MAX( ( VAR( ind_L_NAPLUS ) - NACOR * CFACTOR ) * INVCFAC, 0.0d0 )
      AEROSOL( LCAACC, ACC ) = MAX( ( VAR( ind_L_CAPLUS2 ) - CACOR * CFACTOR ) * INVCFAC, 0.0d0 ) 
      AEROSOL( LMGACC, ACC ) = MAX( ( VAR( ind_L_MGPLUS2 ) - MGCOR * CFACTOR ) * INVCFAC, 0.0d0 ) 
      AEROSOL( LKACC, ACC )  = MAX( ( VAR( ind_L_KPLUS ) - KCOR * CFACTOR ) * INVCFAC, 0.0d0 ) 
      
      AEROSOL( LSO4, ACC ) = MAX( ( (VAR( ind_L_H2SO4 ) + VAR( ind_L_SO4MIN2 ) +    &
                             VAR( ind_L_HSO4MIN ) ) * INVCFAC ) - AEROSOL( LSO4, COR ), 0.0d0 ) 
      AEROSOL( LCL, ACC ) =  MAX( ( VAR( ind_L_CLMIN ) * INVCFAC ) - AEROSOL (LCL, COR ), 0.0d0 )    
      
      TOTAMM = VAR( ind_G_NH3 ) + VAR( ind_L_NH4OH ) + VAR( ind_L_NH4PLUS ) - &
               AEROSOL( LNH4, COR ) * CFACTOR
      TOTNIT = VAR( ind_G_HNO3 ) + VAR( ind_L_HNO3 ) + VAR( ind_L_NO3MIN ) -  &
               AEROSOL( LNO3, COR ) * CFACTOR

      TOTAMM = MAX( TOTAMM, 0.0d0 )
      TOTNIT = MAX( TOTNIT, 0.0d0 )
      
      AEROSOL( LNO3, ACC ) = ( ( 1.0d0 - FHNO3 ) * TOTNIT ) * INVCFAC
      AEROSOL (LNH4, ACC ) = ( ( 1.0d0 - FNH3 ) * TOTAMM ) * INVCFAC            

!...Gas phase species

      GAS(LSO2)   = ( VAR( ind_G_SO2 ) + VAR( ind_L_SO2 ) + VAR( ind_L_HSO3MIN ) + &
                    VAR( ind_L_SO3MIN2 ) + VAR( ind_L_HMSMIN ) + VAR( ind_L_SO3MIN ) + &
                    VAR( ind_L_SO5MIN ) + VAR( ind_L_HSO5MIN ) + VAR( ind_L_SO4MIN )) * INVCFAC
      GAS(LN2O5)  = 0.0D0
      GAS(LCO2)   = ( VAR( ind_G_CO2 ) + VAR( ind_L_H2CO3 ) + VAR( ind_L_HCO3MIN ) + & 
                    VAR( ind_L_CO3MIN2 ) + VAR( ind_L_CO2MIN )) * INVCFAC
      GAS(LH2O2)  = ( VAR( ind_G_H2O2 ) + VAR( ind_L_H2O2 ) ) * INVCFAC 
      GAS(LO3)    = ( VAR( ind_G_O3 ) + VAR( ind_L_O3 ) ) * INVCFAC  
      GAS(LFOA)   = ( VAR( ind_G_HCOOH ) + VAR( ind_L_HCOOH ) + VAR( ind_L_HCOOMIN ) ) * INVCFAC
      GAS(LMHP)   = ( VAR( ind_G_MHP ) + VAR( ind_L_MHP ) ) * INVCFAC
      GAS(LPAA)   = ( VAR( ind_G_PAA ) + VAR( ind_L_PAA ) ) * INVCFAC 
      GAS(LH2SO4) = 0.0D0
      GAS(LHCL)   = ( VAR( ind_G_HCL ) + VAR( ind_L_HCL ) ) * INVCFAC 
      GAS(LGLY)   = ( VAR( ind_G_GLY ) + VAR( ind_L_GLY )*(1.d0-OLIGGLY) ) * INVCFAC
      GAS(LMGLY)  = ( VAR( ind_G_MGLY ) + VAR( ind_L_MGLY ) *(1.d0-OLIGMGLY)) * INVCFAC
      GAS(LHNO3)  = ( FHNO3 * TOTNIT ) * INVCFAC
      GAS(LNH3)   = ( FNH3 * TOTAMM ) * INVCFAC  
! Fixed OHg vs variable       
      GAS(LHO)    = ( FIX( indf_G_HO ) ) * INVCFAC    
!      GAS(LHO)    = ( VAR( ind_G_HO ) + VAR( ind_L_HO ) ) * INVCFAC 

      GAS( LIEPOX ) = ( VAR( ind_G_IEPOX ) + VAR( ind_L_IEPOX ) ) * INVCFAC
      
      IF( ISPC8 .GT. 0 ) THEN  
      GAS( LIMAE )  = ( VAR( ind_G_IMAE ) + VAR( ind_L_IMAE ) ) * INVCFAC 
      GAS( LIHMML ) = ( VAR( ind_G_IHMML ) + VAR( ind_L_IHMML ) ) * INVCFAC 
      END IF
      
      GAS(LHO2)   = ( VAR( ind_G_HO2 ) + VAR( ind_L_HO2 ) + VAR( ind_L_O2MIN ) ) * INVCFAC   
      GAS(LNO2)   = ( VAR( ind_G_NO2 ) + VAR( ind_L_NO2 ) ) * INVCFAC 
      GAS(LHONO)  = ( VAR( ind_G_HONO ) + VAR( ind_L_HONO ) + VAR( ind_L_NO2MIN ) ) * INVCFAC 
      GAS(LHNO4)  = ( VAR( ind_G_HNO4 ) + VAR( ind_L_HNO4 ) + VAR( ind_L_NO4MIN ) ) * INVCFAC 
      
      GAS(LNO3RAD) = ( VAR( ind_G_NO3 ) + VAR( ind_L_NO3 ) ) * INVCFAC 
      GAS(LCH3O2)  = ( VAR( ind_G_CH3O2 ) + VAR( ind_L_CH3O2 ) ) * INVCFAC     
      GAS(LCCOOH)  = ( VAR(ind_G_CCOOH) + VAR(ind_L_CCOOH) + VAR(ind_L_CCOOHMIN) )*INVCFAC
      GAS(LHCHO)   = (VAR(ind_G_HCHO) + VAR(ind_L_CH2OHYD) + VAR(ind_L_HCHO) + VAR(ind_L_HMSMIN) )*INVCFAC
      GAS(LGCOL)   = (VAR(ind_G_GCOL) + VAR(ind_L_GCOL) )*INVCFAC

      GAS(LHCHOP)  = (VAR(ind_G_HCHOP) + VAR(ind_L_CH2OHYDP) + VAR(ind_L_HCHOP) + VAR(ind_L_HMSMINP) )*INVCFAC 

!...Gas phase species deposition amounts

      GASWDEP( LSO2 )   = VAR( ind_WD_SO2 ) 
      GASWDEP( LHNO3 )  = VAR( ind_WD_HNO3 )
      GASWDEP( LN2O5 )  = 0.0D0                  ! already transferred to HNO3
      GASWDEP( LCO2 )   = VAR( ind_WD_CO2 )
      GASWDEP( LNH3 )   = VAR( ind_WD_NH4OH )
      GASWDEP( LH2O2 )  = VAR( ind_WD_H2O2 )
      GASWDEP( LO3 )    = VAR( ind_WD_O3 )
      GASWDEP( LFOA )   = VAR( ind_WD_HCOOH )
      GASWDEP( LMHP )   = VAR( ind_WD_MHP )
      GASWDEP( LPAA )   = VAR( ind_WD_PAA )
      GASWDEP( LH2SO4 ) = 0.0D0                 ! already transferred to SO4
      GASWDEP( LHCL )   = VAR( ind_WD_HCL )
      GASWDEP( LGLY )   = VAR( ind_WD_GLY ) * (1.d0-OLIGGLY)
      GASWDEP( LMGLY )  = VAR( ind_WD_MGLY ) * (1.d0-OLIGMGLY)
      GASWDEP( LHO )    = VAR( ind_WD_HO )
      
      GASWDEP( LIEPOX ) = VAR( ind_WD_IEPOX )
      
      IF( ISPC8 .GT. 0 ) THEN 
         GASWDEP( LIMAE )  = VAR( ind_WD_IMAE )
         GASWDEP( LIHMML ) = VAR( ind_WD_IHMML ) 
      END IF     
      
      GASWDEP( LNO2 )   = VAR( ind_WD_NO2 )
      GASWDEP( LHONO )  = VAR( ind_WD_HONO )
      GASWDEP( LHNO4 )  = VAR( ind_WD_HNO4 )
      
      GASWDEP( LNO3RAD )  = VAR( ind_WD_NO3 )
      GASWDEP( LCH3O2 ) = VAR( ind_WD_CH3O2 )
      GASWDEP( LGCOL) = VAR( ind_WD_GCOL )
      GASWDEP( LCCOOH ) = VAR( ind_WD_CCOOH ) 
      GASWDEP( LHCHO ) = VAR( ind_WD_CH2OHYD )
      GASWDEP( LHO2 ) = VAR( ind_WD_HO2 )
      GASWDEP( LHCHOP ) = VAR( ind_WD_CH2OHYDP )
      
      APYRAC = 0.d0 
      WDPYRAC = 0.d0
      
      IF( MTPYRAC .GT. 0 ) THEN
         GAS( LPYRUV ) = ( VAR( ind_G_PYRAC ) + VAR( ind_L_PYRAC ) )*INVCFAC  ! returning G_PYRAC and L_PYRAC
                                                                              ! to the gas phase when gas phase
                                                                              ! pyruvic acid species exists in 
                                                                              ! gas phase mechanism
         GASWDEP( LPYRUV ) = VAR( ind_WD_PYRAC )
         WDPYRAC = 0
         APYRAC = VAR( ind_L_PYRACMIN )
      ELSE
         WDPYRAC = VAR(ind_WD_PYRAC) 
         APYRAC = VAR( ind_L_PYRAC ) + VAR( ind_L_PYRACMIN ) 
      END IF
      

      AEROSOL(LORGC,ACC) = (VAR(ind_L_ORGC) + ((74.04/177.)*VAR(ind_L_GLYAC)) + ((90.03/177.)*VAR(ind_L_OXLAC)) &
      + ((90.03/177.)*VAR(ind_L_OXLACMIN)) + ((90.03/177.)*VAR(ind_L_OXLACMIN2)) + ((88.06/177.)* APYRAC ) & 
      + ((76.05/177.)*VAR(ind_L_GCOLAC)) + ((74.04/177.)*VAR(ind_L_GLYACMIN)) &
      + ((76.05/177.)*VAR(ind_L_GCOLACMIN)) &
      + (58.04/177.)*OLIGGLY*VAR(ind_L_GLY) + (72.06/177.)*OLIGMGLY*VAR(ind_L_MGLY))*INVCFAC
  
      AERWDEP(LORGC,ACC) = VAR(ind_WD_ORGC) + ((74.04/177.)*VAR(ind_WD_GLYAC)) + &
                          ((90.03/177.)*VAR(ind_WD_OXLAC))  + & 
                          ((88.06/177.)* WDPYRAC) + &
                          ((76.05/177.)*VAR(ind_WD_GCOLAC)) + &
                           (58.04/177.)*OLIGGLY*VAR(ind_WD_GLY) + (72.06/177.)*OLIGMGLY*VAR(ind_WD_MGLY)
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
!...Convert to appropriate units (mol / m2)
     
      DO I = 1,NGAS
         GASWDEP( I ) = GASWDEP( I ) * WFACTOR
      END DO
     
      DO J = 1, NMODES
      DO I = 1, NAER
         AERWDEP( I, J ) = AERWDEP( I, J ) * WFACTOR
      END DO
      END DO     
     
      DO I = 1, NGAS
         IF( GAS( I ) .LT. 0.0d0 )     GAS( I ) = 0.0d0
         IF( GASWDEP( I ) .LT. 0.0d0 ) GASWDEP( I ) = 0.0d0
      END DO
     
      DO J =1, NMODES
      DO I =1, NAER
         IF( AEROSOL( I, J ) .LT. 0.d0 ) AEROSOL( I, J ) = 0.d0
         IF( AERWDEP( I, J ) .LT. 0.d0 ) AERWDEP( I, J ) = 0.d0
      END DO
      END DO  

!...Store the amount of hydrogen deposition

      HPWDEP = VAR( ind_WD_HPLUS ) * WFACTOR
      BETASO4 = 0.0D0
      DEPSUM =  AERWDEP( LSO4, ACC ) / WFACTOR
      
      IF( AEROSOL( LSO4, ACC ) * CFACTOR + DEPSUM .GT. 0.0d0 ) THEN
         BETASO4 = DEPSUM / ( AEROSOL( LSO4, ACC ) * CFACTOR + DEPSUM ) / &
                   TAUCLD      
      ELSE
         BETASO4 = 0.d0
      END IF
      
      AEROSOL( LNUM, ACC ) = AEROSOL( LNUM, ACC ) * EXP( -BETASO4 * TAUCLD )   

!...Mass balance check - end
                
        ENDM(1) = (GAS(LSO2) + GAS(LH2SO4) + (GASWDEP(LSO2) + &
                  GASWDEP(LH2SO4))/WFACTOR/CFACTOR) * 32.06
        ENDM(2) = (GAS(LHNO3) + 2*GAS(LN2O5) + GAS(LNO2) + GAS(LHONO) + &
                  GAS(LHNO4) + GAS(LNO3RAD)+(GASWDEP(LHNO3) + GASWDEP(LNO2) + GASWDEP(LHONO) + &
                  GASWDEP(LHNO4) + GASWDEP(LNO3RAD))/WFACTOR/CFACTOR)*14.007  !
        ENDM(3) = (GAS(LNH3)+ GASWDEP(LNH3)/WFACTOR/CFACTOR)*14.007      
        ENDM(4) = (GAS(LHCL)+GASWDEP(LHCL)/WFACTOR/CFACTOR)*35.5 
          
        DO I = 1,NMODES
           ENDM(1) = ENDM(1) + AEROSOL(LSO4, I)*32.06
           ENDM(2) = ENDM(2) + AEROSOL(LNO3, I)*14.007
           ENDM(3) = ENDM(3) + AEROSOL(LNH4, I)*14.007
           ENDM(4) = ENDM(4) + AEROSOL(LCL, I)*35.5
        ENDDO 
   
        DO I = 1,NMODES
           ENDM(1) = ENDM(1) + (AERWDEP(LSO4,I)/WFACTOR/CFACTOR)*32.06
           ENDM(2) = ENDM(2) + (AERWDEP(LNO3,I)/WFACTOR/CFACTOR)*14.007
           ENDM(3) = ENDM(3) + (AERWDEP(LNH4,I)/WFACTOR/CFACTOR)*14.007
           ENDM(4) = ENDM(4) + (AERWDEP(LCL,I)/WFACTOR/CFACTOR)*35.5
        ENDDO
                  
        DO I = 1,4
                        
           IF(STARTM(I) .GT. 0.d0) THEN
              MBAL(I) = 100*(STARTM(I) - ENDM(I)) / STARTM(I)
           ELSE
              MBAL(I) = 0.d0
           END IF
             
           IF( ABS(MBAL(I)) .GT. 0.5 ) THEN         
              write(logdev,*) 'POTL MBAL PROB IN AQCHEM'
              write(logdev,*) '1=S, 2=N(not NH3), 3=NH3/4, 4=CL'
              write(logdev,*) 'I, START, END'
              write(logdev,*) I, STARTM(I), ENDM(I)
              XMSG = 'Mass balance problem in KMT?'
              CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
           END IF  
                      
        END DO
     
      RETURN

!...formats

      END
