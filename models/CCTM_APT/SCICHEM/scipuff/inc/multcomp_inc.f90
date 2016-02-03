!******************************************************************************
!Added variables for more reaction types and particle number and surface area
!concentrations
!PK, AER, January 2005
!Updated March 22, 2005 for backward compatibility for reaction type 4
!Changed species names from C*8 to C*16 for CMAQ-APT-PM, April 2005, PK AER
! 23 MAY 2005: Added release list pointers to account for moving equilibrium
!              species to end of species list (RIS)
!Calculate and store dry and wet dep separately, Oct 2005, PK, AER
!Add multcomp_mc_inc-PK, AER, August 2007
! 01/31/2007 : Add multcomp_mc_inc  -BC
! March 2012 : Updated for CMAQ 5.0 final, PK, ENVIRON
!******************************************************************************

module multcomp_inc
use multcomp_mc_inc
use param_inc
use units_inc
use aer_sections_inc

implicit none
save

! Multi-component variables

!==== parameters

! Mode flags for read_mc
integer  MODE_NA,   MODE_S,   MODE_EQ,   MODE_T,   MODE_CTRL
integer  MODE_AQ,   MODE_BAL, MODE_LIM
parameter (MODE_NA=0, MODE_S=2, MODE_EQ=3, MODE_T=4, MODE_CTRL=1)
parameter (MODE_AQ=5, MODE_BAL=6, MODE_LIM=7)
!       MODE_CNTRL must be first since needs lstage to read equations

integer    MAX_EQ      !Max # of equilibrium variables
parameter (MAX_EQ   = 20)

integer    MAX_AMB     !Max # of ambient variables
parameter (MAX_AMB  = 10)

integer    MAX_STAGES   !Max # of chemical stages
parameter (MAX_STAGES  = 3)

integer MAX_VOC !MAX # of VOCs
parameter (MAX_VOC = 20)

integer    MAX_ZENITH  !Max # of zenith values
integer    MAX_KTAB    !Max # of radiation dependent reaction rates
parameter (MAX_ZENITH = 20)
parameter (MAX_KTAB   = 30)

integer    MAX_PRODUCTS!Max # of products in any one reaction
integer    MAX_KDATA   !Max # of reaction rate coefficients
parameter (MAX_PRODUCTS  =  10)
parameter (MAX_KDATA     =   6)

integer    ID_REACT_FAST        !Fast species bit in reaction class
integer    ID_REACT_SLOW        !Slow species bit in reaction class
integer    ID_REACT_EQUILIBRIUM !Equilibrium species bit in reaction class
integer    ID_REACT_LINEAR      !Linear reaction bit in reaction class
parameter (ID_REACT_FAST = 1)
parameter (ID_REACT_SLOW = 2)
parameter (ID_REACT_EQUILIBRIUM = 3)
parameter (ID_REACT_LINEAR      = 4)

integer    ID_SPECIES_FAST     !Fast species class identifier
integer    ID_SPECIES_SLOW     !Slow species class identifier
integer    ID_SPECIES_EQUILIBRIUM      !Equilibrium species class identifier
integer    ID_SPECIES_AMBIENT  !Ambient species class identifier
integer    ID_SPECIES_PARTICLE !Aerosol particle species class identifier
parameter (ID_SPECIES_FAST = 1)
parameter (ID_SPECIES_SLOW = 2)
parameter (ID_SPECIES_EQUILIBRIUM = 3)
parameter (ID_SPECIES_AMBIENT     = 4)
parameter (ID_SPECIES_PARTICLE    = 5)

!Aerosol number class identifier
integer, parameter :: ID_SPECIES_NUMBER = 6

!Aerosol surface area class identifier
integer, parameter :: ID_SPECIES_SURFACE = 7

integer, parameter :: ID_K_RAD = 0       !Radiation dependent (photolysis)
integer, parameter :: ID_K_CONST = 1     !Constant
integer, parameter :: ID_K_TEMP = 2      !Temperature dependent (type 1)
integer, parameter :: ID_K_PRES = 3      !Pressure dependent (type 1)
integer, parameter :: ID_K_H2O = 4       !H2O dependent (type 1)
integer, parameter :: ID_K_M = 5         !M dependent
integer, parameter :: ID_K_LWC = 6       !LWC (and drop diam) dependent
integer, parameter :: ID_K_PRES2 = 7     !Pressure dependent (type 2)
integer, parameter :: ID_K_EQM = 8       !reverse decomposition
integer, parameter :: ID_K_O2 = 9        !O2 dependent
integer, parameter :: ID_K_N2 = 10       !N2 dependent
integer, parameter :: ID_K_FALLOFF1 = 11 !Falloff (type 1)
integer, parameter :: ID_K_FALLOFF2 = 12 !Falloff (type 2)
integer, parameter :: ID_K_FALLOFF3 = 13 !Falloff (type 3)
integer, parameter :: ID_K_CH4 = 14      !CH4 dependent
integer, parameter :: ID_K_H2OB = 15     !H2O dependent (type 2)

integer   NO_CONVERSION      !Rate/Species in same units
integer   MOLECULE_PPM       !molecule/cm3 <-> PPM conversion
integer   G_PPM !grams <-> PPM conversion
integer   G_MOLECULE !grams <-> molecule/cm3
parameter  (NO_CONVERSION = 0)
parameter  (MOLECULE_PPM  = 1)
parameter  (G_PPM         = 2)
parameter  (G_MOLECULE    = 3)

integer   IDIR_EQ     !direct solution for equilibrium
integer   ISUBS_EQ    !final substitution for equilibrium
integer   ISOLVE_EQ   !fully coupled solution for equilibrium
parameter  (IDIR_EQ   = -2)     !used in init_equilibrium
parameter  (ISUBS_EQ  = -1)     !and set_equilibrium
parameter  (ISOLVE_EQ =  0)

integer    NCRITERIA    !# of criteria for staged reactions
parameter (NCRITERIA = 6)

integer    NKEY_SPEC    !# of key species for staged rxns
parameter (NKEY_SPEC = 10)

integer    NKEY_RXNS    !# of key reactions for staged rxns
parameter (NKEY_RXNS = 5)

integer IPHNO3, IPNO3, INOO3, INO3NO2, INOHO2 !pointers for key rxns
parameter (IPHNO3  = 1) !production of HNO3 by NO and OH
parameter (IPNO3   = 2) !production of NO3 by NO2 and O3
parameter (INOO3   = 3) !NO + O3 --> NO2
parameter (INO3NO2 = 4) !NO3 + NO2 --> N2O5
parameter (INOHO2  = 5) !reaction of NO with HO2

integer NO, NO2, O3, OH, HO2, NO3, HNO3, N2O5, O1D, C2O3
parameter (NO   = 1)    !pointers for key species
parameter (NO2  = 2)    !in staged chemistry
parameter (O3   = 3)
parameter (OH   = 4)
parameter (HO2  = 5)
parameter (NO3  = 6)
parameter (HNO3 = 7)
parameter (N2O5 = 8)
parameter (O1D  = 9)
parameter (C2O3 = 10)

integer    ID_LSODE   !Use LSODE to solve the rate equations
integer    ID_VODE    !Use VODE to solve the rate equations
integer    ID_YNB     !Use Young & Boris to solve the rate equations
parameter (ID_LSODE = 1)
parameter (ID_VODE  = 2)
parameter (ID_YNB   = 3)

!==== Header data

integer ncorrm  !# of mean correlation species (star)
integer ncorrt  !# of turbulent correlations
integer nreacts !# of total reactions
integer nstage  !# of stages (must be 1 or 3)
integer nvoc    !# of VOCs
integer nzenith !# of zenith angles in k vs zenith table
integer nkrad   !# of radiation dependent rates
integer i_units !Unit conversion index
integer ie_units !Unit conversion index for emissions
integer isolve   !solver to use for rate equations

real  em_conv      !Emissions conversion factor
real  ks_conv      !Rate species conversion factor
real  kt_conv      !Rate time conversion factor
real  rtol(1)      !Relative tolerance for all species
real  param_chem   !Lower limit on ozone conc (in YNB) as fraction of ambient

character*16   mc_units
character*128  amb_file    !3D ambient concentrations file

logical lstep_amb          !Ambient stepping flag
logical lstage             !Staged chemistry flag
logical lchem_split        !Use chemical criteria to split puffs
logical laqueous           !Include aqueous phase chemistry
logical lstep_amb3d        !3D ambient stepping flag from amb_file
logical ldump_chem         !Use chemical criteria to dump puffs
logical lsolve_ynb         !Young & Boris chemistry solver (no longer used)
                           !(replaced by isolve)
                           !(must stay on namelist for backwards compatability)
!==== Species data

type  chem_species
  sequence

  integer class !Species type
  integer star  !Pointer for mean correlation
  integer dos   !Srf dos pointer
  integer dep   !Srf dep pointer
  integer pmode !species mode (if particle)

  real    amb   !Ambient value
  real    tol   !Tolerance
  real    vdep  !Deposition velocity
  real    emis_split    !Fraction of emitted species that is this species
  real    density       !Particle density (if particle)

  character*16 name     !Species name
  character*16 nameemit !Name of surrogate emitted species
end type  chem_species

type (chem_species)  species(MAX_MC)

type  param_species
  sequence
    real    scav !Scavenging coefficient
    real    mwt  !Molecular Weight (grams/mole)
    real    nit  !Number of nitrogen molecules
    real    lim  !whether or not species should be used to limit the volume
    real    param(6) !Save space for other parameters
end type  param_species

type (param_species)  sparam(MAX_MC)
real nitr(MAX_MC)

type  equil_species
  sequence
  integer class  !Species type
  integer star   !Pointer for mean correlati
  real    amb    !Ambient value
  real    tol    !Tolerance
  character*16 name       !Species name
end type  equil_species

type (equil_species)  equilibrium(MAX_EQ)

type  amb_species
  sequence
  integer class  !Species type
  real    amb    !Ambient value
  character*16 name       !Species name
end type  amb_species

type (amb_species)  ambient(MAX_AMB)

integer indxf(MAX_MC)    !List of Fast species IDs
integer indxf_s(MAX_MC,MAX_STAGES)       !List of Fast species IDs
integer indxs(MAX_MC)    !List of Slow species IDs
integer indxs_s(MAX_MC,MAX_STAGES)       !List of Slow species IDs
integer indx_corr(MAX_REACTIONS) !List of correlation reaction IDs

!==== Reaction data

type  chem_reaction
  sequence
  integer class  !Reaction Class
  integer ID     !Reactant ID
  integer iA     !Reactant species ID
  integer iB     !Reactant species ID
  integer nP     !# of products
  integer icorr  !Reaction turbulent correla
  integer ktype  !Rate type
  integer iP(MAX_PRODUCTS)       !Product species IDs
  real    k      !Current rate
  real    fB     !Reactant stoichiometric co
  real    fP(MAX_PRODUCTS)       !Product stoichiometric coe
  real    kdata(MAX_KDATA)       !Rate data
end type  chem_reaction

type (chem_reaction) reaction(MAX_REACTIONS)

real    tk, pk, hk, cldk !Values of temp, pressure,
                         !humidity and LWC last use to compute rates

!==== k vs zenith table data

integer indxkr(MAX_KTAB) !List of radiation dependent reaction IDs
integer nreadkr(MAX_KTAB) !List of # of zenith angles read
real    zenith(MAX_ZENITH)       !Zenith angles in table
real    ktable(MAX_ZENITH, MAX_KTAB)     !Rate table

!==== Working space

type (work_species)  pswork(MAX_MC), psamb (MAX_MC)

logical lstep_ynb     !flag to use Young & Boris method instead of LSODE
real chem_damp(MAX_NCORR)

logical lflag_amb !flag used to step total vs perts
logical lstep_tot !flag used to step total and ambient vs perts
integer H2O
real conc_lim(MAX_MC) !limit to use on concentrations (different if stepping ambient)
integer ic_units !Concentration units index
integer ik_units !Rate concentration units index
integer ik_conv !Rate conversion index
integer ie_conv !Emissions conversion index
                ! NO_CONVERSION=no conversion (conv=1.0)
                ! MOLECULE_PPM =cm3/molecule -> 1/PPM (conv=7.34e+15*(P/T))
                !
                ! + -> rate = rate*conv
                ! - -> rate = rate/conv

integer indx_eq(MAX_EQ)   !List of equilibrium species IDs,
                          !order in which they must be solved
integer itype_eq(MAX_EQ)  !List of solution type for eqm species
integer irow_eq(MAX_EQ)   !List of row #s that eqm species are in
integer nsolve_eq         !# of species that must be solved
integer nlin_eq           !# of species that can be eliminated
integer ndir_eq           !# of species that can solved directly
integer nsubs_eq          !# of species that can be subs. at the end
                          !Note: these are now temporary since intro of stages

integer nspec_s(MAX_STAGES)     !# of species in each stage
integer nfast                   !temporary no. of fast species
integer neq_s                   !temporary no. of equilibrium species
integer nslow                   !temporary no. of slow species
integer nslow_s(MAX_STAGES)     !# of slowly varying species in each stage
integer nfast_s(MAX_STAGES)     !# of rapidly varying species (LSODE) in each stage
integer neqm_s(MAX_STAGES)      !# of equilibrium species in each stage
integer nreact_s(MAX_STAGES)    !# of reactions in each stage
integer indx_spec(MAX_MC,MAX_STAGES) !List of species in each stages
integer indx_eq_s(MAX_EQ,MAX_STAGES) !List of equilibrium species IDs (order to be solved) i
integer itype_eq_s(MAX_EQ,MAX_STAGES)!List of solution type for eqm species for each stage
integer irow_eq_s(MAX_EQ,MAX_STAGES) !List of row #s that eqm species are in for each stage
integer nsolve_eq_s(MAX_STAGES)   !# of species that must be solved for each stage
integer nlin_eq_s(MAX_STAGES)     !# of species that can be eliminated for each stage
integer ndir_eq_s(MAX_STAGES)     !# of species that can solved directly for each stage
integer nsubs_eq_s(MAX_STAGES)    !# of species that can be subs. at the end for each stage
integer indx_rxns(MAX_REACTIONS)  !index of rxns to be considered
integer nrxn_curr                 !# of rxns to be considered
integer nreact_voc                !# of VOC + NO3 reactions - used for criteria
integer indx_voc(MAX_VOC)         !species id numbers that correspond to VOCs
integer indx_react_voc(MAX_REACTIONS) !list of reactions that are VOC + NO3
integer ikey_rxn(NKEY_RXNS)
integer ikey_spec(NKEY_SPEC)
real phno3,pno3,cno3,rho2o3,co3,rvocno2 !criteria for stages
character*16 voc_names(MAX_VOC)   !user provided list of VOC names
logical lchng_rad                 !if there is a night/day transition

real criteria(NCRITERIA)
common /mc_crit/ phno3,pno3,cno3,rho2o3,co3,rvocno2 !criteria for stages
equivalence (criteria(1),phno3)

real ypre(MAX_MC), yc1(MAX_MC), yc2(MAX_MC)
real ydpre(MAX_MC), apre(MAX_MC), bpre(MAX_MC)
real ydc1(MAX_MC), ac1(MAX_MC), bc1(MAX_MC)
integer istiff(MAX_MC)

integer i_spec_list(MAX_MC)
integer i_hostspec_list(MAX_MC)
integer i_eq_list(MAX_MC)
integer i_rel_list(MAX_MC)

integer index_aero(MAX_MC) !pointers
integer naero !number of required species for aerosol chem
character*16 aero_names(MAX_MC)

real rho_aer, secbnds_aer(MAX_SEC+1), dm_aer(MAX_SEC)
real diff_aer(MAX_SEC), vs_aer(MAX_SEC), twash_aer(0:NWASH,MAX_SEC)

integer index_aqueous(MAX_MC) !pointers to aqueous species
integer naqueous  !number of required species for aqueous chem/scavenging
integer naqchem   !number of required species for aqueous chem only
character*16 aqueous_names(MAX_MC)

integer nemit, index_emit(MAX_MC)
character*16 nameemit(MAX_MC)

logical lbalance

integer nlim
integer index_lim(MAX_MC)

logical IsLinear(MAX_REACTIONS), IsStar(MAX_MC)

real concsav(MAX_MC)   !concentrations at the beginning of step_mc

end module multcomp_inc
