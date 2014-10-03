       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: /home/hwo/CCTM_git_repository/MECHS/cb05tump_ae6_aq/mech_cb05tump_ae6_aq.def
! for Mechanism Name: CB05TUMP_AE6_AQ                 

! This file is used to create mechanism data and functions

! The following are reserved symbols declared in this file:
!    MECHNAME       = Mechanism name
!    N_GAS_CHEM_SPC = Total number of gas species in chemical mechanism
!    NUMB_CHEM_SPC  = Total number of species in chemical mechanism
!    N_ACT_SP       = Number of active (determined by ODE solver) species in mechanism
!    GAS_CHEM_SPC   = Names of gas species in chemical mechanism
!    CHEMISTRY_SPC  = Names of species in chemical mechanism
!    CGRID_INDEX    = CGRID Index of species in chemical mechanism
!    SPECIES_TYPE   = Group or type of species 
!    SPECIES_MOLWT  = Molecular Weight of species (gm/mole)
!    NRXNS          = Number of mechanism reactions
!    KUNITS         = Units of mechanism reactions
!    KTYPE          = Reaction type
!    IRXBITS        = Bit test mask vector for selected reactions
!    IORDER         = Order of the reaction
!    KTN1           = Number of type 1 reactions
!    KRX1           = Reactions list pointer to type 1 reactions
!    KTN2           = Number of type 2 reactions
!    KRX2           = Reactions list pointer to type 2 reactions
!    KTN3           = Number of type 3 reactions
!    KRX3           = Reactions list pointer to type 3 reactions
!    KTN4           = Number of type 4 reactions
!    KRX4           = Reactions list pointer to type 4 reactions
!    KTN5           = Number of type 5 reactions
!    KRX5           = Reactions list pointer to type 5 reactions
!    KTN6           = Number of type 6 reactions
!    KRX6           = Reactions list pointer to type 6 reactions
!    KTN7           = Number of type 7 reactions
!    KRX7           = Reactions list pointer to type 7 reactions

! The following are reserved symbols declared in this file:
!    MECHNAME       = Mechanism name
!    N_GAS_CHEM_SPC = Total number of gas species in chemical mechanism
!    NUMB_CHEM_SPC  = Total number of species in chemical mechanism
!    N_ACT_SP       = Number of active (determined by ODE solver) species in mechanism
!    GAS_CHEM_SPC   = Names of gas species in chemical mechanism
!    CHEMISTRY_SPC  = Names of species in chemical mechanism
!    CGRID_INDEX    = CGRID Index of species in chemical mechanism
!    SPECIES_TYPE   = Group or type of species 
!    SPECIES_MOLWT  = Molecular Weight of species (gm/mole)
!    NRXNS          = Number of mechanism reactions
!    KUNITS         = Units of mechanism reactions
!    KTYPE          = Reaction type
!    IRXBITS        = Bit test mask vector for selected reactions
!    IORDER         = Order of the reaction
!    KTN1           = Number of type 1 reactions
!    KRX1           = Reactions list pointer to type 1 reactions
!    KTN2           = Number of type 2 reactions
!    KRX2           = Reactions list pointer to type 2 reactions
!    KTN3           = Number of type 3 reactions
!    KRX3           = Reactions list pointer to type 3 reactions
!    KTN4           = Number of type 4 reactions
!    KRX4           = Reactions list pointer to type 4 reactions
!    KTN5           = Number of type 5 reactions
!    KRX5           = Reactions list pointer to type 5 reactions
!    KTN6           = Number of type 6 reactions
!    KRX6           = Reactions list pointer to type 6 reactions
!    KTN7           = Number of type 7 reactions
!    KRX7           = Reactions list pointer to type 7 reactions

!    NWM       = Number of air 3-body reactions
!    NRXWM     = Reactions list pointer to air 3-body reactions
!    ATM_AIR   = air 3-body reactions concentration
!    NWW       = Number of H2O 3-body reactions
!    NRXWW     = Reactions list pointer to H2O 3-body reactions
!    NWO2      = Number of reactions with O2
!    NRXWO2    = Reactions list pointer to O2 reactions
!    ATM_O2    = Oxygen reactions concentration
!    NWN2      = Number of N2 3-body reactions
!    NRXWN2    = Reactions list pointer to N2 3-body reactions
!    ATM_N2    = Nitrogen 3-body reactions concentration
!    NWCH4     = Number of reactions with CH4
!    NRXWCH4   = Reactions list pointer to CH4 reactions
!    ATM_CH4   = Methane reactions concentration
!    NWH2      = Number of reactions with H2
!    NRXWH2    = Reactions list pointer to H2 reactions
!    ATM_H2    = Hydrogen reactions concentration

!    MXPRD     = Maximum number of mechanism reaction products
!    IRR       = Reactions list pointer to reactants and products
!    RTDAT     = Kinetic reaction rates expressions components
!    NFALLOFFF = Number of falloff reactions
!    IRRFALL   = Reactions list pointer to falloff reactions
!    RFDAT     = Falloff reaction rates expressions components
!    SC        = Stoichiometric coefficients
!    NREACT    = Number of reactants in each mechanism reaction
!    NPRDCT    = Number of products in each mechanism reaction
!    RXLABEL   = Character label list for mechanism reactions
!    NMPHOT    = Number of mechanism photolytic reactions
!    NPHOTAB   = Number of photolytic reactions tables
!    IPH       = Reactions list pointer to photolytic reactions and tables
!    MHETERO   = Number of mechanism heteorogenous reactions
!    NHETERO   = Number of unique heteorogenous rate constants
!    IHETERO   = Reactions list pointer to heteorogenous reactions and tables

      CHARACTER( 32 ), PARAMETER :: MECHNAME = 'CB05TUMP_AE6_AQ'

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC =  97
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 118

      CHARACTER( 16 ) :: GAS_CHEM_SPC( N_GAS_CHEM_SPC )
      CHARACTER( 16 ) :: CHEMISTRY_SPC( NUMB_MECH_SPC )
      CHARACTER( 16 ) :: SPECIES_TYPE(  NUMB_MECH_SPC )
      INTEGER         :: CGRID_INDEX (  NUMB_MECH_SPC )
      INTEGER         :: TYPE_INDEX  (  NUMB_MECH_SPC )
      LOGICAL         :: CONVERT_CONC(  NUMB_MECH_SPC )
      REAL            :: SPECIES_MOLWT( NUMB_MECH_SPC )

! The below character and integer arrays list the model species names used in the 
! chemical mechanism. The gas species and their order should agree with 
! the GC_SPC array for the gas phase chemistry to work correctly. 
! If present, the CHEMISTRY_SPC names and species type should agree with the CGRID_SPCS module

      DATA GAS_CHEM_SPC(   1 ) / 'NO2             ' /
      DATA GAS_CHEM_SPC(   2 ) / 'NO              ' /
      DATA GAS_CHEM_SPC(   3 ) / 'O               ' /
      DATA GAS_CHEM_SPC(   4 ) / 'O3              ' /
      DATA GAS_CHEM_SPC(   5 ) / 'NO3             ' /
      DATA GAS_CHEM_SPC(   6 ) / 'O1D             ' /
      DATA GAS_CHEM_SPC(   7 ) / 'OH              ' /
      DATA GAS_CHEM_SPC(   8 ) / 'HO2             ' /
      DATA GAS_CHEM_SPC(   9 ) / 'N2O5            ' /
      DATA GAS_CHEM_SPC(  10 ) / 'HNO3            ' /
      DATA GAS_CHEM_SPC(  11 ) / 'HONO            ' /
      DATA GAS_CHEM_SPC(  12 ) / 'PNA             ' /
      DATA GAS_CHEM_SPC(  13 ) / 'H2O2            ' /
      DATA GAS_CHEM_SPC(  14 ) / 'XO2             ' /
      DATA GAS_CHEM_SPC(  15 ) / 'XO2N            ' /
      DATA GAS_CHEM_SPC(  16 ) / 'NTR             ' /
      DATA GAS_CHEM_SPC(  17 ) / 'ROOH            ' /
      DATA GAS_CHEM_SPC(  18 ) / 'FORM            ' /
      DATA GAS_CHEM_SPC(  19 ) / 'ALD2            ' /
      DATA GAS_CHEM_SPC(  20 ) / 'ALDX            ' /
      DATA GAS_CHEM_SPC(  21 ) / 'PAR             ' /
      DATA GAS_CHEM_SPC(  22 ) / 'CO              ' /
      DATA GAS_CHEM_SPC(  23 ) / 'MEO2            ' /
      DATA GAS_CHEM_SPC(  24 ) / 'MEPX            ' /
      DATA GAS_CHEM_SPC(  25 ) / 'MEOH            ' /
      DATA GAS_CHEM_SPC(  26 ) / 'HCO3            ' /
      DATA GAS_CHEM_SPC(  27 ) / 'FACD            ' /
      DATA GAS_CHEM_SPC(  28 ) / 'C2O3            ' /
      DATA GAS_CHEM_SPC(  29 ) / 'PAN             ' /
      DATA GAS_CHEM_SPC(  30 ) / 'PACD            ' /
      DATA GAS_CHEM_SPC(  31 ) / 'AACD            ' /
      DATA GAS_CHEM_SPC(  32 ) / 'CXO3            ' /
      DATA GAS_CHEM_SPC(  33 ) / 'PANX            ' /
      DATA GAS_CHEM_SPC(  34 ) / 'ROR             ' /
      DATA GAS_CHEM_SPC(  35 ) / 'OLE             ' /
      DATA GAS_CHEM_SPC(  36 ) / 'ETH             ' /
      DATA GAS_CHEM_SPC(  37 ) / 'IOLE            ' /
      DATA GAS_CHEM_SPC(  38 ) / 'TOL             ' /
      DATA GAS_CHEM_SPC(  39 ) / 'CRES            ' /
      DATA GAS_CHEM_SPC(  40 ) / 'TO2             ' /
      DATA GAS_CHEM_SPC(  41 ) / 'TOLRO2          ' /
      DATA GAS_CHEM_SPC(  42 ) / 'OPEN            ' /
      DATA GAS_CHEM_SPC(  43 ) / 'MGLY            ' /
      DATA GAS_CHEM_SPC(  44 ) / 'CRO             ' /
      DATA GAS_CHEM_SPC(  45 ) / 'CAT1            ' /
      DATA GAS_CHEM_SPC(  46 ) / 'CRON            ' /
      DATA GAS_CHEM_SPC(  47 ) / 'CRNO            ' /
      DATA GAS_CHEM_SPC(  48 ) / 'CRN2            ' /
      DATA GAS_CHEM_SPC(  49 ) / 'CRPX            ' /
      DATA GAS_CHEM_SPC(  50 ) / 'OPO3            ' /
      DATA GAS_CHEM_SPC(  51 ) / 'CAO2            ' /
      DATA GAS_CHEM_SPC(  52 ) / 'OPAN            ' /
      DATA GAS_CHEM_SPC(  53 ) / 'XYL             ' /
      DATA GAS_CHEM_SPC(  54 ) / 'XYLRO2          ' /
      DATA GAS_CHEM_SPC(  55 ) / 'ISOP            ' /
      DATA GAS_CHEM_SPC(  56 ) / 'ISPD            ' /
      DATA GAS_CHEM_SPC(  57 ) / 'ISOPRXN         ' /
      DATA GAS_CHEM_SPC(  58 ) / 'TERP            ' /
      DATA GAS_CHEM_SPC(  59 ) / 'TRPRXN          ' /
      DATA GAS_CHEM_SPC(  60 ) / 'SO2             ' /
      DATA GAS_CHEM_SPC(  61 ) / 'SULF            ' /
      DATA GAS_CHEM_SPC(  62 ) / 'SULRXN          ' /
      DATA GAS_CHEM_SPC(  63 ) / 'ETOH            ' /
      DATA GAS_CHEM_SPC(  64 ) / 'ETHA            ' /
      DATA GAS_CHEM_SPC(  65 ) / 'CL2             ' /
      DATA GAS_CHEM_SPC(  66 ) / 'CL              ' /
      DATA GAS_CHEM_SPC(  67 ) / 'HOCL            ' /
      DATA GAS_CHEM_SPC(  68 ) / 'CLO             ' /
      DATA GAS_CHEM_SPC(  69 ) / 'FMCL            ' /
      DATA GAS_CHEM_SPC(  70 ) / 'HCL             ' /
      DATA GAS_CHEM_SPC(  71 ) / 'CLNO2           ' /
      DATA GAS_CHEM_SPC(  72 ) / 'TOLNRXN         ' /
      DATA GAS_CHEM_SPC(  73 ) / 'TOLHRXN         ' /
      DATA GAS_CHEM_SPC(  74 ) / 'XYLNRXN         ' /
      DATA GAS_CHEM_SPC(  75 ) / 'XYLHRXN         ' /
      DATA GAS_CHEM_SPC(  76 ) / 'BENZENE         ' /
      DATA GAS_CHEM_SPC(  77 ) / 'BENZRO2         ' /
      DATA GAS_CHEM_SPC(  78 ) / 'BNZNRXN         ' /
      DATA GAS_CHEM_SPC(  79 ) / 'BNZHRXN         ' /
      DATA GAS_CHEM_SPC(  80 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC(  81 ) / 'SESQRXN         ' /
      DATA GAS_CHEM_SPC(  82 ) / 'H2NO3PIJ        ' /
      DATA GAS_CHEM_SPC(  83 ) / 'H2NO3PK         ' /
      DATA GAS_CHEM_SPC(  84 ) / 'FORM_PRIMARY    ' /
      DATA GAS_CHEM_SPC(  85 ) / 'ALD2_PRIMARY    ' /
      DATA GAS_CHEM_SPC(  86 ) / 'BUTADIENE13     ' /
      DATA GAS_CHEM_SPC(  87 ) / 'ACROLEIN        ' /
      DATA GAS_CHEM_SPC(  88 ) / 'ACROLEIN_PRIMARY' /
      DATA GAS_CHEM_SPC(  89 ) / 'TOLU            ' /
      DATA GAS_CHEM_SPC(  90 ) / 'MXYL            ' /
      DATA GAS_CHEM_SPC(  91 ) / 'OXYL            ' /
      DATA GAS_CHEM_SPC(  92 ) / 'PXYL            ' /
      DATA GAS_CHEM_SPC(  93 ) / 'APIN            ' /
      DATA GAS_CHEM_SPC(  94 ) / 'BPIN            ' /
      DATA GAS_CHEM_SPC(  95 ) / 'HG              ' /
      DATA GAS_CHEM_SPC(  96 ) / 'HGIIAER         ' /
      DATA GAS_CHEM_SPC(  97 ) / 'HGIIGAS         ' /




! MAPPED_TO_CGRID declares whether CMAQ namelists were used to determine 
! the below values of CGRID_INDEX, SPECIES_TYPE, SPECIES_MOLWT, and CONVERT_CONC
      LOGICAL, PARAMETER, PRIVATE :: F = .FALSE.
      LOGICAL, PARAMETER, PRIVATE :: T = .TRUE.


      LOGICAL   :: MAPPED_TO_CGRID = .FALSE. 

      DATA CHEMISTRY_SPC(   1 ), CGRID_INDEX(   1 ), SPECIES_TYPE(   1 ), SPECIES_MOLWT(   1 ), CONVERT_CONC(   1 ) / 'NO2             ',    1, 'GC',   46.00, F /
      DATA CHEMISTRY_SPC(   2 ), CGRID_INDEX(   2 ), SPECIES_TYPE(   2 ), SPECIES_MOLWT(   2 ), CONVERT_CONC(   2 ) / 'NO              ',    2, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC(   3 ), CGRID_INDEX(   3 ), SPECIES_TYPE(   3 ), SPECIES_MOLWT(   3 ), CONVERT_CONC(   3 ) / 'O               ',    3, 'GC',   16.00, F /
      DATA CHEMISTRY_SPC(   4 ), CGRID_INDEX(   4 ), SPECIES_TYPE(   4 ), SPECIES_MOLWT(   4 ), CONVERT_CONC(   4 ) / 'O3              ',    4, 'GC',   48.00, F /
      DATA CHEMISTRY_SPC(   5 ), CGRID_INDEX(   5 ), SPECIES_TYPE(   5 ), SPECIES_MOLWT(   5 ), CONVERT_CONC(   5 ) / 'NO3             ',    5, 'GC',   62.00, F /
      DATA CHEMISTRY_SPC(   6 ), CGRID_INDEX(   6 ), SPECIES_TYPE(   6 ), SPECIES_MOLWT(   6 ), CONVERT_CONC(   6 ) / 'O1D             ',    6, 'GC',   16.00, F /
      DATA CHEMISTRY_SPC(   7 ), CGRID_INDEX(   7 ), SPECIES_TYPE(   7 ), SPECIES_MOLWT(   7 ), CONVERT_CONC(   7 ) / 'OH              ',    7, 'GC',   17.00, F /
      DATA CHEMISTRY_SPC(   8 ), CGRID_INDEX(   8 ), SPECIES_TYPE(   8 ), SPECIES_MOLWT(   8 ), CONVERT_CONC(   8 ) / 'HO2             ',    8, 'GC',   33.00, F /
      DATA CHEMISTRY_SPC(   9 ), CGRID_INDEX(   9 ), SPECIES_TYPE(   9 ), SPECIES_MOLWT(   9 ), CONVERT_CONC(   9 ) / 'N2O5            ',    9, 'GC',  108.00, F /
      DATA CHEMISTRY_SPC(  10 ), CGRID_INDEX(  10 ), SPECIES_TYPE(  10 ), SPECIES_MOLWT(  10 ), CONVERT_CONC(  10 ) / 'HNO3            ',   10, 'GC',   63.00, F /
      DATA CHEMISTRY_SPC(  11 ), CGRID_INDEX(  11 ), SPECIES_TYPE(  11 ), SPECIES_MOLWT(  11 ), CONVERT_CONC(  11 ) / 'HONO            ',   11, 'GC',   47.00, F /
      DATA CHEMISTRY_SPC(  12 ), CGRID_INDEX(  12 ), SPECIES_TYPE(  12 ), SPECIES_MOLWT(  12 ), CONVERT_CONC(  12 ) / 'PNA             ',   12, 'GC',   79.00, F /
      DATA CHEMISTRY_SPC(  13 ), CGRID_INDEX(  13 ), SPECIES_TYPE(  13 ), SPECIES_MOLWT(  13 ), CONVERT_CONC(  13 ) / 'H2O2            ',   13, 'GC',   34.00, F /
      DATA CHEMISTRY_SPC(  14 ), CGRID_INDEX(  14 ), SPECIES_TYPE(  14 ), SPECIES_MOLWT(  14 ), CONVERT_CONC(  14 ) / 'XO2             ',   14, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  15 ), CGRID_INDEX(  15 ), SPECIES_TYPE(  15 ), SPECIES_MOLWT(  15 ), CONVERT_CONC(  15 ) / 'XO2N            ',   15, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  16 ), CGRID_INDEX(  16 ), SPECIES_TYPE(  16 ), SPECIES_MOLWT(  16 ), CONVERT_CONC(  16 ) / 'NTR             ',   16, 'GC',  130.00, F /
      DATA CHEMISTRY_SPC(  17 ), CGRID_INDEX(  17 ), SPECIES_TYPE(  17 ), SPECIES_MOLWT(  17 ), CONVERT_CONC(  17 ) / 'ROOH            ',   17, 'GC',   62.00, F /
      DATA CHEMISTRY_SPC(  18 ), CGRID_INDEX(  18 ), SPECIES_TYPE(  18 ), SPECIES_MOLWT(  18 ), CONVERT_CONC(  18 ) / 'FORM            ',   18, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC(  19 ), CGRID_INDEX(  19 ), SPECIES_TYPE(  19 ), SPECIES_MOLWT(  19 ), CONVERT_CONC(  19 ) / 'ALD2            ',   19, 'GC',   44.00, F /
      DATA CHEMISTRY_SPC(  20 ), CGRID_INDEX(  20 ), SPECIES_TYPE(  20 ), SPECIES_MOLWT(  20 ), CONVERT_CONC(  20 ) / 'ALDX            ',   20, 'GC',   44.00, F /
      DATA CHEMISTRY_SPC(  21 ), CGRID_INDEX(  21 ), SPECIES_TYPE(  21 ), SPECIES_MOLWT(  21 ), CONVERT_CONC(  21 ) / 'PAR             ',   21, 'GC',   14.00, F /
      DATA CHEMISTRY_SPC(  22 ), CGRID_INDEX(  22 ), SPECIES_TYPE(  22 ), SPECIES_MOLWT(  22 ), CONVERT_CONC(  22 ) / 'CO              ',   22, 'GC',   28.00, F /
      DATA CHEMISTRY_SPC(  23 ), CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), SPECIES_MOLWT(  23 ), CONVERT_CONC(  23 ) / 'MEO2            ',   23, 'GC',   47.00, F /
      DATA CHEMISTRY_SPC(  24 ), CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), SPECIES_MOLWT(  24 ), CONVERT_CONC(  24 ) / 'MEPX            ',   24, 'GC',   48.00, F /
      DATA CHEMISTRY_SPC(  25 ), CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), SPECIES_MOLWT(  25 ), CONVERT_CONC(  25 ) / 'MEOH            ',   25, 'GC',   32.00, F /
      DATA CHEMISTRY_SPC(  26 ), CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), SPECIES_MOLWT(  26 ), CONVERT_CONC(  26 ) / 'HCO3            ',   26, 'GC',   63.00, F /
      DATA CHEMISTRY_SPC(  27 ), CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), SPECIES_MOLWT(  27 ), CONVERT_CONC(  27 ) / 'FACD            ',   27, 'GC',   46.00, F /
      DATA CHEMISTRY_SPC(  28 ), CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), SPECIES_MOLWT(  28 ), CONVERT_CONC(  28 ) / 'C2O3            ',   28, 'GC',   75.00, F /
      DATA CHEMISTRY_SPC(  29 ), CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), SPECIES_MOLWT(  29 ), CONVERT_CONC(  29 ) / 'PAN             ',   29, 'GC',  121.00, F /
      DATA CHEMISTRY_SPC(  30 ), CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), SPECIES_MOLWT(  30 ), CONVERT_CONC(  30 ) / 'PACD            ',   30, 'GC',   76.00, F /
      DATA CHEMISTRY_SPC(  31 ), CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), SPECIES_MOLWT(  31 ), CONVERT_CONC(  31 ) / 'AACD            ',   31, 'GC',   60.00, F /
      DATA CHEMISTRY_SPC(  32 ), CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), SPECIES_MOLWT(  32 ), CONVERT_CONC(  32 ) / 'CXO3            ',   32, 'GC',   75.00, F /
      DATA CHEMISTRY_SPC(  33 ), CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), SPECIES_MOLWT(  33 ), CONVERT_CONC(  33 ) / 'PANX            ',   33, 'GC',  121.00, F /
      DATA CHEMISTRY_SPC(  34 ), CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), SPECIES_MOLWT(  34 ), CONVERT_CONC(  34 ) / 'ROR             ',   34, 'GC',   31.00, F /
      DATA CHEMISTRY_SPC(  35 ), CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), SPECIES_MOLWT(  35 ), CONVERT_CONC(  35 ) / 'OLE             ',   35, 'GC',   27.00, F /
      DATA CHEMISTRY_SPC(  36 ), CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), SPECIES_MOLWT(  36 ), CONVERT_CONC(  36 ) / 'ETH             ',   36, 'GC',   28.00, F /
      DATA CHEMISTRY_SPC(  37 ), CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), SPECIES_MOLWT(  37 ), CONVERT_CONC(  37 ) / 'IOLE            ',   37, 'GC',   48.00, F /
      DATA CHEMISTRY_SPC(  38 ), CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), SPECIES_MOLWT(  38 ), CONVERT_CONC(  38 ) / 'TOL             ',   38, 'GC',   92.00, F /
      DATA CHEMISTRY_SPC(  39 ), CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), SPECIES_MOLWT(  39 ), CONVERT_CONC(  39 ) / 'CRES            ',   39, 'GC',  108.00, F /
      DATA CHEMISTRY_SPC(  40 ), CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), SPECIES_MOLWT(  40 ), CONVERT_CONC(  40 ) / 'TO2             ',   40, 'GC',  173.00, F /
      DATA CHEMISTRY_SPC(  41 ), CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), SPECIES_MOLWT(  41 ), CONVERT_CONC(  41 ) / 'TOLRO2          ',   41, 'GC',  141.00, F /
      DATA CHEMISTRY_SPC(  42 ), CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), SPECIES_MOLWT(  42 ), CONVERT_CONC(  42 ) / 'OPEN            ',   42, 'GC',   84.00, F /
      DATA CHEMISTRY_SPC(  43 ), CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), SPECIES_MOLWT(  43 ), CONVERT_CONC(  43 ) / 'MGLY            ',   43, 'GC',   72.00, F /
      DATA CHEMISTRY_SPC(  44 ), CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), SPECIES_MOLWT(  44 ), CONVERT_CONC(  44 ) / 'CRO             ',   44, 'GC',  107.00, F /
      DATA CHEMISTRY_SPC(  45 ), CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), SPECIES_MOLWT(  45 ), CONVERT_CONC(  45 ) / 'CAT1            ',   45, 'GC',  124.00, F /
      DATA CHEMISTRY_SPC(  46 ), CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), SPECIES_MOLWT(  46 ), CONVERT_CONC(  46 ) / 'CRON            ',   46, 'GC',  153.00, F /
      DATA CHEMISTRY_SPC(  47 ), CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), SPECIES_MOLWT(  47 ), CONVERT_CONC(  47 ) / 'CRNO            ',   47, 'GC',  152.00, F /
      DATA CHEMISTRY_SPC(  48 ), CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), SPECIES_MOLWT(  48 ), CONVERT_CONC(  48 ) / 'CRN2            ',   48, 'GC',  168.00, F /
      DATA CHEMISTRY_SPC(  49 ), CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), SPECIES_MOLWT(  49 ), CONVERT_CONC(  49 ) / 'CRPX            ',   49, 'GC',  169.00, F /
      DATA CHEMISTRY_SPC(  50 ), CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), SPECIES_MOLWT(  50 ), CONVERT_CONC(  50 ) / 'OPO3            ',   50, 'GC',  115.00, F /
      DATA CHEMISTRY_SPC(  51 ), CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), SPECIES_MOLWT(  51 ), CONVERT_CONC(  51 ) / 'CAO2            ',   51, 'GC',  133.00, F /
      DATA CHEMISTRY_SPC(  52 ), CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), SPECIES_MOLWT(  52 ), CONVERT_CONC(  52 ) / 'OPAN            ',   52, 'GC',  161.00, F /
      DATA CHEMISTRY_SPC(  53 ), CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), SPECIES_MOLWT(  53 ), CONVERT_CONC(  53 ) / 'XYL             ',   53, 'GC',  106.00, F /
      DATA CHEMISTRY_SPC(  54 ), CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), SPECIES_MOLWT(  54 ), CONVERT_CONC(  54 ) / 'XYLRO2          ',   54, 'GC',  155.00, F /
      DATA CHEMISTRY_SPC(  55 ), CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), SPECIES_MOLWT(  55 ), CONVERT_CONC(  55 ) / 'ISOP            ',   55, 'GC',   68.00, F /
      DATA CHEMISTRY_SPC(  56 ), CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), SPECIES_MOLWT(  56 ), CONVERT_CONC(  56 ) / 'ISPD            ',   56, 'GC',   70.00, F /
      DATA CHEMISTRY_SPC(  57 ), CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), SPECIES_MOLWT(  57 ), CONVERT_CONC(  57 ) / 'ISOPRXN         ',   57, 'GC',   68.00, F /
      DATA CHEMISTRY_SPC(  58 ), CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), SPECIES_MOLWT(  58 ), CONVERT_CONC(  58 ) / 'TERP            ',   58, 'GC',  136.00, F /
      DATA CHEMISTRY_SPC(  59 ), CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), SPECIES_MOLWT(  59 ), CONVERT_CONC(  59 ) / 'TRPRXN          ',   59, 'GC',  136.00, F /
      DATA CHEMISTRY_SPC(  60 ), CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), SPECIES_MOLWT(  60 ), CONVERT_CONC(  60 ) / 'SO2             ',   60, 'GC',   64.00, F /
      DATA CHEMISTRY_SPC(  61 ), CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), SPECIES_MOLWT(  61 ), CONVERT_CONC(  61 ) / 'SULF            ',   61, 'GC',   98.00, F /
      DATA CHEMISTRY_SPC(  62 ), CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), SPECIES_MOLWT(  62 ), CONVERT_CONC(  62 ) / 'SULRXN          ',   62, 'GC',   98.00, F /
      DATA CHEMISTRY_SPC(  63 ), CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), SPECIES_MOLWT(  63 ), CONVERT_CONC(  63 ) / 'ETOH            ',   63, 'GC',   46.00, F /
      DATA CHEMISTRY_SPC(  64 ), CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), SPECIES_MOLWT(  64 ), CONVERT_CONC(  64 ) / 'ETHA            ',   64, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC(  65 ), CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), SPECIES_MOLWT(  65 ), CONVERT_CONC(  65 ) / 'CL2             ',   65, 'GC',   71.00, F /
      DATA CHEMISTRY_SPC(  66 ), CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), SPECIES_MOLWT(  66 ), CONVERT_CONC(  66 ) / 'CL              ',   66, 'GC',   35.50, F /
      DATA CHEMISTRY_SPC(  67 ), CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), SPECIES_MOLWT(  67 ), CONVERT_CONC(  67 ) / 'HOCL            ',   67, 'GC',   52.50, F /
      DATA CHEMISTRY_SPC(  68 ), CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), SPECIES_MOLWT(  68 ), CONVERT_CONC(  68 ) / 'CLO             ',   68, 'GC',   51.50, F /
      DATA CHEMISTRY_SPC(  69 ), CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), SPECIES_MOLWT(  69 ), CONVERT_CONC(  69 ) / 'FMCL            ',   69, 'GC',   64.50, F /
      DATA CHEMISTRY_SPC(  70 ), CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), SPECIES_MOLWT(  70 ), CONVERT_CONC(  70 ) / 'HCL             ',   70, 'GC',   36.50, F /
      DATA CHEMISTRY_SPC(  71 ), CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), SPECIES_MOLWT(  71 ), CONVERT_CONC(  71 ) / 'CLNO2           ',   71, 'GC',   81.50, F /
      DATA CHEMISTRY_SPC(  72 ), CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), SPECIES_MOLWT(  72 ), CONVERT_CONC(  72 ) / 'TOLNRXN         ',   72, 'GC',  141.00, F /
      DATA CHEMISTRY_SPC(  73 ), CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), SPECIES_MOLWT(  73 ), CONVERT_CONC(  73 ) / 'TOLHRXN         ',   73, 'GC',  141.00, F /
      DATA CHEMISTRY_SPC(  74 ), CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), SPECIES_MOLWT(  74 ), CONVERT_CONC(  74 ) / 'XYLNRXN         ',   74, 'GC',  155.00, F /
      DATA CHEMISTRY_SPC(  75 ), CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), SPECIES_MOLWT(  75 ), CONVERT_CONC(  75 ) / 'XYLHRXN         ',   75, 'GC',  155.00, F /
      DATA CHEMISTRY_SPC(  76 ), CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), SPECIES_MOLWT(  76 ), CONVERT_CONC(  76 ) / 'BENZENE         ',   76, 'GC',   78.00, F /
      DATA CHEMISTRY_SPC(  77 ), CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), SPECIES_MOLWT(  77 ), CONVERT_CONC(  77 ) / 'BENZRO2         ',   77, 'GC',  127.00, F /
      DATA CHEMISTRY_SPC(  78 ), CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), SPECIES_MOLWT(  78 ), CONVERT_CONC(  78 ) / 'BNZNRXN         ',   78, 'GC',  127.00, F /
      DATA CHEMISTRY_SPC(  79 ), CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), SPECIES_MOLWT(  79 ), CONVERT_CONC(  79 ) / 'BNZHRXN         ',   79, 'GC',  127.00, F /
      DATA CHEMISTRY_SPC(  80 ), CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), SPECIES_MOLWT(  80 ), CONVERT_CONC(  80 ) / 'SESQ            ',   80, 'GC',  204.00, F /
      DATA CHEMISTRY_SPC(  81 ), CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), SPECIES_MOLWT(  81 ), CONVERT_CONC(  81 ) / 'SESQRXN         ',   81, 'GC',  204.00, F /
      DATA CHEMISTRY_SPC(  82 ), CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), SPECIES_MOLWT(  82 ), CONVERT_CONC(  82 ) / 'H2NO3PIJ        ',   82, 'GC',   64.00, F /
      DATA CHEMISTRY_SPC(  83 ), CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), SPECIES_MOLWT(  83 ), CONVERT_CONC(  83 ) / 'H2NO3PK         ',   83, 'GC',   64.00, F /
      DATA CHEMISTRY_SPC(  84 ), CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), SPECIES_MOLWT(  84 ), CONVERT_CONC(  84 ) / 'ACLI            ',  150, 'AE',   35.50, F /
      DATA CHEMISTRY_SPC(  85 ), CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), SPECIES_MOLWT(  85 ), CONVERT_CONC(  85 ) / 'ACLJ            ',  149, 'AE',   35.50, F /
      DATA CHEMISTRY_SPC(  86 ), CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), SPECIES_MOLWT(  86 ), CONVERT_CONC(  86 ) / 'ACLK            ',  152, 'AE',   35.50, F /
      DATA CHEMISTRY_SPC(  87 ), CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), SPECIES_MOLWT(  87 ), CONVERT_CONC(  87 ) / 'AALKJ           ',  105, 'AE',  150.00, F /
      DATA CHEMISTRY_SPC(  88 ), CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), SPECIES_MOLWT(  88 ), CONVERT_CONC(  88 ) / 'AOLGAJ          ',  158, 'AE',  176.40, F /
      DATA CHEMISTRY_SPC(  89 ), CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), SPECIES_MOLWT(  89 ), CONVERT_CONC(  89 ) / 'AXYL1J          ',  106, 'AE',  192.00, F /
      DATA CHEMISTRY_SPC(  90 ), CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), SPECIES_MOLWT(  90 ), CONVERT_CONC(  90 ) / 'AXYL2J          ',  107, 'AE',  192.00, F /
      DATA CHEMISTRY_SPC(  91 ), CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), SPECIES_MOLWT(  91 ), CONVERT_CONC(  91 ) / 'ATOL1J          ',  109, 'AE',  168.00, F /
      DATA CHEMISTRY_SPC(  92 ), CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), SPECIES_MOLWT(  92 ), CONVERT_CONC(  92 ) / 'ATOL2J          ',  110, 'AE',  168.00, F /
      DATA CHEMISTRY_SPC(  93 ), CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), SPECIES_MOLWT(  93 ), CONVERT_CONC(  93 ) / 'ABNZ1J          ',  112, 'AE',  144.00, F /
      DATA CHEMISTRY_SPC(  94 ), CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), SPECIES_MOLWT(  94 ), CONVERT_CONC(  94 ) / 'ABNZ2J          ',  113, 'AE',  144.00, F /
      DATA CHEMISTRY_SPC(  95 ), CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), SPECIES_MOLWT(  95 ), CONVERT_CONC(  95 ) / 'ATRP1J          ',  115, 'AE',  168.00, F /
      DATA CHEMISTRY_SPC(  96 ), CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), SPECIES_MOLWT(  96 ), CONVERT_CONC(  96 ) / 'AOLGBJ          ',  159, 'AE',  252.00, F /
      DATA CHEMISTRY_SPC(  97 ), CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), SPECIES_MOLWT(  97 ), CONVERT_CONC(  97 ) / 'ATRP2J          ',  116, 'AE',  168.00, F /
      DATA CHEMISTRY_SPC(  98 ), CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), SPECIES_MOLWT(  98 ), CONVERT_CONC(  98 ) / 'AISO1J          ',  117, 'AE',   96.00, T /
      DATA CHEMISTRY_SPC(  99 ), CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), SPECIES_MOLWT(  99 ), CONVERT_CONC(  99 ) / 'AISO2J          ',  118, 'AE',   96.00, T /
      DATA CHEMISTRY_SPC( 100 ), CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), SPECIES_MOLWT( 100 ), CONVERT_CONC( 100 ) / 'ASQTJ           ',  119, 'AE',  378.00, T /
      DATA CHEMISTRY_SPC( 101 ), CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), SPECIES_MOLWT( 101 ), CONVERT_CONC( 101 ) / 'APOCI           ',  122, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 102 ), CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), SPECIES_MOLWT( 102 ), CONVERT_CONC( 102 ) / 'APNCOMI         ',  124, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 103 ), CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), SPECIES_MOLWT( 103 ), CONVERT_CONC( 103 ) / 'APOCJ           ',  121, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 104 ), CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), SPECIES_MOLWT( 104 ), CONVERT_CONC( 104 ) / 'APNCOMJ         ',  123, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 105 ), CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), SPECIES_MOLWT( 105 ), CONVERT_CONC( 105 ) / 'FORM_PRIMARY    ',   84, 'GC',   30.00, T /
      DATA CHEMISTRY_SPC( 106 ), CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), SPECIES_MOLWT( 106 ), CONVERT_CONC( 106 ) / 'ALD2_PRIMARY    ',   85, 'GC',   44.00, T /
      DATA CHEMISTRY_SPC( 107 ), CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), SPECIES_MOLWT( 107 ), CONVERT_CONC( 107 ) / 'BUTADIENE13     ',   86, 'GC',   54.00, T /
      DATA CHEMISTRY_SPC( 108 ), CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), SPECIES_MOLWT( 108 ), CONVERT_CONC( 108 ) / 'ACROLEIN        ',   87, 'GC',   56.10, T /
      DATA CHEMISTRY_SPC( 109 ), CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), SPECIES_MOLWT( 109 ), CONVERT_CONC( 109 ) / 'ACROLEIN_PRIMARY',   88, 'GC',   56.10, T /
      DATA CHEMISTRY_SPC( 110 ), CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), SPECIES_MOLWT( 110 ), CONVERT_CONC( 110 ) / 'TOLU            ',   89, 'GC',   92.00, T /
      DATA CHEMISTRY_SPC( 111 ), CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), SPECIES_MOLWT( 111 ), CONVERT_CONC( 111 ) / 'MXYL            ',   90, 'GC',  106.20, T /
      DATA CHEMISTRY_SPC( 112 ), CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), SPECIES_MOLWT( 112 ), CONVERT_CONC( 112 ) / 'OXYL            ',   91, 'GC',  106.20, T /
      DATA CHEMISTRY_SPC( 113 ), CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), SPECIES_MOLWT( 113 ), CONVERT_CONC( 113 ) / 'PXYL            ',   92, 'GC',  106.20, T /
      DATA CHEMISTRY_SPC( 114 ), CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), SPECIES_MOLWT( 114 ), CONVERT_CONC( 114 ) / 'APIN            ',   93, 'GC',  136.30, T /
      DATA CHEMISTRY_SPC( 115 ), CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), SPECIES_MOLWT( 115 ), CONVERT_CONC( 115 ) / 'BPIN            ',   94, 'GC',  136.30, T /
      DATA CHEMISTRY_SPC( 116 ), CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), SPECIES_MOLWT( 116 ), CONVERT_CONC( 116 ) / 'HG              ',   95, 'GC',  200.60, T /
      DATA CHEMISTRY_SPC( 117 ), CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), SPECIES_MOLWT( 117 ), CONVERT_CONC( 117 ) / 'HGIIAER         ',   96, 'GC',  200.60, T /
      DATA CHEMISTRY_SPC( 118 ), CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), SPECIES_MOLWT( 118 ), CONVERT_CONC( 118 ) / 'HGIIGAS         ',   97, 'GC',  200.60, T /

      INTEGER, PARAMETER :: N_ACT_SP = 118

      INTEGER, PARAMETER :: NRXNS = 279

      INTEGER            :: KUNITS

      DATA  KUNITS /   2 /

      INTEGER IRXXN

      REAL( 8 )          :: RTDAT( 3,NRXNS )

      INTEGER, PARAMETER :: NFALLOFF =  20
      REAL( 8 )          :: RFDAT( 5,NFALLOFF )

      INTEGER            :: KTYPE( NRXNS )

      DATA ( KTYPE( IRXXN ), IRXXN = 1, NRXNS ) /  & 
     &      0,    2,    3,    3,   10,   10,    3,    0,    0,    3, & ! O   
     &      1,    3,    3,    0,    0,    3,    3,   10,    1,    1, & ! 1   
     &     10,    3,    1,   10,    0,    3,    1,   10,    8,    3, & ! 2   
     &     10,   10,    3,    9,    9,    0,    3,    1,    3,    3, & ! 3   
     &      3,   10,    3,    3,    3,    1,    1,    1,    1,    3, & ! 4   
     &      0,    0,    0,    3,    3,    3,    3,    1,    1,    1, & ! 5   
     &      3,    0,    3,    0,    9,    3,    3,    3,    3,    3, & ! 6   
     &      0,    3,    1,    0,    0,    3,    1,    3,    3,    1, & ! 7   
     &      3,    1,    3,    3,    3,    0,    3,   10,   10,    0, & ! 8   
     &      3,    3,    3,    3,    3,    0,    3,    3,    3,    1, & ! 9   
     &      0,    3,   10,   10,    0,    1,    3,    3,    3,    3, & ! O   
     &      3,    1,    3,    1,    1,    3,    1,    3,    3,    3, & ! 1   
     &     10,    3,    3,    1,    3,    3,    3,    3,    3,    3, & ! 2   
     &      3,    1,    1,    1,    1,    1,    1,    1,    3,    3, & ! 3   
     &      0,    3,    0,    1,    3,    1,    1,    1,    3,    3, & ! 4   
     &      1,    1,    1,    3,    1,    0,    1,    3,    3,    3, & ! 5   
     &      1,    1,    1,    0,    1,    3,    3,    3,   10,    3, & ! 6   
     &      3,    1,    0,    0,    3,    1,    3,    3,    1,    0, & ! 7   
     &      3,    1,    3,    1,    1,    1,    1,    3,    1,    1, & ! 8   
     &      1,    3,    4,    1,    1,   10,    0,    3,    3,    3, & ! 9   
     &      3,    3,    3,    3,    1,    1,    1,   -1,   -1,   -1, & ! O   
     &     -1,   -1,   -1,   -1,   -1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,   -1,    1, & ! 2   
     &     -1,    1,    1,    3,    0,    0,    3,    3,    3,    3, & ! 3   
     &      0,    1,    3,    3,    1,    1,    1,    1,    3,    0, & ! 4   
     &      1,    1,    1,    3,    0,    1,    3,    1,    3,    1, & ! 5   
     &      1,    1,    1,    1,    1,    3,    3,    3,    1,    1, & ! 6   
     &      1,    3,    1,    1,    3,    1,    1,    1,    3/     !7   

      INTEGER            :: IRXBITS( NRXNS )

      DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,  276,    0,    0,    1,    1,    0,    2,    2,  260, & ! O   
     &      8,    0,    0,    2,    2,    0,    0,    1,    8,    8, & ! 1   
     &      1,   16,    8,    1,    2,    0,    0,    1,    0,    0, & ! 2   
     &      1,    1,    0,    0,    8,    2,    0,  128,  128,    0, & ! 3   
     &      0,    1,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      2,    2,    2,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    2,    0,    2,    0,   64,    0,    0,    0,    0, & ! 6   
     &      2,    0,    0,    2,    2,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    2,    0,    1,    1,    2, & ! 8   
     &      0,    0,    0,    0,    0,    2,    0,    0,    0,    0, & ! 9   
     &      2,    0,    1,    1,    2,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      1,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      2,    0,    2,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    2,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    2,    0,    0,    0,    0,    1,    0, & ! 6   
     &      0,    0,    2,    2,    0,    0,    0,    0,    0,    2, & ! 7   
     &     64,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    1,    2,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    1,    0, & ! 2   
     &      1,    0,    0,    0,    2,    2,    0,    0,    0,    0, & ! 3   
     &      2,    0,    0,    0,    0,    0,    0,    0,    0,    2, & ! 4   
     &      0,    0,    0,    0,    2,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,  260/     !7   

      INTEGER            :: IORDER( NRXNS )

      DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    2,    2,    2,    2,    2,    1,    1,    2, & ! O   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    3, & ! 1   
     &      1,    3,    3,    2,    1,    2,    2,    2,    2,    2, & ! 2   
     &      2,    1,    2,    2,    3,    1,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      1,    1,    1,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    1,    2,    1,    2,    2,    2,    2,    2,    2, & ! 6   
     &      1,    2,    2,    1,    1,    2,    2,    2,    1,    2, & ! 7   
     &      2,    2,    2,    2,    2,    1,    2,    2,    1,    1, & ! 8   
     &      2,    2,    2,    2,    2,    1,    2,    2,    2,    2, & ! 9   
     &      1,    2,    2,    1,    1,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      1,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    1,    2,    2,    1,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    1,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    1, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    1,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    1,    1,    1, & ! O   
     &      1,    2,    2,    2,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    1,    1,    2,    2,    2,    2, & ! 3   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 4   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    3/     !7   

      INTEGER, PARAMETER :: KTN1 = 100
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     11,   19,   20,   23,   27,   38,   46,   47,   48,   49, & ! O   
     &     58,   59,   60,   73,   77,   80,   82,  100,  106,  112, & ! 1   
     &    114,  115,  117,  124,  132,  133,  134,  135,  136,  137, & ! 2   
     &    138,  144,  146,  147,  148,  151,  152,  153,  155,  157, & ! 3   
     &    161,  162,  163,  165,  172,  176,  179,  182,  184,  185, & ! 4   
     &    186,  187,  189,  190,  191,  194,  195,  205,  206,  207, & ! 5   
     &    216,  217,  218,  219,  220,  221,  222,  223,  224,  225, & ! 6   
     &    226,  227,  228,  230,  232,  233,  242,  245,  246,  247, & ! 7   
     &    248,  251,  252,  253,  256,  258,  260,  261,  262,  263, & ! 8   
     &    264,  265,  269,  270,  271,  273,  274,  276,  277,  278/!9   

      INTEGER, PARAMETER :: KTN2 =   1
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &      2/

      INTEGER, PARAMETER :: KTN3 = 114
      INTEGER            :: KRX3( KTN3 )

      DATA ( KRX3( IRXXN ), IRXXN = 1, KTN3 ) / & 
     &      3,    4,    7,   10,   12,   13,   16,   17,   22,   26, & ! O   
     &     30,   33,   37,   39,   40,   41,   43,   44,   45,   50, & ! 1   
     &     54,   55,   56,   57,   61,   63,   66,   67,   68,   69, & ! 2   
     &     70,   72,   76,   78,   79,   81,   83,   84,   85,   87, & ! 3   
     &     91,   92,   93,   94,   95,   97,   98,   99,  102,  107, & ! 4   
     &    108,  109,  110,  111,  113,  116,  118,  119,  120,  122, & ! 5   
     &    123,  125,  126,  127,  128,  129,  130,  131,  139,  140, & ! 6   
     &    142,  145,  149,  150,  154,  158,  159,  160,  166,  167, & ! 7   
     &    168,  170,  171,  175,  177,  178,  181,  183,  188,  192, & ! 8   
     &    198,  199,  200,  201,  202,  203,  204,  234,  237,  238, & ! 9   
     &    239,  240,  243,  244,  249,  254,  257,  259,  266,  267, & ! O   
     &    268,  272,  275,  279/     !  1   

      INTEGER, PARAMETER :: KTN4 =   1
      INTEGER            :: KRX4( KTN4 )

      DATA ( KRX4( IRXXN ), IRXXN = 1, KTN4 ) / & 
     &    193/

      INTEGER, PARAMETER :: KTN5 =   0
      INTEGER            :: KRX5( 1 )

      DATA   KRX5( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN6 =   0
      INTEGER            :: KRX6( 1 )

      DATA   KRX6( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN7 =   0
      INTEGER            :: KRX7( 1 )

      DATA   KRX7( 1 ) / 0 /

      INTEGER, PARAMETER :: NWM =   3
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &      2,   10,  279/
      REAL,    PARAMETER :: ATM_AIR = 1.00000E+06

      INTEGER, PARAMETER :: NWW =   6
      INTEGER            :: NRXWW( NWW )

      DATA ( NRXWW( IRXXN ), IRXXN = 1, NWW ) / & 
     &     11,   19,   20,   20,   23,   35/

      INTEGER, PARAMETER :: NWO2 =   2
      INTEGER            :: NRXWO2( NWO2 )

      DATA ( NRXWO2( IRXXN ), IRXXN = 1, NWO2 ) / & 
     &      2,   22/
      REAL,    PARAMETER :: ATM_O2 = 2.09500E+05

      INTEGER, PARAMETER :: NWN2 =   0
      INTEGER            :: NRXWN2( 1 )

      DATA   NRXWN2( 1 ) / 0 /
      REAL,    PARAMETER :: ATM_N2 = 7.80800E+05

      INTEGER, PARAMETER :: NWCH4 =   2
      INTEGER            :: NRXWCH4( NWCH4 )

      DATA ( NRXWCH4( IRXXN ), IRXXN = 1, NWCH4 ) / & 
     &     66,  181/
      REAL,    PARAMETER :: ATM_CH4 = 1.85000E+00

      INTEGER, PARAMETER :: NWH2 =   2
      INTEGER            :: NRXWH2( NWH2 )

      DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) / & 
     &     38,   39/
      REAL,    PARAMETER :: ATM_H2 = 5.60000E-01

      INTEGER, PARAMETER :: MXPRD =  10
      INTEGER            :: IRR( NRXNS,MXPRD+3 )

      DATA ( IRR( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    4,    3,    3,    3,    1,    4,    4,    6, & ! O   
     &      6,    4,    4,    5,    5,    5,    5,    5,    9,    9, & ! 1   
     &      9,    2,    2,    2,   11,    7,   11,    1,    7,    8, & ! 2   
     &      8,   12,    7,    8,    8,   13,    7,    6,    7,    7, & ! 3   
     &      7,    7,    7,    8,   13,    5,    5,    5,    5,    5, & ! 4   
     &     12,   10,    9,   14,   15,   14,   15,   14,   15,   14, & ! 5   
     &     16,   16,   17,   17,    7,    7,   23,   23,   23,   24, & ! 6   
     &     24,   25,   18,   18,   18,   18,   18,   18,   26,   26, & ! 7   
     &     26,   27,   19,   19,   19,   19,   28,   28,   29,   29, & ! 8   
     &     28,   28,   28,   28,   30,   30,   31,   20,   20,   20, & ! 9   
     &     20,   32,   32,   33,   33,   33,   32,   32,   32,   32, & ! O   
     &     32,   21,   34,   34,   34,    3,    7,    4,    5,    3, & ! 1   
     &      7,    4,    5,   37,   37,   37,   37,   38,   40,   40, & ! 2   
     &      7,   39,   44,   44,   46,   46,   47,   47,   48,   48, & ! 3   
     &     49,   49,   42,   42,   42,   42,   45,   45,   51,   51, & ! 4   
     &     50,   50,   52,    7,    7,   43,    3,    7,    4,    5, & ! 5   
     &      7,    4,    5,   56,   58,   58,   58,   58,   60,    7, & ! 6   
     &      7,    1,   65,   67,   66,   68,   68,   68,    7,   69, & ! 7   
     &     66,   66,   66,   66,   66,   66,   66,   66,   66,   66, & ! 8   
     &     66,   66,   70,   66,   66,   66,   71,   41,   41,   54, & ! 9   
     &     54,   76,   77,   77,   80,   80,   80,    9,    9,   82, & ! O   
     &     83,   82,   82,   83,    1,   87,   89,   90,   91,   92, & ! 1   
     &     93,   94,   95,   97,   98,   99,  100,  101,  102,  103, & ! 2   
     &    104,  105,  105,  105,  105,  105,  105,  106,  106,  106, & ! 3   
     &    106,  106,  107,  107,  107,  107,  109,  109,  109,  109, & ! 4   
     &    109,  108,  108,  108,  108,  108,  110,  110,  111,  111, & ! 5   
     &    112,  112,  113,  113,  114,  114,  114,  114,  114,  115, & ! 6   
     &    115,  115,  115,  115,  116,  116,  116,  116,  116/     !7   

      DATA ( IRR( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    2,    1,    1,    2,    4,    0,    0,    0, & ! O   
     &      0,    7,    8,    0,    0,    2,    1,    1,    0,    0, & ! 1   
     &      0,    2,    1,    7,    0,   11,   11,    7,   10,    2, & ! 2   
     &      1,    0,   12,    8,    8,    0,   13,    0,    0,    3, & ! 3   
     &      7,    7,    8,    3,    3,    3,    7,    8,    4,    5, & ! 4   
     &      0,    0,    0,    2,    2,    8,    8,   14,   15,   15, & ! 5   
     &      7,    0,    7,    0,   22,    0,    2,    8,   23,    7, & ! 6   
     &      0,    7,    7,    0,    0,    3,    5,    8,    0,    2, & ! 7   
     &      8,    7,    3,    7,    5,    0,    2,    1,    0,    0, & ! 8   
     &      8,   23,   14,   28,    7,    0,    7,    3,    7,    5, & ! 9   
     &      0,    2,    1,    0,    0,    7,    8,   23,   14,   32, & ! O   
     &     28,    7,    0,    0,    1,   35,   35,   35,   35,   36, & ! 1   
     &     36,   36,   36,    3,    7,    4,    5,    7,    2,    8, & ! 2   
     &     39,    5,    1,    8,    7,    5,    1,    4,    2,    8, & ! 3   
     &      0,    7,    0,    7,    4,    5,    7,    5,    2,    8, & ! 4   
     &      2,    1,    0,   53,   43,    0,   55,   55,   55,   55, & ! 5   
     &     56,   56,   56,    0,    3,    7,    4,    5,    7,   63, & ! 6   
     &     64,   55,    0,    0,    4,   68,    2,    8,   69,    0, & ! 7   
     &      0,   21,   64,   36,   35,   37,   55,   18,   19,   20, & ! 8   
     &     25,   63,    7,   38,   53,    1,    0,    2,    8,    2, & ! 9   
     &      8,    7,    2,    8,    4,    7,    5,    0,    0,    0, & ! O   
     &      0,   84,   85,   86,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    7,    7,    7, & ! 2   
     &      7,    7,    5,    3,    0,    0,   66,    7,    5,    3, & ! 3   
     &      0,   66,    7,    4,    5,   66,    7,    4,    5,    0, & ! 4   
     &     66,    7,    4,    5,    0,   66,    7,   66,    7,   66, & ! 5   
     &      7,   66,    7,   66,    3,    7,    4,    5,   66,    3, & ! 6   
     &      7,    4,    5,   66,    4,   65,   13,    7,   66/     !7   

      DATA ( IRR( IRXXN,  3 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !7   

      DATA ( IRR( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &      2,    4,    1,    2,    5,    1,    5,    3,    6,    3, & ! O   
     &      7,    8,    7,    1,    2,    1,    2,    9,   10,   10, & ! 1   
     &      5,    1,   11,   11,    2,    1,    2,   10,    5,    7, & ! 2   
     &     12,    8,    1,   13,   13,    7,    8,    7,    8,    8, & ! 3   
     &      3,   13,    0,    7,    7,    1,    8,   10,    1,    1, & ! 4   
     &      8,    7,    1,    1,   16,   17,   17,    0,    0,    0, & ! 5   
     &     10,    1,   14,    7,    8,   23,   18,   24,   18,   23, & ! 6   
     &     18,   18,    8,    8,   22,    7,   10,   26,   18,   27, & ! 7   
     &     24,    8,   28,   28,   28,   23,   23,   29,   28,   28, & ! 8   
     &     30,   23,   23,   23,   28,   23,   23,   32,   32,   32, & ! 9   
     &     23,   19,   33,   32,   32,   19,   30,   19,   19,   19, & ! O   
     &     23,   14,   14,    8,   16,   19,   18,   19,    1,   18, & ! 1   
     &     14,   18,    1,   19,   19,   19,   19,    8,    1,    0, & ! 2   
     &     44,   44,   46,   39,   47,   47,   16,   48,   47,   49, & ! 3   
     &     47,   48,   50,   50,   20,   50,   51,   44,    1,    0, & ! 4   
     &      1,   52,   50,    8,   14,   28,   56,   56,   56,   56, & ! 5   
     &     21,   28,   20,   22,   20,    8,    7,    1,   61,    8, & ! 6   
     &     19,   56,   66,    7,   68,   65,   66,   67,   66,   66, & ! 7   
     &     70,   70,   70,   69,   69,   70,   70,   70,   70,   70, & ! 8   
     &     70,   70,   66,   70,   70,   71,   66,    2,    8,    2, & ! 9   
     &      8,    7,    2,    8,    4,    7,    5,   10,   10,   10, & ! O   
     &     10,   71,   71,   71,   11,   88,   88,   88,   88,   88, & ! 1   
     &     88,   88,   96,   96,   96,   96,   96,  102,    7,  104, & ! 2   
     &      7,    7,    5,    3,    0,    0,   66,    7,    5,    3, & ! 3   
     &      0,   66,    7,    4,    5,   66,    7,    4,    5,    0, & ! 4   
     &     66,    7,    4,    5,    0,   66,    7,   66,    7,   66, & ! 5   
     &      7,   66,    7,   66,    3,    7,    4,    5,   66,    3, & ! 6   
     &      7,    4,    5,   66,  117,  118,  118,  117,  116/     !7   

      DATA ( IRR( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &      3,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    3,    0,    0,    1,    0,    0,    0, & ! 1   
     &      1,    0,    0,    0,    7,    0,    1,    0,    0,    1, & ! 2   
     &      0,    1,    0,    0,    0,    0,    0,    8,    0,    0, & ! 3   
     &      0,    0,    0,    0,    8,    0,    1,    0,    0,    0, & ! 4   
     &      1,    1,    5,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      8,    8,   19,    8,    0,    0,    8,    0,    8,   14, & ! 6   
     &      8,    8,   22,   22,    0,    8,    8,    0,    8,    1, & ! 7   
     &      0,    0,    7,    0,   10,   22,    1,    0,    1,    1, & ! 8   
     &     31,    8,   31,    0,    0,    7,    0,    7,    0,   10, & ! 9   
     &     22,    1,    0,    1,    1,    1,   31,   14,   31,   14, & ! O   
     &     14,   15,   19,    0,    0,   20,   19,   18,   18,    8, & ! 1   
     &     18,   22,   14,   20,   20,   20,   20,   14,    8,    0, & ! 2   
     &     14,   10,    0,    0,    0,   10,    0,    0,    1,    0, & ! 3   
     &      7,    0,    8,   51,   50,   10,    0,   10,    8,    0, & ! 4   
     &     14,    0,    1,   14,   28,    8,   18,   18,   18,   16, & ! 5   
     &     18,   18,   18,   19,   21,   14,    8,    8,    8,   19, & ! 6   
     &     14,   16,    0,   66,    0,   66,    1,    0,   22,   22, & ! 7   
     &     23,   14,   19,   14,   19,   69,   14,    8,   28,   32, & ! 8   
     &      8,    8,    0,   14,   14,    0,    1,   72,   73,   74, & ! 9   
     &     75,   77,   78,   79,   81,   81,   81,   82,   83,    0, & ! O   
     &      0,    0,    0,    0,   10,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,  101,    0,  103, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,  108,  108,  108,  108,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,  118,   65,   13,  118,  118/     !7   

      DATA ( IRR( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      7,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     18,   18,   20,   19,    0,    0,    1,    0,   25,    8, & ! 6   
     &      7,    0,    0,    0,    0,   22,   22,    0,    0,    8, & ! 7   
     &      0,    0,    0,    0,    0,    8,    0,    0,    0,    0, & ! 8   
     &      4,   18,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      8,    8,    0,    0,    0,    0,    4,    8,    0,    8, & ! O   
     &      8,    8,    8,    0,    0,    8,   20,   20,   14,   22, & ! 1   
     &     20,    8,   18,    8,    8,   18,    8,   39,   42,    0, & ! 2   
     &      8,   14,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   22,    0,   18,    0,    0,    0,   18,    0, & ! 4   
     &      8,    0,    0,   39,    0,   22,   14,   14,   14,   14, & ! 5   
     &     14,   43,   21,   18,   59,   15,   14,   14,   62,   20, & ! 6   
     &     15,   14,    0,    0,    0,    0,    0,    0,    0,    8, & ! 7   
     &      0,   15,   14,    8,   20,   19,    8,   22,    0,    0, & ! 8   
     &     18,   19,    0,    8,    8,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    7,    0,    7, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    4,    0,    0,    7,   66/     !7   

      DATA ( IRR( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      5,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     19,   19,    0,   20,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,   31,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,   14,    0,    0,    0,    0,    0,   31,    0,    0, & ! O   
     &     19,   19,   21,    0,    0,   14,   14,   14,   15,   14, & ! 1   
     &      8,    7,    0,   14,   14,   22,    1,   40,   16,    0, & ! 2   
     &     42,    8,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,   14,    0,    0,    0,   22,    0, & ! 4   
     &     20,    0,    0,   43,    0,    0,    8,    8,    8,    8, & ! 5   
     &      8,    8,    8,   21,    0,   18,   15,   15,    0,   18, & ! 6   
     &      8,    8,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    8,   15,   18,   14,   20,   69,    0,    0,    0, & ! 8   
     &      0,    0,    0,   15,   15,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !7   

      DATA ( IRR( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     20,   20,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,   18,    0,    0, & ! O   
     &      0,   21,   15,    0,    0,   22,    8,    7,   20,    7, & ! 1   
     &      0,   27,    0,   22,    0,    3,    0,    7,   43,    0, & ! 2   
     &     45,   20,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,   22,    0,    0,    0,   16,    0, & ! 4   
     &      0,    0,    0,   21,    0,    0,   32,   15,    7,    1, & ! 5   
     &     22,    7,   22,    8,    0,   21,   18,   20,    0,   14, & ! 6   
     &      0,    2,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   19,    8,    0,    8,   35,   56,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !7   

      DATA ( IRR( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     21,   21,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,   34,   34,    0,    0,   18,   21,   22,   19,    0, & ! 1   
     &      0,    0,    0,   21,    0,    7,    0,   41,   18,    0, & ! 2   
     &     22,   18,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    7,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,   40,    0,    0,   21,   57,   32,   20, & ! 5   
     &     43,   14,   16,   14,    0,   20,   22,   16,    0,    0, & ! 6   
     &      0,   20,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   21,    0,    0,   21,   21,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !7   

      DATA ( IRR( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,   20,   20,    0,    0,   15,    0,    8,   21,    0, & ! 1   
     &      0,    0,    0,    0,    0,    8,    0,    0,   22,    0, & ! 2   
     &     15,   43,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    8,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,   54,    0,    0,    0,    0,   20,   21, & ! 5   
     &     19,   19,   32,   28,    0,   59,   21,   59,    0,    0, & ! 6   
     &      0,   21,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   34,    0,    0,    0,   14,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !7   

      DATA ( IRR( IRXXN, 11 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,   21,    0,   21,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &     18,   42,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,   43,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,   21,    0, & ! 5   
     &     28,   21,   14,    0,    0,    0,   20,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   20,    0,    0,    0,    8,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !7   

      DATA ( IRR( IRXXN, 12 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    7,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,   15,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,   22,    0, & ! 5   
     &     32,   22,   10,    0,    0,    0,   32,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !7   

      DATA ( IRR( IRXXN, 13 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,   22,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     20,    0,    0,    0,    0,    0,   59,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0/     !7   

      DATA ( RTDAT( 1,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 6.0000D-34, 3.0000D-12, 5.6000D-12, 2.5000D-31, & ! O   
     &     9.0000D-32, 1.2000D-13, 1.0000D+00, 1.0000D+00, 2.1000D-11, & ! +   
     &     2.2000D-10, 1.7000D-12, 1.0000D-14, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.5000D-11, 4.5000D-14, 2.0000D-30, 1.0000D-22, 0.0000D+00, & ! +   
     &     1.0000D-03, 3.3000D-39, 5.0000D-40, 7.0000D-31, 1.0000D+00, & ! 2   
     &     1.8000D-11, 1.0000D-20, 2.0000D-30, 2.4000D-14, 3.5000D-12, & ! +   
     &     1.8000D-31, 4.1000D-05, 1.3000D-12, 2.3000D-13, 3.2200D-34, & ! 3   
     &     1.0000D+00, 2.9000D-12, 1.1000D-10, 5.5000D-12, 2.2000D-11, & ! +   
     &     4.2000D-12, 6.9000D-31, 4.8000D-11, 3.0000D-11, 1.4000D-12, & ! 4   
     &     1.0000D-11, 2.2000D-11, 3.5000D-12, 1.0000D-17, 8.5000D-13, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.6000D-12, 2.6000D-12, & ! 5   
     &     7.5000D-13, 7.5000D-13, 6.8000D-14, 6.8000D-14, 6.8000D-14, & ! +   
     &     5.9000D-13, 1.0000D+00, 3.0100D-12, 1.0000D+00, 1.4400D-13, & ! 6   
     &     2.4500D-12, 2.8000D-12, 4.1000D-13, 9.5000D-14, 3.8000D-12, & ! +   
     &     1.0000D+00, 7.3000D-12, 9.0000D-12, 1.0000D+00, 1.0000D+00, & ! 7   
     &     3.4000D-11, 5.8000D-16, 9.7000D-15, 2.4000D+12, 5.6000D-12, & ! +   
     &     5.6000D-15, 4.0000D-13, 1.8000D-11, 5.6000D-12, 1.4000D-12, & ! 8   
     &     1.0000D+00, 8.1000D-12, 2.7000D-28, 4.9000D-03, 1.0000D+00, & ! +   
     &     4.3000D-13, 2.0000D-12, 4.4000D-13, 2.9000D-12, 4.0000D-13, & ! 9   
     &     1.0000D+00, 4.0000D-13, 1.3000D-11, 5.1000D-12, 6.5000D-15, & ! +   
     &     1.0000D+00, 6.7000D-12, 2.7000D-28, 4.9000D-03, 1.0000D+00, & ! O   
     &     3.0000D-13, 4.3000D-13, 2.0000D-12, 4.4000D-13, 2.9000D-12, & ! +   
     &     2.9000D-12, 8.1000D-13, 1.0000D+15, 1.6000D+03, 1.5000D-11, & ! 1   
     &     1.0000D-11, 3.2000D-11, 6.5000D-15, 7.0000D-13, 1.0400D-11, & ! +   
     &     1.0000D-28, 1.2000D-14, 3.3000D-12, 2.3000D-11, 1.0000D-11, & ! 2   
     &     8.4000D-15, 9.6000D-13, 1.8000D-12, 2.7000D-12, 1.9000D-13, & ! +   
     &     1.7000D-12, 1.4000D-11, 2.1000D-12, 5.5000D-12, 1.5300D-12, & ! 3   
     &     3.8000D-12, 2.1000D-12, 2.8600D-13, 2.5400D-12, 2.4000D-13, & ! +   
     &     1.0000D-02, 1.9000D-12, 4.0000D-02, 4.4000D-11, 5.4000D-17, & ! 4   
     &     3.8000D-12, 7.0000D-11, 1.7000D-10, 2.5400D-12, 2.4000D-13, & ! +   
     &     1.1000D-11, 1.1000D-11, 1.0000D-04, 1.7000D-11, 1.8000D-11, & ! 5   
     &     1.0000D+00, 3.6000D-11, 2.5400D-11, 7.8600D-15, 3.0300D-12, & ! +   
     &     3.3600D-11, 7.1000D-18, 1.0000D-15, 3.6000D-03, 3.6000D-11, & ! 6   
     &     1.5000D-11, 1.2000D-15, 3.7000D-12, 3.3000D-31, 6.9000D-12, & ! +   
     &     8.7000D-12, 1.5000D-19, 1.0000D+00, 1.0000D+00, 2.3000D-11, & ! 7   
     &     1.6300D-14, 6.4000D-12, 2.7000D-12, 5.0000D-13, 1.0000D+00, & ! +   
     &     6.6000D-12, 5.0000D-11, 8.3000D-11, 1.0700D-10, 2.5000D-10, & ! 8   
     &     3.5000D-10, 4.3000D-10, 8.2000D-11, 7.9000D-11, 1.3000D-10, & ! +   
     &     5.5000D-11, 8.2000D-11, 6.5800D-13, 6.1000D-11, 1.2000D-10, & ! 9   
     &     1.8000D-31, 1.0000D+00, 2.7000D-12, 1.9000D-13, 2.7000D-12, & ! +   
     &     1.9000D-13, 2.4700D-12, 2.7000D-12, 1.9000D-13, 1.1600D-14, & ! O   
     &     1.9700D-10, 1.9000D-11, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 2   
     &     9.4882D-06, 9.4882D-06, 2.5000D-12, 1.0000D+00, 2.5000D-12, & ! +   
     &     1.0000D+00, 9.0000D-12, 5.8000D-16, 3.4000D-11, 1.0000D+00, & ! 3   
     &     1.0000D+00, 8.2000D-11, 5.6000D-12, 1.4000D-12, 1.8000D-11, & ! +   
     &     1.0000D+00, 7.9000D-11, 1.4000D-11, 8.2000D-15, 1.7900D-13, & ! 4   
     &     2.5100D-10, 2.0000D-11, 2.6100D-19, 1.7000D-11, 1.0000D+00, & ! +   
     &     2.3700D-10, 2.0000D-11, 2.6100D-19, 1.7000D-11, 1.0000D+00, & ! 5   
     &     2.3700D-10, 1.8000D-12, 6.1000D-11, 1.7000D-11, 1.4000D-10, & ! +   
     &     1.2200D-11, 1.5000D-10, 1.3000D-11, 1.5000D-10, 2.7900D-11, & ! 6   
     &     1.2000D-11, 6.3000D-16, 1.2000D-12, 4.7000D-10, 2.8100D-11, & ! +   
     &     7.5100D-11, 1.7400D-15, 2.8100D-11, 5.3000D-10, 2.1100D-18, & ! 7   
     &     2.6000D-18, 8.5000D-19, 7.7000D-14, 2.2500D-33/           !+   

      DATA ( RTDAT( 2,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00,-2.4000D+00, 0.0000D+00, 0.0000D+00,-1.8000D+00, & ! O   
     &    -1.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00,-4.4000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -3.5000D+00, 0.0000D+00, 0.0000D+00,-2.6000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00,-3.0000D+00, 4.6000D+02, 0.0000D+00, & ! +   
     &    -3.2000D+00, 0.0000D+00, 0.0000D+00, 6.0000D+02, 2.8000D+03, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00,-7.1000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-7.1000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -8.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-4.3000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.1600D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &    -2.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !+   

      DATA ( RTDAT( 3,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00,-1.5000D+03, 1.8000D+02, 0.0000D+00, & ! O   
     &     0.0000D+00,-2.4500D+03, 0.0000D+00, 0.0000D+00, 1.0200D+02, & ! +   
     &     0.0000D+00,-9.4000D+02,-4.9000D+02, 0.0000D+00, 0.0000D+00, & ! 1   
     &     1.7000D+02,-1.2600D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -1.1000D+04, 5.3000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &    -3.9000D+02, 0.0000D+00, 0.0000D+00, 2.7000D-17, 2.5000D+02, & ! +   
     &     0.0000D+00,-1.0650D+04, 3.8000D+02, 1.7000D-33, 2.3800D-54, & ! 3   
     &     0.0000D+00,-1.6000D+02, 0.0000D+00,-2.0000D+03, 1.2000D+02, & ! +   
     &    -2.4000D+02, 0.0000D+00, 2.5000D+02, 2.0000D+02,-2.0000D+03, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-2.4500D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.6500D+02, 3.6500D+02, & ! 5   
     &     7.0000D+02, 7.0000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -3.6000D+02, 0.0000D+00, 1.9000D+02, 0.0000D+00, 3.4300D-33, & ! 6   
     &    -1.7750D+03, 3.0000D+02, 7.5000D+02, 3.9000D+02, 2.0000D+02, & ! +   
     &     0.0000D+00,-6.2000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &    -1.6000D+03, 0.0000D+00, 6.2500D+02,-7.0000D+03, 0.0000D+00, & ! +   
     &     2.3000D+03, 0.0000D+00,-1.1000D+03, 2.7000D+02,-1.9000D+03, & ! 8   
     &     0.0000D+00, 2.7000D+02, 0.0000D+00,-1.2100D+04, 0.0000D+00, & ! +   
     &     1.0400D+03, 5.0000D+02, 1.0700D+03, 5.0000D+02, 2.0000D+02, & ! 9   
     &     0.0000D+00, 2.0000D+02,-8.7000D+02, 4.0500D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.4000D+02, 0.0000D+00,-1.2100D+04, 0.0000D+00, & ! O   
     &     0.0000D+00, 1.0400D+03, 5.0000D+02, 1.0700D+03, 5.0000D+02, & ! +   
     &     5.0000D+02, 0.0000D+00,-8.0000D+03, 0.0000D+00, 0.0000D+00, & ! 1   
     &    -2.8000D+02, 0.0000D+00,-1.9000D+03,-2.1600D+03,-7.9200D+02, & ! +   
     &     0.0000D+00,-2.6300D+03,-2.8800D+03, 0.0000D+00, 5.5000D+02, & ! 2   
     &    -1.1000D+03,-2.7000D+02, 3.5500D+02, 3.6000D+02, 1.3000D+03, & ! +   
     &     9.5000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.6000D+02, 1.3000D+03, & ! +   
     &     0.0000D+00, 1.9000D+02, 0.0000D+00, 0.0000D+00,-5.0000D+02, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.6000D+02, 1.3000D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.1600D+02, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 4.0760D+02,-1.9120D+03,-4.4800D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     4.4900D+02,-8.2100D+02, 1.7500D+02, 0.0000D+00,-2.3000D+02, & ! +   
     &    -1.0700D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00,-2.0000D+02, & ! 7   
     &     0.0000D+00, 2.9000D+02, 2.2000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &    -1.2400D+03, 0.0000D+00,-1.0000D+02, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00,-3.4000D+01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 4.5000D+01, 5.8000D+01, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 3.6000D+02, 1.3000D+03, 3.6000D+02, & ! +   
     &     1.3000D+03,-2.0600D+02, 3.6000D+02, 1.3000D+03, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.6000D+03, 0.0000D+00, & ! 3   
     &     0.0000D+00,-3.4000D+01, 2.7000D+02,-1.9000D+03,-1.1000D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.2400D+02,-2.0700D+03, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-3.1310D+03, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-3.1310D+03, 0.0000D+00, & ! 5   
     &     0.0000D+00, 3.5500D+02, 0.0000D+00, 1.1600D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     4.4000D+02,-5.8000D+02, 4.9000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-1.2600D+03, 0.0000D+00, 0.0000D+00,-1.2565D+03, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-6.8000D+02/           !+   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &      5,    6,   18,   21,   24,   28,   29,   31,   32,   34, & 
     &     35,   42,   65,   88,   89,  103,  104,  121,  169,  196/

      DATA ( RFDAT( 1,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     2.2000D-11, 3.0000D-11, 1.4000D-12, 9.7000D+14, 3.6000D-11, & 
     &     2.5000D-11, 2.1990D+03, 4.7000D-12, 4.8000D+15, 1.0000D+03, & 
     &     3.2000D+03, 2.6000D-11, 0.0000D+00, 1.2000D-11, 5.4000D+16, & 
     &     1.2000D-11, 5.4000D+16, 8.8000D-12, 1.6000D-12, 1.0000D-10/

      DATA ( RFDAT( 2,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &    -7.0000D-01, 0.0000D+00,-7.0000D-01, 1.0000D-01,-1.0000D-01, & 
     &     0.0000D+00, 6.5000D-34, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-9.0000D-01, 0.0000D+00, & 
     &    -9.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.0000D+00/

      DATA ( RFDAT( 3,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.1080D+04, 0.0000D+00, & 
     &     0.0000D+00, 1.3350D+03, 0.0000D+00,-1.1170D+04, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.3830D+04, & 
     &     0.0000D+00,-1.3830D+04, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( RFDAT( 4,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     6.0000D-01, 6.0000D-01, 6.0000D-01, 4.5000D-01, 6.0000D-01, & 
     &     6.0000D-01, 0.0000D+00, 6.0000D-01, 6.0000D-01, 0.0000D+00, & 
     &     0.0000D+00, 6.0000D-01, 0.0000D+00, 3.0000D-01, 3.0000D-01, & 
     &     3.0000D-01, 3.0000D-01, 6.0000D-01, 6.0000D-01, 6.0000D-01/

      DATA ( RFDAT( 5,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00/

      REAL               :: SC( NRXNS,MXPRD )

      DATA ( SC( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        2.00000,    1.00000,    1.00000,    2.00000,    2.00000, & ! +   
     &        1.00000,    2.00000,    2.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        2.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    2.00000, & ! +   
     &        0.61000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.37000,    0.70000, & ! +   
     &        1.00000,    1.00000,    1.00000,    2.00000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.80000,    0.90000,    0.90000,    2.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    0.80000,    0.90000,    0.90000,    2.00000, & ! +   
     &        1.00000,    0.87000,    0.96000,    1.00000,    1.00000, & ! 1   
     &        0.20000,    0.80000,    0.18000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.24000,    1.30000, & ! 2   
     &        0.65000,    1.18000,    0.28000,    0.86000,    0.00000, & ! +   
     &        0.06000,    0.30000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    2.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.60000,    0.03000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    0.86000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.70000,    1.00000, & ! 5   
     &        1.00000,    0.75000,    0.91200,    0.65000,    0.20000, & ! +   
     &        1.56500,    0.11400,    0.35700,    0.33300,    0.15000, & ! 6   
     &        0.75000,    0.57000,    0.47000,    1.00000,    1.00000, & ! +   
     &        0.99100,    0.20000,    2.00000,    1.00000,    1.00000, & ! 7   
     &        0.30000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        0.30000,    0.15000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.50000, & ! 1   
     &        1.14280,    1.14280,    1.14280,    1.00000,    1.00000, & ! +   
     &        0.85714,    0.85714,    1.00000,    1.00000,    0.50000, & ! 2   
     &        0.50000,    1.50000,    1.25000,    1.00000,    1.25000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 3   
     &        0.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.50000, & ! 7   
     &        1.00000,    1.00000,    0.50000,    0.50000/           ! &  

      DATA ( SC( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 1   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 2   
     &        0.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 4   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.61000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.50000,    1.00000,    0.00000, & ! 6   
     &        0.00000,    1.00000,    0.00000,    0.74000,    0.30000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 7   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.20000,    0.90000,    0.10000,    0.00000,    0.00000, & ! 9   
     &        1.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    0.20000,    0.90000,    0.10000,    2.00000, & ! +   
     &        1.00000,    0.13000,    0.60000,    0.00000,    0.00000, & ! 1   
     &        0.30000,    0.33000,    0.74000,    1.00000,    1.70000, & ! +   
     &        1.56000,    0.63000,    1.00000,    0.66000,    0.70000, & ! 2   
     &        0.35000,    0.64000,    0.10000,    1.20000,    0.00000, & ! +   
     &        0.12000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        1.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.40000,    0.62000, & ! 4   
     &        1.00000,    0.00000,    1.00000,    1.20000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    1.00000, & ! 5   
     &        1.00000,    0.50000,    0.62900,    0.60000,    0.80000, & ! +   
     &        0.16700,    0.15000,    0.28200,    0.06700,    5.12000, & ! 6   
     &        1.25000,    0.07000,    0.28000,    1.00000,    0.90000, & ! +   
     &        0.99100,    0.80000,    0.00000,    1.00000,    0.00000, & ! 7   
     &        1.40000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.87000,    0.99100,    2.00000,    0.33000, & ! 8   
     &        0.70000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.88000,    0.84000, & ! 9   
     &        0.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.50000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.58000,    0.52000,    0.04500, & ! 4   
     &        0.58000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.50000, & ! 7   
     &        1.00000,    1.00000,    0.50000,    0.50000/           ! &  

      DATA ( SC( IRXXN,  3 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.39000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.33000,    0.33000,    0.50000,    0.50000,    0.00000, & ! 6   
     &        0.00000,    1.00000,    0.00000,    0.63000,    0.30000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        1.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.20000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.20000,    1.00000,    0.00000,    2.00000, & ! +   
     &        1.00000,    0.11000,    0.94000,    0.00000,    0.00000, & ! 1   
     &        0.30000,    0.62000,    0.32000,    0.91000,    1.00000, & ! +   
     &        0.22000,    0.13000,    2.00000,    0.10000,    1.00000, & ! 2   
     &        0.25000,    1.00000,    0.18000,    0.86000,    0.00000, & ! +   
     &        1.12000,    0.60000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.70000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.34400,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.20000,    0.00000, & ! 5   
     &        1.00000,    0.25000,    0.99100,    0.20000,    1.00000, & ! +   
     &        0.71300,    0.85000,    1.28200,    0.90000,    1.00000, & ! 6   
     &        0.25000,    0.76000,    1.03000,    1.00000,    0.05000, & ! +   
     &        0.00900,    1.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.13000,    0.99100,    1.00000,    0.67000, & ! 8   
     &        0.45000,    1.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.88000,    0.84000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 7   
     &        0.00000,    0.00000,    1.00000,    1.00000/           ! &  

      DATA ( SC( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.39000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.33000,    0.33000,    0.00000,    0.50000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.10000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.10000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.06000,   -2.10000,    0.00000,    0.00000, & ! 1   
     &        0.20000,    0.80000,    0.22000,    0.09000,    0.70000, & ! +   
     &        1.00000,    0.13000,    0.00000,    0.10000,    1.00000, & ! 2   
     &        0.25000,    1.00000,    0.65000,    0.14000,    0.00000, & ! +   
     &        0.13000,    0.36000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.03000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.34400,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.80000,    0.00000, & ! 5   
     &        0.00000,    0.25000,    0.91200,    0.06600,    0.80000, & ! +   
     &        0.50300,    0.15400,    0.92500,    0.83200,    0.00000, & ! 6   
     &        0.28000,    0.18000,    0.25000,    0.00000,    0.10000, & ! +   
     &        1.00000,    0.80000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.11000,    0.00900,    1.00000,    2.00000, & ! 8   
     &        0.55000,    0.85000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.12000,    0.16000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    1.00000/           ! &  

      DATA ( SC( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.33000,    0.33000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.10000,    0.00000,    0.00000, & ! +   
     &        0.00000,   -0.11000,    0.04000,    0.00000,    0.00000, & ! 1   
     &        0.20000,    0.95000,    0.10000,    0.56000,    0.30000, & ! +   
     &        0.00000,    0.37000,    0.00000,    0.10000,    0.00000, & ! 2   
     &        0.50000,    0.00000,    0.07200,    0.52000,    0.00000, & ! +   
     &        0.73200,    0.48000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.69000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.14000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.10000,    0.00000, & ! 5   
     &        0.00000,    0.25000,    0.08800,    0.26600,    0.20000, & ! +   
     &        0.33400,    0.26800,    0.64300,    1.03300,    0.00000, & ! 6   
     &        1.66000,    0.24000,    0.47000,    0.00000,    0.10000, & ! +   
     &        0.00000,    0.20000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.06000,    1.00000,    0.00000,    1.00000, & ! 8   
     &        0.30000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      DATA ( SC( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &       -0.66000,   -0.66000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.76000,    0.02000,    0.00000,    0.00000, & ! 1   
     &        0.20000,   -0.70000,    0.33000,    0.35000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.10000,    0.00000, & ! 2   
     &        0.50000,    0.00000,    1.00000,    0.33600,    0.00000, & ! +   
     &        0.06000,    0.24000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.08000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.30000,    0.00000, & ! 5   
     &        0.00000,    0.25000,    1.00000,    0.20000,    0.80000, & ! +   
     &        0.16800,    0.06400,    0.85000,    0.70000,    0.00000, & ! 6   
     &        0.47000,    0.00100,    0.53000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.80000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,   -0.11000,    0.00000,    0.00000,   -1.00000, & ! 8   
     &        0.30000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      DATA ( SC( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.05000,    0.50000,    0.00000,    0.00000, & ! 1   
     &        0.01000,    0.00000,    0.44000,   -1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.50000,    0.00000,    0.00000,    0.33600,    0.00000, & ! +   
     &        0.06000,    0.24000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.76000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.15000,    2.40000, & ! +   
     &        0.25200,    0.02000,    0.07500,    0.96700,    0.00000, & ! 6   
     &        1.00000,    7.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    2.40000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.76000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        1.70000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      DATA ( SC( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.20000,    0.00000,   -1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.06000,    0.12000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.20000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.35000,    0.00000, & ! +   
     &        0.21000,    0.36000,    0.07500,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.21000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.05000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      DATA ( SC( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.10000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.10000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.06600,    0.00000, & ! +   
     &        0.25000,    0.22500,    0.15000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.39000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      DATA ( SC( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.24000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.12000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000/           ! &  

      INTEGER            :: NREACT( NRXNS )

      DATA ( NREACT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    2,    2,    2,    2,    2,    1,    1,    1, & ! O   
     &      1,    2,    2,    1,    1,    2,    2,    2,    1,    1, & ! 1   
     &      1,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 2   
     &      2,    1,    2,    2,    2,    1,    2,    1,    1,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      1,    1,    1,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    1,    2,    1,    2,    1,    2,    2,    2,    2, & ! 6   
     &      1,    2,    2,    1,    1,    2,    2,    2,    1,    2, & ! 7   
     &      2,    2,    2,    2,    2,    1,    2,    2,    1,    1, & ! 8   
     &      2,    2,    2,    2,    2,    1,    2,    2,    2,    2, & ! 9   
     &      1,    2,    2,    1,    1,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      1,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    1,    2,    2,    1,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    1,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    1, & ! 7   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    1,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    1,    1,    1, & ! O   
     &      1,    2,    2,    2,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    1,    1,    2,    2,    2,    2, & ! 3   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 4   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2/     !7   
      INTEGER            :: NPRDCT( NRXNS )

      DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    2,    1,    1,    2,    1,    1,    1, & ! 1   
     &      2,    1,    1,    1,    2,    1,    2,    1,    1,    2, & ! 2   
     &      1,    2,    1,    1,    1,    1,    1,    2,    1,    1, & ! 3   
     &      1,    1,    0,    1,    2,    1,    2,    1,    1,    1, & ! 4   
     &      4,    2,    2,    1,    1,    1,    1,    0,    0,    0, & ! 5   
     &      6,    6,    3,    4,    1,    1,    3,    1,    3,    3, & ! 6   
     &      3,    2,    2,    2,    1,    3,    3,    1,    2,    3, & ! 7   
     &      1,    1,    2,    1,    2,    3,    2,    1,    2,    2, & ! 8   
     &      3,    4,    2,    1,    1,    2,    1,    2,    1,    2, & ! 9   
     &      3,    4,    1,    2,    2,    2,    3,    5,    2,    3, & ! O   
     &      4,    7,    7,    1,    1,    9,    6,    8,    7,    5, & ! 1   
     &      4,    5,    3,    6,    4,    7,    4,    6,    7,    0, & ! 2   
     &      8,   10,    1,    1,    1,    2,    1,    1,    2,    1, & ! 3   
     &      2,    1,    3,    2,    8,    2,    1,    2,    5,    0, & ! 4   
     &      4,    1,    2,    7,    2,    3,    6,    6,    9,    7, & ! 5   
     &     10,    9,    9,    7,    3,    7,   10,    7,    3,    5, & ! 6   
     &      4,    7,    1,    2,    1,    2,    2,    1,    2,    3, & ! 7   
     &      2,    8,    5,    4,    6,    8,    5,    3,    2,    2, & ! 8   
     &      3,    3,    1,    4,    4,    1,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! O   
     &      1,    1,    1,    1,    2,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    3,    1,    3, & ! 2   
     &      1,    1,    1,    1,    0,    0,    1,    1,    1,    1, & ! 3   
     &      0,    1,    2,    2,    2,    2,    1,    1,    1,    0, & ! 4   
     &      1,    1,    1,    1,    0,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    3,    2,    2,    3,    3/     !7   

      INTEGER, PARAMETER :: NMPHOT =  33
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    8,    9,   14,   15,   25,   36,   51,   52,   53, & 
     &     62,   64,   71,   74,   75,   86,   90,   96,  101,  105, & 
     &    141,  143,  156,  164,  173,  174,  180,  197,  235,  236, & 
     &    241,  250,  255/

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   12,   13,   14,   15,   16,   17,   18,   16, & 
     &      1,    1,   19,   20,   21,   22,   23,   24,   13,   14, & 
     &     15,   20,   20/

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & 
     &     31,   32,   33/

      INTEGER, PARAMETER :: MHETERO =  10
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    208,  209,  210,  211,  212,  213,  214,  215,  229,  231/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    2,    3,    4,    5,    5,    6,    7,    8,    9/

      INTEGER, PARAMETER :: NPHOTAB =  24
      CHARACTER( 16 )    :: PHOTAB( NPHOTAB )

      DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & 
     &   'NO2_SAPRC99     ', 'O3_O3P_IUPAC04  ', 'O3_O1D_IUPAC04  ', & 
     &   'NO3NO2_SAPRC99  ', 'NO3NO_SAPRC99   ', 'HONO_IUPAC04    ', & 
     &   'H2O2_SAPRC99    ', 'HO2NO2_IUPAC04  ', 'HNO3_IUPAC04    ', & 
     &   'N2O5_IUPAC04    ', 'NTR_IUPAC04     ', 'COOH_SAPRC99    ', & 
     &   'HCHO_R_SAPRC99  ', 'HCHO_M_SAPRC99  ', 'CCHO_R_SAPRC99  ', & 
     &   'PAN_IUPAC04     ', 'PACD_CB05       ', 'C2CHO_SAPRC99   ', & 
     &   'MGLY_IUPAC04    ', 'ACROLEIN_SAPRC99', 'CL2_IUPAC04     ', & 
     &   'HOCL_IUPAC04    ', 'FMCL_IUPAC04    ', 'CLNO2           '/

      INTEGER, PARAMETER :: NHETERO =   9
      CHARACTER( 16 )    :: HETERO( NHETERO )

      DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & 
     &   'HETERO_N2O5IJ   ', 'HETERO_N2O5K    ', 'HETERO_H2NO3PAIJ', &
     &   'HETERO_H2NO3PAK ', 'HETERO_H2NO3PBIJ', 'HETERO_H2NO3PBK ', &
     &   'HETERO_NO2      ', 'HETERO_PNCOMLI  ', 'HETERO_PNCOMLJ  '/

      CHARACTER( 16 )    :: RXLABEL( NRXNS )

      DATA ( RXLABEL( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &    'R1              ', 'R2              ', 'R3              ', & ! 0   
     &    'R4              ', 'R5              ', 'R6              ', & ! 1   
     &    'R7              ', 'R8              ', 'R9              ', & ! 2   
     &    'R10             ', 'R11             ', 'R12             ', & ! 3   
     &    'R13             ', 'R14             ', 'R15             ', & ! 4   
     &    'R16             ', 'R17             ', 'R18             ', & ! 5   
     &    'R19             ', 'R20             ', 'R21             ', & ! 6   
     &    'R22             ', 'R23             ', 'R24             ', & ! 7   
     &    'R25             ', 'R26             ', 'R27             ', & ! 8   
     &    'R28             ', 'R29             ', 'R30             ', & ! 9   
     &    'R31             ', 'R32             ', 'R33             ', & ! 0   
     &    'R34             ', 'R35             ', 'R36             ', & ! 1   
     &    'R37             ', 'R38             ', 'R39             ', & ! 2   
     &    'R40             ', 'R41             ', 'R42             ', & ! 3   
     &    'R43             ', 'R44             ', 'R45             ', & ! 4   
     &    'R46             ', 'R47             ', 'R48             ', & ! 5   
     &    'R49             ', 'R50             ', 'R51             ', & ! 6   
     &    'R52             ', 'R53             ', 'R54             ', & ! 7   
     &    'R55             ', 'R56             ', 'R57             ', & ! 8   
     &    'R58             ', 'R59             ', 'R60             ', & ! 9   
     &    'R61             ', 'R62             ', 'R63             ', & ! 0   
     &    'R64             ', 'R65             ', 'R66             ', & ! 1   
     &    'R67             ', 'R68             ', 'R69             ', & ! 2   
     &    'R70             ', 'R71             ', 'R72             ', & ! 3   
     &    'R73             ', 'R74             ', 'R75             ', & ! 4   
     &    'R76             ', 'R77             ', 'R78             ', & ! 5   
     &    'R79             ', 'R80             ', 'R81             ', & ! 6   
     &    'R82             ', 'R83             ', 'R84             ', & ! 7   
     &    'R85             ', 'R86             ', 'R87             ', & ! 8   
     &    'R88             ', 'R89             ', 'R90             ', & ! 9   
     &    'R91             ', 'R92             ', 'R93             ', & ! 0   
     &    'R94             ', 'R95             ', 'R96             ', & ! 1   
     &    'R97             ', 'R98             ', 'R99             ', & ! 2   
     &    'R100            ', 'R101            ', 'R102            ', & ! 3   
     &    'R103            ', 'R104            ', 'R105            ', & ! 4   
     &    'R106            ', 'R107            ', 'R108            ', & ! 5   
     &    'R109            ', 'R110            ', 'R111            ', & ! 6   
     &    'R112            ', 'R113            ', 'R114            ', & ! 7   
     &    'R115            ', 'R116            ', 'R117            ', & ! 8   
     &    'R118            ', 'R119            ', 'R120            ', & ! 9   
     &    'R121            ', 'R122            ', 'R123            ', & ! 0   
     &    'R124            ', 'R125            ', 'R126            ', & ! 1   
     &    'R127            ', 'R128            ', 'R129            ', & ! 2   
     &    'R130            ', 'R131            ', 'R132            ', & ! 3   
     &    'R133            ', 'R134            ', 'R135            ', & ! 4   
     &    'R136            ', 'R137            ', 'R138            ', & ! 5   
     &    'R139            ', 'R140            ', 'R141            ', & ! 6   
     &    'R142            ', 'R143            ', 'R144            ', & ! 7   
     &    'R145            ', 'R146            ', 'R147            ', & ! 8   
     &    'R148            ', 'R149            ', 'R150            ', & ! 9   
     &    'R151            ', 'R152            ', 'R153            ', & ! 0   
     &    'R154            ', 'R155            ', 'R156            ', & ! 1   
     &    'R157            ', 'R158            ', 'R159            ', & ! 2   
     &    'R160            ', 'R161            ', 'R162            ', & ! 3   
     &    'R163            ', 'R164            ', 'R165            ', & ! 4   
     &    'R166            ', 'R167            ', 'R168            ', & ! 5   
     &    'R169            ', 'R170            ', 'R171            ', & ! 6   
     &    'R172            ', 'CL1             ', 'CL2             ', & ! 7   
     &    'CL3             ', 'CL4             ', 'CL5             ', & ! 8   
     &    'CL6             ', 'CL7             ', 'CL8             ', & ! 9   
     &    'CL9             ', 'CL10            ', 'CL11            ', & ! 0   
     &    'CL12            ', 'CL13            ', 'CL14            ', & ! 1   
     &    'CL15            ', 'CL16            ', 'CL17            ', & ! 2   
     &    'CL18            ', 'CL19            ', 'CL20            ', & ! 3   
     &    'CL21            ', 'CL22            ', 'CL23            ', & ! 4   
     &    'CL24            ', 'CL25            ', 'SA01            ', & ! 5   
     &    'SA02            ', 'SA03            ', 'SA04            ', & ! 6   
     &    'SA05            ', 'SA06            ', 'SA07            ', & ! 7   
     &    'SA08            ', 'SA09            ', 'SA10            ', & ! 8   
     &    'HET_N2O5IJ      ', 'HET_N2O5K       ', 'HET_H2NO3PIJA   ', & ! 9   
     &    'HET_H2NO3PKA    ', 'HET_H2NO3PIB    ', 'HET_H2NO3PJB    ', & ! 0   
     &    'HET_H2NO3PKB    ', 'HET_N02         ', 'OLIG_ALKENE     ', & ! 1   
     &    'OLIG_XYLENE1    ', 'OLIG_XYLENE2    ', 'OLIG_TOLUENE1   ', & ! 2   
     &    'OLIG_TOLUENE2   ', 'OLIG_BENZENE1   ', 'OLIG_BENZENE2   ', & ! 3   
     &    'OLIG_TERPENE1   ', 'OLIG_TERPENE2   ', 'OLIG_ISOPRENE1  ', & ! 4   
     &    'OLIG_ISOPRENE2  ', 'OLIG_SESQT1     ', 'RPOAGEPI        ', & ! 5   
     &    'RPOAGELI        ', 'RPOAGEPJ        ', 'RPOAGELJ        ', & ! 6   
     &    'T01             ', 'T02             ', 'T03             ', & ! 7   
     &    'T04             ', 'T05             ', 'TCL1            ', & ! 8   
     &    'T06             ', 'T07             ', 'T08             ', & ! 9   
     &    'T09             ', 'TCL2            ', 'T10             ', & ! 0   
     &    'T11             ', 'T12             ', 'TCL3            ', & ! 1   
     &    'T14             ', 'T15             ', 'T16             ', & ! 2   
     &    'T17             ', 'TCL4            ', 'T18             ', & ! 3   
     &    'T19             ', 'T20             ', 'T21             ', & ! 4   
     &    'TCL5            ', 'T22             ', 'TCL6            ', & ! 5   
     &    'T23             ', 'TCL7            ', 'T24             ', & ! 6   
     &    'TCL8            ', 'T25             ', 'TCL9            ', & ! 7   
     &    'T26             ', 'T27             ', 'T28             ', & ! 8   
     &    'T29             ', 'TCL10           ', 'T30             ', & ! 9   
     &    'T31             ', 'T32             ', 'T33             ', & ! 0   
     &    'TCL11           ', 'HG1             ', 'HG2             ', & ! 1   
     &    'HG3             ', 'HG4             ', 'HG5             '/! 2  

!    NSPECIAL     = Number of special rate coefficients
!    SPECIAL      = Names of special rate coefficients
!    NSPECIAL_RXN = Number of reactions with special rates
!    ISPECIAL     = Pointers to reactions using special rates and their special rate coefficients
!    MAXSPECTERMS = Max Number of each term type in  special rate coefficients
!    KC_COEFFS    = Coefficients of standard rate coefficients  times concentration terms 
!    INDEX_KTERMS  = Pointers to standard rate coefficients in  special rate coefficients
!    INDEX_CTERMS  = Pointers to species concentrations in  special rate coefficients
!    OPERATOR_COEFFS = Coefficients of preceeding special  rate coefficients used in special coefficient 
!    OPERATORS       = Pointers to preceeding special  rate coefficients used in special coefficient 

! Special Rate information not available ..
      INTEGER, PARAMETER :: NSPECIAL_RXN = 0
      INTEGER            :: ISPECIAL( 1, 2 )

! Special Rate information not available ...
      INTEGER, PARAMETER :: NSPECIAL = 0

! Special Rate information not available ...
      CHARACTER( 16 )    :: SPECIAL( 1 )

      INTEGER, PARAMETER :: MAXSPECTERMS =  10
      REAL( 8 )          :: KC_COEFFS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: INDEX_KTERMS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: INDEX_CTERMS( NSPECIAL + 1, MAXSPECTERMS)
      REAL( 8 )          :: OPERATOR_COEFFS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: OPERATORS( NSPECIAL + 1, MAXSPECTERMS)


!    Steady-state species section
!    N_SS_SPC     = Number of species assumed to be in steady-state
!    SS_SPC_DIM   = Dimension paramete for steady-state species
!    SS_SPC       = Names of species assumed to be in steady-state
!    MAX_SS_LOSS  = Max no. of SS loss rxns for any SS species
!    MAX_SS_PROD  = Max no. of SS prod rxns for any SS species
!    N_LOSS_RXNS  = No. of SS loss rxns for each SS species
!    N_PROD_RXNS  = No. of SS prod rxns for each SS species
!    SS_LOSS_RXNS = List of SS loss rxns for each SS species
!    SS_PROD_RXNS = List of SS prod rxns for each SS species
!    SS_PROD_COEF = List of SS prod yields for each SS species
!    SS_RCT_IND   = SS species index if it is a rxn reactant

      INTEGER, PARAMETER :: N_SS_SPC =   0

      INTEGER, PARAMETER :: SS_SPC_DIM =   1

      INTEGER, PARAMETER :: MAX_SS_LOSS =   0

      INTEGER, PARAMETER :: MAX_SS_PROD =   0

      CHARACTER( 16 )    :: SS_SPC( 1 )

      INTEGER            :: N_LOSS_RXNS( 1 )
      INTEGER            :: N_PROD_RXNS( 1 )
      INTEGER            :: SS_LOSS_RXNS( 1, 1 )
      INTEGER            :: SS_PROD_RXNS( 1, 1 )
      INTEGER            :: SS_RCT_IND( 1 )

      REAL               :: SS_PROD_COEF( 1,1 ) 
       LOGICAL,  PARAMETER :: USE_SPECIAL_RATES = .FALSE.
! pointers and names to specific photolysis rates
       INTEGER, PARAMETER  :: IJ_NO2_SAPRC99      =   1
       INTEGER, PARAMETER  :: IJ_O3_O3P_IUPAC04   =   2
       INTEGER, PARAMETER  :: IJ_O3_O1D_IUPAC04   =   3
       INTEGER, PARAMETER  :: IJ_NO3NO2_SAPRC99   =   4
       INTEGER, PARAMETER  :: IJ_NO3NO_SAPRC99    =   5
       INTEGER, PARAMETER  :: IJ_HONO_IUPAC04     =   6
       INTEGER, PARAMETER  :: IJ_H2O2_SAPRC99     =   7
       INTEGER, PARAMETER  :: IJ_HO2NO2_IUPAC04   =   8
       INTEGER, PARAMETER  :: IJ_HNO3_IUPAC04     =   9
       INTEGER, PARAMETER  :: IJ_N2O5_IUPAC04     =  10
       INTEGER, PARAMETER  :: IJ_NTR_IUPAC04      =  11
       INTEGER, PARAMETER  :: IJ_COOH_SAPRC99     =  12
       INTEGER, PARAMETER  :: IJ_HCHO_R_SAPRC99   =  13
       INTEGER, PARAMETER  :: IJ_HCHO_M_SAPRC99   =  14
       INTEGER, PARAMETER  :: IJ_CCHO_R_SAPRC99   =  15
       INTEGER, PARAMETER  :: IJ_PAN_IUPAC04      =  16
       INTEGER, PARAMETER  :: IJ_PACD_CB05        =  17
       INTEGER, PARAMETER  :: IJ_C2CHO_SAPRC99    =  18
       INTEGER, PARAMETER  :: IJ_MGLY_IUPAC04     =  19
       INTEGER, PARAMETER  :: IJ_ACROLEIN_SAPRC99 =  20
       INTEGER, PARAMETER  :: IJ_CL2_IUPAC04      =  21
       INTEGER, PARAMETER  :: IJ_HOCL_IUPAC04     =  22
       INTEGER, PARAMETER  :: IJ_FMCL_IUPAC04     =  23
       INTEGER, PARAMETER  :: IJ_CLNO2            =  24
       INTEGER, PARAMETER  :: IK_HETERO_N2O5IJ    =   1
       INTEGER, PARAMETER  :: IK_HETERO_N2O5K     =   2
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAIJ =   3
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAK  =   4
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBIJ =   5
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBK  =   6
       INTEGER, PARAMETER  :: IK_HETERO_NO2       =   7
       INTEGER, PARAMETER  :: IK_HETERO_PNCOMLI   =   8
       INTEGER, PARAMETER  :: IK_HETERO_PNCOMLJ   =   9
       END MODULE RXNS_DATA
