       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: ../../CCTM/src/MECHS/cb05tucl_ae6_aq/mech_cb05tucl_ae6_aq.def
! for Mechanism Name: CB05TUCL_AE6_AQ                 

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

      CHARACTER( 32 ), PARAMETER :: MECHNAME = 'CB05TUCL_AE6_AQ'

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC = 101
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 121

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
      DATA GAS_CHEM_SPC(  53 ) / 'XYLMN           ' /
      DATA GAS_CHEM_SPC(  54 ) / 'XYLRO2          ' /
      DATA GAS_CHEM_SPC(  55 ) / 'NAPH            ' /
      DATA GAS_CHEM_SPC(  56 ) / 'PAHRO2          ' /
      DATA GAS_CHEM_SPC(  57 ) / 'ISOP            ' /
      DATA GAS_CHEM_SPC(  58 ) / 'ISPD            ' /
      DATA GAS_CHEM_SPC(  59 ) / 'ISOPRXN         ' /
      DATA GAS_CHEM_SPC(  60 ) / 'TERP            ' /
      DATA GAS_CHEM_SPC(  61 ) / 'TRPRXN          ' /
      DATA GAS_CHEM_SPC(  62 ) / 'SO2             ' /
      DATA GAS_CHEM_SPC(  63 ) / 'SULF            ' /
      DATA GAS_CHEM_SPC(  64 ) / 'SULRXN          ' /
      DATA GAS_CHEM_SPC(  65 ) / 'ETOH            ' /
      DATA GAS_CHEM_SPC(  66 ) / 'ETHA            ' /
      DATA GAS_CHEM_SPC(  67 ) / 'CL2             ' /
      DATA GAS_CHEM_SPC(  68 ) / 'CL              ' /
      DATA GAS_CHEM_SPC(  69 ) / 'HOCL            ' /
      DATA GAS_CHEM_SPC(  70 ) / 'CLO             ' /
      DATA GAS_CHEM_SPC(  71 ) / 'FMCL            ' /
      DATA GAS_CHEM_SPC(  72 ) / 'HCL             ' /
      DATA GAS_CHEM_SPC(  73 ) / 'CLNO2           ' /
      DATA GAS_CHEM_SPC(  74 ) / 'TOLNRXN         ' /
      DATA GAS_CHEM_SPC(  75 ) / 'TOLHRXN         ' /
      DATA GAS_CHEM_SPC(  76 ) / 'XYLNRXN         ' /
      DATA GAS_CHEM_SPC(  77 ) / 'XYLHRXN         ' /
      DATA GAS_CHEM_SPC(  78 ) / 'BENZENE         ' /
      DATA GAS_CHEM_SPC(  79 ) / 'BENZRO2         ' /
      DATA GAS_CHEM_SPC(  80 ) / 'BNZNRXN         ' /
      DATA GAS_CHEM_SPC(  81 ) / 'BNZHRXN         ' /
      DATA GAS_CHEM_SPC(  82 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC(  83 ) / 'SESQRXN         ' /
      DATA GAS_CHEM_SPC(  84 ) / 'PAHNRXN         ' /
      DATA GAS_CHEM_SPC(  85 ) / 'PAHHRXN         ' /
      DATA GAS_CHEM_SPC(  86 ) / 'SOAALK          ' /
      DATA GAS_CHEM_SPC(  87 ) / 'ALKRXN          ' /
      DATA GAS_CHEM_SPC(  88 ) / 'H2NO3PIJ        ' /
      DATA GAS_CHEM_SPC(  89 ) / 'H2NO3PK         ' /
      DATA GAS_CHEM_SPC(  90 ) / 'PCVOC           ' /
      DATA GAS_CHEM_SPC(  91 ) / 'PCSOARXN        ' /
      DATA GAS_CHEM_SPC(  92 ) / 'VLVPO1          ' /
      DATA GAS_CHEM_SPC(  93 ) / 'VSVPO1          ' /
      DATA GAS_CHEM_SPC(  94 ) / 'VSVPO2          ' /
      DATA GAS_CHEM_SPC(  95 ) / 'VSVPO3          ' /
      DATA GAS_CHEM_SPC(  96 ) / 'VIVPO1          ' /
      DATA GAS_CHEM_SPC(  97 ) / 'VLVOO1          ' /
      DATA GAS_CHEM_SPC(  98 ) / 'VLVOO2          ' /
      DATA GAS_CHEM_SPC(  99 ) / 'VSVOO2          ' /
      DATA GAS_CHEM_SPC( 100 ) / 'VSVOO3          ' /
      DATA GAS_CHEM_SPC( 101 ) / 'VSVOO1          ' /




      LOGICAL   :: HALOGEN_PARAMETER = .TRUE. 

      DATA CHEMISTRY_SPC(   1 ), SPECIES_MOLWT(   1 ) / 'NO2             ',   46.00 /
      DATA CHEMISTRY_SPC(   2 ), SPECIES_MOLWT(   2 ) / 'NO              ',   30.00 /
      DATA CHEMISTRY_SPC(   3 ), SPECIES_MOLWT(   3 ) / 'O               ',   16.00 /
      DATA CHEMISTRY_SPC(   4 ), SPECIES_MOLWT(   4 ) / 'O3              ',   48.00 /
      DATA CHEMISTRY_SPC(   5 ), SPECIES_MOLWT(   5 ) / 'NO3             ',   62.00 /
      DATA CHEMISTRY_SPC(   6 ), SPECIES_MOLWT(   6 ) / 'O1D             ',   16.00 /
      DATA CHEMISTRY_SPC(   7 ), SPECIES_MOLWT(   7 ) / 'OH              ',   17.00 /
      DATA CHEMISTRY_SPC(   8 ), SPECIES_MOLWT(   8 ) / 'HO2             ',   33.00 /
      DATA CHEMISTRY_SPC(   9 ), SPECIES_MOLWT(   9 ) / 'N2O5            ',  108.00 /
      DATA CHEMISTRY_SPC(  10 ), SPECIES_MOLWT(  10 ) / 'HNO3            ',   63.00 /
      DATA CHEMISTRY_SPC(  11 ), SPECIES_MOLWT(  11 ) / 'HONO            ',   47.00 /
      DATA CHEMISTRY_SPC(  12 ), SPECIES_MOLWT(  12 ) / 'PNA             ',   79.00 /
      DATA CHEMISTRY_SPC(  13 ), SPECIES_MOLWT(  13 ) / 'H2O2            ',   34.00 /
      DATA CHEMISTRY_SPC(  14 ), SPECIES_MOLWT(  14 ) / 'XO2             ',    1.00 /
      DATA CHEMISTRY_SPC(  15 ), SPECIES_MOLWT(  15 ) / 'XO2N            ',    1.00 /
      DATA CHEMISTRY_SPC(  16 ), SPECIES_MOLWT(  16 ) / 'NTR             ',  130.00 /
      DATA CHEMISTRY_SPC(  17 ), SPECIES_MOLWT(  17 ) / 'ROOH            ',   62.00 /
      DATA CHEMISTRY_SPC(  18 ), SPECIES_MOLWT(  18 ) / 'FORM            ',   30.00 /
      DATA CHEMISTRY_SPC(  19 ), SPECIES_MOLWT(  19 ) / 'ALD2            ',   44.00 /
      DATA CHEMISTRY_SPC(  20 ), SPECIES_MOLWT(  20 ) / 'ALDX            ',   44.00 /
      DATA CHEMISTRY_SPC(  21 ), SPECIES_MOLWT(  21 ) / 'PAR             ',   14.00 /
      DATA CHEMISTRY_SPC(  22 ), SPECIES_MOLWT(  22 ) / 'CO              ',   28.00 /
      DATA CHEMISTRY_SPC(  23 ), SPECIES_MOLWT(  23 ) / 'MEO2            ',   47.00 /
      DATA CHEMISTRY_SPC(  24 ), SPECIES_MOLWT(  24 ) / 'MEPX            ',   48.00 /
      DATA CHEMISTRY_SPC(  25 ), SPECIES_MOLWT(  25 ) / 'MEOH            ',   32.00 /
      DATA CHEMISTRY_SPC(  26 ), SPECIES_MOLWT(  26 ) / 'HCO3            ',   63.00 /
      DATA CHEMISTRY_SPC(  27 ), SPECIES_MOLWT(  27 ) / 'FACD            ',   46.00 /
      DATA CHEMISTRY_SPC(  28 ), SPECIES_MOLWT(  28 ) / 'C2O3            ',   75.00 /
      DATA CHEMISTRY_SPC(  29 ), SPECIES_MOLWT(  29 ) / 'PAN             ',  121.00 /
      DATA CHEMISTRY_SPC(  30 ), SPECIES_MOLWT(  30 ) / 'PACD            ',   76.00 /
      DATA CHEMISTRY_SPC(  31 ), SPECIES_MOLWT(  31 ) / 'AACD            ',   60.00 /
      DATA CHEMISTRY_SPC(  32 ), SPECIES_MOLWT(  32 ) / 'CXO3            ',   75.00 /
      DATA CHEMISTRY_SPC(  33 ), SPECIES_MOLWT(  33 ) / 'PANX            ',  121.00 /
      DATA CHEMISTRY_SPC(  34 ), SPECIES_MOLWT(  34 ) / 'ROR             ',   31.00 /
      DATA CHEMISTRY_SPC(  35 ), SPECIES_MOLWT(  35 ) / 'OLE             ',   27.00 /
      DATA CHEMISTRY_SPC(  36 ), SPECIES_MOLWT(  36 ) / 'ETH             ',   28.00 /
      DATA CHEMISTRY_SPC(  37 ), SPECIES_MOLWT(  37 ) / 'IOLE            ',   48.00 /
      DATA CHEMISTRY_SPC(  38 ), SPECIES_MOLWT(  38 ) / 'TOL             ',   92.00 /
      DATA CHEMISTRY_SPC(  39 ), SPECIES_MOLWT(  39 ) / 'CRES            ',  108.00 /
      DATA CHEMISTRY_SPC(  40 ), SPECIES_MOLWT(  40 ) / 'TO2             ',  173.00 /
      DATA CHEMISTRY_SPC(  41 ), SPECIES_MOLWT(  41 ) / 'TOLRO2          ',  141.00 /
      DATA CHEMISTRY_SPC(  42 ), SPECIES_MOLWT(  42 ) / 'OPEN            ',   84.00 /
      DATA CHEMISTRY_SPC(  43 ), SPECIES_MOLWT(  43 ) / 'MGLY            ',   72.00 /
      DATA CHEMISTRY_SPC(  44 ), SPECIES_MOLWT(  44 ) / 'CRO             ',  107.00 /
      DATA CHEMISTRY_SPC(  45 ), SPECIES_MOLWT(  45 ) / 'CAT1            ',  124.00 /
      DATA CHEMISTRY_SPC(  46 ), SPECIES_MOLWT(  46 ) / 'CRON            ',  153.00 /
      DATA CHEMISTRY_SPC(  47 ), SPECIES_MOLWT(  47 ) / 'CRNO            ',  152.00 /
      DATA CHEMISTRY_SPC(  48 ), SPECIES_MOLWT(  48 ) / 'CRN2            ',  168.00 /
      DATA CHEMISTRY_SPC(  49 ), SPECIES_MOLWT(  49 ) / 'CRPX            ',  169.00 /
      DATA CHEMISTRY_SPC(  50 ), SPECIES_MOLWT(  50 ) / 'OPO3            ',  115.00 /
      DATA CHEMISTRY_SPC(  51 ), SPECIES_MOLWT(  51 ) / 'CAO2            ',  133.00 /
      DATA CHEMISTRY_SPC(  52 ), SPECIES_MOLWT(  52 ) / 'OPAN            ',  161.00 /
      DATA CHEMISTRY_SPC(  53 ), SPECIES_MOLWT(  53 ) / 'XYLMN           ',  106.00 /
      DATA CHEMISTRY_SPC(  54 ), SPECIES_MOLWT(  54 ) / 'XYLRO2          ',  155.00 /
      DATA CHEMISTRY_SPC(  55 ), SPECIES_MOLWT(  55 ) / 'NAPH            ',  128.20 /
      DATA CHEMISTRY_SPC(  56 ), SPECIES_MOLWT(  56 ) / 'PAHRO2          ',  187.20 /
      DATA CHEMISTRY_SPC(  57 ), SPECIES_MOLWT(  57 ) / 'ISOP            ',   68.00 /
      DATA CHEMISTRY_SPC(  58 ), SPECIES_MOLWT(  58 ) / 'ISPD            ',   70.00 /
      DATA CHEMISTRY_SPC(  59 ), SPECIES_MOLWT(  59 ) / 'ISOPRXN         ',   68.00 /
      DATA CHEMISTRY_SPC(  60 ), SPECIES_MOLWT(  60 ) / 'TERP            ',  136.00 /
      DATA CHEMISTRY_SPC(  61 ), SPECIES_MOLWT(  61 ) / 'TRPRXN          ',  136.00 /
      DATA CHEMISTRY_SPC(  62 ), SPECIES_MOLWT(  62 ) / 'SO2             ',   64.00 /
      DATA CHEMISTRY_SPC(  63 ), SPECIES_MOLWT(  63 ) / 'SULF            ',   98.00 /
      DATA CHEMISTRY_SPC(  64 ), SPECIES_MOLWT(  64 ) / 'SULRXN          ',   98.00 /
      DATA CHEMISTRY_SPC(  65 ), SPECIES_MOLWT(  65 ) / 'ETOH            ',   46.00 /
      DATA CHEMISTRY_SPC(  66 ), SPECIES_MOLWT(  66 ) / 'ETHA            ',   30.00 /
      DATA CHEMISTRY_SPC(  67 ), SPECIES_MOLWT(  67 ) / 'CL2             ',   71.00 /
      DATA CHEMISTRY_SPC(  68 ), SPECIES_MOLWT(  68 ) / 'CL              ',   35.50 /
      DATA CHEMISTRY_SPC(  69 ), SPECIES_MOLWT(  69 ) / 'HOCL            ',   52.50 /
      DATA CHEMISTRY_SPC(  70 ), SPECIES_MOLWT(  70 ) / 'CLO             ',   51.50 /
      DATA CHEMISTRY_SPC(  71 ), SPECIES_MOLWT(  71 ) / 'FMCL            ',   64.50 /
      DATA CHEMISTRY_SPC(  72 ), SPECIES_MOLWT(  72 ) / 'HCL             ',   36.50 /
      DATA CHEMISTRY_SPC(  73 ), SPECIES_MOLWT(  73 ) / 'CLNO2           ',   81.50 /
      DATA CHEMISTRY_SPC(  74 ), SPECIES_MOLWT(  74 ) / 'TOLNRXN         ',  141.00 /
      DATA CHEMISTRY_SPC(  75 ), SPECIES_MOLWT(  75 ) / 'TOLHRXN         ',  141.00 /
      DATA CHEMISTRY_SPC(  76 ), SPECIES_MOLWT(  76 ) / 'XYLNRXN         ',  155.00 /
      DATA CHEMISTRY_SPC(  77 ), SPECIES_MOLWT(  77 ) / 'XYLHRXN         ',  155.00 /
      DATA CHEMISTRY_SPC(  78 ), SPECIES_MOLWT(  78 ) / 'BENZENE         ',   78.00 /
      DATA CHEMISTRY_SPC(  79 ), SPECIES_MOLWT(  79 ) / 'BENZRO2         ',  127.00 /
      DATA CHEMISTRY_SPC(  80 ), SPECIES_MOLWT(  80 ) / 'BNZNRXN         ',  127.00 /
      DATA CHEMISTRY_SPC(  81 ), SPECIES_MOLWT(  81 ) / 'BNZHRXN         ',  127.00 /
      DATA CHEMISTRY_SPC(  82 ), SPECIES_MOLWT(  82 ) / 'SESQ            ',  204.00 /
      DATA CHEMISTRY_SPC(  83 ), SPECIES_MOLWT(  83 ) / 'SESQRXN         ',  204.00 /
      DATA CHEMISTRY_SPC(  84 ), SPECIES_MOLWT(  84 ) / 'PAHNRXN         ',  187.20 /
      DATA CHEMISTRY_SPC(  85 ), SPECIES_MOLWT(  85 ) / 'PAHHRXN         ',  187.20 /
      DATA CHEMISTRY_SPC(  86 ), SPECIES_MOLWT(  86 ) / 'SOAALK          ',  112.00 /
      DATA CHEMISTRY_SPC(  87 ), SPECIES_MOLWT(  87 ) / 'ALKRXN          ',  112.00 /
      DATA CHEMISTRY_SPC(  88 ), SPECIES_MOLWT(  88 ) / 'H2NO3PIJ        ',   64.00 /
      DATA CHEMISTRY_SPC(  89 ), SPECIES_MOLWT(  89 ) / 'H2NO3PK         ',   64.00 /
      DATA CHEMISTRY_SPC(  90 ), SPECIES_MOLWT(  90 ) / 'ACLI            ',   35.50 /
      DATA CHEMISTRY_SPC(  91 ), SPECIES_MOLWT(  91 ) / 'ACLJ            ',   35.50 /
      DATA CHEMISTRY_SPC(  92 ), SPECIES_MOLWT(  92 ) / 'ACLK            ',   35.50 /
      DATA CHEMISTRY_SPC(  93 ), SPECIES_MOLWT(  93 ) / 'AXYL1J          ',  174.00 /
      DATA CHEMISTRY_SPC(  94 ), SPECIES_MOLWT(  94 ) / 'AOLGAJ          ',  206.00 /
      DATA CHEMISTRY_SPC(  95 ), SPECIES_MOLWT(  95 ) / 'AXYL2J          ',  185.00 /
      DATA CHEMISTRY_SPC(  96 ), SPECIES_MOLWT(  96 ) / 'ATOL1J          ',  163.00 /
      DATA CHEMISTRY_SPC(  97 ), SPECIES_MOLWT(  97 ) / 'ATOL2J          ',  175.00 /
      DATA CHEMISTRY_SPC(  98 ), SPECIES_MOLWT(  98 ) / 'ABNZ1J          ',  161.00 /
      DATA CHEMISTRY_SPC(  99 ), SPECIES_MOLWT(  99 ) / 'ABNZ2J          ',  134.00 /
      DATA CHEMISTRY_SPC( 100 ), SPECIES_MOLWT( 100 ) / 'ATRP1J          ',  177.00 /
      DATA CHEMISTRY_SPC( 101 ), SPECIES_MOLWT( 101 ) / 'AOLGBJ          ',  248.00 /
      DATA CHEMISTRY_SPC( 102 ), SPECIES_MOLWT( 102 ) / 'ATRP2J          ',  198.00 /
      DATA CHEMISTRY_SPC( 103 ), SPECIES_MOLWT( 103 ) / 'AISO1J          ',  132.00 /
      DATA CHEMISTRY_SPC( 104 ), SPECIES_MOLWT( 104 ) / 'AISO2J          ',  133.00 /
      DATA CHEMISTRY_SPC( 105 ), SPECIES_MOLWT( 105 ) / 'ASQTJ           ',  273.00 /
      DATA CHEMISTRY_SPC( 106 ), SPECIES_MOLWT( 106 ) / 'APAH1J          ',  195.60 /
      DATA CHEMISTRY_SPC( 107 ), SPECIES_MOLWT( 107 ) / 'APAH2J          ',  178.70 /
      DATA CHEMISTRY_SPC( 108 ), SPECIES_MOLWT( 108 ) / 'AALK1J          ',  225.00 /
      DATA CHEMISTRY_SPC( 109 ), SPECIES_MOLWT( 109 ) / 'AALK2J          ',  205.10 /
      DATA CHEMISTRY_SPC( 110 ), SPECIES_MOLWT( 110 ) / 'PCVOC           ',  170.00 /
      DATA CHEMISTRY_SPC( 111 ), SPECIES_MOLWT( 111 ) / 'PCSOARXN        ',  170.00 /
      DATA CHEMISTRY_SPC( 112 ), SPECIES_MOLWT( 112 ) / 'VLVPO1          ',  218.00 /
      DATA CHEMISTRY_SPC( 113 ), SPECIES_MOLWT( 113 ) / 'VSVPO1          ',  230.00 /
      DATA CHEMISTRY_SPC( 114 ), SPECIES_MOLWT( 114 ) / 'VSVPO2          ',  241.00 /
      DATA CHEMISTRY_SPC( 115 ), SPECIES_MOLWT( 115 ) / 'VSVPO3          ',  253.00 /
      DATA CHEMISTRY_SPC( 116 ), SPECIES_MOLWT( 116 ) / 'VIVPO1          ',  266.00 /
      DATA CHEMISTRY_SPC( 117 ), SPECIES_MOLWT( 117 ) / 'VLVOO1          ',  136.00 /
      DATA CHEMISTRY_SPC( 118 ), SPECIES_MOLWT( 118 ) / 'VLVOO2          ',  136.00 /
      DATA CHEMISTRY_SPC( 119 ), SPECIES_MOLWT( 119 ) / 'VSVOO2          ',  135.00 /
      DATA CHEMISTRY_SPC( 120 ), SPECIES_MOLWT( 120 ) / 'VSVOO3          ',  134.00 /
      DATA CHEMISTRY_SPC( 121 ), SPECIES_MOLWT( 121 ) / 'VSVOO1          ',  135.00 /



      LOGICAL   :: MAPPED_TO_CGRID   = .FALSE. 


! MAPPED_TO_CGRID declares whether CMAQ namelists were used to determine 
! the below values of CGRID_INDEX, SPECIES_TYPE, SPECIES_MOLWT, and CONVERT_CONC
      LOGICAL, PARAMETER, PRIVATE :: F = .FALSE.
      LOGICAL, PARAMETER, PRIVATE :: T = .TRUE.

      DATA CGRID_INDEX(   1 ), SPECIES_TYPE(   1 ), CONVERT_CONC(   1 ) /    1, 'GC', F /  ! NO2
      DATA CGRID_INDEX(   2 ), SPECIES_TYPE(   2 ), CONVERT_CONC(   2 ) /    2, 'GC', F /  ! NO
      DATA CGRID_INDEX(   3 ), SPECIES_TYPE(   3 ), CONVERT_CONC(   3 ) /    3, 'GC', F /  ! O
      DATA CGRID_INDEX(   4 ), SPECIES_TYPE(   4 ), CONVERT_CONC(   4 ) /    4, 'GC', F /  ! O3
      DATA CGRID_INDEX(   5 ), SPECIES_TYPE(   5 ), CONVERT_CONC(   5 ) /    5, 'GC', F /  ! NO3
      DATA CGRID_INDEX(   6 ), SPECIES_TYPE(   6 ), CONVERT_CONC(   6 ) /    6, 'GC', F /  ! O1D
      DATA CGRID_INDEX(   7 ), SPECIES_TYPE(   7 ), CONVERT_CONC(   7 ) /    7, 'GC', F /  ! OH
      DATA CGRID_INDEX(   8 ), SPECIES_TYPE(   8 ), CONVERT_CONC(   8 ) /    8, 'GC', F /  ! HO2
      DATA CGRID_INDEX(   9 ), SPECIES_TYPE(   9 ), CONVERT_CONC(   9 ) /    9, 'GC', F /  ! N2O5
      DATA CGRID_INDEX(  10 ), SPECIES_TYPE(  10 ), CONVERT_CONC(  10 ) /   10, 'GC', F /  ! HNO3
      DATA CGRID_INDEX(  11 ), SPECIES_TYPE(  11 ), CONVERT_CONC(  11 ) /   11, 'GC', F /  ! HONO
      DATA CGRID_INDEX(  12 ), SPECIES_TYPE(  12 ), CONVERT_CONC(  12 ) /   12, 'GC', F /  ! PNA
      DATA CGRID_INDEX(  13 ), SPECIES_TYPE(  13 ), CONVERT_CONC(  13 ) /   13, 'GC', F /  ! H2O2
      DATA CGRID_INDEX(  14 ), SPECIES_TYPE(  14 ), CONVERT_CONC(  14 ) /   14, 'GC', F /  ! XO2
      DATA CGRID_INDEX(  15 ), SPECIES_TYPE(  15 ), CONVERT_CONC(  15 ) /   15, 'GC', F /  ! XO2N
      DATA CGRID_INDEX(  16 ), SPECIES_TYPE(  16 ), CONVERT_CONC(  16 ) /   16, 'GC', F /  ! NTR
      DATA CGRID_INDEX(  17 ), SPECIES_TYPE(  17 ), CONVERT_CONC(  17 ) /   17, 'GC', F /  ! ROOH
      DATA CGRID_INDEX(  18 ), SPECIES_TYPE(  18 ), CONVERT_CONC(  18 ) /   18, 'GC', F /  ! FORM
      DATA CGRID_INDEX(  19 ), SPECIES_TYPE(  19 ), CONVERT_CONC(  19 ) /   19, 'GC', F /  ! ALD2
      DATA CGRID_INDEX(  20 ), SPECIES_TYPE(  20 ), CONVERT_CONC(  20 ) /   20, 'GC', F /  ! ALDX
      DATA CGRID_INDEX(  21 ), SPECIES_TYPE(  21 ), CONVERT_CONC(  21 ) /   21, 'GC', F /  ! PAR
      DATA CGRID_INDEX(  22 ), SPECIES_TYPE(  22 ), CONVERT_CONC(  22 ) /   22, 'GC', F /  ! CO
      DATA CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), CONVERT_CONC(  23 ) /   23, 'GC', F /  ! MEO2
      DATA CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), CONVERT_CONC(  24 ) /   24, 'GC', F /  ! MEPX
      DATA CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), CONVERT_CONC(  25 ) /   25, 'GC', F /  ! MEOH
      DATA CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), CONVERT_CONC(  26 ) /   26, 'GC', F /  ! HCO3
      DATA CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), CONVERT_CONC(  27 ) /   27, 'GC', F /  ! FACD
      DATA CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), CONVERT_CONC(  28 ) /   28, 'GC', F /  ! C2O3
      DATA CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), CONVERT_CONC(  29 ) /   29, 'GC', F /  ! PAN
      DATA CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), CONVERT_CONC(  30 ) /   30, 'GC', F /  ! PACD
      DATA CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), CONVERT_CONC(  31 ) /   31, 'GC', F /  ! AACD
      DATA CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), CONVERT_CONC(  32 ) /   32, 'GC', F /  ! CXO3
      DATA CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), CONVERT_CONC(  33 ) /   33, 'GC', F /  ! PANX
      DATA CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), CONVERT_CONC(  34 ) /   34, 'GC', F /  ! ROR
      DATA CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), CONVERT_CONC(  35 ) /   35, 'GC', F /  ! OLE
      DATA CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), CONVERT_CONC(  36 ) /   36, 'GC', F /  ! ETH
      DATA CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), CONVERT_CONC(  37 ) /   37, 'GC', F /  ! IOLE
      DATA CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), CONVERT_CONC(  38 ) /   38, 'GC', F /  ! TOL
      DATA CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), CONVERT_CONC(  39 ) /   39, 'GC', F /  ! CRES
      DATA CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), CONVERT_CONC(  40 ) /   40, 'GC', F /  ! TO2
      DATA CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), CONVERT_CONC(  41 ) /   41, 'GC', F /  ! TOLRO2
      DATA CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), CONVERT_CONC(  42 ) /   42, 'GC', F /  ! OPEN
      DATA CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), CONVERT_CONC(  43 ) /   43, 'GC', F /  ! MGLY
      DATA CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), CONVERT_CONC(  44 ) /   44, 'GC', F /  ! CRO
      DATA CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), CONVERT_CONC(  45 ) /   45, 'GC', F /  ! CAT1
      DATA CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), CONVERT_CONC(  46 ) /   46, 'GC', F /  ! CRON
      DATA CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), CONVERT_CONC(  47 ) /   47, 'GC', F /  ! CRNO
      DATA CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), CONVERT_CONC(  48 ) /   48, 'GC', F /  ! CRN2
      DATA CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), CONVERT_CONC(  49 ) /   49, 'GC', F /  ! CRPX
      DATA CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), CONVERT_CONC(  50 ) /   50, 'GC', F /  ! OPO3
      DATA CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), CONVERT_CONC(  51 ) /   51, 'GC', F /  ! CAO2
      DATA CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), CONVERT_CONC(  52 ) /   52, 'GC', F /  ! OPAN
      DATA CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), CONVERT_CONC(  53 ) /   53, 'GC', F /  ! XYLMN
      DATA CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), CONVERT_CONC(  54 ) /   54, 'GC', F /  ! XYLRO2
      DATA CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), CONVERT_CONC(  55 ) /   55, 'GC', F /  ! NAPH
      DATA CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), CONVERT_CONC(  56 ) /   56, 'GC', F /  ! PAHRO2
      DATA CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), CONVERT_CONC(  57 ) /   57, 'GC', F /  ! ISOP
      DATA CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), CONVERT_CONC(  58 ) /   58, 'GC', F /  ! ISPD
      DATA CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), CONVERT_CONC(  59 ) /   59, 'GC', F /  ! ISOPRXN
      DATA CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), CONVERT_CONC(  60 ) /   60, 'GC', F /  ! TERP
      DATA CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), CONVERT_CONC(  61 ) /   61, 'GC', F /  ! TRPRXN
      DATA CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), CONVERT_CONC(  62 ) /   62, 'GC', F /  ! SO2
      DATA CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), CONVERT_CONC(  63 ) /   63, 'GC', F /  ! SULF
      DATA CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), CONVERT_CONC(  64 ) /   64, 'GC', F /  ! SULRXN
      DATA CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), CONVERT_CONC(  65 ) /   65, 'GC', F /  ! ETOH
      DATA CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), CONVERT_CONC(  66 ) /   66, 'GC', F /  ! ETHA
      DATA CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), CONVERT_CONC(  67 ) /   67, 'GC', F /  ! CL2
      DATA CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), CONVERT_CONC(  68 ) /   68, 'GC', F /  ! CL
      DATA CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), CONVERT_CONC(  69 ) /   69, 'GC', F /  ! HOCL
      DATA CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), CONVERT_CONC(  70 ) /   70, 'GC', F /  ! CLO
      DATA CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), CONVERT_CONC(  71 ) /   71, 'GC', F /  ! FMCL
      DATA CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), CONVERT_CONC(  72 ) /   72, 'GC', F /  ! HCL
      DATA CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), CONVERT_CONC(  73 ) /   73, 'GC', F /  ! CLNO2
      DATA CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), CONVERT_CONC(  74 ) /   74, 'GC', F /  ! TOLNRXN
      DATA CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), CONVERT_CONC(  75 ) /   75, 'GC', F /  ! TOLHRXN
      DATA CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), CONVERT_CONC(  76 ) /   76, 'GC', F /  ! XYLNRXN
      DATA CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), CONVERT_CONC(  77 ) /   77, 'GC', F /  ! XYLHRXN
      DATA CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), CONVERT_CONC(  78 ) /   78, 'GC', F /  ! BENZENE
      DATA CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), CONVERT_CONC(  79 ) /   79, 'GC', F /  ! BENZRO2
      DATA CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), CONVERT_CONC(  80 ) /   80, 'GC', F /  ! BNZNRXN
      DATA CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), CONVERT_CONC(  81 ) /   81, 'GC', F /  ! BNZHRXN
      DATA CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), CONVERT_CONC(  82 ) /   82, 'GC', F /  ! SESQ
      DATA CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), CONVERT_CONC(  83 ) /   83, 'GC', F /  ! SESQRXN
      DATA CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), CONVERT_CONC(  84 ) /   84, 'GC', F /  ! PAHNRXN
      DATA CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), CONVERT_CONC(  85 ) /   85, 'GC', F /  ! PAHHRXN
      DATA CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), CONVERT_CONC(  86 ) /   86, 'GC', F /  ! SOAALK
      DATA CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), CONVERT_CONC(  87 ) /   87, 'GC', F /  ! ALKRXN
      DATA CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), CONVERT_CONC(  88 ) /   88, 'GC', F /  ! H2NO3PIJ
      DATA CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), CONVERT_CONC(  89 ) /   89, 'GC', F /  ! H2NO3PK
      DATA CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), CONVERT_CONC(  90 ) /  156, 'AE', T /  ! ACLI
      DATA CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), CONVERT_CONC(  91 ) /  155, 'AE', T /  ! ACLJ
      DATA CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), CONVERT_CONC(  92 ) /  158, 'AE', T /  ! ACLK
      DATA CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), CONVERT_CONC(  93 ) /  111, 'AE', T /  ! AXYL1J
      DATA CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), CONVERT_CONC(  94 ) /  165, 'AE', T /  ! AOLGAJ
      DATA CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), CONVERT_CONC(  95 ) /  112, 'AE', T /  ! AXYL2J
      DATA CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), CONVERT_CONC(  96 ) /  114, 'AE', T /  ! ATOL1J
      DATA CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), CONVERT_CONC(  97 ) /  115, 'AE', T /  ! ATOL2J
      DATA CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), CONVERT_CONC(  98 ) /  117, 'AE', T /  ! ABNZ1J
      DATA CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), CONVERT_CONC(  99 ) /  118, 'AE', T /  ! ABNZ2J
      DATA CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), CONVERT_CONC( 100 ) /  123, 'AE', T /  ! ATRP1J
      DATA CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), CONVERT_CONC( 101 ) /  166, 'AE', T /  ! AOLGBJ
      DATA CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), CONVERT_CONC( 102 ) /  124, 'AE', T /  ! ATRP2J
      DATA CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), CONVERT_CONC( 103 ) /  125, 'AE', T /  ! AISO1J
      DATA CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), CONVERT_CONC( 104 ) /  126, 'AE', T /  ! AISO2J
      DATA CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), CONVERT_CONC( 105 ) /  127, 'AE', T /  ! ASQTJ
      DATA CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), CONVERT_CONC( 106 ) /  120, 'AE', T /  ! APAH1J
      DATA CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), CONVERT_CONC( 107 ) /  121, 'AE', T /  ! APAH2J
      DATA CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), CONVERT_CONC( 108 ) /  109, 'AE', T /  ! AALK1J
      DATA CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), CONVERT_CONC( 109 ) /  110, 'AE', T /  ! AALK2J
      DATA CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), CONVERT_CONC( 110 ) /  100, 'GC', F /  ! PCVOC
      DATA CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), CONVERT_CONC( 111 ) /  101, 'GC', F /  ! PCSOARXN
      DATA CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), CONVERT_CONC( 112 ) /   90, 'GC', F /  ! VLVPO1
      DATA CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), CONVERT_CONC( 113 ) /   91, 'GC', F /  ! VSVPO1
      DATA CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), CONVERT_CONC( 114 ) /   92, 'GC', F /  ! VSVPO2
      DATA CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), CONVERT_CONC( 115 ) /   93, 'GC', F /  ! VSVPO3
      DATA CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), CONVERT_CONC( 116 ) /   94, 'GC', F /  ! VIVPO1
      DATA CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), CONVERT_CONC( 117 ) /   95, 'GC', F /  ! VLVOO1
      DATA CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), CONVERT_CONC( 118 ) /   96, 'GC', F /  ! VLVOO2
      DATA CGRID_INDEX( 119 ), SPECIES_TYPE( 119 ), CONVERT_CONC( 119 ) /   98, 'GC', F /  ! VSVOO2
      DATA CGRID_INDEX( 120 ), SPECIES_TYPE( 120 ), CONVERT_CONC( 120 ) /   99, 'GC', F /  ! VSVOO3
      DATA CGRID_INDEX( 121 ), SPECIES_TYPE( 121 ), CONVERT_CONC( 121 ) /   97, 'GC', F /  ! VSVOO1

      INTEGER, PARAMETER :: N_ACT_SP = 121

      INTEGER, PARAMETER :: NRXNS = 246

      INTEGER            :: KUNITS

      DATA  KUNITS /   2 /

      INTEGER IRXXN

      REAL( 8 )          :: RTDAT( 3,NRXNS )

      INTEGER, PARAMETER :: NFALLOFF =  21
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
     &      1,    1,    1,    3,    3,    1,    0,    1,    3,    3, & ! 5   
     &      3,    1,    1,    1,    0,    1,    3,    3,    3,   10, & ! 6   
     &      3,    3,    1,    0,    0,    3,    1,    3,    3,    1, & ! 7   
     &      0,    3,    1,    3,    1,    1,    1,    1,    3,    1, & ! 8   
     &      1,    1,    3,    4,    1,    1,   10,    0,    3,    3, & ! 9   
     &      3,    3,    3,    3,    3,    1,    1,    1,    3,    3, & ! O   
     &      3,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   12, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1/     !  4   

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
     &      0,    0,    0,    0,    0,    0,    2,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    2,    0,    0,    0,    0,    1, & ! 6   
     &      0,    0,    0,    2,    2,    0,    0,    0,    0,    0, & ! 7   
     &      2,   64,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    1,    2,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    1,    1,    1,    1,    1,    1,    1,    1,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0/     !  4   

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
     &      2,    2,    1,    2,    2,    2,    1,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    2, & ! 7   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    1,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    1,    1,    1,    1,    2,    2,    2,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2/     !  4   

      INTEGER, PARAMETER :: KTN1 =  86
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     11,   19,   20,   23,   27,   38,   46,   47,   48,   49, & ! O   
     &     58,   59,   60,   73,   77,   80,   82,  100,  106,  112, & ! 1   
     &    114,  115,  117,  124,  132,  133,  134,  135,  136,  137, & ! 2   
     &    138,  144,  146,  147,  148,  151,  152,  153,  156,  158, & ! 3   
     &    162,  163,  164,  166,  173,  177,  180,  183,  185,  186, & ! 4   
     &    187,  188,  190,  191,  192,  195,  196,  206,  207,  208, & ! 5   
     &    221,  222,  223,  224,  225,  226,  227,  228,  229,  230, & ! 6   
     &    231,  232,  233,  234,  235,  236,  237,  238,  239,  240, & ! 7   
     &    241,  242,  243,  244,  245,  246/     !  8   

      INTEGER, PARAMETER :: KTN2 =   1
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &      2/

      INTEGER, PARAMETER :: KTN3 = 101
      INTEGER            :: KRX3( KTN3 )

      DATA ( KRX3( IRXXN ), IRXXN = 1, KTN3 ) / & 
     &      3,    4,    7,   10,   12,   13,   16,   17,   22,   26, & ! O   
     &     30,   33,   37,   39,   40,   41,   43,   44,   45,   50, & ! 1   
     &     54,   55,   56,   57,   61,   63,   66,   67,   68,   69, & ! 2   
     &     70,   72,   76,   78,   79,   81,   83,   84,   85,   87, & ! 3   
     &     91,   92,   93,   94,   95,   97,   98,   99,  102,  107, & ! 4   
     &    108,  109,  110,  111,  113,  116,  118,  119,  120,  122, & ! 5   
     &    123,  125,  126,  127,  128,  129,  130,  131,  139,  140, & ! 6   
     &    142,  145,  149,  150,  154,  155,  159,  160,  161,  167, & ! 7   
     &    168,  169,  171,  172,  176,  178,  179,  182,  184,  189, & ! 8   
     &    193,  199,  200,  201,  202,  203,  204,  205,  209,  210, & ! 9   
     &    211/     !  O   

      INTEGER, PARAMETER :: KTN4 =   1
      INTEGER            :: KRX4( KTN4 )

      DATA ( KRX4( IRXXN ), IRXXN = 1, KTN4 ) / & 
     &    194/

      INTEGER, PARAMETER :: KTN5 =   0
      INTEGER            :: KRX5( 1 )

      DATA   KRX5( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN6 =   0
      INTEGER            :: KRX6( 1 )

      DATA   KRX6( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN7 =   0
      INTEGER            :: KRX7( 1 )

      DATA   KRX7( 1 ) / 0 /

      INTEGER, PARAMETER :: NWM =   2
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &      2,   10/
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
     &     66,  182/
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
     &     50,   50,   52,    7,    7,    7,   43,    3,    7,    4, & ! 5   
     &      5,    7,    4,    5,   58,   60,   60,   60,   60,   62, & ! 6   
     &      7,    7,    1,   67,   69,   68,   70,   70,   70,    7, & ! 7   
     &     71,   68,   68,   68,   68,   68,   68,   68,   68,   68, & ! 8   
     &     68,   68,   68,   72,   68,   68,   68,   73,   41,   41, & ! 9   
     &     54,   54,   78,   79,   79,   82,   82,   82,   56,   56, & ! O   
     &     86,    9,    9,   88,   89,   88,   88,   89,    1,    4, & ! 1   
     &     93,   95,   96,   97,   98,   99,  100,  102,  103,  104, & ! 2   
     &    105,  106,  107,  108,  109,  110,  112,  113,  114,  115, & ! 3   
     &    116,  117,  118,  121,  119,  120/     !  4   

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
     &      2,    1,    0,   53,   55,   43,    0,   57,   57,   57, & ! 5   
     &     57,   58,   58,   58,    0,    3,    7,    4,    5,    7, & ! 6   
     &     65,   66,   57,    0,    0,    4,   70,    2,    8,   71, & ! 7   
     &      0,    0,   21,   66,   36,   35,   37,   57,   18,   19, & ! 8   
     &     20,   25,   65,    7,   38,   53,    1,    0,    2,    8, & ! 9   
     &      2,    8,    7,    2,    8,    4,    7,    5,    2,    8, & ! O   
     &      7,    0,    0,    0,    0,   90,   91,   92,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    7,    7,    7,    7,    7, & ! 3   
     &      7,    7,    7,    7,    7,    7/     !  4   

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
     &      0,    0,    0,    0,    0,    0/     !  4   

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
     &      1,   52,   50,    8,    8,   14,   28,   58,   58,   58, & ! 5   
     &     58,   21,   28,   20,   22,   20,    8,    7,    1,   63, & ! 6   
     &      8,   19,   58,   68,    7,   70,   67,   68,   69,   68, & ! 7   
     &     68,   72,   72,   72,   71,   71,   72,   72,   72,   72, & ! 8   
     &     72,   72,   72,   68,   72,   72,   73,   68,    2,    8, & ! 9   
     &      2,    8,    7,    2,    8,    4,    7,    5,    2,    8, & ! O   
     &      7,   10,   10,   10,   10,   73,   73,   73,   11,    0, & ! 1   
     &     94,   94,   94,   94,   94,   94,  101,  101,  101,  101, & ! 2   
     &    101,   94,   94,   94,   94,    7,    7,    7,    7,    7, & ! 3   
     &      7,    7,    7,    7,    7,    7/     !  4   

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
     &     14,    0,    1,   14,   14,   28,    8,   18,   18,   18, & ! 5   
     &     16,   18,   18,   18,   19,   21,   14,    8,    8,    8, & ! 6   
     &     19,   14,   16,    0,   68,    0,   68,    1,    0,   22, & ! 7   
     &     22,   23,   14,   19,   14,   19,   71,   14,    8,   28, & ! 8   
     &     32,    8,    8,    0,   14,   14,    0,    1,   74,   75, & ! 9   
     &     76,   77,   79,   80,   81,   83,   83,   83,   84,   85, & ! O   
     &     87,   88,   89,    0,    0,    0,    0,    0,   10,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,  111,  112,  112,  112,  112, & ! 3   
     &    112,  117,  117,  117,  117,  117/     !  4   

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
     &      8,    0,    0,   39,   39,    0,   22,   14,   14,   14, & ! 5   
     &     14,   14,   43,   21,   18,   61,   15,   14,   14,   64, & ! 6   
     &     20,   15,   14,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      8,    0,   15,   14,    8,   20,   19,    8,   22,    0, & ! 8   
     &      0,   18,   19,    0,    8,    8,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  113,  113,  113,  113, & ! 3   
     &    113,  118,  118,  118,  118,  118/     !  4   

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
     &     20,    0,    0,   43,   43,    0,    0,    8,    8,    8, & ! 5   
     &      8,    8,    8,    8,   21,    0,   18,   15,   15,    0, & ! 6   
     &     18,    8,    8,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    8,   15,   18,   14,   20,   71,    0,    0, & ! 8   
     &      0,    0,    0,    0,   15,   15,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  114,  114,  114,  114, & ! 3   
     &    114,  121,  121,  121,  121,  121/     !  4   

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
     &      0,    0,    0,   21,   21,    0,    0,   32,   15,    7, & ! 5   
     &      1,   22,    7,   22,    8,    0,   21,   18,   20,    0, & ! 6   
     &     14,    0,    2,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   19,    8,    0,    8,   35,   58,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  115,  115,  115,  115, & ! 3   
     &    117,  119,  119,  119,  119,  119/     !  4   

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
     &      0,    0,    0,   40,   40,    0,    0,   21,   59,   32, & ! 5   
     &     20,   43,   14,   16,   14,    0,   20,   22,   16,    0, & ! 6   
     &      0,    0,   20,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   21,    0,    0,   21,   21,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  116,  117,  117,  117, & ! 3   
     &    118,  120,  120,  120,  120,  120/     !  4   

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
     &      0,    0,    0,   54,   56,    0,    0,    0,    0,   20, & ! 5   
     &     21,   19,   19,   32,   28,    0,   61,   21,   61,    0, & ! 6   
     &      0,    0,   21,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   34,    0,    0,    0,   14,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  117,  118,  118,  118, & ! 3   
     &      0,    0,    0,    0,    0,    0/     !  4   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   21, & ! 5   
     &      0,   28,   21,   14,    0,    0,    0,   20,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   20,    0,    0,    0,    8,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  118,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0/     !  4   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   22, & ! 5   
     &      0,   32,   22,   10,    0,    0,    0,   32,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  119,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0/     !  4   

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
     &      0,   20,    0,    0,    0,    0,    0,   61,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,  120,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0/     !  4   

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
     &     1.1000D-11, 1.1000D-11, 1.0000D-04, 1.7000D-11, 1.7000D-11, & ! 5   
     &     1.8000D-11, 1.0000D+00, 3.6000D-11, 2.5400D-11, 7.8600D-15, & ! +   
     &     3.0300D-12, 3.3600D-11, 7.1000D-18, 1.0000D-15, 3.6000D-03, & ! 6   
     &     3.6000D-11, 1.5000D-11, 1.2000D-15, 3.7000D-12, 3.3000D-31, & ! +   
     &     6.9000D-12, 8.7000D-12, 1.5000D-19, 1.0000D+00, 1.0000D+00, & ! 7   
     &     2.3000D-11, 1.6300D-14, 6.4000D-12, 2.7000D-12, 5.0000D-13, & ! +   
     &     1.0000D+00, 6.6000D-12, 5.0000D-11, 8.3000D-11, 1.0700D-10, & ! 8   
     &     2.5000D-10, 3.5000D-10, 4.3000D-10, 8.2000D-11, 7.9000D-11, & ! +   
     &     1.3000D-10, 5.5000D-11, 8.2000D-11, 6.5800D-13, 6.1000D-11, & ! 9   
     &     1.2000D-10, 1.8000D-31, 1.0000D+00, 2.7000D-12, 1.9000D-13, & ! +   
     &     2.7000D-12, 1.9000D-13, 2.4700D-12, 2.7000D-12, 1.9000D-13, & ! O   
     &     1.1600D-14, 1.9700D-10, 1.9000D-11, 2.7000D-12, 1.9000D-13, & ! +   
     &     2.7000D-12, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D-40, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 2   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 3   
     &     1.2500D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, & ! +   
     &     4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, & ! 4   
     &     4.0000D-11/           !        +   

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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-4.3000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.1600D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00,-2.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.0582D-09, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00/           !        +   

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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.1600D+02, 1.1600D+02, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 4.0760D+02,-1.9120D+03, & ! +   
     &    -4.4800D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 4.4900D+02,-8.2100D+02, 1.7500D+02, 0.0000D+00, & ! +   
     &    -2.3000D+02,-1.0700D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &    -2.0000D+02, 0.0000D+00, 2.9000D+02, 2.2000D+02, 0.0000D+00, & ! +   
     &     0.0000D+00,-1.2400D+03, 0.0000D+00,-1.0000D+02, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-3.4000D+01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.5000D+01, 5.8000D+01, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.6000D+02, 1.3000D+03, & ! +   
     &     3.6000D+02, 1.3000D+03,-2.0600D+02, 3.6000D+02, 1.3000D+03, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.6000D+02, 1.3000D+03, & ! +   
     &     3.7400D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00/           !        +   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &      5,    6,   18,   21,   24,   28,   29,   31,   32,   34, & 
     &     35,   42,   65,   88,   89,  103,  104,  121,  170,  197, & 
     &    220/

      DATA ( RFDAT( 1,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     2.2000D-11, 3.0000D-11, 1.4000D-12, 9.7000D+14, 3.6000D-11, & 
     &     2.5000D-11, 2.1990D+03, 4.7000D-12, 4.8000D+15, 1.0000D+03, & 
     &     3.2000D+03, 2.6000D-11, 0.0000D+00, 1.2000D-11, 5.4000D+16, & 
     &     1.2000D-11, 5.4000D+16, 8.8000D-12, 1.6000D-12, 1.0000D-10, & 
     &     7.8426D+01/

      DATA ( RFDAT( 2,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &    -7.0000D-01, 0.0000D+00,-7.0000D-01, 1.0000D-01,-1.0000D-01, & 
     &     0.0000D+00, 6.5000D-34, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-9.0000D-01, 0.0000D+00, & 
     &    -9.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.0000D+00, & 
     &     5.8212D+00/

      DATA ( RFDAT( 3,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.1080D+04, 0.0000D+00, & 
     &     0.0000D+00, 1.3350D+03, 0.0000D+00,-1.1170D+04, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.3830D+04, & 
     &     0.0000D+00,-1.3830D+04, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00/

      DATA ( RFDAT( 4,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     6.0000D-01, 6.0000D-01, 6.0000D-01, 4.5000D-01, 6.0000D-01, & 
     &     6.0000D-01, 0.0000D+00, 6.0000D-01, 6.0000D-01, 0.0000D+00, & 
     &     0.0000D+00, 6.0000D-01, 0.0000D+00, 3.0000D-01, 3.0000D-01, & 
     &     3.0000D-01, 3.0000D-01, 6.0000D-01, 6.0000D-01, 6.0000D-01, & 
     &     0.0000D+00/

      DATA ( RFDAT( 5,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     0.0000D+00/

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
     &        1.00000,    1.00000,    1.00000,    0.70000,    0.70000, & ! 5   
     &        1.00000,    1.00000,    0.75000,    0.91200,    0.65000, & ! +   
     &        0.20000,    1.56500,    0.11400,    0.35700,    0.33300, & ! 6   
     &        0.15000,    0.75000,    0.57000,    0.47000,    1.00000, & ! +   
     &        1.00000,    0.99100,    0.20000,    2.00000,    1.00000, & ! 7   
     &        1.00000,    0.30000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    0.30000,    0.15000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    0.50000,    0.00000, & ! +   
     &        1.14280,    1.14280,    1.00000,    1.00000,    0.85714, & ! 2   
     &        0.85714,    1.00000,    1.00000,    0.50000,    0.50000, & ! +   
     &        1.50000,    1.42860,    1.42860,    1.71430,    1.71430, & ! 3   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000/           !         &  

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
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 5   
     &        1.00000,    1.00000,    0.50000,    0.62900,    0.60000, & ! +   
     &        0.80000,    0.16700,    0.15000,    0.28200,    0.06700, & ! 6   
     &        5.12000,    1.25000,    0.07000,    0.28000,    1.00000, & ! +   
     &        0.90000,    0.99100,    0.80000,    0.00000,    1.00000, & ! 7   
     &        0.00000,    1.40000,    1.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.87000,    0.99100,    2.00000, & ! 8   
     &        0.33000,    0.70000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.00000,    0.88000, & ! 9   
     &        0.84000,    0.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.47000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.50000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        1.00000,    0.48570,    0.30030,    0.38560,    0.21810, & ! +   
     &        0.24120,    0.66640,    0.28580,    0.33030,    0.34440, & ! 4   
     &        0.38860/           !         &  

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
     &        1.00000,    0.00000,    0.00000,    0.20000,    0.20000, & ! 5   
     &        0.00000,    1.00000,    0.25000,    0.99100,    0.20000, & ! +   
     &        1.00000,    0.71300,    0.85000,    1.28200,    0.90000, & ! 6   
     &        1.00000,    0.25000,    0.76000,    1.03000,    1.00000, & ! +   
     &        0.05000,    0.00900,    1.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.13000,    0.99100,    1.00000, & ! 8   
     &        0.67000,    0.45000,    1.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.88000, & ! 9   
     &        0.84000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00620,    0.28620,    0.09500,    0.30630, & ! +   
     &        0.20890,    0.01430,    0.39310,    0.22720,    0.27490, & ! 4   
     &        0.24210/           !         &  

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
     &        1.00000,    0.00000,    0.00000,    0.80000,    0.80000, & ! 5   
     &        0.00000,    0.00000,    0.25000,    0.91200,    0.06600, & ! +   
     &        0.80000,    0.50300,    0.15400,    0.92500,    0.83200, & ! 6   
     &        0.00000,    0.28000,    0.18000,    0.25000,    0.00000, & ! +   
     &        0.10000,    1.00000,    0.80000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.11000,    0.00900,    1.00000, & ! 8   
     &        2.00000,    0.55000,    0.85000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.12000, & ! 9   
     &        0.16000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00250,    0.00410,    0.13730,    0.01530, & ! +   
     &        0.30000,    0.01230,    0.01390,    0.26070,    0.04910, & ! 4   
     &        0.06400/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    1.10000,    1.10000, & ! 5   
     &        0.00000,    0.00000,    0.25000,    0.08800,    0.26600, & ! +   
     &        0.20000,    0.33400,    0.26800,    0.64300,    1.03300, & ! 6   
     &        0.00000,    1.66000,    0.24000,    0.47000,    0.00000, & ! +   
     &        0.10000,    0.00000,    0.20000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.06000,    1.00000,    0.00000, & ! 8   
     &        1.00000,    0.30000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00260,    0.00350,    0.00050,    0.10430, & ! +   
     &        0.20280,    0.12390,    0.10270,    0.07020,    0.25770, & ! 4   
     &        0.03850/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    0.30000,    0.30000, & ! 5   
     &        0.00000,    0.00000,    0.25000,    1.00000,    0.20000, & ! +   
     &        0.80000,    0.16800,    0.06400,    0.85000,    0.70000, & ! 6   
     &        0.00000,    0.47000,    0.00100,    0.53000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.80000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,   -0.11000,    0.00000,    0.00000, & ! 8   
     &       -1.00000,    0.30000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00230,    0.22390,    0.20510,    0.18930, & ! +   
     &        0.04710,    0.18310,    0.20450,    0.11160,    0.07390, & ! 4   
     &        0.26670/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.15000, & ! +   
     &        2.40000,    0.25200,    0.02000,    0.07500,    0.96700, & ! 6   
     &        0.00000,    1.00000,    7.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    2.40000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.76000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    1.70000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.29440,    0.18200,    0.17640,    0.16680, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.35000, & ! +   
     &        0.00000,    0.21000,    0.36000,    0.07500,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.21000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.05000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.20210,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.06600, & ! +   
     &        0.00000,    0.25000,    0.22500,    0.15000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.39000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00190,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000/           !         &  

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
     &        0.00000,    0.12000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00230,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000/           !         &  

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
     &      2,    2,    1,    2,    2,    2,    1,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    2, & ! 7   
     &      1,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    1,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    1,    1,    1,    1,    2,    2,    2,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2/     !  4   
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
     &      4,    1,    2,    7,    7,    2,    3,    6,    6,    9, & ! 5   
     &      7,   10,    9,    9,    7,    3,    7,   10,    7,    3, & ! 6   
     &      5,    4,    7,    1,    2,    1,    2,    2,    1,    2, & ! 7   
     &      3,    2,    8,    5,    4,    6,    8,    5,    3,    2, & ! 8   
     &      2,    3,    3,    1,    4,    4,    1,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    1,    1,    1,    1,    1,    2,    0, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    2,   10,    7,    7,    7, & ! 3   
     &      6,    6,    6,    6,    6,    6/     !  4   

      INTEGER, PARAMETER :: NMPHOT =  28
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    8,    9,   14,   15,   25,   36,   51,   52,   53, & 
     &     62,   64,   71,   74,   75,   86,   90,   96,  101,  105, & 
     &    141,  143,  157,  165,  174,  175,  181,  198/

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   12,   13,   14,   15,   16,   17,   18,   16, & 
     &      1,    1,   19,   20,   21,   22,   23,   24/

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28/

      INTEGER, PARAMETER :: MHETERO =   8
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    212,  213,  214,  215,  216,  217,  218,  219/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    2,    3,    4,    5,    5,    6,    7/

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

      INTEGER, PARAMETER :: NHETERO =   7
      CHARACTER( 16 )    :: HETERO( NHETERO )

      DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & 
     &   'HETERO_N2O5IJ   ', 'HETERO_N2O5K    ', 'HETERO_H2NO3PAIJ', &
     &   'HETERO_H2NO3PAK ', 'HETERO_H2NO3PBIJ', 'HETERO_H2NO3PBK ', &
     &   'HETERO_NO2      '/

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
     &    'R154a           ', 'R154b           ', 'R155            ', & ! 1   
     &    'R156            ', 'R157            ', 'R158            ', & ! 2   
     &    'R159            ', 'R160            ', 'R161            ', & ! 3   
     &    'R162            ', 'R163            ', 'R164            ', & ! 4   
     &    'R165            ', 'R166            ', 'R167            ', & ! 5   
     &    'R168            ', 'R169            ', 'R170            ', & ! 6   
     &    'R171            ', 'R172            ', 'CL1             ', & ! 7   
     &    'CL2             ', 'CL3             ', 'CL4             ', & ! 8   
     &    'CL5             ', 'CL6             ', 'CL7             ', & ! 9   
     &    'CL8             ', 'CL9             ', 'CL10            ', & ! 0   
     &    'CL11            ', 'CL12            ', 'CL13            ', & ! 1   
     &    'CL14            ', 'CL15            ', 'CL16            ', & ! 2   
     &    'CL17            ', 'CL18            ', 'CL19            ', & ! 3   
     &    'CL20            ', 'CL21            ', 'CL22            ', & ! 4   
     &    'CL23            ', 'CL24            ', 'CL25            ', & ! 5   
     &    'SA01            ', 'SA02            ', 'SA03            ', & ! 6   
     &    'SA04            ', 'SA05            ', 'SA06            ', & ! 7   
     &    'SA07            ', 'SA08            ', 'SA09            ', & ! 8   
     &    'SA10            ', 'SA11            ', 'SA12            ', & ! 9   
     &    'SA13            ', 'HET_N2O5IJ      ', 'HET_N2O5K       ', & ! 0   
     &    'HET_H2NO3PIJA   ', 'HET_H2NO3PKA    ', 'HET_H2NO3PIB    ', & ! 1   
     &    'HET_H2NO3PJB    ', 'HET_H2NO3PKB    ', 'HET_N02         ', & ! 2   
     &    'HAL_Ozone       ', 'OLIG_XYLENE1    ', 'OLIG_XYLENE2    ', & ! 3   
     &    'OLIG_TOLUENE1   ', 'OLIG_TOLUENE2   ', 'OLIG_BENZENE1   ', & ! 4   
     &    'OLIG_BENZENE2   ', 'OLIG_TERPENE1   ', 'OLIG_TERPENE2   ', & ! 5   
     &    'OLIG_ISOPRENE1  ', 'OLIG_ISOPRENE2  ', 'OLIG_SESQT1     ', & ! 6   
     &    'OLIG_PAH1       ', 'OLIG_PAH2       ', 'OLIG_ALK1       ', & ! 7   
     &    'OLIG_ALK2       ', 'PCSOA           ', 'POA_AGE1        ', & ! 8   
     &    'POA_AGE2        ', 'POA_AGE3        ', 'POA_AGE4        ', & ! 9   
     &    'POA_AGE5        ', 'POA_AGE6        ', 'POA_AGE7        ', & ! 0   
     &    'POA_AGE8        ', 'POA_AGE9        ', 'POA_AGE10       '/!    

!    NSPECIAL     = Number of special rate coefficients
!    SPECIAL      = Names of special rate coefficients
!    NSPECIAL_RXN = Number of reactions with special rates
!    ISPECIAL     = Pointers to reactions using special rates and their special rate coefficients
!    MAXSPECTERMS = Max Number of terms type used by special rate coefficients
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

      INTEGER, PARAMETER :: MAXSPECTERMS =   1
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
       END MODULE RXNS_DATA
