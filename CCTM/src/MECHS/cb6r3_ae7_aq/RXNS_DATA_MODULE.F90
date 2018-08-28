       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: /home/ixk/v52/git_v53b/CCTM/src/MECHS/cb6r3_ae7_aq/mech_cb6r3_ae7_aq.def
! for Mechanism Name: CB6R3_AE7_AQ                    

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

      CHARACTER( 32 ), PARAMETER :: MECHNAME = 'CB6R3_AE7_AQ'

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC = 126
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 141

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
      DATA GAS_CHEM_SPC(   9 ) / 'H2O2            ' /
      DATA GAS_CHEM_SPC(  10 ) / 'N2O5            ' /
      DATA GAS_CHEM_SPC(  11 ) / 'HNO3            ' /
      DATA GAS_CHEM_SPC(  12 ) / 'HONO            ' /
      DATA GAS_CHEM_SPC(  13 ) / 'PNA             ' /
      DATA GAS_CHEM_SPC(  14 ) / 'SO2             ' /
      DATA GAS_CHEM_SPC(  15 ) / 'SULF            ' /
      DATA GAS_CHEM_SPC(  16 ) / 'SULRXN          ' /
      DATA GAS_CHEM_SPC(  17 ) / 'C2O3            ' /
      DATA GAS_CHEM_SPC(  18 ) / 'MEO2            ' /
      DATA GAS_CHEM_SPC(  19 ) / 'RO2             ' /
      DATA GAS_CHEM_SPC(  20 ) / 'PAN             ' /
      DATA GAS_CHEM_SPC(  21 ) / 'PACD            ' /
      DATA GAS_CHEM_SPC(  22 ) / 'AACD            ' /
      DATA GAS_CHEM_SPC(  23 ) / 'CXO3            ' /
      DATA GAS_CHEM_SPC(  24 ) / 'ALD2            ' /
      DATA GAS_CHEM_SPC(  25 ) / 'XO2H            ' /
      DATA GAS_CHEM_SPC(  26 ) / 'PANX            ' /
      DATA GAS_CHEM_SPC(  27 ) / 'FORM            ' /
      DATA GAS_CHEM_SPC(  28 ) / 'MEPX            ' /
      DATA GAS_CHEM_SPC(  29 ) / 'MEOH            ' /
      DATA GAS_CHEM_SPC(  30 ) / 'ROOH            ' /
      DATA GAS_CHEM_SPC(  31 ) / 'XO2             ' /
      DATA GAS_CHEM_SPC(  32 ) / 'XO2N            ' /
      DATA GAS_CHEM_SPC(  33 ) / 'NTR1            ' /
      DATA GAS_CHEM_SPC(  34 ) / 'NTR2            ' /
      DATA GAS_CHEM_SPC(  35 ) / 'FACD            ' /
      DATA GAS_CHEM_SPC(  36 ) / 'CO              ' /
      DATA GAS_CHEM_SPC(  37 ) / 'HCO3            ' /
      DATA GAS_CHEM_SPC(  38 ) / 'ALDX            ' /
      DATA GAS_CHEM_SPC(  39 ) / 'GLYD            ' /
      DATA GAS_CHEM_SPC(  40 ) / 'GLY             ' /
      DATA GAS_CHEM_SPC(  41 ) / 'MGLY            ' /
      DATA GAS_CHEM_SPC(  42 ) / 'ETHA            ' /
      DATA GAS_CHEM_SPC(  43 ) / 'ETOH            ' /
      DATA GAS_CHEM_SPC(  44 ) / 'KET             ' /
      DATA GAS_CHEM_SPC(  45 ) / 'PAR             ' /
      DATA GAS_CHEM_SPC(  46 ) / 'ACET            ' /
      DATA GAS_CHEM_SPC(  47 ) / 'PRPA            ' /
      DATA GAS_CHEM_SPC(  48 ) / 'XPRP            ' /
      DATA GAS_CHEM_SPC(  49 ) / 'XPAR            ' /
      DATA GAS_CHEM_SPC(  50 ) / 'ROR             ' /
      DATA GAS_CHEM_SPC(  51 ) / 'ETHY            ' /
      DATA GAS_CHEM_SPC(  52 ) / 'ETH             ' /
      DATA GAS_CHEM_SPC(  53 ) / 'OLE             ' /
      DATA GAS_CHEM_SPC(  54 ) / 'IOLE            ' /
      DATA GAS_CHEM_SPC(  55 ) / 'ISOP            ' /
      DATA GAS_CHEM_SPC(  56 ) / 'ISO2            ' /
      DATA GAS_CHEM_SPC(  57 ) / 'ISOPRXN         ' /
      DATA GAS_CHEM_SPC(  58 ) / 'ISPD            ' /
      DATA GAS_CHEM_SPC(  59 ) / 'INTR            ' /
      DATA GAS_CHEM_SPC(  60 ) / 'ISPX            ' /
      DATA GAS_CHEM_SPC(  61 ) / 'HPLD            ' /
      DATA GAS_CHEM_SPC(  62 ) / 'OPO3            ' /
      DATA GAS_CHEM_SPC(  63 ) / 'EPOX            ' /
      DATA GAS_CHEM_SPC(  64 ) / 'EPX2            ' /
      DATA GAS_CHEM_SPC(  65 ) / 'TERP            ' /
      DATA GAS_CHEM_SPC(  66 ) / 'TRPRXN          ' /
      DATA GAS_CHEM_SPC(  67 ) / 'TERPNRO2        ' /
      DATA GAS_CHEM_SPC(  68 ) / 'APIN            ' /
      DATA GAS_CHEM_SPC(  69 ) / 'BENZENE         ' /
      DATA GAS_CHEM_SPC(  70 ) / 'CRES            ' /
      DATA GAS_CHEM_SPC(  71 ) / 'BZO2            ' /
      DATA GAS_CHEM_SPC(  72 ) / 'OPEN            ' /
      DATA GAS_CHEM_SPC(  73 ) / 'BENZRO2         ' /
      DATA GAS_CHEM_SPC(  74 ) / 'TOL             ' /
      DATA GAS_CHEM_SPC(  75 ) / 'TO2             ' /
      DATA GAS_CHEM_SPC(  76 ) / 'TOLRO2          ' /
      DATA GAS_CHEM_SPC(  77 ) / 'XOPN            ' /
      DATA GAS_CHEM_SPC(  78 ) / 'XYLMN           ' /
      DATA GAS_CHEM_SPC(  79 ) / 'XLO2            ' /
      DATA GAS_CHEM_SPC(  80 ) / 'XYLRO2          ' /
      DATA GAS_CHEM_SPC(  81 ) / 'NAPH            ' /
      DATA GAS_CHEM_SPC(  82 ) / 'PAHRO2          ' /
      DATA GAS_CHEM_SPC(  83 ) / 'CRO             ' /
      DATA GAS_CHEM_SPC(  84 ) / 'CAT1            ' /
      DATA GAS_CHEM_SPC(  85 ) / 'CRON            ' /
      DATA GAS_CHEM_SPC(  86 ) / 'OPAN            ' /
      DATA GAS_CHEM_SPC(  87 ) / 'ECH4            ' /
      DATA GAS_CHEM_SPC(  88 ) / 'CL2             ' /
      DATA GAS_CHEM_SPC(  89 ) / 'CL              ' /
      DATA GAS_CHEM_SPC(  90 ) / 'HOCL            ' /
      DATA GAS_CHEM_SPC(  91 ) / 'CLO             ' /
      DATA GAS_CHEM_SPC(  92 ) / 'FMCL            ' /
      DATA GAS_CHEM_SPC(  93 ) / 'HCL             ' /
      DATA GAS_CHEM_SPC(  94 ) / 'CLNO2           ' /
      DATA GAS_CHEM_SPC(  95 ) / 'CLNO3           ' /
      DATA GAS_CHEM_SPC(  96 ) / 'SVAVB2          ' /
      DATA GAS_CHEM_SPC(  97 ) / 'SVAVB3          ' /
      DATA GAS_CHEM_SPC(  98 ) / 'SVAVB4          ' /
      DATA GAS_CHEM_SPC(  99 ) / 'SVAVB1          ' /
      DATA GAS_CHEM_SPC( 100 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC( 101 ) / 'SESQRXN         ' /
      DATA GAS_CHEM_SPC( 102 ) / 'SOAALK          ' /
      DATA GAS_CHEM_SPC( 103 ) / 'H2NO3PIJ        ' /
      DATA GAS_CHEM_SPC( 104 ) / 'H2NO3PK         ' /
      DATA GAS_CHEM_SPC( 105 ) / 'MTNO3           ' /
      DATA GAS_CHEM_SPC( 106 ) / 'PCVOC           ' /
      DATA GAS_CHEM_SPC( 107 ) / 'PCSOARXN        ' /
      DATA GAS_CHEM_SPC( 108 ) / 'VLVPO1          ' /
      DATA GAS_CHEM_SPC( 109 ) / 'VSVPO1          ' /
      DATA GAS_CHEM_SPC( 110 ) / 'VSVPO2          ' /
      DATA GAS_CHEM_SPC( 111 ) / 'VSVPO3          ' /
      DATA GAS_CHEM_SPC( 112 ) / 'VIVPO1          ' /
      DATA GAS_CHEM_SPC( 113 ) / 'VLVOO1          ' /
      DATA GAS_CHEM_SPC( 114 ) / 'VLVOO2          ' /
      DATA GAS_CHEM_SPC( 115 ) / 'VSVOO2          ' /
      DATA GAS_CHEM_SPC( 116 ) / 'VSVOO3          ' /
      DATA GAS_CHEM_SPC( 117 ) / 'VSVOO1          ' /
      DATA GAS_CHEM_SPC( 118 ) / 'FORM_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 119 ) / 'ALD2_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 120 ) / 'BUTADIENE13     ' /
      DATA GAS_CHEM_SPC( 121 ) / 'ACROLEIN        ' /
      DATA GAS_CHEM_SPC( 122 ) / 'ACRO_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 123 ) / 'TOLU            ' /
      DATA GAS_CHEM_SPC( 124 ) / 'HG              ' /
      DATA GAS_CHEM_SPC( 125 ) / 'HGIIAER         ' /
      DATA GAS_CHEM_SPC( 126 ) / 'HGIIGAS         ' /




      LOGICAL   :: HALOGEN_PARAMETER = .TRUE. 

      DATA CHEMISTRY_SPC(   1 ), SPECIES_MOLWT(   1 ) / 'NO2             ',   46.00 /
      DATA CHEMISTRY_SPC(   2 ), SPECIES_MOLWT(   2 ) / 'NO              ',   30.00 /
      DATA CHEMISTRY_SPC(   3 ), SPECIES_MOLWT(   3 ) / 'O               ',   16.00 /
      DATA CHEMISTRY_SPC(   4 ), SPECIES_MOLWT(   4 ) / 'O3              ',   48.00 /
      DATA CHEMISTRY_SPC(   5 ), SPECIES_MOLWT(   5 ) / 'NO3             ',   62.00 /
      DATA CHEMISTRY_SPC(   6 ), SPECIES_MOLWT(   6 ) / 'O1D             ',   16.00 /
      DATA CHEMISTRY_SPC(   7 ), SPECIES_MOLWT(   7 ) / 'OH              ',   17.00 /
      DATA CHEMISTRY_SPC(   8 ), SPECIES_MOLWT(   8 ) / 'HO2             ',   33.00 /
      DATA CHEMISTRY_SPC(   9 ), SPECIES_MOLWT(   9 ) / 'H2O2            ',   34.00 /
      DATA CHEMISTRY_SPC(  10 ), SPECIES_MOLWT(  10 ) / 'N2O5            ',  108.00 /
      DATA CHEMISTRY_SPC(  11 ), SPECIES_MOLWT(  11 ) / 'HNO3            ',   63.00 /
      DATA CHEMISTRY_SPC(  12 ), SPECIES_MOLWT(  12 ) / 'HONO            ',   47.00 /
      DATA CHEMISTRY_SPC(  13 ), SPECIES_MOLWT(  13 ) / 'PNA             ',   79.00 /
      DATA CHEMISTRY_SPC(  14 ), SPECIES_MOLWT(  14 ) / 'SO2             ',   64.00 /
      DATA CHEMISTRY_SPC(  15 ), SPECIES_MOLWT(  15 ) / 'SULF            ',   98.00 /
      DATA CHEMISTRY_SPC(  16 ), SPECIES_MOLWT(  16 ) / 'SULRXN          ',   98.00 /
      DATA CHEMISTRY_SPC(  17 ), SPECIES_MOLWT(  17 ) / 'C2O3            ',   75.00 /
      DATA CHEMISTRY_SPC(  18 ), SPECIES_MOLWT(  18 ) / 'MEO2            ',   47.00 /
      DATA CHEMISTRY_SPC(  19 ), SPECIES_MOLWT(  19 ) / 'RO2             ',   87.10 /
      DATA CHEMISTRY_SPC(  20 ), SPECIES_MOLWT(  20 ) / 'PAN             ',  121.00 /
      DATA CHEMISTRY_SPC(  21 ), SPECIES_MOLWT(  21 ) / 'PACD            ',   76.00 /
      DATA CHEMISTRY_SPC(  22 ), SPECIES_MOLWT(  22 ) / 'AACD            ',   60.00 /
      DATA CHEMISTRY_SPC(  23 ), SPECIES_MOLWT(  23 ) / 'CXO3            ',   89.00 /
      DATA CHEMISTRY_SPC(  24 ), SPECIES_MOLWT(  24 ) / 'ALD2            ',   44.00 /
      DATA CHEMISTRY_SPC(  25 ), SPECIES_MOLWT(  25 ) / 'XO2H            ',   87.10 /
      DATA CHEMISTRY_SPC(  26 ), SPECIES_MOLWT(  26 ) / 'PANX            ',  135.00 /
      DATA CHEMISTRY_SPC(  27 ), SPECIES_MOLWT(  27 ) / 'FORM            ',   30.00 /
      DATA CHEMISTRY_SPC(  28 ), SPECIES_MOLWT(  28 ) / 'MEPX            ',   48.00 /
      DATA CHEMISTRY_SPC(  29 ), SPECIES_MOLWT(  29 ) / 'MEOH            ',   32.00 /
      DATA CHEMISTRY_SPC(  30 ), SPECIES_MOLWT(  30 ) / 'ROOH            ',   90.10 /
      DATA CHEMISTRY_SPC(  31 ), SPECIES_MOLWT(  31 ) / 'XO2             ',   87.10 /
      DATA CHEMISTRY_SPC(  32 ), SPECIES_MOLWT(  32 ) / 'XO2N            ',   87.10 /
      DATA CHEMISTRY_SPC(  33 ), SPECIES_MOLWT(  33 ) / 'NTR1            ',  119.10 /
      DATA CHEMISTRY_SPC(  34 ), SPECIES_MOLWT(  34 ) / 'NTR2            ',  135.10 /
      DATA CHEMISTRY_SPC(  35 ), SPECIES_MOLWT(  35 ) / 'FACD            ',   46.00 /
      DATA CHEMISTRY_SPC(  36 ), SPECIES_MOLWT(  36 ) / 'CO              ',   28.00 /
      DATA CHEMISTRY_SPC(  37 ), SPECIES_MOLWT(  37 ) / 'HCO3            ',   63.00 /
      DATA CHEMISTRY_SPC(  38 ), SPECIES_MOLWT(  38 ) / 'ALDX            ',   58.10 /
      DATA CHEMISTRY_SPC(  39 ), SPECIES_MOLWT(  39 ) / 'GLYD            ',   60.00 /
      DATA CHEMISTRY_SPC(  40 ), SPECIES_MOLWT(  40 ) / 'GLY             ',   58.00 /
      DATA CHEMISTRY_SPC(  41 ), SPECIES_MOLWT(  41 ) / 'MGLY            ',   72.00 /
      DATA CHEMISTRY_SPC(  42 ), SPECIES_MOLWT(  42 ) / 'ETHA            ',   30.10 /
      DATA CHEMISTRY_SPC(  43 ), SPECIES_MOLWT(  43 ) / 'ETOH            ',   46.10 /
      DATA CHEMISTRY_SPC(  44 ), SPECIES_MOLWT(  44 ) / 'KET             ',   72.10 /
      DATA CHEMISTRY_SPC(  45 ), SPECIES_MOLWT(  45 ) / 'PAR             ',   14.00 /
      DATA CHEMISTRY_SPC(  46 ), SPECIES_MOLWT(  46 ) / 'ACET            ',   58.10 /
      DATA CHEMISTRY_SPC(  47 ), SPECIES_MOLWT(  47 ) / 'PRPA            ',   44.10 /
      DATA CHEMISTRY_SPC(  48 ), SPECIES_MOLWT(  48 ) / 'XPRP            ',   89.10 /
      DATA CHEMISTRY_SPC(  49 ), SPECIES_MOLWT(  49 ) / 'XPAR            ',   45.00 /
      DATA CHEMISTRY_SPC(  50 ), SPECIES_MOLWT(  50 ) / 'ROR             ',   29.00 /
      DATA CHEMISTRY_SPC(  51 ), SPECIES_MOLWT(  51 ) / 'ETHY            ',   26.00 /
      DATA CHEMISTRY_SPC(  52 ), SPECIES_MOLWT(  52 ) / 'ETH             ',   28.00 /
      DATA CHEMISTRY_SPC(  53 ), SPECIES_MOLWT(  53 ) / 'OLE             ',   42.10 /
      DATA CHEMISTRY_SPC(  54 ), SPECIES_MOLWT(  54 ) / 'IOLE            ',   56.10 /
      DATA CHEMISTRY_SPC(  55 ), SPECIES_MOLWT(  55 ) / 'ISOP            ',   68.10 /
      DATA CHEMISTRY_SPC(  56 ), SPECIES_MOLWT(  56 ) / 'ISO2            ',  117.10 /
      DATA CHEMISTRY_SPC(  57 ), SPECIES_MOLWT(  57 ) / 'ISOPRXN         ',   68.10 /
      DATA CHEMISTRY_SPC(  58 ), SPECIES_MOLWT(  58 ) / 'ISPD            ',   70.10 /
      DATA CHEMISTRY_SPC(  59 ), SPECIES_MOLWT(  59 ) / 'INTR            ',  147.10 /
      DATA CHEMISTRY_SPC(  60 ), SPECIES_MOLWT(  60 ) / 'ISPX            ',  118.10 /
      DATA CHEMISTRY_SPC(  61 ), SPECIES_MOLWT(  61 ) / 'HPLD            ',  116.10 /
      DATA CHEMISTRY_SPC(  62 ), SPECIES_MOLWT(  62 ) / 'OPO3            ',  115.00 /
      DATA CHEMISTRY_SPC(  63 ), SPECIES_MOLWT(  63 ) / 'EPOX            ',  118.10 /
      DATA CHEMISTRY_SPC(  64 ), SPECIES_MOLWT(  64 ) / 'EPX2            ',  149.10 /
      DATA CHEMISTRY_SPC(  65 ), SPECIES_MOLWT(  65 ) / 'TERP            ',  136.20 /
      DATA CHEMISTRY_SPC(  66 ), SPECIES_MOLWT(  66 ) / 'TRPRXN          ',  136.20 /
      DATA CHEMISTRY_SPC(  67 ), SPECIES_MOLWT(  67 ) / 'TERPNRO2        ',  197.00 /
      DATA CHEMISTRY_SPC(  68 ), SPECIES_MOLWT(  68 ) / 'APIN            ',  136.20 /
      DATA CHEMISTRY_SPC(  69 ), SPECIES_MOLWT(  69 ) / 'BENZENE         ',   78.10 /
      DATA CHEMISTRY_SPC(  70 ), SPECIES_MOLWT(  70 ) / 'CRES            ',  108.10 /
      DATA CHEMISTRY_SPC(  71 ), SPECIES_MOLWT(  71 ) / 'BZO2            ',  159.10 /
      DATA CHEMISTRY_SPC(  72 ), SPECIES_MOLWT(  72 ) / 'OPEN            ',   84.00 /
      DATA CHEMISTRY_SPC(  73 ), SPECIES_MOLWT(  73 ) / 'BENZRO2         ',  127.00 /
      DATA CHEMISTRY_SPC(  74 ), SPECIES_MOLWT(  74 ) / 'TOL             ',   92.10 /
      DATA CHEMISTRY_SPC(  75 ), SPECIES_MOLWT(  75 ) / 'TO2             ',  173.10 /
      DATA CHEMISTRY_SPC(  76 ), SPECIES_MOLWT(  76 ) / 'TOLRO2          ',  141.00 /
      DATA CHEMISTRY_SPC(  77 ), SPECIES_MOLWT(  77 ) / 'XOPN            ',   98.10 /
      DATA CHEMISTRY_SPC(  78 ), SPECIES_MOLWT(  78 ) / 'XYLMN           ',  106.20 /
      DATA CHEMISTRY_SPC(  79 ), SPECIES_MOLWT(  79 ) / 'XLO2            ',  187.10 /
      DATA CHEMISTRY_SPC(  80 ), SPECIES_MOLWT(  80 ) / 'XYLRO2          ',  155.00 /
      DATA CHEMISTRY_SPC(  81 ), SPECIES_MOLWT(  81 ) / 'NAPH            ',  128.20 /
      DATA CHEMISTRY_SPC(  82 ), SPECIES_MOLWT(  82 ) / 'PAHRO2          ',  187.20 /
      DATA CHEMISTRY_SPC(  83 ), SPECIES_MOLWT(  83 ) / 'CRO             ',  107.10 /
      DATA CHEMISTRY_SPC(  84 ), SPECIES_MOLWT(  84 ) / 'CAT1            ',  124.10 /
      DATA CHEMISTRY_SPC(  85 ), SPECIES_MOLWT(  85 ) / 'CRON            ',  153.10 /
      DATA CHEMISTRY_SPC(  86 ), SPECIES_MOLWT(  86 ) / 'OPAN            ',  161.00 /
      DATA CHEMISTRY_SPC(  87 ), SPECIES_MOLWT(  87 ) / 'ECH4            ',   16.00 /
      DATA CHEMISTRY_SPC(  88 ), SPECIES_MOLWT(  88 ) / 'CL2             ',   71.00 /
      DATA CHEMISTRY_SPC(  89 ), SPECIES_MOLWT(  89 ) / 'CL              ',   35.50 /
      DATA CHEMISTRY_SPC(  90 ), SPECIES_MOLWT(  90 ) / 'HOCL            ',   52.50 /
      DATA CHEMISTRY_SPC(  91 ), SPECIES_MOLWT(  91 ) / 'CLO             ',   51.50 /
      DATA CHEMISTRY_SPC(  92 ), SPECIES_MOLWT(  92 ) / 'FMCL            ',   64.50 /
      DATA CHEMISTRY_SPC(  93 ), SPECIES_MOLWT(  93 ) / 'HCL             ',   36.50 /
      DATA CHEMISTRY_SPC(  94 ), SPECIES_MOLWT(  94 ) / 'CLNO2           ',   81.50 /
      DATA CHEMISTRY_SPC(  95 ), SPECIES_MOLWT(  95 ) / 'CLNO3           ',   97.50 /
      DATA CHEMISTRY_SPC(  96 ), SPECIES_MOLWT(  96 ) / 'SVAVB2          ',  179.00 /
      DATA CHEMISTRY_SPC(  97 ), SPECIES_MOLWT(  97 ) / 'SVAVB3          ',  169.00 /
      DATA CHEMISTRY_SPC(  98 ), SPECIES_MOLWT(  98 ) / 'SVAVB4          ',  158.00 /
      DATA CHEMISTRY_SPC(  99 ), SPECIES_MOLWT(  99 ) / 'SVAVB1          ',  198.00 /
      DATA CHEMISTRY_SPC( 100 ), SPECIES_MOLWT( 100 ) / 'SESQ            ',  204.00 /
      DATA CHEMISTRY_SPC( 101 ), SPECIES_MOLWT( 101 ) / 'SESQRXN         ',  204.00 /
      DATA CHEMISTRY_SPC( 102 ), SPECIES_MOLWT( 102 ) / 'SOAALK          ',  112.00 /
      DATA CHEMISTRY_SPC( 103 ), SPECIES_MOLWT( 103 ) / 'H2NO3PIJ        ',   64.00 /
      DATA CHEMISTRY_SPC( 104 ), SPECIES_MOLWT( 104 ) / 'H2NO3PK         ',   64.00 /
      DATA CHEMISTRY_SPC( 105 ), SPECIES_MOLWT( 105 ) / 'ACLI            ',   35.50 /
      DATA CHEMISTRY_SPC( 106 ), SPECIES_MOLWT( 106 ) / 'ACLJ            ',   35.50 /
      DATA CHEMISTRY_SPC( 107 ), SPECIES_MOLWT( 107 ) / 'ACLK            ',   35.50 /
      DATA CHEMISTRY_SPC( 108 ), SPECIES_MOLWT( 108 ) / 'AISO3J          ',  168.20 /
      DATA CHEMISTRY_SPC( 109 ), SPECIES_MOLWT( 109 ) / 'AGLYJ           ',   66.40 /
      DATA CHEMISTRY_SPC( 110 ), SPECIES_MOLWT( 110 ) / 'MTNO3           ',  231.00 /
      DATA CHEMISTRY_SPC( 111 ), SPECIES_MOLWT( 111 ) / 'AMTNO3J         ',  231.00 /
      DATA CHEMISTRY_SPC( 112 ), SPECIES_MOLWT( 112 ) / 'AMTHYDJ         ',  168.00 /
      DATA CHEMISTRY_SPC( 113 ), SPECIES_MOLWT( 113 ) / 'AAVB2J          ',  179.00 /
      DATA CHEMISTRY_SPC( 114 ), SPECIES_MOLWT( 114 ) / 'AOLGAJ          ',  206.00 /
      DATA CHEMISTRY_SPC( 115 ), SPECIES_MOLWT( 115 ) / 'AAVB3J          ',  169.00 /
      DATA CHEMISTRY_SPC( 116 ), SPECIES_MOLWT( 116 ) / 'AAVB4J          ',  158.00 /
      DATA CHEMISTRY_SPC( 117 ), SPECIES_MOLWT( 117 ) / 'AISO1J          ',  132.00 /
      DATA CHEMISTRY_SPC( 118 ), SPECIES_MOLWT( 118 ) / 'AOLGBJ          ',  248.00 /
      DATA CHEMISTRY_SPC( 119 ), SPECIES_MOLWT( 119 ) / 'AISO2J          ',  133.00 /
      DATA CHEMISTRY_SPC( 120 ), SPECIES_MOLWT( 120 ) / 'ASQTJ           ',  273.00 /
      DATA CHEMISTRY_SPC( 121 ), SPECIES_MOLWT( 121 ) / 'PCVOC           ',  170.00 /
      DATA CHEMISTRY_SPC( 122 ), SPECIES_MOLWT( 122 ) / 'PCSOARXN        ',  170.00 /
      DATA CHEMISTRY_SPC( 123 ), SPECIES_MOLWT( 123 ) / 'VLVPO1          ',  218.00 /
      DATA CHEMISTRY_SPC( 124 ), SPECIES_MOLWT( 124 ) / 'VSVPO1          ',  230.00 /
      DATA CHEMISTRY_SPC( 125 ), SPECIES_MOLWT( 125 ) / 'VSVPO2          ',  241.00 /
      DATA CHEMISTRY_SPC( 126 ), SPECIES_MOLWT( 126 ) / 'VSVPO3          ',  253.00 /
      DATA CHEMISTRY_SPC( 127 ), SPECIES_MOLWT( 127 ) / 'VIVPO1          ',  266.00 /
      DATA CHEMISTRY_SPC( 128 ), SPECIES_MOLWT( 128 ) / 'VLVOO1          ',  136.00 /
      DATA CHEMISTRY_SPC( 129 ), SPECIES_MOLWT( 129 ) / 'VLVOO2          ',  136.00 /
      DATA CHEMISTRY_SPC( 130 ), SPECIES_MOLWT( 130 ) / 'VSVOO2          ',  135.00 /
      DATA CHEMISTRY_SPC( 131 ), SPECIES_MOLWT( 131 ) / 'VSVOO3          ',  134.00 /
      DATA CHEMISTRY_SPC( 132 ), SPECIES_MOLWT( 132 ) / 'VSVOO1          ',  135.00 /
      DATA CHEMISTRY_SPC( 133 ), SPECIES_MOLWT( 133 ) / 'FORM_PRIMARY    ',   30.00 /
      DATA CHEMISTRY_SPC( 134 ), SPECIES_MOLWT( 134 ) / 'ALD2_PRIMARY    ',   44.00 /
      DATA CHEMISTRY_SPC( 135 ), SPECIES_MOLWT( 135 ) / 'BUTADIENE13     ',   54.00 /
      DATA CHEMISTRY_SPC( 136 ), SPECIES_MOLWT( 136 ) / 'ACROLEIN        ',   56.10 /
      DATA CHEMISTRY_SPC( 137 ), SPECIES_MOLWT( 137 ) / 'ACRO_PRIMARY    ',   56.10 /
      DATA CHEMISTRY_SPC( 138 ), SPECIES_MOLWT( 138 ) / 'TOLU            ',   92.00 /
      DATA CHEMISTRY_SPC( 139 ), SPECIES_MOLWT( 139 ) / 'HG              ',  200.60 /
      DATA CHEMISTRY_SPC( 140 ), SPECIES_MOLWT( 140 ) / 'HGIIAER         ',  200.60 /
      DATA CHEMISTRY_SPC( 141 ), SPECIES_MOLWT( 141 ) / 'HGIIGAS         ',  271.50 /



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
      DATA CGRID_INDEX(   9 ), SPECIES_TYPE(   9 ), CONVERT_CONC(   9 ) /    9, 'GC', F /  ! H2O2
      DATA CGRID_INDEX(  10 ), SPECIES_TYPE(  10 ), CONVERT_CONC(  10 ) /   10, 'GC', F /  ! N2O5
      DATA CGRID_INDEX(  11 ), SPECIES_TYPE(  11 ), CONVERT_CONC(  11 ) /   11, 'GC', F /  ! HNO3
      DATA CGRID_INDEX(  12 ), SPECIES_TYPE(  12 ), CONVERT_CONC(  12 ) /   12, 'GC', F /  ! HONO
      DATA CGRID_INDEX(  13 ), SPECIES_TYPE(  13 ), CONVERT_CONC(  13 ) /   13, 'GC', F /  ! PNA
      DATA CGRID_INDEX(  14 ), SPECIES_TYPE(  14 ), CONVERT_CONC(  14 ) /   14, 'GC', F /  ! SO2
      DATA CGRID_INDEX(  15 ), SPECIES_TYPE(  15 ), CONVERT_CONC(  15 ) /   15, 'GC', F /  ! SULF
      DATA CGRID_INDEX(  16 ), SPECIES_TYPE(  16 ), CONVERT_CONC(  16 ) /   16, 'GC', F /  ! SULRXN
      DATA CGRID_INDEX(  17 ), SPECIES_TYPE(  17 ), CONVERT_CONC(  17 ) /   17, 'GC', F /  ! C2O3
      DATA CGRID_INDEX(  18 ), SPECIES_TYPE(  18 ), CONVERT_CONC(  18 ) /   18, 'GC', F /  ! MEO2
      DATA CGRID_INDEX(  19 ), SPECIES_TYPE(  19 ), CONVERT_CONC(  19 ) /   19, 'GC', F /  ! RO2
      DATA CGRID_INDEX(  20 ), SPECIES_TYPE(  20 ), CONVERT_CONC(  20 ) /   20, 'GC', F /  ! PAN
      DATA CGRID_INDEX(  21 ), SPECIES_TYPE(  21 ), CONVERT_CONC(  21 ) /   21, 'GC', F /  ! PACD
      DATA CGRID_INDEX(  22 ), SPECIES_TYPE(  22 ), CONVERT_CONC(  22 ) /   22, 'GC', F /  ! AACD
      DATA CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), CONVERT_CONC(  23 ) /   23, 'GC', F /  ! CXO3
      DATA CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), CONVERT_CONC(  24 ) /   24, 'GC', F /  ! ALD2
      DATA CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), CONVERT_CONC(  25 ) /   25, 'GC', F /  ! XO2H
      DATA CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), CONVERT_CONC(  26 ) /   26, 'GC', F /  ! PANX
      DATA CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), CONVERT_CONC(  27 ) /   27, 'GC', F /  ! FORM
      DATA CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), CONVERT_CONC(  28 ) /   28, 'GC', F /  ! MEPX
      DATA CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), CONVERT_CONC(  29 ) /   29, 'GC', F /  ! MEOH
      DATA CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), CONVERT_CONC(  30 ) /   30, 'GC', F /  ! ROOH
      DATA CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), CONVERT_CONC(  31 ) /   31, 'GC', F /  ! XO2
      DATA CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), CONVERT_CONC(  32 ) /   32, 'GC', F /  ! XO2N
      DATA CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), CONVERT_CONC(  33 ) /   35, 'GC', F /  ! NTR1
      DATA CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), CONVERT_CONC(  34 ) /   36, 'GC', F /  ! NTR2
      DATA CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), CONVERT_CONC(  35 ) /   37, 'GC', F /  ! FACD
      DATA CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), CONVERT_CONC(  36 ) /   38, 'GC', F /  ! CO
      DATA CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), CONVERT_CONC(  37 ) /   39, 'GC', F /  ! HCO3
      DATA CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), CONVERT_CONC(  38 ) /   40, 'GC', F /  ! ALDX
      DATA CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), CONVERT_CONC(  39 ) /   41, 'GC', F /  ! GLYD
      DATA CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), CONVERT_CONC(  40 ) /   42, 'GC', F /  ! GLY
      DATA CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), CONVERT_CONC(  41 ) /   43, 'GC', F /  ! MGLY
      DATA CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), CONVERT_CONC(  42 ) /   44, 'GC', F /  ! ETHA
      DATA CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), CONVERT_CONC(  43 ) /   45, 'GC', F /  ! ETOH
      DATA CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), CONVERT_CONC(  44 ) /   46, 'GC', F /  ! KET
      DATA CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), CONVERT_CONC(  45 ) /   47, 'GC', F /  ! PAR
      DATA CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), CONVERT_CONC(  46 ) /   48, 'GC', F /  ! ACET
      DATA CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), CONVERT_CONC(  47 ) /   49, 'GC', F /  ! PRPA
      DATA CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), CONVERT_CONC(  48 ) /   34, 'GC', F /  ! XPRP
      DATA CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), CONVERT_CONC(  49 ) /   33, 'GC', F /  ! XPAR
      DATA CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), CONVERT_CONC(  50 ) /   50, 'GC', F /  ! ROR
      DATA CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), CONVERT_CONC(  51 ) /   51, 'GC', F /  ! ETHY
      DATA CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), CONVERT_CONC(  52 ) /   52, 'GC', F /  ! ETH
      DATA CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), CONVERT_CONC(  53 ) /   53, 'GC', F /  ! OLE
      DATA CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), CONVERT_CONC(  54 ) /   54, 'GC', F /  ! IOLE
      DATA CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), CONVERT_CONC(  55 ) /   55, 'GC', F /  ! ISOP
      DATA CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), CONVERT_CONC(  56 ) /   56, 'GC', F /  ! ISO2
      DATA CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), CONVERT_CONC(  57 ) /   57, 'GC', F /  ! ISOPRXN
      DATA CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), CONVERT_CONC(  58 ) /   58, 'GC', F /  ! ISPD
      DATA CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), CONVERT_CONC(  59 ) /   59, 'GC', F /  ! INTR
      DATA CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), CONVERT_CONC(  60 ) /   60, 'GC', F /  ! ISPX
      DATA CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), CONVERT_CONC(  61 ) /   61, 'GC', F /  ! HPLD
      DATA CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), CONVERT_CONC(  62 ) /   62, 'GC', F /  ! OPO3
      DATA CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), CONVERT_CONC(  63 ) /   63, 'GC', F /  ! EPOX
      DATA CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), CONVERT_CONC(  64 ) /   64, 'GC', F /  ! EPX2
      DATA CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), CONVERT_CONC(  65 ) /   65, 'GC', F /  ! TERP
      DATA CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), CONVERT_CONC(  66 ) /   69, 'GC', F /  ! TRPRXN
      DATA CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), CONVERT_CONC(  67 ) /   67, 'GC', F /  ! TERPNRO2
      DATA CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), CONVERT_CONC(  68 ) /   66, 'GC', F /  ! APIN
      DATA CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), CONVERT_CONC(  69 ) /   70, 'GC', F /  ! BENZENE
      DATA CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), CONVERT_CONC(  70 ) /   71, 'GC', F /  ! CRES
      DATA CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), CONVERT_CONC(  71 ) /   72, 'GC', F /  ! BZO2
      DATA CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), CONVERT_CONC(  72 ) /   73, 'GC', F /  ! OPEN
      DATA CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), CONVERT_CONC(  73 ) /   74, 'GC', F /  ! BENZRO2
      DATA CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), CONVERT_CONC(  74 ) /   75, 'GC', F /  ! TOL
      DATA CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), CONVERT_CONC(  75 ) /   76, 'GC', F /  ! TO2
      DATA CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), CONVERT_CONC(  76 ) /   77, 'GC', F /  ! TOLRO2
      DATA CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), CONVERT_CONC(  77 ) /   78, 'GC', F /  ! XOPN
      DATA CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), CONVERT_CONC(  78 ) /   79, 'GC', F /  ! XYLMN
      DATA CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), CONVERT_CONC(  79 ) /   80, 'GC', F /  ! XLO2
      DATA CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), CONVERT_CONC(  80 ) /   81, 'GC', F /  ! XYLRO2
      DATA CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), CONVERT_CONC(  81 ) /   82, 'GC', F /  ! NAPH
      DATA CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), CONVERT_CONC(  82 ) /   83, 'GC', F /  ! PAHRO2
      DATA CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), CONVERT_CONC(  83 ) /   84, 'GC', F /  ! CRO
      DATA CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), CONVERT_CONC(  84 ) /   85, 'GC', F /  ! CAT1
      DATA CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), CONVERT_CONC(  85 ) /   86, 'GC', F /  ! CRON
      DATA CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), CONVERT_CONC(  86 ) /   87, 'GC', F /  ! OPAN
      DATA CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), CONVERT_CONC(  87 ) /   88, 'GC', F /  ! ECH4
      DATA CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), CONVERT_CONC(  88 ) /   89, 'GC', F /  ! CL2
      DATA CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), CONVERT_CONC(  89 ) /   90, 'GC', F /  ! CL
      DATA CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), CONVERT_CONC(  90 ) /   91, 'GC', F /  ! HOCL
      DATA CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), CONVERT_CONC(  91 ) /   92, 'GC', F /  ! CLO
      DATA CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), CONVERT_CONC(  92 ) /   93, 'GC', F /  ! FMCL
      DATA CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), CONVERT_CONC(  93 ) /   94, 'GC', F /  ! HCL
      DATA CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), CONVERT_CONC(  94 ) /   95, 'GC', F /  ! CLNO2
      DATA CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), CONVERT_CONC(  95 ) /   96, 'GC', F /  ! CLNO3
      DATA CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), CONVERT_CONC(  96 ) /  124, 'GC', F /  ! SVAVB2
      DATA CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), CONVERT_CONC(  97 ) /  125, 'GC', F /  ! SVAVB3
      DATA CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), CONVERT_CONC(  98 ) /  126, 'GC', F /  ! SVAVB4
      DATA CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), CONVERT_CONC(  99 ) /  123, 'GC', F /  ! SVAVB1
      DATA CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), CONVERT_CONC( 100 ) /   97, 'GC', F /  ! SESQ
      DATA CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), CONVERT_CONC( 101 ) /   98, 'GC', F /  ! SESQRXN
      DATA CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), CONVERT_CONC( 102 ) /   99, 'GC', F /  ! SOAALK
      DATA CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), CONVERT_CONC( 103 ) /  100, 'GC', F /  ! H2NO3PIJ
      DATA CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), CONVERT_CONC( 104 ) /  101, 'GC', F /  ! H2NO3PK
      DATA CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), CONVERT_CONC( 105 ) /  166, 'AE', T /  ! ACLI
      DATA CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), CONVERT_CONC( 106 ) /  165, 'AE', T /  ! ACLJ
      DATA CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), CONVERT_CONC( 107 ) /  168, 'AE', T /  ! ACLK
      DATA CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), CONVERT_CONC( 108 ) /  174, 'AE', T /  ! AISO3J
      DATA CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), CONVERT_CONC( 109 ) /  177, 'AE', T /  ! AGLYJ
      DATA CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), CONVERT_CONC( 110 ) /   68, 'GC', F /  ! MTNO3
      DATA CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), CONVERT_CONC( 111 ) /  178, 'AE', T /  ! AMTNO3J
      DATA CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), CONVERT_CONC( 112 ) /  179, 'AE', T /  ! AMTHYDJ
      DATA CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), CONVERT_CONC( 113 ) /  199, 'AE', T /  ! AAVB2J
      DATA CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), CONVERT_CONC( 114 ) /  175, 'AE', T /  ! AOLGAJ
      DATA CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), CONVERT_CONC( 115 ) /  200, 'AE', T /  ! AAVB3J
      DATA CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), CONVERT_CONC( 116 ) /  201, 'AE', T /  ! AAVB4J
      DATA CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), CONVERT_CONC( 117 ) /  134, 'AE', T /  ! AISO1J
      DATA CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), CONVERT_CONC( 118 ) /  176, 'AE', T /  ! AOLGBJ
      DATA CGRID_INDEX( 119 ), SPECIES_TYPE( 119 ), CONVERT_CONC( 119 ) /  135, 'AE', T /  ! AISO2J
      DATA CGRID_INDEX( 120 ), SPECIES_TYPE( 120 ), CONVERT_CONC( 120 ) /  136, 'AE', T /  ! ASQTJ
      DATA CGRID_INDEX( 121 ), SPECIES_TYPE( 121 ), CONVERT_CONC( 121 ) /  112, 'GC', F /  ! PCVOC
      DATA CGRID_INDEX( 122 ), SPECIES_TYPE( 122 ), CONVERT_CONC( 122 ) /  113, 'GC', F /  ! PCSOARXN
      DATA CGRID_INDEX( 123 ), SPECIES_TYPE( 123 ), CONVERT_CONC( 123 ) /  102, 'GC', F /  ! VLVPO1
      DATA CGRID_INDEX( 124 ), SPECIES_TYPE( 124 ), CONVERT_CONC( 124 ) /  103, 'GC', F /  ! VSVPO1
      DATA CGRID_INDEX( 125 ), SPECIES_TYPE( 125 ), CONVERT_CONC( 125 ) /  104, 'GC', F /  ! VSVPO2
      DATA CGRID_INDEX( 126 ), SPECIES_TYPE( 126 ), CONVERT_CONC( 126 ) /  105, 'GC', F /  ! VSVPO3
      DATA CGRID_INDEX( 127 ), SPECIES_TYPE( 127 ), CONVERT_CONC( 127 ) /  106, 'GC', F /  ! VIVPO1
      DATA CGRID_INDEX( 128 ), SPECIES_TYPE( 128 ), CONVERT_CONC( 128 ) /  107, 'GC', F /  ! VLVOO1
      DATA CGRID_INDEX( 129 ), SPECIES_TYPE( 129 ), CONVERT_CONC( 129 ) /  108, 'GC', F /  ! VLVOO2
      DATA CGRID_INDEX( 130 ), SPECIES_TYPE( 130 ), CONVERT_CONC( 130 ) /  110, 'GC', F /  ! VSVOO2
      DATA CGRID_INDEX( 131 ), SPECIES_TYPE( 131 ), CONVERT_CONC( 131 ) /  111, 'GC', F /  ! VSVOO3
      DATA CGRID_INDEX( 132 ), SPECIES_TYPE( 132 ), CONVERT_CONC( 132 ) /  109, 'GC', F /  ! VSVOO1
      DATA CGRID_INDEX( 133 ), SPECIES_TYPE( 133 ), CONVERT_CONC( 133 ) /  114, 'GC', F /  ! FORM_PRIMARY
      DATA CGRID_INDEX( 134 ), SPECIES_TYPE( 134 ), CONVERT_CONC( 134 ) /  115, 'GC', F /  ! ALD2_PRIMARY
      DATA CGRID_INDEX( 135 ), SPECIES_TYPE( 135 ), CONVERT_CONC( 135 ) /  116, 'GC', F /  ! BUTADIENE13
      DATA CGRID_INDEX( 136 ), SPECIES_TYPE( 136 ), CONVERT_CONC( 136 ) /  117, 'GC', F /  ! ACROLEIN
      DATA CGRID_INDEX( 137 ), SPECIES_TYPE( 137 ), CONVERT_CONC( 137 ) /  118, 'GC', F /  ! ACRO_PRIMARY
      DATA CGRID_INDEX( 138 ), SPECIES_TYPE( 138 ), CONVERT_CONC( 138 ) /  119, 'GC', F /  ! TOLU
      DATA CGRID_INDEX( 139 ), SPECIES_TYPE( 139 ), CONVERT_CONC( 139 ) /  120, 'GC', F /  ! HG
      DATA CGRID_INDEX( 140 ), SPECIES_TYPE( 140 ), CONVERT_CONC( 140 ) /  121, 'GC', F /  ! HGIIAER
      DATA CGRID_INDEX( 141 ), SPECIES_TYPE( 141 ), CONVERT_CONC( 141 ) /  122, 'GC', F /  ! HGIIGAS

      INTEGER, PARAMETER :: N_ACT_SP = 141

      INTEGER, PARAMETER :: NRXNS = 338

      INTEGER            :: KUNITS

      DATA  KUNITS /   2 /

      INTEGER IRXXN

      REAL( 8 )          :: RTDAT( 3,NRXNS )

      INTEGER, PARAMETER :: NFALLOFF =  22
      REAL( 8 )          :: RFDAT( 5,NFALLOFF )

      INTEGER            :: KTYPE( NRXNS )

      DATA ( KTYPE( IRXXN ), IRXXN = 1, NRXNS ) /  & 
     &      0,    2,    3,    2,    3,   10,    3,    0,    0,    3, & ! O   
     &      1,    3,    4,    3,    3,    4,   10,    3,    9,    9, & ! 1   
     &      0,    3,    3,    3,    3,    3,    0,    0,    3,    3, & ! 2   
     &      1,    1,    1,    1,    3,   10,   10,    0,    1,   10, & ! 3   
     &      1,    1,    0,    3,   10,    8,    0,   10,   10,    0, & ! 4   
     &      3,   10,    3,   10,   10,    0,    3,    3,    3,    3, & ! 5   
     &      3,    6,    6,    0,    3,    3,    3,    3,    3,    3, & ! 6   
     &      3,    3,    3,    6,    3,    3,    6,    6,    6,    6, & ! 7   
     &      6,    6,    6,    6,    6,    6,    3,    0,    3,    0, & ! 8   
     &      1,    0,    1,    3,    3,    3,    0,    0,    3,    1, & ! 9   
     &      3,    3,    1,    3,    3,    3,    3,    0,    3,    3, & ! O   
     &      1,    0,    1,    0,    3,    3,    0,    3,    0,    3, & ! 1   
     &      3,    3,    9,    3,    3,    3,    3,    0,    0,    3, & ! 2   
     &      3,    1,    3,    3,    3,   10,    3,   10,    3,    3, & ! 3   
     &      3,   10,    3,    3,    1,    3,    3,    1,    3,    1, & ! 4   
     &      3,    3,    6,    6,    3,    3,    3,    3,    3,    3, & ! 5   
     &      0,    3,    0,    3,    3,    3,    3,    6,    6,    1, & ! 6   
     &      1,    3,    3,    3,    1,    3,    3,    3,    3,    3, & ! 7   
     &      6,    3,    6,    3,    3,    6,    3,    6,    1,    1, & ! 8   
     &      3,    3,    6,    6,    3,    1,    1,    1,    1,    1, & ! 9   
     &      0,    0,    1,    3,    1,    0,    1,    3,    1,    1, & ! O   
     &      1,    1,    6,    6,    6,    6,    6,    1,    1,    3, & ! 1   
     &     10,    1,   10,    1,    0,    0,    3,    1,    3,    3, & ! 2   
     &      3,    1,    0,    3,    1,    1,    3,    1,    1,    1, & ! 3   
     &      1,    3,    1,    1,    1,    3,    4,    1,    1,    1, & ! 4   
     &      0,   10,    0,    0,   -1,   -1,    3,    3,    3,    3, & ! 5   
     &      3,    3,    1,    1,    1,    3,    3,    3,   -1,   -1, & ! 6   
     &     -1,   -1,   -1,   -1,   -1,   -1,   -1,   12,   -1,   -1, & ! 7   
     &     -1,    3,    3,    1,    1,    1,    1,    0,    1,    1, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 9   
     &      1,    1,    1,    1,    1,    1,    3,    1,    3,    0, & ! O   
     &      0,    3,    3,    3,    3,    0,    1,    3,    3,    1, & ! 1   
     &      1,    1,    1,    1,    0,    1,    1,    1,    1,    0, & ! 2   
     &      1,    3,    1,    3,    1,    1,    1,    3/     !  3   

      INTEGER            :: IRXBITS( NRXNS )

      DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,  276,    0,  260,    0,    1,    0,    2,    2,  260, & ! O   
     &      8,    0,    0,    0,    0,    0,    1,    0,    0,    8, & ! 1   
     &      2,    0,    0,   16,    0,    0,    2,    2,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    1,    1,    2,    8,    1, & ! 3   
     &      8,    0,    2,    0,    1,    0,    2,    1,    1,    2, & ! 4   
     &      0,    1,    0,    1,    1,    2,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    2,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    2,    0,    2, & ! 8   
     &      0,    2,    0,    0,    0,    0,    2,    2,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    2,    0,    0, & ! O   
     &      0,    2,    0,    2,    0,    0,    2,    0,    2,    0, & ! 1   
     &      0,  128,    0,   64,    0,    0,    0,    2,    2,    0, & ! 2   
     &      0,    0,    0,   16,    0,    1,    0,    1,    0,    0, & ! 3   
     &      0,    1,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      2,    0,    2,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      2,    2,    0,    0,    0,    2,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      1,    0,    1,    0,    2,    2,    0,    0,    0,    0, & ! 2   
     &      0,    0,    2,   64,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      2,    1,    2,    2,    1,    1,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    0,    1,    1, & ! 7   
     &      1,    0,    0,    0,    0,    0,    0,    2,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    2, & ! O   
     &      2,    0,    0,    0,    0,    2,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    2,    0,    0,    0,    0,    2, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,  260/     !  3   

      INTEGER            :: IORDER( NRXNS )

      DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    2,    3,    2,    2,    2,    1,    1,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    3, & ! 1   
     &      1,    2,    2,    3,    2,    2,    1,    1,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    1,    1,    2,    2, & ! 3   
     &      3,    2,    1,    2,    2,    2,    1,    2,    1,    1, & ! 4   
     &      2,    2,    2,    2,    1,    1,    2,    2,    2,    2, & ! 5   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    1,    2,    1, & ! 8   
     &      2,    1,    2,    2,    2,    2,    1,    1,    2,    2, & ! 9   
     &      2,    1,    2,    2,    2,    2,    2,    1,    2,    2, & ! O   
     &      2,    1,    2,    1,    2,    2,    1,    2,    1,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    1,    1,    2, & ! 2   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 5   
     &      1,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      1,    1,    2,    2,    2,    1,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    1,    2,    2,    2,    2,    2,    2, & ! 1   
     &      1,    1,    1,    1,    1,    1,    2,    2,    2,    2, & ! 2   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      1,    2,    1,    1,    1,    1,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    1, & ! 6   
     &      1,    1,    1,    2,    2,    2,    1,    1,    1,    1, & ! 7   
     &      1,    2,    2,    2,    2,    2,    2,    1,    1,    1, & ! 8   
     &      1,    1,    1,    1,    1,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! O   
     &      1,    2,    2,    2,    2,    1,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    1, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    3/     !  3   

      INTEGER, PARAMETER :: KTN1 =  94
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     11,   31,   32,   33,   34,   39,   41,   42,   91,   93, & ! O   
     &    100,  103,  111,  113,  132,  145,  148,  150,  170,  171, & ! 1   
     &    175,  189,  190,  196,  197,  198,  199,  200,  203,  205, & ! 2   
     &    207,  209,  210,  211,  212,  218,  219,  222,  224,  228, & ! 3   
     &    232,  235,  236,  238,  239,  240,  241,  243,  244,  245, & ! 4   
     &    248,  249,  250,  263,  264,  265,  284,  285,  286,  287, & ! 5   
     &    289,  290,  291,  292,  293,  294,  295,  296,  297,  298, & ! 6   
     &    299,  300,  301,  302,  303,  304,  305,  306,  308,  317, & ! 7   
     &    320,  321,  322,  323,  324,  326,  327,  328,  329,  331, & ! 8   
     &    333,  335,  336,  337/     !  9   

      INTEGER, PARAMETER :: KTN2 =   2
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &      2,    4/

      INTEGER, PARAMETER :: KTN3 = 134
      INTEGER            :: KRX3( KTN3 )

      DATA ( KRX3( IRXXN ), IRXXN = 1, KTN3 ) / & 
     &      3,    5,    7,   10,   12,   14,   15,   18,   22,   23, & ! O   
     &     24,   25,   26,   29,   30,   35,   44,   51,   53,   57, & ! 1   
     &     58,   59,   60,   61,   65,   66,   67,   68,   69,   70, & ! 2   
     &     71,   72,   73,   75,   76,   87,   89,   94,   95,   96, & ! 3   
     &     99,  101,  102,  104,  105,  106,  107,  109,  110,  115, & ! 4   
     &    116,  118,  120,  121,  122,  124,  125,  126,  127,  130, & ! 5   
     &    131,  133,  134,  135,  137,  139,  140,  141,  143,  144, & ! 6   
     &    146,  147,  149,  151,  152,  155,  156,  157,  158,  159, & ! 7   
     &    160,  162,  164,  165,  166,  167,  172,  173,  174,  176, & ! 8   
     &    177,  178,  179,  180,  182,  184,  185,  187,  191,  192, & ! 9   
     &    195,  204,  208,  220,  227,  229,  230,  231,  234,  237, & ! O   
     &    242,  246,  257,  258,  259,  260,  261,  262,  266,  267, & ! 1   
     &    268,  282,  283,  307,  309,  312,  313,  314,  315,  318, & ! 2   
     &    319,  332,  334,  338/     !  3   

      INTEGER, PARAMETER :: KTN4 =   3
      INTEGER            :: KRX4( KTN4 )

      DATA ( KRX4( IRXXN ), IRXXN = 1, KTN4 ) / & 
     &     13,   16,  247/

      INTEGER, PARAMETER :: KTN5 =   0
      INTEGER            :: KRX5( 1 )

      DATA   KRX5( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN6 =  28
      INTEGER            :: KRX6( KTN6 )

      DATA ( KRX6( IRXXN ), IRXXN = 1, KTN6 ) / & 
     &     62,   63,   74,   77,   78,   79,   80,   81,   82,   83, & 
     &     84,   85,   86,  153,  154,  168,  169,  181,  183,  186, & 
     &    188,  193,  194,  213,  214,  215,  216,  217/

      INTEGER, PARAMETER :: KTN7 =   0
      INTEGER            :: KRX7( 1 )

      DATA   KRX7( 1 ) / 0 /

      INTEGER, PARAMETER :: NWM =   4
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &      2,    4,   10,  338/
      REAL,    PARAMETER :: ATM_AIR = 1.00000E+06

      INTEGER, PARAMETER :: NWW =   4
      INTEGER            :: NRXWW( NWW )

      DATA ( NRXWW( IRXXN ), IRXXN = 1, NWW ) / & 
     &     11,   20,   39,   41/

      INTEGER, PARAMETER :: NWO2 =   3
      INTEGER            :: NRXWO2( NWO2 )

      DATA ( NRXWO2( IRXXN ), IRXXN = 1, NWO2 ) / & 
     &      2,   24,  134/
      REAL,    PARAMETER :: ATM_O2 = 2.09500E+05

      INTEGER, PARAMETER :: NWN2 =   0
      INTEGER            :: NRXWN2( 1 )

      DATA   NRXWN2( 1 ) / 0 /
      REAL,    PARAMETER :: ATM_N2 = 7.80800E+05

      INTEGER, PARAMETER :: NWCH4 =   2
      INTEGER            :: NRXWCH4( NWCH4 )

      DATA ( NRXWCH4( IRXXN ), IRXXN = 1, NWCH4 ) / & 
     &    124,  234/
      REAL,    PARAMETER :: ATM_CH4 = 1.85000E+00

      INTEGER, PARAMETER :: NWH2 =   1
      INTEGER            :: NRXWH2( NWH2 )

      DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) / & 
     &    122/
      REAL,    PARAMETER :: ATM_H2 = 5.60000E-01

      INTEGER, PARAMETER :: MXPRD =  14
      INTEGER            :: IRR( NRXNS,MXPRD+3 )

      DATA ( IRR( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    4,    3,    3,    3,    3,    4,    4,    6, & ! O   
     &      6,    4,    4,    7,    8,    7,    7,    7,    8,    8, & ! 1   
     &      9,    9,    9,    2,    8,    1,    5,    5,    5,    5, & ! 2   
     &      5,    5,    5,    5,    5,    5,   10,   10,   10,    2, & ! 3   
     &      2,   12,   12,   12,    1,   11,   11,    8,   13,   13, & ! 4   
     &     13,   14,   17,   17,   20,   20,   17,   17,   17,   17, & ! 5   
     &     23,   23,   26,   26,   23,   23,   23,   19,   19,   19, & ! 6   
     &     18,   18,   18,   18,   25,   25,   25,   25,   31,   31, & ! 7   
     &     31,   31,   32,   32,   32,   32,   28,   28,   30,   30, & ! 8   
     &     33,   33,   35,   22,   21,   27,   27,   27,   27,   27, & ! 9   
     &     27,   37,   37,   37,   24,   24,   24,   24,   38,   38, & ! O   
     &     38,   38,   39,   39,   39,   40,   40,   40,   41,   41, & ! 1   
     &     41,    7,   36,    7,   42,   29,   43,   44,   46,   46, & ! 2   
     &     47,   45,   50,   50,   50,   51,   52,   52,   52,   52, & ! 3   
     &     53,   53,   53,   53,   54,   54,   54,   54,   55,   55, & ! 4   
     &     56,   56,   56,   56,   56,   55,   55,   58,   58,   58, & ! 5   
     &     58,   60,   61,   61,   63,   64,   64,   64,   64,   59, & ! 6   
     &     65,   65,   65,   65,   68,   68,   68,   68,   69,   71, & ! 7   
     &     71,   71,   71,   74,   75,   75,   75,   75,   78,   81, & ! 8   
     &     79,   79,   79,   79,   70,   70,   83,   83,   85,   85, & ! 9   
     &     85,   77,   77,   77,   77,   72,   72,   72,   72,   84, & ! O   
     &     84,   62,   62,   86,   62,   62,   62,   86,   26,   87, & ! 1   
     &     48,   48,   49,   49,   88,   90,   89,   91,   91,   91, & ! 2   
     &     91,    7,   92,   89,   89,   89,   89,   89,   89,   89, & ! 3   
     &     89,   89,   89,   89,   89,   89,   93,   89,   89,   89, & ! 4   
     &     94,   91,   95,   95,   95,   95,   76,   76,   80,   80, & ! 5   
     &     73,   73,  100,  100,  100,   82,   82,  102,   34,   10, & ! 6   
     &     10,  103,  104,  103,  103,  104,    1,    4,   63,   40, & ! 7   
     &     41,   67,   67,   67,   67,  110,  110,  110,  111,  113, & ! 8   
     &    115,  116,  117,  119,  120,  121,  123,  124,  125,  126, & ! 9   
     &    127,  128,  129,  132,  130,  131,  133,  133,  133,  133, & ! O   
     &    133,  133,  134,  134,  134,  134,  134,  135,  135,  135, & ! 1   
     &    135,  137,  137,  137,  137,  137,  136,  136,  136,  136, & ! 2   
     &    136,  138,  138,  139,  139,  139,  139,  139/     !  3   

      DATA ( IRR( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    2,    2,    1,    1,    4,    0,    0,    0, & ! O   
     &      0,    7,    8,    3,    3,    7,    7,    8,    8,    8, & ! 1   
     &      0,    7,    3,    2,    2,    4,    0,    0,    2,    1, & ! 2   
     &      3,    7,    8,    4,    5,    1,    0,    0,    0,    7, & ! 3   
     &      1,   12,    0,    7,    7,    7,    0,    1,    0,    0, & ! 4   
     &      7,    7,    2,    1,    0,    0,    8,   19,   17,   23, & ! 5   
     &      2,    1,    0,    0,    8,   19,   23,    2,    8,   19, & ! 6   
     &      2,    8,   17,   19,    2,    8,   17,   19,    2,    8, & ! 7   
     &     17,   19,    2,    8,   17,   19,    7,    0,    7,    0, & ! 8   
     &      7,    0,    7,    7,    7,    7,    0,    0,    3,    5, & ! 9   
     &      8,    0,    2,    8,    3,    7,    5,    0,    3,    7, & ! O   
     &      5,    0,    7,    0,    5,    7,    0,    5,    0,    5, & ! 1   
     &      7,    0,    7,    0,    7,    7,    7,    0,    0,    7, & ! 2   
     &      7,    7,    0,    0,    1,    7,    3,    7,    4,    5, & ! 3   
     &      3,    7,    4,    5,    3,    7,    4,    5,    7,    3, & ! 4   
     &      2,    8,   17,   19,    0,    4,    5,    7,    4,    5, & ! 5   
     &      0,    7,    0,    5,    7,    8,    2,   17,   19,    7, & ! 6   
     &      3,    7,    4,    5,    3,    7,    4,    5,    7,    2, & ! 7   
     &     17,    8,   19,    7,    2,   17,    8,   19,    7,    7, & ! 8   
     &      2,    8,   17,   19,    7,    5,    1,    8,    7,    5, & ! 9   
     &      0,    0,    7,    4,    5,    0,    7,    4,    5,    7, & ! O   
     &      5,    2,    1,    0,    8,   17,   19,    7,    7,    7, & ! 1   
     &      0,    0,    0,    0,    0,    0,    4,   91,    2,    8, & ! 2   
     &     18,   92,    0,    0,   45,   47,   42,   52,   53,   54, & ! 3   
     &     55,   27,   24,   38,   29,   43,    7,   74,   78,   81, & ! 4   
     &      0,    1,    0,    0,    0,    0,    2,    8,    2,    8, & ! 5   
     &      2,    8,    4,    7,    5,    2,    8,    7,    0,    0, & ! 6   
     &      0,    0,    0,  105,  106,  107,    0,    0,    0,    0, & ! 7   
     &      0,    2,    8,    5,   19,   89,    7,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    7,    7,    7,    7,    7, & ! 9   
     &      7,    7,    7,    7,    7,    7,    7,    5,    3,    0, & ! O   
     &      0,   89,    7,    5,    3,    0,   89,    7,    4,    5, & ! 1   
     &     89,    7,    4,    5,    0,   89,    7,    4,    5,    0, & ! 2   
     &     89,    7,   89,    4,   88,    9,    7,   89/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &      2,    4,    1,    1,    2,    5,    0,    3,    6,    3, & ! O   
     &      7,    8,    7,    8,    7,    3,    9,    0,    9,    9, & ! 1   
     &      7,    8,    7,    1,    7,    5,    1,    2,    1,    2, & ! 2   
     &      1,    8,    7,    1,    1,   10,    5,    1,   11,   12, & ! 3   
     &     12,    2,    2,    1,   11,    5,    7,   13,    8,    8, & ! 4   
     &      1,   15,    1,   20,    1,    1,   21,   17,   18,   18, & ! 5   
     &      1,   26,    1,    1,   21,   24,   24,    2,    8,    0, & ! 6   
     &     27,   28,   27,   27,    1,   30,    8,    8,    1,   30, & ! 7   
     &     18,   19,   33,   30,    8,   19,   18,   18,   25,    8, & ! 8   
     &     34,    1,    8,   18,   17,    8,    8,   36,    7,   11, & ! 9   
     &     37,   27,   35,   28,   17,   17,   17,   18,   23,   23, & ! O   
     &     23,   24,   40,   27,   11,   36,    8,   11,   17,   11, & ! 1   
     &     17,    8,    8,   18,   24,   27,   24,   24,   36,   27, & ! 2   
     &     48,   49,   44,   44,   33,   40,   27,   25,   27,    1, & ! 3   
     &     24,   27,   24,    1,   24,   24,   24,    1,   56,   58, & ! 4   
     &     59,   60,   27,   27,    8,   27,    1,   32,   24,   11, & ! 5   
     &      8,   63,    7,   11,   64,   39,   39,   39,   39,   31, & ! 6   
     &     38,   25,    7,    1,   38,   25,    7,    1,   70,    1, & ! 7   
     &     40,    0,   40,   70,    1,   40,    0,   40,   70,   70, & ! 8   
     &      1,    0,   40,   40,   40,   83,   85,   70,   34,   34, & ! 9   
     &     12,   40,   41,   41,    1,   62,   62,   40,   62,   27, & ! O   
     &     83,    1,   86,   62,   21,   18,   25,    1,   24,   18, & ! 1   
     &     32,   46,   32,   38,   89,    7,   91,   88,   89,   90, & ! 2   
     &     89,   89,   89,   93,   93,   93,   93,   92,   92,   93, & ! 3   
     &     92,   93,   93,   93,   93,   93,   89,   93,   93,   93, & ! 4   
     &     89,   95,   91,   89,   90,   90,    2,    8,    2,    8, & ! 5   
     &      2,    8,    4,    7,    5,    2,    8,    7,   11,   11, & ! 6   
     &     11,   11,   11,   94,   94,   94,   12,    0,  108,  109, & ! 7   
     &    109,    2,    8,    5,   19,   89,    7,    0,  112,  114, & ! 8   
     &    114,  114,  118,  118,  118,    7,    7,    7,    7,    7, & ! 9   
     &      7,    7,    7,    7,    7,    7,    7,    5,    3,    0, & ! O   
     &      0,   89,    7,    5,    3,    0,   89,    7,    4,    5, & ! 1   
     &     89,    7,    4,    5,    0,   89,    7,    4,    5,    0, & ! 2   
     &     89,    7,   89,  140,  141,  141,  140,  139/     !  3   

      DATA ( IRR( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &      3,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    8,    0,    1,    0,    3,    0,    0,    1, & ! 2   
     &      0,    1,    1,    0,    0,    0,    1,    5,    0,    0, & ! 3   
     &      0,    1,    7,    0,    0,    0,    1,    0,    1,    1, & ! 4   
     &      0,    8,   18,    0,   17,   17,   22,    0,   19,   24, & ! 5   
     &     24,    0,   23,   23,   22,   25,   25,    0,    0,    0, & ! 6   
     &      8,   27,    8,   29,    8,    0,   18,   19,    0,    0, & ! 7   
     &     22,    0,   34,    0,   18,    0,   19,   19,   32,    7, & ! 8   
     &      0,    0,    0,   19,    0,   36,   36,    0,    8,    8, & ! 9   
     &      0,    8,    1,   35,    7,    0,   11,   19,    7,    0, & ! O   
     &     11,   25,    8,   36,   17,   31,   36,   36,    8,   17, & ! 1   
     &     36,    0,    0,   19,   25,    8,    8,   17,   18,   17, & ! 2   
     &      0,    0,   46,    8,    0,    7,    8,   19,   36,   33, & ! 3   
     &     38,   24,   27,   33,   38,   38,   38,   33,   19,   27, & ! 4   
     &      1,    7,   58,   58,   61,   58,   34,   31,   27,   34, & ! 5   
     &     25,    7,   58,   58,   19,   40,   40,   40,   40,   25, & ! 6   
     &     45,   31,   25,   25,   45,   31,   25,   25,   71,   34, & ! 7   
     &     72,    0,   72,   75,   34,   41,    0,   41,   79,   79, & ! 8   
     &     34,    0,   41,   41,   72,   11,    0,    0,   83,   83, & ! 9   
     &      8,   25,   40,    7,   34,    8,   25,   41,   11,    8, & ! O   
     &     11,   40,    0,    1,   22,   31,   38,   40,    1,   19, & ! 1   
     &     19,   38,   19,   50,    0,   89,    0,   89,    1,    0, & ! 2   
     &     27,   36,   36,   18,   49,   46,   24,   31,   24,   92, & ! 3   
     &     58,    8,   17,   23,    8,    8,    0,   70,   70,   70, & ! 4   
     &      1,    0,    1,    5,   11,   11,   96,   99,   96,   99, & ! 5   
     &     96,   99,  101,  101,  101,   96,   99,   96,    0,  103, & ! 6   
     &    104,    0,    0,    0,    0,    0,   11,    0,    0,    0, & ! 7   
     &      0,  110,  110,  110,  110,  110,  110,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,  122,  123,  123,  123,  123, & ! 9   
     &    123,  128,  128,  128,  128,  128,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,  136,  136,  136, & ! 1   
     &    136,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,  141,   88,    9,  141,  141/     !  3   

      DATA ( IRR( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    7, & ! 4   
     &      0,   16,   19,    0,    0,    5,    4,    0,    0,   25, & ! 5   
     &     25,    0,    0,    5,    4,   19,   19,    0,    0,    0, & ! 6   
     &      1,    0,   18,    8,    0,    0,   22,    0,    0,    0, & ! 7   
     &     19,    0,    0,    0,   22,    0,   27,    7,   19,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,   36,   36, & ! 9   
     &      0,    0,    8,    7,    0,    0,    0,   36,    0,    0, & ! O   
     &      0,   19,   17,    8,    0,   19,    0,   31,   36,   31, & ! 1   
     &      0,    0,    0,    0,   32,    0,   25,   25,   19,   31, & ! 2   
     &      0,    0,   24,    0,    0,   35,   36,   27,    8,   25, & ! 3   
     &      8,   38,   38,   31,   25,   25,   27,   31,   57,   31, & ! 4   
     &     27,    8,    8,    8,    0,   38,   25,   41,   41,    1, & ! 5   
     &     31,   56,    0,    0,    0,   41,   41,   41,   41,   19, & ! 6   
     &     66,   32,   31,   31,   66,   32,   31,   31,   19,   40, & ! 7   
     &      8,    0,    8,   19,   40,   72,    0,   72,   19,   19, & ! 8   
     &     40,    0,   72,   72,    8,   31,    0,    0,    0,   11, & ! 9   
     &     27,    8,   25,   17,   25,   36,   19,    7,    0,   83, & ! O   
     &      0,   36,    0,    0,    4,   38,   19,   36,    0,    0, & ! 1   
     &      0,   45,    0,   25,    0,    0,    0,    0,    0,    0, & ! 2   
     &      8,    0,    8,   19,    0,   25,   25,    8,   38,   24, & ! 3   
     &     25,   36,    0,    0,   27,   24,    0,   75,   79,   79, & ! 4   
     &      0,    0,    0,    0,    0,    0,   97,    0,   97,    0, & ! 5   
     &     98,    0,    0,    0,    0,   97,    0,   97,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,  124,  124,  124,  124, & ! 9   
     &    124,  129,  129,  129,  129,  129,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    4,    0,    0,    7,   89/     !  3   

      DATA ( IRR( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    5, & ! 4   
     &      0,    0,    0,    0,    0,   18,   18,    0,    0,   19, & ! 5   
     &     19,    0,    0,   24,   24,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,   22,   19,    0,    0,   19,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,   19,    0,    7,    0,    7,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    8,    0,    0,    0,    8,    0,    0, & ! O   
     &      0,   36,    0,   29,    0,    8,    0,   19,    0,   19, & ! 1   
     &      0,    0,    0,    0,   19,    0,   19,   23,   17,   19, & ! 2   
     &      0,    0,   38,    0,    0,   36,   25,   39,    7,   31, & ! 3   
     &     25,   25,   25,   25,   19,   19,   36,   25,    0,   19, & ! 4   
     &     58,   27,   25,   25,    0,   23,   31,   18,   40,   31, & ! 5   
     &     18,   19,    0,    0,    0,    7,    7,    7,    7,    1, & ! 6   
     &      0,   19,   32,   32,    0,   19,   32,   32,   72,   72, & ! 7   
     &     18,    0,   19,   72,   41,   77,    0,   77,   77,   77, & ! 8   
     &     41,    0,   77,   77,   83,   25,    0,    0,    0,    0, & ! 9   
     &     72,   36,   19,   24,   31,    0,   40,   17,    0,    0, & ! O   
     &      0,    8,    0,    0,   38,   19,   22,   34,    0,    0, & ! 1   
     &      0,   25,    0,   31,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   32,   32,   27,   31,   38, & ! 3   
     &     32,    0,    0,    0,    0,    0,    0,   19,   19,   19, & ! 4   
     &      0,    0,    0,    0,    0,    0,   98,    0,   98,    0, & ! 5   
     &      0,    0,    0,    0,    0,   98,    0,   98,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,  125,  125,  125,  125, & ! 9   
     &    125,  132,  132,  132,  132,  132,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,   19,   19,    0,    0,    0, & ! 5   
     &      0,    0,    0,   25,   25,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,   19,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    8,    0,    7,    0,    0,    0,    8,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   27,   18,    0,    0, & ! 2   
     &      0,    0,   32,    0,    0,    8,   19,    0,   35,   19, & ! 3   
     &     36,   31,   19,   32,   36,    0,    7,   32,    0,    8, & ! 4   
     &      8,   58,   18,   19,    0,   45,   32,   39,   46,   25, & ! 5   
     &     17,   54,    0,    0,    0,    8,    8,    8,    8,    5, & ! 6   
     &      0,   27,   19,   19,    0,   27,   19,   19,    7,    8, & ! 7   
     &     19,    0,    0,    7,   72,    8,    0,    8,    7,    7, & ! 8   
     &     72,    0,    8,    8,   84,   40,    0,    0,    0,    0, & ! 9   
     &      0,   17,    0,   36,   32,    0,    0,   27,    0,    0, & ! O   
     &      0,   23,    0,    0,   25,    0,    0,    0,    0,    0, & ! 1   
     &      0,   19,    0,   19,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   19,   19,    0,    8,   53, & ! 3   
     &     19,    0,    0,    0,    0,    0,    0,   72,   77,   77, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,  126,  126,  126,  126, & ! 9   
     &    128,  130,  130,  130,  130,  130,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    7,    0,    0,    0, & ! 5   
     &      0,    0,    0,   19,   19,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,   40,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   39,   19,    0,    0, & ! 2   
     &      0,    0,   25,    0,    0,    0,    7,    0,    0,   27, & ! 3   
     &     27,   32,    7,   19,   45,    0,   25,   19,    0,   23, & ! 4   
     &     25,    0,   22,    0,    0,    7,   19,   17,   36,   39, & ! 5   
     &     27,   38,    0,    0,    0,   27,   27,   27,   27,   59, & ! 6   
     &      0,   45,   27,   38,    0,   45,   27,   38,    8,    0, & ! 7   
     &      0,    0,    0,   25,   77,   18,    0,   19,   25,   25, & ! 8   
     &     77,    0,   18,   19,   32,   41,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,   25,   19,    0,    0,   24,    0,    0, & ! O   
     &      0,    0,    0,    0,   19,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,   45,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,   45,   45, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    7,    7,    7, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,  127,  128,  128,  128, & ! 9   
     &    129,  131,  131,  131,  131,  131,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    7,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,   25,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,   45,    0,    0, & ! 2   
     &      0,    0,   19,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &     32,   19,    8,   27,    0,    0,   19,   24,    0,   45, & ! 4   
     &     19,    0,   19,    0,    0,   31,   27,   62,    7,   41, & ! 5   
     &     53,    0,    0,    0,    0,   35,    1,   36,   36,   27, & ! 6   
     &      0,   38,   36,   34,    0,   38,   36,   34,   73,    0, & ! 7   
     &      0,    0,    0,    8,    8,   19,    0,    0,    8,    8, & ! 8   
     &      8,    0,   19,    0,   19,   62,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,   19,   72,    0,    0,   36,    0,    0, & ! O   
     &      0,    0,    0,    0,    7,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   31, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,   25,   25,   25, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,  128,  129,  129,  129, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

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
     &      0,    0,    0,   19,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   50,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &     19,   45,   36,   24,    0,    0,   40,   38,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,   19,   58,   45,   35,   45, & ! 5   
     &     45,    0,    0,    0,    0,   36,   36,   45,   45,   39, & ! 6   
     &      0,   66,   45,   67,    0,   66,   45,    0,    0,    0, & ! 7   
     &      0,    0,    0,   76,    0,    0,    0,    0,   80,   82, & ! 8   
     &      0,    0,    0,    0,    0,   32,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,   41,    0,    0,    8,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    8, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    8,    8,    8, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,  129,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   45,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &     45,    0,   40,   38,    0,    0,   41,   45,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    8,   57,   46,    8,   23, & ! 5   
     &     46,    0,    0,    0,    0,   45,   45,   18,   19,   35, & ! 6   
     &      0,    0,   38,    0,    0,    0,   38,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   19,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,   76,   80,   82, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,  130,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      7,    0,   41,   45,    0,    0,   45,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,   36,    0,   36,   17,   19, & ! 5   
     &     39,    0,    0,    0,    0,    0,    0,   22,    0,   45, & ! 6   
     &      0,    0,   23,    0,    0,    0,   23,    0,    0,    0, & ! 7   
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
     &      0,    0,    0,    0,    0,    0,  131,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 14 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,   35,    0,    0,    0,   22,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    8,    0,    0, & ! 5   
     &     19,    0,    0,    0,    0,    0,    0,   19,    0,   53, & ! 6   
     &      0,    0,   66,    0,    0,    0,   66,    0,    0,    0, & ! 7   
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
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 15 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,   22,    0,    0,    0,    9,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   19,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   38, & ! 6   
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
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 16 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,    9,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   34, & ! 6   
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
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( IRR( IRXXN, 17 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,   45,    0,    0,    0,    0,    0,    0,    0, & ! 4   
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
     &      0,    0,    0,    0,    0,    0,    0,    0/     !  3   

      DATA ( RTDAT( 1,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 5.6800D-34, 1.4000D-12, 1.0000D-31, 5.5000D-12, & ! O   
     &     1.3000D-31, 8.0000D-12, 1.0000D+00, 1.0000D+00, 2.2300D-11, & ! +   
     &     2.1400D-10, 1.7000D-12, 2.0300D-16, 2.4000D-11, 2.7000D-11, & ! 1   
     &     6.2000D-14, 6.9000D-31, 4.8000D-11, 2.2000D-13, 3.0800D-34, & ! +   
     &     1.0000D+00, 2.9000D-12, 1.4000D-12, 3.3000D-39, 3.4500D-12, & ! 2   
     &     1.4000D-13, 1.0000D+00, 1.0000D+00, 1.8000D-11, 4.5000D-14, & ! +   
     &     1.7000D-11, 2.0000D-11, 4.0000D-12, 1.0000D-17, 8.5000D-13, & ! 3   
     &     3.6000D-30, 1.3000D-03, 1.0000D+00, 1.0000D-22, 7.4000D-31, & ! +   
     &     5.0000D-40, 1.0000D-20, 1.0000D+00, 2.5000D-12, 1.8000D-30, & ! 4   
     &     2.4000D-14, 1.0000D+00, 1.8000D-31, 4.1000D-05, 1.0000D+00, & ! +   
     &     3.2000D-13, 4.5000D-31, 7.5000D-12, 2.7000D-28, 4.9000D-03, & ! 5   
     &     1.0000D+00, 5.2000D-13, 8.9000D-13, 2.9000D-12, 2.9000D-12, & ! +   
     &     6.7000D-12, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.2000D-13, & ! 6   
     &     8.9000D-13, 3.2000D-12, 2.4000D-12, 4.8000D-13, 6.5000D-14, & ! +   
     &     2.3000D-12, 3.8000D-13, 2.0000D-12, 1.0000D+00, 2.7000D-12, & ! 7   
     &     6.8000D-13, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 5.3000D-12, 1.0000D+00, 5.3000D-12, 1.0000D+00, & ! +   
     &     2.0000D-12, 1.0000D+00, 4.5000D-13, 4.0000D-14, 5.3000D-12, & ! 9   
     &     5.4000D-12, 1.0000D+00, 1.0000D+00, 3.4000D-11, 5.5000D-16, & ! +   
     &     9.7000D-15, 2.4000D+12, 5.6000D-12, 5.6000D-15, 1.8000D-11, & ! O   
     &     4.7000D-12, 1.4000D-12, 1.0000D+00, 1.3000D-11, 4.9000D-12, & ! +   
     &     6.3000D-15, 1.0000D+00, 8.0000D-12, 1.0000D+00, 1.4000D-12, & ! 1   
     &     3.1000D-12, 1.0000D+00, 1.4000D-12, 1.0000D+00, 1.4000D-12, & ! +   
     &     1.9000D-12, 7.7000D-12, 1.4400D-13, 1.8500D-12, 6.9000D-12, & ! 2   
     &     2.8500D-12, 3.0000D-12, 1.0000D+00, 1.0000D+00, 1.4100D-12, & ! +   
     &     7.6000D-12, 8.1000D-13, 5.7000D+12, 1.5000D-14, 8.6000D-12, & ! 3   
     &     5.0000D-30, 1.0400D-11, 8.6000D-29, 9.1000D-15, 3.3000D-12, & ! +   
     &     1.0000D-11, 8.0000D-27, 5.5000D-15, 4.6000D-13, 2.3000D-11, & ! 4   
     &     1.0500D-11, 4.7000D-15, 3.7000D-13, 2.7000D-11, 3.0000D-11, & ! +   
     &     2.3900D-12, 7.4300D-13, 1.0000D+00, 1.0000D+00, 3.3000D+09, & ! 5   
     &     1.0300D-14, 3.0300D-12, 5.5800D-12, 3.8800D-15, 4.1000D-12, & ! +   
     &     1.0000D+00, 2.2300D-11, 1.0000D+00, 6.0000D-12, 5.7800D-11, & ! 6   
     &     7.4300D-13, 2.3900D-12, 1.0000D+00, 1.0000D+00, 3.1000D-11, & ! +   
     &     3.6000D-11, 1.5000D-11, 1.2000D-15, 3.7000D-12, 3.6000D-11, & ! 7   
     &     1.5000D-11, 1.2000D-15, 3.7000D-12, 2.3000D-12, 2.7000D-12, & ! +   
     &     1.0000D+00, 1.9000D-13, 1.0000D+00, 1.8000D-12, 2.7000D-12, & ! 8   
     &     1.0000D+00, 1.9000D-13, 1.0000D+00, 1.8500D-11, 1.8500D-11, & ! +   
     &     2.7000D-12, 1.9000D-13, 1.0000D+00, 1.0000D+00, 1.7000D-12, & ! 9   
     &     1.4000D-11, 2.1000D-12, 5.5000D-12, 1.5300D-12, 3.8000D-12, & ! +   
     &     1.0000D+00, 5.0000D-02, 9.0000D-11, 1.0800D-16, 3.0000D-12, & ! O   
     &     2.8000D-02, 4.4000D-11, 5.4000D-17, 3.8000D-12, 5.0000D-11, & ! +   
     &     1.7000D-10, 1.0000D-11, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 3.6000D-11, 3.0000D-12, 1.8500D-12, & ! +   
     &     2.3700D-21, 1.0000D+00, 4.8100D-20, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 2.3000D-11, 1.6300D-14, 6.4000D-12, 2.2000D-12, & ! +   
     &     3.2000D-12, 5.0000D-13, 1.0000D+00, 6.6000D-12, 5.0000D-11, & ! 3   
     &     1.4000D-10, 8.3000D-11, 1.0700D-10, 2.5000D-10, 3.5000D-10, & ! +   
     &     4.3000D-10, 8.2000D-11, 7.9000D-11, 1.3000D-10, 5.5000D-11, & ! 4   
     &     8.2000D-11, 6.5800D-13, 6.1000D-11, 1.2000D-10, 1.2000D-10, & ! +   
     &     1.0000D+00, 1.8000D-31, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 2.7000D-12, 1.9000D-13, 2.7000D-12, 1.9000D-13, & ! +   
     &     2.7000D-12, 1.9000D-13, 1.1600D-14, 1.9700D-10, 1.9000D-11, & ! 6   
     &     2.7000D-12, 1.9000D-13, 2.7000D-12, 1.4000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 2.8964D-11, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 2.6000D-12, 2.6500D-13, 2.3000D-12, 3.5000D-14, & ! 8   
     &     1.9200D-10, 7.2000D-12, 1.0000D+00, 9.2590D-05, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 9   
     &     1.2500D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, & ! +   
     &     4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, & ! O   
     &     4.0000D-11, 5.4000D-12, 5.5000D-16, 3.4000D-11, 1.0000D+00, & ! +   
     &     1.0000D+00, 8.2000D-11, 4.7000D-12, 1.4000D-12, 1.8000D-11, & ! 1   
     &     1.0000D+00, 7.9000D-11, 1.4800D-11, 1.3400D-14, 1.7900D-13, & ! +   
     &     2.5100D-10, 2.0000D-11, 2.6100D-19, 1.1500D-15, 1.0000D+00, & ! 2   
     &     2.3700D-10, 2.0000D-11, 2.6100D-19, 1.1500D-15, 1.0000D+00, & ! +   
     &     2.3700D-10, 1.8000D-12, 6.1000D-11, 2.1100D-18, 2.6000D-18, & ! 3   
     &     8.5000D-19, 7.7000D-14, 2.2500D-33/           !        +   

      DATA ( RTDAT( 2,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00,-2.6000D+00, 0.0000D+00,-1.6000D+00, 0.0000D+00, & ! O   
     &    -1.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.5700D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     2.6000D+00,-8.0000D-01, 0.0000D+00, 6.0000D+02, 2.8000D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &    -4.1000D+00,-3.5000D+00, 0.0000D+00, 0.0000D+00,-2.4000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-3.0000D+00, & ! 4   
     &     4.6000D+02, 0.0000D+00,-3.2000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-3.9000D+00, 0.0000D+00,-7.1000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 5.4000D+01, 5.5000D+01, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 7.0000D+01, 0.0000D+00, & ! 7   
     &     0.0000D+00, 5.8000D+01, 7.0000D+01, 7.5000D+01, 7.6000D+01, & ! +   
     &     5.8000D+01, 7.0000D+01, 7.5000D+01, 7.6000D+01, 5.8000D+01, & ! 8   
     &     7.0000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &    -1.5000D+00, 0.0000D+00,-3.1000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-3.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.8000D+01, 7.0000D+01, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 5.8000D+01, 7.0000D+01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     5.8000D+01, 0.0000D+00, 7.0000D+01, 0.0000D+00, 0.0000D+00, & ! 8   
     &     5.8000D+01, 0.0000D+00, 7.0000D+01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.8000D+01, 7.0000D+01, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.4000D+01, 5.5000D+01, 5.7000D+01, & ! 1   
     &     5.9000D+01, 5.8000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 1.1600D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-3.4000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        +   

      DATA ( RTDAT( 3,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00,-1.3100D+03, 0.0000D+00, 1.8800D+02, & ! O   
     &     0.0000D+00,-2.0600D+03, 0.0000D+00, 0.0000D+00, 1.1500D+02, & ! +   
     &     0.0000D+00,-9.4000D+02, 6.9300D+02, 1.1000D+02, 2.2400D+02, & ! 1   
     &     9.4500D+02, 0.0000D+00, 2.5000D+02, 1.9000D-33, 2.6600D-54, & ! +   
     &     0.0000D+00,-1.6000D+02,-2.0000D+03, 5.3000D+02, 2.7000D+02, & ! 2   
     &    -2.4700D+03, 0.0000D+00, 0.0000D+00, 1.1000D+02,-1.2600D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-2.4500D+03, & ! 3   
     &     0.0000D+00,-1.1000D+04, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.6000D+02, 0.0000D+00, & ! 4   
     &     2.7000D-17, 0.0000D+00, 0.0000D+00,-1.0650D+04, 0.0000D+00, & ! +   
     &     6.9000D+02, 0.0000D+00, 2.9000D+02, 0.0000D+00,-1.2100D+04, & ! 5   
     &     0.0000D+00, 9.8000D+02, 8.0000D+02, 5.0000D+02, 5.0000D+02, & ! +   
     &     3.4000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 9.8000D+02, & ! 6   
     &     8.0000D+02, 5.0000D+02, 3.6000D+02, 8.0000D+02, 5.0000D+02, & ! +   
     &     3.6000D+02, 7.8000D+02, 5.0000D+02, 0.0000D+00, 3.6000D+02, & ! 7   
     &     8.0000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 1.9000D+02, 0.0000D+00, 1.9000D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 8.5000D+02, 1.9000D+02, & ! 9   
     &     1.3500D+02, 0.0000D+00, 0.0000D+00,-1.6000D+03, 0.0000D+00, & ! +   
     &     6.2500D+02,-7.0000D+03, 0.0000D+00, 2.3000D+03,-1.1000D+03, & ! O   
     &     3.4500D+02,-1.8600D+03, 0.0000D+00,-8.7000D+02, 4.0500D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.8600D+03, & ! 1   
     &     3.4000D+02, 0.0000D+00,-1.8600D+03, 0.0000D+00,-1.8600D+03, & ! +   
     &     5.7500D+02,-2.1000D+03, 3.4300D-33,-1.6900D+03,-1.0000D+03, & ! 2   
     &    -3.4500D+02, 2.0000D+01, 0.0000D+00, 0.0000D+00,-6.2060D+02, & ! +   
     &    -5.8500D+02, 0.0000D+00,-5.7800D+03,-2.0000D+02, 4.0000D+02, & ! 3   
     &     0.0000D+00,-7.9200D+02, 0.0000D+00,-2.5800D+03,-2.8800D+03, & ! +   
     &    -2.8000D+02, 0.0000D+00,-1.8800D+03,-1.1550D+03, 0.0000D+00, & ! 4   
     &     5.1900D+02,-1.0130D+03, 0.0000D+00, 3.9000D+02, 0.0000D+00, & ! +   
     &     3.6500D+02, 7.0000D+02, 0.0000D+00, 0.0000D+00,-8.3000D+03, & ! 5   
     &    -1.9950D+03,-4.4800D+02, 5.1100D+02,-1.7700D+03,-1.8600D+03, & ! +   
     &     0.0000D+00, 3.7200D+02, 0.0000D+00,-1.8600D+03,-4.0000D+02, & ! 6   
     &     7.0000D+02, 3.6500D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 4.4900D+02,-8.2100D+02, 1.7500D+02, 0.0000D+00, & ! 7   
     &     4.4900D+02,-8.2100D+02, 1.7500D+02,-1.9000D+02, 3.6000D+02, & ! +   
     &     0.0000D+00, 1.3000D+03, 0.0000D+00, 3.4000D+02, 3.6000D+02, & ! 8   
     &     0.0000D+00, 1.3000D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.6000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, 9.5000D+02, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-5.0000D+02, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00,-5.0000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.6900D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00,-2.0000D+02, 0.0000D+00, 2.9000D+02, 3.4000D+02, & ! +   
     &    -1.1000D+02, 0.0000D+00, 0.0000D+00,-1.2400D+03, 0.0000D+00, & ! 3   
     &     0.0000D+00,-1.0000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-3.4000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     4.5000D+01, 5.8000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 3.6000D+02, 1.3000D+03, 3.6000D+02, 1.3000D+03, & ! +   
     &     3.6000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     3.6000D+02, 1.3000D+03, 3.7400D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.8000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 1.3500D+02, 0.0000D+00,-1.6000D+03, 0.0000D+00, & ! +   
     &     0.0000D+00,-3.4000D+01, 3.4500D+02,-1.8600D+03,-1.1000D+03, & ! 1   
     &     0.0000D+00, 0.0000D+00, 4.4800D+02,-2.2830D+03, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.4000D+02, 0.0000D+00,-1.2565D+03, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 6.8000D+02/           !        +   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &      6,   17,   19,   20,   36,   37,   40,   45,   46,   48, & 
     &     49,   52,   54,   55,  123,  136,  138,  142,  221,  223, & 
     &    252,  278/

      DATA ( RFDAT( 1,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     2.3000D-11, 2.6000D-11, 9.8000D+02, 3.1800D+03, 1.9000D-12, & 
     &     9.7000D+14, 3.3000D-11, 2.8000D-11, 2.1990D+03, 4.7000D-12, & 
     &     4.8000D+15, 1.3000D-12, 1.2000D-11, 5.4000D+16, 0.0000D+00, & 
     &     1.0000D-12, 9.0000D-12, 3.0000D-11, 4.3000D-01, 4.3000D-01, & 
     &     1.5000D-11, 1.1998D+01/

      DATA ( RFDAT( 2,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     2.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D-01, & 
     &     1.0000D-01,-3.0000D-01, 0.0000D+00, 6.5000D-34, 0.0000D+00, & 
     &     0.0000D+00,-7.0000D-01,-9.0000D-01, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00,-8.5000D-01,-1.0000D+00,-8.0000D+00,-8.0000D+00, & 
     &    -1.9000D+00, 0.0000D+00/

      DATA ( RFDAT( 3,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &    -1.1080D+04, 0.0000D+00, 0.0000D+00, 1.3350D+03, 0.0000D+00, & 
     &    -1.1170D+04, 0.0000D+00, 0.0000D+00,-1.3830D+04, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00/

      DATA ( RFDAT( 4,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     6.0000D-01, 5.0000D-01, 0.0000D+00, 0.0000D+00, 3.5000D-01, & 
     &     3.5000D-01, 8.1000D-01, 6.0000D-01, 0.0000D+00, 6.0000D-01, & 
     &     6.0000D-01, 5.3000D-01, 3.0000D-01, 3.0000D-01, 0.0000D+00, & 
     &     3.7000D-01, 4.8000D-01, 5.0000D-01, 4.1000D-01, 4.1000D-01, & 
     &     6.0000D-01, 0.0000D+00/

      DATA ( RFDAT( 5,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     1.0000D+00, 1.1300D+00, 0.0000D+00, 0.0000D+00, 1.3300D+00, & 
     &     1.3300D+00, 8.7000D-01, 1.0000D+00, 0.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 1.1000D+00, 1.4100D+00, 1.4100D+00, 0.0000D+00, & 
     &     1.3000D+00, 1.1500D+00, 1.1300D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 0.0000D+00/

      REAL               :: SC( NRXNS,MXPRD )

      DATA ( SC( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    1.00000,    2.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    2.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    2.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    2.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.59000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        0.60000,    0.41000,    1.00000,    2.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.60000,    0.41000, & ! 6   
     &        0.80000,    2.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.90000,    1.00000,    0.68500,    1.00000, & ! 7   
     &        1.00000,    0.80000,    0.60000,    1.00000,    1.00000, & ! +   
     &        0.80000,    1.00000,    0.50000,    1.00000,    0.80000, & ! 8   
     &        1.00000,    0.60000,    1.00000,    0.54000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    2.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.50000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.20000,    0.74000,    1.00000, & ! 1   
     &        1.80000,    2.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.99100, & ! 2   
     &        1.00000,    0.95000,    0.50000,    0.38000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.20000,    1.00000,    1.00000, & ! 3   
     &        0.70000,    1.00000,    1.00000,    1.00000,    0.50000, & ! +   
     &        0.20000,    0.78100,    0.29500,    0.50000,    1.24000, & ! 4   
     &        1.30000,    0.73200,    0.50000,    1.00000,    0.75000, & ! +   
     &        0.10000,    0.88000,    0.59800,    0.59800,    1.00000, & ! 5   
     &        0.60000,    0.35000,    0.02200,    0.04000,    0.71700, & ! +   
     &        0.76000,    0.90400,    1.00000,    1.00000,    1.00000, & ! 6   
     &        0.27500,    0.27500,    0.22000,    0.27500,    0.63000, & ! +   
     &        0.15000,    0.75000,    0.57000,    0.47000,    0.15000, & ! 7   
     &        0.75000,    0.57000,    0.47000,    0.53000,    0.91800, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.18000,    0.86000, & ! 8   
     &        0.48000,    0.00000,    0.48000,    0.15500,    0.15500, & ! +   
     &        0.86000,    0.00000,    0.26000,    0.26000,    0.02500, & ! 9   
     &        0.30000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.40000,    1.00000,    1.20000,    0.50000, & ! O   
     &        1.00000,    0.60000,    1.40000,    1.00000,    0.14000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.41000, & ! 1   
     &        1.00000,    0.80000,    0.50000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.73200,    1.00000,    0.12600,    2.00000, & ! 2   
     &        1.00000,    1.00000,    0.30000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.30000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 7   
     &        1.00000,    0.50000,    0.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    0.00000,    1.00000,    0.90700, & ! +   
     &        0.92500,    0.94300,    0.50000,    0.50000,    1.50000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        0.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.50000,    1.00000, & ! 3   
     &        1.00000,    0.50000,    0.50000/           !         &  

      DATA ( SC( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &        1.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! 2   
     &        0.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.59000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! 5   
     &        0.60000,    0.15000,    0.00000,    2.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.60000,    0.15000, & ! 6   
     &        0.80000,    2.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.10000,    0.90000,    0.31500,    1.00000, & ! 7   
     &        0.00000,    0.80000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.20000,    0.00000,    0.50000,    0.00000,    0.80000, & ! 8   
     &        0.00000,    0.60000,    1.00000,    0.06000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 9   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.50000,    1.00000, & ! O   
     &        0.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.20000,    0.89000,    1.00000, & ! 1   
     &        0.20000,    2.00000,    1.50000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    1.00000,    0.99100, & ! 2   
     &        1.00000,    0.90000,    0.50000,    1.38000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.42000,    1.00000,    0.00000, & ! 3   
     &        0.70000,    1.00000,    1.00000,    0.51000,    0.50000, & ! +   
     &        0.30000,    0.48800,    0.55500,    0.50000,    0.66000, & ! 4   
     &        0.70000,    0.44200,    0.50000,    1.00000,    0.50000, & ! +   
     &        0.90000,    0.12000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        0.65000,    0.65000,    0.52100,    0.23100,    0.14200, & ! +   
     &        0.34000,    0.93300,    1.00000,    1.00000,    1.00000, & ! 6   
     &        0.27500,    0.27500,    0.22000,    0.27500,    0.37000, & ! +   
     &        5.12000,    0.50000,    0.07000,    0.28000,    5.12000, & ! 7   
     &        0.50000,    0.07000,    0.28000,    0.35200,    0.08200, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.65000,    0.14000, & ! 8   
     &        0.52000,    0.00000,    0.52000,    0.54400,    0.54400, & ! +   
     &        0.14000,    0.00000,    0.77000,    0.77000,    0.02500, & ! 9   
     &        1.00000,    0.00000,    0.00000,    0.50000,    0.50000, & ! +   
     &        1.00000,    1.00000,    0.40000,    0.50000,    0.50000, & ! O   
     &        1.00000,    0.40000,    0.24000,    1.00000,    0.20000, & ! +   
     &        1.00000,    0.50000,    0.00000,    1.00000,    0.15000, & ! 1   
     &        1.00000,    0.80000,    0.50000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.26800,    1.00000,    0.87400,    0.00000, & ! 2   
     &        1.00000,    0.00000,    1.40000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    0.99100,    2.00000,    0.33000,    0.70000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    0.00000,    0.18000,    0.15500,    0.15500, & ! +   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    0.01600,    0.14000,    0.01500,    0.19300, & ! +   
     &        0.03400,    0.14600,    1.00000,    1.00000,    1.00000, & ! 6   
     &        0.02800,    0.47300,    0.00600,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.50000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.68800,    1.00000,    0.42200,    0.71100, & ! 8   
     &        0.37000,    0.24000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        1.00000,    0.48570,    0.30030,    0.38560,    0.21810, & ! +   
     &        0.24120,    0.66640,    0.28580,    0.33030,    0.34440, & ! O   
     &        0.38860,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.58000,    0.52000,    0.04500, & ! +   
     &        0.58000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.50000,    1.00000, & ! 3   
     &        1.00000,    0.50000,    0.50000/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.41000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 5   
     &        0.40000,    0.15000,    0.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.40000,    0.15000, & ! 6   
     &        0.80000,    2.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.90000,    0.37000,    0.00000, & ! 7   
     &        0.00000,    0.20000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.80000,    0.00000,    0.00000,    0.00000,    0.20000, & ! 8   
     &        0.00000,    0.40000,    1.00000,    0.60000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.20000,    0.00000, & ! O   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.80000,    1.40000,    0.00000, & ! 1   
     &        0.20000,    0.00000,    0.50000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00900, & ! 2   
     &        0.00000,    0.10000,    0.50000,    1.38000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.74000,    0.00000,    0.00000, & ! 3   
     &        0.30000,    1.00000,    1.56000,    0.16000,    0.50000, & ! +   
     &        0.10000,    0.48800,    0.27000,    0.48000,    0.10000, & ! 4   
     &        1.00000,    0.12800,    0.48000,    1.00000,    0.25000, & ! +   
     &        0.67300,    0.12000,    0.72800,    0.72800,    0.00000, & ! 5   
     &        0.15000,    0.64000,    0.11500,    0.53100,    0.14200, & ! +   
     &        0.16000,    0.06700,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.27500,    0.27500,    0.22000,    0.27500,    1.00000, & ! +   
     &        1.00000,    0.25000,    0.69000,    0.75000,    1.00000, & ! 7   
     &        0.25000,    0.69000,    0.75000,    0.35200,    0.91800, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.72000,    0.41700, & ! 8   
     &        0.77000,    0.00000,    0.77000,    0.60200,    0.60200, & ! +   
     &        0.22100,    0.00000,    0.35000,    0.35000,    1.00000, & ! 9   
     &        0.48000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.70000,    2.00000,    0.60000,    0.45000, & ! O   
     &        1.00000,    0.40000,    0.50000,    0.00000,    0.50000, & ! +   
     &        0.00000,    0.50000,    0.00000,    0.00000,    0.15000, & ! 1   
     &        1.00000,    1.80000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.26800,    0.00000,    0.12600,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! 3   
     &        0.97000,    0.99100,    1.00000,    0.67000,    0.45000, & ! +   
     &        0.96000,    1.00000,    0.00000,    0.00000,    1.00000, & ! 4   
     &        1.00000,    0.00000,    0.65000,    0.54400,    0.54400, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.05100,    0.00000,    0.02300,    0.00000, & ! +   
     &        0.39200,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.22500,    0.00000,    0.05200,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00620,    0.28620,    0.09500,    0.30630, & ! +   
     &        0.20890,    0.01430,    0.39310,    0.22720,    0.27490, & ! O   
     &        0.24210,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 3   
     &        0.00000,    1.00000,    1.00000/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.41000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.40000,    0.44000,    0.00000,    0.00000,    2.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.40000,    0.44000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.10000,    1.00000,    0.00000, & ! 7   
     &        0.00000,    0.80000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.80000, & ! 8   
     &        0.00000,    0.40000,    0.00000,    0.40000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.20000,    0.00000, & ! O   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.15000,    0.00000, & ! 1   
     &        1.00000,    0.00000,    0.50000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 2   
     &        0.00000,    0.10000,    0.50000,    0.62000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.37000,    0.00000,    0.00000, & ! 3   
     &        0.30000,    0.70000,    0.22000,    0.16000,    0.50000, & ! +   
     &        0.20000,    0.97600,    0.15000,    0.48000,    0.10000, & ! 4   
     &        1.00000,    0.24500,    0.48000,    0.00000,    0.25000, & ! +   
     &        0.90000,    0.12000,    0.07200,    0.07200,    0.00000, & ! 5   
     &        0.20000,    0.33000,    0.11500,    0.17000,    0.14200, & ! +   
     &        0.34000,    0.06700,    0.00000,    0.00000,    0.00000, & ! 6   
     &        1.12500,    0.12500,    0.10000,    0.12500,    0.44400, & ! +   
     &        0.00000,    1.50000,    0.18000,    0.25000,    0.00000, & ! 7   
     &        1.50000,    0.18000,    0.25000,    0.11800,    0.91800, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.10000,    0.44300, & ! 8   
     &        0.23000,    0.00000,    0.23000,    0.24400,    0.24400, & ! +   
     &        0.67500,    0.00000,    0.65000,    0.65000,    0.20000, & ! 9   
     &        0.12000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.70000,    2.00000,    0.10000,    0.45000, & ! O   
     &        0.00000,    0.40000,    0.12000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.80000,    0.00000,    0.00000,    0.44000, & ! 1   
     &        2.00000,    0.20000,    0.50000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.87400,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.03000,    0.00900,    1.00000,    2.00000,    0.55000, & ! +   
     &        0.04000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.72000,    0.60200,    0.60200, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.04700,    0.00000,    0.06000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.19100,    0.00000,    0.08100,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00250,    0.00410,    0.13730,    0.01530, & ! +   
     &        0.30000,    0.01230,    0.01390,    0.26070,    0.04910, & ! O   
     &        0.06400,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    1.00000/           !         &  

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
     &        0.40000,    0.44000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.40000,    0.44000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.90000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.19000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.07800,    0.50000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.04000,    0.00000,    0.00000, & ! 3   
     &        0.30000,    0.70000,    0.00000,    0.37000,    1.00000, & ! +   
     &        0.20000,    0.19500,    0.15000,    0.04000,    0.10000, & ! 4   
     &        0.00000,    0.50000,    0.04000,    0.00000,    0.25000, & ! +   
     &        0.81800,    0.12000,    0.80000,    1.07200,    0.00000, & ! 5   
     &        0.35000,    0.03000,    0.26900,    0.17000,    0.14200, & ! +   
     &        0.20800,    0.02900,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.82500,    0.82500,    0.66000,    0.82500,    0.18500, & ! +   
     &        0.00000,    0.28000,    0.94000,    1.28000,    0.00000, & ! 7   
     &        0.28000,    0.94000,    1.28000,    0.11800,    0.91800, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.10000,    0.66000, & ! 8   
     &        1.00000,    0.00000,    1.00000,    0.24400,    0.24400, & ! +   
     &        0.30000,    0.00000,    1.00000,    1.00000,    0.73200, & ! 9   
     &        0.24000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.30000,    0.00000,    0.50000,    0.10000, & ! O   
     &        0.00000,    0.00000,    0.08000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.20000,    0.00000,    0.00000,    0.44000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        1.00000,    1.00000,    0.00000,    1.00000,    0.30000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.10000,    0.24400,    0.24400, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00260,    0.00350,    0.00050,    0.10430, & ! +   
     &        0.20280,    0.12390,    0.10270,    0.07020,    0.25770, & ! O   
     &        0.03850,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

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
     &        0.00000,    0.44000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.40000,    0.44000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.11000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.01100,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.94000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.30000,    0.00000,    0.00000,    1.12500, & ! +   
     &        0.20000,    0.02400,    0.33400,    1.00000,    0.10000, & ! 4   
     &        0.00000,    0.30000,    1.00000,    0.00000,    0.25000, & ! +   
     &        0.08200,    0.00000,    0.20000,    0.00000,    0.00000, & ! 5   
     &        0.26600,    1.00000,    0.26900,    0.54300,    0.11300, & ! +   
     &        0.26000,    0.02900,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.37500,    0.37500,    0.30000,    0.37500,    0.10400, & ! +   
     &        0.00000,    1.66000,    0.24000,    0.47000,    0.00000, & ! 7   
     &        1.66000,    0.24000,    0.47000,    0.53000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.07000,    0.20000, & ! 8   
     &        1.00000,    0.00000,    1.00000,    0.05800,    0.05800, & ! +   
     &        0.56000,    0.00000,    1.00000,    1.00000,    0.02000, & ! 9   
     &        0.24000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.30000,    1.00000, & ! O   
     &        0.00000,    0.00000,    0.02000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.44000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,   -0.12600,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,   -1.00000,    0.30000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.10000,    0.24400,    0.24400, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00230,    0.22390,    0.20510,    0.18930, & ! +   
     &        0.04710,    0.18310,    0.20450,    0.11160,    0.07390, & ! O   
     &        0.26670,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.44000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.11000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,   -2.50000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.98000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.01000,    1.19500,    0.08000,    0.50000,    0.00000, & ! 4   
     &        0.00000,    0.30000,    0.50000,    0.00000,    0.25000, & ! +   
     &        0.08200,    0.00000,    0.87200,    0.00000,    0.00000, & ! 5   
     &        0.20000,    0.35000,    0.45700,    0.46100,    0.11300, & ! +   
     &        0.24000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.07400,    1.00000,    0.20000,    0.25100,    0.59200, & ! +   
     &        0.00000,    0.47000,    0.00100,    0.53000,    0.00000, & ! 7   
     &        0.47000,    0.00100,    0.53000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.18000,    0.86000, & ! 8   
     &        1.00000,    0.00000,    0.00000,    0.15500,    0.15500, & ! +   
     &        0.86000,    0.00000,    1.00000,    0.00000,    0.02000, & ! 9   
     &        0.48000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.30000,    0.25000, & ! O   
     &        0.00000,    0.00000,    1.98000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.44000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.70000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.07000,    0.05800,    0.05800, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.29440,    0.18200,    0.17640,    0.16680, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    0.11000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.02000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.21000,   -0.73000,    0.37800,    0.25000,    0.00000, & ! 4   
     &        0.00000,    0.24000,    0.62500,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.20000,    0.35000,    0.11700,    0.15000,    0.71700, & ! +   
     &        0.24000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.25100,    0.25100,    1.74000,    2.17500,    0.33100, & ! +   
     &        0.00000,    1.00000,    7.00000,    1.00000,    0.00000, & ! 7   
     &        1.00000,    7.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.10000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.25000, & ! O   
     &        0.00000,    0.00000,    0.56000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.18000,    0.15500,    0.15500, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.20210,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,   -2.70000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.20000,    0.00000,    0.07500,    0.37500,    0.00000, & ! 4   
     &        0.00000,    0.06000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.06600,    1.00000,    0.13700,    0.39800,    0.71700, & ! +   
     &        0.17000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        2.17500,    2.17500,    0.80000,    1.00000,    0.18500, & ! +   
     &        0.00000,    0.00000,    0.21000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.21000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.70000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00190,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.10000,    0.00000,    0.07500,   -1.00000,    0.00000, & ! 4   
     &        0.00000,    0.29000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.06600,    0.00000,    0.13700,    0.14300,    0.28400, & ! +   
     &        0.12800,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.20000,    0.00000,    2.70000, & ! +   
     &        0.00000,    0.00000,    0.39000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.39000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00230,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 11 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.09000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.08000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.13700,    0.00000,    0.00000, & ! +   
     &        0.84000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.80000,    0.00000,    0.09800, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 12 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.13000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.08000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.65800,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.07800, & ! +   
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
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 13 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.04000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.26600, & ! +   
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
     &        0.00000,    0.00000,    0.00000/           !         &  

      DATA ( SC( IRXXN, 14 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,   -0.79000,    0.00000,    0.00000, & ! 4   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000/           !         &  

      INTEGER            :: NREACT( NRXNS )

      DATA ( NREACT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    2,    2,    2,    2,    2,    1,    1,    1, & ! O   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      1,    2,    2,    2,    2,    2,    1,    1,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    1,    1,    1,    2, & ! 3   
     &      2,    2,    1,    2,    2,    2,    1,    2,    1,    1, & ! 4   
     &      2,    2,    2,    2,    1,    1,    2,    2,    2,    2, & ! 5   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    1,    2,    1, & ! 8   
     &      2,    1,    2,    2,    2,    2,    1,    1,    2,    2, & ! 9   
     &      2,    1,    2,    2,    2,    2,    2,    1,    2,    2, & ! O   
     &      2,    1,    2,    1,    2,    2,    1,    2,    1,    2, & ! 1   
     &      2,    1,    2,    1,    2,    2,    2,    1,    1,    2, & ! 2   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 5   
     &      1,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      1,    1,    2,    2,    2,    1,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    1,    2,    2,    2,    2,    2,    2, & ! 1   
     &      1,    1,    1,    1,    1,    1,    2,    2,    2,    2, & ! 2   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      1,    2,    1,    1,    1,    1,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    1, & ! 6   
     &      1,    1,    1,    2,    2,    2,    1,    1,    1,    1, & ! 7   
     &      1,    2,    2,    2,    2,    2,    2,    1,    1,    1, & ! 8   
     &      1,    1,    1,    1,    1,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! O   
     &      1,    2,    2,    2,    2,    1,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    1, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2/     !  3   
      INTEGER            :: NPRDCT( NRXNS )

      DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    1,    1,    1,    1,    1,    0,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    0,    1,    1, & ! 1   
     &      1,    1,    2,    1,    2,    1,    2,    1,    1,    2, & ! 2   
     &      1,    2,    2,    1,    1,    1,    2,    2,    1,    1, & ! 3   
     &      1,    2,    2,    1,    1,    1,    2,    1,    2,    4, & ! 4   
     &      1,    3,    3,    1,    2,    5,    6,    1,    2,    4, & ! 5   
     &      4,    1,    2,    6,    7,    3,    3,    1,    1,    0, & ! 6   
     &      3,    2,    5,    4,    2,    1,    4,    2,    1,    1, & ! 7   
     &      3,    1,    2,    1,    4,    1,    4,    3,    4,    2, & ! 8   
     &      1,    1,    1,    2,    1,    2,    2,    1,    3,    3, & ! 9   
     &      1,    2,    3,    4,    2,    1,    2,    4,    2,    1, & ! O   
     &      2,    5,    3,    8,    2,    4,    2,    5,    3,    4, & ! 1   
     &      2,    1,    1,    2,    4,    2,    6,    7,    4,    4, & ! 2   
     &      1,    1,    9,    2,    1,    5,    6,    4,    5,    6, & ! 3   
     &     10,    8,   14,   10,    6,    4,   12,    9,    3,    7, & ! 4   
     &      7,    5,    7,    5,    2,   10,    9,   12,   10,   10, & ! 5   
     &     11,    6,    2,    2,    2,    9,    9,   11,    9,   13, & ! 6   
     &      3,    8,   11,    8,    3,    8,   11,    7,    7,    5, & ! 7   
     &      5,    0,    4,    8,    7,    7,    0,    6,    8,    8, & ! 8   
     &      7,    0,    7,    6,    7,    9,    1,    1,    2,    3, & ! 9   
     &      4,    5,    4,    7,    8,    3,    4,    8,    2,    3, & ! O   
     &      2,    5,    1,    2,    7,    4,    4,    4,    2,    2, & ! 1   
     &      2,    5,    2,    6,    1,    2,    1,    2,    2,    1, & ! 2   
     &      3,    2,    3,    3,    2,    5,    5,    4,    6,    8, & ! 3   
     &      5,    3,    2,    2,    3,    3,    1,    9,    9,    9, & ! 4   
     &      2,    1,    2,    2,    2,    2,    4,    2,    4,    2, & ! 5   
     &      3,    2,    2,    2,    2,    4,    2,    4,    1,    2, & ! 6   
     &      2,    1,    1,    1,    1,    1,    2,    0,    1,    1, & ! 7   
     &      1,    2,    2,    2,    2,    2,    2,    0,    1,    1, & ! 8   
     &      1,    1,    1,    1,    1,    2,   10,    7,    7,    7, & ! 9   
     &      6,    6,    6,    6,    6,    6,    1,    1,    1,    0, & ! O   
     &      0,    1,    1,    1,    1,    0,    1,    2,    2,    2, & ! 1   
     &      2,    1,    1,    1,    0,    1,    1,    1,    1,    0, & ! 2   
     &      1,    1,    1,    3,    2,    2,    3,    3/     !  3   

      INTEGER, PARAMETER :: NMPHOT =  41
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    8,    9,   21,   27,   28,   38,   43,   47,   50, & 
     &     56,   64,   88,   90,   92,   97,   98,  108,  112,  114, & 
     &    117,  119,  128,  129,  161,  163,  201,  202,  206,  225, & 
     &    226,  233,  251,  253,  254,  288,  310,  311,  316,  325, & 
     &    330/

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   11,   12,   12,   13,   14,   15,   16,   17,   18, & 
     &     19,   20,   21,   22,   23,   24,   13,    1,    1,   25, & 
     &     26,   27,   28,   29,   30,   31,   14,   15,   16,   32, & 
     &     32/

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & 
     &     31,   32,   33,   34,   35,   36,   37,   38,   39,   40, & 
     &     41/

      INTEGER, PARAMETER :: MHETERO =  14
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    255,  256,  269,  270,  271,  272,  273,  274,  275,  276, & 
     &    277,  279,  280,  281/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    8,    9, & 
     &     10,   11,   12,   13/

      INTEGER, PARAMETER :: NPHOTAB =  32
      CHARACTER( 16 )    :: PHOTAB( NPHOTAB )

      DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & 
     &   'NO2_IUPAC10     ', 'O3_O3P_IUPAC10  ', 'O3_O1D_IUPAC10  ', & 
     &   'H2O2_IUPAC10    ', 'NO3NO2_06       ', 'NO3NO_06        ', & 
     &   'N2O5_IUPAC10    ', 'HONO_IUPAC10    ', 'HNO3_IUPAC10    ', & 
     &   'PNA_IUPAC10     ', 'PAN_IUPAC10     ', 'MEPX_IUPAC10    ', & 
     &   'NTR_IUPAC10     ', 'FORM_R_IUPAC10  ', 'FORM_M_IUPAC10  ', & 
     &   'ALD2_R_IUPAC10  ', 'ALDX_R_IUPAC10  ', 'GLYD_IUPAC10    ', & 
     &   'GLY_R_IUPAC10   ', 'MGLY_IUPAC10    ', 'KET_IUPAC10     ', & 
     &   'ACET_IUPAC10    ', 'ISPD            ', 'HPALD           ', & 
     &   'CL2_IUPAC04     ', 'HOCL_IUPAC04    ', 'FMCL_IUPAC04    ', & 
     &   'CLNO2_IUPAC13   ', 'CLONO2_1        ', 'CLONO2_2        ', & 
     &   'IC3ONO2         ', 'ACRO_09         '/

      INTEGER, PARAMETER :: NHETERO =  13
      CHARACTER( 16 )    :: HETERO( NHETERO )

      DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & 
     &   'HETERO_CLNO3_WAJ', 'HETERO_CLNO3_WAK', 'HETERO_NTR2     ', &
     &   'HETERO_N2O5IJ   ', 'HETERO_N2O5K    ', 'HETERO_H2NO3PAIJ', &
     &   'HETERO_H2NO3PAK ', 'HETERO_H2NO3PBIJ', 'HETERO_H2NO3PBK ', &
     &   'HETERO_NO2      ', 'HETERO_IEPOX    ', 'HETERO_GLY      ', &
     &   'HETERO_MGLY     '/

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
     &    'R172            ', 'R173            ', 'R174            ', & ! 7   
     &    'R171a           ', 'R172a           ', 'R173a           ', & ! 8   
     &    'R174a           ', 'R175            ', 'R176            ', & ! 9   
     &    'R177            ', 'R178            ', 'R179            ', & ! 0   
     &    'R180            ', 'R181            ', 'R182            ', & ! 1   
     &    'R183            ', 'R184            ', 'R185            ', & ! 2   
     &    'R185a           ', 'R186            ', 'R187            ', & ! 3   
     &    'R188            ', 'R189            ', 'R190            ', & ! 4   
     &    'R191            ', 'R192            ', 'R193            ', & ! 5   
     &    'R194            ', 'R195            ', 'R196            ', & ! 6   
     &    'R197            ', 'R198            ', 'R199            ', & ! 7   
     &    'R200            ', 'R201            ', 'R202            ', & ! 8   
     &    'R203            ', 'R204            ', 'R205            ', & ! 9   
     &    'R206            ', 'R207            ', 'R208            ', & ! 0   
     &    'R209            ', 'R210            ', 'R211            ', & ! 1   
     &    'R212            ', 'R213            ', 'R214            ', & ! 2   
     &    'R216            ', 'R217            ', 'R218            ', & ! 3   
     &    'R219            ', 'R220            ', 'CL1             ', & ! 4   
     &    'CL2             ', 'CL3             ', 'CL4             ', & ! 5   
     &    'CL5             ', 'CL6             ', 'CL7             ', & ! 6   
     &    'CL8             ', 'CL9             ', 'CL10            ', & ! 7   
     &    'CL11            ', 'CL12            ', 'CL13            ', & ! 8   
     &    'CL14            ', 'CL15            ', 'CL16            ', & ! 9   
     &    'CL17            ', 'CL18            ', 'CL19            ', & ! 0   
     &    'CL20            ', 'CL21            ', 'CL22            ', & ! 1   
     &    'CL23            ', 'CL24            ', 'CL25            ', & ! 2   
     &    'CL26            ', 'CL27            ', 'CL28            ', & ! 3   
     &    'CL30            ', 'CL31            ', 'HET_CLNO3_WAJ   ', & ! 4   
     &    'HET_CLNO3_WAK   ', 'SA01            ', 'SA02            ', & ! 5   
     &    'SA03            ', 'SA04            ', 'SA06            ', & ! 6   
     &    'SA07            ', 'SA08            ', 'SA09            ', & ! 7   
     &    'SA10            ', 'SA11            ', 'SA12            ', & ! 8   
     &    'SA13            ', 'HET_NTR2        ', 'HET_N2O5IJ      ', & ! 9   
     &    'HET_N2O5K       ', 'HET_H2NO3PIJA   ', 'HET_H2NO3PKA    ', & ! 0   
     &    'HET_H2NO3PIB    ', 'HET_H2NO3PJB    ', 'HET_H2NO3PKB    ', & ! 1   
     &    'HET_N02         ', 'HAL_Ozone       ', 'HET_IEPOX       ', & ! 2   
     &    'HET_GLY         ', 'HET_MGLY        ', 'BL18a           ', & ! 3   
     &    'BL18b           ', 'BL18c           ', 'BL18d           ', & ! 4   
     &    'CP07mtp         ', 'BP70mtp         ', 'BP71mtp         ', & ! 5   
     &    'HYD_MT          ', 'OLIG_AROMATIC1  ', 'OLIG_AROMATIC2  ', & ! 6   
     &    'OLIG_AROMATIC3  ', 'OLIG_ISOPRENE1  ', 'OLIG_ISOPRENE2  ', & ! 7   
     &    'OLIG_SESQT1     ', 'PCSOA           ', 'POA_AGE1        ', & ! 8   
     &    'POA_AGE2        ', 'POA_AGE3        ', 'POA_AGE4        ', & ! 9   
     &    'POA_AGE5        ', 'POA_AGE6        ', 'POA_AGE7        ', & ! 0   
     &    'POA_AGE8        ', 'POA_AGE9        ', 'POA_AGE10       ', & ! 1   
     &    'T01             ', 'T02             ', 'T03             ', & ! 2   
     &    'T04             ', 'T05             ', 'TCL1            ', & ! 3   
     &    'T06             ', 'T07             ', 'T08             ', & ! 4   
     &    'T09             ', 'TCL2            ', 'T10             ', & ! 5   
     &    'T11             ', 'T12             ', 'TCL3            ', & ! 6   
     &    'T13             ', 'T14             ', 'T15             ', & ! 7   
     &    'T16             ', 'TCL4            ', 'T17             ', & ! 8   
     &    'T18             ', 'T19             ', 'T20             ', & ! 9   
     &    'TCL5            ', 'T21             ', 'TCL6            ', & ! 0   
     &    'HG1             ', 'HG2             ', 'HG3             ', & ! 1   
     &    'HG4             ', 'HG5             '/                   ! 2  

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
       INTEGER, PARAMETER  :: IJ_NO2_IUPAC10      =   1
       INTEGER, PARAMETER  :: IJ_O3_O3P_IUPAC10   =   2
       INTEGER, PARAMETER  :: IJ_O3_O1D_IUPAC10   =   3
       INTEGER, PARAMETER  :: IJ_H2O2_IUPAC10     =   4
       INTEGER, PARAMETER  :: IJ_NO3NO2_06        =   5
       INTEGER, PARAMETER  :: IJ_NO3NO_06         =   6
       INTEGER, PARAMETER  :: IJ_N2O5_IUPAC10     =   7
       INTEGER, PARAMETER  :: IJ_HONO_IUPAC10     =   8
       INTEGER, PARAMETER  :: IJ_HNO3_IUPAC10     =   9
       INTEGER, PARAMETER  :: IJ_PNA_IUPAC10      =  10
       INTEGER, PARAMETER  :: IJ_PAN_IUPAC10      =  11
       INTEGER, PARAMETER  :: IJ_MEPX_IUPAC10     =  12
       INTEGER, PARAMETER  :: IJ_NTR_IUPAC10      =  13
       INTEGER, PARAMETER  :: IJ_FORM_R_IUPAC10   =  14
       INTEGER, PARAMETER  :: IJ_FORM_M_IUPAC10   =  15
       INTEGER, PARAMETER  :: IJ_ALD2_R_IUPAC10   =  16
       INTEGER, PARAMETER  :: IJ_ALDX_R_IUPAC10   =  17
       INTEGER, PARAMETER  :: IJ_GLYD_IUPAC10     =  18
       INTEGER, PARAMETER  :: IJ_GLY_R_IUPAC10    =  19
       INTEGER, PARAMETER  :: IJ_MGLY_IUPAC10     =  20
       INTEGER, PARAMETER  :: IJ_KET_IUPAC10      =  21
       INTEGER, PARAMETER  :: IJ_ACET_IUPAC10     =  22
       INTEGER, PARAMETER  :: IJ_ISPD             =  23
       INTEGER, PARAMETER  :: IJ_HPALD            =  24
       INTEGER, PARAMETER  :: IJ_CL2_IUPAC04      =  25
       INTEGER, PARAMETER  :: IJ_HOCL_IUPAC04     =  26
       INTEGER, PARAMETER  :: IJ_FMCL_IUPAC04     =  27
       INTEGER, PARAMETER  :: IJ_CLNO2_IUPAC13    =  28
       INTEGER, PARAMETER  :: IJ_CLONO2_1         =  29
       INTEGER, PARAMETER  :: IJ_CLONO2_2         =  30
       INTEGER, PARAMETER  :: IJ_IC3ONO2          =  31
       INTEGER, PARAMETER  :: IJ_ACRO_09          =  32
       INTEGER, PARAMETER  :: IK_HETERO_CLNO3_WAJ =   1
       INTEGER, PARAMETER  :: IK_HETERO_CLNO3_WAK =   2
       INTEGER, PARAMETER  :: IK_HETERO_NTR2      =   3
       INTEGER, PARAMETER  :: IK_HETERO_N2O5IJ    =   4
       INTEGER, PARAMETER  :: IK_HETERO_N2O5K     =   5
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAIJ =   6
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAK  =   7
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBIJ =   8
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBK  =   9
       INTEGER, PARAMETER  :: IK_HETERO_NO2       =  10
       INTEGER, PARAMETER  :: IK_HETERO_IEPOX     =  11
       INTEGER, PARAMETER  :: IK_HETERO_GLY       =  12
       INTEGER, PARAMETER  :: IK_HETERO_MGLY      =  13
       END MODULE RXNS_DATA
