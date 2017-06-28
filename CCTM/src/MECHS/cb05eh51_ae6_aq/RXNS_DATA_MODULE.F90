       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: ../../CCTM/src/MECHS/cb05eh51_ae6_aq/mech_cb05eh51_ae6_aq.def
! for Mechanism Name: CB05EH51_AE6_AQ                 

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

      CHARACTER( 32 ), PARAMETER :: MECHNAME = 'CB05EH51_AE6_AQ'

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC = 167
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 188

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
      DATA GAS_CHEM_SPC(  16 ) / 'NTROH           ' /
      DATA GAS_CHEM_SPC(  17 ) / 'NTRALK          ' /
      DATA GAS_CHEM_SPC(  18 ) / 'ROOH            ' /
      DATA GAS_CHEM_SPC(  19 ) / 'ALD2            ' /
      DATA GAS_CHEM_SPC(  20 ) / 'ALDX            ' /
      DATA GAS_CHEM_SPC(  21 ) / 'ISOPX           ' /
      DATA GAS_CHEM_SPC(  22 ) / 'IEPOX           ' /
      DATA GAS_CHEM_SPC(  23 ) / 'ISOPO2          ' /
      DATA GAS_CHEM_SPC(  24 ) / 'IOLE            ' /
      DATA GAS_CHEM_SPC(  25 ) / 'IEPXO2          ' /
      DATA GAS_CHEM_SPC(  26 ) / 'MGLY            ' /
      DATA GAS_CHEM_SPC(  27 ) / 'FORM            ' /
      DATA GAS_CHEM_SPC(  28 ) / 'FACD            ' /
      DATA GAS_CHEM_SPC(  29 ) / 'CO              ' /
      DATA GAS_CHEM_SPC(  30 ) / 'PAR             ' /
      DATA GAS_CHEM_SPC(  31 ) / 'C2O3            ' /
      DATA GAS_CHEM_SPC(  32 ) / 'MEO2            ' /
      DATA GAS_CHEM_SPC(  33 ) / 'AACD            ' /
      DATA GAS_CHEM_SPC(  34 ) / 'MEPX            ' /
      DATA GAS_CHEM_SPC(  35 ) / 'MEOH            ' /
      DATA GAS_CHEM_SPC(  36 ) / 'HCO3            ' /
      DATA GAS_CHEM_SPC(  37 ) / 'PAN             ' /
      DATA GAS_CHEM_SPC(  38 ) / 'PACD            ' /
      DATA GAS_CHEM_SPC(  39 ) / 'CXO3            ' /
      DATA GAS_CHEM_SPC(  40 ) / 'PANX            ' /
      DATA GAS_CHEM_SPC(  41 ) / 'ROR             ' /
      DATA GAS_CHEM_SPC(  42 ) / 'OLE             ' /
      DATA GAS_CHEM_SPC(  43 ) / 'ETH             ' /
      DATA GAS_CHEM_SPC(  44 ) / 'TOL             ' /
      DATA GAS_CHEM_SPC(  45 ) / 'CRES            ' /
      DATA GAS_CHEM_SPC(  46 ) / 'TO2             ' /
      DATA GAS_CHEM_SPC(  47 ) / 'TOLRO2          ' /
      DATA GAS_CHEM_SPC(  48 ) / 'OPEN            ' /
      DATA GAS_CHEM_SPC(  49 ) / 'CRO             ' /
      DATA GAS_CHEM_SPC(  50 ) / 'CAT1            ' /
      DATA GAS_CHEM_SPC(  51 ) / 'CRON            ' /
      DATA GAS_CHEM_SPC(  52 ) / 'CRNO            ' /
      DATA GAS_CHEM_SPC(  53 ) / 'CRN2            ' /
      DATA GAS_CHEM_SPC(  54 ) / 'CRPX            ' /
      DATA GAS_CHEM_SPC(  55 ) / 'OPO3            ' /
      DATA GAS_CHEM_SPC(  56 ) / 'CAO2            ' /
      DATA GAS_CHEM_SPC(  57 ) / 'OPAN            ' /
      DATA GAS_CHEM_SPC(  58 ) / 'XYLMN           ' /
      DATA GAS_CHEM_SPC(  59 ) / 'XYLRO2          ' /
      DATA GAS_CHEM_SPC(  60 ) / 'NAPH            ' /
      DATA GAS_CHEM_SPC(  61 ) / 'PAHRO2          ' /
      DATA GAS_CHEM_SPC(  62 ) / 'ISOP            ' /
      DATA GAS_CHEM_SPC(  63 ) / 'ISPD            ' /
      DATA GAS_CHEM_SPC(  64 ) / 'ISOPRXN         ' /
      DATA GAS_CHEM_SPC(  65 ) / 'NTRM            ' /
      DATA GAS_CHEM_SPC(  66 ) / 'MACO3           ' /
      DATA GAS_CHEM_SPC(  67 ) / 'NTRI            ' /
      DATA GAS_CHEM_SPC(  68 ) / 'TERP            ' /
      DATA GAS_CHEM_SPC(  69 ) / 'TRPRXN          ' /
      DATA GAS_CHEM_SPC(  70 ) / 'XO2T            ' /
      DATA GAS_CHEM_SPC(  71 ) / 'SO2             ' /
      DATA GAS_CHEM_SPC(  72 ) / 'SULF            ' /
      DATA GAS_CHEM_SPC(  73 ) / 'SULRXN          ' /
      DATA GAS_CHEM_SPC(  74 ) / 'ETOH            ' /
      DATA GAS_CHEM_SPC(  75 ) / 'ETHA            ' /
      DATA GAS_CHEM_SPC(  76 ) / 'CL2             ' /
      DATA GAS_CHEM_SPC(  77 ) / 'CL              ' /
      DATA GAS_CHEM_SPC(  78 ) / 'HOCL            ' /
      DATA GAS_CHEM_SPC(  79 ) / 'CLO             ' /
      DATA GAS_CHEM_SPC(  80 ) / 'FMCL            ' /
      DATA GAS_CHEM_SPC(  81 ) / 'HCL             ' /
      DATA GAS_CHEM_SPC(  82 ) / 'CLNO2           ' /
      DATA GAS_CHEM_SPC(  83 ) / 'TOLNRXN         ' /
      DATA GAS_CHEM_SPC(  84 ) / 'TOLHRXN         ' /
      DATA GAS_CHEM_SPC(  85 ) / 'XYLNRXN         ' /
      DATA GAS_CHEM_SPC(  86 ) / 'XYLHRXN         ' /
      DATA GAS_CHEM_SPC(  87 ) / 'BENZENE         ' /
      DATA GAS_CHEM_SPC(  88 ) / 'BENZRO2         ' /
      DATA GAS_CHEM_SPC(  89 ) / 'BNZNRXN         ' /
      DATA GAS_CHEM_SPC(  90 ) / 'BNZHRXN         ' /
      DATA GAS_CHEM_SPC(  91 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC(  92 ) / 'SESQRXN         ' /
      DATA GAS_CHEM_SPC(  93 ) / 'PAHNRXN         ' /
      DATA GAS_CHEM_SPC(  94 ) / 'PAHHRXN         ' /
      DATA GAS_CHEM_SPC(  95 ) / 'SOAALK          ' /
      DATA GAS_CHEM_SPC(  96 ) / 'ALKRXN          ' /
      DATA GAS_CHEM_SPC(  97 ) / 'MAPAN           ' /
      DATA GAS_CHEM_SPC(  98 ) / 'NALKO2          ' /
      DATA GAS_CHEM_SPC(  99 ) / 'NTRCN           ' /
      DATA GAS_CHEM_SPC( 100 ) / 'NTRCNOH         ' /
      DATA GAS_CHEM_SPC( 101 ) / 'NTRPX           ' /
      DATA GAS_CHEM_SPC( 102 ) / 'NOHO2           ' /
      DATA GAS_CHEM_SPC( 103 ) / 'NCNO2           ' /
      DATA GAS_CHEM_SPC( 104 ) / 'NCNOHO2         ' /
      DATA GAS_CHEM_SPC( 105 ) / 'NTRMO2          ' /
      DATA GAS_CHEM_SPC( 106 ) / 'NTRIO2          ' /
      DATA GAS_CHEM_SPC( 107 ) / 'H2NO3PIJ        ' /
      DATA GAS_CHEM_SPC( 108 ) / 'H2NO3PK         ' /
      DATA GAS_CHEM_SPC( 109 ) / 'PCVOC           ' /
      DATA GAS_CHEM_SPC( 110 ) / 'PCSOARXN        ' /
      DATA GAS_CHEM_SPC( 111 ) / 'VLVPO1          ' /
      DATA GAS_CHEM_SPC( 112 ) / 'VSVPO1          ' /
      DATA GAS_CHEM_SPC( 113 ) / 'VSVPO2          ' /
      DATA GAS_CHEM_SPC( 114 ) / 'VSVPO3          ' /
      DATA GAS_CHEM_SPC( 115 ) / 'VIVPO1          ' /
      DATA GAS_CHEM_SPC( 116 ) / 'VLVOO1          ' /
      DATA GAS_CHEM_SPC( 117 ) / 'VLVOO2          ' /
      DATA GAS_CHEM_SPC( 118 ) / 'VSVOO2          ' /
      DATA GAS_CHEM_SPC( 119 ) / 'VSVOO3          ' /
      DATA GAS_CHEM_SPC( 120 ) / 'VSVOO1          ' /
      DATA GAS_CHEM_SPC( 121 ) / 'FORM_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 122 ) / 'ALD2_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 123 ) / 'BUTADIENE13     ' /
      DATA GAS_CHEM_SPC( 124 ) / 'ACROLEIN        ' /
      DATA GAS_CHEM_SPC( 125 ) / 'ACRO_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 126 ) / 'TOLU            ' /
      DATA GAS_CHEM_SPC( 127 ) / 'MXYL            ' /
      DATA GAS_CHEM_SPC( 128 ) / 'OXYL            ' /
      DATA GAS_CHEM_SPC( 129 ) / 'PXYL            ' /
      DATA GAS_CHEM_SPC( 130 ) / 'APIN            ' /
      DATA GAS_CHEM_SPC( 131 ) / 'BPIN            ' /
      DATA GAS_CHEM_SPC( 132 ) / 'HG              ' /
      DATA GAS_CHEM_SPC( 133 ) / 'HGIIAER         ' /
      DATA GAS_CHEM_SPC( 134 ) / 'HGIIGAS         ' /
      DATA GAS_CHEM_SPC( 135 ) / 'I               ' /
      DATA GAS_CHEM_SPC( 136 ) / 'IO              ' /
      DATA GAS_CHEM_SPC( 137 ) / 'OIO             ' /
      DATA GAS_CHEM_SPC( 138 ) / 'HI              ' /
      DATA GAS_CHEM_SPC( 139 ) / 'HOI             ' /
      DATA GAS_CHEM_SPC( 140 ) / 'I2O2            ' /
      DATA GAS_CHEM_SPC( 141 ) / 'I2O3            ' /
      DATA GAS_CHEM_SPC( 142 ) / 'I2O4            ' /
      DATA GAS_CHEM_SPC( 143 ) / 'I2              ' /
      DATA GAS_CHEM_SPC( 144 ) / 'IONO2           ' /
      DATA GAS_CHEM_SPC( 145 ) / 'DMS             ' /
      DATA GAS_CHEM_SPC( 146 ) / 'MSA             ' /
      DATA GAS_CHEM_SPC( 147 ) / 'INO2            ' /
      DATA GAS_CHEM_SPC( 148 ) / 'INO             ' /
      DATA GAS_CHEM_SPC( 149 ) / 'BRO             ' /
      DATA GAS_CHEM_SPC( 150 ) / 'BR              ' /
      DATA GAS_CHEM_SPC( 151 ) / 'ICL             ' /
      DATA GAS_CHEM_SPC( 152 ) / 'CH3I            ' /
      DATA GAS_CHEM_SPC( 153 ) / 'MI2             ' /
      DATA GAS_CHEM_SPC( 154 ) / 'MIB             ' /
      DATA GAS_CHEM_SPC( 155 ) / 'MIC             ' /
      DATA GAS_CHEM_SPC( 156 ) / 'HOBR            ' /
      DATA GAS_CHEM_SPC( 157 ) / 'HBR             ' /
      DATA GAS_CHEM_SPC( 158 ) / 'BR2             ' /
      DATA GAS_CHEM_SPC( 159 ) / 'BRONO2          ' /
      DATA GAS_CHEM_SPC( 160 ) / 'MB3             ' /
      DATA GAS_CHEM_SPC( 161 ) / 'CH3BR           ' /
      DATA GAS_CHEM_SPC( 162 ) / 'MB2             ' /
      DATA GAS_CHEM_SPC( 163 ) / 'MB2C            ' /
      DATA GAS_CHEM_SPC( 164 ) / 'MBC2            ' /
      DATA GAS_CHEM_SPC( 165 ) / 'MBC             ' /
      DATA GAS_CHEM_SPC( 166 ) / 'BRNO2           ' /
      DATA GAS_CHEM_SPC( 167 ) / 'BRCL            ' /




      LOGICAL   :: HALOGEN_PARMAETER = .FALSE. 

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
      DATA CHEMISTRY_SPC(  16 ), SPECIES_MOLWT(  16 ) / 'NTROH           ',  135.00 /
      DATA CHEMISTRY_SPC(  17 ), SPECIES_MOLWT(  17 ) / 'NTRALK          ',  119.00 /
      DATA CHEMISTRY_SPC(  18 ), SPECIES_MOLWT(  18 ) / 'ROOH            ',   62.00 /
      DATA CHEMISTRY_SPC(  19 ), SPECIES_MOLWT(  19 ) / 'ALD2            ',   44.00 /
      DATA CHEMISTRY_SPC(  20 ), SPECIES_MOLWT(  20 ) / 'ALDX            ',   44.00 /
      DATA CHEMISTRY_SPC(  21 ), SPECIES_MOLWT(  21 ) / 'ISOPX           ',  118.10 /
      DATA CHEMISTRY_SPC(  22 ), SPECIES_MOLWT(  22 ) / 'IEPOX           ',  118.10 /
      DATA CHEMISTRY_SPC(  23 ), SPECIES_MOLWT(  23 ) / 'ISOPO2          ',  117.10 /
      DATA CHEMISTRY_SPC(  24 ), SPECIES_MOLWT(  24 ) / 'IOLE            ',   48.00 /
      DATA CHEMISTRY_SPC(  25 ), SPECIES_MOLWT(  25 ) / 'IEPXO2          ',  149.10 /
      DATA CHEMISTRY_SPC(  26 ), SPECIES_MOLWT(  26 ) / 'MGLY            ',   72.00 /
      DATA CHEMISTRY_SPC(  27 ), SPECIES_MOLWT(  27 ) / 'FORM            ',   30.00 /
      DATA CHEMISTRY_SPC(  28 ), SPECIES_MOLWT(  28 ) / 'FACD            ',   46.00 /
      DATA CHEMISTRY_SPC(  29 ), SPECIES_MOLWT(  29 ) / 'CO              ',   28.00 /
      DATA CHEMISTRY_SPC(  30 ), SPECIES_MOLWT(  30 ) / 'PAR             ',   14.00 /
      DATA CHEMISTRY_SPC(  31 ), SPECIES_MOLWT(  31 ) / 'C2O3            ',   75.00 /
      DATA CHEMISTRY_SPC(  32 ), SPECIES_MOLWT(  32 ) / 'MEO2            ',   47.00 /
      DATA CHEMISTRY_SPC(  33 ), SPECIES_MOLWT(  33 ) / 'AACD            ',   60.00 /
      DATA CHEMISTRY_SPC(  34 ), SPECIES_MOLWT(  34 ) / 'MEPX            ',   48.00 /
      DATA CHEMISTRY_SPC(  35 ), SPECIES_MOLWT(  35 ) / 'MEOH            ',   32.00 /
      DATA CHEMISTRY_SPC(  36 ), SPECIES_MOLWT(  36 ) / 'HCO3            ',   63.00 /
      DATA CHEMISTRY_SPC(  37 ), SPECIES_MOLWT(  37 ) / 'PAN             ',  121.00 /
      DATA CHEMISTRY_SPC(  38 ), SPECIES_MOLWT(  38 ) / 'PACD            ',   76.00 /
      DATA CHEMISTRY_SPC(  39 ), SPECIES_MOLWT(  39 ) / 'CXO3            ',   75.00 /
      DATA CHEMISTRY_SPC(  40 ), SPECIES_MOLWT(  40 ) / 'PANX            ',  121.00 /
      DATA CHEMISTRY_SPC(  41 ), SPECIES_MOLWT(  41 ) / 'ROR             ',   31.00 /
      DATA CHEMISTRY_SPC(  42 ), SPECIES_MOLWT(  42 ) / 'OLE             ',   27.00 /
      DATA CHEMISTRY_SPC(  43 ), SPECIES_MOLWT(  43 ) / 'ETH             ',   28.00 /
      DATA CHEMISTRY_SPC(  44 ), SPECIES_MOLWT(  44 ) / 'TOL             ',   92.00 /
      DATA CHEMISTRY_SPC(  45 ), SPECIES_MOLWT(  45 ) / 'CRES            ',  108.00 /
      DATA CHEMISTRY_SPC(  46 ), SPECIES_MOLWT(  46 ) / 'TO2             ',  173.00 /
      DATA CHEMISTRY_SPC(  47 ), SPECIES_MOLWT(  47 ) / 'TOLRO2          ',  141.00 /
      DATA CHEMISTRY_SPC(  48 ), SPECIES_MOLWT(  48 ) / 'OPEN            ',   84.00 /
      DATA CHEMISTRY_SPC(  49 ), SPECIES_MOLWT(  49 ) / 'CRO             ',  107.00 /
      DATA CHEMISTRY_SPC(  50 ), SPECIES_MOLWT(  50 ) / 'CAT1            ',  124.00 /
      DATA CHEMISTRY_SPC(  51 ), SPECIES_MOLWT(  51 ) / 'CRON            ',  153.00 /
      DATA CHEMISTRY_SPC(  52 ), SPECIES_MOLWT(  52 ) / 'CRNO            ',  152.00 /
      DATA CHEMISTRY_SPC(  53 ), SPECIES_MOLWT(  53 ) / 'CRN2            ',  168.00 /
      DATA CHEMISTRY_SPC(  54 ), SPECIES_MOLWT(  54 ) / 'CRPX            ',  169.00 /
      DATA CHEMISTRY_SPC(  55 ), SPECIES_MOLWT(  55 ) / 'OPO3            ',  115.00 /
      DATA CHEMISTRY_SPC(  56 ), SPECIES_MOLWT(  56 ) / 'CAO2            ',  133.00 /
      DATA CHEMISTRY_SPC(  57 ), SPECIES_MOLWT(  57 ) / 'OPAN            ',  161.00 /
      DATA CHEMISTRY_SPC(  58 ), SPECIES_MOLWT(  58 ) / 'XYLMN           ',  106.00 /
      DATA CHEMISTRY_SPC(  59 ), SPECIES_MOLWT(  59 ) / 'XYLRO2          ',  155.00 /
      DATA CHEMISTRY_SPC(  60 ), SPECIES_MOLWT(  60 ) / 'NAPH            ',  128.20 /
      DATA CHEMISTRY_SPC(  61 ), SPECIES_MOLWT(  61 ) / 'PAHRO2          ',  187.20 /
      DATA CHEMISTRY_SPC(  62 ), SPECIES_MOLWT(  62 ) / 'ISOP            ',   68.00 /
      DATA CHEMISTRY_SPC(  63 ), SPECIES_MOLWT(  63 ) / 'ISPD            ',   70.00 /
      DATA CHEMISTRY_SPC(  64 ), SPECIES_MOLWT(  64 ) / 'ISOPRXN         ',   68.00 /
      DATA CHEMISTRY_SPC(  65 ), SPECIES_MOLWT(  65 ) / 'NTRM            ',  147.00 /
      DATA CHEMISTRY_SPC(  66 ), SPECIES_MOLWT(  66 ) / 'MACO3           ',  102.00 /
      DATA CHEMISTRY_SPC(  67 ), SPECIES_MOLWT(  67 ) / 'NTRI            ',  149.10 /
      DATA CHEMISTRY_SPC(  68 ), SPECIES_MOLWT(  68 ) / 'TERP            ',  136.00 /
      DATA CHEMISTRY_SPC(  69 ), SPECIES_MOLWT(  69 ) / 'TRPRXN          ',  136.00 /
      DATA CHEMISTRY_SPC(  70 ), SPECIES_MOLWT(  70 ) / 'XO2T            ',    1.00 /
      DATA CHEMISTRY_SPC(  71 ), SPECIES_MOLWT(  71 ) / 'SO2             ',   64.00 /
      DATA CHEMISTRY_SPC(  72 ), SPECIES_MOLWT(  72 ) / 'SULF            ',   98.00 /
      DATA CHEMISTRY_SPC(  73 ), SPECIES_MOLWT(  73 ) / 'SULRXN          ',   98.00 /
      DATA CHEMISTRY_SPC(  74 ), SPECIES_MOLWT(  74 ) / 'ETOH            ',   46.00 /
      DATA CHEMISTRY_SPC(  75 ), SPECIES_MOLWT(  75 ) / 'ETHA            ',   30.00 /
      DATA CHEMISTRY_SPC(  76 ), SPECIES_MOLWT(  76 ) / 'CL2             ',   71.00 /
      DATA CHEMISTRY_SPC(  77 ), SPECIES_MOLWT(  77 ) / 'CL              ',   35.50 /
      DATA CHEMISTRY_SPC(  78 ), SPECIES_MOLWT(  78 ) / 'HOCL            ',   52.50 /
      DATA CHEMISTRY_SPC(  79 ), SPECIES_MOLWT(  79 ) / 'CLO             ',   51.50 /
      DATA CHEMISTRY_SPC(  80 ), SPECIES_MOLWT(  80 ) / 'FMCL            ',   64.50 /
      DATA CHEMISTRY_SPC(  81 ), SPECIES_MOLWT(  81 ) / 'HCL             ',   36.50 /
      DATA CHEMISTRY_SPC(  82 ), SPECIES_MOLWT(  82 ) / 'CLNO2           ',   81.50 /
      DATA CHEMISTRY_SPC(  83 ), SPECIES_MOLWT(  83 ) / 'TOLNRXN         ',  141.00 /
      DATA CHEMISTRY_SPC(  84 ), SPECIES_MOLWT(  84 ) / 'TOLHRXN         ',  141.00 /
      DATA CHEMISTRY_SPC(  85 ), SPECIES_MOLWT(  85 ) / 'XYLNRXN         ',  155.00 /
      DATA CHEMISTRY_SPC(  86 ), SPECIES_MOLWT(  86 ) / 'XYLHRXN         ',  155.00 /
      DATA CHEMISTRY_SPC(  87 ), SPECIES_MOLWT(  87 ) / 'BENZENE         ',   78.00 /
      DATA CHEMISTRY_SPC(  88 ), SPECIES_MOLWT(  88 ) / 'BENZRO2         ',  127.00 /
      DATA CHEMISTRY_SPC(  89 ), SPECIES_MOLWT(  89 ) / 'BNZNRXN         ',  127.00 /
      DATA CHEMISTRY_SPC(  90 ), SPECIES_MOLWT(  90 ) / 'BNZHRXN         ',  127.00 /
      DATA CHEMISTRY_SPC(  91 ), SPECIES_MOLWT(  91 ) / 'SESQ            ',  204.00 /
      DATA CHEMISTRY_SPC(  92 ), SPECIES_MOLWT(  92 ) / 'SESQRXN         ',  204.00 /
      DATA CHEMISTRY_SPC(  93 ), SPECIES_MOLWT(  93 ) / 'PAHNRXN         ',  187.20 /
      DATA CHEMISTRY_SPC(  94 ), SPECIES_MOLWT(  94 ) / 'PAHHRXN         ',  187.20 /
      DATA CHEMISTRY_SPC(  95 ), SPECIES_MOLWT(  95 ) / 'SOAALK          ',  112.00 /
      DATA CHEMISTRY_SPC(  96 ), SPECIES_MOLWT(  96 ) / 'ALKRXN          ',  112.00 /
      DATA CHEMISTRY_SPC(  97 ), SPECIES_MOLWT(  97 ) / 'MAPAN           ',  148.00 /
      DATA CHEMISTRY_SPC(  98 ), SPECIES_MOLWT(  98 ) / 'NALKO2          ',  106.00 /
      DATA CHEMISTRY_SPC(  99 ), SPECIES_MOLWT(  99 ) / 'NTRCN           ',  147.00 /
      DATA CHEMISTRY_SPC( 100 ), SPECIES_MOLWT( 100 ) / 'NTRCNOH         ',  149.00 /
      DATA CHEMISTRY_SPC( 101 ), SPECIES_MOLWT( 101 ) / 'NTRPX           ',  151.00 /
      DATA CHEMISTRY_SPC( 102 ), SPECIES_MOLWT( 102 ) / 'NOHO2           ',  106.00 /
      DATA CHEMISTRY_SPC( 103 ), SPECIES_MOLWT( 103 ) / 'NCNO2           ',  106.00 /
      DATA CHEMISTRY_SPC( 104 ), SPECIES_MOLWT( 104 ) / 'NCNOHO2         ',  106.00 /
      DATA CHEMISTRY_SPC( 105 ), SPECIES_MOLWT( 105 ) / 'NTRMO2          ',  106.00 /
      DATA CHEMISTRY_SPC( 106 ), SPECIES_MOLWT( 106 ) / 'NTRIO2          ',  106.00 /
      DATA CHEMISTRY_SPC( 107 ), SPECIES_MOLWT( 107 ) / 'H2NO3PIJ        ',   64.00 /
      DATA CHEMISTRY_SPC( 108 ), SPECIES_MOLWT( 108 ) / 'H2NO3PK         ',   64.00 /
      DATA CHEMISTRY_SPC( 109 ), SPECIES_MOLWT( 109 ) / 'ACLI            ',   35.50 /
      DATA CHEMISTRY_SPC( 110 ), SPECIES_MOLWT( 110 ) / 'ACLJ            ',   35.50 /
      DATA CHEMISTRY_SPC( 111 ), SPECIES_MOLWT( 111 ) / 'ACLK            ',   35.50 /
      DATA CHEMISTRY_SPC( 112 ), SPECIES_MOLWT( 112 ) / 'AISO3J          ',  168.20 /
      DATA CHEMISTRY_SPC( 113 ), SPECIES_MOLWT( 113 ) / 'AXYL1J          ',  174.00 /
      DATA CHEMISTRY_SPC( 114 ), SPECIES_MOLWT( 114 ) / 'AOLGAJ          ',  206.00 /
      DATA CHEMISTRY_SPC( 115 ), SPECIES_MOLWT( 115 ) / 'AXYL2J          ',  185.00 /
      DATA CHEMISTRY_SPC( 116 ), SPECIES_MOLWT( 116 ) / 'ATOL1J          ',  163.00 /
      DATA CHEMISTRY_SPC( 117 ), SPECIES_MOLWT( 117 ) / 'ATOL2J          ',  175.00 /
      DATA CHEMISTRY_SPC( 118 ), SPECIES_MOLWT( 118 ) / 'ABNZ1J          ',  161.00 /
      DATA CHEMISTRY_SPC( 119 ), SPECIES_MOLWT( 119 ) / 'ABNZ2J          ',  134.00 /
      DATA CHEMISTRY_SPC( 120 ), SPECIES_MOLWT( 120 ) / 'ATRP1J          ',  177.00 /
      DATA CHEMISTRY_SPC( 121 ), SPECIES_MOLWT( 121 ) / 'AOLGBJ          ',  248.00 /
      DATA CHEMISTRY_SPC( 122 ), SPECIES_MOLWT( 122 ) / 'ATRP2J          ',  198.00 /
      DATA CHEMISTRY_SPC( 123 ), SPECIES_MOLWT( 123 ) / 'AISO1J          ',  132.00 /
      DATA CHEMISTRY_SPC( 124 ), SPECIES_MOLWT( 124 ) / 'AISO2J          ',  133.00 /
      DATA CHEMISTRY_SPC( 125 ), SPECIES_MOLWT( 125 ) / 'ASQTJ           ',  273.00 /
      DATA CHEMISTRY_SPC( 126 ), SPECIES_MOLWT( 126 ) / 'APAH1J          ',  195.60 /
      DATA CHEMISTRY_SPC( 127 ), SPECIES_MOLWT( 127 ) / 'APAH2J          ',  178.70 /
      DATA CHEMISTRY_SPC( 128 ), SPECIES_MOLWT( 128 ) / 'AALK1J          ',  225.00 /
      DATA CHEMISTRY_SPC( 129 ), SPECIES_MOLWT( 129 ) / 'AALK2J          ',  205.10 /
      DATA CHEMISTRY_SPC( 130 ), SPECIES_MOLWT( 130 ) / 'PCVOC           ',  170.00 /
      DATA CHEMISTRY_SPC( 131 ), SPECIES_MOLWT( 131 ) / 'PCSOARXN        ',  170.00 /
      DATA CHEMISTRY_SPC( 132 ), SPECIES_MOLWT( 132 ) / 'VLVPO1          ',  218.00 /
      DATA CHEMISTRY_SPC( 133 ), SPECIES_MOLWT( 133 ) / 'VSVPO1          ',  230.00 /
      DATA CHEMISTRY_SPC( 134 ), SPECIES_MOLWT( 134 ) / 'VSVPO2          ',  241.00 /
      DATA CHEMISTRY_SPC( 135 ), SPECIES_MOLWT( 135 ) / 'VSVPO3          ',  253.00 /
      DATA CHEMISTRY_SPC( 136 ), SPECIES_MOLWT( 136 ) / 'VIVPO1          ',  266.00 /
      DATA CHEMISTRY_SPC( 137 ), SPECIES_MOLWT( 137 ) / 'VLVOO1          ',  136.00 /
      DATA CHEMISTRY_SPC( 138 ), SPECIES_MOLWT( 138 ) / 'VLVOO2          ',  136.00 /
      DATA CHEMISTRY_SPC( 139 ), SPECIES_MOLWT( 139 ) / 'VSVOO2          ',  135.00 /
      DATA CHEMISTRY_SPC( 140 ), SPECIES_MOLWT( 140 ) / 'VSVOO3          ',  134.00 /
      DATA CHEMISTRY_SPC( 141 ), SPECIES_MOLWT( 141 ) / 'VSVOO1          ',  135.00 /
      DATA CHEMISTRY_SPC( 142 ), SPECIES_MOLWT( 142 ) / 'FORM_PRIMARY    ',   30.00 /
      DATA CHEMISTRY_SPC( 143 ), SPECIES_MOLWT( 143 ) / 'ALD2_PRIMARY    ',   44.00 /
      DATA CHEMISTRY_SPC( 144 ), SPECIES_MOLWT( 144 ) / 'BUTADIENE13     ',   54.00 /
      DATA CHEMISTRY_SPC( 145 ), SPECIES_MOLWT( 145 ) / 'ACROLEIN        ',   56.10 /
      DATA CHEMISTRY_SPC( 146 ), SPECIES_MOLWT( 146 ) / 'ACRO_PRIMARY    ',   56.10 /
      DATA CHEMISTRY_SPC( 147 ), SPECIES_MOLWT( 147 ) / 'TOLU            ',   92.00 /
      DATA CHEMISTRY_SPC( 148 ), SPECIES_MOLWT( 148 ) / 'MXYL            ',  106.20 /
      DATA CHEMISTRY_SPC( 149 ), SPECIES_MOLWT( 149 ) / 'OXYL            ',  106.20 /
      DATA CHEMISTRY_SPC( 150 ), SPECIES_MOLWT( 150 ) / 'PXYL            ',  106.20 /
      DATA CHEMISTRY_SPC( 151 ), SPECIES_MOLWT( 151 ) / 'APIN            ',  136.30 /
      DATA CHEMISTRY_SPC( 152 ), SPECIES_MOLWT( 152 ) / 'BPIN            ',  136.30 /
      DATA CHEMISTRY_SPC( 153 ), SPECIES_MOLWT( 153 ) / 'HG              ',  200.60 /
      DATA CHEMISTRY_SPC( 154 ), SPECIES_MOLWT( 154 ) / 'HGIIAER         ',  200.60 /
      DATA CHEMISTRY_SPC( 155 ), SPECIES_MOLWT( 155 ) / 'HGIIGAS         ',  200.60 /
      DATA CHEMISTRY_SPC( 156 ), SPECIES_MOLWT( 156 ) / 'I               ',  126.90 /
      DATA CHEMISTRY_SPC( 157 ), SPECIES_MOLWT( 157 ) / 'IO              ',  142.90 /
      DATA CHEMISTRY_SPC( 158 ), SPECIES_MOLWT( 158 ) / 'OIO             ',  158.90 /
      DATA CHEMISTRY_SPC( 159 ), SPECIES_MOLWT( 159 ) / 'HI              ',  127.90 /
      DATA CHEMISTRY_SPC( 160 ), SPECIES_MOLWT( 160 ) / 'HOI             ',  143.90 /
      DATA CHEMISTRY_SPC( 161 ), SPECIES_MOLWT( 161 ) / 'I2O2            ',  285.80 /
      DATA CHEMISTRY_SPC( 162 ), SPECIES_MOLWT( 162 ) / 'I2O3            ',  301.80 /
      DATA CHEMISTRY_SPC( 163 ), SPECIES_MOLWT( 163 ) / 'I2O4            ',  317.80 /
      DATA CHEMISTRY_SPC( 164 ), SPECIES_MOLWT( 164 ) / 'I2              ',  253.80 /
      DATA CHEMISTRY_SPC( 165 ), SPECIES_MOLWT( 165 ) / 'IONO2           ',  188.90 /
      DATA CHEMISTRY_SPC( 166 ), SPECIES_MOLWT( 166 ) / 'DMS             ',   62.00 /
      DATA CHEMISTRY_SPC( 167 ), SPECIES_MOLWT( 167 ) / 'MSA             ',   96.00 /
      DATA CHEMISTRY_SPC( 168 ), SPECIES_MOLWT( 168 ) / 'INO2            ',  172.90 /
      DATA CHEMISTRY_SPC( 169 ), SPECIES_MOLWT( 169 ) / 'INO             ',  156.90 /
      DATA CHEMISTRY_SPC( 170 ), SPECIES_MOLWT( 170 ) / 'BRO             ',   95.90 /
      DATA CHEMISTRY_SPC( 171 ), SPECIES_MOLWT( 171 ) / 'BR              ',   79.90 /
      DATA CHEMISTRY_SPC( 172 ), SPECIES_MOLWT( 172 ) / 'ICL             ',  162.40 /
      DATA CHEMISTRY_SPC( 173 ), SPECIES_MOLWT( 173 ) / 'CH3I            ',  141.90 /
      DATA CHEMISTRY_SPC( 174 ), SPECIES_MOLWT( 174 ) / 'MI2             ',  267.80 /
      DATA CHEMISTRY_SPC( 175 ), SPECIES_MOLWT( 175 ) / 'MIB             ',  219.90 /
      DATA CHEMISTRY_SPC( 176 ), SPECIES_MOLWT( 176 ) / 'MIC             ',  176.40 /
      DATA CHEMISTRY_SPC( 177 ), SPECIES_MOLWT( 177 ) / 'HOBR            ',   96.90 /
      DATA CHEMISTRY_SPC( 178 ), SPECIES_MOLWT( 178 ) / 'HBR             ',   80.90 /
      DATA CHEMISTRY_SPC( 179 ), SPECIES_MOLWT( 179 ) / 'BR2             ',  159.80 /
      DATA CHEMISTRY_SPC( 180 ), SPECIES_MOLWT( 180 ) / 'BRONO2          ',  141.90 /
      DATA CHEMISTRY_SPC( 181 ), SPECIES_MOLWT( 181 ) / 'MB3             ',  252.70 /
      DATA CHEMISTRY_SPC( 182 ), SPECIES_MOLWT( 182 ) / 'CH3BR           ',   94.90 /
      DATA CHEMISTRY_SPC( 183 ), SPECIES_MOLWT( 183 ) / 'MB2             ',  173.80 /
      DATA CHEMISTRY_SPC( 184 ), SPECIES_MOLWT( 184 ) / 'MB2C            ',  208.30 /
      DATA CHEMISTRY_SPC( 185 ), SPECIES_MOLWT( 185 ) / 'MBC2            ',  243.80 /
      DATA CHEMISTRY_SPC( 186 ), SPECIES_MOLWT( 186 ) / 'MBC             ',  129.40 /
      DATA CHEMISTRY_SPC( 187 ), SPECIES_MOLWT( 187 ) / 'BRNO2           ',  125.90 /
      DATA CHEMISTRY_SPC( 188 ), SPECIES_MOLWT( 188 ) / 'BRCL            ',  115.40 /



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
      DATA CGRID_INDEX(  16 ), SPECIES_TYPE(  16 ), CONVERT_CONC(  16 ) /   16, 'GC', F /  ! NTROH
      DATA CGRID_INDEX(  17 ), SPECIES_TYPE(  17 ), CONVERT_CONC(  17 ) /   17, 'GC', F /  ! NTRALK
      DATA CGRID_INDEX(  18 ), SPECIES_TYPE(  18 ), CONVERT_CONC(  18 ) /   18, 'GC', F /  ! ROOH
      DATA CGRID_INDEX(  19 ), SPECIES_TYPE(  19 ), CONVERT_CONC(  19 ) /   19, 'GC', F /  ! ALD2
      DATA CGRID_INDEX(  20 ), SPECIES_TYPE(  20 ), CONVERT_CONC(  20 ) /   20, 'GC', F /  ! ALDX
      DATA CGRID_INDEX(  21 ), SPECIES_TYPE(  21 ), CONVERT_CONC(  21 ) /   21, 'GC', F /  ! ISOPX
      DATA CGRID_INDEX(  22 ), SPECIES_TYPE(  22 ), CONVERT_CONC(  22 ) /   22, 'GC', F /  ! IEPOX
      DATA CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), CONVERT_CONC(  23 ) /   23, 'GC', F /  ! ISOPO2
      DATA CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), CONVERT_CONC(  24 ) /   24, 'GC', F /  ! IOLE
      DATA CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), CONVERT_CONC(  25 ) /   25, 'GC', F /  ! IEPXO2
      DATA CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), CONVERT_CONC(  26 ) /   26, 'GC', F /  ! MGLY
      DATA CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), CONVERT_CONC(  27 ) /   27, 'GC', F /  ! FORM
      DATA CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), CONVERT_CONC(  28 ) /   28, 'GC', F /  ! FACD
      DATA CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), CONVERT_CONC(  29 ) /   29, 'GC', F /  ! CO
      DATA CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), CONVERT_CONC(  30 ) /   30, 'GC', F /  ! PAR
      DATA CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), CONVERT_CONC(  31 ) /   31, 'GC', F /  ! C2O3
      DATA CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), CONVERT_CONC(  32 ) /   32, 'GC', F /  ! MEO2
      DATA CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), CONVERT_CONC(  33 ) /   33, 'GC', F /  ! AACD
      DATA CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), CONVERT_CONC(  34 ) /   34, 'GC', F /  ! MEPX
      DATA CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), CONVERT_CONC(  35 ) /   35, 'GC', F /  ! MEOH
      DATA CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), CONVERT_CONC(  36 ) /   36, 'GC', F /  ! HCO3
      DATA CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), CONVERT_CONC(  37 ) /   37, 'GC', F /  ! PAN
      DATA CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), CONVERT_CONC(  38 ) /   38, 'GC', F /  ! PACD
      DATA CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), CONVERT_CONC(  39 ) /   39, 'GC', F /  ! CXO3
      DATA CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), CONVERT_CONC(  40 ) /   40, 'GC', F /  ! PANX
      DATA CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), CONVERT_CONC(  41 ) /   41, 'GC', F /  ! ROR
      DATA CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), CONVERT_CONC(  42 ) /   42, 'GC', F /  ! OLE
      DATA CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), CONVERT_CONC(  43 ) /   44, 'GC', F /  ! ETH
      DATA CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), CONVERT_CONC(  44 ) /   45, 'GC', F /  ! TOL
      DATA CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), CONVERT_CONC(  45 ) /   46, 'GC', F /  ! CRES
      DATA CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), CONVERT_CONC(  46 ) /   47, 'GC', F /  ! TO2
      DATA CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), CONVERT_CONC(  47 ) /   48, 'GC', F /  ! TOLRO2
      DATA CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), CONVERT_CONC(  48 ) /   49, 'GC', F /  ! OPEN
      DATA CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), CONVERT_CONC(  49 ) /   50, 'GC', F /  ! CRO
      DATA CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), CONVERT_CONC(  50 ) /   51, 'GC', F /  ! CAT1
      DATA CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), CONVERT_CONC(  51 ) /   52, 'GC', F /  ! CRON
      DATA CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), CONVERT_CONC(  52 ) /   53, 'GC', F /  ! CRNO
      DATA CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), CONVERT_CONC(  53 ) /   54, 'GC', F /  ! CRN2
      DATA CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), CONVERT_CONC(  54 ) /   55, 'GC', F /  ! CRPX
      DATA CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), CONVERT_CONC(  55 ) /   56, 'GC', F /  ! OPO3
      DATA CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), CONVERT_CONC(  56 ) /   57, 'GC', F /  ! CAO2
      DATA CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), CONVERT_CONC(  57 ) /   58, 'GC', F /  ! OPAN
      DATA CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), CONVERT_CONC(  58 ) /   59, 'GC', F /  ! XYLMN
      DATA CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), CONVERT_CONC(  59 ) /   60, 'GC', F /  ! XYLRO2
      DATA CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), CONVERT_CONC(  60 ) /   61, 'GC', F /  ! NAPH
      DATA CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), CONVERT_CONC(  61 ) /   62, 'GC', F /  ! PAHRO2
      DATA CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), CONVERT_CONC(  62 ) /   63, 'GC', F /  ! ISOP
      DATA CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), CONVERT_CONC(  63 ) /   64, 'GC', F /  ! ISPD
      DATA CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), CONVERT_CONC(  64 ) /   65, 'GC', F /  ! ISOPRXN
      DATA CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), CONVERT_CONC(  65 ) /   66, 'GC', F /  ! NTRM
      DATA CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), CONVERT_CONC(  66 ) /   67, 'GC', F /  ! MACO3
      DATA CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), CONVERT_CONC(  67 ) /   68, 'GC', F /  ! NTRI
      DATA CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), CONVERT_CONC(  68 ) /   69, 'GC', F /  ! TERP
      DATA CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), CONVERT_CONC(  69 ) /   70, 'GC', F /  ! TRPRXN
      DATA CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), CONVERT_CONC(  70 ) /   71, 'GC', F /  ! XO2T
      DATA CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), CONVERT_CONC(  71 ) /   72, 'GC', F /  ! SO2
      DATA CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), CONVERT_CONC(  72 ) /   73, 'GC', F /  ! SULF
      DATA CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), CONVERT_CONC(  73 ) /   74, 'GC', F /  ! SULRXN
      DATA CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), CONVERT_CONC(  74 ) /   75, 'GC', F /  ! ETOH
      DATA CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), CONVERT_CONC(  75 ) /   76, 'GC', F /  ! ETHA
      DATA CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), CONVERT_CONC(  76 ) /   77, 'GC', F /  ! CL2
      DATA CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), CONVERT_CONC(  77 ) /   78, 'GC', F /  ! CL
      DATA CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), CONVERT_CONC(  78 ) /   79, 'GC', F /  ! HOCL
      DATA CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), CONVERT_CONC(  79 ) /   80, 'GC', F /  ! CLO
      DATA CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), CONVERT_CONC(  80 ) /   81, 'GC', F /  ! FMCL
      DATA CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), CONVERT_CONC(  81 ) /   82, 'GC', F /  ! HCL
      DATA CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), CONVERT_CONC(  82 ) /   83, 'GC', F /  ! CLNO2
      DATA CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), CONVERT_CONC(  83 ) /   84, 'GC', F /  ! TOLNRXN
      DATA CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), CONVERT_CONC(  84 ) /   85, 'GC', F /  ! TOLHRXN
      DATA CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), CONVERT_CONC(  85 ) /   86, 'GC', F /  ! XYLNRXN
      DATA CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), CONVERT_CONC(  86 ) /   87, 'GC', F /  ! XYLHRXN
      DATA CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), CONVERT_CONC(  87 ) /   88, 'GC', F /  ! BENZENE
      DATA CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), CONVERT_CONC(  88 ) /   89, 'GC', F /  ! BENZRO2
      DATA CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), CONVERT_CONC(  89 ) /   90, 'GC', F /  ! BNZNRXN
      DATA CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), CONVERT_CONC(  90 ) /   91, 'GC', F /  ! BNZHRXN
      DATA CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), CONVERT_CONC(  91 ) /   92, 'GC', F /  ! SESQ
      DATA CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), CONVERT_CONC(  92 ) /   93, 'GC', F /  ! SESQRXN
      DATA CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), CONVERT_CONC(  93 ) /   94, 'GC', F /  ! PAHNRXN
      DATA CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), CONVERT_CONC(  94 ) /   95, 'GC', F /  ! PAHHRXN
      DATA CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), CONVERT_CONC(  95 ) /   96, 'GC', F /  ! SOAALK
      DATA CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), CONVERT_CONC(  96 ) /   97, 'GC', F /  ! ALKRXN
      DATA CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), CONVERT_CONC(  97 ) /   98, 'GC', F /  ! MAPAN
      DATA CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), CONVERT_CONC(  98 ) /   43, 'GC', F /  ! NALKO2
      DATA CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), CONVERT_CONC(  99 ) /   99, 'GC', F /  ! NTRCN
      DATA CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), CONVERT_CONC( 100 ) /  100, 'GC', F /  ! NTRCNOH
      DATA CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), CONVERT_CONC( 101 ) /  101, 'GC', F /  ! NTRPX
      DATA CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), CONVERT_CONC( 102 ) /  102, 'GC', F /  ! NOHO2
      DATA CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), CONVERT_CONC( 103 ) /  103, 'GC', F /  ! NCNO2
      DATA CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), CONVERT_CONC( 104 ) /  104, 'GC', F /  ! NCNOHO2
      DATA CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), CONVERT_CONC( 105 ) /  105, 'GC', F /  ! NTRMO2
      DATA CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), CONVERT_CONC( 106 ) /  106, 'GC', F /  ! NTRIO2
      DATA CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), CONVERT_CONC( 107 ) /  107, 'GC', F /  ! H2NO3PIJ
      DATA CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), CONVERT_CONC( 108 ) /  108, 'GC', F /  ! H2NO3PK
      DATA CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), CONVERT_CONC( 109 ) /  222, 'AE', T /  ! ACLI
      DATA CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), CONVERT_CONC( 110 ) /  221, 'AE', T /  ! ACLJ
      DATA CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), CONVERT_CONC( 111 ) /  224, 'AE', T /  ! ACLK
      DATA CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), CONVERT_CONC( 112 ) /  230, 'AE', T /  ! AISO3J
      DATA CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), CONVERT_CONC( 113 ) /  177, 'AE', T /  ! AXYL1J
      DATA CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), CONVERT_CONC( 114 ) /  231, 'AE', T /  ! AOLGAJ
      DATA CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), CONVERT_CONC( 115 ) /  178, 'AE', T /  ! AXYL2J
      DATA CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), CONVERT_CONC( 116 ) /  180, 'AE', T /  ! ATOL1J
      DATA CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), CONVERT_CONC( 117 ) /  181, 'AE', T /  ! ATOL2J
      DATA CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), CONVERT_CONC( 118 ) /  183, 'AE', T /  ! ABNZ1J
      DATA CGRID_INDEX( 119 ), SPECIES_TYPE( 119 ), CONVERT_CONC( 119 ) /  184, 'AE', T /  ! ABNZ2J
      DATA CGRID_INDEX( 120 ), SPECIES_TYPE( 120 ), CONVERT_CONC( 120 ) /  189, 'AE', T /  ! ATRP1J
      DATA CGRID_INDEX( 121 ), SPECIES_TYPE( 121 ), CONVERT_CONC( 121 ) /  232, 'AE', T /  ! AOLGBJ
      DATA CGRID_INDEX( 122 ), SPECIES_TYPE( 122 ), CONVERT_CONC( 122 ) /  190, 'AE', T /  ! ATRP2J
      DATA CGRID_INDEX( 123 ), SPECIES_TYPE( 123 ), CONVERT_CONC( 123 ) /  191, 'AE', T /  ! AISO1J
      DATA CGRID_INDEX( 124 ), SPECIES_TYPE( 124 ), CONVERT_CONC( 124 ) /  192, 'AE', T /  ! AISO2J
      DATA CGRID_INDEX( 125 ), SPECIES_TYPE( 125 ), CONVERT_CONC( 125 ) /  193, 'AE', T /  ! ASQTJ
      DATA CGRID_INDEX( 126 ), SPECIES_TYPE( 126 ), CONVERT_CONC( 126 ) /  186, 'AE', T /  ! APAH1J
      DATA CGRID_INDEX( 127 ), SPECIES_TYPE( 127 ), CONVERT_CONC( 127 ) /  187, 'AE', T /  ! APAH2J
      DATA CGRID_INDEX( 128 ), SPECIES_TYPE( 128 ), CONVERT_CONC( 128 ) /  175, 'AE', T /  ! AALK1J
      DATA CGRID_INDEX( 129 ), SPECIES_TYPE( 129 ), CONVERT_CONC( 129 ) /  176, 'AE', T /  ! AALK2J
      DATA CGRID_INDEX( 130 ), SPECIES_TYPE( 130 ), CONVERT_CONC( 130 ) /  166, 'GC', F /  ! PCVOC
      DATA CGRID_INDEX( 131 ), SPECIES_TYPE( 131 ), CONVERT_CONC( 131 ) /  167, 'GC', F /  ! PCSOARXN
      DATA CGRID_INDEX( 132 ), SPECIES_TYPE( 132 ), CONVERT_CONC( 132 ) /  156, 'GC', F /  ! VLVPO1
      DATA CGRID_INDEX( 133 ), SPECIES_TYPE( 133 ), CONVERT_CONC( 133 ) /  157, 'GC', F /  ! VSVPO1
      DATA CGRID_INDEX( 134 ), SPECIES_TYPE( 134 ), CONVERT_CONC( 134 ) /  158, 'GC', F /  ! VSVPO2
      DATA CGRID_INDEX( 135 ), SPECIES_TYPE( 135 ), CONVERT_CONC( 135 ) /  159, 'GC', F /  ! VSVPO3
      DATA CGRID_INDEX( 136 ), SPECIES_TYPE( 136 ), CONVERT_CONC( 136 ) /  160, 'GC', F /  ! VIVPO1
      DATA CGRID_INDEX( 137 ), SPECIES_TYPE( 137 ), CONVERT_CONC( 137 ) /  161, 'GC', F /  ! VLVOO1
      DATA CGRID_INDEX( 138 ), SPECIES_TYPE( 138 ), CONVERT_CONC( 138 ) /  162, 'GC', F /  ! VLVOO2
      DATA CGRID_INDEX( 139 ), SPECIES_TYPE( 139 ), CONVERT_CONC( 139 ) /  164, 'GC', F /  ! VSVOO2
      DATA CGRID_INDEX( 140 ), SPECIES_TYPE( 140 ), CONVERT_CONC( 140 ) /  165, 'GC', F /  ! VSVOO3
      DATA CGRID_INDEX( 141 ), SPECIES_TYPE( 141 ), CONVERT_CONC( 141 ) /  163, 'GC', F /  ! VSVOO1
      DATA CGRID_INDEX( 142 ), SPECIES_TYPE( 142 ), CONVERT_CONC( 142 ) /  109, 'GC', F /  ! FORM_PRIMARY
      DATA CGRID_INDEX( 143 ), SPECIES_TYPE( 143 ), CONVERT_CONC( 143 ) /  110, 'GC', F /  ! ALD2_PRIMARY
      DATA CGRID_INDEX( 144 ), SPECIES_TYPE( 144 ), CONVERT_CONC( 144 ) /  111, 'GC', F /  ! BUTADIENE13
      DATA CGRID_INDEX( 145 ), SPECIES_TYPE( 145 ), CONVERT_CONC( 145 ) /  112, 'GC', F /  ! ACROLEIN
      DATA CGRID_INDEX( 146 ), SPECIES_TYPE( 146 ), CONVERT_CONC( 146 ) /  113, 'GC', F /  ! ACRO_PRIMARY
      DATA CGRID_INDEX( 147 ), SPECIES_TYPE( 147 ), CONVERT_CONC( 147 ) /  114, 'GC', F /  ! TOLU
      DATA CGRID_INDEX( 148 ), SPECIES_TYPE( 148 ), CONVERT_CONC( 148 ) /  115, 'GC', F /  ! MXYL
      DATA CGRID_INDEX( 149 ), SPECIES_TYPE( 149 ), CONVERT_CONC( 149 ) /  116, 'GC', F /  ! OXYL
      DATA CGRID_INDEX( 150 ), SPECIES_TYPE( 150 ), CONVERT_CONC( 150 ) /  117, 'GC', F /  ! PXYL
      DATA CGRID_INDEX( 151 ), SPECIES_TYPE( 151 ), CONVERT_CONC( 151 ) /  118, 'GC', F /  ! APIN
      DATA CGRID_INDEX( 152 ), SPECIES_TYPE( 152 ), CONVERT_CONC( 152 ) /  119, 'GC', F /  ! BPIN
      DATA CGRID_INDEX( 153 ), SPECIES_TYPE( 153 ), CONVERT_CONC( 153 ) /  120, 'GC', F /  ! HG
      DATA CGRID_INDEX( 154 ), SPECIES_TYPE( 154 ), CONVERT_CONC( 154 ) /  121, 'GC', F /  ! HGIIAER
      DATA CGRID_INDEX( 155 ), SPECIES_TYPE( 155 ), CONVERT_CONC( 155 ) /  122, 'GC', F /  ! HGIIGAS
      DATA CGRID_INDEX( 156 ), SPECIES_TYPE( 156 ), CONVERT_CONC( 156 ) /  123, 'GC', F /  ! I
      DATA CGRID_INDEX( 157 ), SPECIES_TYPE( 157 ), CONVERT_CONC( 157 ) /  124, 'GC', F /  ! IO
      DATA CGRID_INDEX( 158 ), SPECIES_TYPE( 158 ), CONVERT_CONC( 158 ) /  125, 'GC', F /  ! OIO
      DATA CGRID_INDEX( 159 ), SPECIES_TYPE( 159 ), CONVERT_CONC( 159 ) /  126, 'GC', F /  ! HI
      DATA CGRID_INDEX( 160 ), SPECIES_TYPE( 160 ), CONVERT_CONC( 160 ) /  127, 'GC', F /  ! HOI
      DATA CGRID_INDEX( 161 ), SPECIES_TYPE( 161 ), CONVERT_CONC( 161 ) /  128, 'GC', F /  ! I2O2
      DATA CGRID_INDEX( 162 ), SPECIES_TYPE( 162 ), CONVERT_CONC( 162 ) /  129, 'GC', F /  ! I2O3
      DATA CGRID_INDEX( 163 ), SPECIES_TYPE( 163 ), CONVERT_CONC( 163 ) /  130, 'GC', F /  ! I2O4
      DATA CGRID_INDEX( 164 ), SPECIES_TYPE( 164 ), CONVERT_CONC( 164 ) /  131, 'GC', F /  ! I2
      DATA CGRID_INDEX( 165 ), SPECIES_TYPE( 165 ), CONVERT_CONC( 165 ) /  132, 'GC', F /  ! IONO2
      DATA CGRID_INDEX( 166 ), SPECIES_TYPE( 166 ), CONVERT_CONC( 166 ) /  133, 'GC', F /  ! DMS
      DATA CGRID_INDEX( 167 ), SPECIES_TYPE( 167 ), CONVERT_CONC( 167 ) /  134, 'GC', F /  ! MSA
      DATA CGRID_INDEX( 168 ), SPECIES_TYPE( 168 ), CONVERT_CONC( 168 ) /  135, 'GC', F /  ! INO2
      DATA CGRID_INDEX( 169 ), SPECIES_TYPE( 169 ), CONVERT_CONC( 169 ) /  136, 'GC', F /  ! INO
      DATA CGRID_INDEX( 170 ), SPECIES_TYPE( 170 ), CONVERT_CONC( 170 ) /  137, 'GC', F /  ! BRO
      DATA CGRID_INDEX( 171 ), SPECIES_TYPE( 171 ), CONVERT_CONC( 171 ) /  138, 'GC', F /  ! BR
      DATA CGRID_INDEX( 172 ), SPECIES_TYPE( 172 ), CONVERT_CONC( 172 ) /  139, 'GC', F /  ! ICL
      DATA CGRID_INDEX( 173 ), SPECIES_TYPE( 173 ), CONVERT_CONC( 173 ) /  140, 'GC', F /  ! CH3I
      DATA CGRID_INDEX( 174 ), SPECIES_TYPE( 174 ), CONVERT_CONC( 174 ) /  141, 'GC', F /  ! MI2
      DATA CGRID_INDEX( 175 ), SPECIES_TYPE( 175 ), CONVERT_CONC( 175 ) /  142, 'GC', F /  ! MIB
      DATA CGRID_INDEX( 176 ), SPECIES_TYPE( 176 ), CONVERT_CONC( 176 ) /  143, 'GC', F /  ! MIC
      DATA CGRID_INDEX( 177 ), SPECIES_TYPE( 177 ), CONVERT_CONC( 177 ) /  144, 'GC', F /  ! HOBR
      DATA CGRID_INDEX( 178 ), SPECIES_TYPE( 178 ), CONVERT_CONC( 178 ) /  145, 'GC', F /  ! HBR
      DATA CGRID_INDEX( 179 ), SPECIES_TYPE( 179 ), CONVERT_CONC( 179 ) /  146, 'GC', F /  ! BR2
      DATA CGRID_INDEX( 180 ), SPECIES_TYPE( 180 ), CONVERT_CONC( 180 ) /  147, 'GC', F /  ! BRONO2
      DATA CGRID_INDEX( 181 ), SPECIES_TYPE( 181 ), CONVERT_CONC( 181 ) /  148, 'GC', F /  ! MB3
      DATA CGRID_INDEX( 182 ), SPECIES_TYPE( 182 ), CONVERT_CONC( 182 ) /  149, 'GC', F /  ! CH3BR
      DATA CGRID_INDEX( 183 ), SPECIES_TYPE( 183 ), CONVERT_CONC( 183 ) /  150, 'GC', F /  ! MB2
      DATA CGRID_INDEX( 184 ), SPECIES_TYPE( 184 ), CONVERT_CONC( 184 ) /  151, 'GC', F /  ! MB2C
      DATA CGRID_INDEX( 185 ), SPECIES_TYPE( 185 ), CONVERT_CONC( 185 ) /  152, 'GC', F /  ! MBC2
      DATA CGRID_INDEX( 186 ), SPECIES_TYPE( 186 ), CONVERT_CONC( 186 ) /  153, 'GC', F /  ! MBC
      DATA CGRID_INDEX( 187 ), SPECIES_TYPE( 187 ), CONVERT_CONC( 187 ) /  154, 'GC', F /  ! BRNO2
      DATA CGRID_INDEX( 188 ), SPECIES_TYPE( 188 ), CONVERT_CONC( 188 ) /  155, 'GC', F /  ! BRCL

      INTEGER, PARAMETER :: N_ACT_SP = 188

      INTEGER, PARAMETER :: NRXNS = 443

      INTEGER            :: KUNITS

      DATA  KUNITS /   2 /

      INTEGER IRXXN

      REAL( 8 )          :: RTDAT( 3,NRXNS )

      INTEGER, PARAMETER :: NFALLOFF =  26
      REAL( 8 )          :: RFDAT( 5,NFALLOFF )

      INTEGER            :: KTYPE( NRXNS )

      DATA ( KTYPE( IRXXN ), IRXXN = 1, NRXNS ) /  & 
     &      0,    2,    3,    3,   10,   10,    3,    0,    0,    3, & ! O   
     &      1,    3,    3,    0,    0,    3,    3,   10,    1,    1, & ! 1   
     &     10,    3,    1,   10,    0,    3,    1,   10,    8,    3, & ! 2   
     &      9,   10,   10,    3,    9,    9,    0,    3,    1,    3, & ! 3   
     &      3,    3,   10,    3,    3,    3,    1,    1,    1,    1, & ! 4   
     &      3,    0,    0,    0,    3,    3,    3,    3,    1,    1, & ! 5   
     &      1,    3,    0,    3,    3,    3,    3,    3,    9,    3, & ! 6   
     &      3,    3,    3,    3,    0,    3,    1,    0,    0,    3, & ! 7   
     &      1,    3,    3,    1,    3,    1,    3,    3,    3,    0, & ! 8   
     &      3,   10,   10,    0,    3,    3,    3,    3,    3,    0, & ! 9   
     &      3,    3,    3,    1,    0,    3,   10,   10,    0,    1, & ! O   
     &      3,    3,    3,    3,    3,    1,    3,    1,    1,    3, & ! 1   
     &      1,    3,    3,    3,   10,    3,    3,    1,    3,    3, & ! 2   
     &      3,    3,    3,    3,    3,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    3,    3,    0,    3,    0,    1,    3,    1, & ! 4   
     &      1,    1,    3,    3,    1,    1,    1,    3,    3,    1, & ! 5   
     &      0,    1,    3,    3,    6,    3,    3,    3,    1,    1, & ! 6   
     &      1,    0,    1,    3,    3,    3,    1,    1,    3,    3, & ! 7   
     &     10,    3,    3,    1,    0,    0,    3,    1,    3,    3, & ! 8   
     &      1,    0,    3,    1,    3,    1,    1,    1,    1,    3, & ! 9   
     &      1,    1,    1,    3,    4,    1,    1,    1,   10,    0, & ! O   
     &      3,    3,    3,    3,    3,    3,    3,    1,    1,    1, & ! 1   
     &      3,    3,    3,    3,    4,    3,    0,    1,    3,    3, & ! 2   
     &      3,    3,    3,    1,    3,    3,    1,    3,    3,    1, & ! 3   
     &      3,    3,    1,    3,    3,    1,    1,    3,    3,    1, & ! 4   
     &      3,    3,    0,    0,    0,    0,    0,    0,    0,   -1, & ! 5   
     &     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1, & ! 6   
     &     -1,   -1,   -1,   -1,   -1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 9   
     &      1,    1,    1,    3,    0,    0,    3,    3,    3,    3, & ! O   
     &      0,    1,    3,    3,    1,    1,    1,    1,    3,    0, & ! 1   
     &      1,    1,    1,    3,    0,    1,    3,    1,    3,    1, & ! 2   
     &      1,    1,    1,    1,    1,    3,    3,    3,    1,    1, & ! 3   
     &      1,    3,    1,    1,    3,    1,    1,    1,    3,    3, & ! 4   
     &      1,    3,    3,    3,    3,    3,    3,    3,    1,    1, & ! 5   
     &      1,    3,    3,    3,    1,    1,    1,    3,    3,    1, & ! 6   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    1, & ! 7   
     &      3,    3,    3,    1,    1,    1,    3,   10,   10,   10, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    3,    3,    3,    3,    3,    3,    3,    3, & ! O   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 1   
     &      3,    3,    3,    3,    3,   10,   10,    3,    3,    3, & ! 2   
     &      3,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,   -1,   -1/     !  4   

      INTEGER            :: IRXBITS( NRXNS )

      DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,  276,    0,    0,    1,    1,    0,    2,    2,  260, & ! O   
     &      8,    0,    0,    2,    2,    0,    0,    1,    8,    8, & ! 1   
     &      1,   16,    8,    1,    2,    0,    0,    1,    0,    0, & ! 2   
     &      0,    1,    1,    0,    0,    8,    2,    0,  128,  128, & ! 3   
     &      0,    0,    1,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    2,    2,    2,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    2,    0,    0,    0,    0,    0,    0,   64, & ! 6   
     &      0,    0,    0,    0,    2,    0,    0,    2,    2,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    2, & ! 8   
     &      0,    1,    1,    2,    0,    0,    0,    0,    0,    2, & ! 9   
     &      0,    0,    0,    0,    2,    0,    1,    1,    2,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    1,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    2,    0,    2,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      2,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    2,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      1,    0,    0,    0,    2,    2,    0,    0,    0,    0, & ! 8   
     &      0,    2,   64,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    1,    2, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    2,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    2,    2,    2,    2,    2,    2,    2,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    2,    2,    0,    0,    0,    0, & ! O   
     &      2,    0,    0,    0,    0,    0,    0,    0,    0,    2, & ! 1   
     &      0,    0,    0,    0,    2,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,  260,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    1,    1,    1, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    1,    1,    0,    0,    0, & ! 2   
     &      0,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    1,    1/     !  4   

      INTEGER            :: IORDER( NRXNS )

      DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    2,    2,    2,    2,    2,    1,    1,    2, & ! O   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    3, & ! 1   
     &      1,    3,    3,    2,    1,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    1,    2,    2,    3,    1,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    1,    1,    1,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    1,    2,    2,    1,    1,    2, & ! 7   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    1, & ! 8   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    1, & ! 9   
     &      2,    2,    2,    2,    1,    2,    2,    1,    1,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    1,    1,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    1,    2,    1,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    1,    2,    2,    2, & ! 5   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    1,    1,    2,    2,    2,    2, & ! 8   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    1,    1,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      2,    2,    2,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    1,    1,    2,    2,    2,    2, & ! O   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 1   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    3,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    1,    1,    1,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    1,    1,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 9   
     &      1,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1/     !  4   

      INTEGER, PARAMETER :: KTN1 = 135
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     11,   19,   20,   23,   27,   39,   47,   48,   49,   50, & ! O   
     &     59,   60,   61,   77,   81,   84,   86,  104,  110,  116, & ! 1   
     &    118,  119,  121,  128,  136,  137,  138,  139,  140,  141, & ! 2   
     &    142,  148,  150,  151,  152,  155,  156,  157,  160,  162, & ! 3   
     &    169,  170,  171,  173,  177,  178,  184,  188,  191,  194, & ! 4   
     &    196,  197,  198,  199,  201,  202,  203,  206,  207,  208, & ! 5   
     &    218,  219,  220,  228,  234,  237,  240,  243,  246,  247, & ! 6   
     &    250,  276,  277,  278,  279,  280,  281,  282,  283,  284, & ! 7   
     &    285,  286,  287,  288,  289,  290,  291,  292,  293,  294, & ! 8   
     &    295,  296,  297,  298,  299,  300,  301,  302,  303,  312, & ! 9   
     &    315,  316,  317,  318,  321,  322,  323,  326,  328,  330, & ! O   
     &    331,  332,  333,  334,  335,  339,  340,  341,  343,  344, & ! 1   
     &    346,  347,  348,  351,  359,  360,  361,  365,  366,  367, & ! 2   
     &    370,  380,  384,  385,  386/     !  3   

      INTEGER, PARAMETER :: KTN2 =   1
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &      2/

      INTEGER, PARAMETER :: KTN3 = 198
      INTEGER            :: KRX3( KTN3 )

      DATA ( KRX3( IRXXN ), IRXXN = 1, KTN3 ) / & 
     &      3,    4,    7,   10,   12,   13,   16,   17,   22,   26, & ! O   
     &     30,   34,   38,   40,   41,   42,   44,   45,   46,   51, & ! 1   
     &     55,   56,   57,   58,   62,   64,   65,   66,   67,   68, & ! 2   
     &     70,   71,   72,   73,   74,   76,   80,   82,   83,   85, & ! 3   
     &     87,   88,   89,   91,   95,   96,   97,   98,   99,  101, & ! 4   
     &    102,  103,  106,  111,  112,  113,  114,  115,  117,  120, & ! 5   
     &    122,  123,  124,  126,  127,  129,  130,  131,  132,  133, & ! 6   
     &    134,  135,  143,  144,  146,  149,  153,  154,  158,  159, & ! 7   
     &    163,  164,  166,  167,  168,  174,  175,  176,  179,  180, & ! 8   
     &    182,  183,  187,  189,  190,  193,  195,  200,  204,  211, & ! 9   
     &    212,  213,  214,  215,  216,  217,  221,  222,  223,  224, & ! O   
     &    226,  229,  230,  231,  232,  233,  235,  236,  238,  239, & ! 1   
     &    241,  242,  244,  245,  248,  249,  251,  252,  304,  307, & ! 2   
     &    308,  309,  310,  313,  314,  319,  324,  327,  329,  336, & ! 3   
     &    337,  338,  342,  345,  349,  350,  352,  353,  354,  355, & ! 4   
     &    356,  357,  358,  362,  363,  364,  368,  369,  371,  372, & ! 5   
     &    373,  374,  375,  376,  377,  378,  379,  381,  382,  383, & ! 6   
     &    387,  403,  404,  405,  406,  407,  408,  409,  410,  411, & ! 7   
     &    412,  413,  414,  415,  416,  417,  418,  419,  420,  421, & ! 8   
     &    422,  423,  424,  425,  428,  429,  430,  431/     !  9   

      INTEGER, PARAMETER :: KTN4 =   2
      INTEGER            :: KRX4( KTN4 )

      DATA ( KRX4( IRXXN ), IRXXN = 1, KTN4 ) / & 
     &    205,  225/

      INTEGER, PARAMETER :: KTN5 =   0
      INTEGER            :: KRX5( 1 )

      DATA   KRX5( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN6 =   1
      INTEGER            :: KRX6( KTN6 )

      DATA ( KRX6( IRXXN ), IRXXN = 1, KTN6 ) / & 
     &    165/

      INTEGER, PARAMETER :: KTN7 =   0
      INTEGER            :: KRX7( 1 )

      DATA   KRX7( 1 ) / 0 /

      INTEGER, PARAMETER :: NWM =   3
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &      2,   10,  349/
      REAL,    PARAMETER :: ATM_AIR = 1.00000E+06

      INTEGER, PARAMETER :: NWW =   6
      INTEGER            :: NRXWW( NWW )

      DATA ( NRXWW( IRXXN ), IRXXN = 1, NWW ) / & 
     &     11,   19,   20,   20,   23,   36/

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
     &     70,  193/
      REAL,    PARAMETER :: ATM_CH4 = 1.85000E+00

      INTEGER, PARAMETER :: NWH2 =   2
      INTEGER            :: NRXWH2( NWH2 )

      DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) / & 
     &     39,   40/
      REAL,    PARAMETER :: ATM_H2 = 5.60000E-01

      INTEGER, PARAMETER :: MXPRD =  10
      INTEGER            :: IRR( NRXNS,MXPRD+3 )

      DATA ( IRR( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    4,    3,    3,    3,    1,    4,    4,    6, & ! O   
     &      6,    4,    4,    5,    5,    5,    5,    5,    9,    9, & ! 1   
     &      9,    2,    2,    2,   11,    7,   11,    1,    7,    8, & ! 2   
     &      2,    8,   12,    7,    8,    8,   13,    7,    6,    7, & ! 3   
     &      7,    7,    7,    7,    8,   13,    5,    5,    5,    5, & ! 4   
     &      5,   12,   10,    9,   14,   15,   14,   15,   14,   15, & ! 5   
     &     14,   18,   18,   21,   22,   25,   25,   25,    7,    7, & ! 6   
     &     32,   32,   32,   34,   34,   35,   27,   27,   27,   27, & ! 7   
     &     27,   27,   36,   36,   36,   28,   19,   19,   19,   19, & ! 8   
     &     31,   31,   37,   37,   31,   31,   31,   31,   38,   38, & ! 9   
     &     33,   20,   20,   20,   20,   39,   39,   40,   40,   40, & ! O   
     &     39,   39,   39,   39,   39,   30,   41,   41,   41,    3, & ! 1   
     &      7,    4,    5,    3,    7,    4,    5,   24,   24,   24, & ! 2   
     &     24,   44,   46,   46,    7,   45,   49,   49,   51,   51, & ! 3   
     &     52,   52,   53,   53,   54,   54,   48,   48,   48,   48, & ! 4   
     &     50,   50,   56,   56,   55,   55,   57,    7,    7,    7, & ! 5   
     &     26,    3,   62,   23,   23,   23,    4,    5,    7,    4, & ! 6   
     &      5,   63,   68,   68,   70,   70,   70,   14,   68,   68, & ! 7   
     &     71,    7,    7,    1,   76,   78,   77,   79,   79,   79, & ! 8   
     &      7,   80,   77,   77,   77,   77,   77,   77,   77,   77, & ! 9   
     &     77,   77,   77,   77,   81,   77,   77,   77,   77,   82, & ! O   
     &     47,   47,   59,   59,   87,   88,   88,   91,   91,   91, & ! 1   
     &     61,   61,   95,   66,   66,   97,   97,   97,   66,   66, & ! 2   
     &     66,   66,   66,   17,   98,   98,   16,  102,  102,   99, & ! 3   
     &    103,  103,  100,  104,  104,  101,   65,  105,  105,   67, & ! 4   
     &    106,  106,   65,   67,   17,   16,   99,  100,  101,   17, & ! 5   
     &     16,   99,  100,  101,   65,   67,    9,    9,  107,  108, & ! 6   
     &    107,  107,  108,    1,   22,  113,  115,  116,  117,  118, & ! 7   
     &    119,  120,  122,  123,  124,  125,  126,  127,  128,  129, & ! 8   
     &    130,  132,  133,  134,  135,  136,  137,  138,  141,  139, & ! 9   
     &    140,  142,  142,  142,  142,  142,  142,  143,  143,  143, & ! O   
     &    143,  143,  144,  144,  144,  144,  146,  146,  146,  146, & ! 1   
     &    146,  145,  145,  145,  145,  145,  147,  147,  148,  148, & ! 2   
     &    149,  149,  150,  150,  151,  151,  151,  151,  151,  152, & ! 3   
     &    152,  152,  152,  152,  153,  153,  153,  153,  153,  156, & ! 4   
     &    157,  156,  157,  157,  157,  157,  157,  158,  164,  157, & ! 5   
     &    157,  161,  161,  163,  164,  164,  156,    7,  156,  160, & ! 6   
     &    157,  168,  165,  169,  168,  158,  159,  157,  157,  156, & ! 7   
     &    157,  157,  157,  157,  157,  157,  173,  156,  157,  156, & ! 8   
     &    173,  174,  175,  176,  164,  157,  158,  169,  168,  165, & ! 9   
     &    160,  172,  171,  170,  171,  178,  170,  170,  170,  171, & ! O   
     &    179,  170,  171,  171,  171,  171,  170,  170,  170,  181, & ! 1   
     &    182,  183,  184,  185,  186,  170,  171,  170,  170,  170, & ! 2   
     &    166,  188,  170,  177,  180,  180,  187,  179,  181,  184, & ! 3   
     &    185,  180,  180/     !  4   

      DATA ( IRR( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    2,    1,    1,    2,    4,    0,    0,    0, & ! O   
     &      0,    7,    8,    0,    0,    2,    1,    1,    0,    0, & ! 1   
     &      0,    2,    1,    7,    0,   11,   11,    7,   10,    2, & ! 2   
     &      8,    1,    0,   12,    8,    8,    0,   13,    0,    0, & ! 3   
     &      3,    7,    7,    8,    3,    3,    3,    7,    8,    4, & ! 4   
     &      5,    0,    0,    0,    2,    2,    8,    8,   14,   15, & ! 5   
     &     15,    7,    0,    7,    7,    8,    2,   31,   29,    0, & ! 6   
     &      2,    8,   32,    7,    0,    7,    7,    0,    0,    3, & ! 7   
     &      5,    8,    0,    2,    8,    7,    3,    7,    5,    0, & ! 8   
     &      2,    1,    0,    0,    8,   32,   14,   31,    7,    0, & ! 9   
     &      7,    3,    7,    5,    0,    2,    1,    0,    0,    7, & ! O   
     &      8,   32,   14,   39,   31,    7,    0,    0,    1,   42, & ! 1   
     &     42,   42,   42,   43,   43,   43,   43,    3,    7,    4, & ! 2   
     &      5,    7,    2,    8,   45,    5,    1,    8,    7,    5, & ! 3   
     &      1,    4,    2,    8,    0,    7,    0,    7,    4,    5, & ! 4   
     &      7,    5,    2,    8,    2,    1,    0,   58,   60,   26, & ! 5   
     &      0,   62,    7,    2,   31,    8,   62,   62,   63,   63, & ! 6   
     &     63,    0,    3,    7,    2,    8,   70,   70,    4,    5, & ! 7   
     &      7,   74,   75,   62,    0,    0,    4,   79,    2,    8, & ! 8   
     &     80,    0,    0,   30,   75,   43,   42,   24,   62,   27, & ! 9   
     &     19,   20,   35,   74,    7,   44,   58,   60,    1,    0, & ! O   
     &      2,    8,    2,    8,    7,    2,    8,    4,    7,    5, & ! 1   
     &      2,    8,    7,    2,    1,    0,    0,    7,    8,   32, & ! 2   
     &     14,   39,   31,    7,    2,    8,    7,    2,    8,    7, & ! 3   
     &      2,    8,    7,    2,    8,    7,    7,    2,    8,    7, & ! 4   
     &      2,    8,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &    109,  110,  111,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      7,    7,    7,    7,    7,    7,    7,    7,    7,    7, & ! 9   
     &      7,    7,    5,    3,    0,    0,   77,    7,    5,    3, & ! O   
     &      0,   77,    7,    4,    5,   77,    7,    4,    5,    0, & ! 1   
     &     77,    7,    4,    5,    0,   77,    7,   77,    7,   77, & ! 2   
     &      7,   77,    7,   77,    3,    7,    4,    5,   77,    3, & ! 3   
     &      7,    4,    5,   77,    4,   76,   13,    7,   77,    4, & ! 4   
     &      4,    8,    2,    8,  157,  157,  158,  158,    3,    3, & ! 5   
     &      7,    0,    0,    0,    7,    5,    5,  159,  165,    7, & ! 6   
     &    166,    0,    0,  169,  168,    2,    5,  170,  170,  170, & ! 7   
     &     79,   79,   79,  171,    5,   32,    7,    1,    1,    2, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    4,    8,    8,    7,  170,  170,    2,  180, & ! O   
     &      7,    7,    5,   27,   19,   20,   32,   32,   31,    7, & ! 1   
     &      7,    7,    7,    7,    7,    1,    1,   79,   79,   79, & ! 2   
     &    170,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0/     !  4   

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
     &      0,    0,    0/     !  4   

      DATA ( IRR( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &      2,    4,    1,    2,    5,    1,    5,    3,    6,    3, & ! O   
     &      7,    8,    7,    1,    2,    1,    2,    9,   10,   10, & ! 1   
     &      5,    1,   11,   11,    2,    1,    2,   10,    5,    7, & ! 2   
     &     10,   12,    8,    1,   13,   13,    7,    8,    7,    8, & ! 3   
     &      8,    3,   13,    0,    7,    7,    1,    8,   10,    1, & ! 4   
     &      1,    8,    7,    1,    1,   16,   18,   18,    0,    0, & ! 5   
     &      0,   14,    7,   22,   25,   19,   19,   19,    8,   32, & ! 6   
     &     27,   34,   27,   32,   27,   27,    8,    8,   29,    7, & ! 7   
     &     10,   36,   27,   28,   34,    8,   31,   31,   31,   32, & ! 8   
     &     32,   37,   31,    1,   38,   32,   32,   32,   31,   32, & ! 9   
     &     32,   39,   39,   39,   32,   19,   40,   39,   39,   19, & ! O   
     &     38,   19,   19,   19,   32,   14,   14,    8,   17,   19, & ! 1   
     &     27,   19,    1,   27,   14,   27,    1,   19,   19,   19, & ! 2   
     &     19,    8,    1,    0,   49,   49,   51,   45,   52,   52, & ! 3   
     &     16,   53,   52,   54,   52,   53,   55,   55,   20,   55, & ! 4   
     &     56,   49,    1,    0,    1,   57,   55,    8,    8,   14, & ! 5   
     &     31,   63,   23,   65,   27,   21,   63,   63,   30,   31, & ! 6   
     &     20,   29,   20,    8,   65,   18,    0,    0,    7,    1, & ! 7   
     &     72,    8,   19,   63,   77,    7,   79,   76,   77,   78, & ! 8   
     &     77,   77,   81,   81,   81,   80,   80,   81,   81,   81, & ! 9   
     &     81,   81,   81,   81,   77,   81,   81,   81,   82,   77, & ! O   
     &      2,    8,    2,    8,    7,    2,    8,    4,    7,    5, & ! 1   
     &      2,    8,    7,   27,   97,   66,   66,   19,   31,   27, & ! 2   
     &     19,   19,   32,   98,    1,  101,  102,    1,  101,  103, & ! 3   
     &      1,  101,  104,    1,  101,   99,  105,    1,   67,  106, & ! 4   
     &      1,   67,    1,    1,    1,    1,    1,    1,    1,   10, & ! 5   
     &     10,   10,   10,   10,   10,   10,   10,   10,   10,   10, & ! 6   
     &     82,   82,   82,   11,  112,  114,  114,  114,  114,  114, & ! 7   
     &    114,  121,  121,  121,  121,  121,  114,  114,  114,  114, & ! 8   
     &      7,    7,    7,    7,    7,    7,    7,    7,    7,    7, & ! 9   
     &      7,    7,    5,    3,    0,    0,   77,    7,    5,    3, & ! O   
     &      0,   77,    7,    4,    5,   77,    7,    4,    5,    0, & ! 1   
     &     77,    7,    4,    5,    0,   77,    7,   77,    7,   77, & ! 2   
     &      7,   77,    7,   77,    3,    7,    4,    5,   77,    3, & ! 3   
     &      7,    4,    5,   77,  154,  155,  155,  154,  153,  157, & ! 4   
     &    158,  159,  156,  160,  158,  161,  162,  163,  157,  156, & ! 5   
     &      8,  158,  157,  158,  160,  156,  157,  156,  164,  157, & ! 6   
     &     71,  156,  157,  164,  164,  157,  156,  171,  171,  157, & ! 7   
     &    156,  156,  172,  156,  158,   27,  156,  168,  165,  169, & ! 8   
     &    156,  156,  156,  156,  156,  156,  156,  156,  156,  156, & ! 9   
     &    156,  156,  170,  177,  178,  171,  171,  179,  171,  179, & ! O   
     &    177,  171,  170,  178,  178,  178,  177,  171,  171,  171, & ! 1   
     &    171,  171,  171,  171,  171,  180,  187,  171,  171,  188, & ! 2   
     &     71,  171,  171,    7,  170,  171,  171,  171,  171,  171, & ! 3   
     &    171,  177,  177/     !  4   

      DATA ( IRR( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &      3,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    3,    0,    0,    1,    0,    0,    0, & ! 1   
     &      1,    0,    0,    0,    7,    0,    1,    0,    0,    1, & ! 2   
     &      0,    0,    1,    0,    0,    0,    0,    0,    8,    0, & ! 3   
     &      0,    0,    0,    0,    0,    8,    0,    1,    0,    0, & ! 4   
     &      0,    1,    1,    5,    0,   17,    0,    0,    0,    0, & ! 5   
     &      0,   19,    8,    7,    0,   26,   26,   26,    0,    0, & ! 6   
     &      8,    0,    8,   14,    8,    8,   29,   29,    0,    8, & ! 7   
     &      8,    0,    8,    1,    0,    0,    7,    0,   10,   29, & ! 8   
     &      1,    0,    1,   31,   33,    8,   33,    0,    0,    7, & ! 9   
     &      0,    7,    0,   10,   29,    1,    0,    1,    1,    1, & ! O   
     &     33,   14,   33,   14,   14,   15,   19,    0,    0,   20, & ! 1   
     &     19,   27,   27,    8,   27,   29,   14,   20,   20,   20, & ! 2   
     &     20,   14,    8,    0,   14,   10,    0,    0,    0,   10, & ! 3   
     &      0,    0,    1,    0,    7,    0,    8,   56,   55,   10, & ! 4   
     &      0,   10,    8,    0,   14,    0,    1,   14,   14,   31, & ! 5   
     &      8,   27,   64,    1,   63,    0,   27,   65,   27,   27, & ! 6   
     &     27,   19,   30,   14,    0,    0,    0,    0,    8,    8, & ! 7   
     &      8,   19,   14,   16,    0,   77,    0,   77,    1,    0, & ! 8   
     &     29,   29,   32,   14,   19,   14,   19,   80,   14,    8, & ! 9   
     &     31,   39,    8,    8,    0,   14,   14,   14,    0,    1, & ! O   
     &     83,   84,   85,   86,   88,   89,   90,   92,   92,   92, & ! 1   
     &     93,   94,   96,    1,    0,    1,    1,   29,   27,    8, & ! 2   
     &     31,   14,   27,    0,   99,    0,    0,   99,    0,    0, & ! 3   
     &     99,    0,    0,  100,    0,    7,    0,   67,    0,    0, & ! 4   
     &     67,    0,    8,    8,    8,    8,    8,    8,    8,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,  107,  108,    0,    0, & ! 6   
     &      0,    0,    0,   10,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &    131,  132,  132,  132,  132,  132,  137,  137,  137,  137, & ! 9   
     &    137,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,  145,  145,  145,  145,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,  155,   76,   13,  155,  155,    0, & ! 4   
     &      0,    0,    1,    0,  156,    0,    0,    0,  156,    0, & ! 5   
     &    156,  156,    0,    0,  156,  165,    1,    0,    5,    0, & ! 6   
     &    167,    1,    1,    2,    1,    1,   10,  156,  158,  171, & ! 7   
     &     77,   77,    0,  170,    1,  156,   27,    0,    0,    0, & ! 8   
     &     32,   27,  171,   77,    0,    3,    0,    2,    1,    5, & ! 9   
     &      7,   77,    0,    0,    0,    0,    0,    0,    1,    5, & ! O   
     &    171,    8,    1,    8,   31,   39,   27,   27,   32,   29, & ! 1   
     &     27,    8,   77,   77,   77,    0,    0,   77,   77,    0, & ! 2   
     &    167,   77,    3,  171,    1,    5,    1,    0,    8,   77, & ! 3   
     &     77,   10,   10/     !  4   

      DATA ( IRR( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    7,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,   20,   19,   23,    0,    7,    7,    7,    0,    0, & ! 6   
     &      1,    0,   35,    8,    7,    0,    0,    0,    0,   29, & ! 7   
     &     29,    0,    0,    8,    0,    0,    0,    0,    0,    8, & ! 8   
     &      0,    0,    0,    5,    7,   27,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    8,    8,    0,    0,    5,    0, & ! O   
     &      7,    8,    0,    8,    8,    8,    8,    0,    0,    8, & ! 1   
     &     20,   20,   14,   29,   20,    8,   27,    8,    8,   27, & ! 2   
     &      8,   45,   48,    0,    8,   14,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,   29,    0,   27,    0, & ! 4   
     &      0,    0,   27,    0,    8,    0,    0,   45,   45,    0, & ! 5   
     &     29,   14,    0,   27,    8,    0,   14,   14,   14,   26, & ! 6   
     &     30,   27,   69,   70,    0,    0,    0,    0,   14,   14, & ! 7   
     &     73,   20,   15,   14,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    8,    0,   15,   14,    8,   20,   19,    8,   29, & ! 9   
     &      0,    0,   27,   19,    0,    8,    8,    8,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,   32,    0,    0,    5,    1,   32,   31, & ! 2   
     &     27,    8,   31,    0,  100,    0,    0,  100,    0,    0, & ! 3   
     &    100,    0,    0,   99,    0,    0,    0,    8,    0,    0, & ! 4   
     &      0,    0,   19,   27,   27,   27,   27,   27,   18,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,  133,  133,  133,  133,  133,  138,  138,  138,  138, & ! 9   
     &    138,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    4,    0,    0,    7,   77,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &     32,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    8,    0,    0,    0,    0, & ! 8   
     &      0,    0,   27,   27,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,   29,    0,    0,    0,    8,    0,    0, & ! 1   
     &      0,   29,   29,   29,   29,    0,    0,    0,    0,    0, & ! 2   
     &     32,    0,    0,    0,    0,    0,    0,    0,   29,    8, & ! 3   
     &      8,    0,    0/     !  4   

      DATA ( IRR( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    5,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,   20,   24,    0,    8,    8,    8,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,   32,   32,   33,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,   14,    0,    0,   19,    0, & ! O   
     &     14,   33,    0,    0,   19,   19,   30,    0,    0,   14, & ! 1   
     &     14,   14,   15,   14,    8,    7,    0,   14,   14,   29, & ! 2   
     &      1,   46,   16,    0,   48,    8,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,   14,    0, & ! 4   
     &      0,    0,   29,    0,   20,    0,    0,   26,   26,    0, & ! 5   
     &      0,    8,    0,   63,   32,    0,    8,    8,    8,    8, & ! 6   
     &      8,   30,    0,   27,    0,    0,    0,    0,   70,   70, & ! 7   
     &      0,   27,    8,    8,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    8,   15,   27,   14,   20,   80,    0, & ! 9   
     &      0,    0,    0,    0,    0,   15,   15,   15,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   27,    0,   29,   32, & ! 2   
     &     32,   27,   29,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,   27,    0,    0, & ! 4   
     &      0,    0,   24,   19,   19,   19,   19,   19,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,  134,  134,  134,  134,  134,  141,  141,  141,  141, & ! 9   
     &    141,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &    156,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    8,    0,    0,    0,    0,    0, & ! 2   
     &    171,    0,    0,    0,    0,    0,    0,    0,    0,   29, & ! 3   
     &     29,    0,    0/     !  4   

      DATA ( IRR( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,   20,    0,   27,   27,   27,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    4,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    8,    0, & ! O   
     &      4,   27,    0,    0,    0,   30,   15,    0,    0,   29, & ! 1   
     &      8,    7,   20,    7,    0,   28,    0,   29,    0,    3, & ! 2   
     &      0,    7,   26,    0,   50,   20,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,   29,    0, & ! 4   
     &      0,    0,   16,    0,    0,    0,    0,   30,   30,    0, & ! 5   
     &      0,   39,    0,    8,   33,    0,    7,    1,   29,    7, & ! 6   
     &     29,   14,    0,   30,    0,    0,    0,    0,   27,   20, & ! 7   
     &      0,   14,    0,    2,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,   19,    8,    0,    8,   42,   63,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   31,    0,    7,   33, & ! 2   
     &     29,   31,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,   19,    0,    0, & ! 4   
     &      0,    0,    0,   20,   20,   20,   20,   20,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,  135,  135,  135,  135,  137,  139,  139,  139,  139, & ! 9   
     &    139,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
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
     &      0,    0,    0/     !  4   

      DATA ( IRR( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   28,    1,   29,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,   14,    0, & ! O   
     &      0,    0,    0,    0,    0,   41,   41,    0,    0,   27, & ! 1   
     &     30,   29,   19,    0,    0,    0,    0,   30,    0,    7, & ! 2   
     &      0,   47,   27,    0,   29,   27,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    7,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   46,   46,    0, & ! 5   
     &      0,   30,    0,    0,    0,    0,   66,   20,   26,   14, & ! 6   
     &     67,    8,    0,   20,    0,    0,    0,    0,   29,   65, & ! 7   
     &      0,    0,    0,   20,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,   30,    0,    0,   30,   30,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,   33,    0, & ! 2   
     &     33,   29,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,   30,    0,    0, & ! 4   
     &      0,    0,    0,   30,   30,   30,   30,   30,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,  136,  137,  137,  137,  138,  140,  140,  140,  140, & ! 9   
     &    140,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
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
     &      0,    0,    0/     !  4   

      DATA ( IRR( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   29,   29,   30,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,   20,   20,    0,    0,   15, & ! 1   
     &      0,    8,   30,    0,    0,    0,    0,    0,    0,    8, & ! 2   
     &      0,    0,   29,    0,   15,   26,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    8,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   59,   61,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,   20,   64,   19,   19, & ! 6   
     &     66,   66,    0,   69,    0,    0,    0,    0,   30,   69, & ! 7   
     &      0,    0,    0,   30,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,   41,    0,    0,    0,   14,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    4,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,  137,  138,  138,  138,    0,    0,    0,    0,    0, & ! 9   
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
     &      0,    0,    0/     !  4   

      DATA ( IRR( IRXXN, 11 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   30,   30,   32,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   30, & ! 1   
     &      0,   30,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,   27,   48,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,   26,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,   30,   30,   31,   30, & ! 6   
     &     14,   31,    0,    0,    0,    0,    0,    0,   20,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,   20,    0,    0,    0,    8,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,   38,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,  138,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
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
     &      0,    0,    0/     !  4   

      DATA ( IRR( IRXXN, 12 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,   33,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    7, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   15,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,   29,    0,   66,   29, & ! 6   
     &     10,    0,    0,    0,    0,    0,    0,    0,   39,    0, & ! 7   
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
     &      0,  139,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
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
     &      0,    0,    0/     !  4   

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
     &      0,    0,    0,    0,    0,   29,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,   20,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,   69,    0, & ! 7   
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
     &      0,  140,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
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
     &      0,    0,    0/     !  4   

      DATA ( RTDAT( 1,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 6.0000D-34, 3.0000D-12, 5.6000D-12, 2.5000D-31, & ! O   
     &     9.0000D-32, 1.2000D-13, 1.0000D+00, 1.0000D+00, 2.1000D-11, & ! +   
     &     2.2000D-10, 1.7000D-12, 1.0000D-14, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.5000D-11, 4.5000D-14, 2.0000D-30, 1.0000D-22, 0.0000D+00, & ! +   
     &     1.0000D-03, 3.3000D-39, 5.0000D-40, 7.0000D-31, 1.0000D+00, & ! 2   
     &     1.8000D-11, 1.0000D-20, 3.2000D-30, 2.4000D-14, 3.5000D-12, & ! +   
     &     6.0950D-14, 1.8000D-31, 4.1000D-05, 1.3000D-12, 2.3000D-13, & ! 3   
     &     3.2200D-34, 1.0000D+00, 2.9000D-12, 1.1000D-10, 5.5000D-12, & ! +   
     &     2.2000D-11, 4.2000D-12, 6.9000D-31, 4.8000D-11, 3.0000D-11, & ! 4   
     &     1.4000D-12, 1.0000D-11, 2.2000D-11, 3.5000D-12, 1.0000D-17, & ! +   
     &     8.5000D-13, 1.0000D+00, 1.0000D+00, 1.0000D+00, 2.6000D-12, & ! 5   
     &     2.6000D-12, 7.5000D-13, 7.5000D-13, 6.8000D-14, 6.8000D-14, & ! +   
     &     6.8000D-14, 3.0100D-12, 1.0000D+00, 2.2300D-11, 5.7800D-11, & ! 6   
     &     7.4300D-13, 2.3900D-12, 8.9000D-13, 1.4400D-13, 2.4500D-12, & ! +   
     &     2.8000D-12, 4.1000D-13, 9.5000D-14, 3.8000D-12, 1.0000D+00, & ! 7   
     &     7.3000D-12, 9.0000D-12, 1.0000D+00, 1.0000D+00, 3.4000D-11, & ! +   
     &     5.8000D-16, 9.7000D-15, 2.4000D+12, 5.6000D-12, 5.6000D-15, & ! 8   
     &     4.0000D-13, 1.8000D-11, 5.6000D-12, 1.4000D-12, 1.0000D+00, & ! +   
     &     8.1000D-12, 2.7000D-28, 4.9000D-03, 1.0000D+00, 4.3000D-13, & ! 9   
     &     2.0000D-12, 4.4000D-13, 2.9000D-12, 4.0000D-13, 1.0000D+00, & ! +   
     &     4.0000D-13, 1.3000D-11, 5.1000D-12, 6.5000D-15, 1.0000D+00, & ! O   
     &     6.7000D-12, 2.7000D-28, 1.7000D-03, 1.0000D+00, 3.0000D-13, & ! +   
     &     4.3000D-13, 2.0000D-12, 4.4000D-13, 2.9000D-12, 2.9000D-12, & ! 1   
     &     8.1000D-13, 1.0000D+15, 1.6000D+03, 1.5000D-11, 1.0000D-11, & ! +   
     &     3.2000D-11, 6.5000D-15, 7.0000D-13, 1.0400D-11, 1.0000D-28, & ! 2   
     &     1.2000D-14, 3.3000D-12, 2.3000D-11, 1.0000D-11, 8.4000D-15, & ! +   
     &     9.6000D-13, 1.8000D-12, 2.7000D-12, 1.9000D-13, 1.7000D-12, & ! 3   
     &     1.4000D-11, 2.1000D-12, 5.5000D-12, 1.5300D-12, 3.8000D-12, & ! +   
     &     2.1000D-12, 2.8600D-13, 2.5400D-12, 2.4000D-13, 1.0000D-02, & ! 4   
     &     1.9000D-12, 4.0000D-02, 4.4000D-11, 5.4000D-17, 3.8000D-12, & ! +   
     &     7.0000D-11, 1.7000D-10, 2.5400D-12, 2.4000D-13, 1.1000D-11, & ! 5   
     &     1.1000D-11, 1.0000D-04, 1.7000D-11, 1.7000D-11, 1.8000D-11, & ! +   
     &     1.0000D+00, 3.6000D-11, 2.7000D-11, 2.3900D-12, 1.0000D+00, & ! 6   
     &     7.4300D-13, 7.8600D-15, 3.0300D-12, 3.3600D-11, 7.1000D-18, & ! +   
     &     1.0000D-15, 3.6000D-03, 3.6000D-11, 1.5000D-11, 2.6000D-12, & ! 7   
     &     7.5000D-13, 6.8000D-14, 6.8000D-14, 1.2000D-15, 3.7000D-12, & ! +   
     &     3.3000D-31, 6.9000D-12, 8.7000D-12, 1.5000D-19, 1.0000D+00, & ! 8   
     &     1.0000D+00, 2.3000D-11, 1.6300D-14, 6.4000D-12, 2.7000D-12, & ! +   
     &     5.0000D-13, 1.0000D+00, 6.6000D-12, 5.0000D-11, 8.3000D-11, & ! 9   
     &     1.0700D-10, 2.5000D-10, 3.5000D-10, 4.3000D-10, 8.2000D-11, & ! +   
     &     7.9000D-11, 1.3000D-10, 5.5000D-11, 8.2000D-11, 6.5800D-13, & ! O   
     &     6.1000D-11, 1.2000D-10, 1.2000D-10, 1.8000D-31, 1.0000D+00, & ! +   
     &     2.7000D-12, 1.9000D-13, 2.7000D-12, 1.9000D-13, 2.4700D-12, & ! 1   
     &     2.7000D-12, 1.9000D-13, 1.1600D-14, 1.9700D-10, 1.9000D-11, & ! +   
     &     2.7000D-12, 1.9000D-13, 2.7000D-12, 6.7000D-12, 1.2100D-11, & ! 2   
     &     1.6000D+16, 1.0000D+00, 2.9000D-11, 5.2000D-13, 2.0000D-12, & ! +   
     &     4.4000D-13, 2.9000D-12, 2.9000D-12, 1.2900D-12, 2.7000D-12, & ! 3   
     &     2.0500D-13, 7.2600D-12, 2.7000D-12, 2.0500D-13, 1.1000D-12, & ! +   
     &     2.7000D-12, 2.0500D-13, 5.7000D-12, 2.7000D-12, 2.0500D-13, & ! 4   
     &     6.0000D-12, 3.3000D-11, 2.7000D-12, 2.0500D-13, 2.3200D-12, & ! +   
     &     2.7000D-12, 2.0500D-13, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 8   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! +   
     &     1.2500D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, & ! 9   
     &     4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, & ! +   
     &     4.0000D-11, 9.0000D-12, 5.8000D-16, 3.4000D-11, 1.0000D+00, & ! O   
     &     1.0000D+00, 8.2000D-11, 5.6000D-12, 1.4000D-12, 1.8000D-11, & ! +   
     &     1.0000D+00, 7.9000D-11, 1.4000D-11, 8.2000D-15, 1.7900D-13, & ! 1   
     &     2.5100D-10, 2.0000D-11, 2.6100D-19, 1.7000D-11, 1.0000D+00, & ! +   
     &     2.3700D-10, 2.0000D-11, 2.6100D-19, 1.7000D-11, 1.0000D+00, & ! 2   
     &     2.3700D-10, 1.8000D-12, 6.1000D-11, 1.7000D-11, 1.4000D-10, & ! +   
     &     1.2200D-11, 1.5000D-10, 1.3000D-11, 1.5000D-10, 2.7900D-11, & ! 3   
     &     1.2000D-11, 6.3000D-16, 1.2000D-12, 4.7000D-10, 2.8100D-11, & ! +   
     &     7.5100D-11, 1.7400D-15, 2.8100D-11, 5.3000D-10, 2.1100D-18, & ! 4   
     &     2.6000D-18, 8.5000D-19, 7.7000D-14, 2.2500D-33, 2.1000D-11, & ! +   
     &     3.6000D-16, 1.5000D-11, 7.1500D-12, 1.4000D-11, 2.1600D-11, & ! 5   
     &     3.2400D-11, 1.5000D-10, 1.5000D-10, 1.2500D-10, 1.4000D-10, & ! +   
     &     1.0000D-10, 2.5400D+14, 1.0000D+12, 3.8000D-02, 1.8000D-10, & ! 6   
     &     1.5000D-12, 1.0000D-10, 1.6000D-11, 9.1000D-11, 2.0000D-13, & ! +   
     &     3.2000D-13, 1.0000D+18, 2.1000D+15, 8.4000D-11, 4.7000D-13, & ! 7   
     &     1.1000D-12, 1.3000D-12, 3.0000D-12, 1.2000D-11, 1.4400D-11, & ! +   
     &     2.5850D-12, 1.1750D-12, 9.4000D-13, 2.4900D-11, 9.0000D-12, & ! 8   
     &     2.0000D-12, 2.9000D-12, 3.0000D-31, 6.5000D-31, 1.8000D-32, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.7000D-11, 3.7000D-12, 1.4000D-11, & ! O   
     &     1.1000D-11, 2.7000D-12, 2.9000D-14, 8.7000D-12, 1.7800D-11, & ! +   
     &     1.9000D-11, 7.5000D-11, 1.6000D-11, 1.7000D-11, 1.3000D-11, & ! 1   
     &     9.7000D-12, 4.1000D-12, 1.6000D-12, 1.7000D-12, 1.6000D-12, & ! +   
     &     4.0000D-12, 2.4000D-12, 2.4000D-12, 2.4000D-12, 2.4000D-12, & ! 2   
     &     5.2000D-31, 4.2000D-31, 9.5000D-13, 2.3000D-12, 4.1000D-13, & ! +   
     &     1.5400D-14, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00/           !        4   

      DATA ( RTDAT( 2,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00,-2.4000D+00, 0.0000D+00, 0.0000D+00,-1.8000D+00, & ! O   
     &    -1.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00,-4.4000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -3.5000D+00, 0.0000D+00, 0.0000D+00,-2.6000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00,-4.5000D+00, 4.6000D+02, 0.0000D+00, & ! +   
     &     2.7000D+02,-3.2000D+00, 0.0000D+00, 0.0000D+00, 6.0000D+02, & ! 3   
     &     2.8000D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-7.1000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00,-7.1000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-8.0000D-01, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 5.9000D+01, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -4.3000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.1600D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-2.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.0700D+00, & ! 2   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00,-1.0000D+00,-3.5000D+00,-1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &    -3.2000D+00,-2.4000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        4   

      DATA ( RTDAT( 3,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00,-1.5000D+03, 1.8000D+02, 0.0000D+00, & ! O   
     &     0.0000D+00,-2.4500D+03, 0.0000D+00, 0.0000D+00, 1.0200D+02, & ! +   
     &     0.0000D+00,-9.4000D+02,-4.9000D+02, 0.0000D+00, 0.0000D+00, & ! 1   
     &     1.7000D+02,-1.2600D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -1.1000D+04, 5.3000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &    -3.9000D+02, 0.0000D+00, 0.0000D+00, 2.7000D-17, 2.5000D+02, & ! +   
     &     6.8570D-34, 0.0000D+00,-1.0650D+04, 3.8000D+02, 1.7000D-33, & ! 3   
     &     2.3800D-54, 0.0000D+00,-1.6000D+02, 0.0000D+00,-2.0000D+03, & ! +   
     &     1.2000D+02,-2.4000D+02, 0.0000D+00, 2.5000D+02, 2.0000D+02, & ! 4   
     &    -2.0000D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -2.4500D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.6500D+02, & ! 5   
     &     3.6500D+02, 7.0000D+02, 7.0000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.9000D+02, 0.0000D+00, 3.7200D+02,-4.0000D+02, & ! 6   
     &     7.0000D+02, 3.6500D+02, 8.0000D+02, 3.4300D-33,-1.7750D+03, & ! +   
     &     3.0000D+02, 7.5000D+02, 3.9000D+02, 2.0000D+02, 0.0000D+00, & ! 7   
     &    -6.2000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.6000D+03, & ! +   
     &     0.0000D+00, 6.2500D+02,-7.0000D+03, 0.0000D+00, 2.3000D+03, & ! 8   
     &     0.0000D+00,-1.1000D+03, 2.7000D+02,-1.9000D+03, 0.0000D+00, & ! +   
     &     2.7000D+02, 0.0000D+00,-1.2100D+04, 0.0000D+00, 1.0400D+03, & ! 9   
     &     5.0000D+02, 1.0700D+03, 5.0000D+02, 2.0000D+02, 0.0000D+00, & ! +   
     &     2.0000D+02,-8.7000D+02, 4.0500D+02, 0.0000D+00, 0.0000D+00, & ! O   
     &     3.4000D+02, 0.0000D+00,-1.1280D+04, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0400D+03, 5.0000D+02, 1.0700D+03, 5.0000D+02, 5.0000D+02, & ! 1   
     &     0.0000D+00,-8.0000D+03, 0.0000D+00, 0.0000D+00,-2.8000D+02, & ! +   
     &     0.0000D+00,-1.9000D+03,-2.1600D+03,-7.9200D+02, 0.0000D+00, & ! 2   
     &    -2.6300D+03,-2.8800D+03, 0.0000D+00, 5.5000D+02,-1.1000D+03, & ! +   
     &    -2.7000D+02, 3.5500D+02, 3.6000D+02, 1.3000D+03, 9.5000D+02, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.6000D+02, 1.3000D+03, 0.0000D+00, & ! 4   
     &     1.9000D+02, 0.0000D+00, 0.0000D+00,-5.0000D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.6000D+02, 1.3000D+03, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 1.1600D+02, 1.1600D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.9000D+02, 3.6500D+02, 0.0000D+00, & ! 6   
     &     7.0000D+02,-1.9120D+03,-4.4800D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 4.4900D+02, 3.6500D+02, & ! 7   
     &     7.0000D+02, 0.0000D+00, 0.0000D+00,-8.2100D+02, 1.7500D+02, & ! +   
     &     0.0000D+00,-2.3000D+02,-1.0700D+03, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00,-2.0000D+02, 0.0000D+00, 2.9000D+02, 2.2000D+02, & ! +   
     &     0.0000D+00, 0.0000D+00,-1.2400D+03, 0.0000D+00,-1.0000D+02, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-3.4000D+01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 4.5000D+01, 5.8000D+01, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.6000D+02, 1.3000D+03, 3.6000D+02, 1.3000D+03,-2.0600D+02, & ! 1   
     &     3.6000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.6000D+02, 1.3000D+03, 3.7400D+02, 3.4000D+02, 0.0000D+00, & ! 2   
     &    -1.3486D+04, 0.0000D+00, 0.0000D+00, 9.8000D+02, 5.0000D+02, & ! +   
     &     1.0700D+03, 5.0000D+02, 5.0000D+02, 0.0000D+00, 3.6000D+02, & ! 3   
     &     1.3000D+03, 0.0000D+00, 3.6000D+02, 1.3000D+03, 0.0000D+00, & ! +   
     &     3.6000D+02, 1.3000D+03, 0.0000D+00, 3.6000D+02, 1.3000D+03, & ! 4   
     &     0.0000D+00, 0.0000D+00, 3.6000D+02, 1.3000D+03, 0.0000D+00, & ! +   
     &     3.6000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.6000D+03, 0.0000D+00, & ! O   
     &     0.0000D+00,-3.4000D+01, 2.7000D+02,-1.9000D+03,-1.1000D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.2400D+02,-2.0700D+03, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-3.1310D+03, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-3.1310D+03, 0.0000D+00, & ! 2   
     &     0.0000D+00, 3.5500D+02, 0.0000D+00, 1.1600D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     4.4000D+02,-5.8000D+02, 4.9000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-1.2600D+03, 0.0000D+00, 0.0000D+00,-1.2565D+03, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-6.8000D+02,-8.3000D+02, & ! +   
     &     0.0000D+00,-1.0900D+03, 3.0000D+02, 5.4000D+02, 1.8000D+02, & ! 5   
     &     1.8000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-9.7700D+03,-9.7700D+03, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 4.4000D+02,-1.4600D+02, 0.0000D+00, & ! +   
     &    -9.2500D+02,-1.3670D+04,-1.3670D+04,-2.6200D+03,-1.6700D+03, & ! 7   
     &     5.4200D+02,-1.8300D+03, 5.1000D+02, 5.1000D+02, 0.0000D+00, & ! +   
     &     2.8000D+02, 2.8000D+02, 2.8000D+02, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00,-1.1000D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-8.0000D+02, 5.4500D+02,-5.9000D+02, & ! O   
     &     0.0000D+00, 0.0000D+00, 8.4000D+02, 2.6000D+02, 3.6500D+02, & ! +   
     &     2.4000D+02, 0.0000D+00, 0.0000D+00,-8.0000D+02,-3.6000D+02, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-7.1000D+02, & ! +   
     &    -1.4700D+03,-9.0000D+02,-9.0000D+02,-9.0000D+02,-9.3000D+02, & ! 2   
     &     0.0000D+00, 0.0000D+00, 5.5000D+02, 2.6000D+02, 2.9000D+02, & ! +   
     &     1.0000D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        4   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &      5,    6,   18,   21,   24,   28,   29,   31,   32,   33, & 
     &     35,   36,   43,   69,   92,   93,  107,  108,  125,  181, & 
     &    209,  388,  389,  390,  426,  427/

      DATA ( RFDAT( 1,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     2.2000D-11, 3.0000D-11, 1.4000D-12, 9.7000D+14, 3.6000D-11, & 
     &     3.0000D-11, 2.1990D+03, 2.7000D+02, 4.7000D-12, 4.8000D+15, & 
     &     1.0000D+03, 3.2000D+03, 2.6000D-11, 0.0000D+00, 1.2000D-11, & 
     &     5.4000D+16, 1.2000D-11, 8.3000D+16, 8.8000D-12, 1.6000D-12, & 
     &     1.0000D-10, 6.6000D-11, 7.6000D-12, 1.7000D-11, 6.9000D-12, & 
     &     2.7000D-11/

      DATA ( RFDAT( 2,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &    -7.0000D-01, 0.0000D+00,-7.0000D-01, 1.0000D-01,-1.0000D-01, & 
     &     0.0000D+00, 6.5000D-34,-1.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-9.0000D-01, & 
     &     0.0000D+00,-9.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &    -1.0000D+00, 0.0000D+00,-1.5000D+00, 0.0000D+00,-2.9000D+00, & 
     &     0.0000D+00/

      DATA ( RFDAT( 3,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.1080D+04, 0.0000D+00, & 
     &     0.0000D+00, 1.3350D+03, 1.0000D+00, 0.0000D+00,-1.1170D+04, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &    -1.3830D+04, 0.0000D+00,-1.3940D+04, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00/

      DATA ( RFDAT( 4,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     6.0000D-01, 6.0000D-01, 6.0000D-01, 4.5000D-01, 6.0000D-01, & 
     &     4.1000D-01, 0.0000D+00,-5.9680D-14, 6.0000D-01, 6.0000D-01, & 
     &     0.0000D+00, 0.0000D+00, 6.0000D-01, 0.0000D+00, 3.0000D-01, & 
     &     3.0000D-01, 3.0000D-01, 3.6000D-01, 6.0000D-01, 6.0000D-01, & 
     &     6.0000D-01, 6.0000D-01, 6.0000D-01, 6.0000D-01, 6.0000D-01, & 
     &     6.0000D-01/

      DATA ( RFDAT( 5,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.2400D+00, 0.0000D+00, 2.7000D+02, 1.0000D+00, 1.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.4100D+00, & 
     &     1.4100D+00, 1.4100D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00/

      REAL               :: SC( NRXNS,MXPRD )

      DATA ( SC( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        2.00000,    1.00000,    1.00000,    2.00000,    2.00000, & ! +   
     &        1.00000,    2.00000,    2.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    2.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    0.61000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        0.50000,    1.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.90400,    1.00000, & ! 6   
     &        0.27500,    0.27500,    0.22000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.37000,    0.70000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    2.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.60000,    0.41000, & ! 9   
     &        0.90000,    0.90000,    2.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    0.60000,    1.00000, & ! +   
     &        0.41000,    0.90000,    0.90000,    2.00000,    1.00000, & ! 1   
     &        0.87000,    0.96000,    1.00000,    1.00000,    0.20000, & ! +   
     &        0.80000,    0.18000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.24000,    1.30000,    0.65000, & ! +   
     &        1.18000,    0.28000,    0.86000,    0.00000,    0.06000, & ! 3   
     &        0.30000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    0.60000,    0.03000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.86000,    0.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    0.70000,    0.70000,    1.00000, & ! +   
     &        1.00000,    0.75000,    1.00000,    0.10000,    0.59800, & ! 6   
     &        1.00000,    0.65000,    0.20000,    1.56500,    0.11400, & ! +   
     &        0.35700,    0.33300,    0.15000,    0.75000,    1.00000, & ! 7   
     &        1.00000,    0.00000,    0.00000,    0.57000,    0.47000, & ! +   
     &        1.00000,    1.00000,    0.99100,    0.20000,    2.00000, & ! 8   
     &        1.00000,    1.00000,    0.30000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    0.30000,    0.15000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    0.60000,    1.00000,    0.15000,    1.90000, & ! +   
     &        1.00000,    1.00000,    1.65000,    1.00000,    1.30000, & ! 3   
     &        1.00000,    1.00000,    1.22000,    1.00000,    1.00000, & ! +   
     &        1.53000,    1.00000,    1.00000,    1.24000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    0.87000,    1.00000,    1.00000, & ! +   
     &        1.40000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.50000,    1.00000, & ! 7   
     &        0.85710,    1.14290,    0.85710,    1.14290,    0.71430, & ! +   
     &        0.71430,    0.80000,    0.90000,    0.50000,    0.50000, & ! 8   
     &        1.50000,    1.42860,    1.42860,    1.71430,    1.71430, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! O   
     &        0.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.50000, & ! 4   
     &        1.00000,    1.00000,    0.50000,    0.50000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    2.00000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.75000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    2.00000,    1.00000,    1.00000,    2.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    2.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    3.00000, & ! +   
     &        1.00000,    2.00000,    2.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.75000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    2.00000,    3.00000,    2.00000, & ! +   
     &        1.00000,    1.00000,    1.00000/           !        4   

      DATA ( SC( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 1   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 2   
     &        0.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        1.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.61000,    1.00000,    1.00000,    0.00000, & ! 5   
     &        0.50000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.50000,    1.00000,    0.93300,    0.00000, & ! 6   
     &        0.27500,    0.27500,    0.22000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.74000,    0.30000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! 8   
     &        0.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.60000,    0.15000, & ! 9   
     &        0.90000,    0.10000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    0.00000,    1.00000,    0.60000,    1.00000, & ! +   
     &        0.15000,    0.90000,    0.10000,    2.00000,    1.00000, & ! 1   
     &        0.13000,    0.60000,    0.00000,    0.00000,    0.30000, & ! +   
     &        0.33000,    0.74000,    1.00000,    1.70000,    1.56000, & ! 2   
     &        0.63000,    1.00000,    0.66000,    0.70000,    0.35000, & ! +   
     &        0.64000,    0.10000,    1.20000,    0.00000,    0.12000, & ! 3   
     &        1.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! 4   
     &        0.00000,    1.00000,    0.40000,    0.62000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.20000,    0.00000,    1.00000, & ! 5   
     &        0.00000,    1.00000,    0.50000,    0.50000,    1.00000, & ! +   
     &        1.00000,    0.50000,    1.00000,    0.90000,    1.00000, & ! 6   
     &        0.00000,    0.60000,    0.80000,    0.16700,    0.15000, & ! +   
     &        0.28200,    0.06700,    5.12000,    1.25000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.07000,    0.28000, & ! +   
     &        1.00000,    0.95000,    0.99100,    0.80000,    0.00000, & ! 8   
     &        1.00000,    0.00000,    1.40000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.87000,    0.99100, & ! 9   
     &        2.00000,    0.33000,    0.70000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! O   
     &        0.88000,    0.84000,    0.84000,    0.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.47000,    1.00000,    0.00000, & ! 2   
     &        1.00000,    0.60000,    1.00000,    0.44000,    0.90000, & ! +   
     &        0.30000,    1.00000,    1.00000,    0.00000,    0.15000, & ! 3   
     &        0.00000,    0.00000,    0.53000,    0.00000,    0.00000, & ! +   
     &        0.21000,    0.00000,    0.00000,    0.59000,    0.00000, & ! 4   
     &        1.00000,    0.00000,    1.15000,    0.00000,    0.00000, & ! +   
     &        0.60000,    0.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.50000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.48570,    0.30030,    0.38560,    0.21810, & ! 9   
     &        0.24120,    0.66640,    0.28580,    0.33030,    0.34440, & ! +   
     &        0.38860,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.58000,    0.52000,    0.04500, & ! 1   
     &        0.58000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.50000, & ! 4   
     &        1.00000,    1.00000,    0.50000,    0.50000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.25000,    1.00000,    1.00000,    2.00000,    2.00000, & ! 7   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 9   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    2.00000,    1.00000, & ! 2   
     &        0.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        0.25000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    1.00000/           !        4   

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
     &        0.00000,    0.39000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.50000,    0.50000,    0.06700,    0.00000, & ! 6   
     &        1.12500,    0.12500,    0.10000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.63000,    0.30000,    1.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.40000,    0.44000, & ! 9   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! O   
     &        1.00000,    0.00000,    0.00000,    0.40000,    0.00000, & ! +   
     &        0.44000,    1.00000,    0.00000,    2.00000,    1.00000, & ! 1   
     &        0.11000,    0.94000,    0.00000,    0.00000,    0.30000, & ! +   
     &        0.62000,    0.32000,    0.91000,    1.00000,    0.22000, & ! 2   
     &        0.13000,    2.00000,    0.10000,    1.00000,    0.25000, & ! +   
     &        1.00000,    0.18000,    0.86000,    0.00000,    1.12000, & ! 3   
     &        0.60000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    1.00000,    0.00000,    0.70000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.34400,    0.00000,    1.00000, & ! 5   
     &        0.00000,    0.00000,    0.20000,    0.20000,    0.00000, & ! +   
     &        1.00000,    0.25000,    0.00000,    0.67300,    0.80000, & ! 6   
     &        0.00000,    0.20000,    1.00000,    0.71300,    0.85000, & ! +   
     &        1.28200,    0.90000,    1.00000,    0.25000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.76000,    1.03000, & ! +   
     &        1.00000,    0.01000,    0.00900,    1.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.13000,    0.99100, & ! 9   
     &        1.00000,    0.67000,    0.45000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! O   
     &        0.88000,    0.84000,    0.84000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 2   
     &        0.00000,    0.40000,    1.00000,    0.29000,    0.30000, & ! +   
     &        0.90000,    1.00000,    0.35000,    0.00000,    0.55000, & ! 3   
     &        0.00000,    0.00000,    0.25000,    0.00000,    0.00000, & ! +   
     &        0.26000,    0.00000,    0.00000,    0.17000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.68000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.33000,    0.33000, & ! 5   
     &        0.33000,    0.33000,    0.33000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00620,    0.28620,    0.09500,    0.30630, & ! 9   
     &        0.20890,    0.01430,    0.39310,    0.22720,    0.27490, & ! +   
     &        0.24210,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 4   
     &        0.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000/           !        4   

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
     &        0.00000,    0.39000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.50000,    0.02900,    0.00000, & ! 6   
     &        0.82500,    0.82500,    0.66000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.40000,    0.44000, & ! 9   
     &        0.10000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        1.00000,    0.00000,    0.00000,    0.40000,    0.00000, & ! +   
     &        0.44000,    0.10000,    0.00000,    0.00000,    1.00000, & ! 1   
     &        0.06000,   -2.10000,    0.00000,    0.00000,    0.20000, & ! +   
     &        0.80000,    0.22000,    0.09000,    0.70000,    1.00000, & ! 2   
     &        0.13000,    0.00000,    0.10000,    1.00000,    0.25000, & ! +   
     &        1.00000,    0.65000,    0.14000,    0.00000,    0.13000, & ! 3   
     &        0.36000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.03000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.34400,    0.00000,    1.00000, & ! 5   
     &        0.00000,    0.00000,    0.80000,    0.80000,    0.00000, & ! +   
     &        0.00000,    0.25000,    0.00000,    0.90000,    0.80000, & ! 6   
     &        0.00000,    0.06600,    0.80000,    0.50300,    0.15400, & ! +   
     &        0.92500,    0.83200,    0.00000,    0.28000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.18000,    0.25000, & ! +   
     &        0.00000,    0.08000,    1.00000,    0.80000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.11000,    0.00900, & ! 9   
     &        1.00000,    2.00000,    0.55000,    0.85000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.12000,    0.16000,    0.16000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.40000,    0.00000,    0.29000,    0.60000, & ! +   
     &        0.60000,    1.00000,    0.65000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.55000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.33000,    0.33000, & ! 5   
     &        0.33000,    0.33000,    0.33000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00250,    0.00410,    0.13730,    0.01530, & ! 9   
     &        0.30000,    0.01230,    0.01390,    0.26070,    0.04910, & ! +   
     &        0.06400,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000/           !        4   

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
     &        0.00000,    0.00000,    0.00000,    0.02900,    0.00000, & ! 6   
     &        0.65000,    0.65000,    0.52000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.15000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.40000,    0.00000, & ! +   
     &        0.15000,    0.10000,    0.00000,    0.00000,    0.00000, & ! 1   
     &       -0.11000,    0.04000,    0.00000,    0.00000,    0.20000, & ! +   
     &        0.95000,    0.10000,    0.56000,    0.30000,    0.00000, & ! 2   
     &        0.37000,    0.00000,    0.10000,    0.00000,    0.50000, & ! +   
     &        0.00000,    0.07200,    0.52000,    0.00000,    0.73200, & ! 3   
     &        0.48000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.69000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.14000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    1.10000,    1.10000,    0.00000, & ! +   
     &        0.00000,    0.25000,    0.00000,    0.90000,    0.20000, & ! 6   
     &        0.00000,    0.26600,    0.20000,    0.33400,    0.26800, & ! +   
     &        0.64300,    0.70000,    0.00000,    1.66000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.24000,    0.47000, & ! +   
     &        0.00000,    0.05000,    0.00000,    0.20000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.06000,    1.00000, & ! 9   
     &        0.00000,    1.00000,    0.30000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.40000,    0.00000,    0.44000,    0.10000, & ! +   
     &        0.60000,    0.35000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.15000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.33000,    0.33000, & ! 5   
     &        0.33000,    0.33000,    0.33000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00260,    0.00350,    0.00050,    0.10430, & ! 9   
     &        0.20280,    0.12390,    0.10270,    0.07020,    0.25770, & ! +   
     &        0.03850,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
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
     &        0.00000,    0.00000,    0.00000/           !        4   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.07400,    1.00000,    0.20000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.40000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.76000,    0.02000,    0.00000,    0.00000,    0.20000, & ! +   
     &       -0.70000,    0.33000,    0.35000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.10000,    0.00000,    0.50000, & ! +   
     &        0.00000,    1.00000,    0.33600,    0.00000,    0.06000, & ! 3   
     &        0.24000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.08000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.30000,    0.30000,    0.00000, & ! +   
     &        0.00000,    0.25000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.20000,    0.80000,    0.16800,    0.06400, & ! +   
     &        0.85000,    1.03300,    0.00000,    0.47000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00100,    0.53000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.80000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,   -0.11000,    0.00000, & ! 9   
     &        0.00000,   -1.00000,    0.30000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.15000,    0.00000, & ! +   
     &        0.10000,    0.65000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.43000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,   -0.66000,   -0.66000, & ! 5   
     &       -0.66000,   -0.66000,   -0.66000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00230,    0.22390,    0.20510,    0.18930, & ! 9   
     &        0.04710,    0.18310,    0.20450,    0.11160,    0.07390, & ! +   
     &        0.26670,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
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
     &        0.00000,    0.00000,    0.00000/           !        4   

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
     &        0.25100,    0.25100,    1.96000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.05000,    0.50000,    0.00000,    0.00000,    0.01000, & ! +   
     &        0.00000,    0.44000,   -1.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.50000, & ! +   
     &        0.00000,    0.00000,    0.33600,    0.00000,    0.06000, & ! 3   
     &        0.24000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.76000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.15000,    1.00000,    0.25200,    0.02000, & ! +   
     &        0.07500,    0.70000,    0.00000,    1.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    7.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    2.40000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.76000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    1.70000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.15000,    0.00000, & ! +   
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
     &        0.00000,    0.29440,    0.18200,    0.17640,    0.16680, & ! 9   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000/           !        4   

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
     &        2.45000,    2.45000,    0.80000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.20000, & ! +   
     &        0.00000,   -1.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.06000, & ! 3   
     &        0.12000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.20000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.35000,    2.40000,    0.33000,    0.36000, & ! +   
     &        0.07500,    0.26700,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.21000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.05000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.41000,    0.00000, & ! +   
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
     &        0.00000,    0.20210,    0.00000,    0.00000,    0.00000, & ! 9   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000/           !        4   

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
     &        0.00000,    0.00000,    0.20000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.10000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.10000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.06600,    0.00000,    0.13000,    0.22500, & ! +   
     &        0.15000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.39000,    0.00000, & ! +   
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
     &        0.00000,    0.00190,    0.00000,    0.00000,    0.00000, & ! 9   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000/           !        4   

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
     &        0.24000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.12000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00230,    0.00000,    0.00000,    0.00000, & ! 9   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000/           !        4   

      INTEGER            :: NREACT( NRXNS )

      DATA ( NREACT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    2,    2,    2,    2,    2,    1,    1,    1, & ! O   
     &      1,    2,    2,    1,    1,    2,    2,    2,    1,    1, & ! 1   
     &      1,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    1,    2,    2,    2,    1,    2,    1,    1, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    1,    1,    1,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    1, & ! 6   
     &      2,    2,    2,    2,    1,    2,    2,    1,    1,    2, & ! 7   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    1, & ! 8   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    1, & ! 9   
     &      2,    2,    2,    2,    1,    2,    2,    1,    1,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    1,    1,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    1,    2,    1,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    1,    2,    2,    2, & ! 5   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    1,    1,    2,    2,    2,    2, & ! 8   
     &      2,    1,    1,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    1,    1,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      2,    2,    2,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    1,    1,    2,    2,    2,    2, & ! O   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 1   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    1,    1,    1,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    1,    1,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 9   
     &      1,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1/     !  4   
      INTEGER            :: NPRDCT( NRXNS )

      DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    2,    1,    1,    2,    1,    1,    1, & ! 1   
     &      2,    1,    1,    1,    2,    1,    2,    1,    1,    2, & ! 2   
     &      1,    1,    2,    1,    1,    1,    1,    1,    2,    1, & ! 3   
     &      1,    1,    1,    0,    1,    2,    1,    2,    1,    1, & ! 4   
     &      1,    4,    2,    2,    1,    2,    1,    1,    0,    0, & ! 5   
     &      0,    3,    4,    5,    1,    8,    8,    9,    1,    1, & ! 6   
     &      3,    1,    3,    3,    3,    2,    2,    2,    1,    3, & ! 7   
     &      3,    1,    2,    3,    1,    1,    2,    1,    2,    3, & ! 8   
     &      2,    1,    2,    4,    5,    4,    2,    1,    1,    2, & ! 9   
     &      1,    2,    1,    2,    3,    4,    1,    2,    6,    2, & ! O   
     &      5,    5,    2,    3,    4,    7,    7,    1,    1,    9, & ! 1   
     &      6,    8,    7,    5,    4,    5,    3,    6,    4,    7, & ! 2   
     &      4,    6,    7,    0,    8,   10,    1,    1,    1,    2, & ! 3   
     &      1,    1,    2,    1,    2,    1,    3,    2,    8,    2, & ! 4   
     &      1,    2,    5,    0,    4,    1,    2,    7,    7,    2, & ! 5   
     &      3,    6,    2,    5,    5,    1,    9,    8,   10,    9, & ! 6   
     &      9,    8,    3,    7,    1,    1,    0,    0,   10,    7, & ! 7   
     &      3,    5,    4,    7,    1,    2,    1,    2,    2,    1, & ! 8   
     &      2,    3,    2,    8,    5,    4,    6,    8,    5,    3, & ! 9   
     &      2,    2,    3,    3,    1,    4,    4,    4,    1,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    3,    1,    2,    5,    3,    8,    5, & ! 2   
     &      6,    6,    4,    1,    3,    1,    1,    3,    1,    1, & ! 3   
     &      3,    1,    1,    3,    1,    2,    1,    6,    1,    1, & ! 4   
     &      2,    1,    4,    6,    6,    6,    6,    6,    3,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    2,    2,    1,    1, & ! 6   
     &      1,    1,    1,    2,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 8   
     &      2,   10,    7,    7,    7,    6,    6,    6,    6,    6, & ! 9   
     &      6,    1,    1,    1,    0,    0,    1,    1,    1,    1, & ! O   
     &      0,    1,    2,    2,    2,    2,    1,    1,    1,    0, & ! 1   
     &      1,    1,    1,    1,    0,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    3,    2,    2,    3,    3,    1, & ! 4   
     &      1,    1,    2,    1,    2,    1,    1,    1,    2,    1, & ! 5   
     &      2,    2,    1,    1,    2,    2,    2,    1,    2,    1, & ! 6   
     &      4,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    1,    2,    2,    3,    2,    1,    1,    1, & ! 8   
     &      2,    2,    3,    3,    1,    2,    1,    2,    2,    2, & ! 9   
     &      2,    2,    1,    1,    1,    1,    1,    1,    2,    2, & ! O   
     &      2,    2,    2,    3,    2,    2,    2,    3,    2,    2, & ! 1   
     &      2,    3,    3,    3,    4,    1,    1,    2,    2,    1, & ! 2   
     &      4,    2,    2,    2,    2,    2,    2,    1,    3,    4, & ! 3   
     &      4,    2,    2/     !  4   

      INTEGER, PARAMETER :: NMPHOT =  62
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    8,    9,   14,   15,   25,   37,   52,   53,   54, & ! O   
     &     63,   75,   78,   79,   90,   94,  100,  105,  109,  145, & ! 1   
     &    147,  161,  172,  185,  186,  192,  210,  227,  253,  254, & ! 2   
     &    255,  256,  257,  258,  259,  305,  306,  311,  320,  325, & ! 3   
     &    391,  392,  393,  394,  395,  396,  397,  398,  399,  400, & ! 4   
     &    401,  402,  432,  433,  434,  435,  436,  437,  438,  439, & ! 5   
     &    440,  441/     !  6   

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & ! O   
     &     11,   11,   12,   13,   14,   15,   16,   17,   15,    1, & ! 1   
     &      1,   18,   19,   20,   21,   22,   23,   15,   24,   25, & ! 2   
     &     24,   24,   26,   26,   24,   12,   13,   14,   19,   19, & ! 3   
     &     27,   28,   29,   30,   31,   32,   33,   34,   35,   36, & ! 4   
     &     37,   38,   39,   40,   41,   42,   43,   44,   45,   46, & ! 5   
     &     47,   48/     !  6   

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & ! O   
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & ! 1   
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & ! 2   
     &     31,   32,   33,   34,   35,   36,   37,   38,   39,   40, & ! 3   
     &     41,   42,   43,   44,   45,   46,   47,   48,   49,   50, & ! 4   
     &     51,   52,   53,   54,   55,   56,   57,   58,   59,   60, & ! 5   
     &     61,   62/     !  6   

      INTEGER, PARAMETER :: MHETERO =  18
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    260,  261,  262,  263,  264,  265,  266,  267,  268,  269, & 
     &    270,  271,  272,  273,  274,  275,  442,  443/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    1,    1,    1,    1,    1,    1,    2,    3,    4, & 
     &      5,    6,    6,    7,    8,    9,   10,   11/

      INTEGER, PARAMETER :: NPHOTAB =  48
      CHARACTER( 16 )    :: PHOTAB( NPHOTAB )

      DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & 
     &   'NO2_IUPAC10     ', 'O3_O3P_IUPAC10  ', 'O3_O1D_IUPAC10  ', & 
     &   'NO3NO2_06       ', 'NO3NO_06        ', 'HONO_IUPAC10    ', & 
     &   'H2O2_IUPAC10    ', 'PNA_IUPAC10     ', 'HNO3_IUPAC10    ', & 
     &   'N2O5_IUPAC10    ', 'MEPX_IUPAC10    ', 'FORM_R_IUPAC10  ', & 
     &   'FORM_M_IUPAC10  ', 'ALD2_R_IUPAC10  ', 'PAN_IUPAC10     ', & 
     &   'PACD_CB05       ', 'ALDX_R_IUPAC10  ', 'MGLY_IUPAC10    ', & 
     &   'ACRO_09         ', 'CL2_IUPAC04     ', 'HOCL_IUPAC04    ', & 
     &   'FMCL_IUPAC04    ', 'CLNO2           ', 'NTR_IUPAC10     ', & 
     &   'NOA_14          ', 'NBO_14          ', 'CH3I_IUPAC10    ', & 
     &   'MI2_IUPAC10     ', 'MIB_IUPAC10     ', 'MIC_IUPAC10     ', & 
     &   'I2_IUPAC10      ', 'IO_IUPAC10      ', 'OIO_06          ', & 
     &   'INO_06          ', 'INO2_06         ', 'IONO2_06        ', & 
     &   'HOI_IUPAC10     ', 'ICL_IUPAC10     ', 'BRCL_IUPAC10    ', & 
     &   'BRO_IUPAC10     ', 'HOBR_IUPAC10    ', 'BRONO2_M_IUPAC10', & 
     &   'BRONO2_R_IUPAC10', 'BRNO2_IUPAC10   ', 'BR2_IUPAC10     ', & 
     &   'MB3_IUPAC10     ', 'MB2C_BLIDE98    ', 'MBC2_BLIDE98    '/

      INTEGER, PARAMETER :: NHETERO =  11
      CHARACTER( 16 )    :: HETERO( NHETERO )

      DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & 
     &   'HETERO_NTR2     ', 'HETERO_N2O5IJ   ', 'HETERO_N2O5K    ', &
     &   'HETERO_H2NO3PAIJ', 'HETERO_H2NO3PAK ', 'HETERO_H2NO3PBIJ', &
     &   'HETERO_H2NO3PBK ', 'HETERO_NO2      ', 'HETERO_IEPOX    ', &
     &   'HETERO_BRONO2IJ ', 'HETERO_BRONO2K  '/

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
     &    'R30a            ', 'R31             ', 'R32             ', & ! 0   
     &    'R33             ', 'R34             ', 'R35             ', & ! 1   
     &    'R36             ', 'R37             ', 'R38             ', & ! 2   
     &    'R39             ', 'R40             ', 'R41             ', & ! 3   
     &    'R42             ', 'R43             ', 'R44             ', & ! 4   
     &    'R45             ', 'R46             ', 'R47             ', & ! 5   
     &    'R48             ', 'R49             ', 'R50             ', & ! 6   
     &    'R51             ', 'R52             ', 'R53             ', & ! 7   
     &    'R54             ', 'R55             ', 'R56             ', & ! 8   
     &    'R57             ', 'R58             ', 'R59             ', & ! 9   
     &    'R60             ', 'R63             ', 'R64             ', & ! 0   
     &    'R64a            ', 'R64b            ', 'R64c            ', & ! 1   
     &    'R64d            ', 'R64e            ', 'R65             ', & ! 2   
     &    'R66             ', 'R67             ', 'R68             ', & ! 3   
     &    'R69             ', 'R70             ', 'R71             ', & ! 4   
     &    'R72             ', 'R73             ', 'R74             ', & ! 5   
     &    'R75             ', 'R76             ', 'R77             ', & ! 6   
     &    'R78             ', 'R79             ', 'R80             ', & ! 7   
     &    'R81             ', 'R82             ', 'R83             ', & ! 8   
     &    'R84             ', 'R85             ', 'R86             ', & ! 9   
     &    'R87             ', 'R88             ', 'R89             ', & ! 0   
     &    'R90             ', 'R91             ', 'R92             ', & ! 1   
     &    'R93             ', 'R94             ', 'R95             ', & ! 2   
     &    'R96             ', 'R97             ', 'R98             ', & ! 3   
     &    'R99             ', 'R100            ', 'R101            ', & ! 4   
     &    'R102            ', 'R103            ', 'R104            ', & ! 5   
     &    'R105            ', 'R106            ', 'R107            ', & ! 6   
     &    'R108            ', 'R109            ', 'R110            ', & ! 7   
     &    'R111            ', 'R112            ', 'R113            ', & ! 8   
     &    'R114            ', 'R115            ', 'R116            ', & ! 9   
     &    'R117            ', 'R118            ', 'R119            ', & ! 0   
     &    'R120            ', 'R121            ', 'R122            ', & ! 1   
     &    'R123            ', 'R124            ', 'R125            ', & ! 2   
     &    'R126            ', 'R127            ', 'R128            ', & ! 3   
     &    'R129            ', 'R130            ', 'R131            ', & ! 4   
     &    'R132            ', 'R133            ', 'R134            ', & ! 5   
     &    'R135            ', 'R136            ', 'R137            ', & ! 6   
     &    'R138            ', 'R139            ', 'R140            ', & ! 7   
     &    'R141            ', 'R142            ', 'R143            ', & ! 8   
     &    'R144            ', 'R145            ', 'R146            ', & ! 9   
     &    'R147            ', 'R148            ', 'R149            ', & ! 0   
     &    'R150            ', 'R151            ', 'R152            ', & ! 1   
     &    'R153            ', 'R154a           ', 'R154b           ', & ! 2   
     &    'R155            ', 'R156            ', 'R157            ', & ! 3   
     &    'R158            ', 'R158a           ', 'R158b           ', & ! 4   
     &    'R158c           ', 'R159            ', 'R160            ', & ! 5   
     &    'R161            ', 'R162            ', 'R163            ', & ! 6   
     &    'R164            ', 'R165            ', 'R166            ', & ! 7   
     &    'R166a           ', 'R166b           ', 'R166c           ', & ! 8   
     &    'R166d           ', 'R167            ', 'R168            ', & ! 9   
     &    'R169            ', 'R170            ', 'R171            ', & ! 0   
     &    'R172            ', 'CL1             ', 'CL2             ', & ! 1   
     &    'CL3             ', 'CL4             ', 'CL5             ', & ! 2   
     &    'CL6             ', 'CL7             ', 'CL8             ', & ! 3   
     &    'CL9             ', 'CL10            ', 'CL11            ', & ! 4   
     &    'CL12            ', 'CL13            ', 'CL14            ', & ! 5   
     &    'CL15            ', 'CL16            ', 'CL17            ', & ! 6   
     &    'CL18            ', 'CL19            ', 'CL20            ', & ! 7   
     &    'CL21            ', 'CL22            ', 'CL23a           ', & ! 8   
     &    'CL23b           ', 'CL24            ', 'CL25            ', & ! 9   
     &    'SA01            ', 'SA02            ', 'SA03            ', & ! 0   
     &    'SA04            ', 'SA05            ', 'SA06            ', & ! 1   
     &    'SA07            ', 'SA08            ', 'SA09            ', & ! 2   
     &    'SA10            ', 'SA11            ', 'SA12            ', & ! 3   
     &    'SA13            ', 'R63M            ', 'R64M            ', & ! 4   
     &    'R65M            ', 'R66M            ', 'R67M            ', & ! 5   
     &    'R68M            ', 'R69M            ', 'R70M            ', & ! 6   
     &    'R71M            ', 'R72M            ', 'N08             ', & ! 7   
     &    'N08b            ', 'N08c            ', 'N09             ', & ! 8   
     &    'N09b            ', 'N09c            ', 'N10             ', & ! 9   
     &    'N10b            ', 'N10c            ', 'N11             ', & ! 0   
     &    'N11b            ', 'N11c            ', 'N14             ', & ! 1   
     &    'N15             ', 'N15b            ', 'N15c            ', & ! 2   
     &    'N16             ', 'N16b            ', 'N16c            ', & ! 3   
     &    'N17             ', 'N18b            ', 'N19             ', & ! 4   
     &    'N20             ', 'N21             ', 'N22             ', & ! 5   
     &    'N25             ', 'HET_NT1         ', 'HET_NT2         ', & ! 6   
     &    'HET_NT3         ', 'HET_NT4         ', 'HET_NT5         ', & ! 7   
     &    'HET_NT6         ', 'HET_NT7         ', 'HET_N2O5IJ      ', & ! 8   
     &    'HET_N2O5K       ', 'HET_H2NO3PIJA   ', 'HET_H2NO3PKA    ', & ! 9   
     &    'HET_H2NO3PIB    ', 'HET_H2NO3PJB    ', 'HET_H2NO3PKB    ', & ! 0   
     &    'HET_N02         ', 'HET_IEPOX       ', 'OLIG_XYLENE1    ', & ! 1   
     &    'OLIG_XYLENE2    ', 'OLIG_TOLUENE1   ', 'OLIG_TOLUENE2   ', & ! 2   
     &    'OLIG_BENZENE1   ', 'OLIG_BENZENE2   ', 'OLIG_TERPENE1   ', & ! 3   
     &    'OLIG_TERPENE2   ', 'OLIG_ISOPRENE1  ', 'OLIG_ISOPRENE2  ', & ! 4   
     &    'OLIG_SESQT1     ', 'OLIG_PAH1       ', 'OLIG_PAH2       ', & ! 5   
     &    'OLIG_ALK1       ', 'OLIG_ALK2       ', 'PCSOA           ', & ! 6   
     &    'POA_AGE1        ', 'POA_AGE2        ', 'POA_AGE3        ', & ! 7   
     &    'POA_AGE4        ', 'POA_AGE5        ', 'POA_AGE6        ', & ! 8   
     &    'POA_AGE7        ', 'POA_AGE8        ', 'POA_AGE9        ', & ! 9   
     &    'POA_AGE10       ', 'T01             ', 'T02             ', & ! 0   
     &    'T03             ', 'T04             ', 'T05             ', & ! 1   
     &    'TCL1            ', 'T06             ', 'T07             ', & ! 2   
     &    'T08             ', 'T09             ', 'TCL2            ', & ! 3   
     &    'T10             ', 'T11             ', 'T12             ', & ! 4   
     &    'TCL3            ', 'T14             ', 'T15             ', & ! 5   
     &    'T16             ', 'T17             ', 'TCL4            ', & ! 6   
     &    'T18             ', 'T19             ', 'T20             ', & ! 7   
     &    'T21             ', 'TCL5            ', 'T22             ', & ! 8   
     &    'TCL6            ', 'T23             ', 'TCL7            ', & ! 9   
     &    'T24             ', 'TCL8            ', 'T25             ', & ! 0   
     &    'TCL9            ', 'T26             ', 'T27             ', & ! 1   
     &    'T28             ', 'T29             ', 'TCL10           ', & ! 2   
     &    'T30             ', 'T31             ', 'T32             ', & ! 3   
     &    'T33             ', 'TCL11           ', 'HG1             ', & ! 4   
     &    'HG2             ', 'HG3             ', 'HG4             ', & ! 5   
     &    'HG5             ', 'IO01            ', 'IO02            ', & ! 6   
     &    'IO03            ', 'IO04            ', 'IO05            ', & ! 7   
     &    'IO06            ', 'IO07            ', 'IO08            ', & ! 8   
     &    'IO09            ', 'IO10            ', 'IO11            ', & ! 9   
     &    'IO12            ', 'IO13            ', 'IO14            ', & ! 0   
     &    'IO15            ', 'IO16            ', 'IO17            ', & ! 1   
     &    'IO18            ', 'IO19            ', 'IO20            ', & ! 2   
     &    'IO21            ', 'IO22            ', 'IO23            ', & ! 3   
     &    'IO24            ', 'IO25            ', 'IO26            ', & ! 4   
     &    'IO27            ', 'IO28            ', 'IO29            ', & ! 5   
     &    'IO30            ', 'IO31            ', 'IO32            ', & ! 6   
     &    'IO33            ', 'IO34            ', 'IO35            ', & ! 7   
     &    'IO36            ', 'IO37            ', 'IO38            ', & ! 8   
     &    'IO39            ', 'IO40            ', 'IO41            ', & ! 9   
     &    'IO42            ', 'IO43            ', 'IO44            ', & ! 0   
     &    'IO45            ', 'IO46            ', 'IO47            ', & ! 1   
     &    'IO48            ', 'IO49            ', 'IO50            ', & ! 2   
     &    'IO51            ', 'IO52            ', 'IO53            ', & ! 3   
     &    'BR01            ', 'BR02            ', 'BR03            ', & ! 4   
     &    'BR04            ', 'BR05            ', 'BR06            ', & ! 5   
     &    'BR07            ', 'BR08            ', 'BR09            ', & ! 6   
     &    'BR10            ', 'BR11            ', 'BR12            ', & ! 7   
     &    'BR13            ', 'BR14            ', 'BR15            ', & ! 8   
     &    'BR16            ', 'BR17            ', 'BR18            ', & ! 9   
     &    'BR19            ', 'BR20            ', 'BR21            ', & ! 0   
     &    'BR22            ', 'BR23            ', 'BR24            ', & ! 1   
     &    'BR25            ', 'BR26            ', 'BR27            ', & ! 2   
     &    'BR28            ', 'BR29            ', 'BR30            ', & ! 3   
     &    'BR31            ', 'BR32            ', 'BR33            ', & ! 4   
     &    'BR34            ', 'BR35            ', 'BR36            ', & ! 5   
     &    'BR37            ', 'BR38            ', 'BR39            ', & ! 6   
     &    'HET_BRONO2IJ    ', 'HET_BRONO2K     '/                   !    

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
       INTEGER, PARAMETER  :: IJ_NO3NO2_06        =   4
       INTEGER, PARAMETER  :: IJ_NO3NO_06         =   5
       INTEGER, PARAMETER  :: IJ_HONO_IUPAC10     =   6
       INTEGER, PARAMETER  :: IJ_H2O2_IUPAC10     =   7
       INTEGER, PARAMETER  :: IJ_PNA_IUPAC10      =   8
       INTEGER, PARAMETER  :: IJ_HNO3_IUPAC10     =   9
       INTEGER, PARAMETER  :: IJ_N2O5_IUPAC10     =  10
       INTEGER, PARAMETER  :: IJ_MEPX_IUPAC10     =  11
       INTEGER, PARAMETER  :: IJ_FORM_R_IUPAC10   =  12
       INTEGER, PARAMETER  :: IJ_FORM_M_IUPAC10   =  13
       INTEGER, PARAMETER  :: IJ_ALD2_R_IUPAC10   =  14
       INTEGER, PARAMETER  :: IJ_PAN_IUPAC10      =  15
       INTEGER, PARAMETER  :: IJ_PACD_CB05        =  16
       INTEGER, PARAMETER  :: IJ_ALDX_R_IUPAC10   =  17
       INTEGER, PARAMETER  :: IJ_MGLY_IUPAC10     =  18
       INTEGER, PARAMETER  :: IJ_ACRO_09          =  19
       INTEGER, PARAMETER  :: IJ_CL2_IUPAC04      =  20
       INTEGER, PARAMETER  :: IJ_HOCL_IUPAC04     =  21
       INTEGER, PARAMETER  :: IJ_FMCL_IUPAC04     =  22
       INTEGER, PARAMETER  :: IJ_CLNO2            =  23
       INTEGER, PARAMETER  :: IJ_NTR_IUPAC10      =  24
       INTEGER, PARAMETER  :: IJ_NOA_14           =  25
       INTEGER, PARAMETER  :: IJ_NBO_14           =  26
       INTEGER, PARAMETER  :: IJ_CH3I_IUPAC10     =  27
       INTEGER, PARAMETER  :: IJ_MI2_IUPAC10      =  28
       INTEGER, PARAMETER  :: IJ_MIB_IUPAC10      =  29
       INTEGER, PARAMETER  :: IJ_MIC_IUPAC10      =  30
       INTEGER, PARAMETER  :: IJ_I2_IUPAC10       =  31
       INTEGER, PARAMETER  :: IJ_IO_IUPAC10       =  32
       INTEGER, PARAMETER  :: IJ_OIO_06           =  33
       INTEGER, PARAMETER  :: IJ_INO_06           =  34
       INTEGER, PARAMETER  :: IJ_INO2_06          =  35
       INTEGER, PARAMETER  :: IJ_IONO2_06         =  36
       INTEGER, PARAMETER  :: IJ_HOI_IUPAC10      =  37
       INTEGER, PARAMETER  :: IJ_ICL_IUPAC10      =  38
       INTEGER, PARAMETER  :: IJ_BRCL_IUPAC10     =  39
       INTEGER, PARAMETER  :: IJ_BRO_IUPAC10      =  40
       INTEGER, PARAMETER  :: IJ_HOBR_IUPAC10     =  41
       INTEGER, PARAMETER  :: IJ_BRONO2_M_IUPAC10 =  42
       INTEGER, PARAMETER  :: IJ_BRONO2_R_IUPAC10 =  43
       INTEGER, PARAMETER  :: IJ_BRNO2_IUPAC10    =  44
       INTEGER, PARAMETER  :: IJ_BR2_IUPAC10      =  45
       INTEGER, PARAMETER  :: IJ_MB3_IUPAC10      =  46
       INTEGER, PARAMETER  :: IJ_MB2C_BLIDE98     =  47
       INTEGER, PARAMETER  :: IJ_MBC2_BLIDE98     =  48
       INTEGER, PARAMETER  :: IK_HETERO_NTR2      =   1
       INTEGER, PARAMETER  :: IK_HETERO_N2O5IJ    =   2
       INTEGER, PARAMETER  :: IK_HETERO_N2O5K     =   3
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAIJ =   4
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAK  =   5
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBIJ =   6
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBK  =   7
       INTEGER, PARAMETER  :: IK_HETERO_NO2       =   8
       INTEGER, PARAMETER  :: IK_HETERO_IEPOX     =   9
       INTEGER, PARAMETER  :: IK_HETERO_BRONO2IJ  =  10
       INTEGER, PARAMETER  :: IK_HETERO_BRONO2K   =  11
       END MODULE RXNS_DATA
