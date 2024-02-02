       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: /home/bmurphy/cmaq_projects/bicicle_1.6/UTIL/chemmech/input/cb6r5m_ae7_aq/mech_cb6r5m_ae7_aq.def
! for Mechanism Name: CB6R5M_AE7_AQ                   

! This file is used to create mechanism data and functions

! The following are reserved symbols declared in this file:
!    MECHNAME        = Mechanism name
!    N_GAS_CHEM_SPC  = Total number of gas species in chemical mechanism
!    NUMB_MECH_SPC   = Total number of species in chemical mechanism
!    N_ACT_SP        = Number of active (determined by ODE solver) species in mechanism
!    GAS_CHEM_SPC    = Names of gas species in chemical mechanism
!    CHEMISTRY_SPC   = Names of species in chemical mechanism
!    CGRID_INDEX     = CGRID Index of species in chemical mechanism
!    SPECIES_TYPE    = Group or type of species 
!    SPECIES_MOLWT   = Molecular Weight of species (gm/mole)
!    NRXNS           = Number of mechanism reactions
!    ZERO_REACT_REACTIONS  = number zero reactant reactions
!    ONE_REACT_REACTIONS   = number one reactant reactions
!    TWO_REACT_REACTIONS   = number second order reactions
!    THREE_REACT_REACTIONS = number three reactant reactions
!    NSUNLIGHT_RXNS  = Number of mechanism reactions requiring sunlight
!    NTHERMAL_RXNS   = Number of mechanism reactions not requiring sunlight
!    KUNITS          = Units of mechanism reactions
!    KTYPE           = Reaction type
!    IRXBITS         = Bit test mask vector for selected reactions
!    IORDER          = Order of the reaction
!    NTERMS_JACOB    = Maximum number of nonzero terms in day/night Jacobian
!    MSTEPS_JACOB    = Maximum number of LU Decomposition steps to solve each Jacobian
!    KTN1            = Number of type 1 reactions
!    KRX1            = Reactions list pointer to type 1 reactions
!    KTN2            = Number of type 2 reactions
!    KRX2            = Reactions list pointer to type 2 reactions
!    KTN3            = Number of type 3 reactions
!    KRX3            = Reactions list pointer to type 3 reactions
!    KTN4            = Number of type 4 reactions
!    KRX4            = Reactions list pointer to type 4 reactions
!    KTN5            = Number of type 5 reactions
!    KRX5            = Reactions list pointer to type 5 reactions
!    KTN6            = Number of type 6 reactions
!    KRX6            = Reactions list pointer to type 6 reactions
!    KTN7            = Number of type 7 reactions
!    KRX7            = Reactions list pointer to type 7 reactions

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

      CHARACTER( 32 ), PARAMETER :: MECHNAME = 'CB6R5M_AE7_AQ'

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC = 161
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 182

      CHARACTER( 16 ) :: GAS_CHEM_SPC( N_GAS_CHEM_SPC )
      CHARACTER( 16 ) :: CHEMISTRY_SPC( NUMB_MECH_SPC )
      CHARACTER( 16 ) :: SPECIES_TYPE(  NUMB_MECH_SPC )
      INTEGER         :: CGRID_INDEX (  NUMB_MECH_SPC )
      INTEGER         :: TYPE_INDEX  (  NUMB_MECH_SPC )
      REAL( 8 )       :: SPECIES_MOLWT( NUMB_MECH_SPC )
      LOGICAL         :: CONVERT_CONC(  NUMB_MECH_SPC )

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
      DATA GAS_CHEM_SPC( 105 ) / 'IEPOXP          ' /
      DATA GAS_CHEM_SPC( 106 ) / 'MTNO3           ' /
      DATA GAS_CHEM_SPC( 107 ) / 'PCVOC           ' /
      DATA GAS_CHEM_SPC( 108 ) / 'PCSOARXN        ' /
      DATA GAS_CHEM_SPC( 109 ) / 'VLVPO1          ' /
      DATA GAS_CHEM_SPC( 110 ) / 'VSVPO1          ' /
      DATA GAS_CHEM_SPC( 111 ) / 'VSVPO2          ' /
      DATA GAS_CHEM_SPC( 112 ) / 'VSVPO3          ' /
      DATA GAS_CHEM_SPC( 113 ) / 'VIVPO1          ' /
      DATA GAS_CHEM_SPC( 114 ) / 'VLVOO1          ' /
      DATA GAS_CHEM_SPC( 115 ) / 'VLVOO2          ' /
      DATA GAS_CHEM_SPC( 116 ) / 'VSVOO2          ' /
      DATA GAS_CHEM_SPC( 117 ) / 'VSVOO3          ' /
      DATA GAS_CHEM_SPC( 118 ) / 'VSVOO1          ' /
      DATA GAS_CHEM_SPC( 119 ) / 'FORM_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 120 ) / 'ALD2_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 121 ) / 'BUTADIENE13     ' /
      DATA GAS_CHEM_SPC( 122 ) / 'ACROLEIN        ' /
      DATA GAS_CHEM_SPC( 123 ) / 'ACRO_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 124 ) / 'TOLU            ' /
      DATA GAS_CHEM_SPC( 125 ) / 'HG              ' /
      DATA GAS_CHEM_SPC( 126 ) / 'HGIIAER         ' /
      DATA GAS_CHEM_SPC( 127 ) / 'HGIIGAS         ' /
      DATA GAS_CHEM_SPC( 128 ) / 'BR              ' /
      DATA GAS_CHEM_SPC( 129 ) / 'BRO             ' /
      DATA GAS_CHEM_SPC( 130 ) / 'HOBR            ' /
      DATA GAS_CHEM_SPC( 131 ) / 'HBR             ' /
      DATA GAS_CHEM_SPC( 132 ) / 'BR2             ' /
      DATA GAS_CHEM_SPC( 133 ) / 'BRNO3           ' /
      DATA GAS_CHEM_SPC( 134 ) / 'BRNO2           ' /
      DATA GAS_CHEM_SPC( 135 ) / 'FMBR            ' /
      DATA GAS_CHEM_SPC( 136 ) / 'MB3             ' /
      DATA GAS_CHEM_SPC( 137 ) / 'MB2             ' /
      DATA GAS_CHEM_SPC( 138 ) / 'MB2C            ' /
      DATA GAS_CHEM_SPC( 139 ) / 'MBC2            ' /
      DATA GAS_CHEM_SPC( 140 ) / 'MBC             ' /
      DATA GAS_CHEM_SPC( 141 ) / 'DMS             ' /
      DATA GAS_CHEM_SPC( 142 ) / 'MSA             ' /
      DATA GAS_CHEM_SPC( 143 ) / 'BRCL            ' /
      DATA GAS_CHEM_SPC( 144 ) / 'I               ' /
      DATA GAS_CHEM_SPC( 145 ) / 'IO              ' /
      DATA GAS_CHEM_SPC( 146 ) / 'HI              ' /
      DATA GAS_CHEM_SPC( 147 ) / 'I2              ' /
      DATA GAS_CHEM_SPC( 148 ) / 'HOI             ' /
      DATA GAS_CHEM_SPC( 149 ) / 'INO             ' /
      DATA GAS_CHEM_SPC( 150 ) / 'INO2            ' /
      DATA GAS_CHEM_SPC( 151 ) / 'INO3            ' /
      DATA GAS_CHEM_SPC( 152 ) / 'OIO             ' /
      DATA GAS_CHEM_SPC( 153 ) / 'I2O4            ' /
      DATA GAS_CHEM_SPC( 154 ) / 'I2O2            ' /
      DATA GAS_CHEM_SPC( 155 ) / 'I2O3            ' /
      DATA GAS_CHEM_SPC( 156 ) / 'CH3I            ' /
      DATA GAS_CHEM_SPC( 157 ) / 'ICL             ' /
      DATA GAS_CHEM_SPC( 158 ) / 'IBR             ' /
      DATA GAS_CHEM_SPC( 159 ) / 'MI2             ' /
      DATA GAS_CHEM_SPC( 160 ) / 'MIB             ' /
      DATA GAS_CHEM_SPC( 161 ) / 'MIC             ' /




      LOGICAL   :: HALOGEN_PARMAETER = .FALSE. 


! The below character and integer arrays list the model species names used in the 
! chemical mechanism. The gas species and their order should agree with 
! the GC_SPC array for the gas phase chemistry to work correctly. 
! If present, the CHEMISTRY_SPC names and species type should agree with the CGRID_SPCS module


! MAPPED_TO_CGRID declares whether CMAQ namelists were used to determine 
! the below values of CGRID_INDEX, SPECIES_TYPE, SPECIES_MOLWT, and CONVERT_CONC
      LOGICAL, PARAMETER, PRIVATE :: F = .FALSE.
      LOGICAL, PARAMETER, PRIVATE :: T = .TRUE.


      LOGICAL   :: MAPPED_TO_CGRID   = .FALSE. 

      TYPE MEMBER
         CHARACTER( 16 ) :: CHEMISTRY_SPC
         INTEGER         :: CGRID_INDEX
         CHARACTER(  2 ) :: SPECIES_TYPE
         REAL( 8 )       :: SPECIES_MOLWT
         LOGICAL         :: CONVERT_CONC
      END TYPE MEMBER
      TYPE( MEMBER ) ::  SPECIES_LIST( NUMB_MECH_SPC ) = (/ &
      & MEMBER("NO2             ",    1, "GC",   46.00D0, F), &
      & MEMBER("NO              ",    2, "GC",   30.00D0, F), &
      & MEMBER("O               ",    3, "GC",   16.00D0, F), &
      & MEMBER("O3              ",    4, "GC",   48.00D0, F), &
      & MEMBER("NO3             ",    5, "GC",   62.00D0, F), &
      & MEMBER("O1D             ",    6, "GC",   16.00D0, F), &
      & MEMBER("OH              ",    7, "GC",   17.00D0, F), &
      & MEMBER("HO2             ",    8, "GC",   33.00D0, F), &
      & MEMBER("H2O2            ",    9, "GC",   34.00D0, F), &
      & MEMBER("N2O5            ",   10, "GC",  108.00D0, F), &
      & MEMBER("HNO3            ",   11, "GC",   63.00D0, F), &
      & MEMBER("HONO            ",   12, "GC",   47.00D0, F), &
      & MEMBER("PNA             ",   13, "GC",   79.00D0, F), &
      & MEMBER("SO2             ",   14, "GC",   64.00D0, F), &
      & MEMBER("SULF            ",   15, "GC",   98.00D0, F), &
      & MEMBER("SULRXN          ",   16, "GC",   98.00D0, F), &
      & MEMBER("C2O3            ",   17, "GC",   75.00D0, F), &
      & MEMBER("MEO2            ",   18, "GC",   47.00D0, F), &
      & MEMBER("RO2             ",   19, "GC",   87.10D0, F), &
      & MEMBER("PAN             ",   20, "GC",  121.00D0, F), &
      & MEMBER("PACD            ",   21, "GC",   76.00D0, F), &
      & MEMBER("AACD            ",   22, "GC",   60.00D0, F), &
      & MEMBER("CXO3            ",   23, "GC",   89.00D0, F), &
      & MEMBER("ALD2            ",   24, "GC",   44.00D0, F), &
      & MEMBER("XO2H            ",   25, "GC",   87.10D0, F), &
      & MEMBER("PANX            ",   26, "GC",  135.00D0, F), &
      & MEMBER("FORM            ",   27, "GC",   30.00D0, F), &
      & MEMBER("MEPX            ",   28, "GC",   48.00D0, F), &
      & MEMBER("MEOH            ",   29, "GC",   32.00D0, F), &
      & MEMBER("ROOH            ",   30, "GC",   90.10D0, F), &
      & MEMBER("XO2             ",   31, "GC",   87.10D0, F), &
      & MEMBER("XO2N            ",   32, "GC",   87.10D0, F), &
      & MEMBER("NTR1            ",   35, "GC",  119.10D0, F), &
      & MEMBER("NTR2            ",   36, "GC",  135.10D0, F), &
      & MEMBER("FACD            ",   37, "GC",   46.00D0, F), &
      & MEMBER("CO              ",   38, "GC",   28.00D0, F), &
      & MEMBER("HCO3            ",   39, "GC",   63.00D0, F), &
      & MEMBER("ALDX            ",   40, "GC",   58.10D0, F), &
      & MEMBER("GLYD            ",   41, "GC",   60.00D0, F), &
      & MEMBER("GLY             ",   42, "GC",   58.00D0, F), &
      & MEMBER("MGLY            ",   43, "GC",   72.00D0, F), &
      & MEMBER("ETHA            ",   44, "GC",   30.10D0, F), &
      & MEMBER("ETOH            ",   45, "GC",   46.10D0, F), &
      & MEMBER("KET             ",   46, "GC",   72.10D0, F), &
      & MEMBER("PAR             ",   47, "GC",   14.00D0, F), &
      & MEMBER("ACET            ",   48, "GC",   58.10D0, F), &
      & MEMBER("PRPA            ",   49, "GC",   44.10D0, F), &
      & MEMBER("XPRP            ",   34, "GC",   89.10D0, F), &
      & MEMBER("XPAR            ",   33, "GC",   45.00D0, F), &
      & MEMBER("ROR             ",   50, "GC",   29.00D0, F), &
      & MEMBER("ETHY            ",   51, "GC",   26.00D0, F), &
      & MEMBER("ETH             ",   52, "GC",   28.00D0, F), &
      & MEMBER("OLE             ",   53, "GC",   42.10D0, F), &
      & MEMBER("IOLE            ",   54, "GC",   56.10D0, F), &
      & MEMBER("ISOP            ",   55, "GC",   68.10D0, F), &
      & MEMBER("ISO2            ",   56, "GC",  117.10D0, F), &
      & MEMBER("ISOPRXN         ",   57, "GC",   68.10D0, F), &
      & MEMBER("ISPD            ",   58, "GC",   70.10D0, F), &
      & MEMBER("INTR            ",   59, "GC",  147.10D0, F), &
      & MEMBER("ISPX            ",   60, "GC",  118.10D0, F), &
      & MEMBER("HPLD            ",   61, "GC",  116.10D0, F), &
      & MEMBER("OPO3            ",   62, "GC",  115.00D0, F), &
      & MEMBER("EPOX            ",   63, "GC",  118.10D0, F), &
      & MEMBER("EPX2            ",   65, "GC",  149.10D0, F), &
      & MEMBER("TERP            ",   66, "GC",  136.20D0, F), &
      & MEMBER("TRPRXN          ",   70, "GC",  136.20D0, F), &
      & MEMBER("TERPNRO2        ",   68, "GC",  197.00D0, F), &
      & MEMBER("APIN            ",   67, "GC",  136.20D0, F), &
      & MEMBER("BENZENE         ",   71, "GC",   78.10D0, F), &
      & MEMBER("CRES            ",   72, "GC",  108.10D0, F), &
      & MEMBER("BZO2            ",   73, "GC",  159.10D0, F), &
      & MEMBER("OPEN            ",   74, "GC",   84.00D0, F), &
      & MEMBER("BENZRO2         ",   75, "GC",  127.00D0, F), &
      & MEMBER("TOL             ",   76, "GC",   92.10D0, F), &
      & MEMBER("TO2             ",   77, "GC",  173.10D0, F), &
      & MEMBER("TOLRO2          ",   78, "GC",  141.00D0, F), &
      & MEMBER("XOPN            ",   79, "GC",   98.10D0, F), &
      & MEMBER("XYLMN           ",   80, "GC",  106.20D0, F), &
      & MEMBER("XLO2            ",   81, "GC",  187.10D0, F), &
      & MEMBER("XYLRO2          ",   82, "GC",  155.00D0, F), &
      & MEMBER("NAPH            ",   83, "GC",  128.20D0, F), &
      & MEMBER("PAHRO2          ",   84, "GC",  187.20D0, F), &
      & MEMBER("CRO             ",   85, "GC",  107.10D0, F), &
      & MEMBER("CAT1            ",   86, "GC",  124.10D0, F), &
      & MEMBER("CRON            ",   87, "GC",  153.10D0, F), &
      & MEMBER("OPAN            ",   88, "GC",  161.00D0, F), &
      & MEMBER("ECH4            ",   89, "GC",   16.00D0, F), &
      & MEMBER("CL2             ",   90, "GC",   71.00D0, F), &
      & MEMBER("CL              ",   91, "GC",   35.50D0, F), &
      & MEMBER("HOCL            ",   92, "GC",   52.50D0, F), &
      & MEMBER("CLO             ",   93, "GC",   51.50D0, F), &
      & MEMBER("FMCL            ",   94, "GC",   64.50D0, F), &
      & MEMBER("HCL             ",   95, "GC",   36.50D0, F), &
      & MEMBER("CLNO2           ",   96, "GC",   81.50D0, F), &
      & MEMBER("CLNO3           ",   97, "GC",   97.50D0, F), &
      & MEMBER("SVAVB2          ",  125, "GC",  179.00D0, F), &
      & MEMBER("SVAVB3          ",  126, "GC",  169.00D0, F), &
      & MEMBER("SVAVB4          ",  127, "GC",  158.00D0, F), &
      & MEMBER("SVAVB1          ",  124, "GC",  198.00D0, F), &
      & MEMBER("SESQ            ",   98, "GC",  204.00D0, F), &
      & MEMBER("SESQRXN         ",   99, "GC",  204.00D0, F), &
      & MEMBER("SOAALK          ",  100, "GC",  112.00D0, F), &
      & MEMBER("H2NO3PIJ        ",  101, "GC",   64.00D0, F), &
      & MEMBER("H2NO3PK         ",  102, "GC",   64.00D0, F), &
      & MEMBER("ACLI            ",  174, "AE",   35.50D0, T), &
      & MEMBER("ACLJ            ",  175, "AE",   35.50D0, T), &
      & MEMBER("ACLK            ",  176, "AE",   35.50D0, T), &
      & MEMBER("IEPOXP          ",   64, "GC",  118.10D0, F), &
      & MEMBER("ASO4J           ",  164, "AE",   96.00D0, T), &
      & MEMBER("AISO3J          ",  209, "AE",  168.20D0, T), &
      & MEMBER("AGLYJ           ",  212, "AE",   66.40D0, T), &
      & MEMBER("MTNO3           ",   69, "GC",  231.00D0, F), &
      & MEMBER("AMTNO3J         ",  213, "AE",  231.00D0, T), &
      & MEMBER("AMTHYDJ         ",  214, "AE",  168.00D0, T), &
      & MEMBER("AAVB2J          ",  238, "AE",  179.00D0, T), &
      & MEMBER("AOLGAJ          ",  210, "AE",  206.00D0, T), &
      & MEMBER("AAVB3J          ",  239, "AE",  169.00D0, T), &
      & MEMBER("AAVB4J          ",  240, "AE",  158.00D0, T), &
      & MEMBER("AISO1J          ",  177, "AE",  132.00D0, T), &
      & MEMBER("AOLGBJ          ",  211, "AE",  248.00D0, T), &
      & MEMBER("AISO2J          ",  178, "AE",  133.00D0, T), &
      & MEMBER("ASQTJ           ",  179, "AE",  273.00D0, T), &
      & MEMBER("APOCI           ",  215, "AE",  220.00D0, T), &
      & MEMBER("APNCOMI         ",  217, "AE",  220.00D0, T), &
      & MEMBER("APOCJ           ",  216, "AE",  220.00D0, T), &
      & MEMBER("APNCOMJ         ",  218, "AE",  220.00D0, T), &
      & MEMBER("PCVOC           ",  113, "GC",  170.00D0, F), &
      & MEMBER("PCSOARXN        ",  114, "GC",  170.00D0, F), &
      & MEMBER("VLVPO1          ",  103, "GC",  218.00D0, F), &
      & MEMBER("VSVPO1          ",  104, "GC",  230.00D0, F), &
      & MEMBER("VSVPO2          ",  105, "GC",  241.00D0, F), &
      & MEMBER("VSVPO3          ",  106, "GC",  253.00D0, F), &
      & MEMBER("VIVPO1          ",  107, "GC",  266.00D0, F), &
      & MEMBER("VLVOO1          ",  108, "GC",  136.00D0, F), &
      & MEMBER("VLVOO2          ",  109, "GC",  136.00D0, F), &
      & MEMBER("VSVOO2          ",  111, "GC",  135.00D0, F), &
      & MEMBER("VSVOO3          ",  112, "GC",  134.00D0, F), &
      & MEMBER("VSVOO1          ",  110, "GC",  135.00D0, F), &
      & MEMBER("FORM_PRIMARY    ",  115, "GC",   30.00D0, F), &
      & MEMBER("ALD2_PRIMARY    ",  116, "GC",   44.00D0, F), &
      & MEMBER("BUTADIENE13     ",  117, "GC",   54.00D0, F), &
      & MEMBER("ACROLEIN        ",  118, "GC",   56.10D0, F), &
      & MEMBER("ACRO_PRIMARY    ",  119, "GC",   56.10D0, F), &
      & MEMBER("TOLU            ",  120, "GC",   92.00D0, F), &
      & MEMBER("HG              ",  121, "GC",  200.60D0, F), &
      & MEMBER("HGIIAER         ",  122, "GC",  200.60D0, F), &
      & MEMBER("HGIIGAS         ",  123, "GC",  271.50D0, F), &
      & MEMBER("BR              ",  128, "GC",   79.90D0, F), &
      & MEMBER("BRO             ",  129, "GC",   95.90D0, F), &
      & MEMBER("HOBR            ",  130, "GC",   96.90D0, F), &
      & MEMBER("HBR             ",  131, "GC",   80.90D0, F), &
      & MEMBER("BR2             ",  132, "GC",  159.80D0, F), &
      & MEMBER("BRNO3           ",  133, "GC",  141.90D0, F), &
      & MEMBER("BRNO2           ",  134, "GC",  125.90D0, F), &
      & MEMBER("FMBR            ",  135, "GC",  108.90D0, F), &
      & MEMBER("MB3             ",  136, "GC",  252.70D0, F), &
      & MEMBER("MB2             ",  137, "GC",  173.80D0, F), &
      & MEMBER("MB2C            ",  138, "GC",  208.30D0, F), &
      & MEMBER("MBC2            ",  139, "GC",  243.80D0, F), &
      & MEMBER("MBC             ",  140, "GC",  129.40D0, F), &
      & MEMBER("DMS             ",  141, "GC",   62.00D0, F), &
      & MEMBER("MSA             ",  142, "GC",   96.00D0, F), &
      & MEMBER("BRCL            ",  143, "GC",  115.40D0, F), &
      & MEMBER("ABRJ            ",  247, "AE",   79.90D0, T), &
      & MEMBER("I               ",  144, "GC",  126.90D0, F), &
      & MEMBER("IO              ",  145, "GC",  142.90D0, F), &
      & MEMBER("HI              ",  146, "GC",  127.90D0, F), &
      & MEMBER("I2              ",  147, "GC",  253.80D0, F), &
      & MEMBER("HOI             ",  148, "GC",  143.90D0, F), &
      & MEMBER("INO             ",  149, "GC",  156.90D0, F), &
      & MEMBER("INO2            ",  150, "GC",  172.90D0, F), &
      & MEMBER("INO3            ",  151, "GC",  188.90D0, F), &
      & MEMBER("OIO             ",  152, "GC",  158.90D0, F), &
      & MEMBER("I2O4            ",  153, "GC",  317.80D0, F), &
      & MEMBER("I2O2            ",  154, "GC",  285.80D0, F), &
      & MEMBER("I2O3            ",  155, "GC",  301.80D0, F), &
      & MEMBER("CH3I            ",  156, "GC",  141.90D0, F), &
      & MEMBER("ICL             ",  157, "GC",  162.40D0, F), &
      & MEMBER("IBR             ",  158, "GC",  206.80D0, F), &
      & MEMBER("MI2             ",  159, "GC",  267.80D0, F), &
      & MEMBER("MIB             ",  160, "GC",  219.90D0, F), &
      & MEMBER("MIC             ",  161, "GC",  176.40D0, F) /)

      DATA CHEMISTRY_SPC(   1 ), SPECIES_MOLWT(   1 ) / 'NO2             ',   46.00D0 /
      DATA CHEMISTRY_SPC(   2 ), SPECIES_MOLWT(   2 ) / 'NO              ',   30.00D0 /
      DATA CHEMISTRY_SPC(   3 ), SPECIES_MOLWT(   3 ) / 'O               ',   16.00D0 /
      DATA CHEMISTRY_SPC(   4 ), SPECIES_MOLWT(   4 ) / 'O3              ',   48.00D0 /
      DATA CHEMISTRY_SPC(   5 ), SPECIES_MOLWT(   5 ) / 'NO3             ',   62.00D0 /
      DATA CHEMISTRY_SPC(   6 ), SPECIES_MOLWT(   6 ) / 'O1D             ',   16.00D0 /
      DATA CHEMISTRY_SPC(   7 ), SPECIES_MOLWT(   7 ) / 'OH              ',   17.00D0 /
      DATA CHEMISTRY_SPC(   8 ), SPECIES_MOLWT(   8 ) / 'HO2             ',   33.00D0 /
      DATA CHEMISTRY_SPC(   9 ), SPECIES_MOLWT(   9 ) / 'H2O2            ',   34.00D0 /
      DATA CHEMISTRY_SPC(  10 ), SPECIES_MOLWT(  10 ) / 'N2O5            ',  108.00D0 /
      DATA CHEMISTRY_SPC(  11 ), SPECIES_MOLWT(  11 ) / 'HNO3            ',   63.00D0 /
      DATA CHEMISTRY_SPC(  12 ), SPECIES_MOLWT(  12 ) / 'HONO            ',   47.00D0 /
      DATA CHEMISTRY_SPC(  13 ), SPECIES_MOLWT(  13 ) / 'PNA             ',   79.00D0 /
      DATA CHEMISTRY_SPC(  14 ), SPECIES_MOLWT(  14 ) / 'SO2             ',   64.00D0 /
      DATA CHEMISTRY_SPC(  15 ), SPECIES_MOLWT(  15 ) / 'SULF            ',   98.00D0 /
      DATA CHEMISTRY_SPC(  16 ), SPECIES_MOLWT(  16 ) / 'SULRXN          ',   98.00D0 /
      DATA CHEMISTRY_SPC(  17 ), SPECIES_MOLWT(  17 ) / 'C2O3            ',   75.00D0 /
      DATA CHEMISTRY_SPC(  18 ), SPECIES_MOLWT(  18 ) / 'MEO2            ',   47.00D0 /
      DATA CHEMISTRY_SPC(  19 ), SPECIES_MOLWT(  19 ) / 'RO2             ',   87.10D0 /
      DATA CHEMISTRY_SPC(  20 ), SPECIES_MOLWT(  20 ) / 'PAN             ',  121.00D0 /
      DATA CHEMISTRY_SPC(  21 ), SPECIES_MOLWT(  21 ) / 'PACD            ',   76.00D0 /
      DATA CHEMISTRY_SPC(  22 ), SPECIES_MOLWT(  22 ) / 'AACD            ',   60.00D0 /
      DATA CHEMISTRY_SPC(  23 ), SPECIES_MOLWT(  23 ) / 'CXO3            ',   89.00D0 /
      DATA CHEMISTRY_SPC(  24 ), SPECIES_MOLWT(  24 ) / 'ALD2            ',   44.00D0 /
      DATA CHEMISTRY_SPC(  25 ), SPECIES_MOLWT(  25 ) / 'XO2H            ',   87.10D0 /
      DATA CHEMISTRY_SPC(  26 ), SPECIES_MOLWT(  26 ) / 'PANX            ',  135.00D0 /
      DATA CHEMISTRY_SPC(  27 ), SPECIES_MOLWT(  27 ) / 'FORM            ',   30.00D0 /
      DATA CHEMISTRY_SPC(  28 ), SPECIES_MOLWT(  28 ) / 'MEPX            ',   48.00D0 /
      DATA CHEMISTRY_SPC(  29 ), SPECIES_MOLWT(  29 ) / 'MEOH            ',   32.00D0 /
      DATA CHEMISTRY_SPC(  30 ), SPECIES_MOLWT(  30 ) / 'ROOH            ',   90.10D0 /
      DATA CHEMISTRY_SPC(  31 ), SPECIES_MOLWT(  31 ) / 'XO2             ',   87.10D0 /
      DATA CHEMISTRY_SPC(  32 ), SPECIES_MOLWT(  32 ) / 'XO2N            ',   87.10D0 /
      DATA CHEMISTRY_SPC(  33 ), SPECIES_MOLWT(  33 ) / 'NTR1            ',  119.10D0 /
      DATA CHEMISTRY_SPC(  34 ), SPECIES_MOLWT(  34 ) / 'NTR2            ',  135.10D0 /
      DATA CHEMISTRY_SPC(  35 ), SPECIES_MOLWT(  35 ) / 'FACD            ',   46.00D0 /
      DATA CHEMISTRY_SPC(  36 ), SPECIES_MOLWT(  36 ) / 'CO              ',   28.00D0 /
      DATA CHEMISTRY_SPC(  37 ), SPECIES_MOLWT(  37 ) / 'HCO3            ',   63.00D0 /
      DATA CHEMISTRY_SPC(  38 ), SPECIES_MOLWT(  38 ) / 'ALDX            ',   58.10D0 /
      DATA CHEMISTRY_SPC(  39 ), SPECIES_MOLWT(  39 ) / 'GLYD            ',   60.00D0 /
      DATA CHEMISTRY_SPC(  40 ), SPECIES_MOLWT(  40 ) / 'GLY             ',   58.00D0 /
      DATA CHEMISTRY_SPC(  41 ), SPECIES_MOLWT(  41 ) / 'MGLY            ',   72.00D0 /
      DATA CHEMISTRY_SPC(  42 ), SPECIES_MOLWT(  42 ) / 'ETHA            ',   30.10D0 /
      DATA CHEMISTRY_SPC(  43 ), SPECIES_MOLWT(  43 ) / 'ETOH            ',   46.10D0 /
      DATA CHEMISTRY_SPC(  44 ), SPECIES_MOLWT(  44 ) / 'KET             ',   72.10D0 /
      DATA CHEMISTRY_SPC(  45 ), SPECIES_MOLWT(  45 ) / 'PAR             ',   14.00D0 /
      DATA CHEMISTRY_SPC(  46 ), SPECIES_MOLWT(  46 ) / 'ACET            ',   58.10D0 /
      DATA CHEMISTRY_SPC(  47 ), SPECIES_MOLWT(  47 ) / 'PRPA            ',   44.10D0 /
      DATA CHEMISTRY_SPC(  48 ), SPECIES_MOLWT(  48 ) / 'XPRP            ',   89.10D0 /
      DATA CHEMISTRY_SPC(  49 ), SPECIES_MOLWT(  49 ) / 'XPAR            ',   45.00D0 /
      DATA CHEMISTRY_SPC(  50 ), SPECIES_MOLWT(  50 ) / 'ROR             ',   29.00D0 /
      DATA CHEMISTRY_SPC(  51 ), SPECIES_MOLWT(  51 ) / 'ETHY            ',   26.00D0 /
      DATA CHEMISTRY_SPC(  52 ), SPECIES_MOLWT(  52 ) / 'ETH             ',   28.00D0 /
      DATA CHEMISTRY_SPC(  53 ), SPECIES_MOLWT(  53 ) / 'OLE             ',   42.10D0 /
      DATA CHEMISTRY_SPC(  54 ), SPECIES_MOLWT(  54 ) / 'IOLE            ',   56.10D0 /
      DATA CHEMISTRY_SPC(  55 ), SPECIES_MOLWT(  55 ) / 'ISOP            ',   68.10D0 /
      DATA CHEMISTRY_SPC(  56 ), SPECIES_MOLWT(  56 ) / 'ISO2            ',  117.10D0 /
      DATA CHEMISTRY_SPC(  57 ), SPECIES_MOLWT(  57 ) / 'ISOPRXN         ',   68.10D0 /
      DATA CHEMISTRY_SPC(  58 ), SPECIES_MOLWT(  58 ) / 'ISPD            ',   70.10D0 /
      DATA CHEMISTRY_SPC(  59 ), SPECIES_MOLWT(  59 ) / 'INTR            ',  147.10D0 /
      DATA CHEMISTRY_SPC(  60 ), SPECIES_MOLWT(  60 ) / 'ISPX            ',  118.10D0 /
      DATA CHEMISTRY_SPC(  61 ), SPECIES_MOLWT(  61 ) / 'HPLD            ',  116.10D0 /
      DATA CHEMISTRY_SPC(  62 ), SPECIES_MOLWT(  62 ) / 'OPO3            ',  115.00D0 /
      DATA CHEMISTRY_SPC(  63 ), SPECIES_MOLWT(  63 ) / 'EPOX            ',  118.10D0 /
      DATA CHEMISTRY_SPC(  64 ), SPECIES_MOLWT(  64 ) / 'EPX2            ',  149.10D0 /
      DATA CHEMISTRY_SPC(  65 ), SPECIES_MOLWT(  65 ) / 'TERP            ',  136.20D0 /
      DATA CHEMISTRY_SPC(  66 ), SPECIES_MOLWT(  66 ) / 'TRPRXN          ',  136.20D0 /
      DATA CHEMISTRY_SPC(  67 ), SPECIES_MOLWT(  67 ) / 'TERPNRO2        ',  197.00D0 /
      DATA CHEMISTRY_SPC(  68 ), SPECIES_MOLWT(  68 ) / 'APIN            ',  136.20D0 /
      DATA CHEMISTRY_SPC(  69 ), SPECIES_MOLWT(  69 ) / 'BENZENE         ',   78.10D0 /
      DATA CHEMISTRY_SPC(  70 ), SPECIES_MOLWT(  70 ) / 'CRES            ',  108.10D0 /
      DATA CHEMISTRY_SPC(  71 ), SPECIES_MOLWT(  71 ) / 'BZO2            ',  159.10D0 /
      DATA CHEMISTRY_SPC(  72 ), SPECIES_MOLWT(  72 ) / 'OPEN            ',   84.00D0 /
      DATA CHEMISTRY_SPC(  73 ), SPECIES_MOLWT(  73 ) / 'BENZRO2         ',  127.00D0 /
      DATA CHEMISTRY_SPC(  74 ), SPECIES_MOLWT(  74 ) / 'TOL             ',   92.10D0 /
      DATA CHEMISTRY_SPC(  75 ), SPECIES_MOLWT(  75 ) / 'TO2             ',  173.10D0 /
      DATA CHEMISTRY_SPC(  76 ), SPECIES_MOLWT(  76 ) / 'TOLRO2          ',  141.00D0 /
      DATA CHEMISTRY_SPC(  77 ), SPECIES_MOLWT(  77 ) / 'XOPN            ',   98.10D0 /
      DATA CHEMISTRY_SPC(  78 ), SPECIES_MOLWT(  78 ) / 'XYLMN           ',  106.20D0 /
      DATA CHEMISTRY_SPC(  79 ), SPECIES_MOLWT(  79 ) / 'XLO2            ',  187.10D0 /
      DATA CHEMISTRY_SPC(  80 ), SPECIES_MOLWT(  80 ) / 'XYLRO2          ',  155.00D0 /
      DATA CHEMISTRY_SPC(  81 ), SPECIES_MOLWT(  81 ) / 'NAPH            ',  128.20D0 /
      DATA CHEMISTRY_SPC(  82 ), SPECIES_MOLWT(  82 ) / 'PAHRO2          ',  187.20D0 /
      DATA CHEMISTRY_SPC(  83 ), SPECIES_MOLWT(  83 ) / 'CRO             ',  107.10D0 /
      DATA CHEMISTRY_SPC(  84 ), SPECIES_MOLWT(  84 ) / 'CAT1            ',  124.10D0 /
      DATA CHEMISTRY_SPC(  85 ), SPECIES_MOLWT(  85 ) / 'CRON            ',  153.10D0 /
      DATA CHEMISTRY_SPC(  86 ), SPECIES_MOLWT(  86 ) / 'OPAN            ',  161.00D0 /
      DATA CHEMISTRY_SPC(  87 ), SPECIES_MOLWT(  87 ) / 'ECH4            ',   16.00D0 /
      DATA CHEMISTRY_SPC(  88 ), SPECIES_MOLWT(  88 ) / 'CL2             ',   71.00D0 /
      DATA CHEMISTRY_SPC(  89 ), SPECIES_MOLWT(  89 ) / 'CL              ',   35.50D0 /
      DATA CHEMISTRY_SPC(  90 ), SPECIES_MOLWT(  90 ) / 'HOCL            ',   52.50D0 /
      DATA CHEMISTRY_SPC(  91 ), SPECIES_MOLWT(  91 ) / 'CLO             ',   51.50D0 /
      DATA CHEMISTRY_SPC(  92 ), SPECIES_MOLWT(  92 ) / 'FMCL            ',   64.50D0 /
      DATA CHEMISTRY_SPC(  93 ), SPECIES_MOLWT(  93 ) / 'HCL             ',   36.50D0 /
      DATA CHEMISTRY_SPC(  94 ), SPECIES_MOLWT(  94 ) / 'CLNO2           ',   81.50D0 /
      DATA CHEMISTRY_SPC(  95 ), SPECIES_MOLWT(  95 ) / 'CLNO3           ',   97.50D0 /
      DATA CHEMISTRY_SPC(  96 ), SPECIES_MOLWT(  96 ) / 'SVAVB2          ',  179.00D0 /
      DATA CHEMISTRY_SPC(  97 ), SPECIES_MOLWT(  97 ) / 'SVAVB3          ',  169.00D0 /
      DATA CHEMISTRY_SPC(  98 ), SPECIES_MOLWT(  98 ) / 'SVAVB4          ',  158.00D0 /
      DATA CHEMISTRY_SPC(  99 ), SPECIES_MOLWT(  99 ) / 'SVAVB1          ',  198.00D0 /
      DATA CHEMISTRY_SPC( 100 ), SPECIES_MOLWT( 100 ) / 'SESQ            ',  204.00D0 /
      DATA CHEMISTRY_SPC( 101 ), SPECIES_MOLWT( 101 ) / 'SESQRXN         ',  204.00D0 /
      DATA CHEMISTRY_SPC( 102 ), SPECIES_MOLWT( 102 ) / 'SOAALK          ',  112.00D0 /
      DATA CHEMISTRY_SPC( 103 ), SPECIES_MOLWT( 103 ) / 'H2NO3PIJ        ',   64.00D0 /
      DATA CHEMISTRY_SPC( 104 ), SPECIES_MOLWT( 104 ) / 'H2NO3PK         ',   64.00D0 /
      DATA CHEMISTRY_SPC( 105 ), SPECIES_MOLWT( 105 ) / 'ACLI            ',   35.50D0 /
      DATA CHEMISTRY_SPC( 106 ), SPECIES_MOLWT( 106 ) / 'ACLJ            ',   35.50D0 /
      DATA CHEMISTRY_SPC( 107 ), SPECIES_MOLWT( 107 ) / 'ACLK            ',   35.50D0 /
      DATA CHEMISTRY_SPC( 108 ), SPECIES_MOLWT( 108 ) / 'IEPOXP          ',  118.10D0 /
      DATA CHEMISTRY_SPC( 109 ), SPECIES_MOLWT( 109 ) / 'ASO4J           ',   96.00D0 /
      DATA CHEMISTRY_SPC( 110 ), SPECIES_MOLWT( 110 ) / 'AISO3J          ',  168.20D0 /
      DATA CHEMISTRY_SPC( 111 ), SPECIES_MOLWT( 111 ) / 'AGLYJ           ',   66.40D0 /
      DATA CHEMISTRY_SPC( 112 ), SPECIES_MOLWT( 112 ) / 'MTNO3           ',  231.00D0 /
      DATA CHEMISTRY_SPC( 113 ), SPECIES_MOLWT( 113 ) / 'AMTNO3J         ',  231.00D0 /
      DATA CHEMISTRY_SPC( 114 ), SPECIES_MOLWT( 114 ) / 'AMTHYDJ         ',  168.00D0 /
      DATA CHEMISTRY_SPC( 115 ), SPECIES_MOLWT( 115 ) / 'AAVB2J          ',  179.00D0 /
      DATA CHEMISTRY_SPC( 116 ), SPECIES_MOLWT( 116 ) / 'AOLGAJ          ',  206.00D0 /
      DATA CHEMISTRY_SPC( 117 ), SPECIES_MOLWT( 117 ) / 'AAVB3J          ',  169.00D0 /
      DATA CHEMISTRY_SPC( 118 ), SPECIES_MOLWT( 118 ) / 'AAVB4J          ',  158.00D0 /
      DATA CHEMISTRY_SPC( 119 ), SPECIES_MOLWT( 119 ) / 'AISO1J          ',  132.00D0 /
      DATA CHEMISTRY_SPC( 120 ), SPECIES_MOLWT( 120 ) / 'AOLGBJ          ',  248.00D0 /
      DATA CHEMISTRY_SPC( 121 ), SPECIES_MOLWT( 121 ) / 'AISO2J          ',  133.00D0 /
      DATA CHEMISTRY_SPC( 122 ), SPECIES_MOLWT( 122 ) / 'ASQTJ           ',  273.00D0 /
      DATA CHEMISTRY_SPC( 123 ), SPECIES_MOLWT( 123 ) / 'APOCI           ',  220.00D0 /
      DATA CHEMISTRY_SPC( 124 ), SPECIES_MOLWT( 124 ) / 'APNCOMI         ',  220.00D0 /
      DATA CHEMISTRY_SPC( 125 ), SPECIES_MOLWT( 125 ) / 'APOCJ           ',  220.00D0 /
      DATA CHEMISTRY_SPC( 126 ), SPECIES_MOLWT( 126 ) / 'APNCOMJ         ',  220.00D0 /
      DATA CHEMISTRY_SPC( 127 ), SPECIES_MOLWT( 127 ) / 'PCVOC           ',  170.00D0 /
      DATA CHEMISTRY_SPC( 128 ), SPECIES_MOLWT( 128 ) / 'PCSOARXN        ',  170.00D0 /
      DATA CHEMISTRY_SPC( 129 ), SPECIES_MOLWT( 129 ) / 'VLVPO1          ',  218.00D0 /
      DATA CHEMISTRY_SPC( 130 ), SPECIES_MOLWT( 130 ) / 'VSVPO1          ',  230.00D0 /
      DATA CHEMISTRY_SPC( 131 ), SPECIES_MOLWT( 131 ) / 'VSVPO2          ',  241.00D0 /
      DATA CHEMISTRY_SPC( 132 ), SPECIES_MOLWT( 132 ) / 'VSVPO3          ',  253.00D0 /
      DATA CHEMISTRY_SPC( 133 ), SPECIES_MOLWT( 133 ) / 'VIVPO1          ',  266.00D0 /
      DATA CHEMISTRY_SPC( 134 ), SPECIES_MOLWT( 134 ) / 'VLVOO1          ',  136.00D0 /
      DATA CHEMISTRY_SPC( 135 ), SPECIES_MOLWT( 135 ) / 'VLVOO2          ',  136.00D0 /
      DATA CHEMISTRY_SPC( 136 ), SPECIES_MOLWT( 136 ) / 'VSVOO2          ',  135.00D0 /
      DATA CHEMISTRY_SPC( 137 ), SPECIES_MOLWT( 137 ) / 'VSVOO3          ',  134.00D0 /
      DATA CHEMISTRY_SPC( 138 ), SPECIES_MOLWT( 138 ) / 'VSVOO1          ',  135.00D0 /
      DATA CHEMISTRY_SPC( 139 ), SPECIES_MOLWT( 139 ) / 'FORM_PRIMARY    ',   30.00D0 /
      DATA CHEMISTRY_SPC( 140 ), SPECIES_MOLWT( 140 ) / 'ALD2_PRIMARY    ',   44.00D0 /
      DATA CHEMISTRY_SPC( 141 ), SPECIES_MOLWT( 141 ) / 'BUTADIENE13     ',   54.00D0 /
      DATA CHEMISTRY_SPC( 142 ), SPECIES_MOLWT( 142 ) / 'ACROLEIN        ',   56.10D0 /
      DATA CHEMISTRY_SPC( 143 ), SPECIES_MOLWT( 143 ) / 'ACRO_PRIMARY    ',   56.10D0 /
      DATA CHEMISTRY_SPC( 144 ), SPECIES_MOLWT( 144 ) / 'TOLU            ',   92.00D0 /
      DATA CHEMISTRY_SPC( 145 ), SPECIES_MOLWT( 145 ) / 'HG              ',  200.60D0 /
      DATA CHEMISTRY_SPC( 146 ), SPECIES_MOLWT( 146 ) / 'HGIIAER         ',  200.60D0 /
      DATA CHEMISTRY_SPC( 147 ), SPECIES_MOLWT( 147 ) / 'HGIIGAS         ',  271.50D0 /
      DATA CHEMISTRY_SPC( 148 ), SPECIES_MOLWT( 148 ) / 'BR              ',   79.90D0 /
      DATA CHEMISTRY_SPC( 149 ), SPECIES_MOLWT( 149 ) / 'BRO             ',   95.90D0 /
      DATA CHEMISTRY_SPC( 150 ), SPECIES_MOLWT( 150 ) / 'HOBR            ',   96.90D0 /
      DATA CHEMISTRY_SPC( 151 ), SPECIES_MOLWT( 151 ) / 'HBR             ',   80.90D0 /
      DATA CHEMISTRY_SPC( 152 ), SPECIES_MOLWT( 152 ) / 'BR2             ',  159.80D0 /
      DATA CHEMISTRY_SPC( 153 ), SPECIES_MOLWT( 153 ) / 'BRNO3           ',  141.90D0 /
      DATA CHEMISTRY_SPC( 154 ), SPECIES_MOLWT( 154 ) / 'BRNO2           ',  125.90D0 /
      DATA CHEMISTRY_SPC( 155 ), SPECIES_MOLWT( 155 ) / 'FMBR            ',  108.90D0 /
      DATA CHEMISTRY_SPC( 156 ), SPECIES_MOLWT( 156 ) / 'MB3             ',  252.70D0 /
      DATA CHEMISTRY_SPC( 157 ), SPECIES_MOLWT( 157 ) / 'MB2             ',  173.80D0 /
      DATA CHEMISTRY_SPC( 158 ), SPECIES_MOLWT( 158 ) / 'MB2C            ',  208.30D0 /
      DATA CHEMISTRY_SPC( 159 ), SPECIES_MOLWT( 159 ) / 'MBC2            ',  243.80D0 /
      DATA CHEMISTRY_SPC( 160 ), SPECIES_MOLWT( 160 ) / 'MBC             ',  129.40D0 /
      DATA CHEMISTRY_SPC( 161 ), SPECIES_MOLWT( 161 ) / 'DMS             ',   62.00D0 /
      DATA CHEMISTRY_SPC( 162 ), SPECIES_MOLWT( 162 ) / 'MSA             ',   96.00D0 /
      DATA CHEMISTRY_SPC( 163 ), SPECIES_MOLWT( 163 ) / 'BRCL            ',  115.40D0 /
      DATA CHEMISTRY_SPC( 164 ), SPECIES_MOLWT( 164 ) / 'ABRJ            ',   79.90D0 /
      DATA CHEMISTRY_SPC( 165 ), SPECIES_MOLWT( 165 ) / 'I               ',  126.90D0 /
      DATA CHEMISTRY_SPC( 166 ), SPECIES_MOLWT( 166 ) / 'IO              ',  142.90D0 /
      DATA CHEMISTRY_SPC( 167 ), SPECIES_MOLWT( 167 ) / 'HI              ',  127.90D0 /
      DATA CHEMISTRY_SPC( 168 ), SPECIES_MOLWT( 168 ) / 'I2              ',  253.80D0 /
      DATA CHEMISTRY_SPC( 169 ), SPECIES_MOLWT( 169 ) / 'HOI             ',  143.90D0 /
      DATA CHEMISTRY_SPC( 170 ), SPECIES_MOLWT( 170 ) / 'INO             ',  156.90D0 /
      DATA CHEMISTRY_SPC( 171 ), SPECIES_MOLWT( 171 ) / 'INO2            ',  172.90D0 /
      DATA CHEMISTRY_SPC( 172 ), SPECIES_MOLWT( 172 ) / 'INO3            ',  188.90D0 /
      DATA CHEMISTRY_SPC( 173 ), SPECIES_MOLWT( 173 ) / 'OIO             ',  158.90D0 /
      DATA CHEMISTRY_SPC( 174 ), SPECIES_MOLWT( 174 ) / 'I2O4            ',  317.80D0 /
      DATA CHEMISTRY_SPC( 175 ), SPECIES_MOLWT( 175 ) / 'I2O2            ',  285.80D0 /
      DATA CHEMISTRY_SPC( 176 ), SPECIES_MOLWT( 176 ) / 'I2O3            ',  301.80D0 /
      DATA CHEMISTRY_SPC( 177 ), SPECIES_MOLWT( 177 ) / 'CH3I            ',  141.90D0 /
      DATA CHEMISTRY_SPC( 178 ), SPECIES_MOLWT( 178 ) / 'ICL             ',  162.40D0 /
      DATA CHEMISTRY_SPC( 179 ), SPECIES_MOLWT( 179 ) / 'IBR             ',  206.80D0 /
      DATA CHEMISTRY_SPC( 180 ), SPECIES_MOLWT( 180 ) / 'MI2             ',  267.80D0 /
      DATA CHEMISTRY_SPC( 181 ), SPECIES_MOLWT( 181 ) / 'MIB             ',  219.90D0 /
      DATA CHEMISTRY_SPC( 182 ), SPECIES_MOLWT( 182 ) / 'MIC             ',  176.40D0 /


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
      DATA CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), CONVERT_CONC(  64 ) /   65, 'GC', F /  ! EPX2
      DATA CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), CONVERT_CONC(  65 ) /   66, 'GC', F /  ! TERP
      DATA CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), CONVERT_CONC(  66 ) /   70, 'GC', F /  ! TRPRXN
      DATA CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), CONVERT_CONC(  67 ) /   68, 'GC', F /  ! TERPNRO2
      DATA CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), CONVERT_CONC(  68 ) /   67, 'GC', F /  ! APIN
      DATA CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), CONVERT_CONC(  69 ) /   71, 'GC', F /  ! BENZENE
      DATA CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), CONVERT_CONC(  70 ) /   72, 'GC', F /  ! CRES
      DATA CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), CONVERT_CONC(  71 ) /   73, 'GC', F /  ! BZO2
      DATA CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), CONVERT_CONC(  72 ) /   74, 'GC', F /  ! OPEN
      DATA CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), CONVERT_CONC(  73 ) /   75, 'GC', F /  ! BENZRO2
      DATA CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), CONVERT_CONC(  74 ) /   76, 'GC', F /  ! TOL
      DATA CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), CONVERT_CONC(  75 ) /   77, 'GC', F /  ! TO2
      DATA CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), CONVERT_CONC(  76 ) /   78, 'GC', F /  ! TOLRO2
      DATA CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), CONVERT_CONC(  77 ) /   79, 'GC', F /  ! XOPN
      DATA CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), CONVERT_CONC(  78 ) /   80, 'GC', F /  ! XYLMN
      DATA CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), CONVERT_CONC(  79 ) /   81, 'GC', F /  ! XLO2
      DATA CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), CONVERT_CONC(  80 ) /   82, 'GC', F /  ! XYLRO2
      DATA CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), CONVERT_CONC(  81 ) /   83, 'GC', F /  ! NAPH
      DATA CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), CONVERT_CONC(  82 ) /   84, 'GC', F /  ! PAHRO2
      DATA CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), CONVERT_CONC(  83 ) /   85, 'GC', F /  ! CRO
      DATA CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), CONVERT_CONC(  84 ) /   86, 'GC', F /  ! CAT1
      DATA CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), CONVERT_CONC(  85 ) /   87, 'GC', F /  ! CRON
      DATA CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), CONVERT_CONC(  86 ) /   88, 'GC', F /  ! OPAN
      DATA CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), CONVERT_CONC(  87 ) /   89, 'GC', F /  ! ECH4
      DATA CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), CONVERT_CONC(  88 ) /   90, 'GC', F /  ! CL2
      DATA CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), CONVERT_CONC(  89 ) /   91, 'GC', F /  ! CL
      DATA CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), CONVERT_CONC(  90 ) /   92, 'GC', F /  ! HOCL
      DATA CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), CONVERT_CONC(  91 ) /   93, 'GC', F /  ! CLO
      DATA CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), CONVERT_CONC(  92 ) /   94, 'GC', F /  ! FMCL
      DATA CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), CONVERT_CONC(  93 ) /   95, 'GC', F /  ! HCL
      DATA CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), CONVERT_CONC(  94 ) /   96, 'GC', F /  ! CLNO2
      DATA CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), CONVERT_CONC(  95 ) /   97, 'GC', F /  ! CLNO3
      DATA CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), CONVERT_CONC(  96 ) /  125, 'GC', F /  ! SVAVB2
      DATA CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), CONVERT_CONC(  97 ) /  126, 'GC', F /  ! SVAVB3
      DATA CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), CONVERT_CONC(  98 ) /  127, 'GC', F /  ! SVAVB4
      DATA CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), CONVERT_CONC(  99 ) /  124, 'GC', F /  ! SVAVB1
      DATA CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), CONVERT_CONC( 100 ) /   98, 'GC', F /  ! SESQ
      DATA CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), CONVERT_CONC( 101 ) /   99, 'GC', F /  ! SESQRXN
      DATA CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), CONVERT_CONC( 102 ) /  100, 'GC', F /  ! SOAALK
      DATA CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), CONVERT_CONC( 103 ) /  101, 'GC', F /  ! H2NO3PIJ
      DATA CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), CONVERT_CONC( 104 ) /  102, 'GC', F /  ! H2NO3PK
      DATA CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), CONVERT_CONC( 105 ) /  174, 'AE', T /  ! ACLI
      DATA CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), CONVERT_CONC( 106 ) /  175, 'AE', T /  ! ACLJ
      DATA CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), CONVERT_CONC( 107 ) /  176, 'AE', T /  ! ACLK
      DATA CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), CONVERT_CONC( 108 ) /   64, 'GC', F /  ! IEPOXP
      DATA CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), CONVERT_CONC( 109 ) /  164, 'AE', T /  ! ASO4J
      DATA CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), CONVERT_CONC( 110 ) /  209, 'AE', T /  ! AISO3J
      DATA CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), CONVERT_CONC( 111 ) /  212, 'AE', T /  ! AGLYJ
      DATA CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), CONVERT_CONC( 112 ) /   69, 'GC', F /  ! MTNO3
      DATA CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), CONVERT_CONC( 113 ) /  213, 'AE', T /  ! AMTNO3J
      DATA CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), CONVERT_CONC( 114 ) /  214, 'AE', T /  ! AMTHYDJ
      DATA CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), CONVERT_CONC( 115 ) /  238, 'AE', T /  ! AAVB2J
      DATA CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), CONVERT_CONC( 116 ) /  210, 'AE', T /  ! AOLGAJ
      DATA CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), CONVERT_CONC( 117 ) /  239, 'AE', T /  ! AAVB3J
      DATA CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), CONVERT_CONC( 118 ) /  240, 'AE', T /  ! AAVB4J
      DATA CGRID_INDEX( 119 ), SPECIES_TYPE( 119 ), CONVERT_CONC( 119 ) /  177, 'AE', T /  ! AISO1J
      DATA CGRID_INDEX( 120 ), SPECIES_TYPE( 120 ), CONVERT_CONC( 120 ) /  211, 'AE', T /  ! AOLGBJ
      DATA CGRID_INDEX( 121 ), SPECIES_TYPE( 121 ), CONVERT_CONC( 121 ) /  178, 'AE', T /  ! AISO2J
      DATA CGRID_INDEX( 122 ), SPECIES_TYPE( 122 ), CONVERT_CONC( 122 ) /  179, 'AE', T /  ! ASQTJ
      DATA CGRID_INDEX( 123 ), SPECIES_TYPE( 123 ), CONVERT_CONC( 123 ) /  215, 'AE', T /  ! APOCI
      DATA CGRID_INDEX( 124 ), SPECIES_TYPE( 124 ), CONVERT_CONC( 124 ) /  217, 'AE', T /  ! APNCOMI
      DATA CGRID_INDEX( 125 ), SPECIES_TYPE( 125 ), CONVERT_CONC( 125 ) /  216, 'AE', T /  ! APOCJ
      DATA CGRID_INDEX( 126 ), SPECIES_TYPE( 126 ), CONVERT_CONC( 126 ) /  218, 'AE', T /  ! APNCOMJ
      DATA CGRID_INDEX( 127 ), SPECIES_TYPE( 127 ), CONVERT_CONC( 127 ) /  113, 'GC', F /  ! PCVOC
      DATA CGRID_INDEX( 128 ), SPECIES_TYPE( 128 ), CONVERT_CONC( 128 ) /  114, 'GC', F /  ! PCSOARXN
      DATA CGRID_INDEX( 129 ), SPECIES_TYPE( 129 ), CONVERT_CONC( 129 ) /  103, 'GC', F /  ! VLVPO1
      DATA CGRID_INDEX( 130 ), SPECIES_TYPE( 130 ), CONVERT_CONC( 130 ) /  104, 'GC', F /  ! VSVPO1
      DATA CGRID_INDEX( 131 ), SPECIES_TYPE( 131 ), CONVERT_CONC( 131 ) /  105, 'GC', F /  ! VSVPO2
      DATA CGRID_INDEX( 132 ), SPECIES_TYPE( 132 ), CONVERT_CONC( 132 ) /  106, 'GC', F /  ! VSVPO3
      DATA CGRID_INDEX( 133 ), SPECIES_TYPE( 133 ), CONVERT_CONC( 133 ) /  107, 'GC', F /  ! VIVPO1
      DATA CGRID_INDEX( 134 ), SPECIES_TYPE( 134 ), CONVERT_CONC( 134 ) /  108, 'GC', F /  ! VLVOO1
      DATA CGRID_INDEX( 135 ), SPECIES_TYPE( 135 ), CONVERT_CONC( 135 ) /  109, 'GC', F /  ! VLVOO2
      DATA CGRID_INDEX( 136 ), SPECIES_TYPE( 136 ), CONVERT_CONC( 136 ) /  111, 'GC', F /  ! VSVOO2
      DATA CGRID_INDEX( 137 ), SPECIES_TYPE( 137 ), CONVERT_CONC( 137 ) /  112, 'GC', F /  ! VSVOO3
      DATA CGRID_INDEX( 138 ), SPECIES_TYPE( 138 ), CONVERT_CONC( 138 ) /  110, 'GC', F /  ! VSVOO1
      DATA CGRID_INDEX( 139 ), SPECIES_TYPE( 139 ), CONVERT_CONC( 139 ) /  115, 'GC', F /  ! FORM_PRIMARY
      DATA CGRID_INDEX( 140 ), SPECIES_TYPE( 140 ), CONVERT_CONC( 140 ) /  116, 'GC', F /  ! ALD2_PRIMARY
      DATA CGRID_INDEX( 141 ), SPECIES_TYPE( 141 ), CONVERT_CONC( 141 ) /  117, 'GC', F /  ! BUTADIENE13
      DATA CGRID_INDEX( 142 ), SPECIES_TYPE( 142 ), CONVERT_CONC( 142 ) /  118, 'GC', F /  ! ACROLEIN
      DATA CGRID_INDEX( 143 ), SPECIES_TYPE( 143 ), CONVERT_CONC( 143 ) /  119, 'GC', F /  ! ACRO_PRIMARY
      DATA CGRID_INDEX( 144 ), SPECIES_TYPE( 144 ), CONVERT_CONC( 144 ) /  120, 'GC', F /  ! TOLU
      DATA CGRID_INDEX( 145 ), SPECIES_TYPE( 145 ), CONVERT_CONC( 145 ) /  121, 'GC', F /  ! HG
      DATA CGRID_INDEX( 146 ), SPECIES_TYPE( 146 ), CONVERT_CONC( 146 ) /  122, 'GC', F /  ! HGIIAER
      DATA CGRID_INDEX( 147 ), SPECIES_TYPE( 147 ), CONVERT_CONC( 147 ) /  123, 'GC', F /  ! HGIIGAS
      DATA CGRID_INDEX( 148 ), SPECIES_TYPE( 148 ), CONVERT_CONC( 148 ) /  128, 'GC', F /  ! BR
      DATA CGRID_INDEX( 149 ), SPECIES_TYPE( 149 ), CONVERT_CONC( 149 ) /  129, 'GC', F /  ! BRO
      DATA CGRID_INDEX( 150 ), SPECIES_TYPE( 150 ), CONVERT_CONC( 150 ) /  130, 'GC', F /  ! HOBR
      DATA CGRID_INDEX( 151 ), SPECIES_TYPE( 151 ), CONVERT_CONC( 151 ) /  131, 'GC', F /  ! HBR
      DATA CGRID_INDEX( 152 ), SPECIES_TYPE( 152 ), CONVERT_CONC( 152 ) /  132, 'GC', F /  ! BR2
      DATA CGRID_INDEX( 153 ), SPECIES_TYPE( 153 ), CONVERT_CONC( 153 ) /  133, 'GC', F /  ! BRNO3
      DATA CGRID_INDEX( 154 ), SPECIES_TYPE( 154 ), CONVERT_CONC( 154 ) /  134, 'GC', F /  ! BRNO2
      DATA CGRID_INDEX( 155 ), SPECIES_TYPE( 155 ), CONVERT_CONC( 155 ) /  135, 'GC', F /  ! FMBR
      DATA CGRID_INDEX( 156 ), SPECIES_TYPE( 156 ), CONVERT_CONC( 156 ) /  136, 'GC', F /  ! MB3
      DATA CGRID_INDEX( 157 ), SPECIES_TYPE( 157 ), CONVERT_CONC( 157 ) /  137, 'GC', F /  ! MB2
      DATA CGRID_INDEX( 158 ), SPECIES_TYPE( 158 ), CONVERT_CONC( 158 ) /  138, 'GC', F /  ! MB2C
      DATA CGRID_INDEX( 159 ), SPECIES_TYPE( 159 ), CONVERT_CONC( 159 ) /  139, 'GC', F /  ! MBC2
      DATA CGRID_INDEX( 160 ), SPECIES_TYPE( 160 ), CONVERT_CONC( 160 ) /  140, 'GC', F /  ! MBC
      DATA CGRID_INDEX( 161 ), SPECIES_TYPE( 161 ), CONVERT_CONC( 161 ) /  141, 'GC', F /  ! DMS
      DATA CGRID_INDEX( 162 ), SPECIES_TYPE( 162 ), CONVERT_CONC( 162 ) /  142, 'GC', F /  ! MSA
      DATA CGRID_INDEX( 163 ), SPECIES_TYPE( 163 ), CONVERT_CONC( 163 ) /  143, 'GC', F /  ! BRCL
      DATA CGRID_INDEX( 164 ), SPECIES_TYPE( 164 ), CONVERT_CONC( 164 ) /  247, 'AE', T /  ! ABRJ
      DATA CGRID_INDEX( 165 ), SPECIES_TYPE( 165 ), CONVERT_CONC( 165 ) /  144, 'GC', F /  ! I
      DATA CGRID_INDEX( 166 ), SPECIES_TYPE( 166 ), CONVERT_CONC( 166 ) /  145, 'GC', F /  ! IO
      DATA CGRID_INDEX( 167 ), SPECIES_TYPE( 167 ), CONVERT_CONC( 167 ) /  146, 'GC', F /  ! HI
      DATA CGRID_INDEX( 168 ), SPECIES_TYPE( 168 ), CONVERT_CONC( 168 ) /  147, 'GC', F /  ! I2
      DATA CGRID_INDEX( 169 ), SPECIES_TYPE( 169 ), CONVERT_CONC( 169 ) /  148, 'GC', F /  ! HOI
      DATA CGRID_INDEX( 170 ), SPECIES_TYPE( 170 ), CONVERT_CONC( 170 ) /  149, 'GC', F /  ! INO
      DATA CGRID_INDEX( 171 ), SPECIES_TYPE( 171 ), CONVERT_CONC( 171 ) /  150, 'GC', F /  ! INO2
      DATA CGRID_INDEX( 172 ), SPECIES_TYPE( 172 ), CONVERT_CONC( 172 ) /  151, 'GC', F /  ! INO3
      DATA CGRID_INDEX( 173 ), SPECIES_TYPE( 173 ), CONVERT_CONC( 173 ) /  152, 'GC', F /  ! OIO
      DATA CGRID_INDEX( 174 ), SPECIES_TYPE( 174 ), CONVERT_CONC( 174 ) /  153, 'GC', F /  ! I2O4
      DATA CGRID_INDEX( 175 ), SPECIES_TYPE( 175 ), CONVERT_CONC( 175 ) /  154, 'GC', F /  ! I2O2
      DATA CGRID_INDEX( 176 ), SPECIES_TYPE( 176 ), CONVERT_CONC( 176 ) /  155, 'GC', F /  ! I2O3
      DATA CGRID_INDEX( 177 ), SPECIES_TYPE( 177 ), CONVERT_CONC( 177 ) /  156, 'GC', F /  ! CH3I
      DATA CGRID_INDEX( 178 ), SPECIES_TYPE( 178 ), CONVERT_CONC( 178 ) /  157, 'GC', F /  ! ICL
      DATA CGRID_INDEX( 179 ), SPECIES_TYPE( 179 ), CONVERT_CONC( 179 ) /  158, 'GC', F /  ! IBR
      DATA CGRID_INDEX( 180 ), SPECIES_TYPE( 180 ), CONVERT_CONC( 180 ) /  159, 'GC', F /  ! MI2
      DATA CGRID_INDEX( 181 ), SPECIES_TYPE( 181 ), CONVERT_CONC( 181 ) /  160, 'GC', F /  ! MIB
      DATA CGRID_INDEX( 182 ), SPECIES_TYPE( 182 ), CONVERT_CONC( 182 ) /  161, 'GC', F /  ! MIC

! The below integers define the locations of mechanism species in the solver
! concentration array.

      INTEGER :: INDEX_NO2          =    1
      INTEGER :: INDEX_NO           =    2
      INTEGER :: INDEX_O            =    3
      INTEGER :: INDEX_O3           =    4
      INTEGER :: INDEX_NO3          =    5
      INTEGER :: INDEX_O1D          =    6
      INTEGER :: INDEX_OH           =    7
      INTEGER :: INDEX_HO2          =    8
      INTEGER :: INDEX_H2O2         =    9
      INTEGER :: INDEX_N2O5         =   10
      INTEGER :: INDEX_HNO3         =   11
      INTEGER :: INDEX_HONO         =   12
      INTEGER :: INDEX_PNA          =   13
      INTEGER :: INDEX_SO2          =   14
      INTEGER :: INDEX_SULF         =   15
      INTEGER :: INDEX_SULRXN       =   16
      INTEGER :: INDEX_C2O3         =   17
      INTEGER :: INDEX_MEO2         =   18
      INTEGER :: INDEX_RO2          =   19
      INTEGER :: INDEX_PAN          =   20
      INTEGER :: INDEX_PACD         =   21
      INTEGER :: INDEX_AACD         =   22
      INTEGER :: INDEX_CXO3         =   23
      INTEGER :: INDEX_ALD2         =   24
      INTEGER :: INDEX_XO2H         =   25
      INTEGER :: INDEX_PANX         =   26
      INTEGER :: INDEX_FORM         =   27
      INTEGER :: INDEX_MEPX         =   28
      INTEGER :: INDEX_MEOH         =   29
      INTEGER :: INDEX_ROOH         =   30
      INTEGER :: INDEX_XO2          =   31
      INTEGER :: INDEX_XO2N         =   32
      INTEGER :: INDEX_NTR1         =   33
      INTEGER :: INDEX_NTR2         =   34
      INTEGER :: INDEX_FACD         =   35
      INTEGER :: INDEX_CO           =   36
      INTEGER :: INDEX_HCO3         =   37
      INTEGER :: INDEX_ALDX         =   38
      INTEGER :: INDEX_GLYD         =   39
      INTEGER :: INDEX_GLY          =   40
      INTEGER :: INDEX_MGLY         =   41
      INTEGER :: INDEX_ETHA         =   42
      INTEGER :: INDEX_ETOH         =   43
      INTEGER :: INDEX_KET          =   44
      INTEGER :: INDEX_PAR          =   45
      INTEGER :: INDEX_ACET         =   46
      INTEGER :: INDEX_PRPA         =   47
      INTEGER :: INDEX_XPRP         =   48
      INTEGER :: INDEX_XPAR         =   49
      INTEGER :: INDEX_ROR          =   50
      INTEGER :: INDEX_ETHY         =   51
      INTEGER :: INDEX_ETH          =   52
      INTEGER :: INDEX_OLE          =   53
      INTEGER :: INDEX_IOLE         =   54
      INTEGER :: INDEX_ISOP         =   55
      INTEGER :: INDEX_ISO2         =   56
      INTEGER :: INDEX_ISOPRXN      =   57
      INTEGER :: INDEX_ISPD         =   58
      INTEGER :: INDEX_INTR         =   59
      INTEGER :: INDEX_ISPX         =   60
      INTEGER :: INDEX_HPLD         =   61
      INTEGER :: INDEX_OPO3         =   62
      INTEGER :: INDEX_EPOX         =   63
      INTEGER :: INDEX_EPX2         =   64
      INTEGER :: INDEX_TERP         =   65
      INTEGER :: INDEX_TRPRXN       =   66
      INTEGER :: INDEX_TERPNRO2     =   67
      INTEGER :: INDEX_APIN         =   68
      INTEGER :: INDEX_BENZENE      =   69
      INTEGER :: INDEX_CRES         =   70
      INTEGER :: INDEX_BZO2         =   71
      INTEGER :: INDEX_OPEN         =   72
      INTEGER :: INDEX_BENZRO2      =   73
      INTEGER :: INDEX_TOL          =   74
      INTEGER :: INDEX_TO2          =   75
      INTEGER :: INDEX_TOLRO2       =   76
      INTEGER :: INDEX_XOPN         =   77
      INTEGER :: INDEX_XYLMN        =   78
      INTEGER :: INDEX_XLO2         =   79
      INTEGER :: INDEX_XYLRO2       =   80
      INTEGER :: INDEX_NAPH         =   81
      INTEGER :: INDEX_PAHRO2       =   82
      INTEGER :: INDEX_CRO          =   83
      INTEGER :: INDEX_CAT1         =   84
      INTEGER :: INDEX_CRON         =   85
      INTEGER :: INDEX_OPAN         =   86
      INTEGER :: INDEX_ECH4         =   87
      INTEGER :: INDEX_CL2          =   88
      INTEGER :: INDEX_CL           =   89
      INTEGER :: INDEX_HOCL         =   90
      INTEGER :: INDEX_CLO          =   91
      INTEGER :: INDEX_FMCL         =   92
      INTEGER :: INDEX_HCL          =   93
      INTEGER :: INDEX_CLNO2        =   94
      INTEGER :: INDEX_CLNO3        =   95
      INTEGER :: INDEX_SVAVB2       =   96
      INTEGER :: INDEX_SVAVB3       =   97
      INTEGER :: INDEX_SVAVB4       =   98
      INTEGER :: INDEX_SVAVB1       =   99
      INTEGER :: INDEX_SESQ         =  100
      INTEGER :: INDEX_SESQRXN      =  101
      INTEGER :: INDEX_SOAALK       =  102
      INTEGER :: INDEX_H2NO3PIJ     =  103
      INTEGER :: INDEX_H2NO3PK      =  104
      INTEGER :: INDEX_ACLI         =  105
      INTEGER :: INDEX_ACLJ         =  106
      INTEGER :: INDEX_ACLK         =  107
      INTEGER :: INDEX_IEPOXP       =  108
      INTEGER :: INDEX_ASO4J        =  109
      INTEGER :: INDEX_AISO3J       =  110
      INTEGER :: INDEX_AGLYJ        =  111
      INTEGER :: INDEX_MTNO3        =  112
      INTEGER :: INDEX_AMTNO3J      =  113
      INTEGER :: INDEX_AMTHYDJ      =  114
      INTEGER :: INDEX_AAVB2J       =  115
      INTEGER :: INDEX_AOLGAJ       =  116
      INTEGER :: INDEX_AAVB3J       =  117
      INTEGER :: INDEX_AAVB4J       =  118
      INTEGER :: INDEX_AISO1J       =  119
      INTEGER :: INDEX_AOLGBJ       =  120
      INTEGER :: INDEX_AISO2J       =  121
      INTEGER :: INDEX_ASQTJ        =  122
      INTEGER :: INDEX_APOCI        =  123
      INTEGER :: INDEX_APNCOMI      =  124
      INTEGER :: INDEX_APOCJ        =  125
      INTEGER :: INDEX_APNCOMJ      =  126
      INTEGER :: INDEX_PCVOC        =  127
      INTEGER :: INDEX_PCSOARXN     =  128
      INTEGER :: INDEX_VLVPO1       =  129
      INTEGER :: INDEX_VSVPO1       =  130
      INTEGER :: INDEX_VSVPO2       =  131
      INTEGER :: INDEX_VSVPO3       =  132
      INTEGER :: INDEX_VIVPO1       =  133
      INTEGER :: INDEX_VLVOO1       =  134
      INTEGER :: INDEX_VLVOO2       =  135
      INTEGER :: INDEX_VSVOO2       =  136
      INTEGER :: INDEX_VSVOO3       =  137
      INTEGER :: INDEX_VSVOO1       =  138
      INTEGER :: INDEX_FORM_PRIMARY =  139
      INTEGER :: INDEX_ALD2_PRIMARY =  140
      INTEGER :: INDEX_BUTADIENE13  =  141
      INTEGER :: INDEX_ACROLEIN     =  142
      INTEGER :: INDEX_ACRO_PRIMARY =  143
      INTEGER :: INDEX_TOLU         =  144
      INTEGER :: INDEX_HG           =  145
      INTEGER :: INDEX_HGIIAER      =  146
      INTEGER :: INDEX_HGIIGAS      =  147
      INTEGER :: INDEX_BR           =  148
      INTEGER :: INDEX_BRO          =  149
      INTEGER :: INDEX_HOBR         =  150
      INTEGER :: INDEX_HBR          =  151
      INTEGER :: INDEX_BR2          =  152
      INTEGER :: INDEX_BRNO3        =  153
      INTEGER :: INDEX_BRNO2        =  154
      INTEGER :: INDEX_FMBR         =  155
      INTEGER :: INDEX_MB3          =  156
      INTEGER :: INDEX_MB2          =  157
      INTEGER :: INDEX_MB2C         =  158
      INTEGER :: INDEX_MBC2         =  159
      INTEGER :: INDEX_MBC          =  160
      INTEGER :: INDEX_DMS          =  161
      INTEGER :: INDEX_MSA          =  162
      INTEGER :: INDEX_BRCL         =  163
      INTEGER :: INDEX_ABRJ         =  164
      INTEGER :: INDEX_I            =  165
      INTEGER :: INDEX_IO           =  166
      INTEGER :: INDEX_HI           =  167
      INTEGER :: INDEX_I2           =  168
      INTEGER :: INDEX_HOI          =  169
      INTEGER :: INDEX_INO          =  170
      INTEGER :: INDEX_INO2         =  171
      INTEGER :: INDEX_INO3         =  172
      INTEGER :: INDEX_OIO          =  173
      INTEGER :: INDEX_I2O4         =  174
      INTEGER :: INDEX_I2O2         =  175
      INTEGER :: INDEX_I2O3         =  176
      INTEGER :: INDEX_CH3I         =  177
      INTEGER :: INDEX_ICL          =  178
      INTEGER :: INDEX_IBR          =  179
      INTEGER :: INDEX_MI2          =  180
      INTEGER :: INDEX_MIB          =  181
      INTEGER :: INDEX_MIC          =  182

      INTEGER, PARAMETER :: N_ACT_SP = 182

      INTEGER, PARAMETER :: NRXNS = 453

      INTEGER, PARAMETER ::     ONE_REACT_REACTIONS =    0

      INTEGER, PARAMETER ::     TWO_REACT_REACTIONS =    0

      INTEGER, PARAMETER ::   THREE_REACT_REACTIONS =    0

      INTEGER, PARAMETER ::    ZERO_REACT_REACTIONS =    0

      LOGICAL, PARAMETER ::       UNITARY_REACTIONS = .FALSE.

      INTEGER, PARAMETER ::         ONE_REACT_START =    0

      INTEGER, PARAMETER ::         ONE_REACT_STOP  =   -1

      LOGICAL, PARAMETER ::       BINARY_REACTIONS  = .FALSE.

      INTEGER, PARAMETER ::         TWO_REACT_START =    0

      INTEGER, PARAMETER ::         TWO_REACT_STOP  =   -1

      LOGICAL, PARAMETER ::       TERNARY_REACTIONS = .FALSE.

      INTEGER, PARAMETER ::       THREE_REACT_START =    0

      INTEGER, PARAMETER ::       THREE_REACT_STOP  =   -1

      LOGICAL, PARAMETER ::       NULL_REACTIONS    = .FALSE.

      INTEGER, PARAMETER ::        ZERO_REACT_START =    0

      INTEGER, PARAMETER ::        ZERO_REACT_STOP  =   -1

      INTEGER, PARAMETER ::        NSUNLIGHT_RXNS   =   68

      INTEGER, PARAMETER ::        NTHERMAL_RXNS    =  385

      INTEGER, PARAMETER ::        KUNITS           =    2

      INTEGER  :: IRXXN

      INTEGER, PARAMETER :: NMPHOT =  68
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    8,    9,   21,   27,   28,   38,   43,   47,   50, & ! O   
     &     56,   64,   88,   90,   92,   97,   98,  108,  112,  114, & ! 1   
     &    117,  119,  128,  129,  161,  163,  201,  202,  206,  226, & ! 2   
     &    227,  234,  252,  254,  255,  290,  316,  317,  322,  331, & ! 3   
     &    336,  371,  372,  373,  374,  375,  376,  377,  378,  379, & ! 4   
     &    380,  381,  419,  420,  421,  422,  423,  424,  425,  426, & ! 5   
     &    427,  428,  429,  430,  431,  432,  433,  434/     !  6   

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & ! O   
     &     11,   11,   12,   12,   13,   14,   15,   16,   17,   18, & ! 1   
     &     19,   20,   21,   22,   23,   24,   13,    1,    1,   25, & ! 2   
     &     26,   27,   28,   29,   30,   31,   14,   15,   16,   32, & ! 3   
     &     32,   33,   34,   35,   36,   37,   38,   39,   40,   41, & ! 4   
     &     42,   43,   44,   45,   46,   47,   48,   49,   50,   50, & ! 5   
     &     50,   50,   51,   52,   53,   54,   55,   56/     !  6   

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & ! O   
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & ! 1   
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & ! 2   
     &     31,   32,   33,   34,   35,   36,   37,   38,   39,   40, & ! 3   
     &     41,   42,   43,   44,   45,   46,   47,   48,   49,   50, & ! 4   
     &     51,   52,   53,   54,   55,   56,   57,   58,   59,   60, & ! 5   
     &     61,   62,   63,   64,   65,   66,   67,   68/     !  6   

      REAL( 8 )          :: RTDAT( 3,NRXNS )

      INTEGER, PARAMETER :: NFALLOFF =  28
      REAL( 8 )          :: RFDAT( 5,NFALLOFF )

      INTEGER            :: KTYPE( NRXNS )

      DATA ( KTYPE( IRXXN ), IRXXN = 1, NRXNS ) /  & 
     &      0,    2,    3,   10,    3,   10,    3,    0,    0,    3, & ! O   
     &      1,    3,    4,    3,    3,    4,   10,    3,    9,    9, & ! 1   
     &      0,    1,    3,    3,    3,    3,    0,    0,    3,    3, & ! 2   
     &      1,    1,    1,    1,    3,   10,   10,    0,    1,   10, & ! 3   
     &      1,    1,    0,    3,   10,    8,    0,   10,   10,    0, & ! 4   
     &      3,   10,    3,   10,   10,    0,    3,    3,    3,    6, & ! 5   
     &      3,    6,    6,    0,    6,    6,    6,    3,    3,    3, & ! 6   
     &      3,    3,    3,    6,    3,    3,    6,    6,    6,    6, & ! 7   
     &      6,    6,    6,    6,    6,    6,    3,    0,    3,    0, & ! 8   
     &      1,    0,    1,    3,    3,    3,    0,    0,    3,    1, & ! 9   
     &      3,    3,    1,    3,    3,    3,    3,    0,    3,    3, & ! O   
     &      1,    0,    1,    0,    6,    3,    0,    1,    0,    1, & ! 1   
     &      3,    3,    9,    3,    3,    3,    3,    0,    0,    3, & ! 2   
     &      3,    1,    3,    3,    3,   10,    3,   10,    3,    3, & ! 3   
     &      3,   10,    3,    3,    1,    3,    3,    1,    3,    1, & ! 4   
     &      3,    3,    6,    6,    3,    3,    3,    3,    3,    3, & ! 5   
     &      0,    3,    0,    3,    3,    3,    3,    6,    6,    1, & ! 6   
     &      1,    3,    3,    3,    1,    3,    3,    3,    3,    3, & ! 7   
     &      6,    3,    6,    3,    3,    6,    3,    6,    1,    1, & ! 8   
     &      3,    3,    6,    6,    3,    1,    1,    1,    1,    1, & ! 9   
     &      0,    0,    1,    3,    1,    0,    1,    3,    1,    1, & ! O   
     &      1,    6,    6,    6,    6,    6,    6,    1,    1,    3, & ! 1   
     &     10,    1,   10,    1,    1,    0,    0,    3,    1,    3, & ! 2   
     &      3,    3,    1,    0,    3,    1,    1,    3,    1,    1, & ! 3   
     &      1,    1,    3,    1,    1,    1,    3,    4,    1,    1, & ! 4   
     &      1,    0,   10,    0,    0,   -1,   -1,    3,    3,    3, & ! 5   
     &      3,    3,    3,    1,    1,    1,    3,    3,    3,   -1, & ! 6   
     &     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1, & ! 7   
     &     -1,   -1,   -1,    3,    3,    1,    1,    1,    1,    0, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    1,   -1,    1, & ! 9   
     &     -1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    3,    1,    3,    0,    0,    3,    3,    3, & ! 1   
     &      3,    0,    1,    3,    3,    1,    1,    1,    1,    1, & ! 2   
     &      0,    1,    1,    1,    1,    0,    1,    3,    1,    3, & ! 3   
     &      1,    1,    1,    3,    3,    3,    3,    3,    3,    3, & ! 4   
     &      3,    3,    3,    3,    3,   10,   10,    3,    3,    3, & ! 5   
     &      1,    1,    1,    3,    3,    3,    3,    3,    3,    3, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1, & ! 8   
     &      3,    3,    1,    3,    1,    3,    3,    3,    3,    1, & ! 9   
     &      3,    1,    1,    3,    3,    3,    3,    3,    3,    3, & ! O   
     &      3,    3,    3,   10,   10,   10,    3,    3,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    2,    3,   -1,   -1,   -1,   -1, & ! 3   
     &     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,    3,   10, & ! 4   
     &      3,    3,    3/     !  5   

      INTEGER            :: IRXBITS( NRXNS )

      DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,  276,    0,    1,    0,    1,    0,    2,    2,  260, & ! O   
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
     &      1,    0,    1,    0,    8,    2,    2,    0,    0,    0, & ! 2   
     &      0,    0,    0,    2,   64,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    2,    1,    2,    2,    1,    1,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    0,    0,    0,    0,    0,    0,    2, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    1,    0, & ! 9   
     &      1,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    2,    2,    0,    0,    0, & ! 1   
     &      0,    2,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      2,    0,    0,    0,    0,    2,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,  260,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    1,    1,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    1,    1,    1,    0,    0,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    0,    0,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    0,    1, & ! 4   
     &      0,    0,    0/     !  5   

      INTEGER, PARAMETER :: NTERMS_JACOB =    33124

      INTEGER, PARAMETER :: NSTEPS_JACOB =      906

      INTEGER            :: IORDER( NRXNS )

      DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    2,    2,    2,    2,    2,    1,    1,    2, & ! O   
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
     &      1,    1,    1,    1,    3,    1,    1,    2,    2,    2, & ! 2   
     &      2,    2,    2,    1,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    1,    2,    1,    1,    1,    1,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 6   
     &      1,    1,    1,    1,    2,    2,    2,    1,    1,    2, & ! 7   
     &      1,    1,    1,    2,    2,    2,    2,    2,    2,    1, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    1,    1,    2,    2,    2, & ! 1   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      1,    2,    2,    2,    2,    1,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    3,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    2,    2,    2,    2,    2,    2,    1, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! O   
     &      1,    1,    1,    2,    2,    2,    2,    2,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    2,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2/     !  5   

      INTEGER, PARAMETER :: KTN1 = 107
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     11,   22,   31,   32,   33,   34,   39,   41,   42,   91, & ! O   
     &     93,  100,  103,  111,  113,  118,  120,  132,  145,  148, & ! 1   
     &    150,  170,  171,  175,  189,  190,  196,  197,  198,  199, & ! 2   
     &    200,  203,  205,  207,  209,  210,  211,  218,  219,  222, & ! 3   
     &    224,  225,  229,  233,  236,  237,  239,  240,  241,  242, & ! 4   
     &    244,  245,  246,  249,  250,  251,  264,  265,  266,  286, & ! 5   
     &    287,  288,  289,  291,  292,  293,  294,  295,  296,  297, & ! 6   
     &    298,  300,  302,  303,  304,  305,  306,  307,  308,  309, & ! 7   
     &    310,  311,  312,  314,  323,  326,  327,  328,  329,  330, & ! 8   
     &    332,  333,  334,  335,  337,  339,  341,  342,  343,  361, & ! 9   
     &    362,  363,  393,  395,  400,  402,  403/     !  O   

      INTEGER, PARAMETER :: KTN2 =   2
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &      2,  435/

      INTEGER, PARAMETER :: KTN3 = 172
      INTEGER            :: KRX3( KTN3 )

      DATA ( KRX3( IRXXN ), IRXXN = 1, KTN3 ) / & 
     &      3,    5,    7,   10,   12,   14,   15,   18,   23,   24, & ! O   
     &     25,   26,   29,   30,   35,   44,   51,   53,   57,   58, & ! 1   
     &     59,   61,   68,   69,   70,   71,   72,   73,   75,   76, & ! 2   
     &     87,   89,   94,   95,   96,   99,  101,  102,  104,  105, & ! 3   
     &    106,  107,  109,  110,  116,  121,  122,  124,  125,  126, & ! 4   
     &    127,  130,  131,  133,  134,  135,  137,  139,  140,  141, & ! 5   
     &    143,  144,  146,  147,  149,  151,  152,  155,  156,  157, & ! 6   
     &    158,  159,  160,  162,  164,  165,  166,  167,  172,  173, & ! 7   
     &    174,  176,  177,  178,  179,  180,  182,  184,  185,  187, & ! 8   
     &    191,  192,  195,  204,  208,  220,  228,  230,  231,  232, & ! 9   
     &    235,  238,  243,  247,  258,  259,  260,  261,  262,  263, & ! O   
     &    267,  268,  269,  284,  285,  313,  315,  318,  319,  320, & ! 1   
     &    321,  324,  325,  338,  340,  344,  345,  346,  347,  348, & ! 2   
     &    349,  350,  351,  352,  353,  354,  355,  358,  359,  360, & ! 3   
     &    364,  365,  366,  367,  368,  369,  370,  391,  392,  394, & ! 4   
     &    396,  397,  398,  399,  401,  404,  405,  406,  407,  408, & ! 5   
     &    409,  410,  411,  412,  413,  417,  418,  436,  449,  451, & ! 6   
     &    452,  453/     !  7   

      INTEGER, PARAMETER :: KTN4 =   3
      INTEGER            :: KRX4( KTN4 )

      DATA ( KRX4( IRXXN ), IRXXN = 1, KTN4 ) / & 
     &     13,   16,  248/

      INTEGER, PARAMETER :: KTN5 =   0
      INTEGER            :: KRX5( 1 )

      DATA   KRX5( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN6 =  34
      INTEGER            :: KRX6( KTN6 )

      DATA ( KRX6( IRXXN ), IRXXN = 1, KTN6 ) / & 
     &     60,   62,   63,   65,   66,   67,   74,   77,   78,   79, & 
     &     80,   81,   82,   83,   84,   85,   86,  115,  153,  154, & 
     &    168,  169,  181,  183,  186,  188,  193,  194,  212,  213, & 
     &    214,  215,  216,  217/

      INTEGER, PARAMETER :: KTN7 =   0
      INTEGER            :: KRX7( 1 )

      DATA   KRX7( 1 ) / 0 /

      INTEGER, PARAMETER :: NWM =   3
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &      2,   10,  344/
      REAL( 8 ),    PARAMETER :: ATM_AIR = 1.00000D+06

      INTEGER, PARAMETER :: NWW =   5
      INTEGER            :: NRXWW( NWW )

      DATA ( NRXWW( IRXXN ), IRXXN = 1, NWW ) / & 
     &     11,   20,   39,   41,  225/

      INTEGER, PARAMETER :: NWO2 =   3
      INTEGER            :: NRXWO2( NWO2 )

      DATA ( NRXWO2( IRXXN ), IRXXN = 1, NWO2 ) / & 
     &      2,   24,  134/
      REAL( 8 ),    PARAMETER :: ATM_O2 = 2.09500D+05

      INTEGER, PARAMETER :: NWN2 =   0
      INTEGER            :: NRXWN2( 1 )

      DATA   NRXWN2( 1 ) / 0 /
      REAL( 8 ),    PARAMETER :: ATM_N2 = 7.80800D+05

      INTEGER, PARAMETER :: NWCH4 =   2
      INTEGER            :: NRXWCH4( NWCH4 )

      DATA ( NRXWCH4( IRXXN ), IRXXN = 1, NWCH4 ) / & 
     &    124,  235/
      REAL( 8 ),    PARAMETER :: ATM_CH4 = 1.85000D+00

      INTEGER, PARAMETER :: NWH2 =   1
      INTEGER            :: NRXWH2( NWH2 )

      DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) / & 
     &    122/
      REAL( 8 ),    PARAMETER :: ATM_H2 = 5.60000D-01

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
     &     48,   48,   49,   49,    1,   88,   90,   89,   91,   91, & ! 2   
     &     91,   91,    7,   92,   89,   89,   89,   89,   89,   89, & ! 3   
     &     89,   89,   89,   89,   89,   89,   89,   93,   89,   89, & ! 4   
     &     89,   94,   91,   95,   95,   95,   95,   76,   76,   80, & ! 5   
     &     80,   73,   73,  100,  100,  100,   82,   82,  102,   34, & ! 6   
     &     10,   10,  103,  104,  103,  103,  104,    1,   63,  108, & ! 7   
     &    108,   40,   41,   67,   67,   67,   67,  112,  112,  112, & ! 8   
     &    113,  115,  117,  118,  119,  121,  122,  123,  124,  125, & ! 9   
     &    126,  127,  129,  130,  131,  132,  133,  134,  135,  138, & ! O   
     &    136,  137,  139,  139,  139,  139,  139,  139,  140,  140, & ! 1   
     &    140,  140,  140,  141,  141,  141,  141,  143,  143,  143, & ! 2   
     &    143,  143,  142,  142,  142,  142,  142,  144,  144,  145, & ! 3   
     &    145,  145,  145,  145,  148,  149,  148,  151,  149,  149, & ! 4   
     &    149,  148,  152,  149,  148,  149,  148,  149,   27,   24, & ! 5   
     &     53,   55,  155,  149,  156,  157,  158,  159,  160,  161, & ! 6   
     &    152,  150,  149,  154,  153,  153,  163,  155,  156,  158, & ! 7   
     &    159,  153,  153,  150,  150,  153,  153,  154,  154,  151, & ! 8   
     &    165,  165,  168,  167,  169,  166,  166,  170,  171,  168, & ! 9   
     &    172,  165,  166,  166,  166,  173,  173,  166,  166,  175, & ! O   
     &    174,  171,  172,  165,  165,  166,  177,  161,  168,  169, & ! 1   
     &    166,  173,  170,  171,  172,  175,  176,  174,  178,  179, & ! 2   
     &    177,  180,  181,  182,  169,  175,  175,  175,  176,  176, & ! 3   
     &    174,  174,  172,  172,  171,  171,  169,  169,  161,  161, & ! 4   
     &    161,  161,  161/     !  5   

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
     &      0,    0,    0,    0,    7,    0,    0,    4,   91,    2, & ! 2   
     &      8,   18,   92,    0,    0,   45,   47,   42,   52,   53, & ! 3   
     &     54,   55,   27,   24,   38,   29,   43,    7,   74,   78, & ! 4   
     &     81,    0,    1,    0,    0,    0,    0,    2,    8,    2, & ! 5   
     &      8,    2,    8,    4,    7,    5,    2,    8,    7,    0, & ! 6   
     &      0,    0,    0,    0,  105,  106,  107,    0,    0,  109, & ! 7   
     &      0,    0,    0,    2,    8,    5,   19,   89,    7,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    7,    7,    7, & ! 9   
     &      7,    7,    7,    7,    7,    7,    7,    7,    7,    7, & ! O   
     &      7,    7,    7,    5,    3,    0,    0,   89,    7,    5, & ! 1   
     &      3,    0,   89,    7,    4,    5,   89,    7,    4,    5, & ! 2   
     &      0,   89,    7,    4,    5,    0,   89,    7,   89,    4, & ! 3   
     &     88,    9,    7,   89,    4,    8,    8,    7,  149,  149, & ! 4   
     &      2,  153,    7,    7,    5,    1,    1,   91,  148,  148, & ! 5   
     &    148,  148,    7,   18,    7,    7,    7,    7,    7,  149, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,  106,  164,  106,  164,  106,  164,    0, & ! 8   
     &      4,    8,    7,    7,    7,    8,    2,  170,  171,    5, & ! 9   
     &    165,  149,  148,  149,   91,  173,    2,  166,  173,    0, & ! O   
     &      0,    0,    0,    2,    1,    1,    7,  166,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    5,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,  106,  164,  106,  164,  106,  164,    7,    7, & ! 4   
     &      5,   89,   91/     !  5   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0/     !  5   

      DATA ( IRR( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &      2,    4,    1,    1,    2,    5,    0,    3,    6,    3, & ! O   
     &      7,    8,    7,    8,    7,    3,    9,    0,    9,    9, & ! 1   
     &      7,    8,    7,    1,    7,    5,    1,    2,    1,    2, & ! 2   
     &      1,    8,    7,    1,    1,   10,    5,    1,   11,   12, & ! 3   
     &     12,    2,    2,    1,   11,    5,    7,   13,    8,    8, & ! 4   
     &      1,   15,    1,   20,    1,    1,   21,   18,   18,   18, & ! 5   
     &      1,   26,    1,    1,   21,   18,   18,    2,    8,    0, & ! 6   
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
     &     32,   46,   32,   38,   11,   89,    7,   91,   88,   89, & ! 2   
     &     90,   89,   89,   89,   93,   93,   93,   93,   92,   92, & ! 3   
     &     93,   92,   93,   93,   93,   93,   93,   89,   93,   93, & ! 4   
     &     93,   89,   95,   91,   89,   90,   90,    2,    8,    2, & ! 5   
     &      8,    2,    8,    4,    7,    5,    2,    8,    7,   11, & ! 6   
     &     11,   11,   11,   11,   94,   94,   94,   12,  108,  110, & ! 7   
     &    110,  111,  111,    2,    8,    5,   19,   89,    7,    0, & ! 8   
     &    114,  116,  116,  116,  120,  120,  120,  124,    7,  126, & ! 9   
     &      7,    7,    7,    7,    7,    7,    7,    7,    7,    7, & ! O   
     &      7,    7,    7,    5,    3,    0,    0,   89,    7,    5, & ! 1   
     &      3,    0,   89,    7,    4,    5,   89,    7,    4,    5, & ! 2   
     &      0,   89,    7,    4,    5,    0,   89,    7,   89,  146, & ! 3   
     &    147,  147,  146,  145,  149,  150,  151,  148,  148,  152, & ! 4   
     &    148,  152,  150,  148,  149,  153,  154,  148,  151,  151, & ! 5   
     &    155,  155,  148,  150,  148,  148,  148,  148,  148,   14, & ! 6   
     &    148,    7,  148,  148,  149,  148,  148,  148,  148,  148, & ! 7   
     &    148,  150,  150,  163,  152,  163,  152,  163,  152,  164, & ! 8   
     &    166,  167,  169,  165,  166,  169,  165,  168,  168,  165, & ! 9   
     &    168,  166,  165,  148,  165,  174,  166,  173,  176,  173, & ! O   
     &    173,  165,  166,  170,  171,  172,  165,   14,  165,  165, & ! 1   
     &    165,  165,  165,  165,  165,  165,  166,  173,  165,  165, & ! 2   
     &    165,  165,  165,  165,  166,  166,    0,    0,    0,    0, & ! 3   
     &      0,    0,  178,  179,  178,  179,  178,  179,   14,   14, & ! 4   
     &     14,   14,   14/     !  5   

      DATA ( IRR( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &      3,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    8,    0,    1,    0,    3,    0,    0,    1, & ! 2   
     &      0,    1,    1,    0,    0,    0,    1,    5,    0,    0, & ! 3   
     &      0,    1,    7,    0,    0,    0,    1,    0,    1,    1, & ! 4   
     &      0,    8,   18,    0,   17,   17,   22,    0,   19,   24, & ! 5   
     &     24,    0,   23,   23,   22,    0,   19,    0,    0,    0, & ! 6   
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
     &     19,   38,   19,   50,    0,    0,   89,    0,   89,    1, & ! 2   
     &      0,   27,   36,   36,   18,   49,   46,   24,   31,   24, & ! 3   
     &     92,   58,    8,   17,   23,    8,    8,    0,   70,   70, & ! 4   
     &     70,    1,    0,    1,    5,   11,   11,   96,   99,   96, & ! 5   
     &     99,   96,   99,  101,  101,  101,   96,   99,   96,    0, & ! 6   
     &    103,  104,    0,    0,    0,    0,    0,   11,    0,    0, & ! 7   
     &      0,    0,    0,  112,  112,  112,  112,  112,  112,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,  123,    0,  125, & ! 9   
     &      0,  128,  129,  129,  129,  129,  129,  134,  134,  134, & ! O   
     &    134,  134,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,  142,  142,  142,  142,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  147, & ! 3   
     &     88,    9,  147,  147,    0,    0,    0,    0,    0,    0, & ! 4   
     &      1,    5,  148,    8,    1,    0,    0,   89,    8,   17, & ! 5   
     &     24,   58,   36,  148,   36,    8,   89,   89,   89,  162, & ! 6   
     &      0,  148,    3,    1,    1,    5,   89,   36,    8,   89, & ! 7   
     &     89,   11,   11,    0,    0,   11,   11,   12,   12,    0, & ! 8   
     &      0,    0,  165,    0,    0,    0,    1,    2,    1,  172, & ! 9   
     &      5,  148,  149,  165,   89,    0,    1,  165,    0,  165, & ! O   
     &      0,    1,    1,    0,    0,    0,   27,  162,    0,    7, & ! 1   
     &      3,    0,    2,    1,    5,  173,  173,    0,   89,  148, & ! 2   
     &     18,   27,  148,   89,   11,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   11,   11,   12,   12,    0,    0,   18,  162, & ! 4   
     &     11,   18,  162/     !  5   

      DATA ( IRR( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    7, & ! 4   
     &      0,   16,   19,    0,    0,    5,    4,    0,    0,   25, & ! 5   
     &     25,    0,    0,    5,    4,    0,    0,    0,    0,    0, & ! 6   
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
     &      0,    8,    0,    8,   19,    0,   25,   25,    8,   38, & ! 3   
     &     24,   25,   36,    0,    0,   27,   24,    0,   75,   79, & ! 4   
     &     79,    0,    0,    0,    0,    0,    0,   97,    0,   97, & ! 5   
     &      0,   98,    0,    0,    0,    0,   97,    0,   97,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    7,    0,    7, & ! 9   
     &      0,    0,  130,  130,  130,  130,  130,  135,  135,  135, & ! O   
     &    135,  135,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    4, & ! 3   
     &      0,    0,    7,   89,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,   36,    0, & ! 5   
     &     25,   25,    0,   27,    0,   36,   36,   36,   36,   18, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    8,   36,    8, & ! 7   
     &      8,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,  175,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,   18,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   27,   27,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,   27,   18, & ! 4   
     &     18,   27,   18/     !  5   

      DATA ( IRR( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    5, & ! 4   
     &      0,    0,    0,    0,    0,   18,   18,    0,    0,   19, & ! 5   
     &     19,    0,    0,   24,    7,    0,    0,    0,    0,    0, & ! 6   
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
     &      0,    8,    0,    0,    7,   19,   22,   34,    0,    0, & ! 1   
     &      0,   25,    0,   31,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,   32,   32,   27,   31, & ! 3   
     &     38,   32,    0,    0,    0,    0,    0,    0,   19,   19, & ! 4   
     &     19,    0,    0,    0,    0,    0,    0,   98,    0,   98, & ! 5   
     &      0,    0,    0,    0,    0,    0,   98,    0,   98,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,  131,  131,  131,  131,  131,  138,  138,  138, & ! O   
     &    138,  138,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     45,   32,    0,    0,    0,    0,    0,    0,    8,  148, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   36, & ! 7   
     &     36,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,  165,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &     27,   93,   89/     !  5   

      DATA ( IRR( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,   19,   19,    0,    0,    0, & ! 5   
     &      0,    0,    0,   25,   18,    0,    0,    0,    0,    0, & ! 6   
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
     &      0,   23,    0,    0,   18,    0,    0,    0,    0,    0, & ! 1   
     &      0,   19,    0,   19,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,   19,   19,    0,    8, & ! 3   
     &     53,   19,    0,    0,    0,    0,    0,    0,   72,   77, & ! 4   
     &     77,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,  132,  132,  132,  132,  134,  136,  136,  136, & ! O   
     &    136,  136,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     19,   19,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,  162,    0/     !  5   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   45, & ! 3   
     &     45,    0,    0,    0,    0,    0,    0,    0,    7,    7, & ! 4   
     &      7,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,  133,  134,  134,  134,  135,  137,  137,  137, & ! O   
     &    137,  137,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
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
     &      0,   91,    0/     !  5   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &     31,    0,    0,    0,    0,    0,    0,    0,   25,   25, & ! 4   
     &     25,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,  134,  135,  135,  135,    0,    0,    0,    0, & ! O   
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
     &      0,    0,    0/     !  5   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      8,    0,    0,    0,    0,    0,    0,    0,    8,    8, & ! 4   
     &      8,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,  135,    0,    0,    0,    0,    0,    0,    0, & ! O   
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
     &      0,    0,    0/     !  5   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,   76,   80, & ! 4   
     &     82,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,  136,    0,    0,    0,    0,    0,    0,    0, & ! O   
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
     &      0,    0,    0/     !  5   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,  137,    0,    0,    0,    0,    0,    0,    0, & ! O   
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
     &      0,    0,    0/     !  5   

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
     &      0,    0,    0/     !  5   

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
     &      0,    0,    0/     !  5   

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
     &      0,    0,    0/     !  5   

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
     &      0,    0,    0/     !  5   

      DATA ( RTDAT( 1,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 6.0000D-34, 2.0700D-12, 1.0000D-31, 5.1000D-12, & ! O   
     &     1.3000D-31, 8.0000D-12, 1.0000D+00, 1.0000D+00, 2.2300D-11, & ! +   
     &     2.1400D-10, 1.7000D-12, 2.0300D-16, 2.4000D-11, 3.0000D-11, & ! 1   
     &     6.2000D-14, 9.0000D-31, 4.8000D-11, 2.2000D-13, 3.0800D-34, & ! +   
     &     1.0000D+00, 1.8000D-12, 1.4000D-12, 4.2500D-39, 3.4500D-12, & ! 2   
     &     1.4000D-13, 1.0000D+00, 1.0000D+00, 1.8000D-11, 4.5000D-14, & ! +   
     &     1.7000D-11, 2.0000D-11, 4.0000D-12, 1.0000D-17, 8.5000D-13, & ! 3   
     &     3.6000D-30, 1.3000D-03, 1.0000D+00, 1.0000D-22, 7.4000D-31, & ! +   
     &     5.0000D-40, 1.0000D-20, 1.0000D+00, 2.5000D-12, 1.8000D-30, & ! 4   
     &     2.4000D-14, 1.0000D+00, 1.4000D-31, 4.1000D-05, 1.0000D+00, & ! +   
     &     3.2000D-13, 2.8000D-31, 7.5000D-12, 3.6100D-28, 1.1000D-05, & ! 5   
     &     1.0000D+00, 3.1400D-12, 4.4000D-13, 2.9000D-12, 1.0000D+00, & ! +   
     &     6.7000D-12, 8.4000D-01, 8.4000D-01, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 2.4000D-12, 4.8000D-13, 6.5000D-14, & ! +   
     &     2.3000D-12, 3.8000D-13, 2.0000D-12, 1.0000D+00, 2.7000D-12, & ! 7   
     &     6.8000D-13, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 5.3000D-12, 1.0000D+00, 5.3000D-12, 1.0000D+00, & ! +   
     &     2.0000D-12, 1.0000D+00, 4.5000D-13, 4.0000D-14, 5.3000D-12, & ! 9   
     &     5.4000D-12, 1.0000D+00, 1.0000D+00, 3.4000D-11, 5.5000D-16, & ! +   
     &     9.7000D-15, 2.4000D+12, 5.6000D-12, 5.6000D-15, 1.8000D-11, & ! O   
     &     4.7000D-12, 1.4000D-12, 1.0000D+00, 1.3000D-11, 4.9000D-12, & ! +   
     &     6.3000D-15, 1.0000D+00, 8.0000D-12, 1.0000D+00, 1.0000D+00, & ! 1   
     &     3.1000D-12, 1.0000D+00, 4.0000D-16, 1.0000D+00, 5.0000D-16, & ! +   
     &     1.9000D-12, 7.7000D-12, 1.4400D-13, 1.8500D-12, 6.9000D-12, & ! 2   
     &     2.8500D-12, 3.0000D-12, 1.0000D+00, 1.0000D+00, 1.4100D-12, & ! +   
     &     7.6000D-12, 8.1000D-13, 5.7000D+12, 1.5000D-14, 8.6000D-12, & ! 3   
     &     5.0000D-30, 1.0400D-11, 8.6000D-29, 6.8200D-15, 3.3000D-12, & ! +   
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
     &     1.7000D-10, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 3.6000D-11, 3.0000D-12, 1.8500D-12, & ! +   
     &     2.3700D-21, 1.0000D+00, 4.8100D-20, 1.0000D+00, 1.1000D-30, & ! 2   
     &     1.0000D+00, 1.0000D+00, 2.3000D-11, 1.6300D-14, 6.4000D-12, & ! +   
     &     2.2000D-12, 3.2000D-12, 5.0000D-13, 1.0000D+00, 6.6000D-12, & ! 3   
     &     5.0000D-11, 1.4000D-10, 8.3000D-11, 1.0700D-10, 2.5000D-10, & ! +   
     &     3.5000D-10, 4.3000D-10, 8.2000D-11, 7.9000D-11, 1.3000D-10, & ! 4   
     &     5.5000D-11, 8.2000D-11, 6.5800D-13, 6.1000D-11, 1.2000D-10, & ! +   
     &     1.2000D-10, 1.0000D+00, 1.8000D-31, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 2.7000D-12, 1.9000D-13, 2.7000D-12, & ! +   
     &     1.9000D-13, 2.7000D-12, 1.9000D-13, 1.1600D-14, 1.9700D-10, & ! 6   
     &     1.9000D-11, 2.7000D-12, 1.9000D-13, 2.7000D-12, 1.4000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.6000D-12, 2.6500D-13, & ! 8   
     &     2.3000D-12, 3.5000D-14, 1.9200D-10, 7.2000D-12, 1.0000D+00, & ! +   
     &     9.2590D-05, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 9   
     &     9.4882D-06, 9.4882D-06, 2.5000D-12, 1.0000D+00, 2.5000D-12, & ! +   
     &     1.0000D+00, 1.2500D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, & ! O   
     &     4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, & ! +   
     &     4.0000D-11, 4.0000D-11, 5.4000D-12, 5.5000D-16, 3.4000D-11, & ! 1   
     &     1.0000D+00, 1.0000D+00, 8.2000D-11, 4.7000D-12, 1.4000D-12, & ! +   
     &     1.8000D-11, 1.0000D+00, 7.9000D-11, 1.4800D-11, 1.3400D-14, & ! 2   
     &     1.7900D-13, 2.5100D-10, 2.0000D-11, 2.6100D-19, 1.1500D-15, & ! +   
     &     1.0000D+00, 2.3700D-10, 2.0000D-11, 2.6100D-19, 1.1500D-15, & ! 3   
     &     1.0000D+00, 2.3700D-10, 1.8000D-12, 6.1000D-11, 2.1100D-18, & ! +   
     &     2.6000D-18, 8.5000D-19, 7.7000D-14, 2.2500D-33, 1.6000D-11, & ! 4   
     &     4.5000D-12, 4.8000D-12, 6.7000D-12, 1.4000D-12, 2.9000D-14, & ! +   
     &     8.8000D-12, 4.9000D-11, 2.1000D-11, 1.7000D-11, 1.6000D-11, & ! 5   
     &     5.2000D-31, 4.2000D-31, 4.7000D-12, 1.7000D-11, 1.3000D-11, & ! +   
     &     3.6000D-12, 5.0000D-12, 5.0000D-12, 2.7000D-14, 1.0000D-12, & ! 6   
     &     2.0000D-12, 9.0000D-13, 9.4000D-13, 2.1000D-12, 1.5000D-14, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     2.1000D-11, 1.5000D-11, 2.1000D-10, 1.6000D-11, 5.0000D-12, & ! 9   
     &     1.4000D-11, 7.1500D-12, 8.4000D-11, 4.7000D-13, 1.5000D-12, & ! +   
     &     9.1000D-11, 1.2000D-11, 2.7000D-11, 1.5000D-11, 4.7000D-12, & ! O   
     &     1.5000D-10, 1.1000D-12, 5.4000D-11, 1.5000D-10, 2.5000D+14, & ! +   
     &     3.8000D-02, 9.9400D+17, 2.1000D+15, 1.8000D-32, 3.0000D-31, & ! 1   
     &     7.7000D-31, 4.3000D-12, 3.3000D-13, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 2.7000D-12, & ! 3   
     &     1.0000D+12, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.1200D-11, 1.9900D-39, & ! +   
     &     1.9000D-13, 3.4000D-13, 1.7000D-15/           !        5   

      DATA ( RTDAT( 2,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00,-2.6000D+00, 0.0000D+00,-1.6000D+00, 0.0000D+00, & ! O   
     &    -1.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.5700D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     2.6000D+00,-3.2000D+00, 0.0000D+00, 6.0000D+02, 2.8000D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &    -4.1000D+00,-3.5000D+00, 0.0000D+00, 0.0000D+00,-2.4000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-3.0000D+00, & ! 4   
     &     4.6000D+02, 0.0000D+00,-3.1000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-2.6000D+00, 0.0000D+00,-6.8700D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 5.9000D+01, & ! +   
     &     0.0000D+00, 5.4000D+01, 5.5000D+01, 0.0000D+00, 5.7000D+01, & ! 6   
     &     5.8000D+01, 5.9000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 7.0000D+01, 0.0000D+00, & ! 7   
     &     0.0000D+00, 5.8000D+01, 7.0000D+01, 7.5000D+01, 7.6000D+01, & ! +   
     &     5.8000D+01, 7.0000D+01, 7.5000D+01, 7.6000D+01, 5.8000D+01, & ! 8   
     &     7.0000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0700D+02, & ! 1   
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
     &     0.0000D+00, 6.1000D+01, 6.2000D+01, 6.3000D+01, 5.7000D+01, & ! 1   
     &     5.9000D+01, 5.8000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 1.1600D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-3.4000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
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
     &    -3.2000D+00,-2.4000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.0000D+00,-1.0000D+00, & ! 1   
     &    -5.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-2.6600D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        5   

      DATA ( RTDAT( 3,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00,-1.4000D+03, 0.0000D+00, 1.9800D+02, & ! O   
     &     0.0000D+00,-2.0600D+03, 0.0000D+00, 0.0000D+00, 1.1500D+02, & ! +   
     &     0.0000D+00,-9.4000D+02, 6.9300D+02, 1.1000D+02, 2.0000D+02, & ! 1   
     &     9.4500D+02, 0.0000D+00, 2.5000D+02, 1.9000D-33, 2.6600D-54, & ! +   
     &     0.0000D+00, 0.0000D+00,-2.0000D+03, 6.6400D+02, 2.7000D+02, & ! 2   
     &    -2.4700D+03, 0.0000D+00, 0.0000D+00, 1.1000D+02,-1.2600D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-2.4500D+03, & ! 3   
     &     0.0000D+00,-1.1000D+04, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.6000D+02, 0.0000D+00, & ! 4   
     &     2.7000D-17, 0.0000D+00, 0.0000D+00,-1.0650D+04, 0.0000D+00, & ! +   
     &     6.9000D+02, 0.0000D+00, 2.9000D+02, 0.0000D+00,-1.0100D+04, & ! 5   
     &     0.0000D+00, 5.8000D+02, 1.0700D+03, 5.0000D+02, 0.0000D+00, & ! +   
     &     3.4000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 3.6000D+02, 8.0000D+02, 5.0000D+02, & ! +   
     &     3.6000D+02, 7.8000D+02, 5.0000D+02, 0.0000D+00, 3.6000D+02, & ! 7   
     &     8.0000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 1.9000D+02, 0.0000D+00, 1.9000D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 8.5000D+02, 1.9000D+02, & ! 9   
     &     1.3500D+02, 0.0000D+00, 0.0000D+00,-1.6000D+03, 0.0000D+00, & ! +   
     &     6.2500D+02,-7.0000D+03, 0.0000D+00, 2.3000D+03,-1.1000D+03, & ! O   
     &     3.4500D+02,-1.8600D+03, 0.0000D+00,-8.7000D+02, 4.0500D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     3.4000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     5.7500D+02,-2.1000D+03, 3.4300D-33,-1.6900D+03,-1.0000D+03, & ! 2   
     &    -3.4500D+02, 2.0000D+01, 0.0000D+00, 0.0000D+00,-6.2060D+02, & ! +   
     &    -5.8500D+02, 0.0000D+00,-5.7800D+03,-2.0000D+02, 4.0000D+02, & ! 3   
     &     0.0000D+00,-7.9200D+02, 0.0000D+00,-2.5000D+03,-2.8800D+03, & ! +   
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
     &     0.0000D+00, 0.0000D+00,-2.0000D+02, 0.0000D+00, 2.9000D+02, & ! +   
     &     3.4000D+02,-1.1000D+02, 0.0000D+00, 0.0000D+00,-1.2400D+03, & ! 3   
     &     0.0000D+00, 0.0000D+00,-1.0000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-3.4000D+01, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 4.5000D+01, 5.8000D+01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 3.6000D+02, 1.3000D+03, 3.6000D+02, & ! +   
     &     1.3000D+03, 3.6000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 3.6000D+02, 1.3000D+03, 3.7400D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.8000D+02, 1.3000D+03, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.3500D+02, 0.0000D+00,-1.6000D+03, & ! 1   
     &     0.0000D+00, 0.0000D+00,-3.4000D+01, 3.4500D+02,-1.8600D+03, & ! +   
     &    -1.1000D+03, 0.0000D+00, 0.0000D+00, 4.4800D+02,-2.2830D+03, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 3.4000D+02, 0.0000D+00,-1.2565D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 6.8000D+02,-7.8000D+02, & ! 4   
     &     4.6000D+02,-3.1000D+02, 1.5500D+02, 2.1000D+02, 8.4000D+02, & ! +   
     &     2.6000D+02, 0.0000D+00, 2.4000D+02, 2.5000D+02, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 3.2000D+02,-8.0000D+02,-3.6000D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.6000D+03,-3.8800D+02, & ! 6   
     &    -8.4000D+02,-4.2000D+02,-5.1000D+02,-8.8000D+02, 1.0000D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -8.3000D+02,-1.0900D+03, 0.0000D+00, 4.4000D+02, 0.0000D+00, & ! 9   
     &     5.4000D+02, 3.0000D+02,-2.6200D+03,-1.6700D+03, 0.0000D+00, & ! +   
     &    -1.4600D+02, 0.0000D+00, 0.0000D+00, 5.1000D+02, 2.8000D+02, & ! O   
     &     0.0000D+00, 5.4200D+02, 1.8000D+02, 0.0000D+00,-9.7700D+03, & ! +   
     &     0.0000D+00,-1.1859D+04,-1.3670D+04, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00,-1.1200D+03,-9.2500D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &    -9.7700D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-2.5000D+02, 5.2700D+03, & ! +   
     &     5.2000D+02, 2.0810D+03, 3.4000D+02/           !        5   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &      4,    6,   17,   19,   20,   36,   37,   40,   45,   46, & 
     &     48,   49,   52,   54,   55,  123,  136,  138,  142,  221, & 
     &    223,  253,  356,  357,  414,  415,  416,  450/

      DATA ( RFDAT( 1,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     5.0000D-11, 2.3000D-11, 3.9000D-11, 9.8000D+02, 3.1800D+03, & 
     &     1.9000D-12, 9.7000D+14, 3.3000D-11, 2.8000D-11, 2.1990D+03, & 
     &     4.0000D-12, 6.0000D+15, 2.0000D-12, 1.2400D-11, 1.9000D+17, & 
     &     0.0000D+00, 1.0000D-12, 9.0000D-12, 3.0000D-11, 4.0700D-01, & 
     &     4.0700D-01, 1.5000D-11, 6.9000D-12, 2.7000D-11, 1.7000D-11, & 
     &     6.6000D-11, 1.6000D-11, 1.2600D-10/

      DATA ( RFDAT( 2,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     3.0000D-01, 2.4000D-01,-4.7000D-01, 0.0000D+00, 0.0000D+00, & 
     &     2.0000D-01, 1.0000D-01,-3.0000D-01, 0.0000D+00, 6.5000D-34, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.1050D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00,-8.5000D-01,-1.0000D+00,-8.0000D+00, & 
     &    -8.0000D+00,-1.9000D+00,-2.9000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( RFDAT( 3,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00,-1.1080D+04, 0.0000D+00, 0.0000D+00, 1.3350D+03, & 
     &     0.0000D+00,-1.1170D+04, 0.0000D+00, 0.0000D+00,-1.4100D+04, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00,-3.4000D+02/

      DATA ( RFDAT( 4,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     8.5000D-01, 6.0000D-01, 4.2000D-01, 0.0000D+00, 0.0000D+00, & 
     &     3.5000D-01, 3.5000D-01, 8.1000D-01, 6.0000D-01, 0.0000D+00, & 
     &     4.0000D-01, 4.0000D-01, 5.3000D-01, 3.0000D-01, 3.0000D-01, & 
     &     0.0000D+00, 3.7000D-01, 4.8000D-01, 5.0000D-01, 4.1000D-01, & 
     &     4.1000D-01, 6.0000D-01, 6.0000D-01, 6.0000D-01, 6.0000D-01, & 
     &     6.3000D-01, 4.0000D-01, 1.0000D+00/

      DATA ( RFDAT( 5,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     8.4000D-01, 1.0300D+00, 1.2300D+00, 0.0000D+00, 0.0000D+00, & 
     &     1.3300D+00, 1.3300D+00, 8.7000D-01, 1.0000D+00, 0.0000D+00, & 
     &     1.2600D+00, 1.2600D+00, 1.1000D+00, 1.4100D+00, 1.4100D+00, & 
     &     0.0000D+00, 1.3000D+00, 1.1500D+00, 1.1300D+00, 1.0000D+00, & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00/

      REAL( 8 )               :: SC( NRXNS,MXPRD )

      DATA ( SC( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! +   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.9000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     6.0000D-01, 3.7000D-01, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 6.0000D-01, 3.7000D-01, & ! 6   
     &     1.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 9.0000D-01, 1.0000D+00, 6.8500D-01, 1.0000D+00, & ! 7   
     &     1.0000D+00, 8.0000D-01, 6.0000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     8.0000D-01, 1.0000D+00, 5.0000D-01, 1.0000D+00, 8.0000D-01, & ! 8   
     &     1.0000D+00, 6.0000D-01, 1.0000D+00, 5.4000D-01, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 5.0000D-01, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D-01, 7.4000D-01, 1.0000D+00, & ! 1   
     &     1.8000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 9.9100D-01, & ! 2   
     &     1.0000D+00, 9.5000D-01, 5.0000D-01, 3.8000D-01, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D-01, 1.0000D+00, 1.0000D+00, & ! 3   
     &     7.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.0000D-01, & ! +   
     &     2.0000D-01, 7.8100D-01, 2.9500D-01, 5.0000D-01, 1.2400D+00, & ! 4   
     &     1.3000D+00, 7.3200D-01, 5.0000D-01, 1.0000D+00, 7.5000D-01, & ! +   
     &     1.0000D-01, 8.8000D-01, 5.9800D-01, 5.9800D-01, 1.0000D+00, & ! 5   
     &     6.0000D-01, 3.5000D-01, 2.2000D-02, 4.0000D-02, 7.1700D-01, & ! +   
     &     7.6000D-01, 9.0400D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     2.7500D-01, 2.7500D-01, 2.2000D-01, 2.7500D-01, 6.3000D-01, & ! +   
     &     1.5000D-01, 7.5000D-01, 5.7000D-01, 4.7000D-01, 1.5000D-01, & ! 7   
     &     7.5000D-01, 5.7000D-01, 4.7000D-01, 5.3000D-01, 9.1800D-01, & ! +   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.8000D-01, 8.6000D-01, & ! 8   
     &     4.8000D-01, 0.0000D+00, 4.8000D-01, 1.5500D-01, 1.5500D-01, & ! +   
     &     8.6000D-01, 0.0000D+00, 2.6000D-01, 2.6000D-01, 2.5000D-02, & ! 9   
     &     3.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 4.0000D-01, 1.0000D+00, 1.2000D+00, 5.0000D-01, & ! O   
     &     1.0000D+00, 6.0000D-01, 1.4000D+00, 1.0000D+00, 1.4000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 3.7000D-01, & ! 1   
     &     1.0000D+00, 8.0000D-01, 5.0000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 7.3200D-01, 1.0000D+00, 1.2600D-01, 1.0000D+00, & ! 2   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 3.0000D-01, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     3.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 5.0000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 9.0700D-01, 9.2500D-01, 9.4300D-01, 5.0000D-01, & ! 9   
     &     5.0000D-01, 1.5000D+00, 1.2500D+00, 1.0000D+00, 1.2500D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.0000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 5.0000D-01, 5.0000D-01, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 8.0000D-01, 3.0000D+00, & ! 6   
     &     2.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, 7.5000D-01, & ! +   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 3.0000D+00, 2.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 4.0000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 7.5000D-01, 2.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     2.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 7.5000D-01, & ! +   
     &     1.0000D+00, 8.6000D-01, 7.5000D-01/           !        5   

      DATA ( SC( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! 2   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 5.9000D-01, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! 5   
     &     6.0000D-01, 1.3000D-01, 0.0000D+00, 2.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 6.0000D-01, 1.3000D-01, & ! 6   
     &     0.0000D+00, 2.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D-01, 9.0000D-01, 3.1500D-01, 1.0000D+00, & ! 7   
     &     0.0000D+00, 8.0000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.0000D-01, 0.0000D+00, 5.0000D-01, 0.0000D+00, 8.0000D-01, & ! 8   
     &     0.0000D+00, 6.0000D-01, 1.0000D+00, 6.0000D-02, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 5.0000D-01, 1.0000D+00, & ! O   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D-01, 8.9000D-01, 1.0000D+00, & ! 1   
     &     2.0000D-01, 2.0000D+00, 1.5000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 9.9100D-01, & ! 2   
     &     1.0000D+00, 9.0000D-01, 5.0000D-01, 1.3800D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.2000D-01, 1.0000D+00, 0.0000D+00, & ! 3   
     &     7.0000D-01, 1.0000D+00, 1.0000D+00, 3.5000D-01, 5.0000D-01, & ! +   
     &     3.0000D-01, 4.8800D-01, 5.5500D-01, 5.0000D-01, 6.6000D-01, & ! 4   
     &     7.0000D-01, 4.4200D-01, 5.0000D-01, 1.0000D+00, 5.0000D-01, & ! +   
     &     9.0000D-01, 1.2000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     6.5000D-01, 6.5000D-01, 5.2100D-01, 2.3100D-01, 1.4200D-01, & ! +   
     &     3.4000D-01, 9.3300D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     2.7500D-01, 2.7500D-01, 2.2000D-01, 2.7500D-01, 3.7000D-01, & ! +   
     &     5.1200D+00, 5.0000D-01, 7.0000D-02, 2.8000D-01, 5.1200D+00, & ! 7   
     &     5.0000D-01, 7.0000D-02, 2.8000D-01, 3.5200D-01, 8.2000D-02, & ! +   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 6.5000D-01, 1.4000D-01, & ! 8   
     &     5.2000D-01, 0.0000D+00, 5.2000D-01, 5.4400D-01, 5.4400D-01, & ! +   
     &     1.4000D-01, 0.0000D+00, 7.7000D-01, 7.7000D-01, 2.5000D-02, & ! 9   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-01, 5.0000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 4.0000D-01, 5.0000D-01, 5.0000D-01, & ! O   
     &     1.0000D+00, 4.0000D-01, 2.4000D-01, 1.0000D+00, 2.0000D-01, & ! +   
     &     1.0000D+00, 5.0000D-01, 0.0000D+00, 1.0000D+00, 1.3000D-01, & ! 1   
     &     1.0000D+00, 8.0000D-01, 5.0000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 2.6800D-01, 1.0000D+00, 8.7400D-01, 0.0000D+00, & ! 2   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.4000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 9.9100D-01, 2.0000D+00, 3.3000D-01, & ! +   
     &     7.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 1.8000D-01, 1.5500D-01, & ! +   
     &     1.5500D-01, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.6000D-02, 1.4000D-01, 1.5000D-02, & ! +   
     &     1.9300D-01, 3.4000D-02, 1.4600D-01, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 2.8000D-02, 4.7300D-01, 6.0000D-03, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 6.8800D-01, 1.0000D+00, & ! 8   
     &     4.2200D-01, 7.1100D-01, 3.7000D-01, 2.4000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 4.8570D-01, 3.0030D-01, 3.8560D-01, & ! O   
     &     2.1810D-01, 2.4120D-01, 6.6640D-01, 2.8580D-01, 3.3030D-01, & ! +   
     &     3.4440D-01, 3.8860D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.8000D-01, 5.2000D-01, & ! 2   
     &     4.5000D-02, 5.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 5.0000D-01, 5.0000D-01, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D-01, 1.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, 2.5000D-01, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 1.0000D+00, 2.0000D+00, 2.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     0.0000D+00, 1.0000D+00, 4.0000D-01, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 1.0000D+00, 2.5000D-01, 0.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 2.5000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.5000D-01/           !        5   

      DATA ( SC( IRXXN,  3 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.1000D-01, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     4.0000D-01, 1.3000D-01, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 4.0000D-01, 1.3000D-01, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 9.0000D-01, 3.7000D-01, 0.0000D+00, & ! 7   
     &     0.0000D+00, 2.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     8.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D-01, & ! 8   
     &     0.0000D+00, 4.0000D-01, 1.0000D+00, 6.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 2.0000D-01, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 8.0000D-01, 1.4000D+00, 0.0000D+00, & ! 1   
     &     2.0000D-01, 0.0000D+00, 5.0000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 9.0000D-03, & ! 2   
     &     0.0000D+00, 1.0000D-01, 5.0000D-01, 1.3800D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 7.4000D-01, 0.0000D+00, 0.0000D+00, & ! 3   
     &     3.0000D-01, 1.0000D+00, 1.5600D+00, 2.7000D-01, 5.0000D-01, & ! +   
     &     1.0000D-01, 4.8800D-01, 2.7000D-01, 4.8000D-01, 1.0000D-01, & ! 4   
     &     1.0000D+00, 1.2800D-01, 4.8000D-01, 1.0000D+00, 2.5000D-01, & ! +   
     &     6.7300D-01, 1.2000D-01, 7.2800D-01, 7.2800D-01, 0.0000D+00, & ! 5   
     &     1.5000D-01, 6.4000D-01, 1.1500D-01, 5.3100D-01, 1.4200D-01, & ! +   
     &     1.6000D-01, 6.7000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     2.7500D-01, 2.7500D-01, 2.2000D-01, 2.7500D-01, 1.0000D+00, & ! +   
     &     1.0000D+00, 2.5000D-01, 6.9000D-01, 7.5000D-01, 1.0000D+00, & ! 7   
     &     2.5000D-01, 6.9000D-01, 7.5000D-01, 3.5200D-01, 9.1800D-01, & ! +   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 7.2000D-01, 4.1700D-01, & ! 8   
     &     7.7000D-01, 0.0000D+00, 7.7000D-01, 6.0200D-01, 6.0200D-01, & ! +   
     &     2.2100D-01, 0.0000D+00, 3.5000D-01, 3.5000D-01, 1.0000D+00, & ! 9   
     &     4.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 7.0000D-01, 2.0000D+00, 6.0000D-01, 4.5000D-01, & ! O   
     &     1.0000D+00, 4.0000D-01, 5.0000D-01, 0.0000D+00, 5.0000D-01, & ! +   
     &     0.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, 1.3000D-01, & ! 1   
     &     1.0000D+00, 1.8000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.6800D-01, 0.0000D+00, 1.2600D-01, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     0.0000D+00, 9.7000D-01, 9.9100D-01, 1.0000D+00, 6.7000D-01, & ! +   
     &     4.5000D-01, 9.6000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 6.5000D-01, 5.4400D-01, & ! +   
     &     5.4400D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 5.1000D-02, 0.0000D+00, 2.3000D-02, & ! +   
     &     0.0000D+00, 3.9200D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 2.2500D-01, 0.0000D+00, 5.2000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 6.2000D-03, 2.8620D-01, 9.5000D-02, & ! O   
     &     3.0630D-01, 2.0890D-01, 1.4300D-02, 3.9310D-01, 2.2720D-01, & ! +   
     &     2.7490D-01, 2.4210D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 9.6000D-01, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 6.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 4.5000D-01, 1.0000D+00/           !        5   

      DATA ( SC( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.1000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     4.0000D-01, 5.0000D-01, 0.0000D+00, 0.0000D+00, 2.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 4.0000D-01, 5.0000D-01, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D-01, 1.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 8.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 8.0000D-01, & ! 8   
     &     0.0000D+00, 4.0000D-01, 0.0000D+00, 4.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D-01, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.5000D-01, 0.0000D+00, & ! 1   
     &     1.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! 2   
     &     0.0000D+00, 1.0000D-01, 5.0000D-01, 6.2000D-01, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.7000D-01, 0.0000D+00, 0.0000D+00, & ! 3   
     &     3.0000D-01, 7.0000D-01, 2.2000D-01, 1.7000D-01, 5.0000D-01, & ! +   
     &     2.0000D-01, 9.7600D-01, 1.5000D-01, 4.8000D-01, 1.0000D-01, & ! 4   
     &     1.0000D+00, 2.4500D-01, 4.8000D-01, 0.0000D+00, 2.5000D-01, & ! +   
     &     9.0000D-01, 1.2000D-01, 7.2000D-02, 7.2000D-02, 0.0000D+00, & ! 5   
     &     2.0000D-01, 3.3000D-01, 1.1500D-01, 1.7000D-01, 1.4200D-01, & ! +   
     &     3.4000D-01, 6.7000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     1.1250D+00, 1.2500D-01, 1.0000D-01, 1.2500D-01, 4.4400D-01, & ! +   
     &     0.0000D+00, 1.5000D+00, 1.8000D-01, 2.5000D-01, 0.0000D+00, & ! 7   
     &     1.5000D+00, 1.8000D-01, 2.5000D-01, 1.1800D-01, 9.1800D-01, & ! +   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D-01, 4.4300D-01, & ! 8   
     &     2.3000D-01, 0.0000D+00, 2.3000D-01, 2.4400D-01, 2.4400D-01, & ! +   
     &     6.7500D-01, 0.0000D+00, 6.5000D-01, 6.5000D-01, 2.0000D-01, & ! 9   
     &     1.2000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 7.0000D-01, 2.0000D+00, 1.0000D-01, 4.5000D-01, & ! O   
     &     0.0000D+00, 4.0000D-01, 1.2000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 8.0000D-01, 0.0000D+00, 0.0000D+00, 5.0000D-01, & ! 1   
     &     2.0000D+00, 2.0000D-01, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 8.7400D-01, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 3.0000D-02, 9.0000D-03, 1.0000D+00, 2.0000D+00, & ! +   
     &     5.5000D-01, 4.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 7.2000D-01, 6.0200D-01, & ! +   
     &     6.0200D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 4.7000D-02, 0.0000D+00, 6.0000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 1.9100D-01, 0.0000D+00, 8.1000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.5000D-03, 4.1000D-03, 1.3730D-01, & ! O   
     &     1.5300D-02, 3.0000D-01, 1.2300D-02, 1.3900D-02, 2.6070D-01, & ! +   
     &     4.9100D-02, 6.4000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -1.0000D+00, 4.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 4.5000D-01, 1.0000D+00/           !        5   

      DATA ( SC( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
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
     &     4.0000D-01, 5.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 4.0000D-01, 5.0000D-01, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 9.0000D-01, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.9000D-01, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 7.8000D-02, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.0000D-02, 0.0000D+00, 0.0000D+00, & ! 3   
     &     3.0000D-01, 7.0000D-01, 0.0000D+00, 4.2000D-01, 1.0000D+00, & ! +   
     &     2.0000D-01, 1.9500D-01, 1.5000D-01, 4.0000D-02, 1.0000D-01, & ! 4   
     &     0.0000D+00, 5.0000D-01, 4.0000D-02, 0.0000D+00, 2.5000D-01, & ! +   
     &     8.1800D-01, 1.2000D-01, 8.0000D-01, 1.0720D+00, 0.0000D+00, & ! 5   
     &     3.5000D-01, 3.0000D-02, 2.6900D-01, 1.7000D-01, 1.4200D-01, & ! +   
     &     2.0800D-01, 2.9000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     8.2500D-01, 8.2500D-01, 6.6000D-01, 8.2500D-01, 1.8500D-01, & ! +   
     &     0.0000D+00, 2.8000D-01, 9.4000D-01, 1.2800D+00, 0.0000D+00, & ! 7   
     &     2.8000D-01, 9.4000D-01, 1.2800D+00, 1.1800D-01, 9.1800D-01, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-01, 6.6000D-01, & ! 8   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 2.4400D-01, 2.4400D-01, & ! +   
     &     3.0000D-01, 0.0000D+00, 1.0000D+00, 1.0000D+00, 7.3200D-01, & ! 9   
     &     2.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.0000D-01, 0.0000D+00, 5.0000D-01, 1.0000D-01, & ! O   
     &     0.0000D+00, 0.0000D+00, 8.0000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.0000D-01, 0.0000D+00, 0.0000D+00, 5.0000D-01, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     3.0000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-01, 2.4400D-01, & ! +   
     &     2.4400D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.6000D-03, 3.5000D-03, 5.0000D-04, & ! O   
     &     1.0430D-01, 2.0280D-01, 1.2390D-01, 1.0270D-01, 7.0200D-02, & ! +   
     &     2.5770D-01, 3.8500D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
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
     &     0.0000D+00, 1.4000D-01, 0.0000D+00/           !        5   

      DATA ( SC( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
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
     &     0.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 4.0000D-01, 5.0000D-01, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.1000D-01, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 1.1000D-02, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 9.4000D-01, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 3.0000D-01, 0.0000D+00, 0.0000D+00, 1.1250D+00, & ! +   
     &     2.0000D-01, 2.4000D-02, 3.3400D-01, 1.0000D+00, 1.0000D-01, & ! 4   
     &     0.0000D+00, 3.0000D-01, 1.0000D+00, 0.0000D+00, 2.5000D-01, & ! +   
     &     8.2000D-02, 0.0000D+00, 2.0000D-01, 0.0000D+00, 0.0000D+00, & ! 5   
     &     2.6600D-01, 1.0000D+00, 2.6900D-01, 5.4300D-01, 1.1300D-01, & ! +   
     &     2.6000D-01, 2.9000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     3.7500D-01, 3.7500D-01, 3.0000D-01, 3.7500D-01, 1.0400D-01, & ! +   
     &     0.0000D+00, 1.6600D+00, 2.4000D-01, 4.7000D-01, 0.0000D+00, & ! 7   
     &     1.6600D+00, 2.4000D-01, 4.7000D-01, 5.3000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 7.0000D-02, 2.0000D-01, & ! 8   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 5.8000D-02, 5.8000D-02, & ! +   
     &     5.6000D-01, 0.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D-02, & ! 9   
     &     2.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.0000D-01, 1.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 2.0000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-01, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.2600D-01, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.0000D+00, & ! +   
     &     3.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-01, 2.4400D-01, & ! +   
     &     2.4400D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.3000D-03, 2.2390D-01, 2.0510D-01, & ! O   
     &     1.8930D-01, 4.7100D-02, 1.8310D-01, 2.0450D-01, 1.1160D-01, & ! +   
     &     7.3900D-02, 2.6670D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
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
     &     0.0000D+00, 5.5000D-01, 0.0000D+00/           !        5   

      DATA ( SC( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.1000D-01, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00,-2.5000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 9.8000D-01, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D-02, 1.1950D+00, 8.0000D-02, 5.0000D-01, 0.0000D+00, & ! 4   
     &     0.0000D+00, 3.0000D-01, 5.0000D-01, 0.0000D+00, 2.5000D-01, & ! +   
     &     8.2000D-02, 0.0000D+00, 8.7200D-01, 0.0000D+00, 0.0000D+00, & ! 5   
     &     2.0000D-01, 3.5000D-01, 4.5700D-01, 4.6100D-01, 1.1300D-01, & ! +   
     &     2.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     7.4000D-02, 1.0000D+00, 2.0000D-01, 2.5100D-01, 5.9200D-01, & ! +   
     &     0.0000D+00, 4.7000D-01, 1.0000D-03, 5.3000D-01, 0.0000D+00, & ! 7   
     &     4.7000D-01, 1.0000D-03, 5.3000D-01, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.8000D-01, 8.6000D-01, & ! 8   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 1.5500D-01, 1.5500D-01, & ! +   
     &     8.6000D-01, 0.0000D+00, 1.0000D+00, 0.0000D+00, 2.0000D-02, & ! 9   
     &     4.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.0000D-01, 2.5000D-01, & ! O   
     &     0.0000D+00, 0.0000D+00, 1.9800D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.7000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 7.0000D-02, 5.8000D-02, & ! +   
     &     5.8000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.9440D-01, 1.8200D-01, 1.7640D-01, & ! O   
     &     1.6680D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        5   

      DATA ( SC( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.1000D-01, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.0000D-02, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.1000D-01,-7.3000D-01, 3.7800D-01, 2.5000D-01, 0.0000D+00, & ! 4   
     &     0.0000D+00, 2.4000D-01, 6.2500D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     2.0000D-01, 3.5000D-01, 1.1700D-01, 1.5000D-01, 7.1700D-01, & ! +   
     &     2.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     2.5100D-01, 2.5100D-01, 1.7400D+00, 2.1750D+00, 3.3100D-01, & ! +   
     &     0.0000D+00, 1.0000D+00, 7.0000D+00, 1.0000D+00, 0.0000D+00, & ! 7   
     &     1.0000D+00, 7.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     1.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-01, & ! O   
     &     0.0000D+00, 0.0000D+00, 5.6000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.8000D-01, 1.5500D-01, & ! +   
     &     1.5500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.0210D-01, 0.0000D+00, 0.0000D+00, & ! O   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        5   

      DATA ( SC( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-2.7000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.0000D-01, 0.0000D+00, 7.5000D-02, 3.7500D-01, 0.0000D+00, & ! 4   
     &     0.0000D+00, 6.0000D-02, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     6.6000D-02, 1.0000D+00, 1.3700D-01, 3.9800D-01, 7.1700D-01, & ! +   
     &     1.7000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     2.1750D+00, 2.1750D+00, 8.0000D-01, 1.0000D+00, 1.8500D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.1000D-01, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 2.1000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     7.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.9000D-03, 0.0000D+00, 0.0000D+00, & ! O   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        5   

      DATA ( SC( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
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
     &     1.0000D-01, 0.0000D+00, 7.5000D-02,-1.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 2.9000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     6.6000D-02, 0.0000D+00, 1.3700D-01, 1.4300D-01, 2.8400D-01, & ! +   
     &     1.2800D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 2.0000D-01, 0.0000D+00, 2.7000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.9000D-01, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 3.9000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.3000D-03, 0.0000D+00, 0.0000D+00, & ! O   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        5   

      DATA ( SC( IRXXN, 11 ), IRXXN = 1, NRXNS ) / & 
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
     &     0.0000D+00, 0.0000D+00, 9.0000D-02, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 8.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 1.3700D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     8.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 8.0000D-01, 0.0000D+00, 9.8000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        5   

      DATA ( SC( IRXXN, 12 ), IRXXN = 1, NRXNS ) / & 
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
     &     0.0000D+00, 0.0000D+00, 1.3000D-01, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 8.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 6.5800D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 7.8000D-02, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        5   

      DATA ( SC( IRXXN, 13 ), IRXXN = 1, NRXNS ) / & 
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
     &     0.0000D+00, 0.0000D+00, 4.0000D-02, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.6600D-01, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        5   

      DATA ( SC( IRXXN, 14 ), IRXXN = 1, NRXNS ) / & 
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
     &     0.0000D+00, 0.0000D+00,-7.9000D-01, 0.0000D+00, 0.0000D+00, & ! 4   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        5   

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
     &      1,    1,    1,    1,    2,    1,    1,    2,    2,    2, & ! 2   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    1,    2,    1,    1,    1,    1,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 6   
     &      1,    1,    1,    1,    2,    2,    2,    1,    1,    2, & ! 7   
     &      1,    1,    1,    2,    2,    2,    2,    2,    2,    1, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    1,    1,    2,    2,    2, & ! 1   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      1,    2,    2,    2,    2,    1,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    2,    2,    2,    2,    2,    2,    1, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! O   
     &      1,    1,    1,    2,    2,    2,    2,    2,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    2,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2/     !  5   
      INTEGER            :: NPRDCT( NRXNS )

      DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    1,    1,    1,    1,    1,    0,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    0,    1,    1, & ! 1   
     &      1,    1,    2,    1,    2,    1,    2,    1,    1,    2, & ! 2   
     &      1,    2,    2,    1,    1,    1,    2,    2,    1,    1, & ! 3   
     &      1,    2,    2,    1,    1,    1,    2,    1,    2,    4, & ! 4   
     &      1,    3,    3,    1,    2,    5,    6,    1,    2,    4, & ! 5   
     &      4,    1,    2,    6,    6,    1,    2,    1,    1,    0, & ! 6   
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
     &      2,    5,    1,    2,    6,    4,    4,    4,    2,    2, & ! 1   
     &      2,    5,    2,    6,    1,    1,    2,    1,    2,    2, & ! 2   
     &      1,    3,    2,    3,    3,    2,    5,    5,    4,    6, & ! 3   
     &      8,    5,    3,    2,    2,    3,    3,    1,    9,    9, & ! 4   
     &      9,    2,    1,    2,    2,    2,    2,    4,    2,    4, & ! 5   
     &      2,    3,    2,    2,    2,    2,    4,    2,    4,    1, & ! 6   
     &      2,    2,    1,    1,    1,    1,    1,    2,    1,    1, & ! 7   
     &      1,    1,    1,    2,    2,    2,    2,    2,    2,    0, & ! 8   
     &      1,    1,    1,    1,    1,    1,    1,    3,    1,    3, & ! 9   
     &      1,    2,   10,    7,    7,    7,    6,    6,    6,    6, & ! O   
     &      6,    6,    1,    1,    1,    0,    0,    1,    1,    1, & ! 1   
     &      1,    0,    1,    2,    2,    2,    2,    1,    1,    1, & ! 2   
     &      0,    1,    1,    1,    1,    0,    1,    1,    1,    3, & ! 3   
     &      2,    2,    3,    3,    1,    1,    1,    1,    1,    1, & ! 4   
     &      2,    2,    2,    2,    2,    1,    1,    2,    3,    2, & ! 5   
     &      5,    5,    2,    3,    2,    3,    3,    3,    4,    4, & ! 6   
     &      1,    2,    2,    2,    2,    2,    2,    3,    3,    4, & ! 7   
     &      4,    2,    2,    1,    1,    2,    2,    2,    2,    1, & ! 8   
     &      1,    1,    2,    1,    1,    1,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    1,    2,    3,    1,    2, & ! O   
     &      1,    2,    2,    1,    1,    1,    2,    4,    1,    2, & ! 1   
     &      2,    1,    2,    2,    2,    2,    2,    1,    2,    2, & ! 2   
     &      2,    2,    3,    3,    2,    1,    0,    0,    0,    0, & ! 3   
     &      0,    0,    2,    2,    2,    2,    1,    1,    3,    3, & ! 4   
     &      4,    6,    4/     !  5   

      INTEGER, PARAMETER :: MHETERO =  39
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    256,  257,  270,  271,  272,  273,  274,  275,  276,  277, & 
     &    278,  279,  280,  281,  282,  283,  299,  301,  382,  383, & 
     &    384,  385,  386,  387,  388,  389,  390,  437,  438,  439, & 
     &    440,  441,  442,  443,  444,  445,  446,  447,  448/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    8,    9, & 
     &     10,   11,   12,   13,   14,   15,   16,   17,   18,   19, & 
     &     20,   21,   22,   23,   24,   25,   26,   27,   28,   29, & 
     &     30,   31,   32,   33,   34,   35,   36,   37,   38/

      INTEGER, PARAMETER :: NPHOTAB =  56
      CHARACTER( 16 )    :: PHOTAB( NPHOTAB )

      DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & 
     &   'NO2_IUPAC10     ', 'O3_O3P_IUPAC10  ', 'O3_O1D_IUPAC10  ', & 
     &   'H2O2_IUPAC10    ', 'NO3NO2_06       ', 'NO3NO_06        ', & 
     &   'N2O5_IUPAC10    ', 'HONO_IUPAC10    ', 'HNO3_IUPAC10    ', & 
     &   'PNA_IUPAC10     ', 'PAN_IUPAC10     ', 'MEPX_IUPAC10    ', & 
     &   'NTR_IUPAC10     ', 'FORM_R_IUPAC13  ', 'FORM_M_IUPAC13  ', & 
     &   'ALD2_R_IUPAC13  ', 'ALDX_R_IUPAC13  ', 'GLYD_IUPAC13    ', & 
     &   'GLY_R_IUPAC13   ', 'MGLY_IUPAC10    ', 'KET_IUPAC10     ', & 
     &   'ACET_IUPAC10    ', 'ISPD            ', 'HPALD           ', & 
     &   'CL2_IUPAC04     ', 'HOCL_IUPAC04    ', 'FMCL_IUPAC04    ', & 
     &   'CLNO2_IUPAC13   ', 'CLONO2_1        ', 'CLONO2_2        ', & 
     &   'IC3ONO2         ', 'ACRO_09         ', 'BR2_IUPAC10     ', & 
     &   'HOBR_IUPAC10    ', 'BRO_IUPAC10     ', 'BRNO2_IUPAC10   ', & 
     &   'BRONO2_M_IUPAC10', 'BRONO2_R_IUPAC10', 'BRCL_IUPAC10    ', & 
     &   'COHBR_JPL2010   ', 'MB3_IUPAC10     ', 'MB2C_BLIDE98    ', & 
     &   'MBC2_BLIDE98    ', 'I2_IUPAC10      ', 'HOI_IUPAC10     ', & 
     &   'IO_IUPAC10      ', 'OIO_06          ', 'INO_06          ', & 
     &   'INO2_06         ', 'IONO2_06        ', 'ICL_IUPAC10     ', & 
     &   'IBR_IUPAC10     ', 'CH3I_IUPAC10    ', 'MI2_IUPAC10     ', & 
     &   'MIB_IUPAC10     ', 'MIC_IUPAC10     '/

      INTEGER, PARAMETER :: NHETERO =  38
      CHARACTER( 16 )    :: HETERO( NHETERO )

      DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & 
     &   'HETERO_CLNO3_WAI', 'HETERO_CLNO3_WAJ', 'HETERO_NTR2     ', &
     &   'HETERO_N2O5IJ   ', 'HETERO_N2O5K    ', 'HETERO_H2NO3PAIJ', &
     &   'HETERO_H2NO3PAK ', 'HETERO_H2NO3PBIJ', 'HETERO_H2NO3PBK ', &
     &   'HETERO_NO2      ', 'HETERO_IEPOX    ', 'HETERO_IEPOXOS  ', &
     &   'HETERO_TETROL   ', 'HETERO_GLY      ', 'HETERO_MGLY     ', &
     &   'HETERO_PNCOMLI  ', 'HETERO_PNCOMLJ  ', 'HETERO_BRNO3_WAI', &
     &   'HETERO_BRNO3_WAJ', 'HETERO_HOBR_CLJ ', 'HETERO_HOBR_BRJ ', &
     &   'HETERO_BRNO3_CLJ', 'HETERO_BRNO3_BRJ', 'HETERO_BRNO2_CLJ', &
     &   'HETERO_BRNO2_BRJ', 'HETERO_HBR_BRJ  ', 'HETERO_I2O2_AI  ', &
     &   'HETERO_I2O2_AJ  ', 'HETERO_I2O3_AI  ', 'HETERO_I2O3_AJ  ', &
     &   'HETERO_I2O4_AI  ', 'HETERO_I2O4_AJ  ', 'HETERO_INO3_CLJ ', &
     &   'HETERO_INO3_BRJ ', 'HETERO_INO2_CLJ ', 'HETERO_INO2_BRJ ', &
     &   'HETERO_HOI_CLJ  ', 'HETERO_HOI_BRJ  '/

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
     &    'R219            ', 'R220            ', 'R221            ', & ! 4   
     &    'CL1             ', 'CL2             ', 'CL3             ', & ! 5   
     &    'CL4             ', 'CL5             ', 'CL6             ', & ! 6   
     &    'CL7             ', 'CL8             ', 'CL9             ', & ! 7   
     &    'CL10            ', 'CL11            ', 'CL12            ', & ! 8   
     &    'CL13            ', 'CL14            ', 'CL15            ', & ! 9   
     &    'CL16            ', 'CL17            ', 'CL18            ', & ! 0   
     &    'CL19            ', 'CL20            ', 'CL21            ', & ! 1   
     &    'CL22            ', 'CL23            ', 'CL24            ', & ! 2   
     &    'CL25            ', 'CL26            ', 'CL27            ', & ! 3   
     &    'CL28            ', 'CL30            ', 'CL31            ', & ! 4   
     &    'HET_CLNO3_WAI   ', 'HET_CLNO3_WAJ   ', 'SA01            ', & ! 5   
     &    'SA02            ', 'SA03            ', 'SA04            ', & ! 6   
     &    'SA06            ', 'SA07            ', 'SA08            ', & ! 7   
     &    'SA09            ', 'SA10            ', 'SA11            ', & ! 8   
     &    'SA12            ', 'SA13            ', 'HET_NTR2        ', & ! 9   
     &    'HET_N2O5IJ      ', 'HET_N2O5K       ', 'HET_H2NO3PIJA   ', & ! 0   
     &    'HET_H2NO3PKA    ', 'HET_H2NO3PIB    ', 'HET_H2NO3PJB    ', & ! 1   
     &    'HET_H2NO3PKB    ', 'HET_N02         ', 'HET_IEPOX       ', & ! 2   
     &    'HET_IEPOXOS     ', 'HET_TETROL      ', 'HET_GLY         ', & ! 3   
     &    'HET_MGLY        ', 'BL18a           ', 'BL18b           ', & ! 4   
     &    'BL18c           ', 'BL18d           ', 'CP07mtp         ', & ! 5   
     &    'BP70mtp         ', 'BP71mtp         ', 'HYD_MT          ', & ! 6   
     &    'OLIG_AROMATIC1  ', 'OLIG_AROMATIC2  ', 'OLIG_AROMATIC3  ', & ! 7   
     &    'OLIG_ISOPRENE1  ', 'OLIG_ISOPRENE2  ', 'OLIG_SESQT1     ', & ! 8   
     &    'RPOAGEPI        ', 'RPOAGELI        ', 'RPOAGEPJ        ', & ! 9   
     &    'RPOAGELJ        ', 'PCSOA           ', 'POA_AGE1        ', & ! 0   
     &    'POA_AGE2        ', 'POA_AGE3        ', 'POA_AGE4        ', & ! 1   
     &    'POA_AGE5        ', 'POA_AGE6        ', 'POA_AGE7        ', & ! 2   
     &    'POA_AGE8        ', 'POA_AGE9        ', 'POA_AGE10       ', & ! 3   
     &    'T01             ', 'T02             ', 'T03             ', & ! 4   
     &    'T04             ', 'T05             ', 'TCL1            ', & ! 5   
     &    'T06             ', 'T07             ', 'T08             ', & ! 6   
     &    'T09             ', 'TCL2            ', 'T10             ', & ! 7   
     &    'T11             ', 'T12             ', 'TCL3            ', & ! 8   
     &    'T13             ', 'T14             ', 'T15             ', & ! 9   
     &    'T16             ', 'TCL4            ', 'T17             ', & ! 0   
     &    'T18             ', 'T19             ', 'T20             ', & ! 1   
     &    'TCL5            ', 'T21             ', 'TCL6            ', & ! 2   
     &    'HG1             ', 'HG2             ', 'HG3             ', & ! 3   
     &    'HG4             ', 'HG5             ', 'BR01            ', & ! 4   
     &    'BR02            ', 'BR03            ', 'BR04            ', & ! 5   
     &    'BR05            ', 'BR06            ', 'BR07            ', & ! 6   
     &    'BR08            ', 'BR09            ', 'BR10            ', & ! 7   
     &    'BR11            ', 'BR12            ', 'BR13            ', & ! 8   
     &    'BR14            ', 'BR15            ', 'BR16            ', & ! 9   
     &    'BR17            ', 'BR18            ', 'BR19            ', & ! 0   
     &    'BR20            ', 'BR22            ', 'BR23            ', & ! 1   
     &    'BR24            ', 'BR25            ', 'BR26            ', & ! 2   
     &    'BR27            ', 'BR28            ', 'BR29            ', & ! 3   
     &    'BR30            ', 'BR31            ', 'BR32            ', & ! 4   
     &    'BR33            ', 'BR34            ', 'BR35            ', & ! 5   
     &    'BR36            ', 'BR37            ', 'BR38            ', & ! 6   
     &    'HET_BRNO3_WAI   ', 'HET_BRNO3_WAJ   ', 'HET_HOBR_CLJ    ', & ! 7   
     &    'HET_HOBR_BRJ    ', 'HET_BRNO3_CLJ   ', 'HET_BRNO3_BRJ   ', & ! 8   
     &    'HET_BRNO2_CLJ   ', 'HET_BRNO2_BRJ   ', 'HET_HBR_BRJ     ', & ! 9   
     &    'IO01            ', 'IO02            ', 'IO03            ', & ! 0   
     &    'IO04            ', 'IO05            ', 'IO06            ', & ! 1   
     &    'IO07            ', 'IO08            ', 'IO09            ', & ! 2   
     &    'IO10            ', 'IO11            ', 'IO12            ', & ! 3   
     &    'IO13            ', 'IO14            ', 'IO15            ', & ! 4   
     &    'IO16            ', 'IO17            ', 'IO18            ', & ! 5   
     &    'IO19            ', 'IO20            ', 'IO21            ', & ! 6   
     &    'IO22            ', 'IO23            ', 'IO24            ', & ! 7   
     &    'IO25            ', 'IO26            ', 'IO27            ', & ! 8   
     &    'IO28            ', 'IO29            ', 'IO30            ', & ! 9   
     &    'IO31            ', 'IO32            ', 'IO33            ', & ! 0   
     &    'IO34            ', 'IO35            ', 'IO36            ', & ! 1   
     &    'IO37            ', 'IO38            ', 'IO39            ', & ! 2   
     &    'IO40            ', 'IO41            ', 'IO42            ', & ! 3   
     &    'IO43            ', 'IO44            ', 'IO45            ', & ! 4   
     &    'IO46            ', 'HET_I2O2_AI     ', 'HET_I2O2_AJ     ', & ! 5   
     &    'HET_I2O3_AI     ', 'HET_I2O3_AJ     ', 'HET_I2O4_AI     ', & ! 6   
     &    'HET_I2O4_AJ     ', 'HET_INO3_CLJ    ', 'HET_INO3_BRJ    ', & ! 7   
     &    'HET_INO2_CLJ    ', 'HET_INO2_BRJ    ', 'HET_HOI_CLJ     ', & ! 8   
     &    'HET_HOI_BRJ     ', 'DMS1            ', 'DMS2            ', & ! 9   
     &    'DMS3            ', 'DMS4            ', 'DMS5            '/! 0  

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
       INTEGER, PARAMETER  :: IJ_FORM_R_IUPAC13   =  14
       INTEGER, PARAMETER  :: IJ_FORM_M_IUPAC13   =  15
       INTEGER, PARAMETER  :: IJ_ALD2_R_IUPAC13   =  16
       INTEGER, PARAMETER  :: IJ_ALDX_R_IUPAC13   =  17
       INTEGER, PARAMETER  :: IJ_GLYD_IUPAC13     =  18
       INTEGER, PARAMETER  :: IJ_GLY_R_IUPAC13    =  19
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
       INTEGER, PARAMETER  :: IJ_BR2_IUPAC10      =  33
       INTEGER, PARAMETER  :: IJ_HOBR_IUPAC10     =  34
       INTEGER, PARAMETER  :: IJ_BRO_IUPAC10      =  35
       INTEGER, PARAMETER  :: IJ_BRNO2_IUPAC10    =  36
       INTEGER, PARAMETER  :: IJ_BRONO2_M_IUPAC10 =  37
       INTEGER, PARAMETER  :: IJ_BRONO2_R_IUPAC10 =  38
       INTEGER, PARAMETER  :: IJ_BRCL_IUPAC10     =  39
       INTEGER, PARAMETER  :: IJ_COHBR_JPL2010    =  40
       INTEGER, PARAMETER  :: IJ_MB3_IUPAC10      =  41
       INTEGER, PARAMETER  :: IJ_MB2C_BLIDE98     =  42
       INTEGER, PARAMETER  :: IJ_MBC2_BLIDE98     =  43
       INTEGER, PARAMETER  :: IJ_I2_IUPAC10       =  44
       INTEGER, PARAMETER  :: IJ_HOI_IUPAC10      =  45
       INTEGER, PARAMETER  :: IJ_IO_IUPAC10       =  46
       INTEGER, PARAMETER  :: IJ_OIO_06           =  47
       INTEGER, PARAMETER  :: IJ_INO_06           =  48
       INTEGER, PARAMETER  :: IJ_INO2_06          =  49
       INTEGER, PARAMETER  :: IJ_IONO2_06         =  50
       INTEGER, PARAMETER  :: IJ_ICL_IUPAC10      =  51
       INTEGER, PARAMETER  :: IJ_IBR_IUPAC10      =  52
       INTEGER, PARAMETER  :: IJ_CH3I_IUPAC10     =  53
       INTEGER, PARAMETER  :: IJ_MI2_IUPAC10      =  54
       INTEGER, PARAMETER  :: IJ_MIB_IUPAC10      =  55
       INTEGER, PARAMETER  :: IJ_MIC_IUPAC10      =  56
       INTEGER, PARAMETER  :: IK_HETERO_CLNO3_WAI =   1
       INTEGER, PARAMETER  :: IK_HETERO_CLNO3_WAJ =   2
       INTEGER, PARAMETER  :: IK_HETERO_NTR2      =   3
       INTEGER, PARAMETER  :: IK_HETERO_N2O5IJ    =   4
       INTEGER, PARAMETER  :: IK_HETERO_N2O5K     =   5
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAIJ =   6
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAK  =   7
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBIJ =   8
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBK  =   9
       INTEGER, PARAMETER  :: IK_HETERO_NO2       =  10
       INTEGER, PARAMETER  :: IK_HETERO_IEPOX     =  11
       INTEGER, PARAMETER  :: IK_HETERO_IEPOXOS   =  12
       INTEGER, PARAMETER  :: IK_HETERO_TETROL    =  13
       INTEGER, PARAMETER  :: IK_HETERO_GLY       =  14
       INTEGER, PARAMETER  :: IK_HETERO_MGLY      =  15
       INTEGER, PARAMETER  :: IK_HETERO_PNCOMLI   =  16
       INTEGER, PARAMETER  :: IK_HETERO_PNCOMLJ   =  17
       INTEGER, PARAMETER  :: IK_HETERO_BRNO3_WAI =  18
       INTEGER, PARAMETER  :: IK_HETERO_BRNO3_WAJ =  19
       INTEGER, PARAMETER  :: IK_HETERO_HOBR_CLJ  =  20
       INTEGER, PARAMETER  :: IK_HETERO_HOBR_BRJ  =  21
       INTEGER, PARAMETER  :: IK_HETERO_BRNO3_CLJ =  22
       INTEGER, PARAMETER  :: IK_HETERO_BRNO3_BRJ =  23
       INTEGER, PARAMETER  :: IK_HETERO_BRNO2_CLJ =  24
       INTEGER, PARAMETER  :: IK_HETERO_BRNO2_BRJ =  25
       INTEGER, PARAMETER  :: IK_HETERO_HBR_BRJ   =  26
       INTEGER, PARAMETER  :: IK_HETERO_I2O2_AI   =  27
       INTEGER, PARAMETER  :: IK_HETERO_I2O2_AJ   =  28
       INTEGER, PARAMETER  :: IK_HETERO_I2O3_AI   =  29
       INTEGER, PARAMETER  :: IK_HETERO_I2O3_AJ   =  30
       INTEGER, PARAMETER  :: IK_HETERO_I2O4_AI   =  31
       INTEGER, PARAMETER  :: IK_HETERO_I2O4_AJ   =  32
       INTEGER, PARAMETER  :: IK_HETERO_INO3_CLJ  =  33
       INTEGER, PARAMETER  :: IK_HETERO_INO3_BRJ  =  34
       INTEGER, PARAMETER  :: IK_HETERO_INO2_CLJ  =  35
       INTEGER, PARAMETER  :: IK_HETERO_INO2_BRJ  =  36
       INTEGER, PARAMETER  :: IK_HETERO_HOI_CLJ   =  37
       INTEGER, PARAMETER  :: IK_HETERO_HOI_BRJ   =  38
       END MODULE RXNS_DATA
