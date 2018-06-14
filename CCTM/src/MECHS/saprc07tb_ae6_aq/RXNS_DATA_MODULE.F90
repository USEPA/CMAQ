       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: ../../CCTM/src/MECHS/saprc07tb_ae6_aq/mech_saprc07tb_ae6_aq.def
! for Mechanism Name: SAPRC07TB_AE6_AQ                

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

      CHARACTER( 32 ), PARAMETER :: MECHNAME = 'SAPRC07TB_AE6_AQ'

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC = 173
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 194

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
      DATA GAS_CHEM_SPC(   3 ) / 'O3P             ' /
      DATA GAS_CHEM_SPC(   4 ) / 'O3              ' /
      DATA GAS_CHEM_SPC(   5 ) / 'NO3             ' /
      DATA GAS_CHEM_SPC(   6 ) / 'N2O5            ' /
      DATA GAS_CHEM_SPC(   7 ) / 'HNO3            ' /
      DATA GAS_CHEM_SPC(   8 ) / 'O1D             ' /
      DATA GAS_CHEM_SPC(   9 ) / 'OH              ' /
      DATA GAS_CHEM_SPC(  10 ) / 'HONO            ' /
      DATA GAS_CHEM_SPC(  11 ) / 'HO2             ' /
      DATA GAS_CHEM_SPC(  12 ) / 'CO              ' /
      DATA GAS_CHEM_SPC(  13 ) / 'CO2             ' /
      DATA GAS_CHEM_SPC(  14 ) / 'HNO4            ' /
      DATA GAS_CHEM_SPC(  15 ) / 'HO2H            ' /
      DATA GAS_CHEM_SPC(  16 ) / 'SO2             ' /
      DATA GAS_CHEM_SPC(  17 ) / 'SULF            ' /
      DATA GAS_CHEM_SPC(  18 ) / 'SULRXN          ' /
      DATA GAS_CHEM_SPC(  19 ) / 'MEO2            ' /
      DATA GAS_CHEM_SPC(  20 ) / 'HCHO            ' /
      DATA GAS_CHEM_SPC(  21 ) / 'COOH            ' /
      DATA GAS_CHEM_SPC(  22 ) / 'MEOH            ' /
      DATA GAS_CHEM_SPC(  23 ) / 'RO2C            ' /
      DATA GAS_CHEM_SPC(  24 ) / 'RO2XC           ' /
      DATA GAS_CHEM_SPC(  25 ) / 'MECO3           ' /
      DATA GAS_CHEM_SPC(  26 ) / 'PAN             ' /
      DATA GAS_CHEM_SPC(  27 ) / 'CCOOOH          ' /
      DATA GAS_CHEM_SPC(  28 ) / 'CCOOH           ' /
      DATA GAS_CHEM_SPC(  29 ) / 'RCO3            ' /
      DATA GAS_CHEM_SPC(  30 ) / 'PAN2            ' /
      DATA GAS_CHEM_SPC(  31 ) / 'xHO2            ' /
      DATA GAS_CHEM_SPC(  32 ) / 'yROOH           ' /
      DATA GAS_CHEM_SPC(  33 ) / 'xCCHO           ' /
      DATA GAS_CHEM_SPC(  34 ) / 'RCOOOH          ' /
      DATA GAS_CHEM_SPC(  35 ) / 'RCOOH           ' /
      DATA GAS_CHEM_SPC(  36 ) / 'BZCO3           ' /
      DATA GAS_CHEM_SPC(  37 ) / 'PBZN            ' /
      DATA GAS_CHEM_SPC(  38 ) / 'BZO             ' /
      DATA GAS_CHEM_SPC(  39 ) / 'MACO3           ' /
      DATA GAS_CHEM_SPC(  40 ) / 'MAPAN           ' /
      DATA GAS_CHEM_SPC(  41 ) / 'TBUO            ' /
      DATA GAS_CHEM_SPC(  42 ) / 'RNO3            ' /
      DATA GAS_CHEM_SPC(  43 ) / 'ACETONE         ' /
      DATA GAS_CHEM_SPC(  44 ) / 'NPHE            ' /
      DATA GAS_CHEM_SPC(  45 ) / 'CRES            ' /
      DATA GAS_CHEM_SPC(  46 ) / 'xOH             ' /
      DATA GAS_CHEM_SPC(  47 ) / 'xNO2            ' /
      DATA GAS_CHEM_SPC(  48 ) / 'xMEO2           ' /
      DATA GAS_CHEM_SPC(  49 ) / 'xMECO3          ' /
      DATA GAS_CHEM_SPC(  50 ) / 'xRCO3           ' /
      DATA GAS_CHEM_SPC(  51 ) / 'xMACO3          ' /
      DATA GAS_CHEM_SPC(  52 ) / 'xTBUO           ' /
      DATA GAS_CHEM_SPC(  53 ) / 'xCO             ' /
      DATA GAS_CHEM_SPC(  54 ) / 'CCHO            ' /
      DATA GAS_CHEM_SPC(  55 ) / 'RCHO            ' /
      DATA GAS_CHEM_SPC(  56 ) / 'xHCHO           ' /
      DATA GAS_CHEM_SPC(  57 ) / 'MEK             ' /
      DATA GAS_CHEM_SPC(  58 ) / 'zRNO3           ' /
      DATA GAS_CHEM_SPC(  59 ) / 'xRCHO           ' /
      DATA GAS_CHEM_SPC(  60 ) / 'HCOOH           ' /
      DATA GAS_CHEM_SPC(  61 ) / 'xMGLY           ' /
      DATA GAS_CHEM_SPC(  62 ) / 'xBACL           ' /
      DATA GAS_CHEM_SPC(  63 ) / 'ROOH            ' /
      DATA GAS_CHEM_SPC(  64 ) / 'xPROD2          ' /
      DATA GAS_CHEM_SPC(  65 ) / 'R6OOH           ' /
      DATA GAS_CHEM_SPC(  66 ) / 'PRD2            ' /
      DATA GAS_CHEM_SPC(  67 ) / 'yR6OOH          ' /
      DATA GAS_CHEM_SPC(  68 ) / 'RAOOH           ' /
      DATA GAS_CHEM_SPC(  69 ) / 'MGLY            ' /
      DATA GAS_CHEM_SPC(  70 ) / 'IPRD            ' /
      DATA GAS_CHEM_SPC(  71 ) / 'xGLY            ' /
      DATA GAS_CHEM_SPC(  72 ) / 'xMEK            ' /
      DATA GAS_CHEM_SPC(  73 ) / 'xAFG1           ' /
      DATA GAS_CHEM_SPC(  74 ) / 'xAFG2           ' /
      DATA GAS_CHEM_SPC(  75 ) / 'GLY             ' /
      DATA GAS_CHEM_SPC(  76 ) / 'AFG1            ' /
      DATA GAS_CHEM_SPC(  77 ) / 'AFG2            ' /
      DATA GAS_CHEM_SPC(  78 ) / 'HCOCO3          ' /
      DATA GAS_CHEM_SPC(  79 ) / 'BACL            ' /
      DATA GAS_CHEM_SPC(  80 ) / 'BALD            ' /
      DATA GAS_CHEM_SPC(  81 ) / 'AFG3            ' /
      DATA GAS_CHEM_SPC(  82 ) / 'xIPRD           ' /
      DATA GAS_CHEM_SPC(  83 ) / 'MACR            ' /
      DATA GAS_CHEM_SPC(  84 ) / 'MVK             ' /
      DATA GAS_CHEM_SPC(  85 ) / 'xHOCCHO         ' /
      DATA GAS_CHEM_SPC(  86 ) / 'xRNO3           ' /
      DATA GAS_CHEM_SPC(  87 ) / 'HOCCHO          ' /
      DATA GAS_CHEM_SPC(  88 ) / 'xACETONE        ' /
      DATA GAS_CHEM_SPC(  89 ) / 'ACROLEIN        ' /
      DATA GAS_CHEM_SPC(  90 ) / 'xBALD           ' /
      DATA GAS_CHEM_SPC(  91 ) / 'xAFG3           ' /
      DATA GAS_CHEM_SPC(  92 ) / 'xMACR           ' /
      DATA GAS_CHEM_SPC(  93 ) / 'xMVK            ' /
      DATA GAS_CHEM_SPC(  94 ) / 'yISOPOOH        ' /
      DATA GAS_CHEM_SPC(  95 ) / 'ISOPOOH         ' /
      DATA GAS_CHEM_SPC(  96 ) / 'yRAOOH          ' /
      DATA GAS_CHEM_SPC(  97 ) / 'xACROLEIN       ' /
      DATA GAS_CHEM_SPC(  98 ) / 'ETHENE          ' /
      DATA GAS_CHEM_SPC(  99 ) / 'PROPENE         ' /
      DATA GAS_CHEM_SPC( 100 ) / 'BUTADIENE13     ' /
      DATA GAS_CHEM_SPC( 101 ) / 'ISOPRENE        ' /
      DATA GAS_CHEM_SPC( 102 ) / 'ISOPRXN         ' /
      DATA GAS_CHEM_SPC( 103 ) / 'IEPOX           ' /
      DATA GAS_CHEM_SPC( 104 ) / 'ARO2MN          ' /
      DATA GAS_CHEM_SPC( 105 ) / 'IEPOXOO         ' /
      DATA GAS_CHEM_SPC( 106 ) / 'APIN            ' /
      DATA GAS_CHEM_SPC( 107 ) / 'TRPRXN          ' /
      DATA GAS_CHEM_SPC( 108 ) / 'ACETYLENE       ' /
      DATA GAS_CHEM_SPC( 109 ) / 'BENZENE         ' /
      DATA GAS_CHEM_SPC( 110 ) / 'BENZRO2         ' /
      DATA GAS_CHEM_SPC( 111 ) / 'TOLUENE         ' /
      DATA GAS_CHEM_SPC( 112 ) / 'TOLRO2          ' /
      DATA GAS_CHEM_SPC( 113 ) / 'MXYL            ' /
      DATA GAS_CHEM_SPC( 114 ) / 'XYLRO2          ' /
      DATA GAS_CHEM_SPC( 115 ) / 'OXYL            ' /
      DATA GAS_CHEM_SPC( 116 ) / 'PXYL            ' /
      DATA GAS_CHEM_SPC( 117 ) / 'TMBENZ124       ' /
      DATA GAS_CHEM_SPC( 118 ) / 'ETOH            ' /
      DATA GAS_CHEM_SPC( 119 ) / 'ALK1            ' /
      DATA GAS_CHEM_SPC( 120 ) / 'ALK2            ' /
      DATA GAS_CHEM_SPC( 121 ) / 'ALK3            ' /
      DATA GAS_CHEM_SPC( 122 ) / 'ALK4            ' /
      DATA GAS_CHEM_SPC( 123 ) / 'ALK5            ' /
      DATA GAS_CHEM_SPC( 124 ) / 'SOAALK          ' /
      DATA GAS_CHEM_SPC( 125 ) / 'ALKRXN          ' /
      DATA GAS_CHEM_SPC( 126 ) / 'OLE1            ' /
      DATA GAS_CHEM_SPC( 127 ) / 'OLE2            ' /
      DATA GAS_CHEM_SPC( 128 ) / 'ARO1            ' /
      DATA GAS_CHEM_SPC( 129 ) / 'NAPHTHAL        ' /
      DATA GAS_CHEM_SPC( 130 ) / 'PAHRO2          ' /
      DATA GAS_CHEM_SPC( 131 ) / 'TERP            ' /
      DATA GAS_CHEM_SPC( 132 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC( 133 ) / 'SESQRXN         ' /
      DATA GAS_CHEM_SPC( 134 ) / 'CL2             ' /
      DATA GAS_CHEM_SPC( 135 ) / 'CL              ' /
      DATA GAS_CHEM_SPC( 136 ) / 'CLNO            ' /
      DATA GAS_CHEM_SPC( 137 ) / 'CLONO           ' /
      DATA GAS_CHEM_SPC( 138 ) / 'CLNO2           ' /
      DATA GAS_CHEM_SPC( 139 ) / 'HCL             ' /
      DATA GAS_CHEM_SPC( 140 ) / 'CLO             ' /
      DATA GAS_CHEM_SPC( 141 ) / 'CLONO2          ' /
      DATA GAS_CHEM_SPC( 142 ) / 'HOCL            ' /
      DATA GAS_CHEM_SPC( 143 ) / 'xCL             ' /
      DATA GAS_CHEM_SPC( 144 ) / 'xCLCCHO         ' /
      DATA GAS_CHEM_SPC( 145 ) / 'xCLACET         ' /
      DATA GAS_CHEM_SPC( 146 ) / 'CLCCHO          ' /
      DATA GAS_CHEM_SPC( 147 ) / 'CLACET          ' /
      DATA GAS_CHEM_SPC( 148 ) / 'CLCHO           ' /
      DATA GAS_CHEM_SPC( 149 ) / 'BNZNRXN         ' /
      DATA GAS_CHEM_SPC( 150 ) / 'BNZHRXN         ' /
      DATA GAS_CHEM_SPC( 151 ) / 'XYLNRXN         ' /
      DATA GAS_CHEM_SPC( 152 ) / 'XYLHRXN         ' /
      DATA GAS_CHEM_SPC( 153 ) / 'TOLNRXN         ' /
      DATA GAS_CHEM_SPC( 154 ) / 'TOLHRXN         ' /
      DATA GAS_CHEM_SPC( 155 ) / 'PAHNRXN         ' /
      DATA GAS_CHEM_SPC( 156 ) / 'PAHHRXN         ' /
      DATA GAS_CHEM_SPC( 157 ) / 'HCHO_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 158 ) / 'CCHO_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 159 ) / 'ACRO_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 160 ) / 'H2NO3PIJ        ' /
      DATA GAS_CHEM_SPC( 161 ) / 'H2NO3PK         ' /
      DATA GAS_CHEM_SPC( 162 ) / 'PCVOC           ' /
      DATA GAS_CHEM_SPC( 163 ) / 'PCSOARXN        ' /
      DATA GAS_CHEM_SPC( 164 ) / 'VLVPO1          ' /
      DATA GAS_CHEM_SPC( 165 ) / 'VSVPO1          ' /
      DATA GAS_CHEM_SPC( 166 ) / 'VSVPO2          ' /
      DATA GAS_CHEM_SPC( 167 ) / 'VSVPO3          ' /
      DATA GAS_CHEM_SPC( 168 ) / 'VIVPO1          ' /
      DATA GAS_CHEM_SPC( 169 ) / 'VLVOO1          ' /
      DATA GAS_CHEM_SPC( 170 ) / 'VLVOO2          ' /
      DATA GAS_CHEM_SPC( 171 ) / 'VSVOO2          ' /
      DATA GAS_CHEM_SPC( 172 ) / 'VSVOO3          ' /
      DATA GAS_CHEM_SPC( 173 ) / 'VSVOO1          ' /




      LOGICAL   :: HALOGEN_PARAMETER = .TRUE. 

      DATA CHEMISTRY_SPC(   1 ), SPECIES_MOLWT(   1 ) / 'NO2             ',   46.00 /
      DATA CHEMISTRY_SPC(   2 ), SPECIES_MOLWT(   2 ) / 'NO              ',   30.00 /
      DATA CHEMISTRY_SPC(   3 ), SPECIES_MOLWT(   3 ) / 'O3P             ',   16.00 /
      DATA CHEMISTRY_SPC(   4 ), SPECIES_MOLWT(   4 ) / 'O3              ',   48.00 /
      DATA CHEMISTRY_SPC(   5 ), SPECIES_MOLWT(   5 ) / 'NO3             ',   62.00 /
      DATA CHEMISTRY_SPC(   6 ), SPECIES_MOLWT(   6 ) / 'N2O5            ',  108.00 /
      DATA CHEMISTRY_SPC(   7 ), SPECIES_MOLWT(   7 ) / 'HNO3            ',   63.00 /
      DATA CHEMISTRY_SPC(   8 ), SPECIES_MOLWT(   8 ) / 'O1D             ',   16.00 /
      DATA CHEMISTRY_SPC(   9 ), SPECIES_MOLWT(   9 ) / 'OH              ',   17.00 /
      DATA CHEMISTRY_SPC(  10 ), SPECIES_MOLWT(  10 ) / 'HONO            ',   47.00 /
      DATA CHEMISTRY_SPC(  11 ), SPECIES_MOLWT(  11 ) / 'HO2             ',   33.00 /
      DATA CHEMISTRY_SPC(  12 ), SPECIES_MOLWT(  12 ) / 'CO              ',   28.00 /
      DATA CHEMISTRY_SPC(  13 ), SPECIES_MOLWT(  13 ) / 'CO2             ',   44.00 /
      DATA CHEMISTRY_SPC(  14 ), SPECIES_MOLWT(  14 ) / 'HNO4            ',   79.00 /
      DATA CHEMISTRY_SPC(  15 ), SPECIES_MOLWT(  15 ) / 'HO2H            ',   34.00 /
      DATA CHEMISTRY_SPC(  16 ), SPECIES_MOLWT(  16 ) / 'SO2             ',   64.10 /
      DATA CHEMISTRY_SPC(  17 ), SPECIES_MOLWT(  17 ) / 'SULF            ',   98.10 /
      DATA CHEMISTRY_SPC(  18 ), SPECIES_MOLWT(  18 ) / 'SULRXN          ',   98.10 /
      DATA CHEMISTRY_SPC(  19 ), SPECIES_MOLWT(  19 ) / 'MEO2            ',   47.00 /
      DATA CHEMISTRY_SPC(  20 ), SPECIES_MOLWT(  20 ) / 'HCHO            ',   30.00 /
      DATA CHEMISTRY_SPC(  21 ), SPECIES_MOLWT(  21 ) / 'COOH            ',   48.00 /
      DATA CHEMISTRY_SPC(  22 ), SPECIES_MOLWT(  22 ) / 'MEOH            ',   32.00 /
      DATA CHEMISTRY_SPC(  23 ), SPECIES_MOLWT(  23 ) / 'RO2C            ',    1.00 /
      DATA CHEMISTRY_SPC(  24 ), SPECIES_MOLWT(  24 ) / 'RO2XC           ',    1.00 /
      DATA CHEMISTRY_SPC(  25 ), SPECIES_MOLWT(  25 ) / 'MECO3           ',   75.00 /
      DATA CHEMISTRY_SPC(  26 ), SPECIES_MOLWT(  26 ) / 'PAN             ',  121.10 /
      DATA CHEMISTRY_SPC(  27 ), SPECIES_MOLWT(  27 ) / 'CCOOOH          ',   76.00 /
      DATA CHEMISTRY_SPC(  28 ), SPECIES_MOLWT(  28 ) / 'CCOOH           ',   60.10 /
      DATA CHEMISTRY_SPC(  29 ), SPECIES_MOLWT(  29 ) / 'RCO3            ',   89.10 /
      DATA CHEMISTRY_SPC(  30 ), SPECIES_MOLWT(  30 ) / 'PAN2            ',  135.10 /
      DATA CHEMISTRY_SPC(  31 ), SPECIES_MOLWT(  31 ) / 'xHO2            ',   33.00 /
      DATA CHEMISTRY_SPC(  32 ), SPECIES_MOLWT(  32 ) / 'yROOH           ',   76.10 /
      DATA CHEMISTRY_SPC(  33 ), SPECIES_MOLWT(  33 ) / 'xCCHO           ',   44.10 /
      DATA CHEMISTRY_SPC(  34 ), SPECIES_MOLWT(  34 ) / 'RCOOOH          ',   74.10 /
      DATA CHEMISTRY_SPC(  35 ), SPECIES_MOLWT(  35 ) / 'RCOOH           ',   74.10 /
      DATA CHEMISTRY_SPC(  36 ), SPECIES_MOLWT(  36 ) / 'BZCO3           ',  137.10 /
      DATA CHEMISTRY_SPC(  37 ), SPECIES_MOLWT(  37 ) / 'PBZN            ',  183.10 /
      DATA CHEMISTRY_SPC(  38 ), SPECIES_MOLWT(  38 ) / 'BZO             ',   93.00 /
      DATA CHEMISTRY_SPC(  39 ), SPECIES_MOLWT(  39 ) / 'MACO3           ',  101.10 /
      DATA CHEMISTRY_SPC(  40 ), SPECIES_MOLWT(  40 ) / 'MAPAN           ',  147.10 /
      DATA CHEMISTRY_SPC(  41 ), SPECIES_MOLWT(  41 ) / 'TBUO            ',   73.00 /
      DATA CHEMISTRY_SPC(  42 ), SPECIES_MOLWT(  42 ) / 'RNO3            ',  147.20 /
      DATA CHEMISTRY_SPC(  43 ), SPECIES_MOLWT(  43 ) / 'ACETONE         ',   58.10 /
      DATA CHEMISTRY_SPC(  44 ), SPECIES_MOLWT(  44 ) / 'NPHE            ',  139.10 /
      DATA CHEMISTRY_SPC(  45 ), SPECIES_MOLWT(  45 ) / 'CRES            ',  108.10 /
      DATA CHEMISTRY_SPC(  46 ), SPECIES_MOLWT(  46 ) / 'xOH             ',   17.00 /
      DATA CHEMISTRY_SPC(  47 ), SPECIES_MOLWT(  47 ) / 'xNO2            ',   46.00 /
      DATA CHEMISTRY_SPC(  48 ), SPECIES_MOLWT(  48 ) / 'xMEO2           ',   47.00 /
      DATA CHEMISTRY_SPC(  49 ), SPECIES_MOLWT(  49 ) / 'xMECO3          ',   75.00 /
      DATA CHEMISTRY_SPC(  50 ), SPECIES_MOLWT(  50 ) / 'xRCO3           ',   89.10 /
      DATA CHEMISTRY_SPC(  51 ), SPECIES_MOLWT(  51 ) / 'xMACO3          ',  101.10 /
      DATA CHEMISTRY_SPC(  52 ), SPECIES_MOLWT(  52 ) / 'xTBUO           ',   73.00 /
      DATA CHEMISTRY_SPC(  53 ), SPECIES_MOLWT(  53 ) / 'xCO             ',   28.00 /
      DATA CHEMISTRY_SPC(  54 ), SPECIES_MOLWT(  54 ) / 'CCHO            ',   44.10 /
      DATA CHEMISTRY_SPC(  55 ), SPECIES_MOLWT(  55 ) / 'RCHO            ',   58.10 /
      DATA CHEMISTRY_SPC(  56 ), SPECIES_MOLWT(  56 ) / 'xHCHO           ',   30.00 /
      DATA CHEMISTRY_SPC(  57 ), SPECIES_MOLWT(  57 ) / 'MEK             ',   72.10 /
      DATA CHEMISTRY_SPC(  58 ), SPECIES_MOLWT(  58 ) / 'zRNO3           ',  147.20 /
      DATA CHEMISTRY_SPC(  59 ), SPECIES_MOLWT(  59 ) / 'xRCHO           ',   58.10 /
      DATA CHEMISTRY_SPC(  60 ), SPECIES_MOLWT(  60 ) / 'HCOOH           ',   46.00 /
      DATA CHEMISTRY_SPC(  61 ), SPECIES_MOLWT(  61 ) / 'xMGLY           ',   72.10 /
      DATA CHEMISTRY_SPC(  62 ), SPECIES_MOLWT(  62 ) / 'xBACL           ',   86.10 /
      DATA CHEMISTRY_SPC(  63 ), SPECIES_MOLWT(  63 ) / 'ROOH            ',   76.10 /
      DATA CHEMISTRY_SPC(  64 ), SPECIES_MOLWT(  64 ) / 'xPROD2          ',  116.20 /
      DATA CHEMISTRY_SPC(  65 ), SPECIES_MOLWT(  65 ) / 'R6OOH           ',  118.20 /
      DATA CHEMISTRY_SPC(  66 ), SPECIES_MOLWT(  66 ) / 'PRD2            ',  116.20 /
      DATA CHEMISTRY_SPC(  67 ), SPECIES_MOLWT(  67 ) / 'yR6OOH          ',  118.20 /
      DATA CHEMISTRY_SPC(  68 ), SPECIES_MOLWT(  68 ) / 'RAOOH           ',  188.20 /
      DATA CHEMISTRY_SPC(  69 ), SPECIES_MOLWT(  69 ) / 'MGLY            ',   72.10 /
      DATA CHEMISTRY_SPC(  70 ), SPECIES_MOLWT(  70 ) / 'IPRD            ',  100.10 /
      DATA CHEMISTRY_SPC(  71 ), SPECIES_MOLWT(  71 ) / 'xGLY            ',   58.00 /
      DATA CHEMISTRY_SPC(  72 ), SPECIES_MOLWT(  72 ) / 'xMEK            ',   72.10 /
      DATA CHEMISTRY_SPC(  73 ), SPECIES_MOLWT(  73 ) / 'xAFG1           ',   98.10 /
      DATA CHEMISTRY_SPC(  74 ), SPECIES_MOLWT(  74 ) / 'xAFG2           ',   98.10 /
      DATA CHEMISTRY_SPC(  75 ), SPECIES_MOLWT(  75 ) / 'GLY             ',   58.00 /
      DATA CHEMISTRY_SPC(  76 ), SPECIES_MOLWT(  76 ) / 'AFG1            ',   98.10 /
      DATA CHEMISTRY_SPC(  77 ), SPECIES_MOLWT(  77 ) / 'AFG2            ',   98.10 /
      DATA CHEMISTRY_SPC(  78 ), SPECIES_MOLWT(  78 ) / 'HCOCO3          ',   89.00 /
      DATA CHEMISTRY_SPC(  79 ), SPECIES_MOLWT(  79 ) / 'BACL            ',   86.10 /
      DATA CHEMISTRY_SPC(  80 ), SPECIES_MOLWT(  80 ) / 'BALD            ',  106.10 /
      DATA CHEMISTRY_SPC(  81 ), SPECIES_MOLWT(  81 ) / 'AFG3            ',  124.10 /
      DATA CHEMISTRY_SPC(  82 ), SPECIES_MOLWT(  82 ) / 'xIPRD           ',  100.10 /
      DATA CHEMISTRY_SPC(  83 ), SPECIES_MOLWT(  83 ) / 'MACR            ',   70.10 /
      DATA CHEMISTRY_SPC(  84 ), SPECIES_MOLWT(  84 ) / 'MVK             ',   70.10 /
      DATA CHEMISTRY_SPC(  85 ), SPECIES_MOLWT(  85 ) / 'xHOCCHO         ',   60.10 /
      DATA CHEMISTRY_SPC(  86 ), SPECIES_MOLWT(  86 ) / 'xRNO3           ',  147.20 /
      DATA CHEMISTRY_SPC(  87 ), SPECIES_MOLWT(  87 ) / 'HOCCHO          ',   60.10 /
      DATA CHEMISTRY_SPC(  88 ), SPECIES_MOLWT(  88 ) / 'xACETONE        ',   58.10 /
      DATA CHEMISTRY_SPC(  89 ), SPECIES_MOLWT(  89 ) / 'ACROLEIN        ',   56.10 /
      DATA CHEMISTRY_SPC(  90 ), SPECIES_MOLWT(  90 ) / 'xBALD           ',  106.10 /
      DATA CHEMISTRY_SPC(  91 ), SPECIES_MOLWT(  91 ) / 'xAFG3           ',  124.70 /
      DATA CHEMISTRY_SPC(  92 ), SPECIES_MOLWT(  92 ) / 'xMACR           ',   70.10 /
      DATA CHEMISTRY_SPC(  93 ), SPECIES_MOLWT(  93 ) / 'xMVK            ',   70.10 /
      DATA CHEMISTRY_SPC(  94 ), SPECIES_MOLWT(  94 ) / 'yISOPOOH        ',  118.20 /
      DATA CHEMISTRY_SPC(  95 ), SPECIES_MOLWT(  95 ) / 'ISOPOOH         ',  118.20 /
      DATA CHEMISTRY_SPC(  96 ), SPECIES_MOLWT(  96 ) / 'yRAOOH          ',  188.20 /
      DATA CHEMISTRY_SPC(  97 ), SPECIES_MOLWT(  97 ) / 'xACROLEIN       ',   56.10 /
      DATA CHEMISTRY_SPC(  98 ), SPECIES_MOLWT(  98 ) / 'ETHENE          ',   28.10 /
      DATA CHEMISTRY_SPC(  99 ), SPECIES_MOLWT(  99 ) / 'PROPENE         ',   42.10 /
      DATA CHEMISTRY_SPC( 100 ), SPECIES_MOLWT( 100 ) / 'BUTADIENE13     ',   54.10 /
      DATA CHEMISTRY_SPC( 101 ), SPECIES_MOLWT( 101 ) / 'ISOPRENE        ',   68.10 /
      DATA CHEMISTRY_SPC( 102 ), SPECIES_MOLWT( 102 ) / 'ISOPRXN         ',   68.00 /
      DATA CHEMISTRY_SPC( 103 ), SPECIES_MOLWT( 103 ) / 'IEPOX           ',  118.13 /
      DATA CHEMISTRY_SPC( 104 ), SPECIES_MOLWT( 104 ) / 'ARO2MN          ',  118.70 /
      DATA CHEMISTRY_SPC( 105 ), SPECIES_MOLWT( 105 ) / 'IEPOXOO         ',  149.12 /
      DATA CHEMISTRY_SPC( 106 ), SPECIES_MOLWT( 106 ) / 'APIN            ',  136.20 /
      DATA CHEMISTRY_SPC( 107 ), SPECIES_MOLWT( 107 ) / 'TRPRXN          ',  136.20 /
      DATA CHEMISTRY_SPC( 108 ), SPECIES_MOLWT( 108 ) / 'ACETYLENE       ',   26.00 /
      DATA CHEMISTRY_SPC( 109 ), SPECIES_MOLWT( 109 ) / 'BENZENE         ',   78.10 /
      DATA CHEMISTRY_SPC( 110 ), SPECIES_MOLWT( 110 ) / 'BENZRO2         ',  159.10 /
      DATA CHEMISTRY_SPC( 111 ), SPECIES_MOLWT( 111 ) / 'TOLUENE         ',   92.10 /
      DATA CHEMISTRY_SPC( 112 ), SPECIES_MOLWT( 112 ) / 'TOLRO2          ',  172.10 /
      DATA CHEMISTRY_SPC( 113 ), SPECIES_MOLWT( 113 ) / 'MXYL            ',  106.20 /
      DATA CHEMISTRY_SPC( 114 ), SPECIES_MOLWT( 114 ) / 'XYLRO2          ',  187.20 /
      DATA CHEMISTRY_SPC( 115 ), SPECIES_MOLWT( 115 ) / 'OXYL            ',  106.20 /
      DATA CHEMISTRY_SPC( 116 ), SPECIES_MOLWT( 116 ) / 'PXYL            ',  106.20 /
      DATA CHEMISTRY_SPC( 117 ), SPECIES_MOLWT( 117 ) / 'TMBENZ124       ',  120.20 /
      DATA CHEMISTRY_SPC( 118 ), SPECIES_MOLWT( 118 ) / 'ETOH            ',   46.10 /
      DATA CHEMISTRY_SPC( 119 ), SPECIES_MOLWT( 119 ) / 'ALK1            ',   30.10 /
      DATA CHEMISTRY_SPC( 120 ), SPECIES_MOLWT( 120 ) / 'ALK2            ',   36.70 /
      DATA CHEMISTRY_SPC( 121 ), SPECIES_MOLWT( 121 ) / 'ALK3            ',   58.60 /
      DATA CHEMISTRY_SPC( 122 ), SPECIES_MOLWT( 122 ) / 'ALK4            ',   77.60 /
      DATA CHEMISTRY_SPC( 123 ), SPECIES_MOLWT( 123 ) / 'ALK5            ',  118.90 /
      DATA CHEMISTRY_SPC( 124 ), SPECIES_MOLWT( 124 ) / 'SOAALK          ',  112.00 /
      DATA CHEMISTRY_SPC( 125 ), SPECIES_MOLWT( 125 ) / 'ALKRXN          ',  112.00 /
      DATA CHEMISTRY_SPC( 126 ), SPECIES_MOLWT( 126 ) / 'OLE1            ',   72.30 /
      DATA CHEMISTRY_SPC( 127 ), SPECIES_MOLWT( 127 ) / 'OLE2            ',   75.80 /
      DATA CHEMISTRY_SPC( 128 ), SPECIES_MOLWT( 128 ) / 'ARO1            ',   95.20 /
      DATA CHEMISTRY_SPC( 129 ), SPECIES_MOLWT( 129 ) / 'NAPHTHAL        ',  118.70 /
      DATA CHEMISTRY_SPC( 130 ), SPECIES_MOLWT( 130 ) / 'PAHRO2          ',  187.20 /
      DATA CHEMISTRY_SPC( 131 ), SPECIES_MOLWT( 131 ) / 'TERP            ',  136.20 /
      DATA CHEMISTRY_SPC( 132 ), SPECIES_MOLWT( 132 ) / 'SESQ            ',  204.40 /
      DATA CHEMISTRY_SPC( 133 ), SPECIES_MOLWT( 133 ) / 'SESQRXN         ',  204.40 /
      DATA CHEMISTRY_SPC( 134 ), SPECIES_MOLWT( 134 ) / 'CL2             ',   70.00 /
      DATA CHEMISTRY_SPC( 135 ), SPECIES_MOLWT( 135 ) / 'CL              ',   35.50 /
      DATA CHEMISTRY_SPC( 136 ), SPECIES_MOLWT( 136 ) / 'CLNO            ',   65.50 /
      DATA CHEMISTRY_SPC( 137 ), SPECIES_MOLWT( 137 ) / 'CLONO           ',   81.50 /
      DATA CHEMISTRY_SPC( 138 ), SPECIES_MOLWT( 138 ) / 'CLNO2           ',   81.50 /
      DATA CHEMISTRY_SPC( 139 ), SPECIES_MOLWT( 139 ) / 'HCL             ',   36.50 /
      DATA CHEMISTRY_SPC( 140 ), SPECIES_MOLWT( 140 ) / 'CLO             ',   51.50 /
      DATA CHEMISTRY_SPC( 141 ), SPECIES_MOLWT( 141 ) / 'CLONO2          ',   97.50 /
      DATA CHEMISTRY_SPC( 142 ), SPECIES_MOLWT( 142 ) / 'HOCL            ',   52.50 /
      DATA CHEMISTRY_SPC( 143 ), SPECIES_MOLWT( 143 ) / 'xCL             ',   35.50 /
      DATA CHEMISTRY_SPC( 144 ), SPECIES_MOLWT( 144 ) / 'xCLCCHO         ',   78.50 /
      DATA CHEMISTRY_SPC( 145 ), SPECIES_MOLWT( 145 ) / 'xCLACET         ',   92.50 /
      DATA CHEMISTRY_SPC( 146 ), SPECIES_MOLWT( 146 ) / 'CLCCHO          ',   78.50 /
      DATA CHEMISTRY_SPC( 147 ), SPECIES_MOLWT( 147 ) / 'CLACET          ',   92.50 /
      DATA CHEMISTRY_SPC( 148 ), SPECIES_MOLWT( 148 ) / 'CLCHO           ',   64.50 /
      DATA CHEMISTRY_SPC( 149 ), SPECIES_MOLWT( 149 ) / 'BNZNRXN         ',  159.10 /
      DATA CHEMISTRY_SPC( 150 ), SPECIES_MOLWT( 150 ) / 'BNZHRXN         ',  159.10 /
      DATA CHEMISTRY_SPC( 151 ), SPECIES_MOLWT( 151 ) / 'XYLNRXN         ',  187.20 /
      DATA CHEMISTRY_SPC( 152 ), SPECIES_MOLWT( 152 ) / 'XYLHRXN         ',  187.20 /
      DATA CHEMISTRY_SPC( 153 ), SPECIES_MOLWT( 153 ) / 'TOLNRXN         ',  172.10 /
      DATA CHEMISTRY_SPC( 154 ), SPECIES_MOLWT( 154 ) / 'TOLHRXN         ',  172.10 /
      DATA CHEMISTRY_SPC( 155 ), SPECIES_MOLWT( 155 ) / 'PAHNRXN         ',  172.10 /
      DATA CHEMISTRY_SPC( 156 ), SPECIES_MOLWT( 156 ) / 'PAHHRXN         ',  172.10 /
      DATA CHEMISTRY_SPC( 157 ), SPECIES_MOLWT( 157 ) / 'HCHO_PRIMARY    ',   30.00 /
      DATA CHEMISTRY_SPC( 158 ), SPECIES_MOLWT( 158 ) / 'CCHO_PRIMARY    ',   44.10 /
      DATA CHEMISTRY_SPC( 159 ), SPECIES_MOLWT( 159 ) / 'ACRO_PRIMARY    ',   56.10 /
      DATA CHEMISTRY_SPC( 160 ), SPECIES_MOLWT( 160 ) / 'H2NO3PIJ        ',   64.00 /
      DATA CHEMISTRY_SPC( 161 ), SPECIES_MOLWT( 161 ) / 'H2NO3PK         ',   64.00 /
      DATA CHEMISTRY_SPC( 162 ), SPECIES_MOLWT( 162 ) / 'ACLI            ',   35.50 /
      DATA CHEMISTRY_SPC( 163 ), SPECIES_MOLWT( 163 ) / 'ACLJ            ',   35.50 /
      DATA CHEMISTRY_SPC( 164 ), SPECIES_MOLWT( 164 ) / 'ACLK            ',   35.50 /
      DATA CHEMISTRY_SPC( 165 ), SPECIES_MOLWT( 165 ) / 'AXYL1J          ',  174.00 /
      DATA CHEMISTRY_SPC( 166 ), SPECIES_MOLWT( 166 ) / 'AOLGAJ          ',  206.00 /
      DATA CHEMISTRY_SPC( 167 ), SPECIES_MOLWT( 167 ) / 'AXYL2J          ',  185.00 /
      DATA CHEMISTRY_SPC( 168 ), SPECIES_MOLWT( 168 ) / 'ATOL1J          ',  163.00 /
      DATA CHEMISTRY_SPC( 169 ), SPECIES_MOLWT( 169 ) / 'ATOL2J          ',  175.00 /
      DATA CHEMISTRY_SPC( 170 ), SPECIES_MOLWT( 170 ) / 'ABNZ1J          ',  161.00 /
      DATA CHEMISTRY_SPC( 171 ), SPECIES_MOLWT( 171 ) / 'ABNZ2J          ',  134.00 /
      DATA CHEMISTRY_SPC( 172 ), SPECIES_MOLWT( 172 ) / 'ATRP1J          ',  177.00 /
      DATA CHEMISTRY_SPC( 173 ), SPECIES_MOLWT( 173 ) / 'AOLGBJ          ',  248.00 /
      DATA CHEMISTRY_SPC( 174 ), SPECIES_MOLWT( 174 ) / 'ATRP2J          ',  198.00 /
      DATA CHEMISTRY_SPC( 175 ), SPECIES_MOLWT( 175 ) / 'AISO1J          ',  132.00 /
      DATA CHEMISTRY_SPC( 176 ), SPECIES_MOLWT( 176 ) / 'AISO2J          ',  133.00 /
      DATA CHEMISTRY_SPC( 177 ), SPECIES_MOLWT( 177 ) / 'ASQTJ           ',  273.00 /
      DATA CHEMISTRY_SPC( 178 ), SPECIES_MOLWT( 178 ) / 'APAH1J          ',  195.60 /
      DATA CHEMISTRY_SPC( 179 ), SPECIES_MOLWT( 179 ) / 'APAH2J          ',  178.70 /
      DATA CHEMISTRY_SPC( 180 ), SPECIES_MOLWT( 180 ) / 'AALK1J          ',  225.00 /
      DATA CHEMISTRY_SPC( 181 ), SPECIES_MOLWT( 181 ) / 'AALK2J          ',  205.10 /
      DATA CHEMISTRY_SPC( 182 ), SPECIES_MOLWT( 182 ) / 'PCVOC           ',  170.00 /
      DATA CHEMISTRY_SPC( 183 ), SPECIES_MOLWT( 183 ) / 'PCSOARXN        ',  170.00 /
      DATA CHEMISTRY_SPC( 184 ), SPECIES_MOLWT( 184 ) / 'VLVPO1          ',  218.00 /
      DATA CHEMISTRY_SPC( 185 ), SPECIES_MOLWT( 185 ) / 'VSVPO1          ',  230.00 /
      DATA CHEMISTRY_SPC( 186 ), SPECIES_MOLWT( 186 ) / 'VSVPO2          ',  241.00 /
      DATA CHEMISTRY_SPC( 187 ), SPECIES_MOLWT( 187 ) / 'VSVPO3          ',  253.00 /
      DATA CHEMISTRY_SPC( 188 ), SPECIES_MOLWT( 188 ) / 'VIVPO1          ',  266.00 /
      DATA CHEMISTRY_SPC( 189 ), SPECIES_MOLWT( 189 ) / 'VLVOO1          ',  136.00 /
      DATA CHEMISTRY_SPC( 190 ), SPECIES_MOLWT( 190 ) / 'VLVOO2          ',  136.00 /
      DATA CHEMISTRY_SPC( 191 ), SPECIES_MOLWT( 191 ) / 'VSVOO2          ',  135.00 /
      DATA CHEMISTRY_SPC( 192 ), SPECIES_MOLWT( 192 ) / 'VSVOO3          ',  134.00 /
      DATA CHEMISTRY_SPC( 193 ), SPECIES_MOLWT( 193 ) / 'VSVOO1          ',  135.00 /
      DATA CHEMISTRY_SPC( 194 ), SPECIES_MOLWT( 194 ) / 'AISO3J          ',  168.20 /



      LOGICAL   :: MAPPED_TO_CGRID   = .FALSE. 


! MAPPED_TO_CGRID declares whether CMAQ namelists were used to determine 
! the below values of CGRID_INDEX, SPECIES_TYPE, SPECIES_MOLWT, and CONVERT_CONC
      LOGICAL, PARAMETER, PRIVATE :: F = .FALSE.
      LOGICAL, PARAMETER, PRIVATE :: T = .TRUE.

      DATA CGRID_INDEX(   1 ), SPECIES_TYPE(   1 ), CONVERT_CONC(   1 ) /    1, 'GC', F /  ! NO2
      DATA CGRID_INDEX(   2 ), SPECIES_TYPE(   2 ), CONVERT_CONC(   2 ) /    2, 'GC', F /  ! NO
      DATA CGRID_INDEX(   3 ), SPECIES_TYPE(   3 ), CONVERT_CONC(   3 ) /    3, 'GC', F /  ! O3P
      DATA CGRID_INDEX(   4 ), SPECIES_TYPE(   4 ), CONVERT_CONC(   4 ) /    4, 'GC', F /  ! O3
      DATA CGRID_INDEX(   5 ), SPECIES_TYPE(   5 ), CONVERT_CONC(   5 ) /    5, 'GC', F /  ! NO3
      DATA CGRID_INDEX(   6 ), SPECIES_TYPE(   6 ), CONVERT_CONC(   6 ) /    6, 'GC', F /  ! N2O5
      DATA CGRID_INDEX(   7 ), SPECIES_TYPE(   7 ), CONVERT_CONC(   7 ) /    7, 'GC', F /  ! HNO3
      DATA CGRID_INDEX(   8 ), SPECIES_TYPE(   8 ), CONVERT_CONC(   8 ) /    8, 'GC', F /  ! O1D
      DATA CGRID_INDEX(   9 ), SPECIES_TYPE(   9 ), CONVERT_CONC(   9 ) /    9, 'GC', F /  ! OH
      DATA CGRID_INDEX(  10 ), SPECIES_TYPE(  10 ), CONVERT_CONC(  10 ) /   10, 'GC', F /  ! HONO
      DATA CGRID_INDEX(  11 ), SPECIES_TYPE(  11 ), CONVERT_CONC(  11 ) /   11, 'GC', F /  ! HO2
      DATA CGRID_INDEX(  12 ), SPECIES_TYPE(  12 ), CONVERT_CONC(  12 ) /   12, 'GC', F /  ! CO
      DATA CGRID_INDEX(  13 ), SPECIES_TYPE(  13 ), CONVERT_CONC(  13 ) /   13, 'GC', F /  ! CO2
      DATA CGRID_INDEX(  14 ), SPECIES_TYPE(  14 ), CONVERT_CONC(  14 ) /   14, 'GC', F /  ! HNO4
      DATA CGRID_INDEX(  15 ), SPECIES_TYPE(  15 ), CONVERT_CONC(  15 ) /   15, 'GC', F /  ! HO2H
      DATA CGRID_INDEX(  16 ), SPECIES_TYPE(  16 ), CONVERT_CONC(  16 ) /   16, 'GC', F /  ! SO2
      DATA CGRID_INDEX(  17 ), SPECIES_TYPE(  17 ), CONVERT_CONC(  17 ) /   17, 'GC', F /  ! SULF
      DATA CGRID_INDEX(  18 ), SPECIES_TYPE(  18 ), CONVERT_CONC(  18 ) /   18, 'GC', F /  ! SULRXN
      DATA CGRID_INDEX(  19 ), SPECIES_TYPE(  19 ), CONVERT_CONC(  19 ) /   19, 'GC', F /  ! MEO2
      DATA CGRID_INDEX(  20 ), SPECIES_TYPE(  20 ), CONVERT_CONC(  20 ) /   20, 'GC', F /  ! HCHO
      DATA CGRID_INDEX(  21 ), SPECIES_TYPE(  21 ), CONVERT_CONC(  21 ) /   21, 'GC', F /  ! COOH
      DATA CGRID_INDEX(  22 ), SPECIES_TYPE(  22 ), CONVERT_CONC(  22 ) /   22, 'GC', F /  ! MEOH
      DATA CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), CONVERT_CONC(  23 ) /   23, 'GC', F /  ! RO2C
      DATA CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), CONVERT_CONC(  24 ) /   24, 'GC', F /  ! RO2XC
      DATA CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), CONVERT_CONC(  25 ) /   25, 'GC', F /  ! MECO3
      DATA CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), CONVERT_CONC(  26 ) /   26, 'GC', F /  ! PAN
      DATA CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), CONVERT_CONC(  27 ) /   27, 'GC', F /  ! CCOOOH
      DATA CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), CONVERT_CONC(  28 ) /   28, 'GC', F /  ! CCOOH
      DATA CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), CONVERT_CONC(  29 ) /   29, 'GC', F /  ! RCO3
      DATA CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), CONVERT_CONC(  30 ) /   30, 'GC', F /  ! PAN2
      DATA CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), CONVERT_CONC(  31 ) /   31, 'GC', F /  ! xHO2
      DATA CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), CONVERT_CONC(  32 ) /   32, 'GC', F /  ! yROOH
      DATA CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), CONVERT_CONC(  33 ) /   33, 'GC', F /  ! xCCHO
      DATA CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), CONVERT_CONC(  34 ) /   34, 'GC', F /  ! RCOOOH
      DATA CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), CONVERT_CONC(  35 ) /   35, 'GC', F /  ! RCOOH
      DATA CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), CONVERT_CONC(  36 ) /   36, 'GC', F /  ! BZCO3
      DATA CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), CONVERT_CONC(  37 ) /   37, 'GC', F /  ! PBZN
      DATA CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), CONVERT_CONC(  38 ) /   38, 'GC', F /  ! BZO
      DATA CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), CONVERT_CONC(  39 ) /   39, 'GC', F /  ! MACO3
      DATA CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), CONVERT_CONC(  40 ) /   40, 'GC', F /  ! MAPAN
      DATA CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), CONVERT_CONC(  41 ) /   41, 'GC', F /  ! TBUO
      DATA CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), CONVERT_CONC(  42 ) /   42, 'GC', F /  ! RNO3
      DATA CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), CONVERT_CONC(  43 ) /   43, 'GC', F /  ! ACETONE
      DATA CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), CONVERT_CONC(  44 ) /   44, 'GC', F /  ! NPHE
      DATA CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), CONVERT_CONC(  45 ) /   45, 'GC', F /  ! CRES
      DATA CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), CONVERT_CONC(  46 ) /   46, 'GC', F /  ! xOH
      DATA CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), CONVERT_CONC(  47 ) /   47, 'GC', F /  ! xNO2
      DATA CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), CONVERT_CONC(  48 ) /   48, 'GC', F /  ! xMEO2
      DATA CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), CONVERT_CONC(  49 ) /   49, 'GC', F /  ! xMECO3
      DATA CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), CONVERT_CONC(  50 ) /   50, 'GC', F /  ! xRCO3
      DATA CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), CONVERT_CONC(  51 ) /   51, 'GC', F /  ! xMACO3
      DATA CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), CONVERT_CONC(  52 ) /   52, 'GC', F /  ! xTBUO
      DATA CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), CONVERT_CONC(  53 ) /   53, 'GC', F /  ! xCO
      DATA CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), CONVERT_CONC(  54 ) /   54, 'GC', F /  ! CCHO
      DATA CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), CONVERT_CONC(  55 ) /   55, 'GC', F /  ! RCHO
      DATA CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), CONVERT_CONC(  56 ) /   56, 'GC', F /  ! xHCHO
      DATA CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), CONVERT_CONC(  57 ) /   57, 'GC', F /  ! MEK
      DATA CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), CONVERT_CONC(  58 ) /   58, 'GC', F /  ! zRNO3
      DATA CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), CONVERT_CONC(  59 ) /   59, 'GC', F /  ! xRCHO
      DATA CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), CONVERT_CONC(  60 ) /   60, 'GC', F /  ! HCOOH
      DATA CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), CONVERT_CONC(  61 ) /   61, 'GC', F /  ! xMGLY
      DATA CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), CONVERT_CONC(  62 ) /   62, 'GC', F /  ! xBACL
      DATA CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), CONVERT_CONC(  63 ) /   63, 'GC', F /  ! ROOH
      DATA CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), CONVERT_CONC(  64 ) /   64, 'GC', F /  ! xPROD2
      DATA CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), CONVERT_CONC(  65 ) /   65, 'GC', F /  ! R6OOH
      DATA CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), CONVERT_CONC(  66 ) /   69, 'GC', F /  ! PRD2
      DATA CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), CONVERT_CONC(  67 ) /   70, 'GC', F /  ! yR6OOH
      DATA CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), CONVERT_CONC(  68 ) /   72, 'GC', F /  ! RAOOH
      DATA CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), CONVERT_CONC(  69 ) /   73, 'GC', F /  ! MGLY
      DATA CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), CONVERT_CONC(  70 ) /   74, 'GC', F /  ! IPRD
      DATA CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), CONVERT_CONC(  71 ) /   75, 'GC', F /  ! xGLY
      DATA CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), CONVERT_CONC(  72 ) /   76, 'GC', F /  ! xMEK
      DATA CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), CONVERT_CONC(  73 ) /   77, 'GC', F /  ! xAFG1
      DATA CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), CONVERT_CONC(  74 ) /   78, 'GC', F /  ! xAFG2
      DATA CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), CONVERT_CONC(  75 ) /   79, 'GC', F /  ! GLY
      DATA CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), CONVERT_CONC(  76 ) /   80, 'GC', F /  ! AFG1
      DATA CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), CONVERT_CONC(  77 ) /   81, 'GC', F /  ! AFG2
      DATA CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), CONVERT_CONC(  78 ) /   82, 'GC', F /  ! HCOCO3
      DATA CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), CONVERT_CONC(  79 ) /   83, 'GC', F /  ! BACL
      DATA CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), CONVERT_CONC(  80 ) /   84, 'GC', F /  ! BALD
      DATA CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), CONVERT_CONC(  81 ) /   85, 'GC', F /  ! AFG3
      DATA CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), CONVERT_CONC(  82 ) /   86, 'GC', F /  ! xIPRD
      DATA CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), CONVERT_CONC(  83 ) /   87, 'GC', F /  ! MACR
      DATA CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), CONVERT_CONC(  84 ) /   88, 'GC', F /  ! MVK
      DATA CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), CONVERT_CONC(  85 ) /   89, 'GC', F /  ! xHOCCHO
      DATA CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), CONVERT_CONC(  86 ) /   90, 'GC', F /  ! xRNO3
      DATA CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), CONVERT_CONC(  87 ) /   91, 'GC', F /  ! HOCCHO
      DATA CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), CONVERT_CONC(  88 ) /   92, 'GC', F /  ! xACETONE
      DATA CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), CONVERT_CONC(  89 ) /   93, 'GC', F /  ! ACROLEIN
      DATA CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), CONVERT_CONC(  90 ) /   94, 'GC', F /  ! xBALD
      DATA CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), CONVERT_CONC(  91 ) /   95, 'GC', F /  ! xAFG3
      DATA CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), CONVERT_CONC(  92 ) /   96, 'GC', F /  ! xMACR
      DATA CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), CONVERT_CONC(  93 ) /   97, 'GC', F /  ! xMVK
      DATA CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), CONVERT_CONC(  94 ) /   71, 'GC', F /  ! yISOPOOH
      DATA CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), CONVERT_CONC(  95 ) /   66, 'GC', F /  ! ISOPOOH
      DATA CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), CONVERT_CONC(  96 ) /   98, 'GC', F /  ! yRAOOH
      DATA CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), CONVERT_CONC(  97 ) /   99, 'GC', F /  ! xACROLEIN
      DATA CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), CONVERT_CONC(  98 ) /  100, 'GC', F /  ! ETHENE
      DATA CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), CONVERT_CONC(  99 ) /  101, 'GC', F /  ! PROPENE
      DATA CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), CONVERT_CONC( 100 ) /  102, 'GC', F /  ! BUTADIENE13
      DATA CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), CONVERT_CONC( 101 ) /  103, 'GC', F /  ! ISOPRENE
      DATA CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), CONVERT_CONC( 102 ) /  104, 'GC', F /  ! ISOPRXN
      DATA CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), CONVERT_CONC( 103 ) /   67, 'GC', F /  ! IEPOX
      DATA CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), CONVERT_CONC( 104 ) /  128, 'GC', F /  ! ARO2MN
      DATA CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), CONVERT_CONC( 105 ) /   68, 'GC', F /  ! IEPOXOO
      DATA CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), CONVERT_CONC( 106 ) /  105, 'GC', F /  ! APIN
      DATA CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), CONVERT_CONC( 107 ) /  106, 'GC', F /  ! TRPRXN
      DATA CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), CONVERT_CONC( 108 ) /  107, 'GC', F /  ! ACETYLENE
      DATA CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), CONVERT_CONC( 109 ) /  108, 'GC', F /  ! BENZENE
      DATA CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), CONVERT_CONC( 110 ) /  109, 'GC', F /  ! BENZRO2
      DATA CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), CONVERT_CONC( 111 ) /  110, 'GC', F /  ! TOLUENE
      DATA CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), CONVERT_CONC( 112 ) /  111, 'GC', F /  ! TOLRO2
      DATA CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), CONVERT_CONC( 113 ) /  112, 'GC', F /  ! MXYL
      DATA CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), CONVERT_CONC( 114 ) /  113, 'GC', F /  ! XYLRO2
      DATA CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), CONVERT_CONC( 115 ) /  114, 'GC', F /  ! OXYL
      DATA CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), CONVERT_CONC( 116 ) /  115, 'GC', F /  ! PXYL
      DATA CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), CONVERT_CONC( 117 ) /  116, 'GC', F /  ! TMBENZ124
      DATA CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), CONVERT_CONC( 118 ) /  117, 'GC', F /  ! ETOH
      DATA CGRID_INDEX( 119 ), SPECIES_TYPE( 119 ), CONVERT_CONC( 119 ) /  118, 'GC', F /  ! ALK1
      DATA CGRID_INDEX( 120 ), SPECIES_TYPE( 120 ), CONVERT_CONC( 120 ) /  119, 'GC', F /  ! ALK2
      DATA CGRID_INDEX( 121 ), SPECIES_TYPE( 121 ), CONVERT_CONC( 121 ) /  120, 'GC', F /  ! ALK3
      DATA CGRID_INDEX( 122 ), SPECIES_TYPE( 122 ), CONVERT_CONC( 122 ) /  121, 'GC', F /  ! ALK4
      DATA CGRID_INDEX( 123 ), SPECIES_TYPE( 123 ), CONVERT_CONC( 123 ) /  122, 'GC', F /  ! ALK5
      DATA CGRID_INDEX( 124 ), SPECIES_TYPE( 124 ), CONVERT_CONC( 124 ) /  123, 'GC', F /  ! SOAALK
      DATA CGRID_INDEX( 125 ), SPECIES_TYPE( 125 ), CONVERT_CONC( 125 ) /  124, 'GC', F /  ! ALKRXN
      DATA CGRID_INDEX( 126 ), SPECIES_TYPE( 126 ), CONVERT_CONC( 126 ) /  125, 'GC', F /  ! OLE1
      DATA CGRID_INDEX( 127 ), SPECIES_TYPE( 127 ), CONVERT_CONC( 127 ) /  126, 'GC', F /  ! OLE2
      DATA CGRID_INDEX( 128 ), SPECIES_TYPE( 128 ), CONVERT_CONC( 128 ) /  127, 'GC', F /  ! ARO1
      DATA CGRID_INDEX( 129 ), SPECIES_TYPE( 129 ), CONVERT_CONC( 129 ) /  129, 'GC', F /  ! NAPHTHAL
      DATA CGRID_INDEX( 130 ), SPECIES_TYPE( 130 ), CONVERT_CONC( 130 ) /  130, 'GC', F /  ! PAHRO2
      DATA CGRID_INDEX( 131 ), SPECIES_TYPE( 131 ), CONVERT_CONC( 131 ) /  131, 'GC', F /  ! TERP
      DATA CGRID_INDEX( 132 ), SPECIES_TYPE( 132 ), CONVERT_CONC( 132 ) /  132, 'GC', F /  ! SESQ
      DATA CGRID_INDEX( 133 ), SPECIES_TYPE( 133 ), CONVERT_CONC( 133 ) /  133, 'GC', F /  ! SESQRXN
      DATA CGRID_INDEX( 134 ), SPECIES_TYPE( 134 ), CONVERT_CONC( 134 ) /  134, 'GC', F /  ! CL2
      DATA CGRID_INDEX( 135 ), SPECIES_TYPE( 135 ), CONVERT_CONC( 135 ) /  135, 'GC', F /  ! CL
      DATA CGRID_INDEX( 136 ), SPECIES_TYPE( 136 ), CONVERT_CONC( 136 ) /  136, 'GC', F /  ! CLNO
      DATA CGRID_INDEX( 137 ), SPECIES_TYPE( 137 ), CONVERT_CONC( 137 ) /  137, 'GC', F /  ! CLONO
      DATA CGRID_INDEX( 138 ), SPECIES_TYPE( 138 ), CONVERT_CONC( 138 ) /  138, 'GC', F /  ! CLNO2
      DATA CGRID_INDEX( 139 ), SPECIES_TYPE( 139 ), CONVERT_CONC( 139 ) /  139, 'GC', F /  ! HCL
      DATA CGRID_INDEX( 140 ), SPECIES_TYPE( 140 ), CONVERT_CONC( 140 ) /  140, 'GC', F /  ! CLO
      DATA CGRID_INDEX( 141 ), SPECIES_TYPE( 141 ), CONVERT_CONC( 141 ) /  141, 'GC', F /  ! CLONO2
      DATA CGRID_INDEX( 142 ), SPECIES_TYPE( 142 ), CONVERT_CONC( 142 ) /  142, 'GC', F /  ! HOCL
      DATA CGRID_INDEX( 143 ), SPECIES_TYPE( 143 ), CONVERT_CONC( 143 ) /  143, 'GC', F /  ! xCL
      DATA CGRID_INDEX( 144 ), SPECIES_TYPE( 144 ), CONVERT_CONC( 144 ) /  144, 'GC', F /  ! xCLCCHO
      DATA CGRID_INDEX( 145 ), SPECIES_TYPE( 145 ), CONVERT_CONC( 145 ) /  145, 'GC', F /  ! xCLACET
      DATA CGRID_INDEX( 146 ), SPECIES_TYPE( 146 ), CONVERT_CONC( 146 ) /  146, 'GC', F /  ! CLCCHO
      DATA CGRID_INDEX( 147 ), SPECIES_TYPE( 147 ), CONVERT_CONC( 147 ) /  147, 'GC', F /  ! CLACET
      DATA CGRID_INDEX( 148 ), SPECIES_TYPE( 148 ), CONVERT_CONC( 148 ) /  148, 'GC', F /  ! CLCHO
      DATA CGRID_INDEX( 149 ), SPECIES_TYPE( 149 ), CONVERT_CONC( 149 ) /  149, 'GC', F /  ! BNZNRXN
      DATA CGRID_INDEX( 150 ), SPECIES_TYPE( 150 ), CONVERT_CONC( 150 ) /  150, 'GC', F /  ! BNZHRXN
      DATA CGRID_INDEX( 151 ), SPECIES_TYPE( 151 ), CONVERT_CONC( 151 ) /  151, 'GC', F /  ! XYLNRXN
      DATA CGRID_INDEX( 152 ), SPECIES_TYPE( 152 ), CONVERT_CONC( 152 ) /  152, 'GC', F /  ! XYLHRXN
      DATA CGRID_INDEX( 153 ), SPECIES_TYPE( 153 ), CONVERT_CONC( 153 ) /  153, 'GC', F /  ! TOLNRXN
      DATA CGRID_INDEX( 154 ), SPECIES_TYPE( 154 ), CONVERT_CONC( 154 ) /  154, 'GC', F /  ! TOLHRXN
      DATA CGRID_INDEX( 155 ), SPECIES_TYPE( 155 ), CONVERT_CONC( 155 ) /  155, 'GC', F /  ! PAHNRXN
      DATA CGRID_INDEX( 156 ), SPECIES_TYPE( 156 ), CONVERT_CONC( 156 ) /  156, 'GC', F /  ! PAHHRXN
      DATA CGRID_INDEX( 157 ), SPECIES_TYPE( 157 ), CONVERT_CONC( 157 ) /  157, 'GC', F /  ! HCHO_PRIMARY
      DATA CGRID_INDEX( 158 ), SPECIES_TYPE( 158 ), CONVERT_CONC( 158 ) /  158, 'GC', F /  ! CCHO_PRIMARY
      DATA CGRID_INDEX( 159 ), SPECIES_TYPE( 159 ), CONVERT_CONC( 159 ) /  159, 'GC', F /  ! ACRO_PRIMARY
      DATA CGRID_INDEX( 160 ), SPECIES_TYPE( 160 ), CONVERT_CONC( 160 ) /  160, 'GC', F /  ! H2NO3PIJ
      DATA CGRID_INDEX( 161 ), SPECIES_TYPE( 161 ), CONVERT_CONC( 161 ) /  161, 'GC', F /  ! H2NO3PK
      DATA CGRID_INDEX( 162 ), SPECIES_TYPE( 162 ), CONVERT_CONC( 162 ) /  228, 'AE', T /  ! ACLI
      DATA CGRID_INDEX( 163 ), SPECIES_TYPE( 163 ), CONVERT_CONC( 163 ) /  227, 'AE', T /  ! ACLJ
      DATA CGRID_INDEX( 164 ), SPECIES_TYPE( 164 ), CONVERT_CONC( 164 ) /  230, 'AE', T /  ! ACLK
      DATA CGRID_INDEX( 165 ), SPECIES_TYPE( 165 ), CONVERT_CONC( 165 ) /  183, 'AE', T /  ! AXYL1J
      DATA CGRID_INDEX( 166 ), SPECIES_TYPE( 166 ), CONVERT_CONC( 166 ) /  237, 'AE', T /  ! AOLGAJ
      DATA CGRID_INDEX( 167 ), SPECIES_TYPE( 167 ), CONVERT_CONC( 167 ) /  184, 'AE', T /  ! AXYL2J
      DATA CGRID_INDEX( 168 ), SPECIES_TYPE( 168 ), CONVERT_CONC( 168 ) /  186, 'AE', T /  ! ATOL1J
      DATA CGRID_INDEX( 169 ), SPECIES_TYPE( 169 ), CONVERT_CONC( 169 ) /  187, 'AE', T /  ! ATOL2J
      DATA CGRID_INDEX( 170 ), SPECIES_TYPE( 170 ), CONVERT_CONC( 170 ) /  189, 'AE', T /  ! ABNZ1J
      DATA CGRID_INDEX( 171 ), SPECIES_TYPE( 171 ), CONVERT_CONC( 171 ) /  190, 'AE', T /  ! ABNZ2J
      DATA CGRID_INDEX( 172 ), SPECIES_TYPE( 172 ), CONVERT_CONC( 172 ) /  195, 'AE', T /  ! ATRP1J
      DATA CGRID_INDEX( 173 ), SPECIES_TYPE( 173 ), CONVERT_CONC( 173 ) /  238, 'AE', T /  ! AOLGBJ
      DATA CGRID_INDEX( 174 ), SPECIES_TYPE( 174 ), CONVERT_CONC( 174 ) /  196, 'AE', T /  ! ATRP2J
      DATA CGRID_INDEX( 175 ), SPECIES_TYPE( 175 ), CONVERT_CONC( 175 ) /  197, 'AE', T /  ! AISO1J
      DATA CGRID_INDEX( 176 ), SPECIES_TYPE( 176 ), CONVERT_CONC( 176 ) /  198, 'AE', T /  ! AISO2J
      DATA CGRID_INDEX( 177 ), SPECIES_TYPE( 177 ), CONVERT_CONC( 177 ) /  199, 'AE', T /  ! ASQTJ
      DATA CGRID_INDEX( 178 ), SPECIES_TYPE( 178 ), CONVERT_CONC( 178 ) /  192, 'AE', T /  ! APAH1J
      DATA CGRID_INDEX( 179 ), SPECIES_TYPE( 179 ), CONVERT_CONC( 179 ) /  193, 'AE', T /  ! APAH2J
      DATA CGRID_INDEX( 180 ), SPECIES_TYPE( 180 ), CONVERT_CONC( 180 ) /  181, 'AE', T /  ! AALK1J
      DATA CGRID_INDEX( 181 ), SPECIES_TYPE( 181 ), CONVERT_CONC( 181 ) /  182, 'AE', T /  ! AALK2J
      DATA CGRID_INDEX( 182 ), SPECIES_TYPE( 182 ), CONVERT_CONC( 182 ) /  172, 'GC', F /  ! PCVOC
      DATA CGRID_INDEX( 183 ), SPECIES_TYPE( 183 ), CONVERT_CONC( 183 ) /  173, 'GC', F /  ! PCSOARXN
      DATA CGRID_INDEX( 184 ), SPECIES_TYPE( 184 ), CONVERT_CONC( 184 ) /  162, 'GC', F /  ! VLVPO1
      DATA CGRID_INDEX( 185 ), SPECIES_TYPE( 185 ), CONVERT_CONC( 185 ) /  163, 'GC', F /  ! VSVPO1
      DATA CGRID_INDEX( 186 ), SPECIES_TYPE( 186 ), CONVERT_CONC( 186 ) /  164, 'GC', F /  ! VSVPO2
      DATA CGRID_INDEX( 187 ), SPECIES_TYPE( 187 ), CONVERT_CONC( 187 ) /  165, 'GC', F /  ! VSVPO3
      DATA CGRID_INDEX( 188 ), SPECIES_TYPE( 188 ), CONVERT_CONC( 188 ) /  166, 'GC', F /  ! VIVPO1
      DATA CGRID_INDEX( 189 ), SPECIES_TYPE( 189 ), CONVERT_CONC( 189 ) /  167, 'GC', F /  ! VLVOO1
      DATA CGRID_INDEX( 190 ), SPECIES_TYPE( 190 ), CONVERT_CONC( 190 ) /  168, 'GC', F /  ! VLVOO2
      DATA CGRID_INDEX( 191 ), SPECIES_TYPE( 191 ), CONVERT_CONC( 191 ) /  170, 'GC', F /  ! VSVOO2
      DATA CGRID_INDEX( 192 ), SPECIES_TYPE( 192 ), CONVERT_CONC( 192 ) /  171, 'GC', F /  ! VSVOO3
      DATA CGRID_INDEX( 193 ), SPECIES_TYPE( 193 ), CONVERT_CONC( 193 ) /  169, 'GC', F /  ! VSVOO1
      DATA CGRID_INDEX( 194 ), SPECIES_TYPE( 194 ), CONVERT_CONC( 194 ) /  236, 'AE', T /  ! AISO3J

      INTEGER, PARAMETER :: N_ACT_SP = 194

      INTEGER, PARAMETER :: NRXNS = 464

      INTEGER            :: KUNITS

      DATA  KUNITS /   2 /

      INTEGER IRXXN

      REAL( 8 )          :: RTDAT( 3,NRXNS )

      INTEGER, PARAMETER :: NFALLOFF =  24
      REAL( 8 )          :: RFDAT( 5,NFALLOFF )

      INTEGER            :: KTYPE( NRXNS )

      DATA ( KTYPE( IRXXN ), IRXXN = 1, NRXNS ) /  & 
     &      0,    2,    3,   10,    3,   10,    3,    3,    3,    3, & ! O   
     &     10,   10,    1,    1,    3,    0,    0,    0,    0,    3, & ! 1   
     &      3,   10,    0,    3,   10,    1,    8,    0,    9,    3, & ! 2   
     &      3,   10,   10,    0,    3,    4,    9,    9,    1,    3, & ! 3   
     &      0,    1,    3,   10,    3,    3,    4,    4,    1,    4, & ! 4   
     &      3,    3,    3,    1,    1,    1,    6,    6,    6,    6, & ! 5   
     &      6,    6,   10,   10,    0,    3,    3,    6,    3,    3, & ! 6   
     &      6,    3,    4,    3,    0,    3,    6,    6,    6,    6, & ! 7   
     &      6,    6,    6,    1,    3,    0,    6,    6,    6,    6, & ! 8   
     &      6,    6,    6,    6,    6,    6,    3,    0,    6,    6, & ! 9   
     &      6,    6,    6,    6,    6,    6,    6,    6,    1,    3, & ! O   
     &      3,    6,    1,   11,   11,   11,   11,   11,   11,   11, & ! 1   
     &     11,   11,   11,   11,   11,   11,   11,   11,   11,   11, & ! 2   
     &     11,    0,    0,    3,    3,    3,    0,    3,    3,    0, & ! 3   
     &      3,    4,    0,    4,    0,    3,    1,    3,    1,    3, & ! 4   
     &      0,    1,    0,    1,    0,    1,    0,    0,    0,    3, & ! 5   
     &      3,    0,    1,    3,    0,    3,    1,    1,    0,    0, & ! 6   
     &      1,    0,    3,    1,    1,    0,    1,    1,    0,    1, & ! 7   
     &      1,    3,    3,    3,    1,    0,    3,    3,    1,    0, & ! 8   
     &      1,    1,    1,    0,    1,    0,    1,    0,    6,    0, & ! 9   
     &      6,    1,    3,    1,    1,    0,    1,    0,    1,    6, & ! O   
     &      6,    6,   11,   11,   11,   11,   11,   11,   11,   11, & ! 1   
     &     11,   11,   11,   11,   11,   11,   11,   11,   11,   11, & ! 2   
     &     11,   11,   11,   11,   11,   11,   11,   11,   11,   11, & ! 3   
     &     11,   11,   11,   11,   11,   11,   11,   11,   11,   11, & ! 4   
     &     11,   11,   11,   11,   11,   11,   11,   11,   11,   11, & ! 5   
     &     11,   11,   11,   11,   11,    3,   10,    3,    3,    3, & ! 6   
     &      3,    3,    3,    3,    3,    3,    1,    3,    3,    3, & ! 7   
     &      3,    1,    3,    3,    0,    3,    3,    3,    1,    1, & ! 8   
     &      3,    3,    3,    3,    1,   10,    3,    3,    3,    1, & ! 9   
     &      1,    1,    1,    4,    4,    4,    3,    3,    3,    3, & ! O   
     &      3,    3,    3,    3,    3,    3,    3,    3,    1,    1, & ! 1   
     &      1,    3,    3,    3,    1,    6,    6,    6,    6,    0, & ! 2   
     &      2,    0,   10,   10,    0,    0,    2,    2,    3,    1, & ! 3   
     &      3,   10,    0,    0,   10,    3,    3,    0,    3,    3, & ! 4   
     &      3,    3,    1,    1,    1,    3,    1,    1,    1,    3, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      0,    1,    1,    0,   11,   11,   11,   11,   11,   11, & ! 7   
     &      3,   10,    1,    1,    1,    1,   10,    1,    1,    1, & ! 8   
     &      1,    1,    3,    3,    3,    1,    1,    1,    1,    1, & ! 9   
     &      1,    1,    1,    1,    6,    6,    6,    6,    6,    6, & ! O   
     &      6,    6,    6,    0,    0,    3,    3,    3,    3,    0, & ! 1   
     &      3,    1,    1,    3,    1,    1,    0,    1,   -1,   -1, & ! 2   
     &     -1,   -1,   -1,   -1,   -1,   -1,   12,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,   -1/     !  6   

      INTEGER            :: IRXBITS( NRXNS )

      DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,   20,    0,    1,    0,    1,    0,    0,    0,   16, & ! O   
     &      1,    1,    8,    8,    0,    2,    2,    2,    2,    8, & ! 1   
     &      4,    1,    2,    0,    1,    0,    0,    2,    0,    0, & ! 2   
     &      0,    1,    1,    2,    0,    0,    0,    8,    0,    0, & ! 3   
     &      2,    0,    0,    1,  128,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    1,    1,    2,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    2,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    2,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    2,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    2,    2,    0,    0,    0,    2,    0,    0,    2, & ! 3   
     &      0,    0,    2,    0,    2,    0,    0,    0,    0,    0, & ! 4   
     &      2,    0,    2,    0,    2,    0,    2,    2,    2,    0, & ! 5   
     &      0,    2,    0,    0,    2,    0,    0,    0,    2,    2, & ! 6   
     &      0,    2,    0,    0,    0,    2,    0,    0,    2,    0, & ! 7   
     &      0,    0,    0,    0,    0,    2,    0,    0,    0,    2, & ! 8   
     &      0,    0,    0,    2,    0,    2,    0,    2,    0,    2, & ! 9   
     &      0,    0,    0,    0,    0,    2,    0,    2,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   64,    1,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    2,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    1,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    2, & ! 2   
     &      4,    2,    1,    1,    2,    2,    0,    0,    0,    0, & ! 3   
     &      0,    1,    2,    2,    1,    0,    0,    2,    0,    0, & ! 4   
     &    128,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      2,    0,    0,    2,    0,    0,    0,    0,    0,    0, & ! 7   
     &     64,    1,    0,    0,    0,    0,    1,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    2,    2,    0,    0,    0,    0,    2, & ! 1   
     &      0,    0,    0,    0,    0,    0,    2,    0,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    1/     !  6   

      INTEGER            :: IORDER( NRXNS )

      DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    2,    2,    2,    2,    2,    2,    2,    3, & ! O   
     &      2,    1,    2,    3,    2,    1,    1,    1,    1,    2, & ! 1   
     &      2,    2,    1,    2,    2,    2,    2,    1,    2,    2, & ! 2   
     &      2,    2,    1,    1,    2,    2,    2,    3,    2,    2, & ! 3   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    1,    1,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    1,    1,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! O   
     &      2,    2,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    2,    2,    2,    1,    2,    2,    1, & ! 3   
     &      2,    2,    1,    2,    1,    2,    2,    2,    2,    2, & ! 4   
     &      1,    2,    1,    2,    1,    2,    1,    1,    1,    2, & ! 5   
     &      2,    1,    2,    2,    1,    2,    2,    2,    1,    1, & ! 6   
     &      2,    1,    2,    2,    2,    1,    2,    2,    1,    2, & ! 7   
     &      2,    2,    2,    2,    2,    1,    2,    2,    2,    1, & ! 8   
     &      2,    2,    2,    1,    2,    1,    2,    1,    2,    1, & ! 9   
     &      2,    2,    2,    2,    2,    1,    2,    1,    2,    2, & ! O   
     &      2,    2,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 2   
     &      3,    1,    2,    2,    1,    1,    2,    2,    2,    2, & ! 3   
     &      2,    2,    1,    1,    1,    2,    2,    1,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      1,    2,    2,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    1, & ! 1   
     &      2,    2,    2,    2,    2,    2,    1,    2,    1,    1, & ! 2   
     &      1,    1,    1,    2,    2,    2,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    1/     !  6   

      INTEGER, PARAMETER :: KTN1 = 120
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     13,   14,   26,   39,   42,   49,   54,   55,   56,   84, & ! O   
     &    109,  113,  147,  149,  152,  154,  156,  163,  167,  168, & ! 1   
     &    171,  174,  175,  177,  178,  180,  181,  185,  189,  191, & ! 2   
     &    192,  193,  195,  197,  202,  204,  205,  207,  209,  277, & ! 3   
     &    282,  289,  290,  295,  300,  301,  302,  303,  319,  320, & ! 4   
     &    321,  325,  340,  353,  354,  355,  357,  358,  359,  361, & ! 5   
     &    362,  363,  364,  365,  366,  367,  368,  369,  370,  372, & ! 6   
     &    373,  383,  384,  385,  386,  388,  389,  390,  391,  392, & ! 7   
     &    396,  397,  398,  399,  400,  401,  402,  403,  404,  422, & ! 8   
     &    423,  425,  426,  428,  438,  439,  440,  441,  442,  443, & ! 9   
     &    444,  445,  446,  447,  448,  449,  450,  451,  452,  453, & ! O   
     &    454,  455,  456,  457,  458,  459,  460,  461,  462,  463/!1   

      INTEGER, PARAMETER :: KTN2 =   4
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &      2,  331,  337,  338/

      INTEGER, PARAMETER :: KTN3 = 112
      INTEGER            :: KRX3( KTN3 )

      DATA ( KRX3( IRXXN ), IRXXN = 1, KTN3 ) / & 
     &      3,    5,    7,    8,    9,   10,   15,   20,   21,   24, & ! O   
     &     30,   31,   35,   40,   43,   45,   46,   51,   52,   53, & ! 1   
     &     66,   67,   69,   70,   72,   74,   76,   85,   97,  110, & ! 2   
     &    111,  134,  135,  136,  138,  139,  141,  146,  148,  150, & ! 3   
     &    160,  161,  164,  166,  173,  182,  183,  184,  187,  188, & ! 4   
     &    203,  266,  268,  269,  270,  271,  272,  273,  274,  275, & ! 5   
     &    276,  278,  279,  280,  281,  283,  284,  286,  287,  288, & ! 6   
     &    291,  292,  293,  294,  297,  298,  299,  307,  308,  309, & ! 7   
     &    310,  311,  312,  313,  314,  315,  316,  317,  318,  322, & ! 8   
     &    323,  324,  339,  341,  346,  347,  349,  350,  351,  352, & ! 9   
     &    356,  360,  381,  393,  394,  395,  416,  417,  418,  419, & ! O   
     &    421,  424/     !  1   

      INTEGER, PARAMETER :: KTN4 =  10
      INTEGER            :: KRX4( KTN4 )

      DATA ( KRX4( IRXXN ), IRXXN = 1, KTN4 ) / & 
     &     36,   47,   48,   50,   73,  142,  144,  304,  305,  306/

      INTEGER, PARAMETER :: KTN5 =   0
      INTEGER            :: KRX5( 1 )

      DATA   KRX5( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN6 =  54
      INTEGER            :: KRX6( KTN6 )

      DATA ( KRX6( IRXXN ), IRXXN = 1, KTN6 ) / & 
     &     57,   58,   59,   60,   61,   62,   68,   71,   77,   78, & 
     &     79,   80,   81,   82,   83,   87,   88,   89,   90,   91, & 
     &     92,   93,   94,   95,   96,   99,  100,  101,  102,  103, & 
     &    104,  105,  106,  107,  108,  112,  199,  201,  210,  211, & 
     &    212,  326,  327,  328,  329,  405,  406,  407,  408,  409, & 
     &    410,  411,  412,  413/

      INTEGER, PARAMETER :: KTN7 =   0
      INTEGER            :: KRX7( 1 )

      DATA   KRX7( 1 ) / 0 /

      INTEGER, PARAMETER :: NWM =   3
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &      2,   21,  331/
      REAL,    PARAMETER :: ATM_AIR = 1.00000E+06

      INTEGER, PARAMETER :: NWW =   5
      INTEGER            :: NRXWW( NWW )

      DATA ( NRXWW( IRXXN ), IRXXN = 1, NWW ) / & 
     &     13,   14,   14,   20,   38/

      INTEGER, PARAMETER :: NWO2 =   2
      INTEGER            :: NRXWO2( NWO2 )

      DATA ( NRXWO2( IRXXN ), IRXXN = 1, NWO2 ) / & 
     &      2,   10/
      REAL,    PARAMETER :: ATM_O2 = 2.09500E+05

      INTEGER, PARAMETER :: NWN2 =   0
      INTEGER            :: NRXWN2( 1 )

      DATA   NRXWN2( 1 ) / 0 /
      REAL,    PARAMETER :: ATM_N2 = 7.80800E+05

      INTEGER, PARAMETER :: NWCH4 =   2
      INTEGER            :: NRXWCH4( NWCH4 )

      DATA ( NRXWCH4( IRXXN ), IRXXN = 1, NWCH4 ) / & 
     &    266,  381/
      REAL,    PARAMETER :: ATM_CH4 = 1.85000E+00

      INTEGER, PARAMETER :: NWH2 =   2
      INTEGER            :: NRXWH2( NWH2 )

      DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) / & 
     &     45,  351/
      REAL,    PARAMETER :: ATM_H2 = 5.60000E-01

      INTEGER, PARAMETER :: MXPRD =  26
      INTEGER            :: IRR( NRXNS,MXPRD+3 )

      DATA ( IRR( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    3,    3,    3,    3,    4,    4,    2,    2, & ! O   
     &      1,    6,    6,    6,    1,    5,    5,    4,    4,    8, & ! 1   
     &      8,    9,   10,    9,    9,    9,    9,    7,    9,    9, & ! 2   
     &     11,   11,   14,   14,   14,   11,   11,   11,    5,    5, & ! 3   
     &     15,   15,    9,    9,    9,   19,   19,   19,   19,   19, & ! 4   
     &     19,   23,   23,   23,   23,   23,   24,   24,   24,   24, & ! 5   
     &     24,   24,   25,   26,   26,   25,   25,   25,   25,   25, & ! 6   
     &     25,   25,   29,   30,   30,   29,   29,   29,   29,   29, & ! 7   
     &     29,   29,   29,   36,   37,   37,   36,   36,   36,   36, & ! 8   
     &     36,   36,   36,   36,   36,   39,   40,   40,   39,   39, & ! 9   
     &     39,   39,   39,   39,   39,   39,   39,   39,   41,   41, & ! O   
     &     38,   38,   38,   31,   31,   46,   46,   47,   47,   48, & ! 1   
     &     48,   49,   49,   50,   50,   51,   51,   52,   52,   53, & ! 2   
     &     53,   20,   20,   20,   20,   54,   54,   54,   55,   55, & ! 3   
     &     55,   43,   43,   57,   57,   22,   60,   28,   35,   21, & ! 4   
     &     21,   63,   63,   65,   65,   68,   68,   75,   75,   75, & ! 5   
     &     75,   69,   69,   69,   79,   45,   45,   44,   44,   44, & ! 6   
     &     80,   80,   80,   76,   76,   76,   77,   77,   77,   81, & ! 7   
     &     81,   83,   83,   83,   83,   83,   84,   84,   84,   84, & ! 8   
     &     70,   70,   70,   70,   66,   66,   42,   42,   87,   87, & ! 9   
     &     87,   89,   89,   89,   89,   89,   27,   27,   34,   78, & ! O   
     &     78,   78,   56,   56,   33,   33,   59,   59,   88,   88, & ! 1   
     &     72,   72,   64,   64,   71,   71,   61,   61,   62,   62, & ! 2   
     &     90,   90,   73,   73,   74,   74,   91,   91,   92,   92, & ! 3   
     &     93,   93,   82,   82,   86,   86,   58,   58,   58,   32, & ! 4   
     &     32,   32,   67,   67,   67,   94,   94,   94,   96,   96, & ! 5   
     &     96,   85,   85,   97,   97,    9,   98,   98,   98,   98, & ! 6   
     &     99,   99,   99,   99,  100,  100,  100,  100,  101,  101, & ! 7   
     &    101,  101,   95,   95,   95,  103,  105,  105,  105,  105, & ! 8   
     &    105,  106,  106,  106,  106,  108,  108,  109,  111,  113, & ! 9   
     &    115,  116,  117,  118,  119,  120,  121,  122,  123,  124, & ! O   
     &    126,  126,  126,  126,  127,  127,  127,  127,  128,  104, & ! 1   
     &    129,  131,  131,  131,  131,  132,  132,  132,  132,  134, & ! 2   
     &    135,  136,  135,  135,  137,  138,  135,  135,  135,  135, & ! 3   
     &    140,  140,  141,  141,  141,  135,  140,  142,  140,    9, & ! 4   
     &    135,   20,   54,   22,   55,   43,   57,   42,   66,   75, & ! 5   
     &     69,   45,   80,   63,   65,   68,   89,   83,   84,   70, & ! 6   
     &    146,  146,  146,  147,  143,  143,  144,  144,  145,  145, & ! 7   
     &    135,   98,   99,  100,  101,  106,  108,  111,  113,  115, & ! 8   
     &    116,  117,  118,  119,  120,  121,  122,  123,  126,  127, & ! 9   
     &    128,  104,  129,  131,  132,  110,  110,  114,  114,  112, & ! O   
     &    112,  130,  130,  157,  157,  157,  157,  157,  158,  158, & ! 1   
     &    158,  158,  159,  159,  159,  159,  159,  159,    1,    6, & ! 2   
     &      6,  160,  161,  160,  160,  161,    4,  165,  167,  168, & ! 3   
     &    169,  170,  171,  172,  174,  175,  176,  177,  178,  179, & ! 4   
     &    180,  181,  182,  184,  185,  186,  187,  188,  189,  190, & ! 5   
     &    193,  191,  192,  103/     !  6   

      DATA ( IRR( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    4,    2,    1,    1,    2,    1,    5,    2, & ! O   
     &      5,    0,    0,    0,    5,    0,    0,    0,    0,    0, & ! 1   
     &      0,    2,    0,   10,    1,    5,    7,    0,   12,    4, & ! 2   
     &      2,    1,    0,    0,    9,    4,   11,   11,   11,    5, & ! 3   
     &      0,    9,   11,   16,    0,    2,   11,   11,    5,   19, & ! 4   
     &     19,    2,   11,    5,   19,   23,    2,   11,    5,   19, & ! 5   
     &     23,   24,    1,    0,    0,    2,   11,    5,   19,   23, & ! 6   
     &     24,   25,    1,    0,    0,    2,   11,    5,   19,   23, & ! 7   
     &     24,   25,   29,    1,    0,    0,    2,   11,    5,   19, & ! 8   
     &     23,   24,   25,   29,   36,    1,    0,    0,    2,   11, & ! 9   
     &      5,   19,   23,   24,   25,   29,   36,   39,    1,    0, & ! O   
     &      1,   11,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    9,    5,    9,    0,    5,    9,    0, & ! 3   
     &      5,    9,    0,    9,    0,    9,    9,    9,    9,    9, & ! 4   
     &      0,    9,    0,    9,    0,    9,    0,    0,    0,    9, & ! 5   
     &      5,    0,    9,    5,    0,    9,    5,    9,    0,    0, & ! 6   
     &      9,    0,    5,    9,    4,    0,    9,    4,    0,    9, & ! 7   
     &      4,    9,    4,    5,    3,    0,    9,    4,    3,    0, & ! 8   
     &      9,    4,    5,    0,    9,    0,    9,    0,    9,    0, & ! 9   
     &      5,    9,    4,    5,    3,    0,    9,    0,    9,    2, & ! O   
     &      1,   11,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    9,    4,    5,    3, & ! 6   
     &      9,    4,    5,    3,    9,    4,    5,    3,    9,    4, & ! 7   
     &      5,    3,    9,    9,    0,    9,   11,    2,   19,   23, & ! 8   
     &     25,    9,    4,    5,    3,    9,    4,    9,    9,    9, & ! 9   
     &      9,    9,    9,    9,    9,    9,    9,    9,    9,    9, & ! O   
     &      9,    4,    5,    3,    9,    4,    5,    3,    9,    9, & ! 1   
     &      9,    9,    4,    5,    3,    9,    4,    5,    3,    0, & ! 2   
     &      2,    0,    1,    1,    0,    0,   11,   11,    4,    5, & ! 3   
     &      2,    1,    0,    0,    0,  141,   11,    0,  140,  139, & ! 4   
     &      0,  135,  135,  135,  135,  135,  135,  135,  135,  135, & ! 5   
     &    135,  135,  135,  135,  135,  135,  135,  135,  135,  135, & ! 6   
     &      0,    9,  135,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,  135,  135,  135,  135,  135,  135,  135,  135,  135, & ! 8   
     &    135,  135,  135,  135,  135,  135,  135,  135,  135,  135, & ! 9   
     &    135,  135,  135,  135,  135,    2,   11,    2,   11,    2, & ! O   
     &     11,    2,   11,    0,    0,    9,    5,  135,    9,    0, & ! 1   
     &      5,  135,    9,    4,    5,    3,    0,  135,    0,    0, & ! 2   
     &      0,    0,    0,  162,  163,  164,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    9,    9,    9,    9,    9,    9,    9,    9, & ! 5   
     &      9,    9,    9,    0/     !  6   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &      2,    4,    0,    1,    2,    5,    1,    5,    1,    1, & ! O   
     &      6,    1,    7,    7,    2,    2,    1,    8,    3,    9, & ! 1   
     &      3,   10,    9,    1,    7,   11,    5,    9,   11,   11, & ! 2   
     &      9,   14,   11,   11,    1,    9,   15,   15,    9,    1, & ! 3   
     &      9,   11,    0,   11,   11,    1,   21,   20,   20,   22, & ! 4   
     &     20,    1,    0,    1,   11,    0,    0,    0,    1,   11, & ! 5   
     &      0,    0,   26,   25,   25,   19,   27,   19,   28,   19, & ! 6   
     &     19,   19,   30,   29,   29,    1,   34,    1,   20,   23, & ! 7   
     &     23,   13,   23,   37,   36,   36,    1,   34,    1,   20, & ! 8   
     &     23,   23,   13,   13,   38,   40,   39,   39,    1,   34, & ! 9   
     &      1,   20,   13,   13,   13,   20,   20,   20,   42,   43, & ! O   
     &     44,   45,   45,   11,    0,    9,    0,    1,    0,   19, & ! 1   
     &      0,   25,    0,   29,    0,   39,    0,   41,    0,   12, & ! 2   
     &      0,   11,   12,   11,    7,   25,   12,    7,   29,   23, & ! 3   
     &      7,   23,   25,   23,   25,   20,   11,   19,   23,   20, & ! 4   
     &     20,    9,   55,    9,    9,    9,    9,   12,   20,   11, & ! 5   
     &      7,   11,   12,    7,   25,   38,    7,   38,   10,    0, & ! 6   
     &     36,    0,    7,   39,    9,   11,   39,    9,   66,   39, & ! 7   
     &      9,   39,    9,   39,   55,    9,   23,    9,   55,   19, & ! 8   
     &     39,    9,   39,   11,   11,   31,   11,   11,   25,   12, & ! 9   
     &      7,   31,   11,   31,   55,   11,   25,   19,   29,   11, & ! O   
     &     11,    9,   20,    0,   54,    0,   55,    0,   43,    0, & ! 1   
     &     57,    0,   66,    0,   75,    0,   69,    0,   79,    0, & ! 2   
     &     80,    0,   76,    0,   77,    0,   81,    0,   83,    0, & ! 3   
     &     84,    0,   70,    0,   42,    0,   42,   66,    0,   63, & ! 4   
     &     57,    0,   65,   66,    0,   95,   66,    0,   68,   66, & ! 5   
     &      0,   87,    0,   89,    0,   19,   31,   11,   31,   11, & ! 6   
     &     31,   11,   31,   55,   31,   11,   31,   11,   31,   11, & ! 7   
     &     31,   19,  103,   93,    9,  105,   57,   57,   57,   57, & ! 8   
     &     57,   31,   11,   31,   66,   11,   11,   11,   11,   11, & ! 9   
     &     11,   11,   11,   11,   31,   31,   31,   31,   31,    9, & ! O   
     &     31,   11,   31,   55,   31,   11,   31,   55,   11,   11, & ! 1   
     &     11,   31,   11,   31,   55,   31,   11,   31,   55,  135, & ! 2   
     &    136,  135,  137,  138,  135,  135,  139,  140,  140,  140, & ! 3   
     &    135,  141,  140,  135,  140,  134,  142,    9,  134,  135, & ! 4   
     &    139,  139,  139,  139,  139,  139,  139,  139,  139,  139, & ! 5   
     &    139,  139,  139,  139,  139,  139,   31,  139,   23,  139, & ! 6   
     &     11,   29,  139,   25,  135,    0,  146,    0,  147,    0, & ! 7   
     &    139,   31,  139,   31,  139,  139,   11,   31,   31,   31, & ! 8   
     &     31,   31,  139,  139,  139,  139,  139,  139,  139,  139, & ! 9   
     &     31,   31,   31,  139,   31,    2,   11,    2,   11,    2, & ! O   
     &     11,    2,   11,    0,    0,    9,    5,  135,    9,    0, & ! 1   
     &      5,  135,    9,    4,    5,    3,    0,  135,   10,    7, & ! 2   
     &      7,    7,    7,  138,  138,  138,    0,  166,  166,  166, & ! 3   
     &    166,  166,  166,  173,  173,  173,  173,  173,  166,  166, & ! 4   
     &    166,  166,    9,    9,    9,    9,    9,    9,    9,    9, & ! 5   
     &      9,    9,    9,  194/     !  6   

      DATA ( IRR( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &      3,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    5,    0,    0,    1,    0,    3,    0,    0,    0, & ! 1   
     &      0,    0,    2,    0,    0,    1,    0,    1,   13,    0, & ! 2   
     &      1,    0,    1,    1,    0,    0,    0,    0,    1,    0, & ! 3   
     &      0,    0,    0,   17,    0,   20,    0,    0,   11,   20, & ! 4   
     &     11,    0,    0,    0,   20,    0,    0,    0,    0,   20, & ! 5   
     &      0,    0,    0,    1,    1,   13,   28,   13,   20,   13, & ! 6   
     &     13,   13,    0,    1,    1,   23,   35,   23,   11,   31, & ! 7   
     &     31,   19,   31,    0,    1,    1,   13,   35,   13,   11, & ! 8   
     &     38,   38,   19,   23,   23,    0,    1,    1,   13,   35, & ! 9   
     &     13,   11,   20,   20,   19,   25,   25,   25,    0,   19, & ! O   
     &      0,    0,   23,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,   12,    0,   12,   11,    0,   11,   25,   23,   31, & ! 3   
     &     29,   49,   19,   24,   23,   11,   13,   23,   31,    9, & ! 4   
     &     11,   23,   11,   23,   11,   11,   11,   11,   12,   12, & ! 5   
     &     11,   12,   25,   12,    0,   23,   38,    0,    0,    0, & ! 6   
     &      0,    0,   36,   23,   11,   19,   23,   11,    0,   23, & ! 7   
     &     11,   23,   11,   23,    0,   11,   24,   11,   57,   12, & ! 8   
     &     23,   11,    7,   25,   31,   25,   31,   31,    0,   11, & ! 9   
     &     25,   39,    9,   39,    0,    9,   23,   13,   23,   12, & ! O   
     &     12,   11,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,   11,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,   23,    9,   23,   31, & ! 6   
     &     23,    9,   23,   57,   23,    9,   47,   31,   23,    9, & ! 7   
     &     47,   51,    9,   92,   11,    0,   87,   87,   87,   87, & ! 8   
     &     87,   50,   31,   47,  107,    9,    9,   31,   31,   31, & ! 9   
     &     31,   31,   31,   31,   23,   23,   52,   48,   23,  125, & ! O   
     &     48,   31,   23,   57,   23,   31,   47,   57,   31,   31, & ! 1   
     &     31,   50,   31,   47,   66,   50,   31,   47,   66,    0, & ! 2   
     &      0,    2,    0,    0,    1,    1,    0,    9,    0,    1, & ! 3   
     &      1,    0,    1,    5,    1,    5,    0,  135,  135,    0, & ! 4   
     &     11,   11,   25,   20,   29,   23,   23,    1,   11,   11, & ! 5   
     &     12,   31,   36,    9,    9,    9,  143,   39,   24,   11, & ! 6   
     &     12,    0,   29,   23,    0,    0,    0,    0,    0,    0, & ! 7   
     &     19,   23,   31,  143,   31,   31,   12,   23,   23,   23, & ! 8   
     &     23,   23,   11,   31,   31,   31,   31,   31,   31,   31, & ! 9   
     &     23,   23,   23,   31,  143,  149,  150,  151,  152,  153, & ! O   
     &    154,  155,  156,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    7,  160, & ! 2   
     &    161,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,  183,  184,  184,  184,  184,  184,  189,  189, & ! 5   
     &    189,  189,  189,    0/     !  6   

      DATA ( IRR( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    9,    0,    0,    0,    0,    7,    0, & ! 3   
     &      0,    0,    0,   18,    0,   11,    0,    0,    1,    0, & ! 4   
     &      0,    0,    0,    0,   22,    0,    0,    0,    0,   22, & ! 5   
     &      0,    0,    0,    0,   19,    1,    4,    1,   11,    0, & ! 6   
     &      0,    0,    0,    0,   23,   31,    4,   31,   23,   33, & ! 7   
     &     33,   23,   33,    0,    0,   13,   38,    4,   38,   23, & ! 8   
     &     13,   13,   38,   31,   13,    0,    0,   13,   20,    4, & ! 9   
     &     20,   13,   25,   25,   20,   23,   38,   13,    0,    0, & ! O   
     &      0,    0,   31,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,   12,    0,   19,    0,   31,   32, & ! 3   
     &      0,   56,   12,   58,   31,    0,    0,   13,   13,   19, & ! 4   
     &      9,   24,    9,   24,   23,   23,   75,    0,    0,   78, & ! 5   
     &     12,   25,    0,   25,    0,   31,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   24,   23,   25,   24,   23,    0,   24, & ! 7   
     &     25,   31,   23,    7,    0,   25,   58,   23,    0,   66, & ! 8   
     &     31,   23,   23,   29,   49,   29,    1,    1,    0,   20, & ! 9   
     &      0,   23,   12,   23,    0,   19,   13,    9,   32,   13, & ! O   
     &     13,   12,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,   56,   12,   59,   19, & ! 6   
     &     24,   19,   24,    0,   24,   12,   23,   51,   24,   51, & ! 7   
     &     23,   23,    0,   23,   20,    0,   75,   75,   75,   75, & ! 8   
     &     75,   23,    9,   50,    0,   12,   12,    9,    9,    9, & ! 9   
     &      9,    9,    9,   23,   33,   24,   23,   49,   24,    0, & ! O   
     &     23,    9,   24,   66,   24,    9,   48,   66,    9,    9, & ! 1   
     &      9,   23,    9,   50,  107,   23,    9,   50,  133,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,   12,    0,   11,   23,   56,   24,   11,   23,   12, & ! 5   
     &     25,   90,    0,   23,   23,   11,   39,   23,   58,   39, & ! 6   
     &     23,    0,    0,  143,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   56,   23,   23,  143,  143,    0,   24,   24,   24, & ! 8   
     &     24,   24,   31,   23,   23,   52,   48,   23,   23,  143, & ! 9   
     &     24,   24,   24,  143,   49,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,  185,  185,  185,  185,  185,  190,  190, & ! 5   
     &    190,  190,  190,    0/     !  6   

      DATA ( IRR( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    5,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,   13,    0,    9,    0,   19,    0, & ! 6   
     &      0,    0,    0,    0,   31,   32,    9,   32,   31,   32, & ! 7   
     &     32,   31,   32,    0,    0,   38,   23,    9,   23,   38, & ! 8   
     &      0,    0,   23,   32,    0,    0,    0,   20,   25,    9, & ! 9   
     &     25,   25,    0,    0,   25,   31,   23,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,   53,   33, & ! 3   
     &      0,   32,    0,   31,   33,    0,    0,   31,   33,    0, & ! 4   
     &      0,   58,    0,   58,   24,   24,   69,    0,    0,    0, & ! 5   
     &     78,    0,    0,    0,    0,   67,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   58,   12,   39,   58,   12,    0,   58, & ! 7   
     &     23,   53,   12,   31,    0,   39,   31,   31,    0,   39, & ! 8   
     &     24,   50,   31,   12,   50,   23,   47,   23,    0,    0, & ! 9   
     &      0,   53,   13,   24,    0,   39,   46,    0,   13,    1, & ! O   
     &      5,   13,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,   85,   13,   32,   23, & ! 6   
     &     58,   12,   58,    0,   58,   13,   24,   23,   58,   23, & ! 7   
     &     24,   24,    0,   86,   84,    0,   69,   69,   69,   69, & ! 8   
     &     69,   24,   49,   23,    0,   60,   13,   23,   23,   23, & ! 9   
     &     23,   23,   23,   56,   32,   58,   24,   23,   58,    0, & ! O   
     &     24,   23,   58,    0,   58,   19,   23,    0,   23,   23, & ! 1   
     &     23,   24,   49,   23,    0,   24,   49,   23,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,   33,   49,   58,   23,   24,   29, & ! 5   
     &      0,   67,    0,   55,   24,   23,   23,   24,   31,   23, & ! 6   
     &    143,    0,    0,   56,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,  148,   24,   24,   23,   49,    0,   58,   58,   58, & ! 8   
     &     58,   58,   23,   33,   24,   23,   49,   24,   24,   48, & ! 9   
     &     58,   58,   58,   49,   50,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,  186,  186,  186,  186,  186,  193,  193, & ! 5   
     &    193,  193,  193,    0/     !  6   

      DATA ( IRR( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    5,    0,   19,    0,   13,    0, & ! 6   
     &      0,    0,    0,    0,   32,   33,   31,   33,   33,   13, & ! 7   
     &     13,   32,   13,    0,    0,   23,    0,   38,    0,   13, & ! 8   
     &      0,    0,    0,   33,    0,    0,    0,   25,    0,   20, & ! 9   
     &      0,    0,    0,    0,    0,   32,   13,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,   33,   12, & ! 3   
     &      0,    0,    0,   49,   32,    0,    0,   61,   59,    0, & ! 4   
     &      0,   55,    0,   66,   58,   58,   76,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   61,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   31,   13,   12,   31,   13,    0,   31, & ! 7   
     &     24,   56,   13,   53,    0,   23,   49,   12,    0,    0, & ! 8   
     &     58,   12,   24,   20,   23,   24,   23,   24,    0,    0, & ! 9   
     &      0,   56,   20,   58,    0,   12,   56,    0,   46,    0, & ! O   
     &      0,   75,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,   32,   20,    0,   12, & ! 6   
     &     56,   13,   32,    0,   56,   20,   58,   24,   56,   24, & ! 7   
     &     58,   58,    0,   56,   83,    0,    9,    9,    9,    9, & ! 8   
     &      9,   58,   50,   24,    0,   75,    0,   24,   24,   24, & ! 9   
     &     24,   24,   24,   54,    0,   59,   58,   24,   56,    0, & ! O   
     &     58,   24,   33,    0,   56,   49,   24,    0,   24,   24, & ! 1   
     &     24,   58,   50,   24,    0,   58,   50,   24,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,   53,   32,   31,   24,   58,    0, & ! 5   
     &      0,    0,    0,   46,   58,   24,   24,   58,   49,   24, & ! 6   
     &     56,    0,    0,   32,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   58,   58,   24,   50,    0,   90,   90,   90, & ! 8   
     &     90,   90,   56,   32,   58,   24,   23,   58,   58,   23, & ! 9   
     &     64,   90,   90,   50,   51,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,  187,  187,  187,  187,  189,  191,  191, & ! 5   
     &    191,  191,  191,    0/     !  6   

      DATA ( IRR( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,   13,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,   33,   13,   23,   13,   32,    0, & ! 7   
     &      0,   33,    0,    0,    0,    5,    0,   23,    0,    0, & ! 8   
     &      0,    0,    0,   38,    0,    0,    0,    5,    0,   25, & ! 9   
     &      0,    0,    0,    0,    0,   33,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,   32,   11, & ! 3   
     &      0,    0,    0,   50,    0,    0,    0,   32,   62,    0, & ! 4   
     &      0,   31,    0,   31,   55,   66,   77,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   49,   75,   75,   49,   75,    0,   49, & ! 7   
     &     58,   72,   20,   32,    0,   12,   56,   13,    0,    0, & ! 8   
     &     53,   13,   58,   87,   24,   58,   24,   58,    0,    0, & ! 9   
     &      0,   33,   60,    7,    0,   13,   32,    0,   33,    0, & ! O   
     &      0,    4,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,   60,    0,   53, & ! 6   
     &     33,   20,    0,    0,   97,   60,   56,   58,   92,   58, & ! 7   
     &     82,   56,    0,   59,   23,    0,   11,   11,   11,   11, & ! 8   
     &     11,   53,   23,   58,    0,    0,    0,   58,   58,   58, & ! 9   
     &     58,   58,   58,   85,    0,   88,   56,   58,   33,    0, & ! O   
     &     56,   58,   59,    0,   33,   50,   58,    0,   58,   58, & ! 1   
     &     58,   53,   23,   58,    0,   53,   23,   58,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,   31,    0,   49,   58,   55,    0, & ! 5   
     &      0,    0,    0,   31,   66,   58,   58,   31,  144,   58, & ! 6   
     &     32,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   97,   56,   58,   51,    0,    0,    0,    0, & ! 8   
     &      0,    0,   54,    0,   59,   58,   24,   56,   56,   24, & ! 9   
     &      0,   64,   64,   51,   23,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,  188,  189,  189,  189,  190,  192,  192, & ! 5   
     &    192,  192,  192,    0/     !  6   

      DATA ( IRR( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,   13,    0,   13,    0,   13,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,   13,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   13, & ! 9   
     &      0,    0,    0,    0,    0,   13,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,   56,    0,    0,    0,    0,   32,    0, & ! 4   
     &      0,   46,    0,   46,   66,   69,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   53,   69,   69,   53,   69,    0,   53, & ! 7   
     &     12,   61,   69,    0,    0,   20,   85,   20,    0,    0, & ! 8   
     &     56,   20,   53,   57,   58,   56,   58,   20,    0,    0, & ! 9   
     &      0,   71,   75,   53,    0,   20,    0,    0,   31,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   56, & ! 6   
     &     32,   54,    0,    0,   82,   89,   93,   53,   93,   12, & ! 7   
     &     67,   66,    0,   31,   55,    0,   13,   13,   13,   13, & ! 8   
     &     13,   56,   24,   53,    0,    0,    0,   71,   71,   71, & ! 9   
     &     71,   71,   71,   32,    0,   32,   33,   53,   59,    0, & ! O   
     &     33,   12,   88,    0,   59,   23,   56,    0,   71,   71, & ! 1   
     &     71,   56,   24,   53,    0,   56,   24,   53,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,   32,    0,   50,   55,   66,    0, & ! 5   
     &      0,    0,    0,   56,   46,   66,  139,   53,   32,   76, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,  144,   97,   56,   23,    0,    0,    0,    0, & ! 8   
     &      0,    0,   85,    0,   88,   56,   58,   33,   33,   58, & ! 9   
     &      0,    0,    0,   23,   24,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,  189,  190,  190,  190,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 11 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    5,    0,   33,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,   33,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,   56,    0,   33,   31,   70,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   59,   50,    0,   59,   50,    0,   71, & ! 7   
     &     13,   32,   60,    0,    0,   49,   61,   69,    0,    0, & ! 8   
     &     85,   57,   56,    0,   20,   33,   56,   56,    0,    0, & ! 9   
     &      0,   32,    0,   86,    0,   28,    0,    0,   59,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   54, & ! 6   
     &      0,   60,    0,    0,   32,   84,   82,   97,   82,   13, & ! 7   
     &      0,   67,    0,  104,  104,    0,   20,   20,   20,   20, & ! 8   
     &     20,   59,   58,   56,    0,    0,    0,   45,   61,   61, & ! 9   
     &     61,   61,   61,    0,    0,    0,   59,   56,   88,    0, & ! O   
     &     85,   13,   86,    0,   88,   24,   33,    0,   61,   61, & ! 1   
     &     61,   59,   58,   56,    0,   59,   58,   56,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,   56,   57,   31,    0, & ! 5   
     &      0,    0,    0,   33,   31,   69,   53,   82,    0,   77, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,  145,   82,   93,   24,    0,    0,    0,    0, & ! 8   
     &      0,    0,   32,    0,   32,   33,   53,   59,   59,   56, & ! 9   
     &      0,    0,    0,   24,   58,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,  190,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 12 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,   32,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,   59,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,   33,    0,   59,   33,   31,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   72,   56,    0,   72,   56,    0,   61, & ! 7   
     &     75,    0,   50,    0,    0,   56,   32,   60,    0,    0, & ! 8   
     &     59,   75,   59,    0,   56,   59,   33,   54,    0,    0, & ! 9   
     &      0,    0,    0,   32,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   71, & ! 6   
     &      0,   28,    0,    0,    0,   66,   86,   73,   94,   20, & ! 7   
     &      0,    0,    0,   94,    0,    0,   60,   60,   60,   60, & ! 8   
     &     60,   88,   12,   59,    0,    0,    0,   73,   45,   45, & ! 9   
     &     62,   45,   62,    0,    0,    0,   88,   33,   72,    0, & ! O   
     &     59,   20,   32,    0,   72,   58,   59,    0,   45,   62, & ! 1   
     &     62,   88,   12,   85,    0,   88,   12,   33,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,   33,   66,   49,    0, & ! 5   
     &      0,    0,    0,   59,   33,   70,   56,  144,    0,   31, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   32,   32,   82,   58,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   59,   56,   88,   97,   33, & ! 9   
     &      0,    0,    0,   58,  139,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,  191,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

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
     &      0,    0,    0,   32,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,   59,    0,   64,   59,   46,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   71,   67,    0,   71,   67,    0,   62, & ! 7   
     &     69,    0,   56,    0,    0,   32,    0,   50,    0,    0, & ! 8   
     &     72,   69,   61,    0,   54,   67,   55,   33,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   32, & ! 6   
     &      0,    0,    0,    0,    0,    0,   32,   74,  102,   56, & ! 7   
     &      0,    0,    0,   55,    0,    0,   12,   12,   12,   12, & ! 8   
     &     12,   61,   53,   88,    0,    0,    0,   74,   90,   90, & ! 9   
     &     45,   90,   45,    0,    0,    0,   72,   59,   64,    0, & ! O   
     &     88,   33,   67,    0,   93,   12,   88,    0,   73,   45, & ! 1   
     &     45,   72,   13,   59,    0,   72,   13,   59,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,   59,   42,   50,    0, & ! 5   
     &      0,    0,    0,   72,   59,   31,   71,  145,    0,   53, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,  144,   53,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   88,   33,   72,   93,   59, & ! 9   
     &      0,    0,    0,   53,   53,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,  192,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,   64,    0,   32,   64,   59,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   61,    0,    0,   61,    0,    0,   73, & ! 7   
     &     76,    0,   32,    0,    0,    0,    0,   56,    0,    0, & ! 8   
     &     64,   60,   86,    0,   33,    0,   59,   55,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,   66,    0,   60, & ! 7   
     &      0,    0,    0,    9,    0,    0,    0,    1,   66,   66, & ! 8   
     &     19,   62,   13,   61,    0,    0,    0,   81,   73,   73, & ! 9   
     &     90,   73,   90,    0,    0,    0,   32,   88,   67,    0, & ! O   
     &     97,   55,    0,    0,   82,   13,   72,    0,   74,   90, & ! 1   
     &     90,   61,   20,   88,    0,   61,   20,   88,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,   32,   47,   56,    0, & ! 5   
     &      0,    0,    0,   32,   64,   46,  144,   32,    0,   56, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,   67,   56,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   72,   59,   64,   82,   88, & ! 9   
     &      0,    0,    0,   56,   56,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,   32,    0,    0,   67,   71,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   67,    0,    0,   67,    0,    0,   74, & ! 7   
     &     77,    0,    0,    0,    0,    0,    0,   32,    0,    0, & ! 8   
     &     71,   35,   67,    0,   55,    0,   88,   59,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,   32,    0,   83, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,   22,    0, & ! 8   
     &      0,   67,   56,   86,    0,    0,    0,   96,   74,   74, & ! 9   
     &     73,   74,   73,    0,    0,    0,   67,   72,    0,    0, & ! O   
     &     92,   59,    0,    0,   64,   20,   86,    0,   81,   73, & ! 1   
     &     73,   62,   56,   92,    0,   62,   56,   92,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   31,   33,    0, & ! 5   
     &      0,    0,    0,    0,   67,   59,   32,    0,    0,   61, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,   59,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   32,   88,   67,  144,   72, & ! 9   
     &      0,    0,    0,   59,   59,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,   72,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   82, & ! 7   
     &     81,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &     61,   85,    0,    0,   59,    0,   57,   88,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   84, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,  107,   59,   67,    0,    0,    0,  110,   81,   81, & ! 9   
     &     74,   81,   74,    0,    0,    0,    0,   64,    0,    0, & ! O   
     &     93,   88,    0,    0,   32,   56,   32,    0,   64,   74, & ! 1   
     &     74,   93,   59,   93,    0,   93,   59,   93,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   56,   59,    0, & ! 5   
     &      0,    0,    0,    0,    0,   71,    0,    0,    0,   73, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,   88,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   67,   72,    0,  145,   92, & ! 9   
     &      0,    0,    0,   88,   88,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,   61,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   67, & ! 7   
     &     83,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &     67,   56,    0,    0,   72,    0,   72,   57,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   70, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,   88,  107,    0,    0,    0,    0,   67,   67, & ! 9   
     &     81,   91,   81,    0,    0,    0,    0,   67,    0,    0, & ! O   
     &     82,   57,    0,    0,   67,   54,   67,    0,   67,   81, & ! 1   
     &     81,   82,   88,   82,    0,   82,   88,   82,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   33,   72,    0, & ! 5   
     &      0,    0,    0,    0,    0,   72,    0,    0,    0,   74, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,   71,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,   64,    0,   32,   93, & ! 9   
     &      0,    0,    0,   71,   71,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 18 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,    0,    0,    0,   73,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     84,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,   67,    0,    0,   66,    0,   66,   72,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   66, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,   57,    0,    0,    0,    0,    0,   96,   96, & ! 9   
     &     67,   67,   91,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &     64,   60,    0,    0,    0,   33,    0,    0,  112,   91, & ! 1   
     &     91,   64,   57,   86,    0,   64,   57,   86,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   59,   64,    0, & ! 5   
     &      0,    0,    0,    0,    0,   61,    0,    0,    0,   82, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,   62,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,   67,    0,   67,   82, & ! 9   
     &      0,    0,    0,   62,   62,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 19 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,    0,    0,    0,   74,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     70,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,   64,    0,   64,   66,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   67, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,   71,    0,    0,    0,    0,    0,  112,  114, & ! 9   
     &     96,   96,   67,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &     32,   35,    0,    0,    0,   55,    0,    0,    0,   64, & ! 1   
     &     64,   67,   60,   67,    0,   67,   60,   67,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   88,   67,    0, & ! 5   
     &      0,    0,    0,    0,    0,   73,    0,    0,    0,  144, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,   93,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  144, & ! 9   
     &      0,    0,    0,   93,   93,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 20 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,    0,    0,    0,   67,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     31,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,   67,    0,   42,   64,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,   62,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &    114,  114,   96,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &     67,   66,    0,    0,    0,   59,    0,    0,    0,   67, & ! 1   
     &     67,  107,   35,  107,    0,  133,   35,  133,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   72,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   74,    0,    0,    0,  145, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,   82,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  145, & ! 9   
     &      0,    0,    0,   82,   82,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 21 ), IRXXN = 1, NRXNS ) / & 
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
     &     50,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,   86,   32,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,   66,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,  114,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,   32,    0,    0,    0,   43,    0,    0,    0,  114, & ! 1   
     &    130,    0,   61,    0,    0,    0,   61,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   64,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   67,    0,    0,    0,   67, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,   73,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   32, & ! 9   
     &      0,    0,    0,   73,   73,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 22 ), IRXXN = 1, NRXNS ) / & 
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
     &     56,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,   32,   67,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,   67,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,   67,    0,    0,    0,   57,    0,    0,    0,    0, & ! 1   
     &      0,    0,   62,    0,    0,    0,   62,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   86,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,   74,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   67, & ! 9   
     &      0,    0,    0,   74,   74,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 23 ), IRXXN = 1, NRXNS ) / & 
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
     &     61,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,   67,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,  107,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,   72,    0,    0,    0,    0, & ! 1   
     &      0,    0,   92,    0,    0,    0,   92,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   67,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,   91,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,   91,   91,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 24 ), IRXXN = 1, NRXNS ) / & 
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
     &     67,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
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
     &      0,    0,    0,    0,    0,   60,    0,    0,    0,    0, & ! 1   
     &      0,    0,   82,    0,    0,    0,   82,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,  144,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,  144,  144,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 25 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,    0,    0,    0,   28,    0,    0,    0,    0, & ! 1   
     &      0,    0,   66,    0,    0,    0,   66,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,   67,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,   67,   67,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 26 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,    0,    0,    0,   35,    0,    0,    0,    0, & ! 1   
     &      0,    0,   67,    0,    0,    0,   67,    0,    0,    0, & ! 2   
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
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 27 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,    0,    0,    0,   66,    0,    0,    0,    0, & ! 1   
     &      0,    0,  107,    0,    0,    0,  133,    0,    0,    0, & ! 2   
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
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 28 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,    0,    0,    0,   32,    0,    0,    0,    0, & ! 1   
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
     &      0,    0,    0,    0/     !  6   

      DATA ( IRR( IRXXN, 29 ), IRXXN = 1, NRXNS ) / & 
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
     &      0,    0,    0,    0,    0,   67,    0,    0,    0,    0, & ! 1   
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
     &      0,    0,    0,    0/     !  6   

      DATA ( RTDAT( 1,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 5.6800D-34, 8.0000D-12, 9.0000D-32, 5.5000D-12, & ! O   
     &     2.5000D-31, 3.0000D-12, 1.4000D-13, 1.8000D-11, 3.3000D-39, & ! +   
     &     3.6000D-30, 1.3000D-03, 1.0000D-22, 0.0000D+00, 4.5000D-14, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.6300D-10, & ! +   
     &     2.3800D-11, 7.0000D-31, 1.0000D+00, 2.5000D-12, 3.2000D-30, & ! 2   
     &     2.0000D-11, 2.4000D-14, 1.0000D+00, 1.4400D-13, 1.7000D-12, & ! +   
     &     3.6000D-12, 2.0000D-31, 3.7200D-05, 1.0000D+00, 1.3000D-12, & ! 3   
     &     2.0300D-16, 2.2000D-13, 3.0800D-34, 4.0000D-12, 8.5000D-13, & ! +   
     &     1.0000D+00, 1.8000D-12, 4.8000D-11, 3.3000D-31, 7.7000D-12, & ! 4   
     &     2.3000D-12, 3.4600D-13, 3.3400D-14, 1.3000D-12, 6.3900D-14, & ! +   
     &     7.4000D-13, 2.6000D-12, 3.8000D-13, 2.3000D-12, 2.0000D-13, & ! 5   
     &     3.5000D-14, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.7000D-28, 4.9000D-03, 1.0000D+00, & ! 6   
     &     7.5000D-12, 5.2000D-13, 1.0000D+00, 2.0000D-12, 4.4000D-13, & ! +   
     &     1.0000D+00, 2.9000D-12, 1.2100D-11, 8.3000D+16, 1.0000D+00, & ! 7   
     &     6.7000D-12, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.3700D-11, 7.9000D+16, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 1.6000D+16, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.4000D-11, 7.5000D+14, & ! +   
     &     2.3000D-11, 1.0000D+00, 1.0000D-03, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 5.4000D-12, 2.0000D-12, & ! 3   
     &     4.4000D-12, 1.0000D+00, 1.4000D-12, 5.1000D-12, 1.0000D+00, & ! +   
     &     1.4000D-12, 4.5600D-14, 5.0000D-01, 1.3000D-12, 1.7500D-01, & ! 4   
     &     2.8500D-12, 4.5000D-13, 4.2000D-14, 1.2000D-12, 3.8000D-12, & ! +   
     &     1.0000D+00, 2.5000D-11, 1.0000D+00, 5.6000D-11, 1.0000D+00, & ! 5   
     &     1.4100D-10, 1.0000D+00, 1.0000D+00, 1.0000D+00, 3.1000D-12, & ! +   
     &     2.8000D-12, 1.0000D+00, 1.5000D-11, 1.4000D-12, 1.0000D+00, & ! 6   
     &     1.7000D-12, 1.4000D-11, 3.5000D-12, 1.5000D-03, 1.5000D-02, & ! +   
     &     1.2000D-11, 6.0000D-02, 1.3400D-12, 7.4000D-11, 9.6600D-18, & ! 7   
     &     1.0000D+00, 7.4000D-11, 9.6600D-18, 1.0000D+00, 9.3500D-11, & ! +   
     &     1.4300D-17, 8.0000D-12, 1.4000D-15, 1.5000D-12, 6.3400D-12, & ! 8   
     &     1.0000D+00, 2.6000D-12, 8.5000D-16, 4.3200D-12, 1.0000D+00, & ! +   
     &     6.1900D-11, 4.1800D-18, 1.0000D-13, 1.0000D+00, 1.5500D-11, & ! 9   
     &     4.8600D-03, 7.2000D-12, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.9900D-11, 1.4000D-15, 1.1800D-15, 2.3700D-12, & ! O   
     &     1.0000D+00, 5.2800D-12, 1.0000D+00, 6.4200D-12, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.8500D-12, 1.0000D-28, 9.1400D-15, 3.3000D-12, 1.0700D-11, & ! +   
     &     4.8500D-12, 5.5100D-15, 4.5900D-13, 1.0200D-11, 1.4800D-11, & ! 7   
     &     1.3400D-14, 1.0000D-13, 2.2600D-11, 2.5400D-11, 7.8600D-15, & ! +   
     &     3.0300D-12, 3.5000D-11, 1.9000D-11, 4.7500D-12, 1.0000D+00, & ! 8   
     &     5.7800D-11, 2.0600D-13, 2.6000D-12, 2.0000D-13, 3.5000D-14, & ! +   
     &     4.4000D-13, 1.2100D-11, 5.0000D-16, 1.1900D-12, 3.2000D-11, & ! 9   
     &     5.5000D-30, 1.0000D-14, 2.3300D-12, 1.8100D-12, 2.3100D-11, & ! +   
     &     1.3600D-11, 1.4300D-11, 3.2500D-11, 5.4900D-13, 1.3400D-12, & ! O   
     &     1.4900D-12, 1.5100D-12, 3.7500D-12, 2.7000D-12, 2.7000D-12, & ! +   
     &     6.7200D-12, 3.1900D-15, 5.3700D-13, 1.6100D-11, 1.2600D-11, & ! 1   
     &     8.5900D-15, 2.3100D-13, 1.4300D-11, 7.8400D-12, 3.0900D-11, & ! +   
     &     3.0900D-11, 2.2700D-11, 8.2800D-16, 1.3300D-12, 4.0200D-11, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     7.6000D-32, 1.0000D+00, 1.3000D-30, 1.8000D-31, 1.0000D+00, & ! 3   
     &     1.0000D+00, 3.4400D-11, 9.4100D-12, 2.8000D-11, 2.4000D-11, & ! +   
     &     6.2000D-12, 1.8000D-31, 1.0000D+00, 1.0000D+00, 4.4800D-05, & ! 4   
     &     6.2000D-12, 2.2000D-12, 1.0000D+00, 1.2500D-11, 1.7000D-12, & ! +   
     &     3.9000D-11, 8.1000D-11, 8.0000D-11, 5.5000D-11, 1.2300D-10, & ! 5   
     &     7.7000D-11, 3.6000D-11, 1.9200D-10, 2.0000D-10, 8.1000D-11, & ! +   
     &     8.0000D-11, 6.2000D-11, 8.0000D-11, 1.6600D-10, 3.0000D-10, & ! 6   
     &     4.2900D-10, 2.9400D-10, 3.8500D-10, 2.3200D-10, 4.1200D-10, & ! +   
     &     1.0000D+00, 3.1000D-12, 1.2900D-11, 5.0000D-01, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     7.3000D-12, 1.6000D-29, 2.6700D-10, 4.9000D-10, 4.8000D-10, & ! 8   
     &     5.4600D-10, 5.2000D-30, 6.2000D-11, 1.3500D-10, 1.4000D-10, & ! +   
     &     1.4400D-10, 2.4200D-10, 8.6000D-11, 8.3000D-11, 1.2000D-10, & ! 9   
     &     1.8600D-10, 2.6300D-10, 4.2100D-10, 3.9200D-10, 3.7700D-10, & ! +   
     &     2.1600D-10, 2.6600D-10, 2.6600D-10, 5.4600D-10, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     5.4000D-12, 2.0000D-12, 8.1000D-11, 4.4000D-12, 1.0000D+00, & ! +   
     &     1.4000D-12, 8.0000D-11, 1.9900D-11, 1.4000D-15, 1.1800D-15, & ! 2   
     &     2.3700D-12, 1.0000D+00, 2.9400D-10, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D-40, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 4   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 1.2500D-11, 4.0000D-11, 4.0000D-11, & ! 5   
     &     4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, & ! +   
     &     4.0000D-11, 4.0000D-11, 4.0000D-11, 1.0000D+00/           !6   

      DATA ( RTDAT( 2,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00,-2.6000D+00, 0.0000D+00,-1.5000D+00, 0.0000D+00, & ! O   
     &    -1.8000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -4.1000D+00,-3.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-2.6000D+00, 0.0000D+00, 0.0000D+00,-4.5000D+00, & ! 2   
     &     0.0000D+00, 4.6000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-3.4000D+00,-2.4000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     4.5700D+00, 6.0000D+02, 2.8000D+03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-4.3000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 3.6000D-01,-3.5300D+00, 0.0000D+00,-1.8000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, & ! +   
     &     5.6000D+01, 5.6000D+01,-7.1000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 5.4000D+01, 0.0000D+00, 0.0000D+00, & ! +   
     &     7.0000D+01, 0.0000D+00,-1.0700D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 6.7000D+01, 5.4000D+01, 6.9000D+01, 7.0000D+01, & ! +   
     &     7.0000D+01, 7.2000D+01, 7.2000D+01, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 7.6000D+01, 6.7000D+01, 5.4000D+01, 6.9000D+01, & ! +   
     &     7.0000D+01, 7.0000D+01, 7.2000D+01, 7.2000D+01, 7.2000D+01, & ! 9   
     &     7.3000D+01, 0.0000D+00, 0.0000D+00, 7.6000D+01, 6.7000D+01, & ! +   
     &     5.4000D+01, 6.9000D+01, 7.0000D+01, 7.0000D+01, 7.2000D+01, & ! O   
     &     7.2000D+01, 7.2000D+01, 7.2000D+01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 5.3000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.6500D+00, 0.0000D+00, 2.0000D+00, 0.0000D+00, & ! 4   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.3600D+02, 0.0000D+00, & ! +   
     &     1.3800D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 7.6000D+01, & ! +   
     &     7.3000D+01, 6.7000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
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
     &     0.0000D+00,-4.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D+00, 2.0000D+00, & ! O   
     &     2.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     3.2200D+02, 3.2300D+02, 3.2400D+02, 3.2500D+02, 0.0000D+00, & ! +   
     &    -1.8000D+00, 0.0000D+00,-2.0000D+00,-2.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00,-5.6000D-01, 2.1000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-3.4000D+00, 0.0000D+00, 0.0000D+00,-1.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-3.3000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00,-2.4000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.0400D+02, & ! O   
     &     5.2000D+01, 5.3000D+01, 5.2000D+01, 5.3000D+01, 5.2000D+01, & ! +   
     &     5.3000D+01, 5.2000D+01, 5.3000D+01, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 4.0582D-09, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   

      DATA ( RTDAT( 3,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00,-2.0600D+03, 0.0000D+00, 1.8800D+02, & ! O   
     &     0.0000D+00,-1.5000D+03,-2.4700D+03, 1.1000D+02, 5.3000D+02, & ! +   
     &     0.0000D+00,-1.1000D+04, 0.0000D+00, 0.0000D+00,-1.2600D+03, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 6.0000D+01, & ! +   
     &     9.6000D+01, 0.0000D+00, 0.0000D+00, 2.6000D+02, 0.0000D+00, & ! 2   
     &     0.0000D+00, 2.7000D-17, 0.0000D+00, 3.4300D-33,-9.4000D+02, & ! +   
     &     2.7000D+02, 0.0000D+00,-1.0650D+04, 0.0000D+00, 3.8000D+02, & ! 3   
     &     6.9300D+02, 1.9000D-33, 2.6600D-54, 0.0000D+00,-2.4500D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.5000D+02, 0.0000D+00,-2.1000D+03, & ! 4   
     &     3.6000D+02, 7.8000D+02, 7.8000D+02, 0.0000D+00, 3.6500D+02, & ! +   
     &    -5.2000D+02, 3.8000D+02, 9.0000D+02, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.2100D+04, 0.0000D+00, & ! 6   
     &     2.9000D+02, 9.8000D+02, 0.0000D+00, 5.0000D+02, 1.0700D+03, & ! +   
     &     0.0000D+00, 5.0000D+02, 0.0000D+00,-1.3940D+04, 0.0000D+00, & ! 7   
     &     3.4000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.4000D+04, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00,-1.3486D+04, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-8.1520D+03, & ! +   
     &     1.5000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.3500D+02,-2.4310D+03, & ! 3   
     &     3.6500D+02, 0.0000D+00,-1.8600D+03, 4.0500D+02, 0.0000D+00, & ! +   
     &    -1.6010D+03, 4.2900D+02, 0.0000D+00,-2.5000D+01, 0.0000D+00, & ! 4   
     &    -3.4500D+02, 0.0000D+00, 8.5500D+02, 0.0000D+00, 2.0000D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.4220D+02, & ! +   
     &    -2.3900D+03, 0.0000D+00, 0.0000D+00,-1.8950D+03, 0.0000D+00, & ! 6   
     &     9.5000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-1.8600D+03, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.8000D+02,-2.1000D+03,-1.8150D+03, 0.0000D+00, & ! 8   
     &     0.0000D+00, 6.1000D+02,-1.5200D+03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-2.5280D+03, 0.0000D+00, 0.0000D+00, & ! O   
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
     &    -1.6900D+03, 0.0000D+00,-2.5800D+03,-2.8800D+03,-8.0000D+02, & ! +   
     &     5.0400D+02,-1.8780D+03,-1.1560D+03,-2.8000D+02, 4.4800D+02, & ! 7   
     &    -2.2830D+03, 0.0000D+00,-4.0000D+01, 4.1000D+02,-1.9120D+03, & ! +   
     &    -4.4800D+02, 0.0000D+00, 3.9000D+02, 2.0000D+02, 0.0000D+00, & ! 8   
     &    -4.0000D+02, 1.3000D+03, 3.8000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0700D+03, 4.3600D+02,-5.3000D+02, 4.9000D+02, 0.0000D+00, & ! 9   
     &     0.0000D+00,-4.1000D+03,-1.9300D+02, 3.3800D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.3000D+02,-4.9900D+02, & ! O   
     &    -8.7000D+01, 1.2600D+02, 4.4000D+01, 3.7400D+02, 3.7400D+02, & ! +   
     &     5.0100D+02,-1.7010D+03,-1.0470D+03,-3.2600D+02, 4.8800D+02, & ! 1   
     &    -1.2550D+03, 3.8200D+02, 1.1100D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 4.3500D+02,-7.8500D+02, 4.9000D+02, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-2.5000D+02, 0.0000D+00, & ! +   
     &     2.9500D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.2530D+04, & ! 4   
     &     1.4500D+02, 3.4000D+02, 0.0000D+00,-1.9600D+03,-2.3000D+02, & ! +   
     &    -2.3100D+03,-3.0000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &    -1.0000D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00,-3.0000D+01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -1.2800D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.5000D+01,-1.0000D+02, 4.0000D+01, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     1.3500D+02,-2.4310D+03,-3.0000D+01, 3.6500D+02, 0.0000D+00, & ! +   
     &    -1.8600D+03, 0.0000D+00, 0.0000D+00,-2.5280D+03, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !6   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &      4,    6,   11,   12,   22,   25,   27,   29,   32,   33, & 
     &     37,   38,   44,   63,   64,  267,  296,  333,  334,  342, & 
     &    345,  382,  387,  437/

      DATA ( RFDAT( 1,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     3.0000D-11, 2.2000D-11, 1.9000D-12, 9.7000D+14, 3.6000D-11, & 
     &     3.0000D-11, 2.1990D+03, 0.0000D+00, 2.9000D-12, 5.4200D+15, & 
     &     9.8000D+02, 3.1800D+03, 1.6000D-12, 1.2100D-11, 4.0000D+16, & 
     &     8.8000D-12, 8.3000D-13, 1.0000D-10, 1.0000D-10, 1.5000D-11, & 
     &     3.7100D+15, 3.1000D-10, 2.2000D-10, 7.8426D+01/

      DATA ( RFDAT( 2,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00,-7.0000D-01, 2.0000D-01, 1.0000D-01,-1.0000D-01, & 
     &     0.0000D+00, 6.5000D-34, 0.0000D+00,-1.1000D+00,-2.3000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-9.0000D-01, 0.0000D+00, & 
     &    -8.5000D-01,-2.0000D+00,-1.0000D+00,-1.0000D+00,-1.9000D+00, & 
     &     3.5000D+00,-1.0000D+00, 0.0000D+00, 5.8212D+00/

      DATA ( RFDAT( 3,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.1080D+04, 0.0000D+00, & 
     &     0.0000D+00, 1.3350D+03, 0.0000D+00, 0.0000D+00,-1.1170D+04, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.3600D+04, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &    -1.2530D+04, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( RFDAT( 4,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     6.0000D-01, 6.0000D-01, 3.5000D-01, 3.5000D-01, 6.0000D-01, & 
     &     4.1000D-01, 0.0000D+00, 0.0000D+00, 6.0000D-01, 6.0000D-01, & 
     &     0.0000D+00, 0.0000D+00, 6.0000D-01, 3.0000D-01, 3.0000D-01, & 
     &     6.0000D-01, 6.0000D-01, 6.0000D-01, 6.0000D-01, 6.0000D-01, & 
     &     6.0000D-01, 6.0000D-01, 6.0000D-01, 0.0000D+00/

      DATA ( RFDAT( 5,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     1.0000D+00, 1.0000D+00, 1.3300D+00, 1.3300D+00, 1.0000D+00, & 
     &     1.2400D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.4100D+00, 1.4100D+00, & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00/

      REAL               :: SC( NRXNS,MXPRD )

      DATA ( SC( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    2.00000,    2.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    2.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    2.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.61000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    0.80000,    2.00000, & ! +   
     &        2.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    0.00000,    1.00000,    0.50000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.50000, & ! +   
     &        0.00000,    0.00000,    1.00000,    1.00000,    0.60000, & ! 6   
     &        1.00000,    0.10500,    1.00000,    0.10000,    1.00000, & ! +   
     &        1.00000,    2.00000,    1.00000,    1.00000,    0.60000, & ! 7   
     &        1.00000,    0.30750,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    2.00000,    2.00000,    1.00000,    1.00000, & ! 8   
     &        0.60000,    1.00000,    0.30750,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    2.00000,    2.00000, & ! 9   
     &        1.00000,    1.00000,    0.60000,    1.00000,    0.30750, & ! +   
     &        1.00000,    2.00000,    1.00000,    1.00000,    2.00000, & ! O   
     &        1.00000,    1.00000,    2.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 1   
     &        1.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! 2   
     &        1.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    2.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    0.96500,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.62000,    0.96700,    1.00000, & ! 4   
     &        1.00000,    1.00000,    0.50900,    1.00000,    0.30000, & ! +   
     &        1.00000,    0.74400,    1.00000,    0.84000,    1.00000, & ! 5   
     &        0.13900,    1.00000,    2.00000,    1.00000,    0.70000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    2.00000, & ! 6   
     &        0.20000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.21700,    0.82600, & ! 7   
     &        1.02300,    0.21700,    0.82600,    1.00000,    0.20600, & ! +   
     &        0.47100,    0.50000,    0.20800,    0.50000,    1.00000, & ! 8   
     &        0.33000,    0.97500,    0.16400,    0.45000,    0.40000, & ! +   
     &        0.28900,    0.28500,    0.15000,    1.23300,    0.47200, & ! 9   
     &        0.91300,    0.18900,    0.34400,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.25000,    0.83000,    0.03100,    1.00000, & ! O   
     &        1.06600,    0.98000,    1.00000,    0.80600,    1.00000, & ! +   
     &        1.00000,    0.44000,    1.00000,    0.00000,    1.00000, & ! 1   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! 2   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! 3   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! 4   
     &        0.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! 5   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! 6   
     &        1.00000,    1.00000,    0.16000,    1.00000,    0.80000, & ! +   
     &        0.98400,    0.16500,    0.94900,    0.45000,    0.95100, & ! 7   
     &        0.08000,    0.81500,    0.25000,    0.90700,    0.06600, & ! +   
     &        0.74900,    0.25000,    1.00000,    0.16000,    1.00000, & ! 8   
     &        1.00000,    0.72500,    0.72500,    0.36300,    0.36300, & ! +   
     &        0.72500,    0.79900,    0.00900,    0.05600,    1.00000, & ! 9   
     &        0.30000,    1.50000,    0.57000,    0.18100,    0.15900, & ! +   
     &        0.16100,    0.15900,    0.02200,    0.95000,    1.00000, & ! O   
     &        0.96500,    0.69500,    0.83000,    0.64700,    1.00000, & ! +   
     &        0.87100,    0.09500,    0.77200,    0.45000,    0.91200, & ! 1   
     &        0.09400,    0.40000,    0.07900,    0.12300,    0.07700, & ! +   
     &        0.07700,    0.73400,    0.07800,    0.22700,    0.23700, & ! 2   
     &        0.73400,    0.07800,    0.22700,    0.23700,    2.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    0.29000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        0.40400,    0.48400,    0.25000,    1.28300,    0.40100, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 7   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.12400,    0.39000,    0.15000, & ! 8   
     &        0.54800,    1.00000,    0.89400,    0.86400,    0.86400, & ! +   
     &        0.86400,    0.83800,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    0.38400,    0.27900, & ! +   
     &        0.84000,    0.82800,    0.82800,    0.54800,    0.25200, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    0.00000,    1.00000,    0.50000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    0.00000,    0.85710,    1.14290,    0.85710, & ! +   
     &        1.14290,    0.71430,    0.71430,    0.80000,    0.90000, & ! 4   
     &        0.50000,    0.50000,    1.50000,    1.42860,    1.42860, & ! +   
     &        1.71430,    1.71430,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000/           !6   

      DATA ( SC( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! 1   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 2   
     &        1.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.61000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.80000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 4   
     &        1.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    0.00000,    0.00000,    0.00000,    0.75000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.75000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.60000, & ! 6   
     &        1.00000,    0.04500,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    2.00000,    0.00000,    1.00000,    0.60000, & ! 7   
     &        1.00000,    0.10250,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    0.00000,    1.00000, & ! 8   
     &        0.60000,    1.00000,    0.10250,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    2.00000,    2.00000, & ! 9   
     &        0.00000,    1.00000,    0.60000,    1.00000,    0.10250, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    2.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! 3   
     &        0.00000,    1.00000,    1.00000,    0.03500,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.38000,    0.03900,    1.00000, & ! 4   
     &        1.00000,    1.00000,    0.49100,    1.00000,    0.30000, & ! +   
     &        1.00000,    0.25100,    1.00000,    0.22200,    0.14200, & ! 5   
     &        0.14800,    1.00000,    2.00000,    1.00000,    1.40000, & ! +   
     &        0.70000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 6   
     &        0.80000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.72300,    0.52200, & ! 7   
     &        0.17300,    0.72300,    0.52200,    0.00000,    0.73300, & ! +   
     &        0.55400,    0.50000,    0.10800,    0.50000,    0.00000, & ! 8   
     &        0.67000,    0.02500,    0.06400,    0.55000,    0.60000, & ! +   
     &        0.67000,    0.40000,    0.15000,    0.46700,    0.37900, & ! 9   
     &        0.40000,    0.30500,    0.55400,    0.00000,    2.00000, & ! +   
     &        1.00000,    0.75000,    0.33000,    0.96700,    0.00000, & ! O   
     &        0.17800,    0.02000,    1.00000,    0.19400,    1.00000, & ! +   
     &        1.00000,    0.44000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    1.00000,    0.16000,    1.00000,    0.29000, & ! +   
     &        0.98400,    0.35000,    0.94900,    0.55000,    1.18900, & ! 7   
     &        0.08000,    0.12000,    0.11700,    0.98600,    0.26600, & ! +   
     &        0.18700,    0.24000,    1.00000,    0.10000,    0.91000, & ! 8   
     &        0.00000,    0.27500,    0.27500,    0.13800,    0.13800, & ! +   
     &        0.27500,    0.00400,    0.10200,    0.64300,    1.00000, & ! 9   
     &        0.70000,    0.50000,    0.29000,    0.45400,    0.52000, & ! +   
     &        0.55400,    0.48700,    0.62700,    0.05000,    1.00000, & ! O   
     &        0.96500,    0.23600,    0.01000,    1.60500,    0.47000, & ! +   
     &        0.00100,    0.05700,    1.46300,    0.39000,    0.95300, & ! 1   
     &        0.04100,    0.42600,    0.75100,    0.56600,    0.61700, & ! +   
     &        0.61700,    0.06400,    0.04600,    0.28700,    0.76300, & ! 2   
     &        0.06400,    0.04600,    0.28700,    0.76300,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! 3   
     &        1.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    0.00000,    1.00000,    1.42000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.90000, & ! 5   
     &        1.00000,    0.97500,    0.03800,    0.31400,    0.63000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.41400,    0.14500, & ! 6   
     &        0.13900,    0.27400,    0.16500,    0.05300,    0.08400, & ! +   
     &        1.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    2.00000,    0.97100,    0.54100,    0.73800, & ! 8   
     &        0.25200,    1.00000,    0.89400,    0.86400,    0.86400, & ! +   
     &        0.86400,    0.83800,    0.68800,    1.00000,    0.97000, & ! 9   
     &        0.83500,    0.82700,    0.64700,    0.87300,    0.45000, & ! +   
     &        0.84000,    0.82800,    0.82800,    0.25200,    0.06800, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.50000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.48570,    0.30030, & ! 5   
     &        0.38560,    0.21810,    0.24120,    0.66640,    0.28580, & ! +   
     &        0.33030,    0.34440,    0.38860,    0.00000/           !6   

      DATA ( SC( IRXXN,  3 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.39000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.20000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 4   
     &        1.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.25000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.25000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.40000, & ! 6   
     &        1.00000,    0.15000,    1.00000,    0.90000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.40000, & ! 7   
     &        1.00000,    0.15000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    0.00000,    0.00000, & ! 8   
     &        0.40000,    1.00000,    0.15000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    2.00000, & ! 9   
     &        0.00000,    0.00000,    0.40000,    1.00000,    0.15000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    2.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 3   
     &        0.00000,    1.00000,    0.00000,    0.03500,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.38000,    0.03900,    1.00000, & ! 4   
     &        0.00000,    0.00000,    0.50900,    0.14300,    0.70000, & ! +   
     &        1.00000,    0.00400,    1.00000,    0.02900,    0.78200, & ! 5   
     &        0.58900,    0.50000,    0.00000,    0.00000,    0.30000, & ! +   
     &        1.40000,    1.00000,    0.00000,    1.00000,    0.00000, & ! 6   
     &        0.80000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.06000,    0.65200, & ! 7   
     &        0.30500,    0.06000,    0.65200,    0.00000,    0.11700, & ! +   
     &        0.01300,    0.50000,    0.10000,    0.50000,    0.00000, & ! 8   
     &        0.34000,    0.02500,    0.05000,    0.00000,    0.60000, & ! +   
     &        0.67000,    0.04800,    0.79900,    0.30000,    0.02900, & ! 9   
     &        0.60000,    0.01900,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.25000,    1.00500,    0.03100,    0.00000, & ! O   
     &        0.23400,    0.02000,    1.00000,    0.19400,    1.00000, & ! +   
     &        1.00000,    0.44000,    0.00000,    0.00000,    0.00000, & ! 1   
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
     &        0.00000,    1.61000,    0.51000,    1.00000,    0.51000, & ! +   
     &        0.01600,    0.35500,    0.05100,    0.00000,    0.04900, & ! 7   
     &        0.25500,    1.05500,    0.11800,    0.09300,    0.19200, & ! +   
     &        0.93600,    0.24000,    0.00000,    0.35000,    0.75000, & ! 8   
     &        0.00000,    0.27500,    0.27500,    0.13800,    0.13800, & ! +   
     &        0.27500,    1.04200,    0.72800,    0.00700,    0.00000, & ! 9   
     &        0.30000,    1.50000,    0.11600,    0.31200,    0.23900, & ! +   
     &        0.19800,    0.27800,    0.23000,    0.05000,    1.00000, & ! O   
     &        0.03500,    1.25300,    0.01100,    0.35300,    0.00000, & ! +   
     &        1.20200,    0.12800,    0.22800,    0.16000,    0.08800, & ! 1   
     &        0.44300,    0.03500,    0.17000,    0.20200,    0.17800, & ! +   
     &        0.17800,    1.21100,    0.49900,    0.02600,    1.00000, & ! 2   
     &        1.21100,    0.49900,    0.02600,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.10000, & ! 5   
     &        1.00000,    0.03900,    0.05500,    0.68000,    1.26000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.58800,    1.07800, & ! 6   
     &        0.14800,    0.21600,    0.80200,    0.05300,    0.15400, & ! +   
     &        1.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.97100,    1.88400,    0.17700, & ! 8   
     &        0.06800,    0.00000,    0.10600,    0.13600,    0.13600, & ! +   
     &        0.13600,    0.16200,    0.31200,    1.00000,    0.97000, & ! 9   
     &        0.09400,    0.00300,    1.54100,    1.60800,    0.44200, & ! +   
     &        0.16000,    0.17200,    0.17200,    0.06800,    0.03400, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00620,    0.28620, & ! 5   
     &        0.09500,    0.30630,    0.20890,    0.01430,    0.39310, & ! +   
     &        0.22720,    0.27490,    0.24210,    0.00000/           !6   

      DATA ( SC( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.39000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.40000, & ! 6   
     &        0.00000,    0.44000,    0.00000,    0.90000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.40000, & ! 7   
     &        1.00000,    0.44000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    0.00000,    0.00000, & ! 8   
     &        0.40000,    1.00000,    0.44000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.40000,    1.00000,    0.44000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.03500,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.37600,    1.00000, & ! 4   
     &        0.00000,    0.00000,    0.49100,    0.14200,    0.00000, & ! +   
     &        0.00000,    0.00400,    0.00000,    0.02900,    0.07700, & ! 5   
     &        0.12400,    0.50000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.30000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.80000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.06000,    0.52200, & ! 7   
     &        0.50000,    0.06000,    0.52200,    0.00000,    0.11700, & ! +   
     &        0.25800,    0.41600,    0.45000,    0.50000,    0.00000, & ! 8   
     &        0.33000,    0.30000,    0.05000,    0.00000,    0.40000, & ! +   
     &        0.04100,    0.04800,    0.79900,    1.23300,    0.04900, & ! 9   
     &        1.59000,    0.31300,    0.72100,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.16700,    0.31000,    0.00200,    0.00000, & ! O   
     &        0.33000,    0.02000,    0.00000,    0.11000,    1.00000, & ! +   
     &        1.00000,    0.44000,    0.00000,    0.00000,    0.00000, & ! 1   
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
     &        0.00000,    0.19500,    0.12000,    1.00000,    0.29000, & ! +   
     &        0.01600,    0.52500,    0.05100,    0.00000,    0.04900, & ! 7   
     &        0.18500,    0.06500,    0.23500,    0.09300,    0.19200, & ! +   
     &        0.06400,    0.01000,    0.00000,    0.05000,    0.45000, & ! 8   
     &        0.00000,    0.27500,    0.27500,    0.13800,    0.13800, & ! +   
     &        0.27500,    0.19700,    0.00100,    1.05000,    0.00000, & ! 9   
     &        0.30000,    0.50000,    0.29000,    0.45400,    0.52000, & ! +   
     &        0.55400,    0.48700,    0.62700,    0.08100,    1.00000, & ! O   
     &        0.03500,    0.07000,    1.76300,    0.35300,    0.00000, & ! +   
     &        0.12800,    0.09000,    0.22800,    0.00000,    0.08800, & ! 1   
     &        0.30700,    1.19300,    0.00000,    0.56600,    0.61700, & ! +   
     &        0.61700,    0.20100,    0.20200,    1.78600,    0.00000, & ! 2   
     &        0.20100,    0.20200,    1.78600,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.10000, & ! 5   
     &        1.00000,    0.03900,    1.28200,    0.11600,    0.37000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.41400,    0.11700, & ! 6   
     &        0.58900,    1.03200,    0.03300,    0.32200,    0.73000, & ! +   
     &        1.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.02900,    0.06900,    1.16800, & ! 8   
     &        0.03400,    0.00000,    0.10600,    0.13600,    0.13600, & ! +   
     &        0.13600,    0.16200,    0.31200,    1.00000,    0.03000, & ! 9   
     &        1.36100,    0.00400,    0.35200,    0.12700,    0.00100, & ! +   
     &        0.16000,    0.17200,    0.17200,    0.03400,    0.05000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00250,    0.00410, & ! 5   
     &        0.13730,    0.01530,    0.30000,    0.01230,    0.01390, & ! +   
     &        0.26070,    0.04910,    0.06400,    0.00000/           !6   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.40000, & ! 6   
     &        0.00000,    0.44000,    0.00000,    0.90000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.40000, & ! 7   
     &        1.00000,    0.44000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    0.00000,    0.00000, & ! 8   
     &        0.40000,    0.00000,    0.44000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.40000,    0.00000,    0.44000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        1.00000,    2.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.03500,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.51000,    1.00000, & ! 4   
     &        0.00000,    0.00000,    0.49100,    0.40000,    0.00000, & ! +   
     &        0.00000,    0.74400,    0.00000,    0.84000,    0.07700, & ! 5   
     &        0.12400,    0.50000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.25000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.52100,    0.17400, & ! 7   
     &        0.69500,    0.52100,    0.17400,    0.00000,    0.56100, & ! +   
     &        0.00700,    0.08400,    0.11700,    0.50000,    0.00000, & ! 8   
     &        0.33000,    0.67500,    0.47500,    0.00000,    0.00000, & ! +   
     &        0.04100,    0.49800,    0.05100,    0.30000,    0.47300, & ! 9   
     &        0.08700,    0.97600,    0.10200,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.08300,    0.50000,    0.00200,    0.00000, & ! O   
     &        1.18800,    0.02000,    0.00000,    0.11000,    0.00000, & ! +   
     &        0.00000,    0.56000,    0.00000,    0.00000,    0.00000, & ! 1   
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
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.51000, & ! +   
     &        0.98400,    0.21500,    1.00000,    0.00000,    0.70800, & ! 7   
     &        0.50000,    0.06500,    0.01500,    0.62400,    0.00800, & ! +   
     &        0.06400,    0.01000,    0.00000,    0.26000,    0.29000, & ! 8   
     &        0.00000,    1.12500,    0.12500,    0.06300,    0.06300, & ! +   
     &        0.12500,    0.19700,    0.29700,    0.29300,    0.00000, & ! 9   
     &        0.70000,    0.00000,    0.02400,    0.05400,    0.08200, & ! +   
     &        0.08700,    0.07600,    0.12100,    0.95000,    0.00000, & ! O   
     &        0.26100,    0.07000,    0.14900,    0.04000,    0.00000, & ! +   
     &        0.12800,    0.00500,    0.01300,    0.00000,    0.17900, & ! 1   
     &        0.15600,    0.14000,    0.00000,    0.11000,    0.12800, & ! +   
     &        0.12800,    0.20100,    0.05900,    0.46000,    0.00000, & ! 2   
     &        0.20100,    0.05900,    0.46000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.10000, & ! 5   
     &        1.00000,    0.84000,    0.20200,    0.11600,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.10400,    0.11700, & ! 6   
     &        0.12400,    0.02600,    0.03300,    0.62500,    0.05100, & ! +   
     &        1.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.02900,    0.06900,    0.08500, & ! 8   
     &        0.05000,    0.00000,    0.89400,    0.86400,    0.86400, & ! +   
     &        0.86400,    0.83800,    0.50300,    1.00000,    0.03000, & ! 9   
     &        0.07000,    1.73700,    0.35200,    0.12700,    1.49200, & ! +   
     &        0.84000,    0.46900,    0.46900,    0.05000,    0.01600, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00260,    0.00350, & ! 5   
     &        0.00050,    0.10430,    0.20280,    0.12390,    0.10270, & ! +   
     &        0.07020,    0.25770,    0.03850,    0.00000/           !6   

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
     &        0.00000,    0.44000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.40000, & ! 7   
     &        1.00000,    0.44000,    1.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.40000,    0.00000,    0.44000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.40000,    0.00000,    0.44000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.03500,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.07400,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.49100,    0.45700,    0.00000, & ! +   
     &        0.00000,    0.23900,    0.00000,    0.09000,    0.08500, & ! 5   
     &        0.07400,    0.50000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.20100,    0.43200, & ! 7   
     &        0.19500,    0.20100,    0.43200,    0.00000,    0.11700, & ! +   
     &        0.00700,    0.41600,    0.10000,    0.50000,    0.00000, & ! 8   
     &        0.67000,    0.30000,    0.12400,    0.00000,    0.00000, & ! +   
     &        0.33600,    0.14000,    0.05100,    0.46700,    0.07100, & ! 9   
     &        0.08700,    0.17500,    0.10200,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.16700,    0.18500,    0.96700,    0.00000, & ! O   
     &        0.10200,    0.02000,    0.00000,    0.11000,    0.00000, & ! +   
     &        0.00000,    0.15000,    0.00000,    0.00000,    0.00000, & ! 1   
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
     &        0.00000,    0.00000,    0.37000,    0.00000,    0.27800, & ! +   
     &        0.98400,    0.50000,    0.00000,    0.00000,    0.48000, & ! 7   
     &        0.18500,    0.11500,    0.01500,    0.23000,    0.00800, & ! +   
     &        0.93600,    0.24000,    0.00000,    0.04000,    0.09000, & ! 8   
     &        0.00000,    0.82500,    0.82500,    0.91300,    0.41300, & ! +   
     &        0.82500,    0.00200,    1.51100,    0.29300,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.02400,    0.05400,    0.08200, & ! +   
     &        0.08700,    0.07600,    0.12100,    0.01000,    0.00000, & ! O   
     &        0.70400,    0.02600,    0.14900,    0.10600,    0.00000, & ! +   
     &        0.58200,    0.00500,    0.00300,    0.00000,    0.83500, & ! 1   
     &        0.00800,    0.14000,    0.00000,    0.11000,    0.12800, & ! +   
     &        0.12800,    0.00100,    0.49000,    0.46000,    0.00000, & ! 2   
     &        0.00100,    0.49000,    0.46000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.10000, & ! 5   
     &        0.00000,    0.08500,    0.20200,    0.19800,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.48200,    0.14500, & ! 6   
     &        0.12400,    0.02600,    0.80200,    0.94700,    0.05100, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.12400,    0.86300,    0.08500, & ! 8   
     &        0.01600,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.68800,    0.00000,    0.48200, & ! 9   
     &        0.07000,    0.16500,    0.02200,    0.03600,    0.10600, & ! +   
     &        0.00000,    0.35900,    0.35900,    0.01600,    2.25800, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00230,    0.22390, & ! 5   
     &        0.20510,    0.18930,    0.04710,    0.18310,    0.20450, & ! +   
     &        0.11160,    0.07390,    0.26670,    0.00000/           !6   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.40000, & ! 7   
     &        0.00000,    0.44000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.44000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.44000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        2.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.08800,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.01200,    0.00000,    0.04100,    0.14200, & ! 5   
     &        0.14700,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.33400,    0.56800, & ! 7   
     &        0.30500,    0.33400,    0.56800,    0.00000,    0.11400, & ! +   
     &        0.58000,    0.08400,    0.90000,    0.00000,    0.00000, & ! 8   
     &        0.34000,    0.67500,    0.05000,    0.00000,    0.00000, & ! +   
     &        0.05500,    0.12400,    0.57200,    0.23300,    0.07100, & ! 9   
     &        0.30300,    0.17500,    0.07400,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.08300,    0.50000,    0.03100,    0.00000, & ! O   
     &        0.34000,    0.00000,    0.00000,    0.08400,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.27800, & ! +   
     &        1.00000,    0.50000,    0.00000,    0.00000,    0.47100, & ! 7   
     &        0.50000,    0.46000,    0.11500,    0.32000,    0.27500, & ! +   
     &        1.00000,    0.75000,    0.00000,    0.31000,    0.11000, & ! 8   
     &        0.00000,    0.20000,    0.20000,    0.10000,    0.10000, & ! +   
     &        1.20000,    0.02200,    0.33700,    0.00500,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.29000,    0.23800,    0.10000, & ! +   
     &        0.08400,    0.28600,    0.07400,    0.05000,    0.00000, & ! O   
     &        1.00000,    0.44500,    0.00200,    0.20900,    0.00000, & ! +   
     &        0.01000,    0.30300,    0.03400,    0.00000,    0.51000, & ! 1   
     &        0.21200,    0.07200,    0.00000,    0.15800,    0.08800, & ! +   
     &        0.08800,    0.41100,    0.12100,    0.01200,    0.00000, & ! 2   
     &        0.41100,    0.12100,    0.01200,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.10000, & ! 5   
     &        0.00000,    0.03600,    0.00900,    0.11600,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.10600,    0.50200, & ! 6   
     &        0.07400,    0.21600,    0.54100,    1.00000,    0.04200, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.30600,    0.45700,    0.27500, & ! 8   
     &        2.25800,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.06100,    0.00000,    0.48800, & ! 9   
     &        0.07800,    0.16500,    0.08000,    0.20600,    0.10600, & ! +   
     &        0.00000,    0.00000,    0.00000,    2.25800,    0.58200, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.29440,    0.18200, & ! 5   
     &        0.17640,    0.16680,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.40000, & ! 7   
     &        0.00000,    0.44000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.50400,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.01200,    0.00000,    0.02000,    0.78200, & ! 5   
     &        0.13900,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.40700,    0.65200, & ! 7   
     &        0.00000,    0.40700,    0.65200,    0.00000,    0.27400, & ! +   
     &        0.19000,    0.50000,    0.33300,    0.00000,    0.00000, & ! 8   
     &        0.33000,    0.30000,    0.95000,    0.00000,    0.00000, & ! +   
     &        0.12900,    0.21000,    0.22700,    0.00000,    0.00200, & ! 9   
     &        0.16300,    0.01100,    0.06100,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.25000,    0.00000,    0.03100,    0.00000, & ! O   
     &        0.05000,    0.00000,    0.00000,    0.08400,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.10000, & ! +   
     &        0.00000,    0.18500,    0.00000,    0.00000,    1.00000, & ! 7   
     &        0.37500,    0.12000,    0.11500,    0.35700,    0.12200, & ! +   
     &        0.00000,    0.25000,    0.00000,    0.02000,    0.05000, & ! 8   
     &        0.00000,    0.37500,    0.37500,    0.93800,    0.18800, & ! +   
     &        0.37500,    0.77600,    0.33700,    0.00700,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.57000,    0.15100,    0.38000, & ! +   
     &        0.23800,    0.11200,    0.40500,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.12200,    0.02900,    0.07100,    0.00000, & ! +   
     &        0.00700,    0.08800,    0.77400,    0.00000,    0.14400, & ! 1   
     &        0.00300,    0.57900,    0.00000,    0.10000,    0.31200, & ! +   
     &        0.31200,    0.38500,    0.12100,    0.02300,    0.00000, & ! 2   
     &        0.38500,    0.12100,    0.02300,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.06500,    0.01800,    0.54100,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.10400,    0.23700, & ! 6   
     &        0.14700,    0.48400,    0.08200,    0.00000,    0.04200, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.54000,    0.47300,    0.17700, & ! 8   
     &        0.58200,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.31200,    0.00000,    1.00000, & ! 9   
     &        0.34000,    0.00300,    0.25800,    0.07200,    0.19000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.58200,    0.58200, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.20210,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

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
     &        0.00000,    0.44000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.37600,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.01200,    0.00000,    0.07500,    0.02600, & ! 5   
     &        0.56500,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.12900,    0.65200, & ! 7   
     &        0.00000,    0.12900,    0.65200,    0.00000,    0.15300, & ! +   
     &        0.36600,    0.00000,    0.10000,    0.00000,    0.00000, & ! 8   
     &        0.33000,    1.00000,    0.35100,    0.00000,    0.00000, & ! +   
     &        0.01300,    0.02300,    0.21800,    0.00000,    0.21100, & ! 9   
     &        0.78000,    0.42900,    0.21400,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.03300,    0.00000, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.01200, & ! +   
     &        0.00000,    0.07500,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.12500,    0.35500,    0.00100,    1.00000,    0.40000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.38700,    0.00000, & ! 8   
     &        0.00000,    0.07400,    0.07400,    0.03700,    0.03700, & ! +   
     &        0.07400,    0.03400,    0.02900,    0.68400,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.02900,    0.18100,    0.15900, & ! +   
     &        0.18500,    0.15900,    0.11200,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.02400,    0.43800,    0.08600,    0.00000, & ! +   
     &        0.66600,    0.50000,    0.16900,    0.00000,    0.08000, & ! 1   
     &        0.00300,    0.16300,    0.00000,    0.12300,    0.13400, & ! +   
     &        0.13400,    0.03700,    0.24900,    0.00200,    0.00000, & ! 2   
     &        0.03700,    0.24900,    0.00200,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.07000,    0.01200,    0.00700,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.19700,    0.18600, & ! 6   
     &        0.13900,    0.27400,    0.18000,    0.00000,    0.71200, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    1.00000,    0.67100, & ! 8   
     &        0.58200,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.34300,    0.03400,    0.04400,    0.21500,    0.38300, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.58200,    0.54800, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00190,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

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
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.20500,    0.00000,    0.08400,    0.05800, & ! 5   
     &        0.02400,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.10700,    0.65200, & ! 7   
     &        0.00000,    0.10700,    0.65200,    0.00000,    0.01900, & ! +   
     &        0.18400,    0.00000,    0.10000,    0.00000,    0.00000, & ! 8   
     &        0.33000,    0.00000,    0.05000,    0.00000,    0.00000, & ! +   
     &        0.15000,    0.74200,    0.00800,    0.00000,    0.00100, & ! 9   
     &        1.00000,    0.00100,    0.23000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.29000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    1.00000,    0.00100,    1.00000,    0.19200, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.61000,    0.00000, & ! 8   
     &        0.00000,    0.25100,    0.25100,    0.12600,    0.12600, & ! +   
     &        0.25100,    0.02000,    0.05100,    0.06900,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.26100,    0.06500,    0.04100, & ! +   
     &        0.16100,    0.08800,    0.02200,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.33200,    0.23600,    0.40700,    0.00000, & ! +   
     &        0.00700,    0.01100,    0.83100,    0.00000,    0.00200, & ! 1   
     &        0.29900,    0.11600,    0.00000,    0.07200,    0.07700, & ! +   
     &        0.07700,    0.00700,    0.06300,    0.40300,    0.00000, & ! 2   
     &        0.00700,    0.06300,    0.40300,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.84000,    0.05500,    0.02200,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.28500,    0.67600, & ! 6   
     &        0.56500,    0.27400,    0.54100,    0.00000,    0.49800, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.06700, & ! 8   
     &        0.03500,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.07500,    0.28700,    0.04100,    0.01900,    0.31700, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.03500,    0.03500, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00230,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.03400,    0.00000,    0.16000,    0.69800, & ! 5   
     &        0.44800,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.26700,    0.00000, & ! 7   
     &        0.00000,    0.26700,    0.00000,    0.00000,    0.19500, & ! +   
     &        0.35000,    0.00000,    0.10000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.05000,    0.00000,    0.00000, & ! +   
     &        0.33200,    0.10000,    0.57200,    0.00000,    0.08300, & ! 9   
     &        0.00000,    0.03600,    0.07400,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.75000,    0.00000,    0.20400, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.61000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! +   
     &        1.00000,    0.02300,    0.01700,    0.00200,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.11600,    0.19500,    0.33600, & ! +   
     &        0.04700,    0.04500,    0.03600,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.98300,    0.42600,    1.00000,    0.00000, & ! +   
     &        0.03600,    0.50000,    0.00000,    0.00000,    0.01200, & ! 1   
     &        0.16100,    0.00200,    0.00000,    0.18500,    0.02600, & ! +   
     &        0.02600,    0.00300,    0.12700,    0.23900,    0.00000, & ! 2   
     &        0.00300,    0.12700,    0.23900,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    1.00000,    0.15900,    0.23700,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.58600,    0.28000, & ! 6   
     &        0.02400,    0.48400,    0.83500,    0.00000,    0.19500, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 8   
     &        0.15800,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.25300,    0.41200,    0.37800,    0.03800,    0.08600, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.15800,    0.15800, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.25600,    0.00000,    0.00000,    0.85800, & ! 5   
     &        0.02600,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.78300,    0.00000, & ! 7   
     &        0.00000,    0.78300,    0.00000,    0.00000,    0.19500, & ! +   
     &        0.35000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.05000,    0.00000,    0.00000, & ! +   
     &        0.15000,    0.37200,    0.85000,    0.00000,    0.14300, & ! 9   
     &        0.00000,    0.00400,    0.06300,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.25000,    0.00000,    0.39000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.25000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.34400,    0.05600,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.31400,    0.19500,    0.14400, & ! +   
     &        0.25300,    0.06700,    0.08800,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.01700,    0.10600,    0.00000,    0.00000, & ! +   
     &        0.00100,    0.04400,    0.00000,    0.00000,    0.02300, & ! 1   
     &        0.13100,    0.32000,    0.00000,    0.20200,    0.22100, & ! +   
     &        0.22100,    0.00900,    0.03300,    0.00500,    0.00000, & ! 2   
     &        0.00900,    0.03300,    0.00500,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.54700,    0.10900,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.85500, & ! 6   
     &        0.44800,    0.78400,    0.00000,    0.00000,    0.01700, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.18500,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.98300,    0.24700,    1.00000,    0.19200,    0.04200, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.18500,    0.18500, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.03000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.23100, & ! +   
     &        0.13900,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.17400,    0.04700,    0.00000,    0.00000,    0.40200, & ! 9   
     &        0.00000,    0.01000,    0.00800,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.16000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.24000,    1.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    1.00000,    0.31200,    0.23900, & ! +   
     &        0.25300,    0.27800,    0.35200,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.14600,    0.00000,    0.00000, & ! +   
     &        0.01200,    0.00300,    0.00000,    0.00000,    0.31900, & ! 1   
     &        0.11400,    0.31900,    0.00000,    0.30900,    0.24700, & ! +   
     &        0.24700,    0.00300,    0.20800,    0.00100,    0.00000, & ! 2   
     &        0.00300,    0.20800,    0.00100,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.04500,    0.59100,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.02600,    0.00000,    0.00000,    0.00000,    0.00900, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.27400,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.01700,    0.07600,    0.00000,    0.33700,    0.02500, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.27400,    0.27400, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.25200,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.79400, & ! +   
     &        0.00300,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.71100,    0.00100,    0.00000,    0.00000,    0.11500, & ! 9   
     &        0.00000,    0.17000,    0.12400,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.15000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.34500,    1.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.07300,    0.04700, & ! +   
     &        0.19800,    0.28600,    0.23000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00900,    0.00900,    0.00000,    0.00000,    0.68100, & ! 1   
     &        0.45300,    0.68100,    0.00000,    0.36900,    0.17800, & ! +   
     &        0.17800,    0.00200,    0.05700,    0.00400,    0.00000, & ! 2   
     &        0.00200,    0.05700,    0.00400,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.30000,    0.05100,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.03000,    0.00000,    0.00000,    0.00000,    0.00900, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00700,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.13000,    0.00000,    0.16900,    0.05800, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00700,    0.00700, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

      DATA ( SC( IRXXN, 15 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.07300,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00400,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.04800,    0.00000,    0.00000,    0.32900, & ! 9   
     &        0.00000,    0.00800,    0.08300,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.10000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00800,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.43500,    0.55500, & ! +   
     &        0.05500,    0.10200,    0.15100,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.16800,    0.18500,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.07100,    0.00000,    0.00000,    1.00000,    0.06800, & ! +   
     &        0.06800,    0.40900,    0.00200,    0.22800,    0.00000, & ! 2   
     &        0.40900,    0.00200,    0.22800,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.02000,    0.04000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.25200,    0.00000,    0.00000,    0.00000,    0.11500, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00300,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    1.00000,    0.00000,    0.83100,    0.16100, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00300,    0.00300, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

      DATA ( SC( IRXXN, 16 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.07300,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00300,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00700, & ! 9   
     &        0.00000,    0.03100,    0.19000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.20000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00200,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.58600,    0.46100,    0.04300,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.16900,    0.15900,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.33300,    0.00000,    0.00000,    0.00000,    0.05700, & ! +   
     &        0.05700,    1.00000,    0.17200,    1.00000,    0.00000, & ! 2   
     &        1.00000,    0.17200,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00300,    0.68600,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.07300,    0.00000,    0.00000,    0.00000,    0.14000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00300,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.01300, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00300,    0.00300, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

      DATA ( SC( IRXXN, 17 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.71300,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.09500,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.52800, & ! 9   
     &        0.00000,    0.18900,    0.26100,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.08100,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.70500,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.83100,    0.26800,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.01900,    0.00000,    0.00000,    0.00000,    0.10100, & ! +   
     &        0.10100,    1.00000,    0.06800,    1.00000,    0.00000, & ! 2   
     &        1.00000,    0.06800,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.04100,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.07300,    0.00000,    0.00000,    0.00000,    0.42000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.15800,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.19100, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.15800,    0.15800, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

      DATA ( SC( IRXXN, 18 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.16300,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.30500,    0.06600,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.25500,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.01100,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.05100,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00300,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00300,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.04600,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.71300,    0.00000,    0.00000,    0.00000,    0.76200, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00600,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.31900, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00600,    0.00600, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

      DATA ( SC( IRXXN, 19 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.16300,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.15700,    0.59100,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.73700,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.05200,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.03300,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.03900,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.03900,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.54700,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00600,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.68100, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00600,    0.00600, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

      DATA ( SC( IRXXN, 20 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.09500,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.63600,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00100,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00200,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00200,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.90800,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00100,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00100,    0.00100, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

      DATA ( SC( IRXXN, 21 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.26400,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
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
     &        0.02400,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00100,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00100,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.10900,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.10900,    0.10900, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

      DATA ( SC( IRXXN, 22 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.06500,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.50200,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.50200,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

      DATA ( SC( IRXXN, 23 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.23500,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.42800,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.42800,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

      DATA ( SC( IRXXN, 24 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.03700,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

      DATA ( SC( IRXXN, 25 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.07300,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

      DATA ( SC( IRXXN, 26 ), IRXXN = 1, NRXNS ) / & 
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
     &        0.13600,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000/           !6   

      INTEGER            :: NREACT( NRXNS )

      DATA ( NREACT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    1,    1,    1,    2,    1,    1,    1,    1,    1, & ! 1   
     &      1,    2,    1,    2,    2,    2,    2,    1,    2,    2, & ! 2   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    2, & ! 3   
     &      1,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    1,    1,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    1,    1,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! O   
     &      2,    2,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    2,    2,    2,    1,    2,    2,    1, & ! 3   
     &      2,    2,    1,    2,    1,    2,    2,    2,    2,    2, & ! 4   
     &      1,    2,    1,    2,    1,    2,    1,    1,    1,    2, & ! 5   
     &      2,    1,    2,    2,    1,    2,    2,    2,    1,    1, & ! 6   
     &      2,    1,    2,    2,    2,    1,    2,    2,    1,    2, & ! 7   
     &      2,    2,    2,    2,    2,    1,    2,    2,    2,    1, & ! 8   
     &      2,    2,    2,    1,    2,    1,    2,    1,    2,    1, & ! 9   
     &      2,    2,    2,    2,    2,    1,    2,    1,    2,    2, & ! O   
     &      2,    2,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 2   
     &      2,    1,    2,    2,    1,    1,    2,    2,    2,    2, & ! 3   
     &      2,    2,    1,    1,    1,    2,    2,    1,    2,    2, & ! 4   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      1,    2,    2,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    1, & ! 1   
     &      2,    2,    2,    2,    2,    2,    1,    2,    1,    1, & ! 2   
     &      1,    1,    1,    2,    2,    2,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    1/     !  6   
      INTEGER            :: NPRDCT( NRXNS )

      DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    1,    0,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    2,    1,    1,    2,    1,    2,    1,    1,    1, & ! 1   
     &      1,    1,    2,    1,    1,    2,    1,    2,    2,    1, & ! 2   
     &      2,    1,    2,    4,    1,    1,    1,    1,    3,    1, & ! 3   
     &      1,    1,    0,    3,    1,    3,    1,    1,    3,    2, & ! 4   
     &      2,    1,    0,    1,    3,    0,    0,    0,    1,    3, & ! 5   
     &      0,    0,    1,    2,    5,    3,    6,    3,    5,    2, & ! 6   
     &      2,    2,    1,    2,    8,    6,    9,    6,    7,    5, & ! 7   
     &      5,    6,    5,    1,    2,    6,    4,    7,    4,    5, & ! 8   
     &      3,    3,    4,    6,    3,    1,    2,    6,    4,    7, & ! 9   
     &      4,    4,    3,    3,    4,    7,    5,    3,    1,    2, & ! O   
     &      1,    1,    3,    1,    0,    1,    0,    1,    0,    1, & ! 1   
     &      0,    1,    0,    1,    0,    1,    0,    1,    0,    1, & ! 2   
     &      0,    2,    1,    2,    3,    1,    3,    2,    6,    6, & ! 3   
     &      2,    4,    3,   10,    5,    2,    2,    6,    7,    3, & ! 4   
     &      3,   12,    3,   11,   12,   17,    6,    2,    2,    3, & ! 5   
     &      4,    3,    2,    3,    1,    5,    2,    1,    1,    0, & ! 6   
     &      1,    0,    2,   12,   10,    7,   12,   10,    1,   14, & ! 7   
     &     21,    8,   11,    6,    1,   10,    9,   12,    2,    4, & ! 8   
     &     14,   15,   12,    7,   17,   10,   20,   19,    1,    3, & ! 9   
     &      2,    8,    7,    9,    1,    8,    6,    3,    8,    4, & ! O   
     &      4,    6,    1,    0,    1,    0,    1,    0,    1,    0, & ! 1   
     &      1,    0,    1,    0,    1,    0,    1,    0,    1,    0, & ! 2   
     &      1,    0,    1,    0,    1,    0,    1,    0,    1,    0, & ! 3   
     &      1,    0,    1,    0,    1,    0,    1,    2,    0,    1, & ! 4   
     &      1,    0,    1,    1,    0,    1,    1,    0,    1,    1, & ! 5   
     &      0,    1,    0,    1,    0,    1,    5,    6,    4,   10, & ! 6   
     &      7,    9,    5,    2,    8,    9,   10,   12,   10,   16, & ! 7   
     &      7,    8,    2,   11,    8,    1,   10,   11,   12,   11, & ! 8   
     &     11,   13,   20,   14,    2,    5,    4,   13,   16,   16, & ! 9   
     &     17,   17,   18,    7,    4,    7,   12,   14,   11,    2, & ! O   
     &     17,   19,   10,    3,   14,   26,   14,    3,   15,   18, & ! 1   
     &     18,   17,   24,   17,    3,   17,   24,   17,    3,    1, & ! 2   
     &      1,    2,    1,    1,    2,    2,    1,    2,    1,    2, & ! 3   
     &      2,    1,    2,    2,    2,    2,    1,    2,    2,    1, & ! 4   
     &      2,    3,    2,    3,    7,    5,   11,   20,   16,    4, & ! 5   
     &      3,    4,    2,   11,   12,   18,   12,   11,    7,   18, & ! 6   
     &      6,    1,    2,    5,    1,    0,    1,    0,    1,    0, & ! 7   
     &      2,    4,    9,    9,   11,   22,    2,    5,    5,    5, & ! 8   
     &      5,    5,    8,    5,    8,   13,   15,   12,   15,   19, & ! 9   
     &      5,    6,    6,   22,   22,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    0,    0,    1,    1,    1,    1,    0, & ! 1   
     &      1,    1,    1,    1,    1,    1,    0,    1,    2,    2, & ! 2   
     &      2,    1,    1,    1,    1,    1,    0,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    2,   10,    7,    7,    7,    6,    6,    6, & ! 5   
     &      6,    6,    6,    1/     !  6   

      INTEGER, PARAMETER :: NMPHOT =  54
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,   16,   17,   18,   19,   23,   28,   34,   41,   65, & 
     &     75,   86,   98,  132,  133,  137,  140,  143,  145,  151, & 
     &    153,  155,  157,  158,  159,  162,  165,  169,  170,  172, & 
     &    176,  179,  186,  190,  194,  196,  198,  200,  206,  208, & 
     &    285,  330,  332,  335,  336,  343,  344,  348,  371,  374, & 
     &    414,  415,  420,  427/

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     10,   10,   10,   11,   12,   13,   14,   15,   16,   17, & 
     &     17,   17,   17,   18,   19,   20,   21,    1,    1,   22, & 
     &     23,   23,   24,   25,   24,   16,   26,   27,   28,   29, & 
     &     17,   30,   31,   32,   33,   34,   35,   36,   37,   38, & 
     &     11,   12,   13,   28/

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & 
     &     31,   32,   33,   34,   35,   36,   37,   38,   39,   40, & 
     &     41,   42,   43,   44,   45,   46,   47,   48,   49,   50, & 
     &     51,   52,   53,   54/

      INTEGER, PARAMETER :: MHETERO =   9
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    429,  430,  431,  432,  433,  434,  435,  436,  464/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    2,    3,    4,    5,    6,    6,    7,    8/

      INTEGER, PARAMETER :: NPHOTAB =  38
      CHARACTER( 16 )    :: PHOTAB( NPHOTAB )

      DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & 
     &   'NO2_06          ', 'NO3NO_06        ', 'NO3NO2_6        ', & 
     &   'O3O1D_06        ', 'O3O3P_06        ', 'HONO_06         ', & 
     &   'HNO3            ', 'HNO4_06         ', 'H2O2            ', & 
     &   'PAN             ', 'HCHOR_06        ', 'HCHOM_06        ', & 
     &   'CCHO_R          ', 'C2CHO           ', 'ACET_06         ', & 
     &   'MEK_06          ', 'COOH            ', 'GLY_07R         ', & 
     &   'GLY_07M         ', 'MGLY_06         ', 'BACL_07         ', & 
     &   'BALD_06         ', 'AFG1            ', 'MACR_06         ', & 
     &   'MVK_06          ', 'IC3ONO2         ', 'HOCCHO_IUPAC    ', & 
     &   'ACRO_09         ', 'PAA             ', 'CL2             ', & 
     &   'CLNO_06         ', 'CLONO           ', 'CLNO2           ', & 
     &   'CLONO2_1        ', 'CLONO2_2        ', 'HOCL_06         ', & 
     &   'CLCCHO          ', 'CLACET          '/

      INTEGER, PARAMETER :: NHETERO =   8
      CHARACTER( 16 )    :: HETERO( NHETERO )

      DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & 
     &   'HETERO_NO2      ', 'HETERO_N2O5IJ   ', 'HETERO_N2O5K    ', &
     &   'HETERO_H2NO3PAIJ', 'HETERO_H2NO3PAK ', 'HETERO_H2NO3PBIJ', &
     &   'HETERO_H2NO3PBK ', 'HETERO_IEPOX    '/

      CHARACTER( 16 )    :: RXLABEL( NRXNS )

      DATA ( RXLABEL( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &    '1               ', '2               ', '3               ', & ! 0   
     &    '4               ', '5               ', '6               ', & ! 1   
     &    '7               ', '8               ', '9               ', & ! 2   
     &    '10              ', '11              ', '12              ', & ! 3   
     &    '13              ', '14              ', '15              ', & ! 4   
     &    '16              ', '17              ', '18              ', & ! 5   
     &    '19              ', '20              ', '21              ', & ! 6   
     &    '22              ', '23              ', '24              ', & ! 7   
     &    '25              ', '26              ', '27              ', & ! 8   
     &    '28              ', '29              ', '30              ', & ! 9   
     &    '31              ', '32              ', '33              ', & ! 0   
     &    '34              ', '35              ', '36              ', & ! 1   
     &    '37              ', '38              ', '39              ', & ! 2   
     &    '40              ', '41              ', '42              ', & ! 3   
     &    '43              ', '44              ', '45              ', & ! 4   
     &    'BR01            ', 'BR02            ', 'BR03            ', & ! 5   
     &    'BR04            ', 'BR05            ', 'BR06            ', & ! 6   
     &    'BR07            ', 'BR08            ', 'BR09            ', & ! 7   
     &    'BR10            ', 'BR11            ', 'BR12            ', & ! 8   
     &    'BR13            ', 'BR14            ', 'BR15            ', & ! 9   
     &    'BR16            ', 'BR17            ', 'BR18            ', & ! 0   
     &    'BR19            ', 'BR20            ', 'BR21            ', & ! 1   
     &    'BR22            ', 'BR23            ', 'BR24            ', & ! 2   
     &    'BR25            ', 'BR26            ', 'BR27            ', & ! 3   
     &    'BR28            ', 'BR29            ', 'BR30            ', & ! 4   
     &    'BR31            ', 'BR32            ', 'BR33            ', & ! 5   
     &    'BR34            ', 'BR35            ', 'BR36            ', & ! 6   
     &    'BR37            ', 'BR38            ', 'BR39            ', & ! 7   
     &    'BR40            ', 'BR41            ', 'BR42            ', & ! 8   
     &    'BR43            ', 'BR44            ', 'BR45            ', & ! 9   
     &    'BR46            ', 'BR47            ', 'BR48            ', & ! 0   
     &    'BR49            ', 'BR50            ', 'BR51            ', & ! 1   
     &    'BR52            ', 'BR53            ', 'BR54            ', & ! 2   
     &    'BR55            ', 'BR56            ', 'BR57            ', & ! 3   
     &    'BR58            ', 'BR59            ', 'BR60            ', & ! 4   
     &    'BR61            ', 'BR62            ', 'BR63            ', & ! 5   
     &    'BR64            ', 'BR65            ', 'BR66            ', & ! 6   
     &    'BR67            ', 'BR68            ', 'RO01            ', & ! 7   
     &    'RO02            ', 'RO03            ', 'RO04            ', & ! 8   
     &    'RO05            ', 'RO06            ', 'RO07            ', & ! 9   
     &    'RO08            ', 'RO09            ', 'RO10            ', & ! 0   
     &    'RO11            ', 'RO12            ', 'RO13            ', & ! 1   
     &    'RO14            ', 'RO15            ', 'RO16            ', & ! 2   
     &    'RO17            ', 'RO18            ', 'BP01            ', & ! 3   
     &    'BP02            ', 'BP03            ', 'BP07            ', & ! 4   
     &    'BP08            ', 'BP09            ', 'BP10            ', & ! 5   
     &    'BP11            ', 'BP12            ', 'BP13            ', & ! 6   
     &    'BP14            ', 'BP15            ', 'BP16            ', & ! 7   
     &    'BP17            ', 'BP18            ', 'BP19            ', & ! 8   
     &    'BP20            ', 'BP21            ', 'BP22            ', & ! 9   
     &    'BP23            ', 'BP24            ', 'BP25            ', & ! 0   
     &    'BP26            ', 'BP27            ', 'BP28            ', & ! 1   
     &    'BP29            ', 'BP30            ', 'BP31            ', & ! 2   
     &    'BP32            ', 'BP33            ', 'BP34            ', & ! 3   
     &    'BP35            ', 'BP36            ', 'BP37            ', & ! 4   
     &    'BP38            ', 'BP39            ', 'BP40            ', & ! 5   
     &    'BP41            ', 'BP42            ', 'BP43            ', & ! 6   
     &    'BP44            ', 'BP45            ', 'BP46            ', & ! 7   
     &    'BP47            ', 'BP48            ', 'BP49            ', & ! 8   
     &    'BP50            ', 'BP51            ', 'BP52            ', & ! 9   
     &    'BP53            ', 'BP54            ', 'BP55            ', & ! 0   
     &    'BP56            ', 'BP57            ', 'BP58            ', & ! 1   
     &    'BP59            ', 'BP60            ', 'BP62            ', & ! 2   
     &    'BP63            ', 'BP64            ', 'BP65            ', & ! 3   
     &    'BP66            ', 'BP67            ', 'BP68            ', & ! 4   
     &    'BP69            ', 'BP70            ', 'BP71            ', & ! 5   
     &    'BP72            ', 'BP73            ', 'BP74            ', & ! 6   
     &    'BP75            ', 'BP76            ', 'BP77            ', & ! 7   
     &    'BP78            ', 'BP79            ', 'BP80            ', & ! 8   
     &    'BP81            ', 'BP82            ', 'BP84            ', & ! 9   
     &    'BP85            ', 'BP86            ', 'PO01            ', & ! 0   
     &    'PO02            ', 'PO03            ', 'PO04            ', & ! 1   
     &    'PO05            ', 'PO06            ', 'PO07            ', & ! 2   
     &    'PO08            ', 'PO09            ', 'PO10            ', & ! 3   
     &    'PO11            ', 'PO12            ', 'PO13            ', & ! 4   
     &    'PO14            ', 'PO15            ', 'PO16            ', & ! 5   
     &    'PO17            ', 'PO18            ', 'PO19            ', & ! 6   
     &    'PO20            ', 'PO21            ', 'PO22            ', & ! 7   
     &    'PO23            ', 'PO24            ', 'PO25            ', & ! 8   
     &    'PO26            ', 'PO27            ', 'PO28            ', & ! 9   
     &    'PO29            ', 'PO30            ', 'PO31            ', & ! 0   
     &    'PO32            ', 'PO33            ', 'PO34            ', & ! 1   
     &    'PO35            ', 'PO36            ', 'PO37            ', & ! 2   
     &    'PO38            ', 'PO39            ', 'PO40            ', & ! 3   
     &    'PO41            ', 'PO42            ', 'PO43            ', & ! 4   
     &    'PO41a           ', 'PO42b           ', 'PO43c           ', & ! 5   
     &    'PO44            ', 'PO45            ', 'PO46            ', & ! 6   
     &    'PO47            ', 'PO48            ', 'PO49            ', & ! 7   
     &    'PO50            ', 'BE01            ', 'BE02            ', & ! 8   
     &    'BE03            ', 'BE04            ', 'BE05            ', & ! 9   
     &    'BT01            ', 'BT02            ', 'BT03            ', & ! 0   
     &    'BT04            ', 'BT05            ', 'BT06            ', & ! 1   
     &    'BT07            ', 'BT08            ', 'BE06            ', & ! 2   
     &    'BE07            ', 'BE08            ', 'BE09            ', & ! 3   
     &    'IS88            ', 'IS89            ', 'IS92            ', & ! 4   
     &    'IS90            ', 'IS91            ', 'IS96            ', & ! 5   
     &    'IS112           ', 'IS113           ', 'IS114           ', & ! 6   
     &    'BT09            ', 'BT10            ', 'BT11            ', & ! 7   
     &    'BT12            ', 'BE13            ', 'BE14            ', & ! 8   
     &    'BE15            ', 'BT13            ', 'BT14            ', & ! 9   
     &    'BT15            ', 'BT16            ', 'BT17            ', & ! 0   
     &    'BT18            ', 'BL01            ', 'BL02            ', & ! 1   
     &    'BL03            ', 'BL04            ', 'BL05            ', & ! 2   
     &    'AALK            ', 'BL06            ', 'BL07            ', & ! 3   
     &    'BL08            ', 'BL09            ', 'BL10            ', & ! 4   
     &    'BL11            ', 'BL12            ', 'BL13            ', & ! 5   
     &    'BL14            ', 'BL15            ', 'BL15b           ', & ! 6   
     &    'BL16            ', 'BL17            ', 'BL18            ', & ! 7   
     &    'BL19            ', 'BT19            ', 'BT20            ', & ! 8   
     &    'BT21            ', 'BT22            ', 'CI01            ', & ! 9   
     &    'CI02            ', 'CI03            ', 'CI04            ', & ! 0   
     &    'CI05            ', 'CI06            ', 'CI07            ', & ! 1   
     &    'CI08            ', 'CI09            ', 'CI10            ', & ! 2   
     &    'CI11            ', 'CI12            ', 'CI13            ', & ! 3   
     &    'CI14            ', 'CI15            ', 'CI16            ', & ! 4   
     &    'CI17            ', 'CI18            ', 'CI19            ', & ! 5   
     &    'CI20            ', 'CI21            ', 'CI22            ', & ! 6   
     &    'CP01            ', 'CP02            ', 'CP03            ', & ! 7   
     &    'CP04            ', 'CP05            ', 'CP06            ', & ! 8   
     &    'CP07            ', 'CP08            ', 'CP09            ', & ! 9   
     &    'CP10            ', 'CP11            ', 'CP12            ', & ! 0   
     &    'CP13            ', 'CP14            ', 'CP15            ', & ! 1   
     &    'TP01            ', 'CP16            ', 'CP17            ', & ! 2   
     &    'CP18            ', 'CP19            ', 'CP20            ', & ! 3   
     &    'CP21            ', 'CP22            ', 'CP23            ', & ! 4   
     &    'CP24            ', 'CP25            ', 'CP26            ', & ! 5   
     &    'CP27            ', 'CP28            ', 'CE01            ', & ! 6   
     &    'CE02            ', 'TE01            ', 'TE02            ', & ! 7   
     &    'CE03            ', 'TE03            ', 'CE04            ', & ! 8   
     &    'TE04            ', 'TE05            ', 'TE06            ', & ! 9   
     &    'TE07            ', 'TE08            ', 'TE09            ', & ! 0   
     &    'BC01            ', 'BC02            ', 'BC03            ', & ! 1   
     &    'BC04            ', 'BC05            ', 'BC06            ', & ! 2   
     &    'BC07            ', 'BC08            ', 'BC09            ', & ! 3   
     &    'BC09b           ', 'BC10            ', 'BC11            ', & ! 4   
     &    'AE51            ', 'AE52            ', 'AE53            ', & ! 5   
     &    'AE54            ', 'AE55            ', 'AE56            ', & ! 6   
     &    'AE55b           ', 'AE56b           ', 'TR01            ', & ! 7   
     &    'TR02            ', 'TR03            ', 'TR05            ', & ! 8   
     &    'TR06            ', 'TR07            ', 'TR08            ', & ! 9   
     &    'TR09            ', 'TR10            ', 'TR11            ', & ! 0   
     &    'TR12            ', 'TR13            ', 'TR14            ', & ! 1   
     &    'TR15            ', 'TR16            ', 'HET_N02         ', & ! 2   
     &    'HET_N2O5IJ      ', 'HET_N2O5K       ', 'HET_H2NO3PIJA   ', & ! 3   
     &    'HET_H2NO3PKA    ', 'HET_H2NO3PIB    ', 'HET_H2NO3PJB    ', & ! 4   
     &    'HET_H2NO3PKB    ', 'HAL_Ozone       ', 'OLIG_XYLENE1    ', & ! 5   
     &    'OLIG_XYLENE2    ', 'OLIG_TOLUENE1   ', 'OLIG_TOLUENE2   ', & ! 6   
     &    'OLIG_BENZENE1   ', 'OLIG_BENZENE2   ', 'OLIG_TERPENE1   ', & ! 7   
     &    'OLIG_TERPENE2   ', 'OLIG_ISOPRENE1  ', 'OLIG_ISOPRENE2  ', & ! 8   
     &    'OLIG_SESQT1     ', 'OLIG_PAH1       ', 'OLIG_PAH2       ', & ! 9   
     &    'OLIG_ALK1       ', 'OLIG_ALK2       ', 'PCSOA           ', & ! 0   
     &    'POA_AGE1        ', 'POA_AGE2        ', 'POA_AGE3        ', & ! 1   
     &    'POA_AGE4        ', 'POA_AGE5        ', 'POA_AGE6        ', & ! 2   
     &    'POA_AGE7        ', 'POA_AGE8        ', 'POA_AGE9        ', & ! 3   
     &    'POA_AGE10       ', 'HET_IEPOX       '/                   ! 4  

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

      INTEGER, PARAMETER :: NSPECIAL_RXN =  77
      INTEGER            :: ISPECIAL( NSPECIAL_RXN,2 )

      DATA ( ISPECIAL( IRXXN,1 ), IRXXN = 1, NSPECIAL_RXN ) / & 
     &    114,  115,  116,  117,  118,  119,  120,  121,  122,  123, & ! O   
     &    124,  125,  126,  127,  128,  129,  130,  131,  213,  214, & ! 1   
     &    215,  216,  217,  218,  219,  220,  221,  222,  223,  224, & ! 2   
     &    225,  226,  227,  228,  229,  230,  231,  232,  233,  234, & ! 3   
     &    235,  236,  237,  238,  239,  240,  241,  242,  243,  244, & ! 4   
     &    245,  246,  247,  248,  249,  250,  251,  252,  253,  254, & ! 5   
     &    255,  256,  257,  258,  259,  260,  261,  262,  263,  264, & ! 6   
     &    265,  375,  376,  377,  378,  379,  380/     !  7   

      DATA ( ISPECIAL( IRXXN,2 ), IRXXN = 1, NSPECIAL_RXN ) / & 
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! O   
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! 1   
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! 2   
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! 3   
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! 4   
     &      6,    7,    1,    9,    7,    2,    8,    6,    2,    8, & ! 5   
     &      6,    2,    8,    6,    2,    8,    6,    6,    7,    6, & ! 6   
     &      7,    6,    7,    6,    7,    6,    7/     !  7   

      INTEGER, PARAMETER :: NSPECIAL =   9
      CHARACTER( 16 )    :: SPECIAL( NSPECIAL )

      DATA ( SPECIAL( IRXXN ), IRXXN = 1, NSPECIAL ) / & 
     &   'RO2NO           ', 'RO2HO2          ', 'RO2NO3          ', & 
     &   'RO2RO2          ', 'RO2RO3          ', 'RO2RO           ', & 
     &   'RO2XRO          ', 'RO2RO2M         ', 'RO22NN          '/

      INTEGER, PARAMETER :: MAXSPECTERMS =   4
      REAL( 8 )          :: KC_COEFFS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: INDEX_KTERMS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: INDEX_CTERMS( NSPECIAL + 1, MAXSPECTERMS)
      REAL( 8 )          :: OPERATOR_COEFFS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: OPERATORS( NSPECIAL + 1, MAXSPECTERMS)

      DATA ( KC_COEFFS(   1,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  1,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     52,    0,    0,    0/

      DATA ( INDEX_CTERMS(  1,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      2,    0,    0,    0/

      DATA ( KC_COEFFS(   2,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  2,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     53,    0,    0,    0/

      DATA ( INDEX_CTERMS(  2,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     11,    0,    0,    0/

      DATA ( KC_COEFFS(   3,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  3,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     54,    0,    0,    0/

      DATA ( INDEX_CTERMS(  3,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      5,    0,    0,    0/

      DATA ( KC_COEFFS(   4,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  4,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     55,   56,   56,    0/

      DATA ( INDEX_CTERMS(  4,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     19,   23,   24,    0/

      DATA ( KC_COEFFS(   5,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00/

      DATA ( INDEX_KTERMS(  5,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     70,   70,   70,   70/

      DATA ( INDEX_CTERMS(  5,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     25,   29,   36,   39/

      DATA ( KC_COEFFS(   6,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  6,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  6,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( KC_COEFFS(   7,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  7,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  7,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( KC_COEFFS(   8,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  8,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  8,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( KC_COEFFS(   9,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  9,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  9,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   1,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  1,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   2,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  2,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   3,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  3,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   4,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  4,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   5,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  5,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   6,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 5.0000D-01/

      DATA ( OPERATORS(  6,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      1,    3,    5,    4/

      DATA ( OPERATOR_COEFFS(   7,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  7,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      2,    4,    0,    0/

      DATA ( OPERATOR_COEFFS(   8,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     5.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  8,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      4,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   9,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 1.0000D+00, 5.0000D-01, 0.0000D+00/

      DATA ( OPERATORS(  9,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      3,    5,    4,    0/


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
       LOGICAL,  PARAMETER :: USE_SPECIAL_RATES = .TRUE. 
! pointers and names to specific photolysis rates
       INTEGER, PARAMETER  :: IJ_NO2_06           =   1
       INTEGER, PARAMETER  :: IJ_NO3NO_06         =   2
       INTEGER, PARAMETER  :: IJ_NO3NO2_6         =   3
       INTEGER, PARAMETER  :: IJ_O3O1D_06         =   4
       INTEGER, PARAMETER  :: IJ_O3O3P_06         =   5
       INTEGER, PARAMETER  :: IJ_HONO_06          =   6
       INTEGER, PARAMETER  :: IJ_HNO3             =   7
       INTEGER, PARAMETER  :: IJ_HNO4_06          =   8
       INTEGER, PARAMETER  :: IJ_H2O2             =   9
       INTEGER, PARAMETER  :: IJ_PAN              =  10
       INTEGER, PARAMETER  :: IJ_HCHOR_06         =  11
       INTEGER, PARAMETER  :: IJ_HCHOM_06         =  12
       INTEGER, PARAMETER  :: IJ_CCHO_R           =  13
       INTEGER, PARAMETER  :: IJ_C2CHO            =  14
       INTEGER, PARAMETER  :: IJ_ACET_06          =  15
       INTEGER, PARAMETER  :: IJ_MEK_06           =  16
       INTEGER, PARAMETER  :: IJ_COOH             =  17
       INTEGER, PARAMETER  :: IJ_GLY_07R          =  18
       INTEGER, PARAMETER  :: IJ_GLY_07M          =  19
       INTEGER, PARAMETER  :: IJ_MGLY_06          =  20
       INTEGER, PARAMETER  :: IJ_BACL_07          =  21
       INTEGER, PARAMETER  :: IJ_BALD_06          =  22
       INTEGER, PARAMETER  :: IJ_AFG1             =  23
       INTEGER, PARAMETER  :: IJ_MACR_06          =  24
       INTEGER, PARAMETER  :: IJ_MVK_06           =  25
       INTEGER, PARAMETER  :: IJ_IC3ONO2          =  26
       INTEGER, PARAMETER  :: IJ_HOCCHO_IUPAC     =  27
       INTEGER, PARAMETER  :: IJ_ACRO_09          =  28
       INTEGER, PARAMETER  :: IJ_PAA              =  29
       INTEGER, PARAMETER  :: IJ_CL2              =  30
       INTEGER, PARAMETER  :: IJ_CLNO_06          =  31
       INTEGER, PARAMETER  :: IJ_CLONO            =  32
       INTEGER, PARAMETER  :: IJ_CLNO2            =  33
       INTEGER, PARAMETER  :: IJ_CLONO2_1         =  34
       INTEGER, PARAMETER  :: IJ_CLONO2_2         =  35
       INTEGER, PARAMETER  :: IJ_HOCL_06          =  36
       INTEGER, PARAMETER  :: IJ_CLCCHO           =  37
       INTEGER, PARAMETER  :: IJ_CLACET           =  38
       INTEGER, PARAMETER  :: IK_HETERO_NO2       =   1
       INTEGER, PARAMETER  :: IK_HETERO_N2O5IJ    =   2
       INTEGER, PARAMETER  :: IK_HETERO_N2O5K     =   3
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAIJ =   4
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAK  =   5
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBIJ =   6
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBK  =   7
       INTEGER, PARAMETER  :: IK_HETERO_IEPOX     =   8
       END MODULE RXNS_DATA
