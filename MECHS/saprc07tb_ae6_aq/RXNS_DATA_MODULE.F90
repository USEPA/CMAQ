       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: /home/hutzellb/CCTM_git_repository/MECHS/saprc07tb_ae6_aq/mech.def
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

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC = 150
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 168

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
      DATA GAS_CHEM_SPC(  19 ) / 'NO2EX           ' /
      DATA GAS_CHEM_SPC(  20 ) / 'MEO2            ' /
      DATA GAS_CHEM_SPC(  21 ) / 'HCHO            ' /
      DATA GAS_CHEM_SPC(  22 ) / 'COOH            ' /
      DATA GAS_CHEM_SPC(  23 ) / 'MEOH            ' /
      DATA GAS_CHEM_SPC(  24 ) / 'RO2C            ' /
      DATA GAS_CHEM_SPC(  25 ) / 'RO2XC           ' /
      DATA GAS_CHEM_SPC(  26 ) / 'MECO3           ' /
      DATA GAS_CHEM_SPC(  27 ) / 'PAN             ' /
      DATA GAS_CHEM_SPC(  28 ) / 'CCOOOH          ' /
      DATA GAS_CHEM_SPC(  29 ) / 'CCOOH           ' /
      DATA GAS_CHEM_SPC(  30 ) / 'RCO3            ' /
      DATA GAS_CHEM_SPC(  31 ) / 'PAN2            ' /
      DATA GAS_CHEM_SPC(  32 ) / 'xHO2            ' /
      DATA GAS_CHEM_SPC(  33 ) / 'yROOH           ' /
      DATA GAS_CHEM_SPC(  34 ) / 'xCCHO           ' /
      DATA GAS_CHEM_SPC(  35 ) / 'RCOOOH          ' /
      DATA GAS_CHEM_SPC(  36 ) / 'RCOOH           ' /
      DATA GAS_CHEM_SPC(  37 ) / 'BZCO3           ' /
      DATA GAS_CHEM_SPC(  38 ) / 'PBZN            ' /
      DATA GAS_CHEM_SPC(  39 ) / 'BZO             ' /
      DATA GAS_CHEM_SPC(  40 ) / 'MACO3           ' /
      DATA GAS_CHEM_SPC(  41 ) / 'MAPAN           ' /
      DATA GAS_CHEM_SPC(  42 ) / 'TBUO            ' /
      DATA GAS_CHEM_SPC(  43 ) / 'RNO3            ' /
      DATA GAS_CHEM_SPC(  44 ) / 'ACETONE         ' /
      DATA GAS_CHEM_SPC(  45 ) / 'NPHE            ' /
      DATA GAS_CHEM_SPC(  46 ) / 'CRES            ' /
      DATA GAS_CHEM_SPC(  47 ) / 'xOH             ' /
      DATA GAS_CHEM_SPC(  48 ) / 'xNO2            ' /
      DATA GAS_CHEM_SPC(  49 ) / 'xMEO2           ' /
      DATA GAS_CHEM_SPC(  50 ) / 'xMECO3          ' /
      DATA GAS_CHEM_SPC(  51 ) / 'xRCO3           ' /
      DATA GAS_CHEM_SPC(  52 ) / 'xMACO3          ' /
      DATA GAS_CHEM_SPC(  53 ) / 'xTBUO           ' /
      DATA GAS_CHEM_SPC(  54 ) / 'xCO             ' /
      DATA GAS_CHEM_SPC(  55 ) / 'CCHO            ' /
      DATA GAS_CHEM_SPC(  56 ) / 'RCHO            ' /
      DATA GAS_CHEM_SPC(  57 ) / 'xHCHO           ' /
      DATA GAS_CHEM_SPC(  58 ) / 'MEK             ' /
      DATA GAS_CHEM_SPC(  59 ) / 'zRNO3           ' /
      DATA GAS_CHEM_SPC(  60 ) / 'xRCHO           ' /
      DATA GAS_CHEM_SPC(  61 ) / 'HCOOH           ' /
      DATA GAS_CHEM_SPC(  62 ) / 'xMGLY           ' /
      DATA GAS_CHEM_SPC(  63 ) / 'xBACL           ' /
      DATA GAS_CHEM_SPC(  64 ) / 'ROOH            ' /
      DATA GAS_CHEM_SPC(  65 ) / 'xPROD2          ' /
      DATA GAS_CHEM_SPC(  66 ) / 'R6OOH           ' /
      DATA GAS_CHEM_SPC(  67 ) / 'PRD2            ' /
      DATA GAS_CHEM_SPC(  68 ) / 'yR6OOH          ' /
      DATA GAS_CHEM_SPC(  69 ) / 'RAOOH           ' /
      DATA GAS_CHEM_SPC(  70 ) / 'MGLY            ' /
      DATA GAS_CHEM_SPC(  71 ) / 'IPRD            ' /
      DATA GAS_CHEM_SPC(  72 ) / 'xGLY            ' /
      DATA GAS_CHEM_SPC(  73 ) / 'xMEK            ' /
      DATA GAS_CHEM_SPC(  74 ) / 'xAFG1           ' /
      DATA GAS_CHEM_SPC(  75 ) / 'xAFG2           ' /
      DATA GAS_CHEM_SPC(  76 ) / 'GLY             ' /
      DATA GAS_CHEM_SPC(  77 ) / 'AFG1            ' /
      DATA GAS_CHEM_SPC(  78 ) / 'AFG2            ' /
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
      DATA GAS_CHEM_SPC(  94 ) / 'yRAOOH          ' /
      DATA GAS_CHEM_SPC(  95 ) / 'xACROLEIN       ' /
      DATA GAS_CHEM_SPC(  96 ) / 'ETHENE          ' /
      DATA GAS_CHEM_SPC(  97 ) / 'PROPENE         ' /
      DATA GAS_CHEM_SPC(  98 ) / 'BUTADIENE13     ' /
      DATA GAS_CHEM_SPC(  99 ) / 'ISOPRENE        ' /
      DATA GAS_CHEM_SPC( 100 ) / 'ISOPRXN         ' /
      DATA GAS_CHEM_SPC( 101 ) / 'APIN            ' /
      DATA GAS_CHEM_SPC( 102 ) / 'TRPRXN          ' /
      DATA GAS_CHEM_SPC( 103 ) / 'ACETYLENE       ' /
      DATA GAS_CHEM_SPC( 104 ) / 'BENZENE         ' /
      DATA GAS_CHEM_SPC( 105 ) / 'BENZRO2         ' /
      DATA GAS_CHEM_SPC( 106 ) / 'TOLUENE         ' /
      DATA GAS_CHEM_SPC( 107 ) / 'TOLRO2          ' /
      DATA GAS_CHEM_SPC( 108 ) / 'MXYL            ' /
      DATA GAS_CHEM_SPC( 109 ) / 'XYLRO2          ' /
      DATA GAS_CHEM_SPC( 110 ) / 'OXYL            ' /
      DATA GAS_CHEM_SPC( 111 ) / 'PXYL            ' /
      DATA GAS_CHEM_SPC( 112 ) / 'TRIMETH_BENZ124 ' /
      DATA GAS_CHEM_SPC( 113 ) / 'ETOH            ' /
      DATA GAS_CHEM_SPC( 114 ) / 'ALK1            ' /
      DATA GAS_CHEM_SPC( 115 ) / 'ALK2            ' /
      DATA GAS_CHEM_SPC( 116 ) / 'ALK3            ' /
      DATA GAS_CHEM_SPC( 117 ) / 'ALK4            ' /
      DATA GAS_CHEM_SPC( 118 ) / 'ALK5            ' /
      DATA GAS_CHEM_SPC( 119 ) / 'ALK5RXN         ' /
      DATA GAS_CHEM_SPC( 120 ) / 'OLE1            ' /
      DATA GAS_CHEM_SPC( 121 ) / 'OLE2            ' /
      DATA GAS_CHEM_SPC( 122 ) / 'ARO1            ' /
      DATA GAS_CHEM_SPC( 123 ) / 'ARO2            ' /
      DATA GAS_CHEM_SPC( 124 ) / 'TERP            ' /
      DATA GAS_CHEM_SPC( 125 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC( 126 ) / 'SESQRXN         ' /
      DATA GAS_CHEM_SPC( 127 ) / 'CL2             ' /
      DATA GAS_CHEM_SPC( 128 ) / 'CL              ' /
      DATA GAS_CHEM_SPC( 129 ) / 'CLNO            ' /
      DATA GAS_CHEM_SPC( 130 ) / 'CLONO           ' /
      DATA GAS_CHEM_SPC( 131 ) / 'CLNO2           ' /
      DATA GAS_CHEM_SPC( 132 ) / 'HCL             ' /
      DATA GAS_CHEM_SPC( 133 ) / 'CLO             ' /
      DATA GAS_CHEM_SPC( 134 ) / 'CLONO2          ' /
      DATA GAS_CHEM_SPC( 135 ) / 'HOCL            ' /
      DATA GAS_CHEM_SPC( 136 ) / 'xCL             ' /
      DATA GAS_CHEM_SPC( 137 ) / 'xCLCCHO         ' /
      DATA GAS_CHEM_SPC( 138 ) / 'xCLACET         ' /
      DATA GAS_CHEM_SPC( 139 ) / 'CLCCHO          ' /
      DATA GAS_CHEM_SPC( 140 ) / 'CLACET          ' /
      DATA GAS_CHEM_SPC( 141 ) / 'CLCHO           ' /
      DATA GAS_CHEM_SPC( 142 ) / 'BNZNRXN         ' /
      DATA GAS_CHEM_SPC( 143 ) / 'BNZHRXN         ' /
      DATA GAS_CHEM_SPC( 144 ) / 'XYLNRXN         ' /
      DATA GAS_CHEM_SPC( 145 ) / 'XYLHRXN         ' /
      DATA GAS_CHEM_SPC( 146 ) / 'TOLNRXN         ' /
      DATA GAS_CHEM_SPC( 147 ) / 'TOLHRXN         ' /
      DATA GAS_CHEM_SPC( 148 ) / 'HCHO_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 149 ) / 'CCHO_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 150 ) / 'ACROLEIN_PRIMARY' /




! MAPPED_TO_CGRID declares whether CMAQ namelists were used to determine 
! the below values of CGRID_INDEX, SPECIES_TYPE, SPECIES_MOLWT, and CONVERT_CONC
      LOGICAL, PARAMETER, PRIVATE :: F = .FALSE.
      LOGICAL, PARAMETER, PRIVATE :: T = .TRUE.


      LOGICAL   :: MAPPED_TO_CGRID = .TRUE. 

      DATA CHEMISTRY_SPC(   1 ), CGRID_INDEX(   1 ), SPECIES_TYPE(   1 ), SPECIES_MOLWT(   1 ), CONVERT_CONC(   1 ) / 'NO2             ',    1, 'GC',   46.00, F /
      DATA CHEMISTRY_SPC(   2 ), CGRID_INDEX(   2 ), SPECIES_TYPE(   2 ), SPECIES_MOLWT(   2 ), CONVERT_CONC(   2 ) / 'NO              ',    2, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC(   3 ), CGRID_INDEX(   3 ), SPECIES_TYPE(   3 ), SPECIES_MOLWT(   3 ), CONVERT_CONC(   3 ) / 'O3P             ',    3, 'GC',   16.00, F /
      DATA CHEMISTRY_SPC(   4 ), CGRID_INDEX(   4 ), SPECIES_TYPE(   4 ), SPECIES_MOLWT(   4 ), CONVERT_CONC(   4 ) / 'O3              ',    4, 'GC',   48.00, F /
      DATA CHEMISTRY_SPC(   5 ), CGRID_INDEX(   5 ), SPECIES_TYPE(   5 ), SPECIES_MOLWT(   5 ), CONVERT_CONC(   5 ) / 'NO3             ',    5, 'GC',   62.00, F /
      DATA CHEMISTRY_SPC(   6 ), CGRID_INDEX(   6 ), SPECIES_TYPE(   6 ), SPECIES_MOLWT(   6 ), CONVERT_CONC(   6 ) / 'N2O5            ',    6, 'GC',  108.00, F /
      DATA CHEMISTRY_SPC(   7 ), CGRID_INDEX(   7 ), SPECIES_TYPE(   7 ), SPECIES_MOLWT(   7 ), CONVERT_CONC(   7 ) / 'HNO3            ',    7, 'GC',   63.00, F /
      DATA CHEMISTRY_SPC(   8 ), CGRID_INDEX(   8 ), SPECIES_TYPE(   8 ), SPECIES_MOLWT(   8 ), CONVERT_CONC(   8 ) / 'O1D             ',    8, 'GC',   16.00, F /
      DATA CHEMISTRY_SPC(   9 ), CGRID_INDEX(   9 ), SPECIES_TYPE(   9 ), SPECIES_MOLWT(   9 ), CONVERT_CONC(   9 ) / 'OH              ',    9, 'GC',   17.00, F /
      DATA CHEMISTRY_SPC(  10 ), CGRID_INDEX(  10 ), SPECIES_TYPE(  10 ), SPECIES_MOLWT(  10 ), CONVERT_CONC(  10 ) / 'HONO            ',   10, 'GC',   47.00, F /
      DATA CHEMISTRY_SPC(  11 ), CGRID_INDEX(  11 ), SPECIES_TYPE(  11 ), SPECIES_MOLWT(  11 ), CONVERT_CONC(  11 ) / 'HO2             ',   11, 'GC',   33.00, F /
      DATA CHEMISTRY_SPC(  12 ), CGRID_INDEX(  12 ), SPECIES_TYPE(  12 ), SPECIES_MOLWT(  12 ), CONVERT_CONC(  12 ) / 'CO              ',   12, 'GC',   28.00, F /
      DATA CHEMISTRY_SPC(  13 ), CGRID_INDEX(  13 ), SPECIES_TYPE(  13 ), SPECIES_MOLWT(  13 ), CONVERT_CONC(  13 ) / 'CO2             ',   13, 'GC',   44.00, F /
      DATA CHEMISTRY_SPC(  14 ), CGRID_INDEX(  14 ), SPECIES_TYPE(  14 ), SPECIES_MOLWT(  14 ), CONVERT_CONC(  14 ) / 'HNO4            ',   14, 'GC',   79.00, F /
      DATA CHEMISTRY_SPC(  15 ), CGRID_INDEX(  15 ), SPECIES_TYPE(  15 ), SPECIES_MOLWT(  15 ), CONVERT_CONC(  15 ) / 'HO2H            ',   15, 'GC',   34.00, F /
      DATA CHEMISTRY_SPC(  16 ), CGRID_INDEX(  16 ), SPECIES_TYPE(  16 ), SPECIES_MOLWT(  16 ), CONVERT_CONC(  16 ) / 'SO2             ',   16, 'GC',   64.10, F /
      DATA CHEMISTRY_SPC(  17 ), CGRID_INDEX(  17 ), SPECIES_TYPE(  17 ), SPECIES_MOLWT(  17 ), CONVERT_CONC(  17 ) / 'SULF            ',   17, 'GC',   98.10, F /
      DATA CHEMISTRY_SPC(  18 ), CGRID_INDEX(  18 ), SPECIES_TYPE(  18 ), SPECIES_MOLWT(  18 ), CONVERT_CONC(  18 ) / 'SULRXN          ',   18, 'GC',   98.10, F /
      DATA CHEMISTRY_SPC(  19 ), CGRID_INDEX(  19 ), SPECIES_TYPE(  19 ), SPECIES_MOLWT(  19 ), CONVERT_CONC(  19 ) / 'NO2EX           ',   19, 'GC',   44.00, F /
      DATA CHEMISTRY_SPC(  20 ), CGRID_INDEX(  20 ), SPECIES_TYPE(  20 ), SPECIES_MOLWT(  20 ), CONVERT_CONC(  20 ) / 'MEO2            ',   20, 'GC',   47.00, F /
      DATA CHEMISTRY_SPC(  21 ), CGRID_INDEX(  21 ), SPECIES_TYPE(  21 ), SPECIES_MOLWT(  21 ), CONVERT_CONC(  21 ) / 'HCHO            ',   21, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC(  22 ), CGRID_INDEX(  22 ), SPECIES_TYPE(  22 ), SPECIES_MOLWT(  22 ), CONVERT_CONC(  22 ) / 'COOH            ',   22, 'GC',   48.00, F /
      DATA CHEMISTRY_SPC(  23 ), CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), SPECIES_MOLWT(  23 ), CONVERT_CONC(  23 ) / 'MEOH            ',   23, 'GC',   32.00, F /
      DATA CHEMISTRY_SPC(  24 ), CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), SPECIES_MOLWT(  24 ), CONVERT_CONC(  24 ) / 'RO2C            ',   24, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  25 ), CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), SPECIES_MOLWT(  25 ), CONVERT_CONC(  25 ) / 'RO2XC           ',   25, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  26 ), CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), SPECIES_MOLWT(  26 ), CONVERT_CONC(  26 ) / 'MECO3           ',   26, 'GC',   75.00, F /
      DATA CHEMISTRY_SPC(  27 ), CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), SPECIES_MOLWT(  27 ), CONVERT_CONC(  27 ) / 'PAN             ',   27, 'GC',  121.10, F /
      DATA CHEMISTRY_SPC(  28 ), CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), SPECIES_MOLWT(  28 ), CONVERT_CONC(  28 ) / 'CCOOOH          ',   28, 'GC',   76.00, F /
      DATA CHEMISTRY_SPC(  29 ), CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), SPECIES_MOLWT(  29 ), CONVERT_CONC(  29 ) / 'CCOOH           ',   29, 'GC',   60.10, F /
      DATA CHEMISTRY_SPC(  30 ), CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), SPECIES_MOLWT(  30 ), CONVERT_CONC(  30 ) / 'RCO3            ',   30, 'GC',   89.10, F /
      DATA CHEMISTRY_SPC(  31 ), CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), SPECIES_MOLWT(  31 ), CONVERT_CONC(  31 ) / 'PAN2            ',   31, 'GC',  135.10, F /
      DATA CHEMISTRY_SPC(  32 ), CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), SPECIES_MOLWT(  32 ), CONVERT_CONC(  32 ) / 'xHO2            ',   32, 'GC',   33.00, F /
      DATA CHEMISTRY_SPC(  33 ), CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), SPECIES_MOLWT(  33 ), CONVERT_CONC(  33 ) / 'yROOH           ',   33, 'GC',   76.10, F /
      DATA CHEMISTRY_SPC(  34 ), CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), SPECIES_MOLWT(  34 ), CONVERT_CONC(  34 ) / 'xCCHO           ',   34, 'GC',   44.10, F /
      DATA CHEMISTRY_SPC(  35 ), CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), SPECIES_MOLWT(  35 ), CONVERT_CONC(  35 ) / 'RCOOOH          ',   35, 'GC',   74.10, F /
      DATA CHEMISTRY_SPC(  36 ), CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), SPECIES_MOLWT(  36 ), CONVERT_CONC(  36 ) / 'RCOOH           ',   36, 'GC',   74.10, F /
      DATA CHEMISTRY_SPC(  37 ), CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), SPECIES_MOLWT(  37 ), CONVERT_CONC(  37 ) / 'BZCO3           ',   37, 'GC',  137.10, F /
      DATA CHEMISTRY_SPC(  38 ), CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), SPECIES_MOLWT(  38 ), CONVERT_CONC(  38 ) / 'PBZN            ',   38, 'GC',  183.10, F /
      DATA CHEMISTRY_SPC(  39 ), CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), SPECIES_MOLWT(  39 ), CONVERT_CONC(  39 ) / 'BZO             ',   39, 'GC',   93.00, F /
      DATA CHEMISTRY_SPC(  40 ), CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), SPECIES_MOLWT(  40 ), CONVERT_CONC(  40 ) / 'MACO3           ',   40, 'GC',  101.10, F /
      DATA CHEMISTRY_SPC(  41 ), CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), SPECIES_MOLWT(  41 ), CONVERT_CONC(  41 ) / 'MAPAN           ',   41, 'GC',  147.10, F /
      DATA CHEMISTRY_SPC(  42 ), CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), SPECIES_MOLWT(  42 ), CONVERT_CONC(  42 ) / 'TBUO            ',   42, 'GC',   73.00, F /
      DATA CHEMISTRY_SPC(  43 ), CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), SPECIES_MOLWT(  43 ), CONVERT_CONC(  43 ) / 'RNO3            ',   43, 'GC',  147.20, F /
      DATA CHEMISTRY_SPC(  44 ), CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), SPECIES_MOLWT(  44 ), CONVERT_CONC(  44 ) / 'ACETONE         ',   44, 'GC',   58.10, F /
      DATA CHEMISTRY_SPC(  45 ), CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), SPECIES_MOLWT(  45 ), CONVERT_CONC(  45 ) / 'NPHE            ',   45, 'GC',  139.10, F /
      DATA CHEMISTRY_SPC(  46 ), CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), SPECIES_MOLWT(  46 ), CONVERT_CONC(  46 ) / 'CRES            ',   46, 'GC',  108.10, F /
      DATA CHEMISTRY_SPC(  47 ), CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), SPECIES_MOLWT(  47 ), CONVERT_CONC(  47 ) / 'xOH             ',   47, 'GC',   17.00, F /
      DATA CHEMISTRY_SPC(  48 ), CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), SPECIES_MOLWT(  48 ), CONVERT_CONC(  48 ) / 'xNO2            ',   48, 'GC',   46.00, F /
      DATA CHEMISTRY_SPC(  49 ), CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), SPECIES_MOLWT(  49 ), CONVERT_CONC(  49 ) / 'xMEO2           ',   49, 'GC',   47.00, F /
      DATA CHEMISTRY_SPC(  50 ), CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), SPECIES_MOLWT(  50 ), CONVERT_CONC(  50 ) / 'xMECO3          ',   50, 'GC',   75.00, F /
      DATA CHEMISTRY_SPC(  51 ), CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), SPECIES_MOLWT(  51 ), CONVERT_CONC(  51 ) / 'xRCO3           ',   51, 'GC',   89.10, F /
      DATA CHEMISTRY_SPC(  52 ), CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), SPECIES_MOLWT(  52 ), CONVERT_CONC(  52 ) / 'xMACO3          ',   52, 'GC',  101.10, F /
      DATA CHEMISTRY_SPC(  53 ), CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), SPECIES_MOLWT(  53 ), CONVERT_CONC(  53 ) / 'xTBUO           ',   53, 'GC',   73.00, F /
      DATA CHEMISTRY_SPC(  54 ), CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), SPECIES_MOLWT(  54 ), CONVERT_CONC(  54 ) / 'xCO             ',   54, 'GC',   28.00, F /
      DATA CHEMISTRY_SPC(  55 ), CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), SPECIES_MOLWT(  55 ), CONVERT_CONC(  55 ) / 'CCHO            ',   55, 'GC',   44.10, F /
      DATA CHEMISTRY_SPC(  56 ), CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), SPECIES_MOLWT(  56 ), CONVERT_CONC(  56 ) / 'RCHO            ',   56, 'GC',   58.10, F /
      DATA CHEMISTRY_SPC(  57 ), CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), SPECIES_MOLWT(  57 ), CONVERT_CONC(  57 ) / 'xHCHO           ',   57, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC(  58 ), CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), SPECIES_MOLWT(  58 ), CONVERT_CONC(  58 ) / 'MEK             ',   58, 'GC',   72.10, F /
      DATA CHEMISTRY_SPC(  59 ), CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), SPECIES_MOLWT(  59 ), CONVERT_CONC(  59 ) / 'zRNO3           ',   59, 'GC',  147.20, F /
      DATA CHEMISTRY_SPC(  60 ), CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), SPECIES_MOLWT(  60 ), CONVERT_CONC(  60 ) / 'xRCHO           ',   60, 'GC',   58.10, F /
      DATA CHEMISTRY_SPC(  61 ), CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), SPECIES_MOLWT(  61 ), CONVERT_CONC(  61 ) / 'HCOOH           ',   61, 'GC',   46.00, F /
      DATA CHEMISTRY_SPC(  62 ), CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), SPECIES_MOLWT(  62 ), CONVERT_CONC(  62 ) / 'xMGLY           ',   62, 'GC',   72.10, F /
      DATA CHEMISTRY_SPC(  63 ), CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), SPECIES_MOLWT(  63 ), CONVERT_CONC(  63 ) / 'xBACL           ',   63, 'GC',   86.10, F /
      DATA CHEMISTRY_SPC(  64 ), CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), SPECIES_MOLWT(  64 ), CONVERT_CONC(  64 ) / 'ROOH            ',   64, 'GC',   76.10, F /
      DATA CHEMISTRY_SPC(  65 ), CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), SPECIES_MOLWT(  65 ), CONVERT_CONC(  65 ) / 'xPROD2          ',   65, 'GC',  116.20, F /
      DATA CHEMISTRY_SPC(  66 ), CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), SPECIES_MOLWT(  66 ), CONVERT_CONC(  66 ) / 'R6OOH           ',   66, 'GC',  118.20, F /
      DATA CHEMISTRY_SPC(  67 ), CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), SPECIES_MOLWT(  67 ), CONVERT_CONC(  67 ) / 'PRD2            ',   67, 'GC',  116.20, F /
      DATA CHEMISTRY_SPC(  68 ), CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), SPECIES_MOLWT(  68 ), CONVERT_CONC(  68 ) / 'yR6OOH          ',   68, 'GC',  118.20, F /
      DATA CHEMISTRY_SPC(  69 ), CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), SPECIES_MOLWT(  69 ), CONVERT_CONC(  69 ) / 'RAOOH           ',   69, 'GC',  188.20, F /
      DATA CHEMISTRY_SPC(  70 ), CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), SPECIES_MOLWT(  70 ), CONVERT_CONC(  70 ) / 'MGLY            ',   70, 'GC',   72.10, F /
      DATA CHEMISTRY_SPC(  71 ), CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), SPECIES_MOLWT(  71 ), CONVERT_CONC(  71 ) / 'IPRD            ',   71, 'GC',  100.10, F /
      DATA CHEMISTRY_SPC(  72 ), CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), SPECIES_MOLWT(  72 ), CONVERT_CONC(  72 ) / 'xGLY            ',   72, 'GC',   58.00, F /
      DATA CHEMISTRY_SPC(  73 ), CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), SPECIES_MOLWT(  73 ), CONVERT_CONC(  73 ) / 'xMEK            ',   73, 'GC',   72.10, F /
      DATA CHEMISTRY_SPC(  74 ), CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), SPECIES_MOLWT(  74 ), CONVERT_CONC(  74 ) / 'xAFG1           ',   74, 'GC',   98.10, F /
      DATA CHEMISTRY_SPC(  75 ), CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), SPECIES_MOLWT(  75 ), CONVERT_CONC(  75 ) / 'xAFG2           ',   75, 'GC',   98.10, F /
      DATA CHEMISTRY_SPC(  76 ), CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), SPECIES_MOLWT(  76 ), CONVERT_CONC(  76 ) / 'GLY             ',   76, 'GC',   58.00, F /
      DATA CHEMISTRY_SPC(  77 ), CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), SPECIES_MOLWT(  77 ), CONVERT_CONC(  77 ) / 'AFG1            ',   77, 'GC',   98.10, F /
      DATA CHEMISTRY_SPC(  78 ), CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), SPECIES_MOLWT(  78 ), CONVERT_CONC(  78 ) / 'AFG2            ',   78, 'GC',   98.10, F /
      DATA CHEMISTRY_SPC(  79 ), CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), SPECIES_MOLWT(  79 ), CONVERT_CONC(  79 ) / 'BACL            ',   79, 'GC',   86.10, F /
      DATA CHEMISTRY_SPC(  80 ), CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), SPECIES_MOLWT(  80 ), CONVERT_CONC(  80 ) / 'BALD            ',   80, 'GC',  106.10, F /
      DATA CHEMISTRY_SPC(  81 ), CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), SPECIES_MOLWT(  81 ), CONVERT_CONC(  81 ) / 'AFG3            ',   81, 'GC',  124.10, F /
      DATA CHEMISTRY_SPC(  82 ), CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), SPECIES_MOLWT(  82 ), CONVERT_CONC(  82 ) / 'xIPRD           ',   82, 'GC',  100.10, F /
      DATA CHEMISTRY_SPC(  83 ), CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), SPECIES_MOLWT(  83 ), CONVERT_CONC(  83 ) / 'MACR            ',   83, 'GC',   70.10, F /
      DATA CHEMISTRY_SPC(  84 ), CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), SPECIES_MOLWT(  84 ), CONVERT_CONC(  84 ) / 'MVK             ',   84, 'GC',   70.10, F /
      DATA CHEMISTRY_SPC(  85 ), CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), SPECIES_MOLWT(  85 ), CONVERT_CONC(  85 ) / 'xHOCCHO         ',   85, 'GC',   60.10, F /
      DATA CHEMISTRY_SPC(  86 ), CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), SPECIES_MOLWT(  86 ), CONVERT_CONC(  86 ) / 'xRNO3           ',   86, 'GC',  147.20, F /
      DATA CHEMISTRY_SPC(  87 ), CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), SPECIES_MOLWT(  87 ), CONVERT_CONC(  87 ) / 'HOCCHO          ',   87, 'GC',   60.10, F /
      DATA CHEMISTRY_SPC(  88 ), CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), SPECIES_MOLWT(  88 ), CONVERT_CONC(  88 ) / 'xACETONE        ',   88, 'GC',   58.10, F /
      DATA CHEMISTRY_SPC(  89 ), CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), SPECIES_MOLWT(  89 ), CONVERT_CONC(  89 ) / 'ACROLEIN        ',   89, 'GC',   56.10, F /
      DATA CHEMISTRY_SPC(  90 ), CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), SPECIES_MOLWT(  90 ), CONVERT_CONC(  90 ) / 'xBALD           ',   90, 'GC',  106.10, F /
      DATA CHEMISTRY_SPC(  91 ), CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), SPECIES_MOLWT(  91 ), CONVERT_CONC(  91 ) / 'xAFG3           ',   91, 'GC',  124.70, F /
      DATA CHEMISTRY_SPC(  92 ), CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), SPECIES_MOLWT(  92 ), CONVERT_CONC(  92 ) / 'xMACR           ',   92, 'GC',   70.10, F /
      DATA CHEMISTRY_SPC(  93 ), CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), SPECIES_MOLWT(  93 ), CONVERT_CONC(  93 ) / 'xMVK            ',   93, 'GC',   70.10, F /
      DATA CHEMISTRY_SPC(  94 ), CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), SPECIES_MOLWT(  94 ), CONVERT_CONC(  94 ) / 'yRAOOH          ',   94, 'GC',  188.20, F /
      DATA CHEMISTRY_SPC(  95 ), CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), SPECIES_MOLWT(  95 ), CONVERT_CONC(  95 ) / 'xACROLEIN       ',   95, 'GC',   56.10, F /
      DATA CHEMISTRY_SPC(  96 ), CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), SPECIES_MOLWT(  96 ), CONVERT_CONC(  96 ) / 'ETHENE          ',   96, 'GC',   28.10, F /
      DATA CHEMISTRY_SPC(  97 ), CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), SPECIES_MOLWT(  97 ), CONVERT_CONC(  97 ) / 'PROPENE         ',   97, 'GC',   42.10, F /
      DATA CHEMISTRY_SPC(  98 ), CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), SPECIES_MOLWT(  98 ), CONVERT_CONC(  98 ) / 'BUTADIENE13     ',   98, 'GC',   54.10, F /
      DATA CHEMISTRY_SPC(  99 ), CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), SPECIES_MOLWT(  99 ), CONVERT_CONC(  99 ) / 'ISOPRENE        ',   99, 'GC',   68.10, F /
      DATA CHEMISTRY_SPC( 100 ), CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), SPECIES_MOLWT( 100 ), CONVERT_CONC( 100 ) / 'ISOPRXN         ',  100, 'GC',   68.00, F /
      DATA CHEMISTRY_SPC( 101 ), CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), SPECIES_MOLWT( 101 ), CONVERT_CONC( 101 ) / 'APIN            ',  101, 'GC',  136.20, F /
      DATA CHEMISTRY_SPC( 102 ), CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), SPECIES_MOLWT( 102 ), CONVERT_CONC( 102 ) / 'TRPRXN          ',  102, 'GC',  136.20, F /
      DATA CHEMISTRY_SPC( 103 ), CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), SPECIES_MOLWT( 103 ), CONVERT_CONC( 103 ) / 'ACETYLENE       ',  103, 'GC',   26.00, F /
      DATA CHEMISTRY_SPC( 104 ), CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), SPECIES_MOLWT( 104 ), CONVERT_CONC( 104 ) / 'BENZENE         ',  104, 'GC',   78.10, F /
      DATA CHEMISTRY_SPC( 105 ), CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), SPECIES_MOLWT( 105 ), CONVERT_CONC( 105 ) / 'BENZRO2         ',  105, 'GC',  159.10, F /
      DATA CHEMISTRY_SPC( 106 ), CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), SPECIES_MOLWT( 106 ), CONVERT_CONC( 106 ) / 'TOLUENE         ',  106, 'GC',   92.10, F /
      DATA CHEMISTRY_SPC( 107 ), CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), SPECIES_MOLWT( 107 ), CONVERT_CONC( 107 ) / 'TOLRO2          ',  107, 'GC',  172.10, F /
      DATA CHEMISTRY_SPC( 108 ), CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), SPECIES_MOLWT( 108 ), CONVERT_CONC( 108 ) / 'MXYL            ',  108, 'GC',  106.20, F /
      DATA CHEMISTRY_SPC( 109 ), CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), SPECIES_MOLWT( 109 ), CONVERT_CONC( 109 ) / 'XYLRO2          ',  109, 'GC',  187.20, F /
      DATA CHEMISTRY_SPC( 110 ), CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), SPECIES_MOLWT( 110 ), CONVERT_CONC( 110 ) / 'OXYL            ',  110, 'GC',  106.20, F /
      DATA CHEMISTRY_SPC( 111 ), CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), SPECIES_MOLWT( 111 ), CONVERT_CONC( 111 ) / 'PXYL            ',  111, 'GC',  106.20, F /
      DATA CHEMISTRY_SPC( 112 ), CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), SPECIES_MOLWT( 112 ), CONVERT_CONC( 112 ) / 'TRIMETH_BENZ124 ',  112, 'GC',  120.20, F /
      DATA CHEMISTRY_SPC( 113 ), CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), SPECIES_MOLWT( 113 ), CONVERT_CONC( 113 ) / 'ETOH            ',  113, 'GC',   46.10, F /
      DATA CHEMISTRY_SPC( 114 ), CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), SPECIES_MOLWT( 114 ), CONVERT_CONC( 114 ) / 'ALK1            ',  114, 'GC',   30.10, F /
      DATA CHEMISTRY_SPC( 115 ), CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), SPECIES_MOLWT( 115 ), CONVERT_CONC( 115 ) / 'ALK2            ',  115, 'GC',   36.70, F /
      DATA CHEMISTRY_SPC( 116 ), CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), SPECIES_MOLWT( 116 ), CONVERT_CONC( 116 ) / 'ALK3            ',  116, 'GC',   58.60, F /
      DATA CHEMISTRY_SPC( 117 ), CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), SPECIES_MOLWT( 117 ), CONVERT_CONC( 117 ) / 'ALK4            ',  117, 'GC',   77.60, F /
      DATA CHEMISTRY_SPC( 118 ), CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), SPECIES_MOLWT( 118 ), CONVERT_CONC( 118 ) / 'ALK5            ',  118, 'GC',  118.90, F /
      DATA CHEMISTRY_SPC( 119 ), CGRID_INDEX( 119 ), SPECIES_TYPE( 119 ), SPECIES_MOLWT( 119 ), CONVERT_CONC( 119 ) / 'ALK5RXN         ',  119, 'GC',  118.90, F /
      DATA CHEMISTRY_SPC( 120 ), CGRID_INDEX( 120 ), SPECIES_TYPE( 120 ), SPECIES_MOLWT( 120 ), CONVERT_CONC( 120 ) / 'OLE1            ',  120, 'GC',   72.30, F /
      DATA CHEMISTRY_SPC( 121 ), CGRID_INDEX( 121 ), SPECIES_TYPE( 121 ), SPECIES_MOLWT( 121 ), CONVERT_CONC( 121 ) / 'OLE2            ',  121, 'GC',   75.80, F /
      DATA CHEMISTRY_SPC( 122 ), CGRID_INDEX( 122 ), SPECIES_TYPE( 122 ), SPECIES_MOLWT( 122 ), CONVERT_CONC( 122 ) / 'ARO1            ',  122, 'GC',   95.20, F /
      DATA CHEMISTRY_SPC( 123 ), CGRID_INDEX( 123 ), SPECIES_TYPE( 123 ), SPECIES_MOLWT( 123 ), CONVERT_CONC( 123 ) / 'ARO2            ',  123, 'GC',  118.70, F /
      DATA CHEMISTRY_SPC( 124 ), CGRID_INDEX( 124 ), SPECIES_TYPE( 124 ), SPECIES_MOLWT( 124 ), CONVERT_CONC( 124 ) / 'TERP            ',  124, 'GC',  136.20, F /
      DATA CHEMISTRY_SPC( 125 ), CGRID_INDEX( 125 ), SPECIES_TYPE( 125 ), SPECIES_MOLWT( 125 ), CONVERT_CONC( 125 ) / 'SESQ            ',  125, 'GC',  204.40, F /
      DATA CHEMISTRY_SPC( 126 ), CGRID_INDEX( 126 ), SPECIES_TYPE( 126 ), SPECIES_MOLWT( 126 ), CONVERT_CONC( 126 ) / 'SESQRXN         ',  126, 'GC',  204.40, F /
      DATA CHEMISTRY_SPC( 127 ), CGRID_INDEX( 127 ), SPECIES_TYPE( 127 ), SPECIES_MOLWT( 127 ), CONVERT_CONC( 127 ) / 'CL2             ',  127, 'GC',   70.00, F /
      DATA CHEMISTRY_SPC( 128 ), CGRID_INDEX( 128 ), SPECIES_TYPE( 128 ), SPECIES_MOLWT( 128 ), CONVERT_CONC( 128 ) / 'CL              ',  128, 'GC',   35.50, F /
      DATA CHEMISTRY_SPC( 129 ), CGRID_INDEX( 129 ), SPECIES_TYPE( 129 ), SPECIES_MOLWT( 129 ), CONVERT_CONC( 129 ) / 'CLNO            ',  129, 'GC',   65.50, F /
      DATA CHEMISTRY_SPC( 130 ), CGRID_INDEX( 130 ), SPECIES_TYPE( 130 ), SPECIES_MOLWT( 130 ), CONVERT_CONC( 130 ) / 'CLONO           ',  130, 'GC',   81.50, F /
      DATA CHEMISTRY_SPC( 131 ), CGRID_INDEX( 131 ), SPECIES_TYPE( 131 ), SPECIES_MOLWT( 131 ), CONVERT_CONC( 131 ) / 'CLNO2           ',  131, 'GC',   81.50, F /
      DATA CHEMISTRY_SPC( 132 ), CGRID_INDEX( 132 ), SPECIES_TYPE( 132 ), SPECIES_MOLWT( 132 ), CONVERT_CONC( 132 ) / 'HCL             ',  132, 'GC',   36.50, F /
      DATA CHEMISTRY_SPC( 133 ), CGRID_INDEX( 133 ), SPECIES_TYPE( 133 ), SPECIES_MOLWT( 133 ), CONVERT_CONC( 133 ) / 'CLO             ',  133, 'GC',   51.50, F /
      DATA CHEMISTRY_SPC( 134 ), CGRID_INDEX( 134 ), SPECIES_TYPE( 134 ), SPECIES_MOLWT( 134 ), CONVERT_CONC( 134 ) / 'CLONO2          ',  134, 'GC',   97.50, F /
      DATA CHEMISTRY_SPC( 135 ), CGRID_INDEX( 135 ), SPECIES_TYPE( 135 ), SPECIES_MOLWT( 135 ), CONVERT_CONC( 135 ) / 'HOCL            ',  135, 'GC',   52.50, F /
      DATA CHEMISTRY_SPC( 136 ), CGRID_INDEX( 136 ), SPECIES_TYPE( 136 ), SPECIES_MOLWT( 136 ), CONVERT_CONC( 136 ) / 'xCL             ',  136, 'GC',   35.50, F /
      DATA CHEMISTRY_SPC( 137 ), CGRID_INDEX( 137 ), SPECIES_TYPE( 137 ), SPECIES_MOLWT( 137 ), CONVERT_CONC( 137 ) / 'xCLCCHO         ',  137, 'GC',   78.50, F /
      DATA CHEMISTRY_SPC( 138 ), CGRID_INDEX( 138 ), SPECIES_TYPE( 138 ), SPECIES_MOLWT( 138 ), CONVERT_CONC( 138 ) / 'xCLACET         ',  138, 'GC',   92.50, F /
      DATA CHEMISTRY_SPC( 139 ), CGRID_INDEX( 139 ), SPECIES_TYPE( 139 ), SPECIES_MOLWT( 139 ), CONVERT_CONC( 139 ) / 'CLCCHO          ',  139, 'GC',   78.50, F /
      DATA CHEMISTRY_SPC( 140 ), CGRID_INDEX( 140 ), SPECIES_TYPE( 140 ), SPECIES_MOLWT( 140 ), CONVERT_CONC( 140 ) / 'CLACET          ',  140, 'GC',   92.50, F /
      DATA CHEMISTRY_SPC( 141 ), CGRID_INDEX( 141 ), SPECIES_TYPE( 141 ), SPECIES_MOLWT( 141 ), CONVERT_CONC( 141 ) / 'CLCHO           ',  141, 'GC',   64.50, F /
      DATA CHEMISTRY_SPC( 142 ), CGRID_INDEX( 142 ), SPECIES_TYPE( 142 ), SPECIES_MOLWT( 142 ), CONVERT_CONC( 142 ) / 'BNZNRXN         ',  142, 'GC',  159.10, F /
      DATA CHEMISTRY_SPC( 143 ), CGRID_INDEX( 143 ), SPECIES_TYPE( 143 ), SPECIES_MOLWT( 143 ), CONVERT_CONC( 143 ) / 'BNZHRXN         ',  143, 'GC',  159.10, F /
      DATA CHEMISTRY_SPC( 144 ), CGRID_INDEX( 144 ), SPECIES_TYPE( 144 ), SPECIES_MOLWT( 144 ), CONVERT_CONC( 144 ) / 'XYLNRXN         ',  144, 'GC',  187.20, F /
      DATA CHEMISTRY_SPC( 145 ), CGRID_INDEX( 145 ), SPECIES_TYPE( 145 ), SPECIES_MOLWT( 145 ), CONVERT_CONC( 145 ) / 'XYLHRXN         ',  145, 'GC',  187.20, F /
      DATA CHEMISTRY_SPC( 146 ), CGRID_INDEX( 146 ), SPECIES_TYPE( 146 ), SPECIES_MOLWT( 146 ), CONVERT_CONC( 146 ) / 'TOLNRXN         ',  146, 'GC',  172.10, F /
      DATA CHEMISTRY_SPC( 147 ), CGRID_INDEX( 147 ), SPECIES_TYPE( 147 ), SPECIES_MOLWT( 147 ), CONVERT_CONC( 147 ) / 'TOLHRXN         ',  147, 'GC',  172.10, F /
      DATA CHEMISTRY_SPC( 148 ), CGRID_INDEX( 148 ), SPECIES_TYPE( 148 ), SPECIES_MOLWT( 148 ), CONVERT_CONC( 148 ) / 'HCHO_PRIMARY    ',  148, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC( 149 ), CGRID_INDEX( 149 ), SPECIES_TYPE( 149 ), SPECIES_MOLWT( 149 ), CONVERT_CONC( 149 ) / 'CCHO_PRIMARY    ',  149, 'GC',   44.10, F /
      DATA CHEMISTRY_SPC( 150 ), CGRID_INDEX( 150 ), SPECIES_TYPE( 150 ), SPECIES_MOLWT( 150 ), CONVERT_CONC( 150 ) / 'ACROLEIN_PRIMARY',  150, 'GC',   56.10, F /
      DATA CHEMISTRY_SPC( 151 ), CGRID_INDEX( 151 ), SPECIES_TYPE( 151 ), SPECIES_MOLWT( 151 ), CONVERT_CONC( 151 ) / 'AALKJ           ',  158, 'AE',  150.00, T /
      DATA CHEMISTRY_SPC( 152 ), CGRID_INDEX( 152 ), SPECIES_TYPE( 152 ), SPECIES_MOLWT( 152 ), CONVERT_CONC( 152 ) / 'AOLGAJ          ',  211, 'AE',  176.40, T /
      DATA CHEMISTRY_SPC( 153 ), CGRID_INDEX( 153 ), SPECIES_TYPE( 153 ), SPECIES_MOLWT( 153 ), CONVERT_CONC( 153 ) / 'AXYL1J          ',  159, 'AE',  192.00, T /
      DATA CHEMISTRY_SPC( 154 ), CGRID_INDEX( 154 ), SPECIES_TYPE( 154 ), SPECIES_MOLWT( 154 ), CONVERT_CONC( 154 ) / 'AXYL2J          ',  160, 'AE',  192.00, T /
      DATA CHEMISTRY_SPC( 155 ), CGRID_INDEX( 155 ), SPECIES_TYPE( 155 ), SPECIES_MOLWT( 155 ), CONVERT_CONC( 155 ) / 'ATOL1J          ',  162, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 156 ), CGRID_INDEX( 156 ), SPECIES_TYPE( 156 ), SPECIES_MOLWT( 156 ), CONVERT_CONC( 156 ) / 'ATOL2J          ',  163, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 157 ), CGRID_INDEX( 157 ), SPECIES_TYPE( 157 ), SPECIES_MOLWT( 157 ), CONVERT_CONC( 157 ) / 'ABNZ1J          ',  165, 'AE',  144.00, T /
      DATA CHEMISTRY_SPC( 158 ), CGRID_INDEX( 158 ), SPECIES_TYPE( 158 ), SPECIES_MOLWT( 158 ), CONVERT_CONC( 158 ) / 'ABNZ2J          ',  166, 'AE',  144.00, T /
      DATA CHEMISTRY_SPC( 159 ), CGRID_INDEX( 159 ), SPECIES_TYPE( 159 ), SPECIES_MOLWT( 159 ), CONVERT_CONC( 159 ) / 'ATRP1J          ',  168, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 160 ), CGRID_INDEX( 160 ), SPECIES_TYPE( 160 ), SPECIES_MOLWT( 160 ), CONVERT_CONC( 160 ) / 'AOLGBJ          ',  212, 'AE',  252.00, T /
      DATA CHEMISTRY_SPC( 161 ), CGRID_INDEX( 161 ), SPECIES_TYPE( 161 ), SPECIES_MOLWT( 161 ), CONVERT_CONC( 161 ) / 'ATRP2J          ',  169, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 162 ), CGRID_INDEX( 162 ), SPECIES_TYPE( 162 ), SPECIES_MOLWT( 162 ), CONVERT_CONC( 162 ) / 'AISO1J          ',  170, 'AE',   96.00, T /
      DATA CHEMISTRY_SPC( 163 ), CGRID_INDEX( 163 ), SPECIES_TYPE( 163 ), SPECIES_MOLWT( 163 ), CONVERT_CONC( 163 ) / 'AISO2J          ',  171, 'AE',   96.00, T /
      DATA CHEMISTRY_SPC( 164 ), CGRID_INDEX( 164 ), SPECIES_TYPE( 164 ), SPECIES_MOLWT( 164 ), CONVERT_CONC( 164 ) / 'ASQTJ           ',  172, 'AE',  378.00, T /
      DATA CHEMISTRY_SPC( 165 ), CGRID_INDEX( 165 ), SPECIES_TYPE( 165 ), SPECIES_MOLWT( 165 ), CONVERT_CONC( 165 ) / 'APOCI           ',  175, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 166 ), CGRID_INDEX( 166 ), SPECIES_TYPE( 166 ), SPECIES_MOLWT( 166 ), CONVERT_CONC( 166 ) / 'APNCOMI         ',  177, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 167 ), CGRID_INDEX( 167 ), SPECIES_TYPE( 167 ), SPECIES_MOLWT( 167 ), CONVERT_CONC( 167 ) / 'APOCJ           ',  174, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 168 ), CGRID_INDEX( 168 ), SPECIES_TYPE( 168 ), SPECIES_MOLWT( 168 ), CONVERT_CONC( 168 ) / 'APNCOMJ         ',  176, 'AE',  220.00, T /

      INTEGER, PARAMETER :: N_ACT_SP = 168

      INTEGER, PARAMETER :: NRXNS = 431

      INTEGER            :: KUNITS

      DATA  KUNITS /   2 /

      INTEGER IRXXN

      REAL( 8 )          :: RTDAT( 3,NRXNS )

      INTEGER, PARAMETER :: NFALLOFF =  23
      REAL( 8 )          :: RFDAT( 5,NFALLOFF )

      INTEGER            :: KTYPE( NRXNS )

      DATA ( KTYPE( IRXXN ), IRXXN = 1, NRXNS ) /  & 
     &      0,    2,    3,   10,    3,   10,    3,    3,    3,    3, & ! O   
     &     10,   10,    1,    1,    3,    0,    0,    0,    0,    3, & ! 1   
     &      3,   10,    0,    3,   10,    1,    8,    0,    9,    3, & ! 2   
     &      3,   10,   10,    0,    3,    4,    9,    9,    1,    3, & ! 3   
     &      0,    1,    3,   10,    3,    0,    1,    1,    1,    3, & ! 4   
     &      4,    4,    1,    4,    3,    3,    3,    1,    1,    1, & ! 5   
     &      6,    6,    6,    6,    6,    6,   10,   10,    0,    3, & ! 6   
     &      3,    6,    3,    3,    6,    3,    4,    3,    0,    3, & ! 7   
     &      6,    6,    6,    6,    6,    6,    6,    1,    3,    0, & ! 8   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 9   
     &      3,    0,    6,    6,    6,    6,    6,    6,    6,    6, & ! O   
     &      6,    6,    1,    3,    3,    6,    1,   11,   11,   11, & ! 1   
     &     11,   11,   11,   11,   11,   11,   11,   11,   11,   11, & ! 2   
     &     11,   11,   11,   11,   11,    0,    0,    3,    3,    3, & ! 3   
     &      0,    3,    3,    0,    3,    4,    0,    4,    0,    3, & ! 4   
     &      1,    3,    1,    3,    0,    1,    0,    1,    0,    1, & ! 5   
     &      0,    0,    0,    1,    3,    0,    1,    3,    0,    3, & ! 6   
     &      1,    1,    0,    0,    1,    0,    3,    1,    1,    0, & ! 7   
     &      1,    1,    0,    1,    1,    3,    3,    3,    1,    0, & ! 8   
     &      3,    3,    1,    0,    1,    1,    1,    0,    1,    0, & ! 9   
     &      1,    0,    6,    0,    6,    1,    3,    1,    1,    0, & ! O   
     &      1,    0,    1,    0,   11,   11,   11,   11,   11,   11, & ! 1   
     &     11,   11,   11,   11,   11,   11,   11,   11,   11,   11, & ! 2   
     &     11,   11,   11,   11,   11,   11,   11,   11,   11,   11, & ! 3   
     &     11,   11,   11,   11,   11,   11,   11,   11,   11,   11, & ! 4   
     &     11,   11,   11,   11,   11,   11,   11,   11,   11,   11, & ! 5   
     &     11,   11,   11,   11,    3,   10,    3,    4,    3,    3, & ! 6   
     &      3,    3,    3,    3,    3,    1,    3,    3,    3,    3, & ! 7   
     &      1,    3,    3,    3,    1,   10,    3,    3,    3,    1, & ! 8   
     &      1,    1,    1,    4,    4,    4,    3,    3,    3,    3, & ! 9   
     &      3,    3,    3,    3,    3,    3,    3,    1,    1,    3, & ! O   
     &      3,    3,    1,    6,    6,    6,    6,    0,    2,    0, & ! 1   
     &     10,   10,    0,    0,    2,    2,    3,    1,    3,   10, & ! 2   
     &      0,    0,   10,    3,    3,    0,    3,    3,    3,    3, & ! 3   
     &      1,    1,    1,    3,    1,    1,    1,    3,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    0,    1, & ! 5   
     &      1,    0,   11,   11,   11,   11,   11,   11,    3,   10, & ! 6   
     &      1,    1,    1,    1,   10,    1,    1,    1,    1,    1, & ! 7   
     &      3,    3,    3,    1,    1,    1,    1,    1,    1,    1, & ! 8   
     &      1,    6,    6,    6,    6,    6,    6,    6,    0,    0, & ! 9   
     &      3,    3,    3,    3,    0,    3,    1,    1,    3,    1, & ! O   
     &      1,    0,    1,   -1,   -1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,   -1,    1, & ! 2   
     &     -1/     !  3   

      INTEGER            :: IRXBITS( NRXNS )

      DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,   20,    0,    1,    0,    1,    0,    0,    0,   16, & ! O   
     &      1,    1,    8,    8,    0,    2,    2,    2,    2,    8, & ! 1   
     &      4,    1,    2,    0,    1,    0,    0,    2,    0,    0, & ! 2   
     &      0,    1,    1,    2,    0,    0,    0,    8,    0,    0, & ! 3   
     &      2,    0,    0,    1,  128,    2,    4,    8,    8,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    1,    1,    2,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    2,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    2, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    2,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    2,    2,    0,    0,    0, & ! 3   
     &      2,    0,    0,    2,    0,    0,    2,    0,    2,    0, & ! 4   
     &      0,    0,    0,    0,    2,    0,    2,    0,    2,    0, & ! 5   
     &      2,    2,    2,    0,    0,    2,    0,    0,    2,    0, & ! 6   
     &      0,    0,    2,    2,    0,    2,    0,    0,    0,    2, & ! 7   
     &      0,    0,    2,    0,    0,    0,    0,    0,    0,    2, & ! 8   
     &      0,    0,    0,    2,    0,    0,    0,    2,    0,    2, & ! 9   
     &      0,    2,    0,    2,    0,    0,    0,    0,    0,    2, & ! O   
     &      0,    2,    0,    2,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,   64,    1,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    1,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    2,    4,    2, & ! 1   
     &      1,    1,    2,    2,    0,    0,    0,    0,    0,    1, & ! 2   
     &      2,    2,    1,    0,    0,    2,    0,    0,  128,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    2,    0, & ! 5   
     &      0,    2,    0,    0,    0,    0,    0,    0,   64,    1, & ! 6   
     &      0,    0,    0,    0,    1,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    2,    2, & ! 9   
     &      0,    0,    0,    0,    2,    0,    0,    0,    0,    0, & ! O   
     &      0,    2,    0,    1,    1,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    1,    0, & ! 2   
     &      1/     !  3   

      INTEGER            :: IORDER( NRXNS )

      DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    2,    2,    2,    2,    2,    2,    2,    3, & ! O   
     &      2,    1,    2,    3,    2,    1,    1,    1,    1,    2, & ! 1   
     &      2,    2,    1,    2,    2,    2,    2,    1,    2,    2, & ! 2   
     &      2,    2,    1,    1,    2,    2,    2,    3,    2,    2, & ! 3   
     &      1,    2,    2,    2,    2,    1,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    1,    1,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    1,    1,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    1, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      1,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    1,    2,    2,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    2,    2,    2, & ! 3   
     &      1,    2,    2,    1,    2,    2,    1,    2,    1,    2, & ! 4   
     &      2,    2,    2,    2,    1,    2,    1,    2,    1,    2, & ! 5   
     &      1,    1,    1,    2,    2,    1,    2,    2,    1,    2, & ! 6   
     &      2,    2,    1,    1,    2,    1,    2,    2,    2,    1, & ! 7   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    1, & ! 8   
     &      2,    2,    2,    1,    2,    2,    2,    1,    2,    1, & ! 9   
     &      2,    1,    2,    1,    2,    2,    2,    2,    2,    1, & ! O   
     &      2,    1,    2,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    1,    3,    1, & ! 1   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    2, & ! 2   
     &      1,    1,    1,    2,    2,    1,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    2, & ! 5   
     &      2,    1,    1,    1,    1,    1,    1,    1,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    1, & ! 9   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! O   
     &      2,    1,    2,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    2,    2,    2, & ! 2   
     &      2/     !  3   

      INTEGER, PARAMETER :: KTN1 = 108
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     13,   14,   26,   39,   42,   47,   48,   49,   53,   58, & ! O   
     &     59,   60,   88,  113,  117,  151,  153,  156,  158,  160, & ! 1   
     &    164,  167,  171,  172,  175,  178,  179,  181,  182,  184, & ! 2   
     &    185,  189,  193,  195,  196,  197,  199,  201,  206,  208, & ! 3   
     &    209,  211,  213,  276,  281,  285,  290,  291,  292,  293, & ! 4   
     &    308,  309,  313,  328,  341,  342,  343,  345,  346,  347, & ! 5   
     &    349,  350,  351,  352,  353,  354,  355,  356,  357,  358, & ! 6   
     &    360,  361,  371,  372,  373,  374,  376,  377,  378,  379, & ! 7   
     &    380,  384,  385,  386,  387,  388,  389,  390,  391,  407, & ! 8   
     &    408,  410,  411,  413,  416,  417,  418,  419,  420,  421, & ! 9   
     &    422,  423,  424,  425,  426,  427,  428,  430/     !  O   

      INTEGER, PARAMETER :: KTN2 =   4
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &      2,  319,  325,  326/

      INTEGER, PARAMETER :: KTN3 = 103
      INTEGER            :: KRX3( KTN3 )

      DATA ( KRX3( IRXXN ), IRXXN = 1, KTN3 ) / & 
     &      3,    5,    7,    8,    9,   10,   15,   20,   21,   24, & ! O   
     &     30,   31,   35,   40,   43,   45,   50,   55,   56,   57, & ! 1   
     &     70,   71,   73,   74,   76,   78,   80,   89,  101,  114, & ! 2   
     &    115,  138,  139,  140,  142,  143,  145,  150,  152,  154, & ! 3   
     &    165,  168,  170,  177,  186,  187,  188,  191,  192,  207, & ! 4   
     &    265,  267,  269,  270,  271,  272,  273,  274,  275,  277, & ! 5   
     &    278,  279,  280,  282,  283,  284,  287,  288,  289,  297, & ! 6   
     &    298,  299,  300,  301,  302,  303,  304,  305,  306,  307, & ! 7   
     &    310,  311,  312,  327,  329,  334,  335,  337,  338,  339, & ! 8   
     &    340,  344,  348,  369,  381,  382,  383,  401,  402,  403, & ! 9   
     &    404,  406,  409/     !  O   

      INTEGER, PARAMETER :: KTN4 =  11
      INTEGER            :: KRX4( KTN4 )

      DATA ( KRX4( IRXXN ), IRXXN = 1, KTN4 ) / & 
     &     36,   51,   52,   54,   77,  146,  148,  268,  294,  295, & 
     &    296/

      INTEGER, PARAMETER :: KTN5 =   0
      INTEGER            :: KRX5( 1 )

      DATA   KRX5( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN6 =  49
      INTEGER            :: KRX6( KTN6 )

      DATA ( KRX6( IRXXN ), IRXXN = 1, KTN6 ) / & 
     &     61,   62,   63,   64,   65,   66,   72,   75,   81,   82, & 
     &     83,   84,   85,   86,   87,   91,   92,   93,   94,   95, & 
     &     96,   97,   98,   99,  100,  103,  104,  105,  106,  107, & 
     &    108,  109,  110,  111,  112,  116,  203,  205,  314,  315, & 
     &    316,  317,  392,  393,  394,  395,  396,  397,  398/

      INTEGER, PARAMETER :: KTN7 =   0
      INTEGER            :: KRX7( 1 )

      DATA   KRX7( 1 ) / 0 /

      INTEGER, PARAMETER :: NWM =   4
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &      2,   21,   47,  319/
      REAL,    PARAMETER :: ATM_AIR = 1.00000E+06

      INTEGER, PARAMETER :: NWW =   7
      INTEGER            :: NRXWW( NWW )

      DATA ( NRXWW( IRXXN ), IRXXN = 1, NWW ) / & 
     &     13,   14,   14,   20,   38,   48,   49/

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
     &    265,  369/
      REAL,    PARAMETER :: ATM_CH4 = 1.85000E+00

      INTEGER, PARAMETER :: NWH2 =   2
      INTEGER            :: NRXWH2( NWH2 )

      DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) / & 
     &     45,  339/
      REAL,    PARAMETER :: ATM_H2 = 5.60000E-01

      INTEGER, PARAMETER :: MXPRD =  26
      INTEGER            :: IRR( NRXNS,MXPRD+3 )

      DATA ( IRR( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    3,    3,    3,    3,    4,    4,    2,    2, & ! O   
     &      1,    6,    6,    6,    1,    5,    5,    4,    4,    8, & ! 1   
     &      8,    9,   10,    9,    9,    9,    9,    7,    9,    9, & ! 2   
     &     11,   11,   14,   14,   14,   11,   11,   11,    5,    5, & ! 3   
     &     15,   15,    9,    9,    9,    1,   19,   19,   19,   20, & ! 4   
     &     20,   20,   20,   20,   20,   24,   24,   24,   24,   24, & ! 5   
     &     25,   25,   25,   25,   25,   25,   26,   27,   27,   26, & ! 6   
     &     26,   26,   26,   26,   26,   26,   30,   31,   31,   30, & ! 7   
     &     30,   30,   30,   30,   30,   30,   30,   37,   38,   38, & ! 8   
     &     37,   37,   37,   37,   37,   37,   37,   37,   37,   40, & ! 9   
     &     41,   41,   40,   40,   40,   40,   40,   40,   40,   40, & ! O   
     &     40,   40,   42,   42,   39,   39,   39,   32,   32,   47, & ! 1   
     &     47,   48,   48,   49,   49,   50,   50,   51,   51,   52, & ! 2   
     &     52,   53,   53,   54,   54,   21,   21,   21,   21,   55, & ! 3   
     &     55,   55,   56,   56,   56,   44,   44,   58,   58,   23, & ! 4   
     &     61,   29,   36,   22,   22,   64,   64,   66,   66,   69, & ! 5   
     &     69,   76,   76,   76,   76,   70,   70,   70,   79,   46, & ! 6   
     &     46,   45,   45,   45,   80,   80,   80,   77,   77,   77, & ! 7   
     &     78,   78,   78,   81,   81,   83,   83,   83,   83,   83, & ! 8   
     &     84,   84,   84,   84,   71,   71,   71,   71,   67,   67, & ! 9   
     &     43,   43,   87,   87,   87,   89,   89,   89,   89,   89, & ! O   
     &     28,   28,   35,   35,   57,   57,   34,   34,   60,   60, & ! 1   
     &     88,   88,   73,   73,   65,   65,   72,   72,   62,   62, & ! 2   
     &     63,   63,   90,   90,   74,   74,   75,   75,   91,   91, & ! 3   
     &     92,   92,   93,   93,   82,   82,   86,   86,   59,   59, & ! 4   
     &     59,   33,   33,   33,   68,   68,   68,   94,   94,   94, & ! 5   
     &     85,   85,   95,   95,    9,   96,   96,   96,   96,   97, & ! 6   
     &     97,   97,   97,   98,   98,   98,   98,   99,   99,   99, & ! 7   
     &     99,  101,  101,  101,  101,  103,  103,  104,  106,  108, & ! 8   
     &    110,  111,  112,  113,  114,  115,  116,  117,  118,  120, & ! 9   
     &    120,  120,  120,  121,  121,  121,  121,  122,  123,  124, & ! O   
     &    124,  124,  124,  125,  125,  125,  125,  127,  128,  129, & ! 1   
     &    128,  128,  130,  131,  128,  128,  128,  128,  133,  133, & ! 2   
     &    134,  134,  134,  128,  133,  135,  133,    9,  128,   21, & ! 3   
     &     55,   23,   56,   44,   58,   43,   67,   76,   70,   46, & ! 4   
     &     80,   64,   66,   69,   89,   83,   84,   71,  139,  139, & ! 5   
     &    139,  140,  136,  136,  137,  137,  138,  138,  128,   96, & ! 6   
     &     97,   98,   99,  101,  103,  106,  108,  110,  111,  112, & ! 7   
     &    113,  114,  115,  116,  117,  118,  120,  121,  122,  123, & ! 8   
     &    124,  125,  105,  105,  109,  109,  107,  107,  148,  148, & ! 9   
     &    148,  148,  148,  149,  149,  149,  149,  150,  150,  150, & ! O   
     &    150,  150,  150,    6,    1,  151,  153,  154,  155,  156, & ! 1   
     &    157,  158,  159,  161,  162,  163,  164,  165,  166,  167, & ! 2   
     &    168/     !  3   

      DATA ( IRR( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    4,    2,    1,    1,    2,    1,    5,    2, & ! O   
     &      5,    0,    0,    0,    5,    0,    0,    0,    0,    0, & ! 1   
     &      0,    2,    0,   10,    1,    5,    7,    0,   12,    4, & ! 2   
     &      2,    1,    0,    0,    9,    4,   11,   11,   11,    5, & ! 3   
     &      0,    9,   11,   16,    0,    0,    0,    0,    0,    2, & ! 4   
     &     11,   11,    5,   20,   20,    2,   11,    5,   20,   24, & ! 5   
     &      2,   11,    5,   20,   24,   25,    1,    0,    0,    2, & ! 6   
     &     11,    5,   20,   24,   25,   26,    1,    0,    0,    2, & ! 7   
     &     11,    5,   20,   24,   25,   26,   30,    1,    0,    0, & ! 8   
     &      2,   11,    5,   20,   24,   25,   26,   30,   37,    1, & ! 9   
     &      0,    0,    2,   11,    5,   20,   24,   25,   26,   30, & ! O   
     &     37,   40,    1,    0,    1,   11,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    9,    5,    9, & ! 3   
     &      0,    5,    9,    0,    5,    9,    0,    9,    0,    9, & ! 4   
     &      9,    9,    9,    9,    0,    9,    0,    9,    0,    9, & ! 5   
     &      0,    0,    0,    9,    5,    0,    9,    5,    0,    9, & ! 6   
     &      5,    9,    0,    0,    9,    0,    5,    9,    4,    0, & ! 7   
     &      9,    4,    0,    9,    4,    9,    4,    5,    3,    0, & ! 8   
     &      9,    4,    3,    0,    9,    4,    5,    0,    9,    0, & ! 9   
     &      9,    0,    9,    0,    5,    9,    4,    5,    3,    0, & ! O   
     &      9,    0,    9,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    9,    4,    5,    3,    9, & ! 6   
     &      4,    5,    3,    9,    4,    5,    3,    9,    4,    5, & ! 7   
     &      3,    9,    4,    5,    3,    9,    4,    9,    9,    9, & ! 8   
     &      9,    9,    9,    9,    9,    9,    9,    9,    9,    9, & ! 9   
     &      4,    5,    3,    9,    4,    5,    3,    9,    9,    9, & ! O   
     &      4,    5,    3,    9,    4,    5,    3,    0,    2,    0, & ! 1   
     &      1,    1,    0,    0,   11,   11,    4,    5,    2,    1, & ! 2   
     &      0,    0,    0,  134,   11,    0,  133,  132,    0,  128, & ! 3   
     &    128,  128,  128,  128,  128,  128,  128,  128,  128,  128, & ! 4   
     &    128,  128,  128,  128,  128,  128,  128,  128,    0,    9, & ! 5   
     &    128,    0,    0,    0,    0,    0,    0,    0,    0,  128, & ! 6   
     &    128,  128,  128,  128,  128,  128,  128,  128,  128,  128, & ! 7   
     &    128,  128,  128,  128,  128,  128,  128,  128,  128,  128, & ! 8   
     &    128,  128,    2,   11,    2,   11,    2,   11,    0,    0, & ! 9   
     &      9,    5,  128,    9,    0,    5,  128,    9,    4,    5, & ! O   
     &      3,    0,  128,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    9,    9,    9, & ! 2   
     &      9/     !  3   

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
     &      0/     !  3   

      DATA ( IRR( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &      2,    4,    0,    1,    2,    5,    1,    5,    1,    1, & ! O   
     &      6,    1,    7,    7,    2,    2,    1,    8,    3,    9, & ! 1   
     &      3,   10,    9,    1,    7,   11,    5,    9,   11,   11, & ! 2   
     &      9,   14,   11,   11,    1,    9,   15,   15,    9,    1, & ! 3   
     &      9,   11,    0,   11,   11,   19,    1,    1,    9,    1, & ! 4   
     &     22,   21,   21,   23,   21,    1,    0,    1,   11,    0, & ! 5   
     &      0,    0,    1,   11,    0,    0,   27,   26,   26,   20, & ! 6   
     &     28,   20,   29,   20,   20,   20,   31,   30,   30,    1, & ! 7   
     &     35,    1,   21,   24,   24,   13,   24,   38,   37,   37, & ! 8   
     &      1,   35,    1,   21,   24,   24,   13,   13,   39,   41, & ! 9   
     &     40,   40,    1,   35,    1,   21,   13,   13,   13,   21, & ! O   
     &     21,   21,   43,   44,   45,   46,   46,   11,    0,    9, & ! 1   
     &      0,    1,    0,   20,    0,   26,    0,   30,    0,   40, & ! 2   
     &      0,   42,    0,   12,    0,   11,   12,   11,    7,   26, & ! 3   
     &     12,    7,   30,   24,    7,   24,   26,   24,   26,   21, & ! 4   
     &     11,   20,   24,   21,   21,    9,   56,    9,    9,    9, & ! 5   
     &      9,   12,   21,   11,    7,   11,   12,    7,   26,   39, & ! 6   
     &      7,   39,   10,    0,   37,    0,    7,   40,    9,   11, & ! 7   
     &     40,    9,   67,   40,    9,   40,    9,   40,   56,    9, & ! 8   
     &     24,    9,   56,   20,   40,    9,   40,   11,   11,   32, & ! 9   
     &     11,   11,   26,   12,    7,   32,   11,   32,   56,   11, & ! O   
     &     26,   20,   30,   32,   21,    0,   55,    0,   56,    0, & ! 1   
     &     44,    0,   58,    0,   67,    0,   76,    0,   70,    0, & ! 2   
     &     79,    0,   80,    0,   77,    0,   78,    0,   81,    0, & ! 3   
     &     83,    0,   84,    0,   71,    0,   43,    0,   43,   67, & ! 4   
     &      0,   64,   58,    0,   66,   67,    0,   69,   67,    0, & ! 5   
     &     87,    0,   89,    0,   20,   32,   11,   32,   11,   32, & ! 6   
     &     11,   32,   56,   32,   11,   32,   11,   32,   11,   32, & ! 7   
     &     20,   32,   11,   32,   67,   11,   11,   11,   11,   11, & ! 8   
     &     11,   11,   11,   11,   32,   32,   32,   32,   32,   32, & ! 9   
     &     11,   32,   56,   32,   11,   32,   56,   11,   11,   32, & ! O   
     &     11,   32,   56,   32,   11,   32,   56,  128,  129,  128, & ! 1   
     &    130,  131,  128,  128,  132,  133,  133,  133,  128,  134, & ! 2   
     &    133,  128,  133,  127,  135,    9,  127,  128,  132,  132, & ! 3   
     &    132,  132,  132,  132,  132,  132,  132,  132,  132,  132, & ! 4   
     &    132,  132,  132,  132,   32,  132,   24,  132,   11,   30, & ! 5   
     &    132,   26,  128,    0,  139,    0,  140,    0,  132,   32, & ! 6   
     &    132,   32,  132,  132,   11,   32,   32,   32,   32,   32, & ! 7   
     &    132,  132,  132,  132,  132,  132,  132,  132,   32,   32, & ! 8   
     &    132,   32,    2,   11,    2,   11,    2,   11,    0,    0, & ! 9   
     &      9,    5,  128,    9,    0,    5,  128,    9,    4,    5, & ! O   
     &      3,    0,  128,    7,   10,  152,  152,  152,  152,  152, & ! 1   
     &    152,  152,  160,  160,  160,  160,  160,  166,    9,  168, & ! 2   
     &      9/     !  3   

      DATA ( IRR( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &      3,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    5,    0,    0,    1,    0,    3,    0,    0,    0, & ! 1   
     &      0,    0,    2,    0,    0,    1,    0,    1,   13,    0, & ! 2   
     &      1,    0,    1,    1,    0,    0,    0,    0,    1,    0, & ! 3   
     &      0,    0,    0,   17,    0,    0,    0,    0,   10,   21, & ! 4   
     &      0,    0,   11,   21,   11,    0,    0,    0,   21,    0, & ! 5   
     &      0,    0,    0,   21,    0,    0,    0,    1,    1,   13, & ! 6   
     &     29,   13,   21,   13,   13,   13,    0,    1,    1,   24, & ! 7   
     &     36,   24,   11,   32,   32,   20,   32,    0,    1,    1, & ! 8   
     &     13,   36,   13,   11,   39,   39,   20,   24,   24,    0, & ! 9   
     &      1,    1,   13,   36,   13,   11,   21,   21,   20,   26, & ! O   
     &     26,   26,    0,   20,    0,    0,   24,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   12,    0,   12,   11,    0, & ! 3   
     &     11,   26,   24,   32,   30,   50,   20,   25,   24,   11, & ! 4   
     &     13,   24,   32,    9,   11,   24,   11,   24,   11,   11, & ! 5   
     &     11,   11,   12,   12,   11,   12,   26,   12,    0,   24, & ! 6   
     &     39,    0,    0,    0,    0,    0,   37,   24,   11,   20, & ! 7   
     &     24,   11,    0,   24,   11,   24,   11,   24,    0,   11, & ! 8   
     &     25,   11,   58,   12,   24,   11,    7,   26,   32,   26, & ! 9   
     &     32,   32,    0,   11,   26,   40,    9,   40,    0,    9, & ! O   
     &     24,   13,   24,   34,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   11, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   24,    9,   24,   32,   24, & ! 6   
     &      9,   24,   58,   24,    9,   48,   32,   24,    9,   48, & ! 7   
     &     52,   51,   32,   48,  102,    9,    9,   32,   32,   32, & ! 8   
     &     32,   32,   32,   32,   24,   24,   53,   49,   24,   49, & ! 9   
     &     32,   24,   58,   24,   32,   48,   58,   32,   32,   51, & ! O   
     &     32,   48,   67,   51,   32,   48,   67,    0,    0,    2, & ! 1   
     &      0,    0,    1,    1,    0,    9,    0,    1,    1,    0, & ! 2   
     &      1,    5,    1,    5,    0,  128,  128,    0,   11,   11, & ! 3   
     &     26,   21,   30,   24,   24,    1,   11,   11,   12,   32, & ! 4   
     &     37,    9,    9,    9,  136,   40,   25,   11,   12,    0, & ! 5   
     &     30,   24,    0,    0,    0,    0,    0,    0,   20,   24, & ! 6   
     &     32,  136,   32,   32,   12,   24,   24,   24,   24,   24, & ! 7   
     &     11,   32,   32,   32,   32,   32,   32,   32,   24,   24, & ! 8   
     &     32,  136,  142,  143,  144,  145,  146,  147,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    7,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,  165,    0,  167, & ! 2   
     &      0/     !  3   

      DATA ( IRR( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    9,    0,    0,    0,    0,    7,    0, & ! 3   
     &      0,    0,    0,   18,    0,    0,    0,    0,    0,   11, & ! 4   
     &      0,    0,    1,    0,    0,    0,    0,    0,   23,    0, & ! 5   
     &      0,    0,    0,   23,    0,    0,    0,    0,   20,    1, & ! 6   
     &      4,    1,   11,    0,    0,    0,    0,    0,   24,   32, & ! 7   
     &      4,   32,   24,   34,   34,   24,   34,    0,    0,   13, & ! 8   
     &     39,    4,   39,   24,   13,   13,   39,   32,   13,    0, & ! 9   
     &      0,   13,   21,    4,   21,   13,   26,   26,   21,   24, & ! O   
     &     39,   13,    0,    0,    0,    0,   32,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,   12,    0, & ! 3   
     &     20,    0,   32,   33,    0,   57,   12,   59,   32,    0, & ! 4   
     &      0,   13,   13,   20,    9,   25,    9,   25,   24,   24, & ! 5   
     &     76,    0,    0,   30,   12,   26,    0,   26,    0,   32, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,   25,   24,   26, & ! 7   
     &     25,   24,    0,   25,   26,   32,   24,    7,    0,   26, & ! 8   
     &     59,   24,    0,   67,   32,   24,   24,   30,   50,   30, & ! 9   
     &      1,    1,    0,   21,    0,   24,   12,   24,    0,   20, & ! O   
     &     13,    9,   33,   33,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   57,   12,   60,   20,   25, & ! 6   
     &     20,   25,    0,   25,   12,   24,   52,   25,   52,   24, & ! 7   
     &     24,   24,    9,   51,    0,   12,   12,    9,    9,    9, & ! 8   
     &      9,    9,    9,   24,   34,   25,   24,   50,   25,   24, & ! 9   
     &      9,   25,   67,   25,    9,   49,   67,    9,    9,   24, & ! O   
     &      9,   51,  102,   24,    9,   51,  126,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   12, & ! 3   
     &      0,   11,   24,   57,   25,   11,   24,   12,   26,   90, & ! 4   
     &      0,   24,   24,   11,   40,   24,   59,   40,   24,    0, & ! 5   
     &      0,  136,    0,    0,    0,    0,    0,    0,    0,   57, & ! 6   
     &     24,   24,  136,  136,    0,   25,   25,   25,   25,   25, & ! 7   
     &     32,   24,   24,   53,   49,   24,   24,  136,   25,   25, & ! 8   
     &    136,   50,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    9,    0,    9, & ! 2   
     &      0/     !  3   

      DATA ( IRR( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    5,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,   13,    0, & ! 6   
     &      0,    0,   20,    0,    0,    0,    0,    0,   32,   33, & ! 7   
     &      0,   33,   32,   33,   33,   32,   33,    0,    0,   39, & ! 8   
     &     24,    0,   24,   39,    0,    0,   24,   33,    0,    0, & ! 9   
     &      0,   21,   26,    0,   26,   26,    0,    0,   26,   32, & ! O   
     &     24,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   54,   34,    0,   33,    0,   32,   34,    0, & ! 4   
     &      0,   32,   34,    0,    0,   59,    0,   59,   25,   25, & ! 5   
     &     70,    0,    0,    0,   30,    0,    0,    0,    0,   68, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,   59,   12,   40, & ! 7   
     &     59,   12,    0,   59,   24,   54,   12,   32,    0,   40, & ! 8   
     &     32,   32,    0,   40,   25,   51,   32,   12,   51,   24, & ! 9   
     &     48,   24,    0,    0,    0,   54,   13,   25,    0,   40, & ! O   
     &     47,    0,   13,   13,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   85,   13,   33,   24,   59, & ! 6   
     &     12,   59,    0,   59,   13,   25,   24,   59,   24,   25, & ! 7   
     &     25,   25,   50,   24,    0,   61,   13,   24,   24,   24, & ! 8   
     &     24,   24,   24,   57,   33,   59,   25,   24,   59,   25, & ! 9   
     &     24,   59,    0,   59,   20,   24,    0,   24,   24,   25, & ! O   
     &     50,   24,    0,   25,   50,   24,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   34,   50,   59,   24,   25,   30,    0,   68, & ! 4   
     &      0,   56,   25,   24,   24,   25,   32,   24,  136,    0, & ! 5   
     &      0,   57,    0,    0,    0,    0,    0,    0,    0,  141, & ! 6   
     &     25,   25,   24,   50,    0,   59,   59,   59,   59,   59, & ! 7   
     &     24,   34,   25,   24,   50,   25,   25,   49,   59,   59, & ! 8   
     &     50,   51,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

      DATA ( IRR( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    5,    0, & ! 6   
     &      0,    0,   13,    0,    0,    0,    0,    0,   33,   34, & ! 7   
     &      0,   34,   34,   13,   13,   33,   13,    0,    0,   24, & ! 8   
     &      0,    0,    0,   13,    0,    0,    0,   34,    0,    0, & ! 9   
     &      0,   26,    0,    0,    0,    0,    0,    0,    0,   33, & ! O   
     &     13,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   34,   12,    0,    0,    0,   50,   33,    0, & ! 4   
     &      0,   62,   60,    0,    0,   56,    0,   67,   59,   59, & ! 5   
     &     77,    0,    0,    0,    0,    0,    0,    0,    0,   62, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,   32,   13,   12, & ! 7   
     &     32,   13,    0,   32,   25,   57,   13,   54,    0,   24, & ! 8   
     &     50,   12,    0,    0,   59,   12,   25,   21,   24,   25, & ! 9   
     &     24,   25,    0,    0,    0,   57,   21,   59,    0,   12, & ! O   
     &     57,    0,   47,    9,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   33,   21,    0,   12,   57, & ! 6   
     &     13,   33,    0,   57,   21,   59,   25,   57,   25,   59, & ! 7   
     &     59,   59,   51,   25,    0,   76,    0,   25,   25,   25, & ! 8   
     &     25,   25,   25,   55,    0,   60,   59,   25,   57,   59, & ! 9   
     &     25,   34,    0,   57,   50,   25,    0,   25,   25,   59, & ! O   
     &     51,   25,    0,   59,   51,   25,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   54,   33,   32,   25,   59,    0,    0,    0, & ! 4   
     &      0,   47,   59,   25,   25,   59,   50,   25,   57,    0, & ! 5   
     &      0,   33,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &     59,   59,   25,   51,    0,   90,   90,   90,   90,   90, & ! 7   
     &     57,   33,   59,   25,   24,   59,   59,   24,   65,   90, & ! 8   
     &     51,   52,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

      DATA ( IRR( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,   34,   13, & ! 7   
     &      0,   13,   33,    0,    0,   34,    0,    0,    0,    5, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,   39,    0,    0, & ! 9   
     &      0,    5,    0,    0,    0,    0,    0,    0,    0,   34, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   33,   11,    0,    0,    0,   51,    0,    0, & ! 4   
     &      0,   33,   63,    0,    0,   32,    0,   32,   56,   67, & ! 5   
     &     78,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,   50,   76,   76, & ! 7   
     &     50,   76,    0,   50,   59,   73,   21,   33,    0,   12, & ! 8   
     &     57,   13,    0,    0,   54,   13,   59,   87,   25,   59, & ! 9   
     &     25,   59,    0,    0,    0,   34,   61,    7,    0,   13, & ! O   
     &     33,    0,   34,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,   61,    0,   54,   34, & ! 6   
     &     21,    0,    0,   95,   61,   57,   59,   92,   59,   82, & ! 7   
     &     57,   54,   24,   59,    0,    0,    0,   59,   59,   59, & ! 8   
     &     59,   59,   59,   85,    0,   88,   57,   59,   34,   57, & ! 9   
     &     59,   60,    0,   34,   51,   59,    0,   59,   59,   54, & ! O   
     &     24,   59,    0,   54,   24,   59,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   32,    0,   50,   59,   56,    0,    0,    0, & ! 4   
     &      0,   32,   67,   59,   59,   32,  137,   59,   33,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &     95,   57,   59,   52,    0,    0,    0,    0,    0,    0, & ! 7   
     &     55,    0,   60,   59,   25,   57,   57,   25,    0,   65, & ! 8   
     &     52,   24,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

      DATA ( IRR( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,   13,    0, & ! 7   
     &      0,    0,   13,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   13, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,   57,    0,    0, & ! 4   
     &      0,    0,   33,    0,    0,   47,    0,   47,   67,   70, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,   54,   70,   70, & ! 7   
     &     54,   70,    0,   54,   12,   62,   70,    0,    0,   21, & ! 8   
     &     85,   21,    0,    0,   57,   21,   54,   58,   59,   57, & ! 9   
     &     59,   21,    0,    0,    0,   72,   76,   54,    0,   21, & ! O   
     &      0,    0,   32,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,   57,   33, & ! 6   
     &     55,    0,    0,   82,   89,   93,   54,   93,   12,   68, & ! 7   
     &     67,   57,   25,   54,    0,    0,    0,   72,   72,   72, & ! 8   
     &     72,   72,   72,   33,    0,   33,   34,   54,   60,   34, & ! 9   
     &     12,   88,    0,   60,   24,   57,    0,   72,   72,   57, & ! O   
     &     25,   54,    0,   57,   25,   54,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   33,    0,   51,   56,   67,    0,    0,    0, & ! 4   
     &      0,   57,   47,   67,  132,   54,   33,   77,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &    137,   95,   57,   24,    0,    0,    0,    0,    0,    0, & ! 7   
     &     85,    0,   88,   57,   59,   34,   34,   59,    0,    0, & ! 8   
     &     24,   25,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

      DATA ( IRR( IRXXN, 11 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    5,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,   34,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,   57,    0,   34,   32,   71, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,   60,   51,    0, & ! 7   
     &     60,   51,    0,   72,   13,   33,   61,    0,    0,   50, & ! 8   
     &     62,   70,    0,    0,   85,   58,   57,    0,   21,   34, & ! 9   
     &     57,   57,    0,    0,    0,   33,    0,   86,    0,   29, & ! O   
     &      0,    0,   60,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,   55,    0, & ! 6   
     &     61,    0,    0,   33,   84,   82,   95,   82,   13,    0, & ! 7   
     &     68,   60,   59,   57,    0,    0,    0,   46,   62,   62, & ! 8   
     &     62,   62,   62,    0,    0,    0,   60,   57,   88,   85, & ! 9   
     &     13,   86,    0,   88,   25,   34,    0,   62,   62,   60, & ! O   
     &     59,   57,    0,   60,   59,   57,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,   57,   58,   32,    0,    0,    0, & ! 4   
     &      0,   34,   32,   70,   54,   82,    0,   78,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &    138,   82,   93,   25,    0,    0,    0,    0,    0,    0, & ! 7   
     &     33,    0,   33,   34,   54,   60,   60,   57,    0,    0, & ! 8   
     &     25,   59,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,   60,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,   34,    0,   60,   34,   32, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,   73,   57,    0, & ! 7   
     &     73,   57,    0,   62,   76,    0,   51,    0,    0,   57, & ! 8   
     &     33,   61,    0,    0,   60,   76,   60,    0,   57,   60, & ! 9   
     &     34,   55,    0,    0,    0,    0,    0,   33,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,   72,    0, & ! 6   
     &     29,    0,    0,    0,   67,   86,   74,   68,   21,    0, & ! 7   
     &      0,   88,   12,   60,    0,    0,    0,   74,   46,   46, & ! 8   
     &     63,   46,   63,    0,    0,    0,   88,   34,   73,   60, & ! 9   
     &     21,   33,    0,   73,   59,   60,    0,   46,   63,   88, & ! O   
     &     12,   85,    0,   88,   12,   34,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,   34,   67,   50,    0,    0,    0, & ! 4   
     &      0,   60,   34,   71,   57,  137,    0,   32,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &     33,   33,   82,   59,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,   60,   57,   88,   95,   34,    0,    0, & ! 8   
     &     59,  132,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,   33,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,   60,    0,   65,   60,   47, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,   72,   68,    0, & ! 7   
     &     72,   68,    0,   63,   70,    0,   57,    0,    0,   33, & ! 8   
     &      0,   51,    0,    0,   73,   70,   62,    0,   55,   68, & ! 9   
     &     56,   34,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,   33,    0, & ! 6   
     &      0,    0,    0,    0,    0,   33,   75,  100,   57,    0, & ! 7   
     &      0,   62,   54,   88,    0,    0,    0,   75,   90,   90, & ! 8   
     &     46,   90,   46,    0,    0,    0,   73,   60,   65,   88, & ! 9   
     &     34,   68,    0,   93,   12,   88,    0,   74,   46,   73, & ! O   
     &     13,   60,    0,   73,   13,   60,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,   60,   43,   51,    0,    0,    0, & ! 4   
     &      0,   73,   60,   32,   72,  138,    0,   54,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,  137,   54,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,   88,   34,   73,   93,   60,    0,    0, & ! 8   
     &     54,   54,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,    0,   65,    0,   33,   65,   60, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,   62,    0,    0, & ! 7   
     &     62,    0,    0,   74,   77,    0,   33,    0,    0,    0, & ! 8   
     &      0,   57,    0,    0,   65,   61,   86,    0,   34,    0, & ! 9   
     &     60,   56,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,   67,    0,   61,    0, & ! 7   
     &      0,   63,   13,   62,    0,    0,    0,   81,   74,   74, & ! 8   
     &     90,   74,   90,    0,    0,    0,   33,   88,   68,   95, & ! 9   
     &     56,    0,    0,   82,   13,   73,    0,   75,   90,   62, & ! O   
     &     21,   88,    0,   62,   21,   88,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,   33,   48,   57,    0,    0,    0, & ! 4   
     &      0,   33,   65,   47,  137,   33,    0,   57,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,   68,   57,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,   73,   60,   65,   82,   88,    0,    0, & ! 8   
     &     57,   57,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,    0,   33,    0,    0,   68,   72, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,   68,    0,    0, & ! 7   
     &     68,    0,    0,   75,   78,    0,    0,    0,    0,    0, & ! 8   
     &      0,   33,    0,    0,   72,   36,   68,    0,   56,    0, & ! 9   
     &     88,   60,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,   33,    0,   83,    0, & ! 7   
     &      0,   68,   57,   86,    0,    0,    0,   94,   75,   75, & ! 8   
     &     74,   75,   74,    0,    0,    0,   68,   73,  119,   92, & ! 9   
     &     60,    0,    0,   65,   21,   86,    0,   81,   74,   63, & ! O   
     &     57,   92,    0,   63,   57,   92,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,   32,   34,    0,    0,    0, & ! 4   
     &      0,    0,   68,   60,   33,    0,    0,   62,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   60,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,   33,   88,   68,  137,   73,    0,    0, & ! 8   
     &     60,   60,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   73, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,   82,   81,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,   62,   85,    0,    0,   60,    0, & ! 9   
     &     58,   88,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,   84,    0, & ! 7   
     &      0,  102,   60,   68,    0,    0,    0,  105,   81,   81, & ! 8   
     &     75,   81,   75,    0,    0,    0,    0,   65,    0,   93, & ! 9   
     &     88,    0,    0,   33,   57,   33,    0,   65,   75,   93, & ! O   
     &     60,   93,    0,   93,   60,   93,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,   57,   60,    0,    0,    0, & ! 4   
     &      0,    0,    0,   72,    0,    0,    0,   74,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   88,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,   68,   73,    0,  138,   92,    0,    0, & ! 8   
     &     88,   88,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   62, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,   68,   83,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,   68,   57,    0,    0,   73,    0, & ! 9   
     &     73,   58,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,   71,    0, & ! 7   
     &      0,    0,   88,  102,    0,    0,    0,    0,   68,   68, & ! 8   
     &     81,   91,   81,    0,    0,    0,    0,   68,    0,   82, & ! 9   
     &     58,    0,    0,   68,   55,   68,    0,   68,   81,   82, & ! O   
     &     88,   82,    0,   82,   88,   82,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,   34,   73,    0,    0,    0, & ! 4   
     &      0,    0,    0,   73,    0,    0,    0,   75,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   72,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,   65,    0,   33,   93,    0,    0, & ! 8   
     &     72,   72,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   74, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,   84,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   68,    0,    0,   67,    0, & ! 9   
     &     67,   73,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,   67,    0, & ! 7   
     &      0,    0,   58,    0,    0,    0,    0,    0,   94,   94, & ! 8   
     &     68,   68,   91,    0,    0,    0,    0,    0,    0,   65, & ! 9   
     &     61,    0,    0,    0,   34,    0,    0,  107,   91,   65, & ! O   
     &     58,   86,    0,   65,   58,   86,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,   60,   65,    0,    0,    0, & ! 4   
     &      0,    0,    0,   62,    0,    0,    0,   82,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   63,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,   68,    0,   68,   82,    0,    0, & ! 8   
     &     63,   63,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   75, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,   71,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,   65,    0, & ! 9   
     &     65,   67,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,   68,    0, & ! 7   
     &      0,    0,   72,    0,    0,    0,    0,    0,  107,  109, & ! 8   
     &     94,   94,   68,    0,    0,    0,    0,    0,    0,   33, & ! 9   
     &     36,    0,    0,    0,   56,    0,    0,    0,   65,   68, & ! O   
     &     61,   68,    0,   68,   61,   68,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,   88,   68,    0,    0,    0, & ! 4   
     &      0,    0,    0,   74,    0,    0,    0,  137,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   93,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,  137,    0,    0, & ! 8   
     &     93,   93,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   68, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,   32,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,   68,    0, & ! 9   
     &     43,   65,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   63,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &    109,  109,   94,    0,    0,    0,    0,    0,    0,   68, & ! 9   
     &     67,    0,    0,    0,   60,    0,    0,    0,   68,  102, & ! O   
     &     36,  102,    0,  126,   36,  126,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,   73,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,   75,    0,    0,    0,  138,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   82,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,  138,    0,    0, & ! 8   
     &     82,   82,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,   51,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &     86,   33,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   67,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,  109,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &     33,    0,    0,    0,   44,    0,    0,    0,  109,    0, & ! O   
     &     62,    0,    0,    0,   62,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,   65,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,   68,    0,    0,    0,   68,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   74,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,   33,    0,    0, & ! 8   
     &     74,   74,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,   57,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &     33,   68,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   68,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &     68,    0,    0,    0,   58,    0,    0,    0,    0,    0, & ! O   
     &     63,    0,    0,    0,   63,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,   86,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   75,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,   68,    0,    0, & ! 8   
     &     75,   75,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,   62,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &     68,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,  102,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,   73,    0,    0,    0,    0,    0, & ! O   
     &     92,    0,    0,    0,   92,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,   68,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   91,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &     91,   91,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,   68,    0,    0,    0,    0,    0, & ! 8   
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
     &      0,    0,    0,    0,   61,    0,    0,    0,    0,    0, & ! O   
     &     82,    0,    0,    0,   82,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,  137,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &    137,  137,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,   29,    0,    0,    0,    0,    0, & ! O   
     &     67,    0,    0,    0,   67,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   68,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &     68,   68,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0/     !  3   

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
     &      0,    0,    0,    0,   36,    0,    0,    0,    0,    0, & ! O   
     &     68,    0,    0,    0,   68,    0,    0,    0,    0,    0, & ! 1   
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
     &      0/     !  3   

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
     &      0,    0,    0,    0,   67,    0,    0,    0,    0,    0, & ! O   
     &    102,    0,    0,    0,  126,    0,    0,    0,    0,    0, & ! 1   
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
     &      0/     !  3   

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
     &      0,    0,    0,    0,   33,    0,    0,    0,    0,    0, & ! O   
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
     &      0/     !  3   

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
     &      0,    0,    0,    0,   68,    0,    0,    0,    0,    0, & ! O   
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
     &      0/     !  3   

      DATA ( RTDAT( 1,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 5.6800D-34, 8.0000D-12, 9.0000D-32, 5.5000D-12, & ! O   
     &     2.5000D-31, 3.0000D-12, 1.4000D-13, 1.8000D-11, 3.3000D-39, & ! +   
     &     3.6000D-30, 1.3000D-03, 1.0000D-22, 0.0000D+00, 4.5000D-14, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.6300D-10, & ! +   
     &     2.3800D-11, 7.0000D-31, 1.0000D+00, 2.5000D-12, 1.4800D-30, & ! 2   
     &     2.0000D-11, 2.4000D-14, 1.0000D+00, 1.4400D-13, 1.7000D-12, & ! +   
     &     3.6000D-12, 2.0000D-31, 3.7200D-05, 1.0000D+00, 1.3000D-12, & ! 3   
     &     2.0300D-16, 2.2000D-13, 3.0800D-34, 4.0000D-12, 8.5000D-13, & ! +   
     &     1.0000D+00, 1.8000D-12, 4.8000D-11, 3.3000D-31, 7.7000D-12, & ! 4   
     &     0.0000D+00, 2.7600D-11, 1.7000D-10, 0.0000D+00, 2.3000D-12, & ! +   
     &     3.4600D-13, 3.3400D-14, 1.3000D-12, 6.3900D-14, 7.4000D-13, & ! 5   
     &     2.6000D-12, 3.8000D-13, 2.3000D-12, 2.0000D-13, 3.5000D-14, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 2.7000D-28, 4.9000D-03, 1.0000D+00, 7.5000D-12, & ! +   
     &     5.2000D-13, 1.0000D+00, 2.0000D-12, 4.4000D-13, 1.0000D+00, & ! 7   
     &     2.9000D-12, 1.2100D-11, 8.3000D+16, 1.0000D+00, 6.7000D-12, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.3700D-11, 7.9000D+16, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.6000D+16, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.4000D-11, 7.5000D+14, 2.3000D-11, & ! 1   
     &     1.0000D+00, 1.0000D-03, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 5.4000D-12, 2.0000D-12, 4.4000D-12, & ! +   
     &     1.0000D+00, 1.4000D-12, 5.1000D-12, 1.0000D+00, 1.4000D-12, & ! 4   
     &     4.5600D-14, 5.0000D-01, 1.3000D-12, 1.7500D-01, 2.8500D-12, & ! +   
     &     4.5000D-13, 4.2000D-14, 1.2000D-12, 3.8000D-12, 1.0000D+00, & ! 5   
     &     2.5000D-11, 1.0000D+00, 5.6000D-11, 1.0000D+00, 1.4100D-10, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.1000D-11, 2.8000D-12, & ! 6   
     &     1.0000D+00, 1.5000D-11, 1.4000D-12, 1.0000D+00, 1.7000D-12, & ! +   
     &     1.4000D-11, 3.5000D-12, 1.5000D-03, 1.5000D-02, 1.2000D-11, & ! 7   
     &     6.0000D-02, 1.3400D-12, 7.4000D-11, 9.6600D-18, 1.0000D+00, & ! +   
     &     7.4000D-11, 9.6600D-18, 1.0000D+00, 9.3500D-11, 1.4300D-17, & ! 8   
     &     8.0000D-12, 1.4000D-15, 1.5000D-12, 6.3400D-12, 1.0000D+00, & ! +   
     &     2.6000D-12, 8.5000D-16, 4.3200D-12, 1.0000D+00, 6.1900D-11, & ! 9   
     &     4.1800D-18, 1.0000D-13, 1.0000D+00, 1.5500D-11, 4.8600D-03, & ! +   
     &     7.2000D-12, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.9900D-11, 1.4000D-15, 1.1800D-15, 2.3700D-12, 1.0000D+00, & ! +   
     &     5.2800D-12, 1.0000D+00, 6.4200D-12, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.8500D-12, & ! 6   
     &     1.0000D-28, 9.1400D-15, 3.3000D-12, 1.0700D-11, 4.8500D-12, & ! +   
     &     5.5100D-15, 4.5900D-13, 1.0200D-11, 1.4800D-11, 1.3400D-14, & ! 7   
     &     1.0000D-13, 2.2600D-11, 2.5400D-11, 7.8600D-15, 3.0300D-12, & ! +   
     &     3.5000D-11, 1.2100D-11, 5.0000D-16, 1.1900D-12, 3.2000D-11, & ! 8   
     &     5.5000D-30, 1.0000D-14, 2.3300D-12, 1.8100D-12, 2.3100D-11, & ! +   
     &     1.3600D-11, 1.4300D-11, 3.2500D-11, 5.4900D-13, 1.3400D-12, & ! 9   
     &     1.4900D-12, 1.5100D-12, 3.7500D-12, 2.7000D-12, 6.7200D-12, & ! +   
     &     3.1900D-15, 5.3700D-13, 1.6100D-11, 1.2600D-11, 8.5900D-15, & ! O   
     &     2.3100D-13, 1.4300D-11, 7.8400D-12, 3.0900D-11, 2.2700D-11, & ! +   
     &     8.2800D-16, 1.3300D-12, 4.0200D-11, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 7.6000D-32, 1.0000D+00, & ! +   
     &     1.3000D-30, 1.8000D-31, 1.0000D+00, 1.0000D+00, 3.4400D-11, & ! 2   
     &     9.4100D-12, 2.8000D-11, 2.4000D-11, 6.2000D-12, 1.8000D-31, & ! +   
     &     1.0000D+00, 1.0000D+00, 4.4800D-05, 6.2000D-12, 2.2000D-12, & ! 3   
     &     1.0000D+00, 1.2500D-11, 1.7000D-12, 3.9000D-11, 8.1000D-11, & ! +   
     &     8.0000D-11, 5.5000D-11, 1.2300D-10, 7.7000D-11, 3.6000D-11, & ! 4   
     &     1.9200D-10, 2.0000D-10, 8.1000D-11, 8.0000D-11, 6.2000D-11, & ! +   
     &     8.0000D-11, 1.6600D-10, 3.0000D-10, 4.2900D-10, 2.9400D-10, & ! 5   
     &     3.8500D-10, 2.3200D-10, 4.1200D-10, 1.0000D+00, 3.1000D-12, & ! +   
     &     1.2900D-11, 5.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 7.3000D-12, 1.6000D-29, & ! +   
     &     2.6700D-10, 4.9000D-10, 4.8000D-10, 5.4600D-10, 5.2000D-30, & ! 7   
     &     6.2000D-11, 1.3500D-10, 1.4000D-10, 1.4400D-10, 2.4200D-10, & ! +   
     &     8.6000D-11, 8.3000D-11, 1.2000D-10, 1.8600D-10, 2.6300D-10, & ! 8   
     &     4.2100D-10, 3.9200D-10, 3.7700D-10, 2.1600D-10, 2.6600D-10, & ! +   
     &     5.4600D-10, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     5.4000D-12, 2.0000D-12, 8.1000D-11, 4.4000D-12, 1.0000D+00, & ! O   
     &     1.4000D-12, 8.0000D-11, 1.9900D-11, 1.4000D-15, 1.1800D-15, & ! +   
     &     2.3700D-12, 1.0000D+00, 2.9400D-10, 1.0000D+00, 1.0000D+00, & ! 1   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 2   
     &     9.4882D-06, 9.4882D-06, 2.5000D-12, 1.0000D+00, 2.5000D-12, & ! +   
     &     1.0000D+00/           !        3   

      DATA ( RTDAT( 2,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00,-2.6000D+00, 0.0000D+00,-1.5000D+00, 0.0000D+00, & ! O   
     &    -1.8000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -4.1000D+00,-3.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-2.6000D+00, 0.0000D+00, 0.0000D+00,-3.0000D+00, & ! 2   
     &     0.0000D+00, 4.6000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-3.4000D+00,-2.4000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     4.5700D+00, 6.0000D+02, 2.8000D+03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-4.3000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.6000D-01,-3.5300D+00, 0.0000D+00,-1.8000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     5.6000D+01, 5.7000D+01, 5.8000D+01, 5.9000D+01, 6.0000D+01, & ! 6   
     &     6.0000D+01,-7.1000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 5.8000D+01, 0.0000D+00, 0.0000D+00, 7.4000D+01, & ! 7   
     &     0.0000D+00,-1.0700D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     7.1000D+01, 5.8000D+01, 7.3000D+01, 7.4000D+01, 7.4000D+01, & ! 8   
     &     7.6000D+01, 7.6000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     8.0000D+01, 7.1000D+01, 5.8000D+01, 7.3000D+01, 7.4000D+01, & ! 9   
     &     7.4000D+01, 7.6000D+01, 7.6000D+01, 7.6000D+01, 7.7000D+01, & ! +   
     &     0.0000D+00, 0.0000D+00, 8.0000D+01, 7.1000D+01, 5.8000D+01, & ! O   
     &     7.3000D+01, 7.4000D+01, 7.4000D+01, 7.6000D+01, 7.6000D+01, & ! +   
     &     7.6000D+01, 7.6000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     5.7000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     3.6500D+00, 0.0000D+00, 2.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 1.4000D+02, 0.0000D+00, 1.4200D+02, & ! O   
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
     &    -4.5000D+00, 0.0000D+00, 2.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &    -2.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D+00, 2.0000D+00, & ! 9   
     &     2.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.1000D+02, 3.1100D+02, & ! 1   
     &     3.1200D+02, 3.1300D+02, 0.0000D+00,-1.8000D+00, 0.0000D+00, & ! +   
     &    -2.0000D+00,-2.0000D+00, 0.0000D+00, 0.0000D+00,-5.6000D-01, & ! 2   
     &     2.1000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-3.4000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-3.3000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-2.4000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.9100D+02, 5.6000D+01, 5.7000D+01, 5.6000D+01, & ! 9   
     &     5.7000D+01, 5.6000D+01, 5.7000D+01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00/           !        3   

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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.6000D+02, & ! +   
     &     7.8000D+02, 7.8000D+02, 0.0000D+00, 3.6500D+02,-5.2000D+02, & ! 5   
     &     3.8000D+02, 9.0000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00,-1.2100D+04, 0.0000D+00, 2.9000D+02, & ! +   
     &     9.8000D+02, 0.0000D+00, 5.0000D+02, 1.0700D+03, 0.0000D+00, & ! 7   
     &     5.0000D+02, 0.0000D+00,-1.3940D+04, 0.0000D+00, 3.4000D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.4000D+04, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -1.3486D+04, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-8.1520D+03, 1.5000D+02, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 1.3500D+02,-2.4310D+03, 3.6500D+02, & ! +   
     &     0.0000D+00,-1.8600D+03, 4.0500D+02, 0.0000D+00,-1.6010D+03, & ! 4   
     &     4.2900D+02, 0.0000D+00,-2.5000D+01, 0.0000D+00,-3.4500D+02, & ! +   
     &     0.0000D+00, 8.5500D+02, 0.0000D+00, 2.0000D+02, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-2.3760D+03, & ! 6   
     &     0.0000D+00, 0.0000D+00,-1.8950D+03, 0.0000D+00, 9.5000D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00,-1.8600D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     3.8000D+02,-2.1000D+03,-1.8150D+03, 0.0000D+00, 0.0000D+00, & ! +   
     &     6.1000D+02,-1.5200D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00,-2.5280D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.6900D+03, & ! 6   
     &     0.0000D+00,-2.5800D+03,-2.8800D+03,-8.0000D+02, 5.0400D+02, & ! +   
     &    -1.8780D+03,-1.1560D+03,-2.8000D+02, 4.4800D+02,-2.2830D+03, & ! 7   
     &     0.0000D+00,-4.0000D+01, 4.1000D+02,-1.9120D+03,-4.4800D+02, & ! +   
     &     0.0000D+00, 4.3600D+02,-5.3000D+02, 4.9000D+02, 0.0000D+00, & ! 8   
     &     0.0000D+00,-4.1000D+03,-1.9300D+02, 3.3800D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.3000D+02,-4.9900D+02, & ! 9   
     &    -8.7000D+01, 1.2600D+02, 4.4000D+01, 3.7400D+02, 5.0100D+02, & ! +   
     &    -1.7010D+03,-1.0470D+03,-3.2600D+02, 4.8800D+02,-1.2550D+03, & ! O   
     &     3.8200D+02, 1.1100D+02, 0.0000D+00, 0.0000D+00, 4.3500D+02, & ! +   
     &    -7.8500D+02, 4.9000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00,-2.5000D+02, 0.0000D+00, 2.9500D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-1.2530D+04, 1.4500D+02, 3.4000D+02, & ! 3   
     &     0.0000D+00,-1.9600D+03,-2.3000D+02,-2.3100D+03,-3.0000D+01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.0000D+03, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00,-3.0000D+01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.2800D+03, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     4.5000D+01,-1.0000D+02, 4.0000D+01, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.3500D+02,-2.4310D+03,-3.0000D+01, 3.6500D+02, 0.0000D+00, & ! O   
     &    -1.8600D+03, 0.0000D+00, 0.0000D+00,-2.5280D+03, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00/           !        3   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &      4,    6,   11,   12,   22,   25,   27,   29,   32,   33, & 
     &     37,   38,   44,   67,   68,  266,  286,  321,  322,  330, & 
     &    333,  370,  375/

      DATA ( RFDAT( 1,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     3.0000D-11, 2.2000D-11, 1.9000D-12, 9.7000D+14, 3.6000D-11, & 
     &     2.5800D-11, 2.1990D+03, 0.0000D+00, 2.9000D-12, 5.4200D+15, & 
     &     9.8000D+02, 3.1800D+03, 1.6000D-12, 1.2100D-11, 4.0000D+16, & 
     &     8.8000D-12, 8.3000D-13, 1.0000D-10, 1.0000D-10, 1.5000D-11, & 
     &     3.7100D+15, 3.1000D-10, 2.2000D-10/

      DATA ( RFDAT( 2,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00,-7.0000D-01, 2.0000D-01, 1.0000D-01,-1.0000D-01, & 
     &     0.0000D+00, 6.5000D-34, 0.0000D+00,-1.1000D+00,-2.3000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-9.0000D-01, 0.0000D+00, & 
     &    -8.5000D-01, 0.0000D+00,-1.0000D+00,-1.0000D+00,-1.9000D+00, & 
     &     3.5000D+00,-1.0000D+00, 0.0000D+00/

      DATA ( RFDAT( 3,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.1080D+04, 0.0000D+00, & 
     &     0.0000D+00, 1.3350D+03, 0.0000D+00, 0.0000D+00,-1.1170D+04, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.3600D+04, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &    -1.2530D+04, 0.0000D+00, 0.0000D+00/

      DATA ( RFDAT( 4,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     6.0000D-01, 6.0000D-01, 3.5000D-01, 3.5000D-01, 6.0000D-01, & 
     &     6.0000D-01, 0.0000D+00, 0.0000D+00, 6.0000D-01, 6.0000D-01, & 
     &     0.0000D+00, 0.0000D+00, 6.0000D-01, 3.0000D-01, 3.0000D-01, & 
     &     6.0000D-01, 6.0000D-01, 6.0000D-01, 6.0000D-01, 6.0000D-01, & 
     &     6.0000D-01, 6.0000D-01, 6.0000D-01/

      DATA ( RFDAT( 5,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     1.0000D+00, 1.0000D+00, 1.3300D+00, 1.3300D+00, 1.0000D+00, & 
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.4100D+00, 1.4100D+00, & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00/

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
     &        1.00000,    1.00000,    1.00000,    1.00000,    2.00000, & ! 5   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.50000,    0.00000, & ! 6   
     &        0.00000,    1.00000,    1.00000,    0.60000,    1.00000, & ! +   
     &        0.70000,    1.00000,    0.10000,    1.00000,    1.00000, & ! 7   
     &        2.00000,    1.00000,    1.00000,    0.60000,    1.00000, & ! +   
     &        0.75000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        2.00000,    2.00000,    1.00000,    1.00000,    0.60000, & ! +   
     &        1.00000,    0.75000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    2.00000,    2.00000,    2.00000,    1.00000, & ! +   
     &        1.00000,    0.60000,    1.00000,    0.75000,    1.00000, & ! O   
     &        2.00000,    1.00000,    1.00000,    2.00000,    1.00000, & ! +   
     &        1.00000,    2.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! 2   
     &        1.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! 3   
     &        2.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.96500,    1.00000,    1.00000, & ! 4   
     &        1.00000,    0.62000,    0.96700,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.50900,    1.00000,    0.30000,    1.00000, & ! 5   
     &        0.74400,    1.00000,    0.84000,    1.00000,    0.13900, & ! +   
     &        1.00000,    2.00000,    1.00000,    0.63000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    2.00000,    0.20000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! 7   
     &        0.00000,    1.00000,    0.21700,    0.82600,    1.02300, & ! +   
     &        0.21700,    0.82600,    1.00000,    0.20600,    0.47100, & ! 8   
     &        0.50000,    0.20800,    0.50000,    1.00000,    0.33000, & ! +   
     &        0.97500,    0.16400,    0.45000,    0.40000,    0.28900, & ! 9   
     &        0.28500,    0.15000,    1.23300,    0.47200,    0.91300, & ! +   
     &        0.18900,    0.34400,    1.00000,    1.00000,    1.00000, & ! O   
     &        0.25000,    0.83000,    0.03100,    1.00000,    1.06600, & ! +   
     &        0.98000,    1.00000,    0.80600,    1.00000,    1.00000, & ! 1   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! 2   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! 3   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! 4   
     &        0.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! 5   
     &        1.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! 6   
     &        1.00000,    0.16000,    1.00000,    0.80000,    0.98400, & ! +   
     &        0.16500,    0.94900,    0.45000,    0.95100,    0.08000, & ! 7   
     &        0.81500,    0.25000,    0.90700,    0.06600,    0.74900, & ! +   
     &        0.25000,    0.79900,    0.00900,    0.05600,    1.00000, & ! 8   
     &        0.30000,    1.50000,    0.57000,    0.18100,    0.15900, & ! +   
     &        0.16100,    0.15900,    0.02200,    0.95000,    1.00000, & ! 9   
     &        0.96500,    0.69500,    0.83000,    0.64700,    0.87100, & ! +   
     &        0.09500,    0.77200,    0.45000,    0.91200,    0.09400, & ! O   
     &        0.40000,    0.07900,    0.12300,    0.07700,    0.73400, & ! +   
     &        0.07800,    0.22700,    0.23700,    0.73400,    0.07800, & ! 1   
     &        0.22700,    0.23700,    2.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    0.29000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.40400,    0.48400, & ! 5   
     &        0.25000,    1.28300,    0.40100,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! 6   
     &        0.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.12400,    0.39000,    0.15000,    0.54800,    1.00000, & ! 7   
     &        0.89400,    0.86400,    0.86400,    0.86400,    0.83800, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    0.38400,    0.27900,    0.84000,    0.82800, & ! +   
     &        0.54800,    0.25200,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    2.00000,    0.50000, & ! 1   
     &        1.14280,    1.14280,    1.14280,    1.00000,    1.00000, & ! +   
     &        0.85714,    0.85714,    1.00000,    1.00000,    0.50000, & ! 2   
     &        0.50000,    1.50000,    1.25000,    1.00000,    1.25000, & ! +   
     &        1.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    1.00000,    2.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.75000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.75000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    1.00000,    0.60000,    1.00000, & ! +   
     &        0.30000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 7   
     &        2.00000,    0.00000,    1.00000,    0.60000,    1.00000, & ! +   
     &        0.25000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    2.00000,    0.00000,    1.00000,    0.60000, & ! +   
     &        1.00000,    0.25000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    2.00000,    2.00000,    0.00000, & ! +   
     &        1.00000,    0.60000,    1.00000,    0.25000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    2.00000,    0.00000,    1.00000,    0.00000, & ! 1   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        1.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.03500,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.38000,    0.03900,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.49100,    1.00000,    0.30000,    1.00000, & ! 5   
     &        0.25100,    1.00000,    0.22200,    0.14200,    0.14800, & ! +   
     &        1.00000,    2.00000,    1.00000,    1.26000,    0.63000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    0.00000,    0.80000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    1.00000,    0.72300,    0.52200,    0.17300, & ! +   
     &        0.72300,    0.52200,    0.00000,    0.73300,    0.55400, & ! 8   
     &        0.50000,    0.10800,    0.50000,    0.00000,    0.67000, & ! +   
     &        0.02500,    0.06400,    0.55000,    0.60000,    0.67000, & ! 9   
     &        0.40000,    0.15000,    0.46700,    0.37900,    0.40000, & ! +   
     &        0.30500,    0.55400,    0.00000,    2.00000,    1.00000, & ! O   
     &        0.75000,    0.33000,    0.96700,    0.00000,    0.17800, & ! +   
     &        0.02000,    1.00000,    0.19400,    1.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        1.00000,    0.16000,    1.00000,    0.29000,    0.98400, & ! +   
     &        0.35000,    0.94900,    0.55000,    1.18900,    0.08000, & ! 7   
     &        0.12000,    0.11700,    0.98600,    0.26600,    0.18700, & ! +   
     &        0.24000,    0.00400,    0.10200,    0.64300,    1.00000, & ! 8   
     &        0.70000,    0.50000,    0.29000,    0.45400,    0.52000, & ! +   
     &        0.55400,    0.48700,    0.62700,    0.05000,    1.00000, & ! 9   
     &        0.96500,    0.23600,    0.01000,    1.60500,    0.00100, & ! +   
     &        0.05700,    1.46300,    0.39000,    0.95300,    0.04100, & ! O   
     &        0.42600,    0.75100,    0.56600,    0.61700,    0.06400, & ! +   
     &        0.04600,    0.28700,    0.76300,    0.06400,    0.04600, & ! 1   
     &        0.28700,    0.76300,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! 2   
     &        1.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 3   
     &        1.00000,    1.42000,    0.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.90000,    1.00000,    0.97500, & ! 4   
     &        0.03800,    0.31400,    0.63000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.41400,    0.14500,    0.13900,    0.27400, & ! 5   
     &        0.16500,    0.05300,    0.08400,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    1.00000,    2.00000, & ! +   
     &        0.97100,    0.54100,    0.73800,    0.25200,    1.00000, & ! 7   
     &        0.89400,    0.86400,    0.86400,    0.86400,    0.83800, & ! +   
     &        0.68800,    1.00000,    0.97000,    0.83500,    0.82700, & ! 8   
     &        0.64700,    0.87300,    0.45000,    0.84000,    0.82800, & ! +   
     &        0.25200,    0.06800,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.50000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.25000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.25000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.40000,    1.00000, & ! +   
     &        0.30000,    1.00000,    0.90000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.40000,    1.00000, & ! +   
     &        0.25000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    2.00000,    0.00000,    0.00000,    0.40000, & ! +   
     &        1.00000,    0.25000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    2.00000,    0.00000, & ! +   
     &        0.00000,    0.40000,    1.00000,    0.25000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    2.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.03500,    1.00000,    0.00000, & ! 4   
     &        1.00000,    0.38000,    0.03900,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.50900,    0.14300,    0.70000,    1.00000, & ! 5   
     &        0.00400,    1.00000,    0.02900,    0.78200,    0.58900, & ! +   
     &        0.50000,    0.00000,    0.00000,    0.37000,    1.26000, & ! 6   
     &        1.00000,    0.00000,    1.00000,    0.00000,    0.80000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.06000,    0.65200,    0.30500, & ! +   
     &        0.06000,    0.65200,    0.00000,    0.11700,    0.01300, & ! 8   
     &        0.50000,    0.10000,    0.50000,    0.00000,    0.34000, & ! +   
     &        0.02500,    0.05000,    0.00000,    0.60000,    0.67000, & ! 9   
     &        0.04800,    0.79900,    0.30000,    0.02900,    0.60000, & ! +   
     &        0.01900,    1.00000,    0.00000,    1.00000,    0.00000, & ! O   
     &        0.25000,    1.00500,    0.03100,    0.00000,    0.23400, & ! +   
     &        0.02000,    1.00000,    0.19400,    1.00000,    0.00000, & ! 1   
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
     &        1.61000,    0.51000,    1.00000,    0.51000,    0.01600, & ! +   
     &        0.35500,    0.05100,    0.00000,    0.04900,    0.25500, & ! 7   
     &        1.05500,    0.11800,    0.09300,    0.19200,    0.93600, & ! +   
     &        0.24000,    1.04200,    0.72800,    0.00700,    0.00000, & ! 8   
     &        0.30000,    1.50000,    0.11600,    0.31200,    0.23900, & ! +   
     &        0.19800,    0.27800,    0.23000,    0.05000,    1.00000, & ! 9   
     &        0.03500,    1.25300,    0.01100,    0.35300,    1.20200, & ! +   
     &        0.12800,    0.22800,    0.16000,    0.08800,    0.44300, & ! O   
     &        0.03500,    0.17000,    0.20200,    0.17800,    1.21100, & ! +   
     &        0.49900,    0.02600,    1.00000,    1.21100,    0.49900, & ! 1   
     &        0.02600,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.10000,    1.00000,    0.03900, & ! 4   
     &        0.05500,    0.68000,    1.26000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.58800,    1.07800,    0.14800,    0.21600, & ! 5   
     &        0.80200,    0.05300,    0.15400,    1.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.97100,    1.88400,    0.17700,    0.06800,    0.00000, & ! 7   
     &        0.10600,    0.13600,    0.13600,    0.13600,    0.16200, & ! +   
     &        0.31200,    1.00000,    0.97000,    0.09400,    0.00300, & ! 8   
     &        1.54100,    1.60800,    0.44200,    0.16000,    0.17200, & ! +   
     &        0.06800,    0.03400,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.40000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.90000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.40000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    2.00000,    0.00000,    0.00000,    0.40000, & ! +   
     &        1.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! 9   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.40000,    1.00000,    0.00000,    1.00000, & ! O   
     &        1.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.03500,    1.00000,    0.00000, & ! 4   
     &        1.00000,    0.00000,    0.37600,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.49100,    0.14200,    0.00000,    0.00000, & ! 5   
     &        0.00400,    0.00000,    0.02900,    0.07700,    0.12400, & ! +   
     &        0.50000,    0.00000,    0.00000,    0.00000,    0.37000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.80000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.06000,    0.52200,    0.50000, & ! +   
     &        0.06000,    0.52200,    0.00000,    0.11700,    0.25800, & ! 8   
     &        0.41600,    0.45000,    0.50000,    0.00000,    0.33000, & ! +   
     &        0.30000,    0.05000,    0.00000,    0.40000,    0.04100, & ! 9   
     &        0.04800,    0.79900,    1.23300,    0.04900,    1.59000, & ! +   
     &        0.31300,    0.72100,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.16700,    0.31000,    0.00200,    0.00000,    0.33000, & ! +   
     &        0.02000,    0.00000,    0.11000,    1.00000,    0.00000, & ! 1   
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
     &        0.19500,    0.12000,    1.00000,    0.29000,    0.01600, & ! +   
     &        0.52500,    0.05100,    0.00000,    0.04900,    0.18500, & ! 7   
     &        0.06500,    0.23500,    0.09300,    0.19200,    0.06400, & ! +   
     &        0.01000,    0.19700,    0.00100,    1.05000,    0.00000, & ! 8   
     &        0.30000,    0.50000,    0.29000,    0.45400,    0.52000, & ! +   
     &        0.55400,    0.48700,    0.62700,    0.08100,    1.00000, & ! 9   
     &        0.03500,    0.07000,    1.76300,    0.35300,    0.12800, & ! +   
     &        0.09000,    0.22800,    0.00000,    0.08800,    0.30700, & ! O   
     &        1.19300,    0.00000,    0.56600,    0.61700,    0.20100, & ! +   
     &        0.20200,    1.78600,    0.00000,    0.20100,    0.20200, & ! 1   
     &        1.78600,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.10000,    1.00000,    0.03900, & ! 4   
     &        1.28200,    0.11600,    0.37000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.41400,    0.11700,    0.58900,    1.03200, & ! 5   
     &        0.03300,    0.32200,    0.73000,    1.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.02900,    0.06900,    1.16800,    0.03400,    0.00000, & ! 7   
     &        0.10600,    0.13600,    0.13600,    0.13600,    0.16200, & ! +   
     &        0.31200,    1.00000,    0.03000,    1.36100,    0.00400, & ! 8   
     &        0.35200,    0.12700,    0.00100,    0.16000,    0.17200, & ! +   
     &        0.03400,    0.05000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.40000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.90000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.40000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    2.00000,    0.00000,    0.00000,    0.40000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.40000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        2.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.03500,    1.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.51000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.49100,    0.40000,    0.00000,    0.00000, & ! 5   
     &        0.74400,    0.00000,    0.84000,    0.07700,    0.12400, & ! +   
     &        0.50000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.25000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.52100,    0.17400,    0.69500, & ! +   
     &        0.52100,    0.17400,    0.00000,    0.56100,    0.00700, & ! 8   
     &        0.08400,    0.11700,    0.50000,    0.00000,    0.33000, & ! +   
     &        0.67500,    0.47500,    0.00000,    0.00000,    0.04100, & ! 9   
     &        0.49800,    0.05100,    0.30000,    0.47300,    0.08700, & ! +   
     &        0.97600,    0.10200,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.08300,    0.50000,    0.00200,    0.00000,    1.18800, & ! +   
     &        0.02000,    0.00000,    0.11000,    1.00000,    0.00000, & ! 1   
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
     &        1.00000,    1.00000,    0.00000,    0.51000,    0.98400, & ! +   
     &        0.21500,    1.00000,    0.00000,    0.70800,    0.50000, & ! 7   
     &        0.06500,    0.01500,    0.62400,    0.00800,    0.06400, & ! +   
     &        0.01000,    0.19700,    0.29700,    0.29300,    0.00000, & ! 8   
     &        0.70000,    0.00000,    0.02400,    0.05400,    0.08200, & ! +   
     &        0.08700,    0.07600,    0.12100,    0.95000,    0.00000, & ! 9   
     &        0.26100,    0.07000,    0.14900,    0.04000,    0.12800, & ! +   
     &        0.00500,    0.01300,    0.00000,    0.17900,    0.15600, & ! O   
     &        0.14000,    0.00000,    0.11000,    0.12800,    0.20100, & ! +   
     &        0.05900,    0.46000,    0.00000,    0.20100,    0.05900, & ! 1   
     &        0.46000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.10000,    1.00000,    0.84000, & ! 4   
     &        0.20200,    0.11600,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.10400,    0.11700,    0.12400,    0.02600, & ! 5   
     &        0.03300,    0.62500,    0.05100,    1.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.02900,    0.06900,    0.08500,    0.05000,    0.00000, & ! 7   
     &        0.89400,    0.86400,    0.86400,    0.86400,    0.83800, & ! +   
     &        0.50300,    1.00000,    0.03000,    0.07000,    1.73700, & ! 8   
     &        0.35200,    0.12700,    1.49200,    0.84000,    0.46900, & ! +   
     &        0.05000,    0.01600,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.40000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 8   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.40000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.40000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.03500,    1.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.07400,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.49100,    0.45700,    0.00000,    0.00000, & ! 5   
     &        0.23900,    0.00000,    0.09000,    0.08500,    0.07400, & ! +   
     &        0.50000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.20100,    0.43200,    0.19500, & ! +   
     &        0.20100,    0.43200,    0.00000,    0.11700,    0.00700, & ! 8   
     &        0.41600,    0.10000,    0.50000,    0.00000,    0.67000, & ! +   
     &        0.30000,    0.12400,    0.00000,    0.00000,    0.33600, & ! 9   
     &        0.14000,    0.05100,    0.46700,    0.07100,    0.08700, & ! +   
     &        0.17500,    0.10200,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.16700,    0.18500,    0.96700,    0.00000,    0.10200, & ! +   
     &        0.02000,    0.00000,    0.11000,    0.00000,    0.00000, & ! 1   
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
     &        0.00000,    0.37000,    0.00000,    0.27800,    0.98400, & ! +   
     &        0.50000,    0.00000,    0.00000,    0.48000,    0.18500, & ! 7   
     &        0.11500,    0.01500,    0.23000,    0.00800,    0.93600, & ! +   
     &        0.24000,    0.00200,    1.51100,    0.29300,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.02400,    0.05400,    0.08200, & ! +   
     &        0.08700,    0.07600,    0.12100,    0.01000,    0.00000, & ! 9   
     &        0.70400,    0.02600,    0.14900,    0.10600,    0.58200, & ! +   
     &        0.00500,    0.00300,    0.00000,    0.83500,    0.00800, & ! O   
     &        0.14000,    0.00000,    0.11000,    0.12800,    0.00100, & ! +   
     &        0.49000,    0.46000,    0.00000,    0.00100,    0.49000, & ! 1   
     &        0.46000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.10000,    0.00000,    0.08500, & ! 4   
     &        0.20200,    0.19800,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.48200,    0.14500,    0.12400,    0.02600, & ! 5   
     &        0.80200,    0.94700,    0.05100,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.12400,    0.86300,    0.08500,    0.01600,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.68800,    0.00000,    0.48200,    0.07000,    0.16500, & ! 8   
     &        0.02200,    0.03600,    0.10600,    0.00000,    0.35900, & ! +   
     &        0.01600,    2.25800,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.40000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    2.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.08800,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 5   
     &        0.01200,    0.00000,    0.04100,    0.14200,    0.14700, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.33400,    0.56800,    0.30500, & ! +   
     &        0.33400,    0.56800,    0.00000,    0.11400,    0.58000, & ! 8   
     &        0.08400,    0.90000,    0.00000,    0.00000,    0.34000, & ! +   
     &        0.67500,    0.05000,    0.00000,    0.00000,    0.05500, & ! 9   
     &        0.12400,    0.57200,    0.23300,    0.07100,    0.30300, & ! +   
     &        0.17500,    0.07400,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.08300,    0.50000,    0.03100,    0.00000,    0.34000, & ! +   
     &        0.00000,    0.00000,    0.08400,    0.00000,    0.00000, & ! 1   
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
     &        0.00000,    0.00000,    0.00000,    0.27800,    1.00000, & ! +   
     &        0.50000,    0.00000,    0.00000,    0.47100,    0.50000, & ! 7   
     &        0.46000,    0.11500,    0.32000,    0.27500,    1.00000, & ! +   
     &        0.75000,    0.02200,    0.33700,    0.00500,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.29000,    0.23800,    0.10000, & ! +   
     &        0.08400,    0.28600,    0.07400,    0.05000,    0.00000, & ! 9   
     &        1.00000,    0.44500,    0.00200,    0.20900,    0.01000, & ! +   
     &        0.30300,    0.03400,    0.00000,    0.51000,    0.21200, & ! O   
     &        0.07200,    0.00000,    0.15800,    0.08800,    0.41100, & ! +   
     &        0.12100,    0.01200,    0.00000,    0.41100,    0.12100, & ! 1   
     &        0.01200,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.10000,    0.00000,    0.03600, & ! 4   
     &        0.00900,    0.11600,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.10600,    0.50200,    0.07400,    0.21600, & ! 5   
     &        0.54100,    1.00000,    0.04200,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.30600,    0.45700,    0.27500,    2.25800,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.06100,    0.00000,    0.48800,    0.07800,    0.16500, & ! 8   
     &        0.08000,    0.20600,    0.10600,    0.00000,    0.00000, & ! +   
     &        2.25800,    0.58200,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.40000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.50400,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.01200,    0.00000,    0.02000,    0.78200,    0.13900, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.40700,    0.65200,    0.00000, & ! +   
     &        0.40700,    0.65200,    0.00000,    0.27400,    0.19000, & ! 8   
     &        0.50000,    0.33300,    0.00000,    0.00000,    0.33000, & ! +   
     &        0.30000,    0.95000,    0.00000,    0.00000,    0.12900, & ! 9   
     &        0.21000,    0.22700,    0.00000,    0.00200,    0.16300, & ! +   
     &        0.01100,    0.06100,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.25000,    0.00000,    0.03100,    0.00000,    0.05000, & ! +   
     &        0.00000,    0.00000,    0.08400,    0.00000,    0.00000, & ! 1   
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
     &        0.00000,    0.00000,    0.00000,    0.10000,    0.00000, & ! +   
     &        0.18500,    0.00000,    0.00000,    1.00000,    0.37500, & ! 7   
     &        0.12000,    0.11500,    0.35700,    0.12200,    0.00000, & ! +   
     &        0.25000,    0.77600,    0.33700,    0.00700,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.57000,    0.15100,    0.38000, & ! +   
     &        0.23800,    0.11200,    0.40500,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.12200,    0.02900,    0.07100,    0.00700, & ! +   
     &        0.08800,    0.77400,    0.00000,    0.14400,    0.00300, & ! O   
     &        0.57900,    0.00000,    0.10000,    0.31200,    0.38500, & ! +   
     &        0.12100,    0.02300,    0.00000,    0.38500,    0.12100, & ! 1   
     &        0.02300,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.06500, & ! 4   
     &        0.01800,    0.54100,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.10400,    0.23700,    0.14700,    0.48400, & ! 5   
     &        0.08200,    0.00000,    0.04200,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.54000,    0.47300,    0.17700,    0.58200,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.31200,    0.00000,    1.00000,    0.34000,    0.00300, & ! 8   
     &        0.25800,    0.07200,    0.19000,    0.00000,    0.00000, & ! +   
     &        0.58200,    0.58200,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.37600,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.01200,    0.00000,    0.07500,    0.02600,    0.56500, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.12900,    0.65200,    0.00000, & ! +   
     &        0.12900,    0.65200,    0.00000,    0.15300,    0.36600, & ! 8   
     &        0.00000,    0.10000,    0.00000,    0.00000,    0.33000, & ! +   
     &        1.00000,    0.35100,    0.00000,    0.00000,    0.01300, & ! 9   
     &        0.02300,    0.21800,    0.00000,    0.21100,    0.78000, & ! +   
     &        0.42900,    0.21400,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.03300,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.01200,    0.00000, & ! +   
     &        0.07500,    0.00000,    0.00000,    0.00000,    0.12500, & ! 7   
     &        0.35500,    0.00100,    1.00000,    0.40000,    0.00000, & ! +   
     &        0.00000,    0.03400,    0.02900,    0.68400,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.02900,    0.18100,    0.15900, & ! +   
     &        0.18500,    0.15900,    0.11200,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.02400,    0.43800,    0.08600,    0.66600, & ! +   
     &        0.50000,    0.16900,    0.00000,    0.08000,    0.00300, & ! O   
     &        0.16300,    0.00000,    0.12300,    0.13400,    0.03700, & ! +   
     &        0.24900,    0.00200,    0.00000,    0.03700,    0.24900, & ! 1   
     &        0.00200,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.07000, & ! 4   
     &        0.01200,    0.00700,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.19700,    0.18600,    0.13900,    0.27400, & ! 5   
     &        0.18000,    0.00000,    0.71200,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.67100,    0.58200,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.34300,    0.03400, & ! 8   
     &        0.04400,    0.21500,    0.38300,    0.00000,    0.00000, & ! +   
     &        0.58200,    0.54800,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.20500,    0.00000,    0.08400,    0.05800,    0.02400, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.10700,    0.65200,    0.00000, & ! +   
     &        0.10700,    0.65200,    0.00000,    0.01900,    0.18400, & ! 8   
     &        0.00000,    0.10000,    0.00000,    0.00000,    0.33000, & ! +   
     &        0.00000,    0.05000,    0.00000,    0.00000,    0.15000, & ! 9   
     &        0.74200,    0.00800,    0.00000,    0.00100,    1.00000, & ! +   
     &        0.00100,    0.23000,    0.00000,    0.00000,    0.00000, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.29000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        1.00000,    0.00100,    1.00000,    0.19200,    0.00000, & ! +   
     &        0.00000,    0.02000,    0.05100,    0.06900,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.26100,    0.06500,    0.04100, & ! +   
     &        0.16100,    0.08800,    0.02200,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.33200,    0.23600,    0.40700,    0.00700, & ! +   
     &        0.01100,    0.83100,    0.00000,    0.00200,    0.29900, & ! O   
     &        0.11600,    0.00000,    0.07200,    0.07700,    0.00700, & ! +   
     &        0.06300,    0.40300,    0.00000,    0.00700,    0.06300, & ! 1   
     &        0.40300,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.84000, & ! 4   
     &        0.05500,    0.02200,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.28500,    0.67600,    0.56500,    0.27400, & ! 5   
     &        0.54100,    0.00000,    0.49800,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.06700,    0.03500,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.07500,    0.28700, & ! 8   
     &        0.04100,    0.01900,    0.31700,    0.00000,    0.00000, & ! +   
     &        0.03500,    0.03500,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.03400,    0.00000,    0.16000,    0.69800,    0.44800, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.26700,    0.00000,    0.00000, & ! +   
     &        0.26700,    0.00000,    0.00000,    0.19500,    0.35000, & ! 8   
     &        0.00000,    0.10000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.05000,    0.00000,    0.00000,    0.33200, & ! 9   
     &        0.10000,    0.57200,    0.00000,    0.08300,    0.00000, & ! +   
     &        0.03600,    0.07400,    0.00000,    0.00000,    0.00000, & ! O   
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
     &        0.00000,    0.75000,    0.00000,    0.20400,    0.00000, & ! +   
     &        0.00000,    0.02300,    0.01700,    0.00200,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.11600,    0.19500,    0.33600, & ! +   
     &        0.04700,    0.04500,    0.03600,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.98300,    0.42600,    1.00000,    0.03600, & ! +   
     &        0.50000,    0.00000,    0.00000,    0.01200,    0.16100, & ! O   
     &        0.00200,    0.00000,    0.18500,    0.02600,    0.00300, & ! +   
     &        0.12700,    0.23900,    0.00000,    0.00300,    0.12700, & ! 1   
     &        0.23900,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 4   
     &        0.15900,    0.23700,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.58600,    0.28000,    0.02400,    0.48400, & ! 5   
     &        0.83500,    0.00000,    0.19500,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.15800,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.25300,    0.41200, & ! 8   
     &        0.37800,    0.03800,    0.08600,    0.00000,    0.00000, & ! +   
     &        0.15800,    0.15800,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.25600,    0.00000,    0.00000,    0.85800,    0.02600, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.78300,    0.00000,    0.00000, & ! +   
     &        0.78300,    0.00000,    0.00000,    0.19500,    0.35000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.05000,    0.00000,    0.00000,    0.15000, & ! 9   
     &        0.37200,    0.85000,    0.00000,    0.14300,    0.00000, & ! +   
     &        0.00400,    0.06300,    0.00000,    0.00000,    0.00000, & ! O   
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
     &        0.00000,    0.25000,    0.00000,    0.39000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.34400,    0.05600,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.31400,    0.19500,    0.14400, & ! +   
     &        0.25300,    0.06700,    0.08800,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.01700,    0.10600,    1.00000,    0.00100, & ! +   
     &        0.04400,    0.00000,    0.00000,    0.02300,    0.13100, & ! O   
     &        0.32000,    0.00000,    0.20200,    0.22100,    0.00900, & ! +   
     &        0.03300,    0.00500,    0.00000,    0.00900,    0.03300, & ! 1   
     &        0.00500,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.54700,    0.10900,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.85500,    0.44800,    0.78400, & ! 5   
     &        0.00000,    0.00000,    0.01700,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.18500,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.98300,    0.24700, & ! 8   
     &        1.00000,    0.19200,    0.04200,    0.00000,    0.00000, & ! +   
     &        0.18500,    0.18500,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.03000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.23100,    0.13900, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.17400, & ! 9   
     &        0.04700,    0.00000,    0.00000,    0.40200,    0.00000, & ! +   
     &        0.01000,    0.00800,    0.00000,    0.00000,    0.00000, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.16000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.24000,    1.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    1.00000,    0.31200,    0.23900, & ! +   
     &        0.25300,    0.27800,    0.35200,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.14600,    0.00000,    0.01200, & ! +   
     &        0.00300,    0.00000,    0.00000,    0.31900,    0.11400, & ! O   
     &        0.31900,    0.00000,    0.30900,    0.24700,    0.00300, & ! +   
     &        0.20800,    0.00100,    0.00000,    0.00300,    0.20800, & ! 1   
     &        0.00100,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.04500,    0.59100,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.02600,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00900,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.27400,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.01700,    0.07600, & ! 8   
     &        0.00000,    0.33700,    0.02500,    0.00000,    0.00000, & ! +   
     &        0.27400,    0.27400,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.25200, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.79400,    0.00300, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.71100, & ! 9   
     &        0.00100,    0.00000,    0.00000,    0.11500,    0.00000, & ! +   
     &        0.17000,    0.12400,    0.00000,    0.00000,    0.00000, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.15000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.34500,    1.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.07300,    0.04700, & ! +   
     &        0.19800,    0.28600,    0.23000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00900, & ! +   
     &        0.00900,    0.00000,    0.00000,    0.68100,    0.45300, & ! O   
     &        0.68100,    0.00000,    0.36900,    0.17800,    0.00200, & ! +   
     &        0.05700,    0.00400,    0.00000,    0.00200,    0.05700, & ! 1   
     &        0.00400,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.30000,    0.05100,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.03000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00900,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00700,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.13000, & ! 8   
     &        0.00000,    0.16900,    0.05800,    0.00000,    0.00000, & ! +   
     &        0.00700,    0.00700,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.07300, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00400, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.04800,    0.00000,    0.00000,    0.32900,    0.00000, & ! +   
     &        0.00800,    0.08300,    0.00000,    0.00000,    0.00000, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.10000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00800,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.43500,    0.55500, & ! +   
     &        0.05500,    0.10200,    0.15100,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.16800, & ! +   
     &        0.18500,    0.00000,    0.00000,    0.00000,    0.07100, & ! O   
     &        0.00000,    0.00000,    1.00000,    0.06800,    0.40900, & ! +   
     &        0.00200,    0.22800,    0.00000,    0.40900,    0.00200, & ! 1   
     &        0.22800,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.02000,    0.04000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.25200,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.11500,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00300,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 8   
     &        0.00000,    0.83100,    0.16100,    0.00000,    0.00000, & ! +   
     &        0.00300,    0.00300,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.07300, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00300, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00700,    0.00000, & ! +   
     &        0.03100,    0.19000,    0.00000,    0.00000,    0.00000, & ! O   
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
     &        0.00000,    0.00000,    0.00000,    0.20000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00200,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.58600,    0.46100,    0.04300,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.16900, & ! +   
     &        0.15900,    0.00000,    0.00000,    0.00000,    0.33300, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.05700,    1.00000, & ! +   
     &        0.17200,    1.00000,    0.00000,    1.00000,    0.17200, & ! 1   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00300,    0.68600,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.07300,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.14000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00300,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.01300,    0.00000,    0.00000, & ! +   
     &        0.00300,    0.00300,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.71300, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.09500, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.52800,    0.00000, & ! +   
     &        0.18900,    0.26100,    0.00000,    0.00000,    0.00000, & ! O   
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
     &        0.00000,    0.00000,    0.08100,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.70500,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.83100, & ! +   
     &        0.26800,    0.00000,    0.00000,    0.00000,    0.01900, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.10100,    1.00000, & ! +   
     &        0.06800,    1.00000,    0.00000,    1.00000,    0.06800, & ! 1   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.04100,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.07300,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.42000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.15800,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.19100,    0.00000,    0.00000, & ! +   
     &        0.15800,    0.15800,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.16300, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.30500,    0.06600,    0.00000,    0.00000,    0.00000, & ! O   
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
     &        0.00000,    0.00000,    0.25500,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.01100,    0.00000,    0.00000,    0.00000,    0.05100, & ! O   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.00300,    0.00000,    0.00000,    0.00000,    0.00300, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.04600,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.71300,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.76200,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00600,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.31900,    0.00000,    0.00000, & ! +   
     &        0.00600,    0.00600,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.16300, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.15700,    0.59100,    0.00000,    0.00000,    0.00000, & ! O   
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
     &        0.00000,    0.00000,    0.73700,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.05200,    0.00000,    0.00000,    0.00000,    0.03300, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.03900,    0.00000,    0.00000,    0.00000,    0.03900, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.54700,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00600,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.68100,    0.00000,    0.00000, & ! +   
     &        0.00600,    0.00600,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.09500, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.63600,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
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
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00100, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00200,    0.00000,    0.00000,    0.00000,    0.00200, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.90800,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00100,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00100,    0.00100,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.26400, & ! 8   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.02400, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00100,    0.00000,    0.00000,    0.00000,    0.00100, & ! 1   
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
     &        0.00000,    0.00000,    0.00000,    0.10900,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.10900,    0.10900,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.06500, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.50200,    0.00000,    0.00000,    0.00000,    0.50200, & ! 1   
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
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.23500, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.42800,    0.00000,    0.00000,    0.00000,    0.42800, & ! 1   
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
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.03700, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 1   
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
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.07300, & ! O   
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
     &        0.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.13600, & ! O   
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
     &        0.00000/           !        3   

      INTEGER            :: NREACT( NRXNS )

      DATA ( NREACT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    1,    1,    1,    2,    1,    1,    1,    1,    1, & ! 1   
     &      1,    2,    1,    2,    2,    2,    2,    1,    2,    2, & ! 2   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    2, & ! 3   
     &      1,    2,    2,    2,    1,    1,    1,    1,    1,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    1,    1,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    1,    1,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    1, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      1,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    1,    2,    2,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    2,    2,    2, & ! 3   
     &      1,    2,    2,    1,    2,    2,    1,    2,    1,    2, & ! 4   
     &      2,    2,    2,    2,    1,    2,    1,    2,    1,    2, & ! 5   
     &      1,    1,    1,    2,    2,    1,    2,    2,    1,    2, & ! 6   
     &      2,    2,    1,    1,    2,    1,    2,    2,    2,    1, & ! 7   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    1, & ! 8   
     &      2,    2,    2,    1,    2,    2,    2,    1,    2,    1, & ! 9   
     &      2,    1,    2,    1,    2,    2,    2,    2,    2,    1, & ! O   
     &      2,    1,    2,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    1,    2,    1, & ! 1   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    2, & ! 2   
     &      1,    1,    1,    2,    2,    1,    2,    2,    1,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    2, & ! 5   
     &      2,    1,    1,    1,    1,    1,    1,    1,    1,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    1, & ! 9   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! O   
     &      2,    1,    2,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    2,    2,    2, & ! 2   
     &      2/     !  3   
      INTEGER            :: NPRDCT( NRXNS )

      DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    1,    0,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    2,    1,    1,    2,    1,    2,    1,    1,    1, & ! 1   
     &      1,    1,    2,    1,    1,    2,    1,    2,    2,    1, & ! 2   
     &      2,    1,    2,    4,    1,    1,    1,    1,    3,    1, & ! 3   
     &      1,    1,    0,    3,    1,    1,    1,    1,    2,    3, & ! 4   
     &      1,    1,    3,    2,    2,    1,    0,    1,    3,    0, & ! 5   
     &      0,    0,    1,    3,    0,    0,    1,    2,    5,    3, & ! 6   
     &      3,    3,    5,    2,    2,    2,    1,    2,    8,    6, & ! 7   
     &      3,    6,    7,    5,    5,    6,    5,    1,    2,    6, & ! 8   
     &      4,    3,    4,    5,    3,    3,    4,    6,    3,    1, & ! 9   
     &      2,    6,    4,    3,    4,    4,    3,    3,    4,    7, & ! O   
     &      5,    3,    1,    2,    1,    1,    3,    1,    0,    1, & ! 1   
     &      0,    1,    0,    1,    0,    1,    0,    1,    0,    1, & ! 2   
     &      0,    1,    0,    1,    0,    2,    1,    2,    3,    1, & ! 3   
     &      3,    2,    6,    6,    2,    4,    3,   10,    5,    2, & ! 4   
     &      2,    6,    7,    3,    3,   12,    3,   11,   12,   17, & ! 5   
     &      6,    2,    2,    3,    4,    3,    2,    3,    1,    5, & ! 6   
     &      2,    1,    1,    0,    1,    0,    2,   12,   10,    7, & ! 7   
     &     12,   10,    1,   14,   21,    8,   11,    6,    1,   10, & ! 8   
     &      9,   12,    2,    4,   14,   15,   12,    7,   17,   10, & ! 9   
     &     20,   19,    1,    3,    2,    8,    7,    9,    1,    8, & ! O   
     &      6,    3,    8,    5,    1,    0,    1,    0,    1,    0, & ! 1   
     &      1,    0,    1,    0,    1,    0,    1,    0,    1,    0, & ! 2   
     &      1,    0,    1,    0,    1,    0,    1,    0,    1,    0, & ! 3   
     &      1,    0,    1,    0,    1,    0,    1,    0,    1,    2, & ! 4   
     &      0,    1,    1,    0,    1,    1,    0,    1,    1,    0, & ! 5   
     &      1,    0,    1,    0,    1,    5,    6,    4,   10,    7, & ! 6   
     &      9,    5,    2,    8,    9,   10,   12,   10,   16,    7, & ! 7   
     &      8,   13,   20,   14,    2,    5,    4,   13,   16,   16, & ! 8   
     &     17,   17,   18,    7,    4,    7,   12,   14,   12,   17, & ! 9   
     &     19,   10,    3,   14,   26,   14,    3,   15,   18,   17, & ! O   
     &     24,   17,    3,   17,   24,   17,    3,    1,    1,    2, & ! 1   
     &      1,    1,    2,    2,    1,    2,    1,    2,    2,    1, & ! 2   
     &      2,    2,    2,    2,    1,    2,    2,    1,    2,    3, & ! 3   
     &      2,    3,    7,    5,   11,   20,   16,    4,    3,    4, & ! 4   
     &      2,   11,   12,   18,   12,   11,    7,   18,    6,    1, & ! 5   
     &      2,    5,    1,    0,    1,    0,    1,    0,    2,    4, & ! 6   
     &      9,    9,   11,   22,    2,    5,    5,    5,    5,    5, & ! 7   
     &      8,    5,    8,   13,   15,   12,   15,   19,    5,    6, & ! 8   
     &     22,   22,    2,    2,    2,    2,    2,    2,    0,    0, & ! 9   
     &      1,    1,    1,    1,    0,    1,    1,    1,    1,    1, & ! O   
     &      1,    0,    1,    1,    2,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    3,    1,    3, & ! 2   
     &      1/     !  3   

      INTEGER, PARAMETER :: NMPHOT =  55
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,   16,   17,   18,   19,   23,   28,   34,   41,   46, & 
     &     69,   79,   90,  102,  136,  137,  141,  144,  147,  149, & 
     &    155,  157,  159,  161,  162,  163,  166,  169,  173,  174, & 
     &    176,  180,  183,  190,  194,  198,  200,  202,  204,  210, & 
     &    212,  214,  318,  320,  323,  324,  331,  332,  336,  359, & 
     &    362,  399,  400,  405,  412/

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   11,   11,   11,   12,   13,   14,   15,   16,   17, & 
     &     18,   18,   18,   18,   19,   20,   21,   22,    1,    1, & 
     &     23,   24,   24,   25,   26,   25,   17,   27,   28,   29, & 
     &     30,   30,   31,   32,   33,   34,   35,   36,   37,   38, & 
     &     39,   12,   13,   14,   29/

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & 
     &     31,   32,   33,   34,   35,   36,   37,   38,   39,   40, & 
     &     41,   42,   43,   44,   45,   46,   47,   48,   49,   50, & 
     &     51,   52,   53,   54,   55/

      INTEGER, PARAMETER :: MHETERO =   4
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    414,  415,  429,  431/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    2,    3,    4/

      INTEGER, PARAMETER :: NPHOTAB =  39
      CHARACTER( 16 )    :: PHOTAB( NPHOTAB )

      DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & 
     &   'NO2_06          ', 'NO3NO_06        ', 'NO3NO2_6        ', & 
     &   'O3O1D_06        ', 'O3O3P_06        ', 'HONO_06         ', & 
     &   'HNO3            ', 'HNO4_06         ', 'H2O2            ', & 
     &   'NO2EX           ', 'PAN             ', 'HCHOR_06        ', & 
     &   'HCHOM_06        ', 'CCHO_R          ', 'C2CHO           ', & 
     &   'ACET_06         ', 'MEK_06          ', 'COOH            ', & 
     &   'GLY_07R         ', 'GLY_07M         ', 'MGLY_06         ', & 
     &   'BACL_07         ', 'BALD_06         ', 'AFG1            ', & 
     &   'MACR_06         ', 'MVK_06          ', 'IC3ONO2         ', & 
     &   'HOCCHO_IUPAC    ', 'ACRO_09         ', 'PAA             ', & 
     &   'CL2             ', 'CLNO_06         ', 'CLONO           ', & 
     &   'CLNO2           ', 'CLONO2_1        ', 'CLONO2_2        ', & 
     &   'HOCL_06         ', 'CLCCHO          ', 'CLACET          '/

      INTEGER, PARAMETER :: NHETERO =   4
      CHARACTER( 16 )    :: HETERO( NHETERO )

      DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & 
     &   'HETERO_N2O5IJ   ', 'HETERO_NO2      ', 'HETERO_PNCOMLI  ', &
     &   'HETERO_PNCOMLJ  '/

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
     &    'EX1             ', 'EX2             ', 'EX3             ', & ! 5   
     &    'EXOH            ', 'BR01            ', 'BR02            ', & ! 6   
     &    'BR03            ', 'BR04            ', 'BR05            ', & ! 7   
     &    'BR06            ', 'BR07            ', 'BR08            ', & ! 8   
     &    'BR09            ', 'BR10            ', 'BR11            ', & ! 9   
     &    'BR12            ', 'BR13            ', 'BR14            ', & ! 0   
     &    'BR15            ', 'BR16            ', 'BR17            ', & ! 1   
     &    'BR18            ', 'BR19            ', 'BR20            ', & ! 2   
     &    'BR21            ', 'BR22            ', 'BR23            ', & ! 3   
     &    'BR24            ', 'BR25            ', 'BR26            ', & ! 4   
     &    'BR27            ', 'BR28            ', 'BR29            ', & ! 5   
     &    'BR30            ', 'BR31            ', 'BR32            ', & ! 6   
     &    'BR33            ', 'BR34            ', 'BR35            ', & ! 7   
     &    'BR36            ', 'BR37            ', 'BR38            ', & ! 8   
     &    'BR39            ', 'BR40            ', 'BR41            ', & ! 9   
     &    'BR42            ', 'BR43            ', 'BR44            ', & ! 0   
     &    'BR45            ', 'BR46            ', 'BR47            ', & ! 1   
     &    'BR48            ', 'BR49            ', 'BR50            ', & ! 2   
     &    'BR51            ', 'BR52            ', 'BR53            ', & ! 3   
     &    'BR54            ', 'BR55            ', 'BR56            ', & ! 4   
     &    'BR57            ', 'BR58            ', 'BR59            ', & ! 5   
     &    'BR60            ', 'BR61            ', 'BR62            ', & ! 6   
     &    'BR63            ', 'BR64            ', 'BR65            ', & ! 7   
     &    'BR66            ', 'BR67            ', 'BR68            ', & ! 8   
     &    'RO01            ', 'RO02            ', 'RO03            ', & ! 9   
     &    'RO04            ', 'RO05            ', 'RO06            ', & ! 0   
     &    'RO07            ', 'RO08            ', 'RO09            ', & ! 1   
     &    'RO10            ', 'RO11            ', 'RO12            ', & ! 2   
     &    'RO13            ', 'RO14            ', 'RO15            ', & ! 3   
     &    'RO16            ', 'RO17            ', 'RO18            ', & ! 4   
     &    'BP01            ', 'BP02            ', 'BP03            ', & ! 5   
     &    'BP07            ', 'BP08            ', 'BP09            ', & ! 6   
     &    'BP10            ', 'BP11            ', 'BP12            ', & ! 7   
     &    'BP13            ', 'BP14            ', 'BP15            ', & ! 8   
     &    'BP16            ', 'BP17            ', 'BP18            ', & ! 9   
     &    'BP19            ', 'BP20            ', 'BP21            ', & ! 0   
     &    'BP22            ', 'BP23            ', 'BP24            ', & ! 1   
     &    'BP25            ', 'BP26            ', 'BP27            ', & ! 2   
     &    'BP28            ', 'BP29            ', 'BP30            ', & ! 3   
     &    'BP31            ', 'BP32            ', 'BP33            ', & ! 4   
     &    'BP34            ', 'BP35            ', 'BP36            ', & ! 5   
     &    'BP37            ', 'BP38            ', 'BP39            ', & ! 6   
     &    'BP40            ', 'BP41            ', 'BP42            ', & ! 7   
     &    'BP43            ', 'BP44            ', 'BP45            ', & ! 8   
     &    'BP46            ', 'BP47            ', 'BP48            ', & ! 9   
     &    'BP49            ', 'BP50            ', 'BP51            ', & ! 0   
     &    'BP52            ', 'BP53            ', 'BP54            ', & ! 1   
     &    'BP55            ', 'BP56            ', 'BP57            ', & ! 2   
     &    'BP58            ', 'BP59            ', 'BP60            ', & ! 3   
     &    'BP62            ', 'BP63            ', 'BP64            ', & ! 4   
     &    'BP65            ', 'BP66            ', 'BP67            ', & ! 5   
     &    'BP68            ', 'BP69            ', 'BP70            ', & ! 6   
     &    'BP71            ', 'BP72            ', 'BP73            ', & ! 7   
     &    'BP74            ', 'BP75            ', 'BP76            ', & ! 8   
     &    'BP77            ', 'BP78            ', 'BP79            ', & ! 9   
     &    'BP80            ', 'BP81            ', 'BP82            ', & ! 0   
     &    'BP83            ', 'PO01            ', 'PO02            ', & ! 1   
     &    'PO03            ', 'PO04            ', 'PO05            ', & ! 2   
     &    'PO06            ', 'PO07            ', 'PO08            ', & ! 3   
     &    'PO09            ', 'PO10            ', 'PO11            ', & ! 4   
     &    'PO12            ', 'PO13            ', 'PO14            ', & ! 5   
     &    'PO15            ', 'PO16            ', 'PO17            ', & ! 6   
     &    'PO18            ', 'PO19            ', 'PO20            ', & ! 7   
     &    'PO21            ', 'PO22            ', 'PO23            ', & ! 8   
     &    'PO24            ', 'PO25            ', 'PO26            ', & ! 9   
     &    'PO27            ', 'PO28            ', 'PO29            ', & ! 0   
     &    'PO30            ', 'PO31            ', 'PO32            ', & ! 1   
     &    'PO33            ', 'PO34            ', 'PO35            ', & ! 2   
     &    'PO36            ', 'PO37            ', 'PO38            ', & ! 3   
     &    'PO39            ', 'PO40            ', 'PO41            ', & ! 4   
     &    'PO42            ', 'PO43            ', 'PO44            ', & ! 5   
     &    'PO45            ', 'PO46            ', 'PO47            ', & ! 6   
     &    'PO48            ', 'PO49            ', 'PO50            ', & ! 7   
     &    'BE01            ', 'BE02            ', 'BE03            ', & ! 8   
     &    'BE04            ', 'BE05            ', 'BT01            ', & ! 9   
     &    'BT02            ', 'BT03            ', 'BT04            ', & ! 0   
     &    'BT05            ', 'BT06            ', 'BT07            ', & ! 1   
     &    'BT08            ', 'BE06            ', 'BE07            ', & ! 2   
     &    'BE08            ', 'BE09            ', 'BT09            ', & ! 3   
     &    'BT10            ', 'BT11            ', 'BT12            ', & ! 4   
     &    'BE10            ', 'BE11            ', 'BE12            ', & ! 5   
     &    'BT13            ', 'BT14            ', 'BT15            ', & ! 6   
     &    'BT16            ', 'BT17            ', 'BT18            ', & ! 7   
     &    'BL01            ', 'BL02            ', 'BL03            ', & ! 8   
     &    'BL04            ', 'BL05            ', 'BL06            ', & ! 9   
     &    'BL07            ', 'BL08            ', 'BL09            ', & ! 0   
     &    'BL10            ', 'BL11            ', 'BL12            ', & ! 1   
     &    'BL13            ', 'BL14            ', 'BL15            ', & ! 2   
     &    'BL16            ', 'BL17            ', 'BL18            ', & ! 3   
     &    'BL19            ', 'BT19            ', 'BT20            ', & ! 4   
     &    'BT21            ', 'BT22            ', 'CI01            ', & ! 5   
     &    'CI02            ', 'CI03            ', 'CI04            ', & ! 6   
     &    'CI05            ', 'CI06            ', 'CI07            ', & ! 7   
     &    'CI08            ', 'CI09            ', 'CI10            ', & ! 8   
     &    'CI11            ', 'CI12            ', 'CI13            ', & ! 9   
     &    'CI14            ', 'CI15            ', 'CI16            ', & ! 0   
     &    'CI17            ', 'CI18            ', 'CI19            ', & ! 1   
     &    'CI20            ', 'CI21            ', 'CI22            ', & ! 2   
     &    'CP01            ', 'CP02            ', 'CP03            ', & ! 3   
     &    'CP04            ', 'CP05            ', 'CP06            ', & ! 4   
     &    'CP07            ', 'CP08            ', 'CP09            ', & ! 5   
     &    'CP10            ', 'CP11            ', 'CP12            ', & ! 6   
     &    'CP13            ', 'CP14            ', 'CP15            ', & ! 7   
     &    'TP01            ', 'CP16            ', 'CP17            ', & ! 8   
     &    'CP18            ', 'CP19            ', 'CP20            ', & ! 9   
     &    'CP21            ', 'CP22            ', 'CP23            ', & ! 0   
     &    'CP24            ', 'CP25            ', 'CP26            ', & ! 1   
     &    'CP27            ', 'CP28            ', 'CE01            ', & ! 2   
     &    'CE02            ', 'TE01            ', 'TE02            ', & ! 3   
     &    'CE03            ', 'TE03            ', 'CE04            ', & ! 4   
     &    'TE04            ', 'TE05            ', 'TE06            ', & ! 5   
     &    'TE07            ', 'TE08            ', 'TE09            ', & ! 6   
     &    'BC01            ', 'BC02            ', 'BC03            ', & ! 7   
     &    'BC04            ', 'BC05            ', 'BC06            ', & ! 8   
     &    'BC07            ', 'BC08            ', 'BC09            ', & ! 9   
     &    'BC10            ', 'BC11            ', 'AE51            ', & ! 0   
     &    'AE52            ', 'AE53            ', 'AE54            ', & ! 1   
     &    'AE55            ', 'AE56            ', 'TR01            ', & ! 2   
     &    'TR02            ', 'TR03            ', 'TR05            ', & ! 3   
     &    'TR06            ', 'TR07            ', 'TR08            ', & ! 4   
     &    'TR09            ', 'TR10            ', 'TR11            ', & ! 5   
     &    'TR12            ', 'TR13            ', 'TR14            ', & ! 6   
     &    'TR15            ', 'TR16            ', 'HET_N2O5        ', & ! 7   
     &    'HET_N02         ', 'OLIG_ALKENE     ', 'OLIG_XYLENE1    ', & ! 8   
     &    'OLIG_XYLENE2    ', 'OLIG_TOLUENE1   ', 'OLIG_TOLUENE2   ', & ! 9   
     &    'OLIG_BENZENE1   ', 'OLIG_BENZENE2   ', 'OLIG_TERPENE1   ', & ! 0   
     &    'OLIG_TERPENE2   ', 'OLIG_ISOPRENE1  ', 'OLIG_ISOPRENE2  ', & ! 1   
     &    'OLIG_SESQT1     ', 'RPOAGEPI        ', 'RPOAGELI        ', & ! 2   
     &    'RPOAGEPJ        ', 'RPOAGELJ        '/                   !    

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

      INTEGER, PARAMETER :: NSPECIAL_RXN =  74
      INTEGER            :: ISPECIAL( NSPECIAL_RXN,2 )

      DATA ( ISPECIAL( IRXXN,1 ), IRXXN = 1, NSPECIAL_RXN ) / & 
     &    118,  119,  120,  121,  122,  123,  124,  125,  126,  127, & ! O   
     &    128,  129,  130,  131,  132,  133,  134,  135,  215,  216, & ! 1   
     &    217,  218,  219,  220,  221,  222,  223,  224,  225,  226, & ! 2   
     &    227,  228,  229,  230,  231,  232,  233,  234,  235,  236, & ! 3   
     &    237,  238,  239,  240,  241,  242,  243,  244,  245,  246, & ! 4   
     &    247,  248,  249,  250,  251,  252,  253,  254,  255,  256, & ! 5   
     &    257,  258,  259,  260,  261,  262,  263,  264,  363,  364, & ! 6   
     &    365,  366,  367,  368/     !  7   

      DATA ( ISPECIAL( IRXXN,2 ), IRXXN = 1, NSPECIAL_RXN ) / & 
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! O   
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! 1   
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! 2   
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! 3   
     &      6,    7,    6,    7,    6,    7,    6,    7,    6,    7, & ! 4   
     &      6,    7,    1,    9,    7,    2,    8,    6,    2,    8, & ! 5   
     &      6,    2,    8,    6,    6,    7,    6,    7,    6,    7, & ! 6   
     &      6,    7,    6,    7/     !  7   

      INTEGER, PARAMETER :: NSPECIAL =   9
      CHARACTER( 16 )    :: SPECIAL( NSPECIAL )

      DATA ( SPECIAL( IRXXN ), IRXXN = 1, NSPECIAL ) / & 
     &   'RO2NO           ', 'RO2HO2          ', 'RO2NO3          ', & 
     &   'RO2RO2          ', 'RO2RO3          ', 'RO2RO           ', & 
     &   'RO2XRO          ', 'RO2RO2M         ', 'RO22NN          '/

      INTEGER, PARAMETER :: MAXSPECTERMS =  10
      REAL( 8 )          :: KC_COEFFS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: INDEX_KTERMS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: INDEX_CTERMS( NSPECIAL + 1, MAXSPECTERMS)
      REAL( 8 )          :: OPERATOR_COEFFS( NSPECIAL + 1, MAXSPECTERMS)
      INTEGER            :: OPERATORS( NSPECIAL + 1, MAXSPECTERMS)

      DATA ( KC_COEFFS(   1,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  1,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     56,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  1,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      2,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( KC_COEFFS(   2,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  2,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     57,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  2,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     11,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( KC_COEFFS(   3,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  3,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     58,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  3,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      5,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( KC_COEFFS(   4,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  4,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     59,   60,   60,    0,    0,    0,    0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  4,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     20,   24,   25,    0,    0,    0,    0,    0,    0,    0/

      DATA ( KC_COEFFS(   5,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  5,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     74,   74,   74,   74,    0,    0,    0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  5,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &     26,   30,   37,   40,    0,    0,    0,    0,    0,    0/

      DATA ( KC_COEFFS(   6,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  6,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  6,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( KC_COEFFS(   7,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  7,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  7,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( KC_COEFFS(   8,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  8,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  8,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( KC_COEFFS(   9,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( INDEX_KTERMS(  9,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( INDEX_CTERMS(  9,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   1,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  1,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   2,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  2,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   3,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  3,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   4,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  4,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   5,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  5,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   6,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 5.0000D-01, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  6,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      1,    3,    5,    4,    0,    0,    0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   7,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  7,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      2,    4,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   8,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     5.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  8,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      4,    0,    0,    0,    0,    0,    0,    0,    0,    0/

      DATA ( OPERATOR_COEFFS(   9,IRXXN ), IRXXN = 1, MAXSPECTERMS ) / & 
     &     1.0000D+00, 1.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/

      DATA ( OPERATORS(  9,  IRXXN), IRXXN = 1, MAXSPECTERMS ) / & 
     &      3,    5,    4,    0,    0,    0,    0,    0,    0,    0/


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
       INTEGER, PARAMETER  :: IJ_NO2EX            =  10
       INTEGER, PARAMETER  :: IJ_PAN              =  11
       INTEGER, PARAMETER  :: IJ_HCHOR_06         =  12
       INTEGER, PARAMETER  :: IJ_HCHOM_06         =  13
       INTEGER, PARAMETER  :: IJ_CCHO_R           =  14
       INTEGER, PARAMETER  :: IJ_C2CHO            =  15
       INTEGER, PARAMETER  :: IJ_ACET_06          =  16
       INTEGER, PARAMETER  :: IJ_MEK_06           =  17
       INTEGER, PARAMETER  :: IJ_COOH             =  18
       INTEGER, PARAMETER  :: IJ_GLY_07R          =  19
       INTEGER, PARAMETER  :: IJ_GLY_07M          =  20
       INTEGER, PARAMETER  :: IJ_MGLY_06          =  21
       INTEGER, PARAMETER  :: IJ_BACL_07          =  22
       INTEGER, PARAMETER  :: IJ_BALD_06          =  23
       INTEGER, PARAMETER  :: IJ_AFG1             =  24
       INTEGER, PARAMETER  :: IJ_MACR_06          =  25
       INTEGER, PARAMETER  :: IJ_MVK_06           =  26
       INTEGER, PARAMETER  :: IJ_IC3ONO2          =  27
       INTEGER, PARAMETER  :: IJ_HOCCHO_IUPAC     =  28
       INTEGER, PARAMETER  :: IJ_ACRO_09          =  29
       INTEGER, PARAMETER  :: IJ_PAA              =  30
       INTEGER, PARAMETER  :: IJ_CL2              =  31
       INTEGER, PARAMETER  :: IJ_CLNO_06          =  32
       INTEGER, PARAMETER  :: IJ_CLONO            =  33
       INTEGER, PARAMETER  :: IJ_CLNO2            =  34
       INTEGER, PARAMETER  :: IJ_CLONO2_1         =  35
       INTEGER, PARAMETER  :: IJ_CLONO2_2         =  36
       INTEGER, PARAMETER  :: IJ_HOCL_06          =  37
       INTEGER, PARAMETER  :: IJ_CLCCHO           =  38
       INTEGER, PARAMETER  :: IJ_CLACET           =  39
       INTEGER, PARAMETER  :: IK_HETERO_N2O5IJ    =   1
       INTEGER, PARAMETER  :: IK_HETERO_NO2       =   2
       INTEGER, PARAMETER  :: IK_HETERO_PNCOMLI   =   3
       INTEGER, PARAMETER  :: IK_HETERO_PNCOMLJ   =   4
       END MODULE RXNS_DATA
