       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: /work/MOD3DEV/has/2021cracmm/cmaq/20211117cmaq/UTIL/chemmech/input/cracmm1_aq/mech_cracmm1_aq.def
! for Mechanism Name: CRACMM1_AQ                      

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

      CHARACTER( 32 ), PARAMETER :: MECHNAME = 'CRACMM1_AQ'

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC = 156
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 179

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

      DATA GAS_CHEM_SPC(   1 ) / 'O3              ' /
      DATA GAS_CHEM_SPC(   2 ) / 'O3P             ' /
      DATA GAS_CHEM_SPC(   3 ) / 'O1D             ' /
      DATA GAS_CHEM_SPC(   4 ) / 'H2O2            ' /
      DATA GAS_CHEM_SPC(   5 ) / 'HO              ' /
      DATA GAS_CHEM_SPC(   6 ) / 'NO2             ' /
      DATA GAS_CHEM_SPC(   7 ) / 'NO              ' /
      DATA GAS_CHEM_SPC(   8 ) / 'NO3             ' /
      DATA GAS_CHEM_SPC(   9 ) / 'HONO            ' /
      DATA GAS_CHEM_SPC(  10 ) / 'HNO3            ' /
      DATA GAS_CHEM_SPC(  11 ) / 'HNO4            ' /
      DATA GAS_CHEM_SPC(  12 ) / 'HO2             ' /
      DATA GAS_CHEM_SPC(  13 ) / 'HCHO            ' /
      DATA GAS_CHEM_SPC(  14 ) / 'CO              ' /
      DATA GAS_CHEM_SPC(  15 ) / 'ACD             ' /
      DATA GAS_CHEM_SPC(  16 ) / 'MO2             ' /
      DATA GAS_CHEM_SPC(  17 ) / 'ALD             ' /
      DATA GAS_CHEM_SPC(  18 ) / 'ETHP            ' /
      DATA GAS_CHEM_SPC(  19 ) / 'ACT             ' /
      DATA GAS_CHEM_SPC(  20 ) / 'ACO3            ' /
      DATA GAS_CHEM_SPC(  21 ) / 'UALD            ' /
      DATA GAS_CHEM_SPC(  22 ) / 'KET             ' /
      DATA GAS_CHEM_SPC(  23 ) / 'MEK             ' /
      DATA GAS_CHEM_SPC(  24 ) / 'HKET            ' /
      DATA GAS_CHEM_SPC(  25 ) / 'MACR            ' /
      DATA GAS_CHEM_SPC(  26 ) / 'MACP            ' /
      DATA GAS_CHEM_SPC(  27 ) / 'XO2             ' /
      DATA GAS_CHEM_SPC(  28 ) / 'MVK             ' /
      DATA GAS_CHEM_SPC(  29 ) / 'GLY             ' /
      DATA GAS_CHEM_SPC(  30 ) / 'MGLY            ' /
      DATA GAS_CHEM_SPC(  31 ) / 'DCB1            ' /
      DATA GAS_CHEM_SPC(  32 ) / 'DCB2            ' /
      DATA GAS_CHEM_SPC(  33 ) / 'BALD            ' /
      DATA GAS_CHEM_SPC(  34 ) / 'CHO             ' /
      DATA GAS_CHEM_SPC(  35 ) / 'OP1             ' /
      DATA GAS_CHEM_SPC(  36 ) / 'OP2             ' /
      DATA GAS_CHEM_SPC(  37 ) / 'PAA             ' /
      DATA GAS_CHEM_SPC(  38 ) / 'ONIT            ' /
      DATA GAS_CHEM_SPC(  39 ) / 'PAN             ' /
      DATA GAS_CHEM_SPC(  40 ) / 'N2O5            ' /
      DATA GAS_CHEM_SPC(  41 ) / 'SO2             ' /
      DATA GAS_CHEM_SPC(  42 ) / 'SULF            ' /
      DATA GAS_CHEM_SPC(  43 ) / 'SULRXN          ' /
      DATA GAS_CHEM_SPC(  44 ) / 'ETH             ' /
      DATA GAS_CHEM_SPC(  45 ) / 'HC3             ' /
      DATA GAS_CHEM_SPC(  46 ) / 'HC3P            ' /
      DATA GAS_CHEM_SPC(  47 ) / 'HC5             ' /
      DATA GAS_CHEM_SPC(  48 ) / 'HC5P            ' /
      DATA GAS_CHEM_SPC(  49 ) / 'HC8             ' /
      DATA GAS_CHEM_SPC(  50 ) / 'HC8P            ' /
      DATA GAS_CHEM_SPC(  51 ) / 'ETE             ' /
      DATA GAS_CHEM_SPC(  52 ) / 'ETEP            ' /
      DATA GAS_CHEM_SPC(  53 ) / 'OLT             ' /
      DATA GAS_CHEM_SPC(  54 ) / 'OLTP            ' /
      DATA GAS_CHEM_SPC(  55 ) / 'OLI             ' /
      DATA GAS_CHEM_SPC(  56 ) / 'OLIP            ' /
      DATA GAS_CHEM_SPC(  57 ) / 'ACE             ' /
      DATA GAS_CHEM_SPC(  58 ) / 'ORA1            ' /
      DATA GAS_CHEM_SPC(  59 ) / 'BEN             ' /
      DATA GAS_CHEM_SPC(  60 ) / 'BENP            ' /
      DATA GAS_CHEM_SPC(  61 ) / 'EPX             ' /
      DATA GAS_CHEM_SPC(  62 ) / 'PHEN            ' /
      DATA GAS_CHEM_SPC(  63 ) / 'BENZRO2         ' /
      DATA GAS_CHEM_SPC(  64 ) / 'TOL             ' /
      DATA GAS_CHEM_SPC(  65 ) / 'TR2             ' /
      DATA GAS_CHEM_SPC(  66 ) / 'TLP1            ' /
      DATA GAS_CHEM_SPC(  67 ) / 'CSL             ' /
      DATA GAS_CHEM_SPC(  68 ) / 'TOLRO2          ' /
      DATA GAS_CHEM_SPC(  69 ) / 'XYM             ' /
      DATA GAS_CHEM_SPC(  70 ) / 'XY2             ' /
      DATA GAS_CHEM_SPC(  71 ) / 'XYL1            ' /
      DATA GAS_CHEM_SPC(  72 ) / 'XYLRO2          ' /
      DATA GAS_CHEM_SPC(  73 ) / 'XYE             ' /
      DATA GAS_CHEM_SPC(  74 ) / 'ISO             ' /
      DATA GAS_CHEM_SPC(  75 ) / 'ISOP            ' /
      DATA GAS_CHEM_SPC(  76 ) / 'ISOPRXN         ' /
      DATA GAS_CHEM_SPC(  77 ) / 'API             ' /
      DATA GAS_CHEM_SPC(  78 ) / 'APIP            ' /
      DATA GAS_CHEM_SPC(  79 ) / 'TRPRXN          ' /
      DATA GAS_CHEM_SPC(  80 ) / 'LIM             ' /
      DATA GAS_CHEM_SPC(  81 ) / 'LIMP            ' /
      DATA GAS_CHEM_SPC(  82 ) / 'RCO3            ' /
      DATA GAS_CHEM_SPC(  83 ) / 'ACTP            ' /
      DATA GAS_CHEM_SPC(  84 ) / 'MEKP            ' /
      DATA GAS_CHEM_SPC(  85 ) / 'KETP            ' /
      DATA GAS_CHEM_SPC(  86 ) / 'MCP             ' /
      DATA GAS_CHEM_SPC(  87 ) / 'MVKP            ' /
      DATA GAS_CHEM_SPC(  88 ) / 'UALP            ' /
      DATA GAS_CHEM_SPC(  89 ) / 'DCB3            ' /
      DATA GAS_CHEM_SPC(  90 ) / 'BALP            ' /
      DATA GAS_CHEM_SPC(  91 ) / 'ADDC            ' /
      DATA GAS_CHEM_SPC(  92 ) / 'MCT             ' /
      DATA GAS_CHEM_SPC(  93 ) / 'MCTO            ' /
      DATA GAS_CHEM_SPC(  94 ) / 'MOH             ' /
      DATA GAS_CHEM_SPC(  95 ) / 'EOH             ' /
      DATA GAS_CHEM_SPC(  96 ) / 'ROH             ' /
      DATA GAS_CHEM_SPC(  97 ) / 'ETEG            ' /
      DATA GAS_CHEM_SPC(  98 ) / 'ISHP            ' /
      DATA GAS_CHEM_SPC(  99 ) / 'IEPOX           ' /
      DATA GAS_CHEM_SPC( 100 ) / 'MAHP            ' /
      DATA GAS_CHEM_SPC( 101 ) / 'ORA2            ' /
      DATA GAS_CHEM_SPC( 102 ) / 'ORAP            ' /
      DATA GAS_CHEM_SPC( 103 ) / 'PPN             ' /
      DATA GAS_CHEM_SPC( 104 ) / 'MPAN            ' /
      DATA GAS_CHEM_SPC( 105 ) / 'NALD            ' /
      DATA GAS_CHEM_SPC( 106 ) / 'ISON            ' /
      DATA GAS_CHEM_SPC( 107 ) / 'MCTP            ' /
      DATA GAS_CHEM_SPC( 108 ) / 'OLNN            ' /
      DATA GAS_CHEM_SPC( 109 ) / 'OLND            ' /
      DATA GAS_CHEM_SPC( 110 ) / 'ADCN            ' /
      DATA GAS_CHEM_SPC( 111 ) / 'TOLP            ' /
      DATA GAS_CHEM_SPC( 112 ) / 'PER1            ' /
      DATA GAS_CHEM_SPC( 113 ) / 'XYLP            ' /
      DATA GAS_CHEM_SPC( 114 ) / 'PER2            ' /
      DATA GAS_CHEM_SPC( 115 ) / 'XYO2            ' /
      DATA GAS_CHEM_SPC( 116 ) / 'XYOP            ' /
      DATA GAS_CHEM_SPC( 117 ) / 'BAL1            ' /
      DATA GAS_CHEM_SPC( 118 ) / 'BAL2            ' /
      DATA GAS_CHEM_SPC( 119 ) / 'TOLNRXN         ' /
      DATA GAS_CHEM_SPC( 120 ) / 'TOLHRXN         ' /
      DATA GAS_CHEM_SPC( 121 ) / 'XYLNRXN         ' /
      DATA GAS_CHEM_SPC( 122 ) / 'XYLHRXN         ' /
      DATA GAS_CHEM_SPC( 123 ) / 'BNZNRXN         ' /
      DATA GAS_CHEM_SPC( 124 ) / 'BNZHRXN         ' /
      DATA GAS_CHEM_SPC( 125 ) / 'SOAALK          ' /
      DATA GAS_CHEM_SPC( 126 ) / 'ALKRXN          ' /
      DATA GAS_CHEM_SPC( 127 ) / 'ROCIOXY         ' /
      DATA GAS_CHEM_SPC( 128 ) / 'SLOWROC         ' /
      DATA GAS_CHEM_SPC( 129 ) / 'NAPH            ' /
      DATA GAS_CHEM_SPC( 130 ) / 'PAHRO2          ' /
      DATA GAS_CHEM_SPC( 131 ) / 'PAHNRXN         ' /
      DATA GAS_CHEM_SPC( 132 ) / 'PAHHRXN         ' /
      DATA GAS_CHEM_SPC( 133 ) / 'ACRO            ' /
      DATA GAS_CHEM_SPC( 134 ) / 'BDE13           ' /
      DATA GAS_CHEM_SPC( 135 ) / 'FURAN           ' /
      DATA GAS_CHEM_SPC( 136 ) / 'FURANO2         ' /
      DATA GAS_CHEM_SPC( 137 ) / 'FURANONE        ' /
      DATA GAS_CHEM_SPC( 138 ) / 'PROG            ' /
      DATA GAS_CHEM_SPC( 139 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC( 140 ) / 'SESQNRO2        ' /
      DATA GAS_CHEM_SPC( 141 ) / 'VROCP0OXY       ' /
      DATA GAS_CHEM_SPC( 142 ) / 'VROCP3OXY       ' /
      DATA GAS_CHEM_SPC( 143 ) / 'VROCN2OXY       ' /
      DATA GAS_CHEM_SPC( 144 ) / 'SESQRO2         ' /
      DATA GAS_CHEM_SPC( 145 ) / 'PCVOC           ' /
      DATA GAS_CHEM_SPC( 146 ) / 'PCSOARXN        ' /
      DATA GAS_CHEM_SPC( 147 ) / 'VLVPO1          ' /
      DATA GAS_CHEM_SPC( 148 ) / 'VSVPO1          ' /
      DATA GAS_CHEM_SPC( 149 ) / 'VSVPO2          ' /
      DATA GAS_CHEM_SPC( 150 ) / 'VSVPO3          ' /
      DATA GAS_CHEM_SPC( 151 ) / 'VIVPO1          ' /
      DATA GAS_CHEM_SPC( 152 ) / 'VLVOO1          ' /
      DATA GAS_CHEM_SPC( 153 ) / 'VLVOO2          ' /
      DATA GAS_CHEM_SPC( 154 ) / 'VSVOO2          ' /
      DATA GAS_CHEM_SPC( 155 ) / 'VSVOO3          ' /
      DATA GAS_CHEM_SPC( 156 ) / 'VSVOO1          ' /




      LOGICAL   :: HALOGEN_PARAMETER = .TRUE. 


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
      & MEMBER("O3              ",    1, "GC",   48.00D0, F), &
      & MEMBER("O3P             ",    2, "GC",   16.00D0, F), &
      & MEMBER("O1D             ",    3, "GC",   16.00D0, F), &
      & MEMBER("H2O2            ",    4, "GC",   34.00D0, F), &
      & MEMBER("HO              ",    5, "GC",   17.00D0, F), &
      & MEMBER("NO2             ",    6, "GC",   46.00D0, F), &
      & MEMBER("NO              ",    7, "GC",   30.00D0, F), &
      & MEMBER("NO3             ",    8, "GC",   62.00D0, F), &
      & MEMBER("HONO            ",    9, "GC",   47.00D0, F), &
      & MEMBER("HNO3            ",   10, "GC",   63.00D0, F), &
      & MEMBER("HNO4            ",   11, "GC",   79.00D0, F), &
      & MEMBER("HO2             ",   12, "GC",   33.00D0, F), &
      & MEMBER("HCHO            ",   13, "GC",   30.00D0, F), &
      & MEMBER("CO              ",   14, "GC",   28.00D0, F), &
      & MEMBER("ACD             ",   15, "GC",   44.00D0, F), &
      & MEMBER("MO2             ",   16, "GC",   47.00D0, F), &
      & MEMBER("ALD             ",   17, "GC",   58.00D0, F), &
      & MEMBER("ETHP            ",   18, "GC",   61.00D0, F), &
      & MEMBER("ACT             ",   19, "GC",   58.00D0, F), &
      & MEMBER("ACO3            ",   20, "GC",   75.00D0, F), &
      & MEMBER("UALD            ",   21, "GC",   84.10D0, F), &
      & MEMBER("KET             ",   22, "GC",   86.00D0, F), &
      & MEMBER("MEK             ",   23, "GC",   72.10D0, F), &
      & MEMBER("HKET            ",   24, "GC",   74.00D0, F), &
      & MEMBER("MACR            ",   25, "GC",   70.00D0, F), &
      & MEMBER("MACP            ",   26, "GC",  101.00D0, F), &
      & MEMBER("XO2             ",   27, "GC",    1.00D0, F), &
      & MEMBER("MVK             ",   28, "GC",   70.10D0, F), &
      & MEMBER("GLY             ",   29, "GC",   58.00D0, F), &
      & MEMBER("MGLY            ",   30, "GC",   72.00D0, F), &
      & MEMBER("DCB1            ",   31, "GC",   91.00D0, F), &
      & MEMBER("DCB2            ",   32, "GC",  110.00D0, F), &
      & MEMBER("BALD            ",   33, "GC",  106.00D0, F), &
      & MEMBER("CHO             ",   34, "GC",  139.00D0, F), &
      & MEMBER("OP1             ",   35, "GC",   48.00D0, F), &
      & MEMBER("OP2             ",   36, "GC",   62.00D0, F), &
      & MEMBER("PAA             ",   37, "GC",   76.00D0, F), &
      & MEMBER("ONIT            ",   38, "GC",  119.00D0, F), &
      & MEMBER("PAN             ",   39, "GC",  121.00D0, F), &
      & MEMBER("N2O5            ",   40, "GC",  108.00D0, F), &
      & MEMBER("SO2             ",   41, "GC",   64.00D0, F), &
      & MEMBER("SULF            ",   42, "GC",   98.00D0, F), &
      & MEMBER("SULRXN          ",   43, "GC",   98.00D0, F), &
      & MEMBER("ETH             ",   44, "GC",   30.10D0, F), &
      & MEMBER("HC3             ",   45, "GC",   51.90D0, F), &
      & MEMBER("HC3P            ",   46, "GC",   75.00D0, F), &
      & MEMBER("ASOATJ          ",  169, "AE",  200.00D0, T), &
      & MEMBER("HC5             ",   47, "GC",   84.30D0, F), &
      & MEMBER("HC5P            ",   48, "GC",  103.00D0, F), &
      & MEMBER("HC8             ",   49, "GC",  136.20D0, F), &
      & MEMBER("HC8P            ",   50, "GC",  145.00D0, F), &
      & MEMBER("ETE             ",   51, "GC",   28.10D0, F), &
      & MEMBER("ETEP            ",   52, "GC",   77.00D0, F), &
      & MEMBER("OLT             ",   53, "GC",   42.00D0, F), &
      & MEMBER("OLTP            ",   54, "GC",   91.00D0, F), &
      & MEMBER("OLI             ",   55, "GC",   68.00D0, F), &
      & MEMBER("OLIP            ",   56, "GC",  117.00D0, F), &
      & MEMBER("ACE             ",   57, "GC",   26.00D0, F), &
      & MEMBER("ORA1            ",   58, "GC",   46.00D0, F), &
      & MEMBER("BEN             ",   59, "GC",   78.10D0, F), &
      & MEMBER("BENP            ",   60, "GC",  127.00D0, F), &
      & MEMBER("EPX             ",   61, "GC",  122.50D0, F), &
      & MEMBER("PHEN            ",   62, "GC",  105.50D0, F), &
      & MEMBER("BENZRO2         ",   63, "GC",  127.00D0, F), &
      & MEMBER("TOL             ",   64, "GC",   92.10D0, F), &
      & MEMBER("TR2             ",   65, "GC",  109.00D0, F), &
      & MEMBER("TLP1            ",   66, "GC",   91.00D0, F), &
      & MEMBER("CSL             ",   67, "GC",  129.40D0, F), &
      & MEMBER("TOLRO2          ",   68, "GC",  141.00D0, F), &
      & MEMBER("XYM             ",   69, "GC",  112.00D0, F), &
      & MEMBER("XY2             ",   70, "GC",  124.00D0, F), &
      & MEMBER("XYL1            ",   71, "GC",  156.00D0, F), &
      & MEMBER("XYLRO2          ",   72, "GC",  155.00D0, F), &
      & MEMBER("XYE             ",   73, "GC",  115.00D0, F), &
      & MEMBER("ISO             ",   75, "GC",   68.10D0, F), &
      & MEMBER("ISOP            ",   76, "GC",  117.00D0, F), &
      & MEMBER("ISOPRXN         ",   77, "GC",   68.00D0, F), &
      & MEMBER("API             ",   78, "GC",  136.40D0, F), &
      & MEMBER("APIP            ",   79, "GC",  185.00D0, F), &
      & MEMBER("TRPRXN          ",   80, "GC",  136.00D0, F), &
      & MEMBER("LIM             ",   81, "GC",  136.30D0, F), &
      & MEMBER("LIMP            ",   82, "GC",  185.00D0, F), &
      & MEMBER("RCO3            ",   83, "GC",   90.00D0, F), &
      & MEMBER("ACTP            ",   84, "GC",   89.00D0, F), &
      & MEMBER("MEKP            ",   85, "GC",  103.00D0, F), &
      & MEMBER("KETP            ",   86, "GC",  117.00D0, F), &
      & MEMBER("MCP             ",   87, "GC",  119.00D0, F), &
      & MEMBER("MVKP            ",   88, "GC",  119.00D0, F), &
      & MEMBER("UALP            ",   89, "GC",  133.00D0, F), &
      & MEMBER("DCB3            ",   90, "GC",   84.00D0, F), &
      & MEMBER("BALP            ",   91, "GC",  137.00D0, F), &
      & MEMBER("ADDC            ",   92, "GC",  125.00D0, F), &
      & MEMBER("MCT             ",   93, "GC",  124.10D0, F), &
      & MEMBER("MCTO            ",   94, "GC",  123.00D0, F), &
      & MEMBER("MOH             ",   95, "GC",   32.00D0, F), &
      & MEMBER("EOH             ",   96, "GC",   46.10D0, F), &
      & MEMBER("ROH             ",   97, "GC",   60.00D0, F), &
      & MEMBER("ETEG            ",   98, "GC",   62.10D0, F), &
      & MEMBER("ISHP            ",   99, "GC",  118.00D0, F), &
      & MEMBER("IEPOX           ",  100, "GC",  118.10D0, F), &
      & MEMBER("MAHP            ",  101, "GC",  102.00D0, F), &
      & MEMBER("ORA2            ",  102, "GC",   60.20D0, F), &
      & MEMBER("ORAP            ",  103, "GC",  109.00D0, F), &
      & MEMBER("PPN             ",  104, "GC",  135.00D0, F), &
      & MEMBER("MPAN            ",  105, "GC",  148.00D0, F), &
      & MEMBER("NALD            ",  106, "GC",  105.00D0, F), &
      & MEMBER("ISON            ",  107, "GC",  147.00D0, F), &
      & MEMBER("MCTP            ",  108, "GC",  172.00D0, F), &
      & MEMBER("OLNN            ",  109, "GC",  136.00D0, F), &
      & MEMBER("OLND            ",  110, "GC",  136.00D0, F), &
      & MEMBER("ADCN            ",  111, "GC",  156.00D0, F), &
      & MEMBER("TOLP            ",  112, "GC",  141.00D0, F), &
      & MEMBER("PER1            ",  113, "GC",  141.00D0, F), &
      & MEMBER("XYLP            ",  114, "GC",  155.00D0, F), &
      & MEMBER("PER2            ",  115, "GC",  157.00D0, F), &
      & MEMBER("XYO2            ",   74, "GC",  155.00D0, F), &
      & MEMBER("XYOP            ",  116, "GC",  155.00D0, F), &
      & MEMBER("BAL1            ",  117, "GC",  121.00D0, F), &
      & MEMBER("BAL2            ",  118, "GC",  105.00D0, F), &
      & MEMBER("TOLNRXN         ",  127, "GC",  141.00D0, F), &
      & MEMBER("TOLHRXN         ",  128, "GC",  141.00D0, F), &
      & MEMBER("XYLNRXN         ",  129, "GC",  155.00D0, F), &
      & MEMBER("XYLHRXN         ",  130, "GC",  155.00D0, F), &
      & MEMBER("BNZNRXN         ",  131, "GC",  127.00D0, F), &
      & MEMBER("BNZHRXN         ",  132, "GC",  127.00D0, F), &
      & MEMBER("SOAALK          ",  140, "GC",  112.00D0, F), &
      & MEMBER("ALKRXN          ",  141, "GC",  112.00D0, F), &
      & MEMBER("ROCIOXY         ",  125, "GC",  247.00D0, F), &
      & MEMBER("SLOWROC         ",  126, "GC",   75.40D0, F), &
      & MEMBER("NAPH            ",  136, "GC",  133.10D0, F), &
      & MEMBER("PAHRO2          ",  137, "GC",  187.20D0, F), &
      & MEMBER("PAHNRXN         ",  138, "GC",  187.20D0, F), &
      & MEMBER("PAHHRXN         ",  139, "GC",  187.20D0, F), &
      & MEMBER("ACRO            ",  119, "GC",   56.10D0, F), &
      & MEMBER("BDE13           ",  120, "GC",   54.10D0, F), &
      & MEMBER("FURAN           ",  122, "GC",   96.10D0, F), &
      & MEMBER("FURANO2         ",  123, "GC",  145.10D0, F), &
      & MEMBER("FURANONE        ",  124, "GC",  100.10D0, F), &
      & MEMBER("PROG            ",  121, "GC",   76.10D0, F), &
      & MEMBER("SESQ            ",  133, "GC",  204.40D0, F), &
      & MEMBER("SESQNRO2        ",  135, "GC",  298.40D0, F), &
      & MEMBER("VROCP0OXY       ",  158, "GC",  135.00D0, F), &
      & MEMBER("VROCP3OXY       ",  161, "GC",  134.00D0, F), &
      & MEMBER("VROCN2OXY       ",  156, "GC",  136.00D0, F), &
      & MEMBER("SESQRO2         ",  134, "GC",  253.40D0, F), &
      & MEMBER("AGLYJ           ",  170, "AE",   66.40D0, T), &
      & MEMBER("AISO3J          ",  226, "AE",  168.20D0, T), &
      & MEMBER("AXYL1J          ",  173, "AE",  174.00D0, T), &
      & MEMBER("AOLGAJ          ",  227, "AE",  206.00D0, T), &
      & MEMBER("AXYL2J          ",  174, "AE",  185.00D0, T), &
      & MEMBER("ATOL1J          ",  176, "AE",  163.00D0, T), &
      & MEMBER("ATOL2J          ",  177, "AE",  175.00D0, T), &
      & MEMBER("ABNZ1J          ",  179, "AE",  161.00D0, T), &
      & MEMBER("ABNZ2J          ",  180, "AE",  134.00D0, T), &
      & MEMBER("ATRP1J          ",  185, "AE",  177.00D0, T), &
      & MEMBER("AOLGBJ          ",  228, "AE",  248.00D0, T), &
      & MEMBER("ATRP2J          ",  186, "AE",  198.00D0, T), &
      & MEMBER("AISO1J          ",  187, "AE",  132.00D0, T), &
      & MEMBER("AISO2J          ",  188, "AE",  133.00D0, T), &
      & MEMBER("APAH1J          ",  182, "AE",  195.60D0, T), &
      & MEMBER("APAH2J          ",  183, "AE",  178.70D0, T), &
      & MEMBER("AALK1J          ",  171, "AE",  225.00D0, T), &
      & MEMBER("AALK2J          ",  172, "AE",  205.10D0, T), &
      & MEMBER("APOCI           ",  229, "AE",  220.00D0, T), &
      & MEMBER("APNCOMI         ",  231, "AE",  220.00D0, T), &
      & MEMBER("APOCJ           ",  230, "AE",  220.00D0, T), &
      & MEMBER("APNCOMJ         ",  232, "AE",  220.00D0, T), &
      & MEMBER("PCVOC           ",  152, "GC",  170.00D0, F), &
      & MEMBER("PCSOARXN        ",  153, "GC",  170.00D0, F), &
      & MEMBER("VLVPO1          ",  142, "GC",  218.00D0, F), &
      & MEMBER("VSVPO1          ",  143, "GC",  230.00D0, F), &
      & MEMBER("VSVPO2          ",  144, "GC",  241.00D0, F), &
      & MEMBER("VSVPO3          ",  145, "GC",  253.00D0, F), &
      & MEMBER("VIVPO1          ",  146, "GC",  266.00D0, F), &
      & MEMBER("VLVOO1          ",  147, "GC",  136.00D0, F), &
      & MEMBER("VLVOO2          ",  148, "GC",  136.00D0, F), &
      & MEMBER("VSVOO2          ",  150, "GC",  135.00D0, F), &
      & MEMBER("VSVOO3          ",  151, "GC",  134.00D0, F), &
      & MEMBER("VSVOO1          ",  149, "GC",  135.00D0, F) /)

      DATA CHEMISTRY_SPC(   1 ), SPECIES_MOLWT(   1 ) / 'O3              ',   48.00D0 /
      DATA CHEMISTRY_SPC(   2 ), SPECIES_MOLWT(   2 ) / 'O3P             ',   16.00D0 /
      DATA CHEMISTRY_SPC(   3 ), SPECIES_MOLWT(   3 ) / 'O1D             ',   16.00D0 /
      DATA CHEMISTRY_SPC(   4 ), SPECIES_MOLWT(   4 ) / 'H2O2            ',   34.00D0 /
      DATA CHEMISTRY_SPC(   5 ), SPECIES_MOLWT(   5 ) / 'HO              ',   17.00D0 /
      DATA CHEMISTRY_SPC(   6 ), SPECIES_MOLWT(   6 ) / 'NO2             ',   46.00D0 /
      DATA CHEMISTRY_SPC(   7 ), SPECIES_MOLWT(   7 ) / 'NO              ',   30.00D0 /
      DATA CHEMISTRY_SPC(   8 ), SPECIES_MOLWT(   8 ) / 'NO3             ',   62.00D0 /
      DATA CHEMISTRY_SPC(   9 ), SPECIES_MOLWT(   9 ) / 'HONO            ',   47.00D0 /
      DATA CHEMISTRY_SPC(  10 ), SPECIES_MOLWT(  10 ) / 'HNO3            ',   63.00D0 /
      DATA CHEMISTRY_SPC(  11 ), SPECIES_MOLWT(  11 ) / 'HNO4            ',   79.00D0 /
      DATA CHEMISTRY_SPC(  12 ), SPECIES_MOLWT(  12 ) / 'HO2             ',   33.00D0 /
      DATA CHEMISTRY_SPC(  13 ), SPECIES_MOLWT(  13 ) / 'HCHO            ',   30.00D0 /
      DATA CHEMISTRY_SPC(  14 ), SPECIES_MOLWT(  14 ) / 'CO              ',   28.00D0 /
      DATA CHEMISTRY_SPC(  15 ), SPECIES_MOLWT(  15 ) / 'ACD             ',   44.00D0 /
      DATA CHEMISTRY_SPC(  16 ), SPECIES_MOLWT(  16 ) / 'MO2             ',   47.00D0 /
      DATA CHEMISTRY_SPC(  17 ), SPECIES_MOLWT(  17 ) / 'ALD             ',   58.00D0 /
      DATA CHEMISTRY_SPC(  18 ), SPECIES_MOLWT(  18 ) / 'ETHP            ',   61.00D0 /
      DATA CHEMISTRY_SPC(  19 ), SPECIES_MOLWT(  19 ) / 'ACT             ',   58.00D0 /
      DATA CHEMISTRY_SPC(  20 ), SPECIES_MOLWT(  20 ) / 'ACO3            ',   75.00D0 /
      DATA CHEMISTRY_SPC(  21 ), SPECIES_MOLWT(  21 ) / 'UALD            ',   84.10D0 /
      DATA CHEMISTRY_SPC(  22 ), SPECIES_MOLWT(  22 ) / 'KET             ',   86.00D0 /
      DATA CHEMISTRY_SPC(  23 ), SPECIES_MOLWT(  23 ) / 'MEK             ',   72.10D0 /
      DATA CHEMISTRY_SPC(  24 ), SPECIES_MOLWT(  24 ) / 'HKET            ',   74.00D0 /
      DATA CHEMISTRY_SPC(  25 ), SPECIES_MOLWT(  25 ) / 'MACR            ',   70.00D0 /
      DATA CHEMISTRY_SPC(  26 ), SPECIES_MOLWT(  26 ) / 'MACP            ',  101.00D0 /
      DATA CHEMISTRY_SPC(  27 ), SPECIES_MOLWT(  27 ) / 'XO2             ',    1.00D0 /
      DATA CHEMISTRY_SPC(  28 ), SPECIES_MOLWT(  28 ) / 'MVK             ',   70.10D0 /
      DATA CHEMISTRY_SPC(  29 ), SPECIES_MOLWT(  29 ) / 'GLY             ',   58.00D0 /
      DATA CHEMISTRY_SPC(  30 ), SPECIES_MOLWT(  30 ) / 'MGLY            ',   72.00D0 /
      DATA CHEMISTRY_SPC(  31 ), SPECIES_MOLWT(  31 ) / 'DCB1            ',   91.00D0 /
      DATA CHEMISTRY_SPC(  32 ), SPECIES_MOLWT(  32 ) / 'DCB2            ',  110.00D0 /
      DATA CHEMISTRY_SPC(  33 ), SPECIES_MOLWT(  33 ) / 'BALD            ',  106.00D0 /
      DATA CHEMISTRY_SPC(  34 ), SPECIES_MOLWT(  34 ) / 'CHO             ',  139.00D0 /
      DATA CHEMISTRY_SPC(  35 ), SPECIES_MOLWT(  35 ) / 'OP1             ',   48.00D0 /
      DATA CHEMISTRY_SPC(  36 ), SPECIES_MOLWT(  36 ) / 'OP2             ',   62.00D0 /
      DATA CHEMISTRY_SPC(  37 ), SPECIES_MOLWT(  37 ) / 'PAA             ',   76.00D0 /
      DATA CHEMISTRY_SPC(  38 ), SPECIES_MOLWT(  38 ) / 'ONIT            ',  119.00D0 /
      DATA CHEMISTRY_SPC(  39 ), SPECIES_MOLWT(  39 ) / 'PAN             ',  121.00D0 /
      DATA CHEMISTRY_SPC(  40 ), SPECIES_MOLWT(  40 ) / 'N2O5            ',  108.00D0 /
      DATA CHEMISTRY_SPC(  41 ), SPECIES_MOLWT(  41 ) / 'SO2             ',   64.00D0 /
      DATA CHEMISTRY_SPC(  42 ), SPECIES_MOLWT(  42 ) / 'SULF            ',   98.00D0 /
      DATA CHEMISTRY_SPC(  43 ), SPECIES_MOLWT(  43 ) / 'SULRXN          ',   98.00D0 /
      DATA CHEMISTRY_SPC(  44 ), SPECIES_MOLWT(  44 ) / 'ETH             ',   30.10D0 /
      DATA CHEMISTRY_SPC(  45 ), SPECIES_MOLWT(  45 ) / 'HC3             ',   51.90D0 /
      DATA CHEMISTRY_SPC(  46 ), SPECIES_MOLWT(  46 ) / 'HC3P            ',   75.00D0 /
      DATA CHEMISTRY_SPC(  47 ), SPECIES_MOLWT(  47 ) / 'ASOATJ          ',  200.00D0 /
      DATA CHEMISTRY_SPC(  48 ), SPECIES_MOLWT(  48 ) / 'HC5             ',   84.30D0 /
      DATA CHEMISTRY_SPC(  49 ), SPECIES_MOLWT(  49 ) / 'HC5P            ',  103.00D0 /
      DATA CHEMISTRY_SPC(  50 ), SPECIES_MOLWT(  50 ) / 'HC8             ',  136.20D0 /
      DATA CHEMISTRY_SPC(  51 ), SPECIES_MOLWT(  51 ) / 'HC8P            ',  145.00D0 /
      DATA CHEMISTRY_SPC(  52 ), SPECIES_MOLWT(  52 ) / 'ETE             ',   28.10D0 /
      DATA CHEMISTRY_SPC(  53 ), SPECIES_MOLWT(  53 ) / 'ETEP            ',   77.00D0 /
      DATA CHEMISTRY_SPC(  54 ), SPECIES_MOLWT(  54 ) / 'OLT             ',   42.00D0 /
      DATA CHEMISTRY_SPC(  55 ), SPECIES_MOLWT(  55 ) / 'OLTP            ',   91.00D0 /
      DATA CHEMISTRY_SPC(  56 ), SPECIES_MOLWT(  56 ) / 'OLI             ',   68.00D0 /
      DATA CHEMISTRY_SPC(  57 ), SPECIES_MOLWT(  57 ) / 'OLIP            ',  117.00D0 /
      DATA CHEMISTRY_SPC(  58 ), SPECIES_MOLWT(  58 ) / 'ACE             ',   26.00D0 /
      DATA CHEMISTRY_SPC(  59 ), SPECIES_MOLWT(  59 ) / 'ORA1            ',   46.00D0 /
      DATA CHEMISTRY_SPC(  60 ), SPECIES_MOLWT(  60 ) / 'BEN             ',   78.10D0 /
      DATA CHEMISTRY_SPC(  61 ), SPECIES_MOLWT(  61 ) / 'BENP            ',  127.00D0 /
      DATA CHEMISTRY_SPC(  62 ), SPECIES_MOLWT(  62 ) / 'EPX             ',  122.50D0 /
      DATA CHEMISTRY_SPC(  63 ), SPECIES_MOLWT(  63 ) / 'PHEN            ',  105.50D0 /
      DATA CHEMISTRY_SPC(  64 ), SPECIES_MOLWT(  64 ) / 'BENZRO2         ',  127.00D0 /
      DATA CHEMISTRY_SPC(  65 ), SPECIES_MOLWT(  65 ) / 'TOL             ',   92.10D0 /
      DATA CHEMISTRY_SPC(  66 ), SPECIES_MOLWT(  66 ) / 'TR2             ',  109.00D0 /
      DATA CHEMISTRY_SPC(  67 ), SPECIES_MOLWT(  67 ) / 'TLP1            ',   91.00D0 /
      DATA CHEMISTRY_SPC(  68 ), SPECIES_MOLWT(  68 ) / 'CSL             ',  129.40D0 /
      DATA CHEMISTRY_SPC(  69 ), SPECIES_MOLWT(  69 ) / 'TOLRO2          ',  141.00D0 /
      DATA CHEMISTRY_SPC(  70 ), SPECIES_MOLWT(  70 ) / 'XYM             ',  112.00D0 /
      DATA CHEMISTRY_SPC(  71 ), SPECIES_MOLWT(  71 ) / 'XY2             ',  124.00D0 /
      DATA CHEMISTRY_SPC(  72 ), SPECIES_MOLWT(  72 ) / 'XYL1            ',  156.00D0 /
      DATA CHEMISTRY_SPC(  73 ), SPECIES_MOLWT(  73 ) / 'XYLRO2          ',  155.00D0 /
      DATA CHEMISTRY_SPC(  74 ), SPECIES_MOLWT(  74 ) / 'XYE             ',  115.00D0 /
      DATA CHEMISTRY_SPC(  75 ), SPECIES_MOLWT(  75 ) / 'ISO             ',   68.10D0 /
      DATA CHEMISTRY_SPC(  76 ), SPECIES_MOLWT(  76 ) / 'ISOP            ',  117.00D0 /
      DATA CHEMISTRY_SPC(  77 ), SPECIES_MOLWT(  77 ) / 'ISOPRXN         ',   68.00D0 /
      DATA CHEMISTRY_SPC(  78 ), SPECIES_MOLWT(  78 ) / 'API             ',  136.40D0 /
      DATA CHEMISTRY_SPC(  79 ), SPECIES_MOLWT(  79 ) / 'APIP            ',  185.00D0 /
      DATA CHEMISTRY_SPC(  80 ), SPECIES_MOLWT(  80 ) / 'TRPRXN          ',  136.00D0 /
      DATA CHEMISTRY_SPC(  81 ), SPECIES_MOLWT(  81 ) / 'LIM             ',  136.30D0 /
      DATA CHEMISTRY_SPC(  82 ), SPECIES_MOLWT(  82 ) / 'LIMP            ',  185.00D0 /
      DATA CHEMISTRY_SPC(  83 ), SPECIES_MOLWT(  83 ) / 'RCO3            ',   90.00D0 /
      DATA CHEMISTRY_SPC(  84 ), SPECIES_MOLWT(  84 ) / 'ACTP            ',   89.00D0 /
      DATA CHEMISTRY_SPC(  85 ), SPECIES_MOLWT(  85 ) / 'MEKP            ',  103.00D0 /
      DATA CHEMISTRY_SPC(  86 ), SPECIES_MOLWT(  86 ) / 'KETP            ',  117.00D0 /
      DATA CHEMISTRY_SPC(  87 ), SPECIES_MOLWT(  87 ) / 'MCP             ',  119.00D0 /
      DATA CHEMISTRY_SPC(  88 ), SPECIES_MOLWT(  88 ) / 'MVKP            ',  119.00D0 /
      DATA CHEMISTRY_SPC(  89 ), SPECIES_MOLWT(  89 ) / 'UALP            ',  133.00D0 /
      DATA CHEMISTRY_SPC(  90 ), SPECIES_MOLWT(  90 ) / 'DCB3            ',   84.00D0 /
      DATA CHEMISTRY_SPC(  91 ), SPECIES_MOLWT(  91 ) / 'BALP            ',  137.00D0 /
      DATA CHEMISTRY_SPC(  92 ), SPECIES_MOLWT(  92 ) / 'ADDC            ',  125.00D0 /
      DATA CHEMISTRY_SPC(  93 ), SPECIES_MOLWT(  93 ) / 'MCT             ',  124.10D0 /
      DATA CHEMISTRY_SPC(  94 ), SPECIES_MOLWT(  94 ) / 'MCTO            ',  123.00D0 /
      DATA CHEMISTRY_SPC(  95 ), SPECIES_MOLWT(  95 ) / 'MOH             ',   32.00D0 /
      DATA CHEMISTRY_SPC(  96 ), SPECIES_MOLWT(  96 ) / 'EOH             ',   46.10D0 /
      DATA CHEMISTRY_SPC(  97 ), SPECIES_MOLWT(  97 ) / 'ROH             ',   60.00D0 /
      DATA CHEMISTRY_SPC(  98 ), SPECIES_MOLWT(  98 ) / 'ETEG            ',   62.10D0 /
      DATA CHEMISTRY_SPC(  99 ), SPECIES_MOLWT(  99 ) / 'ISHP            ',  118.00D0 /
      DATA CHEMISTRY_SPC( 100 ), SPECIES_MOLWT( 100 ) / 'IEPOX           ',  118.10D0 /
      DATA CHEMISTRY_SPC( 101 ), SPECIES_MOLWT( 101 ) / 'MAHP            ',  102.00D0 /
      DATA CHEMISTRY_SPC( 102 ), SPECIES_MOLWT( 102 ) / 'ORA2            ',   60.20D0 /
      DATA CHEMISTRY_SPC( 103 ), SPECIES_MOLWT( 103 ) / 'ORAP            ',  109.00D0 /
      DATA CHEMISTRY_SPC( 104 ), SPECIES_MOLWT( 104 ) / 'PPN             ',  135.00D0 /
      DATA CHEMISTRY_SPC( 105 ), SPECIES_MOLWT( 105 ) / 'MPAN            ',  148.00D0 /
      DATA CHEMISTRY_SPC( 106 ), SPECIES_MOLWT( 106 ) / 'NALD            ',  105.00D0 /
      DATA CHEMISTRY_SPC( 107 ), SPECIES_MOLWT( 107 ) / 'ISON            ',  147.00D0 /
      DATA CHEMISTRY_SPC( 108 ), SPECIES_MOLWT( 108 ) / 'MCTP            ',  172.00D0 /
      DATA CHEMISTRY_SPC( 109 ), SPECIES_MOLWT( 109 ) / 'OLNN            ',  136.00D0 /
      DATA CHEMISTRY_SPC( 110 ), SPECIES_MOLWT( 110 ) / 'OLND            ',  136.00D0 /
      DATA CHEMISTRY_SPC( 111 ), SPECIES_MOLWT( 111 ) / 'ADCN            ',  156.00D0 /
      DATA CHEMISTRY_SPC( 112 ), SPECIES_MOLWT( 112 ) / 'TOLP            ',  141.00D0 /
      DATA CHEMISTRY_SPC( 113 ), SPECIES_MOLWT( 113 ) / 'PER1            ',  141.00D0 /
      DATA CHEMISTRY_SPC( 114 ), SPECIES_MOLWT( 114 ) / 'XYLP            ',  155.00D0 /
      DATA CHEMISTRY_SPC( 115 ), SPECIES_MOLWT( 115 ) / 'PER2            ',  157.00D0 /
      DATA CHEMISTRY_SPC( 116 ), SPECIES_MOLWT( 116 ) / 'XYO2            ',  155.00D0 /
      DATA CHEMISTRY_SPC( 117 ), SPECIES_MOLWT( 117 ) / 'XYOP            ',  155.00D0 /
      DATA CHEMISTRY_SPC( 118 ), SPECIES_MOLWT( 118 ) / 'BAL1            ',  121.00D0 /
      DATA CHEMISTRY_SPC( 119 ), SPECIES_MOLWT( 119 ) / 'BAL2            ',  105.00D0 /
      DATA CHEMISTRY_SPC( 120 ), SPECIES_MOLWT( 120 ) / 'TOLNRXN         ',  141.00D0 /
      DATA CHEMISTRY_SPC( 121 ), SPECIES_MOLWT( 121 ) / 'TOLHRXN         ',  141.00D0 /
      DATA CHEMISTRY_SPC( 122 ), SPECIES_MOLWT( 122 ) / 'XYLNRXN         ',  155.00D0 /
      DATA CHEMISTRY_SPC( 123 ), SPECIES_MOLWT( 123 ) / 'XYLHRXN         ',  155.00D0 /
      DATA CHEMISTRY_SPC( 124 ), SPECIES_MOLWT( 124 ) / 'BNZNRXN         ',  127.00D0 /
      DATA CHEMISTRY_SPC( 125 ), SPECIES_MOLWT( 125 ) / 'BNZHRXN         ',  127.00D0 /
      DATA CHEMISTRY_SPC( 126 ), SPECIES_MOLWT( 126 ) / 'SOAALK          ',  112.00D0 /
      DATA CHEMISTRY_SPC( 127 ), SPECIES_MOLWT( 127 ) / 'ALKRXN          ',  112.00D0 /
      DATA CHEMISTRY_SPC( 128 ), SPECIES_MOLWT( 128 ) / 'ROCIOXY         ',  247.00D0 /
      DATA CHEMISTRY_SPC( 129 ), SPECIES_MOLWT( 129 ) / 'SLOWROC         ',   75.40D0 /
      DATA CHEMISTRY_SPC( 130 ), SPECIES_MOLWT( 130 ) / 'NAPH            ',  133.10D0 /
      DATA CHEMISTRY_SPC( 131 ), SPECIES_MOLWT( 131 ) / 'PAHRO2          ',  187.20D0 /
      DATA CHEMISTRY_SPC( 132 ), SPECIES_MOLWT( 132 ) / 'PAHNRXN         ',  187.20D0 /
      DATA CHEMISTRY_SPC( 133 ), SPECIES_MOLWT( 133 ) / 'PAHHRXN         ',  187.20D0 /
      DATA CHEMISTRY_SPC( 134 ), SPECIES_MOLWT( 134 ) / 'ACRO            ',   56.10D0 /
      DATA CHEMISTRY_SPC( 135 ), SPECIES_MOLWT( 135 ) / 'BDE13           ',   54.10D0 /
      DATA CHEMISTRY_SPC( 136 ), SPECIES_MOLWT( 136 ) / 'FURAN           ',   96.10D0 /
      DATA CHEMISTRY_SPC( 137 ), SPECIES_MOLWT( 137 ) / 'FURANO2         ',  145.10D0 /
      DATA CHEMISTRY_SPC( 138 ), SPECIES_MOLWT( 138 ) / 'FURANONE        ',  100.10D0 /
      DATA CHEMISTRY_SPC( 139 ), SPECIES_MOLWT( 139 ) / 'PROG            ',   76.10D0 /
      DATA CHEMISTRY_SPC( 140 ), SPECIES_MOLWT( 140 ) / 'SESQ            ',  204.40D0 /
      DATA CHEMISTRY_SPC( 141 ), SPECIES_MOLWT( 141 ) / 'SESQNRO2        ',  298.40D0 /
      DATA CHEMISTRY_SPC( 142 ), SPECIES_MOLWT( 142 ) / 'VROCP0OXY       ',  135.00D0 /
      DATA CHEMISTRY_SPC( 143 ), SPECIES_MOLWT( 143 ) / 'VROCP3OXY       ',  134.00D0 /
      DATA CHEMISTRY_SPC( 144 ), SPECIES_MOLWT( 144 ) / 'VROCN2OXY       ',  136.00D0 /
      DATA CHEMISTRY_SPC( 145 ), SPECIES_MOLWT( 145 ) / 'SESQRO2         ',  253.40D0 /
      DATA CHEMISTRY_SPC( 146 ), SPECIES_MOLWT( 146 ) / 'AGLYJ           ',   66.40D0 /
      DATA CHEMISTRY_SPC( 147 ), SPECIES_MOLWT( 147 ) / 'AISO3J          ',  168.20D0 /
      DATA CHEMISTRY_SPC( 148 ), SPECIES_MOLWT( 148 ) / 'AXYL1J          ',  174.00D0 /
      DATA CHEMISTRY_SPC( 149 ), SPECIES_MOLWT( 149 ) / 'AOLGAJ          ',  206.00D0 /
      DATA CHEMISTRY_SPC( 150 ), SPECIES_MOLWT( 150 ) / 'AXYL2J          ',  185.00D0 /
      DATA CHEMISTRY_SPC( 151 ), SPECIES_MOLWT( 151 ) / 'ATOL1J          ',  163.00D0 /
      DATA CHEMISTRY_SPC( 152 ), SPECIES_MOLWT( 152 ) / 'ATOL2J          ',  175.00D0 /
      DATA CHEMISTRY_SPC( 153 ), SPECIES_MOLWT( 153 ) / 'ABNZ1J          ',  161.00D0 /
      DATA CHEMISTRY_SPC( 154 ), SPECIES_MOLWT( 154 ) / 'ABNZ2J          ',  134.00D0 /
      DATA CHEMISTRY_SPC( 155 ), SPECIES_MOLWT( 155 ) / 'ATRP1J          ',  177.00D0 /
      DATA CHEMISTRY_SPC( 156 ), SPECIES_MOLWT( 156 ) / 'AOLGBJ          ',  248.00D0 /
      DATA CHEMISTRY_SPC( 157 ), SPECIES_MOLWT( 157 ) / 'ATRP2J          ',  198.00D0 /
      DATA CHEMISTRY_SPC( 158 ), SPECIES_MOLWT( 158 ) / 'AISO1J          ',  132.00D0 /
      DATA CHEMISTRY_SPC( 159 ), SPECIES_MOLWT( 159 ) / 'AISO2J          ',  133.00D0 /
      DATA CHEMISTRY_SPC( 160 ), SPECIES_MOLWT( 160 ) / 'APAH1J          ',  195.60D0 /
      DATA CHEMISTRY_SPC( 161 ), SPECIES_MOLWT( 161 ) / 'APAH2J          ',  178.70D0 /
      DATA CHEMISTRY_SPC( 162 ), SPECIES_MOLWT( 162 ) / 'AALK1J          ',  225.00D0 /
      DATA CHEMISTRY_SPC( 163 ), SPECIES_MOLWT( 163 ) / 'AALK2J          ',  205.10D0 /
      DATA CHEMISTRY_SPC( 164 ), SPECIES_MOLWT( 164 ) / 'APOCI           ',  220.00D0 /
      DATA CHEMISTRY_SPC( 165 ), SPECIES_MOLWT( 165 ) / 'APNCOMI         ',  220.00D0 /
      DATA CHEMISTRY_SPC( 166 ), SPECIES_MOLWT( 166 ) / 'APOCJ           ',  220.00D0 /
      DATA CHEMISTRY_SPC( 167 ), SPECIES_MOLWT( 167 ) / 'APNCOMJ         ',  220.00D0 /
      DATA CHEMISTRY_SPC( 168 ), SPECIES_MOLWT( 168 ) / 'PCVOC           ',  170.00D0 /
      DATA CHEMISTRY_SPC( 169 ), SPECIES_MOLWT( 169 ) / 'PCSOARXN        ',  170.00D0 /
      DATA CHEMISTRY_SPC( 170 ), SPECIES_MOLWT( 170 ) / 'VLVPO1          ',  218.00D0 /
      DATA CHEMISTRY_SPC( 171 ), SPECIES_MOLWT( 171 ) / 'VSVPO1          ',  230.00D0 /
      DATA CHEMISTRY_SPC( 172 ), SPECIES_MOLWT( 172 ) / 'VSVPO2          ',  241.00D0 /
      DATA CHEMISTRY_SPC( 173 ), SPECIES_MOLWT( 173 ) / 'VSVPO3          ',  253.00D0 /
      DATA CHEMISTRY_SPC( 174 ), SPECIES_MOLWT( 174 ) / 'VIVPO1          ',  266.00D0 /
      DATA CHEMISTRY_SPC( 175 ), SPECIES_MOLWT( 175 ) / 'VLVOO1          ',  136.00D0 /
      DATA CHEMISTRY_SPC( 176 ), SPECIES_MOLWT( 176 ) / 'VLVOO2          ',  136.00D0 /
      DATA CHEMISTRY_SPC( 177 ), SPECIES_MOLWT( 177 ) / 'VSVOO2          ',  135.00D0 /
      DATA CHEMISTRY_SPC( 178 ), SPECIES_MOLWT( 178 ) / 'VSVOO3          ',  134.00D0 /
      DATA CHEMISTRY_SPC( 179 ), SPECIES_MOLWT( 179 ) / 'VSVOO1          ',  135.00D0 /


      DATA CGRID_INDEX(   1 ), SPECIES_TYPE(   1 ), CONVERT_CONC(   1 ) /    1, 'GC', F /  ! O3
      DATA CGRID_INDEX(   2 ), SPECIES_TYPE(   2 ), CONVERT_CONC(   2 ) /    2, 'GC', F /  ! O3P
      DATA CGRID_INDEX(   3 ), SPECIES_TYPE(   3 ), CONVERT_CONC(   3 ) /    3, 'GC', F /  ! O1D
      DATA CGRID_INDEX(   4 ), SPECIES_TYPE(   4 ), CONVERT_CONC(   4 ) /    4, 'GC', F /  ! H2O2
      DATA CGRID_INDEX(   5 ), SPECIES_TYPE(   5 ), CONVERT_CONC(   5 ) /    5, 'GC', F /  ! HO
      DATA CGRID_INDEX(   6 ), SPECIES_TYPE(   6 ), CONVERT_CONC(   6 ) /    6, 'GC', F /  ! NO2
      DATA CGRID_INDEX(   7 ), SPECIES_TYPE(   7 ), CONVERT_CONC(   7 ) /    7, 'GC', F /  ! NO
      DATA CGRID_INDEX(   8 ), SPECIES_TYPE(   8 ), CONVERT_CONC(   8 ) /    8, 'GC', F /  ! NO3
      DATA CGRID_INDEX(   9 ), SPECIES_TYPE(   9 ), CONVERT_CONC(   9 ) /    9, 'GC', F /  ! HONO
      DATA CGRID_INDEX(  10 ), SPECIES_TYPE(  10 ), CONVERT_CONC(  10 ) /   10, 'GC', F /  ! HNO3
      DATA CGRID_INDEX(  11 ), SPECIES_TYPE(  11 ), CONVERT_CONC(  11 ) /   11, 'GC', F /  ! HNO4
      DATA CGRID_INDEX(  12 ), SPECIES_TYPE(  12 ), CONVERT_CONC(  12 ) /   12, 'GC', F /  ! HO2
      DATA CGRID_INDEX(  13 ), SPECIES_TYPE(  13 ), CONVERT_CONC(  13 ) /   13, 'GC', F /  ! HCHO
      DATA CGRID_INDEX(  14 ), SPECIES_TYPE(  14 ), CONVERT_CONC(  14 ) /   14, 'GC', F /  ! CO
      DATA CGRID_INDEX(  15 ), SPECIES_TYPE(  15 ), CONVERT_CONC(  15 ) /   15, 'GC', F /  ! ACD
      DATA CGRID_INDEX(  16 ), SPECIES_TYPE(  16 ), CONVERT_CONC(  16 ) /   16, 'GC', F /  ! MO2
      DATA CGRID_INDEX(  17 ), SPECIES_TYPE(  17 ), CONVERT_CONC(  17 ) /   17, 'GC', F /  ! ALD
      DATA CGRID_INDEX(  18 ), SPECIES_TYPE(  18 ), CONVERT_CONC(  18 ) /   18, 'GC', F /  ! ETHP
      DATA CGRID_INDEX(  19 ), SPECIES_TYPE(  19 ), CONVERT_CONC(  19 ) /   19, 'GC', F /  ! ACT
      DATA CGRID_INDEX(  20 ), SPECIES_TYPE(  20 ), CONVERT_CONC(  20 ) /   20, 'GC', F /  ! ACO3
      DATA CGRID_INDEX(  21 ), SPECIES_TYPE(  21 ), CONVERT_CONC(  21 ) /   21, 'GC', F /  ! UALD
      DATA CGRID_INDEX(  22 ), SPECIES_TYPE(  22 ), CONVERT_CONC(  22 ) /   22, 'GC', F /  ! KET
      DATA CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), CONVERT_CONC(  23 ) /   23, 'GC', F /  ! MEK
      DATA CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), CONVERT_CONC(  24 ) /   24, 'GC', F /  ! HKET
      DATA CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), CONVERT_CONC(  25 ) /   25, 'GC', F /  ! MACR
      DATA CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), CONVERT_CONC(  26 ) /   26, 'GC', F /  ! MACP
      DATA CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), CONVERT_CONC(  27 ) /   27, 'GC', F /  ! XO2
      DATA CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), CONVERT_CONC(  28 ) /   28, 'GC', F /  ! MVK
      DATA CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), CONVERT_CONC(  29 ) /   29, 'GC', F /  ! GLY
      DATA CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), CONVERT_CONC(  30 ) /   30, 'GC', F /  ! MGLY
      DATA CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), CONVERT_CONC(  31 ) /   31, 'GC', F /  ! DCB1
      DATA CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), CONVERT_CONC(  32 ) /   32, 'GC', F /  ! DCB2
      DATA CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), CONVERT_CONC(  33 ) /   33, 'GC', F /  ! BALD
      DATA CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), CONVERT_CONC(  34 ) /   34, 'GC', F /  ! CHO
      DATA CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), CONVERT_CONC(  35 ) /   35, 'GC', F /  ! OP1
      DATA CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), CONVERT_CONC(  36 ) /   36, 'GC', F /  ! OP2
      DATA CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), CONVERT_CONC(  37 ) /   37, 'GC', F /  ! PAA
      DATA CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), CONVERT_CONC(  38 ) /   38, 'GC', F /  ! ONIT
      DATA CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), CONVERT_CONC(  39 ) /   39, 'GC', F /  ! PAN
      DATA CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), CONVERT_CONC(  40 ) /   40, 'GC', F /  ! N2O5
      DATA CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), CONVERT_CONC(  41 ) /   41, 'GC', F /  ! SO2
      DATA CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), CONVERT_CONC(  42 ) /   42, 'GC', F /  ! SULF
      DATA CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), CONVERT_CONC(  43 ) /   43, 'GC', F /  ! SULRXN
      DATA CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), CONVERT_CONC(  44 ) /   44, 'GC', F /  ! ETH
      DATA CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), CONVERT_CONC(  45 ) /   45, 'GC', F /  ! HC3
      DATA CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), CONVERT_CONC(  46 ) /   46, 'GC', F /  ! HC3P
      DATA CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), CONVERT_CONC(  47 ) /  169, 'AE', T /  ! ASOATJ
      DATA CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), CONVERT_CONC(  48 ) /   47, 'GC', F /  ! HC5
      DATA CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), CONVERT_CONC(  49 ) /   48, 'GC', F /  ! HC5P
      DATA CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), CONVERT_CONC(  50 ) /   49, 'GC', F /  ! HC8
      DATA CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), CONVERT_CONC(  51 ) /   50, 'GC', F /  ! HC8P
      DATA CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), CONVERT_CONC(  52 ) /   51, 'GC', F /  ! ETE
      DATA CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), CONVERT_CONC(  53 ) /   52, 'GC', F /  ! ETEP
      DATA CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), CONVERT_CONC(  54 ) /   53, 'GC', F /  ! OLT
      DATA CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), CONVERT_CONC(  55 ) /   54, 'GC', F /  ! OLTP
      DATA CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), CONVERT_CONC(  56 ) /   55, 'GC', F /  ! OLI
      DATA CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), CONVERT_CONC(  57 ) /   56, 'GC', F /  ! OLIP
      DATA CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), CONVERT_CONC(  58 ) /   57, 'GC', F /  ! ACE
      DATA CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), CONVERT_CONC(  59 ) /   58, 'GC', F /  ! ORA1
      DATA CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), CONVERT_CONC(  60 ) /   59, 'GC', F /  ! BEN
      DATA CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), CONVERT_CONC(  61 ) /   60, 'GC', F /  ! BENP
      DATA CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), CONVERT_CONC(  62 ) /   61, 'GC', F /  ! EPX
      DATA CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), CONVERT_CONC(  63 ) /   62, 'GC', F /  ! PHEN
      DATA CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), CONVERT_CONC(  64 ) /   63, 'GC', F /  ! BENZRO2
      DATA CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), CONVERT_CONC(  65 ) /   64, 'GC', F /  ! TOL
      DATA CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), CONVERT_CONC(  66 ) /   65, 'GC', F /  ! TR2
      DATA CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), CONVERT_CONC(  67 ) /   66, 'GC', F /  ! TLP1
      DATA CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), CONVERT_CONC(  68 ) /   67, 'GC', F /  ! CSL
      DATA CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), CONVERT_CONC(  69 ) /   68, 'GC', F /  ! TOLRO2
      DATA CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), CONVERT_CONC(  70 ) /   69, 'GC', F /  ! XYM
      DATA CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), CONVERT_CONC(  71 ) /   70, 'GC', F /  ! XY2
      DATA CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), CONVERT_CONC(  72 ) /   71, 'GC', F /  ! XYL1
      DATA CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), CONVERT_CONC(  73 ) /   72, 'GC', F /  ! XYLRO2
      DATA CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), CONVERT_CONC(  74 ) /   73, 'GC', F /  ! XYE
      DATA CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), CONVERT_CONC(  75 ) /   75, 'GC', F /  ! ISO
      DATA CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), CONVERT_CONC(  76 ) /   76, 'GC', F /  ! ISOP
      DATA CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), CONVERT_CONC(  77 ) /   77, 'GC', F /  ! ISOPRXN
      DATA CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), CONVERT_CONC(  78 ) /   78, 'GC', F /  ! API
      DATA CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), CONVERT_CONC(  79 ) /   79, 'GC', F /  ! APIP
      DATA CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), CONVERT_CONC(  80 ) /   80, 'GC', F /  ! TRPRXN
      DATA CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), CONVERT_CONC(  81 ) /   81, 'GC', F /  ! LIM
      DATA CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), CONVERT_CONC(  82 ) /   82, 'GC', F /  ! LIMP
      DATA CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), CONVERT_CONC(  83 ) /   83, 'GC', F /  ! RCO3
      DATA CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), CONVERT_CONC(  84 ) /   84, 'GC', F /  ! ACTP
      DATA CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), CONVERT_CONC(  85 ) /   85, 'GC', F /  ! MEKP
      DATA CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), CONVERT_CONC(  86 ) /   86, 'GC', F /  ! KETP
      DATA CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), CONVERT_CONC(  87 ) /   87, 'GC', F /  ! MCP
      DATA CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), CONVERT_CONC(  88 ) /   88, 'GC', F /  ! MVKP
      DATA CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), CONVERT_CONC(  89 ) /   89, 'GC', F /  ! UALP
      DATA CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), CONVERT_CONC(  90 ) /   90, 'GC', F /  ! DCB3
      DATA CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), CONVERT_CONC(  91 ) /   91, 'GC', F /  ! BALP
      DATA CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), CONVERT_CONC(  92 ) /   92, 'GC', F /  ! ADDC
      DATA CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), CONVERT_CONC(  93 ) /   93, 'GC', F /  ! MCT
      DATA CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), CONVERT_CONC(  94 ) /   94, 'GC', F /  ! MCTO
      DATA CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), CONVERT_CONC(  95 ) /   95, 'GC', F /  ! MOH
      DATA CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), CONVERT_CONC(  96 ) /   96, 'GC', F /  ! EOH
      DATA CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), CONVERT_CONC(  97 ) /   97, 'GC', F /  ! ROH
      DATA CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), CONVERT_CONC(  98 ) /   98, 'GC', F /  ! ETEG
      DATA CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), CONVERT_CONC(  99 ) /   99, 'GC', F /  ! ISHP
      DATA CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), CONVERT_CONC( 100 ) /  100, 'GC', F /  ! IEPOX
      DATA CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), CONVERT_CONC( 101 ) /  101, 'GC', F /  ! MAHP
      DATA CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), CONVERT_CONC( 102 ) /  102, 'GC', F /  ! ORA2
      DATA CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), CONVERT_CONC( 103 ) /  103, 'GC', F /  ! ORAP
      DATA CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), CONVERT_CONC( 104 ) /  104, 'GC', F /  ! PPN
      DATA CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), CONVERT_CONC( 105 ) /  105, 'GC', F /  ! MPAN
      DATA CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), CONVERT_CONC( 106 ) /  106, 'GC', F /  ! NALD
      DATA CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), CONVERT_CONC( 107 ) /  107, 'GC', F /  ! ISON
      DATA CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), CONVERT_CONC( 108 ) /  108, 'GC', F /  ! MCTP
      DATA CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), CONVERT_CONC( 109 ) /  109, 'GC', F /  ! OLNN
      DATA CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), CONVERT_CONC( 110 ) /  110, 'GC', F /  ! OLND
      DATA CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), CONVERT_CONC( 111 ) /  111, 'GC', F /  ! ADCN
      DATA CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), CONVERT_CONC( 112 ) /  112, 'GC', F /  ! TOLP
      DATA CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), CONVERT_CONC( 113 ) /  113, 'GC', F /  ! PER1
      DATA CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), CONVERT_CONC( 114 ) /  114, 'GC', F /  ! XYLP
      DATA CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), CONVERT_CONC( 115 ) /  115, 'GC', F /  ! PER2
      DATA CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), CONVERT_CONC( 116 ) /   74, 'GC', F /  ! XYO2
      DATA CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), CONVERT_CONC( 117 ) /  116, 'GC', F /  ! XYOP
      DATA CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), CONVERT_CONC( 118 ) /  117, 'GC', F /  ! BAL1
      DATA CGRID_INDEX( 119 ), SPECIES_TYPE( 119 ), CONVERT_CONC( 119 ) /  118, 'GC', F /  ! BAL2
      DATA CGRID_INDEX( 120 ), SPECIES_TYPE( 120 ), CONVERT_CONC( 120 ) /  127, 'GC', F /  ! TOLNRXN
      DATA CGRID_INDEX( 121 ), SPECIES_TYPE( 121 ), CONVERT_CONC( 121 ) /  128, 'GC', F /  ! TOLHRXN
      DATA CGRID_INDEX( 122 ), SPECIES_TYPE( 122 ), CONVERT_CONC( 122 ) /  129, 'GC', F /  ! XYLNRXN
      DATA CGRID_INDEX( 123 ), SPECIES_TYPE( 123 ), CONVERT_CONC( 123 ) /  130, 'GC', F /  ! XYLHRXN
      DATA CGRID_INDEX( 124 ), SPECIES_TYPE( 124 ), CONVERT_CONC( 124 ) /  131, 'GC', F /  ! BNZNRXN
      DATA CGRID_INDEX( 125 ), SPECIES_TYPE( 125 ), CONVERT_CONC( 125 ) /  132, 'GC', F /  ! BNZHRXN
      DATA CGRID_INDEX( 126 ), SPECIES_TYPE( 126 ), CONVERT_CONC( 126 ) /  140, 'GC', F /  ! SOAALK
      DATA CGRID_INDEX( 127 ), SPECIES_TYPE( 127 ), CONVERT_CONC( 127 ) /  141, 'GC', F /  ! ALKRXN
      DATA CGRID_INDEX( 128 ), SPECIES_TYPE( 128 ), CONVERT_CONC( 128 ) /  125, 'GC', F /  ! ROCIOXY
      DATA CGRID_INDEX( 129 ), SPECIES_TYPE( 129 ), CONVERT_CONC( 129 ) /  126, 'GC', F /  ! SLOWROC
      DATA CGRID_INDEX( 130 ), SPECIES_TYPE( 130 ), CONVERT_CONC( 130 ) /  136, 'GC', F /  ! NAPH
      DATA CGRID_INDEX( 131 ), SPECIES_TYPE( 131 ), CONVERT_CONC( 131 ) /  137, 'GC', F /  ! PAHRO2
      DATA CGRID_INDEX( 132 ), SPECIES_TYPE( 132 ), CONVERT_CONC( 132 ) /  138, 'GC', F /  ! PAHNRXN
      DATA CGRID_INDEX( 133 ), SPECIES_TYPE( 133 ), CONVERT_CONC( 133 ) /  139, 'GC', F /  ! PAHHRXN
      DATA CGRID_INDEX( 134 ), SPECIES_TYPE( 134 ), CONVERT_CONC( 134 ) /  119, 'GC', F /  ! ACRO
      DATA CGRID_INDEX( 135 ), SPECIES_TYPE( 135 ), CONVERT_CONC( 135 ) /  120, 'GC', F /  ! BDE13
      DATA CGRID_INDEX( 136 ), SPECIES_TYPE( 136 ), CONVERT_CONC( 136 ) /  122, 'GC', F /  ! FURAN
      DATA CGRID_INDEX( 137 ), SPECIES_TYPE( 137 ), CONVERT_CONC( 137 ) /  123, 'GC', F /  ! FURANO2
      DATA CGRID_INDEX( 138 ), SPECIES_TYPE( 138 ), CONVERT_CONC( 138 ) /  124, 'GC', F /  ! FURANONE
      DATA CGRID_INDEX( 139 ), SPECIES_TYPE( 139 ), CONVERT_CONC( 139 ) /  121, 'GC', F /  ! PROG
      DATA CGRID_INDEX( 140 ), SPECIES_TYPE( 140 ), CONVERT_CONC( 140 ) /  133, 'GC', F /  ! SESQ
      DATA CGRID_INDEX( 141 ), SPECIES_TYPE( 141 ), CONVERT_CONC( 141 ) /  135, 'GC', F /  ! SESQNRO2
      DATA CGRID_INDEX( 142 ), SPECIES_TYPE( 142 ), CONVERT_CONC( 142 ) /  158, 'GC', F /  ! VROCP0OXY
      DATA CGRID_INDEX( 143 ), SPECIES_TYPE( 143 ), CONVERT_CONC( 143 ) /  161, 'GC', F /  ! VROCP3OXY
      DATA CGRID_INDEX( 144 ), SPECIES_TYPE( 144 ), CONVERT_CONC( 144 ) /  156, 'GC', F /  ! VROCN2OXY
      DATA CGRID_INDEX( 145 ), SPECIES_TYPE( 145 ), CONVERT_CONC( 145 ) /  134, 'GC', F /  ! SESQRO2
      DATA CGRID_INDEX( 146 ), SPECIES_TYPE( 146 ), CONVERT_CONC( 146 ) /  170, 'AE', T /  ! AGLYJ
      DATA CGRID_INDEX( 147 ), SPECIES_TYPE( 147 ), CONVERT_CONC( 147 ) /  226, 'AE', T /  ! AISO3J
      DATA CGRID_INDEX( 148 ), SPECIES_TYPE( 148 ), CONVERT_CONC( 148 ) /  173, 'AE', T /  ! AXYL1J
      DATA CGRID_INDEX( 149 ), SPECIES_TYPE( 149 ), CONVERT_CONC( 149 ) /  227, 'AE', T /  ! AOLGAJ
      DATA CGRID_INDEX( 150 ), SPECIES_TYPE( 150 ), CONVERT_CONC( 150 ) /  174, 'AE', T /  ! AXYL2J
      DATA CGRID_INDEX( 151 ), SPECIES_TYPE( 151 ), CONVERT_CONC( 151 ) /  176, 'AE', T /  ! ATOL1J
      DATA CGRID_INDEX( 152 ), SPECIES_TYPE( 152 ), CONVERT_CONC( 152 ) /  177, 'AE', T /  ! ATOL2J
      DATA CGRID_INDEX( 153 ), SPECIES_TYPE( 153 ), CONVERT_CONC( 153 ) /  179, 'AE', T /  ! ABNZ1J
      DATA CGRID_INDEX( 154 ), SPECIES_TYPE( 154 ), CONVERT_CONC( 154 ) /  180, 'AE', T /  ! ABNZ2J
      DATA CGRID_INDEX( 155 ), SPECIES_TYPE( 155 ), CONVERT_CONC( 155 ) /  185, 'AE', T /  ! ATRP1J
      DATA CGRID_INDEX( 156 ), SPECIES_TYPE( 156 ), CONVERT_CONC( 156 ) /  228, 'AE', T /  ! AOLGBJ
      DATA CGRID_INDEX( 157 ), SPECIES_TYPE( 157 ), CONVERT_CONC( 157 ) /  186, 'AE', T /  ! ATRP2J
      DATA CGRID_INDEX( 158 ), SPECIES_TYPE( 158 ), CONVERT_CONC( 158 ) /  187, 'AE', T /  ! AISO1J
      DATA CGRID_INDEX( 159 ), SPECIES_TYPE( 159 ), CONVERT_CONC( 159 ) /  188, 'AE', T /  ! AISO2J
      DATA CGRID_INDEX( 160 ), SPECIES_TYPE( 160 ), CONVERT_CONC( 160 ) /  182, 'AE', T /  ! APAH1J
      DATA CGRID_INDEX( 161 ), SPECIES_TYPE( 161 ), CONVERT_CONC( 161 ) /  183, 'AE', T /  ! APAH2J
      DATA CGRID_INDEX( 162 ), SPECIES_TYPE( 162 ), CONVERT_CONC( 162 ) /  171, 'AE', T /  ! AALK1J
      DATA CGRID_INDEX( 163 ), SPECIES_TYPE( 163 ), CONVERT_CONC( 163 ) /  172, 'AE', T /  ! AALK2J
      DATA CGRID_INDEX( 164 ), SPECIES_TYPE( 164 ), CONVERT_CONC( 164 ) /  229, 'AE', T /  ! APOCI
      DATA CGRID_INDEX( 165 ), SPECIES_TYPE( 165 ), CONVERT_CONC( 165 ) /  231, 'AE', T /  ! APNCOMI
      DATA CGRID_INDEX( 166 ), SPECIES_TYPE( 166 ), CONVERT_CONC( 166 ) /  230, 'AE', T /  ! APOCJ
      DATA CGRID_INDEX( 167 ), SPECIES_TYPE( 167 ), CONVERT_CONC( 167 ) /  232, 'AE', T /  ! APNCOMJ
      DATA CGRID_INDEX( 168 ), SPECIES_TYPE( 168 ), CONVERT_CONC( 168 ) /  152, 'GC', F /  ! PCVOC
      DATA CGRID_INDEX( 169 ), SPECIES_TYPE( 169 ), CONVERT_CONC( 169 ) /  153, 'GC', F /  ! PCSOARXN
      DATA CGRID_INDEX( 170 ), SPECIES_TYPE( 170 ), CONVERT_CONC( 170 ) /  142, 'GC', F /  ! VLVPO1
      DATA CGRID_INDEX( 171 ), SPECIES_TYPE( 171 ), CONVERT_CONC( 171 ) /  143, 'GC', F /  ! VSVPO1
      DATA CGRID_INDEX( 172 ), SPECIES_TYPE( 172 ), CONVERT_CONC( 172 ) /  144, 'GC', F /  ! VSVPO2
      DATA CGRID_INDEX( 173 ), SPECIES_TYPE( 173 ), CONVERT_CONC( 173 ) /  145, 'GC', F /  ! VSVPO3
      DATA CGRID_INDEX( 174 ), SPECIES_TYPE( 174 ), CONVERT_CONC( 174 ) /  146, 'GC', F /  ! VIVPO1
      DATA CGRID_INDEX( 175 ), SPECIES_TYPE( 175 ), CONVERT_CONC( 175 ) /  147, 'GC', F /  ! VLVOO1
      DATA CGRID_INDEX( 176 ), SPECIES_TYPE( 176 ), CONVERT_CONC( 176 ) /  148, 'GC', F /  ! VLVOO2
      DATA CGRID_INDEX( 177 ), SPECIES_TYPE( 177 ), CONVERT_CONC( 177 ) /  150, 'GC', F /  ! VSVOO2
      DATA CGRID_INDEX( 178 ), SPECIES_TYPE( 178 ), CONVERT_CONC( 178 ) /  151, 'GC', F /  ! VSVOO3
      DATA CGRID_INDEX( 179 ), SPECIES_TYPE( 179 ), CONVERT_CONC( 179 ) /  149, 'GC', F /  ! VSVOO1

! The below integers define the locations of mechanism species in the solver
! concentration array.

      INTEGER :: INDEX_O3        =    1
      INTEGER :: INDEX_O3P       =    2
      INTEGER :: INDEX_O1D       =    3
      INTEGER :: INDEX_H2O2      =    4
      INTEGER :: INDEX_HO        =    5
      INTEGER :: INDEX_NO2       =    6
      INTEGER :: INDEX_NO        =    7
      INTEGER :: INDEX_NO3       =    8
      INTEGER :: INDEX_HONO      =    9
      INTEGER :: INDEX_HNO3      =   10
      INTEGER :: INDEX_HNO4      =   11
      INTEGER :: INDEX_HO2       =   12
      INTEGER :: INDEX_HCHO      =   13
      INTEGER :: INDEX_CO        =   14
      INTEGER :: INDEX_ACD       =   15
      INTEGER :: INDEX_MO2       =   16
      INTEGER :: INDEX_ALD       =   17
      INTEGER :: INDEX_ETHP      =   18
      INTEGER :: INDEX_ACT       =   19
      INTEGER :: INDEX_ACO3      =   20
      INTEGER :: INDEX_UALD      =   21
      INTEGER :: INDEX_KET       =   22
      INTEGER :: INDEX_MEK       =   23
      INTEGER :: INDEX_HKET      =   24
      INTEGER :: INDEX_MACR      =   25
      INTEGER :: INDEX_MACP      =   26
      INTEGER :: INDEX_XO2       =   27
      INTEGER :: INDEX_MVK       =   28
      INTEGER :: INDEX_GLY       =   29
      INTEGER :: INDEX_MGLY      =   30
      INTEGER :: INDEX_DCB1      =   31
      INTEGER :: INDEX_DCB2      =   32
      INTEGER :: INDEX_BALD      =   33
      INTEGER :: INDEX_CHO       =   34
      INTEGER :: INDEX_OP1       =   35
      INTEGER :: INDEX_OP2       =   36
      INTEGER :: INDEX_PAA       =   37
      INTEGER :: INDEX_ONIT      =   38
      INTEGER :: INDEX_PAN       =   39
      INTEGER :: INDEX_N2O5      =   40
      INTEGER :: INDEX_SO2       =   41
      INTEGER :: INDEX_SULF      =   42
      INTEGER :: INDEX_SULRXN    =   43
      INTEGER :: INDEX_ETH       =   44
      INTEGER :: INDEX_HC3       =   45
      INTEGER :: INDEX_HC3P      =   46
      INTEGER :: INDEX_ASOATJ    =   47
      INTEGER :: INDEX_HC5       =   48
      INTEGER :: INDEX_HC5P      =   49
      INTEGER :: INDEX_HC8       =   50
      INTEGER :: INDEX_HC8P      =   51
      INTEGER :: INDEX_ETE       =   52
      INTEGER :: INDEX_ETEP      =   53
      INTEGER :: INDEX_OLT       =   54
      INTEGER :: INDEX_OLTP      =   55
      INTEGER :: INDEX_OLI       =   56
      INTEGER :: INDEX_OLIP      =   57
      INTEGER :: INDEX_ACE       =   58
      INTEGER :: INDEX_ORA1      =   59
      INTEGER :: INDEX_BEN       =   60
      INTEGER :: INDEX_BENP      =   61
      INTEGER :: INDEX_EPX       =   62
      INTEGER :: INDEX_PHEN      =   63
      INTEGER :: INDEX_BENZRO2   =   64
      INTEGER :: INDEX_TOL       =   65
      INTEGER :: INDEX_TR2       =   66
      INTEGER :: INDEX_TLP1      =   67
      INTEGER :: INDEX_CSL       =   68
      INTEGER :: INDEX_TOLRO2    =   69
      INTEGER :: INDEX_XYM       =   70
      INTEGER :: INDEX_XY2       =   71
      INTEGER :: INDEX_XYL1      =   72
      INTEGER :: INDEX_XYLRO2    =   73
      INTEGER :: INDEX_XYE       =   74
      INTEGER :: INDEX_ISO       =   75
      INTEGER :: INDEX_ISOP      =   76
      INTEGER :: INDEX_ISOPRXN   =   77
      INTEGER :: INDEX_API       =   78
      INTEGER :: INDEX_APIP      =   79
      INTEGER :: INDEX_TRPRXN    =   80
      INTEGER :: INDEX_LIM       =   81
      INTEGER :: INDEX_LIMP      =   82
      INTEGER :: INDEX_RCO3      =   83
      INTEGER :: INDEX_ACTP      =   84
      INTEGER :: INDEX_MEKP      =   85
      INTEGER :: INDEX_KETP      =   86
      INTEGER :: INDEX_MCP       =   87
      INTEGER :: INDEX_MVKP      =   88
      INTEGER :: INDEX_UALP      =   89
      INTEGER :: INDEX_DCB3      =   90
      INTEGER :: INDEX_BALP      =   91
      INTEGER :: INDEX_ADDC      =   92
      INTEGER :: INDEX_MCT       =   93
      INTEGER :: INDEX_MCTO      =   94
      INTEGER :: INDEX_MOH       =   95
      INTEGER :: INDEX_EOH       =   96
      INTEGER :: INDEX_ROH       =   97
      INTEGER :: INDEX_ETEG      =   98
      INTEGER :: INDEX_ISHP      =   99
      INTEGER :: INDEX_IEPOX     =  100
      INTEGER :: INDEX_MAHP      =  101
      INTEGER :: INDEX_ORA2      =  102
      INTEGER :: INDEX_ORAP      =  103
      INTEGER :: INDEX_PPN       =  104
      INTEGER :: INDEX_MPAN      =  105
      INTEGER :: INDEX_NALD      =  106
      INTEGER :: INDEX_ISON      =  107
      INTEGER :: INDEX_MCTP      =  108
      INTEGER :: INDEX_OLNN      =  109
      INTEGER :: INDEX_OLND      =  110
      INTEGER :: INDEX_ADCN      =  111
      INTEGER :: INDEX_TOLP      =  112
      INTEGER :: INDEX_PER1      =  113
      INTEGER :: INDEX_XYLP      =  114
      INTEGER :: INDEX_PER2      =  115
      INTEGER :: INDEX_XYO2      =  116
      INTEGER :: INDEX_XYOP      =  117
      INTEGER :: INDEX_BAL1      =  118
      INTEGER :: INDEX_BAL2      =  119
      INTEGER :: INDEX_TOLNRXN   =  120
      INTEGER :: INDEX_TOLHRXN   =  121
      INTEGER :: INDEX_XYLNRXN   =  122
      INTEGER :: INDEX_XYLHRXN   =  123
      INTEGER :: INDEX_BNZNRXN   =  124
      INTEGER :: INDEX_BNZHRXN   =  125
      INTEGER :: INDEX_SOAALK    =  126
      INTEGER :: INDEX_ALKRXN    =  127
      INTEGER :: INDEX_ROCIOXY   =  128
      INTEGER :: INDEX_SLOWROC   =  129
      INTEGER :: INDEX_NAPH      =  130
      INTEGER :: INDEX_PAHRO2    =  131
      INTEGER :: INDEX_PAHNRXN   =  132
      INTEGER :: INDEX_PAHHRXN   =  133
      INTEGER :: INDEX_ACRO      =  134
      INTEGER :: INDEX_BDE13     =  135
      INTEGER :: INDEX_FURAN     =  136
      INTEGER :: INDEX_FURANO2   =  137
      INTEGER :: INDEX_FURANONE  =  138
      INTEGER :: INDEX_PROG      =  139
      INTEGER :: INDEX_SESQ      =  140
      INTEGER :: INDEX_SESQNRO2  =  141
      INTEGER :: INDEX_VROCP0OXY =  142
      INTEGER :: INDEX_VROCP3OXY =  143
      INTEGER :: INDEX_VROCN2OXY =  144
      INTEGER :: INDEX_SESQRO2   =  145
      INTEGER :: INDEX_AGLYJ     =  146
      INTEGER :: INDEX_AISO3J    =  147
      INTEGER :: INDEX_AXYL1J    =  148
      INTEGER :: INDEX_AOLGAJ    =  149
      INTEGER :: INDEX_AXYL2J    =  150
      INTEGER :: INDEX_ATOL1J    =  151
      INTEGER :: INDEX_ATOL2J    =  152
      INTEGER :: INDEX_ABNZ1J    =  153
      INTEGER :: INDEX_ABNZ2J    =  154
      INTEGER :: INDEX_ATRP1J    =  155
      INTEGER :: INDEX_AOLGBJ    =  156
      INTEGER :: INDEX_ATRP2J    =  157
      INTEGER :: INDEX_AISO1J    =  158
      INTEGER :: INDEX_AISO2J    =  159
      INTEGER :: INDEX_APAH1J    =  160
      INTEGER :: INDEX_APAH2J    =  161
      INTEGER :: INDEX_AALK1J    =  162
      INTEGER :: INDEX_AALK2J    =  163
      INTEGER :: INDEX_APOCI     =  164
      INTEGER :: INDEX_APNCOMI   =  165
      INTEGER :: INDEX_APOCJ     =  166
      INTEGER :: INDEX_APNCOMJ   =  167
      INTEGER :: INDEX_PCVOC     =  168
      INTEGER :: INDEX_PCSOARXN  =  169
      INTEGER :: INDEX_VLVPO1    =  170
      INTEGER :: INDEX_VSVPO1    =  171
      INTEGER :: INDEX_VSVPO2    =  172
      INTEGER :: INDEX_VSVPO3    =  173
      INTEGER :: INDEX_VIVPO1    =  174
      INTEGER :: INDEX_VLVOO1    =  175
      INTEGER :: INDEX_VLVOO2    =  176
      INTEGER :: INDEX_VSVOO2    =  177
      INTEGER :: INDEX_VSVOO3    =  178
      INTEGER :: INDEX_VSVOO1    =  179

      INTEGER, PARAMETER :: N_ACT_SP = 179

      INTEGER, PARAMETER :: NRXNS = 430

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

      INTEGER, PARAMETER ::        NSUNLIGHT_RXNS   =   35

      INTEGER, PARAMETER ::        NTHERMAL_RXNS    =  395

      INTEGER, PARAMETER ::        KUNITS           =    2

      INTEGER  :: IRXXN

      INTEGER, PARAMETER :: NMPHOT =  34
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & 
     &     31,   32,   33,   34/

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   25,   25,   26,   27,   27, & 
     &     28,   29,   30,   31/

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & 
     &     31,   32,   33,   34/

      REAL( 8 )          :: RTDAT( 3,NRXNS )

      INTEGER, PARAMETER :: NFALLOFF =  17
      REAL( 8 )          :: RFDAT( 5,NFALLOFF )

      INTEGER            :: KTYPE( NRXNS )

      DATA ( KTYPE( IRXXN ), IRXXN = 1, NRXNS ) /  & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    3,    3,    3,    3,    2,    3, & ! 3   
     &      3,    3,    3,    3,    3,    9,    9,    3,   10,   10, & ! 4   
     &      3,    9,    3,    3,    3,   10,   10,    8,    1,    1, & ! 5   
     &      3,    3,    3,   10,    5,    1,   10,    5,    3,   10, & ! 6   
     &      9,    3,    3,    3,    3,    3,   10,    3,    3,   10, & ! 7   
     &      3,    3,    1,    1,    3,    3,    3,    3,    3,    3, & ! 8   
     &      4,    3,    3,    1,    3,    3,    3,    1,    3,    3, & ! 9   
     &      3,    1,    3,    3,    3,    3,    3,    3,    3,    3, & ! O   
     &      1,    3,    3,    1,    1,    1,    3,    3,    1,    1, & ! 1   
     &      1,    3,    3,    1,    3,    3,    3,    3,    3,    3, & ! 2   
     &      3,    3,    1,    1,    1,    1,    1,    1,    4,    3, & ! 3   
     &      3,    3,    3,    1,    3,    3,    3,    1,    3,    3, & ! 4   
     &      3,    1,    1,    3,    1,    3,    1,    1,    1,    1, & ! 5   
     &      1,    1,   10,    5,   10,    5,    3,    3,    3,    3, & ! 6   
     &      1,    1,    1,    1,    1,    1,    3,    1,    3,    3, & ! 7   
     &      1,    3,    3,    3,    3,    1,    1,    3,    3,    3, & ! 8   
     &      1,    1,    3,    3,    3,    3,    1,    1,    3,    3, & ! 9   
     &      1,    1,    1,    3,    1,    1,    1,    1,    3,    3, & ! O   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 1   
     &      3,    3,    3,    3,    3,    1,    1,    3,    3,    3, & ! 2   
     &      3,    3,    3,    3,    3,    3,    3,    1,    3,    3, & ! 3   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 4   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 5   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 6   
     &      1,    3,    3,    3,    3,    3,    3,    3,    3,    1, & ! 7   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 8   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 9   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! O   
     &      3,    3,    3,    3,    3,    3,    3,    3,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    3,    3,    3,    1,    3,    3, & ! 5   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    1, & ! 6   
     &      1,    3,    3,    1,    1,    1,    3,    3,    1,    1, & ! 7   
     &      3,    3,    1,    1,    1,    1,    1,    3,    3,    1, & ! 8   
     &      1,    1,    3,    1,    3,   -1,   -1,   -1,   -1,   12, & ! 9   
     &     -1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,   -1,    1,   -1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1/!2   

      INTEGER            :: IRXBITS( NRXNS )

      DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    0,    0,    0,    0,   20,    0, & ! 3   
     &     16,   32,    8,  128,    0,    0,    8,    0,    1,    1, & ! 4   
     &      0,    0,   16,    0,    0,    1,    1,    0,    0,    0, & ! 5   
     &      0,    0,    0,    1,    0,    8,    1,    0,    0,    1, & ! 6   
     &      0,   64,    0,    0,    0,    0,    1,    0,    0,    1, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    1,    0,    1,    0,    0,    0,    0,    0, & ! 6   
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
     &      0,    0,    0,    0,    0,    1,    1,    1,    1,    2, & ! 9   
     &      1,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    1,    0,    1,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

      INTEGER, PARAMETER :: NTERMS_JACOB =    32041

      INTEGER, PARAMETER :: NSTEPS_JACOB =      860

      INTEGER            :: IORDER( NRXNS )

      DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    2,    2,    2,    2,    3,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    3,    2,    2,    2, & ! 4   
     &      2,    2,    3,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    1,    2,    2,    1,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    1,    1,    1,    1, & ! 5   
     &      1,    1,    2,    1,    2,    1,    2,    1,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    1,    1,    1,    1,    1, & ! 9   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2/!2   

      INTEGER, PARAMETER :: KTN1 = 139
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     59,   60,   66,   83,   84,   94,   98,  102,  111,  114, & ! O   
     &    115,  116,  119,  120,  121,  124,  133,  134,  135,  136, & ! 1   
     &    137,  138,  144,  148,  152,  153,  155,  157,  158,  159, & ! 2   
     &    160,  161,  162,  171,  172,  173,  174,  175,  176,  178, & ! 3   
     &    181,  186,  187,  191,  192,  197,  198,  201,  202,  203, & ! 4   
     &    205,  206,  207,  208,  226,  227,  238,  271,  280,  319, & ! 5   
     &    320,  321,  322,  323,  324,  325,  326,  327,  328,  329, & ! 6   
     &    330,  331,  332,  333,  334,  335,  336,  337,  338,  339, & ! 7   
     &    340,  341,  342,  343,  344,  345,  346,  347,  348,  349, & ! 8   
     &    350,  351,  352,  353,  354,  358,  370,  371,  374,  375, & ! 9   
     &    376,  379,  380,  383,  384,  385,  386,  387,  390,  391, & ! O   
     &    392,  394,  402,  403,  404,  405,  406,  407,  408,  409, & ! 1   
     &    410,  411,  412,  413,  414,  415,  416,  418,  420,  421, & ! 2   
     &    422,  423,  424,  425,  426,  427,  428,  429,  430/     !3   

      INTEGER, PARAMETER :: KTN2 =   1
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &     39/

      INTEGER, PARAMETER :: KTN3 = 226
      INTEGER            :: KRX3( KTN3 )

      DATA ( KRX3( IRXXN ), IRXXN = 1, KTN3 ) / & 
     &     35,   36,   37,   38,   40,   41,   42,   43,   44,   45, & ! O   
     &     48,   51,   53,   54,   55,   61,   62,   63,   69,   72, & ! 1   
     &     73,   74,   75,   76,   78,   79,   81,   82,   85,   86, & ! 2   
     &     87,   88,   89,   90,   92,   93,   95,   96,   97,   99, & ! 3   
     &    100,  101,  103,  104,  105,  106,  107,  108,  109,  110, & ! 4   
     &    112,  113,  117,  118,  122,  123,  125,  126,  127,  128, & ! 5   
     &    129,  130,  131,  132,  140,  141,  142,  143,  145,  146, & ! 6   
     &    147,  149,  150,  151,  154,  156,  167,  168,  169,  170, & ! 7   
     &    177,  179,  180,  182,  183,  184,  185,  188,  189,  190, & ! 8   
     &    193,  194,  195,  196,  199,  200,  204,  209,  210,  211, & ! 9   
     &    212,  213,  214,  215,  216,  217,  218,  219,  220,  221, & ! O   
     &    222,  223,  224,  225,  228,  229,  230,  231,  232,  233, & ! 1   
     &    234,  235,  236,  237,  239,  240,  241,  242,  243,  244, & ! 2   
     &    245,  246,  247,  248,  249,  250,  251,  252,  253,  254, & ! 3   
     &    255,  256,  257,  258,  259,  260,  261,  262,  263,  264, & ! 4   
     &    265,  266,  267,  268,  269,  270,  272,  273,  274,  275, & ! 5   
     &    276,  277,  278,  279,  281,  282,  283,  284,  285,  286, & ! 6   
     &    287,  288,  289,  290,  291,  292,  293,  294,  295,  296, & ! 7   
     &    297,  298,  299,  300,  301,  302,  303,  304,  305,  306, & ! 8   
     &    307,  308,  309,  310,  311,  312,  313,  314,  315,  316, & ! 9   
     &    317,  318,  355,  356,  357,  359,  360,  361,  362,  363, & ! O   
     &    364,  365,  366,  367,  368,  369,  372,  373,  377,  378, & ! 1   
     &    381,  382,  388,  389,  393,  395/     !  2   

      INTEGER, PARAMETER :: KTN4 =   2
      INTEGER            :: KRX4( KTN4 )

      DATA ( KRX4( IRXXN ), IRXXN = 1, KTN4 ) / & 
     &     91,  139/

      INTEGER, PARAMETER :: KTN5 =   4
      INTEGER            :: KRX5( KTN5 )

      DATA ( KRX5( IRXXN ), IRXXN = 1, KTN5 ) / & 
     &     65,   68,  164,  166/

      INTEGER, PARAMETER :: KTN6 =   0
      INTEGER            :: KRX6( 1 )

      DATA   KRX6( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN7 =   0
      INTEGER            :: KRX7( 1 )

      DATA   KRX7( 1 ) / 0 /

      INTEGER, PARAMETER :: NWM =   1
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &     39/
      REAL( 8 ),    PARAMETER :: ATM_AIR = 1.00000D+06

      INTEGER, PARAMETER :: NWW =   3
      INTEGER            :: NRXWW( NWW )

      DATA ( NRXWW( IRXXN ), IRXXN = 1, NWW ) / & 
     &     43,   47,   66/

      INTEGER, PARAMETER :: NWO2 =   3
      INTEGER            :: NRXWO2( NWO2 )

      DATA ( NRXWO2( IRXXN ), IRXXN = 1, NWO2 ) / & 
     &     39,   41,   53/
      REAL( 8 ),    PARAMETER :: ATM_O2 = 2.09500D+05

      INTEGER, PARAMETER :: NWN2 =   1
      INTEGER            :: NRXWN2( NWN2 )

      DATA ( NRXWN2( IRXXN ), IRXXN = 1, NWN2 ) / & 
     &     42/
      REAL( 8 ),    PARAMETER :: ATM_N2 = 7.80800D+05

      INTEGER, PARAMETER :: NWCH4 =   1
      INTEGER            :: NRXWCH4( NWCH4 )

      DATA ( NRXWCH4( IRXXN ), IRXXN = 1, NWCH4 ) / & 
     &     72/
      REAL( 8 ),    PARAMETER :: ATM_CH4 = 1.85000D+00

      INTEGER, PARAMETER :: NWH2 =   1
      INTEGER            :: NRXWH2( NWH2 )

      DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) / & 
     &     44/
      REAL( 8 ),    PARAMETER :: ATM_H2 = 5.60000D-01

      INTEGER, PARAMETER :: MXPRD =  21
      INTEGER            :: IRR( NRXNS,MXPRD+3 )

      DATA ( IRR( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    4,    6,    8,    8,    9,   10,   11,   13, & ! O   
     &     13,   15,   17,   19,   19,   21,   23,   22,   24,   25, & ! 1   
     &     28,   29,   29,   29,   30,   31,   32,   33,   35,   36, & ! 2   
     &     37,   38,   39,   39,    1,    1,    1,    1,    2,    2, & ! 3   
     &      3,    3,    3,    5,    5,   12,   12,    4,    7,    7, & ! 4   
     &      7,    7,    7,    9,    6,    6,    6,   10,    8,    8, & ! 5   
     &      8,    8,    8,    8,   40,   40,    6,   11,   11,   41, & ! 6   
     &     14,    5,   44,   45,   48,   50,   52,   54,   56,   58, & ! 7   
     &     60,   65,   70,   74,   75,   78,   81,   13,   15,   17, & ! 8   
     &     19,   23,   22,   24,   25,   28,   21,   29,   30,   31, & ! 9   
     &     32,   90,   33,   63,   68,   62,   93,   95,   96,   97, & ! O   
     &     98,   35,   36,   99,  101,   59,  102,   37,   39,  104, & ! 1   
     &    105,   38,  106,  107,   52,   54,   56,   75,   78,   81, & ! 2   
     &     25,   28,   21,   31,   32,   90,   62,   94,   52,   54, & ! 3   
     &     56,   75,   78,   81,   13,   15,   17,   25,   21,   29, & ! 4   
     &     30,   63,   68,   62,   93,  105,   66,  112,   71,  114, & ! 5   
     &    116,  117,   20,   39,   83,  104,   26,  105,   16,   18, & ! 6   
     &     46,   49,   51,   53,   55,   57,   61,   67,  112,  113, & ! 7   
     &     72,  114,  115,  117,   76,   79,   82,   20,   83,   84, & ! 8   
     &     85,   86,   26,   87,   88,   89,   91,  118,   92,  108, & ! 9   
     &    103,  109,  110,  111,   27,  119,   34,   94,   16,   18, & ! O   
     &     46,   49,   51,   53,   55,   57,   61,   67,  112,  113, & ! 1   
     &     72,  114,  115,  117,   76,   79,   82,   20,   83,   84, & ! 2   
     &     85,   86,   26,   87,   88,   89,   92,   34,  108,  103, & ! 3   
     &    109,  110,  111,   27,   16,   18,   46,   49,   51,   53, & ! 4   
     &     55,   57,   61,   67,  112,  113,   72,  114,  115,  117, & ! 5   
     &     76,   79,   82,   20,   83,   84,   85,   86,   26,   87, & ! 6   
     &     88,   89,   91,  118,   92,  108,  103,  109,  110,  111, & ! 7   
     &     27,   18,   46,   49,   51,   53,   55,   57,   61,   67, & ! 8   
     &    112,  113,   72,  114,  115,  117,   76,   79,   82,   20, & ! 9   
     &     83,   84,   85,   86,   26,   87,   88,   89,   91,  118, & ! O   
     &     92,  108,  103,  109,  110,  111,   27,   83,   16,   18, & ! 1   
     &     46,   49,   51,   53,   55,   57,   61,   67,  112,  113, & ! 2   
     &     72,  114,  115,  117,   76,   79,   82,   20,   83,   84, & ! 3   
     &     85,   86,   26,   87,   88,   89,   91,  118,   92,  108, & ! 4   
     &    103,  109,  110,  111,  109,  109,  110,   27,   27,   27, & ! 5   
     &     69,   69,   73,   73,   64,   64,  126,  100,  128,  129, & ! 6   
     &    130,  131,  131,  134,  134,  134,  135,  135,  135,  136, & ! 7   
     &    137,  137,  138,  136,  136,  139,  140,  141,  141,  141, & ! 8   
     &    140,  140,  145,  145,  145,   29,   30,   40,    6,    1, & ! 9   
     &    100,  148,  150,  151,  152,  153,  154,  155,  157,  158, & ! O   
     &    159,  160,  161,  162,  163,  164,  165,  166,  167,  168, & ! 1   
     &    170,  171,  172,  173,  174,  175,  176,  179,  177,  178/!2   

      DATA ( IRR( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    5,   12,    7,    6,    0,    1, & ! 3   
     &      0,    0,    0,    0,   12,   12,   12,    5,    2,    5, & ! 4   
     &     12,   12,    7,    5,    2,    2,    5,    5,    5,   12, & ! 5   
     &      7,    6,    8,    6,    0,    0,   12,    0,    5,    5, & ! 6   
     &      5,    0,    5,    5,    5,    5,    5,    5,    5,    5, & ! 7   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! 8   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! 9   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! O   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! 1   
     &      5,    5,    5,    5,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    8,    8, & ! 3   
     &      8,    8,    8,    8,    8,    8,    8,    8,    8,    8, & ! 4   
     &      8,    8,    8,    8,    8,    8,    0,    0,    0,    0, & ! 5   
     &      0,    0,    6,    0,    6,    0,    6,    0,    7,    7, & ! 6   
     &      7,    7,    7,    7,    7,    7,    7,    7,    7,    7, & ! 7   
     &      7,    7,    7,    7,    7,    7,    7,    7,    7,    7, & ! 8   
     &      7,    7,    7,    7,    7,    7,    7,    7,    7,    7, & ! 9   
     &      7,    7,    7,    7,    7,    6,    6,    6,   12,   12, & ! O   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 1   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 2   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 3   
     &     12,   12,   12,   12,   16,   16,   16,   16,   16,   16, & ! 4   
     &     16,   16,   16,   16,   16,   16,   16,   16,   16,   16, & ! 5   
     &     16,   16,   16,   16,   16,   16,   16,   16,   16,   16, & ! 6   
     &     16,   16,   16,   16,   16,   16,   16,   16,   16,   16, & ! 7   
     &     16,   20,   20,   20,   20,   20,   20,   20,   20,   20, & ! 8   
     &     20,   20,   20,   20,   20,   20,   20,   20,   20,   20, & ! 9   
     &     20,   20,   20,   20,   20,   20,   20,   20,   20,   20, & ! O   
     &     20,   20,   20,   20,   20,   20,   20,   83,    8,    8, & ! 1   
     &      8,    8,    8,    8,    8,    8,    8,    8,    8,    8, & ! 2   
     &      8,    8,    8,    8,    8,    8,    8,    8,    8,    8, & ! 3   
     &      8,    8,    8,    8,    8,    8,    8,    8,    8,    8, & ! 4   
     &      8,    8,    8,    8,  109,  110,  110,    8,   83,   27, & ! 5   
     &      7,   12,    7,   12,    7,   12,    5,    5,    5,    5, & ! 6   
     &      5,    7,   12,    5,    1,    8,    5,    1,    8,    5, & ! 7   
     &      7,   12,    5,    1,    8,    5,    8,   12,    7,    8, & ! 8   
     &      1,    5,   12,    8,    7,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    5,    5,    5,    5,    5, & ! 1   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5/!2   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

      DATA ( IRR( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &      2,    3,    5,    2,    7,    2,    5,    5,    5,   14, & ! O   
     &     12,   12,   12,   16,   16,   12,   16,   18,   12,    5, & ! 1   
     &     16,   14,   13,   12,   12,   12,   12,   34,    5,    5, & ! 2   
     &      5,   12,   20,   16,   12,    5,    6,    8,    1,    0, & ! 3   
     &      2,    2,    5,   12,    0,    4,    4,   12,    6,    9, & ! 4   
     &      6,   10,    6,    6,    7,    8,   10,    8,   12,    5, & ! 5   
     &      6,    7,    6,   40,    6,   10,   11,   12,    6,   12, & ! 6   
     &     12,   16,   18,   46,   49,   12,   53,   55,   57,    5, & ! 7   
     &     12,   12,   12,   12,   76,   79,   82,   12,   20,   83, & ! 8   
     &     84,   85,   86,   12,   26,   88,   20,   12,   20,   12, & ! 9   
     &     12,   12,   91,   12,   12,   12,   94,   12,   12,   12, & ! O   
     &     12,    5,    5,    5,   26,   12,   16,    5,   27,   27, & ! 1   
     &      6,   46,    6,  106,    5,    5,    5,    5,    5,    5, & ! 2   
     &      5,    5,    5,    5,    5,    5,    5,  108,  109,  109, & ! 3   
     &    109,  107,  109,  109,   12,   20,   83,   13,   12,   12, & ! 4   
     &     20,   34,   34,    5,   94,   26,    5,    5,    5,    5, & ! 5   
     &      5,    5,   39,   20,  104,   83,  105,   26,   12,   12, & ! 6   
     &     12,   12,   12,   12,   12,   12,   12,    6,   12,   12, & ! 7   
     &      6,   12,   12,   12,   12,   12,   12,   16,   18,   20, & ! 8   
     &     12,   12,   16,    6,   12,   12,  118,  119,   12,   94, & ! 9   
     &      6,    6,    6,    6,    6,   38,   38,   38,   35,   36, & ! O   
     &     36,   36,   36,   36,   36,   36,   36,   36,   36,   36, & ! 1   
     &     36,   36,   36,   36,   99,   36,   36,    5,    5,    5, & ! 2   
     &     36,   36,  101,  101,   36,   36,   36,   68,   36,   36, & ! 3   
     &     38,   38,   36,   36,   12,   12,   12,   12,   12,   12, & ! 4   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 5   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,    6, & ! 6   
     &     12,   12,   12,   12,   12,   12,   13,   12,   12,   12, & ! 7   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   16, & ! 8   
     &     12,   12,   16,   12,   12,   12,   12,   12,   12,   16, & ! 9   
     &     16,   16,   12,   12,  102,    6,   12,   12,   16,   16, & ! O   
     &     12,   12,   16,   12,   16,   12,   16,   18,   12,   12, & ! 1   
     &     12,   12,   12,   12,   17,   12,   12,    6,   12,   12, & ! 2   
     &      6,   12,   12,   12,   12,   12,   12,   16,   18,   20, & ! 3   
     &     12,   12,   13,    6,   12,   12,  118,  119,   12,    6, & ! 4   
     &      6,   12,    6,    6,   12,   12,    6,    6,   18,    0, & ! 5   
     &      7,   12,    7,   12,    7,   12,    5,    5,   12,   18, & ! 6   
     &      5,    7,   12,    5,    1,    8,    5,    1,    8,   31, & ! 7   
     &     38,   36,   22,    5,    6,   24,  141,  142,  143,  143, & ! 8   
     &    143,  145,  142,  143,  142,  146,  146,   10,    9,    0, & ! 9   
     &    147,  149,  149,  149,  149,  149,  149,  156,  156,  156, & ! O   
     &    156,  149,  149,  149,  149,  165,    5,  167,    5,    5, & ! 1   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5/!2   

      DATA ( IRR( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    7,    0,    6,    7,    6,   12,    0, & ! O   
     &     14,   16,   18,   20,   14,   20,   18,   20,   20,   12, & ! 1   
     &     26,    0,   14,   14,   20,   20,   20,   12,   12,   12, & ! 2   
     &     16,    6,    6,    8,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      5,    0,    0,    0,    0,    0,    0,    0,    6,    6, & ! 5   
     &      0,    6,    0,    0,    8,    0,    0,    6,    0,   42, & ! 6   
     &      0,    0,    0,   47,   47,   51,    0,    0,    0,   12, & ! 7   
     &     61,   66,   71,   71,   77,   80,   80,   14,    0,    0, & ! 8   
     &      0,    0,    0,   30,   87,    0,   89,   14,   14,   14, & ! 9   
     &     14,   26,    0,   92,   92,   27,    0,   13,   15,   17, & ! O   
     &     17,   16,   46,   25,    0,    0,  103,   20,    8,    8, & ! 1   
     &     24,    6,   27,   24,   12,   12,   12,   12,   12,   12, & ! 2   
     &     12,   12,   12,   12,   12,   12,   12,    0,  110,  110, & ! 3   
     &    110,   77,  110,  110,   14,   10,   10,   26,   27,   14, & ! 4   
     &     14,   92,   92,   12,   10,    6,   12,   12,   12,   12, & ! 5   
     &     12,   12,    0,    6,    0,    6,    0,    6,    6,    6, & ! 6   
     &     16,   16,   18,    6,    6,    6,    6,   33,    6,    6, & ! 7   
     &     33,    6,    6,    6,    6,    6,   21,    6,    6,    6, & ! 8   
     &      6,   20,   20,   12,   20,    6,    6,    6,    6,    6, & ! 9   
     &     29,   12,   13,   29,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,   16,   18,   20, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,   13,   13,   16,   16,   18,   13, & ! 4   
     &     13,   13,   90,   13,   13,   13,   13,   13,   13,   13, & ! 5   
     &     13,   13,   13,   16,   16,   20,   13,   13,   20,   12, & ! 6   
     &     20,   14,  118,  119,   13,   94,   12,   13,    6,    6, & ! 7   
     &     13,   16,   16,   16,   16,   16,   16,   16,   16,   33, & ! 8   
     &     16,   16,   33,   16,   16,   16,   16,   16,   16,    0, & ! 9   
     &     18,   20,   16,   16,   16,   12,   16,   16,  118,  119, & ! O   
     &     16,   16,   29,   16,    6,   16,    0,    0,   13,    6, & ! 1   
     &     16,   16,   18,    6,   13,   17,    6,   33,    6,    6, & ! 2   
     &     33,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 3   
     &      6,    6,   20,   12,   20,    6,    6,    6,    6,   94, & ! 4   
     &     29,    6,   13,   29,   38,    6,   13,    0,    0,    0, & ! 5   
     &    120,  121,  122,  123,  124,  125,  127,    0,   51,   47, & ! 6   
     &    131,  132,  133,    0,    0,    0,  134,  134,  134,   12, & ! 7   
     &      6,  138,   29,   17,   31,   17,    0,    0,    6,    6, & ! 8   
     &    144,    0,    0,    0,    6,    0,    0,    0,   10,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,  164,    0,  166,    0,  169, & ! 1   
     &    170,  170,  170,  170,  170,  175,  175,  175,  175,  175/!2   

      DATA ( IRR( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    6,    0, & ! O   
     &      0,   14,   14,    0,    0,   14,   20,   14,   13,   20, & ! 1   
     &     14,    0,    0,    0,   14,   27,   27,   14,   13,   17, & ! 2   
     &      0,   17,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   10, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   43, & ! 6   
     &      0,    0,    0,    0,    0,   17,    0,    0,    0,   14, & ! 7   
     &     62,   67,   72,   72,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   17, & ! 9   
     &     23,   14,    0,   34,   34,   14,    0,    0,    0,   15, & ! O   
     &      0,   13,   27,  100,    0,    0,    0,   27,   13,   13, & ! 1   
     &      0,    0,   24,   13,   14,   16,   16,   16,   18,   18, & ! 2   
     &     20,   20,   16,   83,   83,   14,   14,    0,    0,    0, & ! 3   
     &      0,    0,   80,   80,   10,    0,    0,   27,   14,   10, & ! 4   
     &     10,  111,  111,   14,    0,    0,  112,  113,   83,  115, & ! 5   
     &     83,  115,    0,    0,    0,    0,    0,    0,   13,   15, & ! 6   
     &     18,   18,   27,   13,   13,   15,   32,    0,   32,   33, & ! 7   
     &      0,   90,   30,   29,   13,   13,   13,    0,    0,   13, & ! 8   
     &     13,   27,    6,   13,   27,   14,    0,    0,   24,    0, & ! 9   
     &     12,   38,   17,   36,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,  102,  102,   13, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,   95,   15,   18,   18,   27,   17, & ! 4   
     &     17,   17,   13,   33,   29,   30,   33,   32,   30,   29, & ! 5   
     &     25,   17,   56,   13,   13,   13,   31,   31,   14,   13, & ! 6   
     &     27,   13,   13,   13,   24,   13,   29,   38,   13,   13, & ! 7   
     &      0,   15,   18,   18,   18,   13,   13,   17,   32,    0, & ! 8   
     &     32,   30,    0,   32,   30,   29,   13,   17,   56,    0, & ! 9   
     &      0,   13,   13,   31,   20,   13,   20,   14,    0,    0, & ! O   
     &     24,   94,    0,   38,   13,    6,    0,    0,    6,   15, & ! 1   
     &     27,   18,   27,   13,   12,   22,   32,    0,   32,   30, & ! 2   
     &      0,   90,   30,   29,   13,   17,   56,    0,    0,   13, & ! 3   
     &     13,   31,   14,   13,   27,   14,    0,    0,   24,    0, & ! 4   
     &     12,   38,   17,   36,    0,   13,   17,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,   47,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  137, & ! 7   
     &    138,    5,   12,    0,   90,   12,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    5,    0,    5,    0,    0, & ! 1   
     &    171,  171,  171,  171,  171,  176,  176,  176,  176,  176/!2   

      DATA ( IRR( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    8,    0, & ! O   
     &      0,    0,    0,    0,    0,   13,   14,    0,    0,   26, & ! 1   
     &     21,    0,    0,    0,    0,   14,   14,    0,    0,    0, & ! 2   
     &      0,   22,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,   24,    0,    0,    0,   29, & ! 7   
     &     63,   68,   68,   68,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   22, & ! 9   
     &     29,   29,    0,   93,   93,   17,    0,    0,    0,    0, & ! O   
     &      0,    0,   17,    0,    0,    0,    0,   13,    0,    0, & ! 1   
     &      0,    0,    0,    0,   13,   18,   18,   20,   86,   86, & ! 2   
     &     14,   27,   20,   27,   27,   29,   33,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,   30,   13,    0, & ! 4   
     &      0,   10,   10,   29,    0,    0,  113,   32,  114,   32, & ! 5   
     &    115,   32,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &     27,   27,    6,   17,   15,   17,   90,    0,   38,   30, & ! 7   
     &      0,   38,   31,   30,   25,   17,   59,    0,    0,    0, & ! 8   
     &     31,    6,   14,   24,    6,   13,    0,    0,   29,    0, & ! 9   
     &      0,    0,   22,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,   37,   37,   36, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,   95,   27,   27,   13,   95, & ! 4   
     &     22,   22,   32,    0,   32,   31,    0,    0,   31,   30, & ! 5   
     &     28,   22,   25,  102,  102,   95,   95,   95,   13,   24, & ! 6   
     &     13,   17,    0,    0,   29,    0,    0,    0,   17,   29, & ! 7   
     &      0,  102,   27,   27,   27,   17,   17,   22,   90,    0, & ! 8   
     &      0,   31,    0,    0,   31,   30,   25,   22,   13,    0, & ! 9   
     &      0,  102,   31,  102,   14,   24,   27,   13,    0,    0, & ! O   
     &     29,    0,    0,    0,   17,   29,    0,    0,    0,    0, & ! 1   
     &     18,   27,    6,   17,    6,    6,   90,    0,    0,   31, & ! 2   
     &      0,    0,   31,   30,   25,   22,   13,    0,    0,    0, & ! 3   
     &     31,    0,    6,   24,    6,   13,    0,    0,   29,    0, & ! 4   
     &      0,    0,   22,    0,    0,   17,   22,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,   17,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     12,   12,   16,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &    172,  172,  172,  172,  172,  179,  179,  179,  179,  179/!2   

      DATA ( IRR( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,   17,    0,    0,    0,   27, & ! 1   
     &      0,    0,    0,    0,    0,   29,   29,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,   47,    0,    0,    0,   59, & ! 7   
     &     64,   69,   73,   73,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   29, & ! 9   
     &     30,   30,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,   22,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,   59,   46,   46,   26,    4,    4, & ! 2   
     &     30,   14,   27,   14,   14,   31,   29,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,   10,   17,    0, & ! 4   
     &      0,    0,    0,    6,    0,    0,   32,   68,  115,   68, & ! 5   
     &    117,   68,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      6,    6,   17,    0,   17,   19,   29,    0,    0,   31, & ! 7   
     &      0,    0,   90,   31,   28,   19,    6,    0,    0,    0, & ! 8   
     &      0,   17,   13,    0,   13,   17,    0,    0,   36,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,   96,   13,   13,   17,   98, & ! 4   
     &     95,   95,   29,    0,    0,    0,    0,    0,   90,   31, & ! 5   
     &     95,   95,   95,    0,    0,   97,   97,   97,  102,   95, & ! 6   
     &     17,   22,    0,    0,   36,    0,    0,    0,   22,   36, & ! 7   
     &      0,    0,   13,   13,   17,  102,   22,  102,   29,    0, & ! 8   
     &      0,    0,    0,    0,   90,   31,   28,  102,   25,    0, & ! 9   
     &      0,    0,  102,    0,   13,   16,   13,   17,    0,    0, & ! O   
     &     36,    0,    0,    0,   22,   36,    0,    0,    0,    0, & ! 1   
     &      6,    6,   17,    0,   23,   19,   29,    0,    0,   33, & ! 2   
     &      0,    0,   90,   31,   28,    0,   25,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,   13,   17,    0,    0,   36,    0, & ! 4   
     &      0,    0,    0,    0,    0,   22,   38,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,   24,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     16,   16,   14,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &    173,  173,  173,  173,  175,  177,  177,  177,  177,  177/!2   

      DATA ( IRR( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,   22,    0,    0,    0,   14, & ! 1   
     &      0,    0,    0,    0,    0,   30,   30,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   30, & ! 9   
     &     36,   36,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,   49,   20,    4,   14,   14, & ! 2   
     &     59,   13,   14,   13,   13,   59,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    6,   38,    0, & ! 4   
     &      0,    0,    0,   10,    0,    0,   68,    0,   32,    0, & ! 5   
     &     32,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &     15,   13,   22,    0,   19,   22,   38,    0,    0,   38, & ! 7   
     &      0,    0,   38,   32,  107,   22,   56,    0,    0,    0, & ! 8   
     &      0,   30,    0,    0,   17,   29,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,   17,   17,   22,    0, & ! 4   
     &     97,   97,    0,    0,    0,    0,    0,    0,    0,   32, & ! 5   
     &     97,   97,   97,    0,    0,  102,    0,    0,   16,   97, & ! 6   
     &     30,   29,    0,    0,    0,    0,    0,    0,   95,   38, & ! 7   
     &      0,    0,   17,   17,   22,    0,  102,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   32,  102,    0,  102,    0, & ! 9   
     &      0,    0,    0,    0,    0,  102,   17,   22,    0,    0, & ! O   
     &      0,    0,    0,    0,  102,   38,    0,    0,    0,    0, & ! 1   
     &     15,   13,   22,    0,   15,   15,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,   32,   29,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,   17,   22,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,   38,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &    174,  175,  175,  175,  176,  178,  178,  178,  178,  178/!2   

      DATA ( IRR( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   13, & ! 1   
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
     &      0,    0,    0,    0,    0,    4,   14,   14,   17,   54, & ! 2   
     &      0,   30,   13,   29,   29,  102,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,   62,    0,   68,    0, & ! 5   
     &     68,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &     17,   15,   38,    0,   23,   24,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,   90,   29,   59,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,   30,   22,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,   22,   22,   95,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   90, & ! 5   
     &     17,    0,    0,    0,    0,    0,    0,    0,   95,    0, & ! 6   
     &     95,   30,    0,    0,    0,    0,    0,    0,   97,    0, & ! 7   
     &      0,    0,   22,   22,  102,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   90,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,   30,   29,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &     17,   17,    0,    0,   19,   24,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,   90,   24,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,   30,   29,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &    175,  176,  176,  176,    0,    0,    0,    0,    0,    0/!2   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,   14,    4,   13,   22,   13, & ! 2   
     &      0,   59,   15,   30,   30,   37,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,   62,    0, & ! 5   
     &     62,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &     19,   17,    0,    0,   38,   38,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,   38,   24,   38,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   30,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,   29,   95,   97,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     29,    0,    0,    0,    0,    0,    0,    0,   97,    0, & ! 6   
     &     97,   95,    0,    0,    0,    0,    0,    0,   38,    0, & ! 7   
     &      0,    0,   29,  102,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,  102,   30,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &     23,   22,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,   17,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,   30,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &    176,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

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
     &      0,    0,    0,    0,    0,   44,   44,   25,   80,   25, & ! 2   
     &      0,  102,   22,   36,   31,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &     23,   23,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,   17,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,   95,   97,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     24,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &    102,   97,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,  102,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,  102,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &     19,   23,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &    177,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

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
     &      0,    0,    0,    0,    0,   45,   45,   28,    0,   59, & ! 2   
     &      0,   17,   29,    0,   36,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &     38,   19,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,   97,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,   19,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &    178,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

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
     &      0,    0,    0,    0,    0,   48,   13,   59,    0,  102, & ! 2   
     &      0,    0,   30,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,   22,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
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
     &      0,   15,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

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
     &      0,    0,    0,    0,    0,   60,   15,   54,    0,   80, & ! 2   
     &      0,    0,   59,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,   24,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
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
     &      0,   24,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

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
     &      0,    0,    0,    0,    0,   13,   17,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,   38,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

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
     &      0,    0,    0,    0,    0,   15,   19,    0,    0,    0, & ! 2   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

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
     &      0,    0,    0,    0,    0,   17,   22,    0,    0,    0, & ! 2   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

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
     &      0,    0,    0,    0,    0,   19,   24,    0,    0,    0, & ! 2   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

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
     &      0,    0,    0,    0,    0,   33,  102,    0,    0,    0, & ! 2   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

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
     &      0,    0,    0,    0,    0,   23,    0,    0,    0,    0, & ! 2   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

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
     &      0,    0,    0,    0,    0,   24,    0,    0,    0,    0, & ! 2   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

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
     &      0,    0,    0,    0,    0,   59,    0,    0,    0,    0, & ! 2   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

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
     &      0,    0,    0,    0,    0,  102,    0,    0,    0,    0, & ! 2   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!2   

      DATA ( RTDAT( 1,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.7000D-12, & ! 3   
     &     1.0000D-14, 3.0000D-12, 1.2000D-13, 6.1000D-34, 8.0000D-12, & ! +   
     &     3.3000D-11, 2.1500D-11, 1.6300D-10, 2.8000D-12, 4.8000D-11, & ! 4   
     &     3.0000D-13, 4.2000D-34, 1.8000D-12, 9.1000D-32, 7.1000D-31, & ! +   
     &     3.4400D-12, 6.0950D-14, 4.2500D-39, 3.0000D-12, 5.3000D-12, & ! 5   
     &     3.4000D-31, 1.8000D-30, 2.4000D-14, 2.0000D-11, 3.5000D-12, & ! +   
     &     1.7000D-11, 4.3500D-14, 8.5000D-13, 2.4000D-30, 5.8000D-27, & ! 6   
     &     1.0000D-22, 1.9000D-31, 2.1000D-27, 4.5000D-13, 2.9000D-31, & ! +   
     &     1.4400D-13, 2.4500D-12, 7.6600D-12, 7.6800D-12, 1.0100D-11, & ! 7   
     &     2.8200D-11, 1.0000D-28, 5.7200D-12, 1.3300D-11, 5.5000D-30, & ! +   
     &     2.3300D-12, 1.8100D-12, 2.3100D-11, 7.1600D-12, 2.7000D-11, & ! 8   
     &     1.2100D-11, 4.2000D-11, 5.5000D-12, 4.7000D-12, 4.9000D-12, & ! +   
     &     4.5600D-14, 1.5000D-12, 2.8000D-12, 3.0000D-12, 8.0000D-12, & ! 9   
     &     2.6000D-12, 5.7700D-12, 1.1000D-11, 9.2600D-13, 2.8000D-11, & ! +   
     &     2.8000D-11, 1.0000D-11, 5.3200D-12, 6.7500D-12, 4.6500D-11, & ! O   
     &     2.8000D-11, 2.0500D-10, 2.8500D-12, 3.0000D-12, 2.6000D-12, & ! +   
     &     1.4700D-11, 2.9000D-12, 3.4000D-12, 1.0000D-10, 3.0000D-11, & ! 1   
     &     4.5000D-13, 4.0000D-14, 2.9300D-12, 4.0000D-14, 4.0000D-14, & ! +   
     &     3.2000D-11, 5.3100D-12, 5.6000D-12, 1.3000D-11, 9.1400D-15, & ! 2   
     &     4.3300D-15, 4.4000D-15, 7.8600D-15, 5.0000D-16, 2.9500D-15, & ! +   
     &     1.3600D-15, 8.5000D-16, 1.6600D-18, 2.0000D-16, 2.0000D-16, & ! 3   
     &     9.0000D-17, 5.0000D-16, 2.8600D-13, 4.3920D-13, 1.7900D-13, & ! +   
     &     8.6400D-13, 3.0300D-12, 1.1900D-12, 1.2200D-11, 2.0000D-12, & ! 4   
     &     1.4000D-12, 3.7600D-12, 3.4000D-15, 5.0200D-13, 2.9000D-12, & ! +   
     &     3.7600D-12, 3.7800D-12, 1.0600D-12, 2.8700D-13, 2.0100D-10, & ! 5   
     &     2.2000D-14, 1.0000D+03, 1.0000D+03, 1.0000D+03, 1.0000D+03, & ! +   
     &     1.0000D+03, 1.0000D+03, 9.7000D-29, 9.0000D-29, 9.7000D-29, & ! 6   
     &     9.0000D-29, 2.8000D-12, 1.6000D+16, 2.8000D-12, 2.6000D-12, & ! +   
     &     4.0000D-12, 4.0000D-12, 4.0000D-12, 9.0000D-12, 4.0000D-12, & ! 7   
     &     4.0000D-12, 2.5400D-12, 4.0000D-12, 2.7000D-12, 2.7000D-12, & ! +   
     &     4.0000D-12, 2.7000D-12, 2.7000D-12, 2.7000D-12, 2.4300D-12, & ! 8   
     &     4.0000D-12, 4.0000D-12, 8.1000D-12, 8.1000D-12, 2.9000D-12, & ! +   
     &     4.0000D-12, 4.0000D-12, 2.5400D-12, 2.5400D-12, 2.5400D-12, & ! 9   
     &     2.5400D-12, 4.0000D-12, 4.0000D-12, 2.7000D-12, 2.7000D-12, & ! +   
     &     4.0000D-12, 4.0000D-12, 4.0000D-12, 2.7000D-12, 4.0000D-12, & ! O   
     &     2.0000D-11, 2.0000D-11, 2.0800D-12, 4.1000D-13, 7.5000D-13, & ! +   
     &     1.6600D-13, 1.6600D-13, 1.6600D-13, 1.9000D-13, 1.6600D-13, & ! 1   
     &     1.6600D-13, 2.9100D-13, 3.7500D-13, 3.7500D-13, 3.7500D-13, & ! +   
     &     3.7500D-13, 3.7500D-13, 3.7500D-13, 3.7500D-13, 2.0500D-13, & ! 2   
     &     1.5000D-11, 1.5000D-11, 4.3000D-13, 4.3000D-13, 1.1500D-13, & ! +   
     &     1.1500D-13, 1.1500D-13, 1.8200D-13, 1.8200D-13, 2.9100D-13, & ! 3   
     &     2.9100D-13, 3.7500D-13, 1.0000D-11, 3.7500D-13, 1.1500D-13, & ! +   
     &     1.6600D-13, 1.6600D-13, 3.7500D-13, 1.6600D-13, 9.5000D-14, & ! 4   
     &     1.1800D-13, 9.4600D-14, 1.0000D-13, 4.3400D-14, 1.7100D-13, & ! +   
     &     1.4600D-13, 9.1800D-14, 3.5600D-14, 3.5600D-14, 3.5600D-14, & ! 5   
     &     3.5600D-14, 3.5600D-14, 3.5600D-14, 3.5600D-14, 3.5600D-14, & ! +   
     &     3.4000D-14, 3.5600D-14, 3.5600D-14, 2.0000D-11, 2.0000D-11, & ! 6   
     &     7.5000D-13, 6.9100D-13, 6.9100D-13, 3.4000D-14, 3.4000D-14, & ! +   
     &     8.3700D-14, 3.4000D-14, 3.5600D-14, 3.5600D-14, 3.5600D-14, & ! 7   
     &     3.5600D-14, 7.5000D-13, 1.6000D-13, 9.6800D-14, 3.5600D-14, & ! +   
     &     5.9900D-15, 1.0300D-12, 6.9000D-13, 5.5900D-13, 2.4700D-13, & ! 8   
     &     9.4800D-13, 8.1100D-13, 5.0900D-13, 7.4000D-13, 7.4000D-13, & ! +   
     &     7.4000D-13, 7.4000D-13, 7.4000D-13, 7.4000D-13, 7.4000D-13, & ! 9   
     &     7.4000D-13, 8.4000D-14, 7.4000D-13, 7.4000D-13, 2.5000D-12, & ! +   
     &     2.5000D-12, 7.5100D-13, 7.5100D-13, 7.5100D-13, 8.4000D-14, & ! O   
     &     8.4000D-14, 1.6800D-12, 1.6800D-12, 7.4000D-13, 7.4000D-13, & ! +   
     &     7.4000D-13, 7.4000D-13, 7.5100D-13, 8.8500D-13, 5.3700D-13, & ! 1   
     &     7.4000D-13, 3.4000D-14, 2.5000D-12, 1.2000D-12, 1.2000D-12, & ! +   
     &     1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, & ! 2   
     &     1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, & ! +   
     &     1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, & ! 3   
     &     1.2000D-12, 1.2000D-12, 4.0000D-12, 4.0000D-12, 1.2000D-12, & ! +   
     &     1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 2.5000D-12, & ! 4   
     &     2.5000D-12, 2.5000D-12, 2.5000D-12, 1.2000D-12, 1.2000D-12, & ! +   
     &     1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 7.0000D-14, & ! 5   
     &     4.2500D-14, 2.9600D-14, 1.2000D-12, 2.5000D-12, 7.1300D-17, & ! +   
     &     2.7000D-12, 1.9000D-13, 2.7000D-12, 1.9000D-13, 2.7000D-12, & ! 6   
     &     1.9000D-13, 2.7000D-12, 5.7800D-11, 2.8200D-11, 6.5500D-14, & ! +   
     &     2.3100D-11, 2.7000D-12, 1.9000D-13, 2.0000D-11, 2.6100D-19, & ! 7   
     &     1.1500D-15, 1.4800D-11, 1.3400D-14, 1.7900D-13, 5.0100D-11, & ! +   
     &     2.7000D-12, 3.7500D-13, 4.4000D-11, 3.4300D-17, 8.9900D-12, & ! 8   
     &     1.2000D-11, 1.9000D-11, 2.8400D-13, 2.7000D-12, 2.3000D-12, & ! +   
     &     1.2000D-14, 1.9700D-10, 2.8400D-13, 2.3000D-12, 2.7000D-12, & ! 9   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 6.7006D-11, & ! +   
     &     1.0000D+00, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! O   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 1   
     &     2.5000D-12, 1.0000D+00, 2.5000D-12, 1.0000D+00, 1.2500D-11, & ! +   
     &     4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, & ! 2   
     &     4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11/!+   

      DATA ( RTDAT( 2,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-2.4000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     4.6000D+02, 2.6600D+03, 0.0000D+00,-1.5000D+00,-2.6000D+00, & ! +   
     &     0.0000D+00, 2.7000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &    -1.6000D+00,-3.0000D+00, 4.6000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-3.0000D+00, 1.0840D+04, & ! 6   
     &     0.0000D+00,-3.4000D+00, 1.0900D+04, 0.0000D+00,-4.1000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00,-4.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.6500D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-5.6000D+00, 1.4000D+04,-5.6000D+00, & ! 6   
     &     1.4000D+04, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.4153D-08, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

      DATA ( RTDAT( 3,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-9.4000D+02, & ! 3   
     &    -4.9000D+02,-1.5000D+03,-2.4500D+03, 0.0000D+00,-2.0600D+03, & ! +   
     &     5.5000D+01, 1.1000D+02, 6.0000D+01,-1.8000D+03, 2.5000D+02, & ! 4   
     &     2.1000D-33, 2.9400D-54, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.6000D+02, 6.8570D-34, 6.6350D+02, 2.5000D+02, 2.0000D+02, & ! 5   
     &     0.0000D+00, 0.0000D+00, 2.7000D-17, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.2500D+02,-1.3350D+03,-2.4500D+03, 0.0000D+00, 6.4000D+01, & ! 6   
     &     0.0000D+00, 0.0000D+00, 6.7000D+01, 6.1000D+02, 0.0000D+00, & ! +   
     &     2.7400D-33,-1.7750D+03,-1.0200D+03,-3.7000D+02,-2.4500D+02, & ! 7   
     &    -2.7300D+02, 0.0000D+00, 5.0000D+02, 5.0000D+02, 0.0000D+00, & ! +   
     &    -1.9300D+02, 3.5400D+02, 0.0000D+00, 0.0000D+00, 3.9000D+02, & ! 8   
     &     4.4000D+02, 4.0100D+02, 1.2500D+02, 3.4500D+02, 4.0500D+02, & ! +   
     &    -4.2700D+02,-9.0000D+01, 1.0000D+01, 0.0000D+00, 3.8000D+02, & ! 9   
     &     6.1000D+02, 5.3300D+02, 0.0000D+00, 8.3000D+02, 1.7500D+02, & ! +   
     &     1.7500D+02, 0.0000D+00, 2.4300D+02, 4.0500D+02, 0.0000D+00, & ! O   
     &     1.7500D+02, 0.0000D+00,-3.4500D+02, 2.0000D+01, 2.0000D+02, & ! +   
     &     0.0000D+00, 1.9000D+02, 1.9000D+02, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 8.5000D+02, 1.9000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-2.6000D+02, 2.7000D+02, 0.0000D+00,-2.5800D+03, & ! 2   
     &    -1.8000D+03,-8.4500D+02,-1.9130D+03,-5.3000D+02,-7.8300D+02, & ! +   
     &    -2.1120D+03,-1.5200D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-2.2820D+03,-4.5000D+02, & ! +   
     &     4.5000D+02,-4.4600D+02, 4.9000D+02, 0.0000D+00,-2.4400D+03, & ! 4   
     &    -1.9000D+03,-1.9000D+03, 0.0000D+00,-1.0760D+03,-1.9000D+03, & ! +   
     &    -1.9000D+03, 0.0000D+00, 0.0000D+00,-1.0000D+03, 0.0000D+00, & ! 5   
     &    -5.0000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.6300D+02, 0.0000D+00, & ! 6   
     &     1.6500D+02, 1.8100D+02,-1.3486D+04, 3.0000D+02, 3.6500D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 3.6000D+02, 0.0000D+00, 3.6000D+02, 3.6000D+02, & ! +   
     &     0.0000D+00, 3.6000D+02, 3.6000D+02, 3.6000D+02, 3.6000D+02, & ! 8   
     &     0.0000D+00, 0.0000D+00, 2.7000D+02, 2.7000D+02, 3.0000D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.6000D+02, 3.6000D+02, 3.6000D+02, & ! 9   
     &     3.6000D+02, 0.0000D+00, 0.0000D+00, 3.6000D+02, 3.6000D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.6000D+02, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 7.5000D+02, 7.0000D+02, & ! +   
     &     1.3000D+03, 1.3000D+03, 1.3000D+03, 1.3000D+03, 1.3000D+03, & ! 1   
     &     1.3000D+03, 1.3000D+03, 9.8000D+02, 9.8000D+02, 9.8000D+02, & ! +   
     &     9.8000D+02, 9.8000D+02, 9.8000D+02, 9.8000D+02, 1.3000D+03, & ! 2   
     &     0.0000D+00, 0.0000D+00, 1.0400D+03, 1.0400D+03, 1.3000D+03, & ! +   
     &     1.3000D+03, 1.3000D+03, 1.3000D+03, 1.3000D+03, 1.3000D+03, & ! 3   
     &     1.3000D+03, 9.8000D+02, 0.0000D+00, 9.8000D+02, 1.3000D+03, & ! +   
     &     1.3000D+03, 1.3000D+03, 9.8000D+02, 1.3000D+03, 3.9000D+02, & ! 4   
     &     1.5800D+02, 4.3100D+02, 4.6700D+02, 6.3300D+02, 7.0800D+02, & ! +   
     &     7.0800D+02, 7.0800D+02, 7.0800D+02, 7.0800D+02, 7.0800D+02, & ! 5   
     &     7.0800D+02, 7.0800D+02, 7.0800D+02, 7.0800D+02, 7.0800D+02, & ! +   
     &     2.2100D+02, 7.0800D+02, 7.0800D+02, 5.0000D+02, 5.0000D+02, & ! 6   
     &     5.0000D+02, 5.0800D+02, 5.0800D+02, 2.2100D+02, 2.2100D+02, & ! +   
     &     0.0000D+00, 2.2100D+02, 7.0800D+02, 7.0800D+02, 7.0800D+02, & ! 7   
     &     7.0800D+02, 5.0000D+02, 7.0800D+02, 7.0800D+02, 0.0000D+00, & ! +   
     &     1.5100D+03, 2.1100D+02, 4.6000D+02, 5.2200D+02, 6.8300D+02, & ! 8   
     &     7.6500D+02, 7.6500D+02, 7.6500D+02, 7.6500D+02, 7.6500D+02, & ! +   
     &     7.6500D+02, 7.6500D+02, 7.6500D+02, 7.6500D+02, 7.6500D+02, & ! 9   
     &     7.6500D+02, 2.2100D+02, 7.6500D+02, 7.6500D+02, 5.0000D+02, & ! +   
     &     5.0000D+02, 5.6500D+02, 5.6500D+02, 5.6500D+02, 2.2100D+02, & ! O   
     &     2.2100D+02, 5.0000D+02, 5.0000D+02, 7.6500D+02, 7.6500D+02, & ! +   
     &     7.0800D+02, 7.0800D+02, 5.6500D+02, 7.6500D+02, 7.6500D+02, & ! 1   
     &     7.0800D+02, 1.5600D+03, 5.0000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+03, & ! 5   
     &     1.0000D+03, 1.0000D+03, 0.0000D+00, 5.0000D+02, 2.9500D+03, & ! +   
     &     3.6000D+02, 1.3000D+03, 3.6000D+02, 1.3000D+03, 3.6000D+02, & ! 6   
     &     1.3000D+03, 3.7400D+02,-4.0000D+02,-2.7300D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.6000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 4.4800D+02,-2.2830D+03, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.6000D+02, 9.8000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 1.3000D+03, 3.6000D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.3000D+03, 0.0000D+00, 3.6000D+02, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D-06, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     46,   47,   49,   50,   52,   56,   57,   58,   64,   67, & 
     &     70,   71,   77,   80,  163,  165,  400/

      DATA ( RFDAT( 1,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     9.2000D+02, 3.1200D+03, 3.0000D-11, 3.6000D-11, 2.7000D+02, & 
     &     2.3000D-11, 2.8000D-11, 2.1990D+03, 1.6000D-12, 4.0000D-12, & 
     &     1.7000D-12, 0.0000D+00, 8.8000D-12, 8.3000D-13, 9.3000D-12, & 
     &     9.3000D-12, 1.0743D+01/

      DATA ( RFDAT( 2,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.0000D-01,-1.0000D+00, & 
     &    -2.0000D-01, 0.0000D+00, 6.5000D-34, 1.0000D-01,-3.0000D-01, & 
     &     2.0000D-01, 0.0000D+00,-8.5000D-01, 2.0000D+00,-1.5000D+00, & 
     &    -1.5000D+00,-6.7130D-01/

      DATA ( RFDAT( 3,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 1.3350D+03, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00/

      DATA ( RFDAT( 4,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 6.0000D-01, 6.0000D-01,-5.9680D-14, & 
     &     6.0000D-01, 6.0000D-01, 0.0000D+00, 6.0000D-01, 6.0000D-01, & 
     &     6.0000D-01, 0.0000D+00, 6.0000D-01, 6.0000D-01, 6.0000D-01, & 
     &     6.0000D-01, 0.0000D+00/

      DATA ( RFDAT( 5,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 2.7000D+02, & 
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 0.0000D+00/

      REAL( 8 )               :: SC( NRXNS,MXPRD )

      DATA ( SC( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D-01, 1.0000D+00, & ! +   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, & ! 1   
     &     1.2200D+00, 1.0000D-01, 1.5000D+00, 1.0000D+00, 3.4000D-01, & ! +   
     &     3.0000D-01, 2.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! 2   
     &     1.5000D+00, 1.5000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, 0.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 7.0000D-01, & ! +   
     &     2.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     4.9000D-02, 1.0000D+00, 1.0000D+00, 1.0000D+00, 6.5000D-01, & ! +   
     &     6.4800D-01, 1.7700D-01, 1.7700D-01, 1.7700D-01, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.7000D-01, & ! 9   
     &     1.0000D+00, 3.1300D-01, 1.0000D+00, 1.0000D+00, 5.2000D-01, & ! +   
     &     5.2000D-01, 5.6000D-01, 1.0000D+00, 7.3000D-01, 7.3000D-01, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 3.5000D-01, 1.0000D-02, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 6.4000D-01, 3.5000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 8.0000D-02, & ! 2   
     &     2.2000D-01, 4.6000D-01, 2.5000D-01, 8.5000D-01, 8.5000D-01, & ! +   
     &     1.9000D-01, 1.6000D-01, 1.0000D-01, 5.0000D-02, 5.0000D-02, & ! 3   
     &     5.0000D-02, 5.0000D-02, 1.0000D+00, 8.0000D-01, 4.3000D-01, & ! +   
     &     1.1000D-01, 1.0000D+00, 1.0000D-01, 7.1000D-01, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 6.8000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 4.0000D-01, 4.0000D-01, 5.0000D-01, 1.0000D+00, & ! 5   
     &     1.0000D+00, 2.8000D-01, 4.9000D-01, 1.5800D-01, 3.9000D-01, & ! +   
     &     1.5800D-01, 3.9000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     6.6000D-01, 2.0000D-01, 6.0600D-01, 1.0000D+00, 7.8000D-01, & ! 7   
     &     8.3000D-01, 9.1800D-01, 1.0000D+00, 9.5000D-01, 5.0000D-01, & ! +   
     &     1.0000D+00, 9.5000D-01, 9.5000D-01, 9.5000D-01, 8.8000D-01, & ! 8   
     &     8.2000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     6.7000D-01, 7.7000D-01, 6.5000D-01, 1.0000D+00, 3.0000D-01, & ! 9   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 2.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 4.4000D-01, 4.4000D-01, 1.5000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 7.4000D-01, & ! 4   
     &     1.0000D+00, 8.9400D-01, 8.4200D-01, 9.1000D-01, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.6000D+00, 1.0000D+00, 2.0000D+00, & ! 5   
     &     2.0000D+00, 1.0000D+00, 2.0000D+00, 2.0000D+00, 2.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 9.0000D-01, 9.0000D-01, & ! 6   
     &     5.0000D-01, 8.3400D-01, 1.0000D+00, 5.0000D-01, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 5.0000D-01, 1.0000D+00, & ! +   
     &     1.0000D+00, 5.0000D-01, 3.9400D-01, 3.4200D-01, 3.0300D-01, & ! 8   
     &     5.0000D-01, 5.0000D-01, 5.0000D-01, 6.0000D-01, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 5.0000D-01, 5.0000D-01, 5.0000D-01, 2.0000D+00, & ! +   
     &     1.0000D+00, 5.0000D-01, 3.3000D-01, 5.0000D-01, 6.3500D-01, & ! O   
     &     1.0000D+00, 5.0000D-01, 5.0000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.0000D-01, & ! 1   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     2.5400D-01, 4.8800D-01, 8.2000D-01, 1.0000D+00, 4.7000D-01, & ! 2   
     &     8.6000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.0000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     6.7000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 3.0000D-01, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 2.0000D+00, 1.0000D+00, & ! 5   
     &     5.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 4.9000D-02, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 4.9000D-01, & ! +   
     &     8.0000D-02, 6.0000D-01, 6.9000D-01, 2.0000D-02, 1.0000D+00, & ! 8   
     &     6.1300D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     9.8200D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 5.0000D-01, 0.0000D+00, & ! +   
     &     1.0000D+00, 8.5710D-01, 1.1429D+00, 8.5710D-01, 1.1429D+00, & ! O   
     &     7.1430D-01, 7.1430D-01, 8.0000D-01, 9.0000D-01, 5.0000D-01, & ! +   
     &     5.0000D-01, 1.4286D+00, 1.4286D+00, 1.7143D+00, 1.7143D+00, & ! 1   
     &     1.2500D+00, 1.0000D+00, 1.2500D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00/!+   

      DATA ( SC( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 8.0000D-01, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     7.8400D-01, 1.0000D+00, 5.0000D-01, 1.0000D+00, 6.6000D-01, & ! +   
     &     3.0000D-01, 0.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! 2   
     &     2.5000D-01, 2.5000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 7.0000D-01, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.3100D-05, 1.5800D-03, & ! 7   
     &     9.5100D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.5000D-01, & ! +   
     &     3.5200D-01, 7.6300D-01, 7.6300D-01, 7.6300D-01, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 4.3000D-01, & ! 9   
     &     0.0000D+00, 6.8700D-01, 2.0000D+00, 1.0000D+00, 3.3000D-01, & ! +   
     &     3.3000D-01, 2.1000D-01, 0.0000D+00, 2.0000D-01, 2.0000D-01, & ! O   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 7.1900D-01, & ! +   
     &     1.0000D+00, 6.5000D-01, 4.4000D-01, 1.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 3.6000D-01, 6.5000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 7.0000D-02, 1.5000D-01, & ! 2   
     &     3.2000D-01, 7.0000D-02, 2.5000D-01, 1.0000D-01, 1.0000D-01, & ! +   
     &     1.4000D-01, 1.1000D-01, 7.2000D-02, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.5000D+00, 0.0000D+00, 2.0000D-01, 5.7000D-01, & ! +   
     &     8.9000D-01, 1.0000D+00, 9.0000D-01, 2.9000D-01, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 3.2000D-01, 1.0000D+00, 2.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D-01, 1.0000D-01, 1.5000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 2.9000D-01, 1.0000D-02, 3.0800D-01, 1.0000D-02, & ! +   
     &     3.0800D-01, 1.0000D-02, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! 6   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.3100D-01, 5.1000D-02, 1.3300D-01, 1.0000D+00, 9.7000D-01, & ! 7   
     &     9.5000D-01, 9.1800D-01, 1.0000D+00, 9.5000D-01, 9.5000D-01, & ! +   
     &     1.0000D+00, 9.5000D-01, 9.5000D-01, 9.5000D-01, 8.8000D-01, & ! 8   
     &     8.2000D-01, 6.8000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 2.3000D-01, 3.5000D-01, 5.0000D-01, 7.0000D-01, & ! 9   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.8700D-01, 1.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 4.4000D-01, 4.4000D-01, 1.5000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.3700D+00, & ! 4   
     &     7.5000D-01, 8.0000D-02, 1.8000D-02, 9.0000D-02, 1.9500D+00, & ! +   
     &     1.5000D+00, 7.5000D-01, 4.5900D-01, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.3100D+00, 7.5000D-01, 1.0400D+00, 9.0000D-01, 9.0000D-01, & ! 6   
     &     5.0000D-01, 1.0000D+00, 7.5000D-01, 2.6900D-01, 1.0000D+00, & ! +   
     &     1.1600D+00, 3.0500D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 5.0000D-01, 7.0000D-01, & ! +   
     &     1.0000D+00, 5.0000D-01, 5.8000D-01, 5.1800D-01, 5.0000D-01, & ! 8   
     &     5.0000D-01, 5.0000D-01, 5.0000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 5.0000D-01, 5.0000D-01, 5.0000D-01, 0.0000D+00, & ! +   
     &     1.0000D+00, 5.0000D-01, 5.0000D-01, 5.0000D-01, 5.0000D-01, & ! O   
     &     5.0000D-01, 5.0000D-01, 5.0000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.4000D-01, 5.5000D-02, 1.8000D-01, 1.0000D+00, 7.9000D-01, & ! 2   
     &     7.2000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 5.3800D-01, 1.0000D+00, 7.0000D-01, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.8700D-01, 1.0000D+00, 2.0000D+00, & ! 5   
     &     5.0000D-01, 5.0400D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 4.7000D-01, 0.0000D+00, 9.5100D-01, 1.0100D-03, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 5.8000D-01, 5.2000D-01, 4.5000D-02, 4.9000D-01, & ! +   
     &     9.2000D-01, 4.0000D-01, 3.1000D-01, 1.0000D+00, 8.0000D-01, & ! 8   
     &     3.8700D-01, 0.0000D+00, 0.0000D+00, 2.0000D+00, 2.0000D+00, & ! +   
     &     1.8000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 7.5300D-01, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     4.8570D-01, 3.0030D-01, 3.8560D-01, 2.1810D-01, 2.4120D-01, & ! 2   
     &     6.6640D-01, 2.8580D-01, 3.3030D-01, 3.4440D-01, 3.8860D-01/!+   

      DATA ( SC( IRXXN,  3 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 8.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     1.2200D+00, 9.0000D-01, 5.0000D-01, 1.0000D+00, 6.7000D-01, & ! +   
     &     7.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! 2   
     &     2.0000D-01, 2.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 2.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     2.5000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.5000D-01, & ! +   
     &     1.1800D-01, 6.0000D-02, 6.0000D-02, 6.0000D-02, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.0000D-01, & ! +   
     &     1.3000D-01, 1.1000D-01, 0.0000D+00, 7.0000D-02, 7.0000D-02, & ! O   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.8400D-01, & ! +   
     &     0.0000D+00, 3.5000D-01, 7.0000D-02, 9.0400D-01, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 3.5000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 7.0000D-02, 4.3000D-01, & ! 2   
     &     8.0000D-02, 3.2000D-01, 8.0000D-02, 2.0000D-01, 1.6000D-01, & ! +   
     &     1.0000D-01, 2.8000D-01, 8.0000D-03, 6.0000D-01, 6.0000D-01, & ! 3   
     &     1.5000D+00, 1.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 6.8000D-01, 6.6800D-01, 1.0000D+00, & ! +   
     &     1.0000D+00, 5.0000D-01, 5.0000D-01, 1.5000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 2.8000D-01, 5.0000D-01, 2.5000D-01, 3.0000D-01, & ! +   
     &     2.5000D-01, 5.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     4.8000D-02, 2.3100D-01, 4.1600D-01, 1.6000D+00, 7.8000D-01, & ! 7   
     &     8.1000D-01, 4.5900D-01, 0.0000D+00, 9.5000D-01, 5.0000D-01, & ! +   
     &     0.0000D+00, 9.5000D-01, 9.5000D-01, 3.5000D-01, 2.0000D-01, & ! 8   
     &     2.3000D-01, 4.3000D-01, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     3.3000D-01, 1.6000D-01, 1.0000D+00, 5.0000D-01, 7.0000D-01, & ! 9   
     &     6.1000D-01, 0.0000D+00, 0.0000D+00, 3.2000D-01, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.2400D+00, 1.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 1.5000D-01, 1.5000D-01, 1.5000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 6.3000D-01, & ! 4   
     &     7.5000D-01, 2.6000D-02, 1.4000D-01, 2.8100D-01, 1.5000D-01, & ! +   
     &     7.0500D-01, 1.2800D+00, 1.0000D+00, 1.0000D+00, 2.7100D-01, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 3.6800D-01, & ! +   
     &     1.5900D-01, 7.5000D-01, 1.9200D-01, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.5000D+00, 3.3400D-01, 5.0000D-01, 5.0000D-01, 1.5000D+00, & ! +   
     &     1.1600D+00, 7.7300D-01, 1.0000D+00, 1.0000D+00, 3.2000D-01, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 9.6500D-01, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 2.6000D-02, 1.4000D-01, 6.7000D-02, & ! 8   
     &     1.6000D+00, 1.0000D+00, 1.7100D+00, 4.5900D-01, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     3.6800D-01, 1.0480D+00, 1.0000D+00, 1.9200D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 3.3000D-01, 5.0000D-01, 2.6900D-01, & ! O   
     &     1.0000D+00, 1.1600D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.2000D-01, 1.0000D+00, 0.0000D+00, 1.0000D+00, 2.8700D-01, & ! 1   
     &     7.0000D-01, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     9.2000D-02, 2.8000D-01, 5.6300D-01, 1.6000D+00, 7.9000D-01, & ! 2   
     &     1.1000D-01, 5.0000D-01, 0.0000D+00, 1.0000D+00, 5.0000D-01, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 3.6800D-01, 7.5000D-01, & ! 3   
     &     1.0000D+00, 3.8500D-01, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     3.3000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 7.0000D-01, & ! 4   
     &     6.1000D-01, 0.0000D+00, 0.0000D+00, 3.2000D-01, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.2400D+00, 1.0000D+00, 0.0000D+00, & ! 5   
     &     2.0200D-01, 1.2100D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.4900D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 5.1000D-01, & ! +   
     &     9.2000D-01, 4.0000D-01, 6.6000D-01, 0.0000D+00, 2.0000D-01, & ! 8   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     6.2000D-03, 2.8620D-01, 9.5000D-02, 3.0630D-01, 2.0890D-01, & ! 2   
     &     1.4300D-02, 3.9310D-01, 2.2720D-01, 2.7490D-01, 2.4210D-01/!+   

      DATA ( SC( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     3.5000D-01, 1.0000D-01, 0.0000D+00, 0.0000D+00, 3.3000D-01, & ! +   
     &     7.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 8.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     2.4000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 6.5000D-01, & ! +   
     &     5.3000D-01, 1.7700D-01, 1.7700D-01, 1.7700D-01, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 7.8000D-01, & ! +   
     &     1.0000D-01, 2.7000D-01, 0.0000D+00, 7.3000D-01, 7.3000D-01, & ! O   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 8.0000D-02, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 3.5000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! 2   
     &     6.0000D-02, 7.0000D-02, 1.0000D-01, 4.2000D-01, 4.2000D-01, & ! +   
     &     2.2000D-01, 1.0000D-02, 2.0000D-03, 6.0000D-01, 6.0000D-01, & ! 3   
     &     4.8000D-01, 8.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 6.8000D-01, 3.3200D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 5.0000D-01, 5.0000D-01, 1.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 1.5000D-01, 4.9000D-01, 3.0800D-01, 4.9000D-01, & ! +   
     &     1.5000D-01, 4.9000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     8.9000D-02, 2.3500D-01, 7.3900D-01, 2.0000D-01, 1.2000D-02, & ! 7   
     &     6.8000D-01, 4.5900D-01, 0.0000D+00, 5.0000D-02, 5.0000D-01, & ! +   
     &     0.0000D+00, 5.0000D-02, 9.5000D-01, 6.0000D-01, 2.8000D-01, & ! 8   
     &     4.3000D-01, 7.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     6.7000D-01, 1.0000D+00, 6.5000D-01, 1.0000D+00, 1.0000D+00, & ! 9   
     &     3.0000D-02, 0.0000D+00, 0.0000D+00, 6.8000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.6400D-01, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 4.1000D-01, 4.1000D-01, 8.5000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     2.5000D-01, 2.6000D-02, 1.9100D-01, 7.5000D-01, 2.5000D-01, & ! +   
     &     4.5000D-02, 2.1800D-01, 4.5900D-01, 0.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 6.3200D-01, & ! +   
     &     2.5000D-01, 7.5000D-01, 3.0800D-01, 1.0000D-01, 1.0000D-01, & ! 6   
     &     2.5000D-01, 2.5000D-01, 2.5000D-01, 1.6600D+00, 5.0000D-01, & ! +   
     &     1.5000D+00, 2.0300D-01, 0.0000D+00, 0.0000D+00, 6.8000D-01, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 9.3000D-01, 7.0000D-01, & ! +   
     &     0.0000D+00, 5.0000D-01, 2.6000D-02, 1.9100D-01, 2.0800D-01, & ! 8   
     &     2.0000D-01, 9.4000D-01, 2.9000D-01, 4.5800D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! 9   
     &     6.3200D-01, 2.1900D-01, 1.0000D+00, 3.8500D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 7.5000D-01, 3.3400D-01, 5.0000D-01, 5.0000D-01, & ! O   
     &     5.0000D-01, 1.1600D+00, 3.0000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     6.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.2400D+00, & ! 1   
     &     7.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     5.0300D-01, 4.8500D-01, 1.0000D+00, 2.0000D-01, 1.0000D+00, & ! 2   
     &     1.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, 5.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 6.3200D-01, 3.1800D-01, & ! 3   
     &     1.0000D+00, 3.8500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     6.7000D-01, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     3.0000D-02, 0.0000D+00, 0.0000D+00, 6.8000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.6400D-01, 0.0000D+00, 0.0000D+00, & ! 5   
     &     6.4000D-01, 2.8500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     7.5000D-01, 3.2000D-01, 3.4000D-01, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.5000D-03, 4.1000D-03, 1.3730D-01, 1.5300D-02, 3.0000D-01, & ! 2   
     &     1.2300D-02, 1.3900D-02, 2.6070D-01, 4.9100D-02, 6.4000D-02/!+   

      DATA ( SC( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     4.3400D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.4000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     5.0000D-01, 5.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     4.2000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.5000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 9.8000D-01, 1.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-01, & ! +   
     &     1.0000D-02, 1.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.1000D-01, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.7000D-01, & ! 2   
     &     4.0000D-02, 4.0000D-02, 1.0000D-01, 2.0000D-02, 2.0000D-02, & ! +   
     &     5.0000D-01, 5.6000D-01, 1.0000D-01, 1.5000D+00, 1.5000D+00, & ! 3   
     &     7.0000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 3.2000D-01, 3.3200D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, & ! 5   
     &     0.0000D+00, 2.8000D-01, 1.0000D-02, 1.5000D-01, 1.0000D-02, & ! +   
     &     3.0800D-01, 1.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     9.3500D-01, 8.6400D-01, 1.5000D-01, 0.0000D+00, 4.4000D-01, & ! 7   
     &     2.0000D-01, 9.1800D-01, 0.0000D+00, 0.0000D+00, 5.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0500D+00, 7.0000D-01, 4.4000D-01, & ! 8   
     &     1.1000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 4.6000D-01, 6.5000D-01, 0.0000D+00, 3.0000D-01, & ! 9   
     &     2.7000D-01, 0.0000D+00, 0.0000D+00, 6.8000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     2.5000D-01, 8.2700D-01, 7.7700D-01, 1.9700D-01, 2.5000D-01, & ! +   
     &     2.5000D-01, 2.5000D-01, 6.0000D-01, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0500D+00, 7.3700D-01, & ! +   
     &     2.5000D-01, 2.5000D-01, 2.5000D-01, 0.0000D+00, 0.0000D+00, & ! 6   
     &     2.5000D-01, 2.5000D-01, 2.5000D-01, 6.7000D-02, 2.5000D-01, & ! +   
     &     1.7500D+00, 5.2500D-01, 0.0000D+00, 0.0000D+00, 6.8000D-01, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.4800D-01, 7.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.3000D-01, 4.2000D-02, 2.1700D-01, & ! 8   
     &     5.0000D-01, 6.0000D-02, 5.0000D-01, 6.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0500D+00, & ! 9   
     &     7.3700D-01, 3.0500D-01, 1.0000D+00, 3.0800D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, 1.0000D+00, & ! O   
     &     5.0000D-01, 1.0000D+00, 2.7000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     6.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.6400D-01, & ! 1   
     &     7.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0300D-01, 0.0000D+00, 1.8000D-01, & ! 2   
     &     2.0000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0500D+00, 7.3700D-01, 5.0000D-01, & ! 3   
     &     0.0000D+00, 6.1500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.0000D-01, & ! 4   
     &     2.7000D-01, 0.0000D+00, 0.0000D+00, 6.8000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     1.4900D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.4000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.7000D-01, 8.0000D-02, 4.3000D-01, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.6000D-03, 3.5000D-03, 5.0000D-04, 1.0430D-01, 2.0280D-01, & ! 2   
     &     1.2390D-01, 1.0270D-01, 7.0200D-02, 2.5770D-01, 3.8500D-02/!+   

      DATA ( SC( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     2.1600D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 6.7000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     5.0000D-01, 5.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-02, & ! +   
     &     7.8000D-01, 7.9000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     2.0000D-02, 9.0000D-02, 9.0000D-02, 1.4000D-01, 1.4000D-01, & ! +   
     &     4.5000D-01, 1.0000D-01, 2.4300D-01, 5.0000D-02, 5.0000D-02, & ! 3   
     &     2.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 6.8000D-01, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, & ! 5   
     &     0.0000D+00, 1.0000D-02, 0.0000D+00, 2.2400D-01, 0.0000D+00, & ! +   
     &     2.2400D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     5.0400D-01, 1.8000D-02, 6.4200D-01, 0.0000D+00, 6.0000D-02, & ! 7   
     &     9.0000D-02, 8.2000D-02, 0.0000D+00, 0.0000D+00, 5.0000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.0000D-02, 7.3000D-02, 1.2000D-01, & ! 8   
     &     4.4000D-01, 5.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 5.4000D-01, 0.0000D+00, 0.0000D+00, 7.0000D-01, & ! 9   
     &     1.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 1.9800D-01, 2.5100D-01, 6.5200D-01, 0.0000D+00, & ! +   
     &     2.5000D-01, 2.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 7.7000D-02, & ! +   
     &     2.5000D-01, 2.5000D-01, 2.5000D-01, 0.0000D+00, 0.0000D+00, & ! 6   
     &     1.2500D-01, 0.0000D+00, 0.0000D+00, 2.5000D-01, 2.5000D-01, & ! +   
     &     5.0000D-01, 1.3500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-01, 3.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.7300D-01, 3.8100D-01, 6.4200D-01, & ! 8   
     &     0.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     7.7000D-02, 5.0000D-01, 0.0000D+00, 5.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     5.0000D-01, 2.3000D+00, 7.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-01, & ! 1   
     &     3.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     5.1900D-01, 2.4000D-02, 8.6900D-01, 0.0000D+00, 2.0000D-02, & ! 2   
     &     8.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 7.7000D-02, 2.4000D-02, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 7.0000D-01, & ! 4   
     &     7.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     1.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     2.3000D-03, 2.2390D-01, 2.0510D-01, 1.8930D-01, 4.7100D-02, & ! 2   
     &     1.8310D-01, 2.0450D-01, 1.1160D-01, 7.3900D-02, 2.6670D-01/!+   

      DATA ( SC( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 6.7000D-01, & ! +   
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
     &     6.8000D-02, 3.7000D-01, 1.4000D-01, 6.5000D-01, 4.6000D-01, & ! +   
     &     0.0000D+00, 5.4000D-01, 8.0000D-02, 5.0000D-02, 5.0000D-02, & ! 3   
     &     2.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 2.8000D-01, 0.0000D+00, 1.0000D-02, 0.0000D+00, & ! +   
     &     1.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.3200D-01, 4.5000D-02, 2.6100D-01, 0.0000D+00, 1.3000D-01, & ! 7   
     &     2.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.7700D-01, 2.1000D-02, & ! 8   
     &     7.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.0000D-01, & ! 9   
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
     &     0.0000D+00, 4.9700D-01, 6.1800D-01, 2.5000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.8600D-01, & ! +   
     &     2.3000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-01, 0.0000D+00, & ! +   
     &     2.5000D-01, 1.0500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 6.6200D-01, 8.2400D-01, 4.9500D-01, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     1.8600D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 5.0000D-01, 1.8000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.4700D-01, 2.4100D-01, 0.0000D+00, 0.0000D+00, 9.0000D-02, & ! 2   
     &     4.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.8600D-01, 3.3000D-02, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.0000D-01, & ! 4   
     &     1.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     2.9440D-01, 1.8200D-01, 1.7640D-01, 1.6680D-01, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     4.3000D-01, 2.6000D-02, 5.8000D-01, 5.3000D-01, 4.0000D-02, & ! +   
     &     0.0000D+00, 7.0000D-02, 4.2000D-01, 8.0000D-02, 8.0000D-02, & ! 3   
     &     1.1000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 8.4000D-01, 0.0000D+00, & ! +   
     &     8.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.6500D-01, 2.0300D-01, 0.0000D+00, 0.0000D+00, 3.0000D-02, & ! 7   
     &     5.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-02, 2.9000D-02, & ! 8   
     &     1.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     2.1000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 5.0000D-02, 2.5000D-01, 2.5000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.8000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-01, 0.0000D+00, & ! +   
     &     2.5000D-01, 2.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 6.7000D-02, 5.0000D-01, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 1.0830D+00, 1.0500D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     7.5000D-02, 6.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.1000D-02, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     2.1000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     2.0210D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

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
     &     2.0000D-02, 1.0000D-02, 4.6100D-01, 1.0000D+00, 7.9000D-01, & ! +   
     &     0.0000D+00, 7.0000D-02, 2.8000D-02, 6.5000D-01, 7.0000D-01, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     4.2000D-02, 3.3000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.7000D-02, & ! 8   
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
     &     0.0000D+00, 2.5000D-01, 2.5000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.6000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.9200D-01, 2.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     9.5000D-02, 6.3000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
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
     &     1.9000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

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
     &     1.5000D-02, 1.0000D-02, 1.8900D-01, 0.0000D+00, 1.0000D-02, & ! +   
     &     0.0000D+00, 1.0000D-01, 4.9100D-01, 0.0000D+00, 6.5000D-01, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     6.5000D-02, 2.1700D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
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
     &     0.0000D+00, 2.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 2.4700D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
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
     &     2.3000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

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
     &     6.0000D-03, 9.0000D-02, 2.8000D-01, 0.0000D+00, 7.0000D-02, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.0000D-03, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.3000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
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
     &     0.0000D+00, 4.8000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

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
     &     3.2000D-02, 4.5700D-01, 1.5300D-01, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.4000D-02, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.7200D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
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
     &     0.0000D+00, 2.7500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

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
     &     5.6000D-01, 7.3000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.3600D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

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
     &     1.0000D-02, 1.1000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

      DATA ( SC( IRXXN, 15 ), IRXXN = 1, NRXNS ) / & 
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
     &     4.4000D-01, 1.7000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

      DATA ( SC( IRXXN, 16 ), IRXXN = 1, NRXNS ) / & 
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
     &     3.0000D-02, 4.4000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

      DATA ( SC( IRXXN, 17 ), IRXXN = 1, NRXNS ) / & 
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
     &     2.0000D-02, 1.7000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

      DATA ( SC( IRXXN, 18 ), IRXXN = 1, NRXNS ) / & 
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
     &     6.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

      DATA ( SC( IRXXN, 19 ), IRXXN = 1, NRXNS ) / & 
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
     &     1.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

      DATA ( SC( IRXXN, 20 ), IRXXN = 1, NRXNS ) / & 
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
     &     3.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

      DATA ( SC( IRXXN, 21 ), IRXXN = 1, NRXNS ) / & 
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
     &     6.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

      INTEGER            :: NREACT( NRXNS )

      DATA ( NREACT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    2,    2,    2,    2,    1,    2, & ! 3   
     &      1,    1,    1,    1,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    1,    1,    2,    1,    2,    2, & ! 6   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    1,    1,    1,    1, & ! 5   
     &      1,    1,    2,    1,    2,    1,    2,    1,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    1,    1,    1,    1,    1, & ! 9   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2/!2   
      INTEGER            :: NPRDCT( NRXNS )

      DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    1,    2,    1,    2,    2,    2,    4,    1, & ! O   
     &      2,    3,    3,    2,    2,    6,    4,    3,    3,    7, & ! 1   
     &      4,    1,    2,    2,    3,    6,    6,    3,    3,    3, & ! 2   
     &      2,    4,    2,    2,    1,    1,    1,    1,    1,    0, & ! 3   
     &      1,    1,    1,    1,    0,    1,    1,    1,    1,    1, & ! 4   
     &      2,    1,    1,    1,    1,    1,    1,    1,    2,    3, & ! 5   
     &      1,    2,    1,    1,    2,    1,    1,    2,    1,    3, & ! 6   
     &      1,    1,    1,    2,    2,    5,    1,    1,    1,    5, & ! 7   
     &      5,    5,    5,    5,    2,    2,    2,    2,    1,    1, & ! 8   
     &      1,    1,    1,    2,    2,    1,    2,    2,    2,    6, & ! 9   
     &      6,    6,    1,    4,    4,    4,    1,    2,    2,    3, & ! O   
     &      2,    3,    5,    3,    1,    1,    2,    4,    3,    3, & ! 1   
     &      2,    2,    3,    3,    5,   21,   17,   12,    9,   12, & ! 2   
     &      6,   10,   12,    9,   10,    8,    5,    1,    2,    2, & ! 3   
     &      2,    2,    3,    3,    3,    2,    2,    6,    6,    3, & ! 4   
     &      3,    4,    4,    6,    2,    2,    7,    5,    8,    5, & ! 5   
     &      8,    5,    1,    2,    1,    2,    1,    2,    3,    3, & ! 6   
     &     10,   13,    7,    4,    8,    8,    6,    2,    4,    6, & ! 7   
     &      2,    4,    6,    8,    9,    8,    6,    2,    2,    3, & ! 8   
     &      4,    6,    5,    4,    7,    8,    2,    2,    5,    2, & ! 9   
     &      3,    3,    4,    3,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    4,    4,    4, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    3,    5,   10,    9,    8,    5, & ! 4   
     &      6,    6,    5,    3,    4,    4,    3,    3,    5,    7, & ! 5   
     &      9,    6,    6,    4,    4,    6,    5,    5,    8,    6, & ! 6   
     &      9,    9,    3,    3,    5,    3,    3,    3,    8,    6, & ! 7   
     &      2,    4,    9,    8,    7,    5,    6,    5,    5,    2, & ! 8   
     &      3,    4,    2,    3,    5,    7,    6,    5,    6,    1, & ! 9   
     &      2,    4,    5,    4,    5,    6,    8,    9,    2,    2, & ! O   
     &      5,    3,    2,    3,    6,    6,    1,    1,    3,    3, & ! 1   
     &      9,   12,    6,    4,    7,    7,    5,    2,    3,    5, & ! 2   
     &      2,    3,    5,    7,    8,    4,    5,    2,    2,    3, & ! 3   
     &      4,    3,    4,    4,    7,    8,    2,    2,    5,    2, & ! 4   
     &      3,    3,    4,    3,    2,    6,    5,    1,    1,    0, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    1,    5,    2, & ! 6   
     &      2,    2,    2,    1,    1,    1,    2,    2,    2,    3, & ! 7   
     &      5,    5,    5,    2,    3,    3,    1,    1,    2,    2, & ! 8   
     &      2,    1,    1,    1,    2,    1,    1,    1,    2,    0, & ! 9   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    3,    1,    3,    1,    2, & ! 1   
     &     10,    7,    7,    7,    6,    6,    6,    6,    6,    6/!2   

      INTEGER, PARAMETER :: MHETERO =   7
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    396,  397,  398,  399,  401,  417,  419/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    2,    3,    4,    5,    6,    7/

      INTEGER, PARAMETER :: NPHOTAB =  31
      CHARACTER( 16 )    :: PHOTAB( NPHOTAB )

      DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & 
     &   'O3O3P_NASA06    ', 'O3O1D_NASA06    ', 'H2O2_RACM2      ', & 
     &   'NO2_RACM2       ', 'NO3NO_RACM2     ', 'NO3NO2_RACM2    ', & 
     &   'HONO_RACM2      ', 'HNO3_RACM2      ', 'HNO4_RACM2      ', & 
     &   'HCHO_MOL_RACM2  ', 'HCHO_RAD_RACM2  ', 'CH3CHO_RACM2    ', & 
     &   'ALD_RACM2       ', 'CH3COCH3_RACM2A ', 'CH3COCH3_RACM2B ', & 
     &   'UALD_RACM2      ', 'MEK_RACM2       ', 'KET_RACM2       ', & 
     &   'HKET_RACM2      ', 'MACR_RACM2      ', 'MVK_RACM2       ', & 
     &   'GLYH2_RACM2     ', 'GLYF_RACM2      ', 'GLYHX_RACM2     ', & 
     &   'MGLY_RACM2      ', 'BALD_RACM2      ', 'OP1_RACM2       ', & 
     &   'PAA_RACM2       ', 'ONIT_RACM2      ', 'PAN1_RACM2      ', & 
     &   'PAN2_RACM2      '/

      INTEGER, PARAMETER :: NHETERO =   7
      CHARACTER( 16 )    :: HETERO( NHETERO )

      DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & 
     &   'HETERO_GLY      ', 'HETERO_MGLY     ', 'HETERO_N2O5IJ   ', &
     &   'HETERO_NO2      ', 'HETERO_IEPOX    ', 'HETERO_PNCOMLI  ', &
     &   'HETERO_PNCOMLJ  '/

      CHARACTER( 16 )    :: RXLABEL( NRXNS )

      DATA ( RXLABEL( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &    'R001            ', 'R002            ', 'R003            ', & ! 0   
     &    'R004            ', 'R005            ', 'R006            ', & ! 1   
     &    'R007            ', 'R008            ', 'R009            ', & ! 2   
     &    'R010            ', 'R011            ', 'R012            ', & ! 3   
     &    'R013            ', 'R014            ', 'R014a           ', & ! 4   
     &    'R015            ', 'R016            ', 'R017            ', & ! 5   
     &    'R018            ', 'R019            ', 'R020            ', & ! 6   
     &    'R021            ', 'R022            ', 'R023            ', & ! 7   
     &    'R024            ', 'R025            ', 'R026            ', & ! 8   
     &    'R027            ', 'R028            ', 'R029            ', & ! 9   
     &    'R030            ', 'R031            ', 'R032            ', & ! 0   
     &    'R033            ', 'R034            ', 'R035            ', & ! 1   
     &    'R036            ', 'R037            ', 'R038            ', & ! 2   
     &    'R039            ', 'R040            ', 'R041            ', & ! 3   
     &    'R042            ', 'R043            ', 'R044            ', & ! 4   
     &    'R045            ', 'R046            ', 'R047            ', & ! 5   
     &    'R048            ', 'R049            ', 'R050            ', & ! 6   
     &    'R051            ', 'R052            ', 'R053            ', & ! 7   
     &    'R054            ', 'R055            ', 'R056            ', & ! 8   
     &    'R057            ', 'R058            ', 'R059            ', & ! 9   
     &    'R060            ', 'R061            ', 'R062            ', & ! 0   
     &    'R063            ', 'R064            ', 'R065            ', & ! 1   
     &    'R066            ', 'R067            ', 'R068            ', & ! 2   
     &    'R069            ', 'R070            ', 'R071            ', & ! 3   
     &    'R072            ', 'R073            ', 'R074            ', & ! 4   
     &    'R075            ', 'R076            ', 'R077            ', & ! 5   
     &    'R078            ', 'R080            ', 'R081            ', & ! 6   
     &    'R082            ', 'R083            ', 'R084c           ', & ! 7   
     &    'R086            ', 'R087            ', 'R088            ', & ! 8   
     &    'R089            ', 'R090            ', 'R091            ', & ! 9   
     &    'R092            ', 'R093            ', 'R094            ', & ! 0   
     &    'R095            ', 'R096            ', 'R097            ', & ! 1   
     &    'R098            ', 'R099            ', 'R100            ', & ! 2   
     &    'R101            ', 'R102            ', 'R103            ', & ! 3   
     &    'R104            ', 'R105            ', 'R106            ', & ! 4   
     &    'R107            ', 'R108            ', 'R109            ', & ! 5   
     &    'R110            ', 'R111            ', 'R112            ', & ! 6   
     &    'R113            ', 'R114            ', 'R115            ', & ! 7   
     &    'R116            ', 'R117            ', 'R118            ', & ! 8   
     &    'R119            ', 'R120            ', 'R121            ', & ! 9   
     &    'R122            ', 'R123            ', 'R124            ', & ! 0   
     &    'R125            ', 'R126            ', 'R127            ', & ! 1   
     &    'R128            ', 'R130            ', 'R131            ', & ! 2   
     &    'R132            ', 'R133            ', 'R134            ', & ! 3   
     &    'R135            ', 'R136            ', 'R137            ', & ! 4   
     &    'R138            ', 'R139            ', 'R140            ', & ! 5   
     &    'R141            ', 'R142            ', 'R143            ', & ! 6   
     &    'R145            ', 'R146            ', 'R147            ', & ! 7   
     &    'R148            ', 'R149            ', 'R150            ', & ! 8   
     &    'R151            ', 'R152            ', 'R153            ', & ! 9   
     &    'R154            ', 'R155            ', 'R156            ', & ! 0   
     &    'R157            ', 'R158            ', 'R159            ', & ! 1   
     &    'R160            ', 'R161            ', 'R162            ', & ! 2   
     &    'R163            ', 'R164            ', 'R165            ', & ! 3   
     &    'R166            ', 'R167            ', 'R168            ', & ! 4   
     &    'R169            ', 'R170            ', 'R171            ', & ! 5   
     &    'R172            ', 'R173            ', 'R174            ', & ! 6   
     &    'R175            ', 'R176            ', 'R177            ', & ! 7   
     &    'R178            ', 'R179            ', 'R180            ', & ! 8   
     &    'R181            ', 'R182            ', 'R183            ', & ! 9   
     &    'R184            ', 'R185            ', 'R186            ', & ! 0   
     &    'R187            ', 'R188            ', 'R189            ', & ! 1   
     &    'R190            ', 'R191            ', 'R192            ', & ! 2   
     &    'R193            ', 'R194            ', 'R195            ', & ! 3   
     &    'R196            ', 'R197            ', 'R198            ', & ! 4   
     &    'R199            ', 'R200            ', 'R201            ', & ! 5   
     &    'R202            ', 'R203            ', 'R204            ', & ! 6   
     &    'R205            ', 'R206            ', 'R207            ', & ! 7   
     &    'R208            ', 'R209            ', 'R210            ', & ! 8   
     &    'R211            ', 'R212            ', 'R213            ', & ! 9   
     &    'R214            ', 'R215            ', 'R216            ', & ! 0   
     &    'R217            ', 'R218            ', 'R219            ', & ! 1   
     &    'R220            ', 'R221            ', 'R222            ', & ! 2   
     &    'R223            ', 'R224            ', 'R225            ', & ! 3   
     &    'R226            ', 'R227            ', 'R228            ', & ! 4   
     &    'R229            ', 'R230            ', 'R231            ', & ! 5   
     &    'R232            ', 'R233            ', 'R234            ', & ! 6   
     &    'R235            ', 'R236            ', 'R237            ', & ! 7   
     &    'R238            ', 'R239            ', 'R240            ', & ! 8   
     &    'R241            ', 'R242            ', 'R243            ', & ! 9   
     &    'R244            ', 'R245            ', 'R246            ', & ! 0   
     &    'R247            ', 'R248            ', 'R249            ', & ! 1   
     &    'R250            ', 'R251            ', 'R252            ', & ! 2   
     &    'R253            ', 'R254            ', 'R255            ', & ! 3   
     &    'R256            ', 'R257            ', 'R258            ', & ! 4   
     &    'R259            ', 'R260            ', 'R261            ', & ! 5   
     &    'R262            ', 'R263            ', 'R264            ', & ! 6   
     &    'R265            ', 'R266            ', 'R267            ', & ! 7   
     &    'R268            ', 'R269            ', 'R270            ', & ! 8   
     &    'R271            ', 'R272            ', 'R273            ', & ! 9   
     &    'R274            ', 'R275            ', 'R276            ', & ! 0   
     &    'R277            ', 'R278            ', 'R279            ', & ! 1   
     &    'R280            ', 'R281            ', 'R282            ', & ! 2   
     &    'R283            ', 'R284            ', 'R285            ', & ! 3   
     &    'R286            ', 'R287            ', 'R288            ', & ! 4   
     &    'R289            ', 'R290            ', 'R291            ', & ! 5   
     &    'R292            ', 'R293            ', 'R294            ', & ! 6   
     &    'R295            ', 'R296            ', 'R297            ', & ! 7   
     &    'R298            ', 'R299            ', 'R300            ', & ! 8   
     &    'R301            ', 'R302            ', 'R303            ', & ! 9   
     &    'R304            ', 'R305            ', 'R306            ', & ! 0   
     &    'R307            ', 'R308            ', 'R309            ', & ! 1   
     &    'R310            ', 'R311            ', 'R312            ', & ! 2   
     &    'R313            ', 'R314            ', 'R315            ', & ! 3   
     &    'R316            ', 'R317            ', 'R318            ', & ! 4   
     &    'R319            ', 'R320            ', 'R321            ', & ! 5   
     &    'R322            ', 'R323            ', 'R324            ', & ! 6   
     &    'R325            ', 'R326            ', 'R327            ', & ! 7   
     &    'R328            ', 'R329            ', 'R330            ', & ! 8   
     &    'R331            ', 'R332            ', 'R333            ', & ! 9   
     &    'R334            ', 'R335            ', 'R336            ', & ! 0   
     &    'R337            ', 'R338            ', 'R339            ', & ! 1   
     &    'R340            ', 'R341            ', 'R342            ', & ! 2   
     &    'R343            ', 'R344            ', 'R345            ', & ! 3   
     &    'R346            ', 'R347            ', 'R348            ', & ! 4   
     &    'R349            ', 'R350            ', 'R351            ', & ! 5   
     &    'R352            ', 'R353            ', 'R354            ', & ! 6   
     &    'R355            ', 'R356            ', 'R357            ', & ! 7   
     &    'R358            ', 'R359            ', 'R360            ', & ! 8   
     &    'R361            ', 'R362            ', 'R363            ', & ! 9   
     &    'SA01            ', 'SA02            ', 'SA03            ', & ! 0   
     &    'SA04            ', 'SA05            ', 'SA06            ', & ! 1   
     &    'SA13            ', 'SA14            ', 'R001c           ', & ! 2   
     &    'R002c           ', 'SA10            ', 'SA11            ', & ! 3   
     &    'SA12            ', 'T17             ', 'T18             ', & ! 4   
     &    'T19             ', 'T10             ', 'T11             ', & ! 5   
     &    'T12             ', 'R003c           ', 'R004c           ', & ! 6   
     &    'R005c           ', 'R006c           ', 'R007c           ', & ! 7   
     &    'R008c           ', 'R010c           ', 'R011c           ', & ! 8   
     &    'R012c           ', 'R013c           ', 'R014c           ', & ! 9   
     &    'R015c           ', 'R016c           ', 'R017c           ', & ! 0   
     &    'R019c           ', 'R020c           ', 'HET_GLY         ', & ! 1   
     &    'HET_MGLY        ', 'HET_N2O5        ', 'HET_N02         ', & ! 2   
     &    'HAL_Ozone       ', 'HET_IEPOX       ', 'OLIG_XYLENE1    ', & ! 3   
     &    'OLIG_XYLENE2    ', 'OLIG_TOLUENE1   ', 'OLIG_TOLUENE2   ', & ! 4   
     &    'OLIG_BENZENE1   ', 'OLIG_BENZENE2   ', 'OLIG_TERPENE1   ', & ! 5   
     &    'OLIG_TERPENE2   ', 'OLIG_ISOPRENE1  ', 'OLIG_ISOPRENE2  ', & ! 6   
     &    'OLIG_PAH1       ', 'OLIG_PAH2       ', 'OLIG_ALK1       ', & ! 7   
     &    'OLIG_ALK2       ', 'RPOAGEPI        ', 'RPOAGELI        ', & ! 8   
     &    'RPOAGEPJ        ', 'RPOAGELJ        ', 'PCSOA           ', & ! 9   
     &    'POA_AGE1        ', 'POA_AGE2        ', 'POA_AGE3        ', & ! 0   
     &    'POA_AGE4        ', 'POA_AGE5        ', 'POA_AGE6        ', & ! 1   
     &    'POA_AGE7        ', 'POA_AGE8        ', 'POA_AGE9        ', & ! 2   
     &    'POA_AGE10       '/                   !                    

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
       INTEGER, PARAMETER  :: IJ_O3O3P_NASA06     =   1
       INTEGER, PARAMETER  :: IJ_O3O1D_NASA06     =   2
       INTEGER, PARAMETER  :: IJ_H2O2_RACM2       =   3
       INTEGER, PARAMETER  :: IJ_NO2_RACM2        =   4
       INTEGER, PARAMETER  :: IJ_NO3NO_RACM2      =   5
       INTEGER, PARAMETER  :: IJ_NO3NO2_RACM2     =   6
       INTEGER, PARAMETER  :: IJ_HONO_RACM2       =   7
       INTEGER, PARAMETER  :: IJ_HNO3_RACM2       =   8
       INTEGER, PARAMETER  :: IJ_HNO4_RACM2       =   9
       INTEGER, PARAMETER  :: IJ_HCHO_MOL_RACM2   =  10
       INTEGER, PARAMETER  :: IJ_HCHO_RAD_RACM2   =  11
       INTEGER, PARAMETER  :: IJ_CH3CHO_RACM2     =  12
       INTEGER, PARAMETER  :: IJ_ALD_RACM2        =  13
       INTEGER, PARAMETER  :: IJ_CH3COCH3_RACM2A  =  14
       INTEGER, PARAMETER  :: IJ_CH3COCH3_RACM2B  =  15
       INTEGER, PARAMETER  :: IJ_UALD_RACM2       =  16
       INTEGER, PARAMETER  :: IJ_MEK_RACM2        =  17
       INTEGER, PARAMETER  :: IJ_KET_RACM2        =  18
       INTEGER, PARAMETER  :: IJ_HKET_RACM2       =  19
       INTEGER, PARAMETER  :: IJ_MACR_RACM2       =  20
       INTEGER, PARAMETER  :: IJ_MVK_RACM2        =  21
       INTEGER, PARAMETER  :: IJ_GLYH2_RACM2      =  22
       INTEGER, PARAMETER  :: IJ_GLYF_RACM2       =  23
       INTEGER, PARAMETER  :: IJ_GLYHX_RACM2      =  24
       INTEGER, PARAMETER  :: IJ_MGLY_RACM2       =  25
       INTEGER, PARAMETER  :: IJ_BALD_RACM2       =  26
       INTEGER, PARAMETER  :: IJ_OP1_RACM2        =  27
       INTEGER, PARAMETER  :: IJ_PAA_RACM2        =  28
       INTEGER, PARAMETER  :: IJ_ONIT_RACM2       =  29
       INTEGER, PARAMETER  :: IJ_PAN1_RACM2       =  30
       INTEGER, PARAMETER  :: IJ_PAN2_RACM2       =  31
       INTEGER, PARAMETER  :: IK_HETERO_GLY       =   1
       INTEGER, PARAMETER  :: IK_HETERO_MGLY      =   2
       INTEGER, PARAMETER  :: IK_HETERO_N2O5IJ    =   3
       INTEGER, PARAMETER  :: IK_HETERO_NO2       =   4
       INTEGER, PARAMETER  :: IK_HETERO_IEPOX     =   5
       INTEGER, PARAMETER  :: IK_HETERO_PNCOMLI   =   6
       INTEGER, PARAMETER  :: IK_HETERO_PNCOMLJ   =   7
       END MODULE RXNS_DATA
