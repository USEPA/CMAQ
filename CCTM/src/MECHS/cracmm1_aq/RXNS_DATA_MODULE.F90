       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: /home/bplace/CMAQ_ProjectT2/UTIL/chemmech/input/cracmm1_aq/mech_cracmm1_aq.def
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

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC = 169
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 194

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
      DATA GAS_CHEM_SPC(  23 ) / 'PINAL           ' /
      DATA GAS_CHEM_SPC(  24 ) / 'HC10P           ' /
      DATA GAS_CHEM_SPC(  25 ) / 'LIMAL           ' /
      DATA GAS_CHEM_SPC(  26 ) / 'MEK             ' /
      DATA GAS_CHEM_SPC(  27 ) / 'HKET            ' /
      DATA GAS_CHEM_SPC(  28 ) / 'MACR            ' /
      DATA GAS_CHEM_SPC(  29 ) / 'MACP            ' /
      DATA GAS_CHEM_SPC(  30 ) / 'XO2             ' /
      DATA GAS_CHEM_SPC(  31 ) / 'MVK             ' /
      DATA GAS_CHEM_SPC(  32 ) / 'GLY             ' /
      DATA GAS_CHEM_SPC(  33 ) / 'MGLY            ' /
      DATA GAS_CHEM_SPC(  34 ) / 'DCB1            ' /
      DATA GAS_CHEM_SPC(  35 ) / 'DCB2            ' /
      DATA GAS_CHEM_SPC(  36 ) / 'BALD            ' /
      DATA GAS_CHEM_SPC(  37 ) / 'CHO             ' /
      DATA GAS_CHEM_SPC(  38 ) / 'OP1             ' /
      DATA GAS_CHEM_SPC(  39 ) / 'OP2             ' /
      DATA GAS_CHEM_SPC(  40 ) / 'OPB             ' /
      DATA GAS_CHEM_SPC(  41 ) / 'PAA             ' /
      DATA GAS_CHEM_SPC(  42 ) / 'ONIT            ' /
      DATA GAS_CHEM_SPC(  43 ) / 'PAN             ' /
      DATA GAS_CHEM_SPC(  44 ) / 'N2O5            ' /
      DATA GAS_CHEM_SPC(  45 ) / 'SO2             ' /
      DATA GAS_CHEM_SPC(  46 ) / 'SULF            ' /
      DATA GAS_CHEM_SPC(  47 ) / 'SULRXN          ' /
      DATA GAS_CHEM_SPC(  48 ) / 'ETH             ' /
      DATA GAS_CHEM_SPC(  49 ) / 'HC3             ' /
      DATA GAS_CHEM_SPC(  50 ) / 'HC3P            ' /
      DATA GAS_CHEM_SPC(  51 ) / 'HC5             ' /
      DATA GAS_CHEM_SPC(  52 ) / 'HC5P            ' /
      DATA GAS_CHEM_SPC(  53 ) / 'HC8             ' /
      DATA GAS_CHEM_SPC(  54 ) / 'HC8P            ' /
      DATA GAS_CHEM_SPC(  55 ) / 'ETE             ' /
      DATA GAS_CHEM_SPC(  56 ) / 'ETEP            ' /
      DATA GAS_CHEM_SPC(  57 ) / 'OLT             ' /
      DATA GAS_CHEM_SPC(  58 ) / 'OLTP            ' /
      DATA GAS_CHEM_SPC(  59 ) / 'OLI             ' /
      DATA GAS_CHEM_SPC(  60 ) / 'OLIP            ' /
      DATA GAS_CHEM_SPC(  61 ) / 'ACE             ' /
      DATA GAS_CHEM_SPC(  62 ) / 'ORA1            ' /
      DATA GAS_CHEM_SPC(  63 ) / 'BEN             ' /
      DATA GAS_CHEM_SPC(  64 ) / 'BENP            ' /
      DATA GAS_CHEM_SPC(  65 ) / 'EPX             ' /
      DATA GAS_CHEM_SPC(  66 ) / 'PHEN            ' /
      DATA GAS_CHEM_SPC(  67 ) / 'BENZRO2         ' /
      DATA GAS_CHEM_SPC(  68 ) / 'TOL             ' /
      DATA GAS_CHEM_SPC(  69 ) / 'TR2             ' /
      DATA GAS_CHEM_SPC(  70 ) / 'TLP1            ' /
      DATA GAS_CHEM_SPC(  71 ) / 'CSL             ' /
      DATA GAS_CHEM_SPC(  72 ) / 'TOLRO2          ' /
      DATA GAS_CHEM_SPC(  73 ) / 'XYM             ' /
      DATA GAS_CHEM_SPC(  74 ) / 'XY2             ' /
      DATA GAS_CHEM_SPC(  75 ) / 'XYL1            ' /
      DATA GAS_CHEM_SPC(  76 ) / 'XYLRO2          ' /
      DATA GAS_CHEM_SPC(  77 ) / 'XYE             ' /
      DATA GAS_CHEM_SPC(  78 ) / 'ISO             ' /
      DATA GAS_CHEM_SPC(  79 ) / 'ISOP            ' /
      DATA GAS_CHEM_SPC(  80 ) / 'API             ' /
      DATA GAS_CHEM_SPC(  81 ) / 'APIP1           ' /
      DATA GAS_CHEM_SPC(  82 ) / 'APIP2           ' /
      DATA GAS_CHEM_SPC(  83 ) / 'LIM             ' /
      DATA GAS_CHEM_SPC(  84 ) / 'LIMP1           ' /
      DATA GAS_CHEM_SPC(  85 ) / 'LIMP2           ' /
      DATA GAS_CHEM_SPC(  86 ) / 'PINALP          ' /
      DATA GAS_CHEM_SPC(  87 ) / 'LIMALP          ' /
      DATA GAS_CHEM_SPC(  88 ) / 'RCO3            ' /
      DATA GAS_CHEM_SPC(  89 ) / 'ACTP            ' /
      DATA GAS_CHEM_SPC(  90 ) / 'MEKP            ' /
      DATA GAS_CHEM_SPC(  91 ) / 'KETP            ' /
      DATA GAS_CHEM_SPC(  92 ) / 'MCP             ' /
      DATA GAS_CHEM_SPC(  93 ) / 'MVKP            ' /
      DATA GAS_CHEM_SPC(  94 ) / 'UALP            ' /
      DATA GAS_CHEM_SPC(  95 ) / 'DCB3            ' /
      DATA GAS_CHEM_SPC(  96 ) / 'BALP            ' /
      DATA GAS_CHEM_SPC(  97 ) / 'ADDC            ' /
      DATA GAS_CHEM_SPC(  98 ) / 'MCT             ' /
      DATA GAS_CHEM_SPC(  99 ) / 'MCTO            ' /
      DATA GAS_CHEM_SPC( 100 ) / 'MOH             ' /
      DATA GAS_CHEM_SPC( 101 ) / 'EOH             ' /
      DATA GAS_CHEM_SPC( 102 ) / 'ROH             ' /
      DATA GAS_CHEM_SPC( 103 ) / 'ETEG            ' /
      DATA GAS_CHEM_SPC( 104 ) / 'ISHP            ' /
      DATA GAS_CHEM_SPC( 105 ) / 'IEPOX           ' /
      DATA GAS_CHEM_SPC( 106 ) / 'MAHP            ' /
      DATA GAS_CHEM_SPC( 107 ) / 'ORA2            ' /
      DATA GAS_CHEM_SPC( 108 ) / 'ORAP            ' /
      DATA GAS_CHEM_SPC( 109 ) / 'PPN             ' /
      DATA GAS_CHEM_SPC( 110 ) / 'MPAN            ' /
      DATA GAS_CHEM_SPC( 111 ) / 'TRPN            ' /
      DATA GAS_CHEM_SPC( 112 ) / 'HOM             ' /
      DATA GAS_CHEM_SPC( 113 ) / 'NALD            ' /
      DATA GAS_CHEM_SPC( 114 ) / 'ISON            ' /
      DATA GAS_CHEM_SPC( 115 ) / 'MCTP            ' /
      DATA GAS_CHEM_SPC( 116 ) / 'OLNN            ' /
      DATA GAS_CHEM_SPC( 117 ) / 'OLND            ' /
      DATA GAS_CHEM_SPC( 118 ) / 'APINP1          ' /
      DATA GAS_CHEM_SPC( 119 ) / 'APINP2          ' /
      DATA GAS_CHEM_SPC( 120 ) / 'LIMNP1          ' /
      DATA GAS_CHEM_SPC( 121 ) / 'LIMNP2          ' /
      DATA GAS_CHEM_SPC( 122 ) / 'ADCN            ' /
      DATA GAS_CHEM_SPC( 123 ) / 'TOLP            ' /
      DATA GAS_CHEM_SPC( 124 ) / 'PER1            ' /
      DATA GAS_CHEM_SPC( 125 ) / 'XYLP            ' /
      DATA GAS_CHEM_SPC( 126 ) / 'PER2            ' /
      DATA GAS_CHEM_SPC( 127 ) / 'XYO2            ' /
      DATA GAS_CHEM_SPC( 128 ) / 'XYOP            ' /
      DATA GAS_CHEM_SPC( 129 ) / 'BAL1            ' /
      DATA GAS_CHEM_SPC( 130 ) / 'BAL2            ' /
      DATA GAS_CHEM_SPC( 131 ) / 'ELHOM           ' /
      DATA GAS_CHEM_SPC( 132 ) / 'TOLNRXN         ' /
      DATA GAS_CHEM_SPC( 133 ) / 'TOLHRXN         ' /
      DATA GAS_CHEM_SPC( 134 ) / 'XYLNRXN         ' /
      DATA GAS_CHEM_SPC( 135 ) / 'XYLHRXN         ' /
      DATA GAS_CHEM_SPC( 136 ) / 'BNZNRXN         ' /
      DATA GAS_CHEM_SPC( 137 ) / 'BNZHRXN         ' /
      DATA GAS_CHEM_SPC( 138 ) / 'SOAALK          ' /
      DATA GAS_CHEM_SPC( 139 ) / 'ALKRXN          ' /
      DATA GAS_CHEM_SPC( 140 ) / 'ROCIOXY         ' /
      DATA GAS_CHEM_SPC( 141 ) / 'SLOWROC         ' /
      DATA GAS_CHEM_SPC( 142 ) / 'NAPH            ' /
      DATA GAS_CHEM_SPC( 143 ) / 'PAHRO2          ' /
      DATA GAS_CHEM_SPC( 144 ) / 'PAHNRXN         ' /
      DATA GAS_CHEM_SPC( 145 ) / 'PAHHRXN         ' /
      DATA GAS_CHEM_SPC( 146 ) / 'ACRO            ' /
      DATA GAS_CHEM_SPC( 147 ) / 'BDE13           ' /
      DATA GAS_CHEM_SPC( 148 ) / 'FURAN           ' /
      DATA GAS_CHEM_SPC( 149 ) / 'FURANO2         ' /
      DATA GAS_CHEM_SPC( 150 ) / 'FURANONE        ' /
      DATA GAS_CHEM_SPC( 151 ) / 'PROG            ' /
      DATA GAS_CHEM_SPC( 152 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC( 153 ) / 'SESQNRO2        ' /
      DATA GAS_CHEM_SPC( 154 ) / 'VROCP0OXY       ' /
      DATA GAS_CHEM_SPC( 155 ) / 'VROCP3OXY       ' /
      DATA GAS_CHEM_SPC( 156 ) / 'VROCN2OXY       ' /
      DATA GAS_CHEM_SPC( 157 ) / 'SESQRO2         ' /
      DATA GAS_CHEM_SPC( 158 ) / 'PCVOC           ' /
      DATA GAS_CHEM_SPC( 159 ) / 'PCSOARXN        ' /
      DATA GAS_CHEM_SPC( 160 ) / 'VLVPO1          ' /
      DATA GAS_CHEM_SPC( 161 ) / 'VSVPO1          ' /
      DATA GAS_CHEM_SPC( 162 ) / 'VSVPO2          ' /
      DATA GAS_CHEM_SPC( 163 ) / 'VSVPO3          ' /
      DATA GAS_CHEM_SPC( 164 ) / 'VIVPO1          ' /
      DATA GAS_CHEM_SPC( 165 ) / 'VLVOO1          ' /
      DATA GAS_CHEM_SPC( 166 ) / 'VLVOO2          ' /
      DATA GAS_CHEM_SPC( 167 ) / 'VSVOO2          ' /
      DATA GAS_CHEM_SPC( 168 ) / 'VSVOO3          ' /
      DATA GAS_CHEM_SPC( 169 ) / 'VSVOO1          ' /




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
      & MEMBER("PINAL           ",   86, "GC",  168.00D0, F), &
      & MEMBER("HC10P           ",   52, "GC",  173.27D0, F), &
      & MEMBER("LIMAL           ",   93, "GC",  168.00D0, F), &
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
      & MEMBER("OPB             ",   37, "GC",  186.00D0, F), &
      & MEMBER("PAA             ",   38, "GC",   76.00D0, F), &
      & MEMBER("ONIT            ",   39, "GC",  119.00D0, F), &
      & MEMBER("PAN             ",   40, "GC",  121.00D0, F), &
      & MEMBER("N2O5            ",   41, "GC",  108.00D0, F), &
      & MEMBER("SO2             ",   42, "GC",   64.00D0, F), &
      & MEMBER("SULF            ",   43, "GC",   98.00D0, F), &
      & MEMBER("SULRXN          ",   44, "GC",   98.00D0, F), &
      & MEMBER("ETH             ",   45, "GC",   30.10D0, F), &
      & MEMBER("HC3             ",   46, "GC",   51.90D0, F), &
      & MEMBER("HC3P            ",   47, "GC",   75.00D0, F), &
      & MEMBER("ASOATJ          ",  184, "AE",  200.00D0, T), &
      & MEMBER("HC5             ",   48, "GC",   84.30D0, F), &
      & MEMBER("HC5P            ",   49, "GC",  103.00D0, F), &
      & MEMBER("HC8             ",   50, "GC",  136.20D0, F), &
      & MEMBER("HC8P            ",   51, "GC",  145.00D0, F), &
      & MEMBER("ETE             ",   53, "GC",   28.10D0, F), &
      & MEMBER("ETEP            ",   54, "GC",   77.00D0, F), &
      & MEMBER("OLT             ",   55, "GC",   42.00D0, F), &
      & MEMBER("OLTP            ",   56, "GC",   91.00D0, F), &
      & MEMBER("OLI             ",   57, "GC",   68.00D0, F), &
      & MEMBER("OLIP            ",   58, "GC",  117.00D0, F), &
      & MEMBER("ACE             ",   59, "GC",   26.00D0, F), &
      & MEMBER("ORA1            ",   60, "GC",   46.00D0, F), &
      & MEMBER("BEN             ",   61, "GC",   78.10D0, F), &
      & MEMBER("BENP            ",   62, "GC",  127.00D0, F), &
      & MEMBER("EPX             ",   63, "GC",  122.50D0, F), &
      & MEMBER("PHEN            ",   64, "GC",  105.50D0, F), &
      & MEMBER("BENZRO2         ",   65, "GC",  127.00D0, F), &
      & MEMBER("TOL             ",   66, "GC",   92.10D0, F), &
      & MEMBER("TR2             ",   67, "GC",  109.00D0, F), &
      & MEMBER("TLP1            ",   68, "GC",   91.00D0, F), &
      & MEMBER("CSL             ",   69, "GC",  129.40D0, F), &
      & MEMBER("TOLRO2          ",   70, "GC",  141.00D0, F), &
      & MEMBER("XYM             ",   71, "GC",  112.00D0, F), &
      & MEMBER("XY2             ",   72, "GC",  124.00D0, F), &
      & MEMBER("XYL1            ",   73, "GC",  156.00D0, F), &
      & MEMBER("XYLRO2          ",   74, "GC",  155.00D0, F), &
      & MEMBER("XYE             ",   75, "GC",  115.00D0, F), &
      & MEMBER("ISO             ",   77, "GC",   68.10D0, F), &
      & MEMBER("ISOP            ",   78, "GC",  117.00D0, F), &
      & MEMBER("API             ",   80, "GC",  136.40D0, F), &
      & MEMBER("APIP1           ",   82, "GC",  185.00D0, F), &
      & MEMBER("APIP2           ",   83, "GC",  185.00D0, F), &
      & MEMBER("LIM             ",   88, "GC",  136.30D0, F), &
      & MEMBER("LIMP1           ",   89, "GC",  185.00D0, F), &
      & MEMBER("LIMP2           ",   90, "GC",  185.00D0, F), &
      & MEMBER("PINALP          ",   87, "GC",  199.00D0, F), &
      & MEMBER("LIMALP          ",   94, "GC",  217.00D0, F), &
      & MEMBER("RCO3            ",   97, "GC",   90.00D0, F), &
      & MEMBER("ACTP            ",   98, "GC",   89.00D0, F), &
      & MEMBER("MEKP            ",   99, "GC",  103.00D0, F), &
      & MEMBER("KETP            ",  100, "GC",  117.00D0, F), &
      & MEMBER("MCP             ",  101, "GC",  119.00D0, F), &
      & MEMBER("MVKP            ",  102, "GC",  119.00D0, F), &
      & MEMBER("UALP            ",  103, "GC",  133.00D0, F), &
      & MEMBER("DCB3            ",  104, "GC",   84.00D0, F), &
      & MEMBER("BALP            ",  105, "GC",  137.00D0, F), &
      & MEMBER("ADDC            ",  106, "GC",  125.00D0, F), &
      & MEMBER("MCT             ",  107, "GC",  124.10D0, F), &
      & MEMBER("MCTO            ",  108, "GC",  123.00D0, F), &
      & MEMBER("MOH             ",  109, "GC",   32.00D0, F), &
      & MEMBER("EOH             ",  110, "GC",   46.10D0, F), &
      & MEMBER("ROH             ",  111, "GC",   60.00D0, F), &
      & MEMBER("ETEG            ",  112, "GC",   62.10D0, F), &
      & MEMBER("ISHP            ",  113, "GC",  118.00D0, F), &
      & MEMBER("IEPOX           ",  114, "GC",  118.10D0, F), &
      & MEMBER("MAHP            ",  115, "GC",  102.00D0, F), &
      & MEMBER("ORA2            ",  116, "GC",   60.20D0, F), &
      & MEMBER("ORAP            ",  117, "GC",  109.00D0, F), &
      & MEMBER("PPN             ",  118, "GC",  135.00D0, F), &
      & MEMBER("MPAN            ",  119, "GC",  148.00D0, F), &
      & MEMBER("TRPN            ",  122, "GC",  215.00D0, F), &
      & MEMBER("HOM             ",   95, "GC",  250.00D0, F), &
      & MEMBER("NALD            ",  120, "GC",  105.00D0, F), &
      & MEMBER("ISON            ",  121, "GC",  147.00D0, F), &
      & MEMBER("MCTP            ",  123, "GC",  172.00D0, F), &
      & MEMBER("OLNN            ",  124, "GC",  136.00D0, F), &
      & MEMBER("OLND            ",  125, "GC",  136.00D0, F), &
      & MEMBER("APINP1          ",   84, "GC",  230.00D0, F), &
      & MEMBER("APINP2          ",   85, "GC",  230.00D0, F), &
      & MEMBER("LIMNP1          ",   91, "GC",  230.00D0, F), &
      & MEMBER("LIMNP2          ",   92, "GC",  230.00D0, F), &
      & MEMBER("ADCN            ",  126, "GC",  156.00D0, F), &
      & MEMBER("TOLP            ",  127, "GC",  141.00D0, F), &
      & MEMBER("PER1            ",  128, "GC",  141.00D0, F), &
      & MEMBER("XYLP            ",  129, "GC",  155.00D0, F), &
      & MEMBER("PER2            ",  130, "GC",  157.00D0, F), &
      & MEMBER("XYO2            ",   76, "GC",  155.00D0, F), &
      & MEMBER("XYOP            ",  131, "GC",  155.00D0, F), &
      & MEMBER("BAL1            ",  132, "GC",  121.00D0, F), &
      & MEMBER("BAL2            ",  133, "GC",  105.00D0, F), &
      & MEMBER("ELHOM           ",   96, "GC",  402.00D0, F), &
      & MEMBER("TOLNRXN         ",  142, "GC",  141.00D0, F), &
      & MEMBER("TOLHRXN         ",  143, "GC",  141.00D0, F), &
      & MEMBER("XYLNRXN         ",  144, "GC",  155.00D0, F), &
      & MEMBER("XYLHRXN         ",  145, "GC",  155.00D0, F), &
      & MEMBER("BNZNRXN         ",  146, "GC",  127.00D0, F), &
      & MEMBER("BNZHRXN         ",  147, "GC",  127.00D0, F), &
      & MEMBER("SOAALK          ",  155, "GC",  112.00D0, F), &
      & MEMBER("ALKRXN          ",  156, "GC",  112.00D0, F), &
      & MEMBER("ROCIOXY         ",  140, "GC",  247.00D0, F), &
      & MEMBER("SLOWROC         ",  141, "GC",   75.40D0, F), &
      & MEMBER("NAPH            ",  151, "GC",  133.10D0, F), &
      & MEMBER("PAHRO2          ",  152, "GC",  187.20D0, F), &
      & MEMBER("PAHNRXN         ",  153, "GC",  187.20D0, F), &
      & MEMBER("PAHHRXN         ",  154, "GC",  187.20D0, F), &
      & MEMBER("ACRO            ",  134, "GC",   56.10D0, F), &
      & MEMBER("BDE13           ",  135, "GC",   54.10D0, F), &
      & MEMBER("FURAN           ",  137, "GC",   96.10D0, F), &
      & MEMBER("FURANO2         ",  138, "GC",  145.10D0, F), &
      & MEMBER("FURANONE        ",  139, "GC",  100.10D0, F), &
      & MEMBER("PROG            ",  136, "GC",   76.10D0, F), &
      & MEMBER("SESQ            ",  148, "GC",  204.40D0, F), &
      & MEMBER("SESQNRO2        ",  150, "GC",  298.40D0, F), &
      & MEMBER("VROCP0OXY       ",  173, "GC",  135.00D0, F), &
      & MEMBER("VROCP3OXY       ",  176, "GC",  134.00D0, F), &
      & MEMBER("VROCN2OXY       ",  171, "GC",  136.00D0, F), &
      & MEMBER("SESQRO2         ",  149, "GC",  253.40D0, F), &
      & MEMBER("AGLYJ           ",  185, "AE",   66.40D0, T), &
      & MEMBER("AISONJ          ",  207, "AE",  147.00D0, T), &
      & MEMBER("ATRPNJ          ",  202, "AE",  215.00D0, T), &
      & MEMBER("AISO3J          ",  245, "AE",  168.20D0, T), &
      & MEMBER("AXYL1J          ",  188, "AE",  174.00D0, T), &
      & MEMBER("AOLGAJ          ",  246, "AE",  206.00D0, T), &
      & MEMBER("AXYL2J          ",  189, "AE",  185.00D0, T), &
      & MEMBER("ATOL1J          ",  191, "AE",  163.00D0, T), &
      & MEMBER("ATOL2J          ",  192, "AE",  175.00D0, T), &
      & MEMBER("ABNZ1J          ",  194, "AE",  161.00D0, T), &
      & MEMBER("ABNZ2J          ",  195, "AE",  134.00D0, T), &
      & MEMBER("ATRP1J          ",  200, "AE",  177.00D0, T), &
      & MEMBER("AOLGBJ          ",  247, "AE",  248.00D0, T), &
      & MEMBER("ATRP2J          ",  201, "AE",  198.00D0, T), &
      & MEMBER("AISO1J          ",  203, "AE",  132.00D0, T), &
      & MEMBER("AISO2J          ",  204, "AE",  133.00D0, T), &
      & MEMBER("APAH1J          ",  197, "AE",  195.60D0, T), &
      & MEMBER("APAH2J          ",  198, "AE",  178.70D0, T), &
      & MEMBER("AALK1J          ",  186, "AE",  225.00D0, T), &
      & MEMBER("AALK2J          ",  187, "AE",  205.10D0, T), &
      & MEMBER("APOCI           ",  248, "AE",  220.00D0, T), &
      & MEMBER("APNCOMI         ",  250, "AE",  220.00D0, T), &
      & MEMBER("APOCJ           ",  249, "AE",  220.00D0, T), &
      & MEMBER("APNCOMJ         ",  251, "AE",  220.00D0, T), &
      & MEMBER("PCVOC           ",  167, "GC",  170.00D0, F), &
      & MEMBER("PCSOARXN        ",  168, "GC",  170.00D0, F), &
      & MEMBER("VLVPO1          ",  157, "GC",  218.00D0, F), &
      & MEMBER("VSVPO1          ",  158, "GC",  230.00D0, F), &
      & MEMBER("VSVPO2          ",  159, "GC",  241.00D0, F), &
      & MEMBER("VSVPO3          ",  160, "GC",  253.00D0, F), &
      & MEMBER("VIVPO1          ",  161, "GC",  266.00D0, F), &
      & MEMBER("VLVOO1          ",  162, "GC",  136.00D0, F), &
      & MEMBER("VLVOO2          ",  163, "GC",  136.00D0, F), &
      & MEMBER("VSVOO2          ",  165, "GC",  135.00D0, F), &
      & MEMBER("VSVOO3          ",  166, "GC",  134.00D0, F), &
      & MEMBER("VSVOO1          ",  164, "GC",  135.00D0, F) /)

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
      DATA CHEMISTRY_SPC(  23 ), SPECIES_MOLWT(  23 ) / 'PINAL           ',  168.00D0 /
      DATA CHEMISTRY_SPC(  24 ), SPECIES_MOLWT(  24 ) / 'HC10P           ',  173.27D0 /
      DATA CHEMISTRY_SPC(  25 ), SPECIES_MOLWT(  25 ) / 'LIMAL           ',  168.00D0 /
      DATA CHEMISTRY_SPC(  26 ), SPECIES_MOLWT(  26 ) / 'MEK             ',   72.10D0 /
      DATA CHEMISTRY_SPC(  27 ), SPECIES_MOLWT(  27 ) / 'HKET            ',   74.00D0 /
      DATA CHEMISTRY_SPC(  28 ), SPECIES_MOLWT(  28 ) / 'MACR            ',   70.00D0 /
      DATA CHEMISTRY_SPC(  29 ), SPECIES_MOLWT(  29 ) / 'MACP            ',  101.00D0 /
      DATA CHEMISTRY_SPC(  30 ), SPECIES_MOLWT(  30 ) / 'XO2             ',    1.00D0 /
      DATA CHEMISTRY_SPC(  31 ), SPECIES_MOLWT(  31 ) / 'MVK             ',   70.10D0 /
      DATA CHEMISTRY_SPC(  32 ), SPECIES_MOLWT(  32 ) / 'GLY             ',   58.00D0 /
      DATA CHEMISTRY_SPC(  33 ), SPECIES_MOLWT(  33 ) / 'MGLY            ',   72.00D0 /
      DATA CHEMISTRY_SPC(  34 ), SPECIES_MOLWT(  34 ) / 'DCB1            ',   91.00D0 /
      DATA CHEMISTRY_SPC(  35 ), SPECIES_MOLWT(  35 ) / 'DCB2            ',  110.00D0 /
      DATA CHEMISTRY_SPC(  36 ), SPECIES_MOLWT(  36 ) / 'BALD            ',  106.00D0 /
      DATA CHEMISTRY_SPC(  37 ), SPECIES_MOLWT(  37 ) / 'CHO             ',  139.00D0 /
      DATA CHEMISTRY_SPC(  38 ), SPECIES_MOLWT(  38 ) / 'OP1             ',   48.00D0 /
      DATA CHEMISTRY_SPC(  39 ), SPECIES_MOLWT(  39 ) / 'OP2             ',   62.00D0 /
      DATA CHEMISTRY_SPC(  40 ), SPECIES_MOLWT(  40 ) / 'OPB             ',  186.00D0 /
      DATA CHEMISTRY_SPC(  41 ), SPECIES_MOLWT(  41 ) / 'PAA             ',   76.00D0 /
      DATA CHEMISTRY_SPC(  42 ), SPECIES_MOLWT(  42 ) / 'ONIT            ',  119.00D0 /
      DATA CHEMISTRY_SPC(  43 ), SPECIES_MOLWT(  43 ) / 'PAN             ',  121.00D0 /
      DATA CHEMISTRY_SPC(  44 ), SPECIES_MOLWT(  44 ) / 'N2O5            ',  108.00D0 /
      DATA CHEMISTRY_SPC(  45 ), SPECIES_MOLWT(  45 ) / 'SO2             ',   64.00D0 /
      DATA CHEMISTRY_SPC(  46 ), SPECIES_MOLWT(  46 ) / 'SULF            ',   98.00D0 /
      DATA CHEMISTRY_SPC(  47 ), SPECIES_MOLWT(  47 ) / 'SULRXN          ',   98.00D0 /
      DATA CHEMISTRY_SPC(  48 ), SPECIES_MOLWT(  48 ) / 'ETH             ',   30.10D0 /
      DATA CHEMISTRY_SPC(  49 ), SPECIES_MOLWT(  49 ) / 'HC3             ',   51.90D0 /
      DATA CHEMISTRY_SPC(  50 ), SPECIES_MOLWT(  50 ) / 'HC3P            ',   75.00D0 /
      DATA CHEMISTRY_SPC(  51 ), SPECIES_MOLWT(  51 ) / 'ASOATJ          ',  200.00D0 /
      DATA CHEMISTRY_SPC(  52 ), SPECIES_MOLWT(  52 ) / 'HC5             ',   84.30D0 /
      DATA CHEMISTRY_SPC(  53 ), SPECIES_MOLWT(  53 ) / 'HC5P            ',  103.00D0 /
      DATA CHEMISTRY_SPC(  54 ), SPECIES_MOLWT(  54 ) / 'HC8             ',  136.20D0 /
      DATA CHEMISTRY_SPC(  55 ), SPECIES_MOLWT(  55 ) / 'HC8P            ',  145.00D0 /
      DATA CHEMISTRY_SPC(  56 ), SPECIES_MOLWT(  56 ) / 'ETE             ',   28.10D0 /
      DATA CHEMISTRY_SPC(  57 ), SPECIES_MOLWT(  57 ) / 'ETEP            ',   77.00D0 /
      DATA CHEMISTRY_SPC(  58 ), SPECIES_MOLWT(  58 ) / 'OLT             ',   42.00D0 /
      DATA CHEMISTRY_SPC(  59 ), SPECIES_MOLWT(  59 ) / 'OLTP            ',   91.00D0 /
      DATA CHEMISTRY_SPC(  60 ), SPECIES_MOLWT(  60 ) / 'OLI             ',   68.00D0 /
      DATA CHEMISTRY_SPC(  61 ), SPECIES_MOLWT(  61 ) / 'OLIP            ',  117.00D0 /
      DATA CHEMISTRY_SPC(  62 ), SPECIES_MOLWT(  62 ) / 'ACE             ',   26.00D0 /
      DATA CHEMISTRY_SPC(  63 ), SPECIES_MOLWT(  63 ) / 'ORA1            ',   46.00D0 /
      DATA CHEMISTRY_SPC(  64 ), SPECIES_MOLWT(  64 ) / 'BEN             ',   78.10D0 /
      DATA CHEMISTRY_SPC(  65 ), SPECIES_MOLWT(  65 ) / 'BENP            ',  127.00D0 /
      DATA CHEMISTRY_SPC(  66 ), SPECIES_MOLWT(  66 ) / 'EPX             ',  122.50D0 /
      DATA CHEMISTRY_SPC(  67 ), SPECIES_MOLWT(  67 ) / 'PHEN            ',  105.50D0 /
      DATA CHEMISTRY_SPC(  68 ), SPECIES_MOLWT(  68 ) / 'BENZRO2         ',  127.00D0 /
      DATA CHEMISTRY_SPC(  69 ), SPECIES_MOLWT(  69 ) / 'TOL             ',   92.10D0 /
      DATA CHEMISTRY_SPC(  70 ), SPECIES_MOLWT(  70 ) / 'TR2             ',  109.00D0 /
      DATA CHEMISTRY_SPC(  71 ), SPECIES_MOLWT(  71 ) / 'TLP1            ',   91.00D0 /
      DATA CHEMISTRY_SPC(  72 ), SPECIES_MOLWT(  72 ) / 'CSL             ',  129.40D0 /
      DATA CHEMISTRY_SPC(  73 ), SPECIES_MOLWT(  73 ) / 'TOLRO2          ',  141.00D0 /
      DATA CHEMISTRY_SPC(  74 ), SPECIES_MOLWT(  74 ) / 'XYM             ',  112.00D0 /
      DATA CHEMISTRY_SPC(  75 ), SPECIES_MOLWT(  75 ) / 'XY2             ',  124.00D0 /
      DATA CHEMISTRY_SPC(  76 ), SPECIES_MOLWT(  76 ) / 'XYL1            ',  156.00D0 /
      DATA CHEMISTRY_SPC(  77 ), SPECIES_MOLWT(  77 ) / 'XYLRO2          ',  155.00D0 /
      DATA CHEMISTRY_SPC(  78 ), SPECIES_MOLWT(  78 ) / 'XYE             ',  115.00D0 /
      DATA CHEMISTRY_SPC(  79 ), SPECIES_MOLWT(  79 ) / 'ISO             ',   68.10D0 /
      DATA CHEMISTRY_SPC(  80 ), SPECIES_MOLWT(  80 ) / 'ISOP            ',  117.00D0 /
      DATA CHEMISTRY_SPC(  81 ), SPECIES_MOLWT(  81 ) / 'API             ',  136.40D0 /
      DATA CHEMISTRY_SPC(  82 ), SPECIES_MOLWT(  82 ) / 'APIP1           ',  185.00D0 /
      DATA CHEMISTRY_SPC(  83 ), SPECIES_MOLWT(  83 ) / 'APIP2           ',  185.00D0 /
      DATA CHEMISTRY_SPC(  84 ), SPECIES_MOLWT(  84 ) / 'LIM             ',  136.30D0 /
      DATA CHEMISTRY_SPC(  85 ), SPECIES_MOLWT(  85 ) / 'LIMP1           ',  185.00D0 /
      DATA CHEMISTRY_SPC(  86 ), SPECIES_MOLWT(  86 ) / 'LIMP2           ',  185.00D0 /
      DATA CHEMISTRY_SPC(  87 ), SPECIES_MOLWT(  87 ) / 'PINALP          ',  199.00D0 /
      DATA CHEMISTRY_SPC(  88 ), SPECIES_MOLWT(  88 ) / 'LIMALP          ',  217.00D0 /
      DATA CHEMISTRY_SPC(  89 ), SPECIES_MOLWT(  89 ) / 'RCO3            ',   90.00D0 /
      DATA CHEMISTRY_SPC(  90 ), SPECIES_MOLWT(  90 ) / 'ACTP            ',   89.00D0 /
      DATA CHEMISTRY_SPC(  91 ), SPECIES_MOLWT(  91 ) / 'MEKP            ',  103.00D0 /
      DATA CHEMISTRY_SPC(  92 ), SPECIES_MOLWT(  92 ) / 'KETP            ',  117.00D0 /
      DATA CHEMISTRY_SPC(  93 ), SPECIES_MOLWT(  93 ) / 'MCP             ',  119.00D0 /
      DATA CHEMISTRY_SPC(  94 ), SPECIES_MOLWT(  94 ) / 'MVKP            ',  119.00D0 /
      DATA CHEMISTRY_SPC(  95 ), SPECIES_MOLWT(  95 ) / 'UALP            ',  133.00D0 /
      DATA CHEMISTRY_SPC(  96 ), SPECIES_MOLWT(  96 ) / 'DCB3            ',   84.00D0 /
      DATA CHEMISTRY_SPC(  97 ), SPECIES_MOLWT(  97 ) / 'BALP            ',  137.00D0 /
      DATA CHEMISTRY_SPC(  98 ), SPECIES_MOLWT(  98 ) / 'ADDC            ',  125.00D0 /
      DATA CHEMISTRY_SPC(  99 ), SPECIES_MOLWT(  99 ) / 'MCT             ',  124.10D0 /
      DATA CHEMISTRY_SPC( 100 ), SPECIES_MOLWT( 100 ) / 'MCTO            ',  123.00D0 /
      DATA CHEMISTRY_SPC( 101 ), SPECIES_MOLWT( 101 ) / 'MOH             ',   32.00D0 /
      DATA CHEMISTRY_SPC( 102 ), SPECIES_MOLWT( 102 ) / 'EOH             ',   46.10D0 /
      DATA CHEMISTRY_SPC( 103 ), SPECIES_MOLWT( 103 ) / 'ROH             ',   60.00D0 /
      DATA CHEMISTRY_SPC( 104 ), SPECIES_MOLWT( 104 ) / 'ETEG            ',   62.10D0 /
      DATA CHEMISTRY_SPC( 105 ), SPECIES_MOLWT( 105 ) / 'ISHP            ',  118.00D0 /
      DATA CHEMISTRY_SPC( 106 ), SPECIES_MOLWT( 106 ) / 'IEPOX           ',  118.10D0 /
      DATA CHEMISTRY_SPC( 107 ), SPECIES_MOLWT( 107 ) / 'MAHP            ',  102.00D0 /
      DATA CHEMISTRY_SPC( 108 ), SPECIES_MOLWT( 108 ) / 'ORA2            ',   60.20D0 /
      DATA CHEMISTRY_SPC( 109 ), SPECIES_MOLWT( 109 ) / 'ORAP            ',  109.00D0 /
      DATA CHEMISTRY_SPC( 110 ), SPECIES_MOLWT( 110 ) / 'PPN             ',  135.00D0 /
      DATA CHEMISTRY_SPC( 111 ), SPECIES_MOLWT( 111 ) / 'MPAN            ',  148.00D0 /
      DATA CHEMISTRY_SPC( 112 ), SPECIES_MOLWT( 112 ) / 'TRPN            ',  215.00D0 /
      DATA CHEMISTRY_SPC( 113 ), SPECIES_MOLWT( 113 ) / 'HOM             ',  250.00D0 /
      DATA CHEMISTRY_SPC( 114 ), SPECIES_MOLWT( 114 ) / 'NALD            ',  105.00D0 /
      DATA CHEMISTRY_SPC( 115 ), SPECIES_MOLWT( 115 ) / 'ISON            ',  147.00D0 /
      DATA CHEMISTRY_SPC( 116 ), SPECIES_MOLWT( 116 ) / 'MCTP            ',  172.00D0 /
      DATA CHEMISTRY_SPC( 117 ), SPECIES_MOLWT( 117 ) / 'OLNN            ',  136.00D0 /
      DATA CHEMISTRY_SPC( 118 ), SPECIES_MOLWT( 118 ) / 'OLND            ',  136.00D0 /
      DATA CHEMISTRY_SPC( 119 ), SPECIES_MOLWT( 119 ) / 'APINP1          ',  230.00D0 /
      DATA CHEMISTRY_SPC( 120 ), SPECIES_MOLWT( 120 ) / 'APINP2          ',  230.00D0 /
      DATA CHEMISTRY_SPC( 121 ), SPECIES_MOLWT( 121 ) / 'LIMNP1          ',  230.00D0 /
      DATA CHEMISTRY_SPC( 122 ), SPECIES_MOLWT( 122 ) / 'LIMNP2          ',  230.00D0 /
      DATA CHEMISTRY_SPC( 123 ), SPECIES_MOLWT( 123 ) / 'ADCN            ',  156.00D0 /
      DATA CHEMISTRY_SPC( 124 ), SPECIES_MOLWT( 124 ) / 'TOLP            ',  141.00D0 /
      DATA CHEMISTRY_SPC( 125 ), SPECIES_MOLWT( 125 ) / 'PER1            ',  141.00D0 /
      DATA CHEMISTRY_SPC( 126 ), SPECIES_MOLWT( 126 ) / 'XYLP            ',  155.00D0 /
      DATA CHEMISTRY_SPC( 127 ), SPECIES_MOLWT( 127 ) / 'PER2            ',  157.00D0 /
      DATA CHEMISTRY_SPC( 128 ), SPECIES_MOLWT( 128 ) / 'XYO2            ',  155.00D0 /
      DATA CHEMISTRY_SPC( 129 ), SPECIES_MOLWT( 129 ) / 'XYOP            ',  155.00D0 /
      DATA CHEMISTRY_SPC( 130 ), SPECIES_MOLWT( 130 ) / 'BAL1            ',  121.00D0 /
      DATA CHEMISTRY_SPC( 131 ), SPECIES_MOLWT( 131 ) / 'BAL2            ',  105.00D0 /
      DATA CHEMISTRY_SPC( 132 ), SPECIES_MOLWT( 132 ) / 'ELHOM           ',  402.00D0 /
      DATA CHEMISTRY_SPC( 133 ), SPECIES_MOLWT( 133 ) / 'TOLNRXN         ',  141.00D0 /
      DATA CHEMISTRY_SPC( 134 ), SPECIES_MOLWT( 134 ) / 'TOLHRXN         ',  141.00D0 /
      DATA CHEMISTRY_SPC( 135 ), SPECIES_MOLWT( 135 ) / 'XYLNRXN         ',  155.00D0 /
      DATA CHEMISTRY_SPC( 136 ), SPECIES_MOLWT( 136 ) / 'XYLHRXN         ',  155.00D0 /
      DATA CHEMISTRY_SPC( 137 ), SPECIES_MOLWT( 137 ) / 'BNZNRXN         ',  127.00D0 /
      DATA CHEMISTRY_SPC( 138 ), SPECIES_MOLWT( 138 ) / 'BNZHRXN         ',  127.00D0 /
      DATA CHEMISTRY_SPC( 139 ), SPECIES_MOLWT( 139 ) / 'SOAALK          ',  112.00D0 /
      DATA CHEMISTRY_SPC( 140 ), SPECIES_MOLWT( 140 ) / 'ALKRXN          ',  112.00D0 /
      DATA CHEMISTRY_SPC( 141 ), SPECIES_MOLWT( 141 ) / 'ROCIOXY         ',  247.00D0 /
      DATA CHEMISTRY_SPC( 142 ), SPECIES_MOLWT( 142 ) / 'SLOWROC         ',   75.40D0 /
      DATA CHEMISTRY_SPC( 143 ), SPECIES_MOLWT( 143 ) / 'NAPH            ',  133.10D0 /
      DATA CHEMISTRY_SPC( 144 ), SPECIES_MOLWT( 144 ) / 'PAHRO2          ',  187.20D0 /
      DATA CHEMISTRY_SPC( 145 ), SPECIES_MOLWT( 145 ) / 'PAHNRXN         ',  187.20D0 /
      DATA CHEMISTRY_SPC( 146 ), SPECIES_MOLWT( 146 ) / 'PAHHRXN         ',  187.20D0 /
      DATA CHEMISTRY_SPC( 147 ), SPECIES_MOLWT( 147 ) / 'ACRO            ',   56.10D0 /
      DATA CHEMISTRY_SPC( 148 ), SPECIES_MOLWT( 148 ) / 'BDE13           ',   54.10D0 /
      DATA CHEMISTRY_SPC( 149 ), SPECIES_MOLWT( 149 ) / 'FURAN           ',   96.10D0 /
      DATA CHEMISTRY_SPC( 150 ), SPECIES_MOLWT( 150 ) / 'FURANO2         ',  145.10D0 /
      DATA CHEMISTRY_SPC( 151 ), SPECIES_MOLWT( 151 ) / 'FURANONE        ',  100.10D0 /
      DATA CHEMISTRY_SPC( 152 ), SPECIES_MOLWT( 152 ) / 'PROG            ',   76.10D0 /
      DATA CHEMISTRY_SPC( 153 ), SPECIES_MOLWT( 153 ) / 'SESQ            ',  204.40D0 /
      DATA CHEMISTRY_SPC( 154 ), SPECIES_MOLWT( 154 ) / 'SESQNRO2        ',  298.40D0 /
      DATA CHEMISTRY_SPC( 155 ), SPECIES_MOLWT( 155 ) / 'VROCP0OXY       ',  135.00D0 /
      DATA CHEMISTRY_SPC( 156 ), SPECIES_MOLWT( 156 ) / 'VROCP3OXY       ',  134.00D0 /
      DATA CHEMISTRY_SPC( 157 ), SPECIES_MOLWT( 157 ) / 'VROCN2OXY       ',  136.00D0 /
      DATA CHEMISTRY_SPC( 158 ), SPECIES_MOLWT( 158 ) / 'SESQRO2         ',  253.40D0 /
      DATA CHEMISTRY_SPC( 159 ), SPECIES_MOLWT( 159 ) / 'AGLYJ           ',   66.40D0 /
      DATA CHEMISTRY_SPC( 160 ), SPECIES_MOLWT( 160 ) / 'AISONJ          ',  147.00D0 /
      DATA CHEMISTRY_SPC( 161 ), SPECIES_MOLWT( 161 ) / 'ATRPNJ          ',  215.00D0 /
      DATA CHEMISTRY_SPC( 162 ), SPECIES_MOLWT( 162 ) / 'AISO3J          ',  168.20D0 /
      DATA CHEMISTRY_SPC( 163 ), SPECIES_MOLWT( 163 ) / 'AXYL1J          ',  174.00D0 /
      DATA CHEMISTRY_SPC( 164 ), SPECIES_MOLWT( 164 ) / 'AOLGAJ          ',  206.00D0 /
      DATA CHEMISTRY_SPC( 165 ), SPECIES_MOLWT( 165 ) / 'AXYL2J          ',  185.00D0 /
      DATA CHEMISTRY_SPC( 166 ), SPECIES_MOLWT( 166 ) / 'ATOL1J          ',  163.00D0 /
      DATA CHEMISTRY_SPC( 167 ), SPECIES_MOLWT( 167 ) / 'ATOL2J          ',  175.00D0 /
      DATA CHEMISTRY_SPC( 168 ), SPECIES_MOLWT( 168 ) / 'ABNZ1J          ',  161.00D0 /
      DATA CHEMISTRY_SPC( 169 ), SPECIES_MOLWT( 169 ) / 'ABNZ2J          ',  134.00D0 /
      DATA CHEMISTRY_SPC( 170 ), SPECIES_MOLWT( 170 ) / 'ATRP1J          ',  177.00D0 /
      DATA CHEMISTRY_SPC( 171 ), SPECIES_MOLWT( 171 ) / 'AOLGBJ          ',  248.00D0 /
      DATA CHEMISTRY_SPC( 172 ), SPECIES_MOLWT( 172 ) / 'ATRP2J          ',  198.00D0 /
      DATA CHEMISTRY_SPC( 173 ), SPECIES_MOLWT( 173 ) / 'AISO1J          ',  132.00D0 /
      DATA CHEMISTRY_SPC( 174 ), SPECIES_MOLWT( 174 ) / 'AISO2J          ',  133.00D0 /
      DATA CHEMISTRY_SPC( 175 ), SPECIES_MOLWT( 175 ) / 'APAH1J          ',  195.60D0 /
      DATA CHEMISTRY_SPC( 176 ), SPECIES_MOLWT( 176 ) / 'APAH2J          ',  178.70D0 /
      DATA CHEMISTRY_SPC( 177 ), SPECIES_MOLWT( 177 ) / 'AALK1J          ',  225.00D0 /
      DATA CHEMISTRY_SPC( 178 ), SPECIES_MOLWT( 178 ) / 'AALK2J          ',  205.10D0 /
      DATA CHEMISTRY_SPC( 179 ), SPECIES_MOLWT( 179 ) / 'APOCI           ',  220.00D0 /
      DATA CHEMISTRY_SPC( 180 ), SPECIES_MOLWT( 180 ) / 'APNCOMI         ',  220.00D0 /
      DATA CHEMISTRY_SPC( 181 ), SPECIES_MOLWT( 181 ) / 'APOCJ           ',  220.00D0 /
      DATA CHEMISTRY_SPC( 182 ), SPECIES_MOLWT( 182 ) / 'APNCOMJ         ',  220.00D0 /
      DATA CHEMISTRY_SPC( 183 ), SPECIES_MOLWT( 183 ) / 'PCVOC           ',  170.00D0 /
      DATA CHEMISTRY_SPC( 184 ), SPECIES_MOLWT( 184 ) / 'PCSOARXN        ',  170.00D0 /
      DATA CHEMISTRY_SPC( 185 ), SPECIES_MOLWT( 185 ) / 'VLVPO1          ',  218.00D0 /
      DATA CHEMISTRY_SPC( 186 ), SPECIES_MOLWT( 186 ) / 'VSVPO1          ',  230.00D0 /
      DATA CHEMISTRY_SPC( 187 ), SPECIES_MOLWT( 187 ) / 'VSVPO2          ',  241.00D0 /
      DATA CHEMISTRY_SPC( 188 ), SPECIES_MOLWT( 188 ) / 'VSVPO3          ',  253.00D0 /
      DATA CHEMISTRY_SPC( 189 ), SPECIES_MOLWT( 189 ) / 'VIVPO1          ',  266.00D0 /
      DATA CHEMISTRY_SPC( 190 ), SPECIES_MOLWT( 190 ) / 'VLVOO1          ',  136.00D0 /
      DATA CHEMISTRY_SPC( 191 ), SPECIES_MOLWT( 191 ) / 'VLVOO2          ',  136.00D0 /
      DATA CHEMISTRY_SPC( 192 ), SPECIES_MOLWT( 192 ) / 'VSVOO2          ',  135.00D0 /
      DATA CHEMISTRY_SPC( 193 ), SPECIES_MOLWT( 193 ) / 'VSVOO3          ',  134.00D0 /
      DATA CHEMISTRY_SPC( 194 ), SPECIES_MOLWT( 194 ) / 'VSVOO1          ',  135.00D0 /


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
      DATA CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), CONVERT_CONC(  23 ) /   86, 'GC', F /  ! PINAL
      DATA CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), CONVERT_CONC(  24 ) /   52, 'GC', F /  ! HC10P
      DATA CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), CONVERT_CONC(  25 ) /   93, 'GC', F /  ! LIMAL
      DATA CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), CONVERT_CONC(  26 ) /   23, 'GC', F /  ! MEK
      DATA CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), CONVERT_CONC(  27 ) /   24, 'GC', F /  ! HKET
      DATA CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), CONVERT_CONC(  28 ) /   25, 'GC', F /  ! MACR
      DATA CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), CONVERT_CONC(  29 ) /   26, 'GC', F /  ! MACP
      DATA CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), CONVERT_CONC(  30 ) /   27, 'GC', F /  ! XO2
      DATA CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), CONVERT_CONC(  31 ) /   28, 'GC', F /  ! MVK
      DATA CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), CONVERT_CONC(  32 ) /   29, 'GC', F /  ! GLY
      DATA CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), CONVERT_CONC(  33 ) /   30, 'GC', F /  ! MGLY
      DATA CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), CONVERT_CONC(  34 ) /   31, 'GC', F /  ! DCB1
      DATA CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), CONVERT_CONC(  35 ) /   32, 'GC', F /  ! DCB2
      DATA CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), CONVERT_CONC(  36 ) /   33, 'GC', F /  ! BALD
      DATA CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), CONVERT_CONC(  37 ) /   34, 'GC', F /  ! CHO
      DATA CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), CONVERT_CONC(  38 ) /   35, 'GC', F /  ! OP1
      DATA CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), CONVERT_CONC(  39 ) /   36, 'GC', F /  ! OP2
      DATA CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), CONVERT_CONC(  40 ) /   37, 'GC', F /  ! OPB
      DATA CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), CONVERT_CONC(  41 ) /   38, 'GC', F /  ! PAA
      DATA CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), CONVERT_CONC(  42 ) /   39, 'GC', F /  ! ONIT
      DATA CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), CONVERT_CONC(  43 ) /   40, 'GC', F /  ! PAN
      DATA CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), CONVERT_CONC(  44 ) /   41, 'GC', F /  ! N2O5
      DATA CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), CONVERT_CONC(  45 ) /   42, 'GC', F /  ! SO2
      DATA CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), CONVERT_CONC(  46 ) /   43, 'GC', F /  ! SULF
      DATA CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), CONVERT_CONC(  47 ) /   44, 'GC', F /  ! SULRXN
      DATA CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), CONVERT_CONC(  48 ) /   45, 'GC', F /  ! ETH
      DATA CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), CONVERT_CONC(  49 ) /   46, 'GC', F /  ! HC3
      DATA CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), CONVERT_CONC(  50 ) /   47, 'GC', F /  ! HC3P
      DATA CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), CONVERT_CONC(  51 ) /  184, 'AE', T /  ! ASOATJ
      DATA CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), CONVERT_CONC(  52 ) /   48, 'GC', F /  ! HC5
      DATA CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), CONVERT_CONC(  53 ) /   49, 'GC', F /  ! HC5P
      DATA CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), CONVERT_CONC(  54 ) /   50, 'GC', F /  ! HC8
      DATA CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), CONVERT_CONC(  55 ) /   51, 'GC', F /  ! HC8P
      DATA CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), CONVERT_CONC(  56 ) /   53, 'GC', F /  ! ETE
      DATA CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), CONVERT_CONC(  57 ) /   54, 'GC', F /  ! ETEP
      DATA CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), CONVERT_CONC(  58 ) /   55, 'GC', F /  ! OLT
      DATA CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), CONVERT_CONC(  59 ) /   56, 'GC', F /  ! OLTP
      DATA CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), CONVERT_CONC(  60 ) /   57, 'GC', F /  ! OLI
      DATA CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), CONVERT_CONC(  61 ) /   58, 'GC', F /  ! OLIP
      DATA CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), CONVERT_CONC(  62 ) /   59, 'GC', F /  ! ACE
      DATA CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), CONVERT_CONC(  63 ) /   60, 'GC', F /  ! ORA1
      DATA CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), CONVERT_CONC(  64 ) /   61, 'GC', F /  ! BEN
      DATA CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), CONVERT_CONC(  65 ) /   62, 'GC', F /  ! BENP
      DATA CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), CONVERT_CONC(  66 ) /   63, 'GC', F /  ! EPX
      DATA CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), CONVERT_CONC(  67 ) /   64, 'GC', F /  ! PHEN
      DATA CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), CONVERT_CONC(  68 ) /   65, 'GC', F /  ! BENZRO2
      DATA CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), CONVERT_CONC(  69 ) /   66, 'GC', F /  ! TOL
      DATA CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), CONVERT_CONC(  70 ) /   67, 'GC', F /  ! TR2
      DATA CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), CONVERT_CONC(  71 ) /   68, 'GC', F /  ! TLP1
      DATA CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), CONVERT_CONC(  72 ) /   69, 'GC', F /  ! CSL
      DATA CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), CONVERT_CONC(  73 ) /   70, 'GC', F /  ! TOLRO2
      DATA CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), CONVERT_CONC(  74 ) /   71, 'GC', F /  ! XYM
      DATA CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), CONVERT_CONC(  75 ) /   72, 'GC', F /  ! XY2
      DATA CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), CONVERT_CONC(  76 ) /   73, 'GC', F /  ! XYL1
      DATA CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), CONVERT_CONC(  77 ) /   74, 'GC', F /  ! XYLRO2
      DATA CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), CONVERT_CONC(  78 ) /   75, 'GC', F /  ! XYE
      DATA CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), CONVERT_CONC(  79 ) /   77, 'GC', F /  ! ISO
      DATA CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), CONVERT_CONC(  80 ) /   78, 'GC', F /  ! ISOP
      DATA CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), CONVERT_CONC(  81 ) /   80, 'GC', F /  ! API
      DATA CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), CONVERT_CONC(  82 ) /   82, 'GC', F /  ! APIP1
      DATA CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), CONVERT_CONC(  83 ) /   83, 'GC', F /  ! APIP2
      DATA CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), CONVERT_CONC(  84 ) /   88, 'GC', F /  ! LIM
      DATA CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), CONVERT_CONC(  85 ) /   89, 'GC', F /  ! LIMP1
      DATA CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), CONVERT_CONC(  86 ) /   90, 'GC', F /  ! LIMP2
      DATA CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), CONVERT_CONC(  87 ) /   87, 'GC', F /  ! PINALP
      DATA CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), CONVERT_CONC(  88 ) /   94, 'GC', F /  ! LIMALP
      DATA CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), CONVERT_CONC(  89 ) /   97, 'GC', F /  ! RCO3
      DATA CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), CONVERT_CONC(  90 ) /   98, 'GC', F /  ! ACTP
      DATA CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), CONVERT_CONC(  91 ) /   99, 'GC', F /  ! MEKP
      DATA CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), CONVERT_CONC(  92 ) /  100, 'GC', F /  ! KETP
      DATA CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), CONVERT_CONC(  93 ) /  101, 'GC', F /  ! MCP
      DATA CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), CONVERT_CONC(  94 ) /  102, 'GC', F /  ! MVKP
      DATA CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), CONVERT_CONC(  95 ) /  103, 'GC', F /  ! UALP
      DATA CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), CONVERT_CONC(  96 ) /  104, 'GC', F /  ! DCB3
      DATA CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), CONVERT_CONC(  97 ) /  105, 'GC', F /  ! BALP
      DATA CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), CONVERT_CONC(  98 ) /  106, 'GC', F /  ! ADDC
      DATA CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), CONVERT_CONC(  99 ) /  107, 'GC', F /  ! MCT
      DATA CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), CONVERT_CONC( 100 ) /  108, 'GC', F /  ! MCTO
      DATA CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), CONVERT_CONC( 101 ) /  109, 'GC', F /  ! MOH
      DATA CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), CONVERT_CONC( 102 ) /  110, 'GC', F /  ! EOH
      DATA CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), CONVERT_CONC( 103 ) /  111, 'GC', F /  ! ROH
      DATA CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), CONVERT_CONC( 104 ) /  112, 'GC', F /  ! ETEG
      DATA CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), CONVERT_CONC( 105 ) /  113, 'GC', F /  ! ISHP
      DATA CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), CONVERT_CONC( 106 ) /  114, 'GC', F /  ! IEPOX
      DATA CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), CONVERT_CONC( 107 ) /  115, 'GC', F /  ! MAHP
      DATA CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), CONVERT_CONC( 108 ) /  116, 'GC', F /  ! ORA2
      DATA CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), CONVERT_CONC( 109 ) /  117, 'GC', F /  ! ORAP
      DATA CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), CONVERT_CONC( 110 ) /  118, 'GC', F /  ! PPN
      DATA CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), CONVERT_CONC( 111 ) /  119, 'GC', F /  ! MPAN
      DATA CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), CONVERT_CONC( 112 ) /  122, 'GC', F /  ! TRPN
      DATA CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), CONVERT_CONC( 113 ) /   95, 'GC', F /  ! HOM
      DATA CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), CONVERT_CONC( 114 ) /  120, 'GC', F /  ! NALD
      DATA CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), CONVERT_CONC( 115 ) /  121, 'GC', F /  ! ISON
      DATA CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), CONVERT_CONC( 116 ) /  123, 'GC', F /  ! MCTP
      DATA CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), CONVERT_CONC( 117 ) /  124, 'GC', F /  ! OLNN
      DATA CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), CONVERT_CONC( 118 ) /  125, 'GC', F /  ! OLND
      DATA CGRID_INDEX( 119 ), SPECIES_TYPE( 119 ), CONVERT_CONC( 119 ) /   84, 'GC', F /  ! APINP1
      DATA CGRID_INDEX( 120 ), SPECIES_TYPE( 120 ), CONVERT_CONC( 120 ) /   85, 'GC', F /  ! APINP2
      DATA CGRID_INDEX( 121 ), SPECIES_TYPE( 121 ), CONVERT_CONC( 121 ) /   91, 'GC', F /  ! LIMNP1
      DATA CGRID_INDEX( 122 ), SPECIES_TYPE( 122 ), CONVERT_CONC( 122 ) /   92, 'GC', F /  ! LIMNP2
      DATA CGRID_INDEX( 123 ), SPECIES_TYPE( 123 ), CONVERT_CONC( 123 ) /  126, 'GC', F /  ! ADCN
      DATA CGRID_INDEX( 124 ), SPECIES_TYPE( 124 ), CONVERT_CONC( 124 ) /  127, 'GC', F /  ! TOLP
      DATA CGRID_INDEX( 125 ), SPECIES_TYPE( 125 ), CONVERT_CONC( 125 ) /  128, 'GC', F /  ! PER1
      DATA CGRID_INDEX( 126 ), SPECIES_TYPE( 126 ), CONVERT_CONC( 126 ) /  129, 'GC', F /  ! XYLP
      DATA CGRID_INDEX( 127 ), SPECIES_TYPE( 127 ), CONVERT_CONC( 127 ) /  130, 'GC', F /  ! PER2
      DATA CGRID_INDEX( 128 ), SPECIES_TYPE( 128 ), CONVERT_CONC( 128 ) /   76, 'GC', F /  ! XYO2
      DATA CGRID_INDEX( 129 ), SPECIES_TYPE( 129 ), CONVERT_CONC( 129 ) /  131, 'GC', F /  ! XYOP
      DATA CGRID_INDEX( 130 ), SPECIES_TYPE( 130 ), CONVERT_CONC( 130 ) /  132, 'GC', F /  ! BAL1
      DATA CGRID_INDEX( 131 ), SPECIES_TYPE( 131 ), CONVERT_CONC( 131 ) /  133, 'GC', F /  ! BAL2
      DATA CGRID_INDEX( 132 ), SPECIES_TYPE( 132 ), CONVERT_CONC( 132 ) /   96, 'GC', F /  ! ELHOM
      DATA CGRID_INDEX( 133 ), SPECIES_TYPE( 133 ), CONVERT_CONC( 133 ) /  142, 'GC', F /  ! TOLNRXN
      DATA CGRID_INDEX( 134 ), SPECIES_TYPE( 134 ), CONVERT_CONC( 134 ) /  143, 'GC', F /  ! TOLHRXN
      DATA CGRID_INDEX( 135 ), SPECIES_TYPE( 135 ), CONVERT_CONC( 135 ) /  144, 'GC', F /  ! XYLNRXN
      DATA CGRID_INDEX( 136 ), SPECIES_TYPE( 136 ), CONVERT_CONC( 136 ) /  145, 'GC', F /  ! XYLHRXN
      DATA CGRID_INDEX( 137 ), SPECIES_TYPE( 137 ), CONVERT_CONC( 137 ) /  146, 'GC', F /  ! BNZNRXN
      DATA CGRID_INDEX( 138 ), SPECIES_TYPE( 138 ), CONVERT_CONC( 138 ) /  147, 'GC', F /  ! BNZHRXN
      DATA CGRID_INDEX( 139 ), SPECIES_TYPE( 139 ), CONVERT_CONC( 139 ) /  155, 'GC', F /  ! SOAALK
      DATA CGRID_INDEX( 140 ), SPECIES_TYPE( 140 ), CONVERT_CONC( 140 ) /  156, 'GC', F /  ! ALKRXN
      DATA CGRID_INDEX( 141 ), SPECIES_TYPE( 141 ), CONVERT_CONC( 141 ) /  140, 'GC', F /  ! ROCIOXY
      DATA CGRID_INDEX( 142 ), SPECIES_TYPE( 142 ), CONVERT_CONC( 142 ) /  141, 'GC', F /  ! SLOWROC
      DATA CGRID_INDEX( 143 ), SPECIES_TYPE( 143 ), CONVERT_CONC( 143 ) /  151, 'GC', F /  ! NAPH
      DATA CGRID_INDEX( 144 ), SPECIES_TYPE( 144 ), CONVERT_CONC( 144 ) /  152, 'GC', F /  ! PAHRO2
      DATA CGRID_INDEX( 145 ), SPECIES_TYPE( 145 ), CONVERT_CONC( 145 ) /  153, 'GC', F /  ! PAHNRXN
      DATA CGRID_INDEX( 146 ), SPECIES_TYPE( 146 ), CONVERT_CONC( 146 ) /  154, 'GC', F /  ! PAHHRXN
      DATA CGRID_INDEX( 147 ), SPECIES_TYPE( 147 ), CONVERT_CONC( 147 ) /  134, 'GC', F /  ! ACRO
      DATA CGRID_INDEX( 148 ), SPECIES_TYPE( 148 ), CONVERT_CONC( 148 ) /  135, 'GC', F /  ! BDE13
      DATA CGRID_INDEX( 149 ), SPECIES_TYPE( 149 ), CONVERT_CONC( 149 ) /  137, 'GC', F /  ! FURAN
      DATA CGRID_INDEX( 150 ), SPECIES_TYPE( 150 ), CONVERT_CONC( 150 ) /  138, 'GC', F /  ! FURANO2
      DATA CGRID_INDEX( 151 ), SPECIES_TYPE( 151 ), CONVERT_CONC( 151 ) /  139, 'GC', F /  ! FURANONE
      DATA CGRID_INDEX( 152 ), SPECIES_TYPE( 152 ), CONVERT_CONC( 152 ) /  136, 'GC', F /  ! PROG
      DATA CGRID_INDEX( 153 ), SPECIES_TYPE( 153 ), CONVERT_CONC( 153 ) /  148, 'GC', F /  ! SESQ
      DATA CGRID_INDEX( 154 ), SPECIES_TYPE( 154 ), CONVERT_CONC( 154 ) /  150, 'GC', F /  ! SESQNRO2
      DATA CGRID_INDEX( 155 ), SPECIES_TYPE( 155 ), CONVERT_CONC( 155 ) /  173, 'GC', F /  ! VROCP0OXY
      DATA CGRID_INDEX( 156 ), SPECIES_TYPE( 156 ), CONVERT_CONC( 156 ) /  176, 'GC', F /  ! VROCP3OXY
      DATA CGRID_INDEX( 157 ), SPECIES_TYPE( 157 ), CONVERT_CONC( 157 ) /  171, 'GC', F /  ! VROCN2OXY
      DATA CGRID_INDEX( 158 ), SPECIES_TYPE( 158 ), CONVERT_CONC( 158 ) /  149, 'GC', F /  ! SESQRO2
      DATA CGRID_INDEX( 159 ), SPECIES_TYPE( 159 ), CONVERT_CONC( 159 ) /  185, 'AE', T /  ! AGLYJ
      DATA CGRID_INDEX( 160 ), SPECIES_TYPE( 160 ), CONVERT_CONC( 160 ) /  207, 'AE', T /  ! AISONJ
      DATA CGRID_INDEX( 161 ), SPECIES_TYPE( 161 ), CONVERT_CONC( 161 ) /  202, 'AE', T /  ! ATRPNJ
      DATA CGRID_INDEX( 162 ), SPECIES_TYPE( 162 ), CONVERT_CONC( 162 ) /  245, 'AE', T /  ! AISO3J
      DATA CGRID_INDEX( 163 ), SPECIES_TYPE( 163 ), CONVERT_CONC( 163 ) /  188, 'AE', T /  ! AXYL1J
      DATA CGRID_INDEX( 164 ), SPECIES_TYPE( 164 ), CONVERT_CONC( 164 ) /  246, 'AE', T /  ! AOLGAJ
      DATA CGRID_INDEX( 165 ), SPECIES_TYPE( 165 ), CONVERT_CONC( 165 ) /  189, 'AE', T /  ! AXYL2J
      DATA CGRID_INDEX( 166 ), SPECIES_TYPE( 166 ), CONVERT_CONC( 166 ) /  191, 'AE', T /  ! ATOL1J
      DATA CGRID_INDEX( 167 ), SPECIES_TYPE( 167 ), CONVERT_CONC( 167 ) /  192, 'AE', T /  ! ATOL2J
      DATA CGRID_INDEX( 168 ), SPECIES_TYPE( 168 ), CONVERT_CONC( 168 ) /  194, 'AE', T /  ! ABNZ1J
      DATA CGRID_INDEX( 169 ), SPECIES_TYPE( 169 ), CONVERT_CONC( 169 ) /  195, 'AE', T /  ! ABNZ2J
      DATA CGRID_INDEX( 170 ), SPECIES_TYPE( 170 ), CONVERT_CONC( 170 ) /  200, 'AE', T /  ! ATRP1J
      DATA CGRID_INDEX( 171 ), SPECIES_TYPE( 171 ), CONVERT_CONC( 171 ) /  247, 'AE', T /  ! AOLGBJ
      DATA CGRID_INDEX( 172 ), SPECIES_TYPE( 172 ), CONVERT_CONC( 172 ) /  201, 'AE', T /  ! ATRP2J
      DATA CGRID_INDEX( 173 ), SPECIES_TYPE( 173 ), CONVERT_CONC( 173 ) /  203, 'AE', T /  ! AISO1J
      DATA CGRID_INDEX( 174 ), SPECIES_TYPE( 174 ), CONVERT_CONC( 174 ) /  204, 'AE', T /  ! AISO2J
      DATA CGRID_INDEX( 175 ), SPECIES_TYPE( 175 ), CONVERT_CONC( 175 ) /  197, 'AE', T /  ! APAH1J
      DATA CGRID_INDEX( 176 ), SPECIES_TYPE( 176 ), CONVERT_CONC( 176 ) /  198, 'AE', T /  ! APAH2J
      DATA CGRID_INDEX( 177 ), SPECIES_TYPE( 177 ), CONVERT_CONC( 177 ) /  186, 'AE', T /  ! AALK1J
      DATA CGRID_INDEX( 178 ), SPECIES_TYPE( 178 ), CONVERT_CONC( 178 ) /  187, 'AE', T /  ! AALK2J
      DATA CGRID_INDEX( 179 ), SPECIES_TYPE( 179 ), CONVERT_CONC( 179 ) /  248, 'AE', T /  ! APOCI
      DATA CGRID_INDEX( 180 ), SPECIES_TYPE( 180 ), CONVERT_CONC( 180 ) /  250, 'AE', T /  ! APNCOMI
      DATA CGRID_INDEX( 181 ), SPECIES_TYPE( 181 ), CONVERT_CONC( 181 ) /  249, 'AE', T /  ! APOCJ
      DATA CGRID_INDEX( 182 ), SPECIES_TYPE( 182 ), CONVERT_CONC( 182 ) /  251, 'AE', T /  ! APNCOMJ
      DATA CGRID_INDEX( 183 ), SPECIES_TYPE( 183 ), CONVERT_CONC( 183 ) /  167, 'GC', F /  ! PCVOC
      DATA CGRID_INDEX( 184 ), SPECIES_TYPE( 184 ), CONVERT_CONC( 184 ) /  168, 'GC', F /  ! PCSOARXN
      DATA CGRID_INDEX( 185 ), SPECIES_TYPE( 185 ), CONVERT_CONC( 185 ) /  157, 'GC', F /  ! VLVPO1
      DATA CGRID_INDEX( 186 ), SPECIES_TYPE( 186 ), CONVERT_CONC( 186 ) /  158, 'GC', F /  ! VSVPO1
      DATA CGRID_INDEX( 187 ), SPECIES_TYPE( 187 ), CONVERT_CONC( 187 ) /  159, 'GC', F /  ! VSVPO2
      DATA CGRID_INDEX( 188 ), SPECIES_TYPE( 188 ), CONVERT_CONC( 188 ) /  160, 'GC', F /  ! VSVPO3
      DATA CGRID_INDEX( 189 ), SPECIES_TYPE( 189 ), CONVERT_CONC( 189 ) /  161, 'GC', F /  ! VIVPO1
      DATA CGRID_INDEX( 190 ), SPECIES_TYPE( 190 ), CONVERT_CONC( 190 ) /  162, 'GC', F /  ! VLVOO1
      DATA CGRID_INDEX( 191 ), SPECIES_TYPE( 191 ), CONVERT_CONC( 191 ) /  163, 'GC', F /  ! VLVOO2
      DATA CGRID_INDEX( 192 ), SPECIES_TYPE( 192 ), CONVERT_CONC( 192 ) /  165, 'GC', F /  ! VSVOO2
      DATA CGRID_INDEX( 193 ), SPECIES_TYPE( 193 ), CONVERT_CONC( 193 ) /  166, 'GC', F /  ! VSVOO3
      DATA CGRID_INDEX( 194 ), SPECIES_TYPE( 194 ), CONVERT_CONC( 194 ) /  164, 'GC', F /  ! VSVOO1

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
      INTEGER :: INDEX_PINAL     =   23
      INTEGER :: INDEX_HC10P     =   24
      INTEGER :: INDEX_LIMAL     =   25
      INTEGER :: INDEX_MEK       =   26
      INTEGER :: INDEX_HKET      =   27
      INTEGER :: INDEX_MACR      =   28
      INTEGER :: INDEX_MACP      =   29
      INTEGER :: INDEX_XO2       =   30
      INTEGER :: INDEX_MVK       =   31
      INTEGER :: INDEX_GLY       =   32
      INTEGER :: INDEX_MGLY      =   33
      INTEGER :: INDEX_DCB1      =   34
      INTEGER :: INDEX_DCB2      =   35
      INTEGER :: INDEX_BALD      =   36
      INTEGER :: INDEX_CHO       =   37
      INTEGER :: INDEX_OP1       =   38
      INTEGER :: INDEX_OP2       =   39
      INTEGER :: INDEX_OPB       =   40
      INTEGER :: INDEX_PAA       =   41
      INTEGER :: INDEX_ONIT      =   42
      INTEGER :: INDEX_PAN       =   43
      INTEGER :: INDEX_N2O5      =   44
      INTEGER :: INDEX_SO2       =   45
      INTEGER :: INDEX_SULF      =   46
      INTEGER :: INDEX_SULRXN    =   47
      INTEGER :: INDEX_ETH       =   48
      INTEGER :: INDEX_HC3       =   49
      INTEGER :: INDEX_HC3P      =   50
      INTEGER :: INDEX_ASOATJ    =   51
      INTEGER :: INDEX_HC5       =   52
      INTEGER :: INDEX_HC5P      =   53
      INTEGER :: INDEX_HC8       =   54
      INTEGER :: INDEX_HC8P      =   55
      INTEGER :: INDEX_ETE       =   56
      INTEGER :: INDEX_ETEP      =   57
      INTEGER :: INDEX_OLT       =   58
      INTEGER :: INDEX_OLTP      =   59
      INTEGER :: INDEX_OLI       =   60
      INTEGER :: INDEX_OLIP      =   61
      INTEGER :: INDEX_ACE       =   62
      INTEGER :: INDEX_ORA1      =   63
      INTEGER :: INDEX_BEN       =   64
      INTEGER :: INDEX_BENP      =   65
      INTEGER :: INDEX_EPX       =   66
      INTEGER :: INDEX_PHEN      =   67
      INTEGER :: INDEX_BENZRO2   =   68
      INTEGER :: INDEX_TOL       =   69
      INTEGER :: INDEX_TR2       =   70
      INTEGER :: INDEX_TLP1      =   71
      INTEGER :: INDEX_CSL       =   72
      INTEGER :: INDEX_TOLRO2    =   73
      INTEGER :: INDEX_XYM       =   74
      INTEGER :: INDEX_XY2       =   75
      INTEGER :: INDEX_XYL1      =   76
      INTEGER :: INDEX_XYLRO2    =   77
      INTEGER :: INDEX_XYE       =   78
      INTEGER :: INDEX_ISO       =   79
      INTEGER :: INDEX_ISOP      =   80
      INTEGER :: INDEX_API       =   81
      INTEGER :: INDEX_APIP1     =   82
      INTEGER :: INDEX_APIP2     =   83
      INTEGER :: INDEX_LIM       =   84
      INTEGER :: INDEX_LIMP1     =   85
      INTEGER :: INDEX_LIMP2     =   86
      INTEGER :: INDEX_PINALP    =   87
      INTEGER :: INDEX_LIMALP    =   88
      INTEGER :: INDEX_RCO3      =   89
      INTEGER :: INDEX_ACTP      =   90
      INTEGER :: INDEX_MEKP      =   91
      INTEGER :: INDEX_KETP      =   92
      INTEGER :: INDEX_MCP       =   93
      INTEGER :: INDEX_MVKP      =   94
      INTEGER :: INDEX_UALP      =   95
      INTEGER :: INDEX_DCB3      =   96
      INTEGER :: INDEX_BALP      =   97
      INTEGER :: INDEX_ADDC      =   98
      INTEGER :: INDEX_MCT       =   99
      INTEGER :: INDEX_MCTO      =  100
      INTEGER :: INDEX_MOH       =  101
      INTEGER :: INDEX_EOH       =  102
      INTEGER :: INDEX_ROH       =  103
      INTEGER :: INDEX_ETEG      =  104
      INTEGER :: INDEX_ISHP      =  105
      INTEGER :: INDEX_IEPOX     =  106
      INTEGER :: INDEX_MAHP      =  107
      INTEGER :: INDEX_ORA2      =  108
      INTEGER :: INDEX_ORAP      =  109
      INTEGER :: INDEX_PPN       =  110
      INTEGER :: INDEX_MPAN      =  111
      INTEGER :: INDEX_TRPN      =  112
      INTEGER :: INDEX_HOM       =  113
      INTEGER :: INDEX_NALD      =  114
      INTEGER :: INDEX_ISON      =  115
      INTEGER :: INDEX_MCTP      =  116
      INTEGER :: INDEX_OLNN      =  117
      INTEGER :: INDEX_OLND      =  118
      INTEGER :: INDEX_APINP1    =  119
      INTEGER :: INDEX_APINP2    =  120
      INTEGER :: INDEX_LIMNP1    =  121
      INTEGER :: INDEX_LIMNP2    =  122
      INTEGER :: INDEX_ADCN      =  123
      INTEGER :: INDEX_TOLP      =  124
      INTEGER :: INDEX_PER1      =  125
      INTEGER :: INDEX_XYLP      =  126
      INTEGER :: INDEX_PER2      =  127
      INTEGER :: INDEX_XYO2      =  128
      INTEGER :: INDEX_XYOP      =  129
      INTEGER :: INDEX_BAL1      =  130
      INTEGER :: INDEX_BAL2      =  131
      INTEGER :: INDEX_ELHOM     =  132
      INTEGER :: INDEX_TOLNRXN   =  133
      INTEGER :: INDEX_TOLHRXN   =  134
      INTEGER :: INDEX_XYLNRXN   =  135
      INTEGER :: INDEX_XYLHRXN   =  136
      INTEGER :: INDEX_BNZNRXN   =  137
      INTEGER :: INDEX_BNZHRXN   =  138
      INTEGER :: INDEX_SOAALK    =  139
      INTEGER :: INDEX_ALKRXN    =  140
      INTEGER :: INDEX_ROCIOXY   =  141
      INTEGER :: INDEX_SLOWROC   =  142
      INTEGER :: INDEX_NAPH      =  143
      INTEGER :: INDEX_PAHRO2    =  144
      INTEGER :: INDEX_PAHNRXN   =  145
      INTEGER :: INDEX_PAHHRXN   =  146
      INTEGER :: INDEX_ACRO      =  147
      INTEGER :: INDEX_BDE13     =  148
      INTEGER :: INDEX_FURAN     =  149
      INTEGER :: INDEX_FURANO2   =  150
      INTEGER :: INDEX_FURANONE  =  151
      INTEGER :: INDEX_PROG      =  152
      INTEGER :: INDEX_SESQ      =  153
      INTEGER :: INDEX_SESQNRO2  =  154
      INTEGER :: INDEX_VROCP0OXY =  155
      INTEGER :: INDEX_VROCP3OXY =  156
      INTEGER :: INDEX_VROCN2OXY =  157
      INTEGER :: INDEX_SESQRO2   =  158
      INTEGER :: INDEX_AGLYJ     =  159
      INTEGER :: INDEX_AISONJ    =  160
      INTEGER :: INDEX_ATRPNJ    =  161
      INTEGER :: INDEX_AISO3J    =  162
      INTEGER :: INDEX_AXYL1J    =  163
      INTEGER :: INDEX_AOLGAJ    =  164
      INTEGER :: INDEX_AXYL2J    =  165
      INTEGER :: INDEX_ATOL1J    =  166
      INTEGER :: INDEX_ATOL2J    =  167
      INTEGER :: INDEX_ABNZ1J    =  168
      INTEGER :: INDEX_ABNZ2J    =  169
      INTEGER :: INDEX_ATRP1J    =  170
      INTEGER :: INDEX_AOLGBJ    =  171
      INTEGER :: INDEX_ATRP2J    =  172
      INTEGER :: INDEX_AISO1J    =  173
      INTEGER :: INDEX_AISO2J    =  174
      INTEGER :: INDEX_APAH1J    =  175
      INTEGER :: INDEX_APAH2J    =  176
      INTEGER :: INDEX_AALK1J    =  177
      INTEGER :: INDEX_AALK2J    =  178
      INTEGER :: INDEX_APOCI     =  179
      INTEGER :: INDEX_APNCOMI   =  180
      INTEGER :: INDEX_APOCJ     =  181
      INTEGER :: INDEX_APNCOMJ   =  182
      INTEGER :: INDEX_PCVOC     =  183
      INTEGER :: INDEX_PCSOARXN  =  184
      INTEGER :: INDEX_VLVPO1    =  185
      INTEGER :: INDEX_VSVPO1    =  186
      INTEGER :: INDEX_VSVPO2    =  187
      INTEGER :: INDEX_VSVPO3    =  188
      INTEGER :: INDEX_VIVPO1    =  189
      INTEGER :: INDEX_VLVOO1    =  190
      INTEGER :: INDEX_VLVOO2    =  191
      INTEGER :: INDEX_VSVOO2    =  192
      INTEGER :: INDEX_VSVOO3    =  193
      INTEGER :: INDEX_VSVOO1    =  194

      INTEGER, PARAMETER :: N_ACT_SP = 194

      INTEGER, PARAMETER :: NRXNS = 484

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

      INTEGER, PARAMETER ::        NSUNLIGHT_RXNS   =   38

      INTEGER, PARAMETER ::        NTHERMAL_RXNS    =  446

      INTEGER, PARAMETER ::        KUNITS           =    2

      INTEGER  :: IRXXN

      INTEGER, PARAMETER :: NMPHOT =  37
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & 
     &     31,   32,   33,   34,   35,   36,   37/

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   13,   13,   17,   18, & 
     &     19,   20,   21,   22,   23,   24,   25,   25,   25,   26, & 
     &     27,   27,   27,   28,   29,   30,   31/

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & 
     &     31,   32,   33,   34,   35,   36,   37/

      REAL( 8 )          :: RTDAT( 3,NRXNS )

      INTEGER, PARAMETER :: NFALLOFF =  17
      REAL( 8 )          :: RFDAT( 5,NFALLOFF )

      INTEGER            :: KTYPE( NRXNS )

      DATA ( KTYPE( IRXXN ), IRXXN = 1, NRXNS ) /  & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    3,    3,    3, & ! 3   
     &      3,    2,    3,    3,    3,    3,    3,    3,    9,    9, & ! 4   
     &      3,   10,   10,    3,    9,    3,    3,    3,   10,   10, & ! 5   
     &      8,    1,    1,    3,    3,    3,   10,    5,    1,   10, & ! 6   
     &      5,    3,   10,    9,    3,    3,    3,    3,    3,   10, & ! 7   
     &      3,    3,   10,    3,    3,    1,    1,    3,    3,    3, & ! 8   
     &      3,    1,    3,    3,    3,    4,    3,    3,    1,    3, & ! 9   
     &      3,    3,    1,    3,    3,    3,    1,    3,    3,    3, & ! O   
     &      3,    3,    3,    3,    3,    1,    3,    3,    3,    1, & ! 1   
     &      1,    1,    3,    3,    1,    1,    1,    3,    1,    3, & ! 2   
     &      1,    3,    3,    3,    3,    3,    3,    1,    1,    3, & ! 3   
     &      3,    1,    1,    1,    1,    1,    1,    4,    3,    3, & ! 4   
     &      3,    3,    1,    3,    3,    3,    3,    1,    3,    3, & ! 5   
     &      3,    1,    1,    3,    1,    3,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,   10,    5,   10,    5,    3,    3, & ! 7   
     &      3,    3,    1,    1,    1,    1,    1,    1,    3,    1, & ! 8   
     &      3,    3,    1,    3,    3,    3,    3,    1,    1,    1, & ! 9   
     &      1,    1,    1,    1,    1,    3,    3,    3,    3,    3, & ! O   
     &      1,    1,    3,    3,    3,    3,    1,    1,    3,    3, & ! 1   
     &      1,    1,    1,    3,    1,    1,    1,    1,    3,    3, & ! 2   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 3   
     &      3,    3,    3,    3,    3,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    3,    3,    3,    3,    3,    3,    3, & ! 5   
     &      3,    3,    3,    3,    3,    1,    3,    3,    3,    3, & ! 6   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 7   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 8   
     &      1,    3,    1,    3,    1,    3,    1,    3,    3,    3, & ! 9   
     &      3,    3,    3,    3,    1,    3,    3,    3,    3,    3, & ! O   
     &      3,    3,    3,    1,    3,    3,    3,    3,    3,    3, & ! 1   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 2   
     &      3,    3,    1,    3,    1,    3,    1,    3,    1,    3, & ! 3   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 4   
     &      3,    3,    3,    3,    3,    3,    3,    3,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 8   
     &      1,    1,    1,    1,    3,    3,    3,    1,    3,    3, & ! 9   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    3,    3,    3,    3,    3,    3,    3,    3, & ! 1   
     &      3,    1,    1,    3,    3,    1,    1,    1,    3,    3, & ! 2   
     &      1,    1,    3,    3,    1,    1,    1,    1,    1,    3, & ! 3   
     &      3,    1,    1,    1,    3,    1,    3,   -1,   -1,    1, & ! 4   
     &      1,   -1,   -1,   12,   -1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &     -1,    1,   -1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1/     !  8   

      INTEGER            :: IRXBITS( NRXNS )

      DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    0,    0,    0, & ! 3   
     &      0,   20,    0,   16,   32,    8,  128,    0,    0,    8, & ! 4   
     &      0,    1,    1,    0,    0,   16,    0,    0,    1,    1, & ! 5   
     &      0,    0,    0,    0,    0,    0,    1,    0,    8,    1, & ! 6   
     &      0,    0,    1,    0,   64,    0,    0,    0,    0,    1, & ! 7   
     &      0,    0,    1,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    1,    0,    1,    0,    0,    0, & ! 7   
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
     &      0,    0,    0,    0,    0,    0,    0,    1,    1,    0, & ! 4   
     &      0,    1,    1,    2,    1,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      1,    0,    1,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0/     !  8   

      INTEGER, PARAMETER :: NTERMS_JACOB =    37636

      INTEGER, PARAMETER :: NSTEPS_JACOB =      968

      INTEGER            :: IORDER( NRXNS )

      DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    2,    2,    2, & ! 3   
     &      2,    3,    2,    2,    2,    2,    2,    2,    2,    3, & ! 4   
     &      2,    2,    2,    2,    2,    3,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    1,    2,    2, & ! 6   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    2,    1,    2,    1,    2,    1, & ! 7   
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
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2/     !  8   

      INTEGER, PARAMETER :: KTN1 = 179
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     62,   63,   69,   86,   87,   92,   99,  103,  107,  116, & ! O   
     &    120,  121,  122,  125,  126,  127,  129,  131,  138,  139, & ! 1   
     &    142,  143,  144,  145,  146,  147,  153,  158,  162,  163, & ! 2   
     &    165,  167,  168,  169,  170,  171,  172,  173,  174,  183, & ! 3   
     &    184,  185,  186,  187,  188,  190,  193,  198,  199,  200, & ! 4   
     &    201,  202,  203,  204,  205,  211,  212,  217,  218,  221, & ! 5   
     &    222,  223,  225,  226,  227,  228,  246,  247,  248,  249, & ! 6   
     &    250,  251,  252,  253,  266,  291,  293,  295,  297,  305, & ! 7   
     &    314,  333,  335,  337,  339,  359,  360,  361,  362,  363, & ! 8   
     &    364,  365,  366,  367,  368,  369,  370,  371,  372,  373, & ! 9   
     &    374,  375,  376,  377,  378,  379,  380,  381,  382,  383, & ! O   
     &    384,  385,  386,  387,  388,  389,  390,  391,  392,  393, & ! 1   
     &    394,  398,  401,  402,  403,  404,  405,  406,  407,  408, & ! 2   
     &    409,  410,  411,  412,  422,  423,  426,  427,  428,  431, & ! 3   
     &    432,  435,  436,  437,  438,  439,  442,  443,  444,  446, & ! 4   
     &    450,  451,  456,  457,  458,  459,  460,  461,  462,  463, & ! 5   
     &    464,  465,  466,  467,  468,  469,  470,  472,  474,  475, & ! 6   
     &    476,  477,  478,  479,  480,  481,  482,  483,  484/     !7   

      INTEGER, PARAMETER :: KTN2 =   1
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &     42/

      INTEGER, PARAMETER :: KTN3 = 237
      INTEGER            :: KRX3( KTN3 )

      DATA ( KRX3( IRXXN ), IRXXN = 1, KTN3 ) / & 
     &     38,   39,   40,   41,   43,   44,   45,   46,   47,   48, & ! O   
     &     51,   54,   56,   57,   58,   64,   65,   66,   72,   75, & ! 1   
     &     76,   77,   78,   79,   81,   82,   84,   85,   88,   89, & ! 2   
     &     90,   91,   93,   94,   95,   97,   98,  100,  101,  102, & ! 3   
     &    104,  105,  106,  108,  109,  110,  111,  112,  113,  114, & ! 4   
     &    115,  117,  118,  119,  123,  124,  128,  130,  132,  133, & ! 5   
     &    134,  135,  136,  137,  140,  141,  149,  150,  151,  152, & ! 6   
     &    154,  155,  156,  157,  159,  160,  161,  164,  166,  179, & ! 7   
     &    180,  181,  182,  189,  191,  192,  194,  195,  196,  197, & ! 8   
     &    206,  207,  208,  209,  210,  213,  214,  215,  216,  219, & ! 9   
     &    220,  224,  229,  230,  231,  232,  233,  234,  235,  236, & ! O   
     &    237,  238,  239,  240,  241,  242,  243,  244,  245,  254, & ! 1   
     &    255,  256,  257,  258,  259,  260,  261,  262,  263,  264, & ! 2   
     &    265,  267,  268,  269,  270,  271,  272,  273,  274,  275, & ! 3   
     &    276,  277,  278,  279,  280,  281,  282,  283,  284,  285, & ! 4   
     &    286,  287,  288,  289,  290,  292,  294,  296,  298,  299, & ! 5   
     &    300,  301,  302,  303,  304,  306,  307,  308,  309,  310, & ! 6   
     &    311,  312,  313,  315,  316,  317,  318,  319,  320,  321, & ! 7   
     &    322,  323,  324,  325,  326,  327,  328,  329,  330,  331, & ! 8   
     &    332,  334,  336,  338,  340,  341,  342,  343,  344,  345, & ! 9   
     &    346,  347,  348,  349,  350,  351,  352,  353,  354,  355, & ! O   
     &    356,  357,  358,  395,  396,  397,  399,  400,  413,  414, & ! 1   
     &    415,  416,  417,  418,  419,  420,  421,  424,  425,  429, & ! 2   
     &    430,  433,  434,  440,  441,  445,  447/     !  3   

      INTEGER, PARAMETER :: KTN4 =   2
      INTEGER            :: KRX4( KTN4 )

      DATA ( KRX4( IRXXN ), IRXXN = 1, KTN4 ) / & 
     &     96,  148/

      INTEGER, PARAMETER :: KTN5 =   4
      INTEGER            :: KRX5( KTN5 )

      DATA ( KRX5( IRXXN ), IRXXN = 1, KTN5 ) / & 
     &     68,   71,  176,  178/

      INTEGER, PARAMETER :: KTN6 =   0
      INTEGER            :: KRX6( 1 )

      DATA   KRX6( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN7 =   0
      INTEGER            :: KRX7( 1 )

      DATA   KRX7( 1 ) / 0 /

      INTEGER, PARAMETER :: NWM =   1
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &     42/
      REAL( 8 ),    PARAMETER :: ATM_AIR = 1.00000D+06

      INTEGER, PARAMETER :: NWW =   3
      INTEGER            :: NRXWW( NWW )

      DATA ( NRXWW( IRXXN ), IRXXN = 1, NWW ) / & 
     &     46,   50,   69/

      INTEGER, PARAMETER :: NWO2 =   3
      INTEGER            :: NRXWO2( NWO2 )

      DATA ( NRXWO2( IRXXN ), IRXXN = 1, NWO2 ) / & 
     &     42,   44,   56/
      REAL( 8 ),    PARAMETER :: ATM_O2 = 2.09500D+05

      INTEGER, PARAMETER :: NWN2 =   1
      INTEGER            :: NRXWN2( NWN2 )

      DATA ( NRXWN2( IRXXN ), IRXXN = 1, NWN2 ) / & 
     &     45/
      REAL( 8 ),    PARAMETER :: ATM_N2 = 7.80800D+05

      INTEGER, PARAMETER :: NWCH4 =   1
      INTEGER            :: NRXWCH4( NWCH4 )

      DATA ( NRXWCH4( IRXXN ), IRXXN = 1, NWCH4 ) / & 
     &     75/
      REAL( 8 ),    PARAMETER :: ATM_CH4 = 1.85000D+00

      INTEGER, PARAMETER :: NWH2 =   1
      INTEGER            :: NRXWH2( NWH2 )

      DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) / & 
     &     47/
      REAL( 8 ),    PARAMETER :: ATM_H2 = 5.60000D-01

      INTEGER, PARAMETER :: MXPRD =  21
      INTEGER            :: IRR( NRXNS,MXPRD+3 )

      DATA ( IRR( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    4,    6,    8,    8,    9,   10,   11,   13, & ! O   
     &     13,   15,   17,   19,   19,   21,   23,   25,   26,   22, & ! 1   
     &     27,   28,   31,   32,   32,   32,   33,   34,   35,   36, & ! 2   
     &     38,   39,   40,   41,   42,   43,   43,    1,    1,    1, & ! 3   
     &      1,    2,    2,    3,    3,    3,    5,    5,   12,   12, & ! 4   
     &      4,    7,    7,    7,    7,    7,    9,    6,    6,    6, & ! 5   
     &     10,    8,    8,    8,    8,    8,    8,   44,   44,    6, & ! 6   
     &     11,   11,   45,   14,    5,   48,   49,   52,   54,   56, & ! 7   
     &     58,   60,   62,   64,   69,   74,   78,   79,   81,   84, & ! 8   
     &     23,   25,   13,   15,   17,   19,   26,   22,   27,   28, & ! 9   
     &     31,   21,   32,   33,   34,   35,   96,   36,   67,   72, & ! O   
     &     66,   99,  101,  102,  103,  104,   38,   39,   40,  105, & ! 1   
     &    107,   63,  108,   41,   43,  110,  111,   42,  112,  114, & ! 2   
     &    115,   56,   58,   60,   79,   81,   84,   25,  112,   28, & ! 3   
     &     31,   21,   34,   35,   96,   66,  100,   56,   58,   60, & ! 4   
     &     79,   81,   84,  112,   13,   15,   17,   28,   21,   32, & ! 5   
     &     33,   67,   72,   66,   99,  111,   70,  124,   75,  126, & ! 6   
     &    128,  129,   87,   88,   20,   43,   89,  110,   29,  111, & ! 7   
     &     16,   18,   50,   53,   55,   57,   59,   61,   65,   71, & ! 8   
     &    124,  125,   76,  126,  127,  129,   80,   82,   83,  119, & ! 9   
     &    120,   85,   86,  121,  122,   87,   88,   20,   89,   90, & ! O   
     &     91,   92,   29,   93,   94,   95,   97,  130,   98,  116, & ! 1   
     &    109,  117,  118,  123,   30,  131,   37,  100,   16,   18, & ! 2   
     &     50,   53,   55,   57,   59,   61,   65,   71,  124,  125, & ! 3   
     &     76,  126,  127,  129,   80,   82,   83,  119,  120,   85, & ! 4   
     &     86,  121,  122,   87,   88,   20,   89,   90,   91,   92, & ! 5   
     &     29,   93,   94,   95,   98,   37,  116,  109,  117,  118, & ! 6   
     &    123,   30,   16,   18,   50,   53,   55,   57,   59,   61, & ! 7   
     &     65,   71,  124,  125,   76,  126,  127,  129,   80,   82, & ! 8   
     &     83,  119,  120,   85,   86,  121,  122,   20,   89,   90, & ! 9   
     &     91,   92,   29,   93,   94,   95,   97,  130,   98,  116, & ! O   
     &    109,  117,  118,  123,   30,   18,   50,   53,   55,   57, & ! 1   
     &     59,   61,   65,   71,  124,  125,   76,  126,  127,  129, & ! 2   
     &     80,   82,   83,  119,  120,   85,   86,  121,  122,   20, & ! 3   
     &     89,   90,   91,   92,   29,   93,   94,   95,   97,  130, & ! 4   
     &     98,  116,  109,  117,  118,  123,   30,   89,   16,   18, & ! 5   
     &     50,   53,   55,   57,   59,   61,   65,   71,  124,  125, & ! 6   
     &     76,  126,  127,  129,   80,   82,   85,   20,   89,   90, & ! 7   
     &     91,   92,   29,   93,   94,   95,   97,  130,   98,  116, & ! 8   
     &    109,  117,  118,  123,  117,  117,  118,   30,   30,   30, & ! 9   
     &     83,   83,   83,   86,   86,   86,  120,  120,  120,  122, & ! O   
     &    122,  122,   73,   73,   77,   77,   68,   68,  139,  106, & ! 1   
     &    141,  142,  143,  144,  144,  147,  147,  147,  148,  148, & ! 2   
     &    148,  149,  150,  150,  151,  149,  149,  152,  153,  154, & ! 3   
     &    154,  154,  153,  153,  158,  158,  158,   32,   33,  115, & ! 4   
     &    112,   44,    6,    1,  106,  163,  165,  166,  167,  168, & ! 5   
     &    169,  170,  172,  173,  174,  175,  176,  177,  178,  179, & ! 6   
     &    180,  181,  182,  183,  185,  186,  187,  188,  189,  190, & ! 7   
     &    191,  194,  192,  193/     !  8   

      DATA ( IRR( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    5,   12,    7, & ! 3   
     &      6,    0,    1,    0,    0,    0,    0,   12,   12,   12, & ! 4   
     &      5,    2,    5,   12,   12,    7,    5,    2,    2,    5, & ! 5   
     &      5,    5,   12,    7,    6,    8,    6,    0,    0,   12, & ! 6   
     &      0,    5,    5,    5,    0,    5,    5,    5,    5,    5, & ! 7   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! 8   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! 9   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! O   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! 1   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! 2   
     &      5,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    8,    8,    8, & ! 4   
     &      8,    8,    8,    8,    8,    8,    8,    8,    8,    8, & ! 5   
     &      8,    8,    8,    8,    8,    8,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    6,    0,    6,    0,    6,    0, & ! 7   
     &      7,    7,    7,    7,    7,    7,    7,    7,    7,    7, & ! 8   
     &      7,    7,    7,    7,    7,    7,    7,    7,    7,    7, & ! 9   
     &      7,    7,    7,    7,    7,    7,    7,    7,    7,    7, & ! O   
     &      7,    7,    7,    7,    7,    7,    7,    7,    7,    7, & ! 1   
     &      7,    7,    7,    7,    7,    6,    6,    6,   12,   12, & ! 2   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 3   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 4   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 5   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 6   
     &     12,   12,   16,   16,   16,   16,   16,   16,   16,   16, & ! 7   
     &     16,   16,   16,   16,   16,   16,   16,   16,   16,   16, & ! 8   
     &     16,   16,   16,   16,   16,   16,   16,   16,   16,   16, & ! 9   
     &     16,   16,   16,   16,   16,   16,   16,   16,   16,   16, & ! O   
     &     16,   16,   16,   16,   16,   20,   20,   20,   20,   20, & ! 1   
     &     20,   20,   20,   20,   20,   20,   20,   20,   20,   20, & ! 2   
     &     20,   20,   20,   20,   20,   20,   20,   20,   20,   20, & ! 3   
     &     20,   20,   20,   20,   20,   20,   20,   20,   20,   20, & ! 4   
     &     20,   20,   20,   20,   20,   20,   20,   89,    8,    8, & ! 5   
     &      8,    8,    8,    8,    8,    8,    8,    8,    8,    8, & ! 6   
     &      8,    8,    8,    8,    8,    8,    8,    8,    8,    8, & ! 7   
     &      8,    8,    8,    8,    8,    8,    8,    8,    8,    8, & ! 8   
     &      8,    8,    8,    8,  117,  118,  118,    8,   89,   30, & ! 9   
     &     82,   85,   80,   82,   85,   80,   82,   85,   80,   82, & ! O   
     &     85,   80,    7,   12,    7,   12,    7,   12,    5,    5, & ! 1   
     &      5,    5,    5,    7,   12,    5,    1,    8,    5,    1, & ! 2   
     &      8,    5,    7,   12,    5,    1,    8,    5,    8,   12, & ! 3   
     &      7,    8,    1,    5,   12,    8,    7,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    5, & ! 6   
     &      5,    5,    5,    5,    5,    5,    5,    5,    5,    5, & ! 7   
     &      5,    5,    5,    5/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0/     !  8   

      DATA ( IRR( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &      2,    3,    5,    2,    7,    2,    5,    5,    5,   14, & ! O   
     &     12,   12,   12,   16,   16,   12,   12,   12,   16,   18, & ! 1   
     &     12,    5,   16,   14,   13,   12,   12,   12,   12,   37, & ! 2   
     &      5,    5,    5,    5,   12,   20,   16,   12,    5,    6, & ! 3   
     &      8,    1,    0,    2,    2,    5,   12,    0,    4,    4, & ! 4   
     &     12,    6,    9,    6,   10,    6,    6,    7,    8,   10, & ! 5   
     &      8,   12,    5,    6,    7,    6,   44,    6,   10,   11, & ! 6   
     &     12,    6,   12,   12,   16,   18,   50,   53,   12,   57, & ! 7   
     &     59,   61,    5,   12,   12,   12,   12,   80,   82,   85, & ! 8   
     &     87,   88,   12,   20,   89,   90,   91,   92,   12,   29, & ! 9   
     &     94,   20,   12,   20,   12,   12,   12,   97,   51,   51, & ! O   
     &     12,  100,   12,   12,   12,   12,    5,    5,    5,    5, & ! 1   
     &     29,   12,   16,    5,   30,   30,    6,   50,  113,    6, & ! 2   
     &    114,    5,    5,    5,    5,    5,    5,    5,  113,    5, & ! 3   
     &      5,    5,    5,    5,    5,    5,  116,  117,  117,  117, & ! 4   
     &    115,  119,  121,  113,   12,   20,   89,   13,   12,   12, & ! 5   
     &     20,   51,   51,    5,  100,   29,    5,    5,    5,    5, & ! 6   
     &      5,    5,  113,  113,   43,   20,  110,   89,  111,   29, & ! 7   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,    6, & ! 8   
     &     12,   12,    6,   12,   12,   12,   12,   12,    5,    6, & ! 9   
     &      6,   12,    5,    6,    6,   12,   12,   16,   18,   20, & ! O   
     &     12,   12,   16,    6,   12,   12,  130,  131,   12,  100, & ! 1   
     &      6,    6,    6,    6,    6,   42,   42,   42,   38,   39, & ! 2   
     &     39,   39,   39,   39,   39,   39,   39,   39,   39,   39, & ! 3   
     &     39,   39,   39,   39,  105,   40,  113,   40,  113,   40, & ! 4   
     &    113,   40,  113,   40,   40,    5,    5,    5,   39,   39, & ! 5   
     &    107,  107,   39,   39,   39,   72,   39,   39,   42,   42, & ! 6   
     &     39,   39,   12,   12,   12,   12,   12,   12,   12,   12, & ! 7   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 8   
     &     12,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 9   
     &     12,   12,   12,    6,   12,   12,   12,   12,   12,   12, & ! O   
     &     13,   12,   12,   12,   12,   12,   12,   12,   12,   12, & ! 1   
     &     12,   12,   12,   16,   12,   12,   16,   12,   12,   12, & ! 2   
     &     12,   12,    5,   12,    6,   12,    5,   12,    5,   16, & ! 3   
     &     16,   16,   12,   12,  108,    6,   12,   12,   16,   16, & ! 4   
     &     12,   12,   16,   12,   16,   12,   16,   18,   12,   12, & ! 5   
     &     12,   12,   12,   12,   17,   12,   12,    6,   12,   12, & ! 6   
     &      6,   12,   12,   12,   12,   12,   12,   16,   18,   20, & ! 7   
     &     12,   12,   13,    6,   12,   12,  130,  131,   12,    6, & ! 8   
     &      6,   12,    6,    6,   12,   12,    6,    6,   18,    0, & ! 9   
     &    113,  113,  113,  113,  113,  113,  113,  113,  113,  113, & ! O   
     &    113,  113,    7,   12,    7,   12,    7,   12,    5,    5, & ! 1   
     &     12,   18,    5,    7,   12,    5,    1,    8,    5,    1, & ! 2   
     &      8,   34,   42,   39,   22,    5,    6,   27,  154,  155, & ! 3   
     &    156,  156,  156,  158,  155,  156,  155,  159,  159,  160, & ! 4   
     &    161,   10,    9,    0,  162,  164,  164,  164,  164,  164, & ! 5   
     &    164,  171,  171,  171,  171,  164,  164,  164,  164,  180, & ! 6   
     &      5,  182,    5,    5,    5,    5,    5,    5,    5,    5, & ! 7   
     &      5,    5,    5,    5/     !  8   

      DATA ( IRR( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    7,    0,    6,    7,    6,   12,    0, & ! O   
     &     14,   16,   18,   20,   14,   20,   24,   24,   18,   20, & ! 1   
     &     20,   12,   29,    0,   14,   14,   20,   20,   20,   12, & ! 2   
     &     12,   12,   12,   16,    6,    6,    8,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    5,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    6,    6,    0,    6,    0,    0,    8,    0,    0, & ! 6   
     &      6,    0,   46,    0,    0,    0,   51,   51,   55,    0, & ! 7   
     &      0,    0,   12,   65,   70,   75,   75,    0,   83,   86, & ! 8   
     &      0,    0,   14,    0,    0,    0,    0,    0,   33,   93, & ! 9   
     &      0,   95,   14,   14,   14,   14,   29,    0,   12,   12, & ! O   
     &     30,    0,   13,   15,   17,   17,   16,   50,   24,   28, & ! 1   
     &      0,    0,  109,   20,    8,    8,   27,    6,    0,   30, & ! 2   
     &     27,   12,   12,   12,   12,   82,   85,   24,    0,   12, & ! 3   
     &     12,   12,   12,   12,   12,   12,    0,  118,  118,  118, & ! 4   
     &      0,  120,  122,    0,   14,   10,   10,   29,   30,   14, & ! 5   
     &     14,   37,   37,   12,   10,    6,   12,   12,   12,   12, & ! 6   
     &     12,   12,    0,    0,    0,    6,    0,    6,    0,    6, & ! 7   
     &      6,    6,   16,   16,   18,    6,    6,    6,    6,   36, & ! 8   
     &      6,    6,   36,    6,    6,    6,    6,    6,    6,   23, & ! 9   
     &      5,    6,    6,   25,    5,    6,    6,    6,    6,    6, & ! O   
     &      6,   20,   20,   12,   20,    6,    6,    6,    6,    6, & ! 1   
     &     32,   12,   13,   32,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,   16,   18,   20,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,   13,   13,   16,   16,   18,   13,   13,   13, & ! 7   
     &     96,   13,   13,   13,   13,   13,   13,   13,   13,   13, & ! 8   
     &     13,    6,    6,   13,   13,   13,   13,   16,   16,   20, & ! 9   
     &     13,   13,   20,   12,   20,   14,  130,  131,   13,  100, & ! O   
     &     12,   13,    6,    6,   13,   16,   16,   16,   16,   16, & ! 1   
     &     16,   16,   16,   36,   16,   16,   36,   16,   16,   16, & ! 2   
     &     16,   16,   16,    6,   16,   16,   16,   25,   16,    0, & ! 3   
     &     18,   20,   16,   16,   16,   12,   16,   16,  130,  131, & ! 4   
     &     16,   16,   32,   16,    6,   16,    0,    0,   13,    6, & ! 5   
     &     16,   16,   18,    6,   13,   17,    6,   36,    6,    6, & ! 6   
     &     36,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 7   
     &      6,    6,   20,   12,   20,    6,    6,    6,    6,  100, & ! 8   
     &     32,    6,   13,   32,   42,    6,   13,    0,    0,    0, & ! 9   
     &    103,  103,  103,  103,  103,  103,  103,  103,  103,  103, & ! O   
     &    103,  103,  133,  134,  135,  136,  137,  138,  140,    0, & ! 1   
     &     55,   51,  144,  145,  146,    0,    0,    0,  147,  147, & ! 2   
     &    147,   12,    6,  151,   32,   17,   34,   17,    0,    0, & ! 3   
     &      6,    6,  157,    0,    0,    0,    6,    0,    0,    0, & ! 4   
     &      0,    0,   10,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  179, & ! 6   
     &      0,  181,    0,  184,  185,  185,  185,  185,  185,  190, & ! 7   
     &    190,  190,  190,  190/     !  8   

      DATA ( IRR( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    6,    0, & ! O   
     &      0,   14,   14,    0,    0,   14,   14,   14,   20,   14, & ! 1   
     &     13,   20,   14,    0,    0,    0,   14,   30,   30,   14, & ! 2   
     &     13,   17,   17,    0,   17,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,   10,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,   47,    0,    0,    0,    0,    0,   17,    0, & ! 7   
     &      0,    0,   14,   66,   71,   76,   76,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,   17,   26,   14,    0,   98,   98, & ! O   
     &     14,    0,    0,    0,   15,    0,   13,   30,   30,  106, & ! 1   
     &      0,    0,    0,   30,   13,   13,    0,    0,    0,   27, & ! 2   
     &     13,   14,   16,   16,   16,   83,   86,   13,    0,   20, & ! 3   
     &     20,   16,   89,   89,   14,   14,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,   10,    0,    0,   30,   14,   10, & ! 5   
     &     10,   98,   98,   14,    0,    0,  124,  125,   89,  127, & ! 6   
     &     89,  127,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     13,   15,   18,   18,   30,   13,   13,   15,   35,    0, & ! 8   
     &     35,   36,    0,   96,   33,   32,   13,   23,  113,  112, & ! 9   
     &    113,   25,  113,  112,  113,  112,  112,    0,    0,   13, & ! O   
     &     13,   30,    6,   13,   30,   14,    0,    0,   27,    0, & ! 1   
     &     12,   42,   17,   39,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,  108,  108,   13,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,  101,   15,   18,   18,   30,   17,   17,   17, & ! 7   
     &     13,   36,   32,   33,   36,   35,   33,   32,   28,   23, & ! 8   
     &    101,   13,  101,   25,  101,   25,  103,   13,   13,   13, & ! 9   
     &     34,   34,   14,   13,   30,   13,   13,   13,   27,   13, & ! O   
     &     32,   42,   13,   13,    0,   15,   18,   18,   18,   13, & ! 1   
     &     13,   17,   35,    0,   35,   33,    0,   35,   33,   32, & ! 2   
     &     13,   23,  108,   23,  108,   25,  108,   16,  108,    0, & ! 3   
     &      0,   13,   13,   34,   20,   13,   20,   14,    0,    0, & ! 4   
     &     27,  100,    0,   42,   13,    6,    0,    0,    6,   15, & ! 5   
     &     30,   18,   30,   13,   12,   22,   35,    0,   35,   33, & ! 6   
     &      0,   96,   33,   32,   13,   17,   60,    0,    0,   13, & ! 7   
     &     13,   34,   14,   13,   30,   14,    0,    0,   27,    0, & ! 8   
     &     12,   42,   17,   39,    0,   13,   17,    0,    0,    0, & ! 9   
     &     23,   25,   13,   23,   25,   13,   23,   25,   13,   23, & ! O   
     &     25,   13,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &     51,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,  150,  151,    5,   12,    0,   96,   12,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    5, & ! 6   
     &      0,    5,    0,    0,  186,  186,  186,  186,  186,  191, & ! 7   
     &    191,  191,  191,  191/     !  8   

      DATA ( IRR( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    8,    0, & ! O   
     &      0,    0,    0,    0,    0,   13,    0,    0,   14,    0, & ! 1   
     &      0,   29,   21,    0,    0,    0,    0,   14,   14,    0, & ! 2   
     &      0,    0,    0,    0,   22,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,   27,    0, & ! 7   
     &      0,    0,   32,   67,   72,   72,   72,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,   22,   32,   32,    0,   37,   37, & ! O   
     &     17,    0,    0,    0,    0,    0,    0,   17,   17,    0, & ! 1   
     &      0,    0,    0,   13,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,   13,   18,   18,   20,   23,   25,   22,    0,   14, & ! 3   
     &     30,   20,   30,   30,   32,   36,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   33,   13,    0, & ! 5   
     &      0,  123,  123,   32,    0,    0,  125,   35,  126,   35, & ! 6   
     &    127,   35,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   30,   30,    6,   17,   15,   17,   96,    0, & ! 8   
     &     42,   33,    0,   42,   34,   33,   28,  112,    0,    0, & ! 9   
     &      0,   13,    0,    0,    0,   13,   13,    0,    0,    0, & ! O   
     &     34,    6,   14,   27,    6,   13,    0,    0,   32,    0, & ! 1   
     &      0,    0,   22,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,   41,   41,   39,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,  101,   30,   30,   13,  101,   22,   22, & ! 7   
     &     35,    0,   35,   34,    0,    0,   34,   33,   31,   22, & ! 8   
     &    113,   23,  113,   22,  113,   22,  113,  108,  108,  101, & ! 9   
     &    101,  101,   13,   27,   13,   17,    0,    0,   32,    0, & ! O   
     &      0,    0,   17,   32,    0,  108,   30,   30,   30,   17, & ! 1   
     &     17,   22,   96,    0,    0,   34,    0,    0,   34,   33, & ! 2   
     &     28,  108,  113,   16,  113,   22,  113,  108,  113,    0, & ! 3   
     &      0,  108,   34,  108,   14,   27,   30,   13,    0,    0, & ! 4   
     &     32,    0,    0,    0,   17,   32,    0,    0,    0,    0, & ! 5   
     &     18,   30,    6,   17,    6,    6,   96,    0,    0,   34, & ! 6   
     &      0,    0,   34,   33,   28,   22,   13,    0,    0,    0, & ! 7   
     &     34,    0,    6,   27,    6,   13,    0,    0,   32,    0, & ! 8   
     &      0,    0,   22,    0,    0,   17,   22,    0,    0,    0, & ! 9   
     &      5,    5,   31,    5,    5,   31,    6,    6,   31,    6, & ! O   
     &      6,   31,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &     17,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   12,   12,   16,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,  187,  187,  187,  187,  187,  194, & ! 7   
     &    194,  194,  194,  194/     !  8   

      DATA ( IRR( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,   17,    0,    0,    0,    0, & ! 1   
     &      0,   30,    0,    0,    0,    0,    0,   32,   32,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,   51,    0, & ! 7   
     &      0,    0,   63,   68,   73,   77,   77,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,   32,   33,   33,    0,   99,   99, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,   22,   22,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,   63,   50,   50,   29,    4,    4,   12,    0,   33, & ! 3   
     &     14,   30,   14,   14,   34,   32,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,   10,   17,    0, & ! 5   
     &      0,   10,   10,    6,    0,    0,   35,   72,  127,   72, & ! 6   
     &    129,   72,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    6,    6,   17,    0,   17,   19,   32,    0, & ! 8   
     &      0,   34,    0,    0,   96,   34,   31,    0,    0,    0, & ! 9   
     &      0,   21,    0,    0,    0,   22,   22,    0,    0,    0, & ! O   
     &      0,   17,   13,    0,   13,   17,    0,    0,   39,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,  102,   13,   13,   17,  104,  101,  101, & ! 7   
     &     32,    0,    0,    0,    0,    0,   96,   34,  101,  101, & ! 8   
     &      0,   22,    0,  101,    0,  101,    0,    0,    0,  103, & ! 9   
     &    103,  103,  108,  101,   17,   22,    0,    0,   39,    0, & ! O   
     &      0,    0,   22,   39,    0,    0,   13,   13,   17,  108, & ! 1   
     &     22,  108,   32,    0,    0,    0,    0,    0,   96,   34, & ! 2   
     &     31,   22,    0,  108,    0,  108,    0,   22,    0,    0, & ! 3   
     &      0,    0,  108,    0,   13,   16,   13,   17,    0,    0, & ! 4   
     &     39,    0,    0,    0,   22,   39,    0,    0,    0,    0, & ! 5   
     &      6,    6,   17,    0,   26,   19,   32,    0,    0,   36, & ! 6   
     &      0,    0,   96,   34,   31,    0,   28,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,   13,   17,    0,    0,   39,    0, & ! 8   
     &      0,    0,    0,    0,    0,   22,   42,    0,    0,    0, & ! 9   
     &     12,   12,    5,   12,   12,    5,   12,   12,    6,   12, & ! O   
     &     12,    6,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &     27,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   16,   16,   14,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,  188,  188,  188,  188,  190,  192, & ! 7   
     &    192,  192,  192,  192/     !  8   

      DATA ( IRR( IRXXN,  9 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,   22,    0,    0,    0,    0, & ! 1   
     &      0,   14,    0,    0,    0,    0,    0,   33,   33,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,   33,   39,   39,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   53,   20,    4,   14,   14,   14,    0,   63, & ! 3   
     &     13,   14,   13,   13,   63,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    6,   42,    0, & ! 5   
     &      0,    0,    0,   10,    0,    0,   72,    0,   35,    0, & ! 6   
     &     35,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   15,   13,   22,    0,   19,   22,   42,    0, & ! 8   
     &      0,   42,    0,    0,   42,   35,  115,    0,    0,    0, & ! 9   
     &      0,  112,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,   33,    0,    0,   17,   32,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,   17,   17,   22,    0,  103,  103, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,   35,  103,  103, & ! 8   
     &      0,  101,    0,  103,    0,  103,    0,    0,    0,  108, & ! 9   
     &      0,    0,   16,  103,   33,   32,    0,    0,    0,    0, & ! O   
     &      0,    0,  101,   42,    0,    0,   17,   17,   22,    0, & ! 1   
     &    108,    0,    0,    0,    0,    0,    0,    0,    0,   35, & ! 2   
     &    108,    0,    0,   22,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,  108,   17,   22,    0,    0, & ! 4   
     &      0,    0,    0,    0,  108,   42,    0,    0,    0,    0, & ! 5   
     &     15,   13,   22,    0,   15,   15,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   35,   32,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,   17,   22,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   42,    0,    0,    0,    0, & ! 9   
     &    132,  132,   12,  132,  132,   12,  132,  132,   12,  132, & ! O   
     &    132,   12,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,   51,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,  189,  190,  190,  190,  191,  193, & ! 7   
     &    193,  193,  193,  193/     !  8   

      DATA ( IRR( IRXXN, 10 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,   13,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
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
     &      0,    0,    4,   14,   14,    0,    0,    0,    0,    0, & ! 3   
     &     33,   13,   32,   32,  108,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,   66,    0,   72,    0, & ! 6   
     &     72,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   17,   15,   42,    0,   26,   27,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   96,   32,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,   33,   22,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,   22,   22,  101,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,   96,   17,    0, & ! 8   
     &      0,  103,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,  101,    0,  101,   33,    0,    0,    0,    0, & ! O   
     &      0,    0,  103,    0,    0,    0,   22,   22,  108,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   96, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,   33,   32,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     17,   17,    0,    0,   19,   27,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   96,   27,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,   33,   32,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,  132,    0,    0,  132,    0,    0,  132,    0, & ! O   
     &      0,  132,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,  190,  191,  191,  191,    0,    0, & ! 7   
     &      0,    0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   14,    4,   13,    0,    0,    0,    0,    0, & ! 3   
     &     63,   15,   33,   33,   41,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,   66,    0, & ! 6   
     &     66,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   19,   17,    0,    0,   42,   42,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   42,   27,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,   33,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,   32,  101,  103,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,   32,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,  103,    0,  103,  101,    0,    0,    0,    0, & ! O   
     &      0,    0,   42,    0,    0,    0,   32,  108,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,  108,   33,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     26,   22,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,   17,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,   33,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,  191,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0/     !  8   

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
     &      0,    0,   48,   48,   28,    0,    0,    0,    0,    0, & ! 3   
     &    108,   22,   39,   34,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   26,   26,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,   17,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,  101,  103,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,   27,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,  108,  103,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,  108,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,  108,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     19,   26,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
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
     &      0,    0,    0,    0,  192,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0/     !  8   

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
     &      0,    0,   49,   49,   31,    0,    0,    0,    0,    0, & ! 3   
     &     17,   32,    0,   39,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   42,   19,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,  103,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,   19,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
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
     &      0,    0,    0,    0,  193,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0/     !  8   

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
     &      0,    0,   52,   13,   63,    0,    0,    0,    0,    0, & ! 3   
     &      0,   33,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,   22,    0,    0,    0,    0,    0,    0, & ! 8   
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
     &      0,   15,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
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
     &      0,    0,    0,    0/     !  8   

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
     &      0,    0,   64,   15,   58,    0,    0,    0,    0,    0, & ! 3   
     &      0,   63,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,   27,    0,    0,    0,    0,    0,    0, & ! 8   
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
     &      0,   27,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
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
     &      0,    0,    0,    0/     !  8   

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
     &      0,    0,   13,   17,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,   42,    0,    0,    0,    0,    0,    0, & ! 8   
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
     &      0,    0,    0,    0/     !  8   

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
     &      0,    0,   15,   19,    0,    0,    0,    0,    0,    0, & ! 3   
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
     &      0,    0,    0,    0/     !  8   

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
     &      0,    0,   17,   22,    0,    0,    0,    0,    0,    0, & ! 3   
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
     &      0,    0,    0,    0/     !  8   

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
     &      0,    0,   19,   27,    0,    0,    0,    0,    0,    0, & ! 3   
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
     &      0,    0,    0,    0/     !  8   

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
     &      0,    0,   36,  108,    0,    0,    0,    0,    0,    0, & ! 3   
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
     &      0,    0,    0,    0/     !  8   

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
     &      0,    0,   26,    0,    0,    0,    0,    0,    0,    0, & ! 3   
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
     &      0,    0,    0,    0/     !  8   

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
     &      0,    0,   27,    0,    0,    0,    0,    0,    0,    0, & ! 3   
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
     &      0,    0,    0,    0/     !  8   

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
     &      0,    0,   63,    0,    0,    0,    0,    0,    0,    0, & ! 3   
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
     &      0,    0,    0,    0/     !  8   

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
     &      0,    0,  108,    0,    0,    0,    0,    0,    0,    0, & ! 3   
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
     &      0,    0,    0,    0/     !  8   

      DATA ( RTDAT( 1,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.7000D-12, 1.0000D-14, 3.0000D-12, & ! +   
     &     1.2000D-13, 6.1000D-34, 8.0000D-12, 3.3000D-11, 2.1500D-11, & ! 4   
     &     1.6300D-10, 2.8000D-12, 4.8000D-11, 3.0000D-13, 4.2000D-34, & ! +   
     &     1.8000D-12, 9.1000D-32, 7.1000D-31, 3.4400D-12, 6.0950D-14, & ! 5   
     &     4.2500D-39, 3.0000D-12, 5.3000D-12, 3.4000D-31, 1.8000D-30, & ! +   
     &     2.4000D-14, 2.0000D-11, 3.5000D-12, 1.7000D-11, 4.3500D-14, & ! 6   
     &     8.5000D-13, 2.4000D-30, 5.8000D-27, 1.0000D-22, 1.9000D-31, & ! +   
     &     2.1000D-27, 4.5000D-13, 2.9000D-31, 1.4400D-13, 2.4500D-12, & ! 7   
     &     7.6600D-12, 7.6800D-12, 1.0100D-11, 2.8200D-11, 1.0000D-28, & ! +   
     &     5.7200D-12, 1.3300D-11, 5.5000D-30, 2.3300D-12, 1.8100D-12, & ! 8   
     &     2.3100D-11, 7.1600D-12, 2.7000D-11, 1.2100D-11, 4.2000D-12, & ! +   
     &     5.2000D-11, 1.0000D-10, 5.5000D-12, 4.7000D-12, 4.9000D-12, & ! 9   
     &     4.5600D-14, 1.5000D-12, 2.8000D-12, 3.0000D-12, 8.0000D-12, & ! +   
     &     2.6000D-12, 5.7700D-12, 1.1000D-11, 9.2600D-13, 2.8000D-11, & ! O   
     &     2.8000D-11, 1.0000D-11, 5.3200D-12, 6.7500D-12, 4.6500D-11, & ! +   
     &     2.8000D-11, 2.0500D-10, 2.8500D-12, 3.0000D-12, 2.6000D-12, & ! 1   
     &     1.4700D-11, 2.9000D-12, 3.4000D-12, 3.4000D-12, 1.0000D-10, & ! +   
     &     3.0000D-11, 4.5000D-13, 4.0000D-14, 2.9300D-12, 4.0000D-14, & ! 2   
     &     4.0000D-14, 3.2000D-11, 5.3100D-12, 4.8000D-12, 5.6000D-12, & ! +   
     &     1.3000D-11, 9.1400D-15, 4.3300D-15, 4.4000D-15, 7.8600D-15, & ! 3   
     &     5.0000D-16, 2.9500D-15, 8.3000D-18, 1.6700D-16, 1.3600D-15, & ! +   
     &     8.5000D-16, 1.6600D-18, 2.0000D-16, 2.0000D-16, 9.0000D-17, & ! 4   
     &     5.0000D-16, 2.8600D-13, 4.3920D-13, 1.7900D-13, 8.6400D-13, & ! +   
     &     3.0300D-12, 1.1900D-12, 1.2200D-11, 3.1500D-14, 2.0000D-12, & ! 5   
     &     1.4000D-12, 3.7600D-12, 3.4000D-15, 5.0200D-13, 2.9000D-12, & ! +   
     &     3.7600D-12, 3.7800D-12, 1.0600D-12, 2.8700D-13, 2.0100D-10, & ! 6   
     &     2.2000D-14, 1.0000D+03, 1.0000D+03, 1.0000D+03, 1.0000D+03, & ! +   
     &     1.0000D+03, 1.0000D+03, 1.0000D+00, 1.0000D+00, 9.7000D-29, & ! 7   
     &     9.0000D-29, 9.7000D-29, 9.0000D-29, 2.8000D-12, 1.6000D+16, & ! +   
     &     2.8000D-12, 2.6000D-12, 4.0000D-12, 4.0000D-12, 4.0000D-12, & ! 8   
     &     9.0000D-12, 4.0000D-12, 4.0000D-12, 2.5400D-12, 4.0000D-12, & ! +   
     &     2.7000D-12, 2.7000D-12, 4.0000D-12, 2.7000D-12, 2.7000D-12, & ! 9   
     &     2.7000D-12, 2.4300D-12, 4.0000D-12, 4.0000D-12, 4.0000D-12, & ! +   
     &     4.0000D-12, 4.0000D-12, 4.0000D-12, 4.0000D-12, 4.0000D-12, & ! O   
     &     2.7000D-12, 2.7000D-12, 8.1000D-12, 8.1000D-12, 2.9000D-12, & ! +   
     &     4.0000D-12, 4.0000D-12, 2.5400D-12, 2.5400D-12, 2.5400D-12, & ! 1   
     &     2.5400D-12, 4.0000D-12, 4.0000D-12, 2.7000D-12, 2.7000D-12, & ! +   
     &     4.0000D-12, 4.0000D-12, 4.0000D-12, 2.7000D-12, 4.0000D-12, & ! 2   
     &     2.0000D-11, 2.0000D-11, 2.0800D-12, 4.1000D-13, 7.5000D-13, & ! +   
     &     1.6600D-13, 1.6600D-13, 1.6600D-13, 1.9000D-13, 1.6600D-13, & ! 3   
     &     1.6600D-13, 2.9100D-13, 3.7500D-13, 3.7500D-13, 3.7500D-13, & ! +   
     &     3.7500D-13, 3.7500D-13, 3.7500D-13, 3.7500D-13, 2.0500D-13, & ! 4   
     &     1.5000D-11, 1.5000D-11, 1.5000D-11, 1.5000D-11, 1.5000D-11, & ! +   
     &     1.5000D-11, 1.5000D-11, 1.5000D-11, 2.9100D-13, 2.9100D-13, & ! 5   
     &     4.3000D-13, 4.3000D-13, 1.1500D-13, 1.1500D-13, 1.1500D-13, & ! +   
     &     1.8200D-13, 1.8200D-13, 2.9100D-13, 2.9100D-13, 3.7500D-13, & ! 6   
     &     1.0000D-11, 3.7500D-13, 1.1500D-13, 1.6600D-13, 1.6600D-13, & ! +   
     &     3.7500D-13, 1.6600D-13, 9.5000D-14, 1.1800D-13, 9.4600D-14, & ! 7   
     &     1.0000D-13, 4.3400D-14, 1.7100D-13, 1.4600D-13, 9.1800D-14, & ! +   
     &     3.5600D-14, 3.5600D-14, 3.5600D-14, 3.5600D-14, 3.5600D-14, & ! 8   
     &     3.5600D-14, 3.5600D-14, 3.5600D-14, 3.4000D-14, 3.5600D-14, & ! +   
     &     1.0000D-10, 3.5600D-14, 1.0000D-10, 3.5600D-14, 1.0000D-10, & ! 9   
     &     3.5600D-14, 1.0000D-10, 2.0000D-11, 2.0000D-11, 7.5000D-13, & ! +   
     &     6.9100D-13, 6.9100D-13, 3.4000D-14, 3.4000D-14, 8.3700D-14, & ! O   
     &     3.4000D-14, 3.5600D-14, 3.5600D-14, 3.5600D-14, 3.5600D-14, & ! +   
     &     7.5000D-13, 1.6000D-13, 9.6800D-14, 3.5600D-14, 5.9900D-15, & ! 1   
     &     1.0300D-12, 6.9000D-13, 5.5900D-13, 2.4700D-13, 9.4800D-13, & ! +   
     &     8.1100D-13, 5.0900D-13, 7.4000D-13, 7.4000D-13, 7.4000D-13, & ! 2   
     &     7.4000D-13, 7.4000D-13, 7.4000D-13, 7.4000D-13, 7.4000D-13, & ! +   
     &     8.4000D-14, 7.4000D-13, 1.0000D-10, 7.4000D-13, 1.0000D-10, & ! 3   
     &     7.4000D-13, 1.0000D-10, 7.4000D-13, 1.0000D-10, 2.5000D-12, & ! +   
     &     2.5000D-12, 7.5100D-13, 7.5100D-13, 7.5100D-13, 8.4000D-14, & ! 4   
     &     8.4000D-14, 1.6800D-12, 1.6800D-12, 7.4000D-13, 7.4000D-13, & ! +   
     &     7.4000D-13, 7.4000D-13, 7.5100D-13, 8.8500D-13, 5.3700D-13, & ! 5   
     &     7.4000D-13, 3.4000D-14, 2.5000D-12, 1.2000D-12, 1.2000D-12, & ! +   
     &     1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, & ! 6   
     &     1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, & ! +   
     &     1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, & ! 7   
     &     1.2000D-12, 1.2000D-12, 4.0000D-12, 4.0000D-12, 1.2000D-12, & ! +   
     &     1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 2.5000D-12, & ! 8   
     &     2.5000D-12, 2.5000D-12, 2.5000D-12, 1.2000D-12, 1.2000D-12, & ! +   
     &     1.2000D-12, 1.2000D-12, 1.2000D-12, 1.2000D-12, 7.0000D-14, & ! 9   
     &     4.2500D-14, 2.9600D-14, 1.2000D-12, 2.5000D-12, 7.1300D-17, & ! +   
     &     1.0000D-10, 1.0000D-10, 1.0000D-10, 1.0000D-10, 1.0000D-10, & ! O   
     &     1.0000D-10, 1.0000D-10, 1.0000D-10, 1.0000D-10, 1.0000D-10, & ! +   
     &     1.0000D-10, 1.0000D-10, 2.7000D-12, 1.9000D-13, 2.7000D-12, & ! 1   
     &     1.9000D-13, 2.7000D-12, 1.9000D-13, 2.7000D-12, 5.7800D-11, & ! +   
     &     2.8200D-11, 6.5500D-14, 2.3100D-11, 2.7000D-12, 1.9000D-13, & ! 2   
     &     2.0000D-11, 2.6100D-19, 1.1500D-15, 1.4800D-11, 1.3400D-14, & ! +   
     &     1.7900D-13, 5.0100D-11, 2.7000D-12, 3.7500D-13, 4.4000D-11, & ! 3   
     &     3.4300D-17, 8.9900D-12, 1.2000D-11, 1.9000D-11, 2.8400D-13, & ! +   
     &     2.7000D-12, 2.3000D-12, 1.2000D-14, 1.9700D-10, 2.8400D-13, & ! 4   
     &     2.3000D-12, 2.7000D-12, 1.0000D+00, 1.0000D+00, 6.5000D-07, & ! +   
     &     1.3000D-06, 1.0000D+00, 1.0000D+00, 6.7006D-11, 1.0000D+00, & ! 5   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 6   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 2.5000D-12, & ! +   
     &     1.0000D+00, 2.5000D-12, 1.0000D+00, 1.2500D-11, 4.0000D-11, & ! 7   
     &     4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11, & ! +   
     &     4.0000D-11, 4.0000D-11, 4.0000D-11, 4.0000D-11/           !8   

      DATA ( RTDAT( 2,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-2.4000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 4.6000D+02, 2.6600D+03, & ! +   
     &     0.0000D+00,-1.5000D+00,-2.6000D+00, 0.0000D+00, 2.7000D+02, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.6000D+00,-3.0000D+00, & ! +   
     &     4.6000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00,-3.0000D+00, 1.0840D+04, 0.0000D+00,-3.4000D+00, & ! +   
     &     1.0900D+04, 0.0000D+00,-4.1000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-4.5000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     3.6500D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 2.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-5.6000D+00, & ! 7   
     &     1.4000D+04,-5.6000D+00, 1.4000D+04, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.4153D-08, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

      DATA ( RTDAT( 3,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00,-9.4000D+02,-4.9000D+02,-1.5000D+03, & ! +   
     &    -2.4500D+03, 0.0000D+00,-2.0600D+03, 5.5000D+01, 1.1000D+02, & ! 4   
     &     6.0000D+01,-1.8000D+03, 2.5000D+02, 2.1000D-33, 2.9400D-54, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.6000D+02, 6.8570D-34, & ! 5   
     &     6.6350D+02, 2.5000D+02, 2.0000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.7000D-17, 0.0000D+00, 0.0000D+00, 1.2500D+02,-1.3350D+03, & ! 6   
     &    -2.4500D+03, 0.0000D+00, 6.7000D+01, 0.0000D+00, 0.0000D+00, & ! +   
     &     7.0000D+01, 6.1000D+02, 0.0000D+00, 2.7400D-33,-1.7750D+03, & ! 7   
     &    -1.0200D+03,-3.7000D+02,-2.4500D+02,-2.7300D+02, 0.0000D+00, & ! +   
     &     5.0000D+02, 5.0000D+02, 0.0000D+00,-1.9300D+02, 3.5400D+02, & ! 8   
     &     0.0000D+00, 0.0000D+00, 3.9000D+02, 4.4000D+02, 4.0100D+02, & ! +   
     &     6.0000D+02, 0.0000D+00, 1.2500D+02, 3.4500D+02, 4.0500D+02, & ! 9   
     &    -4.2700D+02,-9.0000D+01, 1.0000D+01, 0.0000D+00, 3.8000D+02, & ! +   
     &     6.1000D+02, 5.3300D+02, 0.0000D+00, 8.3000D+02, 1.7500D+02, & ! O   
     &     1.7500D+02, 0.0000D+00, 2.4300D+02, 4.0500D+02, 0.0000D+00, & ! +   
     &     1.7500D+02, 0.0000D+00,-3.4500D+02, 2.0000D+01, 2.0000D+02, & ! 1   
     &     0.0000D+00, 1.9000D+02, 1.9000D+02, 1.9000D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 8.5000D+02, 1.9000D+02, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00,-2.6000D+02, 0.0000D+00, 2.7000D+02, & ! +   
     &     0.0000D+00,-2.5800D+03,-1.8000D+03,-8.4500D+02,-1.9130D+03, & ! 3   
     &    -5.3000D+02,-7.8300D+02, 0.0000D+00, 0.0000D+00,-2.1120D+03, & ! +   
     &    -1.5200D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00,-2.2820D+03,-4.5000D+02, 4.5000D+02, & ! +   
     &    -4.4600D+02, 4.9000D+02, 0.0000D+00,-4.4800D+02,-2.4400D+03, & ! 5   
     &    -1.9000D+03,-1.9000D+03, 0.0000D+00,-1.0760D+03,-1.9000D+03, & ! +   
     &    -1.9000D+03, 0.0000D+00, 0.0000D+00,-1.0000D+03, 0.0000D+00, & ! 6   
     &    -5.0000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     1.7500D+02, 0.0000D+00, 1.7700D+02, 1.8100D+02,-1.3486D+04, & ! +   
     &     3.0000D+02, 3.6500D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.6000D+02, 0.0000D+00, & ! +   
     &     3.6000D+02, 3.6000D+02, 0.0000D+00, 3.6000D+02, 3.6000D+02, & ! 9   
     &     3.6000D+02, 3.6000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     3.6000D+02, 3.6000D+02, 2.7000D+02, 2.7000D+02, 3.0000D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.6000D+02, 3.6000D+02, 3.6000D+02, & ! 1   
     &     3.6000D+02, 0.0000D+00, 0.0000D+00, 3.6000D+02, 3.6000D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.6000D+02, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 7.5000D+02, 7.0000D+02, & ! +   
     &     1.3000D+03, 1.3000D+03, 1.3000D+03, 1.3000D+03, 1.3000D+03, & ! 3   
     &     1.3000D+03, 1.3000D+03, 9.8000D+02, 9.8000D+02, 9.8000D+02, & ! +   
     &     9.8000D+02, 9.8000D+02, 9.8000D+02, 9.8000D+02, 1.3000D+03, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.3000D+03, 1.3000D+03, & ! 5   
     &     1.0400D+03, 1.0400D+03, 1.3000D+03, 1.3000D+03, 1.3000D+03, & ! +   
     &     1.3000D+03, 1.3000D+03, 1.3000D+03, 1.3000D+03, 9.8000D+02, & ! 6   
     &     0.0000D+00, 9.8000D+02, 1.3000D+03, 1.3000D+03, 1.3000D+03, & ! +   
     &     9.8000D+02, 1.3000D+03, 3.9000D+02, 1.5800D+02, 4.3100D+02, & ! 7   
     &     4.6700D+02, 6.3300D+02, 7.0800D+02, 7.0800D+02, 7.0800D+02, & ! +   
     &     7.0800D+02, 7.0800D+02, 7.0800D+02, 7.0800D+02, 7.0800D+02, & ! 8   
     &     7.0800D+02, 7.0800D+02, 7.0800D+02, 2.2100D+02, 7.0800D+02, & ! +   
     &     0.0000D+00, 7.0800D+02, 0.0000D+00, 7.0800D+02, 0.0000D+00, & ! 9   
     &     7.0800D+02, 0.0000D+00, 5.0000D+02, 5.0000D+02, 5.0000D+02, & ! +   
     &     5.0800D+02, 5.0800D+02, 2.2100D+02, 2.2100D+02, 0.0000D+00, & ! O   
     &     2.2100D+02, 7.0800D+02, 7.0800D+02, 7.0800D+02, 7.0800D+02, & ! +   
     &     5.0000D+02, 7.0800D+02, 7.0800D+02, 0.0000D+00, 1.5100D+03, & ! 1   
     &     2.1100D+02, 4.6000D+02, 5.2200D+02, 6.8300D+02, 7.6500D+02, & ! +   
     &     7.6500D+02, 7.6500D+02, 7.6500D+02, 7.6500D+02, 7.6500D+02, & ! 2   
     &     7.6500D+02, 7.6500D+02, 7.6500D+02, 7.6500D+02, 7.6500D+02, & ! +   
     &     2.2100D+02, 7.6500D+02, 0.0000D+00, 7.6500D+02, 0.0000D+00, & ! 3   
     &     7.6500D+02, 0.0000D+00, 7.6500D+02, 0.0000D+00, 5.0000D+02, & ! +   
     &     5.0000D+02, 5.6500D+02, 5.6500D+02, 5.6500D+02, 2.2100D+02, & ! 4   
     &     2.2100D+02, 5.0000D+02, 5.0000D+02, 7.6500D+02, 7.6500D+02, & ! +   
     &     7.0800D+02, 7.0800D+02, 5.6500D+02, 7.6500D+02, 7.6500D+02, & ! 5   
     &     7.0800D+02, 1.5600D+03, 5.0000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+03, & ! 9   
     &     1.0000D+03, 1.0000D+03, 0.0000D+00, 5.0000D+02, 2.9500D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.6000D+02, 1.3000D+03, 3.6000D+02, & ! 1   
     &     1.3000D+03, 3.6000D+02, 1.3000D+03, 3.7400D+02,-4.0000D+02, & ! +   
     &    -2.7300D+02, 0.0000D+00, 0.0000D+00, 3.6000D+02, 1.3000D+03, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 4.4800D+02,-2.2830D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.6000D+02, 9.8000D+02, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.3000D+03, & ! +   
     &     3.6000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.3000D+03, & ! 4   
     &     0.0000D+00, 3.6000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D-06, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     49,   50,   52,   53,   55,   59,   60,   61,   67,   70, & 
     &     73,   74,   80,   83,  175,  177,  454/

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
     &     1.2200D+00, 1.0000D+00, 1.0000D+00, 1.0000D-01, 1.5000D+00, & ! +   
     &     1.0000D+00, 3.4000D-01, 3.0000D-01, 2.0000D+00, 1.0000D+00, & ! 2   
     &     2.0000D+00, 1.0000D+00, 1.5000D+00, 1.5000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     2.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 7.0000D-01, 2.0000D+00, 1.0000D+00, & ! 6   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 4.9000D-02, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 6.5000D-01, 6.4800D-01, 1.7700D-01, & ! 8   
     &     1.7700D-01, 1.7700D-01, 1.0000D+00, 9.5000D-01, 8.7000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.7000D-01, & ! +   
     &     1.0000D+00, 3.1300D-01, 1.0000D+00, 1.0000D+00, 5.2000D-01, & ! O   
     &     5.2000D-01, 5.6000D-01, 1.0000D+00, 1.5200D-01, 2.0000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 3.5000D-01, 1.0000D-02, 1.0000D-02, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 6.4000D-01, 3.5000D-01, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 8.0000D-02, 2.2000D-01, 4.6000D-01, 2.5000D-01, & ! 3   
     &     8.5000D-01, 8.5000D-01, 4.0000D-02, 1.0000D+00, 1.9000D-01, & ! +   
     &     1.6000D-01, 1.0000D-01, 5.0000D-02, 5.0000D-02, 5.0000D-02, & ! 4   
     &     5.0000D-02, 1.0000D+00, 8.0000D-01, 4.3000D-01, 1.1000D-01, & ! +   
     &     1.0000D+00, 9.5000D-01, 9.5000D-01, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 6.8000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.5200D-01, 2.0000D-01, 5.0000D-01, 1.0000D+00, & ! 6   
     &     1.0000D+00, 2.8000D-01, 4.9000D-01, 1.5800D-01, 3.9000D-01, & ! +   
     &     1.5800D-01, 3.9000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 6.6000D-01, 2.0000D-01, 6.0600D-01, & ! 8   
     &     1.0000D+00, 7.8000D-01, 8.3000D-01, 9.1800D-01, 1.0000D+00, & ! +   
     &     9.5000D-01, 5.0000D-01, 1.0000D+00, 9.5000D-01, 9.5000D-01, & ! 9   
     &     9.5000D-01, 8.8000D-01, 8.2000D-01, 8.2000D-01, 1.6400D+00, & ! +   
     &     8.2000D-01, 7.7000D-01, 7.7000D-01, 1.4400D+00, 7.7000D-01, & ! O   
     &     9.5000D-01, 9.4000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     6.7000D-01, 7.7000D-01, 6.5000D-01, 1.0000D+00, 3.0000D-01, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 2.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     4.4000D-01, 4.4000D-01, 1.5000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 7.4000D-01, 1.0000D+00, 8.9400D-01, & ! 7   
     &     8.4200D-01, 9.1000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.6000D+00, 1.0000D+00, 2.0000D+00, 2.0000D+00, 1.0000D+00, & ! 8   
     &     2.0000D+00, 2.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 5.0000D-01, 7.5000D-01, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 9.0000D-01, 9.0000D-01, 5.0000D-01, & ! +   
     &     8.3400D-01, 1.0000D+00, 5.0000D-01, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 2.0000D+00, 5.0000D-01, 1.0000D+00, 1.0000D+00, & ! 1   
     &     5.0000D-01, 3.9400D-01, 3.4200D-01, 3.0300D-01, 5.0000D-01, & ! +   
     &     5.0000D-01, 5.0000D-01, 6.0000D-01, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     5.0000D-01, 5.0000D-01, 5.0000D-01, 5.0000D-01, 5.0000D-01, & ! 3   
     &     5.0000D-01, 5.0000D-01, 5.0000D-01, 5.0000D-01, 2.0000D+00, & ! +   
     &     1.0000D+00, 5.0000D-01, 3.3000D-01, 5.0000D-01, 6.3500D-01, & ! 4   
     &     1.0000D+00, 5.0000D-01, 5.0000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     2.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.0000D-01, & ! 5   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     2.5400D-01, 4.8800D-01, 8.2000D-01, 1.0000D+00, 4.7000D-01, & ! 6   
     &     8.6000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.0000D-01, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     6.7000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 3.0000D-01, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0000D+00, 2.0000D+00, 1.0000D+00, & ! 9   
     &     5.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     9.6000D-01, 9.6000D-01, 9.6000D-01, 9.6000D-01, 9.6000D-01, & ! O   
     &     9.6000D-01, 9.6000D-01, 9.6000D-01, 9.6000D-01, 9.6000D-01, & ! +   
     &     9.6000D-01, 9.6000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     4.9000D-02, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 4.9000D-01, 8.0000D-02, 6.0000D-01, 6.5000D-01, & ! 3   
     &     2.0000D-02, 1.0000D+00, 6.1300D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 9.8200D-01, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 2.0000D+00, 5.0000D-01, 0.0000D+00, 1.0000D+00, & ! 5   
     &     8.5710D-01, 1.1429D+00, 8.5710D-01, 1.1429D+00, 7.1430D-01, & ! +   
     &     7.1430D-01, 8.0000D-01, 9.0000D-01, 5.0000D-01, 5.0000D-01, & ! 6   
     &     1.4286D+00, 1.4286D+00, 1.7143D+00, 1.7143D+00, 1.2500D+00, & ! +   
     &     1.0000D+00, 1.2500D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00/           !8   

      DATA ( SC( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 8.0000D-01, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     7.8400D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 5.0000D-01, & ! +   
     &     1.0000D+00, 6.6000D-01, 3.0000D-01, 0.0000D+00, 1.0000D+00, & ! 2   
     &     2.0000D+00, 1.0000D+00, 2.5000D-01, 2.5000D-01, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 7.0000D-01, 0.0000D+00, 1.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 3.3100D-05, 1.5800D-03, 9.5100D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.5000D-01, 3.5200D-01, 7.6300D-01, & ! 8   
     &     7.6300D-01, 7.6300D-01, 0.0000D+00, 5.0000D-02, 1.3000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 4.3000D-01, & ! +   
     &     0.0000D+00, 6.8700D-01, 2.0000D+00, 1.0000D+00, 3.3000D-01, & ! O   
     &     3.3000D-01, 2.1000D-01, 0.0000D+00, 6.1900D-01, 5.8400D-01, & ! +   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 7.1900D-01, & ! 1   
     &     1.0000D+00, 6.5000D-01, 4.4000D-01, 4.4000D-01, 1.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.6000D-01, 6.5000D-01, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     7.0000D-02, 1.5000D-01, 3.2000D-01, 7.0000D-02, 2.5000D-01, & ! 3   
     &     8.5000D-01, 8.5000D-01, 6.7000D-01, 0.0000D+00, 1.4000D-01, & ! +   
     &     1.1000D-01, 7.2000D-02, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 4   
     &     1.5000D+00, 0.0000D+00, 2.0000D-01, 5.7000D-01, 8.9000D-01, & ! +   
     &     0.0000D+00, 5.0000D-02, 5.0000D-02, 0.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 3.2000D-01, 1.0000D+00, 2.0000D+00, & ! +   
     &     1.0000D+00, 3.3900D-01, 3.2000D-01, 1.5000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 2.9000D-01, 1.0000D-02, 3.0800D-01, 1.0000D-02, & ! +   
     &     3.0800D-01, 1.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.3100D-01, 5.1000D-02, 1.3300D-01, & ! 8   
     &     1.0000D+00, 9.7000D-01, 9.5000D-01, 9.1800D-01, 1.0000D+00, & ! +   
     &     9.5000D-01, 9.5000D-01, 1.0000D+00, 9.5000D-01, 9.5000D-01, & ! 9   
     &     9.5000D-01, 8.8000D-01, 8.2000D-01, 8.2000D-01, 8.2000D-01, & ! +   
     &     8.2000D-01, 7.7000D-01, 7.7000D-01, 7.7000D-01, 7.7000D-01, & ! O   
     &     9.5000D-01, 9.4000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 2.3000D-01, 3.5000D-01, 5.0000D-01, 7.0000D-01, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.8700D-01, 1.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     4.4000D-01, 4.4000D-01, 1.5000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.3700D+00, 7.5000D-01, 8.0000D-02, & ! 7   
     &     1.8000D-02, 9.0000D-02, 1.9500D+00, 1.5000D+00, 7.5000D-01, & ! +   
     &     4.5900D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.3100D+00, 7.5000D-01, & ! +   
     &     7.5000D-01, 5.0000D-01, 7.5000D-01, 7.5000D-01, 7.5000D-01, & ! 9   
     &     7.5000D-01, 7.5000D-01, 9.0000D-01, 9.0000D-01, 5.0000D-01, & ! +   
     &     1.0000D+00, 7.5000D-01, 2.6900D-01, 1.0000D+00, 1.1600D+00, & ! O   
     &     3.0500D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 5.0000D-01, 7.0000D-01, 1.0000D+00, & ! 1   
     &     5.0000D-01, 5.8000D-01, 5.1800D-01, 5.0000D-01, 5.0000D-01, & ! +   
     &     5.0000D-01, 5.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     5.0000D-01, 5.0000D-01, 5.0000D-01, 5.0000D-01, 5.0000D-01, & ! 3   
     &     5.0000D-01, 5.0000D-01, 5.0000D-01, 5.0000D-01, 0.0000D+00, & ! +   
     &     1.0000D+00, 5.0000D-01, 5.0000D-01, 5.0000D-01, 5.0000D-01, & ! 4   
     &     5.0000D-01, 5.0000D-01, 5.0000D-01, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.4000D-01, 5.5000D-02, 1.8000D-01, 1.0000D+00, 7.9000D-01, & ! 6   
     &     7.2000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 5.3800D-01, 1.0000D+00, 7.0000D-01, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.8700D-01, 1.0000D+00, 2.0000D+00, & ! 9   
     &     5.0000D-01, 5.0400D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, & ! O   
     &     4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, & ! +   
     &     4.8000D-01, 4.8000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 4.7000D-01, 0.0000D+00, & ! +   
     &     9.5100D-01, 1.0100D-03, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.8000D-01, 5.2000D-01, & ! +   
     &     4.5000D-02, 4.9000D-01, 9.2000D-01, 4.0000D-01, 3.1000D-01, & ! 3   
     &     1.0000D+00, 8.0000D-01, 3.8700D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.0000D+00, 2.0000D+00, 1.8000D-02, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 7.5300D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.0000D+00, 4.8570D-01, & ! 7   
     &     3.0030D-01, 3.8560D-01, 2.1810D-01, 2.4120D-01, 6.6640D-01, & ! +   
     &     2.8580D-01, 3.3030D-01, 3.4440D-01, 3.8860D-01/           !8   

      DATA ( SC( IRXXN,  3 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 8.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     1.2200D+00, 1.0000D+00, 1.0000D+00, 9.0000D-01, 5.0000D-01, & ! +   
     &     1.0000D+00, 6.7000D-01, 7.0000D-01, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 1.0000D+00, 2.0000D-01, 2.0000D-01, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, 2.0000D-01, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.0000D-01, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.5000D-01, 1.1800D-01, 6.0000D-02, & ! 8   
     &     6.0000D-02, 6.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.0000D-01, & ! O   
     &     1.3000D-01, 1.1000D-01, 0.0000D+00, 1.7000D-01, 1.6000D-01, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.8400D-01, & ! 1   
     &     0.0000D+00, 3.5000D-01, 7.0000D-02, 7.0000D-02, 9.0400D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.5000D-01, 1.0000D+00, & ! 2   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     7.0000D-02, 4.3000D-01, 8.0000D-02, 3.2000D-01, 8.0000D-02, & ! 3   
     &     1.0000D-01, 1.0000D-01, 7.9000D-01, 0.0000D+00, 1.0000D-01, & ! +   
     &     2.8000D-01, 8.0000D-03, 6.0000D-01, 6.0000D-01, 1.5000D+00, & ! 4   
     &     1.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 6.8000D-01, 6.6800D-01, 1.0000D+00, & ! +   
     &     1.0000D+00, 8.5000D-01, 8.0000D-02, 1.5000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 2.8000D-01, 5.0000D-01, 2.5000D-01, 3.0000D-01, & ! +   
     &     2.5000D-01, 5.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 4.8000D-02, 2.3100D-01, 4.1600D-01, & ! 8   
     &     1.6000D+00, 7.8000D-01, 8.1000D-01, 4.5900D-01, 0.0000D+00, & ! +   
     &     9.5000D-01, 5.0000D-01, 0.0000D+00, 9.5000D-01, 9.5000D-01, & ! 9   
     &     3.5000D-01, 2.0000D-01, 1.8000D-01, 1.0000D+00, 1.8000D-01, & ! +   
     &     1.0000D+00, 4.9000D-01, 1.0000D+00, 2.3000D-01, 1.0000D+00, & ! O   
     &     5.0000D-02, 6.0000D-02, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     3.3000D-01, 1.6000D-01, 1.0000D+00, 5.0000D-01, 7.0000D-01, & ! 1   
     &     6.1000D-01, 0.0000D+00, 0.0000D+00, 3.2000D-01, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.2400D+00, 1.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     1.5000D-01, 1.5000D-01, 1.5000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 6.3000D-01, 7.5000D-01, 2.6000D-02, & ! 7   
     &     1.4000D-01, 2.8100D-01, 1.5000D-01, 7.0500D-01, 1.2800D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.7100D-01, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 3.6800D-01, 1.5900D-01, 5.0000D-01, & ! +   
     &     2.5000D-01, 7.5000D-01, 2.5000D-01, 5.0000D-01, 2.5000D-01, & ! 9   
     &     5.0000D-01, 2.5000D-01, 1.0000D+00, 1.0000D+00, 1.5000D+00, & ! +   
     &     3.3400D-01, 5.0000D-01, 5.0000D-01, 1.5000D+00, 1.1600D+00, & ! O   
     &     7.7300D-01, 1.0000D+00, 1.0000D+00, 3.2000D-01, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 9.6500D-01, 1.0000D+00, 0.0000D+00, & ! 1   
     &     1.0000D+00, 2.6000D-02, 1.4000D-01, 6.7000D-02, 1.6000D+00, & ! +   
     &     1.0000D+00, 1.7100D+00, 4.5900D-01, 0.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 3.6800D-01, & ! +   
     &     1.0480D+00, 5.0000D-01, 5.0000D-01, 5.0000D-01, 5.0000D-01, & ! 3   
     &     5.0000D-01, 5.0000D-01, 5.0000D-01, 5.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 3.3000D-01, 5.0000D-01, 2.6900D-01, & ! 4   
     &     1.0000D+00, 1.1600D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     3.2000D-01, 1.0000D+00, 0.0000D+00, 1.0000D+00, 2.8700D-01, & ! 5   
     &     7.0000D-01, 0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     9.2000D-02, 2.8000D-01, 5.6300D-01, 1.6000D+00, 7.9000D-01, & ! 6   
     &     1.1000D-01, 5.0000D-01, 0.0000D+00, 1.0000D+00, 5.0000D-01, & ! +   
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 3.6800D-01, 7.5000D-01, & ! 7   
     &     1.0000D+00, 3.8500D-01, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     3.3000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, 7.0000D-01, & ! 8   
     &     6.1000D-01, 0.0000D+00, 0.0000D+00, 3.2000D-01, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.2400D+00, 1.0000D+00, 0.0000D+00, & ! 9   
     &     2.0200D-01, 1.2100D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, & ! O   
     &     4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, & ! +   
     &     4.8000D-01, 4.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.4900D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 5.1000D-01, 9.2000D-01, 4.0000D-01, 6.6000D-01, & ! 3   
     &     0.0000D+00, 2.0000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 0.0000D+00, 6.2000D-03, & ! 7   
     &     2.8620D-01, 9.5000D-02, 3.0630D-01, 2.0890D-01, 1.4300D-02, & ! +   
     &     3.9310D-01, 2.2720D-01, 2.7490D-01, 2.4210D-01/           !8   

      DATA ( SC( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     3.5000D-01, 0.0000D+00, 0.0000D+00, 1.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.3000D-01, 7.0000D-01, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 8.0000D-01, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.4000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 6.5000D-01, 5.3000D-01, 1.7700D-01, & ! 8   
     &     1.7700D-01, 1.7700D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 7.8000D-01, & ! O   
     &     1.0000D-01, 2.7000D-01, 0.0000D+00, 5.9000D-02, 5.6000D-02, & ! +   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 8.0000D-02, 8.0000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.5000D-01, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0000D+00, 6.0000D-02, 7.0000D-02, 1.0000D-01, & ! 3   
     &     5.0000D-02, 5.0000D-02, 3.3000D-01, 0.0000D+00, 2.2000D-01, & ! +   
     &     1.0000D-02, 2.0000D-03, 6.0000D-01, 6.0000D-01, 4.8000D-01, & ! 4   
     &     8.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 6.8000D-01, 3.3200D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 4.2400D-01, 4.0000D-01, 1.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 1.5000D-01, 4.9000D-01, 3.0800D-01, 4.9000D-01, & ! +   
     &     1.5000D-01, 4.9000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 8.9000D-02, 2.3500D-01, 7.3900D-01, & ! 8   
     &     2.0000D-01, 1.2000D-02, 6.8000D-01, 4.5900D-01, 0.0000D+00, & ! +   
     &     5.0000D-02, 5.0000D-01, 0.0000D+00, 5.0000D-02, 9.5000D-01, & ! 9   
     &     6.0000D-01, 2.8000D-01, 1.8000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     9.5000D-01, 9.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     6.7000D-01, 1.0000D+00, 6.5000D-01, 1.0000D+00, 1.0000D+00, & ! 1   
     &     3.0000D-02, 0.0000D+00, 0.0000D+00, 6.8000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.6400D-01, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     4.1000D-01, 4.1000D-01, 8.5000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-01, 2.6000D-02, & ! 7   
     &     1.9100D-01, 7.5000D-01, 2.5000D-01, 4.5000D-02, 2.1800D-01, & ! +   
     &     4.5900D-01, 0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 1.0000D+00, 6.3200D-01, 2.5000D-01, 2.5000D-01, & ! +   
     &     1.0000D+00, 5.0000D-01, 1.0000D+00, 2.5000D-01, 1.0000D+00, & ! 9   
     &     2.5000D-01, 1.0000D+00, 1.0000D-01, 1.0000D-01, 2.5000D-01, & ! +   
     &     2.5000D-01, 2.5000D-01, 1.6600D+00, 5.0000D-01, 1.5000D+00, & ! O   
     &     2.0300D-01, 0.0000D+00, 0.0000D+00, 6.8000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 9.3000D-01, 7.0000D-01, 0.0000D+00, & ! 1   
     &     5.0000D-01, 2.6000D-02, 1.9100D-01, 2.0800D-01, 2.0000D-01, & ! +   
     &     9.4000D-01, 2.9000D-01, 4.5800D-01, 0.0000D+00, 0.0000D+00, & ! 2   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D+00, 6.3200D-01, & ! +   
     &     2.1900D-01, 5.0000D-01, 1.0000D+00, 5.0000D-01, 1.0000D+00, & ! 3   
     &     5.0000D-01, 1.0000D+00, 5.0000D-01, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 7.5000D-01, 3.3400D-01, 5.0000D-01, 5.0000D-01, & ! 4   
     &     5.0000D-01, 1.1600D+00, 3.0000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     6.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.2400D+00, & ! 5   
     &     7.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     5.0300D-01, 4.8500D-01, 1.0000D+00, 2.0000D-01, 1.0000D+00, & ! 6   
     &     1.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, 5.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 6.3200D-01, 3.1800D-01, & ! 7   
     &     1.0000D+00, 3.8500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     6.7000D-01, 0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     3.0000D-02, 0.0000D+00, 0.0000D+00, 6.8000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.6400D-01, 0.0000D+00, 0.0000D+00, & ! 9   
     &     6.4000D-01, 2.8500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, & ! O   
     &     4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, & ! +   
     &     4.8000D-01, 4.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.5000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 7.5000D-01, 3.2000D-01, 3.4000D-01, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-03, & ! 7   
     &     4.1000D-03, 1.3730D-01, 1.5300D-02, 3.0000D-01, 1.2300D-02, & ! +   
     &     1.3900D-02, 2.6070D-01, 4.9100D-02, 6.4000D-02/           !8   

      DATA ( SC( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     4.3400D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 5.0000D-01, 5.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 4.2000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.5000D-01, 1.0000D+00, 1.0000D+00, & ! 8   
     &     9.8000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-01, & ! O   
     &     1.0000D-02, 1.0000D-02, 0.0000D+00, 6.1900D-01, 5.8400D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 4.1000D-01, 4.1000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.7000D-01, 4.0000D-02, 4.0000D-02, 1.0000D-01, & ! 3   
     &     5.0000D-02, 5.0000D-02, 4.0000D-02, 0.0000D+00, 5.0000D-01, & ! +   
     &     5.6000D-01, 1.0000D-01, 1.5000D+00, 1.5000D+00, 7.0000D-01, & ! 4   
     &     1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 3.2000D-01, 3.3200D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 4.2400D-01, 4.0000D-01, 5.0000D-01, 0.0000D+00, & ! 6   
     &     0.0000D+00, 2.8000D-01, 1.0000D-02, 1.5000D-01, 1.0000D-02, & ! +   
     &     3.0800D-01, 1.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 9.3500D-01, 8.6400D-01, 1.5000D-01, & ! 8   
     &     0.0000D+00, 4.4000D-01, 2.0000D-01, 9.1800D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, 1.0500D+00, & ! 9   
     &     7.0000D-01, 4.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     9.5000D-01, 9.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 4.6000D-01, 6.5000D-01, 0.0000D+00, 3.0000D-01, & ! 1   
     &     2.7000D-01, 0.0000D+00, 0.0000D+00, 6.8000D-01, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-01, 8.2700D-01, & ! 7   
     &     7.7700D-01, 1.9700D-01, 2.5000D-01, 2.5000D-01, 2.5000D-01, & ! +   
     &     6.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 1.0500D+00, 7.3700D-01, 2.5000D-01, 2.5000D-01, & ! +   
     &     0.0000D+00, 2.5000D-01, 0.0000D+00, 2.5000D-01, 0.0000D+00, & ! 9   
     &     2.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-01, & ! +   
     &     2.5000D-01, 2.5000D-01, 6.7000D-02, 2.5000D-01, 1.7500D+00, & ! O   
     &     5.2500D-01, 0.0000D+00, 0.0000D+00, 6.8000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.4800D-01, 7.0000D-01, 0.0000D+00, & ! 1   
     &     0.0000D+00, 1.3000D-01, 4.2000D-02, 2.1700D-01, 5.0000D-01, & ! +   
     &     6.0000D-02, 5.0000D-01, 6.0000D-01, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0500D+00, 7.3700D-01, & ! +   
     &     3.0500D-01, 5.0000D-01, 0.0000D+00, 5.0000D-01, 0.0000D+00, & ! 3   
     &     5.0000D-01, 0.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, 1.0000D+00, & ! 4   
     &     5.0000D-01, 1.0000D+00, 2.7000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     6.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.6400D-01, & ! 5   
     &     7.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.0300D-01, 0.0000D+00, 1.8000D-01, & ! 6   
     &     2.0000D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0500D+00, 7.3700D-01, 5.0000D-01, & ! 7   
     &     0.0000D+00, 6.1500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.0000D-01, & ! 8   
     &     2.7000D-01, 0.0000D+00, 0.0000D+00, 6.8000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     1.4900D-01, 1.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, & ! O   
     &     4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, 4.8000D-01, & ! +   
     &     4.8000D-01, 4.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     2.4000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.7000D-01, 8.0000D-02, 4.3000D-01, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.6000D-03, & ! 7   
     &     3.5000D-03, 5.0000D-04, 1.0430D-01, 2.0280D-01, 1.2390D-01, & ! +   
     &     1.0270D-01, 7.0200D-02, 2.5770D-01, 3.8500D-02/           !8   

      DATA ( SC( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     2.1600D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 6.7000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 5.0000D-01, 5.0000D-01, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0000D-02, & ! O   
     &     7.8000D-01, 7.9000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.0000D-02, 9.0000D-02, 9.0000D-02, & ! 3   
     &     1.4000D-01, 1.4000D-01, 2.0000D-01, 0.0000D+00, 4.5000D-01, & ! +   
     &     1.0000D-01, 2.4300D-01, 5.0000D-02, 5.0000D-02, 2.5000D-01, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 6.8000D-01, 1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, & ! 6   
     &     0.0000D+00, 1.0000D-02, 0.0000D+00, 2.2400D-01, 0.0000D+00, & ! +   
     &     2.2400D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.0400D-01, 1.8000D-02, 6.4200D-01, & ! 8   
     &     0.0000D+00, 6.0000D-02, 9.0000D-02, 8.2000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 5.0000D-02, 0.0000D+00, 0.0000D+00, 5.0000D-02, & ! 9   
     &     7.3000D-02, 1.2000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.3000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 5.4000D-01, 0.0000D+00, 0.0000D+00, 7.0000D-01, & ! 1   
     &     1.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.9800D-01, & ! 7   
     &     2.5100D-01, 6.5200D-01, 0.0000D+00, 2.5000D-01, 2.5000D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 7.7000D-02, 2.5000D-01, 2.5000D-01, & ! +   
     &     0.0000D+00, 2.5000D-01, 0.0000D+00, 2.5000D-01, 0.0000D+00, & ! 9   
     &     2.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.2500D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.5000D-01, 2.5000D-01, 5.0000D-01, & ! O   
     &     1.3500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.5000D-01, 3.0000D-01, 0.0000D+00, & ! 1   
     &     0.0000D+00, 2.7300D-01, 3.8100D-01, 6.4200D-01, 0.0000D+00, & ! +   
     &     5.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 7.7000D-02, & ! +   
     &     5.0000D-01, 0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     5.0000D-01, 2.3000D+00, 7.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-01, & ! 5   
     &     3.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     5.1900D-01, 2.4000D-02, 8.6900D-01, 0.0000D+00, 2.0000D-02, & ! 6   
     &     8.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 7.7000D-02, 2.4000D-02, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 7.0000D-01, & ! 8   
     &     7.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     1.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     4.0000D-02, 4.0000D-02, 4.8000D-01, 4.0000D-02, 4.0000D-02, & ! O   
     &     4.8000D-01, 4.0000D-02, 4.0000D-02, 4.8000D-01, 4.0000D-02, & ! +   
     &     4.0000D-02, 4.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.0000D-02, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.3000D-03, & ! 7   
     &     2.2390D-01, 2.0510D-01, 1.8930D-01, 4.7100D-02, 1.8310D-01, & ! +   
     &     2.0450D-01, 1.1160D-01, 7.3900D-02, 2.6670D-01/           !8   

      DATA ( SC( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 6.7000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
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
     &     0.0000D+00, 0.0000D+00, 6.8000D-02, 3.7000D-01, 1.4000D-01, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     5.4000D-01, 8.0000D-02, 5.0000D-02, 5.0000D-02, 2.5000D-01, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 2.8000D-01, 0.0000D+00, 1.0000D-02, 0.0000D+00, & ! +   
     &     1.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.3200D-01, 4.5000D-02, 2.6100D-01, & ! 8   
     &     0.0000D+00, 1.3000D-01, 2.0000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     1.7700D-01, 2.1000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.0000D-01, & ! 1   
     &     7.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.9700D-01, & ! 7   
     &     6.1800D-01, 2.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 1.8600D-01, 2.3000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 2.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.5000D-01, 0.0000D+00, 2.5000D-01, & ! O   
     &     1.0500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.5000D-01, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 6.6200D-01, 8.2400D-01, 4.9500D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.8600D-01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 5.0000D-01, 1.8000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.4700D-01, 2.4100D-01, 0.0000D+00, 0.0000D+00, 9.0000D-02, & ! 6   
     &     4.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.8600D-01, 3.3000D-02, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.0000D-01, & ! 8   
     &     1.8000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.0000D-02, 0.0000D+00, 0.0000D+00, & ! O   
     &     4.0000D-02, 0.0000D+00, 0.0000D+00, 4.0000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 4.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.9440D-01, & ! 7   
     &     1.8200D-01, 1.7640D-01, 1.6680D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.3000D-01, 2.6000D-02, 5.8000D-01, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     7.0000D-02, 4.2000D-01, 8.0000D-02, 8.0000D-02, 1.1000D-01, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 8.4000D-01, 0.0000D+00, & ! +   
     &     8.4000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.6500D-01, 2.0300D-01, 0.0000D+00, & ! 8   
     &     0.0000D+00, 3.0000D-02, 5.0000D-02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     5.0000D-02, 2.9000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     2.1000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 5.0000D-02, & ! 7   
     &     2.5000D-01, 2.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.8000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.5000D-01, 0.0000D+00, 2.5000D-01, & ! O   
     &     2.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 6.7000D-02, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 1.0830D+00, 1.0500D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     7.5000D-02, 6.0000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.1000D-02, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     2.1000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0210D-01, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

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
     &     0.0000D+00, 0.0000D+00, 2.0000D-02, 1.0000D-02, 4.6100D-01, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     7.0000D-02, 2.8000D-02, 6.5000D-01, 7.0000D-01, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.2000D-02, 3.3000D-02, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 2.7000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-01, & ! 7   
     &     2.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.6000D-02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.9200D-01, & ! O   
     &     2.5000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 5.0000D-01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     9.5000D-02, 6.3000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.9000D-03, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

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
     &     0.0000D+00, 0.0000D+00, 1.5000D-02, 1.0000D-02, 1.8900D-01, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0000D-01, 4.9100D-01, 0.0000D+00, 6.5000D-01, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 6.5000D-02, 2.1700D-01, 0.0000D+00, & ! 8   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.5000D-01, & ! 7   
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
     &     0.0000D+00, 2.4700D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.3000D-03, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

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
     &     0.0000D+00, 0.0000D+00, 6.0000D-03, 9.0000D-02, 2.8000D-01, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.0000D-03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.3000D-02, 0.0000D+00, & ! 8   
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
     &     0.0000D+00, 4.8000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

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
     &     0.0000D+00, 0.0000D+00, 3.2000D-02, 4.5700D-01, 1.5300D-01, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 4.4000D-02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.7200D-01, 0.0000D+00, & ! 8   
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
     &     0.0000D+00, 2.7500D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

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
     &     0.0000D+00, 0.0000D+00, 5.6000D-01, 7.3000D-01, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 1.3600D-01, 0.0000D+00, & ! 8   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

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
     &     0.0000D+00, 0.0000D+00, 1.0000D-02, 1.1000D-01, 0.0000D+00, & ! 3   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.4000D-01, 1.7000D-02, 0.0000D+00, & ! 3   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.0000D-02, 4.4000D-02, 0.0000D+00, & ! 3   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 2.0000D-02, 1.7000D-02, 0.0000D+00, & ! 3   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 6.0000D-02, 0.0000D+00, 0.0000D+00, & ! 3   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0000D-02, 0.0000D+00, 0.0000D+00, & ! 3   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.0000D-02, 0.0000D+00, 0.0000D+00, & ! 3   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 6.0000D-02, 0.0000D+00, 0.0000D+00, & ! 3   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/           !8   

      INTEGER            :: NREACT( NRXNS )

      DATA ( NREACT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    2,    2,    2, & ! 3   
     &      2,    1,    2,    1,    1,    1,    1,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    1,    1,    2, & ! 6   
     &      1,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    2,    1,    2,    1,    2,    1, & ! 7   
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
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2/     !  8   
      INTEGER            :: NPRDCT( NRXNS )

      DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    1,    2,    1,    2,    2,    2,    4,    1, & ! O   
     &      2,    3,    3,    2,    2,    6,    3,    3,    4,    3, & ! 1   
     &      3,    7,    4,    1,    2,    2,    3,    6,    6,    3, & ! 2   
     &      3,    3,    3,    2,    4,    2,    2,    1,    1,    1, & ! 3   
     &      1,    1,    0,    1,    1,    1,    1,    0,    1,    1, & ! 4   
     &      1,    1,    1,    2,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    2,    3,    1,    2,    1,    1,    2,    1,    1, & ! 6   
     &      2,    1,    3,    1,    1,    1,    2,    2,    5,    1, & ! 7   
     &      1,    1,    5,    5,    5,    5,    5,    1,    2,    2, & ! 8   
     &      1,    1,    2,    1,    1,    1,    1,    1,    2,    2, & ! 9   
     &      1,    2,    2,    2,    6,    6,    6,    1,    5,    5, & ! O   
     &      4,    1,    2,    2,    3,    2,    3,    5,    5,    3, & ! 1   
     &      1,    1,    2,    4,    3,    3,    2,    2,    1,    3, & ! 2   
     &      3,    5,   21,   17,   12,    6,    6,    6,    1,    6, & ! 3   
     &     10,   12,    9,   10,    8,    5,    1,    2,    2,    2, & ! 4   
     &      1,    2,    2,    1,    3,    2,    2,    6,    6,    3, & ! 5   
     &      3,    5,    5,    6,    2,    2,    7,    5,    8,    5, & ! 6   
     &      8,    5,    1,    1,    1,    2,    1,    2,    1,    2, & ! 7   
     &      3,    3,   10,   13,    7,    4,    8,    8,    6,    2, & ! 8   
     &      4,    6,    2,    4,    6,    8,    9,    4,    3,    3, & ! 9   
     &      3,    6,    3,    3,    3,    5,    5,    2,    2,    3, & ! O   
     &      4,    6,    5,    4,    7,    8,    2,    2,    5,    2, & ! 1   
     &      3,    3,    4,    3,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 3   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    4,    4,    4,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    3,    5,   10,    9,    8,    5,    6,    6, & ! 7   
     &      5,    3,    4,    4,    3,    3,    5,    7,    9,    6, & ! 8   
     &      4,    7,    4,    6,    4,    6,    4,    4,    4,    6, & ! 9   
     &      5,    5,    8,    6,    9,    9,    3,    3,    5,    3, & ! O   
     &      3,    3,    8,    6,    2,    4,    9,    8,    7,    5, & ! 1   
     &      6,    5,    5,    2,    3,    4,    2,    3,    5,    7, & ! 2   
     &      6,    5,    4,    6,    4,    5,    4,    5,    4,    1, & ! 3   
     &      2,    4,    5,    4,    5,    6,    8,    9,    2,    2, & ! 4   
     &      5,    3,    2,    3,    6,    6,    1,    1,    3,    3, & ! 5   
     &      9,   12,    6,    4,    7,    7,    5,    2,    3,    5, & ! 6   
     &      2,    3,    5,    7,    8,    4,    5,    2,    2,    3, & ! 7   
     &      4,    3,    4,    4,    7,    8,    2,    2,    5,    2, & ! 8   
     &      3,    3,    4,    3,    2,    6,    5,    1,    1,    0, & ! 9   
     &      6,    6,    7,    6,    6,    7,    6,    6,    7,    6, & ! O   
     &      6,    7,    2,    2,    2,    2,    2,    2,    2,    1, & ! 1   
     &      5,    2,    2,    2,    2,    1,    1,    1,    2,    2, & ! 2   
     &      2,    3,    5,    5,    6,    2,    3,    3,    1,    1, & ! 3   
     &      2,    2,    2,    1,    1,    1,    2,    1,    1,    1, & ! 4   
     &      1,    1,    2,    0,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    3, & ! 6   
     &      1,    3,    1,    2,   10,    7,    7,    7,    6,    6, & ! 7   
     &      6,    6,    6,    6/     !  8   

      INTEGER, PARAMETER :: MHETERO =   7
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    448,  449,  452,  453,  455,  471,  473/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    2,    3,    4,    5,    6,    7/

      INTEGER, PARAMETER :: NPHOTAB =  31
      CHARACTER( 16 )    :: PHOTAB( NPHOTAB )

      DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & 
     &   'O3O3P_NASA06    ', 'O3O1D_NASA06    ', 'H2O2_RACM2      ', & 
     &   'NO2_RACM2       ', 'NO3NO_RACM2     ', 'NO3NO2_RACM2    ', & 
     &   'HONO_RACM2      ', 'HNO3_RACM2      ', 'HNO4_RACM2      ', & 
     &   'HCHO_MOL_JPL19  ', 'HCHO_RAD_JPL19  ', 'CH3CHO_RACM2    ', & 
     &   'ALD_JPL19       ', 'CH3COCH3A_JPL19 ', 'CH3COCH3B_JPL19 ', & 
     &   'UALD_RACM2      ', 'MEK_JGR19       ', 'KET_JGR19       ', & 
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
     &    'R015            ', 'TRP01           ', 'TRP02           ', & ! 5   
     &    'R016            ', 'R017            ', 'R018            ', & ! 6   
     &    'R019            ', 'R020            ', 'R021            ', & ! 7   
     &    'R022            ', 'R023            ', 'R024            ', & ! 8   
     &    'R025            ', 'R026            ', 'R027            ', & ! 9   
     &    'R028            ', 'R029            ', 'TRP03           ', & ! 0   
     &    'R030            ', 'R031            ', 'R032            ', & ! 1   
     &    'R033            ', 'R034            ', 'R035            ', & ! 2   
     &    'R036            ', 'R037            ', 'R038            ', & ! 3   
     &    'R039            ', 'R040            ', 'R041            ', & ! 4   
     &    'R042            ', 'R043            ', 'R044            ', & ! 5   
     &    'R045            ', 'R046            ', 'R047            ', & ! 6   
     &    'R048            ', 'R049            ', 'R050            ', & ! 7   
     &    'R051            ', 'R052            ', 'R053            ', & ! 8   
     &    'R054            ', 'R055            ', 'R056            ', & ! 9   
     &    'R057            ', 'R058            ', 'R059            ', & ! 0   
     &    'R060            ', 'R061            ', 'R062            ', & ! 1   
     &    'R063            ', 'R064            ', 'R065            ', & ! 2   
     &    'R066            ', 'R067            ', 'R068            ', & ! 3   
     &    'R069            ', 'R070            ', 'R071            ', & ! 4   
     &    'R072            ', 'R073            ', 'R074            ', & ! 5   
     &    'R075            ', 'R076            ', 'R077            ', & ! 6   
     &    'R078            ', 'R080            ', 'R081            ', & ! 7   
     &    'R082            ', 'R083            ', 'R084c           ', & ! 8   
     &    'R086            ', 'R087            ', 'R088            ', & ! 9   
     &    'TRP04           ', 'TRP05           ', 'R089            ', & ! 0   
     &    'R090            ', 'R091            ', 'R092            ', & ! 1   
     &    'R093            ', 'R094            ', 'R095            ', & ! 2   
     &    'R096            ', 'R097            ', 'R098            ', & ! 3   
     &    'R099            ', 'R100            ', 'R101            ', & ! 4   
     &    'R102            ', 'R103            ', 'R104            ', & ! 5   
     &    'R105            ', 'R106            ', 'R107            ', & ! 6   
     &    'R108            ', 'R109            ', 'R110            ', & ! 7   
     &    'R111            ', 'R112            ', 'R113            ', & ! 8   
     &    'R114            ', 'TRP06           ', 'R115            ', & ! 9   
     &    'R116            ', 'R117            ', 'R118            ', & ! 0   
     &    'R119            ', 'R120            ', 'R121            ', & ! 1   
     &    'R122            ', 'R123            ', 'TRP07           ', & ! 2   
     &    'R124            ', 'R125            ', 'R126            ', & ! 3   
     &    'R127            ', 'R128            ', 'R130            ', & ! 4   
     &    'R131            ', 'R132            ', 'TRP08           ', & ! 5   
     &    'TRP09           ', 'R132            ', 'R134            ', & ! 6   
     &    'R135            ', 'R136            ', 'R137            ', & ! 7   
     &    'R138            ', 'R139            ', 'R140            ', & ! 8   
     &    'R141            ', 'R142            ', 'R143            ', & ! 9   
     &    'R145            ', 'R146            ', 'R147            ', & ! 0   
     &    'TRP10           ', 'R148            ', 'R149            ', & ! 1   
     &    'R150            ', 'R151            ', 'R152            ', & ! 2   
     &    'R153            ', 'R154            ', 'R155            ', & ! 3   
     &    'R156            ', 'R157            ', 'R158            ', & ! 4   
     &    'R159            ', 'R160            ', 'R161            ', & ! 5   
     &    'R162            ', 'R163            ', 'R164            ', & ! 6   
     &    'R165            ', 'TRP11           ', 'TRP12           ', & ! 7   
     &    'R166            ', 'R167            ', 'R168            ', & ! 8   
     &    'R169            ', 'R170            ', 'R171            ', & ! 9   
     &    'R172            ', 'R173            ', 'R174            ', & ! 0   
     &    'R175            ', 'R176            ', 'R177            ', & ! 1   
     &    'R178            ', 'R179            ', 'R180            ', & ! 2   
     &    'R181            ', 'R182            ', 'R183            ', & ! 3   
     &    'R184            ', 'R185            ', 'R186            ', & ! 4   
     &    'R187            ', 'R188            ', 'R189            ', & ! 5   
     &    'TRP13           ', 'TRP14           ', 'TRP15           ', & ! 6   
     &    'R190            ', 'TRP16           ', 'TRP17           ', & ! 7   
     &    'TRP18           ', 'TRP19           ', 'TRP20           ', & ! 8   
     &    'R191            ', 'R192            ', 'R193            ', & ! 9   
     &    'R194            ', 'R195            ', 'R196            ', & ! 0   
     &    'R197            ', 'R198            ', 'R199            ', & ! 1   
     &    'R200            ', 'R201            ', 'R202            ', & ! 2   
     &    'R203            ', 'R204            ', 'R205            ', & ! 3   
     &    'R206            ', 'R207            ', 'R208            ', & ! 4   
     &    'R209            ', 'R210            ', 'R211            ', & ! 5   
     &    'R212            ', 'R213            ', 'R214            ', & ! 6   
     &    'R215            ', 'R216            ', 'R217            ', & ! 7   
     &    'R218            ', 'R219            ', 'R220            ', & ! 8   
     &    'R221            ', 'R222            ', 'R223            ', & ! 9   
     &    'R224            ', 'R225            ', 'R226            ', & ! 0   
     &    'R227            ', 'R228            ', 'R229            ', & ! 1   
     &    'TRP21           ', 'TRP22           ', 'TRP23           ', & ! 2   
     &    'R230            ', 'TRP24           ', 'TRP25           ', & ! 3   
     &    'TRP26           ', 'TRP27           ', 'TRP28           ', & ! 4   
     &    'R231            ', 'R232            ', 'R233            ', & ! 5   
     &    'R234            ', 'R235            ', 'R236            ', & ! 6   
     &    'R237            ', 'R238            ', 'R239            ', & ! 7   
     &    'R240            ', 'R241            ', 'R242            ', & ! 8   
     &    'R243            ', 'R244            ', 'R245            ', & ! 9   
     &    'R246            ', 'R247            ', 'R248            ', & ! 0   
     &    'R249            ', 'R250            ', 'R251            ', & ! 1   
     &    'R252            ', 'R253            ', 'R254            ', & ! 2   
     &    'R255            ', 'R256            ', 'R257            ', & ! 3   
     &    'R258            ', 'R259            ', 'R260            ', & ! 4   
     &    'R261            ', 'R262            ', 'R263            ', & ! 5   
     &    'R264            ', 'R265            ', 'TRP29           ', & ! 6   
     &    'TRP30           ', 'TRP31           ', 'R266            ', & ! 7   
     &    'TRP32           ', 'TRP33           ', 'TRP34           ', & ! 8   
     &    'R267            ', 'R268            ', 'R269            ', & ! 9   
     &    'R270            ', 'R271            ', 'R272            ', & ! 0   
     &    'R273            ', 'R274            ', 'R275            ', & ! 1   
     &    'R276            ', 'R277            ', 'R278            ', & ! 2   
     &    'R279            ', 'R280            ', 'R281            ', & ! 3   
     &    'R282            ', 'R283            ', 'R284            ', & ! 4   
     &    'R285            ', 'R286            ', 'R287            ', & ! 5   
     &    'R288            ', 'R289            ', 'R290            ', & ! 6   
     &    'R291            ', 'R292            ', 'R293            ', & ! 7   
     &    'R294            ', 'R295            ', 'R296            ', & ! 8   
     &    'R297            ', 'R298            ', 'R299            ', & ! 9   
     &    'R300            ', 'R301            ', 'TRP35           ', & ! 0   
     &    'TRP36           ', 'TRP37           ', 'R302            ', & ! 1   
     &    'TRP38           ', 'TRP39           ', 'TRP40           ', & ! 2   
     &    'R303            ', 'R304            ', 'R305            ', & ! 3   
     &    'R306            ', 'R307            ', 'R308            ', & ! 4   
     &    'R309            ', 'R310            ', 'R311            ', & ! 5   
     &    'R312            ', 'R313            ', 'R314            ', & ! 6   
     &    'R315            ', 'R316            ', 'R317            ', & ! 7   
     &    'R318            ', 'R319            ', 'R320            ', & ! 8   
     &    'R321            ', 'R322            ', 'R323            ', & ! 9   
     &    'R324            ', 'R325            ', 'R326            ', & ! 0   
     &    'R327            ', 'R328            ', 'R329            ', & ! 1   
     &    'R330            ', 'R331            ', 'R332            ', & ! 2   
     &    'R333            ', 'R334            ', 'R335            ', & ! 3   
     &    'R336            ', 'R337            ', 'R338            ', & ! 4   
     &    'R339            ', 'R340            ', 'R341            ', & ! 5   
     &    'R342            ', 'R343            ', 'R344            ', & ! 6   
     &    'R345            ', 'R346            ', 'R347            ', & ! 7   
     &    'R348            ', 'R349            ', 'R350            ', & ! 8   
     &    'R351            ', 'R352            ', 'R353            ', & ! 9   
     &    'R354            ', 'R355            ', 'R356            ', & ! 0   
     &    'R357            ', 'R358            ', 'R359            ', & ! 1   
     &    'R360            ', 'R361            ', 'R362            ', & ! 2   
     &    'R363            ', 'TRP41           ', 'TRP42           ', & ! 3   
     &    'TRP43           ', 'TRP44           ', 'TRP45           ', & ! 4   
     &    'TRP46           ', 'TRP47           ', 'TRP48           ', & ! 5   
     &    'TRP49           ', 'TRP50           ', 'TRP51           ', & ! 6   
     &    'TRP52           ', 'SA01            ', 'SA02            ', & ! 7   
     &    'SA03            ', 'SA04            ', 'SA05            ', & ! 8   
     &    'SA06            ', 'SA13            ', 'SA14            ', & ! 9   
     &    'R001c           ', 'R002c           ', 'SA10            ', & ! 0   
     &    'SA11            ', 'SA12            ', 'T17             ', & ! 1   
     &    'T18             ', 'T19             ', 'T10             ', & ! 2   
     &    'T11             ', 'T12             ', 'R003c           ', & ! 3   
     &    'R004c           ', 'R005c           ', 'R006c           ', & ! 4   
     &    'R007c           ', 'R008c           ', 'R010c           ', & ! 5   
     &    'R011c           ', 'R012c           ', 'R013c           ', & ! 6   
     &    'R014c           ', 'R015c           ', 'R016c           ', & ! 7   
     &    'R017c           ', 'R019c           ', 'R020c           ', & ! 8   
     &    'HET_GLY         ', 'HET_MGLY        ', 'HET_ISON        ', & ! 9   
     &    'HET_TRPN        ', 'HET_N2O5        ', 'HET_N02         ', & ! 0   
     &    'HAL_Ozone       ', 'HET_IEPOX       ', 'OLIG_XYLENE1    ', & ! 1   
     &    'OLIG_XYLENE2    ', 'OLIG_TOLUENE1   ', 'OLIG_TOLUENE2   ', & ! 2   
     &    'OLIG_BENZENE1   ', 'OLIG_BENZENE2   ', 'OLIG_TERPENE1   ', & ! 3   
     &    'OLIG_TERPENE2   ', 'OLIG_ISOPRENE1  ', 'OLIG_ISOPRENE2  ', & ! 4   
     &    'OLIG_PAH1       ', 'OLIG_PAH2       ', 'OLIG_ALK1       ', & ! 5   
     &    'OLIG_ALK2       ', 'RPOAGEPI        ', 'RPOAGELI        ', & ! 6   
     &    'RPOAGEPJ        ', 'RPOAGELJ        ', 'PCSOA           ', & ! 7   
     &    'POA_AGE1        ', 'POA_AGE2        ', 'POA_AGE3        ', & ! 8   
     &    'POA_AGE4        ', 'POA_AGE5        ', 'POA_AGE6        ', & ! 9   
     &    'POA_AGE7        ', 'POA_AGE8        ', 'POA_AGE9        ', & ! 0   
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
       INTEGER, PARAMETER  :: IJ_HCHO_MOL_JPL19   =  10
       INTEGER, PARAMETER  :: IJ_HCHO_RAD_JPL19   =  11
       INTEGER, PARAMETER  :: IJ_CH3CHO_RACM2     =  12
       INTEGER, PARAMETER  :: IJ_ALD_JPL19        =  13
       INTEGER, PARAMETER  :: IJ_CH3COCH3A_JPL19  =  14
       INTEGER, PARAMETER  :: IJ_CH3COCH3B_JPL19  =  15
       INTEGER, PARAMETER  :: IJ_UALD_RACM2       =  16
       INTEGER, PARAMETER  :: IJ_MEK_JGR19        =  17
       INTEGER, PARAMETER  :: IJ_KET_JGR19        =  18
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
