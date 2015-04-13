       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: /home/has/gitrepos/2015isoprene/CCTM/MECHS/saprc07tic_ae6i_aq/mech_saprc07tic_ae6i_aq.def
! for Mechanism Name: SAPRC07TIC_AE6I_AQ              

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

      CHARACTER( 32 ), PARAMETER :: MECHNAME = 'SAPRC07TIC_AE6I_AQ'

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC = 219
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 219

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
      DATA GAS_CHEM_SPC(  94 ) / 'yRAOOH          ' /
      DATA GAS_CHEM_SPC(  95 ) / 'xACROLEIN       ' /
      DATA GAS_CHEM_SPC(  96 ) / 'ETHENE          ' /
      DATA GAS_CHEM_SPC(  97 ) / 'PROPENE         ' /
      DATA GAS_CHEM_SPC(  98 ) / 'BUTADIENE13     ' /
      DATA GAS_CHEM_SPC(  99 ) / 'ISOPRENE        ' /
      DATA GAS_CHEM_SPC( 100 ) / 'APIN            ' /
      DATA GAS_CHEM_SPC( 101 ) / 'TRPRXN          ' /
      DATA GAS_CHEM_SPC( 102 ) / 'ACETYLENE       ' /
      DATA GAS_CHEM_SPC( 103 ) / 'BENZENE         ' /
      DATA GAS_CHEM_SPC( 104 ) / 'BENZRO2         ' /
      DATA GAS_CHEM_SPC( 105 ) / 'TOLUENE         ' /
      DATA GAS_CHEM_SPC( 106 ) / 'TOLRO2          ' /
      DATA GAS_CHEM_SPC( 107 ) / 'MXYL            ' /
      DATA GAS_CHEM_SPC( 108 ) / 'XYLRO2          ' /
      DATA GAS_CHEM_SPC( 109 ) / 'OXYL            ' /
      DATA GAS_CHEM_SPC( 110 ) / 'PXYL            ' /
      DATA GAS_CHEM_SPC( 111 ) / 'TRIMETH_BENZ124 ' /
      DATA GAS_CHEM_SPC( 112 ) / 'ETOH            ' /
      DATA GAS_CHEM_SPC( 113 ) / 'ALK1            ' /
      DATA GAS_CHEM_SPC( 114 ) / 'ALK2            ' /
      DATA GAS_CHEM_SPC( 115 ) / 'ALK3            ' /
      DATA GAS_CHEM_SPC( 116 ) / 'ALK4            ' /
      DATA GAS_CHEM_SPC( 117 ) / 'ALK5            ' /
      DATA GAS_CHEM_SPC( 118 ) / 'SOAALK          ' /
      DATA GAS_CHEM_SPC( 119 ) / 'ALKRXN          ' /
      DATA GAS_CHEM_SPC( 120 ) / 'OLE1            ' /
      DATA GAS_CHEM_SPC( 121 ) / 'OLE2            ' /
      DATA GAS_CHEM_SPC( 122 ) / 'ARO1            ' /
      DATA GAS_CHEM_SPC( 123 ) / 'ARO2MN          ' /
      DATA GAS_CHEM_SPC( 124 ) / 'NAPHTHAL        ' /
      DATA GAS_CHEM_SPC( 125 ) / 'PAHRO2          ' /
      DATA GAS_CHEM_SPC( 126 ) / 'TERP            ' /
      DATA GAS_CHEM_SPC( 127 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC( 128 ) / 'SESQRXN         ' /
      DATA GAS_CHEM_SPC( 129 ) / 'CL2             ' /
      DATA GAS_CHEM_SPC( 130 ) / 'CL              ' /
      DATA GAS_CHEM_SPC( 131 ) / 'CLNO            ' /
      DATA GAS_CHEM_SPC( 132 ) / 'CLONO           ' /
      DATA GAS_CHEM_SPC( 133 ) / 'CLNO2           ' /
      DATA GAS_CHEM_SPC( 134 ) / 'HCL             ' /
      DATA GAS_CHEM_SPC( 135 ) / 'CLO             ' /
      DATA GAS_CHEM_SPC( 136 ) / 'CLONO2          ' /
      DATA GAS_CHEM_SPC( 137 ) / 'HOCL            ' /
      DATA GAS_CHEM_SPC( 138 ) / 'xCL             ' /
      DATA GAS_CHEM_SPC( 139 ) / 'xCLCCHO         ' /
      DATA GAS_CHEM_SPC( 140 ) / 'xCLACET         ' /
      DATA GAS_CHEM_SPC( 141 ) / 'CLCCHO          ' /
      DATA GAS_CHEM_SPC( 142 ) / 'CLACET          ' /
      DATA GAS_CHEM_SPC( 143 ) / 'CLCHO           ' /
      DATA GAS_CHEM_SPC( 144 ) / 'BNZNRXN         ' /
      DATA GAS_CHEM_SPC( 145 ) / 'BNZHRXN         ' /
      DATA GAS_CHEM_SPC( 146 ) / 'XYLNRXN         ' /
      DATA GAS_CHEM_SPC( 147 ) / 'XYLHRXN         ' /
      DATA GAS_CHEM_SPC( 148 ) / 'TOLNRXN         ' /
      DATA GAS_CHEM_SPC( 149 ) / 'TOLHRXN         ' /
      DATA GAS_CHEM_SPC( 150 ) / 'PAHNRXN         ' /
      DATA GAS_CHEM_SPC( 151 ) / 'PAHHRXN         ' /
      DATA GAS_CHEM_SPC( 152 ) / 'HCHO_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 153 ) / 'CCHO_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 154 ) / 'ACROLEIN_PRIMARY' /
      DATA GAS_CHEM_SPC( 155 ) / 'ISOPO2          ' /
      DATA GAS_CHEM_SPC( 156 ) / 'ISOPRXN         ' /
      DATA GAS_CHEM_SPC( 157 ) / 'ISOPND          ' /
      DATA GAS_CHEM_SPC( 158 ) / 'ISOPNB          ' /
      DATA GAS_CHEM_SPC( 159 ) / 'HC5             ' /
      DATA GAS_CHEM_SPC( 160 ) / 'DIBOO           ' /
      DATA GAS_CHEM_SPC( 161 ) / 'ISOPOOH         ' /
      DATA GAS_CHEM_SPC( 162 ) / 'HPALD           ' /
      DATA GAS_CHEM_SPC( 163 ) / 'HACET           ' /
      DATA GAS_CHEM_SPC( 164 ) / 'NISOPO2         ' /
      DATA GAS_CHEM_SPC( 165 ) / 'NIT1            ' /
      DATA GAS_CHEM_SPC( 166 ) / 'NISOPOOH        ' /
      DATA GAS_CHEM_SPC( 167 ) / 'HC5OO           ' /
      DATA GAS_CHEM_SPC( 168 ) / 'DHMOB           ' /
      DATA GAS_CHEM_SPC( 169 ) / 'ISOPNOOD        ' /
      DATA GAS_CHEM_SPC( 170 ) / 'PROPNN          ' /
      DATA GAS_CHEM_SPC( 171 ) / 'MVKN            ' /
      DATA GAS_CHEM_SPC( 172 ) / 'ETHLN           ' /
      DATA GAS_CHEM_SPC( 173 ) / 'RNO3I           ' /
      DATA GAS_CHEM_SPC( 174 ) / 'ISOPNOOB        ' /
      DATA GAS_CHEM_SPC( 175 ) / 'MACRN           ' /
      DATA GAS_CHEM_SPC( 176 ) / 'NIT1NO3OOA      ' /
      DATA GAS_CHEM_SPC( 177 ) / 'NIT1NO3OOB      ' /
      DATA GAS_CHEM_SPC( 178 ) / 'PROPNNB         ' /
      DATA GAS_CHEM_SPC( 179 ) / 'NIT1OHOO        ' /
      DATA GAS_CHEM_SPC( 180 ) / 'MVKOO           ' /
      DATA GAS_CHEM_SPC( 181 ) / 'MACROO          ' /
      DATA GAS_CHEM_SPC( 182 ) / 'PYRUACD         ' /
      DATA GAS_CHEM_SPC( 183 ) / 'IEPOX           ' /
      DATA GAS_CHEM_SPC( 184 ) / 'IEPOXOO         ' /
      DATA GAS_CHEM_SPC( 185 ) / 'IMACO3          ' /
      DATA GAS_CHEM_SPC( 186 ) / 'IMPAA           ' /
      DATA GAS_CHEM_SPC( 187 ) / 'IMAPAN          ' /
      DATA GAS_CHEM_SPC( 188 ) / 'IMAE            ' /
      DATA GAS_CHEM_SPC( 189 ) / 'IHMML           ' /
      DATA GAS_CHEM_SPC( 190 ) / 'H2NO3PIJ        ' /
      DATA GAS_CHEM_SPC( 191 ) / 'H2NO3PK         ' /
      DATA GAS_CHEM_SPC( 192 ) / 'ACLI            ' /
      DATA GAS_CHEM_SPC( 193 ) / 'ACLJ            ' /
      DATA GAS_CHEM_SPC( 194 ) / 'ACLK            ' /
      DATA GAS_CHEM_SPC( 195 ) / 'IEPOXP          ' /
      DATA GAS_CHEM_SPC( 196 ) / 'IMAEP           ' /
      DATA GAS_CHEM_SPC( 197 ) / 'IHMMLP          ' /
      DATA GAS_CHEM_SPC( 198 ) / 'AIETETJ         ' /
      DATA GAS_CHEM_SPC( 199 ) / 'AIEOSJ          ' /
      DATA GAS_CHEM_SPC( 200 ) / 'ADIMJ           ' /
      DATA GAS_CHEM_SPC( 201 ) / 'AIMGAJ          ' /
      DATA GAS_CHEM_SPC( 202 ) / 'AIMOSJ          ' /
      DATA GAS_CHEM_SPC( 203 ) / 'AALK1J          ' /
      DATA GAS_CHEM_SPC( 204 ) / 'AOLGAJ          ' /
      DATA GAS_CHEM_SPC( 205 ) / 'AALK2J          ' /
      DATA GAS_CHEM_SPC( 206 ) / 'AXYL1J          ' /
      DATA GAS_CHEM_SPC( 207 ) / 'AXYL2J          ' /
      DATA GAS_CHEM_SPC( 208 ) / 'ATOL1J          ' /
      DATA GAS_CHEM_SPC( 209 ) / 'ATOL2J          ' /
      DATA GAS_CHEM_SPC( 210 ) / 'ABNZ1J          ' /
      DATA GAS_CHEM_SPC( 211 ) / 'ABNZ2J          ' /
      DATA GAS_CHEM_SPC( 212 ) / 'ATRP1J          ' /
      DATA GAS_CHEM_SPC( 213 ) / 'AOLGBJ          ' /
      DATA GAS_CHEM_SPC( 214 ) / 'ATRP2J          ' /
      DATA GAS_CHEM_SPC( 215 ) / 'ASQTJ           ' /
      DATA GAS_CHEM_SPC( 216 ) / 'APOCI           ' /
      DATA GAS_CHEM_SPC( 217 ) / 'APNCOMI         ' /
      DATA GAS_CHEM_SPC( 218 ) / 'APOCJ           ' /
      DATA GAS_CHEM_SPC( 219 ) / 'APNCOMJ         ' /




      LOGICAL   :: HALOGEN_PARAMETER = .TRUE. 


! MAPPED_TO_CGRID declares whether CMAQ namelists were used to determine 
! the below values of CGRID_INDEX, SPECIES_TYPE, SPECIES_MOLWT, and CONVERT_CONC
      LOGICAL, PARAMETER, PRIVATE :: F = .FALSE.
      LOGICAL, PARAMETER, PRIVATE :: T = .TRUE.


      LOGICAL   :: MAPPED_TO_CGRID   = .FALSE. 

      DATA CHEMISTRY_SPC(   1 ), CGRID_INDEX(   1 ), SPECIES_TYPE(   1 ), SPECIES_MOLWT(   1 ), CONVERT_CONC(   1 ) / 'NO2             ',    1, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(   2 ), CGRID_INDEX(   2 ), SPECIES_TYPE(   2 ), SPECIES_MOLWT(   2 ), CONVERT_CONC(   2 ) / 'NO              ',    2, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(   3 ), CGRID_INDEX(   3 ), SPECIES_TYPE(   3 ), SPECIES_MOLWT(   3 ), CONVERT_CONC(   3 ) / 'O3P             ',    3, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(   4 ), CGRID_INDEX(   4 ), SPECIES_TYPE(   4 ), SPECIES_MOLWT(   4 ), CONVERT_CONC(   4 ) / 'O3              ',    4, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(   5 ), CGRID_INDEX(   5 ), SPECIES_TYPE(   5 ), SPECIES_MOLWT(   5 ), CONVERT_CONC(   5 ) / 'NO3             ',    5, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(   6 ), CGRID_INDEX(   6 ), SPECIES_TYPE(   6 ), SPECIES_MOLWT(   6 ), CONVERT_CONC(   6 ) / 'N2O5            ',    6, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(   7 ), CGRID_INDEX(   7 ), SPECIES_TYPE(   7 ), SPECIES_MOLWT(   7 ), CONVERT_CONC(   7 ) / 'HNO3            ',    7, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(   8 ), CGRID_INDEX(   8 ), SPECIES_TYPE(   8 ), SPECIES_MOLWT(   8 ), CONVERT_CONC(   8 ) / 'O1D             ',    8, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(   9 ), CGRID_INDEX(   9 ), SPECIES_TYPE(   9 ), SPECIES_MOLWT(   9 ), CONVERT_CONC(   9 ) / 'OH              ',    9, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  10 ), CGRID_INDEX(  10 ), SPECIES_TYPE(  10 ), SPECIES_MOLWT(  10 ), CONVERT_CONC(  10 ) / 'HONO            ',   10, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  11 ), CGRID_INDEX(  11 ), SPECIES_TYPE(  11 ), SPECIES_MOLWT(  11 ), CONVERT_CONC(  11 ) / 'HO2             ',   11, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  12 ), CGRID_INDEX(  12 ), SPECIES_TYPE(  12 ), SPECIES_MOLWT(  12 ), CONVERT_CONC(  12 ) / 'CO              ',   12, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  13 ), CGRID_INDEX(  13 ), SPECIES_TYPE(  13 ), SPECIES_MOLWT(  13 ), CONVERT_CONC(  13 ) / 'CO2             ',   13, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  14 ), CGRID_INDEX(  14 ), SPECIES_TYPE(  14 ), SPECIES_MOLWT(  14 ), CONVERT_CONC(  14 ) / 'HNO4            ',   14, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  15 ), CGRID_INDEX(  15 ), SPECIES_TYPE(  15 ), SPECIES_MOLWT(  15 ), CONVERT_CONC(  15 ) / 'HO2H            ',   15, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  16 ), CGRID_INDEX(  16 ), SPECIES_TYPE(  16 ), SPECIES_MOLWT(  16 ), CONVERT_CONC(  16 ) / 'SO2             ',   16, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  17 ), CGRID_INDEX(  17 ), SPECIES_TYPE(  17 ), SPECIES_MOLWT(  17 ), CONVERT_CONC(  17 ) / 'SULF            ',   17, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  18 ), CGRID_INDEX(  18 ), SPECIES_TYPE(  18 ), SPECIES_MOLWT(  18 ), CONVERT_CONC(  18 ) / 'SULRXN          ',   18, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  19 ), CGRID_INDEX(  19 ), SPECIES_TYPE(  19 ), SPECIES_MOLWT(  19 ), CONVERT_CONC(  19 ) / 'MEO2            ',   19, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  20 ), CGRID_INDEX(  20 ), SPECIES_TYPE(  20 ), SPECIES_MOLWT(  20 ), CONVERT_CONC(  20 ) / 'HCHO            ',   20, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  21 ), CGRID_INDEX(  21 ), SPECIES_TYPE(  21 ), SPECIES_MOLWT(  21 ), CONVERT_CONC(  21 ) / 'COOH            ',   21, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  22 ), CGRID_INDEX(  22 ), SPECIES_TYPE(  22 ), SPECIES_MOLWT(  22 ), CONVERT_CONC(  22 ) / 'MEOH            ',   22, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  23 ), CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), SPECIES_MOLWT(  23 ), CONVERT_CONC(  23 ) / 'RO2C            ',   23, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  24 ), CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), SPECIES_MOLWT(  24 ), CONVERT_CONC(  24 ) / 'RO2XC           ',   24, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  25 ), CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), SPECIES_MOLWT(  25 ), CONVERT_CONC(  25 ) / 'MECO3           ',   25, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  26 ), CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), SPECIES_MOLWT(  26 ), CONVERT_CONC(  26 ) / 'PAN             ',   26, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  27 ), CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), SPECIES_MOLWT(  27 ), CONVERT_CONC(  27 ) / 'CCOOOH          ',   27, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  28 ), CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), SPECIES_MOLWT(  28 ), CONVERT_CONC(  28 ) / 'CCOOH           ',   28, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  29 ), CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), SPECIES_MOLWT(  29 ), CONVERT_CONC(  29 ) / 'RCO3            ',   29, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  30 ), CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), SPECIES_MOLWT(  30 ), CONVERT_CONC(  30 ) / 'PAN2            ',   30, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  31 ), CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), SPECIES_MOLWT(  31 ), CONVERT_CONC(  31 ) / 'xHO2            ',   31, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  32 ), CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), SPECIES_MOLWT(  32 ), CONVERT_CONC(  32 ) / 'yROOH           ',   32, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  33 ), CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), SPECIES_MOLWT(  33 ), CONVERT_CONC(  33 ) / 'xCCHO           ',   33, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  34 ), CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), SPECIES_MOLWT(  34 ), CONVERT_CONC(  34 ) / 'RCOOOH          ',   34, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  35 ), CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), SPECIES_MOLWT(  35 ), CONVERT_CONC(  35 ) / 'RCOOH           ',   35, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  36 ), CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), SPECIES_MOLWT(  36 ), CONVERT_CONC(  36 ) / 'BZCO3           ',   36, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  37 ), CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), SPECIES_MOLWT(  37 ), CONVERT_CONC(  37 ) / 'PBZN            ',   37, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  38 ), CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), SPECIES_MOLWT(  38 ), CONVERT_CONC(  38 ) / 'BZO             ',   38, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  39 ), CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), SPECIES_MOLWT(  39 ), CONVERT_CONC(  39 ) / 'MACO3           ',   39, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  40 ), CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), SPECIES_MOLWT(  40 ), CONVERT_CONC(  40 ) / 'MAPAN           ',   40, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  41 ), CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), SPECIES_MOLWT(  41 ), CONVERT_CONC(  41 ) / 'TBUO            ',   41, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  42 ), CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), SPECIES_MOLWT(  42 ), CONVERT_CONC(  42 ) / 'RNO3            ',   42, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  43 ), CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), SPECIES_MOLWT(  43 ), CONVERT_CONC(  43 ) / 'ACETONE         ',   43, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  44 ), CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), SPECIES_MOLWT(  44 ), CONVERT_CONC(  44 ) / 'NPHE            ',   44, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  45 ), CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), SPECIES_MOLWT(  45 ), CONVERT_CONC(  45 ) / 'CRES            ',   45, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  46 ), CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), SPECIES_MOLWT(  46 ), CONVERT_CONC(  46 ) / 'xOH             ',   46, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  47 ), CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), SPECIES_MOLWT(  47 ), CONVERT_CONC(  47 ) / 'xNO2            ',   47, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  48 ), CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), SPECIES_MOLWT(  48 ), CONVERT_CONC(  48 ) / 'xMEO2           ',   48, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  49 ), CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), SPECIES_MOLWT(  49 ), CONVERT_CONC(  49 ) / 'xMECO3          ',   49, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  50 ), CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), SPECIES_MOLWT(  50 ), CONVERT_CONC(  50 ) / 'xRCO3           ',   50, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  51 ), CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), SPECIES_MOLWT(  51 ), CONVERT_CONC(  51 ) / 'xMACO3          ',   51, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  52 ), CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), SPECIES_MOLWT(  52 ), CONVERT_CONC(  52 ) / 'xTBUO           ',   52, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  53 ), CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), SPECIES_MOLWT(  53 ), CONVERT_CONC(  53 ) / 'xCO             ',   53, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  54 ), CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), SPECIES_MOLWT(  54 ), CONVERT_CONC(  54 ) / 'CCHO            ',   54, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  55 ), CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), SPECIES_MOLWT(  55 ), CONVERT_CONC(  55 ) / 'RCHO            ',   55, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  56 ), CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), SPECIES_MOLWT(  56 ), CONVERT_CONC(  56 ) / 'xHCHO           ',   56, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  57 ), CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), SPECIES_MOLWT(  57 ), CONVERT_CONC(  57 ) / 'MEK             ',   57, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  58 ), CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), SPECIES_MOLWT(  58 ), CONVERT_CONC(  58 ) / 'zRNO3           ',   58, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  59 ), CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), SPECIES_MOLWT(  59 ), CONVERT_CONC(  59 ) / 'xRCHO           ',   59, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  60 ), CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), SPECIES_MOLWT(  60 ), CONVERT_CONC(  60 ) / 'HCOOH           ',   60, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  61 ), CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), SPECIES_MOLWT(  61 ), CONVERT_CONC(  61 ) / 'xMGLY           ',   61, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  62 ), CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), SPECIES_MOLWT(  62 ), CONVERT_CONC(  62 ) / 'xBACL           ',   62, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  63 ), CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), SPECIES_MOLWT(  63 ), CONVERT_CONC(  63 ) / 'ROOH            ',   63, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  64 ), CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), SPECIES_MOLWT(  64 ), CONVERT_CONC(  64 ) / 'xPROD2          ',   64, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  65 ), CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), SPECIES_MOLWT(  65 ), CONVERT_CONC(  65 ) / 'R6OOH           ',   65, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  66 ), CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), SPECIES_MOLWT(  66 ), CONVERT_CONC(  66 ) / 'PRD2            ',   66, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  67 ), CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), SPECIES_MOLWT(  67 ), CONVERT_CONC(  67 ) / 'yR6OOH          ',   67, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  68 ), CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), SPECIES_MOLWT(  68 ), CONVERT_CONC(  68 ) / 'RAOOH           ',   68, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  69 ), CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), SPECIES_MOLWT(  69 ), CONVERT_CONC(  69 ) / 'MGLY            ',   69, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  70 ), CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), SPECIES_MOLWT(  70 ), CONVERT_CONC(  70 ) / 'IPRD            ',   70, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  71 ), CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), SPECIES_MOLWT(  71 ), CONVERT_CONC(  71 ) / 'xGLY            ',   71, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  72 ), CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), SPECIES_MOLWT(  72 ), CONVERT_CONC(  72 ) / 'xMEK            ',   72, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  73 ), CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), SPECIES_MOLWT(  73 ), CONVERT_CONC(  73 ) / 'xAFG1           ',   73, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  74 ), CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), SPECIES_MOLWT(  74 ), CONVERT_CONC(  74 ) / 'xAFG2           ',   74, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  75 ), CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), SPECIES_MOLWT(  75 ), CONVERT_CONC(  75 ) / 'GLY             ',   75, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  76 ), CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), SPECIES_MOLWT(  76 ), CONVERT_CONC(  76 ) / 'AFG1            ',   76, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  77 ), CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), SPECIES_MOLWT(  77 ), CONVERT_CONC(  77 ) / 'AFG2            ',   77, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  78 ), CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), SPECIES_MOLWT(  78 ), CONVERT_CONC(  78 ) / 'HCOCO3          ',   78, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  79 ), CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), SPECIES_MOLWT(  79 ), CONVERT_CONC(  79 ) / 'BACL            ',   79, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  80 ), CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), SPECIES_MOLWT(  80 ), CONVERT_CONC(  80 ) / 'BALD            ',   80, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  81 ), CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), SPECIES_MOLWT(  81 ), CONVERT_CONC(  81 ) / 'AFG3            ',   81, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  82 ), CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), SPECIES_MOLWT(  82 ), CONVERT_CONC(  82 ) / 'xIPRD           ',   82, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  83 ), CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), SPECIES_MOLWT(  83 ), CONVERT_CONC(  83 ) / 'MACR            ',   83, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  84 ), CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), SPECIES_MOLWT(  84 ), CONVERT_CONC(  84 ) / 'MVK             ',   84, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  85 ), CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), SPECIES_MOLWT(  85 ), CONVERT_CONC(  85 ) / 'xHOCCHO         ',   85, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  86 ), CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), SPECIES_MOLWT(  86 ), CONVERT_CONC(  86 ) / 'xRNO3           ',   86, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  87 ), CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), SPECIES_MOLWT(  87 ), CONVERT_CONC(  87 ) / 'HOCCHO          ',   87, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  88 ), CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), SPECIES_MOLWT(  88 ), CONVERT_CONC(  88 ) / 'xACETONE        ',   88, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  89 ), CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), SPECIES_MOLWT(  89 ), CONVERT_CONC(  89 ) / 'ACROLEIN        ',   89, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  90 ), CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), SPECIES_MOLWT(  90 ), CONVERT_CONC(  90 ) / 'xBALD           ',   90, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  91 ), CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), SPECIES_MOLWT(  91 ), CONVERT_CONC(  91 ) / 'xAFG3           ',   91, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  92 ), CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), SPECIES_MOLWT(  92 ), CONVERT_CONC(  92 ) / 'xMACR           ',   92, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  93 ), CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), SPECIES_MOLWT(  93 ), CONVERT_CONC(  93 ) / 'xMVK            ',   93, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  94 ), CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), SPECIES_MOLWT(  94 ), CONVERT_CONC(  94 ) / 'yRAOOH          ',   94, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  95 ), CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), SPECIES_MOLWT(  95 ), CONVERT_CONC(  95 ) / 'xACROLEIN       ',   95, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  96 ), CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), SPECIES_MOLWT(  96 ), CONVERT_CONC(  96 ) / 'ETHENE          ',   96, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  97 ), CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), SPECIES_MOLWT(  97 ), CONVERT_CONC(  97 ) / 'PROPENE         ',   97, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  98 ), CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), SPECIES_MOLWT(  98 ), CONVERT_CONC(  98 ) / 'BUTADIENE13     ',   98, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  99 ), CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), SPECIES_MOLWT(  99 ), CONVERT_CONC(  99 ) / 'ISOPRENE        ',   99, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 100 ), CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), SPECIES_MOLWT( 100 ), CONVERT_CONC( 100 ) / 'APIN            ',  100, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 101 ), CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), SPECIES_MOLWT( 101 ), CONVERT_CONC( 101 ) / 'TRPRXN          ',  101, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 102 ), CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), SPECIES_MOLWT( 102 ), CONVERT_CONC( 102 ) / 'ACETYLENE       ',  102, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 103 ), CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), SPECIES_MOLWT( 103 ), CONVERT_CONC( 103 ) / 'BENZENE         ',  103, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 104 ), CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), SPECIES_MOLWT( 104 ), CONVERT_CONC( 104 ) / 'BENZRO2         ',  104, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 105 ), CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), SPECIES_MOLWT( 105 ), CONVERT_CONC( 105 ) / 'TOLUENE         ',  105, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 106 ), CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), SPECIES_MOLWT( 106 ), CONVERT_CONC( 106 ) / 'TOLRO2          ',  106, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 107 ), CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), SPECIES_MOLWT( 107 ), CONVERT_CONC( 107 ) / 'MXYL            ',  107, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 108 ), CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), SPECIES_MOLWT( 108 ), CONVERT_CONC( 108 ) / 'XYLRO2          ',  108, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 109 ), CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), SPECIES_MOLWT( 109 ), CONVERT_CONC( 109 ) / 'OXYL            ',  109, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 110 ), CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), SPECIES_MOLWT( 110 ), CONVERT_CONC( 110 ) / 'PXYL            ',  110, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 111 ), CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), SPECIES_MOLWT( 111 ), CONVERT_CONC( 111 ) / 'TRIMETH_BENZ124 ',  111, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 112 ), CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), SPECIES_MOLWT( 112 ), CONVERT_CONC( 112 ) / 'ETOH            ',  112, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 113 ), CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), SPECIES_MOLWT( 113 ), CONVERT_CONC( 113 ) / 'ALK1            ',  113, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 114 ), CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), SPECIES_MOLWT( 114 ), CONVERT_CONC( 114 ) / 'ALK2            ',  114, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 115 ), CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), SPECIES_MOLWT( 115 ), CONVERT_CONC( 115 ) / 'ALK3            ',  115, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 116 ), CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), SPECIES_MOLWT( 116 ), CONVERT_CONC( 116 ) / 'ALK4            ',  116, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 117 ), CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), SPECIES_MOLWT( 117 ), CONVERT_CONC( 117 ) / 'ALK5            ',  117, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 118 ), CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), SPECIES_MOLWT( 118 ), CONVERT_CONC( 118 ) / 'SOAALK          ',  118, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 119 ), CGRID_INDEX( 119 ), SPECIES_TYPE( 119 ), SPECIES_MOLWT( 119 ), CONVERT_CONC( 119 ) / 'ALKRXN          ',  119, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 120 ), CGRID_INDEX( 120 ), SPECIES_TYPE( 120 ), SPECIES_MOLWT( 120 ), CONVERT_CONC( 120 ) / 'OLE1            ',  120, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 121 ), CGRID_INDEX( 121 ), SPECIES_TYPE( 121 ), SPECIES_MOLWT( 121 ), CONVERT_CONC( 121 ) / 'OLE2            ',  121, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 122 ), CGRID_INDEX( 122 ), SPECIES_TYPE( 122 ), SPECIES_MOLWT( 122 ), CONVERT_CONC( 122 ) / 'ARO1            ',  122, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 123 ), CGRID_INDEX( 123 ), SPECIES_TYPE( 123 ), SPECIES_MOLWT( 123 ), CONVERT_CONC( 123 ) / 'ARO2MN          ',  123, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 124 ), CGRID_INDEX( 124 ), SPECIES_TYPE( 124 ), SPECIES_MOLWT( 124 ), CONVERT_CONC( 124 ) / 'NAPHTHAL        ',  124, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 125 ), CGRID_INDEX( 125 ), SPECIES_TYPE( 125 ), SPECIES_MOLWT( 125 ), CONVERT_CONC( 125 ) / 'PAHRO2          ',  125, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 126 ), CGRID_INDEX( 126 ), SPECIES_TYPE( 126 ), SPECIES_MOLWT( 126 ), CONVERT_CONC( 126 ) / 'TERP            ',  126, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 127 ), CGRID_INDEX( 127 ), SPECIES_TYPE( 127 ), SPECIES_MOLWT( 127 ), CONVERT_CONC( 127 ) / 'SESQ            ',  127, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 128 ), CGRID_INDEX( 128 ), SPECIES_TYPE( 128 ), SPECIES_MOLWT( 128 ), CONVERT_CONC( 128 ) / 'SESQRXN         ',  128, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 129 ), CGRID_INDEX( 129 ), SPECIES_TYPE( 129 ), SPECIES_MOLWT( 129 ), CONVERT_CONC( 129 ) / 'CL2             ',  129, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 130 ), CGRID_INDEX( 130 ), SPECIES_TYPE( 130 ), SPECIES_MOLWT( 130 ), CONVERT_CONC( 130 ) / 'CL              ',  130, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 131 ), CGRID_INDEX( 131 ), SPECIES_TYPE( 131 ), SPECIES_MOLWT( 131 ), CONVERT_CONC( 131 ) / 'CLNO            ',  131, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 132 ), CGRID_INDEX( 132 ), SPECIES_TYPE( 132 ), SPECIES_MOLWT( 132 ), CONVERT_CONC( 132 ) / 'CLONO           ',  132, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 133 ), CGRID_INDEX( 133 ), SPECIES_TYPE( 133 ), SPECIES_MOLWT( 133 ), CONVERT_CONC( 133 ) / 'CLNO2           ',  133, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 134 ), CGRID_INDEX( 134 ), SPECIES_TYPE( 134 ), SPECIES_MOLWT( 134 ), CONVERT_CONC( 134 ) / 'HCL             ',  134, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 135 ), CGRID_INDEX( 135 ), SPECIES_TYPE( 135 ), SPECIES_MOLWT( 135 ), CONVERT_CONC( 135 ) / 'CLO             ',  135, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 136 ), CGRID_INDEX( 136 ), SPECIES_TYPE( 136 ), SPECIES_MOLWT( 136 ), CONVERT_CONC( 136 ) / 'CLONO2          ',  136, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 137 ), CGRID_INDEX( 137 ), SPECIES_TYPE( 137 ), SPECIES_MOLWT( 137 ), CONVERT_CONC( 137 ) / 'HOCL            ',  137, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 138 ), CGRID_INDEX( 138 ), SPECIES_TYPE( 138 ), SPECIES_MOLWT( 138 ), CONVERT_CONC( 138 ) / 'xCL             ',  138, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 139 ), CGRID_INDEX( 139 ), SPECIES_TYPE( 139 ), SPECIES_MOLWT( 139 ), CONVERT_CONC( 139 ) / 'xCLCCHO         ',  139, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 140 ), CGRID_INDEX( 140 ), SPECIES_TYPE( 140 ), SPECIES_MOLWT( 140 ), CONVERT_CONC( 140 ) / 'xCLACET         ',  140, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 141 ), CGRID_INDEX( 141 ), SPECIES_TYPE( 141 ), SPECIES_MOLWT( 141 ), CONVERT_CONC( 141 ) / 'CLCCHO          ',  141, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 142 ), CGRID_INDEX( 142 ), SPECIES_TYPE( 142 ), SPECIES_MOLWT( 142 ), CONVERT_CONC( 142 ) / 'CLACET          ',  142, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 143 ), CGRID_INDEX( 143 ), SPECIES_TYPE( 143 ), SPECIES_MOLWT( 143 ), CONVERT_CONC( 143 ) / 'CLCHO           ',  143, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 144 ), CGRID_INDEX( 144 ), SPECIES_TYPE( 144 ), SPECIES_MOLWT( 144 ), CONVERT_CONC( 144 ) / 'BNZNRXN         ',  144, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 145 ), CGRID_INDEX( 145 ), SPECIES_TYPE( 145 ), SPECIES_MOLWT( 145 ), CONVERT_CONC( 145 ) / 'BNZHRXN         ',  145, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 146 ), CGRID_INDEX( 146 ), SPECIES_TYPE( 146 ), SPECIES_MOLWT( 146 ), CONVERT_CONC( 146 ) / 'XYLNRXN         ',  146, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 147 ), CGRID_INDEX( 147 ), SPECIES_TYPE( 147 ), SPECIES_MOLWT( 147 ), CONVERT_CONC( 147 ) / 'XYLHRXN         ',  147, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 148 ), CGRID_INDEX( 148 ), SPECIES_TYPE( 148 ), SPECIES_MOLWT( 148 ), CONVERT_CONC( 148 ) / 'TOLNRXN         ',  148, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 149 ), CGRID_INDEX( 149 ), SPECIES_TYPE( 149 ), SPECIES_MOLWT( 149 ), CONVERT_CONC( 149 ) / 'TOLHRXN         ',  149, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 150 ), CGRID_INDEX( 150 ), SPECIES_TYPE( 150 ), SPECIES_MOLWT( 150 ), CONVERT_CONC( 150 ) / 'PAHNRXN         ',  150, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 151 ), CGRID_INDEX( 151 ), SPECIES_TYPE( 151 ), SPECIES_MOLWT( 151 ), CONVERT_CONC( 151 ) / 'PAHHRXN         ',  151, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 152 ), CGRID_INDEX( 152 ), SPECIES_TYPE( 152 ), SPECIES_MOLWT( 152 ), CONVERT_CONC( 152 ) / 'HCHO_PRIMARY    ',  152, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 153 ), CGRID_INDEX( 153 ), SPECIES_TYPE( 153 ), SPECIES_MOLWT( 153 ), CONVERT_CONC( 153 ) / 'CCHO_PRIMARY    ',  153, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 154 ), CGRID_INDEX( 154 ), SPECIES_TYPE( 154 ), SPECIES_MOLWT( 154 ), CONVERT_CONC( 154 ) / 'ACROLEIN_PRIMARY',  154, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 155 ), CGRID_INDEX( 155 ), SPECIES_TYPE( 155 ), SPECIES_MOLWT( 155 ), CONVERT_CONC( 155 ) / 'ISOPO2          ',  155, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 156 ), CGRID_INDEX( 156 ), SPECIES_TYPE( 156 ), SPECIES_MOLWT( 156 ), CONVERT_CONC( 156 ) / 'ISOPRXN         ',  156, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 157 ), CGRID_INDEX( 157 ), SPECIES_TYPE( 157 ), SPECIES_MOLWT( 157 ), CONVERT_CONC( 157 ) / 'ISOPND          ',  157, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 158 ), CGRID_INDEX( 158 ), SPECIES_TYPE( 158 ), SPECIES_MOLWT( 158 ), CONVERT_CONC( 158 ) / 'ISOPNB          ',  158, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 159 ), CGRID_INDEX( 159 ), SPECIES_TYPE( 159 ), SPECIES_MOLWT( 159 ), CONVERT_CONC( 159 ) / 'HC5             ',  159, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 160 ), CGRID_INDEX( 160 ), SPECIES_TYPE( 160 ), SPECIES_MOLWT( 160 ), CONVERT_CONC( 160 ) / 'DIBOO           ',  160, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 161 ), CGRID_INDEX( 161 ), SPECIES_TYPE( 161 ), SPECIES_MOLWT( 161 ), CONVERT_CONC( 161 ) / 'ISOPOOH         ',  161, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 162 ), CGRID_INDEX( 162 ), SPECIES_TYPE( 162 ), SPECIES_MOLWT( 162 ), CONVERT_CONC( 162 ) / 'HPALD           ',  162, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 163 ), CGRID_INDEX( 163 ), SPECIES_TYPE( 163 ), SPECIES_MOLWT( 163 ), CONVERT_CONC( 163 ) / 'HACET           ',  163, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 164 ), CGRID_INDEX( 164 ), SPECIES_TYPE( 164 ), SPECIES_MOLWT( 164 ), CONVERT_CONC( 164 ) / 'NISOPO2         ',  164, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 165 ), CGRID_INDEX( 165 ), SPECIES_TYPE( 165 ), SPECIES_MOLWT( 165 ), CONVERT_CONC( 165 ) / 'NIT1            ',  165, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 166 ), CGRID_INDEX( 166 ), SPECIES_TYPE( 166 ), SPECIES_MOLWT( 166 ), CONVERT_CONC( 166 ) / 'NISOPOOH        ',  166, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 167 ), CGRID_INDEX( 167 ), SPECIES_TYPE( 167 ), SPECIES_MOLWT( 167 ), CONVERT_CONC( 167 ) / 'HC5OO           ',  167, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 168 ), CGRID_INDEX( 168 ), SPECIES_TYPE( 168 ), SPECIES_MOLWT( 168 ), CONVERT_CONC( 168 ) / 'DHMOB           ',  168, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 169 ), CGRID_INDEX( 169 ), SPECIES_TYPE( 169 ), SPECIES_MOLWT( 169 ), CONVERT_CONC( 169 ) / 'ISOPNOOD        ',  169, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 170 ), CGRID_INDEX( 170 ), SPECIES_TYPE( 170 ), SPECIES_MOLWT( 170 ), CONVERT_CONC( 170 ) / 'PROPNN          ',  170, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 171 ), CGRID_INDEX( 171 ), SPECIES_TYPE( 171 ), SPECIES_MOLWT( 171 ), CONVERT_CONC( 171 ) / 'MVKN            ',  171, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 172 ), CGRID_INDEX( 172 ), SPECIES_TYPE( 172 ), SPECIES_MOLWT( 172 ), CONVERT_CONC( 172 ) / 'ETHLN           ',  172, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 173 ), CGRID_INDEX( 173 ), SPECIES_TYPE( 173 ), SPECIES_MOLWT( 173 ), CONVERT_CONC( 173 ) / 'RNO3I           ',  173, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 174 ), CGRID_INDEX( 174 ), SPECIES_TYPE( 174 ), SPECIES_MOLWT( 174 ), CONVERT_CONC( 174 ) / 'ISOPNOOB        ',  174, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 175 ), CGRID_INDEX( 175 ), SPECIES_TYPE( 175 ), SPECIES_MOLWT( 175 ), CONVERT_CONC( 175 ) / 'MACRN           ',  175, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 176 ), CGRID_INDEX( 176 ), SPECIES_TYPE( 176 ), SPECIES_MOLWT( 176 ), CONVERT_CONC( 176 ) / 'NIT1NO3OOA      ',  176, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 177 ), CGRID_INDEX( 177 ), SPECIES_TYPE( 177 ), SPECIES_MOLWT( 177 ), CONVERT_CONC( 177 ) / 'NIT1NO3OOB      ',  177, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 178 ), CGRID_INDEX( 178 ), SPECIES_TYPE( 178 ), SPECIES_MOLWT( 178 ), CONVERT_CONC( 178 ) / 'PROPNNB         ',  178, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 179 ), CGRID_INDEX( 179 ), SPECIES_TYPE( 179 ), SPECIES_MOLWT( 179 ), CONVERT_CONC( 179 ) / 'NIT1OHOO        ',  179, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 180 ), CGRID_INDEX( 180 ), SPECIES_TYPE( 180 ), SPECIES_MOLWT( 180 ), CONVERT_CONC( 180 ) / 'MVKOO           ',  180, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 181 ), CGRID_INDEX( 181 ), SPECIES_TYPE( 181 ), SPECIES_MOLWT( 181 ), CONVERT_CONC( 181 ) / 'MACROO          ',  181, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 182 ), CGRID_INDEX( 182 ), SPECIES_TYPE( 182 ), SPECIES_MOLWT( 182 ), CONVERT_CONC( 182 ) / 'PYRUACD         ',  182, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 183 ), CGRID_INDEX( 183 ), SPECIES_TYPE( 183 ), SPECIES_MOLWT( 183 ), CONVERT_CONC( 183 ) / 'IEPOX           ',  183, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 184 ), CGRID_INDEX( 184 ), SPECIES_TYPE( 184 ), SPECIES_MOLWT( 184 ), CONVERT_CONC( 184 ) / 'IEPOXOO         ',  184, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 185 ), CGRID_INDEX( 185 ), SPECIES_TYPE( 185 ), SPECIES_MOLWT( 185 ), CONVERT_CONC( 185 ) / 'IMACO3          ',  185, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 186 ), CGRID_INDEX( 186 ), SPECIES_TYPE( 186 ), SPECIES_MOLWT( 186 ), CONVERT_CONC( 186 ) / 'IMPAA           ',  186, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 187 ), CGRID_INDEX( 187 ), SPECIES_TYPE( 187 ), SPECIES_MOLWT( 187 ), CONVERT_CONC( 187 ) / 'IMAPAN          ',  187, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 188 ), CGRID_INDEX( 188 ), SPECIES_TYPE( 188 ), SPECIES_MOLWT( 188 ), CONVERT_CONC( 188 ) / 'IMAE            ',  188, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 189 ), CGRID_INDEX( 189 ), SPECIES_TYPE( 189 ), SPECIES_MOLWT( 189 ), CONVERT_CONC( 189 ) / 'IHMML           ',  189, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 190 ), CGRID_INDEX( 190 ), SPECIES_TYPE( 190 ), SPECIES_MOLWT( 190 ), CONVERT_CONC( 190 ) / 'H2NO3PIJ        ',  190, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 191 ), CGRID_INDEX( 191 ), SPECIES_TYPE( 191 ), SPECIES_MOLWT( 191 ), CONVERT_CONC( 191 ) / 'H2NO3PK         ',  191, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 192 ), CGRID_INDEX( 192 ), SPECIES_TYPE( 192 ), SPECIES_MOLWT( 192 ), CONVERT_CONC( 192 ) / 'ACLI            ',  192, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 193 ), CGRID_INDEX( 193 ), SPECIES_TYPE( 193 ), SPECIES_MOLWT( 193 ), CONVERT_CONC( 193 ) / 'ACLJ            ',  193, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 194 ), CGRID_INDEX( 194 ), SPECIES_TYPE( 194 ), SPECIES_MOLWT( 194 ), CONVERT_CONC( 194 ) / 'ACLK            ',  194, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 195 ), CGRID_INDEX( 195 ), SPECIES_TYPE( 195 ), SPECIES_MOLWT( 195 ), CONVERT_CONC( 195 ) / 'IEPOXP          ',  195, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 196 ), CGRID_INDEX( 196 ), SPECIES_TYPE( 196 ), SPECIES_MOLWT( 196 ), CONVERT_CONC( 196 ) / 'IMAEP           ',  196, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 197 ), CGRID_INDEX( 197 ), SPECIES_TYPE( 197 ), SPECIES_MOLWT( 197 ), CONVERT_CONC( 197 ) / 'IHMMLP          ',  197, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 198 ), CGRID_INDEX( 198 ), SPECIES_TYPE( 198 ), SPECIES_MOLWT( 198 ), CONVERT_CONC( 198 ) / 'AIETETJ         ',  198, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 199 ), CGRID_INDEX( 199 ), SPECIES_TYPE( 199 ), SPECIES_MOLWT( 199 ), CONVERT_CONC( 199 ) / 'AIEOSJ          ',  199, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 200 ), CGRID_INDEX( 200 ), SPECIES_TYPE( 200 ), SPECIES_MOLWT( 200 ), CONVERT_CONC( 200 ) / 'ADIMJ           ',  200, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 201 ), CGRID_INDEX( 201 ), SPECIES_TYPE( 201 ), SPECIES_MOLWT( 201 ), CONVERT_CONC( 201 ) / 'AIMGAJ          ',  201, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 202 ), CGRID_INDEX( 202 ), SPECIES_TYPE( 202 ), SPECIES_MOLWT( 202 ), CONVERT_CONC( 202 ) / 'AIMOSJ          ',  202, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 203 ), CGRID_INDEX( 203 ), SPECIES_TYPE( 203 ), SPECIES_MOLWT( 203 ), CONVERT_CONC( 203 ) / 'AALK1J          ',  203, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 204 ), CGRID_INDEX( 204 ), SPECIES_TYPE( 204 ), SPECIES_MOLWT( 204 ), CONVERT_CONC( 204 ) / 'AOLGAJ          ',  204, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 205 ), CGRID_INDEX( 205 ), SPECIES_TYPE( 205 ), SPECIES_MOLWT( 205 ), CONVERT_CONC( 205 ) / 'AALK2J          ',  205, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 206 ), CGRID_INDEX( 206 ), SPECIES_TYPE( 206 ), SPECIES_MOLWT( 206 ), CONVERT_CONC( 206 ) / 'AXYL1J          ',  206, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 207 ), CGRID_INDEX( 207 ), SPECIES_TYPE( 207 ), SPECIES_MOLWT( 207 ), CONVERT_CONC( 207 ) / 'AXYL2J          ',  207, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 208 ), CGRID_INDEX( 208 ), SPECIES_TYPE( 208 ), SPECIES_MOLWT( 208 ), CONVERT_CONC( 208 ) / 'ATOL1J          ',  208, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 209 ), CGRID_INDEX( 209 ), SPECIES_TYPE( 209 ), SPECIES_MOLWT( 209 ), CONVERT_CONC( 209 ) / 'ATOL2J          ',  209, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 210 ), CGRID_INDEX( 210 ), SPECIES_TYPE( 210 ), SPECIES_MOLWT( 210 ), CONVERT_CONC( 210 ) / 'ABNZ1J          ',  210, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 211 ), CGRID_INDEX( 211 ), SPECIES_TYPE( 211 ), SPECIES_MOLWT( 211 ), CONVERT_CONC( 211 ) / 'ABNZ2J          ',  211, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 212 ), CGRID_INDEX( 212 ), SPECIES_TYPE( 212 ), SPECIES_MOLWT( 212 ), CONVERT_CONC( 212 ) / 'ATRP1J          ',  212, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 213 ), CGRID_INDEX( 213 ), SPECIES_TYPE( 213 ), SPECIES_MOLWT( 213 ), CONVERT_CONC( 213 ) / 'AOLGBJ          ',  213, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 214 ), CGRID_INDEX( 214 ), SPECIES_TYPE( 214 ), SPECIES_MOLWT( 214 ), CONVERT_CONC( 214 ) / 'ATRP2J          ',  214, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 215 ), CGRID_INDEX( 215 ), SPECIES_TYPE( 215 ), SPECIES_MOLWT( 215 ), CONVERT_CONC( 215 ) / 'ASQTJ           ',  215, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 216 ), CGRID_INDEX( 216 ), SPECIES_TYPE( 216 ), SPECIES_MOLWT( 216 ), CONVERT_CONC( 216 ) / 'APOCI           ',  216, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 217 ), CGRID_INDEX( 217 ), SPECIES_TYPE( 217 ), SPECIES_MOLWT( 217 ), CONVERT_CONC( 217 ) / 'APNCOMI         ',  217, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 218 ), CGRID_INDEX( 218 ), SPECIES_TYPE( 218 ), SPECIES_MOLWT( 218 ), CONVERT_CONC( 218 ) / 'APOCJ           ',  218, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC( 219 ), CGRID_INDEX( 219 ), SPECIES_TYPE( 219 ), SPECIES_MOLWT( 219 ), CONVERT_CONC( 219 ) / 'APNCOMJ         ',  219, 'GC',    1.00, F /

      INTEGER, PARAMETER :: N_ACT_SP = 219

      INTEGER, PARAMETER :: NRXNS = 883

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
     &      6,    6,    6,    6,    6,    6,    3,    0,    1,    3, & ! 9   
     &      3,    6,    1,    6,    6,    6,    6,    6,    6,    6, & ! O   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 1   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 2   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 3   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 4   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 5   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 6   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 7   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 8   
     &      6,    6,    6,    0,    0,    3,    3,    3,    0,    3, & ! 9   
     &      3,    0,    3,    4,    0,    4,    0,    3,    1,    3, & ! O   
     &      1,    3,    0,    1,    0,    1,    0,    1,    0,    0, & ! 1   
     &      0,    3,    3,    0,    1,    3,    0,    3,    1,    1, & ! 2   
     &      0,    0,    1,    0,    3,    1,    1,    0,    1,    1, & ! 3   
     &      0,    1,    1,    3,    1,    3,    1,    0,    1,    1, & ! 4   
     &      1,    0,    1,    0,    1,    0,    0,    6,    1,    3, & ! 5   
     &      1,    1,    0,    1,    0,    1,    0,    6,    6,    6, & ! 6   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 7   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 8   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 9   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! O   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 1   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 2   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 3   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 4   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 5   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 6   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 7   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 8   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 9   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! O   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 1   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 2   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 3   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 4   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 5   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 6   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 7   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 8   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 9   
     &      3,   10,    3,    3,    3,    3,    3,    3,    3,    3, & ! O   
     &      3,    1,    3,    3,    1,    3,    3,    3,    1,   10, & ! 1   
     &      3,    3,    3,    1,    1,    1,    1,    4,    4,    4, & ! 2   
     &      3,    3,    3,    3,    3,    3,    3,    3,    3,    3, & ! 3   
     &      3,    3,    1,    1,    1,    3,    3,    3,    1,    6, & ! 4   
     &      6,    6,    6,    0,    2,    0,   10,   10,    0,    0, & ! 5   
     &      2,    2,    3,    1,    3,   10,    0,    0,   10,    3, & ! 6   
     &      3,    0,    3,    3,    3,    3,    1,    1,    1,    3, & ! 7   
     &      1,    1,    1,    3,    1,    1,    1,    1,    1,    1, & ! 8   
     &      1,    1,    1,    0,    1,    1,    0,    6,    6,    6, & ! 9   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! O   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 1   
     &      6,    6,    6,    6,    6,    6,    6,    3,   10,    1, & ! 2   
     &      1,    1,    1,   10,    1,    1,    1,    1,    1,    3, & ! 3   
     &      3,    3,    1,    1,    1,    1,    1,    1,    1,    1, & ! 4   
     &      1,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 5   
     &      0,    0,    3,    3,    3,    3,    0,    3,    1,    1, & ! 6   
     &      3,    1,    1,    0,    1,    3,    3,    3,    1,    1, & ! 7   
     &      1,    3,    3,    0,    1,    3,    1,    3,    3,    1, & ! 8   
     &      1,    1,    3,    3,    3,    3,    1,    1,    3,    3, & ! 9   
     &      3,    3,    3,    1,    1,    3,    1,    3,    3,    3, & ! O   
     &      1,    1,    3,    1,    3,    1,    6,    6,    6,    6, & ! 1   
     &      6,    6,    1,    3,    3,    1,    1,    3,    3,    3, & ! 2   
     &      3,    3,    1,    1,    3,    3,    3,    1,    1,    3, & ! 3   
     &      3,    3,    3,    1,    1,    3,    3,    3,    1,    1, & ! 4   
     &      3,    3,    6,    1,    6,    6,    6,    6,    6,    6, & ! 5   
     &      6,    1,    1,    3,    0,    3,    0,    1,    1,    0, & ! 6   
     &      0,    3,    0,    3,    0,    1,    0,    3,    3,    3, & ! 7   
     &      3,    3,    1,    1,    3,    0,    1,    1,    3,    3, & ! 8   
     &      3,    0,    1,    3,    6,    1,    6,    6,    6,    6, & ! 9   
     &      6,    6,    6,    6,    3,    6,    3,    0,    6,    6, & ! O   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 1   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 2   
     &      6,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 3   
     &      6,    6,    6,    1,    1,    1,    1,   -1,   -1,   -1, & ! 4   
     &     -1,   -1,   -1,   -1,   -1,   -1,   12,   -1,   -1,   -1, & ! 5   
     &     -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &     -1,    1,   -1/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    2,    2,    0,    0,    0,    2,    0, & ! 9   
     &      0,    2,    0,    0,    2,    0,    2,    0,    0,    0, & ! O   
     &      0,    0,    2,    0,    2,    0,    2,    0,    2,    2, & ! 1   
     &      2,    0,    0,    2,    0,    0,    2,    0,    0,    0, & ! 2   
     &      2,    2,    0,    2,    0,    0,    0,    2,    0,    0, & ! 3   
     &      2,    0,    0,    0,    0,    0,    0,    2,    0,    0, & ! 4   
     &      0,    2,    0,    2,    0,    2,    2,    0,    0,    0, & ! 5   
     &      0,    0,    2,    0,    2,    0,    2,    0,    0,    0, & ! 6   
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
     &     64,    1,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    1, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    2,    4,    2,    1,    1,    2,    2, & ! 5   
     &      0,    0,    0,    0,    0,    1,    2,    2,    1,    0, & ! 6   
     &      0,    2,    0,    0,  128,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    2,    0,    0,    2,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,   64,    1,    0, & ! 2   
     &      0,    0,    0,    1,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      2,    2,    0,    0,    0,    0,    2,    0,    0,    0, & ! 6   
     &      0,    0,    0,    2,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    2,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    2,    0,    2,    0,    0,    2, & ! 6   
     &      2,    0,    2,    0,    2,    0,    2,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    2,    0,    0,    0,    0, & ! 8   
     &      0,    2,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    2,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    0,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      1,    0,    1/     !  8   

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
     &      2,    2,    2,    2,    2,    2,    1,    1,    2,    1, & ! 9   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    1,    1,    2,    2,    2,    1,    2, & ! 9   
     &      2,    1,    2,    2,    1,    2,    1,    2,    2,    2, & ! O   
     &      2,    2,    1,    2,    1,    2,    1,    2,    1,    1, & ! 1   
     &      1,    2,    2,    1,    2,    2,    1,    2,    2,    2, & ! 2   
     &      1,    1,    2,    1,    2,    2,    2,    1,    2,    2, & ! 3   
     &      1,    2,    2,    2,    2,    2,    2,    1,    2,    2, & ! 4   
     &      2,    1,    2,    1,    2,    1,    1,    2,    2,    2, & ! 5   
     &      2,    2,    1,    2,    1,    2,    1,    2,    2,    2, & ! 6   
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
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    1,    3,    1,    2,    2,    1,    1, & ! 5   
     &      2,    2,    2,    2,    2,    2,    1,    1,    1,    2, & ! 6   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    1,    2,    2,    1,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      1,    1,    2,    2,    2,    2,    1,    2,    2,    2, & ! 6   
     &      2,    2,    2,    1,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    1,    2,    1,    2,    2,    1, & ! 6   
     &      1,    2,    1,    2,    1,    2,    1,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    1,    2,    2,    2,    2, & ! 8   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    1,    2,    1,    1,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    1,    1,    1, & ! 4   
     &      1,    1,    1,    2,    2,    2,    1,    1,    1,    1, & ! 5   
     &      1,    1,    2,    2,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    2, & ! 7   
     &      2,    2,    2/     !  8   

      INTEGER, PARAMETER :: KTN1 = 148
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     13,   14,   26,   39,   42,   49,   54,   55,   56,   84, & ! O   
     &     99,  103,  209,  211,  214,  216,  218,  225,  229,  230, & ! 1   
     &    233,  236,  237,  239,  240,  242,  243,  245,  247,  249, & ! 2   
     &    250,  251,  253,  255,  259,  261,  262,  264,  266,  512, & ! 3   
     &    515,  519,  524,  525,  526,  527,  543,  544,  545,  549, & ! 4   
     &    564,  577,  578,  579,  581,  582,  583,  585,  586,  587, & ! 5   
     &    588,  589,  590,  591,  592,  593,  595,  596,  630,  631, & ! 6   
     &    632,  633,  635,  636,  637,  638,  639,  643,  644,  645, & ! 7   
     &    646,  647,  648,  649,  650,  651,  669,  670,  672,  673, & ! 8   
     &    675,  679,  680,  681,  685,  687,  690,  691,  692,  697, & ! 9   
     &    698,  704,  705,  707,  711,  712,  714,  716,  723,  726, & ! O   
     &    727,  733,  734,  738,  739,  744,  745,  749,  750,  754, & ! 1   
     &    762,  763,  768,  769,  776,  783,  784,  787,  788,  793, & ! 2   
     &    796,  844,  845,  846,  847,  869,  870,  871,  872,  873, & ! 3   
     &    874,  875,  876,  877,  878,  879,  880,  882/     !  4   

      INTEGER, PARAMETER :: KTN2 =   4
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &      2,  555,  561,  562/

      INTEGER, PARAMETER :: KTN3 = 159
      INTEGER            :: KRX3( KTN3 )

      DATA ( KRX3( IRXXN ), IRXXN = 1, KTN3 ) / & 
     &      3,    5,    7,    8,    9,   10,   15,   20,   21,   24, & ! O   
     &     30,   31,   35,   40,   43,   45,   46,   51,   52,   53, & ! 1   
     &     66,   67,   69,   70,   72,   74,   76,   85,   97,  100, & ! 2   
     &    101,  196,  197,  198,  200,  201,  203,  208,  210,  212, & ! 3   
     &    222,  223,  226,  228,  235,  244,  246,  260,  501,  503, & ! 4   
     &    504,  505,  506,  507,  508,  509,  510,  511,  513,  514, & ! 5   
     &    516,  517,  518,  521,  522,  523,  531,  532,  533,  534, & ! 6   
     &    535,  536,  537,  538,  539,  540,  541,  542,  546,  547, & ! 7   
     &    548,  563,  565,  570,  571,  573,  574,  575,  576,  580, & ! 8   
     &    584,  628,  640,  641,  642,  663,  664,  665,  666,  668, & ! 9   
     &    671,  676,  677,  678,  682,  683,  686,  688,  689,  693, & ! O   
     &    694,  695,  696,  699,  700,  701,  702,  703,  706,  708, & ! 1   
     &    709,  710,  713,  715,  724,  725,  728,  729,  730,  731, & ! 2   
     &    732,  735,  736,  737,  740,  741,  742,  743,  746,  747, & ! 3   
     &    748,  751,  752,  764,  766,  772,  774,  778,  779,  780, & ! 4   
     &    781,  782,  785,  789,  790,  791,  794,  805,  807/     !5   

      INTEGER, PARAMETER :: KTN4 =  10
      INTEGER            :: KRX4( KTN4 )

      DATA ( KRX4( IRXXN ), IRXXN = 1, KTN4 ) / & 
     &     36,   47,   48,   50,   73,  204,  206,  528,  529,  530/

      INTEGER, PARAMETER :: KTN5 =   0
      INTEGER            :: KRX5( 1 )

      DATA   KRX5( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN6 = 452
      INTEGER            :: KRX6( KTN6 )

      DATA ( KRX6( IRXXN ), IRXXN = 1, KTN6 ) / & 
     &     57,   58,   59,   60,   61,   62,   68,   71,   77,   78, & ! O   
     &     79,   80,   81,   82,   83,   87,   88,   89,   90,   91, & ! 1   
     &     92,   93,   94,   95,   96,  102,  104,  105,  106,  107, & ! 2   
     &    108,  109,  110,  111,  112,  113,  114,  115,  116,  117, & ! 3   
     &    118,  119,  120,  121,  122,  123,  124,  125,  126,  127, & ! 4   
     &    128,  129,  130,  131,  132,  133,  134,  135,  136,  137, & ! 5   
     &    138,  139,  140,  141,  142,  143,  144,  145,  146,  147, & ! 6   
     &    148,  149,  150,  151,  152,  153,  154,  155,  156,  157, & ! 7   
     &    158,  159,  160,  161,  162,  163,  164,  165,  166,  167, & ! 8   
     &    168,  169,  170,  171,  172,  173,  174,  175,  176,  177, & ! 9   
     &    178,  179,  180,  181,  182,  183,  184,  185,  186,  187, & ! O   
     &    188,  189,  190,  191,  192,  193,  258,  268,  269,  270, & ! 1   
     &    271,  272,  273,  274,  275,  276,  277,  278,  279,  280, & ! 2   
     &    281,  282,  283,  284,  285,  286,  287,  288,  289,  290, & ! 3   
     &    291,  292,  293,  294,  295,  296,  297,  298,  299,  300, & ! 4   
     &    301,  302,  303,  304,  305,  306,  307,  308,  309,  310, & ! 5   
     &    311,  312,  313,  314,  315,  316,  317,  318,  319,  320, & ! 6   
     &    321,  322,  323,  324,  325,  326,  327,  328,  329,  330, & ! 7   
     &    331,  332,  333,  334,  335,  336,  337,  338,  339,  340, & ! 8   
     &    341,  342,  343,  344,  345,  346,  347,  348,  349,  350, & ! 9   
     &    351,  352,  353,  354,  355,  356,  357,  358,  359,  360, & ! O   
     &    361,  362,  363,  364,  365,  366,  367,  368,  369,  370, & ! 1   
     &    371,  372,  373,  374,  375,  376,  377,  378,  379,  380, & ! 2   
     &    381,  382,  383,  384,  385,  386,  387,  388,  389,  390, & ! 3   
     &    391,  392,  393,  394,  395,  396,  397,  398,  399,  400, & ! 4   
     &    401,  402,  403,  404,  405,  406,  407,  408,  409,  410, & ! 5   
     &    411,  412,  413,  414,  415,  416,  417,  418,  419,  420, & ! 6   
     &    421,  422,  423,  424,  425,  426,  427,  428,  429,  430, & ! 7   
     &    431,  432,  433,  434,  435,  436,  437,  438,  439,  440, & ! 8   
     &    441,  442,  443,  444,  445,  446,  447,  448,  449,  450, & ! 9   
     &    451,  452,  453,  454,  455,  456,  457,  458,  459,  460, & ! O   
     &    461,  462,  463,  464,  465,  466,  467,  468,  469,  470, & ! 1   
     &    471,  472,  473,  474,  475,  476,  477,  478,  479,  480, & ! 2   
     &    481,  482,  483,  484,  485,  486,  487,  488,  489,  490, & ! 3   
     &    491,  492,  493,  494,  495,  496,  497,  498,  499,  500, & ! 4   
     &    550,  551,  552,  553,  598,  599,  600,  601,  602,  603, & ! 5   
     &    604,  605,  606,  607,  608,  609,  610,  611,  612,  613, & ! 6   
     &    614,  615,  616,  617,  618,  619,  620,  621,  622,  623, & ! 7   
     &    624,  625,  626,  627,  652,  653,  654,  655,  656,  657, & ! 8   
     &    658,  659,  660,  717,  718,  719,  720,  721,  722,  753, & ! 9   
     &    755,  756,  757,  758,  759,  760,  761,  795,  797,  798, & ! O   
     &    799,  800,  801,  802,  803,  804,  806,  809,  810,  811, & ! 1   
     &    812,  813,  814,  815,  816,  817,  818,  819,  820,  821, & ! 2   
     &    822,  823,  824,  825,  826,  827,  828,  829,  830,  831, & ! 3   
     &    832,  833,  834,  835,  836,  837,  838,  839,  840,  841, & ! 4   
     &    842,  843/     !  5   

      INTEGER, PARAMETER :: KTN7 =   0
      INTEGER            :: KRX7( 1 )

      DATA   KRX7( 1 ) / 0 /

      INTEGER, PARAMETER :: NWM =   3
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &      2,   21,  555/
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
     &    501,  628/
      REAL,    PARAMETER :: ATM_CH4 = 1.85000E+00

      INTEGER, PARAMETER :: NWH2 =   2
      INTEGER            :: NRXWH2( NWH2 )

      DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) / & 
     &     45,  575/
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
     &     36,   36,   36,   36,   36,   39,   40,   40,   41,   41, & ! 9   
     &     38,   38,   38,   31,   31,   31,   31,   31,   31,   31, & ! O   
     &     31,   31,   31,   46,   46,   46,   46,   46,   46,   46, & ! 1   
     &     46,   46,   46,   47,   47,   47,   47,   47,   47,   47, & ! 2   
     &     47,   47,   47,   48,   48,   48,   48,   48,   48,   48, & ! 3   
     &     48,   48,   48,   49,   49,   49,   49,   49,   49,   49, & ! 4   
     &     49,   49,   49,   50,   50,   50,   50,   50,   50,   50, & ! 5   
     &     50,   50,   50,   51,   51,   51,   51,   51,   51,   51, & ! 6   
     &     51,   51,   51,   52,   52,   52,   52,   52,   52,   52, & ! 7   
     &     52,   52,   52,   53,   53,   53,   53,   53,   53,   53, & ! 8   
     &     53,   53,   53,   20,   20,   20,   20,   54,   54,   54, & ! 9   
     &     55,   55,   55,   43,   43,   57,   57,   22,   60,   28, & ! O   
     &     35,   21,   21,   63,   63,   65,   65,   68,   68,   75, & ! 1   
     &     75,   75,   75,   69,   69,   69,   79,   45,   45,   44, & ! 2   
     &     44,   44,   80,   80,   80,   76,   76,   76,   77,   77, & ! 3   
     &     77,   81,   81,   83,   83,   84,   84,   84,   70,   70, & ! 4   
     &     70,   70,   66,   66,   42,   42,   87,   87,   89,   89, & ! 5   
     &     89,   89,   89,   27,   27,   34,   34,   78,   78,   78, & ! 6   
     &     56,   56,   56,   56,   56,   56,   56,   56,   56,   56, & ! 7   
     &     33,   33,   33,   33,   33,   33,   33,   33,   33,   33, & ! 8   
     &     59,   59,   59,   59,   59,   59,   59,   59,   59,   59, & ! 9   
     &     88,   88,   88,   88,   88,   88,   88,   88,   88,   88, & ! O   
     &     72,   72,   72,   72,   72,   72,   72,   72,   72,   72, & ! 1   
     &     64,   64,   64,   64,   64,   64,   64,   64,   64,   64, & ! 2   
     &     71,   71,   71,   71,   71,   71,   71,   71,   71,   71, & ! 3   
     &     61,   61,   61,   61,   61,   61,   61,   61,   61,   61, & ! 4   
     &     62,   62,   62,   62,   62,   62,   62,   62,   62,   62, & ! 5   
     &     90,   90,   90,   90,   90,   90,   90,   90,   90,   90, & ! 6   
     &     73,   73,   73,   73,   73,   73,   73,   73,   73,   73, & ! 7   
     &     74,   74,   74,   74,   74,   74,   74,   74,   74,   74, & ! 8   
     &     91,   91,   91,   91,   91,   91,   91,   91,   91,   91, & ! 9   
     &     92,   92,   92,   92,   92,   92,   92,   92,   92,   92, & ! O   
     &     93,   93,   93,   93,   93,   93,   93,   93,   93,   93, & ! 1   
     &     82,   82,   82,   82,   82,   82,   82,   82,   82,   82, & ! 2   
     &     86,   86,   86,   86,   86,   86,   86,   86,   86,   86, & ! 3   
     &     32,   32,   32,   32,   32,   32,   32,   32,   32,   32, & ! 4   
     &     67,   67,   67,   67,   67,   67,   67,   67,   67,   67, & ! 5   
     &     94,   94,   94,   94,   94,   94,   94,   94,   94,   94, & ! 6   
     &     58,   58,   58,   58,   58,   58,   58,   58,   58,   58, & ! 7   
     &     85,   85,   85,   85,   85,   85,   85,   85,   85,   85, & ! 8   
     &     95,   95,   95,   95,   95,   95,   95,   95,   95,   95, & ! 9   
     &      9,   96,   96,   96,   96,   97,   97,   97,   97,   98, & ! O   
     &     98,   98,   98,   99,   99,  100,  100,  100,  100,  102, & ! 1   
     &    102,  103,  105,  107,  109,  110,  111,  112,  113,  114, & ! 2   
     &    115,  116,  117,  118,  120,  120,  120,  120,  121,  121, & ! 3   
     &    121,  121,  122,  123,  124,  126,  126,  126,  126,  127, & ! 4   
     &    127,  127,  127,  129,  130,  131,  130,  130,  132,  133, & ! 5   
     &    130,  130,  130,  130,  135,  135,  136,  136,  136,  130, & ! 6   
     &    135,  137,  135,    9,  130,   20,   54,   22,   55,   43, & ! 7   
     &     57,   42,   66,   75,   69,   45,   80,   63,   65,   68, & ! 8   
     &     89,   84,   70,  141,  141,  141,  142,  138,  138,  138, & ! 9   
     &    138,  138,  138,  138,  138,  138,  138,  139,  139,  139, & ! O   
     &    139,  139,  139,  139,  139,  139,  139,  140,  140,  140, & ! 1   
     &    140,  140,  140,  140,  140,  140,  140,  130,   96,   97, & ! 2   
     &     98,   99,  100,  102,  105,  107,  109,  110,  111,  112, & ! 3   
     &    113,  114,  115,  116,  117,  120,  121,  122,  123,  124, & ! 4   
     &    126,  127,  104,  104,  108,  108,  106,  106,  125,  125, & ! 5   
     &    152,  152,  152,  152,  152,  153,  153,  153,  153,  154, & ! 6   
     &    154,  154,  154,  154,  154,   99,  155,  155,  155,  155, & ! 7   
     &    155,  155,  155,  162,  162,   99,  164,  164,  164,  164, & ! 8   
     &    164,  164,  164,  159,  167,  167,  167,  167,  167,  159, & ! 9   
     &    157,  169,  169,  169,  169,  169,  157,  158,  174,  174, & ! O   
     &    174,  174,  174,  158,  165,  176,  176,  176,  176,  176, & ! 1   
     &    176,  176,  177,  177,  177,  177,  177,  177,  165,  165, & ! 2   
     &    179,  179,  179,  179,  179,  160,  160,  160,  160,  160, & ! 3   
     &     84,  180,  180,  180,  180,  180,  181,  181,  181,  181, & ! 4   
     &    181,   39,   39,   39,   39,   39,   39,   39,   39,   39, & ! 5   
     &     39,   40,   87,  163,  163,  172,  172,  170,  178,  170, & ! 6   
     &    178,  171,  171,  175,  175,  168,  182,  161,  161,  183, & ! 7   
     &    184,  184,  184,  184,  184,  161,  173,  166,  166,   83, & ! 8   
     &     83,   83,   83,  185,  185,  185,  185,  185,  185,  185, & ! 9   
     &    185,  185,  185,  185,  181,  185,  187,  187,   53,   52, & ! O   
     &     51,   50,   49,   48,   47,   46,   31,   95,   85,   58, & ! 1   
     &     94,   67,   32,   86,   82,   93,   92,   91,   74,   73, & ! 2   
     &     90,   62,   61,   71,   64,   72,   88,   59,   33,   56, & ! 3   
     &    138,  140,  139,  187,  188,  189,  186,    6,    1,    6, & ! 4   
     &      6,  190,  191,  190,  190,  191,    4,  183,  188,  189, & ! 5   
     &    195,  195,  195,  195,  196,  196,  197,  197,  203,  205, & ! 6   
     &    206,  207,  208,  209,  210,  211,  212,  214,  215,  216, & ! 7   
     &    217,  218,  219/     !  8   

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
     &     23,   24,   25,   29,   36,    1,    0,    0,    1,    0, & ! 9   
     &      1,   11,    0,    2,   11,    5,   19,   23,   24,   25, & ! O   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 1   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 2   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 3   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 4   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 5   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 6   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 7   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 8   
     &     29,   36,   39,    0,    0,    9,    5,    9,    0,    5, & ! 9   
     &      9,    0,    5,    9,    0,    9,    0,    9,    9,    9, & ! O   
     &      9,    9,    0,    9,    0,    9,    0,    9,    0,    0, & ! 1   
     &      0,    9,    5,    0,    9,    5,    0,    9,    5,    9, & ! 2   
     &      0,    0,    9,    0,    5,    9,    4,    0,    9,    4, & ! 3   
     &      0,    9,    4,    4,    3,    4,    3,    0,    9,    4, & ! 4   
     &      5,    0,    9,    0,    9,    0,    0,    5,    9,    4, & ! 5   
     &      5,    3,    0,    9,    0,    9,    0,    2,    1,   11, & ! 6   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 7   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 8   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 9   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! O   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 1   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 2   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 3   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 4   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 5   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 6   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 7   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 8   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 9   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! O   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 1   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 2   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 3   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 4   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 5   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 6   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 7   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 8   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 9   
     &      0,    9,    4,    5,    3,    9,    4,    5,    3,    9, & ! O   
     &      4,    5,    3,    4,    3,    9,    4,    5,    3,    9, & ! 1   
     &      4,    9,    9,    9,    9,    9,    9,    9,    9,    9, & ! 2   
     &      9,    9,    9,    9,    9,    4,    5,    3,    9,    4, & ! 3   
     &      5,    3,    9,    9,    9,    9,    4,    5,    3,    9, & ! 4   
     &      4,    5,    3,    0,    2,    0,    1,    1,    0,    0, & ! 5   
     &     11,   11,    4,    5,    2,    1,    0,    0,    0,  136, & ! 6   
     &     11,    0,  135,  134,    0,  130,  130,  130,  130,  130, & ! 7   
     &    130,  130,  130,  130,  130,  130,  130,  130,  130,  130, & ! 8   
     &    130,  130,  130,    0,    9,  130,    0,    2,   11,    5, & ! 9   
     &     19,   23,   24,   25,   29,   36,   39,    2,   11,    5, & ! O   
     &     19,   23,   24,   25,   29,   36,   39,    2,   11,    5, & ! 1   
     &     19,   23,   24,   25,   29,   36,   39,    0,  130,  130, & ! 2   
     &    130,  130,  130,  130,  130,  130,  130,  130,  130,  130, & ! 3   
     &    130,  130,  130,  130,  130,  130,  130,  130,  130,  130, & ! 4   
     &    130,  130,    2,   11,    2,   11,    2,   11,    2,   11, & ! 5   
     &      0,    0,    9,    5,  130,    9,    0,    5,  130,    9, & ! 6   
     &      4,    5,    3,    0,  130,    9,    2,   11,   19,   23, & ! 7   
     &    155,   25,    0,    0,    9,    5,    5,    2,   11,   19, & ! 8   
     &     23,  164,   25,    9,    2,   11,   19,   23,   25,    4, & ! 9   
     &      9,    2,   11,   19,   23,   25,    4,    9,    2,   11, & ! O   
     &     19,   23,   25,    4,    5,    5,    2,    1,   11,   23, & ! 1   
     &     19,   25,    5,    2,   11,   23,   19,   25,    4,    9, & ! 2   
     &      2,   11,   23,   19,   25,    2,   11,   19,   23,   25, & ! 3   
     &      9,    2,   11,   19,   23,   25,    2,   11,   19,   23, & ! 4   
     &     25,    2,   11,    5,   19,   23,   24,   25,   29,   36, & ! 5   
     &     39,    9,    9,    9,    0,    9,    0,    9,    9,    0, & ! 6   
     &      0,    9,    0,    9,    0,    9,    0,    9,    9,    9, & ! 7   
     &     11,    2,   19,   23,   25,    0,    9,    9,    9,    9, & ! 8   
     &      5,    0,  130,    2,   11,    5,   19,   23,   24,   25, & ! 9   
     &     29,   36,   39,  185,    0,    1,    0,    0,  185,  185, & ! O   
     &    185,  185,  185,  185,  185,  185,  185,  185,  185,  185, & ! 1   
     &    185,  185,  185,  185,  185,  185,  185,  185,  185,  185, & ! 2   
     &    185,  185,  185,  185,  185,  185,  185,  185,  185,  185, & ! 3   
     &    185,  185,  185,    9,    9,    9,    9,    0,    0,    0, & ! 4   
     &      0,    0,    0,  192,  193,  194,    0,    0,    0,    0, & ! 5   
     &      0,    0,  198,  199,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    9, & ! 7   
     &      9,    9,    9/     !  8   

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
     &      0,    0,    0/     !  8   

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
     &     23,   23,   13,   13,   38,   40,   39,   39,   42,   43, & ! 9   
     &     44,   45,   45,    2,   11,    5,   19,   23,   24,   25, & ! O   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 1   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 2   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 3   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 4   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 5   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 6   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 7   
     &     29,   36,   39,    2,   11,    5,   19,   23,   24,   25, & ! 8   
     &     29,   36,   39,   11,   12,   11,    7,   25,   12,    7, & ! 9   
     &     29,   23,    7,   23,   25,   23,   25,   20,   11,   19, & ! O   
     &     23,   20,   20,    9,   55,    9,    9,    9,    9,   12, & ! 1   
     &     20,   11,    7,   11,   12,    7,   25,   38,    7,   38, & ! 2   
     &     10,    0,   36,    0,    7,   39,    9,   11,   39,    9, & ! 3   
     &     66,   39,    9,    9,   55,    9,   55,   19,   39,    9, & ! 4   
     &     39,   11,   11,   31,   11,   11,   12,    7,   31,   11, & ! 5   
     &     31,   55,   11,   25,   19,   29,   31,   11,   11,    9, & ! 6   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 7   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 8   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 9   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! O   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 1   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 2   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 3   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 4   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 5   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 6   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 7   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 8   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 9   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! O   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 1   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 2   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 3   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 4   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 5   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 6   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 7   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 8   
     &      2,   11,    5,   19,   23,   24,   25,   29,   36,   39, & ! 9   
     &     19,   31,   11,   31,   11,   31,   11,   31,   55,   31, & ! O   
     &     11,   31,   11,   11,   19,   31,   11,   31,   66,   11, & ! 1   
     &     11,   11,   11,   11,   11,   11,   11,   11,   31,   31, & ! 2   
     &     31,   31,   31,    9,   31,   11,   31,   55,   31,   11, & ! 3   
     &     31,   55,   11,   11,   11,   31,   11,   31,   55,   31, & ! 4   
     &     11,   31,   55,  130,  131,  130,  132,  133,  130,  130, & ! 5   
     &    134,  135,  135,  135,  130,  136,  135,  130,  135,  129, & ! 6   
     &    137,    9,  129,  130,  134,  134,  134,  134,  134,  134, & ! 7   
     &    134,  134,  134,  134,  134,  134,  134,  134,  134,  134, & ! 8   
     &     31,   23,  134,   11,   29,  134,   25,    2,   11,    5, & ! 9   
     &     19,   23,   24,   25,   29,   36,   39,    2,   11,    5, & ! O   
     &     19,   23,   24,   25,   29,   36,   39,    2,   11,    5, & ! 1   
     &     19,   23,   24,   25,   29,   36,   39,  134,   31,  134, & ! 2   
     &     31,  134,  134,   11,   31,   31,   31,   31,   31,  134, & ! 3   
     &    134,  134,  134,  134,  134,  134,  134,   31,   31,   31, & ! 4   
     &    134,   31,    2,   11,    2,   11,    2,   11,    2,   11, & ! 5   
     &      0,    0,    9,    5,  130,    9,    0,    5,  130,    9, & ! 6   
     &      4,    5,    3,    0,  130,  155,   84,  161,   11,   11, & ! 7   
     &     11,   19,   11,    9,    9,  164,  165,  165,  166,  165, & ! 8   
     &    165,  165,   19,  167,    1,   65,   87,   87,   19,   69, & ! 9   
     &    169,   66,  173,   66,   66,   19,  172,  174,   87,  173, & ! O   
     &     87,   87,   19,  171,  176,    1,    1,   40,   34,  178, & ! 1   
     &    178,   19,  178,  178,  173,  178,  178,   19,  178,  176, & ! 2   
     &    178,   65,  178,  178,   19,    1,   65,   11,   11,   11, & ! 3   
     &    180,   87,   63,   87,   87,   19,    1,   63,   11,   11, & ! 4   
     &     19,    1,   34,    1,   20,   12,   12,   13,   12,   12, & ! 5   
     &     12,  163,   11,   69,   11,   20,    1,   69,   69,   25, & ! 6   
     &     25,   60,   25,   28,  163,   12,   54,  183,  155,  184, & ! 7   
     &    163,  163,  163,  163,  163,    9,    1,  173,  164,  181, & ! 8   
     &    185,    9,  134,    1,  186,    1,   20,   12,   12,   13, & ! 9   
     &     12,   12,   12,   12,  163,  187,  185,  185,  185,  185, & ! O   
     &    185,  185,  185,  185,  185,  185,  185,  185,  185,  185, & ! 1   
     &    185,  185,  185,  185,  185,  185,  185,  185,  185,  185, & ! 2   
     &    185,  185,  185,  185,  185,  185,  185,  185,  185,  185, & ! 3   
     &    185,  185,  185,  163,    0,    0,  185,    7,   10,    7, & ! 4   
     &      7,    7,    7,  133,  133,  133,    0,  195,  196,  197, & ! 5   
     &    198,  199,  200,  200,  201,  202,  201,  202,  204,  204, & ! 6   
     &    204,  204,  204,  204,  204,  204,  213,  213,  213,  217, & ! 7   
     &      9,  219,    9/     !  8   

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
     &     38,   38,   19,   23,   23,    0,    1,    1,    0,   19, & ! 9   
     &      0,    0,   23,   11,    0,   11,   11,   11,   11,   11, & ! O   
     &     11,   11,   11,    9,    0,    9,    9,    9,    9,    9, & ! 1   
     &      9,    9,    9,    1,    0,    1,    1,    1,    1,    1, & ! 2   
     &      1,    1,    1,   19,    0,   19,    0,   19,   19,   19, & ! 3   
     &     19,   19,   19,   25,    0,   25,   25,   25,   25,    0, & ! 4   
     &     25,   25,   25,   29,    0,   29,   29,   29,   29,   29, & ! 5   
     &      0,   29,   29,   39,    0,   39,   39,   39,   39,   39, & ! 6   
     &     39,   39,    0,   41,    0,   41,   41,   41,   41,   41, & ! 7   
     &     41,   41,   41,   12,    0,   12,   12,   12,   12,   12, & ! 8   
     &     12,   12,   12,   12,    0,   12,   11,    0,   11,   25, & ! 9   
     &     23,   31,   29,   49,   19,   24,   23,   11,   13,   23, & ! O   
     &     31,    9,   11,   23,   11,   23,   11,   11,   11,   11, & ! 1   
     &     12,   12,   11,   12,   25,   12,    0,   23,   38,    0, & ! 2   
     &      0,    0,    0,    0,   36,   23,   11,   19,   23,   11, & ! 3   
     &      0,   23,   11,   11,    0,   11,   57,   12,   23,   11, & ! 4   
     &      7,   25,   31,   25,   31,   31,   11,   25,   39,    9, & ! 5   
     &     39,    0,    9,   23,   13,   23,   33,   12,   12,   11, & ! 6   
     &     20,    0,   20,   20,   20,   20,   20,   20,   20,   20, & ! 7   
     &     54,    0,   54,   54,   54,   54,   54,   54,   54,   54, & ! 8   
     &     55,    0,   55,   55,   55,   55,   55,   55,   55,   55, & ! 9   
     &     43,    0,   43,   43,   43,   43,   43,   43,   43,   43, & ! O   
     &     57,    0,   57,   57,   57,   57,   57,   57,   57,   57, & ! 1   
     &     66,    0,   66,   66,   66,   66,   66,   66,   66,   66, & ! 2   
     &     75,    0,   75,   75,   75,   75,   75,   75,   75,   75, & ! 3   
     &     69,    0,   69,   69,   69,   69,   69,   69,   69,   69, & ! 4   
     &     79,    0,   79,   79,   79,   79,   79,   79,   79,   79, & ! 5   
     &     80,    0,   80,   80,   80,   80,   80,   80,   80,   80, & ! 6   
     &     76,    0,   76,   76,   76,   76,   76,   76,   76,   76, & ! 7   
     &     77,    0,   77,   77,   77,   77,   77,   77,   77,   77, & ! 8   
     &     81,    0,   81,   81,   81,   81,   81,   81,   81,   81, & ! 9   
     &     83,    0,   83,   83,   83,   83,   83,   83,   83,   83, & ! O   
     &     84,    0,   84,   84,   84,   84,   84,   84,   84,   84, & ! 1   
     &     70,    0,   70,   70,   70,   70,   70,   70,   70,   70, & ! 2   
     &     42,    0,   42,   42,   42,   42,   42,   42,   42,   42, & ! 3   
     &      0,   63,    0,   57,   57,   57,    0,    0,    0,    0, & ! 4   
     &      0,   65,    0,   66,   66,   66,    0,    0,    0,    0, & ! 5   
     &      0,   68,    0,   66,   66,   66,    0,    0,    0,    0, & ! 6   
     &     42,    0,   66,   66,   66,   66,   66,   66,   66,   66, & ! 7   
     &     87,    0,   87,   87,   87,   87,   87,   87,   87,   87, & ! 8   
     &     89,    0,   89,   89,   89,   89,   89,   89,   89,   89, & ! 9   
     &      0,   23,    9,   23,   31,   23,    9,   23,   57,   23, & ! O   
     &      9,   47,   31,    9,   51,   50,   31,   47,  101,    9, & ! 1   
     &      9,   31,   31,   31,   31,   31,   31,   31,   23,   23, & ! 2   
     &     52,   48,   23,  119,   48,   31,   23,   57,   23,   31, & ! 3   
     &     47,   57,   31,   31,   31,   50,   31,   47,   66,   50, & ! 4   
     &     31,   47,   66,    0,    0,    2,    0,    0,    1,    1, & ! 5   
     &      0,    9,    0,    1,    1,    0,    1,    5,    1,    5, & ! 6   
     &      0,  130,  130,    0,   11,   11,   25,   20,   29,   23, & ! 7   
     &     23,    1,   11,   11,   12,   31,   36,    9,    9,    9, & ! 8   
     &    138,   24,   11,   12,    0,   29,   23,  130,    0,  130, & ! 9   
     &    130,  130,  130,  130,  130,  130,  130,  141,    0,  141, & ! O   
     &    141,  141,  141,  141,  141,  141,  141,  142,    0,  142, & ! 1   
     &    142,  142,  142,  142,  142,  142,  142,   19,   23,   31, & ! 2   
     &    138,   31,   31,   12,   23,   23,   23,   23,   23,   11, & ! 3   
     &     31,   31,   31,   31,   31,   31,   31,   23,   23,   23, & ! 4   
     &     31,  138,  144,  145,  146,  147,  148,  149,  150,  151, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,  156,   83,    9,   20,   20, & ! 7   
     &     20,   13,  162,   11,   66,  156,   84,   84,    0,   84, & ! 8   
     &     84,   84,   13,    0,   87,    0,   69,   69,   13,   75, & ! 9   
     &      0,  170,    0,  170,  170,   13,  170,    0,  163,    0, & ! O   
     &    163,  163,   13,  175,    7,  178,  178,    0,   35,   12, & ! 1   
     &     12,   13,   75,   75,    0,   75,   75,   13,   12,  179, & ! 2   
     &     75,    0,   75,   75,   13,   11,    0,   87,   87,   87, & ! 3   
     &      0,   25,    0,   25,   25,   13,   11,    0,  163,  163, & ! 4   
     &     13,   12,   35,   12,   11,   13,   13,   19,   13,   13, & ! 5   
     &     13,   12,    9,   11,   25,   13,   20,    1,    1,   20, & ! 6   
     &     20,   69,    1,   20,    1,   11,   13,    9,  159,    0, & ! 7   
     &     87,   87,   87,   87,   87,   11,   11,    9,    9,  185, & ! 8   
     &     23,   11,  185,   12,   35,   12,   11,   13,   13,   19, & ! 9   
     &     13,   13,   13,   13,   12,    0,    1,    1,   12,   41, & ! O   
     &     39,   29,   25,   19,    1,    9,   11,   89,   87,   66, & ! 1   
     &      0,    0,    0,   42,   70,   84,   83,   81,   77,   76, & ! 2   
     &     80,   79,   69,   75,   66,   57,   43,   55,   54,   20, & ! 3   
     &    130,  142,  141,   12,    0,    0,  189,    0,    7,  190, & ! 4   
     &    191,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  216, & ! 7   
     &      0,  218,    0/     !  8   

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
     &     13,   13,   38,   31,   13,    0,    0,   13,    0,    0, & ! 9   
     &      0,    0,   31,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,   12,    0,   19,    0, & ! 9   
     &     31,   32,    0,   56,   12,   58,   31,    0,    0,   13, & ! O   
     &     13,   19,    9,   24,    9,   24,   23,   23,   75,    0, & ! 1   
     &      0,   78,   12,   25,    0,   25,    0,   31,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   24,   23,   25,   24,   23, & ! 3   
     &      0,   24,   25,   23,    0,   23,    0,   66,   31,   23, & ! 4   
     &     23,   29,   49,   29,    1,    1,   20,    0,   23,   12, & ! 5   
     &     23,    0,   19,   13,    9,   32,   32,   13,   13,   12, & ! 6   
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
     &      0,    0,   11,   11,   11,   11,   11,   11,   11,   11, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,   56,   12,   59,   19,   24,   19,   24,    0,   24, & ! O   
     &     12,   23,   51,   51,   23,   23,    9,   50,    0,   12, & ! 1   
     &     12,    9,    9,    9,    9,    9,    9,   23,   33,   24, & ! 2   
     &     23,   49,   24,    0,   23,    9,   24,   66,   24,    9, & ! 3   
     &     48,   66,    9,    9,    9,   23,    9,   50,  101,   23, & ! 4   
     &      9,   50,  128,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,   12,    0,   11,   23,   56, & ! 7   
     &     24,   11,   23,   12,   25,   90,    0,   23,   23,   11, & ! 8   
     &     39,   58,   39,   23,    0,    0,  138,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,   56,   23, & ! 2   
     &     23,  138,  138,    0,   24,   24,   24,   24,   24,   31, & ! 3   
     &     23,   23,   52,   48,   23,   23,  138,   24,   24,   24, & ! 4   
     &    138,   49,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    1,   83,   84,   84, & ! 7   
     &     84,   11,    0,  163,    0,    0,   83,   83,    0,   83, & ! 8   
     &     83,   83,  165,    0,   69,    0,   75,   75,   87,    9, & ! 9   
     &      0,  163,    0,  163,  163,   66,   69,    0,   20,    0, & ! O   
     &     20,   20,   87,    9,  177,   12,   12,    0,    4,   13, & ! 1   
     &     13,  178,    1,    1,    0,    1,    1,  178,    9,    0, & ! 2   
     &     12,    0,   12,   12,  178,   87,    0,   69,   69,   69, & ! 3   
     &      0,   69,    0,   69,   69,   87,  163,    0,   12,   12, & ! 4   
     &     11,   13,    4,   13,   12,   20,   20,   12,   20,   20, & ! 5   
     &     20,    1,   75,   60,   20,    1,   11,    0,    0,    1, & ! 6   
     &      1,   20,   87,    5,   12,  163,    0,    0,    9,    0, & ! 7   
     &     75,   75,   75,   75,   75,   20,   66,    0,  165,    0, & ! 8   
     &      7,   25,   23,   13,    4,   13,   12,   20,   20,   12, & ! 9   
     &     20,   20,   20,   20,    9,    0,    0,   13,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   11, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    5,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    9, & ! 7   
     &      0,    9,    0/     !  8   

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
     &      0,    0,   23,   32,    0,    0,    0,   20,    0,    0, & ! 9   
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
     &     53,   33,    0,   32,    0,   31,   33,    0,    0,   31, & ! O   
     &     33,    0,    0,   58,    0,   58,   24,   24,   69,    0, & ! 1   
     &      0,    0,   78,    0,    0,    0,    0,   67,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   58,   12,   39,   58,   12, & ! 3   
     &      0,   58,   23,   12,    0,   31,    0,   39,   24,   50, & ! 4   
     &     31,   12,   50,   23,   47,   23,    0,    0,   53,   13, & ! 5   
     &     24,    0,   39,   46,    0,   13,   13,    1,    5,   13, & ! 6   
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
     &      0,   85,   13,   32,   23,   58,   12,   58,    0,   58, & ! O   
     &     13,   24,   23,   23,   24,   24,   49,   23,    0,   60, & ! 1   
     &     13,   23,   23,   23,   23,   23,   23,   56,   32,   58, & ! 2   
     &     24,   23,   58,    0,   24,   23,   58,    0,   58,   19, & ! 3   
     &     23,    0,   23,   23,   23,   24,   49,   23,    0,   24, & ! 4   
     &     49,   23,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,   33,   49, & ! 7   
     &     58,   23,   24,   29,    0,   67,    0,   55,   24,   23, & ! 8   
     &     23,   31,   23,  138,    0,    0,   56,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,  143,   24, & ! 2   
     &     24,   23,   49,    0,   58,   58,   58,   58,   58,   23, & ! 3   
     &     33,   24,   23,   49,   24,   24,   48,   58,   58,   58, & ! 4   
     &     49,   50,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,  157,   84,   83,   83, & ! 7   
     &     83,   20,    0,   69,    0,    0,    1,    1,    0,    1, & ! 8   
     &      1,    1,   84,    0,   75,    0,  163,  163,   69,   20, & ! 9   
     &      0,  171,    0,  171,  171,  170,   35,    0,   11,    0, & ! O   
     &     11,   11,  163,   11,    0,   13,   13,    0,    0,   11, & ! 1   
     &     11,   12,    0,  173,    0,  173,  173,   75,   11,    0, & ! 2   
     &    173,    0,  173,  173,   75,   69,    0,   75,   75,   75, & ! 3   
     &      0,   20,    0,   20,   20,   25,   12,    0,   20,   20, & ! 4   
     &     69,   20,    9,   20,   13,   19,   19,   20,   19,   19, & ! 5   
     &     19,    0,   12,    9,    0,    0,   12,    0,    0,    0, & ! 6   
     &      0,  182,    0,   60,   11,   66,    0,    0,    0,    0, & ! 7   
     &     69,   69,   69,   69,   69,   84,    0,    0,    0,    0, & ! 8   
     &     31,  185,   24,   20,    0,   20,   13,   19,   19,   20, & ! 9   
     &     19,   19,   19,   19,    0,    0,    0,   20,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,  188,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,   33,    0,    0,    0,   25,    0,    0, & ! 9   
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
     &     33,   12,    0,    0,    0,   49,   32,    0,    0,   61, & ! O   
     &     59,    0,    0,   55,    0,   66,   58,   58,   76,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,   61,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   31,   13,   12,   31,   13, & ! 3   
     &      0,   31,   24,   13,    0,   12,    0,    0,   58,   12, & ! 4   
     &     24,   20,   23,   24,   23,   24,    0,    0,   56,   20, & ! 5   
     &     58,    0,   12,   56,    0,   46,    9,    0,    0,   75, & ! 6   
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
     &      0,   32,   20,    0,   12,   56,   13,   32,    0,   56, & ! O   
     &     20,   58,   24,   24,   58,   58,   50,   24,    0,   75, & ! 1   
     &      0,   24,   24,   24,   24,   24,   24,   54,    0,   59, & ! 2   
     &     58,   24,   56,    0,   58,   24,   33,    0,   56,   49, & ! 3   
     &     24,    0,   24,   24,   24,   58,   50,   24,    0,   58, & ! 4   
     &     50,   24,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,   53,   32, & ! 7   
     &     31,   24,   58,    0,    0,    0,    0,   46,   58,   24, & ! 8   
     &     24,   49,   24,   56,    0,    0,   32,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   58, & ! 2   
     &     58,   24,   50,    0,   90,   90,   90,   90,   90,   56, & ! 3   
     &     32,   58,   24,   23,   58,   58,   23,   64,   90,   90, & ! 4   
     &     50,   51,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,  158,   11,  160,  160, & ! 7   
     &    160,   84,    0,   87,    0,    0,   11,   11,    0,   11, & ! 8   
     &     11,   11,   83,    0,  163,    0,  168,  168,   75,   87, & ! 9   
     &      0,  172,    0,  172,  172,  163,   11,    0,  175,    0, & ! O   
     &    175,  175,   20,   12,    0,   11,   11,    0,    0,    0, & ! 1   
     &     20,   11,    0,    0,    0,    0,   20,    1,   13,    0, & ! 2   
     &      1,    0,   11,   11,   12,   75,    0,  163,  163,  163, & ! 3   
     &      0,   11,    0,   11,   11,   69,   20,    0,   69,   69, & ! 4   
     &    163,   19,   20,   19,   19,    0,    0,    0,   23,   38, & ! 5   
     &      0,    0,   13,   19,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    5,    0,   69,    0,    0,    0,    0,    0,    0, & ! 7   
     &      9,    9,    9,    9,    9,   83,    0,    0,    0,    0, & ! 8   
     &     53,   23,   58,   19,    0,   19,   19,    0,    0,    0, & ! 9   
     &     23,   38,    0,    0,    0,    0,    0,   25,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,  189,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,   38,    0,    0,    0,    5,    0,    0, & ! 9   
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
     &     32,   11,    0,    0,    0,   50,    0,    0,    0,   32, & ! O   
     &     62,    0,    0,   31,    0,   31,   55,   66,   77,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   49,   75,   75,   49,   75, & ! 3   
     &      0,   49,   58,   20,    0,   13,    0,    0,   53,   13, & ! 4   
     &     58,   87,   24,   58,   24,   58,    0,    0,   33,   60, & ! 5   
     &      7,    0,   13,   32,    0,   33,    0,    0,    0,    4, & ! 6   
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
     &      0,    0,   60,    0,   53,   33,   20,    0,    0,   95, & ! O   
     &     60,   56,   58,   58,   56,   53,   23,   58,    0,    0, & ! 1   
     &      0,   58,   58,   58,   58,   58,   58,   85,    0,   88, & ! 2   
     &     56,   58,   33,    0,   56,   58,   59,    0,   33,   50, & ! 3   
     &     58,    0,   58,   58,   58,   53,   23,   58,    0,   53, & ! 4   
     &     23,   58,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,   31,    0, & ! 7   
     &     49,   58,   55,    0,    0,    0,    0,   31,   66,   58, & ! 8   
     &     58,  139,   58,   32,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   95, & ! 2   
     &     56,   58,   51,    0,    0,    0,    0,    0,    0,   54, & ! 3   
     &      0,   59,   58,   24,   56,   56,   24,    0,   64,   64, & ! 4   
     &     51,   23,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,   20,   20,  159,  159, & ! 7   
     &    159,   83,    0,   75,    0,    0,   20,   20,    0,   20, & ! 8   
     &     20,   20,    1,    0,  168,    0,   35,   35,  163,   12, & ! 9   
     &      0,   60,    0,   60,   60,  171,   20,    0,  171,    0, & ! O   
     &    171,  171,   11,   13,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   22,    0,   75,    0, & ! 2   
     &     11,    0,    0,   20,  173,  163,    0,   66,   66,   19, & ! 3   
     &      0,  171,    0,   57,   57,   20,   69,    0,   66,   66, & ! 4   
     &     12,    0,   25,    0,    0,    0,    0,    0,   31,   23, & ! 5   
     &      0,    0,   60,   13,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,  163,    0,    0,    0,    0,    0,    0, & ! 7   
     &     11,   11,   11,   11,   11,  160,    0,    0,    0,    0, & ! 8   
     &     32,   12,   31,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &     31,   23,    0,    0,    0,    0,    0,    5,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,   26,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,   56,    0,    0,    0,    0, & ! O   
     &     32,    0,    0,   46,    0,   46,   66,   69,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   53,   69,   69,   53,   69, & ! 3   
     &      0,   53,   12,   69,    0,   20,    0,    0,   56,   20, & ! 4   
     &     53,   57,   58,   56,   58,   20,    0,    0,   71,   75, & ! 5   
     &     53,    0,   20,    0,    0,   31,    0,    0,    0,    0, & ! 6   
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
     &      0,    0,    0,    0,   56,   32,   54,    0,    0,   82, & ! O   
     &     89,   93,   53,   12,   66,   56,   24,   53,    0,    0, & ! 1   
     &      0,   71,   71,   71,   71,   71,   71,   32,    0,   32, & ! 2   
     &     33,   53,   59,    0,   33,   12,   88,    0,   59,   23, & ! 3   
     &     56,    0,   71,   71,   71,   56,   24,   53,    0,   56, & ! 4   
     &     24,   53,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,   32,    0, & ! 7   
     &     50,   55,   66,    0,    0,    0,    0,   56,   46,   66, & ! 8   
     &    134,   32,   76,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  139, & ! 2   
     &     95,   56,   23,    0,    0,    0,    0,    0,    0,   85, & ! 3   
     &      0,   88,   56,   58,   33,   33,   58,    0,    0,    0, & ! 4   
     &     23,   24,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,  159,    0,  123,  123, & ! 7   
     &    123,  160,    0,   20,    0,    0,  159,  159,    0,  159, & ! 8   
     &    159,  159,   11,    0,   35,    0,   66,   66,  168,  163, & ! 9   
     &      0,    5,    0,    5,    5,  172,   12,    0,    1,    0, & ! O   
     &      1,    1,  175,   60,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   11,    0,    1,    0, & ! 2   
     &      0,    0,    0,   22,   11,    0,    0,   20,    0,   13, & ! 3   
     &      0,    1,    0,   22,    0,   11,  175,    0,   22,    0, & ! 4   
     &     20,    0,   13,    0,    0,    0,    0,    0,   32,    0, & ! 5   
     &      0,    0,   20,   12,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    1,    0,    0,    0,    0,    0,    0, & ! 7   
     &     13,   13,   13,   13,   13,  159,    0,    0,    0,    0, & ! 8   
     &      0,   20,   53,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &     32,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,   20,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,   33,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,   56,    0,   33,   31,   70,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   59,   50,    0,   59,   50, & ! 3   
     &      0,   71,   13,   60,    0,   69,    0,    0,   85,   57, & ! 4   
     &     56,    0,   20,   33,   56,   56,    0,    0,   32,    0, & ! 5   
     &     86,    0,   28,    0,    0,   59,    0,    0,    0,    0, & ! 6   
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
     &      0,    0,    0,    0,   54,    0,   60,    0,    0,   32, & ! O   
     &     84,   82,   95,   13,   67,   59,   58,   56,    0,    0, & ! 1   
     &      0,   45,   61,   61,   61,   61,   61,    0,    0,    0, & ! 2   
     &     59,   56,   88,    0,   85,   13,   86,    0,   88,   24, & ! 3   
     &     33,    0,   61,   61,   61,   59,   58,   56,    0,   59, & ! 4   
     &     58,   56,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     56,   57,   31,    0,    0,    0,    0,   33,   31,   69, & ! 8   
     &     53,    0,   77,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  140, & ! 2   
     &     82,   93,   24,    0,    0,    0,    0,    0,    0,   32, & ! 3   
     &      0,   32,   33,   53,   59,   59,   56,    0,    0,    0, & ! 4   
     &     24,   58,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,  123,    0,   66,   66, & ! 7   
     &     66,  159,    0,    0,    0,    0,    0,    0,    0,  157, & ! 8   
     &    157,  157,   20,    0,   66,    0,   12,   12,   35,   35, & ! 9   
     &      0,   20,    0,   20,   20,   60,    9,    0,    0,    0, & ! O   
     &    173,  173,  171,   20,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,   69,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,   22,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,   33,    0, & ! 5   
     &      0,    0,    0,   28,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,   13,    0,    0,    0,    0,    0,    0, & ! 7   
     &     20,   20,   20,   20,   20,  123,    0,    0,    0,    0, & ! 8   
     &      0,   49,   82,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &     33,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,   11,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,   59,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,   33,    0,   59,   33,   31,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   72,   56,    0,   72,   56, & ! 3   
     &      0,   61,   75,   50,    0,   60,    0,    0,   59,   75, & ! 4   
     &     59,    0,   56,   59,   33,   54,    0,    0,    0,    0, & ! 5   
     &     32,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
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
     &      0,    0,    0,    0,   71,    0,   28,    0,    0,    0, & ! O   
     &     66,   86,   73,   20,    0,   88,   12,   59,    0,    0, & ! 1   
     &      0,   73,   45,   45,   62,   45,   62,    0,    0,    0, & ! 2   
     &     88,   33,   72,    0,   59,   20,   32,    0,   72,   58, & ! 3   
     &     59,    0,   45,   62,   62,   88,   12,   85,    0,   88, & ! 4   
     &     12,   33,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     33,   66,   49,    0,    0,    0,    0,   59,   33,   70, & ! 8   
     &     56,    0,   31,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   32, & ! 2   
     &     32,   82,   58,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   59,   56,   88,   95,   33,    0,    0,    0, & ! 4   
     &     58,  134,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,  160,    0,   22,    0, & ! 7   
     &      0,  123,    0,    0,    0,    0,    0,    0,    0,   22, & ! 8   
     &      0,    0,  159,    0,   12,    0,   11,   11,   66,   13, & ! 9   
     &      0,   87,    0,   87,   87,    5,   13,    0,    0,    0, & ! O   
     &     22,    0,    1,  173,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     60,   60,   60,   60,   60,    0,    0,    0,    0,    0, & ! 8   
     &      0,   56,  139,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,   32,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,   59,    0,   64,   59,   46,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   71,   67,    0,   71,   67, & ! 3   
     &      0,   62,   69,   56,    0,   50,    0,    0,   72,   69, & ! 4   
     &     61,    0,   54,   67,   55,   33,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0,    0,   32,    0,    0,    0,    0,    0, & ! O   
     &      0,   32,   74,   56,    0,   61,   53,   88,    0,    0, & ! 1   
     &      0,   74,   90,   90,   45,   90,   45,    0,    0,    0, & ! 2   
     &     72,   59,   64,    0,   88,   33,   67,    0,   93,   12, & ! 3   
     &     88,    0,   73,   45,   45,   72,   13,   59,    0,   72, & ! 4   
     &     13,   59,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     59,   42,   50,    0,    0,    0,    0,   72,   59,   31, & ! 8   
     &     71,    0,   53,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,  139,   53,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   88,   33,   72,   93,   59,    0,    0,    0, & ! 4   
     &     53,   53,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,   11,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,   11,    0,   20,    0,   12,   11, & ! 9   
     &      0,    1,    0,    1,    1,   20,    1,    0,    0,    0, & ! O   
     &      0,    0,    0,  163,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     12,   12,   12,   12,   12,    0,    0,    0,    0,    0, & ! 8   
     &      0,   32,  140,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,   64,    0,   32,   64,   59,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   61,    0,    0,   61,    0, & ! 3   
     &      0,   73,   76,   32,    0,   56,    0,    0,   64,   60, & ! 4   
     &     86,    0,   33,    0,   59,   55,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,   66,   60,    0,   62,   13,   61,    0,    0, & ! 1   
     &      0,   81,   73,   73,   90,   73,   90,    0,    0,    0, & ! 2   
     &     32,   88,   67,    0,   95,   55,    0,    0,   82,   13, & ! 3   
     &     72,    0,   74,   90,   90,   61,   20,   88,    0,   61, & ! 4   
     &     20,   88,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     32,   47,   56,    0,    0,    0,    0,   32,   64,   46, & ! 8   
     &    139,    0,   56,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,   67,   56,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   72,   59,   64,   82,   88,    0,    0,    0, & ! 4   
     &     56,   56,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,   22,    0,   11,   25, & ! 9   
     &      0,   11,    0,   11,   11,   87,  163,    0,    0,    0, & ! O   
     &      0,    0,    0,    1,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    1,   66,   66,   19,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,   32,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,   32,    0,    0,   67,   71,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   67,    0,    0,   67,    0, & ! 3   
     &      0,   74,   77,    0,    0,   32,    0,    0,   71,   35, & ! 4   
     &     67,    0,   55,    0,   88,   59,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,   32,   83,    0,   67,   56,   86,    0,    0, & ! 1   
     &      0,   94,   74,   74,   73,   74,   73,    0,    0,    0, & ! 2   
     &     67,   72,    0,    0,   92,   59,    0,    0,   64,   20, & ! 3   
     &     86,    0,   81,   73,   73,   62,   56,   92,    0,   62, & ! 4   
     &     56,   92,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   31,   33,    0,    0,    0,    0,    0,   67,   59, & ! 8   
     &     32,    0,   61,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   59,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   32,   88,   67,  139,   72,    0,    0,    0, & ! 4   
     &     59,   59,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,  173,  173,    1,   87,    0,    0,    0, & ! O   
     &      0,    0,    0,   79,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,   22,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,   72,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,   82,   81,    0,    0,    0,    0,    0,   61,   85, & ! 4   
     &      0,    0,   59,    0,   57,   88,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0,   84,    0,  101,   59,   67,    0,    0, & ! 1   
     &      0,  104,   81,   81,   74,   81,   74,    0,    0,    0, & ! 2   
     &      0,   64,    0,    0,   93,   88,    0,    0,   32,   56, & ! 3   
     &     32,    0,   64,   74,   74,   93,   59,   93,    0,   93, & ! 4   
     &     59,   93,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   56,   59,    0,    0,    0,    0,    0,    0,   71, & ! 8   
     &      0,    0,   73,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   88,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   67,   72,    0,  140,   92,    0,    0,    0, & ! 4   
     &     88,   88,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,   22,    0,   11,    0,    0,    0,    0, & ! O   
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
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,   61,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,   67,   83,    0,    0,    0,    0,    0,   67,   56, & ! 4   
     &      0,    0,   72,    0,   72,   57,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0,   70,    0,    0,   88,  101,    0,    0, & ! 1   
     &      0,    0,   67,   67,   81,   91,   81,    0,    0,    0, & ! 2   
     &      0,   67,    0,    0,   82,   57,    0,    0,   67,   54, & ! 3   
     &     67,    0,   67,   81,   81,   82,   88,   82,    0,   82, & ! 4   
     &     88,   82,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   33,   72,    0,    0,    0,    0,    0,    0,   72, & ! 8   
     &      0,    0,   74,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   71,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,   64,    0,   32,   93,    0,    0,    0, & ! 4   
     &     71,   71,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,   73,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   84,    0,    0,    0,    0,    0,    0,   67, & ! 4   
     &      0,    0,   66,    0,   66,   72,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0,   66,    0,    0,   57,    0,    0,    0, & ! 1   
     &      0,    0,   94,   94,   67,   67,   91,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,   64,   60,    0,    0,    0,   33, & ! 3   
     &      0,    0,  106,   91,   91,   64,   57,   86,    0,   64, & ! 4   
     &     57,   86,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   59,   64,    0,    0,    0,    0,    0,    0,   61, & ! 8   
     &      0,    0,   82,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   62,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,   67,    0,   67,   82,    0,    0,    0, & ! 4   
     &     62,   62,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,   74,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   70,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,   64,    0,   64,   66,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0,   67,    0,    0,   71,    0,    0,    0, & ! 1   
     &      0,    0,  106,  108,   94,   94,   67,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,   32,   35,    0,    0,    0,   55, & ! 3   
     &      0,    0,    0,   64,   64,   67,   60,   67,    0,   67, & ! 4   
     &     60,   67,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   88,   67,    0,    0,    0,    0,    0,    0,   73, & ! 8   
     &      0,    0,  139,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   93,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,  139,    0,    0,    0, & ! 4   
     &     93,   93,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,   67,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   31,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,   67,    0,   42,   64,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0,    0,    0,    0,   62,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,  108,  108,   94,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,   67,   66,    0,    0,    0,   59, & ! 3   
     &      0,    0,    0,   67,   67,  101,   35,  101,    0,  128, & ! 4   
     &     35,  128,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   72,    0,    0,    0,    0,    0,    0,    0,   74, & ! 8   
     &      0,    0,  140,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   82,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,  140,    0,    0,    0, & ! 4   
     &     82,   82,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   50,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,   86,   32,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0,    0,    0,    0,   66,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,  108,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   32,    0,    0,    0,   43, & ! 3   
     &      0,    0,    0,  108,  125,    0,   61,    0,    0,    0, & ! 4   
     &     61,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   64,    0,    0,    0,    0,    0,    0,    0,   67, & ! 8   
     &      0,    0,   67,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   73,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,   32,    0,    0,    0, & ! 4   
     &     73,   73,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   56,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,   32,   67,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0,    0,    0,    0,   67,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,   67,    0,    0,    0,   57, & ! 3   
     &      0,    0,    0,    0,    0,    0,   62,    0,    0,    0, & ! 4   
     &     62,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   86,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   74,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,   67,    0,    0,    0, & ! 4   
     &     74,   74,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   61,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,   67,    0,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0,    0,    0,    0,  101,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   72, & ! 3   
     &      0,    0,    0,    0,    0,    0,   92,    0,    0,    0, & ! 4   
     &     92,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,   67,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   91,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &     91,   91,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   67,    0,    0,    0,    0,    0,    0,    0, & ! 4   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   60, & ! 3   
     &      0,    0,    0,    0,    0,    0,   82,    0,    0,    0, & ! 4   
     &     82,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,  139,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &    139,  139,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   28, & ! 3   
     &      0,    0,    0,    0,    0,    0,   66,    0,    0,    0, & ! 4   
     &     66,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   67,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &     67,   67,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   35, & ! 3   
     &      0,    0,    0,    0,    0,    0,   67,    0,    0,    0, & ! 4   
     &     67,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   66, & ! 3   
     &      0,    0,    0,    0,    0,    0,  101,    0,    0,    0, & ! 4   
     &    128,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
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
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   32, & ! 3   
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
     &      0,    0,    0/     !  8   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   67, & ! 3   
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
     &      0,    0,    0/     !  8   

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
     &     1.0000D+00, 1.6000D+16, 1.0000D+00, 2.4000D-11, 7.5000D+14, & ! +   
     &     2.3000D-11, 1.0000D+00, 1.0000D-03, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
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
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     5.4000D-12, 2.0000D-12, 4.4000D-12, 1.0000D+00, 1.4000D-12, & ! +   
     &     5.1000D-12, 1.0000D+00, 1.4000D-12, 4.5600D-14, 5.0000D-01, & ! O   
     &     1.3000D-12, 1.7500D-01, 2.8500D-12, 4.5000D-13, 4.2000D-14, & ! +   
     &     1.2000D-12, 3.8000D-12, 1.0000D+00, 2.5000D-11, 1.0000D+00, & ! 1   
     &     5.6000D-11, 1.0000D+00, 1.4100D-10, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 3.1000D-12, 2.8000D-12, 1.0000D+00, 1.5000D-11, & ! 2   
     &     1.4000D-12, 1.0000D+00, 1.7000D-12, 1.4000D-11, 3.5000D-12, & ! +   
     &     1.5000D-03, 1.5000D-02, 1.2000D-11, 6.0000D-02, 1.3400D-12, & ! 3   
     &     7.4000D-11, 9.6600D-18, 1.0000D+00, 7.4000D-11, 9.6600D-18, & ! +   
     &     1.0000D+00, 9.3500D-11, 1.4300D-17, 1.4000D-15, 6.3400D-12, & ! 4   
     &     8.5000D-16, 4.3200D-12, 1.0000D+00, 6.1900D-11, 4.1800D-18, & ! +   
     &     1.0000D-13, 1.0000D+00, 1.5500D-11, 4.8600D-03, 7.2000D-12, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.9900D-11, 1.4000D-15, & ! +   
     &     1.1800D-15, 2.3700D-12, 1.0000D+00, 5.2800D-12, 1.0000D+00, & ! 6   
     &     6.4200D-12, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
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
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
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
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 7   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.8500D-12, 1.0000D-28, 9.1400D-15, 3.3000D-12, 1.0700D-11, & ! O   
     &     4.8500D-12, 5.5100D-15, 4.5900D-13, 1.0200D-11, 1.4800D-11, & ! +   
     &     1.3400D-14, 1.0000D-13, 2.2600D-11, 7.8600D-15, 3.5000D-11, & ! 1   
     &     1.2100D-11, 5.0000D-16, 1.1900D-12, 3.2000D-11, 5.5000D-30, & ! +   
     &     1.0000D-14, 2.3300D-12, 1.8100D-12, 2.3100D-11, 1.3600D-11, & ! 2   
     &     1.4300D-11, 3.2500D-11, 5.4900D-13, 1.3400D-12, 1.4900D-12, & ! +   
     &     1.5100D-12, 3.7500D-12, 2.7000D-12, 2.7000D-12, 6.7200D-12, & ! 3   
     &     3.1900D-15, 5.3700D-13, 1.6100D-11, 1.2600D-11, 8.5900D-15, & ! +   
     &     2.3100D-13, 1.4300D-11, 7.8400D-12, 3.0900D-11, 3.0900D-11, & ! 4   
     &     2.2700D-11, 8.2800D-16, 1.3300D-12, 4.0200D-11, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 7.6000D-32, & ! 5   
     &     1.0000D+00, 1.3000D-30, 1.8000D-31, 1.0000D+00, 1.0000D+00, & ! +   
     &     3.4400D-11, 9.4100D-12, 2.8000D-11, 2.4000D-11, 6.2000D-12, & ! 6   
     &     1.8000D-31, 1.0000D+00, 1.0000D+00, 4.4800D-05, 6.2000D-12, & ! +   
     &     2.2000D-12, 1.0000D+00, 1.2500D-11, 1.7000D-12, 3.9000D-11, & ! 7   
     &     8.1000D-11, 8.0000D-11, 5.5000D-11, 1.2300D-10, 7.7000D-11, & ! +   
     &     3.6000D-11, 1.9200D-10, 2.0000D-10, 8.1000D-11, 8.0000D-11, & ! 8   
     &     6.2000D-11, 8.0000D-11, 1.6600D-10, 3.0000D-10, 4.2900D-10, & ! +   
     &     2.9400D-10, 2.3200D-10, 4.1200D-10, 1.0000D+00, 3.1000D-12, & ! 9   
     &     1.2900D-11, 5.0000D-01, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 7.3000D-12, 1.6000D-29, 2.6700D-10, & ! +   
     &     4.9000D-10, 4.8000D-10, 5.4600D-10, 5.2000D-30, 6.2000D-11, & ! 3   
     &     1.3500D-10, 1.4000D-10, 1.4400D-10, 2.4200D-10, 8.6000D-11, & ! +   
     &     8.3000D-11, 1.2000D-10, 1.8600D-10, 2.6300D-10, 4.2100D-10, & ! 4   
     &     3.9200D-10, 3.7700D-10, 2.1600D-10, 2.6600D-10, 2.6600D-10, & ! +   
     &     5.4600D-10, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 5.4000D-12, 2.0000D-12, 8.1000D-11, & ! 6   
     &     4.4000D-12, 1.0000D+00, 1.4000D-12, 8.0000D-11, 1.9900D-11, & ! +   
     &     1.4000D-15, 1.1800D-15, 2.3700D-12, 1.0000D+00, 2.9400D-10, & ! 7   
     &     2.5400D-11, 2.6000D-12, 2.0600D-13, 1.8000D-12, 6.8000D-13, & ! +   
     &     2.3000D-12, 4.4000D-13, 4.0700D+08, 1.0000D+00, 4.6000D-11, & ! 8   
     &     3.0300D-12, 2.3000D-12, 2.6000D-12, 2.0600D-13, 1.3000D-12, & ! +   
     &     6.0400D-13, 1.2000D-12, 4.4000D-13, 1.4200D-11, 2.6000D-12, & ! 9   
     &     2.0600D-13, 2.0000D-13, 3.5000D-14, 4.4000D-13, 3.9400D-15, & ! +   
     &     1.2000D-11, 2.4000D-12, 2.0600D-13, 2.0000D-13, 3.5000D-14, & ! O   
     &     4.4000D-13, 2.9000D-17, 2.4000D-12, 2.4000D-12, 2.0600D-13, & ! +   
     &     2.0000D-13, 3.5000D-14, 4.4000D-13, 3.7000D-19, 3.1500D-13, & ! 1   
     &     4.0000D-12, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 2.3000D-12, 2.6000D-12, 2.0600D-13, & ! 2   
     &     3.5000D-14, 2.0000D-13, 4.4000D-13, 4.1500D-15, 7.4800D-12, & ! +   
     &     2.6000D-12, 2.0600D-13, 3.5000D-14, 2.0000D-13, 4.4000D-13, & ! 3   
     &     2.6000D-12, 2.0600D-13, 2.0000D-13, 3.5000D-14, 4.4000D-13, & ! +   
     &     2.6000D-12, 2.6000D-12, 1.8200D-13, 2.0000D-13, 3.5000D-14, & ! 4   
     &     4.4000D-13, 2.6000D-12, 1.8200D-13, 2.0000D-13, 3.5000D-14, & ! +   
     &     4.4000D-13, 6.7000D-12, 1.0000D+00, 4.0000D-12, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 2.9000D-11, 8.0000D-12, 2.1500D-12, 1.7500D-01, & ! 6   
     &     2.9400D-12, 1.0000D+00, 4.0000D-13, 4.0000D-13, 1.0000D+00, & ! +   
     &     1.0000D+00, 3.5000D-12, 1.0000D+00, 1.2800D-11, 1.0000D+00, & ! 7   
     &     1.0000D-11, 1.0000D+00, 1.9000D-11, 4.7500D-12, 5.7800D-11, & ! +   
     &     2.0600D-13, 2.6000D-12, 2.0000D-13, 3.5000D-14, 4.4000D-13, & ! 8   
     &     1.0000D+00, 8.0000D-12, 5.0000D-11, 3.8000D-12, 8.0000D-12, & ! +   
     &     1.5000D-12, 1.0000D+00, 3.8500D-10, 6.7000D-12, 1.0000D+00, & ! 9   
     &     4.0000D-12, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 2.9000D+07, & ! O   
     &     1.0000D+00, 1.6000D+16, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 2   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 3   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 3.0000D-11, 1.0000D-12, & ! 4   
     &     4.4000D-12, 1.6600D-11, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 8.3300D-47, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 9.4882D-06, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 7   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 2.5000D-12, & ! +   
     &     1.0000D+00, 2.5000D-12, 1.0000D+00/           !        8   

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
     &     7.3000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 5.3000D+01, 0.0000D+00, 5.2000D+01, 5.3000D+01, & ! O   
     &     5.4000D+01, 5.5000D+01, 5.6000D+01, 5.6000D+01, 7.0000D+01, & ! +   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 5.2000D+01, 5.3000D+01, & ! 1   
     &     5.4000D+01, 5.5000D+01, 5.6000D+01, 5.6000D+01, 7.0000D+01, & ! +   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 5.2000D+01, 5.3000D+01, & ! 2   
     &     5.4000D+01, 5.5000D+01, 5.6000D+01, 5.6000D+01, 7.0000D+01, & ! +   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 5.2000D+01, 5.3000D+01, & ! 3   
     &     5.4000D+01, 5.5000D+01, 5.6000D+01, 5.6000D+01, 7.0000D+01, & ! +   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 5.2000D+01, 5.3000D+01, & ! 4   
     &     5.4000D+01, 5.5000D+01, 5.6000D+01, 5.6000D+01, 7.0000D+01, & ! +   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 5.2000D+01, 5.3000D+01, & ! 5   
     &     5.4000D+01, 5.5000D+01, 5.6000D+01, 5.6000D+01, 7.0000D+01, & ! +   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 5.2000D+01, 5.3000D+01, & ! 6   
     &     5.4000D+01, 5.5000D+01, 5.6000D+01, 5.6000D+01, 7.0000D+01, & ! +   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 5.2000D+01, 5.3000D+01, & ! 7   
     &     5.4000D+01, 5.5000D+01, 5.6000D+01, 5.6000D+01, 7.0000D+01, & ! +   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 5.2000D+01, 5.3000D+01, & ! 8   
     &     5.4000D+01, 5.5000D+01, 5.6000D+01, 5.6000D+01, 7.0000D+01, & ! +   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.6500D+00, 0.0000D+00, & ! O   
     &     2.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 2.0000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 7.6000D+01, 7.3000D+01, 6.7000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 7   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 8   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 9   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! O   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 1   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 2   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 3   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 4   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 5   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 6   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 7   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 8   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 9   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! O   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 1   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 2   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 3   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 4   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 5   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 6   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 7   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 8   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, 5.6000D+01, & ! 9   
     &     5.6000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     0.0000D+00,-4.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 2.0000D+00, 2.0000D+00, 2.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 5.4600D+02, & ! +   
     &     5.4700D+02, 5.4800D+02, 5.4900D+02, 0.0000D+00,-1.8000D+00, & ! 5   
     &     0.0000D+00,-2.0000D+00,-2.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -5.6000D-01, 2.1000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &    -3.4000D+00, 0.0000D+00, 0.0000D+00,-1.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 5.2000D+01, 5.3000D+01, 5.4000D+01, & ! +   
     &     5.5000D+01, 5.6000D+01, 5.6000D+01, 7.0000D+01, 7.0000D+01, & ! O   
     &     7.0000D+01, 7.0000D+01, 5.2000D+01, 5.3000D+01, 5.4000D+01, & ! +   
     &     5.5000D+01, 5.6000D+01, 5.6000D+01, 7.0000D+01, 7.0000D+01, & ! 1   
     &     7.0000D+01, 7.0000D+01, 5.2000D+01, 5.3000D+01, 5.4000D+01, & ! +   
     &     5.5000D+01, 5.6000D+01, 5.6000D+01, 7.0000D+01, 7.0000D+01, & ! 2   
     &     7.0000D+01, 7.0000D+01, 0.0000D+00,-3.3000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-2.4000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 6.5100D+02, 5.2000D+01, 5.3000D+01, 5.2000D+01, & ! 5   
     &     5.3000D+01, 5.2000D+01, 5.3000D+01, 5.2000D+01, 5.3000D+01, & ! +   
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
     &     0.0000D+00, 7.6000D+01, 7.3000D+01, 6.7000D+01, 7.0000D+01, & ! +   
     &     6.9000D+01, 7.2000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 6.7000D+01, 0.0000D+00, 6.9000D+01, & ! 5   
     &     7.0000D+01, 7.0000D+01, 7.2000D+01, 7.2000D+01, 7.2000D+01, & ! +   
     &     7.2000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 6.7000D+01, & ! 9   
     &     0.0000D+00, 6.9000D+01, 7.0000D+01, 7.0000D+01, 7.2000D+01, & ! +   
     &     7.2000D+01, 7.2000D+01, 7.2000D+01, 7.2000D+01, 0.0000D+00, & ! O   
     &     7.3000D+01, 0.0000D+00, 0.0000D+00, 7.0000D+01, 7.0000D+01, & ! +   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! 1   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! 2   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! 3   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, 7.0000D+01, & ! +   
     &     7.0000D+01, 7.0000D+01, 7.0000D+01, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 1.4500D-08, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        8   

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
     &     0.0000D+00,-1.3486D+04, 0.0000D+00, 0.0000D+00,-8.1520D+03, & ! +   
     &     1.5000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
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
     &     1.3500D+02,-2.4310D+03, 3.6500D+02, 0.0000D+00,-1.8600D+03, & ! +   
     &     4.0500D+02, 0.0000D+00,-1.6010D+03, 4.2900D+02, 0.0000D+00, & ! O   
     &    -2.5000D+01, 0.0000D+00,-3.4500D+02, 0.0000D+00, 8.5500D+02, & ! +   
     &     0.0000D+00, 2.0000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.4220D+02,-2.3900D+03, 0.0000D+00, 0.0000D+00, & ! 2   
     &    -1.8950D+03, 0.0000D+00, 9.5000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.8600D+03, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-2.1000D+03, 0.0000D+00, & ! 4   
     &    -1.5200D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-2.5280D+03, & ! +   
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
     &    -1.6900D+03, 0.0000D+00,-2.5800D+03,-2.8800D+03,-8.0000D+02, & ! O   
     &     5.0400D+02,-1.8780D+03,-1.1560D+03,-2.8000D+02, 4.4800D+02, & ! +   
     &    -2.2830D+03, 0.0000D+00,-4.0000D+01,-1.9120D+03, 0.0000D+00, & ! 1   
     &     4.3600D+02,-5.3000D+02, 4.9000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &    -4.1000D+03,-1.9300D+02, 3.3800D+02, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 5.3000D+02,-4.9900D+02,-8.7000D+01, & ! +   
     &     1.2600D+02, 4.4000D+01, 3.7400D+02, 3.7400D+02, 5.0100D+02, & ! 3   
     &    -1.7010D+03,-1.0470D+03,-3.2600D+02, 4.8800D+02,-1.2550D+03, & ! +   
     &     3.8200D+02, 1.1100D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     4.3500D+02,-7.8500D+02, 4.9000D+02, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-2.5000D+02, 0.0000D+00, 2.9500D+02, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.2530D+04, 1.4500D+02, & ! +   
     &     3.4000D+02, 0.0000D+00,-1.9600D+03,-2.3000D+02,-2.3100D+03, & ! 7   
     &    -3.0000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.0000D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-3.0000D+01, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00,-1.2800D+03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.5000D+01, & ! +   
     &    -1.0000D+02, 4.0000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.3500D+02,-2.4310D+03,-3.0000D+01, & ! 6   
     &     3.6500D+02, 0.0000D+00,-1.8600D+03, 0.0000D+00, 0.0000D+00, & ! +   
     &    -2.5280D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     4.1000D+02, 3.8000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.0700D+03,-7.6940D+03, 0.0000D+00, 0.0000D+00, & ! 8   
     &    -4.4800D+02, 0.0000D+00, 3.8000D+02, 1.3000D+03, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0700D+03, 6.1000D+02, 3.8000D+02, & ! 9   
     &     1.3000D+03, 0.0000D+00, 0.0000D+00, 1.0700D+03,-1.5200D+03, & ! +   
     &     6.5200D+02, 3.6000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, & ! O   
     &     1.0700D+03, 0.0000D+00, 7.4500D+02, 3.6000D+02, 1.3000D+03, & ! +   
     &     0.0000D+00, 0.0000D+00, 1.0700D+03, 0.0000D+00,-4.4800D+02, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.8000D+02, 1.3000D+03, & ! 2   
     &     0.0000D+00, 0.0000D+00, 1.0700D+03,-1.5200D+03, 4.1000D+02, & ! +   
     &     3.8000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, 1.0700D+03, & ! 3   
     &     3.8000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, 1.0700D+03, & ! +   
     &     6.1000D+02, 3.8000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, & ! 4   
     &     1.0700D+03, 3.8000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, & ! +   
     &     1.0700D+03, 3.4000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 3.0500D+02, 0.0000D+00, & ! 6   
     &     3.6500D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.4000D+02, 0.0000D+00, 4.0500D+02, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 3.9000D+02, 2.0000D+02,-4.0000D+02, & ! +   
     &     1.3000D+03, 3.8000D+02, 0.0000D+00, 0.0000D+00, 1.0700D+03, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D+02, 3.8000D+02, & ! +   
     &    -1.8150D+03, 0.0000D+00, 0.0000D+00, 3.4000D+02, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-5.2970D+03, & ! O   
     &     0.0000D+00,-1.3486D+04, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00/           !        8   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &      4,    6,   11,   12,   22,   25,   27,   29,   32,   33, & 
     &     37,   38,   44,   63,   64,  502,  520,  557,  558,  566, & 
     &    569,  629,  634,  857/

      DATA ( RFDAT( 1,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     3.0000D-11, 2.2000D-11, 1.9000D-12, 9.7000D+14, 3.6000D-11, & 
     &     3.0000D-11, 2.1990D+03, 0.0000D+00, 2.9000D-12, 5.4200D+15, & 
     &     9.8000D+02, 3.1800D+03, 1.6000D-12, 1.2100D-11, 4.0000D+16, & 
     &     8.8000D-12, 8.3000D-13, 1.0000D-10, 1.0000D-10, 1.5000D-11, & 
     &     3.7100D+15, 3.1000D-10, 2.2000D-10, 9.3220D+01/

      DATA ( RFDAT( 2,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00,-7.0000D-01, 2.0000D-01, 1.0000D-01,-1.0000D-01, & 
     &     0.0000D+00, 6.5000D-34, 0.0000D+00,-1.1000D+00,-2.3000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-9.0000D-01, 0.0000D+00, & 
     &    -8.5000D-01,-2.0000D+00,-1.0000D+00,-1.0000D+00,-1.9000D+00, & 
     &     3.5000D+00,-1.0000D+00, 0.0000D+00, 3.8400D+00/

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
     &        1.00000,    1.00000,    0.60000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.50000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    2.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    1.00000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    2.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.96500,    1.00000,    1.00000,    1.00000,    0.62000, & ! O   
     &        0.96700,    1.00000,    1.00000,    1.00000,    0.50900, & ! +   
     &        1.00000,    0.30000,    1.00000,    0.74400,    1.00000, & ! 1   
     &        0.84000,    1.00000,    0.13900,    1.00000,    2.00000, & ! +   
     &        1.00000,    0.70000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    2.00000,    0.20000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! 3   
     &        0.21700,    0.82600,    1.02300,    0.21700,    0.82600, & ! +   
     &        1.00000,    0.20600,    0.47100,    0.20800,    1.00000, & ! 4   
     &        0.16400,    0.45000,    0.40000,    0.28900,    0.28500, & ! +   
     &        0.15000,    1.23300,    0.47200,    0.91300,    0.18900, & ! 5   
     &        0.34400,    1.00000,    1.00000,    0.25000,    0.83000, & ! +   
     &        0.03100,    1.00000,    1.06600,    0.98000,    1.00000, & ! 6   
     &        0.80600,    1.00000,    1.00000,    1.00000,    0.44000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.16000,    1.00000,    0.80000, & ! O   
     &        0.98400,    0.16500,    0.94900,    0.45000,    0.95100, & ! +   
     &        0.08000,    0.81500,    0.25000,    0.06600,    0.25000, & ! 1   
     &        0.79900,    0.00900,    0.05600,    1.00000,    0.30000, & ! +   
     &        1.50000,    0.57000,    0.18100,    0.15900,    0.16100, & ! 2   
     &        0.15900,    0.02200,    0.95000,    1.00000,    0.96500, & ! +   
     &        0.69500,    0.83000,    0.64700,    1.00000,    0.87100, & ! 3   
     &        0.09500,    0.77200,    0.45000,    0.91200,    0.09400, & ! +   
     &        0.40000,    0.07900,    0.12300,    0.07700,    0.07700, & ! 4   
     &        0.73400,    0.07800,    0.22700,    0.23700,    0.73400, & ! +   
     &        0.07800,    0.22700,    0.23700,    2.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.29000,    1.00000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.40400, & ! +   
     &        0.48400,    1.28300,    0.40100,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.12400, & ! +   
     &        0.39000,    0.15000,    0.54800,    1.00000,    0.89400, & ! 3   
     &        0.86400,    0.86400,    0.86400,    0.83800,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        0.38400,    0.27900,    0.84000,    0.82800,    0.82800, & ! +   
     &        0.54800,    0.25200,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! 7   
     &        1.00000,    0.40000,    0.88000,    0.95000,    0.45000, & ! +   
     &        0.91000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    0.70000,    0.70000,    1.00000,    0.60000, & ! +   
     &        0.60000,    1.20000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    0.11700,    0.11700,    1.00000,    0.50000, & ! +   
     &        1.00000,    0.34000,    1.00000,    0.17000,    0.17000, & ! O   
     &        1.00000,    0.36000,    1.00000,    0.60000,    1.00000, & ! +   
     &        0.30000,    0.30000,    1.00000,    0.12000,    0.60000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    0.75000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.94000,    1.00000, & ! 2   
     &        0.70000,    0.70000,    1.00000,    0.30000,    0.34500, & ! +   
     &        0.91900,    1.00000,    0.68900,    0.68900,    1.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    0.50000,    1.00000, & ! +   
     &        1.00000,    0.62500,    1.00000,    0.35000,    0.35000, & ! 4   
     &        1.00000,    0.85000,    1.00000,    1.00000,    0.50000, & ! +   
     &        1.00000,    1.00000,    0.30750,    1.00000,    2.00000, & ! 5   
     &        1.00000,    1.00000,    2.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    0.75000,    0.75000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.65000,    1.00000,    0.08000,    1.00000, & ! 7   
     &        1.50000,    1.00000,    1.00000,    0.38700,    1.00000, & ! +   
     &        0.72500,    0.72500,    0.36300,    0.36300,    0.72500, & ! 8   
     &        1.00000,    1.00000,    1.00000,    0.30000,    0.53000, & ! +   
     &        0.50000,    0.33000,    0.25000,    1.00000,    0.75000, & ! 9   
     &        1.00000,    2.00000,    1.00000,    1.00000,    2.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    2.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    0.60000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.03000,    0.00000, & ! 4   
     &        0.00000,    0.83000,    2.00000,    0.50000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.71430,    1.71430, & ! +   
     &        1.14280,    1.14280,    1.00000,    1.00000,    0.85714, & ! 7   
     &        0.85714,    1.00000,    1.00000,    1.50000,    1.25000, & ! +   
     &        1.00000,    1.25000,    1.00000/           !        8   

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
     &        0.00000,    1.00000,    0.60000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! O   
     &        1.00000,    0.50000,    0.50000,    0.50000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 1   
     &        1.00000,    0.50000,    0.50000,    0.50000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 2   
     &        1.00000,    0.50000,    0.50000,    0.50000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 3   
     &        1.00000,    0.00000,    0.50000,    0.50000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 4   
     &        1.00000,    0.50000,    0.50000,    0.50000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 5   
     &        1.00000,    0.50000,    0.50000,    0.50000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 6   
     &        1.00000,    0.50000,    0.50000,    0.50000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! 7   
     &        1.00000,    0.50000,    0.50000,    0.50000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 8   
     &        1.00000,    0.50000,    0.50000,    0.50000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! 9   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.03500,    1.00000,    1.00000,    1.00000,    1.38000, & ! O   
     &        0.03900,    1.00000,    1.00000,    1.00000,    0.49100, & ! +   
     &        1.00000,    0.30000,    1.00000,    0.25100,    1.00000, & ! 1   
     &        0.22200,    0.14200,    0.14800,    1.00000,    2.00000, & ! +   
     &        1.00000,    1.40000,    0.70000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    0.00000,    0.80000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 3   
     &        0.72300,    0.52200,    0.17300,    0.72300,    0.52200, & ! +   
     &        0.00000,    0.73300,    0.55400,    0.10800,    0.00000, & ! 4   
     &        0.06400,    0.55000,    0.60000,    0.67000,    0.40000, & ! +   
     &        0.15000,    0.46700,    0.37900,    0.40000,    0.30500, & ! 5   
     &        0.55400,    2.00000,    1.00000,    0.75000,    0.33000, & ! +   
     &        0.96700,    0.00000,    0.17800,    0.02000,    1.00000, & ! 6   
     &        0.19400,    1.00000,    1.00000,    1.00000,    0.44000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 7   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 8   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 9   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! O   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 1   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 2   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 3   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 4   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 5   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 6   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 7   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 8   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 9   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! O   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 1   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 2   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 3   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.50000,    0.50000, & ! 4   
     &        0.50000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.50000,    0.50000, & ! 5   
     &        0.50000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.50000,    0.50000, & ! 6   
     &        0.50000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 7   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 8   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 9   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.16000,    1.00000,    0.29000, & ! O   
     &        0.98400,    0.35000,    0.94900,    0.55000,    1.18900, & ! +   
     &        0.08000,    0.12000,    0.11700,    0.26600,    0.24000, & ! 1   
     &        0.00400,    0.10200,    0.64300,    1.00000,    0.70000, & ! +   
     &        0.50000,    0.29000,    0.45400,    0.52000,    0.55400, & ! 2   
     &        0.48700,    0.62700,    0.05000,    1.00000,    0.96500, & ! +   
     &        0.23600,    0.01000,    1.60500,    0.47000,    0.00100, & ! 3   
     &        0.05700,    1.46300,    0.39000,    0.95300,    0.04100, & ! +   
     &        0.42600,    0.75100,    0.56600,    0.61700,    0.61700, & ! 4   
     &        0.06400,    0.04600,    0.28700,    0.76300,    0.06400, & ! +   
     &        0.04600,    0.28700,    0.76300,    0.00000,    0.00000, & ! 5   
     &        1.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! 6   
     &        0.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.42000,    0.00000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    1.00000,    0.90000,    1.00000, & ! +   
     &        0.97500,    0.03800,    0.31400,    0.63000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    0.41400,    0.14500,    0.13900, & ! +   
     &        0.27400,    0.05300,    0.08400,    1.00000,    0.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.50000,    0.50000,    0.50000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.50000,    0.50000,    0.50000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.50000,    0.50000,    0.50000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    2.00000,    0.97100, & ! +   
     &        0.54100,    0.73800,    0.25200,    1.00000,    0.89400, & ! 3   
     &        0.86400,    0.86400,    0.86400,    0.83800,    0.68800, & ! +   
     &        1.00000,    0.97000,    0.83500,    0.82700,    0.64700, & ! 4   
     &        0.87300,    0.45000,    0.84000,    0.82800,    0.82800, & ! +   
     &        0.25200,    0.06800,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        1.00000,    0.26000,    0.12000,    1.12000,    0.37000, & ! +   
     &        0.75000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    0.03500,    0.03500,    0.00000,    0.01750, & ! +   
     &        0.01750,    0.03500,    1.00000,    0.00000,    0.23400, & ! 9   
     &        0.00000,    0.11700,    0.11700,    1.00000,    0.35000, & ! +   
     &        0.00000,    0.15000,    0.00000,    0.07500,    0.07500, & ! O   
     &        1.00000,    0.29000,    0.00000,    0.60000,    0.00000, & ! +   
     &        0.30000,    0.30000,    1.00000,    0.32000,    0.60000, & ! 1   
     &        1.00000,    1.00000,    0.00000,    0.25000,    1.00000, & ! +   
     &        1.00000,    2.00000,    1.00000,    0.94000,    0.00000, & ! 2   
     &        0.70000,    0.70000,    1.00000,    0.45000,    0.65500, & ! +   
     &        0.91900,    0.00000,    0.68900,    0.68900,    1.00000, & ! 3   
     &        1.00000,    0.00000,    0.26000,    0.26000,    0.52000, & ! +   
     &        0.00000,    0.62500,    0.00000,    0.35000,    0.35000, & ! 4   
     &        1.00000,    0.85000,    0.00000,    0.42400,    0.42400, & ! +   
     &        1.00000,    1.00000,    0.10250,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    2.00000,    2.00000,    2.00000, & ! +   
     &        2.00000,    1.00000,    0.25000,    0.82500,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.65000,    1.00000,    0.08000,    1.00000, & ! 7   
     &        0.50000,    1.00000,    1.00000,    0.61300,    0.00000, & ! +   
     &        0.27500,    0.27500,    0.13800,    0.13800,    0.27500, & ! 8   
     &        0.91000,    1.00000,    1.00000,    0.70000,    0.47000, & ! +   
     &        0.50000,    0.67000,    0.16500,    1.00000,    0.25000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    2.00000, & ! +   
     &        2.00000,    2.00000,    2.00000,    2.00000,    1.00000, & ! O   
     &        0.00000,    1.00000,    0.60000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.03000,    0.00000, & ! 4   
     &        0.00000,    0.17000,    0.00000,    0.50000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.40000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! O   
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
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.03500,    1.00000,    0.00000,    1.00000,    0.38000, & ! O   
     &        0.03900,    1.00000,    0.00000,    0.00000,    0.50900, & ! +   
     &        0.14300,    0.70000,    1.00000,    0.00400,    1.00000, & ! 1   
     &        0.02900,    0.78200,    0.58900,    0.50000,    0.00000, & ! +   
     &        0.00000,    0.30000,    1.40000,    1.00000,    0.00000, & ! 2   
     &        1.00000,    0.00000,    0.80000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.06000,    0.65200,    0.30500,    0.06000,    0.65200, & ! +   
     &        0.00000,    0.11700,    0.01300,    0.10000,    0.00000, & ! 4   
     &        0.05000,    0.00000,    0.60000,    0.67000,    0.04800, & ! +   
     &        0.79900,    0.30000,    0.02900,    0.60000,    0.01900, & ! 5   
     &        1.00000,    1.00000,    0.00000,    0.25000,    1.00500, & ! +   
     &        0.03100,    0.00000,    0.23400,    0.02000,    1.00000, & ! 6   
     &        0.19400,    1.00000,    1.00000,    1.00000,    0.44000, & ! +   
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
     &        0.00000,    0.00000,    1.00000,    0.50000,    0.50000, & ! 7   
     &        0.50000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.61000,    0.51000,    1.00000,    0.51000, & ! O   
     &        0.01600,    0.35500,    0.05100,    0.00000,    0.04900, & ! +   
     &        0.25500,    1.05500,    0.11800,    0.19200,    0.24000, & ! 1   
     &        1.04200,    0.72800,    0.00700,    0.00000,    0.30000, & ! +   
     &        1.50000,    0.11600,    0.31200,    0.23900,    0.19800, & ! 2   
     &        0.27800,    0.23000,    0.05000,    1.00000,    0.03500, & ! +   
     &        1.25300,    0.01100,    0.35300,    0.00000,    1.20200, & ! 3   
     &        0.12800,    0.22800,    0.16000,    0.08800,    0.44300, & ! +   
     &        0.03500,    0.17000,    0.20200,    0.17800,    0.17800, & ! 4   
     &        1.21100,    0.49900,    0.02600,    1.00000,    1.21100, & ! +   
     &        0.49900,    0.02600,    1.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        1.00000,    0.00000,    1.00000,    0.10000,    1.00000, & ! +   
     &        0.03900,    0.05500,    0.68000,    1.26000,    1.00000, & ! 8   
     &        1.00000,    0.00000,    0.58800,    1.07800,    0.14800, & ! +   
     &        0.21600,    0.05300,    0.15400,    1.00000,    0.00000, & ! 9   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.97100, & ! +   
     &        1.88400,    0.17700,    0.06800,    0.00000,    0.10600, & ! 3   
     &        0.13600,    0.13600,    0.13600,    0.16200,    0.31200, & ! +   
     &        1.00000,    0.97000,    0.09400,    0.00300,    1.54100, & ! 4   
     &        1.60800,    0.44200,    0.16000,    0.17200,    0.17200, & ! +   
     &        0.06800,    0.03400,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.88300,    0.04700,    0.23000,    0.23000, & ! +   
     &        0.45000,    0.91000,    0.00000,    0.50000,    0.00000, & ! 8   
     &        0.00000,    0.03500,    0.03500,    0.00000,    0.01750, & ! +   
     &        0.01750,    0.03500,    0.70000,    0.00000,    0.23400, & ! 9   
     &        0.00000,    0.10800,    0.10800,    0.23400,    0.79000, & ! +   
     &        0.00000,    0.44000,    0.00000,    0.22000,    0.22000, & ! O   
     &        0.34000,    0.70000,    0.00000,    0.40000,    0.00000, & ! +   
     &        0.95000,    0.20000,    0.60000,    0.34000,    0.40000, & ! 1   
     &        1.00000,    1.00000,    0.00000,    0.25000,    1.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    1.88000,    0.00000, & ! 2   
     &        0.70000,    0.70000,    1.00000,    0.85000,    0.00000, & ! +   
     &        0.01500,    0.00000,    0.01100,    0.01100,    0.98400, & ! 3   
     &        0.52000,    0.00000,    0.26000,    0.26000,    0.52000, & ! +   
     &        0.00000,    0.26500,    0.00000,    0.15000,    0.15000, & ! 4   
     &        0.70000,    0.72000,    0.00000,    0.42400,    0.42400, & ! +   
     &        1.00000,    1.00000,    0.15000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    0.13000,    0.12500,    1.00000, & ! 6   
     &        1.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.35000,    1.00000,    0.15000,    1.00000, & ! 7   
     &        0.50000,    0.00000,    0.00000,    0.61300,    0.00000, & ! +   
     &        0.27500,    0.27500,    0.13800,    0.13800,    0.27500, & ! 8   
     &        0.75000,    1.00000,    0.00000,    0.70000,    0.00000, & ! +   
     &        0.50000,    0.34000,    0.80200,    1.00000,    0.25000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    2.00000,    1.00000, & ! O   
     &        0.00000,    0.00000,    0.40000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.81000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.40000,    0.00000,    0.00000, & ! +   
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
     &        0.03500,    1.00000,    0.00000,    1.00000,    0.00000, & ! O   
     &        0.37600,    1.00000,    0.00000,    0.00000,    0.49100, & ! +   
     &        0.14200,    0.00000,    0.00000,    0.00400,    0.00000, & ! 1   
     &        0.02900,    0.07700,    0.12400,    0.50000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.30000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.80000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.06000,    0.52200,    0.50000,    0.06000,    0.52200, & ! +   
     &        0.00000,    0.11700,    0.25800,    0.45000,    0.00000, & ! 4   
     &        0.05000,    0.00000,    0.40000,    0.04100,    0.04800, & ! +   
     &        0.79900,    1.23300,    0.04900,    1.59000,    0.31300, & ! 5   
     &        0.72100,    0.00000,    0.00000,    0.16700,    0.31000, & ! +   
     &        0.00200,    0.00000,    0.33000,    0.02000,    0.00000, & ! 6   
     &        0.11000,    1.00000,    1.00000,    1.00000,    0.44000, & ! +   
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
     &        0.00000,    0.19500,    0.12000,    1.00000,    0.29000, & ! O   
     &        0.01600,    0.52500,    0.05100,    0.00000,    0.04900, & ! +   
     &        0.18500,    0.06500,    0.23500,    0.19200,    0.01000, & ! 1   
     &        0.19700,    0.00100,    1.05000,    0.00000,    0.30000, & ! +   
     &        0.50000,    0.29000,    0.45400,    0.52000,    0.55400, & ! 2   
     &        0.48700,    0.62700,    0.08100,    1.00000,    0.03500, & ! +   
     &        0.07000,    1.76300,    0.35300,    0.00000,    0.12800, & ! 3   
     &        0.09000,    0.22800,    0.00000,    0.08800,    0.30700, & ! +   
     &        1.19300,    0.00000,    0.56600,    0.61700,    0.61700, & ! 4   
     &        0.20100,    0.20200,    1.78600,    0.00000,    0.20100, & ! +   
     &        0.20200,    1.78600,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.10000,    1.00000, & ! +   
     &        0.03900,    1.28200,    0.11600,    0.37000,    0.00000, & ! 8   
     &        1.00000,    0.00000,    0.41400,    0.11700,    0.58900, & ! +   
     &        1.03200,    0.32200,    0.73000,    1.00000,    0.00000, & ! 9   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.02900, & ! +   
     &        0.06900,    1.16800,    0.03400,    0.00000,    0.10600, & ! 3   
     &        0.13600,    0.13600,    0.13600,    0.16200,    0.31200, & ! +   
     &        1.00000,    0.03000,    1.36100,    0.00400,    0.35200, & ! 4   
     &        0.12700,    0.00100,    0.16000,    0.17200,    0.17200, & ! +   
     &        0.03400,    0.05000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.07000,    0.07300,    0.15000,    0.15000, & ! +   
     &        0.29000,    0.75000,    0.00000,    0.50000,    0.00000, & ! 8   
     &        0.00000,    1.30000,    1.30000,    0.00000,    0.15000, & ! +   
     &        0.15000,    0.30000,    0.03500,    0.00000,    0.21600, & ! 9   
     &        0.00000,    0.10800,    0.10800,    0.23400,    0.02000, & ! +   
     &        0.00000,    0.07000,    0.00000,    0.03500,    0.03500, & ! O   
     &        0.15000,    0.12000,    0.00000,    0.40000,    0.00000, & ! +   
     &        0.70000,    0.20000,    0.60000,    0.08000,    0.00000, & ! 1   
     &        1.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    0.00000,    0.06000,    0.00000, & ! 2   
     &        0.30000,    0.30000,    1.00000,    0.45000,    0.00000, & ! +   
     &        0.08100,    0.00000,    0.31100,    0.31100,    0.98400, & ! 3   
     &        0.52000,    0.00000,    0.24000,    0.24000,    0.48000, & ! +   
     &        0.00000,    0.26500,    0.00000,    0.90000,    0.15000, & ! 4   
     &        0.70000,    0.72000,    0.00000,    0.82600,    0.07600, & ! +   
     &        0.15000,    1.00000,    0.44000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    0.00000,    0.52000,    0.10000,    0.00000, & ! 6   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.35000,    0.00000,    0.07000,    1.00000, & ! 7   
     &        0.50000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.27500,    0.27500,    0.13800,    0.13800,    0.27500, & ! 8   
     &        0.45000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.50000,    0.33000,    0.03300,    1.00000,    0.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    2.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.40000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.21000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.40000,    0.00000,    0.00000, & ! +   
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
     &        0.03500,    1.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.51000,    1.00000,    0.00000,    0.00000,    0.49100, & ! +   
     &        0.40000,    0.00000,    0.00000,    0.74400,    0.00000, & ! 1   
     &        0.84000,    0.07700,    0.12400,    0.50000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.25000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.52100,    0.17400,    0.69500,    0.52100,    0.17400, & ! +   
     &        0.00000,    0.56100,    0.00700,    0.11700,    0.00000, & ! 4   
     &        0.47500,    0.00000,    0.00000,    0.04100,    0.49800, & ! +   
     &        0.05100,    0.30000,    0.47300,    0.08700,    0.97600, & ! 5   
     &        0.10200,    0.00000,    0.00000,    0.08300,    0.50000, & ! +   
     &        0.00200,    0.00000,    1.18800,    0.02000,    0.00000, & ! 6   
     &        0.11000,    1.00000,    0.00000,    0.00000,    0.56000, & ! +   
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
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.51000, & ! O   
     &        0.98400,    0.21500,    1.00000,    0.00000,    0.70800, & ! +   
     &        0.50000,    0.06500,    0.01500,    0.00800,    0.01000, & ! 1   
     &        0.19700,    0.29700,    0.29300,    0.00000,    0.70000, & ! +   
     &        0.00000,    0.02400,    0.05400,    0.08200,    0.08700, & ! 2   
     &        0.07600,    0.12100,    0.95000,    0.00000,    0.26100, & ! +   
     &        0.07000,    0.14900,    0.04000,    0.00000,    0.12800, & ! 3   
     &        0.00500,    0.01300,    0.00000,    0.17900,    0.15600, & ! +   
     &        0.14000,    0.00000,    0.11000,    0.12800,    0.12800, & ! 4   
     &        0.20100,    0.05900,    0.46000,    0.00000,    0.20100, & ! +   
     &        0.05900,    0.46000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.10000,    1.00000, & ! +   
     &        0.84000,    0.20200,    0.11600,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.10400,    0.11700,    0.12400, & ! +   
     &        0.02600,    0.62500,    0.05100,    1.00000,    0.00000, & ! 9   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.02900, & ! +   
     &        0.06900,    0.08500,    0.05000,    0.00000,    0.89400, & ! 3   
     &        0.86400,    0.86400,    0.86400,    0.83800,    0.50300, & ! +   
     &        1.00000,    0.03000,    0.07000,    1.73700,    0.35200, & ! 4   
     &        0.12700,    1.49200,    0.84000,    0.46900,    0.46900, & ! +   
     &        0.05000,    0.01600,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.04700,    0.12000,    0.05000,    0.05000, & ! +   
     &        0.09000,    0.45000,    0.00000,    0.25000,    0.00000, & ! 8   
     &        0.00000,    0.80000,    0.80000,    0.00000,    0.90000, & ! +   
     &        0.40000,    0.80000,    0.03500,    0.00000,    0.21600, & ! 9   
     &        0.00000,    0.14500,    0.14500,    0.21600,    0.35000, & ! +   
     &        0.00000,    0.13000,    0.00000,    0.06500,    0.06500, & ! O   
     &        0.44000,    0.39000,    0.00000,    0.26000,    0.00000, & ! +   
     &        0.13000,    0.13000,    0.40000,    0.26000,    0.00000, & ! 1   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.75000,    1.00000,    0.15000,    0.00000, & ! +   
     &        0.93400,    0.00000,    0.70000,    1.20000,    0.01600, & ! 3   
     &        0.48000,    0.00000,    0.24000,    0.24000,    0.48000, & ! +   
     &        0.00000,    0.26500,    0.00000,    0.65000,    0.15000, & ! 4   
     &        0.30000,    0.13000,    0.00000,    0.07600,    0.07600, & ! +   
     &        0.85000,    1.00000,    0.44000,    1.00000,    1.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.35000,    0.12500,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.07000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.12500,    0.12500,    0.06300,    0.06300,    0.12500, & ! 8   
     &        0.29000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.50000,    0.33000,    0.03300,    1.00000,    0.00000, & ! 9   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.40000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.57000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.40000,    0.00000,    0.00000, & ! +   
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
     &        0.03500,    1.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.07400,    0.00000,    0.00000,    0.00000,    0.49100, & ! +   
     &        0.45700,    0.00000,    0.00000,    0.23900,    0.00000, & ! 1   
     &        0.09000,    0.08500,    0.07400,    0.50000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.20100,    0.43200,    0.19500,    0.20100,    0.43200, & ! +   
     &        0.00000,    0.11700,    0.00700,    0.10000,    0.00000, & ! 4   
     &        0.12400,    0.00000,    0.00000,    0.33600,    0.14000, & ! +   
     &        0.05100,    0.46700,    0.07100,    0.08700,    0.17500, & ! 5   
     &        0.10200,    0.00000,    0.00000,    0.16700,    0.18500, & ! +   
     &        0.96700,    0.00000,    0.10200,    0.02000,    0.00000, & ! 6   
     &        0.11000,    0.00000,    0.00000,    0.00000,    0.15000, & ! +   
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
     &        0.00000,    0.00000,    0.37000,    0.00000,    0.27800, & ! O   
     &        0.98400,    0.50000,    0.00000,    0.00000,    0.58000, & ! +   
     &        0.18500,    0.11500,    0.01500,    0.00800,    0.24000, & ! 1   
     &        0.00200,    1.51100,    0.29300,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.02400,    0.05400,    0.08200,    0.08700, & ! 2   
     &        0.07600,    0.12100,    0.01000,    0.00000,    0.70400, & ! +   
     &        0.02600,    0.14900,    0.10600,    0.00000,    0.58200, & ! 3   
     &        0.00500,    0.00300,    0.00000,    0.83500,    0.00800, & ! +   
     &        0.14000,    0.00000,    0.11000,    0.12800,    0.12800, & ! 4   
     &        0.00100,    0.49000,    0.46000,    0.00000,    0.00100, & ! +   
     &        0.49000,    0.46000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.10000,    0.00000, & ! +   
     &        0.08500,    0.20200,    0.19800,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.48200,    0.14500,    0.12400, & ! +   
     &        0.02600,    0.94700,    0.05100,    1.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.12400, & ! +   
     &        0.86300,    0.08500,    0.01600,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.68800, & ! +   
     &        0.00000,    0.48200,    0.07000,    0.16500,    0.02200, & ! 4   
     &        0.03600,    0.10600,    0.00000,    0.35900,    0.35900, & ! +   
     &        0.01600,    2.25800,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.66000,    0.12000,    0.06000,    0.06000, & ! +   
     &        0.11000,    0.29000,    0.00000,    0.25000,    0.00000, & ! 8   
     &        0.00000,    0.07000,    0.07000,    0.00000,    0.78500, & ! +   
     &        0.03500,    0.07000,    0.30000,    0.00000,    0.29000, & ! 9   
     &        0.00000,    0.08500,    0.08500,    0.21600,    0.59000, & ! +   
     &        0.00000,    0.31000,    0.00000,    0.15500,    0.15500, & ! O   
     &        0.07000,    0.03800,    0.00000,    0.14000,    0.00000, & ! +   
     &        0.07000,    0.07000,    0.40000,    0.07000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.25000,    0.00000,    0.70000,    0.00000, & ! +   
     &        0.93400,    0.00000,    0.00000,    0.75000,    0.01600, & ! 3   
     &        0.48000,    0.00000,    0.50000,    0.50000,    1.00000, & ! +   
     &        0.00000,    0.11000,    0.00000,    0.50000,    0.50000, & ! 4   
     &        0.30000,    0.13000,    0.00000,    0.50000,    0.50000, & ! +   
     &        0.85000,    0.00000,    0.44000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.16000,    0.20000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.85000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.82500,    0.82500,    0.91300,    0.41300,    0.82500, & ! 8   
     &        0.09000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.50000,    0.67000,    0.80200,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.40000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.19000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.08800,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.01200,    0.00000, & ! 1   
     &        0.04100,    0.14200,    0.14700,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.33400,    0.56800,    0.30500,    0.33400,    0.56800, & ! +   
     &        0.00000,    0.11400,    0.58000,    0.90000,    0.00000, & ! 4   
     &        0.05000,    0.00000,    0.00000,    0.05500,    0.12400, & ! +   
     &        0.57200,    0.23300,    0.07100,    0.30300,    0.17500, & ! 5   
     &        0.07400,    0.00000,    0.00000,    0.08300,    0.50000, & ! +   
     &        0.03100,    0.00000,    0.34000,    0.00000,    0.00000, & ! 6   
     &        0.08400,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.27800, & ! O   
     &        1.00000,    0.50000,    0.00000,    0.00000,    0.47100, & ! +   
     &        0.50000,    0.46000,    0.11500,    0.27500,    0.75000, & ! 1   
     &        0.02200,    0.33700,    0.00500,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.29000,    0.23800,    0.10000,    0.08400, & ! 2   
     &        0.28600,    0.07400,    0.05000,    0.00000,    1.00000, & ! +   
     &        0.44500,    0.00200,    0.20900,    0.00000,    0.01000, & ! 3   
     &        0.30300,    0.03400,    0.00000,    0.51000,    0.21200, & ! +   
     &        0.07200,    0.00000,    0.15800,    0.08800,    0.08800, & ! 4   
     &        0.41100,    0.12100,    0.01200,    0.00000,    0.41100, & ! +   
     &        0.12100,    0.01200,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.10000,    0.00000, & ! +   
     &        0.03600,    0.00900,    0.11600,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.10600,    0.50200,    0.07400, & ! +   
     &        0.21600,    1.00000,    0.04200,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.30600, & ! +   
     &        0.45700,    0.27500,    2.25800,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.06100, & ! +   
     &        0.00000,    0.48800,    0.07800,    0.16500,    0.08000, & ! 4   
     &        0.20600,    0.10600,    0.00000,    0.00000,    0.00000, & ! +   
     &        2.25800,    0.58200,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.10000,    0.00000,    0.02000,    0.02000, & ! +   
     &        0.05000,    0.09000,    0.00000,    1.00000,    0.00000, & ! 8   
     &        0.00000,    0.23000,    0.23000,    0.00000,    0.11500, & ! +   
     &        0.11500,    0.23000,    0.80000,    0.00000,    0.17000, & ! 9   
     &        0.00000,    0.54500,    0.54500,    0.29000,    0.15000, & ! +   
     &        0.00000,    0.31000,    0.00000,    0.15500,    0.15500, & ! O   
     &        0.13000,    0.02900,    0.00000,    1.60000,    0.00000, & ! +   
     &        0.30000,    0.30000,    0.26000,    0.16000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.50000,    0.00000,    0.70000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.25000,    1.00000, & ! 3   
     &        0.00000,    0.00000,    0.75000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.89000,    0.00000,    0.25000,    0.00000, & ! 4   
     &        0.30000,    0.15000,    0.00000,    0.25000,    0.00000, & ! +   
     &        0.15000,    0.00000,    0.44000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.71000,    0.05000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.85000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.20000,    0.20000,    0.10000,    0.10000,    1.20000, & ! 8   
     &        0.11000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.34000,    0.54100,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.19000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.50400,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.01200,    0.00000, & ! 1   
     &        0.02000,    0.78200,    0.13900,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.40700,    0.65200,    0.00000,    0.40700,    0.65200, & ! +   
     &        0.00000,    0.27400,    0.19000,    0.33300,    0.00000, & ! 4   
     &        0.95000,    0.00000,    0.00000,    0.12900,    0.21000, & ! +   
     &        0.22700,    0.00000,    0.00200,    0.16300,    0.01100, & ! 5   
     &        0.06100,    0.00000,    0.00000,    0.25000,    0.00000, & ! +   
     &        0.03100,    0.00000,    0.05000,    0.00000,    0.00000, & ! 6   
     &        0.08400,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.10000, & ! O   
     &        0.00000,    0.18500,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.37500,    0.12000,    0.11500,    0.12200,    0.25000, & ! 1   
     &        0.77600,    0.33700,    0.00700,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.57000,    0.15100,    0.38000,    0.23800, & ! 2   
     &        0.11200,    0.40500,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.12200,    0.02900,    0.07100,    0.00000,    0.00700, & ! 3   
     &        0.08800,    0.77400,    0.00000,    0.14400,    0.00300, & ! +   
     &        0.57900,    0.00000,    0.10000,    0.31200,    0.31200, & ! 4   
     &        0.38500,    0.12100,    0.02300,    0.00000,    0.38500, & ! +   
     &        0.12100,    0.02300,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.06500,    0.01800,    0.54100,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.10400,    0.23700,    0.14700, & ! +   
     &        0.48400,    0.00000,    0.04200,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.54000, & ! +   
     &        0.47300,    0.17700,    0.58200,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.31200, & ! +   
     &        0.00000,    1.00000,    0.34000,    0.00300,    0.25800, & ! 4   
     &        0.07200,    0.19000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.58200,    0.58200,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.04300,    0.00000,    0.50000,    0.50000, & ! +   
     &        1.00000,    0.11000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.25000, & ! +   
     &        0.25000,    0.50000,    0.07000,    0.00000,    0.09000, & ! 9   
     &        0.00000,    0.04500,    0.04500,    0.17000,    0.13000, & ! +   
     &        0.00000,    0.72000,    0.00000,    1.11000,    0.36000, & ! O   
     &        0.31000,    0.73000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.50000,    0.50000,    0.14000,    0.56000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.70000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.25000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.12500,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.93000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.37500,    0.37500,    0.93800,    0.18800,    0.37500, & ! 8   
     &        0.05000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.33000,    0.08200,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.19000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.37600,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.01200,    0.00000, & ! 1   
     &        0.07500,    0.02600,    0.56500,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.12900,    0.65200,    0.00000,    0.12900,    0.65200, & ! +   
     &        0.00000,    0.15300,    0.36600,    0.10000,    0.00000, & ! 4   
     &        0.35100,    0.00000,    0.00000,    0.01300,    0.02300, & ! +   
     &        0.21800,    0.00000,    0.21100,    0.78000,    0.42900, & ! 5   
     &        0.21400,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.03300,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.01200, & ! O   
     &        0.00000,    0.07500,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.12500,    0.35500,    0.00100,    0.40000,    0.00000, & ! 1   
     &        0.03400,    0.02900,    0.68400,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.02900,    0.18100,    0.15900,    0.18500, & ! 2   
     &        0.15900,    0.11200,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.02400,    0.43800,    0.08600,    0.00000,    0.66600, & ! 3   
     &        0.50000,    0.16900,    0.00000,    0.08000,    0.00300, & ! +   
     &        0.16300,    0.00000,    0.12300,    0.13400,    0.13400, & ! 4   
     &        0.03700,    0.24900,    0.00200,    0.00000,    0.03700, & ! +   
     &        0.24900,    0.00200,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.07000,    0.01200,    0.00700,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.19700,    0.18600,    0.13900, & ! +   
     &        0.27400,    0.00000,    0.71200,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.67100,    0.58200,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.34300,    0.03400,    0.04400, & ! 4   
     &        0.21500,    0.38300,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.58200,    0.54800,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.08000,    0.00000,    0.25000,    0.00000, & ! +   
     &        0.00000,    0.05000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.25000, & ! +   
     &        0.00000,    0.00000,    0.23000,    0.00000,    0.09000, & ! 9   
     &        0.00000,    1.00000,    0.50000,    0.09000,    0.08000, & ! +   
     &        0.00000,    0.15000,    0.00000,    0.07500,    0.07500, & ! O   
     &        0.31000,    0.01700,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.25000,    0.00000,    0.60000,    0.28000,    0.00000, & ! 1   
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
     &        0.07400,    0.07400,    0.03700,    0.03700,    0.07400, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.33000,    0.18000,    0.00000,    0.00000, & ! 9   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.20500,    0.00000, & ! 1   
     &        0.08400,    0.05800,    0.02400,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.10700,    0.65200,    0.00000,    0.10700,    0.65200, & ! +   
     &        0.00000,    0.01900,    0.18400,    0.10000,    0.00000, & ! 4   
     &        0.05000,    0.00000,    0.00000,    0.15000,    0.74200, & ! +   
     &        0.00800,    0.00000,    0.00100,    1.00000,    0.00100, & ! 5   
     &        0.23000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.29000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00100,    0.19200,    0.00000, & ! 1   
     &        0.02000,    0.05100,    0.06900,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.26100,    0.06500,    0.04100,    0.16100, & ! 2   
     &        0.08800,    0.02200,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.33200,    0.23600,    0.40700,    0.00000,    0.00700, & ! 3   
     &        0.01100,    0.83100,    0.00000,    0.00200,    0.29900, & ! +   
     &        0.11600,    0.00000,    0.07200,    0.07700,    0.07700, & ! 4   
     &        0.00700,    0.06300,    0.40300,    0.00000,    0.00700, & ! +   
     &        0.06300,    0.40300,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.84000,    0.05500,    0.02200,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.28500,    0.67600,    0.56500, & ! +   
     &        0.27400,    0.00000,    0.49800,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.06700,    0.03500,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.07500,    0.28700,    0.04100, & ! 4   
     &        0.01900,    0.31700,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.03500,    0.03500,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.80300,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 9   
     &        0.00000,    0.75000,    0.00000,    0.09000,    0.60000, & ! +   
     &        0.00000,    1.34000,    0.00000,    0.17000,    0.17000, & ! O   
     &        0.72000,    0.36000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.04000,    0.00000, & ! 1   
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
     &        0.25100,    0.25100,    0.12600,    0.12600,    0.25100, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.33000,    0.54100,    0.00000,    0.00000, & ! 9   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.00000,    0.03400,    0.00000, & ! 1   
     &        0.16000,    0.69800,    0.44800,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.26700,    0.00000,    0.00000,    0.26700,    0.00000, & ! +   
     &        0.00000,    0.19500,    0.35000,    0.10000,    0.00000, & ! 4   
     &        0.05000,    0.00000,    0.00000,    0.33200,    0.10000, & ! +   
     &        0.57200,    0.00000,    0.08300,    0.00000,    0.03600, & ! 5   
     &        0.07400,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.75000,    0.20400,    0.00000, & ! 1   
     &        0.02300,    0.01700,    0.00200,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.11600,    0.19500,    0.33600,    0.04700, & ! 2   
     &        0.04500,    0.03600,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.98300,    0.42600,    1.00000,    0.00000,    0.03600, & ! 3   
     &        0.50000,    0.00000,    0.00000,    0.01200,    0.16100, & ! +   
     &        0.00200,    0.00000,    0.18500,    0.02600,    0.02600, & ! 4   
     &        0.00300,    0.12700,    0.23900,    0.00000,    0.00300, & ! +   
     &        0.12700,    0.23900,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.15900,    0.23700,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.58600,    0.28000,    0.02400, & ! +   
     &        0.48400,    0.00000,    0.19500,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.15800,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.25300,    0.41200,    0.37800, & ! 4   
     &        0.03800,    0.08600,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.15800,    0.15800,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.25000,    0.00000,    1.00000,    0.35000, & ! +   
     &        0.00000,    0.35000,    0.00000,    0.67500,    0.17500, & ! O   
     &        0.15000,    0.16000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.28000,    0.00000, & ! 1   
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
     &        0.00000,    1.00000,    0.50000,    0.50000,    1.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.83500,    0.00000,    0.00000, & ! 9   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.00000,    0.25600,    0.00000, & ! 1   
     &        0.00000,    0.85800,    0.02600,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.78300,    0.00000,    0.00000,    0.78300,    0.00000, & ! +   
     &        0.00000,    0.19500,    0.35000,    0.00000,    0.00000, & ! 4   
     &        0.05000,    0.00000,    0.00000,    0.15000,    0.37200, & ! +   
     &        0.85000,    0.00000,    0.14300,    0.00000,    0.00400, & ! 5   
     &        0.06300,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.25000,    0.39000,    0.00000, & ! 1   
     &        1.00000,    0.34400,    0.05600,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.31400,    0.19500,    0.14400,    0.25300, & ! 2   
     &        0.06700,    0.08800,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.01700,    0.10600,    0.00000,    0.00000,    0.00100, & ! 3   
     &        0.04400,    0.00000,    0.00000,    0.02300,    0.13100, & ! +   
     &        0.32000,    0.00000,    0.20200,    0.22100,    0.22100, & ! 4   
     &        0.00900,    0.03300,    0.00500,    0.00000,    0.00900, & ! +   
     &        0.03300,    0.00500,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.54700,    0.10900,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.85500,    0.44800, & ! +   
     &        0.78400,    0.00000,    0.01700,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.18500,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.98300,    0.24700,    1.00000, & ! 4   
     &        0.19200,    0.04200,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.18500,    0.18500,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.50000,    0.50000, & ! O   
     &        0.34000,    0.34000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.24000,    0.00000, & ! 1   
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
     &        0.00000,    0.00000,    0.25000,    0.00000,    0.00000, & ! 8   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.03000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.23100,    0.13900,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.17400,    0.04700, & ! +   
     &        0.00000,    0.00000,    0.40200,    0.00000,    0.01000, & ! 5   
     &        0.00800,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.16000,    0.00000, & ! 1   
     &        1.00000,    0.24000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.31200,    0.23900,    0.25300, & ! 2   
     &        0.27800,    0.35200,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.14600,    0.00000,    0.00000,    0.01200, & ! 3   
     &        0.00300,    0.00000,    0.00000,    0.31900,    0.11400, & ! +   
     &        0.31900,    0.00000,    0.30900,    0.24700,    0.24700, & ! 4   
     &        0.00300,    0.20800,    0.00100,    0.00000,    0.00300, & ! +   
     &        0.20800,    0.00100,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.04500,    0.59100,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.02600, & ! +   
     &        0.00000,    0.00000,    0.00900,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.27400,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.01700,    0.07600,    0.00000, & ! 4   
     &        0.33700,    0.02500,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.27400,    0.27400,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.25000,    0.00000, & ! O   
     &        0.35000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.25200,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.79400,    0.00300,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.71100,    0.00100, & ! +   
     &        0.00000,    0.00000,    0.11500,    0.00000,    0.17000, & ! 5   
     &        0.12400,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.15000,    0.00000, & ! 1   
     &        0.00000,    0.34500,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.07300,    0.04700,    0.19800, & ! 2   
     &        0.28600,    0.23000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00900, & ! 3   
     &        0.00900,    0.00000,    0.00000,    0.68100,    0.45300, & ! +   
     &        0.68100,    0.00000,    0.36900,    0.17800,    0.17800, & ! 4   
     &        0.00200,    0.05700,    0.00400,    0.00000,    0.00200, & ! +   
     &        0.05700,    0.00400,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.30000,    0.05100,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.03000, & ! +   
     &        0.00000,    0.00000,    0.00900,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00700,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.13000,    0.00000, & ! 4   
     &        0.16900,    0.05800,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00700,    0.00700,    0.00000,    0.00000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.07300,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00400,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.04800, & ! +   
     &        0.00000,    0.00000,    0.32900,    0.00000,    0.00800, & ! 5   
     &        0.08300,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.10000,    0.00000, & ! 1   
     &        0.00000,    0.00800,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.43500,    0.55500,    0.05500, & ! 2   
     &        0.10200,    0.15100,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.16800, & ! 3   
     &        0.18500,    0.00000,    0.00000,    0.00000,    0.07100, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.06800,    0.06800, & ! 4   
     &        0.40900,    0.00200,    0.22800,    0.00000,    0.40900, & ! +   
     &        0.00200,    0.22800,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.02000,    0.04000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.25200, & ! +   
     &        0.00000,    0.00000,    0.11500,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00300,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 4   
     &        0.83100,    0.16100,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00300,    0.00300,    0.00000,    0.00000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.07300,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00300,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00700,    0.00000,    0.03100, & ! 5   
     &        0.19000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.00000,    0.00000,    0.20000,    0.00000, & ! 1   
     &        0.00000,    0.00200,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    1.00000,    0.58600, & ! 2   
     &        0.46100,    0.04300,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.16900, & ! 3   
     &        0.15900,    0.00000,    0.00000,    0.00000,    0.33300, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.05700,    0.05700, & ! 4   
     &        1.00000,    0.17200,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.17200,    1.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00300,    0.68600,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.07300, & ! +   
     &        0.00000,    0.00000,    0.14000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00300,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.01300,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00300,    0.00300,    0.00000,    0.00000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.71300,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.09500,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.52800,    0.00000,    0.18900, & ! 5   
     &        0.26100,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.08100,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 2   
     &        1.00000,    0.70500,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.83100, & ! 3   
     &        0.26800,    0.00000,    0.00000,    0.00000,    0.01900, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.10100,    0.10100, & ! 4   
     &        1.00000,    0.06800,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.06800,    1.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.04100,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.07300, & ! +   
     &        0.00000,    0.00000,    0.42000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.15800,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.19100,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.15800,    0.15800,    0.00000,    0.00000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.16300,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.30500, & ! 5   
     &        0.06600,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.25500,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.01100,    0.00000,    0.00000,    0.00000,    0.05100, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! 4   
     &        0.00000,    0.00300,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00300,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.04600,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.71300, & ! +   
     &        0.00000,    0.00000,    0.76200,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00600,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.31900,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00600,    0.00600,    0.00000,    0.00000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.16300,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.15700, & ! 5   
     &        0.59100,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
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
     &        0.00000,    0.73700,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.05200,    0.00000,    0.00000,    0.00000,    0.03300, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.03900,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.03900,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.54700,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00600,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.68100,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00600,    0.00600,    0.00000,    0.00000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.09500,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.63600, & ! 5   
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
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00100, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00200,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00200,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.90800,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00100,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00100,    0.00100,    0.00000,    0.00000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.26400,    0.00000,    0.00000, & ! 4   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.02400, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00100,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00100,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    0.10900,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.10900,    0.10900,    0.00000,    0.00000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.06500, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.50200,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.50200,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.23500, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.42800,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.42800,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.03700, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.07300, & ! +   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.13600, & ! +   
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
     &        0.00000,    0.00000,    0.00000/           !        8   

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
     &      2,    2,    2,    2,    2,    2,    1,    1,    2,    1, & ! 9   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    1,    1,    2,    2,    2,    1,    2, & ! 9   
     &      2,    1,    2,    2,    1,    2,    1,    2,    2,    2, & ! O   
     &      2,    2,    1,    2,    1,    2,    1,    2,    1,    1, & ! 1   
     &      1,    2,    2,    1,    2,    2,    1,    2,    2,    2, & ! 2   
     &      1,    1,    2,    1,    2,    2,    2,    1,    2,    2, & ! 3   
     &      1,    2,    2,    2,    2,    2,    2,    1,    2,    2, & ! 4   
     &      2,    1,    2,    1,    2,    1,    1,    2,    2,    2, & ! 5   
     &      2,    2,    1,    2,    1,    2,    1,    2,    2,    2, & ! 6   
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
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    1,    2,    1,    2,    2,    1,    1, & ! 5   
     &      2,    2,    2,    2,    2,    2,    1,    1,    1,    2, & ! 6   
     &      2,    1,    2,    2,    1,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    1,    2,    2,    1,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    1,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      1,    1,    2,    2,    2,    2,    1,    2,    2,    2, & ! 6   
     &      2,    2,    2,    1,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    1,    2,    1,    2,    2,    1, & ! 6   
     &      1,    2,    1,    2,    1,    2,    1,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    1,    2,    2,    2,    2, & ! 8   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    1,    2,    1,    1,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    1,    1,    1, & ! 4   
     &      1,    1,    1,    2,    2,    2,    1,    1,    1,    1, & ! 5   
     &      1,    1,    2,    2,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    2, & ! 7   
     &      2,    2,    2/     !  8   
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
     &      3,    3,    4,    6,    3,    1,    2,    6,    1,    2, & ! 9   
     &      1,    1,    3,    2,    1,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    1,    2,    1,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    1, & ! 4   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 5   
     &      1,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    1,    2,    1,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    1,    2,    3,    1,    3,    2, & ! 9   
     &      6,    6,    2,    4,    3,   10,    5,    2,    2,    6, & ! O   
     &      7,    3,    3,   12,    3,   11,   12,   17,    6,    2, & ! 1   
     &      2,    3,    4,    3,    2,    3,    1,    5,    2,    1, & ! 2   
     &      1,    0,    1,    0,    2,   12,   10,    7,   12,   10, & ! 3   
     &      1,   14,   21,   11,    1,   12,    2,    4,   14,   15, & ! 4   
     &     12,    7,   17,   10,   20,   19,    3,    2,    8,    7, & ! 5   
     &      9,    1,    8,    6,    3,    8,    5,    4,    4,    6, & ! 6   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      1,    2,    1,    2,    2,    2,    1,    1,    1,    1, & ! 4   
     &      1,    2,    1,    2,    2,    2,    1,    1,    1,    1, & ! 5   
     &      1,    2,    1,    2,    2,    2,    1,    1,    1,    1, & ! 6   
     &      2,    1,    3,    3,    3,    3,    3,    3,    3,    3, & ! 7   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      1,    5,    6,    4,   10,    7,    9,    5,    2,    8, & ! O   
     &      9,   10,   12,   16,    8,   13,   20,   14,    2,    5, & ! 1   
     &      4,   13,   16,   16,   17,   17,   18,    7,    4,    7, & ! 2   
     &     12,   14,   11,    2,   17,   19,   10,    3,   14,   26, & ! 3   
     &     14,    3,   15,   18,   18,   17,   24,   17,    3,   17, & ! 4   
     &     24,   17,    3,    1,    1,    2,    1,    1,    2,    2, & ! 5   
     &      1,    2,    1,    2,    2,    1,    2,    2,    2,    2, & ! 6   
     &      1,    2,    2,    1,    2,    3,    2,    3,    7,    5, & ! 7   
     &     11,   20,   16,    4,    3,    4,    2,   11,   12,   18, & ! 8   
     &     12,    7,   18,    6,    1,    2,    5,    2,    1,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    4,    9, & ! 2   
     &      9,   11,   22,    2,    5,    5,    5,    5,    5,    8, & ! 3   
     &      5,    8,   13,   15,   12,   15,   19,    5,    6,    6, & ! 4   
     &     22,   22,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      0,    0,    1,    1,    1,    1,    0,    1,    1,    1, & ! 6   
     &      1,    1,    1,    0,    1,    2,   10,    6,    9,    8, & ! 7   
     &      8,    9,    2,    7,    2,    2,    7,    7,    1,    9, & ! 8   
     &      8,    8,    9,    1,   10,    1,   11,    9,   11,   11, & ! 9   
     &      1,   11,    1,   13,   12,   13,   12,    1,    7,    1, & ! O   
     &      9,    8,    9,   12,    3,    5,    5,    1,    3,    4, & ! 1   
     &      5,    5,    3,    4,    1,    4,    7,    5,    8,    2, & ! 2   
     &      6,    1,    5,    7,    7,    6,    1,    8,    6,    7, & ! 3   
     &      1,    7,    1,    7,    6,    7,    7,    1,    7,    6, & ! 4   
     &      7,    5,    7,    5,    5,    4,    4,    4,    8,    6, & ! 5   
     &      4,    3,    7,    8,    3,    3,    4,    2,    2,    3, & ! 6   
     &      3,    5,    3,    8,    4,    4,    2,    2,    3,    1, & ! 7   
     &     10,   11,   12,   11,   11,    8,    3,    2,    3,    2, & ! 8   
     &      6,   10,   11,    5,    3,    5,    5,    4,    4,    4, & ! 9   
     &      8,    6,    4,    4,    3,    1,    2,    6,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    3, & ! 1   
     &      1,    1,    1,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    8,    0,    0,    2,    1,    2,    2, & ! 4   
     &      2,    1,    1,    1,    1,    1,    0,    1,    1,    1, & ! 5   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    3, & ! 7   
     &      1,    3,    1/     !  8   

      INTEGER, PARAMETER :: NMPHOT =  64
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,   16,   17,   18,   19,   23,   28,   34,   41,   65, & ! O   
     &     75,   86,   98,  194,  195,  199,  202,  205,  207,  213, & ! 1   
     &    215,  217,  219,  220,  221,  224,  227,  231,  232,  234, & ! 2   
     &    238,  241,  248,  252,  254,  256,  257,  263,  265,  267, & ! 3   
     &    554,  556,  559,  560,  567,  568,  572,  594,  597,  661, & ! 4   
     &    662,  667,  674,  684,  765,  767,  770,  771,  773,  775, & ! 5   
     &    777,  786,  792,  808/     !  6   

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & ! O   
     &     10,   10,   10,   11,   12,   13,   14,   15,   16,   17, & ! 1   
     &     17,   17,   17,   18,   19,   20,   21,    1,    1,   22, & ! 2   
     &     23,   23,   24,   25,   16,   26,   27,   28,   29,   29, & ! 3   
     &     30,   31,   32,   33,   34,   35,   36,   37,   38,   11, & ! 4   
     &     12,   13,   28,   39,   16,   40,   40,   40,   40,   14, & ! 5   
     &     20,   17,   25,   10/     !  6   

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & ! O   
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & ! 1   
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & ! 2   
     &     31,   32,   33,   34,   35,   36,   37,   38,   39,   40, & ! 3   
     &     41,   42,   43,   44,   45,   46,   47,   48,   49,   50, & ! 4   
     &     51,   52,   53,   54,   55,   56,   57,   58,   59,   60, & ! 5   
     &     61,   62,   63,   64/     !  6   

      INTEGER, PARAMETER :: MHETERO =  22
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    848,  849,  850,  851,  852,  853,  854,  855,  856,  858, & 
     &    859,  860,  861,  862,  863,  864,  865,  866,  867,  868, & 
     &    881,  883/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    2,    1,    3,    4,    5,    6,    6,    7,    8, & 
     &      9,    9,   10,   11,   12,   13,   14,   15,   14,   15, & 
     &     16,   17/

      INTEGER, PARAMETER :: NPHOTAB =  40
      CHARACTER( 16 )    :: PHOTAB( NPHOTAB )

      DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & 
     &   'NO2_06          ', 'NO3NO_06        ', 'NO3NO2_6        ', & 
     &   'O3O1D_06        ', 'O3O3P_06        ', 'HONO_06         ', & 
     &   'HNO3            ', 'HNO4_06         ', 'H2O2            ', & 
     &   'PAN             ', 'HCHOR_06        ', 'HCHOM_06        ', & 
     &   'CCHO_R          ', 'C2CHO           ', 'ACET_06         ', & 
     &   'MEK_06          ', 'COOH            ', 'GLY_07R         ', & 
     &   'GLY_07M         ', 'MGLY_06         ', 'BACL_07         ', & 
     &   'BALD_06         ', 'AFG1            ', 'MVK_06          ', & 
     &   'MACR_06         ', 'IC3ONO2         ', 'HOCCHO_IUPAC    ', & 
     &   'ACRO_09         ', 'PAA             ', 'CL2             ', & 
     &   'CLNO_06         ', 'CLONO           ', 'CLNO2           ', & 
     &   'CLONO2_1        ', 'CLONO2_2        ', 'HOCL_06         ', & 
     &   'CLCCHO          ', 'CLACET          ', 'HPALD           ', & 
     &   'NOA             '/

      INTEGER, PARAMETER :: NHETERO =  17
      CHARACTER( 16 )    :: HETERO( NHETERO )

      DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & 
     &   'HETERO_N2O5IJ   ', 'HETERO_NO2      ', 'HETERO_N2O5K    ', &
     &   'HETERO_H2NO3PAIJ', 'HETERO_H2NO3PAK ', 'HETERO_H2NO3PBIJ', &
     &   'HETERO_H2NO3PBK ', 'HETERO_IEPOX    ', 'HETERO_IMAE     ', &
     &   'HETERO_TETROL   ', 'HETERO_IEPOXOS  ', 'HETERO_TETROLDIM', &
     &   'HETERO_IEPOXOSDI', 'HETERO_2MG      ', 'HETERO_IMAEOS   ', &
     &   'HETERO_PNCOMLI  ', 'HETERO_PNCOMLJ  '/

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
     &    'BR52            ', 'BR53            ', 'BR64            ', & ! 2   
     &    'BR65            ', 'BR66            ', 'BR67            ', & ! 3   
     &    'BR68            ', 'R019            ', 'R020            ', & ! 4   
     &    'R021            ', 'R022            ', 'R023            ', & ! 5   
     &    'R024            ', 'R025            ', 'R026            ', & ! 6   
     &    'R027            ', 'R028            ', 'R029            ', & ! 7   
     &    'R030            ', 'R031            ', 'R032            ', & ! 8   
     &    'R033            ', 'R034            ', 'R035            ', & ! 9   
     &    'R036            ', 'R037            ', 'R038            ', & ! 0   
     &    'R039            ', 'R040            ', 'R041            ', & ! 1   
     &    'R042            ', 'R043            ', 'R044            ', & ! 2   
     &    'R045            ', 'R046            ', 'R047            ', & ! 3   
     &    'R048            ', 'R049            ', 'R050            ', & ! 4   
     &    'R051            ', 'R052            ', 'R053            ', & ! 5   
     &    'R054            ', 'R055            ', 'R056            ', & ! 6   
     &    'R057            ', 'R058            ', 'R059            ', & ! 7   
     &    'R060            ', 'R061            ', 'R062            ', & ! 8   
     &    'R063            ', 'R064            ', 'R065            ', & ! 9   
     &    'R066            ', 'R067            ', 'R068            ', & ! 0   
     &    'R069            ', 'R070            ', 'R071            ', & ! 1   
     &    'R072            ', 'R073            ', 'R074            ', & ! 2   
     &    'R075            ', 'R076            ', 'R077            ', & ! 3   
     &    'R078            ', 'R079            ', 'R080            ', & ! 4   
     &    'R081            ', 'R082            ', 'R083            ', & ! 5   
     &    'R084            ', 'R085            ', 'R086            ', & ! 6   
     &    'R087            ', 'R088            ', 'R089            ', & ! 7   
     &    'R090            ', 'R091            ', 'R092            ', & ! 8   
     &    'R093            ', 'R094            ', 'R095            ', & ! 9   
     &    'R096            ', 'R097            ', 'R098            ', & ! 0   
     &    'R099            ', 'R100            ', 'R101            ', & ! 1   
     &    'R102            ', 'R103            ', 'R104            ', & ! 2   
     &    'R105            ', 'R106            ', 'R107            ', & ! 3   
     &    'R108            ', 'BP01            ', 'BP02            ', & ! 4   
     &    'BP03            ', 'BP07            ', 'BP08            ', & ! 5   
     &    'BP09            ', 'BP10            ', 'BP11            ', & ! 6   
     &    'BP12            ', 'BP13            ', 'BP14            ', & ! 7   
     &    'BP15            ', 'BP16            ', 'BP17            ', & ! 8   
     &    'BP18            ', 'BP19            ', 'BP20            ', & ! 9   
     &    'BP21            ', 'BP22            ', 'BP23            ', & ! 0   
     &    'BP24            ', 'BP25            ', 'BP26            ', & ! 1   
     &    'BP27            ', 'BP28            ', 'BP29            ', & ! 2   
     &    'BP30            ', 'BP31            ', 'BP32            ', & ! 3   
     &    'BP33            ', 'BP34            ', 'BP35            ', & ! 4   
     &    'BP36            ', 'BP37            ', 'BP38            ', & ! 5   
     &    'BP39            ', 'BP40            ', 'BP41            ', & ! 6   
     &    'BP42            ', 'BP43            ', 'BP44            ', & ! 7   
     &    'BP45            ', 'BP46            ', 'BP47            ', & ! 8   
     &    'BP48            ', 'BP49            ', 'BP50            ', & ! 9   
     &    'BP51            ', 'BP52            ', 'BP53            ', & ! 0   
     &    'BP55            ', 'BP57            ', 'BP60            ', & ! 1   
     &    'BP62            ', 'BP63            ', 'BP64            ', & ! 2   
     &    'BP65            ', 'BP66            ', 'BP67            ', & ! 3   
     &    'BP68            ', 'BP69            ', 'BP70            ', & ! 4   
     &    'BP71            ', 'BP73            ', 'BP74            ', & ! 5   
     &    'BP75            ', 'BP76            ', 'BP77            ', & ! 6   
     &    'BP78            ', 'BP79            ', 'BP80            ', & ! 7   
     &    'BP81            ', 'BP82            ', 'BP83            ', & ! 8   
     &    'BP84            ', 'BP85            ', 'BP86            ', & ! 9   
     &    'P001            ', 'P002            ', 'P003            ', & ! 0   
     &    'P004            ', 'P005            ', 'P006            ', & ! 1   
     &    'P007            ', 'P008            ', 'P009            ', & ! 2   
     &    'P010            ', 'P011            ', 'P012            ', & ! 3   
     &    'P013            ', 'P014            ', 'P015            ', & ! 4   
     &    'P016            ', 'P017            ', 'P018            ', & ! 5   
     &    'P019            ', 'P020            ', 'P021            ', & ! 6   
     &    'P022            ', 'P023            ', 'P024            ', & ! 7   
     &    'P025            ', 'P026            ', 'P027            ', & ! 8   
     &    'P028            ', 'P029            ', 'P030            ', & ! 9   
     &    'P031            ', 'P032            ', 'P033            ', & ! 0   
     &    'P034            ', 'P035            ', 'P036            ', & ! 1   
     &    'P037            ', 'P038            ', 'P039            ', & ! 2   
     &    'P040            ', 'P041            ', 'P042            ', & ! 3   
     &    'P043            ', 'P044            ', 'P045            ', & ! 4   
     &    'P046            ', 'P047            ', 'P048            ', & ! 5   
     &    'P049            ', 'P050            ', 'P051            ', & ! 6   
     &    'P052            ', 'P053            ', 'P054            ', & ! 7   
     &    'P055            ', 'P056            ', 'P057            ', & ! 8   
     &    'P058            ', 'P059            ', 'P060            ', & ! 9   
     &    'P061            ', 'P062            ', 'P063            ', & ! 0   
     &    'P064            ', 'P065            ', 'P066            ', & ! 1   
     &    'P067            ', 'P068            ', 'P069            ', & ! 2   
     &    'P070            ', 'P071            ', 'P072            ', & ! 3   
     &    'P073            ', 'P074            ', 'P075            ', & ! 4   
     &    'P076            ', 'P077            ', 'P078            ', & ! 5   
     &    'P079            ', 'P080            ', 'P081            ', & ! 6   
     &    'P082            ', 'P083            ', 'P084            ', & ! 7   
     &    'P085            ', 'P086            ', 'P087            ', & ! 8   
     &    'P088            ', 'P089            ', 'P090            ', & ! 9   
     &    'P091            ', 'P092            ', 'P093            ', & ! 0   
     &    'P094            ', 'P095            ', 'P096            ', & ! 1   
     &    'P097            ', 'P098            ', 'P099            ', & ! 2   
     &    'P100            ', 'P101            ', 'P102            ', & ! 3   
     &    'P103            ', 'P104            ', 'P105            ', & ! 4   
     &    'P106            ', 'P107            ', 'P108            ', & ! 5   
     &    'P109            ', 'P110            ', 'P111            ', & ! 6   
     &    'P112            ', 'P113            ', 'P114            ', & ! 7   
     &    'P115            ', 'P116            ', 'P117            ', & ! 8   
     &    'P118            ', 'P119            ', 'P120            ', & ! 9   
     &    'P121            ', 'P122            ', 'P123            ', & ! 0   
     &    'P124            ', 'P125            ', 'P126            ', & ! 1   
     &    'P127            ', 'P128            ', 'P129            ', & ! 2   
     &    'P130            ', 'P131            ', 'P132            ', & ! 3   
     &    'P133            ', 'P134            ', 'P135            ', & ! 4   
     &    'P136            ', 'P137            ', 'P138            ', & ! 5   
     &    'P139            ', 'P140            ', 'P141            ', & ! 6   
     &    'P142            ', 'P143            ', 'P144            ', & ! 7   
     &    'P145            ', 'P146            ', 'P147            ', & ! 8   
     &    'P148            ', 'P149            ', 'P150            ', & ! 9   
     &    'P151            ', 'P152            ', 'P153            ', & ! 0   
     &    'P154            ', 'P155            ', 'P156            ', & ! 1   
     &    'P157            ', 'P158            ', 'P159            ', & ! 2   
     &    'P160            ', 'P161            ', 'P162            ', & ! 3   
     &    'P163            ', 'P164            ', 'P165            ', & ! 4   
     &    'P166            ', 'P167            ', 'P168            ', & ! 5   
     &    'P169            ', 'P170            ', 'P171            ', & ! 6   
     &    'P172            ', 'P173            ', 'P174            ', & ! 7   
     &    'P175            ', 'P176            ', 'P177            ', & ! 8   
     &    'P178            ', 'P179            ', 'P180            ', & ! 9   
     &    'P181            ', 'P182            ', 'P183            ', & ! 0   
     &    'P184            ', 'P185            ', 'P186            ', & ! 1   
     &    'P187            ', 'P188            ', 'P189            ', & ! 2   
     &    'P190            ', 'P191            ', 'P192            ', & ! 3   
     &    'P193            ', 'P194            ', 'P195            ', & ! 4   
     &    'P196            ', 'P197            ', 'P198            ', & ! 5   
     &    'P199            ', 'P200            ', 'P201            ', & ! 6   
     &    'P202            ', 'P203            ', 'P204            ', & ! 7   
     &    'P205            ', 'P206            ', 'P207            ', & ! 8   
     &    'P208            ', 'P209            ', 'P210            ', & ! 9   
     &    'P211            ', 'P212            ', 'P213            ', & ! 0   
     &    'P214            ', 'P215            ', 'P216            ', & ! 1   
     &    'P217            ', 'P218            ', 'P219            ', & ! 2   
     &    'P220            ', 'P221            ', 'P222            ', & ! 3   
     &    'P223            ', 'P224            ', 'P225            ', & ! 4   
     &    'P226            ', 'P227            ', 'P228            ', & ! 5   
     &    'P229            ', 'P230            ', 'BE01            ', & ! 6   
     &    'BE02            ', 'BE03            ', 'BE04            ', & ! 7   
     &    'BE05            ', 'BT01            ', 'BT02            ', & ! 8   
     &    'BT03            ', 'BT04            ', 'BT05            ', & ! 9   
     &    'BT06            ', 'BT07            ', 'BT08            ', & ! 0   
     &    'BE07            ', 'BE09            ', 'BT09            ', & ! 1   
     &    'BT10            ', 'BT11            ', 'BT12            ', & ! 2   
     &    'BE10            ', 'BE11            ', 'BE12            ', & ! 3   
     &    'BT13            ', 'BT14            ', 'BT15            ', & ! 4   
     &    'BT16            ', 'BT17            ', 'BT18            ', & ! 5   
     &    'BL01            ', 'BL02            ', 'BL03            ', & ! 6   
     &    'BL04            ', 'BL05            ', 'AALK            ', & ! 7   
     &    'BL06            ', 'BL07            ', 'BL08            ', & ! 8   
     &    'BL09            ', 'BL10            ', 'BL11            ', & ! 9   
     &    'BL12            ', 'BL13            ', 'BL14            ', & ! 0   
     &    'BL15a           ', 'BL15b           ', 'BL16            ', & ! 1   
     &    'BL17            ', 'BL18            ', 'BL19            ', & ! 2   
     &    'BT19            ', 'BT20            ', 'BT21            ', & ! 3   
     &    'BT22            ', 'CI01            ', 'CI02            ', & ! 4   
     &    'CI03            ', 'CI04            ', 'CI05            ', & ! 5   
     &    'CI06            ', 'CI07            ', 'CI08            ', & ! 6   
     &    'CI09            ', 'CI10            ', 'CI11            ', & ! 7   
     &    'CI12            ', 'CI13            ', 'CI14            ', & ! 8   
     &    'CI15            ', 'CI16            ', 'CI17            ', & ! 9   
     &    'CI18            ', 'CI19            ', 'CI20            ', & ! 0   
     &    'CI21            ', 'CI22            ', 'CP01            ', & ! 1   
     &    'CP02            ', 'CP03            ', 'CP04            ', & ! 2   
     &    'CP05            ', 'CP06            ', 'CP07            ', & ! 3   
     &    'CP08            ', 'CP09            ', 'CP10            ', & ! 4   
     &    'CP11            ', 'CP12            ', 'CP13            ', & ! 5   
     &    'CP14            ', 'CP15            ', 'TP01            ', & ! 6   
     &    'CP17            ', 'CP18            ', 'CP19            ', & ! 7   
     &    'CP20            ', 'CP21            ', 'CP22            ', & ! 8   
     &    'CP29            ', 'CP30            ', 'CP31            ', & ! 9   
     &    'CP32            ', 'CP33            ', 'CP34            ', & ! 0   
     &    'CP35            ', 'CP36            ', 'CP37            ', & ! 1   
     &    'CP38            ', 'CP39            ', 'CP40            ', & ! 2   
     &    'CP41            ', 'CP42            ', 'CP43            ', & ! 3   
     &    'CP44            ', 'CP45            ', 'CP46            ', & ! 4   
     &    'CP47            ', 'CP48            ', 'CP49            ', & ! 5   
     &    'CP50            ', 'CP51            ', 'CP52            ', & ! 6   
     &    'CP53            ', 'CP54            ', 'CP55            ', & ! 7   
     &    'CP56            ', 'CP57            ', 'CP58            ', & ! 8   
     &    'CE01            ', 'CE02            ', 'TE01            ', & ! 9   
     &    'TE02            ', 'CE03            ', 'TE03            ', & ! 0   
     &    'CE04            ', 'TE04            ', 'TE05            ', & ! 1   
     &    'TE06            ', 'TE07            ', 'TE08            ', & ! 2   
     &    'TE09            ', 'BC01            ', 'BC02            ', & ! 3   
     &    'BC03            ', 'BC04            ', 'BC05            ', & ! 4   
     &    'BC06            ', 'BC07            ', 'BC08            ', & ! 5   
     &    'BC09a           ', 'BC09b           ', 'BC10            ', & ! 6   
     &    'BC11            ', 'AE51            ', 'AE52            ', & ! 7   
     &    'AE53            ', 'AE54            ', 'AE55            ', & ! 8   
     &    'AE56            ', 'AE57            ', 'AE58            ', & ! 9   
     &    'TR01            ', 'TR02            ', 'TR03            ', & ! 0   
     &    'TR05            ', 'TR06            ', 'TR07            ', & ! 1   
     &    'TR08            ', 'TR09            ', 'TR10            ', & ! 2   
     &    'TR11            ', 'TR12            ', 'TR13            ', & ! 3   
     &    'TR14            ', 'TR15            ', 'TR16            ', & ! 4   
     &    'IS1             ', 'IS2             ', 'IS3             ', & ! 5   
     &    'IS4             ', 'IS5             ', 'IS6             ', & ! 6   
     &    'IS7             ', 'IS107           ', 'IS137           ', & ! 7   
     &    'IS138           ', 'IS9             ', 'IS10            ', & ! 8   
     &    'IS11            ', 'IS12            ', 'IS13            ', & ! 9   
     &    'IS14            ', 'IS140           ', 'IS15            ', & ! 0   
     &    'IS17            ', 'IS18            ', 'IS19            ', & ! 1   
     &    'IS20            ', 'IS21            ', 'IS22            ', & ! 2   
     &    'IS24            ', 'IS25            ', 'IS26            ', & ! 3   
     &    'IS141           ', 'IS142           ', 'IS143           ', & ! 4   
     &    'IS144           ', 'IS27            ', 'IS28            ', & ! 5   
     &    'IS29            ', 'IS145           ', 'IS146           ', & ! 6   
     &    'IS147           ', 'IS148           ', 'IS30            ', & ! 7   
     &    'IS31            ', 'IS32            ', 'IS34            ', & ! 8   
     &    'IS109           ', 'IS36            ', 'IS38            ', & ! 9   
     &    'IS40            ', 'IS41            ', 'IS33            ', & ! 0   
     &    'IS35            ', 'IS37            ', 'IS39            ', & ! 1   
     &    'IS43            ', 'IS44            ', 'IS46            ', & ! 2   
     &    'IS47            ', 'IS48            ', 'IS50            ', & ! 3   
     &    'IS51            ', 'IS52            ', 'IS53            ', & ! 4   
     &    'IS55            ', 'IS102           ', 'IS103           ', & ! 5   
     &    'IS104           ', 'IS105           ', 'IS56            ', & ! 6   
     &    'IS57            ', 'IS58            ', 'IS59            ', & ! 7   
     &    'IS60            ', 'IS61            ', 'IS63            ', & ! 8   
     &    'IS64            ', 'IS65            ', 'IS66            ', & ! 9   
     &    'IS67            ', 'IS69            ', 'IS70            ', & ! 0   
     &    'IS71            ', 'IS72            ', 'IS73            ', & ! 1   
     &    'IS74            ', 'IS75            ', 'IS76            ', & ! 2   
     &    'IS77            ', 'IS78            ', 'IS108           ', & ! 3   
     &    'IS79            ', 'IS80            ', 'IS81            ', & ! 4   
     &    'IS82            ', 'IS111           ', 'IS83            ', & ! 5   
     &    'IS93            ', 'IS97            ', 'IS98            ', & ! 6   
     &    'IS84            ', 'IS106           ', 'IS85            ', & ! 7   
     &    'IS110           ', 'IS86            ', 'IS87            ', & ! 8   
     &    'IS88            ', 'IS89            ', 'IS90            ', & ! 9   
     &    'IS91            ', 'IS96            ', 'IS112           ', & ! 0   
     &    'IS113           ', 'IS114           ', 'IS92            ', & ! 1   
     &    'IS94            ', 'IS99            ', 'IS139           ', & ! 2   
     &    'IS00            ', 'BP56            ', 'BP58            ', & ! 3   
     &    'CP16            ', 'IA69            ', 'IA70            ', & ! 4   
     &    'IA71            ', 'IA72            ', 'IA73            ', & ! 5   
     &    'IA74            ', 'IA75            ', 'IA76            ', & ! 6   
     &    'IA77            ', 'IA78            ', 'IA79            ', & ! 7   
     &    'IA80            ', 'IA51            ', 'IA52            ', & ! 8   
     &    'IA53            ', 'IC01            ', 'IC02            ', & ! 9   
     &    'IC03            ', 'IC04            ', 'IC05            ', & ! 0   
     &    'IC06            ', 'IC07            ', 'IC08            ', & ! 1   
     &    'IC09            ', 'IC10            ', 'IC11            ', & ! 2   
     &    'IC12            ', 'IC13            ', 'IC14            ', & ! 3   
     &    'IC15            ', 'IC16            ', 'IC17            ', & ! 4   
     &    'IC18            ', 'IC19            ', 'IC20            ', & ! 5   
     &    'IC21            ', 'IC22            ', 'IC23            ', & ! 6   
     &    'IC24            ', 'IC25            ', 'IC26            ', & ! 7   
     &    'IC27            ', 'IC28            ', 'IC29            ', & ! 8   
     &    'IC30            ', 'IC31            ', 'IC32            ', & ! 9   
     &    'IC33            ', 'IC34            ', 'IC35            ', & ! 0   
     &    'IA108           ', 'IA90            ', 'IA91            ', & ! 1   
     &    'IA92            ', 'HET_N2O5        ', 'HET_N02         ', & ! 2   
     &    'HET_N2O5IJ      ', 'HET_N2O5K       ', 'HET_H2NO3PIJA   ', & ! 3   
     &    'HET_H2NO3PKA    ', 'HET_H2NO3PIB    ', 'HET_H2NO3PJB    ', & ! 4   
     &    'HET_H2NO3PKB    ', 'HAL_Ozone       ', 'HET_IEPOX       ', & ! 5   
     &    'HET_IMAE        ', 'HET_IHMML       ', 'HET_TETROL      ', & ! 6   
     &    'HET_IEPOXOS     ', 'HET_DIM1        ', 'HET_DIM2        ', & ! 7   
     &    'HET_2MG1        ', 'HET_IMAEOS1     ', 'HET_2MG2        ', & ! 8   
     &    'HET_IMAEOS2     ', 'OLIG_ALK1       ', 'OLIG_ALK2       ', & ! 9   
     &    'OLIG_XYLENE1    ', 'OLIG_XYLENE2    ', 'OLIG_TOLUENE1   ', & ! 0   
     &    'OLIG_TOLUENE2   ', 'OLIG_BENZENE1   ', 'OLIG_BENZENE2   ', & ! 1   
     &    'OLIG_TERPENE1   ', 'OLIG_TERPENE2   ', 'OLIG_SESQT1     ', & ! 2   
     &    'RPOAGEPI        ', 'RPOAGELI        ', 'RPOAGEPJ        ', & ! 3   
     &    'RPOAGELJ        '/                   !                 4  

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
       INTEGER, PARAMETER  :: IJ_MVK_06           =  24
       INTEGER, PARAMETER  :: IJ_MACR_06          =  25
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
       INTEGER, PARAMETER  :: IJ_HPALD            =  39
       INTEGER, PARAMETER  :: IJ_NOA              =  40
       INTEGER, PARAMETER  :: IK_HETERO_N2O5IJ    =   1
       INTEGER, PARAMETER  :: IK_HETERO_NO2       =   2
       INTEGER, PARAMETER  :: IK_HETERO_N2O5K     =   3
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAIJ =   4
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAK  =   5
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBIJ =   6
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBK  =   7
       INTEGER, PARAMETER  :: IK_HETERO_IEPOX     =   8
       INTEGER, PARAMETER  :: IK_HETERO_IMAE      =   9
       INTEGER, PARAMETER  :: IK_HETERO_TETROL    =  10
       INTEGER, PARAMETER  :: IK_HETERO_IEPOXOS   =  11
       INTEGER, PARAMETER  :: IK_HETERO_TETROLDIM =  12
       INTEGER, PARAMETER  :: IK_HETERO_IEPOXOSDI =  13
       INTEGER, PARAMETER  :: IK_HETERO_2MG       =  14
       INTEGER, PARAMETER  :: IK_HETERO_IMAEOS    =  15
       INTEGER, PARAMETER  :: IK_HETERO_PNCOMLI   =  16
       INTEGER, PARAMETER  :: IK_HETERO_PNCOMLJ   =  17
       END MODULE RXNS_DATA
