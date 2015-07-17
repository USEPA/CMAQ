       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: /home/sgq/util/MP/cb05e51_ae6_aq/mech.def
! for Mechanism Name: CB05E51_AE6_AQ                  

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

      CHARACTER( 32 ), PARAMETER :: MECHNAME = 'CB05E51_AE6_AQ'

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC = 122
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 147

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
      DATA GAS_CHEM_SPC( 109 ) / 'FORM_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 110 ) / 'ALD2_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 111 ) / 'BUTADIENE13     ' /
      DATA GAS_CHEM_SPC( 112 ) / 'ACROLEIN        ' /
      DATA GAS_CHEM_SPC( 113 ) / 'ACROLEIN_PRIMARY' /
      DATA GAS_CHEM_SPC( 114 ) / 'TOLU            ' /
      DATA GAS_CHEM_SPC( 115 ) / 'MXYL            ' /
      DATA GAS_CHEM_SPC( 116 ) / 'OXYL            ' /
      DATA GAS_CHEM_SPC( 117 ) / 'PXYL            ' /
      DATA GAS_CHEM_SPC( 118 ) / 'APIN            ' /
      DATA GAS_CHEM_SPC( 119 ) / 'BPIN            ' /
      DATA GAS_CHEM_SPC( 120 ) / 'HG              ' /
      DATA GAS_CHEM_SPC( 121 ) / 'HGIIAER         ' /
      DATA GAS_CHEM_SPC( 122 ) / 'HGIIGAS         ' /




      LOGICAL   :: HALOGEN_PARAMETER = .TRUE. 


! MAPPED_TO_CGRID declares whether CMAQ namelists were used to determine 
! the below values of CGRID_INDEX, SPECIES_TYPE, SPECIES_MOLWT, and CONVERT_CONC
      LOGICAL, PARAMETER, PRIVATE :: F = .FALSE.
      LOGICAL, PARAMETER, PRIVATE :: T = .TRUE.


      LOGICAL   :: MAPPED_TO_CGRID   = .FALSE. 

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
      DATA CHEMISTRY_SPC(  16 ), CGRID_INDEX(  16 ), SPECIES_TYPE(  16 ), SPECIES_MOLWT(  16 ), CONVERT_CONC(  16 ) / 'NTROH           ',   16, 'GC',  135.00, F /
      DATA CHEMISTRY_SPC(  17 ), CGRID_INDEX(  17 ), SPECIES_TYPE(  17 ), SPECIES_MOLWT(  17 ), CONVERT_CONC(  17 ) / 'NTRALK          ',   17, 'GC',  119.00, F /
      DATA CHEMISTRY_SPC(  18 ), CGRID_INDEX(  18 ), SPECIES_TYPE(  18 ), SPECIES_MOLWT(  18 ), CONVERT_CONC(  18 ) / 'ROOH            ',   18, 'GC',   62.00, F /
      DATA CHEMISTRY_SPC(  19 ), CGRID_INDEX(  19 ), SPECIES_TYPE(  19 ), SPECIES_MOLWT(  19 ), CONVERT_CONC(  19 ) / 'ALD2            ',   19, 'GC',   44.00, F /
      DATA CHEMISTRY_SPC(  20 ), CGRID_INDEX(  20 ), SPECIES_TYPE(  20 ), SPECIES_MOLWT(  20 ), CONVERT_CONC(  20 ) / 'ALDX            ',   20, 'GC',   44.00, F /
      DATA CHEMISTRY_SPC(  21 ), CGRID_INDEX(  21 ), SPECIES_TYPE(  21 ), SPECIES_MOLWT(  21 ), CONVERT_CONC(  21 ) / 'ISOPX           ',   21, 'GC',  118.10, F /
      DATA CHEMISTRY_SPC(  22 ), CGRID_INDEX(  22 ), SPECIES_TYPE(  22 ), SPECIES_MOLWT(  22 ), CONVERT_CONC(  22 ) / 'IEPOX           ',   22, 'GC',  118.10, F /
      DATA CHEMISTRY_SPC(  23 ), CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), SPECIES_MOLWT(  23 ), CONVERT_CONC(  23 ) / 'ISOPO2          ',   23, 'GC',  117.10, F /
      DATA CHEMISTRY_SPC(  24 ), CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), SPECIES_MOLWT(  24 ), CONVERT_CONC(  24 ) / 'IOLE            ',   24, 'GC',   48.00, F /
      DATA CHEMISTRY_SPC(  25 ), CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), SPECIES_MOLWT(  25 ), CONVERT_CONC(  25 ) / 'IEPXO2          ',   25, 'GC',  149.10, F /
      DATA CHEMISTRY_SPC(  26 ), CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), SPECIES_MOLWT(  26 ), CONVERT_CONC(  26 ) / 'MGLY            ',   26, 'GC',   72.00, F /
      DATA CHEMISTRY_SPC(  27 ), CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), SPECIES_MOLWT(  27 ), CONVERT_CONC(  27 ) / 'FORM            ',   27, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC(  28 ), CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), SPECIES_MOLWT(  28 ), CONVERT_CONC(  28 ) / 'FACD            ',   28, 'GC',   46.00, F /
      DATA CHEMISTRY_SPC(  29 ), CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), SPECIES_MOLWT(  29 ), CONVERT_CONC(  29 ) / 'CO              ',   29, 'GC',   28.00, F /
      DATA CHEMISTRY_SPC(  30 ), CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), SPECIES_MOLWT(  30 ), CONVERT_CONC(  30 ) / 'PAR             ',   30, 'GC',   14.00, F /
      DATA CHEMISTRY_SPC(  31 ), CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), SPECIES_MOLWT(  31 ), CONVERT_CONC(  31 ) / 'C2O3            ',   31, 'GC',   75.00, F /
      DATA CHEMISTRY_SPC(  32 ), CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), SPECIES_MOLWT(  32 ), CONVERT_CONC(  32 ) / 'MEO2            ',   32, 'GC',   47.00, F /
      DATA CHEMISTRY_SPC(  33 ), CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), SPECIES_MOLWT(  33 ), CONVERT_CONC(  33 ) / 'AACD            ',   33, 'GC',   60.00, F /
      DATA CHEMISTRY_SPC(  34 ), CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), SPECIES_MOLWT(  34 ), CONVERT_CONC(  34 ) / 'MEPX            ',   34, 'GC',   48.00, F /
      DATA CHEMISTRY_SPC(  35 ), CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), SPECIES_MOLWT(  35 ), CONVERT_CONC(  35 ) / 'MEOH            ',   35, 'GC',   32.00, F /
      DATA CHEMISTRY_SPC(  36 ), CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), SPECIES_MOLWT(  36 ), CONVERT_CONC(  36 ) / 'HCO3            ',   36, 'GC',   63.00, F /
      DATA CHEMISTRY_SPC(  37 ), CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), SPECIES_MOLWT(  37 ), CONVERT_CONC(  37 ) / 'PAN             ',   37, 'GC',  121.00, F /
      DATA CHEMISTRY_SPC(  38 ), CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), SPECIES_MOLWT(  38 ), CONVERT_CONC(  38 ) / 'PACD            ',   38, 'GC',   76.00, F /
      DATA CHEMISTRY_SPC(  39 ), CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), SPECIES_MOLWT(  39 ), CONVERT_CONC(  39 ) / 'CXO3            ',   39, 'GC',   75.00, F /
      DATA CHEMISTRY_SPC(  40 ), CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), SPECIES_MOLWT(  40 ), CONVERT_CONC(  40 ) / 'PANX            ',   40, 'GC',  121.00, F /
      DATA CHEMISTRY_SPC(  41 ), CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), SPECIES_MOLWT(  41 ), CONVERT_CONC(  41 ) / 'ROR             ',   41, 'GC',   31.00, F /
      DATA CHEMISTRY_SPC(  42 ), CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), SPECIES_MOLWT(  42 ), CONVERT_CONC(  42 ) / 'OLE             ',   42, 'GC',   27.00, F /
      DATA CHEMISTRY_SPC(  43 ), CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), SPECIES_MOLWT(  43 ), CONVERT_CONC(  43 ) / 'ETH             ',   43, 'GC',   28.00, F /
      DATA CHEMISTRY_SPC(  44 ), CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), SPECIES_MOLWT(  44 ), CONVERT_CONC(  44 ) / 'TOL             ',   44, 'GC',   92.00, F /
      DATA CHEMISTRY_SPC(  45 ), CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), SPECIES_MOLWT(  45 ), CONVERT_CONC(  45 ) / 'CRES            ',   45, 'GC',  108.00, F /
      DATA CHEMISTRY_SPC(  46 ), CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), SPECIES_MOLWT(  46 ), CONVERT_CONC(  46 ) / 'TO2             ',   46, 'GC',  173.00, F /
      DATA CHEMISTRY_SPC(  47 ), CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), SPECIES_MOLWT(  47 ), CONVERT_CONC(  47 ) / 'TOLRO2          ',   47, 'GC',  141.00, F /
      DATA CHEMISTRY_SPC(  48 ), CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), SPECIES_MOLWT(  48 ), CONVERT_CONC(  48 ) / 'OPEN            ',   48, 'GC',   84.00, F /
      DATA CHEMISTRY_SPC(  49 ), CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), SPECIES_MOLWT(  49 ), CONVERT_CONC(  49 ) / 'CRO             ',   49, 'GC',  107.00, F /
      DATA CHEMISTRY_SPC(  50 ), CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), SPECIES_MOLWT(  50 ), CONVERT_CONC(  50 ) / 'CAT1            ',   50, 'GC',  124.00, F /
      DATA CHEMISTRY_SPC(  51 ), CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), SPECIES_MOLWT(  51 ), CONVERT_CONC(  51 ) / 'CRON            ',   51, 'GC',  153.00, F /
      DATA CHEMISTRY_SPC(  52 ), CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), SPECIES_MOLWT(  52 ), CONVERT_CONC(  52 ) / 'CRNO            ',   52, 'GC',  152.00, F /
      DATA CHEMISTRY_SPC(  53 ), CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), SPECIES_MOLWT(  53 ), CONVERT_CONC(  53 ) / 'CRN2            ',   53, 'GC',  168.00, F /
      DATA CHEMISTRY_SPC(  54 ), CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), SPECIES_MOLWT(  54 ), CONVERT_CONC(  54 ) / 'CRPX            ',   54, 'GC',  169.00, F /
      DATA CHEMISTRY_SPC(  55 ), CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), SPECIES_MOLWT(  55 ), CONVERT_CONC(  55 ) / 'OPO3            ',   55, 'GC',  115.00, F /
      DATA CHEMISTRY_SPC(  56 ), CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), SPECIES_MOLWT(  56 ), CONVERT_CONC(  56 ) / 'CAO2            ',   56, 'GC',  133.00, F /
      DATA CHEMISTRY_SPC(  57 ), CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), SPECIES_MOLWT(  57 ), CONVERT_CONC(  57 ) / 'OPAN            ',   57, 'GC',  161.00, F /
      DATA CHEMISTRY_SPC(  58 ), CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), SPECIES_MOLWT(  58 ), CONVERT_CONC(  58 ) / 'XYLMN           ',   58, 'GC',  106.00, F /
      DATA CHEMISTRY_SPC(  59 ), CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), SPECIES_MOLWT(  59 ), CONVERT_CONC(  59 ) / 'XYLRO2          ',   59, 'GC',  155.00, F /
      DATA CHEMISTRY_SPC(  60 ), CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), SPECIES_MOLWT(  60 ), CONVERT_CONC(  60 ) / 'NAPH            ',   60, 'GC',  128.20, F /
      DATA CHEMISTRY_SPC(  61 ), CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), SPECIES_MOLWT(  61 ), CONVERT_CONC(  61 ) / 'PAHRO2          ',   61, 'GC',  187.20, F /
      DATA CHEMISTRY_SPC(  62 ), CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), SPECIES_MOLWT(  62 ), CONVERT_CONC(  62 ) / 'ISOP            ',   62, 'GC',   68.00, F /
      DATA CHEMISTRY_SPC(  63 ), CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), SPECIES_MOLWT(  63 ), CONVERT_CONC(  63 ) / 'ISPD            ',   63, 'GC',   70.00, F /
      DATA CHEMISTRY_SPC(  64 ), CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), SPECIES_MOLWT(  64 ), CONVERT_CONC(  64 ) / 'ISOPRXN         ',   64, 'GC',   68.00, F /
      DATA CHEMISTRY_SPC(  65 ), CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), SPECIES_MOLWT(  65 ), CONVERT_CONC(  65 ) / 'NTRM            ',   65, 'GC',  147.00, F /
      DATA CHEMISTRY_SPC(  66 ), CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), SPECIES_MOLWT(  66 ), CONVERT_CONC(  66 ) / 'MACO3           ',   66, 'GC',  102.00, F /
      DATA CHEMISTRY_SPC(  67 ), CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), SPECIES_MOLWT(  67 ), CONVERT_CONC(  67 ) / 'NTRI            ',   67, 'GC',  149.10, F /
      DATA CHEMISTRY_SPC(  68 ), CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), SPECIES_MOLWT(  68 ), CONVERT_CONC(  68 ) / 'TERP            ',   68, 'GC',  136.00, F /
      DATA CHEMISTRY_SPC(  69 ), CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), SPECIES_MOLWT(  69 ), CONVERT_CONC(  69 ) / 'TRPRXN          ',   69, 'GC',  136.00, F /
      DATA CHEMISTRY_SPC(  70 ), CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), SPECIES_MOLWT(  70 ), CONVERT_CONC(  70 ) / 'XO2T            ',   70, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  71 ), CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), SPECIES_MOLWT(  71 ), CONVERT_CONC(  71 ) / 'SO2             ',   71, 'GC',   64.00, F /
      DATA CHEMISTRY_SPC(  72 ), CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), SPECIES_MOLWT(  72 ), CONVERT_CONC(  72 ) / 'SULF            ',   72, 'GC',   98.00, F /
      DATA CHEMISTRY_SPC(  73 ), CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), SPECIES_MOLWT(  73 ), CONVERT_CONC(  73 ) / 'SULRXN          ',   73, 'GC',   98.00, F /
      DATA CHEMISTRY_SPC(  74 ), CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), SPECIES_MOLWT(  74 ), CONVERT_CONC(  74 ) / 'ETOH            ',   74, 'GC',   46.00, F /
      DATA CHEMISTRY_SPC(  75 ), CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), SPECIES_MOLWT(  75 ), CONVERT_CONC(  75 ) / 'ETHA            ',   75, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC(  76 ), CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), SPECIES_MOLWT(  76 ), CONVERT_CONC(  76 ) / 'CL2             ',   76, 'GC',   71.00, F /
      DATA CHEMISTRY_SPC(  77 ), CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), SPECIES_MOLWT(  77 ), CONVERT_CONC(  77 ) / 'CL              ',   77, 'GC',   35.50, F /
      DATA CHEMISTRY_SPC(  78 ), CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), SPECIES_MOLWT(  78 ), CONVERT_CONC(  78 ) / 'HOCL            ',   78, 'GC',   52.50, F /
      DATA CHEMISTRY_SPC(  79 ), CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), SPECIES_MOLWT(  79 ), CONVERT_CONC(  79 ) / 'CLO             ',   79, 'GC',   51.50, F /
      DATA CHEMISTRY_SPC(  80 ), CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), SPECIES_MOLWT(  80 ), CONVERT_CONC(  80 ) / 'FMCL            ',   80, 'GC',   64.50, F /
      DATA CHEMISTRY_SPC(  81 ), CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), SPECIES_MOLWT(  81 ), CONVERT_CONC(  81 ) / 'HCL             ',   81, 'GC',   36.50, F /
      DATA CHEMISTRY_SPC(  82 ), CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), SPECIES_MOLWT(  82 ), CONVERT_CONC(  82 ) / 'CLNO2           ',   82, 'GC',   81.50, F /
      DATA CHEMISTRY_SPC(  83 ), CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), SPECIES_MOLWT(  83 ), CONVERT_CONC(  83 ) / 'TOLNRXN         ',   83, 'GC',  141.00, F /
      DATA CHEMISTRY_SPC(  84 ), CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), SPECIES_MOLWT(  84 ), CONVERT_CONC(  84 ) / 'TOLHRXN         ',   84, 'GC',  141.00, F /
      DATA CHEMISTRY_SPC(  85 ), CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), SPECIES_MOLWT(  85 ), CONVERT_CONC(  85 ) / 'XYLNRXN         ',   85, 'GC',  155.00, F /
      DATA CHEMISTRY_SPC(  86 ), CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), SPECIES_MOLWT(  86 ), CONVERT_CONC(  86 ) / 'XYLHRXN         ',   86, 'GC',  155.00, F /
      DATA CHEMISTRY_SPC(  87 ), CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), SPECIES_MOLWT(  87 ), CONVERT_CONC(  87 ) / 'BENZENE         ',   87, 'GC',   78.00, F /
      DATA CHEMISTRY_SPC(  88 ), CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), SPECIES_MOLWT(  88 ), CONVERT_CONC(  88 ) / 'BENZRO2         ',   88, 'GC',  127.00, F /
      DATA CHEMISTRY_SPC(  89 ), CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), SPECIES_MOLWT(  89 ), CONVERT_CONC(  89 ) / 'BNZNRXN         ',   89, 'GC',  127.00, F /
      DATA CHEMISTRY_SPC(  90 ), CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), SPECIES_MOLWT(  90 ), CONVERT_CONC(  90 ) / 'BNZHRXN         ',   90, 'GC',  127.00, F /
      DATA CHEMISTRY_SPC(  91 ), CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), SPECIES_MOLWT(  91 ), CONVERT_CONC(  91 ) / 'SESQ            ',   91, 'GC',  204.00, F /
      DATA CHEMISTRY_SPC(  92 ), CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), SPECIES_MOLWT(  92 ), CONVERT_CONC(  92 ) / 'SESQRXN         ',   92, 'GC',  204.00, F /
      DATA CHEMISTRY_SPC(  93 ), CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), SPECIES_MOLWT(  93 ), CONVERT_CONC(  93 ) / 'PAHNRXN         ',   93, 'GC',  187.20, F /
      DATA CHEMISTRY_SPC(  94 ), CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), SPECIES_MOLWT(  94 ), CONVERT_CONC(  94 ) / 'PAHHRXN         ',   94, 'GC',  187.20, F /
      DATA CHEMISTRY_SPC(  95 ), CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), SPECIES_MOLWT(  95 ), CONVERT_CONC(  95 ) / 'SOAALK          ',   95, 'GC',  112.00, F /
      DATA CHEMISTRY_SPC(  96 ), CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), SPECIES_MOLWT(  96 ), CONVERT_CONC(  96 ) / 'ALKRXN          ',   96, 'GC',  112.00, F /
      DATA CHEMISTRY_SPC(  97 ), CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), SPECIES_MOLWT(  97 ), CONVERT_CONC(  97 ) / 'MAPAN           ',   97, 'GC',  148.00, F /
      DATA CHEMISTRY_SPC(  98 ), CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), SPECIES_MOLWT(  98 ), CONVERT_CONC(  98 ) / 'NALKO2          ',   98, 'GC',  106.00, F /
      DATA CHEMISTRY_SPC(  99 ), CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), SPECIES_MOLWT(  99 ), CONVERT_CONC(  99 ) / 'NTRCN           ',   99, 'GC',  147.00, F /
      DATA CHEMISTRY_SPC( 100 ), CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), SPECIES_MOLWT( 100 ), CONVERT_CONC( 100 ) / 'NTRCNOH         ',  100, 'GC',  149.00, F /
      DATA CHEMISTRY_SPC( 101 ), CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), SPECIES_MOLWT( 101 ), CONVERT_CONC( 101 ) / 'NTRPX           ',  101, 'GC',  151.00, F /
      DATA CHEMISTRY_SPC( 102 ), CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), SPECIES_MOLWT( 102 ), CONVERT_CONC( 102 ) / 'NOHO2           ',  102, 'GC',  106.00, F /
      DATA CHEMISTRY_SPC( 103 ), CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), SPECIES_MOLWT( 103 ), CONVERT_CONC( 103 ) / 'NCNO2           ',  103, 'GC',  106.00, F /
      DATA CHEMISTRY_SPC( 104 ), CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), SPECIES_MOLWT( 104 ), CONVERT_CONC( 104 ) / 'NCNOHO2         ',  104, 'GC',  106.00, F /
      DATA CHEMISTRY_SPC( 105 ), CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), SPECIES_MOLWT( 105 ), CONVERT_CONC( 105 ) / 'NTRMO2          ',  105, 'GC',  106.00, F /
      DATA CHEMISTRY_SPC( 106 ), CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), SPECIES_MOLWT( 106 ), CONVERT_CONC( 106 ) / 'NTRIO2          ',  106, 'GC',  106.00, F /
      DATA CHEMISTRY_SPC( 107 ), CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), SPECIES_MOLWT( 107 ), CONVERT_CONC( 107 ) / 'H2NO3PIJ        ',  107, 'GC',   64.00, F /
      DATA CHEMISTRY_SPC( 108 ), CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), SPECIES_MOLWT( 108 ), CONVERT_CONC( 108 ) / 'H2NO3PK         ',  108, 'GC',   64.00, F /
      DATA CHEMISTRY_SPC( 109 ), CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), SPECIES_MOLWT( 109 ), CONVERT_CONC( 109 ) / 'ACLI            ',  181, 'AE',   35.50, T /
      DATA CHEMISTRY_SPC( 110 ), CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), SPECIES_MOLWT( 110 ), CONVERT_CONC( 110 ) / 'ACLJ            ',  180, 'AE',   35.50, T /
      DATA CHEMISTRY_SPC( 111 ), CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), SPECIES_MOLWT( 111 ), CONVERT_CONC( 111 ) / 'ACLK            ',  183, 'AE',   35.50, T /
      DATA CHEMISTRY_SPC( 112 ), CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), SPECIES_MOLWT( 112 ), CONVERT_CONC( 112 ) / 'AISO3J          ',  189, 'AE',  168.20, T /
      DATA CHEMISTRY_SPC( 113 ), CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), SPECIES_MOLWT( 113 ), CONVERT_CONC( 113 ) / 'AXYL1J          ',  132, 'AE',  192.00, T /
      DATA CHEMISTRY_SPC( 114 ), CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), SPECIES_MOLWT( 114 ), CONVERT_CONC( 114 ) / 'AOLGAJ          ',  190, 'AE',  176.40, T /
      DATA CHEMISTRY_SPC( 115 ), CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), SPECIES_MOLWT( 115 ), CONVERT_CONC( 115 ) / 'AXYL2J          ',  133, 'AE',  192.00, T /
      DATA CHEMISTRY_SPC( 116 ), CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), SPECIES_MOLWT( 116 ), CONVERT_CONC( 116 ) / 'ATOL1J          ',  135, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 117 ), CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), SPECIES_MOLWT( 117 ), CONVERT_CONC( 117 ) / 'ATOL2J          ',  136, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 118 ), CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), SPECIES_MOLWT( 118 ), CONVERT_CONC( 118 ) / 'ABNZ1J          ',  138, 'AE',  144.00, T /
      DATA CHEMISTRY_SPC( 119 ), CGRID_INDEX( 119 ), SPECIES_TYPE( 119 ), SPECIES_MOLWT( 119 ), CONVERT_CONC( 119 ) / 'ABNZ2J          ',  139, 'AE',  144.00, T /
      DATA CHEMISTRY_SPC( 120 ), CGRID_INDEX( 120 ), SPECIES_TYPE( 120 ), SPECIES_MOLWT( 120 ), CONVERT_CONC( 120 ) / 'ATRP1J          ',  144, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 121 ), CGRID_INDEX( 121 ), SPECIES_TYPE( 121 ), SPECIES_MOLWT( 121 ), CONVERT_CONC( 121 ) / 'AOLGBJ          ',  191, 'AE',  252.00, T /
      DATA CHEMISTRY_SPC( 122 ), CGRID_INDEX( 122 ), SPECIES_TYPE( 122 ), SPECIES_MOLWT( 122 ), CONVERT_CONC( 122 ) / 'ATRP2J          ',  145, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 123 ), CGRID_INDEX( 123 ), SPECIES_TYPE( 123 ), SPECIES_MOLWT( 123 ), CONVERT_CONC( 123 ) / 'AISO1J          ',  146, 'AE',   96.00, T /
      DATA CHEMISTRY_SPC( 124 ), CGRID_INDEX( 124 ), SPECIES_TYPE( 124 ), SPECIES_MOLWT( 124 ), CONVERT_CONC( 124 ) / 'AISO2J          ',  147, 'AE',   96.00, T /
      DATA CHEMISTRY_SPC( 125 ), CGRID_INDEX( 125 ), SPECIES_TYPE( 125 ), SPECIES_MOLWT( 125 ), CONVERT_CONC( 125 ) / 'ASQTJ           ',  148, 'AE',  378.00, T /
      DATA CHEMISTRY_SPC( 126 ), CGRID_INDEX( 126 ), SPECIES_TYPE( 126 ), SPECIES_MOLWT( 126 ), CONVERT_CONC( 126 ) / 'APAH1J          ',  141, 'AE',  243.00, T /
      DATA CHEMISTRY_SPC( 127 ), CGRID_INDEX( 127 ), SPECIES_TYPE( 127 ), SPECIES_MOLWT( 127 ), CONVERT_CONC( 127 ) / 'APAH2J          ',  142, 'AE',  243.00, T /
      DATA CHEMISTRY_SPC( 128 ), CGRID_INDEX( 128 ), SPECIES_TYPE( 128 ), SPECIES_MOLWT( 128 ), CONVERT_CONC( 128 ) / 'AALK1J          ',  130, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 129 ), CGRID_INDEX( 129 ), SPECIES_TYPE( 129 ), SPECIES_MOLWT( 129 ), CONVERT_CONC( 129 ) / 'AALK2J          ',  131, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 130 ), CGRID_INDEX( 130 ), SPECIES_TYPE( 130 ), SPECIES_MOLWT( 130 ), CONVERT_CONC( 130 ) / 'APOCI           ',  151, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 131 ), CGRID_INDEX( 131 ), SPECIES_TYPE( 131 ), SPECIES_MOLWT( 131 ), CONVERT_CONC( 131 ) / 'APNCOMI         ',  153, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 132 ), CGRID_INDEX( 132 ), SPECIES_TYPE( 132 ), SPECIES_MOLWT( 132 ), CONVERT_CONC( 132 ) / 'APOCJ           ',  150, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 133 ), CGRID_INDEX( 133 ), SPECIES_TYPE( 133 ), SPECIES_MOLWT( 133 ), CONVERT_CONC( 133 ) / 'APNCOMJ         ',  152, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 134 ), CGRID_INDEX( 134 ), SPECIES_TYPE( 134 ), SPECIES_MOLWT( 134 ), CONVERT_CONC( 134 ) / 'FORM_PRIMARY    ',  109, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC( 135 ), CGRID_INDEX( 135 ), SPECIES_TYPE( 135 ), SPECIES_MOLWT( 135 ), CONVERT_CONC( 135 ) / 'ALD2_PRIMARY    ',  110, 'GC',   44.00, F /
      DATA CHEMISTRY_SPC( 136 ), CGRID_INDEX( 136 ), SPECIES_TYPE( 136 ), SPECIES_MOLWT( 136 ), CONVERT_CONC( 136 ) / 'BUTADIENE13     ',  111, 'GC',   54.00, F /
      DATA CHEMISTRY_SPC( 137 ), CGRID_INDEX( 137 ), SPECIES_TYPE( 137 ), SPECIES_MOLWT( 137 ), CONVERT_CONC( 137 ) / 'ACROLEIN        ',  112, 'GC',   56.10, F /
      DATA CHEMISTRY_SPC( 138 ), CGRID_INDEX( 138 ), SPECIES_TYPE( 138 ), SPECIES_MOLWT( 138 ), CONVERT_CONC( 138 ) / 'ACROLEIN_PRIMARY',  113, 'GC',   56.10, F /
      DATA CHEMISTRY_SPC( 139 ), CGRID_INDEX( 139 ), SPECIES_TYPE( 139 ), SPECIES_MOLWT( 139 ), CONVERT_CONC( 139 ) / 'TOLU            ',  114, 'GC',   92.00, F /
      DATA CHEMISTRY_SPC( 140 ), CGRID_INDEX( 140 ), SPECIES_TYPE( 140 ), SPECIES_MOLWT( 140 ), CONVERT_CONC( 140 ) / 'MXYL            ',  115, 'GC',  106.20, F /
      DATA CHEMISTRY_SPC( 141 ), CGRID_INDEX( 141 ), SPECIES_TYPE( 141 ), SPECIES_MOLWT( 141 ), CONVERT_CONC( 141 ) / 'OXYL            ',  116, 'GC',  106.20, F /
      DATA CHEMISTRY_SPC( 142 ), CGRID_INDEX( 142 ), SPECIES_TYPE( 142 ), SPECIES_MOLWT( 142 ), CONVERT_CONC( 142 ) / 'PXYL            ',  117, 'GC',  106.20, F /
      DATA CHEMISTRY_SPC( 143 ), CGRID_INDEX( 143 ), SPECIES_TYPE( 143 ), SPECIES_MOLWT( 143 ), CONVERT_CONC( 143 ) / 'APIN            ',  118, 'GC',  136.30, F /
      DATA CHEMISTRY_SPC( 144 ), CGRID_INDEX( 144 ), SPECIES_TYPE( 144 ), SPECIES_MOLWT( 144 ), CONVERT_CONC( 144 ) / 'BPIN            ',  119, 'GC',  136.30, F /
      DATA CHEMISTRY_SPC( 145 ), CGRID_INDEX( 145 ), SPECIES_TYPE( 145 ), SPECIES_MOLWT( 145 ), CONVERT_CONC( 145 ) / 'HG              ',  120, 'GC',  200.60, F /
      DATA CHEMISTRY_SPC( 146 ), CGRID_INDEX( 146 ), SPECIES_TYPE( 146 ), SPECIES_MOLWT( 146 ), CONVERT_CONC( 146 ) / 'HGIIAER         ',  121, 'GC',  200.60, F /
      DATA CHEMISTRY_SPC( 147 ), CGRID_INDEX( 147 ), SPECIES_TYPE( 147 ), SPECIES_MOLWT( 147 ), CONVERT_CONC( 147 ) / 'HGIIGAS         ',  122, 'GC',  200.60, F /

      INTEGER, PARAMETER :: N_ACT_SP = 147

      INTEGER, PARAMETER :: NRXNS = 336

      INTEGER            :: KUNITS

      DATA  KUNITS /   2 /

      INTEGER IRXXN

      REAL( 8 )          :: RTDAT( 3,NRXNS )

      INTEGER, PARAMETER :: NFALLOFF =  22
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
     &     -1,   -1,   -1,   -1,   -1,   -1,   -1,   12,   -1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    1,   -1,    1,   -1,    1,    1, & ! 8   
     &      3,    0,    0,    3,    3,    3,    3,    0,    1,    3, & ! 9   
     &      3,    1,    1,    1,    1,    3,    0,    1,    1,    1, & ! O   
     &      3,    0,    1,    3,    1,    3,    1,    1,    1,    1, & ! 1   
     &      1,    1,    3,    3,    3,    1,    1,    1,    3,    1, & ! 2   
     &      1,    3,    1,    1,    1,    3/     !  3   

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
     &      1,    1,    1,    1,    1,    1,    1,    0,    1,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    1,    0,    1,    0,    0, & ! 8   
     &      0,    2,    2,    0,    0,    0,    0,    2,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    2,    0,    0,    0, & ! O   
     &      0,    2,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,  260/     !  3   

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
     &      1,    1,    1,    2,    2,    2,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    1,    1,    2,    2,    2,    2,    1,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    1,    2,    2,    2, & ! O   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    3/     !  3   

      INTEGER, PARAMETER :: KTN1 = 114
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     11,   19,   20,   23,   27,   39,   47,   48,   49,   50, & ! O   
     &     59,   60,   61,   77,   81,   84,   86,  104,  110,  116, & ! 1   
     &    118,  119,  121,  128,  136,  137,  138,  139,  140,  141, & ! 2   
     &    142,  148,  150,  151,  152,  155,  156,  157,  160,  162, & ! 3   
     &    169,  170,  171,  173,  177,  178,  184,  188,  191,  194, & ! 4   
     &    196,  197,  198,  199,  201,  202,  203,  206,  207,  208, & ! 5   
     &    218,  219,  220,  228,  234,  237,  240,  243,  246,  247, & ! 6   
     &    250,  270,  271,  272,  273,  274,  275,  276,  277,  278, & ! 7   
     &    279,  280,  281,  282,  283,  284,  285,  287,  289,  290, & ! 8   
     &    299,  302,  303,  304,  305,  308,  309,  310,  313,  315, & ! 9   
     &    317,  318,  319,  320,  321,  322,  326,  327,  328,  330, & ! O   
     &    331,  333,  334,  335/     !  1   

      INTEGER, PARAMETER :: KTN2 =   1
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &      2/

      INTEGER, PARAMETER :: KTN3 = 145
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
     &    241,  242,  244,  245,  248,  249,  251,  252,  291,  294, & ! 2   
     &    295,  296,  297,  300,  301,  306,  311,  314,  316,  323, & ! 3   
     &    324,  325,  329,  332,  336/     !  4   

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
     &      2,   10,  336/
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
     &    106,  106,   65,   67,   17,   16,   99,  100,  101,    9, & ! 5   
     &      9,  107,  108,  107,  107,  108,    1,    4,   22,  113, & ! 6   
     &    115,  116,  117,  118,  119,  120,  122,  123,  124,  125, & ! 7   
     &    126,  127,  128,  129,  130,  131,  132,  133,  134,  134, & ! 8   
     &    134,  134,  134,  134,  135,  135,  135,  135,  135,  136, & ! 9   
     &    136,  136,  136,  138,  138,  138,  138,  138,  137,  137, & ! O   
     &    137,  137,  137,  139,  139,  140,  140,  141,  141,  142, & ! 1   
     &    142,  143,  143,  143,  143,  143,  144,  144,  144,  144, & ! 2   
     &    144,  145,  145,  145,  145,  145/     !  3   

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
     &      0,    0,    0,  109,  110,  111,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    7,    7,    7,    7,    7,    5, & ! 8   
     &      3,    0,    0,   77,    7,    5,    3,    0,   77,    7, & ! 9   
     &      4,    5,   77,    7,    4,    5,    0,   77,    7,    4, & ! O   
     &      5,    0,   77,    7,   77,    7,   77,    7,   77,    7, & ! 1   
     &     77,    3,    7,    4,    5,   77,    3,    7,    4,    5, & ! 2   
     &     77,    4,   76,   13,    7,   77/     !  3   

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
     &      0,    0,    0,    0,    0,    0/     !  3   

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
     &     10,   10,   10,   82,   82,   82,   11,    0,  112,  114, & ! 6   
     &    114,  114,  114,  114,  114,  121,  121,  121,  121,  121, & ! 7   
     &    114,  114,  114,  114,  131,    7,  133,    7,    7,    5, & ! 8   
     &      3,    0,    0,   77,    7,    5,    3,    0,   77,    7, & ! 9   
     &      4,    5,   77,    7,    4,    5,    0,   77,    7,    4, & ! O   
     &      5,    0,   77,    7,   77,    7,   77,    7,   77,    7, & ! 1   
     &     77,    3,    7,    4,    5,   77,    3,    7,    4,    5, & ! 2   
     &     77,  146,  147,  147,  146,  145/     !  3   

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
     &     99,    0,    0,   99,    0,    7,    0,   67,    0,    0, & ! 4   
     &     67,    0,    8,    8,    8,    8,    8,    8,    8,  107, & ! 5   
     &    108,    0,    0,    0,    0,    0,   10,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,  130,    0,  132,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  137, & ! 9   
     &    137,  137,  137,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,  147,   76,   13,  147,  147/     !  3   

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
     &    100,    0,    0,  100,    0,    0,    0,    8,    0,    0, & ! 4   
     &      0,    0,   19,   27,   27,   27,   27,   27,   18,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    7,    0,    7,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    4,    0,    0,    7,   77/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0/     !  3   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0/     !  3   

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
     &     1.0000D+00, 1.0000D+00, 1.0000D-40, 1.0000D+00, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 7   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 2.5000D-12, & ! 8   
     &     1.0000D+00, 2.5000D-12, 1.0000D+00, 9.0000D-12, 5.8000D-16, & ! +   
     &     3.4000D-11, 1.0000D+00, 1.0000D+00, 8.2000D-11, 5.6000D-12, & ! 9   
     &     1.4000D-12, 1.8000D-11, 1.0000D+00, 7.9000D-11, 1.4000D-11, & ! +   
     &     8.2000D-15, 1.7900D-13, 2.5100D-10, 2.0000D-11, 2.6100D-19, & ! O   
     &     1.7000D-11, 1.0000D+00, 2.3700D-10, 2.0000D-11, 2.6100D-19, & ! +   
     &     1.7000D-11, 1.0000D+00, 2.3700D-10, 1.8000D-12, 6.1000D-11, & ! 1   
     &     1.7000D-11, 1.4000D-10, 1.2200D-11, 1.5000D-10, 1.3000D-11, & ! +   
     &     1.5000D-10, 2.7900D-11, 1.2000D-11, 6.3000D-16, 1.2000D-12, & ! 2   
     &     4.7000D-10, 2.8100D-11, 7.5100D-11, 1.7400D-15, 2.8100D-11, & ! +   
     &     5.3000D-10, 2.1100D-18, 2.6000D-18, 8.5000D-19, 7.7000D-14, & ! 3   
     &     2.2500D-33/           !        +   

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
     &     0.0000D+00, 0.0000D+00, 4.0582D-09, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00/           !        +   

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
     &    -1.6000D+03, 0.0000D+00, 0.0000D+00,-3.4000D+01, 2.7000D+02, & ! 9   
     &    -1.9000D+03,-1.1000D+03, 0.0000D+00, 0.0000D+00, 4.2400D+02, & ! +   
     &    -2.0700D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &    -3.1310D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -3.1310D+03, 0.0000D+00, 0.0000D+00, 3.5500D+02, 0.0000D+00, & ! 1   
     &     1.1600D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.4000D+02,-5.8000D+02, 4.9000D+02, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.2600D+03, 0.0000D+00, & ! +   
     &     0.0000D+00,-1.2565D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &    -6.8000D+02/           !        +   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &      5,    6,   18,   21,   24,   28,   29,   31,   32,   33, & 
     &     35,   36,   43,   69,   92,   93,  107,  108,  125,  181, & 
     &    209,  268/

      DATA ( RFDAT( 1,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     2.2000D-11, 3.0000D-11, 1.4000D-12, 9.7000D+14, 3.6000D-11, & 
     &     3.0000D-11, 2.1990D+03, 2.7000D+02, 4.7000D-12, 4.8000D+15, & 
     &     1.0000D+03, 3.2000D+03, 2.6000D-11, 0.0000D+00, 1.2000D-11, & 
     &     5.4000D+16, 1.2000D-11, 8.3000D+16, 8.8000D-12, 1.6000D-12, & 
     &     1.0000D-10, 7.8426D+01/

      DATA ( RFDAT( 2,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &    -7.0000D-01, 0.0000D+00,-7.0000D-01, 1.0000D-01,-1.0000D-01, & 
     &     0.0000D+00, 6.5000D-34,-1.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-9.0000D-01, & 
     &     0.0000D+00,-9.0000D-01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &    -1.0000D+00, 5.8212D+00/

      DATA ( RFDAT( 3,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.1080D+04, 0.0000D+00, & 
     &     0.0000D+00, 1.3350D+03, 1.0000D+00, 0.0000D+00,-1.1170D+04, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &    -1.3830D+04, 0.0000D+00,-1.3940D+04, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00/

      DATA ( RFDAT( 4,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     6.0000D-01, 6.0000D-01, 6.0000D-01, 4.5000D-01, 6.0000D-01, & 
     &     4.1000D-01, 0.0000D+00,-5.9680D-14, 6.0000D-01, 6.0000D-01, & 
     &     0.0000D+00, 0.0000D+00, 6.0000D-01, 0.0000D+00, 3.0000D-01, & 
     &     3.0000D-01, 3.0000D-01, 3.6000D-01, 6.0000D-01, 6.0000D-01, & 
     &     6.0000D-01, 0.0000D+00/

      DATA ( RFDAT( 5,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.2400D+00, 0.0000D+00, 2.7000D+02, 1.0000D+00, 1.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 0.0000D+00, 1.4100D+00, & 
     &     1.4100D+00, 1.4100D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.0000D+00, 0.0000D+00/

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
     &        1.00000,    1.00000,    1.65000,    1.00000,    1.25000, & ! 3   
     &        1.00000,    1.00000,    1.17000,    1.00000,    1.00000, & ! +   
     &        1.50000,    1.00000,    1.00000,    1.25000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    0.87000,    1.00000,    1.00000, & ! +   
     &        1.65000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    0.50000,    0.00000,    1.00000,    1.14280, & ! +   
     &        1.14280,    1.00000,    1.00000,    0.85714,    0.85714, & ! 7   
     &        1.00000,    1.00000,    0.50000,    0.50000,    1.50000, & ! +   
     &        1.42860,    1.42860,    1.71430,    1.71430,    1.25000, & ! 8   
     &        1.00000,    1.25000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.50000,    1.00000,    1.00000,    0.50000, & ! 3   
     &        0.50000/           !         &  

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
     &        0.30000,    1.00000,    1.00000,    0.00000,    0.12000, & ! 3   
     &        0.00000,    0.00000,    0.60000,    0.00000,    0.00000, & ! +   
     &        0.24000,    0.00000,    0.00000,    0.54000,    0.00000, & ! 4   
     &        1.00000,    0.00000,    1.15000,    0.00000,    0.00000, & ! +   
     &        0.35000,    0.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.50000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 8   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.58000, & ! +   
     &        0.52000,    0.04500,    0.58000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.50000,    1.00000,    1.00000,    0.50000, & ! 3   
     &        0.50000/           !         &  

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
     &        0.90000,    1.00000,    0.35000,    0.00000,    0.63000, & ! 3   
     &        0.00000,    0.00000,    0.22000,    0.00000,    0.00000, & ! +   
     &        0.26000,    0.00000,    0.00000,    0.20000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.68000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.33000,    0.33000, & ! 5   
     &        0.33000,    0.33000,    0.33000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! 8   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! 3   
     &        1.00000/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        1.00000/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000/           !         &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000/           !         &  

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
     &      1,    1,    1,    2,    2,    2,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    1,    1,    2,    2,    2,    2,    1,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    1,    2,    2,    2, & ! O   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2/     !  3   
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
     &      2,    1,    4,    6,    6,    6,    6,    6,    3,    2, & ! 5   
     &      2,    1,    1,    1,    1,    1,    2,    0,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &      1,    1,    1,    1,    3,    1,    3,    1,    1,    1, & ! 8   
     &      1,    0,    0,    1,    1,    1,    1,    0,    1,    2, & ! 9   
     &      2,    2,    2,    1,    1,    1,    0,    1,    1,    1, & ! O   
     &      1,    0,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 2   
     &      1,    3,    2,    2,    3,    3/     !  3   

      INTEGER, PARAMETER :: NMPHOT =  40
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    8,    9,   14,   15,   25,   37,   52,   53,   54, & 
     &     63,   75,   78,   79,   90,   94,  100,  105,  109,  145, & 
     &    147,  161,  172,  185,  186,  192,  210,  227,  253,  254, & 
     &    255,  256,  257,  258,  259,  292,  293,  298,  307,  312/

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   11,   12,   13,   14,   15,   16,   17,   15,    1, & 
     &      1,   18,   19,   20,   21,   22,   23,   15,   24,   25, & 
     &     24,   24,   26,   26,   24,   12,   13,   14,   19,   19/

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & 
     &     31,   32,   33,   34,   35,   36,   37,   38,   39,   40/

      INTEGER, PARAMETER :: MHETERO =  11
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    260,  261,  262,  263,  264,  265,  266,  267,  269,  286, & 
     &    288/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    2,    3,    4,    5,    5,    6,    7,    8,    9, & 
     &     10/

      INTEGER, PARAMETER :: NPHOTAB =  26
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
     &   'NOA_14          ', 'NBO_14          '/

      INTEGER, PARAMETER :: NHETERO =  10
      CHARACTER( 16 )    :: HETERO( NHETERO )

      DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & 
     &   'HETERO_N2O5IJ   ', 'HETERO_N2O5K    ', 'HETERO_H2NO3PAIJ', &
     &   'HETERO_H2NO3PAK ', 'HETERO_H2NO3PBIJ', 'HETERO_H2NO3PBK ', &
     &   'HETERO_NO2      ', 'HETERO_IEPOX    ', 'HETERO_PNCOMLI  ', &
     &   'HETERO_PNCOMLJ  '/

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
     &    'N25             ', 'HET_N2O5IJ      ', 'HET_N2O5K       ', & ! 6   
     &    'HET_H2NO3PIJA   ', 'HET_H2NO3PKA    ', 'HET_H2NO3PIB    ', & ! 7   
     &    'HET_H2NO3PJB    ', 'HET_H2NO3PKB    ', 'HET_N02         ', & ! 8   
     &    'HAL_Ozone       ', 'HET_IEPOX       ', 'OLIG_XYLENE1    ', & ! 9   
     &    'OLIG_XYLENE2    ', 'OLIG_TOLUENE1   ', 'OLIG_TOLUENE2   ', & ! 0   
     &    'OLIG_BENZENE1   ', 'OLIG_BENZENE2   ', 'OLIG_TERPENE1   ', & ! 1   
     &    'OLIG_TERPENE2   ', 'OLIG_ISOPRENE1  ', 'OLIG_ISOPRENE2  ', & ! 2   
     &    'OLIG_SESQT1     ', 'OLIG_PAH1       ', 'OLIG_PAH2       ', & ! 3   
     &    'OLIG_ALK1       ', 'OLIG_ALK2       ', 'RPOAGEPI        ', & ! 4   
     &    'RPOAGELI        ', 'RPOAGEPJ        ', 'RPOAGELJ        ', & ! 5   
     &    'T01             ', 'T02             ', 'T03             ', & ! 6   
     &    'T04             ', 'T05             ', 'TCL1            ', & ! 7   
     &    'T06             ', 'T07             ', 'T08             ', & ! 8   
     &    'T09             ', 'TCL2            ', 'T10             ', & ! 9   
     &    'T11             ', 'T12             ', 'TCL3            ', & ! 0   
     &    'T14             ', 'T15             ', 'T16             ', & ! 1   
     &    'T17             ', 'TCL4            ', 'T18             ', & ! 2   
     &    'T19             ', 'T20             ', 'T21             ', & ! 3   
     &    'TCL5            ', 'T22             ', 'TCL6            ', & ! 4   
     &    'T23             ', 'TCL7            ', 'T24             ', & ! 5   
     &    'TCL8            ', 'T25             ', 'TCL9            ', & ! 6   
     &    'T26             ', 'T27             ', 'T28             ', & ! 7   
     &    'T29             ', 'TCL10           ', 'T30             ', & ! 8   
     &    'T31             ', 'T32             ', 'T33             ', & ! 9   
     &    'TCL11           ', 'HG1             ', 'HG2             ', & ! 0   
     &    'HG3             ', 'HG4             ', 'HG5             '/!    

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
       INTEGER, PARAMETER  :: IK_HETERO_N2O5IJ    =   1
       INTEGER, PARAMETER  :: IK_HETERO_N2O5K     =   2
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAIJ =   3
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PAK  =   4
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBIJ =   5
       INTEGER, PARAMETER  :: IK_HETERO_H2NO3PBK  =   6
       INTEGER, PARAMETER  :: IK_HETERO_NO2       =   7
       INTEGER, PARAMETER  :: IK_HETERO_IEPOX     =   8
       INTEGER, PARAMETER  :: IK_HETERO_PNCOMLI   =   9
       INTEGER, PARAMETER  :: IK_HETERO_PNCOMLJ   =  10
       END MODULE RXNS_DATA
