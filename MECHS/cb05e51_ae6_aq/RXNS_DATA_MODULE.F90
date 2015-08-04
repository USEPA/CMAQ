       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: /home/ixk/cctm/BLD_CB05_t4/mech_CB05e51.def
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

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC = 116
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
      DATA GAS_CHEM_SPC(  21 ) / 'CO              ' /
      DATA GAS_CHEM_SPC(  22 ) / 'MEO2            ' /
      DATA GAS_CHEM_SPC(  23 ) / 'FORM            ' /
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
      DATA GAS_CHEM_SPC(  34 ) / 'PAR             ' /
      DATA GAS_CHEM_SPC(  35 ) / 'ROR             ' /
      DATA GAS_CHEM_SPC(  36 ) / 'OLE             ' /
      DATA GAS_CHEM_SPC(  37 ) / 'ETH             ' /
      DATA GAS_CHEM_SPC(  38 ) / 'IOLE            ' /
      DATA GAS_CHEM_SPC(  39 ) / 'TOL             ' /
      DATA GAS_CHEM_SPC(  40 ) / 'CRES            ' /
      DATA GAS_CHEM_SPC(  41 ) / 'TO2             ' /
      DATA GAS_CHEM_SPC(  42 ) / 'TOLRO2          ' /
      DATA GAS_CHEM_SPC(  43 ) / 'OPEN            ' /
      DATA GAS_CHEM_SPC(  44 ) / 'MGLY            ' /
      DATA GAS_CHEM_SPC(  45 ) / 'CRO             ' /
      DATA GAS_CHEM_SPC(  46 ) / 'CAT1            ' /
      DATA GAS_CHEM_SPC(  47 ) / 'CRON            ' /
      DATA GAS_CHEM_SPC(  48 ) / 'CRNO            ' /
      DATA GAS_CHEM_SPC(  49 ) / 'CRN2            ' /
      DATA GAS_CHEM_SPC(  50 ) / 'CRPX            ' /
      DATA GAS_CHEM_SPC(  51 ) / 'OPO3            ' /
      DATA GAS_CHEM_SPC(  52 ) / 'CAO2            ' /
      DATA GAS_CHEM_SPC(  53 ) / 'OPAN            ' /
      DATA GAS_CHEM_SPC(  54 ) / 'XYLMN           ' /
      DATA GAS_CHEM_SPC(  55 ) / 'XYLRO2          ' /
      DATA GAS_CHEM_SPC(  56 ) / 'NAPH            ' /
      DATA GAS_CHEM_SPC(  57 ) / 'PAHRO2          ' /
      DATA GAS_CHEM_SPC(  58 ) / 'ISOP            ' /
      DATA GAS_CHEM_SPC(  59 ) / 'ISPD            ' /
      DATA GAS_CHEM_SPC(  60 ) / 'IXO2            ' /
      DATA GAS_CHEM_SPC(  61 ) / 'IXO2N           ' /
      DATA GAS_CHEM_SPC(  62 ) / 'ISOPRXN         ' /
      DATA GAS_CHEM_SPC(  63 ) / 'NTRI1           ' /
      DATA GAS_CHEM_SPC(  64 ) / 'ISOPX           ' /
      DATA GAS_CHEM_SPC(  65 ) / 'IEPOX           ' /
      DATA GAS_CHEM_SPC(  66 ) / 'NTRI2           ' /
      DATA GAS_CHEM_SPC(  67 ) / 'TERP            ' /
      DATA GAS_CHEM_SPC(  68 ) / 'TRPRXN          ' /
      DATA GAS_CHEM_SPC(  69 ) / 'XO2T            ' /
      DATA GAS_CHEM_SPC(  70 ) / 'SO2             ' /
      DATA GAS_CHEM_SPC(  71 ) / 'SULF            ' /
      DATA GAS_CHEM_SPC(  72 ) / 'SULRXN          ' /
      DATA GAS_CHEM_SPC(  73 ) / 'ETOH            ' /
      DATA GAS_CHEM_SPC(  74 ) / 'ETHA            ' /
      DATA GAS_CHEM_SPC(  75 ) / 'CL2             ' /
      DATA GAS_CHEM_SPC(  76 ) / 'CL              ' /
      DATA GAS_CHEM_SPC(  77 ) / 'HOCL            ' /
      DATA GAS_CHEM_SPC(  78 ) / 'CLO             ' /
      DATA GAS_CHEM_SPC(  79 ) / 'FMCL            ' /
      DATA GAS_CHEM_SPC(  80 ) / 'HCL             ' /
      DATA GAS_CHEM_SPC(  81 ) / 'CLNO2           ' /
      DATA GAS_CHEM_SPC(  82 ) / 'TOLNRXN         ' /
      DATA GAS_CHEM_SPC(  83 ) / 'TOLHRXN         ' /
      DATA GAS_CHEM_SPC(  84 ) / 'XYLNRXN         ' /
      DATA GAS_CHEM_SPC(  85 ) / 'XYLHRXN         ' /
      DATA GAS_CHEM_SPC(  86 ) / 'BENZENE         ' /
      DATA GAS_CHEM_SPC(  87 ) / 'BENZRO2         ' /
      DATA GAS_CHEM_SPC(  88 ) / 'BNZNRXN         ' /
      DATA GAS_CHEM_SPC(  89 ) / 'BNZHRXN         ' /
      DATA GAS_CHEM_SPC(  90 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC(  91 ) / 'SESQRXN         ' /
      DATA GAS_CHEM_SPC(  92 ) / 'PAHNRXN         ' /
      DATA GAS_CHEM_SPC(  93 ) / 'PAHHRXN         ' /
      DATA GAS_CHEM_SPC(  94 ) / 'SOAALK          ' /
      DATA GAS_CHEM_SPC(  95 ) / 'ALKRXN          ' /
      DATA GAS_CHEM_SPC(  96 ) / 'NTRC1           ' /
      DATA GAS_CHEM_SPC(  97 ) / 'NTRC2           ' /
      DATA GAS_CHEM_SPC(  98 ) / 'NXO2N           ' /
      DATA GAS_CHEM_SPC(  99 ) / 'NTRI1O2         ' /
      DATA GAS_CHEM_SPC( 100 ) / 'NTRI2O2         ' /
      DATA GAS_CHEM_SPC( 101 ) / 'H2NO3PIJ        ' /
      DATA GAS_CHEM_SPC( 102 ) / 'H2NO3PK         ' /
      DATA GAS_CHEM_SPC( 103 ) / 'FORM_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 104 ) / 'ALD2_PRIMARY    ' /
      DATA GAS_CHEM_SPC( 105 ) / 'BUTADIENE13     ' /
      DATA GAS_CHEM_SPC( 106 ) / 'ACROLEIN        ' /
      DATA GAS_CHEM_SPC( 107 ) / 'ACROLEIN_PRIMARY' /
      DATA GAS_CHEM_SPC( 108 ) / 'TOLU            ' /
      DATA GAS_CHEM_SPC( 109 ) / 'MXYL            ' /
      DATA GAS_CHEM_SPC( 110 ) / 'OXYL            ' /
      DATA GAS_CHEM_SPC( 111 ) / 'PXYL            ' /
      DATA GAS_CHEM_SPC( 112 ) / 'APIN            ' /
      DATA GAS_CHEM_SPC( 113 ) / 'BPIN            ' /
      DATA GAS_CHEM_SPC( 114 ) / 'HG              ' /
      DATA GAS_CHEM_SPC( 115 ) / 'HGIIAER         ' /
      DATA GAS_CHEM_SPC( 116 ) / 'HGIIGAS         ' /




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
      DATA CHEMISTRY_SPC(  21 ), CGRID_INDEX(  21 ), SPECIES_TYPE(  21 ), SPECIES_MOLWT(  21 ), CONVERT_CONC(  21 ) / 'CO              ',   27, 'GC',   28.00, F /
      DATA CHEMISTRY_SPC(  22 ), CGRID_INDEX(  22 ), SPECIES_TYPE(  22 ), SPECIES_MOLWT(  22 ), CONVERT_CONC(  22 ) / 'MEO2            ',   30, 'GC',   47.00, F /
      DATA CHEMISTRY_SPC(  23 ), CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), SPECIES_MOLWT(  23 ), CONVERT_CONC(  23 ) / 'FORM            ',   25, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC(  24 ), CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), SPECIES_MOLWT(  24 ), CONVERT_CONC(  24 ) / 'MEPX            ',   32, 'GC',   48.00, F /
      DATA CHEMISTRY_SPC(  25 ), CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), SPECIES_MOLWT(  25 ), CONVERT_CONC(  25 ) / 'MEOH            ',   33, 'GC',   32.00, F /
      DATA CHEMISTRY_SPC(  26 ), CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), SPECIES_MOLWT(  26 ), CONVERT_CONC(  26 ) / 'HCO3            ',   34, 'GC',   63.00, F /
      DATA CHEMISTRY_SPC(  27 ), CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), SPECIES_MOLWT(  27 ), CONVERT_CONC(  27 ) / 'FACD            ',   26, 'GC',   46.00, F /
      DATA CHEMISTRY_SPC(  28 ), CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), SPECIES_MOLWT(  28 ), CONVERT_CONC(  28 ) / 'C2O3            ',   29, 'GC',   75.00, F /
      DATA CHEMISTRY_SPC(  29 ), CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), SPECIES_MOLWT(  29 ), CONVERT_CONC(  29 ) / 'PAN             ',   35, 'GC',  121.00, F /
      DATA CHEMISTRY_SPC(  30 ), CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), SPECIES_MOLWT(  30 ), CONVERT_CONC(  30 ) / 'PACD            ',   36, 'GC',   76.00, F /
      DATA CHEMISTRY_SPC(  31 ), CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), SPECIES_MOLWT(  31 ), CONVERT_CONC(  31 ) / 'AACD            ',   31, 'GC',   60.00, F /
      DATA CHEMISTRY_SPC(  32 ), CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), SPECIES_MOLWT(  32 ), CONVERT_CONC(  32 ) / 'CXO3            ',   37, 'GC',   75.00, F /
      DATA CHEMISTRY_SPC(  33 ), CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), SPECIES_MOLWT(  33 ), CONVERT_CONC(  33 ) / 'PANX            ',   38, 'GC',  121.00, F /
      DATA CHEMISTRY_SPC(  34 ), CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), SPECIES_MOLWT(  34 ), CONVERT_CONC(  34 ) / 'PAR             ',   28, 'GC',   14.00, F /
      DATA CHEMISTRY_SPC(  35 ), CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), SPECIES_MOLWT(  35 ), CONVERT_CONC(  35 ) / 'ROR             ',   39, 'GC',   31.00, F /
      DATA CHEMISTRY_SPC(  36 ), CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), SPECIES_MOLWT(  36 ), CONVERT_CONC(  36 ) / 'OLE             ',   40, 'GC',   27.00, F /
      DATA CHEMISTRY_SPC(  37 ), CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), SPECIES_MOLWT(  37 ), CONVERT_CONC(  37 ) / 'ETH             ',   41, 'GC',   28.00, F /
      DATA CHEMISTRY_SPC(  38 ), CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), SPECIES_MOLWT(  38 ), CONVERT_CONC(  38 ) / 'IOLE            ',   23, 'GC',   48.00, F /
      DATA CHEMISTRY_SPC(  39 ), CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), SPECIES_MOLWT(  39 ), CONVERT_CONC(  39 ) / 'TOL             ',   42, 'GC',   92.00, F /
      DATA CHEMISTRY_SPC(  40 ), CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), SPECIES_MOLWT(  40 ), CONVERT_CONC(  40 ) / 'CRES            ',   43, 'GC',  108.00, F /
      DATA CHEMISTRY_SPC(  41 ), CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), SPECIES_MOLWT(  41 ), CONVERT_CONC(  41 ) / 'TO2             ',   44, 'GC',  173.00, F /
      DATA CHEMISTRY_SPC(  42 ), CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), SPECIES_MOLWT(  42 ), CONVERT_CONC(  42 ) / 'TOLRO2          ',   45, 'GC',  141.00, F /
      DATA CHEMISTRY_SPC(  43 ), CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), SPECIES_MOLWT(  43 ), CONVERT_CONC(  43 ) / 'OPEN            ',   46, 'GC',   84.00, F /
      DATA CHEMISTRY_SPC(  44 ), CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), SPECIES_MOLWT(  44 ), CONVERT_CONC(  44 ) / 'MGLY            ',   24, 'GC',   72.00, F /
      DATA CHEMISTRY_SPC(  45 ), CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), SPECIES_MOLWT(  45 ), CONVERT_CONC(  45 ) / 'CRO             ',   47, 'GC',  107.00, F /
      DATA CHEMISTRY_SPC(  46 ), CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), SPECIES_MOLWT(  46 ), CONVERT_CONC(  46 ) / 'CAT1            ',   48, 'GC',  124.00, F /
      DATA CHEMISTRY_SPC(  47 ), CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), SPECIES_MOLWT(  47 ), CONVERT_CONC(  47 ) / 'CRON            ',   49, 'GC',  153.00, F /
      DATA CHEMISTRY_SPC(  48 ), CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), SPECIES_MOLWT(  48 ), CONVERT_CONC(  48 ) / 'CRNO            ',   50, 'GC',  152.00, F /
      DATA CHEMISTRY_SPC(  49 ), CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), SPECIES_MOLWT(  49 ), CONVERT_CONC(  49 ) / 'CRN2            ',   51, 'GC',  168.00, F /
      DATA CHEMISTRY_SPC(  50 ), CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), SPECIES_MOLWT(  50 ), CONVERT_CONC(  50 ) / 'CRPX            ',   52, 'GC',  169.00, F /
      DATA CHEMISTRY_SPC(  51 ), CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), SPECIES_MOLWT(  51 ), CONVERT_CONC(  51 ) / 'OPO3            ',   53, 'GC',  115.00, F /
      DATA CHEMISTRY_SPC(  52 ), CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), SPECIES_MOLWT(  52 ), CONVERT_CONC(  52 ) / 'CAO2            ',   54, 'GC',  133.00, F /
      DATA CHEMISTRY_SPC(  53 ), CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), SPECIES_MOLWT(  53 ), CONVERT_CONC(  53 ) / 'OPAN            ',   55, 'GC',  161.00, F /
      DATA CHEMISTRY_SPC(  54 ), CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), SPECIES_MOLWT(  54 ), CONVERT_CONC(  54 ) / 'XYLMN           ',   56, 'GC',  106.00, F /
      DATA CHEMISTRY_SPC(  55 ), CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), SPECIES_MOLWT(  55 ), CONVERT_CONC(  55 ) / 'XYLRO2          ',   57, 'GC',  155.00, F /
      DATA CHEMISTRY_SPC(  56 ), CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), SPECIES_MOLWT(  56 ), CONVERT_CONC(  56 ) / 'NAPH            ',   58, 'GC',  128.20, F /
      DATA CHEMISTRY_SPC(  57 ), CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), SPECIES_MOLWT(  57 ), CONVERT_CONC(  57 ) / 'PAHRO2          ',   59, 'GC',  187.20, F /
      DATA CHEMISTRY_SPC(  58 ), CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), SPECIES_MOLWT(  58 ), CONVERT_CONC(  58 ) / 'ISOP            ',   60, 'GC',   68.00, F /
      DATA CHEMISTRY_SPC(  59 ), CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), SPECIES_MOLWT(  59 ), CONVERT_CONC(  59 ) / 'ISPD            ',   63, 'GC',   70.00, F /
      DATA CHEMISTRY_SPC(  60 ), CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), SPECIES_MOLWT(  60 ), CONVERT_CONC(  60 ) / 'IXO2            ',   61, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  61 ), CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), SPECIES_MOLWT(  61 ), CONVERT_CONC(  61 ) / 'IXO2N           ',   62, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  62 ), CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), SPECIES_MOLWT(  62 ), CONVERT_CONC(  62 ) / 'ISOPRXN         ',   64, 'GC',   68.00, F /
      DATA CHEMISTRY_SPC(  63 ), CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), SPECIES_MOLWT(  63 ), CONVERT_CONC(  63 ) / 'NTRI1           ',   65, 'GC',  147.00, F /
      DATA CHEMISTRY_SPC(  64 ), CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), SPECIES_MOLWT(  64 ), CONVERT_CONC(  64 ) / 'ISOPX           ',   21, 'GC',  118.10, F /
      DATA CHEMISTRY_SPC(  65 ), CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), SPECIES_MOLWT(  65 ), CONVERT_CONC(  65 ) / 'IEPOX           ',   22, 'GC',  118.10, F /
      DATA CHEMISTRY_SPC(  66 ), CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), SPECIES_MOLWT(  66 ), CONVERT_CONC(  66 ) / 'NTRI2           ',   66, 'GC',  149.10, F /
      DATA CHEMISTRY_SPC(  67 ), CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), SPECIES_MOLWT(  67 ), CONVERT_CONC(  67 ) / 'TERP            ',   67, 'GC',  136.00, F /
      DATA CHEMISTRY_SPC(  68 ), CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), SPECIES_MOLWT(  68 ), CONVERT_CONC(  68 ) / 'TRPRXN          ',   68, 'GC',  136.00, F /
      DATA CHEMISTRY_SPC(  69 ), CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), SPECIES_MOLWT(  69 ), CONVERT_CONC(  69 ) / 'XO2T            ',   69, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  70 ), CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), SPECIES_MOLWT(  70 ), CONVERT_CONC(  70 ) / 'SO2             ',   70, 'GC',   64.00, F /
      DATA CHEMISTRY_SPC(  71 ), CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), SPECIES_MOLWT(  71 ), CONVERT_CONC(  71 ) / 'SULF            ',   71, 'GC',   98.00, F /
      DATA CHEMISTRY_SPC(  72 ), CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), SPECIES_MOLWT(  72 ), CONVERT_CONC(  72 ) / 'SULRXN          ',   72, 'GC',   98.00, F /
      DATA CHEMISTRY_SPC(  73 ), CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), SPECIES_MOLWT(  73 ), CONVERT_CONC(  73 ) / 'ETOH            ',   73, 'GC',   46.00, F /
      DATA CHEMISTRY_SPC(  74 ), CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), SPECIES_MOLWT(  74 ), CONVERT_CONC(  74 ) / 'ETHA            ',   74, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC(  75 ), CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), SPECIES_MOLWT(  75 ), CONVERT_CONC(  75 ) / 'CL2             ',   75, 'GC',   71.00, F /
      DATA CHEMISTRY_SPC(  76 ), CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), SPECIES_MOLWT(  76 ), CONVERT_CONC(  76 ) / 'CL              ',   76, 'GC',   35.50, F /
      DATA CHEMISTRY_SPC(  77 ), CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), SPECIES_MOLWT(  77 ), CONVERT_CONC(  77 ) / 'HOCL            ',   77, 'GC',   52.50, F /
      DATA CHEMISTRY_SPC(  78 ), CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), SPECIES_MOLWT(  78 ), CONVERT_CONC(  78 ) / 'CLO             ',   78, 'GC',   51.50, F /
      DATA CHEMISTRY_SPC(  79 ), CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), SPECIES_MOLWT(  79 ), CONVERT_CONC(  79 ) / 'FMCL            ',   79, 'GC',   64.50, F /
      DATA CHEMISTRY_SPC(  80 ), CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), SPECIES_MOLWT(  80 ), CONVERT_CONC(  80 ) / 'HCL             ',   80, 'GC',   36.50, F /
      DATA CHEMISTRY_SPC(  81 ), CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), SPECIES_MOLWT(  81 ), CONVERT_CONC(  81 ) / 'CLNO2           ',   81, 'GC',   81.50, F /
      DATA CHEMISTRY_SPC(  82 ), CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), SPECIES_MOLWT(  82 ), CONVERT_CONC(  82 ) / 'TOLNRXN         ',   82, 'GC',  141.00, F /
      DATA CHEMISTRY_SPC(  83 ), CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), SPECIES_MOLWT(  83 ), CONVERT_CONC(  83 ) / 'TOLHRXN         ',   83, 'GC',  141.00, F /
      DATA CHEMISTRY_SPC(  84 ), CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), SPECIES_MOLWT(  84 ), CONVERT_CONC(  84 ) / 'XYLNRXN         ',   84, 'GC',  155.00, F /
      DATA CHEMISTRY_SPC(  85 ), CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), SPECIES_MOLWT(  85 ), CONVERT_CONC(  85 ) / 'XYLHRXN         ',   85, 'GC',  155.00, F /
      DATA CHEMISTRY_SPC(  86 ), CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), SPECIES_MOLWT(  86 ), CONVERT_CONC(  86 ) / 'BENZENE         ',   86, 'GC',   78.00, F /
      DATA CHEMISTRY_SPC(  87 ), CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), SPECIES_MOLWT(  87 ), CONVERT_CONC(  87 ) / 'BENZRO2         ',   87, 'GC',  127.00, F /
      DATA CHEMISTRY_SPC(  88 ), CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), SPECIES_MOLWT(  88 ), CONVERT_CONC(  88 ) / 'BNZNRXN         ',   88, 'GC',  127.00, F /
      DATA CHEMISTRY_SPC(  89 ), CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), SPECIES_MOLWT(  89 ), CONVERT_CONC(  89 ) / 'BNZHRXN         ',   89, 'GC',  127.00, F /
      DATA CHEMISTRY_SPC(  90 ), CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), SPECIES_MOLWT(  90 ), CONVERT_CONC(  90 ) / 'SESQ            ',   90, 'GC',  204.00, F /
      DATA CHEMISTRY_SPC(  91 ), CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), SPECIES_MOLWT(  91 ), CONVERT_CONC(  91 ) / 'SESQRXN         ',   91, 'GC',  204.00, F /
      DATA CHEMISTRY_SPC(  92 ), CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), SPECIES_MOLWT(  92 ), CONVERT_CONC(  92 ) / 'PAHNRXN         ',   92, 'GC',  187.20, F /
      DATA CHEMISTRY_SPC(  93 ), CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), SPECIES_MOLWT(  93 ), CONVERT_CONC(  93 ) / 'PAHHRXN         ',   93, 'GC',  187.20, F /
      DATA CHEMISTRY_SPC(  94 ), CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), SPECIES_MOLWT(  94 ), CONVERT_CONC(  94 ) / 'SOAALK          ',   94, 'GC',  112.00, F /
      DATA CHEMISTRY_SPC(  95 ), CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), SPECIES_MOLWT(  95 ), CONVERT_CONC(  95 ) / 'ALKRXN          ',   95, 'GC',  112.00, F /
      DATA CHEMISTRY_SPC(  96 ), CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), SPECIES_MOLWT(  96 ), CONVERT_CONC(  96 ) / 'NTRC1           ',   97, 'GC',  147.00, F /
      DATA CHEMISTRY_SPC(  97 ), CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), SPECIES_MOLWT(  97 ), CONVERT_CONC(  97 ) / 'NTRC2           ',   98, 'GC',  149.00, F /
      DATA CHEMISTRY_SPC(  98 ), CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), SPECIES_MOLWT(  98 ), CONVERT_CONC(  98 ) / 'NXO2N           ',   96, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  99 ), CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), SPECIES_MOLWT(  99 ), CONVERT_CONC(  99 ) / 'NTRI1O2         ',   99, 'GC',  106.00, F /
      DATA CHEMISTRY_SPC( 100 ), CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), SPECIES_MOLWT( 100 ), CONVERT_CONC( 100 ) / 'NTRI2O2         ',  100, 'GC',  106.00, F /
      DATA CHEMISTRY_SPC( 101 ), CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), SPECIES_MOLWT( 101 ), CONVERT_CONC( 101 ) / 'H2NO3PIJ        ',  101, 'GC',   64.00, F /
      DATA CHEMISTRY_SPC( 102 ), CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), SPECIES_MOLWT( 102 ), CONVERT_CONC( 102 ) / 'H2NO3PK         ',  102, 'GC',   64.00, F /
      DATA CHEMISTRY_SPC( 103 ), CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), SPECIES_MOLWT( 103 ), CONVERT_CONC( 103 ) / 'ACLI            ',  175, 'AE',   35.50, T /
      DATA CHEMISTRY_SPC( 104 ), CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), SPECIES_MOLWT( 104 ), CONVERT_CONC( 104 ) / 'ACLJ            ',  174, 'AE',   35.50, T /
      DATA CHEMISTRY_SPC( 105 ), CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), SPECIES_MOLWT( 105 ), CONVERT_CONC( 105 ) / 'ACLK            ',  177, 'AE',   35.50, T /
      DATA CHEMISTRY_SPC( 106 ), CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), SPECIES_MOLWT( 106 ), CONVERT_CONC( 106 ) / 'AISO3J          ',  183, 'AE',  168.20, T /
      DATA CHEMISTRY_SPC( 107 ), CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), SPECIES_MOLWT( 107 ), CONVERT_CONC( 107 ) / 'AXYL1J          ',  126, 'AE',  192.00, T /
      DATA CHEMISTRY_SPC( 108 ), CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), SPECIES_MOLWT( 108 ), CONVERT_CONC( 108 ) / 'AOLGAJ          ',  184, 'AE',  176.40, T /
      DATA CHEMISTRY_SPC( 109 ), CGRID_INDEX( 109 ), SPECIES_TYPE( 109 ), SPECIES_MOLWT( 109 ), CONVERT_CONC( 109 ) / 'AXYL2J          ',  127, 'AE',  192.00, T /
      DATA CHEMISTRY_SPC( 110 ), CGRID_INDEX( 110 ), SPECIES_TYPE( 110 ), SPECIES_MOLWT( 110 ), CONVERT_CONC( 110 ) / 'ATOL1J          ',  129, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 111 ), CGRID_INDEX( 111 ), SPECIES_TYPE( 111 ), SPECIES_MOLWT( 111 ), CONVERT_CONC( 111 ) / 'ATOL2J          ',  130, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 112 ), CGRID_INDEX( 112 ), SPECIES_TYPE( 112 ), SPECIES_MOLWT( 112 ), CONVERT_CONC( 112 ) / 'ABNZ1J          ',  132, 'AE',  144.00, T /
      DATA CHEMISTRY_SPC( 113 ), CGRID_INDEX( 113 ), SPECIES_TYPE( 113 ), SPECIES_MOLWT( 113 ), CONVERT_CONC( 113 ) / 'ABNZ2J          ',  133, 'AE',  144.00, T /
      DATA CHEMISTRY_SPC( 114 ), CGRID_INDEX( 114 ), SPECIES_TYPE( 114 ), SPECIES_MOLWT( 114 ), CONVERT_CONC( 114 ) / 'ATRP1J          ',  138, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 115 ), CGRID_INDEX( 115 ), SPECIES_TYPE( 115 ), SPECIES_MOLWT( 115 ), CONVERT_CONC( 115 ) / 'AOLGBJ          ',  185, 'AE',  252.00, T /
      DATA CHEMISTRY_SPC( 116 ), CGRID_INDEX( 116 ), SPECIES_TYPE( 116 ), SPECIES_MOLWT( 116 ), CONVERT_CONC( 116 ) / 'ATRP2J          ',  139, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 117 ), CGRID_INDEX( 117 ), SPECIES_TYPE( 117 ), SPECIES_MOLWT( 117 ), CONVERT_CONC( 117 ) / 'AISO1J          ',  140, 'AE',   96.00, T /
      DATA CHEMISTRY_SPC( 118 ), CGRID_INDEX( 118 ), SPECIES_TYPE( 118 ), SPECIES_MOLWT( 118 ), CONVERT_CONC( 118 ) / 'AISO2J          ',  141, 'AE',   96.00, T /
      DATA CHEMISTRY_SPC( 119 ), CGRID_INDEX( 119 ), SPECIES_TYPE( 119 ), SPECIES_MOLWT( 119 ), CONVERT_CONC( 119 ) / 'ASQTJ           ',  142, 'AE',  378.00, T /
      DATA CHEMISTRY_SPC( 120 ), CGRID_INDEX( 120 ), SPECIES_TYPE( 120 ), SPECIES_MOLWT( 120 ), CONVERT_CONC( 120 ) / 'APAH1J          ',  135, 'AE',  243.00, T /
      DATA CHEMISTRY_SPC( 121 ), CGRID_INDEX( 121 ), SPECIES_TYPE( 121 ), SPECIES_MOLWT( 121 ), CONVERT_CONC( 121 ) / 'APAH2J          ',  136, 'AE',  243.00, T /
      DATA CHEMISTRY_SPC( 122 ), CGRID_INDEX( 122 ), SPECIES_TYPE( 122 ), SPECIES_MOLWT( 122 ), CONVERT_CONC( 122 ) / 'AALK1J          ',  124, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 123 ), CGRID_INDEX( 123 ), SPECIES_TYPE( 123 ), SPECIES_MOLWT( 123 ), CONVERT_CONC( 123 ) / 'AALK2J          ',  125, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 124 ), CGRID_INDEX( 124 ), SPECIES_TYPE( 124 ), SPECIES_MOLWT( 124 ), CONVERT_CONC( 124 ) / 'APOCI           ',  145, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 125 ), CGRID_INDEX( 125 ), SPECIES_TYPE( 125 ), SPECIES_MOLWT( 125 ), CONVERT_CONC( 125 ) / 'APNCOMI         ',  147, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 126 ), CGRID_INDEX( 126 ), SPECIES_TYPE( 126 ), SPECIES_MOLWT( 126 ), CONVERT_CONC( 126 ) / 'APOCJ           ',  144, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 127 ), CGRID_INDEX( 127 ), SPECIES_TYPE( 127 ), SPECIES_MOLWT( 127 ), CONVERT_CONC( 127 ) / 'APNCOMJ         ',  146, 'AE',  220.00, T /
      DATA CHEMISTRY_SPC( 128 ), CGRID_INDEX( 128 ), SPECIES_TYPE( 128 ), SPECIES_MOLWT( 128 ), CONVERT_CONC( 128 ) / 'FORM_PRIMARY    ',  103, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC( 129 ), CGRID_INDEX( 129 ), SPECIES_TYPE( 129 ), SPECIES_MOLWT( 129 ), CONVERT_CONC( 129 ) / 'ALD2_PRIMARY    ',  104, 'GC',   44.00, F /
      DATA CHEMISTRY_SPC( 130 ), CGRID_INDEX( 130 ), SPECIES_TYPE( 130 ), SPECIES_MOLWT( 130 ), CONVERT_CONC( 130 ) / 'BUTADIENE13     ',  105, 'GC',   54.00, F /
      DATA CHEMISTRY_SPC( 131 ), CGRID_INDEX( 131 ), SPECIES_TYPE( 131 ), SPECIES_MOLWT( 131 ), CONVERT_CONC( 131 ) / 'ACROLEIN        ',  106, 'GC',   56.10, F /
      DATA CHEMISTRY_SPC( 132 ), CGRID_INDEX( 132 ), SPECIES_TYPE( 132 ), SPECIES_MOLWT( 132 ), CONVERT_CONC( 132 ) / 'ACROLEIN_PRIMARY',  107, 'GC',   56.10, F /
      DATA CHEMISTRY_SPC( 133 ), CGRID_INDEX( 133 ), SPECIES_TYPE( 133 ), SPECIES_MOLWT( 133 ), CONVERT_CONC( 133 ) / 'TOLU            ',  108, 'GC',   92.00, F /
      DATA CHEMISTRY_SPC( 134 ), CGRID_INDEX( 134 ), SPECIES_TYPE( 134 ), SPECIES_MOLWT( 134 ), CONVERT_CONC( 134 ) / 'MXYL            ',  109, 'GC',  106.20, F /
      DATA CHEMISTRY_SPC( 135 ), CGRID_INDEX( 135 ), SPECIES_TYPE( 135 ), SPECIES_MOLWT( 135 ), CONVERT_CONC( 135 ) / 'OXYL            ',  110, 'GC',  106.20, F /
      DATA CHEMISTRY_SPC( 136 ), CGRID_INDEX( 136 ), SPECIES_TYPE( 136 ), SPECIES_MOLWT( 136 ), CONVERT_CONC( 136 ) / 'PXYL            ',  111, 'GC',  106.20, F /
      DATA CHEMISTRY_SPC( 137 ), CGRID_INDEX( 137 ), SPECIES_TYPE( 137 ), SPECIES_MOLWT( 137 ), CONVERT_CONC( 137 ) / 'APIN            ',  112, 'GC',  136.30, F /
      DATA CHEMISTRY_SPC( 138 ), CGRID_INDEX( 138 ), SPECIES_TYPE( 138 ), SPECIES_MOLWT( 138 ), CONVERT_CONC( 138 ) / 'BPIN            ',  113, 'GC',  136.30, F /
      DATA CHEMISTRY_SPC( 139 ), CGRID_INDEX( 139 ), SPECIES_TYPE( 139 ), SPECIES_MOLWT( 139 ), CONVERT_CONC( 139 ) / 'HG              ',  114, 'GC',  200.60, F /
      DATA CHEMISTRY_SPC( 140 ), CGRID_INDEX( 140 ), SPECIES_TYPE( 140 ), SPECIES_MOLWT( 140 ), CONVERT_CONC( 140 ) / 'HGIIAER         ',  115, 'GC',  200.60, F /
      DATA CHEMISTRY_SPC( 141 ), CGRID_INDEX( 141 ), SPECIES_TYPE( 141 ), SPECIES_MOLWT( 141 ), CONVERT_CONC( 141 ) / 'HGIIGAS         ',  116, 'GC',  200.60, F /

      INTEGER, PARAMETER :: N_ACT_SP = 141

      INTEGER, PARAMETER :: NRXNS = 331

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
     &      3,    0,    9,    3,    3,    3,    3,    3,    0,    3, & ! 6   
     &      1,    0,    0,    3,    1,    3,    3,    1,    3,    1, & ! 7   
     &      3,    3,    3,    0,    3,   10,   10,    0,    3,    3, & ! 8   
     &      3,    3,    3,    0,    3,    3,    3,    1,    0,    3, & ! 9   
     &     10,   10,    0,    1,    3,    3,    3,    3,    3,    1, & ! O   
     &      3,    1,    1,    3,    1,    3,    3,    3,   10,    3, & ! 1   
     &      3,    1,    3,    3,    3,    3,    3,    3,    3,    1, & ! 2   
     &      1,    1,    1,    1,    1,    1,    3,    3,    0,    3, & ! 3   
     &      0,    1,    3,    1,    1,    1,    3,    3,    1,    1, & ! 4   
     &      1,    3,    3,    1,    0,    1,    3,    3,    3,    3, & ! 5   
     &      3,    1,    1,    1,    3,    1,    1,    3,    3,    1, & ! 6   
     &      1,    1,    0,    1,    3,    3,    3,    1,    1,    3, & ! 7   
     &      3,    3,   10,    3,    3,    1,    0,    0,    3,    1, & ! 8   
     &      3,    3,    1,    0,    3,    1,    3,    1,    1,    1, & ! 9   
     &      1,    3,    1,    1,    1,    3,    4,    1,    1,    1, & ! O   
     &     10,    0,    3,    3,    3,    3,    3,    3,    3,    1, & ! 1   
     &      1,    1,    3,    3,    3,    1,    3,    3,    3,    1, & ! 2   
     &      1,    3,    3,    1,    3,    1,    1,    1,    1,    3, & ! 3   
     &      3,    1,    1,    1,    3,    3,    1,    1,    0,    0, & ! 4   
     &      0,    0,    0,    0,   -1,   -1,   -1,   -1,   -1,   -1, & ! 5   
     &     -1,   -1,   12,   -1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 7   
     &     -1,    1,   -1,    1,    1,    3,    0,    0,    3,    3, & ! 8   
     &      3,    3,    0,    1,    3,    3,    1,    1,    1,    1, & ! 9   
     &      3,    0,    1,    1,    1,    3,    0,    1,    3,    1, & ! O   
     &      3,    1,    1,    1,    1,    1,    1,    3,    3,    3, & ! 1   
     &      1,    1,    1,    3,    1,    1,    3,    1,    1,    1, & ! 2   
     &      3/     !  3   

      INTEGER            :: IRXBITS( NRXNS )

      DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,  276,    0,    0,    1,    1,    0,    2,    2,  260, & ! O   
     &      8,    0,    0,    2,    2,    0,    0,    1,    8,    8, & ! 1   
     &      1,   16,    8,    1,    2,    0,    0,    1,    0,    0, & ! 2   
     &      1,    1,    0,    0,    8,    2,    0,  128,  128,    0, & ! 3   
     &      0,    1,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      2,    2,    2,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    2,    0,   64,    0,    0,    0,    0,    2,    0, & ! 6   
     &      0,    2,    2,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    2,    0,    1,    1,    2,    0,    0, & ! 8   
     &      0,    0,    0,    2,    0,    0,    0,    0,    2,    0, & ! 9   
     &      1,    1,    2,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    1,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    2,    0, & ! 3   
     &      2,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    2,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    2,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    1,    0,    0,    0,    2,    2,    0,    0, & ! 8   
     &      0,    0,    0,    2,   64,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      1,    2,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    2,    2, & ! 4   
     &      2,    2,    2,    2,    1,    1,    1,    1,    1,    1, & ! 5   
     &      1,    1,    0,    1,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      1,    0,    1,    0,    0,    0,    2,    2,    0,    0, & ! 8   
     &      0,    0,    2,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    2,    0,    0,    0,    0,    2,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &    260/     !  3   

      INTEGER            :: IORDER( NRXNS )

      DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    2,    2,    2,    2,    2,    1,    1,    2, & ! O   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    3, & ! 1   
     &      1,    3,    3,    2,    1,    2,    2,    2,    2,    2, & ! 2   
     &      2,    1,    2,    2,    3,    1,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      1,    1,    1,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    1,    2,    2,    2,    2,    2,    2,    1,    2, & ! 6   
     &      2,    1,    1,    2,    2,    2,    1,    2,    2,    2, & ! 7   
     &      2,    2,    2,    1,    2,    2,    1,    1,    2,    2, & ! 8   
     &      2,    2,    2,    1,    2,    2,    2,    2,    1,    2, & ! 9   
     &      2,    1,    1,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      1,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    2, & ! 3   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      1,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    1,    1,    2,    2, & ! 8   
     &      2,    2,    2,    1,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    2,    2, & ! 5   
     &      2,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    1,    1,    2,    2, & ! 8   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    1,    2,    2,    2,    2,    1,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      3/     !  3   

      INTEGER, PARAMETER :: KTN1 = 124
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     11,   19,   20,   23,   27,   38,   46,   47,   48,   49, & ! O   
     &     58,   59,   60,   71,   75,   78,   80,   98,  104,  110, & ! 1   
     &    112,  113,  115,  122,  130,  131,  132,  133,  134,  135, & ! 2   
     &    136,  142,  144,  145,  146,  149,  150,  151,  154,  156, & ! 3   
     &    162,  163,  164,  166,  167,  170,  171,  172,  174,  178, & ! 4   
     &    179,  186,  190,  193,  196,  198,  199,  200,  201,  203, & ! 5   
     &    204,  205,  208,  209,  210,  220,  221,  222,  226,  230, & ! 6   
     &    231,  234,  236,  237,  238,  239,  242,  243,  244,  247, & ! 7   
     &    248,  265,  266,  267,  268,  269,  270,  271,  272,  273, & ! 8   
     &    274,  275,  276,  277,  278,  279,  280,  282,  284,  285, & ! 9   
     &    294,  297,  298,  299,  300,  303,  304,  305,  308,  310, & ! O   
     &    312,  313,  314,  315,  316,  317,  321,  322,  323,  325, & ! 1   
     &    326,  328,  329,  330/     !  2   

      INTEGER, PARAMETER :: KTN2 =   1
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &      2/

      INTEGER, PARAMETER :: KTN3 = 135
      INTEGER            :: KRX3( KTN3 )

      DATA ( KRX3( IRXXN ), IRXXN = 1, KTN3 ) / & 
     &      3,    4,    7,   10,   12,   13,   16,   17,   22,   26, & ! O   
     &     30,   33,   37,   39,   40,   41,   43,   44,   45,   50, & ! 1   
     &     54,   55,   56,   57,   61,   64,   65,   66,   67,   68, & ! 2   
     &     70,   74,   76,   77,   79,   81,   82,   83,   85,   89, & ! 3   
     &     90,   91,   92,   93,   95,   96,   97,  100,  105,  106, & ! 4   
     &    107,  108,  109,  111,  114,  116,  117,  118,  120,  121, & ! 5   
     &    123,  124,  125,  126,  127,  128,  129,  137,  138,  140, & ! 6   
     &    143,  147,  148,  152,  153,  157,  158,  159,  160,  161, & ! 7   
     &    165,  168,  169,  175,  176,  177,  180,  181,  182,  184, & ! 8   
     &    185,  189,  191,  192,  195,  197,  202,  206,  213,  214, & ! 9   
     &    215,  216,  217,  218,  219,  223,  224,  225,  227,  228, & ! O   
     &    229,  232,  233,  235,  240,  241,  245,  246,  286,  289, & ! 1   
     &    290,  291,  292,  295,  296,  301,  306,  309,  311,  318, & ! 2   
     &    319,  320,  324,  327,  331/     !  3   

      INTEGER, PARAMETER :: KTN4 =   1
      INTEGER            :: KRX4( KTN4 )

      DATA ( KRX4( IRXXN ), IRXXN = 1, KTN4 ) / & 
     &    207/

      INTEGER, PARAMETER :: KTN5 =   0
      INTEGER            :: KRX5( 1 )

      DATA   KRX5( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN6 =   0
      INTEGER            :: KRX6( 1 )

      DATA   KRX6( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN7 =   0
      INTEGER            :: KRX7( 1 )

      DATA   KRX7( 1 ) / 0 /

      INTEGER, PARAMETER :: NWM =   3
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &      2,   10,  331/
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
     &     64,  195/
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
     &     18,   18,    7,    7,   22,   22,   22,   24,   24,   25, & ! 6   
     &     23,   23,   23,   23,   23,   23,   26,   26,   26,   27, & ! 7   
     &     19,   19,   19,   19,   28,   28,   29,   29,   28,   28, & ! 8   
     &     28,   28,   30,   30,   31,   20,   20,   20,   20,   32, & ! 9   
     &     32,   33,   33,   33,   32,   32,   32,   32,   32,   34, & ! O   
     &     35,   35,   35,    3,    7,    4,    5,    3,    7,    4, & ! 1   
     &      5,   38,   38,   38,   38,   39,   41,   41,    7,   40, & ! 2   
     &     45,   45,   47,   47,   48,   48,   49,   49,   50,   50, & ! 3   
     &     43,   43,   43,   43,   46,   46,   52,   52,   51,   51, & ! 4   
     &     53,    7,    7,    7,   44,    3,    7,   60,   61,   60, & ! 5   
     &     61,   60,   61,   60,   28,   64,   65,    4,    5,    7, & ! 6   
     &      4,    5,   59,   67,   67,   69,   69,   69,   14,   28, & ! 7   
     &     67,   67,   70,    7,    7,    1,   75,   77,   76,   78, & ! 8   
     &     78,   78,    7,   79,   76,   76,   76,   76,   76,   76, & ! 9   
     &     76,   76,   76,   76,   76,   76,   80,   76,   76,   76, & ! O   
     &     76,   81,   42,   42,   55,   55,   86,   87,   87,   90, & ! 1   
     &     90,   90,   57,   57,   94,   53,   51,   51,   51,   17, & ! 2   
     &     16,   98,   98,   98,   28,   98,   96,   97,   63,   99, & ! 3   
     &     99,   99,   99,   66,  100,  100,  100,  100,   63,   66, & ! 4   
     &     17,   16,   96,   97,    9,    9,  101,  102,  101,  101, & ! 5   
     &    102,    1,    4,   65,  107,  109,  110,  111,  112,  113, & ! 6   
     &    114,  116,  117,  118,  119,  120,  121,  122,  123,  124, & ! 7   
     &    125,  126,  127,  128,  128,  128,  128,  128,  128,  129, & ! 8   
     &    129,  129,  129,  129,  130,  130,  130,  130,  132,  132, & ! 9   
     &    132,  132,  132,  131,  131,  131,  131,  131,  133,  133, & ! O   
     &    134,  134,  135,  135,  136,  136,  137,  137,  137,  137, & ! 1   
     &    137,  138,  138,  138,  138,  138,  139,  139,  139,  139, & ! 2   
     &    139/     !  3   

      DATA ( IRR( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    2,    1,    1,    2,    4,    0,    0,    0, & ! O   
     &      0,    7,    8,    0,    0,    2,    1,    1,    0,    0, & ! 1   
     &      0,    2,    1,    7,    0,   11,   11,    7,   10,    2, & ! 2   
     &      1,    0,   12,    8,    8,    0,   13,    0,    0,    3, & ! 3   
     &      7,    7,    8,    3,    3,    3,    7,    8,    4,    5, & ! 4   
     &      0,    0,    0,    2,    2,    8,    8,   14,   15,   15, & ! 5   
     &      7,    0,   21,    0,    2,    8,   22,    7,    0,    7, & ! 6   
     &      7,    0,    0,    3,    5,    8,    0,    2,    8,    7, & ! 7   
     &      3,    7,    5,    0,    2,    1,    0,    0,    8,   22, & ! 8   
     &     14,   28,    7,    0,    7,    3,    7,    5,    0,    2, & ! 9   
     &      1,    0,    0,    7,    8,   22,   14,   32,   28,    7, & ! O   
     &      0,    0,    1,   36,   36,   36,   36,   37,   37,   37, & ! 1   
     &     37,    3,    7,    4,    5,    7,    2,    8,   40,    5, & ! 2   
     &      1,    8,    7,    5,    1,    4,    2,    8,    0,    7, & ! 3   
     &      0,    7,    4,    5,    7,    5,    2,    8,    2,    1, & ! 4   
     &      0,   54,   56,   44,    0,   58,   58,    2,    2,    8, & ! 5   
     &      8,   14,   15,   15,   60,    7,    7,   58,   58,   59, & ! 6   
     &     59,   59,    0,    3,    7,    2,    8,   69,   69,   69, & ! 7   
     &      4,    5,    7,   73,   74,   58,    0,    0,    4,   78, & ! 8   
     &      2,    8,   79,    0,    0,   34,   74,   37,   36,   38, & ! 9   
     &     58,   23,   19,   20,   25,   73,    7,   39,   54,   56, & ! O   
     &      1,    0,    2,    8,    2,    8,    7,    2,    8,    4, & ! 1   
     &      7,    5,    2,    8,    7,    7,    8,   22,   28,    7, & ! 2   
     &      7,    2,    8,   15,   98,   22,    7,    7,    7,    2, & ! 3   
     &      8,   22,   28,    7,    2,    8,   22,   28,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,  103,  104, & ! 5   
     &    105,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    7, & ! 7   
     &      7,    7,    7,    7,    5,    3,    0,    0,   76,    7, & ! 8   
     &      5,    3,    0,   76,    7,    4,    5,   76,    7,    4, & ! 9   
     &      5,    0,   76,    7,    4,    5,    0,   76,    7,   76, & ! O   
     &      7,   76,    7,   76,    7,   76,    3,    7,    4,    5, & ! 1   
     &     76,    3,    7,    4,    5,   76,    4,   75,   13,    7, & ! 2   
     &     76/     !  3   

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
     &      0/     !  3   

      DATA ( IRR( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &      2,    4,    1,    2,    5,    1,    5,    3,    6,    3, & ! O   
     &      7,    8,    7,    1,    2,    1,    2,    9,   10,   10, & ! 1   
     &      5,    1,   11,   11,    2,    1,    2,   10,    5,    7, & ! 2   
     &     12,    8,    1,   13,   13,    7,    8,    7,    8,    8, & ! 3   
     &      3,   13,    0,    7,    7,    1,    8,   10,    1,    1, & ! 4   
     &      8,    7,    1,    1,   16,   18,   18,    0,    0,    0, & ! 5   
     &     14,    7,    8,   22,   23,   24,   23,   22,   23,   23, & ! 6   
     &      8,    8,   21,    7,   10,   26,   23,   27,   24,    8, & ! 7   
     &     28,   28,   28,   22,   22,   29,   28,    1,   30,   22, & ! 8   
     &     22,   22,   28,   22,   22,   32,   32,   32,   22,   19, & ! 9   
     &     33,   32,   32,   19,   30,   19,   19,   19,   22,   14, & ! O   
     &     14,    8,   17,   19,   23,   19,    1,   23,   14,   23, & ! 1   
     &      1,   19,   19,   19,   19,    8,    1,    0,   45,   45, & ! 2   
     &     47,   40,   48,   48,   16,   49,   48,   50,   48,   49, & ! 3   
     &     51,   51,   20,   51,   52,   45,    1,    0,    1,   53, & ! 4   
     &     51,    8,    8,   14,   28,   59,   59,    1,   63,   64, & ! 5   
     &     64,    0,    0,    0,   22,   65,   23,   59,   59,   34, & ! 6   
     &     28,   20,   21,   20,    8,   63,   18,    0,    0,   22, & ! 7   
     &      7,    1,   71,    8,   19,   59,   76,    7,   78,   75, & ! 8   
     &     76,   77,   76,   76,   80,   80,   80,   79,   79,   80, & ! 9   
     &     80,   80,   80,   80,   80,   80,   76,   80,   80,   80, & ! O   
     &     81,   76,    2,    8,    2,    8,    7,    2,    8,    4, & ! 1   
     &      7,    5,    2,    8,    7,   34,   28,   23,   22,   14, & ! 2   
     &     14,   97,   97,    0,   97,   97,   14,   14,   99,    1, & ! 3   
     &     66,    1,    1,  100,    1,   66,   66,   66,    1,    1, & ! 4   
     &      1,    1,    1,    1,   10,   10,   10,   10,   81,   81, & ! 5   
     &     81,   11,    0,  106,  108,  108,  108,  108,  108,  108, & ! 6   
     &    115,  115,  115,  115,  115,  108,  108,  108,  108,  125, & ! 7   
     &      7,  127,    7,    7,    5,    3,    0,    0,   76,    7, & ! 8   
     &      5,    3,    0,   76,    7,    4,    5,   76,    7,    4, & ! 9   
     &      5,    0,   76,    7,    4,    5,    0,   76,    7,   76, & ! O   
     &      7,   76,    7,   76,    7,   76,    3,    7,    4,    5, & ! 1   
     &     76,    3,    7,    4,    5,   76,  140,  141,  141,  140, & ! 2   
     &    139/     !  3   

      DATA ( IRR( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &      3,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    3,    0,    0,    1,    0,    0,    0, & ! 1   
     &      1,    0,    0,    0,    7,    0,    1,    0,    0,    1, & ! 2   
     &      0,    1,    0,    0,    0,    0,    0,    8,    0,    0, & ! 3   
     &      0,    0,    0,    0,    8,    0,    1,    0,    0,    0, & ! 4   
     &      1,    1,    5,    0,   17,    0,    0,    0,    0,    0, & ! 5   
     &     19,    8,    0,    0,    8,    0,    8,   14,    8,    8, & ! 6   
     &     21,   21,    0,    8,    8,    0,    8,    1,    0,    0, & ! 7   
     &      7,    0,   10,   21,    1,    0,    1,   28,   31,    8, & ! 8   
     &     31,    0,    0,    7,    0,    7,    0,   10,   21,    1, & ! 9   
     &      0,    1,    1,    1,   31,   14,   31,   14,   14,   15, & ! O   
     &     19,    0,    0,   20,   19,   23,   23,    8,   23,   21, & ! 1   
     &     14,   20,   20,   20,   20,   14,    8,    0,   14,   10, & ! 2   
     &      0,    0,    0,   10,    0,    0,    1,    0,    7,    0, & ! 3   
     &      8,   52,   51,   10,    0,   10,    8,    0,   14,    0, & ! 4   
     &      1,   14,   14,   28,    8,   23,   23,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,   31,    7,   19,   23,   63,   23, & ! 6   
     &     23,   23,   19,   34,   14,    0,    0,    0,    0,   31, & ! 7   
     &      8,    8,    8,   19,   14,   16,    0,   76,    0,   76, & ! 8   
     &      1,    0,   21,   21,   22,   14,   19,   14,   19,   79, & ! 9   
     &     14,    8,   28,   32,    8,    8,    0,   14,   14,   14, & ! O   
     &      0,    1,   82,   83,   84,   85,   87,   88,   89,   91, & ! 1   
     &     91,   91,   92,   93,   95,   21,   23,    8,   23,    1, & ! 2   
     &      1,   34,    0,    0,   22,   23,    1,    1,    0,   66, & ! 3   
     &      0,   66,   66,    0,   66,    0,    1,    1,    8,    8, & ! 4   
     &      8,    8,    8,    8,  101,  102,    0,    0,    0,    0, & ! 5   
     &      0,   10,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,  124, & ! 7   
     &      0,  126,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,  131,  131,  131,  131,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,  141,   75,   13,  141, & ! 2   
     &    141/     !  3   

      DATA ( IRR( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      7,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     20,   19,    0,    0,    1,    0,   25,    8,    7,    0, & ! 6   
     &      0,    0,    0,   21,   21,    0,    0,    8,    0,    0, & ! 7   
     &      0,    0,    0,    8,    0,    0,    0,    5,    7,   23, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    8,    8, & ! 9   
     &      0,    0,    5,    0,    7,    8,    0,    8,    8,    8, & ! O   
     &      8,    0,    0,    8,   20,   20,   14,   21,   20,    8, & ! 1   
     &     23,    8,    8,   23,    8,   40,   43,    0,    8,   14, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &     21,    0,   23,    0,    0,    0,   23,    0,    8,    0, & ! 4   
     &      0,   40,   40,    0,   21,   14,   60,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   64,   14,   14,   14,   14, & ! 6   
     &     44,   34,   23,   68,   69,    0,    0,    0,    0,    0, & ! 7   
     &     14,   14,   72,   20,   15,   14,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    8,    0,   15,   14,    8,   20,   19, & ! 9   
     &      8,   21,    0,    0,   23,   19,    0,    8,    8,    8, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    5,   22,   28,   28,   96, & ! 2   
     &     96,    0,    0,    0,   31,   25,   96,   96,    0,    8, & ! 3   
     &      0,    8,    8,    0,    0,    0,   23,   23,   19,   23, & ! 4   
     &     23,   23,   23,   23,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    7, & ! 7   
     &      0,    7,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    4,    0,    0,    7, & ! 2   
     &     76/     !  3   

      DATA ( IRR( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      5,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,   20,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,   22,   22,   31, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   14, & ! 9   
     &      0,    0,   19,    0,   14,   31,    0,    0,   19,   19, & ! O   
     &     34,    0,    0,   14,   14,   14,   15,   14,    8,    7, & ! 1   
     &      0,   14,   14,   21,    1,   41,   16,    0,   43,    8, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   14,    0,    0,    0,   21,    0,   20,    0, & ! 4   
     &      0,   44,   44,    0,    0,    8,    8,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   59,    8,    8,    8,    8, & ! 6   
     &      8,    8,   34,    0,   23,    0,    0,    0,    0,    0, & ! 7   
     &     69,   69,    0,   23,    8,    8,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    8,   15,   23,   14,   20, & ! 9   
     &     79,    0,    0,    0,    0,    0,    0,   15,   15,   15, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   21,   22,   21,   97, & ! 2   
     &     97,    0,    0,    0,    0,    0,   97,   97,    0,   23, & ! 3   
     &      0,   19,   19,    0,    0,    0,   25,   31,   38,   19, & ! 4   
     &     19,   19,   19,   19,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    4,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    8,    0,    4,   23,    0,    0,    0,   34, & ! O   
     &     15,    0,    0,   21,    8,    7,   20,    7,    0,   27, & ! 1   
     &      0,   21,    0,    3,    0,    7,   44,    0,   46,   20, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   21,    0,    0,    0,   16,    0,    0,    0, & ! 4   
     &      0,   34,   34,    0,    0,   32,   61,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   20,   44,    7,    1,   21, & ! 6   
     &      7,   21,   14,    0,   34,    0,    0,    0,    0,    0, & ! 7   
     &     23,   20,    0,   14,    0,    2,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   19,    8,    0,    8,   36, & ! 9   
     &     59,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    7,   31,    0,   98, & ! 2   
     &     98,    0,    0,    0,    0,    0,   98,   98,    0,   19, & ! 3   
     &      0,   23,   23,    0,    0,    0,    0,    0,    0,   20, & ! 4   
     &     20,   20,   20,   20,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,   14,    0,    0,    0,    0,    0,    0,   35, & ! O   
     &     35,    0,    0,   23,   34,   21,   19,    0,    0,    0, & ! 1   
     &      0,   34,    0,    7,    0,   42,   23,    0,   21,   23, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    7,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,   41,   41,    0,    0,   34,   62,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,   23,   20,   51,   20,   44, & ! 6   
     &     14,   66,    8,    0,   20,    0,    0,    0,    0,    0, & ! 7   
     &     21,   63,    0,    0,    0,   20,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   34,    0,    0,   34,   34, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   31,    0,    0,   34, & ! 2   
     &     34,    0,    0,    0,    0,    0,    0,    0,    0,   14, & ! 3   
     &      0,   25,   31,    0,    0,    0,    0,    0,    0,   34, & ! 4   
     &     34,   34,   34,   34,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   20, & ! O   
     &     20,    0,    0,   15,    0,    8,   34,    0,    0,    0, & ! 1   
     &      0,    0,    0,    8,    0,    0,   21,    0,   15,   44, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    8,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,   55,   57,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,   31,   20,   62,   19, & ! 6   
     &     19,   51,   51,    0,   68,    0,    0,    0,    0,    0, & ! 7   
     &     34,   68,    0,    0,    0,   34,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   35,    0,    0,    0,   14, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    4,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   34, & ! 3   
     &      0,   27,   27,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,   34,    0,   34,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,   23,   43, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,   44,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,   34,   34,   34,   28, & ! 6   
     &     34,   14,   28,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     20,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,   20,    0,    0,    0,    8, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,   30,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,   34,   34,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
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
     &      0,    0,    0,    7,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   15, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,   21,    0,   51, & ! 6   
     &     21,   10,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     32,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   21, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   20, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     68,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
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

      DATA ( RTDAT( 1,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 6.0000D-34, 3.0000D-12, 5.6000D-12, 2.5000D-31, & ! O   
     &     9.0000D-32, 1.2000D-13, 1.0000D+00, 1.0000D+00, 2.1000D-11, & ! +   
     &     2.2000D-10, 1.7000D-12, 1.0000D-14, 1.0000D+00, 1.0000D+00, & ! 1   
     &     1.5000D-11, 4.5000D-14, 2.0000D-30, 1.0000D-22, 0.0000D+00, & ! +   
     &     1.0000D-03, 3.3000D-39, 5.0000D-40, 7.0000D-31, 1.0000D+00, & ! 2   
     &     1.8000D-11, 1.0000D-20, 3.2000D-30, 2.4000D-14, 3.5000D-12, & ! +   
     &     1.8000D-31, 4.1000D-05, 1.3000D-12, 2.3000D-13, 3.2200D-34, & ! 3   
     &     1.0000D+00, 2.9000D-12, 1.1000D-10, 5.5000D-12, 2.2000D-11, & ! +   
     &     4.2000D-12, 6.9000D-31, 4.8000D-11, 3.0000D-11, 1.4000D-12, & ! 4   
     &     1.0000D-11, 2.2000D-11, 3.5000D-12, 1.0000D-17, 8.5000D-13, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.6000D-12, 2.6000D-12, & ! 5   
     &     7.5000D-13, 7.5000D-13, 6.8000D-14, 6.8000D-14, 6.8000D-14, & ! +   
     &     3.0100D-12, 1.0000D+00, 1.4400D-13, 2.4500D-12, 2.8000D-12, & ! 6   
     &     4.1000D-13, 9.5000D-14, 3.8000D-12, 1.0000D+00, 7.3000D-12, & ! +   
     &     9.0000D-12, 1.0000D+00, 1.0000D+00, 3.4000D-11, 5.8000D-16, & ! 7   
     &     9.7000D-15, 2.4000D+12, 5.6000D-12, 5.6000D-15, 4.0000D-13, & ! +   
     &     1.8000D-11, 5.6000D-12, 1.4000D-12, 1.0000D+00, 8.1000D-12, & ! 8   
     &     2.7000D-28, 4.9000D-03, 1.0000D+00, 4.3000D-13, 2.0000D-12, & ! +   
     &     4.4000D-13, 2.9000D-12, 4.0000D-13, 1.0000D+00, 4.0000D-13, & ! 9   
     &     1.3000D-11, 5.1000D-12, 6.5000D-15, 1.0000D+00, 6.7000D-12, & ! +   
     &     2.7000D-28, 1.7000D-03, 1.0000D+00, 3.0000D-13, 4.3000D-13, & ! O   
     &     2.0000D-12, 4.4000D-13, 2.9000D-12, 2.9000D-12, 8.1000D-13, & ! +   
     &     1.0000D+15, 1.6000D+03, 1.5000D-11, 1.0000D-11, 3.2000D-11, & ! 1   
     &     6.5000D-15, 7.0000D-13, 1.0400D-11, 1.0000D-28, 1.2000D-14, & ! +   
     &     3.3000D-12, 2.3000D-11, 1.0000D-11, 8.4000D-15, 9.6000D-13, & ! 2   
     &     1.8000D-12, 2.7000D-12, 1.9000D-13, 1.7000D-12, 1.4000D-11, & ! +   
     &     2.1000D-12, 5.5000D-12, 1.5300D-12, 3.8000D-12, 2.1000D-12, & ! 3   
     &     2.8600D-13, 2.5400D-12, 2.4000D-13, 1.0000D-02, 1.9000D-12, & ! +   
     &     4.0000D-02, 4.4000D-11, 5.4000D-17, 3.8000D-12, 7.0000D-11, & ! 4   
     &     1.7000D-10, 2.5400D-12, 2.4000D-13, 1.1000D-11, 1.1000D-11, & ! +   
     &     1.0000D-04, 1.7000D-11, 1.7000D-11, 1.8000D-11, 1.0000D+00, & ! 5   
     &     3.6000D-11, 2.5400D-11, 2.6000D-12, 2.6000D-12, 7.5000D-13, & ! +   
     &     7.5000D-13, 6.8000D-14, 6.8000D-14, 6.8000D-14, 4.4000D-13, & ! 6   
     &     9.6000D-11, 3.6000D-11, 7.8600D-15, 3.0300D-12, 3.3600D-11, & ! +   
     &     7.1000D-18, 1.0000D-15, 3.6000D-03, 3.6000D-11, 1.5000D-11, & ! 7   
     &     2.6000D-12, 7.5000D-13, 6.8000D-14, 6.8000D-14, 4.4000D-13, & ! +   
     &     1.2000D-15, 3.7000D-12, 3.3000D-31, 6.9000D-12, 8.7000D-12, & ! 8   
     &     1.5000D-19, 1.0000D+00, 1.0000D+00, 2.3000D-11, 1.6300D-14, & ! +   
     &     6.4000D-12, 2.7000D-12, 5.0000D-13, 1.0000D+00, 6.6000D-12, & ! 9   
     &     5.0000D-11, 8.3000D-11, 1.0700D-10, 2.5000D-10, 3.5000D-10, & ! +   
     &     4.3000D-10, 8.2000D-11, 7.9000D-11, 1.3000D-10, 5.5000D-11, & ! O   
     &     8.2000D-11, 6.5800D-13, 6.1000D-11, 1.2000D-10, 1.2000D-10, & ! +   
     &     1.8000D-31, 1.0000D+00, 2.7000D-12, 1.9000D-13, 2.7000D-12, & ! 1   
     &     1.9000D-13, 2.4700D-12, 2.7000D-12, 1.9000D-13, 1.1600D-14, & ! +   
     &     1.9700D-10, 1.9000D-11, 2.7000D-12, 1.9000D-13, 2.7000D-12, & ! 2   
     &     2.9000D-11, 4.3000D-13, 2.0000D-12, 2.9000D-12, 1.2900D-12, & ! +   
     &     7.2600D-12, 2.6000D-12, 7.5000D-13, 6.8000D-14, 4.4000D-13, & ! 3   
     &     2.5000D-13, 1.1000D-12, 5.7000D-12, 3.3000D-11, 2.7000D-12, & ! +   
     &     2.0500D-13, 2.5000D-13, 5.0000D-12, 2.8300D-12, 2.7000D-12, & ! 4   
     &     2.0500D-13, 2.5000D-13, 5.0000D-12, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D-40, 1.0000D+00, 9.4882D-06, & ! 6   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 7   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 2.5000D-12, & ! +   
     &     1.0000D+00, 2.5000D-12, 1.0000D+00, 9.0000D-12, 5.8000D-16, & ! 8   
     &     3.4000D-11, 1.0000D+00, 1.0000D+00, 8.2000D-11, 5.6000D-12, & ! +   
     &     1.4000D-12, 1.8000D-11, 1.0000D+00, 7.9000D-11, 1.4000D-11, & ! 9   
     &     8.2000D-15, 1.7900D-13, 2.5100D-10, 2.0000D-11, 2.6100D-19, & ! +   
     &     1.7000D-11, 1.0000D+00, 2.3700D-10, 2.0000D-11, 2.6100D-19, & ! O   
     &     1.7000D-11, 1.0000D+00, 2.3700D-10, 1.8000D-12, 6.1000D-11, & ! +   
     &     1.7000D-11, 1.4000D-10, 1.2200D-11, 1.5000D-10, 1.3000D-11, & ! 1   
     &     1.5000D-10, 2.7900D-11, 1.2000D-11, 6.3000D-16, 1.2000D-12, & ! +   
     &     4.7000D-10, 2.8100D-11, 7.5100D-11, 1.7400D-15, 2.8100D-11, & ! 2   
     &     5.3000D-10, 2.1100D-18, 2.6000D-18, 8.5000D-19, 7.7000D-14, & ! +   
     &     2.2500D-33/           !        3   

      DATA ( RTDAT( 2,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00,-2.4000D+00, 0.0000D+00, 0.0000D+00,-1.8000D+00, & ! O   
     &    -1.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00,-4.4000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -3.5000D+00, 0.0000D+00, 0.0000D+00,-2.6000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00,-4.5000D+00, 4.6000D+02, 0.0000D+00, & ! +   
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
     &    -7.1000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -7.1000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-8.0000D-01, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00,-4.3000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 1.1600D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -2.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 4.0582D-09, 0.0000D+00, 0.0000D+00, & ! 6   
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
     &     0.0000D+00/           !        3   

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
     &     1.9000D+02, 0.0000D+00, 3.4300D-33,-1.7750D+03, 3.0000D+02, & ! 6   
     &     7.5000D+02, 3.9000D+02, 2.0000D+02, 0.0000D+00,-6.2000D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.6000D+03, 0.0000D+00, & ! 7   
     &     6.2500D+02,-7.0000D+03, 0.0000D+00, 2.3000D+03, 0.0000D+00, & ! +   
     &    -1.1000D+03, 2.7000D+02,-1.9000D+03, 0.0000D+00, 2.7000D+02, & ! 8   
     &     0.0000D+00,-1.2100D+04, 0.0000D+00, 1.0400D+03, 5.0000D+02, & ! +   
     &     1.0700D+03, 5.0000D+02, 2.0000D+02, 0.0000D+00, 2.0000D+02, & ! 9   
     &    -8.7000D+02, 4.0500D+02, 0.0000D+00, 0.0000D+00, 3.4000D+02, & ! +   
     &     0.0000D+00,-1.1280D+04, 0.0000D+00, 0.0000D+00, 1.0400D+03, & ! O   
     &     5.0000D+02, 1.0700D+03, 5.0000D+02, 5.0000D+02, 0.0000D+00, & ! +   
     &    -8.0000D+03, 0.0000D+00, 0.0000D+00,-2.8000D+02, 0.0000D+00, & ! 1   
     &    -1.9000D+03,-2.1600D+03,-7.9200D+02, 0.0000D+00,-2.6300D+03, & ! +   
     &    -2.8800D+03, 0.0000D+00, 5.5000D+02,-1.1000D+03,-2.7000D+02, & ! 2   
     &     3.5500D+02, 3.6000D+02, 1.3000D+03, 9.5000D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 3.6000D+02, 1.3000D+03, 0.0000D+00, 1.9000D+02, & ! +   
     &     0.0000D+00, 0.0000D+00,-5.0000D+02, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 3.6000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 1.1600D+02, 1.1600D+02, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 4.0760D+02, 3.6500D+02, 3.6500D+02, 7.0000D+02, & ! +   
     &     7.0000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 1.0700D+03, & ! 6   
     &     0.0000D+00, 0.0000D+00,-1.9120D+03,-4.4800D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.4900D+02, & ! 7   
     &     3.6500D+02, 7.0000D+02, 0.0000D+00, 0.0000D+00, 1.0700D+03, & ! +   
     &    -8.2100D+02, 1.7500D+02, 0.0000D+00,-2.3000D+02,-1.0700D+03, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-2.0000D+02, 0.0000D+00, & ! +   
     &     2.9000D+02, 2.2000D+02, 0.0000D+00, 0.0000D+00,-1.2400D+03, & ! 9   
     &     0.0000D+00,-1.0000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-3.4000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &     4.5000D+01, 5.8000D+01, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.6000D+02, 1.3000D+03, 3.6000D+02, & ! 1   
     &     1.3000D+03,-2.0600D+02, 3.6000D+02, 1.3000D+03, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 3.6000D+02, 1.3000D+03, 3.7400D+02, & ! 2   
     &     0.0000D+00, 1.0400D+03, 5.0000D+02, 5.0000D+02, 0.0000D+00, & ! +   
     &     0.0000D+00, 3.6500D+02, 7.0000D+02, 0.0000D+00, 1.0700D+03, & ! 3   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.6000D+02, & ! +   
     &     1.3000D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 3.6000D+02, & ! 4   
     &     1.3000D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &    -1.6000D+03, 0.0000D+00, 0.0000D+00,-3.4000D+01, 2.7000D+02, & ! +   
     &    -1.9000D+03,-1.1000D+03, 0.0000D+00, 0.0000D+00, 4.2400D+02, & ! 9   
     &    -2.0700D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -3.1310D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! O   
     &    -3.1310D+03, 0.0000D+00, 0.0000D+00, 3.5500D+02, 0.0000D+00, & ! +   
     &     1.1600D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 4.4000D+02,-5.8000D+02, 4.9000D+02, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.2600D+03, 0.0000D+00, & ! 2   
     &     0.0000D+00,-1.2565D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -6.8000D+02/           !        3   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &      5,    6,   18,   21,   24,   28,   29,   31,   32,   34, & 
     &     35,   42,   63,   86,   87,  101,  102,  119,  183,  211, & 
     &    263/

      DATA ( RFDAT( 1,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     2.2000D-11, 3.0000D-11, 1.4000D-12, 9.7000D+14, 3.6000D-11, & 
     &     3.0000D-11, 2.1990D+03, 4.7000D-12, 4.8000D+15, 1.0000D+03, & 
     &     3.2000D+03, 2.6000D-11, 0.0000D+00, 1.2000D-11, 5.4000D+16, & 
     &     1.2000D-11, 8.3000D+16, 8.8000D-12, 1.6000D-12, 1.0000D-10, & 
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
     &     0.0000D+00,-1.3940D+04, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00/

      DATA ( RFDAT( 4,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     6.0000D-01, 6.0000D-01, 6.0000D-01, 4.5000D-01, 6.0000D-01, & 
     &     4.1000D-01, 0.0000D+00, 6.0000D-01, 6.0000D-01, 0.0000D+00, & 
     &     0.0000D+00, 6.0000D-01, 0.0000D+00, 3.0000D-01, 3.0000D-01, & 
     &     3.0000D-01, 3.6000D-01, 6.0000D-01, 6.0000D-01, 6.0000D-01, & 
     &     0.0000D+00/

      DATA ( RFDAT( 5,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     1.2400D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 1.0000D+00, 0.0000D+00, 1.4100D+00, 1.4100D+00, & 
     &     1.4100D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
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
     &        0.61000,    1.00000,    1.00000,    1.00000,    0.50000, & ! 5   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    1.37000,    0.70000,    1.00000,    1.00000, & ! +   
     &        1.00000,    2.00000,    1.00000,    1.00000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    0.60000,    0.41000,    0.90000, & ! +   
     &        0.90000,    2.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.60000,    1.00000,    0.41000, & ! O   
     &        0.90000,    0.90000,    2.00000,    1.00000,    0.87000, & ! +   
     &        0.96000,    1.00000,    1.00000,    0.20000,    0.80000, & ! 1   
     &        0.18000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.24000,    1.30000,    0.65000,    1.18000, & ! 2   
     &        0.28000,    0.86000,    0.00000,    0.06000,    0.30000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    2.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.60000,    0.03000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    0.86000,    0.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.70000,    0.70000,    1.00000,    1.00000, & ! 5   
     &        0.75000,    0.91200,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.90000, & ! 6   
     &        0.86000,    0.25000,    0.65000,    0.20000,    1.56500, & ! +   
     &        0.11400,    0.35700,    0.33300,    0.15000,    0.75000, & ! 7   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.90000, & ! +   
     &        0.57000,    0.47000,    1.00000,    1.00000,    0.99100, & ! 8   
     &        0.20000,    2.00000,    1.00000,    1.00000,    0.30000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.30000, & ! +   
     &        0.15000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        2.00000,    0.15000,    1.90000,    1.65000,    1.39000, & ! +   
     &        0.45000,    2.00000,    1.00000,    0.00000,    1.00000, & ! 3   
     &        1.00000,    1.12000,    1.25000,    1.00000,    0.85000, & ! +   
     &        1.00000,    0.08000,    0.08000,    1.00000,    1.40000, & ! 4   
     &        1.00000,    0.76000,    0.76000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.50000,    0.00000,    1.00000,    1.14280, & ! 6   
     &        1.14280,    1.00000,    1.00000,    0.85714,    0.85714, & ! +   
     &        1.00000,    1.00000,    0.50000,    0.50000,    1.50000, & ! 7   
     &        1.42860,    1.42860,    1.71430,    1.71430,    1.25000, & ! +   
     &        1.00000,    1.25000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    0.50000,    1.00000,    1.00000,    0.50000, & ! +   
     &        0.50000/           !        3   

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
     &        0.61000,    1.00000,    1.00000,    0.00000,    0.50000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.50000,    1.00000,    0.00000,    0.00000,    1.00000, & ! 6   
     &        0.00000,    0.74000,    0.30000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! 7   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        0.00000,    1.00000,    0.60000,    0.15000,    0.90000, & ! +   
     &        0.10000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 9   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.60000,    1.00000,    0.15000, & ! O   
     &        0.90000,    0.10000,    2.00000,    1.00000,    0.13000, & ! +   
     &        0.60000,    0.00000,    0.00000,    0.30000,    0.33000, & ! 1   
     &        0.74000,    1.00000,    1.70000,    1.56000,    0.63000, & ! +   
     &        1.00000,    0.66000,    0.70000,    0.35000,    0.64000, & ! 2   
     &        0.10000,    1.20000,    0.00000,    0.12000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 3   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.40000,    0.62000,    1.00000,    0.00000, & ! 4   
     &        1.00000,    1.20000,    0.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.50000,    0.50000,    1.00000,    1.00000, & ! 5   
     &        0.50000,    0.62900,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.10000, & ! 6   
     &        0.86000,    0.14000,    0.60000,    0.80000,    0.16700, & ! +   
     &        0.15000,    0.28200,    0.06700,    5.12000,    1.25000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.10000, & ! +   
     &        0.07000,    0.28000,    1.00000,    0.95000,    0.99100, & ! 8   
     &        0.80000,    0.00000,    1.00000,    0.00000,    1.40000, & ! +   
     &        1.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! 9   
     &        0.87000,    0.99100,    2.00000,    0.33000,    0.70000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    0.00000,    0.88000,    0.84000,    0.84000, & ! +   
     &        0.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.47000, & ! 2   
     &        2.00000,    0.44000,    0.90000,    1.00000,    0.31000, & ! +   
     &        0.22000,   -1.00000,    0.00000,    0.00000,    0.90000, & ! 3   
     &        0.80000,    0.82000,    0.40000,    0.00000,    1.15000, & ! +   
     &        0.00000,    0.92000,    0.92000,    0.00000,    0.60000, & ! 4   
     &        0.00000,    0.24000,    0.24000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.50000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.58000, & ! 9   
     &        0.52000,    0.04500,    0.58000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.50000,    1.00000,    1.00000,    0.50000, & ! +   
     &        0.50000/           !        3   

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
     &        0.50000,    0.50000,    0.00000,    0.00000,    1.00000, & ! 6   
     &        0.00000,    0.63000,    0.30000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! 7   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.40000,    0.44000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.40000,    0.00000,    0.44000, & ! O   
     &        1.00000,    0.00000,    2.00000,    1.00000,    0.11000, & ! +   
     &        0.94000,    0.00000,    0.00000,    0.30000,    0.62000, & ! 1   
     &        0.32000,    0.91000,    1.00000,    0.22000,    0.13000, & ! +   
     &        2.00000,    0.10000,    1.00000,    0.25000,    1.00000, & ! 2   
     &        0.18000,    0.86000,    0.00000,    1.12000,    0.60000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.70000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.34400,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.20000,    0.20000,    0.00000,    1.00000, & ! 5   
     &        0.25000,    0.99100,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.04000,    0.65000,    0.20000,    1.00000,    0.71300, & ! +   
     &        0.85000,    1.28200,    0.90000,    1.00000,    0.25000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.76000,    1.03000,    1.00000,    0.01000,    0.00900, & ! 8   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 9   
     &        0.13000,    0.99100,    1.00000,    0.67000,    0.45000, & ! +   
     &        1.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! O   
     &        1.00000,    0.00000,    0.88000,    0.84000,    0.84000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        1.00000,    0.29000,    0.30000,    0.35000,    0.15000, & ! +   
     &        0.53000,    0.00000,    0.00000,    0.00000,    0.10000, & ! 3   
     &        0.20000,    0.08000,    0.14000,    0.00000,    0.68000, & ! +   
     &        0.00000,    0.52000,    0.52000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.80000,    0.60000,    1.00000,    0.33000, & ! +   
     &        0.33000,    0.33000,    0.33000,    0.33000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        1.00000/           !        3   

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
     &        0.00000,    0.50000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.40000,    0.44000,    0.10000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.40000,    0.00000,    0.44000, & ! O   
     &        0.10000,    0.00000,    0.00000,    1.00000,    0.06000, & ! +   
     &       -2.10000,    0.00000,    0.00000,    0.20000,    0.80000, & ! 1   
     &        0.22000,    0.09000,    0.70000,    1.00000,    0.13000, & ! +   
     &        0.00000,    0.10000,    1.00000,    0.25000,    1.00000, & ! 2   
     &        0.65000,    0.14000,    0.00000,    0.13000,    0.36000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.03000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.34400,    0.00000,    1.00000,    0.00000, & ! +   
     &        0.00000,    0.80000,    0.80000,    0.00000,    0.00000, & ! 5   
     &        0.25000,    0.91200,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.10000,    0.87000,    0.06600,    0.80000,    0.50300, & ! +   
     &        0.15400,    0.92500,    0.83200,    0.00000,    0.28000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.18000,    0.25000,    0.00000,    0.08000,    1.00000, & ! 8   
     &        0.80000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.11000,    0.00900,    1.00000,    2.00000,    0.55000, & ! +   
     &        0.85000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.12000,    0.16000,    0.16000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.29000,    0.60000,    0.65000,    0.41000, & ! +   
     &        0.24000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.08000,    0.46000,    0.00000,    0.55000, & ! +   
     &        0.00000,    0.05000,    0.05000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.20000,    0.40000,    1.00000,    0.33000, & ! +   
     &        0.33000,    0.33000,    0.33000,    0.33000,    0.00000, & ! 5   
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
     &        1.00000/           !        3   

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.15000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.40000,    0.00000,    0.15000, & ! O   
     &        0.10000,    0.00000,    0.00000,    0.00000,   -0.11000, & ! +   
     &        0.04000,    0.00000,    0.00000,    0.20000,    0.95000, & ! 1   
     &        0.10000,    0.56000,    0.30000,    0.00000,    0.37000, & ! +   
     &        0.00000,    0.10000,    0.00000,    0.50000,    0.00000, & ! 2   
     &        0.07200,    0.52000,    0.00000,    0.73200,    0.48000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.69000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.14000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.10000,    1.10000,    0.00000,    0.00000, & ! 5   
     &        0.25000,    0.08800,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.02700,    0.14000,    0.26600,    0.20000,    0.33400, & ! +   
     &        0.26800,    0.64300,    0.70000,    0.00000,    1.66000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.24000,    0.47000,    0.00000,    0.05000,    0.00000, & ! 8   
     &        0.20000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.06000,    1.00000,    0.00000,    1.00000,    0.30000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.44000,    0.10000,    0.00000,    0.13000, & ! +   
     &        0.01000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.02000,    0.00400,    0.00000,    0.15000, & ! +   
     &        0.00000,    1.25000,    1.05000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.33000, & ! +   
     &        0.33000,    0.33000,    0.33000,    0.33000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.40000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.76000, & ! +   
     &        0.02000,    0.00000,    0.00000,    0.20000,   -0.70000, & ! 1   
     &        0.33000,    0.35000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.10000,    0.00000,    0.50000,    0.00000, & ! 2   
     &        1.00000,    0.33600,    0.00000,    0.06000,    0.24000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.08000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.30000,    0.30000,    0.00000,    0.00000, & ! 5   
     &        0.25000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.03000,    0.48000,    0.20000,    0.80000,    0.16800, & ! +   
     &        0.06400,    0.85000,    1.03300,    0.00000,    0.47000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00100,    0.53000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.80000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &       -0.11000,    0.00000,    0.00000,   -1.00000,    0.30000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.15000,    0.00000,    0.00000,    0.31000, & ! +   
     &        0.22000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.02000, & ! +   
     &        0.00000,    0.20000,    0.40000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,   -0.66000, & ! +   
     &       -0.66000,   -0.66000,   -0.66000,   -0.66000,    0.00000, & ! 5   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.05000, & ! +   
     &        0.50000,    0.00000,    0.00000,    0.01000,    0.00000, & ! 1   
     &        0.44000,   -1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.50000,    0.00000, & ! 2   
     &        0.00000,    0.33600,    0.00000,    0.06000,    0.24000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.76000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.13000,    0.15000,    1.00000,    0.25200, & ! +   
     &        0.02000,    0.07500,    0.70000,    0.00000,    1.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        7.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        2.40000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.76000,    0.00000,    0.00000,    0.00000,    1.70000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.15000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    3.00000, & ! +   
     &        0.00000,    0.11000,    0.11000,    0.00000,    0.00000, & ! 4   
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
     &        0.00000,    0.00000,    0.00000,    0.20000,    0.00000, & ! 1   
     &       -1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.06000,    0.12000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.20000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    2.83000,    0.35000,    2.40000,    0.33000, & ! +   
     &        0.36000,    0.07500,    0.26700,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.21000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.05000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.41000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.81000,    1.81000,    0.00000,    0.00000, & ! 4   
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
     &        0.00000,    0.00000,    0.00000,    0.10000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.10000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.06600,    0.00000,    0.13000, & ! +   
     &        0.22500,    0.15000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.39000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.24000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.12000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
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
     &        0.00000/           !        3   

      INTEGER            :: NREACT( NRXNS )

      DATA ( NREACT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    2,    2,    2,    2,    2,    1,    1,    1, & ! O   
     &      1,    2,    2,    1,    1,    2,    2,    2,    1,    1, & ! 1   
     &      1,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 2   
     &      2,    1,    2,    2,    2,    1,    2,    1,    1,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      1,    1,    1,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    1,    2,    1,    2,    2,    2,    2,    1,    2, & ! 6   
     &      2,    1,    1,    2,    2,    2,    1,    2,    2,    2, & ! 7   
     &      2,    2,    2,    1,    2,    2,    1,    1,    2,    2, & ! 8   
     &      2,    2,    2,    1,    2,    2,    2,    2,    1,    2, & ! 9   
     &      2,    1,    1,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      1,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    2, & ! 3   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      1,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 6   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    1,    1,    2,    2, & ! 8   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 3   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    2,    2, & ! 5   
     &      2,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    2, & ! 7   
     &      2,    2,    2,    2,    2,    2,    1,    1,    2,    2, & ! 8   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    1,    2,    2,    2,    2,    1,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 2   
     &      2/     !  3   
      INTEGER            :: NPRDCT( NRXNS )

      DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    1,    1,    2,    1,    1,    2,    1,    1,    1, & ! 1   
     &      2,    1,    1,    1,    2,    1,    2,    1,    1,    2, & ! 2   
     &      1,    2,    1,    1,    1,    1,    1,    2,    1,    1, & ! 3   
     &      1,    1,    0,    1,    2,    1,    2,    1,    1,    1, & ! 4   
     &      4,    2,    2,    1,    2,    1,    1,    0,    0,    0, & ! 5   
     &      3,    4,    1,    1,    3,    1,    3,    3,    3,    2, & ! 6   
     &      2,    2,    1,    3,    3,    1,    2,    3,    1,    1, & ! 7   
     &      2,    1,    2,    3,    2,    1,    2,    4,    5,    4, & ! 8   
     &      2,    1,    1,    2,    1,    2,    1,    2,    3,    4, & ! 9   
     &      1,    2,    6,    2,    5,    5,    2,    3,    4,    7, & ! O   
     &      7,    1,    1,    9,    6,    8,    7,    5,    4,    5, & ! 1   
     &      3,    6,    4,    7,    4,    6,    7,    0,    8,   10, & ! 2   
     &      1,    1,    1,    2,    1,    1,    2,    1,    2,    1, & ! 3   
     &      3,    2,    8,    2,    1,    2,    5,    0,    4,    1, & ! 4   
     &      2,    7,    7,    2,    3,    6,    6,    1,    1,    1, & ! 5   
     &      1,    0,    0,    0,    2,    6,    8,    9,    8,   10, & ! 6   
     &      9,    9,    8,    3,    7,    1,    1,    0,    0,    2, & ! 7   
     &     10,    7,    3,    5,    4,    7,    1,    2,    1,    2, & ! 8   
     &      2,    1,    2,    3,    2,    8,    5,    4,    6,    8, & ! 9   
     &      5,    3,    2,    2,    3,    3,    1,    4,    4,    4, & ! O   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    3,    8,    5,    4,    6, & ! 2   
     &      6,    2,    1,    0,    3,    3,    5,    5,    1,    7, & ! 3   
     &      1,    8,    8,    1,    2,    1,    4,    4,    4,    6, & ! 4   
     &      6,    6,    6,    6,    2,    2,    1,    1,    1,    1, & ! 5   
     &      1,    2,    0,    1,    1,    1,    1,    1,    1,    1, & ! 6   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    3, & ! 7   
     &      1,    3,    1,    1,    1,    1,    0,    0,    1,    1, & ! 8   
     &      1,    1,    0,    1,    2,    2,    2,    2,    1,    1, & ! 9   
     &      1,    0,    1,    1,    1,    1,    0,    1,    1,    1, & ! O   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    1,    1,    1,    1,    1,    3,    2,    2,    3, & ! 2   
     &      3/     !  3   

      INTEGER, PARAMETER :: NMPHOT =  38
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    8,    9,   14,   15,   25,   36,   51,   52,   53, & 
     &     62,   69,   72,   73,   84,   88,   94,   99,  103,  139, & 
     &    141,  155,  173,  187,  188,  194,  212,  249,  250,  251, & 
     &    252,  253,  254,  287,  288,  293,  302,  307/

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   11,   12,   13,   14,   15,   16,   17,   15,    1, & 
     &      1,   18,   19,   20,   21,   22,   23,   24,   25,   24, & 
     &     24,   26,   26,   12,   13,   14,   19,   19/

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & 
     &     31,   32,   33,   34,   35,   36,   37,   38/

      INTEGER, PARAMETER :: MHETERO =  11
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    255,  256,  257,  258,  259,  260,  261,  262,  264,  281, & 
     &    283/

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
     &    'R63             ', 'R64             ', 'R65             ', & ! 0   
     &    'R66             ', 'R67             ', 'R68             ', & ! 1   
     &    'R69             ', 'R70             ', 'R71             ', & ! 2   
     &    'R72             ', 'R73             ', 'R74             ', & ! 3   
     &    'R75             ', 'R76             ', 'R77             ', & ! 4   
     &    'R78             ', 'R79             ', 'R80             ', & ! 5   
     &    'R81             ', 'R82             ', 'R83             ', & ! 6   
     &    'R84             ', 'R85             ', 'R86             ', & ! 7   
     &    'R87             ', 'R88             ', 'R89             ', & ! 8   
     &    'R90             ', 'R91             ', 'R92             ', & ! 9   
     &    'R93             ', 'R94             ', 'R95             ', & ! 0   
     &    'R96             ', 'R97             ', 'R98             ', & ! 1   
     &    'R99             ', 'R100            ', 'R101            ', & ! 2   
     &    'R102            ', 'R103            ', 'R104            ', & ! 3   
     &    'R105            ', 'R106            ', 'R107            ', & ! 4   
     &    'R108            ', 'R109            ', 'R110            ', & ! 5   
     &    'R111            ', 'R112            ', 'R113            ', & ! 6   
     &    'R114            ', 'R115            ', 'R116            ', & ! 7   
     &    'R117            ', 'R118            ', 'R119            ', & ! 8   
     &    'R120            ', 'R121            ', 'R122            ', & ! 9   
     &    'R123            ', 'R124            ', 'R125            ', & ! 0   
     &    'R126            ', 'R127            ', 'R128            ', & ! 1   
     &    'R129            ', 'R130            ', 'R131            ', & ! 2   
     &    'R132            ', 'R133            ', 'R134            ', & ! 3   
     &    'R135            ', 'R136            ', 'R137            ', & ! 4   
     &    'R138            ', 'R139            ', 'R140            ', & ! 5   
     &    'R141            ', 'R142            ', 'R143            ', & ! 6   
     &    'R144            ', 'R145            ', 'R146            ', & ! 7   
     &    'R147            ', 'R148            ', 'R149            ', & ! 8   
     &    'R150            ', 'R151            ', 'R152            ', & ! 9   
     &    'R153            ', 'R154a           ', 'R154b           ', & ! 0   
     &    'R155            ', 'R156            ', 'R157            ', & ! 1   
     &    'R158            ', 'I001            ', 'I002            ', & ! 2   
     &    'I003            ', 'I004            ', 'I005            ', & ! 3   
     &    'I006            ', 'I007            ', 'I008            ', & ! 4   
     &    'I009            ', 'I010            ', 'R159            ', & ! 5   
     &    'R160            ', 'R161            ', 'R162            ', & ! 6   
     &    'R163            ', 'R164            ', 'R165            ', & ! 7   
     &    'R166            ', 'I014            ', 'I015            ', & ! 8   
     &    'I016            ', 'I017            ', 'I018            ', & ! 9   
     &    'R167            ', 'R168            ', 'R169            ', & ! 0   
     &    'R170            ', 'R171            ', 'R172            ', & ! 1   
     &    'CL1             ', 'CL2             ', 'CL3             ', & ! 2   
     &    'CL4             ', 'CL5             ', 'CL6             ', & ! 3   
     &    'CL7             ', 'CL8             ', 'CL9             ', & ! 4   
     &    'CL10            ', 'CL11            ', 'CL12            ', & ! 5   
     &    'CL13            ', 'CL14            ', 'CL15            ', & ! 6   
     &    'CL16            ', 'CL17            ', 'CL18            ', & ! 7   
     &    'CL19            ', 'CL20            ', 'CL21            ', & ! 8   
     &    'CL22            ', 'CL23a           ', 'CL23b           ', & ! 9   
     &    'CL24            ', 'CL25            ', 'SA01            ', & ! 0   
     &    'SA02            ', 'SA03            ', 'SA04            ', & ! 1   
     &    'SA05            ', 'SA06            ', 'SA07            ', & ! 2   
     &    'SA08            ', 'SA09            ', 'SA10            ', & ! 3   
     &    'SA11            ', 'SA12            ', 'SA13            ', & ! 4   
     &    'N001            ', 'N002            ', 'N003            ', & ! 5   
     &    'N004            ', 'N005            ', 'N006            ', & ! 6   
     &    'N007            ', 'N008            ', 'N009            ', & ! 7   
     &    'R010            ', 'N011            ', 'N012            ', & ! 8   
     &    'N013            ', 'N014            ', 'N015            ', & ! 9   
     &    'N016            ', 'N017            ', 'N018            ', & ! 0   
     &    'N019            ', 'N020            ', 'N021            ', & ! 1   
     &    'N022            ', 'N023            ', 'N024            ', & ! 2   
     &    'N025            ', 'N026            ', 'N027            ', & ! 3   
     &    'N028            ', 'N029            ', 'HET_N2O5IJ      ', & ! 4   
     &    'HET_N2O5K       ', 'HET_H2NO3PIJA   ', 'HET_H2NO3PKA    ', & ! 5   
     &    'HET_H2NO3PIB    ', 'HET_H2NO3PJB    ', 'HET_H2NO3PKB    ', & ! 6   
     &    'HET_N02         ', 'HAL_Ozone       ', 'HET_IEPOX       ', & ! 7   
     &    'OLIG_XYLENE1    ', 'OLIG_XYLENE2    ', 'OLIG_TOLUENE1   ', & ! 8   
     &    'OLIG_TOLUENE2   ', 'OLIG_BENZENE1   ', 'OLIG_BENZENE2   ', & ! 9   
     &    'OLIG_TERPENE1   ', 'OLIG_TERPENE2   ', 'OLIG_ISOPRENE1  ', & ! 0   
     &    'OLIG_ISOPRENE2  ', 'OLIG_SESQT1     ', 'OLIG_PAH1       ', & ! 1   
     &    'OLIG_PAH2       ', 'OLIG_ALK1       ', 'OLIG_ALK2       ', & ! 2   
     &    'RPOAGEPI        ', 'RPOAGELI        ', 'RPOAGEPJ        ', & ! 3   
     &    'RPOAGELJ        ', 'T01             ', 'T02             ', & ! 4   
     &    'T03             ', 'T04             ', 'T05             ', & ! 5   
     &    'TCL1            ', 'T06             ', 'T07             ', & ! 6   
     &    'T08             ', 'T09             ', 'TCL2            ', & ! 7   
     &    'T10             ', 'T11             ', 'T12             ', & ! 8   
     &    'TCL3            ', 'T14             ', 'T15             ', & ! 9   
     &    'T16             ', 'T17             ', 'TCL4            ', & ! 0   
     &    'T18             ', 'T19             ', 'T20             ', & ! 1   
     &    'T21             ', 'TCL5            ', 'T22             ', & ! 2   
     &    'TCL6            ', 'T23             ', 'TCL7            ', & ! 3   
     &    'T24             ', 'TCL8            ', 'T25             ', & ! 4   
     &    'TCL9            ', 'T26             ', 'T27             ', & ! 5   
     &    'T28             ', 'T29             ', 'TCL10           ', & ! 6   
     &    'T30             ', 'T31             ', 'T32             ', & ! 7   
     &    'T33             ', 'TCL11           ', 'HG1             ', & ! 8   
     &    'HG2             ', 'HG3             ', 'HG4             ', & ! 9   
     &    'HG5             '/                   !                 0  

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
