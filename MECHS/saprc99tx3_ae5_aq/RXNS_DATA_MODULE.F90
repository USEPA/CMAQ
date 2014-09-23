       MODULE RXNS_DATA


       IMPLICIT NONE



! --------- Photochemical Mechanism Reactions, Rates, etc. DAT ---------
! Source file: /home/hutzellb/CCTM_git_repository/MECHS/saprc99tx3_ae5_aq/mech-saprc99tx3_ae5_aq.def
! for Mechanism Name: SAPRC99TX3_AE5_AQ               

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

      CHARACTER( 32 ), PARAMETER :: MECHNAME = 'SAPRC99TX3_AE5_AQ'

      INTEGER, PARAMETER :: N_GAS_CHEM_SPC =  94
      INTEGER, PARAMETER :: NUMB_MECH_SPC  = 108

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
      DATA GAS_CHEM_SPC(   8 ) / 'O1D2            ' /
      DATA GAS_CHEM_SPC(   9 ) / 'HO              ' /
      DATA GAS_CHEM_SPC(  10 ) / 'HONO            ' /
      DATA GAS_CHEM_SPC(  11 ) / 'HO2             ' /
      DATA GAS_CHEM_SPC(  12 ) / 'CO              ' /
      DATA GAS_CHEM_SPC(  13 ) / 'HNO4            ' /
      DATA GAS_CHEM_SPC(  14 ) / 'HO2H            ' /
      DATA GAS_CHEM_SPC(  15 ) / 'SO2             ' /
      DATA GAS_CHEM_SPC(  16 ) / 'SULF            ' /
      DATA GAS_CHEM_SPC(  17 ) / 'SULRXN          ' /
      DATA GAS_CHEM_SPC(  18 ) / 'C_O2            ' /
      DATA GAS_CHEM_SPC(  19 ) / 'HCHO            ' /
      DATA GAS_CHEM_SPC(  20 ) / 'COOH            ' /
      DATA GAS_CHEM_SPC(  21 ) / 'MEOH            ' /
      DATA GAS_CHEM_SPC(  22 ) / 'RO2_R           ' /
      DATA GAS_CHEM_SPC(  23 ) / 'ROOH            ' /
      DATA GAS_CHEM_SPC(  24 ) / 'R2O2            ' /
      DATA GAS_CHEM_SPC(  25 ) / 'RO2_N           ' /
      DATA GAS_CHEM_SPC(  26 ) / 'RNO3            ' /
      DATA GAS_CHEM_SPC(  27 ) / 'MEK             ' /
      DATA GAS_CHEM_SPC(  28 ) / 'PROD2           ' /
      DATA GAS_CHEM_SPC(  29 ) / 'CCO_O2          ' /
      DATA GAS_CHEM_SPC(  30 ) / 'PAN             ' /
      DATA GAS_CHEM_SPC(  31 ) / 'CCO_OOH         ' /
      DATA GAS_CHEM_SPC(  32 ) / 'CCO_OH          ' /
      DATA GAS_CHEM_SPC(  33 ) / 'RCO_O2          ' /
      DATA GAS_CHEM_SPC(  34 ) / 'PAN2            ' /
      DATA GAS_CHEM_SPC(  35 ) / 'CCHO            ' /
      DATA GAS_CHEM_SPC(  36 ) / 'RCO_OOH         ' /
      DATA GAS_CHEM_SPC(  37 ) / 'RCO_OH          ' /
      DATA GAS_CHEM_SPC(  38 ) / 'BZCO_O2         ' /
      DATA GAS_CHEM_SPC(  39 ) / 'PBZN            ' /
      DATA GAS_CHEM_SPC(  40 ) / 'BZ_O            ' /
      DATA GAS_CHEM_SPC(  41 ) / 'MA_RCO3         ' /
      DATA GAS_CHEM_SPC(  42 ) / 'MA_PAN          ' /
      DATA GAS_CHEM_SPC(  43 ) / 'TBU_O           ' /
      DATA GAS_CHEM_SPC(  44 ) / 'ACET            ' /
      DATA GAS_CHEM_SPC(  45 ) / 'NPHE            ' /
      DATA GAS_CHEM_SPC(  46 ) / 'PHEN            ' /
      DATA GAS_CHEM_SPC(  47 ) / 'BZNO2_O         ' /
      DATA GAS_CHEM_SPC(  48 ) / 'HOCOO           ' /
      DATA GAS_CHEM_SPC(  49 ) / 'HCOOH           ' /
      DATA GAS_CHEM_SPC(  50 ) / 'RCHO            ' /
      DATA GAS_CHEM_SPC(  51 ) / 'GLY             ' /
      DATA GAS_CHEM_SPC(  52 ) / 'MGLY            ' /
      DATA GAS_CHEM_SPC(  53 ) / 'BACL            ' /
      DATA GAS_CHEM_SPC(  54 ) / 'CRES            ' /
      DATA GAS_CHEM_SPC(  55 ) / 'BALD            ' /
      DATA GAS_CHEM_SPC(  56 ) / 'METHACRO        ' /
      DATA GAS_CHEM_SPC(  57 ) / 'MVK             ' /
      DATA GAS_CHEM_SPC(  58 ) / 'ISOPROD         ' /
      DATA GAS_CHEM_SPC(  59 ) / 'CCHO_SUR        ' /
      DATA GAS_CHEM_SPC(  60 ) / 'DCB1            ' /
      DATA GAS_CHEM_SPC(  61 ) / 'DCB2            ' /
      DATA GAS_CHEM_SPC(  62 ) / 'DCB3            ' /
      DATA GAS_CHEM_SPC(  63 ) / 'ETHENE          ' /
      DATA GAS_CHEM_SPC(  64 ) / 'ISOPRENE        ' /
      DATA GAS_CHEM_SPC(  65 ) / 'ISOPRXN         ' /
      DATA GAS_CHEM_SPC(  66 ) / 'TRP1            ' /
      DATA GAS_CHEM_SPC(  67 ) / 'TRPRXN          ' /
      DATA GAS_CHEM_SPC(  68 ) / 'ALK1            ' /
      DATA GAS_CHEM_SPC(  69 ) / 'ALK2            ' /
      DATA GAS_CHEM_SPC(  70 ) / 'ALK3            ' /
      DATA GAS_CHEM_SPC(  71 ) / 'ALK4            ' /
      DATA GAS_CHEM_SPC(  72 ) / 'ALK5            ' /
      DATA GAS_CHEM_SPC(  73 ) / 'ALK5RXN         ' /
      DATA GAS_CHEM_SPC(  74 ) / 'ARO1            ' /
      DATA GAS_CHEM_SPC(  75 ) / 'ARO1RO2         ' /
      DATA GAS_CHEM_SPC(  76 ) / 'TOLNRXN         ' /
      DATA GAS_CHEM_SPC(  77 ) / 'TOLHRXN         ' /
      DATA GAS_CHEM_SPC(  78 ) / 'ARO2            ' /
      DATA GAS_CHEM_SPC(  79 ) / 'ARO2RO2         ' /
      DATA GAS_CHEM_SPC(  80 ) / 'XYLNRXN         ' /
      DATA GAS_CHEM_SPC(  81 ) / 'XYLHRXN         ' /
      DATA GAS_CHEM_SPC(  82 ) / 'OLE1            ' /
      DATA GAS_CHEM_SPC(  83 ) / 'OLE2            ' /
      DATA GAS_CHEM_SPC(  84 ) / 'SESQ            ' /
      DATA GAS_CHEM_SPC(  85 ) / 'SESQRXN         ' /
      DATA GAS_CHEM_SPC(  86 ) / 'BENZENE         ' /
      DATA GAS_CHEM_SPC(  87 ) / 'BENZRO2         ' /
      DATA GAS_CHEM_SPC(  88 ) / 'BNZNRXN         ' /
      DATA GAS_CHEM_SPC(  89 ) / 'BNZHRXN         ' /
      DATA GAS_CHEM_SPC(  90 ) / 'ACROLEIN        ' /
      DATA GAS_CHEM_SPC(  91 ) / 'BUTADIENE13     ' /
      DATA GAS_CHEM_SPC(  92 ) / 'FORM_PRIMARY    ' /
      DATA GAS_CHEM_SPC(  93 ) / 'ALD2_PRIMARY    ' /
      DATA GAS_CHEM_SPC(  94 ) / 'ACROLEIN_PRIMARY' /




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
      DATA CHEMISTRY_SPC(   8 ), CGRID_INDEX(   8 ), SPECIES_TYPE(   8 ), SPECIES_MOLWT(   8 ), CONVERT_CONC(   8 ) / 'O1D2            ',    8, 'GC',   16.00, F /
      DATA CHEMISTRY_SPC(   9 ), CGRID_INDEX(   9 ), SPECIES_TYPE(   9 ), SPECIES_MOLWT(   9 ), CONVERT_CONC(   9 ) / 'HO              ',    9, 'GC',   17.00, F /
      DATA CHEMISTRY_SPC(  10 ), CGRID_INDEX(  10 ), SPECIES_TYPE(  10 ), SPECIES_MOLWT(  10 ), CONVERT_CONC(  10 ) / 'HONO            ',   10, 'GC',   47.00, F /
      DATA CHEMISTRY_SPC(  11 ), CGRID_INDEX(  11 ), SPECIES_TYPE(  11 ), SPECIES_MOLWT(  11 ), CONVERT_CONC(  11 ) / 'HO2             ',   11, 'GC',   33.00, F /
      DATA CHEMISTRY_SPC(  12 ), CGRID_INDEX(  12 ), SPECIES_TYPE(  12 ), SPECIES_MOLWT(  12 ), CONVERT_CONC(  12 ) / 'CO              ',   12, 'GC',   28.00, F /
      DATA CHEMISTRY_SPC(  13 ), CGRID_INDEX(  13 ), SPECIES_TYPE(  13 ), SPECIES_MOLWT(  13 ), CONVERT_CONC(  13 ) / 'HNO4            ',   13, 'GC',   79.00, F /
      DATA CHEMISTRY_SPC(  14 ), CGRID_INDEX(  14 ), SPECIES_TYPE(  14 ), SPECIES_MOLWT(  14 ), CONVERT_CONC(  14 ) / 'HO2H            ',   14, 'GC',   34.00, F /
      DATA CHEMISTRY_SPC(  15 ), CGRID_INDEX(  15 ), SPECIES_TYPE(  15 ), SPECIES_MOLWT(  15 ), CONVERT_CONC(  15 ) / 'SO2             ',   15, 'GC',   64.00, F /
      DATA CHEMISTRY_SPC(  16 ), CGRID_INDEX(  16 ), SPECIES_TYPE(  16 ), SPECIES_MOLWT(  16 ), CONVERT_CONC(  16 ) / 'SULF            ',   16, 'GC',   98.00, F /
      DATA CHEMISTRY_SPC(  17 ), CGRID_INDEX(  17 ), SPECIES_TYPE(  17 ), SPECIES_MOLWT(  17 ), CONVERT_CONC(  17 ) / 'SULRXN          ',   17, 'GC',   98.00, F /
      DATA CHEMISTRY_SPC(  18 ), CGRID_INDEX(  18 ), SPECIES_TYPE(  18 ), SPECIES_MOLWT(  18 ), CONVERT_CONC(  18 ) / 'C_O2            ',   18, 'GC',   47.00, F /
      DATA CHEMISTRY_SPC(  19 ), CGRID_INDEX(  19 ), SPECIES_TYPE(  19 ), SPECIES_MOLWT(  19 ), CONVERT_CONC(  19 ) / 'HCHO            ',   19, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC(  20 ), CGRID_INDEX(  20 ), SPECIES_TYPE(  20 ), SPECIES_MOLWT(  20 ), CONVERT_CONC(  20 ) / 'COOH            ',   20, 'GC',   48.00, F /
      DATA CHEMISTRY_SPC(  21 ), CGRID_INDEX(  21 ), SPECIES_TYPE(  21 ), SPECIES_MOLWT(  21 ), CONVERT_CONC(  21 ) / 'MEOH            ',   21, 'GC',   32.00, F /
      DATA CHEMISTRY_SPC(  22 ), CGRID_INDEX(  22 ), SPECIES_TYPE(  22 ), SPECIES_MOLWT(  22 ), CONVERT_CONC(  22 ) / 'RO2_R           ',   22, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  23 ), CGRID_INDEX(  23 ), SPECIES_TYPE(  23 ), SPECIES_MOLWT(  23 ), CONVERT_CONC(  23 ) / 'ROOH            ',   23, 'GC',   62.00, F /
      DATA CHEMISTRY_SPC(  24 ), CGRID_INDEX(  24 ), SPECIES_TYPE(  24 ), SPECIES_MOLWT(  24 ), CONVERT_CONC(  24 ) / 'R2O2            ',   24, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  25 ), CGRID_INDEX(  25 ), SPECIES_TYPE(  25 ), SPECIES_MOLWT(  25 ), CONVERT_CONC(  25 ) / 'RO2_N           ',   25, 'GC',    1.00, F /
      DATA CHEMISTRY_SPC(  26 ), CGRID_INDEX(  26 ), SPECIES_TYPE(  26 ), SPECIES_MOLWT(  26 ), CONVERT_CONC(  26 ) / 'RNO3            ',   26, 'GC',  133.00, F /
      DATA CHEMISTRY_SPC(  27 ), CGRID_INDEX(  27 ), SPECIES_TYPE(  27 ), SPECIES_MOLWT(  27 ), CONVERT_CONC(  27 ) / 'MEK             ',   27, 'GC',   72.00, F /
      DATA CHEMISTRY_SPC(  28 ), CGRID_INDEX(  28 ), SPECIES_TYPE(  28 ), SPECIES_MOLWT(  28 ), CONVERT_CONC(  28 ) / 'PROD2           ',   28, 'GC',  100.00, F /
      DATA CHEMISTRY_SPC(  29 ), CGRID_INDEX(  29 ), SPECIES_TYPE(  29 ), SPECIES_MOLWT(  29 ), CONVERT_CONC(  29 ) / 'CCO_O2          ',   29, 'GC',   75.00, F /
      DATA CHEMISTRY_SPC(  30 ), CGRID_INDEX(  30 ), SPECIES_TYPE(  30 ), SPECIES_MOLWT(  30 ), CONVERT_CONC(  30 ) / 'PAN             ',   30, 'GC',  121.00, F /
      DATA CHEMISTRY_SPC(  31 ), CGRID_INDEX(  31 ), SPECIES_TYPE(  31 ), SPECIES_MOLWT(  31 ), CONVERT_CONC(  31 ) / 'CCO_OOH         ',   31, 'GC',   76.00, F /
      DATA CHEMISTRY_SPC(  32 ), CGRID_INDEX(  32 ), SPECIES_TYPE(  32 ), SPECIES_MOLWT(  32 ), CONVERT_CONC(  32 ) / 'CCO_OH          ',   32, 'GC',   60.00, F /
      DATA CHEMISTRY_SPC(  33 ), CGRID_INDEX(  33 ), SPECIES_TYPE(  33 ), SPECIES_MOLWT(  33 ), CONVERT_CONC(  33 ) / 'RCO_O2          ',   33, 'GC',   89.00, F /
      DATA CHEMISTRY_SPC(  34 ), CGRID_INDEX(  34 ), SPECIES_TYPE(  34 ), SPECIES_MOLWT(  34 ), CONVERT_CONC(  34 ) / 'PAN2            ',   34, 'GC',  135.00, F /
      DATA CHEMISTRY_SPC(  35 ), CGRID_INDEX(  35 ), SPECIES_TYPE(  35 ), SPECIES_MOLWT(  35 ), CONVERT_CONC(  35 ) / 'CCHO            ',   35, 'GC',   44.00, F /
      DATA CHEMISTRY_SPC(  36 ), CGRID_INDEX(  36 ), SPECIES_TYPE(  36 ), SPECIES_MOLWT(  36 ), CONVERT_CONC(  36 ) / 'RCO_OOH         ',   36, 'GC',   90.00, F /
      DATA CHEMISTRY_SPC(  37 ), CGRID_INDEX(  37 ), SPECIES_TYPE(  37 ), SPECIES_MOLWT(  37 ), CONVERT_CONC(  37 ) / 'RCO_OH          ',   37, 'GC',   74.00, F /
      DATA CHEMISTRY_SPC(  38 ), CGRID_INDEX(  38 ), SPECIES_TYPE(  38 ), SPECIES_MOLWT(  38 ), CONVERT_CONC(  38 ) / 'BZCO_O2         ',   38, 'GC',  137.00, F /
      DATA CHEMISTRY_SPC(  39 ), CGRID_INDEX(  39 ), SPECIES_TYPE(  39 ), SPECIES_MOLWT(  39 ), CONVERT_CONC(  39 ) / 'PBZN            ',   39, 'GC',  183.00, F /
      DATA CHEMISTRY_SPC(  40 ), CGRID_INDEX(  40 ), SPECIES_TYPE(  40 ), SPECIES_MOLWT(  40 ), CONVERT_CONC(  40 ) / 'BZ_O            ',   40, 'GC',   93.00, F /
      DATA CHEMISTRY_SPC(  41 ), CGRID_INDEX(  41 ), SPECIES_TYPE(  41 ), SPECIES_MOLWT(  41 ), CONVERT_CONC(  41 ) / 'MA_RCO3         ',   41, 'GC',  101.00, F /
      DATA CHEMISTRY_SPC(  42 ), CGRID_INDEX(  42 ), SPECIES_TYPE(  42 ), SPECIES_MOLWT(  42 ), CONVERT_CONC(  42 ) / 'MA_PAN          ',   42, 'GC',  147.00, F /
      DATA CHEMISTRY_SPC(  43 ), CGRID_INDEX(  43 ), SPECIES_TYPE(  43 ), SPECIES_MOLWT(  43 ), CONVERT_CONC(  43 ) / 'TBU_O           ',   43, 'GC',   73.00, F /
      DATA CHEMISTRY_SPC(  44 ), CGRID_INDEX(  44 ), SPECIES_TYPE(  44 ), SPECIES_MOLWT(  44 ), CONVERT_CONC(  44 ) / 'ACET            ',   44, 'GC',   58.00, F /
      DATA CHEMISTRY_SPC(  45 ), CGRID_INDEX(  45 ), SPECIES_TYPE(  45 ), SPECIES_MOLWT(  45 ), CONVERT_CONC(  45 ) / 'NPHE            ',   45, 'GC',  139.00, F /
      DATA CHEMISTRY_SPC(  46 ), CGRID_INDEX(  46 ), SPECIES_TYPE(  46 ), SPECIES_MOLWT(  46 ), CONVERT_CONC(  46 ) / 'PHEN            ',   46, 'GC',   94.00, F /
      DATA CHEMISTRY_SPC(  47 ), CGRID_INDEX(  47 ), SPECIES_TYPE(  47 ), SPECIES_MOLWT(  47 ), CONVERT_CONC(  47 ) / 'BZNO2_O         ',   47, 'GC',  139.00, F /
      DATA CHEMISTRY_SPC(  48 ), CGRID_INDEX(  48 ), SPECIES_TYPE(  48 ), SPECIES_MOLWT(  48 ), CONVERT_CONC(  48 ) / 'HOCOO           ',   48, 'GC',   63.00, F /
      DATA CHEMISTRY_SPC(  49 ), CGRID_INDEX(  49 ), SPECIES_TYPE(  49 ), SPECIES_MOLWT(  49 ), CONVERT_CONC(  49 ) / 'HCOOH           ',   49, 'GC',   46.00, F /
      DATA CHEMISTRY_SPC(  50 ), CGRID_INDEX(  50 ), SPECIES_TYPE(  50 ), SPECIES_MOLWT(  50 ), CONVERT_CONC(  50 ) / 'RCHO            ',   50, 'GC',   58.00, F /
      DATA CHEMISTRY_SPC(  51 ), CGRID_INDEX(  51 ), SPECIES_TYPE(  51 ), SPECIES_MOLWT(  51 ), CONVERT_CONC(  51 ) / 'GLY             ',   51, 'GC',   58.00, F /
      DATA CHEMISTRY_SPC(  52 ), CGRID_INDEX(  52 ), SPECIES_TYPE(  52 ), SPECIES_MOLWT(  52 ), CONVERT_CONC(  52 ) / 'MGLY            ',   52, 'GC',   72.00, F /
      DATA CHEMISTRY_SPC(  53 ), CGRID_INDEX(  53 ), SPECIES_TYPE(  53 ), SPECIES_MOLWT(  53 ), CONVERT_CONC(  53 ) / 'BACL            ',   53, 'GC',   86.00, F /
      DATA CHEMISTRY_SPC(  54 ), CGRID_INDEX(  54 ), SPECIES_TYPE(  54 ), SPECIES_MOLWT(  54 ), CONVERT_CONC(  54 ) / 'CRES            ',   54, 'GC',  108.00, F /
      DATA CHEMISTRY_SPC(  55 ), CGRID_INDEX(  55 ), SPECIES_TYPE(  55 ), SPECIES_MOLWT(  55 ), CONVERT_CONC(  55 ) / 'BALD            ',   55, 'GC',  106.00, F /
      DATA CHEMISTRY_SPC(  56 ), CGRID_INDEX(  56 ), SPECIES_TYPE(  56 ), SPECIES_MOLWT(  56 ), CONVERT_CONC(  56 ) / 'METHACRO        ',   56, 'GC',   70.00, F /
      DATA CHEMISTRY_SPC(  57 ), CGRID_INDEX(  57 ), SPECIES_TYPE(  57 ), SPECIES_MOLWT(  57 ), CONVERT_CONC(  57 ) / 'MVK             ',   57, 'GC',   70.00, F /
      DATA CHEMISTRY_SPC(  58 ), CGRID_INDEX(  58 ), SPECIES_TYPE(  58 ), SPECIES_MOLWT(  58 ), CONVERT_CONC(  58 ) / 'ISOPROD         ',   58, 'GC',   70.00, F /
      DATA CHEMISTRY_SPC(  59 ), CGRID_INDEX(  59 ), SPECIES_TYPE(  59 ), SPECIES_MOLWT(  59 ), CONVERT_CONC(  59 ) / 'CCHO_SUR        ',   59, 'GC',   44.00, F /
      DATA CHEMISTRY_SPC(  60 ), CGRID_INDEX(  60 ), SPECIES_TYPE(  60 ), SPECIES_MOLWT(  60 ), CONVERT_CONC(  60 ) / 'DCB1            ',   60, 'GC',   58.00, F /
      DATA CHEMISTRY_SPC(  61 ), CGRID_INDEX(  61 ), SPECIES_TYPE(  61 ), SPECIES_MOLWT(  61 ), CONVERT_CONC(  61 ) / 'DCB2            ',   61, 'GC',   72.00, F /
      DATA CHEMISTRY_SPC(  62 ), CGRID_INDEX(  62 ), SPECIES_TYPE(  62 ), SPECIES_MOLWT(  62 ), CONVERT_CONC(  62 ) / 'DCB3            ',   62, 'GC',   72.00, F /
      DATA CHEMISTRY_SPC(  63 ), CGRID_INDEX(  63 ), SPECIES_TYPE(  63 ), SPECIES_MOLWT(  63 ), CONVERT_CONC(  63 ) / 'ETHENE          ',   63, 'GC',   28.00, F /
      DATA CHEMISTRY_SPC(  64 ), CGRID_INDEX(  64 ), SPECIES_TYPE(  64 ), SPECIES_MOLWT(  64 ), CONVERT_CONC(  64 ) / 'ISOPRENE        ',   64, 'GC',   68.00, F /
      DATA CHEMISTRY_SPC(  65 ), CGRID_INDEX(  65 ), SPECIES_TYPE(  65 ), SPECIES_MOLWT(  65 ), CONVERT_CONC(  65 ) / 'ISOPRXN         ',   65, 'GC',   68.00, F /
      DATA CHEMISTRY_SPC(  66 ), CGRID_INDEX(  66 ), SPECIES_TYPE(  66 ), SPECIES_MOLWT(  66 ), CONVERT_CONC(  66 ) / 'TRP1            ',   66, 'GC',  136.00, F /
      DATA CHEMISTRY_SPC(  67 ), CGRID_INDEX(  67 ), SPECIES_TYPE(  67 ), SPECIES_MOLWT(  67 ), CONVERT_CONC(  67 ) / 'TRPRXN          ',   67, 'GC',  136.00, F /
      DATA CHEMISTRY_SPC(  68 ), CGRID_INDEX(  68 ), SPECIES_TYPE(  68 ), SPECIES_MOLWT(  68 ), CONVERT_CONC(  68 ) / 'ALK1            ',   68, 'GC',   30.10, F /
      DATA CHEMISTRY_SPC(  69 ), CGRID_INDEX(  69 ), SPECIES_TYPE(  69 ), SPECIES_MOLWT(  69 ), CONVERT_CONC(  69 ) / 'ALK2            ',   69, 'GC',   36.70, F /
      DATA CHEMISTRY_SPC(  70 ), CGRID_INDEX(  70 ), SPECIES_TYPE(  70 ), SPECIES_MOLWT(  70 ), CONVERT_CONC(  70 ) / 'ALK3            ',   70, 'GC',   58.60, F /
      DATA CHEMISTRY_SPC(  71 ), CGRID_INDEX(  71 ), SPECIES_TYPE(  71 ), SPECIES_MOLWT(  71 ), CONVERT_CONC(  71 ) / 'ALK4            ',   71, 'GC',   77.60, F /
      DATA CHEMISTRY_SPC(  72 ), CGRID_INDEX(  72 ), SPECIES_TYPE(  72 ), SPECIES_MOLWT(  72 ), CONVERT_CONC(  72 ) / 'ALK5            ',   72, 'GC',  118.90, F /
      DATA CHEMISTRY_SPC(  73 ), CGRID_INDEX(  73 ), SPECIES_TYPE(  73 ), SPECIES_MOLWT(  73 ), CONVERT_CONC(  73 ) / 'ALK5RXN         ',   73, 'GC',  118.90, F /
      DATA CHEMISTRY_SPC(  74 ), CGRID_INDEX(  74 ), SPECIES_TYPE(  74 ), SPECIES_MOLWT(  74 ), CONVERT_CONC(  74 ) / 'ARO1            ',   74, 'GC',   98.60, F /
      DATA CHEMISTRY_SPC(  75 ), CGRID_INDEX(  75 ), SPECIES_TYPE(  75 ), SPECIES_MOLWT(  75 ), CONVERT_CONC(  75 ) / 'ARO1RO2         ',   75, 'GC',  147.60, F /
      DATA CHEMISTRY_SPC(  76 ), CGRID_INDEX(  76 ), SPECIES_TYPE(  76 ), SPECIES_MOLWT(  76 ), CONVERT_CONC(  76 ) / 'TOLNRXN         ',   76, 'GC',  147.60, F /
      DATA CHEMISTRY_SPC(  77 ), CGRID_INDEX(  77 ), SPECIES_TYPE(  77 ), SPECIES_MOLWT(  77 ), CONVERT_CONC(  77 ) / 'TOLHRXN         ',   77, 'GC',  147.60, F /
      DATA CHEMISTRY_SPC(  78 ), CGRID_INDEX(  78 ), SPECIES_TYPE(  78 ), SPECIES_MOLWT(  78 ), CONVERT_CONC(  78 ) / 'ARO2            ',   78, 'GC',  118.70, F /
      DATA CHEMISTRY_SPC(  79 ), CGRID_INDEX(  79 ), SPECIES_TYPE(  79 ), SPECIES_MOLWT(  79 ), CONVERT_CONC(  79 ) / 'ARO2RO2         ',   79, 'GC',  155.00, F /
      DATA CHEMISTRY_SPC(  80 ), CGRID_INDEX(  80 ), SPECIES_TYPE(  80 ), SPECIES_MOLWT(  80 ), CONVERT_CONC(  80 ) / 'XYLNRXN         ',   80, 'GC',  155.00, F /
      DATA CHEMISTRY_SPC(  81 ), CGRID_INDEX(  81 ), SPECIES_TYPE(  81 ), SPECIES_MOLWT(  81 ), CONVERT_CONC(  81 ) / 'XYLHRXN         ',   81, 'GC',  155.00, F /
      DATA CHEMISTRY_SPC(  82 ), CGRID_INDEX(  82 ), SPECIES_TYPE(  82 ), SPECIES_MOLWT(  82 ), CONVERT_CONC(  82 ) / 'OLE1            ',   82, 'GC',   72.30, F /
      DATA CHEMISTRY_SPC(  83 ), CGRID_INDEX(  83 ), SPECIES_TYPE(  83 ), SPECIES_MOLWT(  83 ), CONVERT_CONC(  83 ) / 'OLE2            ',   83, 'GC',   75.80, F /
      DATA CHEMISTRY_SPC(  84 ), CGRID_INDEX(  84 ), SPECIES_TYPE(  84 ), SPECIES_MOLWT(  84 ), CONVERT_CONC(  84 ) / 'SESQ            ',   84, 'GC',  204.00, F /
      DATA CHEMISTRY_SPC(  85 ), CGRID_INDEX(  85 ), SPECIES_TYPE(  85 ), SPECIES_MOLWT(  85 ), CONVERT_CONC(  85 ) / 'SESQRXN         ',   85, 'GC',  204.00, F /
      DATA CHEMISTRY_SPC(  86 ), CGRID_INDEX(  86 ), SPECIES_TYPE(  86 ), SPECIES_MOLWT(  86 ), CONVERT_CONC(  86 ) / 'BENZENE         ',   86, 'GC',   78.00, F /
      DATA CHEMISTRY_SPC(  87 ), CGRID_INDEX(  87 ), SPECIES_TYPE(  87 ), SPECIES_MOLWT(  87 ), CONVERT_CONC(  87 ) / 'BENZRO2         ',   87, 'GC',  127.00, F /
      DATA CHEMISTRY_SPC(  88 ), CGRID_INDEX(  88 ), SPECIES_TYPE(  88 ), SPECIES_MOLWT(  88 ), CONVERT_CONC(  88 ) / 'BNZNRXN         ',   88, 'GC',  127.00, F /
      DATA CHEMISTRY_SPC(  89 ), CGRID_INDEX(  89 ), SPECIES_TYPE(  89 ), SPECIES_MOLWT(  89 ), CONVERT_CONC(  89 ) / 'BNZHRXN         ',   89, 'GC',  127.00, F /
      DATA CHEMISTRY_SPC(  90 ), CGRID_INDEX(  90 ), SPECIES_TYPE(  90 ), SPECIES_MOLWT(  90 ), CONVERT_CONC(  90 ) / 'ACROLEIN        ',   90, 'GC',   56.10, F /
      DATA CHEMISTRY_SPC(  91 ), CGRID_INDEX(  91 ), SPECIES_TYPE(  91 ), SPECIES_MOLWT(  91 ), CONVERT_CONC(  91 ) / 'BUTADIENE13     ',   91, 'GC',   54.00, F /
      DATA CHEMISTRY_SPC(  92 ), CGRID_INDEX(  92 ), SPECIES_TYPE(  92 ), SPECIES_MOLWT(  92 ), CONVERT_CONC(  92 ) / 'FORM_PRIMARY    ',   92, 'GC',   30.00, F /
      DATA CHEMISTRY_SPC(  93 ), CGRID_INDEX(  93 ), SPECIES_TYPE(  93 ), SPECIES_MOLWT(  93 ), CONVERT_CONC(  93 ) / 'ALD2_PRIMARY    ',   93, 'GC',   44.00, F /
      DATA CHEMISTRY_SPC(  94 ), CGRID_INDEX(  94 ), SPECIES_TYPE(  94 ), SPECIES_MOLWT(  94 ), CONVERT_CONC(  94 ) / 'ACROLEIN_PRIMARY',   94, 'GC',   56.10, F /
      DATA CHEMISTRY_SPC(  95 ), CGRID_INDEX(  95 ), SPECIES_TYPE(  95 ), SPECIES_MOLWT(  95 ), CONVERT_CONC(  95 ) / 'AALKJ           ',  102, 'AE',  150.00, T /
      DATA CHEMISTRY_SPC(  96 ), CGRID_INDEX(  96 ), SPECIES_TYPE(  96 ), SPECIES_MOLWT(  96 ), CONVERT_CONC(  96 ) / 'AOLGAJ          ',  145, 'AE',  176.40, T /
      DATA CHEMISTRY_SPC(  97 ), CGRID_INDEX(  97 ), SPECIES_TYPE(  97 ), SPECIES_MOLWT(  97 ), CONVERT_CONC(  97 ) / 'AXYL1J          ',  103, 'AE',  192.00, T /
      DATA CHEMISTRY_SPC(  98 ), CGRID_INDEX(  98 ), SPECIES_TYPE(  98 ), SPECIES_MOLWT(  98 ), CONVERT_CONC(  98 ) / 'AXYL2J          ',  104, 'AE',  192.00, T /
      DATA CHEMISTRY_SPC(  99 ), CGRID_INDEX(  99 ), SPECIES_TYPE(  99 ), SPECIES_MOLWT(  99 ), CONVERT_CONC(  99 ) / 'ATOL1J          ',  106, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 100 ), CGRID_INDEX( 100 ), SPECIES_TYPE( 100 ), SPECIES_MOLWT( 100 ), CONVERT_CONC( 100 ) / 'ATOL2J          ',  107, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 101 ), CGRID_INDEX( 101 ), SPECIES_TYPE( 101 ), SPECIES_MOLWT( 101 ), CONVERT_CONC( 101 ) / 'ABNZ1J          ',  109, 'AE',  144.00, T /
      DATA CHEMISTRY_SPC( 102 ), CGRID_INDEX( 102 ), SPECIES_TYPE( 102 ), SPECIES_MOLWT( 102 ), CONVERT_CONC( 102 ) / 'ABNZ2J          ',  110, 'AE',  144.00, T /
      DATA CHEMISTRY_SPC( 103 ), CGRID_INDEX( 103 ), SPECIES_TYPE( 103 ), SPECIES_MOLWT( 103 ), CONVERT_CONC( 103 ) / 'ATRP1J          ',  112, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 104 ), CGRID_INDEX( 104 ), SPECIES_TYPE( 104 ), SPECIES_MOLWT( 104 ), CONVERT_CONC( 104 ) / 'AOLGBJ          ',  146, 'AE',  252.00, T /
      DATA CHEMISTRY_SPC( 105 ), CGRID_INDEX( 105 ), SPECIES_TYPE( 105 ), SPECIES_MOLWT( 105 ), CONVERT_CONC( 105 ) / 'ATRP2J          ',  113, 'AE',  168.00, T /
      DATA CHEMISTRY_SPC( 106 ), CGRID_INDEX( 106 ), SPECIES_TYPE( 106 ), SPECIES_MOLWT( 106 ), CONVERT_CONC( 106 ) / 'AISO1J          ',  114, 'AE',   96.00, T /
      DATA CHEMISTRY_SPC( 107 ), CGRID_INDEX( 107 ), SPECIES_TYPE( 107 ), SPECIES_MOLWT( 107 ), CONVERT_CONC( 107 ) / 'AISO2J          ',  115, 'AE',   96.00, T /
      DATA CHEMISTRY_SPC( 108 ), CGRID_INDEX( 108 ), SPECIES_TYPE( 108 ), SPECIES_MOLWT( 108 ), CONVERT_CONC( 108 ) / 'ASQTJ           ',  116, 'AE',  378.00, T /

      INTEGER, PARAMETER :: N_ACT_SP = 108

      INTEGER, PARAMETER :: NRXNS = 260

      INTEGER            :: KUNITS

      DATA  KUNITS /   2 /

      INTEGER IRXXN

      REAL( 8 )          :: RTDAT( 3,NRXNS )

      INTEGER, PARAMETER :: NFALLOFF =  14
      REAL( 8 )          :: RFDAT( 5,NFALLOFF )

      INTEGER            :: KTYPE( NRXNS )

      DATA ( KTYPE( IRXXN ), IRXXN = 1, NRXNS ) /  & 
     &      0,    2,    3,    2,    3,   10,    3,    3,    3,    3, & ! O   
     &     10,   10,    1,    3,    0,    0,    0,    0,    1,    3, & ! 1   
     &     10,    0,    0,    3,   10,    1,    8,    0,    9,    3, & ! 2   
     &      3,   10,   10,    0,    3,    3,    9,    9,    1,    3, & ! 3   
     &      0,    3,    3,   10,    3,    3,    3,    1,    3,    3, & ! 4   
     &      3,    3,    1,    1,    1,    6,    6,    6,    6,    6, & ! 5   
     &      6,    6,    6,    6,    6,    6,    6,    6,   10,   10, & ! 6   
     &      3,    3,    1,    3,    1,    6,    6,    3,    2,    3, & ! 7   
     &      3,    6,    6,    6,    6,    6,    6,    6,    6,    1, & ! 8   
     &      3,    6,    6,    6,    6,    6,    6,    6,    6,    6, & ! 9   
     &      6,    6,    3,    6,    6,    6,    6,    6,    6,    6, & ! O   
     &      6,    6,    6,    6,    1,    3,    3,    6,    1,    6, & ! 1   
     &      6,    6,    0,    0,    3,    3,    3,    6,    3,    3, & ! 2   
     &      0,    3,    1,    0,    3,    3,    0,    4,    0,    4, & ! 3   
     &      3,    0,    1,    0,    0,    0,    1,    3,    0,    1, & ! 4   
     &      3,    0,    1,    1,    1,    1,    6,    1,    0,    3, & ! 5   
     &      3,    3,    3,    1,    0,    3,    3,    1,    0,    1, & ! 6   
     &      1,    1,    0,    1,    0,    1,    0,    1,    1,    1, & ! 7   
     &      0,    1,    0,    3,    3,    3,    4,    3,    3,    3, & ! 8   
     &      3,    1,    3,    3,    3,    1,    4,    3,    3,    3, & ! 9   
     &      3,    3,    3,    3,    1,    3,    3,    3,    3,    3, & ! O   
     &      3,    3,    3,    1,    1,    1,    1,    1,    1,    1, & ! 1   
     &      1,    3,    3,    3,    1,    3,    1,    1,    0,    3, & ! 2   
     &      3,    1,    1,    0,    0,    3,    3,    3,    3,    0, & ! 3   
     &      3,    1,    3,    1,    1,    0,   -1,   -1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1/!5   

      INTEGER            :: IRXBITS( NRXNS )

      DATA ( IRXBITS( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,   20,    0,    4,    0,    1,    0,    0,    0,   16, & ! O   
     &      1,    1,    8,    0,    2,    2,    2,    2,    8,    4, & ! 1   
     &      1,    2,    2,    0,    1,    0,    0,    2,    0,    0, & ! 2   
     &      0,    1,    1,    2,    0,    0,    0,    8,    0,    0, & ! 3   
     &      2,    0,    0,    1,  128,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    1,    1, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    2,    2,    0,    0,    0,    0,    0,    0, & ! 2   
     &      2,    0,    0,    2,    0,    0,    2,    0,    2,    0, & ! 3   
     &      0,    2,    0,    2,    2,    2,    0,    0,    2,    0, & ! 4   
     &      0,    2,    0,    0,    0,    0,    0,    0,    2,    0, & ! 5   
     &      0,    0,    0,    0,    2,    0,    0,    0,    2,    0, & ! 6   
     &      0,    0,    2,    0,    2,    0,    2,    0,    0,    0, & ! 7   
     &      2,    0,    2,   64,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    2,    0, & ! 2   
     &      0,    0,    0,    2,    2,    0,    0,    0,    0,    2, & ! 3   
     &      0,    0,    0,    0,    0,    2,    1,    1,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

      INTEGER            :: IORDER( NRXNS )

      DATA ( IORDER( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    2,    3,    2,    2,    2,    2,    2,    3, & ! O   
     &      2,    1,    2,    2,    1,    1,    1,    1,    2,    2, & ! 1   
     &      2,    1,    1,    2,    2,    2,    2,    1,    2,    2, & ! 2   
     &      2,    2,    1,    1,    2,    2,    2,    3,    2,    2, & ! 3   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    1,    2,    2,    1,    2, & ! 1   
     &      2,    1,    1,    1,    2,    2,    1,    2,    2,    2, & ! 2   
     &      1,    2,    2,    1,    2,    2,    1,    2,    1,    2, & ! 3   
     &      2,    1,    2,    1,    1,    1,    2,    2,    1,    2, & ! 4   
     &      2,    1,    2,    2,    2,    2,    2,    2,    1,    2, & ! 5   
     &      2,    2,    2,    2,    1,    2,    2,    2,    1,    2, & ! 6   
     &      2,    2,    1,    2,    1,    2,    1,    2,    2,    2, & ! 7   
     &      1,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    2, & ! 2   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    1, & ! 3   
     &      2,    2,    2,    2,    2,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1/!5   

      INTEGER, PARAMETER :: KTN1 =  64
      INTEGER            :: KRX1( KTN1 )

      DATA ( KRX1( IRXXN ), IRXXN = 1, KTN1 ) / & 
     &     13,   19,   26,   39,   48,   53,   54,   55,   73,   75, & ! O   
     &     90,  115,  119,  133,  143,  147,  150,  153,  154,  155, & ! 1   
     &    156,  158,  164,  168,  170,  171,  172,  174,  176,  178, & ! 2   
     &    179,  180,  182,  192,  196,  205,  214,  215,  216,  217, & ! 3   
     &    218,  219,  220,  221,  225,  227,  228,  232,  233,  242, & ! 4   
     &    244,  245,  249,  250,  251,  252,  253,  254,  255,  256, & ! 5   
     &    257,  258,  259,  260/     !  6   

      INTEGER, PARAMETER :: KTN2 =   3
      INTEGER            :: KRX2( KTN2 )

      DATA ( KRX2( IRXXN ), IRXXN = 1, KTN2 ) / & 
     &      2,    4,   79/

      INTEGER, PARAMETER :: KTN3 =  87
      INTEGER            :: KRX3( KTN3 )

      DATA ( KRX3( IRXXN ), IRXXN = 1, KTN3 ) / & 
     &      3,    5,    7,    8,    9,   10,   14,   20,   24,   30, & ! O   
     &     31,   35,   36,   40,   42,   43,   45,   46,   47,   49, & ! 1   
     &     50,   51,   52,   71,   72,   74,   78,   80,   81,   91, & ! 2   
     &    103,  116,  117,  125,  126,  127,  129,  130,  132,  135, & ! 3   
     &    136,  141,  148,  151,  160,  161,  162,  163,  166,  167, & ! 4   
     &    184,  185,  186,  188,  189,  190,  191,  193,  194,  195, & ! 5   
     &    198,  199,  200,  201,  202,  203,  204,  206,  207,  208, & ! 6   
     &    209,  210,  211,  212,  213,  222,  223,  224,  226,  230, & ! 7   
     &    231,  236,  237,  238,  239,  241,  243/     !  8   

      INTEGER, PARAMETER :: KTN4 =   4
      INTEGER            :: KRX4( KTN4 )

      DATA ( KRX4( IRXXN ), IRXXN = 1, KTN4 ) / & 
     &    138,  140,  187,  197/

      INTEGER, PARAMETER :: KTN5 =   0
      INTEGER            :: KRX5( 1 )

      DATA   KRX5( 1 ) / 0 /

      INTEGER, PARAMETER :: KTN6 =  51
      INTEGER            :: KRX6( KTN6 )

      DATA ( KRX6( IRXXN ), IRXXN = 1, KTN6 ) / & 
     &     56,   57,   58,   59,   60,   61,   62,   63,   64,   65, & 
     &     66,   67,   68,   76,   77,   82,   83,   84,   85,   86, & 
     &     87,   88,   89,   92,   93,   94,   95,   96,   97,   98, & 
     &     99,  100,  101,  102,  104,  105,  106,  107,  108,  109, & 
     &    110,  111,  112,  113,  114,  118,  120,  121,  122,  128, & 
     &    157/

      INTEGER, PARAMETER :: KTN7 =   0
      INTEGER            :: KRX7( 1 )

      DATA   KRX7( 1 ) / 0 /

      INTEGER, PARAMETER :: NWM =   3
      INTEGER            :: NRXWM( NWM )

      DATA ( NRXWM( IRXXN ), IRXXN = 1, NWM ) /  & 
     &      2,    4,   20/
      REAL,    PARAMETER :: ATM_AIR = 1.00000E+06

      INTEGER, PARAMETER :: NWW =   3
      INTEGER            :: NRXWW( NWW )

      DATA ( NRXWW( IRXXN ), IRXXN = 1, NWW ) / & 
     &     13,   19,   38/

      INTEGER, PARAMETER :: NWO2 =   2
      INTEGER            :: NRXWO2( NWO2 )

      DATA ( NRXWO2( IRXXN ), IRXXN = 1, NWO2 ) / & 
     &      2,   10/
      REAL,    PARAMETER :: ATM_O2 = 2.09500E+05

      INTEGER, PARAMETER :: NWN2 =   0
      INTEGER            :: NRXWN2( 1 )

      DATA   NRXWN2( 1 ) / 0 /
      REAL,    PARAMETER :: ATM_N2 = 7.80800E+05

      INTEGER, PARAMETER :: NWCH4 =   1
      INTEGER            :: NRXWCH4( NWCH4 )

      DATA ( NRXWCH4( IRXXN ), IRXXN = 1, NWCH4 ) / & 
     &    184/
      REAL,    PARAMETER :: ATM_CH4 = 1.85000E+00

      INTEGER, PARAMETER :: NWH2 =   1
      INTEGER            :: NRXWH2( NWH2 )

      DATA ( NRXWH2( IRXXN ), IRXXN = 1, NWH2 ) / & 
     &     45/
      REAL,    PARAMETER :: ATM_H2 = 5.60000E-01

      INTEGER, PARAMETER :: MXPRD =  20
      INTEGER            :: IRR( NRXNS,MXPRD+3 )

      DATA ( IRR( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &      1,    3,    3,    3,    3,    3,    4,    4,    2,    2, & ! O   
     &      1,    6,    6,    1,    5,    5,    4,    4,    8,    8, & ! 1   
     &      9,   10,   10,    9,    9,    9,    9,    7,    9,    9, & ! 2   
     &     11,   11,   13,   13,   13,   11,   11,   11,    5,    5, & ! 3   
     &     14,   14,    9,    9,    9,   18,   18,   18,   18,   18, & ! 4   
     &     22,   22,   22,   22,   22,   24,   24,   24,   24,   24, & ! 5   
     &     24,   25,   25,   25,   25,   25,   25,   25,   29,   30, & ! 6   
     &     29,   29,   29,   29,   29,   29,   29,   29,   33,   34, & ! 7   
     &     33,   33,   33,   33,   33,   33,   33,   33,   33,   38, & ! 8   
     &     39,   38,   38,   38,   38,   38,   38,   38,   38,   38, & ! 9   
     &     38,   41,   42,   41,   41,   41,   41,   41,   41,   41, & ! O   
     &     41,   41,   41,   41,   43,   43,   40,   40,   40,   47, & ! 1   
     &     47,   47,   19,   19,   19,   19,   48,   48,   19,   35, & ! 2   
     &     35,   35,   50,   50,   50,   44,   44,   27,   27,   21, & ! 3   
     &     20,   20,   23,   23,   51,   51,   51,   51,   52,   52, & ! 4   
     &     52,   53,   46,   46,   54,   54,   45,   55,   55,   55, & ! 5   
     &     56,   56,   56,   56,   56,   57,   57,   57,   57,   58, & ! 6   
     &     58,   58,   58,   28,   28,   26,   26,   60,   60,   61, & ! 7   
     &     61,   62,   62,    9,   63,   63,   63,   63,   64,   64, & ! 8   
     &     64,   64,   66,   66,   66,   66,   68,   69,   70,   71, & ! 9   
     &     72,   74,   75,   75,   78,   79,   79,   82,   82,   82, & ! O   
     &     82,   83,   83,   83,   83,   49,   32,   37,   84,   84, & ! 1   
     &     84,   86,   87,   87,   90,   90,   90,   90,   90,   91, & ! 2   
     &     91,   91,   91,   92,   92,   92,   92,   92,   93,   93, & ! 3   
     &     93,   94,   94,   94,   94,   94,    6,    1,   95,   97, & ! 4   
     &     98,   99,  100,  101,  102,  103,  105,  106,  107,  108/!5   

      DATA ( IRR( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    4,    2,    1,    1,    2,    1,    5,    2, & ! O   
     &      5,    0,    0,    5,    0,    0,    0,    0,    0,    0, & ! 1   
     &      2,    0,    0,   10,    1,    5,    7,    0,   12,    4, & ! 2   
     &      2,    1,    0,    0,    9,    4,   11,   11,   11,    5, & ! 3   
     &      0,    9,   11,   15,    0,    2,   11,    5,   18,   18, & ! 4   
     &      2,   11,    5,   18,   22,    2,   11,    5,   18,   22, & ! 5   
     &     24,    2,   11,   18,    5,   22,   24,   25,    1,    0, & ! 6   
     &      2,   11,    5,   18,   22,   24,   25,   29,    1,    0, & ! 7   
     &      2,   11,    5,   18,   22,   24,   25,   29,   33,    1, & ! 8   
     &      0,    2,   11,    5,   18,   22,   24,   25,   29,   33, & ! 9   
     &     38,    1,    0,    2,   11,    5,   18,   22,   24,   25, & ! O   
     &     29,   33,   38,   41,    1,    0,    1,   11,    0,    1, & ! 1   
     &     11,    0,    0,    0,    9,   11,    0,    2,    5,    9, & ! 2   
     &      0,    5,    9,    0,    5,    9,    0,    9,    0,    9, & ! 3   
     &      9,    0,    9,    0,    0,    0,    9,    5,    0,    9, & ! 4   
     &      5,    0,    9,    5,    9,    5,    5,    9,    0,    5, & ! 5   
     &      9,    4,    5,    3,    0,    9,    4,    3,    0,    9, & ! 6   
     &      4,    5,    0,    9,    0,    9,    0,    9,    4,    9, & ! 7   
     &      0,    9,    0,    0,    9,    4,    5,    3,    9,    4, & ! 8   
     &      5,    3,    9,    4,    5,    3,    9,    9,    9,    9, & ! 9   
     &      9,    9,    2,   11,    9,    2,   11,    9,    4,    5, & ! O   
     &      3,    9,    4,    5,    3,    9,    9,    9,    4,    9, & ! 1   
     &      5,    9,    2,   11,    9,    4,    5,    3,    0,    9, & ! 2   
     &      4,    5,    3,    0,    0,    9,   11,    5,    9,    0, & ! 3   
     &      5,    9,    4,    5,    3,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

      DATA ( IRR( IRXXN,  4 ), IRXXN = 1, NRXNS ) / & 
     &      2,    4,    0,    1,    2,    5,    1,    5,    1,    1, & ! O   
     &      6,    1,    7,    2,    2,    1,    3,    8,    9,    3, & ! 1   
     &     10,    9,   11,    1,    7,   11,    5,    9,   11,   11, & ! 2   
     &      9,   13,   11,   11,    1,    9,   14,   14,    9,    1, & ! 3   
     &      9,   11,    0,   11,   11,    1,   20,   19,   21,   19, & ! 4   
     &      1,   23,    1,   11,   11,    1,   11,    1,   18,   22, & ! 5   
     &      0,   26,   23,   11,    1,   11,   25,   27,   30,   29, & ! 6   
     &     18,   31,   18,   32,   32,   29,   32,   18,   34,   33, & ! 7   
     &      1,   36,    1,   37,   37,   33,   37,   18,   35,   39, & ! 8   
     &     38,    1,   36,    1,   37,   37,   38,   37,   18,   35, & ! 9   
     &     40,   42,   41,    1,   36,    1,   37,   37,   41,   37, & ! O   
     &     18,   19,   19,   19,   26,   44,   45,   46,   46,    0, & ! 1   
     &     45,   45,   11,   12,   11,   48,   11,   49,    7,   29, & ! 2   
     &     12,    7,   22,   35,    7,   19,   29,   22,   29,   19, & ! 3   
     &     19,   19,   50,   50,   12,   19,   11,    7,   11,   12, & ! 4   
     &      7,   29,   40,    7,   40,    7,    7,   38,    0,    7, & ! 5   
     &     22,   11,    7,   50,   11,   22,   11,   50,   18,   22, & ! 6   
     &     11,   22,   11,   11,   22,    1,    1,   50,   11,   24, & ! 7   
     &     22,   24,   22,   18,   22,    9,   22,   11,   22,    9, & ! 8   
     &      1,   25,   22,    9,    1,   50,   22,    9,   22,   22, & ! 9   
     &     22,   11,    2,   11,   11,    2,   11,   22,    9,   22, & ! O   
     &     50,   22,    9,    1,   11,   11,   22,   22,    4,    9, & ! 1   
     &      5,   11,    2,   11,   22,    9,   22,   50,    9,   22, & ! 2   
     &      9,   22,   11,    0,    0,    9,   11,    5,    9,    0, & ! 3   
     &      5,    9,    4,    5,    3,    0,    7,   10,   96,   96, & ! 4   
     &     96,   96,   96,   96,   96,  104,  104,  104,  104,  104/!5   

      DATA ( IRR( IRXXN,  5 ), IRXXN = 1, NRXNS ) / & 
     &      3,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    5,    0,    1,    0,    3,    0,    0,    0,    0, & ! 1   
     &      0,    2,    1,    0,    0,    1,    0,    1,    0,    0, & ! 2   
     &      1,    0,    1,    1,    0,    0,    0,    0,    1,    0, & ! 3   
     &      0,    0,    0,   16,    0,   19,    0,   11,   19,   11, & ! 4   
     &     11,    0,   11,   19,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,   21,   11,   27,    0,   11,    0,    1, & ! 6   
     &      1,   32,    1,   19,    0,    0,   28,    0,    0,    1, & ! 7   
     &     35,   37,   35,   19,    0,    0,   28,   35,   22,    0, & ! 8   
     &      1,   40,   37,   40,   19,    0,    0,   28,   40,   22, & ! 9   
     &     24,    0,    1,   19,   37,   19,   19,    0,    0,    0, & ! O   
     &     19,   29,   29,   29,    0,   18,    0,    0,    0,    0, & ! 1   
     &      0,    0,   12,    0,   12,    0,   19,    1,   11,    0, & ! 2   
     &     11,   29,   25,   22,   33,   29,   18,   25,   35,   11, & ! 3   
     &      9,   11,   22,   11,   11,   12,   12,   11,   12,   29, & ! 4   
     &     12,    0,   22,   40,   22,   40,   47,    0,    0,   38, & ! 5   
     &     12,   22,   22,    0,   22,   25,   22,   27,   12,   25, & ! 6   
     &     22,   25,   29,   22,   25,   11,   11,   22,    9,   50, & ! 7   
     &     29,   50,   29,    0,   19,   11,   50,   22,   25,   22, & ! 8   
     &     22,   24,   25,   11,   22,   28,   35,   11,   25,   25, & ! 9   
     &     25,   22,   76,   77,   22,   80,   81,   25,   11,   25, & ! O   
     &     27,   25,   11,   22,   22,    0,   18,   35,   85,   85, & ! 1   
     &     85,   22,   88,   89,   41,   11,   25,    0,   11,   25, & ! 2   
     &     11,   25,   22,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    7,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

      DATA ( IRR( IRXXN,  6 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    9,    0,    0,    0,    0,    7,    0, & ! 3   
     &      0,    0,    0,   17,    0,   11,    0,    1,    0,    0, & ! 4   
     &      0,    0,    0,   21,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,   27,   27,   28,    0,   28,    0,    0, & ! 6   
     &      0,    4,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &     22,    4,   22,    0,    0,    0,    0,   22,    0,    0, & ! 8   
     &      0,   24,    4,   24,    0,    0,    0,    0,   24,   40, & ! 9   
     &      0,    0,    0,   29,    4,   29,    0,    0,    0,    0, & ! O   
     &     29,   35,   40,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,   11,   12,    0, & ! 2   
     &     18,    0,   33,   12,    0,   24,    0,   24,   22,    0, & ! 3   
     &     18,    9,    9,    9,    0,    0,   33,   12,   29,    0, & ! 4   
     &     29,    0,   51,    0,   52,    0,    0,    0,    0,    0, & ! 5   
     &     19,    9,   12,    0,    9,   24,    9,    0,   28,   41, & ! 6   
     &     33,   41,   33,   25,   24,   22,   22,   12,   12,   29, & ! 7   
     &     11,   29,   11,    0,   59,   12,    0,   18,   24,   25, & ! 8   
     &     25,   18,   24,   22,   25,   67,    0,   22,   24,   24, & ! 9   
     &     24,   25,    0,    0,   25,    0,    0,   24,   22,   24, & ! O   
     &     28,   24,   22,   25,   25,    0,   52,   50,    0,    0, & ! 1   
     &      0,   51,    0,    0,   12,   12,   41,    0,   18,   19, & ! 2   
     &     12,   57,   25,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

      DATA ( IRR( IRXXN,  7 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    5,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,   28,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   24, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,   22,   24,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   12,   11,    0,    0,    0,   29,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,   33,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     27,   33,   41,    0,   29,   29,   33,    0,   41,   12, & ! 6   
     &      9,   12,   12,   29,   29,   25,   25,    0,   51,    0, & ! 7   
     &     12,    0,   12,    0,    0,   19,    0,   12,   19,   24, & ! 8   
     &     24,   41,   19,   25,   24,    0,    0,   25,   43,   18, & ! 9   
     &     19,   28,    0,    0,   51,    0,    0,   19,   25,   35, & ! O   
     &      0,   19,   25,   24,   12,    0,    0,   53,    0,    0, & ! 1   
     &      0,   46,    0,    0,   19,   19,   12,    0,   41,   90, & ! 2   
     &     19,    0,   12,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

      DATA ( IRR( IRXXN,  8 ), IRXXN = 1, NRXNS ) / & 
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,   19,    0,    0,    0,    0,    0,    0, & ! 6   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 8   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,   35,    0,    0,    0,    0,   33,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     52,   12,    0,    0,   12,   19,   12,    0,    0,   19, & ! 6   
     &     12,    7,   19,   33,   33,   24,   24,    0,    0,    0, & ! 7   
     &     24,    0,   24,    0,    0,   49,    0,   19,   56,   41, & ! 8   
     &     58,   19,   50,   24,   50,    0,    0,   12,   19,   29, & ! 9   
     &     35,   51,    0,    0,   52,    0,    0,   35,   18,   50, & ! O   
     &      0,   35,   24,   18,   50,    0,    0,    0,    0,    0, & ! 1   
     &      0,   60,    0,    0,   59,   51,   50,    0,   12,   58, & ! 2   
     &     28,    0,   28,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,   19,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &     41,   19,    0,    0,   19,   50,   19,    0,    0,   59, & ! 6   
     &     19,   19,   59,   19,   19,   19,   19,    0,    0,    0, & ! 7   
     &     51,    0,   51,    0,    0,    0,    0,   59,   57,   12, & ! 8   
     &      0,   28,   28,   29,   26,    0,    0,   19,   35,   12, & ! 9   
     &     50,   52,    0,    0,   53,    0,    0,   50,   12,   44, & ! O   
     &      0,   50,   18,   19,   27,    0,    0,    0,    0,    0, & ! 1   
     &      0,   87,    0,    0,   51,   49,    0,    0,   19,    0, & ! 2   
     &     90,    0,   90,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,   35,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,   52,    0,    0,   41,   52,   52,    0,    0,   50, & ! 6   
     &     59,   50,   27,   35,   35,   35,   35,    0,    0,    0, & ! 7   
     &     52,    0,   52,    0,    0,    0,    0,   51,   58,   19, & ! 8   
     &      0,    0,   67,   33,   67,    0,    0,   50,   50,   19, & ! 9   
     &     44,   46,    0,    0,   54,    0,    0,   44,   19,   26, & ! O   
     &      0,   44,   29,   35,   28,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,   32,    0, & ! 2   
     &     49,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,    0,    0,    0,    0,    0,   50,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,   49,    0,    0,    0,    0,   49,    0,    0,   27, & ! 6   
     &     27,   52,    0,   50,   50,   50,   50,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,   65,   28, & ! 8   
     &      0,    0,    0,   12,    0,    0,    0,   44,   44,   35, & ! 9   
     &     27,   54,    0,    0,   55,    0,    0,   28,   35,    0, & ! O   
     &      0,   27,   33,   50,   56,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &     37,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 5   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   28, & ! 6   
     &     51,   26,    0,   27,    0,   44,   44,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   56, & ! 8   
     &      0,    0,    0,   19,    0,    0,    0,   51,   27,   50, & ! 9   
     &     28,   55,    0,    0,   60,    0,    0,    0,   50,    0, & ! O   
     &      0,   55,   12,   44,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   51, & ! 6   
     &     52,    0,    0,   28,    0,   27,   27,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   57, & ! 8   
     &      0,    0,    0,   50,    0,    0,    0,   49,    0,   44, & ! 9   
     &     73,   60,    0,    0,   61,    0,    0,    0,   44,    0, & ! O   
     &      0,   56,   19,   27,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   52, & ! 6   
     &     49,    0,    0,    0,    0,   28,   28,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   49, & ! 8   
     &      0,    0,    0,   44,    0,    0,    0,    0,    0,   27, & ! 9   
     &      0,   61,    0,    0,   62,    0,    0,    0,   28,    0, & ! O   
     &      0,   58,   35,   55,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &     37,    0,    0,    0,    0,   26,    0,    0,    0,    0, & ! 7   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,   37, & ! 8   
     &      0,    0,    0,   28,    0,    0,    0,    0,    0,   28, & ! 9   
     &      0,   62,    0,    0,   79,    0,    0,    0,   49,    0, & ! O   
     &      0,    0,   50,   57,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,    0,   51,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,   75,    0,    0,    0,    0,    0,    0,   32,    0, & ! O   
     &      0,    0,   44,   26,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,    0,   53,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,   37,    0, & ! O   
     &      0,    0,   27,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,    0,   49,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,   28,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,    0,   37,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,   55,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,    0,   67,    0,    0,    0,    0,    0,    0, & ! 9   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! O   
     &      0,    0,   56,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,   49,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,   32,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

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
     &      0,    0,   37,    0,    0,    0,    0,    0,    0,    0, & ! 1   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 2   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 3   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0, & ! 4   
     &      0,    0,    0,    0,    0,    0,    0,    0,    0,    0/!5   

      DATA ( RTDAT( 1,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     1.0000D+00, 5.6800D-34, 8.0000D-12, 1.0000D-31, 6.5000D-12, & ! O   
     &     9.0000D-32, 1.8000D-12, 1.4000D-13, 1.8000D-11, 3.3000D-39, & ! +   
     &     2.8000D-30, 1.0000D-03, 2.6000D-22, 4.5000D-14, 1.0000D+00, & ! 1   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.2000D-10, 2.0900D-11, & ! +   
     &     7.0000D-31, 1.0000D+00, 1.0000D+00, 2.7000D-12, 2.4300D-30, & ! 2   
     &     2.0000D-11, 7.2000D-15, 1.0000D+00, 1.3000D-13, 1.9000D-12, & ! +   
     &     3.4000D-12, 1.8000D-31, 4.1000D-05, 1.0000D+00, 1.5000D-12, & ! 3   
     &     1.4000D-14, 2.2000D-13, 3.0800D-34, 4.0000D-12, 8.5000D-13, & ! +   
     &     1.0000D+00, 2.9000D-12, 4.8000D-11, 4.0000D-31, 7.7000D-12, & ! 4   
     &     2.8000D-12, 3.8000D-13, 1.3000D-12, 2.4500D-14, 5.9000D-13, & ! +   
     &     2.7000D-12, 1.9000D-13, 2.3000D-12, 2.0000D-13, 3.5000D-14, & ! 5   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 6   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 2.7000D-28, 4.9000D-03, & ! +   
     &     7.8000D-12, 4.3000D-13, 4.0000D-12, 1.8000D-12, 7.5000D-12, & ! 7   
     &     1.0000D+00, 1.0000D+00, 2.9000D-12, 1.2000D-11, 2.0000D+15, & ! +   
     &     1.2500D-11, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 8   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.3700D-11, & ! +   
     &     7.9000D+16, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! 9   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.6000D+16, 1.0000D+00, 1.0000D+00, & ! O   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 2.4000D-11, & ! 1   
     &     7.5000D+14, 2.3000D-11, 1.0000D+00, 1.0000D-03, 1.0000D+00, & ! +   
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 8.6000D-12, & ! 2   
     &     9.7000D-15, 2.4000D+12, 1.0000D+00, 2.0000D-12, 5.6000D-12, & ! +   
     &     1.0000D+00, 1.4000D-12, 2.0000D-11, 1.0000D+00, 1.4000D-12, & ! 3   
     &     1.1000D-12, 1.0000D+00, 1.3000D-12, 1.5000D-01, 3.1000D-12, & ! +   
     &     2.9000D-12, 1.0000D+00, 1.1000D-11, 1.0000D+00, 1.0000D+00, & ! 4   
     &     6.0000D-03, 1.1000D-11, 2.8000D-12, 1.0000D+00, 1.5000D-11, & ! +   
     &     1.4000D-12, 1.0000D+00, 2.6300D-11, 3.7800D-12, 4.2000D-11, & ! 5   
     &     1.3700D-11, 1.0000D+00, 1.2900D-11, 5.0000D-02, 1.4000D-12, & ! +   
     &     1.8600D-11, 1.3600D-15, 1.5000D-12, 6.3400D-12, 4.1000D-03, & ! 6   
     &     4.1400D-12, 7.5100D-16, 4.3200D-12, 2.1000D-03, 6.1900D-11, & ! +   
     &     4.1800D-18, 1.0000D-13, 4.1000D-03, 1.5000D-11, 2.0000D-02, & ! 7   
     &     7.8000D-12, 1.0000D+00, 5.0000D-11, 2.0000D-18, 5.0000D-11, & ! +   
     &     3.6500D-01, 5.0000D-11, 7.2800D+00, 2.1500D-12, 1.9600D-12, & ! 8   
     &     9.1400D-15, 4.3900D-13, 1.0400D-11, 2.5000D-11, 7.8600D-15, & ! +   
     &     3.0300D-12, 3.6000D-11, 1.8300D-11, 1.0800D-15, 3.6600D-12, & ! 9   
     &     3.2700D-11, 1.3700D-12, 9.8700D-12, 1.0200D-11, 5.9500D-12, & ! +   
     &     1.1100D-11, 1.8100D-12, 2.7000D-12, 1.9000D-13, 2.6400D-11, & ! O   
     &     2.7000D-12, 1.9000D-13, 7.1000D-12, 2.6200D-15, 4.4500D-14, & ! +   
     &     1.0700D-11, 1.7400D-11, 5.0200D-16, 7.2600D-13, 2.0900D-11, & ! 1   
     &     4.5000D-13, 8.0000D-13, 1.1600D-12, 1.1600D-14, 1.9700D-10, & ! +   
     &     1.9000D-11, 2.4700D-12, 2.7000D-12, 1.9000D-13, 1.9900D-11, & ! 2   
     &     1.3600D-15, 2.9380D-15, 2.3670D-12, 2.0000D-03, 1.4800D-11, & ! +   
     &     1.3400D-14, 1.0000D-13, 1.9800D-11, 1.0000D+00, 1.0000D+00, & ! 3   
     &     8.6000D-12, 9.7000D-15, 2.0000D-12, 5.6000D-12, 1.0000D+00, & ! +   
     &     1.4000D-12, 1.9900D-11, 1.3600D-15, 2.9380D-15, 2.3670D-12, & ! 4   
     &     2.0000D-03, 1.0000D+00, 1.0000D+00, 9.4882D-06, 9.4882D-06, & ! +   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, & ! 5   
     &     9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06, 9.4882D-06/!+   

      DATA ( RTDAT( 2,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00,-2.8000D+00, 0.0000D+00,-1.6000D+00, 0.0000D+00, & ! O   
     &    -2.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -3.5000D+00,-3.5000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -2.6000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-3.1000D+00, & ! 2   
     &     0.0000D+00, 7.8500D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-3.2000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 6.0000D+02, 2.8000D+03, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-3.3000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     5.1000D+01, 5.2000D+01, 5.3000D+01, 5.4000D+01, 5.5000D+01, & ! +   
     &     5.5000D+01, 5.1000D+01, 5.2000D+01, 5.4000D+01, 5.3000D+01, & ! 6   
     &     5.5000D+01, 5.5000D+01, 5.5000D+01,-7.1000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     7.5000D+01, 7.5000D+01, 0.0000D+00,-9.0000D-01, 0.0000D+00, & ! +   
     &     0.0000D+00, 7.2000D+01, 7.3000D+01, 7.4000D+01, 7.5000D+01, & ! 8   
     &     7.5000D+01, 7.5000D+01, 7.8000D+01, 7.8000D+01, 0.0000D+00, & ! +   
     &     0.0000D+00, 8.1000D+01, 7.2000D+01, 7.3000D+01, 7.4000D+01, & ! 9   
     &     7.5000D+01, 7.5000D+01, 7.5000D+01, 7.8000D+01, 7.8000D+01, & ! +   
     &     7.8000D+01, 7.9000D+01, 0.0000D+00, 8.1000D+01, 7.2000D+01, & ! O   
     &     7.3000D+01, 7.4000D+01, 7.5000D+01, 7.5000D+01, 7.5000D+01, & ! +   
     &     7.8000D+01, 7.8000D+01, 7.8000D+01, 7.8000D+01, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 5.2000D+01, 0.0000D+00, 1.1700D+02, & ! +   
     &     5.2000D+01, 1.1900D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 2   
     &     0.0000D+00, 0.0000D+00, 4.6000D+01, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     0.0000D+00, 0.0000D+00, 2.0000D+00, 0.0000D+00, 2.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 1.5400D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 2.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 2.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
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
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   

      DATA ( RTDAT( 3,IRXXN ), IRXXN = 1, NRXNS ) / & 
     &     0.0000D+00, 0.0000D+00,-2.0600D+03, 0.0000D+00, 1.2000D+02, & ! O   
     &     0.0000D+00,-1.3700D+03,-2.4700D+03, 1.1000D+02, 5.3000D+02, & ! +   
     &     0.0000D+00,-1.1000D+04, 0.0000D+00,-1.2600D+03, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 9.5000D+01, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 2.6000D+02, 0.0000D+00, & ! 2   
     &     0.0000D+00, 4.1000D-16, 0.0000D+00, 3.1900D-33,-1.0000D+03, & ! +   
     &     2.7000D+02, 0.0000D+00,-1.0650D+04, 0.0000D+00, 3.6000D+02, & ! 3   
     &    -6.0000D+02, 1.8500D-33, 2.5900D-54, 0.0000D+00,-2.4500D+03, & ! +   
     &     0.0000D+00,-1.6000D+02, 2.5000D+02, 0.0000D+00,-2.1000D+03, & ! 4   
     &     2.8500D+02, 7.8000D+02, 0.0000D+00, 7.1000D+02,-5.0900D+02, & ! +   
     &     3.6000D+02, 1.3000D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 6   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.2100D+04, & ! +   
     &     3.0000D+02, 1.0400D+03, 0.0000D+00, 5.0000D+02, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 5.0000D+02, 0.0000D+00,-1.2800D+04, & ! +   
     &     2.4000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 8   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &    -1.4000D+04, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 9   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00,-1.3486D+04, 0.0000D+00, 0.0000D+00, & ! O   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 1   
     &    -8.1520D+03, 1.5000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 2.0000D+01, & ! 2   
     &     6.2500D+02,-7.0000D+03, 0.0000D+00,-2.4310D+03, 3.1000D+02, & ! +   
     &     0.0000D+00,-1.8600D+03, 0.0000D+00, 0.0000D+00,-1.7710D+03, & ! 3   
     &    -5.2000D+02, 0.0000D+00,-2.5000D+01, 0.0000D+00,-3.6000D+02, & ! +   
     &     1.9000D+02, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00,-2.3760D+03, 0.0000D+00, 0.0000D+00, & ! +   
     &    -1.8950D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,-1.8720D+03, & ! +   
     &     1.7600D+02,-2.1140D+03,-1.7260D+03, 0.0000D+00, 0.0000D+00, & ! 6   
     &     4.5300D+02,-1.5200D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 7   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.7350D+03, 4.3800D+02, & ! 8   
     &    -2.5800D+03,-2.2820D+03,-7.9200D+02, 4.0800D+02,-1.9120D+03, & ! +   
     &    -4.4800D+02, 0.0000D+00, 4.4900D+02,-8.2100D+02, 1.7500D+02, & ! 9   
     &     0.0000D+00,-4.9800D+02,-6.7100D+02,-4.3400D+02,-9.1000D+01, & ! +   
     &    -5.2000D+01, 3.5500D+02, 3.6000D+02, 1.3000D+03, 0.0000D+00, & ! O   
     &     3.6000D+02, 1.3000D+03, 4.5100D+02,-1.6400D+03,-3.7600D+02, & ! +   
     &    -2.3400D+02, 3.8400D+02,-4.6100D+02, 0.0000D+00, 0.0000D+00, & ! 1   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00,-2.0600D+02, 3.6000D+02, 1.3000D+03, 0.0000D+00, & ! 2   
     &    -2.5190D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 4.4800D+02, & ! +   
     &    -2.2830D+03, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 3   
     &     2.0000D+01, 6.2500D+02,-2.4310D+03, 3.1000D+02, 0.0000D+00, & ! +   
     &    -1.8600D+03, 0.0000D+00,-2.5190D+03, 0.0000D+00, 0.0000D+00, & ! 4   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! +   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & ! 5   
     &     0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00/!+   
      INTEGER            :: IRRFALL( NFALLOFF )

      DATA ( IRRFALL( IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &      6,   11,   12,   21,   25,   27,   29,   32,   33,   37, & 
     &     38,   44,   69,   70/

      DATA ( RFDAT( 1,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     2.2000D-11, 2.0000D-12, 9.7000D+14, 3.6000D-11, 1.6700D-11, & 
     &     1.4400D+03, 0.0000D+00, 4.7000D-12, 5.7000D+15, 9.8000D+02, & 
     &     3.1800D+03, 2.0000D-12, 1.2000D-11, 4.0000D+16/

      DATA ( RFDAT( 2,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 2.0000D-01, 1.0000D-01,-1.0000D-01,-2.1000D+00, & 
     &     1.9000D-33, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00,-9.0000D-01, 0.0000D+00/

      DATA ( RFDAT( 3,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     0.0000D+00, 0.0000D+00,-1.1080D+04, 0.0000D+00, 0.0000D+00, & 
     &     7.2500D+02, 0.0000D+00, 0.0000D+00,-1.1170D+04, 0.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 0.0000D+00,-1.3600D+04/

      DATA ( RFDAT( 4,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     8.0000D-01, 4.5000D-01, 4.5000D-01, 6.0000D-01, 6.0000D-01, & 
     &     0.0000D+00, 0.0000D+00, 6.0000D-01, 5.0000D-01, 0.0000D+00, & 
     &     0.0000D+00, 4.5000D-01, 3.0000D-01, 3.0000D-01/

      DATA ( RFDAT( 5,IRXXN ), IRXXN = 1, NFALLOFF ) / & 
     &     1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00, & 
     &     0.0000D+00, 0.0000D+00, 1.0000D+00, 1.0000D+00, 0.0000D+00, & 
     &     0.0000D+00, 1.0000D+00, 1.0000D+00, 1.0000D+00/

      REAL               :: SC( NRXNS,MXPRD )

      DATA ( SC( IRXXN,  1 ), IRXXN = 1, NRXNS ) / & 
     &        1.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! O   
     &        1.00000,    1.00000,    1.00000,    2.00000,    2.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    1.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    2.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.61000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    0.80000,    2.00000, & ! +   
     &        2.00000,    1.00000,    0.00000,    1.00000,    1.00000, & ! 4   
     &        1.00000,    1.00000,    1.00000,    1.00000,    2.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 6   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.75000,    1.00000,    1.00000,    1.00000, & ! 7   
     &        1.00000,    1.00000,    2.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.75000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        1.00000,    1.00000,    1.00000,    2.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.75000,    1.00000,    1.00000, & ! 9   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    1.00000,    1.00000,    1.00000,    0.75000, & ! O   
     &        1.00000,    1.00000,    1.00000,    1.00000,    2.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    2.00000,    1.00000, & ! 1   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    2.00000,    1.00000,    1.00000, & ! 2   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    1.00000,    0.03400,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    0.37000,    1.00000,    1.00000, & ! +   
     &        0.35000,    1.00000,    1.00000,    1.00000,    2.00000, & ! 4   
     &        1.00000,    0.63000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    2.00000,    0.24000,    1.00000,    0.24000, & ! 5   
     &        1.00000,    1.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        0.50000,    0.00800,    0.50000,    1.00000,    0.34000, & ! 6   
     &        0.30000,    0.06400,    0.45000,    0.30000,    0.67000, & ! +   
     &        0.40000,    0.79900,    1.23300,    0.37900,    0.96000, & ! 7   
     &        0.33800,    1.00000,    1.00000,    1.50000,    1.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 8   
     &        0.12000,    1.00000,    0.50000,    0.90700,    0.26600, & ! +   
     &        0.18700,    0.01000,    0.75000,    0.56700,    0.47400, & ! 9   
     &        0.14700,    1.00000,    0.24600,    0.69500,    0.83500, & ! +   
     &        0.65300,    0.22400,    1.00000,    1.00000,    0.18700, & ! O   
     &        1.00000,    1.00000,    0.91000,    0.15500,    0.82400, & ! +   
     &        0.45000,    0.91800,    0.37800,    0.39100,    0.01300, & ! 1   
     &        1.00000,    0.13000,    1.00000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.23600,    1.00000,    1.00000,    0.25000, & ! 2   
     &        0.31000,    0.03100,    1.00000,    0.17200,    0.96100, & ! +   
     &        0.06000,    0.92100,    0.25000,    0.00000,    0.00000, & ! 3   
     &        1.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    1.00000,    1.00000, & ! 4   
     &        0.00000,    2.00000,    0.50000,    1.14280,    1.14280, & ! +   
     &        1.14280,    1.00000,    1.00000,    0.85714,    0.85714, & ! 5   
     &        1.00000,    1.00000,    0.50000,    0.50000,    1.50000/! &  

      DATA ( SC( IRXXN,  2 ), IRXXN = 1, NRXNS ) / & 
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.00000,    1.00000,    0.00000, & ! 1   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 2   
     &        1.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.61000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.80000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 4   
     &        1.00000,    0.00000,    1.00000,    1.00000,    2.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.75000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.25000,    1.00000, & ! 6   
     &        0.50000,    0.00000,    1.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.25000,    1.00000,    1.00000,    0.00000, & ! 7   
     &        0.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        1.00000,    0.25000,    1.00000,    1.00000,    0.00000, & ! 8   
     &        0.00000,    1.00000,    1.00000,    2.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.25000,    1.00000,    1.00000, & ! 9   
     &        0.00000,    0.00000,    1.00000,    1.00000,    1.00000, & ! +   
     &        2.00000,    0.00000,    1.00000,    1.00000,    0.25000, & ! O   
     &        1.00000,    1.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    2.00000,    0.00000, & ! 1   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.00000,    1.00000, & ! 2   
     &        0.00000,    1.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    0.00100,    1.00000,    1.00000, & ! 3   
     &        1.00000,    1.00000,    0.04200,    1.00000,    1.00000, & ! +   
     &        0.35000,    1.00000,    0.34000,    1.00000,    2.00000, & ! 4   
     &        1.00000,    1.26000,    0.63000,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.00000,    0.76000,    1.00000,    0.76000, & ! 5   
     &        1.00000,    1.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.41600,    0.10000,    0.50000,    0.00000,    0.33000, & ! 6   
     &        0.02500,    0.05000,    0.55000,    0.70000,    0.04100, & ! +   
     &        0.04800,    0.05100,    0.46700,    0.47300,    0.04000, & ! 7   
     &        0.11300,    0.34100,    1.00000,    0.50000,    1.00000, & ! +   
     &        0.50000,    1.00000,    0.50000,    0.00000,    1.61000, & ! 8   
     &        0.12000,    1.00000,    0.20000,    0.09300,    0.06600, & ! +   
     &        0.74900,    0.24000,    0.25000,    0.03300,    0.27600, & ! 9   
     &        0.85300,    1.00000,    0.12100,    0.07000,    0.14300, & ! +   
     &        0.34700,    0.76500,    1.00000,    1.00000,    0.80400, & ! O   
     &        1.00000,    1.00000,    0.09000,    0.05600,    0.17600, & ! +   
     &        0.43700,    0.08200,    0.00300,    0.44200,    0.01200, & ! 1   
     &        0.00000,    0.87000,    0.60500,    1.00000,    1.00000, & ! +   
     &        1.00000,    0.76400,    1.00000,    1.00000,    0.75000, & ! 2   
     &        0.81000,    0.00200,    0.00000,    1.01000,    0.03900, & ! +   
     &        0.06000,    0.08000,    0.23000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.50000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        1.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.25000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.50000,    1.00000, & ! 6   
     &        0.50000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.25000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.25000,    1.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    0.25000,    1.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    1.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.25000, & ! O   
     &        1.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    1.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.96500,    1.00000,    0.00000, & ! 3   
     &        1.00000,    0.00000,    0.61600,    1.00000,    0.00000, & ! +   
     &        0.65000,    1.00000,    0.66000,    1.00000,    0.00000, & ! 4   
     &        0.00000,    0.37000,    1.26000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    0.23000,    0.00000,    0.23000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.08400,    0.20800,    0.50000,    0.00000,    0.33000, & ! 6   
     &        0.67500,    0.16400,    0.00000,    0.70000,    0.28900, & ! +   
     &        0.04800,    0.15000,    0.30000,    0.07000,    0.51500, & ! 7   
     &        0.37600,    0.56400,    1.00000,    1.50000,    1.00000, & ! +   
     &        0.50000,    1.00000,    0.50000,    0.00000,    0.19500, & ! 8   
     &        0.50000,    0.00000,    0.30000,    0.07900,    0.00800, & ! +   
     &        0.06400,    0.25000,    0.50000,    0.03100,    0.25000, & ! 9   
     &        1.00000,    0.00000,    0.61200,    0.55900,    0.93600, & ! +   
     &        0.94800,    0.01100,    0.00000,    0.00000,    0.00900, & ! O   
     &        0.00000,    0.00000,    0.20500,    0.02200,    0.48800, & ! +   
     &        0.11300,    0.00100,    0.03300,    0.13600,    0.00100, & ! 1   
     &        0.00000,    0.13000,    0.21000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.20700,    0.00000,    0.00000,    0.16700, & ! 2   
     &        1.00000,    0.96700,    0.00000,    0.17200,    0.48100, & ! +   
     &        0.25000,    0.92100,    0.02000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.00000,    0.50000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    1.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    1.00000,    1.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.03400,    1.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.49200,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.37000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.41600,    0.10000,    0.50000,    0.00000,    0.67000, & ! 6   
     &        0.67500,    0.05000,    0.00000,    0.30000,    0.33600, & ! +   
     &        0.28500,    0.57200,    1.23300,    0.02900,    0.66700, & ! 7   
     &        0.17300,    0.09500,    0.00000,    1.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 8   
     &        1.00000,    0.00000,    0.49100,    0.62400,    0.12600, & ! +   
     &        0.18700,    0.24000,    0.27600,    0.18000,    0.75000, & ! 9   
     &        0.00000,    0.00000,    0.02100,    0.23600,    0.01100, & ! +   
     &        0.02600,    0.05500,    0.00000,    0.00000,    0.09700, & ! O   
     &        0.00000,    0.00000,    0.73200,    0.00100,    0.00900, & ! +   
     &        0.00000,    0.24400,    0.00200,    0.71100,    0.01200, & ! 1   
     &        0.00000,    0.00000,    0.18500,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.23600,    0.00000,    0.00000,    0.08300, & ! 2   
     &        0.50000,    0.03100,    0.00000,    0.33000,    0.48100, & ! +   
     &        0.50000,    0.00000,    0.23000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.00000,    0.75000,    0.00000, & ! 6   
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
     &        0.00000,    0.00000,    0.03400,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.09600,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.08400,    0.45000,    0.00000,    0.00000,    0.67000, & ! 6   
     &        0.30000,    0.47500,    0.00000,    0.00000,    0.05500, & ! +   
     &        0.49800,    0.15000,    0.30000,    0.04900,    0.33300, & ! 7   
     &        0.59600,    0.15200,    0.00000,    0.00000,    0.00000, & ! +   
     &        1.00000,    0.00000,    1.00000,    0.00000,    0.00000, & ! 8   
     &        0.37000,    0.00000,    0.19100,    0.23000,    0.19200, & ! +   
     &        0.93600,    0.24000,    0.47400,    0.72900,    0.47400, & ! 9   
     &        0.00000,    0.00000,    0.16000,    0.02600,    0.01100, & ! +   
     &        0.09900,    0.11800,    0.00000,    0.00000,    0.28700, & ! O   
     &        0.00000,    0.00000,    0.29400,    0.07600,    0.03700, & ! +   
     &        0.00000,    0.73200,    0.13700,    0.03000,    0.06900, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.76400,    0.00000,    0.00000,    0.16700, & ! 2   
     &        0.50000,    0.03100,    0.00000,    1.18200,    0.48100, & ! +   
     &        0.12500,    0.00000,    0.75000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.11500,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.50000,    0.20000,    0.00000,    0.00000,    0.67000, & ! 6   
     &        0.67500,    0.10000,    0.00000,    0.00000,    0.12900, & ! +   
     &        0.12500,    0.22700,    0.46700,    0.21300,    0.50600, & ! 7   
     &        0.01000,    0.13400,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.50000,    0.00000,    0.50000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.25000,    0.32000,    0.27500, & ! +   
     &        0.00000,    0.75000,    0.27600,    0.12300,    0.27600, & ! 9   
     &        0.00000,    0.00000,    0.03900,    0.44500,    0.00200, & ! +   
     &        0.20400,    0.11900,    0.00000,    0.00000,    0.08700, & ! O   
     &        0.00000,    0.00000,    0.49700,    0.34500,    0.02400, & ! +   
     &        0.00000,    0.51100,    0.19700,    0.07900,    0.65900, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.76400,    0.00000,    0.00000,    0.08300, & ! 2   
     &        0.18500,    0.00000,    0.00000,    0.34000,    0.00000, & ! +   
     &        0.50000,    0.00000,    0.23000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.48200,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.90000,    0.00000,    0.00000,    0.33000, & ! 6   
     &        0.30000,    0.95000,    0.00000,    0.00000,    0.01300, & ! +   
     &        0.04700,    0.21800,    0.23300,    0.08400,    0.24600, & ! 7   
     &        0.43900,    0.43100,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.50000,    0.00000,    0.50000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00900,    0.35700,    0.59200, & ! +   
     &        0.00000,    0.00000,    1.00000,    0.20100,    1.00000, & ! 9   
     &        0.00000,    0.00000,    0.15500,    0.12200,    0.02400, & ! +   
     &        0.07200,    0.01700,    0.00000,    0.00000,    0.18700, & ! O   
     &        0.00000,    0.00000,    0.00500,    0.50000,    0.51100, & ! +   
     &        0.00000,    0.12700,    0.13700,    0.50700,    0.25900, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.11200,    0.00000, & ! +   
     &        0.18500,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.37000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.33300,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.35100,    0.00000,    0.00000,    0.15000, & ! +   
     &        0.21000,    0.00800,    0.00000,    0.55800,    0.71000, & ! 7   
     &        0.21300,    0.14700,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.10000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.15700,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.41700,    0.02400,    0.45500, & ! +   
     &        0.08900,    0.20700,    0.00000,    0.00000,    0.05000, & ! O   
     &        0.00000,    0.00000,    0.11900,    0.15400,    0.00000, & ! +   
     &        0.00000,    0.07200,    0.00600,    0.15100,    0.01200, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.37500,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 6   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.33200, & ! +   
     &        0.02300,    0.57200,    0.00000,    0.11500,    0.00000, & ! 7   
     &        0.00600,    0.02000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.39000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.23500,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.24800,    0.33200,    0.24400, & ! +   
     &        0.41700,    0.05900,    0.00000,    0.00000,    0.56100, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.36300,    0.00000, & ! +   
     &        0.00000,    0.06100,    0.26500,    0.10200,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.15000, & ! +   
     &        0.74200,    0.00000,    0.00000,    0.32900,    0.00000, & ! 7   
     &        0.17700,    0.24300,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.16000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.20500,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.12100,    0.00000,    0.45200, & ! +   
     &        1.00000,    0.49100,    0.00000,    0.00000,    0.09900, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00100,    0.00000, & ! +   
     &        0.00000,    0.02500,    0.26900,    0.00100,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.17400, & ! +   
     &        0.10000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.04800,    0.43500,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.20400, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.13000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.11000, & ! +   
     &        0.00000,    0.10800,    0.00000,    0.00000,    0.09300, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.21500,    0.00000, & ! +   
     &        0.00000,    0.02500,    0.45600,    0.01500,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.37200,    0.00000,    0.00000,    0.00000,    0.00000, & ! 7   
     &        0.31000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 8   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.15000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.27600,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.12500, & ! +   
     &        0.00000,    0.05100,    0.00000,    0.00000,    0.80400, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.18500,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.30500,    0.04800,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.00000,    0.00100,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.76500,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.05000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.04500,    0.32100,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.00000,    0.03100,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.11900,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.02600,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.00000,    0.10300,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00600,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.00000,    0.18900,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.04200,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.00000,    1.00000,    0.00000, & ! 9   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! O   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.02600,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.07300,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.12900,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

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
     &        0.00000,    0.00000,    0.30300,    0.00000,    0.00000, & ! 1   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 2   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 3   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 4   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! +   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000, & ! 5   
     &        0.00000,    0.00000,    0.00000,    0.00000,    0.00000/! &  

      INTEGER            :: NREACT( NRXNS )

      DATA ( NREACT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      1,    1,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    1,    1,    2,    1,    1,    1,    1,    1,    1, & ! 1   
     &      2,    1,    1,    2,    2,    2,    2,    1,    2,    2, & ! 2   
     &      2,    2,    1,    1,    2,    2,    2,    2,    2,    2, & ! 3   
     &      1,    2,    2,    2,    1,    2,    2,    2,    2,    2, & ! 4   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 5   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 6   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    1, & ! 7   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 8   
     &      1,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    1,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    1,    2,    2,    1,    2, & ! 1   
     &      2,    1,    1,    1,    2,    2,    1,    2,    2,    2, & ! 2   
     &      1,    2,    2,    1,    2,    2,    1,    2,    1,    2, & ! 3   
     &      2,    1,    2,    1,    1,    1,    2,    2,    1,    2, & ! 4   
     &      2,    1,    2,    2,    2,    2,    2,    2,    1,    2, & ! 5   
     &      2,    2,    2,    2,    1,    2,    2,    2,    1,    2, & ! 6   
     &      2,    2,    1,    2,    1,    2,    1,    2,    2,    2, & ! 7   
     &      1,    2,    1,    1,    2,    2,    2,    2,    2,    2, & ! 8   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 9   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! O   
     &      2,    2,    2,    2,    2,    2,    2,    2,    2,    2, & ! 1   
     &      2,    2,    2,    2,    2,    2,    2,    2,    1,    2, & ! 2   
     &      2,    2,    2,    1,    1,    2,    2,    2,    2,    1, & ! 3   
     &      2,    2,    2,    2,    2,    1,    1,    1,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1/!5   
      INTEGER            :: NPRDCT( NRXNS )

      DATA ( NPRDCT( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &      2,    1,    0,    1,    1,    1,    1,    1,    1,    1, & ! O   
     &      1,    2,    1,    2,    1,    2,    1,    1,    1,    1, & ! 1   
     &      1,    2,    2,    1,    1,    2,    1,    2,    1,    1, & ! 2   
     &      2,    1,    2,    4,    1,    1,    1,    1,    3,    1, & ! 3   
     &      1,    1,    0,    3,    1,    3,    1,    3,    2,    2, & ! 4   
     &      2,    1,    2,    3,    1,    1,    1,    1,    1,    1, & ! 5   
     &      0,    1,    1,    5,    3,    3,    1,    3,    1,    2, & ! 6   
     &      2,    3,    2,    2,    1,    1,    2,    1,    1,    2, & ! 7   
     &      3,    3,    3,    2,    1,    1,    2,    3,    2,    1, & ! 8   
     &      2,    3,    3,    3,    2,    1,    1,    2,    3,    4, & ! 9   
     &      2,    1,    2,    3,    3,    3,    2,    1,    1,    1, & ! O   
     &      3,    4,    4,    2,    1,    2,    1,    1,    1,    0, & ! 1   
     &      1,    1,    2,    1,    2,    1,    2,    3,    3,    1, & ! 2   
     &      3,    2,    5,    4,    2,    3,    2,    8,    3,    2, & ! 3   
     &      3,    3,    3,    3,    2,    2,    3,    4,    3,    2, & ! 4   
     &      3,    1,    3,    2,    3,    2,    2,    1,    0,    2, & ! 5   
     &      6,    8,    4,    1,    7,    7,    8,    2,    4,   11, & ! 6   
     &     12,    9,    7,   10,    8,   12,   11,    3,    4,    3, & ! 7   
     &      7,    3,    7,    1,    3,    5,    2,    7,    8,   12, & ! 8   
     &      5,    6,    7,   17,    7,    3,    2,   10,    9,   12, & ! 9   
     &     10,   13,    2,    2,   12,    2,    2,    8,   14,    7, & ! O   
     &      3,   11,   20,   13,    8,    1,    3,    4,    2,    2, & ! 1   
     &      2,    6,    2,    2,    6,    6,    5,    1,    7,    5, & ! 2   
     &      8,    3,    6,    0,    0,    1,    1,    1,    1,    0, & ! 3   
     &      1,    1,    1,    1,    1,    0,    1,    2,    1,    1, & ! 4   
     &      1,    1,    1,    1,    1,    1,    1,    1,    1,    1/!5   

      INTEGER, PARAMETER :: NMPHOT =  35
      INTEGER            :: IPH( NMPHOT,3 )

      DATA ( IPH( IRXXN,1 ), IRXXN = 1, NMPHOT ) / & 
     &      1,   15,   16,   17,   18,   22,   23,   28,   34,   41, & 
     &    123,  124,  131,  134,  137,  139,  142,  144,  145,  146, & 
     &    149,  152,  159,  165,  169,  173,  175,  177,  181,  183, & 
     &    229,  234,  235,  240,  246/

      DATA ( IPH( IRXXN,2 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   17,   18,   19, & 
     &     20,   21,   22,   23,   23,   23,   16,   24,   25,   23, & 
     &     23,   11,   12,   13,   23/

      DATA ( IPH( IRXXN,3 ), IRXXN = 1, NMPHOT ) / & 
     &      1,    2,    3,    4,    5,    6,    7,    8,    9,   10, & 
     &     11,   12,   13,   14,   15,   16,   17,   18,   19,   20, & 
     &     21,   22,   23,   24,   25,   26,   27,   28,   29,   30, & 
     &     31,   32,   33,   34,   35/

      INTEGER, PARAMETER :: MHETERO =   2
      INTEGER            :: IHETERO( MHETERO,2 )

      DATA ( IHETERO( IRXXN,1 ), IRXXN = 1, MHETERO ) / & 
     &    247,  248/

      DATA ( IHETERO( IRXXN,2 ), IRXXN = 1, MHETERO ) / & 
     &      1,    2/

      INTEGER, PARAMETER :: NPHOTAB =  25
      CHARACTER( 16 )    :: PHOTAB( NPHOTAB )

      DATA ( PHOTAB( IRXXN ), IRXXN = 1, NPHOTAB ) / & 
     &   'NO2_SAPRC99     ', 'NO3NO_SAPRC99   ', 'NO3NO2_SAPRC99  ', & 
     &   'O3O3P_SAPRC99   ', 'O3O1D_SAPRC99   ', 'HONO_NO_SAPRC99 ', & 
     &   'HONO_NO2_SAPRC99', 'HNO3_SAPRC99    ', 'HO2NO2_SAPRC99  ', & 
     &   'H2O2_SAPRC99    ', 'HCHO_R_SAPRC99  ', 'HCHO_M_SAPRC99  ', & 
     &   'CCHO_R_SAPRC99  ', 'C2CHO_SAPRC99   ', 'ACETONE_SAPRC99 ', & 
     &   'KETONE_SAPRC99  ', 'COOH_SAPRC99    ', 'GLY_R_SAPRC99   ', & 
     &   'GLY_ABS_SAPRC99 ', 'MGLY_ADJ_SAPRC99', 'BACL_ADJ_SAPRC99', & 
     &   'BZCHO_SAPRC99   ', 'ACROLEIN_SAPRC99', 'IC3ONO2_SAPRC99 ', & 
     &   'MGLY_ABS_SAPRC99'/

      INTEGER, PARAMETER :: NHETERO =   2
      CHARACTER( 16 )    :: HETERO( NHETERO )

      DATA ( HETERO( IRXXN ), IRXXN = 1, NHETERO ) / & 
     &   'HETERO_N2O5IJ   ', 'HETERO_NO2      '/

      CHARACTER( 16 )    :: RXLABEL( NRXNS )

      DATA ( RXLABEL( IRXXN ), IRXXN = 1, NRXNS ) / & 
     &    '1               ', '2               ', '3               ', & ! 0   
     &    '4               ', '5               ', '6               ', & ! 1   
     &    '8               ', '9               ', '10              ', & ! 2   
     &    '11              ', '12              ', '13              ', & ! 3   
     &    '14              ', '17              ', '18              ', & ! 4   
     &    '19              ', '20              ', '21              ', & ! 5   
     &    '22              ', '23              ', '24              ', & ! 6   
     &    '25              ', '26              ', '27              ', & ! 7   
     &    '28              ', '29              ', '30              ', & ! 8   
     &    '31              ', '32              ', '33              ', & ! 9   
     &    '34              ', '35              ', '36              ', & ! 0   
     &    '37              ', '38              ', '39              ', & ! 1   
     &    '40A             ', '40B             ', '41              ', & ! 2   
     &    '42              ', '43              ', '44              ', & ! 3   
     &    '45              ', 'S2OH            ', 'H2OH            ', & ! 4   
     &    'MER1            ', 'MER4            ', 'MEN3            ', & ! 5   
     &    'MER5            ', 'MER6            ', 'RRNO            ', & ! 6   
     &    'RRH2            ', 'RRN3            ', 'RRME            ', & ! 7   
     &    'RRR2            ', 'R2NO            ', 'R2H2            ', & ! 8   
     &    'R2N3            ', 'R2ME            ', 'R2RR            ', & ! 9   
     &    'R2R3            ', 'RNNO            ', 'RNH2            ', & ! 0   
     &    'RNME            ', 'RNN3            ', 'RNRR            ', & ! 1   
     &    'RNR2            ', 'RNRN            ', 'APN2            ', & ! 2   
     &    'DPAN            ', 'APNO            ', 'APH2            ', & ! 3   
     &    'APN3            ', 'APME            ', 'APRR            ', & ! 4   
     &    'APR2            ', 'APRN            ', 'APAP            ', & ! 5   
     &    'PPN2            ', 'PAN2            ', 'PPNO            ', & ! 6   
     &    'PPH2            ', 'PPN3            ', 'PPME            ', & ! 7   
     &    'PPRR            ', 'PPR2            ', 'PPRN            ', & ! 8   
     &    'PPAP            ', 'PPPP            ', 'BPN2            ', & ! 9   
     &    'BPAN            ', 'BPNO            ', 'BPH2            ', & ! 0   
     &    'BPN3            ', 'BPME            ', 'BPRR            ', & ! 1   
     &    'BPR2            ', 'BPRN            ', 'BPAP            ', & ! 2   
     &    'BPPP            ', 'BPBP            ', 'MPN2            ', & ! 3   
     &    'MPPN            ', 'MPNO            ', 'MPH2            ', & ! 4   
     &    'MPN3            ', 'MPME            ', 'MPRR            ', & ! 5   
     &    'MPR2            ', 'MPRN            ', 'MPAP            ', & ! 6   
     &    'MPPP            ', 'MPBP            ', 'MPMP            ', & ! 7   
     &    'TBON            ', 'TBOD            ', 'BRN2            ', & ! 8   
     &    'BRH2            ', 'BRXX            ', 'BNN2            ', & ! 9   
     &    'BNH2            ', 'BNXX            ', 'FAHV            ', & ! 0   
     &    'FAVS            ', 'FAOH            ', 'FAH2            ', & ! 1   
     &    'FAHR            ', 'FAHN            ', 'FAN3            ', & ! 2   
     &    'AAOH            ', 'AAHV            ', 'AAN3            ', & ! 3   
     &    'PAOH            ', 'PAHV            ', 'PAN3            ', & ! 4   
     &    'K3OH            ', 'K3HV            ', 'K4OH            ', & ! 5   
     &    'K4HV            ', 'MeOH            ', 'MER9            ', & ! 6   
     &    'MERA            ', 'LPR9            ', 'LPRA            ', & ! 7   
     &    'GLHV            ', 'GLVM            ', 'GLOH            ', & ! 8   
     &    'GLN3            ', 'MGHV            ', 'MGOH            ', & ! 9   
     &    'MGN3            ', 'BAHV            ', 'PHOH            ', & ! 0   
     &    'PHN3            ', 'CROH            ', 'CRN3            ', & ! 1   
     &    'NPN3            ', 'BZOH            ', 'BZHV            ', & ! 2   
     &    'BZNT            ', 'MAOH            ', 'MAO3            ', & ! 3   
     &    'MAN3            ', 'MAOP            ', 'MAHV            ', & ! 4   
     &    'MVOH            ', 'MVO3            ', 'MVOP            ', & ! 5   
     &    'MVHV            ', 'IPOH            ', 'IPO3            ', & ! 6   
     &    'IPN3            ', 'IPHV            ', 'K6OH            ', & ! 7   
     &    'K6HV            ', 'RNOH            ', 'RNHV            ', & ! 8   
     &    'D1OH            ', 'D1O3            ', 'D2OH            ', & ! 9   
     &    'D2HV            ', 'D3OH            ', 'D3HV            ', & ! 0   
     &    'c1OH            ', 'etOH            ', 'etO3            ', & ! 1   
     &    'etN3            ', 'etOA            ', 'isOH            ', & ! 2   
     &    'isO3            ', 'isN3            ', 'isOP            ', & ! 3   
     &    't1OH            ', 't1O3            ', 't1N3            ', & ! 4   
     &    't1OP            ', 'a1OH            ', 'a2OH            ', & ! 5   
     &    'a3OH            ', 'a4OH            ', 'a5OH            ', & ! 6   
     &    'b1OH            ', 'AR1N            ', 'AR1H            ', & ! 7   
     &    'b2OH            ', 'AR2N            ', 'AR2H            ', & ! 8   
     &    'o1OH            ', 'o1O3            ', 'o1N3            ', & ! 9   
     &    'o1OP            ', 'o2OH            ', 'o2O3            ', & ! 0   
     &    'o2N3            ', 'o2OP            ', 'I1OH            ', & ! 1   
     &    'I2OH            ', 'I3OH            ', 'SSO3            ', & ! 2   
     &    'SSOH            ', 'SSN3            ', 'BENZ            ', & ! 3   
     &    'BNZN            ', 'BNZH            ', 'acOH            ', & ! 4   
     &    'acO3            ', 'acN3            ', 'acOP            ', & ! 5   
     &    'acHV            ', 'bdOH            ', 'bdO3            ', & ! 6   
     &    'bdN3            ', 'bdOP            ', 'P1              ', & ! 7   
     &    'P2              ', 'P3              ', 'P4              ', & ! 8   
     &    'P5              ', 'P6              ', 'P7              ', & ! 9   
     &    'P8              ', 'P10             ', 'P11             ', & ! 0   
     &    'P12             ', 'P13             ', 'P14             ', & ! 1   
     &    'HET_N2O5        ', 'HET_N02         ', 'OLIG_ALKENE     ', & ! 2   
     &    'OLIG_XYLENE1    ', 'OLIG_XYLENE2    ', 'OLIG_TOLUENE1   ', & ! 3   
     &    'OLIG_TOLUENE2   ', 'OLIG_BENZENE1   ', 'OLIG_BENZENE2   ', & ! 4   
     &    'OLIG_TERPENE1   ', 'OLIG_TERPENE2   ', 'OLIG_ISOPRENE1  ', & ! 5   
     &    'OLIG_ISOPRENE2  ', 'OLIG_SESQT1     '/                   ! 6  

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
       INTEGER, PARAMETER  :: IJ_NO2_SAPRC99      =   1
       INTEGER, PARAMETER  :: IJ_NO3NO_SAPRC99    =   2
       INTEGER, PARAMETER  :: IJ_NO3NO2_SAPRC99   =   3
       INTEGER, PARAMETER  :: IJ_O3O3P_SAPRC99    =   4
       INTEGER, PARAMETER  :: IJ_O3O1D_SAPRC99    =   5
       INTEGER, PARAMETER  :: IJ_HONO_NO_SAPRC99  =   6
       INTEGER, PARAMETER  :: IJ_HONO_NO2_SAPRC99 =   7
       INTEGER, PARAMETER  :: IJ_HNO3_SAPRC99     =   8
       INTEGER, PARAMETER  :: IJ_HO2NO2_SAPRC99   =   9
       INTEGER, PARAMETER  :: IJ_H2O2_SAPRC99     =  10
       INTEGER, PARAMETER  :: IJ_HCHO_R_SAPRC99   =  11
       INTEGER, PARAMETER  :: IJ_HCHO_M_SAPRC99   =  12
       INTEGER, PARAMETER  :: IJ_CCHO_R_SAPRC99   =  13
       INTEGER, PARAMETER  :: IJ_C2CHO_SAPRC99    =  14
       INTEGER, PARAMETER  :: IJ_ACETONE_SAPRC99  =  15
       INTEGER, PARAMETER  :: IJ_KETONE_SAPRC99   =  16
       INTEGER, PARAMETER  :: IJ_COOH_SAPRC99     =  17
       INTEGER, PARAMETER  :: IJ_GLY_R_SAPRC99    =  18
       INTEGER, PARAMETER  :: IJ_GLY_ABS_SAPRC99  =  19
       INTEGER, PARAMETER  :: IJ_MGLY_ADJ_SAPRC99 =  20
       INTEGER, PARAMETER  :: IJ_BACL_ADJ_SAPRC99 =  21
       INTEGER, PARAMETER  :: IJ_BZCHO_SAPRC99    =  22
       INTEGER, PARAMETER  :: IJ_ACROLEIN_SAPRC99 =  23
       INTEGER, PARAMETER  :: IJ_IC3ONO2_SAPRC99  =  24
       INTEGER, PARAMETER  :: IJ_MGLY_ABS_SAPRC99 =  25
       INTEGER, PARAMETER  :: IK_HETERO_N2O5IJ    =   1
       INTEGER, PARAMETER  :: IK_HETERO_NO2       =   2
       END MODULE RXNS_DATA
