
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

      MODULE  HRDATA

C*************************************************************************
C
C  FUNCTION:  Mechanism & solver data for EBI solver
C
C  PRECONDITIONS: For the CB6R5M_AE7_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Apr 21, 2022
C
C   18 Jul 14 B.Hutzell: revised to use real(8) variables for solution
C*************************************************************************

c..EBI solver fixed parameters
      INTEGER, PARAMETER    ::  NEBITER       = 1000      ! No. of iterations for EBI
      INTEGER, PARAMETER    ::  MAX_CELLS_EBI = 1         ! max # cells in local conc arrays
      REAL( 8 ), PARAMETER  ::  DELTAT        = 1.250D+00 ! EBI time step (min)

c..Mechanism specific variables
      INTEGER   N_SPEC       ! No. of species in mechanism
      INTEGER   N_RXNS       ! No. of reactions in mechanism
      INTEGER   N_EBISP      ! No. of species solved by EBI
      INTEGER   NING1        ! No. of species in group 1
      INTEGER   NING2        ! No. of species in group 2
      INTEGER   N_SPCL_RKS   ! No. of special rate constants
      INTEGER   N_ACTSP      ! No. of species in mechanism (non-ss spec)
      INTEGER   NCELL        ! cell number in EBI Block

c..Control flags
      LOGICAL   L_AE_VRSN    ! Flag for aerosol version of mech
      LOGICAL   L_AQ_VRSN    ! Flag for aqueous chemistry version of mech
      LOGICAL   CALL_IRR     ! Flag initializing for IRR and calling PA_IRR

  ! SWITCH for calling DEGRADE routine
      LOGICAL, SAVE :: CALL_DEG = .TRUE.

c..Miscellaneous variables
      INTEGER   N_EBI_IT      ! No. of iterations for EBI
      INTEGER   N_EBI_STEPS   ! No. of time steps for EBI
      INTEGER   N_INR_STEPS   ! No. of inner time steps for EBI
      INTEGER   PECOL_OFFSET  ! Local Column Offset for processor
      INTEGER   PEROW_OFFSET  ! Local Column Offset for processor
  
      INTEGER, SAVE ::   MODEL_SPECIES ! No. of species in CGRID

      REAL( 8 )    EBI_TMSTEP          ! Time step for EBI loops (min)
      REAL( 8 )    CHEMSTEP            ! Chemistry integration interval (min)

c...Allocatable arrays
      INTEGER, ALLOCATABLE :: EBISP( : )         ! Index of EBI species
      INTEGER, ALLOCATABLE :: SSASP( : )         ! Index of ss species
      INTEGER, ALLOCATABLE :: ACTSP( : )         ! Index of active species
      INTEGER, ALLOCATABLE :: IOLD2NEW( :,: )    ! YC species map, used if NSPECIAL > 0

      REAL( 8 ), ALLOCATABLE ::  RKI(  : )     ! Rate constants
      REAL( 8 ), ALLOCATABLE ::  RKI_SAV(  :,  : )  ! Saved values of RKI for SS species calcs

      REAL( 8 ), ALLOCATABLE ::  YC (  : )      ! Species concentrations
      REAL( 8 ), ALLOCATABLE ::  YC0(  : )      ! Species concentrations
      REAL( 8 ), ALLOCATABLE ::  YCP(  : )      ! Species concentrations
      REAL( 8 ), ALLOCATABLE ::  SYC(  :, : )   ! Species concentrations

      REAL, ALLOCATABLE      :: FORWARD_CONV( : )  ! CGRID to CHEM Species conversion factor
      REAL, ALLOCATABLE      :: REVERSE_CONV( : )  ! CHEM to CGRID Species conversion factor

      REAL( 8 ), ALLOCATABLE ::  RXRAT( : )     ! Reaction rates
      REAL( 8 ), ALLOCATABLE ::  RTOL(  : )        ! Species tolerances
      REAL( 8 ), ALLOCATABLE ::  YCCELL( : )       ! ALL Species in cgrid
      REAL( 8 ), ALLOCATABLE ::  PROD(  : )     ! Prod of species
      REAL( 8 ), ALLOCATABLE ::  LOSS(  : )     ! Loss of species
      REAL( 8 ), ALLOCATABLE ::  PNEG(  : )! Negative production rates

      REAL, ALLOCATABLE      :: PRES( :, :, : )          ! pressure (Pa)
      REAL, ALLOCATABLE      :: QV  ( :, :, : )          ! water vapor (Kg/Kg air)
      REAL, ALLOCATABLE      :: TA  ( :, :, : )          ! temperature (K)

      REAL, ALLOCATABLE      :: SEAICE ( :, :)            ! fractional seaice cover, [-]
      REAL, ALLOCATABLE      :: DENS( :, :, : )           ! mass density (Kg/m^3)

      REAL, ALLOCATABLE      :: SEAWATER_ZONE   ( :,: )       ! fractional area of OPEN+SURF
      LOGICAL, ALLOCATABLE   :: LFLAGIRR    ( :,:,: )     ! Conduct IRR for cell

      REAL( 8 ), ALLOCATABLE ::  ATMPRES ( : )     ! Cell pressure, Atm
!      REAL( 8 ), ALLOCATABLE ::  PRESCELL( : )         ! Cell pressure, Pa
      REAL( 8 ), ALLOCATABLE ::  H2O     ( : )     ! Cell H2O mixing ratio (ppmV)
      REAL( 8 ), ALLOCATABLE ::  TEMP    ( : )     ! Cell Temperature
      REAL( 8 ), ALLOCATABLE ::  DENSITY ( : )     ! Cell mass density (Kg/m3)
      REAL( 8 ), ALLOCATABLE ::  HET     ( :, : )  ! cell heterogeneous reaction rates
      REAL( 8 ), ALLOCATABLE ::  RJIN    ( :, : )  ! J-values for a cell
      REAL( 8 ), ALLOCATABLE ::  SEAWATER ( : )    ! fractional area of OPEN+SURF for specific cell

c..Species indices
      INTEGER, PARAMETER :: NO2              =    1
      INTEGER, PARAMETER :: NO               =    2
      INTEGER, PARAMETER :: O                =    3
      INTEGER, PARAMETER :: O3               =    4
      INTEGER, PARAMETER :: NO3              =    5
      INTEGER, PARAMETER :: O1D              =    6
      INTEGER, PARAMETER :: OH               =    7
      INTEGER, PARAMETER :: HO2              =    8
      INTEGER, PARAMETER :: H2O2             =    9
      INTEGER, PARAMETER :: N2O5             =   10
      INTEGER, PARAMETER :: HNO3             =   11
      INTEGER, PARAMETER :: HONO             =   12
      INTEGER, PARAMETER :: PNA              =   13
      INTEGER, PARAMETER :: SO2              =   14
      INTEGER, PARAMETER :: SULF             =   15
      INTEGER, PARAMETER :: SULRXN           =   16
      INTEGER, PARAMETER :: C2O3             =   17
      INTEGER, PARAMETER :: MEO2             =   18
      INTEGER, PARAMETER :: RO2              =   19
      INTEGER, PARAMETER :: PAN              =   20
      INTEGER, PARAMETER :: PACD             =   21
      INTEGER, PARAMETER :: AACD             =   22
      INTEGER, PARAMETER :: CXO3             =   23
      INTEGER, PARAMETER :: ALD2             =   24
      INTEGER, PARAMETER :: XO2H             =   25
      INTEGER, PARAMETER :: PANX             =   26
      INTEGER, PARAMETER :: FORM             =   27
      INTEGER, PARAMETER :: MEPX             =   28
      INTEGER, PARAMETER :: MEOH             =   29
      INTEGER, PARAMETER :: ROOH             =   30
      INTEGER, PARAMETER :: XO2              =   31
      INTEGER, PARAMETER :: XO2N             =   32
      INTEGER, PARAMETER :: NTR1             =   33
      INTEGER, PARAMETER :: NTR2             =   34
      INTEGER, PARAMETER :: FACD             =   35
      INTEGER, PARAMETER :: CO               =   36
      INTEGER, PARAMETER :: HCO3             =   37
      INTEGER, PARAMETER :: ALDX             =   38
      INTEGER, PARAMETER :: GLYD             =   39
      INTEGER, PARAMETER :: GLY              =   40
      INTEGER, PARAMETER :: MGLY             =   41
      INTEGER, PARAMETER :: ETHA             =   42
      INTEGER, PARAMETER :: ETOH             =   43
      INTEGER, PARAMETER :: KET              =   44
      INTEGER, PARAMETER :: PAR              =   45
      INTEGER, PARAMETER :: ACET             =   46
      INTEGER, PARAMETER :: PRPA             =   47
      INTEGER, PARAMETER :: XPRP             =   48
      INTEGER, PARAMETER :: XPAR             =   49
      INTEGER, PARAMETER :: ROR              =   50
      INTEGER, PARAMETER :: ETHY             =   51
      INTEGER, PARAMETER :: ETH              =   52
      INTEGER, PARAMETER :: OLE              =   53
      INTEGER, PARAMETER :: IOLE             =   54
      INTEGER, PARAMETER :: ISOP             =   55
      INTEGER, PARAMETER :: ISO2             =   56
      INTEGER, PARAMETER :: ISOPRXN          =   57
      INTEGER, PARAMETER :: ISPD             =   58
      INTEGER, PARAMETER :: INTR             =   59
      INTEGER, PARAMETER :: ISPX             =   60
      INTEGER, PARAMETER :: HPLD             =   61
      INTEGER, PARAMETER :: OPO3             =   62
      INTEGER, PARAMETER :: EPOX             =   63
      INTEGER, PARAMETER :: EPX2             =   64
      INTEGER, PARAMETER :: TERP             =   65
      INTEGER, PARAMETER :: TRPRXN           =   66
      INTEGER, PARAMETER :: TERPNRO2         =   67
      INTEGER, PARAMETER :: APIN             =   68
      INTEGER, PARAMETER :: BENZENE          =   69
      INTEGER, PARAMETER :: CRES             =   70
      INTEGER, PARAMETER :: BZO2             =   71
      INTEGER, PARAMETER :: OPEN             =   72
      INTEGER, PARAMETER :: BENZRO2          =   73
      INTEGER, PARAMETER :: TOL              =   74
      INTEGER, PARAMETER :: TO2              =   75
      INTEGER, PARAMETER :: TOLRO2           =   76
      INTEGER, PARAMETER :: XOPN             =   77
      INTEGER, PARAMETER :: XYLMN            =   78
      INTEGER, PARAMETER :: XLO2             =   79
      INTEGER, PARAMETER :: XYLRO2           =   80
      INTEGER, PARAMETER :: NAPH             =   81
      INTEGER, PARAMETER :: PAHRO2           =   82
      INTEGER, PARAMETER :: CRO              =   83
      INTEGER, PARAMETER :: CAT1             =   84
      INTEGER, PARAMETER :: CRON             =   85
      INTEGER, PARAMETER :: OPAN             =   86
      INTEGER, PARAMETER :: ECH4             =   87
      INTEGER, PARAMETER :: CL2              =   88
      INTEGER, PARAMETER :: CL               =   89
      INTEGER, PARAMETER :: HOCL             =   90
      INTEGER, PARAMETER :: CLO              =   91
      INTEGER, PARAMETER :: FMCL             =   92
      INTEGER, PARAMETER :: HCL              =   93
      INTEGER, PARAMETER :: CLNO2            =   94
      INTEGER, PARAMETER :: CLNO3            =   95
      INTEGER, PARAMETER :: SVAVB2           =   96
      INTEGER, PARAMETER :: SVAVB3           =   97
      INTEGER, PARAMETER :: SVAVB4           =   98
      INTEGER, PARAMETER :: SVAVB1           =   99
      INTEGER, PARAMETER :: SESQ             =  100
      INTEGER, PARAMETER :: SESQRXN          =  101
      INTEGER, PARAMETER :: SOAALK           =  102
      INTEGER, PARAMETER :: H2NO3PIJ         =  103
      INTEGER, PARAMETER :: H2NO3PK          =  104
      INTEGER, PARAMETER :: ACLI             =  105
      INTEGER, PARAMETER :: ACLJ             =  106
      INTEGER, PARAMETER :: ACLK             =  107
      INTEGER, PARAMETER :: IEPOXP           =  108
      INTEGER, PARAMETER :: ASO4J            =  109
      INTEGER, PARAMETER :: AISO3J           =  110
      INTEGER, PARAMETER :: AGLYJ            =  111
      INTEGER, PARAMETER :: MTNO3            =  112
      INTEGER, PARAMETER :: AMTNO3J          =  113
      INTEGER, PARAMETER :: AMTHYDJ          =  114
      INTEGER, PARAMETER :: AAVB2J           =  115
      INTEGER, PARAMETER :: AOLGAJ           =  116
      INTEGER, PARAMETER :: AAVB3J           =  117
      INTEGER, PARAMETER :: AAVB4J           =  118
      INTEGER, PARAMETER :: AISO1J           =  119
      INTEGER, PARAMETER :: AOLGBJ           =  120
      INTEGER, PARAMETER :: AISO2J           =  121
      INTEGER, PARAMETER :: ASQTJ            =  122
      INTEGER, PARAMETER :: APOCI            =  123
      INTEGER, PARAMETER :: APNCOMI          =  124
      INTEGER, PARAMETER :: APOCJ            =  125
      INTEGER, PARAMETER :: APNCOMJ          =  126
      INTEGER, PARAMETER :: PCVOC            =  127
      INTEGER, PARAMETER :: PCSOARXN         =  128
      INTEGER, PARAMETER :: VLVPO1           =  129
      INTEGER, PARAMETER :: VSVPO1           =  130
      INTEGER, PARAMETER :: VSVPO2           =  131
      INTEGER, PARAMETER :: VSVPO3           =  132
      INTEGER, PARAMETER :: VIVPO1           =  133
      INTEGER, PARAMETER :: VLVOO1           =  134
      INTEGER, PARAMETER :: VLVOO2           =  135
      INTEGER, PARAMETER :: VSVOO2           =  136
      INTEGER, PARAMETER :: VSVOO3           =  137
      INTEGER, PARAMETER :: VSVOO1           =  138
      INTEGER, PARAMETER :: FORM_PRIMARY     =  139
      INTEGER, PARAMETER :: ALD2_PRIMARY     =  140
      INTEGER, PARAMETER :: BUTADIENE13      =  141
      INTEGER, PARAMETER :: ACROLEIN         =  142
      INTEGER, PARAMETER :: ACRO_PRIMARY     =  143
      INTEGER, PARAMETER :: TOLU             =  144
      INTEGER, PARAMETER :: HG               =  145
      INTEGER, PARAMETER :: HGIIAER          =  146
      INTEGER, PARAMETER :: HGIIGAS          =  147
      INTEGER, PARAMETER :: BR               =  148
      INTEGER, PARAMETER :: BRO              =  149
      INTEGER, PARAMETER :: HOBR             =  150
      INTEGER, PARAMETER :: HBR              =  151
      INTEGER, PARAMETER :: BR2              =  152
      INTEGER, PARAMETER :: BRNO3            =  153
      INTEGER, PARAMETER :: BRNO2            =  154
      INTEGER, PARAMETER :: FMBR             =  155
      INTEGER, PARAMETER :: MB3              =  156
      INTEGER, PARAMETER :: MB2              =  157
      INTEGER, PARAMETER :: MB2C             =  158
      INTEGER, PARAMETER :: MBC2             =  159
      INTEGER, PARAMETER :: MBC              =  160
      INTEGER, PARAMETER :: DMS              =  161
      INTEGER, PARAMETER :: MSA              =  162
      INTEGER, PARAMETER :: BRCL             =  163
      INTEGER, PARAMETER :: ABRJ             =  164
      INTEGER, PARAMETER :: I                =  165
      INTEGER, PARAMETER :: IO               =  166
      INTEGER, PARAMETER :: HI               =  167
      INTEGER, PARAMETER :: I2               =  168
      INTEGER, PARAMETER :: HOI              =  169
      INTEGER, PARAMETER :: INO              =  170
      INTEGER, PARAMETER :: INO2             =  171
      INTEGER, PARAMETER :: INO3             =  172
      INTEGER, PARAMETER :: OIO              =  173
      INTEGER, PARAMETER :: I2O4             =  174
      INTEGER, PARAMETER :: I2O2             =  175
      INTEGER, PARAMETER :: I2O3             =  176
      INTEGER, PARAMETER :: CH3I             =  177
      INTEGER, PARAMETER :: ICL              =  178
      INTEGER, PARAMETER :: IBR              =  179
      INTEGER, PARAMETER :: MI2              =  180
      INTEGER, PARAMETER :: MIB              =  181
      INTEGER, PARAMETER :: MIC              =  182

      END MODULE HRDATA
