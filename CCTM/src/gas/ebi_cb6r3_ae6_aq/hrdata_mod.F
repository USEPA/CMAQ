
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
C  PRECONDITIONS: For the cb6r3 family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Aug 29, 2018
C
C   18 Jul 14 B.Hutzell: revised to use real(8) variables for solution
C*************************************************************************

c..EBI solver fixed parameters
      INTEGER, PARAMETER    ::  NEBITER       = 1000     ! No. of iterations for EBI
      INTEGER, PARAMETER    ::  MAX_CELLS_EBI = 1        ! max # cells in local conc arrays
      REAL( 8 ), PARAMETER  ::  DELTAT        = 2.5D+00  ! EBI time step

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

      REAL,    ALLOCATABLE   :: OPEN_ZONE   ( :,: )       ! fractional sea water cover, [-]
      REAL,    ALLOCATABLE   :: SURF_ZONE   ( :,: )       ! fractional surf-zone cover, [-]
      LOGICAL, ALLOCATABLE   :: LAND_ZONE   ( :,: )       ! Is the surface totally land?
      LOGICAL, ALLOCATABLE   :: LFLAGIRR    ( :,:,: )     ! Conduct IRR for cell

      REAL( 8 ), ALLOCATABLE ::  ATMPRES ( : )     ! Cell pressure, Atm
!      REAL( 8 ), ALLOCATABLE ::  PRESCELL( : )         ! Cell pressure, Pa
      REAL( 8 ), ALLOCATABLE ::  H2O     ( : )     ! Cell H2O mixing ratio (ppmV)
      REAL( 8 ), ALLOCATABLE ::  TEMP    ( : )     ! Cell Temperature
      REAL( 8 ), ALLOCATABLE ::  HET     ( :, : )  ! cell heterogeneous reaction rates
      REAL( 8 ), ALLOCATABLE ::  RJIN    ( :, : )  ! J-values for a cell
      LOGICAL, ALLOCATABLE   ::  LAND    ( : )     ! land_zone value for specific cell

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
      INTEGER, PARAMETER :: BENZENE          =   67
      INTEGER, PARAMETER :: CRES             =   68
      INTEGER, PARAMETER :: BZO2             =   69
      INTEGER, PARAMETER :: OPEN             =   70
      INTEGER, PARAMETER :: BENZRO2          =   71
      INTEGER, PARAMETER :: TOL              =   72
      INTEGER, PARAMETER :: TO2              =   73
      INTEGER, PARAMETER :: TOLRO2           =   74
      INTEGER, PARAMETER :: XOPN             =   75
      INTEGER, PARAMETER :: XYLMN            =   76
      INTEGER, PARAMETER :: XLO2             =   77
      INTEGER, PARAMETER :: XYLRO2           =   78
      INTEGER, PARAMETER :: NAPH             =   79
      INTEGER, PARAMETER :: PAHRO2           =   80
      INTEGER, PARAMETER :: CRO              =   81
      INTEGER, PARAMETER :: CAT1             =   82
      INTEGER, PARAMETER :: CRON             =   83
      INTEGER, PARAMETER :: OPAN             =   84
      INTEGER, PARAMETER :: ECH4             =   85
      INTEGER, PARAMETER :: CL2              =   86
      INTEGER, PARAMETER :: CL               =   87
      INTEGER, PARAMETER :: HOCL             =   88
      INTEGER, PARAMETER :: CLO              =   89
      INTEGER, PARAMETER :: FMCL             =   90
      INTEGER, PARAMETER :: HCL              =   91
      INTEGER, PARAMETER :: CLNO2            =   92
      INTEGER, PARAMETER :: CLNO3            =   93
      INTEGER, PARAMETER :: TOLNRXN          =   94
      INTEGER, PARAMETER :: TOLHRXN          =   95
      INTEGER, PARAMETER :: XYLNRXN          =   96
      INTEGER, PARAMETER :: XYLHRXN          =   97
      INTEGER, PARAMETER :: BNZNRXN          =   98
      INTEGER, PARAMETER :: BNZHRXN          =   99
      INTEGER, PARAMETER :: SESQ             =  100
      INTEGER, PARAMETER :: SESQRXN          =  101
      INTEGER, PARAMETER :: PAHNRXN          =  102
      INTEGER, PARAMETER :: PAHHRXN          =  103
      INTEGER, PARAMETER :: SOAALK           =  104
      INTEGER, PARAMETER :: ALKRXN           =  105
      INTEGER, PARAMETER :: H2NO3PIJ         =  106
      INTEGER, PARAMETER :: H2NO3PK          =  107
      INTEGER, PARAMETER :: ACLI             =  108
      INTEGER, PARAMETER :: ACLJ             =  109
      INTEGER, PARAMETER :: ACLK             =  110
      INTEGER, PARAMETER :: AISO3J           =  111
      INTEGER, PARAMETER :: AGLYJ            =  112
      INTEGER, PARAMETER :: AXYL1J           =  113
      INTEGER, PARAMETER :: AOLGAJ           =  114
      INTEGER, PARAMETER :: AXYL2J           =  115
      INTEGER, PARAMETER :: ATOL1J           =  116
      INTEGER, PARAMETER :: ATOL2J           =  117
      INTEGER, PARAMETER :: ABNZ1J           =  118
      INTEGER, PARAMETER :: ABNZ2J           =  119
      INTEGER, PARAMETER :: ATRP1J           =  120
      INTEGER, PARAMETER :: AOLGBJ           =  121
      INTEGER, PARAMETER :: ATRP2J           =  122
      INTEGER, PARAMETER :: AISO1J           =  123
      INTEGER, PARAMETER :: AISO2J           =  124
      INTEGER, PARAMETER :: ASQTJ            =  125
      INTEGER, PARAMETER :: APAH1J           =  126
      INTEGER, PARAMETER :: APAH2J           =  127
      INTEGER, PARAMETER :: AALK1J           =  128
      INTEGER, PARAMETER :: AALK2J           =  129
      INTEGER, PARAMETER :: PCVOC            =  130
      INTEGER, PARAMETER :: PCSOARXN         =  131
      INTEGER, PARAMETER :: VLVPO1           =  132
      INTEGER, PARAMETER :: VSVPO1           =  133
      INTEGER, PARAMETER :: VSVPO2           =  134
      INTEGER, PARAMETER :: VSVPO3           =  135
      INTEGER, PARAMETER :: VIVPO1           =  136
      INTEGER, PARAMETER :: VLVOO1           =  137
      INTEGER, PARAMETER :: VLVOO2           =  138
      INTEGER, PARAMETER :: VSVOO2           =  139
      INTEGER, PARAMETER :: VSVOO3           =  140
      INTEGER, PARAMETER :: VSVOO1           =  141
      INTEGER, PARAMETER :: FORM_PRIMARY     =  142
      INTEGER, PARAMETER :: ALD2_PRIMARY     =  143
      INTEGER, PARAMETER :: BUTADIENE13      =  144
      INTEGER, PARAMETER :: ACROLEIN         =  145
      INTEGER, PARAMETER :: ACRO_PRIMARY     =  146
      INTEGER, PARAMETER :: TOLU             =  147
      INTEGER, PARAMETER :: HG               =  148
      INTEGER, PARAMETER :: HGIIAER          =  149
      INTEGER, PARAMETER :: HGIIGAS          =  150

      END MODULE HRDATA
