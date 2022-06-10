
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
C  PRECONDITIONS: For the RACM2_AE6_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Jun  8, 2022
C
C   18 Jul 14 B.Hutzell: revised to use real(8) variables for solution
C*************************************************************************

c..EBI solver fixed parameters
      INTEGER, PARAMETER    ::  NEBITER       = 1000      ! No. of iterations for EBI
      INTEGER, PARAMETER    ::  MAX_CELLS_EBI = 1         ! max # cells in local conc arrays
      REAL( 8 ), PARAMETER  ::  DELTAT        = 2.500D+00 ! EBI time step (min)

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
      INTEGER, PARAMETER :: O3               =    1
      INTEGER, PARAMETER :: O3P              =    2
      INTEGER, PARAMETER :: O1D              =    3
      INTEGER, PARAMETER :: H2O2             =    4
      INTEGER, PARAMETER :: HO               =    5
      INTEGER, PARAMETER :: NO2              =    6
      INTEGER, PARAMETER :: NO               =    7
      INTEGER, PARAMETER :: NO3              =    8
      INTEGER, PARAMETER :: HONO             =    9
      INTEGER, PARAMETER :: HNO3             =   10
      INTEGER, PARAMETER :: HNO4             =   11
      INTEGER, PARAMETER :: HO2              =   12
      INTEGER, PARAMETER :: HCHO             =   13
      INTEGER, PARAMETER :: CO               =   14
      INTEGER, PARAMETER :: ACD              =   15
      INTEGER, PARAMETER :: MO2              =   16
      INTEGER, PARAMETER :: ALD              =   17
      INTEGER, PARAMETER :: ETHP             =   18
      INTEGER, PARAMETER :: ACT              =   19
      INTEGER, PARAMETER :: ACO3             =   20
      INTEGER, PARAMETER :: UALD             =   21
      INTEGER, PARAMETER :: KET              =   22
      INTEGER, PARAMETER :: MEK              =   23
      INTEGER, PARAMETER :: HKET             =   24
      INTEGER, PARAMETER :: MACR             =   25
      INTEGER, PARAMETER :: MACP             =   26
      INTEGER, PARAMETER :: XO2              =   27
      INTEGER, PARAMETER :: MVK              =   28
      INTEGER, PARAMETER :: GLY              =   29
      INTEGER, PARAMETER :: MGLY             =   30
      INTEGER, PARAMETER :: DCB1             =   31
      INTEGER, PARAMETER :: DCB2             =   32
      INTEGER, PARAMETER :: BALD             =   33
      INTEGER, PARAMETER :: CHO              =   34
      INTEGER, PARAMETER :: OP1              =   35
      INTEGER, PARAMETER :: OP2              =   36
      INTEGER, PARAMETER :: PAA              =   37
      INTEGER, PARAMETER :: ONIT             =   38
      INTEGER, PARAMETER :: PAN              =   39
      INTEGER, PARAMETER :: N2O5             =   40
      INTEGER, PARAMETER :: SO2              =   41
      INTEGER, PARAMETER :: SULF             =   42
      INTEGER, PARAMETER :: SULRXN           =   43
      INTEGER, PARAMETER :: ETH              =   44
      INTEGER, PARAMETER :: HC3              =   45
      INTEGER, PARAMETER :: HC3P             =   46
      INTEGER, PARAMETER :: HC5              =   47
      INTEGER, PARAMETER :: HC5P             =   48
      INTEGER, PARAMETER :: HC8              =   49
      INTEGER, PARAMETER :: HC8P             =   50
      INTEGER, PARAMETER :: ETE              =   51
      INTEGER, PARAMETER :: ETEP             =   52
      INTEGER, PARAMETER :: OLT              =   53
      INTEGER, PARAMETER :: OLTP             =   54
      INTEGER, PARAMETER :: OLI              =   55
      INTEGER, PARAMETER :: OLIP             =   56
      INTEGER, PARAMETER :: DIEN             =   57
      INTEGER, PARAMETER :: ACE              =   58
      INTEGER, PARAMETER :: ORA1             =   59
      INTEGER, PARAMETER :: BENZENE          =   60
      INTEGER, PARAMETER :: BENP             =   61
      INTEGER, PARAMETER :: EPX              =   62
      INTEGER, PARAMETER :: PHEN             =   63
      INTEGER, PARAMETER :: BENZRO2          =   64
      INTEGER, PARAMETER :: TOL              =   65
      INTEGER, PARAMETER :: TR2              =   66
      INTEGER, PARAMETER :: TLP1             =   67
      INTEGER, PARAMETER :: CSL              =   68
      INTEGER, PARAMETER :: TOLRO2           =   69
      INTEGER, PARAMETER :: XYM              =   70
      INTEGER, PARAMETER :: XY2              =   71
      INTEGER, PARAMETER :: XYL1             =   72
      INTEGER, PARAMETER :: XYLRO2           =   73
      INTEGER, PARAMETER :: XYP              =   74
      INTEGER, PARAMETER :: XYO              =   75
      INTEGER, PARAMETER :: XYO2             =   76
      INTEGER, PARAMETER :: ISO              =   77
      INTEGER, PARAMETER :: ISOP             =   78
      INTEGER, PARAMETER :: ISOPRXN          =   79
      INTEGER, PARAMETER :: API              =   80
      INTEGER, PARAMETER :: APIP             =   81
      INTEGER, PARAMETER :: TRPRXN           =   82
      INTEGER, PARAMETER :: LIM              =   83
      INTEGER, PARAMETER :: LIMP             =   84
      INTEGER, PARAMETER :: RCO3             =   85
      INTEGER, PARAMETER :: ACTP             =   86
      INTEGER, PARAMETER :: MEKP             =   87
      INTEGER, PARAMETER :: KETP             =   88
      INTEGER, PARAMETER :: MCP              =   89
      INTEGER, PARAMETER :: MVKP             =   90
      INTEGER, PARAMETER :: UALP             =   91
      INTEGER, PARAMETER :: DCB3             =   92
      INTEGER, PARAMETER :: BALP             =   93
      INTEGER, PARAMETER :: ADDC             =   94
      INTEGER, PARAMETER :: MCT              =   95
      INTEGER, PARAMETER :: MCTO             =   96
      INTEGER, PARAMETER :: MOH              =   97
      INTEGER, PARAMETER :: EOH              =   98
      INTEGER, PARAMETER :: ROH              =   99
      INTEGER, PARAMETER :: ETEG             =  100
      INTEGER, PARAMETER :: ISHP             =  101
      INTEGER, PARAMETER :: IEPOX            =  102
      INTEGER, PARAMETER :: MAHP             =  103
      INTEGER, PARAMETER :: ORA2             =  104
      INTEGER, PARAMETER :: ORAP             =  105
      INTEGER, PARAMETER :: PPN              =  106
      INTEGER, PARAMETER :: MPAN             =  107
      INTEGER, PARAMETER :: NALD             =  108
      INTEGER, PARAMETER :: ISON             =  109
      INTEGER, PARAMETER :: MCTP             =  110
      INTEGER, PARAMETER :: OLNN             =  111
      INTEGER, PARAMETER :: OLND             =  112
      INTEGER, PARAMETER :: ADCN             =  113
      INTEGER, PARAMETER :: TOLP             =  114
      INTEGER, PARAMETER :: PER1             =  115
      INTEGER, PARAMETER :: XYLP             =  116
      INTEGER, PARAMETER :: PER2             =  117
      INTEGER, PARAMETER :: XYOP             =  118
      INTEGER, PARAMETER :: BAL1             =  119
      INTEGER, PARAMETER :: BAL2             =  120
      INTEGER, PARAMETER :: TOLNRXN          =  121
      INTEGER, PARAMETER :: TOLHRXN          =  122
      INTEGER, PARAMETER :: XYLNRXN          =  123
      INTEGER, PARAMETER :: XYLHRXN          =  124
      INTEGER, PARAMETER :: BNZNRXN          =  125
      INTEGER, PARAMETER :: BNZHRXN          =  126
      INTEGER, PARAMETER :: SESQ             =  127
      INTEGER, PARAMETER :: SESQRXN          =  128
      INTEGER, PARAMETER :: NAPH             =  129
      INTEGER, PARAMETER :: PAHRO2           =  130
      INTEGER, PARAMETER :: PAHNRXN          =  131
      INTEGER, PARAMETER :: PAHHRXN          =  132
      INTEGER, PARAMETER :: SOAALK           =  133
      INTEGER, PARAMETER :: ALKRXN           =  134
      INTEGER, PARAMETER :: AISO3J           =  135
      INTEGER, PARAMETER :: AXYL1J           =  136
      INTEGER, PARAMETER :: AOLGAJ           =  137
      INTEGER, PARAMETER :: AXYL2J           =  138
      INTEGER, PARAMETER :: ATOL1J           =  139
      INTEGER, PARAMETER :: ATOL2J           =  140
      INTEGER, PARAMETER :: ABNZ1J           =  141
      INTEGER, PARAMETER :: ABNZ2J           =  142
      INTEGER, PARAMETER :: ATRP1J           =  143
      INTEGER, PARAMETER :: AOLGBJ           =  144
      INTEGER, PARAMETER :: ATRP2J           =  145
      INTEGER, PARAMETER :: AISO1J           =  146
      INTEGER, PARAMETER :: AISO2J           =  147
      INTEGER, PARAMETER :: ASQTJ            =  148
      INTEGER, PARAMETER :: APAH1J           =  149
      INTEGER, PARAMETER :: APAH2J           =  150
      INTEGER, PARAMETER :: AALK1J           =  151
      INTEGER, PARAMETER :: AALK2J           =  152
      INTEGER, PARAMETER :: APOCI            =  153
      INTEGER, PARAMETER :: APNCOMI          =  154
      INTEGER, PARAMETER :: APOCJ            =  155
      INTEGER, PARAMETER :: APNCOMJ          =  156
      INTEGER, PARAMETER :: PCVOC            =  157
      INTEGER, PARAMETER :: PCSOARXN         =  158
      INTEGER, PARAMETER :: VLVPO1           =  159
      INTEGER, PARAMETER :: VSVPO1           =  160
      INTEGER, PARAMETER :: VSVPO2           =  161
      INTEGER, PARAMETER :: VSVPO3           =  162
      INTEGER, PARAMETER :: VIVPO1           =  163
      INTEGER, PARAMETER :: VLVOO1           =  164
      INTEGER, PARAMETER :: VLVOO2           =  165
      INTEGER, PARAMETER :: VSVOO2           =  166
      INTEGER, PARAMETER :: VSVOO3           =  167
      INTEGER, PARAMETER :: VSVOO1           =  168

      END MODULE HRDATA
