
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
C  PRECONDITIONS: For the CRACMM1_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Dec  3, 2021
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

      LOGICAL, ALLOCATABLE   :: LAND_ZONE   ( :,: )       ! Is the surface totally land?
      LOGICAL, ALLOCATABLE   :: LFLAGIRR    ( :,:,: )     ! Conduct IRR for cell

      REAL( 8 ), ALLOCATABLE ::  ATMPRES ( : )     ! Cell pressure, Atm
!      REAL( 8 ), ALLOCATABLE ::  PRESCELL( : )         ! Cell pressure, Pa
      REAL( 8 ), ALLOCATABLE ::  H2O     ( : )     ! Cell H2O mixing ratio (ppmV)
      REAL( 8 ), ALLOCATABLE ::  TEMP    ( : )     ! Cell Temperature
      REAL( 8 ), ALLOCATABLE ::  DENSITY ( : )     ! Cell mass density (Kg/m3)
      REAL( 8 ), ALLOCATABLE ::  HET     ( :, : )  ! cell heterogeneous reaction rates
      REAL( 8 ), ALLOCATABLE ::  RJIN    ( :, : )  ! J-values for a cell
      LOGICAL, ALLOCATABLE   ::  LAND    ( : )     ! land_zone value for specific cell

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
      INTEGER, PARAMETER :: PINAL            =   23
      INTEGER, PARAMETER :: HC10P            =   24
      INTEGER, PARAMETER :: LIMAL            =   25
      INTEGER, PARAMETER :: MEK              =   26
      INTEGER, PARAMETER :: HKET             =   27
      INTEGER, PARAMETER :: MACR             =   28
      INTEGER, PARAMETER :: MACP             =   29
      INTEGER, PARAMETER :: XO2              =   30
      INTEGER, PARAMETER :: MVK              =   31
      INTEGER, PARAMETER :: GLY              =   32
      INTEGER, PARAMETER :: MGLY             =   33
      INTEGER, PARAMETER :: DCB1             =   34
      INTEGER, PARAMETER :: DCB2             =   35
      INTEGER, PARAMETER :: BALD             =   36
      INTEGER, PARAMETER :: CHO              =   37
      INTEGER, PARAMETER :: OP1              =   38
      INTEGER, PARAMETER :: OP2              =   39
      INTEGER, PARAMETER :: OPB              =   40
      INTEGER, PARAMETER :: PAA              =   41
      INTEGER, PARAMETER :: ONIT             =   42
      INTEGER, PARAMETER :: PAN              =   43
      INTEGER, PARAMETER :: N2O5             =   44
      INTEGER, PARAMETER :: SO2              =   45
      INTEGER, PARAMETER :: SULF             =   46
      INTEGER, PARAMETER :: SULRXN           =   47
      INTEGER, PARAMETER :: ETH              =   48
      INTEGER, PARAMETER :: HC3              =   49
      INTEGER, PARAMETER :: HC3P             =   50
      INTEGER, PARAMETER :: ASOATJ           =   51
      INTEGER, PARAMETER :: HC5              =   52
      INTEGER, PARAMETER :: HC5P             =   53
      INTEGER, PARAMETER :: HC8              =   54
      INTEGER, PARAMETER :: HC8P             =   55
      INTEGER, PARAMETER :: ETE              =   56
      INTEGER, PARAMETER :: ETEP             =   57
      INTEGER, PARAMETER :: OLT              =   58
      INTEGER, PARAMETER :: OLTP             =   59
      INTEGER, PARAMETER :: OLI              =   60
      INTEGER, PARAMETER :: OLIP             =   61
      INTEGER, PARAMETER :: ACE              =   62
      INTEGER, PARAMETER :: ORA1             =   63
      INTEGER, PARAMETER :: BEN              =   64
      INTEGER, PARAMETER :: BENP             =   65
      INTEGER, PARAMETER :: EPX              =   66
      INTEGER, PARAMETER :: PHEN             =   67
      INTEGER, PARAMETER :: BENZRO2          =   68
      INTEGER, PARAMETER :: TOL              =   69
      INTEGER, PARAMETER :: TR2              =   70
      INTEGER, PARAMETER :: TLP1             =   71
      INTEGER, PARAMETER :: CSL              =   72
      INTEGER, PARAMETER :: TOLRO2           =   73
      INTEGER, PARAMETER :: XYM              =   74
      INTEGER, PARAMETER :: XY2              =   75
      INTEGER, PARAMETER :: XYL1             =   76
      INTEGER, PARAMETER :: XYLRO2           =   77
      INTEGER, PARAMETER :: XYE              =   78
      INTEGER, PARAMETER :: ISO              =   79
      INTEGER, PARAMETER :: ISOP             =   80
      INTEGER, PARAMETER :: API              =   81
      INTEGER, PARAMETER :: APIP1            =   82
      INTEGER, PARAMETER :: APIP2            =   83
      INTEGER, PARAMETER :: LIM              =   84
      INTEGER, PARAMETER :: LIMP1            =   85
      INTEGER, PARAMETER :: LIMP2            =   86
      INTEGER, PARAMETER :: PINALP           =   87
      INTEGER, PARAMETER :: LIMALP           =   88
      INTEGER, PARAMETER :: RCO3             =   89
      INTEGER, PARAMETER :: ACTP             =   90
      INTEGER, PARAMETER :: MEKP             =   91
      INTEGER, PARAMETER :: KETP             =   92
      INTEGER, PARAMETER :: MCP              =   93
      INTEGER, PARAMETER :: MVKP             =   94
      INTEGER, PARAMETER :: UALP             =   95
      INTEGER, PARAMETER :: DCB3             =   96
      INTEGER, PARAMETER :: BALP             =   97
      INTEGER, PARAMETER :: ADDC             =   98
      INTEGER, PARAMETER :: MCT              =   99
      INTEGER, PARAMETER :: MCTO             =  100
      INTEGER, PARAMETER :: MOH              =  101
      INTEGER, PARAMETER :: EOH              =  102
      INTEGER, PARAMETER :: ROH              =  103
      INTEGER, PARAMETER :: ETEG             =  104
      INTEGER, PARAMETER :: ISHP             =  105
      INTEGER, PARAMETER :: IEPOX            =  106
      INTEGER, PARAMETER :: MAHP             =  107
      INTEGER, PARAMETER :: ORA2             =  108
      INTEGER, PARAMETER :: ORAP             =  109
      INTEGER, PARAMETER :: PPN              =  110
      INTEGER, PARAMETER :: MPAN             =  111
      INTEGER, PARAMETER :: TRPN             =  112
      INTEGER, PARAMETER :: HOM              =  113
      INTEGER, PARAMETER :: NALD             =  114
      INTEGER, PARAMETER :: ISON             =  115
      INTEGER, PARAMETER :: MCTP             =  116
      INTEGER, PARAMETER :: OLNN             =  117
      INTEGER, PARAMETER :: OLND             =  118
      INTEGER, PARAMETER :: APINP1           =  119
      INTEGER, PARAMETER :: APINP2           =  120
      INTEGER, PARAMETER :: LIMNP1           =  121
      INTEGER, PARAMETER :: LIMNP2           =  122
      INTEGER, PARAMETER :: ADCN             =  123
      INTEGER, PARAMETER :: TOLP             =  124
      INTEGER, PARAMETER :: PER1             =  125
      INTEGER, PARAMETER :: XYLP             =  126
      INTEGER, PARAMETER :: PER2             =  127
      INTEGER, PARAMETER :: XYO2             =  128
      INTEGER, PARAMETER :: XYOP             =  129
      INTEGER, PARAMETER :: BAL1             =  130
      INTEGER, PARAMETER :: BAL2             =  131
      INTEGER, PARAMETER :: ELHOM            =  132
      INTEGER, PARAMETER :: TOLNRXN          =  133
      INTEGER, PARAMETER :: TOLHRXN          =  134
      INTEGER, PARAMETER :: XYLNRXN          =  135
      INTEGER, PARAMETER :: XYLHRXN          =  136
      INTEGER, PARAMETER :: BNZNRXN          =  137
      INTEGER, PARAMETER :: BNZHRXN          =  138
      INTEGER, PARAMETER :: SOAALK           =  139
      INTEGER, PARAMETER :: ALKRXN           =  140
      INTEGER, PARAMETER :: ROCIOXY          =  141
      INTEGER, PARAMETER :: SLOWROC          =  142
      INTEGER, PARAMETER :: NAPH             =  143
      INTEGER, PARAMETER :: PAHRO2           =  144
      INTEGER, PARAMETER :: PAHNRXN          =  145
      INTEGER, PARAMETER :: PAHHRXN          =  146
      INTEGER, PARAMETER :: ACRO             =  147
      INTEGER, PARAMETER :: BDE13            =  148
      INTEGER, PARAMETER :: FURAN            =  149
      INTEGER, PARAMETER :: FURANO2          =  150
      INTEGER, PARAMETER :: FURANONE         =  151
      INTEGER, PARAMETER :: PROG             =  152
      INTEGER, PARAMETER :: SESQ             =  153
      INTEGER, PARAMETER :: SESQNRO2         =  154
      INTEGER, PARAMETER :: VROCP0OXY        =  155
      INTEGER, PARAMETER :: VROCP3OXY        =  156
      INTEGER, PARAMETER :: VROCN2OXY        =  157
      INTEGER, PARAMETER :: SESQRO2          =  158
      INTEGER, PARAMETER :: AGLYJ            =  159
      INTEGER, PARAMETER :: AISONJ           =  160
      INTEGER, PARAMETER :: ATRPNJ           =  161
      INTEGER, PARAMETER :: AISO3J           =  162
      INTEGER, PARAMETER :: AXYL1J           =  163
      INTEGER, PARAMETER :: AOLGAJ           =  164
      INTEGER, PARAMETER :: AXYL2J           =  165
      INTEGER, PARAMETER :: ATOL1J           =  166
      INTEGER, PARAMETER :: ATOL2J           =  167
      INTEGER, PARAMETER :: ABNZ1J           =  168
      INTEGER, PARAMETER :: ABNZ2J           =  169
      INTEGER, PARAMETER :: ATRP1J           =  170
      INTEGER, PARAMETER :: AOLGBJ           =  171
      INTEGER, PARAMETER :: ATRP2J           =  172
      INTEGER, PARAMETER :: AISO1J           =  173
      INTEGER, PARAMETER :: AISO2J           =  174
      INTEGER, PARAMETER :: APAH1J           =  175
      INTEGER, PARAMETER :: APAH2J           =  176
      INTEGER, PARAMETER :: AALK1J           =  177
      INTEGER, PARAMETER :: AALK2J           =  178
      INTEGER, PARAMETER :: APOCI            =  179
      INTEGER, PARAMETER :: APNCOMI          =  180
      INTEGER, PARAMETER :: APOCJ            =  181
      INTEGER, PARAMETER :: APNCOMJ          =  182
      INTEGER, PARAMETER :: PCVOC            =  183
      INTEGER, PARAMETER :: PCSOARXN         =  184
      INTEGER, PARAMETER :: VLVPO1           =  185
      INTEGER, PARAMETER :: VSVPO1           =  186
      INTEGER, PARAMETER :: VSVPO2           =  187
      INTEGER, PARAMETER :: VSVPO3           =  188
      INTEGER, PARAMETER :: VIVPO1           =  189
      INTEGER, PARAMETER :: VLVOO1           =  190
      INTEGER, PARAMETER :: VLVOO2           =  191
      INTEGER, PARAMETER :: VSVOO2           =  192
      INTEGER, PARAMETER :: VSVOO3           =  193
      INTEGER, PARAMETER :: VSVOO1           =  194

      END MODULE HRDATA
