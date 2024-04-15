
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
C  PRECONDITIONS: For the CRACMM2 mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Mar 15, 2024
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
      INTEGER, PARAMETER :: OP3              =   41
      INTEGER, PARAMETER :: PAA              =   42
      INTEGER, PARAMETER :: ONIT             =   43
      INTEGER, PARAMETER :: PAN              =   44
      INTEGER, PARAMETER :: TRPN             =   45
      INTEGER, PARAMETER :: HONIT            =   46
      INTEGER, PARAMETER :: N2O5             =   47
      INTEGER, PARAMETER :: SO2              =   48
      INTEGER, PARAMETER :: SULF             =   49
      INTEGER, PARAMETER :: SULRXN           =   50
      INTEGER, PARAMETER :: ETH              =   51
      INTEGER, PARAMETER :: HC3              =   52
      INTEGER, PARAMETER :: HC3P             =   53
      INTEGER, PARAMETER :: ASOATJ           =   54
      INTEGER, PARAMETER :: HC5              =   55
      INTEGER, PARAMETER :: HC5P             =   56
      INTEGER, PARAMETER :: ETE              =   57
      INTEGER, PARAMETER :: ETEP             =   58
      INTEGER, PARAMETER :: OLT              =   59
      INTEGER, PARAMETER :: OLTP             =   60
      INTEGER, PARAMETER :: OLI              =   61
      INTEGER, PARAMETER :: OLIP             =   62
      INTEGER, PARAMETER :: ACE              =   63
      INTEGER, PARAMETER :: ORA1             =   64
      INTEGER, PARAMETER :: BEN              =   65
      INTEGER, PARAMETER :: BENP             =   66
      INTEGER, PARAMETER :: PHEN             =   67
      INTEGER, PARAMETER :: TOL              =   68
      INTEGER, PARAMETER :: TOLP             =   69
      INTEGER, PARAMETER :: CSL              =   70
      INTEGER, PARAMETER :: XYL              =   71
      INTEGER, PARAMETER :: XYLP             =   72
      INTEGER, PARAMETER :: EBZ              =   73
      INTEGER, PARAMETER :: EBZP             =   74
      INTEGER, PARAMETER :: ISO              =   75
      INTEGER, PARAMETER :: ISON             =   76
      INTEGER, PARAMETER :: INO2             =   77
      INTEGER, PARAMETER :: ISOP             =   78
      INTEGER, PARAMETER :: ISHP             =   79
      INTEGER, PARAMETER :: IEPOX            =   80
      INTEGER, PARAMETER :: IPX              =   81
      INTEGER, PARAMETER :: INALD            =   82
      INTEGER, PARAMETER :: ROH              =   83
      INTEGER, PARAMETER :: API              =   84
      INTEGER, PARAMETER :: APIP1            =   85
      INTEGER, PARAMETER :: APIP2            =   86
      INTEGER, PARAMETER :: LIM              =   87
      INTEGER, PARAMETER :: LIMP1            =   88
      INTEGER, PARAMETER :: LIMP2            =   89
      INTEGER, PARAMETER :: PINALP           =   90
      INTEGER, PARAMETER :: RCO3             =   91
      INTEGER, PARAMETER :: LIMALP           =   92
      INTEGER, PARAMETER :: ACTP             =   93
      INTEGER, PARAMETER :: MEKP             =   94
      INTEGER, PARAMETER :: KETP             =   95
      INTEGER, PARAMETER :: MCP              =   96
      INTEGER, PARAMETER :: MVKP             =   97
      INTEGER, PARAMETER :: UALP             =   98
      INTEGER, PARAMETER :: DCB3             =   99
      INTEGER, PARAMETER :: BALP             =  100
      INTEGER, PARAMETER :: ADDC             =  101
      INTEGER, PARAMETER :: MCT              =  102
      INTEGER, PARAMETER :: MCTO             =  103
      INTEGER, PARAMETER :: MOH              =  104
      INTEGER, PARAMETER :: EOH              =  105
      INTEGER, PARAMETER :: ETEG             =  106
      INTEGER, PARAMETER :: MAHP             =  107
      INTEGER, PARAMETER :: ORA2             =  108
      INTEGER, PARAMETER :: ORAP             =  109
      INTEGER, PARAMETER :: PPN              =  110
      INTEGER, PARAMETER :: MPAN             =  111
      INTEGER, PARAMETER :: MCTP             =  112
      INTEGER, PARAMETER :: OLNN             =  113
      INTEGER, PARAMETER :: OLND             =  114
      INTEGER, PARAMETER :: APINP1           =  115
      INTEGER, PARAMETER :: APINP2           =  116
      INTEGER, PARAMETER :: LIMNP1           =  117
      INTEGER, PARAMETER :: LIMNP2           =  118
      INTEGER, PARAMETER :: ADCN             =  119
      INTEGER, PARAMETER :: HOM              =  120
      INTEGER, PARAMETER :: VROCP4OXY2       =  121
      INTEGER, PARAMETER :: VROCN1OXY6       =  122
      INTEGER, PARAMETER :: FURANONE         =  123
      INTEGER, PARAMETER :: VROCP3OXY2       =  124
      INTEGER, PARAMETER :: VROCP0OXY4       =  125
      INTEGER, PARAMETER :: BAL1             =  126
      INTEGER, PARAMETER :: BAL2             =  127
      INTEGER, PARAMETER :: ELHOM            =  128
      INTEGER, PARAMETER :: VROCIOXY         =  129
      INTEGER, PARAMETER :: SLOWROC          =  130
      INTEGER, PARAMETER :: ACRO             =  131
      INTEGER, PARAMETER :: BDE13            =  132
      INTEGER, PARAMETER :: BDE13P           =  133
      INTEGER, PARAMETER :: FURAN            =  134
      INTEGER, PARAMETER :: FURANO2          =  135
      INTEGER, PARAMETER :: PROG             =  136
      INTEGER, PARAMETER :: SESQ             =  137
      INTEGER, PARAMETER :: SESQNRO2         =  138
      INTEGER, PARAMETER :: VROCN2OXY2       =  139
      INTEGER, PARAMETER :: SESQRO2          =  140
      INTEGER, PARAMETER :: VROCP0OXY2       =  141
      INTEGER, PARAMETER :: VROCP1OXY3       =  142
      INTEGER, PARAMETER :: AGLYJ            =  143
      INTEGER, PARAMETER :: IEPOXP           =  144
      INTEGER, PARAMETER :: AISO3NOSJ        =  145
      INTEGER, PARAMETER :: ASO4J            =  146
      INTEGER, PARAMETER :: AISO3OSJ         =  147
      INTEGER, PARAMETER :: AISO4J           =  148
      INTEGER, PARAMETER :: AISO5J           =  149
      INTEGER, PARAMETER :: VROCP6ALK        =  150
      INTEGER, PARAMETER :: VROCP6ALKP       =  151
      INTEGER, PARAMETER :: VROCP5ALK        =  152
      INTEGER, PARAMETER :: VROCP5ALKP       =  153
      INTEGER, PARAMETER :: VROCP4ALK        =  154
      INTEGER, PARAMETER :: VROCP4ALKP       =  155
      INTEGER, PARAMETER :: VROCP3ALK        =  156
      INTEGER, PARAMETER :: VROCP3ALKP       =  157
      INTEGER, PARAMETER :: VROCP2ALK        =  158
      INTEGER, PARAMETER :: VROCP2ALKP       =  159
      INTEGER, PARAMETER :: VROCP1ALK        =  160
      INTEGER, PARAMETER :: VROCP1ALKP       =  161
      INTEGER, PARAMETER :: HC10             =  162
      INTEGER, PARAMETER :: VROCP6ALKP2      =  163
      INTEGER, PARAMETER :: VROCP5ALKP2      =  164
      INTEGER, PARAMETER :: VROCP4ALKP2      =  165
      INTEGER, PARAMETER :: VROCP2OXY2       =  166
      INTEGER, PARAMETER :: VROCP3ALKP2      =  167
      INTEGER, PARAMETER :: VROCP1OXY1       =  168
      INTEGER, PARAMETER :: VROCP2ALKP2      =  169
      INTEGER, PARAMETER :: VROCP1ALKP2      =  170
      INTEGER, PARAMETER :: VROCN1OXY1       =  171
      INTEGER, PARAMETER :: HC10P2           =  172
      INTEGER, PARAMETER :: VROCP6ARO        =  173
      INTEGER, PARAMETER :: VROCP6AROP       =  174
      INTEGER, PARAMETER :: VROCN2OXY4       =  175
      INTEGER, PARAMETER :: VROCN1OXY3       =  176
      INTEGER, PARAMETER :: VROCP5ARO        =  177
      INTEGER, PARAMETER :: VROCP5AROP       =  178
      INTEGER, PARAMETER :: NAPH             =  179
      INTEGER, PARAMETER :: NAPHP            =  180
      INTEGER, PARAMETER :: VROCN2OXY8       =  181
      INTEGER, PARAMETER :: VROCP5OXY1       =  182
      INTEGER, PARAMETER :: VROCP6OXY1       =  183
      INTEGER, PARAMETER :: ECH4             =  184
      INTEGER, PARAMETER :: ATRPNJ           =  185
      INTEGER, PARAMETER :: AHOMJ            =  186
      INTEGER, PARAMETER :: AHONITJ          =  187
      INTEGER, PARAMETER :: STY              =  188
      INTEGER, PARAMETER :: STYP             =  189

      END MODULE HRDATA
