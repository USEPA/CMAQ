
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
C  REVISION HISTORY: Created by EBI solver program, Jun 14, 2022
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
      INTEGER, PARAMETER :: N2O5             =   45
      INTEGER, PARAMETER :: SO2              =   46
      INTEGER, PARAMETER :: SULF             =   47
      INTEGER, PARAMETER :: SULRXN           =   48
      INTEGER, PARAMETER :: ETH              =   49
      INTEGER, PARAMETER :: HC3              =   50
      INTEGER, PARAMETER :: HC3P             =   51
      INTEGER, PARAMETER :: ASOATJ           =   52
      INTEGER, PARAMETER :: HC5              =   53
      INTEGER, PARAMETER :: HC5P             =   54
      INTEGER, PARAMETER :: ETE              =   55
      INTEGER, PARAMETER :: ETEP             =   56
      INTEGER, PARAMETER :: OLT              =   57
      INTEGER, PARAMETER :: OLTP             =   58
      INTEGER, PARAMETER :: OLI              =   59
      INTEGER, PARAMETER :: OLIP             =   60
      INTEGER, PARAMETER :: ACE              =   61
      INTEGER, PARAMETER :: ORA1             =   62
      INTEGER, PARAMETER :: BEN              =   63
      INTEGER, PARAMETER :: BENP             =   64
      INTEGER, PARAMETER :: PHEN             =   65
      INTEGER, PARAMETER :: TOL              =   66
      INTEGER, PARAMETER :: TOLP             =   67
      INTEGER, PARAMETER :: CSL              =   68
      INTEGER, PARAMETER :: XYM              =   69
      INTEGER, PARAMETER :: XYMP             =   70
      INTEGER, PARAMETER :: XYE              =   71
      INTEGER, PARAMETER :: XYEP             =   72
      INTEGER, PARAMETER :: ISO              =   73
      INTEGER, PARAMETER :: ISOP             =   74
      INTEGER, PARAMETER :: API              =   75
      INTEGER, PARAMETER :: APIP1            =   76
      INTEGER, PARAMETER :: APIP2            =   77
      INTEGER, PARAMETER :: LIM              =   78
      INTEGER, PARAMETER :: LIMP1            =   79
      INTEGER, PARAMETER :: LIMP2            =   80
      INTEGER, PARAMETER :: PINALP           =   81
      INTEGER, PARAMETER :: RCO3             =   82
      INTEGER, PARAMETER :: LIMALP           =   83
      INTEGER, PARAMETER :: ACTP             =   84
      INTEGER, PARAMETER :: MEKP             =   85
      INTEGER, PARAMETER :: KETP             =   86
      INTEGER, PARAMETER :: MCP              =   87
      INTEGER, PARAMETER :: MVKP             =   88
      INTEGER, PARAMETER :: UALP             =   89
      INTEGER, PARAMETER :: DCB3             =   90
      INTEGER, PARAMETER :: BALP             =   91
      INTEGER, PARAMETER :: ADDC             =   92
      INTEGER, PARAMETER :: MCT              =   93
      INTEGER, PARAMETER :: MCTO             =   94
      INTEGER, PARAMETER :: MOH              =   95
      INTEGER, PARAMETER :: EOH              =   96
      INTEGER, PARAMETER :: ROH              =   97
      INTEGER, PARAMETER :: ETEG             =   98
      INTEGER, PARAMETER :: ISHP             =   99
      INTEGER, PARAMETER :: IEPOX            =  100
      INTEGER, PARAMETER :: MAHP             =  101
      INTEGER, PARAMETER :: ORA2             =  102
      INTEGER, PARAMETER :: ORAP             =  103
      INTEGER, PARAMETER :: PPN              =  104
      INTEGER, PARAMETER :: MPAN             =  105
      INTEGER, PARAMETER :: TRPN             =  106
      INTEGER, PARAMETER :: HOM              =  107
      INTEGER, PARAMETER :: NALD             =  108
      INTEGER, PARAMETER :: ISON             =  109
      INTEGER, PARAMETER :: MCTP             =  110
      INTEGER, PARAMETER :: OLNN             =  111
      INTEGER, PARAMETER :: OLND             =  112
      INTEGER, PARAMETER :: APINP1           =  113
      INTEGER, PARAMETER :: APINP2           =  114
      INTEGER, PARAMETER :: LIMNP1           =  115
      INTEGER, PARAMETER :: LIMNP2           =  116
      INTEGER, PARAMETER :: ADCN             =  117
      INTEGER, PARAMETER :: VROCP4OXY2       =  118
      INTEGER, PARAMETER :: VROCN1OXY6       =  119
      INTEGER, PARAMETER :: FURANONE         =  120
      INTEGER, PARAMETER :: VROCP3OXY2       =  121
      INTEGER, PARAMETER :: VROCP0OXY4       =  122
      INTEGER, PARAMETER :: BAL1             =  123
      INTEGER, PARAMETER :: BAL2             =  124
      INTEGER, PARAMETER :: ELHOM            =  125
      INTEGER, PARAMETER :: VROCIOXY         =  126
      INTEGER, PARAMETER :: SLOWROC          =  127
      INTEGER, PARAMETER :: ACRO             =  128
      INTEGER, PARAMETER :: BDE13            =  129
      INTEGER, PARAMETER :: BDE13P           =  130
      INTEGER, PARAMETER :: FURAN            =  131
      INTEGER, PARAMETER :: FURANO2          =  132
      INTEGER, PARAMETER :: PROG             =  133
      INTEGER, PARAMETER :: SESQ             =  134
      INTEGER, PARAMETER :: SESQNRO2         =  135
      INTEGER, PARAMETER :: VROCN2OXY2       =  136
      INTEGER, PARAMETER :: SESQRO2          =  137
      INTEGER, PARAMETER :: VROCP0OXY2       =  138
      INTEGER, PARAMETER :: VROCP1OXY3       =  139
      INTEGER, PARAMETER :: AGLYJ            =  140
      INTEGER, PARAMETER :: IEPOXP           =  141
      INTEGER, PARAMETER :: AISO3NOSJ        =  142
      INTEGER, PARAMETER :: ASO4J            =  143
      INTEGER, PARAMETER :: AISO3OSJ         =  144
      INTEGER, PARAMETER :: VROCP6ALK        =  145
      INTEGER, PARAMETER :: VROCP6ALKP       =  146
      INTEGER, PARAMETER :: VROCP5ALK        =  147
      INTEGER, PARAMETER :: VROCP5ALKP       =  148
      INTEGER, PARAMETER :: VROCP4ALK        =  149
      INTEGER, PARAMETER :: VROCP4ALKP       =  150
      INTEGER, PARAMETER :: VROCP3ALK        =  151
      INTEGER, PARAMETER :: VROCP3ALKP       =  152
      INTEGER, PARAMETER :: VROCP2ALK        =  153
      INTEGER, PARAMETER :: VROCP2ALKP       =  154
      INTEGER, PARAMETER :: VROCP1ALK        =  155
      INTEGER, PARAMETER :: VROCP1ALKP       =  156
      INTEGER, PARAMETER :: HC10             =  157
      INTEGER, PARAMETER :: VROCP6ALKP2      =  158
      INTEGER, PARAMETER :: VROCP5ALKP2      =  159
      INTEGER, PARAMETER :: VROCP4ALKP2      =  160
      INTEGER, PARAMETER :: VROCP2OXY2       =  161
      INTEGER, PARAMETER :: VROCP3ALKP2      =  162
      INTEGER, PARAMETER :: VROCP1OXY1       =  163
      INTEGER, PARAMETER :: VROCP2ALKP2      =  164
      INTEGER, PARAMETER :: VROCP1ALKP2      =  165
      INTEGER, PARAMETER :: VROCN1OXY1       =  166
      INTEGER, PARAMETER :: HC10P2           =  167
      INTEGER, PARAMETER :: VROCP6ARO        =  168
      INTEGER, PARAMETER :: VROCP6AROP       =  169
      INTEGER, PARAMETER :: VROCN2OXY4       =  170
      INTEGER, PARAMETER :: VROCN1OXY3       =  171
      INTEGER, PARAMETER :: VROCP5ARO        =  172
      INTEGER, PARAMETER :: VROCP5AROP       =  173
      INTEGER, PARAMETER :: NAPH             =  174
      INTEGER, PARAMETER :: NAPHP            =  175
      INTEGER, PARAMETER :: VROCN2OXY8       =  176
      INTEGER, PARAMETER :: VROCP5OXY1       =  177
      INTEGER, PARAMETER :: VROCP6OXY1       =  178

      END MODULE HRDATA
