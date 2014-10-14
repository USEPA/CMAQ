
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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      MODULE  HRDATA

C*************************************************************************
C
C  FUNCTION:  Mechanism & solver data for EBI solver
C
C  PRECONDITIONS: For the RACM2 family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Oct. 7, 2014
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


c..Miscellaneous variables
      INTEGER   LOGDEV        ! Unit number of output log
      INTEGER   N_EBI_IT      ! No. of iterations for EBI
      INTEGER   N_EBI_STEPS   ! No. of time steps for EBI
      INTEGER   N_INR_STEPS   ! No. of inner time steps for EBI

      REAL    EBI_TMSTEP   ! Time step for EBI loops (min)

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
      REAL( 8 ), ALLOCATABLE ::  PROD(  : )     ! Prod of species
      REAL( 8 ), ALLOCATABLE ::  LOSS(  : )     ! Loss of species


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
      INTEGER, PARAMETER :: ALK5RXN          =   51
      INTEGER, PARAMETER :: ETE              =   52
      INTEGER, PARAMETER :: ETEP             =   53
      INTEGER, PARAMETER :: OLT              =   54
      INTEGER, PARAMETER :: OLTP             =   55
      INTEGER, PARAMETER :: OLI              =   56
      INTEGER, PARAMETER :: OLIP             =   57
      INTEGER, PARAMETER :: DIEN             =   58
      INTEGER, PARAMETER :: ACE              =   59
      INTEGER, PARAMETER :: ORA1             =   60
      INTEGER, PARAMETER :: BENZENE          =   61
      INTEGER, PARAMETER :: BENP             =   62
      INTEGER, PARAMETER :: EPX              =   63
      INTEGER, PARAMETER :: PHEN             =   64
      INTEGER, PARAMETER :: BENZRO2          =   65
      INTEGER, PARAMETER :: TOL              =   66
      INTEGER, PARAMETER :: TR2              =   67
      INTEGER, PARAMETER :: TLP1             =   68
      INTEGER, PARAMETER :: CSL              =   69
      INTEGER, PARAMETER :: TOLRO2           =   70
      INTEGER, PARAMETER :: XYM              =   71
      INTEGER, PARAMETER :: XY2              =   72
      INTEGER, PARAMETER :: XYL1             =   73
      INTEGER, PARAMETER :: XYLRO2           =   74
      INTEGER, PARAMETER :: XYP              =   75
      INTEGER, PARAMETER :: XYO              =   76
      INTEGER, PARAMETER :: XYO2             =   77
      INTEGER, PARAMETER :: ISO              =   78
      INTEGER, PARAMETER :: ISOP             =   79
      INTEGER, PARAMETER :: ISOPRXN          =   80
      INTEGER, PARAMETER :: API              =   81
      INTEGER, PARAMETER :: APIP             =   82
      INTEGER, PARAMETER :: TRPRXN           =   83
      INTEGER, PARAMETER :: LIM              =   84
      INTEGER, PARAMETER :: LIMP             =   85
      INTEGER, PARAMETER :: RCO3             =   86
      INTEGER, PARAMETER :: ACTP             =   87
      INTEGER, PARAMETER :: MEKP             =   88
      INTEGER, PARAMETER :: KETP             =   89
      INTEGER, PARAMETER :: MCP              =   90
      INTEGER, PARAMETER :: MVKP             =   91
      INTEGER, PARAMETER :: UALP             =   92
      INTEGER, PARAMETER :: DCB3             =   93
      INTEGER, PARAMETER :: BALP             =   94
      INTEGER, PARAMETER :: ADDC             =   95
      INTEGER, PARAMETER :: MCT              =   96
      INTEGER, PARAMETER :: MCTO             =   97
      INTEGER, PARAMETER :: MOH              =   98
      INTEGER, PARAMETER :: EOH              =   99
      INTEGER, PARAMETER :: ROH              =  100
      INTEGER, PARAMETER :: ETEG             =  101
      INTEGER, PARAMETER :: ISHP             =  102
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
      INTEGER, PARAMETER :: AALKJ            =  129
      INTEGER, PARAMETER :: AOLGAJ           =  130
      INTEGER, PARAMETER :: AXYL1J           =  131
      INTEGER, PARAMETER :: AXYL2J           =  132
      INTEGER, PARAMETER :: ATOL1J           =  133
      INTEGER, PARAMETER :: ATOL2J           =  134
      INTEGER, PARAMETER :: ABNZ1J           =  135
      INTEGER, PARAMETER :: ABNZ2J           =  136
      INTEGER, PARAMETER :: ATRP1J           =  137
      INTEGER, PARAMETER :: AOLGBJ           =  138
      INTEGER, PARAMETER :: ATRP2J           =  139
      INTEGER, PARAMETER :: AISO1J           =  140
      INTEGER, PARAMETER :: AISO2J           =  141
      INTEGER, PARAMETER :: ASQTJ            =  142
      INTEGER, PARAMETER :: APOCI            =  143
      INTEGER, PARAMETER :: APNCOMI          =  144
      INTEGER, PARAMETER :: APOCJ            =  145
      INTEGER, PARAMETER :: APNCOMJ          =  146

      END MODULE HRDATA
