
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
C  PRECONDITIONS: For the SAPRC07TC_AE6_AQ mechanism
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
      INTEGER, PARAMETER :: NO2              =    1
      INTEGER, PARAMETER :: NO               =    2
      INTEGER, PARAMETER :: O3P              =    3
      INTEGER, PARAMETER :: O3               =    4
      INTEGER, PARAMETER :: NO3              =    5
      INTEGER, PARAMETER :: N2O5             =    6
      INTEGER, PARAMETER :: HNO3             =    7
      INTEGER, PARAMETER :: O1D              =    8
      INTEGER, PARAMETER :: OH               =    9
      INTEGER, PARAMETER :: HONO             =   10
      INTEGER, PARAMETER :: HO2              =   11
      INTEGER, PARAMETER :: CO               =   12
      INTEGER, PARAMETER :: CO2              =   13
      INTEGER, PARAMETER :: HNO4             =   14
      INTEGER, PARAMETER :: HO2H             =   15
      INTEGER, PARAMETER :: SO2              =   16
      INTEGER, PARAMETER :: SULF             =   17
      INTEGER, PARAMETER :: SULRXN           =   18
      INTEGER, PARAMETER :: MEO2             =   19
      INTEGER, PARAMETER :: HCHO             =   20
      INTEGER, PARAMETER :: COOH             =   21
      INTEGER, PARAMETER :: MEOH             =   22
      INTEGER, PARAMETER :: RO2C             =   23
      INTEGER, PARAMETER :: RO2XC            =   24
      INTEGER, PARAMETER :: MECO3            =   25
      INTEGER, PARAMETER :: PAN              =   26
      INTEGER, PARAMETER :: CCOOOH           =   27
      INTEGER, PARAMETER :: CCOOH            =   28
      INTEGER, PARAMETER :: RCO3             =   29
      INTEGER, PARAMETER :: PAN2             =   30
      INTEGER, PARAMETER :: xHO2             =   31
      INTEGER, PARAMETER :: yROOH            =   32
      INTEGER, PARAMETER :: xCCHO            =   33
      INTEGER, PARAMETER :: RCOOOH           =   34
      INTEGER, PARAMETER :: RCOOH            =   35
      INTEGER, PARAMETER :: BZCO3            =   36
      INTEGER, PARAMETER :: PBZN             =   37
      INTEGER, PARAMETER :: BZO              =   38
      INTEGER, PARAMETER :: MACO3            =   39
      INTEGER, PARAMETER :: MAPAN            =   40
      INTEGER, PARAMETER :: TBUO             =   41
      INTEGER, PARAMETER :: RNO3             =   42
      INTEGER, PARAMETER :: ACETONE          =   43
      INTEGER, PARAMETER :: NPHE             =   44
      INTEGER, PARAMETER :: CRES             =   45
      INTEGER, PARAMETER :: xOH              =   46
      INTEGER, PARAMETER :: xNO2             =   47
      INTEGER, PARAMETER :: xMEO2            =   48
      INTEGER, PARAMETER :: xMECO3           =   49
      INTEGER, PARAMETER :: xRCO3            =   50
      INTEGER, PARAMETER :: xMACO3           =   51
      INTEGER, PARAMETER :: xTBUO            =   52
      INTEGER, PARAMETER :: xCO              =   53
      INTEGER, PARAMETER :: CCHO             =   54
      INTEGER, PARAMETER :: RCHO             =   55
      INTEGER, PARAMETER :: xHCHO            =   56
      INTEGER, PARAMETER :: MEK              =   57
      INTEGER, PARAMETER :: zRNO3            =   58
      INTEGER, PARAMETER :: xRCHO            =   59
      INTEGER, PARAMETER :: HCOOH            =   60
      INTEGER, PARAMETER :: xMGLY            =   61
      INTEGER, PARAMETER :: xBACL            =   62
      INTEGER, PARAMETER :: ROOH             =   63
      INTEGER, PARAMETER :: xPROD2           =   64
      INTEGER, PARAMETER :: R6OOH            =   65
      INTEGER, PARAMETER :: PRD2             =   66
      INTEGER, PARAMETER :: yR6OOH           =   67
      INTEGER, PARAMETER :: RAOOH            =   68
      INTEGER, PARAMETER :: MGLY             =   69
      INTEGER, PARAMETER :: IPRD             =   70
      INTEGER, PARAMETER :: xGLY             =   71
      INTEGER, PARAMETER :: xMEK             =   72
      INTEGER, PARAMETER :: xAFG1            =   73
      INTEGER, PARAMETER :: xAFG2            =   74
      INTEGER, PARAMETER :: GLY              =   75
      INTEGER, PARAMETER :: AFG1             =   76
      INTEGER, PARAMETER :: AFG2             =   77
      INTEGER, PARAMETER :: HCOCO3           =   78
      INTEGER, PARAMETER :: BACL             =   79
      INTEGER, PARAMETER :: BALD             =   80
      INTEGER, PARAMETER :: AFG3             =   81
      INTEGER, PARAMETER :: xIPRD            =   82
      INTEGER, PARAMETER :: MACR             =   83
      INTEGER, PARAMETER :: MVK              =   84
      INTEGER, PARAMETER :: xHOCCHO          =   85
      INTEGER, PARAMETER :: xRNO3            =   86
      INTEGER, PARAMETER :: HOCCHO           =   87
      INTEGER, PARAMETER :: xACETONE         =   88
      INTEGER, PARAMETER :: ACROLEIN         =   89
      INTEGER, PARAMETER :: xBALD            =   90
      INTEGER, PARAMETER :: xAFG3            =   91
      INTEGER, PARAMETER :: xMACR            =   92
      INTEGER, PARAMETER :: xMVK             =   93
      INTEGER, PARAMETER :: yISOPOOH         =   94
      INTEGER, PARAMETER :: ISOPOOH          =   95
      INTEGER, PARAMETER :: yRAOOH           =   96
      INTEGER, PARAMETER :: xACROLEIN        =   97
      INTEGER, PARAMETER :: ETHENE           =   98
      INTEGER, PARAMETER :: PROPENE          =   99
      INTEGER, PARAMETER :: BUTADIENE13      =  100
      INTEGER, PARAMETER :: ISOPRENE         =  101
      INTEGER, PARAMETER :: ISOPRXN          =  102
      INTEGER, PARAMETER :: IEPOX            =  103
      INTEGER, PARAMETER :: ARO2MN           =  104
      INTEGER, PARAMETER :: IEPOXOO          =  105
      INTEGER, PARAMETER :: APIN             =  106
      INTEGER, PARAMETER :: TRPRXN           =  107
      INTEGER, PARAMETER :: ACETYLENE        =  108
      INTEGER, PARAMETER :: BENZENE          =  109
      INTEGER, PARAMETER :: BENZRO2          =  110
      INTEGER, PARAMETER :: TOLUENE          =  111
      INTEGER, PARAMETER :: TOLRO2           =  112
      INTEGER, PARAMETER :: MXYL             =  113
      INTEGER, PARAMETER :: XYLRO2           =  114
      INTEGER, PARAMETER :: OXYL             =  115
      INTEGER, PARAMETER :: PXYL             =  116
      INTEGER, PARAMETER :: TMBENZ124        =  117
      INTEGER, PARAMETER :: ETOH             =  118
      INTEGER, PARAMETER :: ALK1             =  119
      INTEGER, PARAMETER :: ALK2             =  120
      INTEGER, PARAMETER :: ALK3             =  121
      INTEGER, PARAMETER :: ALK4             =  122
      INTEGER, PARAMETER :: ALK5             =  123
      INTEGER, PARAMETER :: SOAALK           =  124
      INTEGER, PARAMETER :: ALKRXN           =  125
      INTEGER, PARAMETER :: OLE1             =  126
      INTEGER, PARAMETER :: OLE2             =  127
      INTEGER, PARAMETER :: ARO1             =  128
      INTEGER, PARAMETER :: NAPHTHAL         =  129
      INTEGER, PARAMETER :: PAHRO2           =  130
      INTEGER, PARAMETER :: TERP             =  131
      INTEGER, PARAMETER :: SESQ             =  132
      INTEGER, PARAMETER :: SESQRXN          =  133
      INTEGER, PARAMETER :: CL2              =  134
      INTEGER, PARAMETER :: CL               =  135
      INTEGER, PARAMETER :: CLNO             =  136
      INTEGER, PARAMETER :: CLONO            =  137
      INTEGER, PARAMETER :: CLNO2            =  138
      INTEGER, PARAMETER :: HCL              =  139
      INTEGER, PARAMETER :: CLO              =  140
      INTEGER, PARAMETER :: CLONO2           =  141
      INTEGER, PARAMETER :: HOCL             =  142
      INTEGER, PARAMETER :: xCL              =  143
      INTEGER, PARAMETER :: xCLCCHO          =  144
      INTEGER, PARAMETER :: xCLACET          =  145
      INTEGER, PARAMETER :: CLCCHO           =  146
      INTEGER, PARAMETER :: CLACET           =  147
      INTEGER, PARAMETER :: CLCHO            =  148
      INTEGER, PARAMETER :: BNZNRXN          =  149
      INTEGER, PARAMETER :: BNZHRXN          =  150
      INTEGER, PARAMETER :: XYLNRXN          =  151
      INTEGER, PARAMETER :: XYLHRXN          =  152
      INTEGER, PARAMETER :: TOLNRXN          =  153
      INTEGER, PARAMETER :: TOLHRXN          =  154
      INTEGER, PARAMETER :: PAHNRXN          =  155
      INTEGER, PARAMETER :: PAHHRXN          =  156
      INTEGER, PARAMETER :: HCHO_PRIMARY     =  157
      INTEGER, PARAMETER :: CCHO_PRIMARY     =  158
      INTEGER, PARAMETER :: ACRO_PRIMARY     =  159
      INTEGER, PARAMETER :: H2NO3PIJ         =  160
      INTEGER, PARAMETER :: H2NO3PK          =  161
      INTEGER, PARAMETER :: ACLI             =  162
      INTEGER, PARAMETER :: ACLJ             =  163
      INTEGER, PARAMETER :: ACLK             =  164
      INTEGER, PARAMETER :: AXYL1J           =  165
      INTEGER, PARAMETER :: AOLGAJ           =  166
      INTEGER, PARAMETER :: AXYL2J           =  167
      INTEGER, PARAMETER :: ATOL1J           =  168
      INTEGER, PARAMETER :: ATOL2J           =  169
      INTEGER, PARAMETER :: ABNZ1J           =  170
      INTEGER, PARAMETER :: ABNZ2J           =  171
      INTEGER, PARAMETER :: ATRP1J           =  172
      INTEGER, PARAMETER :: AOLGBJ           =  173
      INTEGER, PARAMETER :: ATRP2J           =  174
      INTEGER, PARAMETER :: AISO1J           =  175
      INTEGER, PARAMETER :: AISO2J           =  176
      INTEGER, PARAMETER :: ASQTJ            =  177
      INTEGER, PARAMETER :: APAH1J           =  178
      INTEGER, PARAMETER :: APAH2J           =  179
      INTEGER, PARAMETER :: AALK1J           =  180
      INTEGER, PARAMETER :: AALK2J           =  181
      INTEGER, PARAMETER :: APOCI            =  182
      INTEGER, PARAMETER :: APNCOMI          =  183
      INTEGER, PARAMETER :: APOCJ            =  184
      INTEGER, PARAMETER :: APNCOMJ          =  185
      INTEGER, PARAMETER :: PCVOC            =  186
      INTEGER, PARAMETER :: PCSOARXN         =  187
      INTEGER, PARAMETER :: VLVPO1           =  188
      INTEGER, PARAMETER :: VSVPO1           =  189
      INTEGER, PARAMETER :: VSVPO2           =  190
      INTEGER, PARAMETER :: VSVPO3           =  191
      INTEGER, PARAMETER :: VIVPO1           =  192
      INTEGER, PARAMETER :: VLVOO1           =  193
      INTEGER, PARAMETER :: VLVOO2           =  194
      INTEGER, PARAMETER :: VSVOO2           =  195
      INTEGER, PARAMETER :: VSVOO3           =  196
      INTEGER, PARAMETER :: VSVOO1           =  197
      INTEGER, PARAMETER :: AISO3J           =  198

      END MODULE HRDATA
