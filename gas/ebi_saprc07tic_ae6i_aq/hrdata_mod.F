
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
C  PRECONDITIONS: For the SAPRC07TIC family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, March 12, 2015
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
      REAL( 8 ), ALLOCATABLE ::  PROD(  : )     ! Prod of species
      REAL( 8 ), ALLOCATABLE ::  LOSS(  : )     ! Loss of species
      REAL( 8 ), ALLOCATABLE ::  PNEG(  : )! Negative production rates

      REAL, ALLOCATABLE      :: PRES( :, :, : )          ! pressure (Pa)
      REAL, ALLOCATABLE      :: QV  ( :, :, : )          ! water vapor (Kg/Kg air)
      REAL, ALLOCATABLE      :: TA  ( :, :, : )          ! temperature (K)
      REAL, ALLOCATABLE      :: RJ  ( :, :, :, : )       ! J-values (min-1)

      REAL, ALLOCATABLE      :: SEAICE ( :, :)            ! fractional seaice cover, [-]
      REAL, ALLOCATABLE      :: DENS( :, :, : )           ! mass density (Kg/m^3)

      REAL,    ALLOCATABLE   :: OPEN_ZONE   ( :,: )       ! fractional sea water cover, [-]
      REAL,    ALLOCATABLE   :: SURF_ZONE   ( :,: )       ! fractional surf-zone cover, [-]
      LOGICAL, ALLOCATABLE   :: LAND_ZONE   ( :,: )       ! Is the surface totally land?

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
      INTEGER, PARAMETER :: NO2EX            =   19
      INTEGER, PARAMETER :: MEO2             =   20
      INTEGER, PARAMETER :: HCHO             =   21
      INTEGER, PARAMETER :: COOH             =   22
      INTEGER, PARAMETER :: MEOH             =   23
      INTEGER, PARAMETER :: RO2C             =   24
      INTEGER, PARAMETER :: RO2XC            =   25
      INTEGER, PARAMETER :: MECO3            =   26
      INTEGER, PARAMETER :: PAN              =   27
      INTEGER, PARAMETER :: CCOOOH           =   28
      INTEGER, PARAMETER :: CCOOH            =   29
      INTEGER, PARAMETER :: RCO3             =   30
      INTEGER, PARAMETER :: PAN2             =   31
      INTEGER, PARAMETER :: xHO2             =   32
      INTEGER, PARAMETER :: yROOH            =   33
      INTEGER, PARAMETER :: xCCHO            =   34
      INTEGER, PARAMETER :: RCOOOH           =   35
      INTEGER, PARAMETER :: RCOOH            =   36
      INTEGER, PARAMETER :: BZCO3            =   37
      INTEGER, PARAMETER :: PBZN             =   38
      INTEGER, PARAMETER :: BZO              =   39
      INTEGER, PARAMETER :: MACO3            =   40
      INTEGER, PARAMETER :: MAPAN            =   41
      INTEGER, PARAMETER :: TBUO             =   42
      INTEGER, PARAMETER :: RNO3             =   43
      INTEGER, PARAMETER :: ACETONE          =   44
      INTEGER, PARAMETER :: NPHE             =   45
      INTEGER, PARAMETER :: CRES             =   46
      INTEGER, PARAMETER :: xOH              =   47
      INTEGER, PARAMETER :: xNO2             =   48
      INTEGER, PARAMETER :: xMEO2            =   49
      INTEGER, PARAMETER :: xMECO3           =   50
      INTEGER, PARAMETER :: xRCO3            =   51
      INTEGER, PARAMETER :: xMACO3           =   52
      INTEGER, PARAMETER :: xTBUO            =   53
      INTEGER, PARAMETER :: xCO              =   54
      INTEGER, PARAMETER :: CCHO             =   55
      INTEGER, PARAMETER :: RCHO             =   56
      INTEGER, PARAMETER :: xHCHO            =   57
      INTEGER, PARAMETER :: MEK              =   58
      INTEGER, PARAMETER :: zRNO3            =   59
      INTEGER, PARAMETER :: xRCHO            =   60
      INTEGER, PARAMETER :: HCOOH            =   61
      INTEGER, PARAMETER :: xMGLY            =   62
      INTEGER, PARAMETER :: xBACL            =   63
      INTEGER, PARAMETER :: ROOH             =   64
      INTEGER, PARAMETER :: xPROD2           =   65
      INTEGER, PARAMETER :: R6OOH            =   66
      INTEGER, PARAMETER :: PRD2             =   67
      INTEGER, PARAMETER :: yR6OOH           =   68
      INTEGER, PARAMETER :: RAOOH            =   69
      INTEGER, PARAMETER :: MGLY             =   70
      INTEGER, PARAMETER :: IPRD             =   71
      INTEGER, PARAMETER :: xGLY             =   72
      INTEGER, PARAMETER :: xMEK             =   73
      INTEGER, PARAMETER :: xAFG1            =   74
      INTEGER, PARAMETER :: xAFG2            =   75
      INTEGER, PARAMETER :: GLY              =   76
      INTEGER, PARAMETER :: AFG1             =   77
      INTEGER, PARAMETER :: AFG2             =   78
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
      INTEGER, PARAMETER :: yRAOOH           =   94
      INTEGER, PARAMETER :: xACROLEIN        =   95
      INTEGER, PARAMETER :: ETHENE           =   96
      INTEGER, PARAMETER :: PROPENE          =   97
      INTEGER, PARAMETER :: BUTADIENE13      =   98
      INTEGER, PARAMETER :: ISOPRENE         =   99
      INTEGER, PARAMETER :: APIN             =  100
      INTEGER, PARAMETER :: TRPRXN           =  101
      INTEGER, PARAMETER :: ACETYLENE        =  102
      INTEGER, PARAMETER :: BENZENE          =  103
      INTEGER, PARAMETER :: BENZRO2          =  104
      INTEGER, PARAMETER :: TOLUENE          =  105
      INTEGER, PARAMETER :: TOLRO2           =  106
      INTEGER, PARAMETER :: MXYL             =  107
      INTEGER, PARAMETER :: XYLRO2           =  108
      INTEGER, PARAMETER :: OXYL             =  109
      INTEGER, PARAMETER :: PXYL             =  110
      INTEGER, PARAMETER :: TRIMETH_BENZ124  =  111
      INTEGER, PARAMETER :: ETOH             =  112
      INTEGER, PARAMETER :: ALK1             =  113
      INTEGER, PARAMETER :: ALK2             =  114
      INTEGER, PARAMETER :: ALK3             =  115
      INTEGER, PARAMETER :: ALK4             =  116
      INTEGER, PARAMETER :: ALK5             =  117
      INTEGER, PARAMETER :: ALK5RXN          =  118
      INTEGER, PARAMETER :: OLE1             =  119
      INTEGER, PARAMETER :: OLE2             =  120
      INTEGER, PARAMETER :: ARO1             =  121
      INTEGER, PARAMETER :: ARO2             =  122
      INTEGER, PARAMETER :: TERP             =  123
      INTEGER, PARAMETER :: SESQ             =  124
      INTEGER, PARAMETER :: SESQRXN          =  125
      INTEGER, PARAMETER :: CL2              =  126
      INTEGER, PARAMETER :: CL               =  127
      INTEGER, PARAMETER :: CLNO             =  128
      INTEGER, PARAMETER :: CLONO            =  129
      INTEGER, PARAMETER :: CLNO2            =  130
      INTEGER, PARAMETER :: HCL              =  131
      INTEGER, PARAMETER :: CLO              =  132
      INTEGER, PARAMETER :: CLONO2           =  133
      INTEGER, PARAMETER :: HOCL             =  134
      INTEGER, PARAMETER :: xCL              =  135
      INTEGER, PARAMETER :: xCLCCHO          =  136
      INTEGER, PARAMETER :: xCLACET          =  137
      INTEGER, PARAMETER :: CLCCHO           =  138
      INTEGER, PARAMETER :: CLACET           =  139
      INTEGER, PARAMETER :: CLCHO            =  140
      INTEGER, PARAMETER :: BNZNRXN          =  141
      INTEGER, PARAMETER :: BNZHRXN          =  142
      INTEGER, PARAMETER :: XYLNRXN          =  143
      INTEGER, PARAMETER :: XYLHRXN          =  144
      INTEGER, PARAMETER :: TOLNRXN          =  145
      INTEGER, PARAMETER :: TOLHRXN          =  146
      INTEGER, PARAMETER :: HCHO_PRIMARY     =  147
      INTEGER, PARAMETER :: CCHO_PRIMARY     =  148
      INTEGER, PARAMETER :: ACROLEIN_PRIMARY =  149
      INTEGER, PARAMETER :: ISOPO2           =  150
      INTEGER, PARAMETER :: ISOPRXN          =  151
      INTEGER, PARAMETER :: ISOPND           =  152
      INTEGER, PARAMETER :: ISOPNB           =  153
      INTEGER, PARAMETER :: HC5              =  154
      INTEGER, PARAMETER :: DIBOO            =  155
      INTEGER, PARAMETER :: ISOPOOH          =  156
      INTEGER, PARAMETER :: HPALD            =  157
      INTEGER, PARAMETER :: HACET            =  158
      INTEGER, PARAMETER :: NISOPO2          =  159
      INTEGER, PARAMETER :: NIT1             =  160
      INTEGER, PARAMETER :: NISOPOOH         =  161
      INTEGER, PARAMETER :: HC5OO            =  162
      INTEGER, PARAMETER :: DHMOB            =  163
      INTEGER, PARAMETER :: ISOPNOOD         =  164
      INTEGER, PARAMETER :: PROPNN           =  165
      INTEGER, PARAMETER :: MVKN             =  166
      INTEGER, PARAMETER :: ETHLN            =  167
      INTEGER, PARAMETER :: RNO3I            =  168
      INTEGER, PARAMETER :: ISOPNOOB         =  169
      INTEGER, PARAMETER :: MACRN            =  170
      INTEGER, PARAMETER :: NIT1NO3OOA       =  171
      INTEGER, PARAMETER :: NIT1NO3OOB       =  172
      INTEGER, PARAMETER :: PROPNNB          =  173
      INTEGER, PARAMETER :: NIT1OHOO         =  174
      INTEGER, PARAMETER :: MVKOO            =  175
      INTEGER, PARAMETER :: MACROO           =  176
      INTEGER, PARAMETER :: PYRUACD          =  177
      INTEGER, PARAMETER :: IEPOX            =  178
      INTEGER, PARAMETER :: IEPOXOO          =  179
      INTEGER, PARAMETER :: IMACO3           =  180
      INTEGER, PARAMETER :: IMPAA            =  181
      INTEGER, PARAMETER :: IMAPAN           =  182
      INTEGER, PARAMETER :: IMAE             =  183
      INTEGER, PARAMETER :: IHMML            =  184
      INTEGER, PARAMETER :: AALKJ            =  185
      INTEGER, PARAMETER :: AOLGAJ           =  186
      INTEGER, PARAMETER :: AXYL1J           =  187
      INTEGER, PARAMETER :: AXYL2J           =  188
      INTEGER, PARAMETER :: ATOL1J           =  189
      INTEGER, PARAMETER :: ATOL2J           =  190
      INTEGER, PARAMETER :: ABNZ1J           =  191
      INTEGER, PARAMETER :: ABNZ2J           =  192
      INTEGER, PARAMETER :: ATRP1J           =  193
      INTEGER, PARAMETER :: AOLGBJ           =  194
      INTEGER, PARAMETER :: ATRP2J           =  195
      INTEGER, PARAMETER :: ASQTJ            =  196
      INTEGER, PARAMETER :: APOCI            =  197
      INTEGER, PARAMETER :: APNCOMI          =  198
      INTEGER, PARAMETER :: APOCJ            =  199
      INTEGER, PARAMETER :: APNCOMJ          =  200

      END MODULE HRDATA
