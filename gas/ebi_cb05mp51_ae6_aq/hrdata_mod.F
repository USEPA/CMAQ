
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
C  PRECONDITIONS: For the CB05MP51 family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Aug. 5, 2015
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

  ! SWITCH for calling DEGRADE routine
      LOGICAL, SAVE :: CALL_DEG = .TRUE.

c..Miscellaneous variables
      INTEGER   LOGDEV        ! Unit number of output log
      INTEGER   N_EBI_IT      ! No. of iterations for EBI
      INTEGER   N_EBI_STEPS   ! No. of time steps for EBI
      INTEGER   N_INR_STEPS   ! No. of inner time steps for EBI
  
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
      INTEGER, PARAMETER :: O                =    3
      INTEGER, PARAMETER :: O3               =    4
      INTEGER, PARAMETER :: NO3              =    5
      INTEGER, PARAMETER :: O1D              =    6
      INTEGER, PARAMETER :: OH               =    7
      INTEGER, PARAMETER :: HO2              =    8
      INTEGER, PARAMETER :: N2O5             =    9
      INTEGER, PARAMETER :: HNO3             =   10
      INTEGER, PARAMETER :: HONO             =   11
      INTEGER, PARAMETER :: PNA              =   12
      INTEGER, PARAMETER :: H2O2             =   13
      INTEGER, PARAMETER :: XO2              =   14
      INTEGER, PARAMETER :: XO2N             =   15
      INTEGER, PARAMETER :: NTROH            =   16
      INTEGER, PARAMETER :: NTRALK           =   17
      INTEGER, PARAMETER :: ROOH             =   18
      INTEGER, PARAMETER :: ALD2             =   19
      INTEGER, PARAMETER :: ALDX             =   20
      INTEGER, PARAMETER :: CO               =   21
      INTEGER, PARAMETER :: MEO2             =   22
      INTEGER, PARAMETER :: FORM             =   23
      INTEGER, PARAMETER :: MEPX             =   24
      INTEGER, PARAMETER :: MEOH             =   25
      INTEGER, PARAMETER :: HCO3             =   26
      INTEGER, PARAMETER :: FACD             =   27
      INTEGER, PARAMETER :: C2O3             =   28
      INTEGER, PARAMETER :: PAN              =   29
      INTEGER, PARAMETER :: PACD             =   30
      INTEGER, PARAMETER :: AACD             =   31
      INTEGER, PARAMETER :: CXO3             =   32
      INTEGER, PARAMETER :: PANX             =   33
      INTEGER, PARAMETER :: PAR              =   34
      INTEGER, PARAMETER :: ROR              =   35
      INTEGER, PARAMETER :: OLE              =   36
      INTEGER, PARAMETER :: ETH              =   37
      INTEGER, PARAMETER :: IOLE             =   38
      INTEGER, PARAMETER :: TOL              =   39
      INTEGER, PARAMETER :: CRES             =   40
      INTEGER, PARAMETER :: TO2              =   41
      INTEGER, PARAMETER :: TOLRO2           =   42
      INTEGER, PARAMETER :: OPEN             =   43
      INTEGER, PARAMETER :: MGLY             =   44
      INTEGER, PARAMETER :: CRO              =   45
      INTEGER, PARAMETER :: CAT1             =   46
      INTEGER, PARAMETER :: CRON             =   47
      INTEGER, PARAMETER :: CRNO             =   48
      INTEGER, PARAMETER :: CRN2             =   49
      INTEGER, PARAMETER :: CRPX             =   50
      INTEGER, PARAMETER :: OPO3             =   51
      INTEGER, PARAMETER :: CAO2             =   52
      INTEGER, PARAMETER :: OPAN             =   53
      INTEGER, PARAMETER :: XYLMN            =   54
      INTEGER, PARAMETER :: XYLRO2           =   55
      INTEGER, PARAMETER :: NAPH             =   56
      INTEGER, PARAMETER :: PAHRO2           =   57
      INTEGER, PARAMETER :: ISOP             =   58
      INTEGER, PARAMETER :: ISPD             =   59
      INTEGER, PARAMETER :: IXO2             =   60
      INTEGER, PARAMETER :: IXO2N            =   61
      INTEGER, PARAMETER :: ISOPRXN          =   62
      INTEGER, PARAMETER :: NTRI1            =   63
      INTEGER, PARAMETER :: ISOPX            =   64
      INTEGER, PARAMETER :: IEPOX            =   65
      INTEGER, PARAMETER :: NTRI2            =   66
      INTEGER, PARAMETER :: TERP             =   67
      INTEGER, PARAMETER :: TRPRXN           =   68
      INTEGER, PARAMETER :: XO2T             =   69
      INTEGER, PARAMETER :: SO2              =   70
      INTEGER, PARAMETER :: SULF             =   71
      INTEGER, PARAMETER :: SULRXN           =   72
      INTEGER, PARAMETER :: ETOH             =   73
      INTEGER, PARAMETER :: ETHA             =   74
      INTEGER, PARAMETER :: CL2              =   75
      INTEGER, PARAMETER :: CL               =   76
      INTEGER, PARAMETER :: HOCL             =   77
      INTEGER, PARAMETER :: CLO              =   78
      INTEGER, PARAMETER :: FMCL             =   79
      INTEGER, PARAMETER :: HCL              =   80
      INTEGER, PARAMETER :: CLNO2            =   81
      INTEGER, PARAMETER :: TOLNRXN          =   82
      INTEGER, PARAMETER :: TOLHRXN          =   83
      INTEGER, PARAMETER :: XYLNRXN          =   84
      INTEGER, PARAMETER :: XYLHRXN          =   85
      INTEGER, PARAMETER :: BENZENE          =   86
      INTEGER, PARAMETER :: BENZRO2          =   87
      INTEGER, PARAMETER :: BNZNRXN          =   88
      INTEGER, PARAMETER :: BNZHRXN          =   89
      INTEGER, PARAMETER :: SESQ             =   90
      INTEGER, PARAMETER :: SESQRXN          =   91
      INTEGER, PARAMETER :: PAHNRXN          =   92
      INTEGER, PARAMETER :: PAHHRXN          =   93
      INTEGER, PARAMETER :: SOAALK           =   94
      INTEGER, PARAMETER :: ALKRXN           =   95
      INTEGER, PARAMETER :: NTRC1            =   96
      INTEGER, PARAMETER :: NTRC2            =   97
      INTEGER, PARAMETER :: NXO2N            =   98
      INTEGER, PARAMETER :: NTRI1O2          =   99
      INTEGER, PARAMETER :: NTRI2O2          =  100
      INTEGER, PARAMETER :: H2NO3PIJ         =  101
      INTEGER, PARAMETER :: H2NO3PK          =  102
      INTEGER, PARAMETER :: ACLI             =  103
      INTEGER, PARAMETER :: ACLJ             =  104
      INTEGER, PARAMETER :: ACLK             =  105
      INTEGER, PARAMETER :: AISO3J           =  106
      INTEGER, PARAMETER :: AXYL1J           =  107
      INTEGER, PARAMETER :: AOLGAJ           =  108
      INTEGER, PARAMETER :: AXYL2J           =  109
      INTEGER, PARAMETER :: ATOL1J           =  110
      INTEGER, PARAMETER :: ATOL2J           =  111
      INTEGER, PARAMETER :: ABNZ1J           =  112
      INTEGER, PARAMETER :: ABNZ2J           =  113
      INTEGER, PARAMETER :: ATRP1J           =  114
      INTEGER, PARAMETER :: AOLGBJ           =  115
      INTEGER, PARAMETER :: ATRP2J           =  116
      INTEGER, PARAMETER :: AISO1J           =  117
      INTEGER, PARAMETER :: AISO2J           =  118
      INTEGER, PARAMETER :: ASQTJ            =  119
      INTEGER, PARAMETER :: APAH1J           =  120
      INTEGER, PARAMETER :: APAH2J           =  121
      INTEGER, PARAMETER :: AALK1J           =  122
      INTEGER, PARAMETER :: AALK2J           =  123
      INTEGER, PARAMETER :: APOCI            =  124
      INTEGER, PARAMETER :: APNCOMI          =  125
      INTEGER, PARAMETER :: APOCJ            =  126
      INTEGER, PARAMETER :: APNCOMJ          =  127
      INTEGER, PARAMETER :: FORM_PRIMARY     =  128
      INTEGER, PARAMETER :: ALD2_PRIMARY     =  129
      INTEGER, PARAMETER :: BUTADIENE13      =  130
      INTEGER, PARAMETER :: ACROLEIN         =  131
      INTEGER, PARAMETER :: ACROLEIN_PRIMARY =  132
      INTEGER, PARAMETER :: TOLU             =  133
      INTEGER, PARAMETER :: MXYL             =  134
      INTEGER, PARAMETER :: OXYL             =  135
      INTEGER, PARAMETER :: PXYL             =  136
      INTEGER, PARAMETER :: APIN             =  137
      INTEGER, PARAMETER :: BPIN             =  138
      INTEGER, PARAMETER :: HG               =  139
      INTEGER, PARAMETER :: HGIIAER          =  140
      INTEGER, PARAMETER :: HGIIGAS          =  141

      END MODULE HRDATA
