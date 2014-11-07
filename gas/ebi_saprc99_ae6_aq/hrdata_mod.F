
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
C  PRECONDITIONS: For the SAPRC99 family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Nov. 7, 2014
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
      INTEGER, PARAMETER :: NO2              =    1
      INTEGER, PARAMETER :: NO               =    2
      INTEGER, PARAMETER :: O3P              =    3
      INTEGER, PARAMETER :: O3               =    4
      INTEGER, PARAMETER :: NO3              =    5
      INTEGER, PARAMETER :: N2O5             =    6
      INTEGER, PARAMETER :: HNO3             =    7
      INTEGER, PARAMETER :: O1D2             =    8
      INTEGER, PARAMETER :: HO               =    9
      INTEGER, PARAMETER :: HONO             =   10
      INTEGER, PARAMETER :: HO2              =   11
      INTEGER, PARAMETER :: CO               =   12
      INTEGER, PARAMETER :: HNO4             =   13
      INTEGER, PARAMETER :: HO2H             =   14
      INTEGER, PARAMETER :: SO2              =   15
      INTEGER, PARAMETER :: SULF             =   16
      INTEGER, PARAMETER :: SULRXN           =   17
      INTEGER, PARAMETER :: C_O2             =   18
      INTEGER, PARAMETER :: HCHO             =   19
      INTEGER, PARAMETER :: COOH             =   20
      INTEGER, PARAMETER :: MEOH             =   21
      INTEGER, PARAMETER :: RO2_R            =   22
      INTEGER, PARAMETER :: ROOH             =   23
      INTEGER, PARAMETER :: R2O2             =   24
      INTEGER, PARAMETER :: RO2_N            =   25
      INTEGER, PARAMETER :: RNO3             =   26
      INTEGER, PARAMETER :: MEK              =   27
      INTEGER, PARAMETER :: PROD2            =   28
      INTEGER, PARAMETER :: CCO_O2           =   29
      INTEGER, PARAMETER :: PAN              =   30
      INTEGER, PARAMETER :: CCO_OOH          =   31
      INTEGER, PARAMETER :: CCO_OH           =   32
      INTEGER, PARAMETER :: RCO_O2           =   33
      INTEGER, PARAMETER :: PAN2             =   34
      INTEGER, PARAMETER :: CCHO             =   35
      INTEGER, PARAMETER :: RCO_OOH          =   36
      INTEGER, PARAMETER :: RCO_OH           =   37
      INTEGER, PARAMETER :: BZCO_O2          =   38
      INTEGER, PARAMETER :: PBZN             =   39
      INTEGER, PARAMETER :: BZ_O             =   40
      INTEGER, PARAMETER :: MA_RCO3          =   41
      INTEGER, PARAMETER :: MA_PAN           =   42
      INTEGER, PARAMETER :: TBU_O            =   43
      INTEGER, PARAMETER :: ACET             =   44
      INTEGER, PARAMETER :: NPHE             =   45
      INTEGER, PARAMETER :: PHEN             =   46
      INTEGER, PARAMETER :: BZNO2_O          =   47
      INTEGER, PARAMETER :: HOCOO            =   48
      INTEGER, PARAMETER :: HCOOH            =   49
      INTEGER, PARAMETER :: RCHO             =   50
      INTEGER, PARAMETER :: GLY              =   51
      INTEGER, PARAMETER :: MGLY             =   52
      INTEGER, PARAMETER :: BACL             =   53
      INTEGER, PARAMETER :: CRES             =   54
      INTEGER, PARAMETER :: BALD             =   55
      INTEGER, PARAMETER :: METHACRO         =   56
      INTEGER, PARAMETER :: MVK              =   57
      INTEGER, PARAMETER :: ISOPROD          =   58
      INTEGER, PARAMETER :: DCB1             =   59
      INTEGER, PARAMETER :: DCB2             =   60
      INTEGER, PARAMETER :: DCB3             =   61
      INTEGER, PARAMETER :: ETHENE           =   62
      INTEGER, PARAMETER :: ISOPRENE         =   63
      INTEGER, PARAMETER :: ISOPRXN          =   64
      INTEGER, PARAMETER :: TRP1             =   65
      INTEGER, PARAMETER :: TRPRXN           =   66
      INTEGER, PARAMETER :: ALK1             =   67
      INTEGER, PARAMETER :: ALK2             =   68
      INTEGER, PARAMETER :: ALK3             =   69
      INTEGER, PARAMETER :: ALK4             =   70
      INTEGER, PARAMETER :: ALK5             =   71
      INTEGER, PARAMETER :: ALK5RXN          =   72
      INTEGER, PARAMETER :: ARO1             =   73
      INTEGER, PARAMETER :: ARO1RO2          =   74
      INTEGER, PARAMETER :: TOLNRXN          =   75
      INTEGER, PARAMETER :: TOLHRXN          =   76
      INTEGER, PARAMETER :: ARO2             =   77
      INTEGER, PARAMETER :: ARO2RO2          =   78
      INTEGER, PARAMETER :: XYLNRXN          =   79
      INTEGER, PARAMETER :: XYLHRXN          =   80
      INTEGER, PARAMETER :: BENZENE          =   81
      INTEGER, PARAMETER :: BENZRO2          =   82
      INTEGER, PARAMETER :: BNZNRXN          =   83
      INTEGER, PARAMETER :: BNZHRXN          =   84
      INTEGER, PARAMETER :: OLE1             =   85
      INTEGER, PARAMETER :: OLE2             =   86
      INTEGER, PARAMETER :: SESQ             =   87
      INTEGER, PARAMETER :: SESQRXN          =   88
      INTEGER, PARAMETER :: AXYL1J           =   89
      INTEGER, PARAMETER :: AOLGAJ           =   90
      INTEGER, PARAMETER :: AXYL2J           =   91
      INTEGER, PARAMETER :: ATOL1J           =   92
      INTEGER, PARAMETER :: ATOL2J           =   93
      INTEGER, PARAMETER :: ABNZ1J           =   94
      INTEGER, PARAMETER :: ABNZ2J           =   95
      INTEGER, PARAMETER :: ATRP1J           =   96
      INTEGER, PARAMETER :: AOLGBJ           =   97
      INTEGER, PARAMETER :: ATRP2J           =   98
      INTEGER, PARAMETER :: AISO1J           =   99
      INTEGER, PARAMETER :: AISO2J           =  100
      INTEGER, PARAMETER :: ASQTJ            =  101
      INTEGER, PARAMETER :: APAH1J           =  102
      INTEGER, PARAMETER :: APAH2J           =  103
      INTEGER, PARAMETER :: AALK1J           =  104
      INTEGER, PARAMETER :: AALK2J           =  105
      INTEGER, PARAMETER :: APOCI            =  106
      INTEGER, PARAMETER :: APNCOMI          =  107
      INTEGER, PARAMETER :: APOCJ            =  108
      INTEGER, PARAMETER :: APNCOMJ          =  109

      END MODULE HRDATA
