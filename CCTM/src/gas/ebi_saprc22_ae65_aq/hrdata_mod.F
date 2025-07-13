
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
C  PRECONDITIONS: For the SAPRC22_AE65_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, May 20, 2025
C
C   18 Jul 14 B.Hutzell: revised to use real(8) variables for solution
C*************************************************************************

c..EBI solver fixed parameters
      INTEGER, PARAMETER    ::  NEBITER       = 1000      ! No. of iterations for EBI
      INTEGER, PARAMETER    ::  MAX_CELLS_EBI = 1         ! max # cells in local conc arrays
      REAL( 8 ), PARAMETER  ::  DELTAT        = 1.250D+00 ! EBI time step (min)

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
      INTEGER, PARAMETER :: HNO4             =   12
      INTEGER, PARAMETER :: HO2H             =   13
      INTEGER, PARAMETER :: CO               =   14
      INTEGER, PARAMETER :: CO2              =   15
      INTEGER, PARAMETER :: SumRO2           =   16
      INTEGER, PARAMETER :: SumRCO3          =   17
      INTEGER, PARAMETER :: RO2C             =   18
      INTEGER, PARAMETER :: RO2XC            =   19
      INTEGER, PARAMETER :: MEO2             =   20
      INTEGER, PARAMETER :: HCHO             =   21
      INTEGER, PARAMETER :: MEOOH            =   22
      INTEGER, PARAMETER :: MEOH             =   23
      INTEGER, PARAMETER :: ETO2             =   24
      INTEGER, PARAMETER :: MECHO            =   25
      INTEGER, PARAMETER :: ROOH             =   26
      INTEGER, PARAMETER :: ETOH             =   27
      INTEGER, PARAMETER :: BZO2             =   28
      INTEGER, PARAMETER :: BZO              =   29
      INTEGER, PARAMETER :: MECO3            =   30
      INTEGER, PARAMETER :: PAN              =   31
      INTEGER, PARAMETER :: OACID            =   32
      INTEGER, PARAMETER :: PACID            =   33
      INTEGER, PARAMETER :: BZCO3            =   34
      INTEGER, PARAMETER :: PBZN             =   35
      INTEGER, PARAMETER :: ALK5             =   36
      INTEGER, PARAMETER :: TBUO             =   37
      INTEGER, PARAMETER :: R1NO3            =   38
      INTEGER, PARAMETER :: ACET             =   39
      INTEGER, PARAMETER :: NPHE             =   40
      INTEGER, PARAMETER :: CRES             =   41
      INTEGER, PARAMETER :: NPRAD            =   42
      INTEGER, PARAMETER :: NAPPRD           =   43
      INTEGER, PARAMETER :: PNAMIN           =   44
      INTEGER, PARAMETER :: NAMIN            =   45
      INTEGER, PARAMETER :: AMINS            =   46
      INTEGER, PARAMETER :: HCHO2            =   47
      INTEGER, PARAMETER :: HCOOH            =   48
      INTEGER, PARAMETER :: SO2              =   49
      INTEGER, PARAMETER :: SULF             =   50
      INTEGER, PARAMETER :: SULRXN           =   51
      INTEGER, PARAMETER :: MECHO2           =   52
      INTEGER, PARAMETER :: RCHO2            =   53
      INTEGER, PARAMETER :: RCHO             =   54
      INTEGER, PARAMETER :: GLY              =   55
      INTEGER, PARAMETER :: BALD             =   56
      INTEGER, PARAMETER :: PHEN             =   57
      INTEGER, PARAMETER :: NAPS             =   58
      INTEGER, PARAMETER :: CATL             =   59
      INTEGER, PARAMETER :: AFG2A            =   60
      INTEGER, PARAMETER :: AFG2B            =   61
      INTEGER, PARAMETER :: MACO3            =   62
      INTEGER, PARAMETER :: PAHRO2           =   63
      INTEGER, PARAMETER :: CATL3            =   64
      INTEGER, PARAMETER :: OTHN             =   65
      INTEGER, PARAMETER :: PHOT             =   66
      INTEGER, PARAMETER :: ALK3             =   67
      INTEGER, PARAMETER :: IMINE            =   68
      INTEGER, PARAMETER :: CLETHE           =   69
      INTEGER, PARAMETER :: xHO2             =   70
      INTEGER, PARAMETER :: xHCHO            =   71
      INTEGER, PARAMETER :: yROOH            =   72
      INTEGER, PARAMETER :: ACRLNT           =   73
      INTEGER, PARAMETER :: PCE              =   74
      INTEGER, PARAMETER :: PCLBEN           =   75
      INTEGER, PARAMETER :: MECL2            =   76
      INTEGER, PARAMETER :: ETBR2            =   77
      INTEGER, PARAMETER :: ETCL2            =   78
      INTEGER, PARAMETER :: ETOX             =   79
      INTEGER, PARAMETER :: CHCL3            =   80
      INTEGER, PARAMETER :: xOH              =   81
      INTEGER, PARAMETER :: xNO2             =   82
      INTEGER, PARAMETER :: xNO3             =   83
      INTEGER, PARAMETER :: xGLY             =   84
      INTEGER, PARAMETER :: xHCOOH           =   85
      INTEGER, PARAMETER :: xMECHO           =   86
      INTEGER, PARAMETER :: xETCHO           =   87
      INTEGER, PARAMETER :: ETCHO            =   88
      INTEGER, PARAMETER :: xGLCHO           =   89
      INTEGER, PARAMETER :: GLCHO            =   90
      INTEGER, PARAMETER :: xMEK             =   91
      INTEGER, PARAMETER :: MEK              =   92
      INTEGER, PARAMETER :: xACRO            =   93
      INTEGER, PARAMETER :: ACRO             =   94
      INTEGER, PARAMETER :: xACET            =   95
      INTEGER, PARAMETER :: xMACR            =   96
      INTEGER, PARAMETER :: MACR             =   97
      INTEGER, PARAMETER :: xMVK             =   98
      INTEGER, PARAMETER :: MVK              =   99
      INTEGER, PARAMETER :: xBACL            =  100
      INTEGER, PARAMETER :: BACL             =  101
      INTEGER, PARAMETER :: xMGLY            =  102
      INTEGER, PARAMETER :: MGLY             =  103
      INTEGER, PARAMETER :: xBUDAL           =  104
      INTEGER, PARAMETER :: BUDAL            =  105
      INTEGER, PARAMETER :: xFURNS           =  106
      INTEGER, PARAMETER :: FURNS            =  107
      INTEGER, PARAMETER :: xBALD            =  108
      INTEGER, PARAMETER :: xBENX            =  109
      INTEGER, PARAMETER :: BENX             =  110
      INTEGER, PARAMETER :: xRCHO            =  111
      INTEGER, PARAMETER :: xKET2            =  112
      INTEGER, PARAMETER :: KET2             =  113
      INTEGER, PARAMETER :: xLVKS            =  114
      INTEGER, PARAMETER :: LVKS             =  115
      INTEGER, PARAMETER :: xOLEA1           =  116
      INTEGER, PARAMETER :: OLEA1            =  117
      INTEGER, PARAMETER :: xOLEA2           =  118
      INTEGER, PARAMETER :: OLEA2            =  119
      INTEGER, PARAMETER :: xOLEP            =  120
      INTEGER, PARAMETER :: OLEP             =  121
      INTEGER, PARAMETER :: xOACID           =  122
      INTEGER, PARAMETER :: xPACID           =  123
      INTEGER, PARAMETER :: xAMINS           =  124
      INTEGER, PARAMETER :: xRPNO3           =  125
      INTEGER, PARAMETER :: RPNO3            =  126
      INTEGER, PARAMETER :: xRCNO3           =  127
      INTEGER, PARAMETER :: RCNO3            =  128
      INTEGER, PARAMETER :: xRHNO3           =  129
      INTEGER, PARAMETER :: RHNO3            =  130
      INTEGER, PARAMETER :: xRDNO3           =  131
      INTEGER, PARAMETER :: RDNO3            =  132
      INTEGER, PARAMETER :: xHPCRB           =  133
      INTEGER, PARAMETER :: HPCRB            =  134
      INTEGER, PARAMETER :: xAFG1            =  135
      INTEGER, PARAMETER :: AFG1             =  136
      INTEGER, PARAMETER :: xAFG2A           =  137
      INTEGER, PARAMETER :: xAFG2B           =  138
      INTEGER, PARAMETER :: xAFG3            =  139
      INTEGER, PARAMETER :: AFG3             =  140
      INTEGER, PARAMETER :: xPAN2            =  141
      INTEGER, PARAMETER :: PAN2             =  142
      INTEGER, PARAMETER :: xMEO2            =  143
      INTEGER, PARAMETER :: xETO2            =  144
      INTEGER, PARAMETER :: xMECO3           =  145
      INTEGER, PARAMETER :: xR2CO3           =  146
      INTEGER, PARAMETER :: R2CO3            =  147
      INTEGER, PARAMETER :: xMACO3           =  148
      INTEGER, PARAMETER :: xTBUO            =  149
      INTEGER, PARAMETER :: xBZO             =  150
      INTEGER, PARAMETER :: yRUOOH           =  151
      INTEGER, PARAMETER :: RUOOH            =  152
      INTEGER, PARAMETER :: yRAOOH           =  153
      INTEGER, PARAMETER :: RAOOH            =  154
      INTEGER, PARAMETER :: yHPCRB           =  155
      INTEGER, PARAMETER :: yRPNO3           =  156
      INTEGER, PARAMETER :: zR1NO3           =  157
      INTEGER, PARAMETER :: zR2NO3           =  158
      INTEGER, PARAMETER :: R2NO3            =  159
      INTEGER, PARAMETER :: zRHNO3           =  160
      INTEGER, PARAMETER :: zRCNO3           =  161
      INTEGER, PARAMETER :: zRANO3           =  162
      INTEGER, PARAMETER :: RANO3            =  163
      INTEGER, PARAMETER :: zRPNO3           =  164
      INTEGER, PARAMETER :: zRDNO3           =  165
      INTEGER, PARAMETER :: zRNNO3           =  166
      INTEGER, PARAMETER :: RNNO3            =  167
      INTEGER, PARAMETER :: zPAN2            =  168
      INTEGER, PARAMETER :: APANS            =  169
      INTEGER, PARAMETER :: ETHAN            =  170
      INTEGER, PARAMETER :: PROP             =  171
      INTEGER, PARAMETER :: NC4              =  172
      INTEGER, PARAMETER :: ETHEN            =  173
      INTEGER, PARAMETER :: ETHEN_OP         =  174
      INTEGER, PARAMETER :: NROG             =  175
      INTEGER, PARAMETER :: PROPE            =  176
      INTEGER, PARAMETER :: PROPE_O3         =  177
      INTEGER, PARAMETER :: ALK2             =  178
      INTEGER, PARAMETER :: ISOP             =  179
      INTEGER, PARAMETER :: ISOP_OH          =  180
      INTEGER, PARAMETER :: ISOP_O3          =  181
      INTEGER, PARAMETER :: ISOPRXN          =  182
      INTEGER, PARAMETER :: ISOP_N3          =  183
      INTEGER, PARAMETER :: BUT13            =  184
      INTEGER, PARAMETER :: BUT13_OH         =  185
      INTEGER, PARAMETER :: BUT13_O3         =  186
      INTEGER, PARAMETER :: APINE            =  187
      INTEGER, PARAMETER :: APINE_OH         =  188
      INTEGER, PARAMETER :: TRPRXN           =  189
      INTEGER, PARAMETER :: ALK4             =  190
      INTEGER, PARAMETER :: BPINE            =  191
      INTEGER, PARAMETER :: BPINE_OH         =  192
      INTEGER, PARAMETER :: ACETL            =  193
      INTEGER, PARAMETER :: BENZ             =  194
      INTEGER, PARAMETER :: BENZRO2          =  195
      INTEGER, PARAMETER :: TOLU             =  196
      INTEGER, PARAMETER :: TOLRO2           =  197
      INTEGER, PARAMETER :: OXYL             =  198
      INTEGER, PARAMETER :: XYNL             =  199
      INTEGER, PARAMETER :: XYLRO2           =  200
      INTEGER, PARAMETER :: MXYL             =  201
      INTEGER, PARAMETER :: PXYL             =  202
      INTEGER, PARAMETER :: BZ123            =  203
      INTEGER, PARAMETER :: BZ124            =  204
      INTEGER, PARAMETER :: BZ135            =  205
      INTEGER, PARAMETER :: C2BEN            =  206
      INTEGER, PARAMETER :: MTBE             =  207
      INTEGER, PARAMETER :: ALK1             =  208
      INTEGER, PARAMETER :: MECHO_OH         =  209
      INTEGER, PARAMETER :: GLCHO_HV         =  210
      INTEGER, PARAMETER :: ACRO_OH          =  211
      INTEGER, PARAMETER :: ACRO_HV          =  212
      INTEGER, PARAMETER :: MACR_OH          =  213
      INTEGER, PARAMETER :: MACR_N3          =  214
      INTEGER, PARAMETER :: BUDAL_OH         =  215
      INTEGER, PARAMETER :: MALAH            =  216
      INTEGER, PARAMETER :: ALK5_OH          =  217
      INTEGER, PARAMETER :: ALK6             =  218
      INTEGER, PARAMETER :: ALK6_OH          =  219
      INTEGER, PARAMETER :: OLE1             =  220
      INTEGER, PARAMETER :: OLE2             =  221
      INTEGER, PARAMETER :: OLE2_O3          =  222
      INTEGER, PARAMETER :: OLE3             =  223
      INTEGER, PARAMETER :: OLE4             =  224
      INTEGER, PARAMETER :: OLE4_O3          =  225
      INTEGER, PARAMETER :: TERP             =  226
      INTEGER, PARAMETER :: TERP_OH          =  227
      INTEGER, PARAMETER :: TERP_O3          =  228
      INTEGER, PARAMETER :: TERP_N3          =  229
      INTEGER, PARAMETER :: SESQ             =  230
      INTEGER, PARAMETER :: SESQ_OH          =  231
      INTEGER, PARAMETER :: SESQRXN          =  232
      INTEGER, PARAMETER :: SESQ_O3          =  233
      INTEGER, PARAMETER :: SESQ_N3          =  234
      INTEGER, PARAMETER :: ARO1             =  235
      INTEGER, PARAMETER :: ARO2             =  236
      INTEGER, PARAMETER :: ARO2_OH          =  237
      INTEGER, PARAMETER :: STYRS            =  238
      INTEGER, PARAMETER :: TAMNS            =  239
      INTEGER, PARAMETER :: OLEA1_OH         =  240
      INTEGER, PARAMETER :: OLEA1_N3         =  241
      INTEGER, PARAMETER :: OLEA2_OH         =  242
      INTEGER, PARAMETER :: OLEA2_O3         =  243
      INTEGER, PARAMETER :: OLEA2_N3         =  244
      INTEGER, PARAMETER :: OLEA2_HV         =  245
      INTEGER, PARAMETER :: LVKS_OH          =  246
      INTEGER, PARAMETER :: LVKS_O3          =  247
      INTEGER, PARAMETER :: OLEP_OH          =  248
      INTEGER, PARAMETER :: OLEP_O3          =  249
      INTEGER, PARAMETER :: RCNO3_OH         =  250
      INTEGER, PARAMETER :: RCNO3_HV         =  251
      INTEGER, PARAMETER :: RPNO3_OH         =  252
      INTEGER, PARAMETER :: RPNO3_HV         =  253
      INTEGER, PARAMETER :: RDNO3_HV         =  254
      INTEGER, PARAMETER :: R2NO3_HV         =  255
      INTEGER, PARAMETER :: RUOOH_OH         =  256
      INTEGER, PARAMETER :: HPCRB_OH         =  257
      INTEGER, PARAMETER :: ROOH_OH          =  258
      INTEGER, PARAMETER :: AFG1_OH          =  259
      INTEGER, PARAMETER :: AFG1_HV          =  260
      INTEGER, PARAMETER :: AFG2A_OH         =  261
      INTEGER, PARAMETER :: AFG2B_OH         =  262
      INTEGER, PARAMETER :: AFG2B_HV         =  263
      INTEGER, PARAMETER :: SOAALK           =  264
      INTEGER, PARAMETER :: SVAVB2           =  265
      INTEGER, PARAMETER :: SVAVB3           =  266
      INTEGER, PARAMETER :: SVAVB4           =  267
      INTEGER, PARAMETER :: SVAVB1           =  268
      INTEGER, PARAMETER :: H2NO3PIJ         =  269
      INTEGER, PARAMETER :: H2NO3PK          =  270
      INTEGER, PARAMETER :: AISO1J           =  271
      INTEGER, PARAMETER :: AOLGBJ           =  272
      INTEGER, PARAMETER :: AISO2J           =  273
      INTEGER, PARAMETER :: ASQTJ            =  274
      INTEGER, PARAMETER :: ATRP1J           =  275
      INTEGER, PARAMETER :: ATRP2J           =  276
      INTEGER, PARAMETER :: AAVB2J           =  277
      INTEGER, PARAMETER :: AOLGAJ           =  278
      INTEGER, PARAMETER :: AAVB3J           =  279
      INTEGER, PARAMETER :: AAVB4J           =  280
      INTEGER, PARAMETER :: APOCI            =  281
      INTEGER, PARAMETER :: APNCOMI          =  282
      INTEGER, PARAMETER :: APOCJ            =  283
      INTEGER, PARAMETER :: APNCOMJ          =  284
      INTEGER, PARAMETER :: PCVOC            =  285
      INTEGER, PARAMETER :: PCSOARXN         =  286
      INTEGER, PARAMETER :: VLVPO1           =  287
      INTEGER, PARAMETER :: VSVPO1           =  288
      INTEGER, PARAMETER :: VSVPO2           =  289
      INTEGER, PARAMETER :: VSVPO3           =  290
      INTEGER, PARAMETER :: VIVPO1           =  291
      INTEGER, PARAMETER :: VLVOO1           =  292
      INTEGER, PARAMETER :: VLVOO2           =  293
      INTEGER, PARAMETER :: VSVOO2           =  294
      INTEGER, PARAMETER :: VSVOO3           =  295
      INTEGER, PARAMETER :: VSVOO1           =  296
      INTEGER, PARAMETER :: AGLYJ            =  297
      INTEGER, PARAMETER :: HCHO_PRIMARY     =  298
      INTEGER, PARAMETER :: CCHO_PRIMARY     =  299
      INTEGER, PARAMETER :: ACRO_PRIMARY     =  300

      END MODULE HRDATA
