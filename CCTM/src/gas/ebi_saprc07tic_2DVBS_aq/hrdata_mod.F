
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
C  PRECONDITIONS: For the SAPRC07TIC_2DVBS_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Aug 30, 2021
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


c..Miscellaneous variables
      INTEGER   N_EBI_IT      ! No. of iterations for EBI
      INTEGER   N_EBI_STEPS   ! No. of time steps for EBI
      INTEGER   N_INR_STEPS   ! No. of inner time steps for EBI
      INTEGER   PECOL_OFFSET  ! Local Column Offset for processor
      INTEGER   PEROW_OFFSET  ! Local Column Offset for processor

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
      INTEGER, PARAMETER :: xMTNO3           =   94
      INTEGER, PARAMETER :: MTNO3            =   95
      INTEGER, PARAMETER :: IMACO3           =   96
      INTEGER, PARAMETER :: yRAOOH           =   97
      INTEGER, PARAMETER :: zMTNO3           =   98
      INTEGER, PARAMETER :: xACROLEIN        =   99
      INTEGER, PARAMETER :: ETHENE           =  100
      INTEGER, PARAMETER :: PROPENE          =  101
      INTEGER, PARAMETER :: BUTADIENE13      =  102
      INTEGER, PARAMETER :: ISOPRENE         =  103
      INTEGER, PARAMETER :: APIN             =  104
      INTEGER, PARAMETER :: TRPRO2           =  105
      INTEGER, PARAMETER :: ACETYLENE        =  106
      INTEGER, PARAMETER :: BENZENE          =  107
      INTEGER, PARAMETER :: BENZRO2          =  108
      INTEGER, PARAMETER :: TOLUENE          =  109
      INTEGER, PARAMETER :: TOLRO2           =  110
      INTEGER, PARAMETER :: MXYL             =  111
      INTEGER, PARAMETER :: XYLRO2           =  112
      INTEGER, PARAMETER :: OXYL             =  113
      INTEGER, PARAMETER :: PXYL             =  114
      INTEGER, PARAMETER :: TMBENZ124        =  115
      INTEGER, PARAMETER :: ETOH             =  116
      INTEGER, PARAMETER :: ALK1             =  117
      INTEGER, PARAMETER :: ALK2             =  118
      INTEGER, PARAMETER :: ALK3             =  119
      INTEGER, PARAMETER :: ALK4             =  120
      INTEGER, PARAMETER :: ALK5             =  121
      INTEGER, PARAMETER :: SOAALK           =  122
      INTEGER, PARAMETER :: SOAALKRO2        =  123
      INTEGER, PARAMETER :: OLE1             =  124
      INTEGER, PARAMETER :: OLE2             =  125
      INTEGER, PARAMETER :: ARO1             =  126
      INTEGER, PARAMETER :: ARO2MN           =  127
      INTEGER, PARAMETER :: NAPHTHAL         =  128
      INTEGER, PARAMETER :: PAHRO2           =  129
      INTEGER, PARAMETER :: TERP             =  130
      INTEGER, PARAMETER :: TERPNRO2         =  131
      INTEGER, PARAMETER :: SESQ             =  132
      INTEGER, PARAMETER :: SESQRO2          =  133
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
      INTEGER, PARAMETER :: HCHO_PRIMARY     =  149
      INTEGER, PARAMETER :: CCHO_PRIMARY     =  150
      INTEGER, PARAMETER :: ACRO_PRIMARY     =  151
      INTEGER, PARAMETER :: ISOPO2           =  152
      INTEGER, PARAMETER :: ISOPRXN          =  153
      INTEGER, PARAMETER :: ISOPND           =  154
      INTEGER, PARAMETER :: ISOPNB           =  155
      INTEGER, PARAMETER :: HC5              =  156
      INTEGER, PARAMETER :: DIBOO            =  157
      INTEGER, PARAMETER :: ISOPOOH          =  158
      INTEGER, PARAMETER :: HPALD            =  159
      INTEGER, PARAMETER :: HACET            =  160
      INTEGER, PARAMETER :: NISOPO2          =  161
      INTEGER, PARAMETER :: NIT1             =  162
      INTEGER, PARAMETER :: NISOPOOH         =  163
      INTEGER, PARAMETER :: HC5OO            =  164
      INTEGER, PARAMETER :: DHMOB            =  165
      INTEGER, PARAMETER :: ISOPNOOD         =  166
      INTEGER, PARAMETER :: PROPNN           =  167
      INTEGER, PARAMETER :: MVKN             =  168
      INTEGER, PARAMETER :: ETHLN            =  169
      INTEGER, PARAMETER :: RNO3I            =  170
      INTEGER, PARAMETER :: ISOPNOOB         =  171
      INTEGER, PARAMETER :: MACRN            =  172
      INTEGER, PARAMETER :: NIT1NO3OOA       =  173
      INTEGER, PARAMETER :: NIT1NO3OOB       =  174
      INTEGER, PARAMETER :: ISOPNN           =  175
      INTEGER, PARAMETER :: NIT1OHOO         =  176
      INTEGER, PARAMETER :: MVKOO            =  177
      INTEGER, PARAMETER :: MACROO           =  178
      INTEGER, PARAMETER :: PYRUACD          =  179
      INTEGER, PARAMETER :: IEPOX            =  180
      INTEGER, PARAMETER :: IEPOXOO          =  181
      INTEGER, PARAMETER :: IMPAA            =  182
      INTEGER, PARAMETER :: IMAPAN           =  183
      INTEGER, PARAMETER :: IMAE             =  184
      INTEGER, PARAMETER :: IHMML            =  185
      INTEGER, PARAMETER :: H2NO3PIJ         =  186
      INTEGER, PARAMETER :: H2NO3PK          =  187
      INTEGER, PARAMETER :: ACLI             =  188
      INTEGER, PARAMETER :: ACLJ             =  189
      INTEGER, PARAMETER :: ACLK             =  190
      INTEGER, PARAMETER :: IEPOXP           =  191
      INTEGER, PARAMETER :: IMAEP            =  192
      INTEGER, PARAMETER :: IHMMLP           =  193
      INTEGER, PARAMETER :: AIETETJ          =  194
      INTEGER, PARAMETER :: ASO4J            =  195
      INTEGER, PARAMETER :: AIEOSJ           =  196
      INTEGER, PARAMETER :: ADIMJ            =  197
      INTEGER, PARAMETER :: AIMGAJ           =  198
      INTEGER, PARAMETER :: AIMOSJ           =  199
      INTEGER, PARAMETER :: AISO1J           =  200
      INTEGER, PARAMETER :: AOLGBJ           =  201
      INTEGER, PARAMETER :: AISO2J           =  202
      INTEGER, PARAMETER :: AMTNO3J          =  203
      INTEGER, PARAMETER :: AMTHYDJ          =  204
      INTEGER, PARAMETER :: AISOPNNJ         =  205
      INTEGER, PARAMETER :: AGLYJ            =  206
      INTEGER, PARAMETER :: CS00O2C07A       =  207
      INTEGER, PARAMETER :: CS00O2C04A       =  208
      INTEGER, PARAMETER :: CS01O2C04A       =  209
      INTEGER, PARAMETER :: CS02O2C04A       =  210
      INTEGER, PARAMETER :: CS02O2C02A       =  211
      INTEGER, PARAMETER :: CS03O2C04A       =  212
      INTEGER, PARAMETER :: CS03O2C02A       =  213
      INTEGER, PARAMETER :: CS07O2C10A       =  214
      INTEGER, PARAMETER :: CS03O2C07A       =  215
      INTEGER, PARAMETER :: CS04O2C07A       =  216
      INTEGER, PARAMETER :: CS05O2C07A       =  217
      INTEGER, PARAMETER :: CS06O2C07A       =  218
      INTEGER, PARAMETER :: CS07O2C07A       =  219
      INTEGER, PARAMETER :: CS04O2C04A       =  220
      INTEGER, PARAMETER :: CS05O2C04A       =  221
      INTEGER, PARAMETER :: CS06O2C04A       =  222
      INTEGER, PARAMETER :: CS07O2C04A       =  223
      INTEGER, PARAMETER :: CS05O2C02A       =  224
      INTEGER, PARAMETER :: CS06O2C02A       =  225
      INTEGER, PARAMETER :: CS07O2C02A       =  226
      INTEGER, PARAMETER :: CS05O2C01A       =  227
      INTEGER, PARAMETER :: CS06O2C01A       =  228
      INTEGER, PARAMETER :: CS07O2C01A       =  229
      INTEGER, PARAMETER :: CS05O2C00A       =  230
      INTEGER, PARAMETER :: CS06O2C00A       =  231
      INTEGER, PARAMETER :: CS07O2C00A       =  232
      INTEGER, PARAMETER :: CS03O2C10A       =  233
      INTEGER, PARAMETER :: CS04O2C10A       =  234
      INTEGER, PARAMETER :: CS04O2C02A       =  235
      INTEGER, PARAMETER :: CSM1O2C07B       =  236
      INTEGER, PARAMETER :: CS00O2C07B       =  237
      INTEGER, PARAMETER :: CS01O2C07B       =  238
      INTEGER, PARAMETER :: CS02O2C07B       =  239
      INTEGER, PARAMETER :: CSM1O2C04B       =  240
      INTEGER, PARAMETER :: CS00O2C04B       =  241
      INTEGER, PARAMETER :: CS01O2C04B       =  242
      INTEGER, PARAMETER :: CS02O2C04B       =  243
      INTEGER, PARAMETER :: CS03O2C04B       =  244
      INTEGER, PARAMETER :: CS04O2C04B       =  245
      INTEGER, PARAMETER :: CS05O2C04B       =  246
      INTEGER, PARAMETER :: CS06O2C04B       =  247
      INTEGER, PARAMETER :: CS00O2C02B       =  248
      INTEGER, PARAMETER :: CS01O2C02B       =  249
      INTEGER, PARAMETER :: CS02O2C02B       =  250
      INTEGER, PARAMETER :: CS03O2C02B       =  251
      INTEGER, PARAMETER :: CS04O2C02B       =  252
      INTEGER, PARAMETER :: CS05O2C02B       =  253
      INTEGER, PARAMETER :: CS06O2C02B       =  254
      INTEGER, PARAMETER :: CS02O2C01B       =  255
      INTEGER, PARAMETER :: CS03O2C01B       =  256
      INTEGER, PARAMETER :: CS04O2C01B       =  257
      INTEGER, PARAMETER :: CS05O2C01B       =  258
      INTEGER, PARAMETER :: CS06O2C01B       =  259
      INTEGER, PARAMETER :: CS01O2C04        =  260
      INTEGER, PARAMETER :: CS02O2C02        =  261
      INTEGER, PARAMETER :: CS03O2C02        =  262
      INTEGER, PARAMETER :: CS03O2C04        =  263
      INTEGER, PARAMETER :: CS04O2C01        =  264
      INTEGER, PARAMETER :: CS04O2C02        =  265
      INTEGER, PARAMETER :: CS05O2C02        =  266
      INTEGER, PARAMETER :: CS05O2C04        =  267
      INTEGER, PARAMETER :: CS06O2C01        =  268
      INTEGER, PARAMETER :: CSM1O2C10        =  269
      INTEGER, PARAMETER :: CS00O2C10        =  270
      INTEGER, PARAMETER :: CS01O2C10        =  271
      INTEGER, PARAMETER :: CS02O2C10        =  272
      INTEGER, PARAMETER :: CS03O2C10        =  273
      INTEGER, PARAMETER :: CS04O2C10        =  274
      INTEGER, PARAMETER :: CS05O2C10        =  275
      INTEGER, PARAMETER :: CS06O2C10        =  276
      INTEGER, PARAMETER :: CS07O2C10        =  277
      INTEGER, PARAMETER :: CSM1O2C07        =  278
      INTEGER, PARAMETER :: CS00O2C07        =  279
      INTEGER, PARAMETER :: CS01O2C07        =  280
      INTEGER, PARAMETER :: CS02O2C07        =  281
      INTEGER, PARAMETER :: CS03O2C07        =  282
      INTEGER, PARAMETER :: CS04O2C07        =  283
      INTEGER, PARAMETER :: CS05O2C07        =  284
      INTEGER, PARAMETER :: CS06O2C07        =  285
      INTEGER, PARAMETER :: CS07O2C07        =  286
      INTEGER, PARAMETER :: CSM1O2C04        =  287
      INTEGER, PARAMETER :: CS00O2C04        =  288
      INTEGER, PARAMETER :: CS02O2C04        =  289
      INTEGER, PARAMETER :: CS04O2C04        =  290
      INTEGER, PARAMETER :: CS06O2C04        =  291
      INTEGER, PARAMETER :: CS07O2C04        =  292
      INTEGER, PARAMETER :: CSM1O2C02        =  293
      INTEGER, PARAMETER :: CS00O2C02        =  294
      INTEGER, PARAMETER :: CS01O2C02        =  295
      INTEGER, PARAMETER :: CS06O2C02        =  296
      INTEGER, PARAMETER :: CS07O2C02        =  297
      INTEGER, PARAMETER :: CSM1O2C01        =  298
      INTEGER, PARAMETER :: CS00O2C01        =  299
      INTEGER, PARAMETER :: CS01O2C01        =  300
      INTEGER, PARAMETER :: CS02O2C01        =  301
      INTEGER, PARAMETER :: CS03O2C01        =  302
      INTEGER, PARAMETER :: CS05O2C01        =  303
      INTEGER, PARAMETER :: CS07O2C01        =  304
      INTEGER, PARAMETER :: CSM1O2C00        =  305
      INTEGER, PARAMETER :: CS00O2C00        =  306
      INTEGER, PARAMETER :: CS01O2C00        =  307
      INTEGER, PARAMETER :: CS02O2C00        =  308
      INTEGER, PARAMETER :: CS03O2C00        =  309
      INTEGER, PARAMETER :: CS04O2C00        =  310
      INTEGER, PARAMETER :: CS05O2C00        =  311
      INTEGER, PARAMETER :: CS06O2C00        =  312
      INTEGER, PARAMETER :: CS07O2C00        =  313
      INTEGER, PARAMETER :: CSM1O2C04P       =  314
      INTEGER, PARAMETER :: CS00O2C04P       =  315
      INTEGER, PARAMETER :: CS01O2C04P       =  316
      INTEGER, PARAMETER :: CS02O2C04P       =  317
      INTEGER, PARAMETER :: CS03O2C04P       =  318
      INTEGER, PARAMETER :: CS04O2C04P       =  319
      INTEGER, PARAMETER :: CS05O2C04P       =  320
      INTEGER, PARAMETER :: CS06O2C04P       =  321
      INTEGER, PARAMETER :: CSM1O2C02P       =  322
      INTEGER, PARAMETER :: CS00O2C02P       =  323
      INTEGER, PARAMETER :: CS01O2C02P       =  324
      INTEGER, PARAMETER :: CS02O2C02P       =  325
      INTEGER, PARAMETER :: CS03O2C02P       =  326
      INTEGER, PARAMETER :: CS04O2C02P       =  327
      INTEGER, PARAMETER :: CS05O2C02P       =  328
      INTEGER, PARAMETER :: CS06O2C02P       =  329
      INTEGER, PARAMETER :: CSM1O2C01P       =  330
      INTEGER, PARAMETER :: CS00O2C01P       =  331
      INTEGER, PARAMETER :: CS01O2C01P       =  332
      INTEGER, PARAMETER :: CS02O2C01P       =  333
      INTEGER, PARAMETER :: CS03O2C01P       =  334
      INTEGER, PARAMETER :: CS04O2C01P       =  335
      INTEGER, PARAMETER :: CS05O2C01P       =  336
      INTEGER, PARAMETER :: CS06O2C01P       =  337
      INTEGER, PARAMETER :: CSM1O2C00P       =  338
      INTEGER, PARAMETER :: CS00O2C00P       =  339
      INTEGER, PARAMETER :: CS01O2C00P       =  340
      INTEGER, PARAMETER :: CS02O2C00P       =  341
      INTEGER, PARAMETER :: CS03O2C00P       =  342
      INTEGER, PARAMETER :: CS04O2C00P       =  343
      INTEGER, PARAMETER :: CS05O2C00P       =  344
      INTEGER, PARAMETER :: CS06O2C00P       =  345
      INTEGER, PARAMETER :: CSM1O2C20A       =  346
      INTEGER, PARAMETER :: CS00O2C20A       =  347
      INTEGER, PARAMETER :: CS01O2C20A       =  348
      INTEGER, PARAMETER :: CS02O2C20A       =  349
      INTEGER, PARAMETER :: CS03O2C20A       =  350
      INTEGER, PARAMETER :: CS04O2C20A       =  351
      INTEGER, PARAMETER :: CS05O2C20A       =  352
      INTEGER, PARAMETER :: CS06O2C20A       =  353
      INTEGER, PARAMETER :: CS07O2C20A       =  354
      INTEGER, PARAMETER :: CSM1O2C15A       =  355
      INTEGER, PARAMETER :: CS00O2C15A       =  356
      INTEGER, PARAMETER :: CS01O2C15A       =  357
      INTEGER, PARAMETER :: CS02O2C15A       =  358
      INTEGER, PARAMETER :: CS03O2C15A       =  359
      INTEGER, PARAMETER :: CS04O2C15A       =  360
      INTEGER, PARAMETER :: CS05O2C15A       =  361
      INTEGER, PARAMETER :: CS06O2C15A       =  362
      INTEGER, PARAMETER :: CS07O2C15A       =  363
      INTEGER, PARAMETER :: CSM1O2C10A       =  364
      INTEGER, PARAMETER :: CS00O2C10A       =  365
      INTEGER, PARAMETER :: CS01O2C10A       =  366
      INTEGER, PARAMETER :: CS02O2C10A       =  367
      INTEGER, PARAMETER :: CS05O2C10A       =  368
      INTEGER, PARAMETER :: CS06O2C10A       =  369
      INTEGER, PARAMETER :: CSM1O2C07A       =  370
      INTEGER, PARAMETER :: CS01O2C07A       =  371
      INTEGER, PARAMETER :: CS02O2C07A       =  372
      INTEGER, PARAMETER :: CSM1O2C04A       =  373
      INTEGER, PARAMETER :: CSM1O2C02A       =  374
      INTEGER, PARAMETER :: CS00O2C02A       =  375
      INTEGER, PARAMETER :: CS01O2C02A       =  376
      INTEGER, PARAMETER :: CSM1O2C01A       =  377
      INTEGER, PARAMETER :: CS00O2C01A       =  378
      INTEGER, PARAMETER :: CS01O2C01A       =  379
      INTEGER, PARAMETER :: CS02O2C01A       =  380
      INTEGER, PARAMETER :: CS03O2C01A       =  381
      INTEGER, PARAMETER :: CS04O2C01A       =  382
      INTEGER, PARAMETER :: CSM1O2C00A       =  383
      INTEGER, PARAMETER :: CS00O2C00A       =  384
      INTEGER, PARAMETER :: CS01O2C00A       =  385
      INTEGER, PARAMETER :: CS02O2C00A       =  386
      INTEGER, PARAMETER :: CS03O2C00A       =  387
      INTEGER, PARAMETER :: CS04O2C00A       =  388
      INTEGER, PARAMETER :: CSM1O2C10B       =  389
      INTEGER, PARAMETER :: CS00O2C10B       =  390
      INTEGER, PARAMETER :: CS01O2C10B       =  391
      INTEGER, PARAMETER :: CS02O2C10B       =  392
      INTEGER, PARAMETER :: CS03O2C10B       =  393
      INTEGER, PARAMETER :: CS04O2C10B       =  394
      INTEGER, PARAMETER :: CS05O2C10B       =  395
      INTEGER, PARAMETER :: CS06O2C10B       =  396
      INTEGER, PARAMETER :: CS07O2C10B       =  397
      INTEGER, PARAMETER :: CS03O2C07B       =  398
      INTEGER, PARAMETER :: CS04O2C07B       =  399
      INTEGER, PARAMETER :: CS05O2C07B       =  400
      INTEGER, PARAMETER :: CS06O2C07B       =  401
      INTEGER, PARAMETER :: CS07O2C07B       =  402
      INTEGER, PARAMETER :: CS07O2C04B       =  403
      INTEGER, PARAMETER :: CSM1O2C02B       =  404
      INTEGER, PARAMETER :: CS07O2C02B       =  405
      INTEGER, PARAMETER :: CSM1O2C01B       =  406
      INTEGER, PARAMETER :: CS00O2C01B       =  407
      INTEGER, PARAMETER :: CS01O2C01B       =  408
      INTEGER, PARAMETER :: CS07O2C01B       =  409
      INTEGER, PARAMETER :: CSM1O2C00B       =  410
      INTEGER, PARAMETER :: CS00O2C00B       =  411
      INTEGER, PARAMETER :: CS01O2C00B       =  412
      INTEGER, PARAMETER :: CS02O2C00B       =  413
      INTEGER, PARAMETER :: CS03O2C00B       =  414
      INTEGER, PARAMETER :: CS04O2C00B       =  415
      INTEGER, PARAMETER :: CS05O2C00B       =  416
      INTEGER, PARAMETER :: CS06O2C00B       =  417
      INTEGER, PARAMETER :: CS07O2C00B       =  418

      END MODULE HRDATA
